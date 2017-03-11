/*
 * How the ARM translation works:
 * translation_enter in asmcode_arm.S finds the entry point and jumps to it.
 * Some virtual registers are mapped to physical ones and those have to be loaded as well.
 * r10 in translation mode points to the global arm_state and r11 has a copy of
 * cpsr_nzcv that is updated after every virtual instruction if it changes the
 * flags. After returning from translation mode because either a branch was
 * executed or the translated block ends, r11 is split into cpsr_nzcv again
 * and all registers restored.
 */

#include <algorithm>
#include <unordered_map>
#include <cassert>
#include <cstdint>
#include <cstdio>

#ifdef IS_IOS_BUILD
#include <sys/mman.h>
#endif

// Uncomment the following line to support relative jumps if possible,
// it doesn't work that often as the mmaped section is too far away.
// #define REL_BRANCH

#include "asmcode.h"
#include "cpu.h"
#include "cpudefs.h"
#include "mem.h"
#include "mmu.h"
#include "translate.h"
#include "os/os.h"

#ifdef __thumb__
#error Thumb mode is not supported!
#endif

extern "C" {
extern void translation_next() __asm__("translation_next");
extern void translation_next_bx() __asm__("translation_next_bx");
extern void **translation_sp __asm__("translation_sp");
extern void *translation_pc_ptr __asm__("translation_pc_ptr");
#ifdef IS_IOS_BUILD
    int sys_cache_control(int function, void *start, size_t len);
#endif
}

#define MAX_TRANSLATIONS 0x40000
struct translation translation_table[MAX_TRANSLATIONS];
uint32_t *jump_table[MAX_TRANSLATIONS*2],
         **jump_table_current = jump_table;

static uint32_t *translate_buffer = nullptr,
         *translate_current = nullptr,
         *translate_end = nullptr;

static unsigned int next_translation_index = 0;

static inline void emit(uint32_t instruction)
{
    *translate_current++ = instruction;
}

static inline void emit_al(uint32_t instruction)
{
    emit(instruction | (CC_AL << 28));
}

// Loads arm.reg[rs] into rd.
static void emit_ldr_armreg(const unsigned int rd, const unsigned int r_virt)
{
    assert(rd < 16 && r_virt < 16);
    emit_al(0x59a0000 | (rd << 12) | (r_virt << 2)); // ldr rd, [r10, #2*r_virt]
}

// Saves rd into arm.reg[rs].
static void emit_str_armreg(const unsigned int rd, const unsigned int r_virt)
{
    assert(rd < 16 && r_virt < 16);
    emit_al(0x58a0000 | (rd << 12) | (r_virt << 2)); // str rd, [r10, #2*r_virt]
}

// Register allocation: Registers are dynamically reused
enum Reg : uint8_t {
    R0=0,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,R11,R12,SP,LR,PC,
    Invalid=16,Dirty=128
};

static const int first_map_reg = R4,
                    last_map_reg = R9;

// Maps virtual register to physical register (or invalid) and a dirty-flag
// PC is never mapped
static unsigned int regmap[15];

// Maps physical register to virtual register, mustn't contain anything > 0x10
// 0 means not mapped, 1 means R0 and so on. This makes it possible to optimize load_state in asmcode_arm.S
// Only mapreg[first_map_reg] until mapreg[last_map_reg] are valid entries
static unsigned mapreg[15];

// The next available physical register
static unsigned int current_reg = first_map_reg;

// Physical registers that must not be unmapped
static const int MAX_USED_REGS = 4;
static unsigned int used_regs[MAX_USED_REGS];

// How many regs are currently used and must not be unmapped
static unsigned int current_used_regs_count = 0;

static void map_reset()
{
    std::fill(regmap, regmap + 15, Invalid);
    current_used_regs_count = 0;
    current_reg = first_map_reg;
    std::fill(mapreg, mapreg + 15, 0);
}

// Marks a reg as needed. It won't be unmapped until the next instruction
static int map_need(unsigned int reg)
{
    assert(current_used_regs_count < MAX_USED_REGS);
    used_regs[current_used_regs_count++] = reg;
    return reg;
}

// Whether the physical register reg is needed
static bool reg_used(unsigned int reg)
{
    if(current_used_regs_count == 0)
        return false;

    for(unsigned int i = 0; i < current_used_regs_count; ++i)
        if(used_regs[i] == reg)
            return true;

    return false;
}

// Updates current_reg to point to an unneeded one
static void next_reg()
{
    do
    {
        if(current_reg == last_map_reg)
            current_reg = first_map_reg;
        else
            ++current_reg;
    }
    while(reg_used(current_reg));
}

// Flushed reg_phys into arm if needed and invalidates the map entriy
static void unmap_reg(unsigned int reg_phys)
{
    unsigned int oldvirt = mapreg[reg_phys];
    // Mapped?
    if(oldvirt == 0)
        return;

    // See encoding of mapreg above
    oldvirt -= 1;

    // Save register value
    if(regmap[oldvirt] & Dirty)
        emit_str_armreg(current_reg, oldvirt);

    // Invalidate map entry
    regmap[oldvirt] = Invalid;
}

static int map_reg(unsigned int reg_virt, bool write)
{
    assert(reg_virt <= 14); // Must not happen: PC (15) not implemented and any higher nr is invalid

    // Already mapped?
    if(regmap[reg_virt] != Invalid)
    {
        if(write)
            regmap[reg_virt] |= Dirty;
        return regmap[reg_virt] & 0xF; // Mask out the dirty bit
    }

    // Adjust current_reg to point to an unused one
    next_reg();

    // Reg not loaded, reuse register: Remove old mapping first
    unmap_reg(current_reg);

    // Set map entry and load value
    mapreg[current_reg] = reg_virt + 1;
    if(write)
        regmap[reg_virt] = current_reg | Dirty;
    else
    {
        // Load reg from memory
        emit_ldr_armreg(current_reg, reg_virt);
        regmap[reg_virt] = current_reg;
    }

    return current_reg;
}

// The reg is marked as dirty
// Return value: Register number the reg has been mapped to
static int map_save_reg(unsigned int reg)
{
    int ret = map_reg(reg, true);
    return map_need(ret);
}

// The current value is loaded, if not mapped yet
// Return value: Register number the reg has been loaded into
static int map_load_reg(unsigned int reg)
{
    int ret = map_reg(reg, false);
    return map_need(ret);
}

// Load and store flags from r11
static void emit_ldr_flags()
{
    emit_al(0x128f00b); // msr cpsr_f, r11
}

enum Reg : uint8_t {
    R0 = 0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, SP, LR, PC
};

static void emit_str_flags()
{
    emit_al(0x10fb000); // mrs r11, cpsr_f
    // r11 is written to arm.cpsr_flags in translation_next and the memory access helpers in asmcode_arm.S
}

static bool flags_loaded = false, flags_changed = false;

static void need_flags()
{
    if(flags_loaded)
        return;

    emit_ldr_flags();
    flags_loaded = true;
}

static void changed_flags()
{
    flags_changed = true;
}

static void flush_flags()
{
    if(!flags_loaded || !flags_changed)
        return;

    emit_str_flags();
    flags_loaded = flags_changed = false;
}

// Sets phys. rd to phys. rn
static void emit_mov_reg(unsigned int rd, unsigned int rn)
{
    emit_al(0x1a00000 | (rd << 12) | rn);
}

// Sets phys. rd to imm
static void emit_mov_imm(unsigned int rd, uint32_t imm)
{
    // movw/movt only available on >= armv7
    #if defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__)
        emit_al(0x3000000 | (rd << 12) | ((imm & 0xF000) << 4) | (imm & 0xFFF)); // movw rd, #imm&0xFFFF
        imm >>= 16;
        if(imm)
            emit_al(0x3400000 | (rd << 12) | ((imm & 0xF000) << 4) | (imm & 0xFFF)); // movt rd, #imm>>16
    #else
        if((imm & 0xFF) == imm)
            emit_al(0x3a00000 | (rd << 12) | imm); // mov rd, #imm
        else
        {
            // Insert immediate into code and jump over it
            emit_al(0x59f0000 | (rd << 12)); // ldr rd, [pc]
            emit_al(0xa000000); // b pc
            emit(imm);
        }
    #endif
}

// Returns 0 if target not reachable or a branch instruction
static uint32_t maybe_branch(void *target)
{
    #ifdef REL_BRANCH
        // The difference is a count of 4-bytes already, don't shift
        ptrdiff_t diff = reinterpret_cast<uint32_t*>(target) - translate_current - 2;
        if(diff > 0x7FFFFF || -diff > 0x800000)
            return 0;

        return 0xa000000 | (diff & 0xFFFFFF);
    #else
        (void) target;
        return 0; // Never use rel. branches. Tests below are optimized out.
    #endif
}

static void emit_jmp(void *target)
{
    uint32_t branch = maybe_branch(target);
    if(branch)
        return emit_al(branch);

    // Doesn't fit into branch, use memory
    emit_al(0x51ff004); // ldr pc, [pc, #-4]
    emit(reinterpret_cast<uintptr_t>(target));
}

// Flush all mapped regs back into arm.reg[]
static void emit_flush_regs()
{
    for(unsigned int i = first_map_reg; i <= last_map_reg; ++i)
    {
        unsigned int virt = mapreg[i];
        if(!virt) // Not mapped
            continue;

        // See encoding of mapreg above
        virt -= 1;

        unsigned int phys = regmap[virt];
        if(phys & Dirty)
            emit_str_armreg(phys & 0xF, virt);

        regmap[virt] = Invalid;
        mapreg[i] = 0;
    }
}

// Flush all regs into the global arm_state struct
// Has to be called before anything that might cause a translation leave!
static void emit_save_state()
{
    flush_flags();
    emit_flush_regs();
}

// Registers r0-r3 and r12 are not preserved!
static void emit_call(void *target)
{
    if(save)
        emit_save_state();

    uint32_t branch = maybe_branch(target);
    if(branch)
        return emit_al(branch | (1 << 24)); // Set the L-bit

    // This is cheaper than doing it like emit_mov above.
    emit_al(0x28fe004); // add lr, pc, #4
    emit_al(0x51ff004); // ldr pc, [pc, #-4]
    emit(reinterpret_cast<uintptr_t>(target));
}

static __attribute__((unused)) void emit_bkpt()
{
    emit_al(0x1200070); // bkpt #0
}

bool translate_init()
{
#ifdef IS_IOS_BUILD
#include <sys/syscall.h>
    if (!iOS_is_debugger_attached())
    {
        syscall(SYS_ptrace, 0 /*PTRACE_TRACEME*/, 0, 0, 0);
    }
#endif
    
    if(translate_buffer)
        return true;

    map_reset();
    translate_current = translate_buffer = reinterpret_cast<uint32_t*>(os_alloc_executable(INSN_BUFFER_SIZE));
    translate_end = translate_current + INSN_BUFFER_SIZE/sizeof(*translate_buffer);
    jump_table_current = jump_table;
    map_table_current = map_table;
    next_translation_index = 0;

    return !!translate_buffer;
}

void translate_deinit()
{
    if(!translate_buffer)
        return;

    os_free(translate_buffer, INSN_BUFFER_SIZE);
    translate_end = translate_current = translate_buffer = nullptr;
}

void translate(uint32_t pc_start, uint32_t *insn_ptr_start)
{
    if(next_translation_index >= MAX_TRANSLATIONS)
    {
        warn("Out of translation slots!");
        return;
    }

    #ifdef IS_IOS_BUILD
        // Mark translate_buffer as RW_
        mprotect(translate_buffer, INSN_BUFFER_SIZE, PROT_READ | PROT_WRITE);
    #endif

    uint32_t **jump_table_start = jump_table_current;
    uint32_t pc = pc_start, *insn_ptr = insn_ptr_start;
    // Pointer to translation of current instruction
    uint32_t *translate_buffer_inst_start = translate_current;
    // cond_branch points to the bne, see below
    uint32_t *cond_branch = nullptr;
    // Pointer to struct translation for this block
    translation *this_translation = &translation_table[next_translation_index];
    // Whether to stop translating code
    bool stop_here = false;

    // We know this already. end_ptr will be set after the loop
    this_translation->jump_table = reinterpret_cast<void**>(jump_table_start);
    this_translation->start_ptr = insn_ptr_start;

    // This loop is executed once per instruction.
    // Due to the CPU being able to jump to each instruction seperately,
    // there is no state preserved between (virtual) instructions.
    while(1)
    {
        // Translate further?
        if(stop_here
            || translate_current + 0x200 > translate_end
            || RAM_FLAGS(insn_ptr) & (RF_EXEC_BREAKPOINT | RF_EXEC_DEBUG_NEXT | RF_EXEC_HACK | RF_CODE_TRANSLATED | RF_CODE_NO_TRANSLATE)
            || (pc ^ pc_start) & ~0xfff)
            goto exit_translation;

        Instruction i;
        uint32_t insn = i.raw = *insn_ptr;

        // It may be that some reg mappings are flushed into memory,
        // but skipped. So flush everything now.
        // As translate_buffer_inst_start is set after this, emit_save_state counts as the last instruction
        if(i.cond != CC_AL)
            emit_save_state();

        // Right now no regs are needed
        current_used_regs_count = 0;

        // Rollback translate_current to this val if instruction not supported
        *jump_table_current = translate_buffer_inst_start = translate_current;

        // Conditional instructions are translated like this:
        // msr cpsr_f, r11
        // b<cc> after_inst
        // <instruction>
        // after_inst: @ next instruction
        if(i.cond != CC_AL && i.cond != CC_NV)
        {
            need_flags();
            cond_branch = translate_current;
            emit(0x0a000000 | ((i.cond ^ 1) << 28));
        }

        if((insn & 0xE000090) == 0x0000090)
        {
            if(!i.mem_proc2.s && !i.mem_proc2.h)
            {
                // Not mem_proc2 -> multiply or swap
                if((insn & 0x0FB00000) == 0x01000000)
                    goto unimpl; // SWP/SWPB not implemented

                // MUL, UMLAL, etc.
                if(i.mult.rm == PC || i.mult.rs == PC
                        || i.mult.rn == PC || i.mult.rd == PC)
                    goto unimpl; // PC as register not implemented

                // Register renaming, see data processing below
                Instruction translated;

                translated.raw = (i.raw & 0xFFFFFFF) | 0xE0000000;

                unsigned int armreg[2] = {16, 16};

                emit_ldr_armreg(R0, i.mult.rm);
                armreg[R0] = i.mult.rm;
                translated.mult.rm = R0;

                if(armreg[R0] == i.mult.rs)
                    translated.mult.rs = R0;
                else
                {
                    emit_ldr_armreg(R1, i.mult.rs);
                    armreg[R1] = i.mult.rs;
                    translated.mult.rs = R1;
                }

                if(i.mult.a)
                {
                    if(armreg[R0] == i.mult.rdlo)
                        translated.mult.rdlo = R0;
                    else if(armreg[R1] == i.mult.rdlo)
                        translated.mult.rdlo = R1;
                    else
                    {
                        emit_ldr_armreg(R2, i.mult.rdlo);
                        translated.mult.rdlo = R2;
                    }

                    if(i.mult.l)
                    {
                        if(armreg[R0] == i.mult.rdhi)
                            translated.mult.rdhi = R0;
                        else if(armreg[R1] == i.mult.rdhi)
                            translated.mult.rdhi = R1;
                        else
                        {
                            emit_ldr_armreg(R3, i.mult.rdhi);
                            translated.mult.rdhi = R3;
                        }
                    }
                    else
                        translated.mult.rdhi = R3;
                }
                else if(i.mult.l)
                {
                    // Not read, just written to
                    translated.mult.rdlo = R2;
                    translated.mult.rdhi = R3;
                }
                else
                    translated.mult.rd = R2;

                if(i.mult.s)
                {
                    emit_ldr_flags();
                    emit(translated.raw);
                    emit_str_flags();
                }
                else
                    emit(translated.raw);

                if(i.mult.l)
                    emit_str_armreg(translated.mult.rdlo, i.mult.rdlo);
                emit_str_armreg(translated.mult.rd, i.mult.rd);

                goto instruction_translated;
            }

            if(i.mem_proc2.s || !i.mem_proc2.h)
                goto unimpl; // Signed byte/halfword and doubleword not implemented

            if(i.mem_proc2.rn == PC
                    || i.mem_proc2.rd == PC
                    || i.mem_proc2.rm == PC)
                goto unimpl; // PC as operand or dest. not implemented

            // Load base into r0
            emit_ldr_armreg(R0, i.mem_proc2.rn);

            // Offset into r5
            if(i.mem_proc2.i) // Immediate offset
                emit_mov(R5, (i.mem_proc2.immed_h << 4) | i.mem_proc2.immed_l);
            else // Register offset
                emit_ldr_armreg(R5, i.mem_proc2.rm);

            // Get final address..
            if(i.mem_proc2.p)
            {
                // ..into r0
                if(i.mem_proc2.u)
                    emit_al(0x0800005); // add r0, r0, r5
                else
                    emit_al(0x0400005); // sub r0, r0, r5

                if(i.mem_proc2.w) // Writeback: final address into rn
                    emit_str_armreg(R0, i.mem_proc2.rn);
            }
            else
            {
                // ..into r5
                if(i.mem_proc2.u)
                    emit_al(0x0805005); // add r5, r0, r5
                else
                    emit_al(0x0405005); // sub r5, r0, r5
            }

            if(i.mem_proc2.l)
            {
                emit_call(reinterpret_cast<void*>(read_half_asm));
                emit_str_armreg(R0, i.mem_proc2.rd);
            }
            else
            {
                emit_ldr_armreg(R1, i.mem_proc2.rd);
                emit_call(reinterpret_cast<void*>(write_half_asm));
            }

            // Post-indexed: final address in r5 back into rn
            if(!i.mem_proc.p)
                emit_str_armreg(R5, i.mem_proc.rn);
        }
        else if((insn & 0xD900000) == 0x1000000)
        {
            if((insn & 0xFFFFFD0) == 0x12FFF10)
            {
                //B(L)X
                if(i.bx.l)
                {
                    emit_mov(R0, pc + 4);
                    emit_str_armreg(R0, LR);
                }

                if(i.bx.rm == PC)
                    goto unimpl;

                // Load destination into R0
                emit_ldr_armreg(R0, i.bx.rm);
                emit_jmp(reinterpret_cast<void*>(translation_next_bx));

                if(i.cond == CC_EQ)
                    stop_here = true;
            }
            else
                goto unimpl;
        }
        else if((insn & 0xC000000) == 0x0000000)
        {
            // Data processing:
            // The registers the instruction uses are renamed
            bool setcc = i.data_proc.s,
                 reg_shift = !i.data_proc.imm && i.data_proc.reg_shift;

            // Using pc is not supported
            if(i.data_proc.rd == PC
                    || i.data_proc.rn == PC
                    || (!i.data_proc.imm && i.data_proc.rm == PC)
                    || (reg_shift && i.data_proc.rs == PC))
                goto unimpl;

            // Shortcut for simple register mov (mov rd, rm)
            if((i.raw & 0xFFF0FF0) == 0x1a00000)
            {
                emit_ldr_armreg(R0, i.data_proc.rm);
                emit_str_armreg(R0, i.data_proc.rd);
                goto instruction_translated;
            }

            Instruction translated;
            translated.raw = (i.raw & 0xFFFFFFF) | 0xE0000000;

            // Map needed register values:
            // add rd, rn, rm, lsl rs becomes add r4, r0, r1, lsl r2 for example

            int armreg[2] = {-1, -1};

            // MOV and MVN don't have Rn
            if(i.data_proc.op != OP_MOV && i.data_proc.op != OP_MVN)
            {
                emit_ldr_armreg(R0, i.data_proc.rn);
                armreg[R0] = i.data_proc.rn;
                translated.data_proc.rn = R0;
            }

            // rm is stored in the immediate, don't overwrite it
            if(!i.data_proc.imm)
            {
                if(armreg[R0] == i.data_proc.rm)
                    translated.data_proc.rm = R0;
                else
                {
                    emit_ldr_armreg(R1, i.data_proc.rm);
                    armreg[R1] = i.data_proc.rm;
                    translated.data_proc.rm = R1;
                }
            }

            // Only load rs if actually needed
            if(reg_shift)
            {
                if(armreg[R0] == i.data_proc.rs)
                    translated.data_proc.rs = R0;
                else if(armreg[R1] == i.data_proc.rs)
                    translated.data_proc.rs = R1;
                else
                {
                    emit_ldr_armreg(R2, i.data_proc.rs);
                    translated.data_proc.rs = R2;
                }
            }

            // Only change the destination register if it has one
            if(i.data_proc.op < OP_TST || i.data_proc.op > OP_CMN)
            {
                if(i.data_proc.rd != PC)
                    translated.data_proc.rd = map_save_reg(i.data_proc.rd);
                else
                    translated.data_proc.rd = R0; // R0 is first arg to translation_next
            }

            if(unlikely(setcc
                        || i.data_proc.op == OP_ADC
                        || i.data_proc.op == OP_SBC
                        || i.data_proc.op == OP_RSC
                        || (!i.data_proc.reg_shift && i.data_proc.shift == SH_ROR))) // ROR with #0 as imm is RRX (not everything checked)
            {
                // This instruction impacts the flags, load them...
                need_flags();

                // Emit the instruction itself
                emit(translated.raw);

                // ... and store them again if modified
                if(setcc || (!i.data_proc.reg_shift && i.data_proc.shift == SH_ROR))
                    changed_flags();
            }
            else
                emit(translated.raw);

            if(i.data_proc.op < OP_TST || i.data_proc.op > OP_CMN)
            {
                emit_str_armreg(R3, i.data_proc.rd);

                // Jump to destination
                if(i.data_proc.rd == PC)
                {
                    // We're going to leave the translation, flush regs
                    emit_save_state();
                    emit_jmp(reinterpret_cast<void*>(translation_next));
                }
            }
        }
        else if((insn & 0xC000000) == 0x4000000)
        {goto unimpl;
            // Memory access: LDR, STRB, etc.

            // User mode access not implemented
            if(!i.mem_proc.p && i.mem_proc.w)
                goto unimpl;

            bool offset_is_zero = !i.mem_proc.not_imm && i.mem_proc.immed == 0;

            // Base register gets in r0
            if(i.mem_proc.rn != PC)
                emit_ldr_armreg(R0, i.mem_proc.rn);
            else if(i.mem_proc.not_imm)
                emit_mov(R0, pc + 8);
            else // Address known
            {
                int offset = i.mem_proc.u ? i.mem_proc.immed :
                                            -i.mem_proc.immed;
                unsigned int address = pc + 8 + offset;

                if(!i.mem_proc.l)
                {
                    emit_mov(R0, address);
                    goto no_offset;
                }

                // Load: value very likely constant
                uint32_t *ptr = reinterpret_cast<uint32_t*>(try_ptr(address));
                if(!ptr)
                {
                    // Location not readable yet
                    emit_mov(R0, address);
                    goto no_offset;
                }

                emit_mov(R0, i.mem_proc.b ? *ptr & 0xFF : *ptr);
                if(i.mem_proc.rd != PC)
                    emit_str_armreg(R0, i.mem_proc.rd);
                else
                {
                    // pc is destination register
                    emit_jmp(reinterpret_cast<void*>(translation_next));
                    // It's an unconditional jump
                    if(i.cond == CC_EQ)
                        stop_here = true;
                }

                goto instruction_translated;
            }

            // Skip offset calculation
            if(offset_is_zero)
                goto no_offset;

            // Offset gets in r5
            if(!i.mem_proc.not_imm)
            {
                // Immediate offset
                emit_mov(R5, i.mem_proc.immed);
            }
            else
            {
                // Shifted register: translate to mov
                Instruction off;
                off.raw = 0xe1a05000; // mov r5, something
                off.raw |= insn & 0xFFF; // Copy shifter_operand
                off.data_proc.rm = R1;
                if(i.mem_proc.rm != PC)
                    emit_ldr_armreg(R1, i.mem_proc.rm);
                else
                    emit_mov(R1, pc + 8);

                emit(off.raw);
            }

            // Get final address..
            if(i.mem_proc.p)
            {
                // ..in r0
                if(i.mem_proc.u)
                    emit_al(0x0800005); // add r0, r0, r5
                else
                    emit_al(0x0400005); // sub r0, r0, r5

                // TODO: In case of data aborts, this is at the wrong time.
                // It has to be stored AFTER the memory access, to be able
                // to perform the access again after an abort.
                if(i.mem_proc.w) // Writeback: final address into rn
                    emit_str_armreg(R0, i.mem_proc.rn);
            }
            else
            {
                // ..in r5
                if(i.mem_proc.u)
                    emit_al(0x0805005); // add r5, r0, r5
                else
                    emit_al(0x0405005); // sub r5, r0, r5
            }

            no_offset:
            if(i.mem_proc.l)
            {
                emit_call(reinterpret_cast<void*>(i.mem_proc.b ? read_byte_asm : read_word_asm));
                if(i.mem_proc.rd != PC)
                    emit_str_armreg(R0, i.mem_proc.rd); // r0 is return value
                else
                {
                    //TODO: Fix this
                    goto unimpl;
                    // pc is destination register
                    emit_save_state();
                    emit_jmp(reinterpret_cast<void*>(translation_next));
                }
            }
            else
            {
                if(i.mem_proc.rd != PC)
                    emit_ldr_armreg(R1, i.mem_proc.rd); // r1 is the value
                else
                    emit_mov(R1, pc + 12);

                emit_call(reinterpret_cast<void*>(i.mem_proc.b ? write_byte_asm : write_word_asm));
            }

            // Post-indexed: final address from r5 into rn
            if(!offset_is_zero && !i.mem_proc.p)
                emit_str_armreg(R5, i.mem_proc.rn);

            // Jump after writeback, to support post-indexed jumps
            if(i.mem_proc.l && i.mem_proc.rd == PC)
            {
                // pc is destination register
                emit_jmp(reinterpret_cast<void*>(translation_next));
                // It's an unconditional jump
                if(i.cond == CC_EQ)
                    stop_here = true;
            }
        }
        else if((insn & 0xE000000) == 0x8000000)
        {
            if(i.mem_multi.s)
                goto unimpl; // Exception return or usermode not implemented

            if(i.mem_multi.rn == PC)
                goto unimpl;

            uint16_t reglist = i.mem_multi.reglist;
            if(reglist & (1 << i.mem_multi.rn))
                goto unimpl; // Loading/Saving address register

            //TODO: Loading PC is broken
            if(reglist & (1 << PC))
                goto unimpl;

            int count = 0, offset, wb_offset;
            uint16_t reglist_ = reglist;
            while(reglist_)
            {
                if(reglist_ & 1)
                    count += 1;
                reglist_ >>= 1;
            }

            if(i.mem_multi.u) // Increment
            {
                wb_offset = count * 4;
                offset = 0;
            }
            else // Decrement
            {
                wb_offset = count * -4;
                offset = wb_offset;
            }

            if(i.mem_multi.p == i.mem_multi.u)
                offset += 4;

            // Base reg in r4
            emit_ldr_armreg(R4, i.mem_multi.rn);
            unsigned int reg = 0;
            while(reglist)
            {
                if((reglist & 1) == 0)
                    goto next;

                if(offset >= 0)
                    emit_al(0x2840000 | offset); // add r0, r4, #offset
                else
                    emit_al(0x2440000 | -offset); // sub r0, r4, #-offset

                if(i.mem_multi.l)
                {
                    emit_call(reinterpret_cast<void*>(read_word_asm));
                    if(reg != PC)
                        emit_str_armreg(R0, reg);
                    //else written below
                }
                else
                {
                    if(reg == PC)
                        emit_mov(R1, pc + 12);
                    else
                        emit_ldr_armreg(R1, reg);

                    emit_call(reinterpret_cast<void*>(write_word_asm));
                }

                offset += 4;
                next:
                reglist >>= 1;
                ++reg;
            }

            if(i.mem_multi.w)
            {
                if(wb_offset >= 0)
                    emit_al(0x2841000 | wb_offset); // add r1, r4, #wb_offset
                else
                    emit_al(0x2441000 | -wb_offset); // sub r1, r4, #-wb_offset

                emit_str_armreg(R1, i.mem_multi.rn);
            }

            if(i.mem_multi.l && (reglist & (1 << PC))) // Loading PC
            {
                // PC already in R0 (last one loaded)
                emit_jmp(reinterpret_cast<void*>(translation_next_bx));
                stop_here = true;
            }
        }
        else if((insn & 0xE000000) == 0xA000000)
        {
            /* Branches work this way:
             * Either jump to translation_next if code not translated (yet) or
             * jump directly to the translated code, over a small function checking for pending events */

            // We're going to jump somewhere else
            // No register mapping after this
            emit_save_state();

            if(i.branch.l)
            {
                // Save return address in LR
                emit_mov_imm(R0, pc + 4);
                emit_str_armreg(R0, LR);
            }
            else if(i.cond == CC_EQ)
            {
                // It's not likely that the branch will return
                stop_here = true;
            }

            uint32_t addr = pc + ((int32_t)(i.raw << 8) >> 6) + 8;
            uintptr_t entry = reinterpret_cast<uintptr_t>(addr_cache[(addr >> 10) << 1]);
            uint32_t *ptr = reinterpret_cast<uint32_t*>(entry + addr);
            if((entry & AC_FLAGS) || !(RAM_FLAGS(ptr) & RF_CODE_TRANSLATED))
            {
                // Not translated, use translation_next
                emit_mov_imm(R0, addr);
                emit_jmp(reinterpret_cast<void*>(translation_next));
            }
            else
            {
                // Get address of translated code to jump to it
                translation *target_translation = &translation_table[RAM_FLAGS(ptr) >> RFS_TRANSLATION_INDEX];
                uintptr_t jmp_target = reinterpret_cast<uintptr_t>(target_translation->jump_table[ptr - target_translation->start_ptr]);

                // Update pc first
                emit_mov_imm(R0, addr);
                emit_str_armreg(R0, PC);
                emit_mov_imm(R0, jmp_target);
                emit_jmp(reinterpret_cast<void*>(translation_jmp));
            }
        }
        else
            goto unimpl;

        instruction_translated:
        if(cond_branch)
        {
            // The mappings that happened during the execution may have been skipped
            // so they can't be used.
            emit_save_state();

            // Fixup the branch above (-2 to account for the pipeline)
            *cond_branch |= (translate_current - cond_branch - 2) & 0xFFFFFF;
            cond_branch = nullptr;
        }

        RAM_FLAGS(insn_ptr) |= (RF_CODE_TRANSLATED | next_translation_index << RFS_TRANSLATION_INDEX);
        ++jump_table_current;
        ++map_table_current;
        ++insn_ptr;
        pc += 4;
    }

    unimpl:
    // There may be a partial translation in memory, scrap it.
    translate_current = translate_buffer_inst_start;
    RAM_FLAGS(insn_ptr) |= RF_CODE_NO_TRANSLATE;

    exit_translation:
    emit_save_state();
    emit_mov_imm(0, pc);
    emit_jmp(reinterpret_cast<void*>(translation_next));

    #ifdef IS_IOS_BUILD
        // Mark translate_buffer as R_X
        // Even if no translation was done, pages got marked RW_
        mprotect(translate_buffer, INSN_BUFFER_SIZE, PROT_READ | PROT_EXEC);
    #endif
    
    // Did we do any translation at all?
    if(insn_ptr == insn_ptr_start)
        return;

    this_translation->end_ptr = insn_ptr;
    // This effectively flushes this_translation, as it won't get used next time
    next_translation_index += 1;

    // Flush the instruction cache
#ifdef IS_IOS_BUILD
    sys_cache_control(1 /* kCacheFunctionPrepareForExecution */, jump_table_start[0], (translate_current-jump_table_start[0])*4);
#else
    __builtin___clear_cache(jump_table_start[0], translate_current);
#endif
}

void flush_translations()
{
    for(unsigned int index = 0; index < next_translation_index; index++)
    {
        uint32_t *start = translation_table[index].start_ptr;
        uint32_t *end   = translation_table[index].end_ptr;
        for (; start < end; start++)
            RAM_FLAGS(start) &= ~(RF_CODE_TRANSLATED | (~0u << RFS_TRANSLATION_INDEX));
    }

    next_translation_index = 0;
    translate_current = translate_buffer;
    jump_table_current = jump_table;
}

void invalidate_translation(int index)
{
    if(!translation_sp)
        return flush_translations();

    uint32_t flags = RAM_FLAGS(translation_pc_ptr);
    if ((flags & RF_CODE_TRANSLATED) && (int)(flags >> RFS_TRANSLATION_INDEX) == index)
        error("Cannot modify currently executing code block.");

    flush_translations();
}

void translate_fix_pc()
{
    if (!translation_sp)
        return;

    // This is normally done when leaving the translation,
    // but since we are here, this didn't happen (longjmp)
    set_cpsr_flags(arm.cpsr_flags);

    // Not implemented: Get accurate pc back in arm.reg[15]
    assert(false);
}
-
