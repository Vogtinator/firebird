#include "cpu.h"
#include "emu.h"
#include "pattern.h"

static const uint32_t pattern_memcpy_insns[53] =
{
    0xe92d4011, 0xe0213000, 0xe2133003, 0x1a000028,
    0xe2104003, 0x0a000008, 0xe3520005, 0xba000024,
    0xe3a03004, 0xe0433004, 0xe0422003, 0xe4d1c001,
    0xe4c0c001, 0xe2533001, 0x1afffffb, 0xe3520004,
    0xba00001b, 0xe3520008, 0xba000016, 0xe3520010,
    0xba00000f, 0xe3520020, 0xba000008, 0xe8b15018,
    0xe8a05018, 0xe8b15018, 0xe8a05018, 0xe2422020,
    0xe3520020, 0xaafffff8, 0xe3520010, 0xba000002,
    0xe8b15018, 0xe8a05018, 0xe2422010, 0xe3520008,
    0xba000002, 0xe8b14008, 0xe8a04008, 0xe2422008,
    0xe3520004, 0xba000002, 0xe491c004, 0xe480c004,
    0xe2422004, 0xe3520000, 0x0a000003, 0xe4d13001,
    0xe4c03001, 0xe2522001, 0x1afffffb, 0xe8bd4011,
    0xe12fff1e
};

bool pattern_memcpy_interp()
{
    ssize_t size = arm.reg[2];

    if(!size)
    {
        arm.reg[15] = arm.reg[14];
        cycle_count_delta += 1;
        return true;
    }

    uint8_t *dest = reinterpret_cast<uint8_t*>(try_ptr(arm.reg[0])),
            *src = reinterpret_cast<uint8_t*>(try_ptr(arm.reg[1]));

    if(!dest || !src)
        return false;

    uint8_t *dest_end = reinterpret_cast<uint8_t*>(try_ptr(arm.reg[0] + size - 1)),
    *src_end = reinterpret_cast<uint8_t*>(try_ptr(arm.reg[1] + size - 1));

    // Not contiguous.
    // TODO: This is wrong if this spans more than two pages.
    if(std::ptrdiff_t(src_end - src) != size - 1 || std::ptrdiff_t(dest_end - dest) != size - 1)
        return false;

    memcpy(dest, src, size);

    // No need to set r0 to dest, it already is
    arm.reg[15] = arm.reg[14]; // bx lr

    cycle_count_delta += size * 4;

    return true;
}

static const pattern_func pattern_memcpy_func = {
    .interpreter_func = pattern_memcpy_interp,
    .jit_func = nullptr,
};

static const uint32_t pattern_8bparity_insns[7] =
{
    0xe59f3010, 0xe0200220, 0xe200000f, 0xe1a00053,
    0xe2000001, 0xe12fff1e, 0x00006996
};

bool pattern_8bparity_interp()
{
    uint32_t value = arm.reg[0];
    arm.reg[0] = (0x6996 >> ((value ^ (value >> 4)) & 0xF)) & 1;

    arm.reg[15] = arm.reg[14]; // bx lr

    cycle_count_delta += 8;

    return true;
}

static const pattern_func pattern_8bparity_func = {
    .interpreter_func = pattern_8bparity_interp,
    .jit_func = nullptr,
};

const pattern_entry pattern_matches[PATTERN_ENTRY_COUNT] = {
    {.first_insn = pattern_memcpy_insns[0], .next_insns = pattern_memcpy_insns + 1, .next_insns_size = sizeof(pattern_memcpy_insns) - 4, .func = pattern_memcpy_func},
    {.first_insn = pattern_8bparity_insns[0], .next_insns = pattern_8bparity_insns + 1, .next_insns_size = sizeof(pattern_8bparity_insns) - 4, .func = 
pattern_8bparity_func},
};
