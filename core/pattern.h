#ifndef PATTERN_H
#define PATTERN_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

/* Pattern recognition. Detailed explanation in pattern.cpp. */

#ifdef __cplusplus
extern "C" {
#endif

/* A pattern function.
   interpreter_func is never null and may be called from the ARM
   interpreter context.
   jit_func can only be called from the JIT context, the ABI
   depends on the JIT arch and implementation. It may be null,
   in which case it may be quicker to not JIT the pattern and
   return it to the interpreter instead.

   The function may return false, in which case the pattern can
   not get executed for various reasons, e.g. memcpy from/to
   noncontiguous memory or MMIO region. If it returns false,
   it must not have any effect on the visible emulation state. */
typedef struct {
	bool (*interpreter_func)();
	bool (*jit_func)();
} pattern_func;

/* The first insn is split and part of the struct directly
   to allow for more efficient searching by avoiding indirect memory access.
   To allow for pattern_match to cross a page boundary, there are two calls
   to virt_mem_ptr to confirm that it is contiguously allocated in memory.
   However, that means that a pattern must not span over more than two pages,
   i.e. next_insns_size + 4 must be <= 2*1024. */
typedef struct {
    uint32_t first_insn;
    const void *next_insns;
    size_t next_insns_size;
    pattern_func func;
} pattern_entry ;

#define PATTERN_ENTRY_COUNT 2
extern const pattern_entry pattern_matches[PATTERN_ENTRY_COUNT];

void pattern_initialize();
/* phys_ptr points to the virtual RAM, virt_ptr is the address of the
   first instruction. Returns nullptr if no pattern matches. */
const pattern_func *pattern_match(const void *phys_ptr, uint32_t virt_ptr);

#ifdef __cplusplus
}
#endif

#endif
