#include <cassert>
#include <cstring>

#include "cpu.h"
#include "pattern.h"

/* Pattern recognition is used to substitute commonly used and often
   executed snippets of virtual code with optimized code running
   directly on the physical CPU.

   This can provide a huge speedup for functions such as memcpy,
   as for those memory access can be optimized substantially,
   by replacing multiple calls to read_word/write_word with
   a single call to memcpy, if possible. */

/* Size of the biggest pattern. */
static size_t pattern_max_size = 0;

void pattern_initialize()
{
	for(auto &&match : pattern_matches)
	{
		if(pattern_max_size < match.next_insns_size + 4)
			pattern_max_size = match.next_insns_size + 4;
	}

	// See comment on struct pattern_match for explanation.
	assert(pattern_max_size <= 2*1024);
}

const pattern_func *pattern_match(const void *phys_ptr, uint32_t virt_ptr)
{
	/* Verify that the next pattern_max_size bytes are contiguously mapped. */
	if(uintptr_t(try_ptr(virt_ptr + pattern_max_size)) - uintptr_t(phys_ptr) != pattern_max_size)
		return nullptr;

	uint32_t first_insn = *reinterpret_cast<const uint32_t*>(phys_ptr);

	for(auto &&match : pattern_matches)
	{
		if(match.first_insn != first_insn)
			continue;

		if(memcmp((void*)(uintptr_t(phys_ptr) + 4), match.next_insns, match.next_insns_size) == 0)
			return &match.func;
	}

	return nullptr;
}
