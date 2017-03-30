#include "asmcode.h"
#include "debug.h"
#include "mmu.h"
#include "mem.h"

//TODO: Read breakpoints, alignment checks

#if (!defined(__i386__) && !defined(__x86_64__) && !defined(__arm__)) || defined(NO_TRANSLATION)
void flush_translations() {}
bool range_translated(uintptr_t x, uintptr_t y) { (void) x; (void) y; return false; }
#endif

uint32_t FASTCALL read_word(uint32_t addr)
{
    uint32_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
        return *ptr;
    return mmio_read_word(addr);
}

uint8_t FASTCALL read_byte(uint32_t addr)
{
    uint32_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
        return *ptr;
    return mmio_read_byte(addr);
}

uint16_t FASTCALL read_half(uint32_t addr)
{
    uint32_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
        return *ptr;
    return mmio_read_half(addr);
}
void FASTCALL write_word(uint32_t addr, uint32_t value)
{
    uint32_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
         *ptr = value;
else
mmio_write_word(addr, value);
}

void FASTCALL write_byte(uint32_t addr, uint8_t value)
{
    uint8_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
        *ptr = value;
else
    mmio_write_byte(addr, value);
}

void FASTCALL write_half(uint32_t addr, uint16_t value)
{
    uint16_t *ptr = addr_cache_miss(addr, false, data_abort);
    if(ptr)
        *ptr = value;
else
    mmio_write_half(addr, value);
}
