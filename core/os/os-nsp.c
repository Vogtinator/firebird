#include <stdio.h>


FILE *fopen_utf8(const char *name, const char *flags)
{
return fopen(name, flags);
}

_Bool addr_cache_init() { return 0; }

_Bool gdbstub_init() { return 1; }
void gdbstub_reset() {}
void gdbstub_recv() {}
void gdbstub_quit() {}
void gdbstub_debugger() { abort(); }
void os_commit() {}
void os_sparse_commit() {}
void os_sparse_decommit() {}
void os_free(void *p) { free(p); }
void *os_reserve(size_t size) { return (void*)0x11e00000; }
