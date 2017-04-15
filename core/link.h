/* Declarations for link.c */

#ifndef _H_LINK
#define _H_LINK

#ifdef __cplusplus
extern "C" {
#endif

void send_file(char *filename);

void ti84_io_link_reset(void);
uint32_t ti84_io_link_read(uint32_t addr);
void ti84_io_link_write(uint32_t addr, uint32_t value);

#ifdef __cplusplus
}
#endif

#endif
