#include <array>
#include <mutex>
#include <vector>

#include "emu.h"
#include "gif.h"
#include "lcd.h"

// We can't modify giflib.h ourselves, so #include that here.
#include "os/os.h"

struct RGB24 {
    uint8_t r, g, b, a;
};

static bool recording = false;
static std::vector<RGB24> buffer;
static unsigned int framenr = 0, framenrskip = 0, framedelay = 0;

bool gif_start_recording(const char *filename, unsigned int frameskip)
{
    framenr = framenrskip = frameskip;
    framedelay = 100 / (60/(frameskip+1));

    FILE *gif_file = fopen_utf8(filename, "wb");

    buffer.resize(320*240);

    return recording;
}

void gif_new_frame()
{
    if(!recording)
        return;

    if(!recording || --framenr)
        return;

    framenr = framenrskip;

    static std::array<uint16_t, 320 * 240> framebuffer;

    lcd_cx_draw_frame(framebuffer.data());

    uint16_t *ptr16 = framebuffer.data();
    RGB24 *ptr24 = buffer.data();

    /* Convert RGB565 or RGB444 to RGBA8888 */
    if(emulate_cx)
    {
        for(unsigned int i = 0; i < 320*240; ++i)
        {
            ptr24->r = (*ptr16 & 0b1111100000000000) >> 8;
            ptr24->g = (*ptr16 & 0b0000011111100000) >> 3;
            ptr24->b = (*ptr16 & 0b0000000000011111) << 3;
            ++ptr24;
            ++ptr16;
        }
    }
    else
    {
        for(unsigned int i = 0; i < 320*240; ++i)
        {
            uint8_t pix = ~(*ptr16 & 0xF);
            ptr24->r = pix << 4;
            ptr24->g = pix << 4;
            ptr24->b = pix << 4;
            ++ptr24;
            ++ptr16;
        }
    }

}

bool gif_stop_recording()
{
    bool ret = recording;

    recording = false;

    buffer.clear();
    return ret;
}
