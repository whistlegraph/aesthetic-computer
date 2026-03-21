#include "color.h"

ACColor color_parse(int argc, const int *argv) {
    ACColor c = {0, 0, 0, 255};
    if (argc >= 3) {
        c.r = (uint8_t)(argv[0] & 0xFF);
        c.g = (uint8_t)(argv[1] & 0xFF);
        c.b = (uint8_t)(argv[2] & 0xFF);
        if (argc >= 4)
            c.a = (uint8_t)(argv[3] & 0xFF);
    } else if (argc == 1) {
        // Single grayscale value
        c.r = c.g = c.b = (uint8_t)(argv[0] & 0xFF);
    }
    return c;
}
