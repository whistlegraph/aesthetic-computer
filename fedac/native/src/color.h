#ifndef AC_COLOR_H
#define AC_COLOR_H

#include <stdint.h>

typedef struct {
    uint8_t r, g, b, a;
} ACColor;

// Parse color from various formats
// Returns ACColor with a=255 by default
ACColor color_parse(int argc, const int *argv);

// Pack ACColor into ARGB32 pixel
static inline uint32_t color_pack(ACColor c) {
    return ((uint32_t)c.a << 24) | ((uint32_t)c.r << 16) |
           ((uint32_t)c.g << 8) | (uint32_t)c.b;
}

// Unpack ARGB32 pixel into ACColor
static inline ACColor color_unpack(uint32_t pixel) {
    return (ACColor){
        .r = (pixel >> 16) & 0xFF,
        .g = (pixel >> 8) & 0xFF,
        .b = pixel & 0xFF,
        .a = (pixel >> 24) & 0xFF
    };
}

// Alpha blend src over dst
static inline uint32_t color_blend(uint32_t dst, uint32_t src) {
    uint8_t sa = (src >> 24) & 0xFF;
    if (sa == 255) return src;
    if (sa == 0) return dst;

    uint8_t sr = (src >> 16) & 0xFF;
    uint8_t sg = (src >> 8) & 0xFF;
    uint8_t sb = src & 0xFF;
    uint8_t dr = (dst >> 16) & 0xFF;
    uint8_t dg = (dst >> 8) & 0xFF;
    uint8_t db = dst & 0xFF;

    uint8_t r = (sr * sa + dr * (255 - sa)) / 255;
    uint8_t g = (sg * sa + dg * (255 - sa)) / 255;
    uint8_t b = (sb * sa + db * (255 - sa)) / 255;

    return (255u << 24) | ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
}

#endif
