// raylib-soft.c — software rendering bridge into AC native's ARGB32 buffers.
//
// Built only when pkg-config finds raylib (HAVE_RAYLIB). Uses the Image*
// CPU APIs exclusively, so no GL context, no InitWindow, and no windowing
// libraries are touched at runtime — Fedora's libraylib.so dlopens those
// only when its windowing path is exercised.

#include "raylib-soft.h"

#include <math.h>
#include <stdint.h>
#include <string.h>

#include <raylib.h>

int raylib_soft_test(uint32_t *dst, int width, int height,
                     int stride_pixels, int frame) {
    if (!dst || width <= 0 || height <= 0 || stride_pixels < width) {
        return -1;
    }

    // Render into a temporary RGBA8 image so we never touch GPU paths.
    Image img = GenImageColor(width, height, (Color){ 12, 14, 22, 255 });
    if (!img.data) return -1;

    // Animated diagonal stripes — clear visual confirmation that pixels
    // came out of raylib rather than ac-native's own primitives.
    float t = (float)frame * 0.05f;
    for (int y = 0; y < height; y++) {
        int phase = (int)(y + sinf(t) * 24.0f);
        for (int x = 0; x < width; x++) {
            int v = (x + phase) & 31;
            if (v < 6) {
                Color c = {
                    (unsigned char)(60 + v * 12),
                    (unsigned char)(40 + ((y * 255) / height)),
                    (unsigned char)(120 + ((x * 100) / width)),
                    255,
                };
                ImageDrawPixel(&img, x, y, c);
            }
        }
    }

    // Centerpiece: a wobbling circle + crosshair to show shape primitives.
    int cx = width / 2;
    int cy = height / 2;
    int r = (width < height ? width : height) / 5;
    int wobble = (int)(sinf(t * 1.7f) * (r * 0.25f));
    ImageDrawCircle(&img, cx + wobble, cy, r,
                    (Color){ 255, 200, 80, 255 });
    ImageDrawCircleLines(&img, cx + wobble, cy, r + 4,
                         (Color){ 255, 255, 255, 255 });
    ImageDrawLine(&img, 0, cy, width - 1, cy,
                  (Color){ 255, 100, 100, 180 });
    ImageDrawLine(&img, cx, 0, cx, height - 1,
                  (Color){ 100, 255, 100, 180 });

    // Banner — uses raylib's bundled default font, also pure CPU.
    ImageDrawText(&img, "raylib soft", 8, 8, 20,
                  (Color){ 230, 230, 255, 255 });

    // Convert to RGBA8 (no-op if already, but cheap insurance) and copy
    // into the caller's ARGB32 buffer with stride.
    ImageFormat(&img, PIXELFORMAT_UNCOMPRESSED_R8G8B8A8);
    const unsigned char *src = (const unsigned char *)img.data;
    for (int y = 0; y < height; y++) {
        uint32_t *row = dst + (y * stride_pixels);
        for (int x = 0; x < width; x++) {
            const unsigned char *p = src + ((y * width + x) * 4);
            uint32_t r8 = p[0], g8 = p[1], b8 = p[2], a8 = p[3];
            row[x] = (a8 << 24) | (r8 << 16) | (g8 << 8) | b8;
        }
    }

    UnloadImage(img);
    return 0;
}
