#ifndef AC_RAYLIB_SOFT_H
#define AC_RAYLIB_SOFT_H

#include <stdint.h>

// Render a small test pattern using raylib's Image* CPU APIs (no GL/window
// context required). Writes ARGB32 pixels (packed 0xAARRGGBB) into `dst`,
// honoring `stride_pixels` for rows. Returns 0 on success, -1 on bad args.
//
// `frame` lets the pattern animate over time when called repeatedly.
int raylib_soft_test(uint32_t *dst, int width, int height,
                     int stride_pixels, int frame);

#endif
