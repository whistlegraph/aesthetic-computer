#ifndef AC_FRAMEBUFFER_H
#define AC_FRAMEBUFFER_H

#include <stdint.h>

typedef struct {
    uint32_t *pixels;   // ARGB32 pixel buffer
    int width;
    int height;
    int stride;         // Pixels per row (may differ from width for alignment)
} ACFramebuffer;

ACFramebuffer *fb_create(int width, int height);
void fb_destroy(ACFramebuffer *fb);
void fb_clear(ACFramebuffer *fb, uint32_t color);
void fb_copy_to(ACFramebuffer *src, uint32_t *dst, int dst_stride);

// Direct pixel access with bounds checking
static inline void fb_put_pixel(ACFramebuffer *fb, int x, int y, uint32_t color) {
    if (x >= 0 && x < fb->width && y >= 0 && y < fb->height)
        fb->pixels[y * fb->stride + x] = color;
}

static inline void fb_blend_pixel(ACFramebuffer *fb, int x, int y, uint32_t color) {
    if (x >= 0 && x < fb->width && y >= 0 && y < fb->height) {
        int idx = y * fb->stride + x;
        uint8_t sa = (color >> 24) & 0xFF;
        if (sa == 255) {
            fb->pixels[idx] = color;
        } else if (sa > 0) {
            uint32_t dst = fb->pixels[idx];
            uint8_t sr = (color >> 16) & 0xFF, sg = (color >> 8) & 0xFF, sb = color & 0xFF;
            uint8_t dr = (dst >> 16) & 0xFF, dg = (dst >> 8) & 0xFF, db = dst & 0xFF;
            uint8_t r = (sr * sa + dr * (255 - sa)) / 255;
            uint8_t g = (sg * sa + dg * (255 - sa)) / 255;
            uint8_t b = (sb * sa + db * (255 - sa)) / 255;
            fb->pixels[idx] = (255u << 24) | ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
        }
    }
}

static inline uint32_t fb_get_pixel(ACFramebuffer *fb, int x, int y) {
    if (x >= 0 && x < fb->width && y >= 0 && y < fb->height)
        return fb->pixels[y * fb->stride + x];
    return 0;
}

#endif
