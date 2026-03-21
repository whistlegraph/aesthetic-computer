#include "framebuffer.h"
#include <stdlib.h>
#include <string.h>

ACFramebuffer *fb_create(int width, int height) {
    ACFramebuffer *fb = calloc(1, sizeof(ACFramebuffer));
    if (!fb) return NULL;
    fb->width = width;
    fb->height = height;
    fb->stride = width;
    fb->pixels = calloc((size_t)width * height, sizeof(uint32_t));
    if (!fb->pixels) { free(fb); return NULL; }
    return fb;
}

void fb_destroy(ACFramebuffer *fb) {
    if (fb) {
        free(fb->pixels);
        free(fb);
    }
}

void fb_clear(ACFramebuffer *fb, uint32_t color) {
    size_t total = (size_t)fb->width * fb->height;
    // Use memset for black/white, loop for other colors
    if (color == 0xFF000000) {
        memset(fb->pixels, 0, total * 4);
        // Fix alpha
        for (size_t i = 0; i < total; i++)
            fb->pixels[i] = 0xFF000000;
    } else {
        for (size_t i = 0; i < total; i++)
            fb->pixels[i] = color;
    }
}

void fb_copy_to(ACFramebuffer *src, uint32_t *dst, int dst_stride) {
    for (int y = 0; y < src->height; y++) {
        memcpy(dst + y * dst_stride,
               src->pixels + y * src->stride,
               (size_t)src->width * sizeof(uint32_t));
    }
}

void fb_copy_scaled(ACFramebuffer *src, uint32_t *dst, int dst_w, int dst_h, int dst_stride, int scale) {
    // Fast path: expand each source pixel to scale×scale block
    // Avoids per-pixel division, uses memcpy for row duplication
    int src_h = src->height;
    int src_w = src->width;
    int max_dy = dst_h < src_h * scale ? dst_h : src_h * scale;
    int max_dx = dst_w < src_w * scale ? dst_w : src_w * scale;

    for (int sy = 0; sy < src_h && sy * scale < dst_h; sy++) {
        uint32_t *src_row = src->pixels + sy * src->stride;
        uint32_t *dst_row = dst + sy * scale * dst_stride;

        // Expand source row: each pixel repeated 'scale' times
        int dx = 0;
        for (int sx = 0; sx < src_w && dx < max_dx; sx++) {
            uint32_t p = src_row[sx];
            for (int r = 0; r < scale && dx < max_dx; r++)
                dst_row[dx++] = p;
        }
        // Fill remaining dst width with last pixel or black
        for (; dx < dst_w; dx++)
            dst_row[dx] = dx > 0 ? dst_row[dx - 1] : 0;

        // Duplicate this row (scale-1) more times
        for (int r = 1; r < scale && (sy * scale + r) < dst_h; r++) {
            memcpy(dst + (sy * scale + r) * dst_stride, dst_row, (size_t)dst_w * sizeof(uint32_t));
        }
    }
    // Fill any remaining rows at bottom
    if (max_dy < dst_h && max_dy > 0) {
        uint32_t *last_row = dst + (max_dy - 1) * dst_stride;
        for (int dy = max_dy; dy < dst_h; dy++)
            memcpy(dst + dy * dst_stride, last_row, (size_t)dst_w * sizeof(uint32_t));
    }
}
