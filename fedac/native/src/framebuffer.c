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
    for (int dy = 0; dy < dst_h; dy++) {
        int sy = dy / scale;
        if (sy >= src->height) sy = src->height - 1;
        uint32_t *src_row = src->pixels + sy * src->stride;
        uint32_t *dst_row = dst + dy * dst_stride;
        for (int dx = 0; dx < dst_w; dx++) {
            int sx = dx / scale;
            if (sx >= src->width) sx = src->width - 1;
            dst_row[dx] = src_row[sx];
        }
    }
}
