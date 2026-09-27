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
    if (!src || !dst || dst_w <= 0 || dst_h <= 0 || scale <= 0 ||
        src->width <= 0 || src->height <= 0 || dst_stride < dst_w) return;
    // DRM dumb buffers are write-combined memory. Reading a just-written
    // scanline back from one (to duplicate it) can cost an entire frame on
    // Intel laptop graphics. Expand in ordinary cached RAM, then only write
    // to the scanout buffer. Both the duplicated rows and edge fill use RAM.
    uint32_t *expanded = malloc((size_t)dst_w * sizeof(uint32_t));
    if (!expanded) return;
    // Fast path: expand each source pixel to scale×scale block
    // Avoids per-pixel division, uses memcpy for row duplication
    int src_h = src->height;
    int src_w = src->width;
    int max_dy = dst_h < src_h * scale ? dst_h : src_h * scale;
    int max_dx = dst_w < src_w * scale ? dst_w : src_w * scale;

    for (int sy = 0; sy < src_h && sy * scale < dst_h; sy++) {
        uint32_t *src_row = src->pixels + sy * src->stride;
        uint32_t *dst_row = expanded;

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

        // Write every copy from cached RAM, including the first row.
        for (int r = 0; r < scale && (sy * scale + r) < dst_h; r++) {
            memcpy(dst + (sy * scale + r) * dst_stride, dst_row, (size_t)dst_w * sizeof(uint32_t));
        }
    }
    // Fill any remaining rows at bottom
    if (max_dy < dst_h && max_dy > 0) {
        uint32_t *last_row = expanded;
        for (int dy = max_dy; dy < dst_h; dy++)
            memcpy(dst + dy * dst_stride, last_row, (size_t)dst_w * sizeof(uint32_t));
    }
    free(expanded);
}

void fb_copy_resized(ACFramebuffer *src, uint32_t *dst, int dst_w, int dst_h, int dst_stride) {
    if (!src || !src->pixels || !dst || dst_w<=0 || dst_h<=0 ||
        src->width<=0 || src->height<=0 || dst_stride<dst_w) return;
    uint32_t *row=malloc((size_t)dst_w*sizeof(*row));
    int *columns=malloc((size_t)dst_w*sizeof(*columns));
    if (!row || !columns) { free(row);free(columns);return; }
    for(int x=0;x<dst_w;x++) columns[x]=(int)((int64_t)x*src->width/dst_w);
    int previous=-1;
    for(int y=0;y<dst_h;y++) {
        int sy=(int)((int64_t)y*src->height/dst_h);
        if(sy!=previous) {
            const uint32_t *source=src->pixels+(size_t)sy*src->stride;
            for(int x=0;x<dst_w;x++) row[x]=source[columns[x]];
            previous=sy;
        }
        // Never read the uncached scanout buffer to duplicate a row.
        memcpy(dst+(size_t)y*dst_stride,row,(size_t)dst_w*sizeof(*row));
    }
    free(columns);free(row);
}
