#include "graph.h"
#include <stdlib.h>
#include <string.h>

void graph_init(ACGraph *g, ACFramebuffer *screen) {
    g->fb = screen;
    g->screen = screen;
    g->ink = (ACColor){255, 255, 255, 255};
    g->ink_packed = 0xFFFFFFFF;
}

void graph_wipe(ACGraph *g, ACColor color) {
    fb_clear(g->fb, color_pack(color));
}

void graph_ink(ACGraph *g, ACColor color) {
    g->ink = color;
    g->ink_packed = color_pack(color);
}

void graph_plot(ACGraph *g, int x, int y) {
    if (g->ink.a == 255)
        fb_put_pixel(g->fb, x, y, g->ink_packed);
    else
        fb_blend_pixel(g->fb, x, y, g->ink_packed);
}

// Bresenham's line algorithm
void graph_line(ACGraph *g, int x0, int y0, int x1, int y1) {
    int dx = abs(x1 - x0);
    int dy = -abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1;
    int sy = y0 < y1 ? 1 : -1;
    int err = dx + dy;

    for (;;) {
        graph_plot(g, x0, y0);
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 >= dy) { err += dy; x0 += sx; }
        if (e2 <= dx) { err += dx; y0 += sy; }
    }
}

void graph_box(ACGraph *g, int x, int y, int w, int h, int filled) {
    if (filled) {
        // Filled rectangle
        for (int row = y; row < y + h; row++) {
            for (int col = x; col < x + w; col++) {
                graph_plot(g, col, row);
            }
        }
    } else {
        // Outline rectangle
        graph_line(g, x, y, x + w - 1, y);
        graph_line(g, x + w - 1, y, x + w - 1, y + h - 1);
        graph_line(g, x + w - 1, y + h - 1, x, y + h - 1);
        graph_line(g, x, y + h - 1, x, y);
    }
}

// Midpoint circle algorithm
void graph_circle(ACGraph *g, int cx, int cy, int r, int filled) {
    int x = 0, y = r;
    int d = 1 - r;

    while (x <= y) {
        if (filled) {
            // Draw horizontal spans for filled circle
            for (int i = cx - x; i <= cx + x; i++) {
                graph_plot(g, i, cy + y);
                graph_plot(g, i, cy - y);
            }
            for (int i = cx - y; i <= cx + y; i++) {
                graph_plot(g, i, cy + x);
                graph_plot(g, i, cy - x);
            }
        } else {
            // Draw 8 symmetric points
            graph_plot(g, cx + x, cy + y);
            graph_plot(g, cx - x, cy + y);
            graph_plot(g, cx + x, cy - y);
            graph_plot(g, cx - x, cy - y);
            graph_plot(g, cx + y, cy + x);
            graph_plot(g, cx - y, cy + x);
            graph_plot(g, cx + y, cy - x);
            graph_plot(g, cx - y, cy - x);
        }
        if (d < 0) {
            d += 2 * x + 3;
        } else {
            d += 2 * (x - y) + 5;
            y--;
        }
        x++;
    }
}

void graph_scroll(ACGraph *g, int dx, int dy) {
    ACFramebuffer *fb = g->fb;
    size_t buf_size = (size_t)fb->width * fb->height * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    for (int y = 0; y < fb->height; y++) {
        int src_y = y - dy;
        // Wrap
        while (src_y < 0) src_y += fb->height;
        while (src_y >= fb->height) src_y -= fb->height;

        for (int x = 0; x < fb->width; x++) {
            int src_x = x - dx;
            while (src_x < 0) src_x += fb->width;
            while (src_x >= fb->width) src_x -= fb->width;

            fb->pixels[y * fb->stride + x] = tmp[src_y * fb->stride + src_x];
        }
    }
    free(tmp);
}

void graph_blur(ACGraph *g, int strength) {
    ACFramebuffer *fb = g->fb;
    if (strength <= 0) return;

    size_t buf_size = (size_t)fb->width * fb->height * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    int radius = strength;
    // Simple box blur
    for (int y = 0; y < fb->height; y++) {
        for (int x = 0; x < fb->width; x++) {
            int r = 0, gr = 0, b = 0, count = 0;
            for (int dy = -radius; dy <= radius; dy++) {
                for (int dx = -radius; dx <= radius; dx++) {
                    int sx = x + dx, sy = y + dy;
                    if (sx >= 0 && sx < fb->width && sy >= 0 && sy < fb->height) {
                        uint32_t p = tmp[sy * fb->stride + sx];
                        r += (p >> 16) & 0xFF;
                        gr += (p >> 8) & 0xFF;
                        b += p & 0xFF;
                        count++;
                    }
                }
            }
            fb->pixels[y * fb->stride + x] = (0xFFu << 24) |
                (((uint32_t)(r / count)) << 16) |
                (((uint32_t)(gr / count)) << 8) |
                (uint32_t)(b / count);
        }
    }
    free(tmp);
}

ACFramebuffer *graph_painting(int w, int h) {
    return fb_create(w, h);
}

void graph_paste(ACGraph *g, ACFramebuffer *src, int dx, int dy) {
    for (int y = 0; y < src->height; y++) {
        for (int x = 0; x < src->width; x++) {
            uint32_t pixel = src->pixels[y * src->stride + x];
            fb_blend_pixel(g->fb, dx + x, dy + y, pixel);
        }
    }
}

void graph_page(ACGraph *g, ACFramebuffer *target) {
    g->fb = target ? target : g->screen;
}
