#ifndef AC_GRAPH_H
#define AC_GRAPH_H

#include "framebuffer.h"
#include "color.h"

// Graphics state
typedef struct {
    ACFramebuffer *fb;          // Active render target
    ACFramebuffer *screen;      // Primary screen buffer
    ACColor ink;                // Current drawing color
    uint32_t ink_packed;        // Pre-packed ARGB32
} ACGraph;

void graph_init(ACGraph *g, ACFramebuffer *screen);

// Core primitives (matching AC API)
void graph_wipe(ACGraph *g, ACColor color);
void graph_ink(ACGraph *g, ACColor color);
void graph_plot(ACGraph *g, int x, int y);
void graph_line(ACGraph *g, int x0, int y0, int x1, int y1);
void graph_box(ACGraph *g, int x, int y, int w, int h, int filled);
void graph_circle(ACGraph *g, int cx, int cy, int r, int filled);

// Effects
void graph_scroll(ACGraph *g, int dx, int dy);
void graph_blur(ACGraph *g, int strength);

// Off-screen buffer support
ACFramebuffer *graph_painting(int w, int h);
void graph_paste(ACGraph *g, ACFramebuffer *src, int dx, int dy);
void graph_page(ACGraph *g, ACFramebuffer *target); // Switch render target

#endif
