#ifndef AC_FONT_H
#define AC_FONT_H

#include "graph.h"

// Simple 8x8 embedded bitmap font
#define FONT_CHAR_W 8
#define FONT_CHAR_H 8

// Initialize the built-in font
void font_init(void);

// Draw text at position using current ink color
// Returns the x position after the last character (for chaining)
int font_draw(ACGraph *g, const char *text, int x, int y, int scale);

// Measure text width in pixels
int font_measure(const char *text, int scale);

#endif
