#ifndef AC_FONT_H
#define AC_FONT_H

#include "graph.h"

// Simple 8x8 embedded bitmap font
#define FONT_CHAR_W 8
#define FONT_CHAR_H 8

// Font IDs
#define FONT_8X8    0
#define FONT_MATRIX 1

// Initialize the built-in font
void font_init(void);

// Draw text at position using current ink color
// Returns the x position after the last character (for chaining)
int font_draw(ACGraph *g, const char *text, int x, int y, int scale);

// Draw text using MatrixChunky8 BDF font
// Returns x position after last character
int font_draw_matrix(ACGraph *g, const char *text, int x, int y, int scale);

// Measure text width in pixels
int font_measure(const char *text, int scale);

// Measure text width using MatrixChunky8 font
int font_measure_matrix(const char *text, int scale);

#endif
