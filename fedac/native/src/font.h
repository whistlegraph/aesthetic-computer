#ifndef AC_FONT_H
#define AC_FONT_H

#include "graph.h"

// Simple 8x8 embedded bitmap font
#define FONT_CHAR_W 8
#define FONT_CHAR_H 8

// 6x10 fixed-width font metrics
#define FONT_6X10_CHAR_W 6
#define FONT_6X10_CHAR_H 10

// Font IDs
#define FONT_8X8    0
#define FONT_MATRIX 1
#define FONT_6X10   2

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

// Draw text using 6x10 fixed-width BDF font (standard AC write font)
int font_draw_6x10(ACGraph *g, const char *text, int x, int y, int scale);

// Measure text width using 6x10 font
int font_measure_6x10(const char *text, int scale);

#endif
