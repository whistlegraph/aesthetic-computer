// kidlisp.h - KidLisp Playdate Runtime Header
// Defines the API available to KidLisp programs on Playdate

#ifndef KIDLISP_H
#define KIDLISP_H

#include "pd_api.h"

// Screen dimensions
#define KL_WIDTH LCD_COLUMNS   // 400
#define KL_HEIGHT LCD_ROWS     // 240

// Color constants (1-bit display)
typedef enum {
    KL_BLACK = kColorBlack,
    KL_WHITE = kColorWhite
} KLColor;

// Runtime state
typedef struct {
    PlaydateAPI* pd;
    int frame;
    KLColor ink_color;
    float crank_angle;
    float crank_delta;
    PDButtons current_buttons;
    PDButtons pressed_buttons;
    PDButtons released_buttons;
} KidLispState;

// Initialize KidLisp runtime
void kl_init(PlaydateAPI* pd);

// Called each frame to update state
void kl_update(void);

// Get runtime state
KidLispState* kl_state(void);

// === Graphics Functions ===

// Clear screen
void kl_wipe(KLColor color);

// Set drawing color
void kl_ink(KLColor color);

// Draw line (with coords or random)
void kl_line(int x1, int y1, int x2, int y2);
void kl_line_random(void);

// Draw rectangle (outline or fill)
void kl_box(int x, int y, int w, int h, int filled);

// Draw circle (outline or fill)
void kl_circle(int x, int y, int r, int filled);

// Draw single pixel
void kl_plot(int x, int y);

// Draw text
void kl_write(const char* text, int x, int y);

// Draw text with white outline (for HUD)
void kl_write_outlined(const char* text, int x, int y);

// Draw small text (for HUD source lines)
void kl_write_small(const char* text, int x, int y);

// HUD buffer management
void kl_hud_init(void);
void kl_hud_clear(void);
void kl_hud_write(const char* text, int x, int y);
void kl_hud_composite(void);

// Clear HUD regions before effects (prevents scrolling old HUD)
void kl_clear_hud_regions(void);

// Draw QR code (bottom right corner)
void kl_draw_qr(void);

// Scroll the screen buffer by (dx, dy) pixels
void kl_scroll(int dx, int dy);

// Simple 1-bit "blur" - erode/dilate effect
void kl_blur(int amount);

// === System Functions ===

// Get screen dimensions
int kl_width(void);
int kl_height(void);

// Get current frame number
int kl_frame(void);

// === Input Functions (Playdate-specific) ===

// Get crank angle (0-360 degrees)
float kl_crank(void);

// Get crank change since last frame
float kl_crank_delta(void);

// Button state checks
int kl_button_a(void);
int kl_button_b(void);
int kl_button_up(void);
int kl_button_down(void);
int kl_button_left(void);
int kl_button_right(void);

// Button just pressed this frame
int kl_button_pressed_a(void);
int kl_button_pressed_b(void);

// === Math Functions ===

float kl_sin(float x);
float kl_cos(float x);
int kl_random(int max);
int kl_wiggle(int amount);

#endif // KIDLISP_H
