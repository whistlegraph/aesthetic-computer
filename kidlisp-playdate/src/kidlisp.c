// kidlisp.c - KidLisp Playdate Runtime Implementation
// Provides the graphics, input, and math API for KidLisp programs

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "kidlisp.h"

// Global state
static KidLispState state = {0};

// Fonts
static LCDFont* default_font = NULL;
static LCDFont* small_font = NULL;
static const char* font_path = "/System/Fonts/Asheville-Sans-14-Bold.pft";
static const char* small_font_path = "/System/Fonts/Roobert-10-Bold.pft";

// HUD buffer (separate bitmap composited on top)
static LCDBitmap* hud_buffer = NULL;
static const int HUD_HEIGHT = 80;

// QR Code for https://kidlisp.com (25x25)
#define QR_SIZE 25
static const uint8_t qr_data[] = {
    0xFE, 0x9A, 0x3F, 0x80,
    0x82, 0x60, 0xA0, 0x80,
    0xBA, 0x2E, 0xAE, 0x80,
    0xBA, 0x0C, 0xAE, 0x80,
    0xBA, 0x56, 0xAE, 0x80,
    0x82, 0x2A, 0xA0, 0x80,
    0xFE, 0xAA, 0xBF, 0x80,
    0x00, 0xB5, 0x80, 0x00,
    0xDA, 0x74, 0xA0, 0x80,
    0x59, 0x4F, 0x9F, 0x00,
    0x7E, 0xB5, 0x54, 0x80,
    0x45, 0xFC, 0xAF, 0x80,
    0xAF, 0x5D, 0x30, 0x80,
    0xD1, 0xC7, 0x89, 0x00,
    0xFA, 0x9D, 0x6F, 0x80,
    0x88, 0xB1, 0xB6, 0x80,
    0xB3, 0x63, 0xFB, 0x00,
    0x00, 0x9F, 0x8B, 0x00,
    0xFE, 0x70, 0xA8, 0x80,
    0x82, 0x0B, 0x89, 0x00,
    0xBA, 0xAF, 0xF8, 0x80,
    0xBA, 0xC3, 0xE1, 0x80,
    0xBA, 0x4B, 0xCF, 0x80,
    0x82, 0xC2, 0x1B, 0x80,
    0xFE, 0xA6, 0xE4, 0x80,
};

void kl_init(PlaydateAPI* pd) {
    state.pd = pd;
    state.frame = 0;
    state.ink_color = KL_BLACK;
    state.crank_angle = 0;
    state.crank_delta = 0;
    state.current_buttons = 0;
    state.pressed_buttons = 0;
    state.released_buttons = 0;
    
    // Load default font
    const char* err;
    default_font = pd->graphics->loadFont(font_path, &err);
    if (default_font == NULL) {
        pd->system->logToConsole("Failed to load font: %s", err);
    }
    
    // Load small font for HUD
    small_font = pd->graphics->loadFont(small_font_path, &err);
    if (small_font == NULL) {
        pd->system->logToConsole("Failed to load small font: %s", err);
        small_font = default_font;  // Fallback
    }
    
    // Seed random with current time
    srand(pd->system->getCurrentTimeMilliseconds());
}

void kl_update(void) {
    state.frame++;
    
    // Update crank
    state.crank_delta = state.pd->system->getCrankChange();
    state.crank_angle = state.pd->system->getCrankAngle();
    
    // Update buttons
    PDButtons current, pressed, released;
    state.pd->system->getButtonState(&current, &pressed, &released);
    state.current_buttons = current;
    state.pressed_buttons = pressed;
    state.released_buttons = released;
}

KidLispState* kl_state(void) {
    return &state;
}

// === Graphics ===

void kl_wipe(KLColor color) {
    state.pd->graphics->clear(color);
}

void kl_ink(KLColor color) {
    state.ink_color = color;
}

void kl_line(int x1, int y1, int x2, int y2) {
    state.pd->graphics->drawLine(x1, y1, x2, y2, 1, state.ink_color);
}

void kl_line_random(void) {
    int x1 = rand() % KL_WIDTH;
    int y1 = rand() % KL_HEIGHT;
    int x2 = rand() % KL_WIDTH;
    int y2 = rand() % KL_HEIGHT;
    state.pd->graphics->drawLine(x1, y1, x2, y2, 1, state.ink_color);
}

void kl_box(int x, int y, int w, int h, int filled) {
    if (filled) {
        state.pd->graphics->fillRect(x, y, w, h, state.ink_color);
    } else {
        state.pd->graphics->drawRect(x, y, w, h, state.ink_color);
    }
}

void kl_circle(int x, int y, int r, int filled) {
    // Playdate uses ellipse API (x, y, width, height)
    int diameter = r * 2;
    int left = x - r;
    int top = y - r;
    
    if (filled) {
        state.pd->graphics->fillEllipse(left, top, diameter, diameter, 0, 360, state.ink_color);
    } else {
        state.pd->graphics->drawEllipse(left, top, diameter, diameter, 1, 0, 360, state.ink_color);
    }
}

void kl_plot(int x, int y) {
    // Draw a 1x1 filled rect for a single pixel
    state.pd->graphics->fillRect(x, y, 1, 1, state.ink_color);
}

void kl_write(const char* text, int x, int y) {
    if (default_font != NULL) {
        state.pd->graphics->setFont(default_font);
    }
    state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x, y);
}

void kl_write_small(const char* text, int x, int y) {
    if (small_font != NULL) {
        state.pd->graphics->setFont(small_font);
    }
    state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x, y);
}

void kl_write_outlined(const char* text, int x, int y) {
    // Draw white outline by drawing in all 8 directions + diagonals
    if (small_font != NULL) {
        state.pd->graphics->setFont(small_font);
    }
    
    // Draw white "shadow" in all directions for outline effect
    state.pd->graphics->setDrawMode(kDrawModeFillWhite);
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx != 0 || dy != 0) {
                state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x + dx, y + dy);
            }
        }
    }
    
    // Draw black text on top
    state.pd->graphics->setDrawMode(kDrawModeFillBlack);
    state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x, y);
    
    // Reset draw mode
    state.pd->graphics->setDrawMode(kDrawModeCopy);
}

void kl_scroll(int dx, int dy) {
    // Use getFrame() for direct framebuffer access
    uint8_t* data = state.pd->graphics->getFrame();
    if (!data) return;
    
    if (dx == 0 && dy == 0) return;
    
    int rowbytes = 52;  // LCD_ROWSIZE
    int height = 240;   // LCD_ROWS
    int width_pixels = 400;
    
    // Full screen temp buffer for proper wrap-around
    static uint8_t temp_screen[52 * 240];
    
    // Copy entire framebuffer to temp first
    memcpy(temp_screen, data, rowbytes * height);
    
    // Now do the scroll with wrap-around, reading from temp, writing to data
    for (int y = 0; y < height; y++) {
        // Calculate source Y with wrap
        int src_y = y - dy;
        while (src_y < 0) src_y += height;
        while (src_y >= height) src_y -= height;
        
        uint8_t* src_row = temp_screen + src_y * rowbytes;
        uint8_t* dst_row = data + y * rowbytes;
        
        if (dx == 0) {
            // No horizontal scroll, just copy the row
            memcpy(dst_row, src_row, rowbytes);
        } else {
            // Horizontal scroll with wrap - pixel by pixel
            for (int px = 0; px < width_pixels; px++) {
                // Calculate source X with wrap
                int src_px = px - dx;
                while (src_px < 0) src_px += width_pixels;
                while (src_px >= width_pixels) src_px -= width_pixels;
                
                // Get source bit (MSB = leftmost pixel)
                int src_byte = src_px / 8;
                int src_bit = 7 - (src_px % 8);
                int src_val = (src_row[src_byte] >> src_bit) & 1;
                
                // Set dest bit
                int dst_byte = px / 8;
                int dst_bit = 7 - (px % 8);
                if (src_val) {
                    dst_row[dst_byte] |= (1 << dst_bit);
                } else {
                    dst_row[dst_byte] &= ~(1 << dst_bit);
                }
            }
        }
    }
    
    state.pd->graphics->markUpdatedRows(0, height - 1);
}

void kl_blur(int amount) {
    // 1-bit "blur" effect - erode black pixels slightly
    // This creates a fading/dissolving effect
    
    if (amount <= 0) return;
    
    // Use getFrame() for direct framebuffer access (not a copy!)
    uint8_t* data = state.pd->graphics->getFrame();
    if (!data) {
        state.pd->system->logToConsole("blur: no framebuffer!");
        return;
    }
    
    // LCD_ROWSIZE is 52 bytes per row, 240 rows
    int rowbytes = 52;  // LCD_ROWSIZE from SDK
    int height = 240;   // LCD_ROWS
    int total_bytes = height * rowbytes;
    
    // Random erosion: amount * 100 pixels per frame
    int erosion_count = amount * 100;
    
    for (int i = 0; i < erosion_count; i++) {
        int byte_idx = rand() % total_bytes;
        int bit_idx = rand() % 8;
        
        // Check if this bit is black (0) and flip to white (1)
        if ((data[byte_idx] & (0x80 >> bit_idx)) == 0) {
            data[byte_idx] |= (0x80 >> bit_idx);
        }
    }
    
    state.pd->graphics->markUpdatedRows(0, height - 1);
}

// === System ===

int kl_width(void) {
    return KL_WIDTH;
}

int kl_height(void) {
    return KL_HEIGHT;
}

int kl_frame(void) {
    return state.frame;
}

// === Input ===

float kl_crank(void) {
    return state.crank_angle;
}

float kl_crank_delta(void) {
    return state.crank_delta;
}

int kl_button_a(void) {
    return (state.current_buttons & kButtonA) != 0;
}

int kl_button_b(void) {
    return (state.current_buttons & kButtonB) != 0;
}

int kl_button_up(void) {
    return (state.current_buttons & kButtonUp) != 0;
}

int kl_button_down(void) {
    return (state.current_buttons & kButtonDown) != 0;
}

int kl_button_left(void) {
    return (state.current_buttons & kButtonLeft) != 0;
}

int kl_button_right(void) {
    return (state.current_buttons & kButtonRight) != 0;
}

int kl_button_pressed_a(void) {
    return (state.pressed_buttons & kButtonA) != 0;
}

int kl_button_pressed_b(void) {
    return (state.pressed_buttons & kButtonB) != 0;
}

// === Math ===

float kl_sin(float x) {
    // Convert degrees to radians
    return sinf(x * M_PI / 180.0f);
}

float kl_cos(float x) {
    // Convert degrees to radians  
    return cosf(x * M_PI / 180.0f);
}

int kl_random(int max) {
    if (max <= 0) return 0;
    return rand() % max;
}

int kl_wiggle(int amount) {
    if (amount <= 0) return 0;
    return kl_random(amount) - (amount / 2);
}

// === HUD Buffer ===
// Separate bitmap for HUD overlay that gets composited on top

void kl_hud_init(void) {
    if (hud_buffer == NULL) {
        hud_buffer = state.pd->graphics->newBitmap(KL_WIDTH, HUD_HEIGHT, kColorWhite);
    }
}

void kl_hud_clear(void) {
    if (hud_buffer == NULL) return;
    state.pd->graphics->clearBitmap(hud_buffer, kColorClear);  // Transparent
}

void kl_hud_write(const char* text, int x, int y) {
    if (hud_buffer == NULL) return;
    
    // Push context to draw into HUD buffer
    state.pd->graphics->pushContext(hud_buffer);
    
    // Use small font
    if (small_font != NULL) {
        state.pd->graphics->setFont(small_font);
    }
    
    // Draw white outline
    state.pd->graphics->setDrawMode(kDrawModeFillWhite);
    for (int dy = -1; dy <= 1; dy++) {
        for (int dx = -1; dx <= 1; dx++) {
            if (dx != 0 || dy != 0) {
                state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x + dx, y + dy);
            }
        }
    }
    
    // Draw black text on top
    state.pd->graphics->setDrawMode(kDrawModeFillBlack);
    state.pd->graphics->drawText(text, strlen(text), kASCIIEncoding, x, y);
    
    // Restore
    state.pd->graphics->setDrawMode(kDrawModeCopy);
    state.pd->graphics->popContext();
}

void kl_hud_composite(void) {
    if (hud_buffer == NULL) return;
    
    // Draw HUD buffer on top of main framebuffer with transparency
    state.pd->graphics->setDrawMode(kDrawModeNXOR);
    state.pd->graphics->drawBitmap(hud_buffer, 0, 0, kBitmapUnflipped);
    state.pd->graphics->setDrawMode(kDrawModeCopy);
}

// Clear HUD regions to white before effects run
// This prevents old HUD text from being scrolled into the effect
void kl_clear_hud_regions(void) {
    // Clear top-left text area (source code display)
    // Text is at (5, 4) with 8px line spacing, up to 5 lines
    // Font is ~10px tall, so clear about 50px height with some margin
    state.pd->graphics->fillRect(0, 0, 250, 50, kColorWhite);
    
    // Clear bottom-right QR area
    // QR is 25x25 at (369, 206) with 3px border = 31x31 starting at (366, 203)
    int qr_x = KL_WIDTH - QR_SIZE - 6;
    int qr_y = KL_HEIGHT - QR_SIZE - 5;
    state.pd->graphics->fillRect(qr_x - 4, qr_y - 4, QR_SIZE + 8, QR_SIZE + 8, kColorWhite);
}

void kl_draw_qr(void) {
    // Draw QR code in bottom right corner with double border (white outer, black inner)
    int qr_x = KL_WIDTH - QR_SIZE - 6;   // 6px margin from right
    int qr_y = KL_HEIGHT - QR_SIZE - 5;  // 5px margin from bottom
    
    // Draw 1px white outer border (visible on dark backgrounds)
    state.pd->graphics->fillRect(qr_x - 3, qr_y - 3, QR_SIZE + 6, QR_SIZE + 6, kColorWhite);
    
    // Draw 1px black inner border
    state.pd->graphics->fillRect(qr_x - 2, qr_y - 2, QR_SIZE + 4, QR_SIZE + 4, kColorBlack);
    
    // Draw white background
    state.pd->graphics->fillRect(qr_x - 1, qr_y - 1, QR_SIZE + 2, QR_SIZE + 2, kColorWhite);
    
    // Draw QR code pixels
    for (int row = 0; row < QR_SIZE; row++) {
        int byte_offset = row * 4;  // 4 bytes per row (25 bits padded to 32)
        for (int col = 0; col < QR_SIZE; col++) {
            int byte_idx = byte_offset + (col / 8);
            int bit_idx = 7 - (col % 8);
            if (qr_data[byte_idx] & (1 << bit_idx)) {
                // Black pixel
                state.pd->graphics->fillRect(qr_x + col, qr_y + row, 1, 1, kColorBlack);
            }
        }
    }
}
