#include <gb/gb.h>
#include <stdint.h>
#include <gb/drawing.h>
#include "hUGEDriver.h"

// Generated melody pattern
static const unsigned char melody_pattern[] = {
    DN(C_4,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(E_4,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(G_4,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
    DN(___,0,0x000),
};

static const unsigned char order_cnt = 1;
static const unsigned char* const order1[] = {melody_pattern};
static const unsigned char* const order2[] = {melody_pattern};
static const unsigned char* const order3[] = {melody_pattern};
static const unsigned char* const order4[] = {melody_pattern};

static const hUGEDutyInstr_t duty_instruments[] = {{0,0,0,0,0}};
static const hUGEWaveInstr_t wave_instruments[] = {{0,0,NULL,0}};
static const hUGENoiseInstr_t noise_instruments[] = {{0,NULL,0,0,0}};
static const unsigned char waves[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

const hUGESong_t kidlisp_song = {150, &order_cnt, order1, order2, order3, order4, duty_instruments, wave_instruments, noise_instruments, NULL, waves};

// Simple line drawing using Bresenham's algorithm
void draw_line(uint8_t x0, uint8_t y0, uint8_t x1, uint8_t y1) {
    int16_t dx = x1 > x0 ? x1 - x0 : x0 - x1;
    int16_t dy = y1 > y0 ? y1 - y0 : y0 - y1;
    int16_t sx = x0 < x1 ? 1 : -1;
    int16_t sy = y0 < y1 ? 1 : -1;
    int16_t err = dx - dy;
    while (1) {
        plot_point(x0, y0);
        if (x0 == x1 && y0 == y1) break;
        int16_t e2 = 2 * err;
        if (e2 > -dy) { err -= dy; x0 += sx; }
        if (e2 < dx) { err += dx; y0 += sy; }
    }
}

void main(void) {
    // Initialize sound
    NR52_REG = 0x80;
    NR50_REG = 0xFF;
    NR51_REG = 0xFF;

    __critical {
        hUGE_init(&kidlisp_song);
        add_VBL(hUGE_dosound);
    }
    set_interrupts(VBL_IFLAG);

    // Graphics setup
    DISPLAY_ON;
    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);

    // Execute KidLisp commands
    // Wipe screen to BLACK
    fill_rect(0, 0, 160, 144, 0);
    // Set ink color to WHITE
    draw_line(10, 10, 150, 134);
    draw_line(150, 10, 10, 134);
    draw_line(80, 10, 80, 134);
    draw_line(10, 72, 150, 72);
    draw_line(40, 40, 120, 100);
    draw_line(120, 40, 40, 100);

    // Main loop
    while(1) {
        vsync();
    }
}
