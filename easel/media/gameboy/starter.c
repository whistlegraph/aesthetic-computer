// Easel joypad sketch, built with Aesthetic Computer's GBDK workflow.
// D-pad moves the original mark. A returns it to the center.
#include <gb/gb.h>
#include <stdint.h>
#include <stdio.h>

const uint8_t mark[] = {
    0x18, 0x18, 0x3c, 0x3c, 0x7e, 0x7e, 0xff, 0xff,
    0xff, 0xff, 0x7e, 0x7e, 0x3c, 0x3c, 0x18, 0x18
};

void main(void) {
    uint8_t x = 84, y = 88;
    printf("EASEL\n\nGAME BOY SKETCH\n\nD-PAD: MOVE\nA: CENTER");
    set_sprite_data(0, 1, mark);
    set_sprite_tile(0, 0);
    OBP0_REG = 0xe4;
    SHOW_SPRITES;
    DISPLAY_ON;
    while (1) {
        uint8_t keys;
        vsync();
        keys = joypad();
        if ((keys & J_LEFT) && x > 8) --x;
        if ((keys & J_RIGHT) && x < 160) ++x;
        if ((keys & J_UP) && y > 16) --y;
        if ((keys & J_DOWN) && y < 152) ++y;
        if (keys & J_A) { x = 84; y = 88; }
        move_sprite(0, x, y);
    }
}
