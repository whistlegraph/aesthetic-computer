// BUILD_TARGET: CGB
#include <gb/gb.h>
#include <gb/cgb.h>
#include <gbdk/platform.h>
#include <string.h>

uint8_t current_sample = 15;

const unsigned char sprite_tile[16] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

void main(void) {
    DISPLAY_OFF;
    
    // Set up background palette - black
    BCPS_REG = 0x80;
    BCPD_REG = 0x00; BCPD_REG = 0x00;  // Black
    BCPD_REG = 0x00; BCPD_REG = 0x00;  // Black
    BCPD_REG = 0x00; BCPD_REG = 0x00;  // Black
    BCPD_REG = 0x00; BCPD_REG = 0x00;  // Black
    
    // Set up sprite palette - white using set_sprite_palette
    uint16_t white_palette = 0x7FFF;  // White color (RGB555: 31,31,31)
    set_sprite_palette(0, 1, &white_palette);
    
    set_sprite_data(0, 1, sprite_tile);
    
    SHOW_BKG;
    SHOW_SPRITES;
    DISPLAY_ON;
    
    uint8_t last_joy = 0;
    
    while(1) {
        vsync();  // Use vsync() like GBDK examples
        
        uint8_t joy = joypad();
        
        // Simple test: press LEFT to move sprite left, RIGHT to move right
        if(joy & J_LEFT) {
            if(current_sample > 0) current_sample--;
        }
        if(joy & J_RIGHT) {
            if(current_sample < 30) current_sample++;
        }
        
        // Draw a single sprite at the position
        set_sprite_tile(0, 0);
        move_sprite(0, 8 + (current_sample * 4), 80);
        set_sprite_prop(0, 0);
    }
}
