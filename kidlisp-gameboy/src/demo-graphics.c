// Simple animated graphics demo for GameBoy
// Draws bouncing pixels and simple shapes

#include <gb/gb.h>
#include <stdint.h>

// Simple tile with a single pixel in top-left corner
const uint8_t pixel_tile[] = {
    0xFF, 0xFF,  // Row 0: all pixels on
    0xFF, 0xFF,  // Row 1
    0xFF, 0xFF,  // Row 2
    0xFF, 0xFF,  // Row 3
    0xFF, 0xFF,  // Row 4
    0xFF, 0xFF,  // Row 5
    0xFF, 0xFF,  // Row 6
    0xFF, 0xFF   // Row 7
};

// Bouncing ball state
int16_t ball_x = 80;
int16_t ball_y = 72;
int8_t ball_dx = 2;
int8_t ball_dy = 1;

void init_graphics(void) {
    uint8_t tile_num = 0;
    
    // Load our pixel tile into VRAM
    set_bkg_data(0, 1, pixel_tile);
    
    // Fill background with tile 0
    for(uint8_t y = 0; y < 18; y++) {
        for(uint8_t x = 0; x < 20; x++) {
            set_bkg_tiles(x, y, 1, 1, &tile_num);
        }
    }
    
    // Turn on the background
    SHOW_BKG;
    DISPLAY_ON;
}

void update_ball(void) {
    // Update position
    ball_x += ball_dx;
    ball_y += ball_dy;
    
    // Bounce off edges
    if(ball_x <= 0 || ball_x >= 152) {
        ball_dx = -ball_dx;
    }
    if(ball_y <= 0 || ball_y >= 136) {
        ball_dy = -ball_dy;
    }
}

void draw_pattern(void) {
    // Draw a simple animated pattern
    static uint8_t frame = 0;
    uint8_t tile_num = 0;
    
    // Draw diagonal lines
    for(uint8_t i = 0; i < 18; i++) {
        uint8_t x = (i + frame) % 20;
        set_bkg_tiles(x, i, 1, 1, &tile_num);
    }
    
    frame++;
}

void main(void) {
    init_graphics();
    
    uint8_t counter = 0;
    
    while(1) {
        vsync();  // Wait for vertical blank
        
        // Update every 2 frames
        if(counter++ & 1) {
            update_ball();
            draw_pattern();
        }
        
        // Scroll the background slightly for visual interest
        SCX_REG = counter >> 2;
        SCY_REG = counter >> 3;
    }
}
