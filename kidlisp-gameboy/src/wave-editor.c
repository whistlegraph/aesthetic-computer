// BUILD_TARGET: CGB
#include <gb/gb.h>
#include <stdint.h>

// Wave editor - visualize and edit Channel 3 waveform using sprites
// 32 samples, 4-bit each (0-15)

uint8_t wave_samples[32];
uint8_t current_sample = 0;
uint8_t last_joy = 0;

// Sprite tile data - just one filled 8x8 tile
const unsigned char sprite_tile[16] = {
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
};

void update_wave_ram(void) {
    NR30_REG = 0x00;
    for(uint8_t i = 0; i < 16; i++) {
        uint8_t byte = (wave_samples[i*2] << 4) | wave_samples[i*2 + 1];
        ((uint8_t*)0xFF30)[i] = byte;
    }
    NR30_REG = 0x80;
}

void init_wave(void) {
    const uint8_t sine_wave[32] = {
        8, 10, 12, 13, 14, 15, 15, 15,
        15, 15, 14, 13, 12, 10, 8, 6,
        4, 2, 1, 0, 0, 0, 0, 0,
        0, 1, 2, 4, 6, 8, 8, 8
    };
    uint8_t i;
    for(i = 0; i < 32; i++) {
        wave_samples[i] = sine_wave[i];
    }
    update_wave_ram();
}

void setup_tiles(void) {
    // Upload sprite tile
    set_sprite_data(0, 1, sprite_tile);
}

void draw_display(void) {
    uint8_t sprite_idx = 0;
    
    // Hide all sprites first
    for(uint8_t i = 0; i < 40; i++) {
        move_sprite(i, 0, 0);
    }
    
    // Draw 32 bars using sprites - show only first 32 for now
    uint8_t base_y = 128;  // Bottom of visible area
    
    for(uint8_t i = 0; i < 32 && sprite_idx < 40; i++) {
        uint8_t height_pixels = wave_samples[i] * 8;  // 0-120 pixels (0-15 * 8)
        uint8_t x = 8 + (i * 4);  // 4 pixels apart, fits 32 bars in 128 pixels
        
        // Determine color based on selection
        uint8_t palette;
        if(i == current_sample) {
            palette = 1;  // Cyan
        } else {
            palette = 0;  // White
        }
        
        // Draw single sprite for each bar at the correct height
        if(height_pixels > 0 && sprite_idx < 40) {
            uint8_t y = base_y - height_pixels;
            set_sprite_tile(sprite_idx, 0);
            move_sprite(sprite_idx, x, y);
            set_sprite_prop(sprite_idx, palette);
            sprite_idx++;
        }
        
        // Draw cursor above selected bar
        if(i == current_sample && sprite_idx < 40) {
            uint8_t cursor_y = base_y - height_pixels - 8;
            set_sprite_tile(sprite_idx, 0);
            move_sprite(sprite_idx, x, cursor_y);
            set_sprite_prop(sprite_idx, 2);  // Yellow palette
            sprite_idx++;
        }
    }
}

void main(void) {
    uint8_t i;
    uint16_t freq = 0x600;
    
    DISPLAY_OFF;
    
    // Set up sprite palettes manually
    // Palette 0: White
    OCPS_REG = 0x80;
    OCPD_REG = 0xFF; OCPD_REG = 0x7F;
    OCPD_REG = 0xFF; OCPD_REG = 0x7F;
    OCPD_REG = 0xFF; OCPD_REG = 0x7F;
    OCPD_REG = 0xFF; OCPD_REG = 0x7F;
    
    // Palette 1: Cyan
    OCPS_REG = 0x88;
    OCPD_REG = 0xE0; OCPD_REG = 0x7F;
    OCPD_REG = 0xE0; OCPD_REG = 0x7F;
    OCPD_REG = 0xE0; OCPD_REG = 0x7F;
    OCPD_REG = 0xE0; OCPD_REG = 0x7F;
    
    // Palette 2: Yellow
    OCPS_REG = 0x90;
    OCPD_REG = 0xFF; OCPD_REG = 0x03;
    OCPD_REG = 0xFF; OCPD_REG = 0x03;
    OCPD_REG = 0xFF; OCPD_REG = 0x03;
    OCPD_REG = 0xFF; OCPD_REG = 0x03;
    
    setup_tiles();
    
    // Initialize sound
    NR52_REG = 0x80;
    NR51_REG = 0x44;
    NR50_REG = 0x77;
    
    init_wave();
    
    NR30_REG = 0x80;
    NR32_REG = 0x20;
    
    NR33_REG = freq & 0xFF;
    NR34_REG = 0x80 | ((freq >> 8) & 0x07);
    
    SHOW_SPRITES;
    DISPLAY_ON;
    
    draw_display();
    
    while(1) {
        vsync();
        
        i = joypad();
        uint8_t needs_redraw = 0;
        
        // Left/Right: Move selection (continuous)
        if(i & J_LEFT) {
            if(current_sample > 0) {
                current_sample--;
                needs_redraw = 1;
            }
        }
        if(i & J_RIGHT) {
            if(current_sample < 31) {
                current_sample++;
                needs_redraw = 1;
            }
        }
        
        // Up/Down: Adjust sample value (continuous)
        if(i & J_UP) {
            if(wave_samples[current_sample] < 15) {
                wave_samples[current_sample]++;
                update_wave_ram();
                NR34_REG = 0x80 | ((freq >> 8) & 0x07);
                needs_redraw = 1;
            }
        }
        if(i & J_DOWN) {
            if(wave_samples[current_sample] > 0) {
                wave_samples[current_sample]--;
                update_wave_ram();
                NR34_REG = 0x80 | ((freq >> 8) & 0x07);
                needs_redraw = 1;
            }
        }
        
        // A/B: Adjust pitch (continuous)
        if(i & J_A) {
            if(freq < 0x700) {
                freq += 0x40;
                NR33_REG = freq & 0xFF;
                NR34_REG = 0x80 | ((freq >> 8) & 0x07);
            }
        }
        
        if(i & J_B) {
            if(freq > 0x100) {
                freq -= 0x40;
                NR33_REG = freq & 0xFF;
                NR34_REG = 0x80 | ((freq >> 8) & 0x07);
            }
        }
        
        // Select/Start: Reset waveforms (edge detection)
        if((i & J_SELECT) && !(last_joy & J_SELECT)) {
            init_wave();
            NR34_REG = 0x80 | ((freq >> 8) & 0x07);
            needs_redraw = 1;
        }
        
        if((i & J_START) && !(last_joy & J_START)) {
            uint8_t j;
            for(j = 0; j < 32; j++) {
                wave_samples[j] = 8;
            }
            update_wave_ram();
            NR34_REG = 0x80 | ((freq >> 8) & 0x07);
            needs_redraw = 1;
        }
        
        last_joy = i;
        
        if(needs_redraw) {
            draw_display();
        }
    }
}
