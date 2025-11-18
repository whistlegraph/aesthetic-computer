// Aesthetic Computer Boot Logo
// Header file for custom boot screen to replace Nintendo logo
// Include this in your ROM and call show_aesthetic_boot() at startup

#ifndef AESTHETIC_BOOT_H
#define AESTHETIC_BOOT_H

#include <gb/gb.h>
#include <stdint.h>

#ifdef CGB
#include <gb/cgb.h>
#endif

// Logo tiles - "aesthetic.computer" text in a simple font
const uint8_t aesthetic_logo_tiles[] = {
  // Tile 0: Blank
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  
  // Tile 1: 'A'
  0x18, 0x18, 0x24, 0x24, 0x42, 0x42, 0x7E, 0x7E,
  0x42, 0x42, 0x42, 0x42, 0x00, 0x00, 0x00, 0x00,
  
  // Tile 2: 'E'
  0x7E, 0x7E, 0x40, 0x40, 0x40, 0x40, 0x7C, 0x7C,
  0x40, 0x40, 0x40, 0x40, 0x7E, 0x7E, 0x00, 0x00,
  
  // Tile 3: 'S'
  0x3C, 0x3C, 0x42, 0x42, 0x40, 0x40, 0x3C, 0x3C,
  0x02, 0x02, 0x42, 0x42, 0x3C, 0x3C, 0x00, 0x00,
  
  // Tile 4: 'T'
  0x7E, 0x7E, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
  0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 0x00,
  
  // Tile 5: 'H'
  0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x7E, 0x7E,
  0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00,
  
  // Tile 6: 'I'
  0x3C, 0x3C, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
  0x18, 0x18, 0x18, 0x18, 0x3C, 0x3C, 0x00, 0x00,
  
  // Tile 7: 'C'
  0x3C, 0x3C, 0x42, 0x42, 0x40, 0x40, 0x40, 0x40,
  0x40, 0x40, 0x42, 0x42, 0x3C, 0x3C, 0x00, 0x00,
  
  // Tile 8: '.'
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x00,
  
  // Tile 9: 'O'
  0x3C, 0x3C, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,
  0x42, 0x42, 0x42, 0x42, 0x3C, 0x3C, 0x00, 0x00,
  
  // Tile 10: 'M'
  0x42, 0x42, 0x66, 0x66, 0x5A, 0x5A, 0x42, 0x42,
  0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00,
  
  // Tile 11: 'P'
  0x7C, 0x7C, 0x42, 0x42, 0x42, 0x42, 0x7C, 0x7C,
  0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00,
  
  // Tile 12: 'U'
  0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42,
  0x42, 0x42, 0x42, 0x42, 0x3C, 0x3C, 0x00, 0x00,
  
  // Tile 13: 'R'
  0x7C, 0x7C, 0x42, 0x42, 0x42, 0x42, 0x7C, 0x7C,
  0x44, 0x44, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00,
  
  // Tile 14: Top border
  0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  
  // Tile 15: Bottom border
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF,
};

// Display "aesthetic.computer" boot screen
// Horizontal scroll animation on a single line
void show_aesthetic_boot(uint8_t duration) {
  DISPLAY_OFF;
  
  // Load logo tiles
  set_bkg_data(0, 16, aesthetic_logo_tiles);
  
  // Clear screen
  uint8_t blank = 0;
  for(uint8_t y = 0; y < 18; y++) {
    for(uint8_t x = 0; x < 32; x++) {
      set_bkg_tiles(x, y, 1, 1, &blank);
    }
  }
  
  // Draw "AESTHETIC.COMPUTER" on a single line (row 8, middle of screen)
  // Start at x=32 (offscreen right) so it scrolls in
  uint8_t row = 8;
  uint8_t col = 32;  // Start offscreen to the right
  
  // AESTHETIC.COMPUTER = 18 characters
  set_bkg_tile_xy(col++, row, 1);   // A
  set_bkg_tile_xy(col++, row, 2);   // E
  set_bkg_tile_xy(col++, row, 3);   // S
  set_bkg_tile_xy(col++, row, 4);   // T
  set_bkg_tile_xy(col++, row, 5);   // H
  set_bkg_tile_xy(col++, row, 2);   // E
  set_bkg_tile_xy(col++, row, 4);   // T
  set_bkg_tile_xy(col++, row, 6);   // I
  set_bkg_tile_xy(col++, row, 7);   // C
  set_bkg_tile_xy(col++, row, 8);   // .
  set_bkg_tile_xy(col++, row, 7);   // C
  set_bkg_tile_xy(col++, row, 9);   // O
  set_bkg_tile_xy(col++, row, 10);  // M
  set_bkg_tile_xy(col++, row, 11);  // P
  set_bkg_tile_xy(col++, row, 12);  // U
  set_bkg_tile_xy(col++, row, 4);   // T
  set_bkg_tile_xy(col++, row, 2);   // E
  set_bkg_tile_xy(col++, row, 13);  // R
  
#ifdef CGB
  // Set up pink color for the dot on Game Boy Color
  if (_cpu == CGB_TYPE) {
    // Palette 0: white letters (default)
    set_bkg_palette(0, 1, (palette_color_t[]) {
      RGB_WHITE, RGB_BLACK, RGB_BLACK, RGB_BLACK
    });
    
    // Palette 1: pink dot
    set_bkg_palette(1, 1, (palette_color_t[]) {
      RGB_WHITE, RGB(31, 12, 20), RGB_BLACK, RGB_BLACK  // Pink color
    });
    
    // Set the dot to use palette 1
    VBK_REG = 1;  // Switch to attribute map
    set_bkg_tile_xy(41, row, 1);  // Dot is at position 32+9
    VBK_REG = 0;  // Switch back to tile map
  }
#endif
  
  SHOW_BKG;
  DISPLAY_ON;
  
  // Play boot sound (simple ding using channel 1)
  NR52_REG = 0x80;  // Enable sound
  NR51_REG = 0x11;  // CH1 to both speakers
  NR50_REG = 0x77;  // Max volume
  
  // CH1: Quick rising tone
  NR10_REG = 0x16;  // Sweep up
  NR11_REG = 0x80;  // 50% duty, no length
  NR12_REG = 0xF3;  // Volume 15, decay
  NR13_REG = 0x00;  // Frequency low
  NR14_REG = 0xC7;  // Trigger, frequency high (E note)
  
  // Horizontal scroll animation - scroll text in from right to left
  // Text is 18 chars wide, starts at x=256 (32 tiles * 8 pixels)
  // Scroll until centered: (160 - 18*8) / 2 = (160 - 144) / 2 = 8 pixels
  // So scroll from 256 to 8 = 248 pixels in ~40 frames
  SCX_REG = 0;  // Start at leftmost position (text is offscreen right at x=256)
  
  for(uint8_t i = 0; i < 40; i++) {
    SCX_REG += 6;  // Scroll 6 pixels per frame = smooth horizontal motion
    vsync();
  }
  
  // Pause briefly while logo is visible
  for(uint8_t i = 0; i < 20; i++) {
    vsync();
  }
  
  // Quick blink effect - single fast blink
  HIDE_BKG;
  for(uint8_t i = 0; i < 4; i++) vsync();  // 4 frames hidden
  SHOW_BKG;
  for(uint8_t i = 0; i < 4; i++) vsync();  // 4 frames shown
  HIDE_BKG;
  for(uint8_t i = 0; i < 4; i++) vsync();  // 4 frames hidden
  SHOW_BKG;
  
  // Brief pause before ROM starts
  for(uint8_t i = 0; i < 10; i++) {
    vsync();
  }
  
  // Turn off sound
  NR12_REG = 0x00;  // Mute channel 1
  NR14_REG = 0x80;  // Retrigger to apply
  
  // Reset scroll positions
  SCY_REG = 0;
  SCX_REG = 0;
  
  // Clear the entire screen completely before exiting
  DISPLAY_OFF;
  for(uint8_t y = 0; y < 32; y++) {  // Clear all rows including wraparound
    for(uint8_t x = 0; x < 32; x++) {
      set_bkg_tile_xy(x, y, 0);
    }
  }
  // Also clear attributes
  VBK_REG = 1;
  for(uint8_t y = 0; y < 32; y++) {
    for(uint8_t x = 0; x < 32; x++) {
      set_bkg_tile_xy(x, y, 0);
    }
  }
  VBK_REG = 0;
  DISPLAY_ON;
}

#endif // AESTHETIC_BOOT_H
