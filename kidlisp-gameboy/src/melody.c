// BUILD_TARGET: CGB
// Hybrid: Background tiles for bar shapes, sprites for color overlays
#include <gb/gb.h>
#include <gb/cgb.h>
#include <stdint.h>

#include "aesthetic_boot.h"

// Note definitions
enum notes {
  C3 = 36, Cd3, D3, Dd3, E3, F3, Fd3, G3, Gd3, A3, Ad3, B3,
  C4, Cd4, D4, Dd4, E4, F4, Fd4, G4, Gd4, A4, Ad4, B4,
  C5, Cd5, D5, Dd5, E5, F5, Fd5, G5, Gd5, A5, Ad5, B5,
  REST
};

const uint16_t frequencies[] = {
  1798, 1812, 1825, 1837, 1849, 1860, 1871, 1881, 1890, 1899, 1907, 1915,
  1923, 1930, 1936, 1943, 1949, 1954, 1959, 1964, 1969, 1974, 1978, 1982,
  1985, 1988, 1992, 1995, 1998, 2001, 2004, 2006, 2009, 2011, 2013, 2015,
  0
};

// Twinkle Twinkle melody
const uint8_t melody[] = {
  C4, C4, G4, G4, A4, A4, G4, REST,
  F4, F4, E4, E4, D4, D4, C4, REST,
  G4, G4, F4, F4, E4, E4, D4, REST,
  G4, G4, F4, F4, E4, E4, D4, REST,
  C4, C4, G4, G4, A4, A4, G4, REST,
  F4, F4, E4, E4, D4, D4, C4, REST,
  0xFF
};

// Tiles: 0=blank, 1=white bar, 2-8=letters C D E F G A B
const uint8_t tiles[] = {
  // Tile 0: blank
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  // Tile 1: white filled square
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
  // Tile 2: 'C'
  0x3C, 0x3C, 0x66, 0x66, 0x60, 0x60, 0x60, 0x60,
  0x60, 0x60, 0x66, 0x66, 0x3C, 0x3C, 0x00, 0x00,
  // Tile 3: 'D'
  0x78, 0x78, 0x6C, 0x6C, 0x66, 0x66, 0x66, 0x66,
  0x66, 0x66, 0x6C, 0x6C, 0x78, 0x78, 0x00, 0x00,
  // Tile 4: 'E'
  0x7E, 0x7E, 0x60, 0x60, 0x60, 0x60, 0x7C, 0x7C,
  0x60, 0x60, 0x60, 0x60, 0x7E, 0x7E, 0x00, 0x00,
  // Tile 5: 'F'
  0x7E, 0x7E, 0x60, 0x60, 0x60, 0x60, 0x7C, 0x7C,
  0x60, 0x60, 0x60, 0x60, 0x60, 0x60, 0x00, 0x00,
  // Tile 6: 'G'
  0x3C, 0x3C, 0x66, 0x66, 0x60, 0x60, 0x6E, 0x6E,
  0x66, 0x66, 0x66, 0x66, 0x3C, 0x3C, 0x00, 0x00,
  // Tile 7: 'A'
  0x3C, 0x3C, 0x66, 0x66, 0x66, 0x66, 0x7E, 0x7E,
  0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x00, 0x00,
  // Tile 8: 'B'
  0x7C, 0x7C, 0x66, 0x66, 0x66, 0x66, 0x7C, 0x7C,
  0x66, 0x66, 0x66, 0x66, 0x7C, 0x7C, 0x00, 0x00,
};

// Background palette: white bars on black
const uint16_t bg_palettes[] = {
  RGB_BLACK, RGB(31, 31, 31), RGB_WHITE, RGB_WHITE,
};

// OBJ palettes (7 colors for coloring the white bars)
const uint16_t obj_palettes[] = {
  // Palette 0: Red (C)
  RGB_BLACK, RGB(31, 0, 0),   RGB(31, 0, 0),   RGB(31, 0, 0),
  // Palette 1: Orange (D)
  RGB_BLACK, RGB(31, 15, 0),  RGB(31, 15, 0),  RGB(31, 15, 0),
  // Palette 2: Yellow (E)
  RGB_BLACK, RGB(31, 31, 0),  RGB(31, 31, 0),  RGB(31, 31, 0),
  // Palette 3: Green (F)
  RGB_BLACK, RGB(0, 31, 0),   RGB(0, 31, 0),   RGB(0, 31, 0),
  // Palette 4: Cyan (G)
  RGB_BLACK, RGB(0, 31, 31),  RGB(0, 31, 31),  RGB(0, 31, 31),
  // Palette 5: Blue (A)
  RGB_BLACK, RGB(0, 0, 31),   RGB(0, 0, 31),   RGB(0, 0, 31),
  // Palette 6: Purple (B)
  RGB_BLACK, RGB(31, 0, 31),  RGB(31, 0, 31),  RGB(31, 0, 31),
  // Palette 7: Gray (played notes)
  RGB_BLACK, RGB(10, 10, 10), RGB(10, 10, 10), RGB(10, 10, 10),
};

#define SCREEN_WIDTH 160
#define MAX_VISIBLE_BARS 20  // Can show about 20 bars on screen

// DIV-APU timing (512 Hz master clock for perfect audio sync)
// ~15 frames = 0.25 seconds per note at 60 FPS
#define FRAMES_PER_NOTE 15
#define MAX_NOTES 48

uint8_t current_note_index = 0;
uint8_t current_note = REST;
uint16_t frame_counter = 0;
uint8_t auto_play = 1;
int16_t scroll_x = 0;  // Horizontal scroll position

// Animation state - blocks fall when hit
uint8_t anim_frame = 0;         // Global animation counter
int16_t fall_offset[MAX_NOTES]; // How far each block has fallen (negative = falling)
uint8_t hit_frame[MAX_NOTES];   // Frame when block was hit (for flash effect)

void play_note(uint8_t note) {
  if(note == REST || note == 0xFF) {
    // Silence: volume 0, envelope decrease
    NR12_REG = 0x00;
    NR14_REG = 0x80;
    return;
  }
  
  uint16_t freq = frequencies[note - C3];
  
  // Channel 1 configuration (learned from GBDK sound.c example)
  NR10_REG = 0x00;              // No frequency sweep
  NR11_REG = 0x81;              // 50% duty cycle, length = 1
  NR12_REG = 0xF2;              // Volume 15, envelope decrease, pace 2
                                // This gives notes a slight fade for better articulation
  NR13_REG = (uint8_t)(freq & 0xFF);              // Frequency low byte
  NR14_REG = 0x80 | (uint8_t)((freq >> 8) & 0x07); // Trigger + frequency high bits
}

uint8_t get_palette_for_note(uint8_t note) {
  if(note == REST || note == 0xFF) return 7;
  uint8_t palette = ((note - C3) % 12);
  if(palette > 6) palette = palette - 5;
  return palette;
}

// Map note to letter tile (C=2, D=3, E=4, F=5, G=6, A=7, B=8)
uint8_t get_letter_tile(uint8_t note) {
  switch(note) {
    case C3: case C4: case C5: return 2; // 'C'
    case D3: case D4: case D5: return 3; // 'D'
    case E3: case E4: case E5: return 4; // 'E'
    case F3: case F4: case F5: return 5; // 'F'
    case G3: case G4: case G5: return 6; // 'G'
    case A3: case A4: case A5: return 7; // 'A'
    case B3: case B4: case B5: return 8; // 'B'
    default: return 0;
  }
}

// Draw background (just clear - no white bars)
void draw_background_bars() {
  // Clear screen to black
  for(uint8_t y = 0; y < 18; y++) {
    for(uint8_t x = 0; x < 20; x++) {
      set_bkg_tile_xy(x, y, 0);
    }
  }
}

// Update sprite overlays - blocks in a row that fall when hit
void update_sprite_overlays() {
  uint8_t sprite_idx = 0;
  
  // Calculate which notes are visible based on scroll
  int16_t first_visible = scroll_x / 8;
  int16_t last_visible = first_visible + 20;
  
  for(int16_t i = first_visible; i <= last_visible && i < 48 && melody[i] != 0xFF; i++) {
    if(i < 0) continue;
    
    uint8_t note = melody[i];
    // REST notes take time but aren't drawn
    if(note == REST) continue;
    
    // Base position: horizontal row at y=72 (center), accounting for scroll
    int16_t screen_x = 8 + (i * 8) - scroll_x;
    int16_t screen_y = 72;
    
    // Apply fall animation if block has been hit
    if(i < current_note_index) {
      // Block falls down over time
      screen_y += fall_offset[i];
      
      // Continue falling
      if(fall_offset[i] < 100) {
        fall_offset[i] += 3;  // Fall speed
      }
    }
    
    // Determine palette and scale effect
    uint8_t palette;
    
    if(i == current_note_index) {
      // Current note: colored and flashing
      palette = get_palette_for_note(note);
      
      // Flash effect on hit (first few frames)
      uint8_t frames_since_hit = anim_frame - hit_frame[i];
      if(frames_since_hit < 15) {  // Match FRAMES_PER_NOTE
        // Bounce up and down over 15 frames
        int8_t bounce_curve[] = {0, -1, -2, -3, -4, -4, -5, -5, -5, -4, -4, -3, -2, -1, 0};
        screen_y += bounce_curve[frames_since_hit];
      }
    } else if(i < current_note_index) {
      // Already played: gray and falling
      palette = 7;
    } else {
      // Future notes: colored, waiting
      palette = get_palette_for_note(note);
    }
    
    // Don't draw if fallen off screen
    if(screen_y > 160) continue;
    
    // Draw color sprite
    set_sprite_tile(sprite_idx, 1);
    set_sprite_prop(sprite_idx, palette);
    move_sprite(sprite_idx, screen_x, screen_y);
    sprite_idx++;
    
    // Draw letter sprite above
    uint8_t letter_tile = get_letter_tile(note);
    if(letter_tile > 0) {
      set_sprite_tile(sprite_idx, letter_tile);
      set_sprite_prop(sprite_idx, palette);
      move_sprite(sprite_idx, screen_x, screen_y - 8);
      sprite_idx++;
    }
  }
  
  // Hide unused sprites
  for(uint8_t i = sprite_idx; i < 40; i++) {
    move_sprite(i, 0, 0);
  }
}

void init_graphics() {
  DISPLAY_OFF;
  
  // Load tiles (0=blank, 1=white bar, 2-8=letters)
  set_bkg_data(0, 9, tiles);
  set_sprite_data(0, 9, tiles);
  
  // Set palettes
  set_bkg_palette(0, 1, bg_palettes);
  set_sprite_palette(0, 8, obj_palettes);
  
  // Draw background bars
  draw_background_bars();
  
  SHOW_BKG;
  SHOW_SPRITES;
  DISPLAY_ON;
}

void main() {
  // Skip boot screen to avoid timing issues
  // show_aesthetic_boot(0);
  
  // Sound setup (following GBDK sound.c best practices)
  // Must turn on sound before modifying any sound registers!
  NR52_REG = 0x80;  // Sound on
  NR51_REG = 0x11;  // CH1 to both left and right speakers
  NR50_REG = 0x77;  // Max volume on both channels
  
  init_graphics();
  
  // Initialize fall offsets to 0 (no falling yet)
  for(uint8_t i = 0; i < MAX_NOTES; i++) {
    fall_offset[i] = 0;
    hit_frame[i] = 0;
  }
  
  // Initialize to first note and play it immediately
  current_note = melody[0];
  play_note(current_note);
  hit_frame[0] = 0;
  
  while(1) {
    vsync();
    anim_frame++;
    frame_counter++;
    
    // Update sprite animations every frame for smooth motion
    update_sprite_overlays();
    
    uint8_t joy = joypad();
    
    // Manual control
    if(joy & J_RIGHT) {
      if(melody[current_note_index + 1] != 0xFF) {
        current_note_index++;
        current_note = melody[current_note_index];
        play_note(current_note);
        hit_frame[current_note_index] = anim_frame;  // Record when hit
        
        // Scroll to keep current note centered
        scroll_x = (current_note_index * 8) - 72;
        if(scroll_x < 0) scroll_x = 0;
        SCX_REG = scroll_x;
        
        frame_counter = 0;
      }
      auto_play = 0;
    }
    
    if(joy & J_LEFT) {
      if(current_note_index > 0) {
        current_note_index--;
        current_note = melody[current_note_index];
        play_note(current_note);
        hit_frame[current_note_index] = anim_frame;  // Record when hit
        
        // Scroll to keep current note centered
        scroll_x = (current_note_index * 8) - 72;
        if(scroll_x < 0) scroll_x = 0;
        SCX_REG = scroll_x;
        
        frame_counter = 0;
      }
      auto_play = 0;
    }
    
    if(joy & J_A) {
      auto_play = 1;
      frame_counter = 0;
    }
    
    // Auto-play using frame counter (60 FPS timing)
    if(auto_play && frame_counter >= FRAMES_PER_NOTE) {
      if(melody[current_note_index + 1] != 0xFF) {
        current_note_index++;
      } else {
        // Loop: reset everything
        current_note_index = 0;
        scroll_x = 0;
        for(uint8_t i = 0; i < MAX_NOTES; i++) {
          fall_offset[i] = 0;
          hit_frame[i] = 0;
        }
      }
      
      current_note = melody[current_note_index];
      play_note(current_note);
      hit_frame[current_note_index] = anim_frame;  // Record when hit
      
      // Scroll to keep current note centered
      scroll_x = (current_note_index * 8) - 72;
      if(scroll_x < 0) scroll_x = 0;
      SCX_REG = scroll_x;
      
      frame_counter = 0;
    }
  }
}
