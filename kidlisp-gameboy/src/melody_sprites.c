// BUILD_TARGET: CGB
// Music box using sprites for colored bars
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

// Sprite tile: 8x8 filled square
const uint8_t sprite_tile[] = {
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
  0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
};

// OBJ palettes (7 colors + gray)
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

#define FRAMES_PER_NOTE 24
#define SPRITE_BASE_Y 144  // Bottom of screen

uint8_t current_note_index = 0;
uint8_t current_note = REST;
uint8_t frame_counter = 0;
uint8_t auto_play = 1;
uint8_t sprite_count = 0;

void play_note(uint8_t note) {
  if(note == REST || note == 0xFF) {
    NR12_REG = 0x00;
    NR14_REG = 0x80;
    return;
  }
  uint16_t freq = frequencies[note - C3];
  NR10_REG = 0x00;
  NR11_REG = 0x81;
  NR12_REG = 0xF3;
  NR13_REG = (uint8_t)(freq & 0xFF);
  NR14_REG = 0x80 | (uint8_t)((freq >> 8) & 0x07);
}

uint8_t get_palette_for_note(uint8_t note) {
  if(note == REST || note == 0xFF) return 7;
  uint8_t palette = ((note - C3) % 12);
  if(palette > 6) palette = palette - 5;
  return palette;
}

void draw_note_sprites() {
  sprite_count = 0;
  
  for(uint8_t i = 0; melody[i] != 0xFF && sprite_count < 40; i++) {
    uint8_t note = melody[i];
    if(note == REST) continue;
    
    // Calculate bar height (0-17 tiles = 0-136 pixels)
    uint8_t height_tiles = ((note - C3) * 17) / 36;
    if(height_tiles > 17) height_tiles = 17;
    uint8_t height_px = height_tiles * 8;
    
    // X position: each note gets 8 pixels wide
    uint8_t x = 8 + (i * 8);
    
    // Palette: color or gray
    uint8_t palette = (i == current_note_index) ? get_palette_for_note(note) : 7;
    
    // Draw sprites from bottom up
    uint8_t y = SPRITE_BASE_Y - height_px;
    for(uint8_t py = 0; py < height_px && sprite_count < 40; py += 8) {
      set_sprite_tile(sprite_count, 0);
      set_sprite_prop(sprite_count, palette);
      move_sprite(sprite_count, x, y + py);
      sprite_count++;
    }
  }
  
  // Hide unused sprites
  for(uint8_t i = sprite_count; i < 40; i++) {
    move_sprite(i, 0, 0);
  }
}

void init_graphics() {
  DISPLAY_OFF;
  
  // Load sprite tile
  set_sprite_data(0, 1, sprite_tile);
  
  // Set OBJ palettes
  set_sprite_palette(0, 8, obj_palettes);
  
  // Clear background to black
  BGP_REG = 0xE4;
  
  // Draw initial sprites
  draw_note_sprites();
  
  SHOW_SPRITES;
  DISPLAY_ON;
}

void main() {
  show_aesthetic_boot(0);
  
  // Sound setup
  NR52_REG = 0x80;
  NR51_REG = 0x11;
  NR50_REG = 0x77;
  
  init_graphics();
  
  // Start playing
  current_note = melody[0];
  play_note(current_note);
  
  while(1) {
    vsync();
    frame_counter++;
    
    uint8_t joy = joypad();
    
    // Manual control
    if(joy & J_RIGHT) {
      if(melody[current_note_index + 1] != 0xFF) {
        current_note_index++;
        current_note = melody[current_note_index];
        play_note(current_note);
        draw_note_sprites();
        frame_counter = 0;
      }
      auto_play = 0;
    }
    
    if(joy & J_LEFT) {
      if(current_note_index > 0) {
        current_note_index--;
        current_note = melody[current_note_index];
        play_note(current_note);
        draw_note_sprites();
        frame_counter = 0;
      }
      auto_play = 0;
    }
    
    if(joy & J_A) {
      auto_play = 1;
      frame_counter = 0;
    }
    
    // Auto-play
    if(auto_play && frame_counter >= FRAMES_PER_NOTE) {
      if(melody[current_note_index + 1] != 0xFF) {
        current_note_index++;
      } else {
        current_note_index = 0;
      }
      
      current_note = melody[current_note_index];
      play_note(current_note);
      draw_note_sprites();
      frame_counter = 0;
    }
  }
}
