// BUILD_TARGET: CGB
#include <gb/gb.h>
#include <gb/cgb.h>
#include <stdio.h>
#include <gbdk/console.h>
#include <string.h>
#include <rand.h>

#include "aesthetic_boot.h"

// Canvas settings - tile it 32 tiles wide for smooth 256-pixel wrap
#define CANVAS_TILE_X 0        // Start at left edge
#define CANVAS_TILE_Y 0        // Start at top
#define CANVAS_TILE_WIDTH 32    // 32 tiles wide (256 pixels -> wraps nicely)
#define CANVAS_TILE_COLS 16     // Unique tile columns (16 × 8px = 128px) - divides 32 evenly!
#define CANVAS_TILE_ROWS 16     // Unique tile rows (16 × 8px = 128px) - divides 32 evenly!
#define VIEW_TILE_HEIGHT 18     // Visible tile rows (144px)
#define BG_MAP_TILE_WIDTH 32    // Full background map width (tiles)
#define BG_MAP_TILE_HEIGHT 32   // Full background map height (tiles)
#define CANVAS_UNIQUE_WIDTH (CANVAS_TILE_COLS * 8)    // 128px of unique data
#define CANVAS_UNIQUE_HEIGHT (CANVAS_TILE_ROWS * 8)   // 128px of unique data
#define CANVAS_WORLD_WIDTH (CANVAS_TILE_WIDTH * 8)    // 256 pixels displayed before wrap
#define BG_WORLD_HEIGHT (BG_MAP_TILE_HEIGHT * 8)      // 256px tall background
#define VIEW_HEIGHT (VIEW_TILE_HEIGHT * 8)            // 144px visible window
#define CANVAS_WIDTH CANVAS_WORLD_WIDTH               // For legacy checks
#define CANVAS_HEIGHT BG_WORLD_HEIGHT
#define CANVAS_FIRST_TILE 0    // Use entire tile index range
#define CANVAS_NUM_TILES (CANVAS_TILE_ROWS * CANVAS_TILE_COLS)   // 16 columns × 16 rows = 256 tiles
#define WORLD_SCROLL_MASK 0x0FFF
#define BG_SCROLL_MASK (BG_WORLD_HEIGHT - 1)
#define TILE_BANK_THRESHOLD 256

// Canvas tile data - all tiles stored in one array
uint8_t canvas_tiles[CANVAS_NUM_TILES * 16]; // 256 tiles * 16 bytes each

// Current drawing color (0-3)
uint8_t current_color = 3;
uint8_t last_color = 0;  // Track last color to avoid repeats
uint8_t current_palette = 0;  // Which palette we're currently using

// Rolling wavetable buffer
uint8_t wave_buffer[16];  // 16 bytes = 32 4-bit samples
uint16_t last_scroll_x = 0;  // Track when to shift buffer

// Incremental line drawing state
typedef struct {
  int16_t x, y;      // Current position
  int16_t x1, y1;    // End position
  int16_t dx, dy;
  int16_t sx, sy;
  int16_t err;
  uint8_t active;
  uint16_t pixels_drawn;  // Track how many pixels have been drawn
} line_state_t;

static line_state_t line_state;

// Forward declarations
void plot_pixel(int16_t x, int16_t y);
void upload_tile_to_vram(uint16_t tile_idx);

// Function to sample pixels from visible region and create wavetable
void update_sound_from_pixels(uint16_t scroll_x, uint16_t scroll_y) {
    // STRIP-BASED APPROACH: Divide 144 pixels into 4 horizontal strips
    // Strip 0 (y: 0-35):   CH1 (Pulse with sweep)
    // Strip 1 (y: 36-71):  CH2 (Pulse)
    // Strip 2 (y: 72-107): CH3 (Wavetable)
    // Strip 3 (y: 108-143): CH4 (Noise)
    
    // Sample from center of screen (x = 80, middle of 160 pixel width)
    uint16_t sample_x = (scroll_x + 80) & (CANVAS_WIDTH - 1);
    uint16_t unique_x = sample_x % CANVAS_UNIQUE_WIDTH;
    uint8_t sample_tile_x = unique_x >> 3;
    uint8_t sample_pixel_x = unique_x & 7;
    
    uint8_t ch1_count = 0, ch2_count = 0, ch3_count = 0, ch4_count = 0;
    uint16_t ch1_y_sum = 0, ch2_y_sum = 0, ch3_y_sum = 0, ch4_y_sum = 0;
    
    // Clear wave buffer for CH3
    for(uint8_t i = 0; i < 16; i++) {
        wave_buffer[i] = 0x00;
    }
    
    // Sample every pixel in center column, assign to strips
    for(uint8_t y_offset = 0; y_offset < 144; y_offset++) {
        uint16_t y = (scroll_y + y_offset) & BG_SCROLL_MASK;
        uint16_t unique_y = y % CANVAS_UNIQUE_HEIGHT;
        
        uint8_t canvas_tile_y = unique_y >> 3;
        uint8_t pixel_y = unique_y & 7;
        uint16_t tile_idx = (canvas_tile_y * CANVAS_TILE_COLS) + sample_tile_x;
        
        uint8_t byte0 = canvas_tiles[tile_idx * 16 + (pixel_y * 2)];
        uint8_t byte1 = canvas_tiles[tile_idx * 16 + (pixel_y * 2) + 1];
        uint8_t bit_mask = 0x80 >> sample_pixel_x;
        uint8_t color = ((byte0 & bit_mask) ? 1 : 0) | ((byte1 & bit_mask) ? 2 : 0);
        
        if(color > 0) {
            // Determine which strip (36 pixels each)
            if(y_offset < 36) {
                // Strip 0: CH1
                ch1_count++;
                ch1_y_sum += y_offset;
            }
            else if(y_offset < 72) {
                // Strip 1: CH2
                ch2_count++;
                ch2_y_sum += y_offset;
            }
            else if(y_offset < 108) {
                // Strip 2: CH3 (build wavetable)
                ch3_count++;
                ch3_y_sum += y_offset;
                
                // Build wavetable from pixel patterns (2 grains per pixel)
                uint8_t base_sample = ((y_offset - 72) * 32) / 36;
                for(uint8_t grain = 0; grain < 2; grain++) {
                    uint8_t sample_idx = (base_sample + grain) & 0x1F;
                    uint8_t byte_idx = sample_idx >> 1;
                    uint8_t nibble_idx = sample_idx & 1;
                    uint8_t add_val = 2;
                    
                    if(nibble_idx == 0) {
                        uint8_t current = (wave_buffer[byte_idx] >> 4) & 0x0F;
                        uint8_t new_val = current + add_val;
                        if(new_val > 0xF) new_val = 0xF;
                        wave_buffer[byte_idx] = (new_val << 4) | (wave_buffer[byte_idx] & 0x0F);
                    } else {
                        uint8_t current = wave_buffer[byte_idx] & 0x0F;
                        uint8_t new_val = current + add_val;
                        if(new_val > 0xF) new_val = 0xF;
                        wave_buffer[byte_idx] = (wave_buffer[byte_idx] & 0xF0) | new_val;
                    }
                }
            }
            else {
                // Strip 3: CH4
                ch4_count++;
                ch4_y_sum += y_offset;
            }
        }
    }
    
    // CH1 & CH2: Disabled (wavetable only)
    NR12_REG = 0x00;
    NR22_REG = 0x00;
    
    // CH3 (Strips 0 & 1: wavetable synth)
    uint8_t total_count = ch1_count + ch2_count;
    if(total_count > 0) {
        // Combine pixels from both top strips
        uint16_t combined_y = 0;
        if(ch1_count > 0) combined_y += ch1_y_sum / ch1_count;
        if(ch2_count > 0) combined_y += (ch2_y_sum / ch2_count);
        uint8_t avg_y = combined_y / ((ch1_count > 0 ? 1 : 0) + (ch2_count > 0 ? 1 : 0));
        
        // Update wavetable based on pixel density
        NR30_REG = 0x00;
        for(uint8_t i = 0; i < 16; i++) {
            *(uint8_t*)(0xFF30 + i) = wave_buffer[i];
        }
        NR30_REG = 0x80;
        
        NR32_REG = (total_count > 10) ? 0x40 : 0x60;
        
        // Y position controls pitch (0-71 range)
        uint16_t freq = 0x700 - (avg_y << 3);
        if(freq < 0x400) freq = 0x400;
        if(freq > 0x750) freq = 0x750;
        
        NR33_REG = freq & 0xFF;
        NR34_REG = 0x80 | ((freq >> 8) & 0x07);
    } else {
        NR30_REG = 0x00;
    }
    
    // CH4 (Strips 2 & 3: noise channel for rhythmic texture)
    total_count = ch3_count + ch4_count;
    if(total_count > 0) {
        // Combine pixels from both bottom strips
        uint16_t combined_y = 0;
        if(ch3_count > 0) combined_y += (ch3_y_sum / ch3_count) - 72;
        if(ch4_count > 0) combined_y += (ch4_y_sum / ch4_count) - 72;
        uint8_t avg_y = combined_y / ((ch3_count > 0 ? 1 : 0) + (ch4_count > 0 ? 1 : 0));
        
        uint8_t vol = (total_count > 8) ? 0x0D : 0x08;
        
        // Y position controls noise character (0-71 normalized range)
        uint8_t noise_param = avg_y >> 3;  // 0-8 range
        
        NR41_REG = 0x10;  // Medium length
        NR42_REG = (vol << 4) | 0x05;
        NR43_REG = 0x20 | (noise_param & 0x0F);
        NR44_REG = 0xC0;
    } else {
        NR42_REG = 0x00;
    }
    
    // Stereo panning: assign each strip to different channels
    uint8_t pan = 0x00;
    if(ch1_count > 0) pan |= 0x11;  // CH1 both
    if(ch2_count > 0) pan |= 0x22;  // CH2 both
    if(ch3_count > 0) pan |= 0x44;  // CH3 both
    if(ch4_count > 0) pan |= 0x88;  // CH4 both
    NR51_REG = pan ? pan : 0xFF;
}

void upload_tile_to_vram(uint16_t tile_idx) {
  uint8_t tile_bank = (tile_idx >= TILE_BANK_THRESHOLD) ? 1 : 0;
  uint8_t tile_id = tile_idx & 0xFF;
  VBK_REG = tile_bank;
  set_bkg_data(tile_id, 1, &canvas_tiles[tile_idx * 16]);
  VBK_REG = 0;
}

void build_canvas_background_map(void) {
  uint8_t tile_row[BG_MAP_TILE_WIDTH];
  uint8_t attr_row[BG_MAP_TILE_WIDTH];

  for(uint8_t ty = 0; ty < BG_MAP_TILE_HEIGHT; ty++) {
    for(uint8_t tx = 0; tx < BG_MAP_TILE_WIDTH; tx++) {
      // Calculate which unique canvas tile this screen position maps to
      uint8_t canvas_x = tx % CANVAS_TILE_COLS;  // 0-15
      uint8_t canvas_y = ty % CANVAS_TILE_ROWS;  // 0-15
      uint16_t tile_idx = (canvas_y * CANVAS_TILE_COLS) + canvas_x;
      
      tile_row[tx] = tile_idx;
      attr_row[tx] = current_palette;  // Use current palette for whole canvas
    }

    set_bkg_tiles(0, ty, BG_MAP_TILE_WIDTH, 1, tile_row);

    VBK_REG = 1;
    set_bkg_tiles(0, ty, BG_MAP_TILE_WIDTH, 1, attr_row);
    VBK_REG = 0;
  }
}

void seed_canvas_tiles(void) {
  // Initialize all tiles to black (empty)
  memset(canvas_tiles, 0, sizeof(canvas_tiles));
  
  // Create regular patterns across all 4 strips with spacing
  
  // Strip 0 (y: 0-35) - horizontal lines with gaps
  for(uint16_t x = 0; x < CANVAS_UNIQUE_WIDTH; x += 8) {
    for(uint16_t len = 0; len < 4; len++) {
      plot_pixel(x + len, 10);
      plot_pixel(x + len, 25);
    }
  }
  
  // Strip 1 (y: 36-71) - diagonal lines
  for(uint16_t x = 0; x < CANVAS_UNIQUE_WIDTH; x += 12) {
    for(uint8_t d = 0; d < 6; d++) {
      plot_pixel(x + d, 45 + d);
    }
  }
  
  // Strip 2 (y: 72-107) - vertical dashes
  for(uint16_t x = 5; x < CANVAS_UNIQUE_WIDTH; x += 10) {
    for(uint16_t y = 80; y < 100; y += 4) {
      plot_pixel(x, y);
      plot_pixel(x, y + 1);
    }
  }
  
  // Strip 3 (y: 108-143) - scattered grid
  for(uint16_t x = 0; x < CANVAS_UNIQUE_WIDTH; x += 15) {
    for(uint16_t y = 110; y < 140; y += 10) {
      plot_pixel(x, y);
      plot_pixel(x + 1, y);
      plot_pixel(x, y + 1);
      plot_pixel(x + 1, y + 1);
    }
  }
}

// Plot a pixel in the canvas with current color
void plot_pixel(int16_t x, int16_t y) {
  if(y < 0 || y >= CANVAS_HEIGHT) return;  // Only draw in the visible canvas area
    if(x < 0) return;
    
    // Wrap visible world coordinates into the unique tile data footprint
    uint16_t world_x = (uint16_t)x & (CANVAS_WIDTH - 1);
    uint16_t unique_x = world_x % CANVAS_UNIQUE_WIDTH;
    uint16_t unique_y = ((uint16_t)y) % CANVAS_UNIQUE_HEIGHT;
    
    // Calculate which canvas tile this pixel belongs to
    uint8_t canvas_tile_x = unique_x >> 3;  // Divide by 8 (0-13)
    uint8_t canvas_tile_y = unique_y >> 3;  // Divide by 8 (0-17)
    uint8_t pixel_x = unique_x & 7;         // Modulo 8
  uint8_t pixel_y = unique_y & 7;
    
    // Calculate the tile index in our canvas array (14 tiles wide × 18 tiles high)
    uint16_t tile_idx = (canvas_tile_y * CANVAS_TILE_COLS) + canvas_tile_x;
    
    // Calculate the byte offset in the canvas_tiles array
    uint16_t byte_offset = tile_idx * 16 + (pixel_y * 2);
    
    // Set the pixel color using bit planes
    // Color 0 = 00 (white), 1 = 01 (light), 2 = 10 (dark), 3 = 11 (black)
    uint8_t bit_mask = 0x80 >> pixel_x;
    
    if(current_color & 1) {
        canvas_tiles[byte_offset] |= bit_mask;      // Bit plane 0
    } else {
        canvas_tiles[byte_offset] &= ~bit_mask;
    }
    
  if(current_color & 2) {
    canvas_tiles[byte_offset + 1] |= bit_mask;  // Bit plane 1
  } else {
    canvas_tiles[byte_offset + 1] &= ~bit_mask;
  }
    
  // Write the modified tile back to the correct VRAM bank
  upload_tile_to_vram(tile_idx);
}

// Bresenham line algorithm (now uses canvas coordinates 0-159, 0-119)
void draw_line(int16_t x0, int16_t y0, int16_t x1, int16_t y1) {
  int16_t dx = x1 - x0;
  int16_t dy = y1 - y0;
  int16_t sx = (dx > 0) ? 1 : -1;
  int16_t sy = (dy > 0) ? 1 : -1;
  
  if(dx < 0) dx = -dx;
  if(dy < 0) dy = -dy;
  
  int16_t err = dx - dy;
  int16_t e2;
  
  while(1) {
    plot_pixel(x0, y0);
    
    if(x0 == x1 && y0 == y1) break;
    
    e2 = 2 * err;
    if(e2 > -dy) {
      err -= dy;
      x0 += sx;
    }
    if(e2 < dx) {
      err += dx;
      y0 += sy;
    }
  }
}

// Draw horizontal guide lines to show the 4 voice strips
void draw_strip_guides(void) {
  // Draw thin horizontal lines at y = 36, 72, 108 to divide the 4 strips
  // Each strip is 36 pixels tall (144 / 4)
  
  for(uint16_t x = 0; x < CANVAS_UNIQUE_WIDTH; x++) {
    // Draw at y = 35 (bottom of strip 0)
    plot_pixel(x, 35);
    
    // Draw at y = 71 (bottom of strip 1)  
    plot_pixel(x, 71);
    
    // Draw at y = 107 (bottom of strip 2)
    plot_pixel(x, 107);
  }
}

// Bresenham circle algorithm (now uses canvas coordinates)
void draw_circle(int16_t center_x, int16_t center_y, int16_t radius) {
  int16_t x = 0;
  int16_t y = radius;
  int16_t d = 3 - 2 * radius;
  
  while(y >= x) {
    // Plot 8 symmetric points (bounds checking done in plot_pixel)
  plot_pixel(center_x + x, center_y + y);
  plot_pixel(center_x - x, center_y + y);
  plot_pixel(center_x + x, center_y - y);
  plot_pixel(center_x - x, center_y - y);
  plot_pixel(center_x + y, center_y + x);
  plot_pixel(center_x - y, center_y + x);
  plot_pixel(center_x + y, center_y - x);
  plot_pixel(center_x - y, center_y - x);
    
    if(d <= 0) {
      d = d + 4 * x + 6;
    } else {
      d = d + 4 * (x - y) + 10;
      y--;
    }
    x++;
  }
}

// Start drawing a line (sets up state for incremental drawing)
void start_line(int16_t x0, int16_t y0, int16_t x1, int16_t y1) {
  line_state.x = x0;
  line_state.y = y0;
  line_state.x1 = x1;
  line_state.y1 = y1;
  
  line_state.dx = x1 - x0;
  line_state.dy = y1 - y0;
  line_state.sx = (line_state.dx >= 0) ? 1 : -1;
  line_state.sy = (line_state.dy >= 0) ? 1 : -1;
  
  if(line_state.dx < 0) line_state.dx = -line_state.dx;
  if(line_state.dy < 0) line_state.dy = -line_state.dy;
  
  line_state.err = line_state.dx - line_state.dy;
  line_state.active = 1;
  line_state.pixels_drawn = 0;
}

// Draw one pixel of the current line (call each frame)
// Returns 1 if line is complete, 0 if still drawing
uint8_t step_line(uint8_t being_dragged) {
  if(!line_state.active) return 1;
  
  plot_pixel(line_state.x, line_state.y);
  line_state.pixels_drawn++;
  
  // End line if we've drawn enough pixels (max ~120 pixels)
  // or if we reached the endpoint while not being dragged
  if(line_state.pixels_drawn >= 120 || 
     (!being_dragged && line_state.x == line_state.x1 && line_state.y == line_state.y1)) {
    line_state.active = 0;
    return 1;
  }
  
  int16_t e2 = line_state.err << 1;
  if(e2 > -line_state.dy) {
    line_state.err -= line_state.dy;
    line_state.x += line_state.sx;
  }
  if(e2 < line_state.dx) {
    line_state.err += line_state.dx;
    line_state.y += line_state.sy;
  }
  
  return 0;
}

void main(void) {
    uint8_t delay_counter = 0;
    uint8_t direction_counter = 0;
    uint16_t speed_oscillator = 0;  // For sine-wave-like speed variation
    int16_t scroll_vx = 64;  // Much slower auto-scroll (0.25 in 8.8 fixed point = 0.25 pixel/frame)
    int16_t scroll_vy = 0;  // Velocity Y
    int16_t target_vx = 64;  // Much slower constant horizontal scroll
    int16_t target_vy = 0;  // Target velocity Y
    uint16_t scroll_x = 0;
    uint16_t scroll_y = 0;
    
    // Read uninitialized stack memory for entropy (random on real hardware)
    uint8_t stack_entropy[16];
    uint16_t seed = 0;
    
    // Sample some "random" bytes from uninitialized stack
    for(uint8_t i = 0; i < 16; i++) {
        seed ^= stack_entropy[i];
        seed = (seed << 1) | (seed >> 15); // Rotate
    }
    
    // Mix with DIV_REG
    seed ^= DIV_REG;
    seed ^= (uint16_t)DIV_REG << 8;
    
    // If seed ended up as 0, use a default
    if(seed == 0) seed = 0xABCD;
    
    initrand(seed);
    
    // Advance the RNG by a variable amount based on DIV_REG
    uint16_t advance_count = ((uint16_t)DIV_REG << 8) | DIV_REG;
    for(uint16_t i = 0; i < advance_count; i++) {
        rand();
    }

  if(_cpu != CGB_TYPE) {
    printf("CGB ONLY\nPLEASE USE COLOR MODE\n");
    while(1) {
      wait_vbl_done();
    }
  }
  
  // Show Aesthetic Computer boot logo
  show_aesthetic_boot(120);  // 2 seconds
    
    DISPLAY_OFF;
    
  // Simple black and white palette
  BCPS_REG = 0x80;
  
  // Palette 0: Black background, white foreground
  BCPD_REG = 0x00; BCPD_REG = 0x00;  // Black background
  BCPD_REG = 0xFF; BCPD_REG = 0x7F;  // White
  BCPD_REG = 0xFF; BCPD_REG = 0x7F;  // White
  BCPD_REG = 0xFF; BCPD_REG = 0x7F;  // White
  
  current_palette = 0;  // Only use palette 0
    
  // Initialize canvas tiles for drawing
  seed_canvas_tiles();
  
  // Don't draw strip guides - they cause constant sound
  // draw_strip_guides();
  
  for(uint16_t tile_idx = 0; tile_idx < CANVAS_NUM_TILES; tile_idx++) {
    upload_tile_to_vram(tile_idx);
  }
    
  // Set up the entire 32×32 background map to wrap the canvas pattern
  build_canvas_background_map();
  
  // Initialize sound
  NR52_REG = 0x80;  // Enable sound
  NR51_REG = 0xFF;  // Enable all channels on both speakers
  NR50_REG = 0x77;  // Max volume
  
  // Initialize all channels
  NR10_REG = 0x00;  // CH1: No sweep
  NR11_REG = 0x80;  // CH1: 50% duty
  NR12_REG = 0x00;  // CH1: Start silent
  
  NR21_REG = 0x80;  // CH2: 50% duty
  NR22_REG = 0x00;  // CH2: Start silent
  
  // Channel 3: Initialize with triangle wave
  NR30_REG = 0x00;  // Disable to write wave RAM
  for(uint8_t i = 0; i < 16; i++) {
      uint8_t sample = (i < 8) ? (i * 2) : ((15 - i) * 2);
      wave_buffer[i] = (sample << 4) | sample;
      *(uint8_t*)(0xFF30 + i) = wave_buffer[i];
  }
  NR30_REG = 0x80;  // Enable channel 3 DAC
  NR32_REG = 0x00;  // Start silent
  
  // Set up sprite for center line indicator (1 pixel wide)
  // Create a thin vertical line sprite (1 pixel wide in center of 8x8 tile)
  uint8_t sprite_tile[16];
  for(uint8_t i = 0; i < 16; i++) {
      sprite_tile[i] = 0x10;  // Single pixel in center (bit 4)
  }
  set_sprite_data(0, 1, sprite_tile);
  
  // Set up sprite palette - make it red
  uint16_t red_palette = 0x001F;  // Red color (RGB555: 0,0,31)
  set_sprite_palette(0, 1, &red_palette);
  
  // Place sprites to create a vertical line at center (x=80)
  // Sprites are positioned at x+8, y+16 due to GameBoy screen offset
  for(uint8_t i = 0; i < 18; i++) {  // 18 sprites to cover 144 pixels
      set_sprite_tile(i, 0);  // Use tile 0
      move_sprite(i, 88, 16 + (i * 8));  // x=88 (80+8), y offset by sprite number
      set_sprite_prop(i, 0);  // Use palette 0
  }
    
    SHOW_BKG;
    SHOW_SPRITES;
    DISPLAY_ON;
    
    uint8_t sound_update_counter = 0;
    uint16_t last_pixel_x = 0xFFFF;  // Track last pixel position
    uint16_t last_pixel_y = 0xFFFF;  // to avoid re-processing

    while(1) {
        vsync();  // Use vsync() instead of wait_vbl_done() for better input handling
        
        // Calculate center pixel position
        uint16_t center_x = (scroll_x + 80) & WORLD_SCROLL_MASK;
        uint16_t center_y = (scroll_y + 72) & BG_SCROLL_MASK;
        
        // Only update sound if the pixel position changed
        if(center_x != last_pixel_x || center_y != last_pixel_y) {
            update_sound_from_pixels(scroll_x, scroll_y);
            last_pixel_x = center_x;
            last_pixel_y = center_y;
        }

        // Read joypad input for manual scroll control
        uint8_t joy = joypad();
        
        // Check if any d-pad button is pressed
        uint8_t manual_control = joy & (J_RIGHT | J_LEFT | J_DOWN | J_UP);
        
        // Manual scroll control with D-pad (faster scroll)
        if(manual_control) {
            // Manual control - user is pressing d-pad - scroll faster
            if(joy & J_RIGHT) scroll_x = (scroll_x + 4) & WORLD_SCROLL_MASK;
            if(joy & J_LEFT) scroll_x = (scroll_x - 4) & WORLD_SCROLL_MASK;
            if(joy & J_DOWN) scroll_y = (scroll_y + 4) & BG_SCROLL_MASK;
            if(joy & J_UP) scroll_y = (scroll_y - 4) & BG_SCROLL_MASK;
        }
        
        // Update hardware scroll registers
        SCX_REG = (uint8_t)scroll_x;
        SCY_REG = (uint8_t)scroll_y;
    }
}
