#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>
#include <string.h>

// Sine table: 256 entries, values from -64 to +64
const int8_t sine_table[256] = {
  0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45,
  48, 51, 54, 57, 59, 62, 65, 67, 70, 73, 75, 78, 80, 82, 85, 87,
  89, 91, 94, 96, 98, 100, 102, 103, 105, 107, 108, 110, 112, 113, 114, 116,
  117, 118, 119, 120, 121, 122, 123, 124, 124, 125, 126, 126, 127, 127, 127, 127,
  127, 127, 127, 127, 127, 126, 126, 125, 124, 124, 123, 122, 121, 120, 119, 118,
  117, 116, 114, 113, 112, 110, 108, 107, 105, 103, 102, 100, 98, 96, 94, 91,
  89, 87, 85, 82, 80, 78, 75, 73, 70, 67, 65, 62, 59, 57, 54, 51,
  48, 45, 42, 39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9, 6, 3,
  0, -3, -6, -9, -12, -15, -18, -21, -24, -27, -30, -33, -36, -39, -42, -45,
  -48, -51, -54, -57, -59, -62, -65, -67, -70, -73, -75, -78, -80, -82, -85, -87,
  -89, -91, -94, -96, -98, -100, -102, -103, -105, -107, -108, -110, -112, -113, -114, -116,
  -117, -118, -119, -120, -121, -122, -123, -124, -124, -125, -126, -126, -127, -127, -127, -127,
  -127, -127, -127, -127, -127, -126, -126, -125, -124, -124, -123, -122, -121, -120, -119, -118,
  -117, -116, -114, -113, -112, -110, -108, -107, -105, -103, -102, -100, -98, -96, -94, -91,
  -89, -87, -85, -82, -80, -78, -75, -73, -70, -67, -65, -62, -59, -57, -54, -51,
  -48, -45, -42, -39, -36, -33, -30, -27, -24, -21, -18, -15, -12, -9, -6, -3
};

int8_t sin_lookup(uint8_t angle) {
  return sine_table[angle];
}

int8_t cos_lookup(uint8_t angle) {
  return sine_table[(angle + 64) & 0xFF];
}

// Buffer for tile data manipulation
uint8_t tile_buffer[16];
uint8_t next_tile_id = 128; // Start at 128 to avoid printf's font tiles (0-127)

// Plot a pixel in the background by modifying tile data
void plot_pixel(uint8_t x, uint8_t y) {
  uint8_t tile_x, tile_y;
  uint8_t pixel_x, pixel_y;
  uint8_t tile_num;
  uint8_t row_offset;
  uint8_t bit_mask;
  
  // Convert screen coordinates to tile coordinates
  tile_x = x / 8;
  tile_y = y / 8;
  
  // Get pixel position within the tile (0-7)
  pixel_x = x % 8;
  pixel_y = y % 8;
  
  // Get the tile number from the background map
  get_bkg_tiles(tile_x, tile_y, 1, 1, &tile_num);
  
  // If tile is 0 (empty), allocate a new tile
  if(tile_num == 0) {
    // Make sure we don't run out of tiles (max 256)
    if(next_tile_id >= 255) return;
    
    tile_num = next_tile_id;
    next_tile_id++;
    set_bkg_tiles(tile_x, tile_y, 1, 1, &tile_num);
    
    // Clear the tile buffer
    memset(tile_buffer, 0, 16);
  } else {
    // Read existing tile data
    get_bkg_data(tile_num, 1, tile_buffer);
  }
  
  // Set the pixel in the tile data
  // Each row is 2 bytes (low bit plane, high bit plane)
  row_offset = pixel_y * 2;
  bit_mask = 0x80 >> pixel_x; // Bit 7 is leftmost pixel
  
  // Set both bit planes to make pixel black (color 3)
  tile_buffer[row_offset] |= bit_mask;     // Low bit plane
  tile_buffer[row_offset + 1] |= bit_mask; // High bit plane
  
  // Write the modified tile back to VRAM
  set_bkg_data(tile_num, 1, tile_buffer);
}

void main(void) {
  uint16_t angle = 0;  // Angle in degrees: 0-359
  int16_t turtle_x_fixed = 80 << 8;
  int16_t turtle_y_fixed = 72 << 8;
  int16_t dx, dy;
  uint16_t step = 0;
  uint8_t delay_counter = 0;
  uint8_t pixel_x, pixel_y;
  int16_t radius_fixed = 25 << 8;  // 25 pixel radius

  // Display must be off to initialize
  DISPLAY_OFF;
  
  // Initialize background (all tiles start at 0 = blank)
  uint8_t i, j;
  uint8_t blank = 0;
  for(i = 0; i < 18; i++) {
    for(j = 0; j < 20; j++) {
      set_bkg_tiles(j, i, 1, 1, &blank);
    }
  }

  SHOW_BKG;
  DISPLAY_ON;
  
  // Print status in top-left (uses tiles 0-127 for font)
  gotoxy(0, 0);
  printf("TURTLE CIRCLE");

  // Move turtle to starting position (right side of circle at 0°)
  turtle_x_fixed += radius_fixed;

  while(1) {
    wait_vbl_done();
    
    delay_counter++;
    if(delay_counter >= 2) {
      delay_counter = 0;
      
      if(step < 360) {
        // Use polar coordinates: calculate position directly from angle
        // x = center_x + radius * cos(angle)
        // y = center_y + radius * sin(angle)
        
        // Convert angle (0-36000 hundredths) to lookup table (0-255)
        angle_byte = (uint8_t)((angle_degrees * 256UL) / 36000UL);
        
        // Get cos/sin from lookup table (values are -127 to +127)
        int16_t cos_val = cos_lookup(angle_byte);
        int16_t sin_val = sin_lookup(angle_byte);
        
        // Calculate position: center + (radius * cos/sin) / 127
        // radius_fixed is already in 8.8 format (25 << 8)
        // cos/sin are scaled to 127, so multiply then divide by 127
        int16_t x_offset = (radius_fixed * cos_val) / 127;
        int16_t y_offset = (radius_fixed * sin_val) / 127;
        
        // turtle positions start at center (80, 72) in 8.8 fixed point
        int16_t x_pos = (80 << 8) + x_offset;
        int16_t y_pos = (72 << 8) + y_offset;
        
        // Convert to screen pixels
        int16_t x_int = x_pos >> 8;
        int16_t y_int = y_pos >> 8;
        
        // Only plot if on screen
        if(x_int >= 0 && x_int < 160 && y_int >= 0 && y_int < 144) {
          pixel_x = (uint8_t)x_int;
          pixel_y = (uint8_t)y_int;
          plot_pixel(pixel_x, pixel_y);
        }
        
        // Turn: increment angle by 1 degree
        angle_degrees += 100;
        if(angle_degrees >= 36000) angle_degrees -= 36000;
        
        step++;
        
        // Update step counter and angle debug more frequently
        if(step % 5 == 0) {
          gotoxy(0, 1);
          printf("S:%d A:%d.%02d  ", step, angle_degrees/100, angle_degrees%100);
          gotoxy(0, 2);
          printf("X:%d Y:%d    ", x_int, y_int);
        }
      }
    }
  }
}
