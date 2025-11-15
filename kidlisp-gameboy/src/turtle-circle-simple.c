#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>
#include <string.h>

// Buffer for tile data manipulation
uint8_t tile_buffer[16];
uint8_t next_tile_id = 128; // Start at 128 to avoid font tiles

void plot_pixel(uint8_t x, uint8_t y) {
  uint8_t tile_x = x >> 3;  // Divide by 8
  uint8_t tile_y = y >> 3;
  uint8_t pixel_x = x & 7;  // Modulo 8
  uint8_t pixel_y = y & 7;
  uint8_t tile_num;
  uint8_t row_offset;
  uint8_t bit_mask;
  
  // Get the current tile at this position
  get_bkg_tiles(tile_x, tile_y, 1, 1, &tile_num);
  
  // If it's a blank tile (0), allocate a new tile
  if(tile_num == 0) {
    tile_num = next_tile_id++;
    // Clear the tile buffer
    memset(tile_buffer, 0, 16);
    // Set this tile in the background map
    set_bkg_tiles(tile_x, tile_y, 1, 1, &tile_num);
  } else {
    // Read existing tile data
    get_bkg_data(tile_num, 1, tile_buffer);
  }
  
  // Set the pixel in the tile data
  row_offset = pixel_y * 2;
  bit_mask = 0x80 >> pixel_x;
  
  // Set both bit planes to make pixel black
  tile_buffer[row_offset] |= bit_mask;
  tile_buffer[row_offset + 1] |= bit_mask;
  
  // Write the modified tile back to VRAM
  set_bkg_data(tile_num, 1, tile_buffer);
}

void main(void) {
  uint16_t step = 0;
  uint8_t delay_counter = 0;
  int16_t center_x = 80;
  int16_t center_y = 72;
  int16_t radius = 30;
  
  // Bresenham circle algorithm variables
  int16_t x = 0;
  int16_t y = radius;
  int16_t d = 3 - 2 * radius;
  
  DISPLAY_OFF;
  
  // Initialize background
  uint8_t blank = 0;
  for(uint8_t i = 0; i < 18; i++) {
    for(uint8_t j = 0; j < 20; j++) {
      set_bkg_tiles(j, i, 1, 1, &blank);
    }
  }

  SHOW_BKG;
  DISPLAY_ON;
  
  gotoxy(0, 0);
  printf("CIRCLE");

  // Draw circle using Bresenham algorithm
  while(y >= x) {
    wait_vbl_done();
    
    delay_counter++;
    if(delay_counter >= 1) {
      delay_counter = 0;
      
      // Plot 8 symmetric points
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
      
      // Show progress
      gotoxy(0, 1);
      printf("X:%d Y:%d  ", x, y);
    }
  }
  
  gotoxy(0, 1);
  printf("DONE!      ");

  while(1) {
    wait_vbl_done();
  }
}
