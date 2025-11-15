#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

// Dot sprite: 8x8 filled circle
const unsigned char dot_tile[] = {
  0x00, 0x00,
  0x3C, 0x3C,
  0x7E, 0x7E,
  0x7E, 0x7E,
  0x7E, 0x7E,
  0x7E, 0x7E,
  0x3C, 0x3C,
  0x00, 0x00
};

// Turtle sprite: 8x8 triangle pointing right
const unsigned char turtle_tile[] = {
  0x00, 0x00,
  0x10, 0x10,
  0x18, 0x18,
  0x1C, 0x1C,
  0x1E, 0x1E,
  0x1C, 0x1C,
  0x18, 0x18,
  0x10, 0x10
};

// Small pixel tile for background (just a 2x2 dot in top-left)
const unsigned char pixel_tile[] = {
  0xC0, 0xC0,
  0xC0, 0xC0,
  0x00, 0x00,
  0x00, 0x00,
  0x00, 0x00,
  0x00, 0x00,
  0x00, 0x00,
  0x00, 0x00
};

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

void main(void) {
  uint16_t angle = 64;  // Start at 90° (pointing right after setup)
  // Fixed point: 8.8 format (high byte = integer, low byte = fraction)
  // Screen center is approximately 80, 72
  int16_t turtle_x_fixed = 80 << 8;  // 80.0 in fixed point
  int16_t turtle_y_fixed = 72 << 8;  // 72.0 in fixed point
  int16_t dx, dy;
  uint16_t step = 0;
  uint8_t delay_counter = 0;
  uint8_t pixel_x, pixel_y;
  uint8_t angle_byte;
  uint8_t sprite_idx = 0;
  int16_t radius_fixed = 30 << 8;  // Radius of 30 pixels

  // Load sprite tiles (all 40 sprites will show dots)
  set_sprite_data(0, 1, dot_tile);

  // Move turtle up by radius
  turtle_y_fixed -= radius_fixed;

  SHOW_SPRITES;
  DISPLAY_ON;

  while(1) {
    wait_vbl_done();
    
    delay_counter++;
    if(delay_counter >= 2) {  // Delay 2 frames for smooth animation
      delay_counter = 0;
      
      if(step < 360) {
        // Convert fixed point to pixels
        pixel_x = (uint8_t)(turtle_x_fixed >> 8);
        pixel_y = (uint8_t)(turtle_y_fixed >> 8);
        
        // Draw dot sprite at current position (all 40 sprites, rolling buffer)
        sprite_idx = step % 40;
        set_sprite_tile(sprite_idx, 0);
        move_sprite(sprite_idx, pixel_x, pixel_y);
        
        // Move turtle forward using fixed-point math
        // Extract the high byte for sine table lookup
        angle_byte = (uint8_t)(angle >> 8);
        
        // dx = cos(angle) * step_size, in 8.8 fixed point
        // Our sine table is -64 to +64, so we need to scale
        // Using step_size = 0.5 = 128 in 8.8 fixed point
        dx = (cos_lookup(angle_byte) * 128) / 64;  // Result in 8.8 fixed
        dy = (sin_lookup(angle_byte) * 102) / 64;  // Y *= 0.8 for aspect (102/128)
        
        turtle_x_fixed += dx;
        turtle_y_fixed += dy;
        
        // Turn right: 360° = 256 in our angle system
        // Each step should turn 256/360 = 0.7111... degrees
        // In 8.8 fixed point: 256 * 256 / 360 = 182.044 ≈ 182
        angle += 182;
        
        step++;
        
        // Update step counter every 10 steps
        if(step % 10 == 0) {
          gotoxy(0, 0);
          printf("STEP:%d/360", step);
        }
      }
    }
  }
}
