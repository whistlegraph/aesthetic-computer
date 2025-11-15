#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

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
  uint16_t angle = 64 << 8;  // 90 degrees in 8.8 fixed point
  uint8_t step;
  uint8_t angle_byte;
  int8_t s, c;
  
  DISPLAY_ON;
  
  printf("ANGLE TEST\n");
  printf("Start: %d\n", (uint8_t)(angle >> 8));
  
  // Test key angles
  for(step = 0; step < 16; step++) {
    angle_byte = (uint8_t)(angle >> 8);
    s = sin_lookup(angle_byte);
    c = cos_lookup(angle_byte);
    
    gotoxy(0, 3 + step);
    printf("%d:A%d S%d C%d", step * 90, angle_byte, s, c);
    
    angle += 182 * 90;  // Jump by 90 steps worth
  }
  
  // Show what happens at each step
  angle = 64 << 8;
  printf("\n\nPER-STEP:\n");
  for(step = 0; step < 8; step++) {
    angle_byte = (uint8_t)(angle >> 8);
    printf("S%d:A%d\n", step, angle_byte);
    angle += 182;
  }
  
  while(1) {
    wait_vbl_done();
  }
}
