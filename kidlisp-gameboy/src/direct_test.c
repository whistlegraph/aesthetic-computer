#include <gb/gb.h>
#include <stdint.h>
#include <gb/drawing.h>

void main(void) {
    DISPLAY_ON;
    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);
    
    // Fill screen with BLACK (should be dark)
    fill_rect(0, 0, 160, 144, BLACK);
    
    // Draw white line
    color(WHITE, BLACK, SOLID);
    line(50, 50, 100, 100);
    
    while(1) {
        vsync();
    }
}
