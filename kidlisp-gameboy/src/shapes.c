#include <gb/gb.h>
#include <stdint.h>
#include <gb/drawing.h>

void main(void) {
    // Graphics setup
    DISPLAY_ON;
    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);

    // Execute KidLisp commands
    // Wipe screen to BLACK
    fill_rect(0, 0, 160, 144, 0);
    // Set ink color to WHITE
    color(WHITE, BLACK, SOLID);
    box(10, 10, 50, 50, M_NOFILL);
    box(60, 10, 100, 50, M_NOFILL);
    box(110, 10, 150, 50, M_FILL);
    circle(30, 90, 15, M_NOFILL);
    circle(80, 90, 15, M_NOFILL);
    circle(130, 90, 15, M_NOFILL);
    circle(30, 90, 10, M_NOFILL);
    circle(80, 90, 10, M_NOFILL);
    circle(130, 90, 10, M_NOFILL);

    // Main loop
    while(1) {
        vsync();
    }
}
