#include <gb/gb.h>
#include <stdint.h>
#include <gb/drawing.h>
#include <stdio.h>

// Original KidLisp source code
const char* source_lines[] = {
    "wipe black",
    "ink white",
    "box 20 20 60 60",
    "box 80 20 120 60 fill",
    NULL
};

const char* rom_name = "boxtest";

// Aesthetic Computer splash screen
void show_splash(void) {
    uint8_t i, progress;
    
    // Clear screen
    fill_rect(0, 0, 160, 144, BLACK);
    
    // Title
    color(WHITE, BLACK, SOLID);
    gotogxy(1, 1);
    gprintf("AESTHETIC.COMPUTER");
    gotogxy(2, 2);
    gprintf("KidLisp ROM");
    
    // ROM name
    gotogxy(2, 4);
    gprintf("%s", rom_name);
    
    // Source code (max 8 lines to avoid overflow)
    gotogxy(1, 6);
    gprintf("Source:");
    for (i = 0; i < 8 && source_lines[i] != NULL; i++) {
        gotogxy(1, 7 + i);
        // Truncate lines to 18 chars to prevent bleed
        gprintf("%.18s", source_lines[i]);
    }
    
    // Progress bar animation (3 seconds) - 1px line at top
    for (progress = 0; progress <= 160; progress++) {
        // Draw 1px progress line at top of screen
        if (progress > 0) {
            line(0, 0, progress - 1, 0);
        }
        vsync();
    }
}

void main(void) {
    // Graphics setup
    DISPLAY_ON;
    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);

    // Show splash screen
    show_splash();

    // Reinitialize drawing mode after splash
    mode(get_mode() | M_NO_SCROLL | M_NO_INTERP);

    // Execute KidLisp commands
    // Wipe screen to BLACK
    fill_rect(0, 0, 160, 144, 0);
    // Set ink color to WHITE
    color(WHITE, BLACK, SOLID);
    box(20, 20, 60, 60, M_NOFILL);
    box(80, 20, 120, 60, M_FILL);

    // Main loop
    while(1) {
        vsync();
    }
}
