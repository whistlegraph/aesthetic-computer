#include <gb/gb.h>
#include <stdint.h>
#include <gb/drawing.h>
#include <stdio.h>

// Original KidLisp source code
const char* source_lines[] = {
    "wipe black",
    "ink white",
    "line 10 0 10 144",
    "line 20 0 20 144",
    "line 30 0 30 144",
    "line 40 0 40 144",
    "line 50 0 50 144",
    "line 60 0 60 144",
    "line 70 0 70 144",
    "line 80 0 80 144",
    "line 90 0 90 144",
    "line 100 0 100 144",
    "line 110 0 110 144",
    "line 120 0 120 144",
    "line 130 0 130 144",
    "line 140 0 140 144",
    "line 150 0 150 144",
    NULL
};

const char* rom_name = "lines";

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
        // Draw 1px progress using plot_point to avoid line() conflicts
        if (progress > 0) {
            plot_point(progress - 1, 0);
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
    line(10, 0, 10, 144);
    line(20, 0, 20, 144);
    line(30, 0, 30, 144);
    line(40, 0, 40, 144);
    line(50, 0, 50, 144);
    line(60, 0, 60, 144);
    line(70, 0, 70, 144);
    line(80, 0, 80, 144);
    line(90, 0, 90, 144);
    line(100, 0, 100, 144);
    line(110, 0, 110, 144);
    line(120, 0, 120, 144);
    line(130, 0, 130, 144);
    line(140, 0, 140, 144);
    line(150, 0, 150, 144);

    // Main loop
    while(1) {
        vsync();
    }
}
