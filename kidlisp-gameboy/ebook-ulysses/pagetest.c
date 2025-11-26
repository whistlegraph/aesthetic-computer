// Simple page test for Game Boy
#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

const char* test_pages[] = {
    "Page 1\nThis is the\nfirst page of\nour test book.",
    "Page 2\nThis is page\ntwo of the\ntest.",
    "Page 3\nThe final\ntest page."
};

#define TOTAL_PAGES 3

uint8_t current_page = 0;

void draw_page(void) {
    const char* text = test_pages[current_page];
    uint8_t y;
    
    // Clear
    for (y = 0; y < 18; y++) {
        gotoxy(0, y);
        printf("                    ");
    }
    
    gotoxy(0, 0);
    printf("%s", text);
    
    gotoxy(0, 17);
    printf("Page %u/%u", (uint8_t)(current_page + 1), (uint8_t)TOTAL_PAGES);
}

void main(void) {
    printf("ULYSSES TEST\n\nPress START");
    
    while (!(joypad() & J_START)) {
        wait_vbl_done();
    }
    delay(100);
    
    draw_page();
    
    while (1) {
        uint8_t keys = joypad();
        static uint8_t prev = 0;
        uint8_t pressed = keys & ~prev;
        
        if (pressed & J_A) {
            if (current_page < TOTAL_PAGES - 1) {
                current_page++;
                draw_page();
            }
        }
        if (pressed & J_B) {
            if (current_page > 0) {
                current_page--;
                draw_page();
            }
        }
        
        prev = keys;
        wait_vbl_done();
    }
}
