// Ulysses Auto-Reader for Game Boy
// Based on working pagetest.c

#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

#include "book_data_small.h"

uint8_t current_page = 0;
uint16_t timer = 0;
uint8_t auto_mode = 1;

void draw_page(void) {
    const char* text = book_pages[current_page];
    uint8_t y;
    
    // Clear
    for (y = 0; y < 18; y++) {
        gotoxy(0, y);
        printf("                    ");
    }
    
    gotoxy(0, 0);
    printf("%s", text);
    
    gotoxy(0, 17);
    printf("%u/%u %s", (uint8_t)(current_page + 1), (uint8_t)TOTAL_PAGES,
           auto_mode ? "AUTO" : "");
}

void main(void) {
    draw_page();
    
    while (1) {
        uint8_t keys = joypad();
        static uint8_t prev = 0;
        uint8_t pressed = keys & ~prev;
        
        if (pressed & J_A) {
            if (current_page < TOTAL_PAGES - 1) {
                current_page++;
                draw_page();
                timer = 0;
            }
        }
        if (pressed & J_B) {
            if (current_page > 0) {
                current_page--;
                draw_page();
                timer = 0;
            }
        }
        if (pressed & J_START) {
            auto_mode = !auto_mode;
            draw_page();
            timer = 0;
        }
        if (pressed & J_SELECT) {
            current_page = 0;
            draw_page();
            timer = 0;
        }
        
        // Auto advance
        if (auto_mode) {
            timer++;
            if (timer >= 180) {
                timer = 0;
                if (current_page < TOTAL_PAGES - 1) {
                    current_page++;
                } else {
                    current_page = 0;
                }
                draw_page();
            }
        }
        
        prev = keys;
        wait_vbl_done();
    }
}
