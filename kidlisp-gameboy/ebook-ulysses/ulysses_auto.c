// Ulysses Auto-Reader for Game Boy
// James Joyce's Ulysses from Project Gutenberg
// Auto-scrolls through pages, no button press needed to start

#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

#include "book_data_small.h"

uint8_t current_page = 0;
uint16_t scroll_timer = 0;
uint8_t auto_scroll = 1;  // Start with auto-scroll on
#define SCROLL_DELAY 180  // ~3 seconds at 60fps

void draw_page(void) {
    const char* text = book_pages[current_page];
    uint8_t y;
    uint8_t i = 0;
    uint8_t line = 0;
    uint8_t col = 0;
    
    // Clear screen
    for (y = 0; y < 18; y++) {
        gotoxy(0, y);
        printf("                    ");
    }
    
    // Print text
    gotoxy(0, 0);
    while (text[i] != '\0' && line < 17) {
        if (text[i] == '\n') {
            line++;
            col = 0;
            gotoxy(0, line);
        } else {
            putchar(text[i]);
            col++;
            if (col >= 20) {
                col = 0;
                line++;
                gotoxy(0, line);
            }
        }
        i++;
    }
    
    // Status bar
    gotoxy(0, 17);
    printf("%u/%u %s", (uint8_t)(current_page + 1), (uint8_t)TOTAL_PAGES, 
           auto_scroll ? "AUTO" : "STOP");
}

void main(void) {
    uint8_t keys, prev_keys = 0, pressed;
    
    // Start immediately - no title screen
    draw_page();
    
    while (1) {
        keys = joypad();
        pressed = keys & ~prev_keys;
        
        // A = next page
        if (pressed & J_A) {
            if (current_page < TOTAL_PAGES - 1) {
                current_page++;
                draw_page();
                scroll_timer = 0;
            }
        }
        
        // B = previous page
        if (pressed & J_B) {
            if (current_page > 0) {
                current_page--;
                draw_page();
                scroll_timer = 0;
            }
        }
        
        // START = toggle auto-scroll
        if (pressed & J_START) {
            auto_scroll = !auto_scroll;
            draw_page();
            scroll_timer = 0;
        }
        
        // SELECT = go to start
        if (pressed & J_SELECT) {
            current_page = 0;
            draw_page();
            scroll_timer = 0;
        }
        
        // UP = skip forward 5 pages
        if (pressed & J_UP) {
            if (current_page + 5 < TOTAL_PAGES) {
                current_page += 5;
            } else {
                current_page = TOTAL_PAGES - 1;
            }
            draw_page();
            scroll_timer = 0;
        }
        
        // DOWN = skip back 5 pages  
        if (pressed & J_DOWN) {
            if (current_page >= 5) {
                current_page -= 5;
            } else {
                current_page = 0;
            }
            draw_page();
            scroll_timer = 0;
        }
        
        // Auto-scroll logic
        if (auto_scroll) {
            scroll_timer++;
            if (scroll_timer >= SCROLL_DELAY) {
                scroll_timer = 0;
                if (current_page < TOTAL_PAGES - 1) {
                    current_page++;
                    draw_page();
                } else {
                    // Loop back to start
                    current_page = 0;
                    draw_page();
                }
            }
        }
        
        prev_keys = keys;
        wait_vbl_done();
    }
}
