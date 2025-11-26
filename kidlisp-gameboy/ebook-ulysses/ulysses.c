// Ulysses eBook Reader for Game Boy
// James Joyce's Ulysses from Project Gutenberg

#include <gb/gb.h>
#include <stdio.h>
#include <gbdk/console.h>

#include "book_data.h"

// Current page index
uint16_t current_page = 0;

// Display state
uint8_t needs_redraw = 1;

void clear_screen(void) {
    uint8_t y;
    for (y = 0; y < 18; y++) {
        gotoxy(0, y);
        printf("                    ");
    }
}

void draw_page(void) {
    const char* page_text;
    uint8_t line = 0;
    uint8_t col = 0;
    uint16_t i = 0;
    
    clear_screen();
    
    // Get current page text
    page_text = book_pages[current_page];
    
    // Reset position
    gotoxy(0, 0);
    
    // Print character by character
    while (page_text[i] != '\0' && line < 16) {
        if (page_text[i] == '\n') {
            line++;
            col = 0;
            gotoxy(0, line);
        } else if (col < 20) {
            putchar(page_text[i]);
            col++;
        }
        i++;
    }
    
    // Draw page number at bottom
    gotoxy(0, 17);
    printf(" %u/%u", (uint16_t)(current_page + 1), (uint16_t)TOTAL_PAGES);
}

void draw_title_screen(void) {
    clear_screen();
    gotoxy(0, 2);
    printf("      ULYSSES       ");
    gotoxy(0, 4);
    printf("    James Joyce     ");
    gotoxy(0, 7);
    printf("    -- eBook --     ");
    gotoxy(0, 10);
    printf("  Project Gutenberg ");
    gotoxy(0, 14);
    printf("   Press START      ");
    gotoxy(0, 16);
    printf("  A:Next  B:Prev    ");
}

void handle_input(void) {
    uint8_t keys = joypad();
    static uint8_t prev_keys = 0;
    uint8_t pressed = keys & ~prev_keys;
    
    if (pressed & J_A || pressed & J_RIGHT) {
        if (current_page < TOTAL_PAGES - 1) {
            current_page++;
            needs_redraw = 1;
        }
    }
    
    if (pressed & J_B || pressed & J_LEFT) {
        if (current_page > 0) {
            current_page--;
            needs_redraw = 1;
        }
    }
    
    if (pressed & J_UP) {
        if (current_page + 10 < TOTAL_PAGES) {
            current_page += 10;
        } else {
            current_page = TOTAL_PAGES - 1;
        }
        needs_redraw = 1;
    }
    
    if (pressed & J_DOWN) {
        if (current_page >= 10) {
            current_page -= 10;
        } else {
            current_page = 0;
        }
        needs_redraw = 1;
    }
    
    if (pressed & J_SELECT) {
        current_page = 0;
        needs_redraw = 1;
    }
    
    prev_keys = keys;
}

void main(void) {
    // Show title screen
    draw_title_screen();
    
    // Wait for START to begin
    while (!(joypad() & J_START)) {
        wait_vbl_done();
    }
    
    // Small delay to debounce
    delay(100);
    
    // Main loop
    while (1) {
        if (needs_redraw) {
            draw_page();
            needs_redraw = 0;
        }
        
        wait_vbl_done();
        handle_input();
    }
}
