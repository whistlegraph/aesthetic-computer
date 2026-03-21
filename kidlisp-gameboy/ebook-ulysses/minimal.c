// Minimal test
#include <stdio.h>
#include <gbdk/platform.h>
#include <gbdk/font.h>
#include <gbdk/console.h>

void main(void) {
    font_t ibm;
    
    font_init();
    ibm = font_load(font_ibm);
    font_set(ibm);
    
    printf("Hello World!");
    
    while(1) {
        vsync();
    }
}
