// Minimal test ROM
#include <gb/gb.h>
#include <stdio.h>

void main(void) {
    printf("Hello Ulysses!");
    
    while(1) {
        wait_vbl_done();
    }
}
