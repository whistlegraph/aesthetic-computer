// Hello World in C for GameBoy using GBDK
#include <gb/gb.h>
#include <stdio.h>

void main(void)
{
    printf("Hello World!\n");
    printf("Press Start");
    
    // Wait forever
    while(1) {
        wait_vbl_done();
    }
}
