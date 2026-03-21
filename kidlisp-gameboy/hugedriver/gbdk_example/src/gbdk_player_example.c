#include <gb/gb.h>
#include "hUGEDriver.h"

extern const hUGESong_t sample_song;

UBYTE joy;
UBYTE c0 = 0, c1 = 0, c2 = 0, c3 = 0;

const unsigned char pattern1[] = {0x80,0x80,0x40,0x40,0x20,0x20,0x10,0x10,0x08,0x08,0x04,0x04,0x02,0x02,0x01,0x01};
const unsigned char pattern2[] = {0x00,0x00,0x7E,0x7E,0x40,0x40,0x54,0x54,0x48,0x48,0x54,0x54,0x40,0x40,0x00,0x00};
const unsigned char map[]      = {0x00,0x20};

void main(void) {
    LCDC_REG = 0xD1;
    BGP_REG  = 0b11100100;
    NR52_REG = 0x80;
    NR51_REG = 0xFF;
    NR50_REG = 0x77;

    set_bkg_data(0, 1, pattern1);
    set_bkg_data(0x20, 1, pattern2);

    __critical {
        hUGE_init(&sample_song);
        add_VBL(hUGE_dosound);
    }
        
    while(1) {
        wait_vbl_done();
        joy = joypad();
        switch (joy) {
            case J_UP    : c0 ^= 1; hUGE_mute_channel(HT_CH1, c0); set_bkg_tiles(0,0,1,1,&map[c0 & 1]); waitpadup(); break;
            case J_DOWN  : c1 ^= 1; hUGE_mute_channel(HT_CH2, c1); set_bkg_tiles(1,0,1,1,&map[c1 & 1]); waitpadup(); break;
            case J_LEFT  : c2 ^= 1; hUGE_mute_channel(HT_CH3, c2); set_bkg_tiles(2,0,1,1,&map[c2 & 1]); waitpadup(); break;
            case J_RIGHT : c3 ^= 1; hUGE_mute_channel(HT_CH4, c3); set_bkg_tiles(3,0,1,1,&map[c3 & 1]); waitpadup(); break;
        }
    }
}
