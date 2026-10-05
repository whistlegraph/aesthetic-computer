// Linux: gcc -Wall -Wextra -Werror ac-usb-controller-test.c -o /tmp/controller-test
#define main controller_main
#include "ac-usb-controller.c"
#undef main
#include <assert.h>
int main(void) {
    unsigned char packet[36]={0x20,0,1,32,0x10,0x09,0xff,3,0,2,0,0x80,0xff,0x7f,0xff,0x7f,0,0x80};
    reset();assert(decode(packet,sizeof(packet))==1);
    assert(ready&&reports==1&&buttons==0x0910&&lt==1023&&rt==512);
    assert(lx==-32768&&ly==-32767&&rx==32767&&ry==32768);
    // Invalid/truncated reports must never clear or overwrite a held control.
    for(int n=0;n<36;n++)assert(decode(packet,n)==0);
    assert(buttons==0x0910&&reports==1);
    packet[3]=0;assert(decode(packet,sizeof(packet))==0);
    unsigned char xbox[]={7,0x30,2,1,1};assert(decode(xbox,5)&&guide==1);
    xbox[4]=0;assert(decode(xbox,5)&&guide==0);
    memset(packet+4,0,sizeof(packet)-4);packet[3]=32;
    assert(decode(packet,sizeof(packet))&&buttons==0&&lt==0&&lx==0);
    buttons=65535;guide=1;lt=1023;reset();
    assert(!buttons&&!guide&&!lt&&!ready&&!lx);
    puts("PASS: held combinations, signed axes, triggers, guide, release, malformed packets, disconnect reset");
}
