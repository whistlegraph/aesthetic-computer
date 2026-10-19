// Linux: gcc -Wall -Wextra -Werror ac-usb-controller-test.c -o /tmp/controller-test
#define main controller_main
#define ioctl test_ioctl
#include "ac-usb-controller.c"
#undef main
#undef ioctl
#include <assert.h>
#include <stdarg.h>
static unsigned startup_requests,led_requests;
int test_ioctl(int fd,unsigned long request,...) {
    assert(fd==42);
    va_list args;va_start(args,request);
    if(request==USBDEVFS_BULK) {
        struct usbdevfs_bulktransfer *t=va_arg(args,struct usbdevfs_bulktransfer *);
        va_end(args);
        assert(t->ep==ep_out&&t->len==3&&!memcmp(t->data,"\x01\x03\x02",3));
        led_requests++;return t->len;
    }
    assert(request==USBDEVFS_CONTROL&&led_requests==1);
    struct usbdevfs_ctrltransfer *t=va_arg(args,struct usbdevfs_ctrltransfer *);
    va_end(args);
    assert(t->bRequestType==0xc1&&t->bRequest==1&&t->wValue==0x100);
    assert(t->wIndex==0&&t->wLength==20&&t->timeout>0&&t->data);
    memset(t->data,0,t->wLength);startup_requests++;return t->wLength;
}
int main(void) {
    product=0x310b;initialize(42);assert(startup_requests==1&&led_requests==1);
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
    unsigned char xinput[20]={0,20,0xff,0xf7,255,128,0,0x80,0xff,0x7f,0xff,0x7f,0,0x80};
    assert(decode_xinput(xinput,sizeof(xinput))&&buttons==0xfffc&&guide==1);
    assert(lt==1023&&rt==513&&lx==-32768&&ly==-32767&&rx==32767&&ry==32768);
    unsigned before=reports;
    for(int n=0;n<20;n++)assert(!decode_xinput(xinput,n));
    xinput[1]=19;assert(!decode_xinput(xinput,20));xinput[1]=20;
    xinput[0]=1;assert(!decode_xinput(xinput,20));xinput[0]=0;
    assert(reports==before&&buttons==0xfffc&&guide==1);
    memset(xinput+2,0,18);assert(decode_xinput(xinput,20));
    assert(buttons==0&&guide==0&&lt==0&&rt==0&&lx==0&&ry==0);
    // Verify each physical button independently, including the guide bit.
    const unsigned expected[16]={0x100,0x200,0x400,0x800,4,8,0x4000,0x8000,
        0x1000,0x2000,0,0,0x10,0x20,0x40,0x80};
    for(unsigned bit=0;bit<16;bit++) {
        xinput[2]=xinput[3]=0;xinput[2+bit/8]=(unsigned char)(1u<<(bit%8));
        assert(decode_xinput(xinput,20)&&buttons==expected[bit]);
        assert(guide==(bit==10));
    }
    puts("PASS: held combinations, signed axes, triggers, guide, release, malformed packets, disconnect reset");
}
