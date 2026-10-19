// SPDX-License-Identifier: GPL-2.0-or-later
// USB reader for 8BitDo Arcade (202c) and Ultimate 2 Wireless (310b).
// GIP/XInput packet layouts follow Linux drivers/input/joystick/xpad.c:
// https://github.com/torvalds/linux/blob/master/drivers/input/joystick/xpad.c
// For native kernels without xpad. Never detaches an existing kernel driver.
// Publishes atomic state in RAM; retries discovery after unplug/replug.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/usbdevice_fs.h>
#include <poll.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <time.h>
#include <unistd.h>

static volatile sig_atomic_t running=1;
static unsigned buttons,guide,lt,rt,reports;
static unsigned product=0x202c,ep_in=0x81,ep_out=0x01;
static int lx,ly,rx,ry,ready;
static void stop(int sig) { (void)sig; running=0; }
static long long millis(void) { struct timespec t; clock_gettime(CLOCK_REALTIME,&t); return (long long)t.tv_sec*1000+t.tv_nsec/1000000; }
static unsigned le16(const unsigned char *p) { return p[0]|((unsigned)p[1]<<8); }
static void reset(void) { buttons=guide=lt=rt=0; lx=ly=rx=ry=ready=0; }
static void publish(int connected) {
    FILE *f=fopen("/tmp/ac-controller.json.tmp","w"); if(!f)return;
    fprintf(f,"{\"connected\":%s,\"ready\":%s,\"at\":%lld,\"reports\":%u,\"buttons\":%u,\"guide\":%u,\"lt\":%u,\"rt\":%u,\"lx\":%d,\"ly\":%d,\"rx\":%d,\"ry\":%d,\"device\":\"%s\",\"product\":%u,\"transport\":\"USB\",\"triggerMax\":1023}\n",connected?"true":"false",ready?"true":"false",millis(),reports,buttons,guide,lt,rt,lx,ly,rx,ry,product==0x310b?"8BitDo Ultimate 2 Wireless":"8BitDo Arcade Controller",product);
    if(fclose(f)==0)rename("/tmp/ac-controller.json.tmp","/tmp/ac-controller.json");
}
static int number(const char *dir,const char *name,int base) {
    char path[512],buf[64]; snprintf(path,sizeof(path),"%s/%s",dir,name);
    FILE *f=fopen(path,"r"); if(!f)return -1;
    int ok=fgets(buf,sizeof(buf),f)!=NULL; fclose(f); return ok?(int)strtol(buf,NULL,base):-1;
}
static int endpoints(const char *device) {
    char path[1024];snprintf(path,sizeof(path),"%s:1.0",device);
    DIR *d=opendir(path);if(!d)return 0;
    struct dirent *e;ep_in=ep_out=0;
    while((e=readdir(d))) {
        if(strncmp(e->d_name,"ep_",3))continue;
        char endpoint[1536];snprintf(endpoint,sizeof(endpoint),"%s/%s",path,e->d_name);
        int addr=number(endpoint,"bEndpointAddress",16),attr=number(endpoint,"bmAttributes",16);
        if(addr<0||(attr&3)!=3)continue;
        if(addr&0x80)ep_in=(unsigned)addr;else ep_out=(unsigned)addr;
    }
    closedir(d);return ep_in&&ep_out;
}
static int discover(char *path,size_t size) {
    DIR *d=opendir("/sys/bus/usb/devices"); if(!d)return 0;
    struct dirent *e; int found=0;
    while((e=readdir(d))) {
        char dir[512]; snprintf(dir,sizeof(dir),"/sys/bus/usb/devices/%s",e->d_name);
        if(number(dir,"idVendor",16)!=0x2dc8)continue;
        int pid=number(dir,"idProduct",16);
        if((pid!=0x202c&&pid!=0x310b)||!endpoints(dir))continue;
        product=(unsigned)pid;
        int bus=number(dir,"busnum",10),dev=number(dir,"devnum",10);
        if(bus>0&&dev>0) { snprintf(path,size,"/dev/bus/usb/%03d/%03d",bus,dev); found=1; break; }
    }
    closedir(d);return found;
}
static int send_packet(int fd,unsigned char *data,unsigned length) {
    struct usbdevfs_bulktransfer t={.ep=ep_out,.len=length,.timeout=500,.data=data};
    int n=ioctl(fd,USBDEVFS_BULK,&t);
    if(n!=(int)length)fprintf(stderr,"USB output %02x: %s (%d)\n",data[0],strerror(errno),n);
    return n==(int)length;
}
static void initialize(int fd) {
    if(product==0x310b) {
        unsigned char led[]={1,3,2}; // Assign player one, as xpad's LED probe does.
        send_packet(fd,led,sizeof(led));
        // xpad_start_input's vendor read enables reports on third-party XInput pads.
        unsigned char reply[20]={0};
        struct usbdevfs_ctrltransfer t={.bRequestType=0xc1,.bRequest=1,
            .wValue=0x100,.wIndex=0,.wLength=sizeof(reply),.timeout=250,.data=reply};
        int n=ioctl(fd,USBDEVFS_CONTROL,&t);
        if(n<0)fprintf(stderr,"XInput startup: %s\n",strerror(errno));
        else {
            fprintf(stderr,"XInput startup: %d bytes:",n);
            for(int i=0;i<n&&i<(int)sizeof(reply);i++)fprintf(stderr," %02x",reply[i]);
            fprintf(stderr,"\n");
        }
        return;
    }
    unsigned char power[]={5,0x20,1,1,0};
    unsigned char led[]={10,0x20,2,3,0,1,0x14};
    unsigned char auth[]={6,0x20,3,2,1,0};
    send_packet(fd,power,sizeof(power));
    send_packet(fd,led,sizeof(led));
    send_packet(fd,auth,sizeof(auth));
}
static int decode(const unsigned char *p,int n) {
    if(n<4||p[3]>(unsigned)(n-4))return 0;
    if(p[0]==0x20&&p[3]>=14&&n>=18) {
        buttons=le16(p+4);lt=le16(p+6);rt=le16(p+8);
        lx=(int16_t)le16(p+10);ly=-(int16_t)le16(p+12);
        rx=(int16_t)le16(p+14);ry=-(int16_t)le16(p+16);
        ready=1;reports++;return 1;
    }
    if(p[0]==7&&p[3]>=1&&n>=5) { guide=!!(p[4]&3);return 1; }
    return 0;
}
static int decode_xinput(const unsigned char *p,int n) {
    if(n<20||p[0]!=0||p[1]!=20)return 0;
    // Keep the GIP button/trigger schema consumed by the existing pieces.
    buttons=((unsigned)(p[2]&15)<<8)|(p[3]&0xf0)|((unsigned)(p[3]&3)<<12)
        |((p[2]&0x10)?4:0)|((p[2]&0x20)?8:0)
        |((p[2]&0x40)?0x4000:0)|((p[2]&0x80)?0x8000:0);
    guide=!!(p[3]&4);lt=(unsigned)p[4]*1023/255;rt=(unsigned)p[5]*1023/255;
    lx=(int16_t)le16(p+6);ly=-(int16_t)le16(p+8);
    rx=(int16_t)le16(p+10);ry=-(int16_t)le16(p+12);
    ready=1;reports++;return 1;
}
#ifndef AC_CONTROLLER_TEST
int main(void) {
    int lock=open("/tmp/ac-usb-controller.lock",O_CREAT|O_RDWR|O_CLOEXEC,0600);
    if(lock<0||flock(lock,LOCK_EX|LOCK_NB)<0)return 0;
    signal(SIGTERM,stop);signal(SIGINT,stop);signal(SIGHUP,SIG_IGN);
    while(running) {
        char path[128];reset();publish(0);
        if(!discover(path,sizeof(path))) { usleep(500000);continue; }
        int fd=open(path,O_RDWR|O_CLOEXEC);if(fd<0){usleep(500000);continue;}
        unsigned iface=0;
        if(ioctl(fd,USBDEVFS_CLAIMINTERFACE,&iface)<0){fprintf(stderr,"claim: %s\n",strerror(errno));close(fd);sleep(1);continue;}
        unsigned char buffer[64]={0};
        struct usbdevfs_urb urb={.type=USBDEVFS_URB_TYPE_INTERRUPT,.endpoint=ep_in,.buffer=buffer,.buffer_length=sizeof(buffer)};
        if(ioctl(fd,USBDEVFS_SUBMITURB,&urb)<0){perror("submit");close(fd);sleep(1);continue;}
        fprintf(stderr,"Connected %s (8BitDo %04x, in=%02x out=%02x)\n",path,product,ep_in,ep_out);
        initialize(fd);publish(1);
        long long last=millis();unsigned logged=0;
        while(running) {
            void *done=NULL;int status=ioctl(fd,USBDEVFS_REAPURBNDELAY,&done);
            if(status==0) {
                if(done!=&urb||urb.status<0)break;
                int n=urb.actual_length;
                if(logged++<6){fprintf(stderr,"USB report:");for(int j=0;j<n;j++)fprintf(stderr," %02x",buffer[j]);fprintf(stderr,"\n");}
                int changed=product==0x310b?decode_xinput(buffer,n):decode(buffer,n);
                if(product==0x202c&&n>=5&&buffer[0]==7&&buffer[1]==0x30){
                    unsigned char ack[]={1,0x20,buffer[2],9,0,7,0x20,2,0,0,0,0,0};
                    send_packet(fd,ack,sizeof(ack));
                }
                if(product==0x202c&&n>=4&&buffer[0]==2)initialize(fd);
                if(changed){publish(1);last=millis();}
                if(ioctl(fd,USBDEVFS_SUBMITURB,&urb)<0)break;
            } else if(errno!=EAGAIN&&errno!=EINTR)break;
            if(millis()-last>=100){publish(1);last=millis();}
            struct pollfd wait={.fd=fd,.events=POLLOUT};
            if(poll(&wait,1,8)>0&&(wait.revents&(POLLERR|POLLHUP|POLLNVAL)))break;
        }
        ioctl(fd,USBDEVFS_DISCARDURB,&urb);
        // Closing usbfs cancels and drains all pending URBs before storage dies.
        close(fd);reset();publish(0);usleep(250000);
    }
    reset();publish(0);close(lock);return 0;
}
#endif
