// SPDX-License-Identifier: GPL-2.0-or-later
// USB reader for the 8BitDo Arcade Controller for Xbox (2dc8:202c).
// GIP packet layout/init follows Linux drivers/input/joystick/xpad.c:
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
static int lx,ly,rx,ry,ready;
static void stop(int sig) { (void)sig; running=0; }
static long long millis(void) { struct timespec t; clock_gettime(CLOCK_REALTIME,&t); return (long long)t.tv_sec*1000+t.tv_nsec/1000000; }
static unsigned le16(const unsigned char *p) { return p[0]|((unsigned)p[1]<<8); }
static void reset(void) { buttons=guide=lt=rt=0; lx=ly=rx=ry=ready=0; }
static void publish(int connected) {
    FILE *f=fopen("/tmp/ac-controller.json.tmp","w"); if(!f)return;
    fprintf(f,"{\"connected\":%s,\"ready\":%s,\"at\":%lld,\"reports\":%u,\"buttons\":%u,\"guide\":%u,\"lt\":%u,\"rt\":%u,\"lx\":%d,\"ly\":%d,\"rx\":%d,\"ry\":%d}\n",connected?"true":"false",ready?"true":"false",millis(),reports,buttons,guide,lt,rt,lx,ly,rx,ry);
    if(fclose(f)==0)rename("/tmp/ac-controller.json.tmp","/tmp/ac-controller.json");
}
static int number(const char *dir,const char *name,int base) {
    char path[512],buf[64]; snprintf(path,sizeof(path),"%s/%s",dir,name);
    FILE *f=fopen(path,"r"); if(!f)return -1;
    int ok=fgets(buf,sizeof(buf),f)!=NULL; fclose(f); return ok?(int)strtol(buf,NULL,base):-1;
}
static int discover(char *path,size_t size) {
    DIR *d=opendir("/sys/bus/usb/devices"); if(!d)return 0;
    struct dirent *e; int found=0;
    while((e=readdir(d))) {
        char dir[512]; snprintf(dir,sizeof(dir),"/sys/bus/usb/devices/%s",e->d_name);
        if(number(dir,"idVendor",16)!=0x2dc8||number(dir,"idProduct",16)!=0x202c)continue;
        int bus=number(dir,"busnum",10),dev=number(dir,"devnum",10);
        if(bus>0&&dev>0) { snprintf(path,size,"/dev/bus/usb/%03d/%03d",bus,dev); found=1; break; }
    }
    closedir(d);return found;
}
static int send_packet(int fd,unsigned char *data,unsigned length) {
    struct usbdevfs_bulktransfer t={.ep=0x01,.len=length,.timeout=500,.data=data};
    int n=ioctl(fd,USBDEVFS_BULK,&t);
    if(n!=(int)length)fprintf(stderr,"USB output %02x: %s (%d)\n",data[0],strerror(errno),n);
    return n==(int)length;
}
static void initialize(int fd) {
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
        struct usbdevfs_urb urb={.type=USBDEVFS_URB_TYPE_INTERRUPT,.endpoint=0x81,.buffer=buffer,.buffer_length=sizeof(buffer)};
        if(ioctl(fd,USBDEVFS_SUBMITURB,&urb)<0){perror("submit");close(fd);sleep(1);continue;}
        fprintf(stderr,"Connected %s (8BitDo Arcade Controller)\n",path);
        initialize(fd);publish(1);
        long long last=millis();unsigned logged=0;
        while(running) {
            void *done=NULL;int status=ioctl(fd,USBDEVFS_REAPURBNDELAY,&done);
            if(status==0) {
                if(done!=&urb||urb.status<0)break;
                int n=urb.actual_length;
                if(logged++<6){fprintf(stderr,"GIP:");for(int j=0;j<n;j++)fprintf(stderr," %02x",buffer[j]);fprintf(stderr,"\n");}
                int changed=decode(buffer,n);
                if(n>=5&&buffer[0]==7&&buffer[1]==0x30){
                    unsigned char ack[]={1,0x20,buffer[2],9,0,7,0x20,2,0,0,0,0,0};
                    send_packet(fd,ack,sizeof(ack));
                }
                if(n>=4&&buffer[0]==2)initialize(fd);
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
