// Temporary userspace Xbox-360 protocol bridge for AC OS kernels without
// xpad. Claims only the user-selected M30 interface; never detaches drivers.
// Build: cc -O2 -Wall oskiewar-usb-pad.c -o oskiewar-usb-pad
#define _POSIX_C_SOURCE 200809L
#include <linux/usbdevice_fs.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <dirent.h>

static volatile sig_atomic_t running=1;
static void stop(int sig) { (void)sig; running=0; }
static long long millis(void) {
  struct timespec t; clock_gettime(CLOCK_REALTIME,&t);
  return (long long)t.tv_sec*1000+t.tv_nsec/1000000;
}
static int number(const char *dir,const char *file,int base) {
  char path[512],s[64]; snprintf(path,sizeof(path),"%s/%s",dir,file);
  FILE *f=fopen(path,"r"); if(!f)return -1;
  int value=fgets(s,sizeof(s),f)?(int)strtol(s,0,base):-1; fclose(f); return value;
}
static int discover(char *path,size_t size) {
  DIR *dir=opendir("/sys/bus/usb/devices"); if(!dir)return 0;
  struct dirent *e; int found=0;
  while((e=readdir(dir))) {
    char device[512],product[600],name[128];
    snprintf(device,sizeof(device),"/sys/bus/usb/devices/%s",e->d_name);
    if(number(device,"idVendor",16)!=0x045e || number(device,"idProduct",16)!=0x028e)continue;
    snprintf(product,sizeof(product),"%s/product",device);
    FILE *f=fopen(product,"r"); if(!f)continue;
    int match=fgets(name,sizeof(name),f) && strstr(name,"8BitDo M30"); fclose(f);
    if(!match)continue;
    int bus=number(device,"busnum",10),dev=number(device,"devnum",10);
    if(bus<0||dev<0)continue;
    snprintf(path,size,"/dev/bus/usb/%03d/%03d",bus,dev); found=1; break;
  }
  closedir(dir); return found;
}
static int16_t axis(const unsigned char *p) { return (int16_t)(p[0]|(p[1]<<8)); }
static void publish(int connected,const unsigned char *b) {
  const char *names[]={"ArrowUp","ArrowDown","ArrowLeft","ArrowRight","Menu","View","LeftThumbstick","RightThumbstick",
    "LeftShoulder","RightShoulder","Guide",0,"A","B","X","Y"};
  FILE *f=fopen("/tmp/oskiewar-gamepads.json.new","w"); if(!f)return;
  fprintf(f,"{\"at\":%lld,\"pads\":[",millis());
  if(connected) {
    fprintf(f,"{\"connected\":true,\"id\":\"8BitDo M30 gamepad\",\"name\":\"8BitDo M30 gamepad\",\"down\":[");
    unsigned mask=b[2]|(b[3]<<8); int comma=0;
    for(int i=0;i<16;i++)if((mask&(1u<<i)) && names[i]) {
      fprintf(f,"%s\"%s\"",comma?",":"",names[i]); comma=1;
    }
    if(b[4]>30){fprintf(f,"%s\"LeftTrigger\"",comma?",":"");comma=1;}
    if(b[5]>30)fprintf(f,"%s\"RightTrigger\"",comma?",":"");
    fprintf(f,"],\"leftX\":%.5f,\"leftY\":%.5f,\"rightX\":%.5f,\"rightY\":%.5f}",
      axis(b+6)/32768.f,axis(b+8)/32768.f,axis(b+10)/32768.f,axis(b+12)/32768.f);
  }
  fprintf(f,"]}\n"); fclose(f);
  rename("/tmp/oskiewar-gamepads.json.new","/tmp/oskiewar-gamepads.json");
}
int main(void) {
  signal(SIGINT,stop); signal(SIGTERM,stop);
  unsigned char state[32]={0};
  while(running) {
    char path[128]; publish(0,state);
    if(!discover(path,sizeof(path))){sleep(1);continue;}
    int fd=open(path,O_RDWR|O_CLOEXEC); if(fd<0){sleep(1);continue;}
    unsigned int interface=0;
    if(ioctl(fd,USBDEVFS_CLAIMINTERFACE,&interface)<0){close(fd);sleep(1);continue;}
    unsigned char packet[32]={0};
    struct usbdevfs_urb urb={.type=USBDEVFS_URB_TYPE_INTERRUPT,.endpoint=0x81,.buffer=packet,.buffer_length=sizeof(packet)};
    memset(state,0,sizeof(state));
    fprintf(stderr,"M30 connected: %s\n",path);
    int submitted=ioctl(fd,USBDEVFS_SUBMITURB,&urb)==0;
    long long last=0;
    while(running && submitted) {
      struct usbdevfs_urb *done=0;
      if(ioctl(fd,USBDEVFS_REAPURBNDELAY,&done)==0) {
        if(done->status<0)break;
        if(done->actual_length>=14 && packet[0]==0)memcpy(state,packet,sizeof(state));
        submitted=ioctl(fd,USBDEVFS_SUBMITURB,&urb)==0;
      } else if(errno!=EAGAIN && errno!=EINTR)break;
      long long now=millis();
      if(now-last>=16){publish(1,state);last=now;}
      struct timespec wait={0,2000000}; nanosleep(&wait,0);
    }
    ioctl(fd,USBDEVFS_DISCARDURB,&urb);
    ioctl(fd,USBDEVFS_RELEASEINTERFACE,&interface); close(fd);
    fprintf(stderr,"M30 disconnected\n");
  }
  publish(0,state); return 0;
}
