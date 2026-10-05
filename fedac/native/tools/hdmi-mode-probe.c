// Temporary HDMI diagnostic for existing AC OS builds. LD_PRELOAD into ac-native.
// Filters only advertised progressive HDMI modes, leaving primary eDP untouched.
// A request must be confirmed within 20 seconds or the previous selection returns.
#define _GNU_SOURCE
#include <xf86drmMode.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

typedef struct { unsigned w,h,hz,serial; } Choice;
static Choice good,active;
static unsigned seen;
static int pending,initialized,disconnect_once;
static double deadline;
static double seconds(void){struct timespec t;clock_gettime(CLOCK_MONOTONIC,&t);return t.tv_sec+t.tv_nsec/1e9;}
static Choice request(void){Choice c={0};FILE*f=fopen("/tmp/ac-hdmi-request","r");if(f){if(fscanf(f,"%u %u %u %u",&c.w,&c.h,&c.hz,&c.serial)!=4)c=(Choice){0};fclose(f);}return c;}
static void publish(const char *error){
 FILE*f=fopen("/tmp/ac-hdmi-probe.json.new","w");if(!f)return;
 fprintf(f,"{\"pending\":%s,\"serial\":%u,\"width\":%u,\"height\":%u,\"hz\":%u,\"seconds\":%.0f,\"error\":\"%s\"}\n",pending?"true":"false",active.serial,active.w,active.h,active.hz,pending?deadline-seconds():0,error?error:"");
 fclose(f);rename("/tmp/ac-hdmi-probe.json.new","/tmp/ac-hdmi-probe.json");
}
static int find_mode(drmModeConnector*c,Choice v){
 if(!v.w&&!v.h&&!v.hz)return -2;
 for(int i=0;i<c->count_modes;i++){drmModeModeInfo*m=&c->modes[i];
  if(!(m->flags&(DRM_MODE_FLAG_INTERLACE|DRM_MODE_FLAG_DBLSCAN))&&m->hdisplay==v.w&&m->vdisplay==v.h&&m->vrefresh==v.hz)return i;
 }return -1;
}
drmModeConnectorPtr drmModeGetConnector(int fd,uint32_t id){
 static drmModeConnectorPtr(*real)(int,uint32_t);
 if(!real)real=dlsym(RTLD_NEXT,"drmModeGetConnector");
 if(!real)return NULL;
 drmModeConnectorPtr c=real(fd,id);if(!c)return c;
 if(c->connection!=DRM_MODE_CONNECTED||!c->count_modes||
   (c->connector_type!=DRM_MODE_CONNECTOR_HDMIA&&c->connector_type!=DRM_MODE_CONNECTOR_HDMIB))return c;
 // First call is initialization; no request means the normal runtime picker.
 if(!initialized){initialized=1;good=active=(Choice){0};publish(NULL);}
 Choice next=request();
 if(next.serial!=seen){
  seen=next.serial;
  if(find_mode(c,next)==-1){publish("Mode not offered by this HDMI connection");}
  else {active=next;pending=1;deadline=seconds()+20;disconnect_once=1;publish(NULL);}
 }
 if(pending){
  unsigned confirm=0;FILE*f=fopen("/tmp/ac-hdmi-confirm","r");if(f){if(fscanf(f,"%u",&confirm)!=1)confirm=0;fclose(f);}
  if(confirm==active.serial&&confirm){good=active;pending=0;publish(NULL);}
  else if(seconds()>=deadline){active=good;pending=0;disconnect_once=1;publish("Reverted: test was not confirmed");}
  else publish(NULL);
 }
 if(disconnect_once){disconnect_once=0;c->connection=DRM_MODE_DISCONNECTED;return c;}
 int index=find_mode(c,active);
 if(index>=0){c->modes[0]=c->modes[index];c->count_modes=1;}
 return c;
}
