#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "framebuffer.h"
void ac_log(const char *fmt,...) { va_list ap;va_start(ap,fmt);vfprintf(stderr,fmt,ap);va_end(ap); }
#include "screen-gpu.h"
int main(void) {
 if(!ac_gpu_begin(32,32,0,0,0))return 1;
 double v[]={0,0,-1.4,32,0,-1.4,0,32,-1.4,255,0,0};ac_gpu_triangle(v);
 uint32_t data[1024];ACFramebuffer fb={.width=32,.height=32,.stride=32,.pixels=data};ac_gpu_end(&fb);
 if(data[33]!=0xffff0000u)return 2;
 const int sides[]={6,8,12,16,24,32};
 for(int k=0;k<6;k++) {
   int n=sides[k]; uint32_t expected[1024];
   if(!ac_gpu_begin(32,32,0,0,0))return 3;
   double ring[64];
   for(int i=0;i<n;i++) { ring[i*2]=cos(i*2*M_PI/n);ring[i*2+1]=sin(i*2*M_PI/n); }
   for(int i=2;i<n;i++) {
     double t[]={29,16,-1.4,16+ring[(i-1)*2]*13,16+ring[(i-1)*2+1]*13,-1.4,
       16+ring[i*2]*13,16+ring[i*2+1]*13,-1.4,38,82,176};
     ac_gpu_triangle(t);
   }
   fb.pixels=expected;ac_gpu_end(&fb);
   if(!ac_gpu_begin(32,32,0,0,0))return 4;
   if(ac_gpu_disc(16,16,-1.4,13,n,38,82,176)!=n-2)return 5;
   fb.pixels=data;ac_gpu_end(&fb);
   if(memcmp(data,expected,sizeof(data)))return 6;
 }
 puts("GPU depth and native disc pixel parity passed");return 0;
}
