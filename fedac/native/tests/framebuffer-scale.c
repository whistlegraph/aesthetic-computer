#include "../src/framebuffer.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  for(int scale=1;scale<=4;scale++)for(int w=1;w<=13;w++)for(int h=1;h<=9;h++) {
    ACFramebuffer *src=fb_create(3,2);
    for(int y=0;y<2;y++)for(int x=0;x<3;x++)src->pixels[y*src->stride+x]=0xff000000u+y*10+x;
    int stride=w+2; uint32_t *out=malloc((size_t)stride*h*4);
    for(int i=0;i<stride*h;i++)out[i]=0x12345678;
    fb_copy_scaled(src,out,w,h,stride,scale);
    for(int y=0;y<h;y++)for(int x=0;x<stride;x++) {
      if(x>=w)assert(out[y*stride+x]==0x12345678);
      else { int sx=x/scale,sy=y/scale; if(sx>2)sx=2;if(sy>1)sy=1;
        assert(out[y*stride+x]==src->pixels[sy*src->stride+sx]); }
    }
    free(out);fb_destroy(src);
  }
  for(int sw=1;sw<=7;sw++)for(int sh=1;sh<=5;sh++)
  for(int w=1;w<=17;w++)for(int h=1;h<=13;h++) {
    ACFramebuffer *src=fb_create(sw+2,sh);src->width=sw;
    for(int y=0;y<sh;y++)for(int x=0;x<sw;x++)src->pixels[y*src->stride+x]=0xff000000u+y*100+x;
    int stride=w+3;uint32_t *out=malloc((size_t)stride*h*4);
    for(int i=0;i<stride*h;i++)out[i]=0x12345678;
    fb_copy_resized(src,out,w,h,stride);
    for(int y=0;y<h;y++)for(int x=0;x<stride;x++) {
      if(x>=w)assert(out[y*stride+x]==0x12345678);
      else assert(out[y*stride+x]==src->pixels[(y*sh/h)*src->stride+x*sw/w]);
    }
    free(out);fb_destroy(src);
  }
  puts("framebuffer scaling: integer/fractional, up/down, clipping, stride padding pass");
}
