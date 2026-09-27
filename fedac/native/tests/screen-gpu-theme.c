#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/stat.h>
void ac_log(const char *fmt,...) {(void)fmt;}
typedef struct { int width,height,stride; uint32_t *pixels; } ACFramebuffer;
#define HAVE_SCREEN_GPU
#include "screen-gpu.h"
static void solid_fixture(const char *path,int height) {
  FILE *f=fopen(path,"wb");assert(f);
  unsigned char p[]={255,0,0,128};
  for(int i=0;i<1024*height;i++)assert(fwrite(p,1,4,f)==4);
  fclose(f);
}
static void fixture(const char *path,int height,int alpha) {
  FILE *f=fopen(path,"wb");assert(f);
  for(int y=0;y<height;y++)for(int x=0;x<1024;x++) {
    unsigned char p[4]={x<512?255:0,x<512?0:255,y<height/2?0:255,255};
    if(alpha)p[3]=(x<512)?0:255;
    assert(fwrite(p,1,4,f)==4);
  }
  fclose(f);
}
int main(void) {
 mkdir("/pieces",0755);mkdir("/pieces/oskiewar-theme",0755);
 fixture("/pieces/oskiewar-theme/underpass-1024x576.rgba",576,0);
 fixture("/pieces/oskiewar-theme/props-1024x512.rgba",512,1);
 uint32_t pixels[64*64];ACFramebuffer fb={64,64,64,pixels};
 assert(ac_gpu_begin(64,64,0,0,0));assert(ac_gpu_theme_ready());
 double bg[]={0,0,1672,941,32,32,64,64,0,.8};
 assert(ac_gpu_theme_sprite(0,bg,0,1));
 ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xffff0000);assert(pixels[8*64+56]==0xff00ff00);
 assert(pixels[56*64+8]==0xffff00ff);assert(pixels[56*64+56]==0xff00ffff);
 assert(ac_gpu_begin(64,64,0,0,0));assert(ac_gpu_theme_sprite(0,bg,1,1));ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xff00ff00);assert(pixels[8*64+56]==0xffff0000);
 assert(ac_gpu_begin(64,64,0,0,0));
 double quad[]={0,0,1672,941,0,0,.8,64,0,.8,64,64,.8,0,64,.8};
 assert(ac_gpu_theme_quad(0,quad));
 double prop[]={0,0,1774,887,32,32,64,64,0,-.5};
 assert(ac_gpu_theme_sprite(1,prop,0,1));
 // A later blue face is behind the opaque sprite but ahead of background.
 double tri[]={0,0,0,64,0,0,0,64,0,0,0,255};ac_gpu_triangle(tri);
 ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xff0000ff); // transparent prop does not write depth
 assert(pixels[8*64+48]==0xff00ff00); // opaque prop wins depth
 assert(ac_gpu_begin(64,64,0,0,0));
 quad[0]=-1;assert(!ac_gpu_theme_quad(0,quad));
 assert(!ac_gpu_theme_sprite(2,prop,0,1));
 assert(ac_gpu_theme_ready()); // Missing optional assets never disable base theme.
 solid_fixture("/pieces/oskiewar-theme/explosions-v1-1024x512.rgba",512);
 solid_fixture("/pieces/oskiewar-theme/weapons-v2-1024x1024.rgba",1024);
 ac_gpu.theme_asset_retry_at[2]=0;
 assert(ac_gpu_theme_asset_ready(2));assert(ac_gpu_theme_asset_ready(3));
 assert(!ac_gpu_theme_asset_ready(-1));assert(!ac_gpu_theme_asset_ready(5));
 assert(!ac_gpu_theme_asset_ready(4));assert(ac_gpu_theme_ready());
 GLuint retained=ac_gpu.theme_textures[2];
 assert(ac_gpu_theme_asset_ready(2));assert(retained==ac_gpu.theme_textures[2]);
 double effect[]={0,0,1774,887,32,32,64,64,0,-.5};
 assert(ac_gpu_begin(64,64,0,0,255));
 assert(ac_gpu_theme_sprite(2,effect,0,1));ac_gpu_end(&fb);
 uint32_t pixel=pixels[8*64+8];
 assert(((pixel>>16)&255)>=127 && ((pixel>>16)&255)<=129);
 assert((pixel&255)>=126 && (pixel&255)<=128); // Half-red blended over blue.
 assert(ac_gpu_begin(64,64,0,0,0));
 assert(ac_gpu_theme_sprite(2,effect,0,1));ac_gpu_triangle(tri);ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xff0000ff); // VFX never occludes later geometry.
 effect[2]=effect[3]=1254;
 for(int depth_write=0;depth_write<=1;depth_write++) {
   assert(ac_gpu_begin(64,64,0,0,0));
   assert(ac_gpu_theme_sprite(3,effect,0,depth_write));
   ac_gpu_triangle(tri);ac_gpu_end(&fb);
   if(depth_write)assert((pixels[8*64+8]&0xffffff)==0x800000);
   else assert(pixels[8*64+8]==0xff0000ff);
 }
 // Faint atlas speckles must not occlude geometry when drawing solid props.
 unsigned char faint[]={255,0,0,8};
 glBindTexture(GL_TEXTURE_2D,ac_gpu.theme_textures[3]);
 glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,1,1,0,GL_RGBA,GL_UNSIGNED_BYTE,faint);
 assert(ac_gpu_begin(64,64,0,0,0));
 assert(ac_gpu_theme_sprite(3,effect,0,1));
 ac_gpu_triangle(tri);ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xff0000ff);
 // The same alpha remains visible when explicitly used for a soft flash.
 assert(ac_gpu_begin(64,64,0,0,0));
 assert(ac_gpu_theme_sprite(3,effect,0,0));ac_gpu_end(&fb);
 assert(((pixels[8*64+8]>>16)&255)==8);
 // Optional AIR sky is a retained opaque background with ordinary depth.
 fixture("/pieces/oskiewar-theme/sky-clouds-v1-1024x576.rgba",576,0);
 ac_gpu.theme_asset_retry_at[4]=0;
 assert(ac_gpu_theme_asset_ready(4));
 double sky[]={0,0,ac_theme_master_widths[4],ac_theme_master_heights[4],32,32,64,64,0,.8};
 assert(ac_gpu_begin(64,64,0,0,0));
 assert(ac_gpu_theme_sprite(4,sky,0,1));
 double behind[]={0,0,1,64,0,1,0,64,1,0,0,255};
 ac_gpu_triangle(behind);ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xffff0000); // Sky writes depth and hides later distant geometry.
 assert(ac_gpu_begin(64,64,0,0,0));
 assert(ac_gpu_theme_sprite(4,sky,0,1));ac_gpu_triangle(tri);ac_gpu_end(&fb);
 assert(pixels[8*64+8]==0xff0000ff); // Fighters remain in front of sky.
 assert(glGetError()==GL_NO_ERROR);
 puts("PASS theme orientation, retained uploads, optional assets, soft alpha, VFX/prop depth, bounds");
 return 0;
}
