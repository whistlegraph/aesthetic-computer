#include "../src/raycast.h"
#include <assert.h>
#include <stdio.h>
int main(void){
 enum{W=41,H=31,STRIDE=45};uint32_t pixels[STRIDE*H];float depth[W*H];
 for(int i=0;i<STRIDE*H;i++)pixels[i]=0x12345678;
 double u[21]={W,H,20.5,15.5,35,1,0,0,0,-1,1,0,0,0,1,0,0,0,1,2,1};
 double shapes[32]={0,0,10,2,2,2,255,0,0,0,0,1,0,W,0,H, 0,0,20,4,4,4,0,255,0,0,0,0,0,W,0,H};
 ac_raycast(pixels,STRIDE,depth,shapes,2,u);
 assert(fabs(depth[15*W+20]-8)<.00001);assert((pixels[15*STRIDE+20]&0x00ff0000)>0);assert((pixels[15*STRIDE+20]&0x0000ff00)==0);
 assert(isinf(depth[0]));assert(pixels[0]==0x12345678);
 for(int y=0;y<H;y++)for(int x=W;x<STRIDE;x++)assert(pixels[y*STRIDE+x]==0x12345678);
 // Camera inside a sphere takes the positive exit intersection.
 shapes[2]=0;shapes[3]=shapes[4]=shapes[5]=3;
 ac_raycast(pixels,STRIDE,depth,shapes,1,u);assert(fabs(depth[15*W+20]-3)<.00001);
 // Clipped odd-sized coarse blocks cannot touch framebuffer padding.
 shapes[2]=10;shapes[3]=shapes[4]=shapes[5]=100;shapes[11]=0;
 shapes[12]=-100;shapes[13]=1000;shapes[14]=-100;shapes[15]=1000;
 ac_raycast(pixels,STRIDE,depth,shapes,1,u);
 for(int y=0;y<H;y++)for(int x=W;x<STRIDE;x++)assert(pixels[y*STRIDE+x]==0x12345678);
 puts("PASS: native ray hits, occlusion, inside sphere, viewport clipping, stride padding");
}
