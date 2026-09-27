#include "../src/screen-triangle.h"
#include <assert.h>
#include <stdio.h>

int main(void) {
  uint32_t pixels[64]; float depth[64];
  for(int i=0;i<64;i++){pixels[i]=0;depth[i]=INFINITY;}
  ac_screen_triangle(pixels,depth,8,8,8,0,0,.5,8,0,.5,0,8,.5,0xff123456);
  assert(pixels[1*8+1]==0xff123456);
  // Later coplanar geometry replaces the color without speckling.
  ac_screen_triangle(pixels,depth,8,8,8,0,8,.5,8,0,.5,0,0,.5,0xffabcdef);
  for(int y=0;y<7;y++)for(int x=0;x<7-y;x++)assert(pixels[y*8+x]==0xffabcdef);
  // A farther face cannot overwrite it; a nearer one can.
  ac_screen_triangle(pixels,depth,8,8,8,0,0,.6,8,0,.6,0,8,.6,0xff000000);
  assert(pixels[9]==0xffabcdef);
  ac_screen_triangle(pixels,depth,8,8,8,0,0,.4,8,0,.4,0,8,.4,0xff112233);
  assert(pixels[9]==0xff112233);
  // Off-screen, degenerate and invalid geometry is safe.
  ac_screen_triangle(pixels,depth,8,8,8,-100,-100,0,-20,-100,0,-100,-20,0,0);
  ac_screen_triangle(pixels,depth,8,8,8,0,0,0,0,0,0,0,0,0,0);
  ac_screen_triangle(pixels,depth,8,8,8,NAN,0,0,8,0,0,0,8,0,0);
  assert(pixels[9]==0xff112233);
  puts("screen triangles: color, winding, depth, clipping, invalid geometry pass");
}
