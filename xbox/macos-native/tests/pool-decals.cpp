#include "../PoolDecals.h"
#include <cassert>
#include <cmath>
#include <limits>
#include <vector>
#include <iostream>

int main() {
  void* pool=ac_pool_create();
  int dirty[4];const auto* pixels=ac_pool_pixels(pool,dirty);
  assert(pixels&&dirty[0]==0&&dirty[2]==2048);
  ac_pool_clean(pool);assert(!ac_pool_pixels(pool,dirty));
  float stamp[]={0,0,128,128,100,100,132,100,132,132,100,132};
  assert(ac_pool_stamp(pool,stamp));pixels=ac_pool_pixels(pool,dirty);
  assert(dirty[0]==100&&dirty[1]==100&&dirty[2]==132&&dirty[3]==132);
  unsigned alpha=0;for(int y=100;y<132;y++)for(int x=100;x<132;x++)alpha+=pixels[(y*2048+x)*4+3];
  assert(alpha>0);
  float neon[]={0,0,128,128,200,200,240,200,240,240,200,240,255,30,180};
  assert(ac_pool_tint(pool,neon));pixels=ac_pool_pixels(pool,dirty);bool found=false;
  for(int y=200;y<240;y++)for(int x=200;x<240;x++){const auto* p=pixels+(y*2048+x)*4;if(p[3]){assert(p[0]==255&&p[1]==30&&p[2]==180);found=true;}}
  assert(found);neon[12]=300;assert(!ac_pool_tint(pool,neon));
  const auto* originalPixels=ac_pool_all_pixels(pool);
  ac_pool_clean(pool);
  stamp[0]=250;assert(!ac_pool_stamp(pool,stamp));assert(!ac_pool_pixels(pool,dirty));stamp[0]=0;
  const float vertices[]={-10,0,20,10,0,20,10,0,40,-10,0,40};
  const float faces[]={0,1,2,3,255,255,255,0,-1,0};
  assert(ac_pool_mesh(pool,vertices,12,faces,10));
  float badFaces[]={0,1,2,99,255,255,255,0,-1,0};
  assert(!ac_pool_mesh(pool,vertices,12,badFaces,10));
  // A pitched camera with the rear edge beyond the near plane.
  float camera[]={0,-10,0,1,0,0,0,-1,0,0,0,1,100,100,1,100,1,25,0,0,200,200,-1.4f,.000175f,0,-1,0};
  const float bounds[]={-10,20,20,20};int count=0;
  const float* drawn=ac_pool_draw(pool,camera,bounds,&count);
  assert(drawn&&count>=3&&count%3==0);
  const std::vector<float> before(drawn,drawn+count*6);
  for(int i=0;i<count;i++) {
    const auto* v=drawn+i*6;for(int j=0;j<6;j++)assert(std::isfinite(v[j]));
    assert(v[0]>=0&&v[0]<=200&&v[1]>=0&&v[1]<=200);
    assert(v[5]>0&&v[5]<=1.f/25+1e-6);
    assert(v[3]/v[5]>=-1e-5&&v[3]/v[5]<=1.00001);
    assert(v[4]/v[5]>=-1e-5&&v[4]/v[5]<=1.00001);
  }
  // More marks change pixels, never mesh cost or the retained raster size.
  for(int i=0;i<10000;i++)assert(ac_pool_stamp(pool,stamp));
  assert(ac_pool_all_pixels(pool)==originalPixels);
  drawn=ac_pool_draw(pool,camera,bounds,&count);
  assert(std::vector<float>(drawn,drawn+count*6)==before);
  camera[17]=100;ac_pool_draw(pool,camera,bounds,&count);assert(count==0);
  camera[0]=std::numeric_limits<float>::quiet_NaN();assert(!ac_pool_draw(pool,camera,bounds,&count)&&count==0);
  ac_pool_clear(pool);pixels=ac_pool_all_pixels(pool);
  for(int y=100;y<132;y++)for(int x=100;x<132;x++)assert(pixels[(y*2048+x)*4+3]==0);
  ac_pool_destroy(pool);
  std::cout << "Pool megatexture: bounded storage, persistent stamps, clipped UVs, constant draw geometry passed\n";
}
