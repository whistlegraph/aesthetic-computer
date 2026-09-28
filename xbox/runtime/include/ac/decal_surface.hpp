#pragma once
#include "decal_atlas.hpp"
#include <array>

namespace ac::xbox {
// A fixed raster canvas. Stamps are blended once; no stamp history is retained.
class DecalSurface {
 public:
  static constexpr unsigned side = 4096;
  std::vector<uint8_t> pixels = std::vector<uint8_t>(side * side * 4);
  unsigned left=0, top=0, right=side, bottom=side;
  bool dirty=true;
  void clear() {
    std::fill(pixels.begin(),pixels.end(),0);
    left=top=0;right=bottom=side;dirty=true;
  }
  void clean() { dirty=false;left=top=side;right=bottom=0; }
  // Source rectangle in the 256px atlas, then four destination pixel points.
  bool stamp(const std::array<float,12>& q, const std::array<float,3>* tint=nullptr) {
    if(tint)for(float v:*tint)if(!std::isfinite(v)||v<0||v>255)return false;
    for(float v:q)if(!std::isfinite(v)||std::abs(v)>32768)return false;
    if(q[0]<0||q[1]<0||q[2]<=0||q[3]<=0||q[0]+q[2]>256||q[1]+q[3]>256)return false;
    float loX=q[4],hiX=q[4],loY=q[5],hiY=q[5];
    for(int i=6;i<12;i+=2){loX=(std::min)(loX,q[i]);hiX=(std::max)(hiX,q[i]);loY=(std::min)(loY,q[i+1]);hiY=(std::max)(hiY,q[i+1]);}
    const int x0=(std::max)(0,int(std::floor(loX))),x1=(std::min)(int(side),int(std::ceil(hiX)));
    const int y0=(std::max)(0,int(std::floor(loY))),y1=(std::min)(int(side),int(std::ceil(hiY)));
    if(x0>=x1||y0>=y1)return true;
    // One hit per pixel, including the shared diagonal: no double-dark seam.
    const auto uv=[&](float x,float y,float& u,float& v){
      for(int half=0;half<2;half++){
        const int b=half?8:6,c=half?10:8;
        const float bx=q[b]-q[4],by=q[b+1]-q[5],cx=q[c]-q[4],cy=q[c+1]-q[5],den=bx*cy-by*cx;
        if(std::abs(den)<1e-8f)continue;
        const float s=((x-q[4])*cy-(y-q[5])*cx)/den,t=(bx*(y-q[5])-by*(x-q[4]))/den;
        if(s<0||t<0||s+t>1)continue;
        u=half?s:s+t;v=half?s+t:t;return true;
      }return false;
    };
    for(int y=y0;y<y1;y++)for(int x=x0;x<x1;x++){
      float u=0,v=0;if(!uv(x+.5f,y+.5f,u,v))continue;
      const unsigned sx=(std::min)(255u,unsigned(q[0]+u*q[2])),sy=(std::min)(255u,unsigned(q[1]+v*q[3]));
      const auto* src=atlas.data()+(sy*256+sx)*4;auto* dst=pixels.data()+(y*side+x)*4;
      const unsigned a=src[3],old=dst[3],out=a*255+old*(255-a);
      if(!a)continue;
      for(int k=0;k<3;k++)dst[k]=uint8_t(((tint?unsigned((*tint)[k]):src[k])*a*255+dst[k]*old*(255-a)+out/2)/out);
      dst[3]=uint8_t((out+127)/255);
    }
    left=(std::min)(left,unsigned(x0));top=(std::min)(top,unsigned(y0));
    right=(std::max)(right,unsigned(x1));bottom=(std::max)(bottom,unsigned(y1));dirty=true;
    return true;
  }
 private:
  const std::vector<uint8_t> atlas=make_decal_atlas();
};
} // namespace ac::xbox
