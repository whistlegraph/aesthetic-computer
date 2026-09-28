// Mac binding for the same fixed pool megatexture used by the Xbox host.
#include "../runtime/include/ac/decal_surface.hpp"
#include <array>
#include <vector>
#include <algorithm>

namespace {
struct Point { double x,y,z,u=0,v=0,q=1; };
struct Pool {
  ac::xbox::DecalSurface surface;
  std::vector<float> vertices, faces, projected;
};
Point mix(Point a,Point b,double t) {
  return {a.x+(b.x-a.x)*t,a.y+(b.y-a.y)*t,a.z+(b.z-a.z)*t,
    a.u+(b.u-a.u)*t,a.v+(b.v-a.v)*t,a.q+(b.q-a.q)*t};
}
template<class Distance> std::vector<Point> clip(const std::vector<Point>& in,Distance distance) {
  std::vector<Point> out;
  for(size_t i=0;i<in.size();i++) {
    auto a=in[i],b=in[(i+1)%in.size()];double da=distance(a),db=distance(b);
    if(da>=0)out.push_back(a);
    if((da>=0)!=(db>=0))out.push_back(mix(a,b,da/(da-db)));
  }
  return out;
}
}
extern "C" {
void* ac_pool_create() { return new Pool; }
void ac_pool_destroy(void* p) { delete static_cast<Pool*>(p); }
void ac_pool_clear(void* p) { static_cast<Pool*>(p)->surface.clear(); }
bool ac_pool_stamp(void* p,const float* values) {
  std::array<float,12> q;std::copy_n(values,12,q.begin());
  return static_cast<Pool*>(p)->surface.stamp(q);
}
bool ac_pool_tint(void* p,const float* values) {
  std::array<float,12> q;std::copy_n(values,12,q.begin());std::array<float,3> tint;std::copy_n(values+12,3,tint.begin());
  return static_cast<Pool*>(p)->surface.stamp(q,&tint);
}
// Return the dirty rectangle without consuming it; Swift copies it into each
// frame slot's pending bounds before acknowledging it with ac_pool_clean.
const uint8_t* ac_pool_pixels(void* p,int* bounds) {
  const auto& s=static_cast<Pool*>(p)->surface;
  if(!s.dirty)return nullptr;
  bounds[0]=s.left;bounds[1]=s.top;bounds[2]=s.right;bounds[3]=s.bottom;
  return s.pixels.data();
}
const uint8_t* ac_pool_all_pixels(void* p) { return static_cast<Pool*>(p)->surface.pixels.data(); }
void ac_pool_clean(void* p) { static_cast<Pool*>(p)->surface.clean(); }
bool ac_pool_mesh(void* p,const float* v,int nv,const float* f,int nf) {
  if(nv<=0||nv%3||nv>300000||nf<=0||nf%10||nf>100000)return false;
  for(int i=0;i<nv;i++)if(!std::isfinite(v[i]))return false;
  for(int i=0;i<nf;i++)if(!std::isfinite(f[i]))return false;
  for(int i=0;i<nf;i+=10)for(int j=0;j<4;j++)
    if(f[i+j]<0||f[i+j]>=nv/3||std::floor(f[i+j])!=f[i+j])return false;
  auto& s=*static_cast<Pool*>(p);s.vertices.assign(v,v+nv);s.faces.assign(f,f+nf);return true;
}
// Six floats per projected vertex: x, y, depth, u/depth, v/depth, 1/depth.
// Clip both position and texture coordinates, as the Xbox decalMesh does.
const float* ac_pool_draw(void* p,const float* m,const float* bounds,int* count) {
  *count=0;
  for(int i=0;i<27;i++)if(!std::isfinite(m[i]))return nullptr;
  for(int i=0;i<4;i++)if(!std::isfinite(bounds[i]))return nullptr;
  if(m[17]<=0||m[18]>=m[20]||m[19]>=m[21]||bounds[2]<=0||bounds[3]<=0)return nullptr;
  auto& s=*static_cast<Pool*>(p);s.projected.clear();std::vector<Point> view;
  for(size_t i=0;i<s.vertices.size();i+=3) {
    double x=s.vertices[i]-m[0],y=s.vertices[i+1]-m[1],z=s.vertices[i+2]-m[2];
    view.push_back({x*m[3]+y*m[4]+z*m[5],x*m[6]+y*m[7]+z*m[8],x*m[9]+y*m[10]+z*m[11],
      (s.vertices[i]-bounds[0])/bounds[2],(s.vertices[i+2]-bounds[1])/bounds[3]});
  }
  for(size_t i=0;i<s.faces.size();i+=10) {
    auto f=s.faces.data()+i;if(std::abs(f[8])<1e-8)continue;
    std::vector<Point> poly;for(int j=0;j<4;j++)poly.push_back(view[size_t(f[j])]);
    poly=clip(poly,[&](Point a){return a.z-m[17];});
    for(auto& a:poly) {
      double k=m[14]+(m[15]/a.z-m[14])*m[16],q=m[16]>.999?1/a.z:1;
      a={m[12]+a.x*k,m[13]-a.y*k,std::max(-1.499,std::clamp(double(m[22])+a.z*m[23],-1.499,1.4)-.0005),a.u*q,a.v*q,q};
    }
    poly=clip(poly,[&](Point a){return a.x-m[18];});
    poly=clip(poly,[&](Point a){return m[20]-a.x;});
    poly=clip(poly,[&](Point a){return a.y-m[19];});
    poly=clip(poly,[&](Point a){return m[21]-a.y;});
    for(size_t j=1;j+1<poly.size();j++)for(auto a:{poly[0],poly[j],poly[j+1]})
      for(double value:{a.x,a.y,a.z,a.u,a.v,a.q})s.projected.push_back(float(value));
  }
  *count=int(s.projected.size()/6);return s.projected.data();
}
}
