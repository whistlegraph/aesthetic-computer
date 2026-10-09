// Benchmark host compiled to f64 Wasm. rayKernel is generated from the same
// KidLisp numeric plan as the scalar Wasm and WGSL kernels. No libc/imports.
#include "ray-kernel.h"
typedef struct { double x, y, z; } Vec;
typedef struct { int found; Vec position, normal, color; double mirror; } Hit;
static unsigned char pixels[512 * 512 * 4];
static int rays, intersections;
static double greenX;
static Vec v(double x, double y, double z) { return (Vec){x, y, z}; }
static Vec add(Vec a, Vec b) { return v(a.x+b.x,a.y+b.y,a.z+b.z); }
static Vec sub(Vec a, Vec b) { return v(a.x-b.x,a.y-b.y,a.z-b.z); }
static Vec mul(Vec a, double b) { return v(a.x*b,a.y*b,a.z*b); }
static Vec divv(Vec a, double b) { return v(a.x/b,a.y/b,a.z/b); }
static double dot(Vec a, Vec b) { return a.x*b.x+a.y*b.y+a.z*b.z; }
static Vec norm(Vec a) { return divv(a,__builtin_sqrt(dot(a,a))); }
static double max(double a, double b) { return a>b?a:b; }
static double min(double a, double b) { return a<b?a:b; }
static Vec sky(Vec d) { double t=max(0,min(1,0.5+d.y*0.5)); return add(v(0.12,0.17,0.25),mul(v(0.37,0.49,0.65),t)); }
static Hit intersect(Vec origin, Vec direction, double limit) {
  rays++;
  Vec centers[3]={v(-1.12,-0.12,2.3),v(1,-0.35,1.8),v(greenX,0.05,4.15)};
  Vec colors[3]={v(0.78,0.075,0.055),v(0.48,0.56,0.66),v(0.045,0.48,0.31)};
  double radii[3]={0.88,0.65,1.05}, mirrors[3]={0.30,0.82,0.20};
  double nearest=limit;
  Hit hit={0};
  for (int i=0;i<3;i++) {
    Vec r=sub(origin,centers[i]);
    intersections++;
    double discriminant=rayKernel(r.x,r.y,r.z,direction.x,direction.y,direction.z,radii[i]);
    if (discriminant<0) continue;
    double b=dot(r,direction),root=__builtin_sqrt(discriminant),t=-b-root;
    if (t<0.0001) t=-b+root;
    if (t>0.0001 && t<nearest) {
      nearest=t;
      Vec position=add(origin,mul(direction,t));
      hit=(Hit){1,position,divv(sub(position,centers[i]),radii[i]),colors[i],mirrors[i]};
    }
  }
  if (__builtin_fabs(direction.y)>1e-9) {
    double t=(-1-origin.y)/direction.y;
    if (t>0.0001 && t<nearest) {
      Vec position=add(origin,mul(direction,t));
      double parity=__builtin_floor(position.x)+__builtin_floor(position.z);
      // floor parity handles negative coordinates just like normalized JS %.
      int checker=parity-2*__builtin_floor(parity/2)!=0;
      hit=(Hit){1,position,v(0,1,0),checker?v(0.36,0.34,0.30):v(0.09,0.105,0.13),0.18};
    }
  }
  return hit;
}
static double pow80(double x) { double x2=x*x,x4=x2*x2,x8=x4*x4,x16=x8*x8,x32=x16*x16,x64=x32*x32; return x64*x16; }
static Vec trace(Vec direction, int bounces) {
  Vec origin=v(0,0.45,-4.2),locals[3],color;
  double mirrors[3]; int depth=0;
  for (int bounce=0;bounce<4;bounce++) {
    Hit hit=intersect(origin,direction,__builtin_inf());
    if (!hit.found) { color=sky(direction); break; }
    Vec toLight=sub(v(-3.5,5.5,-1.5),hit.position);
    double lightDistance=__builtin_sqrt(dot(toLight,toLight));
    Vec lightDirection=divv(toLight,lightDistance),offset=add(hit.position,mul(hit.normal,0.001));
    int shadowed=intersect(offset,lightDirection,lightDistance).found;
    double diffuse=max(0,dot(hit.normal,lightDirection))*(shadowed?0.08:0.95);
    Vec half=norm(sub(lightDirection,direction));
    double specular=shadowed?0:pow80(max(0,dot(hit.normal,half)))*0.8;
    color=add(mul(hit.color,0.16+diffuse),v(specular,specular,specular));
    if (bounce==bounces) break;
    locals[depth]=color;mirrors[depth++]=hit.mirror;
    direction=norm(sub(direction,mul(hit.normal,2*dot(direction,hit.normal))));
    origin=offset;
  }
  for (int i=depth-1;i>=0;i--) color=add(mul(locals[i],1-mirrors[i]),mul(color,mirrors[i]));
  return color;
}
static unsigned char channel(double x) { return (unsigned char)__builtin_floor(__builtin_sqrt(max(0,min(1,x)))*255+0.5); }
__attribute__((export_name("render")))
int render(int width,int height,int bounces,double x) {
  if (width<1||width>512||height<1||height>512||bounces<0||bounces>3||!__builtin_isfinite(x)||x<-0.23||x>0.33) return 0;
  rays=intersections=0;greenX=x;
  for (int y=0;y<height;y++) for (int x=0;x<width;x++) {
    Vec direction=norm(v(((x+0.5)/width*2-1)*width/height*0.52,(1-(y+0.5)/height*2)*0.52-0.09,1));
    Vec color=trace(direction,bounces);
    int offset=(x+y*width)*4;
    pixels[offset]=channel(color.x);pixels[offset+1]=channel(color.y);pixels[offset+2]=channel(color.z);pixels[offset+3]=255;
  }
  return (int)pixels;
}
__attribute__((export_name("rayCount"))) int rayCount(void) { return rays; }
__attribute__((export_name("intersectionCount"))) int intersectionCount(void) { return intersections; }
