#ifndef AC_RAYCAST_H
#define AC_RAYCAST_H
#include <math.h>
#include <stdint.h>
#include <stddef.h>
// Packed ellipsoids: center xyz, radii xyz, RGB, rock, glow, detail, bounds LRTB.
// Uniforms: width,height,cx,cy,f,cos,sin, light xyz, right xyz, up xyz,
// forward xyz, coarse cell, detail cell. Depth is full-resolution view-space Z.
static inline double ac_ray_clamp(double x,double lo,double hi){return fmax(lo,fmin(hi,x));}
static inline void ac_raycast(uint32_t *pixels,int stride,float *depth,
                             const double *shapes,int count,const double *u){
 const int w=(int)u[0],h=(int)u[1];
 for(size_t i=0;i<(size_t)w*h;i++)depth[i]=INFINITY;
 for(int n=0;n<count;n++){
  const double *o=shapes+n*16;
  const int cell=(int)(o[11]?u[20]:u[19]);
  const double ix=1/(o[3]*o[3]),iy=1/(o[4]*o[4]),iz=1/(o[5]*o[5]);
  const double c=o[0]*o[0]*ix+o[1]*o[1]*iy+o[2]*o[2]*iz-1;
  const int left=(int)floor(ac_ray_clamp(o[12],0,w)/cell)*cell;
  const int right=(int)ceil(ac_ray_clamp(o[13],0,w)/cell)*cell;
  const int top=(int)floor(ac_ray_clamp(o[14],0,h)/cell)*cell;
  const int bottom=(int)ceil(ac_ray_clamp(o[15],0,h)/cell)*cell;
  for(int y=top;y<bottom&&y<h;y+=cell){
   const double v=(y+cell*.5-u[3])/u[4],a0=(left+cell*.5-u[2])/u[4];
   double dx=a0*u[5]+v*u[6],dy=a0*u[6]-v*u[5];
   for(int x=left;x<right&&x<w;x+=cell,dx+=cell/u[4]*u[5],dy+=cell/u[4]*u[6]){
    const double a=dx*dx*ix+dy*dy*iy+iz,q=-(o[0]*dx*ix+o[1]*dy*iy+o[2]*iz);
    const double disc=q*q-a*c;if(disc<0)continue;
    const double root=sqrt(disc);double t=(-q-root)/a;if(t<=1)t=(-q+root)/a;
    if(t<=1||!isfinite(t))continue;
    // An already-nearer full block can be skipped, but partial coverage must
    // be depth-tested per destination pixel (fine ships overlap coarse rocks).
    int visible=0;
    for(int yy=y;yy<y+cell&&yy<h&&!visible;yy++)for(int xx=x;xx<x+cell&&xx<w;xx++)if(t<depth[(size_t)yy*w+xx]){visible=1;break;}
    if(!visible)continue;
    double nx=(t*dx-o[0])*ix,ny=(t*dy-o[1])*iy,nz=(t-o[2])*iz;
    const double norm=1/sqrt(nx*nx+ny*ny+nz*nz);nx*=norm;ny*=norm;nz*=norm;
    double shade=o[10]?1:.18+.82*fmax(0,nx*u[7]+ny*u[8]+nz*u[9]);
    if(o[9]){
     const double wx=nx*u[10]+ny*u[13]+nz*u[16],wy=nx*u[11]+ny*u[14]+nz*u[17],wz=nx*u[12]+ny*u[15]+nz*u[18];
     const uint32_t grain=(uint32_t)(int32_t)floor(wx*13)*73856093u ^ (uint32_t)(int32_t)floor(wy*13)*19349663u ^ (uint32_t)(int32_t)floor(wz*13)*83492791u;
     shade*=grain%11<2?.48:.78+(grain%5)*.07;
    }
    shade=round(shade*32)/32;const double fog=ac_ray_clamp(1-t/1700,.3,1);
    const unsigned r=(unsigned)ac_ray_clamp(round(o[6]*shade*fog+6*(1-fog)),0,255);
    const unsigned g=(unsigned)ac_ray_clamp(round(o[7]*shade*fog+13*(1-fog)),0,255);
    const unsigned b=(unsigned)ac_ray_clamp(round(o[8]*shade*fog+28*(1-fog)),0,255);
    const uint32_t color=0xff000000u|(r<<16)|(g<<8)|b;
    for(int yy=y;yy<y+cell&&yy<h;yy++)for(int xx=x;xx<x+cell&&xx<w;xx++){
     const size_t i=(size_t)yy*w+xx;if(t<depth[i]){depth[i]=(float)t;pixels[(size_t)yy*stride+xx]=color;}
    }
   }
  }
 }
}
#endif
