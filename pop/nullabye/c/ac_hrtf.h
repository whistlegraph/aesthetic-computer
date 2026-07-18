// ac_hrtf.h — allocation-free procedural binaural spatializer for native C/WASM.
// Not a personalized measured HRTF: this models ITD, head shadow, and moving
// elevation-dependent pinna combs. State is per voice; API has no OS dependency.
#ifndef AC_HRTF_H
#define AC_HRTF_H
#include <math.h>
#include <string.h>
#define AC_HRTF_RING 256
#define AC_HRTF_PI 3.14159265358979323846
typedef struct { float ring[AC_HRTF_RING]; int at; float shadowL,shadowR; } ACHrtf;
static inline float ac_hrtf_read(const ACHrtf *h,double delay){
 double p=h->at-delay;while(p<0)p+=AC_HRTF_RING;int a=(int)p&(AC_HRTF_RING-1),b=(a+1)&(AC_HRTF_RING-1);double f=p-floor(p);return h->ring[a]*(1-f)+h->ring[b]*f;
}
static inline void ac_hrtf_process(ACHrtf *h,float input,double azimuth,double elevation,double distance,float *L,float *R){
 // Acoustic floor is deliberately tiny: proximity remains a compositional axis.
 double near=.004+.996/(1+.55*distance*distance),side=sin(azimuth);
 h->at=(h->at+1)&(AC_HRTF_RING-1);h->ring[h->at]=input*(float)near;
 // Woodworth-scale maximum ITD ≈ 650 μs at 48 kHz.
 double itd=fabs(side)*31.2,dl=side>0?itd:0,dr=side<0?itd:0;
 float l=ac_hrtf_read(h,dl),r=ac_hrtf_read(h,dr);
 // Pinna reflection delay moves with elevation: above is a short reflection,
 // below a longer one. The subtraction produces a direction-dependent notch;
 // a quieter second ridge avoids reducing elevation to one static comb.
 double en=fmax(-1,fmin(1,elevation/(AC_HRTF_PI*.5)));
 double tap1=15-en*8,tap2=31-en*10;
 l-=ac_hrtf_read(h,dl+tap1)*(float)(.25+.05*side);l+=ac_hrtf_read(h,dl+tap2)*.11f;
 r-=ac_hrtf_read(h,dr+tap1)*(float)(.25-.05*side);r+=ac_hrtf_read(h,dr+tap2)*.11f;
 // Far-ear head shadow: a moving one-pole lowpass mixed with the direct ear.
 double shadow=.18+.62*fabs(side),a=.72+.20*fabs(side);
 h->shadowL=(float)((1-a)*l+a*h->shadowL);h->shadowR=(float)((1-a)*r+a*h->shadowR);
 if(side>0)l=(float)(l*(1-shadow)+h->shadowL*shadow);else r=(float)(r*(1-shadow)+h->shadowR*shadow);
 // Mild elevation loudness compensation for the subtractive pinna taps.
 double eg=1+.08*fabs(en);*L=l*(float)eg;*R=r*(float)eg;
}
#endif
