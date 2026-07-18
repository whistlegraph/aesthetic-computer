// spatial-sineabye.c — acoustic game-engine cut of sineabye.
// C owns score, oscillators, gravity simulation, stereo projection and pixels;
// ffmpeg is used only to encode/mux the C-generated WAV + PPM frame stream.
#define _POSIX_C_SOURCE 200809L
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ac_hrtf.h"
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2*M_PI)
#define SR 48000
#define BPM 76.0
#define BEAT (60.0/BPM)
#define BAR (4*BEAT)
#define DUR (38*BAR)
#define CTRL 60
#define FPS 24
#define W 720
#define H 720
#define MAXE 4096
#define NSRC 12
#define NFRAMES ((int)(DUR*FPS))

typedef struct { double x,y,z,mass; const char *name; uint32_t color; } Source;
typedef struct { double t,dur,f0,f1,g,atk,rel; int src,type; uint32_t seed; } Event;
typedef struct { double x,y,vx,vy,heading; } Listener;
static Source S[NSRC]={
 {-4.2,-2.8,1.0,1.3,"bass",0x4ecdc4},{.4,-1.2,.4,2.0,"boom",0xff6b6b},
 {-2.3,1.2,1.2,.8,"pad-l",0x63cdda},{4.8,2.4,1.5,.8,"pad-r",0x778beb},
 {-.8,2.1,2.2,1.2,"melody",0xf6c915},{5.8,-1.2,2.8,.55,"echo",0xf8a5c2},
 {-7.0,2.8,3,.35,"hat-l",0x59656a},{7.2,-3.5,3,.35,"hat-r",0x84979d},
 {-2.0,-.1,1.1,.5,"nose-l",0xa29bfe},{2.5,-4.8,1.1,.5,"nose-r",0x81ecec},
 {0,6.5,.7,2.3,"gong",0xe0a464},{-6.2,-5.2,1.6,.25,"air",0xb2bec3}
};
static Event E[MAXE]; static int NE=0;
static Listener *L; static float *busL,*busR,*fieldGain;
// Measured post-HRTF contribution of every source in every video frame.  The
// visualizer reads this telemetry instead of guessing loudness from envelopes.
static double *meterL,*meterR;
static double spatialWet=.32;
static const int TOUR[]={4,8,0,2,10,9,3,5,1,6,11,7};
static int tour_source(double t){return TOUR[((int)fmax(0,t/9.0))%12];}
static double hz(double m){return 440*pow(2,(m-69)/12);}
// Whole-room choreography: from 0:50–1:18 the source constellation eases
// through two complete rotations. Because every physics/audio lookup uses this
// function, the spin changes gravity, distance, Doppler and stereo—not just pixels.
static Source source_at(int s,double t){Source q=S[s];if(t>=50&&t<=78){double u=(t-50)/28;u=u*u*(3-2*u);double a=TAU*2*u;
 // Eight-turn centrifuge from 62–70 s.  Smoothstep gives it a physical ramp;
 // eight whole turns land at the original orientation with no visual/audio cut.
 if(t>=62){double x=fmin(1,(t-62)/8);x=x*x*(3-2*x);a+=TAU*8*x;}
 double x=q.x,y=q.y;q.x=x*cos(a)-y*sin(a);q.y=x*sin(a)+y*cos(a);q.z+=.22*sin(a+s*.7);}
 // Echo and air are memory-bodies: they occupy where the listener used to be.
 // During simulation these indices are always in the already-computed past.
 if(L&&(s==5||s==11)){double lag=s==5?2.2:5.5;int i=(int)(fmax(0,t-lag)*CTRL),max=(int)(DUR*CTRL);if(i>max)i=max;Listener p=L[i];double side=s==5?.55:-.8;q.x=p.x-cos(p.heading)*.45-sin(p.heading)*side;q.y=p.y-sin(p.heading)*.45+cos(p.heading)*side;q.z=s==5?2.35:1.85;}
 // Final movement: every voice peels away from the room and occupies a
 // progressively older point on the listener's path, forming an audible snake.
 if(L&&t>=82){double mix=fmin(1,(t-82)/10);mix=mix*mix*(3-2*mix);double lag=1.0+s*.72;int i=(int)(fmax(0,t-lag)*CTRL),max=(int)(DUR*CTRL);if(i>max)i=max;Listener p=L[i];double side=((s&1)?1:-1)*(.08+.018*s),tx=p.x-sin(p.heading)*side,ty=p.y+cos(p.heading)*side,tz=.55+fmod(s*1.37,2.5);q.x=q.x*(1-mix)+tx*mix;q.y=q.y*(1-mix)+ty*mix;q.z=q.z*(1-mix)+tz*mix;}
 return q;}
static void ev(double t,double d,double m,double g,int src,double a,double r){
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,hz(m),hz(m),g,a,r,src,0,(uint32_t)(n*2654435761u)};}
}
static void glide(double t,double d,double f0,double f1,double g,int src){
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,f0,f1,g,.003,d*.82,src,0,(uint32_t)(n*2654435761u)};}
}
static void noisev(double t,double d,double lo,double hi,double g,int src){
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,lo,hi,g,.006,d*.9,src,1,(uint32_t)(n*2654435761u)};}
}
static void gong(double t,double root,double g){
 double q[]={1,1.41,1.98,2.91,4.07},a[]={1,.52,.34,.2,.1},d[]={8.5,7.2,6.1,4.8,3.5};
 for(int i=0;i<5;i++)if(NE<MAXE){int n=NE++;E[n]=(Event){t,d[i],root*q[i],root*q[i],g*a[i],.018+i*.006,d[i]*.92,10,0,(uint32_t)(n*2654435761u)};}
}
static double env(const Event*e,double t){double u=t-e->t;if(u<0||u>=e->dur)return 0;double a=fmin(1,u/e->atk),r=fmin(1,(e->dur-u)/e->rel);return sin(a*M_PI/2)*sin(a*M_PI/2)*sin(r*M_PI/2)*sin(r*M_PI/2);}

static const int chord[4][4]={{48,55,64,74},{45,52,60,71},{53,60,69,67},{55,62,71,69}};
static const int root[4]={36,33,41,43};
static const int mel[4][4]={{76,79,81,81},{79,76,74,74},{72,74,76,79},{74,76,76,72}};
static void score(void){
 for(int b=0;b<38;b++){
  double t=b*BAR, tr=(b>=20&&b<28)?4.0/3:1;
  for(int k=0;k<4;k++)ev(t,BAR+.1,69+12*log2(hz(chord[b%4][k])*tr/440),.034,k<2?2:3,.7,1.0);
  // Continuous independently filtered noise voices.
  for(int q=0;q<4;q++)noisev(t+(q+.5)*BEAT,.085,4200,10200,.065,q&1?7:6);
  noisev(t,BAR,1700,5700,.013,11);
  for(int q=1;q<4;q+=2)noisev(t+q*BEAT,.15,620,2100,.052,q==1?8:9);
  if(b>=4&&b<35){ev(t,BAR,root[b%4],.055,0,.08,.5);glide(t,.34,66,40,.13,1);glide(t,.58,46,31,.105,1);glide(t+2*BEAT,.34,66,40,.12,1);glide(t+2*BEAT,.58,46,31,.095,1);}
  // Immediate melodic line; small rhythmic cells stay consonant in the bridge.
  for(int q=0;q<4;q++){double raw=mel[b%4][q]-12+12*log2(tr),m=fmin(69,raw);ev(t+q*BEAT,BEAT*.88,m,.075,4,.045,.34);if(b>=12&&b<32)ev(t+(q+.5)*BEAT,BEAT*.48,m+12,.014,5,.06,.28);}
  if(b>=12&&b<32)for(int q=0;q<4;q++)ev(t+(q+.5)*BEAT,.18,chord[b%4][q]+24+12*log2(tr),.017,q&1?5:4,.01,.15);
 }
 gong(12*BAR,65.41,.035);gong(20*BAR,87.31,.042);gong(28*BAR,65.41,.035);gong(34*BAR,65.41,.024);
 // Melody owns the final cadence.
 ev(36*BAR,BEAT*.9,60,.08,4,.05,.35);ev(36*BAR+BEAT,BEAT*.9,62,.075,4,.05,.35);
 ev(36*BAR+2*BEAT,BEAT*.9,64,.07,4,.05,.4);ev(36*BAR+3*BEAT,BEAT*1.8,60,.075,4,.06,1.2);
}

static void simulate(void){
 int n=(int)(DUR*CTRL)+1;L=calloc(n,sizeof(*L));L[0]=(Listener){0,-.5,0,0,0};double dt=1.0/CTRL;
 for(int i=1;i<n;i++){double t=i*dt,fx=0,fy=0,energy[NSRC]={0};
  for(int j=0;j<NE;j++){double a=env(&E[j],t);energy[E[j].src]+=a*E[j].g;}
  Listener p=L[i-1];
  for(int s=0;s<NSRC;s++){Source so=source_at(s,t);double dx=so.x-p.x,dy=so.y-p.y,r2=dx*dx+dy*dy+.8;
   double pull=(.38*S[s].mass*(.06+energy[s]*12))/r2;fx+=dx*pull;fy+=dy*pull;
   // Groove is tangential attraction: orbit energetic sources, don't collide.
   fx+=-dy*pull*.78*sin(TAU*t/(6*BEAT)+s);fy+=dx*pull*.78*sin(TAU*t/(6*BEAT)+s);
  }
  // Curiosity force: every nine seconds the listener travels toward a new
  // voice, close enough for proximity to reshape the mix before moving on.
  {int s=tour_source(t);Source so=source_at(s,t);double dx=so.x-p.x,dy=so.y-p.y,d=hypot(dx,dy)+.3;
   double arrive=.48/(1+.08*d*d);fx+=dx*arrive;fy+=dy*arrive;}
  // At 104 s turn around and accelerate through progressively older points in
  // the breadcrumb path—the listener physically crosses the trailing voices.
  if(t>=104&&t<118){double back=2+(t-104)*2.35,pt=fmax(0,t-back);Listener old=L[(int)(pt*CTRL)];double dx=old.x-p.x,dy=old.y-p.y;fx+=dx*.34;fy+=dy*.34;}
  // Two irrational-feeling wander periods prevent the listener settling into
  // one polite orbit. Weak room gravity eventually draws every excursion back.
  fx+=.27*sin(TAU*t/31)+.13*sin(TAU*t/11.7)-.018*p.x;
  fy+=.24*cos(TAU*t/23)+.11*cos(TAU*t/13.1)-.018*p.y;
  p.vx=(p.vx+fx*dt)*.995;p.vy=(p.vy+fy*dt)*.995;p.x+=p.vx*dt;p.y+=p.vy*dt;
  if(hypot(p.vx,p.vy)>.01)p.heading=atan2(p.vy,p.vx);L[i]=p;
 }
 // Mutual acoustic loading. Active neighboring fields damp one another; mass
 // makes boom/gong exert more pressure. This is control-rate and interpolated
 // by the sample renderer, so it remains deterministic and inexpensive.
 fieldGain=calloc(n*NSRC,sizeof(*fieldGain));for(int i=0;i<n;i++){double t=i/(double)CTRL,en[NSRC]={0};for(int j=0;j<NE;j++)en[E[j].src]+=env(&E[j],t)*E[j].g;
  for(int s=0;s<NSRC;s++){Source a=source_at(s,t);double load=0;for(int o=0;o<NSRC;o++)if(o!=s&&en[o]>0){Source b=source_at(o,t);double dx=a.x-b.x,dy=a.y-b.y,dz=a.z-b.z;load+=en[o]*S[o].mass/(1+dx*dx+dy*dy+.35*dz*dz);}fieldGain[i*NSRC+s]=(float)(.32+.68/(1+5.5*load));}}
}
static double field_gain(int src,double t){double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)return fieldGain[n*NSRC+src];double f=u-i;return fieldGain[i*NSRC+src]*(1-f)+fieldGain[(i+1)*NSRC+src]*f;}
static void gains(int src,double t,double *gl,double *gr,double *dist){
 double u=t*CTRL;int i=(int)u, n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;
 Listener a=L[i],b=L[i+1];double x=a.x+(b.x-a.x)*f,y=a.y+(b.y-a.y)*f,h=a.heading+(b.heading-a.heading)*f;
 Source so=source_at(src,t);double dx=so.x-x,dy=so.y-y,dz=so.z-1.6;*dist=sqrt(dx*dx+dy*dy+dz*dz);
 // Deliberately selective listening: beyond a few room units a source falls
 // away quickly, leaving only a 1.5% diffuse floor. Wandering now changes the
 // arrangement instead of merely nudging its stereo pan.
 double ang=atan2(dy,dx)-h,pan=sin(ang),near=.004+.996/(1+.55*(*dist)*(*dist));
 double wetL=sqrt((1-pan)*.5)*near,wetR=sqrt((1+pan)*.5)*near;
 // Dry reference = a restrained fixed studio panorama with no distance loss.
 // Interpolate gains before summing so --spatial-wet is a genuine mix control.
 double dryPan=fmax(-.62,fmin(.62,S[src].x/6.0)),dryL=sqrt((1-dryPan)*.5),dryR=sqrt((1+dryPan)*.5);
 *gl=dryL*(1-spatialWet)+wetL*spatialWet;*gr=dryR*(1-spatialWet)+wetR*spatialWet;
}
static void spatial_params(int src,double t,double *az,double *el,double *dist){
 double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;Listener a=L[i],b=L[i+1];
 double x=a.x+(b.x-a.x)*f,y=a.y+(b.y-a.y)*f,h=a.heading+(b.heading-a.heading)*f;Source so=source_at(src,t);
 double dx=so.x-x,dy=so.y-y,dz=so.z-1.6,horiz=hypot(dx,dy);*dist=hypot(horiz,dz);*az=atan2(dy,dx)-h;*el=atan2(dz,horiz);
}
static void propagation(int src,double t,double *wall,double *cutoff,double *wind,double *rain){double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;Listener a=L[i],b=L[i+1];double x=a.x+(b.x-a.x)*f,y=a.y+(b.y-a.y)*f;Source so=source_at(src,t);double dx=so.x-x,dy=so.y-y,d=hypot(dx,dy);*wall=1;*cutoff=20000;
 // Brick barrier at x=1.2, y=-3..2.5, height 2.4. Only an actual ray
 // intersection occludes; high sources and paths around its ends remain clear.
 if((x-1.2)*(so.x-1.2)<0){double q=(1.2-x)/(so.x-x),iy=y+(so.y-y)*q,iz=1.6+(so.z-1.6)*q;if(iy>-3&&iy<2.5&&iz<2.4){*wall=.46;*cutoff=1450;}}
 double wx=.7*cos(t*.11),wy=.7*sin(t*.083+.8);*wind=.0045*(wx*dx+wy*dy)/(d+1);*rain=1-.075*fmin(1,d/8.0)*fabs(sin(t*19.7+src*2.13));}
static void render(void){long n=(long)(DUR*SR);busL=calloc(n,4);busR=calloc(n,4);meterL=calloc(NFRAMES*NSRC,sizeof(*meterL));meterR=calloc(NFRAMES*NSRC,sizeof(*meterR));
 for(int j=0;j<NE;j++){Event*e=&E[j];long s0=(long)(e->t*SR),nn=(long)(e->dur*SR);double ph=0,lp=0,hpLP=0,envLP=0;uint32_t rs=e->seed;double lastD=0;ACHrtf hs;memset(&hs,0,sizeof hs);
  for(long k=0;k<nn&&s0+k<n;k++){double t=(s0+k)/(double)SR,u=k/(double)SR,a=env(e,t),az,el,d,wall,cutoff,wind,rain;spatial_params(e->src,t,&az,&el,&d);propagation(e->src,t,&wall,&cutoff,&wind,&rain);
   double v;if(e->type==0){double fullDop=(k?fmax(.985,fmin(1.015,1-(d-lastD)*SR/343)):1),dop=1+(fullDop-1)*spatialWet;double f=e->f0*pow(e->f1/e->f0,u/e->dur)*dop*(1+wind*spatialWet);ph+=TAU*f/SR;v=sin(ph);lastD=d;}
   else {rs=rs*1664525u+1013904223u;double w=((rs>>8)/8388608.0)-1;double ca=exp(-TAU*e->f0/SR),cb=exp(-TAU*e->f1/SR);lp=(1-ca)*w+ca*lp;double hi=w-lp;hpLP=(1-cb)*hi+cb*hpLP;v=hpLP;}
   if(cutoff<19000){double c=exp(-TAU*cutoff/SR);envLP=(1-c)*v+c*envLP;v=envLP;}v*=wall*rain*e->g*a*field_gain(e->src,t);float hl,hr;ac_hrtf_process(&hs,(float)v,az,el,d,&hl,&hr);
   double dryPan=fmax(-.62,fmin(.62,S[e->src].x/6.0)),dl=sqrt((1-dryPan)*.5),dr=sqrt((1+dryPan)*.5);
   double dg=cos(spatialWet*M_PI*.5),wg=sin(spatialWet*M_PI*.5),cl=v*dl*dg+hl*wg,cr=v*dr*dg+hr*wg;
   // Retarded reception: radio energy is written when it reaches the listener,
   // not when the emitter produced it. Distance therefore becomes real delay.
   long at=s0+k+(long)(d*SR/343.0);if(at<n){busL[at]+=(float)cl;busR[at]+=(float)cr;
    // Per-emitter air/room tail; unlike the later master reflections this delay
    // remains attached to the originating voice and its stereo observation.
    long echo=at+(long)((.075+.011*e->src)*SR);if(echo<n){busL[echo]+=(float)(cl*.055);busR[echo]+=(float)(cr*.055);}
    int fr=(int)(at*FPS/(double)SR);if(fr>=0&&fr<NFRAMES){int mi=fr*NSRC+e->src;meterL[mi]+=cl*cl;meterR[mi]+=cr*cr;}}
  }
 }
 // Cross-room early reflections.
 int ds[]={3408,5424,8688};double dg[]={.075,.048,.03};for(int q=0;q<3;q++)for(long i=ds[q];i<n;i++){float l=busL[i-ds[q]],r=busR[i-ds[q]];double room=.22+.78*spatialWet;busL[i]+=r*dg[q]*room;busR[i]+=l*dg[q]*room;}
 double pk=0;for(long i=0;i<n;i++){double a=fmax(fabs(busL[i]),fabs(busR[i]));if(a>pk)pk=a;}double g=pk?.88/pk:1;
 for(long i=0;i<n;i++){double fo=i>n-SR?((n-i)/(double)SR):1;busL[i]*=g*fo;busR[i]*=g*fo;}
}
static int wav(const char*p){FILE*f=fopen(p,"wb");if(!f)return 0;long n=(long)(DUR*SR);uint32_t ds=n*8,sz=36+ds,sr=SR,br=SR*8,fs=16;uint16_t fm=3,ch=2,ba=8,bi=32;
 fwrite("RIFF",1,4,f);fwrite(&sz,4,1,f);fwrite("WAVEfmt ",1,8,f);fwrite(&fs,4,1,f);fwrite(&fm,2,1,f);fwrite(&ch,2,1,f);fwrite(&sr,4,1,f);fwrite(&br,4,1,f);fwrite(&ba,2,1,f);fwrite(&bi,2,1,f);fwrite("data",1,4,f);fwrite(&ds,4,1,f);for(long i=0;i<n;i++){fwrite(&busL[i],4,1,f);fwrite(&busR[i],4,1,f);}fclose(f);return 1;}
static void dot(unsigned char*p,int x,int y,int r,uint32_t c){for(int yy=-r;yy<=r;yy++)for(int xx=-r;xx<=r;xx++)if(xx*xx+yy*yy<=r*r){int X=x+xx,Y=y+yy;if(X>=0&&X<W&&Y>=0&&Y<H){int o=(Y*W+X)*3;p[o]=c>>16;p[o+1]=c>>8;p[o+2]=c;}}}
static void glow(unsigned char*p,int x,int y,int r,uint32_t c,double strength){int rr=c>>16,gg=(c>>8)&255,bb=c&255;for(int yy=-r;yy<=r;yy++)for(int xx=-r;xx<=r;xx++){double d=sqrt(xx*xx+yy*yy)/(double)r;if(d>1)continue;int X=x+xx,Y=y+yy;if(X<0||X>=W||Y<0||Y>=H)continue;double a=strength*(1-d)*(1-d);int o=(Y*W+X)*3;p[o]=p[o]*(1-a)+rr*a;p[o+1]=p[o+1]*(1-a)+gg*a;p[o+2]=p[o+2]*(1-a)+bb*a;}}
typedef struct{double x,y,z;} V3;typedef struct{int x,y;double z;int ok;} P2;
static V3 sub3(V3 a,V3 b){return(V3){a.x-b.x,a.y-b.y,a.z-b.z};}static double dot3(V3 a,V3 b){return a.x*b.x+a.y*b.y+a.z*b.z;}
static V3 cross3(V3 a,V3 b){return(V3){a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x};}
static V3 norm3(V3 a){double n=sqrt(dot3(a,a));return n?(V3){a.x/n,a.y/n,a.z/n}:a;}
static P2 project(V3 p,V3 cam,V3 target){V3 f=norm3(sub3(target,cam)),r=norm3(cross3(f,(V3){0,0,1})),u=cross3(r,f),q=sub3(p,cam);double z=dot3(q,f);if(z<.15)return(P2){0,0,z,0};double focal=W*.694;return(P2){(int)(W*.5+dot3(q,r)*focal/z),(int)(H*.54-dot3(q,u)*focal/z),z,1};}
static void line2(unsigned char*p,int x0,int y0,int x1,int y1,uint32_t c,double alpha){int dx=abs(x1-x0),sx=x0<x1?1:-1,dy=-abs(y1-y0),sy=y0<y1?1:-1,er=dx+dy;for(;;){if(x0>=0&&x0<W&&y0>=0&&y0<H){int o=(y0*W+x0)*3;int rr=c>>16,gg=(c>>8)&255,bb=c&255;p[o]=p[o]*(1-alpha)+rr*alpha;p[o+1]=p[o+1]*(1-alpha)+gg*alpha;p[o+2]=p[o+2]*(1-alpha)+bb*alpha;}if(x0==x1&&y0==y1)break;int e2=2*er;if(e2>=dy){er+=dy;x0+=sx;}if(e2<=dx){er+=dx;y0+=sy;}}}
static void fill2(unsigned char*p,int x0,int y0,int x1,int y1,uint32_t c,double a){int rr=c>>16,gg=(c>>8)&255,bb=c&255;if(x0<0)x0=0;if(y0<0)y0=0;if(x1>W)x1=W;if(y1>H)y1=H;for(int y=y0;y<y1;y++)for(int x=x0;x<x1;x++){int o=(y*W+x)*3;p[o]=p[o]*(1-a)+rr*a;p[o+1]=p[o+1]*(1-a)+gg*a;p[o+2]=p[o+2]*(1-a)+bb*a;}}
static void ellipse2(unsigned char*p,int cx,int cy,int rx,int ry,uint32_t c,double a){int x0=cx+rx,y0=cy;for(int i=1;i<=96;i++){double q=TAU*i/96;int x1=cx+(int)(cos(q)*rx),y1=cy+(int)(sin(q)*ry);line2(p,x0,y0,x1,y1,c,a);x0=x1;y0=y1;}}
static void line3(unsigned char*p,V3 a,V3 b,V3 cam,V3 target,uint32_t c,double alpha){P2 x=project(a,cam,target),y=project(b,cam,target);if(x.ok&&y.ok)line2(p,x.x,x.y,y.x,y.y,c,alpha);}
static void circle3(unsigned char*p,V3 c,double r,V3 cam,V3 target,uint32_t color,double alpha){V3 a={c.x+r,c.y,c.z};for(int i=1;i<=64;i++){double q=TAU*i/64;V3 b={c.x+cos(q)*r,c.y+sin(q)*r,c.z};line3(p,a,b,cam,target,color,alpha);a=b;}}
static void marble(unsigned char*p,int cx,int cy,int r,uint32_t c,double energy){
 int cr=c>>16,cg=(c>>8)&255,cb=c&255;for(int y=-r;y<=r;y++)for(int x=-r;x<=r;x++){double nx=x/(double)r,ny=y/(double)r,r2=nx*nx+ny*ny;if(r2>1)continue;double nz=sqrt(1-r2),lx=-.42,ly=-.55,lz=.72,nd=fmax(0,nx*lx+ny*ly+nz*lz),rim=pow(1-nz,2.2),hx=nx+.26,hy=ny+.34,hl=hypot(hx,hy),spec=pow(fmax(0,1-hl*2.4),18);double shade=.18+.66*nd+.34*rim+1.15*spec+fmin(.45,energy*10);int X=cx+x,Y=cy+y;if(X<0||X>=W||Y<0||Y>=H)continue;int o=(Y*W+X)*3;p[o]=(unsigned char)fmin(255,cr*shade+spec*110);p[o+1]=(unsigned char)fmin(255,cg*shade+spec*110);p[o+2]=(unsigned char)fmin(255,cb*shade+spec*110);}}
static void source_meter(int fr,int src,double *l,double *r){
 // A short symmetric window removes 24 fps sparkle without inventing energy.
 double sl=0,sr=0;int count=0;for(int q=-2;q<=2;q++){int f=fr+q;if(f<0||f>=NFRAMES)continue;double w=q?1.0/(1+abs(q)):1.0;sl+=meterL[f*NSRC+src]*w;sr+=meterR[f*NSRC+src]*w;count+=(int)(SR/(double)FPS*w);}
 *l=count?sqrt(sl/count):0;*r=count?sqrt(sr/count):0;
}
static void stereo_lobes(unsigned char*p,P2 sp,double l,double r,int radius,uint32_t c){
 // The two ears are visible: left/right lobe areas are the measured channel RMS.
 double peak=fmax(l,r),scale=peak>0?1.0/peak:0;int gap=radius+2;
 int rl=(int)fmax(1,radius*(.2+.8*sqrt(l*scale))),rr=(int)fmax(1,radius*(.2+.8*sqrt(r*scale)));
 glow(p,sp.x-gap,sp.y,rl*2,c,.12+fmin(.55,l*18));glow(p,sp.x+gap,sp.y,rr*2,c,.12+fmin(.55,r*18));
 dot(p,sp.x-gap,sp.y,rl,c);dot(p,sp.x+gap,sp.y,rr,c);
}
static void video(const char*wavp,const char*outp){char cmd[2048];size_t ol=strlen(outp);int lossless=ol>=4&&!strcmp(outp+ol-4,".mov");if(lossless)
 snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f image2pipe -vcodec ppm -r %d -i - -i '%s' -c:v libx265 -preset medium -x265-params lossless=1:log-level=error -pix_fmt yuv444p -tag:v hvc1 -c:a pcm_s24le -ar 48000 -shortest '%s'",FPS,wavp,outp);
 else snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f image2pipe -vcodec ppm -r %d -i - -i '%s' -c:v libx264 -pix_fmt yuv420p -crf 17 -c:a aac -b:a 320k -shortest '%s'",FPS,wavp,outp);FILE*ff=popen(cmd,"w");if(!ff)return;unsigned char*p=malloc(W*H*3);
 for(int fr=0;fr<(int)(DUR*FPS);fr++){double t=fr/(double)FPS;for(int y=0;y<H;y++){unsigned char v=(unsigned char)(24-10*y/(double)H);for(int x=0;x<W;x++){int o=(y*W+x)*3;p[o]=v;p[o+1]=(unsigned char)fmax(0,v-1);p[o+2]=(unsigned char)fmin(255,v+2);}}int li=(int)(t*CTRL);Listener l=L[li];
  // Camera begins among the sources, orbits with the listener, then after 78 s
  // retreats far beyond the room until the ensemble is a small constellation.
  int ts=tour_source(t),tn=TOUR[(ts==TOUR[11])?0:((int)(t/9.0)+1)%12];double phase=fmod(t,9.0)/9.0,lookMix=phase<.78?0:(phase-.78)/.22;lookMix=lookMix*lookMix*(3-2*lookMix);Source la=source_at(ts,t),lb=source_at(tn,t);
  // First-person radio receiver at ear height. Smooth focus handoffs avoid cuts;
  // parallax and changing radii now reveal the listener's physical travel.
  V3 cam={l.x,l.y,1.62},target={la.x+(lb.x-la.x)*lookMix,la.y+(lb.y-la.y)*lookMix,la.z+(lb.z-la.z)*lookMix};
  // Perspective floor grid makes depth and the final pull-away legible.
  for(int q=-8;q<=8;q++){line3(p,(V3){q,-8,0},(V3){q,8,0},cam,target,0x536069,.28);line3(p,(V3){-8,q,0},(V3){8,q,0},cam,target,0x536069,.28);}
  // The same propagation world the radio hears: brick occluder and wind-raked
  // rain streaks. They are geometry/fields, never decorative AI additions.
  for(int q=0;q<=5;q++)line3(p,(V3){1.2,-3,q*.48},(V3){1.2,2.5,q*.48},cam,target,0x806f68,.62);
  for(int q=0;q<=6;q++)line3(p,(V3){1.2,-3+q*.92,0},(V3){1.2,-3+q*.92,2.4},cam,target,0x806f68,.62);
  for(int q=0;q<42;q++){double rx=-8+fmod(q*3.71+t*.7,16),ry=-8+fmod(q*6.13+t*.43,16),rz=.3+fmod(q*1.77-t*1.9,5.5);line3(p,(V3){rx,ry,rz},(V3){rx-.12,ry-.08,rz-.38},cam,target,0x8faab5,.18);}
  V3 lv={l.x,l.y,1.6};
  // Six-second fading breadcrumb: the path that the two follower voices chase.
  for(int q=1;q<=36;q++){int ia=li-(q-1)*CTRL/6,ib=li-q*CTRL/6;if(ib<0)break;V3 a={L[ia].x,L[ia].y,1.58},b={L[ib].x,L[ib].y,1.58};line3(p,a,b,cam,target,0xff6b9d,.42*(1-q/37.0));}
  for(int s=0;s<NSRC;s++){double ml,mr;source_meter(fr,s,&ml,&mr);double heard=hypot(ml,mr);Source so=source_at(s,t);V3 sv={so.x,so.y,so.z};P2 sp=project(sv,cam,target);double gl,gr,dd;gains(s,t,&gl,&gr,&dd);
   // Grey is physical distance. Color and lobe area are measured post-HRTF
   // energy arriving at the listener's two ears in this exact video frame.
   line3(p,lv,sv,cam,target,0x778087,.18);circle3(p,(V3){so.x,so.y,.025},1.35,cam,target,S[s].color,s==ts?.34:.08);if(s==ts&&t<104)circle3(p,(V3){so.x,so.y,.03},4.05,cam,target,S[s].color,.2);
   // Phosphor streak = true recent emitter trajectory. Distant transmissions
   // persist longer, making their longer radio flight time visible.
   {double trail=.18+fmin(.9,dd/12);V3 prev=sv;for(int q=1;q<=16;q++){double pt=fmax(0,t-q*trail/16),fade=(1-q/17.0)*(.08+fmin(.55,heard*18));Source old=source_at(s,pt);V3 ov={old.x,old.y,old.z};line3(p,prev,ov,cam,target,S[s].color,fade);prev=ov;}}
   if(heard>.00005)line3(p,lv,sv,cam,target,S[s].color,.03+fmin(.84,heard*28));
   if(sp.ok){double sc=W/360.0;int core=(int)fmax(3,fmin(11,52*sc/sp.z));int halo=(int)fmax(core*2,fmin(62,(4+heard*1500)*sc*10/sp.z));P2 sh=project((V3){so.x,so.y,.02},cam,target);if(sh.ok)glow(p,sh.x,sh.y,(int)fmax(5,halo*.8),S[s].color,.06+fmin(.4,heard*14));if(s==ts&&t<104)glow(p,sp.x,sp.y,halo+18,0xffffff,.16);glow(p,sp.x,sp.y,halo,S[s].color,.18+fmin(.7,heard*16));stereo_lobes(p,sp,ml,mr,core,S[s].color);marble(p,sp.x,sp.y,core+2,S[s].color,heard);}
  }
  P2 lp=project(lv,cam,target),hd=project((V3){l.x+cos(l.heading)*.55,l.y+sin(l.heading)*.55,1.6},cam,target);
  if(lp.ok){int rr=(int)fmax(4,fmin(20,130/lp.z));glow(p,lp.x,lp.y,rr*3,0xf6c915,.35);dot(p,lp.x,lp.y,rr,0xffd54f);if(hd.ok)line2(p,lp.x,lp.y,hd.x,hd.y,0xff6b9d,.95);}
  // Receiver cockpit. The canopy frames the view while the lower scanner maps
  // contacts around the listener; vertical stalks encode above/below exactly.
  fill2(p,0,H-205,W,H,0x080b0d,.72);for(int w=0;w<4;w++){line2(p,35+w,H,178+w,105,0x38454b,.88);line2(p,W-35-w,H,W-178-w,105,0x38454b,.88);line2(p,178+w,105,W-178-w,105,0x38454b,.7);}
  line2(p,W/2-18,H/2,W/2-5,H/2,0xb8c9cc,.65);line2(p,W/2+5,H/2,W/2+18,H/2,0xb8c9cc,.65);line2(p,W/2,H/2-18,W/2,H/2-5,0xb8c9cc,.65);line2(p,W/2,H/2+5,W/2,H/2+18,0xb8c9cc,.65);
  int rcx=W/2,rcy=H-92;ellipse2(p,rcx,rcy,122,62,0x5b6f73,.8);ellipse2(p,rcx,rcy,82,41,0x405256,.65);ellipse2(p,rcx,rcy,41,20,0x405256,.5);line2(p,rcx-122,rcy,rcx+122,rcy,0x405256,.45);line2(p,rcx,rcy-62,rcx,rcy+62,0x405256,.45);
  // Receiver triangle points forward. Contact brightness is measured signal,
  // weak transmissions flicker unresolved instead of being presented as fact.
  line2(p,rcx,rcy-7,rcx-6,rcy+6,0xf6c915,.95);line2(p,rcx-6,rcy+6,rcx+6,rcy+6,0xf6c915,.95);line2(p,rcx+6,rcy+6,rcx,rcy-7,0xf6c915,.95);
  double sumL=0,sumR=0;for(int s=0;s<NSRC;s++){double ml,mr;source_meter(fr,s,&ml,&mr);sumL+=ml;sumR+=mr;Source so=source_at(s,t);double dx=so.x-l.x,dy=so.y-l.y,rx=dx*cos(l.heading)+dy*sin(l.heading),ry=-dx*sin(l.heading)+dy*cos(l.heading),scale=11.0,rr=hypot(rx,ry);if(rr>10.5){rx*=10.5/rr;ry*=10.5/rr;}int bx=rcx+(int)(ry*scale),base=rcy-(int)(rx*scale*.5),by=base-(int)((so.z-1.6)*10),weak=hypot(ml,mr)<.0015;double vis=weak?(.16+.24*(sin(t*31+s*7)>0)):.9;line2(p,bx,base,bx,by,S[s].color,vis*.7);if(s==ts)glow(p,bx,by,12,0xffffff,.28);dot(p,bx,by,s==ts?4:3,S[s].color);}
  int lm=(int)fmin(110,sumL*850),rm=(int)fmin(110,sumR*850);fill2(p,70,H-55,70+lm,H-45,0x4ecdc4,.9);fill2(p,W-70-rm,H-55,W-70,H-45,0xf8a5c2,.9);
  fprintf(ff,"P6\n%d %d\n255\n",W,H);fwrite(p,1,W*H*3,ff);
 }
 free(p);pclose(ff);
}
int main(int argc,char**argv){const char*w="../out/spatial-sineabye.wav",*v="../out/spatial-sineabye.mp4",*mp3=NULL;for(int i=1;i<argc;i++){if(!strcmp(argv[i],"--wav")&&i+1<argc)w=argv[++i];else if(!strcmp(argv[i],"--video")&&i+1<argc)v=argv[++i];else if(!strcmp(argv[i],"--mp3")&&i+1<argc)mp3=argv[++i];else if(!strcmp(argv[i],"--spatial-wet")&&i+1<argc)spatialWet=fmax(0,fmin(1,atof(argv[++i])));}
 score();simulate();render();if(!wav(w)){fprintf(stderr,"write failed\n");return 1;}fprintf(stderr,"✓ %s · %.1fs · %d events · gravitational listener · spatial wet %.0f%%\n",w,DUR,NE,spatialWet*100);
 if(mp3){char cmd[4096];snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -i '%s' -af 'highpass=f=28,equalizer=f=72:t=q:w=.8:g=1.2,equalizer=f=7200:t=q:w=.9:g=-1,lowpass=f=15800,alimiter=limit=.90:attack=6:release=100,volume=.84' -c:a libmp3lame -q:a 2 '%s'",w,mp3);if(system(cmd)!=0)return 1;}
 if(v&&strcmp(v,"none"))video(w,v);return 0;}
