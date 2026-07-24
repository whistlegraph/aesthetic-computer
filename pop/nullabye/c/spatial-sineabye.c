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
#include "ac_prompt_rock.h"
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#include "ac_mesh_acoustics.h"
#define TAU (2*M_PI)
#define SR 48000
#define ROOM_BPM 76.0
#define COSMOS_BPM 104.0
static double tempoBpm=ROOM_BPM;
#define BPM (tempoBpm)
#define BEAT (60.0/BPM)
#define BAR (4*BEAT)
#define DUR (40*BAR)
// Physics/control substeps are independent of the 24 fps picture. 240 Hz
// keeps listener forces, loading, and recursive antenna motion below audible
// control-rate stepping before their states are interpolated at 48 kHz.
#define CTRL 240
#define FPS 24
#define W 720
#define H 720
#define MAXE 4096
#define NSRC 12
#define NFRAMES ((int)(DUR*FPS))
#include "ac_terrarium_pt.h"

typedef struct { double x,y,z,mass; const char *name; uint32_t color; } Source;
typedef struct { double t,dur,f0,f1,g,atk,rel; int src,type; uint32_t seed; } Event;
typedef struct { double x,y,vx,vy,heading,z,vz,pitch,roll; } Listener;
typedef struct { double x,y,z,radius,tempo,phase; const char *name; uint32_t color; } StarSystem;
typedef struct { int system,role,wave; double orbit,rate,eccentricity,height,phase,gain,midi; } EmitterDef;
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
#define WAVE_POINTS 48
// Per-emitter post-HRTF waveform thumbnails, accumulated at the same retarded
// arrival time as the audio bus.  Expanding shells carry these actual samples.
static float *sourceWave;
static double spatialWet=.32;
// A slow, bounded radio-like AM field: clean carriers acquire warm sidebands
// from listener-relative 3D interference rather than distortion or aliasing.
static double antennaDepth=.065;
static float *antennaField;
// The noise voices are part of the instrument, not recording dirt.  Keep a
// permanent breath floor while letting the release mix sit behind the tones.
static double noiseLevel=.55;
static int brightMode=0;
static int themeExplicit=0;
static int globeMode=0;
static int voiceCount=12;
static int duetMode=0;
static int cosmosMode=0;
static int cameraMode=0; // 0 global overview, 1 ship/listener POV
static int acousticsView=0; // score-wide acoustic telemetry / signal-chain view
static void resolve_render_theme(void){if(themeExplicit)return;const char*env=getenv("AC_RENDER_THEME");if(env&&*env){if(!strcmp(env,"light"))brightMode=1;else if(!strcmp(env,"dark"))brightMode=0;else if(!strcmp(env,"custom"))return;else env=NULL;if(env)return;}FILE*f=popen("defaults read -g AppleInterfaceStyle 2>/dev/null","r");if(!f){brightMode=0;return;}char b[32]={0};size_t n=fread(b,1,sizeof(b)-1,f);pclose(f);brightMode=!(n&&strstr(b,"Dark"));}
static const StarSystem SYSTEMS[2]={
 {-7.2,-1.8,1.1,4.2,104,.15,"percussion sun",0xff6b6b},
 { 7.4, 2.2,1.8,5.0,78,.61,"harmony sun",0x63cdda}
};
static const EmitterDef COSMOS[NSRC]={
 {0,0,0,2.3,1.00,.82,.5,.00,.105,36},{0,1,0,3.1,.50,.44,1.1,.19,.052,36},
 {0,2,1,1.8,2.00,.70,1.8,.43,.010,0},{0,3,0,3.7,.25,.25,2.0,.71,.026,48},
 {1,4,0,2.5,.75,.30,1.3,.08,.052,60},{1,5,0,3.2,.50,.22,2.0,.31,.030,64},
 {1,6,0,3.8,.50,.18,2.5,.54,.028,67},{1,7,1,2.0,1.50,.58,2.9,.77,.007,0},
 {0,8,0,4.1,.20,.20,2.4,.26,.018,43},{1,9,0,4.5,.25,.16,1.0,.64,.018,72},
 {0,10,1,2.8,.75,.62,3.0,.88,.006,0},{1,11,0,4.8,.125,.12,3.2,.92,.014,76}
};
static ACRock BODY_MESH[NSRC+3];static ACMeshAcoustics BODY_ACOUSTICS[NSRC+3];static ACMaterial BODY_MATERIAL[NSRC+3];
static Source source_at(int s,double t);
static double kick_gravity(double t);
static ACMaterial cosmos_material(int i){if(i%5==0)return AC_MAT_ALUMINUM;if(i%5==1)return AC_MAT_WOOD;if(i%5==2)return AC_MAT_GLASS;return AC_MAT_STONE;}
static void init_cosmos_bodies(void){for(int i=0;i<NSRC+3;i++){uint64_t seed=UINT64_C(0xcbf29ce484222325)^(uint64_t)(i+1)*UINT64_C(0x100000001b3);ac_rock_generate(seed,&BODY_MESH[i]);BODY_MATERIAL[i]=cosmos_material(i);ac_mesh_analyze(&BODY_MESH[i],BODY_MATERIAL[i],&BODY_ACOUSTICS[i]);}}
static Source system_sun_at(int k,double t){StarSystem d=SYSTEMS[k];double x=d.x,y=d.y,z=d.z;
 // Sectional barycentric turns with a vertical impact shake. Every child body
 // inherits this transform, so the entire local score and broadcast move.
 double starts[2]={28,60};for(int w=0;w<2;w++){double u=(t-starts[w])/12;if(u>0&&u<1){double sh=sin(M_PI*u);z+=sh*(.72*sin(t*11+k*2.1)+.34*sin(t*23+k));x+=sh*.24*sin(t*17+k);y+=sh*.20*cos(t*19+k*.7);}}
 Source q={x+.35*sin(t*.037+k),y+.28*cos(t*.031+k*.7),z,4,d.name,d.color};return q;}
static double cosmos_plane_gain(int src,double t,double az,double el){EmitterDef e=COSMOS[src];double phase=TAU*(SYSTEMS[e.system].tempo/60.0)*t*e.rate+e.phase*TAU;
 static long facetTick[NSRC]={0};static double facetEmitter[NSRC],facetRelay[NSRC];long tick=(long)(t*1000)+1;ACMaterial em=BODY_MATERIAL[src],sm=BODY_MATERIAL[NSRC+e.system],hm=BODY_MATERIAL[NSRC+2];if(facetTick[src]!=tick){facetTick[src]=tick;facetEmitter[src]=ac_mesh_facet_radiation(&BODY_MESH[src],em,phase)/(em.radiation*(1-em.absorption)+1e-6);facetRelay[src]=ac_mesh_facet_radiation(&BODY_MESH[NSRC+e.system],sm,phase*.23)/(sm.radiation*(1-sm.absorption)+1e-6);}double emitter=facetEmitter[src],relay=facetRelay[src],shipFacing=cos(az)*cos(el),receiver=ac_mesh_radiation(&BODY_ACOUSTICS[NSRC+2],hm,shipFacing)/(hm.radiation*(1-hm.absorption)+1e-6);
 // Direction and material remain audible, but enclosure losses are normalized
 // before composition so three shaped stages do not mute one another.
 double shaped=cbrt(fmax(0,emitter*relay*receiver));return .56+.72*fmin(1.05,shaped);}
static double shell_arrival(int s,double t){
 // One global audible field: every body begins outside and enters in score order.
 static const double enter[NSRC]={6.0,9.5,2.0,4.0,0.8,13.0,16.0,18.0,11.0,12.0,20.0,23.0};
 double u=(t-enter[s])/3.2;if(u<=0)return 0;if(u>=1)return 1;return u*u*(3-2*u);
}
static double shell_listen(int s,double t){double u=(shell_arrival(s,t)-.36)/.24;if(u<=0)return 0;if(u>=1)return 1;return u*u*(3-2*u);}
static int voice_enabled(int s){if(cosmosMode)return 1;if(duetMode)return s==0||s==1||s==4||s==6;static const int order[4]={4,0,2,3};if(voiceCount>=12)return 1;for(int i=0;i<voiceCount&&i<4;i++)if(s==order[i])return 1;return 0;}
static void filter_score(void){if(voiceCount>=12)return;int w=0;for(int i=0;i<NE;i++)if(voice_enabled(E[i].src))E[w++]=E[i];NE=w;}
static const int TOUR[]={4,8,0,2,10,9,3,5,1,6,11,7};
static int tour_source(double t){return TOUR[((int)fmax(0,t/9.0))%12];}
static double hz(double m){return 440*pow(2,(m-69)/12);}
static double dominant_turns(double t){return (BPM/60.0)*t;}
static double smooth01(double u){u=fmax(0,fmin(1,u));return u*u*(3-2*u);}
static double ease01(double u){u=fmax(0,fmin(1,u));return u*u*u*(10+u*(-15+6*u));}
static double ending_motion_time(double t){
 // Integrate a squared ease-out velocity over the release's final six seconds:
 // the mechanism winds down like a music box and is motionless at its cut.
 // The final authored cadence ends at bar 38; retain roughly 325 ms for its
 // physical room return, then stop. The former 38.516-bar endpoint left more
 // than a second of effectively silent transport after the decay.
 double end=38*BAR+.325,span=6.0,start=end-span;if(t<=start)return t;double u=fmax(0,fmin(1,(t-start)/span));return start+span*(u-u*u+u*u*u/3.0);
}
static double world_wobble(double t){
 // Long platter-like rocks begin away from bar lines and use non-integer
 // periods. A squared-sine window still guarantees zero displacement and
 // velocity at both ends, but the internal drift no longer feels quantized.
 double at[4]={10*BAR+.37,14*BAR+.83,26*BAR-.64,31*BAR+.41};
 static const double dur[4]={10.8,4.1,12.4,11.6},cycles[4]={1.37,.72,2.18,1.46},amp[4]={.34,.36,.54,.38};
 for(int i=0;i<4;i++)if(t>=at[i]&&t<=at[i]+dur[i]){double u=(t-at[i])/dur[i],e=sin(M_PI*u);e=smooth01(e*e);double drift=u+.075*sin(M_PI*u)*sin(TAU*(.63*u+i*.17)),phase=TAU*cycles[i]*drift;return M_PI*amp[i]*e*(.82*sin(phase)+.18*sin(phase*.47+i*1.31));}
 return 0;
}
static double super_spin_burst(double t,double at,double dur,double turns,double wobble,double cycles){
 // Integer-turn platter bursts return to the exact incoming orientation.
 // Quintic progress and a squared-sine wobble window make angular velocity and
 // the elastic displacement both reach zero at either end—never a scratch cut.
 if(t<at||t>at+dur)return 0;double raw=(t-at)/dur,u=ease01(raw),window=sin(M_PI*raw);window*=window;
 return TAU*turns*u+wobble*window*(.76*sin(TAU*(cycles*raw+.075*sin(TAU*raw)))+.24*sin(TAU*.67*raw+.41));
}
static double eccentric_phase(double mean,double eccentricity){
 // Solve Kepler's equation. Uniform score-time becomes continuous nonuniform
 // orbital speed: a fast periapsis crossing and a long, slow far-field arc.
 double m=fmod(mean,TAU);if(m<0)m+=TAU;double e=m;
 for(int i=0;i<5;i++)e-=(e-eccentricity*sin(e)-m)/(1-eccentricity*cos(e));
 return atan2(sqrt(1-eccentricity*eccentricity)*sin(e),cos(e)-eccentricity);
}
// Whole-room choreography: from 0:50–1:18 the source constellation eases
// through two complete rotations. Because every physics/audio lookup uses this
// function, the spin changes gravity, distance, Doppler and stereo—not just pixels.
static Source source_at(int s,double t){if(!cosmosMode&&!globeMode)t=ending_motion_time(t);Source q=S[s];
 if(cosmosMode){
  EmitterDef e=COSMOS[s];Source sun=system_sun_at(e.system,t);StarSystem sys=SYSTEMS[e.system];
  double parent=TAU*(sys.tempo/60.0)*t+sys.phase*TAU;
  double mean=parent*e.rate+e.phase*TAU,child=eccentric_phase(mean,e.eccentricity);
  // system sun -> dominant orbital plane -> eccentric child endpoint
  double precess=.22*sin(parent*.17+s*.81),bone=parent+precess;
  double ux=cos(bone),uy=sin(bone),vx=-uy,vy=ux;
  double radial=e.orbit*(1-e.eccentricity*cos(child));
  q.x=sun.x+ux*(radial*cos(child))+vx*(e.orbit*.42*sin(child));
  q.y=sun.y+uy*(radial*cos(child))+vy*(e.orbit*.42*sin(child));
  q.z=sun.z+e.height+.48*sin(child+s*.31);q.color=e.system?0x63cdda:S[s].color;
  // Orchestration by travel: each body begins outside its sun's useful local
  // field and physically joins the system in score order.
  static const double enter[NSRC]={0,6,14,22,10,18,26,34,30,38,46,54};double u=(t-enter[s])/7;if(u<1){u=fmax(0,u);u=u*u*(3-2*u);double far=13+fmod(s*2.7,4),ox=cos(e.phase*TAU)*far,oy=sin(e.phase*TAU)*far;q.x=sun.x+ox+(q.x-sun.x-ox)*u;q.y=sun.y+oy+(q.y-sun.y-oy)*u;q.z=sun.z+5.5+(q.z-sun.z-5.5)*u;}
  return q;
 }
 if(globeMode){
  // Lattice score: listener -> flock -> three clusters -> local resonators.
  // Every level has its own phase and breathing radius; distance is the pulse.
  Listener anchor={0,-.5,0,0,0};if(L){double at=fmax(0,t-.06)*CTRL;int i=(int)at,max=(int)(DUR*CTRL)-1;if(i>max)i=max;double f=at-i;Listener a=L[i],b=L[i+1];anchor.x=a.x+(b.x-a.x)*f;anchor.y=a.y+(b.y-a.y)*f;anchor.vx=a.vx+(b.vx-a.vx)*f;anchor.vy=a.vy+(b.vy-a.vy)*f;anchor.heading=a.heading+(b.heading-a.heading)*f;}
  int group=s/4,lane=s%4;
  // Integral of an exponentially slowing angular velocity: the composition
  // begins at 520 RPM and continuously settles to its 76 BPM revolution.
  double globalTurns=dominant_turns(t);
  double ga=TAU*globalTurns+group*TAU/3,clusterR=(2.1+.62*group)*(1+.18*sin(TAU*t/(5.7+group*1.3)+group));
  double cx=anchor.x+cos(ga)*clusterR,cy=anchor.y+sin(ga)*clusterR;
  double localRate=.13+.037*lane+.019*group,la=TAU*(localRate*t)+lane*TAU/4+group*.41;
  double localR=(.55+.18*lane)*(1+.28*sin(TAU*t/(2.3+.31*s)+s*.77));
  double schooling=.22*sin(TAU*t/(1.1+.07*s)+s*1.19);
  q.x=cx+cos(la)*localR-cos(ga)*schooling;
  q.y=cy+sin(la)*localR-sin(ga)*schooling;
  q.z=.55+.42*group+.28*lane+.46*sin(TAU*t/(1.7+.11*s)+s*.53)+.18*sin(ga+la);
  if(duetMode&&(s==0||s==1||s==4||s==6)){
   // A continuous eccentric orbit crosses the listening field. There is no
   // amplitude envelope or radial reset: proximity alone makes each pulse.
   // Skeleton: listener anchor -> rotating flock bone -> precessing shoulder
   // -> eccentric child orbit -> audible endpoint.
   double precess=.24*sin(la*.37+s*.9),bone=ga+precess;
   double rate=s==6?2.0:(s==1?.5:1.0),offset=s==4?M_PI*.58:(s==1?M_PI*.27:(s==6?M_PI*.83:0));
   double mean=TAU*(t/BEAT)*rate+offset,ecc=s==0?.84:(s==6?.72:(s==1?.42:.28));
   double orbit=eccentric_phase(mean,ecc),flex=.16*sin(la*1.7+s);
   double along=cos(orbit+flex),across=sin(orbit);
   double axis=s==0?3.05:(s==1?3.5:(s==6?2.35:2.65)),side=s==0?.92:(s==1?1.25:(s==6?.68:1.08));
   double ux=cos(bone),uy=sin(bone),vx=-uy,vy=ux;
   // The dominant bone turns around an offset world pivot. If it were centered
   // on the listener, spin would change only azimuth and could not pulse range.
   double pivotX=anchor.x+1.45,pivotY=anchor.y-.82;
   double shoulderX=pivotX+ux*axis,shoulderY=pivotY+uy*axis;
   q.x=shoulderX-ux*(axis*along)+vx*(side*across);
   q.y=shoulderY-uy*(axis*along)+vy*(side*across);
   q.z=(s==0?.72:(s==1?.95:(s==6?2.45:1.72)))+(s==0?.18:(s==6?.22:.48))*sin(orbit+(s==4?.7:0));
  }
  return q;
 }
 // Common room-mode listener anchor. The fast release tornado pivots every
 // ordinary score body around this interpolated receiver position.
 Listener anchor={0,-.5,0,0,0};
 if(L){double at=fmax(0,t-.06)*CTRL;int i=(int)at,max=(int)(DUR*CTRL)-1;if(i>max)i=max;double f=at-i;Listener a=L[i],b=L[i+1];anchor.x=a.x+(b.x-a.x)*f;anchor.y=a.y+(b.y-a.y)*f;anchor.vx=a.vx+(b.vx-a.vx)*f;anchor.vy=a.vy+(b.vy-a.vy)*f;anchor.heading=a.heading+(b.heading-a.heading)*f;}
 // The lullaby begins as rotation alone.  Ninety-six complete turns land at
 // the original orientation, while the quartic ease gives ~30 rotations/sec
 // at the first instant and exactly zero angular velocity at the hand-off.
 // At that speed the moving HRTF/proximity field fuses into a spatial hum.
 if(t<4*BAR){double u=fmax(0,fmin(1,t/(4*BAR))),a=TAU*96*(1-pow(1-u,4));
  double x=q.x,y=q.y;q.x=x*cos(a)-y*sin(a);q.y=x*sin(a)+y*cos(a);q.z+=.16*sin(a+s*.7);
 }
 if(t>=50&&t<=78){double raw=(t-50)/28,u=ease01(ease01(raw)),window=sin(M_PI*raw);window*=window;double a=TAU*2*u;
 // The eight-turn centrifuge gathers momentum over eleven seconds. A long,
 // windowed platter wobble now rides inside the turn instead of arriving as a
 // separate scratch afterward. Both displacement and velocity return cleanly
 // to zero at the endpoints, so the gesture stays elastic without a skid.
 if(t>=61.5){double raw=fmax(0,fmin(1,(t-61.5)/11.0)),x=ease01(raw),window=sin(M_PI*raw);window*=window;double wobble=.31*window*(.78*sin(TAU*(1.63*raw+.08*sin(TAU*raw)))+.22*sin(TAU*.71*raw+.4));a+=TAU*8*x+wobble;}
 double x=q.x,y=q.y;q.x=x*cos(a)-y*sin(a);q.y=x*sin(a)+y*cos(a);q.z+=.22*window*sin(a+s*.7);}
 // Three smaller super-spins join the central centrifuge: a four-turn bloom
 // at release 0:14, a two-turn pickup around 0:25, and a late four-turn bloom.
 // All overlap long world-wobble windows, so their orbits audibly bend and
 // breathe instead of skating.
 {
  double intro=super_spin_burst(t,32.6,6.2,32,.46,1.42);
  double early=super_spin_burst(t,43.6,5.9,2,.42,1.18);
  double late=super_spin_burst(t,85.7,8.6,4,.49,1.73);
  if(intro!=0){
   // The fast tornado is listener-centric: translate into head space, rotate
   // and taper there, then return to the world. Thus every sounding body truly
   // crosses the receiver instead of orbiting an unrelated room origin.
   double x=q.x-anchor.x,y=q.y-anchor.y;
   double rx=x*cos(intro)-y*sin(intro),ry=x*sin(intro)+y*cos(intro);
   double w0=pow(sin(M_PI*(t-32.6)/6.2),2),funnel=1-.38*w0;
   q.x=anchor.x+rx*funnel;q.y=anchor.y+ry*funnel;
   q.z+=1.15*w0+.30*w0*sin(intro*.22+s*.67);
  }
  double roomSpin=early+late;
  if(roomSpin!=0){
   double x=q.x,y=q.y;
   q.x=x*cos(roomSpin)-y*sin(roomSpin);q.y=x*sin(roomSpin)+y*cos(roomSpin);
   double w1=(t>=43.6&&t<=49.5)?pow(sin(M_PI*(t-43.6)/5.9),2):0;
   double w2=(t>=85.7&&t<=94.3)?pow(sin(M_PI*(t-85.7)/8.6),2):0;
   q.z+=.16*fmax(w1,w2)*sin(roomSpin+s*.39);
  }
 }
 {double a=world_wobble(t);if(a!=0){double x=q.x,y=q.y;q.x=x*cos(a)-y*sin(a);q.y=x*sin(a)+y*cos(a);q.z+=.12*sin(a+s*.51);}}
 // In the visual/listening-field cut, the whole composition physically winds
 // up after its opening assembly. This rotation feeds gravity, Doppler, HRTF,
 // telemetry, and light transport; the camera does not manufacture the spin.
 // Echo and air are memory-bodies: they occupy where the listener used to be.
 // During simulation these indices are always in the already-computed past.
 if(L&&(s==5||s==11)){double lag=s==5?2.2:5.5;int i=(int)(fmax(0,t-lag)*CTRL),max=(int)(DUR*CTRL);if(i>max)i=max;Listener p=L[i];double side=s==5?.55:-.8;q.x=p.x-cos(p.heading)*.45-sin(p.heading)*side;q.y=p.y-sin(p.heading)*.45+cos(p.heading)*side;q.z=s==5?2.35:1.85;}
 // Final movement: every voice peels away from the room and occupies a
 // progressively older point on the listener's path, forming an audible snake.
 if(L&&t>=82){double mix=fmin(1,(t-82)/10);mix=mix*mix*(3-2*mix);double lag=1.0+s*.72;int i=(int)(fmax(0,t-lag)*CTRL),max=(int)(DUR*CTRL);if(i>max)i=max;Listener p=L[i];double side=((s&1)?1:-1)*(.08+.018*s),tx=p.x-sin(p.heading)*side,ty=p.y+cos(p.heading)*side,tz=.55+fmod(s*1.37,2.5);q.x=q.x*(1-mix)+tx*mix;q.y=q.y*(1-mix)+ty*mix;q.z=q.z*(1-mix)+tz*mix;}
 // Kick Gravity is a shared physical effector: tonal bodies contract toward
 // the boom source, then overshoot outward on the eased elastic rebound. This
 // changes listener forces, Doppler, HRTF, distance, pixels, and room returns.
 if(s!=1){double gravity=kick_gravity(t),pull=.11*gravity;q.x+=(S[1].x-q.x)*pull;q.y+=(S[1].y-q.y)*pull;q.z+=(S[1].z-q.z)*pull;}
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
static void tornado_whir(double t,double d,double g,int src){
 // Type 8 is a band-limited rotor whose blade rate is derived from the exact
 // 32-turn tornado ease. It is observed through the moving source path below,
 // rather than pasted into the stereo master as a centered sound effect.
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,0,0,g,.035,.16,src,8,(uint32_t)(n*2654435761u)};}
}
static void crash_cymbal(double t,double g,int src){
 // One broad, spatially positioned cymbal bloom. Three filtered noise strata
 // create a metallic attack and a long darkening tail without a sample click.
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,3.65,3100,14200,g,.045,3.28,src,1,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t+.012,3.15,1350,7600,g*.58,.052,2.82,src,1,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t+.028,2.45,620,3400,g*.31,.060,2.18,src,1,(uint32_t)(n*2654435761u)};}
}
static void whistlev(double t,double d,double m,double g,int src){
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,hz(m),hz(m),g,.24,.72,src,7,(uint32_t)(n*2654435761u)};}
}
static void blastv(double t,double g,int src){
 // One collision excites a pressure crack, bright debris, body noise, and an
 // inharmonic metal resonator bank. Type 2 owns the visible particle burst;
 // type 4 is additional noise and type 3 is a freely tuned impact partial.
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.92,90,7200,g*1.55,.0004,.84,src,2,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t+.002,.48,1800,13800,g*.82,.0003,.43,src,4,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.72,45,720,g*.72,.0008,.66,src,4,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,1.28,58,58,g*.72,.0009,1.18,src,3,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t+.004,.94,87,87,g*.38,.001,.86,src,3,(uint32_t)(n*2654435761u)};}
 ACRock rock;uint64_t rockSeed=((uint64_t)(t*1000003.0)<<17)^(uint64_t)(src+1)*UINT64_C(0x9e3779b97f4a7c15);ac_rock_generate(rockSeed,&rock);double partial[5],amp[5];ac_rock_modes(&rock,partial,amp);
 for(int k=0;k<5&&NE<MAXE;k++){double f=partial[k]*.58;int n=NE++;E[n]=(Event){t+k*.0007,.76-k*.060,f,f,g*amp[k],.0005,.68-k*.045,src,3,(uint32_t)(n*2654435761u)};}
 // A selection of actual tetrahedral face-shards contributes its own mode.
 for(int k=0;k<rock.nf&&NE<MAXE;k++){ACRockFace f=rock.f[k];ACRockV a=rock.v[f.a],b=rock.v[f.b],c=rock.v[f.c];double ab=hypot(hypot(a.x-b.x,a.y-b.y),a.z-b.z),bc=hypot(hypot(b.x-c.x,b.y-c.y),b.z-c.z),ca=hypot(hypot(c.x-a.x,c.y-a.y),c.z-a.z),span=(ab+bc+ca)/3,depth=(.22+.22*fmod(k*.371+rock.roughness,1))*sqrt((a.x+b.x+c.x)*(a.x+b.x+c.x)+(a.y+b.y+c.y)*(a.y+b.y+c.y)+(a.z+b.z+c.z)*(a.z+b.z+c.z))/3,freq=105+460/(span+.45*depth+.12),returnTime=.18+fmod(k*.037, .34);int n=NE++;E[n]=(Event){t+returnTime,.28+.16*depth,freq,freq,g*(.018+.022*depth),.0005,.24+.12*depth,src,3,(uint32_t)(n*2654435761u)};}
}
static void chipv(double t,double g,int src){
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.105,1800,7800,g,.0003,.090,src,5,(uint32_t)(n*2654435761u)};}
}
static void ticktack(double t,int tack,double g,int src){
 // Diamond ticks are tiny resonant strikes, not broadband groove pops.
 double f=tack?920:2860;if(NE<MAXE){int n=NE++;E[n]=(Event){t,tack?.145:.105,f,f,g*.72,.006,tack?.125:.088,src,3,(uint32_t)(n*2654435761u)};}if(NE<MAXE){int n=NE++;E[n]=(Event){t+.003,tack?.095:.072,f*2.71,f*2.71,g*.20,.008,tack?.078:.058,src,3,(uint32_t)(n*2654435761u)};}
}
static void gong(double t,double root,double g){
 // One deterministic FEM-style gong: a generated triangular body supplies
 // its inharmonic mode ratios, normalized around a very low struck root.
 ACRock body;double mode[5],weight[5];ac_rock_generate(UINT64_C(0xf36a91b72d4c8801),&body);ac_rock_modes(&body,mode,weight);double base=fmax(1,mode[0]);
 for(int i=0;i<5&&NE<MAXE;i++){double ratio=fmax(1,mode[i]/base),f=root*ratio,d=11.8-i*1.35,a=g*(i?weight[i]*.72:1);int n=NE++;E[n]=(Event){t+i*.009,d,f,f,a,.022+i*.009,d*.94,10,3,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,10.5,root*.5,root*.5,g*.28,.030,9.8,10,3,(uint32_t)(n*2654435761u)};}
 noisev(t,.12,520,4600,g*.075,10);
}
static double env(const Event*e,double t){double u=t-e->t;if(u<0||u>=e->dur)return 0;double a=fmin(1,u/e->atk),r=fmin(1,(e->dur-u)/e->rel);return sin(a*M_PI/2)*sin(a*M_PI/2)*sin(r*M_PI/2)*sin(r*M_PI/2);}

static const int chord[4][4]={{48,55,64,74},{45,52,60,71},{53,60,69,67},{55,62,71,69}};
// Middle-act body voicings remain unequivocally beneath Theme B. The earlier
// upper pad tones (69/71 in particular) duplicated and crossed its lead around
// release 0:36; this register fence leaves 9–17 semitones of vertical air.
static const int underChord[4][4]={{36,48,52,55},{33,45,48,52},{41,48,53,57},{43,50,55,59}};
static const int root[4]={36,33,41,43};
// Two related four-bar phrases make the melody a developing story rather than
// one cell copied 38 times. -1 is a sung breath/rest. Phrase A is the intimate
// question; B opens its register and answers it before the bridge transforms it.
static const int THEME_A[4][8]={
 {64,-1,67,69,67,-1,64,62},{64,67,-1,69,72,71,69,-1},
 {65,-1,69,72,69,67,-1,65},{62,67,71,-1,69,67,64,62}
};
static const int THEME_B[4][8]={
 {67,69,72,-1,71,69,67,64},{69,-1,72,76,74,72,71,69},
 {69,72,77,-1,76,72,69,67},{71,-1,74,79,77,74,71,69}
};
static const int COUNTER[4][4]={
 {55,52,48,52},{57,52,48,45},{53,57,60,57},{55,59,62,59}
};
static const int ARP[4][8]={
 {48,52,55,60,64,67,72,67},{45,48,52,57,60,64,69,64},
 {41,45,48,53,57,60,65,60},{43,47,50,55,59,62,67,62}
};
static double human_unit(int bar,int step,int lane){uint32_t x=(uint32_t)(bar+1)*0x9e3779b9u^(uint32_t)(step+3)*0x85ebca6bu^(uint32_t)(lane+7)*0xc2b2ae35u;x^=x>>16;x*=0x7feb352du;x^=x>>15;x*=0x846ca68bu;x^=x>>16;return(x&0xffff)/65535.0;}
static double human_time_offset(int bar,int step,int lane){double form=ease01((bar-6)/32.0),bias=.030*(1-form)-.022*form,commit=.65+.70*human_unit(bar,step,lane+41),jitter=(human_unit(bar,step,lane)-.5)*.038;return bias*commit+jitter;}
static double swung_eighth(double barStart,int bar,int step,int lane){double swing=(step&1)*BEAT*.105,jitter=human_time_offset(bar,step,lane);return fmax(0,barStart+step*BEAT*.5+swing+jitter);}
static double human_gain(int bar,int step,int lane){return .84+.28*human_unit(bar,step,lane);}
static double human_kick_gain(int bar,int step){return .94+.12*human_unit(bar,step,1);}
static int narrative_note(int bar,int step){const int(*theme)[8]=(bar>=12&&bar<28)?THEME_B:THEME_A;int note=theme[bar&3][step];if(note<0)return note;
 // The answer reaches upward before the super-spin. The homecoming keeps the
 // contour but drops occasional notes an octave, like a tired singer returning.
 if(bar>=16&&bar<24&&(step==2||step==4)&&note<=72)note+=12;
 if(bar>=28&&bar<34&&(step==1||step==5)&&note>67)note-=12;
 if(bar>=34&&step>3)return-1;
 return note;
}
static void melody_story_bar(int bar,double t,double transpose,double level){for(int q=0;q<8;q++){int note=narrative_note(bar,q);if(note<0)continue;note+=(int)lrint(12*log2(transpose));while(note>81)note-=12;double at=swung_eighth(t,bar,q,0),dur=BEAT*((q&1)?.37:.45),accent=(q==0||q==4)?1.12:1;ev(at,dur,note,level*accent*human_gain(bar,q,0),4,.035,.22+(q&1)*.05);
  // A few breathy octave ghosts answer phrase endings, never every note.
  if(bar>=12&&bar<32&&(q==3||q==7))ev(at+BEAT*.055,dur*.72,note+12,.0105*human_gain(bar,q,4),5,.06,.25);
 }}
static void counterpoint_bar(int bar,double t,double transpose){if(bar<13||bar>=34)return;double section=bar<16?.022:(bar<20?.030:(bar<23?.030+.003*(bar-19):(bar<28?.039:.025))),delay=bar>=20&&bar<28?BEAT*.54:BEAT*.16;for(int q=0;q<4;q++){int note=COUNTER[bar&3][q]+(int)lrint(12*log2(transpose));if(bar>=28&&q>1)note-=12;double at=swung_eighth(t,bar,q*2,2)+delay+(human_unit(bar,q,2)-.5)*.018;ev(at,BEAT*(bar>=20&&bar<28?1.28:.88),note,section*human_gain(bar,q,2),5,.12,.48);}}
static void arpeggio_bar(int bar,double t,double transpose){if(bar<12||bar>=32)return;int sparse=bar<16||bar>=28;double level=bar<16?.010:(bar<20?.0145:(bar==20?.016:(bar==21?.018:(bar<28?.020:.0115))));for(int q=0;q<8;q++){if(sparse&&!(q&1))continue;int note=ARP[bar&3][q]+(int)lrint(12*log2(transpose));while(note>79)note-=12;if(bar>=12&&bar<20){int lead=narrative_note(bar,q);if(lead>=0){lead+=(int)lrint(12*log2(transpose));while(note>lead-9)note-=12;}}double at=swung_eighth(t,bar,q,3);ev(at,BEAT*.31,note,level*((q==0||q==4)?1.2:1)*human_gain(bar,q,3),q&1?3:2,.022,.19);}}
static void opening_glide(double t,double d,double f0,double f1,double g,int src){if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,f0,f1,g,.028,d*.78,src,0,(uint32_t)(n*2654435761u)};}}
static void opening_air(double t,double d,double lo,double hi,double g,int src){if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,lo,hi,g,.024,d*.72,src,1,(uint32_t)(n*2654435761u)};}}
static void electro_kick(double t,double g){
 // The sub/fundamental owns full-range systems; a short clean upper-bass bend
 // and tiny beater band make the same impact legible on laptop speakers.
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.40,142,45,g,.018,.31,1,0,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.21,68,44,g*.22,.014,.17,1,0,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t+.001,.145,318,112,g*.34,.004,.118,1,0,(uint32_t)(n*2654435761u)};}
 if(NE<MAXE){int n=NE++;E[n]=(Event){t,.030,1180,3900,g*.075,.004,.022,1,1,(uint32_t)(n*2654435761u)};}
}
static double percussion_fade(double t){return t<8*BAR?0:ease01((t-8*BAR)/(4.5*BAR));}
static double rolling_fade(double t){return .22+.78*ease01((t-6*BAR)/(3*BAR));}
static double low_voice_fade(double t){return t<9.5*BAR?0:ease01((t-9.5*BAR)/(4.5*BAR));}
static double kick_gravity(double t){
 if(t<10*BAR||t>=35*BAR)return 0;int bar=(int)floor(t/BAR);double force=0;
 for(int b=bar-1;b<=bar;b++){if(b<10||b>=35)continue;double start=b*BAR,hit[2]={swung_eighth(start,b,0,1),swung_eighth(start,b,4,1)};
  for(int k=0;k<2;k++){double age=t-hit[k];if(age<0||age>.78)continue;double attack=ease01(age/.028),suck=attack*exp(-age/.245),rebound=.18*attack*exp(-age/.44)*sin(TAU*age/.34);force+=low_voice_fade(hit[k])*(suck+rebound);}}
 return fmax(-.28,fmin(1.12,force));
}
static void triangle_bell(double t,double d,int note,double g,int src){if(NE<MAXE){int n=NE++;E[n]=(Event){t,d,hz(note),2.713,g,.42,1.35,src,6,(uint32_t)(n*2654435761u)};}}
static void fem_bell(double t,double d,int note,double g,int src,uint64_t seed){
 // A generated asymmetric body supplies a small set of inharmonic resonances.
 // Every partial is still a clean Nyquist-bounded sine; texture comes from the
 // mesh's geometry and independent decay, not clipping or folded waveforms.
 ACRock body;double mode[5],weight[5];ac_rock_generate(seed,&body);ac_rock_modes(&body,mode,weight);double fundamental=hz(note),base=fmax(1,mode[0]);
 for(int k=0;k<5&&NE<MAXE;k++){double ratio=k?mode[k]/base:1,f=fundamental*ratio;if(f>SR*.38)continue;double dur=d*(1-.105*k),gain=g*(k?weight[k]*1.08:1),attack=.018+.017*k,release=fmax(.24,dur*(.82-.09*k));int n=NE++;E[n]=(Event){t+k*.006,dur,f,f,gain,attack,release,src,3,(uint32_t)(n*2654435761u)};}
}
static void marimba_note(double t,int note,double g,int src){double f=hz(note);if(NE<MAXE){int n=NE++;E[n]=(Event){t,.58,f,f,g,.002,.51,src,3,(uint32_t)(n*2654435761u)};}if(NE<MAXE){int n=NE++;E[n]=(Event){t+.001,.34,f*3.97,f*3.97,g*.23,.0015,.30,src,3,(uint32_t)(n*2654435761u)};}if(NE<MAXE){int n=NE++;E[n]=(Event){t+.002,.19,f*9.18,f*9.18,g*.055,.001,.16,src,3,(uint32_t)(n*2654435761u)};}}
static void marimba_bar(int bar,double t){if(bar<30||bar>32)return;static const int notes[3][4]={{60,64,67,72},{62,67,71,74},{65,69,72,76}};static const int step[4]={0,2,4,6};int row=bar-30;for(int i=0;i<4;i++){double at=swung_eighth(t,bar,step[i],18)+BEAT*.18+(human_unit(bar,i,18)-.5)*.016;marimba_note(at,notes[row][i],.0115*human_gain(bar,i,18),i&1?8:10);}}
static void sine_coda_note(double t,double d,int note,double g,int lane){ev(t,d,note,g,lane&1?3:2,.012,fmin(d*.72,.46));if(note>=67)ev(t+.014,d*.62,note+12,g*.16,lane&1?2:3,.018,fmin(d*.52,.32));}
static void early_sine_dilly(int bar,double t){
 if(bar<14||bar>=27||(bar&1))return;
 static const int motif[4][8]={{72,-1,74,76,-1,74,71,-1},{69,72,-1,76,74,-1,72,-1},{69,-1,72,77,-1,76,72,-1},{71,74,-1,79,77,-1,74,-1}};
 double arc=bar<14?.82:(bar<20?1.0:1.16),level=.0225*arc;
 for(int q=0;q<8;q++){int note=motif[(bar/2)&3][q];if(note<0)continue;double at=swung_eighth(t,bar,q,12);sine_coda_note(at,BEAT*((q&1)?.27:.38),note,level*human_gain(bar,q,12),q);}
}
static void sine_coda_bar(int bar,double t){
 if(bar<27||bar>37)return;
 // The late-act "piano" role remains, but every key is now a positioned sine:
 // C6/9, Cmaj9, Am9, Fmaj9, G13, then an explicit G9 -> Cmaj9 close.
 static const int voicing[11][6]={
  {36,48,52,55,62,-1},{36,55,59,62,64,-1},{45,52,55,59,60,-1},
  {41,48,52,55,57,-1},{43,50,53,59,64,-1},{36,55,59,62,64,-1},
  {45,52,55,59,60,-1},{41,48,52,55,57,-1},{43,50,53,59,64,-1},
  {43,50,53,59,62,-1},{36,48,52,55,59,62}
 };
 static const int run[11][8]={
  {-1,76,74,71,-1,72,74,-1},{64,-1,67,71,74,-1,72,75},
  {76,72,-1,71,-1,67,64,-1},{69,-1,72,76,-1,79,-1,76},
  {71,-1,74,77,76,-1,74,-1},{76,75,74,-1,71,-1,67,-1},
  {72,-1,76,79,-1,83,-1,79},{69,-1,72,-1,76,79,-1,81},
  {71,-1,74,77,-1,76,74,-1},{79,-1,77,-1,74,-1,71,-1},
  {-1,-1,76,74,72,-1,-1,-1}
 };
 int row=bar-27;double arc=bar<30?.68:(bar<34?.94:(bar<37?1.24:.74)),chordGain=.012*arc;
 for(int i=0;i<6&&voicing[row][i]>=0;i++){double at=t+i*.020+(human_unit(bar,i,10)-.5)*.010;sine_coda_note(at,BAR*(bar==37?.96:.84)-i*.020,voicing[row][i],chordGain*(i<2?.82:1)*human_gain(bar,i,10),i);}
 for(int q=0;q<8;q++)if(run[row][q]>=0){double at=swung_eighth(t,bar,q,11);int note=run[row][q];if((bar==30&&q>=2)||bar==31||(bar==32&&q<=4))note+=12;sine_coda_note(at,BEAT*((q&1)?.30:.42),note,.023*arc*human_gain(bar,q,11),q);}
}
static void sine_coda_pickup(void){static const int n[6]={74,77,79,81,80,79};static const double dt[6]={0,.24,.50,.82,1.08,1.28};for(int i=0;i<6;i++)sine_coda_note(84+dt[i],i==5?.36:.18,n[i],.0135+i*.0008,i);}
static void triangle_bell_melody(int bar,double t,double transpose){
 // A restrained FEM field begins one bar before the intro tornado. Its long
 // tails are therefore already radiating when the 32-turn pickup arrives,
 // instead of entering nine seconds after the spin has ended.
 if(bar<10||bar>=36)return;
 static const int melody[4][4]={{84,88,95,91},{81,84,91,88},{89,93,96,95},{91,95,98,93}};
 static const int step[4][4]={{0,3,5,7},{0,2,6,7},{0,3,4,7},{0,2,5,7}};
 double arc=bar<16?.70:(bar<24?1.0:(bar<30?1.18:.82)),tr=12*log2(transpose);
 for(int i=0;i<4;i++){int note=melody[bar&3][i]+(int)lrint(tr);while(note>100)note-=12;double at=swung_eighth(t,bar,step[bar&3][i],14)+(human_unit(bar,i,14)-.5)*.035,d=2.7+.85*human_unit(bar,i,15),gain=.0058*arc*human_gain(bar,i,14);uint64_t seed=UINT64_C(0x62a9d9ed799705f5)^(uint64_t)(bar+1)*UINT64_C(0x9e3779b97f4a7c15)^(uint64_t)(i+3)*UINT64_C(0xbf58476d1ce4e5b9);fem_bell(at,d,note,gain,i&1?10:5,seed);if(((bar+i)&3)==1)triangle_bell(at+.035,d*.82,note,gain*.34,i&1?10:5);}
}
static void whistle_answer(int bar,double t){
 // Sparse native AC-OS/Notepat waveguide-flute answers: held, breathy tones
 // in the E4-C5 register. The opening remains whistle-free.
 if(bar==14){whistlev(t+BEAT*.35,BEAT*2.80,64,.0125,5);whistlev(t+BEAT*2.20,BEAT*2.35,67,.0110,5);}
 else if(bar==22){whistlev(t+BEAT*.20,BEAT*3.00,69,.0135,9);whistlev(t+BEAT*2.35,BEAT*2.50,67,.0115,9);}
 else if(bar==29){whistlev(t+BEAT*.45,BEAT*3.25,72,.0130,5);}
 else if(bar==35){whistlev(t+BEAT*.30,BEAT*2.80,67,.0110,9);whistlev(t+BEAT*2.15,BEAT*2.40,64,.0100,9);}
}
static double duet_frequency(int s,double t){
 if(s==0)return 36;
 if(s==1){static const double roots[8]={36,36,33,33,29,31,33,28};double turns=dominant_turns(t)*.25,whole=floor(turns),phase=turns-whole;int i=((long)whole)%8;if(i<0)i+=8;double x=fmax(0,fmin(1,(phase-.86)/.14));x=x*x*(3-2*x);return hz(roots[i]+(roots[(i+1)%8]-roots[i])*x);}
 static const double phrase[16]={64,67,69,71,69,67,64,62,60,64,67,72,71,67,69,64};
 double turns=dominant_turns(t),whole=floor(turns),phase=turns-whole;
 int i=((long)whole)%16;if(i<0)i+=16;
 // Phase remains continuous, but frequency changes cleanly at the angular
 // boundary. Removing the double portamento avoids the rubbery squeak.
 return hz(phrase[i]-12);
}
static double cosmos_frequency(int s,double t){
 EmitterDef e=COSMOS[s];if(e.wave)return 0;
 int bar=(int)(t/BAR),ci=(bar/2)&3;static const int rootMidi[4]={36,32,39,34};static const int melody[16]={0,3,5,7,10,7,5,3,0,5,7,12,10,7,5,3};int root=rootMidi[ci],role=e.role,midi;
 if(role==0)midi=36;else if(role==1)midi=root;else if(role==3)midi=root+7;else if(role==8)midi=root+12;
 else if(role==4){int step=(int)floor(dominant_turns(t));midi=root+24+melody[step&15];}
 else if(role==5)midi=root+(ci==1?16:15);else if(role==6)midi=root+19;else if(role==9)midi=root+(ci==2?23:22);else midi=root+31;
 return hz(midi);
}
static void score(void){
 if(cosmosMode){init_cosmos_bodies();for(int s=0;s<NSRC;s++){EmitterDef e=COSMOS[s];if(e.wave)noisev(0,DUR+.2,s==2?950:680,s==2?2500:1800,e.gain*.68,s);else ev(0,DUR+.2,e.midi,e.gain*(s>=5?.78:1),s,.006,.035);}for(int b=0;b<38;b++){double t=b*BAR;for(int q=0;q<8;q++)if(!(b%8==7&&q>=6))chipv(t+q*BEAT*.5,(q==0||q==4)?.0215:.0121,(q&1)?7:10);}for(int b=2;b<38;b+=4){int a=(b/2)&1?10:7,o=a==10?7:10;double t=b*BAR,main=b<14?.068:(b<30?.087:.060);blastv(t,main,a);if(b>=14&&b<32)blastv(t+2.5*BEAT,main*.52,o);if(b>=22&&b<30)blastv(t+BAR+3.25*BEAT,main*.42,o);}return;}
 if(duetMode){ev(0,DUR+.2,36,.082,0,.006,.035);ev(0,DUR+.2,36,.064,1,.006,.035);ev(0,DUR+.2,64,.078,4,.006,.035);noisev(0,DUR+.2,1400,3200,.012,6);return;}
 if(globeMode){
  // A resonator is a place, not a note event: one fixed-frequency oscillator
  // with continuous phase for the entire work. Composition comes exclusively
  // from listener/source motion, Doppler, interference, and spatial filtering.
  static const double pitch[NSRC]={36,48,55,64,72,79,84,88,60,67,43,76};
  static const double gain[NSRC]={.060,.032,.040,.038,.048,.022,.010,.009,.025,.023,.034,.014};
  for(int s=0;s<NSRC;s++)ev(0,DUR+.2,pitch[s],gain[s],s,.006,.035);
  return;
 }
 // The release hears one physical rotor during the 8x intro tornado. Source 11
 // is the air body, so its whirr follows the same 3D funnel as the other forms.
 tornado_whir(32.6,6.2,.052,11);
 for(int b=0;b<38;b++){
  double t=b*BAR, tr=(b>=20&&b<28)?4.0/3:1;
  // Opening: no choir yet, but the twisting room is already the song.  Four
  // sustained resonators make the rotational hum; a quarter-note low wave,
  // alternating air answers, and a lighter statement of the lullaby give the
  // visible bodies clear emissions throughout the 96-turn deceleration.
  if(b<4){
   if(b==0){ev(t,4*BAR-.55,36,.055,0,2.8,2.6);ev(t,4*BAR-.55,43,.038,2,3.4,2.8);ev(t,4*BAR-.55,48,.030,3,4.0,3.0);ev(t,4*BAR-.55,55,.018,4,4.8,3.4);noisev(t,4*BAR-.55,2100,7200,.0065*noiseLevel,11);}
   for(int q=0;q<4;q++){double pulse=swung_eighth(t,b,q*2,1),air=swung_eighth(t,b,q*2+1,6);opening_glide(pulse,.36,72,38,(q==0?.090:.066)*human_gain(b,q,1),1);opening_air(air,.12,2400,6800,.025*noiseLevel*human_gain(b,q,6),q&1?7:6);}
   continue;
  }
  // Bars four and five are deliberately empty handles between the discarded
  // source prelude and release. The published track starts directly at bar six
  // so no oscillator pre-roll or reflected residue leaks across its first beat.
  if(b==4||b==5)continue;
  double roll=rolling_fade(t);
  // Release entrance: lead alone, then one new sustained body per bar from
  // high to low. Four distinct spatial bodies make the additive build audible.
  int padVoices=b<=6?0:(b==7?1:(b==8?2:(b==9?3:4))),firstPad=4-padVoices;
  static const int padSource[4]={2,3,5,10};
  for(int k=firstPad;k<4;k++){int note=b==36?((int[]){43,50,53,59})[k]:b==37?((int[]){36,48,52,59})[k]:(b>=12&&b<20?underChord[b%4][k]:chord[b%4][k]);ev(t,BAR+.1,note+12*log2(tr),.034*roll,padSource[k],.7,1.0);}
  // Continuous independently filtered noise voices.
  double noiseScale=noiseLevel;
  for(int q=0;q<4;q++){double at=swung_eighth(t,b,q*2+1,6),fade=percussion_fade(at);if(fade>0)noisev(at,.085,4200,10200,.065*noiseScale*fade*human_gain(b,q,6),q&1?7:6);}
  {double airFade=percussion_fade(t);if(airFade>0)noisev(t,BAR,1700,5700,.013*noiseScale*airFade,11);}
  for(int q=1;q<4;q+=2){double at=swung_eighth(t,b,q*2,7),fade=percussion_fade(at);if(fade>0)noisev(at,.15,620,2100,.052*noiseScale*fade*human_gain(b,q,7),q==1?8:9);}
  // A clockwork trot rather than another hat grid: one bright tick and one
  // lower woody tack occupy the spaces between the two kick anchors.
  if(b>=7&&b<35){double tick=swung_eighth(t,b,2,16),tack=swung_eighth(t,b,6,17),tf=percussion_fade(tick),af=percussion_fade(tack);if(tf>0)ticktack(tick,0,.017*tf*human_gain(b,2,16),8);if(af>0)ticktack(tack,1,.020*af*human_gain(b,6,17),9);}
  if(b>=10&&b<35){double down=swung_eighth(t,b,0,1),back=swung_eighth(t,b,4,1),section=b<12?.76:(b<20?.90:(b<28?1.08:(b<32?.82:.62))),density=exp(-.5*pow((b-25)/1.65,2));section*=1-.60*density;double lowDown=low_voice_fade(down),lowBack=low_voice_fade(back),downArc=section*lowDown,backArc=section*lowBack,bassDelay=.055;ev(down+bassDelay,BAR-bassDelay,root[b%4],.050*(.45+.55*roll)*lowDown*human_gain(b,0,1),0,.13,.5);if(downArc>0)electro_kick(down,.0330*downArc*human_kick_gain(b,1));if(backArc>0)electro_kick(back,.0300*backArc*human_kick_gain(b,4));}
  // The melody now moves through a question, an answering register, a bridge
  // climax, and a partial homecoming. Counterpoint and arpeggios enter only
  // after the theme is known, then recede before the final unaccompanied cadence.
  if(b<36){double lead=b<12?.057:(b<20?.064:(b<28?.070:(b<34?.056:.047)));melody_story_bar(b,t,tr,lead*(.48+.52*roll));}
  counterpoint_bar(b,t,tr);
  arpeggio_bar(b,t,tr);
  early_sine_dilly(b,t);
  triangle_bell_melody(b,t,tr);
  whistle_answer(b,t);
  // Let the bridge harmony arrive before the cymbal: the former downbeat hit
  // stacked with two vowel choirs and made release 0:44 a quantized wall.
  if(b==20)crash_cymbal(t+BEAT*3.10,.036*noiseLevel,10);
  sine_coda_bar(b,t);
  marimba_bar(b,t);
 }
 sine_coda_pickup();
 gong(30*BAR+BEAT*.18,43.65,.052);
 // An explicit G7 -> Cmaj9 homecoming replaces the former unresolved Am tail.
 ev(36*BAR,BAR,43,.060,0,.08,.5);ev(37*BAR,BAR,36,.058,0,.08,1.1);
 ev(36*BAR,BEAT*.86,67,.078,4,.05,.32);ev(36*BAR+BEAT,BEAT*.86,65,.074,4,.05,.32);
 ev(36*BAR+2*BEAT,BEAT*.86,62,.071,4,.05,.36);ev(36*BAR+3*BEAT,BEAT*.92,59,.068,4,.05,.42);
 ev(37*BAR,BAR-.08,60,.082,4,.07,2.2);ev(37*BAR+BEAT*.18,BAR-.35,64,.026,5,.11,2.0);ev(37*BAR+BEAT*.34,BAR-.55,67,.022,5,.13,1.9);
 // No second outro: the bar-37 C-major cadence and its spatial room returns
 // are the final musical event. Mastering does not add a global fade.
}

static void simulate(void){
 int n=(int)(DUR*CTRL)+1;L=calloc(n,sizeof(*L));L[0]=(Listener){0,-.5,0,0,0};double dt=1.0/CTRL;
 if(cosmosMode){
  // Six-DOF goal-seeking ship. It flies a sequence of world-space objectives;
  // yaw/pitch follow velocity and roll banks continuously into each turn.
  static const Source goals[]={{-10,-5,1},{-7,-1,4},{0,6,-1},{7,2,0},{10,-4,4},{0,0,7},{-7,-2,-2},{0,-6,2},{7,2,5},{0,1,0}};
  L[0]=(Listener){-10,-5,0,0,0,1,0,0,0};
  for(int i=1;i<n;i++){double t=i*dt;int gi=((int)(t/8.5))%10;Source g=goals[gi];Listener p=L[i-1];double dx=g.x-p.x,dy=g.y-p.y,dz=g.z-p.z,d=sqrt(dx*dx+dy*dy+dz*dz)+.001;
   double desired=1.85*tanh(d*.38),ax=dx/d*desired-p.vx,ay=dy/d*desired-p.vy,az=dz/d*desired-p.vz;
   p.vx+=ax*dt*.85;p.vy+=ay*dt*.85;p.vz+=az*dt*.85;p.x+=p.vx*dt;p.y+=p.vy*dt;p.z+=p.vz*dt;
   double speed=hypot(p.vx,p.vy);p.heading=atan2(p.vy,p.vx);p.pitch=atan2(p.vz,speed);double lateral=(-sin(p.heading)*ax+cos(p.heading)*ay);double targetRoll=fmax(-.72,fmin(.72,-lateral*.9));p.roll+=(targetRoll-p.roll)*(1-exp(-dt*3.2));L[i]=p;
  }
  fieldGain=calloc(n*NSRC,sizeof(*fieldGain));for(int i=0;i<n*NSRC;i++)fieldGain[i]=1;return;
 }
 // In the room cut the listener/ear starts in deepest space. The source-only
 // prelude circles at that far anchor; the release intro (bars 4–5) then makes
 // one exponential, zero-velocity approach into the sounding wobble room.
 if(!globeMode)L[0]=(Listener){-30,-16,0,0,atan2(15.5,30)};
 for(int i=1;i<n;i++){double t=i*dt,fx=0,fy=0,energy[NSRC]={0};
  if(!globeMode&&t<=6*BAR){double u,px,py;
   if(t<=4*BAR){u=t/(4*BAR);double window=sin(M_PI*u);window*=window;double phase=TAU*(1.5*ease01(u));px=-30+3.4*window*cos(phase);py=-16+3.4*window*sin(phase);}
   else{u=(t-4*BAR)/(2*BAR);double s=ease01(u),floor=exp(-5.4),approach=(exp(-5.4*s)-floor)/(1-floor),arc=3.2*pow(sin(M_PI*u),2)*approach;px=-30*approach;py=-.5-15.5*approach+arc;}
   Listener prev=L[i-1],p={px,py,0,0,0};p.vx=(p.x-prev.x)/dt;p.vy=(p.y-prev.y)/dt;if(hypot(p.vx,p.vy)>.0001)p.heading=atan2(p.vy,p.vx);else p.heading=prev.heading;L[i]=p;continue;}
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
  // Human path phrasing: deterministic off-grid windows first hesitate, then
  // catch up. The sine-squared window keeps force and velocity derivatives
  // continuous, so spatialization bends without an audible positional jump.
  double pathGesture=0;static const double hesitateAt[4]={35.7,58.4,82.9,106.2},hesitateDur[4]={3.8,4.6,3.5,4.2};for(int h=0;h<4;h++)if(t>=hesitateAt[h]&&t<=hesitateAt[h]+hesitateDur[h]){double u=(t-hesitateAt[h])/hesitateDur[h],window=sin(M_PI*u);pathGesture-=sin(TAU*u)*window*window;break;}
  // Two irrational-feeling wander periods prevent the listener settling into
  // one polite orbit. Weak room gravity eventually draws every excursion back.
  fx+=.27*sin(TAU*t/31)+.13*sin(TAU*t/11.7)-.018*p.x;
  fy+=.24*cos(TAU*t/23)+.11*cos(TAU*t/13.1)-.018*p.y;
  double forcePhrase=1+.20*pathGesture,drag60=.995-.010*fmax(0,-pathGesture),drag=pow(drag60,60.0/CTRL);p.vx=(p.vx+fx*forcePhrase*dt)*drag;p.vy=(p.vy+fy*forcePhrase*dt)*drag;double listenerSpeed=hypot(p.vx,p.vy),speedCeiling=1.08;if(listenerSpeed>speedCeiling){double scale=speedCeiling/listenerSpeed;p.vx*=scale;p.vy*=scale;}p.x+=p.vx*dt;p.y+=p.vy*dt;
  if(hypot(p.vx,p.vy)>.01)p.heading=atan2(p.vy,p.vx);L[i]=p;
 }
 // Mutual acoustic loading. Active neighboring fields damp one another; mass
 // makes boom/gong exert more pressure. This is control-rate and interpolated
 // by the sample renderer, so it remains deterministic and inexpensive.
 fieldGain=calloc(n*NSRC,sizeof(*fieldGain));for(int i=0;i<n;i++){double t=i/(double)CTRL,en[NSRC]={0};for(int j=0;j<NE;j++)en[E[j].src]+=env(&E[j],t)*E[j].g;
  for(int s=0;s<NSRC;s++){Source a=source_at(s,t);double load=0;for(int o=0;o<NSRC;o++)if(o!=s&&en[o]>0){Source b=source_at(o,t);double dx=a.x-b.x,dy=a.y-b.y,dz=a.z-b.z;load+=en[o]*S[o].mass/(1+dx*dx+dy*dy+.35*dz*dz);}fieldGain[i*NSRC+s]=(float)(.32+.68/(1+5.5*load));}}
}
static double event_frequency_at(const Event*e,double t){double u=t-e->t;if(u<0||u>=e->dur)return 0;if(e->type==0)return e->f0*pow(e->f1/e->f0,u/e->dur);if(e->type==3||e->type==6)return e->f0;if(e->type==8){double r=u/e->dur,turnsPerSec=32*30*r*r*(1-r)*(1-r)/e->dur;return 24*turnsPerSec;}return 0;}
static void build_antenna_field(void){
 int n=(int)(DUR*CTRL)+1;antennaField=calloc(n*NSRC,sizeof(*antennaField));if(!antennaField||antennaDepth<=0)return;
 double state[NSRC]={0};Source previous[NSRC];for(int s=0;s<NSRC;s++)previous[s]=source_at(s,0);
 for(int i=0;i<n;i++){double t=i/(double)CTRL,en[NSRC]={0},fw[NSRC]={0},freq[NSRC]={0};Source body[NSRC];
  for(int j=0;j<NE;j++){Event*e=&E[j];double f=event_frequency_at(e,t);if(f<=0)continue;double a=env(e,t)*e->g;en[e->src]+=a;fw[e->src]+=a*f;}
  for(int s=0;s<NSRC;s++){if(en[s]>1e-9)freq[s]=fw[s]/en[s];body[s]=source_at(s,t);}
  Listener listener=L[i<n?i:n-1];double next[NSRC]={0};
  for(int s=0;s<NSRC;s++){double raw=0,weightSum=0,recursive=0,recursiveWeight=0;if(en[s]>1e-7&&freq[s]>0){
    for(int o=0;o<NSRC;o++)if(o!=s&&en[o]>1e-7&&freq[o]>0){double dx=body[o].x-body[s].x,dy=body[o].y-body[s].y,dz=body[o].z-body[s].z,d=sqrt(dx*dx+dy*dy+dz*dz)+1e-6,oldDx=previous[o].x-previous[s].x,oldDy=previous[o].y-previous[s].y,oldDz=previous[o].z-previous[s].z,oldD=sqrt(oldDx*oldDx+oldDy*oldDy+oldDz*oldDz),radial=(d-oldD)*CTRL;
      double oct=log2((freq[s]+1e-6)/(freq[o]+1e-6)),cross=exp(-.5*(oct/.24)*(oct/.24)),mx=(body[o].x+body[s].x)*.5-listener.x,my=(body[o].y+body[s].y)*.5-listener.y,mz=(body[o].z+body[s].z)*.5-1.6,listenerDistance=sqrt(mx*mx+my*my+mz*mz),bearing=atan2(my,mx)-listener.heading,receiverPolarization=.58+.42*fabs(cos(bearing));
      // A moving body radiates as a soft dipole aligned to its travel. Close
      // bodies couple through a reactive 1/r^3 term; distant ones transition
      // to a radiative 1/r field according to separation in wavelengths.
      double vx=(body[o].x-previous[o].x)*CTRL,vy=(body[o].y-previous[o].y)*CTRL,vz=(body[o].z-previous[o].z)*CTRL,speed=sqrt(vx*vx+vy*vy+vz*vz),travelDot=speed>1e-6?fabs((vx*(-dx)+vy*(-dy)+vz*(-dz))/(speed*d)):0,transmitLobe=.38+.62*travelDot,meanFrequency=.5*(freq[s]+freq[o]),wavelength=343.0/fmax(35,meanFrequency),zone=d/wavelength,reactive=1/(1+d*d*d),radiative=1/(1+d),fieldLaw=reactive/(1+zone)+radiative*(zone/(1+zone)),listenerFalloff=1/(1+.055*listenerDistance*listenerDistance),weight=sqrt(en[s]*en[o])*cross*fieldLaw*listenerFalloff*receiverPolarization*transmitLobe;
      double beat=fmin(14.0,fabs(freq[s]-freq[o])),phase=TAU*beat*t+TAU*d/wavelength+radial*.31;raw+=weight*sin(phase);weightSum+=weight;
      // Retarded recursion: an antenna hears the other body's already-bounded
      // control field only after acoustic propagation time, never its audio.
      int delayTicks=1+(int)fmin(5.0,round(d*CTRL/343.0)),ri=i-delayTicks;double delayed=ri>=0?antennaField[ri*NSRC+o]:0;recursive+=delayed*weight;recursiveWeight+=weight;}
   }
   double presence=fmin(1,weightSum*210),interference=weightSum>1e-10?raw/weightSum:0,memory=recursiveWeight>1e-10?recursive/recursiveWeight:0,target=tanh(1.25*interference+.20*memory)*presence,antennaSlew=1-exp(-1.0/(CTRL*.096));next[s]=state[s]+(target-state[s])*antennaSlew;antennaField[i*NSRC+s]=(float)next[s];
  }
  for(int s=0;s<NSRC;s++){state[s]=next[s];previous[s]=body[s];}
 }
}
static double field_gain(int src,double t){double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)return fieldGain[n*NSRC+src];double f=u-i;return fieldGain[i*NSRC+src]*(1-f)+fieldGain[(i+1)*NSRC+src]*f;}
static double antenna_mod(int src,double t){if(!antennaField||antennaDepth<=0)return 0;double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)return antennaField[n*NSRC+src];double f=u-i;return antennaField[i*NSRC+src]*(1-f)+antennaField[(i+1)*NSRC+src]*f;}
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
 if(cosmosMode){double z=a.z+(b.z-a.z)*f,pitch=a.pitch+(b.pitch-a.pitch)*f,roll=a.roll+(b.roll-a.roll)*f;Source sun=system_sun_at(COSMOS[src].system,t);double local=hypot(hypot(so.x-sun.x,so.y-sun.y),so.z-sun.z),dx=sun.x-x,dy=sun.y-y,dz=sun.z-z;
  double cy=cos(h),sy=sin(h),cp=cos(pitch),sp=sin(pitch),cr=cos(roll),sr=sin(roll),forward=cy*dx+sy*dy,right=-sy*dx+cy*dy,up=dz;
  double pf=cp*forward+sp*up,pu=-sp*forward+cp*up,rr=cr*right+sr*pu,uu=-sr*right+cr*pu;*dist=local+sqrt(dx*dx+dy*dy+dz*dz);*az=atan2(rr,pf);*el=atan2(uu,hypot(pf,rr));return;}
 double dx=so.x-x,dy=so.y-y,dz=so.z-1.6,horiz=hypot(dx,dy);*dist=hypot(horiz,dz);*az=atan2(dy,dx)-h;*el=atan2(dz,horiz);
}
static double tornado_directional_pickup(int src,double t){
 // Instantaneous tangent of the actual moving body supplies its forward axis.
 // A cardioid receiver catches the rotor strongly as it points toward the
 // listener and softly from behind, producing real directional sweep pulses.
 if(!L||t<32.6||t>38.8)return 1;
 double dt=.5/CTRL,t0=fmax(32.6,t-dt),t1=fmin(38.8,t+dt);
 Source p0=source_at(src,t0),p1=source_at(src,t1),p=source_at(src,t);
 double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;
 Listener a=L[i],b=L[i+1];double lx=a.x+(b.x-a.x)*f,ly=a.y+(b.y-a.y)*f;
 double vx=p1.x-p0.x,vy=p1.y-p0.y,rx=lx-p.x,ry=ly-p.y;
 double vn=hypot(vx,vy),rn=hypot(rx,ry);if(vn<1e-8||rn<1e-8)return 1;
 double facing=fmax(-1,fmin(1,(vx*rx+vy*ry)/(vn*rn)));
 double cardioid=.5+.5*facing;
 return .10+.90*cardioid*cardioid;
}
static void propagation(int src,double t,double *wall,double *cutoff,double *wind,double *rain){double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;Listener a=L[i],b=L[i+1];double x=a.x+(b.x-a.x)*f,y=a.y+(b.y-a.y)*f;Source so=source_at(src,t);double dx=so.x-x,dy=so.y-y,d=hypot(dx,dy);*wall=1;*cutoff=20000;if(duetMode||cosmosMode){*wind=0;*rain=1;return;}
 // Brick barrier at x=1.2, y=-3..2.5, height 2.4. Only an actual ray
 // intersection occludes; high sources and paths around its ends remain clear.
 if((x-1.2)*(so.x-1.2)<0){double q=(1.2-x)/(so.x-x),iy=y+(so.y-y)*q,iz=1.6+(so.z-1.6)*q;if(iy>-3&&iy<2.5&&iz<2.4){*wall=.46;*cutoff=1450;}}
 double wx=.7*cos(t*.11),wy=.7*sin(t*.083+.8);*wind=.0045*(wx*dx+wy*dy)/(d+1);*rain=1-.075*fmin(1,d/8.0)*fabs(sin(t*19.7+src*2.13));}
static double whistle_frac_read(const float*buf,int size,int write,double delay){double rd=write-delay;while(rd<0)rd+=size;int a=(int)rd,b=(a+1)%size;double f=rd-a;return buf[a]*(1-f)+buf[b]*f;}
static void render(void){long n=(long)(DUR*SR);busL=calloc(n,4);busR=calloc(n,4);meterL=calloc(NFRAMES*NSRC,sizeof(*meterL));meterR=calloc(NFRAMES*NSRC,sizeof(*meterR));sourceWave=calloc(NFRAMES*NSRC*WAVE_POINTS,sizeof(*sourceWave));build_antenna_field();
 for(int j=0;j<NE;j++){Event*e=&E[j];long s0=(long)(e->t*SR),nn=(long)(e->dur*SR);double ph=0,lp=0,hpLP=0,envLP=0,saz=0,sel=0,sd=0,swall=1,scutoff=20000,spatialGainState=0,mode1[3]={0},mode2[3]={0},whistleBreath=0,whistleVibrato=0,whistleLP=0,whistleHPX=0,whistleHPY=0;float whistleBore[2048]={0},whistleJet[512]={0};int spatialInit=0,propInit=0,spatialGainInit=0,whistleBoreW=0,whistleJetW=0;uint32_t rs=e->seed;ACHrtf hs;memset(&hs,0,sizeof hs);
  for(long k=0;k<nn&&s0+k<n;k++){double t=(s0+k)/(double)SR,u=k/(double)SR,a=env(e,t),az,el,d,wall,cutoff,wind,rain;int tornadoActive=t>=32.6&&t<=38.8;spatial_params(e->src,t,&az,&el,&d);if(!spatialInit){saz=az;sel=el;sd=d;spatialInit=1;}else{double motionTau=e->type==8?.009:(tornadoActive?.022:((t>=61.5&&t<=72.5)?.18:(t>=50&&t<=78?.24:.18))),smooth=1-exp(-1.0/(SR*motionTau)),da=atan2(sin(az-saz),cos(az-saz));saz+=da*smooth;sel+=(el-sel)*smooth;sd+=(d-sd)*smooth;}az=saz;el=sel;d=sd;propagation(e->src,t,&wall,&cutoff,&wind,&rain);
   // A ray crossing the brick wall must not switch gain and bandwidth as a
   // boolean. During the opening centrifuge that crossing happens many times
   // per second; a hard switch becomes zipper clicks locked to the twist.
   // Smooth both coefficients, more slowly during the 96-turn deceleration.
   if(!propInit){swall=wall;scutoff=cutoff;propInit=1;}else{double tau=t<4*BAR?.045:.022,pc=1-exp(-1.0/(SR*tau));swall+=(wall-swall)*pc;scutoff+=(cutoff-scutoff)*pc;}wall=swall;cutoff=scutoff;
   double spaceD=d+(globeMode?(1-a)*18.0:0),microA=fmin(1,u/.006),microR=fmin(1,(e->dur-u)/.028),clickGate=sin(microA*M_PI/2)*sin(microA*M_PI/2)*sin(microR*M_PI/2)*sin(microR*M_PI/2),gravity=kick_gravity(t);int tonal=e->src!=1&&(e->type==0||e->type==3||e->type==6);if(tonal)cutoff=fmin(cutoff,18500-4500*fmax(0,gravity));
   double v,currentF=0;if(e->type==0||e->type==3){double base=e->type==3?e->f0:(cosmosMode?cosmos_frequency(e->src,t):(duetMode?duet_frequency(e->src,t):e->f0*pow(e->f1/e->f0,u/e->dur))),f=base*(1+wind*spatialWet);currentF=f;ph+=TAU*f/SR;v=sin(ph);}
   else if(e->type==6){double wiggle=1+.0032*sin(TAU*u*5.1+(e->seed&255)*.01)+.0018*sin(TAU*u*7.73);ph+=TAU*e->f0*wiggle/SR;double glint=exp(-u/(e->dur*.48)),seedPhase=(e->seed&1023)*TAU/1024.0,fm=(.045+.31*glint)*sin(ph*e->f1+seedPhase),bellEnv;
    if(u<e->atk)bellEnv=smooth01(u/e->atk);else if(u<e->atk+.72)bellEnv=1-.27*smooth01((u-e->atk)/.72);else if(e->dur-u<e->rel)bellEnv=.73*smooth01((e->dur-u)/e->rel);else bellEnv=.73;
    double bp=ph+fm,triangle=0,saw=0,square=0;
    // Every destination is additive and Nyquist-limited. The old asin(sin())
    // triangle carried infinite harmonics; its upper partials folded back as
    // the top-end fizz that survived otherwise linear mastering.
    int maxH=(int)floor((SR*.45)/e->f0);if(maxH>15)maxH=15;
    for(int h=1;h<=maxH;h+=2){double sign=((h-1)/2)&1?-1:1;triangle+=sign*sin(bp*h)/(h*h);}
    triangle*=8/(M_PI*M_PI);
    for(int h=1;h<=5&&h<=maxH;h++)saw+=sin(bp*h)/h;
    for(int h=1;h<=7&&h<=maxH;h+=2)square+=sin(bp*h)/h;
    saw*=.58;square*=.78;double shape=((e->seed>>9)&1)?square:saw,morphWindow=sin(M_PI*fmax(0,fmin(1,u/e->dur))),morph=smooth01(morphWindow*morphWindow)*(.18+.22*((e->seed>>13)&3)/3.0);if(((e->seed>>7)&3)==0)morph*=.28;
    v=(triangle*(1-morph)+shape*morph+.035*glint*sin(ph*3.917+seedPhase*.37))*bellEnv;}
   else if(e->type==7){
    // The same Cook/STK digital waveguide signal path as native AC-OS:
    // DC breath -> jet delay -> cubic limit cycle -> DC block -> bore loop.
    double breathTarget=.18+.82*sqrt(a),slew=a>whistleBreath?.012:.003;whistleBreath+=(breathTarget-whistleBreath)*slew;
    whistleVibrato+=5.0/SR;if(whistleVibrato>=1)whistleVibrato-=1;
    rs^=rs<<13;rs^=rs>>17;rs^=rs<<5;double white=(rs/(double)UINT32_MAX)*2-1,breath=whistleBreath*(1+(.08+.05*(1-a))*white+.03*sin(TAU*whistleVibrato));
    double freq=fmax(30,fmin(SR*.20,e->f0)),boreDelay=SR/(freq*(2.0/3.0)),jetDelay;if(boreDelay>2046)boreDelay=2046;jetDelay=boreDelay*.32;if(jetDelay>510)jetDelay=510;
    double boreOut=whistle_frac_read(whistleBore,2048,whistleBoreW,boreDelay);whistleLP=.35*(-boreOut)+.65*whistleLP;double temp=whistleLP,pd=breath-.5*temp;
    whistleJet[whistleJetW]=(float)pd;whistleJetW=(whistleJetW+1)%512;pd=whistle_frac_read(whistleJet,512,whistleJetW,jetDelay);pd=pd*(pd*pd-1);pd=fmax(-1,fmin(1,pd));
    double y=pd-whistleHPX+.995*whistleHPY;whistleHPX=pd;whistleHPY=y;double intoBore=y+.5*temp;whistleBore[whistleBoreW]=(float)intoBore;whistleBoreW=(whistleBoreW+1)%2048;v=.3*intoBore*sqrt(a);
   }
   else if(e->type==8){
    // Twenty-four aerodynamic lobes turn the 0..9.68 revolutions/second body
    // motion into a clearly pitched 0..232 Hz whirr. The phase integrates the
    // exact instantaneous angular velocity, so there is no stepped pitch ramp.
    double r=fmax(0,fmin(1,u/e->dur)),spinShape=16*r*r*(1-r)*(1-r);
    double turnsPerSec=32*30*r*r*(1-r)*(1-r)/e->dur,bladeHz=24*turnsPerSec;
    ph+=TAU*bladeHz/SR;
    rs=rs*1664525u+1013904223u;double white=((rs>>8)/8388608.0)-1;
    double airC=exp(-TAU*1450/SR),bodyC=exp(-TAU*95/SR);
    lp=(1-airC)*white+airC*lp;hpLP=(1-bodyC)*lp+bodyC*hpLP;
    double airBand=lp-hpLP;
    v=spinShape*(.68*sin(ph)+.21*sin(2*ph+.19)+.075*sin(3*ph+.43)+.12*airBand);
   }
   else {rs=rs*1664525u+1013904223u;double w=((rs>>8)/8388608.0)-1;double ca=exp(-TAU*e->f0/SR),cb=exp(-TAU*e->f1/SR);lp=(1-ca)*w+ca*lp;double hi=w-lp;hpLP=(1-cb)*hi+cb*hpLP;v=hpLP;}
   double radioField=tonal?antenna_mod(e->src,t):0;
   if(e->type==0&&antennaDepth>0){
    // Geometry controls the depth of two low radio-rate modulators. Their
    // close sidebands add an intentional receiver shimmer while the carrier
    // remains a mathematically clean sine. A minute, Nyquist-gated 2nd/3rd
    // harmonic tint lets the broadcast speak on small transducers too.
    double activity=.22+.78*fabs(radioField),rate=17.0+1.71*e->src+3.2*activity,phaseSeed=(e->seed&1023)*TAU/1024.0,am=antennaDepth*activity*(.62*sin(TAU*rate*t+phaseSeed)+.25*sin(TAU*(rate*.503)*t+phaseSeed*.37));v*=1+am;
    double color=antennaDepth*(.020+.080*fabs(radioField));if(currentF*2<SR*.42)v+=color*.72*sin(2*ph+radioField*.65+phaseSeed*.19);if(currentF*3<SR*.42)v+=color*.28*sin(3*ph-radioField*.42+phaseSeed*.31);
   }
   if(cosmosMode&&e->type!=2&&e->type!=3){double modal=0;ACMeshAcoustics*ma=&BODY_ACOUSTICS[e->src];ACMaterial mm=BODY_MATERIAL[e->src];for(int m=0;m<3;m++){double mf=fmax(45,fmin(12000,ma->mode[m])),r=exp(-M_PI*mf*fmax(.004,mm.loss)/SR),y=2*r*cos(TAU*mf/SR)*mode1[m]-r*r*mode2[m]+(1-r)*v;mode2[m]=mode1[m];mode1[m]=y;modal+=y;}v=.68*v+.32*(modal/3.0);}
   // Keep the filter state alive even while the path is nominally open. This
   // prevents a stale filter accumulator from jumping in at the next crossing.
   if(k==0)envLP=v;{double c=exp(-TAU*fmin(20000,cutoff)/SR);envLP=(1-c)*v+c*envLP;v=envLP;}double ampEnv=(e->type==6||e->type==7)?1:sqrt(a),tonePump=tonal?1-.18*fmax(0,gravity)+.10*fmax(0,-gravity):1,antenna=tonal?1+antennaDepth*radioField:1,directional=tornadoActive?tornado_directional_pickup(e->src,t):1,spatialTarget=antenna*tonePump*wall*rain*directional*field_gain(e->src,t)*(cosmosMode?cosmos_plane_gain(e->src,t,az,el):1);if(!spatialGainInit){spatialGainState=spatialTarget;spatialGainInit=1;}else{double receiverTau=e->type==8?.007:(tornadoActive?.018:((t>=61.5&&t<=72.5)?.28:(t>=50&&t<=78?.36:.24))),receiverSlew=1-exp(-1.0/(SR*receiverTau));spatialGainState+=(spatialTarget-spatialGainState)*receiverSlew;}v*=spatialGainState*ampEnv*e->g*clickGate;float hl,hr;ac_hrtf_process(&hs,(float)v,az,el,spaceD,&hl,&hr);
   double dryPan=fmax(-.62,fmin(.62,S[e->src].x/6.0)),range=(globeMode||cosmosMode)?.008+.992/(1+.32*spaceD*spaceD):1,dl=sqrt((1-dryPan)*.5)*range,dr=sqrt((1+dryPan)*.5)*range;
   // Keep the far release opening genuinely in the acoustic field instead of
   // letting the fixed studio panorama collapse its distance illusion.
   if(!globeMode&&!cosmosMode&&t>=4*BAR&&t<6*BAR){double arrive=ease01((t-4*BAR)/(2*BAR)),depth=.055+.945*arrive;dl*=depth;dr*=depth;}
   // Preserve listener-relative position with a clean equal-power field, then
   // blend in only enough HRTF detail for head/elevation cues. This keeps pure
   // oscillators pure while retaining unmistakable per-body motion.
   // Equal-power listener-relative pan carries the position. A restrained
   // procedural HRTF layer adds ITD/head/elevation cues without letting its
   // moving pinna comb become the audible rubber-squeegee gesture.
   double listenerPan=sin(az),listenerNear=.012+.988/(1+.18*spaceD*spaceD),cleanL=v*sqrt((1-listenerPan)*.5)*listenerNear,cleanR=v*sqrt((1+listenerPan)*.5)*listenerNear,hrtfDetail=.24,spaceL=cleanL*(1-hrtfDetail)+hl*hrtfDetail,spaceR=cleanR*(1-hrtfDetail)+hr*hrtfDetail;
   double dg=cos(spatialWet*M_PI*.5),wg=sin(spatialWet*M_PI*.5),cl=v*dl*dg+spaceL*wg,cr=v*dr*dg+spaceR*wg;
   // Retarded reception: radio energy is written when it reaches the listener,
   // not when the emitter produced it. Distance therefore becomes real delay.
   double arrival=s0+k+spaceD*SR/343.0;long at=(long)floor(arrival);double af=arrival-at;if(at>=0&&at<n){if(at>=1&&at+2<n){double f2=af*af,f3=f2*af,w0=(1-3*af+3*f2-f3)/6,w1=(4-6*f2+3*f3)/6,w2=(1+3*af+3*f2-3*f3)/6,w3=f3/6;busL[at-1]+=(float)(cl*w0);busR[at-1]+=(float)(cr*w0);busL[at]+=(float)(cl*w1);busR[at]+=(float)(cr*w1);busL[at+1]+=(float)(cl*w2);busR[at+1]+=(float)(cr*w2);busL[at+2]+=(float)(cl*w3);busR[at+2]+=(float)(cr*w3);}else{busL[at]+=(float)(cl*(1-af));busR[at]+=(float)(cr*(1-af));if(at+1<n){busL[at+1]+=(float)(cl*af);busR[at+1]+=(float)(cr*af);}}
    // Per-emitter air/room tail; unlike the later master reflections this delay
    // remains attached to the originating voice and its stereo observation.
    long echo=at+(long)((.075+.011*e->src)*SR);if(!duetMode&&!cosmosMode&&echo<n){busL[echo]+=(float)(cl*.055);busR[echo]+=(float)(cr*.055);}
    int fr=(int)(at*FPS/(double)SR);if(fr>=0&&fr<NFRAMES){int mi=fr*NSRC+e->src;meterL[mi]+=cl*cl;meterR[mi]+=cr*cr;long frameStart=(long)fr*SR/FPS;int wi=(int)((at-frameStart)*WAVE_POINTS*FPS/SR);if(wi<0)wi=0;if(wi>=WAVE_POINTS)wi=WAVE_POINTS-1;sourceWave[(mi*WAVE_POINTS)+wi]+=(float)((cl+cr)*.5*(WAVE_POINTS*FPS/(double)SR));}}
  }
 }
 // Cross-room early reflections.
 // Four FIR room images.  Read every return from one immutable snapshot of
 // the direct/per-voice field: reading the destination bus in-place turns each
 // intended tap into a recursive comb lattice (the former creak/fuzz).
 int ds[]={3408,5424,8688,14400};double dg[]={.075,.048,.03,.018};if(!duetMode&&!cosmosMode){float*roomL=malloc(n*sizeof(*roomL)),*roomR=malloc(n*sizeof(*roomR));if(roomL&&roomR){memcpy(roomL,busL,n*sizeof(*roomL));memcpy(roomR,busR,n*sizeof(*roomR));for(int q=0;q<4;q++)for(long i=ds[q];i<n;i++){float l=roomL[i-ds[q]],r=roomR[i-ds[q]];double t=i/(double)SR,room=.22+.78*spatialWet,farRoom=(t>=4*BAR&&t<6*BAR)?1+2.2*(1-ease01((t-4*BAR)/(2*BAR))):1;busL[i]+=r*dg[q]*room*farRoom;busR[i]+=l*dg[q]*room*farRoom;}free(roomL);free(roomR);}else{free(roomL);free(roomR);}}
 double pk=0;for(long i=0;i<n;i++){double a=fmax(fabs(busL[i]),fabs(busR[i]));if(a>pk)pk=a;}double g=pk?.88/pk:1;
 for(long i=0;i<n;i++){double fo=i>n-SR?((n-i)/(double)SR):1;busL[i]*=g*fo;busR[i]*=g*fo;}
}
static int wav(const char*p){FILE*f=fopen(p,"wb");if(!f)return 0;long n=(long)(DUR*SR);uint32_t ds=n*8,sz=36+ds,sr=SR,br=SR*8,fs=16;uint16_t fm=3,ch=2,ba=8,bi=32;
 fwrite("RIFF",1,4,f);fwrite(&sz,4,1,f);fwrite("WAVEfmt ",1,8,f);fwrite(&fs,4,1,f);fwrite(&fm,2,1,f);fwrite(&ch,2,1,f);fwrite(&sr,4,1,f);fwrite(&br,4,1,f);fwrite(&ba,2,1,f);fwrite(&bi,2,1,f);fwrite("data",1,4,f);fwrite(&ds,4,1,f);for(long i=0;i<n;i++){fwrite(&busL[i],4,1,f);fwrite(&busR[i],4,1,f);}fclose(f);return 1;}
// Decode an accepted external master back onto the visualization bus.  This is
// intentionally after the raw C bed has been written: the windshield and its
// little scope windows then show the exact stereo file muxed into the video,
// including Jeffrey's choir and mastering, while source telemetry stays tied
// to the individual C emitters that launched it.
static int load_visual_mix(const char*path){char cmd[4096];snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -loglevel error -i '%s' -f f32le -ar %d -ac 2 -",path,SR);FILE*f=popen(cmd,"r");if(!f)return 0;long n=(long)(DUR*SR),i=0;float pair[2];while(i<n&&fread(pair,sizeof(float),2,f)==2){busL[i]=pair[0];busR[i]=pair[1];i++;}int status=pclose(f);long got=i;for(;i<n;i++)busL[i]=busR[i]=0;return status==0&&got==n;}
static void dot(unsigned char*p,int x,int y,int r,uint32_t c){for(int yy=-r;yy<=r;yy++)for(int xx=-r;xx<=r;xx++)if(xx*xx+yy*yy<=r*r){int X=x+xx,Y=y+yy;if(X>=0&&X<W&&Y>=0&&Y<H){int o=(Y*W+X)*3;p[o]=c>>16;p[o+1]=c>>8;p[o+2]=c;}}}
static void glow(unsigned char*p,int x,int y,int r,uint32_t c,double strength){int rr=c>>16,gg=(c>>8)&255,bb=c&255;for(int yy=-r;yy<=r;yy++)for(int xx=-r;xx<=r;xx++){double d=sqrt(xx*xx+yy*yy)/(double)r;if(d>1)continue;int X=x+xx,Y=y+yy;if(X<0||X>=W||Y<0||Y>=H)continue;double a=strength*(1-d)*(1-d);int o=(Y*W+X)*3;p[o]=p[o]*(1-a)+rr*a;p[o+1]=p[o+1]*(1-a)+gg*a;p[o+2]=p[o+2]*(1-a)+bb*a;}}
typedef struct{double x,y,z;} V3;typedef struct{int x,y;double z;int ok;} P2;
static double projectionRoll=0;
static V3 sub3(V3 a,V3 b){return(V3){a.x-b.x,a.y-b.y,a.z-b.z};}static double dot3(V3 a,V3 b){return a.x*b.x+a.y*b.y+a.z*b.z;}
static V3 cross3(V3 a,V3 b){return(V3){a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x};}
static V3 norm3(V3 a){double n=sqrt(dot3(a,a));return n?(V3){a.x/n,a.y/n,a.z/n}:a;}
static P2 project(V3 p,V3 cam,V3 target){V3 f=norm3(sub3(target,cam)),r=norm3(cross3(f,(V3){0,0,1})),u=cross3(r,f);if(projectionRoll){double c=cos(projectionRoll),s=sin(projectionRoll);V3 rr={r.x*c+u.x*s,r.y*c+u.y*s,r.z*c+u.z*s},uu={u.x*c-r.x*s,u.y*c-r.y*s,u.z*c-r.z*s};r=rr;u=uu;}V3 q=sub3(p,cam);double z=dot3(q,f);if(z<.15)return(P2){0,0,z,0};double focal=W*.694;return(P2){(int)(W*.5+dot3(q,r)*focal/z),(int)(H*.54-dot3(q,u)*focal/z),z,1};}
static void plot2(unsigned char*p,int x,int y,uint32_t c,double alpha){if(x<0||x>=W||y<0||y>=H)return;int o=(y*W+x)*3,rr=c>>16,gg=(c>>8)&255,bb=c&255;p[o]=p[o]*(1-alpha)+rr*alpha;p[o+1]=p[o+1]*(1-alpha)+gg*alpha;p[o+2]=p[o+2]*(1-alpha)+bb*alpha;}
// Native AC uses this exact Bresenham error walk. Stamp one 3x3 source cell at
// each step so the later 720 -> 240 nearest-neighbor pass preserves every step.
static void line2(unsigned char*p,int x0,int y0,int x1,int y1,uint32_t c,double alpha){int dx=abs(x1-x0),sx=x0<x1?1:-1,dy=-abs(y1-y0),sy=y0<y1?1:-1,er=dx+dy;for(;;){for(int yy=-1;yy<=1;yy++)for(int xx=-1;xx<=1;xx++)plot2(p,x0+xx,y0+yy,c,alpha);if(x0==x1&&y0==y1)break;int e2=2*er;if(e2>=dy){er+=dy;x0+=sx;}if(e2<=dx){er+=dx;y0+=sy;}}}
static void fill2(unsigned char*p,int x0,int y0,int x1,int y1,uint32_t c,double a){int rr=c>>16,gg=(c>>8)&255,bb=c&255;if(x0<0)x0=0;if(y0<0)y0=0;if(x1>W)x1=W;if(y1>H)y1=H;for(int y=y0;y<y1;y++)for(int x=x0;x<x1;x++){int o=(y*W+x)*3;p[o]=p[o]*(1-a)+rr*a;p[o+1]=p[o+1]*(1-a)+gg*a;p[o+2]=p[o+2]*(1-a)+bb*a;}}
static void tri2(unsigned char*p,int ax,int ay,int bx,int by,int cx,int cy,uint32_t c,double a){int minx=fmax(0,fmin(ax,fmin(bx,cx))),maxx=fmin(W-1,fmax(ax,fmax(bx,cx))),miny=fmax(0,fmin(ay,fmin(by,cy))),maxy=fmin(H-1,fmax(ay,fmax(by,cy)));double den=(by-cy)*(ax-cx)+(cx-bx)*(ay-cy);if(fabs(den)<1)return;for(int y=miny;y<=maxy;y++)for(int x=minx;x<=maxx;x++){double u=((by-cy)*(x-cx)+(cx-bx)*(y-cy))/den,v=((cy-ay)*(x-cx)+(ax-cx)*(y-cy))/den,w=1-u-v;if(u>=0&&v>=0&&w>=0){int o=(y*W+x)*3,rr=c>>16,gg=(c>>8)&255,bb=c&255;p[o]=p[o]*(1-a)+rr*a;p[o+1]=p[o+1]*(1-a)+gg*a;p[o+2]=p[o+2]*(1-a)+bb*a;}}}
static void ellipse_points(unsigned char*p,int cx,int cy,int x,int y,uint32_t c,double a){for(int yy=-1;yy<=1;yy++)for(int xx=-1;xx<=1;xx++){plot2(p,cx+x+xx,cy+y+yy,c,a);plot2(p,cx-x+xx,cy+y+yy,c,a);plot2(p,cx+x+xx,cy-y+yy,c,a);plot2(p,cx-x+xx,cy-y+yy,c,a);}}
// Integer midpoint ellipse: stable, symmetric staircases without trig facets.
static void ellipse2(unsigned char*p,int cx,int cy,int rx,int ry,uint32_t c,double a){long x=0,y=ry,rx2=(long)rx*rx,ry2=(long)ry*ry,dx=0,dy=2*rx2*y,d1=ry2-rx2*ry+rx2/4;while(dx<dy){ellipse_points(p,cx,cy,(int)x,(int)y,c,a);if(d1<0){x++;dx+=2*ry2;d1+=dx+ry2;}else{x++;y--;dx+=2*ry2;dy-=2*rx2;d1+=dx-dy+ry2;}}long d2=ry2*(x*x+x)+ry2/4+rx2*(y-1)*(y-1)-rx2*ry2;while(y>=0){ellipse_points(p,cx,cy,(int)x,(int)y,c,a);if(d2>0){y--;dy-=2*rx2;d2+=rx2-dy;}else{y--;x++;dx+=2*ry2;dy-=2*rx2;d2+=dx-dy+rx2;}}}
static const char*glyph3(char c){switch(c){case'A':return"010101111101101";case'B':return"110101110101110";case'C':return"011100100100011";case'D':return"110101101101110";case'E':return"111100110100111";case'F':return"111100110100100";case'G':return"011100101101011";case'H':return"101101111101101";case'I':return"111010010010111";case'J':return"001001001101010";case'K':return"101101110101101";case'L':return"100100100100111";case'M':return"101111111101101";case'N':return"101111111111101";case'O':return"010101101101010";case'P':return"110101110100100";case'Q':return"010101101111011";case'R':return"110101110101101";case'S':return"011100010001110";case'T':return"111010010010010";case'U':return"101101101101111";case'V':return"101101101101010";case'W':return"101101111111101";case'X':return"101101010101101";case'Y':return"101101010010010";case'Z':return"111001010100111";case'0':return"111101101101111";case'1':return"010110010010111";case'2':return"110001111100111";case'3':return"110001111001110";case'4':return"101101111001001";case'5':return"111100110001110";case'6':return"011100111101111";case'7':return"111001010010010";case'8':return"111101111101111";case'9':return"111101111001110";case'-':return"000000111000000";case'>':return"100010001010100";case'.':return"000000000000010";default:return"000000000000000";}}
static void text3(unsigned char*p,int x,int y,const char*s,int scale,uint32_t c){for(;*s;s++,x+=4*scale){const char*g=glyph3(*s>='a'&&*s<='z'?*s-32:*s);for(int yy=0;yy<5;yy++)for(int xx=0;xx<3;xx++)if(g[yy*3+xx]=='1')fill2(p,x+xx*scale,y+yy*scale,x+(xx+1)*scale,y+(yy+1)*scale,c,1);}}
static void line3(unsigned char*p,V3 a,V3 b,V3 cam,V3 target,uint32_t c,double alpha){P2 x=project(a,cam,target),y=project(b,cam,target);if(x.ok&&y.ok)line2(p,x.x,x.y,y.x,y.y,c,alpha);}
static V3 globe_point(double x,double y,double altitude);
static uint32_t shade_color(uint32_t c,double k){int r=(c>>16)&255,g=(c>>8)&255,b=c&255;r=(int)fmin(255,r*k);g=(int)fmin(255,g*k);b=(int)fmin(255,b*k);return(uint32_t)(r<<16|g<<8|b);}
static void flat_tri3(unsigned char*p,V3 a,V3 b,V3 c,V3 cam,V3 target,uint32_t color,double alpha){V3 n=cross3(sub3(b,a),sub3(c,a));if(dot3(n,sub3(cam,a))<=0)return;P2 pa=project(a,cam,target),pb=project(b,cam,target),pc=project(c,cam,target);if(pa.ok&&pb.ok&&pc.ok)tri2(p,pa.x,pa.y,pb.x,pb.y,pc.x,pc.y,color,alpha);}
static void draw_prompt_rock_alpha(unsigned char*p,const ACRock*r,V3 center,double scale,double spin,V3 cam,V3 target,uint32_t color,double alpha){V3 vv[42];double c=cos(spin),s=sin(spin),ct=cos(spin*.63),st=sin(spin*.63);for(int i=0;i<r->nv;i++){double x=r->v[i].x*c-r->v[i].y*s,y=r->v[i].x*s+r->v[i].y*c,z=r->v[i].z,yy=y*ct-z*st,zz=y*st+z*ct;vv[i]=(V3){center.x+x*scale,center.y+yy*scale,center.z+zz*scale};}V3 light=norm3((V3){-.35,-.55,.76});for(int i=0;i<r->nf;i++){ACRockFace f=r->f[i];V3 n=norm3(cross3(sub3(vv[f.b],vv[f.a]),sub3(vv[f.c],vv[f.a])));if(dot3(n,sub3(cam,vv[f.a]))<=0)continue;double nd=dot3(n,light),lit=.18+.72*fmax(0,nd)+.18*fmax(0,-nd);uint32_t facet=shade_color(color,lit);flat_tri3(p,vv[f.a],vv[f.b],vv[f.c],cam,target,facet,.88*alpha);}}
static void draw_prompt_rock(unsigned char*p,const ACRock*r,V3 center,double scale,double spin,V3 cam,V3 target,uint32_t color){draw_prompt_rock_alpha(p,r,center,scale,spin,cam,target,color,1);}
static void draw_prompt_rock_facing(unsigned char*p,const ACRock*r,V3 center,V3 receiver,double scale,double phase,V3 cam,V3 target,uint32_t color){V3 f=norm3(sub3(receiver,center)),up=fabs(f.z)>.94?(V3){0,1,0}:(V3){0,0,1},right=norm3(cross3(up,f)),vert=cross3(f,right);double rockx=.16*sin(phase*.37),rocky=.12*cos(phase*.29),push=1+.12*sin(phase),cx=cos(rockx),sx=sin(rockx),cy=cos(rocky),sy=sin(rocky);V3 vv[42];for(int i=0;i<r->nv;i++){double x=r->v[i].x,y=r->v[i].y,z=r->v[i].z*push,yy=y*cx-z*sx,zz=y*sx+z*cx,xx=x*cy+zz*sy;zz=-x*sy+zz*cy;vv[i]=(V3){center.x+scale*(right.x*xx+vert.x*yy+f.x*zz),center.y+scale*(right.y*xx+vert.y*yy+f.y*zz),center.z+scale*(right.z*xx+vert.z*yy+f.z*zz)};}V3 light=norm3((V3){-.35,-.55,.76});for(int i=0;i<r->nf;i++){ACRockFace q=r->f[i];V3 n=norm3(cross3(sub3(vv[q.b],vv[q.a]),sub3(vv[q.c],vv[q.a])));if(dot3(n,sub3(cam,vv[q.a]))<=0)continue;double lit=.18+.72*fmax(0,dot3(n,light))+.18*fmax(0,-dot3(n,light));flat_tri3(p,vv[q.a],vv[q.b],vv[q.c],cam,target,shade_color(color,lit),.9);}}
static V3 pyramid_shard_point(ACRockV v,ACRockV center,Source hit,double age,int index,double scale){double n=sqrt(center.x*center.x+center.y*center.y+center.z*center.z)+1e-6,speed=1.7+fmod(index*2.371,3.6),omega=5.2+fmod(index*.731,3.1),travel=speed/omega*sin(omega*age)*exp(-age*.72),spin=age*(2.4+fmod(index*1.173,5.2)),c=cos(spin),s=sin(spin),ct=cos(spin*.71),st=sin(spin*.71);double x=(v.x-center.x)*c-(v.y-center.y)*s,y=(v.x-center.x)*s+(v.y-center.y)*c,z=v.z-center.z,yy=y*ct-z*st,zz=y*st+z*ct;double px=hit.x+center.x*scale+center.x/n*travel+x*scale,py=hit.y+center.y*scale+center.y/n*travel+yy*scale,pz=hit.z+center.z*scale+center.z/n*travel+zz*scale-.18*age*age;return globe_point(px,py,pz);}
static V3 capture_shard(V3 v,Source body,double age,int index,int corner){double u=fmax(0,fmin(1,(age-.65)/3.2));u=u*u*(3-2*u);double a=age*(.75+.07*(index%7))+index*2.399,r=.34+.045*(index%5),zoff=(corner-1.5)*.075;V3 dst=globe_point(body.x+cos(a)*r,body.y+sin(a)*r,body.z+zoff+.10*sin(a*1.7));return(V3){v.x+(dst.x-v.x)*u,v.y+(dst.y-v.y)*u,v.z+(dst.z-v.z)*u};}
static void circle3(unsigned char*p,V3 c,double r,V3 cam,V3 target,uint32_t color,double alpha){V3 a={c.x+r,c.y,c.z};for(int i=1;i<=64;i++){double q=TAU*i/64;V3 b={c.x+cos(q)*r,c.y+sin(q)*r,c.z};line3(p,a,b,cam,target,color,alpha);a=b;}}
static void marble(unsigned char*p,int cx,int cy,int r,uint32_t c,double energy){
 int cr=c>>16,cg=(c>>8)&255,cb=c&255;for(int y=-r;y<=r;y++)for(int x=-r;x<=r;x++){double nx=x/(double)r,ny=y/(double)r,r2=nx*nx+ny*ny;if(r2>1)continue;double nz=sqrt(1-r2),lx=-.42,ly=-.55,lz=.72,nd=fmax(0,nx*lx+ny*ly+nz*lz),rim=pow(1-nz,2.2),hx=nx+.26,hy=ny+.34,hl=hypot(hx,hy),spec=pow(fmax(0,1-hl*2.4),18);double shade=.18+.66*nd+.34*rim+1.15*spec+fmin(.45,energy*10);int X=cx+x,Y=cy+y;if(X<0||X>=W||Y<0||Y>=H)continue;int o=(Y*W+X)*3;p[o]=(unsigned char)fmin(255,cr*shade+spec*110);p[o+1]=(unsigned char)fmin(255,cg*shade+spec*110);p[o+2]=(unsigned char)fmin(255,cb*shade+spec*110);}}
static void source_meter(int fr,int src,double *l,double *r){
 // A short symmetric window removes 24 fps sparkle without inventing energy.
 double sl=0,sr=0;int count=0;for(int q=-2;q<=2;q++){int f=fr+q;if(f<0||f>=NFRAMES)continue;double w=q?1.0/(1+abs(q)):1.0;sl+=meterL[f*NSRC+src]*w;sr+=meterR[f*NSRC+src]*w;count+=(int)(SR/(double)FPS*w);}
 *l=count?sqrt(sl/count):0;*r=count?sqrt(sr/count):0;
}
// A source is drawn as a spectrum with volume, not as a status dot.  Frequency
// climbs through twelve logarithmic slices; current event energy controls each
// slice's cross-section.  Ribs connect the slices into one translucent body.
#define SPEC_BANDS 12
#define SPEC_RIBS 10
static void source_spectrum(int src,double t,double band[SPEC_BANDS]){
 for(int b=0;b<SPEC_BANDS;b++)band[b]=0;
 double loAll=45,hiAll=14000,span=log(hiAll/loAll);
 for(int j=0;j<NE;j++){Event*e=&E[j];if(e->src!=src)continue;double a=env(e,t)*e->g;if(a<=0)continue;double u=fmax(0,fmin(1,(t-e->t)/e->dur)),f0=fmin(e->f0,e->f1),f1=fmax(e->f0,e->f1);
  if(e->type==1||e->type==2||e->type==4||e->type==5){for(int b=0;b<SPEC_BANDS;b++){double f=loAll*exp(span*b/(SPEC_BANDS-1.0));if(f>=f0*.82&&f<=f1*1.18)band[b]+=a*(.55+.45*sin(M_PI*(b+.5)/SPEC_BANDS));}}
  else{double f=e->type==3?e->f0:(cosmosMode?cosmos_frequency(src,t):(duetMode?duet_frequency(src,t):e->f0*pow(e->f1/e->f0,u)));for(int b=0;b<SPEC_BANDS;b++){double bf=loAll*exp(span*b/(SPEC_BANDS-1.0)),d=log(bf/f)/log(2.0);band[b]+=a*exp(-d*d/.055);}}
 }
}
static void wave_ring3plane(unsigned char*p,V3 c,double r,int plane,const float*wave,double peak,V3 cam,V3 target,uint32_t color,double alpha){V3 first=c,prev=c;for(int i=0;i<=28;i++){int wi=(i%28)*WAVE_POINTS/28;double displacement=peak>1e-8?.30*wave[wi]/peak:0,rr=fmax(.04,r+displacement),a=TAU*i/28,x=cos(a)*rr,y=sin(a)*rr;V3 q=plane==0?(V3){c.x+x,c.y+y,c.z}:plane==1?(V3){c.x+x,c.y,c.z+y}:(V3){c.x,c.y+x,c.z+y};if(i)line3(p,prev,q,cam,target,color,alpha);else first=q;prev=q;}line3(p,prev,first,cam,target,color,alpha);}
static void spectral_volume(unsigned char*p,int src,int fr,double t,V3 c,V3 cam,V3 target,int focus){double band[SPEC_BANDS],peak=0,ml,mr;source_spectrum(src,t,band);source_meter(fr,src,&ml,&mr);double heard=hypot(ml,mr);for(int b=0;b<SPEC_BANDS;b++)if(band[b]>peak)peak=band[b];if(peak<1e-9)peak=1;
 V3 pt[SPEC_BANDS][SPEC_RIBS];double bodyScale=focus?1.22:1.0,alpha=.10+fmin(.42,heard*19)+(focus?.08:0);
 for(int b=0;b<SPEC_BANDS;b++){double z=(b-(SPEC_BANDS-1)*.5)*.125,r=bodyScale*(.11+.58*sqrt(band[b]/peak));uint32_t col=shade_color(S[src].color,.56+.64*b/(SPEC_BANDS-1.0));V3 center={c.x,c.y,c.z+z};P2 pc=project(center,cam,target),pe=project((V3){center.x+r,center.y,center.z},cam,target);if(pc.ok&&pe.ok){int rr=(int)fmax(3,fmin(42,hypot(pe.x-pc.x,pe.y-pc.y)));glow(p,pc.x,pc.y,rr,col,.012+fmin(.055,heard*3.5));}
  for(int k=0;k<SPEC_RIBS;k++){double a=TAU*k/SPEC_RIBS+t*.16+src*.37;pt[b][k]=(V3){c.x+cos(a)*r,c.y+sin(a)*r,c.z+z};if(k)line3(p,pt[b][k-1],pt[b][k],cam,target,col,alpha*.72);if(b)line3(p,pt[b-1][k],pt[b][k],cam,target,col,alpha);}
  line3(p,pt[b][SPEC_RIBS-1],pt[b][0],cam,target,col,alpha*.72);
 }
}
// Acoustic shells are emitted from each body's historical world position.  A
// shell exists only when that source's measured post-HRTF output existed, so
// the waves remain signal visualization rather than decorative animation.
static void sound_shells(unsigned char*p,int src,int fr,V3 cam,V3 target){for(int q=1;q<=6;q++){int pf=fr-q*5;if(pf<0)continue;double ml,mr;source_meter(pf,src,&ml,&mr);double e=hypot(ml,mr);if(e<.00008)continue;double age=(fr-pf)/(double)FPS,et=pf/(double)FPS,fade=(1-age/1.35);fade*=fade;Source so=source_at(src,et);V3 c={so.x,so.y,so.z};double r=.16+age*2.35,a=(.025+fmin(.20,e*15))*fade;uint32_t col=shade_color(S[src].color,.72+q*.055);const float*wave=&sourceWave[(pf*NSRC+src)*WAVE_POINTS];double peak=0;for(int i=0;i<WAVE_POINTS;i++)peak=fmax(peak,fabs(wave[i]));wave_ring3plane(p,c,r,0,wave,peak,cam,target,col,a);wave_ring3plane(p,c,r,1,wave,peak,cam,target,col,a*.78);wave_ring3plane(p,c,r,2,wave,peak,cam,target,col,a*.62);}}
static uint32_t stereo_color(double pan){double u=(pan+1)*.5;int lr=0x4e,lg=0xcd,lb=0xc4,rr=0xf8,rg=0xa5,rb=0xc2,r=(int)(lr+(rr-lr)*u),g=(int)(lg+(rg-lg)*u),b=(int)(lb+(rb-lb)*u);return(uint32_t)(r<<16|g<<8|b);}
// The canopy glass is the final stereo waveform, decoded from the exact master
// that video() will mux.  Perspective rows hold the preceding 55 ms; left to
// right is the calculated L/R interpolation, so this is a sound plane rather
// than a decorative HUD grid.
static void stereo_windshield(unsigned char*p,double t){long now=(long)(t*SR);int rows=10,cols=28,prevX[29],prevY[29];for(int row=0;row<=rows;row++){double v=row/(double)rows,left=178+(35-178)*v,right=542+(685-542)*v,baseY=105+(510-105)*v;long base=now-(long)((1-v)*.055*SR);int lastX=0,lastY=0;for(int col=0;col<=cols;col++){double u=col/(double)cols,pan=u*2-1;long si=base+col*3;if(si<0)si=0;if(si>=(long)(DUR*SR))si=(long)(DUR*SR)-1;double sample=busL[si]*(1-u)+busR[si]*u,x=left+(right-left)*u,y=baseY-sample*(52+40*v);uint32_t c=stereo_color(pan);if(col)line2(p,lastX,lastY,(int)x,(int)y,c,.10+fmin(.18,fabs(sample)*.9));if(row)line2(p,prevX[col],prevY[col],(int)x,(int)y,c,.055+fmin(.12,fabs(sample)*.55));prevX[col]=(int)x;prevY[col]=(int)y;lastX=(int)x;lastY=(int)y;}}
 // A bright current-time trace makes the plane's instantaneous stereo wave
 // legible against its own short perspective history.
 int lastX=0,lastY=0;for(int col=0;col<=cols;col++){double u=col/(double)cols,pan=u*2-1;long si=now+col*3;if(si>=(long)(DUR*SR))si=(long)(DUR*SR)-1;double sample=busL[si]*(1-u)+busR[si]*u,x=178+(542-178)*u,y=105-sample*62;if(col)line2(p,lastX,lastY,(int)x,(int)y,stereo_color(pan),.42);lastX=(int)x;lastY=(int)y;}}
// Source packets travel from their projected spectral body to the windshield.
// Existence/size comes from captured per-source waveform samples; impact x is
// the source's measured post-HRTF stereo balance and impact y its live spectral
// centroid.  The collision splash therefore says what arrived and where.
static void windshield_particles(unsigned char*p,int fr,double t,V3 cam,V3 target){for(int s=0;s<NSRC;s++){double ml,mr,e,band[SPEC_BANDS],sum=0,weighted=0;source_meter(fr,s,&ml,&mr);e=ml+mr;if(e<.00016)continue;double pan=(mr-ml)/(e+1e-9);source_spectrum(s,t,band);for(int b=0;b<SPEC_BANDS;b++){sum+=band[b];weighted+=band[b]*b;}double centroid=sum?weighted/(sum*(SPEC_BANDS-1.0)):.5;int ix=(int)(360+pan*230),iy=(int)(390-centroid*220);Source so=source_at(s,t);P2 sp=project((V3){so.x,so.y,so.z},cam,target);if(!sp.ok)continue;const float*wave=&sourceWave[(fr*NSRC+s)*WAVE_POINTS];double peak=0;for(int i=0;i<WAVE_POINTS;i++)peak=fmax(peak,fabs(wave[i]));if(peak<1e-8)continue;for(int k=0;k<4;k++){int wi=(k*11+s*3)%WAVE_POINTS;double sample=wave[wi]/peak,phase=fmod(t*(1.35+.055*s)+s*.137+k*.241,1.0);phase=phase*phase*(3-2*phase);double prev=fmax(0,phase-.065),arc=sin(M_PI*phase)*(18+8*k),x=sp.x+(ix-sp.x)*phase,y=sp.y+(iy-sp.y)*phase-arc,x0=sp.x+(ix-sp.x)*prev,y0=sp.y+(iy-sp.y)*prev-sin(M_PI*prev)*(18+8*k),strength=.12+.42*fabs(sample);line2(p,(int)x0,(int)y0,(int)x,(int)y,S[s].color,strength);int r=1+(int)(3*fabs(sample));glow(p,(int)x,(int)y,r*3,S[s].color,.13+.20*fabs(sample));dot(p,(int)x,(int)y,r,S[s].color);if(phase>.90){double u=(phase-.90)/.10;int rr=3+(int)(25*u);ellipse2(p,ix,iy,rr,(int)(rr*.46),S[s].color,(1-u)*(.18+.32*fabs(sample)));for(int ray=0;ray<5;ray++){double a=TAU*(ray/5.0+s*.071),len=5+14*u;line2(p,ix+(int)(cos(a)*3),iy+(int)(sin(a)*2),ix+(int)(cos(a)*len),iy+(int)(sin(a)*len*.46),S[s].color,(1-u)*.34);}}}}}
static P2 chart_point(double x,double y){return(P2){226+(int)(x*13.0),260-(int)(y*13.0),0,1};}
static void acoustics_frame(unsigned char*p,int fr,double t,Listener l){
 uint32_t ink=brightMode?0x24383c:0xc7e5e5,sub=brightMode?0x718b8e:0x527a7e,panel=brightMode?0xe4ebe8:0x101c20;
 fill2(p,14,18,452,510,panel,.86);fill2(p,462,18,706,510,panel,.91);text3(p,26,30,"ACOUSTIC WORLD",2,ink);text3(p,474,30,"SIGNAL CHAIN",2,ink);
 for(int g=-12;g<=12;g+=4){P2 a=chart_point(g,-17),b=chart_point(g,17);line2(p,a.x,a.y,b.x,b.y,sub,.12);a=chart_point(-17,g);b=chart_point(17,g);line2(p,a.x,a.y,b.x,b.y,sub,.12);}
 int focus=tour_source(t);P2 ship=chart_point(l.x,l.y);glow(p,ship.x,ship.y,18,0xffd54f,.25);tri2(p,ship.x,ship.y-9,ship.x-7,ship.y+7,ship.x+7,ship.y+7,0xffd54f,.95);text3(p,ship.x+10,ship.y-3,"L",2,ink);
 for(int k=0;k<2;k++){Source sun=system_sun_at(k,t);P2 sp=chart_point(sun.x,sun.y);ellipse2(p,sp.x,sp.y,(int)(COSMOS[k?4:0].orbit*13),(int)(COSMOS[k?4:0].orbit*8),SYSTEMS[k].color,.22);glow(p,sp.x,sp.y,16,SYSTEMS[k].color,.28);dot(p,sp.x,sp.y,7,SYSTEMS[k].color);line2(p,sp.x,sp.y,ship.x,ship.y,SYSTEMS[k].color,.22);}
 for(int s=0;s<NSRC;s++){Source so=source_at(s,t),sun=system_sun_at(COSMOS[s].system,t);P2 q=chart_point(so.x,so.y),sp=chart_point(sun.x,sun.y);double ml,mr;source_meter(fr,s,&ml,&mr);line2(p,sp.x,sp.y,q.x,q.y,S[s].color,.12+fmin(.5,(ml+mr)*10));int r=s==focus?7:4;fill2(p,q.x-r,q.y-r,q.x+r+1,q.y+r+1,S[s].color,.94);if(s==focus){glow(p,q.x,q.y,20,S[s].color,.3);line2(p,q.x,q.y,ship.x,ship.y,S[s].color,.45);}}
 Source so=source_at(focus,t),sun=system_sun_at(COSMOS[focus].system,t);double d1=hypot(hypot(so.x-sun.x,so.y-sun.y),so.z-sun.z),d2=hypot(hypot(sun.x-l.x,sun.y-l.y),sun.z-l.z),az,el,dist,ml,mr;spatial_params(focus,t,&az,&el,&dist);source_meter(fr,focus,&ml,&mr);double mesh=cosmos_plane_gain(focus,t,az,el),field=field_gain(focus,t),delay=(d1+d2)/343.0*1000.0;
 char num[32];int x=476,y=70;const char*labels[]={"OSC","PITCH","MESH","DIRECT","MOVE","E-SUN","SUN-L","DELAY","FIELD","HRTF","LEFT","RIGHT"};double vals[]={COSMOS[focus].wave?1:0,cosmos_frequency(focus,t),BODY_ACOUSTICS[focus].mode[0],mesh,hypot(so.x-sun.x,so.y-sun.y),d1,d2,delay,field,cos(az),ml,mr};double maxv[]={1,1000,4000,1.3,8,8,24,90,1,1,.08,.08};
 for(int i=0;i<12;i++,y+=34){text3(p,x,y,labels[i],2,ink);int bx=x+76,bw=142;fill2(p,bx,y-1,bx+bw,y+9,sub,.16);double u=fmax(0,fmin(1,vals[i]/maxv[i]));fill2(p,bx,y-1,bx+(int)(bw*u),y+9,i>=10?(i==10?0x4ecdc4:0xf8a5c2):S[focus].color,.84);snprintf(num,sizeof num,"%d",(int)lrint(vals[i]));text3(p,bx,y+13,num,2,ink);}
 text3(p,28,526,"12 VOICES  OSC > MESH > MOTION > SUN > SPACE > HRTF > L R",2,ink);
 int base=574,rowh=10;for(int s=0;s<NSRC;s++){double a,b;source_meter(fr,s,&a,&b);int yy=base+s*rowh;fill2(p,28,yy,44,yy+6,S[s].color,.9);fill2(p,50,yy,50+(int)fmin(230,(a+b)*1500),yy+6,S[s].color,.74);Source q=source_at(s,t);int pitch=(int)cosmos_frequency(s,t);int px=320+(pitch%360);fill2(p,px,yy,px+5,yy+6,S[s].color,.9);}
 int play=28+(int)((W-56)*t/DUR);line2(p,28,H-30,W-28,H-30,sub,.45);line2(p,play,H-43,play,H-18,0xffd54f,.95);
}
static void stereo_lobes(unsigned char*p,P2 sp,double l,double r,int radius,uint32_t c){
 // The two ears are visible: left/right lobe areas are the measured channel RMS.
 double peak=fmax(l,r),scale=peak>0?1.0/peak:0;int gap=radius+2;
 int rl=(int)fmax(1,radius*(.2+.8*sqrt(l*scale))),rr=(int)fmax(1,radius*(.2+.8*sqrt(r*scale)));
 glow(p,sp.x-gap,sp.y,rl*2,c,.12+fmin(.55,l*18));glow(p,sp.x+gap,sp.y,rr*2,c,.12+fmin(.55,r*18));
 dot(p,sp.x-gap,sp.y,rl,c);dot(p,sp.x+gap,sp.y,rr,c);
}
static V3 globe_point(double x,double y,double altitude){
 // The acoustic simulation stays Cartesian inside a transparent world shell.
 return(V3){x*.58,y*.58,altitude*.72-1.25};
}
static double bounce_wave(double phase){
 // Triangle motion reads as travel plus a clean rebound, not orbital floating.
 double u=phase-floor(phase);return 1.0-4.0*fabs(u-.5);
}
static V3 globe_body(int s,double t){
 Source q=source_at(s,t);return globe_point(q.x,q.y,q.z);
}
static int pathtrace_still(const char*out,double t,int spp){
 PTBody bodies[NSRC+4];for(int s=0;s<NSRC;s++){V3 v=globe_body(s,t);uint32_t c=S[s].color;double energy=0;for(int j=0;j<NE;j++)if(E[j].src==s)energy+=env(&E[j],t)*E[j].g;double arrival=shell_listen(s,t);bodies[s]=(PTBody){ptv(v.x,v.y,v.z),ptv(((c>>16)&255)/255.0,((c>>8)&255)/255.0,(c&255)/255.0),.16+.12*S[s].mass,(5.5+fmin(18,energy*130))*arrival,.18};}
 Listener l=L[(int)(fmin(DUR,t)*CTRL)];V3 lv=globe_point(l.x,l.y,1.6);bodies[NSRC]=(PTBody){ptv(lv.x,lv.y,lv.z),ptv(1,.57,.16),.24,2.8,.3};
 // Matte objects catch and reveal indirect colored light inside the glass.
 bodies[NSRC+1]=(PTBody){ptv(-2.7,2.1,-2.8),ptv(.12,.38,.31),1.15,0,.88};
 bodies[NSRC+2]=(PTBody){ptv(2.9,1.5,-2.25),ptv(.42,.12,.34),.82,0,.72};
 bodies[NSRC+3]=(PTBody){ptv(.3,-3.1,-2.65),ptv(.12,.2,.48),1.0,0,.82};
 int w=720,h=720;unsigned char*rgb=malloc(w*h*3);double spin=-.78+.12*sin(t*.018),dist=17;PTVec cam=ptv(dist*cos(spin),dist*sin(spin),10.2);pt_render_rgb(rgb,w,h,cam,ptv(0,0,0),bodies,NSRC+4,spp,(int)(t*FPS),t);
 FILE*f=fopen(out,"wb");if(!f){free(rgb);return 0;}fprintf(f,"P6\n%d %d\n255\n",w,h);fwrite(rgb,1,w*h*3,f);fclose(f);free(rgb);return 1;
}
static int export_scene(const char*out,int frames){
 FILE*f=fopen(out,"wb");if(!f)return 0;uint32_t magic=0x53434e45,version=2,count=NSRC,fps=FPS;fwrite(&magic,4,1,f);fwrite(&version,4,1,f);fwrite(&frames,4,1,f);fwrite(&fps,4,1,f);fwrite(&count,4,1,f);
 for(int fr=0;fr<frames;fr++){double t=fr/(double)FPS;int li=(int)(t*CTRL);Listener l=L[li];float head[4]={(float)t,(float)l.x,(float)l.y,(float)(cosmosMode?l.z:1.6)};fwrite(head,4,4,f);for(int s=0;s<NSRC;s++){Source so=source_at(s,t);double ml,mr,pitchSum=0,pitchWeight=0;source_meter(fr,s,&ml,&mr);for(int j=0;j<NE;j++)if(E[j].src==s){double a=env(&E[j],t)*E[j].g;if(a>0){double u=fmax(0,fmin(1,(t-E[j].t)/E[j].dur)),freq=E[j].f0*pow(E[j].f1/E[j].f0,u);pitchSum+=freq*a;pitchWeight+=a;}}float row[6]={(float)so.x,(float)so.y,(float)so.z,(float)ml,(float)mr,(float)(pitchWeight?pitchSum/pitchWeight:0)};fwrite(row,4,6,f);}}
 fclose(f);return 1;
}
static void glass_shell(unsigned char*p,V3 cam,V3 target,double t){
 P2 c=project((V3){0,0,0},cam,target);if(!c.ok||c.z<7)return;
 double focal=W*.694,rp=focal*6.8/sqrt(c.z*c.z-6.8*6.8);int r=(int)rp;
 for(int yy=-r;yy<=r;yy++)for(int xx=-r;xx<=r;xx++){double nx=xx/rp,ny=yy/rp,d2=nx*nx+ny*ny;if(d2>=1)continue;int X=c.x+xx,Y=c.y+yy;if(X<0||X>=W||Y<0||Y>=H)continue;
  double nz=sqrt(1-d2),fres=pow(1-nz,4),glint=pow(fmax(0,1-hypot(nx+.36,ny+.42)*1.85),22),a=.018+.19*fres+.52*glint;
  double caustic=.5+.5*sin(nx*19+ny*13+t*.22),rr=66+35*glint,gg=112+55*glint+10*caustic,bb=124+72*glint+12*caustic;int o=(Y*W+X)*3;
  p[o]=(unsigned char)(p[o]*(1-a)+rr*a);p[o+1]=(unsigned char)(p[o+1]*(1-a)+gg*a);p[o+2]=(unsigned char)(p[o+2]*(1-a)+bb*a);
 }
 ellipse2(p,c.x,c.y,r,r,0x9bd5d5,.48);
}
static void globe_frame(unsigned char*p,int fr,double t,Listener l){
 if(acousticsView){acoustics_frame(p,fr,t,l);return;}
 int focus=tour_source(t);
 // Listener-mounted isometric rig: translation follows the receiver while the
 // viewing basis remains stable, preventing the constellation from drifting
 // offscreen without introducing camera tumble.
 V3 lv=globe_point(l.x,l.y,cosmosMode?l.z:1.6);projectionRoll=(cosmosMode&&cameraMode)?l.roll:0;
 V3 centroid={0,0,0};int visible=0;for(int s=0;s<NSRC;s++)if(voice_enabled(s)){V3 b=globe_body(s,t);centroid.x+=b.x;centroid.y+=b.y;centroid.z+=b.z;visible++;}
 if(visible){centroid.x/=visible;centroid.y/=visible;centroid.z/=visible;}else centroid=lv;
 V3 frameCenter={lv.x*.42+centroid.x*.58,lv.y*.42+centroid.y*.58,lv.z*.42+centroid.z*.58};
 double spin=(cosmosMode&&!cameraMode)?-.78:-.78+.12*sin(t*.018),breath=(cosmosMode&&!cameraMode)?.5:.5+.5*cos(TAU*t/17.0);
 double dist=(cosmosMode?11.9:11.8)+.32*breath;
 if(cosmosMode&&!cameraMode){frameCenter=globe_point((SYSTEMS[0].x+SYSTEMS[1].x)*.5,(SYSTEMS[0].y+SYSTEMS[1].y)*.5,(SYSTEMS[0].z+SYSTEMS[1].z)*.5);}
 V3 cam={frameCenter.x+dist*cos(spin),frameCenter.y+dist*sin(spin),frameCenter.z+7.35};
 double push=0;
 V3 target=frameCenter;
 if(cosmosMode&&cameraMode){double cp=cos(l.pitch),fx=cos(l.heading)*cp,fy=sin(l.heading)*cp,fz=sin(l.pitch);cam=(V3){lv.x-fx*.08,lv.y-fy*.08,lv.z-fz*.08};target=(V3){lv.x+fx*7.0,lv.y+fy*7.0,lv.z+fz*7.0};}
 if(cosmosMode){
  // Spectral horizon and broad emissive fields approximate indirect light in
  // the raster world: systems tint empty space before bodies are drawn.
  for(int q=-42;q<=42;q++){double f=1-abs(q)/43.0,a=.018*f*f;uint32_t c=q< -12?0x283a68:(q>14?0x6b294f:0x2b6671);fill2(p,0,(int)(H*.57)+q,W-1,(int)(H*.57)+q,c,a);}
  for(int k=0;k<2;k++){Source sun=system_sun_at(k,t);V3 sv=globe_point(sun.x,sun.y,sun.z);P2 sp=project(sv,cam,target);if(sp.ok)glow(p,sp.x,sp.y,210,SYSTEMS[k].color,.075);}
 }
 // True background volume: deterministic world-space stars share the scene
 // camera, so depth, scale, occlusion ordering, and parallax are physical.
 for(int q=0;q<240;q++){
  double sx=-24+fmod(q*17.731+3.7,48),sy=-24+fmod(q*29.417+11.2,48),sz=-10+fmod(q*13.137+5.9,27);
  P2 star=project((V3){sx,sy,sz},cam,target);if(!star.ok||star.z<4||star.z>48)continue;
  int r=star.z<14?2:1;uint32_t c=q%11==0?0xb7e8e4:(q%7==0?0x9c8fc4:0x536f78);
  if(star.z<12)glow(p,star.x,star.y,7,c,.08);dot(p,star.x,star.y,r,c);
 }
 uint32_t major=brightMode?0x688f91:0x35666b,minor=brightMode?0xa4b9b4:0x25464b;
 // Empty-space constellation: no floor, globe, grid, or explicit boundary.
 // Listener history and inherited motion trails are the only spatial scaffold.
 int li=(int)(t*CTRL);for(int q=1;q<=120;q++){int ia=li-(q-1)*CTRL/8,ib=li-q*CTRL/8;if(ib<0)break;V3 a=globe_point(L[ia].x,L[ia].y,1.48),b=globe_point(L[ib].x,L[ib].y,1.48);line3(p,a,b,cam,target,0xff6b9d,.34*(1-q/121.0));}
 if(cosmosMode)for(int k=0;k<2;k++){Source sun=system_sun_at(k,t);V3 sv=globe_point(sun.x,sun.y,sun.z);P2 sp=project(sv,cam,target);line3(p,sv,lv,cam,target,SYSTEMS[k].color,.31);if(sp.ok)glow(p,sp.x,sp.y,38,SYSTEMS[k].color,.38);draw_prompt_rock(p,&BODY_MESH[NSRC+k],sv,.78,.22*k,cam,target,SYSTEMS[k].color);}
 for(int s=0;s<NSRC;s++){if(!voice_enabled(s))continue;double ml,mr;source_meter(fr,s,&ml,&mr);double heard=hypot(ml,mr);V3 sv=globe_body(s,t);P2 sp=project(sv,cam,target);
  V3 link=lv;if(cosmosMode){Source sun=system_sun_at(COSMOS[s].system,t);link=globe_point(sun.x,sun.y,sun.z);}line3(p,link,sv,cam,target,heard>.00005?S[s].color:0x667176,.06+fmin(.62,heard*19));
  // Adaptive shutter trail. At launch, dense sampling over a short exposure
  // resolves the 520 RPM orbit instead of drawing polygonal chords. As angular
  // velocity settles, exposure grows into the full phosphor history.
  double settle=1-exp(-t/6.8),trailSecs=.75+7.25*settle;int trailSteps=320;
  V3 prev=sv;for(int q=1;q<=trailSteps;q++){double pt=fmax(0,t-q*trailSecs/trailSteps),fade=(1-q/(trailSteps+1.0))*(.018+fmin(.22,heard*9));V3 ov=globe_body(s,pt);line3(p,prev,ov,cam,target,S[s].color,fade);prev=ov;}
  if(cosmosMode){ACRock*mesh=&BODY_MESH[s];for(int q=1;q<=72;q++){double pt=fmax(0,t-q*4.8/72),fade=(1-q/73.0)*(.10+fmin(.24,heard*7)),ang=pt*(COSMOS[s].rate+COSMOS[s].phase),ca=cos(ang),sa=sin(ang);V3 center=globe_body(s,pt);for(int layer=0;layer<3;layer++){int vi=(q*7+layer*13+s*17)%mesh->nv;ACRockV mv=mesh->v[vi];double shell=(.19+.055*S[s].mass)*(1+layer*.17),vx=(mv.x*ca-mv.y*sa)*shell,vy=(mv.x*sa+mv.y*ca)*shell,vz=mv.z*shell;V3 voxel={center.x+vx,center.y+vy,center.z+vz};P2 vp=project(voxel,cam,target);if(!vp.ok)continue;int block=vp.z<8?3:(vp.z<16?2:1);uint32_t vc=shade_color(S[s].color,.62+.28*layer);fill2(p,vp.x-block,vp.y-block,vp.x+block,vp.y+block,vc,fade*(.62+.16*layer));if(layer==2&&q%12==0)glow(p,vp.x,vp.y,block*3,vc,fade*.16);}}}
  if(sp.ok){double focusScale=s==focus?1.0+2.4*push:1.0;int core=(int)fmax(3,fmin(18,(82/sp.z)*focusScale)),halo=(int)fmax(core*2,fmin(48,(5+heard*620)*focusScale));if(s==focus)glow(p,sp.x,sp.y,halo+8,0xffffff,.05+.08*push);glow(p,sp.x,sp.y,halo,S[s].color,.06+fmin(.25,heard*7));if(!cosmosMode){int gap=core+2,rl=(int)fmax(2,core*(.45+.55*sqrt(ml/(fmax(ml,mr)+1e-9)))),rr=(int)fmax(2,core*(.45+.55*sqrt(mr/(fmax(ml,mr)+1e-9))));dot(p,sp.x-gap,sp.y,rl,S[s].color);dot(p,sp.x+gap,sp.y,rr,S[s].color);marble(p,sp.x,sp.y,core+1,S[s].color,heard);}}
  if(cosmosMode){double phase=TAU*(SYSTEMS[COSMOS[s].system].tempo/60.0)*t*COSMOS[s].rate+COSMOS[s].phase*TAU;draw_prompt_rock_facing(p,&BODY_MESH[s],sv,lv,.29+.060*S[s].mass,phase,cam,target,S[s].color);}
 }
 // Collision-blast particles: impact energy excites the audio event above and
 // the same event seed launches deterministic ballistic luminous fragments.
 if(cosmosMode)for(int j=0;j<NE;j++)if(E[j].type==2){Event*e=&E[j];double age=t-e->t;if(age< -10||age>10.0)continue;Source hit=source_at(e->src,e->t);uint64_t seed=((uint64_t)(e->t*1000003.0)<<17)^(uint64_t)(e->src+1)*UINT64_C(0x9e3779b97f4a7c15);ACRock rock;ac_rock_generate(seed,&rock);
  if(age<0){Source sun=system_sun_at(COSMOS[e->src].system,t);double targetAng=atan2(hit.y-sun.y,hit.x-sun.x),u;
   V3 comet;if(age< -4){u=(age+10)/6;double r=15+(4.4-15)*u,a=targetAng-.72*(1-u);comet=globe_point(sun.x+cos(a)*r,sun.y+sin(a)*r,sun.z+6.0*(1-u)+1.1*sin(u*M_PI));}
   else{u=(age+4)/4;double r=4.4+(hypot(hit.x-sun.x,hit.y-sun.y)-4.4)*u,a=targetAng-TAU*3*(1-u);comet=globe_point(sun.x+cos(a)*r,sun.y+sin(a)*r,sun.z+(hit.z-sun.z)*u+.8*sin(u*TAU));}
   P2 cp=project(comet,cam,target);if(cp.ok)glow(p,cp.x,cp.y,22,S[e->src].color,.22);draw_prompt_rock(p,&rock,comet,.72+.18*rock.roughness,t*2.1,cam,target,S[e->src].color);continue;}
  double shardScale=.72+.18*rock.roughness;for(int q=0;q<rock.nf;q++){ACRockFace f=rock.f[q];ACRockV a=rock.v[f.a],b=rock.v[f.b],c=rock.v[f.c],center={(a.x+b.x+c.x)/3,(a.y+b.y+c.y)/3,(a.z+b.z+c.z)/3};double inset=.22+.22*fmod(q*.371+rock.roughness,1);ACRockV apex={center.x*inset,center.y*inset,center.z*inset};Source catcher=source_at((e->src+1+q%5)%NSRC,t);V3 va=capture_shard(pyramid_shard_point(a,center,hit,age,q,shardScale),catcher,age,q,0),vb=capture_shard(pyramid_shard_point(b,center,hit,age,q,shardScale),catcher,age,q,1),vc=capture_shard(pyramid_shard_point(c,center,hit,age,q,shardScale),catcher,age,q,2),vd=capture_shard(pyramid_shard_point(apex,center,hit,age,q,shardScale),catcher,age,q,3);P2 pa=project(va,cam,target),pb=project(vb,cam,target),pc=project(vc,cam,target),pd=project(vd,cam,target);if(pa.ok&&pb.ok&&pc.ok&&pd.ok){double fade=.16+.42*fmax(0,1-age/10);uint32_t col=S[e->src].color;tri2(p,pa.x,pa.y,pb.x,pb.y,pc.x,pc.y,shade_color(col,.62),fade);tri2(p,pa.x,pa.y,pb.x,pb.y,pd.x,pd.y,shade_color(col,.88),fade);tri2(p,pb.x,pb.y,pc.x,pc.y,pd.x,pd.y,shade_color(col,1.12),fade);tri2(p,pc.x,pc.y,pa.x,pa.y,pd.x,pd.y,shade_color(col,.74),fade);}}
 }
 if(cosmosMode)for(int j=0;j<NE;j++)if(E[j].type==5){Event*e=&E[j];double age=t-e->t;if(age<0||age>.32)continue;Source hit=source_at(e->src,e->t);ACRock*rock=&BODY_MESH[e->src];for(int q=0;q<6;q++){int fi=(int)(((uint64_t)q*11+e->seed)%(uint64_t)rock->nf);ACRockFace f=rock->f[fi];ACRockV a=rock->v[f.a],b=rock->v[f.b],c=rock->v[f.c],center={(a.x+b.x+c.x)/3,(a.y+b.y+c.y)/3,(a.z+b.z+c.z)/3},apex={center.x*.3,center.y*.3,center.z*.3};V3 va=pyramid_shard_point(a,center,hit,age,q,.24),vb=pyramid_shard_point(b,center,hit,age,q,.24),vc=pyramid_shard_point(c,center,hit,age,q,.24),vd=pyramid_shard_point(apex,center,hit,age,q,.24);P2 pa=project(va,cam,target),pb=project(vb,cam,target),pc=project(vc,cam,target),pd=project(vd,cam,target);if(pa.ok&&pb.ok&&pc.ok&&pd.ok){double fade=.62*(1-age/.32);uint32_t col=S[e->src].color;tri2(p,pa.x,pa.y,pb.x,pb.y,pc.x,pc.y,shade_color(col,.7),fade);tri2(p,pa.x,pa.y,pb.x,pb.y,pd.x,pd.y,shade_color(col,1.08),fade);tri2(p,pb.x,pb.y,pc.x,pc.y,pd.x,pd.y,shade_color(col,.86),fade);tri2(p,pc.x,pc.y,pa.x,pa.y,pd.x,pd.y,shade_color(col,.58),fade);}}}
 P2 lp=project(lv,cam,target);if(lp.ok){glow(p,lp.x,lp.y,48,0xffffff,.15);glow(p,lp.x,lp.y,34,0x63cdda,.22);glow(p,lp.x,lp.y,20,0xffd54f,.31);}if(cosmosMode&&!cameraMode){draw_prompt_rock(p,&BODY_MESH[NSRC+2],lv,.62,t*.4,cam,target,0xdffcff);draw_prompt_rock(p,&BODY_MESH[NSRC+2],lv,.55,-t*.31,cam,target,0xffd54f);}else if(lp.ok)dot(p,lp.x,lp.y,8,0xffffff);
 // Listener-relative 2D navigation radar. Distant stars never enter this local
 // map; only suns, speaker bodies, and imminent collision contacts appear.
 if(cosmosMode&&cameraMode){int mx=W-176,my=22,mw=154,cx=mx+mw/2,cy=my+mw/2;fill2(p,mx,my,mx+mw,my+mw,brightMode?0xe7eeeb:0x0c171b,.72);ellipse2(p,cx,cy,68,68,brightMode?0x536e73:0x75aeb2,.72);ellipse2(p,cx,cy,34,34,brightMode?0x82999c:0x426f76,.48);line2(p,cx-68,cy,cx+68,cy,0x688f91,.34);line2(p,cx,cy-68,cx,cy+68,0x688f91,.34);
  for(int k=0;k<2;k++){Source o=system_sun_at(k,t);double dx=o.x-l.x,dy=o.y-l.y,rx=dx*cos(l.heading)+dy*sin(l.heading),ry=-dx*sin(l.heading)+dy*cos(l.heading),d=hypot(rx,ry),sc=5.2;if(d*sc>65){rx*=65/(d*sc);ry*=65/(d*sc);}int x=cx+(int)(ry*sc),y=cy-(int)(rx*sc),r=6;tri2(p,x,y-r,x-r,y,x,y+r,SYSTEMS[k].color,.95);tri2(p,x,y-r,x,y+r,x+r,y,SYSTEMS[k].color,.95);}
  for(int s=0;s<NSRC;s++){Source o=source_at(s,t);double dx=o.x-l.x,dy=o.y-l.y,rx=dx*cos(l.heading)+dy*sin(l.heading),ry=-dx*sin(l.heading)+dy*cos(l.heading),d=hypot(rx,ry),sc=5.2;if(d*sc>65){rx*=65/(d*sc);ry*=65/(d*sc);}int x=cx+(int)(ry*sc),y=cy-(int)(rx*sc),r=3;fill2(p,x-r,y-r,x+r+1,y+r+1,S[s].color,.92);}
  tri2(p,cx,cy-8,cx-6,cy+6,cx+6,cy+6,0xffd54f,.95);}
 // Score horizon: movement spans, present playhead, and the next change arriving.
 {static const int bars[]={0,4,12,20,28,34,36,38};static const uint32_t colors[]={0x536069,0x4ecdc4,0x778beb,0xf6c915,0xf8a5c2,0xe0a464,0xff6b6b};
  int x0=54,x1=W-54,y=H-34,span=x1-x0;fill2(p,x0,y-2,x1,y+3,0x263438,.82);
  for(int m=0;m<7;m++){int a=x0+(int)(span*bars[m]/38.0),b=x0+(int)(span*bars[m+1]/38.0);fill2(p,a,y-3,b,y+4,colors[m],m==0?.34:.56);line2(p,a,y-9,a,y+10,colors[m],.85);}
  double bar=t/BAR;int px=x0+(int)(span*fmin(1,bar/38.0));line2(p,px,y-14,px,y+14,0xffffff,.95);dot(p,px,y,4,0xffffff);
  for(int m=1;m<8;m++)if(bars[m]>bar){double away=bars[m]-bar;if(away<2.0){int nx=x0+(int)(span*bars[m]/38.0),r=5+(int)(8*(1-away/2.0));glow(p,nx,y,r,colors[m<7?m:6],.18+.32*(1-away/2.0));}break;}
 }
}
static void video(const char*wavp,const char*outp,double videoStart,double videoDuration){char cmd[2048];size_t ol=strlen(outp);int lossless=ol>=4&&!strcmp(outp+ol-4,".mov");const char*clean=globeMode?"-af 'highpass=f=30,equalizer=f=7600:t=q:w=.75:g=-4,lowpass=f=11800,alimiter=limit=.90:attack=6:release=110,volume=.88'":"",*pixels=(globeMode&&!acousticsView)?"-vf 'scale=240:240:flags=neighbor,scale=720:720:flags=neighbor'":"";if(lossless)
 snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f rawvideo -pixel_format rgb24 -video_size %dx%d -framerate %d -i - -i '%s' -c:v libx265 -preset medium -x265-params lossless=1:log-level=error -pix_fmt yuv444p -tag:v hvc1 %s -c:a pcm_s24le -ar 48000 -shortest '%s'",W,H,FPS,wavp,clean,outp);
 else snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f rawvideo -pixel_format rgb24 -video_size %dx%d -framerate %d -i - -i '%s' %s -c:v libx264 -preset slow -pix_fmt yuv420p -crf 14 %s -c:a aac -b:a 320k -shortest '%s'",W,H,FPS,wavp,pixels,clean,outp);FILE*ff=popen(cmd,"w");if(!ff)return;unsigned char*p=malloc(W*H*3);
 int videoFrames=(int)floor(videoDuration*FPS+.5);for(int outFr=0;outFr<videoFrames;outFr++){double t=videoStart+outFr/(double)FPS;int fr=(int)floor(t*FPS);for(int y=0;y<H;y++){unsigned char v=brightMode?(unsigned char)(250-15*y/(double)H):(unsigned char)(24-10*y/(double)H);for(int x=0;x<W;x++){int o=(y*W+x)*3;p[o]=v;p[o+1]=(unsigned char)fmax(0,v-(brightMode?3:1));p[o+2]=(unsigned char)fmin(255,v+(brightMode?0:2));}}int li=(int)(t*CTRL);Listener l=L[li];
  if(globeMode){globe_frame(p,fr,t,l);fwrite(p,1,W*H*3,ff);continue;}
  // Camera begins among the sources, orbits with the listener, then after 78 s
  // retreats far beyond the room until the ensemble is a small constellation.
  int ts=tour_source(t),tn=TOUR[(ts==TOUR[11])?0:((int)(t/9.0)+1)%12];double phase=fmod(t,9.0)/9.0,lookMix=phase<.78?0:(phase-.78)/.22;lookMix=lookMix*lookMix*(3-2*lookMix);Source la=source_at(ts,t),lb=source_at(tn,t);
  // First-person radio receiver at ear height. Smooth focus handoffs avoid cuts;
  // parallax and changing radii now reveal the listener's physical travel.
  V3 cam,target;if(cameraMode){cam=(V3){l.x,l.y,1.62};target=(V3){la.x+(lb.x-la.x)*lookMix,la.y+(lb.y-la.y)*lookMix,la.z+(lb.z-la.z)*lookMix};}
  else{double cx=l.x,cy=l.y,cz=1.6;for(int s=0;s<NSRC;s++){Source q=source_at(s,t);cx+=q.x;cy+=q.y;cz+=q.z;}cx/=NSRC+1;cy/=NSRC+1;cz/=NSRC+1;double orbitTime=ending_motion_time(t),orbit=-.72+.16*sin(orbitTime/19.0);cam=(V3){cx+19*cos(orbit),cy+19*sin(orbit),cz+12.5};target=(V3){cx,cy,cz*.55};}
  // Faceted receiver-world horizon. Large triangles scroll gently with travel,
  // providing scale without pretending to be another sound source.
  if(cameraMode){int horizon=brightMode?300:330,shift=(int)(l.x*9+l.y*5);for(int q=-1;q<10;q++){int x0=q*92-(shift%92),x1=x0+46,x2=x0+92,y1=horizon-28-(int)(38*(.5+.5*sin(q*2.31+l.heading)));uint32_t c1=brightMode?(q&1?0xc9d2cf:0xbcc8c6):(q&1?0x20282b:0x182124);tri2(p,x0,horizon,x1,y1,x2,horizon,c1,.82);tri2(p,x0,horizon,x2,horizon,x2,H-205,brightMode?0xd9ded8:0x111719,.52);}line2(p,0,horizon,W,horizon,brightMode?0x7f9292:0x52656a,.55);}
  // Perspective floor grid makes depth and the final pull-away legible.
  for(int q=-8;q<=8;q++){line3(p,(V3){q,-8,0},(V3){q,8,0},cam,target,brightMode?0x9eafb2:0x536069,brightMode?.42:.28);line3(p,(V3){-8,q,0},(V3){8,q,0},cam,target,brightMode?0x9eafb2:0x536069,brightMode?.42:.28);}
  // The same propagation world the radio hears: brick occluder and wind-raked
  // rain streaks. They are geometry/fields, never decorative AI additions.
  for(int q=0;q<=5;q++)line3(p,(V3){1.2,-3,q*.48},(V3){1.2,2.5,q*.48},cam,target,0x806f68,.62);
  for(int q=0;q<=6;q++)line3(p,(V3){1.2,-3+q*.92,0},(V3){1.2,-3+q*.92,2.4},cam,target,0x806f68,.62);
  for(int q=0;q<42;q++){double rx=-8+fmod(q*3.71+t*.7,16),ry=-8+fmod(q*6.13+t*.43,16),rz=.3+fmod(q*1.77-t*1.9,5.5);line3(p,(V3){rx,ry,rz},(V3){rx-.12,ry-.08,rz-.38},cam,target,0x8faab5,.18);}
  V3 lv={l.x,l.y,1.6};
  // Six-second fading breadcrumb: the path that the two follower voices chase.
  for(int q=1;q<=36;q++){int ia=li-(q-1)*CTRL/6,ib=li-q*CTRL/6;if(ib<0)break;V3 a={L[ia].x,L[ia].y,1.58},b={L[ib].x,L[ib].y,1.58};line3(p,a,b,cam,target,0xff6b9d,.42*(1-q/37.0));}
  for(int s=0;s<NSRC;s++){double ml,mr;source_meter(fr,s,&ml,&mr);double heard=hypot(ml,mr);Source so=source_at(s,t);V3 sv={so.x,so.y,so.z};P2 sp=project(sv,cam,target);double gl,gr,dd;gains(s,t,&gl,&gr,&dd);
   // Grey is physical distance. Spectral volume and emitted-shell intensity
   // come from this body's score spectrum and measured post-HRTF energy.
   line3(p,lv,sv,cam,target,0x778087,.18);circle3(p,(V3){so.x,so.y,.025},1.35,cam,target,S[s].color,s==ts?.34:.08);if(s==ts&&t<104)circle3(p,(V3){so.x,so.y,.03},4.05,cam,target,S[s].color,.2);
   sound_shells(p,s,fr,cam,target);
   // Phosphor streak = true recent emitter trajectory. Distant transmissions
   // persist longer, making their longer radio flight time visible.
   {double trail=.18+fmin(.9,dd/12);V3 prev=sv;for(int q=1;q<=16;q++){double pt=fmax(0,t-q*trail/16),fade=(1-q/17.0)*(.08+fmin(.55,heard*18));Source old=source_at(s,pt);V3 ov={old.x,old.y,old.z};line3(p,prev,ov,cam,target,S[s].color,fade);prev=ov;}}
   if(heard>.00005)line3(p,lv,sv,cam,target,S[s].color,.03+fmin(.84,heard*28));
   if(sp.ok){double sc=W/360.0;int core=(int)fmax(3,fmin(11,52*sc/sp.z));int halo=(int)fmax(core*2,fmin(62,(4+heard*1500)*sc*10/sp.z));P2 sh=project((V3){so.x,so.y,.02},cam,target);if(sh.ok)glow(p,sh.x,sh.y,(int)fmax(5,halo*.8),S[s].color,.06+fmin(.4,heard*14));if(s==ts&&t<104)glow(p,sp.x,sp.y,halo+18,0xffffff,.16);glow(p,sp.x,sp.y,halo,S[s].color,.10+fmin(.36,heard*10));}
   spectral_volume(p,s,fr,t,sv,cam,target,s==ts);if(!cameraMode&&sp.ok)text3(p,sp.x+9,sp.y-7,S[s].name,1,brightMode?0x33484c:0xd5e5e2);
  }
  if(cameraMode){
  stereo_windshield(p,t);
  windshield_particles(p,fr,t,cam,target);
  P2 lp=project(lv,cam,target),hd=project((V3){l.x+cos(l.heading)*.55,l.y+sin(l.heading)*.55,1.6},cam,target);
  if(lp.ok){int rr=(int)fmax(4,fmin(20,130/lp.z));glow(p,lp.x,lp.y,rr*3,0xf6c915,.35);dot(p,lp.x,lp.y,rr,0xffd54f);if(hd.ok)line2(p,lp.x,lp.y,hd.x,hd.y,0xff6b9d,.95);}
  // Receiver cockpit. The canopy frames the view while the lower scanner maps
  // contacts around the listener; vertical stalks encode above/below exactly.
  uint32_t deck=brightMode?0xe5e2dc:0x080b0d,frame=brightMode?0x526269:0x38454b,scan=brightMode?0x60777b:0x5b6f73,subscan=brightMode?0x84979d:0x405256;
  fill2(p,0,H-205,W,H,deck,brightMode?.82:.72);for(int w=0;w<4;w++){line2(p,35+w,H,178+w,105,frame,.88);line2(p,W-35-w,H,W-178-w,105,frame,.88);line2(p,178+w,105,W-178-w,105,frame,.7);}
  line2(p,W/2-18,H/2,W/2-5,H/2,0xb8c9cc,.65);line2(p,W/2+5,H/2,W/2+18,H/2,0xb8c9cc,.65);line2(p,W/2,H/2-18,W/2,H/2-5,0xb8c9cc,.65);line2(p,W/2,H/2+5,W/2,H/2+18,0xb8c9cc,.65);
  int rcx=W/2,rcy=H-92;ellipse2(p,rcx,rcy,122,62,scan,.8);ellipse2(p,rcx,rcy,82,41,subscan,.65);ellipse2(p,rcx,rcy,41,20,subscan,.5);line2(p,rcx-122,rcy,rcx+122,rcy,subscan,.45);line2(p,rcx,rcy-62,rcx,rcy+62,subscan,.45);
  // Actual decoded stereo waveform windows from the mastered buses.
  {long si=(long)(t*SR);int lx0=52,rx0=W-202,wy=H-143,span=150,lastL=wy,lastR=wy;for(int q=1;q<span;q++){long k=si+q*6;if(k>=(long)(DUR*SR))k=(long)(DUR*SR)-1;int yl=wy-(int)(busL[k]*42),yr=wy-(int)(busR[k]*42);line2(p,lx0+q-1,lastL,lx0+q,yl,0x4ecdc4,.9);line2(p,rx0+q-1,lastR,rx0+q,yr,0xf8a5c2,.9);lastL=yl;lastR=yr;}}
  // Receiver triangle points forward. Contact brightness is measured signal,
  // weak transmissions flicker unresolved instead of being presented as fact.
  line2(p,rcx,rcy-7,rcx-6,rcy+6,0xf6c915,.95);line2(p,rcx-6,rcy+6,rcx+6,rcy+6,0xf6c915,.95);line2(p,rcx+6,rcy+6,rcx,rcy-7,0xf6c915,.95);
  double sumL=0,sumR=0;for(int s=0;s<NSRC;s++){double ml,mr;source_meter(fr,s,&ml,&mr);sumL+=ml;sumR+=mr;Source so=source_at(s,t);double dx=so.x-l.x,dy=so.y-l.y,rx=dx*cos(l.heading)+dy*sin(l.heading),ry=-dx*sin(l.heading)+dy*cos(l.heading),scale=11.0,rr=hypot(rx,ry);if(rr>10.5){rx*=10.5/rr;ry*=10.5/rr;}int bx=rcx+(int)(ry*scale),base=rcy-(int)(rx*scale*.5),by=base-(int)((so.z-1.6)*10),weak=hypot(ml,mr)<.0015;double vis=weak?(.16+.24*(sin(t*31+s*7)>0)):.9;line2(p,bx,base,bx,by,S[s].color,vis*.7);if(s==ts)glow(p,bx,by,12,0xffffff,.28);dot(p,bx,by,s==ts?4:3,S[s].color);}
  int lm=(int)fmin(110,sumL*850),rm=(int)fmin(110,sumR*850);fill2(p,70,H-55,70+lm,H-45,0x4ecdc4,.9);fill2(p,W-70-rm,H-55,W-70,H-45,0xf8a5c2,.9);
  }else{P2 lp=project(lv,cam,target),hd=project((V3){l.x+cos(l.heading)*.75,l.y+sin(l.heading)*.75,1.6},cam,target);if(lp.ok){glow(p,lp.x,lp.y,26,0xffd54f,.28);dot(p,lp.x,lp.y,7,0xffffff);if(hd.ok)line2(p,lp.x,lp.y,hd.x,hd.y,0xff6b9d,.9);}text3(p,24,22,"GLOBAL ARRANGEMENT",2,brightMode?0x324a4d:0xe2efed);text3(p,24,42,"SOUND BODIES - GRAVITY - WAVES",1,brightMode?0x536e73:0x75aeb2);}
  fwrite(p,1,W*H*3,ff);
 }
 free(p);pclose(ff);
}
  int main(int argc,char**argv){const char*w="../out/spatial-sineabye.wav",*v="../out/spatial-sineabye.mp4",*videoAudio=NULL,*mp3=NULL,*ptStill=NULL,*sceneOut=NULL;double ptTime=32,videoStart=0,videoDuration=DUR;int ptSpp=32,sceneFrames=0,tempoExplicit=0;for(int i=1;i<argc;i++){if(!strcmp(argv[i],"--wav")&&i+1<argc)w=argv[++i];else if(!strcmp(argv[i],"--video")&&i+1<argc)v=argv[++i];else if(!strcmp(argv[i],"--video-audio")&&i+1<argc)videoAudio=argv[++i];else if(!strcmp(argv[i],"--video-start")&&i+1<argc)videoStart=atof(argv[++i]);else if(!strcmp(argv[i],"--video-duration")&&i+1<argc)videoDuration=atof(argv[++i]);else if(!strcmp(argv[i],"--mp3")&&i+1<argc)mp3=argv[++i];else if(!strcmp(argv[i],"--spatial-wet")&&i+1<argc)spatialWet=fmax(0,fmin(1,atof(argv[++i])));else if(!strcmp(argv[i],"--noise-level")&&i+1<argc)noiseLevel=fmax(.05,fmin(1,atof(argv[++i])));else if(!strcmp(argv[i],"--bpm")&&i+1<argc){tempoBpm=fmax(30,fmin(240,atof(argv[++i])));tempoExplicit=1;}else if(!strcmp(argv[i],"--bright")){brightMode=1;themeExplicit=1;}else if(!strcmp(argv[i],"--dark")){brightMode=0;themeExplicit=1;}else if(!strcmp(argv[i],"--theme")&&i+1<argc){const char*th=argv[++i];if(!strcmp(th,"light"))brightMode=1;else if(!strcmp(th,"dark"))brightMode=0;themeExplicit=strcmp(th,"auto")!=0;}else if(!strcmp(argv[i],"--globe"))globeMode=1;else if(!strcmp(argv[i],"--lattice-duet")){globeMode=1;duetMode=1;voiceCount=4;}else if(!strcmp(argv[i],"--cosmos")){globeMode=1;cosmosMode=1;voiceCount=12;}else if(!strcmp(argv[i],"--camera")&&i+1<argc){cameraMode=!strcmp(argv[++i],"ship");}else if(!strcmp(argv[i],"--voices")&&i+1<argc)voiceCount=atoi(argv[++i]);else if(!strcmp(argv[i],"--pathtrace-still")&&i+1<argc)ptStill=argv[++i];else if(!strcmp(argv[i],"--pt-time")&&i+1<argc)ptTime=atof(argv[++i]);else if(!strcmp(argv[i],"--pt-spp")&&i+1<argc)ptSpp=atoi(argv[++i]);else if(!strcmp(argv[i],"--scene-data")&&i+1<argc)sceneOut=argv[++i];else if(!strcmp(argv[i],"--scene-frames")&&i+1<argc)sceneFrames=atoi(argv[++i]);}if(!tempoExplicit)tempoBpm=cosmosMode?COSMOS_BPM:ROOM_BPM;if(sceneFrames<=0)sceneFrames=NFRAMES;resolve_render_theme();videoStart=fmax(0,fmin(DUR,videoStart));videoDuration=fmax(0,fmin(DUR-videoStart,videoDuration));
 for(int i=1;i+1<argc;i++){if(!strcmp(argv[i],"--visual"))acousticsView=!strcmp(argv[i+1],"acoustics");else if(!strcmp(argv[i],"--antenna-am"))antennaDepth=fmax(0,fmin(.18,atof(argv[i+1])));}
 score();filter_score();simulate();if(ptStill){globeMode=1;if(!pathtrace_still(ptStill,fmax(0,fmin(DUR,ptTime)),fmax(1,ptSpp))){fprintf(stderr,"pathtrace write failed\n");return 1;}fprintf(stderr,"✓ %s · path traced · %d spp\n",ptStill,ptSpp);return 0;}render();if(sceneOut&&!export_scene(sceneOut,fmax(1,fmin(NFRAMES,sceneFrames)))){fprintf(stderr,"scene export failed\n");return 1;}if(!wav(w)){fprintf(stderr,"write failed\n");return 1;}fprintf(stderr,"✓ %s · %.1fs · %d sound bodies/events · %d voices · spatial wet %.0f%%\n",w,DUR,NE,voiceCount,spatialWet*100);
 if(mp3){char cmd[4096];snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -i '%s' -af 'highpass=f=28,equalizer=f=72:t=q:w=.8:g=1.2,equalizer=f=7200:t=q:w=.9:g=-1,lowpass=f=15800,alimiter=limit=.90:attack=6:release=100,volume=.84' -c:a libmp3lame -q:a 2 '%s'",w,mp3);if(system(cmd)!=0)return 1;}
 if(v&&strcmp(v,"none")){if(videoAudio&&cameraMode&&!load_visual_mix(videoAudio))fprintf(stderr,"warning: final master could not be decoded for windshield telemetry\n");video(videoAudio?videoAudio:w,v,videoStart,videoDuration);}return 0;}
