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
#define BPM 104.0
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
static double spatialWet=.32;
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
static Source source_at(int s,double t){Source q=S[s];
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
 if(t>=50&&t<=78){double u=(t-50)/28;u=u*u*(3-2*u);double a=TAU*2*u;
 // Eight-turn centrifuge from 62–70 s.  Smoothstep gives it a physical ramp;
 // eight whole turns land at the original orientation with no visual/audio cut.
 if(t>=62){double x=fmin(1,(t-62)/8);x=x*x*(3-2*x);a+=TAU*8*x;}
 double x=q.x,y=q.y;q.x=x*cos(a)-y*sin(a);q.y=x*sin(a)+y*cos(a);q.z+=.22*sin(a+s*.7);}
 // In the visual/listening-field cut, the whole composition physically winds
 // up after its opening assembly. This rotation feeds gravity, Doppler, HRTF,
 // telemetry, and light transport; the camera does not manufacture the spin.
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
static void gong(double t,double root,double g){
 double q[]={1,1.41,1.98,2.91,4.07},a[]={1,.52,.34,.2,.1},d[]={8.5,7.2,6.1,4.8,3.5};
 for(int i=0;i<5;i++)if(NE<MAXE){int n=NE++;E[n]=(Event){t,d[i],root*q[i],root*q[i],g*a[i],.018+i*.006,d[i]*.92,10,0,(uint32_t)(n*2654435761u)};}
}
static double env(const Event*e,double t){double u=t-e->t;if(u<0||u>=e->dur)return 0;double a=fmin(1,u/e->atk),r=fmin(1,(e->dur-u)/e->rel);return sin(a*M_PI/2)*sin(a*M_PI/2)*sin(r*M_PI/2)*sin(r*M_PI/2);}

static const int chord[4][4]={{48,55,64,74},{45,52,60,71},{53,60,69,67},{55,62,71,69}};
static const int root[4]={36,33,41,43};
static const int mel[4][4]={{76,79,81,81},{79,76,74,74},{72,74,76,79},{74,76,76,72}};
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
 for(int b=0;b<38;b++){
  double t=b*BAR, tr=(b>=20&&b<28)?4.0/3:1;
  if(globeMode&&b<4){if(b==0){ev(t,4*BAR+.3,36,.055,0,2.8,2.6);ev(t,4*BAR+.3,43,.038,2,3.4,2.8);ev(t,4*BAR+.3,48,.03,3,4.0,3.0);ev(t,4*BAR+.3,55,.018,4,4.8,3.4);}continue;}
  for(int k=0;k<4;k++)ev(t,BAR+.1,69+12*log2(hz(chord[b%4][k])*tr/440),.034,k<2?2:3,.7,1.0);
  // Continuous independently filtered noise voices.
  double noiseScale=globeMode?.16:1.0;
  for(int q=0;q<4;q++)noisev(t+(q+.5)*BEAT,.085,4200,10200,.065*noiseScale,q&1?7:6);
  noisev(t,BAR,1700,5700,.013*noiseScale,11);
  for(int q=1;q<4;q+=2)noisev(t+q*BEAT,.15,620,2100,.052*noiseScale,q==1?8:9);
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
 if(cosmosMode){double z=a.z+(b.z-a.z)*f,pitch=a.pitch+(b.pitch-a.pitch)*f,roll=a.roll+(b.roll-a.roll)*f;Source sun=system_sun_at(COSMOS[src].system,t);double local=hypot(hypot(so.x-sun.x,so.y-sun.y),so.z-sun.z),dx=sun.x-x,dy=sun.y-y,dz=sun.z-z;
  double cy=cos(h),sy=sin(h),cp=cos(pitch),sp=sin(pitch),cr=cos(roll),sr=sin(roll),forward=cy*dx+sy*dy,right=-sy*dx+cy*dy,up=dz;
  double pf=cp*forward+sp*up,pu=-sp*forward+cp*up,rr=cr*right+sr*pu,uu=-sr*right+cr*pu;*dist=local+sqrt(dx*dx+dy*dy+dz*dz);*az=atan2(rr,pf);*el=atan2(uu,hypot(pf,rr));return;}
 double dx=so.x-x,dy=so.y-y,dz=so.z-1.6,horiz=hypot(dx,dy);*dist=hypot(horiz,dz);*az=atan2(dy,dx)-h;*el=atan2(dz,horiz);
}
static void propagation(int src,double t,double *wall,double *cutoff,double *wind,double *rain){double u=t*CTRL;int i=(int)u,n=(int)(DUR*CTRL);if(i>=n)i=n-1;double f=u-i;Listener a=L[i],b=L[i+1];double x=a.x+(b.x-a.x)*f,y=a.y+(b.y-a.y)*f;Source so=source_at(src,t);double dx=so.x-x,dy=so.y-y,d=hypot(dx,dy);*wall=1;*cutoff=20000;if(duetMode||cosmosMode){*wind=0;*rain=1;return;}
 // Brick barrier at x=1.2, y=-3..2.5, height 2.4. Only an actual ray
 // intersection occludes; high sources and paths around its ends remain clear.
 if((x-1.2)*(so.x-1.2)<0){double q=(1.2-x)/(so.x-x),iy=y+(so.y-y)*q,iz=1.6+(so.z-1.6)*q;if(iy>-3&&iy<2.5&&iz<2.4){*wall=.46;*cutoff=1450;}}
 double wx=.7*cos(t*.11),wy=.7*sin(t*.083+.8);*wind=.0045*(wx*dx+wy*dy)/(d+1);*rain=1-.075*fmin(1,d/8.0)*fabs(sin(t*19.7+src*2.13));}
static void render(void){long n=(long)(DUR*SR);busL=calloc(n,4);busR=calloc(n,4);meterL=calloc(NFRAMES*NSRC,sizeof(*meterL));meterR=calloc(NFRAMES*NSRC,sizeof(*meterR));
 for(int j=0;j<NE;j++){Event*e=&E[j];long s0=(long)(e->t*SR),nn=(long)(e->dur*SR);double ph=0,lp=0,hpLP=0,envLP=0,saz=0,sel=0,sd=0,mode1[3]={0},mode2[3]={0};int spatialInit=0;uint32_t rs=e->seed;ACHrtf hs;memset(&hs,0,sizeof hs);
  for(long k=0;k<nn&&s0+k<n;k++){double t=(s0+k)/(double)SR,u=k/(double)SR,a=env(e,t),az,el,d,wall,cutoff,wind,rain;spatial_params(e->src,t,&az,&el,&d);if(!spatialInit){saz=az;sel=el;sd=d;spatialInit=1;}else{double smooth=1-exp(-1.0/(SR*.018)),da=atan2(sin(az-saz),cos(az-saz));saz+=da*smooth;sel+=(el-sel)*smooth;sd+=(d-sd)*smooth;}az=saz;el=sel;d=sd;propagation(e->src,t,&wall,&cutoff,&wind,&rain);double spaceD=d+(globeMode?(1-a)*18.0:0),microA=fmin(1,u/.006),microR=fmin(1,(e->dur-u)/.028),clickGate=sin(microA*M_PI/2)*sin(microA*M_PI/2)*sin(microR*M_PI/2)*sin(microR*M_PI/2);
   double v;if(e->type==0||e->type==3){double base=e->type==3?e->f0:(cosmosMode?cosmos_frequency(e->src,t):(duetMode?duet_frequency(e->src,t):e->f0*pow(e->f1/e->f0,u/e->dur))),f=base*(1+wind*spatialWet);ph+=TAU*f/SR;v=sin(ph);}
   else {rs=rs*1664525u+1013904223u;double w=((rs>>8)/8388608.0)-1;double ca=exp(-TAU*e->f0/SR),cb=exp(-TAU*e->f1/SR);lp=(1-ca)*w+ca*lp;double hi=w-lp;hpLP=(1-cb)*hi+cb*hpLP;v=hpLP;}
   if(cosmosMode&&e->type!=2&&e->type!=3){double modal=0;ACMeshAcoustics*ma=&BODY_ACOUSTICS[e->src];ACMaterial mm=BODY_MATERIAL[e->src];for(int m=0;m<3;m++){double mf=fmax(45,fmin(12000,ma->mode[m])),r=exp(-M_PI*mf*fmax(.004,mm.loss)/SR),y=2*r*cos(TAU*mf/SR)*mode1[m]-r*r*mode2[m]+(1-r)*v;mode2[m]=mode1[m];mode1[m]=y;modal+=y;}v=.68*v+.32*(modal/3.0);}
   if(cutoff<19000){double c=exp(-TAU*cutoff/SR);envLP=(1-c)*v+c*envLP;v=envLP;}v*=wall*rain*e->g*clickGate*field_gain(e->src,t)*(cosmosMode?cosmos_plane_gain(e->src,t,az,el):1);float hl,hr;ac_hrtf_process(&hs,(float)v,az,el,spaceD,&hl,&hr);
   double dryPan=fmax(-.62,fmin(.62,S[e->src].x/6.0)),range=(globeMode||cosmosMode)?.008+.992/(1+.32*spaceD*spaceD):1,dl=sqrt((1-dryPan)*.5)*range,dr=sqrt((1+dryPan)*.5)*range;
   double dg=cos(spatialWet*M_PI*.5),wg=sin(spatialWet*M_PI*.5),cl=v*dl*dg+hl*wg,cr=v*dr*dg+hr*wg;
   // Retarded reception: radio energy is written when it reaches the listener,
   // not when the emitter produced it. Distance therefore becomes real delay.
   double arrival=s0+k+spaceD*SR/343.0;long at=(long)floor(arrival);double af=arrival-at;if(at>=0&&at<n){busL[at]+=(float)(cl*(1-af));busR[at]+=(float)(cr*(1-af));if(at+1<n){busL[at+1]+=(float)(cl*af);busR[at+1]+=(float)(cr*af);}
    // Per-emitter air/room tail; unlike the later master reflections this delay
    // remains attached to the originating voice and its stereo observation.
    long echo=at+(long)((.075+.011*e->src)*SR);if(!duetMode&&!cosmosMode&&echo<n){busL[echo]+=(float)(cl*.055);busR[echo]+=(float)(cr*.055);}
    int fr=(int)(at*FPS/(double)SR);if(fr>=0&&fr<NFRAMES){int mi=fr*NSRC+e->src;meterL[mi]+=cl*cl;meterR[mi]+=cr*cr;}}
  }
 }
 // Cross-room early reflections.
 int ds[]={3408,5424,8688};double dg[]={.075,.048,.03};if(!duetMode&&!cosmosMode)for(int q=0;q<3;q++)for(long i=ds[q];i<n;i++){float l=busL[i-ds[q]],r=busR[i-ds[q]];double room=.22+.78*spatialWet;busL[i]+=r*dg[q]*room;busR[i]+=l*dg[q]*room;}
 double pk=0;for(long i=0;i<n;i++){double a=fmax(fabs(busL[i]),fabs(busR[i]));if(a>pk)pk=a;}double g=pk?.88/pk:1;
 for(long i=0;i<n;i++){double fo=i>n-SR?((n-i)/(double)SR):1;busL[i]*=g*fo;busR[i]*=g*fo;}
}
static int wav(const char*p){FILE*f=fopen(p,"wb");if(!f)return 0;long n=(long)(DUR*SR);uint32_t ds=n*8,sz=36+ds,sr=SR,br=SR*8,fs=16;uint16_t fm=3,ch=2,ba=8,bi=32;
 fwrite("RIFF",1,4,f);fwrite(&sz,4,1,f);fwrite("WAVEfmt ",1,8,f);fwrite(&fs,4,1,f);fwrite(&fm,2,1,f);fwrite(&ch,2,1,f);fwrite(&sr,4,1,f);fwrite(&br,4,1,f);fwrite(&ba,2,1,f);fwrite(&bi,2,1,f);fwrite("data",1,4,f);fwrite(&ds,4,1,f);for(long i=0;i<n;i++){fwrite(&busL[i],4,1,f);fwrite(&busR[i],4,1,f);}fclose(f);return 1;}
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
static void video(const char*wavp,const char*outp){char cmd[2048];size_t ol=strlen(outp);int lossless=ol>=4&&!strcmp(outp+ol-4,".mov");const char*clean=globeMode?"-af 'highpass=f=30,equalizer=f=7600:t=q:w=.75:g=-4,lowpass=f=11800,alimiter=limit=.90:attack=6:release=110,volume=.88'":"",*pixels=(globeMode&&!acousticsView)?"-vf 'scale=240:240:flags=neighbor,scale=720:720:flags=neighbor'":"";if(lossless)
 snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f image2pipe -vcodec ppm -r %d -i - -i '%s' -c:v libx265 -preset medium -x265-params lossless=1:log-level=error -pix_fmt yuv444p -tag:v hvc1 %s -c:a pcm_s24le -ar 48000 -shortest '%s'",FPS,wavp,clean,outp);
 else snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f image2pipe -vcodec ppm -r %d -i - -i '%s' %s -c:v libx264 -preset slow -pix_fmt yuv420p -crf 14 %s -c:a aac -b:a 320k -shortest '%s'",FPS,wavp,pixels,clean,outp);FILE*ff=popen(cmd,"w");if(!ff)return;unsigned char*p=malloc(W*H*3);
 for(int fr=0;fr<(int)(DUR*FPS);fr++){double t=fr/(double)FPS;for(int y=0;y<H;y++){unsigned char v=brightMode?(unsigned char)(250-15*y/(double)H):(unsigned char)(24-10*y/(double)H);for(int x=0;x<W;x++){int o=(y*W+x)*3;p[o]=v;p[o+1]=(unsigned char)fmax(0,v-(brightMode?3:1));p[o+2]=(unsigned char)fmin(255,v+(brightMode?0:2));}}int li=(int)(t*CTRL);Listener l=L[li];
  if(globeMode){globe_frame(p,fr,t,l);fprintf(ff,"P6\n%d %d\n255\n",W,H);fwrite(p,1,W*H*3,ff);continue;}
  // Camera begins among the sources, orbits with the listener, then after 78 s
  // retreats far beyond the room until the ensemble is a small constellation.
  int ts=tour_source(t),tn=TOUR[(ts==TOUR[11])?0:((int)(t/9.0)+1)%12];double phase=fmod(t,9.0)/9.0,lookMix=phase<.78?0:(phase-.78)/.22;lookMix=lookMix*lookMix*(3-2*lookMix);Source la=source_at(ts,t),lb=source_at(tn,t);
  // First-person radio receiver at ear height. Smooth focus handoffs avoid cuts;
  // parallax and changing radii now reveal the listener's physical travel.
  V3 cam={l.x,l.y,1.62},target={la.x+(lb.x-la.x)*lookMix,la.y+(lb.y-la.y)*lookMix,la.z+(lb.z-la.z)*lookMix};
  // Faceted receiver-world horizon. Large triangles scroll gently with travel,
  // providing scale without pretending to be another sound source.
  int horizon=brightMode?300:330,shift=(int)(l.x*9+l.y*5);for(int q=-1;q<10;q++){int x0=q*92-(shift%92),x1=x0+46,x2=x0+92,y1=horizon-28-(int)(38*(.5+.5*sin(q*2.31+l.heading)));uint32_t c1=brightMode?(q&1?0xc9d2cf:0xbcc8c6):(q&1?0x20282b:0x182124);tri2(p,x0,horizon,x1,y1,x2,horizon,c1,.82);tri2(p,x0,horizon,x2,horizon,x2,H-205,brightMode?0xd9ded8:0x111719,.52);}line2(p,0,horizon,W,horizon,brightMode?0x7f9292:0x52656a,.55);
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
  fprintf(ff,"P6\n%d %d\n255\n",W,H);fwrite(p,1,W*H*3,ff);
 }
 free(p);pclose(ff);
}
  int main(int argc,char**argv){const char*w="../out/spatial-sineabye.wav",*v="../out/spatial-sineabye.mp4",*mp3=NULL,*ptStill=NULL,*sceneOut=NULL;double ptTime=32;int ptSpp=32,sceneFrames=NFRAMES;for(int i=1;i<argc;i++){if(!strcmp(argv[i],"--wav")&&i+1<argc)w=argv[++i];else if(!strcmp(argv[i],"--video")&&i+1<argc)v=argv[++i];else if(!strcmp(argv[i],"--mp3")&&i+1<argc)mp3=argv[++i];else if(!strcmp(argv[i],"--spatial-wet")&&i+1<argc)spatialWet=fmax(0,fmin(1,atof(argv[++i])));else if(!strcmp(argv[i],"--bright")){brightMode=1;themeExplicit=1;}else if(!strcmp(argv[i],"--dark")){brightMode=0;themeExplicit=1;}else if(!strcmp(argv[i],"--theme")&&i+1<argc){const char*th=argv[++i];if(!strcmp(th,"light"))brightMode=1;else if(!strcmp(th,"dark"))brightMode=0;themeExplicit=strcmp(th,"auto")!=0;}else if(!strcmp(argv[i],"--globe"))globeMode=1;else if(!strcmp(argv[i],"--lattice-duet")){globeMode=1;duetMode=1;voiceCount=4;}else if(!strcmp(argv[i],"--cosmos")){globeMode=1;cosmosMode=1;voiceCount=12;}else if(!strcmp(argv[i],"--camera")&&i+1<argc){cameraMode=!strcmp(argv[++i],"ship");}else if(!strcmp(argv[i],"--voices")&&i+1<argc)voiceCount=atoi(argv[++i]);else if(!strcmp(argv[i],"--pathtrace-still")&&i+1<argc)ptStill=argv[++i];else if(!strcmp(argv[i],"--pt-time")&&i+1<argc)ptTime=atof(argv[++i]);else if(!strcmp(argv[i],"--pt-spp")&&i+1<argc)ptSpp=atoi(argv[++i]);else if(!strcmp(argv[i],"--scene-data")&&i+1<argc)sceneOut=argv[++i];else if(!strcmp(argv[i],"--scene-frames")&&i+1<argc)sceneFrames=atoi(argv[++i]);}resolve_render_theme();
 for(int i=1;i+1<argc;i++)if(!strcmp(argv[i],"--visual"))acousticsView=!strcmp(argv[i+1],"acoustics");
 score();filter_score();simulate();if(ptStill){globeMode=1;if(!pathtrace_still(ptStill,fmax(0,fmin(DUR,ptTime)),fmax(1,ptSpp))){fprintf(stderr,"pathtrace write failed\n");return 1;}fprintf(stderr,"✓ %s · path traced · %d spp\n",ptStill,ptSpp);return 0;}render();if(sceneOut&&!export_scene(sceneOut,fmax(1,fmin(NFRAMES,sceneFrames)))){fprintf(stderr,"scene export failed\n");return 1;}if(!wav(w)){fprintf(stderr,"write failed\n");return 1;}fprintf(stderr,"✓ %s · %.1fs · %d sound bodies/events · %d voices · spatial wet %.0f%%\n",w,DUR,NE,voiceCount,spatialWet*100);
 if(mp3){char cmd[4096];snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -i '%s' -af 'highpass=f=28,equalizer=f=72:t=q:w=.8:g=1.2,equalizer=f=7200:t=q:w=.9:g=-1,lowpass=f=15800,alimiter=limit=.90:attack=6:release=100,volume=.84' -c:a libmp3lame -q:a 2 '%s'",w,mp3);if(system(cmd)!=0)return 1;}
 if(v&&strcmp(v,"none"))video(w,v);return 0;}
