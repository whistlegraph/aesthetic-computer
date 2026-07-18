#ifndef AC_TERRARIUM_PT_H
#define AC_TERRARIUM_PT_H

// Small allocation-free CPU path tracer for spatial-sineabye's luminous world.
// It traces diffuse/emissive bodies, a dielectric listening shell, and colored
// participating media. The caller owns animation, audio, and graphic overlays.

typedef struct { double x,y,z; } PTVec;
typedef struct { PTVec center,color; double radius,emission,roughness; } PTBody;
typedef struct { uint64_t state; } PTRng;
#ifdef __CUDACC__
#define PT_HD __host__ __device__
#else
#define PT_HD
#endif

static PT_HD inline PTVec ptv(double x,double y,double z){return(PTVec){x,y,z};}
static PT_HD inline PTVec pt_add(PTVec a,PTVec b){return ptv(a.x+b.x,a.y+b.y,a.z+b.z);}
static PT_HD inline PTVec pt_sub(PTVec a,PTVec b){return ptv(a.x-b.x,a.y-b.y,a.z-b.z);}
static PT_HD inline PTVec pt_mul(PTVec a,double b){return ptv(a.x*b,a.y*b,a.z*b);}
static PT_HD inline PTVec pt_had(PTVec a,PTVec b){return ptv(a.x*b.x,a.y*b.y,a.z*b.z);}
static PT_HD inline double pt_dot(PTVec a,PTVec b){return a.x*b.x+a.y*b.y+a.z*b.z;}
static PT_HD inline PTVec pt_cross(PTVec a,PTVec b){return ptv(a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x);}
static PT_HD inline PTVec pt_norm(PTVec a){double n=sqrt(pt_dot(a,a));return n?pt_mul(a,1/n):a;}
static PT_HD inline uint64_t pt_hash(uint64_t x){x^=x>>30;x*=0xbf58476d1ce4e5b9ULL;x^=x>>27;x*=0x94d049bb133111ebULL;return x^(x>>31);}
static PT_HD inline double pt_rand(PTRng*r){r->state=pt_hash(r->state+0x9e3779b97f4a7c15ULL);return(r->state>>11)*(1.0/9007199254740992.0);}
static PT_HD inline PTVec pt_cosine(PTRng*r,PTVec n){double a=TAU*pt_rand(r),z=sqrt(pt_rand(r)),q=sqrt(1-z*z);PTVec h=fabs(n.z)<.9?ptv(0,0,1):ptv(1,0,0),u=pt_norm(pt_cross(h,n)),v=pt_cross(n,u);return pt_norm(pt_add(pt_add(pt_mul(u,cos(a)*q),pt_mul(v,sin(a)*q)),pt_mul(n,z)));}
static PT_HD inline double pt_sphere(PTVec ro,PTVec rd,PTVec c,double radius){PTVec oc=pt_sub(ro,c);double b=pt_dot(oc,rd),h=b*b-pt_dot(oc,oc)+radius*radius;if(h<0)return 1e30;h=sqrt(h);double t=-b-h;return t>1e-4?t:(-b+h>1e-4?-b+h:1e30);}
static PT_HD inline PTVec pt_sky(PTVec d){double h=.5+.5*d.z;return ptv(.006+.018*h,.009+.028*h,.018+.065*h);}

static PT_HD PTVec pt_trace(PTVec ro,PTVec rd,const PTBody*b,int count,PTRng*r,double time){
 PTVec light=ptv(0,0,0),through=ptv(1,1,1);
#ifdef PT_NO_SHELL
 int inside=1;
#else
 int inside=pt_dot(ro,ro)<6.8*6.8;
#endif
 for(int bounce=0;bounce<7;bounce++){
  double nearest=1e30;int hit=-1;for(int i=0;i<count;i++){double t=pt_sphere(ro,rd,b[i].center,b[i].radius);if(t<nearest){nearest=t;hit=i;}}
#ifndef PT_NO_SHELL
  double shell=pt_sphere(ro,rd,ptv(0,0,0),6.8);if(shell<nearest){
   PTVec hp=pt_add(ro,pt_mul(rd,shell)),n=pt_norm(hp);if(inside)n=pt_mul(n,-1);double eta=inside?1.46:1/1.46,ci=-pt_dot(n,rd),k=1-eta*eta*(1-ci*ci),f=.04+.96*pow(1-ci,5);int reflect=k<0||pt_rand(r)<f;
   if(reflect){rd=pt_sub(rd,pt_mul(n,2*pt_dot(rd,n)));ro=pt_add(hp,pt_mul(n,1e-3));}else{rd=pt_norm(pt_add(pt_mul(rd,eta),pt_mul(n,eta*ci-sqrt(k))));ro=pt_sub(hp,pt_mul(n,1e-3));inside=!inside;}through=pt_mul(through,.965);continue;
  }
#endif
  // Homogeneous colored fog inside the listening shell. Free-flight sampling
  // creates soft shafts and lets emitter colors mix in space.
  if(inside){double density=.045,freePath=-log(fmax(1e-9,1-pt_rand(r)))/density;if(freePath<nearest){ro=pt_add(ro,pt_mul(rd,freePath));
    // Next-event sampling of every luminous body gives smooth colored shafts
    // instead of requiring a random fog bounce to hit a tiny emitter.
    for(int i=0;i<count;i++)if(b[i].emission>1){PTVec to=pt_sub(b[i].center,ro);double d2=pt_dot(to,to),d=sqrt(d2),vis=1;PTVec ld=pt_mul(to,1/d);for(int o=0;o<count;o++)if(o!=i&&pt_sphere(ro,ld,b[o].center,b[o].radius)<d){vis=0;break;}if(vis){double phase=.3+.7*pow(fmax(0,pt_dot(rd,ld)),3),gain=b[i].emission*phase*exp(-density*d)/(2.5+d2);light=pt_add(light,pt_mul(pt_had(through,b[i].color),gain));}}
    PTVec fog=ptv(.78+.05*sin(time*.07),.86,.96);through=pt_had(through,fog);rd=pt_cosine(r,pt_norm(ptv(pt_rand(r)-.5,pt_rand(r)-.5,pt_rand(r)-.5)));continue;}}
  if(hit<0){light=pt_add(light,pt_had(through,pt_sky(rd)));break;}
  PTVec hp=pt_add(ro,pt_mul(rd,nearest)),n=pt_norm(pt_sub(hp,b[hit].center));
  if(b[hit].emission>0){light=pt_add(light,pt_mul(pt_had(through,b[hit].color),b[hit].emission));break;}
  // Direct emitter sampling on matte bodies reveals colored indirect fill.
  for(int i=0;i<count;i++)if(b[i].emission>1&&i!=hit){PTVec to=pt_sub(b[i].center,hp);double d2=pt_dot(to,to),d=sqrt(d2),nl=fmax(0,pt_dot(n,pt_mul(to,1/d)));if(nl>0)light=pt_add(light,pt_mul(pt_had(through,pt_had(b[hit].color,b[i].color)),b[i].emission*nl/(1.5+d2)));}
  through=pt_had(through,b[hit].color);ro=pt_add(hp,pt_mul(n,1e-3));PTVec diffuse=pt_cosine(r,n),refl=pt_sub(rd,pt_mul(n,2*pt_dot(rd,n)));rd=pt_norm(pt_add(pt_mul(refl,1-b[hit].roughness),pt_mul(diffuse,b[hit].roughness)));
  if(bounce>3){double survive=fmin(.94,fmax(through.x,fmax(through.y,through.z)));if(pt_rand(r)>survive)break;through=pt_mul(through,1/survive);}
 }
 return light;
}

static void pt_render_rgb(unsigned char*out,int w,int h,PTVec cam,PTVec target,const PTBody*b,int count,int spp,int frame,double time){
 PTVec f=pt_norm(pt_sub(target,cam)),right=pt_norm(pt_cross(f,ptv(0,0,1))),up=pt_cross(right,f);double aspect=w/(double)h,tanHalf=.72;
 for(int y=0;y<h;y++)for(int x=0;x<w;x++){PTVec sum=ptv(0,0,0);for(int s=0;s<spp;s++){PTRng rng={pt_hash((uint64_t)frame*0x9e3779b9ULL+(uint64_t)(y*w+x)*1315423911ULL+s)};double px=(2*((x+pt_rand(&rng))/w)-1)*aspect*tanHalf,py=(1-2*((y+pt_rand(&rng))/h))*tanHalf;PTVec rd=pt_norm(pt_add(f,pt_add(pt_mul(right,px),pt_mul(up,py))));sum=pt_add(sum,pt_trace(cam,rd,b,count,&rng,time));}sum=pt_mul(sum,1.0/spp);
  // Filmic compression + display gamma preserves emitter color above white.
  double c[3]={sum.x,sum.y,sum.z};for(int k=0;k<3;k++){double v=c[k];v=v/(1+v);v=pow(fmax(0,v),1/2.2);out[(y*w+x)*3+k]=(unsigned char)fmin(255,v*255+.5);}
 }
}

#endif
