// ac_prompt_rock.h — C port of Slab's SigilMesh prompt-rock generator.
// Same SplitMix64 stream, icosahedron, hashed subdivision, radial displacement,
// and anisotropic scale. Geometry also supplies deterministic acoustic modes.
#ifndef AC_PROMPT_ROCK_H
#define AC_PROMPT_ROCK_H
#include <math.h>
#include <stdint.h>
#include <string.h>

typedef struct { double x,y,z; } ACRockV;
typedef struct { int a,b,c; } ACRockFace;
typedef struct { ACRockV v[42]; ACRockFace f[80]; int nv,nf; double roughness,scale[3],extent[3]; } ACRock;
typedef struct { uint64_t s; } ACRockRng;
static uint64_t ac_rock_next(ACRockRng*r){r->s+=UINT64_C(0x9e3779b97f4a7c15);uint64_t z=r->s;z=(z^(z>>30))*UINT64_C(0xbf58476d1ce4e5b9);z=(z^(z>>27))*UINT64_C(0x94d049bb133111eb);return z^(z>>31);}
static double ac_rock_unit(ACRockRng*r){return (ac_rock_next(r)>>40)/(double)(1u<<24);}
static ACRockV ac_rv(double x,double y,double z){ACRockV v={x,y,z};return v;}
static ACRockV ac_rnorm(ACRockV v){double n=sqrt(v.x*v.x+v.y*v.y+v.z*v.z);return ac_rv(v.x/n,v.y/n,v.z/n);}
static int ac_rock_mid(ACRock*r,int a,int b,int ea[30],int eb[30],int em[30],int*ne){if(a>b){int q=a;a=b;b=q;}for(int i=0;i<*ne;i++)if(ea[i]==a&&eb[i]==b)return em[i];ACRockV p=ac_rnorm(ac_rv((r->v[a].x+r->v[b].x)*.5,(r->v[a].y+r->v[b].y)*.5,(r->v[a].z+r->v[b].z)*.5));int m=r->nv;r->v[r->nv++]=p;ea[*ne]=a;eb[*ne]=b;em[*ne]=m;(*ne)++;return m;}
static void ac_rock_generate(uint64_t seed,ACRock*r){
 memset(r,0,sizeof(*r));ACRockRng rng={seed?seed:UINT64_C(0x9e3779b97f4a7c15)};double t=(1+sqrt(5.0))/2;
 ACRockV raw[12]={ac_rv(-1,t,0),ac_rv(1,t,0),ac_rv(-1,-t,0),ac_rv(1,-t,0),ac_rv(0,-1,t),ac_rv(0,1,t),ac_rv(0,-1,-t),ac_rv(0,1,-t),ac_rv(t,0,-1),ac_rv(t,0,1),ac_rv(-t,0,-1),ac_rv(-t,0,1)};
 for(int i=0;i<12;i++)r->v[i]=ac_rnorm(raw[i]);r->nv=12;
 ACRockFace base[20]={{0,11,5},{0,5,1},{0,1,7},{0,7,10},{0,10,11},{1,5,9},{5,11,4},{11,10,2},{10,7,6},{7,1,8},{3,9,4},{3,4,2},{3,2,6},{3,6,8},{3,8,9},{4,9,5},{2,4,11},{6,2,10},{8,6,7},{9,8,1}};memcpy(r->f,base,sizeof(base));r->nf=20;
 int subdivide=ac_rock_unit(&rng)>=.34;if(subdivide){ACRockFace next[80];int ea[30],eb[30],em[30],ne=0,nf=0;for(int i=0;i<20;i++){int a=r->f[i].a,b=r->f[i].b,c=r->f[i].c,ab=ac_rock_mid(r,a,b,ea,eb,em,&ne),bc=ac_rock_mid(r,b,c,ea,eb,em,&ne),ca=ac_rock_mid(r,c,a,ea,eb,em,&ne);next[nf++]=(ACRockFace){a,ab,ca};next[nf++]=(ACRockFace){b,bc,ab};next[nf++]=(ACRockFace){c,ca,bc};next[nf++]=(ACRockFace){ab,bc,ca};}memcpy(r->f,next,sizeof(next));r->nf=80;}
 double amp=.10+.30*ac_rock_unit(&rng);r->roughness=amp;for(int k=0;k<3;k++)r->scale[k]=.66+.64*ac_rock_unit(&rng);
 double lo[3]={1e9,1e9,1e9},hi[3]={-1e9,-1e9,-1e9};for(int i=0;i<r->nv;i++){double d=1+(ac_rock_unit(&rng)-.5)*2*amp;r->v[i].x*=d*r->scale[0];r->v[i].y*=d*r->scale[1];r->v[i].z*=d*r->scale[2];double p[3]={r->v[i].x,r->v[i].y,r->v[i].z};for(int k=0;k<3;k++){if(p[k]<lo[k])lo[k]=p[k];if(p[k]>hi[k])hi[k]=p[k];}}
 for(int k=0;k<3;k++)r->extent[k]=hi[k]-lo[k];
}
static void ac_rock_modes(const ACRock*r,double modes[5],double gains[5]){
 double ex=r->extent[0],ey=r->extent[1],ez=r->extent[2],body=4500/(ex+ey+ez),facet=r->nf==20?1.18:1.0,rough=1+r->roughness*.65;
 modes[0]=body*sqrt(1/(ex*ex)+1/(ey*ey))*facet;modes[1]=body*sqrt(1/(ey*ey)+1/(ez*ez))*1.47;modes[2]=body*sqrt(1/(ex*ex)+1/(ez*ez))*2.13;modes[3]=(modes[0]+modes[1])*1.73*rough;modes[4]=(modes[1]+modes[2])*2.08*rough;
 double g[5]={.46,.34,.26,.19,.13};memcpy(gains,g,sizeof(g));
}
#endif
