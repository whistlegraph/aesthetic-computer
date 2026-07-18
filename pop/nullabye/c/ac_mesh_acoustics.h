// ac_mesh_acoustics.h — reduced-order material acoustics for prompt-rock meshes.
#ifndef AC_MESH_ACOUSTICS_H
#define AC_MESH_ACOUSTICS_H
#include "ac_prompt_rock.h"

typedef struct { double density,young,loss,absorption,radiation; } ACMaterial;
typedef struct { double volume,area,inertia[3],mode[6],decay[6],directivity; } ACMeshAcoustics;
static const ACMaterial AC_MAT_WOOD={690,10.5e9,.034,.22,.72};
static const ACMaterial AC_MAT_ALUMINUM={2700,69e9,.006,.05,.91};
static const ACMaterial AC_MAT_GLASS={2500,70e9,.012,.08,.84};
static const ACMaterial AC_MAT_STONE={2700,45e9,.022,.16,.63};

static ACRockV ac_avsub(ACRockV a,ACRockV b){return ac_rv(a.x-b.x,a.y-b.y,a.z-b.z);}
static ACRockV ac_avcross(ACRockV a,ACRockV b){return ac_rv(a.y*b.z-a.z*b.y,a.z*b.x-a.x*b.z,a.x*b.y-a.y*b.x);}
static double ac_avdot(ACRockV a,ACRockV b){return a.x*b.x+a.y*b.y+a.z*b.z;}
static void ac_mesh_analyze(const ACRock*r,ACMaterial m,ACMeshAcoustics*a){
 memset(a,0,sizeof(*a));for(int i=0;i<r->nf;i++){ACRockFace f=r->f[i];ACRockV p=r->v[f.a],q=r->v[f.b],s=r->v[f.c],n=ac_avcross(ac_avsub(q,p),ac_avsub(s,p));a->area+=.5*sqrt(ac_avdot(n,n));a->volume+=ac_avdot(p,ac_avcross(q,s))/6;}
 a->volume=fabs(a->volume);double mass=fmax(1e-6,a->volume*m.density),ex=r->extent[0],ey=r->extent[1],ez=r->extent[2];a->inertia[0]=mass*(ey*ey+ez*ez)/12;a->inertia[1]=mass*(ex*ex+ez*ez)/12;a->inertia[2]=mass*(ex*ex+ey*ey)/12;
 double wave=sqrt(m.young/m.density),length[6]={ex,ey,ez,sqrt(ex*ey),sqrt(ey*ez),sqrt(ex*ez)},factor[6]={.42,.47,.53,.71,.89,1.17};for(int k=0;k<6;k++){a->mode[k]=fmax(22,wave/(6.283185307179586*fmax(.08,length[k]))*factor[k]);a->decay[k]=1/(m.loss*6.283185307179586*a->mode[k]+1e-6);}
 double longest=fmax(ex,fmax(ey,ez)),shortest=fmin(ex,fmin(ey,ez));a->directivity=m.radiation*fmin(.92,fmax(.08,1-shortest/(longest+1e-6)));
}
// Cosine-power blend between diffuse enclosure radiation and the dominant
// speaker plane. `facingDot` is the oriented mesh-forward · receiver direction.
static double ac_mesh_radiation(const ACMeshAcoustics*a,ACMaterial m,double facingDot){double front=pow(fmax(0,facingDot),1+5*a->directivity),back=.08+.22*(1-a->directivity);return m.radiation*((1-a->directivity)+a->directivity*(front+(facingDot<0?back:0)))*(1-m.absorption);}
// Area-weighted radiation from every polygon center/normal. The mesh's local
// +Z axis is aimed at the receiver; phase supplies the secondary skeletal rock.
static double ac_mesh_facet_radiation(const ACRock*r,ACMaterial m,double phase){double sum=0,area=0,tx=.16*sin(phase*.37),ty=.12*cos(phase*.29);for(int i=0;i<r->nf;i++){ACRockFace f=r->f[i];ACRockV a=r->v[f.a],b=r->v[f.b],c=r->v[f.c],n=ac_avcross(ac_avsub(b,a),ac_avsub(c,a));double ar=.5*sqrt(ac_avdot(n,n));if(ar<1e-9)continue;n=ac_rv(n.x/(2*ar),n.y/(2*ar),n.z/(2*ar));double facing=n.z+tx*n.x+ty*n.y,front=pow(fmax(0,facing),2.4),back=facing<0?.055:0;sum+=ar*(front+back);area+=ar;}double diaphragm=.90+.10*sin(phase);return m.radiation*(1-m.absorption)*diaphragm*(.32+.95*sum/(area+1e-9));}
#endif
