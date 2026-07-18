#include <cuda_runtime.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#define TAU 6.28318530717958647692
#define PT_NO_SHELL 1
#include "ac_terrarium_pt.h"

__global__ static void render_kernel(unsigned char*out,int w,int h,PTVec cam,PTVec target,const PTBody*b,int count,int spp,int frame,double time){
 int x=blockIdx.x*blockDim.x+threadIdx.x,y=blockIdx.y*blockDim.y+threadIdx.y;if(x>=w||y>=h)return;
 PTVec f=pt_norm(pt_sub(target,cam)),right=pt_norm(pt_cross(f,ptv(0,0,1))),up=pt_cross(right,f),sum=ptv(0,0,0);double aspect=w/(double)h,tanHalf=.72;
 for(int s=0;s<spp;s++){PTRng rng={pt_hash((uint64_t)frame*0x9e3779b9ULL+(uint64_t)(y*w+x)*1315423911ULL+s)};double px=(2*((x+pt_rand(&rng))/w)-1)*aspect*tanHalf,py=(1-2*((y+pt_rand(&rng))/h))*tanHalf;PTVec rd=pt_norm(pt_add(f,pt_add(pt_mul(right,px),pt_mul(up,py))));sum=pt_add(sum,pt_trace(cam,rd,b,count,&rng,time));}sum=pt_mul(sum,1.0/spp);
 double c[3]={sum.x,sum.y,sum.z};for(int k=0;k<3;k++){double v=c[k];v=v/(1+v);v=pow(fmax(0.0,v),1/2.2);out[(y*w+x)*3+k]=(unsigned char)fmin(255.0,v*255+.5);}
}

static void ck(cudaError_t e,const char*where){if(e!=cudaSuccess){fprintf(stderr,"%s: %s\n",where,cudaGetErrorString(e));exit(1);}}
static double bounce(double p){double u=p-floor(p);return 1-4*fabs(u-.5);}
static PTVec pitch_color(double hz){double h=fmod(.04+.78*fmax(0.0,fmin(1.0,log2(fmax(40.0,hz)/40.0)/8.0)),1.0),q=h*6,x=1-fabs(fmod(q,2.0)-1);int k=(int)q;PTVec c=k==0?ptv(1,x,0):k==1?ptv(x,1,0):k==2?ptv(0,1,x):k==3?ptv(0,x,1):k==4?ptv(x,0,1):ptv(1,0,x);return pt_add(pt_mul(c,.88),ptv(.12,.12,.12));}
static void scene(PTBody*host,double time){
 const uint32_t colors[12]={0x4ecdc4,0xff6b6b,0x63cdda,0x778beb,0xf6c915,0xf8a5c2,0x59656a,0x84979d,0xa29bfe,0x81ecec,0xe0a464,0xb2bec3};
 static const double enter[12]={1.0,4.2,1.7,2.4,.3,5.0,6.3,7.2,3.2,3.7,8.1,9.2};
 for(int i=0;i<12;i++){double a=TAU*i/12+.14*sin(time*.21+i),r=2.0+.14*i,x=cos(a)*r+1.05*bounce(time/(7.1+i*.31)+i*.173),y=sin(a)*r+1.0*bounce(time/(8.7+i*.23)+i*.271),z=-1.9+(i%4)*1.3+1.25*bounce(time/(6.3+i*.19)+i*.119);double d=sqrt(x*x+y*y+z*z),u=(time-enter[i])/2.2;if(u<0)u=0;if(u>1)u=1;u=u*u*(3-2*u);if(u<1){double outer=9.2+.15*i,k=outer/fmax(.1,d);x=x*u+x*k*(1-u);y=y*u+y*k*(1-u);z=z*u+z*k*(1-u);}uint32_t c=colors[i];host[i]=(PTBody){ptv(x,y,z),ptv(((c>>16)&255)/255.0,((c>>8)&255)/255.0,(c&255)/255.0),.22+(i%3)*.08,(7.0+(i%5)*2.5)*u,.18};}
 host[12]=(PTBody){ptv(.45*sin(time*.31),.35*cos(time*.27),.2),ptv(1,.57,.16),.24,4,.3};host[13]=(PTBody){ptv(-2.7,2.1,-2.8),ptv(.12,.38,.31),1.15,0,.88};host[14]=(PTBody){ptv(2.9,1.5,-2.25),ptv(.42,.12,.34),.82,0,.72};host[15]=(PTBody){ptv(.3,-3.1,-2.65),ptv(.12,.2,.48),1,0,.82};
}
int main(int argc,char**argv){int video=argc>1&&!strcmp(argv[1],"--video"),spp=video?(argc>3?atoi(argv[3]):16):(argc>1?atoi(argv[1]):128),frames=video?(argc>4?atoi(argv[4]):360):1,w=720,h=720,duetVisual=video&&argc>6&&!strcmp(argv[6],"duet");const char*out=video?(argc>2?argv[2]:"terrarium-cuda.mp4"):(argc>2?argv[2]:"terrarium-cuda.ppm"),*scenePath=video&&argc>5?argv[5]:NULL;PTBody host[16]={0};float*sceneData=NULL;int sceneFrames=0,sceneStride=64,rowStride=5;
 if(scenePath){FILE*sf=fopen(scenePath,"rb");uint32_t magic,ver,count,fps;fread(&magic,4,1,sf);fread(&ver,4,1,sf);fread(&sceneFrames,4,1,sf);fread(&fps,4,1,sf);fread(&count,4,1,sf);if(magic!=0x53434e45||count!=12){fprintf(stderr,"bad scene data\n");return 1;}rowStride=ver>=2?6:5;sceneStride=4+12*rowStride;sceneData=(float*)malloc((size_t)sceneFrames*sceneStride*sizeof(float));fread(sceneData,sizeof(float),(size_t)sceneFrames*sceneStride,sf);fclose(sf);if(frames>sceneFrames)frames=sceneFrames;}
 unsigned char*rgb;PTBody*b;ck(cudaMallocManaged(&rgb,w*h*3),"pixels");ck(cudaMallocManaged(&b,sizeof(host)),"bodies");dim3 block(16,16),grid((w+15)/16,(h+15)/16);cudaEvent_t start,stop;ck(cudaEventCreate(&start),"event start");ck(cudaEventCreate(&stop),"event stop");FILE*f=NULL;if(video){char cmd[1024];snprintf(cmd,sizeof cmd,"ffmpeg -hide_banner -y -loglevel error -f image2pipe -vcodec ppm -r 24 -i - -c:v h264_nvenc -preset p7 -tune hq -rc vbr -cq 16 -b:v 0 -pix_fmt yuv420p '%s'",out);f=popen(cmd,"w");}else f=fopen(out,"wb");double total=0;
 for(int frame=0;frame<frames;frame++){double time=video?frame/24.0:32;scene(host,time);PTVec listener=ptv(0,0,1.6);if(sceneData){float*d=sceneData+(size_t)frame*sceneStride;time=d[0];listener=ptv(d[1],d[2],d[3]);for(int s=0;s<12;s++){float*r=d+4+s*rowStride;if(duetVisual&&s!=0&&s!=4){host[s].radius=.001;host[s].emission=0;continue;}PTVec world=ptv(r[0],r[1],r[2]),offset=pt_sub(world,listener);double heard=hypot(r[3],r[4]),pitch=rowStride>5?r[5]:440,pulse=heard/(heard+.002);host[s].center=pt_add(listener,pt_mul(offset,1-.38*pulse));if(pitch>0)host[s].color=pitch_color(pitch);host[s].emission=pitch>0?7.5:.42;double low=pitch>0?1-fmax(0.0,fmin(1.0,log2(pitch/45.0)/7.5)):0;host[s].radius=.14+.38*low;host[s].roughness=.12+.7*(1-low);}}
  host[12].center=listener;host[12].emission=3.2;host[13]=(PTBody){ptv(listener.x-5,listener.y+3,-3.4),ptv(.08,.32,.25),2.2,0,.9};host[14]=(PTBody){ptv(listener.x+5,listener.y+2,-3.0),ptv(.36,.1,.31),1.7,0,.82};host[15]=(PTBody){ptv(listener.x,listener.y,-101.8),ptv(.055,.105,.12),100,0,.96};memcpy(b,host,sizeof(host));double drift=.08*sin(time*.055);PTVec cam=ptv(listener.x+11.8*cos(-.78+drift),listener.y+11.8*sin(-.78+drift),listener.z+11.5),target=ptv(listener.x,listener.y,0);ck(cudaEventRecord(start),"record start");render_kernel<<<grid,block>>>(rgb,w,h,cam,target,b,16,spp,frame,time);ck(cudaGetLastError(),"launch");ck(cudaEventRecord(stop),"record stop");ck(cudaEventSynchronize(stop),"kernel sync");float ms;ck(cudaEventElapsedTime(&ms,start,stop),"elapsed");total+=ms;fprintf(f,"P6\n%d %d\n255\n",w,h);fwrite(rgb,1,w*h*3,f);if(video&&frame%24==0)fprintf(stderr,"frame %d/%d · %.1f ms\n",frame,frames,ms);}
 if(video)pclose(f);else fclose(f);fprintf(stderr,"720x720 %d spp · %d frames · %.2f ms/frame GPU -> %s\n",spp,frames,total/frames,out);free(sceneData);cudaFree(rgb);cudaFree(b);}
