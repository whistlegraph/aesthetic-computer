#define main instruments_cli_main
#include "instruments.c"
#undef main
#include "engine.h"
#include "../../../pop/dsp/c/src/acdsp.h"
#include <stdatomic.h>
typedef struct {float *data;int n;int64_t at;double gl,gr;} Phrase;
static Phrase phrases[64];static int nphrases;
typedef struct {float comb[4][2200],ap[2][250];int cn[4],ci[4],an[2],ai[2];double lp[4];} Room;
static Room room[2];static _Atomic int64_t audibleFrames;static int64_t initialFrames;
static double outputGain=1;
// The record's master stage, per machine: bus saturation at the premaster operating point
// (render.mjs: tanh(1.1x)), the static loudness gain, then a lookahead limiter with the
// master's alimiter settings (limit 0.94, attack 3 ms, release 60 ms). Drive pushes into it.
#define LOOKAHEAD 144
static double drive=1,limGain=1,delayL[LOOKAHEAD],delayR[LOOKAHEAD],need[LOOKAHEAD];static int delayAt;
static const double CEILING=.94;
static double saturate(double x){return tanh(1.1*x)/1.1;}
static void init_room(Room *r,const double *delays){for(int i=0;i<4;i++)r->cn[i]=lround(delays[i]*SR);r->an[0]=lround(.0051*SR);r->an[1]=lround(.0017*SR);}
static double room_sample(Room *r,double x){
 double y=0;for(int k=0;k<4;k++){int i=r->ci[k];double v=r->comb[k][i];r->lp[k]=r->lp[k]*.35+v*.65;r->comb[k][i]=x+r->lp[k]*.79;r->ci[k]=(i+1)%r->cn[k];y+=v;}y=(float)(y/4);
 for(int k=0;k<2;k++){int i=r->ai[k];double v=r->ap[k][i],u=y+v*.5;r->ap[k][i]=u;r->ai[k]=(i+1)%r->an[k];y=(float)(v-.5*u);}return y*pow(10,-15.0/20);
}
int eg_load(const char *path){
 FILE *f=fopen(path,"r");if(!f)return 1;
 if(fscanf(f,"%lf %lf %lf %lf %lf",&duration,&introEnd,&bridge,&bridgeEnd,&outro)!=5){fclose(f);return 2;}
 double t,d,m,v,g,p,fr;int typ;unsigned seed;
 while(fscanf(f,"%d %lf %lf %lf %lf %lf %lf %lf %u",&typ,&t,&d,&m,&v,&g,&p,&fr,&seed)==9){
  if(count>=MAX_EVENTS||typ<0||typ>6||!isfinite(t+d+m+v+g+p+fr)||t<0||d<=0||d>30||fabs(p)>1||g<0||g>2){fclose(f);return 2;}
  Event *e=&ev[count++];e->type=typ;e->start=llround(t*SR);e->dur=d;e->vel=v;e->rng=seed;
  double r=typ==3?.05:typ==4?.7:typ==5?.5:typ==6?.12:0;e->end=e->start+llround((d+r)*SR);e->freq=typ==2?fr:440*pow(2,(m-69)/12);e->gl=g*cos((p+1)*M_PI/4);e->gr=g*sin((p+1)*M_PI/4);
 }
 int ok=feof(f)&&count;fclose(f);if(!ok)return 2;
 double dl[]={.0297,.0371,.0411,.0437},dr[]={.0313,.0353,.0427,.0449};init_room(&room[0],dl);init_room(&room[1],dr);return 0;
}
int eg_voice(const float *samples,int frames,double at,double gain,double pan){
 if(nphrases>=64||frames<=0||!isfinite(at+gain+pan))return 1;
 float *data=malloc((size_t)frames*sizeof(float));if(!data)return 1;
 // The studio singer writes 16-bit WAV before its vocalLead processing.
 for(int i=0;i<frames;i++)data[i]=(float)lrintf(fmaxf(-1,fminf(.9999695f,samples[i]))*32768)/32768;
 if(acdsp_process(data,frames,SR,1,"eq:rumble 1176:ratio=8:in=0:out=-1:attack=6:release=4:iron=0.6 eq:nasal=-1.5 eq:presence=2 eq:sibilance=-2 eq:air=1.5")){free(data);return 2;}
 for(int i=0;i<frames;i++)data[i]=(float)lrintf(fmaxf(-1,fminf(1,data[i]))*8388607)/8388608;
 Phrase *p=&phrases[nphrases++];p->data=data;p->n=frames;p->at=llround(at*SR);p->gl=gain*cos((pan+1)*M_PI/4);p->gr=gain*sin((pan+1)*M_PI/4);return 0;
}
void eg_render(float *out,int n){
 // Caller is bounded to BLOCK frames. Reverb and note states survive callbacks.
 int64_t at=cursor;render(out,n);float vl[BLOCK]={0},vr[BLOCK]={0};
 for(int k=0;k<nphrases;k++){Phrase *p=&phrases[k];int a=(int)fmax(0,p->at-at),b=(int)fmin(n,p->at+p->n-at);for(int j=a;j<b;j++){double x=p->data[at+j-p->at];vl[j]+=x*p->gl;vr[j]+=x*p->gr;}}
 for(int j=0;j<n;j++){
  double mono=(float)((vl[j]+vr[j])*.5),l=out[2*j]+(vl[j]+room_sample(&room[0],mono))*pow(10,.5/20),r=out[2*j+1]+(vr[j]+room_sample(&room[1],mono))*pow(10,.5/20);
  if(outputGain==1){ // the silent check: the premaster path, as compare.py measures it
   out[2*j]=(float)l;out[2*j+1]=(float)r;peak=fmax(peak,fmax(fabs(l),fabs(r)));continue;
  }
  double fade=fmin(1,(at+j)/(double)SR/.3);
  l=saturate(l*drive)*outputGain*fade;r=saturate(r*drive)*outputGain*fade;
  // Stereo-linked lookahead limiter: the gain settles on the window's floor by the time the
  // peak reaches the output (one pole, ~2% short of target after LOOKAHEAD samples), and
  // releases over 60 ms. Output lags input by LOOKAHEAD samples on every machine alike.
  double pk=fmax(fabs(l),fabs(r));need[delayAt]=pk>CEILING?CEILING/pk:1;
  double target=1;for(int k=0;k<LOOKAHEAD;k++)target=fmin(target,need[k]);
  limGain+=(target-limGain)*(target<limGain?1-exp(-4.0/LOOKAHEAD):1-exp(-1.0/(.060*SR)));
  double ol=delayL[delayAt]*limGain,or_=delayR[delayAt]*limGain;delayL[delayAt]=l;delayR[delayAt]=r;delayAt=(delayAt+1)%LOOKAHEAD;
  out[2*j]=(float)fmax(-.999,fmin(.999,ol));out[2*j+1]=(float)fmax(-.999,fmin(.999,or_));peak=fmax(peak,fmax(fabs(out[2*j]),fabs(out[2*j+1])));
 }
}
void eg_set_drive(double db){if(isfinite(db)&&db>-24&&db<24)drive=pow(10,db/20);}
static void mix_callback(void *ctx,AudioQueueRef q,AudioQueueBufferRef b){
 (void)ctx;if(stopped)return;
 eg_render(b->mAudioData,BLOCK);b->mAudioDataByteSize=BLOCK*8;
 OSStatus e=AudioQueueEnqueueBuffer(q,b,0,NULL);if(e&&!stopped){audioError=e;stopped=1;}
 atomic_store(&audibleFrames,cursor-3*BLOCK);
}
int eg_start(double at,const char *path){
 epoch=at;receipt=path;if(epoch-wall()<2)return 2;
 AudioStreamBasicDescription fmt={.mSampleRate=SR,.mFormatID=kAudioFormatLinearPCM,.mFormatFlags=kAudioFormatFlagIsFloat|kAudioFormatFlagIsPacked,.mBytesPerPacket=8,.mFramesPerPacket=1,.mBytesPerFrame=8,.mChannelsPerFrame=2,.mBitsPerChannel=32};
 OSStatus e=AudioQueueNewOutput(&fmt,mix_callback,NULL,NULL,NULL,0,&queue);if(e)return (int)e;
 // Begin at the trimmed recording boundary, including any vocal pickup tail.
 initialFrames=cursor;
 for(int i=0;i<3;i++){AudioQueueBufferRef b;e=AudioQueueAllocateBuffer(queue,BLOCK*8,&b);if(e)return (int)e;mix_callback(NULL,queue,b);}
 mach_timebase_info_data_t tb;mach_timebase_info(&tb);double delay=epoch-wall();if(delay<1)return 2;
 AudioTimeStamp start={0};start.mFlags=kAudioTimeStampHostTimeValid;start.mHostTime=mach_absolute_time()+(uint64_t)(delay*1e9*tb.denom/tb.numer);
 e=AudioQueueStart(queue,&start);if(e)return (int)e;status("armed");return 0;
}
void eg_set_gain(double gain){if(isfinite(gain)&&gain>0&&gain<4)outputGain=gain;}
void eg_stop(void){stopped=1;if(queue){AudioQueueStop(queue,true);AudioQueueDispose(queue,true);queue=NULL;}status(audioError?"error":"complete");}
double eg_time(void){return (double)atomic_load(&audibleFrames)/SR;}
double eg_peak(void){return peak;}

#include <time.h>
double eg_cpu_time(void){struct timespec ts;clock_gettime(CLOCK_THREAD_CPUTIME_ID,&ts);return ts.tv_sec+ts.tv_nsec/1e9;}
