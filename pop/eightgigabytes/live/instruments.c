// The pop cut's oscillators, evaluated in bounded blocks by CoreAudio.
// No song buffers, samples, files, allocations or network reads in the callback.
#include <AudioToolbox/AudioToolbox.h>
#include <mach/mach_time.h>
#include <math.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>
#define SR 48000
#define BLOCK 512
#define MAX_EVENTS 4096
#define TAU (2.0*M_PI)
typedef struct { int type; int64_t start,end; double dur,freq,vel,gl,gr,ph,x1,x2,y1,y2; uint32_t rng; } Event;
static Event ev[MAX_EVENTS]; static int count;
static double duration,introEnd,bridge,bridgeEnd,outro,scale=.72,peak,maxBlockMs; static int64_t cursor;
static volatile sig_atomic_t stopped; static AudioQueueRef queue;
static const char *receipt; static double epoch; static int audioError;
static double wall(void) { struct timeval tv; gettimeofday(&tv,NULL); return tv.tv_sec+tv.tv_usec/1e6; }
static double noise(Event *e) { e->rng=e->rng*1664525u+1013904223u; return e->rng/4294967296.0*2-1; }
static double sample(Event *e,int64_t i) {
 double t=(double)i/SR,f=e->freq,y=0,g;
 switch(e->type) {
 case 0: // kick
  e->ph+=TAU*(46+90*exp(-t*28))/SR;
  y=tanh(1.6*sin(e->ph))*exp(-t*7.5)*e->vel;
  if(t<.004)y+=noise(e)*.25*(1-t/.004); break;
 case 1: { // trackpad tap: RBJ bandpass, then sine body
  double w=TAU*2300/SR,a=sin(w)/(2*1.3),x=noise(e);
  double bp=(a*x-a*e->x2+2*cos(w)*e->y1-(1-a)*e->y2)/(1+a);
  e->x2=e->x1;e->x1=x;e->y2=e->y1;e->y1=bp;
  g=t<.001?t/.001:exp(-(t-.001)/.028*5);
  y=(bp*1.8*g+sin(TAU*185*t)*exp(-t*45)*.55)*e->vel; break;
 }
 case 2: y=(sin(TAU*f*t)*exp(-t*700)+noise(e)*.12*exp(-t*1400))*e->vel;break;
 case 3: case 4: case 6: { // bass, pad, whistle
  double a=e->type==3?.006:e->type==4?.5:.045;
  double r=e->type==3?.05:e->type==4?.7:.12;
  double v=e->type==6?pow(2,22.0/1200*sin(TAU*5.2*t)*fmin(1,t/.25)):1;
  e->ph+=TAU*f*v/SR;g=t<a?t/a:t<e->dur?1:fmax(0,1-(t-e->dur)/r);
  y=sin(e->ph);
  if(e->type==3)y=tanh((y+.35*sin(2*e->ph)+.08*sin(3*e->ph))*1.4)/tanh(1.4);
  if(e->type==4)y+=.3*sin(2*e->ph)+.12*sin(3*e->ph)+.25*sin(.5*e->ph)+.05*sin(4*e->ph);
  if(e->type==6)y+=.05*sin(2*e->ph)+noise(e)*.035;
  y*=g;break;
 }
 case 5: // FM keys
  y=sin(TAU*f*t+(2.6*exp(-t*6)+.25)*sin(TAU*2*f*t))*exp(-t*2.2)
   *(t<e->dur?1:exp(-(t-e->dur)*14))*e->vel*fmin(1,t/.003);break;
 }
 return y;
}
static void render(float *out,int n) {
 double started=wall(); memset(out,0,n*2*sizeof(float));
 float dynamics[BLOCK];
 for(int j=0;j<n;j++) {
  double t=(cursor+j)/(double)SR,want=t<introEnd?.72:t>=outro?.75:(t>=bridge&&t<bridgeEnd)?.6:1;
  scale+=(want-scale)*.00004;dynamics[j]=scale;
 }
 for(int k=0;k<count;k++) {
  Event *e=&ev[k]; if(e->end<=cursor || e->start>=cursor+n)continue;
  int a=(int)fmax(0,e->start-cursor),b=(int)fmin(n,e->end-cursor);
  for(int j=a;j<b;j++) {
   double y=sample(e,cursor+j-e->start)*(e->type==6?1:dynamics[j]);
   out[2*j]+=y*e->gl;out[2*j+1]+=y*e->gr;
  }
 }
 for(int j=0;j<n*2;j++) { peak=fmax(peak,fabs(out[j]));out[j]=fmax(-.94,fmin(.94,out[j])); }
 cursor+=n;maxBlockMs=fmax(maxBlockMs,(wall()-started)*1000);
}
static void status(const char *phase) {
 if(!receipt)return;char tmp[4096];snprintf(tmp,sizeof(tmp),"%s.tmp",receipt);
 FILE *f=fopen(tmp,"w");if(!f)return;
 fprintf(f,"{\"phase\":\"%s\",\"pid\":%d,\"startEpoch\":%.6f,\"frames\":%lld,\"peak\":%.6f,\"maxBlockMs\":%.3f,\"error\":%d}\n",phase,getpid(),epoch,(long long)cursor,peak,maxBlockMs,audioError);
 fclose(f);rename(tmp,receipt);
}
static void callback(void *ctx,AudioQueueRef q,AudioQueueBufferRef b) {
 (void)ctx;if(stopped)return;
 render(b->mAudioData,BLOCK);b->mAudioDataByteSize=BLOCK*2*sizeof(float);
 OSStatus s=AudioQueueEnqueueBuffer(q,b,0,NULL);if(s){audioError=s;stopped=1;}
}
static void stop(int sig){(void)sig;stopped=1;}
static void require(OSStatus s,const char *op){if(s){fprintf(stderr,"%s: %d\n",op,(int)s);exit(1);}}
int main(int argc,char **argv) {
 if(argc<3){fprintf(stderr,"Usage: instruments score.tsv --check [out.f32] | epoch status.json\n");return 2;}
 FILE *f=fopen(argv[1],"r");if(!f){perror(argv[1]);return 1;}
 if(fscanf(f,"%lf %lf %lf %lf %lf\n",&duration,&introEnd,&bridge,&bridgeEnd,&outro)!=5 || !isfinite(duration)||duration<=0||duration>600)return 2;
 double t,d,m,v,g,p,fr;int typ;unsigned seed;
 while(fscanf(f,"%d %lf %lf %lf %lf %lf %lf %lf %u",&typ,&t,&d,&m,&v,&g,&p,&fr,&seed)==9) {
  if(count>=MAX_EVENTS||typ<0||typ>6||!isfinite(t+d+m+v+g+p+fr)||t<0||d<=0||d>30||fabs(p)>1||g<0||g>2)return 2;
  Event *e=&ev[count++];e->type=typ;e->start=llround(t*SR);e->dur=d;e->vel=v;e->rng=seed;
  double release=typ==3?.05:typ==4?.7:typ==5?.5:typ==6?.12:0;
  e->end=e->start+llround((d+release)*SR);e->freq=typ==2?fr:440*pow(2,(m-69)/12);
  e->gl=g*cos((p+1)*M_PI/4);e->gr=g*sin((p+1)*M_PI/4);
 }
 if(!feof(f)||!count){fprintf(stderr,"Invalid or empty score\n");return 2;}fclose(f);
 if(!strcmp(argv[2],"--check")) {
  // Open and dispose the output device without starting it: silent route check.
  AudioStreamBasicDescription fmt={.mSampleRate=SR,.mFormatID=kAudioFormatLinearPCM,
   .mFormatFlags=kAudioFormatFlagIsFloat|kAudioFormatFlagIsPacked,.mBytesPerPacket=8,.mFramesPerPacket=1,
   .mBytesPerFrame=8,.mChannelsPerFrame=2,.mBitsPerChannel=32};
  AudioQueueRef probe;
  require(AudioQueueNewOutput(&fmt,callback,NULL,NULL,NULL,0,&probe),"Silent output check");
  require(AudioQueueDispose(probe,true),"Dispose silent check");
  FILE *raw=argc>3?fopen(argv[3],"wb"):NULL;if(argc>3&&!raw)return 1;
  float b[BLOCK*2];double began=wall();
  while(cursor<llround(duration*SR)){int n=(int)fmin(BLOCK,llround(duration*SR)-cursor);render(b,n);if(raw)fwrite(b,sizeof(float),n*2,raw);}
  if(raw)fclose(raw);
  printf("{\"events\":%d,\"duration\":%.3f,\"renderSeconds\":%.3f,\"peak\":%.6f,\"maxBlockMs\":%.3f,\"blockBudgetMs\":%.3f,\"silent\":true}\n",count,duration,wall()-began,peak,maxBlockMs,1000.0*BLOCK/SR);
  return (!isfinite(peak)||peak<=0||peak>=.94)?1:0;
 }
 int join=argc==5&&!strcmp(argv[4],"--join");
 if(argc!=4&&!join)return 2;epoch=strtod(argv[2],NULL);receipt=argv[3];
 if(!isfinite(epoch)||(!join&&epoch-wall()<2)||epoch+duration<=wall()+2){fprintf(stderr,"Need at least two seconds of lead\n");return 2;}
 double deviceEpoch=join?wall()+2:epoch;
 if(join){float discard[BLOCK*2];int64_t target=llround((deviceEpoch-epoch)*SR);while(cursor<target)render(discard,(int)fmin(BLOCK,target-cursor));}
 signal(SIGTERM,stop);signal(SIGINT,stop);
 AudioStreamBasicDescription fmt={.mSampleRate=SR,.mFormatID=kAudioFormatLinearPCM,
 .mFormatFlags=kAudioFormatFlagIsFloat|kAudioFormatFlagIsPacked,.mBytesPerPacket=8,.mFramesPerPacket=1,
 .mBytesPerFrame=8,.mChannelsPerFrame=2,.mBitsPerChannel=32};
 require(AudioQueueNewOutput(&fmt,callback,NULL,NULL,NULL,0,&queue),"AudioQueueNewOutput");
 for(int i=0;i<3;i++){AudioQueueBufferRef b;require(AudioQueueAllocateBuffer(queue,BLOCK*8,&b),"AllocateBuffer");callback(NULL,queue,b);}
 mach_timebase_info_data_t tb;mach_timebase_info(&tb);
 double delay=deviceEpoch-wall();if(delay<1){AudioQueueDispose(queue,true);return 2;}
 AudioTimeStamp start={0};start.mFlags=kAudioTimeStampHostTimeValid;start.mHostTime=mach_absolute_time()+(uint64_t)(delay*1e9*tb.denom/tb.numer);
 require(AudioQueueStart(queue,&start),"AudioQueueStart");status("armed");
 int playing=0;while(!stopped && wall()<epoch+duration+.1) {
  if(!playing&&wall()>=epoch){playing=1;status("playing");}usleep(20000);
 }
 AudioQueueStop(queue,true);AudioQueueDispose(queue,true);status(audioError?"error":stopped?"stopped":"complete");
 return audioError?1:0;
}
