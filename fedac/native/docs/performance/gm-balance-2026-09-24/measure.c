// Offline unit-gain GM peak and strongest 100ms RMS, no audio device.
#include "gm_synth.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
int main(void){
 const double sr=44100;int program,note,seed;double dur;
 GMVoice *v=calloc(1,sizeof(*v));double ring[4410];
 if(!v)return 2;
 puts("program,midi,duration,seed,rms100,peak");
 while(scanf("%d %d %lf %d",&program,&note,&dur,&seed)==4){
  double hz=440*pow(2,(note-69)/12.0),sum=0,best=0,peak=0;
  int count=(int)(dur*sr);for(int i=0;i<4410;i++)ring[i]=0;
  if(gm_voice_init(v,program,hz,sr,seed))return 3;
  for(int i=0;i<count;i++){
   double env=fmin(1,i/(sr*.01))*fmin(1,(count-i)/(sr*.12));
   double x=gm_voice_render(v,sr,env,hz);if(!isfinite(x))return 4;
   peak=fmax(peak,fabs(x));sum+=x*x-ring[i%4410];ring[i%4410]=x*x;
   if(i>=4409)best=fmax(best,sum/4410);
  }
  printf("%d,%d,%.2f,%d,%.9g,%.9g\n",program,note,dur,seed,sqrt(best),peak);
 }
 free(v);return 0;
}
