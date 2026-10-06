#ifndef AC_RAYCAST_JS_H
#define AC_RAYCAST_JS_H
#include "raycast.h"
// Retain each backing ArrayBuffer throughout the synchronous render. The JS
// caller owns/reuses buffers; no pointers survive this call or context teardown.
static void *ac_ray_array(JSContext *ctx,JSValueConst value,size_t element,
                          size_t needed,JSValue *owner){
 size_t offset,length,bytes;
 *owner=JS_GetTypedArrayBuffer(ctx,value,&offset,&length,&bytes);
 if(JS_IsException(*owner))return NULL;
 size_t total=0;uint8_t *p=JS_GetArrayBuffer(ctx,&total,*owner);
 if(!p||bytes!=element||offset%element||length<needed||offset>total||length>total-offset){
  JS_ThrowRangeError(ctx,"raycast buffer type, alignment or length");return NULL;
 }
 return p+offset;
}
static JSValue js_raycast(JSContext *ctx,JSValueConst self,int argc,JSValueConst *argv){
 (void)self;
 if(argc<4||!current_rt||!current_rt->graph||!current_rt->graph->fb)return JS_UNDEFINED;
 int count=0;if(JS_ToInt32(ctx,&count,argv[1])<0)return JS_EXCEPTION;
 if(count<0||count>128)return JS_ThrowRangeError(ctx,"raycast accepts at most 128 ellipsoids");
 JSValue owners[3]={JS_UNDEFINED,JS_UNDEFINED,JS_UNDEFINED};
 JSValue result=JS_EXCEPTION;
 double *u=ac_ray_array(ctx,argv[2],8,21*sizeof(double),&owners[0]);if(!u)goto done;
 for(int i=0;i<21;i++)if(!isfinite(u[i])){JS_ThrowRangeError(ctx,"nonfinite raycast uniform");goto done;}
 ACFramebuffer *fb=current_rt->graph->fb;
 if(u[0]<1||u[1]<1||u[0]>fb->width||u[1]>fb->height||u[0]>4096||u[1]>4096||floor(u[0])!=u[0]||floor(u[1])!=u[1]||u[4]<1||u[4]>100000||fabs(u[2])>100000||fabs(u[3])>100000){JS_ThrowRangeError(ctx,"invalid raycast viewport");goto done;}
 for(int i=5;i<=18;i++)if(fabs(u[i])>1.001){JS_ThrowRangeError(ctx,"invalid raycast basis");goto done;}
 for(int i=19;i<=20;i++)if(u[i]<1||u[i]>16||floor(u[i])!=u[i]){JS_ThrowRangeError(ctx,"invalid raycast cell size");goto done;}
 double *shapes=ac_ray_array(ctx,argv[0],8,(size_t)count*16*sizeof(double),&owners[1]);if(!shapes)goto done;
 for(int n=0;n<count;n++){
  double *o=shapes+n*16;
  for(int i=0;i<16;i++)if(!isfinite(o[i])||fabs(o[i])>1e7){JS_ThrowRangeError(ctx,"invalid raycast ellipsoid");goto done;}
  for(int i=3;i<6;i++)if(o[i]<.001){JS_ThrowRangeError(ctx,"raycast radii must be positive");goto done;}
 }
 float *depth=ac_ray_array(ctx,argv[3],4,(size_t)u[0]*(size_t)u[1]*sizeof(float),&owners[2]);if(!depth)goto done;
 // Overlapping backing buffers would corrupt uniforms/shapes while writing Z.
 // Compare checked byte ranges via integer addresses (unrelated C pointers
 // cannot be ordered portably).
 uintptr_t d=(uintptr_t)depth,de=d+(size_t)u[0]*(size_t)u[1]*sizeof(float);
 uintptr_t a=(uintptr_t)shapes,ae=a+(size_t)count*16*sizeof(double),v=(uintptr_t)u,ve=v+21*sizeof(double);
 if((d<ae&&a<de)||(d<ve&&v<de)){JS_ThrowRangeError(ctx,"raycast depth must not overlap inputs");goto done;}
 ac_raycast(fb->pixels,fb->stride,depth,shapes,count,u);result=JS_TRUE;
 done:for(int i=0;i<3;i++)JS_FreeValue(ctx,owners[i]);return result;
}
#endif
