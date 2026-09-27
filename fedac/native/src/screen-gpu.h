#ifndef AC_SCREEN_GPU_H
#define AC_SCREEN_GPU_H
#include <math.h>
#include <time.h>
// Optional offscreen GPU rasterization. Scanout stays on the proven DRM path;
// readback keeps painting capture and the native HUD in the same framebuffer.
#ifdef HAVE_SCREEN_GPU
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include <GLES2/gl2.h>
static struct {
  int initialized, active, width, height, count;
  EGLDisplay display;
  EGLContext context;
  EGLSurface surface;
  GLuint program, fbo, texture, depth, vbo;
  GLuint theme_program, theme_textures[5];
  time_t theme_retry_at, theme_asset_retry_at[5];
  float *vertices;
  uint32_t *readback;
} ac_gpu;
#define AC_GPU_VERTICES 65532
static GLuint ac_gpu_shader(GLenum type, const char *source) {
  GLuint shader=glCreateShader(type);
  glShaderSource(shader,1,&source,NULL); glCompileShader(shader);
  GLint ok=0; glGetShaderiv(shader,GL_COMPILE_STATUS,&ok);
  if (!ok) { glDeleteShader(shader); return 0; }
  return shader;
}
static int ac_gpu_init(void) {
  if (ac_gpu.initialized) return ac_gpu.initialized>0;
  ac_gpu.initialized=-1;
  ac_gpu.display=eglGetPlatformDisplay(EGL_PLATFORM_SURFACELESS_MESA,EGL_DEFAULT_DISPLAY,NULL);
  if (ac_gpu.display==EGL_NO_DISPLAY || !eglInitialize(ac_gpu.display,NULL,NULL)) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  EGLint attrs[]={EGL_SURFACE_TYPE,EGL_PBUFFER_BIT,EGL_RENDERABLE_TYPE,EGL_OPENGL_ES2_BIT,
    EGL_RED_SIZE,8,EGL_GREEN_SIZE,8,EGL_BLUE_SIZE,8,EGL_ALPHA_SIZE,8,EGL_NONE};
  EGLConfig config; EGLint count=0;
  if (!eglChooseConfig(ac_gpu.display,attrs,&config,1,&count) || !count) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  eglBindAPI(EGL_OPENGL_ES_API);
  EGLint ca[]={EGL_CONTEXT_CLIENT_VERSION,2,EGL_NONE};
  EGLint pa[]={EGL_WIDTH,1,EGL_HEIGHT,1,EGL_NONE};
  ac_gpu.context=eglCreateContext(ac_gpu.display,config,EGL_NO_CONTEXT,ca);
  ac_gpu.surface=eglCreatePbufferSurface(ac_gpu.display,config,pa);
  if (ac_gpu.context==EGL_NO_CONTEXT || ac_gpu.surface==EGL_NO_SURFACE ||
      !eglMakeCurrent(ac_gpu.display,ac_gpu.surface,ac_gpu.surface,ac_gpu.context)) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  const char *renderer=(const char *)glGetString(GL_RENDERER);
  ac_log("[screen-gpu] renderer=%s\n",renderer?renderer:"unknown");
  if (!renderer || strstr(renderer,"llvmpipe") || strstr(renderer,"softpipe")) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  GLuint vs=ac_gpu_shader(GL_VERTEX_SHADER,
    "attribute vec3 position; attribute vec3 color; varying vec3 tint;"
    "void main(){gl_Position=vec4(position,1.0);tint=color;}");
  GLuint fs=ac_gpu_shader(GL_FRAGMENT_SHADER,
    "precision mediump float; varying vec3 tint;void main(){gl_FragColor=vec4(tint,1.0);}");
  if (!vs || !fs) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  ac_gpu.program=glCreateProgram(); glAttachShader(ac_gpu.program,vs); glAttachShader(ac_gpu.program,fs);
  glBindAttribLocation(ac_gpu.program,0,"position"); glBindAttribLocation(ac_gpu.program,1,"color");
  glLinkProgram(ac_gpu.program); glDeleteShader(vs); glDeleteShader(fs);
  GLint ok=0; glGetProgramiv(ac_gpu.program,GL_LINK_STATUS,&ok); if (!ok) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  glGenFramebuffers(1,&ac_gpu.fbo); glGenTextures(1,&ac_gpu.texture);
  glGenRenderbuffers(1,&ac_gpu.depth); glGenBuffers(1,&ac_gpu.vbo);
  ac_gpu.vertices=malloc(AC_GPU_VERTICES*6*sizeof(float));
  if (!ac_gpu.vertices) { ac_log("[screen-gpu] init failed line=%d egl=%x\n",__LINE__,eglGetError()); return 0; }
  ac_gpu.initialized=1; return 1;
}
static int ac_gpu_begin(int width,int height,float r,float g,float b) {
  ac_gpu.active=0; ac_gpu.count=0;
  if (width<1 || height<1 || width>4096 || height>4096 || !ac_gpu_init()) return 0;
  glBindFramebuffer(GL_FRAMEBUFFER,ac_gpu.fbo);
  if (width!=ac_gpu.width || height!=ac_gpu.height) {
    void *pixels=realloc(ac_gpu.readback,(size_t)width*height*4); if (!pixels) return 0;
    ac_gpu.readback=pixels;
    glBindTexture(GL_TEXTURE_2D,ac_gpu.texture);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,width,height,0,GL_RGBA,GL_UNSIGNED_BYTE,NULL);
    glFramebufferTexture2D(GL_FRAMEBUFFER,GL_COLOR_ATTACHMENT0,GL_TEXTURE_2D,ac_gpu.texture,0);
    glBindRenderbuffer(GL_RENDERBUFFER,ac_gpu.depth);
    glRenderbufferStorage(GL_RENDERBUFFER,GL_DEPTH_COMPONENT16,width,height);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER,GL_DEPTH_ATTACHMENT,GL_RENDERBUFFER,ac_gpu.depth);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER)!=GL_FRAMEBUFFER_COMPLETE) return 0;
    ac_gpu.width=width;ac_gpu.height=height;
  }
  glViewport(0,0,width,height); glDisable(GL_BLEND);glDisable(GL_DITHER);glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);glDepthFunc(GL_LEQUAL);glDepthMask(GL_TRUE);
  glClearColor(r/255.f,g/255.f,b/255.f,1);glClearDepthf(1);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
  ac_gpu.active=1;return 1;
}
static void ac_gpu_draw(void) {
  if (!ac_gpu.count) return;
  glUseProgram(ac_gpu.program);glBindBuffer(GL_ARRAY_BUFFER,ac_gpu.vbo);
  glBufferData(GL_ARRAY_BUFFER,(size_t)ac_gpu.count*6*sizeof(float),ac_gpu.vertices,GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);glEnableVertexAttribArray(1);
  glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,6*sizeof(float),(void *)0);
  glVertexAttribPointer(1,3,GL_FLOAT,GL_FALSE,6*sizeof(float),(void *)(3*sizeof(float)));
  glDrawArrays(GL_TRIANGLES,0,ac_gpu.count);ac_gpu.count=0;
}
// Retained, capped RGBA assets. Upload once, then draw in the same offscreen
// pass as game geometry; the existing end-of-frame readback stays singular.
static const int ac_theme_heights[]={576,512,512,1024,576};
static const double ac_theme_master_widths[]={1672,1774,1774,1254,1672};
static const double ac_theme_master_heights[]={941,887,887,1254,940};
static int ac_gpu_theme_program_ready(void) {
  if (!ac_gpu_init()) return 0;
  if (ac_gpu.theme_program) return 1;
  time_t now=time(NULL);
  if (now<ac_gpu.theme_retry_at) return 0;
  ac_gpu.theme_retry_at=now+1;
  GLuint vs=ac_gpu_shader(GL_VERTEX_SHADER,
    "attribute vec3 position;attribute vec2 uv;varying vec2 texcoord;"
    "void main(){gl_Position=vec4(position,1.0);texcoord=uv;}");
  GLuint fs=ac_gpu_shader(GL_FRAGMENT_SHADER,
    "precision mediump float;uniform sampler2D atlas;uniform float alphaCutoff;varying vec2 texcoord;"
    "void main(){vec4 c=texture2D(atlas,texcoord);if(c.a<=alphaCutoff)discard;gl_FragColor=c;}");
  GLuint program=glCreateProgram();
  if(vs && fs && program) {
    glAttachShader(program,vs);glAttachShader(program,fs);
    glBindAttribLocation(program,0,"position");glBindAttribLocation(program,1,"uv");
    glLinkProgram(program);
  }
  if(vs)glDeleteShader(vs);if(fs)glDeleteShader(fs);
  GLint ok=0;if(program)glGetProgramiv(program,GL_LINK_STATUS,&ok);
  if(!ok) {if(program)glDeleteProgram(program);return 0;}
  ac_gpu.theme_program=program;
  return 1;
}
static int ac_gpu_theme_asset_ready(int asset) {
  if(asset<0 || asset>=5 || !ac_gpu_theme_program_ready())return 0;
  if(ac_gpu.theme_textures[asset])return 1;
  time_t now=time(NULL);
  if(now<ac_gpu.theme_asset_retry_at[asset])return 0;
  ac_gpu.theme_asset_retry_at[asset]=now+1;
  const char *paths[]={"/pieces/oskiewar-theme/underpass-1024x576.rgba",
    "/pieces/oskiewar-theme/props-1024x512.rgba",
    "/pieces/oskiewar-theme/explosions-v1-1024x512.rgba",
    "/pieces/oskiewar-theme/weapons-v2-1024x1024.rgba",
    "/pieces/oskiewar-theme/sky-clouds-v1-1024x576.rgba"};
  size_t bytes=(size_t)1024*ac_theme_heights[asset]*4;
  unsigned char *pixels=malloc(bytes);
  FILE *file=fopen(paths[asset],"rb");
  int valid=pixels && file && fread(pixels,1,bytes,file)==bytes && fgetc(file)==EOF;
  if(file)fclose(file);
  if(!valid){free(pixels);return 0;}
  GLuint texture=0;glGenTextures(1,&texture);glBindTexture(GL_TEXTURE_2D,texture);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
  glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,1024,ac_theme_heights[asset],0,GL_RGBA,GL_UNSIGNED_BYTE,pixels);
  free(pixels);
  if(glGetError()!=GL_NO_ERROR){glDeleteTextures(1,&texture);return 0;}
  ac_gpu.theme_textures[asset]=texture;
  ac_log("[screen-gpu] photoreal asset %d ready (1024x%d)\n",asset,ac_theme_heights[asset]);
  return 1;
}
static int ac_gpu_theme_ready(void) {
  return ac_gpu_theme_asset_ready(0) && ac_gpu_theme_asset_ready(1);
}
static void ac_gpu_theme_submit(int asset,const float *quad,int depth_write) {
  glUseProgram(ac_gpu.theme_program);glBindBuffer(GL_ARRAY_BUFFER,ac_gpu.vbo);
  glBufferData(GL_ARRAY_BUFFER,30*sizeof(float),quad,GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);glEnableVertexAttribArray(1);
  glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,5*sizeof(float),(void *)0);
  glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,5*sizeof(float),(void *)(3*sizeof(float)));
  glActiveTexture(GL_TEXTURE0);glBindTexture(GL_TEXTURE_2D,ac_gpu.theme_textures[asset]);
  glUniform1i(glGetUniformLocation(ac_gpu.theme_program,"atlas"),0);
  const int solid=asset<2 || asset==4 || (asset==3 && depth_write);
  glUniform1f(glGetUniformLocation(ac_gpu.theme_program,"alphaCutoff"),solid?.5f:1.f/255.f);
  glEnable(GL_BLEND);glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glDepthMask(asset==2 || !depth_write?GL_FALSE:GL_TRUE);
  glDrawArrays(GL_TRIANGLES,0,6);glDisable(GL_BLEND);glDepthMask(GL_TRUE);
}
static int ac_gpu_theme_quad(int asset,const double *v) {
  if(!ac_gpu.active || !ac_gpu_theme_asset_ready(asset))return 0;
  for(int i=0;i<16;i++)if(!isfinite(v[i]))return 0;
  const double mw=ac_theme_master_widths[asset],mh=ac_theme_master_heights[asset];
  if(v[0]<0 || v[1]<0 || v[2]<=0 || v[3]<=0 ||
      v[0]+v[2]>mw || v[1]+v[3]>mh)return 0;
  for(int i=0;i<4;i++)if(fabs(v[6+i*3])>1.5)return 0;
  ac_gpu_draw();
  float quad[30];const int order[]={0,1,2,0,2,3};
  for(int i=0;i<6;i++) {
    int corner=order[i],right=corner==1 || corner==2,bottom=corner>=2;
    quad[i*5]=(float)(v[4+corner*3]*2/ac_gpu.width-1);
    quad[i*5+1]=(float)(1-v[5+corner*3]*2/ac_gpu.height);
    quad[i*5+2]=(float)(v[6+corner*3]/1.5);
    quad[i*5+3]=(float)((v[0]+(right?v[2]:0))/mw);
    quad[i*5+4]=(float)((v[1]+(bottom?v[3]:0))/mh);
  }
  ac_gpu_theme_submit(asset,quad,1);return 1;
}
static int ac_gpu_theme_sprite(int asset,const double *v,int flip,int depth_write) {
  if(!ac_gpu.active || !ac_gpu_theme_asset_ready(asset))return 0;
  for(int i=0;i<10;i++)if(!isfinite(v[i]))return 0;
  // sx,sy,sw,sh,cx,cy,width,height,angle,z; source coordinates are masters.
  const double mw=ac_theme_master_widths[asset],mh=ac_theme_master_heights[asset];
  if(v[0]<0 || v[1]<0 || v[2]<=0 || v[3]<=0 || v[6]<=0 || v[7]<=0 ||
      v[0]+v[2]>mw || v[1]+v[3]>mh || fabs(v[9])>1.5)return 0;
  ac_gpu_draw();
  const double c=cos(v[8]),s=sin(v[8]);
  float corners[4][5];
  for(int i=0;i<4;i++) {
    int right=i==1 || i==2,bottom=i>=2;
    double x=(right?.5:-.5)*v[6],y=(bottom?.5:-.5)*v[7];
    corners[i][0]=(v[4]+x*c-y*s)*2/ac_gpu.width-1;
    corners[i][1]=1-(v[5]+x*s+y*c)*2/ac_gpu.height;
    corners[i][2]=v[9]/1.5;
    corners[i][3]=(v[0]+(right!=flip?v[2]:0))/mw;
    corners[i][4]=(v[1]+(bottom?v[3]:0))/mh;
  }
  const int order[]={0,1,2,0,2,3};float quad[30];
  for(int i=0;i<6;i++)memcpy(quad+i*5,corners[order[i]],5*sizeof(float));
  ac_gpu_theme_submit(asset,quad,depth_write);
  return 1;
}
static int ac_gpu_triangle(const double *v) {
  if (!ac_gpu.active) return 0;
  if (ac_gpu.count+3>AC_GPU_VERTICES) ac_gpu_draw();
  for (int i=0;i<3;i++) {
    float *p=ac_gpu.vertices+(ac_gpu.count++)*6;
    p[0]=(float)v[i*3]*2/ac_gpu.width-1;
    p[1]=1-(float)v[i*3+1]*2/ac_gpu.height;
    // Xbox game depth spans [-1.5, 1.5]; GLES clip depth is [-1, 1].
    p[2]=(float)v[i*3+2] / 1.5f;
    p[3]=v[9]/255;p[4]=v[10]/255;p[5]=v[11]/255;
  }
  return 1;
}
// Match the game's rim-fan polygon, with its tessellation chosen in game
// coordinates before pixel scaling. One JS call replaces a call per face.
static int ac_gpu_disc(double x,double y,double z,double radius,int sides,
    double r,double g,double b) {
  if (!ac_gpu.active || sides<3 || sides>32 || radius<=0 ||
      !isfinite(x+y+z+radius+r+g+b)) return 0;
  static double rings[33][64];
  static int ready[33];
  if (!ready[sides]) {
    for(int i=0;i<sides;i++) {
      rings[sides][i*2]=cos(i*2*M_PI/sides);
      rings[sides][i*2+1]=sin(i*2*M_PI/sides);
    }
    ready[sides]=1;
  }
  double *ring=rings[sides];
  double v[]={x+radius,y,z,x+ring[2]*radius,y+ring[3]*radius,z,0,0,z,r,g,b};
  for(int i=2;i<sides;i++) {
    v[6]=x+ring[i*2]*radius;v[7]=y+ring[i*2+1]*radius;
    ac_gpu_triangle(v);v[3]=v[6];v[4]=v[7];
  }
  return sides-2;
}
static void ac_gpu_end(ACFramebuffer *fb) {
  if (!ac_gpu.active) return;
  ac_gpu_draw();
  glReadPixels(0,0,ac_gpu.width,ac_gpu.height,GL_RGBA,GL_UNSIGNED_BYTE,ac_gpu.readback);
  if (fb->width==ac_gpu.width && fb->height==ac_gpu.height) {
    for (int y=0;y<fb->height;y++) {
      const uint32_t *src=ac_gpu.readback+(fb->height-1-y)*fb->width;
      uint32_t *dst=fb->pixels+y*fb->stride;
      for(int x=0;x<fb->width;x++) { uint32_t c=src[x];
        dst[x]=(c&0xff00ff00)|((c&255)<<16)|((c>>16)&255); }
    }
  }
  ac_gpu.active=0;
}
#else
static int ac_gpu_theme_ready(void) { return 0; }
static int ac_gpu_theme_asset_ready(int asset) { (void)asset;return 0; }
static int ac_gpu_theme_sprite(int asset,const double *v,int flip,int depth_write) {
  (void)asset;(void)v;(void)flip;(void)depth_write;return 0;
}
static int ac_gpu_theme_quad(int asset,const double *v) { (void)asset;(void)v;return 0; }
static int ac_gpu_begin(int w,int h,float r,float g,float b) { (void)w;(void)h;(void)r;(void)g;(void)b;return 0; }
static int ac_gpu_triangle(const double *v) { (void)v;return 0; }
static int ac_gpu_disc(double x,double y,double z,double radius,int sides,double r,double g,double b) {
  (void)x;(void)y;(void)z;(void)radius;(void)sides;(void)r;(void)g;(void)b;return 0;
}
static void ac_gpu_end(ACFramebuffer *fb) { (void)fb; }
#endif
#endif
