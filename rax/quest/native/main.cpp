// 🥽 rax, native on a Quest: an OpenXR session, two eyes, and the room drawn
// from a list of boxes. No engine: EGL, OpenGL ES 3, and the Khronos loader.
//
// The room is not in this file. It is a text file of boxes, one a line
// ("x y z w h d rrggbb", metres, in rax's own axes), that the Java side
// (RaxNativeActivity) fetches from the rax server and keeps fresh: this side
// looks at the file once a second and makes its mesh again when it changes.
// So what stands in the room is changed on the server, as the website is,
// and the headset shows it without a new build. With no file yet, a small
// built-in roof is drawn, so there is always something to stand on.
//
// rax faces +z at yaw 0 with +x to the east; OpenXR faces -z with +x to the
// right. One is the other turned half round the upright (x and z negated),
// which is a turn and not a mirror, so nothing is flipped.
#include <android/log.h>
#include <android_native_app_glue.h>
#include <EGL/egl.h>
#include <GLES3/gl3.h>
#include <jni.h>
#include <sys/stat.h>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

#define XR_USE_PLATFORM_ANDROID
#define XR_USE_GRAPHICS_API_OPENGL_ES
#include <openxr/openxr.h>
#include <openxr/openxr_platform.h>

#define LOG(...) __android_log_print(ANDROID_LOG_INFO, "rax", __VA_ARGS__)
#define ERR(...) __android_log_print(ANDROID_LOG_ERROR, "rax", __VA_ARGS__)
static bool ok(XrResult r, const char *what) {
  if (XR_SUCCEEDED(r)) return true;
  ERR("%s failed: %d", what, (int)r);
  return false;
}

// ── a little matrix arithmetic (column-major, as GL has it) ────────────────
struct Mat4 { float m[16]; };
static Mat4 mul(const Mat4 &a, const Mat4 &b) {
  Mat4 r;
  for (int c = 0; c < 4; c++) for (int row = 0; row < 4; row++) {
    float s = 0;
    for (int k = 0; k < 4; k++) s += a.m[k * 4 + row] * b.m[c * 4 + k];
    r.m[c * 4 + row] = s;
  }
  return r;
}
// The projection OpenXR's four half-angles ask for.
static Mat4 projection(const XrFovf &f, float n, float fa) {
  const float l = tanf(f.angleLeft), r = tanf(f.angleRight), u = tanf(f.angleUp), d = tanf(f.angleDown);
  Mat4 p = {};
  p.m[0] = 2 / (r - l); p.m[5] = 2 / (u - d);
  p.m[8] = (r + l) / (r - l); p.m[9] = (u + d) / (u - d);
  p.m[10] = -(fa + n) / (fa - n); p.m[11] = -1;
  p.m[14] = -(2 * fa * n) / (fa - n);
  return p;
}
// The view: the eye's pose, undone.
static Mat4 viewOf(const XrPosef &pose) {
  const XrQuaternionf &q = pose.orientation;
  const float x = q.x, y = q.y, z = q.z, w = q.w;
  // The rotation's own matrix, then its transpose (its undoing), with the place taken off first.
  float R[9] = {1 - 2 * (y * y + z * z), 2 * (x * y + z * w), 2 * (x * z - y * w),
                2 * (x * y - z * w), 1 - 2 * (x * x + z * z), 2 * (y * z + x * w),
                2 * (x * z + y * w), 2 * (y * z - x * w), 1 - 2 * (x * x + y * y)}; // columns
  Mat4 v = {};
  for (int c = 0; c < 3; c++) for (int r = 0; r < 3; r++) v.m[c * 4 + r] = R[r * 3 + c]; // transposed
  const XrVector3f &p = pose.position;
  for (int r = 0; r < 3; r++) v.m[12 + r] = -(v.m[r] * p.x + v.m[4 + r] * p.y + v.m[8 + r] * p.z);
  v.m[15] = 1;
  return v;
}

// ── the room: boxes, made into one mesh ────────────────────────────────────
struct Box { float x, y, z, w, h, d; unsigned rgb; };
static const Box BUILT_IN[] = {
  {0, -1, 0, 16, 2, 16, 0x3a3f4d},       // the roof
  {0, -61, 0, 14.8f, 118, 14.8f, 0x2a2f3c}, // the tower under it
  {0, -120.5f, 0, 420, 1, 420, 0x1b1e26},  // the street
  {0, -45, 36, 10.8f, 150, 10.8f, 0x5b6172}, // the clock tower
  {0, 21, 30.5f, 8.4f, 8.4f, 0.1f, 0xf2ecd8}, // its face
};
static std::vector<float> meshOf(const std::vector<Box> &boxes) {
  std::vector<float> v;
  v.reserve(boxes.size() * 36 * 6);
  // Each face a little lighter or darker by the way it looks: no lamp, but a box still reads as a box.
  static const float shade[6] = {1.0f, 0.55f, 0.82f, 0.82f, 0.68f, 0.68f}; // top, bottom, +x, -x, +z, -z
  static const int F[6][4][3] = {
    {{-1, 1, -1}, {-1, 1, 1}, {1, 1, 1}, {1, 1, -1}},   {{-1, -1, 1}, {-1, -1, -1}, {1, -1, -1}, {1, -1, 1}},
    {{1, -1, -1}, {1, 1, -1}, {1, 1, 1}, {1, -1, 1}},   {{-1, -1, 1}, {-1, 1, 1}, {-1, 1, -1}, {-1, -1, -1}},
    {{-1, -1, 1}, {1, -1, 1}, {1, 1, 1}, {-1, 1, 1}},   {{1, -1, -1}, {-1, -1, -1}, {-1, 1, -1}, {1, 1, -1}},
  };
  for (const Box &b : boxes) {
    const float r = ((b.rgb >> 16) & 255) / 255.f, g = ((b.rgb >> 8) & 255) / 255.f, bl = (b.rgb & 255) / 255.f;
    for (int f = 0; f < 6; f++) {
      static const int T[6] = {0, 1, 2, 0, 2, 3};
      for (int k = 0; k < 6; k++) {
        const int *c = F[f][T[k]];
        // (rax's axes to OpenXR's: x and z negated.)
        v.push_back(-(b.x + c[0] * b.w / 2)); v.push_back(b.y + c[1] * b.h / 2); v.push_back(-(b.z + c[2] * b.d / 2));
        v.push_back(r * shade[f]); v.push_back(g * shade[f]); v.push_back(bl * shade[f]);
      }
    }
  }
  return v;
}
// The scene file: "x y z w h d rrggbb" a line; '#' begins a note. Nothing read is nothing changed.
static bool readScene(const std::string &path, std::vector<Box> &out) {
  FILE *f = fopen(path.c_str(), "r");
  if (!f) return false;
  std::vector<Box> boxes;
  char line[256];
  while (fgets(line, sizeof line, f)) {
    if (line[0] == '#') continue;
    Box b;
    if (sscanf(line, "%f %f %f %f %f %f %x", &b.x, &b.y, &b.z, &b.w, &b.h, &b.d, &b.rgb) == 7 && boxes.size() < 20000) boxes.push_back(b);
  }
  fclose(f);
  if (boxes.empty()) return false;
  out.swap(boxes);
  return true;
}

// ── the app ────────────────────────────────────────────────────────────────
struct Eye { XrSwapchain chain = XR_NULL_HANDLE; int w = 0, h = 0; std::vector<GLuint> fbo; GLuint depth = 0; };
struct App {
  android_app *android = nullptr;
  bool resumed = false, running = false;
  EGLDisplay display = EGL_NO_DISPLAY; EGLConfig config = nullptr; EGLContext context = EGL_NO_CONTEXT; EGLSurface surface = EGL_NO_SURFACE;
  XrInstance instance = XR_NULL_HANDLE; XrSystemId system = 0; XrSession session = XR_NULL_HANDLE; XrSpace space = XR_NULL_HANDLE;
  XrSessionState state = XR_SESSION_STATE_UNKNOWN;
  std::vector<XrViewConfigurationView> configs; std::vector<XrView> views; std::vector<Eye> eyes;
  GLuint program = 0, vao = 0, vbo = 0; GLint uMvp = -1; int vertices = 0;
  std::string scenePath; time_t sceneAt = 0; double checkedAt = 0;
};

static void onCmd(android_app *a, int32_t cmd) {
  App *app = (App *)a->userData;
  if (cmd == APP_CMD_RESUME) app->resumed = true;
  if (cmd == APP_CMD_PAUSE) app->resumed = false;
}

static bool startEgl(App &app) {
  app.display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if (!eglInitialize(app.display, nullptr, nullptr)) return false;
  const EGLint want[] = {EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT, EGL_SURFACE_TYPE, EGL_PBUFFER_BIT, EGL_RED_SIZE, 8, EGL_GREEN_SIZE, 8, EGL_BLUE_SIZE, 8, EGL_ALPHA_SIZE, 8, EGL_NONE};
  EGLint n = 0;
  if (!eglChooseConfig(app.display, want, &app.config, 1, &n) || n < 1) return false;
  const EGLint ctx[] = {EGL_CONTEXT_CLIENT_VERSION, 3, EGL_NONE};
  app.context = eglCreateContext(app.display, app.config, EGL_NO_CONTEXT, ctx);
  const EGLint pb[] = {EGL_WIDTH, 16, EGL_HEIGHT, 16, EGL_NONE};
  app.surface = eglCreatePbufferSurface(app.display, app.config, pb);
  return app.context != EGL_NO_CONTEXT && eglMakeCurrent(app.display, app.surface, app.surface, app.context);
}

static GLuint shader(GLenum type, const char *src) {
  GLuint s = glCreateShader(type);
  glShaderSource(s, 1, &src, nullptr);
  glCompileShader(s);
  GLint good = 0; glGetShaderiv(s, GL_COMPILE_STATUS, &good);
  if (!good) { char log[512]; glGetShaderInfoLog(s, sizeof log, nullptr, log); ERR("shader: %s", log); }
  return s;
}
static void startGl(App &app) {
  const char *vs = "#version 300 es\nlayout(location=0) in vec3 p; layout(location=1) in vec3 c; uniform mat4 mvp; out vec3 ink;\n"
                   "void main(){ ink = c; gl_Position = mvp * vec4(p, 1.0); }";
  const char *fs = "#version 300 es\nprecision mediump float; in vec3 ink; out vec4 o; void main(){ o = vec4(ink, 1.0); }";
  app.program = glCreateProgram();
  glAttachShader(app.program, shader(GL_VERTEX_SHADER, vs));
  glAttachShader(app.program, shader(GL_FRAGMENT_SHADER, fs));
  glLinkProgram(app.program);
  app.uMvp = glGetUniformLocation(app.program, "mvp");
  glGenVertexArrays(1, &app.vao); glGenBuffers(1, &app.vbo);
}
static void setMesh(App &app, const std::vector<Box> &boxes) {
  const std::vector<float> v = meshOf(boxes);
  glBindVertexArray(app.vao);
  glBindBuffer(GL_ARRAY_BUFFER, app.vbo);
  glBufferData(GL_ARRAY_BUFFER, v.size() * sizeof(float), v.data(), GL_STATIC_DRAW);
  glEnableVertexAttribArray(0); glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 24, (void *)0);
  glEnableVertexAttribArray(1); glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 24, (void *)12);
  app.vertices = (int)(v.size() / 6);
  LOG("mesh: %d boxes, %d vertices", (int)boxes.size(), app.vertices);
}
// Once a second: has the scene file changed? Then the room is made again from it.
static void freshen(App &app, double now) {
  if (now - app.checkedAt < 1.0) return;
  app.checkedAt = now;
  struct stat st;
  if (stat(app.scenePath.c_str(), &st) != 0 || st.st_mtime == app.sceneAt) return;
  std::vector<Box> boxes;
  if (readScene(app.scenePath, boxes)) { app.sceneAt = st.st_mtime; setMesh(app, boxes); }
}

static bool startXr(App &app) {
  // The loader is told of the app first (Android only).
  PFN_xrInitializeLoaderKHR init = nullptr;
  xrGetInstanceProcAddr(XR_NULL_HANDLE, "xrInitializeLoaderKHR", (PFN_xrVoidFunction *)&init);
  if (!init) { ERR("no xrInitializeLoaderKHR"); return false; }
  XrLoaderInitInfoAndroidKHR li = {XR_TYPE_LOADER_INIT_INFO_ANDROID_KHR};
  li.applicationVM = app.android->activity->vm; li.applicationContext = app.android->activity->clazz;
  if (!ok(init((XrLoaderInitInfoBaseHeaderKHR *)&li), "xrInitializeLoaderKHR")) return false;

  const char *ext[] = {XR_KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME, XR_KHR_OPENGL_ES_ENABLE_EXTENSION_NAME};
  XrInstanceCreateInfoAndroidKHR ai = {XR_TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR};
  ai.applicationVM = app.android->activity->vm; ai.applicationActivity = app.android->activity->clazz;
  XrInstanceCreateInfo ci = {XR_TYPE_INSTANCE_CREATE_INFO};
  ci.next = &ai;
  strcpy(ci.applicationInfo.applicationName, "rax"); strcpy(ci.applicationInfo.engineName, "rax");
  ci.applicationInfo.apiVersion = XR_MAKE_VERSION(1, 0, 34);
  ci.enabledExtensionCount = 2; ci.enabledExtensionNames = ext;
  if (!ok(xrCreateInstance(&ci, &app.instance), "xrCreateInstance")) return false;

  XrSystemGetInfo si = {XR_TYPE_SYSTEM_GET_INFO}; si.formFactor = XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY;
  if (!ok(xrGetSystem(app.instance, &si, &app.system), "xrGetSystem")) return false;
  XrGraphicsRequirementsOpenGLESKHR req = {XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR};
  PFN_xrGetOpenGLESGraphicsRequirementsKHR getReq = nullptr;
  xrGetInstanceProcAddr(app.instance, "xrGetOpenGLESGraphicsRequirementsKHR", (PFN_xrVoidFunction *)&getReq);
  if (getReq) getReq(app.instance, app.system, &req);

  XrGraphicsBindingOpenGLESAndroidKHR gb = {XR_TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR};
  gb.display = app.display; gb.config = app.config; gb.context = app.context;
  XrSessionCreateInfo sc = {XR_TYPE_SESSION_CREATE_INFO}; sc.next = &gb; sc.systemId = app.system;
  if (!ok(xrCreateSession(app.instance, &sc, &app.session), "xrCreateSession")) return false;

  // The floor is the roof: STAGE has its zero on the real floor. (LOCAL, if there is no stage, has it at the head: lowered by a standing eye.)
  XrReferenceSpaceCreateInfo rs = {XR_TYPE_REFERENCE_SPACE_CREATE_INFO};
  rs.poseInReferenceSpace.orientation.w = 1; rs.referenceSpaceType = XR_REFERENCE_SPACE_TYPE_STAGE;
  if (XR_FAILED(xrCreateReferenceSpace(app.session, &rs, &app.space))) {
    rs.referenceSpaceType = XR_REFERENCE_SPACE_TYPE_LOCAL; rs.poseInReferenceSpace.position.y = -1.6f;
    if (!ok(xrCreateReferenceSpace(app.session, &rs, &app.space), "xrCreateReferenceSpace")) return false;
  }

  uint32_t n = 0;
  xrEnumerateViewConfigurationViews(app.instance, app.system, XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO, 0, &n, nullptr);
  app.configs.assign(n, {XR_TYPE_VIEW_CONFIGURATION_VIEW});
  xrEnumerateViewConfigurationViews(app.instance, app.system, XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO, n, &n, app.configs.data());
  app.views.assign(n, {XR_TYPE_VIEW});
  app.eyes.resize(n);

  uint32_t fn = 0; xrEnumerateSwapchainFormats(app.session, 0, &fn, nullptr);
  std::vector<int64_t> formats(fn); xrEnumerateSwapchainFormats(app.session, fn, &fn, formats.data());
  int64_t format = formats.empty() ? GL_RGBA8 : formats[0];
  for (int64_t f : formats) if (f == GL_SRGB8_ALPHA8) { format = f; break; }

  for (uint32_t i = 0; i < n; i++) {
    Eye &e = app.eyes[i];
    e.w = app.configs[i].recommendedImageRectWidth; e.h = app.configs[i].recommendedImageRectHeight;
    XrSwapchainCreateInfo ci2 = {XR_TYPE_SWAPCHAIN_CREATE_INFO};
    ci2.usageFlags = XR_SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT | XR_SWAPCHAIN_USAGE_SAMPLED_BIT;
    ci2.format = format; ci2.sampleCount = 1; ci2.width = e.w; ci2.height = e.h; ci2.faceCount = 1; ci2.arraySize = 1; ci2.mipCount = 1;
    if (!ok(xrCreateSwapchain(app.session, &ci2, &e.chain), "xrCreateSwapchain")) return false;
    uint32_t in = 0; xrEnumerateSwapchainImages(e.chain, 0, &in, nullptr);
    std::vector<XrSwapchainImageOpenGLESKHR> images(in, {XR_TYPE_SWAPCHAIN_IMAGE_OPENGL_ES_KHR});
    xrEnumerateSwapchainImages(e.chain, in, &in, (XrSwapchainImageBaseHeader *)images.data());
    glGenRenderbuffers(1, &e.depth);
    glBindRenderbuffer(GL_RENDERBUFFER, e.depth);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, e.w, e.h);
    e.fbo.resize(in);
    glGenFramebuffers(in, e.fbo.data());
    for (uint32_t k = 0; k < in; k++) {
      glBindFramebuffer(GL_FRAMEBUFFER, e.fbo[k]);
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, images[k].image, 0);
      glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, e.depth);
    }
    LOG("eye %u: %dx%d, %u images, format %lld", i, e.w, e.h, in, (long long)format);
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  return true;
}

static void events(App &app) {
  XrEventDataBuffer ev = {XR_TYPE_EVENT_DATA_BUFFER};
  while (xrPollEvent(app.instance, &ev) == XR_SUCCESS) {
    if (ev.type == XR_TYPE_EVENT_DATA_SESSION_STATE_CHANGED) {
      const auto *sc = (XrEventDataSessionStateChanged *)&ev;
      app.state = sc->state;
      LOG("session state %d", (int)app.state);
      if (app.state == XR_SESSION_STATE_READY) {
        XrSessionBeginInfo bi = {XR_TYPE_SESSION_BEGIN_INFO}; bi.primaryViewConfigurationType = XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO;
        app.running = ok(xrBeginSession(app.session, &bi), "xrBeginSession");
      } else if (app.state == XR_SESSION_STATE_STOPPING) { xrEndSession(app.session); app.running = false; }
      else if (app.state == XR_SESSION_STATE_EXITING || app.state == XR_SESSION_STATE_LOSS_PENDING) ANativeActivity_finish(app.android->activity);
    }
    ev = {XR_TYPE_EVENT_DATA_BUFFER};
  }
}

static void frame(App &app) {
  XrFrameState fs = {XR_TYPE_FRAME_STATE};
  XrFrameWaitInfo wi = {XR_TYPE_FRAME_WAIT_INFO};
  if (XR_FAILED(xrWaitFrame(app.session, &wi, &fs))) return;
  XrFrameBeginInfo bi = {XR_TYPE_FRAME_BEGIN_INFO};
  xrBeginFrame(app.session, &bi);
  freshen(app, fs.predictedDisplayTime * 1e-9);

  std::vector<XrCompositionLayerProjectionView> pv(app.views.size(), {XR_TYPE_COMPOSITION_LAYER_PROJECTION_VIEW});
  XrCompositionLayerProjection layer = {XR_TYPE_COMPOSITION_LAYER_PROJECTION};
  const XrCompositionLayerBaseHeader *layers[1] = {(XrCompositionLayerBaseHeader *)&layer};
  uint32_t layerCount = 0;
  if (fs.shouldRender) {
    XrViewLocateInfo li = {XR_TYPE_VIEW_LOCATE_INFO};
    li.viewConfigurationType = XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO; li.displayTime = fs.predictedDisplayTime; li.space = app.space;
    XrViewState vs = {XR_TYPE_VIEW_STATE}; uint32_t got = 0;
    if (XR_SUCCEEDED(xrLocateViews(app.session, &li, &vs, (uint32_t)app.views.size(), &got, app.views.data())) && (vs.viewStateFlags & XR_VIEW_STATE_ORIENTATION_VALID_BIT)) {
      for (uint32_t i = 0; i < got; i++) {
        Eye &e = app.eyes[i];
        uint32_t index = 0;
        XrSwapchainImageAcquireInfo aq = {XR_TYPE_SWAPCHAIN_IMAGE_ACQUIRE_INFO};
        xrAcquireSwapchainImage(e.chain, &aq, &index);
        XrSwapchainImageWaitInfo wt = {XR_TYPE_SWAPCHAIN_IMAGE_WAIT_INFO}; wt.timeout = XR_INFINITE_DURATION;
        xrWaitSwapchainImage(e.chain, &wt);
        glBindFramebuffer(GL_FRAMEBUFFER, e.fbo[index]);
        glViewport(0, 0, e.w, e.h);
        glEnable(GL_DEPTH_TEST); glDisable(GL_CULL_FACE);
        glClearColor(0.035f, 0.045f, 0.09f, 1); // the night sky
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        const Mat4 mvp = mul(projection(app.views[i].fov, 0.05f, 2000.f), viewOf(app.views[i].pose));
        glUseProgram(app.program);
        glUniformMatrix4fv(app.uMvp, 1, GL_FALSE, mvp.m);
        glBindVertexArray(app.vao);
        glDrawArrays(GL_TRIANGLES, 0, app.vertices);
        XrSwapchainImageReleaseInfo rl = {XR_TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO};
        xrReleaseSwapchainImage(e.chain, &rl);
        pv[i].pose = app.views[i].pose; pv[i].fov = app.views[i].fov;
        pv[i].subImage.swapchain = e.chain; pv[i].subImage.imageRect.extent = {e.w, e.h};
      }
      layer.space = app.space; layer.viewCount = got; layer.views = pv.data();
      layerCount = 1;
    }
  }
  XrFrameEndInfo ei = {XR_TYPE_FRAME_END_INFO};
  ei.displayTime = fs.predictedDisplayTime; ei.environmentBlendMode = XR_ENVIRONMENT_BLEND_MODE_OPAQUE;
  ei.layerCount = layerCount; ei.layers = layers;
  xrEndFrame(app.session, &ei);
}

void android_main(android_app *android) {
  App app; app.android = android;
  android->userData = &app; android->onAppCmd = onCmd;
  JNIEnv *env = nullptr; android->activity->vm->AttachCurrentThread(&env, nullptr);
  app.scenePath = std::string(android->activity->internalDataPath) + "/scene.txt";
  LOG("rax native: scene at %s", app.scenePath.c_str());

  if (!startEgl(app)) { ERR("no EGL"); return; }
  startGl(app);
  setMesh(app, std::vector<Box>(BUILT_IN, BUILT_IN + sizeof BUILT_IN / sizeof *BUILT_IN));
  if (!startXr(app)) { ERR("no OpenXR"); ANativeActivity_finish(android->activity); }

  while (!android->destroyRequested) {
    int n = 0; android_poll_source *src = nullptr;
    // (Running, the loop never waits on Android: OpenXR paces it. Idle, it waits a little.)
    while (ALooper_pollOnce(app.running ? 0 : 100, nullptr, &n, (void **)&src) >= 0) { if (src) src->process(android, src); if (android->destroyRequested) break; }
    if (app.instance == XR_NULL_HANDLE) continue;
    events(app);
    if (app.running) frame(app);
  }
  android->activity->vm->DetachCurrentThread();
}
