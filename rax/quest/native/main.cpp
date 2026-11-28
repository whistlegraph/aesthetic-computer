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
// Mixed reality only, and hands only (2026-11-28). The real room is seen
// (XR_FB_passthrough, a layer under ours; ours is cleared to nothing), and the
// rax room stands in it small, as on a table: the roof about 0.8 m across,
// a little before and below the head as the headset is put on (so taking it
// off and on again puts a lost room back). No controllers: the
// hands are tracked (XR_EXT_hand_tracking), drawn as small blocks at their
// joints, and two fists take hold of the little world, to move it, size it
// and turn it about the upright (the arithmetic is rax/immersive.mjs's:
// fistOf, grabStart, grabStep). What is below the real floor is not drawn.
//
// Who is in the room is a second file (live.txt), the same kind of lines,
// that the Java side asks the server for about ten times a second; it is a
// second mesh, made again whenever the file changes, drawn over the room.
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
static std::vector<float> meshOf(const std::vector<Box> &boxes, float flip = -1) {
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
        v.push_back(flip * (b.x + c[0] * b.w / 2)); v.push_back(b.y + c[1] * b.h / 2); v.push_back(flip * (b.z + c[2] * b.d / 2));
        v.push_back(r * shade[f]); v.push_back(g * shade[f]); v.push_back(bl * shade[f]);
      }
    }
  }
  return v;
}
// The scene file: "x y z w h d rrggbb" a line; '#' begins a note. Nothing read is nothing changed
// (but the live file may rightly be empty: nobody there).
static bool readScene(const std::string &path, std::vector<Box> &out, bool mayBeEmpty = false) {
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
  if (boxes.empty() && !mayBeEmpty) return false;
  out.swap(boxes);
  return true;
}

// ── the room on a table, and two fists that hold it (rax/immersive.mjs) ────
// The room's point under a real point q is  pivot + (turn(q, rot) - at) / scale;
// all in OpenXR's axes here (the mesh is already turned into them).
struct V3 { float x, y, z; };
struct Table { float scale = 0.05f, rot = 0; V3 pivot = {0, 0, 0}, at = {0, 0, 0}; };
struct Hold { V3 a1, a2; };
static V3 turn(V3 v, float a) { const float c = cosf(a), s = sinf(a); return {v.x * c + v.z * s, v.y, -v.x * s + v.z * c}; }
static float len(V3 v) { return sqrtf(v.x * v.x + v.y * v.y + v.z * v.z); }
static V3 roomAt(const Table &t, V3 q) {
  const V3 p = turn(q, t.rot);
  return {t.pivot.x + (p.x - t.at.x) / t.scale, t.pivot.y + (p.y - t.at.y) / t.scale, t.pivot.z + (p.z - t.at.z) / t.scale};
}
static Hold grabStart(const Table &t, V3 q1, V3 q2) { return {roomAt(t, q1), roomAt(t, q2)}; }
static void grabStep(Table &t, const Hold &h, V3 q1, V3 q2, float least = 0.004f, float most = 2) {
  const V3 dq = {q2.x - q1.x, q2.y - q1.y, q2.z - q1.z}, da = {h.a2.x - h.a1.x, h.a2.y - h.a1.y, h.a2.z - h.a1.z};
  const float far = len(da);
  if (far < 1e-6f || len(dq) < 0.02f) return;
  const float scale = fminf(most, fmaxf(least, len(dq) / far));
  // (Turned only when the fists are apart across the floor: one over the other says nothing of which way round.)
  const bool flat = hypotf(dq.x, dq.z) > 0.04f && hypotf(da.x, da.z) > 1e-6f;
  const float rot = flat ? atan2f(da.x, da.z) - atan2f(dq.x, dq.z) : t.rot;
  const V3 qm = turn({(q1.x + q2.x) / 2, (q1.y + q2.y) / 2, (q1.z + q2.z) / 2}, rot);
  const V3 am = {(h.a1.x + h.a2.x) / 2, (h.a1.y + h.a2.y) / 2, (h.a1.z + h.a2.z) / 2};
  t.scale = scale; t.rot = rot;
  t.at = {qm.x - scale * (am.x - t.pivot.x), qm.y - scale * (am.y - t.pivot.y), qm.z - scale * (am.z - t.pivot.z)};
}
// The matrix that puts the room's mesh where the table says: q = turn(at + scale (a - pivot), -rot).
static Mat4 modelOf(const Table &t) {
  const float c = cosf(-t.rot), s = sinf(-t.rot), k = t.scale;
  const V3 o = {t.at.x - k * t.pivot.x, t.at.y - k * t.pivot.y, t.at.z - k * t.pivot.z};
  Mat4 m = {};
  m.m[0] = c * k; m.m[2] = -s * k; m.m[5] = k; m.m[8] = s * k; m.m[10] = c * k;
  m.m[12] = c * o.x + s * o.z; m.m[13] = o.y; m.m[14] = -s * o.x + c * o.z; m.m[15] = 1;
  return m;
}
static Mat4 identity() { Mat4 m = {}; m.m[0] = m.m[5] = m.m[10] = m.m[15] = 1; return m; }

// ── the app ────────────────────────────────────────────────────────────────
struct Eye { XrSwapchain chain = XR_NULL_HANDLE; int w = 0, h = 0; std::vector<GLuint> fbo; GLuint depth = 0; };
struct Mesh { GLuint vao = 0, vbo = 0; int vertices = 0; };
struct App {
  android_app *android = nullptr;
  bool resumed = false, running = false;
  EGLDisplay display = EGL_NO_DISPLAY; EGLConfig config = nullptr; EGLContext context = EGL_NO_CONTEXT; EGLSurface surface = EGL_NO_SURFACE;
  XrInstance instance = XR_NULL_HANDLE; XrSystemId system = 0; XrSession session = XR_NULL_HANDLE; XrSpace space = XR_NULL_HANDLE;
  XrSessionState state = XR_SESSION_STATE_UNKNOWN;
  std::vector<XrViewConfigurationView> configs; std::vector<XrView> views; std::vector<Eye> eyes;
  GLuint program = 0; GLint uMvp = -1, uModel = -1, uFloor = -1;
  Mesh room, live, hands; // the room (seldom made), who is in it (ten times a second), the two hands (every frame)
  std::string scenePath, livePath; time_t sceneAt = 0; long long liveAt = 0; double checkedAt = 0, saidAt = 0;
  int roomBoxes = 0, liveBoxes = 0, liveReads = 0;
  // Mixed reality: the passthrough and its layer (or, without the extension, the runtime's own alpha blend).
  bool hasPassthrough = false, hasHands = false, alphaBlend = false, stage = true;
  XrPassthroughFB passthrough = XR_NULL_HANDLE; XrPassthroughLayerFB passLayer = XR_NULL_HANDLE;
  // Hands: a tracker each (left, right), where the joints were this frame, and whether each is a fist.
  XrHandTrackerEXT tracker[2] = {XR_NULL_HANDLE, XR_NULL_HANDLE};
  PFN_xrLocateHandJointsEXT locateJoints = nullptr;
  XrHandJointLocationEXT joints[2][XR_HAND_JOINT_COUNT_EXT]; bool tracked[2] = {false, false}, fist[2] = {false, false}; float curl[2] = {0, 0};
  Table table; bool placed = false, holding = false, away = true; Hold hold;
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
  // (`high`: how far over the real floor the point stands; under it, with `fl` set, nothing is drawn: the little tower goes down into the floor and no further.)
  const char *vs = "#version 300 es\nlayout(location=0) in vec3 p; layout(location=1) in vec3 c; uniform mat4 mvp; uniform mat4 model; out vec3 ink; out float high;\n"
                   "void main(){ ink = c; high = (model * vec4(p, 1.0)).y; gl_Position = mvp * vec4(p, 1.0); }";
  const char *fs = "#version 300 es\nprecision mediump float; in vec3 ink; in float high; uniform float fl; out vec4 o;\n"
                   "void main(){ if (fl > 0.5 && high < 0.0) discard; o = vec4(ink, 1.0); }";
  app.program = glCreateProgram();
  glAttachShader(app.program, shader(GL_VERTEX_SHADER, vs));
  glAttachShader(app.program, shader(GL_FRAGMENT_SHADER, fs));
  glLinkProgram(app.program);
  app.uMvp = glGetUniformLocation(app.program, "mvp");
  app.uModel = glGetUniformLocation(app.program, "model");
  app.uFloor = glGetUniformLocation(app.program, "fl");
  for (Mesh *m : {&app.room, &app.live, &app.hands}) { glGenVertexArrays(1, &m->vao); glGenBuffers(1, &m->vbo); }
}
static void setMesh(Mesh &m, const std::vector<Box> &boxes, GLenum usage = GL_STATIC_DRAW, float flip = -1) {
  const std::vector<float> v = meshOf(boxes, flip);
  glBindVertexArray(m.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m.vbo);
  glBufferData(GL_ARRAY_BUFFER, v.size() * sizeof(float), v.data(), usage);
  glEnableVertexAttribArray(0); glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 24, (void *)0);
  glEnableVertexAttribArray(1); glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 24, (void *)12);
  m.vertices = (int)(v.size() / 6);
}
// Once a second: has the scene file changed? Then the room is made again from it.
// Every frame: has the live file? (One stat; it is written, by a rename, only when what it says has changed.)
static void freshen(App &app, double now) {
  struct stat st;
  if (stat(app.livePath.c_str(), &st) == 0) {
    const long long at = (long long)st.st_mtim.tv_sec * 1000000000LL + st.st_mtim.tv_nsec + st.st_ino; // (a rename is a new file: its number tells two made in one instant apart)
    std::vector<Box> boxes;
    if (at != app.liveAt && readScene(app.livePath, boxes, true)) { app.liveAt = at; setMesh(app.live, boxes, GL_DYNAMIC_DRAW); app.liveBoxes = (int)boxes.size(); app.liveReads++; }
  }
  if (now - app.checkedAt < 1.0) return;
  app.checkedAt = now;
  if (stat(app.scenePath.c_str(), &st) != 0 || st.st_mtime == app.sceneAt) return;
  std::vector<Box> boxes;
  if (readScene(app.scenePath, boxes)) { app.sceneAt = st.st_mtime; setMesh(app.room, boxes); app.roomBoxes = (int)boxes.size(); LOG("room: %d boxes, %d vertices", app.roomBoxes, app.room.vertices); }
}

// ── hands ──────────────────────────────────────────────────────────────────
// A hand is a fist when its four fingertips are curled in by the wrist (immersive.mjs fistOf, FIST_CURL);
// a fist once made is kept until the fingers are a little further out than that, so it does not flicker at the line.
static const float FIST_CURL = 0.095f, FIST_LOOSE = 0.115f;
static V3 jointAt(const App &app, int side, int j) { const XrVector3f &p = app.joints[side][j].pose.position; return {p.x, p.y, p.z}; }
static void readHands(App &app, XrTime time) {
  std::vector<Box> boxes;
  for (int side = 0; side < 2; side++) {
    app.tracked[side] = false;
    if (app.tracker[side] == XR_NULL_HANDLE || !app.locateJoints) { app.fist[side] = false; continue; }
    XrHandJointsLocateInfoEXT li = {XR_TYPE_HAND_JOINTS_LOCATE_INFO_EXT}; li.baseSpace = app.space; li.time = time;
    XrHandJointLocationsEXT out = {XR_TYPE_HAND_JOINT_LOCATIONS_EXT}; out.jointCount = XR_HAND_JOINT_COUNT_EXT; out.jointLocations = app.joints[side];
    if (XR_FAILED(app.locateJoints(app.tracker[side], &li, &out)) || !out.isActive) { app.fist[side] = false; continue; }
    static const int need[] = {XR_HAND_JOINT_WRIST_EXT, XR_HAND_JOINT_MIDDLE_PROXIMAL_EXT, XR_HAND_JOINT_INDEX_TIP_EXT, XR_HAND_JOINT_MIDDLE_TIP_EXT, XR_HAND_JOINT_RING_TIP_EXT, XR_HAND_JOINT_LITTLE_TIP_EXT};
    bool all = true;
    for (int j : need) if (!(app.joints[side][j].locationFlags & XR_SPACE_LOCATION_POSITION_VALID_BIT)) all = false;
    if (!all) { app.fist[side] = false; continue; }
    app.tracked[side] = true;
    const V3 wrist = jointAt(app, side, XR_HAND_JOINT_WRIST_EXT);
    float sum = 0;
    for (int k = 2; k < 6; k++) { const V3 t = jointAt(app, side, need[k]); sum += len({t.x - wrist.x, t.y - wrist.y, t.z - wrist.z}); }
    app.curl[side] = sum / 4;
    app.fist[side] = app.curl[side] < (app.fist[side] ? FIST_LOOSE : FIST_CURL);
    // The hand as it is drawn: a block at each joint (a bigger one at a fingertip), yellow when it is a fist.
    for (int j = 1; j < XR_HAND_JOINT_COUNT_EXT; j++) {
      if (!(app.joints[side][j].locationFlags & XR_SPACE_LOCATION_POSITION_VALID_BIT)) continue;
      const V3 p = jointAt(app, side, j);
      const bool tip = j == XR_HAND_JOINT_THUMB_TIP_EXT || j == XR_HAND_JOINT_INDEX_TIP_EXT || j == XR_HAND_JOINT_MIDDLE_TIP_EXT || j == XR_HAND_JOINT_RING_TIP_EXT || j == XR_HAND_JOINT_LITTLE_TIP_EXT;
      const float size = tip ? 0.014f : 0.01f;
      boxes.push_back({p.x, p.y, p.z, size, size, size, app.fist[side] ? 0xffd23fu : (side ? 0x69bfffu : 0xff8fc7u)});
    }
  }
  // ✊ Both closed: the two points of the room under them stay under them. One opened: let go.
  const bool both = app.tracked[0] && app.tracked[1] && app.fist[0] && app.fist[1];
  if (both && app.placed) {
    const V3 q1 = jointAt(app, 0, XR_HAND_JOINT_MIDDLE_PROXIMAL_EXT), q2 = jointAt(app, 1, XR_HAND_JOINT_MIDDLE_PROXIMAL_EXT);
    if (!app.holding) { app.hold = grabStart(app.table, q1, q2); app.holding = true; LOG("grab: taken (scale %.4f rot %.2f)", app.table.scale, app.table.rot); }
    else grabStep(app.table, app.hold, q1, q2);
  } else if (app.holding) { app.holding = false; LOG("grab: let go (scale %.4f rot %.2f at %.2f %.2f %.2f)", app.table.scale, app.table.rot, app.table.at.x, app.table.at.y, app.table.at.z); }
  setMesh(app.hands, boxes, GL_DYNAMIC_DRAW, 1);
}
// 🪑 The room put down as the app starts: its roof (16 m, so a twentieth) 0.8 m across, its middle 0.6 m before the head and 0.35 m under it, its clock tower away from whoever looks.
static void place(App &app, const XrPosef &head) {
  const XrQuaternionf &q = head.orientation;
  float fx = -2 * (q.x * q.z + q.y * q.w), fz = -(1 - 2 * (q.x * q.x + q.y * q.y)); // (-z, turned by the head)
  const float flat = hypotf(fx, fz);
  if (flat < 1e-3f) { fx = 0; fz = -1; } else { fx /= flat; fz /= flat; }
  const V3 middle = {head.position.x + fx * 0.6f, fmaxf(0.3f, head.position.y - 0.35f), head.position.z + fz * 0.6f};
  app.table.scale = 0.05f; app.table.rot = atan2f(fx, -fz); app.table.pivot = {0, 0, 0};
  app.table.at = turn(middle, app.table.rot);
  app.placed = true;
  LOG("table: placed at %.2f %.2f %.2f (head %.2f %.2f %.2f), rot %.2f, scale %.3f", middle.x, middle.y, middle.z, head.position.x, head.position.y, head.position.z, app.table.rot, app.table.scale);
}

static bool startXr(App &app) {
  // The loader is told of the app first (Android only).
  PFN_xrInitializeLoaderKHR init = nullptr;
  xrGetInstanceProcAddr(XR_NULL_HANDLE, "xrInitializeLoaderKHR", (PFN_xrVoidFunction *)&init);
  if (!init) { ERR("no xrInitializeLoaderKHR"); return false; }
  XrLoaderInitInfoAndroidKHR li = {XR_TYPE_LOADER_INIT_INFO_ANDROID_KHR};
  li.applicationVM = app.android->activity->vm; li.applicationContext = app.android->activity->clazz;
  if (!ok(init((XrLoaderInitInfoBaseHeaderKHR *)&li), "xrInitializeLoaderKHR")) return false;

  // What this headset offers, said in the log; passthrough and hands are taken if they are there.
  uint32_t en = 0; xrEnumerateInstanceExtensionProperties(nullptr, 0, &en, nullptr);
  std::vector<XrExtensionProperties> have(en, {XR_TYPE_EXTENSION_PROPERTIES});
  xrEnumerateInstanceExtensionProperties(nullptr, en, &en, have.data());
  LOG("extensions: %u", en);
  { std::string row; for (uint32_t i = 0; i < en; i++) { row += have[i].extensionName; row += ' '; if (row.size() > 600 || i + 1 == en) { LOG("ext: %s", row.c_str()); row.clear(); } } }
  const auto offered = [&](const char *name) { for (const auto &e : have) if (!strcmp(e.extensionName, name)) return true; return false; };
  app.hasPassthrough = offered(XR_FB_PASSTHROUGH_EXTENSION_NAME); app.hasHands = offered(XR_EXT_HAND_TRACKING_EXTENSION_NAME);
  LOG("XR_FB_passthrough %s, XR_EXT_hand_tracking %s", app.hasPassthrough ? "offered" : "NOT offered", app.hasHands ? "offered" : "NOT offered");
  std::vector<const char *> ext = {XR_KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME, XR_KHR_OPENGL_ES_ENABLE_EXTENSION_NAME};
  if (app.hasPassthrough) ext.push_back(XR_FB_PASSTHROUGH_EXTENSION_NAME);
  if (app.hasHands) ext.push_back(XR_EXT_HAND_TRACKING_EXTENSION_NAME);
  XrInstanceCreateInfoAndroidKHR ai = {XR_TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR};
  ai.applicationVM = app.android->activity->vm; ai.applicationActivity = app.android->activity->clazz;
  XrInstanceCreateInfo ci = {XR_TYPE_INSTANCE_CREATE_INFO};
  ci.next = &ai;
  strcpy(ci.applicationInfo.applicationName, "rax"); strcpy(ci.applicationInfo.engineName, "rax");
  ci.applicationInfo.apiVersion = XR_MAKE_VERSION(1, 0, 34);
  ci.enabledExtensionCount = (uint32_t)ext.size(); ci.enabledExtensionNames = ext.data();
  if (!ok(xrCreateInstance(&ci, &app.instance), "xrCreateInstance")) return false;

  XrSystemGetInfo si = {XR_TYPE_SYSTEM_GET_INFO}; si.formFactor = XR_FORM_FACTOR_HEAD_MOUNTED_DISPLAY;
  if (!ok(xrGetSystem(app.instance, &si, &app.system), "xrGetSystem")) return false;
  XrGraphicsRequirementsOpenGLESKHR req = {XR_TYPE_GRAPHICS_REQUIREMENTS_OPENGL_ES_KHR};
  PFN_xrGetOpenGLESGraphicsRequirementsKHR getReq = nullptr;
  xrGetInstanceProcAddr(app.instance, "xrGetOpenGLESGraphicsRequirementsKHR", (PFN_xrVoidFunction *)&getReq);
  if (getReq) getReq(app.instance, app.system, &req);
  // What the system says of itself: passthrough, hand tracking, and the ways it can blend with the real room.
  {
    XrSystemHandTrackingPropertiesEXT hp = {XR_TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT};
    XrSystemPassthroughProperties2FB pp = {XR_TYPE_SYSTEM_PASSTHROUGH_PROPERTIES2_FB};
    XrSystemProperties sp = {XR_TYPE_SYSTEM_PROPERTIES};
    void **tail = &sp.next;
    if (app.hasHands) { *tail = &hp; tail = &hp.next; }
    if (app.hasPassthrough) { *tail = &pp; tail = (void **)&pp.next; }
    if (ok(xrGetSystemProperties(app.instance, app.system, &sp), "xrGetSystemProperties"))
      LOG("system: %s, hand tracking %d, passthrough capabilities 0x%llx", sp.systemName, (int)hp.supportsHandTracking, (unsigned long long)pp.capabilities);
    if (app.hasHands && !hp.supportsHandTracking) { LOG("hands: the system says it does not track them"); app.hasHands = false; }
    uint32_t bn = 0; XrEnvironmentBlendMode modes[8];
    xrEnumerateEnvironmentBlendModes(app.instance, app.system, XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO, 8, &bn, modes);
    for (uint32_t i = 0; i < bn; i++) { LOG("blend mode offered: %d", (int)modes[i]); if (modes[i] == XR_ENVIRONMENT_BLEND_MODE_ALPHA_BLEND) app.alphaBlend = true; }
  }

  XrGraphicsBindingOpenGLESAndroidKHR gb = {XR_TYPE_GRAPHICS_BINDING_OPENGL_ES_ANDROID_KHR};
  gb.display = app.display; gb.config = app.config; gb.context = app.context;
  XrSessionCreateInfo sc = {XR_TYPE_SESSION_CREATE_INFO}; sc.next = &gb; sc.systemId = app.system;
  if (!ok(xrCreateSession(app.instance, &sc, &app.session), "xrCreateSession")) return false;

  // The floor is the roof: STAGE has its zero on the real floor. (LOCAL, if there is no stage, has it at the head: lowered by a standing eye.)
  XrReferenceSpaceCreateInfo rs = {XR_TYPE_REFERENCE_SPACE_CREATE_INFO};
  rs.poseInReferenceSpace.orientation.w = 1; rs.referenceSpaceType = XR_REFERENCE_SPACE_TYPE_STAGE;
  if (XR_FAILED(xrCreateReferenceSpace(app.session, &rs, &app.space))) {
    app.stage = false; LOG("space: no STAGE, LOCAL lowered by 1.6 m");
    rs.referenceSpaceType = XR_REFERENCE_SPACE_TYPE_LOCAL; rs.poseInReferenceSpace.position.y = -1.6f;
    if (!ok(xrCreateReferenceSpace(app.session, &rs, &app.space), "xrCreateReferenceSpace")) return false;
  }

  // 🪟 The real room: a passthrough, running, and one layer of it (the whole view, as the cameras rebuild it) to lie under ours.
  if (app.hasPassthrough) {
    PFN_xrCreatePassthroughFB createPass = nullptr; PFN_xrCreatePassthroughLayerFB createLayer = nullptr;
    xrGetInstanceProcAddr(app.instance, "xrCreatePassthroughFB", (PFN_xrVoidFunction *)&createPass);
    xrGetInstanceProcAddr(app.instance, "xrCreatePassthroughLayerFB", (PFN_xrVoidFunction *)&createLayer);
    XrPassthroughCreateInfoFB pc = {XR_TYPE_PASSTHROUGH_CREATE_INFO_FB}; pc.flags = XR_PASSTHROUGH_IS_RUNNING_AT_CREATION_BIT_FB;
    if (createPass && createLayer && ok(createPass(app.session, &pc, &app.passthrough), "xrCreatePassthroughFB")) {
      XrPassthroughLayerCreateInfoFB lc = {XR_TYPE_PASSTHROUGH_LAYER_CREATE_INFO_FB};
      lc.passthrough = app.passthrough; lc.flags = XR_PASSTHROUGH_IS_RUNNING_AT_CREATION_BIT_FB; lc.purpose = XR_PASSTHROUGH_LAYER_PURPOSE_RECONSTRUCTION_FB;
      if (!ok(createLayer(app.session, &lc, &app.passLayer), "xrCreatePassthroughLayerFB")) app.passLayer = XR_NULL_HANDLE;
      // (Both are asked for running as they are made: starting one again is refused as already so.)
    }
    LOG("passthrough: %s", app.passLayer != XR_NULL_HANDLE ? "layer made (reconstruction), under the room" : "NOT made");
  }
  if (app.passLayer == XR_NULL_HANDLE) LOG("passthrough: none by XR_FB_passthrough; %s", app.alphaBlend ? "the runtime's alpha blend is used" : "and no alpha blend either: the room will stand in black");
  // 🖐️ A tracker for each hand.
  if (app.hasHands) {
    PFN_xrCreateHandTrackerEXT createTracker = nullptr;
    xrGetInstanceProcAddr(app.instance, "xrCreateHandTrackerEXT", (PFN_xrVoidFunction *)&createTracker);
    xrGetInstanceProcAddr(app.instance, "xrLocateHandJointsEXT", (PFN_xrVoidFunction *)&app.locateJoints);
    for (int side = 0; side < 2 && createTracker; side++) {
      XrHandTrackerCreateInfoEXT hc = {XR_TYPE_HAND_TRACKER_CREATE_INFO_EXT};
      hc.hand = side ? XR_HAND_RIGHT_EXT : XR_HAND_LEFT_EXT; hc.handJointSet = XR_HAND_JOINT_SET_DEFAULT_EXT;
      if (!ok(createTracker(app.session, &hc, &app.tracker[side]), side ? "xrCreateHandTrackerEXT right" : "xrCreateHandTrackerEXT left")) app.tracker[side] = XR_NULL_HANDLE;
    }
    LOG("hands: trackers %s %s", app.tracker[0] != XR_NULL_HANDLE ? "left" : "(no left)", app.tracker[1] != XR_NULL_HANDLE ? "right" : "(no right)");
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
      // The headset taken off and put on again (or put on for the first time): the room is put down afresh before the head, wherever it was left.
      if (app.state < XR_SESSION_STATE_VISIBLE || app.state > XR_SESSION_STATE_FOCUSED) app.away = true;
      else if (app.state == XR_SESSION_STATE_FOCUSED && app.away) { app.away = false; app.placed = false; app.holding = false; }
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
  const double now = fs.predictedDisplayTime * 1e-9;
  freshen(app, now);
  readHands(app, fs.predictedDisplayTime);
  if (now - app.saidAt > 2.0) {
    app.saidAt = now;
    LOG("hands L %s curl %.3f %s | R %s curl %.3f %s | %s | table scale %.4f rot %.2f at %.2f %.2f %.2f | room %d boxes, live %d boxes (%d reads) | render %d",
        app.tracked[0] ? "tracked" : "unseen", app.curl[0], app.fist[0] ? "FIST" : "open", app.tracked[1] ? "tracked" : "unseen", app.curl[1], app.fist[1] ? "FIST" : "open",
        app.holding ? "HOLDING" : "free", app.table.scale, app.table.rot, app.table.at.x, app.table.at.y, app.table.at.z, app.roomBoxes, app.liveBoxes, app.liveReads, (int)fs.shouldRender);
  }

  std::vector<XrCompositionLayerProjectionView> pv(app.views.size(), {XR_TYPE_COMPOSITION_LAYER_PROJECTION_VIEW});
  XrCompositionLayerProjection layer = {XR_TYPE_COMPOSITION_LAYER_PROJECTION};
  XrCompositionLayerPassthroughFB pass = {XR_TYPE_COMPOSITION_LAYER_PASSTHROUGH_FB};
  const XrCompositionLayerBaseHeader *layers[2];
  uint32_t layerCount = 0;
  // (The real room first, so it lies under; ours over it, blended by its own alpha: nothing where nothing was drawn.)
  if (app.passLayer != XR_NULL_HANDLE) { pass.layerHandle = app.passLayer; pass.flags = XR_COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT; layers[layerCount++] = (XrCompositionLayerBaseHeader *)&pass; }
  if (fs.shouldRender) {
    XrViewLocateInfo li = {XR_TYPE_VIEW_LOCATE_INFO};
    li.viewConfigurationType = XR_VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO; li.displayTime = fs.predictedDisplayTime; li.space = app.space;
    XrViewState vs = {XR_TYPE_VIEW_STATE}; uint32_t got = 0;
    if (XR_SUCCEEDED(xrLocateViews(app.session, &li, &vs, (uint32_t)app.views.size(), &got, app.views.data())) && (vs.viewStateFlags & XR_VIEW_STATE_ORIENTATION_VALID_BIT)) {
      if (!app.placed && app.state == XR_SESSION_STATE_FOCUSED && (vs.viewStateFlags & XR_VIEW_STATE_POSITION_VALID_BIT) && got) {
        XrPosef head = app.views[0].pose;
        if (got > 1) { head.position.x = (head.position.x + app.views[1].pose.position.x) / 2; head.position.y = (head.position.y + app.views[1].pose.position.y) / 2; head.position.z = (head.position.z + app.views[1].pose.position.z) / 2; }
        place(app, head);
      }
      const Mat4 model = modelOf(app.table), same = identity();
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
        glClearColor(0, 0, 0, 0); // nothing: the real room shows wherever the little one is not
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        const Mat4 vp = mul(projection(app.views[i].fov, 0.03f, 200.f), viewOf(app.views[i].pose));
        glUseProgram(app.program);
        if (app.placed) {
          // The room and who is in it, on the table; cut off at the real floor.
          const Mat4 mvp = mul(vp, model);
          glUniformMatrix4fv(app.uMvp, 1, GL_FALSE, mvp.m); glUniformMatrix4fv(app.uModel, 1, GL_FALSE, model.m); glUniform1f(app.uFloor, 1);
          glBindVertexArray(app.room.vao); glDrawArrays(GL_TRIANGLES, 0, app.room.vertices);
          if (app.live.vertices) { glBindVertexArray(app.live.vao); glDrawArrays(GL_TRIANGLES, 0, app.live.vertices); }
        }
        // The hands, where they are.
        if (app.hands.vertices) {
          glUniformMatrix4fv(app.uMvp, 1, GL_FALSE, vp.m); glUniformMatrix4fv(app.uModel, 1, GL_FALSE, same.m); glUniform1f(app.uFloor, 0);
          glBindVertexArray(app.hands.vao); glDrawArrays(GL_TRIANGLES, 0, app.hands.vertices);
        }
        XrSwapchainImageReleaseInfo rl = {XR_TYPE_SWAPCHAIN_IMAGE_RELEASE_INFO};
        xrReleaseSwapchainImage(e.chain, &rl);
        pv[i].pose = app.views[i].pose; pv[i].fov = app.views[i].fov;
        pv[i].subImage.swapchain = e.chain; pv[i].subImage.imageRect.extent = {e.w, e.h};
      }
      layer.space = app.space; layer.viewCount = got; layer.views = pv.data();
      layer.layerFlags = XR_COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT;
      layers[layerCount++] = (XrCompositionLayerBaseHeader *)&layer;
    }
  }
  if (!fs.shouldRender) layerCount = 0;
  XrFrameEndInfo ei = {XR_TYPE_FRAME_END_INFO};
  ei.displayTime = fs.predictedDisplayTime; ei.environmentBlendMode = app.passLayer == XR_NULL_HANDLE && app.alphaBlend ? XR_ENVIRONMENT_BLEND_MODE_ALPHA_BLEND : XR_ENVIRONMENT_BLEND_MODE_OPAQUE;
  ei.layerCount = layerCount; ei.layers = layers;
  // (What the compositor made of the layers, said when it changes: the one sign from in here that the real room and ours were both taken.)
  static XrResult was = XR_RESULT_MAX_ENUM; static uint32_t wasLayers = 99;
  const XrResult ended = xrEndFrame(app.session, &ei);
  if (ended != was || layerCount != wasLayers) { LOG("xrEndFrame: %d with %u layers (%s)", (int)ended, layerCount, layerCount == 2 ? "passthrough under the room" : layerCount == 1 ? "one" : "none"); was = ended; wasLayers = layerCount; }
}

void android_main(android_app *android) {
  App app; app.android = android;
  android->userData = &app; android->onAppCmd = onCmd;
  JNIEnv *env = nullptr; android->activity->vm->AttachCurrentThread(&env, nullptr);
  app.scenePath = std::string(android->activity->internalDataPath) + "/scene.txt";
  app.livePath = std::string(android->activity->internalDataPath) + "/live.txt";
  LOG("rax native: scene at %s, live at %s", app.scenePath.c_str(), app.livePath.c_str());

  if (!startEgl(app)) { ERR("no EGL"); return; }
  startGl(app);
  setMesh(app.room, std::vector<Box>(BUILT_IN, BUILT_IN + sizeof BUILT_IN / sizeof *BUILT_IN));
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
