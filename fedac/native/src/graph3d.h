#ifndef AC_GRAPH3D_H
#define AC_GRAPH3D_H

#include "framebuffer.h"
#include "color.h"
#include <math.h>

// ============================================================
// 3D Math Types (column-major like OpenGL / gl-matrix)
// ============================================================

typedef float mat4[16];
typedef float vec3[3];
typedef float vec4[4];

// ============================================================
// Depth Buffer
// ============================================================

typedef struct {
    float *data;
    int width;
    int height;
    int stride;
} ACDepthBuffer;

ACDepthBuffer *depth_create(int w, int h, int stride);
void depth_clear(ACDepthBuffer *db);
void depth_destroy(ACDepthBuffer *db);

// ============================================================
// Clip-space vertex (before perspective divide)
// ============================================================

typedef struct {
    float pos[4];     // clip-space x, y, z, w
    float color[4];   // rgba 0-1
    float uv[2];      // texture coordinates
} ClipVertex;

// ============================================================
// Screen-space vertex (after perspective divide + viewport)
// ============================================================

typedef struct {
    float sx, sy;     // screen pixel coordinates
    float z;          // NDC z for depth test
    float w;          // clip-space w for perspective-correct interp
    float color[4];   // rgba 0-1
    float uv[2];      // texture coordinates
} ScreenVertex;

// ============================================================
// 3D Form (mesh) — created in JS, rendered in C
// ============================================================

#define FORM_TYPE_TRIANGLE 0
#define FORM_TYPE_LINE     1

typedef struct {
    int type;               // FORM_TYPE_TRIANGLE or FORM_TYPE_LINE
    float *positions;       // [x,y,z,w] per vertex, flat array
    float *colors;          // [r,g,b,a] per vertex, flat array (may be NULL)
    float *tex_coords;      // [u,v] per vertex, flat array (may be NULL)
    int vert_count;

    float position[3];     // world position
    float rotation[3];     // euler angles in degrees (pitch, yaw, roll)
    float scale;           // uniform scale

    ACFramebuffer *texture; // optional texture (painting)
    int no_fade;           // disable near-plane fading
    int has_colors;        // per-vertex colors present
} ACForm;

// ============================================================
// FPS Camera
// ============================================================

typedef struct {
    float x, y, z;             // world position
    float rotX, rotY, rotZ;    // pitch, yaw, roll in degrees
    float speed;               // movement speed (units per frame)
    float sensitivity;         // mouse look sensitivity (degrees per pixel)
} ACCamera3D;

// ============================================================
// Render Stats
// ============================================================

typedef struct {
    int originalTriangles;
    int clippedTriangles;
    int subdividedTriangles;
    int trianglesRejected;
    int pixelsDrawn;
    int wireframeSegmentsTotal;
    int wireframeSegmentsTextured;
    int wireframeSegmentsGradient;
} ACRenderStats;

// ============================================================
// Matrix Operations
// ============================================================

void mat4_identity(mat4 out);
void mat4_copy(mat4 out, const mat4 m);
void mat4_multiply(mat4 out, const mat4 a, const mat4 b);
void mat4_perspective(mat4 out, float fov_rad, float aspect, float near, float far);
void mat4_translate(mat4 out, const mat4 m, float x, float y, float z);
void mat4_rotateX(mat4 out, const mat4 m, float rad);
void mat4_rotateY(mat4 out, const mat4 m, float rad);
void mat4_rotateZ(mat4 out, const mat4 m, float rad);
void mat4_scale_uniform(mat4 out, const mat4 m, float s);
void mat4_transform_vec4(vec4 out, const mat4 m, const vec4 v);

// ============================================================
// Camera
// ============================================================

void camera3d_init(ACCamera3D *cam);
void camera3d_update(ACCamera3D *cam, int fwd, int back, int left, int right,
                     int up, int down, float dx, float dy);
void camera3d_view_matrix(mat4 out, const ACCamera3D *cam);

// ============================================================
// 3D Rendering
// ============================================================

// Render a form into the framebuffer with depth testing.
// ink_color is used when the form has no per-vertex colors.
void graph3d_render_form(ACFramebuffer *fb, ACDepthBuffer *db,
                         ACForm *form, const mat4 view, const mat4 proj,
                         ACColor ink_color, ACRenderStats *stats);

#endif
