#include "graph3d.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

// ============================================================
// Depth Buffer
// ============================================================

ACDepthBuffer *depth_create(int w, int h, int stride) {
    ACDepthBuffer *db = malloc(sizeof(ACDepthBuffer));
    if (!db) return NULL;
    db->width = w;
    db->height = h;
    db->stride = stride;
    db->data = malloc((size_t)stride * h * sizeof(float));
    if (!db->data) { free(db); return NULL; }
    depth_clear(db);
    return db;
}

void depth_clear(ACDepthBuffer *db) {
    if (!db || !db->data) return;
    size_t total = (size_t)db->stride * db->height;
    for (size_t i = 0; i < total; i++)
        db->data[i] = 1.0f; // far plane
}

void depth_destroy(ACDepthBuffer *db) {
    if (!db) return;
    free(db->data);
    free(db);
}

// ============================================================
// Matrix Math (column-major)
// m[col*4 + row]
// ============================================================

void mat4_identity(mat4 out) {
    memset(out, 0, 16 * sizeof(float));
    out[0] = out[5] = out[10] = out[15] = 1.0f;
}

void mat4_copy(mat4 out, const mat4 m) {
    memcpy(out, m, 16 * sizeof(float));
}

void mat4_multiply(mat4 out, const mat4 a, const mat4 b) {
    mat4 tmp;
    for (int col = 0; col < 4; col++) {
        for (int row = 0; row < 4; row++) {
            float sum = 0;
            for (int k = 0; k < 4; k++)
                sum += a[k * 4 + row] * b[col * 4 + k];
            tmp[col * 4 + row] = sum;
        }
    }
    memcpy(out, tmp, 16 * sizeof(float));
}

void mat4_perspective(mat4 out, float fov_rad, float aspect, float near, float far) {
    memset(out, 0, 16 * sizeof(float));
    float f = 1.0f / tanf(fov_rad * 0.5f);
    float nf = 1.0f / (near - far);
    out[0]  = f / aspect;
    out[5]  = f;
    out[10] = (far + near) * nf;
    out[11] = -1.0f;
    out[14] = 2.0f * far * near * nf;
}

void mat4_translate(mat4 out, const mat4 m, float x, float y, float z) {
    mat4_copy(out, m);
    // out[12..15] += m * [x,y,z,0]
    out[12] += m[0] * x + m[4] * y + m[8]  * z;
    out[13] += m[1] * x + m[5] * y + m[9]  * z;
    out[14] += m[2] * x + m[6] * y + m[10] * z;
    out[15] += m[3] * x + m[7] * y + m[11] * z;
}

void mat4_rotateX(mat4 out, const mat4 m, float rad) {
    float s = sinf(rad), c = cosf(rad);
    // Multiply m by rotation matrix (affects columns 1 and 2)
    mat4 tmp;
    mat4_copy(tmp, m);
    for (int i = 0; i < 4; i++) {
        out[4 + i]  = tmp[4 + i] * c + tmp[8 + i] * s;
        out[8 + i]  = tmp[8 + i] * c - tmp[4 + i] * s;
    }
    // Copy unchanged columns
    for (int i = 0; i < 4; i++) { out[i] = tmp[i]; out[12 + i] = tmp[12 + i]; }
}

void mat4_rotateY(mat4 out, const mat4 m, float rad) {
    float s = sinf(rad), c = cosf(rad);
    mat4 tmp;
    mat4_copy(tmp, m);
    for (int i = 0; i < 4; i++) {
        out[i]      = tmp[i] * c - tmp[8 + i] * s;
        out[8 + i]  = tmp[i] * s + tmp[8 + i] * c;
    }
    for (int i = 0; i < 4; i++) { out[4 + i] = tmp[4 + i]; out[12 + i] = tmp[12 + i]; }
}

void mat4_rotateZ(mat4 out, const mat4 m, float rad) {
    float s = sinf(rad), c = cosf(rad);
    mat4 tmp;
    mat4_copy(tmp, m);
    for (int i = 0; i < 4; i++) {
        out[i]      = tmp[i] * c + tmp[4 + i] * s;
        out[4 + i]  = tmp[4 + i] * c - tmp[i] * s;
    }
    for (int i = 0; i < 4; i++) { out[8 + i] = tmp[8 + i]; out[12 + i] = tmp[12 + i]; }
}

void mat4_scale_uniform(mat4 out, const mat4 m, float s) {
    mat4_copy(out, m);
    for (int i = 0; i < 4; i++) {
        out[i]     *= s;
        out[4 + i] *= s;
        out[8 + i] *= s;
    }
}

void mat4_transform_vec4(vec4 out, const mat4 m, const vec4 v) {
    out[0] = m[0]*v[0] + m[4]*v[1] + m[8]*v[2]  + m[12]*v[3];
    out[1] = m[1]*v[0] + m[5]*v[1] + m[9]*v[2]  + m[13]*v[3];
    out[2] = m[2]*v[0] + m[6]*v[1] + m[10]*v[2] + m[14]*v[3];
    out[3] = m[3]*v[0] + m[7]*v[1] + m[11]*v[2] + m[15]*v[3];
}

// ============================================================
// Camera
// ============================================================

void camera3d_init(ACCamera3D *cam) {
    cam->x = cam->y = cam->z = 0.0f;
    cam->rotX = cam->rotY = cam->rotZ = 0.0f;
    cam->speed = 0.08f;
    cam->sensitivity = 0.15f;
}

void camera3d_update(ACCamera3D *cam, int fwd, int back, int left, int right,
                     int up, int down, float dx, float dy) {
    // Mouse look
    cam->rotY += dx * cam->sensitivity;
    cam->rotX += dy * cam->sensitivity;
    // Clamp pitch
    if (cam->rotX > 89.0f) cam->rotX = 89.0f;
    if (cam->rotX < -89.0f) cam->rotX = -89.0f;

    // Movement relative to yaw
    float yaw_rad = cam->rotY * (float)M_PI / 180.0f;
    float fwd_x = sinf(yaw_rad);
    float fwd_z = -cosf(yaw_rad);
    float right_x = cosf(yaw_rad);
    float right_z = sinf(yaw_rad);

    float move_fwd = (float)(fwd - back);
    float move_right = (float)(right - left);
    float move_up = (float)(up - down);

    cam->x += (fwd_x * move_fwd + right_x * move_right) * cam->speed;
    cam->z += (fwd_z * move_fwd + right_z * move_right) * cam->speed;
    cam->y += move_up * cam->speed;
}

void camera3d_view_matrix(mat4 out, const ACCamera3D *cam) {
    float pitch = -cam->rotX * (float)M_PI / 180.0f;
    float yaw   = -cam->rotY * (float)M_PI / 180.0f;

    mat4 t;
    mat4_identity(t);
    mat4_translate(t, t, -cam->x, -cam->y, -cam->z);

    mat4 ry;
    mat4_identity(ry);
    mat4_rotateY(ry, ry, yaw);

    mat4 rx;
    mat4_identity(rx);
    mat4_rotateX(rx, rx, pitch);

    // V = Rx * Ry * T
    mat4 tmp;
    mat4_multiply(tmp, ry, t);
    mat4_multiply(out, rx, tmp);
}

// ============================================================
// Frustum Clipping (Sutherland-Hodgman in clip space)
// ============================================================

// Max vertices after clipping a single triangle against 6 planes
#define MAX_CLIP_VERTS 24

// Lerp between two clip vertices at parameter t
static void clip_lerp(ClipVertex *out, const ClipVertex *a, const ClipVertex *b, float t) {
    for (int i = 0; i < 4; i++) {
        out->pos[i]   = a->pos[i]   + t * (b->pos[i]   - a->pos[i]);
        out->color[i] = a->color[i] + t * (b->color[i] - a->color[i]);
    }
    out->uv[0] = a->uv[0] + t * (b->uv[0] - a->uv[0]);
    out->uv[1] = a->uv[1] + t * (b->uv[1] - a->uv[1]);
}

// Clip plane distance functions (positive = inside)
// Plane 0: w + x >= 0 (left)
// Plane 1: w - x >= 0 (right)
// Plane 2: w + y >= 0 (bottom)
// Plane 3: w - y >= 0 (top)
// Plane 4: w + z >= 0 (near)
// Plane 5: w - z >= 0 (far)
static float clip_dist(const ClipVertex *v, int plane) {
    switch (plane) {
        case 0: return v->pos[3] + v->pos[0]; // left
        case 1: return v->pos[3] - v->pos[0]; // right
        case 2: return v->pos[3] + v->pos[1]; // bottom
        case 3: return v->pos[3] - v->pos[1]; // top
        case 4: return v->pos[3] + v->pos[2]; // near
        case 5: return v->pos[3] - v->pos[2]; // far
        default: return 0;
    }
}

// Clip polygon against one plane. Returns new vertex count.
static int clip_against_plane(ClipVertex *out, const ClipVertex *in, int count, int plane) {
    if (count < 1) return 0;
    int out_count = 0;
    for (int i = 0; i < count; i++) {
        const ClipVertex *cur = &in[i];
        const ClipVertex *prev = &in[(i + count - 1) % count];
        float d_cur = clip_dist(cur, plane);
        float d_prev = clip_dist(prev, plane);
        if (d_prev >= 0) {
            // Previous inside
            if (d_cur >= 0) {
                // Both inside — emit current
                out[out_count++] = *cur;
            } else {
                // Exiting — emit intersection
                float t = d_prev / (d_prev - d_cur);
                clip_lerp(&out[out_count++], prev, cur, t);
            }
        } else {
            // Previous outside
            if (d_cur >= 0) {
                // Entering — emit intersection then current
                float t = d_prev / (d_prev - d_cur);
                clip_lerp(&out[out_count++], prev, cur, t);
                out[out_count++] = *cur;
            }
            // Both outside — emit nothing
        }
    }
    return out_count;
}

// Clip polygon against all 6 frustum planes. Returns clipped vertex count.
static int clip_polygon(ClipVertex *verts, int count) {
    ClipVertex buf_a[MAX_CLIP_VERTS], buf_b[MAX_CLIP_VERTS];
    memcpy(buf_a, verts, count * sizeof(ClipVertex));

    ClipVertex *src = buf_a, *dst = buf_b;
    for (int plane = 0; plane < 6; plane++) {
        count = clip_against_plane(dst, src, count, plane);
        if (count == 0) return 0;
        // Swap buffers
        ClipVertex *tmp = src; src = dst; dst = tmp;
    }
    // Result is in src
    if (src != verts)
        memcpy(verts, src, count * sizeof(ClipVertex));
    return count;
}

// ============================================================
// Perspective Divide + Viewport Transform
// ============================================================

static void to_screen(ScreenVertex *sv, const ClipVertex *cv, int fb_w, int fb_h) {
    float inv_w = 1.0f / cv->pos[3];
    float ndc_x = cv->pos[0] * inv_w;
    float ndc_y = cv->pos[1] * inv_w;
    float ndc_z = cv->pos[2] * inv_w;

    sv->sx = (ndc_x * 0.5f + 0.5f) * fb_w;
    sv->sy = (1.0f - (ndc_y * 0.5f + 0.5f)) * fb_h; // Y flipped
    sv->z  = ndc_z;
    sv->w  = cv->pos[3];
    memcpy(sv->color, cv->color, 4 * sizeof(float));
    memcpy(sv->uv, cv->uv, 2 * sizeof(float));
}

// ============================================================
// Triangle Rasterization
// ============================================================

static inline int mini(int a, int b) { return a < b ? a : b; }
static inline int maxi(int a, int b) { return a > b ? a : b; }
static inline float minf(float a, float b) { return a < b ? a : b; }
static inline float maxf(float a, float b) { return a > b ? a : b; }
static inline float clampf(float v, float lo, float hi) { return maxf(lo, minf(hi, v)); }

// Sample texture with wrapping
static uint32_t sample_texture(const ACFramebuffer *tex, float u, float v) {
    // Wrap UVs
    u = u - floorf(u);
    v = v - floorf(v);
    int tx = (int)(u * tex->width) % tex->width;
    int ty = (int)(v * tex->height) % tex->height;
    if (tx < 0) tx += tex->width;
    if (ty < 0) ty += tex->height;
    return tex->pixels[ty * tex->stride + tx];
}

static void rasterize_triangle(ACFramebuffer *fb, ACDepthBuffer *db,
                                const ScreenVertex *v0, const ScreenVertex *v1,
                                const ScreenVertex *v2,
                                ACColor ink, int has_colors,
                                const ACFramebuffer *texture,
                                int no_fade, ACRenderStats *stats) {
    // Bounding box
    int min_x = maxi(0, (int)floorf(minf(v0->sx, minf(v1->sx, v2->sx))));
    int max_x = mini(fb->width - 1, (int)ceilf(maxf(v0->sx, maxf(v1->sx, v2->sx))));
    int min_y = maxi(0, (int)floorf(minf(v0->sy, minf(v1->sy, v2->sy))));
    int max_y = mini(fb->height - 1, (int)ceilf(maxf(v0->sy, maxf(v1->sy, v2->sy))));

    if (min_x > max_x || min_y > max_y) return;

    // Edge function setup
    float dx01 = v1->sx - v0->sx, dy01 = v1->sy - v0->sy;
    float dx12 = v2->sx - v1->sx, dy12 = v2->sy - v1->sy;
    float dx20 = v0->sx - v2->sx, dy20 = v0->sy - v2->sy;

    float area = dx01 * (v2->sy - v0->sy) - dy01 * (v2->sx - v0->sx);
    if (fabsf(area) < 0.001f) return; // degenerate
    float inv_area = 1.0f / area;

    // Pre-compute 1/w for perspective-correct interpolation
    float inv_w0 = 1.0f / v0->w;
    float inv_w1 = 1.0f / v1->w;
    float inv_w2 = 1.0f / v2->w;

    for (int y = min_y; y <= max_y; y++) {
        for (int x = min_x; x <= max_x; x++) {
            float px = x + 0.5f, py = y + 0.5f;

            // Barycentric coordinates
            float b0 = (dx12 * (py - v1->sy) - dy12 * (px - v1->sx)) * inv_area;
            float b1 = (dx20 * (py - v2->sy) - dy20 * (px - v2->sx)) * inv_area;
            float b2 = 1.0f - b0 - b1;

            if (b0 < 0 || b1 < 0 || b2 < 0) continue;

            // Perspective-correct interpolation weight
            float inv_w = b0 * inv_w0 + b1 * inv_w1 + b2 * inv_w2;
            float w_interp = 1.0f / inv_w;

            // Depth
            float z = (b0 * v0->z * inv_w0 + b1 * v1->z * inv_w1 + b2 * v2->z * inv_w2) * w_interp;

            int idx = y * db->stride + x;
            if (z >= db->data[idx]) continue;
            db->data[idx] = z;

            uint32_t pixel;

            if (texture) {
                // Perspective-correct UV
                float u = (b0 * v0->uv[0] * inv_w0 + b1 * v1->uv[0] * inv_w1 + b2 * v2->uv[0] * inv_w2) * w_interp;
                float v_coord = (b0 * v0->uv[1] * inv_w0 + b1 * v1->uv[1] * inv_w1 + b2 * v2->uv[1] * inv_w2) * w_interp;
                pixel = sample_texture(texture, u, v_coord);
            } else if (has_colors) {
                // Perspective-correct color interpolation
                float r = (b0 * v0->color[0] * inv_w0 + b1 * v1->color[0] * inv_w1 + b2 * v2->color[0] * inv_w2) * w_interp;
                float g = (b0 * v0->color[1] * inv_w0 + b1 * v1->color[1] * inv_w1 + b2 * v2->color[1] * inv_w2) * w_interp;
                float b_c = (b0 * v0->color[2] * inv_w0 + b1 * v1->color[2] * inv_w1 + b2 * v2->color[2] * inv_w2) * w_interp;
                uint8_t ri = (uint8_t)(clampf(r, 0, 1) * 255);
                uint8_t gi = (uint8_t)(clampf(g, 0, 1) * 255);
                uint8_t bi = (uint8_t)(clampf(b_c, 0, 1) * 255);
                pixel = (255u << 24) | ((uint32_t)ri << 16) | ((uint32_t)gi << 8) | bi;
            } else {
                pixel = color_pack(ink);
            }

            // Near-plane fade (darken objects very close to camera)
            if (!no_fade && v0->w < 1.0f) {
                float fade = clampf(v0->w, 0.0f, 1.0f);
                uint8_t pr = (pixel >> 16) & 0xFF;
                uint8_t pg = (pixel >> 8) & 0xFF;
                uint8_t pb = pixel & 0xFF;
                pr = (uint8_t)(pr * fade);
                pg = (uint8_t)(pg * fade);
                pb = (uint8_t)(pb * fade);
                pixel = (255u << 24) | ((uint32_t)pr << 16) | ((uint32_t)pg << 8) | pb;
            }

            fb->pixels[y * fb->stride + x] = pixel;
            if (stats) stats->pixelsDrawn++;
        }
    }
}

// ============================================================
// 3D Line Rendering (clip space → screen)
// ============================================================

// Cohen-Sutherland-style clip for a single line segment in clip space.
// Returns 1 if visible, 0 if fully clipped.
static int clip_line_segment(ClipVertex *a, ClipVertex *b) {
    for (int plane = 0; plane < 6; plane++) {
        float da = clip_dist(a, plane);
        float db_val = clip_dist(b, plane);
        if (da < 0 && db_val < 0) return 0; // both outside
        if (da < 0) {
            float t = da / (da - db_val);
            clip_lerp(a, a, b, t);
        } else if (db_val < 0) {
            float t = da / (da - db_val);
            clip_lerp(b, a, b, t);
        }
    }
    return 1;
}

static void draw_line_3d(ACFramebuffer *fb, ACDepthBuffer *db,
                         ClipVertex *a, ClipVertex *b,
                         ACColor ink, int has_colors, ACRenderStats *stats) {
    if (!clip_line_segment(a, b)) return;

    ScreenVertex sa, sb;
    to_screen(&sa, a, fb->width, fb->height);
    to_screen(&sb, b, fb->width, fb->height);

    // Bresenham with color interpolation
    int x0 = (int)sa.sx, y0 = (int)sa.sy;
    int x1 = (int)sb.sx, y1 = (int)sb.sy;
    int dx_abs = abs(x1 - x0), dy_abs = -abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1;
    int err = dx_abs + dy_abs;
    int steps = maxi(abs(x1 - x0), abs(y1 - y0));
    if (steps == 0) steps = 1;
    int step = 0;

    for (;;) {
        if (x0 >= 0 && x0 < fb->width && y0 >= 0 && y0 < fb->height) {
            float t = (float)step / steps;
            float z = sa.z + t * (sb.z - sa.z);
            int idx = y0 * db->stride + x0;
            if (z < db->data[idx]) {
                db->data[idx] = z;
                uint32_t pixel;
                if (has_colors) {
                    float r = sa.color[0] + t * (sb.color[0] - sa.color[0]);
                    float g = sa.color[1] + t * (sb.color[1] - sa.color[1]);
                    float bc = sa.color[2] + t * (sb.color[2] - sa.color[2]);
                    pixel = (255u << 24) |
                            ((uint32_t)(uint8_t)(clampf(r, 0, 1) * 255) << 16) |
                            ((uint32_t)(uint8_t)(clampf(g, 0, 1) * 255) << 8) |
                            (uint32_t)(uint8_t)(clampf(bc, 0, 1) * 255);
                } else {
                    pixel = color_pack(ink);
                }
                fb->pixels[y0 * fb->stride + x0] = pixel;
            }
        }
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 >= dy_abs) { err += dy_abs; x0 += sx; }
        if (e2 <= dx_abs) { err += dx_abs; y0 += sy; }
        step++;
    }
    if (stats) stats->wireframeSegmentsTotal++;
}

// ============================================================
// Build Model Matrix
// ============================================================

static void build_model_matrix(mat4 out, const ACForm *form) {
    mat4_identity(out);
    mat4_translate(out, out, form->position[0], form->position[1], form->position[2]);
    float deg2rad = (float)M_PI / 180.0f;
    mat4_rotateY(out, out, form->rotation[1] * deg2rad);
    mat4_rotateX(out, out, form->rotation[0] * deg2rad);
    mat4_rotateZ(out, out, form->rotation[2] * deg2rad);
    if (fabsf(form->scale - 1.0f) > 1e-6f)
        mat4_scale_uniform(out, out, form->scale);
}

// ============================================================
// Render Form
// ============================================================

void graph3d_render_form(ACFramebuffer *fb, ACDepthBuffer *db,
                         ACForm *form, const mat4 view, const mat4 proj,
                         ACColor ink_color, ACRenderStats *stats) {
    if (!form || form->vert_count < 2) return;

    // Build MVP matrix
    mat4 model, mv, mvp;
    build_model_matrix(model, form);
    mat4_multiply(mv, view, model);
    mat4_multiply(mvp, proj, mv);

    if (form->type == FORM_TYPE_LINE) {
        // Render lines (2 verts per line)
        for (int i = 0; i + 1 < form->vert_count; i += 2) {
            ClipVertex a, b;
            // Transform vertex A
            vec4 va = { form->positions[i*4], form->positions[i*4+1],
                        form->positions[i*4+2], form->positions[i*4+3] };
            mat4_transform_vec4(a.pos, mvp, va);
            if (form->has_colors && form->colors) {
                memcpy(a.color, &form->colors[i*4], 4 * sizeof(float));
            } else {
                a.color[0] = ink_color.r / 255.0f;
                a.color[1] = ink_color.g / 255.0f;
                a.color[2] = ink_color.b / 255.0f;
                a.color[3] = 1.0f;
            }
            a.uv[0] = a.uv[1] = 0;

            // Transform vertex B
            int j = i + 1;
            vec4 vb = { form->positions[j*4], form->positions[j*4+1],
                        form->positions[j*4+2], form->positions[j*4+3] };
            mat4_transform_vec4(b.pos, mvp, vb);
            if (form->has_colors && form->colors) {
                memcpy(b.color, &form->colors[j*4], 4 * sizeof(float));
            } else {
                b.color[0] = ink_color.r / 255.0f;
                b.color[1] = ink_color.g / 255.0f;
                b.color[2] = ink_color.b / 255.0f;
                b.color[3] = 1.0f;
            }
            b.uv[0] = b.uv[1] = 0;

            draw_line_3d(fb, db, &a, &b, ink_color, form->has_colors, stats);
        }
    } else {
        // Render triangles (3 verts per triangle)
        int tri_count = form->vert_count / 3;
        if (stats) stats->originalTriangles += tri_count;

        for (int t = 0; t < tri_count; t++) {
            ClipVertex clip_verts[MAX_CLIP_VERTS];
            int base = t * 3;

            // Transform 3 vertices to clip space
            for (int vi = 0; vi < 3; vi++) {
                int idx = base + vi;
                vec4 v_in = { form->positions[idx*4], form->positions[idx*4+1],
                              form->positions[idx*4+2], form->positions[idx*4+3] };
                mat4_transform_vec4(clip_verts[vi].pos, mvp, v_in);

                if (form->has_colors && form->colors) {
                    memcpy(clip_verts[vi].color, &form->colors[idx*4], 4 * sizeof(float));
                } else {
                    clip_verts[vi].color[0] = ink_color.r / 255.0f;
                    clip_verts[vi].color[1] = ink_color.g / 255.0f;
                    clip_verts[vi].color[2] = ink_color.b / 255.0f;
                    clip_verts[vi].color[3] = 1.0f;
                }

                if (form->tex_coords) {
                    clip_verts[vi].uv[0] = form->tex_coords[idx*2];
                    clip_verts[vi].uv[1] = form->tex_coords[idx*2+1];
                } else {
                    clip_verts[vi].uv[0] = clip_verts[vi].uv[1] = 0;
                }
            }

            // Frustum clip
            int clipped_count = clip_polygon(clip_verts, 3);
            if (clipped_count < 3) {
                if (stats) stats->trianglesRejected++;
                continue;
            }
            if (stats) stats->clippedTriangles += (clipped_count - 2);

            // Convert clipped polygon to screen-space and fan-triangulate
            ScreenVertex screen_verts[MAX_CLIP_VERTS];
            for (int vi = 0; vi < clipped_count; vi++)
                to_screen(&screen_verts[vi], &clip_verts[vi], fb->width, fb->height);

            // Fan triangulate: (0, 1, 2), (0, 2, 3), (0, 3, 4), ...
            for (int vi = 1; vi + 1 < clipped_count; vi++) {
                rasterize_triangle(fb, db,
                    &screen_verts[0], &screen_verts[vi], &screen_verts[vi + 1],
                    ink_color, form->has_colors,
                    form->texture, form->no_fade, stats);
            }
        }
    }
}
