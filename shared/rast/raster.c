/*
 * raster.c — cross-platform triangle rasterizer
 *
 * Port of fedac/native/src/graph3d.c's rasterize_triangle + web/graph.mjs's
 * equivalent triangle path, unified as one implementation. See raster.h for
 * the API contract + platform rationale.
 *
 * Pixel output is intended to be exact-match between native and WASM builds
 * of this file — the test harness in raster_test.c checks this by rendering
 * a fixed triangle and diffing against a golden PNG.
 */
#include "raster.h"
#include <math.h>

static inline int   mini(int a, int b)     { return a < b ? a : b; }
static inline int   maxi(int a, int b)     { return a > b ? a : b; }
static inline float minf(float a, float b) { return a < b ? a : b; }
static inline float maxf(float a, float b) { return a > b ? a : b; }
static inline float clampf(float v, float lo, float hi) {
    return maxf(lo, minf(hi, v));
}

static uint32_t sample_texture(const ACRastTexture *tex, float u, float v) {
    /* GL_REPEAT — modulo, wrap negative. */
    u = u - floorf(u);
    v = v - floorf(v);
    int tx = (int)(u * tex->width);
    int ty = (int)(v * tex->height);
    if (tx < 0) tx += tex->width;
    if (ty < 0) ty += tex->height;
    if (tx >= tex->width)  tx = tex->width  - 1;
    if (ty >= tex->height) ty = tex->height - 1;
    return tex->pixels[ty * tex->stride + tx];
}

void ac_rast_clear(ACRastTarget *t, uint32_t color, float depth) {
    if (!t) return;
    int n = t->height * t->stride;
    for (int i = 0; i < n; i++) t->pixels[i] = color;
    if (t->depth) {
        for (int i = 0; i < n; i++) t->depth[i] = depth;
    }
}

void ac_rast_triangle(ACRastTarget *t,
                      const ACRastVertex *v0,
                      const ACRastVertex *v1,
                      const ACRastVertex *v2,
                      const ACRastOptions *opts) {
    if (!t || !t->pixels || !v0 || !v1 || !v2 || !opts) return;

    /* Bounding box, clipped to target + scissor. */
    int min_x = (int)floorf(minf(v0->sx, minf(v1->sx, v2->sx)));
    int max_x = (int)ceilf (maxf(v0->sx, maxf(v1->sx, v2->sx)));
    int min_y = (int)floorf(minf(v0->sy, minf(v1->sy, v2->sy)));
    int max_y = (int)ceilf (maxf(v0->sy, maxf(v1->sy, v2->sy)));

    int tx0 = 0, ty0 = 0, tx1 = t->width - 1, ty1 = t->height - 1;
    if (opts->scissor_x1 > opts->scissor_x0 && opts->scissor_y1 > opts->scissor_y0) {
        tx0 = maxi(tx0, opts->scissor_x0);
        ty0 = maxi(ty0, opts->scissor_y0);
        tx1 = mini(tx1, opts->scissor_x1);
        ty1 = mini(ty1, opts->scissor_y1);
    }
    min_x = maxi(min_x, tx0);
    min_y = maxi(min_y, ty0);
    max_x = mini(max_x, tx1);
    max_y = mini(max_y, ty1);
    if (min_x > max_x || min_y > max_y) return;

    /* Edge function setup (matches graph3d.c). */
    float dx01 = v1->sx - v0->sx, dy01 = v1->sy - v0->sy;
    float dx12 = v2->sx - v1->sx, dy12 = v2->sy - v1->sy;
    float dx20 = v0->sx - v2->sx, dy20 = v0->sy - v2->sy;

    float area = dx01 * (v2->sy - v0->sy) - dy01 * (v2->sx - v0->sx);
    if (fabsf(area) < 0.001f) return; /* degenerate */
    float inv_area = 1.0f / area;

    /* 1/w for perspective-correct interpolation. If caller passed w==1
     * for every vertex (affine mode), inv_w == 1 throughout and the per-
     * pixel perspective divide collapses to a no-op. */
    float inv_w0 = 1.0f / (v0->w != 0.0f ? v0->w : 1.0f);
    float inv_w1 = 1.0f / (v1->w != 0.0f ? v1->w : 1.0f);
    float inv_w2 = 1.0f / (v2->w != 0.0f ? v2->w : 1.0f);

    const ACRastTexture *tex = (opts->fill == AC_RAST_FILL_TEXTURE) ? opts->texture : NULL;
    int has_depth = (t->depth && opts->depth_mode != AC_RAST_DEPTH_NONE);
    int depth_write = has_depth && opts->depth_mode == AC_RAST_DEPTH_RW;

    for (int y = min_y; y <= max_y; y++) {
        int row = y * t->stride;
        for (int x = min_x; x <= max_x; x++) {
            float px = x + 0.5f, py = y + 0.5f;

            /* Barycentric coordinates (edge function). */
            float b0 = (dx12 * (py - v1->sy) - dy12 * (px - v1->sx)) * inv_area;
            float b1 = (dx20 * (py - v2->sy) - dy20 * (px - v2->sx)) * inv_area;
            float b2 = 1.0f - b0 - b1;
            if (b0 < 0.0f || b1 < 0.0f || b2 < 0.0f) continue;

            /* Perspective-correct weight. */
            float inv_w = b0 * inv_w0 + b1 * inv_w1 + b2 * inv_w2;
            float w_interp = 1.0f / inv_w;

            /* Depth interpolation + test. */
            int idx = row + x;
            if (has_depth) {
                float z = (b0 * v0->z * inv_w0 +
                           b1 * v1->z * inv_w1 +
                           b2 * v2->z * inv_w2) * w_interp;
                if (z >= t->depth[idx]) continue;
                if (depth_write) t->depth[idx] = z;
            }

            uint32_t pixel;
            switch (opts->fill) {
                case AC_RAST_FILL_TEXTURE: {
                    float u = (b0 * v0->u * inv_w0 +
                               b1 * v1->u * inv_w1 +
                               b2 * v2->u * inv_w2) * w_interp;
                    float v = (b0 * v0->v * inv_w0 +
                               b1 * v1->v * inv_w1 +
                               b2 * v2->v * inv_w2) * w_interp;
                    pixel = sample_texture(tex, u, v);
                } break;
                case AC_RAST_FILL_COLOR: {
                    float r = (b0 * v0->r * inv_w0 +
                               b1 * v1->r * inv_w1 +
                               b2 * v2->r * inv_w2) * w_interp;
                    float g = (b0 * v0->g * inv_w0 +
                               b1 * v1->g * inv_w1 +
                               b2 * v2->g * inv_w2) * w_interp;
                    float bl = (b0 * v0->b * inv_w0 +
                                b1 * v1->b * inv_w1 +
                                b2 * v2->b * inv_w2) * w_interp;
                    float al = (b0 * v0->a * inv_w0 +
                                b1 * v1->a * inv_w1 +
                                b2 * v2->a * inv_w2) * w_interp;
                    uint8_t ri = (uint8_t)(clampf(r,  0.0f, 1.0f) * 255.0f);
                    uint8_t gi = (uint8_t)(clampf(g,  0.0f, 1.0f) * 255.0f);
                    uint8_t bi = (uint8_t)(clampf(bl, 0.0f, 1.0f) * 255.0f);
                    uint8_t ai = (uint8_t)(clampf(al, 0.0f, 1.0f) * 255.0f);
                    pixel = AC_RAST_PACK(ai ? ai : 255, ri, gi, bi);
                } break;
                case AC_RAST_FILL_SOLID:
                default:
                    pixel = opts->solid_color;
                    break;
            }

            /* Near-plane fade — mirror graph3d.c when !no_fade. The ramp
             * is intentionally gentle so UI-layer triangles (no_fade=1)
             * look identical to 3D content far from the camera. */
            if (!opts->no_fade) {
                /* w_interp in camera space — larger = farther. The fade
                 * tapers in the first 0.5 units, matching graph3d.c. */
                float fade = clampf(w_interp * 2.0f, 0.0f, 1.0f);
                if (fade < 1.0f) {
                    uint8_t a = AC_RAST_A(pixel);
                    uint8_t r = (uint8_t)(AC_RAST_R(pixel) * fade);
                    uint8_t g = (uint8_t)(AC_RAST_G(pixel) * fade);
                    uint8_t b = (uint8_t)(AC_RAST_B(pixel) * fade);
                    pixel = AC_RAST_PACK(a, r, g, b);
                }
            }

            t->pixels[idx] = pixel;
        }
    }
}
