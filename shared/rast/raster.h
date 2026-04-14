/*
 * Aesthetic Computer — shared software rasterizer.
 *
 * One pure-C triangle rasterizer that ships two ways:
 *  1. Linked into ac-native (fedac/native/) so notepat / arena / etc. use
 *     the same pixel output on bare metal as they do in the browser.
 *  2. Compiled via Emscripten into .wasm for the web runtime, replacing
 *     the JS triangle path in system/public/aesthetic.computer/lib/graph.mjs
 *     for big perf gains (2-3× baseline, 20-40× with SIMD + worker-tiled).
 *
 * Rules for keeping parity portable:
 *   - No libc allocs inside the hot path. Caller owns framebuffer + depth.
 *   - No globals / mutable module state — every call is fully re-entrant
 *     so workers can rasterize independent tiles in parallel.
 *   - Pure C99. No POSIX, no SSE/AVX intrinsics here (those belong in
 *     emcc's simd128 path, gated by separate build flags).
 *   - Pixel format is uint32_t BGRA packed as (A<<24)|(R<<16)|(G<<8)|B,
 *     matching graph3d.c and graph.mjs's Uint32Array view.
 */
#ifndef AC_RAST_H
#define AC_RAST_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Pixel format helpers — keep in sync with fedac/native/src/graph3d.c. */
#define AC_RAST_PACK(a, r, g, b) \
    (((uint32_t)(a) << 24) | ((uint32_t)(r) << 16) | \
     ((uint32_t)(g) << 8)  | (uint32_t)(b))
#define AC_RAST_A(c) (((c) >> 24) & 0xFFu)
#define AC_RAST_R(c) (((c) >> 16) & 0xFFu)
#define AC_RAST_G(c) (((c) >>  8) & 0xFFu)
#define AC_RAST_B(c) ( (c)        & 0xFFu)

typedef struct {
    uint32_t *pixels;   /* row-major, AC_RAST_PACK-encoded */
    float    *depth;    /* row-major, same dims; NULL disables depth test */
    int       width, height;
    int       stride;   /* elements per row (usually == width) */
} ACRastTarget;

typedef struct {
    /* Post-perspective-divide screen-space position.
     *   x, y : pixel coordinates inside the target
     *   z    : depth (lower = closer, matches graph3d.c convention)
     *   w    : 1/w-free interpolation denominator — pass the original
     *          clip-space w from the vertex shader. If the caller isn't
     *          doing perspective-correct interpolation, pass 1.0f. */
    float sx, sy, z, w;

    /* Optional per-vertex attributes. Unused channels are ignored based
     * on the mode flag in ACRastOptions, so callers can leave them zero. */
    float r, g, b, a;   /* [0..1] linear RGBA */
    float u, v;         /* texture coords, wrapped */
} ACRastVertex;

typedef struct {
    const uint32_t *pixels;
    int width, height, stride;
} ACRastTexture;

typedef enum {
    AC_RAST_FILL_SOLID   = 0, /* single flat color from opts->solid_color */
    AC_RAST_FILL_COLOR   = 1, /* perspective-correct per-vertex RGBA */
    AC_RAST_FILL_TEXTURE = 2  /* sample from opts->texture with wrapped UV */
} ACRastFill;

typedef struct {
    ACRastFill fill;
    uint32_t   solid_color;    /* AC_RAST_PACK-encoded */
    const ACRastTexture *texture;

    /* Near-plane fade (matches graph3d.c no_fade semantics). When non-zero,
     * pixels close to w≈0 get darkened for a cheap cinematic feel. Set to
     * 1 to skip — required for UI layers and colored overlays. */
    int no_fade;

    /* Depth test mode.
     *   0 = read + write (standard z-buffer)
     *   1 = read-only    (transparent/overlay passes)
     *   2 = no test / no write (painter's-order fallback) */
    int depth_mode;

    /* Optional scissor rect (all zeros disables). Pixels outside are not
     * touched — useful for tile-parallel rasterization where each worker
     * owns a screen region. */
    int scissor_x0, scissor_y0, scissor_x1, scissor_y1;
} ACRastOptions;

#define AC_RAST_DEPTH_RW       0
#define AC_RAST_DEPTH_READONLY 1
#define AC_RAST_DEPTH_NONE     2

/* ---- rendering API ---- */

/* Clear color + depth buffer. `depth` is the sentinel (typically a large
 * float like 1e9 or FLT_MAX). Pass NULL target->depth to skip depth clear. */
void ac_rast_clear(ACRastTarget *target, uint32_t color, float depth);

/* Rasterize one triangle with the given attributes + options. Vertices
 * are already in screen space (no further projection done here). Callers
 * are responsible for near-plane clipping upstream — this function just
 * bounding-box-clips to target and scissor. */
void ac_rast_triangle(ACRastTarget *target,
                      const ACRastVertex *v0,
                      const ACRastVertex *v1,
                      const ACRastVertex *v2,
                      const ACRastOptions *opts);

#ifdef __cplusplus
}
#endif

#endif /* AC_RAST_H */
