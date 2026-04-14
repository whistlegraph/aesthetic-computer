/*
 * raster_test.c — pixel-exact self-test for the shared rasterizer.
 *
 * Build: gcc -O2 -Wall -o raster_test raster.c raster_test.c -lm
 * Run:   ./raster_test
 *
 * Each case renders a fixed triangle + compares a spot-check of pixels
 * against hand-computed expected values. When we later wire a WASM build,
 * the same test compiled via emcc should produce byte-identical output.
 *
 * Golden PNG snapshots (for richer diff-based drift detection) land in a
 * follow-up once the first integration is wired — this file focuses on
 * math correctness so a broken emcc build catches failures locally without
 * needing an image diff toolchain.
 */
#include "raster.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FB_W 64
#define FB_H 64

static int fail_count = 0;

static void check_pixel(ACRastTarget *t, int x, int y, uint32_t expected, const char *label) {
    uint32_t got = t->pixels[y * t->stride + x];
    if (got != expected) {
        fprintf(stderr, "  FAIL %s @ (%d,%d): got 0x%08x, expected 0x%08x\n",
                label, x, y, got, expected);
        fail_count++;
    }
}

static void check_nonzero(ACRastTarget *t, int x, int y, const char *label) {
    uint32_t got = t->pixels[y * t->stride + x];
    if (got == 0) {
        fprintf(stderr, "  FAIL %s @ (%d,%d): got 0x%08x, expected non-zero\n",
                label, x, y, got);
        fail_count++;
    }
}

static void check_zero(ACRastTarget *t, int x, int y, const char *label) {
    uint32_t got = t->pixels[y * t->stride + x];
    if (got != 0) {
        fprintf(stderr, "  FAIL %s @ (%d,%d): got 0x%08x, expected 0x00000000\n",
                label, x, y, got);
        fail_count++;
    }
}

/* ---- test 1: solid color triangle, no depth ---- */
static void test_solid_fill(void) {
    fprintf(stderr, "test_solid_fill\n");

    uint32_t pixels[FB_W * FB_H];
    ACRastTarget t = { .pixels = pixels, .depth = NULL,
                       .width = FB_W, .height = FB_H, .stride = FB_W };
    ac_rast_clear(&t, 0x00000000u, 0.0f);

    ACRastVertex v0 = { .sx = 10, .sy = 10, .z = 0.5f, .w = 1.0f };
    ACRastVertex v1 = { .sx = 50, .sy = 10, .z = 0.5f, .w = 1.0f };
    ACRastVertex v2 = { .sx = 30, .sy = 50, .z = 0.5f, .w = 1.0f };
    ACRastOptions opts = {
        .fill = AC_RAST_FILL_SOLID,
        .solid_color = AC_RAST_PACK(255, 255, 0, 0), /* opaque red */
        .no_fade = 1,
        .depth_mode = AC_RAST_DEPTH_NONE,
    };
    ac_rast_triangle(&t, &v0, &v1, &v2, &opts);

    /* Corner pixel (0,0) untouched. */
    check_zero(&t, 0, 0, "outside tri top-left");

    /* Centroid-ish point (30, 23) is inside. */
    check_pixel(&t, 30, 23, AC_RAST_PACK(255, 255, 0, 0), "centroid");

    /* Far-outside point (60, 60) untouched. */
    check_zero(&t, 60, 60, "outside tri bottom-right");
}

/* ---- test 2: per-vertex color interpolation ---- */
static void test_color_interp(void) {
    fprintf(stderr, "test_color_interp\n");

    uint32_t pixels[FB_W * FB_H];
    ACRastTarget t = { .pixels = pixels, .depth = NULL,
                       .width = FB_W, .height = FB_H, .stride = FB_W };
    ac_rast_clear(&t, 0x00000000u, 0.0f);

    ACRastVertex v0 = { .sx = 0,  .sy = 0,  .z = 0, .w = 1,
                        .r = 1, .g = 0, .b = 0, .a = 1 };   /* red */
    ACRastVertex v1 = { .sx = 63, .sy = 0,  .z = 0, .w = 1,
                        .r = 0, .g = 1, .b = 0, .a = 1 };   /* green */
    ACRastVertex v2 = { .sx = 32, .sy = 63, .z = 0, .w = 1,
                        .r = 0, .g = 0, .b = 1, .a = 1 };   /* blue */
    ACRastOptions opts = {
        .fill = AC_RAST_FILL_COLOR,
        .no_fade = 1,
        .depth_mode = AC_RAST_DEPTH_NONE,
    };
    ac_rast_triangle(&t, &v0, &v1, &v2, &opts);

    /* Near each vertex, the dominant channel should lead. We test inside
     * the triangle but close to each corner. */
    uint32_t near_red   = t.pixels[3 * FB_W + 3];
    uint32_t near_green = t.pixels[3 * FB_W + 60];
    uint32_t near_blue  = t.pixels[60 * FB_W + 32];

    if (!(AC_RAST_R(near_red) > AC_RAST_G(near_red) &&
          AC_RAST_R(near_red) > AC_RAST_B(near_red))) {
        fprintf(stderr, "  FAIL near_red is not reddest: 0x%08x\n", near_red);
        fail_count++;
    }
    if (!(AC_RAST_G(near_green) > AC_RAST_R(near_green) &&
          AC_RAST_G(near_green) > AC_RAST_B(near_green))) {
        fprintf(stderr, "  FAIL near_green is not greenest: 0x%08x\n", near_green);
        fail_count++;
    }
    if (!(AC_RAST_B(near_blue) > AC_RAST_R(near_blue) &&
          AC_RAST_B(near_blue) > AC_RAST_G(near_blue))) {
        fprintf(stderr, "  FAIL near_blue is not bluest: 0x%08x\n", near_blue);
        fail_count++;
    }
}

/* ---- test 3: depth test hides occluded triangle ---- */
static void test_depth_occlusion(void) {
    fprintf(stderr, "test_depth_occlusion\n");

    uint32_t pixels[FB_W * FB_H];
    float    depth [FB_W * FB_H];
    ACRastTarget t = { .pixels = pixels, .depth = depth,
                       .width = FB_W, .height = FB_H, .stride = FB_W };
    ac_rast_clear(&t, 0x00000000u, 1e9f);

    /* Blue triangle at z=0.8 (far), covers whole frame. */
    ACRastVertex bg0 = { .sx = 0,  .sy = 0,  .z = 0.8f, .w = 1.0f };
    ACRastVertex bg1 = { .sx = 63, .sy = 0,  .z = 0.8f, .w = 1.0f };
    ACRastVertex bg2 = { .sx = 32, .sy = 63, .z = 0.8f, .w = 1.0f };
    ACRastOptions bg_opts = {
        .fill = AC_RAST_FILL_SOLID,
        .solid_color = AC_RAST_PACK(255, 0, 0, 255), /* blue */
        .no_fade = 1,
        .depth_mode = AC_RAST_DEPTH_RW,
    };
    ac_rast_triangle(&t, &bg0, &bg1, &bg2, &bg_opts);

    /* Red triangle at z=0.2 (near), covers center region. */
    ACRastVertex fg0 = { .sx = 20, .sy = 20, .z = 0.2f, .w = 1.0f };
    ACRastVertex fg1 = { .sx = 44, .sy = 20, .z = 0.2f, .w = 1.0f };
    ACRastVertex fg2 = { .sx = 32, .sy = 44, .z = 0.2f, .w = 1.0f };
    ACRastOptions fg_opts = bg_opts;
    fg_opts.solid_color = AC_RAST_PACK(255, 255, 0, 0); /* red */
    ac_rast_triangle(&t, &fg0, &fg1, &fg2, &fg_opts);

    /* Center is red (foreground wins). */
    check_pixel(&t, 32, 28, AC_RAST_PACK(255, 255, 0, 0), "fg wins at center");

    /* Corner of bg is blue (fg didn't cover it). */
    check_pixel(&t, 5, 5, AC_RAST_PACK(255, 0, 0, 255), "bg at corner");

    /* Now draw blue AGAIN on top at z=0.9 (even farther) — should be hidden
     * everywhere by the z-buffer and leave pixels unchanged. */
    ACRastVertex late0 = { .sx = 0,  .sy = 0,  .z = 0.9f, .w = 1.0f };
    ACRastVertex late1 = { .sx = 63, .sy = 0,  .z = 0.9f, .w = 1.0f };
    ACRastVertex late2 = { .sx = 32, .sy = 63, .z = 0.9f, .w = 1.0f };
    ACRastOptions late_opts = bg_opts;
    late_opts.solid_color = AC_RAST_PACK(255, 200, 200, 200); /* gray */
    ac_rast_triangle(&t, &late0, &late1, &late2, &late_opts);

    /* Center still red, corner still blue — late_opts shouldn't overwrite. */
    check_pixel(&t, 32, 28, AC_RAST_PACK(255, 255, 0, 0), "depth blocks late-draw center");
    check_pixel(&t, 5, 5, AC_RAST_PACK(255, 0, 0, 255), "depth blocks late-draw corner");
}

/* ---- test 4: scissor rect clips output ---- */
static void test_scissor(void) {
    fprintf(stderr, "test_scissor\n");

    uint32_t pixels[FB_W * FB_H];
    ACRastTarget t = { .pixels = pixels, .depth = NULL,
                       .width = FB_W, .height = FB_H, .stride = FB_W };
    ac_rast_clear(&t, 0x00000000u, 0.0f);

    ACRastVertex v0 = { .sx = 0,  .sy = 0,  .z = 0, .w = 1 };
    ACRastVertex v1 = { .sx = 63, .sy = 0,  .z = 0, .w = 1 };
    ACRastVertex v2 = { .sx = 32, .sy = 63, .z = 0, .w = 1 };
    ACRastOptions opts = {
        .fill = AC_RAST_FILL_SOLID,
        .solid_color = AC_RAST_PACK(255, 255, 255, 255),
        .no_fade = 1,
        .depth_mode = AC_RAST_DEPTH_NONE,
        .scissor_x0 = 20, .scissor_y0 = 20,
        .scissor_x1 = 44, .scissor_y1 = 40,
    };
    ac_rast_triangle(&t, &v0, &v1, &v2, &opts);

    /* Inside scissor → written. */
    check_nonzero(&t, 30, 28, "inside scissor");
    /* Outside scissor but inside triangle → untouched. */
    check_zero(&t, 10, 10, "outside scissor top-left");
    check_zero(&t, 30, 50, "outside scissor bottom");
}

int main(void) {
    test_solid_fill();
    test_color_interp();
    test_depth_occlusion();
    test_scissor();

    if (fail_count) {
        fprintf(stderr, "FAILED: %d assertion(s)\n", fail_count);
        return 1;
    }
    fprintf(stderr, "OK: all tests passed\n");
    return 0;
}
