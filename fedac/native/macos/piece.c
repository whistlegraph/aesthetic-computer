// piece.c — QuickJS host + minimal AC bindings for hello.mjs.
// Keeps scope tight: enough API to run hello.mjs end-to-end. The heavier
// set (graph.c primitives, font.c text, audio, net) comes in later stages.

#include "piece.h"
#include "quickjs.h"
#include "audio.h"
// Vendored from src/ so we don't pull the full graph.h/drm-display.h chain yet.
#include "font-6x10.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

struct PieceCtx {
    JSRuntime *rt;
    JSContext *jsctx;
    PieceFB *fb;

    // Current ink color (ARGB). hello.mjs leans on ink() both as a setter
    // and as a chain root (ink(...).box(...)), so we keep a persistent slot.
    uint32_t ink_argb;

    // Cached lifecycle functions (may be JS_UNDEFINED if absent).
    JSValue boot_fn, paint_fn, sim_fn, act_fn;

    // Pre-built global arg object passed into paint/act: shares the
    // binding functions so destructuring `{ wipe, ink, ... }` resolves.
    JSValue api;

    // Current event (for act()). Rebuilt each call.
    const PieceEvent *current_event;

    // Library root for resolving ES module imports like "/lib/percussion.mjs".
    // Set from AC_LIB_PATH env var at load time.
    char lib_path[1024];

    // SDL3 audio engine — NULL if init failed (piece still runs, silent).
    Audio *audio;
};

// ── Pixel helpers ───────────────────────────────────────────────────────────

static inline void put_pixel(PieceFB *fb, int x, int y, uint32_t c) {
    if (x < 0 || x >= fb->width || y < 0 || y >= fb->height) return;
    fb->pixels[y * fb->stride + x] = c;
}

static void fill_rect(PieceFB *fb, int x, int y, int w, int h, uint32_t c) {
    int x0 = x < 0 ? 0 : x;
    int y0 = y < 0 ? 0 : y;
    int x1 = x + w > fb->width ? fb->width : x + w;
    int y1 = y + h > fb->height ? fb->height : y + h;
    for (int yy = y0; yy < y1; yy++) {
        uint32_t *row = fb->pixels + yy * fb->stride;
        for (int xx = x0; xx < x1; xx++) row[xx] = c;
    }
}

static void stroke_rect(PieceFB *fb, int x, int y, int w, int h, uint32_t c) {
    for (int i = 0; i < w; i++) { put_pixel(fb, x + i, y, c); put_pixel(fb, x + i, y + h - 1, c); }
    for (int i = 0; i < h; i++) { put_pixel(fb, x, y + i, c); put_pixel(fb, x + w - 1, y + i, c); }
}

static void draw_line(PieceFB *fb, int x0, int y0, int x1, int y1, uint32_t c) {
    // Bresenham.
    int dx = abs(x1 - x0), dy = -abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1;
    int err = dx + dy;
    while (1) {
        put_pixel(fb, x0, y0, c);
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 >= dy) { err += dy; x0 += sx; }
        if (e2 <= dx) { err += dx; y0 += sy; }
    }
}

// Render a single 6x10 glyph at (x,y) scaled by `scale`. Bit 7 is the
// leftmost column within each row byte (see font-6x10.h).
static void draw_glyph_6x10(PieceFB *fb, int ch, int x, int y, int scale, uint32_t c) {
    if (ch < 32 || ch > 126) ch = '?';
    const uint8_t *glyph = font_6x10_data[ch - 32];
    for (int row = 0; row < FONT_6X10_H; row++) {
        uint8_t bits = glyph[row];
        for (int col = 0; col < FONT_6X10_W; col++) {
            if (bits & (0x80 >> col)) {
                int px = x + col * scale;
                int py = y + row * scale;
                if (scale == 1) put_pixel(fb, px, py, c);
                else            fill_rect(fb, px, py, scale, scale, c);
            }
        }
    }
}

static void draw_text_6x10(PieceFB *fb, const char *text, int x, int y, int scale, uint32_t c) {
    int col = 0;
    for (const char *p = text; *p; p++) {
        unsigned char ch = (unsigned char)*p;
        // Skip UTF-8 continuation bytes; render the lead byte as ? for now.
        if (ch >= 0x80) continue;
        if (ch == '\n') { col = 0; y += FONT_6X10_H * scale; continue; }
        draw_glyph_6x10(fb, ch, x + col * FONT_6X10_W * scale, y, scale, c);
        col++;
    }
}

static void draw_circle(PieceFB *fb, int cx, int cy, int r, int filled, uint32_t c) {
    if (r <= 0) return;
    if (filled) {
        for (int y = -r; y <= r; y++) {
            int w = (int)sqrtf((float)(r * r - y * y));
            for (int x = -w; x <= w; x++) put_pixel(fb, cx + x, cy + y, c);
        }
    } else {
        // Midpoint circle.
        int x = r, y = 0, err = 1 - r;
        while (x >= y) {
            put_pixel(fb, cx + x, cy + y, c);
            put_pixel(fb, cx + y, cy + x, c);
            put_pixel(fb, cx - y, cy + x, c);
            put_pixel(fb, cx - x, cy + y, c);
            put_pixel(fb, cx - x, cy - y, c);
            put_pixel(fb, cx - y, cy - x, c);
            put_pixel(fb, cx + y, cy - x, c);
            put_pixel(fb, cx + x, cy - y, c);
            y++;
            if (err <= 0) err += 2 * y + 1;
            else { x--; err += 2 * (y - x) + 1; }
        }
    }
}

// ── JS bindings ─────────────────────────────────────────────────────────────

static PieceCtx *ctx_from(JSContext *jsctx) {
    return (PieceCtx *)JS_GetContextOpaque(jsctx);
}

static uint32_t pack_rgb(int r, int g, int b, int a) {
    if (r < 0) r = 0; if (r > 255) r = 255;
    if (g < 0) g = 0; if (g > 255) g = 255;
    if (b < 0) b = 0; if (b > 255) b = 255;
    if (a < 0) a = 0; if (a > 255) a = 255;
    return ((uint32_t)a << 24) | ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
}

// Interpret 0–4 numeric args as (r,g,b[,a]) or (gray) with alpha 255 default.
static uint32_t args_to_argb(JSContext *jsctx, int argc, JSValueConst *argv) {
    int vals[4] = {0, 0, 0, 255};
    int n = argc < 4 ? argc : 4;
    for (int i = 0; i < n; i++) {
        int32_t v = 0;
        if (JS_ToInt32(jsctx, &v, argv[i]) == 0) vals[i] = v;
    }
    if (argc == 1) { vals[1] = vals[0]; vals[2] = vals[0]; }
    return pack_rgb(vals[0], vals[1], vals[2], vals[3]);
}

static JSValue js_wipe(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    uint32_t c = args_to_argb(jsctx, argc, argv);
    fill_rect(pc->fb, 0, 0, pc->fb->width, pc->fb->height, c);
    pc->ink_argb = c;
    return JS_UNDEFINED;
}

static JSValue js_ink(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    pc->ink_argb = args_to_argb(jsctx, argc, argv);
    // Return the api object so `ink(...).box(...)` chains work.
    return JS_DupValue(jsctx, pc->api);
}

static JSValue js_box(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    int32_t x = 0, y = 0, w = 0, h = 0;
    if (argc >= 1) JS_ToInt32(jsctx, &x, argv[0]);
    if (argc >= 2) JS_ToInt32(jsctx, &y, argv[1]);
    if (argc >= 3) JS_ToInt32(jsctx, &w, argv[2]);
    if (argc >= 4) JS_ToInt32(jsctx, &h, argv[3]);
    int outline = 0;
    if (argc >= 5 && JS_IsString(argv[4])) {
        const char *s = JS_ToCString(jsctx, argv[4]);
        if (s) { if (strcmp(s, "outline") == 0) outline = 1; JS_FreeCString(jsctx, s); }
    }
    if (outline) stroke_rect(pc->fb, x, y, w, h, pc->ink_argb);
    else         fill_rect  (pc->fb, x, y, w, h, pc->ink_argb);
    return JS_UNDEFINED;
}

static JSValue js_line(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    int32_t x0 = 0, y0 = 0, x1 = 0, y1 = 0;
    if (argc >= 1) JS_ToInt32(jsctx, &x0, argv[0]);
    if (argc >= 2) JS_ToInt32(jsctx, &y0, argv[1]);
    if (argc >= 3) JS_ToInt32(jsctx, &x1, argv[2]);
    if (argc >= 4) JS_ToInt32(jsctx, &y1, argv[3]);
    draw_line(pc->fb, x0, y0, x1, y1, pc->ink_argb);
    return JS_UNDEFINED;
}

static JSValue js_circle(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    int32_t cx = 0, cy = 0, r = 0;
    if (argc >= 1) JS_ToInt32(jsctx, &cx, argv[0]);
    if (argc >= 2) JS_ToInt32(jsctx, &cy, argv[1]);
    if (argc >= 3) JS_ToInt32(jsctx, &r,  argv[2]);
    int filled = 0;
    if (argc >= 4) filled = JS_ToBool(jsctx, argv[3]);
    draw_circle(pc->fb, cx, cy, r, filled, pc->ink_argb);
    return JS_UNDEFINED;
}

// write(text, { x, y, size, font })
// Only the 6x10 font is implemented here — `font: "matrix"` falls back to 6x10
// until stage 3 pulls in font.c. x/y default to 0; size defaults to 1.
static JSValue js_write(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    if (argc < 1 || !JS_IsString(argv[0])) return JS_UNDEFINED;
    const char *text = JS_ToCString(jsctx, argv[0]);
    if (!text) return JS_UNDEFINED;

    int32_t x = 0, y = 0, size = 1;
    if (argc >= 2 && JS_IsObject(argv[1])) {
        JSValue vx = JS_GetPropertyStr(jsctx, argv[1], "x");
        JSValue vy = JS_GetPropertyStr(jsctx, argv[1], "y");
        JSValue vs = JS_GetPropertyStr(jsctx, argv[1], "size");
        if (!JS_IsUndefined(vx)) JS_ToInt32(jsctx, &x, vx);
        if (!JS_IsUndefined(vy)) JS_ToInt32(jsctx, &y, vy);
        if (!JS_IsUndefined(vs)) JS_ToInt32(jsctx, &size, vs);
        JS_FreeValue(jsctx, vx); JS_FreeValue(jsctx, vy); JS_FreeValue(jsctx, vs);
    }
    if (size < 1) size = 1;
    draw_text_6x10(pc->fb, text, x, y, size, pc->ink_argb);
    JS_FreeCString(jsctx, text);
    return JS_UNDEFINED;
}

// performance.now(): fractional milliseconds since an arbitrary epoch.
// Matches Web Performance API semantics closely enough for piece code.
static JSValue js_performance_now(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    double ms = (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1.0e6;
    return JS_NewFloat64(jsctx, ms);
}

static JSValue js_console_log(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    for (int i = 0; i < argc; i++) {
        const char *s = JS_ToCString(jsctx, argv[i]);
        if (!s) continue;
        fprintf(stderr, "%s%s", i ? " " : "", s);
        JS_FreeCString(jsctx, s);
    }
    fprintf(stderr, "\n");
    return JS_UNDEFINED;
}

// ── Sound bindings (Phase B) ────────────────────────────────────────────────
// sound.synth returns a handle object carrying __voiceId + kill/update methods
// that reach back into the C audio engine via ctx_from(jsctx)->audio.
// The kill/update on `sound` itself take (id, fade) for convenience.

static double opt_num(JSContext *cx, JSValueConst obj, const char *key, double dflt) {
    JSValue v = JS_GetPropertyStr(cx, obj, key);
    double r = dflt;
    if (!JS_IsUndefined(v) && !JS_IsNull(v)) {
        double d = 0.0;
        if (JS_ToFloat64(cx, &d, v) == 0) r = d;
    }
    JS_FreeValue(cx, v);
    return r;
}

static char *opt_str(JSContext *cx, JSValueConst obj, const char *key) {
    JSValue v = JS_GetPropertyStr(cx, obj, key);
    char *out = NULL;
    if (JS_IsString(v)) {
        const char *s = JS_ToCString(cx, v);
        if (s) { out = strdup(s); JS_FreeCString(cx, s); }
    }
    JS_FreeValue(cx, v);
    return out;
}

static uint64_t handle_get_id(JSContext *cx, JSValueConst this_val) {
    JSValue vid = JS_GetPropertyStr(cx, this_val, "__voiceId");
    int64_t id = 0;
    JS_ToInt64(cx, &id, vid);
    JS_FreeValue(cx, vid);
    return (uint64_t)id;
}

static JSValue js_handle_kill(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    PieceCtx *pc = ctx_from(jsctx);
    uint64_t id = handle_get_id(jsctx, this_val);
    double fade = 0.01;
    if (argc >= 1) { double d; if (JS_ToFloat64(jsctx, &d, argv[0]) == 0) fade = d; }
    audio_kill(pc->audio, id, fade);
    return JS_UNDEFINED;
}

static JSValue js_handle_update(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    PieceCtx *pc = ctx_from(jsctx);
    uint64_t id = handle_get_id(jsctx, this_val);
    double freq = NAN, vol = NAN, pan = NAN;
    if (argc >= 1 && JS_IsObject(argv[0])) {
        freq = opt_num(jsctx, argv[0], "tone",   NAN);
        vol  = opt_num(jsctx, argv[0], "volume", NAN);
        pan  = opt_num(jsctx, argv[0], "pan",    NAN);
    }
    audio_update(pc->audio, id, freq, vol, pan);
    return JS_UNDEFINED;
}

static JSValue js_sound_synth(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    if (argc < 1 || !JS_IsObject(argv[0])) return JS_NULL;

    char *type_s    = opt_str(jsctx, argv[0], "type");
    double tone     = opt_num(jsctx, argv[0], "tone",     220.0);
    double duration = opt_num(jsctx, argv[0], "duration", 0.25);
    double volume   = opt_num(jsctx, argv[0], "volume",   0.5);
    double attack   = opt_num(jsctx, argv[0], "attack",   0.01);
    double decay    = opt_num(jsctx, argv[0], "decay",    0.1);
    double pan      = opt_num(jsctx, argv[0], "pan",      0.0);

    // Route gun preset names ("gun:pistol", "gun:sniper", ...) through the
    // full preset engine; everything else goes to the shared oscillator
    // path. Lets notepat opt-in to the richer percussion via a naming
    // convention without a new API surface.
    uint64_t id = 0;
    if (type_s && strncmp(type_s, "gun:", 4) == 0) {
        static const struct { const char *name; GunPreset preset; } guns[] = {
            {"pistol", GUN_PISTOL}, {"rifle", GUN_RIFLE},
            {"shotgun", GUN_SHOTGUN}, {"smg", GUN_SMG},
            {"suppressed", GUN_SUPPRESSED}, {"lmg", GUN_LMG},
            {"sniper", GUN_SNIPER}, {"grenade", GUN_GRENADE},
            {"rpg", GUN_RPG}, {"reload", GUN_RELOAD},
            {"cock", GUN_COCK}, {"ricochet", GUN_RICOCHET},
        };
        GunPreset preset = GUN_PISTOL;
        for (size_t i = 0; i < sizeof(guns)/sizeof(guns[0]); i++) {
            if (strcmp(type_s + 4, guns[i].name) == 0) { preset = guns[i].preset; break; }
        }
        id = audio_synth_gun(pc->audio, preset, duration, volume, attack, decay,
                             pan, 1.0, -1);
    } else {
        WaveType wave = audio_parse_wave(type_s);
        id = audio_synth(pc->audio, wave, tone, duration, volume, attack, decay, pan);
    }
    free(type_s);
    if (!id) return JS_NULL;

    JSValue handle = JS_NewObject(jsctx);
    JS_SetPropertyStr(jsctx, handle, "__voiceId", JS_NewInt64(jsctx, (int64_t)id));
    JS_SetPropertyStr(jsctx, handle, "kill",   JS_NewCFunction(jsctx, js_handle_kill,   "kill",   1));
    JS_SetPropertyStr(jsctx, handle, "update", JS_NewCFunction(jsctx, js_handle_update, "update", 1));
    return handle;
}

static JSValue js_sound_kill(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    // Accept (handle) or (id, fade). If the first arg is an object, treat as handle.
    uint64_t id = 0; double fade = 0.01;
    if (argc >= 1) {
        if (JS_IsObject(argv[0])) id = handle_get_id(jsctx, argv[0]);
        else { int64_t v; if (JS_ToInt64(jsctx, &v, argv[0]) == 0) id = (uint64_t)v; }
    }
    if (argc >= 2) { double d; if (JS_ToFloat64(jsctx, &d, argv[1]) == 0) fade = d; }
    audio_kill(pc->audio, id, fade);
    return JS_UNDEFINED;
}

// Event shape: { is: (pattern) => bool, x, y }.
static JSValue js_event_is(JSContext *jsctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    PieceCtx *pc = ctx_from(jsctx);
    if (!pc->current_event || argc < 1 || !JS_IsString(argv[0])) return JS_FALSE;
    const char *pat = JS_ToCString(jsctx, argv[0]);
    if (!pat) return JS_FALSE;
    // Support exact match and prefix match: e.is("touch") matches "touch",
    // e.is("keyboard:down") matches any "keyboard:down:*". Matches AC semantics.
    const char *t = pc->current_event->type;
    int ok = 0;
    size_t plen = strlen(pat), tlen = strlen(t);
    if (plen == tlen && strcmp(pat, t) == 0) ok = 1;
    else if (plen < tlen && strncmp(pat, t, plen) == 0 && t[plen] == ':') ok = 1;
    JS_FreeCString(jsctx, pat);
    return ok ? JS_TRUE : JS_FALSE;
}

// ── Setup ────────────────────────────────────────────────────────────────────

static JSValue build_screen_obj(JSContext *jsctx, int w, int h) {
    JSValue o = JS_NewObject(jsctx);
    JS_SetPropertyStr(jsctx, o, "width",  JS_NewInt32(jsctx, w));
    JS_SetPropertyStr(jsctx, o, "height", JS_NewInt32(jsctx, h));
    return o;
}

static char *read_file(const char *path, size_t *out_len);  // fwd decl

// Resolve a module specifier (e.g. "/lib/percussion.mjs") to a filesystem
// path. Leading slash = rooted at the lib path; anything else is treated
// relative to the piece directory (caller responsibility, not handled yet).
static void resolve_module(PieceCtx *pc, const char *name, char *out, size_t n) {
    if (name[0] == '/') {
        // strip one leading "/lib" if present to avoid doubling.
        const char *rest = name;
        if (strncmp(name, "/lib/", 5) == 0) rest = name + 4;  // keep the "/"
        snprintf(out, n, "%s%s", pc->lib_path, rest);
    } else {
        snprintf(out, n, "%s", name);
    }
}

// QuickJS module loader: read the file, compile as a module, return its
// JSModuleDef*. QuickJS wraps the def in a JSValue for us; we unwrap via
// JS_VALUE_GET_PTR. Signature matches JSModuleLoaderFunc.
static JSModuleDef *piece_module_loader(JSContext *jsctx, const char *module_name, void *opaque) {
    PieceCtx *pc = (PieceCtx *)opaque;
    char path[1200];
    resolve_module(pc, module_name, path, sizeof(path));

    size_t src_len = 0;
    char *src = read_file(path, &src_len);
    if (!src) {
        JS_ThrowReferenceError(jsctx, "module not found: %s (resolved to %s)", module_name, path);
        return NULL;
    }
    JSValue compiled = JS_Eval(jsctx, src, src_len, module_name,
                               JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
    free(src);
    if (JS_IsException(compiled)) return NULL;
    JSModuleDef *m = JS_VALUE_GET_PTR(compiled);
    JS_FreeValue(jsctx, compiled);
    return m;
}

static char *read_file(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (sz < 0) { fclose(f); return NULL; }
    char *buf = malloc((size_t)sz + 1);
    if (!buf) { fclose(f); return NULL; }
    if (fread(buf, 1, (size_t)sz, f) != (size_t)sz) { free(buf); fclose(f); return NULL; }
    buf[sz] = 0;
    fclose(f);
    if (out_len) *out_len = (size_t)sz;
    return buf;
}

PieceCtx *piece_load(const char *path, PieceFB *fb) {
    PieceCtx *pc = calloc(1, sizeof(PieceCtx));
    if (!pc) return NULL;
    pc->fb = fb;
    pc->ink_argb = 0xFFFFFFFF;
    pc->audio = audio_init();  // NULL if unavailable; JS stubs keep working

    pc->rt = JS_NewRuntime();
    if (!pc->rt) { free(pc); return NULL; }
    pc->jsctx = JS_NewContext(pc->rt);
    if (!pc->jsctx) { JS_FreeRuntime(pc->rt); free(pc); return NULL; }
    JS_SetContextOpaque(pc->jsctx, pc);

    // Build the api object and install its methods both on itself and on
    // globalThis. This lets pieces do `function paint({wipe, ink, ...})`
    // (destructure from arg) *and* call bare `box(...)` at module scope.
    JSContext *cx = pc->jsctx;
    JSValue global = JS_GetGlobalObject(cx);
    pc->api = JS_NewObject(cx);

    struct { const char *name; JSCFunction *fn; int argc; } binds[] = {
        { "wipe",   js_wipe,   0 },
        { "ink",    js_ink,    0 },
        { "box",    js_box,    0 },
        { "line",   js_line,   0 },
        { "circle", js_circle, 0 },
        { "write",  js_write,  0 },
        { NULL, NULL, 0 }
    };
    for (int i = 0; binds[i].name; i++) {
        JSValue f = JS_NewCFunction(cx, binds[i].fn, binds[i].name, binds[i].argc);
        JS_SetPropertyStr(cx, pc->api,  binds[i].name, JS_DupValue(cx, f));
        JS_SetPropertyStr(cx, global,   binds[i].name, f);
    }

    // screen as a plain object on the api (hello.mjs reads .width/.height)
    JSValue scr = build_screen_obj(cx, fb->width, fb->height);
    JS_SetPropertyStr(cx, pc->api,  "screen", JS_DupValue(cx, scr));
    JS_SetPropertyStr(cx, global,   "screen", scr);

    // Phase A: stubs are JS-level. Full-fat no-ops live on globalThis AND
    // on the api object so destructured params (`{ sound }`) resolve too.
    // Phase B replaces `sound` with a real audio engine bound in C.
#ifndef AC_BUILD_NAME
#define AC_BUILD_NAME "macos-dev"
#endif
    const char *stubs_js =
        "(function(api, version){\n"
        "  const noop = () => undefined;\n"
        "  const handle = () => ({ update: noop, kill: noop });\n"
        "  const sound = {\n"
        "    synth: handle, kill: noop,\n"
        "    sample:  { play: handle, kill: noop, loadData: noop, getData: noop },\n"
        "    replay:  { play: handle, kill: noop, loadData: noop, update: noop },\n"
        "    deck:    { play: noop, pause: noop, seek: noop, setSpeed: noop,\n"
        "               setVolume: noop, setCrossfader: noop, setMasterVolume: noop,\n"
        "               load: noop, decks: [] },\n"
        "    room:    { setMix: noop },\n"
        "    glitch:  { setMix: noop },\n"
        "    fx:      { setMix: noop },\n"
        "    speaker: { getRecentBuffer: () => ({ length: 0, rate: 48000 }),\n"
        "               systemVolume: 50 },\n"
        "    microphone: { sampleLength: 0, sampleRate: 48000, device: 'none' },\n"
        "    speak: noop,\n"
        "  };\n"
        "  const wifi = { scan: noop, connect: noop, disconnect: noop,\n"
        "                 networks: [], connected: false, state: 'idle',\n"
        "                 status: 'offline', ip: '', iface: '' };\n"
        "  const system = { version,\n"
        "    readFile: () => null, writeFile: noop,\n"
        "    fetchBinary: noop, mountMusic: noop, mountMusicMounted: false,\n"
        "    hdmi: null, typec: [], usbMidi: { status: noop },\n"
        "    ws: null, udp: null, jump: noop };\n"
        "  const trackpad = { dx: 0, dy: 0 };\n"
        "  const pressures = [];\n"
        "  // Host-provided globals the piece expects.\n"
        "  globalThis.__theme = { update: noop, dark: false };\n"
        "  // Named exports visible both as globals and via the api arg.\n"
        "  for (const [k, v] of Object.entries({ sound, wifi, system, trackpad, pressures })) {\n"
        "    globalThis[k] = v;\n"
        "    api[k] = v;\n"
        "  }\n"
        "})";
    {
        JSValue fn = JS_Eval(cx, stubs_js, strlen(stubs_js), "<stubs>", JS_EVAL_TYPE_GLOBAL);
        if (JS_IsException(fn)) {
            JSValue exc = JS_GetException(cx);
            const char *msg = JS_ToCString(cx, exc);
            fprintf(stderr, "[piece] stubs compile: %s\n", msg ? msg : "(unknown)");
            if (msg) JS_FreeCString(cx, msg);
            JS_FreeValue(cx, exc);
        } else {
            JSValue args[2] = { JS_DupValue(cx, pc->api), JS_NewString(cx, AC_BUILD_NAME) };
            JSValue r = JS_Call(cx, fn, JS_UNDEFINED, 2, args);
            if (JS_IsException(r)) {
                JSValue exc = JS_GetException(cx);
                const char *msg = JS_ToCString(cx, exc);
                fprintf(stderr, "[piece] stubs run: %s\n", msg ? msg : "(unknown)");
                if (msg) JS_FreeCString(cx, msg);
                JS_FreeValue(cx, exc);
            }
            JS_FreeValue(cx, r);
            JS_FreeValue(cx, args[0]);
            JS_FreeValue(cx, args[1]);
            JS_FreeValue(cx, fn);
        }
    }

    // Phase B: overlay real C-bound synth/kill onto the sound stub. Nested
    // namespaces (room/glitch/fx/deck/sample/replay/speak) stay as JS no-ops
    // for now — only note playback is wired through.
    if (pc->audio) {
        JSValue sound_obj = JS_GetPropertyStr(cx, global, "sound");
        if (JS_IsObject(sound_obj)) {
            JS_SetPropertyStr(cx, sound_obj, "synth",
                              JS_NewCFunction(cx, js_sound_synth, "synth", 1));
            JS_SetPropertyStr(cx, sound_obj, "kill",
                              JS_NewCFunction(cx, js_sound_kill, "kill", 2));
        }
        JS_FreeValue(cx, sound_obj);
    }

    // Minimal console so piece code that logs doesn't blow up.
    JSValue console = JS_NewObject(cx);
    JS_SetPropertyStr(cx, console, "log",   JS_NewCFunction(cx, js_console_log, "log",   0));
    JS_SetPropertyStr(cx, console, "warn",  JS_NewCFunction(cx, js_console_log, "warn",  0));
    JS_SetPropertyStr(cx, console, "error", JS_NewCFunction(cx, js_console_log, "error", 0));
    JS_SetPropertyStr(cx, global, "console", console);

    // performance.now() shim (notepat and others use it for timing).
    JSValue perf = JS_NewObject(cx);
    JS_SetPropertyStr(cx, perf, "now", JS_NewCFunction(cx, js_performance_now, "now", 0));
    JS_SetPropertyStr(cx, global, "performance", perf);

    // Resolve lib root. Env var wins; otherwise walk up looking for
    // system/public/aesthetic.computer/lib relative to the piece path.
    const char *env_lib = getenv("AC_LIB_PATH");
    if (env_lib && *env_lib) {
        snprintf(pc->lib_path, sizeof(pc->lib_path), "%s", env_lib);
    } else {
        // Piece is typically at fedac/native/pieces/<name>.mjs; lib is at
        // system/public/aesthetic.computer/lib at the repo root.
        snprintf(pc->lib_path, sizeof(pc->lib_path),
                 "../../../system/public/aesthetic.computer/lib");
    }

    // Wire up module loader so `import "/lib/percussion.mjs"` works.
    JS_SetModuleLoaderFunc(pc->rt, NULL, piece_module_loader, pc);

    size_t src_len = 0;
    char *src = read_file(path, &src_len);
    if (!src) {
        fprintf(stderr, "[piece] cannot read %s\n", path);
        JS_FreeValue(cx, global);
        piece_destroy(pc);
        return NULL;
    }

    // Register the piece under a stable module name so a subsequent
    // bootstrap module can re-import it by name. QuickJS 2024-01-13
    // doesn't expose a public JS_GetModuleNamespace, so we use a small
    // glue module to copy exports onto globalThis.
    const char *piece_module_name = "__piece";
    JSValue ret = JS_Eval(cx, src, src_len, piece_module_name, JS_EVAL_TYPE_MODULE);
    free(src);
    if (JS_IsException(ret)) {
        JSValue exc = JS_GetException(cx);
        const char *msg = JS_ToCString(cx, exc);
        fprintf(stderr, "[piece] eval error: %s\n", msg ? msg : "(unknown)");
        if (msg) JS_FreeCString(cx, msg);
        JS_FreeValue(cx, exc);
        JS_FreeValue(cx, ret);
        JS_FreeValue(cx, global);
        piece_destroy(pc);
        return NULL;
    }
    JS_FreeValue(cx, ret);

    // Drain any async jobs (module init promises, etc.).
    for (int i = 0; i < 100; i++) {
        JSContext *ctx1;
        int r = JS_ExecutePendingJob(pc->rt, &ctx1);
        if (r == 0) break;
        if (r < 0) {
            JSValue exc = JS_GetException(ctx1);
            const char *msg = JS_ToCString(ctx1, exc);
            fprintf(stderr, "[piece] pending job error: %s\n", msg ? msg : "(unknown)");
            if (msg) JS_FreeCString(ctx1, msg);
            JS_FreeValue(ctx1, exc);
            break;
        }
    }

    // Bootstrap: re-import the piece by name and copy lifecycle exports
    // to globalThis so we can fetch them with JS_GetPropertyStr.
    const char *glue =
        "import * as __p from \"__piece\";\n"
        "globalThis.__boot  = __p.boot;\n"
        "globalThis.__paint = __p.paint;\n"
        "globalThis.__act   = __p.act;\n"
        "globalThis.__sim   = __p.sim;\n"
        "globalThis.__leave = __p.leave;\n";
    JSValue g = JS_Eval(cx, glue, strlen(glue), "__glue", JS_EVAL_TYPE_MODULE);
    if (JS_IsException(g)) {
        JSValue exc = JS_GetException(cx);
        const char *msg = JS_ToCString(cx, exc);
        fprintf(stderr, "[piece] glue error: %s\n", msg ? msg : "(unknown)");
        if (msg) JS_FreeCString(cx, msg);
        JS_FreeValue(cx, exc);
        JS_FreeValue(cx, g);
        JS_FreeValue(cx, global);
        piece_destroy(pc);
        return NULL;
    }
    JS_FreeValue(cx, g);

    // Drain again — module imports can queue jobs.
    for (int i = 0; i < 100; i++) {
        JSContext *ctx1;
        int r = JS_ExecutePendingJob(pc->rt, &ctx1);
        if (r == 0) break;
        if (r < 0) break;
    }

    // Refresh global ref since we may have rebound properties.
    JS_FreeValue(cx, global);
    global = JS_GetGlobalObject(cx);

    pc->boot_fn  = JS_GetPropertyStr(cx, global, "__boot");
    pc->paint_fn = JS_GetPropertyStr(cx, global, "__paint");
    pc->sim_fn   = JS_GetPropertyStr(cx, global, "__sim");
    pc->act_fn   = JS_GetPropertyStr(cx, global, "__act");

    JS_FreeValue(cx, global);
    return pc;
}

static void call_lifecycle_with_api(PieceCtx *pc, JSValue fn) {
    if (JS_IsUndefined(fn) || !JS_IsFunction(pc->jsctx, fn)) return;
    JSValue arg = JS_DupValue(pc->jsctx, pc->api);
    JSValue ret = JS_Call(pc->jsctx, fn, JS_UNDEFINED, 1, &arg);
    if (JS_IsException(ret)) {
        JSValue exc = JS_GetException(pc->jsctx);
        const char *msg = JS_ToCString(pc->jsctx, exc);
        fprintf(stderr, "[piece] runtime error: %s\n", msg ? msg : "(unknown)");
        if (msg) JS_FreeCString(pc->jsctx, msg);
        JS_FreeValue(pc->jsctx, exc);
    }
    JS_FreeValue(pc->jsctx, ret);
    JS_FreeValue(pc->jsctx, arg);
}

void piece_boot(PieceCtx *pc)  { call_lifecycle_with_api(pc, pc->boot_fn);  }
void piece_paint(PieceCtx *pc) { call_lifecycle_with_api(pc, pc->paint_fn); }
void piece_sim(PieceCtx *pc)   { call_lifecycle_with_api(pc, pc->sim_fn);   }

void piece_act(PieceCtx *pc, const PieceEvent *ev) {
    if (!pc || !ev) return;
    if (JS_IsUndefined(pc->act_fn) || !JS_IsFunction(pc->jsctx, pc->act_fn)) return;
    pc->current_event = ev;

    JSContext *cx = pc->jsctx;
    JSValue evobj = JS_NewObject(cx);
    JS_SetPropertyStr(cx, evobj, "is", JS_NewCFunction(cx, js_event_is, "is", 1));
    JS_SetPropertyStr(cx, evobj, "x",  JS_NewInt32(cx, ev->x));
    JS_SetPropertyStr(cx, evobj, "y",  JS_NewInt32(cx, ev->y));
    JS_SetPropertyStr(cx, evobj, "type", JS_NewString(cx, ev->type));
    JS_SetPropertyStr(cx, evobj, "key",  JS_NewString(cx, ev->key));

    // Give the act arg `pc->api` as a prototype so `{sound, wifi, system}`
    // destructuring resolves through the chain; `event` lives as an own prop.
    JSValue arg = JS_NewObjectProto(cx, pc->api);
    JS_SetPropertyStr(cx, arg, "event", evobj);

    JSValue ret = JS_Call(cx, pc->act_fn, JS_UNDEFINED, 1, &arg);
    if (JS_IsException(ret)) {
        JSValue exc = JS_GetException(cx);
        const char *msg = JS_ToCString(cx, exc);
        fprintf(stderr, "[piece] act error: %s\n", msg ? msg : "(unknown)");
        if (msg) JS_FreeCString(cx, msg);
        JS_FreeValue(cx, exc);
    }
    JS_FreeValue(cx, ret);
    JS_FreeValue(cx, arg);
    pc->current_event = NULL;
}

void piece_destroy(PieceCtx *pc) {
    if (!pc) return;
    if (pc->audio) audio_destroy(pc->audio);
    if (pc->jsctx) {
        JS_FreeValue(pc->jsctx, pc->boot_fn);
        JS_FreeValue(pc->jsctx, pc->paint_fn);
        JS_FreeValue(pc->jsctx, pc->sim_fn);
        JS_FreeValue(pc->jsctx, pc->act_fn);
        JS_FreeValue(pc->jsctx, pc->api);
        JS_FreeContext(pc->jsctx);
    }
    if (pc->rt) JS_FreeRuntime(pc->rt);
    free(pc);
}
