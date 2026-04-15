#define _GNU_SOURCE
#include "js-bindings.h"
#include "usb-midi.h"
#include "recorder.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/wait.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <linux/reboot.h>
#include <fcntl.h>
#include <errno.h>
#include "qrcodegen.h"
#include <alsa/asoundlib.h>

// Defined in ac-native.c — logs to USB mount
extern void ac_log(const char *fmt, ...);
extern void ac_log_flush(void);
extern void ac_log_pause(void);
extern void ac_log_resume(void);
extern void perf_flush(void);

// Thread-local reference to current runtime (for C callbacks from JS)
static __thread ACRuntime *current_rt = NULL;
static __thread const char *current_phase = "boot";
static int config_cache_dirty = 1;  // reload /mnt/config.json when set

// Forward declaration (defined later in this file)
static int resolve_color_name(const char *name, ACColor *out);
static JSValue js_usb_midi_note_on(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);
static JSValue js_usb_midi_note_off(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);
static JSValue js_usb_midi_all_notes_off(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);

// ============================================================
// QuickJS Class IDs for 3D objects
// ============================================================
static JSClassID form_class_id = 0;
static JSClassID painting_class_id = 0;

// Form class finalizer
static void js_form_finalizer(JSRuntime *rt, JSValue val) {
    (void)rt;
    ACForm *form = JS_GetOpaque(val, form_class_id);
    if (form) {
        free(form->positions);
        free(form->colors);
        free(form->tex_coords);
        free(form);
    }
}

static JSClassDef form_class_def = {
    "Form",
    .finalizer = js_form_finalizer,
};

// Painting class finalizer
static void js_painting_finalizer(JSRuntime *rt, JSValue val) {
    (void)rt;
    ACFramebuffer *fb = JS_GetOpaque(val, painting_class_id);
    if (!fb) return;
    // Don't destroy runtime-owned nopaint buffers
    if (current_rt && (fb == current_rt->nopaint_painting || fb == current_rt->nopaint_buffer))
        return;
    fb_destroy(fb);
}

static JSClassDef painting_class_def = {
    "Painting",
    .finalizer = js_painting_finalizer,
};

// ============================================================
// Form constructor: new Form({type, positions, colors, texCoords}, {pos, rot, scale})
// ============================================================

static JSValue js_form_constructor(JSContext *ctx, JSValueConst new_target,
                                    int argc, JSValueConst *argv) {
    (void)new_target;
    if (argc < 1) return JS_EXCEPTION;

    ACForm *form = calloc(1, sizeof(ACForm));
    if (!form) return JS_EXCEPTION;
    form->scale = 1.0f;

    // Parse geometry descriptor (first argument)
    JSValue geom = argv[0];
    JSValue type_val = JS_GetPropertyStr(ctx, geom, "type");
    if (!JS_IsUndefined(type_val)) {
        const char *type_str = JS_ToCString(ctx, type_val);
        if (type_str && strcmp(type_str, "line") == 0)
            form->type = FORM_TYPE_LINE;
        else
            form->type = FORM_TYPE_TRIANGLE;
        if (type_str) JS_FreeCString(ctx, type_str);
    }
    JS_FreeValue(ctx, type_val);

    // Parse positions array: [[x,y,z,w], ...]
    JSValue pos_arr = JS_GetPropertyStr(ctx, geom, "positions");
    if (JS_IsArray(ctx, pos_arr)) {
        JSValue len_val = JS_GetPropertyStr(ctx, pos_arr, "length");
        int32_t len = 0;
        JS_ToInt32(ctx, &len, len_val);
        JS_FreeValue(ctx, len_val);

        form->vert_count = len;
        form->positions = calloc(len * 4, sizeof(float));

        for (int i = 0; i < len; i++) {
            JSValue vert = JS_GetPropertyUint32(ctx, pos_arr, i);
            for (int j = 0; j < 4; j++) {
                JSValue comp = JS_GetPropertyUint32(ctx, vert, j);
                double v = 0;
                JS_ToFloat64(ctx, &v, comp);
                form->positions[i * 4 + j] = (float)v;
                JS_FreeValue(ctx, comp);
            }
            JS_FreeValue(ctx, vert);
        }
    }
    JS_FreeValue(ctx, pos_arr);

    // Parse colors array: [[r,g,b,a], ...]
    JSValue col_arr = JS_GetPropertyStr(ctx, geom, "colors");
    if (JS_IsArray(ctx, col_arr)) {
        JSValue len_val = JS_GetPropertyStr(ctx, col_arr, "length");
        int32_t len = 0;
        JS_ToInt32(ctx, &len, len_val);
        JS_FreeValue(ctx, len_val);

        form->colors = calloc(len * 4, sizeof(float));
        form->has_colors = 1;

        for (int i = 0; i < len; i++) {
            JSValue col = JS_GetPropertyUint32(ctx, col_arr, i);
            for (int j = 0; j < 4; j++) {
                JSValue comp = JS_GetPropertyUint32(ctx, col, j);
                double v = 0;
                JS_ToFloat64(ctx, &v, comp);
                form->colors[i * 4 + j] = (float)v;
                JS_FreeValue(ctx, comp);
            }
            JS_FreeValue(ctx, col);
        }
    }
    JS_FreeValue(ctx, col_arr);

    // Parse texCoords array: [[u,v], ...]
    JSValue tc_arr = JS_GetPropertyStr(ctx, geom, "texCoords");
    if (JS_IsArray(ctx, tc_arr)) {
        JSValue len_val = JS_GetPropertyStr(ctx, tc_arr, "length");
        int32_t len = 0;
        JS_ToInt32(ctx, &len, len_val);
        JS_FreeValue(ctx, len_val);

        form->tex_coords = calloc(len * 2, sizeof(float));
        for (int i = 0; i < len; i++) {
            JSValue uv = JS_GetPropertyUint32(ctx, tc_arr, i);
            for (int j = 0; j < 2; j++) {
                JSValue comp = JS_GetPropertyUint32(ctx, uv, j);
                double v = 0;
                JS_ToFloat64(ctx, &v, comp);
                form->tex_coords[i * 2 + j] = (float)v;
                JS_FreeValue(ctx, comp);
            }
            JS_FreeValue(ctx, uv);
        }
    }
    JS_FreeValue(ctx, tc_arr);

    // Parse transform (second argument): {pos, rot, scale}
    if (argc >= 2 && JS_IsObject(argv[1])) {
        JSValue opts = argv[1];

        JSValue pos = JS_GetPropertyStr(ctx, opts, "pos");
        if (JS_IsArray(ctx, pos)) {
            for (int i = 0; i < 3; i++) {
                JSValue v = JS_GetPropertyUint32(ctx, pos, i);
                double d = 0; JS_ToFloat64(ctx, &d, v);
                form->position[i] = (float)d;
                JS_FreeValue(ctx, v);
            }
        }
        JS_FreeValue(ctx, pos);

        JSValue rot = JS_GetPropertyStr(ctx, opts, "rot");
        if (JS_IsArray(ctx, rot)) {
            for (int i = 0; i < 3; i++) {
                JSValue v = JS_GetPropertyUint32(ctx, rot, i);
                double d = 0; JS_ToFloat64(ctx, &d, v);
                form->rotation[i] = (float)d;
                JS_FreeValue(ctx, v);
            }
        }
        JS_FreeValue(ctx, rot);

        JSValue sc = JS_GetPropertyStr(ctx, opts, "scale");
        if (!JS_IsUndefined(sc)) {
            double d = 1.0; JS_ToFloat64(ctx, &d, sc);
            form->scale = (float)d;
        }
        JS_FreeValue(ctx, sc);
    }

    // Create JS object with opaque pointer
    JSValue obj = JS_NewObjectClass(ctx, form_class_id);
    JS_SetOpaque(obj, form);

    // Expose position and rotation as JS arrays (live references)
    JSValue js_pos = JS_NewArray(ctx);
    for (int i = 0; i < 3; i++)
        JS_SetPropertyUint32(ctx, js_pos, i, JS_NewFloat64(ctx, form->position[i]));
    JS_SetPropertyStr(ctx, obj, "position", js_pos);

    JSValue js_rot = JS_NewArray(ctx);
    for (int i = 0; i < 3; i++)
        JS_SetPropertyUint32(ctx, js_rot, i, JS_NewFloat64(ctx, form->rotation[i]));
    JS_SetPropertyStr(ctx, obj, "rotation", js_rot);

    JS_SetPropertyStr(ctx, obj, "noFade", JS_FALSE);

    return obj;
}

// ============================================================
// Chain API: .form(f) renders a 3D form with current ink + camera
// ============================================================

static JSValue js_chain_form(JSContext *ctx, JSValueConst this_val,
                              int argc, JSValueConst *argv) {
    if (argc < 1) return JS_DupValue(ctx, this_val);

    ACForm *form = JS_GetOpaque(argv[0], form_class_id);
    if (!form) return JS_DupValue(ctx, this_val);

    ACRuntime *rt = current_rt;
    if (!rt) return JS_DupValue(ctx, this_val);

    // Sync position/rotation from JS arrays back to C struct
    JSValue js_pos = JS_GetPropertyStr(ctx, argv[0], "position");
    if (JS_IsArray(ctx, js_pos)) {
        for (int i = 0; i < 3; i++) {
            JSValue v = JS_GetPropertyUint32(ctx, js_pos, i);
            double d = 0; JS_ToFloat64(ctx, &d, v);
            form->position[i] = (float)d;
            JS_FreeValue(ctx, v);
        }
    }
    JS_FreeValue(ctx, js_pos);

    JSValue js_rot = JS_GetPropertyStr(ctx, argv[0], "rotation");
    if (JS_IsArray(ctx, js_rot)) {
        for (int i = 0; i < 3; i++) {
            JSValue v = JS_GetPropertyUint32(ctx, js_rot, i);
            double d = 0; JS_ToFloat64(ctx, &d, v);
            form->rotation[i] = (float)d;
            JS_FreeValue(ctx, v);
        }
    }
    JS_FreeValue(ctx, js_rot);

    // Sync noFade
    JSValue nf = JS_GetPropertyStr(ctx, argv[0], "noFade");
    form->no_fade = JS_ToBool(ctx, nf);
    JS_FreeValue(ctx, nf);

    // Sync texture (painting with opaque ACFramebuffer*)
    JSValue tex = JS_GetPropertyStr(ctx, argv[0], "texture");
    if (!JS_IsUndefined(tex) && !JS_IsNull(tex)) {
        ACFramebuffer *tex_fb = JS_GetOpaque(tex, painting_class_id);
        form->texture = tex_fb;
    } else {
        form->texture = NULL;
    }
    JS_FreeValue(ctx, tex);

    // Ensure depth buffer exists
    if (!rt->depth_buf) {
        rt->depth_buf = depth_create(rt->graph->fb->width, rt->graph->fb->height,
                                     rt->graph->fb->stride);
    }

    // Build view and projection matrices
    mat4 view, proj;
    camera3d_view_matrix(view, &rt->camera3d);
    float aspect = (float)rt->graph->fb->width / (float)rt->graph->fb->height;
    mat4_perspective(proj, 70.0f * (float)M_PI / 180.0f, aspect, 0.01f, 100.0f);

    // Render
    graph3d_render_form(rt->graph->fb, rt->depth_buf, form,
                        view, proj, rt->graph->ink, &rt->render_stats);

    return JS_DupValue(ctx, this_val);
}

// Chain .ink() — sets ink color, returns chain for further chaining
static JSValue js_chain_ink(JSContext *ctx, JSValueConst this_val,
                             int argc, JSValueConst *argv) {
    ACGraph *g = current_rt->graph;
    ACColor c = {255, 255, 255, 255};
    if (argc >= 3) {
        int r, gr, b;
        JS_ToInt32(ctx, &r, argv[0]);
        JS_ToInt32(ctx, &gr, argv[1]);
        JS_ToInt32(ctx, &b, argv[2]);
        c.r = (uint8_t)r; c.g = (uint8_t)gr; c.b = (uint8_t)b;
        if (argc >= 4) { int a; JS_ToInt32(ctx, &a, argv[3]); c.a = (uint8_t)a; }
    } else if (argc == 1) {
        if (JS_IsString(argv[0])) {
            const char *name = JS_ToCString(ctx, argv[0]);
            if (!resolve_color_name(name, &c))
                c = (ACColor){255, 255, 255, 255};
            JS_FreeCString(ctx, name);
        } else {
            int v; if (JS_ToInt32(ctx, &v, argv[0]) == 0)
                c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
        }
    }
    graph_ink(g, c);
    return JS_DupValue(ctx, this_val);
}

// Chain .write() — forward to existing write, return chain
static JSValue js_write(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv);
static JSValue js_chain_write(JSContext *ctx, JSValueConst this_val,
                               int argc, JSValueConst *argv) {
    js_write(ctx, JS_UNDEFINED, argc, argv);
    return JS_DupValue(ctx, this_val);
}

// ============================================================
// penLock() — enables FPS camera mode
// ============================================================

static JSValue js_pen_lock(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt) {
        current_rt->pen_locked = 1;
        current_rt->fps_system_active = 1;
        camera3d_init(&current_rt->camera3d);
    }
    return JS_UNDEFINED;
}

// ============================================================
// CUBEL geometry constant (wireframe cube, 12 lines = 24 vertices)
// ============================================================

static JSValue build_cubel(JSContext *ctx) {
    // 12 edges of a unit cube from -1 to 1
    static const float cube_lines[24][4] = {
        // Bottom face
        {-1,-1,-1,1}, { 1,-1,-1,1},
        { 1,-1,-1,1}, { 1,-1, 1,1},
        { 1,-1, 1,1}, {-1,-1, 1,1},
        {-1,-1, 1,1}, {-1,-1,-1,1},
        // Top face
        {-1, 1,-1,1}, { 1, 1,-1,1},
        { 1, 1,-1,1}, { 1, 1, 1,1},
        { 1, 1, 1,1}, {-1, 1, 1,1},
        {-1, 1, 1,1}, {-1, 1,-1,1},
        // Vertical edges
        {-1,-1,-1,1}, {-1, 1,-1,1},
        { 1,-1,-1,1}, { 1, 1,-1,1},
        { 1,-1, 1,1}, { 1, 1, 1,1},
        {-1,-1, 1,1}, {-1, 1, 1,1},
    };

    JSValue geom = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, geom, "type", JS_NewString(ctx, "line"));

    JSValue positions = JS_NewArray(ctx);
    for (int i = 0; i < 24; i++) {
        JSValue vert = JS_NewArray(ctx);
        for (int j = 0; j < 4; j++)
            JS_SetPropertyUint32(ctx, vert, j, JS_NewFloat64(ctx, cube_lines[i][j]));
        JS_SetPropertyUint32(ctx, positions, i, vert);
    }
    JS_SetPropertyStr(ctx, geom, "positions", positions);

    return geom;
}

// QUAD geometry constant (2 triangles = 6 vertices with gradient colors)
static JSValue build_quad(JSContext *ctx) {
    static const float quad_pos[6][4] = {
        {-1,-1,0,1}, {-1,1,0,1}, {1,1,0,1},  // tri 1
        {-1,-1,0,1}, {1,1,0,1},  {1,-1,0,1}, // tri 2
    };
    static const float quad_col[6][4] = {
        {1,0,0,1}, {0,1,0,1}, {0,0,1,1},
        {1,0,0,1}, {0,0,1,1}, {1,1,0,1},
    };

    JSValue geom = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, geom, "type", JS_NewString(ctx, "triangle"));

    JSValue positions = JS_NewArray(ctx);
    JSValue colors = JS_NewArray(ctx);
    for (int i = 0; i < 6; i++) {
        JSValue vert = JS_NewArray(ctx);
        JSValue col = JS_NewArray(ctx);
        for (int j = 0; j < 4; j++) {
            JS_SetPropertyUint32(ctx, vert, j, JS_NewFloat64(ctx, quad_pos[i][j]));
            JS_SetPropertyUint32(ctx, col, j, JS_NewFloat64(ctx, quad_col[i][j]));
        }
        JS_SetPropertyUint32(ctx, positions, i, vert);
        JS_SetPropertyUint32(ctx, colors, i, col);
    }
    JS_SetPropertyStr(ctx, geom, "positions", positions);
    JS_SetPropertyStr(ctx, geom, "colors", colors);

    return geom;
}

// ============================================================
// JS Native Functions — Graphics
// ============================================================

// Resolve a CSS color name string to an ACColor. Returns 1 on match, 0 if unknown.
static int resolve_color_name(const char *name, ACColor *out) {
    if (!name) return 0;
    if (strcmp(name, "black") == 0)       { *out = (ACColor){0, 0, 0, 255}; return 1; }
    if (strcmp(name, "white") == 0)       { *out = (ACColor){255, 255, 255, 255}; return 1; }
    if (strcmp(name, "red") == 0)         { *out = (ACColor){255, 0, 0, 255}; return 1; }
    if (strcmp(name, "green") == 0)       { *out = (ACColor){0, 128, 0, 255}; return 1; }
    if (strcmp(name, "blue") == 0)        { *out = (ACColor){0, 0, 255, 255}; return 1; }
    if (strcmp(name, "yellow") == 0)      { *out = (ACColor){255, 255, 0, 255}; return 1; }
    if (strcmp(name, "cyan") == 0)        { *out = (ACColor){0, 255, 255, 255}; return 1; }
    if (strcmp(name, "magenta") == 0)     { *out = (ACColor){255, 0, 255, 255}; return 1; }
    if (strcmp(name, "gray") == 0)        { *out = (ACColor){128, 128, 128, 255}; return 1; }
    if (strcmp(name, "grey") == 0)        { *out = (ACColor){128, 128, 128, 255}; return 1; }
    if (strcmp(name, "orange") == 0)      { *out = (ACColor){255, 165, 0, 255}; return 1; }
    if (strcmp(name, "purple") == 0)      { *out = (ACColor){128, 0, 128, 255}; return 1; }
    if (strcmp(name, "pink") == 0)        { *out = (ACColor){255, 192, 203, 255}; return 1; }
    if (strcmp(name, "lime") == 0)        { *out = (ACColor){0, 255, 0, 255}; return 1; }
    if (strcmp(name, "brown") == 0)       { *out = (ACColor){139, 69, 19, 255}; return 1; }
    return 0;
}

static JSValue js_wipe(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACGraph *g = current_rt->graph;
    ACColor c = {0, 0, 0, 255};
    if (argc == 0) {
        c = (ACColor){255, 255, 255, 255};
    } else if (argc >= 3) {
        int r, gr, b;
        JS_ToInt32(ctx, &r, argv[0]);
        JS_ToInt32(ctx, &gr, argv[1]);
        JS_ToInt32(ctx, &b, argv[2]);
        c = (ACColor){(uint8_t)r, (uint8_t)gr, (uint8_t)b, 255};
    } else if (argc == 1) {
        if (JS_IsString(argv[0])) {
            const char *name = JS_ToCString(ctx, argv[0]);
            if (!resolve_color_name(name, &c))
                c = (ACColor){0, 0, 0, 255}; // unknown name → black
            JS_FreeCString(ctx, name);
        } else {
            int v;
            if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
                c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
            }
        }
    }
    graph_wipe(g, c);

    // Clear depth buffer for new frame
    if (current_rt && current_rt->depth_buf)
        depth_clear(current_rt->depth_buf);
    // Reset render stats
    if (current_rt)
        memset(&current_rt->render_stats, 0, sizeof(ACRenderStats));

    // Return chain object for .form()/.ink() chaining
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue chain = JS_GetPropertyStr(ctx, global, "__paintApi");
    JS_FreeValue(ctx, global);
    return chain;
}

static JSValue js_ink(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACGraph *g = current_rt->graph;
    ACColor c = {255, 255, 255, 255};
    if (argc >= 3) {
        int r, gr, b;
        JS_ToInt32(ctx, &r, argv[0]);
        JS_ToInt32(ctx, &gr, argv[1]);
        JS_ToInt32(ctx, &b, argv[2]);
        c.r = (uint8_t)r; c.g = (uint8_t)gr; c.b = (uint8_t)b;
        if (argc >= 4) {
            int a; JS_ToInt32(ctx, &a, argv[3]);
            c.a = (uint8_t)a;
        }
    } else if (argc == 1) {
        if (JS_IsString(argv[0])) {
            const char *name = JS_ToCString(ctx, argv[0]);
            if (!resolve_color_name(name, &c))
                c = (ACColor){255, 255, 255, 255}; // unknown name → white
            JS_FreeCString(ctx, name);
        } else {
            int v;
            if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
                c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
            }
        }
    }
    graph_ink(g, c);

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue paint_api = JS_GetPropertyStr(ctx, global, "__paintApi");
    JS_FreeValue(ctx, global);
    return paint_api;
}

static JSValue js_plot(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_UNDEFINED;
    int x, y;
    JS_ToInt32(ctx, &x, argv[0]);
    JS_ToInt32(ctx, &y, argv[1]);
    graph_plot(current_rt->graph, x, y);
    return JS_UNDEFINED;
}

static JSValue js_line(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 4) return JS_UNDEFINED;
    int x0, y0, x1, y1;
    JS_ToInt32(ctx, &x0, argv[0]);
    JS_ToInt32(ctx, &y0, argv[1]);
    JS_ToInt32(ctx, &x1, argv[2]);
    JS_ToInt32(ctx, &y1, argv[3]);
    if (argc >= 5) {
        int thickness;
        JS_ToInt32(ctx, &thickness, argv[4]);
        graph_line_thick(current_rt->graph, x0, y0, x1, y1, thickness);
    } else {
        graph_line(current_rt->graph, x0, y0, x1, y1);
    }
    return JS_UNDEFINED;
}

static JSValue js_box(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 4) return JS_UNDEFINED;
    int x, y, w, h;
    JS_ToInt32(ctx, &x, argv[0]);
    JS_ToInt32(ctx, &y, argv[1]);
    JS_ToInt32(ctx, &w, argv[2]);
    JS_ToInt32(ctx, &h, argv[3]);

    int filled = 1;
    if (argc >= 5 && JS_IsString(argv[4])) {
        const char *mode = JS_ToCString(ctx, argv[4]);
        if (mode && strcmp(mode, "outline") == 0) filled = 0;
        JS_FreeCString(ctx, mode);
    }
    graph_box(current_rt->graph, x, y, w, h, filled);

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue paint_api = JS_GetPropertyStr(ctx, global, "__paintApi");
    JS_FreeValue(ctx, global);
    return paint_api;
}

static JSValue js_circle(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 3) return JS_UNDEFINED;
    int cx, cy, r;
    JS_ToInt32(ctx, &cx, argv[0]);
    JS_ToInt32(ctx, &cy, argv[1]);
    JS_ToInt32(ctx, &r, argv[2]);
    int filled = (argc >= 4) ? JS_ToBool(ctx, argv[3]) : 0;
    graph_circle(current_rt->graph, cx, cy, r, filled);
    return JS_UNDEFINED;
}

static JSValue js_write(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;

    int x = 0, y = 0, scale = 1;

    if (argc >= 2) {
        if (JS_IsObject(argv[1])) {
            JSValue vx = JS_GetPropertyStr(ctx, argv[1], "x");
            JSValue vy = JS_GetPropertyStr(ctx, argv[1], "y");
            if (!JS_IsUndefined(vx)) JS_ToInt32(ctx, &x, vx);
            if (!JS_IsUndefined(vy)) JS_ToInt32(ctx, &y, vy);

            JSValue center = JS_GetPropertyStr(ctx, argv[1], "center");
            if (JS_IsString(center)) {
                const char *cv = JS_ToCString(ctx, center);
                if (cv) {
                    int tw = font_measure(text, scale);
                    if (strchr(cv, 'x')) x = (current_rt->graph->fb->width - tw) / 2;
                    if (strchr(cv, 'y')) y = (current_rt->graph->fb->height - FONT_CHAR_H * scale) / 2;
                    JS_FreeCString(ctx, cv);
                }
            }
            JS_FreeValue(ctx, center);

            JSValue size = JS_GetPropertyStr(ctx, argv[1], "size");
            if (!JS_IsUndefined(size)) JS_ToInt32(ctx, &scale, size);
            JS_FreeValue(ctx, size);

            JS_FreeValue(ctx, vx);
            JS_FreeValue(ctx, vy);
        }
    }

    // Check for font option: "matrix", "font_1"/"6x10", default is font_1 (6x10)
    int font_id = FONT_6X10;
    if (argc >= 2 && JS_IsObject(argv[1])) {
        JSValue font_v = JS_GetPropertyStr(ctx, argv[1], "font");
        if (JS_IsString(font_v)) {
            const char *fname = JS_ToCString(ctx, font_v);
            if (fname) {
                if (strcmp(fname, "matrix") == 0) font_id = FONT_MATRIX;
                else if (strcmp(fname, "font_1") == 0 || strcmp(fname, "6x10") == 0)
                    font_id = FONT_6X10;
                JS_FreeCString(ctx, fname);
            }
        }
        JS_FreeValue(ctx, font_v);

        // Re-check center with correct font metrics for non-8x8 fonts
        if (font_id != FONT_8X8) {
            JSValue center2 = JS_GetPropertyStr(ctx, argv[1], "center");
            if (JS_IsString(center2)) {
                const char *cv = JS_ToCString(ctx, center2);
                if (cv) {
                    int tw = (font_id == FONT_MATRIX)
                        ? font_measure_matrix(text, scale)
                        : font_measure_6x10(text, scale);
                    int th = (font_id == FONT_MATRIX)
                        ? 8 * scale  // MatrixChunky8 ascent
                        : FONT_6X10_CHAR_H * scale;
                    if (strchr(cv, 'x')) x = (current_rt->graph->fb->width - tw) / 2;
                    if (strchr(cv, 'y')) y = (current_rt->graph->fb->height - th) / 2;
                    JS_FreeCString(ctx, cv);
                }
            }
            JS_FreeValue(ctx, center2);
        }
    }

    if (font_id == FONT_MATRIX)
        font_draw_matrix(current_rt->graph, text, x, y, scale);
    else if (font_id == FONT_6X10)
        font_draw_6x10(current_rt->graph, text, x, y, scale);
    else
        font_draw(current_rt->graph, text, x, y, scale);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

static JSValue js_scroll(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int dx = 0, dy = 0;
    if (argc >= 1) JS_ToInt32(ctx, &dx, argv[0]);
    if (argc >= 2) JS_ToInt32(ctx, &dy, argv[1]);
    graph_scroll(current_rt->graph, dx, dy);
    return JS_UNDEFINED;
}

static JSValue js_blur(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int s = 1;
    if (argc >= 1) JS_ToInt32(ctx, &s, argv[0]);
    graph_blur(current_rt->graph, s);
    return JS_UNDEFINED;
}

static JSValue js_zoom(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    double level = 1.0;
    if (argc >= 1) JS_ToFloat64(ctx, &level, argv[0]);
    graph_zoom(current_rt->graph, level);
    return JS_UNDEFINED;
}

static JSValue js_contrast(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    double level = 1.0;
    if (argc >= 1) JS_ToFloat64(ctx, &level, argv[0]);
    graph_contrast(current_rt->graph, level);
    return JS_UNDEFINED;
}

static JSValue js_spin(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    double angle = 0.0;
    if (argc >= 1) JS_ToFloat64(ctx, &angle, argv[0]);
    graph_spin(current_rt->graph, angle);
    return JS_UNDEFINED;
}

// qr(text, x, y, scale) — render QR code at position
static JSValue js_qr(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 3) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;
    int x = 0, y = 0, scale = 2;
    JS_ToInt32(ctx, &x, argv[1]);
    JS_ToInt32(ctx, &y, argv[2]);
    if (argc >= 4) JS_ToInt32(ctx, &scale, argv[3]);
    graph_qr(current_rt->graph, text, x, y, scale);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

// ============================================================
// JS Native Functions — System
// ============================================================

static JSValue js_console_log(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    // Build message string, then log via ac_log (writes to both stderr + log file)
    char buf[512];
    int pos = 0;
    for (int i = 0; i < argc && pos < (int)sizeof(buf) - 2; i++) {
        const char *s = JS_ToCString(ctx, argv[i]);
        if (s) {
            int n = snprintf(buf + pos, sizeof(buf) - pos, "%s%s", i ? " " : "", s);
            if (n > 0) pos += n;
            JS_FreeCString(ctx, s);
        }
    }
    buf[pos] = '\0';
    ac_log("[js] %s\n", buf);
    return JS_UNDEFINED;
}

static JSValue js_performance_now(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    double ms = ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
    return JS_NewFloat64(ctx, ms);
}

// No-op function for stubs
static JSValue js_noop(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)ctx; (void)this_val; (void)argc; (void)argv;
    return JS_UNDEFINED;
}

// clock.time() — returns a JS Date object with the current system time
static JSValue js_clock_time(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    double ms = (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1000000.0;
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue date_ctor = JS_GetPropertyStr(ctx, global, "Date");
    JSValue ms_val = JS_NewFloat64(ctx, ms);
    JSValue date = JS_CallConstructor(ctx, date_ctor, 1, &ms_val);
    JS_FreeValue(ctx, ms_val);
    JS_FreeValue(ctx, date_ctor);
    JS_FreeValue(ctx, global);
    return date;
}

// Returns a resolved promise with null
static JSValue js_promise_null(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    // Use Promise.resolve(null)
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue promise = JS_GetPropertyStr(ctx, global, "Promise");
    JSValue resolve = JS_GetPropertyStr(ctx, promise, "resolve");
    JSValue null_val = JS_NULL;
    JSValue result = JS_Call(ctx, resolve, promise, 1, &null_val);
    JS_FreeValue(ctx, resolve);
    JS_FreeValue(ctx, promise);
    JS_FreeValue(ctx, global);
    return result;
}

// Returns a function that returns a no-op object with send()
static JSValue js_net_udp(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "send", JS_NewCFunction(ctx, js_noop, "send", 2));
    return obj;
}

// ============================================================
// Sound Bindings
// ============================================================

static WaveType parse_wave_type(const char *type) {
    if (!type) return WAVE_SINE;
    if (strcmp(type, "sine") == 0) return WAVE_SINE;
    if (strcmp(type, "triangle") == 0) return WAVE_TRIANGLE;
    if (strcmp(type, "sawtooth") == 0) return WAVE_SAWTOOTH;
    if (strcmp(type, "square") == 0) return WAVE_SQUARE;
    if (strcmp(type, "noise-white") == 0 || strcmp(type, "noise") == 0) return WAVE_NOISE;
    if (strcmp(type, "whistle") == 0 || strcmp(type, "ocarina") == 0 ||
        strcmp(type, "flute") == 0 || strcmp(type, "skullwhistle") == 0 ||
        strcmp(type, "skull-whistle") == 0) return WAVE_WHISTLE;
    if (strncmp(type, "gun", 3) == 0) return WAVE_GUN;
    // composite → treat as sine for now
    return WAVE_SINE;
}

// Parse the preset suffix for a gun-* wave type string. Optional model
// suffix `/classic` or `/physical` overrides the preset's default model.
// Examples: "gun-pistol", "gun-pistol/classic", "gun-pistol/physical".
// Returns GUN_PISTOL on parse failure. Pass `out_force_model` to receive
// the override (-1 = use preset default, 0 = CLASSIC, 1 = PHYSICAL).
static GunPreset parse_gun_preset(const char *type, int *out_force_model) {
    if (out_force_model) *out_force_model = -1;
    if (!type) return GUN_PISTOL;
    if (strncmp(type, "gun", 3) != 0) return GUN_PISTOL;
    const char *p = type + 3;
    if (*p == '-' || *p == '_') p++;
    if (!*p) return GUN_PISTOL;

    // Split off /classic or /physical suffix into a temp buffer.
    char name_buf[32];
    const char *slash = strchr(p, '/');
    if (slash && out_force_model) {
        size_t n = (size_t)(slash - p);
        if (n >= sizeof(name_buf)) n = sizeof(name_buf) - 1;
        memcpy(name_buf, p, n);
        name_buf[n] = '\0';
        p = name_buf;
        const char *m = slash + 1;
        if (strcmp(m, "classic") == 0) *out_force_model = 0;
        else if (strcmp(m, "physical") == 0 || strcmp(m, "phys") == 0) *out_force_model = 1;
    }

    if (strcmp(p, "pistol") == 0) return GUN_PISTOL;
    if (strcmp(p, "rifle") == 0) return GUN_RIFLE;
    if (strcmp(p, "shotgun") == 0) return GUN_SHOTGUN;
    if (strcmp(p, "smg") == 0) return GUN_SMG;
    if (strcmp(p, "suppressed") == 0 || strcmp(p, "silenced") == 0) return GUN_SUPPRESSED;
    if (strcmp(p, "lmg") == 0 || strcmp(p, "mg") == 0) return GUN_LMG;
    if (strcmp(p, "sniper") == 0) return GUN_SNIPER;
    if (strcmp(p, "grenade") == 0) return GUN_GRENADE;
    if (strcmp(p, "rpg") == 0 || strcmp(p, "rocket") == 0) return GUN_RPG;
    if (strcmp(p, "reload") == 0) return GUN_RELOAD;
    if (strcmp(p, "cock") == 0 || strcmp(p, "bolt") == 0) return GUN_COCK;
    if (strcmp(p, "ricochet") == 0 || strcmp(p, "pew") == 0) return GUN_RICOCHET;
    return GUN_PISTOL;
}

// synthObj.kill(fade) — method on synth return object
static JSValue js_synth_obj_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    ACAudio *audio = current_rt->audio;
    if (!audio) return JS_UNDEFINED;

    // Get id from 'this' object
    JSValue id_v = JS_GetPropertyStr(ctx, this_val, "id");
    double id_d = 0;
    if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
    JS_FreeValue(ctx, id_v);
    uint64_t id = (uint64_t)id_d;
    if (id == 0) return JS_UNDEFINED;

    double fade = 0.025;
    if (argc >= 1 && JS_IsNumber(argv[0])) {
        JS_ToFloat64(ctx, &fade, argv[0]);
    }
    audio_kill(audio, id, fade);
    return JS_UNDEFINED;
}

// synthObj.update({volume, tone, pan}) — method on synth return object
static JSValue js_synth_obj_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    // Get id from 'this' object
    JSValue id_v = JS_GetPropertyStr(ctx, this_val, "id");
    double id_d = 0;
    if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
    JS_FreeValue(ctx, id_v);
    uint64_t id = (uint64_t)id_d;
    if (id == 0) return JS_UNDEFINED;

    double freq = -1, vol = -1, pan = -3;
    JSValue v;
    v = JS_GetPropertyStr(ctx, argv[0], "tone");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &vol, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    audio_update(audio, id, freq, vol, pan);
    return JS_UNDEFINED;
}

// sound.synth({type, tone, duration, volume, attack, decay, pan})
static JSValue js_synth(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    JSValue opts = argv[0];

    // Parse type. For "gun-*" strings we also extract the preset so the
    // DWG synth can branch to audio_synth_gun below. Optional "/classic"
    // or "/physical" suffix overrides the preset's default model.
    WaveType wt = WAVE_SINE;
    GunPreset gun_preset = GUN_PISTOL;
    int gun_force_model = -1;
    int is_gun = 0;
    JSValue type_v = JS_GetPropertyStr(ctx, opts, "type");
    if (JS_IsString(type_v)) {
        const char *ts = JS_ToCString(ctx, type_v);
        wt = parse_wave_type(ts);
        if (wt == WAVE_GUN) {
            gun_preset = parse_gun_preset(ts, &gun_force_model);
            is_gun = 1;
        }
        JS_FreeCString(ctx, ts);
    }
    JS_FreeValue(ctx, type_v);

    // Parse tone (can be number or string)
    double freq = 440.0;
    JSValue tone_v = JS_GetPropertyStr(ctx, opts, "tone");
    if (JS_IsNumber(tone_v)) {
        JS_ToFloat64(ctx, &freq, tone_v);
    } else if (JS_IsString(tone_v)) {
        const char *ts = JS_ToCString(ctx, tone_v);
        freq = audio_note_to_freq(ts);
        JS_FreeCString(ctx, ts);
    }
    JS_FreeValue(ctx, tone_v);

    // Parse duration (number or "🔁" for infinity)
    double duration = 0.1;
    JSValue dur_v = JS_GetPropertyStr(ctx, opts, "duration");
    if (JS_IsNumber(dur_v)) {
        JS_ToFloat64(ctx, &duration, dur_v);
    } else if (JS_IsString(dur_v)) {
        duration = INFINITY;
    }
    JS_FreeValue(ctx, dur_v);

    // Parse volume, attack, decay, pan
    double volume = 1.0, attack = 0.005, decay = 0.1, pan = 0.0;
    JSValue v;

    v = JS_GetPropertyStr(ctx, opts, "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &volume, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "attack");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &attack, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "decay");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &decay, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    // Create voice. Guns take a separate path so we can seed per-preset
    // DWG parameters (bore length, body modes, etc).
    uint64_t id;
    if (is_gun) {
        // Let `tone` (1.0 default) scale the combustion pressure so pads
        // can express accent via velocity. A JS caller passing tone=1.0
        // = unity pressure from the preset.
        double pressure_scale = freq > 0 && freq < 5.0 ? freq : 1.0;
        id = audio_synth_gun(audio, gun_preset, duration, volume, attack,
                             decay, pan, pressure_scale, gun_force_model);
        fprintf(stderr, "[synth] gun preset=%d model=%d vol=%.2f dur=%.1f id=%lu\n",
                gun_preset, gun_force_model, volume, duration, (unsigned long)id);
        ac_log("[synth] gun preset=%d model=%d vol=%.2f dur=%.1f id=%lu\n",
               gun_preset, gun_force_model, volume, duration, (unsigned long)id);
    } else {
        id = audio_synth(audio, wt, freq, duration, volume, attack, decay, pan);
        fprintf(stderr, "[synth] type=%d freq=%.1f vol=%.2f dur=%.1f id=%lu\n",
                wt, freq, volume, duration, (unsigned long)id);
        ac_log("[synth] type=%d freq=%.1f vol=%.2f dur=%.1f id=%lu\n",
               wt, freq, volume, duration, (unsigned long)id);
    }

    // For gun voices, an optional `params` object on opts passes per-shot
    // overrides into the C-side synth state (drag-to-edit on inspector
    // cards). Each numeric property maps to a gun_presets[] field key.
    // Applied immediately after synth init so the next audio thread tick
    // sees the patched values.
    if (is_gun && id) {
        JSValue params_v = JS_GetPropertyStr(ctx, opts, "params");
        if (JS_IsObject(params_v)) {
            JSPropertyEnum *props = NULL;
            uint32_t plen = 0;
            if (JS_GetOwnPropertyNames(ctx, &props, &plen, params_v,
                                       JS_GPN_STRING_MASK | JS_GPN_ENUM_ONLY) == 0) {
                for (uint32_t i = 0; i < plen; i++) {
                    const char *k = JS_AtomToCString(ctx, props[i].atom);
                    if (k) {
                        JSValue pv = JS_GetProperty(ctx, params_v, props[i].atom);
                        double pd;
                        if (JS_IsNumber(pv) && JS_ToFloat64(ctx, &pd, pv) == 0) {
                            audio_gun_voice_set_param(audio, id, k, pd);
                        }
                        JS_FreeValue(ctx, pv);
                        JS_FreeCString(ctx, k);
                    }
                    JS_FreeAtom(ctx, props[i].atom);
                }
                js_free(ctx, props);
            }
        }
        JS_FreeValue(ctx, params_v);
    }

    // Return sound object with kill(), update(), startedAt
    JSValue snd = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, snd, "id", JS_NewFloat64(ctx, (double)id));
    JS_SetPropertyStr(ctx, snd, "startedAt", JS_NewFloat64(ctx, audio->time));

    // kill(fade) and update({volume,tone,pan}) methods
    JS_SetPropertyStr(ctx, snd, "kill", JS_NewCFunction(ctx, js_synth_obj_kill, "kill", 1));
    JS_SetPropertyStr(ctx, snd, "update", JS_NewCFunction(ctx, js_synth_obj_update, "update", 1));

    return snd;
}

// sound.kill(idOrObj, fade) — accepts raw id or synth/sample/replay object
static JSValue js_sound_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1) return JS_UNDEFINED;

    double id_d = 0;
    // Accept either a number (raw id) or an object with .id property
    if (JS_IsNumber(argv[0])) {
        JS_ToFloat64(ctx, &id_d, argv[0]);
    } else if (JS_IsObject(argv[0])) {
        JSValue id_v = JS_GetPropertyStr(ctx, argv[0], "id");
        if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
        JS_FreeValue(ctx, id_v);
    }
    uint64_t id = (uint64_t)id_d;

    double fade = 0.025;
    if (argc >= 2 && JS_IsNumber(argv[1])) {
        JS_ToFloat64(ctx, &fade, argv[1]);
    }

    // Check if it's a sample voice
    int is_sample = 0;
    int is_replay = 0;
    if (JS_IsObject(argv[0])) {
        JSValue is_v = JS_GetPropertyStr(ctx, argv[0], "isSample");
        is_sample = JS_ToBool(ctx, is_v);
        JS_FreeValue(ctx, is_v);
        is_v = JS_GetPropertyStr(ctx, argv[0], "isReplay");
        is_replay = JS_ToBool(ctx, is_v);
        JS_FreeValue(ctx, is_v);
    }

    if (is_replay) {
        audio_replay_kill(audio, id, fade);
    } else if (is_sample) {
        audio_sample_kill(audio, id, fade);
    } else {
        audio_kill(audio, id, fade);
    }
    return JS_UNDEFINED;
}

// sound.update(id, {tone, volume, pan})
static JSValue js_sound_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 2) return JS_UNDEFINED;

    double id_d;
    JS_ToFloat64(ctx, &id_d, argv[0]);
    uint64_t id = (uint64_t)id_d;

    double freq = -1, vol = -1, pan = -3;
    if (JS_IsObject(argv[1])) {
        JSValue v;
        v = JS_GetPropertyStr(ctx, argv[1], "tone");
        if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
        JS_FreeValue(ctx, v);

        v = JS_GetPropertyStr(ctx, argv[1], "volume");
        if (JS_IsNumber(v)) JS_ToFloat64(ctx, &vol, v);
        JS_FreeValue(ctx, v);

        v = JS_GetPropertyStr(ctx, argv[1], "pan");
        if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
        JS_FreeValue(ctx, v);
    }

    audio_update(audio, id, freq, vol, pan);
    return JS_UNDEFINED;
}

// sound.freq(note) — convert note to Hz
static JSValue js_sound_freq(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NewFloat64(ctx, 440.0);

    double f;
    if (JS_IsNumber(argv[0])) {
        JS_ToFloat64(ctx, &f, argv[0]);
        return JS_NewFloat64(ctx, f);
    }

    const char *note = JS_ToCString(ctx, argv[0]);
    f = audio_note_to_freq(note);
    JS_FreeCString(ctx, note);
    return JS_NewFloat64(ctx, f);
}

// sound.room.toggle()
static JSValue js_room_toggle(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt->audio) audio_room_toggle(current_rt->audio);
    return JS_UNDEFINED;
}

// sound.room.setMix(value)
static JSValue js_set_room_mix(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt->audio) return JS_UNDEFINED;
    double v;
    JS_ToFloat64(ctx, &v, argv[0]);
    audio_set_room_mix(current_rt->audio, (float)v);
    return JS_UNDEFINED;
}

// sound.fx.setMix(value) — dry/wet for entire FX chain
static JSValue js_set_fx_mix(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt->audio) return JS_UNDEFINED;
    double v;
    JS_ToFloat64(ctx, &v, argv[0]);
    audio_set_fx_mix(current_rt->audio, (float)v);
    return JS_UNDEFINED;
}

// sound.glitch.toggle()
static JSValue js_glitch_toggle(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt->audio) audio_glitch_toggle(current_rt->audio);
    return JS_UNDEFINED;
}

// sound.glitch.setMix(value)
static JSValue js_set_glitch_mix(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt->audio) return JS_UNDEFINED;
    double v;
    JS_ToFloat64(ctx, &v, argv[0]);
    audio_set_glitch_mix(current_rt->audio, (float)v);
    return JS_UNDEFINED;
}

// sound.microphone.open() — open device + start hot-mic thread
static JSValue js_mic_open(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt->audio) return JS_FALSE;
    int ok = audio_mic_open(current_rt->audio);
    ac_log("[js][mic] open -> %s\n", ok == 0 ? "ok" : "fail");
    return JS_NewBool(ctx, ok == 0);
}

// sound.microphone.close() — stop hot-mic thread + close device
static JSValue js_mic_close(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt->audio) return JS_UNDEFINED;
    audio_mic_close(current_rt->audio);
    ac_log("[js][mic] close\n");
    return JS_UNDEFINED;
}

// sound.microphone.rec() — start buffering (instant, device already open)
static JSValue js_mic_rec(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt->audio) return JS_FALSE;
    int ok = audio_mic_start(current_rt->audio);
    ac_log("[js][mic] rec -> %s\n", ok == 0 ? "ok" : "fail");
    return JS_NewBool(ctx, ok == 0);
}

// sound.microphone.cut() — stop buffering, return sample length
static JSValue js_mic_cut(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt->audio) return JS_NewInt32(ctx, 0);
    int len = audio_mic_stop(current_rt->audio);
    ac_log("[js][mic] cut -> len=%d rate=%u err=%s\n",
           len,
           current_rt->audio->sample_rate,
           current_rt->audio->mic_last_error[0] ? current_rt->audio->mic_last_error : "(none)");
    // Persist sample to disk for next boot
    if (len > 0) {
        int saved = audio_sample_save(current_rt->audio, "/mnt/ac-sample.raw");
        ac_log("[js][mic] sample saved to /mnt/ac-sample.raw (%d samples)\n", saved);
    }
    return JS_NewInt32(ctx, len);
}

// sound.microphone.recording — check if currently recording
static JSValue js_mic_recording(JSContext *ctx, JSValueConst this_val) {
    if (!current_rt->audio) return JS_FALSE;
    return JS_NewBool(ctx, current_rt->audio->recording);
}

// sound.microphone.sampleLength — length of recorded sample
static JSValue js_mic_sample_length(JSContext *ctx, JSValueConst this_val) {
    if (!current_rt->audio) return JS_NewInt32(ctx, 0);
    return JS_NewInt32(ctx, current_rt->audio->sample_len);
}

// sound.microphone.sampleRate — capture rate of recorded sample
static JSValue js_mic_sample_rate(JSContext *ctx, JSValueConst this_val) {
    if (!current_rt->audio) return JS_NewInt32(ctx, 0);
    return JS_NewInt32(ctx, current_rt->audio->sample_rate);
}

// sampleObj.update({tone, base, volume, pan}) — update a playing sample voice
static JSValue js_sample_obj_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    JSValue id_v = JS_GetPropertyStr(ctx, this_val, "id");
    double id_d = 0;
    if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
    JS_FreeValue(ctx, id_v);
    uint64_t id = (uint64_t)id_d;
    if (id == 0) return JS_UNDEFINED;

    double freq = -1, base_freq = -1, vol = -1, pan = -3;
    JSValue v;
    v = JS_GetPropertyStr(ctx, argv[0], "tone");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "base");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &base_freq, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &vol, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    if (freq > 0 && base_freq < 0) base_freq = 261.63; // default C4
    audio_sample_update(audio, id, freq, base_freq, vol, pan);
    return JS_UNDEFINED;
}

// replayObj.update({tone, base, volume, pan}) — update the dedicated replay voice
static JSValue js_replay_obj_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    JSValue id_v = JS_GetPropertyStr(ctx, this_val, "id");
    double id_d = 0;
    if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
    JS_FreeValue(ctx, id_v);
    uint64_t id = (uint64_t)id_d;
    if (id == 0) return JS_UNDEFINED;

    double freq = -1, base_freq = -1, vol = -1, pan = -3;
    JSValue v;
    v = JS_GetPropertyStr(ctx, argv[0], "tone");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "base");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &base_freq, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &vol, v);
    JS_FreeValue(ctx, v);
    v = JS_GetPropertyStr(ctx, argv[0], "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    if (freq > 0 && base_freq < 0) base_freq = 261.63;
    audio_replay_update(audio, id, freq, base_freq, vol, pan);
    return JS_UNDEFINED;
}

// sound.sample.play({tone, base, volume, pan}) — play recorded sample at pitch
static JSValue js_sample_play(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    JSValue opts = argv[0];
    double freq = 261.63, base_freq = 261.63, volume = 0.7, pan = 0.0;
    JSValue v;

    v = JS_GetPropertyStr(ctx, opts, "tone");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "base");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &base_freq, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &volume, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    int loop = 0;
    v = JS_GetPropertyStr(ctx, opts, "loop");
    if (JS_IsBool(v)) loop = JS_ToBool(ctx, v);
    JS_FreeValue(ctx, v);

    uint64_t id = audio_sample_play(audio, freq, base_freq, volume, pan, loop);
    if (id == 0) {
        ac_log("[sample] play FAILED (sample_len=%d)\n", audio->sample_len);
        return JS_UNDEFINED;
    }
    ac_log("[sample] play OK id=%llu freq=%.1f vol=%.2f\n", (unsigned long long)id, freq, volume);

    // Return object with id, update(), isSample
    JSValue snd = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, snd, "id", JS_NewFloat64(ctx, (double)id));
    JS_SetPropertyStr(ctx, snd, "isSample", JS_TRUE);
    JS_SetPropertyStr(ctx, snd, "update", JS_NewCFunction(ctx, js_sample_obj_update, "update", 1));
    return snd;
}

// sound.replay.play({tone, base, volume, pan}) — play dedicated global replay buffer
static JSValue js_replay_play(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1 || !JS_IsObject(argv[0])) return JS_UNDEFINED;

    JSValue opts = argv[0];
    double freq = 261.63, base_freq = 261.63, volume = 0.7, pan = 0.0;
    JSValue v;

    v = JS_GetPropertyStr(ctx, opts, "tone");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &freq, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "base");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &base_freq, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "volume");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &volume, v);
    JS_FreeValue(ctx, v);

    v = JS_GetPropertyStr(ctx, opts, "pan");
    if (JS_IsNumber(v)) JS_ToFloat64(ctx, &pan, v);
    JS_FreeValue(ctx, v);

    int loop = 0;
    v = JS_GetPropertyStr(ctx, opts, "loop");
    if (JS_IsBool(v)) loop = JS_ToBool(ctx, v);
    JS_FreeValue(ctx, v);

    uint64_t id = audio_replay_play(audio, freq, base_freq, volume, pan, loop);
    if (id == 0) return JS_UNDEFINED;

    JSValue snd = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, snd, "id", JS_NewFloat64(ctx, (double)id));
    JS_SetPropertyStr(ctx, snd, "isReplay", JS_TRUE);
    JS_SetPropertyStr(ctx, snd, "update", JS_NewCFunction(ctx, js_replay_obj_update, "update", 1));
    return snd;
}

// sound.sample.kill(idOrObj, fade)
static JSValue js_sample_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1) return JS_UNDEFINED;

    double id_d = 0;
    if (JS_IsNumber(argv[0])) {
        JS_ToFloat64(ctx, &id_d, argv[0]);
    } else if (JS_IsObject(argv[0])) {
        JSValue id_v = JS_GetPropertyStr(ctx, argv[0], "id");
        if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
        JS_FreeValue(ctx, id_v);
    }

    double fade = 0.02;
    if (argc >= 2 && JS_IsNumber(argv[1])) JS_ToFloat64(ctx, &fade, argv[1]);

    audio_sample_kill(audio, (uint64_t)id_d, fade);
    return JS_UNDEFINED;
}

// sound.replay.kill(idOrObj, fade)
static JSValue js_replay_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1) return JS_UNDEFINED;

    double id_d = 0;
    if (JS_IsNumber(argv[0])) {
        JS_ToFloat64(ctx, &id_d, argv[0]);
    } else if (JS_IsObject(argv[0])) {
        JSValue id_v = JS_GetPropertyStr(ctx, argv[0], "id");
        if (JS_IsNumber(id_v)) JS_ToFloat64(ctx, &id_d, id_v);
        JS_FreeValue(ctx, id_v);
    }

    double fade = 0.02;
    if (argc >= 2 && JS_IsNumber(argv[1])) JS_ToFloat64(ctx, &fade, argv[1]);

    audio_replay_kill(audio, (uint64_t)id_d, fade);
    return JS_UNDEFINED;
}

// sound.sample.getData() — returns Float32Array of current sample buffer
// Proper free callback for JS_NewArrayBuffer (3-arg signature, not plain free)
static void js_free_array_buffer(JSRuntime *rt, void *opaque, void *ptr) {
    (void)rt; (void)opaque;
    free(ptr);
}

static JSValue js_sample_get_data(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    ACAudio *audio = current_rt ? current_rt->audio : NULL;
    if (!audio || !audio->sample_buf || audio->sample_len <= 0) return JS_UNDEFINED;

    // Lock to prevent sample_buf pointer swap during copy
    pthread_mutex_lock(&audio->lock);
    int len = audio->sample_len;
    if (len > audio->sample_max_len) len = audio->sample_max_len;
    if (len <= 0) { pthread_mutex_unlock(&audio->lock); return JS_UNDEFINED; }

    size_t byte_len = (size_t)len * sizeof(float);
    float *copy = malloc(byte_len);
    if (!copy) { pthread_mutex_unlock(&audio->lock); return JS_UNDEFINED; }
    memcpy(copy, audio->sample_buf, byte_len);
    pthread_mutex_unlock(&audio->lock);

    // Create ArrayBuffer from our copy
    JSValue ab = JS_NewArrayBuffer(ctx, (uint8_t *)copy, byte_len,
                                   js_free_array_buffer, NULL, 0);
    if (JS_IsException(ab)) { free(copy); return JS_UNDEFINED; }

    // Create Float32Array view
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue ctor = JS_GetPropertyStr(ctx, global, "Float32Array");
    JSValue f32 = JS_CallConstructor(ctx, ctor, 1, &ab);
    JS_FreeValue(ctx, ctor);
    JS_FreeValue(ctx, global);
    JS_FreeValue(ctx, ab);
    return f32;
}

// sound.sample.saveTo(path) — save current sample to a file, returns sample count or -1
static JSValue js_sample_save_to(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->audio || argc < 1) return JS_NewInt32(ctx, -1);
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NewInt32(ctx, -1);
    int result = audio_sample_save(current_rt->audio, path);
    ac_log("[sample] saveTo(%s) -> %d samples\n", path, result);
    JS_FreeCString(ctx, path);
    return JS_NewInt32(ctx, result);
}

// sound.sample.loadFrom(path) — load sample from a file, returns sample count or -1
static JSValue js_sample_load_from(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->audio || argc < 1) return JS_NewInt32(ctx, -1);
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NewInt32(ctx, -1);
    int result = audio_sample_load(current_rt->audio, path);
    ac_log("[sample] loadFrom(%s) -> %d samples\n", path, result);
    JS_FreeCString(ctx, path);
    return JS_NewInt32(ctx, result);
}

// sound.sample.loadData(float32array, rate) — load sample data from JS array
static JSValue js_sample_load_data(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt) return JS_FALSE;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1) return JS_FALSE;

    size_t byte_len = 0;
    size_t byte_off = 0;
    size_t bytes_per = 0;
    JSValue ab = JS_GetTypedArrayBuffer(ctx, argv[0], &byte_off, &byte_len, &bytes_per);
    if (JS_IsException(ab)) return JS_FALSE;

    size_t ab_len = 0;
    uint8_t *ptr = JS_GetArrayBuffer(ctx, &ab_len, ab);
    if (!ptr) { JS_FreeValue(ctx, ab); return JS_FALSE; }

    float *data = (float *)(ptr + byte_off);
    int len = (int)(byte_len / sizeof(float));
    ac_log("[sample] loadData: byte_len=%zu byte_off=%zu bytes_per=%zu ab_len=%zu len=%d\n",
           byte_len, byte_off, bytes_per, ab_len, len);

    unsigned int rate = 48000;
    if (argc >= 2 && JS_IsNumber(argv[1])) {
        double r; JS_ToFloat64(ctx, &r, argv[1]);
        if (r > 0) rate = (unsigned int)r;
    }

    audio_sample_load_data(audio, data, len, rate);
    JS_FreeValue(ctx, ab);  // Free AFTER memcpy — ptr is into this buffer
    return JS_TRUE;
}

// sound.replay.loadData(float32array, rate) — load data into dedicated replay buffer
static JSValue js_replay_load_data(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt) return JS_FALSE;
    ACAudio *audio = current_rt->audio;
    if (!audio || argc < 1) return JS_FALSE;

    size_t byte_len = 0;
    size_t byte_off = 0;
    size_t bytes_per = 0;
    JSValue ab = JS_GetTypedArrayBuffer(ctx, argv[0], &byte_off, &byte_len, &bytes_per);
    if (JS_IsException(ab)) return JS_FALSE;

    size_t ab_len = 0;
    uint8_t *ptr = JS_GetArrayBuffer(ctx, &ab_len, ab);
    if (!ptr) { JS_FreeValue(ctx, ab); return JS_FALSE; }

    float *data = (float *)(ptr + byte_off);
    int len = (int)(byte_len / sizeof(float));

    unsigned int rate = 48000;
    if (argc >= 2 && JS_IsNumber(argv[1])) {
        double r; JS_ToFloat64(ctx, &r, argv[1]);
        if (r > 0) rate = (unsigned int)r;
    }

    audio_replay_load_data(audio, data, len, rate);
    JS_FreeValue(ctx, ab);
    return JS_TRUE;
}

// sound.speaker.getRecentBuffer(seconds) -> { data: Float32Array, rate: number }
static JSValue js_speaker_get_recent_buffer(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt ? current_rt->audio : NULL;
    if (!audio || !audio->output_history_buf || audio->output_history_size <= 0) return JS_NULL;

    double seconds = 1.0;
    if (argc >= 1 && JS_IsNumber(argv[0])) JS_ToFloat64(ctx, &seconds, argv[0]);
    if (seconds <= 0.0) return JS_NULL;

    unsigned int rate_guess = audio->output_history_rate ? audio->output_history_rate : AUDIO_OUTPUT_HISTORY_RATE;
    int want_len = (int)(seconds * (double)rate_guess + 0.5);
    if (want_len < 1) want_len = 1;
    if (want_len > audio->output_history_size) want_len = audio->output_history_size;

    float *copy = malloc((size_t)want_len * sizeof(float));
    if (!copy) return JS_NULL;

    unsigned int actual_rate = 0;
    int len = audio_output_get_recent(audio, copy, want_len, &actual_rate);
    if (len <= 0 || actual_rate == 0) {
        free(copy);
        return JS_NULL;
    }

    size_t byte_len = (size_t)len * sizeof(float);
    JSValue ab = JS_NewArrayBuffer(ctx, (uint8_t *)copy, byte_len,
                                   js_free_array_buffer, NULL, 0);
    if (JS_IsException(ab)) { free(copy); return JS_NULL; }

    JSValue global = JS_GetGlobalObject(ctx);
    JSValue ctor = JS_GetPropertyStr(ctx, global, "Float32Array");
    JSValue f32 = JS_CallConstructor(ctx, ctor, 1, &ab);
    JS_FreeValue(ctx, ctor);
    JS_FreeValue(ctx, global);
    JS_FreeValue(ctx, ab);
    if (JS_IsException(f32)) return JS_NULL;

    JSValue out = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, out, "data", f32);
    JS_SetPropertyStr(ctx, out, "rate", JS_NewInt32(ctx, (int)actual_rate));
    return out;
}

// sound.tape.recording() -> bool
// Returns true while the tape recorder (recorder.c) is capturing.
// Pure read-only state getter for notepat UI.
static JSValue js_tape_recording(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
#ifdef HAVE_AVCODEC
    if (current_rt && current_rt->recorder) {
        return JS_NewBool(ctx, recorder_is_recording((ACRecorder *)current_rt->recorder));
    }
#endif
    return JS_NewBool(ctx, 0);
}

// sound.tape.elapsed() -> number (seconds, 0 if not recording)
static JSValue js_tape_elapsed(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
#ifdef HAVE_AVCODEC
    if (current_rt && current_rt->recorder) {
        return JS_NewFloat64(ctx, recorder_elapsed((ACRecorder *)current_rt->recorder));
    }
#endif
    return JS_NewFloat64(ctx, 0.0);
}

// sound.speak(text)
static JSValue js_speak(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt->tts) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;
    tts_speak(current_rt->tts, text);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

// sound.speakVoice(text, male) — speak with voice selection (0=female, 1=male)
static JSValue js_speak_voice(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2 || !current_rt->tts) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;
    int male = 0;
    JS_ToInt32(ctx, &male, argv[1]);
    tts_speak_voice(current_rt->tts, text, male);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

// sound.speakCached(key) — instant playback of pre-rendered letter/key
static JSValue js_speak_cached(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt->tts) return JS_UNDEFINED;
    const char *key = JS_ToCString(ctx, argv[0]);
    if (!key) return JS_UNDEFINED;
    tts_speak_cached(current_rt->tts, key);
    JS_FreeCString(ctx, key);
    return JS_UNDEFINED;
}

// ============================================================
// Event System
// ============================================================

static JSValue js_event_is(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_FALSE;
    const char *pattern = JS_ToCString(ctx, argv[0]);
    if (!pattern) return JS_FALSE;

    JSValue type_val = JS_GetPropertyStr(ctx, this_val, "_type");
    JSValue key_val = JS_GetPropertyStr(ctx, this_val, "_key");

    int type;
    JS_ToInt32(ctx, &type, type_val);
    const char *key = JS_ToCString(ctx, key_val);

    int match = 0;

    if (strcmp(pattern, "touch") == 0)
        match = (type == AC_EVENT_TOUCH);
    else if (strcmp(pattern, "lift") == 0)
        match = (type == AC_EVENT_LIFT);
    else if (strcmp(pattern, "draw") == 0)
        match = (type == AC_EVENT_DRAW);
    else if (strcmp(pattern, "keyboard:down") == 0)
        match = (type == AC_EVENT_KEYBOARD_DOWN);
    else if (strcmp(pattern, "keyboard:up") == 0)
        match = (type == AC_EVENT_KEYBOARD_UP);
    else if (strncmp(pattern, "keyboard:down:", 14) == 0)
        match = (type == AC_EVENT_KEYBOARD_DOWN && key && strcmp(key, pattern + 14) == 0);
    else if (strncmp(pattern, "keyboard:up:", 12) == 0)
        match = (type == AC_EVENT_KEYBOARD_UP && key && strcmp(key, pattern + 12) == 0);
    else if (strcmp(pattern, "reframed") == 0)
        match = 0; // no resize on bare metal
    else if (strncmp(pattern, "gamepad", 7) == 0)
        match = 0; // stub
    else if (strncmp(pattern, "midi:", 5) == 0)
        match = 0; // stub
    else if (strcmp(pattern, "compose") == 0)
        match = 0; // stub

    JS_FreeCString(ctx, pattern);
    JS_FreeCString(ctx, key);
    JS_FreeValue(ctx, type_val);
    JS_FreeValue(ctx, key_val);

    return JS_NewBool(ctx, match);
}

static JSValue make_event_object(JSContext *ctx, ACEvent *ev) {
    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "_type", JS_NewInt32(ctx, ev->type));
    JS_SetPropertyStr(ctx, obj, "_key", JS_NewString(ctx, ev->key_name));
    JS_SetPropertyStr(ctx, obj, "x", JS_NewInt32(ctx, ev->x));
    JS_SetPropertyStr(ctx, obj, "y", JS_NewInt32(ctx, ev->y));
    JS_SetPropertyStr(ctx, obj, "repeat", JS_NewBool(ctx, ev->repeat));
    JS_SetPropertyStr(ctx, obj, "key", JS_NewString(ctx, ev->key_name));
    JS_SetPropertyStr(ctx, obj, "code", JS_NewString(ctx, ev->key_name));
    // Velocity: 0-127 from analog pressure (0.0-1.0), default 127 for digital keys
    int velocity = ev->pressure > 0.001f ? (int)(ev->pressure * 127.0f) : 127;
    if (velocity < 1 && ev->type == AC_EVENT_KEYBOARD_DOWN) velocity = 1;
    JS_SetPropertyStr(ctx, obj, "velocity", JS_NewInt32(ctx, velocity));
    JS_SetPropertyStr(ctx, obj, "pressure", JS_NewFloat64(ctx, (double)ev->pressure));
    JS_SetPropertyStr(ctx, obj, "name", JS_NewString(ctx, ev->key_name));

    // pointer sub-object
    JSValue pointer = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, pointer, "x", JS_NewInt32(ctx, ev->x));
    JS_SetPropertyStr(ctx, pointer, "y", JS_NewInt32(ctx, ev->y));
    JS_SetPropertyStr(ctx, obj, "pointer", pointer);

    JS_SetPropertyStr(ctx, obj, "is", JS_NewCFunction(ctx, js_event_is, "is", 1));

    return obj;
}

// ============================================================
// num utilities
// ============================================================

static JSValue js_num_clamp(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 3) return JS_UNDEFINED;
    double v, mn, mx;
    JS_ToFloat64(ctx, &v, argv[0]);
    JS_ToFloat64(ctx, &mn, argv[1]);
    JS_ToFloat64(ctx, &mx, argv[2]);
    if (v < mn) v = mn;
    if (v > mx) v = mx;
    return JS_NewFloat64(ctx, v);
}

static JSValue js_num_rand(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    return JS_NewFloat64(ctx, (double)rand() / RAND_MAX);
}

static JSValue js_num_randint(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_NewInt32(ctx, 0);
    int a, b;
    JS_ToInt32(ctx, &a, argv[0]);
    JS_ToInt32(ctx, &b, argv[1]);
    if (b <= a) return JS_NewInt32(ctx, a);
    return JS_NewInt32(ctx, a + rand() % (b - a + 1));
}

// ============================================================
// Module loader for ES module imports
// ============================================================

static JSModuleDef *js_module_loader(JSContext *ctx, const char *module_name, void *opaque) {
    (void)opaque;

    // Check if it's a stub module
    const char *stub_src = NULL;

    if (strstr(module_name, "chord-detection")) {
        stub_src = "export function detectChord() { return null; }";
    } else if (strstr(module_name, "gamepad-diagram")) {
        stub_src = "export function drawMiniControllerDiagram() {}";
    } else if (strstr(module_name, "pixel-sample")) {
        stub_src = "export function decodeBitmapToSample() { return null; }\n"
                   "export function loadPaintingAsAudio() { return null; }";
    } else if (strstr(module_name, "nopaint")) {
        stub_src = "export function nopaint_generateColoredLabel() {}\n"
                   "export function nopaint_boot() {}\n"
                   "export function nopaint_act() {}\n"
                   "export function nopaint_paint() {}\n"
                   "export function nopaint_is(s) { return false; }\n"
                   "export function nopaint_cancelStroke() {}\n"
                   "export function nopaint_adjust() {}\n"
                   "export function nopaint_handleColor(c, ink) { return ink(c); }\n"
                   "export function nopaint_cleanupColor() {}\n"
                   "export function nopaint_parseBrushParams(o) { return {color:[], mode:'fill', thickness:1}; }\n"
                   "export function nopaint_renderPerfHUD() {}\n"
                   "export function nopaint_triggerBakeFlash() {}";
    } else if (strstr(module_name, "color-highlighting")) {
        stub_src = "export function generateNopaintHUDLabel() { return ''; }\n"
                   "export function colorizeColorName() { return ''; }";
    }

    // Try to load the module from the filesystem
    char resolved[512] = {0};

    if (!stub_src) {
        // Try multiple paths
        const char *search_paths[] = {
            module_name,
            NULL
        };

        // If it's a relative path like "../lib/note-colors.mjs", resolve from /
        if (module_name[0] == '.' && module_name[1] == '.') {
            // "../lib/X" → "/lib/X"
            const char *p = module_name + 2;
            while (*p == '/') p++;
            if (*p == '.') { // "../../lib/X"
                p++;
                while (*p == '.') p++;
                while (*p == '/') p++;
            }
            snprintf(resolved, sizeof(resolved), "/%s", p);
        } else {
            snprintf(resolved, sizeof(resolved), "%s", module_name);
        }

        FILE *f = fopen(resolved, "r");
        if (!f) {
            // Try /lib/ prefix
            snprintf(resolved, sizeof(resolved), "/lib/%s",
                     strrchr(module_name, '/') ? strrchr(module_name, '/') + 1 : module_name);
            f = fopen(resolved, "r");
        }
        if (f) {
            fseek(f, 0, SEEK_END);
            long len = ftell(f);
            fseek(f, 0, SEEK_SET);
            char *src = malloc(len + 1);
            fread(src, 1, len, f);
            src[len] = '\0';
            fclose(f);

            JSValue val = JS_Eval(ctx, src, len, module_name,
                                  JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
            free(src);

            if (JS_IsException(val)) {
                JSValue exc = JS_GetException(ctx);
                const char *str = JS_ToCString(ctx, exc);
                fprintf(stderr, "[js] Module load error (%s): %s\n", module_name, str);
                JS_FreeCString(ctx, str);
                JS_FreeValue(ctx, exc);
                return NULL;
            }

            JSModuleDef *m = (JSModuleDef *)JS_VALUE_GET_PTR(val);
            return m;
        }

        // Module not found — create empty stub
        fprintf(stderr, "[js] Module not found, stubbing: %s\n", module_name);
        stub_src = "// stub";
    }

    // Compile stub module
    JSValue val = JS_Eval(ctx, stub_src, strlen(stub_src), module_name,
                          JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
    if (JS_IsException(val)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Stub module error (%s): %s\n", module_name, str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
        return NULL;
    }

    JSModuleDef *m = (JSModuleDef *)JS_VALUE_GET_PTR(val);
    return m;
}

// ============================================================
// Runtime Init / Lifecycle
// ============================================================

// JS init code evaluated before pieces — defines Button, Box, etc.
static const char *js_init_code =
    "globalThis.__Button = class Button {\n"
    "  constructor(x, y, w, h) {\n"
    "    if (typeof x === 'object' && x !== null) {\n"
    "      this.box = { x: x.x || 0, y: x.y || 0, w: x.w || x.width || 0, h: x.h || x.height || 0 };\n"
    "    } else {\n"
    "      this.box = { x: x || 0, y: y || 0, w: w || 0, h: h || 0 };\n"
    "    }\n"
    "    this.down = false;\n"
    "    this.over = false;\n"
    "    this.disabled = false;\n"
    "    this.multitouch = true;\n"
    "  }\n"
    "  get up() { return !this.down; }\n"
    "  set up(v) { this.down = !v; }\n"
    "  paint(fn) { if (!this.disabled && fn) fn(this); }\n"
    "  act(e, callbacks, pens) {\n"
    "    if (this.disabled) return;\n"
    "    if (typeof callbacks === 'function') callbacks = { push: callbacks };\n"
    "    this.actions = callbacks;\n"
    "  }\n"
    "};\n"
    "globalThis.__Box = class Box {\n"
    "  constructor(x, y, w, h) {\n"
    "    if (typeof x === 'object') { this.x = x.x; this.y = x.y; this.w = x.w; this.h = x.h; }\n"
    "    else { this.x = x || 0; this.y = y || 0; this.w = w || 0; this.h = h || 0; }\n"
    "  }\n"
    "  static from(obj) { return new __Box(obj); }\n"
    "};\n"
    "globalThis.Number.isFinite = globalThis.Number.isFinite || function(v) { return typeof v === 'number' && isFinite(v); };\n"
    // num.shiftRGB — lerp between two RGB arrays
    "globalThis.__shiftRGB = function(from, to, t, mode) {\n"
    "  if (!from || !to) return from || [0,0,0];\n"
    "  t = Math.max(0, Math.min(1, t || 0));\n"
    "  return [\n"
    "    Math.round(from[0] + (to[0] - from[0]) * t),\n"
    "    Math.round(from[1] + (to[1] - from[1]) * t),\n"
    "    Math.round(from[2] + (to[2] - from[2]) * t)\n"
    "  ];\n"
    "};\n"
    // num.dist — Euclidean distance
    "globalThis.__dist = function(x1, y1, x2, y2) {\n"
    "  const dx = x2 - x1, dy = y2 - y1;\n"
    "  return Math.sqrt(dx*dx + dy*dy);\n"
    "};\n"
    // num.map — map value from one range to another
    "globalThis.__map = function(v, inMin, inMax, outMin, outMax) {\n"
    "  return outMin + (v - inMin) * (outMax - outMin) / (inMax - inMin);\n"
    "};\n"
    // num.lerp
    "globalThis.__lerp = function(a, b, t) { return a + (b - a) * t; };\n"
    // num.parseColor — parse color from params array (e.g. ["purple"], ["255","0","0"], ["128"])
    "globalThis.__parseColor = function(params) {\n"
    "  if (!params || params.length === 0) return [];\n"
    "  var names = {red:[255,0,0],orange:[255,165,0],yellow:[255,255,0],green:[0,128,0],\n"
    "    cyan:[0,255,255],blue:[0,0,255],purple:[128,0,128],magenta:[255,0,255],\n"
    "    pink:[255,192,203],white:[255,255,255],gray:[128,128,128],grey:[128,128,128],\n"
    "    black:[0,0,0],brown:[139,69,19]};\n"
    "  var first = params[0];\n"
    "  if (typeof first === 'string' && names[first.toLowerCase()]) {\n"
    "    var c = names[first.toLowerCase()].slice();\n"
    "    if (params.length >= 2) c.push(parseInt(params[1]) || 255);\n"
    "    return c;\n"
    "  }\n"
    "  var nums = params.map(function(p){return parseInt(p)}).filter(function(n){return !isNaN(n)});\n"
    "  return nums;\n"
    "};\n"
    // num.randIntArr — array of N random ints in [0, max]
    "globalThis.__randIntArr = function(max, len) {\n"
    "  var a = []; for (var i = 0; i < len; i++) a.push(Math.floor(Math.random() * (max + 1)));\n"
    "  return a;\n"
    "};\n"
    // num.timestamp — returns a timestamp string
    "globalThis.__timestamp = function() { return Date.now().toString(36); };\n"
    // nopaint_generateColoredLabel stub (native doesn't have HUD color highlighting)
    "globalThis.__nopaint_generateColoredLabel = function() {};\n"
    // Typeface constructor stub
    "globalThis.__Typeface = function Typeface(name) {\n"
    "  this.name = name;\n"
    "  this.loaded = false;\n"
    "  this.load = function(preloadFn) { this.loaded = true; return this; };\n"
    "  this.measure = function(text) { return { width: (text||'').length * 8, height: 8 }; };\n"
    "};\n"
    // ── Global theme system ──
    // Auto-switches dark/light based on LA time. All pieces use theme.fg, theme.bg, etc.
    // Supports named presets via __theme.apply(id) that persist via config.json.
    "globalThis.__theme = (function() {\n"
    "  function getLAOffset() {\n"
    "    var d = new Date(), m = d.getUTCMonth(), y = d.getUTCFullYear();\n"
    "    if (m > 2 && m < 10) return 7;\n"
    "    if (m < 2 || m > 10) return 8;\n"
    "    if (m === 2) {\n"
    "      var mar1 = new Date(y, 2, 1), ss = 8 + (7 - mar1.getDay()) % 7;\n"
    "      return d.getUTCDate() > ss || (d.getUTCDate() === ss && d.getUTCHours() >= 10) ? 7 : 8;\n"
    "    }\n"
    "    var nov1 = new Date(y, 10, 1), fs = 1 + (7 - nov1.getDay()) % 7;\n"
    "    return d.getUTCDate() < fs || (d.getUTCDate() === fs && d.getUTCHours() < 9) ? 7 : 8;\n"
    "  }\n"
    "  function getLAHour() {\n"
    "    return (new Date().getUTCHours() - getLAOffset() + 24) % 24;\n"
    "  }\n"
    "  var t = { dark: true, _lastCheck: 0, _overrideId: null, _override: null };\n"
    "  // Theme presets: each has dark and light color overrides\n"
    "  t.presets = {\n"
    "    serious: {\n"
    "      label: 'serious', desc: 'black & white',\n"
    "      dark: { bg:[0,0,0], bgAlt:[10,10,10], bgDim:[0,0,0],\n"
    "              fg:255, fgDim:160, fgMute:90,\n"
    "              bar:[15,15,15], border:[60,60,60],\n"
    "              accent:[128,128,128], ok:[200,200,200], err:[255,100,100],\n"
    "              warn:[200,200,100], link:[180,180,255],\n"
    "              pad:[10,10,10], padSharp:[5,5,5], padLine:[40,40,40],\n"
    "              cursor:[255,255,255] },\n"
    "      light: { bg:[255,255,255], bgAlt:[245,245,245], bgDim:[235,235,235],\n"
    "               fg:0, fgDim:80, fgMute:160,\n"
    "               bar:[240,240,240], border:[180,180,180],\n"
    "               accent:[100,100,100], ok:[40,40,40], err:[180,40,40],\n"
    "               warn:[120,100,20], link:[40,40,180],\n"
    "               pad:[245,245,245], padSharp:[230,230,230], padLine:[200,200,200],\n"
    "               cursor:[0,0,0] }\n"
    "    },\n"
    "    neo: {\n"
    "      label: 'neo', desc: 'lime & black',\n"
    "      dark: { bg:[0,0,0], bgAlt:[5,10,5], bgDim:[0,0,0],\n"
    "              fg:200, fgDim:120, fgMute:60,\n"
    "              bar:[5,15,5], border:[0,80,0],\n"
    "              accent:[0,200,80], ok:[0,255,0], err:[255,50,50],\n"
    "              warn:[200,255,0], link:[0,180,255],\n"
    "              pad:[5,10,5], padSharp:[0,5,0], padLine:[0,50,0],\n"
    "              cursor:[0,255,80] },\n"
    "      light: { bg:[220,255,220], bgAlt:[230,255,230], bgDim:[200,240,200],\n"
    "               fg:10, fgDim:60, fgMute:120,\n"
    "               bar:[200,240,200], border:[100,180,100],\n"
    "               accent:[0,140,60], ok:[0,120,40], err:[180,30,30],\n"
    "               warn:[120,140,0], link:[0,80,180],\n"
    "               pad:[210,245,210], padSharp:[190,230,190], padLine:[140,200,140],\n"
    "               cursor:[0,120,40] }\n"
    "    },\n"
    "    ember: {\n"
    "      label: 'ember', desc: 'warm amber',\n"
    "      dark: { bg:[20,12,8], bgAlt:[28,18,12], bgDim:[14,8,5],\n"
    "              fg:220, fgDim:150, fgMute:90,\n"
    "              bar:[35,20,12], border:[60,35,20],\n"
    "              accent:[255,140,40], ok:[120,220,80], err:[255,70,50],\n"
    "              warn:[255,200,60], link:[255,180,100],\n"
    "              pad:[28,18,12], padSharp:[18,10,6], padLine:[55,35,22],\n"
    "              cursor:[255,120,30] },\n"
    "      light: { bg:[255,245,230], bgAlt:[255,250,240], bgDim:[245,235,218],\n"
    "               fg:40, fgDim:90, fgMute:140,\n"
    "               bar:[245,232,215], border:[210,190,160],\n"
    "               accent:[200,100,20], ok:[40,140,50], err:[190,40,30],\n"
    "               warn:[180,120,20], link:[180,90,20],\n"
    "               pad:[250,240,225], padSharp:[238,225,208], padLine:[220,200,175],\n"
    "               cursor:[200,90,15] }\n"
    "    }\n"
    "  };\n"
    "  // Apply a named preset (or 'default' to clear override)\n"
    "  t.apply = function(id) {\n"
    "    if (!id || id === 'default') {\n"
    "      t._overrideId = null;\n"
    "      t._override = null;\n"
    "    } else if (t.presets[id]) {\n"
    "      t._overrideId = id;\n"
    "      t._override = t.presets[id];\n"
    "    }\n"
    "    t._lastCheck = 0;\n"
    "    t.update();\n"
    "  };\n"
    "  t.update = function() {\n"
    "    var now = Date.now();\n"
    "    if (now - t._lastCheck < 5000) return t;\n"
    "    t._lastCheck = now;\n"
    "    var h = getLAHour();\n"
    "    t.dark = (t._forceDark !== undefined) ? !!t._forceDark : (h >= 20 || h < 7);\n"
    "    t.hour = h;\n"
    "    // Backgrounds\n"
    "    t.bg     = t.dark ? [20, 20, 25]    : [240, 238, 232];\n"
    "    t.bgAlt  = t.dark ? [28, 28, 30]    : [250, 248, 244];\n"
    "    t.bgDim  = t.dark ? [15, 15, 18]    : [230, 228, 222];\n"
    "    // Foregrounds\n"
    "    t.fg     = t.dark ? 220             : 40;\n"
    "    t.fgDim  = t.dark ? 140             : 100;\n"
    "    t.fgMute = t.dark ? 80              : 150;\n"
    "    // UI elements\n"
    "    t.bar    = t.dark ? [35, 20, 30]    : [225, 220, 215];\n"
    "    t.border = t.dark ? [55, 35, 45]    : [200, 195, 190];\n"
    "    t.accent = t.dark ? [200, 100, 140] : [180, 60, 120];\n"
    "    t.ok     = t.dark ? [80, 255, 120]  : [30, 160, 60];\n"
    "    t.err    = t.dark ? [255, 85, 85]   : [200, 40, 40];\n"
    "    t.warn   = t.dark ? [255, 200, 60]  : [180, 120, 20];\n"
    "    t.link   = t.dark ? [120, 200, 255] : [40, 100, 200];\n"
    "    // Pad colors (for notepat-style grids)\n"
    "    t.pad       = t.dark ? [28, 28, 30]  : [250, 248, 244];\n"
    "    t.padSharp  = t.dark ? [18, 18, 20]  : [235, 232, 228];\n"
    "    t.padLine   = t.dark ? [50, 50, 55]  : [210, 205, 200];\n"
    "    // Cursor\n"
    "    t.cursor = t.dark ? [220, 80, 140]  : [180, 50, 110];\n"
    "    // Apply preset override if active\n"
    "    if (t._override) {\n"
    "      var m = t.dark ? t._override.dark : t._override.light;\n"
    "      if (m) { for (var k in m) { t[k] = m[k]; } }\n"
    "    }\n"
    "    return t;\n"
    "  };\n"
    "  t.update();\n"
    "  return t;\n"
    "})();\n"
    ;

ACRuntime *js_init(ACGraph *graph, ACInput *input, ACAudio *audio, ACWifi *wifi, ACTts *tts) {
    ACRuntime *rt = calloc(1, sizeof(ACRuntime));
    if (!rt) return NULL;

    rt->graph = graph;
    rt->input = input;
    rt->audio = audio;
    rt->wifi = wifi;
    rt->tts = tts;
    rt->ws = ws_create();
    rt->udp = udp_create();
    rt->boot_fn = JS_UNDEFINED;
    rt->paint_fn = JS_UNDEFINED;
    rt->act_fn = JS_UNDEFINED;
    rt->sim_fn = JS_UNDEFINED;
    rt->leave_fn = JS_UNDEFINED;
    rt->beat_fn = JS_UNDEFINED;

    // Initialize 3D camera
    camera3d_init(&rt->camera3d);

    rt->rt = JS_NewRuntime();
    rt->ctx = JS_NewContext(rt->rt);

    // Register Form and Painting classes
    JS_NewClassID(&form_class_id);
    JS_NewClass(rt->rt, form_class_id, &form_class_def);
    JS_NewClassID(&painting_class_id);
    JS_NewClass(rt->rt, painting_class_id, &painting_class_def);

    // Set module loader
    JS_SetModuleLoaderFunc(rt->rt, NULL, js_module_loader, NULL);

    current_rt = rt;

    JSContext *ctx = rt->ctx;
    JSValue global = JS_GetGlobalObject(ctx);

    // Create the chainable paint API object (with 3D .form() and .ink() support)
    JSValue paint_api = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, paint_api, "box", JS_NewCFunction(ctx, js_box, "box", 5));
    JS_SetPropertyStr(ctx, paint_api, "line", JS_NewCFunction(ctx, js_line, "line", 4));
    JS_SetPropertyStr(ctx, paint_api, "circle", JS_NewCFunction(ctx, js_circle, "circle", 4));
    JS_SetPropertyStr(ctx, paint_api, "plot", JS_NewCFunction(ctx, js_plot, "plot", 2));
    JS_SetPropertyStr(ctx, paint_api, "write", JS_NewCFunction(ctx, js_chain_write, "write", 6));
    JS_SetPropertyStr(ctx, paint_api, "scroll", JS_NewCFunction(ctx, js_scroll, "scroll", 2));
    JS_SetPropertyStr(ctx, paint_api, "blur", JS_NewCFunction(ctx, js_blur, "blur", 1));
    JS_SetPropertyStr(ctx, paint_api, "zoom", JS_NewCFunction(ctx, js_zoom, "zoom", 1));
    JS_SetPropertyStr(ctx, paint_api, "contrast", JS_NewCFunction(ctx, js_contrast, "contrast", 1));
    JS_SetPropertyStr(ctx, paint_api, "spin", JS_NewCFunction(ctx, js_spin, "spin", 1));
    JS_SetPropertyStr(ctx, paint_api, "qr", JS_NewCFunction(ctx, js_qr, "qr", 4));
    // 3D chain methods
    JS_SetPropertyStr(ctx, paint_api, "form", JS_NewCFunction(ctx, js_chain_form, "form", 1));
    JS_SetPropertyStr(ctx, paint_api, "ink", JS_NewCFunction(ctx, js_chain_ink, "ink", 4));
    // pppline — polyline through array of {x,y} points (used by line.mjs for 1px strokes)
    {
        const char *pppline_src =
            "(function(pts) {\n"
            "  if (!pts || pts.length < 2) return;\n"
            "  for (var i = 0; i < pts.length - 1; i++)\n"
            "    line(pts[i].x, pts[i].y, pts[i+1].x, pts[i+1].y);\n"
            "})";
        JSValue pppline_fn = JS_Eval(ctx, pppline_src, strlen(pppline_src), "<pppline>", JS_EVAL_TYPE_GLOBAL);
        JS_SetPropertyStr(ctx, paint_api, "pppline", pppline_fn);
    }
    JS_SetPropertyStr(ctx, global, "__paintApi", JS_DupValue(ctx, paint_api));
    JS_FreeValue(ctx, paint_api);

    // Register top-level graphics functions
    JS_SetPropertyStr(ctx, global, "wipe", JS_NewCFunction(ctx, js_wipe, "wipe", 3));
    JS_SetPropertyStr(ctx, global, "ink", JS_NewCFunction(ctx, js_ink, "ink", 4));
    JS_SetPropertyStr(ctx, global, "line", JS_NewCFunction(ctx, js_line, "line", 5));
    JS_SetPropertyStr(ctx, global, "box", JS_NewCFunction(ctx, js_box, "box", 5));
    JS_SetPropertyStr(ctx, global, "circle", JS_NewCFunction(ctx, js_circle, "circle", 4));
    JS_SetPropertyStr(ctx, global, "qr", JS_NewCFunction(ctx, js_qr, "qr", 4));
    JS_SetPropertyStr(ctx, global, "plot", JS_NewCFunction(ctx, js_plot, "plot", 2));
    JS_SetPropertyStr(ctx, global, "write", JS_NewCFunction(ctx, js_write, "write", 6));
    JS_SetPropertyStr(ctx, global, "scroll", JS_NewCFunction(ctx, js_scroll, "scroll", 2));
    JS_SetPropertyStr(ctx, global, "blur", JS_NewCFunction(ctx, js_blur, "blur", 1));
    JS_SetPropertyStr(ctx, global, "zoom", JS_NewCFunction(ctx, js_zoom, "zoom", 1));
    JS_SetPropertyStr(ctx, global, "contrast", JS_NewCFunction(ctx, js_contrast, "contrast", 1));
    JS_SetPropertyStr(ctx, global, "spin", JS_NewCFunction(ctx, js_spin, "spin", 1));

    // console.log
    JSValue console_obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, console_obj, "log", JS_NewCFunction(ctx, js_console_log, "log", 1));
    JS_SetPropertyStr(ctx, console_obj, "warn", JS_NewCFunction(ctx, js_console_log, "warn", 1));
    JS_SetPropertyStr(ctx, console_obj, "error", JS_NewCFunction(ctx, js_console_log, "error", 1));
    JS_SetPropertyStr(ctx, global, "console", console_obj);

    // performance.now()
    {
        JSValue perf = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, perf, "now", JS_NewCFunction(ctx, js_performance_now, "now", 0));
        JS_SetPropertyStr(ctx, global, "performance", perf);
    }

    // Run init JS code (Button, Box classes)
    JSValue init_result = JS_Eval(ctx, js_init_code, strlen(js_init_code), "<init>", JS_EVAL_TYPE_GLOBAL);
    if (JS_IsException(init_result)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Init code error: %s\n", str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
    }
    JS_FreeValue(ctx, init_result);

    // Browser API stubs for KidLisp evaluator compatibility
    static const char *browser_stubs =
        "if(typeof window==='undefined'){"
        "  globalThis.window={"
        "    location:{href:'',origin:'',hostname:'localhost',pathname:'/',search:'',hash:''},"
        "    history:{pushState:function(){},replaceState:function(){}},"
        "    matchMedia:function(){return{matches:false,addEventListener:function(){}}},"
        "    addEventListener:function(){},"
        "    postMessage:function(){},"
        "    navigator:{userAgent:'ac-native'},"
        "    document:{createElement:function(){return{style:{},getContext:function(){return{}},setAttribute:function(){}}}},"
        "    origin:''"
        "  };"
        "  globalThis.document=window.document;"
        "  globalThis.navigator=window.navigator;"
        "  globalThis.localStorage={_s:{},getItem:function(k){return this._s[k]||null},setItem:function(k,v){this._s[k]=v},removeItem:function(k){delete this._s[k]}};"
        "  globalThis.fetch=function(){return Promise.resolve({ok:false,json:function(){return Promise.resolve({})}})};"
        "  globalThis.indexedDB=null;"
        "  globalThis.requestAnimationFrame=function(cb){cb(performance.now());return 0};"
        "  globalThis.cancelAnimationFrame=function(){};"
        "  globalThis.Image=function(){this.src='';this.onload=null};"
        "}";
    JSValue stubs_result = JS_Eval(ctx, browser_stubs, strlen(browser_stubs), "<browser-stubs>", JS_EVAL_TYPE_GLOBAL);
    if (JS_IsException(stubs_result)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Browser stubs error: %s\n", str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
    }
    JS_FreeValue(ctx, stubs_result);

    // Load KidLisp evaluator bundle (IIFE → globalThis.KidLispModule)
    FILE *kl_f = fopen("/jslib/kidlisp-bundle.js", "r");
    if (kl_f) {
        fseek(kl_f, 0, SEEK_END);
        long kl_len = ftell(kl_f);
        fseek(kl_f, 0, SEEK_SET);
        char *kl_src = malloc(kl_len + 1);
        fread(kl_src, 1, kl_len, kl_f);
        kl_src[kl_len] = '\0';
        fclose(kl_f);
        JSValue kl_result = JS_Eval(ctx, kl_src, kl_len, "<kidlisp-bundle>", JS_EVAL_TYPE_GLOBAL);
        free(kl_src);
        if (JS_IsException(kl_result)) {
            JSValue exc = JS_GetException(ctx);
            const char *str = JS_ToCString(ctx, exc);
            fprintf(stderr, "[js] KidLisp bundle error: %s\n", str);
            JS_FreeCString(ctx, str);
            JS_FreeValue(ctx, exc);
        } else {
            fprintf(stderr, "[js] KidLisp evaluator loaded (%ld bytes)\n", kl_len);
        }
        JS_FreeValue(ctx, kl_result);
    } else {
        fprintf(stderr, "[js] KidLisp bundle not found at /jslib/kidlisp-bundle.js (optional)\n");
    }

    // Load FPS system bundle (Camera + Dolly + CamDoll as globalThis.__FpsSystem)
    // so pieces that export `system = "fps"` (arena.mjs etc.) can be wrapped
    // synchronously in the piece-load shim without async module resolution.
    FILE *fps_f = fopen("/jslib/fps-system-bundle.js", "r");
    if (fps_f) {
        fseek(fps_f, 0, SEEK_END);
        long fps_len = ftell(fps_f);
        fseek(fps_f, 0, SEEK_SET);
        char *fps_src = malloc(fps_len + 1);
        fread(fps_src, 1, fps_len, fps_f);
        fps_src[fps_len] = '\0';
        fclose(fps_f);
        JSValue fps_result = JS_Eval(ctx, fps_src, fps_len, "<fps-system-bundle>", JS_EVAL_TYPE_GLOBAL);
        free(fps_src);
        if (JS_IsException(fps_result)) {
            JSValue exc = JS_GetException(ctx);
            const char *str = JS_ToCString(ctx, exc);
            fprintf(stderr, "[js] FPS bundle error: %s\n", str);
            JS_FreeCString(ctx, str);
            JS_FreeValue(ctx, exc);
        } else {
            fprintf(stderr, "[js] FPS system loaded (%ld bytes)\n", fps_len);
        }
        JS_FreeValue(ctx, fps_result);
    } else {
        fprintf(stderr, "[js] FPS bundle not found at /jslib/fps-system-bundle.js (optional)\n");
    }

    JS_FreeValue(ctx, global);
    return rt;
}

// painting(w, h, callback) — create a real off-screen framebuffer for textures
// Calls the callback with a paint API that draws into the off-screen buffer
static JSValue js_painting(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int w = 100, h = 100;
    if (argc >= 1) JS_ToInt32(ctx, &w, argv[0]);
    if (argc >= 2) JS_ToInt32(ctx, &h, argv[1]);

    // Create a real off-screen framebuffer
    ACFramebuffer *tex_fb = fb_create(w, h);
    if (!tex_fb) return JS_EXCEPTION;

    // Create JS painting object with opaque ACFramebuffer*
    JSValue painting = JS_NewObjectClass(ctx, painting_class_id);
    JS_SetOpaque(painting, tex_fb);
    JS_SetPropertyStr(ctx, painting, "width", JS_NewInt32(ctx, w));
    JS_SetPropertyStr(ctx, painting, "height", JS_NewInt32(ctx, h));

    // Expose pixels as a Uint8Array view into the framebuffer memory.
    // Nopaint uses buffer.pixels[i] to adjust alpha during bake.
    // Note: pixel format is ARGB32 (native byte order).
    {
        int byte_len = tex_fb->stride * h * 4;
        JSValue ab = JS_NewArrayBuffer(ctx, (uint8_t *)tex_fb->pixels, byte_len,
            NULL, NULL, 0); // No free — painting finalizer handles the framebuffer
        // Construct Uint8Array from the ArrayBuffer via JS eval
        JSValue global = JS_GetGlobalObject(ctx);
        JSValue uint8_ctor = JS_GetPropertyStr(ctx, global, "Uint8Array");
        JSValue pixels = JS_CallConstructor(ctx, uint8_ctor, 1, &ab);
        JS_SetPropertyStr(ctx, painting, "pixels", pixels);
        JS_FreeValue(ctx, uint8_ctor);
        JS_FreeValue(ctx, global);
        JS_FreeValue(ctx, ab);
    }

    // If callback provided, render into the off-screen buffer
    if (argc >= 3 && JS_IsFunction(ctx, argv[2])) {
        // Save current render target and switch to off-screen
        ACFramebuffer *saved_fb = current_rt->graph->fb;
        graph_page(current_rt->graph, tex_fb);

        JSValue global = JS_GetGlobalObject(ctx);
        JSValue paint_api = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, paint_api, "wipe", JS_GetPropertyStr(ctx, global, "wipe"));
        JS_SetPropertyStr(ctx, paint_api, "ink", JS_GetPropertyStr(ctx, global, "ink"));
        JS_SetPropertyStr(ctx, paint_api, "box", JS_GetPropertyStr(ctx, global, "box"));
        JS_SetPropertyStr(ctx, paint_api, "line", JS_GetPropertyStr(ctx, global, "line"));
        JS_SetPropertyStr(ctx, paint_api, "circle", JS_GetPropertyStr(ctx, global, "circle"));
        JS_SetPropertyStr(ctx, paint_api, "plot", JS_GetPropertyStr(ctx, global, "plot"));
        JS_SetPropertyStr(ctx, paint_api, "write", JS_GetPropertyStr(ctx, global, "write"));
        JS_SetPropertyStr(ctx, paint_api, "kidlisp", JS_NewCFunction(ctx, js_noop, "kidlisp", 5));
        JS_FreeValue(ctx, global);

        JSValue result = JS_Call(ctx, argv[2], JS_UNDEFINED, 1, &paint_api);
        JS_FreeValue(ctx, result);
        JS_FreeValue(ctx, paint_api);

        // Restore previous render target
        graph_page(current_rt->graph, saved_fb);
    }

    return painting;
}

// page(painting) — switch render target to a painting buffer (or back to screen)
// page(painting) returns an object with wipe/ink/box/line/etc that draw into that buffer.
// In AC web, page() returns a chainable proxy. Here we just switch the graph target.
static JSValue js_page(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->graph) return JS_UNDEFINED;

    if (argc < 1 || JS_IsUndefined(argv[0]) || JS_IsNull(argv[0])) {
        // page() with no args or page(screen) — restore screen target
        graph_page(current_rt->graph, current_rt->graph->screen);
    } else {
        // page(painting) — switch to painting buffer
        ACFramebuffer *fb = JS_GetOpaque(argv[0], painting_class_id);
        if (fb) graph_page(current_rt->graph, fb);
    }

    // Return a page proxy object with wipe() so callers can do page(buf).wipe(...)
    JSValue proxy = JS_NewObject(ctx);
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, proxy, "wipe", JS_GetPropertyStr(ctx, global, "wipe"));
    JS_SetPropertyStr(ctx, proxy, "ink", JS_GetPropertyStr(ctx, global, "ink"));
    JS_SetPropertyStr(ctx, proxy, "box", JS_GetPropertyStr(ctx, global, "box"));
    JS_SetPropertyStr(ctx, proxy, "line", JS_GetPropertyStr(ctx, global, "line"));
    JS_SetPropertyStr(ctx, proxy, "circle", JS_GetPropertyStr(ctx, global, "circle"));
    JS_SetPropertyStr(ctx, proxy, "plot", JS_GetPropertyStr(ctx, global, "plot"));
    JS_SetPropertyStr(ctx, proxy, "write", JS_GetPropertyStr(ctx, global, "write"));
    JS_FreeValue(ctx, global);
    return proxy;
}

// paste(painting, dx?, dy?) — alpha-composite a painting onto the current render target
static JSValue js_paste(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->graph) return JS_UNDEFINED;
    if (argc < 1) return JS_UNDEFINED;

    ACFramebuffer *src = JS_GetOpaque(argv[0], painting_class_id);
    if (!src) return JS_UNDEFINED;

    int dx = 0, dy = 0;
    if (argc >= 2) JS_ToInt32(ctx, &dx, argv[1]);
    if (argc >= 3) JS_ToInt32(ctx, &dy, argv[2]);

    graph_paste(current_rt->graph, src, dx, dy);
    return JS_UNDEFINED;
}

// sound.bpm(val?) — get or set BPM, returns current value
static JSValue js_sound_bpm(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    ACAudio *audio = current_rt->audio;
    if (!audio) return JS_NewFloat64(ctx, 120.0);
    if (argc >= 1 && JS_IsNumber(argv[0])) {
        double val;
        JS_ToFloat64(ctx, &val, argv[0]);
        if (val > 0) audio->bpm = val;
    }
    return JS_NewFloat64(ctx, audio->bpm);
}

// --- DJ deck bindings ---

static void blit_argb_nearest(ACFramebuffer *fb, const uint32_t *src,
                              int sw, int sh, int dx, int dy, int dw, int dh) {
    if (!fb || !src || sw <= 0 || sh <= 0 || dw <= 0 || dh <= 0) return;

    int x0 = dx < 0 ? 0 : dx;
    int y0 = dy < 0 ? 0 : dy;
    int x1 = dx + dw;
    int y1 = dy + dh;
    if (x1 > fb->width) x1 = fb->width;
    if (y1 > fb->height) y1 = fb->height;
    if (x0 >= x1 || y0 >= y1) return;

    for (int y = y0; y < y1; y++) {
        int sy = (int)(((int64_t)(y - dy) * sh) / dh);
        if (sy < 0) sy = 0;
        if (sy >= sh) sy = sh - 1;
        const uint32_t *src_row = src + sy * sw;
        uint32_t *dst_row = fb->pixels + y * fb->stride;
        for (int x = x0; x < x1; x++) {
            int sx = (int)(((int64_t)(x - dx) * sw) / dw);
            if (sx < 0) sx = 0;
            if (sx >= sw) sx = sw - 1;
            uint32_t px = src_row[sx];
            if ((px >> 24) == 0) continue;
            dst_row[x] = px;
        }
    }
}

// sound.deck.load(deck, path) -> true/false
static JSValue js_deck_load(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2 || !current_rt || !current_rt->audio) return JS_FALSE;
    int deck;
    JS_ToInt32(ctx, &deck, argv[0]);
    const char *path = JS_ToCString(ctx, argv[1]);
    if (!path) return JS_FALSE;
    int ret = audio_deck_load(current_rt->audio, deck, path);
    JS_FreeCString(ctx, path);
    return JS_NewBool(ctx, ret == 0);
}

// sound.deck.play(deck)
static JSValue js_deck_play(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    audio_deck_play(current_rt->audio, deck);
    return JS_UNDEFINED;
}

// sound.deck.pause(deck)
static JSValue js_deck_pause(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    audio_deck_pause(current_rt->audio, deck);
    return JS_UNDEFINED;
}

// sound.deck.seek(deck, seconds)
static JSValue js_deck_seek(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    double sec; JS_ToFloat64(ctx, &sec, argv[1]);
    audio_deck_seek(current_rt->audio, deck, sec);
    return JS_UNDEFINED;
}

// sound.deck.setSpeed(deck, speed)
static JSValue js_deck_set_speed(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    double spd; JS_ToFloat64(ctx, &spd, argv[1]);
    audio_deck_set_speed(current_rt->audio, deck, spd);
    return JS_UNDEFINED;
}

// sound.deck.getPeaks(deck) — returns Float32Array of normalized peak amplitudes
static JSValue js_deck_get_peaks(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_NULL;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    if (deck < 0 || deck >= AUDIO_MAX_DECKS) return JS_NULL;
    ACDeck *dk = &current_rt->audio->decks[deck];
    if (!dk->decoder || !dk->decoder->peaks || dk->decoder->peak_count <= 0) return JS_NULL;

    // Build a JS array from the peak data
    JSValue arr = JS_NewArray(ctx);
    for (int i = 0; i < dk->decoder->peak_count; i++) {
        JS_SetPropertyUint32(ctx, arr, i, JS_NewFloat64(ctx, dk->decoder->peaks[i]));
    }
    return arr;
}

// sound.deck.prepareVideo(deck[, width, height, fps]) — decode a low-res preview strip
static JSValue js_deck_prepare_video(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_FALSE;
    int deck = 0, width = 96, height = 54, fps = 12;
    JS_ToInt32(ctx, &deck, argv[0]);
    if (argc > 1) JS_ToInt32(ctx, &width, argv[1]);
    if (argc > 2) JS_ToInt32(ctx, &height, argv[2]);
    if (argc > 3) JS_ToInt32(ctx, &fps, argv[3]);
    if (deck < 0 || deck >= AUDIO_MAX_DECKS) return JS_FALSE;
    ACDeck *dk = &current_rt->audio->decks[deck];
    if (!dk->decoder) return JS_FALSE;
    int ret = deck_decoder_generate_video_preview(dk->decoder, width, height, fps);
    return JS_NewBool(ctx, ret > 0);
}

// sound.deck.videoBlit(deck, x, y, w, h) — draw current preview frame into the framebuffer
static JSValue js_deck_video_blit(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 5 || !current_rt || !current_rt->audio || !current_rt->graph || !current_rt->graph->fb)
        return JS_FALSE;

    int deck = 0, x = 0, y = 0, w = 0, h = 0;
    JS_ToInt32(ctx, &deck, argv[0]);
    JS_ToInt32(ctx, &x, argv[1]);
    JS_ToInt32(ctx, &y, argv[2]);
    JS_ToInt32(ctx, &w, argv[3]);
    JS_ToInt32(ctx, &h, argv[4]);
    if (deck < 0 || deck >= AUDIO_MAX_DECKS) return JS_FALSE;

    ACDeck *dk = &current_rt->audio->decks[deck];
    if (!dk->decoder || !dk->decoder->video_ready || !dk->decoder->video_frames ||
        dk->decoder->video_frame_count <= 0 || dk->decoder->video_width <= 0 ||
        dk->decoder->video_height <= 0 || dk->decoder->video_fps <= 0.0) {
        return JS_FALSE;
    }

    if (w <= 0) w = dk->decoder->video_width;
    if (h <= 0) h = dk->decoder->video_height;

    int idx = (int)floor(dk->decoder->position * dk->decoder->video_fps + 0.0001);
    if (idx < 0) idx = 0;
    if (idx >= dk->decoder->video_frame_count) idx = dk->decoder->video_frame_count - 1;

    size_t pixels_per_frame = (size_t)dk->decoder->video_width * (size_t)dk->decoder->video_height;
    const uint32_t *src = dk->decoder->video_frames + ((size_t)idx * pixels_per_frame);
    blit_argb_nearest(current_rt->graph->fb, src,
                      dk->decoder->video_width, dk->decoder->video_height,
                      x, y, w, h);
    return JS_TRUE;
}

// sound.deck.setVolume(deck, vol)
static JSValue js_deck_set_volume(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    int deck; JS_ToInt32(ctx, &deck, argv[0]);
    double vol; JS_ToFloat64(ctx, &vol, argv[1]);
    audio_deck_set_volume(current_rt->audio, deck, (float)vol);
    return JS_UNDEFINED;
}

// sound.deck.setCrossfader(value)
static JSValue js_deck_set_crossfader(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    double val; JS_ToFloat64(ctx, &val, argv[0]);
    audio_deck_set_crossfader(current_rt->audio, (float)val);
    return JS_UNDEFINED;
}

// sound.deck.setMasterVolume(value)
static JSValue js_deck_set_master_vol(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->audio) return JS_UNDEFINED;
    double val; JS_ToFloat64(ctx, &val, argv[0]);
    audio_deck_set_master_volume(current_rt->audio, (float)val);
    return JS_UNDEFINED;
}

// Build the sound object for the API
static JSValue build_sound_obj(JSContext *ctx, ACRuntime *rt) {
    JSValue sound = JS_NewObject(ctx);

    // Core synthesis functions
    JS_SetPropertyStr(ctx, sound, "synth", JS_NewCFunction(ctx, js_synth, "synth", 1));
    JS_SetPropertyStr(ctx, sound, "freq", JS_NewCFunction(ctx, js_sound_freq, "freq", 1));
    JS_SetPropertyStr(ctx, sound, "play", JS_NewCFunction(ctx, js_noop, "play", 2));
    JS_SetPropertyStr(ctx, sound, "kill", JS_NewCFunction(ctx, js_sound_kill, "kill", 2));
    JS_SetPropertyStr(ctx, sound, "update", JS_NewCFunction(ctx, js_sound_update, "update", 2));
    JS_SetPropertyStr(ctx, sound, "bpm", JS_NewCFunction(ctx, js_sound_bpm, "bpm", 1));
    JS_SetPropertyStr(ctx, sound, "time", JS_NewFloat64(ctx, rt->audio ? rt->audio->time : 0.0));
    JS_SetPropertyStr(ctx, sound, "registerSample", JS_NewCFunction(ctx, js_noop, "registerSample", 3));

    // speaker sub-object
    JSValue speaker = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, speaker, "poll", JS_NewCFunction(ctx, js_noop, "poll", 0));
    JS_SetPropertyStr(ctx, speaker, "getRecentBuffer",
        JS_NewCFunction(ctx, js_speaker_get_recent_buffer, "getRecentBuffer", 1));
    JS_SetPropertyStr(ctx, speaker, "sampleRate",
        JS_NewInt32(ctx, rt->audio ? (int)rt->audio->actual_rate : AUDIO_SAMPLE_RATE));

    // sound.tape — read-only tape recorder state. The actual start/stop
    // is driven by the PrintScreen key handler in ac-native.c (which
    // owns the MP4 file lifecycle, TTS announce, on-screen overlay, and
    // cloud upload). JS just observes.
    JSValue tape = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, tape, "recording",
        JS_NewCFunction(ctx, js_tape_recording, "recording", 0));
    JS_SetPropertyStr(ctx, tape, "elapsed",
        JS_NewCFunction(ctx, js_tape_elapsed, "elapsed", 0));
    JS_SetPropertyStr(ctx, sound, "tape", tape);

    // waveforms (32 samples, left channel only — sufficient for visualizer)
    JSValue waveforms = JS_NewObject(ctx);
    JSValue wf_left = JS_NewArray(ctx);
    if (rt->audio) {
        for (int i = 0; i < 32; i++) {
            int idx = (rt->audio->waveform_pos - 32 + i + AUDIO_WAVEFORM_SIZE) % AUDIO_WAVEFORM_SIZE;
            JS_SetPropertyUint32(ctx, wf_left, i, JS_NewFloat64(ctx, rt->audio->waveform_left[idx]));
        }
    }
    JS_SetPropertyStr(ctx, waveforms, "left", wf_left);
    JS_SetPropertyStr(ctx, speaker, "waveforms", waveforms);

    // amplitudes
    JSValue amplitudes = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, amplitudes, "left", JS_NewFloat64(ctx, rt->audio ? rt->audio->amplitude_left : 0.0));
    JS_SetPropertyStr(ctx, amplitudes, "right", JS_NewFloat64(ctx, rt->audio ? rt->audio->amplitude_right : 0.0));
    JS_SetPropertyStr(ctx, speaker, "amplitudes", amplitudes);

    // frequencies (stub)
    JSValue frequencies = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, frequencies, "left", JS_NewArray(ctx));
    JS_SetPropertyStr(ctx, speaker, "frequencies", frequencies);

    // beat detection (stub)
    JSValue beat = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, beat, "detected", JS_FALSE);
    JS_SetPropertyStr(ctx, speaker, "beat", beat);

    // System volume (0-100)
    JS_SetPropertyStr(ctx, speaker, "systemVolume",
        JS_NewInt32(ctx, rt->audio ? rt->audio->system_volume : 100));

    JS_SetPropertyStr(ctx, sound, "speaker", speaker);

    // room
    JSValue room = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, room, "toggle", JS_NewCFunction(ctx, js_room_toggle, "toggle", 0));
    JS_SetPropertyStr(ctx, room, "setMix", JS_NewCFunction(ctx, js_set_room_mix, "setMix", 1));
    JS_SetPropertyStr(ctx, room, "mix", JS_NewFloat64(ctx, rt->audio ? rt->audio->room_mix : 0.0));
    JS_SetPropertyStr(ctx, room, "set", JS_NewCFunction(ctx, js_noop, "set", 1));
    JS_SetPropertyStr(ctx, room, "get", JS_NewCFunction(ctx, js_noop, "get", 0));
    JS_SetPropertyStr(ctx, sound, "room", room);

    // glitch
    JSValue glitch = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, glitch, "toggle", JS_NewCFunction(ctx, js_glitch_toggle, "toggle", 0));
    JS_SetPropertyStr(ctx, glitch, "setMix", JS_NewCFunction(ctx, js_set_glitch_mix, "setMix", 1));
    JS_SetPropertyStr(ctx, glitch, "mix", JS_NewFloat64(ctx, rt->audio ? rt->audio->glitch_mix : 0.0));
    JS_SetPropertyStr(ctx, glitch, "set", JS_NewCFunction(ctx, js_set_glitch_mix, "set", 1));
    JS_SetPropertyStr(ctx, glitch, "get", JS_NewCFunction(ctx, js_noop, "get", 0));
    JS_SetPropertyStr(ctx, sound, "glitch", glitch);

    // fx (dry/wet for entire FX chain)
    JSValue fx = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, fx, "setMix", JS_NewCFunction(ctx, js_set_fx_mix, "setMix", 1));
    JS_SetPropertyStr(ctx, fx, "mix", JS_NewFloat64(ctx, rt->audio ? rt->audio->fx_mix : 1.0));
    JS_SetPropertyStr(ctx, sound, "fx", fx);

    // microphone
    JSValue mic = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, mic, "open", JS_NewCFunction(ctx, js_mic_open, "open", 0));
    JS_SetPropertyStr(ctx, mic, "close", JS_NewCFunction(ctx, js_mic_close, "close", 0));
    JS_SetPropertyStr(ctx, mic, "rec", JS_NewCFunction(ctx, js_mic_rec, "rec", 0));
    JS_SetPropertyStr(ctx, mic, "cut", JS_NewCFunction(ctx, js_mic_cut, "cut", 0));
    JS_SetPropertyStr(ctx, mic, "recording",
        JS_NewBool(ctx, rt->audio ? rt->audio->recording : 0));
    JS_SetPropertyStr(ctx, mic, "connected",
        JS_NewBool(ctx, rt->audio ? rt->audio->mic_connected : 0));
    JS_SetPropertyStr(ctx, mic, "hot",
        JS_NewBool(ctx, rt->audio ? rt->audio->mic_hot : 0));
    JS_SetPropertyStr(ctx, mic, "sampleLength",
        JS_NewInt32(ctx, rt->audio ? rt->audio->sample_len : 0));
    JS_SetPropertyStr(ctx, mic, "sampleRate",
        JS_NewInt32(ctx, rt->audio ? (int)rt->audio->sample_rate : 0));
    JS_SetPropertyStr(ctx, mic, "level",
        JS_NewFloat64(ctx, rt->audio ? rt->audio->mic_level : 0.0));
    JS_SetPropertyStr(ctx, mic, "lastChunk",
        JS_NewInt32(ctx, rt->audio ? rt->audio->mic_last_chunk : 0));
    JS_SetPropertyStr(ctx, mic, "device",
        JS_NewString(ctx, (rt->audio && rt->audio->mic_device[0]) ? rt->audio->mic_device : "none"));
    JS_SetPropertyStr(ctx, mic, "lastError",
        JS_NewString(ctx, (rt->audio && rt->audio->mic_last_error[0]) ? rt->audio->mic_last_error : ""));
    // Mic level is already exposed as mic.level (single float, no array overhead)
    JS_SetPropertyStr(ctx, sound, "microphone", mic);

    // sample playback
    JSValue samp = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, samp, "play", JS_NewCFunction(ctx, js_sample_play, "play", 1));
    JS_SetPropertyStr(ctx, samp, "kill", JS_NewCFunction(ctx, js_sample_kill, "kill", 2));
    JS_SetPropertyStr(ctx, samp, "getData", JS_NewCFunction(ctx, js_sample_get_data, "getData", 0));
    JS_SetPropertyStr(ctx, samp, "loadData", JS_NewCFunction(ctx, js_sample_load_data, "loadData", 2));
    JS_SetPropertyStr(ctx, samp, "saveTo", JS_NewCFunction(ctx, js_sample_save_to, "saveTo", 1));
    JS_SetPropertyStr(ctx, samp, "loadFrom", JS_NewCFunction(ctx, js_sample_load_from, "loadFrom", 1));
    JS_SetPropertyStr(ctx, sound, "sample", samp);

    // dedicated global replay voice/buffer
    JSValue replay = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, replay, "play", JS_NewCFunction(ctx, js_replay_play, "play", 1));
    JS_SetPropertyStr(ctx, replay, "kill", JS_NewCFunction(ctx, js_replay_kill, "kill", 2));
    JS_SetPropertyStr(ctx, replay, "loadData", JS_NewCFunction(ctx, js_replay_load_data, "loadData", 2));
    JS_SetPropertyStr(ctx, sound, "replay", replay);

    // DJ deck
    JSValue deck_obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, deck_obj, "load", JS_NewCFunction(ctx, js_deck_load, "load", 2));
    JS_SetPropertyStr(ctx, deck_obj, "play", JS_NewCFunction(ctx, js_deck_play, "play", 1));
    JS_SetPropertyStr(ctx, deck_obj, "pause", JS_NewCFunction(ctx, js_deck_pause, "pause", 1));
    JS_SetPropertyStr(ctx, deck_obj, "seek", JS_NewCFunction(ctx, js_deck_seek, "seek", 2));
    JS_SetPropertyStr(ctx, deck_obj, "setSpeed", JS_NewCFunction(ctx, js_deck_set_speed, "setSpeed", 2));
    JS_SetPropertyStr(ctx, deck_obj, "setVolume", JS_NewCFunction(ctx, js_deck_set_volume, "setVolume", 2));
    JS_SetPropertyStr(ctx, deck_obj, "setCrossfader", JS_NewCFunction(ctx, js_deck_set_crossfader, "setCrossfader", 1));
    JS_SetPropertyStr(ctx, deck_obj, "setMasterVolume", JS_NewCFunction(ctx, js_deck_set_master_vol, "setMasterVolume", 1));
    JS_SetPropertyStr(ctx, deck_obj, "getPeaks", JS_NewCFunction(ctx, js_deck_get_peaks, "getPeaks", 1));
    JS_SetPropertyStr(ctx, deck_obj, "prepareVideo", JS_NewCFunction(ctx, js_deck_prepare_video, "prepareVideo", 4));
    JS_SetPropertyStr(ctx, deck_obj, "videoBlit", JS_NewCFunction(ctx, js_deck_video_blit, "videoBlit", 5));

    // Deck state (read-only, rebuilt each frame)
    JSValue decks_arr = JS_NewArray(ctx);
    ACAudio *aud = rt->audio;
    for (int d = 0; d < AUDIO_MAX_DECKS; d++) {
        JSValue di = JS_NewObject(ctx);
        if (aud) {
            ACDeck *dk = &aud->decks[d];
            JS_SetPropertyStr(ctx, di, "loaded", JS_NewBool(ctx, dk->active));
            JS_SetPropertyStr(ctx, di, "playing", JS_NewBool(ctx, dk->playing));
            JS_SetPropertyStr(ctx, di, "volume", JS_NewFloat64(ctx, dk->volume));
            if (dk->decoder) {
                JS_SetPropertyStr(ctx, di, "position", JS_NewFloat64(ctx, dk->decoder->position));
                JS_SetPropertyStr(ctx, di, "duration", JS_NewFloat64(ctx, dk->decoder->duration));
                JS_SetPropertyStr(ctx, di, "speed", JS_NewFloat64(ctx, dk->decoder->speed));
                JS_SetPropertyStr(ctx, di, "title", JS_NewString(ctx, dk->decoder->title));
                JS_SetPropertyStr(ctx, di, "artist", JS_NewString(ctx, dk->decoder->artist));
                JS_SetPropertyStr(ctx, di, "finished", JS_NewBool(ctx, dk->decoder->finished));
                JS_SetPropertyStr(ctx, di, "error", JS_NewString(ctx, dk->decoder->error ? dk->decoder->error_msg : ""));
                JS_SetPropertyStr(ctx, di, "videoReady", JS_NewBool(ctx, dk->decoder->video_ready));
                JS_SetPropertyStr(ctx, di, "videoWidth", JS_NewInt32(ctx, dk->decoder->video_width));
                JS_SetPropertyStr(ctx, di, "videoHeight", JS_NewInt32(ctx, dk->decoder->video_height));
                JS_SetPropertyStr(ctx, di, "videoFps", JS_NewFloat64(ctx, dk->decoder->video_fps));
                JS_SetPropertyStr(ctx, di, "videoFrames", JS_NewInt32(ctx, dk->decoder->video_frame_count));
            } else {
                JS_SetPropertyStr(ctx, di, "position", JS_NewFloat64(ctx, 0));
                JS_SetPropertyStr(ctx, di, "duration", JS_NewFloat64(ctx, 0));
                JS_SetPropertyStr(ctx, di, "speed", JS_NewFloat64(ctx, 1.0));
                JS_SetPropertyStr(ctx, di, "title", JS_NewString(ctx, ""));
                JS_SetPropertyStr(ctx, di, "artist", JS_NewString(ctx, ""));
                JS_SetPropertyStr(ctx, di, "finished", JS_FALSE);
                JS_SetPropertyStr(ctx, di, "error", JS_NewString(ctx, ""));
                JS_SetPropertyStr(ctx, di, "videoReady", JS_FALSE);
                JS_SetPropertyStr(ctx, di, "videoWidth", JS_NewInt32(ctx, 0));
                JS_SetPropertyStr(ctx, di, "videoHeight", JS_NewInt32(ctx, 0));
                JS_SetPropertyStr(ctx, di, "videoFps", JS_NewFloat64(ctx, 0));
                JS_SetPropertyStr(ctx, di, "videoFrames", JS_NewInt32(ctx, 0));
            }
        }
        JS_SetPropertyUint32(ctx, decks_arr, d, di);
    }
    JS_SetPropertyStr(ctx, deck_obj, "decks", decks_arr);
    JS_SetPropertyStr(ctx, deck_obj, "crossfaderValue",
        JS_NewFloat64(ctx, aud ? aud->crossfader : 0.5));
    JS_SetPropertyStr(ctx, deck_obj, "masterVolume",
        JS_NewFloat64(ctx, aud ? aud->deck_master_volume : 0.8));
    JS_SetPropertyStr(ctx, sound, "deck", deck_obj);

    // TTS
    JS_SetPropertyStr(ctx, sound, "speak", JS_NewCFunction(ctx, js_speak, "speak", 1));
    JS_SetPropertyStr(ctx, sound, "speakVoice", JS_NewCFunction(ctx, js_speak_voice, "speakVoice", 2));
    JS_SetPropertyStr(ctx, sound, "speakCached", JS_NewCFunction(ctx, js_speak_cached, "speakCached", 1));

    // sound.paint (visualization helpers)
    JSValue sound_paint = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, sound_paint, "bars", JS_NewCFunction(ctx, js_noop, "bars", 10));
    JS_SetPropertyStr(ctx, sound_paint, "audioEngineBadge", JS_NewCFunction(ctx, js_noop, "audioEngineBadge", 5));
    JS_SetPropertyStr(ctx, sound, "paint", sound_paint);

    // midi (stub)
    JSValue midi = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, midi, "connect", JS_NewCFunction(ctx, js_noop, "connect", 0));
    JS_SetPropertyStr(ctx, midi, "noteOn", JS_NewCFunction(ctx, js_usb_midi_note_on, "noteOn", 3));
    JS_SetPropertyStr(ctx, midi, "noteOff", JS_NewCFunction(ctx, js_usb_midi_note_off, "noteOff", 3));
    JS_SetPropertyStr(ctx, midi, "allNotesOff", JS_NewCFunction(ctx, js_usb_midi_all_notes_off, "allNotesOff", 1));
    JS_SetPropertyStr(ctx, sound, "midi", midi);

    // daw (stub)
    JSValue daw = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, daw, "bpm", JS_UNDEFINED);
    JS_SetPropertyStr(ctx, sound, "daw", daw);

    return sound;
}

// Read a small sysfs file into a buffer, return length or -1
static int read_sysfs(const char *path, char *buf, int bufsize) {
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    int len = (int)fread(buf, 1, bufsize - 1, f);
    fclose(f);
    if (len > 0 && buf[len-1] == '\n') len--;
    buf[len] = 0;
    return len;
}

// ============================================================
// WiFi JS bindings
// ============================================================

static JSValue js_wifi_scan(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt->wifi) wifi_scan(current_rt->wifi);
    return JS_UNDEFINED;
}

static JSValue js_wifi_connect(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt->wifi || argc < 1) return JS_UNDEFINED;
    const char *ssid = JS_ToCString(ctx, argv[0]);
    const char *pass = (argc >= 2 && JS_IsString(argv[1])) ? JS_ToCString(ctx, argv[1]) : NULL;
    if (ssid) wifi_connect(current_rt->wifi, ssid, pass);
    if (ssid) JS_FreeCString(ctx, ssid);
    if (pass) JS_FreeCString(ctx, pass);
    return JS_UNDEFINED;
}

static JSValue js_wifi_disconnect(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt->wifi) wifi_disconnect(current_rt->wifi);
    return JS_UNDEFINED;
}

static JSValue build_wifi_obj(JSContext *ctx, ACWifi *wifi) {
    JSValue obj = JS_NewObject(ctx);

    JS_SetPropertyStr(ctx, obj, "scan", JS_NewCFunction(ctx, js_wifi_scan, "scan", 0));
    JS_SetPropertyStr(ctx, obj, "connect", JS_NewCFunction(ctx, js_wifi_connect, "connect", 2));
    JS_SetPropertyStr(ctx, obj, "disconnect", JS_NewCFunction(ctx, js_wifi_disconnect, "disconnect", 0));

    if (wifi) {
        // State is updated by the wifi worker thread — just read it here
        JS_SetPropertyStr(ctx, obj, "state", JS_NewInt32(ctx, wifi->state));
        JS_SetPropertyStr(ctx, obj, "status", JS_NewString(ctx, wifi->status_msg));
        JS_SetPropertyStr(ctx, obj, "connected", JS_NewBool(ctx, wifi->state == WIFI_STATE_CONNECTED));
        JS_SetPropertyStr(ctx, obj, "ssid", JS_NewString(ctx, wifi->connected_ssid));
        JS_SetPropertyStr(ctx, obj, "ip", JS_NewString(ctx, wifi->ip_address));
        JS_SetPropertyStr(ctx, obj, "iface", JS_NewString(ctx, wifi->iface));
        JS_SetPropertyStr(ctx, obj, "signal", JS_NewInt32(ctx, wifi->signal_strength));

        // Networks array
        JSValue networks = JS_NewArray(ctx);
        for (int i = 0; i < wifi->network_count; i++) {
            JSValue net = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, net, "ssid", JS_NewString(ctx, wifi->networks[i].ssid));
            JS_SetPropertyStr(ctx, net, "signal", JS_NewInt32(ctx, wifi->networks[i].signal));
            JS_SetPropertyStr(ctx, net, "encrypted", JS_NewBool(ctx, wifi->networks[i].encrypted));
            JS_SetPropertyUint32(ctx, networks, i, net);
        }
        JS_SetPropertyStr(ctx, obj, "networks", networks);

        // Log ring buffer (last 32 messages from wifi thread)
        JSValue logs = JS_NewArray(ctx);
        int total = wifi->log_count;
        int count = total < 32 ? total : 32;
        for (int i = 0; i < count; i++) {
            // Read in chronological order (oldest first)
            int idx = (total >= 32) ? ((total - 32 + i) % 32) : i;
            JS_SetPropertyUint32(ctx, logs, i,
                JS_NewString(ctx, wifi->log[idx]));
        }
        JS_SetPropertyStr(ctx, obj, "logs", logs);
    } else {
        extern int wifi_disabled;
        JS_SetPropertyStr(ctx, obj, "state", JS_NewInt32(ctx, WIFI_STATE_OFF));
        JS_SetPropertyStr(ctx, obj, "status",
            JS_NewString(ctx, wifi_disabled ? "disabled" : "no wifi"));
        JS_SetPropertyStr(ctx, obj, "disabled", JS_NewBool(ctx, wifi_disabled));
        JS_SetPropertyStr(ctx, obj, "connected", JS_FALSE);
        JS_SetPropertyStr(ctx, obj, "networks", JS_NewArray(ctx));
    }

    return obj;
}

// system.hdmi(r, g, b) — fill secondary display with solid color
static JSValue js_hdmi_fill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->hdmi || !current_rt->hdmi->active) return JS_UNDEFINED;
    if (argc < 3) return JS_UNDEFINED;
    int r, g, b;
    JS_ToInt32(ctx, &r, argv[0]);
    JS_ToInt32(ctx, &g, argv[1]);
    JS_ToInt32(ctx, &b, argv[2]);
    drm_secondary_fill(current_rt->hdmi, (uint8_t)r, (uint8_t)g, (uint8_t)b);
    return JS_UNDEFINED;
}

// system.readFile(path, [lastNLines]) — read a file from disk, returns string or null
// If lastNLines is provided, returns only the last N lines.
static JSValue js_read_file(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NULL;
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NULL;
    int lastN = 0;
    if (argc >= 2) JS_ToInt32(ctx, &lastN, argv[1]);
    FILE *fp = fopen(path, "r");
    if (!fp) { JS_FreeCString(ctx, path); return JS_NULL; }
    fseek(fp, 0, SEEK_END);
    long sz = ftell(fp);
    if (sz <= 0) { fclose(fp); JS_FreeCString(ctx, path); return JS_NULL; }
    if (sz > 65536) {
        fseek(fp, -65536, SEEK_END);
        sz = 65536;
    } else {
        rewind(fp);
    }
    char *buf = malloc(sz + 1);
    if (!buf) { fclose(fp); JS_FreeCString(ctx, path); return JS_NULL; }
    long rd = (long)fread(buf, 1, sz, fp);
    buf[rd] = 0;
    fclose(fp);
    JS_FreeCString(ctx, path);
    if (lastN > 0 && rd > 0) {
        int nl_count = 0;
        char *p = buf + rd - 1;
        if (*p == '\n') p--;
        while (p > buf) {
            if (*p == '\n') { nl_count++; if (nl_count >= lastN) { p++; break; } }
            p--;
        }
        JSValue result = JS_NewString(ctx, p);
        free(buf);
        return result;
    }
    JSValue result = JS_NewStringLen(ctx, buf, rd);
    free(buf);
    return result;
}

// system.writeFile(path, data) — write a string to disk, returns true/false
static JSValue js_write_file(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_FALSE;
    const char *path = JS_ToCString(ctx, argv[0]);
    const char *data = JS_ToCString(ctx, argv[1]);
    if (!path || !data) {
        if (path) JS_FreeCString(ctx, path);
        if (data) JS_FreeCString(ctx, data);
        return JS_FALSE;
    }
    FILE *fp = fopen(path, "w");
    int ok = 0;
    if (fp) {
        fputs(data, fp);
        fclose(fp);
        sync(); // flush to USB
        ok = 1;
    }
    JS_FreeCString(ctx, path);
    JS_FreeCString(ctx, data);
    return JS_NewBool(ctx, ok);
}

// system.deleteFile(path) — unlink a file, returns true on success
static JSValue js_delete_file(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_FALSE;
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_FALSE;
    int ok = (unlink(path) == 0);
    if (ok) sync();
    JS_FreeCString(ctx, path);
    return JS_NewBool(ctx, ok);
}

// system.diskInfo(path) — returns {total, free, available, blockSize, fstype}
// for the filesystem containing `path`, or null on error.
static JSValue js_disk_info(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NULL;
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NULL;
    struct statvfs vfs;
    int rc = statvfs(path, &vfs);
    JS_FreeCString(ctx, path);
    if (rc != 0) return JS_NULL;
    JSValue obj = JS_NewObject(ctx);
    uint64_t bs = (uint64_t)vfs.f_frsize ? (uint64_t)vfs.f_frsize : (uint64_t)vfs.f_bsize;
    JS_SetPropertyStr(ctx, obj, "total", JS_NewInt64(ctx, (int64_t)(vfs.f_blocks * bs)));
    JS_SetPropertyStr(ctx, obj, "free",  JS_NewInt64(ctx, (int64_t)(vfs.f_bfree  * bs)));
    JS_SetPropertyStr(ctx, obj, "available", JS_NewInt64(ctx, (int64_t)(vfs.f_bavail * bs)));
    JS_SetPropertyStr(ctx, obj, "blockSize", JS_NewInt64(ctx, (int64_t)bs));
    return obj;
}

// system.blockDevices() — list /sys/block entries with size + removable + model.
// Returns array of {name, sizeBytes, removable, model, vendor} or null.
static JSValue js_block_devices(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    DIR *dir = opendir("/sys/block");
    if (!dir) return JS_NULL;
    JSValue arr = JS_NewArray(ctx);
    struct dirent *ent;
    int idx = 0;
    while ((ent = readdir(dir)) != NULL) {
        if (ent->d_name[0] == '.') continue;
        // Skip loop/ram/dm pseudo devs
        if (strncmp(ent->d_name, "loop", 4) == 0) continue;
        if (strncmp(ent->d_name, "ram",  3) == 0) continue;
        if (strncmp(ent->d_name, "dm-",  3) == 0) continue;
        char p[256]; char buf[128];
        JSValue obj = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, obj, "name", JS_NewString(ctx, ent->d_name));
        // size (in 512-byte sectors)
        snprintf(p, sizeof(p), "/sys/block/%s/size", ent->d_name);
        FILE *f = fopen(p, "r"); long long sectors = 0;
        if (f) { fscanf(f, "%lld", &sectors); fclose(f); }
        JS_SetPropertyStr(ctx, obj, "sizeBytes", JS_NewInt64(ctx, sectors * 512LL));
        // removable flag
        snprintf(p, sizeof(p), "/sys/block/%s/removable", ent->d_name);
        int removable = 0;
        f = fopen(p, "r");
        if (f) { fscanf(f, "%d", &removable); fclose(f); }
        JS_SetPropertyStr(ctx, obj, "removable", JS_NewBool(ctx, removable != 0));
        // vendor + model
        const char *fields[][2] = { {"vendor", "vendor"}, {"model", "model"}, {NULL, NULL} };
        for (int i = 0; fields[i][0]; i++) {
            snprintf(p, sizeof(p), "/sys/block/%s/device/%s", ent->d_name, fields[i][0]);
            f = fopen(p, "r");
            if (f) {
                if (fgets(buf, sizeof(buf), f)) {
                    // trim trailing whitespace
                    size_t L = strlen(buf);
                    while (L > 0 && (buf[L-1] == '\n' || buf[L-1] == ' ' || buf[L-1] == '\t')) buf[--L] = 0;
                    JS_SetPropertyStr(ctx, obj, fields[i][1], JS_NewString(ctx, buf));
                }
                fclose(f);
            }
        }
        JS_SetPropertyUint32(ctx, arr, idx++, obj);
    }
    closedir(dir);
    return arr;
}

// system.listDir(path) — returns [{name, isDir, size}, ...] or null
static JSValue js_list_dir(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NULL;
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NULL;

    DIR *dir = opendir(path);
    if (!dir) {
        JS_FreeCString(ctx, path);
        return JS_NULL;
    }

    JSValue arr = JS_NewArray(ctx);
    struct dirent *ent;
    int idx = 0;
    while ((ent = readdir(dir)) != NULL) {
        // Skip . and ..
        if (ent->d_name[0] == '.' && (ent->d_name[1] == '\0' ||
            (ent->d_name[1] == '.' && ent->d_name[2] == '\0'))) continue;

        JSValue obj = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, obj, "name", JS_NewString(ctx, ent->d_name));

        // stat for size and type
        char full[1024];
        snprintf(full, sizeof(full), "%s/%s", path, ent->d_name);
        struct stat st;
        int is_dir = 0;
        int64_t size = 0;
        if (stat(full, &st) == 0) {
            is_dir = S_ISDIR(st.st_mode);
            size = st.st_size;
        }
        JS_SetPropertyStr(ctx, obj, "isDir", JS_NewBool(ctx, is_dir));
        JS_SetPropertyStr(ctx, obj, "size", JS_NewInt64(ctx, size));

        JS_SetPropertyUint32(ctx, arr, idx++, obj);
    }
    closedir(dir);
    JS_FreeCString(ctx, path);
    return arr;
}

// system.mountMusic() — non-blocking secondary USB probe.
// Schedules a background mount check for /media and immediately returns the
// last known mounted state. JS reads system.mountMusicMounted /
// system.mountMusicPending for cached state.
static pthread_mutex_t music_mount_mu = PTHREAD_MUTEX_INITIALIZER;
static volatile int music_mount_pending = 0;
static volatile int music_mount_state = 0;
static int music_mount_last_result = -1;
static char music_mount_last_skip_base[64] = "";

static int probe_mount_music_once(void) {

    mkdir("/media", 0755);

    // Check /proc/mounts for any existing mount at /media, and capture the device.
    char media_dev[128] = {0};
    FILE *mf = fopen("/proc/mounts", "r");
    if (mf) {
        char line[512];
        while (fgets(line, sizeof(line), mf)) {
            char dev[128], target[128];
            if (sscanf(line, "%127s %127s", dev, target) == 2 && strcmp(target, "/media") == 0) {
                strncpy(media_dev, dev, sizeof(media_dev) - 1);
                break;
            }
        }
        fclose(mf);
    }

    if (media_dev[0]) {
        // There's something mounted at /media. Verify the backing device is still
        // physically present (USB stick didn't get yanked).
        struct stat ds;
        int dev_alive = (stat(media_dev, &ds) == 0 && S_ISBLK(ds.st_mode));
        // Extra check: opendir succeeds on a live mount but may EIO on a stale one.
        int dir_ok = 0;
        if (dev_alive) {
            DIR *d = opendir("/media");
            if (d) { dir_ok = 1; closedir(d); }
        }
        if (dev_alive && dir_ok) {
            music_mount_last_result = 1;
            return 1;  // legitimate, live mount
        }
        // Stale: device is gone or dir unreadable. Lazy-unmount so we can remount.
        ac_log("[dj] stale /media mount (dev=%s, alive=%d, dir=%d) — detaching\n",
               media_dev, dev_alive, dir_ok);
        if (umount2("/media", MNT_DETACH) != 0) {
            ac_log("[dj] umount /media failed: %s\n", strerror(errno));
        }
    }

    // Find ALL mounted devices to skip (boot USB may have multiple partitions)
    char skip_bases[8][64];
    int skip_count = 0;
    FILE *fp = fopen("/proc/mounts", "r");
    if (fp) {
        char line[256];
        while (fgets(line, sizeof(line), fp) && skip_count < 8) {
            char mdev[64];
            if (sscanf(line, "%63s", mdev) == 1 && strncmp(mdev, "/dev/sd", 7) == 0) {
                // Strip partition number to get base
                char base[64];
                snprintf(base, sizeof(base), "%s", mdev);
                int len = strlen(base);
                while (len > 0 && base[len-1] >= '0' && base[len-1] <= '9') base[--len] = '\0';
                // Add if not already in skip list
                int found = 0;
                for (int i = 0; i < skip_count; i++)
                    if (strcmp(skip_bases[i], base) == 0) { found = 1; break; }
                if (!found) {
                    strncpy(skip_bases[skip_count++], base, 63);
                    if (strcmp(music_mount_last_skip_base, base) != 0) {
                        snprintf(music_mount_last_skip_base, sizeof(music_mount_last_skip_base), "%s", base);
                        ac_log("[dj] skip boot device: %s\n", base);
                    }
                }
            }
        }
        fclose(fp);
    }

    // Try mounting partitions on non-boot USB devices
    const char *bases[] = { "/dev/sda", "/dev/sdb", "/dev/sdc", "/dev/sdd", NULL };
    for (int b = 0; bases[b]; b++) {
        int skip = 0;
        for (int i = 0; i < skip_count; i++)
            if (strcmp(bases[b], skip_bases[i]) == 0) { skip = 1; break; }
        if (skip) continue;

        for (int p = 1; p <= 8; p++) {
            char dev[64];
            snprintf(dev, sizeof(dev), "%s%d", bases[b], p);
            struct stat ds;
            if (stat(dev, &ds) != 0) continue;

            if (mount(dev, "/media", "vfat", MS_RDONLY, "iocharset=utf8") == 0) {
                ac_log("[dj] mounted %s at /media (vfat)\n", dev);
                music_mount_last_result = 1;
                return 1;
            }
            if (mount(dev, "/media", "exfat", MS_RDONLY, NULL) == 0) {
                ac_log("[dj] mounted %s at /media (exfat)\n", dev);
                music_mount_last_result = 1;
                return 1;
            }
            if (mount(dev, "/media", "ext4", MS_RDONLY, NULL) == 0) {
                ac_log("[dj] mounted %s at /media (ext4)\n", dev);
                music_mount_last_result = 1;
                return 1;
            }
            if (mount(dev, "/media", "ntfs3", MS_RDONLY, NULL) == 0) {
                ac_log("[dj] mounted %s at /media (ntfs)\n", dev);
                music_mount_last_result = 1;
                return 1;
            }
        }
    }
    if (music_mount_last_result != 0) {
        ac_log("[dj] no music USB found\n");
        music_mount_last_result = 0;
    }
    return 0;
}

static void *music_mount_thread_fn(void *arg) {
    (void)arg;
    int mounted = probe_mount_music_once();
    pthread_mutex_lock(&music_mount_mu);
    music_mount_state = mounted ? 1 : 0;
    music_mount_pending = 0;
    pthread_mutex_unlock(&music_mount_mu);
    return NULL;
}

static void request_music_mount_probe(void) {
    int should_spawn = 0;
    pthread_t tid;

    pthread_mutex_lock(&music_mount_mu);
    if (!music_mount_pending) {
        music_mount_pending = 1;
        should_spawn = 1;
    }
    pthread_mutex_unlock(&music_mount_mu);

    if (!should_spawn) return;

    if (pthread_create(&tid, NULL, music_mount_thread_fn, NULL) != 0) {
        pthread_mutex_lock(&music_mount_mu);
        music_mount_pending = 0;
        pthread_mutex_unlock(&music_mount_mu);
        ac_log("[dj] mountMusic thread create failed\n");
        return;
    }
    pthread_detach(tid);
}

static JSValue js_mount_music(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    request_music_mount_probe();
    return JS_NewBool(ctx, music_mount_state);
}

// ---------------------------------------------------------------------------
// system.ws — WebSocket client
// ---------------------------------------------------------------------------

static JSValue js_ws_connect(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt) return JS_UNDEFINED;
    const char *url = JS_ToCString(ctx, argv[0]);
    if (!url) return JS_UNDEFINED;
    ws_connect(current_rt->ws, url);  // non-blocking: signals background thread
    JS_FreeCString(ctx, url);
    return JS_UNDEFINED;
}

static JSValue js_ws_send(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt || !current_rt->ws) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;
    ws_send(current_rt->ws, text);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

static JSValue js_ws_close(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt && current_rt->ws) ws_close(current_rt->ws);
    return JS_UNDEFINED;
}

static JSValue build_ws_obj(JSContext *ctx, const char *phase) {
    JSValue ws_obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, ws_obj, "connect", JS_NewCFunction(ctx, js_ws_connect, "connect", 1));
    JS_SetPropertyStr(ctx, ws_obj, "send",    JS_NewCFunction(ctx, js_ws_send,    "send",    1));
    JS_SetPropertyStr(ctx, ws_obj, "close",   JS_NewCFunction(ctx, js_ws_close,   "close",   0));

    ACWs *ws = current_rt ? current_rt->ws : NULL;
    if (ws) {
        // Only drain messages during "paint" phase — that's where JS processes them.
        // Other phases (act, sim) see connected/connecting but empty messages array,
        // preventing the queue from being consumed before paint() can read it.
        int drain = (strcmp(phase, "paint") == 0);
        pthread_mutex_lock(&ws->mu);
        int count = drain ? ws->msg_count : 0;
        if (drain) ws->msg_count = 0;
        int connected = ws->connected;
        int connecting = ws->connecting;
        // Copy only actual message bytes (not full 256KB buffer each) — heap alloc
        if (count > WS_MAX_MESSAGES) count = WS_MAX_MESSAGES;
        char *msgs[WS_MAX_MESSAGES];
        int   msg_lens[WS_MAX_MESSAGES];
        for (int i = 0; i < count; i++) {
            int len = strnlen(ws->messages[i], WS_MAX_MSG_LEN - 1);
            msgs[i] = malloc(len + 1);
            if (msgs[i]) { memcpy(msgs[i], ws->messages[i], len); msgs[i][len] = 0; }
            msg_lens[i] = len;
        }
        pthread_mutex_unlock(&ws->mu);

        JS_SetPropertyStr(ctx, ws_obj, "connected",  JS_NewBool(ctx, connected));
        JS_SetPropertyStr(ctx, ws_obj, "connecting", JS_NewBool(ctx, connecting));

        JSValue arr = JS_NewArray(ctx);
        for (int i = 0; i < count; i++) {
            if (msgs[i]) {
                JS_SetPropertyUint32(ctx, arr, (uint32_t)i, JS_NewStringLen(ctx, msgs[i], msg_lens[i]));
                free(msgs[i]);
            }
        }
        JS_SetPropertyStr(ctx, ws_obj, "messages", arr);
    } else {
        JS_SetPropertyStr(ctx, ws_obj, "connected",  JS_FALSE);
        JS_SetPropertyStr(ctx, ws_obj, "connecting", JS_FALSE);
        JS_SetPropertyStr(ctx, ws_obj, "messages",   JS_NewArray(ctx));
    }
    return ws_obj;
}

// system.fetch(url) — async HTTP GET via curl, result polled via system.fetchResult
static JSValue js_fetch(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_UNDEFINED;
    // Reject if a fetch is already in flight (single-slot)
    if (current_rt->fetch_pending) return JS_FALSE;
    const char *url = JS_ToCString(ctx, argv[0]);
    if (!url) return JS_UNDEFINED;
    unlink("/tmp/ac_fetch.json");
    unlink("/tmp/ac_fetch_rc");
    unlink("/tmp/ac_fetch_err");
    char cmd[2048];
    ac_log("[fetch] start: %s\n", url);
    snprintf(cmd, sizeof(cmd),
        "sh -c 'curl -fsSL --retry 2 --retry-delay 1 --connect-timeout 5 --max-time 12 "
        "--cacert /etc/pki/tls/certs/ca-bundle.crt "
        "--output /tmp/ac_fetch.json \"%s\" 2>/tmp/ac_fetch_err;"
        " echo $? > /tmp/ac_fetch_rc' &", url);
    system(cmd);
    current_rt->fetch_pending = 1;
    current_rt->fetch_result[0] = 0;
    current_rt->fetch_error[0] = 0;
    JS_FreeCString(ctx, url);
    return JS_TRUE;
}

// system.fetchPost(url, body, headersJSON) — POST request with custom headers
// Uses the same single fetch slot as system.fetch.
// headersJSON is a JSON string like '{"Authorization":"Bearer ...","Content-Type":"application/json"}'
static JSValue js_fetch_post(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 2) return JS_UNDEFINED;
    if (current_rt->fetch_pending) return JS_FALSE;
    const char *url = JS_ToCString(ctx, argv[0]);
    const char *body = JS_ToCString(ctx, argv[1]);
    const char *headers_json = (argc >= 3) ? JS_ToCString(ctx, argv[2]) : NULL;
    if (!url || !body) {
        JS_FreeCString(ctx, url);
        JS_FreeCString(ctx, body);
        if (headers_json) JS_FreeCString(ctx, headers_json);
        return JS_UNDEFINED;
    }
    unlink("/tmp/ac_fetch.json");
    unlink("/tmp/ac_fetch_rc");
    unlink("/tmp/ac_fetch_err");

    // Write body to temp file to avoid shell escaping issues
    FILE *bf = fopen("/tmp/ac_fetch_body.json", "w");
    if (bf) { fputs(body, bf); fclose(bf); }

    // Build header flags by parsing simple JSON {"key":"value",...}
    // We build a file of -H flags to avoid shell escaping nightmares
    FILE *hf = fopen("/tmp/ac_fetch_headers.txt", "w");
    if (hf) {
        // Always include Content-Type
        fprintf(hf, "-H\nContent-Type: application/json\n");
        if (headers_json) {
            // Simple JSON parser: find "key":"value" pairs
            const char *p = headers_json;
            while (*p) {
                const char *kq = strchr(p, '"');
                if (!kq) break;
                const char *ke = strchr(kq + 1, '"');
                if (!ke) break;
                const char *vq = strchr(ke + 1, '"');
                if (!vq) break;
                const char *ve = strchr(vq + 1, '"');
                if (!ve) break;
                int klen = (int)(ke - kq - 1);
                int vlen = (int)(ve - vq - 1);
                if (klen > 0 && klen < 256 && vlen > 0 && vlen < 2048) {
                    fprintf(hf, "-H\n%.*s: %.*s\n", klen, kq + 1, vlen, vq + 1);
                }
                p = ve + 1;
            }
        }
        fclose(hf);
    }

    ac_log("[fetchPost] start: %s (body %ld bytes)\n", url, (long)strlen(body));
    char cmd[2048];
    snprintf(cmd, sizeof(cmd),
        "sh -c 'curl -fsSL -X POST --retry 1 --connect-timeout 10 --max-time 120 "
        "--cacert /etc/pki/tls/certs/ca-bundle.crt "
        "-K /tmp/ac_fetch_headers.txt "
        "-d @/tmp/ac_fetch_body.json "
        "--output /tmp/ac_fetch.json \"%s\" 2>/tmp/ac_fetch_err;"
        " echo $? > /tmp/ac_fetch_rc' &", url);
    system(cmd);
    current_rt->fetch_pending = 1;
    current_rt->fetch_result[0] = 0;
    current_rt->fetch_error[0] = 0;
    JS_FreeCString(ctx, url);
    JS_FreeCString(ctx, body);
    if (headers_json) JS_FreeCString(ctx, headers_json);
    return JS_TRUE;
}

// system.fetchCancel() — kill in-flight curl and free the fetch slot
static JSValue js_fetch_cancel(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt) return JS_UNDEFINED;
    if (current_rt->fetch_pending) {
        ac_log("[fetch] cancel: killing in-flight curl\n");
        // Kill any background curl for ac_fetch
        system("pkill -f 'curl.*ac_fetch' 2>/dev/null");
        unlink("/tmp/ac_fetch.json");
        unlink("/tmp/ac_fetch_rc");
        unlink("/tmp/ac_fetch_err");
        current_rt->fetch_pending = 0;
        current_rt->fetch_result[0] = 0;
        current_rt->fetch_error[0] = 0;
    }
    return JS_UNDEFINED;
}

// ---------------------------------------------------------------------------
// system.scanQR() — Open camera and scan for QR codes in a background thread
// system.scanQRStop() — Stop scanning and close camera
// Poll system.qrPending / system.qrResult each frame.
// ---------------------------------------------------------------------------

static void *qr_scan_thread(void *arg) {
    ACRuntime *rt = (ACRuntime *)arg;
    ACCamera *cam = &rt->camera;

    if (camera_open(cam) < 0) {
        cam->scan_done = 1;
        rt->qr_thread_running = 0;
        return NULL;
    }

    // Grab frames and scan until we find a QR code or are told to stop
    int max_attempts = 300; // ~10 seconds at 30fps
    for (int i = 0; i < max_attempts && rt->qr_scan_active; i++) {
        if (camera_grab(cam) == 0) {
            if (camera_scan_qr(cam) > 0) {
                cam->scan_done = 1;
                break;
            }
        }
        usleep(33000); // ~30fps
    }

    if (!cam->scan_done) {
        cam->scan_done = 1;
        if (!cam->scan_result[0] && !cam->scan_error[0]) {
            snprintf(cam->scan_error, sizeof(cam->scan_error), "no QR code found");
        }
    }

    camera_close(cam);
    rt->qr_scan_active = 0;
    rt->qr_thread_running = 0;
    return NULL;
}

static JSValue js_scan_qr(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt) return JS_UNDEFINED;

    // Already scanning?
    if (current_rt->qr_scan_active) return JS_FALSE;

    // Reset state
    current_rt->camera.scan_result[0] = 0;
    current_rt->camera.scan_error[0] = 0;
    current_rt->camera.scan_done = 0;
    current_rt->qr_scan_active = 1;
    current_rt->qr_thread_running = 1;

    if (pthread_create(&current_rt->qr_thread, NULL, qr_scan_thread, current_rt) != 0) {
        current_rt->qr_scan_active = 0;
        current_rt->qr_thread_running = 0;
        snprintf(current_rt->camera.scan_error, sizeof(current_rt->camera.scan_error),
                 "thread create failed");
        current_rt->camera.scan_done = 1;
        return JS_FALSE;
    }
    pthread_detach(current_rt->qr_thread);

    ac_log("[qr] scan started\n");
    return JS_TRUE;
}

static JSValue js_scan_qr_stop(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv; (void)ctx;
    if (!current_rt) return JS_UNDEFINED;
    current_rt->qr_scan_active = 0;
    ac_log("[qr] scan stopped\n");
    return JS_UNDEFINED;
}

// cameraBlit(x, y, w, h) — render camera display buffer to graph framebuffer
static JSValue js_camera_blit(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->graph) return JS_FALSE;
    ACCamera *cam = &current_rt->camera;
    if (!cam->display || !cam->display_ready) return JS_FALSE;

    int dx = 0, dy = 0, dw = 0, dh = 0;
    if (argc >= 4) {
        JS_ToInt32(ctx, &dx, argv[0]);
        JS_ToInt32(ctx, &dy, argv[1]);
        JS_ToInt32(ctx, &dw, argv[2]);
        JS_ToInt32(ctx, &dh, argv[3]);
    } else {
        // Default: fit to screen
        dw = current_rt->graph->fb->width;
        dh = current_rt->graph->fb->height;
    }
    if (dw <= 0 || dh <= 0) return JS_FALSE;

    // Lock and copy the display buffer to a local copy
    int cw = cam->width, ch = cam->height;
    int pixels = cw * ch;
    uint8_t *local = malloc(pixels);
    if (!local) return JS_FALSE;

    pthread_mutex_lock(&cam->display_mu);
    memcpy(local, cam->display, pixels);
    pthread_mutex_unlock(&cam->display_mu);

    // Blit grayscale to framebuffer with nearest-neighbor scaling
    ACFramebuffer *fb = current_rt->graph->fb;
    for (int py = 0; py < dh; py++) {
        int fy = dy + py;
        if (fy < 0 || fy >= fb->height) continue;
        int sy = py * ch / dh;
        if (sy >= ch) sy = ch - 1;
        for (int px = 0; px < dw; px++) {
            int fx = dx + px;
            if (fx < 0 || fx >= fb->width) continue;
            int sx = px * cw / dw;
            if (sx >= cw) sx = cw - 1;
            uint8_t g = local[sy * cw + sx];
            fb->pixels[fy * fb->stride + fx] = 0xFF000000u | ((uint32_t)g << 16) | ((uint32_t)g << 8) | g;
        }
    }

    free(local);
    return JS_TRUE;
}

// ---------------------------------------------------------------------------
// system.udp — Raw UDP fairy point co-presence
// ---------------------------------------------------------------------------

static void sync_udp_identity(void) {
    extern char g_machine_id[64];

    if (!current_rt || !current_rt->udp) return;
    udp_set_identity(
        current_rt->udp,
        current_rt->handle[0] ? current_rt->handle : "",
        g_machine_id[0] ? g_machine_id : "unknown"
    );
}

static JSValue js_udp_connect(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->udp) return JS_UNDEFINED;
    const char *host = argc > 0 ? JS_ToCString(ctx, argv[0]) : NULL;
    if (!host) host = "session-server.aesthetic.computer";
    int port = UDP_FAIRY_PORT;
    if (argc > 1) JS_ToInt32(ctx, &port, argv[1]);
    sync_udp_identity();
    udp_connect(current_rt->udp, host, port);
    if (argc > 0) JS_FreeCString(ctx, host);
    return JS_UNDEFINED;
}

static JSValue js_udp_send_fairy(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->udp || argc < 2) return JS_UNDEFINED;
    double x, y;
    JS_ToFloat64(ctx, &x, argv[0]);
    JS_ToFloat64(ctx, &y, argv[1]);
    udp_send_fairy(current_rt->udp, (float)x, (float)y);
    return JS_UNDEFINED;
}

static JSValue js_udp_send_midi(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->udp || argc < 4) return JS_UNDEFINED;

    const char *event = JS_ToCString(ctx, argv[0]);
    int32_t note = 0;
    int32_t velocity = 0;
    int32_t channel = 0;
    const char *piece = argc > 4 ? JS_ToCString(ctx, argv[4]) : NULL;

    JS_ToInt32(ctx, &note, argv[1]);
    JS_ToInt32(ctx, &velocity, argv[2]);
    JS_ToInt32(ctx, &channel, argv[3]);

    if (!event) {
        if (piece) JS_FreeCString(ctx, piece);
        return JS_UNDEFINED;
    }

    sync_udp_identity();
    udp_send_midi(
        current_rt->udp,
        event,
        (int)note,
        (int)velocity,
        (int)channel,
        piece ? piece : "notepat"
    );

    JS_FreeCString(ctx, event);
    if (piece) JS_FreeCString(ctx, piece);
    return JS_UNDEFINED;
}

static JSValue js_udp_send_midi_heartbeat(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->udp) return JS_UNDEFINED;

    const char *piece = argc > 0 ? JS_ToCString(ctx, argv[0]) : NULL;
    sync_udp_identity();
    udp_send_midi_heartbeat(current_rt->udp, piece ? piece : "notepat");
    if (piece) JS_FreeCString(ctx, piece);
    return JS_UNDEFINED;
}

static JSValue build_udp_obj(JSContext *ctx, const char *phase) {
    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "connect", JS_NewCFunction(ctx, js_udp_connect, "connect", 2));
    JS_SetPropertyStr(ctx, obj, "sendFairy", JS_NewCFunction(ctx, js_udp_send_fairy, "sendFairy", 2));
    JS_SetPropertyStr(ctx, obj, "sendMidi", JS_NewCFunction(ctx, js_udp_send_midi, "sendMidi", 5));
    JS_SetPropertyStr(ctx, obj, "sendMidiHeartbeat", JS_NewCFunction(ctx, js_udp_send_midi_heartbeat, "sendMidiHeartbeat", 1));

    ACUdp *udp = current_rt ? current_rt->udp : NULL;
    if (udp) {
        sync_udp_identity();
        JS_SetPropertyStr(ctx, obj, "connected", JS_NewBool(ctx, udp->connected));
        JS_SetPropertyStr(ctx, obj, "handle",
                          JS_NewString(ctx, current_rt && current_rt->handle[0] ? current_rt->handle : ""));

        // Only deliver fairies during paint phase
        if (strcmp(phase, "paint") == 0) {
            UDPFairy fairies[UDP_MAX_FAIRIES];
            int count = udp_poll_fairies(udp, fairies, UDP_MAX_FAIRIES);
            JSValue arr = JS_NewArray(ctx);
            for (int i = 0; i < count; i++) {
                JSValue f = JS_NewObject(ctx);
                JS_SetPropertyStr(ctx, f, "x", JS_NewFloat64(ctx, fairies[i].x));
                JS_SetPropertyStr(ctx, f, "y", JS_NewFloat64(ctx, fairies[i].y));
                JS_SetPropertyUint32(ctx, arr, i, f);
            }
            JS_SetPropertyStr(ctx, obj, "fairies", arr);
        }
    }
    return obj;
}

// system.fetchBinary(url, destPath[, expectedBytes])
// Non-blocking: runs curl in background; poll system.fetchBinaryProgress/Done/Ok each frame.
static JSValue js_fetch_binary(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 2) return JS_UNDEFINED;
    const char *url  = JS_ToCString(ctx, argv[0]);
    const char *dest = JS_ToCString(ctx, argv[1]);
    if (!url || !dest) {
        JS_FreeCString(ctx, url);
        JS_FreeCString(ctx, dest);
        return JS_UNDEFINED;
    }
    long expected = 0;
    if (argc >= 3) {
        double v;
        JS_ToFloat64(ctx, &v, argv[2]);
        expected = (long)v;
    }
    ac_log("[fetchBinary] start: url=%s dest=%s expected=%ld\n", url, dest, expected);
    // Reset state
    current_rt->fetch_binary_pending  = 1;
    current_rt->fetch_binary_done     = 0;
    current_rt->fetch_binary_ok       = 0;
    current_rt->fetch_binary_progress = 0.0f;
    current_rt->fetch_binary_expected = expected;
    strncpy(current_rt->fetch_binary_dest, dest, sizeof(current_rt->fetch_binary_dest) - 1);
    current_rt->fetch_binary_dest[sizeof(current_rt->fetch_binary_dest) - 1] = 0;
    // Remove stale files
    unlink("/tmp/ac_fb_rc");
    unlink("/tmp/ac_fb_err");
    unlink(dest);
    // Start curl in background
    char cmd[1024];
    snprintf(cmd, sizeof(cmd),
        "sh -c 'curl -fSL --retry 3 --retry-delay 1 --connect-timeout 8 --max-time 600 "
        "--cacert /etc/pki/tls/certs/ca-bundle.crt "
        "--output \"%s\" \"%s\" 2>/tmp/ac_fb_err;"
        " echo $? > /tmp/ac_fb_rc' &", dest, url);
    ac_log("[fetchBinary] cmd: curl -> %s\n", dest);
    system(cmd);
    JS_FreeCString(ctx, url);
    JS_FreeCString(ctx, dest);
    return JS_UNDEFINED;
}

// Detect the EFI partition we booted from:
// - If /sys/block/sda/removable == 1 → USB → /dev/sda1
// - Else if /dev/nvme0n1p1 exists → NVMe internal → /dev/nvme0n1p1
// - Else if /dev/mmcblk0p1 exists → eMMC internal (Chromebooks) → /dev/mmcblk0p1
// - Fallback: /dev/sda1
static void detect_boot_device(char *out, size_t len) {
    char removable[8] = {0};
    if (read_sysfs("/sys/block/sda/removable", removable, sizeof(removable)) > 0
            && removable[0] == '1') {
        snprintf(out, len, "/dev/sda1");
        ac_log("[flash] boot device: USB (/dev/sda1, removable)");
        return;
    }
    if (access("/dev/nvme0n1p1", F_OK) == 0) {
        snprintf(out, len, "/dev/nvme0n1p1");
        ac_log("[flash] boot device: NVMe (/dev/nvme0n1p1)");
        return;
    }
    if (access("/dev/mmcblk0p1", F_OK) == 0) {
        snprintf(out, len, "/dev/mmcblk0p1");
        ac_log("[flash] boot device: eMMC (/dev/mmcblk0p1)");
        return;
    }
    // Fallback — try sda1 anyway
    snprintf(out, len, "/dev/sda1");
    ac_log("[flash] boot device: fallback (/dev/sda1)");
}

// Pure C file copy (no shell needed)
static long flash_copy_file(const char *src, const char *dst) {
    FILE *in = fopen(src, "rb");
    if (!in) {
        ac_log("[flash] fopen src failed: %s errno=%d (%s)", src, errno, strerror(errno));
        return -1;
    }
    // Delete destination first, then sync to flush vfat metadata
    unlink(dst);
    sync();
    FILE *out = fopen(dst, "wb");
    if (!out) {
        ac_log("[flash] fopen dst failed: %s errno=%d (%s)", dst, errno, strerror(errno));
        fclose(in);
        return -1;
    }
    char buf[65536];
    long total = 0;
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) {
            ac_log("[flash] fwrite failed at offset %ld errno=%d (%s)", total, errno, strerror(errno));
            total = -1;
            break;
        }
        total += n;
    }
    // Flush stdio buffer, then fsync to force data+metadata to physical disk
    // (critical for vfat — without this, data stays in page cache and reboot loses it)
    if (total > 0) {
        fflush(out);
        if (fsync(fileno(out)) != 0) {
            ac_log("[flash] fsync failed: errno=%d (%s)", errno, strerror(errno));
            total = -1;
        }
    }
    fclose(out);
    fclose(in);
    return total;
}

// Byte-for-byte verification: evict destination page cache, re-read from disk,
// compare against source (which lives in tmpfs and must NOT be evicted).
// Returns number of verified bytes, or -1 on mismatch/error.
static void flash_trace(const char *fmt, ...);
static void flash_trace_open(void);
static void flash_trace_close_and_archive(void);
static long flash_verify(const char *src, const char *dst) {
    // SIZE CHECK FIRST — cheap, diagnostic, catches the common failure
    // where the copy was short (disk full, sync incomplete, device removed
    // mid-write, etc.) without needing to read 272 MB twice.
    struct stat src_st, dst_st;
    if (stat(src, &src_st) != 0) {
        ac_log("[verify] stat src=%s failed errno=%d", src, errno);
        flash_trace("verify: stat src FAILED errno=%d", errno);
        return -1;
    }
    if (stat(dst, &dst_st) != 0) {
        ac_log("[verify] stat dst=%s failed errno=%d", dst, errno);
        flash_trace("verify: stat dst FAILED errno=%d", errno);
        return -1;
    }
    flash_trace("verify: src=%ld dst=%ld", (long)src_st.st_size, (long)dst_st.st_size);
    if (src_st.st_size != dst_st.st_size) {
        ac_log("[verify] SIZE MISMATCH: src=%ld dst=%ld (diff=%ld)",
               (long)src_st.st_size, (long)dst_st.st_size,
               (long)(src_st.st_size - dst_st.st_size));
        flash_trace("verify: SIZE MISMATCH src=%ld dst=%ld",
                    (long)src_st.st_size, (long)dst_st.st_size);
        return -1;
    }

    // Evict ONLY the destination file's page cache using posix_fadvise.
    // DO NOT use drop_caches — that nukes tmpfs pages and destroys the source!
    int dst_fd = open(dst, O_RDONLY);
    if (dst_fd >= 0) {
        posix_fadvise(dst_fd, 0, dst_st.st_size, POSIX_FADV_DONTNEED);
        ac_log("[verify] evicted dst page cache (%ld bytes)", (long)dst_st.st_size);
        flash_trace("verify: evicted page cache for dst");
        close(dst_fd);
    }

    FILE *fa = fopen(src, "rb");
    FILE *fb = fopen(dst, "rb");
    if (!fa || !fb) {
        ac_log("[verify] fopen failed: src=%s dst=%s", fa ? "ok" : "FAIL", fb ? "ok" : "FAIL");
        flash_trace("verify: fopen src=%s dst=%s", fa ? "ok" : "FAIL", fb ? "ok" : "FAIL");
        if (fa) fclose(fa);
        if (fb) fclose(fb);
        return -1;
    }

    char bufa[65536], bufb[65536];
    long verified = 0;
    size_t na, nb;
    while ((na = fread(bufa, 1, sizeof(bufa), fa)) > 0) {
        nb = fread(bufb, 1, sizeof(bufb), fb);
        if (na != nb || memcmp(bufa, bufb, na) != 0) {
            ac_log("[verify] MISMATCH at offset %ld (read %zu vs %zu)", verified, na, nb);
            flash_trace("verify: MISMATCH at offset=%ld src_read=%zu dst_read=%zu",
                        verified, na, nb);
            fclose(fa);
            fclose(fb);
            return -1;
        }
        verified += (long)na;
    }
    // Make sure dst doesn't have extra bytes
    nb = fread(bufb, 1, 1, fb);
    if (nb != 0) {
        ac_log("[verify] dst has extra bytes after %ld", verified);
        flash_trace("verify: dst has extra bytes after %ld", verified);
        fclose(fa);
        fclose(fb);
        return -1;
    }
    fclose(fa);
    fclose(fb);
    ac_log("[verify] OK: %ld bytes match", verified);
    flash_trace("verify: OK %ld bytes match", verified);
    return verified;
}

// Write a line to the flash telemetry ring buffer (thread-safe enough for single writer)
static void flash_tlog(ACRuntime *rt, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int idx = rt->flash_log_count % 16;
    vsnprintf(rt->flash_log[idx], 128, fmt, ap);
    va_end(ap);
    __sync_fetch_and_add(&rt->flash_log_count, 1);
}

// Append a timestamped line to /tmp/flash-trace.log, which lives in tmpfs so
// it can't be affected by the vfat partition we're flashing. On flash
// completion (success OR failure) this file is copied to /mnt/flash-last.log
// for post-mortem. This is INDEPENDENT of ac_log — the previous design
// paused ac_log across the entire flash and lost all diagnostic output when
// the flash thread hung or crashed. flash_trace NEVER pauses.
static FILE *flash_trace_fp = NULL;
static void flash_trace_open(void) {
    if (flash_trace_fp) { fclose(flash_trace_fp); flash_trace_fp = NULL; }
    // Truncate per-flash so we get a fresh log
    flash_trace_fp = fopen("/tmp/flash-trace.log", "w");
    if (flash_trace_fp) {
        fprintf(flash_trace_fp, "=== flash-trace %ld ===\n", (long)time(NULL));
        fflush(flash_trace_fp);
    }
}
static void flash_trace(const char *fmt, ...) {
    if (!flash_trace_fp) return;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    fprintf(flash_trace_fp, "[%ld.%03ld] ", (long)ts.tv_sec, ts.tv_nsec / 1000000);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(flash_trace_fp, fmt, ap);
    va_end(ap);
    if (fmt[0] && fmt[strlen(fmt) - 1] != '\n') fputc('\n', flash_trace_fp);
    fflush(flash_trace_fp);
    // No fsync — tmpfs doesn't need it and we want speed
}
static void flash_trace_close_and_archive(void) {
    if (flash_trace_fp) { fclose(flash_trace_fp); flash_trace_fp = NULL; }
    // Copy to /mnt/flash-last.log for post-mortem after flash completes.
    // If /mnt is mid-remount or busy, this might fail silently — that's OK,
    // the tmpfs copy is still available while the process lives.
    FILE *src = fopen("/tmp/flash-trace.log", "rb");
    if (!src) return;
    FILE *dst = fopen("/mnt/flash-last.log", "wb");
    if (dst) {
        char buf[4096];
        size_t n;
        while ((n = fread(buf, 1, sizeof(buf), src)) > 0) fwrite(buf, 1, n, dst);
        fflush(dst);
        fsync(fileno(dst));
        fclose(dst);
    }
    fclose(src);
}

// Flash update background thread: mount EFI, copy vmlinuz, sync, umount
// All operations use C syscalls — no shell commands (cp/mkdir/mount not in initramfs PATH)
// NOTE: current_rt is __thread (thread-local); pass ACRuntime * via arg instead
static void *flash_thread_fn(void *arg) {
    ACRuntime *rt = (ACRuntime *)arg;
    if (!rt) return NULL;
    rt->flash_log_count = 0;
    rt->flash_dst[0] = '\0';
    rt->flash_same_device = 0;

    // Open a dedicated trace log in tmpfs. This survives even if the main
    // log gets suspended / the vfat fs we're flashing goes read-only / the
    // flash thread hangs. At the end of the thread (success or failure) we
    // copy it to /mnt/flash-last.log for post-mortem on the next boot.
    flash_trace_open();
    flash_trace("flash_thread_fn start");
    flash_trace("src=%s device=%s", rt->flash_src, rt->flash_device);

    ac_log("[flash] starting: src=%s device=%s", rt->flash_src, rt->flash_device);
    flash_tlog(rt, "src=%s", rt->flash_src);
    flash_tlog(rt, "device=%s", rt->flash_device);

    // Check if device node exists
    {
        struct stat dev_st;
        if (stat(rt->flash_device, &dev_st) != 0) {
            ac_log("[flash] device %s does not exist (errno=%d %s)",
                   rt->flash_device, errno, strerror(errno));
            flash_trace("device %s missing errno=%d", rt->flash_device, errno);
            flash_trace_close_and_archive();
            rt->flash_phase = 4;
            rt->flash_ok = 0;
            rt->flash_pending = 0;
            rt->flash_done = 1;
            return NULL;
        }
        ac_log("[flash] device %s exists (mode=0%o)", rt->flash_device, dev_st.st_mode);
        flash_trace("device exists mode=0%o", dev_st.st_mode);
    }

    // Mount the EFI partition for flashing.
    // IMPORTANT: If the flash target is the same device already mounted at /mnt
    // (e.g. NVMe-to-NVMe OTA), we MUST use /mnt directly. Mounting the same
    // vfat block device twice causes FAT table corruption — each mount has its
    // own in-memory FAT, and umount of one will overwrite the other's changes.
    const char *efi_mount = NULL;
    int did_mount = 0;

    // Check if flash target is already mounted at /mnt
    extern char log_dev[];  // set during setup_logging()
    int same_as_mnt = (log_dev[0] && strcmp(log_dev, rt->flash_device) == 0);
    rt->flash_same_device = same_as_mnt;
    flash_tlog(rt, "log_dev=%s same=%d", log_dev, same_as_mnt);

    if (same_as_mnt) {
        // Same device as /mnt — use it directly, never double-mount
        if (access("/mnt/EFI/BOOT", F_OK) == 0 || access("/mnt/EFI", F_OK) == 0) {
            efi_mount = "/mnt";
            ac_log("[flash] flash target = boot device (%s), using /mnt directly", rt->flash_device);
        } else {
            ac_log("[flash] flash target = boot device but /mnt has no EFI dir");
            // Create EFI structure on /mnt
            mkdir("/mnt/EFI", 0755);
            mkdir("/mnt/EFI/BOOT", 0755);
            if (access("/mnt/EFI/BOOT", F_OK) == 0) {
                efi_mount = "/mnt";
                ac_log("[flash] created EFI/BOOT on /mnt");
            }
        }
    }

    if (!efi_mount) {
        // Different device — mount fresh at /tmp/efi
        mkdir("/tmp/efi", 0755);
        umount("/tmp/efi");  // clean up any stale mount

        const char *fstypes[] = {"vfat", "ext4", "ext2", NULL};
        for (int fi = 0; fstypes[fi] && !did_mount; fi++) {
            int mr = mount(rt->flash_device, "/tmp/efi", fstypes[fi], 0, NULL);
            if (mr == 0) {
                efi_mount = "/tmp/efi";
                did_mount = 1;
                ac_log("[flash] mounted %s at /tmp/efi (type=%s)", rt->flash_device, fstypes[fi]);
            } else {
                ac_log("[flash] mount %s as %s failed: errno=%d (%s)",
                       rt->flash_device, fstypes[fi], errno, strerror(errno));
            }
        }

        // If all mounts failed and this is NVMe, try formatting as vfat (fresh drive)
        if (!did_mount && strstr(rt->flash_device, "nvme")) {
            ac_log("[flash] NVMe mount failed — attempting mkfs.vfat on %s", rt->flash_device);
            char cmd[256];
            snprintf(cmd, sizeof(cmd), "mkfs.vfat -F 32 -n ACBOOT %s 2>&1", rt->flash_device);
            FILE *p = popen(cmd, "r");
            if (p) {
                char buf[256];
                while (fgets(buf, sizeof(buf), p)) ac_log("[flash] mkfs: %s", buf);
                int rc = pclose(p);
                ac_log("[flash] mkfs exit=%d", rc);
                if (rc == 0) {
                    if (mount(rt->flash_device, "/tmp/efi", "vfat", 0, NULL) == 0) {
                        efi_mount = "/tmp/efi";
                        did_mount = 1;
                        ac_log("[flash] mounted fresh vfat on %s", rt->flash_device);
                    }
                }
            }
        }

        // Last resort: check /mnt
        if (!did_mount) {
            if (access("/mnt/EFI/BOOT", F_OK) == 0) {
                efi_mount = "/mnt";
                ac_log("[flash] using existing /mnt mount (fresh mount failed)");
                flash_trace("falling back to existing /mnt mount");
            } else {
                ac_log("[flash] ABORT: mount %s failed and /mnt has no EFI/BOOT",
                       rt->flash_device);
                flash_trace("ABORT mount %s failed and no /mnt/EFI/BOOT", rt->flash_device);
                flash_trace_close_and_archive();
                rt->flash_phase = 4;
                rt->flash_ok = 0;
                rt->flash_pending = 0;
                rt->flash_done = 1;
                return NULL;
            }
        }
    }
    flash_trace("efi_mount=%s did_mount=%d", efi_mount, did_mount);
    // Create EFI directory structure if it doesn't exist (fresh NVMe install)
    char efi_dir[512];
    snprintf(efi_dir, sizeof(efi_dir), "%s/EFI", efi_mount);
    mkdir(efi_dir, 0755);
    snprintf(efi_dir, sizeof(efi_dir), "%s/EFI/BOOT", efi_mount);
    mkdir(efi_dir, 0755);
    if (access(efi_dir, F_OK) != 0) {
        ac_log("[flash] ABORT: %s could not be created", efi_dir);
        flash_trace("ABORT could not create %s", efi_dir);
        flash_trace_close_and_archive();
        if (did_mount) umount("/tmp/efi");
        rt->flash_phase = 4;
        rt->flash_ok = 0;
        rt->flash_pending = 0;
        rt->flash_done = 1;
        return NULL;
    }

    // If splash chainloader is present, kernel lives at KERNEL.EFI; else BOOTX64.EFI
    char dst[512];
    char kernel_path[512];
    snprintf(kernel_path, sizeof(kernel_path), "%s/EFI/BOOT/KERNEL.EFI", efi_mount);
    if (access(kernel_path, F_OK) == 0) {
        snprintf(dst, sizeof(dst), "%s", kernel_path);
        ac_log("[flash] chainloader detected, updating KERNEL.EFI");
    } else {
        snprintf(dst, sizeof(dst), "%s/EFI/BOOT/BOOTX64.EFI", efi_mount);
        ac_log("[flash] no chainloader, updating BOOTX64.EFI");
    }
    ac_log("[flash] destination: %s (efi_mount=%s, same_device=%d, did_mount=%d)",
           dst, efi_mount, same_as_mnt, did_mount);
    strncpy(rt->flash_dst, dst, sizeof(rt->flash_dst) - 1);
    rt->flash_dst[sizeof(rt->flash_dst) - 1] = '\0';
    flash_tlog(rt, "dst=%s mount=%s", dst, efi_mount);

    // NOTE: we intentionally no longer ac_log_pause() the main log during
    // the flash. The old design closed /mnt/ac-native.log while writing to
    // the same vfat partition, which caused every flash failure to leave
    // zero diagnostic trail — if the thread hung or the mount went
    // read-only mid-write, ac_log_resume() never fired and the entire
    // session's logs after the pause were silently dropped. Instead we
    // write flash progress to flash_trace (tmpfs, can't be affected by the
    // partition we're flashing) AND best-effort to the main log. At the
    // end we archive the trace to /mnt/flash-last.log.
    int same_mount = same_as_mnt || !did_mount;
    flash_trace("same_mount=%d same_as_mnt=%d did_mount=%d", same_mount, same_as_mnt, did_mount);

    // Pre-flight: validate source file exists and has reasonable size
    {
        struct stat src_st;
        if (stat(rt->flash_src, &src_st) != 0 || src_st.st_size < 1048576) {
            ac_log("[flash] ABORT: source %s invalid (size=%ld, errno=%d)",
                   rt->flash_src, (long)(src_st.st_size), errno);
            flash_trace("ABORT source invalid size=%ld errno=%d",
                        (long)(src_st.st_size), errno);
            flash_trace_close_and_archive();
            if (did_mount) umount("/tmp/efi");
            rt->flash_phase = 4;
            rt->flash_ok = 0;
            rt->flash_pending = 0;
            rt->flash_done = 1;
            return NULL;
        }
        ac_log("[flash] source validated: %ld bytes", (long)src_st.st_size);
        flash_trace("source validated size=%ld", (long)src_st.st_size);
    }

    // Phase 1: Backup previous kernel, then write new one
    rt->flash_phase = 1;

    // Keep previous version as .prev for rollback
    {
        char prev[512];
        snprintf(prev, sizeof(prev), "%s.prev", dst);
        if (access(dst, F_OK) == 0) {
            // Remove old .prev, rename current to .prev
            unlink(prev);
            if (rename(dst, prev) == 0) {
                ac_log("[flash] backed up previous kernel to %s", prev);
                flash_tlog(rt, "backup=%s", prev);
            } else {
                ac_log("[flash] backup rename failed (errno=%d), continuing anyway", errno);
            }
        }
    }

    flash_tlog(rt, "writing %s -> %s", rt->flash_src, dst);
    long copied = flash_copy_file(rt->flash_src, dst);
    flash_tlog(rt, "wrote %ld bytes", copied);

    // Also update Microsoft Boot Manager path (ThinkPad BIOS often boots this first)
    // Check available space first — don't write second copy if it won't fit.
    // FAT32 can't hardlink, so we need actual space for both copies.
    {
        char ms_dir[512], ms_dst[512];
        snprintf(ms_dir, sizeof(ms_dir), "%s/EFI/Microsoft", efi_mount);
        mkdir(ms_dir, 0755);
        snprintf(ms_dir, sizeof(ms_dir), "%s/EFI/Microsoft/Boot", efi_mount);
        mkdir(ms_dir, 0755);
        snprintf(ms_dst, sizeof(ms_dst), "%s/EFI/Microsoft/Boot/bootmgfw.efi", efi_mount);

        // Check free space on partition before writing second copy
        struct statvfs vfs;
        long free_bytes = 0;
        if (statvfs(efi_mount, &vfs) == 0) {
            free_bytes = (long)vfs.f_bavail * (long)vfs.f_bsize;
        }
        long need_bytes = copied + (1024 * 1024); // need kernel size + 1MB margin

        if (free_bytes >= need_bytes) {
            // Backup old MS boot path too
            char ms_prev[512];
            snprintf(ms_prev, sizeof(ms_prev), "%s.prev", ms_dst);
            unlink(ms_prev);
            if (access(ms_dst, F_OK) == 0) {
                rename(ms_dst, ms_prev);  // safe: old version preserved
            }
            long ms_copied = flash_copy_file(rt->flash_src, ms_dst);
            if (ms_copied > 0) {
                unlink(ms_prev);  // success: remove backup
                ac_log("[flash] also wrote Microsoft boot path: %ld bytes -> %s", ms_copied, ms_dst);
                flash_tlog(rt, "ms_boot=%ld", ms_copied);
            } else {
                // Write failed — restore backup
                if (access(ms_prev, F_OK) == 0) {
                    rename(ms_prev, ms_dst);
                    ac_log("[flash] MS boot write failed, restored previous");
                }
            }
        } else {
            ac_log("[flash] skipping MS boot path: %ldMB free < %ldMB needed",
                   free_bytes / 1048576, need_bytes / 1048576);
            flash_tlog(rt, "ms_boot=skipped (space)");
        }
    }

    // Optional initramfs copy — for OTA updates where the kernel is now
    // slim (Phase 2 de-embed) and must load `\initramfs.cpio.gz` from the
    // ESP root via its baked-in `initrd=` cmdline. Without this step, an
    // os.mjs OTA would leave the new kernel paired with the PREVIOUS
    // initramfs, which is a latent source of boot breakage whenever any
    // file inside initramfs changes.
    if (rt->flash_initramfs_src[0]) {
        char initramfs_dst[512];
        snprintf(initramfs_dst, sizeof(initramfs_dst), "%s/initramfs.cpio.gz", efi_mount);
        ac_log("[flash] writing initramfs: %s -> %s", rt->flash_initramfs_src, initramfs_dst);
        flash_tlog(rt, "initramfs: %s -> %s", rt->flash_initramfs_src, initramfs_dst);
        // Keep previous as .prev for rollback
        char initramfs_prev[512];
        snprintf(initramfs_prev, sizeof(initramfs_prev), "%s.prev", initramfs_dst);
        if (access(initramfs_dst, F_OK) == 0) {
            unlink(initramfs_prev);
            rename(initramfs_dst, initramfs_prev);
        }
        long initramfs_copied = flash_copy_file(rt->flash_initramfs_src, initramfs_dst);
        flash_tlog(rt, "initramfs wrote %ld bytes", initramfs_copied);
        if (initramfs_copied <= 0) {
            ac_log("[flash] initramfs copy FAILED (copied=%ld) — restoring .prev",
                   initramfs_copied);
            if (access(initramfs_prev, F_OK) == 0) {
                unlink(initramfs_dst);
                rename(initramfs_prev, initramfs_dst);
            }
            // Non-fatal to the kernel write — but flag as failure since the
            // new kernel won't boot without a matching initramfs.
            flash_trace_close_and_archive();
            rt->flash_phase = 4;
            rt->flash_ok = 0;
            rt->flash_pending = 0;
            rt->flash_done = 1;
            return NULL;
        }
    }

    // Phase 2: Syncing to disk — belt-and-suspenders for vfat
    rt->flash_phase = 2;

    // Force page cache eviction of the written file so subsequent reads hit disk
    {
        int dst_fd = open(dst, O_RDONLY);
        if (dst_fd >= 0) {
            struct stat st;
            if (fstat(dst_fd, &st) == 0) {
                posix_fadvise(dst_fd, 0, st.st_size, POSIX_FADV_DONTNEED);
                ac_log("[flash] evicted written file from page cache (%ld bytes)", (long)st.st_size);
            }
            close(dst_fd);
        }
    }

    // syncfs + multi-round sync with generous waits. USB flash controllers
    // can take SECONDS to flush 272 MB through vfat metadata, and the old
    // 500 ms window was frequently insufficient — we'd then try to verify
    // against data that still existed only in the kernel's dirty page
    // cache, so the eviction step exposed a mismatch with uncommitted
    // backing store bytes.
    int mnt_fd = open(efi_mount, O_RDONLY);
    if (mnt_fd >= 0) {
        int sr = syncfs(mnt_fd);
        if (sr != 0) {
            ac_log("[flash] syncfs failed: errno=%d (%s)", errno, strerror(errno));
            flash_trace("syncfs FAILED errno=%d", errno);
        } else {
            flash_trace("syncfs ok");
        }
        close(mnt_fd);
    } else {
        ac_log("[flash] WARNING: open(%s) for syncfs failed errno=%d", efi_mount, errno);
        flash_trace("open(efi_mount) for syncfs FAILED errno=%d", errno);
    }
    sync();
    flash_trace("sync #1 returned");
    // First wait — let the block layer start flushing
    usleep(1500000);  // 1.5 s
    sync();
    flash_trace("sync #2 after 1.5s wait");
    // Second wait — USB flash write can be very slow for 272 MB
    usleep(1500000);  // another 1.5 s = 3 s total
    sync();
    flash_trace("sync #3 after 3s total wait");

    // Previously we remounted /mnt ro+rw to force vfat metadata flush. That
    // was fragile: it bypassed the VFS safely-unmount path, briefly made
    // /mnt read-only so any concurrent log write would fail, and on systems
    // with mount stacking (init.sh leaves nvme0n1p1 stacked under sda1)
    // the remount hit the wrong layer. syncfs + sync + 3 s of settle time
    // is more reliable.
    ac_log("[flash] sync complete");
    flash_trace("sync complete (3 rounds, 3s total wait)");

    if (copied <= 0) {
        ac_log("[flash] copy failed (copied=%ld)", copied);
        flash_trace("copy FAILED copied=%ld", copied);
        flash_trace_close_and_archive();
        rt->flash_phase = 4;
        rt->flash_ok = 0;
        rt->flash_pending = 0;
        rt->flash_done = 1;
        return NULL;
    }

    // Phase 3: Verify — read back from physical disk and compare byte-for-byte.
    // flash_verify now does a size-check first (fast, diagnostic) before
    // paying the cost of a 272 MB read+compare.
    rt->flash_phase = 3;
    flash_trace("verify starting (expecting %ld bytes)", copied);
    long verified = flash_verify(rt->flash_src, dst);
    flash_tlog(rt, "verify=%ld (expected=%ld)", verified, copied);
    flash_trace("verify returned: %ld", verified);

    if (did_mount) {
        umount("/tmp/efi");
        ac_log("[flash] unmounted /tmp/efi");
        flash_trace("unmounted /tmp/efi");
    }

    rt->flash_verified_bytes = verified;
    if (verified != copied) {
        ac_log("[flash] VERIFY FAILED: wrote %ld, verified %ld", copied, verified);
        flash_trace("VERIFY FAILED wrote=%ld verified=%ld", copied, verified);
        // CRITICAL: restore previous kernel so device remains bootable
        {
            char prev[512];
            snprintf(prev, sizeof(prev), "%s.prev", dst);
            if (access(prev, F_OK) == 0) {
                unlink(dst);
                if (rename(prev, dst) == 0) {
                    ac_log("[flash] RESTORED previous kernel from %s — device still bootable", prev);
                    flash_trace("RESTORED previous kernel from %s", prev);
                    sync();
                } else {
                    ac_log("[flash] CRITICAL: restore failed errno=%d", errno);
                    flash_trace("CRITICAL restore FAILED errno=%d", errno);
                }
            } else {
                flash_trace("no .prev backup to restore (device may be unbootable)");
            }
        }
        flash_trace_close_and_archive();
        rt->flash_phase = 4;
        rt->flash_ok = 0;
        rt->flash_pending = 0;
        rt->flash_done = 1;
        return NULL;
    }

    // Set UEFI boot order so firmware boots our kernel (not a stale vendor entry).
    // Many OEM firmwares (HP, Dell, Lenovo) have hardcoded boot entries pointing at
    // vendor-specific EFI paths. Without updating the boot order, the firmware may
    // boot an old kernel (e.g. from a previous OS install) instead of ours.
    {
        // Mount efivarfs if not already mounted (needed to write UEFI variables)
        struct stat evs;
        if (stat("/sys/firmware/efi/efivars", &evs) == 0) {
            // Try mounting (harmless if already mounted)
            mount("efivarfs", "/sys/firmware/efi/efivars", "efivarfs", 0, NULL);

            // Extract disk device from partition path for efibootmgr --disk
            // e.g. /dev/sda1 -> /dev/sda, /dev/nvme0n1p1 -> /dev/nvme0n1
            char disk[64] = "", partnum[8] = "";
            strncpy(disk, rt->flash_device, sizeof(disk) - 1);
            char *p = disk + strlen(disk) - 1;
            // Walk back past the partition number
            while (p > disk && *p >= '0' && *p <= '9') p--;
            if (p > disk) {
                strncpy(partnum, p + 1, sizeof(partnum) - 1);
                // For NVMe: /dev/nvme0n1p1 -> strip 'p' before part number
                if (*p == 'p' && p > disk && *(p-1) >= '0' && *(p-1) <= '9')
                    *p = '\0';
                else
                    *(p + 1) = '\0';
            }

            // Use efibootmgr if available (cleanest approach)
            char cmd[512];
            snprintf(cmd, sizeof(cmd),
                "efibootmgr -c -d %s -p %s -L 'AC Native OS' "
                "-l '\\EFI\\BOOT\\BOOTX64.EFI' 2>&1",
                disk, partnum[0] ? partnum : "1");
            ac_log("[flash] setting UEFI boot entry: %s", cmd);
            flash_tlog(rt, "efibootmgr: %s %s part=%s", disk, partnum, partnum[0] ? partnum : "1");

            FILE *efip = popen(cmd, "r");
            if (efip) {
                char buf[256];
                while (fgets(buf, sizeof(buf), efip)) {
                    // Trim newline
                    char *nl = strchr(buf, '\n');
                    if (nl) *nl = '\0';
                    ac_log("[flash] efibootmgr: %s", buf);
                    flash_tlog(rt, "efi: %s", buf);
                }
                int rc = pclose(efip);
                if (rc == 0) {
                    ac_log("[flash] UEFI boot entry created successfully");
                    flash_tlog(rt, "efi_boot=ok");
                } else {
                    ac_log("[flash] efibootmgr exit=%d (boot entry may not be set)", rc);
                    flash_tlog(rt, "efi_boot=fail(%d)", rc);
                }
            } else {
                ac_log("[flash] efibootmgr not available — UEFI boot order unchanged");
                flash_tlog(rt, "efi_boot=no_efibootmgr");
            }
        } else {
            ac_log("[flash] no EFI variables support — skipping boot order update");
        }
    }

    // Remove downloaded file to free /tmp space
    unlink(rt->flash_src);
    ac_log("[flash] done: %ld bytes written, verified OK", copied);
    flash_trace("done: %ld bytes verified OK", copied);
    flash_trace_close_and_archive();
    rt->flash_phase = 4;
    rt->flash_ok = 1;
    rt->flash_pending = 0;
    rt->flash_done = 1;
    return NULL;
}

// ─────────────────────────────────────────────────────────────────
// Audio diagnostic — list PCMs + play a short test tone on an arbitrary
// device. Used by the speaker.mjs piece to figure out which ALSA PCM
// actually produces sound on a given machine (vs wiring-dependent guesses
// like hw:0,0). The tone plays in a detached thread so the JS caller
// returns immediately; the piece UI stays responsive.
// ─────────────────────────────────────────────────────────────────

static JSValue js_audio_list_pcms(JSContext *ctx, JSValueConst this_val,
                                   int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    JSValue arr = JS_NewArray(ctx);
    int idx = 0;
    for (int c = 0; c < 4; c++) {
        for (int d = 0; d < 8; d++) {
            char path[80];
            snprintf(path, sizeof(path),
                     "/proc/asound/card%d/pcm%dp/info", c, d);
            FILE *fp = fopen(path, "r");
            if (!fp) continue;
            char line[256], id_str[96] = "", name_str[96] = "";
            while (fgets(line, sizeof(line), fp)) {
                char *nl = strchr(line, '\n'); if (nl) *nl = 0;
                if (!strncmp(line, "id: ", 4))
                    snprintf(id_str, sizeof(id_str), "%s", line + 4);
                else if (!strncmp(line, "name: ", 6))
                    snprintf(name_str, sizeof(name_str), "%s", line + 6);
            }
            fclose(fp);
            char dev[16];
            snprintf(dev, sizeof(dev), "hw:%d,%d", c, d);
            JSValue obj = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, obj, "device", JS_NewString(ctx, dev));
            JS_SetPropertyStr(ctx, obj, "card",   JS_NewInt32(ctx, c));
            JS_SetPropertyStr(ctx, obj, "num",    JS_NewInt32(ctx, d));
            JS_SetPropertyStr(ctx, obj, "id",     JS_NewString(ctx, id_str));
            JS_SetPropertyStr(ctx, obj, "name",   JS_NewString(ctx, name_str));
            /* Tag the PCM that ac-native's main audio thread picked so the
             * UI can highlight it. */
            if (current_rt && current_rt->audio &&
                strcmp(current_rt->audio->audio_device, dev) == 0)
                JS_SetPropertyStr(ctx, obj, "active", JS_NewBool(ctx, 1));
            JS_SetPropertyUint32(ctx, arr, idx++, obj);
        }
    }
    return arr;
}

struct audio_probe_args {
    char  device[32];
    int   freq_hz;
    int   duration_ms;
    float volume;
};

static void *audio_probe_thread(void *arg) {
    struct audio_probe_args *a = (struct audio_probe_args *)arg;
    snd_pcm_t *pcm = NULL;
    int err = snd_pcm_open(&pcm, a->device, SND_PCM_STREAM_PLAYBACK, 0);
    if (err < 0) {
        ac_log("[audio-probe] open %s failed: %s",
               a->device, snd_strerror(err));
        free(a);
        return NULL;
    }
    unsigned int rate = 48000;
    snd_pcm_uframes_t period = 480, buffer = 1920;
    err = snd_pcm_set_params(pcm,
                             SND_PCM_FORMAT_S16_LE,
                             SND_PCM_ACCESS_RW_INTERLEAVED,
                             2,              /* stereo */
                             rate,
                             1,              /* soft_resample */
                             100 * 1000);    /* 100ms latency */
    if (err < 0) {
        ac_log("[audio-probe] set_params %s failed: %s",
               a->device, snd_strerror(err));
        snd_pcm_close(pcm);
        free(a);
        return NULL;
    }
    /* Synthesize + play a sine wave of a->freq_hz at a->volume for
     * a->duration_ms milliseconds. Stereo S16 LE. */
    int total_frames = (long long)rate * a->duration_ms / 1000;
    int16_t *buf = (int16_t *)malloc(period * 2 * sizeof(int16_t));
    if (!buf) { snd_pcm_close(pcm); free(a); return NULL; }
    double phase = 0.0;
    double step  = 2.0 * 3.14159265358979 * (double)a->freq_hz / (double)rate;
    int32_t amp  = (int32_t)(a->volume * 32760.0);
    if (amp < 0) amp = 0; if (amp > 32760) amp = 32760;
    int written = 0;
    ac_log("[audio-probe] %s %dHz %dms vol=%.2f", a->device, a->freq_hz,
           a->duration_ms, a->volume);
    while (written < total_frames) {
        int chunk = total_frames - written;
        if (chunk > (int)period) chunk = (int)period;
        for (int i = 0; i < chunk; i++) {
            int16_t s = (int16_t)(sin(phase) * amp);
            buf[i * 2]     = s;
            buf[i * 2 + 1] = s;
            phase += step;
            if (phase > 6.283185307179586) phase -= 6.283185307179586;
        }
        snd_pcm_sframes_t w = snd_pcm_writei(pcm, buf, chunk);
        if (w < 0) w = snd_pcm_recover(pcm, w, 1);
        if (w < 0) { ac_log("[audio-probe] writei failed: %s",
                            snd_strerror((int)w)); break; }
        written += (int)w;
    }
    snd_pcm_drain(pcm);
    snd_pcm_close(pcm);
    free(buf);
    ac_log("[audio-probe] %s done (%d frames)", a->device, written);
    free(a);
    return NULL;
}

static JSValue js_audio_test_pcm(JSContext *ctx, JSValueConst this_val,
                                  int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_FALSE;
    struct audio_probe_args *a = (struct audio_probe_args *)calloc(1, sizeof(*a));
    if (!a) return JS_FALSE;
    const char *dev = JS_ToCString(ctx, argv[0]);
    if (!dev) { free(a); return JS_FALSE; }
    strncpy(a->device, dev, sizeof(a->device) - 1);
    JS_FreeCString(ctx, dev);
    a->freq_hz     = 440;
    a->duration_ms = 500;
    a->volume      = 0.3f;
    if (argc >= 2) JS_ToInt32(ctx, &a->freq_hz,     argv[1]);
    if (argc >= 3) JS_ToInt32(ctx, &a->duration_ms, argv[2]);
    if (argc >= 4) {
        double vol; JS_ToFloat64(ctx, &vol, argv[3]);
        a->volume = (float)vol;
    }
    pthread_t th;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    int pr = pthread_create(&th, &attr, audio_probe_thread, a);
    pthread_attr_destroy(&attr);
    if (pr != 0) { free(a); return JS_FALSE; }
    return JS_TRUE;
}

// ─────────────────────────────────────────────────────────────────
// Firmware update thread — runs /bin/ac-firmware-install (bundled copy of
// system/public/install-firmware.sh) and streams its stdout into
// rt->fw_log. The script handles all the dangerous bits (backup via
// flashrom -r, CBFS swap via cbfstool, flashrom -w) — we just orchestrate
// + surface progress. Availability is gated by os.mjs before this runs.
// ─────────────────────────────────────────────────────────────────
static void fw_log_line(ACRuntime *rt, const char *line) {
    int idx = rt->fw_log_count % 32;
    strncpy(rt->fw_log[idx], line, sizeof(rt->fw_log[idx]) - 1);
    rt->fw_log[idx][sizeof(rt->fw_log[idx]) - 1] = 0;
    __sync_fetch_and_add(&rt->fw_log_count, 1);
    ac_log("[fw] %s", line);
}

static void *fw_thread_fn(void *arg) {
    ACRuntime *rt = (ACRuntime *)arg;
    if (!rt) return NULL;
    rt->fw_log_count = 0;
    rt->fw_backup_path[0] = 0;
    if (access("/bin/ac-firmware-install", X_OK) != 0) {
        fw_log_line(rt, "ac-firmware-install not bundled in initramfs");
        rt->fw_ok = 0;
        rt->fw_pending = 0;
        rt->fw_done = 1;
        return NULL;
    }
    char cmd[512];
    snprintf(cmd, sizeof(cmd),
             "AC_CDN=https://aesthetic.computer "
             "SPLASH_URL=https://oven.aesthetic.computer/firmware/splash.bmp "
             "/bin/ac-firmware-install %s 2>&1",
             rt->fw_args[0] ? rt->fw_args : "");
    fw_log_line(rt, "starting ac-firmware-install");
    FILE *p = popen(cmd, "r");
    if (!p) {
        fw_log_line(rt, "popen failed");
        rt->fw_ok = 0;
        rt->fw_pending = 0;
        rt->fw_done = 1;
        return NULL;
    }
    char line[256];
    while (fgets(line, sizeof(line), p)) {
        char *nl = strchr(line, '\n');
        if (nl) *nl = 0;
        // Capture the backup path line so JS can show it for "copy to USB"
        const char *bk = strstr(line, "/tmp/firmware-backup-");
        if (bk) {
            int i = 0;
            while (bk[i] && bk[i] != ' ' && i < (int)sizeof(rt->fw_backup_path) - 1) {
                rt->fw_backup_path[i] = bk[i]; i++;
            }
            rt->fw_backup_path[i] = 0;
        }
        fw_log_line(rt, line);
    }
    int rc = pclose(p);
    rt->fw_ok = (WIFEXITED(rc) && WEXITSTATUS(rc) == 0) ? 1 : 0;
    char finish[64];
    snprintf(finish, sizeof(finish), "ac-firmware-install exit=%d",
             WIFEXITED(rc) ? WEXITSTATUS(rc) : -1);
    fw_log_line(rt, finish);
    rt->fw_pending = 0;
    rt->fw_done = 1;
    return NULL;
}

// system.firmwareInstall([mode]) — mode: "install" (default), "dry-run",
// "restore". Returns immediately; poll system.firmware.{pending,done,ok,log}.
static JSValue js_firmware_install(JSContext *ctx, JSValueConst this_val,
                                    int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt) return JS_UNDEFINED;
    if (current_rt->fw_pending) {
        ac_log("[fw] refused: already running");
        return JS_NewBool(ctx, 0);
    }
    current_rt->fw_args[0] = 0;
    if (argc >= 1 && JS_IsString(argv[0])) {
        const char *mode = JS_ToCString(ctx, argv[0]);
        if (mode) {
            if (strcmp(mode, "dry-run") == 0) {
                strncpy(current_rt->fw_args, "--dry-run",
                        sizeof(current_rt->fw_args) - 1);
            } else if (strcmp(mode, "restore") == 0) {
                strncpy(current_rt->fw_args, "--restore",
                        sizeof(current_rt->fw_args) - 1);
            }
            JS_FreeCString(ctx, mode);
        }
    }
    current_rt->fw_pending = 1;
    current_rt->fw_done = 0;
    current_rt->fw_ok = 0;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    int pr = pthread_create(&current_rt->fw_thread, &attr,
                            fw_thread_fn, current_rt);
    pthread_attr_destroy(&attr);
    if (pr != 0) {
        current_rt->fw_pending = 0;
        current_rt->fw_done = 1;
        return JS_NewBool(ctx, 0);
    }
    return JS_NewBool(ctx, 1);
}

// system.flashUpdate(srcPath[, devicePath[, initramfsSrcPath]])
// devicePath defaults to auto-detected boot device if omitted.
// initramfsSrcPath is optional — when provided, the flash thread also copies
// it to `<ESP>/initramfs.cpio.gz` (matching the kernel's baked-in
// `initrd=\initramfs.cpio.gz` cmdline). Required for OTA updates since the
// kernel no longer embeds initramfs (Phase 2 de-embed).
static JSValue js_flash_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) { ac_log("[flash] flashUpdate called with no runtime/args\n"); return JS_UNDEFINED; }
    const char *src = JS_ToCString(ctx, argv[0]);
    if (!src) { ac_log("[flash] flashUpdate: null src arg\n"); return JS_UNDEFINED; }
    ac_log("[flash] flashUpdate called: src=%s\n", src);
    current_rt->flash_pending = 1;
    current_rt->flash_done    = 0;
    current_rt->flash_ok      = 0;
    strncpy(current_rt->flash_src, src, sizeof(current_rt->flash_src) - 1);
    current_rt->flash_src[sizeof(current_rt->flash_src) - 1] = 0;
    JS_FreeCString(ctx, src);
    current_rt->flash_initramfs_src[0] = '\0';
    // Device: use arg[1] if provided, else auto-detect
    if (argc >= 2 && !JS_IsUndefined(argv[1]) && !JS_IsNull(argv[1])) {
        const char *dev = JS_ToCString(ctx, argv[1]);
        if (dev) {
            strncpy(current_rt->flash_device, dev, sizeof(current_rt->flash_device) - 1);
            current_rt->flash_device[sizeof(current_rt->flash_device) - 1] = 0;
            JS_FreeCString(ctx, dev);
        }
    } else {
        detect_boot_device(current_rt->flash_device, sizeof(current_rt->flash_device));
    }
    // Optional initramfs source — arg[2]
    if (argc >= 3 && !JS_IsUndefined(argv[2]) && !JS_IsNull(argv[2])) {
        const char *init_src = JS_ToCString(ctx, argv[2]);
        if (init_src) {
            strncpy(current_rt->flash_initramfs_src, init_src,
                    sizeof(current_rt->flash_initramfs_src) - 1);
            current_rt->flash_initramfs_src[sizeof(current_rt->flash_initramfs_src) - 1] = 0;
            ac_log("[flash] initramfs src=%s", init_src);
            JS_FreeCString(ctx, init_src);
        }
    }
    // Pass runtime as arg (current_rt is thread-local, unavailable in new thread)
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    int pr = pthread_create(&current_rt->flash_thread, &attr, flash_thread_fn, current_rt);
    pthread_attr_destroy(&attr);
    if (pr != 0) {
        ac_log("[flash] pthread_create failed (%d)", pr);
        current_rt->flash_ok = 0;
        current_rt->flash_pending = 0;
        current_rt->flash_done = 1;
    }
    return JS_UNDEFINED;
}

// system.reboot() — triggers system reboot (direct syscall, no external binary needed)
// system.setPowerRole("port0", "source"|"sink") — swap USB-C power role
static JSValue js_set_power_role(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_NewBool(ctx, 0);
    const char *port = JS_ToCString(ctx, argv[0]);
    const char *role = JS_ToCString(ctx, argv[1]);
    if (!port || !role) {
        if (port) JS_FreeCString(ctx, port);
        if (role) JS_FreeCString(ctx, role);
        return JS_NewBool(ctx, 0);
    }
    // Validate role
    if (strcmp(role, "source") != 0 && strcmp(role, "sink") != 0) {
        JS_FreeCString(ctx, port); JS_FreeCString(ctx, role);
        return JS_NewBool(ctx, 0);
    }
    // Validate port name (must be "portN")
    if (strncmp(port, "port", 4) != 0 || !port[4]) {
        JS_FreeCString(ctx, port); JS_FreeCString(ctx, role);
        return JS_NewBool(ctx, 0);
    }
    char path[256];
    snprintf(path, sizeof(path), "/sys/class/typec/%s/power_role", port);
    FILE *f = fopen(path, "w");
    int ok = 0;
    if (f) {
        ok = fprintf(f, "%s\n", role) > 0;
        fclose(f);
        ac_log("[typec] setPowerRole %s -> %s: %s", port, role, ok ? "ok" : "write failed");
    } else {
        ac_log("[typec] setPowerRole: cannot open %s: %s", path, strerror(errno));
    }
    JS_FreeCString(ctx, port);
    JS_FreeCString(ctx, role);
    return JS_NewBool(ctx, ok);
}

static JSValue build_usb_midi_state_obj(JSContext *ctx) {
    ACUsbMidiState state;
    int has_state = usb_midi_read_state(&state);
    if (!has_state) {
        memset(&state, 0, sizeof(state));
        snprintf(state.reason, sizeof(state.reason), "uninitialized");
    }

    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "enabled", JS_NewBool(ctx, state.enabled));
    JS_SetPropertyStr(ctx, obj, "active", JS_NewBool(ctx, state.active));
    JS_SetPropertyStr(ctx, obj, "reason", JS_NewString(ctx, state.reason[0] ? state.reason : "unknown"));
    JS_SetPropertyStr(ctx, obj, "udc", JS_NewString(ctx, state.udc));
    JS_SetPropertyStr(ctx, obj, "port", JS_NewString(ctx, state.port));
    JS_SetPropertyStr(ctx, obj, "powerRole", JS_NewString(ctx, state.power_role));
    JS_SetPropertyStr(ctx, obj, "dataRole", JS_NewString(ctx, state.data_role));
    JS_SetPropertyStr(ctx, obj, "alsaDevice", JS_NewString(ctx, state.alsa_device));
    JS_SetPropertyStr(ctx, obj, "serial", JS_NewString(ctx, state.serial));
    return obj;
}

static JSValue js_usb_midi_status(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    return build_usb_midi_state_obj(ctx);
}

static JSValue js_usb_midi_control(JSContext *ctx, const char *action) {
    usb_midi_close();
    if (access("/scripts/usb-midi-gadget.sh", X_OK) == 0) {
        char cmd[256];
        snprintf(cmd, sizeof(cmd), "/scripts/usb-midi-gadget.sh %s >/tmp/usb-midi-gadget.log 2>&1", action);
        int rc = system(cmd);
        ac_log("[usb-midi] control %s rc=%d", action, rc);
    } else {
        FILE *fp = fopen("/run/usb-midi.state", "w");
        if (fp) {
            fputs("enabled=0\nactive=0\nreason=missing-script\n", fp);
            fclose(fp);
        }
        ac_log("[usb-midi] control %s failed: missing /scripts/usb-midi-gadget.sh", action);
    }
    usb_midi_close();
    return build_usb_midi_state_obj(ctx);
}

static JSValue js_usb_midi_enable(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    return js_usb_midi_control(ctx, "up");
}

static JSValue js_usb_midi_disable(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    return js_usb_midi_control(ctx, "down");
}

static JSValue js_usb_midi_refresh(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    return js_usb_midi_control(ctx, "refresh");
}

static JSValue js_usb_midi_note_on(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int32_t note = 60;
    int32_t velocity = 100;
    int32_t channel = 0;
    if (argc < 1 || JS_ToInt32(ctx, &note, argv[0])) return JS_FALSE;
    if (argc >= 2) JS_ToInt32(ctx, &velocity, argv[1]);
    if (argc >= 3) JS_ToInt32(ctx, &channel, argv[2]);
    return JS_NewBool(ctx, usb_midi_note_on(note, velocity, channel));
}

static JSValue js_usb_midi_note_off(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int32_t note = 60;
    int32_t velocity = 0;
    int32_t channel = 0;
    if (argc < 1 || JS_ToInt32(ctx, &note, argv[0])) return JS_FALSE;
    if (argc >= 2) JS_ToInt32(ctx, &velocity, argv[1]);
    if (argc >= 3) JS_ToInt32(ctx, &channel, argv[2]);
    return JS_NewBool(ctx, usb_midi_note_off(note, velocity, channel));
}

static JSValue js_usb_midi_all_notes_off(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int32_t channel = 0;
    if (argc >= 1) JS_ToInt32(ctx, &channel, argv[0]);
    return JS_NewBool(ctx, usb_midi_all_notes_off(channel));
}

// Hardened: triple sync with delays, pre-reboot EFI file size sanity check,
// perf flush, log flush before rebooting.
static JSValue js_reboot(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)ctx; (void)this_val; (void)argc; (void)argv;
    ac_log("[system] reboot requested");

    // Pre-reboot sanity: verify EFI file exists and is >1MB (catch corrupted flash)
    {
        const char *efi_paths[] = {
            "/mnt/EFI/BOOT/BOOTX64.EFI",
            "/tmp/efi/EFI/BOOT/BOOTX64.EFI",
            NULL
        };
        int efi_ok = 0;
        for (int i = 0; efi_paths[i]; i++) {
            struct stat st;
            if (stat(efi_paths[i], &st) == 0 && st.st_size > 1048576) {
                ac_log("[reboot] EFI verified: %s (%ld bytes)", efi_paths[i], (long)st.st_size);
                efi_ok = 1;
                break;
            }
        }
        if (!efi_ok) {
            ac_log("[reboot] WARNING: no valid EFI file found on mount — rebooting anyway");
        }
    }

    // Flush perf data before reboot
    perf_flush();

    // Play shutdown chime and let audio drain
    if (current_rt && current_rt->audio) {
        audio_shutdown_sound(current_rt->audio);
        usleep(500000);  // 500ms for chime to play
    }

    // Flush log file
    ac_log("[reboot] syncing filesystems...");
    ac_log_flush();

    // Triple sync with delays — vfat write-back needs time
    sync();
    usleep(500000);
    sync();
    usleep(500000);
    sync();

    ac_log("[reboot] executing reboot syscall");
    ac_log_flush();

    // Exit with code 2 — init script sees this as reboot request
    _exit(2);
    return JS_UNDEFINED;
}

// system.poweroff() — signal main loop to run shutdown animation then exit
extern volatile int poweroff_requested;
static JSValue js_poweroff(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)ctx; (void)this_val; (void)argc; (void)argv;
    ac_log("[system] poweroff requested via JS");
    poweroff_requested = 1;  // main loop will run bye animation + exit(0)
    return JS_UNDEFINED;
}

// ============================================================
// JS Native Functions — PTY Terminal Emulator
// ============================================================

// system.pty.spawn(cmd, [args...], cols, rows) — spawn a process in a PTY
static JSValue js_pty_spawn(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_FALSE;
    if (current_rt->pty_active) {
        pty_destroy(&current_rt->pty);
        current_rt->pty_active = 0;
    }

    const char *cmd = JS_ToCString(ctx, argv[0]);
    if (!cmd) return JS_FALSE;

    // Collect args from JS array (argv[1]) or use cmd as argv[0]
    char *child_argv[32] = {0};
    int nargs = 0;
    child_argv[nargs++] = (char *)cmd;

    if (argc > 1 && JS_IsArray(ctx, argv[1])) {
        int len = 0;
        JSValue jlen = JS_GetPropertyStr(ctx, argv[1], "length");
        JS_ToInt32(ctx, &len, jlen);
        JS_FreeValue(ctx, jlen);
        for (int i = 0; i < len && nargs < 30; i++) {
            JSValue el = JS_GetPropertyUint32(ctx, argv[1], i);
            child_argv[nargs++] = (char *)JS_ToCString(ctx, el);
            JS_FreeValue(ctx, el);
        }
    }
    child_argv[nargs] = NULL;

    int cols = 80, rows = 24;
    if (argc > 2) JS_ToInt32(ctx, &cols, argv[2]);
    if (argc > 3) JS_ToInt32(ctx, &rows, argv[3]);

    int ok = pty_spawn(&current_rt->pty, cols, rows, cmd, child_argv);

    // Free ToCString results for args
    for (int i = 1; i < nargs; i++) {
        JS_FreeCString(ctx, child_argv[i]);
    }
    JS_FreeCString(ctx, cmd);

    if (ok == 0) {
        current_rt->pty_active = 1;
        return JS_TRUE;
    }
    return JS_FALSE;
}

// system.pty.write(str) — send input to PTY
static JSValue js_pty_write(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->pty_active || argc < 1) return JS_UNDEFINED;
    size_t len;
    const char *data = JS_ToCStringLen(ctx, &len, argv[0]);
    if (!data) return JS_UNDEFINED;
    pty_write(&current_rt->pty, data, (int)len);
    JS_FreeCString(ctx, data);
    return JS_UNDEFINED;
}

// system.pty.resize(cols, rows) — resize PTY
static JSValue js_pty_resize(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->pty_active || argc < 2) return JS_UNDEFINED;
    int cols, rows;
    JS_ToInt32(ctx, &cols, argv[0]);
    JS_ToInt32(ctx, &rows, argv[1]);
    pty_resize(&current_rt->pty, cols, rows);
    return JS_UNDEFINED;
}

// system.pty.kill() — kill the PTY child
static JSValue js_pty_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt || !current_rt->pty_active) return JS_UNDEFINED;
    pty_destroy(&current_rt->pty);
    current_rt->pty_active = 0;
    return JS_UNDEFINED;
}

// JS Native Functions — PTY2 (second terminal for split-screen)
// ============================================================

static JSValue js_pty2_spawn(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_FALSE;
    if (current_rt->pty2_active) {
        pty_destroy(&current_rt->pty2);
        current_rt->pty2_active = 0;
    }
    const char *cmd = JS_ToCString(ctx, argv[0]);
    if (!cmd) return JS_FALSE;
    char *child_argv[32] = {0};
    int nargs = 0;
    child_argv[nargs++] = (char *)cmd;
    if (argc > 1 && JS_IsArray(ctx, argv[1])) {
        int len = 0;
        JSValue jlen = JS_GetPropertyStr(ctx, argv[1], "length");
        JS_ToInt32(ctx, &len, jlen);
        JS_FreeValue(ctx, jlen);
        for (int i = 0; i < len && nargs < 30; i++) {
            JSValue el = JS_GetPropertyUint32(ctx, argv[1], i);
            child_argv[nargs++] = (char *)JS_ToCString(ctx, el);
            JS_FreeValue(ctx, el);
        }
    }
    child_argv[nargs] = NULL;
    int cols = 80, rows = 24;
    if (argc > 2) JS_ToInt32(ctx, &cols, argv[2]);
    if (argc > 3) JS_ToInt32(ctx, &rows, argv[3]);
    int ok = pty_spawn(&current_rt->pty2, cols, rows, cmd, child_argv);
    for (int i = 1; i < nargs; i++) JS_FreeCString(ctx, child_argv[i]);
    JS_FreeCString(ctx, cmd);
    if (ok == 0) { current_rt->pty2_active = 1; return JS_TRUE; }
    return JS_FALSE;
}

static JSValue js_pty2_write(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->pty2_active || argc < 1) return JS_UNDEFINED;
    size_t len;
    const char *data = JS_ToCStringLen(ctx, &len, argv[0]);
    if (!data) return JS_UNDEFINED;
    pty_write(&current_rt->pty2, data, (int)len);
    JS_FreeCString(ctx, data);
    return JS_UNDEFINED;
}

static JSValue js_pty2_resize(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->pty2_active || argc < 2) return JS_UNDEFINED;
    int cols, rows;
    JS_ToInt32(ctx, &cols, argv[0]);
    JS_ToInt32(ctx, &rows, argv[1]);
    pty_resize(&current_rt->pty2, cols, rows);
    return JS_UNDEFINED;
}

static JSValue js_pty2_kill(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (!current_rt || !current_rt->pty2_active) return JS_UNDEFINED;
    pty_destroy(&current_rt->pty2);
    current_rt->pty2_active = 0;
    return JS_UNDEFINED;
}

// system.jump(pieceName) — request piece switch (handled in main loop)
// system.saveConfig(key, value) — merge a key-value pair into /mnt/config.json
static JSValue js_save_config(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_FALSE;
    const char *key = JS_ToCString(ctx, argv[0]);
    const char *val = JS_ToCString(ctx, argv[1]);
    if (!key || !val) {
        JS_FreeCString(ctx, key);
        JS_FreeCString(ctx, val);
        return JS_FALSE;
    }

    // Read existing config
    char buf[8192] = {0};
    FILE *f = fopen("/mnt/config.json", "r");
    if (f) {
        size_t n = fread(buf, 1, sizeof(buf) - 1, f);
        buf[n] = '\0';
        fclose(f);
    }

    // Simple JSON merge: if key exists, replace its value; otherwise append
    // We build a new JSON string manually since we don't have a JSON library
    char out[8192] = {0};
    char needle[256];
    snprintf(needle, sizeof(needle), "\"%s\"", key);

    // Detect if value is a bare literal (boolean/number) — don't quote it
    int is_bare = (strcmp(val, "true") == 0 || strcmp(val, "false") == 0 ||
                   (val[0] >= '0' && val[0] <= '9') || val[0] == '-');
    const char *qL = is_bare ? "" : "\"";
    const char *qR = is_bare ? "" : "\"";

    char *existing = strstr(buf, needle);
    if (existing && buf[0] == '{') {
        // Key exists — find value start (after ":") and end (next , or })
        char *colon = strchr(existing, ':');
        if (colon) {
            char *vstart = colon + 1;
            while (*vstart == ' ') vstart++;
            char *vend;
            if (*vstart == '"') {
                vend = strchr(vstart + 1, '"');
                if (vend) vend++; // past closing quote
            } else {
                vend = vstart;
                while (*vend && *vend != ',' && *vend != '}') vend++;
            }
            if (vend) {
                int prefix_len = (int)(vstart - buf);
                snprintf(out, sizeof(out), "%.*s%s%s%s%s",
                         prefix_len, buf, qL, val, qR, vend);
            }
        }
    } else if (buf[0] == '{') {
        // Append new key before closing }
        char *closing = strrchr(buf, '}');
        if (closing) {
            int prefix_len = (int)(closing - buf);
            // Check if there's existing content (non-empty object)
            int has_content = 0;
            for (char *p = buf + 1; p < closing; p++) {
                if (*p != ' ' && *p != '\n' && *p != '\r' && *p != '\t') {
                    has_content = 1; break;
                }
            }
            snprintf(out, sizeof(out), "%.*s%s\"%s\":%s%s%s}",
                     prefix_len, buf, has_content ? "," : "", key, qL, val, qR);
        }
    } else {
        // No valid JSON — create new
        snprintf(out, sizeof(out), "{\"%s\":%s%s%s}", key, qL, val, qR);
    }

    f = fopen("/mnt/config.json", "w");
    if (f) {
        fputs(out[0] ? out : buf, f);
        fflush(f);
        fsync(fileno(f));
        fclose(f);
        config_cache_dirty = 1;
        ac_log("[config] saved %s to /mnt/config.json\n", key);
        // Update runtime config if it's a known field
        if (current_rt && strcmp(key, "handle") == 0) {
            strncpy(current_rt->handle, val, sizeof(current_rt->handle) - 1);
        } else if (current_rt && strcmp(key, "piece") == 0) {
            strncpy(current_rt->piece, val, sizeof(current_rt->piece) - 1);
        } else if (strcmp(key, "voice") == 0) {
            extern int voice_off;
            voice_off = (strcmp(val, "off") == 0);
            ac_log("[config] voice_off = %d\n", voice_off);
        }
    } else {
        ac_log("[config] ERROR: cannot write /mnt/config.json\n");
    }

    JS_FreeCString(ctx, key);
    JS_FreeCString(ctx, val);
    return JS_TRUE;
}

// system.startSSH() — start dropbear SSH daemon (generates host key if needed)
static int ssh_started = 0;
static JSValue js_start_ssh(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (ssh_started) return JS_NewBool(ctx, 1);

    // Check if dropbear exists
    if (access("/usr/sbin/dropbear", X_OK) != 0 && access("/bin/dropbear", X_OK) != 0) {
        ac_log("[ssh] dropbear not found in initramfs\n");
        return JS_NewBool(ctx, 0);
    }

    // Generate host key if needed
    if (access("/etc/dropbear/dropbear_ed25519_host_key", F_OK) != 0) {
        ac_log("[ssh] generating host key...\n");
        system("mkdir -p /etc/dropbear && "
               "dropbearkey -t ed25519 -f /etc/dropbear/dropbear_ed25519_host_key 2>/dev/null");
    }

    // Start dropbear (no password auth, only key-based or allow all for now)
    // -R = generate keys if missing, -B = allow blank passwords (for root w/o passwd)
    // -p 22 = listen on port 22, -F = foreground (& to background)
    ac_log("[ssh] starting dropbear on port 22...\n");
    system("dropbear -R -B -p 22 -P /tmp/dropbear.pid 2>/tmp/dropbear.log &");
    ssh_started = 1;
    ac_log("[ssh] dropbear started\n");
    return JS_NewBool(ctx, 1);
}

// system.execNode(script) — run Node.js with a script string, async
static JSValue js_exec_node(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_UNDEFINED;
    if (current_rt->fetch_pending) return JS_FALSE; // reuse fetch slot for output

    const char *script = JS_ToCString(ctx, argv[0]);
    if (!script) return JS_UNDEFINED;

    // Write script to temp file
    FILE *sf = fopen("/tmp/ac_node_script.mjs", "w");
    if (sf) { fputs(script, sf); fclose(sf); }

    ac_log("[node] executing script (%ld bytes)\n", (long)strlen(script));
    // Run node and capture output to /tmp/ac_fetch.json (reusing fetch result slot)
    unlink("/tmp/ac_fetch.json");
    unlink("/tmp/ac_fetch_rc");
    unlink("/tmp/ac_fetch_err");
    system("sh -c 'node /tmp/ac_node_script.mjs > /tmp/ac_fetch.json 2>/tmp/ac_fetch_err;"
           " echo $? > /tmp/ac_fetch_rc' &");
    current_rt->fetch_pending = 1;
    current_rt->fetch_result[0] = 0;
    current_rt->fetch_error[0] = 0;

    JS_FreeCString(ctx, script);
    return JS_TRUE;
}

// system.listPieces() — scan /pieces/*.mjs and /pieces/*.lisp, return array of piece names
static JSValue js_list_pieces(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    JSValue arr = JS_NewArray(ctx);
    DIR *d = opendir("/pieces");
    if (d) {
        struct dirent *ent;
        uint32_t idx = 0;
        while ((ent = readdir(d)) != NULL) {
            if (ent->d_name[0] == '.') continue;
            // Check for .mjs
            char *dot = strstr(ent->d_name, ".mjs");
            if (dot && dot[4] == '\0') {
                char name[64];
                int len = (int)(dot - ent->d_name);
                if (len > 63) len = 63;
                memcpy(name, ent->d_name, len);
                name[len] = '\0';
                JS_SetPropertyUint32(ctx, arr, idx++, JS_NewString(ctx, name));
                continue;
            }
            // Check for .lisp — include extension so list.mjs can distinguish
            dot = strstr(ent->d_name, ".lisp");
            if (dot && dot[5] == '\0') {
                JS_SetPropertyUint32(ctx, arr, idx++, JS_NewString(ctx, ent->d_name));
            }
        }
        closedir(d);
    }
    return arr;
}

// system.listPrinters() — detect USB printers via /dev/usb/lp* and sysfs
static JSValue js_list_printers(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    JSValue arr = JS_NewArray(ctx);
    uint32_t idx = 0;

    // Scan /dev/usb/lp* devices
    DIR *d = opendir("/dev/usb");
    if (d) {
        struct dirent *ent;
        while ((ent = readdir(d)) != NULL) {
            if (strncmp(ent->d_name, "lp", 2) != 0) continue;
            char devpath[64];
            snprintf(devpath, sizeof(devpath), "/dev/usb/%s", ent->d_name);

            JSValue printer = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, printer, "device", JS_NewString(ctx, devpath));
            JS_SetPropertyStr(ctx, printer, "id", JS_NewString(ctx, ent->d_name));

            // Try to find printer name via sysfs usbmisc
            char syspath[256], name[128] = {0}, vendor[128] = {0};
            snprintf(syspath, sizeof(syspath),
                     "/sys/class/usbmisc/%s/device/../product", ent->d_name);
            FILE *fp = fopen(syspath, "r");
            if (fp) {
                if (fgets(name, sizeof(name), fp)) {
                    char *nl = strchr(name, '\n');
                    if (nl) *nl = 0;
                }
                fclose(fp);
            }
            snprintf(syspath, sizeof(syspath),
                     "/sys/class/usbmisc/%s/device/../manufacturer", ent->d_name);
            fp = fopen(syspath, "r");
            if (fp) {
                if (fgets(vendor, sizeof(vendor), fp)) {
                    char *nl = strchr(vendor, '\n');
                    if (nl) *nl = 0;
                }
                fclose(fp);
            }

            if (name[0])
                JS_SetPropertyStr(ctx, printer, "name", JS_NewString(ctx, name));
            else
                JS_SetPropertyStr(ctx, printer, "name",
                                  JS_NewString(ctx, ent->d_name));
            if (vendor[0])
                JS_SetPropertyStr(ctx, printer, "vendor",
                                  JS_NewString(ctx, vendor));

            JS_SetPropertyUint32(ctx, arr, idx++, printer);
        }
        closedir(d);
    }
    return arr;
}

// system.printRaw(devicePath, byteArray) — write raw bytes to printer device
// byteArray is a JS array of integers (0-255)
static JSValue js_print_raw(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 2) return JS_FALSE;
    const char *devpath = JS_ToCString(ctx, argv[0]);
    if (!devpath) return JS_FALSE;

    // Validate device path starts with /dev/usb/lp
    if (strncmp(devpath, "/dev/usb/lp", 11) != 0) {
        ac_log("[print] rejected non-printer path: %s\n", devpath);
        JS_FreeCString(ctx, devpath);
        return JS_FALSE;
    }

    // Get array length
    JSValue lenVal = JS_GetPropertyStr(ctx, argv[1], "length");
    uint32_t len = 0;
    JS_ToUint32(ctx, &len, lenVal);
    JS_FreeValue(ctx, lenVal);

    if (len == 0 || len > 65536) {
        ac_log("[print] invalid data length: %u\n", len);
        JS_FreeCString(ctx, devpath);
        return JS_FALSE;
    }

    // Build byte buffer from JS array
    uint8_t *buf = malloc(len);
    if (!buf) {
        JS_FreeCString(ctx, devpath);
        return JS_FALSE;
    }
    for (uint32_t i = 0; i < len; i++) {
        JSValue v = JS_GetPropertyUint32(ctx, argv[1], i);
        int32_t b = 0;
        JS_ToInt32(ctx, &b, v);
        JS_FreeValue(ctx, v);
        buf[i] = (uint8_t)(b & 0xFF);
    }

    // Open device and write
    FILE *fp = fopen(devpath, "wb");
    if (!fp) {
        ac_log("[print] failed to open %s: %s\n", devpath, strerror(errno));
        free(buf);
        JS_FreeCString(ctx, devpath);
        return JS_FALSE;
    }

    size_t written = fwrite(buf, 1, len, fp);
    fflush(fp);
    fclose(fp);
    free(buf);

    ac_log("[print] wrote %zu/%u bytes to %s\n", written, len, devpath);
    JS_FreeCString(ctx, devpath);
    return written == len ? JS_TRUE : JS_FALSE;
}

static JSValue js_jump(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_UNDEFINED;
    const char *name = JS_ToCString(ctx, argv[0]);
    if (!name) return JS_UNDEFINED;
    strncpy(current_rt->jump_target, name, sizeof(current_rt->jump_target) - 1);
    current_rt->jump_target[sizeof(current_rt->jump_target) - 1] = 0;
    current_rt->jump_requested = 1;
    ac_log("[system] jump requested: %s", name);
    JS_FreeCString(ctx, name);
    return JS_UNDEFINED;
}

// system.volumeAdjust(delta) — +1 up, -1 down, 0 mute toggle
static JSValue js_volume_adjust(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->audio || argc < 1) return JS_UNDEFINED;
    int delta = 0;
    JS_ToInt32(ctx, &delta, argv[0]);
    audio_volume_adjust(current_rt->audio, delta);
    return JS_UNDEFINED;
}

// system.brightnessAdjust(delta) — +1 up, -1 down
static JSValue js_brightness_adjust(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_UNDEFINED;
    int delta = 0;
    JS_ToInt32(ctx, &delta, argv[0]);
    // Read current backlight from sysfs
    DIR *bldir = opendir("/sys/class/backlight");
    if (!bldir) return JS_UNDEFINED;
    struct dirent *ent;
    while ((ent = readdir(bldir))) {
        if (ent->d_name[0] == '.') continue;
        char tmp[160];
        snprintf(tmp, sizeof(tmp), "/sys/class/backlight/%s/max_brightness", ent->d_name);
        FILE *f = fopen(tmp, "r");
        if (!f) continue;
        int bl_max = 100;
        fscanf(f, "%d", &bl_max);
        fclose(f);
        snprintf(tmp, sizeof(tmp), "/sys/class/backlight/%s/brightness", ent->d_name);
        f = fopen(tmp, "r");
        int cur = bl_max / 2;
        if (f) { fscanf(f, "%d", &cur); fclose(f); }
        int step = bl_max / 20; // 5%
        if (step < 1) step = 1;
        cur += delta * step;
        if (cur < 1) cur = 1;
        if (cur > bl_max) cur = bl_max;
        f = fopen(tmp, "w");
        if (f) { fprintf(f, "%d", cur); fclose(f); }
        break;
    }
    closedir(bldir);
    return JS_UNDEFINED;
}

// system.openBrowser(url) — launch Firefox for OAuth login
// Under Wayland: fork+exec firefox as sibling Wayland client (cage composites it on top)
// Under DRM: releases DRM master, runs cage+firefox, reclaims on exit
// Returns true if browser exited normally.
static JSValue js_open_browser(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt) return JS_FALSE;
    const char *url = JS_ToCString(ctx, argv[0]);
    if (!url) return JS_FALSE;

    ac_log("[browser] Opening: %s", url);
    int rc = -1;

#ifdef USE_WAYLAND
    if (getenv("WAYLAND_DISPLAY")) {
        // Under cage: just fork+exec Firefox as a sibling Wayland client
        // cage handles window stacking — Firefox appears on top, ac-native resumes when it exits
        pid_t pid = fork();
        if (pid == 0) {
            // Child process — exec firefox
            setenv("MOZ_ENABLE_WAYLAND", "1", 1);
            setenv("GDK_BACKEND", "wayland", 1);
            setenv("MOZ_DISABLE_CONTENT_SANDBOX", "1", 1);
            setenv("DBUS_SESSION_BUS_ADDRESS", "disabled:", 1);
            setenv("MOZ_DBUS_REMOTE", "0", 1);
            setenv("HOME", "/tmp", 1);
            setenv("LD_LIBRARY_PATH", "/lib64:/opt/firefox", 1);
            setenv("GRE_HOME", "/opt/firefox", 1);
            mkdir("/tmp/.mozilla", 0700);
            execlp("/opt/firefox/firefox-bin", "firefox-bin",
                   "--kiosk", "--no-remote", "--new-instance", url, NULL);
            _exit(127);
        } else if (pid > 0) {
            // Parent — wait for Firefox to exit
            int status;
            waitpid(pid, &status, 0);
            rc = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
            ac_log("[browser] Firefox exited: %d", rc);
        } else {
            ac_log("[browser] fork failed: %s", strerror(errno));
        }
    } else
#endif
    {
        // Legacy DRM handoff path
        extern int drm_release_master(void *display);
        extern int drm_acquire_master(void *display);
        extern void *g_display;
        if (g_display) {
            drm_release_master(g_display);
            ac_log("[browser] Released DRM master");
        }

        char cmd[4096];
        mkdir("/tmp/xdg", 0700);
        mkdir("/tmp/.mozilla", 0700);
        snprintf(cmd, sizeof(cmd),
            "export HOME=/tmp && "
            "export XDG_RUNTIME_DIR=/tmp/xdg && "
            "export WLR_BACKENDS=drm && "
            "export WLR_SESSION=direct && "
            "export WLR_RENDERER=pixman && "
            "export LIBSEAT_BACKEND=noop && "
            "export LD_LIBRARY_PATH=/lib64:/opt/firefox && "
            "export LIBGL_ALWAYS_SOFTWARE=1 && "
            "export MOZ_ENABLE_WAYLAND=1 && "
            "export GDK_BACKEND=wayland && "
            "cage -s -- /opt/firefox/firefox-bin "
            "--kiosk --no-remote --new-instance '%s' "
            ">/mnt/cage.log 2>&1; echo \"exit=$?\" >>/mnt/cage.log",
            url);
        rc = system(cmd);
        ac_log("[browser] cage exited: %d", rc);

        if (g_display) {
            drm_acquire_master(g_display);
            ac_log("[browser] Reclaimed DRM master");
        }
    }

    JS_FreeCString(ctx, url);
    return JS_NewBool(ctx, rc == 0);
}

// system.qrEncode(text) → { size: N, modules: [bool, bool, ...] }
// Encodes text as QR code using nayuki qrcodegen, returns module grid.
static JSValue js_qr_encode(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;

    // Buffers for qrcodegen (version 1-40, up to ~4296 chars at ECC LOW)
    uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
    uint8_t tempBuf[qrcodegen_BUFFER_LEN_MAX];

    bool ok = qrcodegen_encodeText(text, tempBuf, qrcode,
        qrcodegen_Ecc_LOW, qrcodegen_VERSION_MIN, qrcodegen_VERSION_MAX,
        qrcodegen_Mask_AUTO, true);
    JS_FreeCString(ctx, text);

    if (!ok) return JS_UNDEFINED;

    int size = qrcodegen_getSize(qrcode);
    JSValue result = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, result, "size", JS_NewInt32(ctx, size));

    JSValue modules = JS_NewArray(ctx);
    for (int y = 0; y < size; y++) {
        for (int x = 0; x < size; x++) {
            JS_SetPropertyUint32(ctx, modules, (uint32_t)(y * size + x),
                JS_NewBool(ctx, qrcodegen_getModule(qrcode, x, y)));
        }
    }
    JS_SetPropertyStr(ctx, result, "modules", modules);
    return result;
}

// nopaint.is(stateStr) — returns true if current nopaint state matches
static JSValue js_nopaint_is(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1 || !current_rt) return JS_FALSE;
    const char *query = JS_ToCString(ctx, argv[0]);
    if (!query) return JS_FALSE;
    int match = 0;
    if (strcmp(query, "painting") == 0) match = (current_rt->nopaint_state == 1);
    else if (strcmp(query, "idle") == 0) match = (current_rt->nopaint_state == 0);
    JS_FreeCString(ctx, query);
    return JS_NewBool(ctx, match);
}

// nopaint.cancelStroke() — abort current stroke
static JSValue js_nopaint_cancel(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt) { current_rt->nopaint_state = 0; current_rt->nopaint_needs_bake = 0; }
    return JS_UNDEFINED;
}

static JSValue build_system_obj(JSContext *ctx) {
    JSValue sys = JS_NewObject(ctx);

    // Battery info — scan /sys/class/power_supply/ for any battery
    char buf[64];
    int capacity = -1, power_now = 0, energy_now = 0;
    const char *status = "Unknown";
    char bat_path[128] = "";

    // Try BAT0, BAT1, or scan directory
    const char *bat_names[] = {"BAT0", "BAT1", NULL};
    for (int i = 0; bat_names[i] && !bat_path[0]; i++) {
        char tmp[128];
        snprintf(tmp, sizeof(tmp), "/sys/class/power_supply/%s/capacity", bat_names[i]);
        if (read_sysfs(tmp, buf, sizeof(buf)) > 0) {
            snprintf(bat_path, sizeof(bat_path), "/sys/class/power_supply/%s", bat_names[i]);
        }
    }
    // Fallback: scan directory
    if (!bat_path[0]) {
        DIR *dir = opendir("/sys/class/power_supply");
        if (dir) {
            struct dirent *ent;
            while ((ent = readdir(dir)) && !bat_path[0]) {
                if (ent->d_name[0] == '.') continue;
                char tmp[160];
                snprintf(tmp, sizeof(tmp), "/sys/class/power_supply/%s/type", ent->d_name);
                if (read_sysfs(tmp, buf, sizeof(buf)) > 0 && strcmp(buf, "Battery") == 0) {
                    snprintf(bat_path, sizeof(bat_path), "/sys/class/power_supply/%s", ent->d_name);
                }
            }
            closedir(dir);
        }
    }

    if (bat_path[0]) {
        char tmp[160];
        snprintf(tmp, sizeof(tmp), "%s/capacity", bat_path);
        if (read_sysfs(tmp, buf, sizeof(buf)) > 0) capacity = atoi(buf);
        snprintf(tmp, sizeof(tmp), "%s/status", bat_path);
        if (read_sysfs(tmp, buf, sizeof(buf)) > 0)
            status = (strcmp(buf, "Charging") == 0) ? "Charging" :
                     (strcmp(buf, "Full") == 0) ? "Full" :
                     (strcmp(buf, "Discharging") == 0) ? "Discharging" : "Unknown";
        snprintf(tmp, sizeof(tmp), "%s/power_now", bat_path);
        if (read_sysfs(tmp, buf, sizeof(buf)) > 0) power_now = atoi(buf);
        snprintf(tmp, sizeof(tmp), "%s/energy_now", bat_path);
        if (read_sysfs(tmp, buf, sizeof(buf)) > 0) energy_now = atoi(buf);
    }

    JSValue battery = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, battery, "percent", JS_NewInt32(ctx, capacity));
    JS_SetPropertyStr(ctx, battery, "charging", JS_NewBool(ctx, strcmp(status, "Charging") == 0));
    JS_SetPropertyStr(ctx, battery, "status", JS_NewString(ctx, status));

    if (power_now > 0 && energy_now > 0 && strcmp(status, "Discharging") == 0) {
        double hours = (double)energy_now / (double)power_now;
        JS_SetPropertyStr(ctx, battery, "minutesLeft", JS_NewInt32(ctx, (int)(hours * 60)));
    } else {
        JS_SetPropertyStr(ctx, battery, "minutesLeft", JS_NewInt32(ctx, -1));
    }

    JS_SetPropertyStr(ctx, sys, "battery", battery);

    // Hardware info — system.hw (model, cpu, ram)
    {
        JSValue hw = JS_NewObject(ctx);
        char hbuf[256];

        // Product name: /sys/class/dmi/id/product_name
        if (read_sysfs("/sys/class/dmi/id/product_name", hbuf, sizeof(hbuf)) > 0)
            JS_SetPropertyStr(ctx, hw, "model", JS_NewString(ctx, hbuf));
        else
            JS_SetPropertyStr(ctx, hw, "model", JS_NewString(ctx, "unknown"));

        // Vendor: /sys/class/dmi/id/sys_vendor
        if (read_sysfs("/sys/class/dmi/id/sys_vendor", hbuf, sizeof(hbuf)) > 0)
            JS_SetPropertyStr(ctx, hw, "vendor", JS_NewString(ctx, hbuf));
        else
            JS_SetPropertyStr(ctx, hw, "vendor", JS_NewString(ctx, "unknown"));

        // CPU model: first "model name" line from /proc/cpuinfo
        {
            FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
            char cpuline[256] = {0};
            if (cpuinfo) {
                char line[512];
                while (fgets(line, sizeof(line), cpuinfo)) {
                    if (strncmp(line, "model name", 10) == 0) {
                        char *colon = strchr(line, ':');
                        if (colon) {
                            colon++;
                            while (*colon == ' ') colon++;
                            // Trim newline
                            char *nl = strchr(colon, '\n');
                            if (nl) *nl = 0;
                            strncpy(cpuline, colon, sizeof(cpuline) - 1);
                        }
                        break;
                    }
                }
                fclose(cpuinfo);
            }
            JS_SetPropertyStr(ctx, hw, "cpu", JS_NewString(ctx, cpuline[0] ? cpuline : "unknown"));
        }

        // CPU cores: count "processor" lines in /proc/cpuinfo
        {
            FILE *cpuinfo = fopen("/proc/cpuinfo", "r");
            int cores = 0;
            if (cpuinfo) {
                char line[256];
                while (fgets(line, sizeof(line), cpuinfo)) {
                    if (strncmp(line, "processor", 9) == 0) cores++;
                }
                fclose(cpuinfo);
            }
            JS_SetPropertyStr(ctx, hw, "cores", JS_NewInt32(ctx, cores));
        }

        // RAM: total from /proc/meminfo (in MB)
        {
            FILE *meminfo = fopen("/proc/meminfo", "r");
            long total_kb = 0, avail_kb = 0;
            if (meminfo) {
                char line[256];
                while (fgets(line, sizeof(line), meminfo)) {
                    if (strncmp(line, "MemTotal:", 9) == 0) {
                        sscanf(line + 9, " %ld", &total_kb);
                    } else if (strncmp(line, "MemAvailable:", 13) == 0) {
                        sscanf(line + 13, " %ld", &avail_kb);
                    }
                    if (total_kb && avail_kb) break;
                }
                fclose(meminfo);
            }
            JS_SetPropertyStr(ctx, hw, "ramTotalMB", JS_NewInt32(ctx, (int)(total_kb / 1024)));
            JS_SetPropertyStr(ctx, hw, "ramAvailMB", JS_NewInt32(ctx, (int)(avail_kb / 1024)));
        }

        // Process count from /proc (count numeric dirs)
        {
            int procs = 0;
            DIR *pd = opendir("/proc");
            if (pd) {
                struct dirent *pe;
                while ((pe = readdir(pd))) {
                    if (pe->d_name[0] >= '1' && pe->d_name[0] <= '9') procs++;
                }
                closedir(pd);
            }
            JS_SetPropertyStr(ctx, hw, "processes", JS_NewInt32(ctx, procs));
        }

        // Load average from /proc/loadavg
        {
            char labuf[128] = {0};
            FILE *la = fopen("/proc/loadavg", "r");
            if (la) {
                if (fgets(labuf, sizeof(labuf), la)) {
                    // "0.12 0.34 0.56 1/42 1234"
                    double l1 = 0, l5 = 0, l15 = 0;
                    sscanf(labuf, "%lf %lf %lf", &l1, &l5, &l15);
                    JS_SetPropertyStr(ctx, hw, "load1", JS_NewFloat64(ctx, l1));
                    JS_SetPropertyStr(ctx, hw, "load5", JS_NewFloat64(ctx, l5));
                    JS_SetPropertyStr(ctx, hw, "load15", JS_NewFloat64(ctx, l15));
                }
                fclose(la);
            }
        }

        // CPU governor (power mode)
        if (read_sysfs("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor", hbuf, sizeof(hbuf)) > 0) {
            JS_SetPropertyStr(ctx, hw, "governor", JS_NewString(ctx, hbuf));
        }

        // Connected devices: scan /sys/class and /sys/bus for peripherals
        {
            JSValue devs = JS_NewArray(ctx);
            int di = 0;

            // USB devices: scan /sys/bus/usb/devices/*/product
            {
                DIR *ud = opendir("/sys/bus/usb/devices");
                if (ud) {
                    struct dirent *ue;
                    while ((ue = readdir(ud))) {
                        if (ue->d_name[0] == '.') continue;
                        char pp[256], prod[128] = {0}, mfr[128] = {0};
                        snprintf(pp, sizeof(pp), "/sys/bus/usb/devices/%s/product", ue->d_name);
                        int got_prod = read_sysfs(pp, prod, sizeof(prod)) > 0;
                        if (!got_prod) continue;
                        snprintf(pp, sizeof(pp), "/sys/bus/usb/devices/%s/manufacturer", ue->d_name);
                        read_sysfs(pp, mfr, sizeof(mfr));
                        JSValue d = JS_NewObject(ctx);
                        JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "usb"));
                        JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, prod));
                        if (mfr[0]) JS_SetPropertyStr(ctx, d, "vendor", JS_NewString(ctx, mfr));
                        JS_SetPropertyStr(ctx, d, "id", JS_NewString(ctx, ue->d_name));
                        JS_SetPropertyUint32(ctx, devs, di++, d);
                    }
                    closedir(ud);
                }
            }

            // Input (HID) devices: scan /sys/class/input/*/name
            {
                DIR *id = opendir("/sys/class/input");
                if (id) {
                    struct dirent *ie;
                    while ((ie = readdir(id))) {
                        if (strncmp(ie->d_name, "event", 5) != 0) continue;
                        char np[256], name[128] = {0};
                        snprintf(np, sizeof(np), "/sys/class/input/%s/device/name", ie->d_name);
                        if (read_sysfs(np, name, sizeof(name)) <= 0) continue;
                        JSValue d = JS_NewObject(ctx);
                        JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "input"));
                        JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, name));
                        JS_SetPropertyStr(ctx, d, "id", JS_NewString(ctx, ie->d_name));
                        JS_SetPropertyUint32(ctx, devs, di++, d);
                    }
                    closedir(id);
                }
            }

            // Video devices (cameras): /sys/class/video4linux/*/name
            {
                DIR *vd = opendir("/sys/class/video4linux");
                if (vd) {
                    struct dirent *ve;
                    while ((ve = readdir(vd))) {
                        if (ve->d_name[0] == '.') continue;
                        char vp[256], vname[128] = {0};
                        snprintf(vp, sizeof(vp), "/sys/class/video4linux/%s/name", ve->d_name);
                        if (read_sysfs(vp, vname, sizeof(vname)) <= 0) continue;
                        JSValue d = JS_NewObject(ctx);
                        JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "camera"));
                        JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, vname));
                        JS_SetPropertyStr(ctx, d, "id", JS_NewString(ctx, ve->d_name));
                        JS_SetPropertyUint32(ctx, devs, di++, d);
                    }
                    closedir(vd);
                }
            }

            // Sound cards: /proc/asound/cards
            {
                FILE *sc = fopen("/proc/asound/cards", "r");
                if (sc) {
                    char sline[256];
                    while (fgets(sline, sizeof(sline), sc)) {
                        // Lines like " 0 [PCH            ]: HDA-Intel - HDA Intel PCH"
                        char *dash = strstr(sline, " - ");
                        if (dash) {
                            dash += 3;
                            char *nl = strchr(dash, '\n');
                            if (nl) *nl = 0;
                            JSValue d = JS_NewObject(ctx);
                            JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "audio"));
                            JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, dash));
                            JS_SetPropertyUint32(ctx, devs, di++, d);
                        }
                    }
                    fclose(sc);
                }
            }

            // Block devices: /sys/block/*/device/model (drives, USB sticks)
            {
                DIR *bd = opendir("/sys/block");
                if (bd) {
                    struct dirent *be;
                    while ((be = readdir(bd))) {
                        if (be->d_name[0] == '.') continue;
                        if (strncmp(be->d_name, "loop", 4) == 0) continue;
                        if (strncmp(be->d_name, "ram", 3) == 0) continue;
                        char bp[256], model[128] = {0};
                        snprintf(bp, sizeof(bp), "/sys/block/%s/device/model", be->d_name);
                        read_sysfs(bp, model, sizeof(model));
                        // Get size
                        char sz[32] = {0};
                        snprintf(bp, sizeof(bp), "/sys/block/%s/size", be->d_name);
                        read_sysfs(bp, sz, sizeof(sz));
                        long sectors = sz[0] ? atol(sz) : 0;
                        int gb = (int)(sectors / 2 / 1024 / 1024); // 512-byte sectors -> GB
                        // Removable?
                        char rem[8] = {0};
                        snprintf(bp, sizeof(bp), "/sys/block/%s/removable", be->d_name);
                        read_sysfs(bp, rem, sizeof(rem));
                        JSValue d = JS_NewObject(ctx);
                        JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "disk"));
                        JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, model[0] ? model : be->d_name));
                        JS_SetPropertyStr(ctx, d, "id", JS_NewString(ctx, be->d_name));
                        JS_SetPropertyStr(ctx, d, "sizeGB", JS_NewInt32(ctx, gb));
                        JS_SetPropertyStr(ctx, d, "removable", JS_NewBool(ctx, rem[0] == '1'));
                        JS_SetPropertyUint32(ctx, devs, di++, d);
                    }
                    closedir(bd);
                }
            }

            // DRM connectors (HDMI, DP, etc.): /sys/class/drm/card*-*/status
            {
                DIR *dd = opendir("/sys/class/drm");
                if (dd) {
                    struct dirent *de;
                    while ((de = readdir(dd))) {
                        if (strncmp(de->d_name, "card", 4) != 0) continue;
                        if (!strchr(de->d_name, '-')) continue; // skip "card0" itself
                        char sp[256], st[32] = {0};
                        snprintf(sp, sizeof(sp), "/sys/class/drm/%s/status", de->d_name);
                        if (read_sysfs(sp, st, sizeof(st)) <= 0) continue;
                        JSValue d = JS_NewObject(ctx);
                        JS_SetPropertyStr(ctx, d, "type", JS_NewString(ctx, "display"));
                        JS_SetPropertyStr(ctx, d, "name", JS_NewString(ctx, de->d_name));
                        JS_SetPropertyStr(ctx, d, "connected", JS_NewBool(ctx, strcmp(st, "connected") == 0));
                        JS_SetPropertyUint32(ctx, devs, di++, d);
                    }
                    closedir(dd);
                }
            }

            JS_SetPropertyStr(ctx, hw, "devices", devs);
        }

        // Build name (baked in at compile time)
#ifdef AC_BUILD_NAME
        JS_SetPropertyStr(ctx, hw, "buildName", JS_NewString(ctx, AC_BUILD_NAME));
#endif
#ifdef AC_GIT_HASH
        JS_SetPropertyStr(ctx, hw, "gitHash", JS_NewString(ctx, AC_GIT_HASH));
#endif
#ifdef AC_BUILD_TS
        JS_SetPropertyStr(ctx, hw, "buildTs", JS_NewString(ctx, AC_BUILD_TS));
#endif

        // Display driver and GPU info
        {
            extern void *g_display;
            if (g_display) {
                const char *drv = drm_display_driver((ACDisplay *)g_display);
                JS_SetPropertyStr(ctx, hw, "displayDriver", JS_NewString(ctx, drv));
            } else {
                JS_SetPropertyStr(ctx, hw, "displayDriver", JS_NewString(ctx, "wayland"));
            }
            // Read GPU renderer from sysfs (Mesa exposes via DRI)
            char gpu_name[128] = "unknown";
            FILE *fp = popen("cat /sys/kernel/debug/dri/0/name 2>/dev/null || "
                             "cat /sys/class/drm/card0/device/label 2>/dev/null || "
                             "echo unknown", "r");
            if (fp) {
                if (fgets(gpu_name, sizeof(gpu_name), fp)) {
                    // Strip trailing newline
                    char *nl = strchr(gpu_name, '\n');
                    if (nl) *nl = '\0';
                }
                pclose(fp);
            }
            JS_SetPropertyStr(ctx, hw, "gpu", JS_NewString(ctx, gpu_name));
        }

        // Audio diagnostics
        if (current_rt->audio) {
            JS_SetPropertyStr(ctx, hw, "audioDevice",
                JS_NewString(ctx, current_rt->audio->audio_device));
            JS_SetPropertyStr(ctx, hw, "audioStatus",
                JS_NewString(ctx, current_rt->audio->audio_status));
            JS_SetPropertyStr(ctx, hw, "audioRetries",
                JS_NewInt32(ctx, current_rt->audio->audio_init_retries));
            JS_SetPropertyStr(ctx, hw, "audioRate",
                JS_NewInt32(ctx, (int)current_rt->audio->actual_rate));
        } else {
            JS_SetPropertyStr(ctx, hw, "audioStatus",
                JS_NewString(ctx, "no audio subsystem"));
        }

        JS_SetPropertyStr(ctx, sys, "hw", hw);
    }

    // Backlight brightness — scan /sys/class/backlight/
    int bl_cur = -1, bl_max = -1;
    {
        DIR *bldir = opendir("/sys/class/backlight");
        if (bldir) {
            struct dirent *ent;
            while ((ent = readdir(bldir))) {
                if (ent->d_name[0] == '.') continue;
                char tmp[160];
                snprintf(tmp, sizeof(tmp), "/sys/class/backlight/%s/max_brightness", ent->d_name);
                if (read_sysfs(tmp, buf, sizeof(buf)) > 0) {
                    bl_max = atoi(buf);
                    snprintf(tmp, sizeof(tmp), "/sys/class/backlight/%s/brightness", ent->d_name);
                    if (read_sysfs(tmp, buf, sizeof(buf)) > 0) bl_cur = atoi(buf);
                    break;
                }
            }
            closedir(bldir);
        }
    }
    if (bl_max > 0 && bl_cur >= 0) {
        JS_SetPropertyStr(ctx, sys, "brightness", JS_NewInt32(ctx, (bl_cur * 100) / bl_max));
    } else {
        JS_SetPropertyStr(ctx, sys, "brightness", JS_NewInt32(ctx, -1));
    }

    // USB-C Type-C power role — scan /sys/class/typec/port*/power_role
    {
        JSValue typec = JS_NewArray(ctx);
        int ti = 0;
        DIR *tcdir = opendir("/sys/class/typec");
        if (tcdir) {
            struct dirent *te;
            while ((te = readdir(tcdir))) {
                if (strncmp(te->d_name, "port", 4) != 0) continue;
                // Skip partner/plug entries like "port0-partner"
                if (strchr(te->d_name + 4, '-')) continue;
                char tmp[256], rbuf[64] = {0};
                snprintf(tmp, sizeof(tmp), "/sys/class/typec/%s/power_role", te->d_name);
                if (read_sysfs(tmp, rbuf, sizeof(rbuf)) <= 0) continue;
                // rbuf is like "[source] sink" or "source [sink]"
                const char *current_role = "unknown";
                int can_swap = 0;
                if (strstr(rbuf, "[source]")) current_role = "source";
                else if (strstr(rbuf, "[sink]")) current_role = "sink";
                // If both roles appear, swap is supported
                if (strstr(rbuf, "source") && strstr(rbuf, "sink")) can_swap = 1;
                // Data role
                char drbuf[64] = {0};
                snprintf(tmp, sizeof(tmp), "/sys/class/typec/%s/data_role", te->d_name);
                read_sysfs(tmp, drbuf, sizeof(drbuf));
                const char *data_role = "unknown";
                if (strstr(drbuf, "[host]")) data_role = "host";
                else if (strstr(drbuf, "[device]")) data_role = "device";

                JSValue port = JS_NewObject(ctx);
                JS_SetPropertyStr(ctx, port, "port", JS_NewString(ctx, te->d_name));
                JS_SetPropertyStr(ctx, port, "powerRole", JS_NewString(ctx, current_role));
                JS_SetPropertyStr(ctx, port, "dataRole", JS_NewString(ctx, data_role));
                JS_SetPropertyStr(ctx, port, "canSwap", JS_NewBool(ctx, can_swap));
                JS_SetPropertyUint32(ctx, typec, ti++, port);
            }
            closedir(tcdir);
        }
        JS_SetPropertyStr(ctx, sys, "typec", typec);
    }

    // system.setPowerRole(port, role) — swap USB-C power role ("source" or "sink")
    JS_SetPropertyStr(ctx, sys, "setPowerRole", JS_NewCFunction(ctx, js_set_power_role, "setPowerRole", 2));

    // Tablet mode (lid folded back on convertible laptops)
    JS_SetPropertyStr(ctx, sys, "tabletMode",
        JS_NewBool(ctx, current_rt && current_rt->input && current_rt->input->tablet_mode));

    // HDMI secondary display
    int has_hdmi = (current_rt && current_rt->hdmi && current_rt->hdmi->active);
    JS_SetPropertyStr(ctx, sys, "hasHdmi", JS_NewBool(ctx, has_hdmi));
    JS_SetPropertyStr(ctx, sys, "hdmi", JS_NewCFunction(ctx, js_hdmi_fill, "hdmi", 3));

    // WebSocket client — system.ws
    JS_SetPropertyStr(ctx, sys, "ws", build_ws_obj(ctx, current_phase));

    // Raw UDP fairy co-presence — system.udp
    JS_SetPropertyStr(ctx, sys, "udp", build_udp_obj(ctx, current_phase));

    // Machine identity — system.machineId
    {
        extern char g_machine_id[64];
        JS_SetPropertyStr(ctx, sys, "machineId",
                          JS_NewString(ctx, g_machine_id[0] ? g_machine_id : "unknown"));
    }

    // Remote reboot / poweroff — system.reboot() / system.poweroff()
    JS_SetPropertyStr(ctx, sys, "reboot", JS_NewCFunction(ctx, js_reboot, "reboot", 0));
    JS_SetPropertyStr(ctx, sys, "poweroff", JS_NewCFunction(ctx, js_poweroff, "poweroff", 0));

    // USB MIDI gadget status + control
    {
        JSValue usb_midi = build_usb_midi_state_obj(ctx);
        JS_SetPropertyStr(ctx, usb_midi, "status", JS_NewCFunction(ctx, js_usb_midi_status, "status", 0));
        JS_SetPropertyStr(ctx, usb_midi, "enable", JS_NewCFunction(ctx, js_usb_midi_enable, "enable", 0));
        JS_SetPropertyStr(ctx, usb_midi, "disable", JS_NewCFunction(ctx, js_usb_midi_disable, "disable", 0));
        JS_SetPropertyStr(ctx, usb_midi, "refresh", JS_NewCFunction(ctx, js_usb_midi_refresh, "refresh", 0));
        JS_SetPropertyStr(ctx, usb_midi, "noteOn", JS_NewCFunction(ctx, js_usb_midi_note_on, "noteOn", 3));
        JS_SetPropertyStr(ctx, usb_midi, "noteOff", JS_NewCFunction(ctx, js_usb_midi_note_off, "noteOff", 3));
        JS_SetPropertyStr(ctx, usb_midi, "allNotesOff", JS_NewCFunction(ctx, js_usb_midi_all_notes_off, "allNotesOff", 1));
        JS_SetPropertyStr(ctx, sys, "usbMidi", usb_midi);
    }

    // File I/O — system.readFile(path) / system.writeFile(path, data)
    JS_SetPropertyStr(ctx, sys, "readFile",  JS_NewCFunction(ctx, js_read_file,  "readFile",  1));
    JS_SetPropertyStr(ctx, sys, "writeFile", JS_NewCFunction(ctx, js_write_file, "writeFile", 2));
    JS_SetPropertyStr(ctx, sys, "deleteFile",JS_NewCFunction(ctx, js_delete_file,"deleteFile",1));
    JS_SetPropertyStr(ctx, sys, "listDir",   JS_NewCFunction(ctx, js_list_dir,   "listDir",   1));
    JS_SetPropertyStr(ctx, sys, "diskInfo",  JS_NewCFunction(ctx, js_disk_info,  "diskInfo",  1));
    JS_SetPropertyStr(ctx, sys, "blockDevices", JS_NewCFunction(ctx, js_block_devices, "blockDevices", 0));
    JS_SetPropertyStr(ctx, sys, "mountMusic", JS_NewCFunction(ctx, js_mount_music, "mountMusic", 0));
    JS_SetPropertyStr(ctx, sys, "mountMusicMounted", JS_NewBool(ctx, music_mount_state));
    JS_SetPropertyStr(ctx, sys, "mountMusicPending", JS_NewBool(ctx, music_mount_pending));

    // Async HTTP fetch — system.fetch(url) / system.fetchCancel() / system.fetchResult / system.fetchPending
    JS_SetPropertyStr(ctx, sys, "fetch", JS_NewCFunction(ctx, js_fetch, "fetch", 1));
    JS_SetPropertyStr(ctx, sys, "fetchPost", JS_NewCFunction(ctx, js_fetch_post, "fetchPost", 3));
    JS_SetPropertyStr(ctx, sys, "fetchCancel", JS_NewCFunction(ctx, js_fetch_cancel, "fetchCancel", 0));
    JS_SetPropertyStr(ctx, sys, "fetchPending",
                      JS_NewBool(ctx, current_rt ? current_rt->fetch_pending : 0));
    if (current_rt && current_rt->fetch_pending) {
        FILE *rc = fopen("/tmp/ac_fetch_rc", "r");
        if (rc) {
            int code = -1;
            fscanf(rc, "%d", &code);
            fclose(rc);
            unlink("/tmp/ac_fetch_rc");
            ac_log("[fetch] done: curl exit=%d\n", code);
            current_rt->fetch_result[0] = 0;
            current_rt->fetch_error[0] = 0;
            if (code == 0) {
                FILE *fp = fopen("/tmp/ac_fetch.json", "r");
                if (fp) {
                    int n = (int)fread(current_rt->fetch_result,
                                       1, sizeof(current_rt->fetch_result) - 1, fp);
                    fclose(fp);
                    current_rt->fetch_result[n] = 0;
                    unlink("/tmp/ac_fetch.json");
                } else {
                    snprintf(current_rt->fetch_error, sizeof(current_rt->fetch_error),
                             "request failed: missing response body");
                }
                unlink("/tmp/ac_fetch_err");
            } else {
                char emsg[160] = {0};
                FILE *ef = fopen("/tmp/ac_fetch_err", "r");
                if (ef) {
                    int en = (int)fread(emsg, 1, sizeof(emsg) - 1, ef);
                    fclose(ef);
                    emsg[en] = 0;
                    for (int i = 0; emsg[i]; i++) {
                        if (emsg[i] == '\n' || emsg[i] == '\r' || emsg[i] == '\t') emsg[i] = ' ';
                    }
                }
                unlink("/tmp/ac_fetch_err");
                ac_log("[fetch] error (%d): %s\n", code, emsg[0] ? emsg : "(no stderr)");
                if (emsg[0]) {
                    snprintf(current_rt->fetch_error, sizeof(current_rt->fetch_error),
                             "request failed (%d): %s", code, emsg);
                } else {
                    snprintf(current_rt->fetch_error, sizeof(current_rt->fetch_error),
                             "request failed (%d)", code);
                }
            }
            current_rt->fetch_pending = 0;
        }
    }
    // Deliver fetch result only during paint phase (where JS consumes it)
    // and clear after delivery (one-shot)
    if (current_rt && current_rt->fetch_result[0]
        && strcmp(current_phase, "paint") == 0) {
        JS_SetPropertyStr(ctx, sys, "fetchResult",
                          JS_NewString(ctx, current_rt->fetch_result));
        current_rt->fetch_result[0] = 0;
    } else {
        JS_SetPropertyStr(ctx, sys, "fetchResult", JS_NULL);
    }
    // Fetch error is also one-shot, delivered during paint.
    if (current_rt && current_rt->fetch_error[0]
        && strcmp(current_phase, "paint") == 0) {
        JS_SetPropertyStr(ctx, sys, "fetchError",
                          JS_NewString(ctx, current_rt->fetch_error));
        current_rt->fetch_error[0] = 0;
    } else {
        JS_SetPropertyStr(ctx, sys, "fetchError", JS_NULL);
    }

    // QR camera scanning — system.scanQR() / system.scanQRStop()
    JS_SetPropertyStr(ctx, sys, "scanQR", JS_NewCFunction(ctx, js_scan_qr, "scanQR", 0));
    JS_SetPropertyStr(ctx, sys, "scanQRStop", JS_NewCFunction(ctx, js_scan_qr_stop, "scanQRStop", 0));
    JS_SetPropertyStr(ctx, sys, "cameraBlit", JS_NewCFunction(ctx, js_camera_blit, "cameraBlit", 4));
    JS_SetPropertyStr(ctx, sys, "qrPending",
                      JS_NewBool(ctx, current_rt ? current_rt->qr_scan_active : 0));
    // Deliver QR result one-shot during sim phase
    if (current_rt && current_rt->camera.scan_done) {
        if (current_rt->camera.scan_result[0]) {
            JS_SetPropertyStr(ctx, sys, "qrResult",
                              JS_NewString(ctx, current_rt->camera.scan_result));
            current_rt->camera.scan_result[0] = 0;
        } else if (current_rt->camera.scan_error[0]) {
            JS_SetPropertyStr(ctx, sys, "qrError",
                              JS_NewString(ctx, current_rt->camera.scan_error));
            current_rt->camera.scan_error[0] = 0;
        } else {
            JS_SetPropertyStr(ctx, sys, "qrResult", JS_NULL);
        }
        current_rt->camera.scan_done = 0;
    } else {
        JS_SetPropertyStr(ctx, sys, "qrResult", JS_NULL);
        JS_SetPropertyStr(ctx, sys, "qrError", JS_NULL);
    }

    // OS update version string — matches OTA format: "buildname githash-buildts"
#ifdef AC_BUILD_NAME
#  ifdef AC_GIT_HASH
#    ifdef AC_BUILD_TS
    JS_SetPropertyStr(ctx, sys, "version",
                      JS_NewString(ctx, AC_BUILD_NAME " " AC_GIT_HASH "-" AC_BUILD_TS));
#    else
    JS_SetPropertyStr(ctx, sys, "version",
                      JS_NewString(ctx, AC_BUILD_NAME " " AC_GIT_HASH));
#    endif
#  else
    JS_SetPropertyStr(ctx, sys, "version", JS_NewString(ctx, AC_BUILD_NAME));
#  endif
#elif defined(AC_GIT_HASH)
#  ifdef AC_BUILD_TS
    JS_SetPropertyStr(ctx, sys, "version",
                      JS_NewString(ctx, AC_GIT_HASH "-" AC_BUILD_TS));
#  else
    JS_SetPropertyStr(ctx, sys, "version", JS_NewString(ctx, AC_GIT_HASH));
#  endif
#else
    JS_SetPropertyStr(ctx, sys, "version", JS_NewString(ctx, "unknown"));
#endif

    // Firmware capability probe — exposed as `system.firmware` so os.mjs can
    // gate the firmware-update panel on machines where flashing is actually
    // viable. Criteria, all of which must be true:
    //   1. /dev/mtd0 exists — kernel has a usable SPI-NOR driver bound to
    //      the motherboard's flash chip (CONFIG_SPI_INTEL_PCI + CONFIG_MTD).
    //      Absent this, flashrom's internal programmer can't open the chip.
    //   2. bios_vendor contains "coreboot" — the only firmware we support
    //      reflashing is MrChromebox's coreboot+edk2 build. Stock OEM AMI /
    //      Insyde firmware would either reject writes (SMM) or brick.
    //   3. Optional: a plausible Chromebook/supported-board identifier in
    //      product_name, which lets us suggest a MrChromebox ROM URL.
    // This is an advisory flag; the actual flash is gated by a second probe
    // + `flashrom --probe` step inside the update thread.
    {
        JSValue firmware = JS_NewObject(ctx);
        int mtd_ok = (access("/dev/mtd0", F_OK) == 0);
        char bios_vendor[128] = "";
        char product_name[128] = "";
        char bios_version[128] = "";
        read_sysfs("/sys/class/dmi/id/bios_vendor", bios_vendor, sizeof(bios_vendor));
        read_sysfs("/sys/class/dmi/id/product_name", product_name, sizeof(product_name));
        read_sysfs("/sys/class/dmi/id/bios_version", bios_version, sizeof(bios_version));
        // Trim trailing newlines from sysfs reads
        for (char *p = bios_vendor; *p; p++) if (*p == '\n') { *p = 0; break; }
        for (char *p = product_name; *p; p++) if (*p == '\n') { *p = 0; break; }
        for (char *p = bios_version; *p; p++) if (*p == '\n') { *p = 0; break; }
        int coreboot =
            (strstr(bios_vendor, "coreboot") != NULL) ||
            (strstr(bios_version, "MrChromebox") != NULL);
        int available = mtd_ok && coreboot;
        JS_SetPropertyStr(ctx, firmware, "available", JS_NewBool(ctx, available));
        JS_SetPropertyStr(ctx, firmware, "mtdOk", JS_NewBool(ctx, mtd_ok));
        JS_SetPropertyStr(ctx, firmware, "coreboot", JS_NewBool(ctx, coreboot));
        JS_SetPropertyStr(ctx, firmware, "biosVendor", JS_NewString(ctx, bios_vendor));
        JS_SetPropertyStr(ctx, firmware, "biosVersion", JS_NewString(ctx, bios_version));
        JS_SetPropertyStr(ctx, firmware, "productName", JS_NewString(ctx, product_name));
        // Lowercase board name (first word of product_name) is the URL slug
        // MrChromebox uses: e.g. "Google Drawman/Drawcia" -> "drawcia"
        char board[64] = "";
        {
            const char *src = product_name;
            // Skip "Google " prefix if present
            if (strncmp(src, "Google ", 7) == 0) src += 7;
            // Copy up to '/' or ' ' or end, lowercasing
            int i = 0;
            while (*src && *src != '/' && *src != ' ' && i < (int)sizeof(board) - 1) {
                board[i++] = (*src >= 'A' && *src <= 'Z') ? (*src + 32) : *src;
                src++;
            }
            board[i] = 0;
        }
        JS_SetPropertyStr(ctx, firmware, "board", JS_NewString(ctx, board));
        // Runtime install state — live values updated by fw_thread_fn.
        if (current_rt) {
            JS_SetPropertyStr(ctx, firmware, "pending",
                              JS_NewBool(ctx, current_rt->fw_pending));
            JS_SetPropertyStr(ctx, firmware, "done",
                              JS_NewBool(ctx, current_rt->fw_done));
            JS_SetPropertyStr(ctx, firmware, "ok",
                              JS_NewBool(ctx, current_rt->fw_ok));
            if (current_rt->fw_backup_path[0])
                JS_SetPropertyStr(ctx, firmware, "backupPath",
                                  JS_NewString(ctx, current_rt->fw_backup_path));
            JSValue log = JS_NewArray(ctx);
            int count = current_rt->fw_log_count;
            int show = count < 32 ? count : 32;
            int start = count - show;
            for (int i = 0; i < show; i++) {
                JS_SetPropertyUint32(ctx, log, i,
                    JS_NewString(ctx, current_rt->fw_log[(start + i) % 32]));
            }
            JS_SetPropertyStr(ctx, firmware, "log", log);
        }
        // Action: system.firmware.install(mode?) — "install" | "dry-run" | "restore"
        JS_SetPropertyStr(ctx, firmware, "install",
                          JS_NewCFunction(ctx, js_firmware_install, "install", 1));
        JS_SetPropertyStr(ctx, sys, "firmware", firmware);
    }

    // Audio diagnostic API — speaker.mjs piece uses these to probe which
    // ALSA PCM actually produces sound on this machine. listPcms scans
    // /proc/asound; testPcm plays a short sine tone on the named device
    // in a detached thread. Both are read-only — no state change to the
    // main audio thread.
    {
        JSValue audio = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, audio, "listPcms",
            JS_NewCFunction(ctx, js_audio_list_pcms, "listPcms", 0));
        JS_SetPropertyStr(ctx, audio, "testPcm",
            JS_NewCFunction(ctx, js_audio_test_pcm, "testPcm", 4));
        /* Report which device ac-native's main audio thread is currently
         * using — helpful context when the user is probing alternatives. */
        if (current_rt && current_rt->audio &&
            current_rt->audio->audio_device[0]) {
            JS_SetPropertyStr(ctx, audio, "activeDevice",
                JS_NewString(ctx, current_rt->audio->audio_device));
        }
        JS_SetPropertyStr(ctx, sys, "audio", audio);
    }

    // User config (handle, piece — read from /mnt/config.json at boot)
    {
        JSValue config = JS_NewObject(ctx);
        if (current_rt && current_rt->handle[0])
            JS_SetPropertyStr(ctx, config, "handle", JS_NewString(ctx, current_rt->handle));
        if (current_rt && current_rt->piece[0])
            JS_SetPropertyStr(ctx, config, "piece", JS_NewString(ctx, current_rt->piece));
        // Expose raw config JSON so pieces can read arbitrary fields
        {
            static char cfg_cache[8192] = {0};
            if (config_cache_dirty) {
                cfg_cache[0] = '\0';
                FILE *cf = fopen("/mnt/config.json", "r");
                if (cf) {
                    size_t n = fread(cfg_cache, 1, sizeof(cfg_cache) - 1, cf);
                    cfg_cache[n] = '\0';
                    fclose(cf);
                }
                config_cache_dirty = 0;
            }
            if (cfg_cache[0])
                JS_SetPropertyStr(ctx, config, "raw", JS_NewString(ctx, cfg_cache));
        }
        JS_SetPropertyStr(ctx, sys, "config", config);
    }

    // Boot device detection (cached — detect once, not every frame)
    {
        static char boot_dev_cache[64] = {0};
        if (!boot_dev_cache[0]) detect_boot_device(boot_dev_cache, sizeof(boot_dev_cache));
        JS_SetPropertyStr(ctx, sys, "bootDevice", JS_NewString(ctx, boot_dev_cache));
    }

    // Flash targets: enumerate all devices that could receive an EFI flash
    {
        JSValue targets = JS_NewArray(ctx);
        int idx = 0;
        // Check USB (/dev/sda1)
        if (access("/dev/sda1", F_OK) == 0) {
            char rem[8] = {0};
            read_sysfs("/sys/block/sda/removable", rem, sizeof(rem));
            int is_removable = (rem[0] == '1');
            JSValue t = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, t, "device", JS_NewString(ctx, "/dev/sda1"));
            JS_SetPropertyStr(ctx, t, "label",
                JS_NewString(ctx, is_removable ? "USB" : "Disk (sda)"));
            JS_SetPropertyStr(ctx, t, "removable", JS_NewBool(ctx, is_removable));
            JS_SetPropertyUint32(ctx, targets, idx++, t);
        }
        // Check NVMe (/dev/nvme0n1p1)
        if (access("/dev/nvme0n1p1", F_OK) == 0) {
            JSValue t = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, t, "device", JS_NewString(ctx, "/dev/nvme0n1p1"));
            JS_SetPropertyStr(ctx, t, "label", JS_NewString(ctx, "Internal (NVMe)"));
            JS_SetPropertyStr(ctx, t, "removable", JS_NewBool(ctx, 0));
            JS_SetPropertyUint32(ctx, targets, idx++, t);
        }
        // Check eMMC (/dev/mmcblk0p1) — Chromebooks + some budget laptops use
        // eMMC instead of NVMe. Parent device is mmcblk0, partitions are
        // mmcblk0p1 etc. — same "p<N>" suffix scheme as NVMe.
        if (access("/dev/mmcblk0p1", F_OK) == 0) {
            JSValue t = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, t, "device", JS_NewString(ctx, "/dev/mmcblk0p1"));
            JS_SetPropertyStr(ctx, t, "label", JS_NewString(ctx, "Internal (eMMC)"));
            JS_SetPropertyStr(ctx, t, "removable", JS_NewBool(ctx, 0));
            JS_SetPropertyUint32(ctx, targets, idx++, t);
        }
        // Check sdb1 (second USB)
        if (access("/dev/sdb1", F_OK) == 0) {
            char rem[8] = {0};
            read_sysfs("/sys/block/sdb/removable", rem, sizeof(rem));
            JSValue t = JS_NewObject(ctx);
            JS_SetPropertyStr(ctx, t, "device", JS_NewString(ctx, "/dev/sdb1"));
            JS_SetPropertyStr(ctx, t, "label",
                JS_NewString(ctx, rem[0] == '1' ? "USB (sdb)" : "Disk (sdb)"));
            JS_SetPropertyStr(ctx, t, "removable", JS_NewBool(ctx, rem[0] == '1'));
            JS_SetPropertyUint32(ctx, targets, idx++, t);
        }
        JS_SetPropertyStr(ctx, sys, "flashTargets", targets);
    }

    // Binary fetch for OS update
    JS_SetPropertyStr(ctx, sys, "fetchBinary",
                      JS_NewCFunction(ctx, js_fetch_binary, "fetchBinary", 3));
    if (current_rt) {
        // Poll progress from file size
        if (current_rt->fetch_binary_pending && current_rt->fetch_binary_expected > 0
                && current_rt->fetch_binary_dest[0]) {
            struct stat fst;
            if (stat(current_rt->fetch_binary_dest, &fst) == 0) {
                float p = (float)fst.st_size / (float)current_rt->fetch_binary_expected;
                if (p > 1.0f) p = 1.0f;
                current_rt->fetch_binary_progress = p;
            }
        }
        // Check if curl finished
        if (current_rt->fetch_binary_pending) {
            FILE *fbrc = fopen("/tmp/ac_fb_rc", "r");
            if (fbrc) {
                int rc = -1;
                fscanf(fbrc, "%d", &rc);
                fclose(fbrc);
                unlink("/tmp/ac_fb_rc");
                if (rc != 0) {
                    char emsg[160] = {0};
                    FILE *ef = fopen("/tmp/ac_fb_err", "r");
                    if (ef) {
                        int en = (int)fread(emsg, 1, sizeof(emsg) - 1, ef);
                        fclose(ef);
                        emsg[en] = 0;
                        for (int i = 0; emsg[i]; i++) {
                            if (emsg[i] == '\n' || emsg[i] == '\r' || emsg[i] == '\t') emsg[i] = ' ';
                        }
                    }
                    ac_log("[fetchBinary] error (%d): %s\n", rc, emsg[0] ? emsg : "(no stderr)");
                }
                unlink("/tmp/ac_fb_err");
                current_rt->fetch_binary_pending  = 0;
                current_rt->fetch_binary_done     = 1;
                current_rt->fetch_binary_ok       = (rc == 0) ? 1 : 0;
                current_rt->fetch_binary_progress = (rc == 0) ? 1.0f : 0.0f;
                ac_log("[fetchBinary] complete: rc=%d ok=%d dest=%s\n",
                       rc, current_rt->fetch_binary_ok, current_rt->fetch_binary_dest);
            }
        }
        JS_SetPropertyStr(ctx, sys, "fetchBinaryProgress",
                          JS_NewFloat64(ctx, (double)current_rt->fetch_binary_progress));
        JS_SetPropertyStr(ctx, sys, "fetchBinaryDone",
                          JS_NewBool(ctx, current_rt->fetch_binary_done));
        JS_SetPropertyStr(ctx, sys, "fetchBinaryOk",
                          JS_NewBool(ctx, current_rt->fetch_binary_ok));
        // Keep done latched until next fetchBinary() call resets it.
        // Act/sim/paint run in separate calls; one-shot pulses can be lost.
    }

    // Flash update
    JS_SetPropertyStr(ctx, sys, "flashUpdate",
                      JS_NewCFunction(ctx, js_flash_update, "flashUpdate", 1));
    JS_SetPropertyStr(ctx, sys, "reboot",
                      JS_NewCFunction(ctx, js_reboot, "reboot", 0));
    if (current_rt) {
        JS_SetPropertyStr(ctx, sys, "flashDone",
                          JS_NewBool(ctx, current_rt->flash_done));
        JS_SetPropertyStr(ctx, sys, "flashOk",
                          JS_NewBool(ctx, current_rt->flash_ok));
        // Phase: 0=idle 1=writing 2=syncing 3=verifying 4=done
        JS_SetPropertyStr(ctx, sys, "flashPhase",
                          JS_NewInt32(ctx, current_rt->flash_phase));
        JS_SetPropertyStr(ctx, sys, "flashVerifiedBytes",
                          JS_NewFloat64(ctx, (double)current_rt->flash_verified_bytes));
        // Flash telemetry: destination path, same-device flag, log lines
        if (current_rt->flash_dst[0])
            JS_SetPropertyStr(ctx, sys, "flashDst",
                              JS_NewString(ctx, current_rt->flash_dst));
        JS_SetPropertyStr(ctx, sys, "flashSameDevice",
                          JS_NewBool(ctx, current_rt->flash_same_device));
        // Flash log ring buffer
        {
            int count = current_rt->flash_log_count;
            int start = count > 16 ? count - 16 : 0;
            JSValue arr = JS_NewArray(ctx);
            for (int i = start; i < count; i++) {
                JS_SetPropertyUint32(ctx, arr, i - start,
                    JS_NewString(ctx, current_rt->flash_log[i % 16]));
            }
            JS_SetPropertyStr(ctx, sys, "flashLog", arr);
        }
    }

    // Config persistence
    JS_SetPropertyStr(ctx, sys, "saveConfig",
                      JS_NewCFunction(ctx, js_save_config, "saveConfig", 2));

    // SSH daemon
    JS_SetPropertyStr(ctx, sys, "startSSH",
                      JS_NewCFunction(ctx, js_start_ssh, "startSSH", 0));
    JS_SetPropertyStr(ctx, sys, "sshStarted", JS_NewBool(ctx, ssh_started));

    // Node.js execution
    JS_SetPropertyStr(ctx, sys, "execNode",
                      JS_NewCFunction(ctx, js_exec_node, "execNode", 1));

    // PTY terminal emulator
    {
        JSValue pty_obj = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, pty_obj, "spawn",
                          JS_NewCFunction(ctx, js_pty_spawn, "spawn", 4));
        JS_SetPropertyStr(ctx, pty_obj, "write",
                          JS_NewCFunction(ctx, js_pty_write, "write", 1));
        JS_SetPropertyStr(ctx, pty_obj, "resize",
                          JS_NewCFunction(ctx, js_pty_resize, "resize", 2));
        JS_SetPropertyStr(ctx, pty_obj, "kill",
                          JS_NewCFunction(ctx, js_pty_kill, "kill", 0));
        JS_SetPropertyStr(ctx, pty_obj, "active",
                          JS_NewBool(ctx, current_rt->pty_active));

        // Pump PTY output and expose grid (only during paint phase)
        if (current_rt->pty_active) {
            pty_pump(&current_rt->pty);
            pty_check_alive(&current_rt->pty);

            if (!current_rt->pty.alive) {
                // Drain remaining output (child error messages, etc.) before closing
                pty_pump(&current_rt->pty);
                JS_SetPropertyStr(ctx, pty_obj, "exitCode",
                                  JS_NewInt32(ctx, current_rt->pty.exit_code));
                pty_destroy(&current_rt->pty);
                current_rt->pty_active = 0;
            }

            JS_SetPropertyStr(ctx, pty_obj, "alive",
                              JS_NewBool(ctx, current_rt->pty.alive));
            JS_SetPropertyStr(ctx, pty_obj, "cursorX",
                              JS_NewInt32(ctx, current_rt->pty.cursor_x));
            JS_SetPropertyStr(ctx, pty_obj, "cursorY",
                              JS_NewInt32(ctx, current_rt->pty.cursor_y));
            JS_SetPropertyStr(ctx, pty_obj, "cols",
                              JS_NewInt32(ctx, current_rt->pty.cols));
            JS_SetPropertyStr(ctx, pty_obj, "rows",
                              JS_NewInt32(ctx, current_rt->pty.rows));
            JS_SetPropertyStr(ctx, pty_obj, "dirty",
                              JS_NewBool(ctx, current_rt->pty.grid_dirty));
            JS_SetPropertyStr(ctx, pty_obj, "cursorVisible",
                              JS_NewBool(ctx, current_rt->pty.cursor_visible));

            // Expose grid as flat array: [ch, fg, bg, bold, ...] per cell, row by row
            // Only send if dirty (perf optimization)
            if (current_rt->pty.grid_dirty) {
                ACPty *p = &current_rt->pty;
                JSValue grid = JS_NewArray(ctx);
                int idx = 0;
                for (int y = 0; y < p->rows; y++) {
                    for (int x = 0; x < p->cols; x++) {
                        ACPtyCell *c = &p->grid[y][x];
                        // Pack cell: ch, fg, bg, bold, fg_r, fg_g, fg_b, bg_r, bg_g, bg_b
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->ch));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bold));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_r));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_g));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_b));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_r));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_g));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_b));
                    }
                }
                JS_SetPropertyStr(ctx, pty_obj, "grid", grid);
                p->grid_dirty = 0;
            }
        } else {
            // PTY not active — expose last exit code for diagnostics
            JS_SetPropertyStr(ctx, pty_obj, "exitCode",
                              JS_NewInt32(ctx, current_rt->pty.exit_code));
        }

        JS_SetPropertyStr(ctx, sys, "pty", pty_obj);
    }

    // PTY2 — second terminal emulator for split-screen
    {
        JSValue pty2_obj = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, pty2_obj, "spawn",
                          JS_NewCFunction(ctx, js_pty2_spawn, "spawn", 4));
        JS_SetPropertyStr(ctx, pty2_obj, "write",
                          JS_NewCFunction(ctx, js_pty2_write, "write", 1));
        JS_SetPropertyStr(ctx, pty2_obj, "resize",
                          JS_NewCFunction(ctx, js_pty2_resize, "resize", 2));
        JS_SetPropertyStr(ctx, pty2_obj, "kill",
                          JS_NewCFunction(ctx, js_pty2_kill, "kill", 0));
        JS_SetPropertyStr(ctx, pty2_obj, "active",
                          JS_NewBool(ctx, current_rt->pty2_active));

        if (current_rt->pty2_active) {
            pty_pump(&current_rt->pty2);
            pty_check_alive(&current_rt->pty2);

            if (!current_rt->pty2.alive) {
                pty_pump(&current_rt->pty2);
                JS_SetPropertyStr(ctx, pty2_obj, "exitCode",
                                  JS_NewInt32(ctx, current_rt->pty2.exit_code));
                pty_destroy(&current_rt->pty2);
                current_rt->pty2_active = 0;
            }

            JS_SetPropertyStr(ctx, pty2_obj, "alive",
                              JS_NewBool(ctx, current_rt->pty2.alive));
            JS_SetPropertyStr(ctx, pty2_obj, "cursorX",
                              JS_NewInt32(ctx, current_rt->pty2.cursor_x));
            JS_SetPropertyStr(ctx, pty2_obj, "cursorY",
                              JS_NewInt32(ctx, current_rt->pty2.cursor_y));
            JS_SetPropertyStr(ctx, pty2_obj, "cols",
                              JS_NewInt32(ctx, current_rt->pty2.cols));
            JS_SetPropertyStr(ctx, pty2_obj, "rows",
                              JS_NewInt32(ctx, current_rt->pty2.rows));
            JS_SetPropertyStr(ctx, pty2_obj, "dirty",
                              JS_NewBool(ctx, current_rt->pty2.grid_dirty));
            JS_SetPropertyStr(ctx, pty2_obj, "cursorVisible",
                              JS_NewBool(ctx, current_rt->pty2.cursor_visible));

            if (current_rt->pty2.grid_dirty) {
                ACPty *p = &current_rt->pty2;
                JSValue grid = JS_NewArray(ctx);
                int idx = 0;
                for (int y = 0; y < p->rows; y++) {
                    for (int x = 0; x < p->cols; x++) {
                        ACPtyCell *c = &p->grid[y][x];
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->ch));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bold));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_r));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_g));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->fg_b));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_r));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_g));
                        JS_SetPropertyUint32(ctx, grid, idx++, JS_NewInt32(ctx, c->bg_b));
                    }
                }
                JS_SetPropertyStr(ctx, pty2_obj, "grid", grid);
                p->grid_dirty = 0;
            }
        } else {
            JS_SetPropertyStr(ctx, pty2_obj, "exitCode",
                              JS_NewInt32(ctx, current_rt->pty2.exit_code));
        }

        JS_SetPropertyStr(ctx, sys, "pty2", pty2_obj);
    }

    // Piece navigation
    JS_SetPropertyStr(ctx, sys, "jump",
                      JS_NewCFunction(ctx, js_jump, "jump", 1));

    // system.listPieces() — scan /pieces/*.mjs and return name array
    JS_SetPropertyStr(ctx, sys, "listPieces",
                      JS_NewCFunction(ctx, js_list_pieces, "listPieces", 0));

    // Printer detection and raw printing
    JS_SetPropertyStr(ctx, sys, "listPrinters",
                      JS_NewCFunction(ctx, js_list_printers, "listPrinters", 0));
    JS_SetPropertyStr(ctx, sys, "printRaw",
                      JS_NewCFunction(ctx, js_print_raw, "printRaw", 2));

    // Volume and brightness control from JS
    JS_SetPropertyStr(ctx, sys, "volumeAdjust",
                      JS_NewCFunction(ctx, js_volume_adjust, "volumeAdjust", 1));
    JS_SetPropertyStr(ctx, sys, "brightnessAdjust",
                      JS_NewCFunction(ctx, js_brightness_adjust, "brightnessAdjust", 1));
    JS_SetPropertyStr(ctx, sys, "qrEncode",
                      JS_NewCFunction(ctx, js_qr_encode, "qrEncode", 1));
    JS_SetPropertyStr(ctx, sys, "openBrowser",
                      JS_NewCFunction(ctx, js_open_browser, "openBrowser", 1));

    // Nopaint system — persistent painting canvas
    if (current_rt && strcmp(current_rt->system_mode, "nopaint") == 0) {
        // Create painting + buffer on first use
        if (!current_rt->nopaint_painting) {
            int w = current_rt->graph->screen->width;
            int h = current_rt->graph->screen->height;
            current_rt->nopaint_painting = fb_create(w, h);
            current_rt->nopaint_buffer = fb_create(w, h);
            // Fill painting with theme background
            fb_clear(current_rt->nopaint_painting, 0xFFF0EEE8); // light bg default
            fb_clear(current_rt->nopaint_buffer, 0x00000000);    // transparent
            current_rt->nopaint_active = 1;
            ac_log("[nopaint] Created painting %dx%d\n", w, h);
        }

        JSValue np = JS_NewObject(ctx);

        // nopaint.is(state) — check nopaint state
        JS_SetPropertyStr(ctx, np, "is", JS_NewCFunction(ctx, js_nopaint_is, "is", 1));
        JS_SetPropertyStr(ctx, np, "cancelStroke", JS_NewCFunction(ctx, js_nopaint_cancel, "cancelStroke", 0));

        // nopaint.brush — current brush position
        JSValue brush = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, brush, "x", JS_NewInt32(ctx, current_rt->nopaint_brush_x));
        JS_SetPropertyStr(ctx, brush, "y", JS_NewInt32(ctx, current_rt->nopaint_brush_y));
        JS_SetPropertyStr(ctx, np, "brush", brush);

        // nopaint.buffer — temporary stroke overlay (as Painting object)
        JSValue buf_obj = JS_NewObjectClass(ctx, painting_class_id);
        JS_SetOpaque(buf_obj, current_rt->nopaint_buffer);
        JS_SetPropertyStr(ctx, buf_obj, "width", JS_NewInt32(ctx, current_rt->nopaint_buffer->width));
        JS_SetPropertyStr(ctx, buf_obj, "height", JS_NewInt32(ctx, current_rt->nopaint_buffer->height));
        JS_SetPropertyStr(ctx, np, "buffer", buf_obj);

        // nopaint.needsBake
        JS_SetPropertyStr(ctx, np, "needsBake", JS_NewBool(ctx, current_rt->nopaint_needs_bake));

        JS_SetPropertyStr(ctx, sys, "nopaint", np);

        // system.painting — the persistent canvas (as Painting object)
        JSValue ptg_obj = JS_NewObjectClass(ctx, painting_class_id);
        JS_SetOpaque(ptg_obj, current_rt->nopaint_painting);
        JS_SetPropertyStr(ctx, ptg_obj, "width", JS_NewInt32(ctx, current_rt->nopaint_painting->width));
        JS_SetPropertyStr(ctx, ptg_obj, "height", JS_NewInt32(ctx, current_rt->nopaint_painting->height));
        JS_SetPropertyStr(ctx, sys, "painting", ptg_obj);
    } else if (current_rt && current_rt->nopaint_painting) {
        // Even non-nopaint pieces (like prompt) can access system.painting to show it
        JSValue ptg_obj = JS_NewObjectClass(ctx, painting_class_id);
        JS_SetOpaque(ptg_obj, current_rt->nopaint_painting);
        JS_SetPropertyStr(ctx, ptg_obj, "width", JS_NewInt32(ctx, current_rt->nopaint_painting->width));
        JS_SetPropertyStr(ctx, ptg_obj, "height", JS_NewInt32(ctx, current_rt->nopaint_painting->height));
        JS_SetPropertyStr(ctx, sys, "painting", ptg_obj);
    }

    return sys;
}

// Build the API object passed to lifecycle functions
static JSValue build_api(JSContext *ctx, ACRuntime *rt, const char *phase) {
    current_phase = phase;
    JSValue api = JS_NewObject(ctx);
    JSValue global = JS_GetGlobalObject(ctx);

    // Graphics
    JS_SetPropertyStr(ctx, api, "wipe", JS_GetPropertyStr(ctx, global, "wipe"));
    JS_SetPropertyStr(ctx, api, "ink", JS_GetPropertyStr(ctx, global, "ink"));
    JS_SetPropertyStr(ctx, api, "line", JS_GetPropertyStr(ctx, global, "line"));
    JS_SetPropertyStr(ctx, api, "box", JS_GetPropertyStr(ctx, global, "box"));
    JS_SetPropertyStr(ctx, api, "circle", JS_GetPropertyStr(ctx, global, "circle"));
    JS_SetPropertyStr(ctx, api, "plot", JS_GetPropertyStr(ctx, global, "plot"));
    JS_SetPropertyStr(ctx, api, "write", JS_GetPropertyStr(ctx, global, "write"));
    JS_SetPropertyStr(ctx, api, "scroll", JS_GetPropertyStr(ctx, global, "scroll"));
    JS_SetPropertyStr(ctx, api, "blur", JS_GetPropertyStr(ctx, global, "blur"));
    JS_SetPropertyStr(ctx, api, "zoom", JS_GetPropertyStr(ctx, global, "zoom"));
    JS_SetPropertyStr(ctx, api, "contrast", JS_GetPropertyStr(ctx, global, "contrast"));
    JS_SetPropertyStr(ctx, api, "spin", JS_GetPropertyStr(ctx, global, "spin"));
    JS_SetPropertyStr(ctx, api, "qr", JS_GetPropertyStr(ctx, global, "qr"));

    // screen
    {
        JSValue s = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, s, "width", JS_NewInt32(ctx, rt->graph->fb->width));
        JS_SetPropertyStr(ctx, s, "height", JS_NewInt32(ctx, rt->graph->fb->height));
        JS_SetPropertyStr(ctx, api, "screen", s);
    }

    // Counters
    if (strcmp(phase, "paint") == 0) {
        JS_SetPropertyStr(ctx, api, "paintCount", JS_NewInt32(ctx, rt->paint_count));
    }
    if (strcmp(phase, "sim") == 0) {
        JS_SetPropertyStr(ctx, api, "simCount", JS_NewInt32(ctx, rt->sim_count));
    }

    // Params (colon-separated args from system.jump, e.g. "chat:clock" → ["clock"])
    if (strcmp(phase, "boot") == 0 && rt->jump_param_count > 0) {
        JSValue params = JS_NewArray(ctx);
        for (int i = 0; i < rt->jump_param_count; i++) {
            JS_SetPropertyUint32(ctx, params, i, JS_NewString(ctx, rt->jump_params[i]));
        }
        JS_SetPropertyStr(ctx, api, "params", params);
    }

    // needsPaint (noop — native always paints every frame)
    JS_SetPropertyStr(ctx, api, "needsPaint", JS_NewCFunction(ctx, js_noop, "needsPaint", 0));

    // Sound
    JS_SetPropertyStr(ctx, api, "sound", build_sound_obj(ctx, rt));

    // System (battery, etc)
    JS_SetPropertyStr(ctx, api, "system", build_system_obj(ctx));

    // WiFi
    JS_SetPropertyStr(ctx, api, "wifi", build_wifi_obj(ctx, rt->wifi));

    // Trackpad delta (per-frame relative movement)
    if (rt->input) {
        JSValue trackpad = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, trackpad, "dx", JS_NewInt32(ctx, rt->input->delta_x));
        JS_SetPropertyStr(ctx, trackpad, "dy", JS_NewInt32(ctx, rt->input->delta_y));
        JS_SetPropertyStr(ctx, api, "trackpad", trackpad);

        // Analog key pressures — object mapping key names to 0.0-1.0 pressure
        if (rt->input->has_analog) {
            JSValue pressures = JS_NewObject(ctx);
            for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
                if (rt->input->analog_keys[i].active) {
                    const char *name = input_key_name(rt->input->analog_keys[i].key_code);
                    if (name) {
                        JS_SetPropertyStr(ctx, pressures, name,
                            JS_NewFloat64(ctx, (double)rt->input->analog_keys[i].pressure));
                    }
                }
            }
            JS_SetPropertyStr(ctx, api, "pressures", pressures);
        }
    }

    // params, colon, query
    JS_SetPropertyStr(ctx, api, "params", JS_NewArray(ctx));
    JS_SetPropertyStr(ctx, api, "colon", JS_NewArray(ctx));
    JS_SetPropertyStr(ctx, api, "query", JS_NewObject(ctx));

    // Form constructor
    JS_SetPropertyStr(ctx, api, "Form", JS_NewCFunction2(ctx, js_form_constructor, "Form", 2,
                      JS_CFUNC_constructor, 0));

    // CUBEL and QUAD geometry constants
    JS_SetPropertyStr(ctx, api, "CUBEL", build_cubel(ctx));
    JS_SetPropertyStr(ctx, api, "QUAD", build_quad(ctx));

    // penLock
    JS_SetPropertyStr(ctx, api, "penLock", JS_NewCFunction(ctx, js_pen_lock, "penLock", 0));

    // get (stub for texture loading — fps.mjs checks if get is available)
    JS_SetPropertyStr(ctx, api, "get", JS_NULL);

    // setShowClippedWireframes, clearWireframeBuffer, drawBufferedWireframes,
    // getRenderStats — stubs/no-ops for fps.mjs debug panel
    JS_SetPropertyStr(ctx, api, "setShowClippedWireframes", JS_NewCFunction(ctx, js_noop, "setShowClippedWireframes", 1));
    JS_SetPropertyStr(ctx, api, "clearWireframeBuffer", JS_NewCFunction(ctx, js_noop, "clearWireframeBuffer", 0));
    JS_SetPropertyStr(ctx, api, "drawBufferedWireframes", JS_NewCFunction(ctx, js_noop, "drawBufferedWireframes", 0));
    JS_SetPropertyStr(ctx, api, "getRenderStats", JS_NewCFunction(ctx, js_noop, "getRenderStats", 0));

    // system.fps — camera and render stats for debug panel
    {
        JSValue fps_obj = JS_NewObject(ctx);

        // system.fps.renderStats
        JSValue rs = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, rs, "originalTriangles", JS_NewInt32(ctx, rt->render_stats.originalTriangles));
        JS_SetPropertyStr(ctx, rs, "clippedTriangles", JS_NewInt32(ctx, rt->render_stats.clippedTriangles));
        JS_SetPropertyStr(ctx, rs, "subdividedTriangles", JS_NewInt32(ctx, rt->render_stats.subdividedTriangles));
        JS_SetPropertyStr(ctx, rs, "trianglesRejected", JS_NewInt32(ctx, rt->render_stats.trianglesRejected));
        JS_SetPropertyStr(ctx, rs, "pixelsDrawn", JS_NewInt32(ctx, rt->render_stats.pixelsDrawn));
        JS_SetPropertyStr(ctx, rs, "wireframeSegmentsTotal", JS_NewInt32(ctx, rt->render_stats.wireframeSegmentsTotal));
        JS_SetPropertyStr(ctx, rs, "wireframeSegmentsTextured", JS_NewInt32(ctx, rt->render_stats.wireframeSegmentsTextured));
        JS_SetPropertyStr(ctx, rs, "wireframeSegmentsGradient", JS_NewInt32(ctx, rt->render_stats.wireframeSegmentsGradient));
        JS_SetPropertyStr(ctx, fps_obj, "renderStats", rs);

        // system.fps.doll.cam — camera position/rotation for debug
        JSValue doll = JS_NewObject(ctx);
        JSValue cam = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, cam, "x", JS_NewFloat64(ctx, rt->camera3d.x));
        JS_SetPropertyStr(ctx, cam, "y", JS_NewFloat64(ctx, rt->camera3d.y));
        JS_SetPropertyStr(ctx, cam, "z", JS_NewFloat64(ctx, rt->camera3d.z));
        JS_SetPropertyStr(ctx, cam, "rotX", JS_NewFloat64(ctx, rt->camera3d.rotX));
        JS_SetPropertyStr(ctx, cam, "rotY", JS_NewFloat64(ctx, rt->camera3d.rotY));
        JS_SetPropertyStr(ctx, cam, "rotZ", JS_NewFloat64(ctx, rt->camera3d.rotZ));
        JS_SetPropertyStr(ctx, doll, "cam", cam);
        JS_SetPropertyStr(ctx, fps_obj, "doll", doll);

        // Attach to system object on api
        JSValue sys = JS_GetPropertyStr(ctx, api, "system");
        if (!JS_IsUndefined(sys) && !JS_IsNull(sys))
            JS_SetPropertyStr(ctx, sys, "fps", fps_obj);
        else
            JS_FreeValue(ctx, fps_obj);
        JS_FreeValue(ctx, sys);
    }

    // fps (no-op — kept for non-3D pieces)
    JS_SetPropertyStr(ctx, api, "fps", JS_NewCFunction(ctx, js_noop, "fps", 1));

    // hud
    {
        JSValue hud = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, hud, "label", JS_NewCFunction(ctx, js_noop, "label", 3));
        JS_SetPropertyStr(ctx, hud, "superscript", JS_NewCFunction(ctx, js_noop, "superscript", 1));
        JS_SetPropertyStr(ctx, api, "hud", hud);
    }

    // net
    {
        JSValue net = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, net, "udp", JS_NewCFunction(ctx, js_net_udp, "udp", 0));
        JS_SetPropertyStr(ctx, net, "preload", JS_NewCFunction(ctx, js_promise_null, "preload", 1));
        JS_SetPropertyStr(ctx, net, "pieces", JS_NewCFunction(ctx, js_promise_null, "pieces", 1));
        JS_SetPropertyStr(ctx, net, "rewrite", JS_NewCFunction(ctx, js_noop, "rewrite", 1));
        JS_SetPropertyStr(ctx, net, "log", JS_NewCFunction(ctx, js_noop, "log", 2));
        JS_SetPropertyStr(ctx, api, "net", net);
    }

    // pen — current pointer position
    if (rt->input) {
        JSValue pen = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, pen, "x", JS_NewInt32(ctx, rt->input->pointer_x));
        JS_SetPropertyStr(ctx, pen, "y", JS_NewInt32(ctx, rt->input->pointer_y));
        JS_SetPropertyStr(ctx, pen, "down", JS_NewBool(ctx, rt->input->pointer_down));
        JS_SetPropertyStr(ctx, api, "pen", pen);
    }

    // store
    {
        JSValue store = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, store, "retrieve", JS_NewCFunction(ctx, js_promise_null, "retrieve", 2));
        JS_SetPropertyStr(ctx, store, "persist", JS_NewCFunction(ctx, js_noop, "persist", 2));
        JS_SetPropertyStr(ctx, store, "delete", JS_NewCFunction(ctx, js_promise_null, "delete", 2));
        JS_SetPropertyStr(ctx, api, "store", store);
    }

    // clock
    {
        JSValue clock = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, clock, "resync", JS_NewCFunction(ctx, js_noop, "resync", 0));
        JS_SetPropertyStr(ctx, clock, "time", JS_NewCFunction(ctx, js_clock_time, "time", 0));
        JS_SetPropertyStr(ctx, api, "clock", clock);
    }

    // typeface (no-op function)
    JS_SetPropertyStr(ctx, api, "typeface", JS_NewCFunction(ctx, js_noop, "typeface", 1));

    // painting(w, h, callback) — creates a stub painting object with width/height
    JS_SetPropertyStr(ctx, api, "painting", JS_NewCFunction(ctx, js_painting, "painting", 3));

    // paste, page (real implementations), layer, sharpen (stubs)
    JS_SetPropertyStr(ctx, api, "paste", JS_NewCFunction(ctx, js_paste, "paste", 3));
    JS_SetPropertyStr(ctx, api, "page", JS_NewCFunction(ctx, js_page, "page", 1));
    JS_SetPropertyStr(ctx, api, "layer", JS_NewCFunction(ctx, js_noop, "layer", 1));
    JS_SetPropertyStr(ctx, api, "sharpen", JS_NewCFunction(ctx, js_noop, "sharpen", 1));

    // num
    {
        JSValue num = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, num, "clamp", JS_NewCFunction(ctx, js_num_clamp, "clamp", 3));
        JS_SetPropertyStr(ctx, num, "rand", JS_NewCFunction(ctx, js_num_rand, "rand", 0));
        JS_SetPropertyStr(ctx, num, "randIntRange", JS_NewCFunction(ctx, js_num_randint, "randIntRange", 2));

        // max/min — just reference Math.max/min
        JSValue math = JS_GetPropertyStr(ctx, global, "Math");
        JS_SetPropertyStr(ctx, num, "max", JS_GetPropertyStr(ctx, math, "max"));
        JS_SetPropertyStr(ctx, num, "min", JS_GetPropertyStr(ctx, math, "min"));
        JS_SetPropertyStr(ctx, num, "abs", JS_GetPropertyStr(ctx, math, "abs"));
        JS_SetPropertyStr(ctx, num, "floor", JS_GetPropertyStr(ctx, math, "floor"));
        JS_SetPropertyStr(ctx, num, "ceil", JS_GetPropertyStr(ctx, math, "ceil"));
        JS_SetPropertyStr(ctx, num, "round", JS_GetPropertyStr(ctx, math, "round"));
        JS_SetPropertyStr(ctx, num, "sign", JS_GetPropertyStr(ctx, math, "sign"));
        JS_FreeValue(ctx, math);
        JS_SetPropertyStr(ctx, num, "shiftRGB", JS_GetPropertyStr(ctx, global, "__shiftRGB"));
        JS_SetPropertyStr(ctx, num, "dist", JS_GetPropertyStr(ctx, global, "__dist"));
        JS_SetPropertyStr(ctx, num, "map", JS_GetPropertyStr(ctx, global, "__map"));
        JS_SetPropertyStr(ctx, num, "lerp", JS_GetPropertyStr(ctx, global, "__lerp"));
        JS_SetPropertyStr(ctx, num, "parseColor", JS_GetPropertyStr(ctx, global, "__parseColor"));
        JS_SetPropertyStr(ctx, num, "randIntArr", JS_GetPropertyStr(ctx, global, "__randIntArr"));
        JS_SetPropertyStr(ctx, num, "timestamp", JS_GetPropertyStr(ctx, global, "__timestamp"));
        JS_SetPropertyStr(ctx, api, "num", num);
    }

    // help
    {
        JSValue help = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, help, "resampleArray", JS_NewCFunction(ctx, js_noop, "resampleArray", 2));
        JS_SetPropertyStr(ctx, api, "help", help);
    }

    // pens
    JS_SetPropertyStr(ctx, api, "pens", JS_NewCFunction(ctx, js_noop, "pens", 0));

    // ui
    {
        JSValue ui = JS_NewObject(ctx);
        JSValue btn_ctor = JS_GetPropertyStr(ctx, global, "__Button");
        JS_SetPropertyStr(ctx, ui, "Button", btn_ctor);
        JS_SetPropertyStr(ctx, api, "ui", ui);
    }

    // api sub-object (meta-API)
    // notepat passes this nested `api` to helpers like buildWaveButton(api)
    // which destructure { screen, ui, typeface, geo, sound, num, ... } from it
    {
        JSValue api_meta = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, api_meta, "Typeface", JS_GetPropertyStr(ctx, global, "__Typeface"));
        JS_SetPropertyStr(ctx, api_meta, "beep", JS_NewCFunction(ctx, js_noop, "beep", 1));
        JS_SetPropertyStr(ctx, api_meta, "send", JS_NewCFunction(ctx, js_noop, "send", 1));

        // api.text.box
        JSValue api_text = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, api_text, "box", JS_NewCFunction(ctx, js_noop, "box", 5));
        JS_SetPropertyStr(ctx, api_meta, "text", api_text);

        // api.geo.Box
        JSValue api_geo = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, api_geo, "Box", JS_GetPropertyStr(ctx, global, "__Box"));
        JS_SetPropertyStr(ctx, api_meta, "geo", api_geo);

        // Copy top-level API properties onto api_meta so helper functions
        // like buildWaveButton(api) can destructure { screen, ui, typeface, ... }
        JS_SetPropertyStr(ctx, api_meta, "screen", JS_GetPropertyStr(ctx, api, "screen"));
        JS_SetPropertyStr(ctx, api_meta, "ui", JS_GetPropertyStr(ctx, api, "ui"));
        JS_SetPropertyStr(ctx, api_meta, "typeface", JS_GetPropertyStr(ctx, api, "typeface"));
        JS_SetPropertyStr(ctx, api_meta, "sound", JS_GetPropertyStr(ctx, api, "sound"));
        JS_SetPropertyStr(ctx, api_meta, "num", JS_GetPropertyStr(ctx, api, "num"));
        JS_SetPropertyStr(ctx, api_meta, "hud", JS_GetPropertyStr(ctx, api, "hud"));
        JS_SetPropertyStr(ctx, api_meta, "net", JS_GetPropertyStr(ctx, api, "net"));
        JS_SetPropertyStr(ctx, api_meta, "store", JS_GetPropertyStr(ctx, api, "store"));
        JS_SetPropertyStr(ctx, api_meta, "help", JS_GetPropertyStr(ctx, api, "help"));
        JS_SetPropertyStr(ctx, api_meta, "painting", JS_GetPropertyStr(ctx, api, "painting"));
        JS_SetPropertyStr(ctx, api_meta, "paste", JS_GetPropertyStr(ctx, api, "paste"));
        JS_SetPropertyStr(ctx, api_meta, "page", JS_GetPropertyStr(ctx, api, "page"));
        JS_SetPropertyStr(ctx, api_meta, "layer", JS_GetPropertyStr(ctx, api, "layer"));
        JS_SetPropertyStr(ctx, api_meta, "pens", JS_GetPropertyStr(ctx, api, "pens"));
        JS_SetPropertyStr(ctx, api_meta, "params", JS_GetPropertyStr(ctx, api, "params"));
        JS_SetPropertyStr(ctx, api_meta, "colon", JS_GetPropertyStr(ctx, api, "colon"));
        JS_SetPropertyStr(ctx, api_meta, "wipe", JS_GetPropertyStr(ctx, api, "wipe"));
        JS_SetPropertyStr(ctx, api_meta, "ink", JS_GetPropertyStr(ctx, api, "ink"));
        JS_SetPropertyStr(ctx, api_meta, "box", JS_GetPropertyStr(ctx, api, "box"));
        JS_SetPropertyStr(ctx, api_meta, "line", JS_GetPropertyStr(ctx, api, "line"));
        JS_SetPropertyStr(ctx, api_meta, "circle", JS_GetPropertyStr(ctx, api, "circle"));
        JS_SetPropertyStr(ctx, api_meta, "plot", JS_GetPropertyStr(ctx, api, "plot"));
        JS_SetPropertyStr(ctx, api_meta, "write", JS_GetPropertyStr(ctx, api, "write"));
        JS_SetPropertyStr(ctx, api_meta, "scroll", JS_GetPropertyStr(ctx, api, "scroll"));
        JS_SetPropertyStr(ctx, api_meta, "blur", JS_GetPropertyStr(ctx, api, "blur"));
        JS_SetPropertyStr(ctx, api_meta, "zoom", JS_GetPropertyStr(ctx, api, "zoom"));
        JS_SetPropertyStr(ctx, api_meta, "contrast", JS_GetPropertyStr(ctx, api, "contrast"));
        JS_SetPropertyStr(ctx, api_meta, "spin", JS_GetPropertyStr(ctx, api, "spin"));

        JS_SetPropertyStr(ctx, api, "api", api_meta);
    }

    JS_FreeValue(ctx, global);
    return api;
}

static int piece_load_counter = 0;

int js_load_piece(ACRuntime *rt, const char *path) {
    JSContext *ctx = rt->ctx;
    current_rt = rt;

    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "[js] Cannot open %s\n", path);
        return -1;
    }
    fseek(f, 0, SEEK_END);
    long len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *src = malloc(len + 1);
    fread(src, 1, len, f);
    src[len] = '\0';
    fclose(f);

    // Append globalThis export assignments so we can find lifecycle functions
    // after module evaluation (QuickJS modules don't expose exports publicly).
    // For pieces with `export const system = "fps"` (arena.mjs etc.) this
    // ALSO instantiates CamDoll from the preloaded FPS bundle and wraps
    // boot/sim/act/paint so `api.system.fps.doll` is live on every call
    // without the piece needing to know anything about the native runtime.
    const char *export_shim =
        "\n;if(typeof boot==='function')globalThis.boot=boot;"
        "if(typeof paint==='function')globalThis.paint=paint;"
        "if(typeof act==='function')globalThis.act=act;"
        "if(typeof sim==='function')globalThis.sim=sim;"
        "if(typeof leave==='function')globalThis.leave=leave;"
        "if(typeof beat==='function')globalThis.beat=beat;"
        "if(typeof configureAutopat==='function')globalThis.configureAutopat=configureAutopat;"
        "if(typeof system!=='undefined')globalThis.__pieceSystem=system;"
        "if(typeof fpsOpts!=='undefined')globalThis.__pieceFpsOpts=fpsOpts;"
        // FPS system wiring — runs only when piece opts in.
        "if(globalThis.__pieceSystem==='fps'&&globalThis.__FpsSystem){"
          "try{"
          "const FS=globalThis.__FpsSystem;"
          "const opts=globalThis.__pieceFpsOpts||{fov:80};"
          "const doll=new FS.CamDoll(FS.Camera,FS.Dolly,opts);"
          "globalThis.__fpsDoll=doll;"
          "const _b=globalThis.boot,_s=globalThis.sim,_a=globalThis.act,_p=globalThis.paint;"
          "const inject=(api)=>{if(api){if(!api.system)api.system={};api.system.fps={doll};}};"
          "globalThis.boot=(api)=>{inject(api);return _b?.(api);};"
          "globalThis.sim=(api)=>{inject(api);try{doll.sim?.();}catch(e){}return _s?.(api);};"
          "globalThis.act=(api)=>{inject(api);try{if(api?.event)doll.act?.(api.event);}catch(e){}return _a?.(api);};"
          "globalThis.paint=(api)=>{inject(api);return _p?.(api);};"
          "}catch(e){console.error('[fps] wire-up failed:',e.message);}"
        "}\n";
    size_t shim_len = strlen(export_shim);
    char *patched = malloc(len + shim_len + 1);
    memcpy(patched, src, len);
    memcpy(patched + len, export_shim, shim_len);
    patched[len + shim_len] = '\0';
    free(src);
    src = patched;
    len += shim_len;

    // Use unique module name each load (QuickJS caches modules by name)
    char mod_name[300];
    snprintf(mod_name, sizeof(mod_name), "%s#%d", path, piece_load_counter++);

    // Evaluate as module
    JSValue val = JS_Eval(ctx, src, len, mod_name, JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
    free(src);

    if (JS_IsException(val)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Parse error: %s\n", str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
        return -1;
    }

    JSValue result = JS_EvalFunction(ctx, val);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Module error: %s\n", str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
        return -1;
    }
    JS_FreeValue(ctx, result);

    // Get lifecycle functions from globalThis
    JSValue global = JS_GetGlobalObject(ctx);
    rt->boot_fn = JS_GetPropertyStr(ctx, global, "boot");
    rt->paint_fn = JS_GetPropertyStr(ctx, global, "paint");
    rt->act_fn = JS_GetPropertyStr(ctx, global, "act");
    rt->sim_fn = JS_GetPropertyStr(ctx, global, "sim");
    rt->leave_fn = JS_GetPropertyStr(ctx, global, "leave");
    rt->beat_fn = JS_GetPropertyStr(ctx, global, "beat");

    // Capture piece's `export const system` value (e.g. "prompt", "fps", "nopaint")
    rt->system_mode[0] = '\0';
    JSValue sys_val = JS_GetPropertyStr(ctx, global, "__pieceSystem");
    if (JS_IsString(sys_val)) {
        const char *s = JS_ToCString(ctx, sys_val);
        if (s) {
            strncpy(rt->system_mode, s, sizeof(rt->system_mode) - 1);
            rt->system_mode[sizeof(rt->system_mode) - 1] = '\0';
            JS_FreeCString(ctx, s);
        }
    }
    JS_FreeValue(ctx, sys_val);

    // Auto-detect FPS mode
    if (strcmp(rt->system_mode, "fps") == 0)
        rt->fps_system_active = 1;
    else
        rt->fps_system_active = 0;

    JS_FreeValue(ctx, global);

    fprintf(stderr, "[js] Loaded piece: %s (system=%s)\n", path,
            rt->system_mode[0] ? rt->system_mode : "(none)");
    fprintf(stderr, "[js] boot=%s paint=%s act=%s sim=%s beat=%s\n",
            JS_IsFunction(ctx, rt->boot_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->paint_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->act_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->sim_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->beat_fn) ? "yes" : "no");

    return 0;
}

void js_call_boot(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->boot_fn)) return;
    current_rt = rt;
    JSValue api = build_api(rt->ctx, rt, "boot");
    JSValue result = JS_Call(rt->ctx, rt->boot_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        JSValue stack = JS_GetPropertyStr(rt->ctx, exc, "stack");
        const char *stack_str = JS_ToCString(rt->ctx, stack);
        fprintf(stderr, "[js] boot() error: %s\n%s\n", str, stack_str ? stack_str : "");
        if (stack_str) JS_FreeCString(rt->ctx, stack_str);
        JS_FreeValue(rt->ctx, stack);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }

    // If boot() returned a Promise (async function), we need to catch rejections
    if (JS_IsObject(result)) {
        // Check if it has a .catch method (Promise)
        JSValue catch_fn = JS_GetPropertyStr(rt->ctx, result, "catch");
        if (JS_IsFunction(rt->ctx, catch_fn)) {
            // Register an error handler: promise.catch(e => console.error("boot async error:", e))
            const char *catch_code =
                "(function(p) { p.catch(function(e) { console.error('[js] boot() async error:', e, e.stack || ''); }); })";
            JSValue catch_handler = JS_Eval(rt->ctx, catch_code, strlen(catch_code), "<catch>", JS_EVAL_TYPE_GLOBAL);
            if (JS_IsFunction(rt->ctx, catch_handler)) {
                JSValue catch_result = JS_Call(rt->ctx, catch_handler, JS_UNDEFINED, 1, &result);
                JS_FreeValue(rt->ctx, catch_result);
            }
            JS_FreeValue(rt->ctx, catch_handler);
        }
        JS_FreeValue(rt->ctx, catch_fn);
    }

    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);

    // Execute pending jobs (promises) — boot() is async, drain fully
    // Each await creates a new microtask; need multiple drain rounds
    JSContext *pctx;
    for (int round = 0; round < 100; round++) {
        int jobs = JS_ExecutePendingJob(rt->rt, &pctx);
        if (jobs <= 0) break;
    }
}

// Helper: record a JS crash for the overlay
static void js_record_crash(ACRuntime *rt, const char *fn_name, const char *msg) {
    rt->crash_active = 1;
    rt->crash_count++;
    rt->crash_frame = 0;
    snprintf(rt->crash_msg, sizeof(rt->crash_msg), "%s(): %s", fn_name, msg ? msg : "unknown");
}

void js_call_paint(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->paint_fn)) return;
    current_rt = rt;
    rt->paint_count++;

    // Nopaint: paste the persistent painting as background before piece paints
    if (rt->nopaint_active && rt->nopaint_painting) {
        graph_paste(rt->graph, rt->nopaint_painting, 0, 0);
    }

    JSValue api = build_api(rt->ctx, rt, "paint");
    JSValue result = JS_Call(rt->ctx, rt->paint_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        JSValue stack = JS_GetPropertyStr(rt->ctx, exc, "stack");
        const char *stack_str = JS_ToCString(rt->ctx, stack);
        fprintf(stderr, "[js] paint() error: %s\n%s\n", str, stack_str ? stack_str : "");
        js_record_crash(rt, "paint", str);
        if (stack_str) JS_FreeCString(rt->ctx, stack_str);
        JS_FreeValue(rt->ctx, stack);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);

    // Nopaint: if bake was requested, composite buffer → painting and clear buffer
    if (rt->nopaint_active && rt->nopaint_needs_bake) {
        if (rt->nopaint_buffer && rt->nopaint_painting) {
            graph_paste(rt->graph, rt->nopaint_buffer, 0, 0); // show final stroke on screen
            // Bake: composite buffer onto persistent painting
            ACFramebuffer *saved = rt->graph->fb;
            graph_page(rt->graph, rt->nopaint_painting);
            graph_paste(rt->graph, rt->nopaint_buffer, 0, 0);
            graph_page(rt->graph, saved);
            // Clear buffer
            fb_clear(rt->nopaint_buffer, 0x00000000);
        }
        rt->nopaint_needs_bake = 0;
    }
}

void js_call_act(ACRuntime *rt) {
    current_rt = rt;

    // Track key held state for FPS camera (process even without act_fn)
    ACInput *input = rt->input;
    for (int i = 0; i < input->event_count; i++) {
        ACEvent *ev = &input->events[i];
        if (ev->type == AC_EVENT_KEYBOARD_DOWN && ev->key_code > 0 && ev->key_code < KEY_MAX)
            rt->keys_held[ev->key_code] = 1;
        else if (ev->type == AC_EVENT_KEYBOARD_UP && ev->key_code > 0 && ev->key_code < KEY_MAX)
            rt->keys_held[ev->key_code] = 0;
    }

    // System-level Escape → jump to prompt (mirrors web bios behavior).
    // Skip if current piece IS prompt, or if piece uses system="world" or "prompt".
    if (strcmp(rt->system_mode, "prompt") != 0 &&
        strcmp(rt->system_mode, "world") != 0) {
        for (int i = 0; i < input->event_count; i++) {
            ACEvent *ev = &input->events[i];
            if (ev->type == AC_EVENT_KEYBOARD_DOWN && ev->key_code == KEY_ESC) {
                // Reset FPS camera state if active
                if (rt->pen_locked) {
                    rt->pen_locked = 0;
                    rt->fps_system_active = 0;
                }
                strncpy(rt->jump_target, "prompt", sizeof(rt->jump_target) - 1);
                rt->jump_target[sizeof(rt->jump_target) - 1] = '\0';
                rt->jump_requested = 1;
                rt->jump_param_count = 0;
                ac_log("[system] Escape → prompt\n");
                return; // Skip passing events to piece
            }
        }
    }

    // Nopaint: update brush position and state from touch/mouse events
    if (rt->nopaint_active && strcmp(rt->system_mode, "nopaint") == 0) {
        for (int i = 0; i < input->event_count; i++) {
            ACEvent *ev = &input->events[i];
            if (ev->type == AC_EVENT_TOUCH) {
                rt->nopaint_state = 1; // painting
                rt->nopaint_brush_x = ev->x;
                rt->nopaint_brush_y = ev->y;
                rt->nopaint_needs_bake = 0;
            } else if (ev->type == AC_EVENT_DRAW) {
                if (rt->nopaint_state == 1) {
                    rt->nopaint_brush_x = ev->x;
                    rt->nopaint_brush_y = ev->y;
                }
            } else if (ev->type == AC_EVENT_LIFT) {
                if (rt->nopaint_state == 1) {
                    rt->nopaint_state = 0; // idle
                    rt->nopaint_needs_bake = 1;
                }
            }
        }
    }

    if (!JS_IsFunction(rt->ctx, rt->act_fn)) return;

    for (int i = 0; i < input->event_count; i++) {
        JSValue api = build_api(rt->ctx, rt, "act");
        JSValue event = make_event_object(rt->ctx, &input->events[i]);
        JS_SetPropertyStr(rt->ctx, api, "event", event);

        JSValue result = JS_Call(rt->ctx, rt->act_fn, JS_UNDEFINED, 1, &api);
        if (JS_IsException(result)) {
            JSValue exc = JS_GetException(rt->ctx);
            const char *str = JS_ToCString(rt->ctx, exc);
            JSValue stack = JS_GetPropertyStr(rt->ctx, exc, "stack");
            const char *stack_str = JS_ToCString(rt->ctx, stack);
            fprintf(stderr, "[js] act() error: %s\n%s\n", str, stack_str ? stack_str : "");
            ac_log("[js] act() error: %s\n%s\n", str, stack_str ? stack_str : "");
            js_record_crash(rt, "act", str);
            if (stack_str) JS_FreeCString(rt->ctx, stack_str);
            JS_FreeValue(rt->ctx, stack);
            JS_FreeCString(rt->ctx, str);
            JS_FreeValue(rt->ctx, exc);
        }
        JS_FreeValue(rt->ctx, result);
        JS_FreeValue(rt->ctx, api);
    }
}

void js_call_sim(ACRuntime *rt) {
    current_rt = rt;

    // If the piece's FPS bundle installed a CamDoll (via the export
    // shim in js_load_piece), the doll's own sim() — invoked from the
    // wrapped piece sim() — is the camera authority. We just sync its
    // cam state back to rt->camera3d AFTER the piece sim runs (below).
    // Otherwise, fall back to the native WASD + trackpad driver.
    int have_doll = 0;
    {
        JSValue global = JS_GetGlobalObject(rt->ctx);
        JSValue doll = JS_GetPropertyStr(rt->ctx, global, "__fpsDoll");
        have_doll = !JS_IsUndefined(doll) && !JS_IsNull(doll);
        JS_FreeValue(rt->ctx, doll);
        JS_FreeValue(rt->ctx, global);
    }
    if (!have_doll && rt->pen_locked && rt->fps_system_active) {
        camera3d_update(&rt->camera3d,
            rt->keys_held[KEY_W],
            rt->keys_held[KEY_S],
            rt->keys_held[KEY_A],
            rt->keys_held[KEY_D],
            rt->keys_held[KEY_SPACE],
            rt->keys_held[KEY_LEFTSHIFT],
            (float)(rt->input ? rt->input->delta_x : 0),
            (float)(rt->input ? rt->input->delta_y : 0));
        // Consume trackpad delta
        if (rt->input) { rt->input->delta_x = 0; rt->input->delta_y = 0; }
    }

    if (!JS_IsFunction(rt->ctx, rt->sim_fn)) return;
    rt->sim_count++;
    JSValue api = build_api(rt->ctx, rt, "sim");
    JSValue result = JS_Call(rt->ctx, rt->sim_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        JSValue stack = JS_GetPropertyStr(rt->ctx, exc, "stack");
        const char *stack_str = JS_ToCString(rt->ctx, stack);
        fprintf(stderr, "[js] sim() error: %s\n%s\n", str, stack_str ? stack_str : "");
        js_record_crash(rt, "sim", str);
        if (stack_str) JS_FreeCString(rt->ctx, stack_str);
        JS_FreeValue(rt->ctx, stack);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);

    // FPS camera sync — after the wrapped piece sim() runs (which
    // invoked doll.sim() via the shim), copy doll.cam.{x,y,z,rotX/Y/Z}
    // back into rt->camera3d so the next frame's graph3d rendering
    // uses the doll's camera. Only syncs when the doll is present.
    if (have_doll) {
        JSValue global = JS_GetGlobalObject(rt->ctx);
        JSValue doll = JS_GetPropertyStr(rt->ctx, global, "__fpsDoll");
        if (!JS_IsUndefined(doll) && !JS_IsNull(doll)) {
            JSValue cam = JS_GetPropertyStr(rt->ctx, doll, "cam");
            if (!JS_IsUndefined(cam) && !JS_IsNull(cam)) {
                double x, y, z, rx, ry, rz;
                JSValue v;
                v = JS_GetPropertyStr(rt->ctx, cam, "x");
                if (JS_ToFloat64(rt->ctx, &x, v) == 0) rt->camera3d.x = (float)x;
                JS_FreeValue(rt->ctx, v);
                v = JS_GetPropertyStr(rt->ctx, cam, "y");
                if (JS_ToFloat64(rt->ctx, &y, v) == 0) rt->camera3d.y = (float)y;
                JS_FreeValue(rt->ctx, v);
                v = JS_GetPropertyStr(rt->ctx, cam, "z");
                if (JS_ToFloat64(rt->ctx, &z, v) == 0) rt->camera3d.z = (float)z;
                JS_FreeValue(rt->ctx, v);
                v = JS_GetPropertyStr(rt->ctx, cam, "rotX");
                if (JS_ToFloat64(rt->ctx, &rx, v) == 0) rt->camera3d.rotX = (float)rx;
                JS_FreeValue(rt->ctx, v);
                v = JS_GetPropertyStr(rt->ctx, cam, "rotY");
                if (JS_ToFloat64(rt->ctx, &ry, v) == 0) rt->camera3d.rotY = (float)ry;
                JS_FreeValue(rt->ctx, v);
                v = JS_GetPropertyStr(rt->ctx, cam, "rotZ");
                if (JS_ToFloat64(rt->ctx, &rz, v) == 0) rt->camera3d.rotZ = (float)rz;
                JS_FreeValue(rt->ctx, v);
            }
            JS_FreeValue(rt->ctx, cam);
        }
        JS_FreeValue(rt->ctx, doll);
        JS_FreeValue(rt->ctx, global);
    }

    // Execute pending jobs (promises)
    JSContext *pctx;
    while (JS_ExecutePendingJob(rt->rt, &pctx) > 0) {}
}

void js_call_beat(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->beat_fn)) return;
    current_rt = rt;
    JSValue api = build_api(rt->ctx, rt, "beat");
    JSValue result = JS_Call(rt->ctx, rt->beat_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        fprintf(stderr, "[js] beat() error: %s\n", str);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);
}

void js_call_leave(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->leave_fn)) return;
    current_rt = rt;
    JSValue result = JS_Call(rt->ctx, rt->leave_fn, JS_UNDEFINED, 0, NULL);
    JS_FreeValue(rt->ctx, result);
}

void js_destroy(ACRuntime *rt) {
    if (!rt) return;
    JS_FreeValue(rt->ctx, rt->boot_fn);
    JS_FreeValue(rt->ctx, rt->paint_fn);
    JS_FreeValue(rt->ctx, rt->act_fn);
    JS_FreeValue(rt->ctx, rt->sim_fn);
    JS_FreeValue(rt->ctx, rt->leave_fn);
    JS_FreeValue(rt->ctx, rt->beat_fn);
    ws_destroy(rt->ws);
    udp_destroy(rt->udp);
    depth_destroy(rt->depth_buf);
    JS_FreeContext(rt->ctx);
    JS_FreeRuntime(rt->rt);
    free(rt);
}
