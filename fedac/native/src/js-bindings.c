#define _GNU_SOURCE
#include "js-bindings.h"
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
    if (fb) fb_destroy(fb);
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
    graph_line(current_rt->graph, x0, y0, x1, y1);
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
    // composite → treat as sine for now
    return WAVE_SINE;
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

    // Parse type
    WaveType wt = WAVE_SINE;
    JSValue type_v = JS_GetPropertyStr(ctx, opts, "type");
    if (JS_IsString(type_v)) {
        const char *ts = JS_ToCString(ctx, type_v);
        wt = parse_wave_type(ts);
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

    // Create voice
    uint64_t id = audio_synth(audio, wt, freq, duration, volume, attack, decay, pan);
    fprintf(stderr, "[synth] type=%d freq=%.1f vol=%.2f dur=%.1f id=%lu\n",
            wt, freq, volume, duration, (unsigned long)id);
    ac_log("[synth] type=%d freq=%.1f vol=%.2f dur=%.1f id=%lu\n",
           wt, freq, volume, duration, (unsigned long)id);

    // Return sound object with kill(), update(), startedAt
    JSValue snd = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, snd, "id", JS_NewFloat64(ctx, (double)id));
    JS_SetPropertyStr(ctx, snd, "startedAt", JS_NewFloat64(ctx, audio->time));

    // kill(fade) and update({volume,tone,pan}) methods
    JS_SetPropertyStr(ctx, snd, "kill", JS_NewCFunction(ctx, js_synth_obj_kill, "kill", 1));
    JS_SetPropertyStr(ctx, snd, "update", JS_NewCFunction(ctx, js_synth_obj_update, "update", 1));

    return snd;
}

// sound.kill(idOrObj, fade) — accepts raw id or synth object
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
    if (JS_IsObject(argv[0])) {
        JSValue is_v = JS_GetPropertyStr(ctx, argv[0], "isSample");
        is_sample = JS_ToBool(ctx, is_v);
        JS_FreeValue(ctx, is_v);
    }

    if (is_sample) {
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
    if (id == 0) return JS_UNDEFINED;

    // Return object with id, update(), isSample
    JSValue snd = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, snd, "id", JS_NewFloat64(ctx, (double)id));
    JS_SetPropertyStr(ctx, snd, "isSample", JS_TRUE);
    JS_SetPropertyStr(ctx, snd, "update", JS_NewCFunction(ctx, js_sample_obj_update, "update", 1));
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
    // Typeface constructor stub
    "globalThis.__Typeface = function Typeface(name) {\n"
    "  this.name = name;\n"
    "  this.loaded = false;\n"
    "  this.load = function(preloadFn) { this.loaded = true; return this; };\n"
    "  this.measure = function(text) { return { width: (text||'').length * 8, height: 8 }; };\n"
    "};\n"
    // ── Global theme system ──
    // Auto-switches dark/light based on LA time. All pieces use theme.fg, theme.bg, etc.
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
    "  var t = { dark: true, _lastCheck: 0 };\n"
    "  t.update = function() {\n"
    "    var now = Date.now();\n"
    "    if (now - t._lastCheck < 5000) return t;\n"
    "    t._lastCheck = now;\n"
    "    var h = getLAHour();\n"
    "    t.dark = (h >= 20 || h < 7);\n"
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
    JS_SetPropertyStr(ctx, global, "__paintApi", JS_DupValue(ctx, paint_api));
    JS_FreeValue(ctx, paint_api);

    // Register top-level graphics functions
    JS_SetPropertyStr(ctx, global, "wipe", JS_NewCFunction(ctx, js_wipe, "wipe", 3));
    JS_SetPropertyStr(ctx, global, "ink", JS_NewCFunction(ctx, js_ink, "ink", 4));
    JS_SetPropertyStr(ctx, global, "line", JS_NewCFunction(ctx, js_line, "line", 4));
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
        fprintf(stderr, "[js] KidLisp bundle not found at /lib/kidlisp-bundle.js (optional)\n");
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
    JS_SetPropertyStr(ctx, speaker, "sampleRate",
        JS_NewInt32(ctx, rt->audio ? (int)rt->audio->actual_rate : AUDIO_SAMPLE_RATE));

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
    JS_SetPropertyStr(ctx, glitch, "set", JS_NewCFunction(ctx, js_noop, "set", 1));
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
    JS_SetPropertyStr(ctx, sound, "sample", samp);

    // TTS
    JS_SetPropertyStr(ctx, sound, "speak", JS_NewCFunction(ctx, js_speak, "speak", 1));

    // sound.paint (visualization helpers)
    JSValue sound_paint = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, sound_paint, "bars", JS_NewCFunction(ctx, js_noop, "bars", 10));
    JS_SetPropertyStr(ctx, sound_paint, "audioEngineBadge", JS_NewCFunction(ctx, js_noop, "audioEngineBadge", 5));
    JS_SetPropertyStr(ctx, sound, "paint", sound_paint);

    // midi (stub)
    JSValue midi = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, midi, "connect", JS_NewCFunction(ctx, js_noop, "connect", 0));
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
    } else {
        JS_SetPropertyStr(ctx, obj, "state", JS_NewInt32(ctx, WIFI_STATE_OFF));
        JS_SetPropertyStr(ctx, obj, "status", JS_NewString(ctx, "no wifi"));
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

static JSValue js_udp_connect(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || !current_rt->udp) return JS_UNDEFINED;
    const char *host = argc > 0 ? JS_ToCString(ctx, argv[0]) : NULL;
    if (!host) host = "session-server.aesthetic.computer";
    int port = UDP_FAIRY_PORT;
    if (argc > 1) JS_ToInt32(ctx, &port, argv[1]);
    // Set handle for identity
    if (current_rt->handle[0]) {
        pthread_mutex_lock(&current_rt->udp->mu);
        strncpy(current_rt->udp->handle, current_rt->handle, 63);
        current_rt->udp->handle[63] = 0;
        pthread_mutex_unlock(&current_rt->udp->mu);
    }
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

static JSValue build_udp_obj(JSContext *ctx, const char *phase) {
    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "connect", JS_NewCFunction(ctx, js_udp_connect, "connect", 2));
    JS_SetPropertyStr(ctx, obj, "sendFairy", JS_NewCFunction(ctx, js_udp_send_fairy, "sendFairy", 2));

    ACUdp *udp = current_rt ? current_rt->udp : NULL;
    if (udp) {
        JS_SetPropertyStr(ctx, obj, "connected", JS_NewBool(ctx, udp->connected));

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
static long flash_verify(const char *src, const char *dst) {
    // Evict ONLY the destination file's page cache using posix_fadvise.
    // DO NOT use drop_caches — that nukes tmpfs pages and destroys the source!
    int dst_fd = open(dst, O_RDONLY);
    if (dst_fd >= 0) {
        // Get file size for fadvise range
        struct stat st;
        if (fstat(dst_fd, &st) == 0) {
            posix_fadvise(dst_fd, 0, st.st_size, POSIX_FADV_DONTNEED);
            ac_log("[verify] evicted dst page cache (%ld bytes)", (long)st.st_size);
        }
        close(dst_fd);
    }

    FILE *fa = fopen(src, "rb");
    FILE *fb = fopen(dst, "rb");
    if (!fa || !fb) {
        ac_log("[verify] fopen failed: src=%s dst=%s", fa ? "ok" : "FAIL", fb ? "ok" : "FAIL");
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
        fclose(fa);
        fclose(fb);
        return -1;
    }
    fclose(fa);
    fclose(fb);
    ac_log("[verify] OK: %ld bytes match", verified);
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

// Flash update background thread: mount EFI, copy vmlinuz, sync, umount
// All operations use C syscalls — no shell commands (cp/mkdir/mount not in initramfs PATH)
// NOTE: current_rt is __thread (thread-local); pass ACRuntime * via arg instead
static void *flash_thread_fn(void *arg) {
    ACRuntime *rt = (ACRuntime *)arg;
    if (!rt) return NULL;
    rt->flash_log_count = 0;
    rt->flash_dst[0] = '\0';
    rt->flash_same_device = 0;

    ac_log("[flash] starting: src=%s device=%s", rt->flash_src, rt->flash_device);
    flash_tlog(rt, "src=%s", rt->flash_src);
    flash_tlog(rt, "device=%s", rt->flash_device);

    // Check if device node exists
    {
        struct stat dev_st;
        if (stat(rt->flash_device, &dev_st) != 0) {
            ac_log("[flash] device %s does not exist (errno=%d %s)",
                   rt->flash_device, errno, strerror(errno));
            rt->flash_phase = 4;
            rt->flash_ok = 0;
            rt->flash_pending = 0;
            rt->flash_done = 1;
            return NULL;
        }
        ac_log("[flash] device %s exists (mode=0%o)", rt->flash_device, dev_st.st_mode);
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
            } else {
                ac_log("[flash] ABORT: mount %s failed and /mnt has no EFI/BOOT",
                       rt->flash_device);
                rt->flash_phase = 4;
                rt->flash_ok = 0;
                rt->flash_pending = 0;
                rt->flash_done = 1;
                return NULL;
            }
        }
    }
    // Create EFI directory structure if it doesn't exist (fresh NVMe install)
    char efi_dir[512];
    snprintf(efi_dir, sizeof(efi_dir), "%s/EFI", efi_mount);
    mkdir(efi_dir, 0755);
    snprintf(efi_dir, sizeof(efi_dir), "%s/EFI/BOOT", efi_mount);
    mkdir(efi_dir, 0755);
    if (access(efi_dir, F_OK) != 0) {
        ac_log("[flash] ABORT: %s could not be created", efi_dir);
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

    // Close the log file if writing to the same vfat partition as /mnt
    int same_mount = same_as_mnt || !did_mount;
    if (same_mount) {
        ac_log("[flash] pausing log for write to /mnt");
        ac_log_pause();
    }

    // Pre-flight: validate source file exists and has reasonable size
    {
        struct stat src_st;
        if (stat(rt->flash_src, &src_st) != 0 || src_st.st_size < 1048576) {
            ac_log("[flash] ABORT: source %s invalid (size=%ld, errno=%d)",
                   rt->flash_src, (long)(src_st.st_size), errno);
            if (same_mount) { ac_log_resume(); }
            if (did_mount) umount("/tmp/efi");
            rt->flash_phase = 4;
            rt->flash_ok = 0;
            rt->flash_pending = 0;
            rt->flash_done = 1;
            return NULL;
        }
        ac_log("[flash] source validated: %ld bytes", (long)src_st.st_size);
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

    int mnt_fd = open(efi_mount, O_RDONLY);
    if (mnt_fd >= 0) {
        if (syncfs(mnt_fd) != 0)
            ac_log("[flash] syncfs failed: errno=%d (%s)", errno, strerror(errno));
        close(mnt_fd);
    } else {
        ac_log("[flash] WARNING: open(%s) for syncfs failed errno=%d", efi_mount, errno);
    }
    sync();
    // vfat write-back can be slow; give the block layer time to flush
    usleep(500000);  // 500ms
    sync();

    // If same device: remount read-only then read-write to force vfat metadata flush
    if (same_as_mnt) {
        ac_log("[flash] remounting /mnt ro+rw to force vfat flush");
        mount(NULL, "/mnt", NULL, MS_REMOUNT | MS_RDONLY, NULL);
        usleep(100000);
        mount(NULL, "/mnt", NULL, MS_REMOUNT, NULL);
    }

    ac_log("[flash] sync complete");

    if (copied <= 0) {
        if (same_mount) { ac_log_resume(); }
        ac_log("[flash] copy failed (copied=%ld)", copied);
        rt->flash_phase = 4;
        rt->flash_ok = 0;
        rt->flash_pending = 0;
        rt->flash_done = 1;
        return NULL;
    }

    // Phase 3: Verify — read back from physical disk and compare byte-for-byte
    rt->flash_phase = 3;
    long verified = flash_verify(rt->flash_src, dst);
    flash_tlog(rt, "verify=%ld (expected=%ld)", verified, copied);

    // Reopen log file after flash+verify
    if (same_mount) {
        ac_log_resume();
        ac_log("[flash] log resumed after write+verify");
    }

    if (did_mount) {
        umount("/tmp/efi");
        ac_log("[flash] unmounted /tmp/efi");
    }

    rt->flash_verified_bytes = verified;
    if (verified != copied) {
        ac_log("[flash] VERIFY FAILED: wrote %ld, verified %ld", copied, verified);
        // CRITICAL: restore previous kernel so device remains bootable
        {
            char prev[512];
            snprintf(prev, sizeof(prev), "%s.prev", dst);
            if (access(prev, F_OK) == 0) {
                unlink(dst);
                if (rename(prev, dst) == 0) {
                    ac_log("[flash] RESTORED previous kernel from %s — device still bootable", prev);
                    sync();
                } else {
                    ac_log("[flash] CRITICAL: restore failed errno=%d", errno);
                }
            }
        }
        rt->flash_phase = 4;
        rt->flash_ok = 0;
        rt->flash_pending = 0;
        rt->flash_done = 1;
        return NULL;
    }

    // Remove downloaded file to free /tmp space
    unlink(rt->flash_src);
    ac_log("[flash] done: %ld bytes written, verified OK", copied);
    rt->flash_phase = 4;
    rt->flash_ok = 1;
    rt->flash_pending = 0;
    rt->flash_done = 1;
    return NULL;
}

// system.flashUpdate(srcPath[, devicePath])
// devicePath defaults to auto-detected boot device if omitted
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

    if (getpid() == 1) {
        reboot(LINUX_REBOOT_CMD_RESTART);
    } else {
        // Under cage: tell PID 1 to reboot, then exit so cage closes
        kill(1, SIGUSR2);  // SIGUSR2 = reboot request
        _exit(0);
    }
    return JS_UNDEFINED;
}

// system.poweroff() — clean shutdown (power off)
static JSValue js_poweroff(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)ctx; (void)this_val; (void)argc; (void)argv;
    ac_log("[system] poweroff requested");
    perf_flush();
    if (current_rt && current_rt->audio) {
        audio_shutdown_sound(current_rt->audio);
        usleep(500000);
    }
    ac_log("[poweroff] syncing filesystems...");
    ac_log_flush();
    sync(); usleep(500000); sync(); usleep(500000); sync();
    ac_log("[poweroff] executing power off syscall");
    ac_log_flush();
    if (getpid() == 1) {
        reboot(LINUX_REBOOT_CMD_POWER_OFF);
    } else {
        // Under cage: tell PID 1 to power off, then exit so cage closes
        kill(1, SIGTERM);  // SIGTERM = poweroff request
        _exit(0);
    }
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
                snprintf(out, sizeof(out), "%.*s\"%s\"%s",
                         prefix_len, buf, val, vend);
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
            snprintf(out, sizeof(out), "%.*s%s\"%s\":\"%s\"}",
                     prefix_len, buf, has_content ? "," : "", key, val);
        }
    } else {
        // No valid JSON — create new
        snprintf(out, sizeof(out), "{\"%s\":\"%s\"}", key, val);
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

// system.listPieces() — scan /pieces/*.mjs, return array of piece names
static JSValue js_list_pieces(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    JSValue arr = JS_NewArray(ctx);
    DIR *d = opendir("/pieces");
    if (d) {
        struct dirent *ent;
        uint32_t idx = 0;
        while ((ent = readdir(d)) != NULL) {
            char *dot = strstr(ent->d_name, ".mjs");
            if (dot && dot[4] == '\0' && ent->d_name[0] != '.') {
                char name[64];
                int len = (int)(dot - ent->d_name);
                if (len > 63) len = 63;
                memcpy(name, ent->d_name, len);
                name[len] = '\0';
                JS_SetPropertyUint32(ctx, arr, idx++, JS_NewString(ctx, name));
            }
        }
        closedir(d);
    }
    return arr;
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

    // File I/O — system.readFile(path) / system.writeFile(path, data)
    JS_SetPropertyStr(ctx, sys, "readFile",  JS_NewCFunction(ctx, js_read_file,  "readFile",  1));
    JS_SetPropertyStr(ctx, sys, "writeFile", JS_NewCFunction(ctx, js_write_file, "writeFile", 2));

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

    // Piece navigation
    JS_SetPropertyStr(ctx, sys, "jump",
                      JS_NewCFunction(ctx, js_jump, "jump", 1));

    // system.listPieces() — scan /pieces/*.mjs and return name array
    JS_SetPropertyStr(ctx, sys, "listPieces",
                      JS_NewCFunction(ctx, js_list_pieces, "listPieces", 0));

    // Volume and brightness control from JS
    JS_SetPropertyStr(ctx, sys, "volumeAdjust",
                      JS_NewCFunction(ctx, js_volume_adjust, "volumeAdjust", 1));
    JS_SetPropertyStr(ctx, sys, "brightnessAdjust",
                      JS_NewCFunction(ctx, js_brightness_adjust, "brightnessAdjust", 1));
    JS_SetPropertyStr(ctx, sys, "qrEncode",
                      JS_NewCFunction(ctx, js_qr_encode, "qrEncode", 1));
    JS_SetPropertyStr(ctx, sys, "openBrowser",
                      JS_NewCFunction(ctx, js_open_browser, "openBrowser", 1));

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
        JS_SetPropertyStr(ctx, api, "net", net);
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

    // paste, page, layer, sharpen (stubs)
    JS_SetPropertyStr(ctx, api, "paste", JS_NewCFunction(ctx, js_noop, "paste", 4));
    JS_SetPropertyStr(ctx, api, "page", JS_NewCFunction(ctx, js_noop, "page", 1));
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
    // after module evaluation (QuickJS modules don't expose exports publicly)
    const char *export_shim =
        "\n;if(typeof boot==='function')globalThis.boot=boot;"
        "if(typeof paint==='function')globalThis.paint=paint;"
        "if(typeof act==='function')globalThis.act=act;"
        "if(typeof sim==='function')globalThis.sim=sim;"
        "if(typeof leave==='function')globalThis.leave=leave;"
        "if(typeof beat==='function')globalThis.beat=beat;"
        "if(typeof configureAutopat==='function')globalThis.configureAutopat=configureAutopat;"
        "if(typeof system!=='undefined')globalThis.__pieceSystem=system;\n";
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

void js_call_paint(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->paint_fn)) return;
    current_rt = rt;
    rt->paint_count++;
    JSValue api = build_api(rt->ctx, rt, "paint");
    JSValue result = JS_Call(rt->ctx, rt->paint_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        JSValue stack = JS_GetPropertyStr(rt->ctx, exc, "stack");
        const char *stack_str = JS_ToCString(rt->ctx, stack);
        fprintf(stderr, "[js] paint() error: %s\n%s\n", str, stack_str ? stack_str : "");
        if (stack_str) JS_FreeCString(rt->ctx, stack_str);
        JS_FreeValue(rt->ctx, stack);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);
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

    // Update FPS camera from key state + trackpad delta
    if (rt->pen_locked && rt->fps_system_active) {
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
        if (stack_str) JS_FreeCString(rt->ctx, stack_str);
        JS_FreeValue(rt->ctx, stack);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);

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
