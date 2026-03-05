#include "js-bindings.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// Thread-local reference to current runtime (for C callbacks from JS)
static __thread ACRuntime *current_rt = NULL;

// ============================================================
// JS Native Functions — Graphics
// ============================================================

static JSValue js_wipe(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
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
        // Could be a single number (grayscale) or string
        int v;
        if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
            c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
        }
    }
    graph_wipe(g, c);
    return JS_UNDEFINED;
}

// ink() returns a chainable object with box(), line(), etc.
static JSValue js_ink(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
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
        int v;
        if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
            c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
        }
    }
    graph_ink(g, c);

    // Return the chainable paint API object
    // For now, return the global paint API so ink(r,g,b).box(...) works
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue paint_api = JS_GetPropertyStr(ctx, global, "__paintApi");
    JS_FreeValue(ctx, global);
    return paint_api;
}

static JSValue js_plot(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 2) return JS_UNDEFINED;
    int x, y;
    JS_ToInt32(ctx, &x, argv[0]);
    JS_ToInt32(ctx, &y, argv[1]);
    graph_plot(current_rt->graph, x, y);
    return JS_UNDEFINED;
}

static JSValue js_line(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
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

    // Return chainable
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue paint_api = JS_GetPropertyStr(ctx, global, "__paintApi");
    JS_FreeValue(ctx, global);
    return paint_api;
}

static JSValue js_circle(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
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
    if (argc < 1) return JS_UNDEFINED;
    const char *text = JS_ToCString(ctx, argv[0]);
    if (!text) return JS_UNDEFINED;

    int x = 0, y = 0, scale = 1;

    // write(text, {x, y}, ...)  or  write(text, x, y)
    if (argc >= 2) {
        if (JS_IsObject(argv[1])) {
            JSValue vx = JS_GetPropertyStr(ctx, argv[1], "x");
            JSValue vy = JS_GetPropertyStr(ctx, argv[1], "y");
            if (!JS_IsUndefined(vx)) JS_ToInt32(ctx, &x, vx);
            if (!JS_IsUndefined(vy)) JS_ToInt32(ctx, &y, vy);

            // Check for center:"xy" etc.
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

            // Check for size
            JSValue size = JS_GetPropertyStr(ctx, argv[1], "size");
            if (!JS_IsUndefined(size)) JS_ToInt32(ctx, &scale, size);
            JS_FreeValue(ctx, size);

            JS_FreeValue(ctx, vx);
            JS_FreeValue(ctx, vy);
        }
    }

    font_draw(current_rt->graph, text, x, y, scale);
    JS_FreeCString(ctx, text);
    return JS_UNDEFINED;
}

static JSValue js_scroll(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    int dx = 0, dy = 0;
    if (argc >= 1) JS_ToInt32(ctx, &dx, argv[0]);
    if (argc >= 2) JS_ToInt32(ctx, &dy, argv[1]);
    graph_scroll(current_rt->graph, dx, dy);
    return JS_UNDEFINED;
}

static JSValue js_blur(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    int s = 1;
    if (argc >= 1) JS_ToInt32(ctx, &s, argv[0]);
    graph_blur(current_rt->graph, s);
    return JS_UNDEFINED;
}

// ============================================================
// JS Native Functions — System
// ============================================================

static JSValue js_screen_get(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    JSValue obj = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, obj, "width", JS_NewInt32(ctx, current_rt->graph->fb->width));
    JS_SetPropertyStr(ctx, obj, "height", JS_NewInt32(ctx, current_rt->graph->fb->height));
    return obj;
}

// ============================================================
// Event System
// ============================================================

// Create the event.is() function for act()
static JSValue js_event_is(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    if (argc < 1) return JS_FALSE;
    const char *pattern = JS_ToCString(ctx, argv[0]);
    if (!pattern) return JS_FALSE;

    // Get the event data from 'this'
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
    else if (strncmp(pattern, "keyboard:down:", 14) == 0)
        match = (type == AC_EVENT_KEYBOARD_DOWN && key && strcmp(key, pattern + 14) == 0);
    else if (strncmp(pattern, "keyboard:up:", 12) == 0)
        match = (type == AC_EVENT_KEYBOARD_UP && key && strcmp(key, pattern + 12) == 0);

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

    // Add is() method
    JS_SetPropertyStr(ctx, obj, "is", JS_NewCFunction(ctx, js_event_is, "is", 1));

    return obj;
}

// ============================================================
// JS Native Functions — Builtins
// ============================================================

static JSValue js_console_log(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    for (int i = 0; i < argc; i++) {
        const char *s = JS_ToCString(ctx, argv[i]);
        if (s) { fprintf(stderr, "%s%s", i ? " " : "", s); JS_FreeCString(ctx, s); }
    }
    fprintf(stderr, "\n");
    return JS_UNDEFINED;
}

static JSValue js_performance_now(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    double ms = ts.tv_sec * 1000.0 + ts.tv_nsec / 1000000.0;
    return JS_NewFloat64(ctx, ms);
}

// ============================================================
// Runtime Init / Lifecycle
// ============================================================

ACRuntime *js_init(ACGraph *graph, ACInput *input) {
    ACRuntime *rt = calloc(1, sizeof(ACRuntime));
    if (!rt) return NULL;

    rt->graph = graph;
    rt->input = input;
    rt->boot_fn = JS_UNDEFINED;
    rt->paint_fn = JS_UNDEFINED;
    rt->act_fn = JS_UNDEFINED;
    rt->sim_fn = JS_UNDEFINED;
    rt->leave_fn = JS_UNDEFINED;

    rt->rt = JS_NewRuntime();
    rt->ctx = JS_NewContext(rt->rt);

    current_rt = rt;

    JSContext *ctx = rt->ctx;
    JSValue global = JS_GetGlobalObject(ctx);

    // Create the chainable paint API object
    JSValue paint_api = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, paint_api, "box", JS_NewCFunction(ctx, js_box, "box", 5));
    JS_SetPropertyStr(ctx, paint_api, "line", JS_NewCFunction(ctx, js_line, "line", 4));
    JS_SetPropertyStr(ctx, paint_api, "circle", JS_NewCFunction(ctx, js_circle, "circle", 4));
    JS_SetPropertyStr(ctx, paint_api, "plot", JS_NewCFunction(ctx, js_plot, "plot", 2));
    JS_SetPropertyStr(ctx, paint_api, "write", JS_NewCFunction(ctx, js_write, "write", 6));
    JS_SetPropertyStr(ctx, paint_api, "scroll", JS_NewCFunction(ctx, js_scroll, "scroll", 2));
    JS_SetPropertyStr(ctx, paint_api, "blur", JS_NewCFunction(ctx, js_blur, "blur", 1));
    JS_SetPropertyStr(ctx, global, "__paintApi", JS_DupValue(ctx, paint_api));
    JS_FreeValue(ctx, paint_api);

    // Register top-level functions (for destructuring in piece params)
    JS_SetPropertyStr(ctx, global, "wipe", JS_NewCFunction(ctx, js_wipe, "wipe", 3));
    JS_SetPropertyStr(ctx, global, "ink", JS_NewCFunction(ctx, js_ink, "ink", 4));
    JS_SetPropertyStr(ctx, global, "line", JS_NewCFunction(ctx, js_line, "line", 4));
    JS_SetPropertyStr(ctx, global, "box", JS_NewCFunction(ctx, js_box, "box", 5));
    JS_SetPropertyStr(ctx, global, "circle", JS_NewCFunction(ctx, js_circle, "circle", 4));
    JS_SetPropertyStr(ctx, global, "plot", JS_NewCFunction(ctx, js_plot, "plot", 2));
    JS_SetPropertyStr(ctx, global, "write", JS_NewCFunction(ctx, js_write, "write", 6));
    JS_SetPropertyStr(ctx, global, "scroll", JS_NewCFunction(ctx, js_scroll, "scroll", 2));
    JS_SetPropertyStr(ctx, global, "blur", JS_NewCFunction(ctx, js_blur, "blur", 1));

    // console.log
    JSValue console = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, console, "log",
        JS_NewCFunction(ctx, js_console_log, "log", 1));
    JS_SetPropertyStr(ctx, global, "console", console);

    // performance.now()
    {
        JSValue perf = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, perf, "now",
            JS_NewCFunction(ctx, js_performance_now, "now", 0));
        JS_SetPropertyStr(ctx, global, "performance", perf);
    }

    // Math.random, Math.floor, Math.abs, Math.sin, Math.cos, etc. are built into QuickJS

    JS_FreeValue(ctx, global);
    return rt;
}

int js_load_piece(ACRuntime *rt, const char *path) {
    JSContext *ctx = rt->ctx;
    current_rt = rt;

    // Read file
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

    // Evaluate as module
    JSValue val = JS_Eval(ctx, src, len, path, JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
    free(src);

    if (JS_IsException(val)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        fprintf(stderr, "[js] Parse error: %s\n", str);
        JS_FreeCString(ctx, str);
        JS_FreeValue(ctx, exc);
        return -1;
    }

    // Execute the module
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

    // Get exported functions from the module's namespace
    // QuickJS stores module exports; we need to access them
    // For simplicity, pieces should set globals:
    // globalThis.boot = boot; globalThis.paint = paint; etc.
    // OR we use the module namespace API

    JSValue global = JS_GetGlobalObject(ctx);
    rt->boot_fn = JS_GetPropertyStr(ctx, global, "boot");
    rt->paint_fn = JS_GetPropertyStr(ctx, global, "paint");
    rt->act_fn = JS_GetPropertyStr(ctx, global, "act");
    rt->sim_fn = JS_GetPropertyStr(ctx, global, "sim");
    rt->leave_fn = JS_GetPropertyStr(ctx, global, "leave");
    JS_FreeValue(ctx, global);

    fprintf(stderr, "[js] Loaded piece: %s\n", path);
    fprintf(stderr, "[js] boot=%s paint=%s act=%s sim=%s\n",
            JS_IsFunction(ctx, rt->boot_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->paint_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->act_fn) ? "yes" : "no",
            JS_IsFunction(ctx, rt->sim_fn) ? "yes" : "no");

    return 0;
}

// Build the API object passed to lifecycle functions
static JSValue build_api(JSContext *ctx, ACRuntime *rt, const char *phase) {
    JSValue api = JS_NewObject(ctx);
    JSValue global = JS_GetGlobalObject(ctx);

    // Common properties
    JS_SetPropertyStr(ctx, api, "wipe", JS_GetPropertyStr(ctx, global, "wipe"));
    JS_SetPropertyStr(ctx, api, "ink", JS_GetPropertyStr(ctx, global, "ink"));
    JS_SetPropertyStr(ctx, api, "line", JS_GetPropertyStr(ctx, global, "line"));
    JS_SetPropertyStr(ctx, api, "box", JS_GetPropertyStr(ctx, global, "box"));
    JS_SetPropertyStr(ctx, api, "circle", JS_GetPropertyStr(ctx, global, "circle"));
    JS_SetPropertyStr(ctx, api, "plot", JS_GetPropertyStr(ctx, global, "plot"));
    JS_SetPropertyStr(ctx, api, "write", JS_GetPropertyStr(ctx, global, "write"));
    JS_SetPropertyStr(ctx, api, "scroll", JS_GetPropertyStr(ctx, global, "scroll"));
    JS_SetPropertyStr(ctx, api, "blur", JS_GetPropertyStr(ctx, global, "blur"));

    // screen object
    {
        JSValue s = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, s, "width", JS_NewInt32(ctx, rt->graph->fb->width));
        JS_SetPropertyStr(ctx, s, "height", JS_NewInt32(ctx, rt->graph->fb->height));
        JS_SetPropertyStr(ctx, api, "screen", s);
    }

    if (strcmp(phase, "paint") == 0) {
        JS_SetPropertyStr(ctx, api, "paintCount", JS_NewInt32(ctx, rt->paint_count));
    }
    if (strcmp(phase, "sim") == 0) {
        JS_SetPropertyStr(ctx, api, "simCount", JS_NewInt32(ctx, rt->sim_count));
    }

    JS_FreeValue(ctx, global);
    return api;
}

void js_call_boot(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->boot_fn)) return;
    current_rt = rt;
    JSValue api = build_api(rt->ctx, rt, "boot");
    JSValue result = JS_Call(rt->ctx, rt->boot_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        fprintf(stderr, "[js] boot() error: %s\n", str);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);
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
        fprintf(stderr, "[js] paint() error: %s\n", str);
        JS_FreeCString(rt->ctx, str);
        JS_FreeValue(rt->ctx, exc);
    }
    JS_FreeValue(rt->ctx, result);
    JS_FreeValue(rt->ctx, api);
}

void js_call_act(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->act_fn)) return;
    current_rt = rt;

    ACInput *input = rt->input;
    for (int i = 0; i < input->event_count; i++) {
        JSValue api = build_api(rt->ctx, rt, "act");
        JSValue event = make_event_object(rt->ctx, &input->events[i]);
        JS_SetPropertyStr(rt->ctx, api, "event", event);

        JSValue result = JS_Call(rt->ctx, rt->act_fn, JS_UNDEFINED, 1, &api);
        if (JS_IsException(result)) {
            JSValue exc = JS_GetException(rt->ctx);
            const char *str = JS_ToCString(rt->ctx, exc);
            fprintf(stderr, "[js] act() error: %s\n", str);
            JS_FreeCString(rt->ctx, str);
            JS_FreeValue(rt->ctx, exc);
        }
        JS_FreeValue(rt->ctx, result);
        JS_FreeValue(rt->ctx, api);
    }
}

void js_call_sim(ACRuntime *rt) {
    if (!JS_IsFunction(rt->ctx, rt->sim_fn)) return;
    current_rt = rt;
    rt->sim_count++;
    JSValue api = build_api(rt->ctx, rt, "sim");
    JSValue result = JS_Call(rt->ctx, rt->sim_fn, JS_UNDEFINED, 1, &api);
    if (JS_IsException(result)) {
        JSValue exc = JS_GetException(rt->ctx);
        const char *str = JS_ToCString(rt->ctx, exc);
        fprintf(stderr, "[js] sim() error: %s\n", str);
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
    JS_FreeContext(rt->ctx);
    JS_FreeRuntime(rt->rt);
    free(rt);
}
