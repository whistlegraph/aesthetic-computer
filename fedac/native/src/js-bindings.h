#ifndef AC_JS_BINDINGS_H
#define AC_JS_BINDINGS_H

#include "quickjs.h"
#include "graph.h"
#include "input.h"
#include "font.h"

typedef struct {
    JSRuntime *rt;
    JSContext *ctx;

    // Cached piece lifecycle functions
    JSValue boot_fn;
    JSValue paint_fn;
    JSValue act_fn;
    JSValue sim_fn;
    JSValue leave_fn;

    // State
    ACGraph *graph;
    ACInput *input;
    int paint_count;
    int sim_count;
} ACRuntime;

// Initialize QuickJS and register all AC API bindings
ACRuntime *js_init(ACGraph *graph, ACInput *input);

// Load and prepare a piece module
int js_load_piece(ACRuntime *rt, const char *path);

// Call piece lifecycle functions
void js_call_boot(ACRuntime *rt);
void js_call_paint(ACRuntime *rt);
void js_call_act(ACRuntime *rt);
void js_call_sim(ACRuntime *rt);
void js_call_leave(ACRuntime *rt);

// Cleanup
void js_destroy(ACRuntime *rt);

#endif
