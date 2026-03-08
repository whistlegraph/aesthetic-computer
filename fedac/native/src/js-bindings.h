#ifndef AC_JS_BINDINGS_H
#define AC_JS_BINDINGS_H

#include "quickjs.h"
#include "graph.h"
#include "input.h"
#include "font.h"
#include "audio.h"
#include "wifi.h"
#include "tts.h"
#include "drm-display.h"
#include "ws-client.h"

typedef struct {
    JSRuntime *rt;
    JSContext *ctx;

    // Cached piece lifecycle functions
    JSValue boot_fn;
    JSValue paint_fn;
    JSValue act_fn;
    JSValue sim_fn;
    JSValue leave_fn;
    JSValue beat_fn;

    // State
    ACGraph *graph;
    ACInput *input;
    ACAudio *audio;
    ACWifi *wifi;
    ACTts *tts;
    ACSecondaryDisplay *hdmi;
    ACWs *ws;
    int paint_count;
    int sim_count;
    // Async HTTP fetch state (curl in background)
    int fetch_pending;           // 1 = waiting for curl
    char fetch_result[8192];     // JSON response when done, empty otherwise
} ACRuntime;

// Initialize QuickJS and register all AC API bindings
ACRuntime *js_init(ACGraph *graph, ACInput *input, ACAudio *audio, ACWifi *wifi, ACTts *tts);

// Load and prepare a piece module
int js_load_piece(ACRuntime *rt, const char *path);

// Call piece lifecycle functions
void js_call_boot(ACRuntime *rt);
void js_call_paint(ACRuntime *rt);
void js_call_act(ACRuntime *rt);
void js_call_sim(ACRuntime *rt);
void js_call_beat(ACRuntime *rt);
void js_call_leave(ACRuntime *rt);

// Cleanup
void js_destroy(ACRuntime *rt);

#endif
