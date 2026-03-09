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

// Defined in ac-native.c — logs to USB mount
extern void ac_log(const char *fmt, ...);

// Thread-local reference to current runtime (for C callbacks from JS)
static __thread ACRuntime *current_rt = NULL;
static __thread const char *current_phase = "boot";

// ============================================================
// JS Native Functions — Graphics
// ============================================================

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
        int v;
        if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
            c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
        }
    }
    graph_wipe(g, c);
    return JS_UNDEFINED;
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
        int v;
        if (JS_ToInt32(ctx, &v, argv[0]) == 0) {
            c = (ACColor){(uint8_t)v, (uint8_t)v, (uint8_t)v, 255};
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
    JS_SetPropertyStr(ctx, snd, "kill", JS_NewCFunction(ctx, (JSCFunction *)js_noop, "kill", 1));
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

    audio_kill(audio, id, fade);
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

// sound.glitch.toggle()
static JSValue js_glitch_toggle(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val; (void)argc; (void)argv;
    if (current_rt->audio) audio_glitch_toggle(current_rt->audio);
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

    rt->rt = JS_NewRuntime();
    rt->ctx = JS_NewContext(rt->rt);

    // Set module loader
    JS_SetModuleLoaderFunc(rt->rt, NULL, js_module_loader, NULL);

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

    // Register top-level graphics functions
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

    JS_FreeValue(ctx, global);
    return rt;
}

// painting(w, h, callback) — create a stub painting with width/height
// Calls the callback with a paint API (wipe, ink, box, etc.)
static JSValue js_painting(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    int w = 100, h = 100;
    if (argc >= 1) JS_ToInt32(ctx, &w, argv[0]);
    if (argc >= 2) JS_ToInt32(ctx, &h, argv[1]);

    // Create painting object with width/height
    JSValue painting = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, painting, "width", JS_NewInt32(ctx, w));
    JS_SetPropertyStr(ctx, painting, "height", JS_NewInt32(ctx, h));

    // If callback provided, call it with a paint API
    if (argc >= 3 && JS_IsFunction(ctx, argv[2])) {
        JSValue global = JS_GetGlobalObject(ctx);
        JSValue paint_api = JS_NewObject(ctx);
        JS_SetPropertyStr(ctx, paint_api, "wipe", JS_GetPropertyStr(ctx, global, "wipe"));
        JS_SetPropertyStr(ctx, paint_api, "ink", JS_GetPropertyStr(ctx, global, "ink"));
        JS_SetPropertyStr(ctx, paint_api, "box", JS_GetPropertyStr(ctx, global, "box"));
        JS_SetPropertyStr(ctx, paint_api, "line", JS_GetPropertyStr(ctx, global, "line"));
        JS_SetPropertyStr(ctx, paint_api, "circle", JS_GetPropertyStr(ctx, global, "circle"));
        JS_SetPropertyStr(ctx, paint_api, "plot", JS_GetPropertyStr(ctx, global, "plot"));
        JS_SetPropertyStr(ctx, paint_api, "write", JS_GetPropertyStr(ctx, global, "write"));
        // Stub kidlisp renderer
        JS_SetPropertyStr(ctx, paint_api, "kidlisp", JS_NewCFunction(ctx, js_noop, "kidlisp", 5));
        JS_FreeValue(ctx, global);

        JSValue result = JS_Call(ctx, argv[2], JS_UNDEFINED, 1, &paint_api);
        JS_FreeValue(ctx, result);
        JS_FreeValue(ctx, paint_api);
    }

    return painting;
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
    JS_SetPropertyStr(ctx, sound, "bpm", JS_NewFloat64(ctx, rt->audio ? rt->audio->bpm : 120.0));
    JS_SetPropertyStr(ctx, sound, "time", JS_NewFloat64(ctx, rt->audio ? rt->audio->time : 0.0));
    JS_SetPropertyStr(ctx, sound, "registerSample", JS_NewCFunction(ctx, js_noop, "registerSample", 3));

    // speaker sub-object
    JSValue speaker = JS_NewObject(ctx);
    JS_SetPropertyStr(ctx, speaker, "poll", JS_NewCFunction(ctx, js_noop, "poll", 0));
    JS_SetPropertyStr(ctx, speaker, "sampleRate",
        JS_NewInt32(ctx, rt->audio ? (int)rt->audio->actual_rate : AUDIO_SAMPLE_RATE));

    // waveforms
    JSValue waveforms = JS_NewObject(ctx);
    JSValue wf_left = JS_NewArray(ctx);
    JSValue wf_right = JS_NewArray(ctx);
    if (rt->audio) {
        for (int i = 0; i < 128; i++) {
            int idx = (rt->audio->waveform_pos - 128 + i + AUDIO_WAVEFORM_SIZE) % AUDIO_WAVEFORM_SIZE;
            JS_SetPropertyUint32(ctx, wf_left, i, JS_NewFloat64(ctx, rt->audio->waveform_left[idx]));
            JS_SetPropertyUint32(ctx, wf_right, i, JS_NewFloat64(ctx, rt->audio->waveform_right[idx]));
        }
    }
    JS_SetPropertyStr(ctx, waveforms, "left", wf_left);
    JS_SetPropertyStr(ctx, waveforms, "right", wf_right);
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
        // Poll scan/connect status
        if (wifi->state == WIFI_STATE_SCANNING) wifi_scan_poll(wifi);
        if (wifi->state == WIFI_STATE_CONNECTING) wifi_connect_poll(wifi);

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

// system.readFile(path) — read a file from disk, returns string or null
static JSValue js_read_file(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (argc < 1) return JS_NULL;
    const char *path = JS_ToCString(ctx, argv[0]);
    if (!path) return JS_NULL;
    FILE *fp = fopen(path, "r");
    if (!fp) { JS_FreeCString(ctx, path); return JS_NULL; }
    fseek(fp, 0, SEEK_END);
    long sz = ftell(fp);
    rewind(fp);
    if (sz <= 0 || sz > 65536) { fclose(fp); JS_FreeCString(ctx, path); return JS_NULL; }
    char *buf = malloc(sz + 1);
    if (!buf) { fclose(fp); JS_FreeCString(ctx, path); return JS_NULL; }
    sz = (long)fread(buf, 1, sz, fp);
    buf[sz] = 0;
    fclose(fp);
    JS_FreeCString(ctx, path);
    JSValue result = JS_NewString(ctx, buf);
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
    const char *url = JS_ToCString(ctx, argv[0]);
    if (!url) return JS_UNDEFINED;
    unlink("/tmp/ac_fetch.json");
    unlink("/tmp/ac_fetch_rc");
    char cmd[2048];
    snprintf(cmd, sizeof(cmd),
        "sh -c 'curl -sf --max-time 8 --output /tmp/ac_fetch.json \"%s\" 2>/dev/null;"
        " echo $? > /tmp/ac_fetch_rc' &", url);
    system(cmd);
    current_rt->fetch_pending = 1;
    current_rt->fetch_result[0] = 0;
    JS_FreeCString(ctx, url);
    return JS_UNDEFINED;
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
    unlink(dest);
    // Start curl in background
    char cmd[1024];
    snprintf(cmd, sizeof(cmd),
        "sh -c 'curl -sf -L --max-time 300 --output \"%s\" \"%s\" 2>/dev/null;"
        " echo $? > /tmp/ac_fb_rc' &", dest, url);
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

// Flash update background thread: mount EFI, copy vmlinuz, sync, umount
// NOTE: current_rt is __thread (thread-local); pass ACRuntime * via arg instead
static void *flash_thread_fn(void *arg) {
    ACRuntime *rt = (ACRuntime *)arg;
    if (!rt) return NULL;

    ac_log("[flash] starting: src=%s device=%s", rt->flash_src, rt->flash_device);
    if (system("mkdir -p /tmp/efi") != 0) {
        ac_log("[flash] mkdir failed");
        rt->flash_ok = 0; rt->flash_done = 1;
        return NULL;
    }
    // Unmount if already mounted
    system("umount /tmp/efi 2>/dev/null");
    char cmd[512];
    snprintf(cmd, sizeof(cmd),
        "mount \"%s\" /tmp/efi -t vfat -o rw 2>/dev/null", rt->flash_device);
    if (system(cmd) != 0) {
        ac_log("[flash] mount %s failed", rt->flash_device);
        rt->flash_ok = 0; rt->flash_done = 1;
        return NULL;
    }
    snprintf(cmd, sizeof(cmd),
        "cp -f \"%s\" /tmp/efi/EFI/BOOT/BOOTX64.EFI 2>/dev/null", rt->flash_src);
    int r = system(cmd);
    system("sync");
    system("umount /tmp/efi 2>/dev/null");
    if (r != 0) {
        ac_log("[flash] cp failed (r=%d)", r);
        rt->flash_ok = 0; rt->flash_done = 1;
        return NULL;
    }
    // Remove downloaded file to free /tmp space
    snprintf(cmd, sizeof(cmd), "rm -f \"%s\"", rt->flash_src);
    system(cmd);
    ac_log("[flash] done");
    rt->flash_ok = 1;
    rt->flash_done = 1;
    return NULL;
}

// system.flashUpdate(srcPath[, devicePath])
// devicePath defaults to auto-detected boot device if omitted
static JSValue js_flash_update(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)this_val;
    if (!current_rt || argc < 1) return JS_UNDEFINED;
    const char *src = JS_ToCString(ctx, argv[0]);
    if (!src) return JS_UNDEFINED;
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
    pthread_create(&current_rt->flash_thread, &attr, flash_thread_fn, current_rt);
    pthread_attr_destroy(&attr);
    return JS_UNDEFINED;
}

// system.reboot() — triggers system reboot
static JSValue js_reboot(JSContext *ctx, JSValueConst this_val, int argc, JSValueConst *argv) {
    (void)ctx; (void)this_val; (void)argc; (void)argv;
    ac_log("[system] reboot requested");
    system("reboot");
    return JS_UNDEFINED;
}

// system.jump(pieceName) — request piece switch (handled in main loop)
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

    // HDMI secondary display
    int has_hdmi = (current_rt && current_rt->hdmi && current_rt->hdmi->active);
    JS_SetPropertyStr(ctx, sys, "hasHdmi", JS_NewBool(ctx, has_hdmi));
    JS_SetPropertyStr(ctx, sys, "hdmi", JS_NewCFunction(ctx, js_hdmi_fill, "hdmi", 3));

    // WebSocket client — system.ws
    JS_SetPropertyStr(ctx, sys, "ws", build_ws_obj(ctx, current_phase));

    // Raw UDP fairy co-presence — system.udp
    JS_SetPropertyStr(ctx, sys, "udp", build_udp_obj(ctx, current_phase));

    // File I/O — system.readFile(path) / system.writeFile(path, data)
    JS_SetPropertyStr(ctx, sys, "readFile",  JS_NewCFunction(ctx, js_read_file,  "readFile",  1));
    JS_SetPropertyStr(ctx, sys, "writeFile", JS_NewCFunction(ctx, js_write_file, "writeFile", 2));

    // Async HTTP fetch — system.fetch(url) / system.fetchResult
    JS_SetPropertyStr(ctx, sys, "fetch", JS_NewCFunction(ctx, js_fetch, "fetch", 1));
    if (current_rt && current_rt->fetch_pending) {
        FILE *rc = fopen("/tmp/ac_fetch_rc", "r");
        if (rc) {
            int code = -1;
            fscanf(rc, "%d", &code);
            fclose(rc);
            unlink("/tmp/ac_fetch_rc");
            if (code == 0) {
                FILE *fp = fopen("/tmp/ac_fetch.json", "r");
                if (fp) {
                    int n = (int)fread(current_rt->fetch_result,
                                       1, sizeof(current_rt->fetch_result) - 1, fp);
                    fclose(fp);
                    current_rt->fetch_result[n] = 0;
                    unlink("/tmp/ac_fetch.json");
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

    // OS update version string
#ifdef AC_GIT_HASH
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
        JS_SetPropertyStr(ctx, sys, "config", config);
    }

    // Boot device detection (cached — detect once, not every frame)
    {
        static char boot_dev_cache[64] = {0};
        if (!boot_dev_cache[0]) detect_boot_device(boot_dev_cache, sizeof(boot_dev_cache));
        JS_SetPropertyStr(ctx, sys, "bootDevice", JS_NewString(ctx, boot_dev_cache));
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
                current_rt->fetch_binary_pending  = 0;
                current_rt->fetch_binary_done     = 1;
                current_rt->fetch_binary_ok       = (rc == 0) ? 1 : 0;
                current_rt->fetch_binary_progress = (rc == 0) ? 1.0f : 0.0f;
            }
        }
        JS_SetPropertyStr(ctx, sys, "fetchBinaryProgress",
                          JS_NewFloat64(ctx, (double)current_rt->fetch_binary_progress));
        JS_SetPropertyStr(ctx, sys, "fetchBinaryDone",
                          JS_NewBool(ctx, current_rt->fetch_binary_done));
        JS_SetPropertyStr(ctx, sys, "fetchBinaryOk",
                          JS_NewBool(ctx, current_rt->fetch_binary_ok));
        // Consume done flag so JS sees it only once
        if (current_rt->fetch_binary_done && !current_rt->fetch_binary_pending)
            current_rt->fetch_binary_done = 0;
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
        // Consume done flag
        if (current_rt->flash_done && !current_rt->flash_pending)
            current_rt->flash_done = 0;
    }

    // Piece navigation
    JS_SetPropertyStr(ctx, sys, "jump",
                      JS_NewCFunction(ctx, js_jump, "jump", 1));

    // Volume and brightness control from JS
    JS_SetPropertyStr(ctx, sys, "volumeAdjust",
                      JS_NewCFunction(ctx, js_volume_adjust, "volumeAdjust", 1));
    JS_SetPropertyStr(ctx, sys, "brightnessAdjust",
                      JS_NewCFunction(ctx, js_brightness_adjust, "brightnessAdjust", 1));

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

    // fps (no-op)
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
    JS_SetPropertyStr(ctx, api, "zoom", JS_NewInt32(ctx, 1));

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
        JS_SetPropertyStr(ctx, api_meta, "zoom", JS_GetPropertyStr(ctx, api, "zoom"));

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
        "if(typeof configureAutopat==='function')globalThis.configureAutopat=configureAutopat;\n";
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
    JS_FreeValue(ctx, global);

    fprintf(stderr, "[js] Loaded piece: %s\n", path);
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
    if (!JS_IsFunction(rt->ctx, rt->sim_fn)) return;
    current_rt = rt;
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
    JS_FreeContext(rt->ctx);
    JS_FreeRuntime(rt->rt);
    free(rt);
}
