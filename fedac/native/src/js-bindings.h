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
#include "udp-client.h"
#include "camera.h"
#include "pty.h"
#include <pthread.h>

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
    ACWs *ws2;   // Second WebSocket slot (machines monitoring)
    ACUdp *udp;
    int paint_count;
    int sim_count;
    // Async HTTP fetch state (curl in background)
    int fetch_pending;           // 1 = waiting for curl
    char fetch_result[8192];     // JSON response when done, empty otherwise
    char fetch_error[256];       // one-shot fetch error string

    // Binary fetch for OS update (curl -L -o destPath url)
    volatile int   fetch_binary_pending;  // 1 = curl running
    volatile float fetch_binary_progress; // 0.0-1.0 (polled from file size)
    volatile int   fetch_binary_done;     // 1 = complete (check fetch_binary_ok)
    volatile int   fetch_binary_ok;       // 1 = curl exited 0
    char  fetch_binary_dest[256];         // download destination path
    long  fetch_binary_expected;          // expected file size for progress

    // Flash update (mount EFI partition, copy vmlinuz, verify, umount)
    volatile int   flash_pending;         // 1 = flash thread running
    volatile int   flash_done;            // 1 = complete
    volatile int   flash_ok;             // 1 = success
    volatile int   flash_phase;          // 0=idle 1=writing 2=syncing 3=verifying 4=done
    volatile long  flash_verified_bytes; // bytes verified after readback
    pthread_t flash_thread;
    char  flash_src[256];                 // source vmlinuz path
    char  flash_device[64];              // EFI partition device, e.g. /dev/sda1 or /dev/nvme0n1p1
    char  flash_dst[256];                // actual destination path written to
    volatile int flash_same_device;      // 1 = target is same device as /mnt (NVMe-to-NVMe)
    // Flash telemetry ring buffer (read from JS for diagnostics)
    char  flash_log[16][128];            // last 16 log lines
    volatile int flash_log_count;        // total lines written (modulo 16 for ring index)

    // Camera QR scanning (V4L2 + quirc)
    ACCamera camera;
    volatile int qr_scan_active;         // 1 = camera open, scanning each frame
    pthread_t qr_thread;
    volatile int qr_thread_running;

    // Piece navigation (system.jump)
    volatile int jump_requested;          // 1 = JS called system.jump()
    char jump_target[128];               // piece name, e.g. "prompt" or "notepat"
    char jump_params[8][64];             // colon-separated params (e.g. "chat:clock" → ["clock"])
    int jump_param_count;

    // PTY terminal emulator
    ACPty pty;
    int pty_active;                      // 1 = PTY session is running

    // User config (read from /mnt/config.json on EFI partition)
    char handle[64];                     // e.g. "jeffrey" (without @)
    char piece[64];                      // default piece name, e.g. "notepat"
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
