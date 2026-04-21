// boot_anim.h — host-independent renderer for the ac-native boot animation.
//
// The Linux PID-1 init path (src/ac-native.c) and the macOS host
// (macos/main.c) both invoke this to paint a frame of the boot greeting
// (handle title, city subtitle, matrix rain, version panel, badges,
// optional install hint). All Linux-only concerns — evdev key drain,
// /mnt writes, machines_init, DRM display present — stay in the caller;
// this module only touches ACGraph + ACFramebuffer.
//
// Caller owns frame-timing and display present. To replay the animation
// once, loop f from 0 to BOOT_ANIM_FRAMES and call boot_anim_render_frame
// each tick.
#ifndef AC_BOOT_ANIM_H
#define AC_BOOT_ANIM_H

#include "graph.h"

#define BOOT_ANIM_FRAMES     120  // 2 s @ 60 fps
#define BOOT_ANIM_HOLD_FRAMES 40  // input ignored before this frame
#define BOOT_ANIM_RAIN_MAX_COLS 128

typedef struct {
    // Required.
    const char *title;          // e.g. "hi @jeffrey" — drawn centered
    const char *city;           // e.g. "Los Angeles" — "enjoy <city>!"

    // Optional per-handle palette. NULL → rainbow. When set, applies to
    // chars after the first '@' in title; chars before stay rainbow.
    const ACColor *title_colors;
    int            title_colors_len;

    // Hour-of-day drives the background fade + palette. Pass the LA hour
    // (0–23) or whatever locale you want to key the color scheme off.
    int hour;

    // Version panel (top-right). Any NULL field hides that line; pass
    // all-NULL to suppress the panel entirely. `is_new_version` shows the
    // "FRESH" badge next to the panel.
    const char *git_hash;
    const char *build_ts;
    const char *build_name;
    const char *driver_name;
    int         is_new_version;

    // Install hint (bottom-center). `show_install` gates visibility;
    // `is_installed` picks between "W: update" and "W: install to disk".
    int show_install;
    int is_installed;

    // Auth badges (bottom-left). Caller decides eligibility (e.g. via
    // access("/claude-token") on Linux or env checks on macOS).
    int has_claude_badge;
    int has_github_badge;

    // Title scale override. 0 = legacy auto (tries 3, falls back to 2 if
    // it overflows width — matches Linux hardware look). Any positive
    // value forces that scale for MatrixChunky8 — useful for big
    // marketing shots where the handle should dominate the frame.
    int title_scale;
} BootAnimConfig;

// Persistent per-column state for the matrix rain. Zero-initialize before
// the first render frame (boot_anim_render_frame auto-seeds on first use).
typedef struct {
    int          col_y[BOOT_ANIM_RAIN_MAX_COLS];
    int          col_speed[BOOT_ANIM_RAIN_MAX_COLS];
    unsigned int col_seed[BOOT_ANIM_RAIN_MAX_COLS];
    int          initialized;
} BootAnimState;

// Render frame `f` (0-indexed, must be < BOOT_ANIM_FRAMES) into `graph`/`screen`.
// State is mutable — caller keeps it across frames for rain animation.
void boot_anim_render_frame(ACGraph *graph, ACFramebuffer *screen,
                            int f, const BootAnimConfig *cfg,
                            BootAnimState *state);

#endif
