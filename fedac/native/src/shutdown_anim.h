// shutdown_anim.h — host-independent renderer for the ac-native shutdown
// farewell. Paired with boot_anim.h: same aesthetic family, different
// mood. 90 frames (1.5 s @ 60 fps) of a chaotic red/white strobe with a
// jittered "bye @handle" title and an "aesthetic.computer" subtitle,
// ending on a black frame.
//
// Shared by the Linux PID-1 init path (src/ac-native.c, via
// draw_shutdown_anim()) and the macOS host (macos/main.c, polling the
// g_poweroff_requested flag). Caller owns frame timing + display present.
#ifndef AC_SHUTDOWN_ANIM_H
#define AC_SHUTDOWN_ANIM_H

#include "graph.h"

#define SHUTDOWN_ANIM_FRAMES 90

typedef struct {
    // e.g. "bye @jeffrey" — drawn large + jittered. If NULL, renderer
    // falls back to plain "bye".
    const char *title;
} ShutdownAnimConfig;

// Render frame `f` (0..SHUTDOWN_ANIM_FRAMES-1) into `graph`/`screen`.
// The final frame (f == SHUTDOWN_ANIM_FRAMES - 1) is pure black so the
// process can exit cleanly on top of it.
void shutdown_anim_render_frame(ACGraph *graph, ACFramebuffer *screen,
                                int f, const ShutdownAnimConfig *cfg);

#endif
