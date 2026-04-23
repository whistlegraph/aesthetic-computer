// shutdown_anim.c — canonical ac-native farewell animation.
//
// 90 frames, red/white chaotic strobe, jittered "bye @handle" title +
// subtitle, fades to black. Math mirrors draw_shutdown_anim() in
// ac-native.c so the Linux hardware path and the macOS host produce
// visually identical farewells.
#include "shutdown_anim.h"

#include <stdint.h>
#include <string.h>
#include "graph.h"
#include "font.h"

void shutdown_anim_render_frame(ACGraph *graph, ACFramebuffer *screen,
                                int f, const ShutdownAnimConfig *cfg) {
    if (!graph || !screen) return;
    if (f < 0) f = 0;
    if (f >= SHUTDOWN_ANIM_FRAMES) f = SHUTDOWN_ANIM_FRAMES - 1;

    const char *title = (cfg && cfg->title && cfg->title[0]) ? cfg->title : "bye";

    // Final frame is pure black so the halt / exit lands on something clean.
    if (f == SHUTDOWN_ANIM_FRAMES - 1) {
        graph_wipe(graph, (ACColor){0, 0, 0, 255});
        return;
    }

    double t = (double)f / (double)SHUTDOWN_ANIM_FRAMES;

    // Chaotic strobe — pseudo-random 6-phase cycle.
    int phase = (f * 7 + f / 3) % 6;
    uint8_t br, bg, bb;
    if (phase < 2)      { br = 220; bg = 20;  bb = 20;  } // red
    else if (phase < 3) { br = 255; bg = 255; bb = 255; } // white
    else if (phase < 5) { br = 180; bg = 0;   bb = 0;   } // dark red
    else                { br = 10;  bg = 10;  bb = 10;  } // near black

    // Fade toward end.
    double fade = 1.0 - t * t;
    br = (uint8_t)(br * fade);
    bg = (uint8_t)(bg * fade);
    bb = (uint8_t)(bb * fade);
    graph_wipe(graph, (ACColor){br, bg, bb, 255});

    // Title + subtitle — hidden for the last ~15% of the sequence so the
    // fade to black reads cleanly.
    if (t < 0.85) {
        int alpha = (int)(255.0 * (1.0 - t / 0.85));
        int jx = (f * 13 % 7) - 3;   // -3..+3 jitter
        int jy = (f * 17 % 5) - 2;   // -2..+2
        // Every 3rd frame: full-white flicker. Otherwise: red-biased.
        uint8_t tr = (f % 3 == 0) ? 255 : 200;
        uint8_t tg = (f % 3 == 0) ? 255 : 40;
        uint8_t tb = (f % 3 == 0) ? 255 : 40;
        graph_ink(graph, (ACColor){tr, tg, tb, (uint8_t)alpha});
        int tw = font_measure_matrix(title, 3);
        font_draw_matrix(graph, title,
                         (screen->width - tw) / 2 + jx,
                         screen->height / 2 - 20 + jy, 3);

        graph_ink(graph, (ACColor){(uint8_t)(120 * fade), 40, 40,
                                    (uint8_t)(alpha / 2)});
        int sw = font_measure_matrix("aesthetic.computer", 1);
        font_draw_matrix(graph, "aesthetic.computer",
                         (screen->width - sw) / 2 + jx / 2,
                         screen->height / 2 + 10 + jy / 2, 1);
    }
}
