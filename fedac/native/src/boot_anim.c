// boot_anim.c — extracted render loop from ac-native.c:draw_startup_fade.
// Stateless per-frame: caller owns the framebuffer, the key-drain, the
// display present, and the rain state. See boot_anim.h for the contract.
#include "boot_anim.h"
#include "font.h"

#include <math.h>
#include <stdio.h>
#include <string.h>

static uint8_t clamp_u8(int v) {
    if (v < 0) return 0;
    if (v > 255) return 255;
    return (uint8_t)v;
}

// Rainbow fallback for characters not covered by a custom palette.
// HSV → RGB with fixed saturation/value and hue rotating with frame.
static ACColor rainbow_title_color(int ci, int frame, int alpha) {
    double hue = fmod((double)ci / 7.0 * 360.0 + frame * 2.0, 360.0);
    double h6 = hue / 60.0;
    int    hi = (int)h6 % 6;
    double fr = h6 - (int)h6;
    double sv = 0.7, vv = 1.0;
    double p  = vv * (1.0 - sv);
    double q  = vv * (1.0 - sv * fr);
    double tt = vv * (1.0 - sv * (1.0 - fr));
    double cr = 0, cg = 0, cb = 0;
    switch (hi) {
        case 0: cr = vv; cg = tt; cb = p;  break;
        case 1: cr = q;  cg = vv; cb = p;  break;
        case 2: cr = p;  cg = vv; cb = tt; break;
        case 3: cr = p;  cg = q;  cb = vv; break;
        case 4: cr = tt; cg = p;  cb = vv; break;
        default: cr = vv; cg = p; cb = q;  break;
    }
    return (ACColor){(uint8_t)(cr * 255), (uint8_t)(cg * 255), (uint8_t)(cb * 255),
                     clamp_u8(alpha)};
}

// Per-character palette lookup. If the config has a color list, apply it
// to chars after the first '@' (the handle); otherwise fall back to
// rainbow. Palette colors get a small sinusoidal pulse for liveness and
// are lifted toward white so dark boot backgrounds don't wash them out.
static ACColor title_char_color(int ci, int frame, int alpha,
                                const char *title,
                                const ACColor *pal, int pal_len) {
    if (pal_len <= 0 || !pal) return rainbow_title_color(ci, frame, alpha);
    const char *at = title ? strchr(title, '@') : NULL;
    int handle_start = at ? (int)(at - title) + 1 : 0;
    if (ci < handle_start) return rainbow_title_color(ci, frame, alpha);
    int idx = ci - handle_start;
    if (idx < 0) idx = 0;
    idx %= pal_len;
    ACColor c = pal[idx];
    int pulse = (int)(18.0 * sin((double)(frame + ci * 6) * 0.08));
    int r = (c.r * 7 + 255 * 3) / 10 + pulse;
    int g = (c.g * 7 + 255 * 3) / 10 + pulse;
    int b = (c.b * 7 + 255 * 3) / 10 + pulse;
    return (ACColor){clamp_u8(r), clamp_u8(g), clamp_u8(b), clamp_u8(alpha)};
}

static void seed_rain(BootAnimState *st) {
    // Keep deterministic-ish by xorshift32-ing a fixed seed rather than
    // time(NULL) — screenshot runs should be reproducible.
    unsigned int s = 0xC1A9E5A3u;
    for (int c = 0; c < BOOT_ANIM_RAIN_MAX_COLS; c++) {
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        st->col_y[c] = -(int)(s % 400);
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        st->col_speed[c] = 2 + (int)(s % 4);
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        st->col_seed[c] = s;
    }
    st->initialized = 1;
}

void boot_anim_render_frame(ACGraph *graph, ACFramebuffer *screen,
                            int f, const BootAnimConfig *cfg,
                            BootAnimState *state) {
    if (!state->initialized) seed_rain(state);

    const int total_frames = BOOT_ANIM_FRAMES;
    double t = (double)f / (double)total_frames;

    // ── Background fade (first 0.3 s) ────────────────────────────────
    double fade_t = t * 3.33;
    if (fade_t > 1.0) fade_t = 1.0;
    int hour   = cfg->hour;
    int is_day = (hour >= 7 && hour < 18);
    int target_r, target_g, target_b;
    if (is_day) {
        if (hour >= 7 && hour < 12) {
            target_r = 235; target_g = 230; target_b = 220;  // morning cream
        } else {
            target_r = 240; target_g = 235; target_b = 215;  // afternoon warm white
        }
    } else {
        if (hour >= 5 && hour < 7) {
            target_r = 100; target_g = 45;  target_b = 20;   // sunrise orange
        } else if (hour >= 18 && hour < 20) {
            target_r = 80;  target_g = 25;  target_b = 60;   // sunset purple
        } else {
            target_r = 15;  target_g = 15;  target_b = 40;   // night deep blue
        }
    }
    int start_r = is_day ? 255 : 0;
    int start_g = is_day ? 255 : 0;
    int start_b = is_day ? 255 : 0;
    int bg_r = start_r + (int)((target_r - start_r) * fade_t);
    int bg_g = start_g + (int)((target_g - start_g) * fade_t);
    int bg_b = start_b + (int)((target_b - start_b) * fade_t);
    graph_wipe(graph, (ACColor){(uint8_t)bg_r, (uint8_t)bg_g, (uint8_t)bg_b, 255});

    // ── Matrix rain BG ───────────────────────────────────────────────
    {
        int col_w = 10;
        int glyph_w = 7, glyph_h = 9;
        int nc = screen->width / col_w;
        if (nc > BOOT_ANIM_RAIN_MAX_COLS) nc = BOOT_ANIM_RAIN_MAX_COLS;
        int rain_alpha_scale = is_day ? 80 : 180;
        for (int c = 0; c < nc; c++) {
            state->col_y[c] += state->col_speed[c];
            if (state->col_y[c] > screen->height + glyph_h * 12) {
                unsigned int s = state->col_seed[c];
                s ^= s << 13; s ^= s >> 17; s ^= s << 5;
                state->col_seed[c] = s;
                state->col_y[c]    = -(int)(s % 300);
                state->col_speed[c] = 2 + (int)(s % 4);
            }
            int cx = c * col_w + 1;
            for (int trail = 0; trail < 10; trail++) {
                int gy = state->col_y[c] - trail * glyph_h;
                if (gy < -glyph_h || gy >= screen->height) continue;
                int tb = (trail == 0) ? 220 : 180 - trail * 18;
                if (tb < 15) continue;
                int b = (tb * rain_alpha_scale) >> 8;
                ACColor cg = (trail == 0)
                    ? (ACColor){ (uint8_t)(b * 0.8), 255, (uint8_t)(b * 0.8), (uint8_t)b }
                    : (ACColor){ 40, (uint8_t)(b * 0.9 + 20), (uint8_t)(b * 0.4), (uint8_t)b };
                graph_ink(graph, cg);
                unsigned int gseed = state->col_seed[c]
                    ^ ((unsigned)trail * 2654435761u)
                    ^ ((unsigned)(f / 4) * 2246822519u);
                for (int row = 0; row < glyph_h; row++) {
                    for (int col = 0; col < glyph_w; col++) {
                        gseed ^= gseed << 13; gseed ^= gseed >> 17; gseed ^= gseed << 5;
                        if (gseed & 1) graph_plot(graph, cx + col, gy + row);
                    }
                }
            }
        }
    }

    // ── Title (handle greeting) ──────────────────────────────────────
    int alpha = (int)(255.0 * fade_t);
    // MatrixChunky8 glyph height is ~8 px at scale 1, hence `* 8`.
    int title_scale = 0, title_y_bottom = screen->height / 2 - 20 + 8;
    if (alpha > 0 && cfg->title) {
        const char *title = cfg->title;
        int scale;
        if (cfg->title_scale > 0) {
            scale = cfg->title_scale;
        } else {
            // Legacy auto: prefer 3, drop to 2 if the title overflows.
            // Matches the on-hardware Linux boot aesthetic.
            scale = 3;
            if (font_measure_matrix(title, scale) > screen->width - 20) scale = 2;
        }
        int tw = font_measure_matrix(title, scale);
        int tx = (screen->width - tw) / 2;
        // Large titles push up so the subtitle can sit below without
        // colliding. (scale−3)·4 is the legacy anchor offset; bigger
        // scales also shift the baseline higher by a proportional amount.
        int ty = screen->height / 2 - 20 - (scale - 3) * 4;
        title_scale = scale;
        title_y_bottom = ty + 8 * scale;
        for (int ci = 0; title[ci]; ci++) {
            ACColor cc = title_char_color(ci, f, alpha, title,
                                          cfg->title_colors,
                                          cfg->title_colors_len);
            graph_ink(graph, cc);
            char ch[2] = { title[ci], 0 };
            tx = font_draw_matrix(graph, ch, tx, ty, scale);
        }
    }

    // ── Version panel (top-right) ────────────────────────────────────
    if (alpha > 40 && cfg->git_hash) {
        char ver[64]; char bts[64]; char bname[64] = ""; char ddrv[64] = "";
        snprintf(ver, sizeof(ver), "version %s", cfg->git_hash);
        snprintf(bts, sizeof(bts), "%s", cfg->build_ts ? cfg->build_ts : "build unknown");
        if (cfg->build_name)   snprintf(bname, sizeof(bname), "%s", cfg->build_name);
        if (cfg->driver_name)  snprintf(ddrv,  sizeof(ddrv),  "display %s", cfg->driver_name);

        int wv = font_measure_matrix(ver, 1);
        int wt = font_measure_matrix(bts, 1);
        int wn = bname[0] ? font_measure_matrix(bname, 1) : 0;
        int wd = ddrv[0]  ? font_measure_matrix(ddrv,  1) : 0;
        int max_w = wv;
        if (wt > max_w) max_w = wt;
        if (wn > max_w) max_w = wn;
        if (wd > max_w) max_w = wd;
        int panel_w = max_w + 8;
        int panel_h = (bname[0] ? 28 : 20) + 8;
        int panel_x = screen->width - panel_w - 3;
        int panel_y = 3;
        graph_ink(graph, is_day
            ? (ACColor){255, 255, 255, (uint8_t)(alpha * 0.7)}
            : (ACColor){0,   0,   0,   (uint8_t)(alpha * 0.82)});
        graph_box(graph, panel_x, panel_y, panel_w, panel_h, 1);
        if (bname[0]) {
            graph_ink(graph, is_day
                ? (ACColor){140, 100, 0,   (uint8_t)alpha}
                : (ACColor){255, 200, 60,  (uint8_t)alpha});
            font_draw_matrix(graph, bname, panel_x + 4, panel_y + 3, 1);
        }
        int line_y = panel_y + (bname[0] ? 11 : 3);
        graph_ink(graph, is_day
            ? (ACColor){60,  60,  60,  (uint8_t)alpha}
            : (ACColor){255, 255, 255, (uint8_t)alpha});
        font_draw_matrix(graph, ver, panel_x + 4, line_y, 1);
        graph_ink(graph, is_day
            ? (ACColor){80,  100, 90,  (uint8_t)alpha}
            : (ACColor){210, 235, 220, (uint8_t)alpha});
        font_draw_matrix(graph, bts, panel_x + 4, line_y + 8, 1);
        if (ddrv[0]) {
            graph_ink(graph, is_day
                ? (ACColor){90,  60,  120, (uint8_t)alpha}
                : (ACColor){180, 160, 255, (uint8_t)alpha});
            font_draw_matrix(graph, ddrv, panel_x + 4, line_y + 16, 1);
        }
        if (cfg->is_new_version) {
            graph_ink(graph, is_day
                ? (ACColor){0,  140, 60,  (uint8_t)alpha}
                : (ACColor){80, 255, 120, (uint8_t)alpha});
            font_draw_matrix(graph, "FRESH",
                             panel_x - font_measure_matrix("FRESH", 1) - 4,
                             panel_y + 6, 1);
        }
    }

    // ── Subtitle "enjoy <city>!" (fades in after frame 80) ───────────
    if (f > 80 && cfg->city && cfg->city[0]) {
        double sub_t = (double)(f - 80) / 20.0;
        if (sub_t > 1.0) sub_t = 1.0;
        int sub_alpha = (int)(180.0 * sub_t);
        graph_ink(graph, is_day
            ? (ACColor){120, 100, 80,  (uint8_t)sub_alpha}
            : (ACColor){220, 180, 140, (uint8_t)sub_alpha});
        // Scale subtitle with the title so proportions stay balanced —
        // big title + tiny subtitle looks wrong at screenshot resolutions.
        int sub_scale = title_scale >= 6 ? 2 : 1;
        char subtitle[128];
        snprintf(subtitle, sizeof subtitle, "enjoy %s!", cfg->city);
        int sw = font_measure_matrix(subtitle, sub_scale);
        // Anchor below the title baseline so enlarged titles can't collide.
        int sy = title_y_bottom + 4;
        if (sy < screen->height / 2 + 10) sy = screen->height / 2 + 10;
        font_draw_matrix(graph, subtitle,
                         (screen->width - sw) / 2, sy, sub_scale);
    }

    // ── Auth badges (bottom-left) ────────────────────────────────────
    if (f > 40 && alpha > 80) {
        int badge_x = 6;
        int badge_y = screen->height - 22;
        double badge_t = (double)(f - 40) / 30.0;
        if (badge_t > 1.0) badge_t = 1.0;
        int ba = (int)(220.0 * badge_t);

        if (cfg->has_claude_badge) {
            static const char crab[9][12] = {
                " .       . ",
                "  .     .  ",
                " ..##.##.. ",
                ".# #### #.",
                ". ####### .",
                "  #######  ",
                "  ## . ##  ",
                "  .     .  ",
                " .       . ",
            };
            for (int cy = 0; cy < 9; cy++)
                for (int cx = 0; cx < 11; cx++) {
                    char c = crab[cy][cx];
                    if (c == '#')
                        graph_ink(graph, (ACColor){255, 120, 50, (uint8_t)ba});
                    else if (c == '.')
                        graph_ink(graph, (ACColor){200, 90, 30, (uint8_t)(ba * 2 / 3)});
                    else continue;
                    graph_box(graph, badge_x + cx * 2, badge_y + cy * 2, 2, 2, 1);
                }
            badge_x += 28;
        }
        if (cfg->has_github_badge) {
            static const char octo[11][12] = {
                "   .###.   ",
                "  #######  ",
                " ## o#o ## ",
                " ######### ",
                " ## ### ## ",
                "  #######  ",
                "   #####   ",
                "  .# . #.  ",
                " .#  .  #. ",
                " .   .   . ",
                ".    .    .",
            };
            for (int cy = 0; cy < 11; cy++)
                for (int cx = 0; cx < 11; cx++) {
                    char c = octo[cy][cx];
                    if (c == '#')
                        graph_ink(graph, (ACColor){180, 210, 255, (uint8_t)ba});
                    else if (c == 'o')
                        graph_ink(graph, (ACColor){60, 80, 120, (uint8_t)ba});
                    else if (c == '.')
                        graph_ink(graph, (ACColor){120, 150, 200, (uint8_t)(ba * 2 / 3)});
                    else continue;
                    graph_box(graph, badge_x + cx * 2, badge_y + cy * 2, 2, 2, 1);
                }
            badge_x += 28;
        }
    }

    // ── Drifting triangles (atmospheric decor) ───────────────────────
    if (alpha > 30) {
        int tri_alpha = (int)(alpha * 0.15);
        int W = screen->width;
        int H = screen->height;
        for (int ti = 0; ti < 6; ti++) {
            double phase = (double)f * 0.02 + ti * 1.047;
            int cx = (int)(W * 0.5 + W * 0.35 * sin(phase + 1.5708));
            int cy = (int)(H * 0.5 + H * 0.3  * sin(phase * 0.7));
            int sz = 8 + ti * 3 + (int)(4.0 * sin(f * 0.05 + ti));
            ACColor tc = is_day
                ? (ACColor){180 - ti*15, 140 - ti*10, 120,        (uint8_t)tri_alpha}
                : (ACColor){80  + ti*20, 60  + ti*15, 120 + ti*10, (uint8_t)tri_alpha};
            graph_ink(graph, tc);
            int x0 = cx,      y0 = cy - sz;
            int x1 = cx - sz, y1 = cy + sz / 2;
            int x2 = cx + sz, y2 = cy + sz / 2;
            graph_line(graph, x0, y0, x1, y1);
            graph_line(graph, x1, y1, x2, y2);
            graph_line(graph, x2, y2, x0, y0);
        }
    }

    // ── Shrinking time bar at the bottom ─────────────────────────────
    int bar_full = screen->width - 40;
    int bar_remaining = (int)((1.0 - t) * bar_full);
    if (bar_remaining > 0) {
        graph_ink(graph, (ACColor){200, 150, 180, (uint8_t)(80 * (1.0 - t))});
        graph_box(graph, 20, screen->height - 6, bar_remaining, 3, 1);
    }

    // ── Install hint pill (when running from USB) ────────────────────
    if (alpha > 100 && cfg->show_install) {
        double pulse = 0.5 + 0.5 * sin(f * 0.1);
        int pa = (int)(40 + 30 * pulse);
        const char *hint = cfg->is_installed ? "W: update" : "W: install to disk";
        int hw = font_measure_matrix(hint, 1);
        int hx = (screen->width - hw) / 2;
        int hy = screen->height - 20;
        graph_ink(graph, is_day
            ? (ACColor){200, 160, 120, (uint8_t)pa}
            : (ACColor){60,  40,  80,  (uint8_t)pa});
        graph_box(graph, hx - 4, hy - 2, hw + 8, 12, 1);
        int ax = hx - 10;
        int ay = hy + 3;
        graph_ink(graph, is_day
            ? (ACColor){180, 120, 60,  (uint8_t)(alpha / 2)}
            : (ACColor){200, 150, 255, (uint8_t)(alpha / 2)});
        graph_line(graph, ax, ay - 3, ax, ay + 3);
        graph_line(graph, ax, ay + 3, ax - 3, ay);
        graph_ink(graph, is_day
            ? (ACColor){120, 60,  0,   (uint8_t)(alpha * 2 / 3)}
            : (ACColor){220, 180, 255, (uint8_t)(alpha * 2 / 3)});
        font_draw_matrix(graph, hint, hx, hy, 1);
    }
}
