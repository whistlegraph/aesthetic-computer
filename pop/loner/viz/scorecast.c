// scorecast.c — the loner review-score video, ported to C.
//
// review-score4.py (and its -light twin) is the working PIL+numpy renderer:
// a scrolling clip-timeline review video, mini-DAW style — nine stem lanes
// with beat-quantized clips and in-clip waveforms sliding under a fixed
// center playhead, a section minimap, event markers, a karaoke word/mark
// readout, and in the corner the ORIGINAL loner whistlegraph retraced
// stroke-by-stroke: a red pen tip riding the recovered pen path, fresh ink
// drying from red to grey, one figure per sung pass with pale ghosts of the
// passes already sung. This file is that renderer again, in the lane's
// single-file C-native convention (see pop/loner/c/lonerremix.c) — the
// Python stays as the reference; this is the fast one.
//
// The division of labor with convert-data.py: C reads no npz, no JSON, no
// TrueType. That script runs ONCE and flattens wordclock.json, the two
// whistlegraph archives, and every string the video ever draws (rasterized
// by PIL with the same YWFT ttfs, sizes, and anchors as the Python — there
// is no stb_truetype.h on this machine and no network to fetch it, so the
// C gets the same pixels PIL would have drawn, as alpha atlases) into
// pop/loner/viz/wg.bin. Audio still goes through ffmpeg pipes exactly like
// the Python, EXCEPT the three band splits of other.wav (bells/pluck/pads):
// the Python asked ffmpeg for -af highpass/lowpass at the source rate; here
// they are RBJ biquads (Q = 1/sqrt2, ffmpeg's default 2-pole shape) applied
// after the 8 kHz downmix. Close enough for a clip gate and a waveform —
// the one place this port approximates rather than reproduces.
//
// Frame pipeline, same shape as the Python:
//   load data -> render the whole timeline strip ONCE into memory
//   (~28000 x 645 rgb) -> per frame: crop strip, paste chrome, brighten
//   active clips, playhead, minimap marker, corner card, karaoke, timecode
//   -> fwrite raw rgb24 into an ffmpeg popen (libx264 crf 18 + aac 256k,
//   -shortest, audio straight from the mp3).
//
// Build:  bash pop/loner/viz/build.sh
// Run:    pop/loner/viz/scorecast [--light] /tmp/scorecast-dark.mp4
//         (an output path is REQUIRED — the reference renders in
//         pop/loner/out/ are the Python's to overwrite, not ours)

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef uint8_t u8;
typedef struct { u8 r, g, b; } RGB;

#define SR 8000
#define W 1920
#define H 1080
#define FPS 30
#define PPS 272                       // timeline pixels per second

static const char *S =
    "/private/tmp/claude-501/-Users-jas-aesthetic-computer/"
    "df296e24-513a-488b-b96b-31cb958c1bda/scratchpad";
static const char *MP3 =
    "/Users/jas/aesthetic-computer/pop/loner/out/lonerclub-v4pid.mp3";
static const char *BIN =
    "/Users/jas/aesthetic-computer/pop/loner/viz/wg.bin";

#define BPM 122.0
#define BEAT (60.0 / BPM)
#define BAR (4 * BEAT)
#define EIGHTH (BEAT / 2)
#define GRID0 0.3654

// ── layout (verbatim from the Python) ──────────────────────────────────
#define GUT 150
#define SCROLL_W (W - GUT)            // 1770 px of scrolling timeline
#define PLAY_X (GUT + SCROLL_W / 2)   // 1035, fixed playhead
#define LANE_H 61
#define LANE_GAP 6
#define NLANE 9
#define LBL_BAND 20
#define RULER_H 28
#define STRIP_TOP 100
#define LANES_TOP (STRIP_TOP + LBL_BAND)
#define LANES_BOT (LANES_TOP + NLANE * LANE_H + (NLANE - 1) * LANE_GAP)
#define STRIP_H (LBL_BAND + (LANES_BOT - LANES_TOP) + RULER_H)   // 645
#define STRIP_BOT (STRIP_TOP + STRIP_H)

static double DUR;                    // seconds, from the mp3 decode
static long N;                        // mp3 sample count at 8 kHz
static int SWm, PAD_L, PAD_R, STRIP_W;

static int sx(double t) { return PAD_L + (int)lrint(t * PPS); }  // strip x
// lrint (round-half-even) matches Python's round() — the Python leaned on
// banker's rounding for every pixel snap, so we do too.

// ── the two palettes (diffed out of review-score4{,-light}.py) ─────────
typedef struct {
    RGB bg, lane_bg, grid_hv, grid_lt, tick_hv, tick_lt, num_hv, num_lt;
    double clip_fill_m; int clip_fill_a;    // clip body  = col*m + a
    double clip_line_m, wave_m;             // clip edge, waveform
    RGB seam, ev, title, mm_outline, gutter_line;
    double mm_fill_m; int mm_fill_a;        // minimap section body
    double mm_lbl_m, gut_lbl_m;             // minimap + gutter label tint
    int act_num, act_den;                   // active clip: v*num/den
    double act_line_m;                      // active outline tint
    RGB playhead, mm_marker, kara_word, kara_mark, tc;
    int card_outline; RGB card_outline_c;   // light frames the paper card
} Pal;

static const Pal DARK = {
    .bg = {12, 11, 14}, .lane_bg = {17, 16, 20},
    .grid_hv = {34, 33, 40}, .grid_lt = {24, 23, 28},
    .tick_hv = {130, 128, 134}, .tick_lt = {78, 76, 82},
    .num_hv = {150, 148, 152}, .num_lt = {104, 102, 108},
    .clip_fill_m = 0.22, .clip_fill_a = 0, .clip_line_m = 0.78, .wave_m = 0.60,
    .seam = {255, 80, 80}, .ev = {255, 210, 90}, .title = {240, 238, 232},
    .mm_outline = {58, 58, 64}, .gutter_line = {44, 44, 50},
    .mm_fill_m = 0.40, .mm_fill_a = 0, .mm_lbl_m = 1.0, .gut_lbl_m = 1.0,
    .act_num = 16, .act_den = 10, .act_line_m = 1.0,
    .playhead = {255, 245, 230}, .mm_marker = {245, 243, 238},
    .kara_word = {240, 238, 232}, .kara_mark = {150, 148, 145},
    .tc = {240, 238, 232}, .card_outline = 0, .card_outline_c = {0, 0, 0},
};
static const Pal LIGHT = {
    .bg = {246, 244, 240}, .lane_bg = {234, 231, 226},
    .grid_hv = {206, 203, 197}, .grid_lt = {222, 219, 213},
    .tick_hv = {105, 103, 99}, .tick_lt = {160, 157, 152},
    .num_hv = {88, 86, 83}, .num_lt = {150, 147, 142},
    .clip_fill_m = 0.25, .clip_fill_a = 191, .clip_line_m = 0.58, .wave_m = 0.60,
    .seam = {200, 40, 40}, .ev = {168, 118, 20}, .title = {28, 27, 25},
    .mm_outline = {150, 147, 142}, .gutter_line = {200, 197, 192},
    .mm_fill_m = 0.45, .mm_fill_a = 140, .mm_lbl_m = 0.52, .gut_lbl_m = 0.55,
    .act_num = 84, .act_den = 100, .act_line_m = 0.55,
    .playhead = {28, 27, 25}, .mm_marker = {28, 27, 25},
    .kara_word = {28, 27, 25}, .kara_mark = {110, 108, 104},
    .tc = {28, 27, 25}, .card_outline = 1, .card_outline_c = {182, 179, 174},
};
static const Pal *P;

static RGB tint(RGB c, double m, int a) {   // Python's tuple(int(v*m + a))
    int r = (int)(c.r * m + a), g = (int)(c.g * m + a), b = (int)(c.b * m + a);
    return (RGB){r > 255 ? 255 : r, g > 255 ? 255 : g, b > 255 ? 255 : b};
}

static const char *LANE_NAMES[NLANE] = {"lead vox", "drums", "bass", "bells",
    "pluck", "pads", "ahh arps", "wub sub", "stamp"};
static const RGB LANE_COL[NLANE] = {{110, 220, 205}, {235, 150, 80},
    {200, 120, 235}, {150, 190, 240}, {140, 150, 235}, {120, 140, 190},
    {255, 210, 90}, {235, 100, 60}, {200, 150, 160}};

#define NSECT 4
static const RGB SECT_COL[NSECT] = {{64, 190, 180}, {235, 120, 60},
    {150, 120, 200}, {150, 150, 155}};
static double SECT_T[NSECT + 1] = {0.0, 31.83, 63.30, 92.60, 0};  // [4]=DUR

#define NMARK 6
static const double MARK_T[NMARK] = {23.86, 27.06, 31.83, 55.44, 59.37, 93.60};
static const int MARK_SEAM = 2;       // index of "SEAM" in the atlas group

// ── canvas primitives (PIL semantics: rect coords are INCLUSIVE) ───────
typedef struct { u8 *px; int w, h; } Canvas;

static u8 *at(Canvas *c, int x, int y) { return c->px + (long)(y * (long)c->w + x) * 3; }

static void rect_fill(Canvas *c, int x0, int y0, int x1, int y1, RGB col) {
    if (x0 < 0) x0 = 0; if (y0 < 0) y0 = 0;
    if (x1 >= c->w) x1 = c->w - 1; if (y1 >= c->h) y1 = c->h - 1;
    for (int y = y0; y <= y1; y++) for (int x = x0; x <= x1; x++) {
        u8 *p = at(c, x, y); p[0] = col.r; p[1] = col.g; p[2] = col.b;
    }
}

static void rect_line(Canvas *c, int x0, int y0, int x1, int y1, RGB col, int w) {
    for (int i = 0; i < w; i++) {     // PIL strokes rectangle width inward
        rect_fill(c, x0 + i, y0 + i, x1 - i, y0 + i, col);
        rect_fill(c, x0 + i, y1 - i, x1 - i, y1 - i, col);
        rect_fill(c, x0 + i, y0 + i, x0 + i, y1 - i, col);
        rect_fill(c, x1 - i, y0 + i, x1 - i, y1 - i, col);
    }
}

static void vline(Canvas *c, int x, int y0, int y1, RGB col, int w) {
    rect_fill(c, x, y0, x + w - 1, y1, col);
}

// rounded rect, PIL-style: fill the rounded region, stroke `ow` px inward.
static int in_round(int x, int y, int x0, int y0, int x1, int y1, int r) {
    if (x < x0 || x > x1 || y < y0 || y > y1) return 0;
    int dx = x < x0 + r ? x0 + r - x : (x > x1 - r ? x - (x1 - r) : 0);
    int dy = y < y0 + r ? y0 + r - y : (y > y1 - r ? y - (y1 - r) : 0);
    return dx * dx + dy * dy <= r * r;
}
static void round_rect(Canvas *c, int x0, int y0, int x1, int y1, int r,
                       RGB fill, RGB line, int ow) {
    int ir = r - ow > 0 ? r - ow : 0;
    int cy0 = y0 < 0 ? 0 : y0, cy1 = y1 >= c->h ? c->h - 1 : y1;
    int cx0 = x0 < 0 ? 0 : x0, cx1 = x1 >= c->w ? c->w - 1 : x1;
    for (int y = cy0; y <= cy1; y++) for (int x = cx0; x <= cx1; x++) {
        if (!in_round(x, y, x0, y0, x1, y1, r)) continue;
        RGB col = in_round(x, y, x0 + ow, y0 + ow, x1 - ow, y1 - ow, ir)
                  ? fill : line;
        u8 *p = at(c, x, y); p[0] = col.r; p[1] = col.g; p[2] = col.b;
    }
}

// ── text: pre-rasterized PIL glyphs from wg.bin ────────────────────────
typedef struct { int w, h, dx, dy; float adv; u8 *a; } Glyph;

static void blit(Canvas *c, Glyph *g, int x, int y, RGB col) {
    x += g->dx; y += g->dy;
    for (int gy = 0; gy < g->h; gy++) {
        int fy = y + gy; if (fy < 0 || fy >= c->h) continue;
        for (int gx = 0; gx < g->w; gx++) {
            int fx = x + gx; if (fx < 0 || fx >= c->w) continue;
            int a = g->a[gy * g->w + gx]; if (!a) continue;
            u8 *p = at(c, fx, fy);
            p[0] = (u8)((p[0] * (255 - a) + col.r * a + 127) / 255);
            p[1] = (u8)((p[1] * (255 - a) + col.g * a + 127) / 255);
            p[2] = (u8)((p[2] * (255 - a) + col.b * a + 127) / 255);
        }
    }
}

// atlas groups, in convert-data.py's order
static Glyph *G_TITLE, *G_LANE, *G_SECT, *G_EV, *G_BAR, *G_TC, *G_WORD, *G_MARK;
static int NG_BAR, NG_TC, NG_WORD, NG_MARK;

// ── the whistlegraph sidecar ───────────────────────────────────────────
static int WGH, WGW, OM;
static u8 *INK8;                      // final ink alpha, (ink_a*255) as u8
static int32_t *ORDm, *PENC;          // per-pixel pen rank; pen path (y,x)
static double *PT12;                  // pen rank -> source frame (12 fps)
typedef struct { double t0, t1, v0, v1; int wi, mi; } Word;
static Word *WC; static int NWC;

static void need(size_t got, size_t want, const char *what) {
    if (got != want) { fprintf(stderr, "wg.bin: short read at %s\n", what); exit(1); }
}
static Glyph *read_group(FILE *f, int *count) {
    uint32_t n; need(fread(&n, 4, 1, f), 1, "group");
    Glyph *g = calloc(n, sizeof *g);
    for (uint32_t i = 0; i < n; i++) {
        struct { uint16_t w, h; int16_t dx, dy; float adv; } __attribute__((packed)) hd;
        need(fread(&hd, sizeof hd, 1, f), 1, "glyph");
        g[i] = (Glyph){hd.w, hd.h, hd.dx, hd.dy, hd.adv, malloc((size_t)hd.w * hd.h)};
        need(fread(g[i].a, 1, (size_t)hd.w * hd.h, f), (size_t)hd.w * hd.h, "bitmap");
    }
    *count = (int)n;
    return g;
}
static void load_bin(void) {
    FILE *f = fopen(BIN, "rb");
    if (!f) { fprintf(stderr, "missing %s — run convert-data.py first\n", BIN); exit(1); }
    char magic[4]; uint32_t v[4];
    need(fread(magic, 1, 4, f), 4, "magic");
    if (memcmp(magic, "WGSB", 4)) { fprintf(stderr, "bad wg.bin magic\n"); exit(1); }
    need(fread(v, 4, 4, f), 4, "header");
    WGH = v[1]; WGW = v[2]; OM = v[3];
    long npix = (long)WGH * WGW;
    INK8 = malloc(npix);
    ORDm = malloc(npix * 4);
    PENC = malloc((long)OM * 8);
    PT12 = malloc((long)OM * 8);
    need(fread(INK8, 1, npix, f), npix, "ink8");
    need(fread(ORDm, 4, npix, f), npix, "ordmap");
    need(fread(PENC, 4, (long)OM * 2, f), (long)OM * 2, "pcoords");
    need(fread(PT12, 8, OM, f), OM, "ptimes");
    uint32_t nw; need(fread(&nw, 4, 1, f), 1, "nwords");
    NWC = (int)nw;
    WC = calloc(NWC, sizeof *WC);
    for (int i = 0; i < NWC; i++) {
        double d[4]; uint16_t s[2];
        need(fread(d, 8, 4, f), 4, "wordclock");
        need(fread(s, 2, 2, f), 2, "wordclock");
        WC[i] = (Word){d[0], d[1], d[2], d[3], s[0], s[1]};
    }
    uint32_t ngr; need(fread(&ngr, 4, 1, f), 1, "ngroups");
    if (ngr != 8) { fprintf(stderr, "wg.bin: expected 8 atlas groups\n"); exit(1); }
    int n1;
    G_TITLE = read_group(f, &n1); G_LANE = read_group(f, &n1);
    G_SECT = read_group(f, &n1); G_EV = read_group(f, &n1);
    G_BAR = read_group(f, &NG_BAR); G_TC = read_group(f, &NG_TC);
    G_WORD = read_group(f, &NG_WORD); G_MARK = read_group(f, &NG_MARK);
    fclose(f);
}

// ── audio in: ffmpeg pipes, same args as the Python's load() ───────────
static float *ff_load(const char *path, int raw, long *out_n) {
    char cmd[1024];
    snprintf(cmd, sizeof cmd,
        "ffmpeg -v error %s-i '%s' -ac 1 -ar %d -f f32le -",
        raw ? "-f f32le -ar 48000 -ac 2 " : "", path, SR);
    FILE *p = popen(cmd, "r");
    if (!p) { fprintf(stderr, "popen failed: %s\n", cmd); exit(1); }
    size_t cap = 1 << 22, n = 0;
    float *buf = malloc(cap * 4);
    for (;;) {
        if (n == cap) { cap *= 2; buf = realloc(buf, cap * 4); }
        size_t got = fread(buf + n, 4, cap - n, p);
        if (!got) break;
        n += got;
    }
    if (pclose(p) != 0) { fprintf(stderr, "ffmpeg decode failed for %s\n", path); exit(1); }
    *out_n = (long)n;
    return buf;
}

static float *fit(float *sig, long n) {       // pad/trim to the mix length
    sig = realloc(sig, (size_t)N * 4);
    if (n < N) memset(sig + n, 0, (size_t)(N - n) * 4);
    return sig;
}

// RBJ biquads at 8 kHz stand in for the Python's ffmpeg -af band splits.
typedef struct { double b0, b1, b2, a1, a2; } Biq;
static Biq biq(int hp, double fc) {
    double w0 = 2 * M_PI * fc / SR, cw = cos(w0);
    double alpha = sin(w0) / (2 * M_SQRT1_2);  // Q = 1/sqrt2, ffmpeg default
    double a0 = 1 + alpha, k = hp ? (1 + cw) / 2 : (1 - cw) / 2;
    return (Biq){k / a0, (hp ? -(1 + cw) : 1 - cw) / a0, k / a0,
                 -2 * cw / a0, (1 - alpha) / a0};
}
static void filt(float *sig, Biq f) {
    double x1 = 0, x2 = 0, y1 = 0, y2 = 0;
    for (long i = 0; i < N; i++) {
        double x = sig[i];
        double y = f.b0 * x + f.b1 * x1 + f.b2 * x2 - f.a1 * y1 - f.a2 * y2;
        x2 = x1; x1 = x; y2 = y1; y1 = y;
        sig[i] = (float)y;
    }
}

// ── clip gate (straight port of clips_of) ──────────────────────────────
typedef struct { double t0, t1; } Span;
typedef struct { Span v[256]; int n; } Spans;

static double snap8(double t) {
    double s = GRID0 + nearbyint((t - GRID0) / EIGHTH) * EIGHTH;
    return s < 0 ? 0 : (s > DUR ? DUR : s);
}
static Spans clips_of(const float *sig) {
    Spans out = {.n = 0};
    int hop = (int)(0.05 * SR), nfr = (int)(N / hop);
    double *r = malloc(nfr * 8), rmax = 0;
    for (int i = 0; i < nfr; i++) {
        double s = 0;
        for (int j = 0; j < hop; j++) { double v = sig[(long)i * hop + j]; s += v * v; }
        r[i] = sqrt(s / hop);
        if (r[i] > rmax) rmax = r[i];
    }
    if (rmax <= 0) { free(r); return out; }
    u8 *act = malloc(nfr);
    for (int i = 0; i < nfr; i++) act[i] = r[i] > rmax * 0.11;
    free(r);
    for (int i = 0; i < nfr;) {                // bridge gaps under 3 hops
        if (!act[i]) {
            int j = i;
            while (j < nfr && !act[j]) j++;
            if (i > 0 && j < nfr && j - i < 3) memset(act + i, 1, j - i);
            i = j;
        } else i++;
    }
    Spans reg = {.n = 0};                      // runs >= 0.15 s
    for (int i = 0; i < nfr;) {
        if (act[i]) {
            int j = i;
            while (j < nfr && act[j]) j++;
            double t0 = i * 0.05, t1 = j * 0.05;
            if (t1 - t0 >= 0.15 && reg.n < 256) reg.v[reg.n++] = (Span){t0, t1};
            i = j;
        } else i++;
    }
    free(act);
    Spans mg = {.n = 0};                       // merge gaps < 0.18 s
    for (int i = 0; i < reg.n; i++) {
        if (mg.n && reg.v[i].t0 - mg.v[mg.n - 1].t1 < 0.18)
            mg.v[mg.n - 1].t1 = reg.v[i].t1;
        else mg.v[mg.n++] = reg.v[i];
    }
    Spans sn = {.n = 0};                       // snap to the 8th-note grid
    for (int i = 0; i < mg.n; i++) {
        double a = snap8(mg.v[i].t0), b = snap8(mg.v[i].t1);
        if (b - a < EIGHTH / 2) { b = a + EIGHTH; if (b > DUR) b = DUR; }
        if (sn.n && a <= sn.v[sn.n - 1].t1 + 1e-6) {
            if (b > sn.v[sn.n - 1].t1) sn.v[sn.n - 1].t1 = b;
        } else sn.v[sn.n++] = (Span){a, b};
    }
    for (int i = 0; i < sn.n; i++) {           // split at global 4-bar lines
        double t0 = sn.v[i].t0, t1 = sn.v[i].t1;
        if (t1 - t0 <= 4 * BAR + 0.05) { out.v[out.n++] = sn.v[i]; continue; }
        double cur = t0;
        long k = (long)ceil((t0 - GRID0) / (4 * BAR) - 1e-9);
        for (;;) {
            double b = GRID0 + k * 4 * BAR;
            k++;
            if (b <= cur + 0.3) continue;
            if (b >= t1 - 0.3) break;
            out.v[out.n++] = (Span){cur, b};
            cur = b;
        }
        out.v[out.n++] = (Span){cur, t1};
    }
    return out;
}

// ── word clock: pass / cut / karaoke (port of the Python trio) ─────────
static const double PASS_T0[3] = {0.3654, 31.83, 63.30};
static int pass_of(double t) {
    for (int k = 2; k >= 0; k--) if (t >= PASS_T0[k]) return k;
    return 0;
}
static int wc_before(double t) {      // last entry with t0 <= t, else -1
    int i = -1;
    while (i + 1 < NWC && WC[i + 1].t0 <= t) i++;
    return i;
}
static double video_cut(double t, int *pi) {
    *pi = pass_of(t);
    int i = wc_before(t);
    if (i < 0 || pass_of(WC[i].t0 + 0.001) != *pi) return 0.0;
    if (t < WC[i].t1) {
        double fr = (t - WC[i].t0) / (WC[i].t1 - WC[i].t0);
        return (WC[i].v0 + fr * (WC[i].v1 - WC[i].v0)) * 12.0;
    }
    return WC[i].v1 * 12.0;
}
static int word_now(double t) {       // wordclock index, or -1
    int i = wc_before(t);
    if (i >= 0 && t < WC[i].t1 && pass_of(WC[i].t0 + 0.001) == pass_of(t))
        return i;
    return -1;
}
static int rank_of(double cut) {      // searchsorted(PT12, cut, 'right')
    int lo = 0, hi = OM;
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        if (PT12[mid] <= cut) lo = mid + 1; else hi = mid;
    }
    return lo > OM ? OM : lo;
}

// ── corner card: PIL-Lanczos 2:1 downsample + paper/ink/fresh blend ────
#define CW 226
#define CH 349
static int CX_ = W - 10 - CW, CY_ = H - 10 - CH;
#define CARD_A 0.85
#define GHOST_A 0.16
static const double PAPER[3] = {246.0, 245.0, 241.0};
static const double INKC[3] = {42.0, 42.0, 47.0};
static const double FRESH[3] = {196.0, 44.0, 40.0};
static const double PEN[3] = {200.0, 36.0, 32.0};

// One axis of PIL's LANCZOS resample: 12-tap sinc window, normalized,
// rounded and clamped to u8 per pass — matching Image.resize on 'L'.
typedef struct { int xmin, n; double w[16]; } Tap;
static Tap *taps_for(int in, int out) {
    Tap *t = calloc(out, sizeof *t);
    for (int i = 0; i < out; i++) {
        double center = (i + 0.5) * 2.0, ww = 0;
        int xmin = (int)(center - 6.0 + 0.5); if (xmin < 0) xmin = 0;
        int xmax = (int)(center + 6.0 + 0.5); if (xmax > in) xmax = in;
        t[i].xmin = xmin; t[i].n = xmax - xmin;
        for (int x = 0; x < t[i].n; x++) {
            double u = (x + xmin - center + 0.5) * 0.5, v = 0;
            if (u > -3.0 && u < 3.0)
                v = u == 0 ? 1.0
                  : sin(M_PI * u) / (M_PI * u) * sin(M_PI * u / 3) / (M_PI * u / 3);
            t[i].w[x] = v; ww += v;
        }
        for (int x = 0; x < t[i].n; x++) t[i].w[x] /= ww;
    }
    return t;
}
static Tap *THX, *TVX;                 // 452->226 and 698->349, built once
static u8 *half_tmp;                   // WGH x CW intermediate
static void halve(const u8 *src, u8 *dst) {
    for (int y = 0; y < WGH; y++)
        for (int x = 0; x < CW; x++) {
            Tap *t = &THX[x]; double s = 0;
            const u8 *row = src + (long)y * WGW + t->xmin;
            for (int k = 0; k < t->n; k++) s += row[k] * t->w[k];
            long v = lround(s);
            half_tmp[(long)y * CW + x] = v < 0 ? 0 : v > 255 ? 255 : (u8)v;
        }
    for (int y = 0; y < CH; y++)
        for (int x = 0; x < CW; x++) {
            Tap *t = &TVX[y]; double s = 0;
            for (int k = 0; k < t->n; k++)
                s += half_tmp[(long)(t->xmin + k) * CW + x] * t->w[k];
            long v = lround(s);
            dst[(long)y * CW + x] = v < 0 ? 0 : v > 255 ? 255 : (u8)v;
        }
}

static u8 *card_a8, *card_f8, *card_af, *card_ff;   // full-res masks + halves
static double *ghost_f;                             // constant pale ghost

static void card_init(void) {
    THX = taps_for(WGW, CW); TVX = taps_for(WGH, CH);
    long npix = (long)WGH * WGW;
    half_tmp = malloc((long)WGH * CW);
    card_a8 = malloc(npix); card_f8 = malloc(npix);
    card_af = malloc((long)CH * CW); card_ff = malloc((long)CH * CW);
    ghost_f = malloc((long)CH * CW * 8);
    u8 *g8 = malloc((long)CH * CW);
    halve(INK8, g8);                   // the completed drawing, once
    for (long i = 0; i < (long)CH * CW; i++) ghost_f[i] = g8[i] / 255.0 * GHOST_A;
    free(g8);
}

// draw the card straight into the frame: paper, ghosts of earlier passes,
// revealed ink, fresh red ink, the pen tip, all under CARD_A over the bg.
static void card_paint(Canvas *fr, double t) {
    int pi, pip;
    double cutf = video_cut(t, &pi);
    double cutp = video_cut(t - 0.1 < 0 ? 0 : t - 0.1, &pip);
    int k = rank_of(cutf);
    long npix = (long)WGH * WGW;
    for (long i = 0; i < npix; i++) {
        int o = ORDm[i];
        int on = o >= 0 && o <= k;
        card_a8[i] = on ? INK8[i] : 0;
        card_f8[i] = (on && o > k - 150) ? INK8[i] : 0;
    }
    halve(card_a8, card_af);
    halve(card_f8, card_ff);
    // the pen rides the END of the path — only while the cut is advancing
    int pen = cutf - cutp > 0.02 && k < OM - 1;
    int py0 = 0, py1 = -1, px0 = 0, px1 = -1;
    if (pen) {
        int cy = PENC[2 * k] / 2, cx = PENC[2 * k + 1] / 2;
        py0 = cy - 3 < 0 ? 0 : cy - 3; py1 = cy + 4 > CH ? CH : cy + 4;
        px0 = cx - 3 < 0 ? 0 : cx - 3; px1 = cx + 4 > CW ? CW : cx + 4;
    }
    for (int y = 0; y < CH; y++) {
        u8 *p = at(fr, CX_, CY_ + y);
        for (int x = 0; x < CW; x++, p += 3) {
            long i = (long)y * CW + x;
            double a = card_af[i] / 255.0, f = card_ff[i] / 255.0;
            double c[3] = {PAPER[0], PAPER[1], PAPER[2]};
            if (pi > 0) {
                double g = ghost_f[i];
                for (int q = 0; q < 3; q++) c[q] = c[q] * (1 - g) + INKC[q] * g;
            }
            for (int q = 0; q < 3; q++) c[q] = c[q] * (1 - a) + INKC[q] * a;
            for (int q = 0; q < 3; q++) c[q] = c[q] * (1 - f) + FRESH[q] * f;
            if (pen && y >= py0 && y < py1 && x >= px0 && x < px1)
                { c[0] = PEN[0]; c[1] = PEN[1]; c[2] = PEN[2]; }
            for (int q = 0; q < 3; q++)
                p[q] = (u8)(p[q] * (1.0 - CARD_A) + c[q] * CARD_A);
        }
    }
    if (P->card_outline)
        rect_line(fr, CX_, CY_, CX_ + CW - 1, CY_ + CH - 1, P->card_outline_c, 1);
}

// ── strip + chrome ─────────────────────────────────────────────────────
static Canvas STRIP, CHROME;
static Spans LCLIPS[NLANE];
static float *lane_sig[NLANE];

static void draw_strip(void) {
    STRIP = (Canvas){malloc((long)STRIP_W * STRIP_H * 3), STRIP_W, STRIP_H};
    rect_fill(&STRIP, 0, 0, STRIP_W - 1, STRIP_H - 1, P->bg);
    int x0m = sx(0), x1m = sx(DUR), ry = STRIP_H - RULER_H;
    for (int li = 0; li < NLANE; li++) {
        int y0 = LBL_BAND + li * (LANE_H + LANE_GAP);
        rect_fill(&STRIP, x0m, y0, x1m, y0 + LANE_H - 1, P->lane_bg);
    }
    for (int k = 0;; k++) {            // bar grid + ruler ticks + numbers
        double bt = GRID0 + k * BAR;
        if (bt >= DUR) break;
        int x = sx(bt), heavy = k % 4 == 0;
        vline(&STRIP, x, LBL_BAND, ry - 1, heavy ? P->grid_hv : P->grid_lt, 1);
        vline(&STRIP, x, ry, ry + (heavy ? 12 : 8),
              heavy ? P->tick_hv : P->tick_lt, heavy ? 2 : 1);
        if (k < NG_BAR)
            blit(&STRIP, &G_BAR[k], x + 4, ry + 8, heavy ? P->num_hv : P->num_lt);
    }
    long per = N / SWm;                // samples per strip column
    double *e = malloc((size_t)SWm * 8);
    for (int li = 0; li < NLANE; li++) {
        const float *sig = lane_sig[li];
        int y0 = LBL_BAND + li * (LANE_H + LANE_GAP), mid = y0 + LANE_H / 2;
        double emax = 0;
        for (int cIdx = 0; cIdx < SWm; cIdx++) {
            double s = 0;
            for (long j = 0; j < per; j++) { double v = sig[cIdx * per + j]; s += v * v; }
            e[cIdx] = sqrt(s / per);
            if (e[cIdx] > emax) emax = e[cIdx];
        }
        if (emax > 0) for (int cIdx = 0; cIdx < SWm; cIdx++)
            e[cIdx] = pow(e[cIdx] / emax, 0.8);
        RGB fill = tint(LANE_COL[li], P->clip_fill_m, P->clip_fill_a);
        RGB line = tint(LANE_COL[li], P->clip_line_m, 0);
        RGB wave = tint(LANE_COL[li], P->wave_m, 0);
        LCLIPS[li] = clips_of(sig);
        for (int ci = 0; ci < LCLIPS[li].n; ci++) {
            int xa = sx(LCLIPS[li].v[ci].t0), xb = sx(LCLIPS[li].v[ci].t1);
            round_rect(&STRIP, xa, y0 + 3, xb, y0 + LANE_H - 3, 5, fill, line, 2);
            int amax = LANE_H / 2 - 6;
            for (int x = xa + 2; x < xb - 1; x++) {
                int cIdx = x - PAD_L;
                if (cIdx < 0 || cIdx >= SWm) continue;
                int h = (int)(e[cIdx] * amax);
                if (h > 0) vline(&STRIP, x, mid - h, mid + h, wave, 1);
            }
        }
        printf("  lane %s  %d clips\n", LANE_NAMES[li], LCLIPS[li].n);
        if (getenv("SCORECAST_CLIPS"))     // clip-gate parity check vs Python
            for (int ci = 0; ci < LCLIPS[li].n; ci++)
                printf("    %.2f-%.2f\n", LCLIPS[li].v[ci].t0, LCLIPS[li].v[ci].t1);
        fflush(stdout);
    }
    free(e);
    for (int m = 0; m < NMARK; m++) {  // event markers ride the strip
        int x = sx(MARK_T[m]);
        RGB col = m == MARK_SEAM ? P->seam : P->ev;
        vline(&STRIP, x, 2, ry - 1, col, 2);
        blit(&STRIP, &G_EV[m], x + 5, 1, col);
    }
}

#define MM_X0 40
#define MM_X1 1880
#define MM_Y0 62
#define MM_Y1 88

static void draw_chrome(void) {
    CHROME = (Canvas){malloc((long)W * H * 3), W, H};
    rect_fill(&CHROME, 0, 0, W - 1, H - 1, P->bg);
    blit(&CHROME, &G_TITLE[0], 40, 18, P->title);
    for (int s = 0; s < NSECT; s++) {
        int xa = MM_X0 + (int)(SECT_T[s] / DUR * (MM_X1 - MM_X0));
        int xb = MM_X0 + (int)(SECT_T[s + 1] / DUR * (MM_X1 - MM_X0));
        rect_fill(&CHROME, xa, MM_Y0, xb, MM_Y1,
                  tint(SECT_COL[s], P->mm_fill_m, P->mm_fill_a));
        blit(&CHROME, &G_SECT[s], xa + 6, MM_Y0 + 5,
             tint(SECT_COL[s], P->mm_lbl_m, 0));
    }
    rect_line(&CHROME, MM_X0, MM_Y0, MM_X1, MM_Y1, P->mm_outline, 1);
    for (int li = 0; li < NLANE; li++) {   // gutter stubs + lane labels
        int y0 = LANES_TOP + li * (LANE_H + LANE_GAP);
        rect_fill(&CHROME, 10, y0, GUT - 6, y0 + LANE_H - 1, P->lane_bg);
        blit(&CHROME, &G_LANE[li], 18, y0 + LANE_H / 2,
             tint(LANE_COL[li], P->gut_lbl_m, 0));
    }
    vline(&CHROME, GUT - 2, STRIP_TOP, STRIP_BOT, P->gutter_line, 1);
}

// ── main ───────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    P = &DARK;
    const char *out = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--light")) P = &LIGHT;
        else out = argv[i];
    }
    if (!out) {
        fprintf(stderr, "usage: scorecast [--light] /path/out.mp4\n");
        return 1;
    }
    struct timespec T0; clock_gettime(CLOCK_MONOTONIC, &T0);
    load_bin();
    if (WGW / 2 != CW || WGH / 2 != CH) {
        fprintf(stderr, "wg.bin working res %dx%d != 2x card %dx%d\n",
                WGW, WGH, CW, CH);
        return 1;
    }

    long n;
    float *mix = ff_load(MP3, 0, &n);  // the mix sets the clock; then gone
    free(mix);
    N = n;
    DUR = (double)N / SR;
    SECT_T[NSECT] = DUR;
    SWm = (int)ceil(DUR * PPS);
    PAD_L = PLAY_X - GUT; PAD_R = W - PLAY_X;
    STRIP_W = SWm + PAD_L + PAD_R;
    printf("mix %.2fs, strip %dx%d\ndecoding stems...\n", DUR, STRIP_W, STRIP_H);
    fflush(stdout);

    char pth[512];
    snprintf(pth, sizeof pth, "%s/vocalsFX.wav", S);
    lane_sig[0] = fit(ff_load(pth, 0, &n), n);
    snprintf(pth, sizeof pth, "%s/drums-cool.raw", S);
    lane_sig[1] = fit(ff_load(pth, 1, &n), n);
    snprintf(pth, sizeof pth, "%s/sep4/htdemucs/v4pid-trim/bass.wav", S);
    lane_sig[2] = fit(ff_load(pth, 0, &n), n);
    snprintf(pth, sizeof pth, "%s/sep4/htdemucs/v4pid-trim/other.wav", S);
    float *other = fit(ff_load(pth, 0, &n), n);
    for (int b = 0; b < 3; b++) {      // bells / pluck / pads band splits
        float *sig = malloc((size_t)N * 4);
        memcpy(sig, other, (size_t)N * 4);
        if (b == 0) filt(sig, biq(1, 2800));
        if (b == 1) { filt(sig, biq(1, 700)); filt(sig, biq(0, 2800)); }
        if (b == 2) filt(sig, biq(0, 700));
        lane_sig[3 + b] = sig;
    }
    free(other);
    snprintf(pth, sizeof pth, "%s/stem-flourish.raw", S);
    lane_sig[6] = fit(ff_load(pth, 1, &n), n);
    snprintf(pth, sizeof pth, "%s/stem-wub.raw", S);
    lane_sig[7] = fit(ff_load(pth, 1, &n), n);
    snprintf(pth, sizeof pth, "%s/stem-stamp.raw", S);
    lane_sig[8] = fit(ff_load(pth, 1, &n), n);

    printf("rendering timeline strip...\n"); fflush(stdout);
    draw_strip();
    for (int li = 0; li < NLANE; li++) free(lane_sig[li]);
    draw_chrome();
    card_init();

    int NF = (int)(DUR * FPS);
    printf("encoding %d frames...\n", NF); fflush(stdout);
    char cmd[1024];
    snprintf(cmd, sizeof cmd,
        "ffmpeg -y -v error -f rawvideo -pix_fmt rgb24 -s %dx%d -r %d -i - "
        "-i '%s' -map 0:v -map 1:a -c:v libx264 -preset veryfast -crf 18 "
        "-pix_fmt yuv420p -c:a aac -b:a 256k -shortest '%s'", W, H, FPS, MP3, out);
    FILE *ff = popen(cmd, "w");
    if (!ff) { fprintf(stderr, "encoder popen failed\n"); return 1; }

    Canvas fr = {malloc((long)W * H * 3), W, H};
    struct timespec TE; clock_gettime(CLOCK_MONOTONIC, &TE);
    for (int f = 0; f < NF; f++) {
        double t = (double)f / FPS;
        memcpy(fr.px, CHROME.px, (long)W * H * 3);
        int off = (int)lrint(t * PPS);
        for (int y = 0; y < STRIP_H; y++)      // crop the strip into place
            memcpy(at(&fr, GUT, STRIP_TOP + y), at(&STRIP, off, y),
                   (long)SCROLL_W * 3);
        int act_xa[NLANE], act_xb[NLANE], nact = 0, act_li[NLANE];
        for (int li = 0; li < NLANE; li++)     // active clips light up
            for (int ci = 0; ci < LCLIPS[li].n; ci++) {
                double t0 = LCLIPS[li].v[ci].t0, t1 = LCLIPS[li].v[ci].t1;
                if (!(t0 <= t && t < t1)) continue;
                int xa = GUT + sx(t0) - off, xb = GUT + sx(t1) - off;
                if (xa < GUT) xa = GUT;
                if (xb > W) xb = W;
                if (xb > xa) {
                    int y0f = LANES_TOP + li * (LANE_H + LANE_GAP);
                    for (int y = y0f; y < y0f + LANE_H; y++) {
                        u8 *p = at(&fr, xa, y);
                        for (int x = xa; x < xb; x++)
                            for (int q = 0; q < 3; q++, p++) {
                                int v = *p * P->act_num / P->act_den;
                                *p = v > 255 ? 255 : (u8)v;
                            }
                    }
                    act_xa[nact] = xa; act_xb[nact] = xb; act_li[nact] = li;
                    nact++;
                }
                break;
            }
        rect_fill(&fr, PLAY_X - 1, STRIP_TOP, PLAY_X, STRIP_BOT - 1, P->playhead);
        int mx = MM_X0 + (int)(t / DUR * (MM_X1 - MM_X0));
        rect_fill(&fr, mx, MM_Y0, mx + 1, MM_Y1, P->mm_marker);
        card_paint(&fr, t);
        for (int i = 0; i < nact; i++) {       // outlines over the glow
            int y0f = LANES_TOP + act_li[i] * (LANE_H + LANE_GAP);
            rect_line(&fr, act_xa[i], y0f + 1, act_xb[i] - 1, y0f + LANE_H - 2,
                      tint(LANE_COL[act_li[i]], P->act_line_m, 0), 3);
        }
        int wi = word_now(t);
        if (wi >= 0) {
            blit(&fr, &G_WORD[WC[wi].wi], 60, CY_ + 66, P->kara_word);
            blit(&fr, &G_MARK[WC[wi].mi], 64, CY_ + 170, P->kara_mark);
        }
        char tc[16];                           // m:ss.d, right-anchored
        snprintf(tc, sizeof tc, "%d:%02d.%d", (int)t / 60, (int)t % 60,
                 (int)fmod(t * 10.0, 10.0));
        double tw = 0;
        for (char *c = tc; *c; c++)
            tw += G_TC[*c == ':' ? 10 : *c == '.' ? 11 : *c - '0'].adv;
        double xc = (W - 40) - tw;
        for (char *c = tc; *c; c++) {
            Glyph *g = &G_TC[*c == ':' ? 10 : *c == '.' ? 11 : *c - '0'];
            blit(&fr, g, (int)lrint(xc), 14, P->tc);
            xc += g->adv;
        }
        fwrite(fr.px, 1, (long)W * H * 3, ff);
        if (f % 300 == 0) {
            struct timespec TN; clock_gettime(CLOCK_MONOTONIC, &TN);
            printf("  frame %d/%d  (%.0fs)\n", f, NF,
                   TN.tv_sec - TE.tv_sec + (TN.tv_nsec - TE.tv_nsec) / 1e9);
            fflush(stdout);
        }
    }
    pclose(ff);
    struct timespec T1; clock_gettime(CLOCK_MONOTONIC, &T1);
    printf("done in %.1fs -> %s\n",
           T1.tv_sec - T0.tv_sec + (T1.tv_nsec - T0.tv_nsec) / 1e9, out);
    return 0;
}
