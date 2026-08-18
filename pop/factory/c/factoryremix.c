// factoryremix.c — "whistlegraph factory --- remix (v3, regulated)", in C.
//
// v1 stamped raw slices on principle; v2 pressed every vocal through the
// aesthetivox. Both placed each line as one die-block at the take's own
// internal word offsets, IN SECONDS. v3 puts the words on the beat — and
// that is the version of this record the title has been asking for since
// the beginning, because a cookie cutter does not approximate. Same shape,
// every time, to the frame.
//
// The deal with the Python side: bin/chart.py owns everything WORLD — it
// bakes the regulated vocal bank (snap 0.90 onto D natural minor in HER
// OWN 148.73 Hz frame, per-word frame warp onto the 100 BPM chart, octave
// halos, full-word low-3rd/5th backup) into vox3/, and emits
// c/factory-chart.h: per word unit, its beat slot and its measured
// semitone. This engine reads that header, so THE MACHINE'S MELODY IS THE
// CHANT'S MELODY, whistle flips and all — factory ends on D4, "from"
// drops to Bb2, and the bird lands on Bb2 outside everything.
//
//   THE PRESS  a die coming down: 120→55 Hz thud + three inharmonic
//              partials in free-bar ratios (1 : 2.76 : 5.40 — struck
//              metal, not a chord) + a 2 ms noise chiff. Fast decay; a
//              press strikes, it does not ring. Carried from v1.
//   THE BELT   sixteen ticks a bar panning left to right, one belt-length
//              per bar. It is the timekeeper, not the backbeat.
//   THE HUM    D1 + partials with a 0.4 Hz flutter and an 8.3 Hz rotation
//              roughness; SHUTDOWN bends the whole stack down a minor 3rd.
//   THE PUMP   every press ducks the vox bus and the bed; the belt and the
//              press never duck. The cult bus law survives the genre.
//   SPACE      the dotted-8th dub delay (damped cross-feedback at 0.38)
//              and a decorrelated Schroeder pair on the vox bus.
//
// Form, 64 bars at 100 (2:34 + tail) — the poem is an 8-bar cell, and the
// record is that cell stamped eight times with the tolerances drifting:
//
//   POWER-ON     0:00   0–8    hum, relays finding the grid, the die seats
//   THE STAMP    0:19   8–24   the cell twice, IDENTICAL. That is the point.
//   FULL SPEC    0:57  24–32   halos, backup 3rds and 5ths, pluck doubling
//   OUT OF SPEC  1:16  32–40   copy k is k steps out: 9 ¢ and 9 ms, belt
//                              drops steps, the die bounces
//   BREAK FREE   1:36  40–48   press out. The words alone on the floor.
//   THE ESCAPE   1:55  48–56   everything, and the bird gets the last word
//   SHUTDOWN     2:14  56–64   the press decelerates, the hum winds down
//
// Mixing rules from cult v3 / factory v1–v2 survive: 10 ms raised-cosine
// tails, ramped ducks, no master tanh, one linear trim.
//
// Build:  bash pop/factory/c/build.sh
// Run:    pop/factory/c/factoryremix     # → pop/factory/out/factory-remix-v3-full.wav
//         MINIMAL=1 pop/factory/c/factoryremix   # → out/factory-kickvox-full.wav
//         (from the repo root; cut-v3.sh masters it to mp3)

#include <dirent.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "factory-chart.h"

#define SR 48000
#define TAU 6.283185307179586
static const double BPM = CHART_BPM;      // 100 — the chant's own tempo
static const double TONIC = CHART_TONIC;  // 148.73 Hz — HER D
#define BARS 64
#define TAIL_S 6.0
static double BEAT, BAR, STEP;            // set in main
static long N;
#define LANE "pop/factory"
#define POEM "f-whole-poem"

// ── buses ──────────────────────────────────────────────────────────────
static float *drumsL, *drumsR, *musicL, *musicR, *voxL, *voxR;
static float *sideV, *sideB, *dlySend, *rvbSend;
static const double VOXG = 1.42;

// ── helpers ────────────────────────────────────────────────────────────
static double clampd(double v, double a, double b) { return v < a ? a : v > b ? b : v; }
static double smoothstep(double u) { return u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u); }
static double tail_fade(long i, long n) {
    double u = (double)(n - 1 - i) / (0.010 * SR);
    return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
}
static double at(double bar) { return bar * BAR; }
static double hz_of(double st) { return TONIC * pow(2.0, st / 12.0); }

static uint32_t seed = 20260818u;
static double rnd(void) { seed = seed * 1664525u + 1013904223u; return seed / 4294967296.0; }
static double jit(double ms) { return ((rnd() - 0.5) * 2 * ms) / 1000.0; }
static uint32_t nseed = 20210204u;        // the work's own date
static double nrnd(void) {
    nseed ^= nseed << 13; nseed ^= nseed >> 17; nseed ^= nseed << 5;
    return (nseed / 4294967296.0) * 2 - 1;
}

typedef struct { long itd; double gl, gr; } Spatial;
static Spatial spatial(double az) {
    Spatial s;
    s.itd = lround(0.00027 * SR * sin(az));
    double shadow = 0.35 * sin(az);
    s.gl = 1 - shadow; s.gr = 1 + shadow;
    return s;
}
static void emit(int bus, long i, double mono, double pan, const Spatial *sp,
                 double sideAmt, double dly, double rvb) {
    if (i < 0 || i >= N) return;
    double a = (M_PI / 4) * (1 + pan), cl = cos(a), cr = sin(a);
    if (bus == 0) { drumsL[i] += mono * cl; drumsR[i] += mono * cr; }
    else if (bus == 2) { voxL[i] += mono * cl; voxR[i] += mono * cr; }
    else { musicL[i] += mono * cl; musicR[i] += mono * cr; }
    if (dly) dlySend[i] += mono * dly;
    if (rvb) rvbSend[i] += mono * rvb;
    if (sp && sideAmt) {
        long li = i + sp->itd, ri = i - sp->itd;
        double l = (li >= 0 && li < N) ? mono * sp->gl : 0;
        double r = (ri >= 0 && ri < N) ? mono * sp->gr : 0;
        double s = 0.5 * (l - r) * sideAmt;
        if (bus == 2) sideV[i] += s; else sideB[i] += s;
    }
}
#define BUS_DRUMS 0
#define BUS_MUSIC 1
#define BUS_VOX 2

// ── WAV IO (the fleet loader) ──────────────────────────────────────────
static float *load_wav_mono(const char *path, long *out_n, int *out_sr) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t *)malloc(sz);
    if (!buf || (long)fread(buf, 1, sz, f) != sz) { fclose(f); free(buf); return NULL; }
    fclose(f);
    if (sz < 12 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) { free(buf); return NULL; }
    long p = 12; int fmt = 1, channels = 1, bits = 16; uint32_t rate = SR;
    long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        uint32_t s = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8 | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(buf + p, "fmt ", 4)) {
            fmt = buf[p+8] | (buf[p+9] << 8); channels = buf[p+10] | (buf[p+11] << 8);
            rate = (uint32_t)buf[p+12] | (uint32_t)buf[p+13] << 8 | (uint32_t)buf[p+14] << 16 | (uint32_t)buf[p+15] << 24;
            bits = buf[p+22] | (buf[p+23] << 8);
        } else if (!memcmp(buf + p, "data", 4)) { dOff = p + 8; dLen = s; break; }
        p += 8 + s + (s & 1);
    }
    if (!channels || !bits || !dOff) { free(buf); return NULL; }
    const int bps = bits / 8, fb = bps * channels;
    const long frames = dLen / fb;
    float *mono = (float *)malloc(frames * sizeof(float));
    for (long i = 0; i < frames; i++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
            const long o = dOff + (i * channels + c) * bps;
            if (fmt == 3 && bits == 32) { float v; memcpy(&v, buf + o, 4); acc += v; }
            else if (bits == 32) { int32_t v; memcpy(&v, buf + o, 4); acc += (double)v / 2147483648.0; }
            else if (bits == 24) { int32_t v = buf[o] | (buf[o+1] << 8) | ((int32_t)(int8_t)buf[o+2] << 16); acc += (double)v / 8388608.0; }
            else { int16_t v; memcpy(&v, buf + o, 2); acc += (double)v / 32768.0; }
        }
        mono[i] = (float)(acc / channels);
    }
    free(buf);
    *out_n = frames; *out_sr = (int)rate;
    return mono;
}
static void write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "! cannot write %s\n", path); exit(1); }
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    float *inter = (float *)malloc(n * 2 * sizeof(float));
    for (long i = 0; i < n; i++) { inter[i*2] = L[i]; inter[i*2+1] = R[i]; }
    fwrite(inter, sizeof(float), n * 2, f);
    free(inter); fclose(f);
}

// ── sample bank — vox3/ (the chart bank) ───────────────────────────────
#define MAX_BANK 512
typedef struct { char name[64]; float *s; long n; } Sample;
static Sample BANK[MAX_BANK];
static int bankN = 0;
static Sample *bank_get(const char *name) {
    for (int i = 0; i < bankN; i++) if (!strcmp(BANK[i].name, name)) return &BANK[i];
    return NULL;
}
static void bank_load(const char *name, const char *path) {
    long n = 0; int sr = 0;
    float *raw = load_wav_mono(path, &n, &sr);
    if (!raw) { fprintf(stderr, "  ! missing sample %s\n", path); return; }
    // NO LEADING-SILENCE TRIM on this bank. The other lanes' loader hunts
    // for the first sample above 0.008 and starts there, which is right
    // for a one-shot but catastrophic here: chart.py's lead-in is measured
    // in frames from sample zero, and shaving the quiet head of the /f/
    // would slide the whole poem off the grid by however much it shaved.
    double peak = 0;
    for (long i = 0; i < n; i++) { double v = fabs((double)raw[i]); if (v > peak) peak = v; }
    double g = peak > 1e-6 ? 0.95 / peak : 1.0;
    for (long i = 0; i < n; i++) raw[i] = (float)((double)raw[i] * g);
    Sample *slot = bank_get(name);
    if (!slot) {
        if (bankN >= MAX_BANK) { fprintf(stderr, "! bank full\n"); exit(1); }
        slot = &BANK[bankN++];
        snprintf(slot->name, sizeof slot->name, "%s", name);
    } else free(slot->s);
    slot->s = raw; slot->n = n;
}
static void bank_load_dir(const char *rel) {
    char dirp[512]; snprintf(dirp, sizeof dirp, "%s/%s", LANE, rel);
    DIR *d = opendir(dirp);
    if (!d) { fprintf(stderr, "! cannot open %s\n", dirp); exit(1); }
    struct dirent *e;
    while ((e = readdir(d))) {
        size_t len = strlen(e->d_name);
        if (len < 5 || strcmp(e->d_name + len - 4, ".wav") || e->d_name[0] == '.') continue;
        char name[64]; snprintf(name, sizeof name, "%.*s", (int)(len - 4), e->d_name);
        char path[1024]; snprintf(path, sizeof path, "%s/%s", dirp, e->d_name);
        bank_load(name, path);
    }
    closedir(d);
}
static int missingN = 0;

// ── the one-shot player ────────────────────────────────────────────────
typedef struct {
    double gain, pan, side, dark, dur, dly, rvb, off, attack, rate, det;
    int bus, rev;
} Shot;
static Shot shot_defaults(void) {
    Shot o = { 1, 0, 0.35, 0, 0, 0, 0, 0, 0.0015, 1.0, 0, BUS_VOX, 0 };
    return o;
}
static void shot(const char *name, double t, const Shot *o) {
    Sample *s = bank_get(name);
    if (!s) { fprintf(stderr, "  ! missing %s\n", name); missingN++; return; }
    double rate = (o->rate > 0 ? o->rate : 1.0) * pow(2.0, o->det / 1200.0);
    long start = lround(o->off * SR);
    if (start < 0) start = 0;
    if (start > s->n - 2) start = s->n - 2;
    long avail = o->rev ? (start ? start : s->n - 2) : (s->n - 2 - start);
    long n = o->dur > 0 ? (long)fmin(avail / rate, o->dur * SR) : (long)(avail / rate);
    if (n <= 4) return;
    long i0 = lround(t * SR);
    Spatial sp = spatial(o->pan * 1.2);
    double lp = 0;
    double pos = o->rev ? (start ? start : s->n - 2) : start;
    for (long i = 0; i < n; i++) {
        long q = (long)pos;
        if (q + 1 >= s->n || q < 0) break;
        double f = pos - q;
        double v = s->s[q] + (s->s[q + 1] - s->s[q]) * f;
        if (o->dark > 0) { lp += (1 - o->dark) * (v - lp); v = lp; }
        double env = smoothstep((i / (double)SR) / o->attack);
        emit(o->bus, i0 + i, v * env * o->gain * tail_fade(i, n),
             o->pan, &sp, o->side, o->dly, o->rvb);
        pos += o->rev ? -rate : rate;
    }
}

// lead + halo + optional backup, all locked to the same chart warp
static void sung(const char *name, double t, double gain, double pan, double dly,
                 double det) {
    Shot o = shot_defaults();
    o.gain = gain; o.pan = pan; o.side = 0.5; o.dly = dly; o.rvb = 0.28; o.det = det;
    shot(name, t, &o);
}
static void halo(const char *name, double t, double g) {
    char nm[96];
    Shot o = shot_defaults();
    o.side = 0.9; o.rvb = 0.62; o.attack = 0.35; o.bus = BUS_VOX;
    snprintf(nm, sizeof nm, "%s-8ve-a", name);
    o.gain = g; o.pan = -0.55; o.dly = 0.20; shot(nm, t + 0.028, &o);
    snprintf(nm, sizeof nm, "%s-8ve-b", name);
    o.gain = g * 0.92; o.pan = 0.55; o.dly = 0.24; shot(nm, t + 0.041, &o);
}
static void backup(const char *name, double t, double g, int five) {
    char nm[96];
    Shot o = shot_defaults();
    o.side = 0.7; o.rvb = 0.45; o.attack = 0.06; o.dark = 0.25;
    snprintf(nm, sizeof nm, "%s-low3", name);
    o.gain = g; o.pan = -0.3; o.dly = 0.12; shot(nm, t, &o);
    if (five) {
        snprintf(nm, sizeof nm, "%s-low5", name);
        o.gain = g * 0.8; o.pan = 0.3; o.dly = 0.14; shot(nm, t, &o);
    }
}
// the chart placer: subtracts the phrase's consonant lead-in so word 0
// lands ON the beat
static const ChartPhrase *phrase(const char *name) {
    for (int i = 0; i < CHART_N; i++)
        if (!strcmp(CHART[i].name, name)) return &CHART[i];
    fprintf(stderr, "  ! no chart phrase %s\n", name); exit(1);
}
static void voice_line(const char *name, double bar, double gain,
                       double haloG, double backupG, int five, double det) {
    const ChartPhrase *p = phrase(name);
    double t = at(bar) - p->leadIn;
    sung(name, t, gain, 0, 0.13, det);
    if (haloG > 0) halo(name, t, haloG);
    if (backupG > 0) backup(name, t, backupG, five);
}

// ── the press — a die coming down (v1's, ported) ───────────────────────
#define MAX_PRESS 4096
static double pressT[MAX_PRESS]; static int pressN = 0;
static const double PSAT_D = 0.999329299739067;   // tanh(3.8)
static void press(double t, double gain) {
    if (pressN < MAX_PRESS) pressT[pressN++] = t;
    long n = lround(0.34 * SR), i0 = lround(t * SR);
    double ph = 0, p1 = 0, p2 = 0, p3 = 0, knk = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double f = 55 + 65 * exp(-u * 62);           // 120 → 55, fast
        ph += (TAU * f) / SR;
        // free-bar ratios: struck metal, not a chord
        p1 += (TAU * 186) / SR; p2 += (TAU * 186 * 2.76) / SR; p3 += (TAU * 186 * 5.40) / SR;
        knk += (TAU * 300) / SR;
        double env = (0.6 * exp(-u * 30) + 0.5 * exp(-u * 9)) * fmin(1, u / 0.0008);
        double body = tanh(sin(ph) * env * 3.8) / PSAT_D;
        double bar_ = (sin(p1) * 0.5 + sin(p2) * 0.24 + sin(p3) * 0.10) * exp(-u * 46);
        double knock = sin(knk) * exp(-u * 95) * 0.30;
        double chiff = (u < 0.002 ? nrnd() : 0) * 0.5;
        emit(BUS_DRUMS, i0 + i,
             (body + bar_ * 0.55 + knock + chiff) * 0.86 * gain * tail_fade(i, n),
             0, NULL, 0, 0, 0);
    }
}
// clap — the thin backbeat v2 asked for: one per bar, never a wall
static void clap(double t, double gain, double pan) {
    long n = lround(0.22 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * 3000) / SR), k2 = 1 - exp((-TAU * 1100) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        double env = 0;
        for (int b = 0; b < 3; b++) {
            double dt = u - b * 0.011;
            if (dt > 0) env += exp(-dt * (b == 2 ? 26 : 90)) * (b == 2 ? 1.0 : 0.6);
        }
        double s = tanh((bp - bp2) * 2.1 * env);
        emit(BUS_DRUMS, i0 + i, s * 0.42 * gain * fmin(1, u / 0.001) * tail_fade(i, n),
             pan, &sp, 0.35, 0, 0.06);
    }
}
// tick — one step of the conveyor belt
static void tick(double t, double gain, double pan) {
    long n = lround(0.030 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * 9500) / SR), k2 = 1 - exp((-TAU * 6000) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        emit(BUS_DRUMS, i0 + i, (bp - bp2) * exp(-u * 90) * 1.6 * gain * tail_fade(i, n),
             pan, &sp, 0.4, 0, 0);
    }
}
// airhat — the offbeat, so the ear can find the 8ths
static void airhat(double t, double gain, double pan) {
    long n = lround(0.14 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * 10500) / SR), k2 = 1 - exp((-TAU * 7200) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        emit(BUS_DRUMS, i0 + i, (bp - bp2) * exp(-u * 24) * 1.1 * gain * tail_fade(i, n),
             pan, &sp, 0.45, 0, 0.03);
    }
}
static void relay(double t, double gain, double pan) {
    long n = lround(0.018 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double bp = 0;
    double k = 1 - exp((-TAU * 2600) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        bp += k * (nrnd() - bp);
        emit(BUS_DRUMS, i0 + i, bp * exp(-u * 260) * 2.2 * gain * tail_fade(i, n),
             pan, &sp, 0.5, 0, 0.04);
    }
}
static void riser(double t, double dur, double gain) {
    long n = lround(dur * SR), i0 = lround(t * SR);
    double bp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)n;
        double kf = 1 - exp((-TAU * (600 + 8000 * u * u)) / SR);
        bp += kf * (nrnd() - bp);
        emit(BUS_DRUMS, i0 + i, bp * u * u * gain * tail_fade(i, n),
             (u - 0.5) * 0.4, NULL, 0, 0, 0.10);
    }
}

// ── the bed ────────────────────────────────────────────────────────────
// the hum — the motor. D1 + partials, 0.4 Hz flutter, 8.3 Hz rotation.
static void hum(double t, double dur, double gain, double bend) {
    long n = lround(dur * SR), i0 = lround(t * SR);
    double ph[4] = { 0, 0, 0, 0 };
    static const double mult[4] = { 1.0, 2.0, 3.0, 5.0 };
    static const double amp[4] = { 1.0, 0.34, 0.16, 0.06 };
    double lp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR, wall = (i0 + i) / (double)SR;
        double flut = 1 + 0.006 * sin(TAU * 0.4 * wall);
        double rot = 1 + 0.08 * sin(TAU * 8.3 * wall);
        double glide = pow(2.0, bend * (u / dur) / 12.0);
        double s = 0;
        for (int v = 0; v < 4; v++) {
            ph[v] += (TAU * hz_of(-24) * mult[v] * flut * glide) / SR;
            s += sin(ph[v]) * amp[v];
        }
        lp += 0.30 * (s * rot - lp);
        double env = smoothstep(u / 1.2) * (u > dur - 1.2 ? fmax(0, (dur - u) / 1.2) : 1);
        emit(BUS_MUSIC, i0 + i, lp * 0.22 * env * gain * tail_fade(i, n),
             0, NULL, 0, 0, 0);
    }
}
static void bass(double t, double st, double dur, double gain, double attack) {
    long n = lround((dur + 0.16) * SR), i0 = lround(t * SR);
    double f = hz_of(st);
    double p1 = 0, p2 = 0, p3 = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
        double env = smoothstep(u / attack);
        if (u > dur) env *= fmax(0, 1 - (u - dur) / 0.16);
        double s = sin(p1) + 0.48 * sin(p2) + 0.07 * sin(p3);
        lp += 0.42 * (s - lp);
        emit(BUS_MUSIC, i0 + i, lp * 0.38 * env * gain * tail_fade(i, n), 0, NULL, 0, 0, 0);
    }
}
static void pad(double t, const double *sts, int nst, double dur, double gain,
                double attack, double pan, double side, double dly, double dark) {
    long n = lround((dur + 0.9) * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double ph[6][2], drift[6];
    for (int v = 0; v < nst; v++) {
        ph[v][0] = fmod(t * 7 + v * 2.39, 1.0) * TAU;
        ph[v][1] = fmod(t * 3 + v * 1.17, 1.0) * TAU;
        drift[v] = 0.055 + 0.02 * v;
    }
    double lp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR, wall = (i0 + i) / (double)SR;
        double wow = 0.0023 * sin(TAU * 0.38 * wall) + 0.0011 * sin(TAU * 0.11 * wall + 1.7);
        double s = 0;
        for (int v = 0; v < nst; v++) {
            double f = hz_of(sts[v]) * (1 + wow + 0.0007 * sin(TAU * drift[v] * wall + v * 2.1));
            ph[v][0] += (TAU * f) / SR;
            ph[v][1] += (TAU * f * 1.0028) / SR;
            s += sin(ph[v][0]) + 0.62 * sin(ph[v][1]);
        }
        s /= nst * 1.62;
        lp += (1 - dark) * 0.32 * (s - lp);
        double env = smoothstep(u / attack);
        if (u > dur) env *= fmax(0, 1 - (u - dur) / 0.9);
        emit(BUS_MUSIC, i0 + i, lp * env * gain * tail_fade(i, n), pan, &sp, side, dly, 0);
    }
}
// pluck — the machine's voice, playing only chart notes
static void pluck(double t, double st, double dur, double gain, double pan) {
    double ring = fmin(dur * 1.15 + 0.25, 1.6);
    long n = lround(ring * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double f = hz_of(st);
    double p1 = 0, p2 = 0, p3 = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        p1 += (TAU * f) / SR;
        p2 += (TAU * f * 1.003) / SR;
        p3 += (TAU * f * 2.7) / SR;
        double env = fmin(1, u / 0.006) * exp(-u * (3.4 / ring));
        double s = (sin(p1) + 0.85 * sin(p2)) * 0.5 + 0.22 * sin(p3) * exp(-u * 11);
        emit(BUS_MUSIC, i0 + i, s * 0.30 * env * gain * tail_fade(i, n),
             pan, &sp, 0.55, 0.22, 0);
    }
}
static void pluck_line(const char *name, double bar, double gain, int oct, double pan) {
    const ChartPhrase *p = phrase(name);
    for (int i = 0; i < p->n; i++) {
        const ChartNote *nn = &p->notes[i];
        pluck(at(bar) + nn->beat * BEAT + jit(2), nn->st + oct,
              nn->dur * BEAT, gain * (1 - 0.015 * i), pan * ((i % 2) ? -1 : 1));
    }
}

// ── tape ───────────────────────────────────────────────────────────────
static void hiss_bed(void) {
    double lp = 0, hp = 0, prev = 0, lvl = 0.010;
    double kLp = 1 - exp((-TAU * 5200) / SR);
    double hpRc = 1 / (TAU * 320), hpA = hpRc / (hpRc + 1.0 / SR);
    for (long i = 0; i < N; i++) {
        double bar = i / (double)SR / BAR;
        double target = bar < 32 ? 0.0040 : bar < 48 ? 0.0090 : 0.011;
        lvl += 0.000004 * (target - lvl);
        double w = nrnd();
        lp += kLp * (w - lp);
        hp = hpA * (hp + lp - prev); prev = lp;
        musicL[i] += hp * lvl; musicR[i] += hp * lvl * 0.94;
    }
}

// ── harmony — the walk the poem's arc asks for ─────────────────────────
typedef struct { double root; double tones[3]; } Chord;
static const Chord CH_i   = { 0,  { 0, 3, 7 } };
static const Chord CH_III = { 3,  { 3, 7, 10 } };
static const Chord CH_VI  = { -4, { -4, 0, 3 } };
static const Chord CH_VII = { -2, { -2, 2, 5 } };
static const Chord *ROW_SPEC[8] = { &CH_i, &CH_i, &CH_i, &CH_i, &CH_VI, &CH_VI, &CH_VII, &CH_VII };
static const Chord *ROW_TURN[8] = { &CH_i, &CH_VI, &CH_III, &CH_VII, &CH_i, &CH_VI, &CH_VII, &CH_VII };
static const Chord *chord_at(int bar) {
    return (bar >= 40 && bar < 56) ? ROW_TURN[bar % 8] : ROW_SPEC[bar % 8];
}
// THE PRESS RUNS except through BREAK FREE and the last bars of SHUTDOWN
static int press_on(int bar) { return !(bar >= 40 && bar < 48) && bar >= 6 && bar < 60; }

// ── one bar of the machine ─────────────────────────────────────────────
static const double VEL[16] = { 1, 0.42, 0.66, 0.46, 0.86, 0.42, 0.68, 0.46,
                                0.95, 0.42, 0.66, 0.46, 0.82, 0.46, 0.72, 0.5 };
static void floor_bar(int bar, double pressG, double beltG, int claps, int drops) {
    double t = at(bar);
    for (int b = 0; b < 4; b++) press(t + b * BEAT, pressG * (b == 0 ? 1.0 : 0.93));
    if (claps) clap(t + 2 * BEAT, 0.8, 0.06);
    for (int b = 0; b < 4; b++) airhat(t + (b + 0.5) * BEAT, 0.15, (b % 2) ? 0.3 : -0.3);
    // the belt crosses the field left to right, one belt-length per bar
    for (int s = 0; s < 16; s++) {
        if (drops && (s % 5) == 3) continue;         // OUT OF SPEC drops steps
        tick(t + s * STEP + (drops ? jit(9) : jit(1.5)),
             beltG * VEL[s], -0.85 + 1.7 * (s / 15.0));
    }
}

// ── sidechain — every press ducks the words ────────────────────────────
static float *duck_env(double depth, double atk, double rel) {
    float *e = (float *)malloc(N * sizeof(float));
    for (long i = 0; i < N; i++) e[i] = 1;
    long pre = lround(0.010 * SR);
    for (int k = 0; k < pressN; k++) {
        long i0 = lround(pressT[k] * SR) - pre;
        long span = pre + lround((atk + rel) * SR);
        for (long i = 0; i < span; i++) {
            long j = i0 + i;
            if (j < 0) continue;
            if (j >= N) break;
            double dt = i / (double)SR - 0.010;
            double g;
            if (dt < atk) g = 1 - depth * clampd((dt + 0.010) / (0.010 + atk), 0, 1);
            else { double u = clampd((dt - atk) / rel, 0, 1); g = (1 - depth) + depth * smoothstep(u); }
            if (g < e[j]) e[j] = (float)g;
        }
    }
    return e;
}

// ═══ main ══════════════════════════════════════════════════════════════
static int minimal_bars(void) {
    // The poem is 31.5 beats — 7 bars and 3.5 beats — so it ends on the
    // bar-7 half. Round UP to the bar line and stop: two extra bars was
    // seven seconds of watching an empty roll after the bird had already
    // finished ringing, and the study is for scrutinising, not sitting.
    int b = (int)ceil(phrase(POEM)->beats / 4.0);
    return b < 16 ? b : 16;
}

int main(void) {
    // MINIMAL=1 → the study pass: the press and the words, nothing else.
    // "lets start with just kick and vocals and get that right."
    int minimal = getenv("MINIMAL") != NULL;
    BEAT = 60.0 / BPM; BAR = 4 * BEAT; STEP = BEAT / 4;
    N = lround(((minimal ? minimal_bars() : BARS) * BAR
                + (minimal ? 2.0 : TAIL_S)) * SR);   // 2 s: the bird's
                // synthesized release is 0.4 s and the room tail is short
    drumsL = calloc(N, 4); drumsR = calloc(N, 4);
    musicL = calloc(N, 4); musicR = calloc(N, 4);
    voxL = calloc(N, 4); voxR = calloc(N, 4);
    sideV = calloc(N, 4); sideB = calloc(N, 4);
    dlySend = calloc(N, 4); rvbSend = calloc(N, 4);

    printf("→ scoring %d bars @ %.0f BPM · D minor @ %.2f Hz · the press\n",
           minimal ? minimal_bars() : BARS, BPM, TONIC);
    bank_load_dir("vox3");

    if (minimal) {
        // ONE run of the poem. The study is for scrutinising the
        // alignment, and a second pass only doubles the sitting-through.
        // NO COUNT-IN — the file opens on her /f/. The phrase's beat 0,
        // and the first press with it, land one lead-in later, so the
        // downbeat IS the first word.
        const ChartPhrase *p = phrase(POEM);
        int pressBars = minimal_bars();
        double off = p->leadIn;
        printf("  MINIMAL — press + words, ONE pass, no count-in, poem %.1f beats\n",
               p->beats);
        // The belt and the offbeat hats go in too: four on the floor tells
        // you where the bar is but not where you are inside it, and the
        // words are being placed on 8ths and 16ths.
        for (int bar = 0; bar < pressBars; bar++) {
            double t = off + at(bar);
            for (int b = 0; b < 4; b++) press(t + b * BEAT, 0.95);
            for (int b = 0; b < 4; b++)
                airhat(t + (b + 0.5) * BEAT, 0.15, (b % 2) ? 0.3 : -0.3);
            for (int s = 0; s < 16; s++)
                tick(t + s * STEP, (s % 4 == 0) ? 0.10 : 0.05,
                     -0.85 + 1.7 * (s / 15.0));
        }
        sung(POEM, 0.0, 0.98, 0, 0.0, 0);
        goto mixdown;
    }

    // ── THE BED ────────────────────────────────────────────────────────
    hum(0.0, at(58), 1.0, 0);                    // the motor, running
    hum(at(58), at(6), 0.9, -3);                 // SHUTDOWN: down a minor 3rd
    for (int bar = 4; bar < BARS - 4; bar++) {
        const Chord *c = chord_at(bar);
        int brk = (bar >= 40 && bar < 48), outro = bar >= 56;
        if (bar % 2 == 0) {
            double lows[3] = { c->root - 24, c->root - 12, c->tones[1] - 12 };
            double g = brk ? 0.15 : outro ? 0.12 : 0.16;
            pad(at(bar) + 0.02, lows, 3, 2 * BAR - 0.1, g, brk ? 2.4 : 1.1,
                0, 0.55, 0, 0.30);
            if (!brk && !outro) {
                double mids[3] = { c->tones[0], c->tones[1], c->tones[2] };
                pad(at(bar) + 0.15, mids, 3, 2 * BAR - 0.3, g * 0.66, 1.5,
                    0.18, 0.7, 0.10, 0.36);
            }
        }
        if (press_on(bar))
            for (int b = 0; b < 4; b++)
                bass(at(bar) + (b + 0.5) * BEAT, c->root - 24, 0.30, 0.9, 0.012);
        else
            bass(at(bar), c->root - 24, BAR - 0.12, 0.5, 0.045);
    }

    // ── I · POWER-ON 0–8 — the relays find the grid, the die seats ─────
    for (int bar = 0; bar < 6; bar++) {
        double dens = bar / 6.0;
        for (int s = 0; s < 16; s++)
            if (rnd() < 0.25 + 0.65 * dens)
                relay(at(bar) + s * STEP + jit(6), 0.30 + 0.4 * dens,
                      -0.6 + 1.2 * rnd());
    }
    riser(at(6), 2 * BAR, 0.20);

    // ── II · THE STAMP 8–24 — the cell twice, IDENTICAL ────────────────
    // No humanization. No velocity spread. That is the subject.
    for (int bar = 6; bar < BARS; bar++) {
        if (!press_on(bar)) continue;
        double pg = bar < 16 ? 0.90 : bar < 32 ? 0.97 : bar < 56 ? 1.0 : 0.9;
        double bg = bar < 16 ? 0.060 : 0.080;
        floor_bar(bar, pg, bg, bar >= 12, bar >= 32 && bar < 40);
    }
    voice_line(POEM, 8, 0.96, 0.0, 0, 0, 0);
    pluck_line(POEM, 8, 0.42, 0, 0.3);
    voice_line(POEM, 16, 0.96, 0.0, 0, 0, 0);      // the same copy, again
    pluck_line(POEM, 16, 0.42, 0, 0.3);

    // ── III · FULL SPEC 24–32 — the factory at capacity ────────────────
    voice_line(POEM, 24, 0.98, 0.22, 0.28, 1, 0);
    pluck_line(POEM, 24, 0.50, 0, 0.3);
    pluck_line(POEM, 24, 0.22, 12, -0.35);

    // ── IV · OUT OF SPEC 32–40 — copy k is k steps out of tolerance ────
    // 9 cents and 9 ms per copy, exactly as v1 did it — the tolerances
    // slipping is the middle line of the poem coming true.
    for (int k = 1; k <= 4; k++) {
        double bar = 32 + (k - 1) * 2;
        voice_line(POEM, bar + k * 0.009 / BAR, 0.90 - 0.04 * k,
                   0.14, 0.18, k > 2, k * 9.0);
    }
    pluck_line(POEM, 32, 0.44, 0, 0.3);
    riser(at(38), 2 * BAR, 0.24);

    // ── V · BREAK FREE 40–48 — the press stops. The words alone. ───────
    voice_line(POEM, 40, 1.0, 0.16, 0, 0, 0);
    pluck_line(POEM, 40, 0.34, 0, -0.3);

    // ── VI · THE ESCAPE 48–56 — everything, and the bird last ──────────
    voice_line(POEM, 48, 0.98, 0.26, 0.32, 1, 0);
    pluck_line(POEM, 48, 0.52, 0, 0.35);
    pluck_line(POEM, 48, 0.24, 12, -0.35);
    {   // the bird, alone, an octave down and far away — outside the key
        const ChartPhrase *p = phrase(POEM);
        const ChartNote *last = &p->notes[p->n - 1];
        for (int k = 0; k < 3; k++)
            pluck(at(54) + k * 2 * BEAT, last->st - 12, 2 * BEAT,
                  0.30 - 0.06 * k, (k % 2) ? 0.4 : -0.4);
    }

    // ── VII · SHUTDOWN 56–64 — the press decelerates ───────────────────
    // Real varispeed drift on the read head: each copy slower and lower,
    // and the last one never finishes.
    for (int k = 0; k < 4; k++) {
        double r = 1.0 - 0.06 * (k + 1);
        Shot o = shot_defaults();
        o.gain = 0.72 - 0.14 * k; o.side = 0.6; o.dly = 0.24; o.rvb = 0.5;
        o.rate = r; o.dur = (k == 3) ? 3.0 : 0;
        shot(POEM, at(56) + k * 1.6, &o);
    }
    for (int bar = 60; bar < BARS; bar++) {         // the belt stretches
        double g = 0.9 - 0.2 * (bar - 60);
        double gap = STEP;
        double t = at(bar);
        for (int s = 0; s < 16 && t < at(BARS); s++) {
            tick(t, 0.06 * fmax(0.1, g), -0.85 + 1.7 * (s / 15.0));
            gap *= 1.13;
            t += gap;
        }
    }
    hiss_bed();

mixdown:
    if (missingN) fprintf(stderr, "  ! %d missing samples\n", missingN);

    // ── dub delay — dotted 8th, damped cross-feedback ──────────────────
    {
        long D = lround(0.75 * BEAT * SR);
        double FB = 0.38;
        double damp = 1 - exp((-TAU * 2200) / SR);
        double hpRc = 1 / (TAU * 160), hpA = hpRc / (hpRc + 1.0 / SR);
        float *bL = calloc(N + D + 1, 4), *bR = calloc(N + D + 1, 4);
        double dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
        for (long i = 0; i < N; i++) {
            double tapL = i >= D ? bL[i - D] : 0;
            double tapR = i >= D ? bR[i - D] : 0;
            dL += damp * (tapR - dL);
            dR += damp * (tapL - dR);
            bL[i] = (float)(dlySend[i] + dR * FB);
            bR[i] = (float)(dL * FB);
            hpL = hpA * (hpL + bL[i] - pL); pL = bL[i];
            hpR = hpA * (hpR + bR[i] - pR); pR = bR[i];
            musicL[i] += (float)(hpL * 0.56);
            musicR[i] += (float)(hpR * 0.56);
        }
        free(bL); free(bR);
    }

    // ── the diffuse tail — decorrelated Schroeder pair ─────────────────
    {
        static const double combMsL[4] = { 44.6, 47.9, 51.5, 54.2 };
        static const int combOffR[4] = { 23, 29, 31, 37 };
        double rt60 = 3.2, dampHz = 3400.0;
        double kDamp = 1 - exp((-TAU * dampHz) / SR);
        long pre = lround(0.040 * SR);
        float *retL = calloc(N, 4), *retR = calloc(N, 4);
        for (int side = 0; side < 2; side++) {
            float *ret = side ? retR : retL;
            for (int c = 0; c < 4; c++) {
                long D = lround(combMsL[c] * 0.001 * SR) + (side ? combOffR[c] : 0);
                double g = pow(10.0, -3.0 * (D / (double)SR) / rt60);
                float *buf = calloc(N + D + 1, 4);
                double lp = 0;
                for (long i = 0; i < N; i++) {
                    double in = (i >= pre) ? rvbSend[i - pre] : 0;
                    double tap = i >= D ? buf[i - D] : 0;
                    lp += kDamp * (tap - lp);
                    buf[i] = (float)(in + lp * g);
                    ret[i] += (float)(tap * 0.25);
                }
                free(buf);
            }
            static const double apMs[2] = { 22.2, 9.0 };
            for (int a = 0; a < 2; a++) {
                long D = lround(apMs[a] * 0.001 * SR) + (side ? 5 : 0);
                float *buf = calloc(N + D + 1, 4);
                for (long i = 0; i < N; i++) {
                    double x = ret[i];
                    double tap = i >= D ? buf[i - D] : 0;
                    double v = x + 0.7 * tap;
                    buf[i] = (float)v;
                    ret[i] = (float)(tap - 0.7 * v);
                }
                free(buf);
            }
            double rc = 1 / (TAU * 180), ka = rc / (rc + 1.0 / SR);
            double h = 0, prev = 0;
            for (long i = 0; i < N; i++) {
                h = ka * (h + ret[i] - prev); prev = ret[i];
                ret[i] = (float)h;
            }
        }
        for (long i = 0; i < N; i++) { voxL[i] += retL[i] * 0.34f; voxR[i] += retR[i] * 0.34f; }
        free(retL); free(retR);
    }

    // ── the pump + the mix ─────────────────────────────────────────────
    float *envBed = duck_env(0.62, 0.008, 0.24);
    float *envVox = duck_env(0.46, 0.008, 0.20);
    float *L = calloc(N, 4), *R = calloc(N, 4);
    double kSide = 1 - exp((-TAU * 6000) / SR);
    double sV = 0, sB = 0;
    for (long i = 0; i < N; i++) {
        sV += kSide * (sideV[i] - sV);
        sB += kSide * (sideB[i] - sB);
        double bed = envBed[i], vx = envVox[i] * VOXG;
        double l = drumsL[i] + (musicL[i] + sB) * bed + (voxL[i] + sV) * vx;
        double r = drumsR[i] + (musicR[i] - sB) * bed + (voxR[i] - sV) * vx;
        L[i] = (float)l; R[i] = (float)r;
    }
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double v = fmax(fabs((double)L[i]), fabs((double)R[i]));
        if (v > peak) peak = v;
    }
    double trim = peak > 1e-9 ? 0.92 / peak : 1.0;
    printf("# pre-master peak %.6f · linear trim %.3f\n", peak, trim);
    for (long i = 0; i < N; i++) { L[i] = (float)(L[i] * trim); R[i] = (float)(R[i] * trim); }

    char outp[512];
    snprintf(outp, sizeof outp, "%s/out/%s", LANE,
             minimal ? "factory-kickvox-full.wav" : "factory-remix-v3-full.wav");
    write_wav_f32_stereo(outp, L, R, N);
    printf("✓ %s\n  %.1f s scored · master with: bash %s/c/cut-v3.sh\n",
           outp, N / (double)SR, LANE);
    return 0;
}
