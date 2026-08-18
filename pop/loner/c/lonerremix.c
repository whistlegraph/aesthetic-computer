// lonerremix.c — "whistlegraph loner --- remix (v4, regulated)", the C engine.
//
// @jeffrey (2026-08-17): "i wanna go from bedroom ballad to nice dance
// track" · "keep a pretty strict beat that the lyrics can now regulate
// around, with a kick and stuff, sidechained into the lyrics" · "i want
// this all to be in c code". So v4 is the lane's first C-native score —
// no Node render to port from; this file IS the score — following the
// fleet-standard single-file renderer the other /pop lanes carry
// (pop/cult/c, pop/boombaboom/c, pop/hellsine/c).
//
// The deal with the Python side: bin/halo3.py owns everything WORLD —
// it bakes the regulated vocal bank (snap 0.92, per-word frame warp
// onto the 122 BPM chart, octave halos, full-word low-3rd/low-5th
// backup) into vox4/, and emits c/loner-chart.h: per phrase, per word,
// its beat slot and its measured semitone. This engine reads that
// header, so THE BAND'S MELODY IS HER MELODY — the pluck doubles the
// chart, answers with it in the gaps, and closes the track with it.
//
//   FLOOR     Four on it. Kick every beat from bar 0; clap 2 & 4; an
//             offbeat air-hat; velocity-shaped 16th ticks. Strict — the
//             one thing that never breathes.
//   PUMP      The kick sidechains INTO the lyrics: every beat ducks the
//             vox bus 0.34 and the music bus 0.52 (10 ms pre-open,
//             smooth recovery inside the beat). Drums never duck — the
//             cult bus law survives the genre change.
//   NOW       Her first word lands ON beat 0 of bar 0 (the chart's
//             lead-in consonant runs ahead of the downbeat).
//   HER NOTES the pluck (a music-box pair: sine + 2.7× partial, 6 ms
//             attack, exponential decay, dotted-8th delay send) plays
//             only chart notes. Nothing in this file invents a melody.
//   SPACE     v3's rooms, ported intact: the dotted-8th dub delay
//             (damped cross-feedback at 0.38) and the decorrelated
//             Schroeder pair on the vox bus (4 combs/side 44.6–54.2 ms,
//             RT60 ≈ 3.2 s, damped in-loop, two allpasses, 40 ms
//             pre-delay, 180 Hz high-passed return).
//
// Form, 76 bars at 122 (2:29 + tail):
//
//   V1      0:00  0–16   f- phrases on the chart, pluck answering
//   HOOK    0:31  16–24  of-a-stone + hk unison + stone-long canon
//   V2      0:47  24–32  the "not again!" phrases, backup 3rds in
//   BREAK   1:03  32–40  kick out: naked regulated line, stone-long,
//                        riser — the one breath
//   DROP    1:18  40–60  everything: both backups, harps, ens crowds,
//                        the stone-long-17 crown
//   OUT     1:58  60–76  the pluck alone finishes the sentence
//
// Mixing rules survive from v1→v3: raised-cosine tails, no master tanh,
// one linear trim, mono-safe pans with band-limited antisymmetric side.
//
// Build:  bash pop/loner/c/build.sh
// Run:    pop/loner/c/lonerremix        # → pop/loner/out/loner-remix-v4-full.wav
//         (from the repo root; cut-v4.sh masters it to mp3)

#include <dirent.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "loner-chart.h"

#define SR 48000
#define TAU 6.283185307179586
static const double BPM = CHART_BPM;      // 122 — the chart's clock
static const double TONIC = CHART_TONIC;  // 237 Hz — Camille's frame
#define BARS 76
#define TAIL_S 6.0
static double BEAT, BAR, STEP;            // set in main
static long N;
#define LANE "pop/loner"

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

static uint32_t seed = 20260817u;
static double rnd(void) { seed = seed * 1664525u + 1013904223u; return seed / 4294967296.0; }
static double jit(double ms) { return ((rnd() - 0.5) * 2 * ms) / 1000.0; }
static uint32_t nseed = 20210725u;
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

// ── WAV IO (the fleet loader, cultremix.c's) ───────────────────────────
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

// ── sample bank — vox4/ (the chart bank) + vox3/ (arps, ens, longs) ────
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
    long a = 0;
    while (a < n - 1 && fabs((double)raw[a]) < 0.008) a++;
    long from = a - lround(0.002 * SR); if (from < 0) from = 0;
    long tn = n - from;
    double peak = 0;
    for (long i = 0; i < tn; i++) { double v = fabs((double)raw[from + i]); if (v > peak) peak = v; }
    double g = peak > 1e-6 ? 0.95 / peak : 1.0;
    float *out = (float *)malloc(tn * sizeof(float));
    for (long i = 0; i < tn; i++) out[i] = (float)((double)raw[from + i] * g);
    free(raw);
    Sample *slot = bank_get(name);
    if (!slot) {
        if (bankN >= MAX_BANK) { fprintf(stderr, "! bank full\n"); exit(1); }
        slot = &BANK[bankN++];
        snprintf(slot->name, sizeof slot->name, "%s", name);
    } else free(slot->s);
    slot->s = out; slot->n = tn;
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

// ── the one-shot player (render3's shot(), ported) ─────────────────────
typedef struct {
    double gain, pan, side, dark, dur, dly, rvb, off, attack;
    int bus, rev;
} Shot;
static Shot shot_defaults(void) {
    Shot o = { 1, 0, 0.35, 0, 0, 0, 0, 0, 0.0015, BUS_VOX, 0 };
    return o;
}
static void shot(const char *name, double t, const Shot *o) {
    Sample *s = bank_get(name);
    if (!s) { fprintf(stderr, "  ! missing %s\n", name); missingN++; return; }
    long start = lround(o->off * SR);
    if (start < 0) start = 0;
    if (start > s->n - 2) start = s->n - 2;
    long avail = o->rev ? (start ? start : s->n - 2) : (s->n - 2 - start);
    long n = o->dur > 0 ? (long)fmin((double)avail, o->dur * SR) : avail;
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
        pos += o->rev ? -1.0 : 1.0;
    }
}

// lead + halo + optional backup, all locked to the same chart warp
static void sung(const char *name, double t, double gain, double pan, double dly) {
    Shot o = shot_defaults();
    o.gain = gain; o.pan = pan; o.side = 0.5; o.dly = dly; o.rvb = 0.28;
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
                       double haloG, double backupG, int five) {
    const ChartPhrase *p = phrase(name);
    double t = at(bar) - p->leadIn;
    sung(name, t, gain, 0, 0.13);
    if (haloG > 0) halo(name, t, haloG);
    if (backupG > 0) backup(name, t, backupG, five);
}

// ── the kit — four on the floor, synthesized ───────────────────────────
#define MAX_KICKS 2048
static double kickT[MAX_KICKS]; static int kickN = 0;
static const double KSAT_D = 0.955959731045443; // tanh(1.9)
static void kick(double t, double gain) {
    if (kickN < MAX_KICKS) kickT[kickN++] = t;
    long n = lround(0.34 * SR), i0 = lround(t * SR);
    double ph = 0, sub = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        double f = 48 + 107 * exp(-u * 45);
        ph += (TAU * f) / SR;
        sub += (TAU * 45) / SR;
        double env = (0.6 * exp(-u * 28) + 0.5 * exp(-u * 9)) * fmin(1, u / 0.0012);
        double body = tanh(sin(ph) * env * 1.9) / KSAT_D;
        double low = sin(sub) * exp(-u * 7) * 0.22;
        double blip = exp(-u * 520) * 0.07 * sin(TAU * 2200 * u);
        emit(BUS_DRUMS, i0 + i, (body + low + blip) * 0.80 * gain * tail_fade(i, n),
             0, NULL, 0, 0, 0);
    }
}
// clap — three noise bursts 11 ms apart through a 1.1–3 kHz band, for
// the 2-and-4 (the dance answer to v3's snare, which stays for ghosts)
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
        emit(BUS_DRUMS, i0 + i, s * 0.46 * gain * fmin(1, u / 0.001) * tail_fade(i, n),
             pan, &sp, 0.35, 0, 0.06);
    }
}
static void snare(double t, double gain, double pan) {
    long n = lround(0.18 * SR), i0 = lround(t * SR);
    Spatial sp = spatial(pan * 1.2);
    double ph = 0, bp = 0, bp2 = 0;
    double k = 1 - exp((-TAU * 3200) / SR), k2 = 1 - exp((-TAU * 1200) / SR);
    for (long i = 0; i < n; i++) {
        double u = i / (double)SR;
        ph += (TAU * 196) / SR;
        double w = nrnd();
        bp += k * (w - bp); bp2 += k2 * (w - bp2);
        double noise = (bp - bp2) * exp(-u * 22);
        double knock = sin(ph) * exp(-u * 30) * 0.5;
        double s = tanh((noise * 1.6 + knock) * 1.4);
        emit(BUS_DRUMS, i0 + i, s * 0.52 * gain * fmin(1, u / 0.001) * tail_fade(i, n),
             pan, &sp, 0.3, 0, 0.05);
    }
}
// tick — a synthesized closed hat: 8 kHz-centred noise, 30 ms
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
// airhat — the offbeat open hat, longer and breathier
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
static void riser(double t, double dur, double gain) {
    long n = lround(dur * SR), i0 = lround(t * SR);
    double bp = 0;
    for (long i = 0; i < n; i++) {
        double u = i / (double)n;
        double w = nrnd();
        double kf = 1 - exp((-TAU * (600 + 8000 * u * u)) / SR);
        bp += kf * (w - bp);
        emit(BUS_DRUMS, i0 + i, bp * u * u * gain * tail_fade(i, n),
             (u - 0.5) * 0.4, NULL, 0, 0, 0.10);
    }
}

// ── the bed — pad with wow, bass, and the pluck (her melody) ───────────
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
        double u = i / (double)SR;
        double wall = (i0 + i) / (double)SR;
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
// pluck — the music box that plays only her notes: a detuned sine pair
// plus a 2.7× partial, 6 ms attack, decay scaled to the note length
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
// play a chart phrase's melody as pluck notes — the band doubling her,
// or answering in the gaps (octave 0 doubles; +12 answers up high)
static void pluck_line(const char *name, double bar, double gain, int oct, double pan) {
    const ChartPhrase *p = phrase(name);
    for (int i = 0; i < p->n; i++) {
        const ChartNote *nn = &p->notes[i];
        pluck(at(bar) + nn->beat * BEAT + jit(3), nn->st + oct,
              nn->dur * BEAT, gain * (1 - 0.03 * i), pan * ((i % 2) ? -1 : 1));
    }
}

// ── the harp (vox3's arp bank, LCG-swung) ──────────────────────────────
static const int ARP_I[]   = { 12, 15, 19, 24, 27 };
static const int ARP_III[] = { 15, 19, 22, 27 };
static const int ARP_IV[]  = { 17, 20, 24 };
static const int ARP_VI[]  = { 15, 20, 24, 27 };
static const int ARP_VII[] = { 17, 22, 26 };
static void arp(double t, const int *tones, int ntones, const char *vowel,
                int count, int up, double gap, double gain, double pan) {
    for (int k = 0; k < count; k++) {
        int st = tones[up ? k % ntones : ntones - 1 - (k % ntones)]
                 + 12 * (k / ntones) * (up ? 1 : -1);
        char nm[32]; snprintf(nm, sizeof nm, "arp-%s-%d", vowel, st);
        if (!bank_get(nm)) continue;
        Shot o = shot_defaults();
        o.gain = gain * (1 - 0.06 * k);
        o.pan = pan * ((k % 2) ? -1 : 1);
        o.side = 0.8; o.dly = 0.30; o.rvb = 0.5; o.dur = 0.22; o.attack = 0.022;
        shot(nm, t + k * gap + jit(4) + ((k % 2) ? 0.010 : 0), &o);
    }
}

// ── tape ───────────────────────────────────────────────────────────────
static void hiss_bed(void) {
    double lp = 0, hp = 0, prev = 0, lvl = 0.010;
    double kLp = 1 - exp((-TAU * 5200) / SR);
    double hpRc = 1 / (TAU * 320), hpA = hpRc / (hpRc + 1.0 / SR);
    for (long i = 0; i < N; i++) {
        double bar = i / (double)SR / BAR;
        double target = bar < 32 ? 0.0040 : bar < 40 ? 0.0095 :
                        bar < 60 ? 0.0040 : 0.011;
        lvl += 0.000004 * (target - lvl);
        double w = nrnd();
        lp += kLp * (w - lp);
        hp = hpA * (hp + lp - prev); prev = lp;
        musicL[i] += hp * lvl; musicR[i] += hp * lvl * 0.94;
    }
}
static void dust(double t, double gain) {
    long n = lround(0.0022 * SR), i0 = lround(t * SR);
    for (long i = 0; i < n; i++)
        emit(BUS_MUSIC, i0 + i, nrnd() * exp(-i / (0.0006 * SR)) * gain,
             nrnd() * 0.5, NULL, 0, 0, 0);
}

// ── harmony — the v3 chords on a one-bar dance rhythm ──────────────────
typedef struct { double root; double tones[3]; } Chord;
static const Chord CH_i   = { 0,  { 0, 3, 7 } };
static const Chord CH_III = { 3,  { 3, 7, 10 } };
static const Chord CH_VI  = { -4, { -4, 0, 3 } };
static const Chord CH_VII = { -2, { -2, 2, 5 } };
// per section: 8-bar rows (dance harmonic rhythm: one chord per bar)
static const Chord *ROW_VERSE[8] = { &CH_i, &CH_i, &CH_VI, &CH_VI, &CH_III, &CH_III, &CH_VII, &CH_VII };
static const Chord *ROW_HOOK[8]  = { &CH_VI, &CH_VII, &CH_i, &CH_i, &CH_VI, &CH_VII, &CH_III, &CH_VII };
static const Chord *ROW_BREAK[8] = { &CH_i, &CH_i, &CH_VI, &CH_VI, &CH_i, &CH_i, &CH_VII, &CH_VII };
static const Chord *chord_at(int bar) {
    if (bar < 12) return ROW_VERSE[bar % 8];
    if (bar < 20) return ROW_HOOK[bar % 8];
    if (bar < 28) return ROW_VERSE[bar % 8];
    if (bar < 40) return ROW_BREAK[bar % 8];
    if (bar < 58) return (bar < 52) ? ROW_VERSE[bar % 8] : ROW_HOOK[bar % 8];
    return ROW_BREAK[bar % 8];
}
static int kick_on(int bar) {           // the strict floor: where it runs
    return !(bar >= 28 && bar < 40) && bar < 72;
}

// ── the floor — one bar of the dance kit ───────────────────────────────
static const double VEL[16] = { 1, 0.45, 0.7, 0.5, 0.9, 0.45, 0.72, 0.5,
                                1, 0.45, 0.7, 0.5, 0.85, 0.5, 0.78, 0.55 };
static void floor_bar(int bar, double kickG, double hatG, int claps, int fills) {
    double t = at(bar);
    for (int b = 0; b < 4; b++) kick(t + b * BEAT, kickG * (b == 0 ? 1.0 : 0.94));
    if (claps) { clap(t + 1 * BEAT, 0.85, 0.08); clap(t + 3 * BEAT, 0.82, -0.06); }
    for (int b = 0; b < 4; b++) airhat(t + (b + 0.5) * BEAT, 0.16, (b % 2) ? 0.3 : -0.3);
    for (int s = 0; s < 16; s++) tick(t + s * STEP + jit(2), hatG * VEL[s], (s % 2) ? 0.3 : -0.24);
    if (fills && bar % 4 == 3) {
        snare(t + 14 * STEP, 0.30, 0.2);
        snare(t + 15 * STEP, 0.36, -0.15);
    }
    if (bar % 2 == 1) snare(t + 7 * STEP, 0.20, -0.2);
}

// ── sidechain — the pump, keyed by every kick ──────────────────────────
static float *duck_env(double depth, double atk, double rel) {
    float *e = (float *)malloc(N * sizeof(float));
    for (long i = 0; i < N; i++) e[i] = 1;
    long pre = lround(0.010 * SR);
    for (int k = 0; k < kickN; k++) {
        long i0 = lround(kickT[k] * SR) - pre;
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
    // the line once, plus a bar to breathe (no count-in any more)
    return (int)(ceil(phrase("w-whole-line")->beats / 4.0) + 2);
}

int main(void) {
    // MINIMAL=1 → the study pass: kick + the unbroken vocal, nothing
    // else — "lets start with just kick and vocals and get that right".
    int minimal = getenv("MINIMAL") != NULL;
    BEAT = 60.0 / BPM; BAR = 4 * BEAT; STEP = BEAT / 4;
    N = lround(((minimal ? minimal_bars() : BARS) * BAR + TAIL_S) * SR);
    drumsL = calloc(N, 4); drumsR = calloc(N, 4);
    musicL = calloc(N, 4); musicR = calloc(N, 4);
    voxL = calloc(N, 4); voxR = calloc(N, 4);
    sideV = calloc(N, 4); sideB = calloc(N, 4);
    dlySend = calloc(N, 4); rvbSend = calloc(N, 4);

    printf("→ scoring %d bars @ %.0f BPM · A# minor @ %.0f Hz · four on the floor\n",
           BARS, BPM, TONIC);
    bank_load_dir("vox4");
    bank_load_dir("vox3");

    if (minimal) {
        // ONE run of the line — @jeffrey: "lets only render a single run
        // of the vocals · no need to do anything other than the first
        // loop". The study is for scrutinising the alignment, and a
        // second pass only doubles the sitting-through.
        // NO COUNT-IN — the file opens on her pickup. The render starts
        // at the /s/ of "sitting"; the phrase's beat 0, and the first
        // kick with it, land one lead-in later, so the downbeat IS the
        // first word rather than the third bar of waiting.
        const ChartPhrase *p = phrase("w-whole-line");
        double lineBars = ceil(p->beats / 4.0);
        int kickBars = (int)(lineBars + 1);
        double off = p->leadIn;                 // the pickup sits ahead of beat 0
        printf("  MINIMAL — kick + vocals, ONE pass, no count-in, line %.0f beats\n",
               p->beats);
        // hats too — @jeffrey: "can we add little hi hats so we can get a
        // better head on the beat in this monitor video?" Four on the
        // floor alone tells you where the bar is but not where you are
        // inside it; offbeat air-hats give the 8ths, and quiet 16th ticks
        // give the subdivision the words are actually being placed on.
        for (int bar = 0; bar < kickBars; bar++) {
            double t = off + at(bar);
            for (int b = 0; b < 4; b++) kick(t + b * BEAT, 0.95);
            for (int b = 0; b < 4; b++)
                airhat(t + (b + 0.5) * BEAT, 0.15, (b % 2) ? 0.3 : -0.3);
            for (int s = 0; s < 16; s++)
                tick(t + s * STEP, (s % 4 == 0) ? 0.10 : 0.05,
                     (s % 2) ? 0.28 : -0.22);
        }
        sung("w-whole-line", 0.0, 0.98, 0, 0.0);
        goto mixdown;
    }

    // THE BED — pads on one-bar chords everywhere the floor runs; the
    // break gets the slow-attack ballad voicing.
    for (int bar = 0; bar < BARS - 4; bar++) {
        const Chord *c = chord_at(bar);
        double lows[3] = { c->root - 24, c->root - 12, c->tones[1] - 12 };
        int brk = (bar >= 32 && bar < 40), outro = bar >= 60;
        if (bar % 2 == 0) {
            double g = brk ? 0.15 : outro ? 0.13 : 0.16;
            pad(at(bar) + 0.02, lows, 3, 2 * BAR - 0.1, g,
                brk ? 2.4 : 1.1, 0, 0.55, 0, 0.30);
            if (!brk && !outro) {
                double mids[3] = { c->tones[0], c->tones[1], c->tones[2] };
                pad(at(bar) + 0.15, mids, 3, 2 * BAR - 0.3, g * 0.68,
                    1.5, 0.18, 0.7, 0.10, 0.36);
            }
        }
        // THE BASS — offbeat house sub answering every kick
        if (kick_on(bar)) {
            for (int b = 0; b < 4; b++)
                bass(at(bar) + (b + 0.5) * BEAT, c->root - 24, 0.30, 0.9, 0.012);
        } else {
            bass(at(bar), c->root - 24, BAR - 0.12, 0.5, 0.045);
        }
    }

    // THE FLOOR
    for (int bar = 0; bar < BARS; bar++) {
        if (!kick_on(bar)) continue;
        double build = bar >= 38 ? 0 : 1;   (void)build;
        double kg = bar < 12 ? 0.92 : bar < 20 ? 0.98 : bar < 28 ? 0.95 : 1.0;
        double hg = bar < 12 ? 0.065 : 0.085;
        int claps = bar >= 4;
        floor_bar(bar, kg, hg, claps, bar >= 12);
    }
    // the rebuild rush out of the break
    riser(at(38), 2 * BAR, 0.26);
    for (int s = 0; s < 8; s++) snare(at(39) + (8 + s) * STEP, 0.18 + 0.04 * s, (s % 2) ? 0.25 : -0.25);

    // ── V1 0–12 — THE UNBROKEN TAKE, first word ON beat 0 of bar 0 ─────
    // One 12-bar continuous vocal; the pluck doubles her INSIDE the
    // line — no answer gaps, the held vowels are the arrangement.
    voice_line("w-whole-line", 0, 0.96, 0.18, 0, 0);
    pluck_line("w-whole-line", 0, 0.5, 0, 0.3);
    arp(at(4.5), ARP_III, 4, "oh", 5, 1, 0.15, 0.12, 0.35);   // inside "stone…"
    arp(at(8.5), ARP_VII, 3, "ah", 5, 0, 0.15, 0.12, -0.35);  // inside "patiently…"

    // ── HOOK 12–20 — her held STONE opens it; the stab waits till 14 ───
    { Shot o = shot_defaults(); o.gain = 0.32; o.side = 0.85; o.dly = 0.3; o.rvb = 0.6;
      o.attack = 0.5; shot("stone-long-12", at(12), &o); }
    pluck_line("w-of-a-stone", 12, 0.55, 0, 0.3);            // the melody leads
    voice_line("w-of-a-stone", 14, 1.0, 0.24, 0.30, 0);
    { Shot o = shot_defaults(); o.gain = 0.5; o.pan = 0.3; o.dly = 0.2; o.rvb = 0.4;
      shot("hk-of-a", at(14) - 0.05, &o); }
    voice_line("w-of-a-stone", 16, 0.72, 0.18, 0, 0);        // the echo stab
    voice_line("w-of-a-stone", 18, 1.0, 0.26, 0.32, 1);      // crowned, tiles to V2
    pluck_line("w-of-a-stone", 18, 0.6, 12, -0.3);
    { Shot o = shot_defaults(); o.gain = 0.26; o.side = 0.9; o.dly = 0.32; o.rvb = 0.65;
      o.attack = 0.5; shot("stone-long-5", at(19), &o); }
    { Shot o = shot_defaults(); o.gain = 0.20; o.pan = 0.4; o.side = 0.8; o.rvb = 0.5;
      o.attack = 0.6; shot("ens-o-2", at(15), &o); }
    arp(at(13.5), ARP_I, 5, "oh", 6, 1, 0.13, 0.15, 0.35);
    arp(at(15.5), ARP_VI, 4, "ah", 6, 0, 0.13, 0.15, -0.35);
    arp(at(17.5), ARP_III, 4, "oh", 6, 1, 0.13, 0.15, 0.35);
    arp(at(19.5), ARP_VII, 3, "ah", 7, 0, 0.13, 0.15, -0.35);

    // ── V2 20–28 — the "not again!" verse at HER pace, seamless ────────
    // (10.5 + 16.5 + 6 beats back-to-back: she sings straight through)
    voice_line("w-n-getting-curled", 20, 0.92, 0.20, 0.26, 0);
    pluck_line("w-n-getting-curled", 20, 0.5, 0, 0.3);
    voice_line("w-n-stone-waiting", 22.625, 0.92, 0.20, 0.26, 0);
    pluck_line("w-n-stone-waiting", 22.625, 0.5, 0, -0.3);
    voice_line("w-n-for-time-to-pass", 26.5, 0.94, 0.22, 0.28, 1);
    pluck_line("w-n-for-time-to-pass", 26.5, 0.55, 0, 0.3);
    arp(at(26.5), ARP_IV, 3, "oh", 6, 1, 0.13, 0.14, 0.35);
    arp(at(27.2), ARP_VII, 3, "ah", 7, 1, 0.12, 0.16, -0.35);

    // ── BREAK 28–40 — the whole take again, naked over pads ────────────
    voice_line("w-whole-line", 28, 1.0, 0.15, 0, 0);
    { Shot o = shot_defaults(); o.gain = 0.22; o.side = 0.9; o.dly = 0.3; o.rvb = 0.7;
      o.attack = 0.8; shot("stone-long-echo", at(33), &o); }
    dust(at(30) + 1.1, 0.013); dust(at(35) + 2.3, 0.015);
    arp(at(36.5), ARP_I, 5, "oh", 4, 1, 0.28, 0.09, 0.3);

    // ── DROP 40–58 — the unbroken take with everything she has ─────────
    voice_line("w-whole-line", 40, 0.98, 0.24, 0.32, 1);
    pluck_line("w-whole-line", 40, 0.55, 0, 0.3);
    { Shot o = shot_defaults(); o.gain = 0.5; o.pan = -0.3; o.dly = 0.2; o.rvb = 0.4;
      shot("hk-of-a", at(43.25) - 0.05, &o); }               // under "of a"
    { Shot o = shot_defaults(); o.gain = 0.24; o.side = 0.85; o.dly = 0.3; o.rvb = 0.6;
      o.attack = 0.5; shot("pass-long-15", at(49.6), &o); }
    voice_line("w-of-a-stone", 52, 1.0, 0.26, 0.34, 1);
    pluck_line("w-of-a-stone", 52, 0.6, 12, 0.35);
    { Shot o = shot_defaults(); o.gain = 0.30; o.side = 0.9; o.dly = 0.34; o.rvb = 0.7;
      o.attack = 0.5; shot("stone-long-17", at(53), &o); }
    { Shot o = shot_defaults(); o.gain = 0.22; o.pan = -0.4; o.side = 0.85; o.rvb = 0.55;
      o.attack = 0.7; shot("ens-du-2", at(53), &o); }
    voice_line("w-of-a-stone", 54, 0.78, 0.20, 0, 0);
    voice_line("w-for-time-to-pass", 56, 0.92, 0.22, 0.28, 0); // flows into the OUT
    for (int bar = 40; bar < 58; bar += 2) {
        const int *tones = (bar % 8 < 2) ? ARP_I : (bar % 8 < 4) ? ARP_VI :
                           (bar % 8 < 6) ? ARP_III : ARP_VII;
        int nt = (tones == ARP_III) ? 4 : (tones == ARP_VII) ? 3 :
                 (tones == ARP_I) ? 5 : 4;
        arp(at(bar) + 0.5 * BAR, tones, nt, (bar % 4) ? "ah" : "oh",
            7, (bar / 2) % 2 == 0, 0.12, 0.16, 0.35);
    }

    // ── OUT 58–76 — the ghost: the whole take once more, far away,
    // under the pluck singing the same line ────────────────────────────
    voice_line("w-whole-line", 58, 0.55, 0.16, 0, 0);
    pluck_line("w-whole-line", 58, 0.5, 0, 0.3);
    pluck_line("w-for-time-to-pass", 70, 0.36, 12, 0.35);
    { Shot o = shot_defaults(); o.gain = 0.18; o.side = 0.9; o.dly = 0.3; o.rvb = 0.75;
      o.attack = 0.8; shot("pass-long-3", at(72), &o); }
    { Shot o = shot_defaults(); o.gain = 0.14; o.pan = 0.3; o.side = 0.8; o.rvb = 0.6;
      o.attack = 0.9; shot("ens-o-3", at(70), &o); }
    arp(at(73), ARP_I, 5, "oh", 4, 0, 0.28, 0.09, 0.3);
    arp(at(74.5), ARP_I, 5, "ah", 3, 0, 0.30, 0.07, -0.3);
    dust(at(59) + 0.4, 0.012); dust(at(69) + 2.1, 0.014); dust(at(74) + 1.2, 0.013);
    hiss_bed();

mixdown:
    if (missingN) fprintf(stderr, "  ! %d missing samples\n", missingN);

    // ── dub delay — dotted 8th, damped cross-feedback (v1's) ───────────
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

    // ── the diffuse tail — decorrelated Schroeder pair (v3's) ──────────
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
            // two series allpasses, g 0.7
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
            // 180 Hz high-pass on the return
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
    // "side chained into the lyrics": every kick ducks the vox 0.34 and
    // the bed 0.52; drums never duck.
    float *envBed = duck_env(0.52, 0.020, 0.30);
    float *envVox = duck_env(0.34, 0.020, 0.26);
    float *L = calloc(N, 4), *R = calloc(N, 4);
    // band-limited antisymmetric side (one-pole at 6 kHz)
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
             minimal ? "loner-kickvox-full.wav" : "loner-remix-v4-full.wav");
    write_wav_f32_stereo(outp, L, R, N);
    printf("✓ %s\n  %.1f s scored · master with: bash %s/c/cut-v4.sh\n",
           outp, N / (double)SR, LANE);
    return 0;
}
