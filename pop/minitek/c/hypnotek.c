// hypnotek.c — a hypnotic, super-fast minimal-techno engine in C (A minor,
// 142 BPM). One relentless rolling bass hook locks the groove while a tiny
// acid blip motif evolves microscopically across the whole track: the cutoff
// creeps open, the resonance climbs, and a tempo-synced delay smears the stabs
// into a trance-inducing haze. Extreme repetition, deep sidechain pump, long
// sections, almost no fills. Forked from minitek.c — same two-bus + sidechain
// + reverb + normalize/fade infrastructure; reharmonized and re-grooved.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o hypnotek hypnotek.c -lm
// Run:    ./hypnotek --out out/hypnotek-raw.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static double BPMV = 142, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.515;                  // subtle micro-swing — locked, not loose

static uint32_t rng_s = 0x68797074; // "hypt"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── Euclidean rhythms ────────────────────────────────────────────────────────
// Toussaint (2005), "The Euclidean Algorithm Generates Traditional Musical
// Rhythms": Bjorklund's algorithm distributes k onsets over n steps as evenly
// as possible (a "Euclidean" / maximally-even rhythm), e.g. E(5,16) is the
// bossa-nova clave family. We drive several interlocking lanes from different
// E(k,n) and ROTATE each lane's start offset per 4-bar phrase so the interlock
// slowly precesses — the hypnotic creep at the heart of this track.
// Bjorklund's algorithm (iterative bucket form): grow k "[1]" buckets and
// (n-k) "[0]" buckets, then repeatedly fold the smaller pile onto the larger
// until one pile has <=1 group; concatenate to read off the maximally-even
// onset string. Returns out[0..n-1] as 0/1. (rot rotates the start offset.)
static void bjorklund(int k, int n, int rot, int *out) {
    for (int i = 0; i < n; i++) out[i] = 0;
    if (n <= 0) return;
    if (k <= 0) return;
    if (k >= n) { for (int i = 0; i < n; i++) out[i] = 1; return; }
    // buckets: arrays of bits; we track them as lists of small bit-strings.
    int a = k, b = n - k;                 // counts of the two group types
    int la = 1, lb = 1;                   // each group's current length (bits)
    int A[64], B[64];                     // bit contents of one "a" group / one "b" group
    A[0] = 1; B[0] = 0;
    while (b > 1) {
        int m = a < b ? a : b;            // how many pairs we can fold
        // new "a" group = old a-group followed by one b-group
        int nla = la + lb, nA[64];
        for (int i = 0; i < la; i++) nA[i] = A[i];
        for (int i = 0; i < lb; i++) nA[la + i] = B[i];
        // leftovers of the larger pile become the new "b" group
        int na, nb;
        if (a > b) { na = b; nb = a - b; /* leftover a-groups */ }
        else       { na = a; nb = b - a; }
        // the remaining (un-folded) pile keeps the old group of the larger type
        int nlb, nBb[64];
        if (a > b) { nlb = la; for (int i = 0; i < la; i++) nBb[i] = A[i]; }
        else       { nlb = lb; for (int i = 0; i < lb; i++) nBb[i] = B[i]; }
        (void)m;
        a = na; b = nb; la = nla; lb = nlb;
        for (int i = 0; i < la; i++) A[i] = nA[i];
        for (int i = 0; i < lb; i++) B[i] = nBb[i];
    }
    // read off: a copies of A, then b copies of B
    int pos = 0;
    for (int g = 0; g < a && pos < n; g++) for (int i = 0; i < la && pos < n; i++) out[pos++] = A[i];
    for (int g = 0; g < b && pos < n; g++) for (int i = 0; i < lb && pos < n; i++) out[pos++] = B[i];
    // rotate start offset (the per-phrase precession)
    if (rot) {
        int tmp[64]; rot = ((rot % n) + n) % n;
        for (int i = 0; i < n; i++) tmp[i] = out[(i + rot) % n];
        for (int i = 0; i < n; i++) out[i] = tmp[i];
    }
}

// ── evenness metrics (necklace theory) ───────────────────────────────────────
// Toussaint (2005) frames a maximally-even rhythm as one whose onsets sit as
// close as possible to the vertices of a regular k-gon inscribed in the n-step
// cycle. We quantify "how even" a 0/1 pattern is with three standard measures:
//
//  (1) IOI variance — variance of the k inter-onset intervals (gaps between
//      successive onsets, wrapped cyclically). A perfectly even rhythm wants all
//      gaps = n/k; Bjorklund minimizes the spread (only two gap sizes, floor/
//      ceil of n/k). Lower variance = more even.
//  (2) Normalized evenness E in [0,1]: 1 - (meanAbsDev / maxAbsDev), where
//      meanAbsDev is the mean |gap - n/k| and maxAbsDev is its worst case (all
//      onsets clustered). E=1 is perfectly even; E small is clustered.
//  (3) Vertex distance D — the sum, over onsets, of the angular distance from
//      each onset to the nearest vertex of the ideal regular k-gon (Toussaint's
//      geometric "evenness"); the maximally-even rhythm minimizes D.

typedef struct { int k; double ioiVar; double evenness; double vertexDist; } EvenMetrics;

// gather onset step-indices of a 0/1 pattern of length n.
static int onsets_of(const int *p, int n, int *idx) {
    int k = 0; for (int i = 0; i < n; i++) if (p[i]) idx[k++] = i; return k;
}

static EvenMetrics measure_evenness(const int *p, int n) {
    EvenMetrics em = { 0, 0, 1, 0 };
    int idx[64]; int k = onsets_of(p, n, idx); em.k = k;
    if (k < 2) return em;
    double ideal = (double)n / k;
    // cyclic inter-onset intervals
    double mean = 0; double dev = 0; double var = 0;
    double iois[64];
    for (int i = 0; i < k; i++) {
        int g = (i + 1 < k) ? idx[i + 1] - idx[i] : idx[0] + n - idx[i];
        iois[i] = g; mean += g;
    }
    mean /= k;
    for (int i = 0; i < k; i++) { double d = iois[i] - mean; var += d * d; dev += fabs(iois[i] - ideal); }
    var /= k; em.ioiVar = var;
    double meanAbsDev = dev / k;
    // worst case (all onsets adjacent): k-1 gaps of 1, one gap of n-(k-1).
    double worstDev = ((k - 1) * fabs(1.0 - ideal) + fabs((double)(n - (k - 1)) - ideal)) / k;
    em.evenness = (worstDev > 1e-9) ? (1.0 - meanAbsDev / worstDev) : 1.0;
    // geometric vertex distance: nearest ideal k-gon vertex (in step units),
    // vertices at j*ideal. Sum of nearest-vertex distances over onsets, min over
    // a global vertex phase offset (the k-gon may be rotated to best fit).
    double best = 1e18;
    for (int ph = 0; ph < n; ph++) {
        double sum = 0;
        for (int i = 0; i < k; i++) {
            double pos = idx[i];
            // nearest vertex j*ideal + ph/ ... search nearest among k vertices
            double bestv = 1e18;
            for (int j = 0; j < k; j++) {
                double v = j * ideal + ph * ideal / n; // sub-step phase sweep
                double d = fabs(pos - v); if (d > n / 2.0) d = n - d;
                if (d < bestv) bestv = d;
            }
            sum += bestv;
        }
        if (sum < best) best = sum;
    }
    em.vertexDist = best;
    return em;
}

// a deliberately clustered baseline: pack k onsets into the first k steps. Used
// to show Bjorklund's evenness is far above a naive packing of the same density.
static void clustered(int k, int n, int *out) {
    for (int i = 0; i < n; i++) out[i] = (i < k) ? 1 : 0;
}

static long N;
// two buses: drums hit hard and dry; music (bass/blip/pad) gets ducked by the
// kick. revL/R is a light send. trig[] marks kick onsets for the sidechain.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *trig;
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── drum voices ─────────────────────────────────────────────────────────────
// kick — tight, punchy minimal-techno thump: pitch sweeps 110→44 Hz fast, a
// short clicky body, hard tanh drive. Stamps the sidechain trigger (KEEP this).
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.30 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 44 + 66 * exp(-tt * 48.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 9.5);
        double click = exp(-tt * 420.0) * 0.8;
        double v = tanh((sin(ph) + click) * 2.1) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.11 : 0.035) * SR);
    double dec = open ? 46.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.42;
        addD(s0 + i, v * lg, v * rg);
    }
}

// clap — three quick noise spits then a short diffuse tail; bandpass-ish.
static void clap(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.18 * SR); double prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double spits = exp(-fmod(tt, 0.009) * 650.0) * (tt < 0.026 ? 1.0 : 0.0);
        double tail = exp(-tt * 18.0);
        double v = hp * (spits * 0.8 + tail) * g * 0.45;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.22, v * 0.22);
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — the rolling bass hook. tanh-saturated sine with a touch of 2nd & a
// gritty reese-ish detune buried under it for rumble; quick attack/release so
// the 16th roll stays articulate.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.004 * SR), rel = (long)(0.025 * SR);
    double ph = 0, ph2 = 0, ph3 = 0;
    for (long i = 0; i < n; i++) {
        ph  += TAU * f / SR;
        ph2 += TAU * f * 2.0 / SR;
        ph3 += TAU * f * 1.007 / SR;                  // slight detune → slow reese beat
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double body  = sin(ph) + 0.18 * sin(ph2);
        double reese = 0.22 * sin(ph3);               // rumbling underlay
        double v = tanh((body + reese) * 1.35) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — the acid motif: a saw through a resonant SVF lowpass with a fast
// cutoff envelope. `res` and `cut1` are pushed from the arrangement so the
// timbre creeps open across the track. A tempo-synced delay (added on the bus)
// smears it. The 303 plink that hypnotizes.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.35, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.002 * SR), rel = (long)(0.018 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;        // naive saw ramp 0..1
        double in = 2.0 * saw - 1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 18.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.15) * env * g;        // gentle drive on the acid
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.16, v * 0.16);
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.3 * SR), rel = (long)(0.5 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1200.0 / SR), q = 1.0 / 0.9;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.006 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.32, v * 0.32);
    }
}

// noise riser — filtered white-noise crescendo for the few transitions.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / (dur);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (400 + 6000 * p) / SR), q = 1.0 / 1.3;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = (hp - low) * p * p * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// rim/clave — a short pitched click (resonant blip of noise + tone); the dry
// Euclidean accent that ticks the bossa-clave skeleton over the kick.
static void rim(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double ph = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * 1700.0 / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double body = sin(ph) * 0.7 + hp * 0.5;
        double v = tanh(body * 1.4) * exp(-tt * 120.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// shaker — tight band of noise with a quick swell; the fine Euclidean grain
// that fills the air between the hats. Panned, lightly enveloped.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0, prev2 = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // 2-pole-ish highpass
        double hp2 = hp - prev2; prev2 = hp;
        double env = (tt < 0.012 ? tt / 0.012 : exp(-(tt - 0.012) * 70.0));
        double v = hp2 * env * g * 0.3;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride/ping — a thin metallic ping (a few inharmonic partials) on a sparse
// Euclidean lane for high shimmer; feeds a touch of reverb.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.20 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double f[4] = { 5200, 7300, 9100, 11700 }, ph[4] = {0,0,0,0};
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, s = 0;
        for (int k = 0; k < 4; k++) { ph[k] += TAU * f[k] / SR; s += sin(ph[k]); }
        double v = s * 0.25 * exp(-tt * 22.0) * g * 0.3;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.25, v * 0.25);
    }
}

// tom — a short pitched membrane (sine sweep) for fill accents at phrase ends.
static void tom(double note, double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double ph = 0;
    double f0 = midi_hz(note), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f0 * (1.0 + 0.6 * exp(-tt * 30.0));
        ph += TAU * pf / SR;
        double v = tanh(sin(ph) * 1.6) * exp(-tt * 13.0) * g * 0.6;
        addD(s0 + i, v * lg, v * rg);
    }
}

// cbass — a plucky counter-bass an octave above the sub, driven off its own
// Euclidean lane to weave a syncopated melodic line against the rolling hook.
static void cbass(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / 0.7;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.003 * SR), rel = (long)(0.04 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;
        double in = 2.0 * saw - 1.0;
        double cut = 900 + 700 * exp(-tt * 22.0);
        double fc = 2.0 * sin(M_PI * cut / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.2) * env * g;
        addM(s0 + i, v * lg, v * rg);
    }
}

// drone — a low sustained dual-saw atmosphere under the pad sections; slow LFO
// on the filter for movement. Adds depth without crowding the kick.
static void drone(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, q = 1.0 / 1.1;
    long att = (long)(1.2 * SR), rel = (long)(1.2 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 0.997 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double lfo = 700 + 500 * sin(TAU * 0.07 * tt);
        double fc = 2.0 * sin(M_PI * lfo / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.25, v * 0.25);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64,
       L_RIM = 128, L_SHAK = 256, L_RIDE = 512, L_CBASS = 1024, L_DRONE = 2048 };

// 142 BPM → bar ≈ 1.690s. Form as EXPERIMENT: the independent variable is the
// composite precession rate PREC[s] — how many steps each lane's start offset
// rotates per 4-bar phrase. The track sweeps it still → slow → fast → still so
// the listener hears maximally-even necklaces lock, drift, blur, and re-lock.
static const char *ORDER[6] = { "still", "slow", "fast", "breath", "blur", "lock" };
static const int SECBARS[6] = { 8, 16, 16, 8, 16, 8 };
// PREC[s] = precession rate (start-offset steps per 4-bar phrase). 0 = frozen
// necklaces (no drift); larger = faster precession of the composite interlock.
static const int PREC[6] = { 0, 1, 3, 0, 5, 0 };
static int MASK[6] = {
    /*still */ L_KICK | L_SUB | L_RIM | L_DRONE,
    /*slow  */ L_KICK | L_CHAT | L_SUB | L_BLIP | L_RIM | L_SHAK,
    /*fast  */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_RIM | L_SHAK | L_RIDE | L_CBASS,
    /*breath*/ L_SUB | L_BLIP | L_PAD | L_SHAK | L_DRONE | L_CBASS,
    /*blur  */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD | L_RIM | L_SHAK | L_RIDE | L_CBASS,
    /*lock  */ L_KICK | L_SUB | L_BLIP | L_RIM | L_DRONE,
};
static int START[6];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// A natural minor, dark and static. The hook hammers A almost the whole way;
// the bass drops to a contrasting low only on the final bar of each 8-bar
// block, so the groove feels locked but never quite dead.
static const int ROOT_A = 33;                            // A1 — the relentless root
static const int ROOT_LO[2] = { 28, 31 };                // E1, G1 — the occasional pull
// blip motif sits tight: a small A-minor pentatonic cell that the engine nudges
// one scale step every few phrases — "microscopic" evolution.
static const int BLIP_SCALE[5] = { 57, 60, 62, 64, 67 }; // A3 C4 D4 E4 G4
static const int PAD_CH[2][3] = { {45,48,52}, {40,43,47} }; // Am, Em (low) — breathing chords
// counter-bass cell: A-minor pentatonic in the bass octave, woven on a
// Euclidean lane so it syncopates against the rolling sub hook.
static const int CBASS_SCALE[5] = { 45, 48, 50, 52, 55 }; // A2 C3 D3 E3 G3

int main(int argc, char **argv) {
    const char *out_path = "out/hypnotek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 6; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 6; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# hypnotek.c · %g BPM · %d bars · %.1fs · hypnotic minimal techno (A minor)\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 6; i++) fprintf(stderr, " %s(%d,prec=%d)", ORDER[i], SECBARS[i], PREC[i]); fprintf(stderr, "\n");

    // ── EXPERIMENT 1: evenness of each lane's Euclidean necklace ──────────────
    // Print the maximally-even E(k,n) string, its IOI variance, normalized
    // evenness E∈[0,1], and Toussaint vertex-distance D — and compare against a
    // clustered baseline of the SAME density to show Bjorklund maximizes
    // evenness. These are the numbers the thesis cites.
    {
        struct { const char *name; int k, n; } lanes[] = {
            {"E_KICK ", 4, 16}, {"E_CHAT ", 7, 16}, {"E_OHAT ", 3, 8},
            {"E_BLIP ", 5, 16}, {"E_RIM  ", 3, 16}, {"E_SHAK ", 9, 16},
            {"E_RIDE ", 2, 16}, {"E_CBASS", 5, 16},
        };
        fprintf(stderr, "# --- EXPERIMENT 1: necklace evenness (Bjorklund vs clustered) ---\n");
        fprintf(stderr, "# lane      pattern             IOIvar  E       D     | clustered: E       D\n");
        double sumE = 0, sumEc = 0; int nl = sizeof(lanes)/sizeof(lanes[0]);
        for (int i = 0; i < nl; i++) {
            int p[64], cb[64];
            bjorklund(lanes[i].k, lanes[i].n, 0, p);
            clustered(lanes[i].k, lanes[i].n, cb);
            EvenMetrics em = measure_evenness(p, lanes[i].n);
            EvenMetrics ec = measure_evenness(cb, lanes[i].n);
            char str[80]; int j; for (j = 0; j < lanes[i].n; j++) str[j] = p[j] ? 'x' : '.'; str[j] = 0;
            fprintf(stderr, "# %s E(%d,%2d)=%-17s %6.3f  %.3f  %5.2f | %.3f  %5.2f\n",
                    lanes[i].name, lanes[i].k, lanes[i].n, str, em.ioiVar, em.evenness, em.vertexDist, ec.evenness, ec.vertexDist);
            sumE += em.evenness; sumEc += ec.evenness;
        }
        // clustered E pins to 0 by construction (it IS the worst case), so the
        // evenness gain is reported via vertex distance D (lower = more even):
        // sum D over lanes for Bjorklund vs clustered.
        double sumD = 0, sumDc = 0;
        for (int i = 0; i < nl; i++) {
            int p[64], cb[64];
            bjorklund(lanes[i].k, lanes[i].n, 0, p); clustered(lanes[i].k, lanes[i].n, cb);
            sumD += measure_evenness(p, lanes[i].n).vertexDist;
            sumDc += measure_evenness(cb, lanes[i].n).vertexDist;
        }
        fprintf(stderr, "# mean evenness E: Bjorklund=%.3f  clustered=%.3f\n", sumE / nl, sumEc / nl);
        fprintf(stderr, "# total vertex-dist D: Bjorklund=%.2f  clustered=%.2f  (Bjorklund %.1f× closer to ideal k-gon)\n",
                sumD, sumDc, (sumD > 1e-6) ? (sumDc / sumD) : 0);
    }

    // ── EXPERIMENT 2: composite realignment period per section ────────────────
    // With precession rate PREC[s], the precessing lanes share the period at
    // which the composite interlock returns to its starting phase. Each lane's
    // offset is (phr4 * PREC * w) mod 16; a lane realigns when that ≡ 0, i.e.
    // every 16/gcd(16, PREC*w) phrases. The composite period is the LCM over
    // lanes (weights 7,5,9,3,1,2 for the precessing voices), expressed in 4-bar
    // phrases and seconds. PREC=0 ⇒ infinite (frozen necklace).
    {
        int weights[] = {1, 2, 1, 3, 5, 2};   // CHAT,BLIP,SHAK,RIM(|−|),OHAT?,... (relative rotation weights)
        // (rotation weights as wired below: CHAT*1, OHAT*1, BLIP*2, RIM*-1, SHAK*3, CBASS*2)
        int rw[] = {1, 1, 2, 1, 3, 2};
        fprintf(stderr, "# --- EXPERIMENT 2: composite drift speed & realignment period ---\n");
        fprintf(stderr, "# section  prec  Σrot/phrase  realign(phrases)  realign(bars)  realign(s)\n");
        for (int s = 0; s < 6; s++) {
            int pr = PREC[s];
            if (pr == 0) { fprintf(stderr, "# %-7s  %d     0            frozen (∞)        —              —\n", ORDER[s], pr); continue; }
            long lcm = 1; int driftSum = 0;
            for (int i = 0; i < (int)(sizeof(rw)/sizeof(rw[0])); i++) {
                int rot = pr * rw[i] % 16; if (rot < 0) rot += 16;
                driftSum += rot;                                                // total composite offset advanced per phrase
                int g = 16, a = rot; while (a) { int t = g % a; g = a; a = t; } // gcd(16,rot)
                long per = (rot == 0) ? 1 : 16 / g;                              // phrases to realign this lane
                long aa = lcm, bb = per; while (bb) { long t = aa % bb; aa = bb; bb = t; }
                lcm = lcm / aa * per;
            }
            fprintf(stderr, "# %-7s  %d     %-2d           %ld                %ld             %.1f\n",
                    ORDER[s], pr, driftSum, lcm, lcm * 4, lcm * 4 * BAR);
        }
        (void)weights;
    }

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_SUB  = "x.xxx.x.x.xxx.x.";   // the rolling 16th bass hook

    // ── Euclidean lanes (Toussaint 2005 / Bjorklund) ────────────────────────
    // Each lane is a maximally-even E(k,n) onset string; the kick anchors the
    // floor as E(4,16). The lanes interlock, and each rotates its start offset
    // by a per-4-bar-phrase amount so the composite pattern slowly precesses —
    // the hypnotic creep. Patterns are recomputed per bar with the current rot.
    int E_KICK[16], E_CHAT[16], E_OHAT[16], E_BLIP[16], E_RIM[16], E_SHAK[16], E_RIDE[16], E_CBASS[16];

    for (int s = 0; s < 6; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        for (int b = 0; b < nb; b++) {
            int bar = c + b;
            int blk = b % 8;                          // position in the 8-bar block
            int lastInBlk = (blk == 7);               // the one bar that pulls the root
            int fill = (b % 4 == 3);                  // gentle 4-bar punctuation
            // microscopic evolution: a global phrase index drives the acid creep.
            int gphr = (START[s] + b) / 2;            // changes every 2 bars
            int phr4 = (START[s] + b) / 4;            // 4-bar phrase index → rotation clock

            // Recompute Euclidean lanes for this bar with precessing offsets.
            // The per-section precession rate PREC[s] is the EXPERIMENTAL
            // VARIABLE: each lane rotates pr*(prime weight) steps per 4-bar
            // phrase. PREC=0 freezes the necklaces (a static interlock); larger
            // PREC drives the composite to drift apart and reconverge.
            int pr = PREC[s];
            bjorklund(4, 16, 0,                E_KICK);   // E(4,16) — four-floor anchor (never precesses)
            bjorklund(7, 16, phr4 * pr * 1,    E_CHAT);   // E(7,16) — busy hat tresillo
            bjorklund(3,  8, phr4 * pr * 1,    E_OHAT);   // E(3,8) tiled to 16 (open-hat lift)
            bjorklund(5, 16, phr4 * pr * 2,    E_BLIP);   // E(5,16) — acid clave
            bjorklund(3, 16, -phr4 * pr,       E_RIM);    // E(3,16) — sparse rim, rotates backward
            bjorklund(9, 16, phr4 * pr * 3,    E_SHAK);   // E(9,16) — fine shaker grain
            bjorklund(2, 16, phr4 * pr + 4,    E_RIDE);   // E(2,16) — sparse ride ping (fixed phase)
            bjorklund(5, 16, phr4 * pr * 2 + 8,E_CBASS);  // E(5,16) — counter-bass, anti-phase to blip
            // tile E(3,8) across both halves of the bar:
            for (int i = 8; i < 16; i++) E_OHAT[i] = E_OHAT[i - 8];

            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // kick — Euclidean four-floor, rock solid (drop only the last beat
                // of the final bar of an 8-bar block for one tiny breath).
                if ((m & L_KICK) && E_KICK[st] && !(lastInBlk && st == 12)) kick(t, 0.96);
                // hats — E(7,16) tresillo-ish ride + sparse 16th ghosts for shimmer
                if ((m & L_CHAT) && E_CHAT[st]) hat(t, 0.42 + 0.08 * (st % 4 == 2), 0, rnd2() * 0.28);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.10) hat(t, 0.16, 0, rnd2() * 0.4);
                if ((m & L_OHAT) && E_OHAT[st] && st >= 8) hat(t, 0.34, 1, 0.18);
                // clap on 2 & 4
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.66);
                // rim/clave — dry Euclidean accent skeleton, panned by step
                if ((m & L_RIM) && E_RIM[st]) rim(t, 0.5, (st % 16 < 8 ? -0.35 : 0.35));
                // shaker — E(9,16) fine grain, soft and air-filling
                if ((m & L_SHAK) && E_SHAK[st]) shaker(t, 0.4 + 0.2 * (st & 1), rnd2() * 0.5);
                // ride ping — sparse high shimmer with reverb
                if ((m & L_RIDE) && E_RIDE[st]) ride(t, 0.5, rnd2() * 0.3);
                // rolling sub hook — root A nearly always; pull low on the last bar
                if ((m & L_SUB) && P_SUB[st] == 'x') {
                    int root = lastInBlk ? ROOT_LO[(b / 8) % 2] : ROOT_A;
                    sub(root, t, STEP * 1.05, 0.9);
                }
                // counter-bass — Euclidean E(5,16) lane, syncopated melodic weave
                if ((m & L_CBASS) && E_CBASS[st]) {
                    int idx = (((st >> 1) + gphr / 3) % 5 + 5) % 5;
                    cbass(CBASS_SCALE[idx], t, STEP * 1.3, 0.34, rnd2() * 0.4);
                }
                // acid blip — Euclidean E(5,16) cell; note steps one scale degree
                // every 4 phrases, octave-lifts in the back half for trance lift.
                if ((m & L_BLIP) && E_BLIP[st]) {
                    int idx = (((st >> 1) + gphr / 4) % 5 + 5) % 5;
                    int note = BLIP_SCALE[idx] + (s >= 3 ? 12 : 0);
                    // microscopic timbral creep: resonance climbs, cutoff opens over time
                    double prog = (double)(START[s] + b) / (double)TB; // 0..1
                    double res = 3.2 + 3.0 * prog;
                    double cut1 = 320 + 1100 * prog;
                    blip(note, t, STEP * 1.7, 0.46, res, 2400 + 1800 * prog, cut1, rnd2() * 0.45);
                }
            }
            // tom fill — a quick Euclidean-spaced descending tom run into phrase ends
            if (fill && lastInBlk && (s == 1 || s == 2 || s == 4)) {
                int Tf[16]; bjorklund(5, 16, 0, Tf);
                int tn[5] = { 50, 48, 45, 43, 40 }, ti = 0;
                for (int st = 8; st < 16; st++) if (Tf[st]) tom(tn[ti++ % 5], step_t(bar, st), 0.5, rnd2() * 0.5);
            }
            // drone — low sustained atmosphere, one per 8-bar block
            if ((m & L_DRONE) && blk == 0) drone(ROOT_A + 12, bar * BAR, BAR * 8 - 0.05, 0.07);
            // pad chord — one per 8-bar block, long and breathing
            if ((m & L_PAD) && blk == 0) { const int *ch = PAD_CH[(b / 8) % 2]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 8 - 0.05, 0.085, (z - 1) * 0.42); }
            // a single riser into the two acid sections — the only real fills
            if (fill && (s == 1 || s == 3) && blk == 7) riser(bar * BAR, BAR, 0.15);
        }
    }

    // ── tempo-synced ping-pong delay on the music send (smears the acid) ──
    // an 8th-note feedback delay applied to the reverb send before reverb, so the
    // blips trail into a hypnotic haze. drums are dry (only music feeds revL/R).
    {
        long dly = (long)((BEAT / 2.0) * SR);          // 8th-note delay
        double fb = 0.42, wet = 0.5;
        if (dly > 0 && dly < N) {
            for (long i = dly; i < N; i++) {
                revL[i] += (float)(revR[i - dly] * fb);  // ping-pong: cross channels
                revR[i] += (float)(revL[i - dly] * fb);
            }
            // fold a wet copy back into the music bus so the delay is audible
            for (long i = dly; i < N; i++) {
                musL[i] += (float)(revR[i - dly] * wet * 0.4);
                musR[i] += (float)(revL[i - dly] * wet * 0.4);
            }
        }
    }

    // ── sidechain: kick-triggered duck on the music bus (deep, hypnotic pump) ──
    { double depth = 0.72, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── wetter Schroeder reverb on the send (drums stay dry-ish) ──
    {
        double decay = 0.78, wet = 0.4, damp = 0.42;
        int CD[4]; double cds[4] = { 0.0277, 0.0331, 0.0411, 0.0473 };
        for (int k = 0; k < 4; k++) CD[k] = (int)(cds[k] * SR);
        float *cbL[4], *cbR[4]; int ciL[4] = {0}, ciR[4] = {0}; double lpL[4] = {0}, lpR[4] = {0};
        for (int k = 0; k < 4; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 4; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                lpL[k] = dL * (1 - damp) + lpL[k] * damp; lpR[k] = dR * (1 - damp) + lpR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + lpL[k] * decay); cbR[k][ciR[k]] = (float)(inR + lpR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            musL[i] += (float)(cL / 4 * wet); musR[i] += (float)(cR / 4 * wet);
        }
        for (int k = 0; k < 4; k++) { free(cbL[k]); free(cbR[k]); }
    }

    // ── mix buses, normalize, short fades ──
    float *busL = drumL, *busR = drumR;             // reuse drum bus as the mix
    for (long i = 0; i < N; i++) { busL[i] += musL[i]; busR[i] += musR[i]; }
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.89 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(0.4 * SR), fout = (long)(1.8 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
