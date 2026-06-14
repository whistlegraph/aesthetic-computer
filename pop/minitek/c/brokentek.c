// brokentek.c — broken / shuffled minimal techno in C, treated as a polymeter
// apparatus. Super-fast (144 BPM) and jacking: the kick is NOT four-on-the-floor
// — it's syncopated, with ghost kicks and deliberate gaps, ridden by heavy
// shuffle swing (~0.60). Tribal, busy hats and claps carry the groove. A
// rumbling reese sub and a stuttered, retriggered resonant "blip" stab sit on a
// hard-ducked music bus (the pump breathes deep).
//
// THESIS DEEPENING (polymeter). Three perc ostinato lanes run on cycle lengths
// COPRIME to the 16-step bar — 3, 5, 7 — riding a single never-reset global step
// counter, so they precess against the 4/4 floor and against each other. The
// engine computes and prints (a) each lane length, (b) the pairwise lane×bar LCMs
// (48/80/112 steps), (c) the three-lane mutual coincidence period LCM(3,5,7)=105
// steps, and (d) the FULL realignment period LCM(3,5,7,16)=1680 steps = 105 bars
// — longer than the track, so the listener never hears the seam. A "land" section
// is timed to end on the half-realignment coincidence (gstep 840, bar 52) and is
// marked with a crash. Two clean hemiola voices are added: a 3:2 (three over two
// beats) and a 3:4 (three over the bar) layer, so notated 4/4 and several
// perceived meters coexist. The engine also prints a metric-weight (Pressing
// cognitive-cost) syncopation score for the kick patterns.
//
// Stabs run through a tempo-synced delay into a wet reverb. Reharmonized to G
// minor with its own roots, scale and chords. The two-bus + sidechain + reverb +
// normalize/fade infrastructure is unchanged from the proven minitek engine.
// render.mjs --engine brokentek masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o brokentek brokentek.c -lm
// Run:    ./brokentek --out out/brokentek-raw.wav

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
static double BPMV = 144, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.60;                   // heavy shuffle swing on odd 16ths

static uint32_t rng_s = 0x62726f6b; // "brok"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

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
// kick — pitch sweeps 124→46 Hz fast, snappy body, hard click; tanh weight.
// Stamps the sidechain trigger (the pump depends on this — keep it). `g` lets
// ghost kicks come in much quieter for the broken feel.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.32 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = (float)fmax(trig[s0], g); // ghost ducks less
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 46 + 78 * exp(-tt * 46.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 9.5);
        double click = exp(-tt * 420.0) * 0.85;
        double v = tanh((sin(ph) + click) * 2.1) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.12 : 0.035) * SR);
    double dec = open ? 46.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.45;
        addD(s0 + i, v * lg, v * rg);
    }
}

// clap — three quick noise spits then a short diffuse tail; bandpass-ish.
static void clap(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.20 * SR); double prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double spits = exp(-fmod(tt, 0.010) * 600.0) * (tt < 0.028 ? 1.0 : 0.0);
        double tail = exp(-tt * 16.0);
        double v = hp * (spits * 0.8 + tail) * g * 0.5;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.28, v * 0.28);             // a little verb on the clap
    }
}

// tom — short pitched tribal drum (membrane sine with a quick downsweep), the
// jacking tribal accent. Panned for width.
static void tom(double t, double g, double f0, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.16 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f0 * (0.7 + 0.6 * exp(-tt * 30.0));
        ph += TAU * pf / SR;
        double v = tanh(sin(ph) * 1.4) * exp(-tt * 14.0) * g * 0.6;
        addD(s0 + i, v * lg, v * rg);
    }
}

// rim — a tight clicky woodblock/rimshot: a high pitched ping + a noise tick.
// Drives the coprime ostinato lanes (dry, panned, very short).
static void rim(double t, double g, double f0, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double ph = 0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * f0 / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (sin(ph) * 0.6 + hp * 0.5) * exp(-tt * 130.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// shaker — short noise burst with a soft attack, granular tail; rides the
// secondary coprime lane for a continuously precessing texture.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0, prev2 = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double bp = hp - prev2; prev2 = hp;             // crude double-diff bandpass
        double env = (tt < 0.006 ? tt / 0.006 : exp(-(tt - 0.006) * 55.0));
        double v = bp * env * g * 0.4;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride — a metallic shimmer (cluster of inharmonic partials) with a long tail,
// for the 7-step coprime lane in the later grooves.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    static const double pr[6] = { 2400, 3170, 4100, 5300, 6850, 8200 };
    double ph[6] = {0,0,0,0,0,0};
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, s = 0;
        for (int k = 0; k < 6; k++) { ph[k] += TAU * pr[k] / SR; s += sin(ph[k]); }
        double v = (s / 6.0) * exp(-tt * 11.0) * g * 0.4;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.2, v * 0.2);
    }
}

// crash — a bright noise-plus-inharmonic-cluster splash with a long tail, struck
// once to MARK the half-realignment coincidence (the structural landmark where
// the three coprime lanes all coincide together at gstep 840 / bar 52).
static void crash(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(1.4 * SR); double prev = 0;
    static const double pr[5] = { 3300, 4700, 6100, 7900, 9700 };
    double ph[5] = {0,0,0,0,0};
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2(), hp = nz - prev; prev = nz;
        double s = 0;
        for (int k = 0; k < 5; k++) { ph[k] += TAU * pr[k] / SR; s += sin(ph[k]); }
        double v = (hp * 0.6 + (s / 5.0) * 0.5) * exp(-tt * 3.2) * g * 0.5;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.4, v * 0.4);
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// reese — detuned twin-saw bass through a resonant lowpass: a rumbling,
// growling sub-bass core for the broken groove. tanh-saturated.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.006 * SR), rel = (long)(0.04 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0;
    double fc = 2.0 * sin(M_PI * 420.0 / SR), q = 1.0 / 0.7;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.008 / SR; if (s2 >= 1) s2 -= 1;     // detune → reese beating
        double in = ((2 * s1 - 1) + (2 * s2 - 1)) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((low + 0.5 * sin(TAU * 0 + 0)) * 1.5 + 0.6 * sin(TAU * f * (double)i / SR)) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — a saw through a resonant state-variable lowpass with a fast cutoff
// envelope: the 303-ish acid plink. `res` 1..6, `cut0/cut1` Hz sweep.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.002 * SR), rel = (long)(0.02 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;        // naive saw ramp 0..1
        double in = 2.0 * saw - 1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 18.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.22, v * 0.22);
    }
}

// stutter — fire a blip several times in quick succession (a ratchet/retrigger
// stab) for the broken, glitchy feel. `reps` slices `dur` evenly.
static void stutter(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan, int reps) {
    if (reps < 1) reps = 1; double sl = dur / reps;
    for (int r = 0; r < reps; r++) {
        double gg = g * (0.6 + 0.4 * ((double)(reps - r) / reps));
        blip(note + (r == reps - 1 ? 0 : 0), t + r * sl, sl * 0.92, gg, res, cut0, cut1, pan);
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.25 * SR), rel = (long)(0.4 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1300.0 / SR), q = 1.0 / 0.9;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.006 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.34, v * 0.34);
    }
}

// pluck — a short triangle-ish counter-melody plink with a fast filter decay,
// sits above the reese as a melodic counter-line (ducked music bus).
static void pluck(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, low = 0, band = 0, q = 1.0 / 1.1, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.003 * SR), rel = (long)(0.03 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += f / SR; if (ph >= 1) ph -= 1;
        double tri = 2.0 * fabs(2.0 * ph - 1.0) - 1.0;   // triangle
        double cut = 600 + 2600 * exp(-tt * 22.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = tri - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.25, v * 0.25);
    }
}

// noise riser — filtered white-noise crescendo for transitions into a section.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / (dur);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (400 + 6000 * p) / SR), q = 1.0 / 1.3;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = (hp - low) * p * p * g;            // rising bandpass-ish
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64, L_TOM = 128,
       L_POLY = 256,    // coprime rim/shaker/ride ostinato lanes (3, 5, 7 vs 16)
       L_HEMI = 512,    // 3-against-2 hemiola tom/clap layer (3 over 2 beats)
       L_PLUCK = 1024,  // melodic counter-line
       L_HEMI4 = 2048 }; // 3-against-4 hemiola pluck layer (3 over the whole bar)

// at 144 BPM a bar is ~1.667s; 68 bars ≈ 116.7s + 3s tail ≈ 120s. SECBARS drives
// the length. The form is a polymeter EXPERIMENT, each section adding one cross-
// rhythm stratum: intro/build expose the coprime perc lanes alone (3,5,7 vs 16);
// grvA adds the 3:2 hemiola; grvB keeps it; grvC (starting on bar 52 — the
// HALF-REALIGNMENT coincidence gstep 840) adds the 3:4 hemiola so all strata
// stack at the structural landmark. Full LCM realignment (bar 105) lies beyond
// the track on purpose: the seam is never heard.
static const char *ORDER[8] = { "intro", "build", "grvA", "brk1", "grvB", "brk2", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 4, 16, 4, 12, 4 };
static int MASK[8] = {
    /*intro*/ L_KICK | L_CHAT | L_TOM | L_POLY,
    /*build*/ L_KICK | L_CHAT | L_OHAT | L_TOM | L_SUB | L_POLY,
    /*grvA */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_TOM | L_SUB | L_BLIP | L_POLY | L_HEMI,
    /*brk1 */ L_SUB | L_BLIP | L_PAD | L_CHAT | L_POLY | L_PLUCK,
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_TOM | L_SUB | L_BLIP | L_PAD | L_POLY | L_HEMI | L_PLUCK,
    /*brk2 */ L_PAD | L_SUB | L_BLIP | L_POLY | L_PLUCK,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_TOM | L_SUB | L_BLIP | L_PAD | L_POLY | L_HEMI | L_HEMI4 | L_PLUCK,
    /*outro*/ L_KICK | L_CHAT | L_SUB | L_POLY,
};
static int START[8];

// swung absolute time of (bar, 16th-step). Heavy shuffle on odd 16ths.
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark natural-minor harmony in G: bass roots walk i–VI–III–VII per 4 bars.
static const int ROOTS[4] = { 31, 27, 34, 29 };          // G1, Eb1, Bb1, F1
static const int BLIP_SCALE[7] = { 55, 58, 60, 62, 63, 65, 67 }; // G3 Bb3 C4 D4 Eb4 F4 G4
static const int PAD_CH[4][3] = { {43,46,50}, {39,43,46}, {46,50,53}, {41,45,48} }; // Gm Eb Bb F (low)
// pitches the precessing ride/pluck lanes walk through (G natural minor).
static const int POLY_SCALE[5] = { 67, 70, 72, 74, 75 }; // G4 Bb4 C5 D5 Eb5
static const int PLUCK_SCALE[6] = { 67, 70, 72, 74, 75, 79 }; // G4 Bb4 C5 D5 Eb5 G5

// ── polymeter analysis (printed at render time so the thesis can cite it) ─────
static long igcd(long a, long b) { while (b) { long t = a % b; a = b; b = t; } return a; }
static long ilcm(long a, long b) { return a / igcd(a, b) * b; }

// Metric-weight (cognitive-cost) syncopation after Pressing (1997) / the
// Longuet-Higgins & Lee (1984) metric hierarchy: weight each 16th by how high in
// the binary metric tree it sits (bar > beat > 8th > 16th). A note that lands on
// a weak position whose following stronger position is silent is "syncopated";
// the cost is the weight difference. Higher total = more broken/syncopated. For a
// 16-step bar the tree weights are: step0 = 4 (downbeat), beats 4/8/12 = 3,
// 8th-offbeats 2/6/10/14 = 1, the odd 16ths = 0.
static int metric_weight(int step) {
    if (step % 16 == 0) return 4;          // bar / downbeat
    if (step % 4 == 0)  return 3;          // beat
    if (step % 2 == 0)  return 1;          // 8th offbeat
    return 0;                              // 16th
}
// Sum of per-onset syncopation costs over a 16-step 0/1 pattern (cyclic).
static int syncopation_score(const char *p) {
    int score = 0;
    for (int i = 0; i < 16; i++) {
        if (p[i] != 'x') continue;
        // find the next position; if it is a rest with higher metric weight, the
        // onset "displaces" it → cost = weight(next) − weight(onset).
        int j = (i + 1) % 16;
        if (p[j] != 'x') {
            int d = metric_weight(j) - metric_weight(i);
            if (d > 0) score += d;
        }
    }
    return score;
}

int main(int argc, char **argv) {
    const char *out_path = "out/brokentek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# brokentek.c · %g BPM · %d bars · %.1fs · broken/shuffled minimal techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest). Broken & busy.
    // kick — syncopated, NOT four-floor: downbeat anchor + offbeat/pushed hits + gaps.
    const char *P_KICK = "x.....x..x.x....";   // broken: 1, the 'and' of 2, push into 3, a stutter
    const char *P_KICK2= "x...x....x...x.x.";   // alt bar variant for the broken feel
    const char *P_GHST = "...g....g...g..g.";   // ghost kicks (quiet) filling the gaps
    const char *P_CHAT = "x.xx.x.xxx.x.xx.";   // busy tribal closed hats
    const char *P_OHAT = "..x.....x...x..x";   // open hat off-grid splash
    const char *P_SUB  = "x..xx.x..x.xx.x.";   // rolling broken reese
    const char *P_BLIP = "..x..xx...x.x.x.";   // stuttery acid figure
    const char *P_TOM  = "....x......x....";   // tribal tom accents

    // ── EXPERIMENT 1: polymeter realignment arithmetic ───────────────────────
    // Three perc lanes on cycle lengths coprime to the 16-step bar: 3, 5, 7.
    long C1 = 3, C2 = 5, C3 = 7, GRID = 16;
    long Lp1 = ilcm(C1, GRID), Lp2 = ilcm(C2, GRID), Lp3 = ilcm(C3, GRID);
    long L3 = ilcm(ilcm(C1, C2), C3);            // three lanes coincide with each other
    long LFULL = ilcm(L3, GRID);                  // full realignment (lanes + bar grid)
    long landGstep = LFULL / 2;                   // half-realignment landmark
    int  landBar = (int)(landGstep / GRID), landStep = (int)(landGstep % GRID);
    fprintf(stderr, "# EXPERIMENT 1 — polymeter realignment\n");
    fprintf(stderr, "#   coprime lane lengths vs %ld-grid: %ld, %ld, %ld  (gcd with grid all = %ld)\n",
            GRID, C1, C2, C3, igcd(C1, GRID));
    fprintf(stderr, "#   lane×bar LCMs: %ld vs16=%ld steps (%ld bars) | %ld vs16=%ld (%ld) | %ld vs16=%ld (%ld)\n",
            C1, Lp1, Lp1 / GRID, C2, Lp2, Lp2 / GRID, C3, Lp3, Lp3 / GRID);
    fprintf(stderr, "#   three-lane mutual coincidence period LCM(3,5,7) = %ld steps (%.4f bars)\n", L3, (double)L3 / GRID);
    fprintf(stderr, "#   FULL realignment LCM(3,5,7,16) = %ld steps = %ld bars = %.1fs (BEYOND the %d-bar track → seam never heard)\n",
            LFULL, LFULL / GRID, (double)LFULL / GRID * (BAR), TB);
    fprintf(stderr, "#   half-realignment landmark (crash) at gstep %ld = bar %d step %d  [grvC downbeat: bar %d]\n",
            landGstep, landBar, landStep, START[6]);
    // perceived vs notated: list the perceived pulse period of each cross-rhythm.
    fprintf(stderr, "#   perceived pulse periods (16ths): lane3=%ld, lane5=%ld, lane7=%ld, hemiola3:2=%d, hemiola3:4=%.3f; notated beat=4\n",
            C1, C2, C3, 8 / 3 /*≈2.67 placed on 0,3,5*/, 16.0 / 3.0);

    // ── EXPERIMENT 2: metric-weight syncopation of the kick patterns ──────────
    int skA = syncopation_score(P_KICK), skB = syncopation_score(P_KICK2);
    int skFloor = syncopation_score("x...x...x...x..."); // four-on-the-floor control = 0
    fprintf(stderr, "# EXPERIMENT 2 — metric-weight (Pressing) syncopation score\n");
    fprintf(stderr, "#   four-on-the-floor control = %d (none)\n", skFloor);
    fprintf(stderr, "#   P_KICK  '%s' = %d | P_KICK2 '%s' = %d | mean broken = %.1f\n",
            P_KICK, skA, P_KICK2, skB, (skA + skB) / 2.0);

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            const char *kp = (b & 1) ? P_KICK2 : P_KICK; // alternate broken kick bars
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // ── POLYMETER / CROSS-RHYTHM (Toussaint; African cross-rhythm) ──
                // Global, never-reset 16th-step counter. Three ostinato lanes run
                // on cycle lengths COPRIME to 16 (3, 5, 7), so they precess against
                // the 4/4 kick and only realign with the bar grid at the LCM:
                //   3 vs 16  → realign every 48 steps (3 bars)
                //   5 vs 16  → realign every 80 steps (5 bars)
                //   7 vs 16  → realign every 112 steps (7 bars)
                // all three + the bar coincide at LCM(3,5,7,16)=1680 steps (105 bars).
                long gstep = (long)bar * 16 + st;        // continuous step index
                if (m & L_POLY) {
                    // 3-step rim ostinato: accent on phase 0 of the triple cycle.
                    if (gstep % 3 == 0) rim(t, 0.55, midi_hz(POLY_SCALE[(int)((gstep / 3) % 5)] + 12), 0.55);
                    // 5-step shaker ostinato: phases independently against the 3-lane.
                    if (gstep % 5 == 0) shaker(t, 0.6, -0.5);
                    // 7-step ride bell ostinato (sparser; later/full sections only).
                    if ((s >= 2) && gstep % 7 == 0) ride(t, 0.5 + 0.15 * (s >= 4), 0.35);
                }
                // ── 3-against-2 HEMIOLA tom/clap layer (3 over 2 beats) ──
                // The 4/4 half-beat pulse is "2"; we lay a triple grouping over it.
                // Three evenly-spaced hits across two beats (= 8 sixteenths): ideal
                // positions 0, 8/3≈2.67, 16/3≈5.33 within each beat-pair. We place
                // them on the nearest 16th (0, 3, 5) within each 8-step window to
                // ride the swing — a perceived dotted-quarter pulse over the beat.
                if (m & L_HEMI) {
                    int w = st % 8;                      // position in the 2-beat window
                    int half = (st / 8) & 1;             // which half of the bar
                    if (w == 0) tom(t, 0.5, midi_hz(root + 19), 0.4 * (half ? 1 : -1));
                    else if (w == 3) clap(t, 0.34);
                    else if (w == 5) tom(t, 0.45, midi_hz(root + 22), 0.4 * (half ? -1 : 1));
                }
                // ── 3-against-4 HEMIOLA pluck layer (3 over the WHOLE bar) ──
                // Three evenly-spaced hits across the 16-step bar: ideal positions
                // 0, 16/3≈5.33, 32/3≈10.67 → nearest 16ths 0, 5, 11. This is a
                // slower triplet that fights the bar itself (a perceived 3/4 over
                // notated 4/4). Introduced only in grvC, atop the 3:2, so the
                // listener hears the cross-rhythm stack thicken at the landmark.
                if (m & L_HEMI4) {
                    if (st == 0 || st == 5 || st == 11) {
                        int h = (st == 0) ? 0 : (st == 5) ? 1 : 2;
                        pluck(PLUCK_SCALE[(h + b) % 6] + 12, t, STEP * 2.4, 0.26, (h - 1) * 0.5);
                    }
                }
                // ── structural landmark: crash on the half-realignment coincidence
                if (gstep == landGstep) crash(t, 0.85);
                // kick — broken pattern; drop a hit on fill bars for an extra gap
                if ((m & L_KICK) && kp[st] == 'x' && !(fill && st == 9)) kick(t, 0.97);
                // ghost kicks — quiet syncopated fills (skip on fill bars)
                if ((m & L_KICK) && P_GHST[st] == 'g' && !fill && rnd() < 0.7) kick(t, 0.30);
                // hats — busy tribal
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.42 + 0.12 * (st % 4 == 0), 0, rnd2() * 0.4);
                if ((m & L_CHAT) && P_CHAT[st] != 'x' && rnd() < 0.22) hat(t, 0.18, 0, rnd2() * 0.5); // 16th ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.4, 1, rnd2() * 0.4);
                // clap — broken, on the 'and' of 2 and on 4-ish, plus a flam
                if ((m & L_CLAP) && (st == 6 || st == 14)) clap(t, 0.7);
                if ((m & L_CLAP) && st == 14 && !fill) clap(t + STEP * 0.18, 0.32); // flam
                // tribal toms
                if ((m & L_TOM) && P_TOM[st] == 'x') tom(t, 0.7, midi_hz(root + 24) , rnd2() * 0.6);
                if ((m & L_TOM) && fill && (st == 13 || st == 15)) tom(t, 0.6, midi_hz(root + 26), rnd2() * 0.7);
                // reese sub bass
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root, t, STEP * 1.5, 0.78);
                // acid blip — stutters/retriggers for the broken feel
                if ((m & L_BLIP) && P_BLIP[st] == 'x') {
                    int note = BLIP_SCALE[(st + b * 3) % 7] + (phr >= 2 ? 12 : 0);
                    int reps = (fill && (st == 12 || st == 14)) ? 4 : (rnd() < 0.25 ? 2 : 1);
                    stutter(note, t, STEP * 1.7, 0.46, 4.2 + 1.3 * (s >= 4), 2900, 360, rnd2() * 0.6, reps);
                }
                // pluck counter-melody — itself a coprime (5-step) cross-rhythm so
                // the melodic line precesses against the bar like the perc lanes.
                if ((m & L_PLUCK) && gstep % 5 == 2) {
                    int idx = (int)((gstep / 5) % 6);
                    pluck(PLUCK_SCALE[idx], t, STEP * 2.2, 0.30, rnd2() * 0.5);
                }
            }
            // pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.10, (z - 1) * 0.45); }
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.16);
        }
    }

    // ── tempo-synced delay on the stab/reverb send (dotted-8th feedback) ──
    { long dly = (long)(STEP * 3.0 * SR); double fb = 0.42, wet = 0.5;
      for (long i = dly; i < N; i++) { double el = revL[i - dly] * fb, er = revR[i - dly] * fb;
        revL[i] += (float)(el * wet); revR[i] += (float)(er * wet); } }

    // ── sidechain: kick-triggered duck on the music bus (deep broken pump) ──
    { double depth = 0.78, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = fmax(env, trig[i]); else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── light Schroeder reverb on the send (drums stay dry-ish) ──
    {
        double decay = 0.78, wet = 0.40, damp = 0.42;
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
    long fin = (long)(0.4 * SR), fout = (long)(1.6 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
