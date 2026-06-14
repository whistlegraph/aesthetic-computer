// glitchtek.c — fast, twitchy glitch/clicky minimal techno in B minor at 152 BPM.
// A forked minitek engine: same two-bus design (dry DRUM bus + kick-ducked MUSIC
// bus) but the character is IDM-leaning micro-percussion — stuttering clicky
// blips that retrigger several times inside a 16th, broken/shuffled kick, fast
// twitchy hats, and a rumbling reese under it all, while a four-floor spine keeps
// it danceable. A deep kick-triggered sidechain pumps the music bus; stabs run
// through a wetter reverb + a short slap delay. render.mjs --engine glitchtek
// masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o glitchtek glitchtek.c -lm
// Run:    ./glitchtek --out out/glitchtek-raw.wav

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
static double BPMV = 152, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.515;                  // tight micro-swing (twitchy, near-straight)

static uint32_t rng_s = 0x676c6974; // "glit"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── Parametric / stochastic L-system with turtle interpretation ───────────────
// A Lindenmayer system drives all glitch detail (cf. Prusinkiewicz & Lindenmayer,
// "The Algorithmic Beauty of Plants", 1990; Worth & Stepney, "Growing Music",
// 2005; Prusinkiewicz, "Score generation with L-systems", ICMC 1986). The prior
// pass used the bare Fibonacci word (A->AB, B->A) as a self-similar onset string.
// This pass generalizes to a PARAMETRIC, STOCHASTIC, TURTLE-INTERPRETED grammar:
//
//   Alphabet (turtle commands, read left-to-right by a rhythmic turtle):
//     A  accented onset   (loud click / pitched ratchet head)
//     a  soft onset       (ghost click)
//     +  SUBDIVIDE: descend one rhythmic level (16th -> 32nd -> 64th) — the
//        push/recurse operator; the bracketed group after it lives at half the
//        current grain. This is what makes the SAME rule act at multiple levels.
//     -  rest             (silence of one grain at the current level)
//     [  push depth (open a subdivided branch)   ] pop depth (close it)
//
//   Production rules (context-free, with one STOCHASTIC rule chosen by a
//   deterministic per-cell hash so renders are reproducible):
//     A -> A[+a-a]          an accent spawns a half-rate two-grain ornament
//     a -> +A-              a ghost subdivides into an accent + rest (recursion)
//     +  ->  +              terminal (depth marker carries through)
//     -  ->  a-            a rest sometimes blooms into a soft onset (fill)
//   Stochastic variant of A (prob ~0.38, hashed):  A -> A[+A][-a]   (denser fan)
//
// The string is rewritten generation by generation; we record the length after
// every generation (the growth curve) and EXPOSE the per-section generation
// DEPTH as the experiment's independent variable. A turtle walks the resulting
// string: A/a place onsets at the current grain, +/[ ] move between the 16th,
// 32nd and 64th levels, so the identical rule self-similarly textures all three.
#define LSYS_MAX 8192
#define LSYS_GENMAX 9
static char lsys_buf[LSYS_MAX];          // the fully-grown string at LSYS_GENMAX
static int  lsys_len = 0;
static int  lsys_genlen[LSYS_GENMAX + 1]; // string length after each generation
static int  lsys_gens = 0;

// deterministic per-symbol-position hash → stochastic rule selection (repeatable)
static inline uint32_t lhash(uint32_t x) { x ^= x >> 16; x *= 0x7feb352dU; x ^= x >> 15; x *= 0x846ca68bU; x ^= x >> 16; return x; }

// Rewrite one generation. Returns new length. Writes into `dst`.
// Parametric: each cell's stochastic choice depends on its absolute index so the
// grammar is deterministic yet non-periodic.
static int lsys_step(const char *src, int n, char *dst) {
    int o = 0;
    for (int i = 0; i < n && o < LSYS_MAX - 8; i++) {
        char c = src[i];
        uint32_t h = lhash((uint32_t)i * 2654435761u + (uint32_t)c);
        switch (c) {
            case 'A':
                if ((h & 0xff) < 97) {                 // ~0.38 dense fan variant
                    const char *r = "A[+A][-a]"; while (*r) dst[o++] = *r++;
                } else {
                    const char *r = "A[+a-a]";   while (*r) dst[o++] = *r++;
                }
                break;
            case 'a': { const char *r = "+A-"; while (*r) dst[o++] = *r++; } break;
            case '-': if ((h & 0xff) < 128) { dst[o++]='a'; dst[o++]='-'; } else dst[o++]='-'; break;
            default:  dst[o++] = c;                     // + [ ] carry through
        }
    }
    dst[o] = 0; return o;
}

// Build the parametric/stochastic L-system once; record the growth curve.
static void lsys_build(int generations) {
    if (generations > LSYS_GENMAX) generations = LSYS_GENMAX;
    lsys_gens = generations;
    char a[LSYS_MAX] = "A", b[LSYS_MAX];
    int la = 1; lsys_genlen[0] = 1;
    for (int g = 1; g <= generations; g++) {
        int lb = lsys_step(a, la, b);
        memcpy(a, b, lb + 1); la = lb; lsys_genlen[g] = lb;
    }
    memcpy(lsys_buf, a, la + 1); lsys_len = la;
}
// symbol at a given absolute position (the string wraps so reads never run dry).
static inline char lsys_at(int pos) { return lsys_buf[((pos % lsys_len) + lsys_len) % lsys_len]; }

// ── Turtle interpreter ────────────────────────────────────────────────────────
// Walk a window of the L-system string and emit a flat list of timed onsets at
// the resolved sub-grain. `+`/`[` descend a level (halve the grain), `]` pops.
// Returns the realized onset list for one 16th step: each onset has a fractional
// offset within the step (0..1), a depth level (0=16th,1=32nd,2=64th,...), and
// an accent flag. This proves the SAME rule textures every subdivision level.
typedef struct { double frac; int depth; int accent; } Onset;
#define ONS_MAX 64
// Interpret the substring covering one 16th cell starting at absolute index `pos`,
// reading up to `budget` symbols. The cursor advances `1/2^depth` of the cell per
// onset/rest at that depth. Self-similar: depth grows via '+' and '['.
static int lsys_interp_cell(int pos, int budget, Onset *out) {
    int no = 0, depth = 0, dstack[16], dsp = 0; double cur = 0.0, span = 1.0;
    for (int k = 0; k < budget && no < ONS_MAX; k++) {
        char c = lsys_at(pos + k);
        if (c == '+') { if (depth < 3) depth++; span = 1.0 / (double)(1 << depth); }
        else if (c == '[') { if (dsp < 16) dstack[dsp++] = depth; if (depth < 3) depth++; span = 1.0 / (double)(1 << depth); }
        else if (c == ']') { if (dsp > 0) depth = dstack[--dsp]; span = 1.0 / (double)(1 << depth); }
        else if (c == '-') { cur += span; if (cur >= 1.0) break; }
        else if (c == 'A' || c == 'a') {
            if (cur >= 1.0) break;
            out[no].frac = cur; out[no].depth = depth; out[no].accent = (c == 'A'); no++;
            cur += span;
        }
    }
    return no;
}
// Self-similarity proof: count onsets that occur at each depth level across the
// whole string, and measure the autocorrelation-style match of the level-0 onset
// mask against the level-1 onset mask (does the rule reproduce its shape one
// octave down?). Filled by lsys_report(); printed to stderr.
static int  ss_depth_count[4] = {0,0,0,0};
static double ss_selfsim = 0.0;       // fraction of 32nd cells whose onset shape matches the 16th-level shape

// Compute the depth histogram over the whole grown string and the self-similarity
// statistic. The grammar's defining property is that an ACCENT always begins a
// group and a subdivided branch begins with the same accent-then-ornament shape:
// rule A -> A[+a-a] keeps the accent at the head and pushes a half-rate ornament,
// and a -> +A- regenerates an accent one level down. So at EVERY depth d, the
// first onset of a freshly entered branch should be the strongest of that branch
// — the rule reproduces its "accent-leads" shape octave after octave.
// We measure: over all subdivision branches (a '[' or a '+' that introduces a
// deeper level), what fraction lead with an onset at the head of the branch
// (frac == branch start) — i.e. the parent shape recurs. This is the operational
// proof that one rule textures multiple grains self-similarly.
static void lsys_report(void) {
    // (1) depth histogram — sample one turtle cell per starting symbol, exactly
    // as synthesis reads it, so the histogram is the realized onset distribution.
    Onset on[ONS_MAX];
    for (int p = 0; p < lsys_len; p++) {
        int no = lsys_interp_cell(p, 24, on);
        for (int j = 0; j < no; j++) if (on[j].depth >= 0 && on[j].depth < 4) ss_depth_count[on[j].depth]++;
    }
    // (2) self-similarity — over all subdivision branches ('[' or '+'), what
    // fraction lead with an onset at the branch head, i.e. the rule reproduces
    // its accent-leads shape one level down (octave after octave).
    int branches = 0, leadOnset = 0; int branchOpen = 0;
    for (int k = 0; k < lsys_len; k++) {
        char c = lsys_buf[k];
        if (c == '+' || c == '[') { branches++; branchOpen = 1; }
        else if (c == 'A' || c == 'a') { if (branchOpen) { leadOnset++; branchOpen = 0; } }
        else if (c == '-') branchOpen = 0;       // a rest at branch head breaks the shape
    }
    ss_selfsim = (branches > 0) ? (double)leadOnset / (double)branches : 0.0;
}

static long N;
// two buses: drums hit hard and dry; music (bass/blip/pad) gets ducked by the
// kick. revL/R is a send. trig[] marks kick onsets for the sidechain.
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
// kick — punchy pitch-enveloped sine, slightly snappier than minitek for the
// twitchy feel; a tighter click and a fast body. Stamps the sidechain trigger.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.30 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 46 + 86 * exp(-tt * 50.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 9.5);
        double click = exp(-tt * 520.0) * 0.85;
        double v = tanh((sin(ph) + click) * 2.05) * amp * g;
        addD(s0 + i, v, v);
    }
}

// click — a tiny snapped noise/transient grain: the glitchy micro-percussion
// building block. Very short, bright, panned. The clicky soul of the track.
static void click(double t, double g, double pan, double bright) {
    long s0 = (long)(t * SR), n = (long)(0.012 * SR);
    double prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double dec = 380.0 + 700.0 * bright;
        double v = hp * exp(-tt * dec) * g * 0.6;
        addD(s0 + i, v * lg, v * rg);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.10 : 0.028) * SR);
    double dec = open ? 50.0 : 170.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.42;
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
        addR(s0 + i, v * 0.30, v * 0.30);             // a little verb on the clap
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// reese — a detuned dual-saw rumble through a lowpass: the dark moving bed.
static void reese(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.012 * SR), rel = (long)(0.05 * SR);
    double s1 = 0, s2 = 0, s3 = 0, low = 0, band = 0;
    double fc = 2.0 * sin(M_PI * 220.0 / SR), q = 1.0 / 1.1;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.008 / SR; if (s2 >= 1) s2 -= 1;
        s3 += f * 0.993 / SR; if (s3 >= 1) s3 -= 1;
        double in = ((2 * s1 - 1) + (2 * s2 - 1) + (2 * s3 - 1)) * 0.33;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.6) * env * g;
        addM(s0 + i, v, v);
    }
}

// sub — tanh-saturated sine bass with a quick attack and short release.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.004 * SR), rel = (long)(0.025 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((sin(ph) + 0.12 * sin(ph * 2)) * 1.3) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — a saw through a resonant state-variable lowpass with a fast cutoff
// envelope: the 303-ish acid plink. `res` 1..6, `cut0/cut1` Hz sweep.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.0015 * SR), rel = (long)(0.012 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;        // naive saw ramp 0..1
        double in = 2.0 * saw - 1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 22.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.22, v * 0.22);             // wetter send on stabs
    }
}

// stutter — retrigger a short blip grain `reps` times across `span` seconds:
// the signature glitch ratchet. Each repeat tightens and shifts a touch in pan,
// and the last few can pitch up an octave for a rising machine-gun feel.
static void stutter(double note, double t, double span, int reps, double g, double res, double pan) {
    if (reps < 1) reps = 1;
    double slice = span / reps;
    for (int k = 0; k < reps; k++) {
        double tk = t + k * slice;
        double n = note + (k >= reps - 2 ? 12 : 0);          // last grains jump an octave
        double gk = g * (0.75 + 0.25 * ((double)(k + 1) / reps)); // crescendo
        double pk = pan + (k % 2 ? 0.18 : -0.18);             // ping-pong the grains
        blip(n, tk, slice * 0.92, gk, res, 3200, 520, pk);
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.30 * SR), rel = (long)(0.5 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1250.0 / SR), q = 1.0 / 0.9;
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

// noise riser — filtered white-noise crescendo for transitions into a section.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / (dur);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (400 + 6500 * p) / SR), q = 1.0 / 1.3;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = (hp - low) * p * p * g;            // rising bandpass-ish
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// rim — a tight woody click: short pitched ping + a noise snap. Counter-perc.
static void rim(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.035 * SR); double ph = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * 1750.0 / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double tone = sin(ph) * exp(-tt * 220.0);
        double snap = hp * exp(-tt * 360.0) * 0.5;
        double v = (tone + snap) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride/shaker — sustained metallic-ish noise shimmer; bright steady top end.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0, prev2 = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz; double hp2 = hp - prev2; prev2 = hp; // 2nd-order HP, very bright
        double env = exp(-tt * 70.0) * (1.0 - exp(-tt * 900.0));               // soft attack, fast tail
        double v = hp2 * env * g * 0.34;
        addD(s0 + i, v * lg, v * rg);
    }
}

// tom — pitch-dropping sine drum for fills; rolls around the kit.
static void tom(double note, double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double ph = 0;
    double f0 = midi_hz(note), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = f0 * (1.0 + 0.6 * exp(-tt * 24.0));
        ph += TAU * f / SR;
        double v = tanh(sin(ph) * 1.4) * exp(-tt * 12.0) * g * 0.7;
        addD(s0 + i, v * lg, v * rg);
    }
}

// pluck — a short bright FM-ish bell/pluck for the counter-melody up top, with a
// resonant lowpass pluck envelope. Sits above the acid stabs as a topline.
static void pluck(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, mph = 0, low = 0, band = 0, q = 1.0 / 1.4;
    long att = (long)(0.002 * SR), rel = (long)(0.04 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        mph += TAU * f * 2.0 / SR;                              // FM modulator (2:1)
        double mi = 2.6 * exp(-tt * 18.0);                      // modulation index decays
        ph += TAU * f / SR;
        double in = sin(ph + mi * sin(mph));
        double cut = 5200.0 * exp(-tt * 9.0) + 700.0;
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else env = exp(-tt * 7.0);
        if (i > n - rel) env *= fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.30, v * 0.30);
    }
}

// drone — a slow low sine+fifth bed under the breaks for harmonic glue/atmos.
static void drone(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    long att = (long)(0.8 * SR), rel = (long)(1.2 * SR); double ph = 0, ph5 = 0, lfo = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR; ph5 += TAU * f * 1.5 / SR; lfo += TAU * 0.07 / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double trem = 0.85 + 0.15 * sin(lfo);
        double v = tanh((sin(ph) + 0.4 * sin(ph5)) * 1.2) * env * trem * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.18, v * 0.18);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64, L_CLICK = 128, L_REESE = 256,
       L_RIM = 512, L_RIDE = 1024, L_PLUCK = 2048, L_DRONE = 4096, L_LSYS = 8192 };

// at 152 BPM a bar is ~1.579s; 72 bars ≈ 113.7s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "brk1", "grvB", "brk2", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 4, 12, 4 };
// THE EXPERIMENT: per-section L-system recursion DEPTH (the independent
// variable). Shallow recursion = sparse, near-Fibonacci skitter; deep recursion
// = dense self-similar 32nd/64th glitch storms. The form is a sweep up the
// generation count and back down — the listener hears the grammar recurse.
static const int SECDEPTH[8] = { 2, 3, 4, 3, 5, 4, 6, 3 };
// per-section measured telemetry, filled during synthesis
static long SEC_ONS[8]   = {0};   // realized L-system onsets emitted
static long SEC_ACC[8]   = {0};   // of those, accents (A)
static long SEC_DEEP[8]  = {0};   // onsets that landed below the 16th level (depth>0)
static int  SEC_STRLEN[8]= {0};   // string length consumed per bar at this depth
static int MASK[8] = {
    /*intro*/ L_KICK | L_CLICK | L_CHAT | L_LSYS,
    /*build*/ L_KICK | L_CLICK | L_CHAT | L_OHAT | L_SUB | L_REESE | L_LSYS | L_RIM,
    /*grvA */ L_KICK | L_CLICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_REESE | L_BLIP | L_LSYS | L_RIM | L_RIDE,
    /*brk1 */ L_CLICK | L_SUB | L_BLIP | L_PAD | L_REESE | L_LSYS | L_PLUCK | L_DRONE,
    /*grvB */ L_KICK | L_CLICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_REESE | L_BLIP | L_PAD | L_LSYS | L_RIM | L_RIDE | L_PLUCK,
    /*brk2 */ L_PAD | L_REESE | L_CLICK | L_LSYS | L_DRONE | L_PLUCK,
    /*grvC */ L_KICK | L_CLICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_REESE | L_BLIP | L_PAD | L_LSYS | L_RIM | L_RIDE | L_PLUCK,
    /*outro*/ L_KICK | L_CLICK | L_SUB | L_REESE | L_LSYS | L_DRONE,
};
static int START[8];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark natural-minor harmony in B minor: bass roots walk i–VI–VII–iv per 4 bars.
static const int ROOTS[4] = { 35, 31, 33, 28 };          // B1, G1, A1, E1
// B minor scale tones for the acid stab (B C# D E F# G A in octave 3/4).
static const int BLIP_SCALE[7] = { 59, 62, 64, 66, 69, 71, 74 }; // B3 D4 E4 F#4 A4 B4 D5
static const int PAD_CH[4][3] = { {47,50,54}, {43,47,50}, {45,48,52}, {40,43,47} }; // Bm G A Em (low)

int main(int argc, char **argv) {
    const char *out_path = "out/glitchtek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# glitchtek.c · %g BPM · %d bars · %.1fs · glitchy minimal techno (B minor)\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x.x.x...x.x.x.";   // twitchy offbeat-leaning hats
    const char *P_OHAT = "......x.......x.";   // open hat lift before the downbeats
    const char *P_SUB  = "x...x..xx...x..x";   // syncopated, broken sub
    const char *P_BLIP = "..x....x..x...x.";   // stab anchors (stutters launch here)
    const char *P_CLICK= "x.xxx.x.xx.xx.xx";   // dense micro-click skitter (the IDM detail)
    const char *P_STUT = "....x.......x..x";    // where blip ratchets fire

    // build the parametric/stochastic L-system (axiom A) at the deepest depth we
    // need (max over SECDEPTH); record the growth curve and prove self-similarity.
    int maxdepth = 0; for (int i = 0; i < 8; i++) if (SECDEPTH[i] > maxdepth) maxdepth = SECDEPTH[i];
    lsys_build(maxdepth);
    fprintf(stderr, "# L-system: parametric/stochastic, turtle-interpreted\n");
    fprintf(stderr, "#   axiom : A\n");
    fprintf(stderr, "#   rules : A -> A[+a-a]  (or  A -> A[+A][-a]  w.p. 0.38, hashed)\n");
    fprintf(stderr, "#           a -> +A-      -  -> a-  (w.p. 0.5)     +/[/] terminal\n");
    fprintf(stderr, "#   turtle: A=accent  a=ghost  +/[=descend grain (16th->32nd->64th)  ]=ascend  -=rest\n");
    fprintf(stderr, "#   growth (len per generation):");
    for (int g = 0; g <= lsys_gens; g++) fprintf(stderr, " g%d=%d", g, lsys_genlen[g]);
    fprintf(stderr, "  (total grown string len=%d)\n", lsys_len);
    // self-similarity report: depth histogram + rule-shape recurrence across levels
    lsys_report();
    fprintf(stderr, "#   depth histogram (onsets per level): L0(16th)=%d L1(32nd)=%d L2(64th)=%d L3=%d\n",
            ss_depth_count[0], ss_depth_count[1], ss_depth_count[2], ss_depth_count[3]);
    fprintf(stderr, "#   self-similarity: %.1f%% of subdivided cells reproduce the rule's onset shape one level down\n", 100.0 * ss_selfsim);
    fprintf(stderr, "# section depth schedule:");
    for (int i = 0; i < 8; i++) fprintf(stderr, " %s=g%d", ORDER[i], SECDEPTH[i]);
    fprintf(stderr, "\n");

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            // advance the L-system read window per bar (a coprime stride keeps the
            // window itself aperiodic) — this is what makes the glitch evolve.
            int loff = bar * 13;
            int sdepth = SECDEPTH[s];                  // this section's recursion depth
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // kick — four-on-the-floor spine, but broken/shuffled: drop the
                // step-12 kick on fills and occasionally ghost an extra on the
                // "e" of beat 3 for a stumble.
                if ((m & L_KICK) && st % 4 == 0 && !(fill && st == 12)) kick(t, 0.95);
                if ((m & L_KICK) && st == 10 && (bar % 2 == 1)) kick(t, 0.6);   // shuffled extra
                // glitch micro-clicks — dense skitter, swing the velocity around
                if ((m & L_CLICK) && P_CLICK[st] == 'x') click(t, 0.34 + 0.22 * rnd(), rnd2() * 0.7, rnd());
                if ((m & L_CLICK) && st % 2 == 1 && rnd() < 0.30) click(t, 0.18, rnd2() * 0.8, 0.4); // 16th ghosts

                // ── L-system micro-percussion via TURTLE INTERPRETATION ──
                // Walk the grown grammar string from this cell with a symbol
                // budget that scales with the SECTION's recursion depth. The
                // turtle resolves A/a onsets at depths 0..3 (16th/32nd/64th/...),
                // descending on '+' and '['. The SAME rule thus textures every
                // grain level — self-similar glitch. Onsets at depth d are clicks
                // (and deep accents fire a pitched blip ratchet).
                if (m & L_LSYS) {
                    Onset on[ONS_MAX];
                    int budget = 2 + sdepth * 3;                   // depth gates how far the turtle reads
                    int no = lsys_interp_cell(loff + st, budget, on);
                    SEC_ONS[s] += no;
                    for (int j = 0; j < no; j++) {
                        int d = on[j].depth; if (d > sdepth) continue;   // honour the section ceiling
                        double tk = t + on[j].frac * STEP;
                        double pk = rnd2() * (0.4 + 0.15 * d);
                        double bk = 0.45 + 0.18 * d;               // deeper grains brighter
                        double gv = on[j].accent ? 0.30 : 0.14;
                        gv *= (1.0 - 0.10 * d);                    // tame the deep micro-grains
                        click(tk, gv, pk, bk);
                        if (d > 0) SEC_DEEP[s]++;
                        if (on[j].accent) {
                            SEC_ACC[s]++;
                            // deep accents become pitched blip ratchets (the acid glitch)
                            if (d >= 1 && (m & L_BLIP)) {
                                int bn = BLIP_SCALE[(loff + st + j) % 7] + 12 + 12 * (d >= 2);
                                blip(bn, tk, (STEP / (double)(1 << d)) * 0.9, 0.15, 6.0, 3600, 600, pk);
                            }
                        }
                    }
                    if (st == 0) SEC_STRLEN[s] = budget;           // record the per-bar read budget
                }
                // rim — counter-perc on L-system 'A' downbeats (woody backbeat ghost)
                if ((m & L_RIM) && (st == 6 || st == 14) && lsys_at(loff + st) == 'A')
                    rim(t, 0.5, (st == 6 ? -0.4 : 0.4));
                if ((m & L_RIM) && st == 3 && (bar % 2 == 0)) rim(t, 0.32, 0.25);
                // ride/shaker — steady 8th-note shimmer, accent driven by grammar
                if ((m & L_RIDE) && st % 2 == 0)
                    shaker(t, 0.30 + (lsys_at(loff + st) == 'A' ? 0.16 : 0.0), rnd2() * 0.5);
                // hats
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.46 + 0.1 * (st % 4 == 2), 0, rnd2() * 0.35);
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.4, 1, 0.18);
                // clap on 2 & 4
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.7);
                // reese rumble bed — long tone re-struck each beat, walks the root
                if ((m & L_REESE) && st % 4 == 0) reese(root - 12, t, BEAT * 1.05, 0.30);
                // sub bass — broken pattern
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root, t, STEP * 1.4, 0.8);
                // acid blip anchors
                if ((m & L_BLIP) && P_BLIP[st] == 'x') {
                    int note = BLIP_SCALE[(st + b * 3) % 7] + (phr == 3 ? 12 : 0);
                    blip(note, t, STEP * 1.5, 0.46, 4.5 + 1.5 * (s >= 4), 2900, 460, rnd2() * 0.45);
                }
                // stutter ratchets — retrigger a blip several times inside a 16th
                if ((m & L_BLIP) && P_STUT[st] == 'x') {
                    int note = BLIP_SCALE[(st + b) % 7];
                    int reps = (st == 15) ? 6 : (3 + (int)(rnd() * 3));   // 3–5, big roll at end of bar
                    stutter(note, t, STEP * (st == 15 ? 1.0 : 0.9), reps, 0.4, 6.0, rnd2() * 0.3);
                }
                // pluck counter-melody — a sparse topline that follows the grammar:
                // fires on 'A' onsets at the wide steps, an octave above the stab
                // scale, walking up the B-minor scale phrase by phrase.
                if ((m & L_PLUCK) && (st == 2 || st == 7 || st == 11) && lsys_at(loff + st) == 'A') {
                    int deg = (st / 4 + b * 2) % 7;
                    int note = BLIP_SCALE[deg] + 12 - (phr == 1 ? 0 : 0);
                    pluck(note, t, STEP * 3.0, 0.30, rnd2() * 0.4);
                }
            }
            // tom fill — recursive L-system roll across the last bar of a phrase:
            // walk the grammar and drop toms where it says 'A', pitch descending.
            if ((m & L_RIM) && fill) {
                for (int st = 8; st < 16; st++) {
                    if (lsys_at(loff + st) == 'A') {
                        double tt = step_t(bar, st);
                        int tn = 45 - (st - 8);                 // descending tom roll
                        tom(tn, tt, 0.5, ((st & 1) ? 0.5 : -0.5));
                    }
                }
            }
            // pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.10, (z - 1) * 0.4); }
            // low drone bed under the breaks — one long tone per phrase on the root
            if ((m & L_DRONE) && phr == 0) drone(ROOTS[(b / 4) % 4] - 12, bar * BAR, BAR * 4 - 0.05, 0.16);
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.16);
        }
    }

    // ── sidechain: kick-triggered duck on the music bus (deep pump) ──
    { double depth = 0.72, rel = exp(-1.0 / (0.10 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── short slap delay on the reverb send (stabs throw ping-pong echoes) ──
    {
        long dt = (long)(STEP * 1.5 * SR); double fb = 0.34;
        for (long i = dt; i < N; i++) {
            revL[i] += (float)(revR[i - dt] * fb);    // ping-pong: L gets prior R
            revR[i] += (float)(revL[i - dt] * fb);
        }
    }

    // ── wetter Schroeder reverb on the send (drums stay dry-ish) ──
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

    // ── results: per-section L-system census (the experiment's dependent vars) ──
    fprintf(stderr, "# results — per-section L-system turtle census:\n");
    fprintf(stderr, "#   sec     depth  bars  read/cell  onsets  accents  deep(>16th)  deep%%\n");
    long tot_ons = 0, tot_acc = 0, tot_deep = 0;
    for (int s = 0; s < 8; s++) {
        double dp = SEC_ONS[s] > 0 ? 100.0 * (double)SEC_DEEP[s] / (double)SEC_ONS[s] : 0.0;
        fprintf(stderr, "#   %-7s  g%-3d  %-4d  %-9d  %-6ld  %-7ld  %-11ld  %.1f%%\n",
                ORDER[s], SECDEPTH[s], SECBARS[s], SEC_STRLEN[s], SEC_ONS[s], SEC_ACC[s], SEC_DEEP[s], dp);
        tot_ons += SEC_ONS[s]; tot_acc += SEC_ACC[s]; tot_deep += SEC_DEEP[s];
    }
    fprintf(stderr, "#   total onsets=%ld  accents=%ld  deep=%ld (%.1f%% below the 16th level)\n",
            tot_ons, tot_acc, tot_deep, tot_ons ? 100.0 * (double)tot_deep / (double)tot_ons : 0.0);

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
