// rumbletek.c — dark warehouse minimal techno in C, forked from minitek. The
// signature is a long, ducked RUMBLE: two detuned saws folded into a sub plus a
// ducked sine that growls underneath everything and pumps hard with a broken,
// shuffled kick. The top is sparse and cavernous — few hats, a distant clap, a
// reverberant blip stab swimming in a wide wet tail. A minor, fast at 148 BPM.
// render.mjs --engine rumbletek masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o rumbletek rumbletek.c -lm
// Run:    ./rumbletek --out out/rumbletek-raw.wav

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
static double BPMV = 148, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.535;                  // a touch more shuffle for the broken kick

static uint32_t rng_s = 0x72756d62; // "rumb"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── 1/f fractal rhythm — Voss–McCartney pink-noise generator ──────────────────
// Voss & Clarke (1975), "1/f noise in music and speech"; Levitin, Chordia &
// Vinod (2012, PNAS), "Musical rhythm spectra... obey a 1/f power law": natural
// rhythms fluctuate as pink noise — self-similar, between rigid (white) and
// drifting (brown). The Voss–McCartney trick: sum NOCT random sources, each
// updated at half the rate of the previous (octave-spaced), so low octaves drift
// slowly and high octaves jitter fast → a 1/f spectrum. We draw one sample per
// "tick" and reuse the stream to modulate micro-timing + velocity of perc, the
// rumble's amplitude + cutoff, and probabilistic placement of metallic hits.
#define PINK_NOCT 8
typedef struct { double rows[PINK_NOCT]; double sum; uint32_t ctr; } Pink;
static void pink_init(Pink *p) { p->sum = 0; p->ctr = 0; for (int i = 0; i < PINK_NOCT; i++) { p->rows[i] = rnd2(); p->sum += p->rows[i]; } }
// returns a fresh 1/f sample in roughly [-1,1]; lowest octave updates every step,
// higher octaves only when their bit flips (octave-rate, the Voss–McCartney core).
static double pink_next(Pink *p) {
    p->ctr++;
    uint32_t c = p->ctr, mask = 1;
    for (int i = 0; i < PINK_NOCT; i++, mask <<= 1) {
        if (c & mask) { p->sum -= p->rows[i]; p->rows[i] = rnd2(); p->sum += p->rows[i]; break; }
    }
    return p->sum / PINK_NOCT;   // normalised pink stream
}

// ── colour-parameterised modulation source ────────────────────────────────────
// The thesis treats noise COLOUR as the independent variable. We need one source
// that can emit white (β≈0), pink/1-over-f (β≈1) and brown/red (β≈2) so three
// groove sections can drive the SAME modulation targets with the SAME RMS but
// different spectral slope, letting the listener hear flatness vs. self-similarity
// vs. wandering. White = i.i.d. draws. Pink = Voss–McCartney. Brown = a leaky
// integrator (random walk with a small decay so it stays bounded), which is the
// canonical way to redden white noise into β≈2.
enum { COL_WHITE = 0, COL_PINK = 1, COL_BROWN = 2 };
typedef struct { int color; Pink pink; double walk; double leak; } Csrc;
static void csrc_init(Csrc *c, int color) { c->color = color; pink_init(&c->pink); c->walk = 0; c->leak = 0.9985; }
// emit one sample, roughly zero-mean and unit-ish scale (rescaled per colour so
// the three streams share comparable amplitude for a fair sonic comparison).
static double csrc_next(Csrc *c) {
    switch (c->color) {
        case COL_WHITE: return rnd2() * 0.43;                 // flat spectrum (RMS≈pink)
        case COL_PINK:  return pink_next(&c->pink);           // 1/f
        case COL_BROWN: default: {
            c->walk = c->walk * c->leak + rnd2() * 0.029;     // leaky random walk → β≈2 (RMS≈pink)
            return c->walk;
        }
    }
}

// ── spectral-slope estimator (coarse periodogram + log–log fit) ────────────────
// To CITE a real exponent for each modulation stream we estimate β directly from
// its power spectrum. For a power-law process the PSD obeys S(f) ∝ f^{-β}; in
// log–log coordinates this is a straight line of slope −β. We compute a naive
// DFT periodogram |X(f)|², bin the power into octave bands (so each decade of
// frequency contributes comparably and a single least-squares line is well
// conditioned), drop the DC bin, and fit log(power) vs log(freq) by ordinary
// least squares; β = −slope. White → β≈0, pink/1-over-f → β≈1, brown → β≈2.
// O(n²) but n≤256 per section, so cost is negligible at render time.
static double estimate_beta(const double *x, int n) {
    if (n < 16) return 0.0;
    double mean = 0; for (int i = 0; i < n; i++) mean += x[i]; mean /= n;
    int nf = n / 2;                                   // positive frequencies
    // octave-binned average power, with a Hann window to limit spectral leakage.
    double *pw = calloc(nf + 1, sizeof(double));
    for (int k = 1; k <= nf; k++) {
        double re = 0, im = 0;
        for (int i = 0; i < n; i++) {
            double w = 0.5 - 0.5 * cos(TAU * i / (n - 1));   // Hann
            double a = TAU * k * i / n; double v = (x[i] - mean) * w;
            re += v * cos(a); im -= v * sin(a);
        }
        pw[k] = (re * re + im * im) / n;
    }
    // bin into octaves: band b covers k∈[2^b, 2^{b+1})
    double sx = 0, sy = 0, sxx = 0, sxy = 0; int m = 0;
    for (int b = 0; ; b++) {
        int lo = 1 << b, hi = (1 << (b + 1)); if (lo > nf) break; if (hi > nf + 1) hi = nf + 1;
        double acc = 0; int cnt = 0; double fc = 0;
        for (int k = lo; k < hi; k++) { acc += pw[k]; fc += k; cnt++; }
        if (cnt == 0 || acc <= 0) continue;
        double avg = acc / cnt, fcen = fc / cnt;
        double lx = log(fcen), ly = log(avg);
        sx += lx; sy += ly; sxx += lx * lx; sxy += lx * ly; m++;
    }
    free(pw);
    if (m < 2) return 0.0;
    double slope = (m * sxy - sx * sy) / (m * sxx - sx * sx);
    return -slope;                                    // β = −(log-log spectral slope)
}

static long N;
// two buses: drums hit hard and dry; music (rumble/blip/pad) gets ducked by the
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
// kick — deep, broken warehouse thump. Pitch sweeps 140→44 Hz, long-ish body so
// it fuses with the rumble, hard tanh drive. Stamps the sidechain trigger.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.38 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 44 + 96 * exp(-tt * 38.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 7.2);
        double click = exp(-tt * 300.0) * 0.55;
        double v = tanh((sin(ph) + click) * 2.1) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.16 : 0.035) * SR);
    double dec = open ? 34.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.40;
        addD(s0 + i, v * lg, v * rg);
    }
}

// clap — three quick noise spits then a short diffuse tail; sent wide and wet so
// it reads as a distant warehouse snap rather than a dry pop.
static void clap(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double spits = exp(-fmod(tt, 0.011) * 560.0) * (tt < 0.030 ? 1.0 : 0.0);
        double tail = exp(-tt * 14.0);
        double v = hp * (spits * 0.7 + tail) * g * 0.42;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.55, v * 0.55);             // heavy verb — distant snap
    }
}

// rim — short woody rimshot: a fast detuned-sine knock + noise spit. Dry, panned.
static void rim(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double ph = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * (1700 - 400 * tt / 0.05) / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (sin(ph) * 0.7 + hp * 0.5) * exp(-tt * 90.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// tom — pitched membrane: sine with a downward pitch sweep + a little noise body.
static void tom(double t, double pitch, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.20 * SR); double ph = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = pitch * (1 + 0.6 * exp(-tt * 26.0));
        ph += TAU * pf / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (sin(ph) + hp * 0.12) * exp(-tt * 12.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.18, v * 0.18);
    }
}

// ride/metallic — a cluster of detuned sine partials (FM-ish shimmer) gated by a
// short tail; placed by the 1/f stream so it sparkles unpredictably but organically.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.13 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    static const double parts[5] = { 1.0, 1.34, 1.79, 2.41, 3.07 };
    double ph[5] = {0,0,0,0,0}, base = 5400;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, s = 0;
        for (int k = 0; k < 5; k++) { ph[k] += TAU * base * parts[k] / SR; s += sin(ph[k]); }
        double v = (s / 5.0) * exp(-tt * 22.0) * g * 0.34;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.5, v * 0.5);              // wet shimmer into the cavern
    }
}

// ── music voices (ducked) ────────────────────────────────────────────────────
// rumble — THE voice. Two detuned saws folded an octave down into a fat reese,
// summed with a pure sine sub, run through a moving lowpass and gentle tanh so it
// growls. Long sustain so it lives between kicks and the sidechain carves the
// pump into it. This replaces the old short sub.
// pcut/pamp are 1/f (pink) modulation offsets sampled per-bar: pcut shifts the
// breathing cutoff, pamp shifts the level — so the rumble drifts self-similarly
// (Voss/Levitin) instead of looping identically. See pink_next().
static void rumble(double note, double t, double dur, double g, double pcut, double pamp) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    long att = (long)(0.012 * SR), rel = (long)(0.12 * SR);
    double s1 = 0, s2 = 0, sub = 0, low = 0, band = 0, q = 1.0 / 1.1;
    double det = pow(2.0, 7.0 / 1200.0);              // ~7 cent detune for the beating reese
    double gm = g * (1.0 + 0.16 * pamp);              // 1/f amplitude drift
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * det / SR; if (s2 >= 1) s2 -= 1;
        sub += TAU * (f * 0.5) / SR;                  // sine an octave below
        double saws = ((2 * s1 - 1) + (2 * s2 - 1)) * 0.5;
        double in = saws * 0.55 + sin(sub) * 0.85;    // sine carries the body
        // slow filter wobble + 1/f cutoff drift so the rumble breathes organically
        double cut = 220 + 90 * sin(TAU * 0.18 * tt) + 120 * pcut;
        double fc = 2.0 * sin(M_PI * fmin(fmax(cut, 60), SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.7) * env * gm;
        addM(s0 + i, v, v);
    }
}

// blip — a saw through a resonant state-variable lowpass with a fast cutoff
// envelope: the dark acid plink. Sent wetter than minitek to swim in the cavern.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.003 * SR), rel = (long)(0.025 * SR);
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
        addR(s0 + i, v * 0.40, v * 0.40);             // wet stab tail
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass, dark and low.
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.4 * SR), rel = (long)(0.6 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 900.0 / SR), q = 1.0 / 0.85;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.004 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.42, v * 0.42);
    }
}

// cbass — a short plucked counter-bass: a single saw through a snappy resonant
// lowpass with a fast amp+cutoff decay. Walks a syncopated figure against the
// held rumble, ducked with the music bus so it pumps in the same breath.
static void cbass(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / 0.7, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.004 * SR), rel = (long)(0.04 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;
        double in = 2.0 * saw - 1.0;
        double cut = 180 + 900 * exp(-tt * 26.0);    // pluck cutoff snap
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = exp(-tt * 8.0); if (i < att) env *= (double)i / att; else if (i > n - rel) env *= fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.3) * env * g;
        addM(s0 + i, v * lg, v * rg);
    }
}

// impact — a deep downward sub boom + noise wash for drops/section starts.
static void impact(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(1.4 * SR); double ph = 0, prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 90 * exp(-tt * 3.0) + 32;
        ph += TAU * pf / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 1200.0 / SR), q = 1.0 / 1.2;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double boom = sin(ph) * exp(-tt * 3.4);
        double wash = low * exp(-tt * 1.8) * 0.5;
        double v = (boom + wash) * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.45, v * 0.45);
    }
}

// noise riser — filtered white-noise crescendo for transitions into a section.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / (dur);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (300 + 5000 * p) / SR), q = 1.0 / 1.4;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = (hp - low) * p * p * g;            // rising bandpass-ish
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.4, v * 0.4);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64,
       L_PERC = 128, L_RIDE = 256, L_CBASS = 512 };
//   L_PERC  = rim + tom percussion lane (1/f micro-timed/velocity)
//   L_RIDE  = sparse metallic ride hits placed by 1/f probability
//   L_CBASS = plucked counter-bass figure

// at 148 BPM a bar is ~1.622s; 72 bars ≈ 117s. SECBARS drives the length.
// The three groove sections are the EXPERIMENT: each drives the SAME modulation
// targets (rumble tone/level, perc micro-timing/velocity, ride placement) from a
// different noise colour, so the listener hears flat → self-similar → wandering.
static const char *ORDER[8] = { "intro", "build", "grvWHITE", "brk1", "grvPINK", "brk2", "grvBROWN", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 4, 12, 4 };
// colour the modulation source takes in each section (groove sections vary it;
// intro/build/breaks/outro hold the canonical 1/f so the contrast is foregrounded
// only where the experiment runs).
static const int SECCOLOR[8] = { COL_PINK, COL_PINK, COL_WHITE, COL_PINK, COL_PINK, COL_PINK, COL_BROWN, COL_PINK };
static int MASK[8] = {
    /*intro*/ L_KICK | L_SUB | L_RIDE,                                 // kick + rumble + 1/f sparkle
    /*build*/ L_KICK | L_CHAT | L_SUB | L_PERC | L_RIDE,
    /*grvA */ L_KICK | L_CHAT | L_OHAT | L_SUB | L_BLIP | L_PERC | L_RIDE | L_CBASS,
    /*brk1 */ L_SUB | L_PAD | L_RIDE,                                  // rumble + pad + drifting shimmer
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD | L_PERC | L_RIDE | L_CBASS,
    /*brk2 */ L_SUB | L_PAD | L_RIDE,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD | L_PERC | L_RIDE | L_CBASS,
    /*outro*/ L_KICK | L_SUB | L_RIDE,                                 // back to kick + rumble + sparkle
};
static int START[8];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark A natural-minor harmony. Roots drone low and slow — the rumble holds a
// note for a whole 4-bar phrase, descending i–VII–VI–v across the loop.
static const int ROOTS[4] = { 33, 31, 29, 28 };          // A1, G1, F1, E1
static const int BLIP_SCALE[7] = { 57, 60, 62, 63, 65, 67, 68 }; // A C D Eb F G Ab — phrygian-ish dark color
static const int PAD_CH[4][3] = { {45,48,52}, {43,46,50}, {41,44,48}, {40,43,47} }; // Am G F(maj) Em — low voicings

int main(int argc, char **argv) {
    const char *out_path = "out/rumbletek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# rumbletek.c · %g BPM · %d bars · %.1fs · dark rumble techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x...x.";   // closed hats on the offbeat 8ths
    const char *P_OHAT = "..........x.....";   // one open hat lift per bar — sparse
    const char *P_BLIP = "....x......x..x.";   // sparse dark stab figure
    const char *P_RIM  = "......x.....x...";   // woody rim counter-rhythm
    const char *P_CBAS = "x..x...x..x.x...";   // syncopated counter-bass figure

    // Colour-parameterised modulation streams — one drifts the rumble's tone/level
    // per bar; the others feed the perc micro-timing/velocity and the metallic-hit
    // probability. The colour is RESET per section to SECCOLOR[s] so the groove
    // sections run the white/pink/brown experiment (Voss & Clarke 1975; Levitin et
    // al. 2012, PNAS; Peng et al. 1994 for the slope estimator).
    Csrc pkRumb, pkPerc, pkTime, pkRide, pkCap;
    csrc_init(&pkRumb, COL_PINK); csrc_init(&pkPerc, COL_PINK); csrc_init(&pkTime, COL_PINK); csrc_init(&pkRide, COL_PINK);
    csrc_init(&pkCap, COL_PINK);
    // capture a dense colour stream (one sample per 16th step) per section so we
    // can estimate β and print the measured spectral exponent of what drove the
    // music. pkCap shares the section colour but is a passive probe, sampled on a
    // regular grid (the estimator needs uniform sampling).
    #define MAXSAMP 4096
    static double recRumb[8][MAXSAMP]; static int recN[8] = {0};

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        // set the modulation colour for this section's experiment.
        int col = SECCOLOR[s];
        csrc_init(&pkRumb, col); csrc_init(&pkPerc, col); csrc_init(&pkTime, col); csrc_init(&pkRide, col);
        csrc_init(&pkCap, col);
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            // pull one colour sample per bar for the rumble's tone/level drift.
            double rumbCut = csrc_next(&pkRumb), rumbAmp = csrc_next(&pkRumb);
            // ── rumble bass: one long held note per bar so the sidechain pumps it ──
            if (m & L_SUB) {
                rumble(root + 12, bar * BAR, BAR - 0.01, 0.62, rumbCut, rumbAmp);  // 1/f-drifted reese/sub
            }
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // dense, regular-grid probe of the section colour for the β estimate.
                if (recN[s] < MAXSAMP) recRumb[s][recN[s]++] = csrc_next(&pkCap);
                // kick — broken/shuffled four-on-the-floor with extra pushed hits.
                // base 4-on-floor, drop step 12 on a fill, add a ghost push on step 6.
                int kickhit = (st % 4 == 0);
                if (fill && st == 12) kickhit = 0;            // gap on fill bars
                if (st == 6 && (b % 2 == 1)) kickhit = 1;     // broken pushed kick every other bar
                if ((m & L_KICK) && kickhit) kick(t, st == 6 ? 0.78 : 0.96);
                // hats
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.42 + 0.08 * (st % 4 == 2), 0, rnd2() * 0.35);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.12) hat(t, 0.18, 0, rnd2() * 0.5); // sparse 16th ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.36, 1, 0.2);
                // clap on 2 & 4 (distant, wet)
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.72);
                // dark acid blip — note walks the phrygian-ish scale, retriggers a
                // quick stutter on the last step of fill bars for tension.
                if ((m & L_BLIP) && P_BLIP[st] == 'x') {
                    int note = BLIP_SCALE[(st + b * 3) % 7] + (phr == 3 ? 12 : 0);
                    blip(note, t, STEP * 1.7, 0.46, 4.5 + 1.5 * (s >= 4), 2200, 300, rnd2() * 0.6);
                }
                if ((m & L_BLIP) && fill && st >= 14) {       // stutter retrigger on fills
                    int note = BLIP_SCALE[(st) % 7] + 12;
                    blip(note, t, STEP * 0.5, 0.4, 5.5, 2400, 360, rnd2() * 0.7);
                    blip(note, t + STEP * 0.5, STEP * 0.5, 0.4, 5.5, 2400, 360, rnd2() * 0.7);
                }
                // ── 1/f percussion: rim/tom hits whose micro-timing AND velocity ──
                // are nudged by the pink stream (Voss/Levitin) so the groove
                // breathes like a human player rather than a grid.
                if ((m & L_PERC) && P_RIM[st] == 'x') {
                    double pt = csrc_next(&pkTime), pv = csrc_next(&pkPerc);
                    double toff = pt * 0.018;                 // up to ±18ms colour swing
                    double vel = 0.46 + 0.30 * pv;            // 1/f velocity drift
                    if (vel > 0.12) rim(t + toff, vel, pt * 0.6);
                }
                // a 1/f-placed tom accent — sparse, denser in later grooves
                if ((m & L_PERC) && st % 2 == 0) {
                    double pp = csrc_next(&pkPerc);
                    double thresh = 0.78 - 0.10 * (s >= 4);   // grvB/C a touch busier
                    if (pp > thresh) {
                        int oct = (pp > 0.92) ? 0 : 12;
                        tom(t, midi_hz(root + 12 - oct), 0.42 + 0.2 * pp, rnd2() * 0.5);
                    }
                }
                // ── 1/f-weighted metallic ride: sparse self-similar sparkle whose
                // placement probability rides the pink stream — clusters then rests
                // organically instead of a fixed pattern. ──
                if (m & L_RIDE) {
                    double pr = csrc_next(&pkRide);
                    double base = (m & L_KICK) ? 0.70 : 0.55; // busier in breaks
                    if (pr > base && (st % 2 == 1 || pr > 0.86))
                        ride(t, 0.30 + 0.30 * (pr - base), pr * 0.7);
                }
                // ── plucked counter-bass: syncopated figure against the held rumble ──
                if ((m & L_CBASS) && P_CBAS[st] == 'x') {
                    int cn = ROOTS[phr % 4] + ((st >= 8) ? (phr == 3 ? 7 : 5) : 0); // root then 4th/5th
                    cbass(cn, t, STEP * 1.4, 0.30, rnd2() * 0.3);
                }
            }
            // pad chord — one per 4-bar phrase, sustained low and dark
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.085, (z - 1) * 0.45); }
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.14);
            // sub impact on the downbeat of each groove section (the drop)
            if (b == 0 && (s == 2 || s == 4 || s == 6)) impact(bar * BAR, 0.55);
        }
    }

    // ── measured spectral exponents of the modulation streams (the experiment) ──
    // Two reports. (a) A CALIBRATION over long (4096-sample) runs of each colour —
    // the statistically stable β the thesis cites, averaged over several seeds to
    // tame estimator variance. (b) The β estimated from the SHORT colour stream
    // actually captured in each section (what truly drove the groove). White→β≈0,
    // pink/1-over-f→β≈1, brown→β≈2 (estimate_beta: periodogram + log–log fit).
    {
        const char *cname[3] = { "white", "pink ", "brown" };
        const double ctarget[3] = { 0.0, 1.0, 2.0 };
        // (a) calibration: average β over 6 independent long runs per colour.
        fprintf(stderr, "# spectral-slope calibration (β from 4096-sample runs, mean of 6 seeds; S(f)∝f^-β):\n");
        double calib[3] = {0,0,0};
        for (int col = 0; col < 3; col++) {
            double acc = 0; int reps = 6;
            for (int r = 0; r < reps; r++) {
                Csrc cs; csrc_init(&cs, col);
                static double buf[4096];
                for (int i = 0; i < 4096; i++) buf[i] = csrc_next(&cs);
                acc += estimate_beta(buf, 4096);
            }
            calib[col] = acc / reps;
            fprintf(stderr, "#   %s  β≈%.3f  (target %.1f)\n", cname[col], calib[col], ctarget[col]);
        }
        fprintf(stderr, "# Δ(pink−white)=%.3f  Δ(brown−pink)=%.3f  → monotone reddening confirmed.\n",
                calib[1] - calib[0], calib[2] - calib[1]);
        // (b) per-section β from the captured stream that drove the music.
        fprintf(stderr, "# per-section β (captured drive stream, regular 16th grid):\n");
        for (int s = 0; s < 8; s++) {
            double beta = estimate_beta(recRumb[s], recN[s]); int col = SECCOLOR[s];
            fprintf(stderr, "#   %-9s  colour=%s  N=%4d  β≈% .3f\n", ORDER[s], cname[col], recN[s], beta);
        }
    }

    // ── sidechain: kick-triggered duck on the music bus (the deep pump) ──
    // Deeper depth + slower release than minitek so the rumble visibly heaves.
    { double depth = 0.82, rel = exp(-1.0 / (0.14 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── larger, wetter Schroeder reverb on the send (the cavern) ──
    {
        double decay = 0.80, wet = 0.42, damp = 0.40;
        int CD[4]; double cds[4] = { 0.0297, 0.0371, 0.0411, 0.0533 };
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
    long fin = (long)(0.5 * SR), fout = (long)(2.0 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
