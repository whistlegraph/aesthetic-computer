// acidtek.c — a screaming acid minimal-techno engine in C. Super-fast (146 BPM)
// four-on-the-floor with a TB-303-style LEAD: a saw through a high-resonance SVF
// whose cutoff sweeps open and shut across phrases, with slides and accents and
// stutter retrigs. A deep kick-triggered sidechain ducks the music bus hard for
// a pumping pump; the acid line gets a tempo-synced delay so it ricochets. Key
// of E minor. Arrangement is a SECBARS section map with a per-section lane mask.
// render.mjs --engine acidtek masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o acidtek acidtek.c -lm
// Run:    ./acidtek --out out/acidtek-raw.wav

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
static double BPMV = 146, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.515;                  // tight micro-swing on odd 16ths (0.5 = straight)

static uint32_t rng_s = 0xac1d7e6b; // "acid" seed
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// two buses: drums hit hard and dry; music (bass/blip/pad) gets ducked by the
// kick. revL/R is a send. dlyL/R is the acid delay send. trig[] marks kick
// onsets for the sidechain.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *dlyL, *dlyR, *trig;
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }
static inline void addDly(long i, double l, double r) { if (i >= 0 && i < N) { dlyL[i] += (float)l; dlyR[i] += (float)r; } }

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
// kick — punchy: pitch sweeps 130→46 Hz fast, snappy click, hard tanh weight.
// Stamps the sidechain trigger (keep this or the pump dies).
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.30 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 46 + 84 * exp(-tt * 48.0);
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
    double dec = open ? 50.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.45;
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
        double v = hp * (spits * 0.8 + tail) * g * 0.5;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.22, v * 0.22);             // a little verb on the clap
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — tanh-saturated sine bass with a rumbling 2nd-harmonic reese edge.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.004 * SR), rel = (long)(0.03 * SR); double ph = 0, ph2 = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR; ph2 += TAU * f * 1.007 / SR; // slight detune → reese rumble
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((sin(ph) + sin(ph2) + 0.18 * sin(ph * 2)) * 0.95) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — the STAR: a saw through a high-resonance state-variable lowpass with a
// fast cutoff envelope; an optional slide ramps the pitch in from the previous
// note; accent boosts both gain and resonance. Feeds the acid delay send hard.
static void blip(double note, double prevNote, double t, double dur, double g, double res, double cut0, double cut1, double pan, int slide, int accent) {
    double f = midi_hz(note), fprev = midi_hz(prevNote);
    long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0;
    double ares = res + (accent ? 1.6 : 0.0), ag = g * (accent ? 1.35 : 1.0);
    double q = 1.0 / fmax(0.45, ares), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.002 * SR), rel = (long)(0.02 * SR);
    double slideTime = 0.045 * SR;                    // ~45ms portamento
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double cf = f;
        if (slide && i < slideTime) { double a = (double)i / slideTime; cf = fprev * (1 - a) + f * a; }
        saw += cf / SR; if (saw >= 1) saw -= 1;        // naive saw ramp 0..1
        double in = 2.0 * saw - 1.0;
        double envd = accent ? 9.0 : 14.0;             // accent holds the filter open longer
        double cut = cut1 + (cut0 - cut1) * exp(-tt * envd);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.4) * env * ag;          // soft drive on the acid for scream
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.10, v * 0.10);
        addDly(s0 + i, v * 0.42 * lg, v * 0.42 * rg);   // ricochet send
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

// rim — short woody click (sine burst + noise transient), panned. Adds dry
// percussive interest between the kicks without muddying the low end.
static void rim(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double ph = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * 1700.0 / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (sin(ph) * 0.6 + hp * 0.5) * exp(-tt * 120.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// shaker — tight highpassed noise with a soft attack, swung 16ths for groove.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double att = fmin(1.0, tt / 0.004);
        double v = hp * att * exp(-tt * 60.0) * g * 0.3;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride — a metallic ping: a small bank of inharmonic partials with a long tail,
// gives the peak sections a shimmering top end. Sent to reverb.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.5 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    static const double parts[5] = { 1.0, 1.34, 1.79, 2.41, 3.05 };
    double ph[5] = {0,0,0,0,0};
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, s = 0;
        for (int k = 0; k < 5; k++) { ph[k] += TAU * 5200.0 * parts[k] / SR; s += sin(ph[k]); }
        double v = (s / 5.0) * exp(-tt * 6.0) * g * 0.28;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.18, v * 0.18);
    }
}

// tom — a quick pitched membrane (sine drop + soft noise) for fills/transitions.
static void tom(double note, double t, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(0.22 * SR);
    double ph = 0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f * (1.0 + 0.6 * exp(-tt * 22.0));
        ph += TAU * pf / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (sin(ph) + hp * 0.15) * exp(-tt * 11.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// stab — a short detuned saw chord through a snappy lowpass: a counter-melodic
// rave organ that answers the 303 on the off-phrases. Ducked + reverb send.
static void stab(const int *ch, double t, double dur, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.003 * SR), rel = (long)(0.04 * SR);
    double s1[3] = {0,0,0}, s2[3] = {0,0,0}, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 2600.0 / SR), q = 1.0 / 1.1;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, in = 0;
        for (int z = 0; z < 3; z++) {
            double f = midi_hz(ch[z] + 12);
            s1[z] += f / SR; if (s1[z] >= 1) s1[z] -= 1;
            s2[z] += f * 1.008 / SR; if (s2[z] >= 1) s2[z] -= 1;
            in += (2 * s1[z] - 1 + 2 * s2[z] - 1) * 0.5;
        }
        in /= 3.0;
        double dcut = 2600.0 * exp(-tt * 16.0) + 600.0;
        fc = 2.0 * sin(M_PI * fmin(dcut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.3) * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.16, v * 0.16);
    }
}

// ── Barlow metric indispensability (TRUE recursive function) ─────────────────
// Clarence Barlow, "Two Essays on Theory" (Computer Music Journal 11/1, 1987)
// — the indispensability function of his Çoğluotobüsişletmesi / Autobusk system.
// For a meter stratified as a product of primes, every pulse carries an integer
// indispensability; the more indispensable a pulse, the more essential it is to
// perceiving the meter (the downbeat is maximally indispensable, the bare odd
// 16ths least). Earlier passes of this engine hand-tuned the 16 values. This
// pass implements Barlow's ACTUAL recurrence over the prime factorisation and
// derives the whole 16-vector from first principles, then drives the 303 with it.
//
// Per-stratum (within-prime) indispensability ξ_p(j), Barlow's published tables:
//   p=2 → {1,0}   p=3 → {2,0,1}   p=5 → {4,0,3,1,2}   p=7 → {6,0,4,2,5,1,3}
// Compound indispensability of pulse n in a meter of strata pf[0..z-1]
// (pf[0] = OUTERMOST/slowest grouping, pf[z-1] = the pulse) is the weighted sum
//   ind(n) = Σ_{j=0}^{z-1}  ξ_{pf[j]}( digit_j(n) ) · Π_{k<j} pf[k]
// where digit_j(n) is the j-th mixed-radix digit (MSB = outermost stratum). The
// product Π_{k<j} pf[k] weights deeper strata by how many bars-worth of the
// coarser strata they subdivide, so the slow downbeat dominates and successive
// halvings interleave into the canonical metric tree. For 2·2·2·2 this returns
// the integer vector whose descending order is the textbook
//   0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15  (downbeat → bare last 16th).
static double IND16[16];      // normalised 0..1 (used by the synth voices)
static int    INDRANK[16];    // integer Barlow indispensability 0..15 (15 = strongest)
static int    ORDER_BY_RANK[16]; // step indices, strongest → weakest

static int xi_prime(int p, int j) {
    static const int t2[2] = {1,0};
    static const int t3[3] = {2,0,1};
    static const int t5[5] = {4,0,3,1,2};
    static const int t7[7] = {6,0,4,2,5,1,3};
    if (p == 2) return t2[j];
    if (p == 3) return t3[j];
    if (p == 5) return t5[j];
    if (p == 7) return t7[j];
    return 0;
}

static void build_indispensability(void) {
    // 4/4 sixteenth grid factored as the stratified meter 2·2·2·2.
    static const int pf[4] = {2,2,2,2};
    const int z = 4;
    int Np = 1; for (int i = 0; i < z; i++) Np *= pf[i];   // = 16
    int raw[16];
    for (int n = 0; n < Np; n++) {
        long v = 0;
        for (int j = 0; j < z; j++) {
            int outer = 1; for (int k = 0; k < j; k++) outer *= pf[k];   // Π_{k<j} pf[k]
            int digit = (n / (Np / (outer * pf[j]))) % pf[j];           // MSB-first digit
            v += (long)xi_prime(pf[j], digit) * outer;
        }
        raw[n] = (int)v;        // 0..15 for 2·2·2·2
    }
    // dense-rank raw[] into 0..15 (ties broken by earlier step), 15 = strongest.
    for (int n = 0; n < Np; n++) {
        int r = 0;
        for (int m = 0; m < Np; m++)
            if (raw[m] < raw[n] || (raw[m] == raw[n] && m < n)) r++;
        INDRANK[n] = r;
    }
    for (int n = 0; n < Np; n++) IND16[n] = INDRANK[n] / 15.0;
    // strongest → weakest step order (the metric-reveal schedule reads this).
    for (int target = 15, w = 0; target >= 0; target--)
        for (int n = 0; n < Np; n++) if (INDRANK[n] == target) ORDER_BY_RANK[w++] = n;
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64,
       L_RIM = 128, L_SHKR = 256, L_RIDE = 512, L_STAB = 1024 };

// at 146 BPM a bar is ~1.644s; 70 bars ≈ 115s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "brk1", "grvB", "acidpk", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 12, 4, 12, 8 };
static int MASK[8] = {
    /*intro */ L_KICK | L_CHAT | L_RIM,
    /*build */ L_KICK | L_CHAT | L_OHAT | L_SUB | L_RIM | L_SHKR,
    /*grvA  */ L_KICK | L_CHAT | L_OHAT | L_SUB | L_BLIP | L_SHKR | L_RIM,
    /*brk1  */ L_BLIP | L_PAD | L_RIM | L_STAB,                  // acid + pad solo, no kick
    /*grvB  */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD | L_SHKR | L_STAB,
    /*acidpk*/ L_KICK | L_OHAT | L_SUB | L_BLIP | L_RIDE | L_SHKR, // stripped peak, acid screams
    /*grvC  */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD | L_SHKR | L_RIDE | L_STAB,
    /*outro */ L_KICK | L_CHAT | L_SUB | L_BLIP | L_RIM,
};
static int START[8];

// ── the metric-reveal experiment ─────────────────────────────────────────────
// The dissertation's central experiment: across the eight sections the 303 is
// permitted to ACCENT progressively more of the indispensability hierarchy. In
// the intro only the single most indispensable pulse (the downbeat, rank 15) may
// accent; by the peak the full Barlow profile down to the bare 16ths is exposed.
// REVEAL[s] = how many of the top-ranked steps (read off ORDER_BY_RANK) are the
// section's "accent set". A step accents iff its rank is in the top REVEAL[s].
// This makes the metric tree audible as a controlled sweep of the independent
// variable rather than a fixed groove — the listener hears the meter assemble.
static const int REVEAL[8] = {
    /*intro */  1,   // only the downbeat (rank 15) — pure pulse
    /*build */  2,   // + the half-bar (rank 14): 2-level meter emerges
    /*grvA  */  4,   // + the two quarters: 4-on-the-floor metric frame
    /*brk1  */  8,   // + the four eighths: the swung inner strata appear
    /*grvB  */ 10,   // creeping into the syncopated 16ths
    /*acidpk*/ 16,   // FULL profile — every pulse can scream (max syncopation)
    /*grvC  */ 12,   // pull back to a rich-but-anchored profile
    /*outro */  3,   // collapse toward the bare downbeat + half-bar + a quarter
};
// rank threshold for section s: a step accents iff INDRANK[step] >= this.
static int reveal_threshold(int s) { return 16 - REVEAL[s]; }

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// E natural minor harmony. Bass roots walk i–VI–VII–iv per 4 bars: E G D A.
static const int ROOTS[4] = { 28, 31, 26, 33 };          // E1, G1, D1, A1
// E minor scale across two octaves for the acid line to climb/scream.
static const int BLIP_SCALE[8] = { 52, 55, 57, 59, 62, 64, 67, 69 }; // E3 G3 A3 B3 D4 E4 G4 A4
// pad chords (low): Em  G  D  Am
static const int PAD_CH[4][3] = { {40,43,47}, {43,47,50}, {38,42,45}, {45,48,52} };

int main(int argc, char **argv) {
    const char *out_path = "out/acidtek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    build_indispensability();              // Barlow 2·2·2·2 metric vector → drives the 303
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); dlyL = calloc(N, 4); dlyR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# acidtek.c · %g BPM · %d bars · %.1fs · acid minimal techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // ── Barlow indispensability report (the measured apparatus) ───────────────
    fprintf(stderr, "# Barlow indispensability — meter 2·2·2·2 (16 pulses)\n");
    fprintf(stderr, "#   IND16 rank[step]:");
    for (int i = 0; i < 16; i++) fprintf(stderr, " %d:%d", i, INDRANK[i]);
    fprintf(stderr, "\n#   order strongest→weakest:");
    for (int i = 0; i < 16; i++) fprintf(stderr, " %d", ORDER_BY_RANK[i]);
    // verify against the canonical published ordering for 2·2·2·2.
    static const int CANON[16] = {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};
    int okrank = 1; for (int i = 0; i < 16; i++) if (ORDER_BY_RANK[i] != CANON[i]) okrank = 0;
    fprintf(stderr, "  [%s canonical Barlow ordering]\n", okrank ? "MATCHES" : "DIFFERS FROM");
    // Toussaint metrical "off-beatness": fraction of grid pulses whose rank is
    // below the median — a static property of this meter (Toussaint 2005).
    { int below = 0; for (int i = 0; i < 16; i++) if (INDRANK[i] < 8) below++;
      fprintf(stderr, "#   pulses below median rank (weak/syncopation-capable): %d/16 = %.3f\n",
              below, below / 16.0); }
    fprintf(stderr, "#   metric-reveal schedule REVEAL[s] (top-k strata accentable):");
    for (int i = 0; i < 8; i++) fprintf(stderr, " %s=%d", ORDER[i], REVEAL[i]);
    fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x...x.";   // closed hats on the offbeat 8ths
    const char *P_OHAT = "..x...x...x...x.";   // open hat pushing the offbeat (rave lift)
    const char *P_SUB  = "x.x...x.x.x...x.";   // syncopated rolling sub
    // the 303 line — dense, syncopated, the star. 's'=slide-in, 'A'=accent, 'x'=normal.
    const char *P_BLIP = "x.xAs.xxA.xsx.Ax";

    int prevNote = BLIP_SCALE[0];
    // per-section accent telemetry for the thesis: how many 303 notes fired,
    // how many accented, and the mean indispensability the accents landed on.
    int   sec_blips[8]  = {0}, sec_accents[8] = {0};
    double sec_accInd[8] = {0};
    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        int rthresh = reveal_threshold(s);     // a step may accent iff INDRANK[st] >= rthresh
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            // the acid cutoff breathes across each 4-bar phrase: open near phrase
            // centre, choke at the edges — the signature filter sweep.
            double phPos = (b % 4 + 1) / 4.0;                  // 0.25..1.0
            double openAmt = sin(phPos * M_PI);               // 0→1→0 arc
            double brightBoost = (s == 5) ? 1.0 : 0.0;        // acidpk section screams
            double topCut = 2200 + 4200 * openAmt + 2600 * brightBoost; // sweep ceiling
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                double ind = IND16[st];                    // Barlow metric strength of this pulse
                // kick — four-on-the-floor; broken: drop step-12 on fill bars, and
                // add a ghost off-kick on grvC for shuffle.
                if ((m & L_KICK) && st % 4 == 0 && !(fill && st == 12)) kick(t, 0.96);
                if ((m & L_KICK) && s == 6 && st == 14 && !fill) kick(t, 0.55); // broken off-kick
                // hats
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.5 + 0.1 * (st % 4 == 2), 0, rnd2() * 0.3);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.22) hat(t, 0.2, 0, rnd2() * 0.4); // 16th ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.4, 1, 0.12);
                // clap on 2 & 4
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.72);
                // rim — fires on the *mid-rank* offbeats (Barlow ind ≈ 0.4) so it
                // woodblocks the metric inner strata without colliding with kick/clap.
                if ((m & L_RIM) && (st == 3 || st == 7 || st == 11 || st == 15) && rnd() < 0.85)
                    rim(t, 0.5 + 0.4 * ind, rnd2() * 0.45);
                // shaker — swung 16th texture; velocity tracks indispensability so the
                // groove leans on the strong pulses (Barlow-weighted dynamics).
                if ((m & L_SHKR) && (st % 2 == 1 || st % 4 == 2))
                    shaker(t, 0.35 + 0.55 * ind, rnd2() * 0.5);
                // ride — peak shimmer on the strong inner pulses.
                if ((m & L_RIDE) && (st == 0 || st == 4 || st == 8 || st == 12))
                    ride(t, 0.4 + 0.3 * ind, (st % 8 == 0) ? -0.3 : 0.3);
                // sub bass
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root, t, STEP * 1.5, 0.82);
                // ── acid blip — driven by Barlow indispensability (1987) ──
                // The most indispensable pulses are forced to ACCENT and open the
                // filter widest; the weakest become slides/ghosts. Note velocity and
                // resonance scale with ind, so the line audibly "locks" to the meter.
                char bc = P_BLIP[st];
                if ((m & L_BLIP) && bc != '.') {
                    // ── the experiment's gate ──────────────────────────────────
                    // A 303 note ACCENTS iff this pulse's Barlow rank is within the
                    // section's revealed top-REVEAL[s] strata. Early sections expose
                    // only the strongest pulses; the peak exposes the whole tree.
                    // The pattern char 'A' no longer forces an accent — the meter does.
                    int revealed = (INDRANK[st] >= rthresh);
                    int accent = revealed;
                    int slide  = !accent && ((bc == 's') || (ind <= 0.20)); // weak ⇒ slide/ghost
                    sec_blips[s]++;
                    if (accent) { sec_accents[s]++; sec_accInd[s] += ind; }
                    int idx = (st + b * 3) % 8;
                    int note = BLIP_SCALE[idx] + ((phr == 3) ? 12 : 0);
                    // velocity & resonance ride the indispensability curve.
                    double vel = 0.30 + 0.30 * ind;                  // ghost..full
                    double res = 4.2 + 1.0 * openAmt + 0.8 * brightBoost + 1.4 * ind;
                    // cutoff ceiling opens MOST on the most indispensable pulses.
                    double cut0 = topCut * (0.62 + 0.55 * ind);
                    double cut1 = 220 + 160 * openAmt + 120 * ind;   // bottom of the sweep
                    blip(note, prevNote, t, STEP * 1.9, vel, res, cut0, cut1, rnd2() * 0.55, slide, accent);
                    prevNote = note;
                    // stutter retrig: 32nd double on the strongest accents (ind high).
                    if (accent && ind >= 0.75 && (s == 5 || s == 6) && rnd() < 0.55)
                        blip(note + 12, note, t + STEP * 0.5, STEP * 0.5, 0.32, res, cut0, cut1, rnd2() * 0.6, 0, 1);
                }
            }
            // stab — counter-melodic rave organ answering the 303: lands on the
            // backbeat half-bar (step 8, Barlow's 2nd-most indispensable pulse) and,
            // on fill bars, throws an extra hit on the "and" for a question/answer.
            if (m & L_STAB) {
                const int *ch = PAD_CH[(b / 4 + 1) % 4];      // off-by-one vs pad ⇒ harmonic motion
                stab(ch, step_t(bar, 8), STEP * 2.4, 0.16, -0.35);
                if (fill) stab(ch, step_t(bar, 14), STEP * 1.6, 0.13, 0.35);
            }
            // pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.10, (z - 1) * 0.4); }
            // tom fill — descending roll on the last bar of grooves into the next
            // section (uses Barlow's strong pulses as the roll anchors).
            if (fill && (s == 2 || s == 4 || s == 6)) {
                static const int rollNotes[4] = { 48, 45, 43, 40 };
                for (int z = 0; z < 4; z++)
                    tom(rollNotes[z], step_t(bar, 8 + z * 2), 0.6, (z & 1) ? 0.4 : -0.4);
            }
            // a riser in the last bar before a groove/peak section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 4)) riser(bar * BAR, BAR, 0.16);
        }
    }

    // ── measured results: how the 303 accent set grew across the experiment ───
    fprintf(stderr, "# 303 accent census (the metric-reveal result)\n");
    fprintf(stderr, "#   section   reveal  notes  accents  acc%%   mean-ind(accents)\n");
    int totBlip = 0, totAcc = 0;
    for (int s = 0; s < 8; s++) {
        if (!(MASK[s] & L_BLIP)) continue;
        double pct = sec_blips[s] ? 100.0 * sec_accents[s] / sec_blips[s] : 0.0;
        double mind = sec_accents[s] ? sec_accInd[s] / sec_accents[s] : 0.0;
        fprintf(stderr, "#   %-8s  k=%-3d  %4d   %5d   %5.1f  %.3f\n",
                ORDER[s], REVEAL[s], sec_blips[s], sec_accents[s], pct, mind);
        totBlip += sec_blips[s]; totAcc += sec_accents[s];
    }
    fprintf(stderr, "#   TOTAL 303 notes=%d accents=%d (%.1f%%)\n",
            totBlip, totAcc, totBlip ? 100.0 * totAcc / totBlip : 0.0);

    // ── acid delay: tempo-synced dotted-8th ping-pong on the blip send ──
    { long dly = (long)(STEP * 3.0 * SR); double fb = 0.42;     // dotted 8th = 3 sixteenths
      for (long i = dly; i < N; i++) {
          double l = dlyL[i] + dlyR[i - dly] * fb;              // ping-pong cross-feed
          double r = dlyR[i] + dlyL[i - dly] * fb;
          dlyL[i] = (float)l; dlyR[i] = (float)r;
      }
      for (long i = 0; i < N; i++) { musL[i] += dlyL[i] * 0.5f; musR[i] += dlyR[i] * 0.5f; } }

    // ── sidechain: deep kick-triggered duck on the music bus (the pump) ──
    { double depth = 0.78, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── Schroeder reverb on the send (wetter for the acid/stab atmosphere) ──
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
