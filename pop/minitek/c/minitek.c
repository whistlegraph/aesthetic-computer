// minitek.c — a lean minimal-techno engine in C. Step-sequenced, fast, and
// hypnotic: a punchy pitch-enveloped sine kick four-on-the-floor, noise hats,
// a clap on 2 & 4, a tanh-saturated sine sub, and a resonant SVF "blip" stab
// (303-ish). A kick-triggered sidechain ducks the music bus for the signature
// pump. Arrangement is a SECBARS section map with a per-section lane mask, so
// the groove adds and drops elements as it moves. render.mjs --engine <name>
// masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -o minitek minitek.c -lm
// Run:    ./minitek --out out/minitek-raw.wav

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
static double SWING = 0.52;                   // micro-swing on odd 16ths (0.5 = straight)

static uint32_t rng_s = 0x6d696e69; // "mini"
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
// kick — pitch sweeps 120→48 Hz in ~25ms, fast body decay, a click transient,
// driven through tanh for weight. Also stamps the sidechain trigger.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.34 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 48 + 72 * exp(-tt * 42.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 8.5);
        double click = exp(-tt * 360.0) * 0.7;
        double v = tanh((sin(ph) + click) * 1.9) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.13 : 0.04) * SR);
    double dec = open ? 42.0 : 130.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
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
        addR(s0 + i, v * 0.25, v * 0.25);             // a little verb on the clap
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — tanh-saturated sine bass with a quick attack and short release.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.005 * SR), rel = (long)(0.03 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((sin(ph) + 0.14 * sin(ph * 2)) * 1.25) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — a saw through a resonant state-variable lowpass with a fast cutoff
// envelope: the 303-ish acid plink. `res` 1..6, `cut0/cut1` Hz sweep.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.003 * SR), rel = (long)(0.02 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;        // naive saw ramp 0..1
        double in = 2.0 * saw - 1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 16.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.18, v * 0.18);
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.25 * SR), rel = (long)(0.4 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1400.0 / SR), q = 1.0 / 0.9;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.005 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.3, v * 0.3);
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
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64 };

// at 144 BPM a bar is ~1.667s; 72 bars ≈ 120s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "brk1", "grvB", "brk2", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 4, 12, 4 };
static int MASK[8] = {
    /*intro*/ L_KICK | L_CHAT,
    /*build*/ L_KICK | L_CHAT | L_OHAT | L_SUB,
    /*grvA */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP,
    /*brk1 */ L_SUB | L_BLIP | L_PAD,
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD,
    /*brk2 */ L_PAD | L_SUB,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_BLIP | L_PAD,
    /*outro*/ L_KICK | L_SUB,
};
static int START[8];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark natural-minor harmony in A: bass roots walk i–VI–VII–v per 4 bars.
static const int ROOTS[4] = { 33, 29, 31, 28 };          // A1, F1, G1, E1
static const int BLIP_SCALE[6] = { 57, 60, 62, 64, 67, 69 }; // A3 C4 D4 E4 G4 A4
static const int PAD_CH[4][3] = { {45,48,52}, {41,45,48}, {43,46,50}, {40,43,47} }; // Am F G Em (low)

int main(int argc, char **argv) {
    const char *out_path = "out/minitek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# minitek.c · %g BPM · %d bars · %.1fs · minimal techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x...x.";   // closed hats on the offbeat 8ths + lift
    const char *P_OHAT = "....x.......x...";   // open hat splash
    const char *P_SUB  = "x..x..x.x..x..x.";   // rolling syncopated sub
    const char *P_BLIP = "....x..x....x.xx";   // sparse acid stab figure

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // kick — four-on-the-floor (drop the last kick on a fill bar for a gap)
                if ((m & L_KICK) && st % 4 == 0 && !(fill && st == 12)) kick(t, 0.95);
                // hats
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.5 + 0.1 * (st % 4 == 2), 0, rnd2() * 0.3);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.18) hat(t, 0.22, 0, rnd2() * 0.4); // 16th ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.42, 1, 0.15);
                // clap on 2 & 4
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.7);
                // sub bass
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root, t, STEP * 1.6, 0.85);
                // acid blip — note walks the scale, evolves per phrase
                if ((m & L_BLIP) && P_BLIP[st] == 'x') {
                    int note = BLIP_SCALE[(st + b * 2) % 6] + (phr == 3 ? 12 : 0);
                    blip(note, t, STEP * 1.8, 0.5, 4.0 + 1.5 * (s >= 4), 2600, 380, rnd2() * 0.5);
                }
            }
            // pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.10, (z - 1) * 0.4); }
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.16);
        }
    }

    // ── sidechain: kick-triggered duck on the music bus (the pump) ──
    { double depth = 0.6, rel = exp(-1.0 / (0.11 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── light Schroeder reverb on the send (drums stay dry-ish) ──
    {
        double decay = 0.72, wet = 0.32, damp = 0.45;
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
