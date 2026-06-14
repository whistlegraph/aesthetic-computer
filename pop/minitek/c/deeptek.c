// deeptek.c — a deep & warm rolling minimal-techno engine in C. Super fast at
// 140 BPM but built for groove, not aggression: a syncopated offbeat sub
// bassline sits out front and rolls, soft warm pads breathe behind it, hats are
// restrained, and a gentle plucky blip blips. Round, hypnotic, deep low end.
// Forked from minitek.c — same two-bus + sidechain + reverb + master infra, but
// reharmonized to D minor, retuned voices, deeper sidechain duck, a wetter
// delay-flavoured stab, and a soft sub-rumble reese under the groove.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o deeptek deeptek.c -lm
// Run:    ./deeptek --out out/deeptek-raw.wav

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
static double BPMV = 140, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.54;                   // a touch more swing for the roll

static uint32_t rng_s = 0x64656570; // "deep"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── microtiming / participatory discrepancies — THESIS DEEPENING ──────────────
// Keil, "Participatory Discrepancies" (1987/1995); Friberg & Sundström (2002) on
// swing ratios; Iyer (2002) on microtiming & groove. Small, *systematic* per-lane
// timing deviations from the grid create "feel"/the pocket — distinct from the
// metronomic grid. This pass makes the pocket a measurable, swept variable.
//
// (A) SWING RATIO IS TEMPO-DEPENDENT. Friberg & Sundström (2002), "Swing Ratios
// and Ensemble Timing in Jazz Performance," measured the long:short ratio of
// swung eighth-note pairs across tempo for professional jazz drummers and found
// it falls monotonically with tempo: ~3.0–3.5:1 at slow tempi (~120 BPM eighths)
// toward ~1:1 (even) as the eighth-note rate approaches the fast extreme. We fit
// a curve to their reported data and EVALUATE it at this track's tempo, so the
// off-eighth delay is derived, not hand-set.
// (B) PER-LANE OFFSETS are kept (negative = early/rushed, positive = laid-back),
// but each section scales them by a POCKET depth in [0,1]; one section is DEADPAN
// (depth 0 — every lane on the grid, no swing, no jitter) so the contrast is
// audible and the discrepancy statistic drops to ~0.
#define MS (SR / 1000.0)
static const double OFF_KICK =  0.0;   // anchor — dead on the grid
static const double OFF_HAT  = -6.0;   // hats pushed ~6 ms EARLY (drive)
static const double OFF_TICK = +14.0;  // backbeat laid-back ~14 ms LATE (pocket)
static const double OFF_SUB  = +4.0;   // bass a touch behind the kick
static const double OFF_PERC = +9.0;   // shaker/rim ghosts lean back
static const double OFF_LEAD = +12.0;  // counter-melody dragged for "human" feel

// Friberg & Sundström swing-ratio curve. Their Fig. data: swing ratio ≈ 3.3:1 at
// the slowest tempi, dropping through ~2:1 near mid tempo to ~1:1 (even) at the
// fastest. We model it as ratio(bpm) = 1 + (R0-1)*exp(-(bpm-B0)/TAUB), clamped
// to ≥1, fit so that ratio(120)≈2.0, ratio(150)≈1.5, ratio(200)→~1.15, and
// slow-tempo ratio approaches ~3.3 as bpm→B0. B0=100 anchors the slow end.
#define FS_R0   3.3     // long:short ratio at the slow anchor tempo B0
#define FS_B0   100.0   // slow anchor (BPM)
#define FS_TAUB 95.0    // decay constant (BPM) controlling how fast it flattens
static double swing_ratio_at(double bpm) {
    double r = 1.0 + (FS_R0 - 1.0) * exp(-(bpm - FS_B0) / FS_TAUB);
    return r < 1.0 ? 1.0 : r;
}
// Effective swing ratio + the long-part fraction of an eighth-pair at this tempo,
// computed once in main and shared. (long fraction = R/(R+1).)
static double SWING_RATIO = 1.0, LONG_FRAC = 0.5;

// per-hit humanizing jitter (Iyer): a few ms of random scatter so no two hits
// land identically; scaled small so the pocket stays coherent. POCKET depth
// (set per section) scales the scatter so deadpan sections have zero jitter.
static double POCKET = 1.0;
static inline double jit(double ms) { return POCKET * rnd2() * ms * MS / SR; }
// per-lane systematic offset, scaled by the current section's pocket depth.
static inline double off(double ms) { return POCKET * ms * MS / SR; }
// tempo-aware long–short swing on the off-8ths, ratio from Friberg & Sundström,
// scaled by POCKET depth: returns the extra delay (s) beyond the even split that
// pushes an off-8th later, so the eighth-pair realizes the measured long:short.
static double swing_push(int step) {
    if (step % 4 != 2) return 0.0;        // off-8th = 16th steps 2,6,10,14
    double eighth = BEAT / 2.0;
    double longp = eighth * (2.0 * LONG_FRAC); // long part of the pair (= R/(R+1)*beat)
    return POCKET * (longp - eighth * 0.5);    // extra beyond even, scaled by pocket
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
// kick — deep & round: a slower pitch sweep (96→44 Hz) and a longer body so it
// rolls rather than snaps; soft click. Stamps the sidechain trigger.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.40 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 44 + 52 * exp(-tt * 30.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 6.2);                  // longer body = warmer roll
        double click = exp(-tt * 280.0) * 0.42;       // softer transient
        double v = tanh((sin(ph) + click) * 1.7) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; restrained and soft. open=longer tail.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.10 : 0.035) * SR);
    double dec = open ? 50.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.34;    // softer than the template
        addD(s0 + i, v * lg, v * rg);
    }
}

// soft rim/tick — a tiny filtered noise blip for a gentle backbeat accent
// (instead of a loud clap, the deep groove gets a warm tick).
static void tick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.07 * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 2200.0 / SR), q = 1.0 / 1.4;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = band * exp(-tt * 30.0) * g * 0.6;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// shaker — a soft sustained noise burst, bandpassed bright; rides the 16ths to
// add forward motion. Quieter and longer than the tick.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0, low = 0, band = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 6500.0 / SR), q = 1.0 / 1.1;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = band * (1.0 - exp(-tt * 60.0)) * exp(-tt * 38.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride — a metallic shimmer: a cluster of inharmonic partials with a long decay,
// bandpassed. A soft ping on the offbeat for the late grooves.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.30 * SR); double low = 0, band = 0, prev = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    const double rat[5] = { 2.0, 3.01, 4.17, 5.43, 6.79 };
    double ph[5] = {0};
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, tone = 0;
        for (int k = 0; k < 5; k++) { ph[k] += TAU * (520.0 * rat[k]) / SR; tone += sin(ph[k]); }
        tone *= 0.2;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double in = tone * 0.7 + hp * 0.3;
        double fc = 2.0 * sin(M_PI * 5200.0 / SR), q = 1.0 / 1.3;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double v = band * exp(-tt * 9.0) * g * 0.3;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.25, v * 0.25);
    }
}

// tom — a short pitched membrane for fills: sine with a fast pitch drop + body.
static void tom(double note, double t, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(0.22 * SR); double ph = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f * (1.0 + 0.6 * exp(-tt * 26.0));
        ph += TAU * pf / SR;
        double amp = exp(-tt * 9.0);
        double v = tanh(sin(ph) * 1.4) * amp * g * 0.7;
        addD(s0 + i, v * lg, v * rg);
    }
}

// impact — a deep sub-boom + noise swell for drops/transitions (downbeat hit).
static void impact(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.9 * SR); double ph = 0, low = 0, band = 0, prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 38 + 60 * exp(-tt * 12.0);
        ph += TAU * pf / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 900.0 / SR), q = 1.0 / 1.2;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double boom = tanh(sin(ph) * 1.6) * exp(-tt * 3.0);
        double swell = low * exp(-tt * 2.0) * 0.4;
        double v = (boom + swell) * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — round tanh-saturated sine bass; a soft attack and gentle release so the
// rolling offbeat figure stays warm. A touch of 2nd harmonic for body.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.008 * SR), rel = (long)(0.05 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((sin(ph) + 0.20 * sin(ph * 2)) * 1.15) * env * g;
        addM(s0 + i, v, v);
    }
}

// reese — a detuned twin-saw drone an octave down through a fixed lowpass: a
// soft rumbling bed that fills the low-mid under the groove without aggression.
static void reese(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.08 * SR), rel = (long)(0.12 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0;
    double fc = 2.0 * sin(M_PI * 320.0 / SR), q = 1.0 / 1.0;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.008 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh(low * 1.4) * env * g;
        addM(s0 + i, v, v);
    }
}

// blip — a gentle plucky stab: a saw through a resonant SVF lowpass with a soft
// cutoff envelope and modest resonance (round, not acid-screamy). Wet on verb.
static void blip(double note, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double saw = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.004 * SR), rel = (long)(0.04 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        saw += f / SR; if (saw >= 1) saw -= 1;
        double in = 2.0 * saw - 1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 22.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.34, v * 0.34);             // wetter than the template
    }
}

// lead — a soft 2-op FM bell/marimba pluck for the hypnotic counter-melody. A
// short percussive envelope, gentle index, panned and lightly reverbed. This is
// the new top-line voice that answers the rolling bass up in the mids.
static void lead(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double cph = 0, mph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.004 * SR), rel = (long)(0.06 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double idx = 2.2 * exp(-tt * 9.0);             // bright attack, woody tail
        mph += TAU * (f * 2.0) / SR;
        cph += TAU * f / SR + idx * sin(mph);
        double env = exp(-tt * 5.0);
        if (i < att) env *= (double)i / att; else if (i > n - rel) env *= fmax(0, (double)(n - i) / rel);
        double v = sin(cph) * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.30, v * 0.30);
    }
}

// pad — a soft detuned triple-saw wash through a warm lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.5 * SR), rel = (long)(0.7 * SR);
    double s1 = 0, s2 = 0, s3 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1150.0 / SR), q = 1.0 / 0.85;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.006 / SR; if (s2 >= 1) s2 -= 1;
        s3 += f * 0.994 / SR; if (s3 >= 1) s3 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1 + 2 * s3 - 1) * 0.333;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.4, v * 0.4);
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
        double v = (hp - low) * p * p * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.35, v * 0.35);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_TICK = 8, L_SUB = 16, L_BLIP = 32, L_PAD = 64, L_REES = 128,
       L_SHAK = 256, L_RIDE = 512, L_LEAD = 1024 };

// at 140 BPM a bar is ~1.714s; 68 bars ≈ 117s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "deadpan", "grvB", "brk2", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 4, 8, 4 };
// EXPERIMENT — the pocket as the independent variable. POCKET_DEPTH[s] scales all
// systematic offsets, the swing push, and the per-hit jitter for section s. The
// sweep goes shallow→full→DEADPAN(0)→full→deeper, so the listener hears the same
// groove with the discrepancy dialled from machine-tight to a heavy laid-back
// pocket, with one section (brk1→"deadpan") flat-quantized as the control.
static const double POCKET_DEPTH[8] = { 0.6, 0.8, 1.0, 0.0, 1.0, 0.7, 1.15, 0.5 };
// accumulators for the measured discrepancy statistic, per section: we record
// every voiced onset's signed deviation from its quantized grid time (ms).
static double SEC_DEVSUM[8] = {0}, SEC_DEVSQ[8] = {0}, SEC_DEVABS[8] = {0};
static long SEC_DEVN[8] = {0};
static int CUR_SEC = 0;
static inline void log_dev(double t_actual, double t_grid) {
    double dms = (t_actual - t_grid) * 1000.0;
    SEC_DEVSUM[CUR_SEC] += dms; SEC_DEVSQ[CUR_SEC] += dms * dms;
    SEC_DEVABS[CUR_SEC] += fabs(dms); SEC_DEVN[CUR_SEC]++;
}
static int MASK[8] = {
    /*intro*/ L_KICK | L_SUB | L_PAD,
    /*build*/ L_KICK | L_CHAT | L_SHAK | L_SUB | L_PAD | L_REES,
    /*grvA */ L_KICK | L_CHAT | L_TICK | L_SHAK | L_SUB | L_BLIP | L_LEAD | L_PAD | L_REES,
    /*deadpan*/ L_KICK | L_CHAT | L_TICK | L_SHAK | L_SUB | L_BLIP | L_LEAD | L_PAD | L_REES,
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_TICK | L_SHAK | L_RIDE | L_SUB | L_BLIP | L_LEAD | L_PAD | L_REES,
    /*brk2 */ L_PAD | L_SUB | L_REES | L_LEAD,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_TICK | L_SHAK | L_RIDE | L_SUB | L_BLIP | L_LEAD | L_PAD | L_REES,
    /*outro*/ L_KICK | L_SUB | L_PAD | L_LEAD,
};
static int START[8];

// swung absolute time of (bar, 16th-step). The global off-16th swing displacement
// is scaled by POCKET so the deadpan section (POCKET 0) is flat-quantized, while
// fuller sections lean the offbeat 16ths back proportionally.
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += POCKET * (SWING - 0.5) * (BEAT / 2.0) + 0.5 * (BEAT / 2.0);
    return base;
}

// D natural-minor harmony. Bass roots walk i–VI–III–VII over 4 bars: Dm–Bb–F–C.
static const int ROOTS[4] = { 26, 22, 29, 24 };          // D1, Bb0, F1, C1
static const int BLIP_SCALE[6] = { 62, 65, 67, 69, 72, 74 }; // D4 F4 G4 A4 C5 D5
static const int PAD_CH[4][3] = { {50,53,57}, {46,50,53}, {53,57,60}, {48,52,55} }; // Dm Bb F C (mid)
static const int REES_NOTE[4] = { 26, 22, 29, 24 };      // reese follows the root, low
// counter-melody motif (D natural minor): a small singable phrase per chord that
// answers the bass up in the mids; chord-tone targets keep it consonant.
static const int LEAD_CH[4][4] = { {69,72,74,77}, {65,69,70,74}, {72,76,77,81}, {67,71,72,76} }; // over Dm Bb F C

int main(int argc, char **argv) {
    const char *out_path = "out/deeptek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    SWING_RATIO = swing_ratio_at(BPMV); LONG_FRAC = SWING_RATIO / (SWING_RATIO + 1.0);
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# deeptek.c · %g BPM · %d bars · %.1fs · deep rolling minimal techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // ── EXPERIMENT 1: tempo-dependent swing ratio (Friberg & Sundström 2002) ──
    // Print the fitted curve at reference tempi and the value at this track's BPM,
    // plus the eighth-pair durations that ratio implies and the per-lane offsets.
    fprintf(stderr, "\n# === EXPERIMENT 1: Friberg–Sundström swing-ratio curve ===\n");
    fprintf(stderr, "#   ratio(bpm) = 1 + (%.1f-1)*exp(-(bpm-%.0f)/%.0f), clamp>=1\n", FS_R0, FS_B0, FS_TAUB);
    fprintf(stderr, "#   bpm:  100   120   140   160   200\n#   R:  ");
    double refbpm[5] = {100,120,140,160,200};
    for (int i = 0; i < 5; i++) fprintf(stderr, "%5.2f ", swing_ratio_at(refbpm[i]));
    fprintf(stderr, "(:1)\n");
    double eighth_ms = (BEAT / 2.0) * 1000.0;
    fprintf(stderr, "#   @ %g BPM: swing ratio = %.3f:1  (long-frac %.3f)\n", BPMV, SWING_RATIO, LONG_FRAC);
    fprintf(stderr, "#   eighth-pair = %.1f ms; long = %.1f ms, short = %.1f ms (Δ %.1f ms)\n",
            eighth_ms, eighth_ms * LONG_FRAC, eighth_ms * (1.0 - LONG_FRAC),
            eighth_ms * (2.0 * LONG_FRAC - 1.0));
    fprintf(stderr, "#   per-lane microtiming offsets (ms, full pocket): "
            "kick %+.1f  hat %+.1f  tick %+.1f  sub %+.1f  perc %+.1f  lead %+.1f\n",
            OFF_KICK, OFF_HAT, OFF_TICK, OFF_SUB, OFF_PERC, OFF_LEAD);
    fprintf(stderr, "#   section pocket depths:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s=%.2f", ORDER[i], POCKET_DEPTH[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x...x.";   // restrained offbeat hats
    const char *P_OHAT = "............x...";   // a single open-hat lift per bar
    const char *P_SUB  = ".x.x.xx..x.x.x.x";   // syncopated offbeat rolling bass — out front
    const char *P_BLIP = ".....x......x..x";   // sparse plucky blip figure
    const char *P_SHAK = "x.xxx.xxx.xxx.xx";   // driving 16th shaker, gappy
    const char *P_RIDE = "..x...x...x...x.";   // ride pings on the offbeat
    const char *P_LEAD = "x..x..x...x..x..";   // counter-melody onsets
    // alt patterns for B-section mutation (phrase variety, per Iyer's idea that
    // groove lives in repetition-with-difference).
    const char *P_SUB2 = ".x.xx.x..xx.x.x.";   // busier roll variant
    const char *P_LED2 = "x...x.x..x.x..x.";   // syncopated counter-melody variant

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        CUR_SEC = s; POCKET = POCKET_DEPTH[s];   // EXPERIMENT: pocket depth per section
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            // per-phrase mutation: every other 4-bar phrase uses busier variants.
            int alt = ((b / 4) & 1);
            const char *psub  = alt ? P_SUB2 : P_SUB;
            const char *plead = alt ? P_LED2 : P_LEAD;
            int li = 0;                               // counter-melody note cursor
            for (int st = 0; st < 16; st++) {
                // MICROTIMING (Keil 1987/1995; Friberg & Sundström 2002; Iyer 2002):
                // each lane reads the grid time then nudges by its fixed offset +
                // the long–short swing on off-8ths + a small per-hit jitter, so the
                // ensemble lands in a "pocket" rather than on a quantized grid.
                double tmetro = bar * BAR + st * STEP;        // dead metronomic grid
                double tg = step_t(bar, st);                  // grid time (global swing)
                double tsw = tg + swing_push(st);             // + tempo swing (F&S ratio)
                // kick — anchor, dead-on the grid; broken on fill bars.
                if ((m & L_KICK) && st % 4 == 0 && !(fill && st == 12)) {
                    double th = tg + off(OFF_KICK); kick(th, 0.95); log_dev(th, tmetro);
                }
                // restrained hats — pushed EARLY for drive.
                if ((m & L_CHAT) && P_CHAT[st] == 'x') {
                    double th = tsw + off(OFF_HAT) + jit(2.0);
                    hat(th, 0.42 + 0.08 * (st % 4 == 2), 0, rnd2() * 0.3); log_dev(th, tmetro);
                }
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.10)
                    hat(tsw + off(OFF_HAT) + jit(3.0), 0.16, 0, rnd2() * 0.4); // light ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x')
                    hat(tsw + off(OFF_HAT) + jit(2.0), 0.34, 1, 0.12);
                // shaker — bright 16th drive, leans back a hair with jitter.
                if ((m & L_SHAK) && P_SHAK[st] == 'x') {
                    double th = tsw + off(OFF_PERC) + jit(3.0);
                    shaker(th, 0.28 + 0.06 * (st % 2 == 0), rnd2() * 0.6); log_dev(th, tmetro);
                }
                // ride — metallic offbeat ping, laid-back like the shaker.
                if ((m & L_RIDE) && P_RIDE[st] == 'x')
                    ride(tsw + off(OFF_PERC) + jit(2.0), 0.5, -0.4);
                // gentle warm tick on the backbeat — LAID-BACK ~14 ms (the pocket).
                if ((m & L_TICK) && (st == 4 || st == 12)) {
                    double th = tg + off(OFF_TICK) + jit(2.0); tick(th, 0.6); log_dev(th, tmetro);
                }
                // rolling syncopated offbeat sub — drags a touch behind the kick.
                if ((m & L_SUB) && psub[st] == 'x') {
                    double th = tsw + off(OFF_SUB) + jit(1.5);
                    sub(root + 12, th, STEP * 1.5, 0.92); log_dev(th, tmetro);
                }
                // plucky blip — round, walks the scale, evolves per phrase.
                if ((m & L_BLIP) && P_BLIP[st] == 'x') {
                    int note = BLIP_SCALE[(st + b * 2) % 6] + (phr == 3 ? 12 : 0);
                    double th = tsw + jit(2.0);
                    blip(note, th, STEP * 2.0, 0.40, 2.4 + 0.6 * (s >= 4), 2200, 520, rnd2() * 0.5);
                    log_dev(th, tmetro);
                }
                // counter-melody bell — dragged ~12 ms for the human top-line feel.
                if ((m & L_LEAD) && plead[st] == 'x') {
                    int note = LEAD_CH[phr % 4][li % 4]; li++;
                    double lg = 0.18 + 0.05 * (s == 6);       // a bit brighter by grvC
                    double th = tsw + off(OFF_LEAD) + jit(2.5);
                    lead(note, th, STEP * 3.0, lg, rnd2() * 0.5); log_dev(th, tmetro);
                }
            }
            // soft reese bed — one long note per bar following the root, low
            if (m & L_REES) reese(REES_NOTE[phr % 4], bar * BAR, BAR * 0.98, 0.16);
            // warm pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z], bar * BAR, BAR * 4 - 0.05, 0.11, (z - 1) * 0.45); }
            // tom fill — a little descending roll across the last 8th of fill bars
            // in groove sections, for movement into the next phrase.
            if (fill && (m & L_KICK) && (s == 2 || s == 4 || s == 6)) {
                int toms[4] = { 50, 48, 45, 43 };
                for (int z = 0; z < 4; z++)
                    tom(toms[z], step_t(bar, 12 + z) + off(OFF_PERC) + jit(2.0), 0.6, (z & 1) ? 0.4 : -0.4);
            }
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.14);
        }
        // impact hit at the top of each groove section (drop punctuation).
        if ((s == 2 || s == 4 || s == 6) && (m & L_KICK)) impact(START[s] * BAR, 0.5);
    }

    // ── EXPERIMENT 2: the measured pocket per section ─────────────────────────
    // For every voiced onset we logged its signed deviation (ms) from the dead
    // metronomic grid. Report per-section mean (the systematic lean), RMS (the
    // overall discrepancy magnitude), and mean |dev| — so the deadpan control
    // reads ~0 while full-pocket sections show a real, measurable spread.
    fprintf(stderr, "\n# === EXPERIMENT 2: measured microtiming discrepancy per section ===\n");
    fprintf(stderr, "#   section   pocket   onsets   mean(ms)   RMS(ms)   mean|dev|(ms)\n");
    double allsq = 0, allabs = 0; long alln = 0;
    for (int s = 0; s < 8; s++) {
        long n = SEC_DEVN[s]; if (!n) continue;
        double mean = SEC_DEVSUM[s] / n;
        double rms = sqrt(SEC_DEVSQ[s] / n);
        double mad = SEC_DEVABS[s] / n;
        fprintf(stderr, "#   %-9s %5.2f   %6ld   %+7.2f   %7.2f   %8.2f\n",
                ORDER[s], POCKET_DEPTH[s], n, mean, rms, mad);
        allsq += SEC_DEVSQ[s]; allabs += SEC_DEVABS[s]; alln += n;
    }
    fprintf(stderr, "#   ---------------------------------------------------------------\n");
    fprintf(stderr, "#   whole track: %ld onsets · RMS %.2f ms · mean|dev| %.2f ms\n",
            alln, sqrt(allsq / alln), allabs / alln);
    fprintf(stderr, "#   deadpan vs grvA contrast: RMS %.2f ms  vs  %.2f ms\n",
            sqrt(SEC_DEVSQ[3] / SEC_DEVN[3]), sqrt(SEC_DEVSQ[2] / SEC_DEVN[2]));

    // ── sidechain: deeper kick-triggered duck on the music bus (the pump) ──
    // deeper depth + a slower release for that pronounced rolling breathe.
    { double depth = 0.74, rel = exp(-1.0 / (0.14 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── light Schroeder reverb on the send (drums stay dry-ish), a bit wetter ──
    {
        double decay = 0.78, wet = 0.40, damp = 0.42;
        int CD[4]; double cds[4] = { 0.0297, 0.0353, 0.0431, 0.0497 };
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
    long fin = (long)(0.5 * SR), fout = (long)(1.8 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
