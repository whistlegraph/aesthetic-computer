// momabobasheep.c — C engine for the momabobasheep sleep mix, following
// the house pattern (pop/hellsine/c, pop/americomputadora/c): JS composes
// and bakes (bin/score-extract.mjs → c/bake.mjs → score.h + c/vocals/),
// this file is a dumb, fast DSP executor — zero composition logic, zero
// rng. ffmpeg only masters and encodes afterwards (c/render-c.mjs).
//
// Faithful port of bin/render-momabobasheep.mjs (2026-06-11):
//   • marimba voices — the modal mallet synth from pop/marimba/synths/
//     marimba.mjs (damped-sine modal bank + half-cosine mallet impulse +
//     Chamberlin SVF tube resonator), felt-mallet params, five groups
//     folded with pan + haas exactly like foldGroup();
//   • continuous drone bed — saw sub through a fixed warm SVF lowpass +
//     never-below-38Hz sub sine, 5-voice × 3-detuned-sine drone stack with
//     woozy vibrato/drift, tanh felting, one-pole damping, stereo chorus;
//   • whistle melody — near-pure sine + faint 2nd/3rd partials, fade-in
//     vibrato, pitch scoop, long sighing release;
//   • reverso sine bells — chorded swells peaking on chord arrivals;
//   • JEFFREY CHOIR — loop-crossfaded 16 s sustains (baked raw f32),
//     resampled to chord targets with glide, per-voice one-pole lowpass,
//     duration clamped to the tape, on a dedicated vocal bus that gets a
//     sidechain duck off the walking-bass onsets and its own longer
//     Schroeder hall;
//   • main-mix Schroeder, smooth dynamic-arc gain, NaN scrub, normalize
//     to 0.82 peak, stereo float32 WAV out (the pre-master).
//
// Phase-increment everywhere a phase exists; the constant-frequency modal
// sines use the 2·cos(w) resonator recurrence (exact sine, no per-sample
// transcendentals) so the whole 10:10 renders in seconds, not minutes.
//
// Build:  ./build.sh
// Run:    ./momabobasheep --out ../out/momabobasheep-raw.wav --vocals vocals/
// Tests:  ./momabobasheep --test marimba|drone|whistle|bell|choir --out t.wav
//         (renders ONLY that layer of the full score, for solo audition)

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

#include "score.h"

static const int SR = 44100;
static const char *OUT_PATH = "../out/momabobasheep-raw.wav";
static const char *VOC_DIR = "vocals/";
static const char *TEST_NAME = NULL;     // marimba|drone|whistle|bell|choir

static inline double m2f(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── buffers ─────────────────────────────────────────────────────────────
static long N = 0;
static float *outL, *outR;     // the mix
static float *bus;             // mono scratch, one marimba group at a time
static float *busL, *busR;     // the choir's dedicated vocal bus

static double t0_wall;
static double now_wall(void) {
    struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
static void report(const char *label) {
    fprintf(stderr, "   · %-28s %6.2fs\n", label, now_wall() - t0_wall);
}

// ── wav out — stereo float32, interleaved ───────────────────────────────
static int write_wav_f32_stereo(const char *path, const float *l, const float *r, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); uint32_t fsz = 16; fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&l[i], 4, 1, f); fwrite(&r[i], 4, 1, f); }
    fclose(f);
    return 1;
}

// ═══════════════════════════════════════════════════════════════════════
//  MARIMBA — modal mallet synthesis (port of renderMarimba()).
//  A struck wooden bar = a handful of exponentially-damped sines at the
//  undercut mode ratios, excited by a half-cosine mallet pulse, with a
//  high-Q SVF band-pass standing in for the tube resonator underneath.
// ═══════════════════════════════════════════════════════════════════════
typedef struct {
    int m;                       // number of modes
    double partials[3], amps[3], decays[3];
    double mallet, resQ, resGain, strike, attack, release;
} Preset;

// the four presets this piece plays (values from MARIMBA_PRESETS), with
// the renderer's FELT overrides already merged in per group below.
// (tremolo + stick-noise paths are omitted: none of these presets use them.)
static const Preset P_BASS = {       // bass marimba — mostly tube, felt swell
    2, {1.0, 4.0, 0}, {1.00, 0.18, 0}, {2.40, 0.55, 0},
    0.040, 25, 1.00, 0.5, 0.080, 0.3            // FELT_BASS: rocking chair, no thump
};
static const Preset P_VIBE = {       // vibraphone_off — the dream-haze pad
    3, {1.0, 4.0, 10.0}, {1.00, 0.22, 0.08}, {4.50, 0.90, 0.25},
    0.012, 22, 0.80, 0.5, 0.010, 0.5            // FELT_SOFT
};
static const Preset P_LEAD = {       // rosewood — the broken-chord arpeggio
    3, {1.0, 4.0, 9.2}, {1.00, 0.32, 0.10}, {1.60, 0.32, 0.09},
    0.016, 18, 0.65, 0.5, 0.014, 0.2            // FELT_LEAD
};
static const Preset P_ALTO = {       // rosewood again, softer felt (alto)
    3, {1.0, 4.0, 9.2}, {1.00, 0.32, 0.10}, {1.60, 0.32, 0.09},
    0.012, 18, 0.65, 0.5, 0.010, 0.2            // FELT_SOFT
};
static const Preset P_SPARK = {      // kalimba — the high chord sparkle
    3, {1.0, 5.9, 8.1}, {1.00, 0.20, 0.08}, {1.80, 0.30, 0.12},
    0.012, 14, 0.25, 0.5, 0.010, 0.3            // FELT_SOFT
};

// render one note into `bus` at its startSec. Constant-frequency modes →
// the 2cos(w) recurrence gives the exact sin(w·(i+1)) sequence the JS
// phase accumulator produces, and the decay envelope is a one-multiply
// geometric series — the whole modal bank runs transcendental-free.
static void marimba_note(const MEv *e, const Preset *P, double decayMul) {
    if (e->gain == 0) return;
    double f0 = m2f(e->midi);
    double longest = 0;
    for (int n = 0; n < P->m; n++) if (P->decays[n] > longest) longest = P->decays[n];
    longest *= decayMul;
    double tailSec = (e->dur > longest * 1.2 ? e->dur : longest * 1.2) + P->release;
    long ns = (long)ceil(tailSec * SR);
    long attS = (long)(P->attack * SR); if (attS < 1) attS = 1;
    long relS = (long)(P->release * SR); if (relS < 1) relS = 1;
    long releaseStart = (long)ceil(e->dur * SR);
    if (releaseStart < attS + 1) releaseStart = attS + 1;

    double ampSum = 0;
    for (int n = 0; n < P->m; n++) ampSum += P->amps[n];
    if (ampSum == 0) ampSum = 1;

    // per-mode oscillator + decay state
    double c2[3], s0[3], s1[3], amp0[3], denv[3], dk[3];
    double dt = 1.0 / SR;
    for (int n = 0; n < P->m; n++) {
        double w = TAU * (f0 * P->partials[n]) * dt;
        c2[n] = 2.0 * cos(w);
        s0[n] = 0.0;             // sin(w·0)
        s1[n] = sin(w);          // value at i = 0 (JS increments phase first)
        double shape = fabs(cos((n + 1) * M_PI * P->strike));
        amp0[n] = (P->amps[n] / ampSum) * (0.35 + 0.65 * shape);
        double alpha = log(1000.0) / fmax(0.01, P->decays[n] * decayMul);
        denv[n] = 1.0;           // exp(-alpha·0)
        dk[n] = exp(-alpha * dt);
    }

    long malletS = (long)((P->mallet) * SR); if (malletS < 1) malletS = 1;

    // tube resonator — Chamberlin SVF band-pass at f0
    double resK = 2.0 * sin(M_PI * fmin(f0, SR / 6.0) / SR);
    double resDamp = 1.0 / fmax(1.0, P->resQ);
    double svfLp = 0, svfBp = 0;

    long startIdx = (long)floor(e->t * SR);
    for (long i = 0; i < ns; i++) {
        double force = 0;
        if (i < malletS) force = 0.5 * (1.0 - cos((TAU * i) / malletS));
        double drive = force > 0 ? (0.6 + 0.4 * force) : 1.0;

        double dry = 0;
        for (int n = 0; n < P->m; n++) {
            dry += s1[n] * amp0[n] * denv[n] * drive;
            double s2 = c2[n] * s1[n] - s0[n];     // advance the exact sine
            s0[n] = s1[n]; s1[n] = s2;
            denv[n] *= dk[n];
        }

        double hi = dry - svfLp - resDamp * svfBp;
        svfBp += resK * hi;
        svfLp += resK * svfBp;
        double wet = dry + svfBp * P->resGain;

        double env = 1.0;
        if (i < attS) env = (double)i / attS;
        else if (i >= releaseStart) {
            double r = (double)(i - releaseStart) / relS;
            env = r >= 1 ? 0 : 1 - r;
        }
        if (env <= 0 && i > releaseStart) break;

        long dst = startIdx + i;
        if (dst >= 0 && dst < N) bus[dst] += (float)(wet * env * e->gain);
    }
}

// render a voice group into `bus`, then fold into outL/outR with pan +
// group gain, optionally widening the right channel with a small haas
// delay — a 1:1 port of the renderer's foldGroup().
static void fold_group(const MEv *evs, int n, const Preset *P, double decayMul,
                       double p, double gain, double haasMs, const char *label) {
    memset(bus, 0, N * sizeof(float));
    for (int e = 0; e < n; e++) marimba_note(&evs[e], P, decayMul);
    double a = (p + 1) * M_PI / 4, lg = cos(a), rg = sin(a);
    long haas = (long)floor((haasMs / 1000.0) * SR);
    for (long i = 0; i < N; i++) {
        double s = bus[i] * gain;
        outL[i] += (float)(s * lg);
        outR[i] += (float)((haas && i - haas >= 0 ? bus[i - haas] : bus[i]) * gain * rg);
    }
    report(label);
}

// ═══════════════════════════════════════════════════════════════════════
//  CONTINUOUS DRONE BED — the lush "always something playing" layer.
//  Chord targets come pre-lifted + pre-transposed in BARS[]; everything
//  else is the renderer verbatim: one-pole glide, fixed-cutoff SVF saw
//  sub, ≥38 Hz sub sine, 5×3 detuned woozy sines, tanh felting, one-pole
//  damping, stereo chorus off two modulated delay reads.
// ═══════════════════════════════════════════════════════════════════════
static double read_delay(const float *buf, long w, double d, long len) {
    double r = w - d; while (r < 0) r += len;
    long i0 = (long)floor(r); double frac = r - i0;
    double a = buf[i0 % len], b = buf[(i0 + 1) % len];
    return a + (b - a) * frac;
}

static void drone_render(void) {
    int ck = 0;
    double droneFs[5], subFs = m2f(BARS[0].subMidi);
    for (int n = 0; n < 5; n++) droneFs[n] = m2f(BARS[0].droneMidi[n]);
    double glide = exp(-1.0 / (1.8 * SR));
    double sawPh = 0, subPh = 0, svfLow = 0, svfBand = 0;
    const double Q = 0.42;
    double dPh[5][3] = {{0}};
    const double detune[3] = {1.0, 1.0035, 0.9966};   // ±6¢ chorus spread
    const double vGain[5] = {0.24, 0.24, 0.20, 0.17, 0.14};
    double droneLp = 0;
    long CL = (long)ceil(0.040 * SR);
    float *chBuf = calloc(CL, sizeof(float));
    long cw = 0;
    double chLfoA = 0, chLfoB = 0.27;
    long sneak = (long)(3.0 * SR);
    double f = 2.0 * sin(M_PI * 90.0 / SR);            // fixed gentle cutoff

    for (long i = 0; i < N; i++) {
        double t = (double)i / SR;
        while (ck + 1 < N_BARS && BARS[ck + 1].t <= t) ck++;
        double subTarget = m2f(BARS[ck].subMidi);
        subFs = subTarget + (subFs - subTarget) * glide;
        for (int n = 0; n < 5; n++) {
            double tg = m2f(BARS[ck].droneMidi[n]);
            droneFs[n] = tg + (droneFs[n] - tg) * glide;
        }
        // sneak in over 3 s; dissolve over the last 20 s before the seam
        double env = (i < sneak ? (double)i / sneak : 1.0) *
            (t > MASTER_SEC - 20 ? fmax(0, (MASTER_SEC - t) / 20.0) : 1.0);

        // — deep sub — fixed warm lowpass, no sweep
        sawPh += subFs / SR; if (sawPh >= 1) sawPh -= 1;
        double saw = 2 * sawPh - 1;
        svfLow += f * svfBand;
        svfBand += f * (saw - svfLow - Q * svfBand);
        double subHz = subFs * 0.5 >= 38 ? subFs * 0.5 : subFs;
        subPh += subHz / SR; if (subPh >= 1) subPh -= 1;
        double sub = sin(TAU * subPh);
        double subSig = (svfLow * 0.95 + sub * 1.0) * 0.18 * 0.8 * env;

        // — lush drone: 5 voices × 3 detuned sines, woozy —
        double dryD = 0;
        for (int n = 0; n < 5; n++) {
            double vib   = 0.006  * sin(TAU * 5.5 * t + n * 1.9);
            double drift = 0.0025 * (sin(TAU * 0.07 * t + n * 1.3)
                                   + sin(TAU * 0.113 * t + n * 2.7));
            double base = droneFs[n] * (1 + vib + drift);
            double v = 0;
            for (int d = 0; d < 3; d++) {
                dPh[n][d] += (base * detune[d]) / SR;
                if (dPh[n][d] >= 1) dPh[n][d] -= 1;
                v += sin(TAU * dPh[n][d]);
            }
            double breath = 0.74 + 0.26 * sin(TAU * 0.04 * t + n * 1.7);
            dryD += (v / 3.0) * vGain[n] * breath;
        }
        // felting — matte tanh + one-pole damping
        double felt = tanh(dryD * 1.5) * 0.62;
        droneLp += 0.10 * (felt - droneLp);
        double phased = droneLp * 0.5 * env;

        // — stereo chorus —
        chBuf[cw] = (float)phased;
        double dlA = (0.012 + 0.006 * (0.5 - 0.5 * cos(TAU * chLfoA))) * SR;
        double dlB = (0.013 + 0.006 * (0.5 - 0.5 * cos(TAU * chLfoB))) * SR;
        double rdL = read_delay(chBuf, cw, dlA, CL);
        double rdR = read_delay(chBuf, cw, dlB, CL);
        cw = (cw + 1) % CL;
        chLfoA += 0.11 / SR; if (chLfoA >= 1) chLfoA -= 1;
        chLfoB += 0.093 / SR; if (chLfoB >= 1) chLfoB -= 1;

        outL[i] += (float)(subSig + (phased * 0.5 + rdL * 0.5));
        outR[i] += (float)(subSig + (phased * 0.5 + rdR * 0.5));
    }
    free(chBuf);
    report("drone bed");
}

// ═══════════════════════════════════════════════════════════════════════
//  WHISTLE — near-pure tone (sine + faint 2nd/3rd partial), fade-in
//  vibrato, a tiny pitch scoop into each note, long sighing release.
// ═══════════════════════════════════════════════════════════════════════
static void whistle_note(const WEv *e) {
    double fw = m2f(e->midi);
    long i0 = (long)floor(e->t * SR), n = (long)ceil((e->dur + 0.45) * SR);
    double a = (e->pan + 1) * M_PI / 4, lg = cos(a), rg = sin(a);
    double att = 0.045 * SR, rel = 0.40 * SR, hold = e->dur * SR;
    double ph = 0;
    for (long i = 0; i < n; i++) {
        long dst = i0 + i; if (dst < 0 || dst >= N) continue;
        double x = (double)i / SR;
        double amp = i < att ? i / att : (i > hold ? fmax(0, 1 - (i - hold) / rel) : 1);
        double vibDepth = 0.006 * fmin(1, fmax(0, (x - 0.25) / 0.4));
        double vib = 1 + vibDepth * sin(TAU * 5.4 * x);
        double scoop = 1 - 0.03 * exp(-x / 0.05);
        ph += fw * vib * scoop / SR; if (ph >= 1) ph -= 1;
        double s = sin(TAU * ph) + 0.10 * sin(2 * TAU * ph) + 0.04 * sin(3 * TAU * ph);
        double o = s * amp * e->gain;
        outL[dst] += (float)(o * lg); outR[dst] += (float)(o * rg);
    }
}

// ═══════════════════════════════════════════════════════════════════════
//  REVERSO SINE BELLS — a chord of soft sines (+ a 0.22 octave shimmer)
//  swelling in on a squared ease, peaking at 82% of its span (right on a
//  chord arrival), then melting away on a cosine — never a hard cut.
// ═══════════════════════════════════════════════════════════════════════
#define BELL_PEAK 0.82
static void bell_note(const BellEv *e) {
    long i0 = (long)floor(e->t * SR), nB = (long)ceil(e->dur * SR);
    double a = (e->pan + 1) * M_PI / 4, lg = cos(a), rg = sin(a);
    double fs[3], ph[3] = {0}, ph2[3] = {0};
    for (int j = 0; j < 3; j++) fs[j] = m2f(e->midi[j]);
    for (long i = 0; i < nB; i++) {
        long dst = i0 + i; if (dst < 0 || dst >= N) continue;
        double x = (double)i / nB;
        double envB = x < BELL_PEAK
            ? (x / BELL_PEAK) * (x / BELL_PEAK)
            : 0.5 + 0.5 * cos(M_PI * (x - BELL_PEAK) / (1 - BELL_PEAK));
        double vib = 1 + 0.004 * sin(TAU * 5 * ((double)i / SR));
        double s = 0;
        for (int j = 0; j < 3; j++) {
            ph[j]  += fs[j] * vib / SR;     if (ph[j]  >= 1) ph[j]  -= 1;
            ph2[j] += fs[j] * 2 * vib / SR; if (ph2[j] >= 1) ph2[j] -= 1;
            s += sin(TAU * ph[j]) + 0.22 * sin(TAU * ph2[j]);
        }
        s = (s / 3.0) * envB * e->gain;
        outL[dst] += (float)(s * lg); outR[dst] += (float)(s * rg);
    }
}

// ═══════════════════════════════════════════════════════════════════════
//  JEFFREY CHOIR — execute the baked voice instructions. Each is a
//  loop-crossfaded 16 s sustain (raw f32, 48 k) read at a baked rate
//  (pitch shift × resample), optionally gliding in from the previous
//  shadow note, slow-enveloped, one-pole darkened, onto the vocal bus.
// ═══════════════════════════════════════════════════════════════════════
static float *take_buf[N_TAKES][2];      // [take][0]=vibrato [1]=flat

static int choir_load(void) {
    for (int t = 0; t < N_TAKES; t++) {
        for (int v = 0; v < 2; v++) {
            char path[1024];
            snprintf(path, sizeof path, "%s%s", VOC_DIR, TAKE_FILE[t][v]);
            FILE *fp = fopen(path, "rb");
            if (!fp) { fprintf(stderr, "  ! missing %s — run c/bake.mjs\n", path); return 0; }
            long n = TAKE_LEN[t][v];
            take_buf[t][v] = malloc(n * sizeof(float));
            if (fread(take_buf[t][v], sizeof(float), n, fp) != (size_t)n) {
                fprintf(stderr, "  ! short read %s\n", path); fclose(fp); return 0;
            }
            fclose(fp);
        }
    }
    return 1;
}

static void voice_into(const VoiceEv *v) {
    const float *buf = take_buf[v->take][v->flat];
    long blen = TAKE_LEN[v->take][v->flat];
    double rate = v->rate, rate0 = v->rate0;
    // never outrun the tape: clamp duration so the cosine release lands
    // INSIDE the sustain buffer (running off the end was an audible pop)
    double maxOut = floor((blen - 2) / fmax(rate, rate0));
    long nOut = (long)fmin(floor(v->dur * SR), maxOut);
    if (nOut < SR * 0.5) return;
    double glideN = fmin(floor(0.35 * SR), floor(nOut * 0.25));
    double att = fmin(1.4 * SR, nOut * 0.4), rel = fmin(2.2 * SR, nOut * 0.5);
    long i0 = (long)floor((v->t + v->stagger) * SR);
    double a = (v->pan + 1) * M_PI / 4, lg = cos(a), rg = sin(a);
    double k = 1 - exp(-TAU * v->fc / SR);
    double pos = 0, lp = 0;
    for (long i = 0; i < nOut; i++) {
        long dst = i0 + i; if (dst >= N) break;
        double r = (i < glideN && rate0 != rate)
            ? rate0 + (rate - rate0) * (0.5 - 0.5 * cos(M_PI * i / glideN))
            : rate;
        if (dst < 0) { pos += r; continue; }
        long pi = (long)floor(pos); double fr = pos - pi;
        if (pi + 1 >= blen) break;
        double s = buf[pi] * (1 - fr) + buf[pi + 1] * fr;
        lp += k * (s - lp);
        double env = i < att ? 0.5 - 0.5 * cos(M_PI * i / att) : 1;
        double rem = (double)(nOut - i);
        if (rem < rel) env *= 0.5 - 0.5 * cos(M_PI * rem / rel);
        double o = lp * env * v->gain;
        busL[dst] += (float)(o * lg); busR[dst] += (float)(o * rg);
        pos += r;
    }
}

// ── Schroeder reverb (4 comb + 2 allpass per channel) — shared shape for
// the vocal hall and the main wash, parameterised like the renderer's.
typedef struct { float *buf; long len, i; double lp; } Tank;
static Tank mk_tank(long len) { Tank t = { calloc(len, sizeof(float)), len, 0, 0 }; return t; }
static inline double comb_step(Tank *c, double x, double fb, double damp) {
    double y = c->buf[c->i];
    c->lp = y * (1 - damp) + c->lp * damp;
    c->buf[c->i] = (float)(x + c->lp * fb);
    c->i = (c->i + 1) % c->len;
    return y;
}
static inline double allp_step(Tank *c, double x) {
    double bv = c->buf[c->i];
    double y = -x + bv;
    c->buf[c->i] = (float)(x + bv * 0.5);
    c->i = (c->i + 1) % c->len;
    return y;
}

static void choir_render(void) {
    if (!choir_load()) return;
    for (int i = 0; i < N_VOICE; i++) voice_into(&EV_VOICE[i]);
    report("jeffrey choir (voices)");

    // — SIDECHAIN to the walking bass: gentle duck (42%, ~0.3 s recovery)
    //   slewed through a ~12 ms one-pole so loud sustains never click.
    {
        int oi = 0; double last = -1e9, duckLp = 1;
        const double depth = 0.42, tau = 0.30;
        double aCoef = 1 - exp(-1.0 / (0.012 * SR));
        for (long i = 0; i < N; i++) {
            double t = (double)i / SR;
            while (oi < N_ONSETS && BASS_ONSETS[oi] <= t) { last = BASS_ONSETS[oi]; oi++; }
            double duck = 1 - depth * exp(-(t - last) / tau);
            duckLp += (duck - duckLp) * aCoef;
            busL[i] *= (float)duckLp; busR[i] *= (float)duckLp;
        }
    }

    // — the vocal hall: a second, longer Schroeder just for the choir —
    {
        const double FB = 0.85, DAMP = 0.44, WET = 0.45, DRY = 0.30;
        const long CLd[4] = {1687, 1781, 1933, 2053}, CRd[4] = {1723, 1823, 1979, 2087};
        const long ALd[2] = {841, 667}, ARd[2] = {877, 701};
        Tank cl[4], cr[4], al[2], ar[2];
        for (int j = 0; j < 4; j++) { cl[j] = mk_tank(CLd[j]); cr[j] = mk_tank(CRd[j]); }
        for (int j = 0; j < 2; j++) { al[j] = mk_tank(ALd[j]); ar[j] = mk_tank(ARd[j]); }
        for (long i = 0; i < N; i++) {
            double wl = 0, wr = 0;
            for (int j = 0; j < 4; j++) wl += comb_step(&cl[j], busL[i], FB, DAMP);
            for (int j = 0; j < 4; j++) wr += comb_step(&cr[j], busR[i], FB, DAMP);
            for (int j = 0; j < 2; j++) wl = allp_step(&al[j], wl);
            for (int j = 0; j < 2; j++) wr = allp_step(&ar[j], wr);
            outL[i] += (float)(busL[i] * DRY + wl * WET);
            outR[i] += (float)(busR[i] * DRY + wr * WET);
        }
        for (int j = 0; j < 4; j++) { free(cl[j].buf); free(cr[j].buf); }
        for (int j = 0; j < 2; j++) { free(al[j].buf); free(ar[j].buf); }
    }
    report("jeffrey choir (duck + hall)");
}

// ── main-mix Schroeder — glues marimba + drone into one soft wash ───────
static void main_reverb(void) {
    const double FB = 0.84, DAMP = 0.28, WET = 0.17;
    const long CLd[4] = {1116, 1188, 1277, 1356}, CRd[4] = {1139, 1211, 1300, 1379};
    const long ALd[2] = {556, 441}, ARd[2] = {579, 464};
    Tank cl[4], cr[4], al[2], ar[2];
    for (int j = 0; j < 4; j++) { cl[j] = mk_tank(CLd[j]); cr[j] = mk_tank(CRd[j]); }
    for (int j = 0; j < 2; j++) { al[j] = mk_tank(ALd[j]); ar[j] = mk_tank(ARd[j]); }
    for (long i = 0; i < N; i++) {
        double xl = outL[i], xr = outR[i];
        double wl = 0, wr = 0;
        for (int j = 0; j < 4; j++) wl += comb_step(&cl[j], xl, FB, DAMP);
        for (int j = 0; j < 4; j++) wr += comb_step(&cr[j], xr, FB, DAMP);
        for (int j = 0; j < 2; j++) wl = allp_step(&al[j], wl);
        for (int j = 0; j < 2; j++) wr = allp_step(&ar[j], wr);
        outL[i] = (float)(xl + wl * WET);
        outR[i] = (float)(xr + wr * WET);
    }
    for (int j = 0; j < 4; j++) { free(cl[j].buf); free(cr[j].buf); }
    for (int j = 0; j < 2; j++) { free(al[j].buf); free(ar[j].buf); }
    report("main reverb");
}

// ── DYNAMIC ARC — the night narrative baked into the music level ────────
static double level_smooth_at(double t) {
    if (t <= LV_C[0]) return LV_V[0];
    if (t >= LV_C[N_LV - 1]) return LV_V[N_LV - 1];
    for (int i = 0; i < N_LV - 1; i++) {
        if (t >= LV_C[i] && t <= LV_C[i + 1]) {
            double f2 = (t - LV_C[i]) / (LV_C[i + 1] - LV_C[i]);
            return LV_V[i] + (LV_V[i + 1] - LV_V[i]) * f2;
        }
    }
    return 1;
}

// ── main ────────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) OUT_PATH = argv[++i];
        else if (!strcmp(argv[i], "--vocals") && i + 1 < argc) VOC_DIR = argv[++i];
        else if (!strcmp(argv[i], "--test") && i + 1 < argc) TEST_NAME = argv[++i];
    }
    t0_wall = now_wall();
    N = (long)ceil((double)RENDER_SEC * SR);
    outL = calloc(N, sizeof(float));
    outR = calloc(N, sizeof(float));
    bus  = calloc(N, sizeof(float));
    busL = calloc(N, sizeof(float));
    busR = calloc(N, sizeof(float));
    if (!outL || !outR || !bus || !busL || !busR) { fprintf(stderr, "✗ alloc\n"); return 1; }

    int all = TEST_NAME == NULL;
    int want_marimba = all || !strcmp(TEST_NAME, "marimba");
    int want_drone   = all || !strcmp(TEST_NAME, "drone");
    int want_whistle = all || !strcmp(TEST_NAME, "whistle");
    int want_bell    = all || !strcmp(TEST_NAME, "bell");
    int want_choir   = all || !strcmp(TEST_NAME, "choir");

    fprintf(stderr, "→ momabobasheep.c · %d s @ %d Hz · %d arp / %d bass / %d whistle / %d bells / %d choir voices\n",
            RENDER_SEC, SR, N_LEAD, N_BASS, N_WHISTLE, N_BELL, N_VOICE);

    if (want_marimba) {
        fold_group(EV_BASS,  N_BASS,  &P_BASS,  1.6,  0.00, 0.92,  0, "walking bass");
        fold_group(EV_PAD,   N_PAD,   &P_VIBE,  3.0,  0.00, 0.38, 14, "pad bloom");
        fold_group(EV_LEAD,  N_LEAD,  &P_LEAD,  2.8, -0.26, 0.50,  0, "arpeggio");
        fold_group(EV_ALTO,  N_ALTO,  &P_ALTO,  3.2, -0.08, 0.40,  0, "quaternary (alto)");
        fold_group(EV_SPARK, N_SPARK, &P_SPARK, 2.0,  0.46, 0.30,  0, "chord sparkle");
    }
    if (want_drone) drone_render();
    if (want_whistle) {
        for (int i = 0; i < N_WHISTLE; i++) whistle_note(&EV_WHISTLE[i]);
        report("whistle melody");
    }
    if (want_bell) {
        for (int i = 0; i < N_BELL; i++) bell_note(&EV_BELL[i]);
        report("reverso bells");
    }
    if (want_choir) choir_render();

    main_reverb();

    // dynamic arc — swell to the vivid dream, recede to dawn
    for (long i = 0; i < N; i++) {
        double g = level_smooth_at((double)i / SR);
        outL[i] *= (float)g; outR[i] *= (float)g;
    }

    // scrub + normalize to 0.82 peak (the master applies measured gain)
    long nan = 0;
    for (long i = 0; i < N; i++) {
        if (!isfinite(outL[i])) { outL[i] = 0; nan++; }
        if (!isfinite(outR[i])) { outR[i] = 0; nan++; }
    }
    if (nan) fprintf(stderr, "   ! scrubbed %ld non-finite samples\n", nan);
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double a = fabs(outL[i]); if (a > peak) peak = a;
        double b = fabs(outR[i]); if (b > peak) peak = b;
    }
    if (peak > 0) {
        double nrm = 0.82 / peak;
        for (long i = 0; i < N; i++) { outL[i] *= (float)nrm; outR[i] *= (float)nrm; }
    }
    fprintf(stderr, "   · peak %.4f → normalized 0.82\n", peak);

    if (!write_wav_f32_stereo(OUT_PATH, outL, outR, N)) {
        fprintf(stderr, "✗ couldn't write %s\n", OUT_PATH);
        return 1;
    }
    fprintf(stderr, "✓ %s (%.2fs render)\n", OUT_PATH, now_wall() - t0_wall);
    return 0;
}
