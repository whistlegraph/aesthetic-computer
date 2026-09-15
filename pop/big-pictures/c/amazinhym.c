// amazing.c — C renderer for Amazing Grace verse-1.
//
// Mirrors the hellsine C engine layout: per-sample additive synthesis,
// every voice keeps a normalized phase advanced by f/SR each tick.
//
// VOICES
//   organ_render    — pipe-organ drawbar additive (8 partials)
//   piano_render    — hammered-string additive, fast attack + decay
//   sine_pad_render — 3-osc sine cluster on chord, slow LFO, wide stereo
//                     ("accompaniment sines under the vocals")
//   glock_render    — 3-partial bell sparkle
//   kick_render     — synth kick (pitched sine sweep + click)
//   snare_render    — filtered noise burst + low tone (soft clap)
//   bass_render     — sub bass for low-end weight
//
// SCORE — verse 1 of "Amazing Grace, New Britain tune" (William Walker
// 1835), G major, 70 BPM, phrased as the .np phrases it: a one-beat
// pickup then a long syllable, so the bed lives on CELLS of 4 (or 6)
// beats that start on every strong syllable. See CELLS below.
//
//   0..INTRO       tonic pad swell + intro cells (kick, glock, hats)
//   INTRO          the vocal pickup "a-" (beat 0); "-ma-" is beat 1
//   beats 1..64    15 cells under the jeffrey-pvc sung lead
//   beats 64..68   plagal amen: IV
//   beat 68        I — the resolving hit, then RING_SEC of ring
//
// Build: ./build.sh
// Run:   ./amazinhym --out out/bed.wav [--intro 6.0] [--bpm 70] [--seconds N]

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ── config ────────────────────────────────────────────────────────────
static const int SR = 48000;
static double BPM = 70.0;
static double TOTAL_SEC = 0.0;           // 0 = intro + 68 beats + ring
static double INTRO_SEC = 6.0;            // pickup "a-" lands here (7 beats)
static const char *SEED_STR = "amazing";
static const char *OUT_PATH = NULL;

// ── shared mix buffers ────────────────────────────────────────────────
static long N = 0;
static float *L = NULL;
static float *R = NULL;
static float *WL = NULL;    // reverb wet send L
static float *WR = NULL;

// ── timing / reporting ────────────────────────────────────────────────
static double t0_wall = 0.0;
static double now_wall(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1e9;
}
__attribute__((format(printf, 1, 2)))
static void report(const char *fmt, ...) {
    fprintf(stderr, "[%6.2fs] ", now_wall() - t0_wall);
    va_list args; va_start(args, fmt);
    vfprintf(stderr, fmt, args); va_end(args);
    fputc('\n', stderr);
    fflush(stderr);
}

static inline double m2f(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── pipe-organ voice (drawbar additive) ──────────────────────────────
// 8 stops: 16' 8' 5⅓' 4' 2⅔' 2' 1⅗' 1⅓'.
// Slow attack, sustain, slow release, mild vibrato.
typedef struct {
    double atk, rel;
    double vib_rate, vib_depth;     // depth in cents
    double pan;
    double gain;
    double wet_send;
    double sub_amt;
    double upper_amt;
} OrganOpts;

static const double ORGAN_PARTIALS[8][2] = {
    {0.5,  0.55},   // 16' sub
    {1.0,  1.00},   // 8'  fund
    {1.5,  0.42},   // 5⅓' 12th
    {2.0,  0.78},   // 4'  oct
    {3.0,  0.32},   // 2⅔' 12th
    {4.0,  0.55},   // 2'  super-oct
    {5.0,  0.20},   // 1⅗' tierce
    {6.0,  0.18},   // 1⅓' larigot
};

static void organ_render(double t0, double dur, double midi, OrganOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.08;
    const double rel = opt.rel > 0 ? opt.rel : 0.30;
    const double vibR = opt.vib_rate > 0 ? opt.vib_rate : 4.8;
    const double vibD_cents = opt.vib_depth > 0 ? opt.vib_depth : 5.0;
    const double vibD = pow(2.0, vibD_cents / 1200.0) - 1.0;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    const double sub_amt = opt.sub_amt >= 0 ? opt.sub_amt : 1.0;
    const double upper_amt = opt.upper_amt >= 0 ? opt.upper_amt : 1.0;

    double phs[8] = {0};
    double vibPhase = 0.0;
    const double dVibPhase = vibR / (double)SR;

    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)            env = t / atk;
        else if (tRem < rel)    env = tRem / rel;
        else                    env = 1.0;
        if (env < 0) env = 0;

        vibPhase += dVibPhase; if (vibPhase >= 1.0) vibPhase -= 1.0;
        const double vib = 1.0 + vibD * sin(TAU * vibPhase);

        double sample = 0.0;
        double ampsum = 0.0;
        for (int p = 0; p < 8; p++) {
            const double ratio = ORGAN_PARTIALS[p][0];
            double amp = ORGAN_PARTIALS[p][1];
            if (p == 0)        amp *= sub_amt;
            else if (p >= 4)   amp *= upper_amt;
            ampsum += amp;

            const double dPhs = (f * ratio * vib) / (double)SR;
            phs[p] += dPhs; if (phs[p] >= 1.0) phs[p] -= 1.0;
            sample += amp * sin(TAU * phs[p]);
        }
        sample *= (env * opt.gain) / ampsum;

        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── piano voice (hammered additive, mild inharmonicity) ──────────────
typedef struct {
    double decay_s;
    double velocity;
    double pan;
    double gain;
    double wet_send;
} PianoOpts;

static void piano_render(double t0, double dur, double midi, PianoOpts opt) {
    const double f = m2f(midi);
    const double decay = opt.decay_s > 0 ? opt.decay_s : 3.0;
    const double vel = opt.velocity > 0 ? opt.velocity : 0.7;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    const double attack = 0.004 + 0.006 * (1.0 - vel);
    const double B_inharm = 0.00012;

    double partials[10][2];
    static const double base_amps[10] = {
        1.00, 0.62, 0.42, 0.30, 0.22, 0.16, 0.11, 0.08, 0.05, 0.03
    };
    for (int n = 0; n < 10; n++) {
        const double n1 = n + 1.0;
        const double stretch = sqrt(1.0 + B_inharm * n1 * n1);
        partials[n][0] = n1 * stretch;
        partials[n][1] = base_amps[n] * pow(vel, 0.7) *
                         (n < 3 ? 1.0 : (0.4 + 0.6 * vel));
    }

    double phs[10] = {0};
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    double ampsum = 0;
    for (int n = 0; n < 10; n++) ampsum += partials[n][1];
    const double normG = opt.gain / ampsum;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env;
        if (t < attack) env = t / attack;
        else            env = exp(-(t - attack) / decay);
        if (t > 0.005 && env < 1e-6) break;

        double sample = 0.0;
        for (int n = 0; n < 10; n++) {
            const double ratio = partials[n][0];
            const double amp = partials[n][1];
            const double partial_env = (n < 2) ? 1.0
                                              : exp(-t * (0.25 + 0.45 * n));
            const double dPhs = (f * ratio) / (double)SR;
            phs[n] += dPhs; if (phs[n] >= 1.0) phs[n] -= 1.0;
            sample += amp * partial_env * sin(TAU * phs[n]);
        }
        sample *= env * normG;

        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── sine pad voice (3-osc cluster on chord, slow LFO) ────────────────
// The "accompaniment sines under the vocals". Soft sustained chord pad
// — root + 3rd + 5th with slow tremolo, panned wide.
typedef struct {
    double atk, rel;
    double pan;
    double gain;
    double wet_send;
    double lfo_rate;     // Hz
    double lfo_depth;    // 0..1
} SinePadOpts;

static void sine_pad_render(double t0, double dur,
                            int midi_root, int midi_third, int midi_fifth,
                            SinePadOpts opt) {
    const double atk = opt.atk > 0 ? opt.atk : 0.30;
    const double rel = opt.rel > 0 ? opt.rel : 0.50;
    const double lfo_rate = opt.lfo_rate > 0 ? opt.lfo_rate : 0.18;
    const double lfo_depth = opt.lfo_depth >= 0 ? opt.lfo_depth : 0.15;

    double phs[3] = {0};
    double lfoPhase = 0.0;
    const double dLfo = lfo_rate / (double)SR;
    const double f[3] = {
        m2f(midi_root), m2f(midi_third), m2f(midi_fifth)
    };

    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;

        lfoPhase += dLfo; if (lfoPhase >= 1.0) lfoPhase -= 1.0;
        const double lfo = 1.0 - lfo_depth + lfo_depth *
                           (0.5 * (1.0 + sin(TAU * lfoPhase)));

        double s[3];
        for (int k = 0; k < 3; k++) {
            const double dPhs = f[k] / (double)SR;
            phs[k] += dPhs; if (phs[k] >= 1.0) phs[k] -= 1.0;
            s[k] = sin(TAU * phs[k]);
        }
        // Wide stereo: root centered, third left, fifth right
        const double sL = (s[0] * 0.50 + s[1] * 0.70 + s[2] * 0.35);
        const double sR = (s[0] * 0.50 + s[1] * 0.35 + s[2] * 0.70);
        const double scale = env * lfo * opt.gain;
        L[i] += sL * scale;
        R[i] += sR * scale;
        WL[i] += sL * scale * opt.wet_send;
        WR[i] += sR * scale * opt.wet_send;
    }
}

// ── sine lead voice (sine + 2nd harmonic, vibrato) ───────────────────
// Used for the MELODY-doubling line — needs to cut through the pad
// and the vocal without being a piano hit. Sine fundamental + 15%
// 2nd harmonic for body, mild vibrato.
typedef struct {
    double atk, rel;
    double vib_rate, vib_depth;   // depth in cents
    double pan;
    double gain;
    double wet_send;
} SineLeadOpts;
static void sine_lead_render(double t0, double dur, double midi, SineLeadOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.03;
    const double rel = opt.rel > 0 ? opt.rel : 0.10;
    const double vibR = opt.vib_rate > 0 ? opt.vib_rate : 4.5;
    const double vibD_cents = opt.vib_depth > 0 ? opt.vib_depth : 8.0;
    const double vibD = pow(2.0, vibD_cents / 1200.0) - 1.0;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    double phs1 = 0, phs2 = 0, vibPhase = 0;
    const double dVib = vibR / (double)SR;

    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;

        vibPhase += dVib; if (vibPhase >= 1.0) vibPhase -= 1.0;
        const double vib = 1.0 + vibD * sin(TAU * vibPhase);

        const double dP1 = (f * vib) / (double)SR;
        const double dP2 = (f * 2.0 * vib) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;

        const double sample = env * opt.gain *
                              (0.85 * sin(TAU * phs1) +
                               0.18 * sin(TAU * phs2));
        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── strings voice (additive sine bowed-string, slow attack + vibrato)
// 8 partials with smooth roll-off — sounds like a small string section.
// Used to thicken the sine pad layer with warm sustained character.
typedef struct {
    double atk, rel;
    double vib_rate, vib_depth;   // cents
    double pan;
    double gain;
    double wet_send;
} StringsOpts;
static const double STRINGS_PARTIALS[8][2] = {
    {1.0, 1.00},  // fundamental
    {2.0, 0.66},  // octave
    {3.0, 0.48},  // 12th
    {4.0, 0.34},  // 15th
    {5.0, 0.24},
    {6.0, 0.17},
    {7.0, 0.12},
    {8.0, 0.08},
};
static void strings_render(double t0, double dur, double midi, StringsOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.45;
    const double rel = opt.rel > 0 ? opt.rel : 0.55;
    const double vibR = opt.vib_rate > 0 ? opt.vib_rate : 4.6;
    const double vibD_cents = opt.vib_depth > 0 ? opt.vib_depth : 8.0;
    const double vibD = pow(2.0, vibD_cents / 1200.0) - 1.0;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    double phs[8] = {0};
    double vibPhase = 0.0;
    const double dVibPhase = vibR / (double)SR;

    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    double ampsum = 0.0;
    for (int p = 0; p < 8; p++) ampsum += STRINGS_PARTIALS[p][1];

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;

        vibPhase += dVibPhase; if (vibPhase >= 1.0) vibPhase -= 1.0;
        const double vib = 1.0 + vibD * sin(TAU * vibPhase);

        double sample = 0.0;
        for (int p = 0; p < 8; p++) {
            const double ratio = STRINGS_PARTIALS[p][0];
            const double amp = STRINGS_PARTIALS[p][1];
            const double dPhs = (f * ratio * vib) / (double)SR;
            phs[p] += dPhs; if (phs[p] >= 1.0) phs[p] -= 1.0;
            sample += amp * sin(TAU * phs[p]);
        }
        sample *= (env * opt.gain) / ampsum;

        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── tambourine voice (bright noise + metallic ring) ──────────────────
typedef struct {
    double pan;
    double gain;
    double decay_s;
} TambOpts;
static void tambourine_render(double t0, TambOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.22;
    const double max_dur = decay * 5.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(t0 * 4567.0 + 13) | 1;
    double ring1 = 0.0, ring2 = 0.0;
    const double rf1 = 5800.0, rf2 = 9200.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.001) ? (t / 0.001) : exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        ring1 += rf1 / SR; if (ring1 >= 1.0) ring1 -= 1.0;
        ring2 += rf2 / SR; if (ring2 >= 1.0) ring2 -= 1.0;
        const double sample = (0.55 * noise + 0.30 * sin(TAU * ring1) +
                               0.18 * sin(TAU * ring2)) * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── kalimba voice (4-partial inharmonic mid-range pluck) ─────────────
typedef struct {
    double decay_s;
    double pan;
    double gain;
    double wet_send;
} KalimbaOpts;
static void kalimba_render(double t0, double midi, KalimbaOpts opt) {
    const double f = m2f(midi);
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.70;
    const double max_dur = decay * 5.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double phs[4] = {0};
    static const double K_RATIOS[4] = { 1.00, 2.42, 3.99, 5.81 };
    static const double K_AMPS[4]   = { 1.00, 0.55, 0.32, 0.18 };
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.003) ? (t / 0.003) : exp(-(t - 0.003) / decay);
        if (t > 0.005 && env < 1e-6) break;
        double sample = 0.0;
        for (int k = 0; k < 4; k++) {
            const double dPhs = (f * K_RATIOS[k]) / (double)SR;
            phs[k] += dPhs; if (phs[k] >= 1.0) phs[k] -= 1.0;
            const double partial_env = exp(-t * (0.5 + 0.3 * k));
            sample += K_AMPS[k] * partial_env * sin(TAU * phs[k]);
        }
        sample *= env * opt.gain / 2.05;
        const double sL = sample * (1.0 - 0.5 * opt.pan);
        const double sR = sample * (1.0 + 0.5 * opt.pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * opt.wet_send;
        WR[i] += sR * opt.wet_send;
    }
}

// ── glockenspiel voice (3-partial inharmonic bell) ───────────────────
typedef struct {
    double decay_s;
    double pan;
    double gain;
    double wet_send;
} GlockOpts;
static void glock_render(double t0, double dur, double midi, GlockOpts opt) {
    const double f = m2f(midi);
    const double decay = opt.decay_s > 0 ? opt.decay_s : 1.2;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    double phs1 = 0, phs2 = 0, phs3 = 0;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.002) ? (t / 0.002) : exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-6) break;
        const double dP1 = f / (double)SR;
        const double dP2 = (f * 2.76) / (double)SR;
        const double dP3 = (f * 5.40) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;
        phs3 += dP3; if (phs3 >= 1.0) phs3 -= 1.0;
        const double sample = env * opt.gain *
                              (0.80 * sin(TAU * phs1) +
                               0.40 * sin(TAU * phs2) +
                               0.20 * sin(TAU * phs3));
        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── kick voice (pitched sine sweep + click transient) ────────────────
typedef struct {
    double pan;
    double gain;
    double pitch_hi;
    double pitch_lo;
    double pitch_dur;
    double decay_s;
} KickOpts;
static void kick_render(double t0, KickOpts opt) {
    const double pitch_hi = opt.pitch_hi > 0 ? opt.pitch_hi : 160.0;
    const double pitch_lo = opt.pitch_lo > 0 ? opt.pitch_lo : 52.0;
    const double pitch_dur = opt.pitch_dur > 0 ? opt.pitch_dur : 0.045;
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.28;
    const double max_dur = decay * 5.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double phase = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double freq;
        if (t < pitch_dur) freq = pitch_hi * pow(pitch_lo / pitch_hi, t / pitch_dur);
        else freq = pitch_lo;
        double env;
        if (t < 0.002) env = t / 0.002;
        else env = exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-5) break;
        const double dPhs = freq / (double)SR;
        phase += dPhs; if (phase >= 1.0) phase -= 1.0;
        const double s = sin(TAU * phase) * env * opt.gain;
        L[i] += s * (1.0 - 0.5 * opt.pan);
        R[i] += s * (1.0 + 0.5 * opt.pan);
    }
}

// ── snare voice (filtered noise + low tone burst) ────────────────────
typedef struct {
    double pan;
    double gain;
    double decay_s;
    double brightness;   // 0..1, controls noise highpass cutoff
} SnareOpts;
static void snare_render(double t0, SnareOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.10;
    const double max_dur = decay * 6.0;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.85;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double tonePhase = 0.0;
    const double toneFreq = 220.0;
    // xorshift noise seeded by t0
    uint32_t s = (uint32_t)(t0 * 1000003.0) | 1;
    // 1-pole highpass state
    double hp_in_prev = 0.0, hp_out_prev = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env;
        if (t < 0.001) env = t / 0.001;
        else env = exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        // Highpass for snare brightness
        const double hp_out = bright * (hp_out_prev + noise - hp_in_prev);
        hp_in_prev = noise; hp_out_prev = hp_out;
        // Tone
        const double dPhs = toneFreq / (double)SR;
        tonePhase += dPhs; if (tonePhase >= 1.0) tonePhase -= 1.0;
        const double tone = sin(TAU * tonePhase);
        const double sample = (0.75 * hp_out + 0.25 * tone) * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── hi-hat voice (very short bright noise burst) ─────────────────────
typedef struct {
    double pan;
    double gain;
    double decay_s;     // very short — closed hat ~20-30 ms
    double brightness;  // 0..1, highpass strength
} HatOpts;
static void hat_render(double t0, HatOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.025;
    const double max_dur = decay * 6.0;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.95;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(t0 * 7919.0 + 31) | 1;
    double hp_in_prev = 0.0, hp_out_prev = 0.0;
    // Double highpass for extra brightness
    double hp2_in_prev = 0.0, hp2_out_prev = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.001) ? (t / 0.001) : exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        // Two-pole-ish highpass for "tssss" character
        const double hp1 = bright * (hp_out_prev + noise - hp_in_prev);
        hp_in_prev = noise; hp_out_prev = hp1;
        const double hp2 = bright * (hp2_out_prev + hp1 - hp2_in_prev);
        hp2_in_prev = hp1; hp2_out_prev = hp2;
        const double sample = hp2 * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── shaker voice (band-pass noise burst, slightly longer) ────────────
typedef struct {
    double pan;
    double gain;
    double decay_s;
} ShakerOpts;
static void shaker_render(double t0, ShakerOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.06;
    const double max_dur = decay * 6.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(t0 * 5381.0 + 17) | 1;
    // Bandpass via highpass + lowpass cascade
    double hp_in = 0, hp_out = 0;
    double lp_out = 0;
    const double hp_a = 0.92;
    const double lp_a = 0.55;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.002) ? (t / 0.002) : exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp = hp_a * (hp_out + noise - hp_in);
        hp_in = noise; hp_out = hp;
        lp_out = lp_out * (1.0 - lp_a) + hp * lp_a;
        const double sample = lp_out * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── sub bass voice (root + octave, slow attack) ──────────────────────
typedef struct {
    double atk, rel;
    double pan;
    double gain;
} BassOpts;
static void bass_render(double t0, double dur, double midi, BassOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.20;
    const double rel = opt.rel > 0 ? opt.rel : 0.40;
    const double pan = opt.pan;
    double phs1 = 0, phs2 = 0;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;
        const double dP1 = f / (double)SR;
        const double dP2 = (f * 2.0) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;
        const double sample = env * opt.gain *
                              (0.85 * sin(TAU * phs1) +
                               0.25 * sin(TAU * phs2));
        L[i] += sample * (1.0 - 0.5 * pan);
        R[i] += sample * (1.0 + 0.5 * pan);
    }
}

// ── Schroeder reverb (small church/room) ─────────────────────────────
typedef struct { float *buf; long size; long idx; float feedback; } Comb;
typedef struct { float *buf; long size; long idx; } Allpass;
static void comb_init(Comb *c, long size, float fb) {
    c->buf = calloc(size, sizeof(float));
    c->size = size; c->idx = 0; c->feedback = fb;
}
static inline float comb_tick(Comb *c, float in) {
    float out = c->buf[c->idx];
    c->buf[c->idx] = in + out * c->feedback;
    c->idx++; if (c->idx >= c->size) c->idx = 0;
    return out;
}
static void allpass_init(Allpass *a, long size) {
    a->buf = calloc(size, sizeof(float));
    a->size = size; a->idx = 0;
}
static inline float allpass_tick(Allpass *a, float in) {
    float bufout = a->buf[a->idx];
    float out = -in + bufout;
    a->buf[a->idx] = in + bufout * 0.5f;
    a->idx++; if (a->idx >= a->size) a->idx = 0;
    return out;
}
static void apply_reverb(double wet_mix) {
    static const int comb_dl_L[4] = { 1687, 1601, 2053, 2251 };
    static const int comb_dl_R[4] = { 1739, 1647, 2099, 2293 };
    const double sr_scale = (double)SR / 44100.0;
    Comb cL[4], cR[4];
    for (int i = 0; i < 4; i++) {
        comb_init(&cL[i], (long)(comb_dl_L[i] * sr_scale), 0.82f);
        comb_init(&cR[i], (long)(comb_dl_R[i] * sr_scale), 0.82f);
    }
    Allpass apL[2], apR[2];
    allpass_init(&apL[0], (long)(556 * sr_scale));
    allpass_init(&apL[1], (long)(441 * sr_scale));
    allpass_init(&apR[0], (long)(605 * sr_scale));
    allpass_init(&apR[1], (long)(476 * sr_scale));
    for (long i = 0; i < N; i++) {
        float inL = WL[i], inR = WR[i];
        float sumL = 0, sumR = 0;
        for (int k = 0; k < 4; k++) {
            sumL += comb_tick(&cL[k], inL);
            sumR += comb_tick(&cR[k], inR);
        }
        sumL = allpass_tick(&apL[0], sumL);
        sumL = allpass_tick(&apL[1], sumL);
        sumR = allpass_tick(&apR[0], sumR);
        sumR = allpass_tick(&apR[1], sumR);
        L[i] += sumL * wet_mix;
        R[i] += sumR * wet_mix;
    }
    for (int i = 0; i < 4; i++) { free(cL[i].buf); free(cR[i].buf); }
    for (int i = 0; i < 2; i++) { free(apL[i].buf); free(apR[i].buf); }
}

// ── SCORE: verse-1 in G major ────────────────────────────────────────
typedef struct {
    int bass;
    int chord[3];   // root, third, fifth midi (pad + strings register)
    const char *name;
} Chord;

static const Chord CH_G = { 43, {55, 59, 62}, "G" };
static const Chord CH_C = { 48, {52, 60, 64}, "C" };
static const Chord CH_D = { 50, {54, 57, 62}, "D" };

// ── MELODY (sine lead doubling the vocal) ────────────────────────────
// pop/big-pictures/amaythingra.np verse 1: 28 syllables, 64 beats.
// Beat 0 is the pickup "a-"; the first strong syllable "-ma-" is beat 1.
typedef struct { int midi; int beats; } MelodyNote;
static const MelodyNote MELODY[] = {
    // Line 1: a-mazing grace, how sweet the sound
    {50, 1}, {55, 3}, {59, 1}, {55, 3}, {59, 1}, {57, 3}, {55, 1}, {59, 5},
    // Line 2: that saved a wretch like me
    {50, 1}, {52, 3}, {50, 1}, {59, 3}, {55, 1}, {59, 5},
    // Line 3: I once was lost but now am found
    {50, 1}, {55, 3}, {62, 1}, {62, 3}, {59, 1}, {62, 3}, {57, 1}, {55, 5},
    // Line 4: was blind but now I see
    {59, 1}, {62, 3}, {62, 1}, {59, 3}, {57, 1}, {55, 5},
};
#define MELODY_LEN ((int)(sizeof(MELODY) / sizeof(MELODY[0])))
#define MELODY_BEATS 64

// ── CELLS — the vocal's own accent grid ──────────────────────────────
// The .np phrases the tune as (1 + 3) beats: a one-beat pickup, then a
// long syllable. Every long syllable opens a cell that lasts until the
// next one — four beats, or six where a phrase end is held. The drums,
// pads and chords live on THIS grid, so the kick lands with "-MA-",
// "GRACE", "SWEET", "SOUND" … and never argues with the singer.
// Chords are the standard lead-sheet in G:
//   Amazing (G) grace how sweet the (C) sound that (G) saved a wretch
//   like (D) me. I (G) once was lost but (C) now am found, was (G)
//   blind but (D) now I (G) see.   + a plagal amen (C → G) after.
typedef struct { int beat; int len; const Chord *chord; int line; } Cell;
static const Cell CELLS[] = {
    {  1, 4, &CH_G, 1 },   // -ma-zing
    {  5, 4, &CH_G, 0 },   // grace how
    {  9, 4, &CH_G, 0 },   // sweet the
    { 13, 6, &CH_C, 0 },   // sound — that
    { 19, 4, &CH_G, 1 },   // saved a
    { 23, 4, &CH_G, 0 },   // wretch like
    { 27, 6, &CH_D, 0 },   // me — i
    { 33, 4, &CH_G, 1 },   // once was
    { 37, 4, &CH_G, 0 },   // lost but
    { 41, 4, &CH_C, 0 },   // now am
    { 45, 6, &CH_C, 0 },   // found — was
    { 51, 4, &CH_G, 1 },   // blind but
    { 55, 4, &CH_D, 0 },   // now i
    { 59, 5, &CH_G, 0 },   // see
    { 64, 4, &CH_C, 0 },   // a- (amen, IV)
};
#define CELL_COUNT ((int)(sizeof(CELLS) / sizeof(CELLS[0])))
#define AMEN_BEAT 68            // -men (I): the final resolving hit
#define RING_SEC 5.0            // natural ring after the amen

// Beat → seconds. Beat 0 is the vocal pickup at INTRO_SEC.
static double SPB_G = 0.0;
static inline double tb(double beat) { return INTRO_SEC + beat * SPB_G; }

// ── one accompaniment cell ───────────────────────────────────────────
static void cell_render(const Cell *cl, int idx) {
    const double SPB = SPB_G;
    const Chord *c = cl->chord;
    const double t0 = tb(cl->beat);
    const double len = cl->len * SPB;
    const int amen = (cl->beat >= MELODY_BEATS);

    // Low sine pad — one octave down. Main harmonic body.
    sine_pad_render(t0, len + 0.25,
                    c->chord[0] - 12, c->chord[1] - 12, c->chord[2] - 12,
                    (SinePadOpts){ .atk = 0.30, .rel = 0.55,
                                   .pan = 0.0, .gain = 0.20,
                                   .wet_send = 0.22,
                                   .lfo_rate = 0.20, .lfo_depth = 0.18 });
    // Upper sine pad — original octave, softer, slow wider stereo
    sine_pad_render(t0, len + 0.25,
                    c->chord[0], c->chord[1], c->chord[2],
                    (SinePadOpts){ .atk = 0.40, .rel = 0.60,
                                   .pan = 0.0, .gain = 0.10,
                                   .wet_send = 0.24,
                                   .lfo_rate = 0.16, .lfo_depth = 0.20 });
    // Strings — warm bowed-pad doubling the chord, third/fifth spread.
    strings_render(t0, len + 0.20, c->chord[0],
        (StringsOpts){ .atk = 0.35, .rel = 0.55, .vib_rate = 4.5,
                       .vib_depth = 7.0, .pan = 0.0,
                       .gain = 0.10, .wet_send = 0.20 });
    strings_render(t0, len + 0.20, c->chord[1],
        (StringsOpts){ .atk = 0.40, .rel = 0.55, .vib_rate = 4.8,
                       .vib_depth = 8.0, .pan = -0.18,
                       .gain = 0.08, .wet_send = 0.22 });
    strings_render(t0, len + 0.20, c->chord[2],
        (StringsOpts){ .atk = 0.45, .rel = 0.55, .vib_rate = 5.0,
                       .vib_depth = 8.0, .pan = +0.18,
                       .gain = 0.08, .wet_send = 0.22 });

    // Drums — kick on the strong syllable, soft clap two beats later,
    // hats on every 8th. Six-beat cells get a second kick on beat 4 so
    // the held note keeps walking.
    kick_render(t0,
        (KickOpts){ .pan = 0.0, .gain = 0.70,
                    .pitch_hi = 165, .pitch_lo = 52,
                    .pitch_dur = 0.045, .decay_s = 0.34 });
    snare_render(t0 + 2 * SPB,
        (SnareOpts){ .pan = 0.05, .gain = amen ? 0.20 : 0.28,
                     .decay_s = 0.10, .brightness = 0.88 });
    if (cl->len >= 6) {
        kick_render(t0 + 4 * SPB,
            (KickOpts){ .pan = 0.0, .gain = 0.52,
                        .pitch_hi = 160, .pitch_lo = 52,
                        .pitch_dur = 0.045, .decay_s = 0.30 });
    }
    for (int eighth = 0; eighth < cl->len * 2; eighth++) {
        const double th = t0 + eighth * (SPB * 0.5);
        const double accent = (eighth % 2 == 0) ? 0.16 : 0.11;
        hat_render(th,
            (HatOpts){ .pan = (eighth % 2 == 0 ? -0.15 : 0.20),
                       .gain = accent,
                       .decay_s = 0.022, .brightness = 0.95 });
    }
    // Shaker on the offbeats between beats 1-2 and 3-4
    for (int b = 1; b + 1 < cl->len; b += 2) {
        shaker_render(t0 + SPB * (b + 0.5),
            (ShakerOpts){ .pan = (b % 4 == 1) ? 0.18 : -0.18,
                          .gain = (b % 4 == 1) ? 0.14 : 0.12,
                          .decay_s = 0.05 });
    }

    // Sub-bass on the strong syllable, and again where the cell is long
    bass_render(t0, 0.9, c->bass - 12,
        (BassOpts){ .atk = 0.04, .rel = 0.30, .pan = 0.0, .gain = 0.14 });
    if (cl->len >= 6)
        bass_render(t0 + 4 * SPB, 0.9, c->bass - 12,
            (BassOpts){ .atk = 0.04, .rel = 0.30, .pan = 0.0, .gain = 0.11 });

    // Tambourine on every strong syllable (waltz-pop shimmer)
    tambourine_render(t0,
        (TambOpts){ .pan = -0.12, .gain = 0.18, .decay_s = 0.22 });

    // Line openings: kalimba pluck + piano arpeggio + glock sparkle
    if (cl->line) {
        kalimba_render(t0 + SPB * 0.5, c->chord[2],
            (KalimbaOpts){ .decay_s = 1.4, .pan = 0.20,
                           .gain = 0.20, .wet_send = 0.25 });
        kalimba_render(t0 + SPB * 1.5, c->chord[2] + 5,
            (KalimbaOpts){ .decay_s = 1.2, .pan = -0.20,
                           .gain = 0.16, .wet_send = 0.25 });
        const int arp[4] = { c->chord[0], c->chord[1],
                             c->chord[2], c->chord[0] + 12 };
        for (int a = 0; a < 4; a++) {
            piano_render(t0 + a * (SPB * 0.5), 1.3, arp[a],
                (PianoOpts){ .decay_s = 1.0, .velocity = 0.50,
                             .pan = (a % 2 == 0 ? -0.15 : 0.15),
                             .gain = 0.18, .wet_send = 0.22 });
        }
        glock_render(t0, 1.6, c->chord[2],
            (GlockOpts){ .decay_s = 1.2, .pan = 0.25,
                         .gain = 0.20, .wet_send = 0.22 });
    }
    (void)idx;
}

// ── render the full track ────────────────────────────────────────────
static void render_track(void) {
    const double SPB = 60.0 / BPM;       // 0.857 s
    SPB_G = SPB;
    const double VOX_START = tb(0);
    const double VOX_END   = tb(MELODY_BEATS);
    const double AMEN      = tb(AMEN_BEAT);

    report("amazing: bpm=%.1f, beat=%.3fs, intro %.2fs, total %.2fs",
           BPM, SPB, INTRO_SEC, TOTAL_SEC);
    report("  vocal %.2f..%.2f  ·  amen IV %.2f  ·  amen I %.2f  ·  ring %.1fs",
           VOX_START, VOX_END, tb(64), AMEN, RING_SEC);

    // ── 0 · INTRO — tonic pad swell, a soft kick on each intro cell,
    //        hats arriving on the second so the beat is already walking
    //        when the pickup "a-" comes in one beat before the downbeat.
    {
        const double intro_cells = floor((tb(1) + 0.001) / (4.0 * SPB));
        const double first = tb(1) - intro_cells * 4.0 * SPB;   // downbeat-aligned
        sine_pad_render(0.0, tb(1) + 0.25,
                        CH_G.chord[0] - 12, CH_G.chord[1] - 12, CH_G.chord[2] - 12,
                        (SinePadOpts){ .atk = 1.8, .rel = 0.55,
                                       .pan = 0.0, .gain = 0.18,
                                       .wet_send = 0.26,
                                       .lfo_rate = 0.16, .lfo_depth = 0.18 });
        sine_pad_render(0.0, tb(1) + 0.25,
                        CH_G.chord[0], CH_G.chord[1], CH_G.chord[2],
                        (SinePadOpts){ .atk = 2.4, .rel = 0.60,
                                       .pan = 0.0, .gain = 0.08,
                                       .wet_send = 0.28,
                                       .lfo_rate = 0.14, .lfo_depth = 0.20 });
        for (int k = 0; k < (int)intro_cells; k++) {
            const double t0 = first + k * 4.0 * SPB;
            if (t0 < 0) continue;
            kick_render(t0,
                (KickOpts){ .pan = 0.0, .gain = 0.40 + 0.15 * k,
                            .pitch_hi = 165, .pitch_lo = 52,
                            .pitch_dur = 0.045, .decay_s = 0.34 });
            glock_render(t0, 1.6, CH_G.chord[2] + (k % 2 ? 12 : 0),
                (GlockOpts){ .decay_s = 1.4, .pan = k % 2 ? -0.25 : 0.25,
                             .gain = 0.16, .wet_send = 0.26 });
            if (k >= (int)intro_cells - 1) {
                for (int eighth = 0; eighth < 8; eighth++) {
                    hat_render(t0 + eighth * (SPB * 0.5),
                        (HatOpts){ .pan = (eighth % 2 == 0 ? -0.15 : 0.20),
                                   .gain = (eighth % 2 == 0) ? 0.12 : 0.08,
                                   .decay_s = 0.022, .brightness = 0.95 });
                }
                snare_render(t0 + 2 * SPB,
                    (SnareOpts){ .pan = 0.05, .gain = 0.18,
                                 .decay_s = 0.10, .brightness = 0.88 });
            }
        }
        report("  intro (%d cells) rendered", (int)intro_cells);
    }

    // ── 1 · ACCOMPANIMENT — one cell per strong syllable ─────────────
    for (int i = 0; i < CELL_COUNT; i++) cell_render(&CELLS[i], i);
    report("  accompaniment (%d cells) rendered", CELL_COUNT);

    // ── 2 · MELODY LEAD — sine lead doubling the vocal, opposite side ─
    {
        double beat_cursor = 0.0;
        for (int n = 0; n < MELODY_LEN; n++) {
            const double t = tb(beat_cursor);
            const double dur_s = MELODY[n].beats * SPB;
            sine_lead_render(t, dur_s * 0.95, MELODY[n].midi,
                (SineLeadOpts){ .atk = 0.04, .rel = 0.12,
                                 .vib_rate = 4.5, .vib_depth = 7.0,
                                 .pan = -0.25,
                                 .gain = 0.28,
                                 .wet_send = 0.22 });
            beat_cursor += MELODY[n].beats;
        }
        report("  melody lead (%d notes, %.0f beats) rendered",
               MELODY_LEN, beat_cursor);
    }

    // ── 3 · AMEN — the plagal cadence resolves on I, then rings ──────
    {
        const double t23 = AMEN;
        sine_pad_render(t23, 4.0,
                        CH_G.chord[0] - 12, CH_G.chord[1] - 12, CH_G.chord[2] - 12,
                        (SinePadOpts){ .atk = 0.15, .rel = 3.0,
                                       .pan = 0.0, .gain = 0.24,
                                       .wet_send = 0.32,
                                       .lfo_rate = 0.16, .lfo_depth = 0.20 });
        sine_pad_render(t23, 4.0,
                        CH_G.chord[0], CH_G.chord[1], CH_G.chord[2],
                        (SinePadOpts){ .atk = 0.20, .rel = 3.0,
                                       .pan = 0.0, .gain = 0.14,
                                       .wet_send = 0.34,
                                       .lfo_rate = 0.14, .lfo_depth = 0.22 });
        kick_render(t23,
            (KickOpts){ .pan = 0.0, .gain = 0.85,
                        .pitch_hi = 175, .pitch_lo = 48,
                        .pitch_dur = 0.06, .decay_s = 0.45 });
        sine_lead_render(t23, 3.5, CH_G.chord[0],
            (SineLeadOpts){ .atk = 0.05, .rel = 2.8,
                             .vib_rate = 4.5, .vib_depth = 7.0,
                             .pan = -0.20, .gain = 0.30, .wet_send = 0.32 });
        glock_render(t23, 2.4, CH_G.chord[2],
            (GlockOpts){ .decay_s = 1.8, .pan = 0.30,
                         .gain = 0.30, .wet_send = 0.28 });
        glock_render(t23, 3.2, CH_G.chord[0] + 12,
            (GlockOpts){ .decay_s = 2.2, .pan = -0.30,
                         .gain = 0.22, .wet_send = 0.30 });
        bass_render(t23, 3.5, CH_G.bass - 12,
            (BassOpts){ .atk = 0.04, .rel = 1.5, .pan = 0.0, .gain = 0.18 });
        shaker_render(t23 + SPB * 0.5,
            (ShakerOpts){ .pan = 0.0, .gain = 0.10, .decay_s = 0.20 });
        report("  amen (IV → I) rendered at %.2f", t23);
    }

    // ── 4 · APPLY REVERB ──────────────────────────────────────────────
    apply_reverb(0.28);
    report("  reverb applied (wet 0.28)");
}

// ── WAV writer (16-bit PCM stereo) ────────────────────────────────────
static void write_wav(const char *path) {
    double peak = 0.0;
    for (long i = 0; i < N; i++) {
        double a = fabs(L[i]); if (a > peak) peak = a;
        double b = fabs(R[i]); if (b > peak) peak = b;
    }
    report("  peak before tanh: %.3f", peak);
    double post_peak = 0.0;
    for (long i = 0; i < N; i++) {
        L[i] = tanhf(L[i]);
        R[i] = tanhf(R[i]);
        double a = fabs(L[i]); if (a > post_peak) post_peak = a;
        double b = fabs(R[i]); if (b > post_peak) post_peak = b;
    }
    const double target = pow(10.0, -3.0 / 20.0);
    const double gain = post_peak > 0 ? target / post_peak : 1.0;
    report("  peak after tanh: %.3f → gain %.3f (target -3 dB)", post_peak, gain);
    for (long i = 0; i < N; i++) { L[i] *= gain; R[i] *= gain; }

    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "amazing: cannot open %s\n", path); exit(1); }
    const long data_size = N * 2 * 2;
    uint8_t header[44];
    memcpy(header + 0, "RIFF", 4);
    *(uint32_t*)(header + 4) = (uint32_t)(36 + data_size);
    memcpy(header + 8, "WAVE", 4);
    memcpy(header + 12, "fmt ", 4);
    *(uint32_t*)(header + 16) = 16;
    *(uint16_t*)(header + 20) = 1;
    *(uint16_t*)(header + 22) = 2;
    *(uint32_t*)(header + 24) = (uint32_t)SR;
    *(uint32_t*)(header + 28) = (uint32_t)(SR * 2 * 2);
    *(uint16_t*)(header + 32) = 4;
    *(uint16_t*)(header + 34) = 16;
    memcpy(header + 36, "data", 4);
    *(uint32_t*)(header + 40) = (uint32_t)data_size;
    fwrite(header, 1, 44, f);
    int16_t *out = malloc(N * 2 * sizeof(int16_t));
    for (long i = 0; i < N; i++) {
        double sL = L[i], sR = R[i];
        if (sL > 1.0) sL = 1.0; else if (sL < -1.0) sL = -1.0;
        if (sR > 1.0) sR = 1.0; else if (sR < -1.0) sR = -1.0;
        out[2*i+0] = (int16_t)lrint(sL * 32767.0);
        out[2*i+1] = (int16_t)lrint(sR * 32767.0);
    }
    fwrite(out, sizeof(int16_t), N * 2, f);
    free(out);
    fclose(f);
    report("✓ wrote %s (%.1f MB · %.2f s · %d Hz · stereo)",
           path, data_size / 1024.0 / 1024.0, (double)N / SR, SR);
}

// ── main ──────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    t0_wall = now_wall();
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) OUT_PATH = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPM = atof(argv[++i]);
        else if (!strcmp(argv[i], "--seconds") && i + 1 < argc) TOTAL_SEC = atof(argv[++i]);
        else if (!strcmp(argv[i], "--seed") && i + 1 < argc) SEED_STR = argv[++i];
        else if (!strcmp(argv[i], "--intro") && i + 1 < argc) INTRO_SEC = atof(argv[++i]);
        else {
            fprintf(stderr, "amazing: unknown arg %s\n", argv[i]);
            return 1;
        }
    }
    (void)SEED_STR;  // currently unused — kept for future deterministic noise
    if (!OUT_PATH) { fprintf(stderr, "amazing: --out required\n"); return 1; }

    if (TOTAL_SEC <= 0.0)
        TOTAL_SEC = INTRO_SEC + AMEN_BEAT * (60.0 / BPM) + RING_SEC;
    N = (long)(TOTAL_SEC * SR);
    L  = calloc(N, sizeof(float));
    R  = calloc(N, sizeof(float));
    WL = calloc(N, sizeof(float));
    WR = calloc(N, sizeof(float));
    if (!L || !R || !WL || !WR) {
        fprintf(stderr, "amazing: alloc failed\n"); return 1;
    }
    report("amazing: rendering %.2f s @ %d Hz", TOTAL_SEC, SR);
    render_track();
    write_wav(OUT_PATH);
    free(L); free(R); free(WL); free(WR);
    return 0;
}
