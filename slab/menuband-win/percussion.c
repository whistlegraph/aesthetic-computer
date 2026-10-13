// percussion.c — the Menu Band percussion engine, in C for Windows.
//
// A port of MenuBandPercussion.swift (itself a port of lib/percussion.mjs
// and the oscillator + envelope math in fedac/native/src/audio.c). Numbers
// are the Swift's; see percussion.h for the contract. Two things the Swift
// has that are deliberately left out: the genre kits (Electro — the Menu
// Band membrane is the only TrackDrum voicing here) and the continuous
// finger-friction "scratch" bed.

#define _USE_MATH_DEFINES
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <math.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "percussion.h"

// ───────────────────────────── voices ─────────────────────────────

typedef enum { W_SINE, W_TRIANGLE, W_SQUARE, W_NOISE, W_METAL } Wave;
typedef enum { F_LOWPASS, F_HIGHPASS, F_BANDPASS } NoiseFilter;

typedef struct {
    Wave wave;
    double freq;            // Hz; for noise this is the filter cutoff
    double duration;        // seconds (INFINITY for held voices)
    double volume;
    double attack, decay;
    float gainL, gainR;
    // held / release behaviour (hi-hat foot-pedal model)
    uint64_t group;
    int held;
    double releaseFade;
    double toneTarget;
    int releasing;
    double releaseElapsed;
    double releaseFromFreq;
    // running state
    double phase, elapsed;
    // biquad
    double nb0, nb1, nb2, na1, na2, nx1, nx2, ny1, ny2;
    uint32_t seed;
    // genre-kit shaping (inert at defaults)
    double delay, tau, expGain, expK, sweep, sweepTau, sweepK, drive;
    NoiseFilter filter;
    double q;
    double metalScale;
    int filtered;
} Voice;

#define MAX_VOICES 96
#define PENDING_MAX 256
#define RELEASE_STORE 64

static Voice g_active[MAX_VOICES]; static int g_activeCount = 0;
static Voice g_pending[PENDING_MAX]; static int g_pendingCount = 0;
static struct { uint64_t group; Voice v; } g_releaseStore[RELEASE_STORE]; static int g_releaseCount = 0;
static uint64_t g_pendingReleases[64]; static int g_pendingReleaseCount = 0;
static int g_flushRequested = 0;
static CRITICAL_SECTION g_lock;
static int g_lockReady = 0;
static double g_sampleRate = 48000.0;
static const double g_masterGain = 0.34;
static volatile double g_level = 0.90;
static volatile double g_pitch = 1.0;
static int g_lastActive = 0;
static uint64_t g_groupCounter = 0;

static const double HAT_FREQS[4] = { 800, 540, 522.7, 369.6 };
static const double METAL_FREQS[6] = { 205.3, 304.4, 369.6, 522.7, 540, 800 };

// ─────────────────────────── randomness ───────────────────────────
// The Swift uses the system RNG for rj/rn and per-voice seeds; one
// xorshift64 stream stands in for it.

static uint64_t g_rng = 0x9E3779B97F4A7C15ull;
static double frand(void) {
    g_rng ^= g_rng << 13; g_rng ^= g_rng >> 7; g_rng ^= g_rng << 17;
    return (double)(g_rng >> 11) / 9007199254740992.0;   // [0, 1)
}
static double rj(double c, double f) { return c * (1 + (frand() - 0.5) * 2 * f); }
static double rn(double a, double b) { return a + frand() * (b - a); }
static uint32_t rseed(void) { uint32_t s = (uint32_t)(frand() * 4294967295.0); return s ? s : 1; }

static double clampd(double x, double lo, double hi) { return x < lo ? lo : (x > hi ? hi : x); }
static double smoothstep(double e0, double e1, double x) { double t = clampd((x - e0) / (e1 - e0), 0, 1); return t * t * (3 - 2 * t); }

// ───────────────────────────── filters ────────────────────────────
// RBJ cookbook biquads. Low-pass at Q 1 is the original noise filter.

static void setup_filter(Voice *v) {
    double cutoff = v->freq < g_sampleRate * 0.45 ? v->freq : g_sampleRate * 0.45;
    double q = v->q > 0.1 ? v->q : 0.1;
    double w0 = 2.0 * M_PI * cutoff / g_sampleRate;
    double alpha = sin(w0) / (2.0 * q), cosw = cos(w0), norm = 1.0 + alpha;
    switch (v->filter) {
    case F_LOWPASS: { double b = (1.0 - cosw) / 2.0; v->nb0 = b / norm; v->nb1 = (1.0 - cosw) / norm; v->nb2 = b / norm; break; }
    case F_HIGHPASS: { double b = (1.0 + cosw) / 2.0; v->nb0 = b / norm; v->nb1 = -(1.0 + cosw) / norm; v->nb2 = b / norm; break; }
    case F_BANDPASS: v->nb0 = alpha / norm; v->nb1 = 0; v->nb2 = -alpha / norm; break;
    }
    v->na1 = (-2.0 * cosw) / norm;
    v->na2 = (1.0 - alpha) / norm;
}

// ───────────────────────────── makeVoice ──────────────────────────

static Voice make_voice(Wave wave, double tone, double duration, double volume, double attack, double decay, double pan) {
    Voice v; memset(&v, 0, sizeof v);
    double p = clampd(pan, -1, 1), theta = (p + 1) * 0.25 * M_PI;   // constant-power pan
    v.wave = wave; v.freq = tone > 1 ? tone : 1; v.duration = duration > 0.0005 ? duration : 0.0005;
    v.volume = volume; v.attack = attack > 0 ? attack : 0;
    v.decay = decay > 0 ? decay : (duration > 0.0005 ? duration : 0.0005);
    v.gainL = (float)cos(theta); v.gainR = (float)sin(theta);
    v.seed = rseed(); v.q = 1.0; v.expGain = 1; v.metalScale = 1.0; v.filter = F_LOWPASS;
    if (wave == W_NOISE) setup_filter(&v);
    return v;
}

// A held voice that rings until released, then fades over `releaseFade`;
// noise voices step their cutoff to `toneTarget` (the open-hat pedal close).
static Voice make_held(Wave wave, double tone, double vol, double atk, double pan, uint64_t group, double releaseFade, double toneTarget) {
    Voice v; memset(&v, 0, sizeof v);
    double p = clampd(pan, -1, 1), theta = (p + 1) * 0.25 * M_PI;
    v.wave = wave; v.freq = tone > 1 ? tone : 1; v.duration = INFINITY; v.volume = vol; v.attack = atk > 0 ? atk : 0; v.decay = 0;
    v.gainL = (float)cos(theta); v.gainR = (float)sin(theta);
    v.seed = rseed(); v.q = 1.0; v.expGain = 1; v.metalScale = 1.0; v.filter = F_LOWPASS;
    v.group = group; v.held = 1; v.releaseFade = releaseFade > 0.01 ? releaseFade : 0.01; v.toneTarget = toneTarget;
    if (wave == W_NOISE) setup_filter(&v);
    return v;
}

// ───────────────────────────── staging ────────────────────────────

static void stage(const Voice *v) {
    if (g_pendingCount < PENDING_MAX) g_pending[g_pendingCount++] = *v;
}
static void store_release(uint64_t group, const Voice *v) {
    if (g_releaseCount < RELEASE_STORE) { g_releaseStore[g_releaseCount].group = group; g_releaseStore[g_releaseCount].v = *v; g_releaseCount++; }
}

// ───────────────────────── the twelve drums ───────────────────────
// Port of `build(for:)`: hit = finite one-shot; sustain = finite one-shot
// whose fade spans its whole duration (ride/crash ring regardless of key);
// heldSustain = rings until release (the foot pedal); releaseHit = fired on
// key-up (the closed-hat lift click).

typedef struct { Voice down[32]; int downCount; Voice rel[8]; int relCount; } DrumVoices;

static void dv_hit(DrumVoices *d, Wave w, double tone, double dur, double vol, double atk, double dec, double pan) { if (d->downCount < 32) d->down[d->downCount++] = make_voice(w, tone, dur, vol, atk, dec, pan); }
static void dv_sustain(DrumVoices *d, Wave w, double tone, double both, double vol, double atk, double pan) { if (d->downCount < 32) d->down[d->downCount++] = make_voice(w, tone, both, vol, atk, 0, pan); }
static void dv_held(DrumVoices *d, Wave w, double tone, double vol, double atk, double pan, uint64_t group, double fade, double toneTarget) { if (d->downCount < 32) d->down[d->downCount++] = make_held(w, tone, vol, atk, pan, group, fade, toneTarget); }
static void dv_release(DrumVoices *d, Wave w, double tone, double dur, double vol, double atk, double dec, double pan) { if (d->relCount < 8) d->rel[d->relCount++] = make_voice(w, tone, dur, vol, atk, dec, pan); }

static void build_drum(DrumVoices *d, PercDrum drum, double v, double basePan, uint64_t group) {
    memset(d, 0, sizeof *d);
    double dp;
    switch (drum) {
    case PERC_KICK:
        dp = basePan + rn(-0.02, 0.02);
        dv_hit(d, W_NOISE, 2500, 0.0025, rj(0.50, 0.12) * v, 0.0002, 0.0022, dp);
        dv_hit(d, W_SINE, 200, 0.012, rj(1.1, 0.10) * v, 0.0005, 0.011, dp);
        dv_hit(d, W_SINE, 150, 0.045, rj(1.3, 0.10) * v, 0.001, 0.044, dp);
        dv_hit(d, W_SINE, 90, 0.080, rj(0.85, 0.12) * v, 0.002, 0.078, dp);
        dv_hit(d, W_SINE, 55, rj(0.35, 0.20), rj(1.0, 0.12) * v, 0.003, 0.345, dp);
        dv_release(d, W_SINE, 70, 0.045, rj(0.40, 0.12) * v, 0.001, 0.044, dp);
        dv_release(d, W_NOISE, 1600, 0.003, rj(0.16, 0.18) * v, 0.0002, 0.0028, dp);
        break;
    case PERC_SNARE:
        dp = basePan + rn(-0.02, 0.02);
        dv_hit(d, W_NOISE, 3500, 0.004, rj(0.95, 0.10) * v, 0.0001, 0.004, dp);
        dv_hit(d, W_SINE, 238, 0.030, rj(0.35, 0.12) * v, 0.0003, 0.029, dp);
        dv_hit(d, W_SINE, 476, 0.030, rj(0.28, 0.12) * v, 0.0003, 0.029, dp);
        dv_hit(d, W_NOISE, 3500, rj(0.11, 0.20), rj(0.85, 0.10) * v, 0.0005, 0.108, dp + rn(-0.04, 0.04));
        dv_hit(d, W_NOISE, 1800, rj(0.07, 0.20), rj(0.38, 0.15) * v, 0.0008, 0.068, dp + rn(-0.04, 0.04));
        dv_hit(d, W_TRIANGLE, 180, 0.025, rj(0.22, 0.15) * v, 0.001, 0.024, dp);
        dv_release(d, W_NOISE, 4000, 0.006, rj(0.30, 0.15) * v, 0.0002, 0.0055, dp);
        dv_release(d, W_TRIANGLE, 200, 0.018, rj(0.14, 0.15) * v, 0.0006, 0.017, dp);
        break;
    case PERC_CLAP:
        dp = basePan + rn(-0.06, 0.02);
        dv_hit(d, W_NOISE, 1000, 0.025, rj(0.90, 0.15) * v, 0.005, 0.020, dp);
        dv_hit(d, W_NOISE, 1100, 0.035, rj(0.95, 0.15) * v, 0.015, 0.020, dp);
        dv_hit(d, W_NOISE, 900, 0.045, rj(0.85, 0.15) * v, 0.025, 0.020, dp);
        dv_hit(d, W_NOISE, 3000, 0.008, rj(0.55, 0.15) * v, 0.001, 0.007, dp);
        dv_hit(d, W_NOISE, 1000, rj(0.14, 0.25), rj(0.85, 0.15) * v, 0.045, 0.135, dp + rn(-0.02, 0.10));
        dv_hit(d, W_NOISE, 2200, rj(0.10, 0.25), rj(0.35, 0.18) * v, 0.050, 0.095, dp + rn(-0.02, 0.10));
        break;
    case PERC_SNAP:
        dp = basePan + rn(-0.04, 0.04);
        dv_hit(d, W_NOISE, 6000, 0.003, rj(0.70, 0.15) * v, 0.0001, 0.0028, dp);
        dv_hit(d, W_SINE, 2100, rj(0.045, 0.20), rj(0.55, 0.12) * v, 0.0005, 0.044, dp);
        dv_hit(d, W_SINE, 3500, rj(0.020, 0.25), rj(0.28, 0.18) * v, 0.0005, 0.019, dp);
        break;
    case PERC_HAT_CLOSED:
        dp = basePan + rn(-0.03, 0.03);
        for (int i = 0; i < 4; i++) dv_hit(d, W_SQUARE, HAT_FREQS[i], rj(0.008, 0.20), rj(0.18, 0.18) * v, 0.0005, 0.0075, dp);
        dv_hit(d, W_NOISE, 8000, rj(0.040, 0.20), rj(0.38, 0.12) * v, 0.0005, 0.038, dp);
        dv_release(d, W_NOISE, 9000, 0.004, rj(0.22, 0.20) * v, 0.0002, 0.0038, dp);
        dv_release(d, W_SQUARE, 6000, 0.003, rj(0.08, 0.25) * v, 0.0003, 0.0027, dp);
        break;
    case PERC_HAT_OPEN:
        dp = basePan + rn(-0.04, 0.04);
        for (int i = 0; i < 4; i++) dv_hit(d, W_SQUARE, HAT_FREQS[i], 0.012, rj(0.16, 0.18) * v, 0.0005, 0.011, dp);
        dv_hit(d, W_NOISE, 8200, 0.012, rj(0.42, 0.12) * v, 0.0003, 0.011, dp);
        dv_held(d, W_NOISE, 7000, rj(0.32, 0.15) * v, 0.003, dp + rn(-0.02, 0.08), group, 0.12, 3500);
        dv_held(d, W_NOISE, 5000, rj(0.20, 0.18) * v, 0.003, dp + rn(-0.02, 0.08), group, 0.10, 2800);
        dv_held(d, W_SQUARE, 800, rj(0.08, 0.20) * v, 0.005, dp, group, 0.08, 0);
        break;
    case PERC_RIDE:
        dp = basePan + rn(-0.03, 0.03);
        dv_hit(d, W_SQUARE, 800, 0.020, rj(0.10, 0.18) * v, 0.0005, 0.019, dp);
        dv_hit(d, W_SQUARE, 540, 0.020, rj(0.08, 0.18) * v, 0.0005, 0.019, dp);
        dv_sustain(d, W_SINE, 440, rj(0.40, 0.20), rj(0.24, 0.12) * v, 0.0008, dp);
        dv_sustain(d, W_SINE, 587, rj(0.40, 0.20), rj(0.20, 0.12) * v, 0.0008, dp);
        dv_sustain(d, W_NOISE, 4200, rj(0.9, 0.20), rj(0.26, 0.12) * v, 0.005, dp + rn(-0.03, 0.03));
        break;
    case PERC_CRASH:
        dp = basePan + rn(-0.05, 0.05);
        dv_hit(d, W_NOISE, 8000, 0.030, rj(0.75, 0.15) * v, 0.0005, 0.029, dp);
        for (int i = 0; i < 4; i++) dv_hit(d, W_SQUARE, HAT_FREQS[i], 0.030, rj(0.12, 0.20) * v, 0.0005, 0.029, dp);
        dv_sustain(d, W_NOISE, 5000, rj(1.4, 0.18), rj(0.45, 0.12) * v, 0.008, dp + rn(-0.04, 0.04));
        dv_sustain(d, W_NOISE, 7500, rj(0.9, 0.18), rj(0.30, 0.15) * v, 0.008, dp + rn(-0.04, 0.04));
        dv_sustain(d, W_SQUARE, 800, rj(0.5, 0.20), rj(0.08, 0.20) * v, 0.008, dp);
        break;
    case PERC_SPLASH:
        dp = basePan + rn(-0.04, 0.04);
        dv_hit(d, W_NOISE, 9000, 0.012, rj(0.55, 0.15) * v, 0.0003, 0.011, dp);
        dv_hit(d, W_SQUARE, 800, 0.015, rj(0.14, 0.20) * v, 0.0005, 0.014, dp);
        dv_hit(d, W_SQUARE, 540, 0.015, rj(0.10, 0.20) * v, 0.0005, 0.014, dp);
        dv_hit(d, W_NOISE, 6000, rj(0.35, 0.20), rj(0.42, 0.12) * v, 0.004, 0.345, dp + rn(-0.03, 0.03));
        dv_hit(d, W_NOISE, 8500, rj(0.22, 0.20), rj(0.25, 0.15) * v, 0.004, 0.215, dp + rn(-0.03, 0.03));
        break;
    case PERC_COWBELL:
        dp = basePan + rn(-0.03, 0.03);
        dv_hit(d, W_SQUARE, 1800, 0.004, rj(0.35, 0.15) * v, 0.0002, 0.0038, dp);
        dv_hit(d, W_TRIANGLE, 800, rj(0.28, 0.20), rj(0.42, 0.12) * v, 0.0008, 0.275, dp);
        dv_hit(d, W_TRIANGLE, 540, rj(0.28, 0.20), rj(0.36, 0.12) * v, 0.0008, 0.275, dp);
        dv_release(d, W_SQUARE, 2400, 0.006, rj(0.16, 0.15) * v, 0.0003, 0.0055, dp);
        dv_release(d, W_TRIANGLE, 1080, 0.020, rj(0.12, 0.15) * v, 0.0006, 0.019, dp);
        break;
    case PERC_BLOCK:
        dp = basePan + rn(-0.03, 0.03);
        dv_hit(d, W_NOISE, 5000, 0.002, rj(0.35, 0.18) * v, 0.0001, 0.0018, dp);
        dv_hit(d, W_TRIANGLE, 2500, rj(0.050, 0.25), rj(0.52, 0.12) * v, 0.0003, 0.048, dp);
        dv_hit(d, W_TRIANGLE, 1250, rj(0.050, 0.25), rj(0.18, 0.18) * v, 0.0005, 0.048, dp);
        break;
    case PERC_TAMBO:
        dp = basePan + rn(-0.04, 0.04);
        dv_hit(d, W_NOISE, 7000, 0.08, rj(0.38, 0.18) * v, 0.002, 0.075, dp);
        dv_hit(d, W_NOISE, 7500, 0.09, rj(0.30, 0.18) * v, 0.015, 0.075, dp);
        dv_hit(d, W_NOISE, 6500, 0.10, rj(0.25, 0.18) * v, 0.030, 0.070, dp);
        dv_hit(d, W_SQUARE, 6000, 0.030, rj(0.14, 0.20) * v, 0.001, 0.028, dp);
        dv_hit(d, W_NOISE, 7000, rj(0.20, 0.22), rj(0.32, 0.18) * v, 0.050, 0.195, dp + rn(-0.04, 0.04));
        dv_hit(d, W_NOISE, 4500, rj(0.15, 0.22), rj(0.20, 0.18) * v, 0.055, 0.145, dp + rn(-0.04, 0.04));
        break;
    default: break;
    }
}

// ─────────────────────── the TrackDrum membrane ───────────────────
// Rounded-rectangle inset coordinate for the physical trackpad: a 1.64:1
// surface with a modest corner radius; 0 at the centre, 1 at every edge.

static double rounded_pad_distance(double sx, double sy) {
    const double halfWidth = 0.82, halfHeight = 0.50, corner = 0.055;
    double px = fabs(sx) * halfWidth, py = fabs(sy) * halfHeight;
    double qx = px - (halfWidth - corner), qy = py - (halfHeight - corner);
    double ox = qx > 0 ? qx : 0, oy = qy > 0 ? qy : 0;
    double outside = hypot(ox, oy);
    double inside = (qx > qy ? qx : qy); if (inside > 0) inside = 0;
    double signedDistance = outside + inside - corner;
    double inwardDepth = -signedDistance; if (inwardDepth < 0) inwardDepth = 0;
    double r = inwardDepth / halfHeight; if (r > 1) r = 1;
    return 1.0 - r;
}

// A Mac trackpad pans along its long axis.
static double stereo(double sx, double sy, double reach) { return clampd((sx * 1.0 + sy * 0.0) * reach, -1, 1); }

static double bessel_jn(int m, double x) { return _jn(m, x); }

typedef struct { int m; double root, gain; } Mode;
static const Mode MODES[10] = {
    { 0, 2.4048, 0.46 }, { 1, 3.8317, 0.70 }, { 2, 5.1356, 0.58 }, { 0, 5.5201, 0.34 }, { 3, 6.3802, 0.40 },
    { 1, 7.0156, 0.34 }, { 4, 7.5883, 0.29 }, { 2, 8.4172, 0.24 }, { 0, 8.6537, 0.15 }, { 5, 8.7715, 0.18 },
};

// `v` is the Swift's velocity law: clamp(0.1, 2.2, midiVelocity / 100).
static double velocity_law(double velocity01) { return clampd(velocity01 * 127.0 / 100.0, 0.1, 2.2); }

static PercDrum membrane_strike(double x, double y, double v, const double *anchors, int anchorCount, Voice *out, int *outCount) {
    int n = 0;
    double sx = (x - 0.5) * 2, sy = (y - 0.5) * 2;
    double radius = rounded_pad_distance(sx, sy);
    double theta = atan2(sy * 0.50, sx * 0.82);
    double strikeLevel = v * (1.0 - 0.38 * smoothstep(0.46, 1.0, radius));
    double edge = smoothstep(0.62, 0.70, radius);
    double outerClick = smoothstep(0.88, 0.965, radius);
    double hatEdge = edge * (1.0 - outerClick * 0.94);
    double tomBand = smoothstep(0.23, 0.31, radius) * (1.0 - smoothstep(0.40, 0.48, radius));
    double snareBand = smoothstep(0.40, 0.48, radius) * (1.0 - smoothstep(0.62, 0.70, radius));
    double tension = 1.0 + (anchorCount * 0.10 < 0.45 ? anchorCount * 0.10 : 0.45);
    double fingerDamping = anchorCount * 0.13 < 0.68 ? anchorCount * 0.13 : 0.68;
    double fundamental = (104.0 + radius * 28.0) * tension;
    double pan = stereo(sx, sy, 0.72);
    #define EMIT(...) do { if (n < 64) out[n++] = make_voice(__VA_ARGS__); } while (0)
    double centerKick = 1.0 - smoothstep(0.10, 0.38, radius);
    if (centerKick > 0.01) {
        double k = centerKick * strikeLevel;
        EMIT(W_NOISE, 2500, 0.0025, 0.48 * k, 0.0002, 0.0022, pan);
        EMIT(W_SINE, 200, 0.012, 0.90 * k, 0.0005, 0.011, pan);
        EMIT(W_SINE, 150, 0.045, 1.05 * k, 0.001, 0.044, pan);
        EMIT(W_SINE, 90, 0.080, 0.70 * k, 0.002, 0.078, pan);
        EMIT(W_SINE, 55, 0.17, 0.74 * k, 0.003, 0.164, pan);
    }
    double membraneBlend = 0.12 + 0.88 * smoothstep(0.12, 0.40, radius);
    for (int i = 0; i < 10; i++) {
        const Mode *mode = &MODES[i];
        double strikeShape = fabs(bessel_jn(mode->m, mode->root * radius) * cos(mode->m * theta));
        double constraint = 1.0;
        for (int a = 0; a < anchorCount; a++) {
            double ax = (anchors[2 * a] - 0.5) * 2, ay = (anchors[2 * a + 1] - 0.5) * 2;
            double ar = rounded_pad_distance(ax, ay), at = atan2(ay * 0.50, ax * 0.82);
            double shape = fabs(bessel_jn(mode->m, mode->root * ar) * cos(mode->m * at));
            double c = 1.0 - shape * 0.72; constraint *= c > 0.18 ? c : 0.18;
        }
        double amplitude = mode->gain * strikeShape * constraint * (1 - edge) * strikeLevel * membraneBlend;
        if (amplitude <= 0.004) continue;
        double ratio = mode->root / MODES[0].root;
        double duration = (0.285 - i * 0.019) * (1.0 - fingerDamping); if (duration < 0.060) duration = 0.060;
        EMIT(W_SINE, fundamental * ratio, duration, amplitude * 0.82, 0.0008, duration * 0.94, pan);
    }
    double contactDuration = 0.012 + hatEdge * 0.014;
    EMIT(W_NOISE, 2400 + hatEdge * 6200, contactDuration, (0.22 + hatEdge * 0.09) * (1.0 - outerClick * 0.58) * strikeLevel, 0.0002, contactDuration * 0.92, pan);
    if (tomBand > 0.01) {
        EMIT(W_SINE, 148 * tension, 0.105, 0.25 * tomBand * strikeLevel, 0.0005, 0.098, pan);
        EMIT(W_TRIANGLE, 222 * tension, 0.052, 0.12 * tomBand * strikeLevel, 0.0003, 0.048, pan);
    }
    if (snareBand > 0.01) {
        EMIT(W_NOISE, 4300, 0.064, 0.24 * snareBand * strikeLevel, 0.0003, 0.060, pan);
        EMIT(W_SQUARE, 196 * tension, 0.038, 0.045 * snareBand * strikeLevel, 0.0004, 0.035, pan);
    }
    if (hatEdge > 0.02) {
        for (int i = 0; i < 4; i++) EMIT(W_SQUARE, HAT_FREQS[i], 0.008 + hatEdge * 0.014, 0.035 * hatEdge * strikeLevel, 0.0002, 0.007 + hatEdge * 0.012, pan);
    }
    if (outerClick > 0.01) {
        double click = outerClick * v;
        EMIT(W_NOISE, 7200, 0.0020, 0.50 * click, 0.00005, 0.00165, pan);
        EMIT(W_TRIANGLE, 1650 * tension, 0.0040, 0.38 * click, 0.00008, 0.0035, pan);
        EMIT(W_SINE, 520 * tension, 0.0055, 0.20 * click, 0.0001, 0.0048, pan);
    }
    #undef EMIT
    *outCount = n;
    return edge > 0.55 ? PERC_HAT_CLOSED : (snareBand > 0.45 ? PERC_SNARE : (tomBand > 0.45 ? PERC_BLOCK : PERC_KICK));
}

// Finger-up articulation (the membrane's, non-synthetic): a low dry tack on
// the skin, a tiny closure in the hat band, a near-instant click at the rim.
static int membrane_lift(double x, double y, double v, Voice *out) {
    int n = 0;
    double sx = (x - 0.5) * 2, sy = (y - 0.5) * 2;
    double distance = rounded_pad_distance(sx, sy);
    double pan = stereo(sx, sy, 0.72);
    double tension = 1.0, damping = 1.0;          // no resting anchors are passed for lifts
    double edge = smoothstep(0.62, 0.70, distance), outerClick = smoothstep(0.88, 0.965, distance);
    double centerKick = 1.0 - smoothstep(0.10, 0.38, distance);
    double tomBand = smoothstep(0.23, 0.31, distance) * (1.0 - smoothstep(0.40, 0.48, distance));
    double snareBand = smoothstep(0.40, 0.48, distance) * (1.0 - smoothstep(0.62, 0.70, distance));
    #define EMIT(...) do { if (n < 24) out[n++] = make_voice(__VA_ARGS__); } while (0)
    if (centerKick > 0.01) {
        EMIT(W_SINE, 70 * tension, 0.045 * damping, 0.18 * centerKick * v, 0.0005, 0.043 * damping, pan);
        EMIT(W_NOISE, 1450, 0.003, 0.07 * centerKick * v, 0.0001, 0.0027, pan);
    }
    if (tomBand > 0.01) EMIT(W_SINE, 148 * tension, 0.022 * damping, 0.10 * tomBand * v, 0.0002, 0.020 * damping, pan);
    if (snareBand > 0.01) {
        EMIT(W_NOISE, 3500 * tension, 0.006 * damping, 0.12 * snareBand * v, 0.0001, 0.0055 * damping, pan);
        EMIT(W_TRIANGLE, 190 * tension, 0.016 * damping, 0.065 * snareBand * v, 0.0002, 0.014 * damping, pan);
    }
    double hatLift = edge * (1.0 - outerClick * 0.92);
    if (hatLift > 0.01) for (int i = 0; i < 4; i++) EMIT(W_SQUARE, HAT_FREQS[i], 0.005, 0.014 * hatLift * v, 0.0001, 0.0045, pan);
    if (outerClick > 0.01) {
        EMIT(W_NOISE, 7800, 0.0016, 0.32 * outerClick * v, 0.00004, 0.0013, pan);
        EMIT(W_TRIANGLE, 1900 * tension, 0.0030, 0.24 * outerClick * v, 0.00005, 0.0025, pan);
        EMIT(W_SINE, 600 * tension, 0.0040, 0.12 * outerClick * v, 0.00008, 0.0034, pan);
    }
    #undef EMIT
    return n;
}

// ────────────────────────── envelope + sample ─────────────────────

static double envelope(const Voice *v, double t) {
    double env = 1.0;
    if (v->attack > 0 && t < v->attack) env = t / v->attack;
    if (v->decay > 0) {
        double decayStart = v->duration - v->decay; if (decayStart < 0) decayStart = 0;
        if (t > decayStart) { double prog = (t - decayStart) / v->decay; if (prog > 1) prog = 1; env *= (1 - prog); }
    }
    return env;
}

static uint32_t xorshift32(uint32_t *s) { *s ^= *s << 13; *s ^= *s >> 17; *s ^= *s << 5; return *s; }

static double next_sample(Voice *v, double pitch, double dt) {
    double t = v->elapsed - v->delay;
    if (t < 0) { v->elapsed += dt; return 0; }
    double env = envelope(v, t);
    if (v->tau > 0) { if (v->expK == 0) v->expK = exp(-dt / v->tau); env *= v->expGain; v->expGain *= v->expK; }
    double relGain = v->releasing ? (1.0 - v->releaseElapsed / v->releaseFade > 0 ? 1.0 - v->releaseElapsed / v->releaseFade : 0) : 1.0;
    double s;
    switch (v->wave) {
    case W_SINE: s = sin(2.0 * M_PI * v->phase); break;
    case W_TRIANGLE: { double tp = fmod(v->phase + 0.25, 1.0); s = 4.0 * fabs(tp - 0.5) - 1.0; break; }
    case W_SQUARE:
        s = v->phase < 0.5 ? 1.0 : -1.0;
        if (v->filtered) { double y = v->nb0 * s + v->nb1 * v->nx1 + v->nb2 * v->nx2 - v->na1 * v->ny1 - v->na2 * v->ny2; v->nx2 = v->nx1; v->nx1 = s; v->ny2 = v->ny1; v->ny1 = y; s = y; }
        break;
    default: {   // noise and metal share the biquad
        double x;
        if (v->wave == W_NOISE) x = (double)xorshift32(&v->seed) / 4294967295.0 * 2.0 - 1.0;
        else { x = 0; for (int i = 0; i < 6; i++) { double ph = fmod(t * METAL_FREQS[i] * v->metalScale, 1.0); x += ph < 0.5 ? 1 : -1; } x /= 6; }
        double y = v->nb0 * x + v->nb1 * v->nx1 + v->nb2 * v->nx2 - v->na1 * v->ny1 - v->na2 * v->ny2;
        v->nx2 = v->nx1; v->nx1 = x; v->ny2 = v->ny1; v->ny1 = y; s = y;
        break;
    }
    }
    double out = s * env * relGain;
    if (v->drive > 0) out = tanh(out * v->drive) / tanh(v->drive);
    out *= v->volume;
    double f = v->freq;
    if (v->sweep > 0) { if (v->sweepK == 0) v->sweepK = exp(-dt / (v->sweepTau > 1e-4 ? v->sweepTau : 1e-4)); f += v->sweep; v->sweep *= v->sweepK; }
    v->phase += f * (v->wave == W_NOISE ? 1.0 : pitch) * dt;
    if (v->phase >= 1) v->phase -= floor(v->phase);
    v->elapsed += dt;
    if (v->releasing) v->releaseElapsed += dt;
    return out;
}

// ─────────────────────────── public API ───────────────────────────

void perc_init(double sample_rate) {
    if (!g_lockReady) { InitializeCriticalSection(&g_lock); g_lockReady = 1; }
    g_sampleRate = sample_rate > 0 ? sample_rate : 48000.0;
    g_activeCount = 0; g_pendingCount = 0; g_releaseCount = 0; g_pendingReleaseCount = 0;
}

void perc_set_pitch(double scale) { g_pitch = scale > 0 ? scale : 1.0; }
void perc_set_level(double level) { g_level = clampd(level, 0, 3); }

// Windows level trims, in dB. The kit came over from the Mac at the Mac's
// levels, but here the melodic engines are loudness-normalized to the
// SoundFont median, so the drums sat ~11 dB under a piano note (peak 100 ms
// K-weighted loudness at velocity 100, measured offline). Each drum is lifted
// to its own target rather than by one flat gain: the kick already peaks near
// full scale while the clap and closed hat are ~16 dB down. Targets against
// the piano's −12.6 LUFS: kick −13, hats −17, everything else −15.
static const double DRUM_TRIM_DB[PERC_DRUM_COUNT] = {
    2.7, 6.0, 6.7, 8.1, 14.8, 6.7, 6.9, 13.0, 10.5, 7.0, 10.1, 8.9,   // BAS CRS SNR SPL CLP SNP CBL HHC BLK HHO TMB RDE
};
static const double PAD_TRIM_DB = 4.0;   // the TrackDrum membrane: centre −18 → −14
static double db_gain(double db) { return pow(10.0, db / 20.0); }

static uint64_t next_group(void) { uint64_t g; EnterCriticalSection(&g_lock); g = ++g_groupCounter; LeaveCriticalSection(&g_lock); return g; }

void perc_hit_drum(PercDrum drum, double velocity, double pan, uint64_t group) {
    if (!g_lockReady) return;
    if (drum < 0 || drum >= PERC_DRUM_COUNT) return;
    double v = velocity_law(velocity);
    if (group == 0) group = next_group();
    DrumVoices d; build_drum(&d, drum, v, pan, group);
    double trim = db_gain(DRUM_TRIM_DB[drum]);
    for (int i = 0; i < d.downCount; i++) d.down[i].volume *= trim;
    for (int i = 0; i < d.relCount; i++) d.rel[i].volume *= trim;
    EnterCriticalSection(&g_lock);
    for (int i = 0; i < d.downCount; i++) stage(&d.down[i]);
    for (int i = 0; i < d.relCount; i++) store_release(group, &d.rel[i]);
    LeaveCriticalSection(&g_lock);
}

PercDrum perc_strike_pad(double x, double y, double velocity, const double *anchors_xy, int anchor_count, uint64_t group) {
    (void)group;
    if (!g_lockReady) return PERC_KICK;
    Voice voices[64]; int n = 0;
    PercDrum pulse = membrane_strike(x, y, velocity_law(velocity), anchors_xy, anchor_count < 0 ? 0 : anchor_count, voices, &n);
    for (int i = 0; i < n; i++) voices[i].volume *= db_gain(PAD_TRIM_DB);
    EnterCriticalSection(&g_lock);
    for (int i = 0; i < n; i++) stage(&voices[i]);
    LeaveCriticalSection(&g_lock);
    return pulse;
}

void perc_lift_pad(double x, double y, uint64_t group) {
    (void)group;
    if (!g_lockReady) return;
    Voice voices[24]; int n = membrane_lift(x, y, 0.5, voices);   // the Swift's lift velocity 64/127, clamped 0.08…1
    for (int i = 0; i < n; i++) voices[i].volume *= db_gain(PAD_TRIM_DB);
    EnterCriticalSection(&g_lock);
    for (int i = 0; i < n; i++) stage(&voices[i]);
    LeaveCriticalSection(&g_lock);
}

void perc_release(uint64_t group) {
    if (!g_lockReady || group == 0) return;
    EnterCriticalSection(&g_lock);
    int w = 0;
    for (int i = 0; i < g_releaseCount; i++) {
        if (g_releaseStore[i].group == group) stage(&g_releaseStore[i].v);
        else g_releaseStore[w++] = g_releaseStore[i];
    }
    g_releaseCount = w;
    if (g_pendingReleaseCount < 64) g_pendingReleases[g_pendingReleaseCount++] = group;
    LeaveCriticalSection(&g_lock);
}

void perc_flush(void) {
    if (!g_lockReady) return;
    EnterCriticalSection(&g_lock);
    g_pendingCount = 0; g_releaseCount = 0; g_pendingReleaseCount = 0; g_flushRequested = 1;
    LeaveCriticalSection(&g_lock);
}

int perc_active_voices(void) { return g_lastActive; }

const char *perc_label(PercDrum drum) {
    static const char *labels[PERC_DRUM_COUNT] = { "BAS", "CRS", "SNR", "SPL", "CLP", "SNP", "CBL", "HHC", "BLK", "HHO", "TMB", "RDE" };
    return (drum >= 0 && drum < PERC_DRUM_COUNT) ? labels[drum] : "???";
}

static int voice_alive(const Voice *v) {
    return v->releasing ? (v->releaseElapsed < v->releaseFade) : (v->held || v->elapsed < v->duration + v->delay);
}

void perc_render(float *out, int frames) {
    if (!g_lockReady) return;
    double pitch = g_pitch, level = g_level;
    uint64_t releases[64]; int releaseCount = 0;

    EnterCriticalSection(&g_lock);
    if (g_flushRequested) { g_activeCount = 0; g_flushRequested = 0; }
    // Retire voices that ended at the prior block before deciding whether the
    // new onsets fit.
    int live = 0;
    for (int i = 0; i < g_activeCount; i++) if (voice_alive(&g_active[i])) g_active[live++] = g_active[i];
    g_activeCount = live;
    if (g_pendingCount) {
        // New attacks win over inaudible old tails: steal the oldest finite
        // voices first, never a held open-hat voice.
        int needed = g_activeCount + g_pendingCount - MAX_VOICES;
        if (needed > 0) {
            int keep = 0;
            for (int i = 0; i < g_activeCount; i++) {
                if (needed > 0 && !g_active[i].held) needed--;
                else g_active[keep++] = g_active[i];
            }
            g_activeCount = keep;
        }
        int available = MAX_VOICES - g_activeCount; if (available < 0) available = 0;
        int accepted = g_pendingCount < available ? g_pendingCount : available;
        for (int i = 0; i < accepted; i++) g_active[g_activeCount++] = g_pending[i];
        g_pendingCount = 0;
    }
    g_lastActive = g_activeCount;
    if (g_pendingReleaseCount) { releaseCount = g_pendingReleaseCount; memcpy(releases, g_pendingReleases, sizeof(uint64_t) * releaseCount); g_pendingReleaseCount = 0; }
    LeaveCriticalSection(&g_lock);

    // Flip newly-released held voices into their fade-out; noise voices step
    // their cutoff to the darker release target (pedal close).
    for (int r = 0; r < releaseCount; r++)
        for (int i = 0; i < g_activeCount; i++) {
            Voice *v = &g_active[i];
            if (v->held && !v->releasing && v->group == releases[r]) {
                v->releasing = 1; v->releaseElapsed = 0; v->releaseFromFreq = v->freq;
                if (v->toneTarget > 0 && v->wave == W_NOISE) { v->freq = v->toneTarget; setup_filter(v); }
            }
        }
    if (!g_activeCount) return;

    const double dt = 1.0 / g_sampleRate, g = g_masterGain * level;
    int w = 0;
    for (int idx = 0; idx < g_activeCount; idx++) {
        Voice v = g_active[idx]; int alive = 1;
        for (int i = 0; i < frames; i++) {
            if (v.releasing) { if (v.releaseElapsed >= v.releaseFade) { alive = 0; break; } }
            else if (!v.held) { if (v.elapsed >= v.duration + v.delay) { alive = 0; break; } }
            double amp = next_sample(&v, pitch, dt) * g;
            out[2 * i] += (float)(amp * v.gainL); out[2 * i + 1] += (float)(amp * v.gainR);
        }
        if (alive) g_active[w++] = v;
    }
    g_activeCount = w;
}
