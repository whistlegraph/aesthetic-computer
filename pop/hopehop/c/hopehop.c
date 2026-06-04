// hopehop.c — summer hip-hop / R&B beat, De La Soul / Native-Tongues
// flavor. 96 BPM 4/4 with a HARD 16th shuffle. Bright jazzy boom-bap:
// walking upright bass, drawbar-organ stabs on the offbeats, hand
// percussion (congas + tambourine), a sweet flute hook, and a formant-
// synthesized "ooh-ah" vocal chop.
//
// Architecture mirrors amaythingra.c: voices are *_render() functions
// that sum into global L/R buffers (+ WL/WR reverb sends); a Schroeder
// reverb, a chord SCORE, one render_track(), a WAV writer, and main().
//
// Build: ./build-hopehop.sh
// Run:   ./hopehop --out out/hopehop.wav

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
static double BPM = 82.0;              // slower, chill — less "war war"
static double TOTAL_SEC = 90.0;        // 30 bars @ 2.927s + tail
static double SWING = 0.60;            // relaxed shuffle (0.5 = straight)
static const char *OUT_PATH = NULL;
static double DRIVE = 1.0;
static unsigned SEED_ARG = 0;
static int SYNTH_PERC = 0;             // synth drums/perc OFF by default —
                                       // the JS driver layers freesound
                                       // trap samples instead (--synth-drums
                                       // re-enables the built-in synth kit).

static uint32_t RNG_STATE = 0x9e3779b9u;
static void rng_seed(uint32_t s) { RNG_STATE = s ? s : 0x9e3779b9u; }
static uint32_t rng_u32(void) { uint32_t x = RNG_STATE; x ^= x << 13; x ^= x >> 17; x ^= x << 5; RNG_STATE = x; return x; }
static double rng_d(void) { return (double)rng_u32() / 4294967296.0; }
static double jit(double a) { return (rng_d() * 2.0 - 1.0) * a; }

// ── shared mix buffers ────────────────────────────────────────────────
static long N = 0;
static float *L = NULL, *R = NULL, *WL = NULL, *WR = NULL;

// ── timing / reporting ────────────────────────────────────────────────
static double t0_wall = 0.0;
static double now_wall(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return ts.tv_sec + ts.tv_nsec / 1e9; }
__attribute__((format(printf, 1, 2)))
static void report(const char *fmt, ...) {
    fprintf(stderr, "[%6.2fs] ", now_wall() - t0_wall);
    va_list a; va_start(a, fmt); vfprintf(stderr, fmt, a); va_end(a); fprintf(stderr, "\n");
}
static inline double m2f(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── drawbar organ (the offbeat stabs + soft pad) ─────────────────────
// Three detuned unison voices (±7 cents, spread L–C–R) through tanh
// overdrive — the chorus/Leslie thickness + tube warmth that keeps it
// off the thin "additive sine" sound.
typedef struct { double atk, rel, pan, gain, wet_send, sub_amt, upper_amt, drive; } OrganOpts;
static const double ORGAN_PARTIALS[8][2] = {
    {0.5, 0.55}, {1.0, 1.00}, {1.5, 0.42}, {2.0, 0.78},
    {3.0, 0.32}, {4.0, 0.55}, {5.0, 0.20}, {6.0, 0.18},
};
static void organ_render(double t0, double dur, double midi, OrganOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.006;
    const double rel = opt.rel > 0 ? opt.rel : 0.05;
    const double sub = opt.sub_amt >= 0 ? opt.sub_amt : 1.0;
    const double upr = opt.upper_amt >= 0 ? opt.upper_amt : 1.0;
    const double drive = opt.drive > 0 ? opt.drive : 1.7;
    const double ndrive = tanh(drive);
    static const double det[3] = { 0.9959, 1.0, 1.0041 }, vpan[3] = { -0.5, 0.0, 0.5 };
    double phs[3][8] = {{0}};
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR, tRem = dur - t;
        double env = (t < atk) ? t / atk : (tRem < rel ? tRem / rel : 1.0);
        if (env < 0) env = 0;
        const double vib = 1.0 + 0.0009 * sin(TAU * 6.2 * t);   // gentle Leslie
        double oL = 0, oR = 0;
        for (int u = 0; u < 3; u++) {
            double smp = 0, ampsum = 0;
            for (int p = 0; p < 8; p++) {
                double amp = ORGAN_PARTIALS[p][1];
                if (p == 0) amp *= sub; else if (p >= 4) amp *= upr;
                ampsum += amp;
                phs[u][p] += (f * ORGAN_PARTIALS[p][0] * vib * det[u]) / (double)SR; if (phs[u][p] >= 1.0) phs[u][p] -= 1.0;
                smp += amp * sin(TAU * phs[u][p]);
            }
            smp = tanh((smp / ampsum) * drive) / ndrive;   // overdriven, normalized
            const double up = opt.pan + vpan[u] * 0.35;
            oL += smp * (1.0 - 0.5 * up); oR += smp * (1.0 + 0.5 * up);
        }
        oL *= env * opt.gain / 3.0; oR *= env * opt.gain / 3.0;
        L[i] += oL; R[i] += oR; WL[i] += oL * opt.wet_send; WR[i] += oR * opt.wet_send;
    }
}

// ── walking upright bass (rich woody body + string-pluck transient) ──
// Two slightly-detuned voices × 6 harmonics (sawtooth-ish woody body) +
// a bright filtered-noise pluck, all driven through tanh for a fat,
// saturated double-bass — not a thin sine.
static const double UP_HAMP[6] = { 1.0, 0.62, 0.42, 0.30, 0.20, 0.13 };
static void upright_render(double t0, double dur, double midi, double gain, double pan) {
    const double f = m2f(midi);
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur + 0.12) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    static const double det[2] = { 0.997, 1.003 };
    double phs[2][6] = {{0}}, hpI = 0, hpP = 0;
    uint32_t s = (uint32_t)(t0 * 100003.0) | 1;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        const double env = (t < 0.006) ? t / 0.006 : exp(-(t - 0.006) / (dur * 0.7 + 0.12));
        if (t > 0.01 && env < 4e-4) break;
        double body = 0;
        for (int u = 0; u < 2; u++) for (int h = 0; h < 6; h++) {
            phs[u][h] += f * (h + 1) * det[u] / (double)SR; if (phs[u][h] >= 1) phs[u][h] -= 1;
            body += UP_HAMP[h] * sin(TAU * phs[u][h]);
        }
        body /= (2.0 * 2.67);
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double nz = ((double)s / 4294967296.0) * 2 - 1;
        const double hp = 0.9 * (hpP + nz - hpI); hpI = nz; hpP = hp;
        const double pluck = hp * exp(-t / 0.014) * 0.9;
        const double smp = tanh((body + pluck) * 1.9) * 0.72 * env * gain;
        L[i] += smp * (1.0 - 0.5 * pan); R[i] += smp * (1.0 + 0.5 * pan);
        WL[i] += smp * 0.05; WR[i] += smp * 0.05;
    }
}

// ── flute (the hook) ─────────────────────────────────────────────────
// Two chorus-detuned voices (3 harmonics each) + an attack "chiff" noise
// burst + audible breath — a breathy, present flute instead of a thin
// sine. The chiff is what reads as a real player articulating each note.
static void flute_render(double t0, double midi, double dur, double gain, double pan) {
    const double f = m2f(midi);
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur + 0.3) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    static const double det[2] = { 0.996, 1.004 };
    double ph[2][3] = {{0}}, lp = 0; uint32_t s = (uint32_t)(t0 * 1000033.0) | 1;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        double env = (t < 0.04) ? t / 0.04 : (t > dur ? exp(-(t - dur) / 0.11) : 1.0);
        if (t > dur && env < 4e-4) break;
        const double vib = 1.0 + 0.009 * (t / 0.25 < 1 ? t / 0.25 : 1.0) * sin(TAU * 5.2 * t);
        double tone = 0;
        for (int u = 0; u < 2; u++) {
            ph[u][0] += f * vib * det[u] / (double)SR; if (ph[u][0] >= 1) ph[u][0] -= 1;
            ph[u][1] += 2 * f * vib * det[u] / (double)SR; if (ph[u][1] >= 1) ph[u][1] -= 1;
            ph[u][2] += 3 * f * vib * det[u] / (double)SR; if (ph[u][2] >= 1) ph[u][2] -= 1;
            tone += sin(TAU * ph[u][0]) + 0.25 * sin(TAU * ph[u][1]) + 0.10 * sin(TAU * ph[u][2]);
        }
        tone *= 0.5;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double nz = ((double)s / 4294967296.0) * 2 - 1; lp += (nz - lp) * 0.10;
        const double chiff = (t < 0.05) ? nz * exp(-t / 0.02) * 0.5 : 0.0;
        const double smp = (tone + lp * 0.18 + chiff) * env * gain;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.16; WR[i] += sR * 0.16;
    }
}

// ── bell (FM glass sparkle) ──────────────────────────────────────────
static void bell_render(double t0, double midi, double dur, double gain, double pan) {
    const double f = m2f(midi);
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur + 0.8) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double cph = 0, mph = 0;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        double env = exp(-t / (dur * 0.5 + 0.3)); if (t < 0.004) env *= t / 0.004;
        if (env < 2e-4) break;
        const double index = 2.2 * exp(-t / 0.12);
        cph += f / (double)SR; if (cph >= 1) cph -= 1;
        mph += f * 3.51 / (double)SR; if (mph >= 1) mph -= 1;
        const double smp = sin(TAU * cph + index * sin(TAU * mph)) * env * gain;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.16; WR[i] += sR * 0.16;
    }
}

// ── music box (cute twinkly mallet — FM bell, bright + short) ────────
// Brighter, shorter sibling of the bell: high partial ratio, fast decay,
// a tiny attack tick. The "cute instrumentation" sparkle.
static void musicbox_render(double t0, double midi, double gain, double pan) {
    const double f = m2f(midi);
    long iS = (long)(t0 * SR), iE = iS + (long)(0.9 * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double cph = 0, mph = 0;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        double env = exp(-t / 0.28); if (t < 0.002) env *= t / 0.002;
        if (env < 2e-4) break;
        const double index = 1.4 * exp(-t / 0.05);     // quick metallic tick
        cph += f / (double)SR; if (cph >= 1) cph -= 1;
        mph += f * 5.0 / (double)SR; if (mph >= 1) mph -= 1;   // glassy ratio
        double smp = sin(TAU * cph + index * sin(TAU * mph));
        smp += 0.3 * sin(TAU * 2 * cph) * exp(-t / 0.12);       // shimmer octave
        smp *= env * gain;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.18; WR[i] += sR * 0.18;
    }
}

// ── sine bell (pure AC sinebell — soft attack, ringing decay) ────────
static void sinebell_render(double t0, double midi, double gain, double pan) {
    const double f = m2f(midi);
    long iS = (long)(t0 * SR), iE = iS + (long)(1.4 * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double p1 = 0, p2 = 0;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        double env = exp(-t / 0.55); if (t < 0.004) env *= t / 0.004;
        if (env < 2e-4) break;
        p1 += f / (double)SR; if (p1 >= 1) p1 -= 1;
        p2 += 2 * f / (double)SR; if (p2 >= 1) p2 -= 1;
        const double smp = (sin(TAU * p1) + 0.22 * sin(TAU * p2) * exp(-t / 0.18)) * env * gain;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.24; WR[i] += sR * 0.24;
    }
}

// ── vocal chop (formant synthesis — the "ooh-ah" hook) ───────────────
// Glottal sawtooth source → three resonant bandpass FORMANT filters (RBJ
// biquads) tuned to a vowel, morphing vowelA→vowelB across the note for
// the sung "ooh→ah" soul shape. Fully bottom-up: no sample.
enum { VOW_OO = 0, VOW_AH = 1, VOW_EH = 2 };
static const double VF[3][3] = { {320, 870, 2250}, {730, 1090, 2440}, {530, 1840, 2480} };
static const double VG[3][3] = { {1.0, 0.22, 0.10}, {1.0, 0.45, 0.18}, {1.0, 0.33, 0.16} };
static const double VQ[3][3] = { {12, 13, 13}, {10, 11, 12}, {11, 12, 12} };
static void vocal_chop_render(double t0, double midi, double dur, double gain, double pan,
                              int va, int vb, double glideSemis) {
    const double fT = m2f(midi);
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur + 0.18) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double x1[3] = {0}, x2[3] = {0}, y1[3] = {0}, y2[3] = {0};
    double b0[3] = {0}, b2[3] = {0}, a1[3] = {0}, a2[3] = {0};
    double phase = 0, phase2 = 0, subph = 0; const double glideT = 0.07; uint32_t s = (uint32_t)(t0 * 1000003.0) | 1;
    for (long i = iS; i < iE; i++) {
        const long k = i - iS; const double t = k / (double)SR;
        double env;
        if (t < 0.012) env = t / 0.012;
        else if (t > dur) env = exp(-(t - dur) / 0.07);
        else env = 0.85 + 0.15 * exp(-(t - 0.012) / 0.2);
        if (t > dur && env < 4e-4) break;
        const double gfrac = 1.0 - t / glideT; const double glide = glideSemis * (gfrac > 0 ? gfrac : 0.0);
        const double vib = 1.0 + 0.01 * (t / 0.12 < 1 ? t / 0.12 : 1.0) * sin(TAU * 5.6 * t);
        const double f = fT * pow(2.0, glide / 12.0) * vib;
        phase += f / (double)SR; if (phase >= 1.0) phase -= 1.0;
        phase2 += f * 1.006 / (double)SR; if (phase2 >= 1.0) phase2 -= 1.0;   // detuned twin
        subph += f * 0.5 / (double)SR; if (subph >= 1.0) subph -= 1.0;        // sub octave
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        // richer glottal source: two detuned saws + a sub sine + breath
        double src = 0.55 * (2.0 * (phase - floor(phase + 0.5)))
                   + 0.45 * (2.0 * (phase2 - floor(phase2 + 0.5)))
                   + 0.30 * sin(TAU * subph);
        src = src * 0.7 + (((double)s / 4294967296.0) * 2 - 1) * 0.05;
        const double mr = t / (dur * 0.6 + 0.001); const double m = mr > 1 ? 1 : mr;
        if ((k & 31) == 0) for (int j = 0; j < 3; j++) {
            const double fc = VF[va][j] + (VF[vb][j] - VF[va][j]) * m, Q = VQ[va][j];
            const double w0 = TAU * fc / (double)SR, cs = cos(w0), sn = sin(w0), al = sn / (2 * Q), a0 = 1 + al;
            b0[j] = al / a0; b2[j] = -al / a0; a1[j] = (-2 * cs) / a0; a2[j] = (1 - al) / a0;
        }
        double out = 0;
        for (int j = 0; j < 3; j++) {
            const double g = VG[va][j] + (VG[vb][j] - VG[va][j]) * m;
            const double y = b0[j] * src + b2[j] * x2[j] - a1[j] * y1[j] - a2[j] * y2[j];
            x2[j] = x1[j]; x1[j] = src; y2[j] = y1[j]; y1[j] = y;
            out += y * g;
        }
        const double smp = tanh(out * env * gain * 1.76);
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.15; WR[i] += sR * 0.15;
    }
}

// ── kick (pitched sine sweep + click) ────────────────────────────────
static void kick_render(double t0, double gain) {
    long iS = (long)(t0 * SR), iE = iS + (long)(0.4 * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double phase = 0; uint32_t s = (uint32_t)(t0 * 13337.0) | 1;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        const double freq = (t < 0.028) ? 115.0 * pow(48.0 / 115.0, t / 0.028) : 48.0;
        const double env = (t < 0.002) ? t / 0.002 : exp(-(t - 0.002) / 0.15);
        if (t > 0.005 && env < 1e-5) break;
        phase += freq / (double)SR; if (phase >= 1) phase -= 1;
        double smp = sin(TAU * phase) * env;
        if (t < 0.003) { s ^= s << 13; s ^= s >> 17; s ^= s << 5; smp += (((double)s / 4294967296.0) * 2 - 1) * (1 - t / 0.003) * 0.5; }
        smp = tanh(smp * gain * 1.2);
        L[i] += smp; R[i] += smp;
    }
}

// ── AC-native perc kit (sine/tri/sqr/noise hit) ──────────────────────
enum PercWave { PW_SINE, PW_TRI, PW_SQR, PW_NOISE };
static void perc_hit(double t0, int wave, double tone, double dur, double vol,
                     double atk, double dec, double pan, double wet) {
    long iS = (long)(t0 * SR), iE = (long)((t0 + dur) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double phase = 0; const double dP = tone / (double)SR; uint32_t s = (uint32_t)(t0 * 99877.0) | 1;
    double lp = 0; const double lpc = exp(-TAU * tone / (double)SR); const double a = atk > 0 ? atk : 1e-6;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        const double env = (t < a) ? t / a : exp(-(t - a) / dec);
        if (t > a + 0.0005 && env < 1e-5) break;
        double osc;
        if (wave == PW_NOISE) { s ^= s << 13; s ^= s >> 17; s ^= s << 5; const double w = ((double)s / 4294967296.0) * 2 - 1; lp = lp * lpc + (1 - lpc) * w; osc = lp * 2; }
        else { phase += dP; if (phase >= 1) phase -= 1;
            if (wave == PW_SINE) osc = sin(TAU * phase);
            else if (wave == PW_TRI) osc = 2.0 * fabs(2.0 * (phase - floor(phase + 0.5))) - 1.0;
            else osc = (phase < 0.5) ? 1.0 : -1.0; }
        const double smp = osc * env * vol;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * wet; WR[i] += sR * wet;
    }
}
static void tambo(double t0, double v, double pan) {
    perc_hit(t0, PW_NOISE, 7000, 0.080, 0.38 * v, 0.002, 0.075, pan, 0.18);
    perc_hit(t0, PW_NOISE, 6500, 0.100, 0.25 * v, 0.030, 0.070, pan, 0.18);
    perc_hit(t0, PW_SQR,   6000, 0.030, 0.14 * v, 0.001, 0.028, pan, 0.10);
}
static void snare808(double t0, double v, double pan) {
    perc_hit(t0, PW_SINE,  200, 0.20, 0.55 * v, 0.0005, 0.16, pan, 0.12);
    perc_hit(t0, PW_SINE,  330, 0.14, 0.30 * v, 0.0005, 0.10, pan, 0.12);
    perc_hit(t0, PW_NOISE, 3800, 0.20, 0.70 * v, 0.0004, 0.16, pan, 0.16);
}

// ── conga (pitched membrane + skin noise) ────────────────────────────
static void conga_render(double t0, int slap, double gain, double pan) {
    const double f0 = slap ? 380.0 : 210.0, dec = slap ? 0.09 : 0.16;
    long iS = (long)(t0 * SR), iE = iS + (long)((dec + 0.02) * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    double ph = 0, hpI = 0, hpP = 0; uint32_t s = (uint32_t)(t0 * 99991.0) | 1;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        ph += (f0 + f0 * 1.4 * exp(-t / 0.012)) / (double)SR; if (ph >= 1) ph -= 1;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5; const double nz = ((double)s / 4294967296.0) * 2 - 1;
        const double hp = 0.7 * (hpP + nz - hpI); hpI = nz; hpP = hp;
        const double smp = (sin(TAU * ph) * 0.9 + hp * 0.25 * exp(-t / 0.02)) * exp(-t / dec) * gain;
        const double sL = smp * (1.0 - 0.5 * pan), sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR; WL[i] += sL * 0.08; WR[i] += sR * 0.08;
    }
}

// ── hi-hat (doubled noise burst) ─────────────────────────────────────
static void hat_burst(double t0, double pan, double gain, double decay, double bright, uint32_t seed) {
    long iS = (long)(t0 * SR), iE = iS + (long)(decay * 6.0 * SR);
    if (iS < 0) iS = 0; if (iE > N) iE = N;
    uint32_t s = seed | 1; double i1 = 0, o1 = 0, i2 = 0, o2 = 0;
    for (long i = iS; i < iE; i++) {
        const double t = (i - iS) / (double)SR;
        const double env = (t < 0.001) ? t / 0.001 : exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5; const double nz = ((double)s / 4294967296.0) * 2 - 1;
        const double h1 = bright * (o1 + nz - i1); i1 = nz; o1 = h1;
        const double h2 = bright * (o2 + h1 - i2); i2 = h1; o2 = h2;
        const double smp = h2 * env * gain;
        L[i] += smp * (1.0 - 0.5 * pan); R[i] += smp * (1.0 + 0.5 * pan);
    }
}
static void hihat(double t0, double pan, double gain, int open) {
    const double dec = open ? 0.14 : 0.032;
    hat_burst(t0, pan, gain, dec, 0.93, (uint32_t)(t0 * 7919.0 + 31));
    hat_burst(t0 + 0.004, pan, gain * 0.6, dec * 0.85, 0.9, (uint32_t)(t0 * 8087.0 + 17));
}

// ── Schroeder reverb ─────────────────────────────────────────────────
typedef struct { float *buf; long size; long idx; float feedback; } Comb;
typedef struct { float *buf; long size; long idx; } Allpass;
static void comb_init(Comb *c, long size, float fb) { c->buf = calloc(size, sizeof(float)); c->size = size; c->idx = 0; c->feedback = fb; }
static inline float comb_tick(Comb *c, float in) { float out = c->buf[c->idx]; c->buf[c->idx] = in + out * c->feedback; if (++c->idx >= c->size) c->idx = 0; return out; }
static void allpass_init(Allpass *a, long size) { a->buf = calloc(size, sizeof(float)); a->size = size; a->idx = 0; }
static inline float allpass_tick(Allpass *a, float in) { float bo = a->buf[a->idx]; float out = -in + bo; a->buf[a->idx] = in + bo * 0.5f; if (++a->idx >= a->size) a->idx = 0; return out; }
static void apply_reverb(double wet) {
    static const int dl_L[4] = { 1116, 1188, 1277, 1356 };
    static const int dl_R[4] = { 1139, 1211, 1300, 1379 };
    const double sc = (double)SR / 44100.0;
    Comb cL[4], cR[4]; for (int i = 0; i < 4; i++) { comb_init(&cL[i], (long)(dl_L[i] * sc), 0.82f); comb_init(&cR[i], (long)(dl_R[i] * sc), 0.82f); }
    Allpass apL[2], apR[2];
    allpass_init(&apL[0], (long)(556 * sc)); allpass_init(&apL[1], (long)(441 * sc));
    allpass_init(&apR[0], (long)(589 * sc)); allpass_init(&apR[1], (long)(467 * sc));
    for (long i = 0; i < N; i++) {
        float sumL = 0, sumR = 0;
        for (int k = 0; k < 4; k++) { sumL += comb_tick(&cL[k], WL[i]); sumR += comb_tick(&cR[k], WR[i]); }
        sumL = allpass_tick(&apL[0], sumL); sumL = allpass_tick(&apL[1], sumL);
        sumR = allpass_tick(&apR[0], sumR); sumR = allpass_tick(&apR[1], sumR);
        L[i] += sumL * wet; R[i] += sumR * wet;
    }
    for (int i = 0; i < 4; i++) { free(cL[i].buf); free(cR[i].buf); }
    for (int i = 0; i < 2; i++) { free(apL[i].buf); free(apR[i].buf); }
}

// ── global stereo chorus (modulated delay — width + analog motion) ───
// Two LFO-modulated delay lines (one per channel, different rates) mixed
// in ~28% wet. This is the single biggest "un-MIDI" move: it smears the
// dry phase so the bed breathes and widens instead of sitting static.
static inline double dread(const float *buf, long size, long wi, double dly) {
    double rp = (double)wi - dly; while (rp < 0) rp += size;
    long i0 = (long)rp; double fr = rp - i0; long i1 = i0 + 1; if (i1 >= size) i1 -= size;
    return buf[i0] * (1.0 - fr) + buf[i1] * fr;
}
static void apply_chorus(double wet) {
    const long maxd = (long)(0.014 * SR) + 4;
    float *dl = calloc(maxd, sizeof(float)), *dr = calloc(maxd, sizeof(float));
    long wi = 0;
    for (long i = 0; i < N; i++) {
        dl[wi] = L[i]; dr[wi] = R[i];
        const double t = (double)i / SR;
        const double m1 = (0.007 + 0.0045 * sin(TAU * 0.5 * t)) * SR;
        const double m2 = (0.007 + 0.0045 * sin(TAU * 0.34 * t + 1.7)) * SR;
        const double rL = dread(dl, maxd, wi, m1), rR = dread(dr, maxd, wi, m2);
        L[i] = (float)(L[i] * (1.0 - wet * 0.5) + rL * wet);
        R[i] = (float)(R[i] * (1.0 - wet * 0.5) + rR * wet);
        if (++wi >= maxd) wi = 0;
    }
    free(dl); free(dr);
}

// ── SCORE: bright 8-bar loop (ii-V motion) ───────────────────────────
typedef struct { int n[4]; const char *name; } Chord;
static const Chord PROG[8] = {
    { {60, 64, 65, 69}, "Fmaj7" }, { {60, 64, 67, 69}, "Am7" },
    { {60, 62, 65, 69}, "Dm7" },   { {59, 62, 65, 67}, "G7" },
    { {60, 64, 65, 69}, "Fmaj7" }, { {59, 62, 64, 67}, "Em7" },
    { {60, 64, 67, 69}, "Am7" },   { {60, 62, 65, 67}, "Gm7C7" },
};
// walking double-bass: 4 quarters/bar, landing a step from the next root.
static const int WALK[8][4] = {
    {41, 45, 48, 44}, {45, 48, 52, 39}, {38, 41, 45, 42}, {43, 46, 38, 40},
    {41, 45, 48, 41}, {40, 43, 47, 46}, {45, 48, 52, 44}, {43, 46, 48, 41},
};

// hook melody (flute). loopBar, 16th-step, midi, dur in beats.
typedef struct { int lb, step, midi; double durB; } HookNote;
static const HookNote HOOK[] = {
    {0, 4, 69, 0.5}, {0, 6, 72, 0.5}, {0, 10, 69, 1.0},
    {1, 4, 67, 0.5}, {1, 6, 69, 1.5},
    {2, 4, 65, 0.5}, {2, 6, 69, 0.5}, {2, 8, 72, 1.0},
    {3, 4, 74, 0.5}, {3, 6, 67, 0.5}, {3, 8, 71, 1.0},
    {4, 4, 69, 0.5}, {4, 6, 72, 0.5}, {4, 10, 69, 1.0},
    {5, 4, 67, 0.5}, {5, 6, 71, 1.5},
    {6, 2, 72, 0.5}, {6, 4, 76, 0.5}, {6, 8, 72, 0.5}, {6, 10, 69, 1.0},
    {7, 4, 70, 0.5}, {7, 8, 67, 0.5}, {7, 10, 65, 1.0},
};
static const int HOOK_N = (int)(sizeof(HOOK) / sizeof(HOOK[0]));

// vocal chop hits: 16th-step, chord-tone index, vowelA, vowelB, dur(beats), glide(semis)
typedef struct { int step, idx, va, vb; double durB, glide; } VoxHit;
static const VoxHit VOX_BUSY[] = {
    {0, 3, VOW_OO, VOW_AH, 0.5, 0}, {3, 2, VOW_AH, VOW_AH, 0.5, -2},
    {6, 3, VOW_OO, VOW_OO, 0.5, 0}, {8, 1, VOW_AH, VOW_AH, 0.5, 0},
    {11, 3, VOW_OO, VOW_AH, 0.75, 0}, {14, 2, VOW_EH, VOW_EH, 0.25, 0},
};
static const VoxHit VOX_SPARSE[] = { {2, 3, VOW_OO, VOW_AH, 0.5, 0}, {11, 2, VOW_OO, VOW_OO, 0.75, 0} };

// patterns
static const int KICK_S[3] = {0, 7, 10};
static const int SNARE_S[2] = {4, 12};
static const int GHOST_S[2] = {7, 15};
static const int HAT8[8] = {0, 2, 4, 6, 8, 10, 12, 14};
static const int TAMB_S[4] = {2, 6, 10, 14};
static const int CONGA_S[5] = {3, 6, 8, 11, 14};
static const int CONGA_SLAP[5] = {0, 1, 0, 0, 1};
static const int ORGAN_S[5] = {2, 6, 9, 10, 14};

// section flags — arc for FLOW. drums/perc are sequenced in the JS layer
// (freesound), so those columns stay 0; the bed carries the musical arc.
//                    name      bars soft organ bass flute vox music vpad
typedef struct { const char *name; int bars, soft, organ, bass, flute, vox, music, vpad; } Sec;
static const Sec SECTIONS[] = {
    { "intro",  2, 1, 0, 1, 0, 1, 0, 1 },   // soft pad + light bass + dreamy vox pad
    { "groove", 8, 0, 1, 1, 0, 2, 0, 0 },   // organ + walking bass + busy vocal chop
    { "hook",   8, 0, 1, 1, 1, 1, 1, 0 },   // + flute hook + music-box twinkle
    { "break",  2, 1, 0, 1, 1, 0, 0, 1 },   // breather: pad + flute + woo-woo pad
    { "drop",   8, 0, 1, 1, 1, 1, 1, 0 },   // full hook energy
    { "outro",  2, 1, 1, 0, 0, 0, 1, 1 },   // pad + sparse twinkle, fade
};
static const int NSEC = (int)(sizeof(SECTIONS) / sizeof(SECTIONS[0]));

// ── render the full track ────────────────────────────────────────────
static double SPB, BAR, S16;
static double t16(int bar, int s) { const int pb = (s / 2) * 2; const double off = (s & 1) ? 2.0 * SWING : 0.0; return bar * BAR + (pb + off) * S16; }

static void render_track(void) {
    SPB = 60.0 / BPM; BAR = 4.0 * SPB; S16 = SPB / 4.0;
    int total_bars = 0; for (int i = 0; i < NSEC; i++) total_bars += SECTIONS[i].bars;
    report("hopehop: bpm=%.1f swing=%.2f bar=%.3fs · %d bars · %.1fs", BPM, SWING, BAR, total_bars, TOTAL_SEC);

    int bar = 0;
    for (int si = 0; si < NSEC; si++) {
        const Sec *sec = &SECTIONS[si];
        for (int b = 0; b < sec->bars; b++, bar++) {
            const int lb = bar % 8;
            const Chord *c = &PROG[lb];
            const double baseT = bar * BAR;

            // organ — offbeat stabs (the bounce)
            if (sec->organ) {
                for (int k = 0; k < 5; k++) {
                    const double t = t16(bar, ORGAN_S[k]) + jit(0.004);
                    for (int v = 0; v < 4; v++)
                        organ_render(t, S16 * 1.1, c->n[v],
                            (OrganOpts){ .atk = 0.006, .rel = 0.05, .pan = jit(0.22),
                                         .gain = 0.14, .wet_send = 0.12, .sub_amt = 0.5, .upper_amt = 0.85 });
                }
            }
            // soft organ pad — intro / break / outro warmth
            if (sec->soft) {
                for (int v = 0; v < 4; v++)
                    organ_render(baseT + jit(0.004), 3.0 * SPB, c->n[v],
                        (OrganOpts){ .atk = 0.4, .rel = 0.6, .pan = jit(0.25),
                                     .gain = 0.06, .wet_send = 0.3, .sub_amt = 0.8, .upper_amt = 0.4 });
                organ_render(baseT, 3.0 * SPB, c->n[0] - 12,
                    (OrganOpts){ .atk = 0.5, .rel = 0.6, .pan = 0, .gain = 0.05, .wet_send = 0.3, .sub_amt = 1.0, .upper_amt = 0.3 });
            }
            // walking upright bass — straight quarters (pulled back so the
            // pitched PANTHER can sing the low end in the JS layer)
            if (sec->bass) for (int beat = 0; beat < 4; beat++)
                upright_render(baseT + beat * SPB + jit(0.004), WALK[lb][beat], 0.92 * SPB, 0.32, 0.0);

            // optional built-in synth kit (--synth-drums); default OFF —
            // the JS driver sequences freesound trap samples instead.
            if (SYNTH_PERC) {
                for (int k = 0; k < 3; k++) kick_render(t16(bar, KICK_S[k]) + jit(0.004), 0.9);
                for (int k = 0; k < 2; k++) snare808(t16(bar, SNARE_S[k]) + jit(0.005), 0.6, 0.04);
                for (int k = 0; k < 2; k++) snare808(t16(bar, GHOST_S[k]) + jit(0.005), 0.13, 0.04);
                for (int k = 0; k < 8; k++) hihat(t16(bar, HAT8[k]) + jit(0.003), jit(0.35), ((HAT8[k] % 4 == 2) ? 0.32 : 0.2) + jit(0.04), 0);
                for (int k = 0; k < 4; k++) tambo(t16(bar, TAMB_S[k]) + jit(0.004), 0.16, jit(0.45));
                for (int k = 0; k < 5; k++) conga_render(t16(bar, CONGA_S[k]) + jit(0.004), CONGA_SLAP[k], 0.26, jit(0.4));
            }
            // flute hook (+ a soft sparkle bell on the climax bar)
            if (sec->flute) {
                for (int k = 0; k < HOOK_N; k++) if (HOOK[k].lb == lb)
                    flute_render(t16(bar, HOOK[k].step), HOOK[k].midi, SPB * HOOK[k].durB, 0.10, jit(0.18));
                if (lb == 6) bell_render(t16(bar, 12), 81, SPB * 1.0, 0.09, 0.3);
            }
            // music box — cute high twinkle (chord tones, two octaves up)
            if (sec->music) {
                static const int TW_STEP[4] = { 0, 5, 8, 13 };
                static const int TW_IDX[4]  = { 3, 0, 2, 3 };
                for (int k = 0; k < 4; k++)
                    musicbox_render(t16(bar, TW_STEP[k]) + jit(0.004), c->n[TW_IDX[k]] + 24, 0.12, jit(0.4));
                // sine bells ring a sparse counter-line over the music box
                sinebell_render(t16(bar, 6) + jit(0.004), c->n[1] + 12, 0.10, jit(0.35));
                if (lb % 2 == 1) sinebell_render(t16(bar, 12) + jit(0.004), c->n[3] + 12, 0.09, jit(0.4));
            }
            // sine-bell twinkles drift through the soft sections (intro/break/outro)
            if (sec->soft && lb % 2 == 0)
                sinebell_render(t16(bar, 8) + jit(0.004), c->n[2] + 12, 0.09, jit(0.4));
            // vocal chop — the "woo woo", brought forward
            if (sec->vox) {
                const VoxHit *pat = sec->vox == 2 ? VOX_BUSY : VOX_SPARSE;
                const int pn = sec->vox == 2 ? (int)(sizeof(VOX_BUSY) / sizeof(VOX_BUSY[0]))
                                             : (int)(sizeof(VOX_SPARSE) / sizeof(VOX_SPARSE[0]));
                for (int k = 0; k < pn; k++) {
                    const int m = c->n[pat[k].idx < 4 ? pat[k].idx : 3];
                    vocal_chop_render(t16(bar, pat[k].step) + jit(0.004), m, SPB * pat[k].durB,
                                      0.26, jit(0.2), pat[k].va, pat[k].vb, pat[k].glide);
                }
            }
            // dreamy sustained vocal pad — held "wooo" choir chord (woo woo)
            if (sec->vpad) for (int v = 1; v < 4; v++)
                vocal_chop_render(baseT, c->n[v], BAR * 0.95, 0.10, (v - 2) * 0.4, VOW_OO, VOW_AH, 0.0);
        }
    }
    report("  scheduled %d bars", bar);
    apply_chorus(0.14);
    report("  chorus applied");
    apply_reverb(0.11);
    report("  reverb applied");
}

// ── WAV writer (tanh drive → normalize to -3 dB) ─────────────────────
static void write_wav(const char *path) {
    double peak = 0; for (long i = 0; i < N; i++) { double a = fabs(L[i]); if (a > peak) peak = a; double b = fabs(R[i]); if (b > peak) peak = b; }
    // Pre-gain into tanh so the saturation amount is CONSISTENT regardless
    // of arrangement density — pushes the sum into tanh's warm knee for
    // analog glue (gentle, capped) instead of staying clean-thin or
    // hard-clipping. This is tape-style drive, not a brick limiter.
    double pre = peak > 0 ? 1.3 / peak : 1.0; if (pre > 3.0) pre = 3.0;
    report("  peak before tanh: %.3f → pregain %.2f × drive %.2f", peak, pre, DRIVE);
    double pp = 0;
    for (long i = 0; i < N; i++) { L[i] = tanhf(L[i] * pre * DRIVE); R[i] = tanhf(R[i] * pre * DRIVE); double a = fabs(L[i]); if (a > pp) pp = a; double b = fabs(R[i]); if (b > pp) pp = b; }
    const double target = pow(10.0, -3.0 / 20.0); const double g = pp > 0 ? target / pp : 1.0;
    for (long i = 0; i < N; i++) { L[i] *= g; R[i] *= g; }
    FILE *f = fopen(path, "wb"); if (!f) { fprintf(stderr, "hopehop: cannot open %s\n", path); exit(1); }
    const long data_size = N * 2 * 2; uint8_t h[44];
    memcpy(h, "RIFF", 4); *(uint32_t*)(h + 4) = (uint32_t)(36 + data_size); memcpy(h + 8, "WAVE", 4);
    memcpy(h + 12, "fmt ", 4); *(uint32_t*)(h + 16) = 16; *(uint16_t*)(h + 20) = 1; *(uint16_t*)(h + 22) = 2;
    *(uint32_t*)(h + 24) = (uint32_t)SR; *(uint32_t*)(h + 28) = (uint32_t)(SR * 4); *(uint16_t*)(h + 32) = 4; *(uint16_t*)(h + 34) = 16;
    memcpy(h + 36, "data", 4); *(uint32_t*)(h + 40) = (uint32_t)data_size; fwrite(h, 1, 44, f);
    int16_t *out = malloc(N * 2 * sizeof(int16_t));
    for (long i = 0; i < N; i++) {
        double sL = L[i], sR = R[i]; if (sL > 1) sL = 1; else if (sL < -1) sL = -1; if (sR > 1) sR = 1; else if (sR < -1) sR = -1;
        out[2 * i] = (int16_t)lrint(sL * 32767.0); out[2 * i + 1] = (int16_t)lrint(sR * 32767.0);
    }
    fwrite(out, sizeof(int16_t), N * 2, f); free(out); fclose(f);
    report("✓ wrote %s (%.2f s · %d Hz · stereo)", path, (double)N / SR, SR);
}

int main(int argc, char **argv) {
    t0_wall = now_wall();
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) OUT_PATH = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPM = atof(argv[++i]);
        else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]);
        else if (!strcmp(argv[i], "--seconds") && i + 1 < argc) TOTAL_SEC = atof(argv[++i]);
        else if (!strcmp(argv[i], "--drive") && i + 1 < argc) DRIVE = atof(argv[++i]);
        else if (!strcmp(argv[i], "--seed") && i + 1 < argc) SEED_ARG = (unsigned)strtoul(argv[++i], NULL, 10);
        else if (!strcmp(argv[i], "--synth-drums")) SYNTH_PERC = 1;
        else { fprintf(stderr, "hopehop: unknown arg %s\n", argv[i]); return 1; }
    }
    if (!OUT_PATH) { fprintf(stderr, "hopehop: --out required\n"); return 1; }
    rng_seed(SEED_ARG ? SEED_ARG : (uint32_t)time(NULL));
    N = (long)(TOTAL_SEC * SR);
    L = calloc(N, sizeof(float)); R = calloc(N, sizeof(float)); WL = calloc(N, sizeof(float)); WR = calloc(N, sizeof(float));
    if (!L || !R || !WL || !WR) { fprintf(stderr, "hopehop: alloc failed\n"); return 1; }
    report("hopehop: rendering %.2f s @ %d Hz", TOTAL_SEC, SR);
    render_track();
    write_wav(OUT_PATH);
    free(L); free(R); free(WL); free(WR);
    return 0;
}
