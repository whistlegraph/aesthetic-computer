// sleephellsine.c — circle-of-fifths chord progression.
//
// A 15-minute sleep mix where the harmony walks the CIRCLE OF FIFTHS
// once every 60 seconds. Two upper sine voices each play a chord tone
// (M3 + P5) of the current chord. They oscillate around the chord's
// midpoint in contrary motion — when one is up at the P5, the other is
// down at the M3 — and cross twice per chord. Each crossing TRIGGERS a
// long sine ROOT BELL deep below them, and the harmony evolves into the
// next chord around the circle.
//
// 12 chord roots × 5 s each = 60 s per rotation. 900 s total = 15 full
// rotations through the wheel. The chord centre is voice-led smoothly
// between adjacent chords so the upper register glides rather than
// jumps; the root bells punctuate each crossing as a deep cushion.
//
// Build:  bash pop/sleephellsine/c/build.sh
// Usage:  ./sleephellsine --out FILE.wav [--seed sleephellsine]

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <pthread.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ── config ────────────────────────────────────────────────────────────
static const int SR = 48000;
static const double TOTAL_SEC = 910.0;       // ~15:10; bake truncates to 15:00
static const double FADE_IN_VOICE = 60.0;
static const double FADE_OUT = 14.0;
static const char *SEED_STR = "sleephellsine";
static const char *OUT_PATH = NULL;

// Circle of fifths — semitone offsets from C, advancing one fifth per
// step. 12 steps complete one rotation. The harmonic root walks this
// wheel; the upper voices and bells are derived from the current step.
static const int CIRCLE_OF_FIFTHS[12] = {
    0, 7, 2, 9, 4, 11, 6, 1, 8, 3, 10, 5,
//  C  G  D  A  E  B   F# C# G# D# A# F
};
#define N_CIRCLE 12
static const double CHORD_DUR_SEC = 5.0;       // 12 × 5 = 60 s per rotation

// Registers
static const int ROOT_BELL_BASE     = 36;       // C2 — root bell low octave
static const double UPPER_CENTER_REF = 64.0;    // E4 — preferred upper-voice centre

// Upper-voice carriers — each voice oscillates between (centre - 1.5)
// and (centre + 1.5) semitones, where centre ≈ midway between the M3
// and the P5 of the current chord. So at the oscillation extremes the
// voices land exactly on chord tones; at the zero-crossings they cross
// each other.
static const double UPPER_AMP_ST = 1.5;        // ±1.5 ST = M3 ↔ P5 swing

// One-pole glide on the chord centre so it voice-leads between chords
// (smooth pitch drift across chord boundaries).
// 0.00004 ≈ 520 ms TC at 48 kHz
static const double CENTER_GLIDE = 0.00004;

// Wavery — gentle random-walk pitch wobble on top of the held tones
static const double WAVERY_CENTS    = 6.0;
static const double WAVERY_UPDATE_MS = 8.0;
static const double WAVERY_SMOOTH   = 0.020;

// Phase-shifting detune (cents — AC "crosssies")
static const double DETUNE_CENTS_PAIR = 4.0;

// Root bell — long, deep, slow attack
static const double ROOT_BELL_ATK    = 0.80;   // 800 ms slow attack
static const double ROOT_BELL_TAU    = 9.0;    // 9 s exponential decay
static const double ROOT_BELL_GAIN   = 0.060;
static const double ROOT_BELL_WET    = 0.50;

// Brownian noise bed — soft "red" rumble underneath the carriers.
static const double BROWN_LEAK    = 0.998;
static const double BROWN_DRIVE   = 0.012;
static const double BROWN_GAIN    = 0.50;
static const double BROWN_FADE_IN = 90.0;

// ── deterministic RNG (xorshift32 keyed by FNV-1a) ────────────────────
static uint32_t xorshift_state = 0;
static uint32_t fnv1a(const char *s) {
    uint32_t h = 2166136261u;
    while (*s) { h ^= (unsigned char)*s++; h *= 16777619u; }
    return h ? h : 1;
}
static inline double rng(void) {
    uint32_t s = xorshift_state;
    s ^= s << 13;
    s ^= s >> 17;
    s ^= s << 5;
    xorshift_state = s;
    return (double)s / 4294967296.0;
}
static inline double hum(double amt) { return (rng() * 2.0 - 1.0) * amt; }

static inline double m2f(double m) {
    return 440.0 * pow(2.0, (m - 69.0) / 12.0);
}

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

// ── shared mix buffers ────────────────────────────────────────────────
static long N = 0;
static float *L = NULL;
static float *R = NULL;
static float *WL = NULL;
static float *WR = NULL;

// ── circle-of-fifths helpers ──────────────────────────────────────────
// Get the chord step at a given time, wrapped to [0, N_CIRCLE).
static inline int step_at(double t) {
    int step = (int)floor(t / CHORD_DUR_SEC);
    int s = step % N_CIRCLE;
    if (s < 0) s += N_CIRCLE;
    return s;
}
// Get the chord pitch class (0..11) at a given time.
static inline int root_pc_at(double t) {
    return CIRCLE_OF_FIFTHS[step_at(t)];
}
// Chord centre target: the pitch midway between M3 and P5 of the chord,
// nudged into the [UPPER_CENTER_REF - 6, UPPER_CENTER_REF + 6] window so
// the upper voices stay in the E4 octave region across all 12 chords.
static inline double chord_center_target(double t) {
    double c = root_pc_at(t) + 5.5;        // pitch-class space, midpoint of M3/P5
    while (c < UPPER_CENTER_REF - 6.0) c += 12.0;
    while (c > UPPER_CENTER_REF + 6.0) c -= 12.0;
    return c;
}
// Root bell midi pitch — chord root in C2 octave.
static inline double root_bell_midi(double t) {
    return (double)ROOT_BELL_BASE + (double)root_pc_at(t);
}

// ── bell voice (pure sine, slow attack, exp decay) ────────────────────
typedef struct { double pan, atk, dec_tau, wet_send, gain; } BellOpts;
static void bell_render(double t0, double midi, BellOpts opt) {
    const double f = m2f(midi);
    const double dPh = f / (double)SR;
    double ph = 0.0;
    const double tailDur = opt.dec_tau * 5.0;
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + opt.atk + tailDur) * SR + 1);
    if (iEnd > N) iEnd = N;
    const double pL = (opt.pan > 0 ? 1.0 - opt.pan : 1.0);
    const double pR = (opt.pan < 0 ? 1.0 + opt.pan : 1.0);
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double env = lt < opt.atk
            ? lt / opt.atk
            : exp(-(lt - opt.atk) / opt.dec_tau);
        ph += dPh; if (ph >= 1.0) ph -= 1.0;
        const double v = sin(TAU * ph) * env * opt.gain;
        const double vL = v * pL;
        const double vR = v * pR;
        L[i] += (float)vL;
        R[i] += (float)vR;
        if (opt.wet_send > 0.0) {
            WL[i] += (float)(vL * opt.wet_send);
            WR[i] += (float)(vR * opt.wet_send);
        }
    }
}

// ── Schroeder reverb (4 combs + 2 allpasses), one channel per thread ──
static const double COMB_L_D[4] = {0.0437, 0.0541, 0.0641, 0.0727};
static const double COMB_R_D[4] = {0.0451, 0.0557, 0.0663, 0.0741};
static const double COMB_FB = 0.78;
static const double AP_D[2] = {0.0089, 0.0027};
static const double AP_FB = 0.5;

typedef struct {
    const float *in;
    float *out;
    const double *combs;
} ReverbJob;

static void *reverb_thread(void *arg) {
    ReverbJob *job = (ReverbJob*)arg;
    int combLens[4]; float *combLines[4]; int combIdx[4] = {0,0,0,0};
    int apLens[2];   float *apLines[2];   int apIdx[2]   = {0,0};
    for (int c = 0; c < 4; c++) {
        combLens[c] = (int)(job->combs[c] * SR);
        combLines[c] = (float*)calloc(combLens[c], sizeof(float));
    }
    for (int a = 0; a < 2; a++) {
        apLens[a] = (int)(AP_D[a] * SR);
        apLines[a] = (float*)calloc(apLens[a], sizeof(float));
    }
    for (long i = 0; i < N; i++) {
        const double in = job->in[i];
        double combOut = 0.0;
        for (int c = 0; c < 4; c++) {
            const int idx = combIdx[c];
            const double delayed = combLines[c][idx];
            combLines[c][idx] = (float)(in + delayed * COMB_FB);
            combIdx[c] = (idx + 1) % combLens[c];
            combOut += delayed;
        }
        combOut *= 0.25;
        double apOut = combOut;
        for (int a = 0; a < 2; a++) {
            const int idx = apIdx[a];
            const double delayed = apLines[a][idx];
            const double newS = apOut + delayed * AP_FB;
            apLines[a][idx] = (float)newS;
            apOut = delayed - newS * AP_FB;
            apIdx[a] = (idx + 1) % apLens[a];
        }
        job->out[i] = (float)apOut;
    }
    for (int c = 0; c < 4; c++) free(combLines[c]);
    for (int a = 0; a < 2; a++) free(apLines[a]);
    return NULL;
}

// ── 32-bit float stereo WAV writer ────────────────────────────────────
static void write_wav_f32_stereo(const char *path, const float *l, const float *r, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { perror("fopen"); exit(1); }
    const uint32_t dataLen = (uint32_t)(n * 2 * 4);
    const uint32_t riffSize = 36 + dataLen;
    const uint32_t fmtSize = 16;
    const uint16_t fmtCode = 3;
    const uint16_t ch = 2;
    const uint32_t sr = (uint32_t)SR;
    const uint32_t byteRate = SR * 2 * 4;
    const uint16_t blockAlign = 2 * 4;
    const uint16_t bps = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riffSize, 4, 1, f);
    fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fmtSize, 4, 1, f);
    fwrite(&fmtCode, 2, 1, f); fwrite(&ch, 2, 1, f);
    fwrite(&sr, 4, 1, f); fwrite(&byteRate, 4, 1, f);
    fwrite(&blockAlign, 2, 1, f); fwrite(&bps, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dataLen, 4, 1, f);
    float *interleaved = (float*)malloc((size_t)n * 2 * sizeof(float));
    for (long i = 0; i < n; i++) {
        interleaved[i * 2 + 0] = l[i];
        interleaved[i * 2 + 1] = r[i];
    }
    fwrite(interleaved, sizeof(float), (size_t)n * 2, f);
    free(interleaved);
    fclose(f);
}

// ── main ──────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    t0_wall = now_wall();

    char *out_default = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) { OUT_PATH = argv[++i]; }
        else if (!strcmp(argv[i], "--seed") && i + 1 < argc) { SEED_STR = argv[++i]; }
    }
    if (!OUT_PATH) {
        const char *home = getenv("HOME"); if (!home) home = ".";
        const size_t LEN = strlen(home) + 100;
        out_default = (char*)malloc(LEN);
        snprintf(out_default, LEN,
                 "%s/Documents/Shelf/sleephellsine/.sleephellsine-pre.wav", home);
        OUT_PATH = out_default;
    }
    xorshift_state = fnv1a(SEED_STR);

    N = (long)ceil(TOTAL_SEC * SR);
    L  = (float*)calloc((size_t)N, sizeof(float));
    R  = (float*)calloc((size_t)N, sizeof(float));
    WL = (float*)calloc((size_t)N, sizeof(float));
    WR = (float*)calloc((size_t)N, sizeof(float));
    if (!L || !R || !WL || !WR) { fprintf(stderr, "alloc failed\n"); return 1; }

    const int n_total_chords = (int)floor(TOTAL_SEC / CHORD_DUR_SEC);
    const double rotations = TOTAL_SEC / (CHORD_DUR_SEC * N_CIRCLE);
    report("sleephellsine.c · %.1fs (%.2f min) · SR=%d · seed=%s",
           TOTAL_SEC, TOTAL_SEC / 60.0, SR, SEED_STR);
    report("circle of fifths · 12 roots × %.1fs · %d chord steps · %.2f rotations",
           CHORD_DUR_SEC, n_total_chords, rotations);
    report("uppers · M3 ↔ P5 sinusoidal swing ±%.1f ST · centre voice-led at TC %.0fms",
           UPPER_AMP_ST, 1000.0 / ((double)SR * CENTER_GLIDE));
    report("root bells · low (C2..B2), atk %.2fs, decay τ %.1fs · fire at every crossing",
           ROOT_BELL_ATK, ROOT_BELL_TAU);

    // ── pass 1: render upper voices + brown noise bed ──────────────
    double phA1 = 0, phA2 = 0, phB1 = 0, phB2 = 0;

    // chord-tone centre (glided so adjacent chords voice-lead smoothly)
    double center = chord_center_target(0.0);

    // wavery — random-walk wobble on each carrier
    double waveryA = 0, waveryA_target = 0;
    double waveryB = 0, waveryB_target = 0;
    const long WAVERY_UPDATE_SAMPLES = (long)(WAVERY_UPDATE_MS / 1000.0 * SR);
    long wavery_next = WAVERY_UPDATE_SAMPLES;

    // crossing detection — sign of (voiceA - voiceB)
    double prev_sign = 0.0;       // 0 = not yet set
    // generous upper bound on crossings: 2 per chord (oscillation = 2 zero-crossings per period)
    const long maxCross = n_total_chords * 3 + 32;
    double *cross_t = (double*)malloc(maxCross * sizeof(double));
    double *cross_root = (double*)malloc(maxCross * sizeof(double));
    long n_cross = 0;

    // brownian noise state — independent L/R walks
    double brownL = 0, brownR = 0;

    const double VOICE_GAIN = 0.105;
    const double VOICE_WET = 0.55;

    for (long i = 0; i < N; i++) {
        const double t = (double)i / SR;

        // ── chord progression & sinusoidal contrary motion ─────────
        const double center_tgt = chord_center_target(t);
        center += (center_tgt - center) * CENTER_GLIDE;

        // Phase: 2π per chord = oscillation period equals chord duration.
        // Two crossings per chord (at cos = 0).
        const double phase = TAU * t / CHORD_DUR_SEC;
        const double swing = cos(phase);                       // -1..+1
        const double voiceA_pitch = center + UPPER_AMP_ST * swing;
        const double voiceB_pitch = center - UPPER_AMP_ST * swing;

        // wavery
        if (i >= wavery_next) {
            waveryA_target = (rng() * 2.0 - 1.0) * WAVERY_CENTS;
            waveryB_target = (rng() * 2.0 - 1.0) * WAVERY_CENTS;
            wavery_next += WAVERY_UPDATE_SAMPLES;
        }
        waveryA += (waveryA_target - waveryA) * WAVERY_SMOOTH;
        waveryB += (waveryB_target - waveryB) * WAVERY_SMOOTH;

        // ── crossing detection ─────────────────────────────────────
        const double diff = voiceA_pitch - voiceB_pitch;
        const double cur_sign = (diff >= 0.0) ? 1.0 : -1.0;
        if (prev_sign == 0.0) prev_sign = cur_sign;
        else if (cur_sign != prev_sign) {
            if (n_cross < maxCross) {
                cross_t[n_cross] = t;
                cross_root[n_cross] = root_bell_midi(t);
                n_cross++;
            }
            prev_sign = cur_sign;
        }

        // ── synthesis (detuned pair per voice) ─────────────────────
        const double midiA = voiceA_pitch + waveryA / 100.0;
        const double midiB = voiceB_pitch + waveryB / 100.0;
        const double fA = m2f(midiA);
        const double fB = m2f(midiB);
        const double dc = pow(2.0, DETUNE_CENTS_PAIR * 0.5 / 1200.0);
        phA1 += (fA / dc) / SR; if (phA1 >= 1.0) phA1 -= 1.0;
        phA2 += (fA * dc) / SR; if (phA2 >= 1.0) phA2 -= 1.0;
        phB1 += (fB / dc) / SR; if (phB1 >= 1.0) phB1 -= 1.0;
        phB2 += (fB * dc) / SR; if (phB2 >= 1.0) phB2 -= 1.0;

        // envelope (60 s fade-in, 14 s fade-out)
        double env = 1.0;
        if (t < FADE_IN_VOICE) env = t / FADE_IN_VOICE;
        else if (t > TOTAL_SEC - FADE_OUT) {
            double e = (TOTAL_SEC - t) / FADE_OUT;
            env = e < 0.0 ? 0.0 : e;
        }
        const double g = VOICE_GAIN * env;

        const double sA = (sin(TAU * phA1) + sin(TAU * phA2)) * 0.5;
        const double sB = (sin(TAU * phB1) + sin(TAU * phB2)) * 0.5;

        // Static stereo placement (A slightly left, B slightly right) —
        // the orbital pan from the popcorn model no longer fits.
        const double panA = -0.25, panB = +0.25;
        const double pAL = (panA > 0.0) ? 1.0 - panA : 1.0;
        const double pAR = (panA < 0.0) ? 1.0 + panA : 1.0;
        const double pBL = (panB > 0.0) ? 1.0 - panB : 1.0;
        const double pBR = (panB < 0.0) ? 1.0 + panB : 1.0;

        const double vAL = sA * g * pAL, vAR = sA * g * pAR;
        const double vBL = sB * g * pBL, vBR = sB * g * pBR;
        L[i] += (float)(vAL + vBL);
        R[i] += (float)(vAR + vBR);
        WL[i] += (float)((vAL + vBL) * VOICE_WET);
        WR[i] += (float)((vAR + vBR) * VOICE_WET);

        // ── brownian noise bed (dry, independent L/R walks) ────────
        brownL = brownL * BROWN_LEAK + (rng() * 2.0 - 1.0) * BROWN_DRIVE;
        brownR = brownR * BROWN_LEAK + (rng() * 2.0 - 1.0) * BROWN_DRIVE;
        double brownEnv = 1.0;
        if (t < BROWN_FADE_IN) brownEnv = t / BROWN_FADE_IN;
        else if (t > TOTAL_SEC - FADE_OUT) {
            double e = (TOTAL_SEC - t) / FADE_OUT;
            brownEnv = e < 0.0 ? 0.0 : e;
        }
        const double bGain = BROWN_GAIN * brownEnv;
        L[i] += (float)(brownL * bGain);
        R[i] += (float)(brownR * bGain);
    }
    report("uppers · rendered · %ld crossings detected (≈%.1f per chord)",
           n_cross, (double)n_cross / (double)n_total_chords);
    report("brown · leak=%.3f drive=%.3f gain=%.2f fade-in=%.0fs",
           BROWN_LEAK, BROWN_DRIVE, BROWN_GAIN, BROWN_FADE_IN);

    // ── pass 2: root bells at every crossing ───────────────────────
    // Each crossing triggers a deep root bell (chord root in C2..B2).
    for (long k = 0; k < n_cross; k++) {
        const double t = cross_t[k];
        const double midi = cross_root[k];
        double envScale = 1.0;
        if (t > TOTAL_SEC - FADE_OUT) {
            double e = (TOTAL_SEC - t) / FADE_OUT;
            envScale = e < 0.0 ? 0.0 : e;
        }
        // alternate slight stereo offset per bell
        const double pan_alt = (k % 2 == 0) ? -0.15 : +0.15;
        const BellOpts bo = {
            .pan = pan_alt + hum(0.05),
            .atk = ROOT_BELL_ATK,
            .dec_tau = ROOT_BELL_TAU,
            .wet_send = ROOT_BELL_WET,
            .gain = ROOT_BELL_GAIN * envScale,
        };
        bell_render(t, midi, bo);
    }
    report("root bells · %ld bells rendered (atk %.2fs, τ %.1fs)",
           n_cross, ROOT_BELL_ATK, ROOT_BELL_TAU);

    free(cross_t); free(cross_root);

    // ── reverb: process L and R in parallel ────────────────────────
    report("reverb · Schroeder 4-comb + 2-allpass · FB=%.2f", COMB_FB);
    float *wetL = (float*)calloc((size_t)N, sizeof(float));
    float *wetR = (float*)calloc((size_t)N, sizeof(float));
    if (!wetL || !wetR) { fprintf(stderr, "wet alloc failed\n"); return 1; }
    ReverbJob jobL = { .in = WL, .out = wetL, .combs = COMB_L_D };
    ReverbJob jobR = { .in = WR, .out = wetR, .combs = COMB_R_D };
    pthread_t thL, thR;
    pthread_create(&thL, NULL, reverb_thread, &jobL);
    pthread_create(&thR, NULL, reverb_thread, &jobR);
    pthread_join(thL, NULL);
    pthread_join(thR, NULL);

    const double WET_MIX = 0.50;
    const double revFadeEnd = 8.0;
    for (long i = 0; i < N; i++) {
        const double t = (double)i / SR;
        const double wet = (t < revFadeEnd)
            ? WET_MIX * (t / revFadeEnd)
            : WET_MIX;
        L[i] += (float)(wetL[i] * wet);
        R[i] += (float)(wetR[i] * wet);
    }
    free(wetL); free(wetR);

    // ── normalize (peak → 0.42) ────────────────────────────────────
    double peak = 0.0;
    for (long i = 0; i < N; i++) {
        const double a = fabs(L[i]); const double b = fabs(R[i]);
        if (a > peak) peak = a; if (b > peak) peak = b;
    }
    const double g = peak > 0.0 ? fmin(1.0, 0.42 / peak) : 1.0;
    for (long i = 0; i < N; i++) {
        L[i] *= (float)g;
        R[i] *= (float)g;
    }
    report("normalize · peak %.3f → gain %.3f", peak, g);

    report("write · %s", OUT_PATH);
    write_wav_f32_stereo(OUT_PATH, L, R, N);

    const double wall = now_wall() - t0_wall;
    report("done · %.2f min audio in %.2fs (%.1fx realtime)",
           TOTAL_SEC / 60.0, wall, (TOTAL_SEC / wall));

    free(L); free(R); free(WL); free(WR);
    if (out_default) free(out_default);
    return 0;
}
