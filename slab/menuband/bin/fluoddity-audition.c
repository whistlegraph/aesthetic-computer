// fluoddity-audition.c — ear + number harness for the Fluoddity voice.
//
// Renders a set of WAVs that answer the musical questions about the port
// (does a seed have a personality? does mutation make a lineage? does a held
// note stay alive?) and prints per-segment stats: peak, RMS, non-finite
// count, and a two-detector pitch estimate (normalized autocorrelation vs
// spectrum-free HPS-lite via autocorr of the autocorr) so octave jumps in a
// single detector don't read as failures.
//
// Build + run:
//   cc -O2 -I Sources/CFluoddity/include bin/fluoddity-audition.c \
//      Sources/CFluoddity/fluoddity_voice.c -lm -o /tmp/fluod-audition
//   /tmp/fluod-audition ~/Desktop/fluoddity-voice
//
// Outputs into the target dir:
//   seeds.wav    — one note (A3) across 10 seeds, 2.2 s each
//   lineage.wav  — seed 7 mutated progressively (the "evolve it" gesture)
//   drone.wav    — one seed held 20 s (the living-sustain test)
//   phrase.wav   — a little melody then a held triad (polyphony test)

#include "../Sources/CFluoddity/include/fluoddity_voice.h"

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define SR 48000

// ── Minimal mono 16-bit WAV writer ──

static void wav_write(const char *path, const float *buf, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "can't write %s\n", path); exit(1); }
    uint32_t data_bytes = (uint32_t)(n * 2);
    uint32_t riff = 36 + data_bytes;
    uint16_t ch = 1, bits = 16, block = 2;
    uint32_t sr = SR, byterate = SR * 2;
    uint16_t fmt = 1;
    uint32_t fmtlen = 16;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fmtlen, 4, 1, f); fwrite(&fmt, 2, 1, f);
    fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f); fwrite(&byterate, 4, 1, f);
    fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&data_bytes, 4, 1, f);
    for (long i = 0; i < n; i++) {
        float s = buf[i];
        if (s > 1.0f) s = 1.0f;
        if (s < -1.0f) s = -1.0f;
        int16_t q = (int16_t)lrintf(s * 32767.0f);
        fwrite(&q, 2, 1, f);
    }
    fclose(f);
}

// ── Stats + pitch ──

static double detect_pitch_autocorr(const float *buf, long n) {
    // Normalized autocorrelation over the last second of the segment.
    long start = n > SR ? n - SR : 0;
    const float *x = buf + start;
    long len = n - start;
    int min_lag = SR / 2000, max_lag = SR / 40;
    if (len < max_lag * 3) return 0;
    double best = 0; int best_lag = 0;
    for (int lag = min_lag; lag <= max_lag; lag++) {
        double num = 0, d1 = 0, d2 = 0;
        for (long i = 0; i + lag < len; i++) {
            num += x[i] * x[i + lag];
            d1 += x[i] * x[i];
            d2 += x[i + lag] * x[i + lag];
        }
        double r = num / (sqrt(d1 * d2) + 1e-12);
        if (r > best) { best = r; best_lag = lag; }
    }
    if (best < 0.3 || best_lag == 0) return 0;
    // Refine: walk down to the SMALLEST lag whose correlation is within 95%
    // of the peak — undoes the classic pick-the-2nd-period octave error.
    for (int lag = min_lag; lag < best_lag; lag++) {
        double num = 0, d1 = 0, d2 = 0;
        for (long i = 0; i + lag < len; i++) {
            num += x[i] * x[i + lag];
            d1 += x[i] * x[i];
            d2 += x[i + lag] * x[i + lag];
        }
        if (num / (sqrt(d1 * d2) + 1e-12) > best * 0.95) { best_lag = lag; break; }
    }
    return (double)SR / best_lag;
}

static void report(const char *label, const float *buf, long n, double want_hz) {
    double peak = 0, sum2 = 0; long bad = 0;
    for (long i = 0; i < n; i++) {
        if (!isfinite(buf[i])) { bad++; continue; }
        double a = fabs(buf[i]);
        if (a > peak) peak = a;
        sum2 += (double)buf[i] * buf[i];
    }
    double rms = sqrt(sum2 / (double)(n ? n : 1));
    double hz = detect_pitch_autocorr(buf, n);
    double cents = (hz > 0 && want_hz > 0) ? 1200.0 * log2(hz / want_hz) : 0;
    // Report both absolute cents and folded-to-octave cents; harmonic
    // octave ambiguity is timbre, not detune.
    double folded = fmod(fmod(cents, 1200.0) + 1800.0, 1200.0) - 600.0;
    printf("%-28s peak %.3f  rms %.3f  nonfinite %ld  pitch %7.1f Hz"
           " (want %6.1f, %+7.0f c, folded %+5.0f c)\n",
           label, peak, rms, bad, hz, want_hz, cents, folded);
}

// ── Note rendering ──

static FluodVoice g_voice; // static: FluodVoice is big; keep off the stack

static void render_note(float *out, long n, uint32_t seed, double hz,
                        const FluodRule *rule, long release_at) {
    if (rule) fluod_voice_init_rule(&g_voice, rule, seed, hz, SR);
    else fluod_voice_init(&g_voice, seed, hz, SR);
    double env = 0;
    for (long i = 0; i < n; i++) {
        double target = (release_at > 0 && i >= release_at) ? 0.0 : 1.0;
        // ~6 ms attack, ~90 ms release
        double k = target > env ? 1.0 / (0.006 * SR) : 1.0 / (0.09 * SR);
        env += (target - env) * fmin(1.0, k * 3.0);
        out[i] += fluod_voice_render(&g_voice, SR, env, hz);
    }
}

static double midi_hz(int m) { return 440.0 * pow(2.0, (m - 69) / 12.0); }

int main(int argc, char **argv) {
    const char *dir = argc > 1 ? argv[1] : "/tmp/fluoddity-voice";
    mkdir(dir, 0755);
    char path[1024];
    const double A3 = midi_hz(57);

    // 1) Seed gallery
    {
        long seg = (long)(2.2 * SR), gap = SR / 5;
        long total = 10 * (seg + gap);
        float *buf = calloc(total, sizeof(float));
        printf("== seeds.wav — A3 across 10 seeds ==\n");
        for (int s = 0; s < 10; s++) {
            long off = s * (seg + gap);
            render_note(buf + off, seg, 100 + s, A3, NULL, seg - SR / 6);
            char lbl[64]; snprintf(lbl, sizeof lbl, "seed %d", 100 + s);
            report(lbl, buf + off, seg, A3);
        }
        snprintf(path, sizeof path, "%s/seeds.wav", dir);
        wav_write(path, buf, total); free(buf);
    }

    // 2) Mutation lineage: one genome, mutated cumulatively
    {
        long seg = (long)(2.2 * SR), gap = SR / 5;
        float amounts[] = {0.0f, 0.1f, 0.2f, 0.4f, 0.8f};
        int steps = 5;
        long total = steps * (seg + gap);
        float *buf = calloc(total, sizeof(float));
        FluodRule r; fluod_rule_from_seed(&r, 107);
        printf("== lineage.wav — seed 107 mutated ==\n");
        for (int s = 0; s < steps; s++) {
            if (s > 0) fluod_rule_mutate(&r, amounts[s], 555 + s);
            long off = s * (seg + gap);
            render_note(buf + off, seg, 42, A3, &r, seg - SR / 6);
            char lbl[64]; snprintf(lbl, sizeof lbl, "mutation +%.1f", amounts[s]);
            report(lbl, buf + off, seg, A3);
        }
        snprintf(path, sizeof path, "%s/lineage.wav", dir);
        wav_write(path, buf, total); free(buf);
    }

    // 3) Drone: does a held note stay alive without dying or blowing up?
    {
        long n = 20 * SR;
        float *buf = calloc(n, sizeof(float));
        printf("== drone.wav — seed 104, A2 held 20 s ==\n");
        render_note(buf, n, 104, midi_hz(45), NULL, n - SR / 2);
        report("drone 0-5s", buf, 5 * SR, midi_hz(45));
        report("drone 15-20s", buf + 15 * SR, 5 * SR, midi_hz(45));
        snprintf(path, sizeof path, "%s/drone.wav", dir);
        wav_write(path, buf, n); free(buf);
    }

    // 4) Phrase: melody then a held triad, all on one seed
    {
        int mel[] = {57, 60, 64, 62, 57, 55, 57};
        long nl = (long)(0.45 * SR);
        long mel_len = 7 * nl;
        long chord_len = 4 * SR;
        long total = mel_len + chord_len + SR / 2;
        float *buf = calloc(total, sizeof(float));
        printf("== phrase.wav — melody + triad, seed 103 ==\n");
        for (int i = 0; i < 7; i++)
            render_note(buf + i * nl, nl + SR / 8, 103, midi_hz(mel[i]),
                        NULL, nl - SR / 20);
        int triad[] = {53, 57, 60};
        for (int i = 0; i < 3; i++)
            render_note(buf + mel_len, chord_len, 103, midi_hz(triad[i]),
                        NULL, chord_len - SR / 3);
        for (long i = 0; i < total; i++) buf[i] *= 0.4f; // headroom for the sum
        report("phrase tail (triad)", buf + mel_len, chord_len, 0);
        snprintf(path, sizeof path, "%s/phrase.wav", dir);
        wav_write(path, buf, total); free(buf);
    }

    printf("wrote WAVs to %s\n", dir);
    return 0;
}
