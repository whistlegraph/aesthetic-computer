// scratch-audition.c — ear + number harness for the drum-skin friction voice.
//
// The friction is a gesture instrument: it only means anything while a finger
// is moving, so the useful test is not "does it make a sound" but "does the
// sound follow the hand". This renders the gestures that matter and prints the
// numbers that would betray a broken port.
//
// Sits beside pitch-audit.c and surface-audit.c: run it after touching the
// friction voice, the same way pitch-audit is run after touching the synth core.
//
// Build + run (from fedac/native/tools):
//   cc -O2 -I ../src scratch-audition.c ../src/scratch_voice.c -lm \
//      -o /tmp/scratch-audition
//   /tmp/scratch-audition ~/Desktop/scratch
//
// Outputs into the target dir:
//   sweep.wav     — a slow drag from the middle of the head to the metal edge
//   speed.wav     — the same spot rubbed at rising speed (the pitch law)
//   material.wav  — a step through the five materials at one speed
//   release.wav   — rub, then stop dead (does it let go, or hang?)
//   synthetic.wav — the electro surface, for comparison

#include "scratch_voice.h"

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define SR 48000

// ── Minimal stereo 16-bit WAV writer ──
static void wav_write(const char *path, const float *interleaved, long frames) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "can't write %s\n", path); exit(1); }
    uint32_t data_bytes = (uint32_t)(frames * 2 * 2);
    uint32_t riff = 36 + data_bytes;
    uint16_t ch = 2, bits = 16, block = 4, fmt = 1;
    uint32_t sr = SR, byterate = SR * 4, fmtlen = 16;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fmtlen, 4, 1, f); fwrite(&fmt, 2, 1, f);
    fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f); fwrite(&byterate, 4, 1, f);
    fwrite(&block, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&data_bytes, 4, 1, f);
    for (long i = 0; i < frames * 2; i++) {
        double v = interleaved[i];
        if (v > 1.0) v = 1.0;
        if (v < -1.0) v = -1.0;
        int16_t s = (int16_t)lrint(v * 32767.0);
        fwrite(&s, 2, 1, f);
    }
    fclose(f);
}

// The piece's material mapping, mirrored here so the harness auditions what a
// finger would actually produce rather than arbitrary parameter values. Kept
// in step with padScratch* in fedac/native/pieces/notepat.mjs.
static double mixd(double a, double b, double t) { return a + (b - a) * t; }
static double smoothstep(double e0, double e1, double x) {
    double t = (x - e0) / (e1 - e0);
    if (t < 0) t = 0;
    if (t > 1) t = 1;
    return t * t * (3.0 - 2.0 * t);
}

static ScratchParams material_at(double radius, double speed, int synthetic) {
    double toSnare = smoothstep(0.23, 0.31, radius);
    double toRim   = smoothstep(0.40, 0.48, radius);
    double toHat   = smoothstep(0.62, 0.70, radius);
    double toClick = smoothstep(0.88, 0.965, radius);

    double cutoff = mixd(175, 430, toSnare);
    cutoff = mixd(cutoff, 680, toRim);
    cutoff = mixd(cutoff, 1250, toHat);
    cutoff = mixd(cutoff, 2050, toClick);

    double res = mixd(mixd(mixd(mixd(48, 90, toSnare), 185, toRim), 360, toHat), 560, toClick);
    // Speed moves pitch in octaves, not hertz.
    double octaves = speed * 0.82;
    if (octaves > 2.25) octaves = 2.25;
    res *= pow(2.0, octaves);

    double rough = mixd(0.30, 0.78, toSnare);
    rough = mixd(rough, 0.48, toRim);
    rough = mixd(rough, 0.70, toHat);
    rough = mixd(rough, 0.38, toClick);

    double level = speed * 0.052;
    if (level > 0.14) level = 0.14;

    ScratchParams p = {
        .target = level, .cutoff = cutoff, .resonance = res,
        .roughness = rough, .release = 0.010, .pan = 0.0,
        .synthetic = synthetic,
    };
    return p;
}

// Render `seconds` while a callback restates the control params every 5 ms,
// the way a piece would once per frame.
typedef void (*GestureFn)(double t, ScratchParams *p);

static long render_gesture(ScratchVoice *v, float *out, double seconds, GestureFn fn) {
    long frames = (long)(SR * seconds);
    long control_every = SR / 200;   // 5 ms
    for (long i = 0; i < frames; i++) {
        if (i % control_every == 0) {
            ScratchParams p = v->p;
            fn((double)i / SR, &p);
            scratch_voice_set(v, &p);
        }
        double l = 0, r = 0;
        scratch_voice_render(v, SR, &l, &r);
        out[i * 2] = (float)l;
        out[i * 2 + 1] = (float)r;
    }
    return frames;
}

static void stats(const char *name, const float *x, long frames) {
    double peak = 0, rms = 0;
    long nonfinite = 0;
    for (long i = 0; i < frames * 2; i++) {
        if (!isfinite(x[i])) { nonfinite++; continue; }
        double m = fabs(x[i]);
        if (m > peak) peak = m;
        rms += x[i] * x[i];
    }
    rms = sqrt(rms / (frames * 2));
    // Tail level over the last 50 ms — a friction voice that never lets go
    // shows up here and nowhere else.
    double tail = 0;
    long tail_n = SR / 20;
    for (long i = (frames - tail_n) * 2; i < frames * 2; i++) tail += x[i] * x[i];
    tail = sqrt(tail / (tail_n * 2));
    printf("  %-11s peak %.4f  rms %.4f  tail %.6f  nonfinite %ld%s\n",
           name, peak, rms, tail, nonfinite, nonfinite ? "  <-- BAD" : "");
}

// ── Gestures ──
static void g_sweep(double t, ScratchParams *p) {
    *p = material_at(t / 4.0, 0.9, 0);          // centre → edge over 4 s
}
static void g_speed(double t, ScratchParams *p) {
    *p = material_at(0.35, (t / 4.0) * 2.6, 0); // one spot, rising speed
}
static void g_material(double t, ScratchParams *p) {
    static const double stops[5] = { 0.10, 0.36, 0.55, 0.75, 0.94 };
    int i = (int)(t / 1.0);
    if (i > 4) i = 4;
    *p = material_at(stops[i], 1.0, 0);
}
static void g_release(double t, ScratchParams *p) {
    if (t < 1.5) *p = material_at(0.40, 1.2, 0);
    else         p->target = 0.0;               // finger stops dead
}
static void g_synthetic(double t, ScratchParams *p) {
    ScratchParams q = material_at(t / 4.0, 1.1, 1);
    q.cutoff = 1200.0 + 9000.0 * (t / 4.0);
    q.resonance = 1100.0 + q.cutoff * 0.42;
    q.roughness = 0.5;
    q.release = 0.018;
    *p = q;
}

int main(int argc, char **argv) {
    const char *dir = argc > 1 ? argv[1] : "/tmp/scratch-audition";
    mkdir(dir, 0755);

    struct { const char *name; GestureFn fn; double secs; } takes[] = {
        { "sweep",     g_sweep,     4.0 },
        { "speed",     g_speed,     4.0 },
        { "material",  g_material,  5.0 },
        { "release",   g_release,   2.5 },
        { "synthetic", g_synthetic, 4.0 },
    };

    printf("drum-skin friction audition -> %s\n", dir);
    float *buf = malloc(sizeof(float) * SR * 6 * 2);
    int bad = 0;
    for (unsigned i = 0; i < sizeof takes / sizeof *takes; i++) {
        ScratchVoice v;
        scratch_voice_init(&v);
        long n = render_gesture(&v, buf, takes[i].secs, takes[i].fn);
        char path[512];
        snprintf(path, sizeof path, "%s/%s.wav", dir, takes[i].name);
        wav_write(path, buf, n);
        stats(takes[i].name, buf, n);
        // The release take is the one with a pass/fail: after a finger stops,
        // the voice must actually be gone.
        if (!strcmp(takes[i].name, "release")) {
            double tail = 0;
            long tn = SR / 20;
            for (long k = (n - tn) * 2; k < n * 2; k++) tail += buf[k] * buf[k];
            tail = sqrt(tail / (tn * 2));
            if (tail > 1e-4) { printf("    release FAILED: voice still ringing\n"); bad = 1; }
            else printf("    release ok: silent within a second of the finger stopping\n");
        }
    }
    free(buf);
    return bad;
}
