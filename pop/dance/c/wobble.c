// wobble.c — the wobble bass, in C. A faithful port of renderWobble in
// pop/dance/synths/wobble.mjs: a fat detuned Reese swept by a beat-synced
// RESONANT LOWPASS (the dubstep "wub wub").
//
// Numerics mirror the JS reference op-for-op (same xorshift32 RNG stream,
// same Chamberlin SVF, same tanh/crush glue, same envelope window), so
// the C output matches the JS within libm ulps — verify with compare.mjs.
// There is no preset table here: the JS side (emitWobbleScore) pre-
// resolves every parameter and bakes the exact per-event seed into the
// score, so this engine is a dumb, exact replayer.
//
// Build:  ./build.sh
// Run:    ./wobble <score.txt> --raw out.f32      (f32le MONO 48k)
//
// Score format (text, whitespace-separated tokens):
//   sr <int>
//   dur <seconds>
//   note <count>
//   then <count> event lines, each 19 fields:
//     t0 midi durSec gain voices detuneCents lfoHz shape lfoDepth
//     cutLo cutHi q drive crush subGain edge attack decay seed
//   shape: 0 sine | 1 tri | 2 saw | 3 square | 4 sh   seed: uint32

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

typedef struct {
    double t0, midi, durSec, gain;
    int voices;
    double detuneCents, lfoHz;
    int shape;
    double lfoDepth, cutLo, cutHi, q, drive;
    int crush;
    double subGain, edge, attack, decay;
    uint32_t seed;
} Note;

static void die(const char *msg) { fprintf(stderr, "wobble: %s\n", msg); exit(1); }

static double midi_to_freq(double midi) { return 440.0 * pow(2.0, (midi - 69.0) / 12.0); }

// xorshift32 — byte-for-byte the JS makeRngFromSeed stream.
static inline double rng_next(uint32_t *s) {
    uint32_t x = *s;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    *s = x;
    return (double)x / 4294967295.0; // 0xffffffff, matches JS
}

// One unipolar LFO sample in [0,1]; `sh` is the held S&H value.
static inline double lfo_value(int shape, double phase, double sh) {
    switch (shape) {
        case 1: return phase < 0.5 ? phase * 2.0 : 2.0 - phase * 2.0;   // tri
        case 2: return phase;                                          // saw
        case 3: return phase < 0.5 ? 0.0 : 1.0;                        // square
        case 4: return sh;                                             // sh
        case 0:
        default: return 0.5 + 0.5 * sin(2.0 * M_PI * phase);           // sine
    }
}

// Render ONE note additively into the mono output buffer.
static void render_note(const Note *e, float *out, long ns_total, int sr) {
    if (!(e->durSec > 0) || e->gain == 0.0) return;

    long durS = (long)ceil(e->durSec * sr);
    long attS = (long)floor(e->attack * sr); if (attS < 1) attS = 1;
    long decS = (long)floor(e->decay  * sr); if (decS < 1) decS = 1;
    long ns = durS > attS + decS ? durS : attS + decS;
    long decayStart = ns - decS;
    long startIdx = (long)floor(e->t0 * sr);

    double fund = midi_to_freq(e->midi);
    int voices = e->voices < 1 ? 1 : e->voices;
    double edge = e->edge;

    uint32_t rs = e->seed ? e->seed : 1;

    // Per-voice phase + increment (same rng order as the JS engine).
    double *phase = malloc(sizeof(double) * voices);
    double *inc   = malloc(sizeof(double) * voices);
    double half = (voices - 1) / 2.0;
    for (int v = 0; v < voices; v++) {
        phase[v] = rng_next(&rs);
        double offset = half == 0.0 ? 0.0 : (v - half) / half;
        double cents = offset * e->detuneCents;
        inc[v] = (fund * pow(2.0, cents / 1200.0)) / sr;
    }
    double norm = 1.0 / voices;

    double lfoPhase = rng_next(&rs);
    double shVal    = rng_next(&rs);
    double low = 0.0, band = 0.0;
    double damp = 1.0 / (e->q > 0.5 ? e->q : 0.5);
    double cutLo = e->cutLo, cutHi = e->cutHi;
    double ratio = cutHi / cutLo;
    double subInc = (fund * 0.5) / sr;
    double subPhase = rng_next(&rs);
    const double TWO_PI = 2.0 * M_PI;
    double fcMax = (double)sr / 6.0;

    for (long i = 0; i < ns; i++) {
        double env;
        if (i < attS) env = (double)i / attS;
        else if (i < decayStart) env = 1.0;
        else { env = 1.0 - (double)(i - decayStart) / decS; if (env <= 0.0) break; }

        lfoPhase += e->lfoHz / sr;
        if (lfoPhase >= 1.0) { lfoPhase -= 1.0; shVal = rng_next(&rs); }
        double lv = lfo_value(e->shape, lfoPhase, shVal) * e->lfoDepth;

        double src = 0.0;
        for (int v = 0; v < voices; v++) {
            double ph = phase[v] + inc[v];
            if (ph >= 1.0) ph -= floor(ph);
            phase[v] = ph;
            double saw = 2.0 * ph - 1.0;
            double sq  = ph < 0.5 ? 1.0 : -1.0;
            src += saw * (1.0 - edge) + sq * edge;
        }
        src *= norm;

        double fc = cutLo * pow(ratio, lv);
        if (fc < 40.0) fc = 40.0; if (fc > fcMax) fc = fcMax;
        double f = 2.0 * sin(M_PI * fc / sr);
        double high = src - low - damp * band;
        band += f * high;
        low  += f * band;
        double s = low;

        s = tanh(s * e->drive);
        if (e->crush > 0) {
            double steps = pow(2.0, e->crush);
            s = round(s * steps) / steps;
        }

        subPhase += subInc; if (subPhase >= 1.0) subPhase -= 1.0;
        double sub = sin(TWO_PI * subPhase) * e->subGain;

        double mix = s * 0.8 + sub;
        if (!isfinite(mix)) { low = 0.0; band = 0.0; mix = 0.0; }

        long dst = startIdx + i;
        if (dst >= 0 && dst < ns_total) out[dst] += (float)(mix * env * e->gain);
    }

    free(phase); free(inc);
}

int main(int argc, char **argv) {
    const char *scorePath = NULL, *rawPath = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
        else scorePath = argv[i];
    }
    if (!scorePath) die("usage: wobble <score.txt> --raw out.f32");

    FILE *f = fopen(scorePath, "r");
    if (!f) die("cannot open score");

    int sr = 48000;
    double dur = 0;
    Note *notes = NULL; int nNotes = 0;

    char key[64];
    while (fscanf(f, "%63s", key) == 1) {
        if (!strcmp(key, "sr")) { if (fscanf(f, "%d", &sr) != 1) die("bad sr"); }
        else if (!strcmp(key, "dur")) { if (fscanf(f, "%lf", &dur) != 1) die("bad dur"); }
        else if (!strcmp(key, "note")) {
            if (fscanf(f, "%d", &nNotes) != 1) die("bad note count");
            notes = malloc(sizeof(Note) * (nNotes > 0 ? nNotes : 1));
            for (int i = 0; i < nNotes; i++) {
                Note *e = &notes[i];
                if (fscanf(f, "%lf %lf %lf %lf %d %lf %lf %d %lf %lf %lf %lf %lf %d %lf %lf %lf %lf %u",
                           &e->t0, &e->midi, &e->durSec, &e->gain, &e->voices,
                           &e->detuneCents, &e->lfoHz, &e->shape, &e->lfoDepth,
                           &e->cutLo, &e->cutHi, &e->q, &e->drive, &e->crush,
                           &e->subGain, &e->edge, &e->attack, &e->decay, &e->seed) != 19)
                    die("bad note line");
            }
        } else die("unknown score key");
    }
    fclose(f);
    if (dur <= 0) die("empty score (dur <= 0)");

    long ns = (long)ceil(dur * sr);
    float *out = calloc(ns, sizeof(float));
    if (!out) die("oom");

    for (int i = 0; i < nNotes; i++) render_note(&notes[i], out, ns, sr);

    FILE *o = rawPath ? fopen(rawPath, "wb") : stdout;
    if (!o) die("cannot open output");
    fwrite(out, sizeof(float), ns, o);
    if (rawPath) fclose(o);
    fprintf(stderr, "wobble: %d notes, %.1f s -> %s\n", nNotes, (double)ns / sr,
            rawPath ? rawPath : "stdout");

    free(out); free(notes);
    return 0;
}
