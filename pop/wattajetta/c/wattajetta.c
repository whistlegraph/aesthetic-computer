// wattajetta.c — the water engine. Everything in this track is made of
// water: a fighter jet, its canopy, the control stick, the spray coming
// off the wingtips. So everything in this file is made of exactly two
// materials and nothing else:
//
//   SINE  — water holding a shape. Phase-increment oscillators only
//           (never sin(TAU*f*t) — long tracks drift). Kicks, sub bass,
//           doppler flybys, droplet pings: all the same material bent
//           into different vessels.
//   SPRAY — water losing its shape. White noise carved by a bandpass.
//           Wingtip streams, cloud punches, afterburner steam.
//
// No samples, no other waveforms, no exceptions.
//
// Build:  ./build.sh
// Run:    ./wattajetta <score.txt> --raw out.f32   (f32le stereo 48k)
//
// Score format (text, one item per line):
//   sr / dur / normpeak / fadein / fadeout            — globals
//   sidechain <atkSec> <relSec> <depthDb>             — kick ducks the rest
//   kick <n> then per event:
//     t0 f0 f1 sweepSec ampDb holeSec decaySec
//   sine <n> then per event:
//     t0 dur f0 f1 ampDb atk rel pan0 pan1 vibHz vibCents
//   noise <n> then per event:
//     t0 dur f0 f1 q peakDb atk rel pan
//   Events within a voice list may be in any order; each renders into
//   its own sample range independently.

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#define TAU (2.0 * M_PI)
#define BLOCK 64

typedef struct { double t0, f0, f1, sweep, ampDb, hole, decay; } Kick;
typedef struct { double t0, dur, f0, f1, ampDb, atk, rel, pan0, pan1, vibHz, vibCents; } Sine;
typedef struct { double t0, dur, f0, f1, q, peakDb, atk, rel, pan; } Noise;

static void die(const char *msg) { fprintf(stderr, "wattajetta: %s\n", msg); exit(1); }

static inline double db2lin(double db) { return pow(10.0, db / 20.0); }

// equal-power pan: pan in [-1, 1] -> left/right gains
static inline void pan_gains(double pan, double *gl, double *gr) {
    double a = (pan + 1.0) * 0.25 * M_PI;
    *gl = cos(a);
    *gr = sin(a);
}

// linear attack / sustain / linear release across [0, dur]
static inline double asr_env(double dt, double dur, double atk, double rel) {
    if (dt < 0 || dt >= dur) return 0;
    double v = 1.0;
    if (atk > 0 && dt < atk) v = dt / atk;
    double tail = dur - dt;
    if (rel > 0 && tail < rel) { double r = tail / rel; if (r < v) v = r; }
    return v;
}

int main(int argc, char **argv) {
    const char *scorePath = NULL, *rawPath = NULL, *kickPath = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
        else if (!strcmp(argv[i], "--kickraw") && i + 1 < argc) kickPath = argv[++i];
        else scorePath = argv[i];
    }
    if (!scorePath) die("usage: wattajetta <score.txt> [--raw out.f32] [--kickraw kick.f32]");

    // ── parse the score ───────────────────────────────────────────────
    FILE *f = fopen(scorePath, "r");
    if (!f) die("cannot open score");
    int sr = 48000;
    double dur = 0, normpeak = 0.9, fadein = 0.004, fadeout = 2.0;
    double scAtk = 0.015, scRel = 0.18, scDepthDb = 0; // 0 = no sidechain
    Kick *kicks = NULL;  int nK = 0;
    Sine *sines = NULL;  int nS = 0;
    Noise *noises = NULL; int nN = 0;

    char key[64];
    while (fscanf(f, "%63s", key) == 1) {
        if (!strcmp(key, "sr")) { if (fscanf(f, "%d", &sr) != 1) die("bad sr"); }
        else if (!strcmp(key, "dur")) { if (fscanf(f, "%lf", &dur) != 1) die("bad dur"); }
        else if (!strcmp(key, "normpeak")) { if (fscanf(f, "%lf", &normpeak) != 1) die("bad normpeak"); }
        else if (!strcmp(key, "fadein")) { if (fscanf(f, "%lf", &fadein) != 1) die("bad fadein"); }
        else if (!strcmp(key, "fadeout")) { if (fscanf(f, "%lf", &fadeout) != 1) die("bad fadeout"); }
        else if (!strcmp(key, "sidechain")) {
            if (fscanf(f, "%lf %lf %lf", &scAtk, &scRel, &scDepthDb) != 3) die("bad sidechain");
        }
        else if (!strcmp(key, "kick")) {
            if (fscanf(f, "%d", &nK) != 1) die("bad kick count");
            kicks = malloc(sizeof(Kick) * nK);
            for (int i = 0; i < nK; i++) {
                Kick *k = &kicks[i];
                if (fscanf(f, "%lf %lf %lf %lf %lf %lf %lf",
                           &k->t0, &k->f0, &k->f1, &k->sweep, &k->ampDb, &k->hole, &k->decay) != 7)
                    die("bad kick event");
            }
        }
        else if (!strcmp(key, "sine")) {
            if (fscanf(f, "%d", &nS) != 1) die("bad sine count");
            sines = malloc(sizeof(Sine) * nS);
            for (int i = 0; i < nS; i++) {
                Sine *s = &sines[i];
                if (fscanf(f, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
                           &s->t0, &s->dur, &s->f0, &s->f1, &s->ampDb, &s->atk, &s->rel,
                           &s->pan0, &s->pan1, &s->vibHz, &s->vibCents) != 11)
                    die("bad sine event");
            }
        }
        else if (!strcmp(key, "noise")) {
            if (fscanf(f, "%d", &nN) != 1) die("bad noise count");
            noises = malloc(sizeof(Noise) * nN);
            for (int i = 0; i < nN; i++) {
                Noise *n = &noises[i];
                if (fscanf(f, "%lf %lf %lf %lf %lf %lf %lf %lf %lf",
                           &n->t0, &n->dur, &n->f0, &n->f1, &n->q, &n->peakDb,
                           &n->atk, &n->rel, &n->pan) != 9)
                    die("bad noise event");
            }
        }
        else die("unknown score key");
    }
    fclose(f);
    if (dur <= 0) die("empty score");
    if (nK + nS + nN == 0) die("no events");

    long ns = (long)ceil(dur * sr);
    // two busses so the kick can duck everything that isn't the kick
    float *kickBus[2], *mainBus[2];
    for (int ch = 0; ch < 2; ch++) {
        kickBus[ch] = calloc(ns, sizeof(float));
        mainBus[ch] = calloc(ns, sizeof(float));
    }
    double dt = 1.0 / sr;

    // ── kicks: pitch-swept sine, hole attack, exponential decay ───────
    // The hole (a short linear ramp from zero) keeps the transient from
    // clicking and leaves a gap the sub breathes through.
    for (int i = 0; i < nK; i++) {
        Kick *k = &kicks[i];
        double amp = db2lin(k->ampDb);
        double tail = k->hole + k->decay * 1.5;
        long a = (long)(k->t0 * sr), z = (long)((k->t0 + tail) * sr);
        if (a < 0) a = 0; if (z > ns) z = ns;
        double phase = 0;
        for (long j = a; j < z; j++) {
            double t = (double)(j - a) / sr;
            double p = k->sweep > 0 ? t / k->sweep : 1.0;
            if (p > 1) p = 1;
            double freq = k->f0 * pow(k->f1 / k->f0, p);
            phase += freq * dt;
            double env = (t < k->hole) ? t / k->hole
                                       : exp(-(t - k->hole) * 6.9 / k->decay);
            float v = (float)(sin(TAU * phase) * env * amp);
            kickBus[0][j] += v;
            kickBus[1][j] += v;
        }
    }

    // ── sines: the water itself — glide, vibrato, pan sweep ───────────
    for (int i = 0; i < nS; i++) {
        Sine *s = &sines[i];
        double amp = db2lin(s->ampDb);
        long a = (long)(s->t0 * sr), z = (long)((s->t0 + s->dur) * sr);
        if (a < 0) a = 0; if (z > ns) z = ns;
        double phase = 0;
        for (long j = a; j < z; j++) {
            double t = (double)(j - a) / sr;
            double p = t / s->dur;
            double freq = s->f0 * pow(s->f1 / s->f0, p);
            if (s->vibCents > 0)
                freq *= pow(2.0, s->vibCents * sin(TAU * s->vibHz * t) / 1200.0);
            phase += freq * dt;
            double env = asr_env(t, s->dur, s->atk, s->rel);
            double pan = s->pan0 + (s->pan1 - s->pan0) * p;
            double gl, gr;
            pan_gains(pan, &gl, &gr);
            double v = sin(TAU * phase) * env * amp;
            mainBus[0][j] += (float)(v * gl);
            mainBus[1][j] += (float)(v * gr);
        }
    }

    // ── spray: white noise through an RBJ bandpass, freq gliding ──────
    // Constant-skirt-gain bandpass (peak = Q) normalized back down so
    // peakDb means what it says. Coefficients update per 64-sample block.
    for (int i = 0; i < nN; i++) {
        Noise *n = &noises[i];
        double amp = db2lin(n->peakDb);
        long a = (long)(n->t0 * sr), z = (long)((n->t0 + n->dur) * sr);
        if (a < 0) a = 0; if (z > ns) z = ns;
        uint32_t seed = 0x57A77E7 + (uint32_t)i * 2654435761u;
        double z1 = 0, z2 = 0, gl, gr;
        pan_gains(n->pan, &gl, &gr);
        double b0c = 0, b1c = 0, b2c = 0, a1c = 0, a2c = 0;
        for (long j = a; j < z; j++) {
            double t = (double)(j - a) / sr;
            if ((j - a) % BLOCK == 0) {
                double p = t / n->dur;
                double freq = n->f0 * pow(n->f1 / n->f0, p);
                double fmax = sr * 0.45;
                if (freq > fmax) freq = fmax;
                double w0 = TAU * freq / sr;
                double alpha = sin(w0) / (2.0 * n->q);
                double a0 = 1.0 + alpha;
                b0c = alpha / a0; b1c = 0.0; b2c = -alpha / a0;
                a1c = (-2.0 * cos(w0)) / a0;
                a2c = (1.0 - alpha) / a0;
            }
            seed = seed * 1664525u + 1013904223u;
            double x = (((double)seed / 4294967296.0) * 2.0 - 1.0) * 2.4; // ≈ unity band level
            double y = b0c * x + z1;
            z1 = b1c * x - a1c * y + z2;
            z2 = b2c * x - a2c * y;
            double env = asr_env(t, n->dur, n->atk, n->rel);
            double v = y * env * amp;
            mainBus[0][j] += (float)(v * gl);
            mainBus[1][j] += (float)(v * gr);
        }
    }

    // ── sidechain: every kick pushes the water aside, then it rushes
    //    back in — duck the main bus, never the kick bus ───────────────
    if (scDepthDb < 0 && nK > 0) {
        double depth = db2lin(scDepthDb);
        // kicks arrive in score order; sort by t0 so the scan below works
        for (int i = 1; i < nK; i++)
            for (int j = i; j > 0 && kicks[j].t0 < kicks[j - 1].t0; j--) {
                Kick tmp = kicks[j]; kicks[j] = kicks[j - 1]; kicks[j - 1] = tmp;
            }
        int cur = 0;
        for (long j = 0; j < ns; j++) {
            double t = (double)j / sr;
            while (cur + 1 < nK && kicks[cur + 1].t0 <= t) cur++;
            double g = 1.0;
            if (kicks[cur].t0 <= t) {
                double d = t - kicks[cur].t0;
                if (d < scAtk) g = 1.0 + (depth - 1.0) * (d / scAtk);
                else if (d < scAtk + scRel) g = depth + (1.0 - depth) * ((d - scAtk) / scRel);
            }
            mainBus[0][j] *= (float)g;
            mainBus[1][j] *= (float)g;
        }
    }

    // ── normalize + edge fades. The gain comes from the SUMMED peak so
    //    kick/main balance survives a split; with --kickraw the busses
    //    are written separately (kick stays pristine through the JS
    //    scratch layer and is summed back on top there) ────────────────
    float *out[2] = { mainBus[0], mainBus[1] };
    double peak = 0;
    for (int ch = 0; ch < 2; ch++)
        for (long i = 0; i < ns; i++) {
            double sum = (double)out[ch][i] + kickBus[ch][i];
            if (!kickPath) out[ch][i] = (float)sum;
            if (fabs(sum) > peak) peak = fabs(sum);
        }
    double g = peak > 0 ? normpeak / peak : 1.0;
    long fi = (long)(fadein * sr), fo = (long)(fadeout * sr);
    for (int ch = 0; ch < 2; ch++) {
        for (long i = 0; i < ns; i++) { out[ch][i] = (float)(out[ch][i] * g); kickBus[ch][i] = (float)(kickBus[ch][i] * g); }
        for (long i = 0; i < fi && i < ns; i++) { out[ch][i] *= (float)i / fi; kickBus[ch][i] *= (float)i / fi; }
        for (long i = 0; i < fo && i < ns; i++) { out[ch][ns - 1 - i] *= (float)i / fo; kickBus[ch][ns - 1 - i] *= (float)i / fo; }
    }

    FILE *o = rawPath ? fopen(rawPath, "wb") : stdout;
    if (!o) die("cannot open output");
    float *inter = malloc(sizeof(float) * 2 * ns);
    for (long i = 0; i < ns; i++) { inter[2 * i] = out[0][i]; inter[2 * i + 1] = out[1][i]; }
    fwrite(inter, sizeof(float), 2 * ns, o);
    if (rawPath) fclose(o);
    if (kickPath) {
        FILE *ko = fopen(kickPath, "wb");
        if (!ko) die("cannot open kick output");
        for (long i = 0; i < ns; i++) { inter[2 * i] = kickBus[0][i]; inter[2 * i + 1] = kickBus[1][i]; }
        fwrite(inter, sizeof(float), 2 * ns, ko);
        fclose(ko);
    }
    fprintf(stderr, "wattajetta: %d kicks, %d sines, %d sprays, %.1f s -> %s\n",
            nK, nS, nN, (double)ns / sr, rawPath ? rawPath : "stdout");
    return 0;
}
