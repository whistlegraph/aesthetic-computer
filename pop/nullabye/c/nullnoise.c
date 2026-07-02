// nullnoise.c — the cancelled-noise engine, in C. Generic: renders any
// baked score (see --bake in the lane renderers), so nuellaby, teknull
// and any future carved-noise track share this one binary.
//
// THE LAW OF THIS LANE: two copies of pink noise, one phase-inverted —
// perfect silence — and every sound is a peaking EQ bell breaking the
// cancellation. residue = chain(noise) − noise. There is no oscillator
// anywhere in this file; a flat chain renders bit-exact digital zero
// (verify with --proof).
//
// Numerics mirror the JS reference (render-nuellaby.mjs) op for op:
// float (f32) wet/dry/out buffers, double biquad state and coefficients,
// the same LCG → Paul Kellett pink filter, the same RBJ peaking bells,
// per-64-sample-block coefficient updates, the same per-window loudness
// ride. Differences vs JS come only from libm ulps.
//
// Build:  ./build.sh
// Run:    ./nullnoise <score.txt> --raw out.f32     (f32le stereo 48k)
//         ./nullnoise <score.txt> --proof           (flat ⇒ silence check)
//
// Score format (text, one item per line):
//   sr / dur / detuneL / detuneR / seedL / seedR — globals
//   normpeak / fadein / fadeout                   — finalize params
//   ridewin <len> <count> then <count> target-dB lines — loudness ride
//   group <seedL> <seedR> — start a new parallel noise pair
//   noisetype pink|white|brown|velvet — noise material for the CURRENT
//     group (default pink). Each type is roughly RMS-matched to pink so
//     peakDb means the same thing across groups: brown = leaky-integrated
//     white (dark — carve lows from it), white = raw LCG (crisp tops),
//     velvet = sparse ±1 impulses on a 16-sample grid (smooth, hiss-free
//     pads). A flat chain still nulls to bit-exact zero for every type.
//   band <eventCount> then per-event lines:
//     t0 freq freqEnd(0=none) peakDb q atk hold rel cut(-1=none)
//   bands appear in chain order; events per band are time-sorted.

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#define BLOCK 64

typedef struct {
    double t0, freq, freqEnd, peakDb, q, atk, hold, rel, end, cut;
    int hasGlide, hasCut;
} Event;

typedef struct {
    Event *ev;
    int n;
    int cursor;
    int group; // which noise pair this band's chain runs on
    double z1, z2;
} Band;

static void die(const char *msg) { fprintf(stderr, "nullnoise: %s\n", msg); exit(1); }

// ── pink noise: LCG → Paul Kellett filter, byte-for-byte the JS one ───
typedef struct {
    uint32_t s;
    double b0, b1, b2, b3, b4, b5, b6;
} Pink;

static void pink_init(Pink *p, uint32_t seed) { memset(p, 0, sizeof *p); p->s = seed; }

static inline double pink_next(Pink *p) {
    p->s = p->s * 1664525u + 1013904223u;
    double w = ((double)p->s / 4294967296.0) * 2.0 - 1.0;
    p->b0 = 0.99886 * p->b0 + w * 0.0555179;
    p->b1 = 0.99332 * p->b1 + w * 0.0750759;
    p->b2 = 0.969   * p->b2 + w * 0.153852;
    p->b3 = 0.8665  * p->b3 + w * 0.3104856;
    p->b4 = 0.55    * p->b4 + w * 0.5329522;
    p->b5 = -0.7616 * p->b5 - w * 0.016898;
    double out = (p->b0 + p->b1 + p->b2 + p->b3 + p->b4 + p->b5 + p->b6 + w * 0.5362) * 0.11;
    p->b6 = w * 0.115926;
    return out;
}

// ── noise generator: one of four materials per group ──────────────────
// Scales chosen so each type's RMS lands near pink's (~0.15), so a
// band's peakDb carves a similar loudness whatever it's carved from.
enum { NT_PINK = 0, NT_WHITE, NT_BROWN, NT_VELVET };

typedef struct {
    int type;
    Pink pink;      // pink (also the LCG for the other types)
    double brown;   // leaky integrator state
    int vPos;       // velvet: sample index within the current grid cell
    int vAt;        // velvet: impulse offset within the cell
    double vSign;   // velvet: impulse polarity
} Gen;

#define VELVET_CELL 16

static inline double lcg_next(Gen *g) {
    g->pink.s = g->pink.s * 1664525u + 1013904223u;
    return ((double)g->pink.s / 4294967296.0) * 2.0 - 1.0;
}

static void gen_init(Gen *g, int type, uint32_t seed) {
    memset(g, 0, sizeof *g);
    g->type = type;
    pink_init(&g->pink, seed);
    g->vPos = VELVET_CELL; // force a fresh cell draw on first sample
}

static inline double gen_next(Gen *g) {
    switch (g->type) {
    case NT_WHITE:
        return lcg_next(g) * 0.26;
    case NT_BROWN:
        g->brown = g->brown * 0.998 + lcg_next(g) * 0.02;
        return g->brown;
    case NT_VELVET: {
        if (g->vPos >= VELVET_CELL) {
            double r = lcg_next(g); // one draw: position + sign
            g->vAt = (int)((r * 0.5 + 0.5) * VELVET_CELL) % VELVET_CELL;
            g->vSign = (g->pink.s & 1u) ? 0.6 : -0.6;
            g->vPos = 0;
        }
        double v = (g->vPos == g->vAt) ? g->vSign : 0.0;
        g->vPos++;
        return v;
    }
    default:
        return pink_next(&g->pink);
    }
}

// envelope in dB-space: linear attack / hold / release, optional cut fade
static inline double env_of(const Event *e, double time) {
    double dt = time - e->t0;
    if (dt < 0 || dt >= e->atk + e->hold + e->rel) return 0;
    double v;
    if (dt < e->atk) v = dt / e->atk;
    else if (dt < e->atk + e->hold) v = 1;
    else v = 1 - (dt - e->atk - e->hold) / e->rel;
    if (e->hasCut) {
        double f = (e->cut - time) / 0.03;
        if (f < 0) f = 0; if (f > 1) f = 1;
        v *= f;
    }
    return v;
}

int main(int argc, char **argv) {
    const char *scorePath = NULL, *rawPath = NULL;
    int proof = 0;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
        else if (!strcmp(argv[i], "--proof")) proof = 1;
        else scorePath = argv[i];
    }
    if (!scorePath) die("usage: nullnoise <score.txt> [--raw out.f32] [--proof]");

    // ── parse the score ───────────────────────────────────────────────
    FILE *f = fopen(scorePath, "r");
    if (!f) die("cannot open score");
    int sr = 48000;
    double dur = 0, detune[2] = { 1.0, 1.0 };
    // groups: independent noise pairs, each with its own serial chain.
    // residues sum in PARALLEL — a second group can reuse frequencies the
    // first already boosts without the serial dB-addition blowup.
    enum { MAXG = 8 };
    uint32_t gseed[MAXG][2] = { { 0xBEEF, 0xC0FFEE } };
    int gtype[MAXG] = { NT_PINK };
    int nGroups = 1, curGroup = 0;
    double normpeak = 0.88, fadein = 0.004, fadeout = 1.6;
    double rideLen = 0; int rideN = 0; double *rideTarget = NULL;
    Band *bands = NULL; int nBands = 0, capBands = 0;

    char key[64];
    while (fscanf(f, "%63s", key) == 1) {
        if (!strcmp(key, "sr")) { if (fscanf(f, "%d", &sr) != 1) die("bad sr"); }
        else if (!strcmp(key, "dur")) { if (fscanf(f, "%lf", &dur) != 1) die("bad dur"); }
        else if (!strcmp(key, "detune")) { if (fscanf(f, "%lf %lf", &detune[0], &detune[1]) != 2) die("bad detune"); }
        else if (!strcmp(key, "seed")) { if (fscanf(f, "%u %u", &gseed[0][0], &gseed[0][1]) != 2) die("bad seed"); }
        else if (!strcmp(key, "group")) {
            if (nGroups >= MAXG) die("too many groups");
            if (fscanf(f, "%u %u", &gseed[nGroups][0], &gseed[nGroups][1]) != 2) die("bad group");
            gtype[nGroups] = NT_PINK;
            curGroup = nGroups++;
        }
        else if (!strcmp(key, "noisetype")) {
            char nt[16];
            if (fscanf(f, "%15s", nt) != 1) die("bad noisetype");
            if (!strcmp(nt, "pink")) gtype[curGroup] = NT_PINK;
            else if (!strcmp(nt, "white")) gtype[curGroup] = NT_WHITE;
            else if (!strcmp(nt, "brown")) gtype[curGroup] = NT_BROWN;
            else if (!strcmp(nt, "velvet")) gtype[curGroup] = NT_VELVET;
            else die("unknown noisetype");
        }
        else if (!strcmp(key, "normpeak")) { if (fscanf(f, "%lf", &normpeak) != 1) die("bad normpeak"); }
        else if (!strcmp(key, "fadein")) { if (fscanf(f, "%lf", &fadein) != 1) die("bad fadein"); }
        else if (!strcmp(key, "fadeout")) { if (fscanf(f, "%lf", &fadeout) != 1) die("bad fadeout"); }
        else if (!strcmp(key, "ridewin")) {
            if (fscanf(f, "%lf %d", &rideLen, &rideN) != 2) die("bad ridewin");
            rideTarget = malloc(sizeof(double) * rideN);
            for (int i = 0; i < rideN; i++)
                if (fscanf(f, "%lf", &rideTarget[i]) != 1) die("bad ride target");
        } else if (!strcmp(key, "band")) {
            int n; if (fscanf(f, "%d", &n) != 1) die("bad band");
            if (nBands == capBands) {
                capBands = capBands ? capBands * 2 : 32;
                bands = realloc(bands, sizeof(Band) * capBands);
            }
            Band *b = &bands[nBands++];
            memset(b, 0, sizeof *b);
            b->ev = malloc(sizeof(Event) * n);
            b->n = n;
            b->group = curGroup;
            for (int i = 0; i < n; i++) {
                Event *e = &b->ev[i];
                if (fscanf(f, "%lf %lf %lf %lf %lf %lf %lf %lf %lf",
                           &e->t0, &e->freq, &e->freqEnd, &e->peakDb, &e->q,
                           &e->atk, &e->hold, &e->rel, &e->cut) != 9)
                    die("bad event");
                e->hasGlide = e->freqEnd > 0;
                e->hasCut = e->cut >= 0;
                e->end = e->hasCut ? e->cut : e->t0 + e->atk + e->hold + e->rel;
                if (proof) e->peakDb = 0;
            }
        } else die("unknown score key");
    }
    fclose(f);
    if (dur <= 0 || nBands == 0) die("empty score");

    long ns = (long)ceil(dur * sr);
    float *out[2];
    out[0] = calloc(ns, sizeof(float));
    out[1] = calloc(ns, sizeof(float));
    float dry[BLOCK], wet[BLOCK], acc[BLOCK];

    // ── render: per channel, per group: EQ'd copy minus the original;
    //    group residues sum in parallel ──────────────────────────────────
    for (int ch = 0; ch < 2; ch++) {
        Gen gens[MAXG];
        for (int g = 0; g < nGroups; g++) gen_init(&gens[g], gtype[g], gseed[g][ch]);
        for (int bi = 0; bi < nBands; bi++) { bands[bi].cursor = 0; bands[bi].z1 = 0; bands[bi].z2 = 0; }

        for (long blockStart = 0; blockStart < ns; blockStart += BLOCK) {
            int n = (int)(ns - blockStart < BLOCK ? ns - blockStart : BLOCK);
            double time = (double)blockStart / sr;
            memset(acc, 0, sizeof(float) * n);
            for (int g = 0; g < nGroups; g++) {
            for (int i = 0; i < n; i++) wet[i] = dry[i] = (float)gen_next(&gens[g]);

            for (int bi = 0; bi < nBands; bi++) {
                Band *b = &bands[bi];
                if (b->group != g) continue;
                while (b->cursor < b->n && time > b->ev[b->cursor].end + 0.4) {
                    b->cursor++; b->z1 = 0; b->z2 = 0;
                }
                if (b->cursor >= b->n) continue;
                Event *e = &b->ev[b->cursor];
                if (time < e->t0 - 0.05) continue;

                double env = env_of(e, time);
                double db = e->peakDb * env;
                int ringing = fabs(b->z1) + fabs(b->z2) > 1e-9;
                if (db <= 0.01 && !ringing) continue;

                double freq = e->freq;
                if (e->hasGlide) {
                    // span uses e->end (which a mono-cut may have shortened)
                    // to match the JS reference exactly
                    double p = (time - e->t0) / (e->end - e->t0);
                    if (p < 0) p = 0; if (p > 1) p = 1;
                    freq = e->freq * pow(e->freqEnd / e->freq, p);
                }
                // RBJ peaking EQ — gain A breaks the cancellation
                double A = pow(10.0, db / 40.0);
                double fd = freq * detune[ch];
                double fmax = sr * 0.45;
                if (fd > fmax) fd = fmax;
                double w0 = 2.0 * M_PI * fd / sr;
                double alpha = sin(w0) / (2.0 * e->q);
                double cosw = cos(w0);
                double a0 = 1.0 + alpha / A;
                double b0c = (1.0 + alpha * A) / a0;
                double b1c = (-2.0 * cosw) / a0;
                double b2c = (1.0 - alpha * A) / a0;
                double a1c = (-2.0 * cosw) / a0;
                double a2c = (1.0 - alpha / A) / a0;
                double z1 = b->z1, z2 = b->z2;
                for (int i = 0; i < n; i++) {
                    double x = wet[i];
                    double y = b0c * x + z1;
                    z1 = b1c * x - a1c * y + z2;
                    z2 = b2c * x - a2c * y;
                    wet[i] = (float)y;
                }
                b->z1 = z1; b->z2 = z2;
            }

            for (int i = 0; i < n; i++) acc[i] += wet[i] - dry[i];
            } // groups

            float *o = out[ch];
            for (int i = 0; i < n; i++) o[blockStart + i] = acc[i];
        }
    }

    // ── proof: flat chain must null to bit-exact digital zero ─────────
    if (proof) {
        double peak = 0;
        for (int ch = 0; ch < 2; ch++)
            for (long i = 0; i < ns; i++)
                if (fabs(out[ch][i]) > peak) peak = fabs(out[ch][i]);
        if (peak == 0) { printf("ok proof: %d bands flat -> bit-exact silence (peak = 0)\n", nBands); return 0; }
        printf("FAIL proof: peak = %g\n", peak); return 1;
    }

    // ── loudness ride: per-window RMS onto baked targets ──────────────
    if (rideN > 0) {
        double *gains = malloc(sizeof(double) * rideN);
        for (int w = 0; w < rideN; w++) {
            long a = (long)(w * rideLen * sr);
            long z = (long)((w + 1) * rideLen * sr);
            if (z > ns) z = ns;
            double sum = 0;
            for (int ch = 0; ch < 2; ch++)
                for (long j = a; j < z; j++) sum += (double)out[ch][j] * out[ch][j];
            long cnt = (z - a) * 2; if (cnt < 1) cnt = 1;
            double rms = 10.0 * log10(sum / cnt + 1e-20);
            double g = rideTarget[w] - rms;
            if (g < -60) g = -60; if (g > 30) g = 30;
            gains[w] = g;
        }
        double *sm = malloc(sizeof(double) * rideN); // 3-window smoothing
        for (int w = 0; w < rideN; w++) {
            int lo = w > 0 ? w - 1 : 0, hi = w < rideN - 1 ? w + 1 : rideN - 1;
            double s = 0; for (int k = lo; k <= hi; k++) s += gains[k];
            sm[w] = s / (hi - lo + 1);
        }
        for (long i = 0; i < ns; i++) { // linear interp between window centers
            double pos = (double)i / sr / rideLen - 0.5;
            int w0i = (int)floor(pos);
            if (w0i < 0) w0i = 0; if (w0i > rideN - 1) w0i = rideN - 1;
            int w1i = w0i + 1 < rideN ? w0i + 1 : rideN - 1;
            double fr = pos - w0i; if (fr < 0) fr = 0; if (fr > 1) fr = 1;
            double gdb = sm[w0i] * (1 - fr) + sm[w1i] * fr;
            float lin = (float)pow(10.0, gdb / 20.0);
            out[0][i] *= lin; out[1][i] *= lin;
        }
        free(gains); free(sm);
    }

    // ── normalize + edge fades ────────────────────────────────────────
    double peak = 0;
    for (int ch = 0; ch < 2; ch++)
        for (long i = 0; i < ns; i++)
            if (fabs(out[ch][i]) > peak) peak = fabs(out[ch][i]);
    double g = peak > 0 ? normpeak / peak : 1.0;
    long fi = (long)(fadein * sr), fo = (long)(fadeout * sr);
    for (int ch = 0; ch < 2; ch++) {
        for (long i = 0; i < ns; i++) out[ch][i] = (float)(out[ch][i] * g);
        for (long i = 0; i < fi && i < ns; i++) out[ch][i] *= (float)i / fi;
        for (long i = 0; i < fo && i < ns; i++) out[ch][ns - 1 - i] *= (float)i / fo;
    }

    // ── write interleaved f32le stereo ────────────────────────────────
    FILE *o = rawPath ? fopen(rawPath, "wb") : stdout;
    if (!o) die("cannot open output");
    float *inter = malloc(sizeof(float) * 2 * ns);
    for (long i = 0; i < ns; i++) { inter[2 * i] = out[0][i]; inter[2 * i + 1] = out[1][i]; }
    fwrite(inter, sizeof(float), 2 * ns, o);
    if (rawPath) fclose(o);
    fprintf(stderr, "nullnoise: %d bands, %.1f s -> %s\n", nBands, (double)ns / sr, rawPath ? rawPath : "stdout");
    return 0;
}
