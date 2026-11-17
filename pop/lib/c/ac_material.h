// ac_material.h — a material for the master, in C. The mirror of
// pop/lib/material.mjs: the last thing the mix passes through before the
// limiter, so every voice ends up made of the same stuff.
//
//   ac_material_apply(mix, frames, 48000, "tape", 1.0);   // interleaved stereo, in place
//
// Keep the table and the loop in step with material.mjs — a lane's compare.mjs
// holds the two to each other.

#ifndef AC_MATERIAL_H
#define AC_MATERIAL_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef struct { double hz, cents; } AcWobble;
typedef struct {
    const char *name;
    AcWobble wow, flutter, scrape;
    double drive, bias, emphasisHz;
    double bumpHz, bumpDb, topHz;
    double crosstalk, hissDb, dropouts;
} AcMaterial;

static const AcMaterial AC_MATERIALS[] = {
    // Plastic tape: a type-I cassette on a consumer deck.
    { "tape", { 0.55, 5 }, { 7.3, 1.6 }, { 0.11, 2 }, 1.5, 0.06, 2600, 90, 2.2, 11500, 0.05, -62, 0.012 },
};

typedef struct { double x1, x2, y1, y2, pre, de, lp1, lp2; } AcMaterialCh;

// Returns 0, or -1 when the material is not in the table.
static int ac_material_apply(float *buf, long n, double sr, const char *name, double amount) {
    const AcMaterial *M = NULL;
    for (size_t i = 0; i < sizeof AC_MATERIALS / sizeof *AC_MATERIALS; i++)
        if (!strcmp(AC_MATERIALS[i].name, name)) M = &AC_MATERIALS[i];
    if (!M) return -1;
    const double TAU = 2.0 * M_PI;

    // pitch deviation in cents → delay swing in samples
#define AC_SWING(m) ((pow(2.0, (m).cents * amount / 1200.0) - 1.0) / (TAU * (m).hz) * sr)
    const double wA = AC_SWING(M->wow), fA = AC_SWING(M->flutter), sA = AC_SWING(M->scrape);
#undef AC_SWING
    const long base = (long)ceil(wA + fA + sA) + 4;
    const uint32_t RB = 1u << (int)ceil(log2((double)(base * 2 + 8))), mask = RB - 1;
    float *ring[2] = { calloc(RB, sizeof(float)), calloc(RB, sizeof(float)) };
    if (!ring[0] || !ring[1]) return -1;

    // head bump: one peaking biquad
    const double A = pow(10.0, M->bumpDb * amount / 40.0), w0 = TAU * M->bumpHz / sr, al = sin(w0) / (2 * 0.9);
    const double a0 = 1 + al / A, b0 = (1 + al * A) / a0, b1 = -2 * cos(w0) / a0, b2 = (1 - al * A) / a0;
    const double a1 = b1, a2 = (1 - al / A) / a0;
    AcMaterialCh st[2] = { { 0 }, { 0 } };
    const double kE = 1 - exp(-TAU * M->emphasisHz / sr);
    const double kT = 1 - exp(-TAU * (M->topHz + (20000 - M->topHz) * (1 - amount)) / sr);
    const double drive = 1 + (M->drive - 1) * amount, norm = 1 / tanh(drive), xt = M->crosstalk * amount;
    const double hiss = pow(10.0, M->hissDb / 20.0) * amount;

    uint32_t seed = 0x7a9e5eed;
#define AC_RND() (seed ^= seed << 13, seed ^= seed >> 17, seed ^= seed << 5, seed / 4294967296.0)
    double hl = 0, hr = 0, drop = 1, dropTo = 1;
    long dropLeft = 0;
    for (long i = 0; i < n; i++) {
        const double t = i / sr;
        // the tape's speed is shared by both tracks, so one delay for both ears
        const double d = base + wA * sin(TAU * M->wow.hz * t) + fA * sin(TAU * M->flutter.hz * t + 1.3)
                       + sA * sin(TAU * M->scrape.hz * t + 0.4);
        const long di = (long)floor(d);
        const double fr = d - di;
        // brief oxide dropouts: the level dips for a few tens of milliseconds
        if (dropLeft-- <= 0) {
            if (AC_RND() < M->dropouts * amount) dropTo = 0.55 + 0.3 * AC_RND(); else dropTo = 1;
            dropLeft = (long)floor(sr * (0.03 + 0.25 * AC_RND()));
        }
        drop += (dropTo - drop) * 0.002;
        double out[2];
        for (int ch = 0; ch < 2; ch++) {
            float *r = ring[ch];
            AcMaterialCh *s = &st[ch];
            r[(uint32_t)i & mask] = buf[i * 2 + ch];
            double x = r[(uint32_t)(i - di) & mask] * (1 - fr) + r[(uint32_t)(i - di - 1) & mask] * fr;
            const double y = b0 * x + b1 * s->x1 + b2 * s->x2 - a1 * s->y1 - a2 * s->y2;
            s->x2 = s->x1; s->x1 = x; s->y2 = s->y1; s->y1 = y; x = y;
            // highs hit the oxide harder: lift them into the saturator, drop them back after
            s->pre += kE * (x - s->pre);
            const double hot = x + (x - s->pre) * 0.9 * amount;
            const double sat = (tanh(hot * drive + M->bias * amount) - tanh(M->bias * amount)) * norm;
            s->de += kE * (sat - s->de);
            x = sat - (sat - s->de) * (0.9 * amount / (1 + 0.9 * amount));
            s->lp1 += kT * (x - s->lp1); s->lp2 += kT * (s->lp1 - s->lp2);
            out[ch] = s->lp2 * drop;
        }
        hl += 0.35 * (AC_RND() * 2 - 1 - hl);
        hr += 0.35 * (AC_RND() * 2 - 1 - hr);
        buf[i * 2] = (float)(out[0] + out[1] * xt + (AC_RND() * 2 - 1 - hl) * hiss);
        buf[i * 2 + 1] = (float)(out[1] + out[0] * xt + (AC_RND() * 2 - 1 - hr) * hiss);
    }
#undef AC_RND
    free(ring[0]); free(ring[1]);
    return 0;
}

#endif
