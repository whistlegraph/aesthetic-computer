// mosstrax.c — a sine-bell sibling of dewtrax/sinetrax, in a wholly different
// register and mood. Same instrument family (FM sine bells + soft sine drones
// + Schroeder reverb), new song: E minor, slow (84 BPM), earthy and grounded.
// The character is mossy and woody — low soft bells with ratios near 2.0–3.0
// (body, not glass) over deep sub drones, with long silences between events.
// Nothing sparkles up high here; it sits in the low-mid, damp and patient.
// No samples, no drums, no vocals — pure sine.
// render-c.mjs --engine mosstrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -o mosstrax mosstrax.c -lm
// Run:    ./mosstrax --out out/mosstrax-raw.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static double BPMV = 84, BEAT, BAR, SX;

static uint32_t rng_s = 0x6d6f7373; // "moss"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// Almost straight here (0.52) — mosstrax barely leans. The earth doesn't
// hurry; events fall close to the grid with the faintest drag on the "&".
static double SWING = 0.52;
static double swing_beats(double beats) {
    double e8 = beats * 2.0;                 // position in 8ths
    long idx = (long)floor(e8 + 1e-9);
    double frac = e8 - idx;
    double pos = (idx / 2) * BEAT + ((idx & 1) ? SWING * BEAT : 0.0);
    return pos + frac * (BEAT / 2.0);        // interpolate 16ths inside the 8th
}

static long N;
static float *busL, *busR, *revL, *revR;
static inline void adds(float *L, float *R, long i, double l, double r) { if (i >= 0 && i < N) { L[i] += (float)l; R[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── voices ────────────────────────────────────────────────────────────────
// FM sine bell — carrier + a decaying modulator. With low `ratio` (2.0–3.0)
// the partials cluster near the fundamental, giving a woody, hollow body
// rather than a glassy chime; exp amp decay. The attack transient is gentle
// here (mod index 2.2, slow mod-decay) so onsets are soft, not plinky.
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.006 * SR);               // soft, woody onset (not a plink)
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 2.2 * exp(-tt * 5.0);        // modest, slow-decaying transient — body over shimmer
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — here it is reimagined as a "drip": a low-mid woody knock, not a
// high sparkle. Short, rounded (ratio 2.0), with a soft reverb send so it
// rings a moment in the damp space. Spelled to sit in the body of the mix.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.85, g, pan, 2.0, 7.0);
    bell(revL, revR, note, t, 0.85, g * 0.55, 0, 2.0, 7.0);
}

// soft sine drone — pad / sub bed with a slow raised-cosine attack/release.
// A touch more upper-harmonic body (3rd/5th) for a reedy, organ-ish warmth
// fitting the woody mood; very long fades so it breathes in and out.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.55 * SR), rel = (long)(0.80 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.20 * sin(ph * 2) + 0.10 * sin(ph * 3) + 0.04 * sin(ph * 5)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (slightly panned for width), with a soft reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.6; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.45, 0); }
}

// ── harmony — E minor, a slow i–VI–iv–v wander (modal, grounded) ────────────
// per-bar bass roots that the loop walks: Em, C, Am, Bm — earthy minor pull.
static const int E_ROOTS[4] = { 52, 48, 45, 47 };       // E3, C3, A2, B2

// four 4-note pad voicings, one per chord — low and close-spaced for warmth.
static const int I_PAD[4]  = { 28, 35, 40, 47 };        // E min   (E B E B → E G B E body)
static const int VI_PAD[4] = { 24, 31, 36, 43 };        // C maj   (C G C G body)
static const int IV_PAD[4] = { 21, 28, 33, 40 };        // A min   (A E A E body)
static const int V_PAD[4]  = { 23, 30, 35, 42 };        // B min/v (B F# B F# body)
static const int *PADS[4] = { I_PAD, VI_PAD, IV_PAD, V_PAD };

// E minor pentatonic (E G A B D), low-mid octave — for the woody bell motif.
static const int PENT[5] = { 40, 43, 45, 47, 50 };
// the "drip" palette — low-mid woody knocks (E3 G3 A3 B3 D4 E4), no high glass.
static const int DROPS[6] = { 52, 55, 57, 59, 62, 64 };

// section map. SECBARS drives every loop bound below. At 84 BPM a bar is
// ~2.857s, so this 40-bar form lands at ~114s + reverb tail. Wave/outro
// counts stay multiples of 4 so the 4-bar phrases (pad loop, motif) line up.
static const char *ORDER[8] = { "soil", "moss", "groveA", "stillA", "groveB", "stillB", "groveC", "settle" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 4, 4 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a woody knock — root + 5th, low and short, ratio 2.0 for hollow body.
// (No octave-up sparkle; this is meant to thud, not chime.)
static void knock(double t, int root, double g, int fifth) {
    bell(busL, busR, root, t, BEAT * 2.0, g, -0.12, 2.0, 3.0);
    if (fifth) bell(busL, busR, root + 7, t + 0.020, BEAT * 1.8, g * 0.55, 0.16, 2.5, 3.5);
    bell(revL, revR, root, t, BEAT * 2.0, g * 0.4, 0, 2.0, 3.0);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// roll — a slow, sparse descending grace-run of n woody bells across `span`
// seconds (a low flourish that settles downward, like something falling soft).
static void roll(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.6, g * (1.0 - 0.30 * i / n), rnd2() * 0.45, 2.5, 5.0);
}

int main(int argc, char **argv) {
    const char *out_path = "out/mosstrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 8.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# mosstrax.c · %g BPM · %d bars · %.1fs · E minor woody sine bells\n", BPMV, TB, totalSec);

    // the woody bell motif, as {phrase-bar, beat, note} — a low, drifting
    // E-minor-pentatonic phrase that rises a little and sinks back, with long
    // gaps between notes (the form leaves space to breathe).
    double MOTIF[6][3] = { {0,0,52},{0,3,55},{1,2,57},{2,0,59},{2,3,55},{3,1.5,52} };

    // ── SOIL (just sub + one low pad — almost empty, the ground settling) ──
    { int idx = sec_index("soil"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        for (int b = 0; b < nb; b++) softsine(busL, busR, 28, t0 + b * BAR, BAR * 0.97, 0.13, 0); // deep sub through the whole intro
        // one lone woody bell near the end, far apart from anything else
        bell(busL, busR, 52, t0 + BAR * (nb - 1) + BEAT * 1.0, BAR * 1.2, 0.12, rnd2() * 0.25, 2.0, 2.0);
    }

    // ── MOSS — the motif emerges, sparse drips, pad climb into the first grove ─
    { int idx = sec_index("moss"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.13);
        for (int b = 0; b < nb; b++) softsine(busL, busR, b < h ? 24 : 23, t0 + b * BAR, BAR * 0.95, 0.11, 0);
        for (int z = 0; z < 6; z++) { int b = (int)MOTIF[z][0]; if (b >= nb) continue; double tt = t0 + b * BAR + swing_beats(MOTIF[z][1]); bell(busL, busR, MOTIF[z][2], tt, BEAT * 1.6, 0.11, rnd2() * 0.35, 2.5, 3.0); bell(revL, revR, MOTIF[z][2], tt, BEAT * 1.6, 0.05, 0, 2.5, 3.0); }
        for (int b = 1; b < nb; b += 2) if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 4)], t0 + b * BAR + 2.5 * BEAT, 0.06, rnd2() * 0.5);
        roll(t0 + BAR * nb - BEAT * 2, 59, 4, -2, BEAT * 1.8, 0.06);   // a soft descent into grove A
    }

    // ── helper for the three groves (slow, sparse, never busy) ──
    const char *groves[3] = { "groveA", "groveB", "groveC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(groves[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = E_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // a slow, soft bass heartbeat — beats 1 & 3 only, woody and low.
            for (int e = 0; e < 4; e++) { double tt = t0 + swing_beats(e * 1.0); int on = (e % 2 == 0); if (on) bell(busL, busR, root - 12, tt, BEAT * 1.4, 0.11, on ? -0.06 : 0.20, 2.0, 4.0); }
            // sustained sub body (one per bar, under the heartbeat)
            softsine(busL, busR, root - 12, t0, BAR * 0.96, 0.12, 0);
            // the woody knock on beat 3 (sparser than dewtrax's stab)
            if (b % 2 == 0) knock(t0 + BEAT * 2, root, big ? 0.15 : 0.12, 1);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.085);
            // the woody bell motif (4-bar phrase) — low, plain, with gaps
            for (int z = 0; z < 6; z++) if ((int)MOTIF[z][0] == b % 4) {
                double note = MOTIF[z][2], tt = t0 + swing_beats(MOTIF[z][1]);
                bell(busL, busR, note, tt, BEAT * 1.5, big ? 0.13 : 0.10, rnd2() * 0.35, 2.5, 3.2);
                bell(revL, revR, note, tt, BEAT * 1.5, 0.06, 0, 2.5, 3.2);
            }
            // a lone pentatonic drip — only sometimes, deep in the off-beat
            if (b % 2 == 1 && rnd() < 0.5) bell(busL, busR, PENT[b % 5] + 12, t0 + swing_beats(2.5), BEAT * 1.0, 0.06, rnd2() * 0.45, 2.0, 4.0);
            // drips — sprinkled, a touch denser in the big grove but still spare
            int ndrops = big ? 2 : 1;
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.45) droplet(DROPS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 4) * 1.0 + 0.5), 0.05, rnd2() * 0.55);
            // a soft descending roll at phrase ends of the big grove
            if (big && b % 4 == 3) roll(t0 + BEAT * 2, 64, 4, -3, BEAT * 1.6, 0.06);
        }
        // a low choir-ish sine bed through the biggest grove (warm undertow)
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z], t0, BAR * 2, 0.045, (z - 1) * 0.4); }
    }

    // ── STILLS — most spacious of all: pad + half-time sub + one or two bells ──
    const char *still[2] = { "stillA", "stillB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(still[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, IV_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.12);
        for (int b = 0; b < nb; b++) { softsine(busL, busR, b < h ? 21 : 23, t0 + b * BAR, BAR * 0.88, 0.12, 0); if (b % 2 == 0) bell(busL, busR, I_PAD[1 + (b / 2) % 3] + 12, t0 + b * BAR + BEAT, BAR * 0.9, 0.10, rnd2() * 0.35, 2.0, 2.4); if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 5)], t0 + b * BAR + 2.5 * BEAT, 0.045, rnd2() * 0.55); }
        if (z2 == 1) roll(t0 + BAR * (nb - 1), 59, 5, -2, BEAT * 2.4, 0.05); // a slow settling descent into grove C
    }

    // ── SETTLE — decaying bells + pad wash + a final deep bell, sinking out ──
    { int idx = sec_index("settle"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        for (int b = 0; b < nb; b++) { double e = fmax(0.1, 1.0 - (double)b / nb); int root = b < h ? 52 : 48; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.12 * e, 0); if (b % 2 == 0) knock(t0 + b * BAR + BEAT * 2, root, 0.10 * e, 1); if (rnd() < 0.45) droplet(DROPS[(int)(rnd() * 5)], t0 + b * BAR + 1.5 * BEAT, 0.035 * e, rnd2() * 0.55); }
        // final low bell — the ground exhaling, very long and round
        bell(busL, busR, 28, t0 + BAR * (nb - 2), 6.0, 0.20, 0, 2.0, 1.0);
        bell(busL, busR, 40, t0 + BAR * (nb - 2), 6.0, 0.11, 0.15, 2.5, 1.2);
        bell(revL, revR, 35, t0 + BAR * (nb - 2), 6.0, 0.11, 0, 2.0, 1.1);
    }

    // ── REVERB (Schroeder) — a wide, damp, cave-like space for the woody bells.
    // Longer decay + heavier damping than dewtrax → dark, mossy tail, no fizz. ─
    {
        double decay = 0.85, wet = 0.50, damp = 0.55;
        int CD[6]; double cds[6] = { 0.0311, 0.0383, 0.0427, 0.0461, 0.0523, 0.0613 };
        for (int k = 0; k < 6; k++) CD[k] = (int)(cds[k] * SR);
        int AD[2] = { (int)(0.005 * SR), (int)(0.0017 * SR) }; double apFb = 0.5;
        float *cbL[6], *cbR[6]; int ciL[6] = {0}, ciR[6] = {0}; double cLPL[6] = {0}, cLPR[6] = {0};
        for (int k = 0; k < 6; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        float *abL[2], *abR[2]; int aiL[2] = {0}, aiR[2] = {0};
        for (int k = 0; k < 2; k++) { abL[k] = calloc(AD[k], 4); abR[k] = calloc(AD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 6; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                cLPL[k] = dL * (1 - damp) + cLPL[k] * damp; cLPR[k] = dR * (1 - damp) + cLPR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + cLPL[k] * decay); cbR[k][ciR[k]] = (float)(inR + cLPR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            cL /= 6; cR /= 6;
            for (int k = 0; k < 2; k++) {
                double dL = abL[k][aiL[k]], dR = abR[k][aiR[k]];
                double oL = -apFb * cL + dL, oR = -apFb * cR + dR;
                abL[k][aiL[k]] = (float)(cL + apFb * oL); abR[k][aiR[k]] = (float)(cR + apFb * oR);
                aiL[k] = (aiL[k] + 1) % AD[k]; aiR[k] = (aiR[k] + 1) % AD[k];
                cL = oL; cR = oR;
            }
            busL[i] += (float)(cL * wet); busR[i] += (float)(cR * wet);
        }
        for (int k = 0; k < 6; k++) { free(cbL[k]); free(cbR[k]); }
        for (int k = 0; k < 2; k++) { free(abL[k]); free(abR[k]); }
    }

    // ── normalize + gentle in/out fades (long, to suit the slow mood) ──
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.85 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(3.5 * SR), fout = (long)(6.0 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
