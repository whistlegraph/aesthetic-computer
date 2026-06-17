// glasstrax.c — a sine-bell sibling of dewtrax. Same instrument family (FM sine
// bells + soft sine drones + Schroeder reverb), new song: A major, faster
// (124 BPM), a light shimmering swing. The character is weightless and
// crystalline — fast pentatonic arps glittering in the upper register, bright
// glassy bells (high ratios, 5.0–7.0), and a busy mist of high droplets. Airy,
// delicate, like sun through cut glass. No samples, no drums, no vocals — pure
// sine. render-c.mjs --engine glasstrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -o glasstrax glasstrax.c -lm
// Run:    ./glasstrax --out out/glasstrax-raw.wav

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
static double BPMV = 124, BEAT, BAR, SX;

static uint32_t rng_s = 0x676C6173; // "glas"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// A light shimmer-swing here (0.55) — the "&"s drag just slightly so the busy
// arps glitter rather than march. 16ths interpolate inside the swung 8th.
static double SWING = 0.55;
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
// FM sine bell — carrier + a decaying modulator gives the glassy partial
// shimmer; exp amp decay. `ratio` shapes the timbre — glasstrax lives high:
// 5.0–7.0 are its home (very glassy, music-box plink), 2.0 is the rare body.
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.0010 * SR);              // crisp, glassy onset
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 6.0 * exp(-tt * 13.0);       // brighter, faster attack transient → more "cut glass"
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — a tiny, very bright, fast-decaying bell up high; the crystalline
// sparkle. Pushed glassier (ratio 6.0) and feeding the reverb tail harder so
// the mist of droplets shimmers and rings out.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.50, g, pan, 6.0, 18.0);
    bell(revL, revR, note, t, 0.50, g * 0.6, 0, 6.0, 18.0);
}

// soft sine drone — pad / sub bed with a slow raised-cosine attack/release.
// Lighter harmonics here so the pad stays airy and recedes under the bells.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.35 * SR), rel = (long)(0.50 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.12 * sin(ph * 2) + 0.04 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (slightly panned for width), with a soft reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.7; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.45, 0); }
}

// ── harmony — A major, an airy I–IV–vi–V loop (bright, suspended) ────────────
// per-bar bass roots that the loop walks: A, D, F#m, E.
static const int A_ROOTS[4] = { 57, 50, 54, 52 };      // A3, D3, F#3, E3
// four 4-note pad voicings, one per chord in the loop — open, with added 2nds
// /9ths so the pad rings glassy rather than blocky.
static const int I_PAD[4]  = { 45, 52, 56, 59 };       // A maj add9 (A E G# B)
static const int IV_PAD[4] = { 38, 45, 50, 54 };       // D maj add9 (D A D F#→ D A D F#? D E F#) → D A E F#
static const int VI_PAD[4] = { 42, 49, 52, 57 };       // F# m11    (F# C# E A)
static const int V_PAD[4]  = { 40, 47, 51, 56 };       // E maj add9 (E B G# A→ E B G# B)
static const int *PADS[4] = { I_PAD, IV_PAD, VI_PAD, V_PAD };

// A major pentatonic (A B C# E F#), mid octave — for arps + droplet picks.
static const int PENT[5] = { 57, 59, 61, 64, 66 };
// the high droplet palette (A5 B5 C#6 E6 F#6 A6 B6) — glasstrax lives up here.
static const int DROPS[7] = { 81, 83, 85, 88, 90, 93, 95 };

// section map. SECBARS drives every loop bound below, so retuning the form
// (or the whole length) is a one-line edit — at 124 BPM a bar is ~1.935s, so
// this 56-bar form lands at ~114s. Keep wave/outro counts multiples of 4 so
// the 4-bar phrases (pad loop, lead) line up.
static const char *ORDER[8] = { "intro", "bloom", "waveA", "hushA", "waveB", "hushB", "waveC", "outro" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 12, 12 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a glass stab — root + 5th + 9th, very bright (high ratio) and short, voiced
// up an octave from the bass so it sparkles rather than thuds.
static void stab(double t, int root, double g, int triad) {
    bell(busL, busR, root + 12, t, BEAT * 1.2, g, -0.15, 7.0, 6.0);
    bell(busL, busR, root + 19, t + 0.010, BEAT * 1.2, g * 0.7, 0.18, 7.0, 6.0);
    if (triad) bell(busL, busR, root + 26, t + 0.020, BEAT * 1.0, g * 0.5, 0.0, 6.0, 7.0);
    bell(revL, revR, root + 19, t, BEAT * 1.2, g * 0.45, 0, 7.0, 6.0);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// frill — a quick grace-run of n bells stepping by `step` semitones across
// `span` seconds (a sparkly flourish for high points / pickups).
static void frill(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.4, g * (1.0 - 0.30 * i / n), rnd2() * 0.6, 6.0, 15.0);
}
// shimmer — a fast pentatonic up-run of 8 droplet-bells over `span`; the
// signature glasstrax glitter (lots of high, delicate notes in a hurry).
static void shimmer(double t, int oct, double span, double g) {
    for (int i = 0; i < 8; i++) {
        int p = PENT[i % 5] + (i / 5) * 12 + oct;
        bell(busL, busR, p, t + span * i / 8.0, span * 0.9, g * (0.7 + 0.3 * i / 8.0), rnd2() * 0.7, 5.0 + rnd() * 1.5, 16.0);
        bell(revL, revR, p, t + span * i / 8.0, span * 0.9, g * 0.4, 0, 6.0, 16.0);
    }
}
// triplet — three quick bells over one beat (middle lifted a 3rd), a gentle
// lift on phrase starts.
static void triplet(double t, double note, double g) {
    for (int i = 0; i < 3; i++) bell(busL, busR, note + (i == 1 ? 4 : 0), t + BEAT * i / 3.0, BEAT / 3.0 * 1.3, g * (i == 1 ? 1.1 : 1.0), (i - 1) * 0.3, 6.0, 13.0);
}

int main(int argc, char **argv) {
    const char *out_path = "out/glasstrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 6.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# glasstrax.c · %g BPM · %d bars · %.1fs · A major sine bells (crystalline)\n", BPMV, TB, totalSec);

    // the lead, as {phrase-bar, beat, note} — a bright, rising-then-cresting
    // motif in A major pentatonic across the 4-bar phrase, sitting high.
    double LEAD[8][3] = { {0,0,76},{0,2.5,81},{1,0,83},{1,1.5,85},{2,0,88},{2,1.5,85},{3,0,83},{3,2,90} };

    // ── INTRO (staggered: airy pad → high bell glints → sub → droplet mist) ──
    { int idx = sec_index("intro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.09);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        int mel[4] = { 88, 85, 83, 81 };                       // descending glass glints, from bar 2
        for (int b = 0; b < 4 && 2 + b < nb; b++) bell(busL, busR, mel[b], t0 + (2 + b) * BAR, BAR * 0.8, 0.11, rnd2() * 0.4, 6.0, 3.5);
        for (int b = h; b < nb; b++) softsine(busL, busR, 33, t0 + b * BAR, BAR * 0.95, 0.10, 0); // airy sub through the back half
        for (int b = 1; b < nb; b++) { int nd = b; for (int k = 0; k < nd; k++) if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 7)], t0 + b * BAR + swing_beats((int)(rnd() * 8) * 0.5), 0.045, rnd2() * 0.7); } // gathering mist
    }

    // ── BLOOM — rising pentatonic shimmer + pad climb into the first wave ──
    { int idx = sec_index("bloom"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.13);
        for (int b = 0; b < nb; b++) shimmer(t0 + b * BAR + BEAT * 2, (b - 1) * 12, BEAT * 2.0, 0.05 + 0.02 * b); // shimmer runs climbing each bar
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * h + swing_beats(s * 0.25); int p = PENT[s % 5] + (s / 5) * 12 + 12; bell(busL, busR, p, tt, SX * 2.2, 0.05 + 0.06 * s / 16.0, rnd2() * 0.5, 5.5, 10.0); }
        frill(t0 + BAR * nb - BEAT * 2, 81, 8, 2, BEAT * 1.6, 0.06);   // ascending flourish into wave A
    }

    // ── helper for the three waves (gentle, flowing, glittering — not a drop) ──
    const char *waves[3] = { "waveA", "waveB", "waveC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(waves[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = A_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // a soft pulsing bass — beats 1 & 3, the "&"s lighter & panned wide.
            for (int e = 0; e < 8; e++) { double tt = t0 + swing_beats(e * 0.5); int on = (e % 2 == 0); if (on || rnd() < 0.45) bell(busL, busR, root - 12, tt, BEAT * 0.5, on ? 0.085 : 0.045, on ? -0.08 : 0.24, 2.0, on ? 12.0 : 15.0); }
            // sustained airy sub body (one per bar, under the pulse)
            softsine(busL, busR, root - 12, t0, BAR * 0.95, 0.085, 0);
            // the glass stab on beat 3
            stab(t0 + BEAT * 2, root, big ? 0.13 : 0.10, big);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.06);
            // a gentle triplet lift on each phrase-start downbeat
            if (phrase) triplet(t0 + 12, root + 24, big ? 0.07 : 0.05);
            // the lead (4-bar phrase) — swung, very bright, frilled on highs
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 0.7, big ? 0.105 : 0.08, rnd2() * 0.45, 6.0, 8.0);
                bell(revL, revR, note, tt, BEAT * 0.7, 0.05, 0, 6.0, 8.0);
                if (note >= 83) frill(tt + 0.03, note, big ? 4 : 3, 2, BEAT * 0.4, big ? 0.05 : 0.035);
            }
            // fast pentatonic arp — busy 16ths through every bar (the glitter bed)
            for (int s = 0; s < 8; s++) { if (rnd() < (big ? 0.95 : 0.75)) { double tt = t0 + swing_beats(s * 0.5); int p = PENT[(s + b) % 5] + 12 + (s >= 4 ? 12 : 0); bell(busL, busR, p, tt, SX * 1.8, 0.04 + 0.015 * (s % 4), rnd2() * 0.7, 5.0 + rnd(), 14.0); } }
            // a higher shimmer run once per phrase as a crest
            if (b % 4 == 2) shimmer(t0 + BEAT * 1, 24, BEAT * 2.0, big ? 0.05 : 0.04);
            // crystalline droplet mist — dense, denser in the big wave
            int ndrops = big ? 4 : 2;
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 7)], t0 + swing_beats((int)(rnd() * 8) * 0.5), 0.04, rnd2() * 0.8);
            // descending frill run at phrase ends of the big wave (high point)
            if (big && b % 4 == 3) frill(t0 + BEAT * 3, 95, 6, -2, BEAT * 0.8, 0.05);
        }
        // a soft glassy sine bed through the biggest wave
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z] + 12, t0, BAR * 2, 0.035, (z - 1) * 0.5); }
    }

    // ── HUSHES — spacious: airy pad + half-time sub + sparse high bells + lone droplets ──
    const char *hush[2] = { "hushA", "hushB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(hush[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, IV_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.11);
        for (int b = 0; b < nb; b++) { softsine(busL, busR, b < h ? 33 : 35, t0 + b * BAR, BAR * 0.85, 0.085, 0); if (b % 2 == 0) bell(busL, busR, I_PAD[1 + (b / 2) % 3] + 12, t0 + b * BAR, BAR * 0.85, 0.09, rnd2() * 0.5, 5.5, 3.0); for (int k = 0; k < 2; k++) if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 7)], t0 + b * BAR + swing_beats((int)(rnd() * 6) * 0.5 + 1), 0.038, rnd2() * 0.8); }
        if (z2 == 1) shimmer(t0 + BAR * (nb - 2), 12, BAR * 2.0, 0.05); // rising shimmer into wave C
    }

    // ── OUTRO — decaying glass bells + airy pad wash + a final ringing chord ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.10);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.09);
        for (int b = 0; b < nb; b++) { double e = fmax(0.1, 1.0 - (double)b / nb); int root = b < h ? 57 : 52; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.085 * e, 0); if (b % 2 == 0) stab(t0 + b * BAR + BEAT * 2, root, 0.10 * e, 0); for (int k = 0; k < 2; k++) if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 7)], t0 + b * BAR + swing_beats((int)(rnd() * 6) * 0.5 + 1), 0.032 * e, rnd2() * 0.8); }
        // a final ringing A-major glass chord
        bell(busL, busR, 45, t0 + BAR * (nb - 3), 5.0, 0.14, 0, 2.0, 1.2);
        bell(busL, busR, 69, t0 + BAR * (nb - 3) + 0.04, 5.0, 0.09, -0.2, 6.0, 1.4);
        bell(busL, busR, 76, t0 + BAR * (nb - 3) + 0.08, 5.0, 0.08, 0.2, 6.0, 1.5);
        bell(busL, busR, 81, t0 + BAR * (nb - 3) + 0.12, 5.0, 0.06, 0.0, 7.0, 1.6);
        bell(revL, revR, 64, t0 + BAR * (nb - 3), 5.0, 0.10, 0, 5.0, 1.4);
    }

    // ── REVERB (Schroeder) — wide, long, and bright for the crystalline space ──
    {
        double decay = 0.85, wet = 0.60, damp = 0.28;   // brighter (low damp) + wetter than dewtrax
        int CD[6]; double cds[6] = { 0.0297, 0.0371, 0.0411, 0.0437, 0.0497, 0.0581 };
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

    // ── normalize + gentle in/out fades ──
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.85 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(2.0 * SR), fout = (long)(4.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
