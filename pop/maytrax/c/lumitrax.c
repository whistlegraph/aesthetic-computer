// lumitrax.c — a sine-bell sibling of dewtrax. Same instrument (FM sine bells
// + soft sine drones + Schroeder reverb), new song: F♯ major, faster (116 BPM),
// and a wholly different intent. Where dewtrax is a nocturnal music-box, lumitrax
// is hypnotic minimal phasing — three interlocking arpeggiated bell sequences
// that loop at slightly different lengths so they slowly shift against one
// another (a Steve Reich / phase-music feel). Bright, repetitive, pulse-driven,
// trance-like; voices are added and subtracted but the pulse never stops.
// No samples, no drums, no vocals — pure sine.
// render-c.mjs --engine lumitrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o lumitrax lumitrax.c -lm
// Run:    ./lumitrax --out out/lumitrax-raw.wav

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
static double BPMV = 116, BEAT, BAR, SX;

static uint32_t rng_s = 0x6c756d69; // "lumi"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// Phase music wants a dead-straight, machine-even grid — the interest comes from
// the patterns drifting against each other, not from groove. SWING ~ 0.50 keeps
// the 8ths perfectly even so the interlock reads cleanly.
static double SWING = 0.50;
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
// shimmer; exp amp decay. `ratio` shapes the timbre (3.5 = bell, 7.0 =
// music-box plink, 2.0 = softer body).
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.0012 * SR);              // sharp, punctuated onset
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 5.0 * exp(-tt * 11.0);       // brighter, faster-decaying attack transient
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — a tiny, very bright, fast-decaying bell up high; a high sparkle.
// pure feed into both the dry bus and (softly) the reverb tail for shimmer.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.55, g, pan, 5.0, 16.0);
    bell(revL, revR, note, t, 0.55, g * 0.5, 0, 5.0, 16.0);
}

// soft sine drone — pad / sub bed with a slow raised-cosine attack/release.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.30 * SR), rel = (long)(0.45 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.16 * sin(ph * 2) + 0.06 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (slightly panned for width), with a soft reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.6; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.4, 0); }
}

// ── harmony — F♯ major, a slow I–V–vi–IV loop (the "anthemic trance" cadence) ─
// Phase music thrives on a small, static harmonic field; this loop turns very
// slowly (one chord per phrase) so the ear hears mostly the shifting patterns.
// per-phrase bass roots: F#, C#, D#m, B.
static const int F_ROOTS[4] = { 54, 49, 51, 47 };      // F#3, C#3, D#3, B2
// four 4-note pad voicings, one per chord in the loop.
static const int I_PAD[4]  = { 42, 49, 54, 58 };       // F# maj  (F# C# F# A#)
static const int V_PAD[4]  = { 37, 44, 49, 53 };       // C# maj  (C# G# C# F)
static const int VI_PAD[4] = { 39, 46, 51, 54 };       // D# min  (D# A# D# F#)
static const int IV_PAD[4] = { 35, 42, 47, 51 };       // B  maj  (B  F# B  D#)
static const int *PADS[4] = { I_PAD, V_PAD, VI_PAD, IV_PAD };

// F# major pentatonic (F# G# A# C# D#), spanning octaves — the spine of every
// interlocking arpeggio. Indices wrap, so a coprime cycle-length vs 5 makes the
// pattern walk through the scale and re-phase.
static const int PENT[5]  = { 54, 56, 58, 61, 63 };    // F#3 G#3 A#3 C#4 D#4
static const int PENTH[5] = { 66, 68, 70, 73, 75 };    // F#4 G#4 A#4 C#5 D#5
// the high droplet palette (F#5 G#5 A#5 C#6 D#6 F#6).
static const int DROPS[6] = { 78, 80, 82, 85, 87, 90 };

// section map. SECBARS drives every loop bound below, so retuning the form
// (or the whole length) is a one-line edit — at 116 BPM a bar is ~2.07s, so this
// 56-bar form lands at ~122s. Keep section counts multiples of 4 so the 4-bar
// pad/harmony phrases line up with the phasing cells.
static const char *ORDER[8] = { "seed", "weave", "cellA", "thinA", "cellB", "cellC", "peak", "fade" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 8, 12, 8 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// ── phasing engine ───────────────────────────────────────────────────────────
// A "cell" is a looping arpeggio of `len` notes played on a steady 8th grid.
// Two or more cells with coprime lengths (e.g. 6 and 7) drift against the bar,
// re-aligning only after their LCM — the core Reich phasing effect. `voice`
// picks the palette; `off` shifts a cell's start so two copies of the same cell
// run a fixed distance apart (classic phase canon).
static void cell(double tStart, double tEnd, const int *pal, int palN, int len, int off,
                 double g, double pan, double ratio, double dec, int oct) {
    // step every 8th note across the window
    double step = BEAT / 2.0;
    long k = 0;
    for (double t = tStart; t < tEnd; t += step, k++) {
        int idx = (int)((k + off) % len);
        int note = pal[idx % palN] + (idx / palN) * 12 + oct;
        // a soft amplitude shimmer following the pattern position so the cell
        // "breathes" as it re-phases.
        double sh = 0.78 + 0.22 * sin(TAU * (double)((k + off) % (len * 2)) / (len * 2));
        bell(busL, busR, note, t, step * 1.9, g * sh, pan, ratio, dec);
    }
}

int main(int argc, char **argv) {
    const char *out_path = "out/lumitrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 6.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# lumitrax.c · %g BPM · %d bars · %.1fs · F♯ major phasing bells\n", BPMV, TB, totalSec);

    // the lead motif — a four-note rising cell in F# major pentatonic that the
    // peak section foregrounds over the phasing wash, {phrase-bar, beat, note}.
    double LEAD[8][3] = { {0,0,66},{0,2,68},{1,0,70},{1,2,73},{2,0,75},{2,2,73},{3,0,70},{3,1.5,68} };

    // a per-bar pulsing pentatonic bass — the steady trance heartbeat that runs
    // under (almost) every section so the pulse never stops.
    #define PULSE(t0, root, g, wide) do { \
        for (int e = 0; e < 8; e++) { double tt = (t0) + swing_beats(e * 0.5); int on = (e % 2 == 0); \
            bell(busL, busR, (root) - 12, tt, BEAT * 0.45, on ? (g) : (g) * 0.5, on ? -0.06 : ((wide) ? 0.26 : 0.10), 2.0, on ? 12.0 : 15.0); } \
        softsine(busL, busR, (root) - 12, (t0), BAR * 0.95, (g) * 0.9, 0); \
    } while (0)

    // ── SEED — one cell wakes, lone pulse, pad halo. The pattern stated plainly. ──
    { int idx = sec_index("seed"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * nb - 0.05, 0.085);
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; if (b >= 1) PULSE(tb, F_ROOTS[0], 0.075, 0); }
        // a single 6-note cell, mid register, entering on bar 1
        cell(t0 + BAR, t0 + BAR * nb, PENT, 5, 6, 0, 0.08, -0.18, 4.0, 4.5, 0);
        // a lone droplet near the close as a promise of the sparkle to come
        droplet(DROPS[2], t0 + BAR * (nb - 1) + 2.5 * BEAT, 0.045, 0.4);
    }

    // ── WEAVE — second cell joins (coprime 7-length), the phasing starts. ──
    { int idx = sec_index("weave"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        pad_chord(t0, V_PAD, 4, BAR * nb - 0.05, 0.10);
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; PULSE(tb, F_ROOTS[1], 0.085, 0); }
        cell(t0, t0 + BAR * nb, PENT, 5, 6, 0, 0.075, -0.22, 4.0, 4.5, 0);          // cell 1
        cell(t0, t0 + BAR * nb, PENTH, 5, 7, 2, 0.060, 0.24, 6.0, 6.0, 0);          // cell 2, brighter, drifts
        // a rising pentatonic flourish into the first full cell section
        for (int s = 0; s < 8; s++) bell(busL, busR, PENT[s % 5] + (s / 5) * 12, t0 + BAR * (nb - 1) + s * SX, SX * 3, 0.05 + 0.04 * s / 8.0, rnd2() * 0.4, 5.0, 8.0);
    }

    // ── the three "cell" sections — full interlocking phasing webs. ──
    // cellA: two cells + canon copy. thinA strips back. cellB/cellC add a third
    // high cell and droplets so the texture thickens as the chords turn.
    const char *cells[3] = { "cellA", "cellB", "cellC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(cells[d]); int c = START[wi], nbw = SECBARS[wi]; int big = (d >= 1);
        double t0 = c * BAR, t1 = (c + nbw) * BAR;
        // harmony turns once per 4-bar phrase across this section
        for (int p = 0; p * 4 < nbw; p++) {
            double tp = t0 + p * 4 * BAR; const int *pad = PADS[(START[wi] / 4 + p) % 4];
            pad_chord(tp, pad, 4, BAR * 4 - 0.05, big ? 0.085 : 0.07);
        }
        // the steady pulse, bass root follows the phrase chord
        for (int b = 0; b < nbw; b++) { double tb = (c + b) * BAR; int root = F_ROOTS[((START[wi] / 4) + (b / 4)) % 4]; PULSE(tb, root, big ? 0.085 : 0.075, b % 2); }
        // cell 1 — the foundation arpeggio (6-cell, mid)
        cell(t0, t1, PENT, 5, 6, 0, 0.072, -0.24, 4.0, 4.5, 0);
        // cell 2 — canon copy of cell 1, offset by 3 steps and panned opposite:
        // two identical patterns sliding past each other (true phase canon).
        cell(t0, t1, PENT, 5, 6, 3, 0.058, 0.24, 4.0, 5.0, 0);
        // cell 3 — a coprime 7-cell up an octave, the shimmering high lattice
        cell(t0, t1, PENTH, 5, 7, big ? 1 : 4, big ? 0.058 : 0.045, big ? -0.30 : 0.30, 6.0, 6.5, 0);
        if (big) {
            // cell 4 — a 5-cell (aligns with the scale) way up top for sparkle,
            // entering only on the thicker sections
            cell(t0, t1, DROPS, 6, 5, 0, 0.030, 0.0, 5.0, 9.0, 0);
            // scattered droplets riding the grid
            for (int b = 0; b < nbw; b++) if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], (c + b) * BAR + swing_beats((int)(rnd() * 8) * 0.5), 0.04, rnd2() * 0.7);
            // a soft choir-ish sine bed for depth on the biggest cell section
            if (d == 2) for (int b = 0; b < nbw; b += 2) { double tb = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z] + 12, tb, BAR * 2, 0.035, (z - 1) * 0.4); }
        }
    }

    // ── THIN — a clearing: pulse + one bare cell + lone droplets. Breath. ──
    { int idx = sec_index("thinA"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * nb - 0.05, 0.10);
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; PULSE(tb, F_ROOTS[2], 0.07, 0); }
        cell(t0, t0 + BAR * nb, PENT, 5, 6, 0, 0.075, 0.0, 4.0, 4.5, 0);
        for (int b = 0; b < nb; b++) if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 2.5 * BEAT, 0.04, rnd2() * 0.7);
        // a descending run that hands off into cellB
        for (int s = 0; s < 8; s++) bell(busL, busR, PENTH[(4 - s % 5 + 5) % 5] + 12 - (s / 5) * 12, t0 + BAR * (nb - 1) + s * SX, SX * 3, 0.045 + 0.03 * (8 - s) / 8.0, rnd2() * 0.4, 5.0, 8.0);
    }

    // ── PEAK — the lead motif rides above the full phasing web; everything on. ──
    { int idx = sec_index("peak"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR, t1 = (c + nb) * BAR;
        for (int p = 0; p * 4 < nb; p++) { double tp = t0 + p * 4 * BAR; pad_chord(tp, PADS[p % 4], 4, BAR * 4 - 0.05, 0.09); }
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; int root = F_ROOTS[(b / 4) % 4]; PULSE(tb, root, 0.09, b % 2); }
        // the full phase lattice
        cell(t0, t1, PENT, 5, 6, 0, 0.070, -0.26, 4.0, 4.5, 0);
        cell(t0, t1, PENT, 5, 6, 3, 0.056, 0.26, 4.0, 5.0, 0);
        cell(t0, t1, PENTH, 5, 7, 1, 0.058, -0.30, 6.0, 6.5, 0);
        cell(t0, t1, DROPS, 6, 5, 2, 0.030, 0.30, 5.0, 9.0, 0);
        // the bright lead motif, one 4-bar phrase repeated, frilled on highs
        for (int b = 0; b < nb; b++) {
            int ph = b % 4; double tb = (c + b) * BAR;
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == ph) {
                double note = LEAD[z][2], tt = tb + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 0.9, 0.11, rnd2() * 0.3, 6.0, 6.0);
                bell(revL, revR, note, tt, BEAT * 0.9, 0.05, 0, 6.0, 6.0);
                if (note >= 73) for (int q = 0; q < 3; q++) bell(busL, busR, note + q * 2, tt + 0.04 + 0.05 * q, BEAT * 0.5, 0.04 * (1.0 - 0.3 * q), rnd2() * 0.5, 5.0, 11.0);
            }
        }
    }

    // ── FADE — voices subtract one by one; pulse thins, one cell lingers, pad wash. ──
    { int idx = sec_index("fade"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR, t1 = (c + nb) * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.10);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.085);
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; double e = fmax(0.12, 1.0 - (double)b / nb);
            // pulse fades and decimates toward silence
            if (rnd() < 0.4 + 0.6 * e) PULSE(tb, b < h ? F_ROOTS[0] : F_ROOTS[3], 0.075 * e, 0); }
        // the foundation cell lingers but fades across the section (manual ramp)
        for (int b = 0; b < nb; b++) { double tb = (c + b) * BAR; double e = fmax(0.0, 1.0 - (double)b / (nb - 1));
            cell(tb, tb + BAR, PENT, 5, 6, (b * 8) % 6, 0.075 * e, (b % 2 ? 0.2 : -0.2), 4.0, 4.5, 0); }
        // the high 7-cell drops out at the midpoint (subtraction)
        cell(t0, t0 + BAR * h, PENTH, 5, 7, 1, 0.050, 0.28, 6.0, 6.5, 0);
        // a final low bell + high droplet to close the loop
        bell(busL, busR, 42, t1 - BAR * 2, 4.5, 0.17, 0, 2.0, 1.1);
        bell(busL, busR, 78, t1 - BAR * 2, 4.5, 0.09, 0.2, 6.0, 1.4);
        bell(revL, revR, 66, t1 - BAR * 2, 4.5, 0.09, 0, 5.0, 1.3);
    }

    // ── REVERB (Schroeder) — wide and shimmery to glue the lattice into a wash ──
    {
        double decay = 0.85, wet = 0.50, damp = 0.30;
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
