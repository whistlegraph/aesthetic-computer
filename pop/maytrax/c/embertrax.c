// embertrax.c — a sine-bell sibling of dewtrax. Same instrument (FM sine bells
// + soft sine drones + Schroeder reverb), new song: G minor, mid-tempo
// (108 BPM), a warm amber glow. Where dewtrax is bright/nocturnal, embertrax
// sits lower and rounder: warm low-mid bells over a steady gentle pulse,
// patiently swelling from a single ember to a full glow and easing back down.
// Bell ratios stay in the rounded 2.5–4.0 zone (no glassy plink), decays run
// long, the reverb is warm and damped. No samples, no drums, no vocals — pure
// sine. render-c.mjs --engine embertrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o embertrax embertrax.c -lm
// Run:    ./embertrax --out out/embertrax-raw.wav

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
static double BPMV = 108, BEAT, BAR, SX;

static uint32_t rng_s = 0x656d6272; // "embr"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// A barely-there sway (0.53) — embertrax breathes, it almost doesn't swing.
// On-beats hold; the "&"s drag a hair late; 16ths interpolate inside the 8th.
static double SWING = 0.53;
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
// FM sine bell — carrier + a decaying modulator gives the warm partial body;
// exp amp decay. `ratio` shapes the timbre. embertrax lives in the rounded
// 2.5–4.0 zone: 2.5 = soft body, 3.0 = warm bell, 4.0 = the brightest it gets.
// A softer attack transient (3.0, slower decay) keeps onsets rounded, not glassy.
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.004 * SR);               // softer, rounder onset
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 3.0 * exp(-tt * 7.0);        // warmer, gentler attack transient
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// ember — a small, warm, mid-register bell that glows rather than sparkles;
// rounded ratio (3.0), medium decay. fed into the dry bus + softly the reverb.
static void ember(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 1.1, g, pan, 3.0, 6.0);
    bell(revL, revR, note, t, 1.1, g * 0.55, 0, 3.0, 6.0);
}

// soft sine drone — pad / sub bed with a slow raised-cosine attack/release.
// embertrax leans on the lower harmonics for a fuller, warmer bed.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.40 * SR), rel = (long)(0.55 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.22 * sin(ph * 2) + 0.09 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (slightly panned for width), with a soft reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.55; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.45, 0); }
}

// ── harmony — G minor, a i–VI–III–v loop (warm, glowing) ────────────────────
// per-bar bass roots that the loop walks: Gm, Eb, Bb, Dm.
static const int G_ROOTS[4] = { 55, 51, 58, 50 };       // G3, Eb3, Bb3, D3
// four 4-note pad voicings, one per chord in the loop (low-mid, close + warm).
static const int I_PAD[4]   = { 31, 38, 43, 46 };       // Gm     (G D Bb D)
static const int VI_PAD[4]  = { 27, 34, 39, 43 };       // Eb maj (Eb Bb G Bb)
static const int III_PAD[4] = { 34, 41, 46, 50 };       // Bb maj (Bb F D F)
static const int V_PAD[4]   = { 26, 33, 38, 41 };       // Dm     (D A F A)
static const int *PADS[4] = { I_PAD, VI_PAD, III_PAD, V_PAD };

// G minor pentatonic (G Bb C D F), low-mid octave — for arps + ember picks.
static const int PENT[5] = { 43, 46, 48, 50, 53 };
// the warm ember palette (G4 Bb4 C5 D5 F5 G5) — lower than dewtrax's droplets.
static const int EMBERS[6] = { 67, 70, 72, 74, 77, 79 };

// section map. SECBARS drives every loop bound below, so retuning the form
// (or the whole length) is a one-line edit — at 108 BPM a bar is ~2.22s, so
// this 52-bar form lands at ~115s. Keep glow/outro counts multiples of 4 so
// the 4-bar phrases (pad loop, lead) line up. The form is a patient swell:
// ember → kindle → three glows of growing warmth → settles → outro.
static const char *ORDER[8] = { "ember", "kindle", "glowA", "settleA", "glowB", "settleB", "glowC", "outro" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 8, 12 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a warm chord stab — root + 5th + octave, rounded (low ratio) and singing.
static void stab(double t, int root, double g, int triad) {
    bell(busL, busR, root, t, BEAT * 2.0, g, -0.12, 3.0, 3.2);
    bell(busL, busR, root + 7, t + 0.018, BEAT * 2.0, g * 0.7, 0.15, 3.0, 3.2);
    if (triad) bell(busL, busR, root + 12, t + 0.036, BEAT * 1.8, g * 0.5, 0.0, 3.5, 3.6);
    bell(revL, revR, root + 7, t, BEAT * 2.0, g * 0.45, 0, 3.0, 3.2);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// warmrun — a slow warm-bell run of n bells stepping by `step` semitones across
// `span` seconds (a glowing flourish for high points / pickups). Lower & rounder
// than dewtrax's frill.
static void warmrun(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.6, g * (1.0 - 0.30 * i / n), rnd2() * 0.45, 3.0, 7.0);
}
// triplet — three warm bells over one beat (middle lifted a minor 3rd), a
// gentle lift on phrase starts.
static void triplet(double t, double note, double g) {
    for (int i = 0; i < 3; i++) bell(busL, busR, note + (i == 1 ? 3 : 0), t + BEAT * i / 3.0, BEAT / 3.0 * 1.5, g * (i == 1 ? 1.1 : 1.0), (i - 1) * 0.22, 3.0, 6.5);
}

int main(int argc, char **argv) {
    const char *out_path = "out/embertrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 7.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# embertrax.c · %g BPM · %d bars · %.1fs · G minor warm sine bells\n", BPMV, TB, totalSec);

    // the warm lead, as {phrase-bar, beat, note} — a rising-then-settling motif
    // in G minor across the 4-bar phrase, sitting in the low-mid (G4 region).
    double LEAD[8][3] = { {0,0,67},{0,2.5,70},{1,0,72},{1,2,70},{2,0,74},{2,2.5,72},{3,0,70},{3,2,67} };

    // ── EMBER (a single warm glow waking up: lone pad → one bell → sub) ──
    { int idx = sec_index("ember"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.09);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        int mel[4] = { 67, 70, 72, 70 };                       // a small rising glow, from bar 2
        for (int b = 0; b < 4 && 2 + b < nb; b++) bell(busL, busR, mel[b], t0 + (2 + b) * BAR, BAR * 1.1, 0.12, rnd2() * 0.25, 3.0, 2.4);
        for (int b = h; b < nb; b++) softsine(busL, busR, 31, t0 + b * BAR, BAR * 0.95, 0.11, 0); // sub through the back half
        for (int b = nb - 2; b < nb; b++) if (b >= 0) ember(EMBERS[(int)(rnd() * 3)], t0 + b * BAR + 2.5 * BEAT, 0.05, rnd2() * 0.5); // first embers
    }

    // ── KINDLE — a rising warm arpeggio + pad climb into the first glow ──
    { int idx = sec_index("kindle"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, III_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.13);
        int arp[8] = { 43, 46, 48, 50, 53, 55, 58, 62 };
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * h + swing_beats(s * 0.25); bell(busL, busR, arp[s % 8] + (s / 8) * 12, tt, SX * 3.0, 0.05 + 0.06 * s / 16.0, rnd2() * 0.35, 3.0, 6.0); }
        warmrun(t0 + BAR * nb - BEAT * 2, 62, 6, 2, BEAT * 2.0, 0.06);   // glowing ascent into glow A
    }

    // ── helper for the three glows (a swelling warmth, not a drop) ──
    const char *glows[3] = { "glowA", "glowB", "glowC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(glows[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = G_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // a steady gentle pulse — warm low bells on every beat, "&"s lighter.
            for (int e = 0; e < 8; e++) { double tt = t0 + swing_beats(e * 0.5); int on = (e % 2 == 0); if (on || rnd() < 0.45) bell(busL, busR, root - 12, tt, BEAT * 0.7, on ? 0.10 : 0.05, on ? -0.06 : 0.20, 2.5, on ? 8.0 : 10.0); }
            // sustained sub body (one per bar, under the pulse)
            softsine(busL, busR, root - 12, t0, BAR * 0.95, 0.11, 0);
            // the warm chord stab on beat 3 (rounded, singing)
            stab(t0 + BEAT * 2, root, big ? 0.16 : 0.12, big || d == 1);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.08 + 0.01 * d);
            // a gentle triplet lift on each phrase-start downbeat
            if (phrase) triplet(t0, root, big ? 0.07 : 0.05);
            // the warm lead (4-bar phrase) — barely swung, rounded, run on highs
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 1.2, big ? 0.11 : 0.08, rnd2() * 0.35, 3.0, 4.0);
                bell(revL, revR, note, tt, BEAT * 1.2, 0.05, 0, 3.0, 4.0);
                if (note >= 74) warmrun(tt + 0.05, note, big ? 4 : 3, 2, BEAT * 0.7, big ? 0.05 : 0.035);
            }
            // pentatonic glow — barely-swung off-beat warmth
            if (b % 2 == 0) bell(busL, busR, PENT[b % 5] + 12 + (big ? 12 : 0), t0 + swing_beats(2.5), BEAT * 1.0, 0.05, rnd2() * 0.45, 3.0, 6.0);
            // embers — sprinkled, denser & warmer in the big glow
            int nem = big ? 3 : (d == 1 ? 2 : 1);
            for (int k = 0; k < nem; k++) if (rnd() < 0.55) ember(EMBERS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 8) * 0.5), 0.04, rnd2() * 0.6);
            // descending warm run at phrase ends of the big glow (the full glow's peak)
            if (big && b % 4 == 3) warmrun(t0 + BEAT * 3, 79, 5, -2, BEAT * 1.1, 0.05);
        }
        // a soft choir-ish sine bed through the biggest glow — full warmth
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z] + 12, t0, BAR * 2, 0.045, (z - 1) * 0.38); }
    }

    // ── SETTLES — spacious: pad + half-time sub + sparse warm bells + lone embers ──
    const char *settle[2] = { "settleA", "settleB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(settle[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, III_PAD, 4, BAR * (nb - h) - 0.05, 0.11);
        for (int b = 0; b < nb; b++) { softsine(busL, busR, b < h ? 27 : 34, t0 + b * BAR, BAR * 0.85, 0.10, 0); if (b % 2 == 0) bell(busL, busR, I_PAD[1 + (b / 2) % 3], t0 + b * BAR, BAR * 1.1, 0.10, rnd2() * 0.35, 3.0, 2.2); if (rnd() < 0.6) ember(EMBERS[(int)(rnd() * 6)], t0 + b * BAR + 2.5 * BEAT, 0.035, rnd2() * 0.6); }
        if (z2 == 1) for (int s = 0; s < 16; s++) bell(busL, busR, 55 + PENT[s % 5] - 43, t0 + BAR * (nb - 2) + s * SX, SX * 3.5, 0.035 + 0.045 * s / 16.0, rnd2() * 0.45, 3.0, 4.5); // rising warm run into glow C
    }

    // ── OUTRO — the glow cooling: decaying warm bells + pad wash + a final low bell ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.10);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.09);
        for (int b = 0; b < nb; b++) { double e = fmax(0.08, 1.0 - (double)b / nb); int root = b < h ? 55 : 51; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.10 * e, 0); if (b % 2 == 0) stab(t0 + b * BAR + BEAT * 2, root, 0.10 * e, 0); if (rnd() < 0.5) ember(EMBERS[(int)(rnd() * 6)], t0 + b * BAR + 1.5 * BEAT, 0.03 * e, rnd2() * 0.6); }
        bell(busL, busR, 31, t0 + BAR * (nb - 3), 5.5, 0.17, 0, 2.5, 1.0);
        bell(busL, busR, 67, t0 + BAR * (nb - 3), 5.5, 0.09, 0.18, 3.0, 1.2);
        bell(revL, revR, 55, t0 + BAR * (nb - 3), 5.5, 0.10, 0, 3.0, 1.1);
    }

    // ── REVERB (Schroeder) — warm & damped for the amber bell space ──
    {
        double decay = 0.84, wet = 0.52, damp = 0.50;
        int CD[6]; double cds[6] = { 0.0311, 0.0383, 0.0427, 0.0451, 0.0509, 0.0593 };
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
    long fin = (long)(3.0 * SR), fout = (long)(5.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
