// sinetrax.c — a sine-bell-only fork of maytrax. Same 88-bar F-minor form,
// same section map (intro → build → drop → break → … → outro), but every
// voice is a pure FM sine bell or a soft sine drone. No samples, no drums,
// no vocals — pure instrumental, meditative. Schroeder reverb glues the
// bells into a wide glassy space; render-c.mjs --sine masters it.
//
// Build:  cc -O3 -std=c11 -o sinetrax sinetrax.c -lm
// Run:    ./sinetrax --out out/sine-raw.wav

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
static double BPMV = 160, BEAT, BAR, SX;

static uint32_t rng_s = 0x73696e65; // "sine"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// SWING = the fraction of the beat where the off-8th lands. 0.5 is straight,
// 0.667 is a triplet shuffle, >0.70 is an extreme lurch/sway. swing_beats()
// maps a straight beat-position to its swung time: on-beats hold, the "&"s
// are dragged late.
static double SWING = 0.72;
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
// shimmer; exp amp decay. `ratio` shapes the timbre (3.5 = bell, 2.0 = softer).
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

// a 3-voice sine pad chord (slightly detuned for width).
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.6; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.4, 0); }
}

// ── harmony (shared with maytrax) ──────────────────────────────────────────
static const int FM_ROOTS[4] = { 53, 51, 49, 48 };
static const int FM_PAD[4] = { 41, 53, 56, 60 };
static const int EB_PAD[4] = { 39, 51, 54, 58 };
static const int DB_PAD[4] = { 37, 49, 52, 56 };
static const int C_PAD[4]  = { 36, 48, 51, 55 };

static const char *ORDER[8] = { "intro", "buildA", "drop1", "breakA", "drop2", "breakB", "drop3", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 8, 16, 12 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a melodic bell stab — root + 5th + octave, tight + punctuated now.
static void stab(double t, int root, double g, int triad) {
    bell(busL, busR, root, t, BEAT * 1.2, g, -0.15, 3.5, 6.5);
    bell(busL, busR, root + 7, t + 0.010, BEAT * 1.2, g * 0.7, 0.18, 3.5, 6.5);
    if (triad) bell(busL, busR, root + 12, t + 0.020, BEAT * 1.1, g * 0.5, 0.0, 3.0, 7.5);
    bell(revL, revR, root + 7, t, BEAT * 1.2, g * 0.4, 0, 3.5, 6.5);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// frill — a quick grace-run of n bells stepping by `step` semitones across
// `span` seconds (a sparkly flourish for high points / pickups).
static void frill(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.4, g * (1.0 - 0.35 * i / n), rnd2() * 0.5, 3.5, 13.0);
}
// triplet — three quick bells over one beat (middle lifted a 3rd), for
// accents on the downbeats / climaxes.
static void triplet(double t, double note, double g) {
    for (int i = 0; i < 3; i++) bell(busL, busR, note + (i == 1 ? 3 : 0), t + BEAT * i / 3.0, BEAT / 3.0 * 1.2, g * (i == 1 ? 1.1 : 1.0), (i - 1) * 0.25, 3.5, 12.0);
}

int main(int argc, char **argv) {
    const char *out_path = "out/sine-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 6.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# sinetrax.c · %g BPM · %d bars · %.1fs · sine bells\n", BPMV, TB, totalSec);

    // the lead melody (shared with maytrax), as pluck bells.
    double LEAD[8][3] = { {0,0,65},{0,2,72},{1,0,75},{1,1,72},{1,2,68},{2,0,65},{2,2,68},{3,0,72} };
    int pent[8] = { 53, 56, 58, 60, 63, 60, 58, 56 };

    // ── INTRO (staggered: pad → bell melody → sub → high tinges) ──
    { int c = START[sec_index("intro")]; double t0 = c * BAR;
        pad_chord(t0, FM_PAD, 4, BAR * 4 - 0.05, 0.10);
        pad_chord(t0 + BAR * 4, EB_PAD, 4, BAR * 4 - 0.05, 0.11);
        int mel[4] = { 77, 72, 68, 65 };                       // bar 2: descending bell melody
        for (int b = 0; b < 4; b++) bell(busL, busR, mel[b], t0 + (2 + b) * BAR, BAR * 0.9, 0.14, rnd2() * 0.3, 3.0, 3.0);
        for (int b = 4; b < 8; b++) softsine(busL, busR, 29, t0 + b * BAR, BAR * 0.95, 0.12, 0); // bar 4: sub
        for (int b = 6; b < 8; b++) bell(busL, busR, 84 + (b & 1) * 3, t0 + b * BAR + 2 * BEAT, BEAT, 0.05, rnd2() * 0.6, 4.0, 6.0); // bar 6: high tinge
    }

    // ── BUILD A — rising bell arpeggio + pad climb ──
    { int c = START[sec_index("buildA")]; double t0 = c * BAR;
        pad_chord(t0, DB_PAD, 4, BAR * 2 - 0.05, 0.13);
        pad_chord(t0 + BAR * 2, C_PAD, 4, BAR * 2 - 0.05, 0.15);
        int arp[8] = { 48, 53, 56, 60, 63, 68, 72, 75 };
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * 2 + swing_beats(s * 0.25); bell(busL, busR, arp[s % 8] + (s / 8) * 12, tt, SX * 2.2, 0.06 + 0.07 * s / 16.0, rnd2() * 0.4, 3.0, 9.0); }
        frill(t0 + BAR * 3 + BEAT * 2, 72, 8, 2, BEAT * 1.8, 0.07);   // ascending frill flourish into the drop
    }

    // ── helper for the three drops ──
    const char *drops[3] = { "drop1", "drop2", "drop3" };
    for (int d = 0; d < 3; d++) {
        int c = START[sec_index(drops[d])]; int big = d == 2;
        for (int b = 0; b < 16; b++) {
            double t0 = (c + b) * BAR; int root = FM_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // swung 8th pulse — on-beats firm, the "&"s dragged late + softer.
            for (int e = 0; e < 8; e++) { double tt = t0 + swing_beats(e * 0.5); int on = (e % 2 == 0); bell(busL, busR, root - 12, tt, BEAT * 0.4, on ? 0.11 : 0.07, on ? -0.08 : 0.22, 2.0, on ? 13.0 : 16.0); }
            // sustained sub body (one per bar, under the swing)
            softsine(busL, busR, root - 12, t0, BAR * 0.95, 0.10, 0);
            // the stab walk on beat 3
            stab(t0 + BEAT * 2, root, big ? 0.20 : 0.16, big);
            // pad sustain every 4 bars
            if (phrase) pad_chord(t0, FM_PAD, 4, BAR * 4 - 0.05, 0.07);
            // accent ornament — a triplet burst on the phrase-start downbeat
            if (phrase) triplet(t0, root + 12, big ? 0.09 : 0.07);
            // lead pluck bells (4-bar phrase) — swung, sharp, with a frill on highs
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 0.7, big ? 0.13 : 0.10, rnd2() * 0.4, 3.0, 8.0);
                bell(revL, revR, note, tt, BEAT * 0.7, 0.05, 0, 3.0, 8.0);
                if (note >= 72) frill(tt + swing_beats(0.5) - swing_beats(0), note, big ? 4 : 3, 2, BEAT * 0.4, big ? 0.06 : 0.045); // frill on the high points
            }
            // 303 bell — swung off-beat, sharp plink
            if (b % 2 == 0) bell(busL, busR, pent[b % 8] + (big ? 12 : 0), t0 + swing_beats(2.5), BEAT * 0.6, 0.06, rnd2() * 0.5, 3.5, 8.0);
            // descending frill run at each phrase end of the big drop (high point)
            if (big && b % 4 == 3) frill(t0 + BEAT * 3, 86, 6, -2, BEAT * 0.9, 0.06);
            // high tinge sprinkle (sharp)
            if (rnd() < 0.35) bell(busL, busR, 84 + (int)(rnd() * 5), t0 + swing_beats((int)(rnd() * 8) * 0.5), BEAT * 0.5, 0.04, rnd2() * 0.7, 4.0, 11.0);
        }
        // choir-ish sine bed through the biggest drop
        if (big) for (int b = 0; b < 16; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { FM_PAD[1], FM_PAD[2], FM_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z] + 12, t0, BAR * 2, 0.04, (z - 1) * 0.4); }
    }

    // ── BREAKS — spacious: pad + sub half-time + sparse bells ──
    const char *breaks[2] = { "breakA", "breakB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int c = START[sec_index(breaks[z2])]; double t0 = c * BAR;
        pad_chord(t0, DB_PAD, 4, BAR * 4 - 0.05, 0.12);
        pad_chord(t0 + BAR * 4, C_PAD, 4, BAR * 4 - 0.05, 0.12);
        for (int b = 0; b < 8; b++) { softsine(busL, busR, b < 4 ? 25 : 24, t0 + b * BAR, BAR * 0.85, 0.10, 0); if (b % 2 == 0) bell(busL, busR, FM_PAD[1 + (b / 2) % 3], t0 + b * BAR, BAR * 0.9, 0.12, rnd2() * 0.4, 3.0, 2.6); }
        if (z2 == 1) for (int s = 0; s < 16; s++) bell(busL, busR, 60 + (s % 8) * 2, t0 + BAR * 6 + s * SX, SX * 3, 0.04 + 0.05 * s / 16.0, rnd2() * 0.5, 3.0, 5.0); // rising bell run into drop3
    }

    // ── OUTRO — decaying bells + pad wash + final low bell ──
    { int c = START[sec_index("outro")]; double t0 = c * BAR;
        pad_chord(t0, FM_PAD, 4, BAR * 6 - 0.05, 0.11);
        pad_chord(t0 + BAR * 6, C_PAD, 4, BAR * 6 - 0.05, 0.10);
        for (int b = 0; b < 12; b++) { double e = fmax(0.1, 1.0 - b * 0.08); int root = b < 6 ? 53 : 48; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.10 * e, 0); if (b % 2 == 0) stab(t0 + b * BAR + BEAT * 2, root, 0.12 * e, 0); }
        bell(busL, busR, 36, t0 + BAR * 10, 4.0, 0.18, 0, 2.0, 1.4);
        bell(busL, busR, 72, t0 + BAR * 10, 4.0, 0.10, 0.2, 3.5, 1.6);
        bell(revL, revR, 60, t0 + BAR * 10, 4.0, 0.10, 0, 3.0, 1.5);
    }

    // ── REVERB (Schroeder) — a touch wetter for the glassy bell space ──
    {
        double decay = 0.78, wet = 0.5, damp = 0.40;
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
    long fin = (long)(2.0 * SR), fout = (long)(4.0 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
