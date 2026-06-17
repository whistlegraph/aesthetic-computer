// dewtrax.c — a sine-bell sibling of sinetrax. Same instrument (FM sine bells
// + soft sine drones + Schroeder reverb), new song: D♭ major, slower (96 BPM),
// a light lilt instead of sinetrax's lurch. The character is dreamier and
// nocturnal — a music-box lead over warm pad washes, with bright "droplet"
// bells sprinkled like dew. No samples, no drums, no vocals — pure sine.
// render-c.mjs --engine dewtrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -o dewtrax dewtrax.c -lm
// Run:    ./dewtrax --out out/dewtrax-raw.wav

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
static double BPMV = 96, BEAT, BAR, SX;

static uint32_t rng_s = 0x64657721; // "dew!"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// A light lilt here (0.56) — dewtrax sways, it doesn't lurch. On-beats hold,
// the "&"s drag a touch late; 16ths interpolate inside the swung 8th.
static double SWING = 0.56;
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

// droplet — a tiny, very bright, fast-decaying bell up high; the "dew" sparkle.
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

// ── harmony — D♭ major, a I–vi–IV–V loop (warm, glassy) ─────────────────────
// per-bar bass roots that the loop walks: Db, Bbm, Gb, Ab.
static const int D_ROOTS[4] = { 61, 58, 54, 56 };       // Db4, Bb3, Gb3, Ab3
// four 4-note pad voicings, one per chord in the loop.
static const int I_PAD[4]  = { 37, 44, 48, 53 };        // Db maj7  (Db Ab C F)
static const int VI_PAD[4] = { 34, 41, 44, 49 };        // Bb m     (Bb F Ab Db)
static const int IV_PAD[4] = { 30, 37, 41, 46 };        // Gb maj   (Gb Db F Bb)
static const int V_PAD[4]  = { 32, 39, 43, 48 };        // Ab maj   (Ab Eb G C)
static const int *PADS[4] = { I_PAD, VI_PAD, IV_PAD, V_PAD };

// Db major pentatonic (Db Eb F Ab Bb), low octave — for arps + droplet picks.
static const int PENT[5] = { 49, 51, 53, 56, 58 };
// the high droplet palette (Db5 Eb5 F5 Ab5 Bb5 Db6).
static const int DROPS[6] = { 73, 75, 77, 80, 82, 85 };

// section map. SECBARS drives every loop bound below, so retuning the form
// (or the whole length) is a one-line edit — at 96 BPM a bar is 2.5s, so this
// 48-bar form lands at ~2 minutes. Keep wave/outro counts multiples of 4 so
// the 4-bar phrases (pad loop, lead) line up.
static const char *ORDER[8] = { "intro", "bloom", "waveA", "hushA", "waveB", "hushB", "waveC", "outro" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 8, 8 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a music-box stab — root + 5th + octave, bright (high ratio) and short.
static void stab(double t, int root, double g, int triad) {
    bell(busL, busR, root, t, BEAT * 1.4, g, -0.15, 7.0, 5.0);
    bell(busL, busR, root + 7, t + 0.012, BEAT * 1.4, g * 0.7, 0.18, 7.0, 5.0);
    if (triad) bell(busL, busR, root + 12, t + 0.024, BEAT * 1.2, g * 0.5, 0.0, 6.0, 6.0);
    bell(revL, revR, root + 7, t, BEAT * 1.4, g * 0.4, 0, 7.0, 5.0);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// frill — a quick grace-run of n bells stepping by `step` semitones across
// `span` seconds (a sparkly flourish for high points / pickups).
static void frill(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.4, g * (1.0 - 0.35 * i / n), rnd2() * 0.5, 5.0, 13.0);
}
// triplet — three quick bells over one beat (middle lifted a 3rd), a gentle
// lift on phrase starts.
static void triplet(double t, double note, double g) {
    for (int i = 0; i < 3; i++) bell(busL, busR, note + (i == 1 ? 4 : 0), t + BEAT * i / 3.0, BEAT / 3.0 * 1.3, g * (i == 1 ? 1.1 : 1.0), (i - 1) * 0.25, 6.0, 12.0);
}

int main(int argc, char **argv) {
    const char *out_path = "out/dewtrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 6.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# dewtrax.c · %g BPM · %d bars · %.1fs · D♭ major sine bells\n", BPMV, TB, totalSec);

    // the music-box lead, as {phrase-bar, beat, note} — a falling-then-rising
    // motif in Db major across the 4-bar phrase.
    double LEAD[8][3] = { {0,0,80},{0,2,77},{1,0,75},{1,1.5,73},{2,0,77},{2,2,80},{3,0,82},{3,2,85} };

    // ── INTRO (staggered: pad → bell melody → sub → high droplets) ──
    { int idx = sec_index("intro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.10);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.11);
        int mel[4] = { 85, 80, 77, 73 };                       // descending music-box melody, from bar 2
        for (int b = 0; b < 4 && 2 + b < nb; b++) bell(busL, busR, mel[b], t0 + (2 + b) * BAR, BAR * 0.9, 0.13, rnd2() * 0.3, 6.0, 3.0);
        for (int b = h; b < nb; b++) softsine(busL, busR, 37, t0 + b * BAR, BAR * 0.95, 0.12, 0); // sub through the back half
        for (int b = nb - 3; b < nb; b++) if (b >= 0) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 2.5 * BEAT, 0.05, rnd2() * 0.6); // first dew
    }

    // ── BLOOM — rising bell arpeggio + pad climb into the first wave ──
    { int idx = sec_index("bloom"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.13);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.15);
        int arp[8] = { 49, 53, 56, 61, 65, 68, 73, 77 };
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * h + swing_beats(s * 0.25); bell(busL, busR, arp[s % 8] + (s / 8) * 12, tt, SX * 2.4, 0.06 + 0.07 * s / 16.0, rnd2() * 0.4, 5.0, 9.0); }
        frill(t0 + BAR * nb - BEAT * 2, 73, 8, 2, BEAT * 1.8, 0.07);   // ascending flourish into wave A
    }

    // ── helper for the three waves (gentle, flowing — not a hard drop) ──
    const char *waves[3] = { "waveA", "waveB", "waveC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(waves[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = D_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // a soft pulsing bass — beats 1 & 3, the "&"s lighter & panned wide.
            for (int e = 0; e < 8; e++) { double tt = t0 + swing_beats(e * 0.5); int on = (e % 2 == 0); if (on || rnd() < 0.5) bell(busL, busR, root - 12, tt, BEAT * 0.5, on ? 0.10 : 0.055, on ? -0.08 : 0.24, 2.0, on ? 11.0 : 14.0); }
            // sustained sub body (one per bar, under the pulse)
            softsine(busL, busR, root - 12, t0, BAR * 0.95, 0.10, 0);
            // the music-box stab on beat 3
            stab(t0 + BEAT * 2, root, big ? 0.18 : 0.14, big);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.07);
            // a gentle triplet lift on each phrase-start downbeat
            if (phrase) triplet(t0, root + 12, big ? 0.08 : 0.06);
            // the music-box lead (4-bar phrase) — swung, bright, frilled on highs
            for (int z = 0; z < 8; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 0.8, big ? 0.12 : 0.09, rnd2() * 0.4, 6.0, 7.0);
                bell(revL, revR, note, tt, BEAT * 0.8, 0.05, 0, 6.0, 7.0);
                if (note >= 80) frill(tt + 0.04, note, big ? 4 : 3, 2, BEAT * 0.45, big ? 0.055 : 0.04);
            }
            // pentatonic plink — swung off-beat sparkle
            if (b % 2 == 0) bell(busL, busR, PENT[b % 5] + 12 + (big ? 12 : 0), t0 + swing_beats(2.5), BEAT * 0.7, 0.055, rnd2() * 0.5, 5.0, 8.0);
            // dew droplets — sprinkled, denser in the big wave
            int ndrops = big ? 3 : 1;
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.55) droplet(DROPS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 8) * 0.5), 0.045, rnd2() * 0.7);
            // descending frill run at phrase ends of the big wave (high point)
            if (big && b % 4 == 3) frill(t0 + BEAT * 3, 87, 6, -2, BEAT * 0.9, 0.055);
        }
        // a soft choir-ish sine bed through the biggest wave
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z] + 12, t0, BAR * 2, 0.04, (z - 1) * 0.4); }
    }

    // ── HUSHES — spacious: pad + half-time sub + sparse bells + lone droplets ──
    const char *hush[2] = { "hushA", "hushB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(hush[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, IV_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, V_PAD, 4, BAR * (nb - h) - 0.05, 0.12);
        for (int b = 0; b < nb; b++) { softsine(busL, busR, b < h ? 30 : 32, t0 + b * BAR, BAR * 0.85, 0.10, 0); if (b % 2 == 0) bell(busL, busR, I_PAD[1 + (b / 2) % 3], t0 + b * BAR, BAR * 0.9, 0.11, rnd2() * 0.4, 5.0, 2.6); if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 2.5 * BEAT, 0.04, rnd2() * 0.7); }
        if (z2 == 1) for (int s = 0; s < 16; s++) bell(busL, busR, 61 + PENT[s % 5] - 49, t0 + BAR * (nb - 2) + s * SX, SX * 3, 0.04 + 0.05 * s / 16.0, rnd2() * 0.5, 5.0, 5.0); // rising run into wave C
    }

    // ── OUTRO — decaying bells + pad wash + a final low bell ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, IV_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        for (int b = 0; b < nb; b++) { double e = fmax(0.1, 1.0 - (double)b / nb); int root = b < h ? 61 : 56; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.10 * e, 0); if (b % 2 == 0) stab(t0 + b * BAR + BEAT * 2, root, 0.11 * e, 0); if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 1.5 * BEAT, 0.035 * e, rnd2() * 0.7); }
        bell(busL, busR, 37, t0 + BAR * (nb - 2), 4.5, 0.18, 0, 2.0, 1.2);
        bell(busL, busR, 73, t0 + BAR * (nb - 2), 4.5, 0.10, 0.2, 6.0, 1.5);
        bell(revL, revR, 61, t0 + BAR * (nb - 2), 4.5, 0.10, 0, 5.0, 1.4);
    }

    // ── REVERB (Schroeder) — a touch wetter/longer for the dreamy bell space ──
    {
        double decay = 0.82, wet = 0.55, damp = 0.38;
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
    long fin = (long)(2.5 * SR), fout = (long)(4.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
