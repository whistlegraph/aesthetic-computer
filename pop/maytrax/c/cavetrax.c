// cavetrax.c — a sine-bell sibling of dewtrax, carved into a vast stone hollow.
// Same instrument (FM sine bells + soft sine drones + Schroeder reverb), new
// song: A natural minor, very slow (72 BPM), cavernous and immense. The
// character is subterranean — deep, wide low bells whose tails ring for a long
// time in a huge cave, a heavy sub bed beneath, and far-off high glints that
// answer from somewhere up in the dark. The reverb is cranked wet and long so
// every strike blooms into the stone. No samples, no drums, no vocals —
// pure sine.  render-c.mjs --engine cavetrax masters it on the sine chain.
//
// Build:  cc -O3 -std=c11 -o cavetrax cavetrax.c -lm
// Run:    ./cavetrax --out out/cavetrax-raw.wav

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
static double BPMV = 72, BEAT, BAR, SX;

static uint32_t rng_s = 0x63617665; // "cave"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// Almost no swing here (0.50) — in a cave things fall on the beat and the room
// does the rest. A whisper of drag keeps it from feeling mechanical.
static double SWING = 0.51;
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
// shimmer; exp amp decay. Low `dec` values give the long cavernous ring this
// track lives on. `ratio` shapes the timbre (1.5–2.0 = round body, 3.5 = bell,
// 5–7 = bright far-off glint).
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.004 * SR);               // a softer, rounder onset for stone
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 3.2 * exp(-tt * 7.0);        // gentler attack transient — less plink, more bloom
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — a tiny, very bright, fast-decaying bell up high; here it's a far-off
// glint glimmering somewhere up in the dark. Feeds the dry bus quietly and the
// reverb tail more strongly, so it lives mostly in the echo.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.9, g * 0.6, pan, 5.0, 9.0);
    bell(revL, revR, note, t, 0.9, g * 0.9, 0, 5.0, 9.0);
}

// soft sine drone — pad / sub bed with a slow raised-cosine attack/release.
// Longer attack/release here than dewtrax so the bed swells like cave air.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.55 * SR), rel = (long)(0.80 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.18 * sin(ph * 2) + 0.07 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (widely panned for cavern width), with a generous
// reverb send so the chord smears into the stone.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.8; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.55, 0); }
}

// ── harmony — A natural minor, a i–VI–III–VII descent (deep, modal) ─────────
// per-bar bass roots that the loop walks low: A, F, C, G.
static const int A_ROOTS[4] = { 45, 41, 48, 43 };       // A2, F2, C3, G2
// four 4-note pad voicings, one per chord in the loop — open, rooted low.
static const int I_PAD[4]   = { 33, 40, 45, 52 };       // A m    (A E A E … A2 E3 A3 E4)
static const int VI_PAD[4]  = { 29, 36, 41, 48 };       // F maj  (F C F C)
static const int III_PAD[4] = { 36, 43, 48, 55 };       // C maj  (C G C G)
static const int VII_PAD[4] = { 31, 38, 43, 50 };       // G maj  (G D G D)
static const int *PADS[4] = { I_PAD, VI_PAD, III_PAD, VII_PAD };

// A natural-minor pentatonic-ish picks (A C D E G), low — for sparse glints.
static const int PENT[5] = { 45, 48, 50, 52, 55 };
// the far-off high glint palette (A5 C6 D6 E6 G6 A6) — high and quiet, answers
// the deep bells from across the cave.
static const int DROPS[6] = { 81, 84, 86, 88, 91, 93 };

// section map. SECBARS drives every loop bound below — at 72 BPM a bar is
// 3.333s, so this 36-bar form lands at ~2 minutes. Keep wave/outro counts
// multiples of 4 so the 4-bar phrases (pad loop, deep-bell motif) line up.
static const char *ORDER[8] = { "mouth", "descent", "vaultA", "stillA", "vaultB", "stillB", "vaultC", "outro" };
static const int SECBARS[8] = { 4, 4, 8, 4, 4, 4, 4, 4 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// a deep cave-bell toll — root + 5th below + octave, round (low ratio) and very
// long ring. This is the signature voice: wide, sub-heavy, slow to fade.
static void toll(double t, int root, double g, int big) {
    bell(busL, busR, root, t, 7.0, g, -0.30, 1.5, big ? 0.55 : 0.75);
    bell(busL, busR, root + 7, t + 0.04, 6.0, g * 0.6, 0.30, 2.0, 0.85);
    if (big) bell(busL, busR, root - 12, t + 0.02, 8.0, g * 0.7, 0.0, 1.5, 0.45);
    bell(revL, revR, root, t, 7.0, g * 0.7, 0, 1.5, 0.7);
    bell(revL, revR, root + 7, t + 0.04, 6.0, g * 0.45, 0, 2.0, 0.8);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// distant call — a slow falling answer of n bells stepping by `step` semitones
// across `span` seconds, way up high and quiet (lives in the reverb).
static void distant_call(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) {
        double note = base + i * step, tt = t + span * i / n;
        bell(busL, busR, note, tt, span * 1.2, g * (1.0 - 0.30 * i / n) * 0.5, rnd2() * 0.7, 5.0, 7.0);
        bell(revL, revR, note, tt, span * 1.2, g * (1.0 - 0.30 * i / n), 0, 5.0, 7.0);
    }
}

int main(int argc, char **argv) {
    const char *out_path = "out/cavetrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 8.0; N = (long)(totalSec * SR);   // longer tail for the big cave
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# cavetrax.c · %g BPM · %d bars · %.1fs · A minor cavern bells\n", BPMV, TB, totalSec);

    // the deep-bell motif, as {phrase-bar, beat, note} — a slow descent that
    // walks down through A minor across the 4-bar phrase, low and spacious.
    double LEAD[6][3] = { {0,0,57},{1,0,53},{1,2,52},{2,0,48},{3,0,50},{3,2,45} };  // E A G  E  D  A (low)

    // ── MOUTH — the entrance: first a low drone swells, then a single far toll ──
    { int idx = sec_index("mouth"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * nb - 0.05, 0.11);
        softsine(busL, busR, 21, t0 + BAR * 0.5, BAR * (nb - 0.5), 0.13, 0);   // deepest sub swell (A0)
        toll(t0 + BAR * 2, 45, 0.20, 1);                                       // the first deep toll
        for (int b = nb - 2; b < nb; b++) if (b >= 0 && rnd() < 0.7) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 3.0 * BEAT, 0.05, rnd2() * 0.8);
    }

    // ── DESCENT — pad climbs down, deep bells begin to walk, glints answer ──
    { int idx = sec_index("descent"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.13);
        pad_chord(t0 + BAR * h, VII_PAD, 4, BAR * (nb - h) - 0.05, 0.13);
        int walk[4] = { 52, 50, 48, 45 };                                      // E D C A, one per bar
        for (int b = 0; b < 4 && b < nb; b++) { bell(busL, busR, walk[b], t0 + b * BAR + BEAT, 6.0, 0.16, rnd2() * 0.4, 1.7, 0.7); bell(revL, revR, walk[b], t0 + b * BAR + BEAT, 6.0, 0.09, 0, 1.7, 0.7); softsine(busL, busR, walk[b] - 24, t0 + b * BAR, BAR * 0.95, 0.12, 0); }
        distant_call(t0 + BAR * nb - BEAT * 3, 88, 5, -3, BEAT * 2.6, 0.06);   // a high call falling into vault A
    }

    // ── helper for the three vaults (the open central space — never a drop) ──
    const char *vaults[3] = { "vaultA", "vaultB", "vaultC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(vaults[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = A_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // sustained sub body — the heavy floor of the cave, one per bar.
            softsine(busL, busR, root - 24, t0, BAR * 0.98, 0.13, 0);
            softsine(busL, busR, root - 12, t0, BAR * 0.95, 0.07, rnd2() * 0.2);
            // the deep toll on beat 1 (half-time feel — strong, sparse, ringing).
            toll(t0, root, big ? 0.19 : 0.15, big || phrase);
            // a second, lighter toll on beat 3 only in the big vault.
            if (big) toll(t0 + BEAT * 2, root + 12, 0.11, 0);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.085);
            // the deep-bell descent motif (4-bar phrase) — low, with a reverb twin.
            for (int z = 0; z < 6; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, 5.5, big ? 0.15 : 0.12, rnd2() * 0.3, 1.8, 0.85);
                bell(revL, revR, note, tt, 5.5, 0.09, 0, 1.8, 0.85);
            }
            // a lone low pentatonic ring mid-bar — sparse, wide.
            if (b % 2 == 1) { double tt = t0 + swing_beats(2.0); bell(busL, busR, PENT[(b * 2) % 5], tt, 5.0, 0.08, rnd2() * 0.6, 1.7, 0.9); bell(revL, revR, PENT[(b * 2) % 5], tt, 5.0, 0.05, 0, 1.7, 0.9); }
            // far-off glints — sprinkled, denser in the big vault but always faint.
            int ndrops = big ? 2 : 1;
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 4) * 1.0), 0.05, rnd2() * 0.85);
            // a slow distant call at phrase ends of the big vault (high point).
            if (big && b % 4 == 3) distant_call(t0 + BEAT * 2, 91, 5, -3, BEAT * 1.8, 0.055);
        }
        // a far choir-ish sine bed through the biggest vault, deep in the reverb.
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { I_PAD[1], I_PAD[2], I_PAD[3] }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z], t0, BAR * 2, 0.045, (z - 1) * 0.5); }
    }

    // ── STILLS — the most spacious: pad + lone sub + a single far toll + glint ──
    const char *still[2] = { "stillA", "stillB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(still[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, III_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.12);
        for (int b = 0; b < nb; b++) {
            softsine(busL, busR, b < h ? 24 : 29, t0 + b * BAR, BAR * 0.92, 0.12, 0);   // lone deep sub
            if (b % 2 == 0) toll(t0 + b * BAR + BEAT, I_PAD[1] + (b / 2 % 2 ? 7 : 0), 0.13, 0);
            if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 3.0 * BEAT, 0.045, rnd2() * 0.85);
        }
        if (z2 == 1) distant_call(t0 + BAR * (nb - 2) + BEAT, 93, 6, -3, BEAT * 4.0, 0.06);  // long call into vault C
    }

    // ── OUTRO — the tolls thin out, the drone sinks, one final deep ring ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, I_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        for (int b = 0; b < nb; b++) { double e = fmax(0.1, 1.0 - (double)b / nb); int root = b < h ? 41 : 45; softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.9, 0.12 * e, 0); if (b % 2 == 0) toll(t0 + b * BAR + BEAT, root, 0.12 * e, 0); if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 2.0 * BEAT, 0.035 * e, rnd2() * 0.85); }
        // the last deep bell of the cave — long, full, fading into the stone.
        bell(busL, busR, 33, t0 + BAR * (nb - 2), 8.0, 0.22, 0, 1.5, 0.40);
        bell(busL, busR, 40, t0 + BAR * (nb - 2) + 0.05, 7.0, 0.12, 0.2, 2.0, 0.55);
        bell(busL, busR, 21, t0 + BAR * (nb - 2), 9.0, 0.16, 0, 1.5, 0.30);
        bell(revL, revR, 45, t0 + BAR * (nb - 2), 8.0, 0.12, 0, 1.5, 0.45);
    }

    // ── REVERB (Schroeder) — cranked WAY wet and long: the huge stone cave. ──
    // longer comb delays + higher decay/wet, low damping so the tail stays
    // bright and rings for a long time.
    {
        double decay = 0.90, wet = 0.78, damp = 0.22;
        int CD[6]; double cds[6] = { 0.0437, 0.0531, 0.0617, 0.0683, 0.0742, 0.0827 };
        for (int k = 0; k < 6; k++) CD[k] = (int)(cds[k] * SR);
        int AD[2] = { (int)(0.0071 * SR), (int)(0.0026 * SR) }; double apFb = 0.55;
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

    // ── normalize + gentle in/out fades (long fade-out for the cave tail) ──
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.85 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(3.5 * SR), fout = (long)(6.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
