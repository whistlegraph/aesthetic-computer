// frosttrax.c — a sine-bell sibling of dewtrax/sinetrax. Same instrument (FM
// sine bells + soft sine drones + Schroeder reverb), new song: B minor, brisk
// (132 BPM) but almost still. The character is COLD and BRITTLE — tiny
// ultra-bright bells and icy droplets (ratios 6.0–8.0) scattered over near-
// silence and a thin high drone, with long shimmering reverb tails. Very
// sparse, lots of empty air, fast bright transients. No samples, no drums, no
// vocals — pure sine. render-c.mjs --engine frosttrax masters it on the
// sine-family chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o frosttrax frosttrax.c -lm
// Run:    ./frosttrax --out out/frosttrax-raw.wav

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
static double BPMV = 132, BEAT, BAR, SX;

static uint32_t rng_s = 0x66726f73; // "fros"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// Almost straight (0.50) — frost doesn't sway, it hangs. A whisper of swing
// (0.52) keeps the scattered droplets from sounding mechanical, but the feel
// is icy-still. On-beats hold, the "&"s drag a hair late; 16ths interpolate.
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
// FM sine bell — carrier + a decaying modulator gives the glassy partial
// shimmer; exp amp decay. `ratio` shapes the timbre. Frost lives high (ratios
// 6.0–8.0 = brittle, ultra-bright plinks); the brighter attack transient is
// dialed up so onsets crack like ice.
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.0008 * SR);              // razor-sharp, brittle onset
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 6.5 * exp(-tt * 14.0);       // very bright, glassy attack crack
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — a tiny, very bright, fast-decaying bell up high; the icy "sleet"
// sparkle. feeds both the dry bus and (generously) the reverb tail so each
// droplet leaves a long shimmering wake — the signature of frosttrax.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.50, g, pan, 7.0, 19.0);
    bell(revL, revR, note, t, 0.70, g * 0.75, 0, 7.0, 14.0);   // big reverb send → long tail
}

// soft sine drone — pad / thin high drone bed with a slow raised-cosine
// attack/release. Frost's bed is THIN and HIGH (fewer low harmonics) — a glassy
// sheet of cold air rather than a warm wash.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.55 * SR), rel = (long)(0.70 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + 0.10 * sin(ph * 2) + 0.04 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a 4-voice sine pad chord (widely panned for a cold, glassy width), with a
// soft reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.8; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.45, 0); }
}

// ── harmony — B minor, a i–VI–III–VII loop (cold, suspended) ────────────────
// per-bar bass roots that the loop walks: Bm, G, D, A — a quietly drifting
// natural-minor progression that never resolves hard (frost hangs).
static const int B_ROOTS[4] = { 59, 55, 50, 57 };       // B3, G3, D3, A3
// four 4-note pad voicings, one per chord — voiced wide and high with added
// 9ths/maj7s for an icy, suspended shimmer (not warm triads).
static const int i_PAD[4]   = { 47, 54, 59, 66 };       // Bm add9   (B F# B C#)
static const int VI_PAD[4]  = { 43, 50, 59, 62 };       // Gmaj7     (G D B D)
static const int III_PAD[4] = { 38, 50, 57, 62 };       // Dmaj add6 (D D A D... -> D A B D)
static const int VII_PAD[4] = { 45, 52, 59, 64 };       // Asus/9    (A E B E)
static const int *PADS[4] = { i_PAD, VI_PAD, III_PAD, VII_PAD };

// B minor pentatonic (B D E F# A), low octave — for arps + droplet picks.
static const int PENT[5] = { 47, 50, 52, 54, 57 };
// the high droplet palette — icy, way up top (F#5 A5 B5 C#6 E6 F#6).
static const int DROPS[6] = { 78, 81, 83, 85, 88, 90 };

// section map. SECBARS drives every loop bound below, so retuning the form
// (or the whole length) is a one-line edit — at 132 BPM a bar is ~1.818s, so
// this 62-bar form lands at ~119s (incl. tail). Keep wave/outro counts
// multiples of 4 so the 4-bar phrases (pad loop, lead) line up.
static const char *ORDER[8] = { "rime", "thaw", "driftA", "stillA", "driftB", "stillB", "driftC", "outro" };
static const int SECBARS[8] = { 6, 4, 8, 6, 8, 6, 8, 16 };  // sum = 62
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// an icicle stab — root + 5th + octave, ultra-bright (high ratio) and short;
// frost's sparse "chime" on a downbeat. Even sparser/brighter than dewtrax.
static void stab(double t, int root, double g, int triad) {
    bell(busL, busR, root, t, BEAT * 1.2, g, -0.2, 8.0, 7.0);
    bell(busL, busR, root + 7, t + 0.010, BEAT * 1.2, g * 0.65, 0.22, 8.0, 7.0);
    if (triad) bell(busL, busR, root + 12, t + 0.020, BEAT * 1.0, g * 0.45, 0.0, 7.0, 8.0);
    bell(revL, revR, root + 7, t, BEAT * 1.4, g * 0.5, 0, 8.0, 6.0);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// frill — a quick grace-run of n bells stepping by `step` semitones across
// `span` seconds (an icy spray for high points / pickups).
static void frill(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) bell(busL, busR, base + i * step, t + span * i / n, span * 1.5, g * (1.0 - 0.35 * i / n), rnd2() * 0.6, 7.0, 16.0);
}
// triplet — three quick bells over one beat (middle lifted a 4th), a brittle
// glint on phrase starts.
static void triplet(double t, double note, double g) {
    for (int i = 0; i < 3; i++) bell(busL, busR, note + (i == 1 ? 5 : 0), t + BEAT * i / 3.0, BEAT / 3.0 * 1.4, g * (i == 1 ? 1.1 : 1.0), (i - 1) * 0.3, 7.0, 15.0);
}

int main(int argc, char **argv) {
    const char *out_path = "out/frosttrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 7.0; N = (long)(totalSec * SR);   // +7s for the long frost tail
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# frosttrax.c · %g BPM · %d bars · %.1fs · B minor icy sine bells\n", BPMV, TB, totalSec);

    // the lead motif — a sparse, brittle figure in B minor pentatonic that
    // climbs and hangs, leaving gaps. {phrase-bar, beat, note}. Higher and
    // airier than dewtrax's music-box line.
    double LEAD[7][3] = { {0,1,83},{0,3,86},{1,2,85},{2,0,88},{2,2.5,90},{3,1,86},{3,3.5,83} };

    // ── RIME (bare: thin high drone fades in, lone droplets, one distant bell) ──
    { int idx = sec_index("rime"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        // a single thin high drone the whole section (the "cold air"), very soft
        softsine(busL, busR, 71, t0, BAR * nb - 0.05, 0.055, -0.3);     // B5 air
        softsine(busL, busR, 78, t0 + BAR * 2, BAR * (nb - 2) - 0.05, 0.035, 0.4); // F#6 sliver, later
        // pad enters whisper-quiet halfway
        pad_chord(t0 + BAR * (nb / 2), i_PAD, 4, BAR * (nb - nb / 2) - 0.05, 0.06);
        // lone droplets, very sparse — first ice
        for (int b = 1; b < nb; b++) if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + (1.0 + rnd() * 2.0) * BEAT, 0.05, rnd2() * 0.7);
        // one distant low bell near the end to ground it
        bell(busL, busR, 35, t0 + BAR * (nb - 1), 3.5, 0.07, 0, 2.0, 1.6);
    }

    // ── THAW — a rising pentatonic spray + pad climb into the first drift ──
    { int idx = sec_index("thaw"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.075);
        pad_chord(t0 + BAR * h, VII_PAD, 4, BAR * (nb - h) - 0.05, 0.085);
        softsine(busL, busR, 71, t0, BAR * nb - 0.05, 0.05, 0.2);       // drone continues
        int arp[8] = { 47, 50, 54, 59, 62, 66, 71, 74 };               // Bm9 arp upward
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * h + swing_beats(s * 0.25); bell(busL, busR, arp[s % 8] + (s / 8) * 12, tt, SX * 2.6, 0.04 + 0.05 * s / 16.0, rnd2() * 0.5, 6.0, 11.0); }
        frill(t0 + BAR * nb - BEAT * 1.5, 78, 7, 2, BEAT * 1.4, 0.05);  // icy spray into drift A
    }

    // ── helper for the three drifts (sparse, suspended — never a hard drop) ──
    const char *drifts[3] = { "driftA", "driftB", "driftC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(drifts[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = B_ROOTS[b % 4]; int phrase = b % 4 == 0;
            // a very sparse, slow sub pulse — only beats 1 (and 3 in the big drift).
            bell(busL, busR, root - 12, t0, BEAT * 0.9, 0.085, -0.05, 2.0, 9.0);
            if (big) bell(busL, busR, root - 12, t0 + BEAT * 2, BEAT * 0.8, 0.05, 0.2, 2.0, 12.0);
            // a thin sustained sub body (one per bar, under the air)
            softsine(busL, busR, root - 12, t0, BAR * 0.92, 0.07, 0);
            // the thin high drone holds across the whole drift (cold air)
            if (phrase) softsine(busL, busR, 71, t0, BAR * 4 - 0.05, 0.04, (b / 4 % 2) ? 0.3 : -0.3);
            // an icicle stab on beat 3 (sparser than dewtrax — only some bars)
            if (b % 2 == 0) stab(t0 + BEAT * 2, root, big ? 0.15 : 0.11, big && b % 4 == 0);
            // pad sustain every 4 bars (chord follows the loop)
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.06);
            // a brittle triplet glint on each phrase-start downbeat
            if (phrase) triplet(t0 + BEAT * 0.5, root + 24, big ? 0.06 : 0.045);
            // the sparse lead motif (4-bar phrase) — high, airy, frilled on peaks
            for (int z = 0; z < 7; z++) if ((int)LEAD[z][0] == b % 4) {
                double note = LEAD[z][2], tt = t0 + swing_beats(LEAD[z][1]);
                bell(busL, busR, note, tt, BEAT * 0.7, big ? 0.10 : 0.075, rnd2() * 0.5, 7.0, 8.0);
                bell(revL, revR, note, tt, BEAT * 0.9, 0.055, 0, 7.0, 7.0);
                if (note >= 88) frill(tt + 0.03, note, big ? 4 : 3, 2, BEAT * 0.4, big ? 0.045 : 0.03);
            }
            // pentatonic plink — a lone off-beat icicle (never every bar)
            if (b % 3 == 1) bell(busL, busR, PENT[b % 5] + 24, t0 + swing_beats(3.5), BEAT * 0.6, 0.05, rnd2() * 0.6, 7.0, 9.0);
            // sleet droplets — sprinkled into the empty air, denser in the big drift
            int ndrops = big ? 3 : 2;
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.55) droplet(DROPS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 8) * 0.5), 0.04, rnd2() * 0.8);
            // a descending icy spray at the big drift's phrase ends (the peak)
            if (big && b % 4 == 3) frill(t0 + BEAT * 2.5, 92, 6, -3, BEAT * 1.0, 0.05);
        }
        // a glassy sine sheet shimmering through the biggest drift (high, in reverb)
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { i_PAD[1] + 12, i_PAD[2] + 12, i_PAD[3] + 12 }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z], t0, BAR * 2, 0.03, (z - 1) * 0.5); }
    }

    // ── STILLS — spacious: thin drone + half-time sub + lone bells + droplets ──
    const char *still[2] = { "stillA", "stillB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(still[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, III_PAD, 4, BAR * h - 0.05, 0.08);
        pad_chord(t0 + BAR * h, VII_PAD, 4, BAR * (nb - h) - 0.05, 0.08);
        softsine(busL, busR, 71, t0, BAR * nb - 0.05, 0.045, (z2 ? 0.3 : -0.3)); // thin air holds
        for (int b = 0; b < nb; b++) {
            softsine(busL, busR, b < h ? 38 : 45, t0 + b * BAR, BAR * 0.82, 0.07, 0);            // half-time sub
            if (b % 2 == 0) bell(busL, busR, i_PAD[1 + (b / 2) % 3] + 12, t0 + b * BAR, BAR * 0.85, 0.085, rnd2() * 0.5, 6.0, 3.2); // a lone hanging bell
            if (rnd() < 0.6) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + (2.0 + rnd()) * BEAT, 0.04, rnd2() * 0.8);
        }
        if (z2 == 1) for (int s = 0; s < 16; s++) bell(busL, busR, 59 + PENT[s % 5] - 47, t0 + BAR * (nb - 2) + s * SX, SX * 3.2, 0.035 + 0.045 * s / 16.0, rnd2() * 0.6, 6.0, 6.0); // rising spray into drift C
    }

    // ── OUTRO — slowly decaying bells + thinning pad + a final low bell, long tail ──
    { int idx = sec_index("outro"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, i_PAD, 4, BAR * h - 0.05, 0.075);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.06);
        softsine(busL, busR, 71, t0, BAR * (nb - 2) - 0.05, 0.04, 0); // air thins out
        for (int b = 0; b < nb; b++) {
            double e = fmax(0.06, 1.0 - (double)b / nb);                   // everything fades over the outro
            int root = b < h ? 59 : 50;
            softsine(busL, busR, root - 12, t0 + b * BAR, BAR * 0.88, 0.07 * e, 0);
            if (b % 2 == 0) stab(t0 + b * BAR + BEAT * 2, root, 0.10 * e, 0);
            if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + (1.0 + rnd() * 2.0) * BEAT, 0.035 * e, rnd2() * 0.8);
        }
        // a final grounding pair of bells, deep into long reverb
        bell(busL, busR, 35, t0 + BAR * (nb - 3), 5.0, 0.16, 0, 2.0, 1.1);
        bell(busL, busR, 71, t0 + BAR * (nb - 3), 5.0, 0.08, 0.2, 7.0, 1.3);
        bell(revL, revR, 59, t0 + BAR * (nb - 3), 5.0, 0.10, 0, 6.0, 1.2);
        droplet(90, t0 + BAR * (nb - 2), 0.05, 0.3);                       // one last icicle
    }

    // ── REVERB (Schroeder) — wetter, longer, brighter for the icy frost space.
    // Less damping than dewtrax (frost keeps its high shimmer), longer decay,
    // higher wet → those long shimmering tails the character calls for.
    {
        double decay = 0.86, wet = 0.62, damp = 0.22;
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
    long fin = (long)(3.0 * SR), fout = (long)(5.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
