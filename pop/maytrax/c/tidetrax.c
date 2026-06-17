// tidetrax.c — an oceanic sine-bell sibling of dewtrax. Same instrument family
// (FM sine bells + soft sine drones + Schroeder reverb), a wholly new song:
// C minor, 100 BPM, no swing-lurch — instead a slow breathing tide. The
// character is the ebb and flow of the sea: big swelling pad waves that crest
// and recede, with call-and-response mid-register bells answering across the
// stereo field. Long pad attacks/releases, dynamics that breathe in and out.
// No samples, no drums, no vocals — pure sine.
// render-c.mjs --engine tidetrax masters it on the sine-family chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o tidetrax tidetrax.c -lm
// Run:    ./tidetrax --out out/tidetrax-raw.wav

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
static double BPMV = 100, BEAT, BAR, SX;

static uint32_t rng_s = 0x74696465; // "tide"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── swing / sway ────────────────────────────────────────────────────────────
// tidetrax barely swings (0.50, near-straight) — the breathing comes from the
// long pad envelopes and the tide LFO, not from a swung subdivision.
static double SWING = 0.50;
static double swing_beats(double beats) {
    double e8 = beats * 2.0;                 // position in 8ths
    long idx = (long)floor(e8 + 1e-9);
    double frac = e8 - idx;
    double pos = (idx / 2) * BEAT + ((idx & 1) ? SWING * BEAT : 0.0);
    return pos + frac * (BEAT / 2.0);        // interpolate 16ths inside the 8th
}

// ── tide LFO — a very slow swell that the whole mix breathes with ───────────
// One full ebb-and-flow every 8 bars; pads & bells scale their gain by it so
// the dynamics crest and recede like surf. period ~ 8 bars.
static double tide(double t) {
    double period = BAR * 8.0;
    return 0.55 + 0.45 * (0.5 - 0.5 * cos(TAU * t / period)); // 0.10 .. 1.00, raised-cos
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
// shimmer; exp amp decay. `ratio` shapes the timbre. tidetrax's bells use a
// softer, slower onset than dewtrax's plinks — they bloom into the wash.
static void bell(float *L, float *R, double note, double t, double dur, double g, double pan, double ratio, double dec) {
    double fc = midi_hz(note), fm = fc * ratio;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double phc = 0, phm = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.010 * SR);               // softer, blooming onset (vs dew's 1.2ms)
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * dec); if (i < att) env *= (double)i / att;
        double mi = 3.5 * exp(-tt * 7.0);        // gentler attack transient — rounder bell
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        double v = sin(phc) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// droplet — a tiny, bright, fast-decaying bell up high; sea-spray sparkle off
// the crest of a wave. feeds dry + (softly) the reverb tail for shimmer.
static void droplet(double note, double t, double g, double pan) {
    bell(busL, busR, note, t, 0.65, g, pan, 4.5, 13.0);
    bell(revL, revR, note, t, 0.65, g * 0.6, 0, 4.5, 13.0);
}

// soft sine drone — pad / sub bed with a LONG raised-cosine attack/release so
// each chord swells in and ebbs out like a wave rolling onto the shore.
static void softsine(float *L, float *R, double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.85 * SR), rel = (long)(1.20 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    if (att > n / 2) att = n / 2; if (rel > n / 2) rel = n / 2;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = 0.5 - 0.5 * cos(M_PI * (double)i / att); else if (i > n - rel) env = fmax(0, 0.5 - 0.5 * cos(M_PI * (double)(n - i) / rel));
        double v = (sin(ph) + 0.18 * sin(ph * 2) + 0.07 * sin(ph * 3)) * env * g;
        adds(L, R, s0 + i, v * lg, v * rg);
    }
}

// a wide sine pad chord — voices spread across the full stereo field for the
// oceanic width, with a generous reverb send.
static void pad_chord(double t, const int *midis, int nm, double dur, double g) {
    for (int k = 0; k < nm; k++) { double pan = (k / fmax(1, nm - 1) - 0.5) * 0.85; softsine(busL, busR, midis[k], t, dur, g, pan); softsine(revL, revR, midis[k], t, dur, g * 0.5, pan * 0.6); }
}

// ── harmony — C minor, a swelling i–VI–III–VII ocean loop ───────────────────
// per-bar bass roots that the tide walks: Cm, Ab, Eb, Bb.
static const int C_ROOTS[4] = { 60, 56, 51, 58 };       // C4, Ab3, Eb3, Bb3
// four 4-note pad voicings, one per chord in the loop (low, wide).
static const int I_PAD[4]   = { 36, 43, 48, 51 };       // Cm   (C G C Eb)
static const int VI_PAD[4]  = { 32, 39, 44, 48 };       // Ab   (Ab Eb Ab C)
static const int III_PAD[4] = { 39, 46, 51, 55 };       // Eb   (Eb Bb Eb G)
static const int VII_PAD[4] = { 34, 41, 46, 50 };       // Bb   (Bb F Bb D)
static const int *PADS[4] = { I_PAD, VI_PAD, III_PAD, VII_PAD };

// C natural-minor pentatonic (C Eb F G Bb), mid octave — for arps + bells.
static const int PENT[5] = { 48, 51, 53, 55, 58 };
// the high sea-spray droplet palette (C5 Eb5 F5 G5 Bb5 C6).
static const int DROPS[6] = { 72, 75, 77, 79, 82, 84 };
// the mid-register CALL palette (left side) and RESPONSE palette (right side)
// answering across the stereo field — both in C minor pentatonic, an octave
// apart so the answer rings brighter, higher.
static const int CALL[4] = { 60, 63, 67, 70 };          // C4 Eb4 G4 Bb4
static const int RESP[4] = { 67, 70, 72, 75 };          // G4 Bb4 C5 Eb5

// section map. SECBARS drives every loop bound below. At 100 BPM a bar is 2.4s,
// so this 48-bar form lands at ~115s + reverb tail. Keep wave/outro counts
// multiples of 4 so the 4-bar phrases (pad loop, tide swell) line up.
static const char *ORDER[8] = { "horizon", "swell", "crestA", "ebbA", "crestB", "ebbB", "crestC", "shore" };
static const int SECBARS[8] = { 4, 4, 8, 4, 8, 4, 8, 8 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }

// ── call-and-response — a mid bell on the LEFT (call) answered on the RIGHT
// (response), the answer a beat-and-a-half later and a touch quieter. This is
// the signature gesture of the track. ratio 3.0 = warm round bell.
static void answer(double t, int call_note, int resp_note, double g, double tideg) {
    bell(busL, busR, call_note, t, BEAT * 2.2, g * tideg, -0.55, 3.0, 3.2);
    bell(revL, revR, call_note, t, BEAT * 2.2, g * 0.45 * tideg, -0.4, 3.0, 3.2);
    double tr = t + BEAT * 1.5;
    bell(busL, busR, resp_note, tr, BEAT * 2.0, g * 0.78 * tideg, 0.58, 3.5, 3.5);
    bell(revL, revR, resp_note, tr, BEAT * 0.45 * 2.0, g * 0.40 * tideg, 0.42, 3.5, 3.5);
}

// ── ornaments ───────────────────────────────────────────────────────────────
// spray — a quick run of n bright bells fanning outward across the stereo
// field (sea spray off a crest). pans alternate L/R for a scattering feel.
static void spray(double t, double base, int n, int step, double span, double g) {
    for (int i = 0; i < n; i++) { double pan = (i & 1 ? 1 : -1) * (0.3 + 0.5 * rnd()); bell(busL, busR, base + i * step, t + span * i / n, span * 1.5, g * (1.0 - 0.3 * i / n), pan, 4.5, 11.0); }
}

int main(int argc, char **argv) {
    const char *out_path = "out/tidetrax-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 7.0; N = (long)(totalSec * SR);
    busL = calloc(N, 4); busR = calloc(N, 4); revL = calloc(N, 4); revR = calloc(N, 4);
    fprintf(stderr, "# tidetrax.c · %g BPM · %d bars · %.1fs · C minor oceanic sine bells\n", BPMV, TB, totalSec);

    // ── HORIZON — a single low pad swelling up from silence, distant spray ──
    { int idx = sec_index("horizon"); int c = START[idx], nb = SECBARS[idx]; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * nb - 0.05, 0.11);                       // one long swell across the whole intro
        softsine(busL, busR, 24, t0 + BAR, BAR * (nb - 1) * 0.95, 0.10, 0);   // deep sub tide rolling under
        // a few far-off droplets near the back, faint
        for (int b = nb - 3; b < nb; b++) if (b >= 0) droplet(DROPS[(int)(rnd() * 6)], t0 + b * BAR + 3.0 * BEAT, 0.035 * tide(t0 + b * BAR), rnd2() * 0.8);
        // one first faint call-and-response, far out, to seed the motif
        answer(t0 + BAR * (nb - 1), CALL[0], RESP[0], 0.06, tide(t0 + BAR * (nb - 1)));
    }

    // ── SWELL — the water gathers: pad climb + rising pentatonic arp ──
    { int idx = sec_index("swell"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.13);
        pad_chord(t0 + BAR * h, III_PAD, 4, BAR * (nb - h) - 0.05, 0.15);
        // a slowly-rising pentatonic arpeggio drifting up the stereo field
        int arp[8] = { 48, 51, 53, 55, 58, 60, 63, 67 };
        for (int s = 0; s < 16; s++) { double tt = t0 + BAR * h + swing_beats(s * 0.5); double tg = tide(tt); bell(busL, busR, arp[s % 8] + (s / 8) * 12, tt, BEAT * 1.6, (0.05 + 0.06 * s / 16.0) * tg, (s & 1 ? 1 : -1) * 0.45, 3.5, 6.0); }
        // call-and-response begins to answer
        answer(t0 + BAR * 1, CALL[1], RESP[1], 0.10, tide(t0 + BAR));
        answer(t0 + BAR * 2.5, CALL[2], RESP[2], 0.10, tide(t0 + BAR * 2.5));
        spray(t0 + BAR * nb - BEAT * 2, 67, 6, 2, BEAT * 1.8, 0.06);          // spray into the first crest
    }

    // ── helper for the three crests (the tide LFO does the dynamics) ──
    const char *crests[3] = { "crestA", "crestB", "crestC" };
    for (int d = 0; d < 3; d++) {
        int wi = sec_index(crests[d]); int c = START[wi], nbw = SECBARS[wi]; int big = d == 2;
        for (int b = 0; b < nbw; b++) {
            double t0 = (c + b) * BAR; int root = C_ROOTS[b % 4]; int phrase = b % 4 == 0;
            double tg = tide(t0);                                              // this bar's place in the swell
            // a rolling sub bass — long soft notes per bar that breathe with the tide
            softsine(busL, busR, root - 12, t0, BAR * 0.98, (0.09 + 0.05 * tg), 0);
            // pad sustain every 4 bars (chord follows the ocean loop), swelling with the tide
            if (phrase) pad_chord(t0, PADS[(b / 4) % 4], 4, BAR * 4 - 0.05, 0.06 + 0.04 * tg);
            // the signature call-and-response — left calls on beat 1, right answers,
            // walking up the CALL/RESP palettes through the phrase.
            int ci = (b % 4);
            answer(t0, CALL[ci], RESP[ci], big ? 0.13 : 0.10, tg);
            // a second softer answer on the back half of the bar in the big crest
            if (big) answer(t0 + BEAT * 2, RESP[ci], CALL[(ci + 1) % 4] + 12, 0.07, tide(t0 + BEAT * 2));
            // pentatonic plinks riding the crest — denser at high tide
            int nplinks = (tg > 0.7) ? 2 : 1;
            for (int k = 0; k < nplinks; k++) if (rnd() < 0.6) bell(busL, busR, PENT[(int)(rnd() * 5)] + 12, t0 + swing_beats((int)(rnd() * 8) * 0.5), BEAT * 0.9, 0.05 * tg, rnd2() * 0.7, 3.5, 6.0);
            // sea-spray droplets, more at the crest of the wave
            int ndrops = big ? 3 : (tg > 0.6 ? 2 : 1);
            for (int k = 0; k < ndrops; k++) if (rnd() < 0.55) droplet(DROPS[(int)(rnd() * 6)], t0 + swing_beats((int)(rnd() * 8) * 0.5), 0.04 * tg, rnd2() * 0.8);
            // spray fan at phrase ends of the big crest (the wave breaking)
            if (big && b % 4 == 3) spray(t0 + BEAT * 2.5, 79, 6, -2, BEAT * 1.1, 0.06);
        }
        // a high choir-ish sine bed shimmering over the biggest crest
        if (big) for (int b = 0; b < nbw; b += 2) { double t0 = (c + b) * BAR; int ns[3] = { III_PAD[1] + 12, III_PAD[2] + 12, III_PAD[3] + 12 }; for (int z = 0; z < 3; z++) softsine(revL, revR, ns[z], t0, BAR * 2, 0.035, (z - 1) * 0.6); }
    }

    // ── EBBS — the tide recedes: spacious pad + half-time sub + lone answers ──
    const char *ebb[2] = { "ebbA", "ebbB" };
    for (int z2 = 0; z2 < 2; z2++) {
        int hi = sec_index(ebb[z2]); int c = START[hi], nb = SECBARS[hi], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, VI_PAD, 4, BAR * h - 0.05, 0.12);
        pad_chord(t0 + BAR * h, VII_PAD, 4, BAR * (nb - h) - 0.05, 0.12);
        for (int b = 0; b < nb; b++) {
            double tb = t0 + b * BAR; double tg = tide(tb);
            softsine(busL, busR, b < h ? 32 : 34, tb, BAR * 0.9, 0.09, 0);    // half-time sub
            // sparse, lone call-and-response drifting in the open space
            if (b % 2 == 0) answer(tb, CALL[(b / 2) % 4], RESP[(b / 2) % 4], 0.09, tg);
            if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], tb + 2.5 * BEAT, 0.035 * tg, rnd2() * 0.8);
        }
        if (z2 == 1) spray(t0 + BAR * (nb - 2), 60, 8, 2, BEAT * 3.0, 0.05);  // rising spray into crest C
    }

    // ── SHORE (outro) — the last wave recedes: decaying answers, pad wash,
    // a final low bell rolling out to sea ──
    { int idx = sec_index("shore"); int c = START[idx], nb = SECBARS[idx], h = nb / 2; double t0 = c * BAR;
        pad_chord(t0, I_PAD, 4, BAR * h - 0.05, 0.11);
        pad_chord(t0 + BAR * h, VI_PAD, 4, BAR * (nb - h) - 0.05, 0.10);
        for (int b = 0; b < nb; b++) {
            double tb = t0 + b * BAR; double e = fmax(0.1, 1.0 - (double)b / nb);
            int root = b < h ? 60 : 56;
            softsine(busL, busR, root - 24, tb, BAR * 0.9, 0.10 * e, 0);
            if (b % 2 == 0) answer(tb + BEAT * 0.5, CALL[b % 4], RESP[b % 4], 0.09 * e, 0.8);
            if (rnd() < 0.5) droplet(DROPS[(int)(rnd() * 6)], tb + 1.5 * BEAT, 0.03 * e, rnd2() * 0.8);
        }
        // a final low bell + high shimmer ringing out as the tide goes still
        bell(busL, busR, 36, t0 + BAR * (nb - 2), 5.0, 0.18, 0, 2.0, 1.0);
        bell(busL, busR, 60, t0 + BAR * (nb - 2), 5.0, 0.10, 0.3, 3.5, 1.3);
        bell(revL, revR, 67, t0 + BAR * (nb - 2), 5.0, 0.10, -0.3, 3.5, 1.2);
    }

    // ── REVERB (Schroeder) — big, wet, oceanic space: longer decay, wetter
    // mix, darker damping than dewtrax for that cavernous sea-cave wash ──
    {
        double decay = 0.87, wet = 0.62, damp = 0.45;
        int CD[6]; double cds[6] = { 0.0311, 0.0389, 0.0431, 0.0461, 0.0523, 0.0613 };
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

    // ── normalize + long gentle in/out fades (tide arriving + receding) ──
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.85 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(3.5 * SR), fout = (long)(5.5 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fin); busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = 0.5 - 0.5 * cos(M_PI * (double)i / fout); long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
