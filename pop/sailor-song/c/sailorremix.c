// sailorremix.c — "sailor song (outside remix)", the C engine.
//
// A cover of Gigi Perez's "Sailor Song", played by @jeffrey's niece: her
// voice and a capo-4 nylon guitar on one phone mic. This file is the score.
// It follows the /pop single-file renderer shape (pop/loner/c,
// pop/cult/c): the measuring side bakes, this engine arranges and mixes.
//
//   THE DEAL  bin/aesthetivox.py owns WORLD (her lead regulated onto G#
//             minor in her guitar's +13¢ frame, a dark octave halo, five
//             self-harmony stems); bin/lock-vox.mjs pulls every onset onto
//             her own strum 8ths; bin/replay-guitar.mjs re-plays her part
//             bar for bar on pop/guitar/c/strum; bin/bells.sh bakes a FEM
//             bell bank (pop/bell/c); bin/chart.mjs emits sailor-chart.h —
//             her bars, her snapped beats, her chords, her tuned notes.
//   FOLLOW    Nothing is warped to a grid. Every hit lands on HER strum
//             points (her motif X..XX..X: 1, &2, 3, &4). The clock breathes
//             from 110 to 125 because she does.
//   POCKET    kick 1 + &2, brush snare 3, rim &4 — her own strum points.
//   TRAP      a gliding 808 on the kick points tuned to her roots; bright
//             16th hats with 32nd / triplet rolls into every other bar
//             line; a layered clap on 3.
//   SINES     voice-led sine choir — sub on the roots, a tenor trio under
//             her, a high pair over her, her register (G#3–G#4) left clear
//             — plus a descant, her melody shadowed a 3rd and an octave up,
//             and her chorus hook re-laid over the guitar-only break.
//             Saturated and pumped by the kick so they hold in a pop mix.
//   AC        FEM bells answer her in the breath gaps (her last three
//             notes ring back); a vibraphone (pop/marimba's modal preset,
//             ported) arpeggiates her chords on her 8ths.
//   SPACE     pop/nullabye/c/ac_hrtf.h: bells orbit overhead, the vibes
//             sweep ear to ear, the highs float up and wide, and her
//             harmonies stand at fixed places — in the bridge they rove.
//   ROOM      near early reflections always on, a 2.2 s dark FDN behind.
//             Verses close and bright (air above 7 kHz), choruses roomier.
//
// Form (bars from sailor-chart.h, 2:59):
//   intro 1–10 · verse1 11–27 · chorus1 28–43 · verse2 44–51 ·
//   chorus2 52–67 · break 68–71 · bridge 72–80 · outro 81–84
//
// v6 (ANALYSIS.md, the platter reading of her take):
//   START     bedroom first. The record opens on her first strum (1.65 s),
//             her guitar and then her voice alone, dry and close — nothing
//             says electronic. From bar 16 the mix grows in one lane at a
//             time (chamber-03 rule 1: one change per phrase): sub 16, pads
//             + shaker 20, congas 22, a kick on her 1 and 3 only at 24, the
//             pickup bass 24, hats 25, riser 26, and the floor lands whole
//             with an explosion on chorus 1 (bar 28).
//   CLOCK     bin/regularize.mjs: her rubato is kept through the bedroom
//             bars; from bar 24 the clock keeps 30% of her sway around
//             120 (verse 2 119, choruses 120–121, outro 122 instead of 130).
//             Every stem and the chart go through the same time map.
//   HOCKET    her hand is the periodic necklace X..XX..X (1 &2 3 &4, LHL 0).
//             The kit now plays her COMPLEMENT .XX..XX. (&1 2 &3 4): open hats
//             on &1/&3, claps on 2/4, so every 8th is struck exactly once
//             between her and the kit (rhythm-platter 06); the floor kick
//             doubles her 1 and 3. Her pickups &2/&4 get only a soft accent
//             and the house bass — the rim that fought her anacrusis is gone
//             where she sings.
//   BINS      off-beat hits sit at her measured bin, +5 ms (rhythm 07): the
//             necklace says which pulse, BIN_OFF says where in it.
//   IMPACT    chamber-04 rule 10: at every drop the whole ring is displaced
//             and springs back (1.1 Hz, damping 1.2); kick and sub never
//             move; an explosion (a noise chiff hurled once around the head)
//             opens it.
//   VOX ARPS  her own harmony stems gated into arpeggios on her 8ths/16ths/
//             triplets, each step at its own seat — a different figure per
//             section (down3 is the resident interval, up5 the rare one:
//             chord-tone shares .60 / .35 against her chords).
//
// v8: NO DROPS. Everything below her is a sine (the beds, the sub, an
//     offbeat sine bass, the halo/harmony/choir voices) plus a minimal electro
//     kit — 808 kick, 808 clap, closed/open hats, a rim on her pickups — that
//     grows in over twelve bars and then rides hills. No guitars but a trace
//     of her own, no vibes, no bells, no hand percussion, no risers, jets,
//     explosions or impacts. Tight, not humanized.
//
// Build: bash pop/sailor-song/c/build.sh
// Run:   pop/sailor-song/c/sailorremix    (from the repo root)
//        → pop/sailor-song/out/sailor-song-v5-full.wav + .events.json
//        bash pop/sailor-song/c/cut.sh → master (−10 LUFS) + mp3 w/ cover

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "../../nullabye/c/ac_hrtf.h"
#include "sailor-chart.h"

#define SR 48000
#define VERSION "v73"
#define EAGER 0.009          // v73: "eager" placement — percussion pushes ~9 ms ahead of the grid
#define PIANO_ON 0           // v71: "let's be rid of the piano" — the sampler stays, the part is off
#define SHIFT27 0.0          // v53: how much earlier everything after bar 27 plays, now that the regularizer fits it to four beats
#define KICK_ONLY 0          // v48: "try removing all percussion other than kick" — claps, snares, hats, toms, shaker, congas, taiko off
#define END_BAR 999          // v23: "the song should end how her video ends" — the record runs to the end of her take (v9 cut at 82 + 1.5 s)
#define START_BAR 11         // v6.6: (the pre-roll is now her first WORD; see main)
#define FLOOR_BAR 52         // v10: the one chorus — the floor drops on its "kiss"
#define BIN_OFF 0.005        // v6: her off-beat bin, measured (ANALYSIS.md §2)
#define LANE "pop/sailor-song"
#define PI 3.14159265358979323846

// ── rng (seeded, so the same score renders the same bytes) ──────────────
static uint32_t rng_state = 0x5a11025u;
static double rnd(void) {   // −1..1
    rng_state ^= rng_state << 13; rng_state ^= rng_state >> 17; rng_state ^= rng_state << 5;
    return (double)rng_state / 2147483648.0 - 1.0;
}

// ── WAV IO (the fleet loader, loner/cult's, stereo) ──────────────────────
typedef struct { float *L, *R; long n; } Stereo;
static Stereo load_wav(const char *path) {
    Stereo s = { 0 };
    FILE *f = fopen(path, "rb");
    if (!f) { fprintf(stderr, "  ! missing %s\n", path); return s; }
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t *)malloc(sz);
    if (!buf || (long)fread(buf, 1, sz, f) != sz) { fclose(f); free(buf); return s; }
    fclose(f);
    long p = 12; int fmt = 1, ch = 1, bits = 16; long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        uint32_t c = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8 | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(buf + p, "fmt ", 4)) { fmt = buf[p+8] | (buf[p+9] << 8); ch = buf[p+10] | (buf[p+11] << 8); bits = buf[p+22] | (buf[p+23] << 8); }
        else if (!memcmp(buf + p, "data", 4)) { dOff = p + 8; dLen = c; if (dOff + dLen > sz) dLen = sz - dOff; break; }
        p += 8 + c + (c & 1);
    }
    const int bps = bits / 8; const long frames = dLen / (bps * ch);
    s.L = (float *)calloc(frames, sizeof(float)); s.R = (float *)calloc(frames, sizeof(float)); s.n = frames;
    for (long i = 0; i < frames; i++) for (int c = 0; c < 2; c++) {
        const long o = dOff + (i * ch + (c < ch ? c : 0)) * bps; double v;
        if (fmt == 3 && bits == 32) { float x; memcpy(&x, buf + o, 4); v = x; }
        else if (bits == 32) { int32_t x; memcpy(&x, buf + o, 4); v = x / 2147483648.0; }
        else if (bits == 24) { int32_t x = buf[o] | (buf[o+1] << 8) | ((int32_t)(int8_t)buf[o+2] << 16); v = x / 8388608.0; }
        else { int16_t x; memcpy(&x, buf + o, 2); v = x / 32768.0; }
        (c ? s.R : s.L)[i] = (float)v;
    }
    free(buf);
    return s;
}
static void write_wav_f32(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "! cannot write %s\n", path); exit(1); }
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f);
}
static float sample(const float *b, long len, long i) { return (b && i >= 0 && i < len) ? b[i] : 0.0f; }

// ── the form + the arrangement ──────────────────────────────────────────
enum { INTRO, VERSE1, CHORUS1, VERSE2, CHORUS2, BREAK, BRIDGE, OUTRO, NSEC };
static const char *SEC_NAME[NSEC] = { "intro", "verse1", "chorus1", "verse2", "chorus2", "break", "bridge", "outro" };
static const int SEC_FROM[NSEC] = { 1, 11, 28, 44, 52, 68, 73, 81 };   // v7: "kiss me on the mouth" lands on the downbeat of 28 and 52; the bridge line on 73
static int section_of(int bar) { int s = 0; for (int k = 0; k < NSEC; k++) if (bar >= SEC_FROM[k]) s = k; return s; }

// her own harmonies: which stem sings, how loud, where (azimuth °)
enum { H_UP3, H_DOWN3, H_UP5, H_DOWN6, H_DOWN8, H_UP8, NHARM };
static const char *HARM_FILE[NHARM] = { "harm-up3", "harm-down3", "harm-up5", "harm-down6", "harm-down8", "harm-up8" };
static const double HARM_DELAY[NHARM] = { 0.018, 0.026, 0.022, 0.030, 0.012, 0.008 };   // a breath apart
typedef struct { double g, az; } HarmSet;

typedef struct {
    double her, acg, elg;                        // her guitar / acoustic replay / electric replay
    double kick, snare, clap, rim, hat, trap, b808;
    double sub, tenor, high, third, octave, descant, hook, vib, bell;
    HarmSet harm[NHARM];
    double send, air;                            // vocal room send, air lift
} Arr;
#define H(i, g, az) [i] = { g, az }
static const Arr ARR[NSEC] = {
    [INTRO]   = { 0, 0, 0,      0, 0, 0, 0, 0, 0, 0,        0, 0, 0, 0, 0, 0, 0, 0, 0,      // v43: nothing of hers before she sings — the radio
                  { { 0 } }, .10, .3 },
    [VERSE1]  = { 1.1, 0, 0,     0, 0, 0, 0, 0, .25, 0,      .8, .7, .3, 0, 0, 0, 0, 0, 0,    // v8: end state of the evolution (gated per bar below)
                  { H(H_DOWN3, .16, 40) }, .07, .45 },
    [CHORUS1] = { 1.35, .7, 0,   1, 0, .8, .5, 0, .5, 1,     1, 1, 1, .3, .2, 0, 0, 0, 0,   // v22: her guitar back, strong
                  { H(H_UP3, .34, -50), H(H_DOWN3, .30, 50), H(H_DOWN6, .2, 0) }, .18, 1.0 },   // v16: harmonies up
    [VERSE2]  = { 1.15, .4, 0,  .5, 0, 0, .35, 0, .1, .4,   .7, .6, .3, 0, 0, 0, 0, 0, 0,    // a verse, not a chorus; v27: kick .5, hat .1
                  { H(H_DOWN3, .30, 45), H(H_UP3, .30, -45), H(H_DOWN6, .18, 0) }, .12, .8 },   // v19: harmonies come in with the cathedral
    [CHORUS2] = { 1.45, 1.0, .6, 1.1, 0, .9, .5, 0, .6, 1,   1, 1, 1, .3, .3, 0, 0, 0, 0,   // v49: her guitar front and centre in chorus 2 (+3 dB, the acoustic replay full)
                  { H(H_UP3, .36, -60), H(H_DOWN3, .34, 60), H(H_DOWN6, .25, -20), H(H_DOWN8, .24, 0) }, .18, 1.0 },
    [BREAK]   = { 1.2, .5, 0,    .8, 0, .4, .3, 0, .3, .6,   .8, 0, 0, 0, 0, 0, 1, 0, 0,   // v27: the break is a break — sub + hook, no tenor/high bed   // v24: no dead space — the beat stays, and rises
                  { { 0 } }, .20, .2 },
    [BRIDGE]  = { 1.2, .6, .7,   1, 0, .7, .4, 0, .6, .9,    1, 1, 1, .2, .3, 0, 0, .3, .3,
                  { { 0 } }, .06, .9 },                   // harmonies rove, see harm_at()   // v32: almost dry — the cat-and-mouse verse up close
    [OUTRO]   = { 1.3, .8, .8,    1.2, 0, 1, .5, 0, .7, 1,    1, 1, 1, .3, .3, 0, 0, .3, .4,   // v24: the FINALE
                  { H(H_UP3, .4, -60), H(H_DOWN3, .4, 60), H(H_DOWN6, .28, -20), H(H_DOWN8, .24, 0) }, .20, 1.0 },
};
// "pitching around": in the bridge her harmony changes interval and place each bar
static double evo(int bar, int from, int to);
static HarmSet harm_at(int bar, int h) {
    int s = section_of(bar);
    if (s == CHORUS1 || s == CHORUS2) { HarmSet r = ARR[s].harm[h]; r.g *= (s == CHORUS1 ? evo(bar, 28, 33) : evo(bar, 52, 57)); if (h != H_UP8) return r; }   // v67: her harmonies swell in over the chorus's first bars
    if (s == VERSE1) { HarmSet r = ARR[s].harm[h]; r.g *= evo(bar, 18, 24); return r; }   // v6: her harmony creeps in
    if (h == H_UP8) {                                                                    // v6.2: her voice an octave up
        if (bar >= 36 && bar <= 43) return (HarmSet){ .4, 0 };                           // second half of chorus 1 (v27: .85 → .4)
        if (bar >= 60 && bar <= 67) return (HarmSet){ .5, 0 };                           // v27: chorus 2's second half only (was all of it at 1.0)
        if (bar >= 77 && bar <= 80) return (HarmSet){ .5, 0 };                           // the bridge's climb
        if (bar >= 81 && bar <= 82) return (HarmSet){ .5, 0 };   // v24: finale; v27: 81–82 only
        return (HarmSet){ 0, 0 };
    }
    if (s != BRIDGE) return ARR[s].harm[h];
    static const HarmSet ROVE[4][NHARM] = {
        { H(H_UP3, .36, -60) }, { H(H_UP5, .30, 60) }, { H(H_DOWN3, .34, -35) }, { H(H_UP3, .30, 45), H(H_DOWN6, .24, -50) } };
    return ROVE[(bar - 73) % 4][h];
}

// ── buses ────────────────────────────────────────────────────────────────
static long N;
static float *dL, *dR, *tL, *tR, *b808, *sL, *sR, *hiM, *bellM, *vibM, *duck;
static float *pcM = NULL;   // v60: claps / snare / toms — a mono bus the listener hears from a seat that sways (the kick stays centre)
static float *pL, *pR;   // hand-percussion / bubble bus
static void add(float *b, long i, double v) { if (i >= 0 && i < N) b[i] += (float)v; }
static long at(double t) { return lround(t * SR); }
static double mtof(double m) { return 440.0 * pow(2.0, (m + CHART_TUNE - 69.0) / 12.0); }

// score events → out/…events.json for the score video
static FILE *EV;
static int evFirst = 1;
static void ev(double t, const char *voice, double dur, double gain, int midi) {
    if (!EV) return;
    fprintf(EV, "%s{\"t\":%.3f,\"voice\":\"%s\",\"dur\":%.3f,\"gain\":%.2f%s", evFirst ? "" : ",\n", t, voice, dur, gain, "");
    if (midi >= 0) fprintf(EV, ",\"midi\":%d", midi);
    fputc('}', EV); evFirst = 0;
}

// ── the kit ──────────────────────────────────────────────────────────────
static void kick(double t, double g) {
    long a = at(t); double ph = 0;
    for (long i = 0; i < 0.36 * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (48 + 110 * exp(-u * 45)) / SR;          // v5.1: a real pop kick —
        double click = i < 0.004 * SR ? rnd() * 0.5 * (1 - u / 0.004) : 0;   // beater click
        double v = (tanh(sin(ph) * 1.6) * exp(-u * 6.5) + click) * g; add(dL, a + i, v); add(dR, a + i, v); }
    for (long i = 0; i < 0.25 * SR; i++) add(duck, a + i, g * exp(-(double)i / SR * 9));
}
static void snare(double t, double g) {   // brushy: slow swell-in, dark noise, soft body
    long a = at(t); double prev = 0, lp = 0, ph = 0;
    for (long i = 0; i < 0.24 * SR; i++) { double u = (double)i / SR, nz = rnd(), hp = nz - prev; prev = nz;
        lp += (hp - lp) * 0.45; ph += 2 * PI * (200 + 60 * exp(-u * 60)) / SR;   // v5.1: crisp, not brushed
        double v = (lp * exp(-u * 17) * 1.4 + sin(ph) * 0.55 * exp(-u * 26)) * g;
        add(dL, a + i, v * 0.92); add(dR, a + i, v); }
}
static void bubble(double t, double g, double pan) {   // v11: a bubble pop — sine 1400 → 380 Hz in 45 ms, a 1 ms click, gone in 90 ms
    long a = at(t); double ph = 0, f0 = 1100 + rnd() * 300;
    for (long i = 0; i < 0.09 * SR; i++) { double u = (double)i / SR; ph += 2 * PI * (380 + (f0 - 380) * exp(-u * 70)) / SR;
        double v = (sin(ph) * exp(-u * 32) + (i < 0.001 * SR ? rnd() * 0.5 : 0)) * g;
        add(pL, a + i, v * (1 - pan)); add(pR, a + i, v * (1 + pan)); }
}
static void rim(double t, double g) {
    long a = at(t);
    for (long i = 0; i < 0.07 * SR; i++) { double u = (double)i / SR;
        double v = (sin(2 * PI * 1650 * u) * 0.5 + sin(2 * PI * 520 * u)) * exp(-u * 70) * g;
        add(dL, a + i, v * 1.1); add(dR, a + i, v * 0.8); }
}
static void soft_hat(double t, double g, double pan) {
    long a = at(t); double prev = 0;
    for (long i = 0; i < 0.03 * SR; i++) { double nz = rnd(), hp = nz - prev; prev = nz;
        double v = hp * exp(-(double)i / SR * 300) * g * 0.6; add(dL, a + i, v * (1 - pan)); add(dR, a + i, v * (1 + pan)); }
}
static void trap_hat(double t, double g, double pan) {   // two-stage highpass: brighter, tighter
    long a = at(t); double p1 = 0, p2 = 0;
    for (long i = 0; i < 0.022 * SR; i++) { double nz = rnd(), h1 = nz - p1; p1 = nz; double h2 = h1 - p2; p2 = h1;
        double v = h2 * exp(-(double)i / SR * 220) * g * 0.35; add(tL, a + i, v * (1 - pan)); add(tR, a + i, v * (1 + pan)); }
}
static void clap(double t, double g) {
    static const double OFF[3] = { 0, 0.011, 0.022 }, AMP[3] = { 0.7, 0.8, 1.0 };
    for (int k = 0; k < 3; k++) {
        long a = at(t + OFF[k]); double lp = 0, prev = 0;
        for (long i = 0; i < 0.16 * SR; i++) { double u = (double)i / SR, nz = rnd(), hp = nz - prev; prev = nz;
            lp += (hp - lp) * 0.45;
            double v = lp * exp(-u * (k < 2 ? 90 : 16)) * g * AMP[k] * 0.9; add(pcM, a + i, v); }   // v60: to the percussion seat
    }
}
// 808: drops ~a 4th into the root, long tail, tanh so its harmonics speak
// above 180 Hz on a phone
static void eight08(double t, int midi, double dur, double g) {
    long a = at(t); double f1 = mtof(midi), f0 = f1 * 1.33, ph = 0;
    for (long i = 0; i < (dur + 0.15) * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (f1 + (f0 - f1) * exp(-u * 26)) / SR;
        double env = fmin(1, u / 0.002) * (u > dur ? exp(-(u - dur) * 30) : exp(-u * 1.1));
        add(b808, a + i, tanh(sin(ph) * 3.4) * 0.7 * env * g); /* v5.1: drive 3.4 so harmonics carry it on phones */ }
    for (long i = 0; i < 0.3 * SR; i++) add(duck, a + i, g * exp(-(double)i / SR * 7));
}

// ── sines ────────────────────────────────────────────────────────────────
// slow swell, 4.5 Hz tremolo, a ±det¢ twin. R == NULL → mono (to a spatial bus)
static void sine(float *L, float *R, double t0, double dur, double midi, double g, double pan,
                 double atk, double rel, double det) {
    double f = mtof(midi), fa = f * pow(2, det / 1200), fb = f * pow(2, -det / 1200);
    double pa = (rnd() + 1) * 3, pb = (rnd() + 1) * 3, pc_ = 0, pa0 = pa, pb0 = pb;
    double gl = g * sqrt((1 - pan) / 2), gr = g * sqrt((1 + pan) / 2);
    long a = at(t0), len = (long)((dur + rel) * SR);
    for (long i = 0; i < len; i++) {
        double u = (double)i / SR;
        double env = fmin(1, u / atk) * (u > dur ? exp(-(u - dur) / (rel / 3)) : 1);
        // analog: each voice drifts on its own slow wander; a centre third voice; soft saturation
        double drift = det > 0 ? 1 + 0.0012 * sin(2 * PI * 0.13 * u + pa0) + 0.0007 * sin(2 * PI * 0.31 * u + pb0) : 1;
        pa += 2 * PI * fa * drift / SR; pb += 2 * PI * fb / drift / SR; pc_ += 2 * PI * f * drift / SR;
        double trem = 1 - 0.08 * (0.5 + 0.5 * sin(2 * PI * 4.5 * u));
        double c3 = det > 0 ? sin(pc_) * 0.5 : 0, xa = tanh((sin(pa) + c3) * 1.5) / 1.0, xb = tanh((sin(pb) + c3) * 1.5) / 1.0;   // v8.1: natural
        if (!R) add(L, a + i, (xa + xb) * 0.5 * env * trem * g);
        else { add(L, a + i, xa * env * trem * gl); add(R, a + i, xb * env * trem * gr); }
    }
}
#define PAD(t, d, m, g, pan) sine(sL, sR, t, d, m, g, pan, 0.35, 0.9, 7)
static float *padM[3];                                                  // v11: the tenor trio, one mono bus per voice, spatialized on a tour

// ── AC voices ────────────────────────────────────────────────────────────
// FEM bells from the baked bank (bin/bells.sh). Loner rule: ≤ E5, fade tails.
static Stereo BELLS[128];
// v42: THE GONG — a FEM bell with a four-second decay: five inharmonic partials, each with its own ring-down
static void gong(double t, int midi, double g) {
    static const double P[5] = { 1.0, 2.0, 2.98, 4.2, 5.43 }, D[5] = { 4.2, 3.4, 2.6, 1.9, 1.3 }, A[5] = { 1.0, 0.55, 0.4, 0.22, 0.14 };
    long a = at(t); double f = 440 * pow(2, (midi + CHART_TUNE - 69) / 12.0), ph[5] = { 0 };
    for (long i = 0; i < 5.0 * SR; i++) { double u = (double)i / SR, v = 0;
        for (int k = 0; k < 5; k++) { ph[k] += 2 * PI * f * P[k] * (1 + 0.0015 * k) / SR; v += sin(ph[k]) * A[k] * exp(-u / D[k]); }
        add(bellM, a + i, v * fmin(1, u / 0.006) * g * 0.35); }
    ev(t, "gong", 4.0, g, midi);
}
static void fem_bell(double t, int midi, double g) {
    while (midi > 76) midi -= 12;
    while (midi < 63) midi += 12;
    if (!BELLS[midi].L) {
        char p[256]; snprintf(p, sizeof p, LANE "/src/bells/bell-%d.wav", midi);
        BELLS[midi] = load_wav(p);
        if (!BELLS[midi].L) return;
        long f = (long)(0.6 * SR);
        for (long i = 0; i < f && i < BELLS[midi].n; i++) { double k = (double)i / f;
            BELLS[midi].L[BELLS[midi].n - 1 - i] *= (float)k; BELLS[midi].R[BELLS[midi].n - 1 - i] *= (float)k; }
    }
    const Stereo *b = &BELLS[midi]; long a = at(t);
    for (long i = 0; i < b->n; i++) add(bellM, a + i, (b->L[i] + b->R[i]) * 0.5 * g);
    ev(t, "bell", 1.2, g, midi);
}
// vibraphone — pop/marimba's modal preset, ported: partials 1/4/10, decays
// 4.5/0.9/0.25 s, 5.5 Hz motor tremolo at 0.55; damper after `hold`
static void vib(double t, int midi, double g, double hold) {
    static const double P[3] = { 1, 4, 10 }, AMP[3] = { 1, 0.22, 0.08 }, DEC[3] = { 4.5, 0.9, 0.25 };
    double f = mtof(midi); long a = at(t), len = (long)((hold + 0.5) * SR);
    for (long i = 0; i < len; i++) { double u = (double)i / SR, v = 0;
        for (int k = 0; k < 3; k++) if (f * P[k] < SR / 2) v += AMP[k] * sin(2 * PI * f * P[k] * u) * exp(-u / DEC[k]);
        double damp = u > hold ? exp(-(u - hold) * 10) : 1;
        double trem = 1 - 0.55 * (0.5 + 0.5 * sin(2 * PI * 5.5 * u));
        add(vibM, a + i, v * fmin(1, u / 0.001) * damp * trem * g * 0.6); }
    ev(t, "vib", 0.25, g, midi);
}

// ── harmony ──────────────────────────────────────────────────────────────
static const int CHORD_PCS[3][3] = { { 8, 11, 3 }, { 8, 11, 3 }, { 11, 3, 6 } };   // Emaj7 bars sound G#m (bass G#)
static const int ROOT[3] = { 44, 44, 47 };                                          // G#2, B2
static const int SCALE[7] = { 8, 10, 11, 1, 3, 4, 6 };                              // G# natural minor
static int is_pc(const int *pcs, int np, int m) { int pc = ((m % 12) + 12) % 12; for (int k = 0; k < np; k++) if (pcs[k] == pc) return 1; return 0; }
static int nearest(const int *pcs, int np, double target, int lo, int hi) {
    int best = -1; for (int m = lo; m <= hi; m++) if (is_pc(pcs, np, m) && (best < 0 || fabs(m - target) < fabs(best - target))) best = m;
    return best;
}
static void lead(int *voices, int nv, const int *pcs, int lo, int hi) {   // nearest chord tone per voice, no doubling
    int used[3] = { -1, -1, -1 }, nu = 0;
    for (int v = 0; v < nv; v++) {
        int freeP[3], nf = 0;
        for (int k = 0; k < 3; k++) { int u = 0; for (int j = 0; j < nu; j++) if (used[j] == pcs[k]) u = 1; if (!u) freeP[nf++] = pcs[k]; }
        int m = nf ? nearest(freeP, nf, voices[v], lo, hi) : nearest(pcs, 3, voices[v], lo, hi);
        voices[v] = m; if (nu < 3) used[nu++] = ((m % 12) + 12) % 12;
    }
}
// v29: scale steps — the nearest scale tone at or under m, moved `steps` degrees in G# natural minor
static int scale_step(int m, int steps) {
    int tones[64], n = 0; for (int x = m - 30; x <= m + 30; x++) { int pc = ((x % 12) + 12) % 12; for (int k = 0; k < 7; k++) if (SCALE[k] == pc) { tones[n++] = x; break; } }
    int i = 0; for (int k = 0; k < n; k++) if (tones[k] <= m) i = k;
    i += steps; if (i < 0) i = 0; if (i >= n) i = n - 1; return tones[i];
}
static int third_above(int m) {
    int pc = ((m % 12) + 12) % 12, oct = m / 12 - (m < 0), i = -1;
    for (int k = 0; k < 7; k++) if (SCALE[k] == pc) i = k;
    if (i < 0) return m + 3;
    int t = SCALE[(i + 2) % 7];
    return oct * 12 + t + (t < pc ? 12 : 0);
}
static const ChartBar *bar_n(int n) {   // v13: the bar numbered n (exact first — the arrangement reorders bars), else the first after it
    for (int k = 0; k < CHART_NBARS; k++) if (CHART_BARS[k].n == n) return &CHART_BARS[k];
    for (int k = 0; k < CHART_NBARS; k++) if (CHART_BARS[k].n >= n) return &CHART_BARS[k];
    return &CHART_BARS[CHART_NBARS - 1];
}
static int pos_n_after(int n, int from) { for (int k = from; k < CHART_NBARS; k++) if (CHART_BARS[k].n == n) return k + 1; return CHART_NBARS; }   // v19: verse 1 now ends on a partial bar 27 too
static int pos_n(int n) { for (int k = 0; k < CHART_NBARS; k++) if (CHART_BARS[k].n == n) return k + 1; return CHART_NBARS; }   // record position of a bar
static const ChartBar *bar_at(double t) {
    for (int k = 0; k < CHART_NBARS; k++) if (t >= CHART_BARS[k].t && t < CHART_BARS[k].t + CHART_BARS[k].dur) return &CHART_BARS[k];
    return NULL;
}

// ── automation: a per-bar target, glided over 150 ms ─────────────────────
typedef double (*BarValue)(const ChartBar *, int);
static float *automate(BarValue fn, int arg) {
    float *a = (float *)malloc(N * sizeof(float)); double cur = -1, glide = 1 - exp(-1.0 / (0.15 * SR));
    const ChartBar *b = &CHART_BARS[0];
    for (long i = 0; i < N; i++) {
        double t = (double)i / SR;
        while (b < &CHART_BARS[CHART_NBARS - 1] && t >= b->t + b->dur) b++;
        double tg = fn(b, arg);
        cur = cur < 0 ? tg : cur + (tg - cur) * glide; a[i] = (float)cur;
    }
    return a;
}
#define ARRV(field) static double v_##field(const ChartBar *b, int x) { (void)x; return ARR[section_of(b->n)].field; }
ARRV(send) ARRV(air) ARRV(acg) ARRV(elg)
static double v_her(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); double g = ARR[s].her; return s == VERSE1 ? g * (0.08 + 0.92 * evo(b->n, 14, 20)) : g; }   // v31c: "very quiet first few bars so it sounds like isolated vocal"   // v28: "start the track with the vocal … lose some of the guitar in the beginning, just have percussion" — a sixth of it under her first line, in by 19
// v6.6: warm — the opening vocal close to the mic: proximity below 350 Hz, no air, almost no room; eases out by the floor
// v7: the choir stands behind her in the choruses, the bridge and the outro; a breath of it in verse 2
static double v_choir(const ChartBar *b, int x) { (void)x; int s = section_of(b->n);   // v67: the choir fades in over the chorus's first six bars
    return s == CHORUS1 ? 0.55 * evo(b->n, 29, 35) : s == VERSE2 ? 0.25 : s == CHORUS2 ? 0.8 * (0.2 + 0.8 * evo(b->n, 52, 58)) : s == BREAK ? 0.5 : s == BRIDGE ? 0.75 : s == OUTRO ? 0.9 : 0; }   // v26: back a step
// v22: SCREAM — a distorted double of her lead (clipped, growled, wide) under the big sections
static double v_scream(const ChartBar *b, int x) { (void)x; int s = section_of(b->n);
    return s == CHORUS2 ? 0.12 : s == BRIDGE ? 0.3 * evo(b->n, 76, 80) : 0; }   // v27: the scream earns itself only in the bridge climb
// v22: the ORCHESTRA (bin/orchestra.mjs → src/orch): where a part plays is decided there; how loud, here
enum { O_STRINGS, O_CELLO, O_PIZZ, O_HORNS, O_TIMP, O_HARP, O_GLOCK, O_AAHS, O_VLN1, O_VLN2, O_VIOLA, O_QCELLO, O_TAIKO, NORCH };
static const char *ORCH_FILE[NORCH] = { "strings", "cello", "pizz", "horns", "timpani", "harp", "glock", "aahs", "vln1", "vln2", "viola", "qcello", "taiko" };
static const int ORCH_DRUM[NORCH] = { 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1 };   // v24: drums (and pizz) are not keyed to her voice
static const double ORCH[NSEC][NORCH] = {
    //              str  cel  piz  hrn  tmp  hrp  glk  aah  vl1  vl2  vla  qvc  tai      v44: strings at a third — the sine pad is the main pad
    [INTRO]   = {   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 },
    [VERSE1]  = {  .3,  .5,  .6,   0,  .8,   0,   0,   0,   0,   0,   0,   0,   0 },
    [CHORUS1] = {  .3,  .7,   0,  .6,  .8,   0,   0,  .6,  .7,  .7,  .7,  .7,   0 },
    [VERSE2]  = {   0,  .5,  .6,   0,   0,  .7,   0,   0,  .6,  .6,   0,   0,   0 },
    [CHORUS2] = {  .4,  .8,   0,  .8,   1,   0,  .7,  .9,  .9,  .9,  .9,  .9,  .5 },
    [BREAK]   = {  .4,  .6,   0,  .5,  .7,  .7,   0,  .5,  .7,  .7,  .7,  .7,  .6 },   // v24: the break rises instead of emptying
    [BRIDGE]  = {  .4,  .8,   0,  .9,   1,  .6,  .8,  .9,   1,   1,   1,   1,  .6 },
    [OUTRO]   = {  .5,   1,   0,   1,   1,  .8,   1,   1,   1,   1,   1,   1,  .6 } };  // v24: the finale; v30: taiko supports the toms
static double v_orch(const ChartBar *b, int o) { return (KICK_ONLY && o == O_TAIKO) ? 0 : ORCH[section_of(b->n)][o]; }   // v48: no taiko in kick-only
static double v_dissolve(const ChartBar *b, int x) { (void)x; return evo(b->n, 81, 85); }   // v41: 0 → 1 across the finale: the band closes through a lowpass
static double v_room(const ChartBar *b, int x) { (void)x; return 1 - 0.8 * evo(b->n, 72, 84); }   // v39: "towards the end we lose the reverb" — the room eases out from the bridge to the last bar
static double v_orch_side(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == BREAK || s == OUTRO ? 0.5 : 1; }     // v27
static double v_orch_shelf(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == BREAK || s == OUTRO ? 0.6 : 0; }    // v27: +4 dB above 3 kHz
// v24: the quartet's seats, each leaning a little on its own phase
static double q_vln1(double t) { return -38 + 4 * sin(2 * PI * 0.15 * t); }
static double q_vln2(double t) { return -14 + 4 * sin(2 * PI * 0.15 * t + 1.6); }
static double q_viola(double t) { return 14 + 4 * sin(2 * PI * 0.15 * t + 3.1); }
static double q_qcello(double t) { return 38 + 4 * sin(2 * PI * 0.15 * t + 4.7); }
static double el_low(double t) { (void)t; return -12; }
static double sway_pc(double t) { return 18 * sin(2 * PI * t / 13); }    // v60: the percussion's slow sway
static double hat_az(double t) { return 40 + 25 * sin(2 * PI * t / 11); }   // v65: the hats, right of centre, wandering
static double seat_gtr(double t) { return 0 + 2 * sin(2 * PI * t / 17); }   // v68: her guitar WITH her — centre, near, the same room
static double spin_az(double t) { return 360 * t / 1.6; }   // v25: the throws' spin, one lap per 1.6 s
// v24: WUB — a saw on her root an octave under the sub, through a resonant lowpass the LFO sweeps 90 Hz → 1.8 kHz
static const double WUB[NSEC] = { 0, 0, 0, 0, .55, .35, .75, .9 };
static const double WUB_RATE[NSEC] = { 1, 1, 1, 1, 1, 1, 2, 2 };   // sweeps per beat (chorus 2 quarters, bridge and finale 8ths)
static float *wubM = NULL, *rsL = NULL, *rsR = NULL;   // v27: the riser on its own pair (the dropout gate leaves it)
static float *melM = NULL, *mirM = NULL, *radL = NULL, *radR = NULL;   // v43: the radio intro   // v33: the melody bus — the sister sine + chorale (centre) and the mirror (sweeping) — NOT keyed to her voice
static void wub(double t0, double t1, int midi, double beatDur, double perBeat, double g) {
    long a = at(t0), z = at(t1); double f = 440 * pow(2, (midi + CHART_TUNE - 69) / 12.0), ph = 0, lp = 0, bp = 0;
    const double rate = perBeat / beatDur, q = 0.28;
    for (long i = a; i < z && i < N; i++) { double u = (double)(i - a) / SR, tail = fmin(1, (double)(z - i) / SR / 0.03), head = fmin(1, u / 0.01);
        ph += f / SR; if (ph >= 1) ph -= 1; double saw = 2 * ph - 1, sq = ph < 0.5 ? 1 : -1, x = 0.7 * saw + 0.3 * sq;
        double lfo = 0.5 - 0.5 * cos(2 * PI * rate * u), fc = 90 * pow(20, lfo), kc = 2 * sin(PI * fmin(fc, 8000) / SR);
        lp += kc * bp; double hp = x - lp - q * bp; bp += kc * hp;                 // Chamberlin SVF
        add(wubM, i, tanh(lp * 2.6) * 0.5 * g * head * tail); }
}
static double v_jeff(const ChartBar *b, int x) { (void)x; int s = section_of(b->n);
    return s == CHORUS1 ? 0 : s == CHORUS2 ? 0.6 * evo(b->n, FLOOR_BAR + 1, FLOOR_BAR + 4) : s == BRIDGE ? 0.5 : s == OUTRO ? 0.3 : 0; }
// v11: the TOUR — seat assignments advance one seat every four bars, eased over the last half of the fourth bar
static double SEAT5[5] = { -70, -35, 0, 35, 70 };
static double tour_az(double t, int voice) {
    const ChartBar *b = bar_at(t); int step = b ? (b->n - 1) / 4 : 0; double x = b ? (t - b->t) / fmax(0.5, b->dur) : 0;
    int s0 = (voice + step) % 5, s1 = (voice + step + 1) % 5; double e = (b && ((b->n - 1) % 4 == 3)) ? fmin(1, fmax(0, (x - 0.5) * 2)) : 0;
    e = e * e * (3 - 2 * e); return SEAT5[s0] + (SEAT5[s1] - SEAT5[s0]) * e;
}
static double tour0(double t) { return tour_az(t, 0); }
static double tour1(double t) { return tour_az(t, 2); }
static double tour2(double t) { return tour_az(t, 4); }
// the SWAY (rule 4): every harmony seat leans ±22° at 0.18 Hz, each on its own phase
static double sway(double t, int h) { return 22 * sin(2 * PI * 0.18 * t + h * 1.3); }
// the choir ORBITS: its three seats make one lap every 32 s
static double orbit0(double t) { return -48 + 360 * t / 32; }
static double orbit1(double t) { return 0 + 360 * t / 32; }
static double orbit2(double t) { return 48 + 360 * t / 32; }
// the sister DALLIES around her: two slow wanders summed, never still, never far
static double dally_hi(double t) { return 55 * sin(2 * PI * t / 9) + 30 * sin(2 * PI * t / 23 + 1); }
static double dally_lo(double t) { return -55 * sin(2 * PI * t / 11 + 2) + 25 * sin(2 * PI * t / 19); }
static double dally_el(double t) { return 12 + 10 * sin(2 * PI * t / 13); }
// the hook ROTATES through the break: 8 turns over 16 s, quintic ease (nullabye)
static double ROT_T0 = 0;
static double rot_az(double t) { double x = fmin(1, fmax(0, (t - ROT_T0) / 16)); double e = x * x * x * (x * (6 * x - 15) + 10); return 360 * 8 * e; }
static double seat_l(double t) { (void)t; return -48; }
static double seat_c(double t) { (void)t; return 0; }
static double seat_r(double t) { (void)t; return 48; }
static double el_back(double t) { (void)t; return -8; }
static double v_sis_hi(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == CHORUS2 ? 0.5 * evo(b->n, FLOOR_BAR + 3, FLOOR_BAR + 7) : s == BRIDGE ? 0.5 : s == OUTRO ? 0.45 : 0; }
static double v_sis_lo(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == VERSE2 ? 0.3 * evo(b->n, 46, 50) : s == CHORUS2 ? 0.45 : s == BRIDGE ? 0.5 : s == OUTRO ? 0.4 : 0; }
static double v_sis_hum(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == VERSE1 ? 0.3 * evo(b->n, 16, 22) : s == VERSE2 ? 0.4 : s == CHORUS2 ? 0.35 : s == BREAK ? 0.55 : s == BRIDGE ? 0.45 : s == OUTRO ? 0.45 : 0; }
// v19: the distance arc, one slow build over the record (by record position, not section):
// she starts quiet and right next to you — dry, intimate, "in the car" — comes up to full
// level across verse 1, then the room opens a little every bar: the dry closeness gives
// way and a long stone tail grows under everything until it is a stadium by the end
static int rec_pos(const ChartBar *b) { return (int)(b - CHART_BARS) + 1; }
static double v_far(const ChartBar *b, int x) { (void)x; return rec_pos(b) < pos_n(11) ? 1 : rec_pos(b) < pos_n(44) ? 1 - evo(b->n, 13, 25) : 0; }   // far = quiet
static double v_cath(const ChartBar *b, int x) { (void)x; return 0.6 * evo(rec_pos(b), pos_n(22), CHART_NBARS - 6) * (section_of(b->n) == BRIDGE ? 0.3 : 1); }   // v32: the cat-and-mouse verse is close — the stone steps back   // v22b: "we still need her to have an up front voice" — half the stone (was 1.3)
static double v_warm(const ChartBar *b, int x) { (void)x; if (section_of(b->n) == BRIDGE) return 0.65; return 1 - 0.6 * evo(rec_pos(b), pos_n(20), CHART_NBARS - 6); }   // v32: "bring her voice back in" for the bridge — at the mic again   // v25: she stays closer all the way (was 0.85)
static double v_thick(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == INTRO ? 0 : s == VERSE1 ? 0.25 * evo(b->n, 16, 24) : s == BREAK ? 0 : s == VERSE2 ? 0.18 : 0.2; }   // v15: the doubles trimmed so the lead stays in front; v26: thinner still
static double v_harm_g(const ChartBar *b, int h) { return harm_at(b->n, h).g; }
static double v_harm_az(const ChartBar *b, int h) { return harm_at(b->n, h).az; }

// ── space: ac_hrtf over a whole bus ─────────────────────────────────────
static double impact_disp(double t);
static double turn_disp(double t);   // v25
static double ring_pull(double t);
typedef double (*Path)(double t);
static void spatialize(const float *mono, const float *azA, Path az, Path el, double dist, float *L, float *R, double g) {
    ACHrtf h; memset(&h, 0, sizeof h);
    for (long i = 0; i < N; i++) {
        double t = (double)i / SR, a = azA ? azA[i] : az(t);
        float l, r;
        a += impact_disp(t) + turn_disp(t);                               // v6: the ring is displaced and springs back; v25: and turns
        double dd = dist * ring_pull(t); if (dd < 0.35) dd = 0.35;      // v11: implosions pull every seat in, then it springs back
        ac_hrtf_process(&h, mono[i], a * PI / 180, el(t) * PI / 180, dd, &l, &r);
        L[i] += l * (float)g; R[i] += r * (float)g;
    }
}
static double orbit_az(double t) { return 70 * sin(2 * PI * t / 24); }
static double orbit_el(double t) { return 25 + 10 * sin(2 * PI * t / 17); }
static double sweep_az(double t) { return 65 * sin(2 * PI * t / 8); }
static double wide_az(double t)  { return 80 * sin(2 * PI * t / 31 + 1); }
static double el_zero(double t)  { (void)t; return 0; }
static double el_up(double t)    { (void)t; return 35; }
static double el_near(double t)  { (void)t; return 5; }

// ── v6 · impact (chamber-04 rule 10): displace every ring seat, spring back ──
// kick and sub never move (they are not on a spatial bus); ring tones open
// with a short noise event — the explosion below.
static const int IMPACT_BARS[] = { 28, 52, 73, 81 };   // v25: IMPLOSIONS at every lift — chorus 1, chorus 2, the bridge, the finale (v11: 52, 73)
#define VOICE_LAG 0.0        // v7: her sung onset sits ~50 ms behind the strum (ANALYSIS.md §2); the explosion waits for it
// v6: the evolution — 0 before `from`, 1 at `to`, cosine between (one lane per phrase)
static double evo(int bar, int from, int to) { if (bar < from) return 0; if (bar >= to) return 1; double x = (double)(bar - from) / (to - from); return 0.5 - 0.5 * cos(PI * x); }
#define NIMPACT ((int)(sizeof IMPACT_BARS / sizeof *IMPACT_BARS))
static double IMPACT_T[NIMPACT];
// v25: TURNS — the whole ring turns once (360°, quintic ease) across the bar before every lift, and in the
// finale it keeps turning, one lap every eight bars
static double TURN_T0[4], TURN_D[4], FIN_T0 = 1e9, FIN_LAP = 15;
// v35: THE PICKUP — "right at 0:34 is where the chorus should start": the chorus begins on "Oh, won't you", beat 3 of the bar
// before "kiss" (bars 27 and 51). Everything that marks a chorus moves to that beat.
static int pickup_bar(int n) { return n == 27 || n == 51; }
static int pickup_j(const ChartBar *b) { return b->nb > 2 ? b->nb - 2 : 0; }                       // v51: "27.4" — TWO beats before "kiss" (27.4 and 51.3), on her "you"
static double pickup_t(const ChartBar *b) { return b->beats[pickup_j(b)]; }
static double SHIFT_T[3] = { 1e9, 1e9, 1e9 }, SPIN_T0 = 1e9, SPIN_T1 = 1e9;   // v61: shifts on chorus 2's phrase downbeats; a spin out of the break
static double LAP_T0 = 1e9, LAP_T1 = 1e9, WOB_T0 = 1e9, WOB_T1 = 1e9;         // v65: one slow lap across chorus 2; a wobble through the bridge climb
static double turn_disp(double t) {
    double d = 0;
    for (int k = 0; k < 3; k++) { double u = t - SHIFT_T[k]; if (u >= 0 && u < 2.0) d += 90 * exp(-u * 2.2); }   // a 90° jerk that eases back
    if (t >= SPIN_T0 && t < SPIN_T1) { double x = (t - SPIN_T0) / (SPIN_T1 - SPIN_T0); d += 360 * x * x * (3 - 2 * x); }   // v65: one lap, eased (two was a blur)
    if (t >= LAP_T0 && t < LAP_T1) d += 360 * (t - LAP_T0) / (LAP_T1 - LAP_T0);                                               // v65: the whole ring turns once, slowly, across chorus 2
    if (t >= WOB_T0 && t < WOB_T1) { double x = (t - WOB_T0) / (WOB_T1 - WOB_T0); d += 35 * sin(2 * PI * 0.5 * (t - WOB_T0)) * fmin(1, x * 4) * fmin(1, (1 - x) * 4); }   // v65: ±35° at 0.5 Hz
    for (int k = 0; k < 4; k++) { double x = (t - TURN_T0[k]) / TURN_D[k]; if (x > 0 && x < 1) d += 360 * x * x * x * (x * (6 * x - 15) + 10); }
    if (t > FIN_T0) d += 360 * (t - FIN_T0) / FIN_LAP;
    return d;
}
static double impact_disp(double t) {          // degrees of azimuth displacement
    double d = 0;
    for (int k = 0; k < NIMPACT; k++) { double u = t - IMPACT_T[k];
        if (u >= 0 && u < 6) d += 110 * exp(-1.2 * u) * sin(2 * PI * 1.1 * u); }
    return d;
}
static double last_impact(double t) { double best = -1e9; for (int k = 0; k < NIMPACT; k++) if (IMPACT_T[k] <= t && IMPACT_T[k] > best) best = IMPACT_T[k]; return best; }
// the explosion: one full lap around the head in 0.7 s, climbing overhead
static double next_impact(double t) { double best = 1e9; for (int k = 0; k < NIMPACT; k++) if (IMPACT_T[k] >= t && IMPACT_T[k] < best) best = IMPACT_T[k]; return best; }
// the implosion: 1.5 laps spiralling IN from the far ring to the centre over the 0.9 s before the downbeat, coming down from overhead
static double expl_az(double t) { double u = next_impact(t) - t, w = t - last_impact(t); return u <= 0.9 ? 540 * u / 0.9 : w <= 1.2 ? 540 * w / 1.2 : 0; }   // v28: in before, OUT after
static double expl_el(double t) { double u = next_impact(t) - t, w = t - last_impact(t); return u <= 0.9 ? 55 * u / 0.9 : w <= 1.2 ? 55 * w / 1.2 : 0; }
// every seat's distance: pulled in over the 0.9 s before an implosion (to 0.45 of its place), then springs back at 1.1 Hz
static double ring_pull(double t) {
    double p = 1;
    for (int k = 0; k < NIMPACT; k++) { double u = t - IMPACT_T[k];
        if (u >= -0.9 && u < 0) { double x = (u + 0.9) / 0.9; p -= 0.55 * x * x; }
        else if (u >= 0 && u < 5) p += 0.35 * exp(-1.2 * u) * cos(2 * PI * 1.1 * u) - 0.35 * exp(-1.2 * u); }
    return p;
}
static float *exM, *hookM;
// v28: the BLAST — the implosion's answer: a whoosh falling 6 kHz → 250 Hz over the 1.2 s after the downbeat, spiralling out (expl_az)
static void blast(double t, double g) {
    long a = at(t); double b1 = 0, b2 = 0;
    for (long i = 0; i < 1.2 * SR; i++) { double x = (double)i / (1.2 * SR), fc = 6000 * pow(1.0 / 24, x), k = 1 - exp(-2 * PI * fc / SR);
        double nz = rnd(); b1 += (nz - b1) * k; b2 += (b1 - b2) * k;
        double env = fmin(1, x / 0.02) * (1 - x) * (1 - x);
        add(exM, a + i, (b1 - b2) * env * g * 2.0); }
}
static void explosion(double t, double g) {   // v11: an IMPLOSION — a reversed whoosh: bandpass rising 250 Hz → 6 kHz over the 0.9 s before t, cut dead on the downbeat, then a sub thump
    long a = at(t - 0.9); double b1 = 0, b2 = 0, ph = 0;
    for (long i = 0; i < 0.9 * SR; i++) { double x = (double)i / (0.9 * SR), fc = 250 * pow(24, x), k = 1 - exp(-2 * PI * fc / SR);
        double nz = rnd(); b1 += (nz - b1) * k; b2 += (b1 - b2) * k;
        double env = x * x * x * (x > 0.985 ? (1 - x) / 0.015 : 1);
        add(exM, a + i, (b1 - b2) * env * g * 2.4); }
    a = at(t);
    for (long i = 0; i < 0.5 * SR; i++) { double u = (double)i / SR; ph += 2 * PI * (38 + 90 * exp(-u * 20)) / SR;
        double v = sin(ph) * exp(-u * 7) * g * 0.7; add(dL, a + i, v); add(dR, a + i, v); }
    ev(t, "impact", 1.0, g, -1);
}

// ── v6 · vocal arpeggios: her harmony stems gated into figures on her grid ──
// -1 = rest. div = steps per beat (2 = 8ths, 3 = triplets, 4 = 16ths).
typedef struct { int div; int len; int step[8]; double g; double hold; } Arp;   // hold: step lengths each note sings (v6.1: > 1 so the stack rolls, never chops)
static const Arp ARP[NSEC] = {
    [INTRO]   = { 0 }, [VERSE1] = { 0 },
    [CHORUS1] = { 2, 4, { H_UP3, H_DOWN3, H_UP3, H_DOWN6 }, .45, 1.8 },                  // 8ths, a rocking third, each held 1.8 steps
    [VERSE2]  = { 0 },                                                                     // the sustained down3 sings here
    [CHORUS2] = { 4, 8, { H_DOWN3, H_UP3, H_UP5, H_DOWN6, H_UP3, H_DOWN3, H_DOWN8, H_UP3 }, .40, 3.0 },   // 16ths, up then down, three steps deep
    [BREAK]   = { 0 },
    [BRIDGE]  = { 3, 3, { H_UP3, H_DOWN3, H_DOWN6 }, .30, 2.0 },                         // triplets, roving seats
    [OUTRO]   = { 2, 2, { H_DOWN8, H_UP3 }, .22, 2.0 },
};
static const double SEAT[5] = { -70, -35, 0, 35, 70 };
static float *arpG[NHARM], *arpAz[NHARM];
static void arp_step(int h, double t0, double t1, double g, double az, double hold) {
    double step = t1 - t0; long a = at(t0), z = at(t0 + step * hold), rel = (long)(0.09 * SR);   // 12 ms in, 90 ms out
    for (long i = a; i < z + rel && i < N; i++) { double u = (double)(i - a) / SR, e = fmin(1, u / 0.012);
        if (i >= z) e *= exp(-(double)(i - z) / rel * 4);
        if (i >= 0) { arpG[h][i] = (float)fmin(g, arpG[h][i] + e * g); if (arpAz[h][i] == 0) arpAz[h][i] = (float)az; } }
}

// ── reverb: 8-line FDN (Hadamard feedback, damped) ──────────────────────
static void fdn(const float *inL, const float *inR, float *oL, float *oR, double t60, double damp, double pre) {
    static const int LENS[8] = { 1433, 1601, 1867, 2053, 2251, 2399, 2617, 2797 };
    float *buf[8]; int len[8], idx[8] = { 0 }; double lp[8] = { 0 }, gain[8], o[8], hh[8];
    for (int k = 0; k < 8; k++) { len[k] = (int)lround(LENS[k] * (double)SR / 44100); buf[k] = (float *)calloc(len[k], sizeof(float));
        gain[k] = pow(10, -3.0 * len[k] / (SR * t60)); }
    long P = lround(pre * SR);
    for (long i = 0; i < N; i++) {
        double xl = i >= P ? inL[i - P] : 0, xr = i >= P ? inR[i - P] : 0;
        for (int k = 0; k < 8; k++) { lp[k] += (buf[k][idx[k]] - lp[k]) * (1 - damp); o[k] = lp[k]; }
        double a0 = o[0] + o[1], a1 = o[0] - o[1], a2 = o[2] + o[3], a3 = o[2] - o[3], a4 = o[4] + o[5], a5 = o[4] - o[5], a6 = o[6] + o[7], a7 = o[6] - o[7];
        double b0 = a0 + a2, b1 = a1 + a3, b2 = a0 - a2, b3 = a1 - a3, b4 = a4 + a6, b5 = a5 + a7, b6 = a4 - a6, b7 = a5 - a7;
        hh[0] = b0 + b4; hh[1] = b1 + b5; hh[2] = b2 + b6; hh[3] = b3 + b7; hh[4] = b0 - b4; hh[5] = b1 - b5; hh[6] = b2 - b6; hh[7] = b3 - b7;
        for (int k = 0; k < 8; k++) { buf[k][idx[k]] = (float)(hh[k] * 0.35355 * gain[k] + (k % 2 ? xr : xl) * 0.5); idx[k] = (idx[k] + 1) % len[k]; }
        oL[i] = (float)(o[0] + o[2] + o[4] + o[6]); oR[i] = (float)(o[1] + o[3] + o[5] + o[7]);
    }
    for (int k = 0; k < 8; k++) free(buf[k]);
}

static float *zeros(void) { return (float *)calloc(N, sizeof(float)); }

// humanize: ±4 ms, ±12 % — a hand, not a sequencer ("a bit midi like")
static double hum_t(double t) { return t + rnd() * 0.004; }
static double hum_g(double g) { return g * (1 + rnd() * 0.12); }
static double hum_t2(double t, double ms) { return t - EAGER + rnd() * ms / 1000; }   // v6.4: looser hands; v73: eager — ahead of the beat
static double hum_g2(double g, double pct) { return g * (1 + rnd() * pct); }

// ── the dance layer (v5.2: "turn the song into a dance mix") ─────────────
// Her ~120 is house tempo, so four on HER floor: a kick on every one of her
// beats, claps 2 + 4, open hats on every offbeat, an offbeat house bass on
// her roots, risers into the drops.
// v46: every kick has its own velocity, attack and decay — `vel` scales the hit and hardens the click, `atk` sets how fast the
// pitch sweep falls into the body (a slow sweep is a softer front), `dec` the body's ring (1 = v38's 4.2/s). The pump follows.
static void dance_kick_v(double t, double g, double vel, double atk, double dec) {
    long a = at(t); double ph = 0, pb = 0; const double decay = 7.5 / dec, sweep = 48 * atk;   // v49: "too long … punchier, boxier" — the body dies in ~130 ms, a box at 190 Hz, a harder front
    for (long i = 0; i < 0.4 * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (51.9 + 190 * exp(-u * sweep) + 60 * exp(-u * 400 * atk)) / SR; pb += 2 * PI * 155.6 / SR;   // v58: body on G#1 (her root), the box on D#3 (the fifth) — in key, under her range
        double click = i < 0.0025 * SR ? rnd() * (0.4 + 0.6 * vel * atk) * (1 - u / 0.0025) : 0;
        double box = sin(pb) * 0.28 * exp(-u * 22);                                                        // the "box": a short 190 Hz knock
        double v = (tanh(sin(ph) * (1.9 + 1.0 * vel)) * 1.05 * exp(-u * decay) + box + click) * g * (0.75 + 0.25 * vel);   // v58: less drive — the 208 Hz harmonic sat on her G#3
        add(dL, a + i, v); add(dR, a + i, v); }
    for (long i = 0; i < 0.25 * SR; i++) add(duck, a + i, g * (1.6 + 0.4 * vel) * exp(-(double)i / SR * 11 / sqrt(dec)));
}
static void dance_kick(double t, double g) { dance_kick_v(t, g, 1, 1, 1); }
// v30: "our kit / tom drums are pretty weak / weird" — a SNARE under the claps and TOMS for the fills, synthesized
static void snare_hit(double t, double g) {   // v30: the pop snare (the v2 brushy one above stays for the verses' table)
    long a = at(t); double ph = 0, p1 = 0, b1 = 0;
    for (long i = 0; i < 0.22 * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (190 + 60 * exp(-u * 60)) / SR; double body = sin(ph) * exp(-u * 24) * 0.7;
        double nz = rnd(); b1 += (nz - b1) * 0.35; double hp = nz - b1; p1 += (hp - p1) * 0.6;
        double wire = p1 * exp(-u * 13) * 0.9 * fmin(1, u / 0.0015);
        double v = (body + wire) * g; add(pcM, a + i, v); }
    for (long i = 0; i < 0.15 * SR; i++) add(duck, a + i, g * 0.5 * exp(-(double)i / SR * 14));
}
// v31d: "wish we had more cool sounds for them" — real toms: four CC0 one-shots from Freesound (src/kit/tom-1..4.wav,
// hi rack → low rack → floor → very low; ids 634272, 634273, 808545, 685559), each normalized at load, played with a
// hair of random pitch, the synth tom underneath at 40 % for weight
static Stereo TOM_S[4];
static void tom(double t, double hz, double g, double pan) {   // pan −1 (left, high) … +1 (right, floor)
    long a = at(t); double ph = 0;
    { int idx = hz >= 200 ? 0 : hz >= 140 ? 1 : hz >= 100 ? 2 : 3; const Stereo *sm = &TOM_S[idx];
      if (sm->L) { double r = 1 + 0.03 * rnd(), pos = 0; for (long i = 0; pos < sm->n - 1; i++, pos += r) { long j = (long)pos; double f = pos - j;
          double v = (sm->L[j] * (1 - f) + sm->L[j + 1] * f) * g * 1.3; add(pcM, a + i, v); } g *= 0.4; } }
    for (long i = 0; i < 0.45 * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (hz * (1 + 0.6 * exp(-u * 30))) / SR;
        double click = i < 0.002 * SR ? rnd() * 0.5 * (1 - u / 0.002) : 0;
        double v = (tanh(sin(ph) * 1.6) * 0.8 * exp(-u * 6.5) + click) * g;
        add(pcM, a + i, v); }
}
// v59: OCTAVE PLAY ON HER GUITAR — a granular pitch shift (50 ms Hann grains, half-overlap, time preserved): `ratio(t)` per grain.
// Used for a 12-string octave-up double under the big sections, and a climb up the scale in quantized 8ths out of the break.
static void shift_into(const float *src, long n, float *dst, long a, long z, double (*ratio)(double), double g) {
    const long W = (long)(0.05 * SR), H = W / 2;
    for (long p = a; p < z; p += H) { double r = ratio((double)p / SR); if (r <= 0) continue;
        for (long k = 0; k < W; k++) { long o = p + k; if (o >= N) break; double w = 0.5 - 0.5 * cos(2 * PI * (k + 0.5) / W);
            double pos = p + (k - W / 2.0) * r + W / 2.0; long j = (long)floor(pos); double f = pos - j;
            double x = (j >= 0 && j + 1 < n) ? src[j] * (1 - f) + src[j + 1] * f : 0; dst[o] += (float)(x * w * g); } }
}
static double ratio_octave(double t) { (void)t; return 2.0; }
static double PITCHY_T0 = 1e9, PITCHY_T1 = 1e9, PITCHY_BEAT = 0.48;   // v73: bars 73–76 — her strums re-pitched per beat: octave · fifth · fourth · octave
static double ratio_pitchy(double t) { if (t < PITCHY_T0 || t >= PITCHY_T1) return 0; static const double R[4] = { 2.0, 1.4983, 1.3348, 2.0 }; int k = (int)((t - PITCHY_T0) / PITCHY_BEAT) % 4; return R[k]; }
static double CLIMB_T0 = 0, CLIMB_T1 = 0, CLIMB_STEP = 0.24;
static double ratio_climb(double t) { if (t < CLIMB_T0 || t >= CLIMB_T1) return 0; static const int DEG[8] = { 0, 2, 3, 5, 7, 8, 10, 12 }; int k = (int)((t - CLIMB_T0) / CLIMB_STEP); if (k > 7) k = 7; return pow(2, DEG[k] / 12.0); }
static float *g12L = NULL, *g12R = NULL;
// v61: REVERSES — a kick, snare or tom synthesized into a scratch buffer and laid down backwards so it swells INTO `t`
static void rev_kick(double t, double g, double len) {
    long n = (long)(len * SR); float *b = (float *)calloc(n, sizeof(float)); double ph = 0;
    for (long i = 0; i < n; i++) { double u = (double)i / SR; ph += 2 * PI * (51.9 + 190 * exp(-u * 48)) / SR; b[i] = (float)(tanh(sin(ph) * 2.4) * exp(-u * 5.0)); }
    long a = at(t) - n; for (long i = 0; i < n; i++) { double w = fmin(1, (double)(n - 1 - i) / (0.004 * SR)); add(dL, a + i, b[n - 1 - i] * g * w); add(dR, a + i, b[n - 1 - i] * g * w); }
    free(b); ev(t - len, "rev-kick", len, g, -1);
}
static void rev_snare(double t, double g, double len) {
    long n = (long)(len * SR); float *b = (float *)calloc(n, sizeof(float)); double ph = 0, p1 = 0, b1 = 0;
    for (long i = 0; i < n; i++) { double u = (double)i / SR; ph += 2 * PI * 190 / SR; double nz = rnd(); b1 += (nz - b1) * 0.35; double hp = nz - b1; p1 += (hp - p1) * 0.6;
        b[i] = (float)(sin(ph) * 0.6 * exp(-u * 20) + p1 * 0.9 * exp(-u * 9)); }
    long a = at(t) - n; for (long i = 0; i < n; i++) add(pcM, a + i, b[n - 1 - i] * g * fmin(1, (double)(n - 1 - i) / (0.004 * SR)));
    free(b); ev(t - len, "rev-snare", len, g, -1);
}
static void rev_tom(double t, double hz, double g, double len) {
    long n = (long)(len * SR); float *b = (float *)calloc(n, sizeof(float)); double ph = 0;
    for (long i = 0; i < n; i++) { double u = (double)i / SR; ph += 2 * PI * hz * (1 + 0.6 * exp(-u * 30)) / SR; b[i] = (float)(tanh(sin(ph) * 1.6) * 0.8 * exp(-u * 5.5)); }
    long a = at(t) - n; for (long i = 0; i < n; i++) add(pcM, a + i, b[n - 1 - i] * g * fmin(1, (double)(n - 1 - i) / (0.004 * SR)));
    free(b); ev(t - len, "rev-tom", len, g, -1);
}
// v62: THE GRAND — AC OS's Salamander grand (CC0, fedac/native/samples/piano/<midi>.raw: f32 mono 48 kHz, every 3 semitones
// from A0). A note picks the nearest anchor and is resampled to pitch; velocity shapes level and a touch of brightness;
// a 120 ms release. Everything lands on a mono bus the listener hears from just right of her.
#define PIANO_DIR "/Users/jas/aesthetic-computer/fedac/native/samples/piano"
static float *PIANO_S[128]; static long PIANO_N[128]; static int PIANO_LOADED = 0;
static float *pianoM = NULL;
static void piano_load(void) { char pth[512]; int n = 0;
    for (int m = 21; m <= 108; m += 3) { snprintf(pth, sizeof pth, PIANO_DIR "/%d.raw", m); FILE *f = fopen(pth, "rb"); if (!f) continue;
        fseek(f, 0, SEEK_END); long bytes = ftell(f); fseek(f, 0, SEEK_SET); PIANO_S[m] = (float *)malloc(bytes); PIANO_N[m] = (long)fread(PIANO_S[m], 1, bytes, f) / 4; fclose(f); n++; }
    PIANO_LOADED = n; fprintf(stderr, "piano: %d anchors\n", n); }
static void piano(double t, int midi, double dur, double vel) {
    if (!PIANO_LOADED) return; int best = -1; for (int m = 21; m <= 108; m += 3) if (PIANO_S[m] && (best < 0 || fabs(m - midi) < fabs(best - midi))) best = m;
    if (best < 0) return; double r = pow(2, (midi - best) / 12.0), g = 0.9 * pow(vel, 1.3), pos = 0; long a = at(t), n = PIANO_N[best];
    long len = (long)((dur + 0.12) * SR); double lp = 0; const double kb = 1 - exp(-2 * PI * (2500 + 6000 * vel) / SR);
    for (long i = 0; i < len && pos < n - 1; i++, pos += r) { long j = (long)pos; double f = pos - j, x = PIANO_S[best][j] * (1 - f) + PIANO_S[best][j + 1] * f;
        lp += (x - lp) * kb; double u = (double)i / SR, env = u < dur ? 1 : exp(-(u - dur) / 0.04); add(pianoM, a + i, lp * g * env * fmin(1, i / (0.001 * SR))); }
    ev(t, "piano", dur, vel, midi);
}
static double seat_piano(double t) { return 26 + 4 * sin(2 * PI * t / 19); }
// v68: HER VOWEL CHOIR — "ooo" and "aaa" resynthesized from her own held vowels (bin/pitch-fx.py → src/vox/vowels/<v>-<midi>.wav),
// one note each at the chord tones, played as a small choir of her just behind her
static Stereo VOWEL[2][128]; static float *vowM[3] = { 0 };
static void vowel_load(void) { char pth[512]; int n = 0; static const char *VN[2] = { "oo", "aa" }; static const int VM[8] = { 56, 59, 63, 66, 68, 71, 75, 78 };
    for (int v = 0; v < 2; v++) for (int k = 0; k < 8; k++) { snprintf(pth, sizeof pth, LANE "/src/vox/vowels/%s-%d.wav", VN[v], VM[k]); if (access(pth, F_OK) == 0) { VOWEL[v][VM[k]] = load_wav(pth); n++; } }
    fprintf(stderr, "vowels: %d notes\n", n); }
static void vowel(double t, int midi, double dur, double g, int which, int seat) {
    const Stereo *sm = &VOWEL[which][midi]; if (!sm->L) return; long a = at(t), n = (long)fmin(sm->n, (dur + 0.6) * SR);
    for (long i = 0; i < n; i++) { double u = (double)i / SR, env = u < dur ? 1 : exp(-(u - dur) / 0.25); add(vowM[seat % 3], a + i, sm->L[i] * g * env); }
    ev(t, which ? "aaa" : "ooo", dur, g, midi); }
static double vow_az0(double t) { return -28 + 6 * sin(2 * PI * t / 15); }
static double vow_az1(double t) { return 0 + 6 * sin(2 * PI * t / 12 + 2); }
static double vow_az2(double t) { return 28 + 6 * sin(2 * PI * t / 14 + 4); }
// v64: THE HORSE — trancepenta's CC0 gallop (archive.org Red_Library_Animals_Horses_1 / R13-10) and stallion neigh (mixkit 1762),
// src/sfx/*.wav. The gallop's stride is 0.48 s = this record's beat, so one stride is triggered per kick, no stretching: it
// arrives at the break, passes right → left across the break and the bridge, and is gone by the finale.
static Stereo GALLOP, NEIGH; static float *horseM = NULL; static double HORSE_T0 = 0, HORSE_T1 = 1;
static double horse_az(double t) { double x = fmin(1, fmax(0, (t - HORSE_T0) / (HORSE_T1 - HORSE_T0))); return 80 - 160 * x; }
static void stride(double t, int k, double g) {   // one stride from the steady part of the sample (0.9 s on), the k-th of eight
    if (!GALLOP.L) return; long sa = at(0.9 + 0.48 * (k % 8)), n = (long)(0.52 * SR), a = at(t);
    for (long i = 0; i < n; i++) { double w = fmin(1, i / (0.01 * SR)) * fmin(1, (n - i) / (0.06 * SR)); add(horseM, a + i, sample(GALLOP.L, GALLOP.n, sa + i) * w * g); } }
static void neigh(double t, double rate, double g) { if (!NEIGH.L) return; long a = at(t); double pos = 0;
    for (long i = 0; pos < NEIGH.n - 1; i++, pos += rate) { long j = (long)pos; double f = pos - j; add(horseM, a + i, (NEIGH.L[j] * (1 - f) + NEIGH.L[j + 1] * f) * g); } ev(t, "neigh", NEIGH.n / SR / rate, g, -1); }
static void open_hat(double t, double g) {
    long a = at(t); double p1 = 0, p2 = 0;
    for (long i = 0; i < 0.24 * SR; i++) { double u = (double)i / SR, nz = rnd(), h1 = nz - p1; p1 = nz; double h2 = h1 - p2; p2 = h1;
        double v = h2 * fmin(1, u / 0.002) * exp(-u * 11) * g * 0.34; add(tL, a + i, v * 0.8); add(tR, a + i, v * 1.1); }   // v38: longer, louder — club offbeats
}
// house bass: a saturated sine pluck with a closing lowpass, on the offbeat
static void house_bass(double t, int midi, double dur, double g) {
    long a = at(t); double f = mtof(midi), ph = 0, lp = 0;
    for (long i = 0; i < (dur + 0.05) * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * f / SR;
        double raw = tanh(sin(ph) * 4.0 + 0.6 * sin(2 * ph));
        double fc = 180 + 2400 * exp(-u * 14), k = 1 - exp(-2 * PI * fc / SR);
        lp += (raw - lp) * k;
        double env = fmin(1, u / 0.003) * (u > dur ? exp(-(u - dur) * 60) : 1);
        add(b808, a + i, lp * env * g); }
}
// riser: white noise through a rising bandpass, swelling into the downbeat
static void riser(double t0, double t1, double g) {
    long a = at(t0), len = at(t1) - a; double b1 = 0, b2 = 0;
    for (long i = 0; i < len; i++) { double x = (double)i / len, fc = 300 * pow(40, x), k = 1 - exp(-2 * PI * fc / SR);
        double nz = rnd(); b1 += (nz - b1) * k; b2 += (b1 - b2) * k;
        double v = (b1 - b2) * x * x * g; add(rsL, a + i, v * (1 - 0.3 * sin(PI * 8 * x))); add(rsR, a + i, v * (1 + 0.3 * sin(PI * 8 * x))); }
}
//                    4otf  clap  ohat  hbass  (v5.2)
static const double DANCE[NSEC][4] = {
    [INTRO] = { 0, 0, 0, 0 }, [VERSE1] = { .8, 0, .5, .7 }, [CHORUS1] = { 1.1, 1, .9, 1.1 }, [VERSE2] = { .5, 0, .3, .6 },
    [CHORUS2] = { 1.25, 1, 1.1, 1.1 }, [BREAK] = { .9, .5, .6, .8 }, [BRIDGE] = { 1.2, .9, 1, 1 }, [OUTRO] = { 1.3, 1, 1.1, 1.1 } };   // v24: the break keeps the beat; the outro is the finale

// ── hand percussion (v5.1: "we have no actual percussion yet") ───────────
// shaker: band-passed noise with a swell-in (the bead mass lags the hand)
static void shaker(double t, double g, double pan) {
    long a = at(t); double b1 = 0, b2 = 0, prev = 0;
    for (long i = 0; i < 0.07 * SR; i++) { double u = (double)i / SR, nz = rnd(), hp = nz - prev; prev = nz;
        b1 += (hp - b1) * 0.55; b2 += (b1 - b2) * 0.55;
        double env = fmin(1, u / 0.012) * exp(-u * 45);
        double v = (b1 - b2 * 0.6) * env * g; add(pL, a + i, v * (1 - pan)); add(pR, a + i, v * (1 + pan)); }
}
// tambourine: jingles = noise through three metallic partials, a short hit + ring
static void tamb(double t, double g, double pan) {
    static const double F[3] = { 7040, 9360, 11870 };
    long a = at(t); double prev = 0;
    for (long i = 0; i < 0.22 * SR; i++) { double u = (double)i / SR, nz = rnd(), hp = nz - prev, v = 0; prev = nz;
        for (int k = 0; k < 3; k++) v += sin(2 * PI * F[k] * u + k) * 0.25;
        v = (v * (0.6 + 0.4 * nz) + hp * 0.5) * exp(-u * 18) * g;
        add(pL, a + i, v * (1 - pan)); add(pR, a + i, v * (1 + pan)); }
}
// conga: a sine membrane with a pitch drop, tuned to the chord; slap = + noise
static void conga(double t, int midi, double g, double pan, int slap) {
    long a = at(t); double f = mtof(midi), ph = 0, prev = 0;
    for (long i = 0; i < 0.3 * SR; i++) { double u = (double)i / SR, nz = rnd(), hp = nz - prev; prev = nz;
        ph += 2 * PI * f * (1 + 0.35 * exp(-u * 60)) / SR;
        double v = sin(ph) * exp(-u * (slap ? 22 : 11)) + (slap ? hp * exp(-u * 80) * 0.8 : 0);
        v *= g; add(pL, a + i, v * (1 - pan)); add(pR, a + i, v * (1 + pan)); }
}
// woodblock: two short resonances, for the tresillo x..x..x. (E(3,8)) that
// runs 3-against-her-2 across the bar (rhythm-platter 01: the Cuban tresillo)
static void block(double t, double g, double pan) {
    long a = at(t);
    for (long i = 0; i < 0.05 * SR; i++) { double u = (double)i / SR;
        double v = (sin(2 * PI * 1180 * u) * 0.7 + sin(2 * PI * 2350 * u) * 0.4) * exp(-u * 110) * g;
        add(pL, a + i, v * (1 - pan)); add(pR, a + i, v * (1 + pan)); }
}
// the jet: decorrelated white noise through a lowpass that opens 300 Hz → 9 kHz
// over the span, amplitude rising as x^1.6 — an airplane taking off into the drop
static float *jL, *jR;
static void jet(double t0, double t1, double g) {
    long a = at(t0), len = at(t1) - a; double l1 = 0, l2 = 0, r1 = 0, r2 = 0;
    for (long i = 0; i < len; i++) { double x = (double)i / len, fc = 300 * pow(30, x), k = 1 - exp(-2 * PI * fc / SR);
        double nl = rnd(), nr = rnd(); l1 += (nl - l1) * k; l2 += (l1 - l2) * k; r1 += (nr - r1) * k; r2 += (r1 - r2) * k;
        double env = pow(x, 1.6) * g; add(jL, a + i, l2 * env); add(jR, a + i, r2 * env); }
}
// air: a steady wide hiss (high-passed noise) under the choruses, pumped by the kick in the mix
static void air(double t0, double t1, double g) {
    long a = at(t0), len = at(t1) - a; double pl1 = 0, pr1 = 0;
    for (long i = 0; i < len; i++) { double nl = rnd(), nr = rnd(), hl = nl - pl1, hr = nr - pr1; pl1 = nl; pr1 = nr;
        double x = (double)i / len, env = fmin(1, x / 0.1) * fmin(1, (1 - x) / 0.1) * g;
        add(jL, a + i, hl * env * 0.5); add(jR, a + i, hr * env * 0.5); }
}
//                     shaker tamb  conga
static const double PERC[NSEC][3] = {
    [INTRO] = { 0, 0, 0 }, [VERSE1] = { 0, 0, 0 }, [CHORUS1] = { 0, 0, 0 }, [VERSE2] = { 0, 0, .5 },   // v28: the shaker carries verse 1 under her voice (v30b: no congas there — they read as toms); v27: congas in verse 2
    [CHORUS2] = { 0, 0, 0 }, [BREAK] = { 0, 0, 0 }, [BRIDGE] = { 0, 0, 0 }, [OUTRO] = { 0, 0, 0 } };

// ── pop acoustic chain for her guitar (v5.1: "doesn't feel poppy") ───────
// RBJ biquads: HP 110 Hz · −4 dB bell at 400 Hz (the box) · +4 dB shelf at
// 10 kHz (sparkle) → fast 4:1 compressor (snap + sustain) → M/S width 1.5.
typedef struct { double b0, b1, b2, a1, a2, x1, x2, y1, y2; } Biquad;
static Biquad bq(int type, double f, double q, double db) {
    double A = pow(10, db / 40), w = 2 * PI * f / SR, c = cos(w), sn = sin(w), al = sn / (2 * q), b0, b1, b2, a0, a1, a2;
    if (type == 0) { b0 = (1 + c) / 2; b1 = -(1 + c); b2 = b0; a0 = 1 + al; a1 = -2 * c; a2 = 1 - al; }            // highpass
    else if (type == 1) { b0 = 1 + al * A; b1 = -2 * c; b2 = 1 - al * A; a0 = 1 + al / A; a1 = -2 * c; a2 = 1 - al / A; }  // bell
    else { double sq = 2 * sqrt(A) * al;                                                                            // high shelf
        b0 = A * ((A + 1) + (A - 1) * c + sq); b1 = -2 * A * ((A - 1) + (A + 1) * c); b2 = A * ((A + 1) + (A - 1) * c - sq);
        a0 = (A + 1) - (A - 1) * c + sq; a1 = 2 * ((A - 1) - (A + 1) * c); a2 = (A + 1) - (A - 1) * c - sq; }
    Biquad b = { b0 / a0, b1 / a0, b2 / a0, a1 / a0, a2 / a0, 0, 0, 0, 0 };
    return b;
}
static double bq_run(Biquad *b, double x) {
    double y = b->b0 * x + b->b1 * b->x1 + b->b2 * b->x2 - b->a1 * b->y1 - b->a2 * b->y2;
    b->x2 = b->x1; b->x1 = x; b->y2 = b->y1; b->y1 = y; return y;
}
// v6: bedroom leveler — her opening strums are 20 dB under her playing at
// bar 11. Before bar 16 the guitar rides a slow RMS leveler (target −18 dBFS,
// up to +16 dB, 30 ms / 500 ms) that eases back to unity by UNTIL.
static void bedroom_level(Stereo *g, double until) {
    if (!g->L) return;
    double env = 0, gain = 1; const double aA = exp(-1 / (0.03 * SR)), aR = exp(-1 / (0.5 * SR)), target = 0.126;
    for (long i = 0; i < g->n; i++) {
        double t = (double)i / SR, m = (g->L[i] + g->R[i]) / 2, pw = m * m;
        env = pw > env ? aA * env + (1 - aA) * pw : aR * env + (1 - aR) * pw;
        double want = env > 1e-7 ? fmin(6.3, target / sqrt(env)) : 6.3;              // ≤ +16 dB
        double w = t >= until ? 0 : t < until - 8 ? 1 : 0.5 + 0.5 * cos(PI * (t - (until - 8)) / 8);
        gain += ((1 + (want - 1) * w) - gain) * 0.0005;
        g->L[i] *= (float)gain; g->R[i] *= (float)gain;
    }
}
static void pop_guitar(Stereo *g, double width) {
    if (!g->L) return;
    for (int ch = 0; ch < 2; ch++) {
        float *x = ch ? g->R : g->L;
        Biquad hp = bq(0, 110, 0.707, 0), box = bq(1, 400, 1.0, -4), air = bq(2, 10000, 0.707, 4);
        double env = 0, gc = 1, aA = exp(-1 / (0.003 * SR)), aR = exp(-1 / (0.08 * SR));
        double rms = 0; for (long i = 0; i < g->n; i++) rms += (double)x[i] * x[i];
        double th = sqrt(rms / g->n) * 1.2;
        for (long i = 0; i < g->n; i++) {
            double y = bq_run(&air, bq_run(&box, bq_run(&hp, x[i])));
            double lv = fabs(y); env = lv > env ? aA * env + (1 - aA) * lv : aR * env + (1 - aR) * lv;
            double gt = env > th ? pow(env / th, 1.0 / 4 - 1) : 1;
            gc += (gt - gc) * 0.02;
            x[i] = (float)(y * gc * 1.8);
        }
    }
    for (long i = 0; i < g->n; i++) { double m = (g->L[i] + g->R[i]) / 2, sd = (g->L[i] - g->R[i]) / 2 * width;
        g->L[i] = (float)(m + sd); g->R[i] = (float)(m - sd); }
}

int main(void) {
    // ── her ──
    // v6: stems on the regularized clock (bin/regularize.mjs → src/vox/reg); SAILOR_STEMS=locked to follow her raw
    // v9: the edit (bin/splice.mjs) lives in src/vox/cut — played by default when it exists
    const char *stems = getenv("SAILOR_STEMS") ? getenv("SAILOR_STEMS") : (access(LANE "/src/vox/cut/vocals-aesthetivox.wav", F_OK) == 0 ? "cut" : "reg");
    const int regd = strcmp(stems, "reg") == 0 || strcmp(stems, "cut") == 0;   // raw + locked: her guitar from src/vox
    char pth[512];
    #define STEM(name) (snprintf(pth, sizeof pth, LANE "/src/vox/%s/%s.wav", stems, name), pth)
    #define GSTEM(name) (regd ? STEM(name) : (snprintf(pth, sizeof pth, LANE "/src/vox/%s.wav", name), pth))
    // v19: her recorded voice, gently pitch-mapped (bin/natural-vox.py) — not the WORLD resynthesis
    snprintf(pth, sizeof pth, LANE "/src/vox/%s/vocals-natural.wav", stems);
    Stereo vox  = load_wav(access(pth, F_OK) == 0 ? pth : STEM("vocals-aesthetivox"));
    Stereo halo = load_wav(STEM("vocals-halo"));
    Stereo gtr  = load_wav(GSTEM("guitar-48k"));
    Stereo acg  = load_wav(GSTEM("replay-acoustic"));
    Stereo elg  = load_wav(GSTEM("replay-electric"));
    Stereo harm[NHARM];
    for (int h = 0; h < NHARM; h++) harm[h] = load_wav(STEM(HARM_FILE[h]));
    // v7: accompaniment voices (bin/choir.py) — her own vocoder choir and @jeffrey's vowel drone
    static const char *CHOIR_FILE[3] = { "choir-low", "choir-mid", "choir-high" };
    Stereo choir[3], jeff[2];
    for (int c = 0; c < 3; c++) choir[c] = load_wav(STEM(CHOIR_FILE[c]));
    jeff[0] = load_wav(STEM("jeffrey-root")); jeff[1] = load_wav(STEM("jeffrey-fifth"));
    Stereo sisHi = load_wav(STEM("sister-high")), sisLo = load_wav(STEM("sister-low")), sisHum = load_wav(STEM("sister-hum"));   // v11: bin/sister.py
    Stereo orch[NORCH]; int norch = 0;
    for (int o = 0; o < NORCH; o++) { snprintf(pth, sizeof pth, LANE "/src/orch/%s.wav", ORCH_FILE[o]); orch[o] = access(pth, F_OK) == 0 ? load_wav(pth) : (Stereo){ 0 }; norch += !!orch[o].L; }
    // v33: the OCTAVE FX (bin/pitch-fx.py → src/vox/fx): WORLD resyntheses of her own vowel — two held "long"s sliding an
    // octave down (crossfaded IN over her lead), and an octave-down ghost under her opening line
    // v36: the held "long"s RISE an octave and keep going (bin/pitch-fx.py: f0 cleaned, the vowel extended on her own frames);
    // the engine replaces her lead with the rise, and lays a gliding sine + bells on every scale step the glide crosses (the .curve)
    // v55: a fourth: "kiss" itself, placed a beat and a bit before where it was sung and held to "me" (60.84) — only the start of the word moves
    static struct { const char *name; double t0, t1, orig; int slide; } FX[4] = { { "rise-long-1", 86.40, 0, 90.95, 1 }, { "rise-long-2", 132.68, 0, 137.08, 1 }, { "hold-out", 159.40, 0, 160.97, 1 }, { "kiss-hold-OFF", 60.07, 0, 60.84, 1 } };   // v57: off — her timing
    Stereo fxS[4]; int nfx = 0; for (int q = 0; q < 4; q++) { snprintf(pth, sizeof pth, LANE "/src/vox/fx/%s.wav", FX[q].name); fxS[q] = access(pth, F_OK) == 0 ? load_wav(pth) : (Stereo){ 0 }; nfx += !!fxS[q].L;
        if (fxS[q].L && FX[q].t1 == 0) FX[q].t1 = FX[q].t0 + (double)fxS[q].n / SR; }
    int ntom = 0; for (int q = 0; q < 4; q++) { snprintf(pth, sizeof pth, LANE "/src/kit/tom-%d.wav", q + 1); TOM_S[q] = access(pth, F_OK) == 0 ? load_wav(pth) : (Stereo){ 0 };
        if (TOM_S[q].L) { double pk = 0; for (long i = 0; i < TOM_S[q].n; i++) pk = fmax(pk, fabs(TOM_S[q].L[i])); if (pk > 0) for (long i = 0; i < TOM_S[q].n; i++) TOM_S[q].L[i] *= (float)(0.8 / pk); ntom++; } }
    fprintf(stderr, "stems: %s · orchestra: %d/%d parts · toms: %d/4 samples · octave fx: %d/3\n", stems, norch, NORCH, ntom, nfx);
    // v7: her first sung word = the first 30 ms window of the lead stem after bar START_BAR
    // above −45 dBFS RMS (guitar bleed in the stem sits near −60; her voice enters near −35)
    double startSecOut = bar_n(START_BAR)->t;
    { long w = (long)(0.03 * SR), from = at(bar_n(START_BAR)->t);
      for (long i = from; i + w < vox.n; i += w / 3) { double acc = 0; for (long j = 0; j < w; j++) acc += (double)vox.L[i + j] * vox.L[i + j];
          if (sqrt(acc / w) > 0.0056) { startSecOut = (double)i / SR - 0.06; break; } } }
    if (!vox.L || !gtr.L) { fprintf(stderr, "! run bin/aesthetivox.py + bin/lock-vox.mjs first\n"); return 1; }
    // v52: THE HESITATION — "27.4 is where 'kiss' should start, but she hesitated": after "you" (ends 60.06) there is 0.4 s of
    // nothing, the false "k-" (60.46), then "kiss" at 60.68. The window 60.08–60.66 is removed from her VOICE only (every stem made
    // from it) and "kiss me on the mouth … sailor?" slides 0.58 s earlier onto 27.4; the time comes back as a breath before "And"
    // (64.29). Her guitar and the band do not move. Her chart notes shift with her (bin/orchestra.mjs mirrors this: VOX_CUT).
    if (0) { const double ca = 60.08, cz = 60.66, until = 64.29, d = cz - ca; Stereo *all[16]; int na = 0; all[na++] = &vox; all[na++] = &halo;   // v53: OFF — the regularizer fits bar 27 instead
      for (int h = 0; h < NHARM; h++) all[na++] = &harm[h]; for (int c = 0; c < 3; c++) all[na++] = &choir[c]; all[na++] = &sisHi; all[na++] = &sisLo; all[na++] = &sisHum;
      long A = at(ca), Z = at(cz), U = at(until), D = Z - A, xf = (long)(0.006 * SR);
      for (int q = 0; q < na; q++) { Stereo *st = all[q]; if (!st->L || U > st->n) continue; float *ch[2] = { st->L, st->R };
          for (int c = 0; c < 2; c++) { float *b = ch[c]; if (!b) continue;
              for (long i = A; i < U - D; i++) { double w = i < A + xf ? (double)(i - A) / xf : 1; b[i] = (float)(b[i] * (1 - w) + b[i + D] * w); }
              for (long i = U - D; i < U; i++) { double w = fmin(1, (double)(i - (U - D)) / xf); b[i] = (float)(b[i + D < U ? i + D : U - 1] * (1 - w)); } } }
      ChartNote *cn = (ChartNote *)CHART_NOTES; for (int k = 0; k < CHART_NNOTES; k++) if (cn[k].t >= ca && cn[k].t < until) cn[k].t -= d;   // her notes move with her
      fprintf(stderr, "hesitation: %.2f s of her voice removed at %.2f — \"kiss\" now at %.2f\n", d, ca, 60.68 - d); }
    // v49: her "k-" false start before chorus 1's "kiss" (60.440–60.660 s) is cut from the performance — from her lead AND every
    // stem made from it (halo, harmonies, choir, sisters), since the v48 lead-only gate left the burst audible in the doubles
    if (0) { const double ka = 60.440, kz = 60.660, r = 0.006; Stereo *all[16]; int na = 0; all[na++] = &vox; all[na++] = &halo;   // v57: OFF — "let's go back to putting the glitch in … and enhance it"
      // v55: the stems made from her voice (not the lead) go quiet from 59.57 to 60.84 — the lead's "kiss" now lives there
      { Stereo *dv[16]; int nd = 0; dv[nd++] = &halo; for (int h = 0; h < NHARM; h++) dv[nd++] = &harm[h]; for (int c = 0; c < 3; c++) dv[nd++] = &choir[c]; dv[nd++] = &sisHi; dv[nd++] = &sisLo; dv[nd++] = &sisHum;
        for (int q = 0; q < nd; q++) { Stereo *st = dv[q]; if (!st->L) continue; for (long i = at(60.05); i < at(60.86) && i < st->n; i++) { double t = (double)i / SR, g = t < 60.07 ? (60.07 - t) / 0.02 : t > 60.84 ? fmin(1, (t - 60.84) / 0.02) : 0; st->L[i] *= (float)g; if (st->R) st->R[i] *= (float)g; } } }
      for (int h = 0; h < NHARM; h++) all[na++] = &harm[h]; for (int c = 0; c < 3; c++) all[na++] = &choir[c]; all[na++] = &sisHi; all[na++] = &sisLo; all[na++] = &sisHum;
      for (int q = 0; q < na; q++) { Stereo *st = all[q]; if (!st->L) continue;
          for (long i = at(ka - r); i < at(kz + r + 0.04) && i < st->n; i++) { double t = (double)i / SR, g = t < ka ? (ka - t) / r : t > kz ? fmin(1, (t - kz) / r) : 0;
              if (i >= 0) { st->L[i] *= (float)g; if (st->R) st->R[i] *= (float)g; } } } }
    // v25b: "the start still has weird intro guitar" — no lowpass. v25c: "a few twangs before she starts singing,
    // smoothed": two bars of her guitar at its own level, risen out of silence by a 2 s fade on the whole mix
    const double firstWordT = startSecOut;   // v27: her first word (the detection above)
    // v63: THE TAPE-START — her first utterance begins 0.83 s before she really sang it, at 0.45× (−14 semitones), and glides to
    // 1× over 3 s; the rate's deficit over the glide equals the head start, so by the end she is exactly in time with herself
    // and the engine hands back to her own stem. Written into the lead in place, so every double follows.
    if (0) { const double r0 = 0.45, D = 3.0, X = D * (1 - (r0 + 1) / 2); const double t0 = firstWordT - X, tz = t0 + D; long a0 = at(t0), az = at(tz);   // v68: OFF — "the first 'I saw' still needs to be hearable"
      float *tmp = (float *)calloc(az - a0 + 1, sizeof(float)); double pos = firstWordT * SR;
      for (long i = a0; i < az; i++) { double u = (double)(i - a0) / (az - a0), k = u * u * (3 - 2 * u), r = r0 + (1 - r0) * k;
          long j = (long)pos; double f = pos - j; tmp[i - a0] = (float)(sample(vox.L, vox.n, j) * (1 - f) + sample(vox.L, vox.n, j + 1) * f); pos += r; }
      double drift = pos / SR - tz;   // should be ~0: where the glide hands back to her
      for (long i = a0; i < az && i < vox.n; i++) { double w = fmin(1, (double)(az - i) / (0.03 * SR)); vox.L[i] = (float)(tmp[i - a0] * w + sample(vox.L, vox.n, i) * (1 - w) * (i >= at(firstWordT) ? 0 : 1)); if (vox.R && vox.R != vox.L) vox.R[i] = vox.L[i]; }
      free(tmp); fprintf(stderr, "tape-start: %.2f → %.2f s, hand-back drift %.3f s\n", t0, tz, drift); }
    // v72: "start right with her vocal" — the record opens on her first word (60 ms early), no pre-roll, no radio
    N = vox.n + 3 * SR;   // v27: three seconds past the stems so the button, the 808 and the 7.5 s tail release (v26 faded them at 0.4 s)
    pL = zeros(); pR = zeros();
    bedroom_level(&gtr, bar_n(16)->t);                                 // v6: until bar 16
    pop_guitar(&gtr, 1.5); pop_guitar(&acg, 1.3);
    // v59: HER HAND — the strum attacks lifted: the 2–6 kHz band's fast envelope against its slow one, the burst of a pick/nail boosted for its length
    { float *ch[2] = { gtr.L, gtr.R }; const double kh = 1 - exp(-2 * PI * 2000 / SR), kl = 1 - exp(-2 * PI * 6000 / SR);
      for (int c = 0; c < 2; c++) { double h = 0, l = 0, fast = 0, slow = 0; float *b = ch[c];
          for (long i = 0; i < gtr.n; i++) { double x = b[i]; h += (x - h) * kh; double hf = x - h; l += (hf - l) * kl; double band = l, a = fabs(band);
              fast = a > fast ? fast + (a - fast) * 0.3 : fast * 0.998; slow += (a - slow) * 0.0015;
              double boost = fmin(1.8, fmax(0, (fast / (slow + 1e-4) - 1.5) * 0.8)); b[i] = (float)(x + band * boost * 0.8); } } }
    { double lp = 0; const double k15 = 1 - exp(-2 * PI * 1500 / SR);   // v27: mono above 1.5 kHz — the phone's highs were out of phase
      for (long i = 0; i < gtr.n; i++) { double sd = (gtr.L[i] - gtr.R[i]) / 2; lp += (sd - lp) * k15; double hp = sd - lp; gtr.L[i] -= (float)hp; gtr.R[i] += (float)hp; } }
    dL = zeros(); dR = zeros(); tL = zeros(); tR = zeros(); b808 = zeros(); sL = zeros(); sR = zeros();
    hiM = zeros(); bellM = zeros(); vibM = zeros(); duck = zeros(); exM = zeros(); jL = zeros(); jR = zeros(); hookM = zeros();
    for (int v = 0; v < 3; v++) padM[v] = zeros();
    wubM = zeros(); rsL = zeros(); rsR = zeros(); melM = zeros(); mirM = zeros(); radL = zeros(); radR = zeros(); g12L = zeros(); g12R = zeros(); pcM = zeros(); pianoM = zeros(); piano_load();
    for (int q = 0; q < 3; q++) vowM[q] = zeros(); vowel_load();
    horseM = zeros(); GALLOP = access(LANE "/src/sfx/gallop.wav", F_OK) == 0 ? load_wav(LANE "/src/sfx/gallop.wav") : (Stereo){ 0 }; NEIGH = access(LANE "/src/sfx/neigh.wav", F_OK) == 0 ? load_wav(LANE "/src/sfx/neigh.wav") : (Stereo){ 0 };
    HORSE_T0 = bar_n(68)->t; HORSE_T1 = bar_n(80)->t + bar_n(80)->dur; fprintf(stderr, "horse: gallop %s, neigh %s\n", GALLOP.L ? "yes" : "no", NEIGH.L ? "yes" : "no");
    // v59: the 12-string — her guitar an octave up, quietly, under the choruses, the bridge climb and the finale
    { static const int SEG[4][2] = { { 28, 43 }, { 52, 67 }, { 77, 80 }, { 81, 82 } }; static const double G12[4] = { .22, .3, .3, .35 };
      for (int q = 0; q < 4; q++) { long a = at(bar_n(SEG[q][0])->t), z = at(bar_n(SEG[q][1])->t + bar_n(SEG[q][1])->dur);
          shift_into(gtr.L, gtr.n, g12L, a, z, ratio_octave, G12[q]); shift_into(gtr.R, gtr.n, g12R, a, z, ratio_octave, G12[q]); } }
    // v59: THE CLIMB — out of the break, bars 71–72, her guitar steps up the scale an 8th at a time to the octave, into the bridge
    { const ChartBar *b71 = bar_n(71), *b72 = bar_n(72); CLIMB_T0 = b71->t; CLIMB_T1 = b72->t + b72->dur; CLIMB_STEP = (CLIMB_T1 - CLIMB_T0) / 8;
      shift_into(gtr.L, gtr.n, g12L, at(CLIMB_T0), at(CLIMB_T1), ratio_climb, 0.9); shift_into(gtr.R, gtr.n, g12R, at(CLIMB_T0), at(CLIMB_T1), ratio_climb, 0.9);
      ev(CLIMB_T0, "gtr-climb", CLIMB_T1 - CLIMB_T0, 1, -1); }
    { const ChartBar *b73 = bar_n(73), *b76 = bar_n(76); PITCHY_T0 = b73->t; PITCHY_T1 = b76->t + b76->dur; PITCHY_BEAT = b73->dur / b73->nb;   // v73: the pitchy guitar in the dead zone
      shift_into(gtr.L, gtr.n, g12L, at(PITCHY_T0), at(PITCHY_T1), ratio_pitchy, 0.85); shift_into(gtr.R, gtr.n, g12R, at(PITCHY_T0), at(PITCHY_T1), ratio_pitchy, 0.85); ev(PITCHY_T0, "gtr-pitchy", PITCHY_T1 - PITCHY_T0, 1, -1); } float *stM = zeros(), *throwM = zeros();   // v24: the wub bass and the stutter; v25: the throws
    for (int h = 0; h < NHARM; h++) { arpG[h] = zeros(); arpAz[h] = zeros(); }
    for (int q = 0; q < NIMPACT; q++) IMPACT_T[q] = -1e9;
    { static const int TURN_BARS[4] = { 27, 51, 72, 80 }; for (int k = 0; k < 4; k++) { const ChartBar *tb = bar_n(TURN_BARS[k]); if (pickup_bar(tb->n)) { TURN_D[k] = tb->dur / tb->nb; TURN_T0[k] = pickup_t(tb) - TURN_D[k]; } else { TURN_T0[k] = tb->t + tb->dur / 2; TURN_D[k] = tb->dur / 2; } }   // v28: a whip; v35: into the pickup; v49: the beat before the pickup bar
      FIN_T0 = bar_n(81)->t; FIN_LAP = 8 * bar_n(81)->dur;
      SHIFT_T[0] = bar_n(56)->t; SHIFT_T[1] = bar_n(60)->t; SHIFT_T[2] = bar_n(64)->t; SPIN_T0 = bar_n(70)->t; SPIN_T1 = bar_n(73)->t;
      LAP_T0 = bar_n(52)->t; LAP_T1 = bar_n(60)->t; WOB_T0 = bar_n(77)->t; WOB_T1 = bar_n(81)->t; }   // v25: the turns; v61: shifts + the spin; v65: the lap + the wobble
    EV = fopen(LANE "/out/sailor-song-" VERSION ".events.json", "w");
    if (EV) fprintf(EV, "{\"tempoBPM\":120,\"seconds\":%.2f,\"startSec\":%.3f,\"startBar\":%d,\"floorBar\":%d,\"endBar\":%d,\"stems\":\"%s\",\"sections\":[", (double)N / SR, startSecOut, START_BAR, FLOOR_BAR, END_BAR, stems);
    if (EV) for (int s = 0; s < NSEC; s++) {
        // v13: a section is the longest contiguous run of bars carrying its numbers (the arrangement reorders bars)
        int lo = SEC_FROM[s], hi = s + 1 < NSEC ? SEC_FROM[s + 1] - 1 : 999, bi = -1, bl = 0;
        for (int k = 0; k < CHART_NBARS;) { if (CHART_BARS[k].n < lo || CHART_BARS[k].n > hi) { k++; continue; }
            int j = k; while (j < CHART_NBARS && CHART_BARS[j].n >= lo && CHART_BARS[j].n <= hi) j++;
            if (j - k > bl) { bl = j - k; bi = k; } k = j; }
        if (bi < 0) continue;
        const ChartBar *a = &CHART_BARS[bi], *z = &CHART_BARS[bi + bl - 1];
        fprintf(EV, "%s{\"name\":\"%s\",\"start\":%.3f,\"end\":%.3f}", s ? "," : "", SEC_NAME[s], a->t, z->t + z->dur);
    }
    if (EV) fprintf(EV, "],\"bars\":[");
    if (EV) for (int k = 0; k < CHART_NBARS; k++) { static const char *CN[3] = { "G#m", "Emaj7", "B" };
        fprintf(EV, "%s{\"n\":%d,\"t\":%.3f,\"dur\":%.3f,\"chord\":\"%s\",\"section\":\"%s\"}", k ? "," : "", CHART_BARS[k].n, CHART_BARS[k].t, CHART_BARS[k].dur, CN[CHART_BARS[k].chord], SEC_NAME[section_of(CHART_BARS[k].n)]); }
    if (EV) fprintf(EV, "],\"events\":[\n");

    // ── score the bars ──
    int tenor[3] = { 68, 71, 75 }, high[2] = { 80, 83 }, desc = 90;   // v6.3: the tenor trio clears her top (G#4 = 68)
    lead(tenor, 3, CHORD_PCS[0], 67, 79);
    for (int k = 0; k < CHART_NBARS; k++) {
        const ChartBar *b = &CHART_BARS[k];
        const int s = section_of(b->n); const Arr *o = &ARR[s];
        const double *bt = b->beats; const int nb = b->nb; const int *pcs = CHORD_PCS[b->chord];
        const double beatDur = b->dur / nb;
        const double swell = s == BRIDGE ? 0.75 + 0.25 * (b->n - 72) / 8.0 : 1;
        #define MID(j) (bt[j] + (bt[(j) + 1] - bt[j]) / 2)
        // pocket on her strum points
        static const double DZ0[4] = { 0, 0, 0, 0 };
        const int release = s == OUTRO && b->n >= 82;                                      // v27: her ring-out, the band leaves; v41: from 82 — the last word dissolves
        const double *dzT = ((s == BREAK && b->n < 70) || release) ? DZ0 : DANCE[s];      // v27: the break's first two bars have no kit
        const double dzv[4] = { dzT[0], KICK_ONLY ? 0 : dzT[1], KICK_ONLY ? 0 : dzT[2], dzT[3] }; const double *dz = dzv;   // v48: kick only
        const double kickLift = b->n >= 81 ? 1.41 : b->n >= 52 ? 1.26 : 1;                // v27: the kick grows (+2 dB at the floor, +3 at the finale)
        for (int q = 0; q < NIMPACT; q++) if (IMPACT_BARS[q] == b->n) {
            const ChartBar *pb = bar_n(b->n - 1); const int pk = pb && pickup_bar(pb->n);
            const double tI = bt[0] + VOICE_LAG; (void)pk;                          // v56: the implosion lands on "kiss" — its whoosh was covering "Oh, won't you"
            IMPACT_T[q] = tI; if (b->n != 28 && b->n != 52) { explosion(tI, b->n == 81 ? 1.0 : 0.8); blast(tI, b->n == 81 ? 0.9 : 0.6); }
            else gong(tI, 78, b->n == 52 ? 0.75 : 0.6);   // v59: "kiss" rings a FEM gong (her F#4 an octave up), not a filter sweep
            }   // v11: implosions; v25: at every lift; v28: and a blast out; v33: chorus 1's is the biggest
        for (int q = 0; q < NIMPACT; q++) if (IMPACT_BARS[q] == b->n + 2) { const ChartBar *nx = &CHART_BARS[k + 1 < CHART_NBARS ? k + 1 : k]; double tEnd = pickup_bar(nx->n) ? pickup_t(nx) : CHART_BARS[k + 2 < CHART_NBARS ? k + 2 : k].t;
            (void)tEnd; }   // v39: no risers — "filter sweeps are a little cheesy" (the implosion/blast stay)   // v25: a two-bar noise riser into each lift; v35: ending on the pickup
        const int pos = k + 1, posV2 = pos_n(44), posC1 = pos_n_after(27, posV2);                   // v13: positions in the record
        // v22: by section — the take plays in its own order now. Chorus 1 is big but not the floor, verse 2 pulls back, chorus 2 lands whole
        const double gDrop = b->n >= FLOOR_BAR ? 1 : s == CHORUS1 ? 0.6 + 0.2 * evo(b->n, 28, 36) : s == VERSE2 ? 0.45 : s == VERSE1 ? 0.5 * evo(b->n, 20, 28) : 0;   // v27: verse 1's pads, high pair and pickup bass were gated to 0   (void)pos; (void)posV2; (void)posC1;
        // v6.6: hills — every section rises to its middle and settles into its edge
        const int secLen = (s + 1 < NSEC ? SEC_FROM[s + 1] : CHART_NBARS + 1) - SEC_FROM[s];
        const double hill = s == INTRO ? 1 : 0.84 + 0.16 * sin(PI * (b->n - SEC_FROM[s] + 0.5) / secLen);
        // v6: the evolution gates (verse 1 only; every other section is its table)
        const int v1 = s == VERSE1;
        // v22: "start the kick sooner, slower build up" — her 1 and 3 from bar 14, soft, growing all the way to chorus 1
        // v25d: "the kick is too loud at start" — from 12%, rising on a square
        // v31b: "the first 7 seconds no kicks" — hats from her first bar, the kick from bar 15 (12 %), rising on a square
        const double gKick = s == VERSE1 ? (b->n >= 15 ? 0.12 + 0.88 * pow(evo(b->n, 15, 27), 2) : 0) : 1, gHat = s == VERSE1 ? 0 : s == VERSE2 ? 0.5 : 1, gBass = s == VERSE1 ? evo(b->n, 24, 28) : 1;
        const double swellIn = s == CHORUS1 ? 0.25 + 0.75 * evo(b->n, 28, 32) : s == CHORUS2 ? 0.25 + 0.75 * evo(b->n, 52, 56) : 1;   // v67: the chorus bed arrives over four bars, not on the downbeat
        const double gSub = evo(b->n, 16, 22), gTen = evo(b->n, 20, 26) * swellIn, gShk = s == VERSE1 ? 0.6 + 0.4 * evo(b->n, 11, 16) : 1, gCon = s == VERSE1 ? 0.5 + 0.5 * evo(b->n, 11, 18) : 1;   // v28: the percussion is there from her first word   // v7: her first line alone; the shuffle from bar 14, congas from 18
        // v6 HOCKET: the floor doubles her 1 and 3 and lands in her empty 2 and 4;
        // open hats on the complement's &1/&3; her own &2/&4 get a soft accent at her bin
        // v26: VARIATION — chorus 1 is half-time (kick 1 and 3 with an &4 pickup, clap on 3) so chorus 2's four on
        // the floor is an event; the hats drop out for the last bar of every other phrase; every phrase ends in a fill
        const int ph = (b->n - SEC_FROM[s]) % 4, phN = (b->n - SEC_FROM[s]) / 4, halfTime = 0;   // v49: "a more steady kick throughout" — four on the floor from the chorus-1 pickup to the finale
        const int hatDrop = (s == CHORUS1 || s == CHORUS2 || s == OUTRO) && ph == 3 && (phN % 2 == 1);
        const int fillBar = (s == CHORUS1 || s == CHORUS2 || s == BRIDGE || s == OUTRO || s == VERSE2) && ph == 3 && nb > 3, lift27 = 0;
        const double eag = 1 + 0.09 * ph / 3.0;   // v73: velocity builds across each four-bar phrase — fast and furious
        if (dz[0] && gKick > 0) for (int j = 0; j < nb; j++) { if ((s == VERSE1 || s == VERSE2) && j % 2) continue;   // v49: only the verses sit on her 1 and 3   // v22: her 1 and 3 in the verses; v27: and in the finale (half-time, big)
            // (v49: bars 73–76 keep the floor too — steady; the orchestra is what thins there)
            double g = 0.95 * dz[0] * (j % 2 ? 0.9 : 1) * gKick * hill * kickLift;
            // v46: the hit's own life — downbeats hardest and longest, 3 a touch under, 2 and 4 softer and shorter; half-time
            // sections ring longer, the floor is tighter, the finale biggest; ±8 % velocity, ±15 % attack and decay per hit
            const double vel = (j == 0 ? 1.0 : j == 2 ? 0.92 : 0.82) * (1 + 0.08 * rnd());
            const double decS = s == OUTRO ? 1.25 : s == CHORUS2 ? 0.95 : s == BRIDGE ? 1.0 : 1.1;
            const double dec = decS * (j % 2 ? 0.85 : 1) * (1 + 0.15 * rnd()), atk = (j == 0 ? 1.1 : 0.9) * (1 + 0.15 * rnd());
            // (v49: no double time — "when the kick starts doubling up i don't like it")
            dance_kick_v(hum_t2(bt[j], 2), g, vel, atk, dec); ev(bt[j], "kick", 0.1, g * vel, -1); }   // v46: ±2 ms — the kick stays on her hand
        if (dz[1] && gKick > 0 && !(s == BRIDGE && b->n < 77)) for (int j = 0; j < nb; j++) { if (halfTime ? j != 2 : !(j % 2)) continue; clap(hum_t2(bt[j], 4), hum_g2(0.75 * dz[1] * swell * gDrop * hill * gKick * eag, 0.1)); clap(hum_t2(bt[j] + 0.012, 4), hum_g2(0.4 * dz[1] * swell * gDrop * hill * gKick, 0.15)); ev(bt[j], "clap", 0.1, dz[1], -1);   // v38: a second clap 12 ms late — wider; v46: humanized
            snare_hit(hum_t2(bt[j], 3), hum_g2((halfTime ? 0.5 : 0.7) * dz[1] * gDrop * hill * kickLift * eag, 0.12)); ev(bt[j], "snare", 0.1, dz[1], -1); }   // v30: a snare under every clap
        // v30: TOM FILLS — hi → mid → low → floor in 16ths: on the last beat of bridge/finale phrases, and on beat 3 of the bar
        // before each lift (beat 4 is the dropout); the taiko sample steps back to support them
        { static const double TOMS[4] = { 220, 165, 120, 85 };
          const int tomFill = (fillBar && (s == BRIDGE || s == OUTRO) && !release && !lift27) ? 3 : (pickup_bar(b->n) && nb > 3) ? nb - 3 : (b->n == 72 || b->n == 80) && nb > 3 ? 2 : -1;   // v51: the toms lead in on the beat before the pickup
          if (tomFill >= 0 && dz[0] && !KICK_ONLY) for (int q = 0; q < 4; q++) { double t = bt[tomFill] + (bt[tomFill + 1] - bt[tomFill]) * q / 4;
              { static const double PT[5] = { 1.0, 1.1225, 1.1892, 1.3348, 0.8909 }; double pr = PT[(b->n + q) % 5];   // v61: pitched around — a 2nd, b3rd, 4th up or a 2nd down, in key
                tom(t, TOMS[q] * pr, (0.55 + 0.15 * q) * fmax(0.6, dz[0]) * kickLift, -1 + q * 0.66); ev(t, "tom", 0.1, 0.6, -1); }
              if (pickup_bar(b->n)) { snare_hit(t, 0.25 + 0.15 * q); snare_hit(t + (bt[tomFill + 1] - bt[tomFill]) / 8, 0.2 + 0.12 * q); } } }   // v33: a snare roll under the pickup into each chorus
        // v35: from the pickup beat the chorus kit is already playing — kick on every beat, clap on the next beat, open hats
        if (pickup_bar(b->n)) { const double dzc[4] = { .7, 1, .9, 1.1 }; for (int j = pickup_j(b); j < nb; j++) {   // v56: softer under "Oh, won't you"
            dance_kick(bt[j], 0.95 * dzc[0] * 1.26); ev(bt[j], "kick", 0.1, 1, -1);
            if ((j == 2 || j == 4) && !KICK_ONLY) { clap(bt[j], 0.75 * dzc[1]); snare_hit(bt[j], 0.6); ev(bt[j], "clap", 0.1, 1, -1); }
            if (!KICK_ONLY) { open_hat(MID(j) + BIN_OFF, 0.36 * dzc[2]); ev(MID(j), "hat", 0.1, 1, -1); } } }
        // v61: REVERSE KICKS into every 4-bar phrase downbeat from the floor on; REVERSE SNARES into the fill-bar claps; WUB KICKS
        // (the kick's tail wobbling through the filter) on chorus 2's and the finale's big downbeats; REVERSE TOMS into the climb
        if ((s == CHORUS2 || s == BRIDGE || s == OUTRO) && ph == 3 && !release) { const ChartBar *nx = k + 1 < CHART_NBARS ? &CHART_BARS[k + 1] : NULL; if (nx) rev_kick(nx->t, 0.7 * kickLift, 0.9 * beatDur); }
        if (fillBar && (s == CHORUS1 || s == CHORUS2) && nb > 3 && !KICK_ONLY) rev_snare(bt[3], 0.45, 0.75 * beatDur);
        if ((b->n == 52 || b->n == 60 || b->n == 64 || b->n == 81) && dz[0]) { wub(bt[0], bt[0] + 0.65, ROOT[b->chord] - 12, 0.65, 3, 0.55); ev(bt[0], "wub-kick", 0.65, 0.55, -1); }
        if (s == BRIDGE && b->n >= 77 && ph % 2 == 0 && !KICK_ONLY) { const ChartBar *nx = k + 1 < CHART_NBARS ? &CHART_BARS[k + 1] : NULL; if (nx) rev_tom(nx->t, 120, 0.6, 1.2 * beatDur); }
        // v27: one fill shape per section, never on a stutter bar (51): chorus 1 a clap roll in 16ths; chorus 2 kick + clap in 8ths across 3–4; bridge/finale leave it to the timpani
        if (fillBar && dz[1] && b->n != 51 && s == CHORUS1) for (int q = 0; q < 4; q++) { double t = bt[3] + (bt[4] - bt[3]) * q / 4, g = (0.3 + 0.17 * q) * dz[1] * gDrop * hill; clap(t, g); ev(t, "clap", 0.05, g, -1); }
        if (fillBar && dz[1] && b->n != 51 && s == CHORUS2) for (int q = 0; q < 4; q++) { double t = bt[2] + (bt[4] - bt[2]) * q / 4, g = (0.4 + 0.15 * q) * dz[1] * gDrop * hill; clap(t, g); ev(t, "clap", 0.05, g, -1);
            if (q % 2 == 0 && dz[0]) { dance_kick(t, 0.55 * dz[0] * gKick * kickLift); ev(t, "kick", 0.1, 0.55, -1); } }
        if (dz[2] && gHat > 0 && !hatDrop) for (int j = 0; j < nb; j++) {
            if (j % 2 == 0) { open_hat(hum_t2(MID(j) + BIN_OFF, 4), hum_g2(0.42 * dz[2] * swell * gHat * gDrop * hill * (b->n >= 52 ? 0.8 : 1) * (s == BRIDGE && b->n < 77 ? 0.5 : 1) * eag, 0.12)); ev(MID(j), "hat", 0.1, dz[2], -1); }   // v46: humanized   // v27: −2 dB from the floor; half in 73–76
            else { trap_hat(MID(j) + BIN_OFF, 0.24 * dz[2] * swell * gHat * gDrop, 0.3); ev(MID(j), "hat", 0.02, dz[2] * 0.5, -1); } }
        // v19: rolling hats from verse 2 on — ghost 16ths on every "e" and "a", and a 32nd roll into every other downbeat
        if (dz[2] && gHat > 0 && !hatDrop && s != OUTRO && s != CHORUS1) for (int j = 0; j < nb; j++) {   // v27: no ghost 16ths in the half-time sections
            const double bd = bt[j + 1] - bt[j];
            for (int q = 1; q < 4; q += 2) trap_hat(bt[j] + bd * q / 4 + BIN_OFF, (q == 3 ? 0.16 : 0.11) * dz[2] * swell * gDrop * hill, q == 3 ? 0.35 : -0.35);
            if (j == nb - 1 && ph == 3) for (int q = 0; q < 8; q++) trap_hat(bt[j] + bd * q / 8 + BIN_OFF, (0.07 + 0.02 * q) * dz[2] * swell * gDrop, -0.5 + q / 7.0); }
        // house bass: her pickups (&2/&4) in the verses, every offbeat in the choruses
        if (dz[3] && gBass > 0) for (int j = 0; j < nb; j++) { if (dz[3] < 0.95 && j % 2 == 0) continue;
            int r = ROOT[b->chord] - 12 + (j == nb - 1 && b->n % 2 ? 7 : 0);
            eight08(MID(j) + BIN_OFF, r, beatDur * 0.38, 0.7 * dz[3] * gBass * gDrop * hill); ev(MID(j), "bass", beatDur * 0.38, dz[3], r); }   // v8: a sine bass
        // her arpeggios
        if (ARP[s].div) { const Arp *A = &ARP[s]; int c = ((b->n - SEC_FROM[s]) * nb * A->div) % A->len;
            for (int j = 0; j < nb; j++) for (int q = 0; q < A->div; q++) {
                double t0 = bt[j] + (bt[j + 1] - bt[j]) * q / A->div + (q ? BIN_OFF : 0), t1 = bt[j] + (bt[j + 1] - bt[j]) * (q + 1) / A->div;
                int h = A->step[c % A->len]; c++;
                if (h < 0) continue;
                double az = s == BRIDGE ? SEAT[(b->n - 73 + q) % 5] : SEAT[(j * A->div + q) % 5];
                arp_step(h, t0, t1, A->g * swell * gDrop, az, A->hold); ev(t0, "arp", (t1 - t0) * A->hold, A->g, h); } }
        // risers: into each chorus + out of the break; a snare roll into the bridge's last bars
        // v8: no risers
        // v6.4: airplanes — an 8-bar jet into chorus 1, chorus 2 and the bridge (4 bars out of the break)
        if (0) air(b->t, CHART_BARS[k + (b->n == 73 ? 8 : 16) - 1].t + CHART_BARS[k + (b->n == 73 ? 8 : 16) - 1].dur, 0.10);
        // (v6.6: the snare roll into the bridge is gone — the jet is the hill)
        if (o->kick && !dz[0]) { kick(bt[0], 0.95 * o->kick); ev(bt[0], "kick", 0.1, o->kick, -1);
                       if (nb > 2) { kick(MID(1), 0.6 * o->kick); ev(MID(1), "kick", 0.1, o->kick * 0.6, -1); } }
        // v8: no snare — the 808 clap is the backbeat
        if (o->clap && nb > 2 && !dz[1] && !KICK_ONLY) { clap(bt[2], 0.5 * o->clap * swell); ev(bt[2], "clap", 0.1, o->clap, -1); }   // v48b: this fallback was the snare under the kick
        if (o->rim && nb > 3 && gHat > 0 && !KICK_ONLY) { rim(MID(1) + BIN_OFF, 0.16 * o->rim * gHat); rim(MID(3) + BIN_OFF, 0.2 * o->rim * gHat); ev(MID(1), "rim", 0.05, o->rim, -1); ev(MID(3), "rim", 0.05, o->rim, -1); }   // v8: 808 rim on her &2 / &4
        if (dz[0] && gHat > 0 && nb > 3 && !KICK_ONLY) { bubble(MID(1) + BIN_OFF, 0.22 * gHat * gDrop, b->n % 2 ? -0.6 : 0.6); ev(MID(1), "bubble", 0.05, gHat, -1);
            if (b->n % 2 == 0) { bubble(MID(3) + BIN_OFF, 0.16 * gHat * gDrop, b->n % 4 ? 0.4 : -0.4); ev(MID(3), "bubble", 0.05, gHat, -1); } }   // v11: bubble pops
        if (o->hat) for (int j = 0; j < nb; j++) { soft_hat(bt[j], 0.15 * o->hat, -0.35); soft_hat(MID(j), 0.09 * o->hat, 0.35); }
        {   // hand percussion on her grid: shaker 16ths, tambourine backbeat, congas on her &2 / &4
            const double *pc = KICK_ONLY ? DZ0 : PERC[s];   // v48
            // v6.5: the shuffle starts on 8ths and thickens into 16ths as the floor nears
            // (odd 16ths appear with a probability rising bar 14 → 28), then loses one 16th in
            // twelve at random so it never runs like a machine
            if (pc[0] && gShk > 0) for (int j = 0; j < nb; j++) for (int q = 0; q < 4; q++) {
                double t = bt[j] + (bt[j + 1] - bt[j]) * q / 4;
                double fill = evo(b->n, 20, FLOOR_BAR);
                if (q % 2 && (rnd() + 1) / 2 > fill) continue;                                  // the odd 16ths fill in (bars 20 → 28)
                if (b->n >= FLOOR_BAR && (rnd() + 1) / 2 < 0.08) continue;                      // a dropped grain now and then
                int c16 = (b->n - 1) * 16 + j * 4 + q;                                          // accents on a 3-cycle over the 16ths, floor only
                double acc = b->n >= FLOOR_BAR ? (c16 % 3 == 0 ? 1 : (q == 2 ? 0.7 : 0.45)) : (q == 0 ? 0.9 : 0.6);   // plain 8ths before the floor
                shaker(hum_t2(t, 10), hum_g2(0.35 * pc[0] * acc * swell * gShk, 0.22), 0.55); ev(t, "shaker", 0.03, pc[0] * acc, -1); }
            if (0) {                                                                               // v8: no woodblock
                static const int TRES[3] = { 0, 3, 6 };
                for (int q = 0; q < 3; q++) { double t = bt[0] + (bt[4] - bt[0]) * TRES[q] / 8.0 + (TRES[q] % 2 ? BIN_OFF : 0);
                    block(hum_t2(t, 7), hum_g2(0.22 * dz[0] * gHat * (q == 0 ? 0.8 : 1), 0.25), q == 1 ? -0.5 : 0.5); ev(t, "block", 0.03, dz[0], -1); } }
            if (pc[1] && nb > 2) { tamb(hum_t2(bt[2], 8), hum_g2(0.30 * pc[1] * swell, 0.2), -0.4); ev(bt[2], "tamb", 0.1, pc[1], -1);
                                   if (nb > 3) tamb(hum_t2(MID(3) + BIN_OFF, 8), hum_g2(0.14 * pc[1], 0.2), -0.4); }
            if (pc[2] && gCon > 0) { int lo = ROOT[b->chord] + 12, hi = lo + 7; double cg = pc[2] * gCon;   // tuned to her root / fifth
                if (nb > 1) { conga(hum_t2(MID(1) + BIN_OFF, 9), hi, hum_g2(0.30 * cg, 0.22), 0.35, 0); ev(MID(1), "conga", 0.1, cg, hi); }
                if (nb > 3) { conga(hum_t2(bt[3], 9), lo, hum_g2(0.34 * cg, 0.22), 0.25, 0); conga(hum_t2(MID(3) + BIN_OFF, 9), hi, hum_g2(0.26 * cg, 0.22), 0.35, 1); ev(bt[3], "conga", 0.1, cg, lo); }
                if (b->n % 2 == 0 && nb > 2) { conga(hum_t2(bt[1] + (bt[2] - bt[1]) * 2 / 3.0, 9), hi, hum_g2(0.2 * cg, 0.25), 0.3, 1); }   // v6.4: a triplet slap every other bar
                if (fillBar && s == VERSE2) for (int q = 0; q < 3; q++) { double t = bt[3] + (bt[4] - bt[3]) * q / 3; conga(t, q == 2 ? lo : hi, 0.3 * cg * (0.7 + 0.15 * q), 0.3, q % 2); ev(t, "conga", 0.08, cg, q == 2 ? lo : hi); } }   // v26: verse 2 phrase ends on a conga fill
        }
        // v49: BELLS OFF HER GUITAR — on each of her strum points (1, &2, 3, &4) in the last two bars of chorus 2 and through the
        // break, a fem bell an 8th behind on the chord's top tones, ringing out: her playing trails into bells
        if ((b->n >= 66 && b->n <= 72) && nb > 3) { int tops[4]; int nt = 0; for (int m = 83; m >= 72 && nt < 4; m--) { int pc = m % 12; if (pc == pcs[0] || pc == pcs[1] || pc == pcs[2]) tops[nt++] = m; }
            const double pts[4] = { bt[0], MID(1), bt[2], MID(3) }; const double g = 0.13 * (b->n <= 67 ? 0.8 : 1.0) * evo(b->n, 66, 69);
            for (int q = 0; q < 4; q++) { double t = pts[q] + beatDur / 2; fem_bell(hum_t(t), tops[q % nt], hum_g(g)); ev(t, "bell", 0.6, g, tops[q % nt]); } }
        // v24: the wub on her root, two octaves under the sub, one sweep per beat (8ths from the bridge)
        if (WUB[s] > 0 && !release) { wub(b->t, b->t + b->dur, ROOT[b->chord] - 24, beatDur, WUB_RATE[s], WUB[s] * hill * (s == BRIDGE && b->n < 77 ? 0.5 : 1)); ev(b->t, "wub", b->dur, WUB[s], ROOT[b->chord] - 24); }
        // v62: THE PIANO — jazz voicings from her cycle: G#m9 (G# B D# F# A#), Emaj7 (E G# B D#) on her Emaj7/G# bar, Bmaj9
        // (B D# F# A# C#). Right hand above her while she sings (69+), left hand below (≤ 55). Verses: broken 8ths. Choruses:
        // comped on 1, the & of 2 and 4 (Charleston) alternating with the & of 1 and 3. Chorus 2: a lament-bass left hand.
        // The break: her kiss figure answered on piano. Bridge climb: repeated chords. Finale: both hands, big. Release: high and few.
        // v70: the grand OPENS the record — a slow broken G#m under the radio, one soft note a beat, into her first word; then quiet
        // single notes under her first line until the part proper begins at 16
        if (PIANO_ON && PIANO_LOADED && ((b->n >= 9 && b->n <= 10) || (s == VERSE1 && b->n < 16))) {
            static const int OPEN[8] = { 44, 56, 59, 63, 68, 63, 59, 56 }; const double pv0 = b->n <= 10 ? 0.38 : 0.26;
            if (b->n <= 10) { for (int j = 0; j < nb; j++) piano(hum_t2(bt[j], 0.01), OPEN[((b->n - 9) * 4 + j) % 8], beatDur * 2.2, pv0 * (j == 0 ? 1 : 0.8)); }
            else { piano(hum_t2(bt[0], 0.01), 44, b->dur * 1.5, pv0); if (nb > 2) piano(hum_t2(bt[2], 0.01), OPEN[2 + b->n % 3], b->dur, pv0 * 0.7); } }
        if (PIANO_ON && PIANO_LOADED && !(s == INTRO) && !(s == VERSE1 && b->n < 16)) {
            static const int RH[3][5] = { { 71, 75, 78, 82, 87 }, { 71, 75, 76, 80, 83 }, { 70, 73, 75, 78, 82 } };   // G#m9 / Emaj7 / Bmaj9, above her
            static const int LH[3] = { 44, 44, 47 };
            const int *rh = RH[b->chord]; const double jit = 0.006, pv = s == VERSE1 ? 0.35 + 0.25 * evo(b->n, 16, 27) : s == VERSE2 ? 0.5 : s == CHORUS1 ? 0.6 : s == CHORUS2 ? 0.75 : s == BREAK ? 0.6 : s == BRIDGE ? (b->n < 77 ? 0.45 : 0.7) : release ? 0.4 : 0.85;
            if (s == VERSE1 || s == VERSE2 || (s == BRIDGE && b->n < 77)) {   // broken 8ths, LH root on 1, RH climbing and falling
                piano(hum_t2(bt[0], jit), LH[b->chord] + 12, beatDur * 2, pv * 0.8);
                static const int ORD[8] = { 0, 1, 2, 3, 4, 3, 2, 1 }; for (int j = 0; j < nb; j++) for (int q = 0; q < 2; q++) { int k = (j * 2 + q) % 8; piano(hum_t2(bt[j] + (bt[j + 1] - bt[j]) * q / 2, jit), rh[ORD[k]], beatDur * 0.9, pv * (q ? 0.7 : 0.85)); }
            } else if (s == CHORUS1 || s == CHORUS2 || (s == BRIDGE && b->n >= 77) || (s == OUTRO && !release)) {
                const int alt = (b->n % 2); double hits[3]; int nh = 0;
                if (!alt) { hits[nh++] = bt[0]; if (nb > 1) hits[nh++] = MID(1); if (nb > 3) hits[nh++] = MID(3); } else { hits[nh++] = MID(0); if (nb > 2) hits[nh++] = bt[2]; if (nb > 2) hits[nh++] = MID(2); }
                for (int h = 0; h < nh; h++) { double t = hum_t2(hits[h], jit); for (int k = 1; k < 5; k++) piano(t + k * 0.004, rh[k], beatDur * (h == 0 ? 1.4 : 0.7), pv * (h == 0 ? 1 : 0.8) * (0.9 + 0.1 * rnd())); }
                if (s == CHORUS2 || s == OUTRO) { static const int LAM_G[4] = { 44, 42, 40, 39 }, LAM_B[3] = { 39, 37, 35 }; int rp = ph % 4;   // the lament bass, octaves
                    for (int hlf = 0; hlf < 2; hlf++) { int m = b->chord == 2 ? LAM_B[(rp * 2 + hlf) % 3] : LAM_G[(rp * 2 + hlf) % 4]; piano(hum_t2(b->t + hlf * b->dur / 2, jit), m, b->dur / 2, pv * 0.9); piano(hum_t2(b->t + hlf * b->dur / 2, jit), m - 12, b->dur / 2, pv * 0.7); } }
                else piano(hum_t2(bt[0], jit), LH[b->chord], b->dur * 0.95, pv * 0.85);
            } else if (s == BREAK) {   // the kiss figure, answered on piano, over the break's first two bars; then comping
                if (b->n == 68) { static const int KF[7] = { 66, 64, 63, 66, 64, 61, 59 }; static const double KO[7] = { 0.02, 0.49, 1.08, 1.74, 2.36, 2.92, 3.13 }, KD[7] = { .48, .59, .37, .63, .56, .2, .3 };
                    for (int k = 0; k < 7; k++) { piano(hum_t2(b->t + KO[k] / 1.915 * b->dur, jit), KF[k] + 12, KD[k] / 1.915 * b->dur * 1.3, 0.75); piano(hum_t2(b->t + KO[k] / 1.915 * b->dur, jit), KF[k], KD[k] / 1.915 * b->dur * 1.3, 0.45); } }
                else if (b->n >= 70) { for (int k = 0; k < 4; k++) piano(hum_t2(bt[0], jit) + k * 0.005, rh[k], b->dur * 0.9, pv * 0.8); piano(hum_t2(bt[0], jit), LH[b->chord], b->dur, pv * 0.8); }
            } else if (release) { if (nb > 2) { piano(hum_t2(bt[0], jit), rh[4], b->dur * 1.5, 0.45); piano(hum_t2(bt[2], jit), rh[3], b->dur, 0.35); } }
        }
        // v64: the horse — a stride on every beat from the break through the bridge, swelling in over 68–70, leaving across 79–80
        if ((s == BREAK || s == BRIDGE) && GALLOP.L) { double g = 0.55 * evo(b->n, 68, 71) * (b->n >= 79 ? 1 - 0.5 * (b->n - 78) : 1);
            for (int j = 0; j < nb; j++) stride(bt[j] - 0.03, (b->n * 4 + j), g * (j == 0 ? 1 : 0.85)); ev(b->t, "gallop", b->dur, g, -1); }
        // v68b: no neighs ("lose the neigh but keep the gallops")
        // v68: her vowel choir — "ooo" swelling in over the opening bars and under her first line, "aaa" through chorus 2 and the
        // climb, both in the finale, "ooo" quiet in the release; three voices on the chord tones in her range, seated around her
        { int vt[3], nv = 0; for (int m = 56; m <= 78 && nv < 3; m++) { int pc = m % 12; if ((pc == pcs[0] || pc == pcs[1] || pc == pcs[2]) && VOWEL[0][m].L) vt[nv++] = m; }
          double gv = 0; int which = 0;
          // v69: not at the start ("they don't blend with the first vocal") — more elsewhere: chorus 1's back half, verse 2, the break
          if (s == CHORUS1 && b->n >= 36) { gv = 0.36 * evo(b->n, 36, 40); which = 1; } else if (s == VERSE2) { gv = 0.26 * evo(b->n, 46, 50); which = 0; } else if (s == BREAK) { gv = 0.4; which = 0; }
          else if (s == CHORUS2) { gv = 0.45 * swellIn; which = 1; } else if (s == BRIDGE && b->n >= 77) { gv = 0.45; which = 1; }
          else if (s == OUTRO && !release) { gv = 0.55; which = (b->n % 2); } else if (release) { gv = 0.3; which = 0; }
          if (gv > 0) for (int k = 0; k < nv; k++) vowel(hum_t2(b->t, 12), vt[k], b->dur * 1.02, gv * (k == 1 ? 0.85 : 1), which, k); }
        // 808 on the kick points, gliding into her root
        if (o->b808 && !dz[3]) { int r = ROOT[b->chord] - 12;
            eight08(bt[0], r, beatDur * 1.4, 0.8 * o->b808); ev(bt[0], "808", beatDur * 1.4, o->b808, r);
            if (nb > 2) { eight08(MID(1), r, beatDur * 1.3, 0.6 * o->b808); ev(MID(1), "808", beatDur * 1.3, o->b808 * 0.6, r); } }
        // trap hats: 16ths, accents on the beat, a roll into every other bar line
        // v27: the trap hats keep the form — 8ths in chorus 1 and the finale (half-time), 16ths in chorus 2 and the bridge and
        // under the pre-chorus line (bars 22–27); they wait for the kick in verse 1 and honour the hat drop
        const double trapG = s == VERSE1 ? 0.15 + 0.85 * evo(b->n, 12, 18) : 1;   // v31c: hats barely there under her first line, in by 18
        if (o->trap && trapG > 0 && !hatDrop && !release && !KICK_ONLY) {
            int roll = 0, trip = 0;
            for (int j = 0; j < nb; j++) {
                int last = j == nb - 1 && roll, div = last ? (trip ? 6 : 8) : ((s == CHORUS2 || s == BRIDGE || (s == VERSE1 && b->n >= 22)) ? 4 : 2);
                for (int q = 0; q < div; q++) {
                    double t = bt[j] + (bt[j + 1] - bt[j]) * q / div;
                    double acc = q == 0 ? 1 : (q % 2 ? 0.55 : 0.75);
                    double g = 0.5 * o->trap * acc * (last ? 0.6 + 0.4 * q / div : 1) * swell * trapG * eag;
                    trap_hat(hum_t2(t, 3), hum_g2(g, 0.15), q % 2 ? 0.45 : -0.25); ev(t, "hat", 0.02, g, -1);   // v46: humanized
                }
            }
        }
        // sine choir
        const double dur = b->dur * 0.97;
        if (o->sub && gSub > 0) { sine(sL, sR, b->t, dur, ROOT[b->chord] - 12, 0.26 * o->sub * gSub, 0, 0.35, 0.9, 0);
                      sine(sL, sR, b->t, dur, ROOT[b->chord], 0.16 * o->sub * gSub, 0, 0.35, 0.9, 1); ev(b->t, "sub", dur, o->sub * gSub, ROOT[b->chord] - 12); }
        lead(tenor, 3, pcs, 67, 79);
        if (o->tenor && gTen > 0) for (int v = 0; v < 3; v++) { sine(padM[v], NULL, b->t, dur, tenor[v], 0.4 * o->tenor * swell * gTen * gDrop * hill, 0, 0.35, 0.9, 7); ev(b->t, "pad", dur, o->tenor * gTen, tenor[v]); }   // v44: up (was .26)
        // v44: "the main pads as sine waves" — the whole chord, 51–83, as slow detuned sines on the touring pad seats; the sampled strings step back
        if (o->tenor && gTen > 0) { int vp[6], nv = 0; for (int m = 51; m <= 83 && nv < 6; m++) { int pc = m % 12; if (pc == pcs[0] || pc == pcs[1] || pc == pcs[2]) vp[nv++] = m; }
            const int struck = (s == CHORUS1 || s == CHORUS2 || s == OUTRO || (s == BRIDGE && b->n >= 77));   // v70: in the big sections the pad is STRUCK on her strum motif (1, &2, 3, &4) — snappy, with her hand — over a low sustain
            for (int k = 0; k < nv; k++) { sine(padM[k % 3], NULL, b->t, dur, vp[k], 0.11 * o->tenor * swell * gTen * gDrop * hill * (k >= 3 ? 0.7 : 1) * (struck ? 0.45 : 1), 0, 0.5, 1.2, 9); ev(b->t, "pad", dur, o->tenor * gTen * 0.5, vp[k]); }
            if (struck && nb > 3) { const double pts[4] = { bt[0], MID(1), bt[2], MID(3) }; const double pg[4] = { 1.0, 0.7, 0.85, 0.7 };
                for (int q = 0; q < 4; q++) for (int k = 0; k < nv && k < 4; k++) { sine(padM[k % 3], NULL, pts[q] + BIN_OFF, beatDur * 0.42, vp[k] + (k >= 2 ? 12 : 0), 0.13 * o->tenor * gTen * gDrop * hill * pg[q] * (k == 3 ? 0.7 : 1), 0, 0.02, 0.22, 9); }
                ev(bt[0], "pad-hit", b->dur, o->tenor, vp[0]); } }
        lead(high, 2, pcs, 76, 88);
        if (o->high) for (int v = 0; v < 2; v++) { sine(hiM, NULL, b->t, dur, high[v], 0.10 * o->high * swell * gDrop, 0, 0.8, 1.4, 7); ev(b->t, "high", dur, o->high, high[v]); }
        if (o->descant) {   // one long high chord tone per bar, leaning down by step
            int c = -1; for (int d = -1; d >= -2 && c < 0; d--) { int m = nearest(pcs, 3, desc + d, 83, 93); if (m <= desc) c = m; }
            if (c < 0) c = nearest(pcs, 3, desc, 83, 93);
            desc = c <= 84 ? 92 : c;
            sine(hiM, NULL, b->t, dur, c, 0.04 * o->descant, 0, 1.0, 1.8, 4); ev(b->t, "descant", dur, o->descant, c);
        }
        // vibraphone arpeggio on her 8ths (16ths in the last bridge bars)
        if (o->vib) {
            int tones[6]; for (int j = 0; j < 6; j++) tones[j] = nearest(pcs, 3, 76 + j * 3, 76, 93);   // v6.3: vibes an octave up, out of her way
            int dens = (s == BRIDGE && b->n >= 77) ? 4 : 2, c = 0;
            for (int j = 0; j < nb; j++) for (int q = 0; q < dens; q++)
                vib(hum_t(bt[j] + (bt[j + 1] - bt[j]) * q / dens), tones[c++ % 6], hum_g(0.10 * o->vib * swell), beatDur / dens * 1.5);
        }
        // a fem bell on every other downbeat where the trap plays
        if (o->bell && o->trap && b->n % 2 == 0) fem_bell(bt[0], nearest(pcs, 3, 72, 68, 76), 0.18 * o->bell);
        ev(b->t, "gtr", dur, o->her, -1);
    }
    // v25: THE BUTTON — the end of bar 84 is the last downbeat: implosion, kick, a long 808 on her root; the
    // orchestra holds its chord (bin/orchestra.mjs) while her guitar rings down and she reaches for the phone
    { const ChartBar *b84 = bar_n(84); double tE = b84->t;   // v27: on her LAST STRUM (bar 84's downbeat), not the computed bar end
      explosion(tE, 1.0); dance_kick(tE, 1.3); eight08(tE, ROOT[b84->chord] - 12, 1.5, 0.63); ev(tE, "impact", 1.5, 1, -1); }
    // v27: the first thing that is not the phone is weight, not a hat — one sub note under her first bar
    // v31c: no sub under her first word — "just hats, guitar and vocal"
    // v25: THROWS — her "sailor?" at the end of each chorus thrown into a dotted-8th echo that spins round the head
    { static const double THROW_T[2] = { 63.37 - SHIFT27, 109.27 - SHIFT27 }, THROW_G[2] = { 63.332 - SHIFT27, 109.191 - SHIFT27 };   // v53: on the fitted clock   // her onsets, and the 8th-grid points they sit on (v27: the echoes ride the grid)
      for (int q = 0; q < 2; q++) { const ChartBar *tb = bar_at(THROW_T[q]); if (!tb) continue; double dot = tb->dur / 4 * 1.5; long sa = at(THROW_T[q]); int n = (int)(0.55 * SR);
          for (int h = 1; h <= 3; h++) { long a = at(THROW_G[q] + h * dot); double g = 0.34 * pow(0.62, h - 1);   // v26: three, quieter
              for (int i = 0; i < n; i++) { double w = fmin(1, i / (0.006 * SR)) * fmin(1, (n - i) / (0.08 * SR)); add(throwM, a + i, sample(vox.L, vox.n, sa + i) * w * g); }
              ev(THROW_T[q] + h * dot, "throw", 0.55, g, -1); } } }
    // v43: THE RADIO — from the record's first sample to her first word: static whose band wanders and locks onto her,
    // a heterodyne whistle sliding down twice, crackle, and chopped & screwed fragments of her first words surfacing
    // through the static's band on the 8th grid; the static snaps off 40 ms before she sings
    { const double r0 = startSecOut, r1 = (double)N / SR - 0.1, len = fmax(0.5, firstWordT - r0); long a0 = at(r0), a1 = at(r1);   // v73: len was 0 once the record opened on her word → 0/0 → NaN   // v60: the air stays — a floor through the whole record
      double b1 = 0, b2 = 0, c1 = 0, c2 = 0, wph = 0; unsigned seed = 7;
      double genv = 0; double resF[3] = { mtof(56), mtof(63), mtof(68) }, resX1[3] = { 0 }, resX2[3] = { 0 }, resY1[3] = { 0 }, resY2[3] = { 0 }, resOut[3] = { 0 };   // v60c: the air's resonators
      for (long i = a0; i < a1 && i < N; i++) { double u = (double)(i - a0) / (a1 - a0), t = (double)i / SR;
          double us = fmin(1, (t - r0) / len), tune = 200 * pow(7.5, 0.5 + 0.5 * sin(2 * PI * 0.35 * (t - r0)) * (1 - us) + 0.45 * us);   // v66: the tuning settles toward 750 Hz — an octave lower
          double k = 1 - exp(-2 * PI * tune / SR), nz1 = rnd(), nz2 = rnd();
          b1 += (nz1 - b1) * k; b2 += (b1 - b2) * k; c1 += (nz2 - c1) * k; c2 += (c1 - c2) * k;
          seed = seed * 1103515245u + 12345u; double crackle = ((seed >> 16) & 0x3fff) < 6 && t < firstWordT ? (rnd() * 1.6) : 0;   // sparse pops, before her only
          double drop = 0.75 + 0.25 * sin(2 * PI * 1.7 * (t - r0)) * sin(2 * PI * 0.23 * (t - r0) + 1);        // the signal fades in and out
          const double air = 0.015, floor_ = 0.006, t20 = bar_n(20)->t, t24 = bar_n(24)->t;   // v72: a trace                        // v60: "keep a bit of air" — down to a floor by bar 24, never gone
          // v70: the radio is back before she sings; the air under her stays a whisper
          double after = t < firstWordT ? 0.3 * fmin(1, (t - r0) / fmax(0.5, len)) : t < firstWordT + 1.6 ? 0.3 - (0.3 - air) * (t - firstWordT) / 1.6 : t < t20 ? air : t < t24 ? air + (floor_ - air) * (t - t20) / (t24 - t20) : floor_;   // v68: from silence, a whisper before she sings
          double env = fmin(1, (t - r0) / len * 1.6) * (t < firstWordT ? drop : 1) * after;       // steady once she is in — air, not drops
          (void)u; double w = fmax(0, (t - r0) < len * 0.5 ? 1 - (t - r0) / (len * 0.5) : 1 - ((t - r0) - len * 0.5) / (len * 0.5));
          wph += 2 * PI * (700 + 2600 * w * w) / SR; double whistle = sin(wph) * 0.05 * fmax(0, 1 - (t - r0) / len);   // the whistle is gone by her entry
          // v60c: PITCHED AIR — once she is in, the bed runs through three resonators tuned to the bar's chord tones (an octave
          // above her root), gliding as the chord changes: the hiss hums the song's harmony, the dry static fades under it
          { const ChartBar *cb = bar_at(t); int ch = cb ? cb->chord : 0; const int *pc3 = CHORD_PCS[ch]; double mix = t < firstWordT ? 0 : fmin(1, (t - firstWordT) / 4.0);
            const int csec = cb ? section_of(cb->n) : 0, up = csec == CHORUS1 || csec == CHORUS2 || csec == OUTRO || (csec == BRIDGE && cb->n >= 77);   // v73: the air sits an octave higher in the choruses
            for (int k = 0; k < 3; k++) { int m = up ? 56 : 44; while (m % 12 != pc3[k]) m++; m += k == 0 ? 0 : 12 * (k == 2); double fT = mtof(m);
                genv = fmax(fabs(sample(gtr.L, gtr.n, i)), genv * 0.9994); fT *= 1 + 0.07 * fmin(1, genv * 5);   // v68: the air's pitch flexes up with each of her strums
                resF[k] += (fT - resF[k]) * 0.00004;   // a slow glide between chords
                double w0 = 2 * PI * resF[k] / SR, alpha = sin(w0) / (2 * 28.0), b0 = alpha / (1 + alpha), a1 = -2 * cos(w0) / (1 + alpha), a2 = (1 - alpha) / (1 + alpha);
                double xin = (b1 - b2) * 0.9, y = b0 * xin - b0 * resX2[k] - a1 * resY1[k] - a2 * resY2[k]; resX2[k] = resX1[k]; resX1[k] = xin; resY2[k] = resY1[k]; resY1[k] = y; resOut[k] = y; }
            double tonal = (resOut[0] + resOut[1] + resOut[2]) * 3.2;
            double dryL = (b1 - b2) * 0.9, dryR = (c1 - c2) * 0.9;
            add(radL, i, (dryL * (1 - 0.7 * mix) + tonal * mix + crackle + whistle) * env * 0.22); add(radR, i, (dryR * (1 - 0.7 * mix) + tonal * mix * 0.9 + crackle + whistle) * env * 0.22); } }
      // the fragments: her first words, re-triggered on the 8th grid, each slower and lower, through the static's band
      // v45: just "I saw", tuning in — at 0.6× from bar 10, at 0.8× from bar 11, then her real one at 26.20: one gesture, not a collage
      static const double FR[2][2] = { { 26.20, 1.25 }, { 26.20, 1.25 } }, RT[2] = { 0.6, 0.8 }, LPF[2] = { 1600, 3200 }; static const int FB[2] = { 10, 11 }, FJ[2] = { 0, 0 };
      // v63: OFF (q < 0) — one opening, not previews
      for (int q = 0; q < 0; q++) { const ChartBar *fb = bar_n(FB[q]); if (!fb || fb->nb <= FJ[q]) continue; long a = at(fb->beats[FJ[q]] + (q % 2 ? fb->dur / 8 : 0)); int n = (int)(FR[q][1] * SR / RT[q]); long sa = at(FR[q][0]);
          double lp = 0, hp = 0; const double kl = 1 - exp(-2 * PI * LPF[q] / SR), kh = 1 - exp(-2 * PI * (q ? 220 : 350) / SR);
          for (int i = 0; i < n; i++) { double pos = i * RT[q]; long j = sa + (long)pos; double f = pos - floor(pos), x = sample(vox.L, vox.n, j) * (1 - f) + sample(vox.L, vox.n, j + 1) * f;
              hp += (x - hp) * kh; x -= hp; lp += (x - lp) * kl; x = lp;                                                        // the radio's band
              double w = fmin(1, i / (0.02 * SR)) * fmin(1, (n - i) / (0.25 * SR)), g = q ? 0.95 : 0.7;                           // the second nearly her level
              add(radL, a + i, x * w * g * 1.5); add(radR, a + i, x * w * g * 1.5); }
          ev(fb->beats[FJ[q]], "radio", FR[q][1] / RT[q], 1, -1); } }
    // v39: CHOPPED & SCREWED — her last word, "out" (159.46 s), re-triggered across the finale, each repeat slower and lower
    // than the last (resampled: rate 1 → 0.7), two to a bar on 1 and 3, the last one alone on bar 84's downbeat
    { const double OUT_T = 159.46 - SHIFT27, OUT_L = 1.3; static const int OB[8] = { 80, 81, 81, 82, 82, 83, 83, 84 }; static const int OJ[8] = { 2, 0, 2, 0, 2, 0, 2, 0 };   // v41: from bar 80 beat 3 — right off her own "out"
      static const double RATE[8] = { 1.0, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.64 }, OG[8] = { .9, .9, .85, .82, .8, .78, .76, .8 };
      long sa = at(OUT_T);
      for (int q = 0; q < 8; q++) { const ChartBar *ob = bar_n(OB[q]); if (!ob || ob->nb <= OJ[q]) continue; long a = at(ob->beats[OJ[q]]); int n = (int)(OUT_L * SR / RATE[q]);
          for (int i = 0; i < n; i++) { double pos = i * RATE[q]; long j = sa + (long)pos; double f = pos - floor(pos);
              double w = fmin(1, i / (0.006 * SR)) * fmin(1, (n - i) / (0.15 * SR)); add(stM, a + i, (sample(vox.L, vox.n, j) * (1 - f) + sample(vox.L, vox.n, j + 1) * f) * w * OG[q]); }
          ev(ob->beats[OJ[q]], "out", OUT_L / RATE[q], OG[q], -1); } }
    // v24: THE STUTTER — "the k-kiss hiccup should be musically recovered": the first 90 ms of her own
    // "kiss" (the chorus downbeat word) repeated on 16ths across the last two beats of the bar before
    // each chorus and the bridge, rising, a trap hat under each — the hiccup becomes the pickup
    { static const double KISS_T[3] = { 60.67, 106.60, 146.065 };   // her "kiss" onsets, and the bridge's own first word "And" (v27)
      static const int INTO[3] = { 28, 52, 73 }; const double SL = 0.09;
      for (int q = 0; q < 0; q++) { const ChartBar *pb = bar_n(INTO[q] - 1); if (!pb || pb->nb < 4) continue;   // v28: OFF — "i don't like the added glitchiness on the k-kiss"; the dropout and the whip do the work
          const double src = KISS_T[q], t0 = pb->beats[pb->nb - 2], step = (pb->beats[pb->nb] - t0) / 8; long sa = at(src);   // v27: the LAST two beats (bar 27 has five)
          for (int h = 0; h < 8; h++) { double t = t0 + h * step, g = 0.3 + 0.7 * h / 7.0; long a = at(t); int n = (int)(SL * SR);
              for (int i = 0; i < n; i++) { double w = fmin(1, i / (0.004 * SR)) * fmin(1, (n - i) / (0.012 * SR)); add(stM, a + i, sample(vox.L, vox.n, sa + i) * w * g); }
              trap_hat(t, 0.25 * g, h % 2 ? 0.4 : -0.4); ev(t, "stutter", SL, g, -1); } } }
    // her melody, shadowed a 3rd up and an octave up
    for (int k = 0; k < CHART_NNOTES; k++) {
        const ChartNote *v = &CHART_NOTES[k]; const ChartBar *b = bar_at(v->t);
        if (!b || v->dur < 0.18) continue;
        const Arr *o = &ARR[section_of(b->n)];
        ev(v->t, "vox", v->dur, 1, v->midi);
        if (o->third) sine(sL, sR, v->t, v->dur, third_above(v->midi), 0.06 * o->third, 0.3, 0.06, 0.35, 2);
        if (o->octave) sine(hiM, NULL, v->t, v->dur, v->midi + 12, 0.03 * o->octave, 0, 0.08, 0.5, 4);
        // v29: COUNTERPOINT — "sine bells and sines that mirror or chorale her lead": a sine MIRROR of her line (inverted
        // about her median D#4, snapped to the key, an octave up, one beat behind — contrary motion in canon), a three-voice
        // sine CHORALE under each note (a 3rd and a 6th below, in the key), and BELLS in canon on her longer notes
        { static const double MIRROR[NSEC] = { 0, 0, .5, .3, .8, .6, .8, .6 }, CHORALE[NSEC] = { 0, .25, .6, .4, .9, .5, .9, .7 }, CANON[NSEC] = { 0, 0, .4, .5, .7, .8, .7, .5 };
          const int sec = section_of(b->n); const double beat = b->dur / b->nb, gV1 = sec == VERSE1 ? evo(b->n, 20, 27) : 1;
          // v33: THE SISTER SINE — a sine on her exact pitch (and its octave), following every note she sings; on the melody
          // bus, which the hollow does not duck (the v29 melody sines sat on the sine bus and were ducked 8 dB while she sang)
          { static const double SIS[NSEC] = { 0, .35, .7, .5, .9, .6, .85, .8 };
            if (SIS[sec] > 0) { sine(melM, NULL, v->t, v->dur, v->midi, 0.075 * SIS[sec] * gV1, 0, 0.03, 0.3, 2); sine(melM, NULL, v->t, v->dur, v->midi + 12, 0.028 * SIS[sec] * gV1, 0, 0.05, 0.4, 4); ev(v->t, "sister", v->dur, SIS[sec], v->midi); } }
          if (MIRROR[sec] > 0) { int mm = scale_step(2 * 63 - v->midi, 0) + 12; sine(mirM, NULL, v->t + beat, v->dur, mm, 0.05 * MIRROR[sec], 0, 0.05, 0.45, 5); ev(v->t + beat, "mirror", v->dur, MIRROR[sec], mm); }
          if (CHORALE[sec] > 0) { int c3 = scale_step(v->midi, -2), c6 = scale_step(v->midi, -5);
              sine(melM, NULL, v->t, v->dur, c3, 0.04 * CHORALE[sec] * gV1, 0, 0.05, 0.4, 3); sine(melM, NULL, v->t, v->dur, c6, 0.034 * CHORALE[sec] * gV1, 0, 0.05, 0.4, 3); ev(v->t, "chorale", v->dur, CHORALE[sec], c3); }
          if (CANON[sec] > 0 && v->dur >= 0.35) { fem_bell(v->t + beat / 2, v->midi + 12, 0.14 * CANON[sec]); ev(v->t + beat / 2, "bell", 0.5, CANON[sec], v->midi + 12); } }
    }
    // fem bell answers: in every breath > 1 s her last three notes ring back
    for (int k = 1; k < CHART_NNOTES; k++) {
        const ChartNote *p = &CHART_NOTES[k - 1]; double end = p->t + p->dur, gap = CHART_NOTES[k].t - end;
        const ChartBar *b = bar_at(end);
        if (gap < 1.0 || !b) continue;
        const Arr *o = &ARR[section_of(b->n)];
        if (!o->bell) continue;
        const double gBell = evo(b->n, 14, 20);                                  // v6.2: no bells in the bedroom bars
        if (gBell <= 0) continue;
        double e8 = (b->beats[1] - b->beats[0]) / 2;
        for (int j = 0, from = k >= 3 ? k - 3 : 0; from + j < k; j++) {
            double t = end + 0.12 + j * e8;
            if (t < CHART_NOTES[k].t - 0.1) fem_bell(hum_t(t), CHART_NOTES[from + j].midi + 12, hum_g(0.16 * o->bell * gBell));
        }
    }
    // the hook on sines over the guitar break: chorus 1 bars 28–31 re-laid on 68–71
    for (int k = 0; k < CHART_NNOTES; k++) {
        const ChartNote *v = &CHART_NOTES[k]; const ChartBar *sb = bar_at(v->t);
        if (!sb || sb->n < 28 || sb->n > 31) continue;
        const ChartBar *db = bar_n(sb->n + 40);
        double t = db->t + (v->t - sb->t) / sb->dur * db->dur, d = v->dur * db->dur / sb->dur;
        sine(hookM, NULL, t, d, v->midi, 0.12, 0, 0.05, 0.6, 3);
        sine(hookM, NULL, t, d, v->midi + 12, 0.045, 0, 0.08, 0.8, 5);
        ev(t, "hook", d, 1, v->midi);
    }

    // ── space ──
    float *spL = zeros(), *spR = zeros();
    ROT_T0 = bar_n(68)->t;
    spatialize(hookM, NULL, rot_az, el_up, 1.3, spL, spR, 1.0);       // v11: the hook rotates through the break
    for (int v = 0; v < 3; v++) spatialize(padM[v], NULL, v == 0 ? tour0 : v == 1 ? tour1 : tour2, el_zero, 1.4, spL, spR, 1.0);   // v11: the pads tour
    spatialize(bellM, NULL, orbit_az, orbit_el, 1.2, spL, spR, 0.9);   // bells orbit overhead
    spatialize(vibM, NULL, sweep_az, el_zero, 1.2, spL, spR, 0.8);     // vibes sweep ear to ear
    float *hiL = zeros(), *hiR = zeros();
    spatialize(hiM, NULL, wide_az, el_up, 1.2, hiL, hiR, 1.3);         // highs float up + wide
    float *exL = zeros(), *exR = zeros();
    spatialize(exM, NULL, expl_az, expl_el, 0.8, exL, exR, 1.0);       // the explosion laps the head
    spatialize(throwM, NULL, spin_az, el_up, 1.3, exL, exR, 1.0);      // v25: the throws spin, overhead
    // v36: along each rise — a sine on the curve (and its octave), swelling in over 0.4 s; a bell on every scale step crossed upward
    for (int q = 0; q < 3; q++) { if (!fxS[q].L) continue; snprintf(pth, sizeof pth, LANE "/src/vox/fx/%s.curve", FX[q].name); FILE *cf = fopen(pth, "r"); if (!cf) continue;
        static double ct[4096], cm[4096]; int nc = 0; while (nc < 4096 && fscanf(cf, "%lf %lf", &ct[nc], &cm[nc]) == 2) nc++; fclose(cf); if (nc < 2) continue;
        double ph = 0, ph2 = 0; int lastStep = scale_step((int)lround(cm[0]), 0); long a0 = at(ct[0]), a1 = at(ct[nc - 1]); int ci = 0;
        for (long i = a0; i < a1 && i < N; i++) { double t = (double)i / SR; while (ci < nc - 2 && ct[ci + 1] <= t) ci++;
            double f = (t - ct[ci]) / fmax(1e-6, ct[ci + 1] - ct[ci]), m = cm[ci] + (cm[ci + 1] - cm[ci]) * fmin(1, fmax(0, f));
            double hz = 440 * pow(2, (m + CHART_TUNE - 69) / 12.0); ph += 2 * PI * hz / SR; ph2 += 2 * PI * hz * 2 / SR;
            double u = (t - ct[0]) / 0.4, g = fmin(1, u) * fmin(1, (ct[nc - 1] - t) / 0.5); g = g * g * (3 - 2 * g);
            add(melM, i, (sin(ph) * 0.07 + sin(ph2) * 0.025) * g);
            int st = scale_step((int)floor(m + 0.02), 0); if (st > lastStep) { fem_bell(t, st + 12, 0.16); ev(t, "bell", 0.5, 1, st + 12); lastStep = st; } }
        ev(ct[0], "rise", ct[nc - 1] - ct[0], 1, (int)lround(cm[0]));
        if (q < 2) { const ChartBar *gb = bar_n(q == 0 ? 44 : 68); gong(gb->t, (int)lround(cm[nc - 1]) + 12, q == 0 ? 0.6 : 0.5); } }   // v42: the gong on the climb's top note starts the next part
    // v60: THE LISTENER — claps, snare and toms from a seat that sways slowly front-right ↔ front-left (the kick stays centre);
    // her guitar from where it sits in the room, a little left of her and near
    float *pcL = zeros(), *pcR = zeros(); spatialize(pcM, NULL, sway_pc, el_near, 1.15, pcL, pcR, 1.0);
    // v65: the hats on the ring too (they were plain stereo, outside the HRTF) — their ticks are what makes a turn audible
    float *htL = zeros(), *htR = zeros(); { float *hm = zeros(); for (long i = 0; i < N; i++) hm[i] = 0.5f * (tL[i] + tR[i]); spatialize(hm, NULL, hat_az, el_up, 1.1, htL, htR, 1.0); free(hm); for (long i = 0; i < N; i++) { tL[i] = 0; tR[i] = 0; } }
    float *pnL = zeros(), *pnR = zeros(); spatialize(pianoM, NULL, seat_piano, el_low, 1.2, pnL, pnR, 1.0);   // v62: the grand, just right of her
    float *hsL = zeros(), *hsR = zeros(); spatialize(horseM, NULL, horse_az, el_low, 1.6, hsL, hsR, 1.0);   // v64: the horse passes
    float *vwL = zeros(), *vwR = zeros(); spatialize(vowM[0], NULL, vow_az0, el_near, 1.05, vwL, vwR, 1.0); spatialize(vowM[1], NULL, vow_az1, el_near, 1.1, vwL, vwR, 1.0); spatialize(vowM[2], NULL, vow_az2, el_near, 1.05, vwL, vwR, 1.0);   // v68: her vowel choir around her
    float *gsL = zeros(), *gsR = zeros(); { float *gm = zeros(); for (long i = 0; i < N; i++) gm[i] = 0.5f * (sample(gtr.L, gtr.n, i) + sample(gtr.R, gtr.n, i)); spatialize(gm, NULL, seat_gtr, el_low, 0.7, gsL, gsR, 1.0); free(gm); }   // v66: nearer
    float *melL = zeros(), *melR = zeros();                            // v33: the melody bus — sister + chorale just behind her, the mirror sweeping
    spatialize(melM, NULL, seat_c, el_near, 1.05, melL, melR, 1.0);
    spatialize(mirM, NULL, sweep_az, el_up, 1.3, melL, melR, 1.0);
    float *hL = zeros(), *hR = zeros();
    for (int h = 0; h < NHARM; h++) {
        if (!harm[h].L) continue;
        float *g = automate(v_harm_g, h), *az = automate(v_harm_az, h);
        int any = 0; for (long i = 0; i < N; i += 480) if (g[i] > 0.002) { any = 1; break; }
        if (any) {
            float *m = zeros(); long d = lround(HARM_DELAY[h] * SR);
            for (long i = 0; i < N; i++) { m[i] = sample(harm[h].L, harm[h].n, i - d) * g[i] * 0.6f; az[i] += (float)sway((double)i / SR, h); }   // v11: sway
            spatialize(m, az, NULL, el_near, 1.0, hL, hR, 1.0);
            free(m);
        }
        free(g); free(az);
        // v6: the arpeggiated copy of the same stem, each step at its own seat
        int anyArp = 0; for (long i = 0; i < N; i += 480) if (arpG[h][i] > 0.002) { anyArp = 1; break; }
        if (anyArp) {
            float *m = zeros();
            for (long i = 0; i < N; i++) m[i] = sample(harm[h].L, harm[h].n, i) * arpG[h][i];
            spatialize(m, arpAz[h], NULL, el_near, 0.9, hL, hR, 1.0);
            free(m);
        }
    }

    // v7: the choir, three seats behind her (further than the harmonies), keyed and pumped in the mix
    float *cL = zeros(), *cR = zeros();
    { float *cg = automate(v_choir, 0); Path seats[3] = { orbit0, orbit1, orbit2 };   // v11: the choir orbits
      for (int c = 0; c < 3; c++) { if (!choir[c].L) continue; float *m = zeros();
          for (long i = 0; i < N; i++) m[i] = sample(choir[c].L, choir[c].n, i) * cg[i] * (c == 1 ? 0.8f : 1.0f);
          spatialize(m, NULL, seats[c], el_back, 1.6, cL, cR, 1.0); free(m); }
      free(cg); }
    float *jg = automate(v_jeff, 0);
    // v11: the sister — high and low octaves dallying around her, the hum behind
    float *sisL = zeros(), *sisR = zeros();
    { float *gh = automate(v_sis_hi, 0), *gl = automate(v_sis_lo, 0), *gm = automate(v_sis_hum, 0);
      if (sisHi.L) { float *m = zeros(); for (long i = 0; i < N; i++) m[i] = sample(sisHi.L, sisHi.n, i) * gh[i]; spatialize(m, NULL, dally_hi, dally_el, 1.1, sisL, sisR, 1.0); free(m); }
      if (sisLo.L) { float *m = zeros(); for (long i = 0; i < N; i++) m[i] = sample(sisLo.L, sisLo.n, i) * gl[i]; spatialize(m, NULL, dally_lo, el_near, 1.2, sisL, sisR, 1.0); free(m); }
      if (sisHum.L) { float *m = zeros(); for (long i = 0; i < N; i++) m[i] = sample(sisHum.L, sisHum.n, i) * gm[i]; spatialize(m, NULL, seat_c, el_back, 2.0, sisL, sisR, 1.0); free(m); }
      free(gh); free(gl); free(gm); }

    // ── mix ──
    float *sendA = automate(v_send, 0), *airA = automate(v_air, 0), *thickA = automate(v_thick, 0), *warmA = automate(v_warm, 0);
    float *herA = automate(v_her, 0), *acgA = automate(v_acg, 0), *elgA = automate(v_elg, 0);
    float *farA = automate(v_far, 0), *cathA = automate(v_cath, 0), *roomA = automate(v_room, 0), *dissA = automate(v_dissolve, 0);
    double dsL = 0, dsR = 0;   // v41: the dissolve lowpass state
    double gbL = 0, gbR = 0; const double k3kG = 1 - exp(-2 * PI * 3000 / SR);   // v48: her guitar's brightness shelf after the button
    float *screamA = automate(v_scream, 0), *orchA[NORCH]; for (int o = 0; o < NORCH; o++) orchA[o] = automate(v_orch, o);
    // v24: "spatialize the thing more" — the orchestra sits in the room on paths, one per part: the section
    // strings tour, the cello right and low, horns behind, timpani centre, the harp sweeps, the glock orbits
    // overhead, the choir behind on its lap, the quartet in its four seats, the taiko wide
    float *orKL = zeros(), *orKR = zeros(), *orDL = zeros(), *orDR = zeros();
    { static const struct { Path az, el; double dist, g; } SEAT[NORCH] = {
        [O_STRINGS] = { tour1, el_zero, 1.5, 1.0 }, [O_CELLO] = { seat_r, el_low, 1.3, 1.0 }, [O_PIZZ] = { dally_lo, el_near, 1.1, 1.0 },
        [O_HORNS] = { orbit0, el_back, 1.6, 1.0 }, [O_TIMP] = { seat_c, el_low, 1.2, 1.1 }, [O_HARP] = { sweep_az, el_up, 1.3, 1.0 },
        [O_GLOCK] = { orbit_az, orbit_el, 1.2, 1.0 }, [O_AAHS] = { orbit2, el_back, 1.7, 1.0 }, [O_VLN1] = { q_vln1, el_near, 1.2, 1.0 },
        [O_VLN2] = { q_vln2, el_near, 1.25, 1.0 }, [O_VIOLA] = { q_viola, el_near, 1.25, 1.0 }, [O_QCELLO] = { q_qcello, el_low, 1.3, 1.0 },
        [O_TAIKO] = { wide_az, el_zero, 1.0, 1.1 } };
      const long hpFrom = at(bar_n(68)->t); const double k180 = 1 - exp(-2 * PI * 180 / SR);
      for (int o = 0; o < NORCH; o++) { if (!orch[o].L) continue; float *m = zeros(); int any = 0; double h1 = 0, h2 = 0;
          const int lowPart = o == O_CELLO || o == O_QCELLO || o == O_AAHS || o == O_HORNS;   // v27: high-passed at 180 Hz from the break (the 808 and sub own 80–250 there)
          for (long i = 0; i < N; i++) { double x = 0.5 * (sample(orch[o].L, orch[o].n, i) + sample(orch[o].R, orch[o].n, i)) * orchA[o][i];
              if (lowPart && i >= hpFrom) { h1 += (x - h1) * k180; double y = x - h1; h2 += (y - h2) * k180; x = y - h2; }
              m[i] = (float)x; any |= m[i] != 0; }
          if (any) spatialize(m, NULL, SEAT[o].az, SEAT[o].el, SEAT[o].dist, ORCH_DRUM[o] ? orDL : orKL, ORCH_DRUM[o] ? orDR : orKR, SEAT[o].g);
          free(m); } }
    // v27: the orchestra buses — side high-passed at 500 Hz always, side −6 dB and a +4 dB shelf above 3 kHz where she is
    // not singing (break, finale): the seats' motion was turning the mids anti-phase and the top end went dark without her
    { float *sideA = automate(v_orch_side, 0), *shelfA = automate(v_orch_shelf, 0); const double k500 = 1 - exp(-2 * PI * 500 / SR), k3k = 1 - exp(-2 * PI * 3000 / SR);
      float *bus[2][2] = { { orKL, orKR }, { orDL, orDR } };
      for (int q = 0; q < 2; q++) { double slp = 0, mlp = 0;
          for (long i = 0; i < N; i++) { double m = (bus[q][0][i] + bus[q][1][i]) / 2, sd = (bus[q][0][i] - bus[q][1][i]) / 2;
              slp += (sd - slp) * k500; double sh = (sd - slp) * sideA[i];
              mlp += (m - mlp) * k3k; if (!q) m += (m - mlp) * shelfA[i];
              bus[q][0][i] = (float)(m + sh); bus[q][1][i] = (float)(m - sh); } }
      free(sideA); free(shelfA); }
    // v27: the cathedral ducks 6 dB from the button so her guitar is the ring-down
    { long atE = at(bar_n(84)->t); for (long i = atE; i < N; i++) cathA[i] *= 0.5f; }
    // v27: THE DROPOUT — the last beat before every lift (bars 27, 51, 72) the band leaves: her, her guitar, the stutter and the riser tail alone, then the impact
    // v57: THE GLITCH IS THE DROP — on her false "k-" (60.44) everything stops dead with her; 0.23 s of nothing but her hesitation,
    // then the chorus lands on the real "kiss" (60.675, bar 28's downbeat)
    { long a = at(60.435), z = at(60.672), r = (long)(0.003 * SR);
      float *gated[24] = { dL, dR, tL, tR, sL, sR, b808, spL, spR, hiL, hiR, hL, hR, cL, cR, sisL, sisR, jL, jR, orKL, orKR, orDL, orDR, wubM };
      for (int q = 0; q < 24; q++) for (long i = a - r; i < z; i++) { if (i < 0 || i >= N) continue; double g = i < a ? 1 - (double)(i - (a - r)) / r : 0; gated[q][i] *= (float)g; } }
    { static const int DROP_BARS[1] = { 72 };   // v56: no dropout under "Oh, won't you" (27, 51) — only into the bridge
      float *gate = (float *)malloc(N * sizeof(float)); for (long i = 0; i < N; i++) gate[i] = 1;
      for (int q = 0; q < 1; q++) { const ChartBar *db = bar_n(DROP_BARS[q]); double zT = pickup_bar(db->n) ? pickup_t(db) : db->t + db->dur, bd = pickup_bar(db->n) ? db->dur / db->nb : (db->t + db->dur - db->beats[db->nb - 1]);
          long a = at(zT - bd / 2), z = at(zT), r = (long)(0.005 * SR);   // v33: the last 8th before the lift; v35: before the pickup
          for (long i = a - r; i < z; i++) { if (i < 0 || i >= N) continue; double g = i < a ? 1 - (double)(i - (a - r)) / r : i >= z - r ? (double)(i - (z - r)) / r : 0; gate[i] = (float)fmin(gate[i], g); } }
      float *gated[24] = { dL, dR, tL, tR, sL, sR, b808, spL, spR, hiL, hiR, hL, hR, cL, cR, sisL, sisR, jL, jR, orKL, orKR, orDL, orDR, wubM };
      for (int q = 0; q < 24; q++) for (long i = 0; i < N; i++) gated[q][i] *= gate[i];
      free(gate); }
    const long atButton = at(bar_n(84)->t), atFirstWord = at(firstWordT);
    // v22: the scream — her lead levelled, driven into a clip, high-passed, growled at 31 Hz
    float *scr = zeros();
    { double env = 0, hp = 0; const double k250 = 1 - exp(-2 * PI * 250 / SR);
      for (long i = 0; i < N; i++) { double x = sample(vox.L, vox.n, i); env = fmax(fabs(x), env * 0.9996);
          double nz = env > 0.004 ? x / env : 0, d = tanh(nz * 3.2) * 0.9; hp += (d - hp) * k250;
          scr[i] = (float)((d - hp) * (1 + 0.45 * sin(2 * PI * 31 * (double)i / SR))); } }
    float *L = zeros(), *R = zeros(), *xL = zeros(), *xR = zeros();
    double env = 0, gcur = 1, e = 0, vlp = 0, dlp = 0, drp = 0, venv = 0, vwarm = 0;
    double ilpL = 0, ilpR = 0; const double introT = bar_n(11)->t, introT0 = startSecOut;   // v23: the intro lowpass state and where it is fully open
    const double k350 = 1 - exp(-2 * PI * 350 / SR);
    const double aA = exp(-1 / (0.02 * SR)), aR = exp(-1 / (0.35 * SR));   // v25: slower, gentler leveler
    double dlp5 = 0, dsEnv = 0; const double k5500 = 1 - exp(-2 * PI * 5500 / SR);   // v25: de-esser state
    double cHp = 0, cLp = 0, cFast = 0, cSlow = 0; const double kc22 = 1 - exp(-2 * PI * 2200 / SR), kc8 = 1 - exp(-2 * PI * 8000 / SR);   // v34: consonant enhancer state
    double vb1 = 0, vb2 = 0, vb3 = 0, vb4 = 0; const double kb350 = 1 - exp(-2 * PI * 350 / SR), kb2k = 1 - exp(-2 * PI * 2000 / SR), kb4k = 1 - exp(-2 * PI * 4000 / SR), kb6k = 1 - exp(-2 * PI * 6000 / SR);   // v55: tone bands
    const double k7 = 1 - exp(-2 * PI * 7000 / SR);
    { struct { const char *n; float *b; } B[25] = { {"dL",dL},{"tL",tL},{"sL",sL},{"spL",spL},{"hiL",hiL},{"hL",hL},{"cL",cL},{"sisL",sisL},{"orKL",orKL},{"orDL",orDL},{"wubM",wubM},{"melL",melL},{"pcL",pcL},{"htL",htL},{"gsL",gsL},{"g12L",g12L},{"vwL",vwL},{"hsL",hsL},{"pnL",pnL},{"radL",radL},{"stM",stM},{"exL",exL},{"rsL",rsL},{"b808",b808},{"bellM",bellM} };
      for (int q = 0; q < 25; q++) { if (!B[q].b) continue; long bad = -1; for (long i = 0; i < N; i++) if (B[q].b[i] != B[q].b[i]) { bad = i; break; } if (bad >= 0) fprintf(stderr, "NaN in %s at %.2f s\n", B[q].n, (double)bad / SR); } }
    for (long i = 0; i < N; i++) {
        // sidechain: kick + 808 pump the sines and the replay guitars
        const double tt = (double)i / SR;
        e = fmax(duck[i], e * 0.99992); double pump = 1 - 0.8 * fmin(1, e), hpump = 1 - 0.45 * fmin(1, e);   // v9: a firmer pump; v38: club
        // vocal leveler: RMS 4:1 over threshold, 10 ms / 200 ms (v6.3: harder, louder — she is the record)
        double x = sample(vox.L, vox.n, i), ghost = 0;
        if (tt >= 60.43 && tt < 60.67) x *= 2.0;   // v57: her false "k-" lifted +6 dB — the glitch, foregrounded
        // (v49: the k- cut moved to load time, across all her stems)
        for (int q = 0; q < 4; q++) { if (!fxS[q].L) continue; double u = tt - FX[q].t0; if (u < 0 || tt >= FX[q].t1) continue;
            double y = sample(fxS[q].L, fxS[q].n, i - at(FX[q].t0));
            if (FX[q].slide) { double w = fmin(1, u / 0.12); w = w * w * (3 - 2 * w); double keep = fmin(1, fmax(0, (FX[q].orig - tt) / 0.08));   // v37: replace her only while HER note lasts;
                x = x * (1 - w * keep) + y; }                                                                                                       // past it the extension is ADDED — her next words stay
            else ghost += y * 0.38; }                                                                                                                // the ghost sits under her
        // v34: CONSONANTS — the 2–8 kHz band's fast envelope against its slow one: when a burst stands out (a c, k, t, p), that band
        // is lifted for its length only; vowels and held sibilants are not. The first "coughed" gets a further lift by hand.
        { cHp += (x - cHp) * kc22; double hf = x - cHp; cLp += (hf - cLp) * kc8; double band = cLp;
          double a = fabs(band); cFast = a > cFast ? cFast + (a - cFast) * 0.25 : cFast * 0.9975; cSlow += (a - cSlow) * 0.0012;
          double ratio = cFast / (cSlow + 1e-4), boost = fmin(2.2, fmax(0, (ratio - 1.6) * 0.9));
          if (tt >= 39.50 && tt < 39.64) boost += 1.5;                                      // the first "coughed" (39.517 s on the stem clock)
          x += band * boost * 0.9; }
        double pw = x * x;
        env = pw > env ? aA * env + (1 - aA) * pw : aR * env + (1 - aR) * pw;
        double lv = sqrt(env), gt = lv > 0.06 ? pow(lv / 0.06, 1.0 / 2.5 - 1) : 1;   // v25: 2.5:1 (was 4:1) — "less processed"
        gcur = gt < gcur ? gt : gcur + (gt - gcur) * 0.0005;
        double v0 = (x + ghost) * gcur * (3.3 + 0.25 * warmA[i]) * (1 - 0.36 * fmin(1, e));   // v68: she NEEDS to be stronger   // v61: she is the loudest thing (2.2 → 2.8)   // v30: she pumps with the kick, ~−4 dB (v25: −2); v33: + the ghost   // v19: her voice pumps with the kick, audibly (was 0.2)
          // v15: a hair louder when close vlp += (v0 - vlp) * k7;   // her voice rides the kick's pump, lightly
        vwarm += (v0 - vwarm) * k350;
        const double warm = warmA[i];
        vb1 += (v0 - vb1) * kb350; vb2 += (v0 - vb2) * kb2k; vb3 += (v0 - vb3) * kb4k; vb4 += (v0 - vb4) * kb6k;   // v55: body / presence / tin bands
        double v = v0 + (v0 - vlp) * airA[i] * (1 - 0.85 * warm) * 0.5 + vwarm * 0.45 * warm                    // less air still
                 + vb1 * 0.32 + (vb3 - vb2) * 0.22 - (v0 - vb4) * 0.28;                                          // + body below 350, + presence 2–4 k, − tin above 6 k
        dlp5 += (v - dlp5) * k5500; { double hf = v - dlp5; dsEnv = fmax(fabs(hf), dsEnv * 0.9992); double ds = dsEnv > 0.05 ? 0.05 / dsEnv : 1; v = dlp5 + hf * ds; }   // v25: de-esser above 5.5 kHz   // v19: proximity halved (0.75) — less bedroom boom   // air above 7 kHz; proximity when warm
        v *= 1 - 0.65 * farA[i];   // v19: far = quiet (−9 dB), nothing else — she comes up as she comes closer
        // v6.3: HOLLOW — the bed is keyed to her: while she sings, sines, guitars,
        // harmonies and halo step back (up to −6 dB, 10 ms in, 200 ms out via env)
        venv = lv > venv ? venv + (lv - venv) * 0.02 : venv + (lv - venv) * 0.0006;
        const double vd = 1 - 0.6 * fmin(1, venv / 0.05), hd = 1 - 0.4 * fmin(1, venv / 0.05);   // v26: the bed steps back further while she sings (−8 dB)
        double hv = sample(halo.L, halo.n, i) * 0.15 * hd * (1 - 0.6 * warm);   // v26: halo down   // v8.1: the halo up; v8.2: mostly out while she is up close
        // v6.4: THICK — two drifting copies of her lead, 28 and 41 ms late (the platter's
        // unison spacing), each wandering ±1.2 ms so they detune a few cents, panned apart
        double d1 = (0.028 + 0.0012 * sin(2 * PI * 0.13 * tt)) * SR, d2 = (0.041 + 0.0012 * sin(2 * PI * 0.31 * tt + 1)) * SR;
        long j1 = i - (long)d1, j2 = i - (long)d2; double f1 = d1 - floor(d1), f2 = d2 - floor(d2);
        double c1 = (sample(vox.L, vox.n, j1) * (1 - f1) + sample(vox.L, vox.n, j1 - 1) * f1) * gcur * 2.0 * thickA[i];
        double c2 = (sample(vox.L, vox.n, j2) * (1 - f2) + sample(vox.L, vox.n, j2 - 1) * f2) * gcur * 2.0 * thickA[i];
        double thL = c1 * 0.42 + c2 * 0.18, thR = c1 * 0.18 + c2 * 0.42;
        dlp = dL[i]; drp = dR[i];                                          // v5.1: kit full-band + forward (was 4.5 kHz lowpass)
        double sl = tanh(sL[i] * 1.8 * 1.4) / tanh(1.4) * pump * vd, sr = tanh(sR[i] * 1.8 * 1.4) / tanh(1.4) * pump * vd;
        // v23 INTRO: "hear her guitar just a bit in the very start, a semblance of it in lower pitches" —
        // before her first word (bar 11) her guitar comes through a lowpass that opens from 220 Hz
        // and a gain that creeps up from a third, so the picture of her strumming has a sound
        double gtl = gsL[i], gtrr = gsR[i];   // v60: her guitar from its seat
        // v23b: "too much of her starting guitar" — an eighth to start, held low until the last bars (k³), lowpass from 150 Hz
        if (0 && tt < introT) { double k = fmax(0, (tt - introT0) / (introT - introT0)), fc = 150 * pow(6000 / 150.0, k * k * k), kc = 1 - exp(-2 * PI * fc / SR);
            ilpL += (gtl - ilpL) * kc; ilpR += (gtrr - ilpR) * kc; double ig = 0.12 + 0.88 * k * k * k; gtl = ilpL * ig; gtrr = ilpR * ig; }
        const double gBtn = (i >= atButton ? 1.6 : 1) * (tt >= CLIMB_T0 && tt < CLIMB_T1 ? fmax(0.15, 1 - (tt - CLIMB_T0) / (CLIMB_T1 - CLIMB_T0) * 1.2) : 1) * (tt >= bar_n(70)->t && tt < PITCHY_T1 ? 1.6 : 1) * (tt >= PITCHY_T0 && tt < PITCHY_T1 ? 0.5 : 1);   // v73: +4 dB through the dead zone; the dry guitar yields to the pitchy one   // v27: +4 dB after the button; v59: the dry guitar yields to the climb
        double gl = ((gtl + g12L[i]) * herA[i] * 1.3 * hpump * gBtn) + (sample(acg.L, acg.n, i) * acgA[i] * 0.55 + sample(elg.L, elg.n, i) * elgA[i] * 0.8) * pump * vd;   // v66: HER guitar is not hollowed under her voice (the replays still are)
        double gr = ((gtrr + g12R[i]) * herA[i] * 1.3 * hpump * gBtn) + (sample(acg.R, acg.n, i) * acgA[i] * 0.55 + sample(elg.R, elg.n, i) * elgA[i] * 0.8) * pump * vd;
        double pl = (spL[i] + hiL[i] * pump) * vd, pr = (spR[i] + hiR[i] * pump) * vd;
        double jl = jL[i] * pump, jr = jR[i] * pump;
        double jd = (sample(jeff[0].L, jeff[0].n, i) * 0.7 + sample(jeff[1].L, jeff[1].n, i) * 0.4) * jg[i] * pump * hd;   // the drone: centre, never moves
        double chl = cL[i] * hpump * hd, chr = cR[i] * hpump * hd;
        double ssl = sisL[i] * hpump * hd * 0.9, ssr = sisR[i] * hpump * hd * 0.9;
        // v22: the scream rides with her (not keyed), two copies 9 and 14 ms late, wide
        double scl = sample(scr, N, i - 432) * screamA[i] * 0.22, scrr = sample(scr, N, i - 672) * screamA[i] * 0.22;
        // v22: the orchestra — pads keyed to her (hollow) and pumped; the timpani and pizz untouched by her voice
        double ol = (orKL[i] * pump * vd + orDL[i] * hpump) * 0.6, orr = (orKR[i] * pump * vd + orDR[i] * hpump) * 0.6;   // v61: under her   // v24: from the spatial buses
        double wb = wubM[i] * pump * 0.55;   // v24: the wub, pumped with the kick, mono (bass rule)
        // v41: everything that is not her closes through a lowpass across the finale (8 kHz → 300 Hz by bar 84) — the dissolve
        double roomGl = 0, roomGr = 0;
        if (i >= atButton) { double d = fmin(1, (double)(i - atButton) / (0.8 * SR));   // v48: over 0.8 s her guitar steps out of the dissolve, +6 dB above 3 kHz — the room, bright and dry
            gbL += (gl - gbL) * k3kG; gbR += (gr - gbR) * k3kG; roomGl = (gl + (gl - gbL) * 1.0) * d; roomGr = (gr + (gr - gbR) * 1.0) * d; gl *= 1 - d; gr *= 1 - d; }
        double bandL = hL[i] * hpump * hd + hv * 0.6 * hpump + gl + dlp * 1.3 + pcL[i] * 1.2 + htL[i] * 1.3 + pnL[i] * pump * vd * 0.9 + hsL[i] * 1.0 + vwL[i] * hpump * 1.1 + (tL[i] + rsL[i]) * 1.3 + pL[i] * 0.9 + b808[i] * 0.42 + sl * 0.85 + pl + exL[i] * 0.5 + jl + chl + jd + ssl + ol + wb + melL[i] * pump;
        double bandR = hR[i] * hpump * hd + hv * 0.6 * hpump + gr + drp * 1.3 + pcR[i] * 1.2 + htR[i] * 1.3 + pnR[i] * pump * vd * 0.9 + hsR[i] * 1.0 + vwR[i] * hpump * 1.1 + (tR[i] + rsR[i]) * 1.3 + pR[i] * 0.9 + b808[i] * 0.42 + sr * 0.85 + pr + exR[i] * 0.5 + jr + chr + jd + ssr + orr + wb + melR[i] * pump;
        if (dissA[i] > 0.001) { double d = dissA[i], fc = 8000 * pow(300.0 / 8000, d), kd = 1 - exp(-2 * PI * fc / SR); dsL += (bandL - dsL) * kd; dsR += (bandR - dsR) * kd; bandL = dsL; bandR = dsR; }
        L[i] = (float)(v + thL + scl + stM[i] * 1.5 + bandL + radL[i] + roomGl);
        R[i] = (float)(v + thR + scrr + stM[i] * 1.5 + bandR + radR[i] + roomGr);
        // v6.4: SPACE — more of everything into the room, the doubles and the jets too
        // v8.2: no room on her in the opening — the send is closed while warm, and opens as the mix grows
        xL[i] = (float)(1 - 0.97 * warm) * roomA[i] * (float)(v * sendA[i] * 1.5 + thL * 0.8 + hL[i] * 0.5 + hv * 1.2 + sl * 0.5 + pl * 0.7 + dlp * 0.06 + gl * 0.1 + exL[i] * 0.8 + jl * 0.6 + chl * 0.9 + jd * 0.5 + ssl * 1.0 + scl * 0.6 + ol * 0.7 + wb * 0.15 + stM[i] * 0.8 + melL[i] * 0.5 + gl * sendA[i] * 1.3);   // v68: her guitar in her room
        xR[i] = (float)(1 - 0.97 * warm) * roomA[i] * (float)(v * sendA[i] * 1.5 + thR * 0.8 + hR[i] * 0.5 + hv * 1.2 + sr * 0.5 + pr * 0.7 + drp * 0.06 + gr * 0.1 + exR[i] * 0.8 + jr * 0.6 + chr * 0.9 + jd * 0.5 + ssr * 1.0 + scrr * 0.6 + orr * 0.7 + wb * 0.15 + stM[i] * 0.8 + melR[i] * 0.5 + gr * sendA[i] * 1.3);
    }
    // the room: near early reflections always on, a 2.2 s dark tail behind
    float *wL = zeros(), *wR = zeros();
    fdn(xL, xR, wL, wR, 4.2, 0.28, 0.035);   // v8.1: angelic — longer, brighter
    // v19: the cathedral — from verse 2's kick-off the same sends also feed a 7.5 s stone tail
    float *catL = zeros(), *catR = zeros(), *cwL = zeros(), *cwR = zeros();
    for (long i = 0; i < N; i++) { catL[i] = xL[i] * cathA[i]; catR[i] = xR[i] * cathA[i]; }
    fdn(catL, catR, cwL, cwR, 7.5, 0.2, 0.075);
    for (long i = 0; i < N; i++) { L[i] += cwL[i] * 0.42f; R[i] += cwR[i] * 0.42f; }
    free(catL); free(catR); free(cwL); free(cwR);
    static const double ROOM[6][3] = { { 0.011, 0.16, 0 }, { 0.017, 0.14, 1 }, { 0.023, 0.11, 0 }, { 0.031, 0.10, 1 }, { 0.043, 0.07, 0 }, { 0.053, 0.06, 1 } };
    for (long i = N - 1; i >= 0; i--) {
        double el = 0, er = 0;
        for (int r = 0; r < 6; r++) { long j = i - lround(ROOM[r][0] * SR); if (j >= 0) { double y = (xL[j] + xR[j]) * ROOM[r][1]; if (ROOM[r][2] > 0) er += y; else el += y; } }
        L[i] += (float)(wL[i] * 0.5 + el); R[i] += (float)(wR[i] * 0.5 + er);
    }
    // v27: OPEN AS THE PHONE — until her first word the whole mix is band-limited (300 Hz – 3.4 kHz, two poles each
    // side) like the recording it came from; it opens to the full band over 250 ms on the word
    { double h[2][2] = { { 0 } }, l[2][2] = { { 0 } }; const double kh = 1 - exp(-2 * PI * 300 / SR), kl = 1 - exp(-2 * PI * 3400 / SR);
      long z = atFirstWord + (long)(2.2 * SR);   // v43b: her first phrase arrives through the radio's band and opens over 2.2 s
      for (long i = 0; i < z && i < N; i++) { double k = i < atFirstWord ? 1 : 1 - (double)(i - atFirstWord) / (2.2 * SR); k = k * k * (3 - 2 * k);
          float *ch[2] = { L, R };
          for (int c = 0; c < 2; c++) { double x = ch[c][i]; h[c][0] += (x - h[c][0]) * kh; double y = x - h[c][0]; h[c][1] += (y - h[c][1]) * kh; y -= h[c][1];
              l[c][0] += (y - l[c][0]) * kl; l[c][1] += (l[c][0] - l[c][1]) * kl; ch[c][i] = (float)(x * (1 - k) + l[c][1] * 1.6 * k); } } }
    // v60: THE WAX PRINT — pop/lib/substrate.mjs "vinyl": tube drive 1.8 (tanh, normalized), an even-harmonic bias, a soft hiss
    // v60b: "things are maxing out" — the sum is brought to a 0.5 peak BEFORE the print (it was going in hot), drive 1.2 (was 1.8)
    { double pk = 1e-6; for (long i = 0; i < N; i++) { pk = fmax(pk, fabs(L[i])); pk = fmax(pk, fabs(R[i])); } const double pre = 0.5 / pk;
      const double drive = 1.2, nrm = 1 / tanh(drive), bias = 0.30 * 0.12, hiss = 0.0035; double hl = 0, hr = 0; const double kh = 1 - exp(-2 * PI * 6000 / SR);
      for (long i = 0; i < N; i++) { double l = L[i] * pre, r = R[i] * pre; l = tanh(l * drive) * nrm + bias * l * fabs(l); r = tanh(r * drive) * nrm + bias * r * fabs(r);
          hl += (rnd() - hl) * kh; hr += (rnd() - hr) * kh; L[i] = (float)(l * 0.9 + hl * hiss); R[i] = (float)(r * 0.9 + hr * hiss); } }
    // bass mono below 120 Hz (house rule)
    { double ml = 0, mr = 0, k = 1 - exp(-2 * PI * 120 / SR);
      for (long i = 0; i < N; i++) { ml += (L[i] - ml) * k; mr += (R[i] - mr) * k; double m = (ml + mr) / 2; L[i] += (float)(m - ml); R[i] += (float)(m - mr); } }
    // v6.6: the record starts on her first sung word (the first charted note after 23 s), 60 ms early
    long start = at(startSecOut); if (start < 0) start = 0;
    long endAt = END_BAR <= CHART_BARS[CHART_NBARS - 1].n ? at(bar_n(END_BAR)->t + 1.6) : N; if (endAt > N) endAt = N;
    { long last = at(bar_n(84)->t); for (long i = N - 1; i > last; i--) if (fabs(L[i]) > 0.004 || fabs(R[i]) > 0.004) { last = i; break; }   // v67: "too many dead seconds" — the file ends 0.5 s after the last thing heard
      long cut = last + (long)(0.5 * SR); if (cut < endAt) endAt = cut; }
    long M = endAt - start; float *oL = L + start, *oR = R + start;
    long fin = (long)(0.005 * SR), fout = (long)(0.4 * SR);   // v72: she is the first sample; v67: a short fall at the trimmed end
    for (long i = 0; i < fin; i++) { double g = 0.5 - 0.5 * cos(PI * i / fin); oL[i] *= (float)g; oR[i] *= (float)g; }
    for (long i = 0; i < fout; i++) { double g = 0.5 - 0.5 * cos(PI * i / fout); oL[M - 1 - i] *= (float)g; oR[M - 1 - i] *= (float)g; }
    double peak = 0; for (long i = 0; i < M; i++) { peak = fmax(peak, fabs(oL[i])); peak = fmax(peak, fabs(oR[i])); }
    for (long i = 0; i < M; i++) { oL[i] *= (float)(0.7 / peak); oR[i] *= (float)(0.7 / peak); }
    write_wav_f32(LANE "/out/sailor-song-" VERSION "-full.wav", oL, oR, M);
    // v12: her lead alone with a click on her (regularized) beats — to hear the vocal's smoothness on its own
    { float *cl = zeros(), *cr = zeros();
      double env2 = 0, g2 = 1; const double aA2 = exp(-1 / (0.01 * SR)), aR2 = exp(-1 / (0.2 * SR));
      for (long i = 0; i < N; i++) { double x = sample(vox.L, vox.n, i), pw = x * x; env2 = pw > env2 ? aA2 * env2 + (1 - aA2) * pw : aR2 * env2 + (1 - aR2) * pw;
          double lv = sqrt(env2), gt = lv > 0.06 ? pow(lv / 0.06, 1.0 / 4 - 1) : 1; g2 = gt < g2 ? gt : g2 + (gt - g2) * 0.0005;
          cl[i] = cr[i] = (float)(x * g2 * 2.0); }
      for (int k = 0; k < CHART_NBARS; k++) for (int j = 0; j < CHART_BARS[k].nb; j++) { long a2 = at(CHART_BARS[k].beats[j]); double f = j == 0 ? 1800 : 1200, g = j == 0 ? 0.35 : 0.22;
          for (long i = 0; i < 0.02 * SR && a2 + i < N; i++) { double u = (double)i / SR, v = sin(2 * PI * f * u) * exp(-u * 260) * g; cl[a2 + i] += (float)v * 0.7f; cr[a2 + i] += (float)v; } }
      double pk = 0; for (long i = start; i < endAt; i++) { pk = fmax(pk, fabs(cl[i])); pk = fmax(pk, fabs(cr[i])); }
      for (long i = 0; i < N; i++) { cl[i] *= (float)(0.8 / pk); cr[i] *= (float)(0.8 / pk); }
      write_wav_f32(LANE "/out/sailor-song-" VERSION "-vox-click.wav", cl + start, cr + start, M); free(cl); free(cr); }
    if (EV) { fprintf(EV, "\n]}\n"); fclose(EV); }
    printf("✓ %s/out/sailor-song-" VERSION "-full.wav  %.1f s (from %.2f s of the take, floor at bar %d), %d bars, %d notes\n", LANE, (double)M / SR, (double)start / SR, FLOOR_BAR, CHART_NBARS, CHART_NNOTES);
    return 0;
}
