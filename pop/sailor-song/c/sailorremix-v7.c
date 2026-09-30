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
// Build: bash pop/sailor-song/c/build.sh
// Run:   pop/sailor-song/c/sailorremix    (from the repo root)
//        → pop/sailor-song/out/sailor-song-v5-full.wav + .events.json
//        bash pop/sailor-song/c/cut.sh → master (−10 LUFS) + mp3 w/ cover

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../nullabye/c/ac_hrtf.h"
#include "sailor-chart.h"

#define SR 48000
#define VERSION "v7"
#define START_BAR 11         // v6.6: (the pre-roll is now her first WORD; see main)
#define FLOOR_BAR 28         // v7: the floor lands on "kiss"
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
    [INTRO]   = { 1, 0, 0,      0, 0, 0, 0, 0, 0, 0,        0, 0, 0, 0, 0, 0, 0, 0, 0,      // v6: bedroom — her guitar alone
                  { { 0 } }, .10, .3 },
    [VERSE1]  = { .95, .15, 0,  0, .4, 0, 0, 0, 0, 0,       .8, .7, 0, 0, 0, 0, 0, 0, .9,   // v6: end state of the evolution (gated per bar below)
                  { H(H_DOWN3, .16, 40) }, .07, .45 },
    [CHORUS1] = { .45, .55, .6, .9, .8, .6, 0, 0, .35, .8,  1, .8, .7, .2, 0, 0, 0, .5, .6,
                  { H(H_UP3, .30, -50), H(H_DOWN3, .26, 50), H(H_DOWN6, .18, 0) }, .14, .3 },
    [VERSE2]  = { .8, .2, .2,   .8, .5, 0, 0, .3, 0, .4,    .7, .5, 0, 0, 0, 0, 0, .3, .9,   // v6.6: a verse, not a chorus
                  { H(H_DOWN3, .24, 45) }, .08, .45 },
    [CHORUS2] = { .4, .6, .85,  1, .9, .8, 0, 0, .5, 1,     1, .8, .9, .2, .2, 1, 0, .6, .5,
                  { H(H_UP3, .32, -60), H(H_DOWN3, .30, 60), H(H_DOWN6, .22, -20), H(H_DOWN8, .20, 0) }, .15, .35 },
    [BREAK]   = { .9, 0, 0,     0, 0, 0, 0, 0, 0, 0,        .8, 1, 1, 0, 0, .6, 1, .6, .8,
                  { { 0 } }, .20, .2 },
    [BRIDGE]  = { .5, .5, .75,  .8, .7, .5, 0, 0, .4, .8,   1, .8, .9, 0, .2, .5, 0, .7, .6,
                  { { 0 } }, .09, .5 },                   // harmonies rove, see harm_at()
    [OUTRO]   = { .9, 0, .4,    .5, 0, 0, .4, 0, 0, .3,     .8, .8, .6, 0, 0, 0, 0, .4, 1,
                  { H(H_DOWN8, .30, 0), H(H_DOWN3, .22, 40), H(H_UP3, .16, -40) }, .20, .15 },
};
// "pitching around": in the bridge her harmony changes interval and place each bar
static double evo(int bar, int from, int to);
static HarmSet harm_at(int bar, int h) {
    int s = section_of(bar);
    if (s == VERSE1) { HarmSet r = ARR[s].harm[h]; r.g *= evo(bar, 18, 24); return r; }   // v6: her harmony creeps in
    if (h == H_UP8) {                                                                    // v6.2: her voice an octave up
        if (bar >= 41 && bar <= 43) return (HarmSet){ .70, 0 };                          // end of chorus 1
        if (bar >= 60 && bar <= 67) return (HarmSet){ 1.0, 0 };                          // second half of chorus 2
        if (bar >= 77 && bar <= 80) return (HarmSet){ 1.0, 0 };                          // the bridge's climb
        if (bar >= 81) return (HarmSet){ .6, 0 };
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
            double v = lp * exp(-u * (k < 2 ? 90 : 16)) * g * AMP[k] * 0.9; add(tL, a + i, v); add(tR, a + i, v * 0.95); }
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
        double c3 = det > 0 ? sin(pc_) * 0.5 : 0, xa = tanh((sin(pa) + c3) * 2.2) / 1.0, xb = tanh((sin(pb) + c3) * 2.2) / 1.0;   // v6.2: power sines
        if (!R) add(L, a + i, (xa + xb) * 0.5 * env * trem * g);
        else { add(L, a + i, xa * env * trem * gl); add(R, a + i, xb * env * trem * gr); }
    }
}
#define PAD(t, d, m, g, pan) sine(sL, sR, t, d, m, g, pan, 0.35, 0.9, 7)

// ── AC voices ────────────────────────────────────────────────────────────
// FEM bells from the baked bank (bin/bells.sh). Loner rule: ≤ E5, fade tails.
static Stereo BELLS[128];
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
static int third_above(int m) {
    int pc = ((m % 12) + 12) % 12, oct = m / 12 - (m < 0), i = -1;
    for (int k = 0; k < 7; k++) if (SCALE[k] == pc) i = k;
    if (i < 0) return m + 3;
    int t = SCALE[(i + 2) % 7];
    return oct * 12 + t + (t < pc ? 12 : 0);
}
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
static double v_her(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); double g = ARR[s].her; return s == VERSE1 ? g * evo(b->n, 13, 17) : g; }   // v6.6: no guitar to start — it creeps in after her first line
// v6.6: warm — the opening vocal close to the mic: proximity below 350 Hz, no air, almost no room; eases out by the floor
// v7: the choir stands behind her in the choruses, the bridge and the outro; a breath of it in verse 2
static double v_choir(const ChartBar *b, int x) { (void)x; int s = section_of(b->n);
    return s == CHORUS1 ? 0.55 * (0.4 + 0.6 * evo(b->n, FLOOR_BAR, FLOOR_BAR + 4)) : s == VERSE2 ? 0.22 : s == CHORUS2 ? 0.7 : s == BRIDGE ? 0.6 : s == OUTRO ? 0.5 : 0; }
static double v_jeff(const ChartBar *b, int x) { (void)x; int s = section_of(b->n);
    return s == CHORUS1 ? 0.30 * evo(b->n, FLOOR_BAR + 2, FLOOR_BAR + 6) : s == CHORUS2 ? 0.42 : s == BRIDGE ? 0.45 : s == OUTRO ? 0.3 : 0; }
static double seat_l(double t) { (void)t; return -48; }
static double seat_c(double t) { (void)t; return 0; }
static double seat_r(double t) { (void)t; return 48; }
static double el_back(double t) { (void)t; return -8; }
static double v_warm(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == INTRO ? 1 : s == VERSE1 ? 1 - evo(b->n, 20, FLOOR_BAR) : 0; }
static double v_thick(const ChartBar *b, int x) { (void)x; int s = section_of(b->n); return s == INTRO ? 0 : s == VERSE1 ? 0.35 * evo(b->n, 16, 24) : s == BREAK ? 0 : s == VERSE2 ? 0.5 : 0.8; }
static double v_harm_g(const ChartBar *b, int h) { return harm_at(b->n, h).g; }
static double v_harm_az(const ChartBar *b, int h) { return harm_at(b->n, h).az; }

// ── space: ac_hrtf over a whole bus ─────────────────────────────────────
static double impact_disp(double t);
typedef double (*Path)(double t);
static void spatialize(const float *mono, const float *azA, Path az, Path el, double dist, float *L, float *R, double g) {
    ACHrtf h; memset(&h, 0, sizeof h);
    for (long i = 0; i < N; i++) {
        double t = (double)i / SR, a = azA ? azA[i] : az(t);
        float l, r;
        a += impact_disp(t);                                              // v6: the ring is displaced and springs back
        ac_hrtf_process(&h, mono[i], a * PI / 180, el(t) * PI / 180, dist, &l, &r);
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
static const int IMPACT_BARS[] = { 28, 52, 73, 81 };
#define VOICE_LAG 0.05       // v7: her sung onset sits ~50 ms behind the strum (ANALYSIS.md §2); the explosion waits for it
// v6: the evolution — 0 before `from`, 1 at `to`, cosine between (one lane per phrase)
static double evo(int bar, int from, int to) { if (bar < from) return 0; if (bar >= to) return 1; double x = (double)(bar - from) / (to - from); return 0.5 - 0.5 * cos(PI * x); }
#define NIMPACT ((int)(sizeof IMPACT_BARS / sizeof *IMPACT_BARS))
static double IMPACT_T[NIMPACT];
static double impact_disp(double t) {          // degrees of azimuth displacement
    double d = 0;
    for (int k = 0; k < NIMPACT; k++) { double u = t - IMPACT_T[k];
        if (u >= 0 && u < 6) d += 110 * exp(-1.2 * u) * sin(2 * PI * 1.1 * u); }
    return d;
}
static double last_impact(double t) { double best = -1e9; for (int k = 0; k < NIMPACT; k++) if (IMPACT_T[k] <= t && IMPACT_T[k] > best) best = IMPACT_T[k]; return best; }
// the explosion: one full lap around the head in 0.7 s, climbing overhead
static double expl_az(double t) { double u = t - last_impact(t); return u < 0 ? 0 : 360 * fmin(1, u / 0.7) * (1 - 0.15 * u); }
static double expl_el(double t) { double u = t - last_impact(t); return u < 0 ? 0 : 60 * fmin(1, u / 0.5) * exp(-u * 0.8); }
static float *exM;
static void explosion(double t, double g) {   // noise chiff: bandpass falling 6 kHz → 250 Hz over 0.9 s, a sub thump underneath on the drums bus
    long a = at(t); double b1 = 0, b2 = 0, ph = 0;
    for (long i = 0; i < 1.1 * SR; i++) { double u = (double)i / SR, fc = 250 + 5750 * exp(-u * 4.2), k = 1 - exp(-2 * PI * fc / SR);
        double nz = rnd(); b1 += (nz - b1) * k; b2 += (b1 - b2) * k;
        double env = fmin(1, u / 0.004) * exp(-u * 3.2);
        add(exM, a + i, (b1 - b2) * env * g * 2.2); }
    for (long i = 0; i < 0.5 * SR; i++) { double u = (double)i / SR; ph += 2 * PI * (38 + 90 * exp(-u * 20)) / SR;
        double v = sin(ph) * exp(-u * 7) * g * 0.7; add(dL, a + i, v); add(dR, a + i, v); }
    ev(t, "impact", 1.0, g, -1);
}

// ── v6 · vocal arpeggios: her harmony stems gated into figures on her grid ──
// -1 = rest. div = steps per beat (2 = 8ths, 3 = triplets, 4 = 16ths).
typedef struct { int div; int len; int step[8]; double g; double hold; } Arp;   // hold: step lengths each note sings (v6.1: > 1 so the stack rolls, never chops)
static const Arp ARP[NSEC] = {
    [INTRO]   = { 0 }, [VERSE1] = { 0 },
    [CHORUS1] = { 2, 4, { H_UP3, H_DOWN3, H_UP3, H_DOWN6 }, .34, 1.8 },                  // 8ths, a rocking third, each held 1.8 steps
    [VERSE2]  = { 0 },                                                                     // the sustained down3 sings here
    [CHORUS2] = { 4, 8, { H_DOWN3, H_UP3, H_UP5, H_DOWN6, H_UP3, H_DOWN3, H_DOWN8, H_UP3 }, .30, 3.0 },   // 16ths, up then down, three steps deep
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
static double hum_t2(double t, double ms) { return t + rnd() * ms / 1000; }   // v6.4: looser hands
static double hum_g2(double g, double pct) { return g * (1 + rnd() * pct); }

// ── the dance layer (v5.2: "turn the song into a dance mix") ─────────────
// Her ~120 is house tempo, so four on HER floor: a kick on every one of her
// beats, claps 2 + 4, open hats on every offbeat, an offbeat house bass on
// her roots, risers into the drops.
static void dance_kick(double t, double g) {
    long a = at(t); double ph = 0;
    for (long i = 0; i < 0.42 * SR; i++) { double u = (double)i / SR;
        ph += 2 * PI * (50 + 160 * exp(-u * 38) + 40 * exp(-u * 300)) / SR;
        double click = i < 0.003 * SR ? rnd() * 0.6 * (1 - u / 0.003) : 0;
        double v = (tanh(sin(ph) * 2.4) * 0.85 * exp(-u * 5.5) + click) * g;
        add(dL, a + i, v); add(dR, a + i, v); }
    for (long i = 0; i < 0.3 * SR; i++) add(duck, a + i, g * 1.6 * exp(-(double)i / SR * 8));
}
static void open_hat(double t, double g) {
    long a = at(t); double p1 = 0, p2 = 0;
    for (long i = 0; i < 0.16 * SR; i++) { double u = (double)i / SR, nz = rnd(), h1 = nz - p1; p1 = nz; double h2 = h1 - p2; p2 = h1;
        double v = h2 * fmin(1, u / 0.002) * exp(-u * 16) * g * 0.3; add(tL, a + i, v * 0.8); add(tR, a + i, v * 1.1); }
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
        double v = (b1 - b2) * x * x * g; add(tL, a + i, v * (1 - 0.3 * sin(PI * 8 * x))); add(tR, a + i, v * (1 + 0.3 * sin(PI * 8 * x))); }
}
//                    4otf  clap  ohat  hbass  (v5.2)
static const double DANCE[NSEC][4] = {
    [INTRO] = { 0, 0, 0, 0 }, [VERSE1] = { .8, 0, .5, .7 }, [CHORUS1] = { 1, 1, .8, 1 }, [VERSE2] = { .65, .3, .3, .5 },
    [CHORUS2] = { 1, 1, 1, 1 }, [BREAK] = { 0, 0, 0, 0 }, [BRIDGE] = { 1, .8, .8, .9 }, [OUTRO] = { .7, .4, .3, .5 } };

// ── hand percussion (v5.1: "we have no actual percussion yet") ───────────
static float *pL, *pR;
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
    [INTRO] = { 0, 0, 0 }, [VERSE1] = { .45, 0, .5 }, [CHORUS1] = { .8, .8, .7 }, [VERSE2] = { .45, .2, .5 },
    [CHORUS2] = { 1, 1, .8 }, [BREAK] = { .4, 0, .5 }, [BRIDGE] = { .8, .7, .8 }, [OUTRO] = { .3, 0, .5 } };

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
    const char *stems = getenv("SAILOR_STEMS") ? getenv("SAILOR_STEMS") : "reg";
    const int regd = strcmp(stems, "reg") == 0;
    char pth[512];
    #define STEM(name) (snprintf(pth, sizeof pth, LANE "/src/vox/%s/%s.wav", stems, name), pth)
    #define GSTEM(name) (regd ? STEM(name) : (snprintf(pth, sizeof pth, LANE "/src/vox/%s.wav", name), pth))
    Stereo vox  = load_wav(STEM("vocals-aesthetivox"));
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
    fprintf(stderr, "stems: %s\n", stems);
    // v7: her first sung word = the first 30 ms window of the lead stem after bar START_BAR
    // above −45 dBFS RMS (guitar bleed in the stem sits near −60; her voice enters near −35)
    double startSecOut = CHART_BARS[START_BAR - 1].t;
    { long w = (long)(0.03 * SR), from = at(CHART_BARS[START_BAR - 1].t);
      for (long i = from; i + w < vox.n; i += w / 3) { double acc = 0; for (long j = 0; j < w; j++) acc += (double)vox.L[i + j] * vox.L[i + j];
          if (sqrt(acc / w) > 0.0056) { startSecOut = (double)i / SR - 0.06; break; } } }
    if (!vox.L || !gtr.L) { fprintf(stderr, "! run bin/aesthetivox.py + bin/lock-vox.mjs first\n"); return 1; }
    N = vox.n;
    pL = zeros(); pR = zeros();
    bedroom_level(&gtr, CHART_BARS[15].t);                             // v6: until bar 16
    pop_guitar(&gtr, 1.5); pop_guitar(&acg, 1.3);
    dL = zeros(); dR = zeros(); tL = zeros(); tR = zeros(); b808 = zeros(); sL = zeros(); sR = zeros();
    hiM = zeros(); bellM = zeros(); vibM = zeros(); duck = zeros(); exM = zeros(); jL = zeros(); jR = zeros();
    for (int h = 0; h < NHARM; h++) { arpG[h] = zeros(); arpAz[h] = zeros(); }
    for (int q = 0; q < NIMPACT; q++) IMPACT_T[q] = -1e9;
    EV = fopen(LANE "/out/sailor-song-" VERSION ".events.json", "w");
    if (EV) fprintf(EV, "{\"tempoBPM\":120,\"seconds\":%.2f,\"startSec\":%.3f,\"startBar\":%d,\"floorBar\":%d,\"stems\":\"%s\",\"sections\":[", (double)N / SR, startSecOut, START_BAR, FLOOR_BAR, stems);
    if (EV) for (int s = 0; s < NSEC; s++) {
        int a = SEC_FROM[s] - 1, z = (s + 1 < NSEC ? SEC_FROM[s + 1] : CHART_NBARS + 1) - 2;
        fprintf(EV, "%s{\"name\":\"%s\",\"start\":%.3f,\"end\":%.3f}", s ? "," : "", SEC_NAME[s], CHART_BARS[a].t, CHART_BARS[z].t + CHART_BARS[z].dur);
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
        const double *dz = DANCE[s];
        for (int q = 0; q < NIMPACT; q++) if (IMPACT_BARS[q] == b->n) { IMPACT_T[q] = bt[0] + VOICE_LAG; explosion(bt[0] + VOICE_LAG, b->n == FLOOR_BAR ? 0.45 : 0.6); }   // v6.5: subtler
        const double gDrop = s == CHORUS1 ? 0.55 + 0.45 * evo(b->n, FLOOR_BAR, FLOOR_BAR + 3) : 1;   // v6.5: the first floor eases in under her line
        // v6.6: hills — every section rises to its middle and settles into its edge
        const int secLen = (s + 1 < NSEC ? SEC_FROM[s + 1] : CHART_NBARS + 1) - SEC_FROM[s];
        const double hill = s == INTRO ? 1 : 0.84 + 0.16 * sin(PI * (b->n - SEC_FROM[s] + 0.5) / secLen);
        // v6: the evolution gates (verse 1 only; every other section is its table)
        const int v1 = s == VERSE1;
        const double gKick = evo(b->n, 24, 26), gHat = evo(b->n, 25, 28), gBass = evo(b->n, 24, 27);
        const double gSub = evo(b->n, 16, 22), gTen = evo(b->n, 20, 26), gShk = evo(b->n, 14, 18), gCon = evo(b->n, 18, 22);   // v7: her first line alone; the shuffle from bar 14, congas from 18
        // v6 HOCKET: the floor doubles her 1 and 3 and lands in her empty 2 and 4;
        // open hats on the complement's &1/&3; her own &2/&4 get a soft accent at her bin
        if (dz[0] && gKick > 0) for (int j = 0; j < nb; j++) { if (v1 && j % 2) continue;   // verse 1: her 1 and 3 only
            double g = 0.95 * dz[0] * (j % 2 ? 0.9 : 1) * gKick * hill;
            dance_kick(bt[j], g); ev(bt[j], "kick", 0.1, g, -1); }
        if (dz[1]) for (int j = 1; j < nb; j += 2) { clap(hum_t2(bt[j], 6), hum_g2(0.55 * dz[1] * swell * gDrop * hill, 0.15)); ev(bt[j], "clap", 0.1, dz[1], -1); }
        if (dz[2] && gHat > 0) for (int j = 0; j < nb; j++) {
            if (j % 2 == 0) { open_hat(hum_t2(MID(j) + BIN_OFF, 6), hum_g2(0.5 * dz[2] * swell * gHat * gDrop * hill, 0.18)); ev(MID(j), "hat", 0.1, dz[2], -1); }
            else { trap_hat(hum_t2(MID(j) + BIN_OFF, 5), hum_g2(0.28 * dz[2] * swell * gHat * gDrop, 0.18), 0.3); ev(MID(j), "hat", 0.02, dz[2] * 0.5, -1); } }
        // house bass: her pickups (&2/&4) in the verses, every offbeat in the choruses
        if (dz[3] && gBass > 0) for (int j = 0; j < nb; j++) { if (dz[3] < 0.95 && j % 2 == 0) continue;
            int r = ROOT[b->chord] - 12 + (j == nb - 1 && b->n % 2 ? 7 : 0);
            house_bass(MID(j) + BIN_OFF, r, beatDur * 0.42, 0.55 * dz[3] * gBass * gDrop * hill); ev(MID(j), "bass", beatDur * 0.42, dz[3], r); }
        // her arpeggios
        if (ARP[s].div) { const Arp *A = &ARP[s]; int c = ((b->n - SEC_FROM[s]) * nb * A->div) % A->len;
            for (int j = 0; j < nb; j++) for (int q = 0; q < A->div; q++) {
                double t0 = bt[j] + (bt[j + 1] - bt[j]) * q / A->div + (q ? BIN_OFF : 0), t1 = bt[j] + (bt[j + 1] - bt[j]) * (q + 1) / A->div;
                int h = A->step[c % A->len]; c++;
                if (h < 0) continue;
                double az = s == BRIDGE ? SEAT[(b->n - 73 + q) % 5] : SEAT[(j * A->div + q) % 5];
                arp_step(h, t0, t1, A->g * swell * gDrop, az, A->hold); ev(t0, "arp", (t1 - t0) * A->hold, A->g, h); } }
        // risers: into each chorus + out of the break; a snare roll into the bridge's last bars
        if (b->n == 26 || b->n == 50 || b->n == 71 || b->n == 79) riser(b->t, b->t + b->dur + CHART_BARS[k + 1].dur, 0.22);
        // v6.4: airplanes — an 8-bar jet into chorus 1, chorus 2 and the bridge (4 bars out of the break)
        if (b->n == 20 || b->n == 44) jet(b->t, CHART_BARS[k + 8].t, 0.8);
        if (b->n == 68) jet(b->t, CHART_BARS[k + 5].t, 1.0);
        if (b->n == 28 || b->n == 52 || b->n == 73) air(b->t, CHART_BARS[k + (b->n == 73 ? 8 : 16) - 1].t + CHART_BARS[k + (b->n == 73 ? 8 : 16) - 1].dur, 0.10);
        // (v6.6: the snare roll into the bridge is gone — the jet is the hill)
        if (o->kick && !dz[0]) { kick(bt[0], 0.95 * o->kick); ev(bt[0], "kick", 0.1, o->kick, -1);
                       if (nb > 2) { kick(MID(1), 0.6 * o->kick); ev(MID(1), "kick", 0.1, o->kick * 0.6, -1); } }
        if (o->snare && gKick > 0) for (int j = 1; j < nb; j += 2) { snare(hum_t2(bt[j], 4), hum_g2(0.62 * o->snare * swell * gKick * hill, 0.1)); ev(bt[j], "snare", 0.1, o->snare, -1); }   // v6.6: the rock backbeat
        if (o->clap && nb > 2 && !dz[1]) { clap(bt[2], 0.5 * o->clap * swell); ev(bt[2], "clap", 0.1, o->clap, -1); }
        if (o->rim && nb > 3) { rim(MID(3) + BIN_OFF, 0.22 * o->rim); ev(MID(3), "rim", 0.05, o->rim, -1); }
        if (o->hat) for (int j = 0; j < nb; j++) { soft_hat(bt[j], 0.15 * o->hat, -0.35); soft_hat(MID(j), 0.09 * o->hat, 0.35); }
        {   // hand percussion on her grid: shaker 16ths, tambourine backbeat, congas on her &2 / &4
            const double *pc = PERC[s];
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
            if (dz[0] && gHat > 0 && nb > 3) {                                                     // v6.4: tresillo x..x..x. on a woodblock
                static const int TRES[3] = { 0, 3, 6 };
                for (int q = 0; q < 3; q++) { double t = bt[0] + (bt[4] - bt[0]) * TRES[q] / 8.0 + (TRES[q] % 2 ? BIN_OFF : 0);
                    block(hum_t2(t, 7), hum_g2(0.22 * dz[0] * gHat * (q == 0 ? 0.8 : 1), 0.25), q == 1 ? -0.5 : 0.5); ev(t, "block", 0.03, dz[0], -1); } }
            if (pc[1] && nb > 2) { tamb(hum_t2(bt[2], 8), hum_g2(0.30 * pc[1] * swell, 0.2), -0.4); ev(bt[2], "tamb", 0.1, pc[1], -1);
                                   if (nb > 3) tamb(hum_t2(MID(3) + BIN_OFF, 8), hum_g2(0.14 * pc[1], 0.2), -0.4); }
            if (pc[2] && gCon > 0) { int lo = ROOT[b->chord] + 12, hi = lo + 7; double cg = pc[2] * gCon;   // tuned to her root / fifth
                if (nb > 1) { conga(hum_t2(MID(1) + BIN_OFF, 9), hi, hum_g2(0.30 * cg, 0.22), 0.35, 0); ev(MID(1), "conga", 0.1, cg, hi); }
                if (nb > 3) { conga(hum_t2(bt[3], 9), lo, hum_g2(0.34 * cg, 0.22), 0.25, 0); conga(hum_t2(MID(3) + BIN_OFF, 9), hi, hum_g2(0.26 * cg, 0.22), 0.35, 1); ev(bt[3], "conga", 0.1, cg, lo); }
                if (b->n % 2 == 0 && nb > 2) { conga(hum_t2(bt[1] + (bt[2] - bt[1]) * 2 / 3.0, 9), hi, hum_g2(0.2 * cg, 0.25), 0.3, 1); } }   // v6.4: a triplet slap every other bar
        }
        // 808 on the kick points, gliding into her root
        if (o->b808 && !dz[3]) { int r = ROOT[b->chord] - 12;
            eight08(bt[0], r, beatDur * 1.4, 0.8 * o->b808); ev(bt[0], "808", beatDur * 1.4, o->b808, r);
            if (nb > 2) { eight08(MID(1), r, beatDur * 1.3, 0.6 * o->b808); ev(MID(1), "808", beatDur * 1.3, o->b808 * 0.6, r); } }
        // trap hats: 16ths, accents on the beat, a roll into every other bar line
        if (o->trap) {
            int roll = 0, trip = 0;   // v6.6: no rushes
            for (int j = 0; j < nb; j++) {
                int last = j == nb - 1 && roll, div = last ? (trip ? 6 : 8) : 4;
                for (int q = 0; q < div; q++) {
                    double t = bt[j] + (bt[j + 1] - bt[j]) * q / div;
                    double acc = q == 0 ? 1 : (q % 2 ? 0.55 : 0.75);
                    double g = 0.5 * o->trap * acc * (last ? 0.6 + 0.4 * q / div : 1) * swell;
                    trap_hat(t, g, q % 2 ? 0.45 : -0.25); ev(t, "hat", 0.02, g, -1);
                }
            }
        }
        // sine choir
        const double dur = b->dur * 0.97;
        if (o->sub && gSub > 0) { sine(sL, sR, b->t, dur, ROOT[b->chord] - 12, 0.18 * o->sub * gSub, 0, 0.35, 0.9, 0);
                      sine(sL, sR, b->t, dur, ROOT[b->chord], 0.12 * o->sub * gSub, 0, 0.35, 0.9, 1); ev(b->t, "sub", dur, o->sub * gSub, ROOT[b->chord] - 12); }
        lead(tenor, 3, pcs, 67, 79);
        if (o->tenor && gTen > 0) for (int v = 0; v < 3; v++) { PAD(b->t, dur, tenor[v], 0.16 * o->tenor * swell * gTen * gDrop * hill, (v - 1) * 0.6); ev(b->t, "pad", dur, o->tenor * gTen, tenor[v]); }
        lead(high, 2, pcs, 76, 88);
        if (o->high) for (int v = 0; v < 2; v++) { sine(hiM, NULL, b->t, dur, high[v], 0.08 * o->high * swell * gDrop, 0, 0.8, 1.4, 7); ev(b->t, "high", dur, o->high, high[v]); }
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
    // her melody, shadowed a 3rd up and an octave up
    for (int k = 0; k < CHART_NNOTES; k++) {
        const ChartNote *v = &CHART_NOTES[k]; const ChartBar *b = bar_at(v->t);
        if (!b || v->dur < 0.18) continue;
        const Arr *o = &ARR[section_of(b->n)];
        ev(v->t, "vox", v->dur, 1, v->midi);
        if (o->third) sine(sL, sR, v->t, v->dur, third_above(v->midi), 0.06 * o->third, 0.3, 0.06, 0.35, 2);
        if (o->octave) sine(hiM, NULL, v->t, v->dur, v->midi + 12, 0.03 * o->octave, 0, 0.08, 0.5, 4);
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
        const ChartBar *db = &CHART_BARS[sb->n + 40 - 1];
        double t = db->t + (v->t - sb->t) / sb->dur * db->dur, d = v->dur * db->dur / sb->dur;
        sine(sL, sR, t, d, v->midi, 0.09, 0, 0.05, 0.6, 3);
        sine(hiM, NULL, t, d, v->midi + 12, 0.035, 0, 0.08, 0.8, 5);
        ev(t, "hook", d, 1, v->midi);
    }

    // ── space ──
    float *spL = zeros(), *spR = zeros();
    spatialize(bellM, NULL, orbit_az, orbit_el, 1.2, spL, spR, 0.9);   // bells orbit overhead
    spatialize(vibM, NULL, sweep_az, el_zero, 1.2, spL, spR, 0.8);     // vibes sweep ear to ear
    float *hiL = zeros(), *hiR = zeros();
    spatialize(hiM, NULL, wide_az, el_up, 1.2, hiL, hiR, 1.3);         // highs float up + wide
    float *exL = zeros(), *exR = zeros();
    spatialize(exM, NULL, expl_az, expl_el, 0.8, exL, exR, 1.0);       // the explosion laps the head
    float *hL = zeros(), *hR = zeros();
    for (int h = 0; h < NHARM; h++) {
        if (!harm[h].L) continue;
        float *g = automate(v_harm_g, h), *az = automate(v_harm_az, h);
        int any = 0; for (long i = 0; i < N; i += 480) if (g[i] > 0.002) { any = 1; break; }
        if (any) {
            float *m = zeros(); long d = lround(HARM_DELAY[h] * SR);
            for (long i = 0; i < N; i++) m[i] = sample(harm[h].L, harm[h].n, i - d) * g[i] * 0.6f;
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
    { float *cg = automate(v_choir, 0); Path seats[3] = { seat_l, seat_c, seat_r };
      for (int c = 0; c < 3; c++) { if (!choir[c].L) continue; float *m = zeros();
          for (long i = 0; i < N; i++) m[i] = sample(choir[c].L, choir[c].n, i) * cg[i] * (c == 1 ? 0.8f : 1.0f);
          spatialize(m, NULL, seats[c], el_back, 1.6, cL, cR, 1.0); free(m); }
      free(cg); }
    float *jg = automate(v_jeff, 0);

    // ── mix ──
    float *sendA = automate(v_send, 0), *airA = automate(v_air, 0), *thickA = automate(v_thick, 0), *warmA = automate(v_warm, 0);
    float *herA = automate(v_her, 0), *acgA = automate(v_acg, 0), *elgA = automate(v_elg, 0);
    float *L = zeros(), *R = zeros(), *xL = zeros(), *xR = zeros();
    double env = 0, gcur = 1, e = 0, vlp = 0, dlp = 0, drp = 0, venv = 0, vwarm = 0;
    const double k350 = 1 - exp(-2 * PI * 350 / SR);
    const double aA = exp(-1 / (0.01 * SR)), aR = exp(-1 / (0.2 * SR));
    const double k7 = 1 - exp(-2 * PI * 7000 / SR);
    for (long i = 0; i < N; i++) {
        // sidechain: kick + 808 pump the sines and the replay guitars
        e = fmax(duck[i], e * 0.99990); double pump = 1 - 0.85 * fmin(1, e), hpump = 1 - 0.5 * fmin(1, e);   // v6.2: K-pop pump
        // vocal leveler: RMS 4:1 over threshold, 10 ms / 200 ms (v6.3: harder, louder — she is the record)
        double x = vox.L[i], pw = x * x;
        env = pw > env ? aA * env + (1 - aA) * pw : aR * env + (1 - aR) * pw;
        double lv = sqrt(env), gt = lv > 0.06 ? pow(lv / 0.06, 1.0 / 4 - 1) : 1;
        gcur = gt < gcur ? gt : gcur + (gt - gcur) * 0.0005;
        double v0 = x * gcur * 2.0 * (1 - 0.3 * fmin(1, e)); vlp += (v0 - vlp) * k7;   // her voice rides the kick's pump, lightly
        vwarm += (v0 - vwarm) * k350;
        const double warm = warmA[i];
        double v = v0 + (v0 - vlp) * airA[i] * (1 - 0.85 * warm) * 1.6 + vwarm * 0.75 * warm;   // air above 7 kHz; proximity when warm
        // v6.3: HOLLOW — the bed is keyed to her: while she sings, sines, guitars,
        // harmonies and halo step back (up to −6 dB, 10 ms in, 200 ms out via env)
        venv = lv > venv ? venv + (lv - venv) * 0.02 : venv + (lv - venv) * 0.0006;
        const double vd = 1 - 0.5 * fmin(1, venv / 0.05), hd = 1 - 0.3 * fmin(1, venv / 0.05);
        double hv = sample(halo.L, halo.n, i) * 0.13 * hd;
        // v6.4: THICK — two drifting copies of her lead, 28 and 41 ms late (the platter's
        // unison spacing), each wandering ±1.2 ms so they detune a few cents, panned apart
        double tt = (double)i / SR;
        double d1 = (0.028 + 0.0012 * sin(2 * PI * 0.13 * tt)) * SR, d2 = (0.041 + 0.0012 * sin(2 * PI * 0.31 * tt + 1)) * SR;
        long j1 = i - (long)d1, j2 = i - (long)d2; double f1 = d1 - floor(d1), f2 = d2 - floor(d2);
        double c1 = (sample(vox.L, vox.n, j1) * (1 - f1) + sample(vox.L, vox.n, j1 - 1) * f1) * gcur * 2.0 * thickA[i];
        double c2 = (sample(vox.L, vox.n, j2) * (1 - f2) + sample(vox.L, vox.n, j2 - 1) * f2) * gcur * 2.0 * thickA[i];
        double thL = c1 * 0.42 + c2 * 0.18, thR = c1 * 0.18 + c2 * 0.42;
        dlp = dL[i]; drp = dR[i];                                          // v5.1: kit full-band + forward (was 4.5 kHz lowpass)
        double sl = tanh(sL[i] * 2.4 * 1.4) / tanh(1.4) * pump * vd, sr = tanh(sR[i] * 2.4 * 1.4) / tanh(1.4) * pump * vd;
        double gl = (sample(gtr.L, gtr.n, i) * herA[i] * 0.8 * hpump + (sample(acg.L, acg.n, i) * acgA[i] * 0.55 + sample(elg.L, elg.n, i) * elgA[i] * 0.8) * pump) * vd;
        double gr = (sample(gtr.R, gtr.n, i) * herA[i] * 0.8 * hpump + (sample(acg.R, acg.n, i) * acgA[i] * 0.55 + sample(elg.R, elg.n, i) * elgA[i] * 0.8) * pump) * vd;
        double pl = (spL[i] + hiL[i] * pump) * vd, pr = (spR[i] + hiR[i] * pump) * vd;
        double jl = jL[i] * pump, jr = jR[i] * pump;
        double jd = (sample(jeff[0].L, jeff[0].n, i) * 0.7 + sample(jeff[1].L, jeff[1].n, i) * 0.4) * jg[i] * pump * hd;   // the drone: centre, never moves
        double chl = cL[i] * hpump * hd, chr = cR[i] * hpump * hd;
        L[i] = (float)(v + thL + hL[i] * hpump * hd + hv * 0.6 * hpump + gl + dlp * 1.25 + tL[i] * 1.1 + pL[i] * 0.9 + b808[i] * 0.42 + sl * 0.85 + pl + exL[i] * 0.5 + jl + chl + jd);
        R[i] = (float)(v + thR + hR[i] * hpump * hd + hv * 0.6 * hpump + gr + drp * 1.25 + tR[i] * 1.1 + pR[i] * 0.9 + b808[i] * 0.42 + sr * 0.85 + pr + exR[i] * 0.5 + jr + chr + jd);
        // v6.4: SPACE — more of everything into the room, the doubles and the jets too
        xL[i] = (float)(v * sendA[i] * 1.5 * (1 - 0.7 * warm) + thL * 0.8 + hL[i] * 0.5 + hv * 1.2 + sl * 0.5 + pl * 0.7 + dlp * 0.06 + gl * 0.1 + exL[i] * 0.8 + jl * 0.6 + chl * 0.9 + jd * 0.5);
        xR[i] = (float)(v * sendA[i] * 1.5 * (1 - 0.7 * warm) + thR * 0.8 + hR[i] * 0.5 + hv * 1.2 + sr * 0.5 + pr * 0.7 + drp * 0.06 + gr * 0.1 + exR[i] * 0.8 + jr * 0.6 + chr * 0.9 + jd * 0.5);
    }
    // the room: near early reflections always on, a 2.2 s dark tail behind
    float *wL = zeros(), *wR = zeros();
    fdn(xL, xR, wL, wR, 3.4, 0.45, 0.03);   // v6.4: a bigger room
    static const double ROOM[6][3] = { { 0.011, 0.16, 0 }, { 0.017, 0.14, 1 }, { 0.023, 0.11, 0 }, { 0.031, 0.10, 1 }, { 0.043, 0.07, 0 }, { 0.053, 0.06, 1 } };
    for (long i = N - 1; i >= 0; i--) {
        double el = 0, er = 0;
        for (int r = 0; r < 6; r++) { long j = i - lround(ROOM[r][0] * SR); if (j >= 0) { double y = (xL[j] + xR[j]) * ROOM[r][1]; if (ROOM[r][2] > 0) er += y; else el += y; } }
        L[i] += (float)(wL[i] * 0.5 + el); R[i] += (float)(wR[i] * 0.5 + er);
    }
    // bass mono below 120 Hz (house rule)
    { double ml = 0, mr = 0, k = 1 - exp(-2 * PI * 120 / SR);
      for (long i = 0; i < N; i++) { ml += (L[i] - ml) * k; mr += (R[i] - mr) * k; double m = (ml + mr) / 2; L[i] += (float)(m - ml); R[i] += (float)(m - mr); } }
    // v6.6: the record starts on her first sung word (the first charted note after 23 s), 60 ms early
    long start = at(startSecOut); if (start < 0) start = 0;
    long M = N - start; float *oL = L + start, *oR = R + start;
    long fin = (long)(0.005 * SR), fout = SR;
    for (long i = 0; i < fin; i++) { double g = 0.5 - 0.5 * cos(PI * i / fin); oL[i] *= (float)g; oR[i] *= (float)g; }
    for (long i = 0; i < fout; i++) { double g = 0.5 - 0.5 * cos(PI * i / fout); oL[M - 1 - i] *= (float)g; oR[M - 1 - i] *= (float)g; }
    double peak = 0; for (long i = 0; i < M; i++) { peak = fmax(peak, fabs(oL[i])); peak = fmax(peak, fabs(oR[i])); }
    for (long i = 0; i < M; i++) { oL[i] *= (float)(0.7 / peak); oR[i] *= (float)(0.7 / peak); }
    write_wav_f32(LANE "/out/sailor-song-" VERSION "-full.wav", oL, oR, M);
    if (EV) { fprintf(EV, "\n]}\n"); fclose(EV); }
    printf("✓ %s/out/sailor-song-" VERSION "-full.wav  %.1f s (from %.2f s of the take, floor at bar %d), %d bars, %d notes\n", LANE, (double)M / SR, (double)start / SR, FLOOR_BAR, CHART_NBARS, CHART_NNOTES);
    return 0;
}
