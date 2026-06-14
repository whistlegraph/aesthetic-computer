// peaktek.c — a peak-time warehouse banger in C. Super-fast minimal techno at
// 150 BPM in F minor: a broken/shuffled pitch-enveloped sine kick, snappy noise
// hats, a snare-ish clap, a rumbling Reese sub, hard stabby resonant chord hits
// (drenched in a slap delay + wet reverb), and big noise risers that crescendo
// into each drop. A kick-triggered sidechain ducks the music bus HARD — the pump
// breathes deep. Arrangement is a SECBARS section map with a per-section lane
// mask so the groove tears elements in and out. render.mjs --engine peaktek
// masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o peaktek peaktek.c -lm
// Run:    ./peaktek --out out/peaktek-raw.wav

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
static double BPMV = 150, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.535;                  // tighter driving swing on odd 16ths

static uint32_t rng_s = 0x7065616b; // "peak"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// two buses: drums hit hard and dry; music (sub/stab/pad) gets ducked by the
// kick. revL/R is a send. trig[] marks kick onsets for the sidechain.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *trig;
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }

// ── syncopation model ────────────────────────────────────────────────────────
// Longuet-Higgins & Lee (1984) metric weights on a 16-step grid (higher = more
// metrically salient): step0 is the strongest downbeat. We score a pattern's
// syncopation per Witek/Clarke/Pearce/Wallentin/Vuust (2014) "Syncopation,
// Body-Movement and Pleasure in Groove Music" (PLOS ONE): an ONSET followed by a
// REST on a metrically STRONGER position is a syncopation; we sum those
// (W_rest − W_onset) weight differences. Pleasure / wanting-to-move follows an
// inverted-U over syncopation — it peaks at MEDIUM, not zero and not maxed — so
// the bass + stab patterns below are tuned into that groove sweet-spot.
static const int LHL_W[16] = {
    /*0*/ 0, /*1*/ -4, /*2*/ -3, /*3*/ -4, /*4*/ -2, /*5*/ -4, /*6*/ -3, /*7*/ -4,
    /*8*/ -1, /*9*/ -4, /*10*/ -3, /*11*/ -4, /*12*/ -2, /*13*/ -4, /*14*/ -3, /*15*/ -4
};
// score one 16-step pattern string: for each onset, find the next event position;
// if that next position is a REST whose metric weight is stronger (greater) than
// the onset's weight, add the difference. Wraps around the bar.
static int sync_score(const char *p) {
    int total = 0;
    for (int i = 0; i < 16; i++) {
        if (p[i] != 'x') continue;
        int j = (i + 1) % 16;                       // next grid position
        if (p[j] != 'x' && LHL_W[j] > LHL_W[i]) total += LHL_W[j] - LHL_W[i];
    }
    return total;
}
// onset count of a 16-step pattern (used to normalize density vs. syncopation).
static int onsets(const char *p) { int c = 0; for (int i = 0; i < 16; i++) if (p[i] == 'x') c++; return c; }

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

// ── drum voices ─────────────────────────────────────────────────────────────
// kick — punchy peak-time thump: pitch sweeps 140→44 Hz fast, hard tanh drive
// for weight, snappy click. Also stamps the sidechain trigger. KEEP the stamp.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.30 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 44 + 96 * exp(-tt * 50.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 9.5);
        double click = exp(-tt * 420.0) * 0.85;
        double v = tanh((sin(ph) + click) * 2.3) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Tight and bright for drive.
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.11 : 0.035) * SR);
    double dec = open ? 48.0 : 155.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.45;
        addD(s0 + i, v * lg, v * rg);
    }
}

// clap — snappy three-spit snare-ish hit then a diffuse tail; wetter verb send.
static void clap(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double spits = exp(-fmod(tt, 0.009) * 700.0) * (tt < 0.030 ? 1.0 : 0.0);
        double tail = exp(-tt * 18.0);
        double v = hp * (spits * 0.9 + tail) * g * 0.55;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.35, v * 0.35);             // wetter verb on the clap
    }
}

// rim — short clicky resonant blip: a fast-decaying triangle-ish tone + a noise
// transient. Sits in the syncopated off-grid slots for the medium-sync groove.
static void rim(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * 1750.0 / SR;
        double tone = sin(ph) * exp(-tt * 90.0);
        double click = rnd2() * exp(-tt * 520.0) * 0.6;
        double v = (tone + click) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
    }
}

// shaker — band-limited noise burst with a soft attack; granular drive filler.
static void shaker(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double env = (tt < 0.012 ? tt / 0.012 : exp(-(tt - 0.012) * 60.0));
        double v = hp * env * g * 0.30;
        addD(s0 + i, v * lg, v * rg);
    }
}

// ride — a longer metallic shimmer: several inharmonic partials + noise, with a
// soft ping. Rides the offbeats in the big sections for warehouse air.
static void ride(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.20 * SR); double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double ph[5] = {0,0,0,0,0}, fr[5] = {3200, 4300, 5600, 7100, 9300}, prev = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, m = 0;
        for (int k = 0; k < 5; k++) { ph[k] += TAU * fr[k] / SR; m += sin(ph[k]); }
        double nz = rnd2(); double hp = nz - prev; prev = nz;
        double v = (m * 0.06 + hp * 0.12) * exp(-tt * 11.0) * g * 0.35;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.15, v * 0.15);
    }
}

// tom — pitched membrane: sine with a quick downward pitch sweep, body+thump.
// Used for fills that tumble into drops.
static void tom(double t, double f0, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.18 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f0 * (0.6 + 0.4 * exp(-tt * 24.0));
        ph += TAU * pf / SR;
        double v = sin(ph) * exp(-tt * 13.0) * g * 0.6;
        addD(s0 + i, v * lg, v * rg);
    }
}

// impact — sub boom + noise splash to mark the top of a drop/section change.
static void impact(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.9 * SR); double ph = 0, prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 80 * exp(-tt * 6.0) + 38;
        ph += TAU * pf / SR;
        double boom = sin(ph) * exp(-tt * 3.2);
        double nz = rnd2(); double hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 1800.0 / SR), q = 1.0 / 1.0;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double splash = (hp - low) * exp(-tt * 7.0) * 0.5;
        double v = (boom + splash) * g * 0.8;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.30, v * 0.30);
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — a rumbling Reese: two slightly-detuned saws folded through tanh, then a
// fixed lowpass to keep it sub-heavy. Growls under the pump.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.004 * SR), rel = (long)(0.04 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0;
    double fc = 2.0 * sin(M_PI * 220.0 / SR), q = 1.0 / 1.1;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.008 / SR; if (s2 >= 1) s2 -= 1;     // slight detune = Reese beating
        double in = tanh(((2 * s1 - 1) + (2 * s2 - 1)) * 0.9) ;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v, v);
    }
}

// cbass — a plucky mid counter-bass: single saw through a resonant lowpass with
// a fast cutoff env (acid-ish pluck). Plays a MEDIUM-syncopated line that
// interlocks with the sub, filling the space the sub leaves on the off-grid.
static void cbass(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double sw = 0, low = 0, band = 0, q = 1.0 / 0.55, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.003 * SR), rel = (long)(0.05 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        sw += f / SR; if (sw >= 1) sw -= 1;
        double in = (2 * sw - 1);
        double cut = 280 + 2600 * exp(-tt * 30.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
    }
}

// stab — hard resonant chord hit: three detuned saws through a resonant SVF
// lowpass with a snappy cutoff env. The aggressive peak-time chord. Sends to a
// slap delay + reverb downstream for width.
static void stab(const int *ch, double t, double dur, double g, double res, double cut0, double cut1, double pan) {
    long s0 = (long)(t * SR), n = (long)(dur * SR);
    double sw[3] = {0,0,0}, fr[3]; for (int z = 0; z < 3; z++) fr[z] = midi_hz(ch[z]);
    double low = 0, band = 0, q = 1.0 / fmax(0.4, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.002 * SR), rel = (long)(0.03 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, in = 0;
        for (int z = 0; z < 3; z++) { sw[z] += fr[z] / SR; if (sw[z] >= 1) sw[z] -= 1; in += (2 * sw[z] - 1); }
        in *= 0.33;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 22.0);
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.30, v * 0.30);             // heavy verb send for warehouse tail
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (atmosphere).
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.25 * SR), rel = (long)(0.4 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1300.0 / SR), q = 1.0 / 0.9;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.006 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.32, v * 0.32);
    }
}

// noise riser — big filtered white-noise crescendo into a drop. Longer + wider
// sweep + a pitch-rising tone underneath for that peak-time lift.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0, ph = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / dur;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (300 + 8000 * p) / SR), q = 1.0 / 1.2;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double tone; ph += TAU * (180 + 1400 * p * p) / SR; tone = sin(ph) * 0.25 * p * p;
        double v = ((hp - low) + tone) * p * p * g;     // rising bandpass + sweep tone
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.35, v * 0.35);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_CLAP = 8, L_SUB = 16, L_STAB = 32, L_PAD = 64,
       L_RIM = 128, L_SHKR = 256, L_RIDE = 512, L_CBASS = 1024 };

// at 150 BPM a bar is 1.6s; 72 bars ≈ 115.2s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "brk1", "grvB", "drop", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 4, 16, 4, 12, 8 };  // sum = 72
static int MASK[8] = {
    /*intro*/ L_KICK | L_CHAT | L_RIM,
    /*build*/ L_KICK | L_CHAT | L_OHAT | L_SUB | L_RIM | L_SHKR,
    /*grvA */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_STAB | L_RIM | L_SHKR | L_CBASS,
    /*brk1 */ L_SUB | L_STAB | L_PAD | L_RIM | L_CBASS,
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_STAB | L_PAD | L_RIM | L_SHKR | L_RIDE | L_CBASS,
    /*drop */ L_PAD | L_SUB | L_CBASS,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_CLAP | L_SUB | L_STAB | L_PAD | L_SHKR | L_RIDE | L_CBASS,
    /*outro*/ L_KICK | L_SUB | L_RIM | L_PAD,
};
static int START[8];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark natural-minor harmony in F: bass roots walk i–VI–III–VII per 4 bars.
static const int ROOTS[4] = { 29, 25, 32, 27 };          // F1, Db1, Ab1, Eb1
// F natural minor scale tones for stab voicing roots: F Ab Bb C Db Eb
static const int STAB_ROOT[4] = { 53, 56, 60, 51 };      // F3, Ab3, C4, Eb3 — moves the chord
// pad triads in F minor: Fm, Db, Ab, Eb (low)
static const int PAD_CH[4][3] = { {41,44,48}, {37,41,44}, {44,48,51}, {39,43,46} }; // Fm Db Ab Eb (low)
// counter-bass roots an octave above the sub root, walking the 5th for motion.
static const int CBASS_ROOT[4] = { 41, 37, 44, 39 };     // F2, Db2, Ab2, Eb2

int main(int argc, char **argv) {
    const char *out_path = "out/peaktek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# peaktek.c · %g BPM · %d bars · %.1fs · peak-time minimal techno (F minor)\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d)", ORDER[i], SECBARS[i]); fprintf(stderr, "\n");

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x.x.x.";   // driving offbeat hats with a 16th lift
    const char *P_OHAT = "..x...x...x...x.";   // open-hat ride on the offbeats
    const char *P_SHKR  = "x.x.x.x.x.x.x.x.";   // steady 8th shaker bed
    const char *P_RIDE  = "..x...x...x...x.";   // ride on the offbeats

    // ── THE EXPERIMENT: a controlled sweep across the syncopation inverted-U ──
    // Witek/Clarke/Pearce/Wallentin/Vuust (2014) find that the urge to move
    // ("groove") rises then falls with syncopation: an inverted-U peaking at a
    // MEDIUM degree, not at zero (stiff) and not at the maximum (too weird to
    // entrain). This track is the apparatus: the four groove-carrying lanes
    // (sub, stab, cbass, rim) carry a different pattern in each major section,
    // dialled to a rising RUNG of syncopation — LOW → MEDIUM → HIGH (over-shoot)
    // → SWEET (settled back into the medium band) — so a listener hears the
    // groove build, peak, over-shoot into awkwardness, then resolve into the
    // pocket. sync_score() is computed for every active groove pattern per
    // section and the per-section trajectory is printed to stderr at render time.
    enum { RUNG_LOW = 0, RUNG_MED = 1, RUNG_HIGH = 2, RUNG_SWEET = 3 };
    // [rung] variants for each groove lane. LOW = on-grid/downbeat-locked (≈0);
    // MED = the canonical interlocking groove; HIGH = onsets crowded onto weak
    // odd steps that each precede a stronger rest → maximal LHL score (15, the
    // over-shoot); SWEET = a refined pocket in the lower-medium band that this
    // material settles into.
    const char *SUB_V[4]   = { "x...x...x...x...", "x.xx.x.x.x.x.x.x", ".x.x.x.x.x.x.x.x", "x.xx.x..x.xx.x.." };
    const char *STAB_V[4]  = { "....x.......x...", ".x.x.x...x.x.x..", ".x.x.x.x.x.x.x.x", ".x.x.....x.x...." };
    const char *CBASS_V[4] = { "x.......x.......", ".x.x...x.x.x....", ".x.x.x.x.x.x.x.x", ".x.....x.x......" };
    const char *RIM_V[4]   = { "....x.......x...", ".x.x...x.x...x..", ".x.x.x.x.x.x.x.x", ".x...x...x......" };
    // section → rung map (aligned to ORDER below). The grooves climb the U:
    // build is stiff (LOW), grvA locks the medium groove, grvB over-shoots HIGH,
    // grvC settles into the SWEET pocket. brk1/drop/intro/outro use SWEET-ish.
    static const int SEC_RUNG[8] = {
        /*intro*/ RUNG_LOW, /*build*/ RUNG_LOW, /*grvA*/ RUNG_MED, /*brk1*/ RUNG_MED,
        /*grvB*/ RUNG_HIGH, /*drop*/ RUNG_SWEET, /*grvC*/ RUNG_SWEET, /*outro*/ RUNG_SWEET
    };
    static const char *RUNG_NAME[4] = { "LOW", "MED", "HIGH", "SWEET" };

    // report the per-section syncopation TRAJECTORY across the inverted-U so the
    // sweep is fully auditable at render time. Σ is the section's total groove
    // syncopation (sum over the four lanes active that section).
    fprintf(stderr, "# === syncopation sweep across the inverted-U (LHL/Witek 2014) ===\n");
    fprintf(stderr, "# section  rung   sub stab cbas rim |  Σ   onsets\n");
    int peak_sync = -1, peak_sec = 0;
    for (int s = 0; s < 8; s++) {
        int r = SEC_RUNG[s], m = MASK[s], sigma = 0, ons = 0;
        int vs = (m & L_SUB)   ? sync_score(SUB_V[r])   : 0;
        int vt = (m & L_STAB)  ? sync_score(STAB_V[r])  : 0;
        int vc = (m & L_CBASS) ? sync_score(CBASS_V[r]) : 0;
        int vr = (m & L_RIM)   ? sync_score(RIM_V[r])   : 0;
        sigma = vs + vt + vc + vr;
        if (m & L_SUB)   ons += onsets(SUB_V[r]);
        if (m & L_STAB)  ons += onsets(STAB_V[r]);
        if (m & L_CBASS) ons += onsets(CBASS_V[r]);
        if (m & L_RIM)   ons += onsets(RIM_V[r]);
        if (sigma > peak_sync) { peak_sync = sigma; peak_sec = s; }
        fprintf(stderr, "# %-7s %-5s  %3d %4d %4d %3d | %3d  %4d\n",
                ORDER[s], RUNG_NAME[r], vs, vt, vc, vr, sigma, ons);
    }
    fprintf(stderr, "# peak Σ-syncopation = %d at section '%s' (the over-shoot); "
            "groove sweet-spot for this material sits at the SWEET rung "
            "(sub=%d stab=%d cbas=%d rim=%d, Σ=%d).\n",
            peak_sync, ORDER[peak_sec],
            sync_score(SUB_V[RUNG_SWEET]), sync_score(STAB_V[RUNG_SWEET]),
            sync_score(CBASS_V[RUNG_SWEET]), sync_score(RIM_V[RUNG_SWEET]),
            sync_score(SUB_V[RUNG_SWEET]) + sync_score(STAB_V[RUNG_SWEET]) +
            sync_score(CBASS_V[RUNG_SWEET]) + sync_score(RIM_V[RUNG_SWEET]));

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        // select this section's syncopation rung → the groove-lane patterns.
        int rung = SEC_RUNG[s];
        const char *P_SUB = SUB_V[rung], *P_STAB = STAB_V[rung];
        const char *P_CBASS = CBASS_V[rung], *P_RIM = RIM_V[rung];
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // kick — broken/shuffled four-on-the-floor: main on beats, plus a
                // ghosted pickup on st 14, and a gap on fill bars for tension.
                if ((m & L_KICK) && st % 4 == 0 && !(fill && st == 12)) kick(t, 0.97);
                if ((m & L_KICK) && st == 14 && !fill && (b % 2 == 1)) kick(t, 0.62); // shuffle pickup
                // hats
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.52 + 0.1 * (st % 4 == 2), 0, rnd2() * 0.35);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.22) hat(t, 0.24, 0, rnd2() * 0.45); // 16th ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.40, 1, 0.18);
                // clap/snare on 2 & 4
                if ((m & L_CLAP) && (st == 4 || st == 12)) clap(t, 0.72);
                // rim accents — syncopated, panned, riding the medium-sync grid
                if ((m & L_RIM) && P_RIM[st] == 'x') rim(t, 0.55, rnd2() * 0.6);
                // shaker bed — steady 8ths with a touch of velocity wobble
                if ((m & L_SHKR) && P_SHKR[st] == 'x') shaker(t, 0.5 + 0.2 * (st % 4 == 0), rnd2() * 0.3);
                // ride shimmer on the offbeats (big sections only)
                if ((m & L_RIDE) && P_RIDE[st] == 'x') ride(t, 0.42, 0.25);
                // rumbling sub
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root + 12, t, STEP * 1.5, 0.80);
                // counter-bass — medium-sync pluck interlocking with the sub
                if ((m & L_CBASS) && P_CBASS[st] == 'x') {
                    int cr = CBASS_ROOT[phr % 4];
                    // walk to the 5th on the second half of the phrase for motion
                    if (b % 4 >= 2 && st >= 8) cr += 7;
                    cbass(cr, t, STEP * 1.1, 0.30, rnd2() * 0.4);
                }
                // hard stab — chord built off a moving root, with stutter retrigs on fills
                if ((m & L_STAB) && P_STAB[st] == 'x') {
                    int r = STAB_ROOT[phr % 4];
                    int ch[3] = { r, r + 3, r + 7 };      // minor triad voicing
                    double dur = STEP * 1.2;
                    stab(ch, t, dur, 0.42, 4.5 + 1.5 * (s >= 4), 3200, 520, rnd2() * 0.5);
                    if (fill && st >= 12) {                // stutter retrigger into the drop
                        stab(ch, t + STEP * 0.5, dur * 0.6, 0.38, 6.0, 3600, 600, rnd2() * 0.6);
                    }
                }
                // off-grid syncopated stab — a single bright extra chord on the
                // weak "e of 3" (step 11). Reserved for the SWEET-rung sections so
                // it ornaments the settled pocket without raising the audible rung
                // of the LOW/MED/HIGH experiment stages.
                if ((m & L_STAB) && rung == 3 && b % 4 >= 2 && st == 11) {
                    int r = STAB_ROOT[phr % 4];
                    int ch[3] = { r + 12, r + 15, r + 19 };  // octave-up bright voicing
                    stab(ch, t, STEP * 0.9, 0.30, 7.0, 4200, 900, -0.4);
                }
            }
            // pad chord — one per 4-bar phrase, sustained
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.10, (z - 1) * 0.4); }
            // tom tumble fill in the last bar of a phrase in the big grooves —
            // a descending tom run on the back half to telegraph the turnaround.
            if (fill && (m & L_KICK) && (s == 2 || s == 4 || s == 6)) {
                double tf[6] = {220, 185, 160, 135, 110, 90};
                for (int k = 0; k < 6; k++) tom(step_t(bar, 6 + k) , tf[k], 0.55, (k % 2 ? 0.4 : -0.4));
            }
            // big riser in the last bar before a groove/drop lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.20);
            // impact boom on the very first downbeat of each big groove + the drop
            if (b == 0 && (s == 2 || s == 4 || s == 5 || s == 6)) impact(bar * BAR, 0.5);
        }
    }

    // ── slap delay on the stab/verb send: 3/16-note feedback echo for width ──
    { long dly = (long)(STEP * 3 * SR); double fb = 0.36;
      if (dly > 0 && dly < N) {
        for (long i = dly; i < N; i++) {
            revL[i] += (float)(revR[i - dly] * fb);     // ping-pong cross-feed
            revR[i] += (float)(revL[i - dly] * fb);
        }
      } }

    // ── sidechain: kick-triggered DEEP duck on the music bus (the pump) ──
    // depth 0.82 + a fast attack hold so it breathes hard on each kick.
    { double depth = 0.82, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── wetter Schroeder reverb on the send (warehouse tail) ──
    {
        double decay = 0.80, wet = 0.40, damp = 0.40;
        int CD[4]; double cds[4] = { 0.0297, 0.0371, 0.0411, 0.0437 };
        for (int k = 0; k < 4; k++) CD[k] = (int)(cds[k] * SR);
        float *cbL[4], *cbR[4]; int ciL[4] = {0}, ciR[4] = {0}; double lpL[4] = {0}, lpR[4] = {0};
        for (int k = 0; k < 4; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 4; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                lpL[k] = dL * (1 - damp) + lpL[k] * damp; lpR[k] = dR * (1 - damp) + lpR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + lpL[k] * decay); cbR[k][ciR[k]] = (float)(inR + lpR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            musL[i] += (float)(cL / 4 * wet); musR[i] += (float)(cR / 4 * wet);
        }
        for (int k = 0; k < 4; k++) { free(cbL[k]); free(cbR[k]); }
    }

    // ── mix buses, normalize, short fades ──
    float *busL = drumL, *busR = drumR;             // reuse drum bus as the mix
    for (long i = 0; i < N; i++) { busL[i] += musL[i]; busR[i] += musR[i]; }
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.89 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(0.3 * SR), fout = (long)(1.6 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
