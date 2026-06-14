// dubtek.c — dub techno in C, forked from the minitek engine. Fast and driving
// (138 BPM) but smoky and spacious: wet off-beat stab chords (Basic Channel /
// Rhythm & Sound) drenched in a long, dark reverb and a tempo-synced feedback
// delay, a deep warm sub, a low rumbling reese, and a very sparse top. The
// signature is the off-beat chord stab dissolving into echo + verb while a
// deep kick-triggered sidechain breathes the whole music bus. Two buses: dry
// drums, ducked music. render.mjs --engine dubtek masters it on the club chain.
//
// Build:  cc -O3 -std=c11 -Wall -Wextra -Wno-unused-parameter -o dubtek dubtek.c -lm
// Run:    ./dubtek --out out/dubtek-raw.wav

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
static double BPMV = 138, BEAT, BAR, STEP;   // STEP = one 16th
static double SWING = 0.54;                   // micro-swing on odd 16ths (0.5 = straight)

static uint32_t rng_s = 0x64756274; // "dubt"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// two buses: drums hit hard and dry; music (bass/stab/pad) gets ducked by the
// kick. revL/R is the (heavy) verb send. stabL/R is a separate pre-delay tap
// that feeds the feedback delay so only the chord stabs echo. trig[] marks
// kick onsets for the sidechain.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *stabL, *stabR, *trig;
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }
static inline void addS(long i, double l, double r) { if (i >= 0 && i < N) { stabL[i] += (float)l; stabR[i] += (float)r; } }

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
// kick — deep dub kick: longer pitch sweep into a low 44 Hz body, soft round
// click, driven through tanh for round weight. Also stamps the sidechain.
static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.40 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 44 + 80 * exp(-tt * 34.0);
        ph += TAU * pf / SR;
        double amp = exp(-tt * 7.2);
        double click = exp(-tt * 260.0) * 0.45;
        double v = tanh((sin(ph) + click) * 1.8) * amp * g;
        addD(s0 + i, v, v);
    }
}

// hat — highpassed white noise; open=longer tail. Panned a touch off-centre.
// Soft and dusty (dub top is wispy, not crisp).
static void hat(double t, double g, int open, double pan) {
    long s0 = (long)(t * SR), n = (long)((open ? 0.16 : 0.035) * SR);
    double dec = open ? 30.0 : 150.0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;             // crude 1st-order highpass
        double v = hp * exp(-tt * dec) * g * 0.34;
        addD(s0 + i, v * lg, v * rg);
        if (open) addR(s0 + i, v * 0.35, v * 0.35);   // open hats drift into verb
    }
}

// rim — a short woody tick (dub backbeat instead of a big clap), with verb.
static void rim(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.06 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * 1700.0 / SR;
        double v = (sin(ph) * 0.6 + rnd2() * 0.4) * exp(-tt * 90.0) * g * 0.5;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.4, v * 0.4);               // rim swims in the verb
    }
}

// ── music voices (ducked) ───────────────────────────────────────────────────
// sub — deep, warm tanh-saturated sine bass with a slow attack and longer
// release; a touch of 2nd harmonic for body.
static void sub(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.012 * SR), rel = (long)(0.06 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = tanh((sin(ph) + 0.18 * sin(ph * 2)) * 1.15) * env * g;
        addM(s0 + i, v, v);
    }
}

// reese — detuned twin-saw bass through a slow lowpass; the rumbling, growling
// low-mid undertow that gives dub techno its weight.
static void reese(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.04 * SR), rel = (long)(0.12 * SR);
    double s1 = 0, s2 = 0, s3 = 0, low = 0, band = 0;
    double fc = 2.0 * sin(M_PI * 420.0 / SR), q = 1.0 / 1.4;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.007 / SR; if (s2 >= 1) s2 -= 1;
        s3 += f * 0.993 / SR; if (s3 >= 1) s3 -= 1;
        double in = ((2 * s1 - 1) + (2 * s2 - 1) + (2 * s3 - 1)) / 3.0;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v, v);
    }
}

// stab — the dub chord. Three detuned saws (a chord, passed as base + a small
// chord array) through a resonant lowpass with a quick pluck envelope, hard
// off-beat. Feeds BOTH the verb send and the feedback-delay tap so each stab
// smears into a long, echoing tail.
static void stab(const int *ch, int nch, double t, double dur, double g, double cut, double pan) {
    long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.004 * SR), rel = (long)(0.05 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double low = 0, band = 0, q = 1.0 / 2.2;
    double ph[8] = {0}, det[8];
    for (int z = 0; z < nch && z < 8; z++) det[z] = midi_hz(ch[z]) * (1.0 + 0.004 * rnd2());
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, in = 0;
        for (int z = 0; z < nch && z < 8; z++) { ph[z] += det[z] / SR; if (ph[z] >= 1) ph[z] -= 1; in += (2 * ph[z] - 1); }
        in /= (nch > 0 ? nch : 1);
        double c = cut * exp(-tt * 6.0) + 300.0;
        double fc = 2.0 * sin(M_PI * fmin(c, SR * 0.45) / SR);
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.55 * lg, v * 0.55 * rg);   // heavy verb send
        addS(s0 + i, v * 0.85 * lg, v * 0.85 * rg);   // into the feedback delay
    }
}

// pad — a soft detuned twin-saw wash through a fixed lowpass (smoky atmosphere),
// heavily sent to the verb.
static void pad(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.5 * SR), rel = (long)(0.8 * SR);
    double s1 = 0, s2 = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double fc = 2.0 * sin(M_PI * 1100.0 / SR), q = 1.0 / 0.9;
    for (long i = 0; i < n; i++) {
        s1 += f / SR; if (s1 >= 1) s1 -= 1;
        s2 += f * 1.006 / SR; if (s2 >= 1) s2 -= 1;
        double in = (2 * s1 - 1 + 2 * s2 - 1) * 0.5;
        double high = in - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.45, v * 0.45);
    }
}

// clave — a dry, woody high-pitched click (two sines + a touch of noise) that
// plays the son-clave timeline. The bright, dustier cousin of the rim.
static void clave(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double p1 = 0, p2 = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        p1 += TAU * 2400.0 / SR; p2 += TAU * 3300.0 / SR;
        double v = (sin(p1) * 0.55 + sin(p2) * 0.30 + rnd2() * 0.15) * exp(-tt * 120.0) * g * 0.5;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.25 * lg, v * 0.25 * rg);   // a little chamber on the clave
    }
}

// bell — a metallic ride/bell tone (a few inharmonic partials) for the bembé
// triplet timeline. Short, shimmery, panned, sent to the verb.
static void bell(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.12 * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double pf[4] = { 0,0,0,0 }; double f[4] = { 540.0, 812.0, 1190.0, 1734.0 };
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, v = 0;
        for (int k = 0; k < 4; k++) { pf[k] += TAU * f[k] / SR; v += sin(pf[k]) * (1.0 - 0.18 * k); }
        v *= 0.16 * exp(-tt * 26.0) * g;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.30 * lg, v * 0.30 * rg);
    }
}

// tom — a pitched membrane drum for dub fills; pitch sweeps down, panned.
static void tom(double t, double g, double f0, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.22 * SR); double ph = 0;
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f0 * (1.0 + 0.6 * exp(-tt * 22.0));
        ph += TAU * pf / SR;
        double v = tanh(sin(ph) * 1.4) * exp(-tt * 8.0) * g * 0.7;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.25 * lg, v * 0.25 * rg);
    }
}

// noise riser — filtered white-noise crescendo for transitions into a section.
static void riser(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / (dur);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (300 + 5000 * p) / SR), q = 1.0 / 1.3;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = (hp - low) * p * p * g;            // rising bandpass-ish
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.45, v * 0.45);
    }
}

// impact — a deep sub boom + filtered noise swell for drop/section heads.
static void impact(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.9 * SR); double ph = 0, prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 38 + 60 * exp(-tt * 9.0);
        ph += TAU * pf / SR;
        double boom = tanh(sin(ph) * 1.6) * exp(-tt * 3.0);
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 1800.0 / SR), q = 1.0 / 1.2;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double swell = (hp - low) * exp(-tt * 4.0) * 0.25;
        double v = (boom + swell) * g;
        addD(s0 + i, v, v);
        addR(s0 + i, v * 0.4, v * 0.4);
    }
}

// ── arrangement ─────────────────────────────────────────────────────────────
// lane bits for the per-section mask.
enum { L_KICK = 1, L_CHAT = 2, L_OHAT = 4, L_RIM = 8, L_SUB = 16, L_STAB = 32, L_PAD = 64, L_REESE = 128,
       L_CLAVE = 256, L_BELL = 512, L_CBASS = 1024 };

// ── TIMELINES (Toussaint, "The Geometry of Musical Rhythm") ─────────────────
// Master TIMELINE: the 3-2 son clave on the 16-step grid. Toussaint analyses the
// son clave as the maximally even / "rhythmic oddity" asymmetric timeline that
// organises Afro-Cuban (and, here, dub-techno) grooves. Onsets at steps:
//   0, 3, 6, 10, 12   (the 3-2 son clave). Its 2-3 rotation starts on the "2"
// side: 4, 6, 10, 13, 16(=0). We hang the chord stabs + key accents on this and
// drive the stab<->bass call-and-response off the two halves (3-side / 2-side).
static const int CLAVE_32[16] = { 1,0,0,1, 0,0,1,0, 0,0,1,0, 1,0,0,0 }; // 0,3,6,10,12
static const int CLAVE_23[16] = { 1,0,1,0, 0,0,1,0, 0,1,0,0, 1,0,0,0 }; // 0,2,6,9,12 (true 2-3 rot.)
// rumba clave (3-2): the rumba variant moves the third onset from step 6 to 7
// (the "tresillo+2" displacement). Onsets: 0,3,7,10,12. Toussaint compares it
// directly with the son as a near-maximally-even sister timeline.
static const int RUMBA_32[16] = { 1,0,0,1, 0,0,0,1, 0,0,1,0, 1,0,0,0 }; // 0,3,7,10,12
// 3-side = steps 0..7 (the syncopated half), 2-side = steps 8..15 (the answer).

// Secondary bell: the standard bembé bell as the Euclidean rhythm E(7,12) on a
// 12-pulse grid laid across the 4 beats (triplet feel). Toussaint shows the
// bembé/standard-pattern bell IS E(7,12). Onsets (12-grid): 0,2,4,5,7,9,11.
static const int BEMBE_12[12] = { 1,0,1,0, 1,1,0,1, 0,1,0,1 }; // E(7,12): 0,2,4,5,7,9,11
// the Euclidean bell E(5,16) on the 16-grid: the maximally even 5-onset timeline
// for n=16 (the "ideal" against which son/rumba are measured). Onsets 0,3,6,10,13.
static const int EUCLID_5_16[16] = { 1,0,0,1, 0,0,1,0, 0,0,1,0, 0,1,0,0 }; // E(5,16)

// ── TIMELINE THEORY (computed at render time, printed to stderr) ─────────────
// Three measurable properties of a binary timeline of period `per` with the
// given onset grid, after Arom (rhythmic oddity) and Toussaint (evenness):
//
//   1. RHYTHMIC ODDITY (Simha Arom 1991): a cycle of EVEN period 2k has the
//      oddity property iff NO pair of onsets divides the cycle into two equal
//      halves — i.e. for no onset i is there an onset at (i + per/2) mod per.
//      The son/rumba claves and the bembé bell are the canonical oddity rhythms.
//
//   2. EVENNESS (Toussaint): how close the onsets sit to a perfectly even
//      k-gon inscribed on the n-pulse cycle. We measure it as the mean pairwise
//      GEODESIC (shortest-arc) inter-onset distance, normalised by the mean
//      pairwise distance of the perfectly even k-point set on the same circle.
//      1.0 = maximally even; <1.0 = clustered.
//
//   3. OFF-BEATNESS (Toussaint): the count of onsets that fall on grid pulses
//      which can NEVER be a vertex of an inscribed regular polygon dividing the
//      cycle — i.e. pulses p with gcd(p, per) == 1. A higher off-beatness means
//      the timeline resists the on-beat skeleton; it is a syncopation proxy.

static int tl_onsets(const int *g, int per, int *out) {
    int k = 0; for (int p = 0; p < per; p++) if (g[p]) out[k++] = p; return k;
}

// rhythmic oddity: returns 1 if NO onset is bisected by the half-period.
static int tl_oddity(const int *g, int per) {
    if (per % 2) return 1;                  // odd period: vacuously holds
    int half = per / 2;
    for (int p = 0; p < per; p++) if (g[p] && g[(p + half) % per]) return 0;
    return 1;
}

static int gcd_i(int a, int b) { while (b) { int t = a % b; a = b; b = t; } return a; }

// off-beatness: onsets on pulses coprime to the period.
static int tl_offbeatness(const int *g, int per) {
    int c = 0; for (int p = 0; p < per; p++) if (g[p] && gcd_i(p, per) == 1) c++; return c;
}

// evenness ratio (1.0 == maximally even). Toussaint's geometric evenness: place
// the n pulses as vertices of a regular n-gon on the unit circle and measure the
// sum of the EUCLIDEAN CHORD lengths between every pair of onsets. The maximally
// even k-onset set (the regular k-gon) provably MAXIMISES this sum, so dividing
// the timeline's chord-sum by that ideal gives a value in (0,1], =1 only when the
// onsets are perfectly even. (Sum of geodesic ARC lengths is degenerate for
// near-even sets — many configurations tie — so the chord sum is the right metric.)
static double tl_evenness(const int *g, int per) {
    int on[32], k = tl_onsets(g, per, on);
    if (k < 2) return 1.0;
    double sum = 0;
    for (int a = 0; a < k; a++) for (int b = a + 1; b < k; b++) {
        double ang = TAU * (on[a] - on[b]) / per;
        sum += 2.0 * fabs(sin(ang / 2.0));               // chord length on unit circle
    }
    // ideal: k onsets evenly spaced at angular step 2π/k (the regular k-gon).
    double ideal = 0;
    for (int a = 0; a < k; a++) for (int b = a + 1; b < k; b++) {
        double ang = TAU * (a - b) / k;
        ideal += 2.0 * fabs(sin(ang / 2.0));
    }
    return ideal > 0 ? sum / ideal : 1.0;
}

static void tl_report(const char *name, const int *g, int per) {
    int on[32], k = tl_onsets(g, per, on);
    fprintf(stderr, "#   %-12s [", name);
    for (int i = 0; i < k; i++) fprintf(stderr, "%d%s", on[i], i + 1 < k ? "," : "");
    fprintf(stderr, "] (k=%d/%d)  oddity=%-3s  evenness=%.3f  offbeat=%d\n",
            k, per, tl_oddity(g, per) ? "yes" : "NO", tl_evenness(g, per), tl_offbeatness(g, per));
}

// at 138 BPM a bar is ~1.739s; ~68 bars ≈ 118s. SECBARS drives the length.
static const char *ORDER[8] = { "intro", "build", "grvA", "dub1", "grvB", "dub2", "grvC", "outro" };
static const int SECBARS[8] = { 8, 4, 12, 8, 12, 4, 12, 8 };
// ── THE TIMELINE EXPERIMENT ──────────────────────────────────────────────────
// Each section nominates a master 16-grid timeline for the chord stab + clave.
// Successive sections SWAP the timeline so the listener hears the same harmony
// re-cut against son, rumba, and the maximally-even Euclidean E(5,16) bell —
// the heart of the thesis. 0=son(3-2/2-3 alternation), 1=rumba, 2=Euclid E(5,16).
enum { TL_SON = 0, TL_RUMBA = 1, TL_EUCLID = 2 };
static const int SEC_TL[8] = {
    /*intro*/ TL_SON,   /*build*/ TL_SON,
    /*grvA */ TL_SON,   /*dub1 */ TL_RUMBA,   // experiment A: son vs rumba
    /*grvB */ TL_RUMBA, /*dub2 */ TL_EUCLID,  // experiment B: rumba vs maximally even
    /*grvC */ TL_EUCLID,/*outro*/ TL_SON,     // return to the son
};
static const char *TL_NAME[3] = { "son", "rumba", "euclid" };
static int MASK[8] = {
    /*intro*/ L_KICK | L_CHAT | L_PAD | L_CLAVE,
    /*build*/ L_KICK | L_CHAT | L_SUB | L_PAD | L_CLAVE | L_BELL,
    /*grvA */ L_KICK | L_CHAT | L_OHAT | L_RIM | L_SUB | L_STAB | L_PAD | L_CLAVE | L_BELL,
    /*dub1 */ L_STAB | L_PAD | L_REESE | L_CHAT | L_CLAVE | L_CBASS | L_BELL,
    /*grvB */ L_KICK | L_CHAT | L_OHAT | L_RIM | L_SUB | L_STAB | L_PAD | L_REESE | L_CLAVE | L_BELL | L_CBASS,
    /*dub2 */ L_STAB | L_PAD | L_REESE | L_CLAVE | L_CBASS,
    /*grvC */ L_KICK | L_CHAT | L_OHAT | L_RIM | L_SUB | L_STAB | L_PAD | L_REESE | L_CLAVE | L_BELL | L_CBASS,
    /*outro*/ L_KICK | L_SUB | L_PAD | L_STAB | L_CLAVE,
};
static int START[8];

// swung absolute time of (bar, 16th-step).
static double step_t(int bar, int step) {
    double base = bar * BAR + (step / 2) * (BEAT / 2.0);
    if (step & 1) base += SWING * (BEAT / 2.0); else base += 0.0;
    return base;
}

// dark C-minor dub harmony: roots walk i–VI–iv–VII per 4 bars (Cm–Ab–Fm–Bb).
static const int ROOTS[4] = { 24, 20, 17, 22 };          // C1, Ab0, F0, Bb0
// the chord stab voices each root as a low minor/major triad (dub chord).
static const int STAB_CH[4][3] = {
    { 48, 51, 55 },   // Cm   (C4 Eb4 G4)
    { 44, 48, 51 },   // Ab   (Ab3 C4 Eb4)
    { 41, 44, 48 },   // Fm   (F3 Ab3 C4)
    { 46, 50, 53 },   // Bb   (Bb3 D4 F4)
};
static const int PAD_CH[4][3] = { {36,39,43}, {32,36,39}, {29,32,36}, {34,38,41} }; // Cm Ab Fm Bb (low)

int main(int argc, char **argv) {
    const char *out_path = "out/dubtek-raw.wav";
    for (int i = 1; i < argc; i++) { if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i]; else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]); else if (!strcmp(argv[i], "--swing") && i + 1 < argc) SWING = atof(argv[++i]); }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = 0; for (int i = 0; i < 8; i++) TB += SECBARS[i];
    double totalSec = TB * BAR + 3.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); stabL = calloc(N, 4); stabR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# dubtek.c · %g BPM · %d bars · %.1fs · dub techno\n", BPMV, TB, totalSec);
    fprintf(stderr, "# form:"); for (int i = 0; i < 8; i++) fprintf(stderr, " %s(%d,%s)", ORDER[i], SECBARS[i], TL_NAME[SEC_TL[i]]); fprintf(stderr, "\n");

    // ── TIMELINE ANALYSIS (Arom oddity · Toussaint evenness/off-beatness) ──────
    // Computed here so the thesis can cite the real numbers this build emits.
    fprintf(stderr, "# timeline analysis  (name [onsets] (k/period)  oddity  evenness  off-beatness)\n");
    tl_report("son 3-2",  CLAVE_32,     16);
    tl_report("son 2-3",  CLAVE_23,     16);
    tl_report("rumba 3-2", RUMBA_32,    16);
    tl_report("euclid",   EUCLID_5_16,  16);
    tl_report("bembe",    BEMBE_12,     12);
    {
        // pairwise Hamming distance between the son and rumba claves on the 16-grid
        // (how many pulses they disagree on) — the size of the swap the ear hears.
        int diff = 0; for (int p = 0; p < 16; p++) diff += (CLAVE_32[p] != RUMBA_32[p]);
        // distance from son to the maximally-even E(5,16) ideal.
        int dse = 0; for (int p = 0; p < 16; p++) dse += (CLAVE_32[p] != EUCLID_5_16[p]);
        fprintf(stderr, "#   son↔rumba Hamming=%d pulses · son↔euclid Hamming=%d pulses\n", diff, dse);
        // count how many of the five candidate timelines satisfy rhythmic oddity.
        int passes = tl_oddity(CLAVE_32,16) + tl_oddity(CLAVE_23,16) + tl_oddity(RUMBA_32,16)
                   + tl_oddity(EUCLID_5_16,16) + tl_oddity(BEMBE_12,12);
        fprintf(stderr, "#   rhythmic oddity: %d/5 candidate timelines pass\n", passes);
    }

    // 16-step patterns ('x' = hit, '.' = rest).
    const char *P_CHAT = "..x...x...x...x.";   // dusty closed hats on the offbeat 8ths
    const char *P_OHAT = "......x.......x.";   // open hat splash, sparse
    const char *P_SUB  = "x.....x...x.....";   // deep, spacious sub — leaves room

    for (int s = 0; s < 8; s++) {
        int m = MASK[s], nb = SECBARS[s], c = START[s];
        for (int b = 0; b < nb; b++) {
            int bar = c + b, phr = b % 4; int root = ROOTS[phr % 4];
            int fill = (b % 4 == 3);                  // last bar of each 4-bar phrase
            // MASTER TIMELINE for this section (the swap experiment). For the son
            // we still alternate 3-2 / 2-3 every 2 bars (the clave breathes); the
            // rumba and Euclid timelines are stated whole so their differing
            // syncopation reads clearly against the same harmony.
            const int *CLAVE;
            switch (SEC_TL[s]) {
                case TL_RUMBA:  CLAVE = RUMBA_32; break;
                case TL_EUCLID: CLAVE = EUCLID_5_16; break;
                default:        CLAVE = ((b / 2) & 1) ? CLAVE_23 : CLAVE_32; break;
            }
            for (int st = 0; st < 16; st++) {
                double t = step_t(bar, st);
                // kick — four-on-the-floor, but shuffled: drop step-8 kick on odd
                // bars for a broken dub gap, and skip the last kick on fills.
                int kk = (st % 4 == 0);
                if (st == 8 && (b & 1)) kk = 0;        // broken bar 2 of each pair
                if (fill && st == 12) kk = 0;          // gap before the turnaround
                if ((m & L_KICK) && kk) kick(t, 0.96);
                // hats — wispy
                if ((m & L_CHAT) && P_CHAT[st] == 'x') hat(t, 0.45, 0, rnd2() * 0.3);
                if ((m & L_CHAT) && st % 2 == 1 && rnd() < 0.10) hat(t, 0.18, 0, rnd2() * 0.4); // sparse ghosts
                if ((m & L_OHAT) && P_OHAT[st] == 'x') hat(t, 0.40, 1, 0.18);
                // rim backbeat on 2 & 4
                if ((m & L_RIM) && (st == 4 || st == 12)) rim(t, 0.7);
                // deep sub bass
                if ((m & L_SUB) && P_SUB[st] == 'x') sub(root + 12, t, STEP * 2.2, 0.9);
                // rumbling reese undertow — one long note per bar, on the root
                if ((m & L_REESE) && st == 0) reese(root + 12, bar * BAR, BAR * 0.95, 0.30);

                // ── CLAVE-DRIVEN STABS: the dub chord stab now hangs on the son
                // clave timeline (not a flat off-beat grid). Each clave onset on
                // the syncopated 3-side gets the full chord; the 2-side answers
                // a touch darker. This is the master TIMELINE for the groove.
                if ((m & L_STAB) && CLAVE[st]) {
                    const int *ch = STAB_CH[phr % 4];
                    int twoSide = (st >= 8);            // 2-side = answer half
                    double cut = (1400 + 600 * (s >= 4)) - (twoSide ? 350 : 0);
                    double pan = (twoSide ? -0.45 : 0.45) + rnd2() * 0.15;
                    stab(ch, 3, t, STEP * 1.4, twoSide ? 0.30 : 0.36, cut, pan);
                }
                // the clave woodblock itself — the audible timeline tick
                if ((m & L_CLAVE) && CLAVE[st]) clave(t, 0.55, (st >= 8) ? 0.5 : -0.4);

                // ── COUNTER-BASS CALL-AND-RESPONSE: bass answers in the GAPS of
                // the clave's 3-side. When the clave stabs on its syncopated half,
                // the counter-bass replies on the 2-side beats (8 & 12) — the
                // call (stab, 3-side) / response (bass, 2-side) of the timeline.
                if ((m & L_CBASS) && (st == 8 || st == 12)) {
                    int cn = STAB_CH[phr % 4][st == 8 ? 0 : 2] - 12; // root / 5th, an octave down
                    sub(cn, t, STEP * 1.8, 0.55);
                }
            }

            // ── BEMBÉ BELL TIMELINE (E(7,12), triplet feel across the 4 beats) ──
            // laid on its own 12-pulse grid so it rides over the 16-grid groove as
            // crossing triplets (Toussaint: the standard bembé bell == E(7,12)).
            if (m & L_BELL) {
                for (int p = 0; p < 12; p++) {
                    if (!BEMBE_12[p]) continue;
                    double bt = bar * BAR + (double)p / 12.0 * BAR; // even triplet grid
                    double pan = (p & 1) ? 0.55 : -0.45;
                    double g = (p == 0 ? 0.42 : 0.26) * (s >= 4 ? 1.0 : 0.8);
                    bell(bt, g, pan);
                }
            }

            // pad chord — one per 4-bar phrase, sustained, smoky
            if ((m & L_PAD) && phr == 0) { const int *ch = PAD_CH[(b / 4) % 4]; for (int z = 0; z < 3; z++) pad(ch[z] + 12, bar * BAR, BAR * 4 - 0.05, 0.085, (z - 1) * 0.4); }
            // tom fill on the turnaround bar of a full groove section — descending
            // run on the clave's 2-side, pulling into the next phrase.
            if (fill && (m & L_RIM) && (s == 2 || s == 4 || s == 6)) {
                double f0[4] = { 150, 124, 98, 82 };
                int fs[4] = { 9, 11, 13, 15 };
                for (int k = 0; k < 4; k++) tom(step_t(bar, fs[k]), 0.7, f0[k], (k & 1) ? 0.5 : -0.5);
            }
            // a riser in the last bar before a groove section lifts the energy
            if (fill && (s == 1 || s == 3 || s == 5)) riser(bar * BAR, BAR, 0.14);
            // impact at the head of each groove/dub section (drop weight)
            if (b == 0 && (s == 2 || s == 3 || s == 4 || s == 6)) impact(bar * BAR, 0.5);
        }
    }

    // ── feedback delay on the stab tap (tempo-synced dotted-8th echo) ──
    // dub's signature: each chord stab repeats and decays, smeared by a lowpass
    // in the feedback path so the echoes get progressively darker. The delayed
    // signal is folded back into the music bus AND fed to the verb send.
    {
        long dly = (long)(BEAT * 0.75 * SR);          // dotted-8th
        double fb = 0.62, wet = 0.85, damp = 0.5;
        float *dlL = calloc(N, 4), *dlR = calloc(N, 4); double lpL = 0, lpR = 0;
        for (long i = 0; i < N; i++) {
            double inL = stabL[i], inR = stabR[i];
            double dL = (i >= dly) ? dlL[i - dly] : 0.0;
            double dR = (i >= dly) ? dlR[i - dly] : 0.0;
            lpL = dL * (1 - damp) + lpL * damp; lpR = dR * (1 - damp) + lpR * damp;
            dlL[i] = (float)(inL + lpL * fb);
            dlR[i] = (float)(inR + lpR * fb);
            // the echoes (not the dry stab — that's already in musL) into the mix + verb
            musL[i] += (float)(dL * wet); musR[i] += (float)(dR * wet);
            addR(i, dL * 0.4, dR * 0.4);
        }
        free(dlL); free(dlR);
    }

    // ── sidechain: deep kick-triggered duck on the music bus (the breath) ──
    { double depth = 0.78, rel = exp(-1.0 / (0.16 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── long, dark Schroeder reverb on the send (the dub chamber) ──
    {
        double decay = 0.86, wet = 0.42, damp = 0.6;
        int CD[6]; double cds[6] = { 0.0297, 0.0371, 0.0411, 0.0437, 0.0531, 0.0617 };
        for (int k = 0; k < 6; k++) CD[k] = (int)(cds[k] * SR);
        float *cbL[6], *cbR[6]; int ciL[6] = {0}, ciR[6] = {0}; double lpL[6] = {0}, lpR[6] = {0};
        for (int k = 0; k < 6; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 6; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                lpL[k] = dL * (1 - damp) + lpL[k] * damp; lpR[k] = dR * (1 - damp) + lpR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + lpL[k] * decay); cbR[k][ciR[k]] = (float)(inR + lpR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            musL[i] += (float)(cL / 6 * wet); musR[i] += (float)(cR / 6 * wet);
        }
        for (int k = 0; k < 6; k++) { free(cbL[k]); free(cbR[k]); }
    }

    // ── mix buses, normalize, short fades ──
    float *busL = drumL, *busR = drumR;             // reuse drum bus as the mix
    for (long i = 0; i < N; i++) { busL[i] += musL[i]; busR[i] += musR[i]; }
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.89 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(0.6 * SR), fout = (long)(2.2 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
