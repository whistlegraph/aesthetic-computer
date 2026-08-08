// bracelet.c — a rhythm you hear as a place.
//
// The cycle is a ring of sound sources around the listener's head: step i of an
// n-pulse cycle sits at azimuth 2*pi*i/n, step 0 dead ahead, in the horizontal
// plane. Two equivalences that are asserted on paper and never heard become
// physical operations:
//
//   necklace (rotation)   -> the sound field spins. Precession in time and
//                            rotation in space become the SAME operation.
//   bracelet (reflection) -> a left/right mirror across the median plane.
//
// The horizontal plane is not decoration. Left/right is the best-resolved axis
// humans have (interaural time and level differences); front/back and elevation
// sit inside the cone of confusion. Reflecting about the frontal plane instead
// would put the whole experiment in the ambiguous axis and prove nothing.
//
// The catch the platter turned up: most canonical timelines are ACHIRAL. son,
// bossa, shiko, tresillo, cinquillo and bembe are each their own reflection up
// to rotation, so mirroring them only turns the ring. Only rumba, soukous and
// gahu are chiral. And the ring must also carry its centroid OFF the mirror
// axis, or the flip changes the pattern without moving the field: soukous sits
// at 180 deg, dead on the axis. gahu is chiral, off-axis, and frontal.
//
// Rhythm theory: pop/lib/c/ac_necklace.h   (spec: papers/rhythm-platter/)
// Binaural:     pop/nullabye/c/ac_hrtf.h
//
// Build:  cc -O3 -std=c11 -o bracelet bracelet.c -lm
// Run:    ./bracelet --out out/bracelet-raw.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "../../lib/c/ac_necklace.h"
#include "../../nullabye/c/ac_hrtf.h"
#include "../../bell/c/bell.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static const int STEPS = 16;              // pulses per cycle; one bar = one cycle
static double BPMV = 138, BEAT, BAR, STEP;
static double RADIUS = 2.0;               // ring radius in room units
static double LIFT = M_PI / 8;            // +22.5 deg for off-beat pulses

static uint32_t rng_s = 0x62726163;       // "brac"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// Two buses. The ring is already binaural by the time it lands in spL/spR, so it
// must never be re-panned. The centre bus is dry and mono-anchored: the kick and
// sub live below ~150 Hz where localisation is weak anyway, so placing them in
// the ring would be a claim the ear cannot check.
static float *ctrL, *ctrR, *spL, *spR, *revL, *revR;
static inline void addC(long i, double l, double r) { if (i >= 0 && i < N) { ctrL[i] += (float)l; ctrR[i] += (float)r; } }
static inline void addS(long i, double l, double r) { if (i >= 0 && i < N) { spL[i] += (float)l; spR[i] += (float)r; } }
static inline void addV(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = (uint32_t)SR, br = (uint32_t)SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── humanization (a SEPARATE layer, on purpose) ─────────────────────────────
// papers/rhythm-platter/digest/07: the necklace decides WHICH PULSE, an offset
// table decides where inside the beat bin. They must not be merged — every
// measured property in this engine is a property of the quantised necklace, and
// folding timing jitter into onset positions would silently invalidate all of
// them. So the printed metrics describe the score; these offsets describe the
// performance of it.
//
// Deterministic: seeded by (bar, step, layer), so a render is reproducible.
static inline uint32_t hash3(int a, int b, int c) {
    uint32_t h = (uint32_t)(a * 73856093) ^ (uint32_t)(b * 19349663) ^ (uint32_t)(c * 83492791);
    h ^= h >> 13; h *= 0x5bd1e995; h ^= h >> 15;
    return h;
}
static inline double jitter(int bar, int step, int layer) { return (double)hash3(bar, step, layer) / 4294967296.0 * 2.0 - 1.0; }

// Per-layer feel. Real ensembles do not agree to the sample: the low voice sits
// behind the beat, the grain leans ahead of it. That disagreement is most of
// what "humanised" actually means — more than random jitter is.
static const double LAYER_PUSH[5] = { 0.000, +0.011, -0.004, -0.006, +0.003 };

static double humanize(double t, int bar, int step, int layer) {
    double j = jitter(bar, step, layer) * 0.0075;      // +/- 7.5 ms
    double swing = (step % 2) ? 0.006 : 0.0;           // light 16th swing
    return t + LAYER_PUSH[layer] + j + swing;
}

// ── spatial voices ──────────────────────────────────────────────────────────
// Every ring voice carries a broadband transient. This is load-bearing, not
// garnish: HRTF cues live in spectral notches and interaural timing, and a pure
// tone gives the ear almost nothing to localise. The chiff makes the bead a place.

static BellModes MD_BRONZE, MD_GLASS, MD_STONE;
static BellModes SCRATCH;
#define BELL_CAP (48000 * 2)
static float BL[BELL_CAP], BR[BELL_CAP];

// fem_bell — a physically modelled bell struck at a point in space.
//
// The FEM engine (pop/bell/c/bell.c) solves the mode set once at startup; each
// strike retunes a copy so the inharmonic ratios are preserved, renders, then
// runs the whole ring through its own HRTF state so the tail stays placed.
//
// dur is per-onset and short strikes are CHOKED, so the tail gets an ~80 ms
// fade — a hard-truncated modal ring clicks.
static void fem_bell(const BellModes *base, double note, double t, double vel,
                     double dur, double g, double az, double el, double dist) {
    long s0 = (long)(t * SR);
    long cap = (long)(dur * SR); if (cap > BELL_CAP) cap = BELL_CAP;
    memcpy(&SCRATCH, base, sizeof(BellModes));
    bell_retune(&SCRATCH, midi_hz(note));
    long w = bell_render(&SCRATCH, vel, (double)SR, dur, BL, BR, cap);
    if (w <= 0) return;

    long fade = (long)(0.080 * SR); if (fade > w) fade = w;
    ACHrtf h; memset(&h, 0, sizeof h);
    double prev = 0;
    for (long i = 0; i < w; i++) {
        double v = 0.5 * ((double)BL[i] + (double)BR[i]);   // FEM is stereo; HRTF wants mono
        // A short noise chiff on the attack. The modal body alone is nearly
        // tonal, and tonal is exactly what does not localise.
        if (i < (long)(0.012 * SR)) {
            double nz = rnd2(), hp = nz - prev; prev = nz;
            v += hp * exp(-(double)i / SR * 300.0) * 0.30 * vel;
        }
        if (i > w - fade) v *= (double)(w - i) / fade;      // no click on choked strikes
        v *= g;
        float l, r; ac_hrtf_process(&h, (float)v, az, el, dist, &l, &r);
        addS(s0 + i, l, r);
        addV(s0 + i, l * 0.16, r * 0.16);
    }
}

// tick — a dry filtered click for the interlocking second ring.
static void tick(double t, double g, double az, double el, double dist) {
    long s0 = (long)(t * SR), n = (long)(0.09 * SR);
    double prev = 0, lp = 0;
    ACHrtf h; memset(&h, 0, sizeof h);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hpv = nz - prev; prev = nz;
        lp = 0.55 * lp + 0.45 * hpv;
        double v = lp * exp(-tt * 90.0) * g;
        float l, r; ac_hrtf_process(&h, (float)v, az, el, dist, &l, &r);
        addS(s0 + i, l, r);
        addV(s0 + i, l * 0.10, r * 0.10);
    }
}

// grain — fine noise shaker on its own fast necklace. Sparse points around a
// head read as pointillistic; the grain is what makes the ring feel continuous.
static void grain(double t, double g, double az, double el, double dist) {
    long s0 = (long)(t * SR), n = (long)(0.055 * SR);
    double prev = 0;
    ACHrtf h; memset(&h, 0, sizeof h);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        double hp = nz - prev; prev = nz;
        double v = hp * exp(-tt * 150.0) * g;
        float l, r; ac_hrtf_process(&h, (float)v, az, el, dist, &l, &r);
        addS(s0 + i, l, r);
    }
}

// ── centre voices (never spatialised) ───────────────────────────────────────

static void kick(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(0.32 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 46 + 74 * exp(-tt * 44.0);
        ph += TAU * pf / SR;
        double v = tanh((sin(ph) + exp(-tt * 400.0) * 0.7) * 2.0) * exp(-tt * 9.0) * g;
        addC(s0 + i, v, v);
    }
}

static void sub(double note, double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double ph = 0;
    double f = midi_hz(note);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = fmin(1.0, tt * 90.0) * exp(-tt * 1.2);
        ph += TAU * f / SR;
        double v = tanh(sin(ph) * 1.5) * env * g;
        addC(s0 + i, v, v);
    }
}

// pad — a slow bed so the ring has something to sit in. Slightly detuned pair
// spread just off centre; it is a floor, not a position.
static void pad(double note, double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR);
    double f = midi_hz(note), pa = 0, pb = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = fmin(1.0, tt * 1.6) * fmin(1.0, (dur - tt) * 1.6);
        if (env < 0) env = 0;
        pa += TAU * f / SR; pb += TAU * f * 1.004 / SR;
        double raw = sin(pa) + sin(pb) * 0.8 + sin(pa * 0.5) * 0.3;
        lp = 0.92 * lp + 0.08 * raw;
        double v = lp * env * g;
        addC(s0 + i, v * 0.85, v * 1.0);
    }
}

// ── the ring ────────────────────────────────────────────────────────────────

// Azimuth of a pulse. Step 0 is dead ahead; the ring advances clockwise.
static double azimuth_of(int step, int n, int mirror) {
    double az = TAU * step / n;
    if (mirror) az = -az;                            // median-plane reflection
    while (az > M_PI) az -= TAU;
    while (az < -M_PI) az += TAU;
    return az;
}

// Elevation carries off-beatness: generator pulses (gcd(p,n)==1) lift above the
// plane. That is the one measure rotation changes, so it is the axis that
// visibly MOVES as the necklace precesses.
static double elevation_of(int step, int n) {
    return (ac_gcd(step, n) == 1) ? LIFT : 0.0;
}

// ── form ────────────────────────────────────────────────────────────────────
// Six sections. Each names the one thing it is for.

typedef struct {
    const char *name;
    int bars;
    const char *timeline;   // which named ring
    int prec;               // step-rotations per 4-bar phrase (0 = frozen)
    int mirror;             // 0 none, 1 alternate 2 bars normal / 2 mirrored
    int ring2;              // interlocking second ring (the complement)
    int layers;             // LAY_* bitmask: which rings sound
    const char *what;
} Section;

// Each layer is its OWN necklace on its OWN rotation weight, so the composite
// never repeats the same way twice — the hypnotek.c precession idea, but the
// lanes now drift in space as well as in time.
#define LAY_BELL    1   // the section timeline
#define LAY_LOW     2   // E(3,16), two octaves down, wide and behind the beat
#define LAY_SHIM    4   // E(2,16), glass, high and quiet, far out
#define LAY_GRAIN   8   // E(9,16), noise grain, fills the ring in

static const Section FORM[] = {
    { "still",  8, "bossa",   0, 0, 0, LAY_BELL,                                 "state the polygon: maximally even, perfectly placed, not turning" },
    { "turn",  12, "bossa",   1, 0, 0, LAY_BELL|LAY_LOW,                         "rotation = the room spins. precession in time IS motion in space" },
    { "mirror", 8, "gahu",    0, 1, 0, LAY_BELL|LAY_LOW|LAY_SHIM,                "the bracelet A/B. gahu is chiral AND its centroid is frontal + off-axis" },
    { "weave", 12, "soukous", 2, 0, 1, LAY_BELL|LAY_LOW|LAY_SHIM|LAY_GRAIN,      "second ring = the complement. interlocking, so no two beads share an azimuth" },
    { "morph", 13, "clump",   0, 0, 0, LAY_BELL|LAY_LOW|LAY_GRAIN,               "swap geodesic from a lopsided clump to the regular pentagon, one move per bar" },
    { "land",   8, "bossa",   0, 0, 0, LAY_BELL|LAY_LOW|LAY_SHIM|LAY_GRAIN, "rotation stops on the balanced set. the centroid resolves to nothing" },
};
static const int NSEC = (int)(sizeof(FORM) / sizeof(FORM[0]));

// Named timelines, matching papers/rhythm-platter/timelines.json.
static void timeline_of(const char *name, int *out) {
    for (int i = 0; i < STEPS; i++) out[i] = 0;
    if (!strcmp(name, "bossa"))        { int o[] = {0,3,6,10,13};  for (int i=0;i<5;i++) out[o[i]]=1; }
    else if (!strcmp(name, "son"))     { int o[] = {0,3,6,10,12};  for (int i=0;i<5;i++) out[o[i]]=1; }
    else if (!strcmp(name, "rumba"))   { int o[] = {0,3,7,10,12};  for (int i=0;i<5;i++) out[o[i]]=1; }
    else if (!strcmp(name, "soukous")) { int o[] = {0,3,6,10,11};  for (int i=0;i<5;i++) out[o[i]]=1; }
    else if (!strcmp(name, "gahu"))    { int o[] = {0,3,6,10,14};  for (int i=0;i<5;i++) out[o[i]]=1; }
    // Not a timeline — the morph's starting point. Five onsets crammed into the
    // first half: gaps (2,2,2,2,8), a badly lopsided pentagon. Spatially it is a
    // clump in the front-right quadrant with the whole left side silent.
    else if (!strcmp(name, "clump"))   { int o[] = {0,2,4,6,8};   for (int i=0;i<5;i++) out[o[i]]=1; }
}

// One step of the swap geodesic: move a single onset one pulse toward its
// target, into a free cell. Mirrors morphPath() in pop/lib/necklace.mjs.
static int morph_step(int *cur, const int *target, int n) {
    int oc[AC_NECKLACE_MAX], ot[AC_NECKLACE_MAX];
    int kc = ac_onsets(cur, n, oc), kt = ac_onsets(target, n, ot);
    if (kc != kt) return 0;
    for (int i = 0; i < kc; i++) {
        if (oc[i] == ot[i]) continue;
        int dir = (ot[i] > oc[i]) ? 1 : -1;
        int next = oc[i] + dir;
        if (next < 0 || next >= n || cur[next]) continue;
        cur[oc[i]] = 0; cur[next] = 1;
        return 1;
    }
    return 0;
}

// A minor pentatonic confined to E4-E5. The standing rule for the FEM bell is
// that melodic runs top out around E5 — above that it reads tangy on laptop
// speakers. The 6th octave is reserved for the quiet shimmer layer only.
static const double SCALE[]  = { 64, 67, 69, 72, 74, 76 };            // E4 G4 A4 C5 D5 E5
static const int NSCALE = (int)(sizeof(SCALE) / sizeof(SCALE[0]));
static const double LOWSCALE[] = { 40, 45, 47, 52 };                  // E2 A2 B2 E3
static const int NLOW = (int)(sizeof(LOWSCALE) / sizeof(LOWSCALE[0]));
static const double SHIMSCALE[] = { 88, 91, 93 };                     // E6 G6 A6 — sparkle, low gain

// Decay length carries the GAP the onset opens. A long gap rings long, a short
// gap is choked. So the two-gap structure of a maximally even rhythm becomes
// audible as a decay contour, and the lopsided clump in the morph sounds
// lopsided as well as looking it.
static double dur_for_gap(int gap) {
    double d = gap * STEP * 2.4;
    if (d < 0.22) d = 0.22;
    if (d > 1.70) d = 1.70;
    return d;
}

int main(int argc, char **argv) {
    const char *out = "out/bracelet-raw.wav";
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) out = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]);
    }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; STEP = BAR / STEPS;

    int totalBars = 0;
    for (int s = 0; s < NSEC; s++) totalBars += FORM[s].bars;
    double dur = totalBars * BAR + 3.0;
    N = (long)(dur * SR);

    ctrL = calloc(N, sizeof(float)); ctrR = calloc(N, sizeof(float));
    spL  = calloc(N, sizeof(float)); spR  = calloc(N, sizeof(float));
    revL = calloc(N, sizeof(float)); revR = calloc(N, sizeof(float));
    if (!ctrL || !ctrR || !spL || !spR || !revL || !revR) { fprintf(stderr, "oom\n"); return 1; }

    // ── solve the bells once ────────────────────────────────────────────────
    // The eigensolve is the expensive part (~7 s per material), so it happens
    // once here and every strike retunes a copy: a uniform geometric scale
    // shifts all modes together, so the inharmonic ratio set is preserved.
    {
        BellGeometry g; BellMaterial m;
        fprintf(stderr, "# solving FEM bell modes (bronze)...\n");
        bell_default_geometry(&g); bell_default_material(&m);
        if (bell_solve_modes(&g, &m, 6, 32, &MD_BRONZE) <= 0) { fprintf(stderr, "bell solve failed\n"); return 1; }
        fprintf(stderr, "#   bronze: %d modes, strike %.1f Hz\n", MD_BRONZE.count, MD_BRONZE.strike_freq);
        // NOTE: bell_material_preset returns 0 on SUCCESS, -1 on failure.
        // Testing it as a boolean silently falls back to bronze for every layer.
        fprintf(stderr, "# solving FEM bell modes (glass)...\n");
        bell_default_geometry(&g);
        if (bell_material_preset(&m, "glass") != 0) bell_default_material(&m);
        if (bell_solve_modes(&g, &m, 6, 32, &MD_GLASS) <= 0) { fprintf(stderr, "bell solve failed\n"); return 1; }
        fprintf(stderr, "#   glass:  %d modes, strike %.1f Hz, loss %.1e (long clear ring)\n",
                MD_GLASS.count, MD_GLASS.strike_freq, m.loss);

        // Three materials = three decay characters, from the physics rather than
        // from an envelope: bronze medium, glass very long, stone short and
        // clinking. Stone carries the low ring — a short broadband thunk also
        // localises far better than a long low tone.
        fprintf(stderr, "# solving FEM bell modes (stone)...\n");
        bell_default_geometry(&g);
        if (bell_material_preset(&m, "stone") != 0) bell_default_material(&m);
        if (bell_solve_modes(&g, &m, 6, 32, &MD_STONE) <= 0) { fprintf(stderr, "bell solve failed\n"); return 1; }
        fprintf(stderr, "#   stone:  %d modes, strike %.1f Hz, loss %.1e (short clink)\n",
                MD_STONE.count, MD_STONE.strike_freq, m.loss);
    }

    // ── the claims, measured and printed ────────────────────────────────────
    fprintf(stderr, "# bracelet.c · %.0f BPM · %d bars · %.1fs · spatial necklaces (A minor)\n",
            BPMV, totalBars, totalBars * BAR);
    fprintf(stderr, "# form:");
    for (int s = 0; s < NSEC; s++) fprintf(stderr, " %s(%d)", FORM[s].name, FORM[s].bars);
    fprintf(stderr, "\n#\n# --- the mapping ---\n");
    fprintf(stderr, "#   step spacing %.2f deg  ·  step %.1f ms  ·  cycle %.2f s\n",
            360.0 / STEPS, STEP * 1000.0, BAR);
    fprintf(stderr, "#   Mills lateral MAA ~10 deg: %s. precedence window ~5 ms: %s.\n",
            (360.0 / STEPS >= 20.0) ? "resolvable" : "MARGINAL",
            (STEP * 1000.0 >= 5.0) ? "clear" : "FUSING");
    fprintf(stderr, "#\n# --- chirality: which rings can actually be mirrored ---\n");
    {
        const char *names[] = { "bossa", "son", "rumba", "soukous", "gahu" };
        for (int i = 0; i < 5; i++) {
            int g[AC_NECKLACE_MAX], rf[AC_NECKLACE_MAX];
            timeline_of(names[i], g);
            ac_reflect(g, STEPS, 0, rf);
            int chiral = !ac_same_necklace(g, rf, STEPS);
            ACBalance b = ac_balance(g, STEPS);
            fprintf(stderr, "#   %-8s %-9s  evenness=%.3f  offbeat=%d  balance=%.3f%s\n",
                    names[i], chiral ? "CHIRAL" : "achiral",
                    ac_evenness(g, STEPS), ac_offbeatness(g, STEPS), b.magnitude,
                    b.balanced ? " (no direction)" : "");
        }
        fprintf(stderr, "#   -> mirror needs CHIRAL + an off-axis centroid; gahu is the only ring that is both.\n");
    }

    // ── arrangement ─────────────────────────────────────────────────────────
    int bar = 0;
    int morphCur[AC_NECKLACE_MAX], morphTarget[AC_NECKLACE_MAX];
    timeline_of("clump", morphCur);
    timeline_of("bossa", morphTarget);

    for (int s = 0; s < NSEC; s++) {
        const Section *sec = &FORM[s];
        fprintf(stderr, "#\n# [%s] %s\n", sec->name, sec->what);

        for (int b = 0; b < sec->bars; b++, bar++) {
            double t0 = bar * BAR;

            // The ring for this bar.
            int base[AC_NECKLACE_MAX], ring[AC_NECKLACE_MAX];
            if (!strcmp(sec->name, "morph")) {
                if (b > 0) morph_step(morphCur, morphTarget, STEPS);
                memcpy(base, morphCur, sizeof(int) * STEPS);
            } else {
                timeline_of(sec->timeline, base);
            }

            int rot = b * sec->prec;   // per BAR: the room has to visibly turn
            if (rot) ac_rotate(base, STEPS, rot, ring); else memcpy(ring, base, sizeof(int) * STEPS);

            // Mirror alternates two bars on, two bars off, so the ear gets the
            // A and the B close enough together to compare.
            int mirror = sec->mirror ? ((b / 2) % 2) : 0;

            if (b % 4 == 0 || (sec->mirror && b % 2 == 0)) {
                ACBalance bal = ac_balance(ring, STEPS);
                char box[AC_NECKLACE_MAX + 1];
                for (int i = 0; i < STEPS; i++) box[i] = ring[i] ? 'x' : '.';
                box[STEPS] = '\0';
                fprintf(stderr, "#   bar %2d  %s  rot=%-2d %s  centroid %s%.1f deg\n",
                        bar, box, rot, mirror ? "MIRRORED" : "        ",
                        bal.balanced ? "none " : "", bal.balanced ? 0.0 : bal.angle * 180.0 / M_PI * (mirror ? -1 : 1));
            }

            // Cyclic gaps of the sounding ring — the decay contour comes from these.
            int gaps[AC_NECKLACE_MAX];
            ac_ioi(ring, STEPS, gaps);

            // ── layer 0: the principal ring ─────────────────────────────────
            if (sec->layers & LAY_BELL) {
                int idx = 0;
                for (int i = 0; i < STEPS; i++) {
                    if (!ring[i]) continue;
                    double t  = humanize(t0 + i * STEP, bar, i, 0);
                    double az = azimuth_of(i, STEPS, mirror);
                    double el = elevation_of(i, STEPS);
                    double note = SCALE[(idx + bar / 4) % NSCALE];
                    // Velocity: downbeat hardest, off-beat pulses softest, plus a
                    // human wobble. Attack character follows velocity in the FEM
                    // model, so this varies timbre and not just level.
                    double vel = (i == 0) ? 0.92 : (ac_gcd(i, STEPS) == 1 ? 0.55 : 0.72);
                    vel += jitter(bar, i, 10) * 0.13;
                    if (vel < 0.30) vel = 0.30; if (vel > 1.0) vel = 1.0;
                    fem_bell(&MD_BRONZE, note, t, vel, dur_for_gap(gaps[idx]),
                             (i == 0) ? 0.50 : 0.38, az, el, RADIUS);
                    idx++;
                }
            }

            // ── layer 1: the low ring, E(3,16), behind the beat ─────────────
            if (sec->layers & LAY_LOW) {
                int lr[AC_NECKLACE_MAX];
                ac_bjorklund(3, STEPS, -(b * (sec->prec ? sec->prec : 1)), lr);
                int lg[AC_NECKLACE_MAX]; ac_ioi(lr, STEPS, lg);
                int idx = 0;
                for (int i = 0; i < STEPS; i++) {
                    if (!lr[i]) continue;
                    double t = humanize(t0 + i * STEP, bar, i, 1);
                    double note = LOWSCALE[(idx + bar / 8) % NLOW];
                    double vel = 0.62 + jitter(bar, i, 11) * 0.12;
                    fem_bell(&MD_STONE, note, t, vel, dur_for_gap(lg[idx]) * 1.5, 0.40,
                             azimuth_of(i, STEPS, mirror), -LIFT * 0.55, RADIUS * 1.5);
                    idx++;
                }
            }

            // ── layer 2: the shimmer, E(2,16), glass, far and quiet ─────────
            if (sec->layers & LAY_SHIM) {
                int sr_[AC_NECKLACE_MAX];
                ac_bjorklund(2, STEPS, b * 3, sr_);
                int idx = 0;
                for (int i = 0; i < STEPS; i++) {
                    if (!sr_[i]) continue;
                    double t = humanize(t0 + i * STEP, bar, i, 2);
                    double note = SHIMSCALE[(idx + bar / 4) % 3];
                    double vel = 0.40 + jitter(bar, i, 12) * 0.14;
                    fem_bell(&MD_GLASS, note, t, vel, 1.1, 0.085,
                             azimuth_of(i, STEPS, mirror), LIFT * 1.6, RADIUS * 2.2);
                    idx++;
                }
            }

            // ── layer 3: the grain, E(9,16), ahead of the beat ──────────────
            if (sec->layers & LAY_GRAIN) {
                int gr[AC_NECKLACE_MAX];
                ac_bjorklund(9, STEPS, b * 2, gr);
                for (int i = 0; i < STEPS; i++) {
                    if (!gr[i]) continue;
                    double t = humanize(t0 + i * STEP, bar, i, 3);
                    double g = 0.13 + jitter(bar, i, 13) * 0.05;
                    grain(t, g, azimuth_of(i, STEPS, mirror), LIFT * 0.35, RADIUS * 1.15);
                }
            }

            // The interlocking second ring: the complement, so by construction
            // no two beads ever strike the same azimuth at the same instant.
            if (sec->ring2) {
                int comp[AC_NECKLACE_MAX];
                ac_complement(ring, STEPS, comp);
                for (int i = 0; i < STEPS; i++) {
                    if (!comp[i] || (i % 2)) continue;
                    double t = humanize(t0 + i * STEP, bar, i, 4);
                    tick(t, 0.26, azimuth_of(i, STEPS, !mirror), -LIFT * 0.5, RADIUS * 1.35);
                }
            }

            // Centre: the fixed floor everything else turns against.
            for (int q = 0; q < 4; q++) kick(t0 + q * BEAT, 0.58);
            if (strcmp(sec->name, "still")) sub(33 + ((bar / 8) % 2 ? 3 : 0), t0, BAR * 0.98, 0.26);
            if (bar % 4 == 0) {
                int root = 45 + ((bar / 8) % 2 ? 3 : 0);      // A2 / C3
                pad(root,      t0, BAR * 4.0, 0.075);
                pad(root + 12, t0, BAR * 4.0, 0.055);
                pad(root + 19, t0, BAR * 4.0, 0.040);          // fifth above
                pad(root + 15, t0, BAR * 4.0, 0.032);          // minor third
            }
        }
    }

    // ── reverb send (Schroeder combs + allpass) ─────────────────────────────
    {
        static const int CT[4] = { 1687, 1601, 2053, 2251 };
        static const double CG[4] = { 0.78, 0.76, 0.74, 0.72 };
        float *wl = calloc(N, sizeof(float)), *wr = calloc(N, sizeof(float));
        if (wl && wr) {
            for (int c = 0; c < 4; c++) {
                int d = CT[c], dr = CT[c] + 23;
                for (long i = 0; i < N; i++) {
                    double xl = revL[i] + (i >= d ? wl[i - d] * CG[c] : 0);
                    double xr = revR[i] + (i >= dr ? wr[i - dr] * CG[c] : 0);
                    wl[i] += (float)(xl * 0.25); wr[i] += (float)(xr * 0.25);
                }
            }
            int ap = 347;
            for (long i = 0; i < N; i++) {
                double al = wl[i] + (i >= ap ? wl[i - ap] * 0.5 : 0);
                double ar = wr[i] + (i >= ap ? wr[i - ap] * 0.5 : 0);
                wl[i] = (float)al; wr[i] = (float)ar;
            }
            for (long i = 0; i < N; i++) { spL[i] += wl[i] * 0.28f; spR[i] += wr[i] * 0.28f; }
        }
        free(wl); free(wr);
    }

    // ── mix, normalize, fade ────────────────────────────────────────────────
    float *L = calloc(N, sizeof(float)), *R = calloc(N, sizeof(float));
    if (!L || !R) { fprintf(stderr, "oom\n"); return 1; }
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double l = ctrL[i] + spL[i], r = ctrR[i] + spR[i];
        L[i] = (float)l; R[i] = (float)r;
        if (fabs(l) > peak) peak = fabs(l);
        if (fabs(r) > peak) peak = fabs(r);
    }
    double norm = peak > 1e-9 ? 0.89 / peak : 1.0;
    long fade = (long)(2.2 * SR);
    for (long i = 0; i < N; i++) {
        double g = norm;
        if (i < SR / 12) g *= (double)i / (SR / 12.0);
        if (i > N - fade) g *= (double)(N - i) / fade;
        L[i] = (float)(L[i] * g); R[i] = (float)(R[i] * g);
    }

    if (!write_wav_f32_stereo(out, L, R, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "#\n");
    fprintf(stderr, "✓ %s · %.1fs · headphones required (binaural)\n", out, (double)N / SR);
    return 0;
}
