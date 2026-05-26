// hellsine.c — incremental C port of pop/hellsine/bin/hellsine.mjs.
//
// THE LAW (amended 2026-05-22): every generated sample is a sum of sin()
// terms or a memoryless waveshaping (tanh) of such a sum. The same law
// holds here.
//
// Per-sample synthesis in the AC house style: every voice keeps a
// normalized phase 0..1 advanced by `frequency / sample_rate` each tick,
// mirroring `generate_sample()` in fedac/native/src/audio.c and the
// "Phase Increment" sine in system/.../lib/sound/synth.mjs.
//
// PHASE 1 (this file): scaffolding + voice() + bell() + sub() + Schroeder
// reverb + WAV writer + DUCK sidechain bus + --test mode for isolated
// voice rendering used by compare.mjs to A/B against the JS engine.
//
// Build:  ./build.sh
// Tests:  ./hellsine --test voice  --out out/voice.wav
//         ./hellsine --test bell   --out out/bell.wav
//         ./hellsine --test sub    --out out/sub.wav
//         ./hellsine --test all    --out out/test-all.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <pthread.h>
#include <dirent.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ── config ────────────────────────────────────────────────────────────
static const int SR = 48000;
static double BPM = 182.0;
static const char *SEED_STR = "hellsine";
static const char *OUT_PATH = NULL;
static const char *TEST_NAME = NULL;     // "voice", "bell", "sub", "all", NULL = full track

// ── deterministic RNG (xorshift32 keyed by FNV-1a) ────────────────────
static uint32_t xorshift_state = 0;
static uint32_t fnv1a(const char *s) {
    uint32_t h = 2166136261u;
    while (*s) { h ^= (unsigned char)*s++; h *= 16777619u; }
    return h ? h : 1;
}
static inline double rng(void) {
    uint32_t s = xorshift_state;
    s ^= s << 13;
    s ^= s >> 17;
    s ^= s << 5;
    xorshift_state = s;
    return (double)s / 4294967296.0;
}
// HUMANIZE_MULT — scales every per-note timing nudge. 1.0 = canonical,
// 1.7 = drunken/adventurous, 2.10 = grungey-swing (@jeffrey 2026-05-26
// "add more swing / more of a grungey beat").
static double HUMANIZE_MULT = 2.10;
// WET_MIX override — when >= 0, replaces the hardcoded 0.42 cathedral
// reverb mix at finalize. Used by --distrokid master to pull the wash
// out for "clean + crunchy" delivery. -1 = engine default (0.42).
static double WET_MIX_OVERRIDE = -1.0;
// EAGER_PERC_OFFSET — fires drum events ahead of the grid for a more
// rushed/forward feel. Default -0.014s (14ms early). Negative = ahead.
// (@jeffrey 2026-05-26 "more eager percussion / forward in timing /
// feels rushed").
static double EAGER_PERC_OFFSET = -0.014;
// SNARE_PAN_BIAS — applied to L/R inside snare_render. Reset to 0
// after each call site to avoid leaking pan to subsequent snares.
static double SNARE_PAN_BIAS = 0.0;
static inline double hum(double amt) { return (rng() * 2.0 - 1.0) * amt * HUMANIZE_MULT; }
static inline double m2f(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── shared mix buffers ────────────────────────────────────────────────
static long N = 0;
static float *L = NULL;
static float *R = NULL;
static float *WL = NULL;        // cathedral Schroeder wet send L
static float *WR = NULL;
static float *SL = NULL;        // SPATIAL RESONATOR wet send L (bright metallic slap)
static float *SR_ = NULL;       // SPATIAL RESONATOR wet send R
static float *DUCK = NULL;      // sidechain bus, written by kick/snare, read by sub/saw

static double HELL = 11.0;       // gabber drive ("hell knob")
static int    NOKICK = 0;
static int    ULTIMATE = 0;       // --ultimate flag (composed-through showcase)
// LEAD voice — default brass (sample + powersine harmonics + slide).
// `--lead powersine` swaps in a trance-style 7-voice detuned supersine
// stack with hard saturation. Both share the same THEME placement /
// gain / portamento logic, just different oscillator core.
typedef enum { LEAD_BRASS = 0, LEAD_POWERSINE = 1 } LeadKind;
static LeadKind LEAD_KIND = LEAD_BRASS;
static const char *RATTLE_MODE = "sparse";  // "off" | "sparse" | "drive"
static double RATTLE_GAIN = 0.5;
static double TAIL_SEC = 3.2;
static double SPBAR_G = 0.0;     // populated when main() picks BPM
static double SPB_G   = 0.0;
static double TOTAL_SEC_G = 0.0;

// Sampled electric guitar (Chem freesound 31933 — D2 power chord, 9.47s).
// Plus a HEAVIER stack-layer (Ax_Grinder freesound 242803 — drop-D Jackson
// → POD XT Live, 15s) for thickness. Both loaded once by render_full_track.
static float *electric_guitar_buf = NULL;
static long   electric_guitar_n = 0;
static float *electric_guitar_heavy_buf = NULL;
static long   electric_guitar_heavy_n = 0;
#define ELECTRIC_GUITAR_ROOT_MIDI 38.0   // D2

// Natural-brass sample (loaded once by render_full_track from Ableton
// Live 12 Suite Core Library: Flugelhorn A#). Used by lay_theme(brass=1)
// pitched per THEME note via playSample rate.
static float *brass_sample_buf = NULL;
static long  brass_sample_n = 0;
static const int BRASS_SAMPLE_MIDI = 58;       // Flugelhorn A# (A#3 ≈ MIDI 58)

// Section ranges (populated during render_full_track, consumed by post-arrangement samples).
typedef struct {
    const char *name;
    int startBar, endBar;
    double startSec, endSec;
} SectionRange;
static SectionRange section_ranges[8];     // (SECN is defined later — 6 sections, room for 8)
static int n_section_ranges = 0;

// Kick event list (populated during render_full_track, consumed by grenade kick layer).
#define MAX_KICK_EVENTS 4096
static double kick_events[MAX_KICK_EVENTS];
static int    n_kick_events = 0;
static inline void push_kick(double t) {
    if (n_kick_events < MAX_KICK_EVENTS) kick_events[n_kick_events++] = t;
}

// ── timing / reporting ────────────────────────────────────────────────
static double t0_wall = 0.0;
static double now_wall(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1e9;
}
__attribute__((format(printf, 1, 2)))
static void report(const char *fmt, ...) {
    fprintf(stderr, "[%6.2fs] ", now_wall() - t0_wall);
    va_list args; va_start(args, fmt);
    vfprintf(stderr, fmt, args); va_end(args);
    fputc('\n', stderr);
    fflush(stderr);
}

// ── voice (additive sine brass/strings, optional vibrato + tanh glue) ──
// Mirrors hellsine.mjs voice():
//   - parts[]: [ratio, amp] pairs (defaults to 6 partials brass/strings shape)
//   - atk, rel: envelope
//   - vibR, vibD: vibrato Hz + depth (depth ramps in over 120 ms)
//   - drive: tanh saturation amount
//   - pan, gain
//
// JS computes sin(TAU * f * r * vib * lt) with absolute time — broken at
// long lt. Here we phase-accumulate per partial.
typedef struct {
    const double (*parts)[2];   // pointer to [N][2] of (ratio, amp); NULL → default
    int n_parts;
    double atk, rel;
    double vibR, vibD;
    double pan, drive;
    double gain_extra;          // hellsine.mjs opt.gain * 1/Σamps
    double wet_send;
} VoiceOpts;

static const double VOICE_DEFAULT_PARTS[6][2] = {
    {1, 1.00}, {2, 0.50}, {3, 0.34}, {4, 0.16}, {5, 0.12}, {6, 0.06}
};

static void voice_render(double t0, double dur, double midi, double gain, VoiceOpts opt) {
    const double f = m2f(midi);
    const double (*parts)[2] = opt.parts ? opt.parts : VOICE_DEFAULT_PARTS;
    const int nP = opt.parts ? opt.n_parts : 6;
    double amps = 0.0;
    for (int i = 0; i < nP; i++) amps += parts[i][1];
    const double norm = (opt.gain_extra > 0 ? opt.gain_extra : 1.0) / amps;
    const double atk = opt.atk > 0 ? opt.atk : 0.05;
    const double rel = opt.rel > 0 ? opt.rel : 0.18;
    const double vibR = opt.vibR > 0 ? opt.vibR : 5.2;
    const double vibD = opt.vibD > 0 ? opt.vibD : 0.006;
    const double drive = opt.drive > 0 ? opt.drive : 1.0;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;

    // per-partial phase + base phase increment (vibrato modulates increment)
    double phs[32] = {0};
    if (nP > 32) { fprintf(stderr, "voice: too many partials\n"); exit(1); }
    double vibPhase = 0.0;
    const double dVibPhase = vibR / (double)SR;

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur + rel) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk;
        if (env > 1.0) env = 1.0;
        if (lt > dur - rel) {
            double rEnv = (dur - lt) / rel;
            if (rEnv < 0.0) rEnv = 0.0;
            env *= rEnv;
        }
        // vibrato: smooth ramp-in over 120 ms
        const double vibRamp = lt < 0.12 ? lt / 0.12 : 1.0;
        vibPhase += dVibPhase; if (vibPhase >= 1.0) vibPhase -= 1.0;
        const double vib = 1.0 + sin(TAU * vibPhase) * vibD * vibRamp;

        double x = 0.0;
        for (int k = 0; k < nP; k++) {
            const double r = parts[k][0];
            const double a = parts[k][1];
            const double dPh = (f * r * vib) / SR;
            phs[k] += dPh; if (phs[k] >= 1.0) phs[k] -= 1.0;
            x += a * sin(TAU * phs[k]);
        }
        x = tanh(x * norm * drive);
        const double v = x * env * gain;
        const double vL = v * (pan > 0 ? 1.0 - pan : 1.0);
        const double vR = v * (pan < 0 ? 1.0 + pan : 1.0);
        L[i] += (float)vL;
        R[i] += (float)vR;
        if (wetSend > 0.0) {
            WL[i] += (float)(vL * wetSend);
            WR[i] += (float)(vR * wetSend);
        }
    }
}

// ── bell (additive partials with inharmonic stretch + fizzle tail) ─────
typedef struct {
    double pan;
    double atk;          // default 0.080
    double dec_tau;      // default 4.0
    double wet_send;     // default 0.8
    int    fizzle_on;    // default 1
} BellOpts;

static const double BELL_PARTS[6][2] = {
    {1.000, 1.00}, {2.012, 0.55}, {3.025, 0.33},
    {4.055, 0.18}, {5.10,  0.10}, {6.20,  0.05}
};
static const double BELL_FIB[6] = {1, 1, 2, 3, 5, 8};

static void bell_render(double t0, double midi, double gain, BellOpts opt) {
    const double f = m2f(midi);
    double ampSum = 0.0;
    for (int i = 0; i < 6; i++) ampSum += BELL_PARTS[i][1];
    const double norm = 1.0 / ampSum;
    const double atk = opt.atk > 0 ? opt.atk : 0.080;
    const double decTau = opt.dec_tau > 0 ? opt.dec_tau : 4.0;
    const double wetSend = opt.wet_send > 0 ? opt.wet_send : 0.8;
    const double pan = opt.pan;
    const int fizzleOn = opt.fizzle_on;
    const double fizzleAfter = atk + decTau * 0.5;
    const double tailDur = decTau * 5.0;

    double phs[6] = {0,0,0,0,0,0};

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + atk + tailDur) * SR + 1);
    if (iEnd > N) iEnd = N;

    const double pL = (pan > 0 ? 1.0 - pan : 1.0);
    const double pR = (pan < 0 ? 1.0 + pan : 1.0);

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double env = lt < atk
            ? lt / atk
            : exp(-(lt - atk) / decTau);
        double x = 0.0;
        for (int k = 0; k < 6; k++) {
            const double r = BELL_PARTS[k][0];
            const double a = BELL_PARTS[k][1];
            double rEff = r;
            if (fizzleOn && lt > fizzleAfter) {
                double fizzleP = (lt - fizzleAfter) / decTau;
                if (fizzleP > 1.0) fizzleP = 1.0;
                const double rateScale = 1.0 + 3.0 * pow(fizzleP, 1.4);
                const double fibHz = BELL_FIB[k % 6] * rateScale;
                const double depth = 0.018 + 0.022 * pow(fizzleP, 2);
                const double drift = (k % 2 == 0 ? 1.0 : -1.0) * 0.06 * pow(fizzleP, 1.8);
                rEff *= (1.0 + drift) * (1.0 + depth * sin(TAU * fibHz * lt) * fizzleP);
            }
            phs[k] += (f * rEff) / SR;
            if (phs[k] >= 1.0) phs[k] -= 1.0;
            const double partEnv = lt < atk ? 1.0 : exp(-(lt - atk) / (decTau / r));
            x += a * sin(TAU * phs[k]) * partEnv;
        }
        x *= norm;
        const double v = x * env * gain;
        const double vL = v * pL;
        const double vR = v * pR;
        L[i] += (float)vL;
        R[i] += (float)vR;
        if (wetSend > 0.0) {
            WL[i] += (float)(vL * wetSend);
            WR[i] += (float)(vR * wetSend);
        }
    }
}

// ── sub (sine fundamental + 2nd/3rd harmonic, click transient, tanh) ───
// Mirrors hellsine.mjs sub(): f = m2f(midi - 12), reads DUCK[].
static void sub_render(double t0, double dur, double midi, double gain) {
    const double f = m2f(midi - 12.0);
    // three partials, phase-accumulated
    double ph1 = 0.0, ph2 = 0.0, ph3 = 0.0;
    const double dPh1 = f / SR;
    const double dPh2 = (f * 2.0) / SR;
    const double dPh3 = (f * 3.0) / SR;
    // click oscillator (1100 Hz)
    double phClick = 0.0;
    const double dPhClick = 1100.0 / SR;

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur + 0.05) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double atkEnv = lt < 0.006 ? lt / 0.006 : 1.0;
        const double relEnv = lt > dur - 0.03
            ? exp(-(lt - (dur - 0.03)) / 0.020)
            : 1.0;
        const double a = atkEnv * relEnv;
        const double d = DUCK[i];
        ph1 += dPh1; if (ph1 >= 1.0) ph1 -= 1.0;
        ph2 += dPh2; if (ph2 >= 1.0) ph2 -= 1.0;
        ph3 += dPh3; if (ph3 >= 1.0) ph3 -= 1.0;
        phClick += dPhClick; if (phClick >= 1.0) phClick -= 1.0;
        // CRUNCHIER bass — more 2nd/3rd, added 4th harmonic, harder
        // saturation so the upper harmonics fold over into 200-500 Hz
        // grit. Click also pushed for sharper attack.
        double x = sin(TAU * ph1)
                 + 0.55 * sin(TAU * ph2)
                 + 0.28 * sin(TAU * ph3)
                 + 0.14 * sin(TAU * 4.0 * ph1);     // 4th harmonic, no separate phase ok at sub freqs
        const double click = lt < 0.004
            ? sin(TAU * phClick) * exp(-lt / 0.0012) * 0.55
            : 0.0;
        x = tanh(x * 2.6) * 0.90 + click * 0.55;     // drive 1.85 → 2.6, output 0.85 → 0.90
        const double v = x * a * d * gain * 0.68;
        L[i] += (float)v;
        R[i] += (float)v;
    }
}

// ── kick — two variants ────────────────────────────────────────────────
// non-ULTIMATE: slow-blooming "hole" gabber kick (sub pressure filling
//               the hole the DUCK creates).
// ULTIMATE:    brighter PUNCH kick — 0.8ms snap, 2.4kHz click, harder
//              drive, faster pitch sweep. Front-of-mix tight clicks
//              instead of the slow bloom. After AC_STAMP, kicks shorten
//              + pitch up an octave.
static double AC_STAMP_TIME = 107.27;     // overwritten by render_full_track

static void kick_render(double t0, double drive, double gain, double thin) {
    if (NOKICK) return;
    // Suppress final kicks past 140 s (coda pop fix). Also kill the
    // "double-kick skip" the user heard at 2:10 (= 130 s ish) — bar
    // 14 of climax in the engine has a 4-fragment staggered kick at
    // beats 0, 0.95, 1.90, 2.85 of that bar. Drop t0 in 129.4-130.6
    // range = effectively wipe that double-kick stutter.
    // (@jeffrey "at 2:10 there's a little kick double kick skip i'd
    //  like to get removed")
    if (t0 >= 140.0) return;
    // Wipe the "lame extra kicks" cluster around 2:06-2:11 (climax
    // bars 12-15: punchy SINE-CHORDAL + 4-fragment staggered + 6
    // micro-kicks). Replaced by a snare rush in post-arrangement.
    // (@jeffrey "at 2:06 the kicks / extra kicks are feeling lame /
    //  better to do like a snare rush")
    // Wipe range extended back through 131.0 — the climax bar-14
    // staggered fragments + bar-15 micro-kicks at 2:09 were reading
    // as arrhythmic double-kick weirdness against the screwed crow
    // tail. Gallop @ 128.0s + crow scratch fill the percussion role.
    // (@jeffrey "extra double kicks at like 2:09 are arrhythmic")
    if (t0 >= 125.5 && t0 <= 131.0) return;
    t0 += EAGER_PERC_OFFSET;     // fire ahead of grid for rushed feel

    if (ULTIMATE) {
        // shortHigh: after the AC stamp, kicks become much shorter + higher
        const int shortHigh = (t0 >= AC_STAMP_TIME);
        // INVERTED globalTighten: pre-stamp kicks now START thin (0.95)
        // and BLOOM into the drop (→ 0 by AC_STAMP_TIME). Earlier
        // ordering was the opposite. (@jeffrey "first kicks should be
        // a bit thin then bomb / booom").
        double globalTighten = shortHigh ? 0.0
            : ((1.0 - t0 / AC_STAMP_TIME) * 0.95);
        if (globalTighten > 0.95) globalTighten = 0.95;
        if (globalTighten < 0.0) globalTighten = 0.0;
        const double effThin = thin > globalTighten ? thin : globalTighten;
        // latePost threshold pushed 126 → 134 so the climax's dense
        // bar-13/14/15 fragments + micro-kicks stay in the consistent
        // shortHigh mode through 2:09-2:11. Hard switch at 126s was
        // creating choppy/cut-off feel in that range.
        // (@jeffrey "kick drums around 2:09-2:11 cut off or weird /
        //  make sure that rides nice")
        const int latePost = (t0 >= 134.0);
        // Post-2:00 (shortHigh) kicks pushed DEEPER for more sub focus
        // (@jeffrey "kick should be more low after the 2:00 drop").
        // pStart 320 → 220, pEnd 55 → 38 — sub bloom + slow pitch fall.
        const double dur     = shortHigh ? (latePost ? 0.13 : 0.24) : 0.30;
        const double pStart  = shortHigh ? (latePost ? 180 : 220) : 260;
        const double pEnd    = shortHigh ? (latePost ? 32  : 38)  : 50;
        const double pT      = shortHigh ? (latePost ? 0.028 : 0.040) : 0.030;
        const double bodyTauBase = shortHigh ? (latePost ? 0.085 : 0.130) : 0.14;
        const double bodyTau = bodyTauBase * (1.0 - effThin * 0.55);
        const double clickAmp = (shortHigh ? (latePost ? 0.24 : 0.32) : 0.55) * (1.0 - effThin * 0.65);

        // Lighter duck — 8 ms close to 35%, 180 ms re-open.
        const long di     = (long)(t0 * SR);
        const long dN     = (long)(0.18 * SR);
        const long closeN = (long)(0.008 * SR);
        const long openN  = dN - closeN;
        for (long k = 0; k < dN && di + k < N; k++) {
            double env;
            if (k < closeN) env = 1.0 - ((double)k / closeN) * 0.65;
            else { const double p = (double)(k - closeN) / openN;
                   env = 0.35 + 0.65 * (1.0 - exp(-p * 3.5)); }
            if (di + k >= 0 && (float)env < DUCK[di + k]) DUCK[di + k] = (float)env;
        }

        double ph = 0.0, phClick = 0.0;
        // Click freq dropped from 2400 → 900 Hz for boxy thock instead
        // of squeak. The SNAP noise burst (below) replaces the high
        // transient detail.
        const double dPhClick = (shortHigh ? 900.0 : 2400.0) / SR;
        long iStart = (long)(t0 * SR); if (iStart < 0) iStart = 0;
        long iEnd = (long)((t0 + dur) * SR + 1); if (iEnd > N) iEnd = N;
        for (long i = iStart; i < iEnd; i++) {
            const double lt = (double)i / SR - t0;
            const double f = pEnd + (pStart - pEnd) * exp(-lt / pT);
            ph += (TAU * f) / SR;
            const double atk   = 1.0 - exp(-lt / 0.0008);       // 0.8 ms snap
            const double decay = exp(-lt / bodyTau);
            const double amp   = atk * decay;
            phClick += dPhClick; if (phClick >= 1.0) phClick -= 1.0;
            const double click = lt < 0.006
                ? sin(TAU * phClick) * exp(-lt / 0.0015) * clickAmp
                : 0.0;
            double x = sin(ph);
            // INDUSTRIAL drive — hotter saturation for post-stamp kicks
            // (the back half of the track), milder for pre-stamp.
            const double drvMul = shortHigh ? 1.45 : 0.65;
            x = tanh(x * drive * drvMul);
            // Hard clip + bit-crush for the back half. The LAST few
            // kicks (135-140s, just before the t>=140 cutoff) get
            // progressively heavier crush + tighter clip — interpolated
            // from 9-bit clip 0.88 → 5-bit clip 0.70 as we approach
            // the cutoff so the final hits read as crushed/compressed
            // out. (@jeffrey "bit crunch / compress the last few kicks
            //  interpolated to that")
            if (shortHigh) {
                double finalFr = 0.0;
                if (t0 >= 135.0) {
                    finalFr = (t0 - 135.0) / 5.0;
                    if (finalFr > 1.0) finalFr = 1.0;
                }
                const double clipLvl = 0.88 - 0.18 * finalFr;     // 0.88 → 0.70
                if (x >  clipLvl) x =  clipLvl;
                if (x < -clipLvl) x = -clipLvl;
                const double Q = 256.0 - (256.0 - 32.0) * finalFr; // 9-bit → 5-bit
                x = floor(x * Q + 0.5) / Q;
            }
            // SNAP + BLAST AIR layers (post-stamp only) — a 2 ms HF
            // noise burst at the very start (the speaker-cone SNAP)
            // followed by a 30 ms low-passed AIR blast (the BLAST AIR
            // tail). Replaces the squeaky 2.4 kHz click with proper
            // industrial-kick transient detail.
            if (shortHigh) {
                static uint32_t snapState = 0x12345678u;
                if (lt < 0.020) {
                    snapState ^= snapState << 13; snapState ^= snapState >> 17; snapState ^= snapState << 5;
                    const double noise = ((double)snapState / 4294967296.0) * 2.0 - 1.0;
                    // SNAP: 0-2.5 ms, HF detail
                    const double snapEnv = exp(-lt / 0.0008);
                    const double snap = noise * snapEnv * 0.55 * gain;
                    // BLAST AIR: 0-30 ms, low-passed noise tail (1-pole LPF)
                    static double airLP = 0.0;
                    const double cutoff = 0.18;     // ~1.4 kHz at 48k
                    airLP += cutoff * (noise - airLP);
                    const double airEnv = exp(-lt / 0.008);
                    const double air = airLP * airEnv * 0.32 * gain;
                    L[i] += (float)(snap + air);
                    R[i] += (float)(snap + air);
                }
            }
            const double v = (x * amp + click) * 0.92 * gain;
            L[i] += (float)v;
            R[i] += (float)v;
        }
        // BOOwub — reverse-kick supersample right after the forward kick.
        // Port of hellsine.mjs:613. Only after the AC stamp. Cycles through
        // [-oct, +fifth, -fifth, +oct] beat-indexed, giving the dub-style
        // wub-wub tonal answer that varies per kick.
        {
            const double revPitchOpts[4] = { 0.5, 1.5, 0.667, 2.0 };
            const int beatIdx = (int)(t0 / SPB_G);
            const double revPitch = revPitchOpts[((beatIdx % 4) + 4) % 4];
            const double revPStart = pStart * revPitch;
            const double revPEnd   = pEnd   * revPitch;
            const double revDur    = dur * 0.85;
            const double tRev      = t0 + dur;
            double phR = 0.0;
            long irStart = (long)(tRev * SR); if (irStart < 0) irStart = 0;
            long irEnd   = (long)((tRev + revDur) * SR + 1); if (irEnd > N) irEnd = N;
            for (long i = irStart; i < irEnd; i++) {
                const double lt  = (double)i / SR - tRev;
                const double rlt = revDur - lt;                 // reversed local time
                if (rlt < 0) break;
                const double f = revPEnd + (revPStart - revPEnd) * exp(-rlt / pT);
                phR += (TAU * f) / SR;
                const double atk   = 1.0 - exp(-rlt / 0.0008);
                const double decay = exp(-rlt / bodyTau);
                const double amp   = atk * decay;
                double x = sin(phR);
                x = tanh(x * drive * 1.25);                    // industrial drive
                if (x >  0.88) x =  0.88;
                if (x < -0.88) x = -0.88;
                const double Q = 256.0;
                x = floor(x * Q + 0.5) / Q;
                // BOOwub gain pulled DOWN (0.55 → 0.30) so the doubled
                // reverse-kicks don't make 2:00+ feel like too many
                // kicks. (@jeffrey "around 2:05 the massive extra kicks
                // is a little too much")
                const double v = x * amp * 0.92 * gain * 0.30;
                L[i] += (float)v;
                R[i] += (float)v;
            }
        }
        return;
    }

    // non-ULTIMATE — the "hole" gabber kick (slow bloom, deep sub-only).
    const double dur = 0.55;
    const double pStart = 180.0, pEnd = 28.0;
    const double pT = 0.080;
    double ph = 0.0;
    const long di = (long)(t0 * SR);
    const long dN = (long)(0.46 * SR);
    const long closeN = (long)(0.015 * SR);
    const long openN  = dN - closeN;
    for (long k = 0; k < dN && di + k < N; k++) {
        double env;
        if (k < closeN) env = 1.0 - ((double)k / closeN) * 0.92;
        else { const double p = (double)(k - closeN) / openN;
               env = 0.08 + 0.92 * (1.0 - exp(-p * 3.2)); }
        if (di + k >= 0 && (float)env < DUCK[di + k]) DUCK[di + k] = (float)env;
    }
    long iStart = (long)(t0 * SR); if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1); if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double f = pEnd + (pStart - pEnd) * exp(-lt / pT);
        ph += (TAU * f) / SR;
        const double atk   = 1.0 - exp(-lt / 0.012);
        const double decay = exp(-lt / 0.22);
        const double amp   = atk * decay;
        double x = sin(ph);
        x = tanh(x * (drive * 0.45));
        const double v = x * amp * 0.95 * gain;
        L[i] += (float)v;
        R[i] += (float)v;
    }
}

// ── snare (sine body chirp + 96-voice additive-sine crack + HF snap) ───
#define SNARE_VOICES 96
static void snare_render(double t0, double gain, double bodyF_opt) {
    const double dur = 0.30;
    const double bodyF = bodyF_opt > 0 ? bodyF_opt : 175.0;
    double freqs[SNARE_VOICES], phs[SNARE_VOICES];
    const double fMin = 1200.0, fMax = 9000.0;
    for (int i = 0; i < SNARE_VOICES; i++) {
        const double u = ((double)i + rng()) / SNARE_VOICES;
        freqs[i] = fMin * pow(fMax / fMin, u);
        phs[i] = rng() * TAU;
    }
    const double norm = 1.0 / sqrt((double)SNARE_VOICES);

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        // body — pitch blip + fast decay (absolute-time sin chirp, matches JS)
        const double pf = bodyF * (1.0 + 0.5 * exp(-lt / 0.008));
        const double body = (sin(TAU * pf * lt) + 0.55 * sin(TAU * pf * 1.48 * lt))
                          * exp(-lt / 0.045);
        // crack — 96 detuned sines (absolute time, with rng-determined phase offsets)
        double noise = 0.0;
        for (int k = 0; k < SNARE_VOICES; k++) {
            noise += sin(TAU * freqs[k] * lt + phs[k]);
        }
        const double crackEnv = (1.0 - exp(-lt / 0.0006)) * exp(-lt / 0.030);
        const double snap = lt < 0.006
            ? sin(TAU * 6200.0 * lt) * exp(-lt / 0.0014) * 0.55
            : 0.0;
        double x = body * 0.75 + noise * norm * crackEnv * 0.85 + snap;
        x = tanh(x * 1.4);
        const double envAtk = lt / 0.0004; const double aE = envAtk < 1.0 ? envAtk : 1.0;
        const double v = x * gain * aE;
        // SNARE_PAN_BIAS — set externally per call so the snare rush
        // can pan around (@jeffrey "pitch around the snares / pan
        // them around").
        const double pL = (SNARE_PAN_BIAS > 0) ? (1.0 - SNARE_PAN_BIAS) : 1.0;
        const double pR = (SNARE_PAN_BIAS < 0) ? (1.0 + SNARE_PAN_BIAS) : 1.0;
        L[i] += (float)(v * 0.96 * pL);
        R[i] += (float)(v * pR);
    }
}

// ── steam (130-voice additive-sine breath, broadband fused) ────────────
#define STEAM_VOICES_MAX 200
static void steam_render(double t0, double dur, double gain, int nVoices,
                         double fMin, double fMax,
                         double atk, double rel,
                         double breathRate, double breathDepth) {
    if (nVoices <= 0) nVoices = 130;
    if (nVoices > STEAM_VOICES_MAX) nVoices = STEAM_VOICES_MAX;
    if (fMin <= 0) fMin = 400.0;
    if (fMax <= 0) fMax = 5500.0;
    if (atk <= 0) atk = 0.6;
    if (rel <= 0) rel = 1.2;
    if (breathRate <= 0) breathRate = 0.5;
    if (breathDepth <= 0) breathDepth = 0.35;
    double freqs[STEAM_VOICES_MAX], phases[STEAM_VOICES_MAX];
    for (int i = 0; i < nVoices; i++) {
        const double u = ((double)i + rng()) / nVoices;
        freqs[i] = fMin * pow(fMax / fMin, u);
        phases[i] = rng() * TAU;
    }
    const double norm = 1.0 / sqrt((double)nVoices);

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk; if (env > 1.0) env = 1.0;
        if (lt > dur - rel) {
            double rEnv = (dur - lt) / rel; if (rEnv < 0.0) rEnv = 0.0;
            env *= rEnv;
        }
        const double breath = (1.0 - breathDepth) + breathDepth * sin(TAU * breathRate * lt);
        double x = 0.0;
        for (int k = 0; k < nVoices; k++) x += sin(TAU * freqs[k] * lt + phases[k]);
        x = tanh(x * norm * 1.1);
        const double v = x * env * breath * gain;
        L[i] += (float)(v * 0.92);
        R[i] += (float)(v * 1.00);
    }
}

// ── woodTick (short bright dual-sine tick) ─────────────────────────────
static void woodtick_render(double t0, double gain) {
    const double f = 1900.0;
    const double dur = 0.045;
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double env = exp(-lt / 0.010) * (1.0 - exp(-lt / 0.0006));
        const double x = sin(TAU * f * lt) + 0.3 * sin(TAU * f * 1.41 * lt);
        const double v = x * env * gain;
        L[i] += (float)(v * 0.92);
        R[i] += (float)v;
    }
}

// ── tick (closed/open hat, ring-mod for open) ──────────────────────────
static void tick_render(double t0, double gain, int open) {
    const double f = open ? 7400.0 : 9200.0;
    const double dur = open ? 0.06 : 0.022;
    const double decay = open ? 0.026 : 0.004;
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double env = exp(-lt / decay);
        double x = sin(TAU * f * lt) + 0.45 * sin(TAU * f * 1.51 * lt);
        if (open) x *= sin(TAU * 5300.0 * lt);
        const double v = x * env * gain;
        L[i] += (float)(v * 0.9);
        R[i] += (float)v;
    }
}

// ── GRAND PIANO SAMPLE BANK (ac-native fedac samples) ─────────────
// Loads /Users/jas/aesthetic-computer/fedac/native/samples/piano/<midi>.raw
// (float32 mono at 192 kHz). Decimates 4:1 to engine 48 kHz.
// 26 anchor pitches: MIDI 21, 24, 27, ..., 96 (every 3 semitones).
// play_grand_piano() finds nearest anchor and pitch-shifts via rate.
#define PIANO_BANK_COUNT 26
#define PIANO_ANCHOR_MIN 21
#define PIANO_ANCHOR_STEP 3
static float *piano_samples[PIANO_BANK_COUNT] = {0};
static long   piano_sample_n[PIANO_BANK_COUNT] = {0};
static int    piano_bank_loaded = 0;

static void piano_bank_load(void) {
    if (piano_bank_loaded) return;
    int loaded = 0;
    for (int i = 0; i < PIANO_BANK_COUNT; i++) {
        const int midi = PIANO_ANCHOR_MIN + i * PIANO_ANCHOR_STEP;
        char path[512];
        snprintf(path, sizeof(path),
                 "/Users/jas/aesthetic-computer/fedac/native/samples/piano/%d.raw",
                 midi);
        FILE *f = fopen(path, "rb");
        if (!f) continue;
        fseek(f, 0, SEEK_END);
        const long sz = ftell(f);
        fseek(f, 0, SEEK_SET);
        if (sz <= 0 || (sz % 4) != 0) { fclose(f); continue; }
        const long n192k = sz / 4;
        float *raw = (float*)malloc(n192k * sizeof(float));
        if (!raw) { fclose(f); continue; }
        if ((long)fread(raw, 4, n192k, f) != n192k) { free(raw); fclose(f); continue; }
        fclose(f);
        // Decimate 192k → 48k via 4-sample box average (anti-aliases enough
        // for piano transients).
        const long n48k = n192k / 4;
        float *dec = (float*)malloc(n48k * sizeof(float));
        if (!dec) { free(raw); continue; }
        for (long j = 0; j < n48k; j++) {
            dec[j] = 0.25f * (raw[j*4] + raw[j*4+1] + raw[j*4+2] + raw[j*4+3]);
        }
        free(raw);
        piano_samples[i] = dec;
        piano_sample_n[i] = n48k;
        loaded++;
    }
    piano_bank_loaded = 1;
    if (loaded > 0) report("→ piano bank · %d/%d ac-native samples loaded (192k→48k)",
                           loaded, PIANO_BANK_COUNT);
}

static void play_grand_piano(double t0, double midi, double gain, double pan) {
    if (!piano_bank_loaded) piano_bank_load();
    int idx = (int)round((midi - PIANO_ANCHOR_MIN) / (double)PIANO_ANCHOR_STEP);
    if (idx < 0) idx = 0;
    if (idx >= PIANO_BANK_COUNT) idx = PIANO_BANK_COUNT - 1;
    if (!piano_samples[idx]) return;
    const int anchorMidi = PIANO_ANCHOR_MIN + idx * PIANO_ANCHOR_STEP;
    const double rate = pow(2.0, (midi - anchorMidi) / 12.0);
    const float *buf = piano_samples[idx];
    const long bufN  = piano_sample_n[idx];
    const long outLen = (long)((double)bufN / rate);
    const long iS = (long)(t0 * SR);
    const long fadeOut = (long)(0.030 * SR);
    const double pL = (pan > 0) ? (1.0 - pan) : 1.0;
    const double pR = (pan < 0) ? (1.0 + pan) : 1.0;
    for (long w = 0; w < outLen; w++) {
        const long oi = iS + w;
        if (oi < 0 || oi >= N) continue;
        const double readPos = (double)w * rate;
        if (readPos + 1 >= (double)bufN) break;
        const long ri = (long)readPos;
        const double frac = readPos - ri;
        double s = (double)buf[ri] * (1.0 - frac) + (double)buf[ri + 1] * frac;
        double env = 1.0;
        if (outLen - w < fadeOut) env = (double)(outLen - w) / fadeOut;
        const double v = s * env * gain;
        L[oi] += (float)(v * pL);
        R[oi] += (float)(v * pR);
        SL[oi] += (float)(v * 0.18);
        SR_[oi]+= (float)(v * 0.18);
    }
}

// ── piano (8 inharmonic stretched partials + bitcrush + S+H) ──────────
// Mirrors hellsine.mjs piano(). All-sine additive grand-piano emulation,
// already phase-accumulated in JS, so C porting is mostly translation.
typedef struct {
    double pan;
    double sus;          // sustain scale on the slow-tau decay (default 1.0)
    int    bits;         // bit-depth for crush (default 6)
    int    hold;         // sample-and-hold ratio (default 4)
} PianoOpts;

static const double PIANO_PARTS[8][3] = {
    {1.0000, 1.00, 1.00}, {2.0008, 0.55, 0.85},
    {3.0024, 0.30, 0.70}, {4.0048, 0.18, 0.55},
    {5.0080, 0.10, 0.45}, {6.0120, 0.06, 0.38},
    {7.0168, 0.035, 0.32}, {8.0224, 0.020, 0.28},
};
static void piano_render(double t0, double dur, double midi, double gain, PianoOpts opt) {
    const double f = m2f(midi);
    const double pan = opt.pan;
    const double sus = opt.sus > 0 ? opt.sus : 1.0;
    const int    bits = opt.bits > 0 ? opt.bits : 6;
    const int    hold = opt.hold > 0 ? opt.hold : 4;
    const double fastTau = 0.35, slowTau = 2.2 * sus;
    const double tauScaleRaw = m2f(60.0) / f;
    const double tauScale = tauScaleRaw > 1.0 ? 1.0 : (tauScaleRaw < 0.3 ? 0.3 : tauScaleRaw);
    const long steps = 1L << (bits - 1);
    double phs[8] = {0,0,0,0,0,0,0,0};
    double held = 0.0; long holdI = 0;

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur + 0.8) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double atk = 1.0 - exp(-lt / 0.003);
        double x = 0.0;
        for (int k = 0; k < 8; k++) {
            const double r  = PIANO_PARTS[k][0];
            const double a  = PIANO_PARTS[k][1];
            const double ds = PIANO_PARTS[k][2];
            phs[k] += (f * r) / SR;
            if (phs[k] >= 1.0) phs[k] -= 1.0;
            const double fast = exp(-lt / (fastTau * ds * tauScale));
            const double slow = exp(-lt / (slowTau * ds * tauScale));
            x += a * sin(TAU * phs[k]) * atk * (0.6 * fast + 0.4 * slow);
        }
        x = tanh(x * 0.55);
        if (lt > dur) x *= exp(-(lt - dur) / 0.15);
        // bitcrush + sample-and-hold
        if (holdI++ % hold == 0) held = round(x * (double)steps) / (double)steps;
        const double v = held * gain;
        L[i] += (float)(v * (pan > 0 ? 1.0 - pan : 1.0));
        R[i] += (float)(v * (pan < 0 ? 1.0 + pan : 1.0));
    }
}

// ── sawLead (18 partials × detuned pair = supersaw, optional gate) ────
// Reads DUCK[] for sidechain ducking.
typedef struct {
    int    partials;     // default 18
    double detune;       // default 0.006
    double pan;
    double atk;          // default 0.012
    double rel;          // default 0.06
    double drive;        // default 0.8
    double gate_ms;      // 0 → no gate
    double gate_on_frac; // default 0.55
} SawOpts;
static void saw_render(double t0, double dur, double midi, double gain, SawOpts opt) {
    const double f = m2f(midi);
    const int partials = opt.partials > 0 ? opt.partials : 18;
    if (partials > 32) { fprintf(stderr, "saw: too many partials\n"); exit(1); }
    const double detune = opt.detune > 0 ? opt.detune : 0.006;
    const double pan = opt.pan;
    const double atk = opt.atk > 0 ? opt.atk : 0.012;
    const double rel = opt.rel > 0 ? opt.rel : 0.06;
    const double drive = opt.drive > 0 ? opt.drive : 0.8;
    const double gateMs = opt.gate_ms;
    const double gateOn = opt.gate_on_frac > 0 ? opt.gate_on_frac : 0.55;
    double phsA[32] = {0}, phsB[32] = {0};
    double norm = 0.0;
    for (int n = 1; n <= partials; n++) norm += 1.0 / n;
    const double partNorm = 1.0 / norm;

    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur + rel) * SR + 1);
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk; if (env > 1.0) env = 1.0;
        if (lt > dur - rel) { double r = (dur - lt) / rel; if (r < 0) r = 0; env *= r; }
        const double d = DUCK[i];
        double gate = 1.0;
        if (gateMs > 0.0) {
            const double phase = fmod(lt * 1000.0 / gateMs, 1.0);
            gate = phase < gateOn ? 1.0 : 0.0;
        }
        double x = 0.0;
        for (int n = 1; n <= partials; n++) {
            const int k = n - 1;
            phsA[k] += (f * (double)n) / SR;
            phsB[k] += (f * (1.0 + detune) * (double)n) / SR;
            if (phsA[k] >= 1.0) phsA[k] -= 1.0;
            if (phsB[k] >= 1.0) phsB[k] -= 1.0;
            const double a = 1.0 / n;
            x += a * (sin(TAU * phsA[k]) + sin(TAU * phsB[k])) * 0.5;
        }
        x = tanh(x * partNorm * drive);
        const double v = x * env * d * gate * gain;
        L[i] += (float)(v * (pan > 0 ? 1.0 - pan : 1.0));
        R[i] += (float)(v * (pan < 0 ? 1.0 + pan : 1.0));
    }
}

// ── hoover (4 detuned voices with rising sine-on-sine FM index) ───────
static void hoover_render(double t0, double dur, double midi, double gain) {
    const double f = m2f(midi);
    const double det[4] = {0.994, 1.0, 1.007, 1.013};
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / 0.02; if (env > 1.0) env = 1.0;
        const double tail = 1.0 - lt / dur; const double tailC = tail < 0 ? 0 : tail;
        env *= tailC;
        double idxP = lt / (dur * 0.6); if (idxP > 1.0) idxP = 1.0;
        const double idx = 1.4 + 3.0 * idxP;
        double x = 0.0;
        for (int k = 0; k < 4; k++) {
            const double mod = sin(TAU * f * det[k] * 0.5 * lt) * idx;
            x += sin(TAU * f * det[k] * lt + mod);
        }
        x = tanh(x * 0.5 * (1.0 + idx * 0.2)) * env * gain;
        L[i] += (float)(x * 0.85);
        R[i] += (float)x;
    }
}

// ── stab (2-op sine FM, high index, hard clip) ────────────────────────
static void stab_render(double t0, double midi, double gain) {
    const double f = m2f(midi);
    const double dur = 0.16;
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double env = exp(-lt / 0.06) * (1.0 - exp(-lt / 0.001));
        const double mod = sin(TAU * f * 1.997 * lt) * (5.5 * exp(-lt / 0.05));
        double x = sin(TAU * f * lt + mod);
        if (x > 0.9) x = 0.9;
        if (x < -0.9) x = -0.9;
        x *= 1.5;
        if (x > 0.9) x = 0.9;
        if (x < -0.9) x = -0.9;
        const double v = x * env * gain;
        L[i] += (float)v;
        R[i] += (float)(v * 0.92);
    }
}

// ── riser (pitch + FM index sweep up) ─────────────────────────────────
static void riser_render(double t0, double dur, double m0, double m1, double gain) {
    const double f0 = m2f(m0), f1 = m2f(m1);
    double ph = 0.0;
    long iStart = (long)(t0 * SR);
    if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur) * SR + 1);
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        const double p = lt / dur;
        const double f = f0 * pow(f1 / f0, p);
        ph += (TAU * f) / SR;
        const double idx = 1.0 + 5.0 * p;
        double x = sin(ph + sin(ph * 0.5) * idx);
        x = tanh(x * 2.2);
        double env = lt / 0.05; if (env > 1.0) env = 1.0;
        env *= (0.3 + 0.7 * p);
        const double v = x * env * gain;
        L[i] += (float)v;
        R[i] += (float)(v * 0.95);
    }
}

// ── bubble (Cook bubble-shape: rising phaseStep, exp-decaying amplitude) ──
// Mirrors hellsine.mjs bubble(). Per-bubble running normalization by
// maxOut means the rendered output amplitude is unit-scale before volume.
static void bubble_render(double startSec, double radiusMM, double rise,
                          double volume, double pan,
                          double wetSend, double depth) {
    const long startIdx = (long)(startSec * SR);
    const double radius = radiusMM * 0.001;
    const double timestep = 1.0 / SR;
    const double pRadius = radius * sqrt(radius);
    double amp = 17.2133 * pRadius * depth;
    const double decay = 0.13 / radius + 0.0072 * pRadius;
    const double gainPerSample = exp(-decay * timestep);
    double phaseStep = (3.0 / radius) * timestep;
    const double phaseRise = phaseStep * decay * rise * timestep;
    double phase = 0.0;
    double lastOut = 0.0;
    double maxOut = 1.0;
    const double QUIET = 0.000001;
    double panC = pan; if (panC > 1.0) panC = 1.0; if (panC < -1.0) panC = -1.0;
    const double angle = (panC * 0.5 + 0.5) * (M_PI / 2.0);
    const double gL = cos(angle), gR = sin(angle);
    const long maxSamples = (long)(4.0 * SR);
    for (long i = 0; i < maxSamples; i++) {
        if (amp < QUIET && phase > 1.0) break;
        const double alpha = phase < 0 ? 0 : (phase < 1.0 ? phase : 1.0);
        const double ph = M_PI * 2.0 * phase;
        const double rich = (sin(ph) + 0.34 * sin(2 * ph) + 0.17 * sin(3 * ph)) / 1.51;
        const double out = (1.0 - alpha) * lastOut + alpha * amp * rich;
        lastOut = out;
        phase += phaseStep;
        phaseStep += phaseRise;
        amp *= gainPerSample;
        double v = out * volume * 1000.0;
        const double av = fabs(v);
        if (av > maxOut) maxOut = av;
        v = v / maxOut;
        const long dst = startIdx + i;
        if (dst < 0 || dst >= N) continue;
        L[dst]  += (float)(v * gL);
        R[dst] += (float)(v * gR);
        if (wetSend > 0.0) {
            WL[dst] += (float)(v * gL * wetSend);
            WR[dst] += (float)(v * gR * wetSend);
        }
    }
}

// ── WAV reader (PCM s16/s24/s32, IEEE float, mono+stereo) → mono ──────
// Mirrors hellsine.mjs loadWavMono(): mix channels to mono, linear
// resample to SR if needed, trim leading/trailing samples below 0.02,
// peak-normalize to 1.0. Caller owns returned buffer.
static float *load_wav_mono(const char *path, long *out_n) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;     // caller decides whether to warn
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t*)malloc(sz);
    if (fread(buf, 1, sz, f) != (size_t)sz) { fclose(f); free(buf); return NULL; }
    fclose(f);
    if (sz < 12 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) {
        fprintf(stderr, "bad WAV: %s\n", path); free(buf); return NULL;
    }
    long p = 12;
    int format = 0, channels = 0, bits = 0;
    uint32_t fmtSR = 0;
    long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        const char *id = (const char*)(buf + p);
        uint32_t s = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8
                   | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(id, "fmt ", 4)) {
            format   = buf[p+8] | (buf[p+9] << 8);
            channels = buf[p+10] | (buf[p+11] << 8);
            fmtSR    = (uint32_t)buf[p+12] | (uint32_t)buf[p+13] << 8
                     | (uint32_t)buf[p+14] << 16 | (uint32_t)buf[p+15] << 24;
            bits     = buf[p+22] | (buf[p+23] << 8);
        } else if (!memcmp(id, "data", 4)) {
            dOff = p + 8; dLen = s;
        }
        p += 8 + s + (s & 1);
    }
    if (!channels || !bits || !dOff) {
        fprintf(stderr, "bad WAV: %s\n", path); free(buf); return NULL;
    }
    const int fb = (bits / 8) * channels;
    const long frames = dLen / fb;
    float *mono = (float*)calloc(frames, sizeof(float));
    for (long i = 0; i < frames; i++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
            const long o = dOff + i * fb + c * (bits / 8);
            if (format == 3 && bits == 32) {
                float v; memcpy(&v, buf + o, 4); acc += v;
            } else if (bits == 16) {
                int16_t v; memcpy(&v, buf + o, 2); acc += (double)v / 32768.0;
            } else if (bits == 24) {
                int32_t v = buf[o] | (buf[o+1] << 8) | (int32_t)((int8_t)buf[o+2]) << 16;
                acc += (double)v / 8388608.0;
            } else if (bits == 32) {
                int32_t v; memcpy(&v, buf + o, 4); acc += (double)v / 2147483648.0;
            }
        }
        mono[i] = (float)(acc / channels);
    }
    free(buf);
    long n = frames;
    if (fmtSR != (uint32_t)SR) {
        const long outN = (long)((double)frames * SR / fmtSR + 0.5);
        float *rs = (float*)calloc(outN, sizeof(float));
        for (long i = 0; i < outN; i++) {
            const double x = (double)i * fmtSR / SR;
            const long i0 = (long)x;
            const double fr = x - i0;
            const double a = (i0 >= 0 && i0 < frames) ? mono[i0] : 0.0;
            const double b = (i0 + 1 >= 0 && i0 + 1 < frames) ? mono[i0 + 1] : 0.0;
            rs[i] = (float)(a + (b - a) * fr);
        }
        free(mono); mono = rs; n = outN;
    }
    // trim leading/trailing silence below TH = 0.02
    const float TH = 0.02f;
    long a = 0, b = n;
    while (a < b && fabsf(mono[a]) < TH) a++;
    while (b > a && fabsf(mono[b - 1]) < TH) b--;
    const long trimmed = b - a;
    float *out = (float*)malloc(trimmed * sizeof(float));
    memcpy(out, mono + a, trimmed * sizeof(float));
    free(mono);
    // peak normalize
    float pk = 0;
    for (long i = 0; i < trimmed; i++) {
        const float av = fabsf(out[i]); if (av > pk) pk = av;
    }
    if (pk > 0) for (long i = 0; i < trimmed; i++) out[i] /= pk;
    *out_n = trimmed;
    return out;
}

// ── playSample (linear-interp resample, pan, fade in/out) ─────────────
typedef struct {
    double rate, pan, wet_send, fade;     // fade in seconds
} PlaySampleOpts;
static void play_sample(double t0, const float *buf, long buf_n,
                        double gain, PlaySampleOpts opt) {
    const double rate = opt.rate > 0 ? opt.rate : 1.0;
    double pan = opt.pan;
    if (pan > 1.0) pan = 1.0; if (pan < -1.0) pan = -1.0;
    const double wetSend = opt.wet_send;
    const long startI = (long)(t0 * SR);
    const long outLen = (long)(buf_n / rate);
    const long fadeN = (long)((opt.fade > 0 ? opt.fade : 0.015) * SR);
    for (long k = 0; k < outLen; k++) {
        const long di = startI + k;
        if (di < 0) continue;
        if (di >= N) break;
        const double sx = (double)k * rate;
        const long si = (long)sx;
        const double f = sx - si;
        if (si + 1 >= buf_n) break;
        double s = buf[si] + (buf[si + 1] - buf[si]) * f;
        if (k < fadeN) s *= (double)k / fadeN;
        if (outLen - k < fadeN) s *= (double)(outLen - k) / fadeN;
        const double v = s * gain;
        const double vL = v * (pan > 0 ? 1.0 - pan : 1.0);
        const double vR = v * (pan < 0 ? 1.0 + pan : 1.0);
        L[di] += (float)vL;
        R[di] += (float)vR;
        if (wetSend > 0.0) {
            WL[di] += (float)(vL * wetSend);
            WR[di] += (float)(vR * wetSend);
        }
    }
}

// ── playSampleSwept (exponential pitch sweep, optional bufOffset) ─────
typedef struct {
    double start_rate, end_rate;
    double pan, wet_send;
    double max_dur_ms;
    double fade;                  // seconds
    double buf_offset;            // seconds
} PlaySweptOpts;
static void play_sample_swept(double t0, const float *buf, long buf_n,
                              double gain, PlaySweptOpts opt) {
    const double startRate = opt.start_rate > 0 ? opt.start_rate : 1.0;
    const double endRate   = opt.end_rate   > 0 ? opt.end_rate   : startRate;
    double pan = opt.pan; if (pan > 1.0) pan = 1.0; if (pan < -1.0) pan = -1.0;
    const double wetSend = opt.wet_send;
    const double maxDurMs = opt.max_dur_ms > 0 ? opt.max_dur_ms : 180.0;
    const double fade = opt.fade > 0 ? opt.fade : 0.028;
    const long startI = (long)(t0 * SR);
    const long maxN = (long)(maxDurMs * SR / 1000.0);
    const long fadeN = (long)((fade < 1.0 ? fade : fade / 1000.0) * SR);
    double bufPos = opt.buf_offset > 0 ? opt.buf_offset * SR : 0.0;
    for (long k = 0; k < maxN; k++) {
        const long di = startI + k;
        if (di < 0) continue;
        if (di >= N) break;
        const long si = (long)bufPos;
        if (si + 1 >= buf_n) break;
        const double fr = bufPos - si;
        double s = buf[si] + (buf[si + 1] - buf[si]) * fr;
        if (k < fadeN) s *= (double)k / fadeN;
        if (maxN - k < fadeN) s *= (double)(maxN - k) / fadeN;
        const double v = s * gain;
        const double vL = v * (pan > 0 ? 1.0 - pan : 1.0);
        const double vR = v * (pan < 0 ? 1.0 + pan : 1.0);
        L[di] += (float)vL;
        R[di] += (float)vR;
        if (wetSend > 0.0) {
            WL[di] += (float)(vL * wetSend);
            WR[di] += (float)(vR * wetSend);
        }
        const double p = (double)k / maxN;
        const double rate = startRate * pow(endRate / startRate, p);
        bufPos += rate;
    }
}

// ── Schroeder reverb (4 combs + 2 allpasses), per-channel thread ──────
// Matches hellsine.mjs — shorter delays + hotter FB than sleephellsine.
// Tail ~2s, punchy hall (not cathedral). Pulls L/R apart for stereo width.
static const double COMB_L_D[4] = {0.0297, 0.0371, 0.0411, 0.0437};
static const double COMB_R_D[4] = {0.0307, 0.0381, 0.0421, 0.0447};
static const double COMB_FB = 0.84;
static const double AP_D[2] = {0.005, 0.0017};
static const double AP_FB = 0.5;

typedef struct {
    const float *in;
    float *out;
    const double *combs;
} ReverbJob;

static void *reverb_thread(void *arg) {
    ReverbJob *job = (ReverbJob*)arg;
    int combLens[4]; float *combLines[4]; int combIdx[4] = {0,0,0,0};
    int apLens[2];   float *apLines[2];   int apIdx[2]   = {0,0};
    for (int c = 0; c < 4; c++) {
        combLens[c] = (int)(job->combs[c] * SR);
        combLines[c] = (float*)calloc(combLens[c], sizeof(float));
    }
    for (int a = 0; a < 2; a++) {
        apLens[a] = (int)(AP_D[a] * SR);
        apLines[a] = (float*)calloc(apLens[a], sizeof(float));
    }
    for (long i = 0; i < N; i++) {
        const double in = job->in[i];
        double combOut = 0.0;
        for (int c = 0; c < 4; c++) {
            const int idx = combIdx[c];
            const double delayed = combLines[c][idx];
            combLines[c][idx] = (float)(in + delayed * COMB_FB);
            combIdx[c] = (idx + 1) % combLens[c];
            combOut += delayed;
        }
        combOut *= 0.25;
        double apOut = combOut;
        for (int a = 0; a < 2; a++) {
            const int idx = apIdx[a];
            const double delayed = apLines[a][idx];
            const double newS = apOut + delayed * AP_FB;
            apLines[a][idx] = (float)newS;
            apOut = delayed - newS * AP_FB;
            apIdx[a] = (idx + 1) % apLens[a];
        }
        job->out[i] = (float)apOut;
    }
    for (int c = 0; c < 4; c++) free(combLines[c]);
    for (int a = 0; a < 2; a++) free(apLines[a]);
    return NULL;
}

// forward decls used by render_full_track (defined further down)
static void alloc_buffers(double totalSec);
static void finalize_and_write(const char *path, double wet_mix);
static float *try_load_sample(const char *rel, long *n_out);

// ── flange (single feedforward comb, slow cos LFO) — used by blaster ──
static float *flange_buf(const float *in, long n, double depthMs, double rateHz, double mix) {
    float *out = (float*)calloc(n, sizeof(float));
    const double dMax = depthMs * SR / 1000.0;
    double peak = 0.0;
    for (long i = 0; i < n; i++) {
        const double lfo = (1.0 - cos(2.0 * M_PI * rateHz * i / SR)) * 0.5;
        const double d = dMax * lfo + 1.0;
        const long di = (long)d;
        const double f = d - di;
        const long i0 = i - di, i1 = i0 - 1;
        const double s0 = i0 >= 0 ? in[i0] : 0;
        const double s1 = i1 >= 0 ? in[i1] : 0;
        const double delayed = s0 * (1.0 - f) + s1 * f;
        out[i] = (float)(in[i] + mix * delayed);
        const double a = fabs(out[i]);
        if (a > peak) peak = a;
    }
    if (peak > 1.0) for (long i = 0; i < n; i++) out[i] = (float)(out[i] / peak);
    return out;
}

// ── humEager — personality-bearing humanizer for MELODIC content ──────
static inline double humEager(double amt) {
    amt *= HUMANIZE_MULT;
    const double r = rng();
    if (r < 0.58) return -amt * (0.40 + rng() * 0.70);
    if (r < 0.80) return (rng() * 2.0 - 1.0) * amt * 0.30;
    if (r < 0.94) return amt * (1.0 + rng() * 1.4);
    return amt * (2.2 + rng() * 2.0);
}
// ── humLate — opposite of humEager, biased to delay notes (lazy / sticky)
//   58 % push back (+amt * 0.4-1.1) — behind the beat
//   22 % tight on the grid
//   14 % really late
//    6 % very late (dragged feel)
static inline double humLate(double amt) {
    amt *= HUMANIZE_MULT;
    const double r = rng();
    if (r < 0.58) return  amt * (0.40 + rng() * 0.70);
    if (r < 0.80) return (rng() * 2.0 - 1.0) * amt * 0.30;
    if (r < 0.94) return  amt * (1.0 + rng() * 1.4);
    return amt * (2.2 + rng() * 2.0);
}

// ── stickySwing — push notes that land on the 8th-note offbeat slightly
// later, so the bar develops a laid-back swing feel. beatPos is the note's
// position in beats (0..bars*4). Returns a time offset in seconds.
static inline double sticky_swing(double beatPos) {
    const double frac = beatPos - floor(beatPos);
    if (frac > 0.40 && frac < 0.60) return (0.667 - 0.5) * SPB_G * HUMANIZE_MULT;
    if ((frac > 0.65 && frac < 0.85) || (frac > 0.15 && frac < 0.35))
        return 0.04 * SPB_G * HUMANIZE_MULT;
    return 0.0;
}

// ── chord table (matches hellsine.mjs CHORD{}) ────────────────────────
typedef struct { const char *name; int root; int q3; int q5; } ChordH;
static const ChordH HC[] = {
    { "Dm", 38, 3, 7 },
    { "Bb", 46, 4, 7 },
    { "F",  41, 4, 7 },
    { "C",  48, 4, 7 },
    { "Gm", 43, 3, 7 },
    { "A",  45, 4, 7 },
};
static const ChordH *hchord(const char *name) {
    for (size_t i = 0; i < sizeof(HC)/sizeof(HC[0]); i++) {
        if (!strcmp(HC[i].name, name)) return &HC[i];
    }
    return &HC[0];
}

// ── PLAN (matches hellsine.mjs PLAN[]) ────────────────────────────────
typedef struct {
    const char *name;
    int bars;
    const char *kick;        // "none" / "halftime" / "halfhard" / "pulse" / "fade"
    double drive;            // 0 / HELL / HELL*… (filled at runtime from a multiplier)
    double drive_mul;        // multiplier on HELL (so we can defer × until BPM known)
    const char *chords[16];
    int nchords;
    const char *theme;       // "soft" / "brass" / "bsoft" / "frag" / "dissolve"
    int transpose;
} SectionH;

#define SECN 6
static SectionH PLAN_H[SECN] = {
    { "overture",  12, "none",     0,    0.0,
      {"Dm","Dm","Bb","Bb","F","F","C","A","Dm","Bb","F","C"}, 12, "soft", 0 },
    { "statement", 24, "halftime", 0,    1.0,
      {"Dm","Dm","Bb","Bb","F","F","C","C"}, 8, "brass", 0 },
    { "bridge",    24, "pulse",    0,    0.7,
      {"Gm","Gm","Dm","Dm","C","C","Bb","A"}, 8, "bsoft", 0 },
    { "develop",   24, "halftime", 0,    0.75,
      {"Dm","Dm","F","F","Gm","Gm","A","A","Bb","Bb","C","C","Dm","A","Dm","A"}, 16, "frag", 0 },
    { "climax",    24, "halfhard", 0,    1.3,
      {"Dm","Dm","Bb","Bb","F","F","C","C"}, 8, "brass", 14 },
    { "coda",      16, "fade",     0,    0.6,
      {"Dm","Dm","Gm","Gm","C","C","Dm","Dm"}, 8, "dissolve", 0 },
};

// ── THEME + BTHEME + COUNTER (matches hellsine.mjs) ───────────────────
#define M2 -2
typedef struct { int off; double beats; } Note;
static const Note THEME[]   = {
    {-5,1},{0,1.5},{3,1.0},{7,1},{7,1},{8,1},{7,1},{5,1},
    {3,1.5},{2,1},{0,1},{0,2},{M2,1},{-5,1},
    {0,1},{3,1},{7,1},{10,1},{12,2},{10,1},{8,1},
    {7,2},{5,1},{3,1},{0,4},
};
static const int  THEME_N = sizeof(THEME)/sizeof(THEME[0]);
static const Note BTHEME[]  = {
    {-5,2},{M2,1},{0,1},{3,2},{2,1},{0,1},{M2,2},{0,1},{2,1},
    {0,4},{0,1},{2,1},{3,1},{5,1},{7,2},{5,1},{3,1},
    {2,2},{0,1},{M2,1},{0,4},
};
static const int  BTHEME_N = sizeof(BTHEME)/sizeof(BTHEME[0]);
static const Note COUNTER[] = {
    {-9,2},{-5,2},{-5,2},{-9,2},{-12,1},{-9,1},{-4,2},
    {-9,1},{-7,1},{-5,2},{-5,2},{-2,2},{-9,1},{-5,1},{-2,1},{-5,1},
    {-7,1},{-10,1},{-2,2},{-10,2},{-7,1},{-5,1},
};
static const int  COUNTER_N = sizeof(COUNTER)/sizeof(COUNTER[0]);

// SIMPLE_INTRO — used for statement loop 0 only. Just two sustained
// chord tones (D4 → A4) so the melody enters spacious instead of
// launching straight into the busy "call" arpeggio.
static const Note SIMPLE_INTRO[] = {
    {0, 4},   // D4 sustained 4 beats
    {7, 4},   // A4 sustained 4 beats
};
static const int SIMPLE_INTRO_N = sizeof(SIMPLE_INTRO)/sizeof(SIMPLE_INTRO[0]);
static const int  ROOT_MEL_H = 62;

// ── strategy theme transforms (subset used by ULTIMATE_MAP) ───────────
// D natural minor reference: D E F G A Bb C → 0 2 3 5 7 8 10
static const int DMIN[7] = {0, 2, 3, 5, 7, 8, 10};
static int in_dmin(int o) {
    const int m = ((o % 12) + 12) % 12;
    for (int i = 0; i < 7; i++) if (DMIN[i] == m) return 1;
    return 0;
}
static int scale_step(int o, int dir) {
    int x = o + dir;
    for (int i = 0; i < 12 && !in_dmin(x); i++) x += dir;
    return x;
}

#define MAX_THEME 128
typedef struct { Note notes[MAX_THEME]; int n; } NoteSeq;

static void strat_none(const Note *in, int nIn, NoteSeq *out) {
    out->n = nIn;
    for (int i = 0; i < nIn; i++) out->notes[i] = in[i];
}

static void strat_ornament(const Note *in, int nIn, NoteSeq *out) {
    out->n = 0;
    for (int i = 0; i < nIn; i++) {
        const int o = in[i].off; const double d = in[i].beats;
        const int nxt = (i + 1 < nIn) ? in[i + 1].off : o;
        if (d >= 1) {
            out->notes[out->n++] = (Note){o, d * 0.75};
            out->notes[out->n++] = (Note){scale_step(o, nxt >= o ? 1 : -1), d * 0.25};
        } else {
            out->notes[out->n++] = (Note){o, d};
        }
    }
}

static void strat_arpeggiate(const Note *in, int nIn, NoteSeq *out) {
    out->n = 0;
    for (int i = 0; i < nIn; i++) {
        const int o = in[i].off; const double d = in[i].beats;
        const int b = scale_step(scale_step(o, 1), 1);
        const int c = scale_step(scale_step(b, 1), 1);
        const int seq[4] = {o, b, c, b};
        const double dd = d / 4.0;
        for (int k = 0; k < 4; k++) {
            out->notes[out->n++] = (Note){seq[k], dd};
        }
    }
}

static void strat_sixteenths(const Note *in, int nIn, NoteSeq *out) {
    out->n = 0;
    for (int i = 0; i < nIn; i++) {
        const int o = in[i].off; const double d = in[i].beats;
        int n = (int)(d * 4.0 + 0.5); if (n < 1) n = 1;
        const double dd = d / n;
        for (int k = 0; k < n; k++) {
            const int off = (k % 2) ? scale_step(o, 1) : o;
            out->notes[out->n++] = (Note){off, dd};
        }
    }
}

static void strat_octave_skips(const Note *in, int nIn, NoteSeq *out) {
    out->n = 0;
    int jumpAt = -10;
    for (int i = 0; i < nIn; i++) {
        const int o = in[i].off; const double d = in[i].beats;
        if (o >= 5 && i - jumpAt >= 4) {
            out->notes[out->n++] = (Note){o + 12, d};
            jumpAt = i;
        } else if (i == jumpAt + 1) {
            out->notes[out->n++] = (Note){o + 7, d};
        } else if (i == jumpAt + 2) {
            out->notes[out->n++] = (Note){o + 3, d};
        } else {
            out->notes[out->n++] = (Note){o, d};
        }
    }
}

typedef void (*StratFn)(const Note*, int, NoteSeq*);
static StratFn strat_by_name(const char *name) {
    if (!strcmp(name, "ornament"))     return strat_ornament;
    if (!strcmp(name, "arpeggiate"))   return strat_arpeggiate;
    if (!strcmp(name, "sixteenths"))   return strat_sixteenths;
    if (!strcmp(name, "octave-skips")) return strat_octave_skips;
    return strat_none;
}

// ULTIMATE_MAP — per-section strategy rotation
typedef struct { const char *section; const char *strats[4]; int n; } UltMap;
static const UltMap ULT_MAP[] = {
    { "overture",  {"none"},                              1 },
    // Statement loop 0 = ornament (compositional glitch via note
    // rearrangement — musical, not DSP chops); loops 1+ = straight
    // THEME for a regular, settled feel. (@jeffrey 2026-05-26 "more
    // musical glitchiness / less super effected / compositional strat
    // melody shifting")
    { "statement", {"ornament", "none", "none"},          3 },
    { "develop",   {"sixteenths"},                        1 },
    // Climax loop 0 = octave-skips (the melody jumps registers as
    // the rock-out hits), then settles. The metal-guitar voice
    // benefits from the more aggressive note-rearrangement.
    { "climax",    {"octave-skips", "none", "none"},      3 },
    { "coda",      {"none"},                              1 },
};
static StratFn ult_theme_strat(const char *sec_name, int lp) {
    for (size_t i = 0; i < sizeof(ULT_MAP)/sizeof(ULT_MAP[0]); i++) {
        if (!strcmp(ULT_MAP[i].section, sec_name)) {
            const int idx = lp < ULT_MAP[i].n ? lp : ULT_MAP[i].n - 1;
            return strat_by_name(ULT_MAP[i].strats[idx]);
        }
    }
    return strat_none;
}

// ── partial-set palettes ──────────────────────────────────────────────
static const double PAD_PARTS_DEFAULT[5][2] = {
    {1,1},{2,0.45},{3,0.22},{4,0.12},{5,0.06}
};
static const double PAD_PARTS_SPARKLE[3][2] = {
    {1,1},{2,0.25},{3,0.10}
};
static const double PAD_PARTS_BODY[4][2] = {
    {1,1},{2,0.35},{3,0.14},{4,0.06}
};
static const double BRASS_PARTS[7][2] = {
    {1,1},{2,0.6},{3,0.45},{4,0.28},{5,0.2},{6,0.12},{7,0.07}
};
// ULTIMATE brass partials — POWERSINE: fundamental dominant, sparse
// upper harmonics for cut, mellower upper partials so the lead doesn't
// read as harsh / annoying. Drive does the saturation work.
static const double BRASS_PARTS_ULT[7][2] = {
    {1, 1.00}, {2, 0.40}, {3, 0.18}, {4, 0.07}, {5, 0.03}, {6, 0.01}, {7, 0.005}
};
static const double BRASS_OCT_DOUBLE_PARTS[3][2] = {
    {1,1},{2,0.4},{3,0.2}
};
static const double SOFT_THEME_PARTS[4][2] = {
    {1,1},{2,0.4},{3,0.22},{4,0.1}
};
static const double FRAG_PARTS[5][2] = {
    {1,1},{2,0.6},{3,0.4},{4,0.24},{5,0.16}
};
static const double COUNTER_PARTS[5][2] = {
    {1,1},{2,0.55},{3,0.3},{4,0.14},{5,0.06}
};
static const double DROOSE_PAD_PARTS[4][2] = {     // dissolve drone (4 partials)
    {1,1},{2,0.35},{3,0.18},{5,0.06}
};
static const double DROOSE_LOW_PARTS[2][2] = {
    {1,1},{2,0.3}
};
// SISTER SUB — wider, low-octave stereo sub-bass partials.
// Fundamental + small 2nd-harmonic glue. Used with pan_spread for width.
static const double SISTER_SUB_PARTS[3][2] = {
    {1, 1.00}, {2, 0.32}, {3, 0.08}
};

// ── brass with slide (portamento) — per-sample frequency glide ──
// from_midi → to_midi over slide_dur seconds, then settle on to_midi.
// Phase-accumulated additive partials (matches voice_render's spectrum)
// + tanh drive + click-spatial routing handled at the call site.
static void brass_slide_render(double t0, double dur, double from_midi, double to_midi,
                                double slide_dur, double gain, VoiceOpts opt) {
    const double (*parts)[2] = opt.parts ? opt.parts : VOICE_DEFAULT_PARTS;
    const int nP = opt.parts ? opt.n_parts : 6;
    double amps = 0.0;
    for (int i = 0; i < nP; i++) amps += parts[i][1];
    const double norm = (opt.gain_extra > 0 ? opt.gain_extra : 1.0) / amps;
    const double atk = opt.atk > 0 ? opt.atk : 0.05;
    const double rel = opt.rel > 0 ? opt.rel : 0.18;
    const double drive = opt.drive > 0 ? opt.drive : 1.0;
    const double pan = opt.pan;

    const double fFrom = m2f(from_midi);
    const double fTo = m2f(to_midi);
    const double sd = slide_dur > 0 ? slide_dur : 0.001;
    double phs[32] = {0};
    if (nP > 32) return;

    // TAIL WARBLE — fast LFO frequency wobble on the last 45 % of
    // each note. More pronounced in the OPENING (t0 < 30 s) for the
    // user's "fishy / oscillating air" character; gentle elsewhere.
    // (@jeffrey "lead sine especially in the beginning had a bit of
    //  warble / fast warble or flange on the tail of notes / some
    //  oscillating air going through it like it's a little fishy")
    const int isOpening = (t0 < 30.0);
    const double tailStart  = dur * 0.55;
    const double warbleHz   = 11.0;
    const double warbleDepth = isOpening ? 0.014 : 0.006;
    long iStart = (long)(t0 * SR); if (iStart < 0) iStart = 0;
    long iEnd = (long)((t0 + dur + rel) * SR + 1); if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk; if (env > 1.0) env = 1.0;
        if (lt > dur - rel) {
            double rEnv = (dur - lt) / rel;
            if (rEnv < 0.0) rEnv = 0.0;
            env *= rEnv;
        }
        // Logarithmic frequency glide (constant semitone-per-sec rate)
        double fNow;
        if (lt < sd) {
            const double p = lt / sd;
            fNow = fFrom * pow(fTo / fFrom, p);
        } else {
            fNow = fTo;
        }
        // Apply tail warble: LFO depth ramps in across the tail window
        if (lt > tailStart && dur > tailStart) {
            double tailFr = (lt - tailStart) / (dur - tailStart);
            if (tailFr > 1.0) tailFr = 1.0;
            const double lfo = sin(TAU * warbleHz * lt);
            fNow *= 1.0 + lfo * warbleDepth * tailFr;
        }
        double x = 0.0;
        for (int k = 0; k < nP; k++) {
            const double r = parts[k][0];
            const double a = parts[k][1];
            phs[k] += (fNow * r) / SR;
            if (phs[k] >= 1.0) phs[k] -= 1.0;
            x += a * sin(TAU * phs[k]);
        }
        x = tanh(x * norm * drive);
        if (env <= 0.0) continue;
        const double v = x * env * gain;
        L[i] += (float)(v * (pan > 0 ? 1.0 - pan : 1.0));
        R[i] += (float)(v * (pan < 0 ? 1.0 + pan : 1.0));
        if (opt.wet_send > 0.0) {
            WL[i] += (float)(v * opt.wet_send * (pan > 0 ? 1.0 - pan : 1.0));
            WR[i] += (float)(v * opt.wet_send * (pan < 0 ? 1.0 + pan : 1.0));
        }
    }
}

// ── METAL ELECTRIC-GUITAR lead — power-chord stacked overdriven
// sine→square voice. Root + perfect 5th + octave stack per note,
// heavy odd-harmonic series, hard tanh drive, fast pick-attack,
// slight pitch bend on entry. Used when LEAD_KIND==POWERSINE and
// t >= CLIMAX_START so the back half "rocks out" (@jeffrey
// "powersine becomes electric guitar at 2:00 break / metal style").
static void metal_guitar_lead_render(double t0, double dur, double from_midi,
                                      double to_midi, double slide_dur,
                                      double gain) {
    const double atk = 0.003;
    const double rel = 0.070;
    const double drive = 5.20;        // wall-of-distortion
    // Power-chord stack: root, +7 st (perfect 5th), +12 st (octave)
    static const double STACK_ST[3] = { 0.0, +7.0, +12.0 };
    // Slight detune per stack + L/R spread for stereo width
    static const double STACK_DET[3]  = { -3,  +1,  -2 };  // cents
    static const double STACK_PAN[3]  = { -0.45,  0.0,  +0.45 };
    static const double STACK_GAIN[3] = { 1.00, 0.78, 0.55 };
    const double fFrom = m2f(from_midi);
    const double fTo   = m2f(to_midi);
    const double sd    = slide_dur > 0 ? slide_dur : 0.001;
    double ph[3] = {0,0,0};
    long iS = (long)(t0 * SR); if (iS < 0) iS = 0;
    long iE = (long)((t0 + dur + rel) * SR + 1); if (iE > N) iE = N;
    for (long i = iS; i < iE; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk; if (env > 1.0) env = 1.0;
        if (lt > dur - rel) {
            double rE = (dur - lt) / rel;
            if (rE < 0.0) rE = 0.0;
            env *= rE;
        }
        if (env <= 0.0) continue;
        // Frequency glide + small upward pick-bend at entry (3 cents
        // over first 25 ms) for guitar-attack character
        double fNow;
        if (lt < sd) {
            const double f = lt / sd;
            fNow = exp(log(fFrom) + f * (log(fTo) - log(fFrom)));
        } else {
            fNow = fTo;
        }
        const double pickBend = (lt < 0.025) ? (1.0 - 0.003 * (1.0 - lt / 0.025)) : 1.0;
        fNow *= pickBend;
        const double duck = (i >= 0 && i < N) ? DUCK[i] : 1.0;
        double sumL = 0, sumR = 0;
        for (int v = 0; v < 3; v++) {
            const double fStack = fNow * pow(2.0, STACK_ST[v] / 12.0)
                                       * pow(2.0, STACK_DET[v] / 1200.0);
            ph[v] += TAU * fStack / SR;
            if (ph[v] > TAU) ph[v] -= TAU;
            // Strong odd-harmonic stack (1,3,5,7,9) → square-ish for metal
            const double h1 = sin(ph[v]);
            const double h3 = sin(3.0 * ph[v]) / 3.0;
            const double h5 = sin(5.0 * ph[v]) / 5.0;
            const double h7 = sin(7.0 * ph[v]) / 7.0;
            const double h9 = sin(9.0 * ph[v]) / 9.0;
            double mix = (h1 + h3 + h5 + h7 + h9) * 0.78;
            mix = tanh(mix * drive);
            // 2nd-order harmonic asymmetry for tube-amp warmth
            mix = mix * 0.92 + mix * fabs(mix) * 0.08;
            const double w = STACK_GAIN[v];
            const double pL = (STACK_PAN[v] > 0) ? (1.0 - STACK_PAN[v]) : 1.0;
            const double pR = (STACK_PAN[v] < 0) ? (1.0 + STACK_PAN[v]) : 1.0;
            sumL += mix * w * pL;
            sumR += mix * w * pR;
        }
        // Normalize stack sum (~2.33) and apply env+gain+duck
        const double vv = env * gain * duck * 0.43;
        L[i]  += (float)(sumL * vv);
        R[i]  += (float)(sumR * vv);
        SL[i] += (float)(sumL * vv * 0.45);
        SR_[i]+= (float)(sumR * vv * 0.45);
    }
}

// ── POWERSINE trance lead — 7 detuned sine voices, additive
// upper-harmonic stack per voice, tanh drive for crunch. Big stereo
// spread + DUCK sidechain so the lead pumps with the kick.
// Replaces brass when LEAD_KIND == LEAD_POWERSINE.
static void powersine_lead_render(double t0, double dur, double from_midi,
                                   double to_midi, double slide_dur,
                                   double gain) {
    const double atk = 0.008;
    const double rel = 0.080;
    const double drive = 2.30;            // hot saturation = trance edge
    static const double DETUNE_CENTS[7] = {-15, -10, -5, 0, +5, +10, +15};
    static const double VPAN[7]         = {-0.65, -0.40, -0.18, 0.0, +0.18, +0.40, +0.65};
    const double fFrom = m2f(from_midi);
    const double fTo   = m2f(to_midi);
    const double sd    = slide_dur > 0 ? slide_dur : 0.001;
    double ph[7] = {0,0,0,0,0,0,0};
    long iS = (long)(t0 * SR); if (iS < 0) iS = 0;
    long iE = (long)((t0 + dur + rel) * SR + 1); if (iE > N) iE = N;
    for (long i = iS; i < iE; i++) {
        const double lt = (double)i / SR - t0;
        double env = lt / atk; if (env > 1.0) env = 1.0;
        if (lt > dur - rel) {
            double rE = (dur - lt) / rel;
            if (rE < 0.0) rE = 0.0;
            env *= rE;
        }
        if (env <= 0.0) continue;
        // Logarithmic frequency glide
        double fNow;
        if (lt < sd) {
            const double f = lt / sd;
            const double lnFrom = log(fFrom), lnTo = log(fTo);
            fNow = exp(lnFrom + f * (lnTo - lnFrom));
        } else {
            fNow = fTo;
        }
        // Sidechain duck from DUCK bus
        const double duck = (i >= 0 && i < N) ? DUCK[i] : 1.0;
        // PITCH-DIVE INTO THE KICK: when DUCK is low (kick just hit),
        // also drop the powersine pitch ~7 st (perfect fifth down) and
        // recover with the duck envelope. Creates the "growl into the
        // kick" interaction (@jeffrey "pitches into the kick").
        const double pitchDip = 0.55 + 0.45 * duck;   // 0.55 at kick (~fifth down), 1.0 idle
        double sumL = 0, sumR = 0;
        for (int v = 0; v < 7; v++) {
            const double fv = fNow * pitchDip * pow(2.0, DETUNE_CENTS[v] / 1200.0);
            ph[v] += TAU * fv / SR;
            if (ph[v] > TAU) ph[v] -= TAU;
            // Powersine: fundamental + odd-harmonic upper stack
            const double s1 = sin(ph[v]);
            const double s2 = sin(2.0 * ph[v]) * 0.55;
            const double s3 = sin(3.0 * ph[v]) * 0.30;
            const double s4 = sin(4.0 * ph[v]) * 0.18;
            const double s5 = sin(5.0 * ph[v]) * 0.10;
            double mix = (s1 + s2 + s3 + s4 + s5) * 0.42;
            mix = tanh(mix * drive);
            const double w = 1.0 / 7.0;
            const double pL = (VPAN[v] > 0) ? (1.0 - VPAN[v]) : 1.0;
            const double pR = (VPAN[v] < 0) ? (1.0 + VPAN[v]) : 1.0;
            sumL += mix * w * pL;
            sumR += mix * w * pR;
        }
        const double vv = env * gain * duck;
        L[i]  += (float)(sumL * vv);
        R[i]  += (float)(sumR * vv);
        SL[i] += (float)(sumL * vv * 0.30);
        SR_[i]+= (float)(sumR * vv * 0.30);
    }
}

// ── helpers: layTheme + layCounter (matches hellsine.mjs closures) ────
static void lay_theme(const Note *notes, int n, double base_gain,
                      int brass, int reg_off, double t0, int tr) {
    double beatPos = 0.0;
    int prevMidi = -1;     // for portamento slide between brass notes
    for (int i = 0; i < n; i++) {
        const double tN = t0 + beatPos * SPB_G;
        const double midi = ROOT_MEL_H + notes[i].off + tr + reg_off;
        const double dur = notes[i].beats * SPB_G * 0.96;
        if (brass) {
            // LEAD — SLIDING + LAZY-SWUNG POWERSINE.
            // Lazy timing (humLate) + sticky-swing offbeat push give the
            // melody a behind-the-beat groove. Mellower partials + softer
            // drive reduce harshness. Single continuous sliding line.
            VoiceOpts vo = {0};
            vo.parts = ULTIMATE ? BRASS_PARTS_ULT : BRASS_PARTS;
            vo.n_parts = 7;
            vo.atk = 0.012;        // a touch slower attack — less harsh snap
            vo.rel = 0.10;
            vo.vibR = 5.2; vo.vibD = 0.006;
            vo.drive = 1.40;       // softer drive — less annoying
            vo.wet_send = 0;       // skip cathedral; SPATIAL send below
            // SWING + LAZY timing on the lead — push behind the beat,
            // accent the 8th-note offbeats with a sticky swing delay.
            const double swing = sticky_swing(beatPos);
            const double lazy  = humLate(0.018);
            const double tLead = tN + swing + lazy;
            // PORTAMENTO SLIDE between notes — single sliding line.
            {
                const double fromM = (prevMidi < 0) ? midi : (double)prevMidi;
                const double intvl = fabs(midi - fromM);
                const double slideDur = (prevMidi < 0) ? 0.0
                    : fmin(0.140, 0.045 + intvl * 0.012);
                // Climax electric-guitar takeover applies to BOTH lead
                // kinds — brass version also rocks out at 2:00+ via
                // the sampled guitar stack (@jeffrey "brass in the end
                // was supposed to be electric guitar now").
                if (tLead >= 110.0) {
                        // SAMPLED ELECTRIC GUITAR — Chem freesound 31933
                        // D2 power chord pitch-shifted per note. Falls
                        // back to synth metal_guitar if sample missing.
                        // (@jeffrey "actual freesound sampled electric
                        // guitar for the last part")
                        if (electric_guitar_buf && electric_guitar_n > 0) {
                            // OCTAVE-FOLDED to stay in low octaves no
                            // matter what THEME pitch the lead is on.
                            double tgtMidi = midi - 36;
                            while (tgtMidi > ELECTRIC_GUITAR_ROOT_MIDI)
                                tgtMidi -= 12.0;
                            // Extra slowdown factor (×0.75) → even longer
                            // sustain + deeper pitch. Stack a 2nd HEAVY
                            // chord sample an octave below, panned right,
                            // for THICKNESS. (@jeffrey "thicker / slow it
                            // down too / find a different guitar")
                            const double slowMul = 0.75;
                            // Layer config: [sample_buf, sample_n, oct_offset, gain, pan]
                            struct { float *buf; long n; double octOff; double gain; double pan; } layers[2] = {
                                { electric_guitar_buf,       electric_guitar_n,       0.0, 1.10, -0.20 },
                                { electric_guitar_heavy_buf, electric_guitar_heavy_n,-12.0, 0.95, +0.20 },
                            };
                            for (int gl = 0; gl < 2; gl++) {
                                if (!layers[gl].buf || layers[gl].n <= 0) continue;
                                const double layerMidi = tgtMidi + layers[gl].octOff;
                                const double rate = pow(2.0,
                                    (layerMidi - ELECTRIC_GUITAR_ROOT_MIDI) / 12.0) * slowMul;
                                const long iS     = (long)(tLead * SR);
                                const long outLen = (long)((dur + 0.04) * SR);
                                const long fadeIn  = (long)(0.006 * SR);
                                const long fadeOut = (long)(0.060 * SR);
                                const double gtGain = base_gain * layers[gl].gain;
                                const double pan = layers[gl].pan;
                                const double pL  = (pan > 0) ? (1.0 - pan) : 1.0;
                                const double pR  = (pan < 0) ? (1.0 + pan) : 1.0;
                                for (long w = 0; w < outLen; w++) {
                                    const long oi = iS + w;
                                    if (oi < 0 || oi >= N) continue;
                                    const double readPos = (double)w * rate;
                                    if (readPos + 1 >= (double)layers[gl].n) break;
                                    const long ri = (long)readPos;
                                    const double frac = readPos - ri;
                                    double s = layers[gl].buf[ri] * (1.0 - frac)
                                             + layers[gl].buf[ri + 1] * frac;
                                    double env = 1.0;
                                    if (w < fadeIn)              env  = (double)w / fadeIn;
                                    if (outLen - w < fadeOut)    env *= (double)(outLen - w) / fadeOut;
                                    const double duck = (oi >= 0 && oi < N) ? DUCK[oi] : 1.0;
                                    const double v = s * env * gtGain * duck;
                                    L[oi]  += (float)(v * pL);
                                    R[oi]  += (float)(v * pR);
                                    SL[oi] += (float)(v * 0.30);
                                    SR_[oi]+= (float)(v * 0.30);
                                }
                            }
                        } else {
                            metal_guitar_lead_render(tLead, dur, fromM - 12, midi - 12,
                                                     slideDur, base_gain * 0.78);
                        }
                        // SUPER-SINE @ 2:14 onward — bright powersine
                        // layered ABOVE the sampled guitar for the
                        // last bars of the climax. Starts at +12 st
                        // and ramps UP to +24 st across the final bars
                        // for a brightening "super sine" climb.
                        // (@jeffrey "bring up the powersine an extra
                        //  octave around 2:14 / for the last few bars
                        //  / super sine!")
                        if (tLead >= 134.0) {
                            // Octave ramp: +12 at 134s → +24 at 145s+
                            const double rampFr = (tLead - 134.0) / 11.0;
                            const double cl = (rampFr < 1.0) ? rampFr : 1.0;
                            const double octShift = 12.0 + 12.0 * cl;
                            const double gn = base_gain * (0.38 + 0.32 * cl);
                            powersine_lead_render(tLead, dur,
                                                  fromM + octShift, midi + octShift,
                                                  slideDur, gn);
                        }
                        prevMidi = (int)midi;
                        beatPos += notes[i].beats;
                        continue;
                }
                // Pre-climax (tLead < 110): powersine or brass lead.
                if (LEAD_KIND == LEAD_POWERSINE) {
                    const int isOpening = (tLead < 32.0);
                    const double openGain = isOpening ? 1.10 : 0.95;
                    powersine_lead_render(tLead, dur, fromM, midi, slideDur,
                                          base_gain * 0.62 * openGain);
                    const double octGain = isOpening ? 0.38 : 0.24;
                    powersine_lead_render(tLead + hum(0.004), dur,
                                          fromM + 12, midi + 12, slideDur,
                                          base_gain * 0.62 * octGain);
                } else {
                    brass_slide_render(tLead, dur, fromM, midi, slideDur,
                                       base_gain * 0.62, vo);
                }
            }
            // SPATIAL send for the brass — also reduced
            {
                const double f = m2f(midi);
                double ph = 0.0;
                long iS = (long)(tN * SR);
                if (iS < 0) iS = 0;
                long iE = (long)((tN + dur + 0.1) * SR);
                if (iE > N) iE = N;
                for (long si = iS; si < iE; si++) {
                    const double lt = (double)si / SR - tN;
                    double env = lt / 0.004;
                    if (env > 1.0) env = 1.0;
                    if (lt > dur - 0.08) {
                        double rE = (dur - lt) / 0.08;
                        if (rE < 0) rE = 0;
                        env *= rE;
                    }
                    if (env <= 0) continue;
                    ph += f / SR; if (ph >= 1.0) ph -= 1.0;
                    const double v = sin(TAU * ph) * env * base_gain * 0.55;
                    SL[si]  += (float)v;
                    SR_[si] += (float)v;
                }
            }

            // METALLIC TRANSIENT CLICK — softer now (less annoying)
            {
                const long ti = (long)((tN + hum(0.002)) * SR);
                const long te = ti + (long)(0.006 * SR);   // 6 ms (was 8)
                const double clickG = base_gain * 0.15;     // 0.45 → 0.15 — way quieter
                uint32_t cs = (uint32_t)(ti * 2654435761u + 1);
                for (long ci = ti; ci < te && ci < N; ci++) {
                    if (ci < 0) continue;
                    const double lt = (double)(ci - ti) / SR;
                    cs ^= cs << 13; cs ^= cs >> 17; cs ^= cs << 5;
                    const double noise = ((double)cs / 4294967296.0) * 2.0 - 1.0;
                    const double env = exp(-lt / 0.0015);
                    const double v = noise * env * clickG;
                    L[ci] += (float)(v * 0.92);
                    R[ci] += (float)v;
                    SL[ci] += (float)(v * 0.55);     // click → spatial resonator
                    SR_[ci] += (float)(v * 0.55);
                }
            }

            // BACKGROUND — natural brass sample BURIED, heavy wet for room.
            if (brass_sample_buf) {
                const double rate = pow(2.0, (midi - BRASS_SAMPLE_MIDI) / 12.0);
                PlaySampleOpts po = {0};
                po.rate = rate;
                po.pan = 0.0;
                po.wet_send = 0.95;
                po.fade = 0.040;
                play_sample(tN + hum(0.002), brass_sample_buf,
                            brass_sample_n, base_gain * 0.12, po);
            }
            prevMidi = (int)midi;
        } else {
            VoiceOpts vo = {0};
            vo.parts = SOFT_THEME_PARTS; vo.n_parts = 4;
            vo.atk = 0.12; vo.rel = 0.22;
            vo.vibR = 4.6; vo.vibD = 0.005;
            voice_render(tN + humEager(0.022), dur, midi, base_gain, vo);
        }
        beatPos += notes[i].beats;
    }
}

static void lay_counter(const Note *notes, int n, double base_gain,
                        double t0, int tr) {
    double beatPos = 0.0;
    for (int i = 0; i < n; i++) {
        const double tN = t0 + beatPos * SPB_G;
        VoiceOpts vo = {0};
        vo.parts = COUNTER_PARTS; vo.n_parts = 5;
        vo.atk = 0.045; vo.rel = 0.18;
        vo.vibR = 4.8; vo.vibD = 0.006;
        vo.drive = 1.05; vo.pan = -0.28;
        voice_render(tN + humEager(0.026), notes[i].beats * SPB_G * 0.98,
                     ROOT_MEL_H + notes[i].off + tr, base_gain, vo);
        beatPos += notes[i].beats;
    }
}

// ── full track render — non-ULTIMATE, no strategies (the canonical default) ──
static void render_full_track(void) {
    // resolve drives now that HELL is final
    for (int s = 0; s < SECN; s++) PLAN_H[s].drive = HELL * PLAN_H[s].drive_mul;
    int totalBars = 0;
    for (int s = 0; s < SECN; s++) totalBars += PLAN_H[s].bars;
    TOTAL_SEC_G = totalBars * SPBAR_G + TAIL_SEC;
    alloc_buffers(TOTAL_SEC_G);
    report("hellsine.c · full track · %d bars · %.1fs (%.2f min) · BPM=%.1f",
           totalBars, TOTAL_SEC_G, TOTAL_SEC_G / 60.0, BPM);

    int bar = 0;
    n_section_ranges = 0;
    n_kick_events = 0;
    // Load the sampled electric guitar (CC-BY Chem freesound 31933 — D2
    // power chord, ~9.5s sustained) once for the climax lead.
    {
        long egn = 0;
        float *eg = try_load_sample("electric-guitar-chord.wav", &egn);
        if (eg) {
            free(electric_guitar_buf);
            electric_guitar_buf = eg;
            electric_guitar_n = egn;
            report("→ electric-guitar · Chem 31933 loaded (%.2fs)",
                   (double)egn / SR);
        }
        long ehn = 0;
        float *eh = try_load_sample("electric-guitar-chord-heavy.wav", &ehn);
        if (eh) {
            free(electric_guitar_heavy_buf);
            electric_guitar_heavy_buf = eh;
            electric_guitar_heavy_n = ehn;
            report("→ electric-guitar HEAVY · Ax_Grinder 242803 loaded (%.2fs)",
                   (double)ehn / SR);
        }
    }

    // Load the natural brass sample once (used by lay_theme brass branch).
    {
        long bn = 0;
        brass_sample_buf = try_load_sample("flugelhorn-asharp.wav", &bn);
        brass_sample_n = bn;
        if (brass_sample_buf) report("→ brass sample · flugelhorn-asharp.wav (%ld samples)", bn);
    }
    // Compute AC_STAMP_TIME so kick_render (ULTIMATE shortHigh check) sees it
    int climaxBarOffset = 0;
    for (int s = 0; s < SECN; s++) {
        if (!strcmp(PLAN_H[s].name, "climax")) break;
        climaxBarOffset += PLAN_H[s].bars;
    }
    AC_STAMP_TIME = climaxBarOffset * SPBAR_G - 3.5;

    for (int si = 0; si < SECN; si++) {
        const SectionH *sec = &PLAN_H[si];
        const int tr = sec->transpose;
        const double startSec = bar * SPBAR_G;
        section_ranges[n_section_ranges++] = (SectionRange){
            .name = sec->name, .startBar = bar, .endBar = bar + sec->bars,
            .startSec = startSec, .endSec = (bar + sec->bars) * SPBAR_G,
        };

        // ── pads / sub / body / sparkle ─────────────────────────────
        for (int b = 0; b < sec->bars; b++) {
            const double tBar = (bar + b) * SPBAR_G;
            const ChordH *ch = hchord(sec->chords[b % sec->nchords]);
            const int root = ch->root + tr;
            const int isOver = !strcmp(sec->name, "overture");
            const int isCoda = !strcmp(sec->name, "coda");
            // ULTIMATE overture fade: 0.03 → 1.0 quadratic across overture bars
            const double overFade = (ULTIMATE && isOver)
                ? (0.03 + pow((double)b / sec->bars, 2.0) * 0.97 > 1.0
                   ? 1.0
                   : 0.03 + pow((double)b / sec->bars, 2.0) * 0.97)
                : 1.0;
            const double padGain = (isOver || isCoda ? 0.085 : 0.072) * overFade;
            const int tier = b / 8;

            // pad triad — root/3rd/5th at root+semi+12.
            // ULTIMATE: voicing rotates every bar — root pos / 1st inv /
            // 2nd inv, then a b7 color on the 4th bar of each 4-bar phrase
            // so the bass-chord wash audibly evolves instead of restating.
            int padQ[4]; int padQN;
            if (ULTIMATE) {
                const int b4 = b % 4;
                if (b4 == 0)      { padQ[0] = 0;          padQ[1] = ch->q3;      padQ[2] = ch->q5;      padQN = 3; }
                else if (b4 == 1) { padQ[0] = ch->q3;     padQ[1] = ch->q5;      padQ[2] = 0 + 12;      padQN = 3; }
                else if (b4 == 2) { padQ[0] = ch->q5;     padQ[1] = 0 + 12;      padQ[2] = ch->q3 + 12; padQN = 3; }
                else              { padQ[0] = 0;          padQ[1] = ch->q3;      padQ[2] = ch->q5;      padQ[3] = 10; padQN = 4; }
            } else {
                padQ[0] = 0; padQ[1] = ch->q3; padQ[2] = ch->q5; padQN = 3;
            }
            for (int t = 0; t < padQN; t++) {
                VoiceOpts vo = {0};
                vo.parts = PAD_PARTS_DEFAULT; vo.n_parts = 5;
                vo.atk = 0.35; vo.rel = 0.4; vo.vibD = 0.003;
                voice_render(tBar + hum(0.004), SPBAR_G * 0.98,
                             root + padQ[t] + 12, padGain, vo);
            }
            if (!isOver && tier >= 1) {
                VoiceOpts vo = {0};
                vo.parts = PAD_PARTS_SPARKLE; vo.n_parts = 3;
                vo.atk = 0.65; vo.rel = 0.65; vo.vibR = 4.2; vo.vibD = 0.004;
                voice_render(tBar + hum(0.005), SPBAR_G * 0.98,
                             root + 24, padGain * 0.22, vo);
            }
            if (tier >= 1 || sec->bars < 24) {
                VoiceOpts vo = {0};
                vo.parts = PAD_PARTS_BODY; vo.n_parts = 4;
                vo.atk = 0.25; vo.rel = 0.35;
                voice_render(tBar + hum(0.003), SPBAR_G * 0.98,
                             root, padGain * 0.55, vo);
            }
            // sub — held out for the first 5 s, then crossfades up to full
            // by t=10 s. Lets the early-overture SFX narrative (typewriter,
            // splash, birdies, meows) read clearly before the low end arrives.
            const int isClimax = !strcmp(sec->name, "climax");
            const int isDevelop = !strcmp(sec->name, "develop");
            const int isBridge  = !strcmp(sec->name, "bridge");
            const double subBoost = (isClimax || isDevelop) ? 1.45 : 1.0;
            double bassEntryFade = 1.0;
            if (tBar < 5.0) bassEntryFade = 0.0;
            else if (tBar < 10.0) bassEntryFade = (tBar - 5.0) / 5.0;
            // Bass slaps harder under the powersine — trance bass-to-lead
            // balance instead of the brass's gentler bass mix.
            const double leadBassBoost = (LEAD_KIND == LEAD_POWERSINE) ? 1.55 : 1.0;
            const double subG = (isBridge ? 0.46 : 0.58) * subBoost * bassEntryFade * leadBassBoost;
            if (ULTIMATE && (b % 4 == 3) && (b + 1 < sec->bars)) {
                // walking sub on bar-4 of every 4-bar group: leading tone into next chord
                const ChordH *next_ch = hchord(sec->chords[(b + 1) % sec->nchords]);
                const int nextRoot = next_ch->root + tr;
                sub_render(tBar, SPBAR_G * 0.5, root, subG);
                sub_render(tBar + SPBAR_G * 0.5, SPBAR_G * 0.49, nextRoot - 1, subG * 0.85);
            } else {
                sub_render(tBar, SPBAR_G * 0.99, root, subG);
            }
            // DEEEEP sub-octave layer for ULTIMATE climax
            if (ULTIMATE && isClimax) {
                sub_render(tBar, SPBAR_G * 0.99, root - 12, subG * 0.55);
            }
            // SISTER SUB — wider, two-octaves-down stereo sub-bass that
            // holds space under the lead. Two voices slightly detuned and
            // panned opposite for L/R stereo width. Industrial space-holding.
            if (ULTIMATE && !isOver) {
                const double detuneSemi = 0.10;     // ~10 cents
                VoiceOpts ssvoL = {0};
                ssvoL.parts = SISTER_SUB_PARTS;
                ssvoL.n_parts = 3;
                ssvoL.atk = 0.20; ssvoL.rel = 0.45;
                ssvoL.pan = -0.55;
                ssvoL.drive = 0.8;
                ssvoL.wet_send = 0.55;
                voice_render(tBar, SPBAR_G * 0.95, root - 12 - detuneSemi,
                             subG * 0.30, ssvoL);
                VoiceOpts ssvoR = ssvoL;
                ssvoR.pan = 0.55;
                voice_render(tBar, SPBAR_G * 0.95, root - 12 + detuneSemi,
                             subG * 0.30, ssvoR);
            }

            // ULTIMATE Bachian piano — two voices through the whole track.
            // Quadratic fade-in across the overture so the piano roll doesn't
            // slap on at t=0; reaches full gain by the end of the overture.
            if (ULTIMATE) {
                double bachG = 0.060;
                if      (!strcmp(sec->name, "overture"))  bachG = 0.052;
                else if (!strcmp(sec->name, "statement")) bachG = 0.072;
                else if (!strcmp(sec->name, "bridge"))    bachG = 0.088;
                else if (!strcmp(sec->name, "develop"))   bachG = 0.058;
                else if (!strcmp(sec->name, "climax"))    bachG = 0.072;
                else if (!strcmp(sec->name, "coda"))      bachG = 0.064;
                if (!strcmp(sec->name, "overture")) {
                    // 0 at b=0 → ~1.0 by end of overture (quadratic ease-in)
                    const double bachFade = pow((double)b / (sec->bars - 1), 2.0);
                    bachG *= bachFade;
                    if (bachG < 1e-5) continue;     // skip near-silent voices
                }
                const int cleanBits = (!strcmp(sec->name, "overture") || !strcmp(sec->name, "coda"));
                PianoOpts po = {0};
                po.sus = 0.95;
                po.bits = cleanBits ? 16 : 6;
                po.hold = cleanBits ? 1 : 4;
                const int fifth = root + ch->q5;
                // bass quarter-notes, panned left
                PianoOpts po_bass = po; po_bass.pan = -0.20;
                piano_render(tBar + 0 * SPB_G, SPB_G * 0.96, root - 12,  bachG * 0.95, po_bass);
                po_bass.sus = 0.85;
                piano_render(tBar + 1 * SPB_G, SPB_G * 0.96, fifth - 12, bachG * 0.82, po_bass);
                po_bass.sus = 0.95;
                piano_render(tBar + 2 * SPB_G, SPB_G * 0.96, root,       bachG * 0.92, po_bass);
                po_bass.sus = 0.85;
                piano_render(tBar + 3 * SPB_G, SPB_G * 0.96, fifth - 12, bachG * 0.82, po_bass);
                // treble eighth-notes, panned right
                PianoOpts po_treb = po; po_treb.pan = 0.20; po_treb.sus = 0.55;
                const int mid = root + 24;
                const int seq[8] = {
                    mid,             mid + ch->q3,     mid + ch->q5,    mid + ch->q3 + 12,
                    mid + 12,        mid + ch->q5,     mid + ch->q3,    mid + ch->q5 - 12,
                };
                const double eighth = SPB_G / 2.0;
                const double tG = bachG * 0.58;
                for (int k = 0; k < 8; k++) {
                    piano_render(tBar + k * eighth + hum(0.004),
                                 eighth * 0.88, seq[k], tG, po_treb);
                }
            }
        }

        // ── steam release per section ───────────────────────────────
        const double secDur = sec->bars * SPBAR_G;
        const double stMul = ULTIMATE
            ? (!strcmp(sec->name, "overture") ? 0.15 : 0.55)
            : 1.0;
        const double steamStartT = (ULTIMATE && !strcmp(sec->name, "overture"))
            ? startSec + 6 * SPBAR_G : startSec;
        const double steamDur0 = (ULTIMATE && !strcmp(sec->name, "overture"))
            ? (secDur - 6 * SPBAR_G > 0 ? secDur - 6 * SPBAR_G : 0)
            : secDur;
        if (steamDur0 > 0) {
            steam_render(steamStartT, steamDur0,
                         ((!strcmp(sec->name, "overture")) ? 0.032 :
                          (!strcmp(sec->name, "bridge"))   ? 0.030 : 0.022) * stMul,
                         (!strcmp(sec->name, "overture")) ? 220 : 130,
                         (!strcmp(sec->name, "overture")) ? 760 : 400,
                         5500.0,
                         (!strcmp(sec->name, "overture")) ? 2.4 : 1.4,
                         1.8, 0.5, 0.35);
        }
        if (strcmp(sec->name, "coda") != 0) {
            const double endStart = startSec + (secDur - 4 * SPBAR_G);
            const double endStartC = endStart < startSec ? startSec : endStart;
            steam_render(endStartC, 4 * SPBAR_G + 0.5,
                         ((!strcmp(sec->name, "develop")) ? 0.065 : 0.045) * stMul,
                         130, 400.0, 5500.0,
                         3.0, 0.6, 0.8, 0.45);
        } else {
            steam_render(startSec + 6 * SPBAR_G, 10 * SPBAR_G + TAIL_SEC,
                         0.035 * stMul, 130, 400.0, 5500.0,
                         3.5, 4.5, 0.35, 0.55);
        }

        // ── kicks ───────────────────────────────────────────────────
        // Per-section progressive thinning: kicks shorten linearly across
        // the section as the music gets denser. Climax + coda stay full.
        double sectionThinMax = 0;
        if      (!strcmp(sec->name, "statement")) sectionThinMax = 0.40;
        else if (!strcmp(sec->name, "develop"))   sectionThinMax = 0.55;
        else if (!strcmp(sec->name, "bridge"))    sectionThinMax = 0.50;
        for (int b = 0; b < sec->bars; b++) {
            const double tBar = (bar + b) * SPBAR_G;
            const double dr = sec->drive;
            const int lastBar = (b == sec->bars - 1);
            const double thin = ULTIMATE
                ? ((double)b / (sec->bars > 1 ? sec->bars - 1 : 1)) * sectionThinMax
                : 0.0;

            if (!strcmp(sec->kick, "halftime") || !strcmp(sec->kick, "halfhard")) {
                const int beats[2] = {0, 2};
                for (int k = 0; k < 2; k++) {
                    const double tk = tBar + beats[k] * SPB_G + hum(0.003);
                    kick_render(tk, dr, 1.0, thin); push_kick(tk);
                }
                // ULTIMATE syncopated push-kick on and-of-3 every 4-bar phrase
                // in statement + develop — leans the rhythm forward.
                if (ULTIMATE && (b % 4 == 3) &&
                    (!strcmp(sec->name, "statement") || !strcmp(sec->name, "develop"))) {
                    const double tk = tBar + 2.5 * SPB_G + hum(0.003);
                    kick_render(tk, dr * 0.55, 0.55, thin); push_kick(tk);
                }
                if (!strcmp(sec->kick, "halfhard") && (b % 2 == 1)) {
                    const double tk = tBar + 3.5 * SPB_G + hum(0.003);
                    kick_render(tk, dr * 0.7, 0.7, thin); push_kick(tk);
                }
                if (lastBar && !strcmp(sec->name, "develop")) {
                    for (int r = 0; r < 4; r++) {
                        const double tk = tBar + r * SPB_G;
                        kick_render(tk, dr * (0.65 + r * 0.10), 0.85, thin); push_kick(tk);
                    }
                }
            } else if (!strcmp(sec->kick, "pulse")) {
                const int beats[2] = {0, 2};
                for (int k = 0; k < 2; k++) {
                    const double tk = tBar + beats[k] * SPB_G;
                    kick_render(tk, dr, 0.85, thin); push_kick(tk);
                }
            }

            // backbeat snare + hi-hats (any kick mode except "none"/"fade")
            if (strcmp(sec->kick, "none") != 0 && strcmp(sec->kick, "fade") != 0) {
                const int bridgeSparse = !strcmp(sec->name, "bridge");
                const int ptier = b / 8;
                // ULTIMATE coda taper: hi-hat density 1.0 → 0.15 across 16 bars
                const double codaFade = !strcmp(sec->name, "coda")
                    ? fmax(0.15, 1.0 - 0.85 * ((double)b / (sec->bars > 1 ? sec->bars - 1 : 1)))
                    : 1.0;
                // backbeat snare — beats 2 + 4
                const double snGain = bridgeSparse ? 0.30 :
                    (!strcmp(sec->name, "climax"))  ? 0.56 :
                    (!strcmp(sec->name, "develop")) ? 0.50 : 0.48;
                const int snBeats[2] = {1, 3};
                for (int k = 0; k < 2; k++) {
                    const int beat = snBeats[k];
                    if (lastBar && !strcmp(sec->name, "develop") && beat == 3) continue;
                    const double ts = tBar + beat * SPB_G + hum(0.003);
                    snare_render(ts, snGain, 175.0);
                }
                // pre-climax snare-roll fill (last bar of develop, 8×16ths over beats 3-4)
                if (lastBar && !strcmp(sec->name, "develop")) {
                    for (int r = 0; r < 8; r++) {
                        const double ts = tBar + 2 * SPB_G + r * (SPB_G / 4);
                        snare_render(ts, 0.26 + r * 0.045, 175.0);
                    }
                }
                // hi-hats
                // tickEarly: ULTIMATE suppresses closed hat at top of statement
                // (b<8) — placeholder space for the rattle-intro layer.
                const int tickEarly = !(ULTIMATE && !strcmp(sec->name, "statement") && b < 8);
                if (tickEarly) tick_render(tBar + 1 * (SPB_G / 2), (bridgeSparse ? 0.14 : 0.20) * codaFade, 0);
                if (ptier >= 1) tick_render(tBar + 5 * (SPB_G / 2), (bridgeSparse ? 0.14 : 0.20) * codaFade, 0);
                if (!bridgeSparse && (ptier >= 1 || !strcmp(sec->name, "coda"))) {
                    tick_render(tBar + 3.5 * SPB_G, 0.42 * codaFade, 1);          // open and-of-3
                    if (b % 2 == 0) tick_render(tBar + 1.5 * SPB_G, 0.32 * codaFade, 1);
                    if (!strcmp(sec->name, "develop") || !strcmp(sec->name, "climax")) {
                        tick_render(tBar + 7.5 * SPB_G, 0.28, 1);      // open and-of-4
                    }
                }
            }

            // ── CODA fade kick — 4-stage evolution mirroring JS ─────────
            if (!strcmp(sec->kick, "fade")) {
                const ChordH *ch_c = hchord(sec->chords[b % sec->nchords]);
                const int chRoot = ch_c->root + tr;
                const double tk = tBar;
                if (b < 3) {
                    // REVERSE-FEEL swell — slow-attack sub on chord root that
                    // ramps INTO the downbeat (env = pow(lt/dur, 1.4)). JS
                    // uses write() per-sample; here we synthesize it inline.
                    const double swellDur = SPBAR_G * 0.95;
                    const double swellF = m2f(chRoot - 12);
                    double ph = 0.0;
                    long iStart = (long)(tk * SR); if (iStart < 0) iStart = 0;
                    long iEnd = (long)((tk + swellDur) * SR); if (iEnd > N) iEnd = N;
                    for (long i = iStart; i < iEnd; i++) {
                        const double lt = (double)i / SR - tk;
                        const double envR = lt / swellDur;
                        double env = pow(envR, 1.4);
                        if (env > 1.0) env = 1.0;
                        ph += swellF / SR; if (ph >= 1.0) ph -= 1.0;
                        const double v = sin(TAU * ph) * env * 0.40;
                        L[i] += (float)v; R[i] += (float)v;
                    }
                } else if (b < 6) {
                    // Lerp swell → forward kick. blend goes 0→1 across bars 3..5
                    const double blend = (b - 3) / 3.0;
                    // swell at 0.35 * (1 - blend) gain
                    const double swellDur = SPBAR_G * 0.95;
                    const double swellF = m2f(chRoot - 12);
                    double ph = 0.0;
                    long iStart = (long)(tk * SR); if (iStart < 0) iStart = 0;
                    long iEnd = (long)((tk + swellDur) * SR); if (iEnd > N) iEnd = N;
                    for (long i = iStart; i < iEnd; i++) {
                        const double lt = (double)i / SR - tk;
                        const double envR = lt / swellDur;
                        double env = pow(envR, 1.4);
                        if (env > 1.0) env = 1.0;
                        ph += swellF / SR; if (ph >= 1.0) ph -= 1.0;
                        const double v = sin(TAU * ph) * env * 0.35 * (1.0 - blend);
                        L[i] += (float)v; R[i] += (float)v;
                    }
                    kick_render(tk, dr * 0.85, 0.55 * blend, 0.40);
                    sub_render(tk, 0.20, chRoot - 12, 0.40 * (1.0 - blend * 0.2));
                } else if (b < 13) {
                    // Bars 6..12: punchy SINE-CHORDAL kick — kick body + sub
                    // stack on root/3rd/5th. The clean dance kick.
                    kick_render(tk, dr * 1.15, 0.90, 0.30);
                    sub_render(tk, 0.32, chRoot - 12, 0.50);
                    sub_render(tk, 0.22, chRoot - 12 + ch_c->q3, 0.32);
                    sub_render(tk, 0.22, chRoot - 12 + ch_c->q5, 0.26);
                } else if (b == 13) {
                    // Bar 13: 4 staggered pitch-dropping kick fragments
                    for (int s = 0; s < 4; s++) {
                        const double ts = tk + s * (SPB_G * 0.95);
                        const double pitchDrop = s * 2;
                        kick_render(ts, dr * (1 - s * 0.18), 0.75 - s * 0.13, 0.45);
                        sub_render(ts, 0.20, chRoot - 12 - pitchDrop, 0.40 * (1 - s * 0.20));
                    }
                } else if (b == 14) {
                    // Bar 14: 6 micro-kicks at irregular subdivs
                    const double subs[6] = {0, 0.18, 0.31, 0.52, 0.71, 0.88};
                    for (int s = 0; s < 6; s++) {
                        const double ts = tk + subs[s] * SPBAR_G;
                        const double drop = s * 1.5;
                        kick_render(ts, dr * (0.85 - s * 0.10), 0.55 - s * 0.07, 0.55);
                        sub_render(ts, 0.16, chRoot - 12 - drop, 0.30 * (1 - s * 0.12));
                    }
                } else {
                    // Bar 15: long pitch-down sub swell, ~6 semis/sec down
                    const double swellDur = SPBAR_G * 0.95;
                    long iStart = (long)(tk * SR); if (iStart < 0) iStart = 0;
                    long iEnd = (long)((tk + swellDur) * SR); if (iEnd > N) iEnd = N;
                    double ph = 0.0;
                    const double f0 = m2f(chRoot - 12);
                    for (long i = iStart; i < iEnd; i++) {
                        const double lt = (double)i / SR - tk;
                        const double semisDown = 6.0 * lt;
                        const double f = f0 * pow(2.0, -semisDown / 12.0);
                        ph += f / SR; if (ph >= 1.0) ph -= 1.0;
                        const double env = 0.55 * exp(-lt / 1.2);
                        const double v = sin(TAU * ph) * env;
                        L[i] += (float)v; R[i] += (float)v;
                    }
                }
            }
            // "none" — no kicks (overture)
        }

        // ── ULTIMATE: bell pings on accents through brass-driven sections ──
        if (ULTIMATE && (!strcmp(sec->theme, "brass") ||
                         !strcmp(sec->name, "bridge") ||
                         !strcmp(sec->name, "develop"))) {
            for (int bb = 0; bb < sec->bars; bb += 2) {
                if (!strcmp(sec->name, "climax") && bb >= 8 && bb < 16) continue;
                const double tBar = (bar + bb) * SPBAR_G;
                const ChordH *ch2 = hchord(sec->chords[bb % sec->nchords]);
                const int root2 = ch2->root + tr;
                const int offset = (bb % 4 == 0) ? 48 : 48 + ch2->q5;
                const int midiBell = root2 + offset;
                if (midiBell < 120) {
                    const double bellG = !strcmp(sec->name, "climax") ? 0.072 : 0.058;
                    const double panBase = ((bb / 2) % 2 == 0) ? -0.30 : 0.30;
                    BellOpts bo = {0};
                    bo.pan = panBase; bo.dec_tau = 4.5; bo.atk = 0.020; bo.wet_send = 0.85;
                    bell_render(tBar + hum(0.006), midiBell, bellG, bo);
                    if (tBar >= 75.0) {
                        const int fifth = midiBell + 7;
                        if (fifth < 120) {
                            BellOpts bo2 = bo; bo2.pan = -panBase * 0.60;
                            bo2.dec_tau = 4.0; bo2.atk = 0.022;
                            bell_render(tBar + 0.014 + hum(0.004), fifth, bellG * 0.78, bo2);
                        }
                        if ((bb % 4) == 0 && midiBell + 12 < 124) {
                            BellOpts bo3 = bo; bo3.pan = panBase * 0.35;
                            bo3.dec_tau = 3.0; bo3.atk = 0.018; bo3.wet_send = 0.90;
                            bell_render(tBar + 0.028 + hum(0.004),
                                        midiBell + 12, bellG * 0.42, bo3);
                        }
                    }
                }
            }
        }

        // ── ULTIMATE: polyrhythm 3-against-4 wood-block in develop+climax ──
        if (ULTIMATE && (!strcmp(sec->name, "develop") || !strcmp(sec->name, "climax"))) {
            for (int b = 0; b < sec->bars; b++) {
                const double tBar = (bar + b) * SPBAR_G;
                double polyG = (b - 8) / 4.0;
                if (polyG < 0) polyG = 0; if (polyG > 1) polyG = 1;
                polyG *= 0.12;
                if (polyG > 0.006) {
                    for (int p = 0; p < 3; p++) {
                        woodtick_render(tBar + (p * 4.0 / 3.0) * SPB_G + hum(0.003), polyG);
                    }
                }
            }
        }

        // ── theme rendering ─────────────────────────────────────────
        const char *th = sec->theme;
        // ULTIMATE: per-section per-loop strategy rotation produces tv[]
        // Non-ULTIMATE: tv[] = plain THEME
        NoteSeq tvbuf;
        if (!strcmp(th, "soft")) {
            if (ULTIMATE) ult_theme_strat(sec->name, 0)(THEME, THEME_N, &tvbuf);
            else strat_none(THEME, THEME_N, &tvbuf);
            const int useN = tvbuf.n < 11 ? tvbuf.n : 11;
            // ULTIMATE: phase the soft theme to enter at bar 6
            const double themeT0 = ULTIMATE ? startSec + 6 * SPBAR_G : startSec;
            lay_theme(tvbuf.notes, useN, 0.085, 0, 0, themeT0, tr);
        } else if (!strcmp(th, "brass")) {
            const int loops = sec->bars / 8; const int loopsC = loops < 1 ? 1 : loops;
            for (int lp = 0; lp < loopsC; lp++) {
                const double t0 = startSec + lp * 8 * SPBAR_G;
                // Lead pushed louder + more up front (@jeffrey 2026-05-26
                // "push the lead melody / more up front with the brass").
                const double meldG = ULTIMATE ? ((lp == 0) ? 0.52 : 0.46) : ((lp == 0) ? 0.40 : 0.35);
                const double cntG  = ULTIMATE ? 0.21 : 0.17;
                const double sprG  = ULTIMATE ? 0.10 : 0.082;
                const int climaxBreathe = ULTIMATE && !strcmp(sec->name, "climax") && (lp == 1);
                // resolve theme variant for this section+loop (ULTIMATE only)
                NoteSeq tv;
                if (ULTIMATE) ult_theme_strat(sec->name, lp)(THEME, THEME_N, &tv);
                else strat_none(THEME, THEME_N, &tv);
                // Statement loop 0 — play the full leitmotif from bar 1
                // (no SIMPLE_INTRO buildup) so the lead hits AT the drop,
                // not 8 bars later (@jeffrey 2026-05-25).
                lay_theme(tv.notes, tv.n, meldG, 1, 0, t0, tr);
                // SIDE INSTRUMENT — soft synth pad doubling the THEME
                // an octave up. CUT after the climax drop.
                if (strcmp(sec->name, "climax") != 0 && strcmp(sec->name, "coda") != 0) {
                    lay_theme(tv.notes, tv.n, meldG * 0.18, 0, 12, t0, tr);
                }
                // BIG ORCHESTRAL OPENING — first 2 statement loops only.
                // Stack THREE extra harmonized layers: octave-up shimmer
                // (+12), fifth-up bright (+7), and an octave-down anchor
                // (-12). Together they read as a string-stack/orchestral
                // arrival on the first bars of the drop. (@jeffrey "the
                // melody sines / power sine in its first few bars have
                // some upper octaves / big orchestral harmonization")
                // Orchestral harmony stack DISABLED — it was driving
                // the engine peak to 5.7x, which forced normalize to
                // cut the vocals down to inaudible. Side instrument
                // above (octave-up at 0.18*meldG) is enough.
                // To restore: flip this `if(0)` back to the original
                // condition.
                if (0 && !strcmp(sec->name, "statement") && lp <= 1) {
                    lay_theme(tv.notes, tv.n, meldG * 0.16, 0, 24, t0, tr);
                }
                if (ULTIMATE && !strcmp(sec->name, "climax")) {
                    const double cntScale = climaxBreathe ? 0.55 : 1.55;
                    lay_counter(COUNTER, COUNTER_N, cntG * cntScale, t0, tr);
                    if (!climaxBreathe) {
                        // octave-up counter shadow
                        Note upCounter[COUNTER_N];
                        for (int i = 0; i < COUNTER_N; i++) {
                            upCounter[i].off = COUNTER[i].off + 12;
                            upCounter[i].beats = COUNTER[i].beats;
                        }
                        lay_counter(upCounter, COUNTER_N, cntG * 0.85, t0, tr);
                    }
                } else if (lp >= 1) {
                    lay_counter(COUNTER, COUNTER_N, cntG, t0, tr);
                }
                if (lp >= 2) lay_theme(tv.notes, tv.n, sprG, 1, 12, t0, tr);
                // ULTIMATE lead doubling — statement uses saw-lead stutter,
                // climax uses GRAND PIANO arpeggio (cleaner, less industrial
                // stutter at the peak — what the user wants at master 1:52+).
                if (ULTIMATE && lp >= 1 && !climaxBreathe &&
                    (!strcmp(sec->name, "statement") || !strcmp(sec->name, "climax"))) {
                    const int isClimax = !strcmp(sec->name, "climax");
                    // climax piano arpeggio dialled further down — was still
                    // too prominent at master 1:53 according to @jeffrey
                    const double leadG = isClimax ? 0.045 : 0.080;
                    double bp = 0.0;
                    for (int i = 0; i < tv.n; i++) {
                        const double tN = t0 + bp * SPB_G;
                        const int midi = ROOT_MEL_H + tv.notes[i].off + tr;
                        const double dur = tv.notes[i].beats * SPB_G * 0.96;
                        if (isClimax) {
                            // GRAND PIANO arpeggio — 4 sub-notes per note,
                            // ascending through chord tones. Clean (bits=16,
                            // hold=1, no bitcrush).
                            const int subN = 4;
                            const double subDur = dur / subN;
                            // arp interval pattern: root → 4 → 7 → 12 (oct)
                            const int arpOffsets[4] = {0, 4, 7, 12};
                            for (int s = 0; s < subN; s++) {
                                PianoOpts pop = {0};
                                pop.sus = 1.0; pop.bits = 16; pop.hold = 1;
                                pop.pan = (s - 1.5) * 0.12;
                                piano_render(tN + s * subDur, subDur * 0.95,
                                             midi + arpOffsets[s], leadG, pop);
                            }
                            // Sustained octave-up shimmer
                            PianoOpts pop2 = {0};
                            pop2.sus = 1.0; pop2.bits = 16; pop2.hold = 1;
                            pop2.pan = 0.30;
                            piano_render(tN, dur, midi + 12, leadG * 0.35, pop2);
                        } else {
                            SawOpts so = {0};
                            so.atk = 0.008; so.rel = 0.05; so.gate_ms = 82;
                            so.gate_on_frac = 0.55; so.drive = 0.85;
                            saw_render(tN, dur, midi, leadG, so);
                            so.atk = 0.010; so.detune = 0.009;
                            saw_render(tN, dur, midi + 12, leadG * 0.5, so);
                        }
                        bp += tv.notes[i].beats;
                    }
                }
            }
        } else if (!strcmp(th, "bsoft")) {
            lay_theme(BTHEME, BTHEME_N, 0.11, 0, 0, startSec, tr);
            lay_theme(BTHEME, BTHEME_N, 0.10, 0, 0, startSec, tr);
        } else if (!strcmp(th, "frag")) {
            NoteSeq fv;
            if (ULTIMATE) ult_theme_strat(sec->name, 0)(THEME, THEME_N, &fv);
            else strat_none(THEME, THEME_N, &fv);
            // 8 segments of head (first 4 notes of the variant), sequenced up
            const int headN = fv.n < 4 ? fv.n : 4;
            for (int seg = 0; seg < 8; seg++) {
                const double t0 = startSec + seg * 2 * SPBAR_G;
                double bp = 0.0;
                for (int k = 0; k < headN; k++) {
                    const int midi = ROOT_MEL_H + fv.notes[k].off + tr + (seg % 4) * 2;
                    VoiceOpts vo = {0};
                    vo.parts = FRAG_PARTS; vo.n_parts = 5;
                    vo.atk = 0.016; vo.rel = 0.09; vo.drive = 1.3;
                    voice_render(t0 + bp * SPB_G + hum(0.004),
                                 fv.notes[k].beats * SPB_G * 0.9, midi, 0.135, vo);
                    // ULTIMATE skips hoover doubling
                    if (seg >= 4 && !ULTIMATE) {
                        hoover_render(t0 + bp * SPB_G,
                                      fv.notes[k].beats * SPB_G * 0.8,
                                      midi - 12, 0.13);
                    }
                    bp += fv.notes[k].beats;
                }
            }
            // riser into climax
            riser_render(startSec + (sec->bars - 2) * SPBAR_G, 2 * SPBAR_G,
                         ROOT_MEL_H - 12, ROOT_MEL_H + 14, 0.28);
        } else if (!strcmp(th, "dissolve")) {
            NoteSeq dv;
            if (ULTIMATE) ult_theme_strat(sec->name, 0)(THEME, THEME_N, &dv);
            else strat_none(THEME, THEME_N, &dv);
            const int useN = dv.n < 8 ? dv.n : 8;
            lay_theme(dv.notes, useN, 0.10, 1, 0, startSec, tr);
            lay_counter(COUNTER, 4, 0.058, startSec, tr);
            VoiceOpts vd = {0};
            vd.parts = DROOSE_PAD_PARTS; vd.n_parts = 4;
            vd.atk = 1.2; vd.rel = 2.4;
            voice_render(startSec + 6 * SPBAR_G, 6 * SPBAR_G + TAIL_SEC,
                         ROOT_MEL_H + tr, 0.07, vd);
            VoiceOpts vdl = {0};
            vdl.parts = DROOSE_LOW_PARTS; vdl.n_parts = 2;
            vdl.atk = 1.2; vdl.rel = 2.4;
            voice_render(startSec + 6 * SPBAR_G, 6 * SPBAR_G + TAIL_SEC,
                         ROOT_MEL_H + tr - 12, 0.06, vdl);
        }

        // ── stabs (climax only, offbeats) ───────────────────────────
        if (!strcmp(sec->name, "climax")) {
            for (int b = 0; b < sec->bars; b++) {
                const double tBar = (bar + b) * SPBAR_G;
                const ChordH *ch = hchord(sec->chords[b % sec->nchords]);
                const double sg1 = ULTIMATE ? 0.18 : 0.30;
                const double sg2 = ULTIMATE ? 0.16 : 0.26;
                stab_render(tBar + 1.5 * SPB_G, ch->root + tr + 24, sg1);
                stab_render(tBar + 3.5 * SPB_G, ch->root + tr + 24 + 7, sg2);
            }
        }

        bar += sec->bars;
        report("§ %-10s · bar %3d/%d", sec->name, bar, totalBars);
    }
}

// ── post-arrangement sample layers ────────────────────────────────────
#define HELLSINE_SAMPLES_DIR "pop/hellsine/samples"

static float *try_load_sample(const char *rel, long *n_out) {
    char path[1024];
    snprintf(path, sizeof(path), "%s/%s", HELLSINE_SAMPLES_DIR, rel);
    return load_wav_mono(path, n_out);     // returns NULL + prints if missing
}

static void post_arrangement_grenade(void) {
    if (n_kick_events <= 0) return;
    long gn = 0;
    float *gren = try_load_sample("grenade.wav", &gn);
    if (!gren) return;
    int placed = 0;
    for (int i = 0; i < n_kick_events; i++) {
        if (i % 8 != 7) continue;
        const double kt = kick_events[i];
        PlaySampleOpts po = {0};
        po.rate = 0.92; po.pan = 0; po.wet_send = 0.20; po.fade = 0.005;
        play_sample(kt, gren, gn, 0.42, po);
        placed++;
    }
    free(gren);
    report("→ grenade-kick · %d layered every-8th-kick", placed);
}

static void post_arrangement_rattle(void) {
    if (!strcmp(RATTLE_MODE, "off")) return;
    long rn = 0;
    float *rat = try_load_sample("rattle.wav", &rn);
    if (!rat) {
        report("· rattle · no sample at %s/rattle.wav — pure all-sine", HELLSINE_SAMPLES_DIR);
        return;
    }
    int placed = 0;
    for (int s = 0; s < n_section_ranges; s++) {
        const SectionRange *sr = &section_ranges[s];
        const int driven = !strcmp(sr->name, "statement") || !strcmp(sr->name, "develop") || !strcmp(sr->name, "climax");
        if (!driven && strcmp(sr->name, "bridge") != 0) continue;
        for (int b = sr->startBar; b < sr->endBar; b++) {
            const double tBar = b * SPBAR_G;
            if (!strcmp(RATTLE_MODE, "drive") && driven) {
                const double beats[4] = {0.5, 1.5, 2.5, 3.5};
                for (int k = 0; k < 4; k++) {
                    PlaySampleOpts po = {0};
                    po.rate = 1.0 + hum(0.03);
                    po.pan = hum(0.35);
                    po.fade = 0.015;
                    play_sample(tBar + beats[k] * SPB_G + hum(0.004), rat, rn,
                                RATTLE_GAIN * 0.5, po);
                    placed++;
                }
            } else if ((b - sr->startBar) % 2 == 1) {
                PlaySampleOpts po = {0};
                po.rate = 1.0 + hum(0.03);
                po.pan = hum(0.3);
                po.fade = 0.015;
                play_sample(tBar + 3.5 * SPB_G + hum(0.004), rat, rn,
                            driven ? RATTLE_GAIN : RATTLE_GAIN * 0.6, po);
                placed++;
            }
        }
    }
    free(rat);
    report("→ rattle · %s · %d hits · sampled (THE LAW amended)", RATTLE_MODE, placed);
}

// ── ULTIMATE — drop-time grenade + last-drop grenade + AC stamp ding ──
// Most ULTIMATE features are folded into the bars loop (see ULTIMATE checks
// in render_full_track). The post-arrangement ULTIMATE layer is the
// sample-driven sound design: grenades at fixed times, AC-stamp ding,
// crowd roar, etc.
static void post_arrangement_ultimate(void) {
    if (!ULTIMATE) return;
    int climaxBarOffset = 0;
    for (int s = 0; s < SECN; s++) {
        if (!strcmp(PLAN_H[s].name, "climax")) break;
        climaxBarOffset += PLAN_H[s].bars;
    }
    const double CLIMAX_START = climaxBarOffset * SPBAR_G;
    const double AC_STAMP_TIME = CLIMAX_START - 3.5;

    // grenade @ t=12.90 — pushed LOUDER and more poppin' after the
    // meow at 12.40s. (@jeffrey "grande explosion after meow can be a
    // little more poppin / loud").
    long gn = 0;
    float *gren = try_load_sample("grenade.wav", &gn);
    if (gren) {
        PlaySampleOpts po = {0};
        po.rate = 0.88; po.wet_send = 0.22; po.fade = 0.005;
        play_sample(12.90, gren, gn, 1.85, po);             // was 1.15 — POP
        po.rate = 0.50; po.wet_send = 0.40;
        play_sample(12.90, gren, gn, 0.95, po);             // was 0.50 — body
        // last-drop grenade at climax start
        po.rate = 0.88; po.wet_send = 0.25;
        play_sample(CLIMAX_START, gren, gn, 1.50, po);
        po.rate = 0.50; po.wet_send = 0.45;
        play_sample(CLIMAX_START, gren, gn, 0.75, po);
        report("→ grenade · drop + climax bookends (post-meow POP boosted)");
        free(gren);
    }

    // ── "aesthetic dot computer" stamp — ELABORATE 4-LAYER VERSION ────
    // Main pitched up + perfect 5th + octave-up harmony + slow body
    // underneath, heavy reverb wash. Matches JS hellsine.mjs:2933-2951.
    long dn = 0;
    float *acBuf = try_load_sample("aesthetic-dot-computer.wav", &dn);
    if (acBuf) {
        const double stampT = AC_STAMP_TIME;
        // Main — pitched up (rate 1.18 ≈ +2.9 semis)
        PlaySampleOpts po = {0};
        po.rate = 1.18; po.pan = 0.0; po.wet_send = 0.75; po.fade = 0.005;
        play_sample(stampT, acBuf, dn, 0.45, po);
        // Harmony — perfect 5th up (×1.5 rate)
        PlaySampleOpts ph5 = {0};
        ph5.rate = 1.18 * 1.5; ph5.pan = -0.35; ph5.wet_send = 0.85; ph5.fade = 0.005;
        play_sample(stampT + 0.025, acBuf, dn, 0.28, ph5);
        // Harmony — octave up (×2 rate)
        PlaySampleOpts po1 = {0};
        po1.rate = 1.18 * 2.0; po1.pan = 0.35; po1.wet_send = 0.85; po1.fade = 0.005;
        play_sample(stampT + 0.045, acBuf, dn, 0.18, po1);
        // Slow body — rate 0.78 for weight + smear
        PlaySampleOpts pob = {0};
        pob.rate = 0.78; pob.pan = 0.0; pob.wet_send = 0.90; pob.fade = 0.005;
        play_sample(stampT + 0.060, acBuf, dn, 0.22, pob);
        report("→ AC-stamp · 4-layer pitched+harmonized stack @ %.2fs", stampT);
        free(acBuf);
    }

    // ── EXTREME SQUARE-WAVE TRIAD — first drop full, climax drop softer
    // so the climax drop has more air. Climax was at 1.10 (2× first); now
    // at 0.50 so it accents without dominating the other climax-drop hits.
    {
        const double dropTimes[2] = { 15.82, CLIMAX_START };
        const double dropGains[2] = { 0.55, 0.50 };
        for (int d = 0; d < 2; d++) {
            const double t0 = dropTimes[d];
            const double g  = dropGains[d];
            const double triad[3] = {38, 41, 45};           // D2 + F2 + A2
            const double durTri = 0.42;
            const long iS = (long)(t0 * SR);
            const long iE = iS + (long)(durTri * SR);
            for (long i = iS; i < iE && i < N; i++) {
                if (i < 0) continue;
                const double lt = (double)(i - iS) / SR;
                const double atk = 1.0 - exp(-lt / 0.0008);   // sub-ms snap
                const double dec = exp(-lt / 0.18);
                const double amp = atk * dec;
                double x = 0;
                for (int k = 0; k < 3; k++) {
                    const double f = m2f(triad[k]);
                    const double ph = fmod(f * lt, 1.0);
                    // Hard-clip the sine = square wave (odd partials all the way up)
                    double s = sin(TAU * ph);
                    s = (s > 0.5) ? 1.0 : (s < -0.5 ? -1.0 : s * 2.0);
                    x += s;
                }
                x = tanh(x * 1.8) * amp * g;
                L[i] += (float)x;
                R[i] += (float)x;
                // Send to spatial resonator for industrial slap
                SL[i] += (float)(x * 0.70);
                SR_[i] += (float)(x * 0.70);
            }
        }
        report("→ EXTREME square-wave triad @ first drop + climax drop");
    }

    // ── PERC BREAK — minimal fast perc climbing into the climax drop.
    //    105.0 → 110.77 s (≈ master 1:45-1:51). Density accelerates:
    //    8ths → 16ths → 32nds. Tick + woodtick + sparse sub accent. Builds
    //    tension that resolves at the drop.
    {
        const double pbStart = 105.0;
        const double pbEnd   = CLIMAX_START;          // 110.77
        const double dur     = pbEnd - pbStart;        // 5.77 s
        const double n8  = dur / (SPB_G / 2.0);       // # of 8ths
        // Walk every 16th; conditionally hit based on local density curve.
        const double n16 = dur / (SPB_G / 4.0);
        int hits = 0;
        for (int i = 0; i < (int)n16; i++) {
            const double t = pbStart + i * (SPB_G / 4.0);
            const double p = (t - pbStart) / dur;     // 0..1 across break
            // Density grows from 0.25 (every 4th 16th = 8th notes early)
            // to 1.0 (every 16th — late) to 1.5 (every 32nd — final ramp)
            const double density = 0.25 + p * 1.30;
            // Compute whether this 16th fires: spread evenly via accumulator
            // Use density as probability-driven, but deterministic via rng
            if (rng() > density) continue;
            // Pan ping-pong + velocity grows
            const double pan = (i % 2 == 0) ? -0.55 : 0.55;
            const double g = (0.12 + p * 0.18);
            tick_render(t + hum(0.002), g, 0);
            hits++;
            // Accent every 4th step with wood-block + a quieter open hat
            if (i % 4 == 0) {
                woodtick_render(t + hum(0.002), 0.20 + p * 0.15);
            }
            // Final 16 steps (32nd density): insert a 32nd-note ghost-tick
            // between each step for the "rolling up" feel
            if (p > 0.70) {
                tick_render(t + (SPB_G / 8.0) + hum(0.002),
                            (0.10 + p * 0.12), 0);
            }
        }
        // Spaced sub accents every half-bar through the break for low-end pulse
        for (double t = pbStart; t < pbEnd - 0.2; t += SPBAR_G * 0.5) {
            sub_render(t, 0.18, 38, 0.32 + 0.20 * ((t - pbStart) / dur));   // D2
        }
        report("→ perc BREAK · accelerating ticks/woodticks + sub %.2f→%.2fs (%d ticks)",
               pbStart, pbEnd, hits);
        (void)n8;
    }

    // ── RISER into the drop — D1 → D5 across 500 ms ending on climax.
    //    (Lightning crack + brass slide rollercoaster pulled — they were
    //    competing with the brass theme entry and reading as "weird".)
    riser_render(CLIMAX_START - 0.50, 0.50, 26, 86, 0.45);
    report("→ pitch RISER · D1 → D5 sweep into drop @ %.2fs", CLIMAX_START - 0.50);

    // Extra stacked grenade removed — was too many transients on the climax
    // drop. The standard grenade @ CLIMAX_START + sub shockwave already
    // carry the impact.

    // ── "I need you" — pre-drop glitch stutter + main pitched up + echoes ──
    long inN = 0;
    float *ineed = try_load_sample("i-need-you.wav", &inN);
    if (ineed) {
        const double startT = CLIMAX_START - 1.5;
        const int glitchN = 3;
        const double glitchStep = 0.080;
        for (int g = 0; g < glitchN; g++) {
            PlaySweptOpts sp = {0};
            sp.start_rate = 1.10; sp.end_rate = 1.10;
            sp.max_dur_ms = 90; sp.pan = (g % 2 == 0) ? -0.28 : 0.28;
            sp.wet_send = 0.55; sp.fade = 0.020;
            play_sample_swept(startT + g * glitchStep, ineed, inN, 0.34, sp);
        }
        const double mainT = startT + glitchN * glitchStep;
        PlaySampleOpts pm = {0};
        pm.rate = 1.22; pm.pan = 0.0; pm.wet_send = 0.65; pm.fade = 0.010;
        play_sample(mainT, ineed, inN, 0.55, pm);
        // Two echoes
        const double edt[2] = {0.40, 0.85};
        const double eg[2]  = {0.30, 0.18};
        const double ep[2]  = {-0.45, 0.45};
        for (int e = 0; e < 2; e++) {
            PlaySampleOpts pe = {0};
            pe.rate = 1.22; pe.pan = ep[e]; pe.wet_send = 0.85; pe.fade = 0.010;
            play_sample(mainT + edt[e], ineed, inN, 0.55 * eg[e], pe);
        }
        report("→ \"I need you\" · 3-glitch stutter + main + 2 echoes @ %.2fs", startT);
        free(ineed);
    }

    // EARLY crowd-roar bed @ 38 s — quieter introduction that
    // overlaps with the crow intro chops (bb=18 ≈ 39.5 s) so the
    // crow scratches mix INTO a developing crowd. Main bridge roar
    // still at 53 s. (@jeffrey "mix crow with crowds sooner")
    long crn = 0;
    float *roar = try_load_sample("crowd-roar.wav", &crn);
    if (roar) {
        PlaySampleOpts poE = {0};
        poE.rate = 0.92; poE.wet_send = 0.80; poE.fade = 6.0;
        play_sample(38.00, roar, crn, 0.18, poE);
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.wet_send = 0.70; po.fade = 7.0;
        play_sample(53.00, roar, crn, 0.32, po);
        report("→ crowd-roar · early bed @ 38s + bridge build @ 53s");
        free(roar);
    }

    // crowd win @ t=63s
    long cwn = 0;
    float *win = try_load_sample("crowd-win.wav", &cwn);
    if (win) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.wet_send = 0.50; po.fade = 1.5;
        play_sample(63.00, win, cwn, 0.18, po);
        free(win);
    }

    // splash + flicks @ t=3 (gain boosted)
    long spn = 0;
    float *splash = try_load_sample("splash-intro.wav", &spn);
    if (splash) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.wet_send = 0.75; po.fade = 4.0;
        play_sample(3.0, splash, spn, 0.28, po);             // was 0.18
        report("→ splash · t=3 (boosted)");
        free(splash);
    }

    // ── OPENING card flick — FIRST flick is cards-fast.wav (the
    //    actual fast riffle sound, not the slow medium one). Second
    //    flick stays on cards-medium at high rate for variety.
    //    (@jeffrey "first card sound should be a fast flick, not the
    //     slow one, in the start of track")
    long cfN0 = 0;
    // Upgraded from cards-fast → cards-burst-a (the even faster
    // burst-style flick). Stays dry + sharp at rate 1.25 (a touch
    // up-pitched without going chipmunk).
    // (@jeffrey "can the card flick at beginning be the even faster one")
    float *cFast = try_load_sample("cards-burst-a.wav", &cfN0);
    if (!cFast) cFast = try_load_sample("cards-fast.wav", &cfN0);     // fallback
    if (cFast) {
        PlaySampleOpts po = {0};
        po.rate = 1.25; po.pan = -0.20;
        po.wet_send = 0.10; po.fade = 0.003;
        play_sample(0.30, cFast, cfN0, 0.80, po);
        free(cFast);
    }
    long cmN = 0;
    float *cMed = try_load_sample("cards-medium.wav", &cmN);
    if (cMed) {
        PlaySampleOpts po = {0};
        po.rate = 1.75; po.pan = 0.35; po.wet_send = 0.55; po.fade = 0.005;
        play_sample(1.10, cMed, cmN, 0.42, po);
        report("→ opening flicks · cards-fast @ 0.30s + cards-medium-squinchy @ 1.10s");
        free(cMed);
    }

    // ── Crow caw + Fibonacci scratching break (statement bb=18..23) ────
    // Main caw at statement startSec + 20*SPBAR (~42.2 s, not the wrong
    // t=20). Mystery chops at bb=18,19 (sound like vinyl scratches) →
    // caw reveal at bb=20 → Fibonacci-gap 16th-note scratch arc bb=21,22,23.
    long crwn = 0;
    float *crow = try_load_sample("crow.wav", &crwn);
    if (crow) {
        double crowT = 20.0;
        for (int s = 0; s < n_section_ranges; s++) {
            if (!strcmp(section_ranges[s].name, "statement")) {
                crowT = section_ranges[s].startSec + 18 * SPBAR_G; break;   // 20→18 bars: starts ~2.6s sooner
            }
        }
        // The main reveal caw — drier (0.65 → 0.35) so it hits forward
        // instead of sitting in the cathedral.
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0.0; po.wet_send = 0.35; po.fade = 0.05;
        play_sample(crowT, crow, crwn, 0.58, po);

        // Cawpoints inside the crow buffer (5 different caws at varied offsets)
        const double C1 = 0.10, C2 = 0.85, C3 = 1.55, C4 = 2.30, C5 = 3.05;
        const double SP16 = SPB_G / 4.0;

        // INTRO mystery chops (bb=18, 19) — sound like distorted scratches,
        // not yet recognizable as a crow.
        const struct { int bar; int i16; double cawOff; double rate; double ms; double g; double pan; } introChops[6] = {
            // Gains pushed up so the scratchy crow chops sit forward in
            // the mix again. (@jeffrey "not hearing the crow scratch as
            // much anymore")
            {-2,  4, C2, 1.50, 130, 1.45, -0.55},
            {-2, 10, C3, 0.55, 240, 1.50,  0.50},
            {-2, 14, C4, 1.40, 110, 1.40,  0.30},
            {-1,  2, C5, 0.45, 280, 1.55, -0.45},
            {-1,  8, C2, 1.40, 100, 1.45,  0.55},
            {-1, 12, C3, 0.55, 230, 1.55, -0.30},
        };
        // Intro chops — drier (0.85 → 0.18 wet) and each chop now has
        // a downward pitch SWEEP for scratch-character glitch (instead
        // of constant rate). Sharper fades so transients punch.
        // (@jeffrey "less reverb / more scratchy to glitch at start")
        for (int i = 0; i < 6; i++) {
            const double t = crowT + introChops[i].bar * SPBAR_G + introChops[i].i16 * SP16;
            PlaySweptOpts po2 = {0};
            // Alternate scratch direction per chop: even = down-scratch
            // (start hi, end low), odd = up-scratch (start low, end hi)
            const double base = introChops[i].rate;
            if (i % 2 == 0) { po2.start_rate = base * 1.25; po2.end_rate = base * 0.75; }
            else            { po2.start_rate = base * 0.80; po2.end_rate = base * 1.30; }
            po2.max_dur_ms = introChops[i].ms; po2.pan = introChops[i].pan;
            po2.wet_send = 0.18; po2.buf_offset = introChops[i].cawOff; po2.fade = 0.012;
            play_sample_swept(t, crow, crwn, introChops[i].g, po2);
        }

        // FIBONACCI scratch block (bb=21,22,23 = 3 bars after the caw).
        // Gap sequence (16ths): 1,2,3,5,8,13,13,8,5,3,2,1.
        const int FIB_GAPS[12] = {1, 2, 3, 5, 8, 13, 13, 8, 5, 3, 2, 1};
        const double caws[12] = {C5, C4, C3, C2, C1, C2, C3, C4, C5, C4, C3, C5};
        int stepCursor = 0;
        int fibCount = 0;
        for (int i = 0; i < 12; i++) {
            const int gap = FIB_GAPS[i];
            stepCursor += (i == 0) ? 0 : FIB_GAPS[i - 1];
            const int barOff = 1 + stepCursor / 16;
            const int i16    = stepCursor % 16;
            if (barOff > 3) break;
            const int isBigDrag = gap >= 8;
            const int isSmallChop = gap <= 2;
            const double rate = isBigDrag ? 0.45 : (isSmallChop ? 1.55 : 1.00);
            const double ms   = isBigDrag ? 360 : (isSmallChop ? 130 : 200);
            const double g    = isBigDrag ? 1.35 : 1.25;
            const double pan  = (i % 2 == 0) ? -0.50 : 0.50;
            const double t = crowT + barOff * SPBAR_G + i16 * SP16;
            PlaySweptOpts po2 = {0};
            po2.start_rate = rate; po2.end_rate = rate;
            po2.max_dur_ms = ms; po2.pan = pan;
            po2.wet_send = 0.85; po2.buf_offset = caws[i]; po2.fade = 0.025;
            play_sample_swept(t, crow, crwn, g, po2);
            fibCount++;
        }
        // Final screwed-down crawl at bb=23 end
        {
            const double t = crowT + 3 * SPBAR_G + 14 * SP16;
            PlaySweptOpts po2 = {0};
            po2.start_rate = 0.95; po2.end_rate = 0.18;
            po2.max_dur_ms = 720; po2.pan = 0.0;
            po2.wet_send = 0.85; po2.buf_offset = C5; po2.fade = 0.025;
            play_sample_swept(t, crow, crwn, 1.40, po2);
            fibCount++;
        }

        // BRIDGE EXTENSION — sparser slow chops blending into crowd.
        // Gains pulled hard (0.95→0.40, 0.75→0.32) + shorter ms because the
        // pitched-down chops at rate 0.35/0.65 were reading as "glups"
        // around 45-55s, overlapping the next jeffrey/TTS pass.
        const int BRIDGE_FIB[8] = {3, 5, 8, 13, 21, 13, 8, 5};
        const double bCaws[8] = {C1, C3, C5, C4, C2, C3, C5, C4};
        int bcursor = 0;
        int bridgeCount = 0;
        for (int i = 0; i < 8; i++) {
            const int gap = BRIDGE_FIB[i];
            bcursor += (i == 0) ? 0 : BRIDGE_FIB[i - 1];
            const int barOff = 4 + bcursor / 16;
            const int i16    = bcursor % 16;
            if (barOff > 12) break;
            const int isBig = gap >= 13;
            const double rate = isBig ? 0.45 : 0.75;     // less extreme pitch-down
            const double ms   = isBig ? 380  : 260;      // shorter chops
            const double g    = isBig ? 0.40 : 0.32;     // halved
            const double pan  = (i % 2 == 0) ? -0.65 : 0.65;
            const double t = crowT + barOff * SPBAR_G + i16 * SP16;
            PlaySweptOpts po2 = {0};
            po2.start_rate = rate; po2.end_rate = rate;
            po2.max_dur_ms = ms; po2.pan = pan;
            po2.wet_send = 0.85; po2.buf_offset = bCaws[i]; po2.fade = 0.060;
            play_sample_swept(t, crow, crwn, g, po2);
            bridgeCount++;
        }
        report("→ crow break · 6 intro chops + reveal @ %.2fs + %d Fibonacci 16ths + %d bridge ext",
               crowT, fibCount, bridgeCount);
        free(crow);
    }

    // typewriter keys — 7 hand-placed punches across first 2 bars, each
    // with a soft D2 sub pulse following the same fade-in curve so the
    // intro has "punch + depth", not just clicks.
    long tyn = 0;
    float *typer = try_load_sample("typewriter-key.wav", &tyn);
    if (typer) {
        const struct { double t; double rate; double g; double pan; } keys[7] = {
            { 0.18, 1.00, 0.65, -0.30 },
            { 0.42, 1.08, 0.60,  0.35 },
            { 0.78, 0.96, 0.62, -0.20 },
            { 1.12, 1.04, 0.58,  0.30 },
            { 1.55, 1.10, 0.55, -0.40 },
            { 1.92, 0.98, 0.52,  0.25 },
            { 2.34, 1.06, 0.48,  0.00 },
        };
        for (int k = 0; k < 7; k++) {
            // Curved fade-in (squared) — pushes the LOW-end content
            // later in the intro so the first 1-2 seconds aren't
            // bass-dominated. (@jeffrey "too much low freq too fast
            //  in the first few seconds / come in slower / more after")
            const double linK   = (double)k / 6.0;
            const double topVol = 0.12 + (1.0 - 0.12) * linK;   // typewriter ramp
            const double bassFr = 0.05 + (1.0 - 0.05) * linK * linK;   // squared for bass
            const double tJ = keys[k].t + hum(0.012);
            PlaySampleOpts po = {0};
            po.rate = keys[k].rate; po.pan = keys[k].pan;
            po.wet_send = 0.25; po.fade = 0.003;
            play_sample(tJ, typer, tyn, keys[k].g * topVol, po);
            // Sub thump dropped from 0.14 → 0.06 base and ramped via
            // squared curve so it barely registers on the first few
            // keys and blooms in toward the drop.
            sub_render(tJ, 0.18, 38, 0.06 * bassFr);
            // Kick gain dropped 0.35 → 0.18 and also squared-ramp.
            // (@jeffrey "kicks on the initial key punch sounds")
            kick_render(tJ, 1.0, 0.18 * bassFr, 0.0);
        }
        free(typer);
        report("→ typewriter · 7 key punches w/ soft D2 sub thumps across first 2 bars");
    }

    // skid snares (statement 20-47s) — clap with playSampleSwept rate ramp
    long skn = 0;
    float *clap = try_load_sample("clap.wav", &skn);
    if (clap) {
        for (int b = 0; b < 21; b++) {
            const double tBar = (12 + b) * SPBAR_G;     // bars 12..32 (statement)
            if (tBar < 20.0 || tBar > 47.0) continue;
            PlaySweptOpts po = {0};
            po.start_rate = 1.0; po.end_rate = 0.70;
            po.pan = -0.10; po.wet_send = 0.50; po.max_dur_ms = 220; po.fade = 0.015;
            play_sample_swept(tBar + 1 * SPB_G + hum(0.005), clap, skn, 0.50, po);
            po.pan = 0.10;
            play_sample_swept(tBar + 3 * SPB_G + hum(0.005), clap, skn, 0.50, po);
        }

        // 808 clap pattern 72-95s — straight claps on 2+4. Wet send
        // pulled DOWN (0.50 → 0.12) so the claps punch dry/forward
        // around 1:17 instead of being drowned in cathedral.
        // (@jeffrey "around 1:17 bring the claps more out of reverb")
        for (int b = 0; b < 32; b++) {
            const double tBar = (36 + b) * SPBAR_G;
            if (tBar < 72.0 || tBar > 95.0) continue;
            const double g = 0.50;     // bumped from 0.42 since wet's gone
            PlaySampleOpts po = {0};
            po.rate = 1.0; po.pan = -0.08 + hum(0.05); po.wet_send = 0.12; po.fade = 0.01;
            play_sample(tBar + 1 * SPB_G + hum(0.004), clap, skn, g, po);
            po.pan = 0.08 + hum(0.05);
            play_sample(tBar + 3 * SPB_G + hum(0.004), clap, skn, g, po);
        }
        free(clap);
        report("→ skid + 808 claps placed");
    }

    // crow + crowd ambient blend through develop + climax
    long crAn = 0, roAn = 0;
    float *crowAmb = try_load_sample("crow.wav", &crAn);
    float *roarAmb = try_load_sample("crowd-roar.wav", &roAn);
    if (crowAmb) {
        for (double t = 90.0; t < 140.0; t += 4.0 + rng() * 3.0) {
            PlaySweptOpts po = {0};
            po.start_rate = 0.90 + rng() * 0.35;
            po.end_rate   = po.start_rate * (0.85 + rng() * 0.3);
            po.pan = (rng() * 2.0 - 1.0) * 0.7;
            po.wet_send = 0.65; po.max_dur_ms = 380 + rng() * 220;
            po.fade = 0.03;
            play_sample_swept(t + hum(0.04), crowAmb, crAn, 0.06 + rng() * 0.04, po);
        }
        free(crowAmb);
    }
    if (roarAmb) {
        PlaySampleOpts po = {0};
        po.rate = 0.92; po.pan = -0.20; po.wet_send = 0.55; po.fade = 5.5;
        play_sample(108.00, roarAmb, roAn, 0.13, po);
        po.rate = 1.08; po.pan =  0.25;
        play_sample(115.00, roarAmb, roAn, 0.11, po);
        free(roarAmb);
        report("→ crow + crowd ambient blend (develop+climax)");
    }

    // melon stab + squish (sound design sweetener around t=2.45)
    long msn = 0;
    float *melon = try_load_sample("melon-stab.wav", &msn);
    if (melon) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0.10; po.wet_send = 0.85; po.fade = 0.003;
        play_sample(2.45, melon, msn, 0.85, po);
        free(melon);
    }

    // ── drum-couplets / Tri-Tone beat drop ─────────────────────────────
    // Tri-Tone (imessage-ding) glitches into the first kick of statement
    // as the beat-drop anchor: 4 stutters leading into the drop + main
    // half-speed ding on the drop + 3 decaying echo taps.
    long dgn = 0;
    float *triTone = try_load_sample("imessage-ding.wav", &dgn);
    if (triTone) {
        double stSec = 0;
        for (int s = 0; s < n_section_ranges; s++) {
            if (!strcmp(section_ranges[s].name, "statement")) {
                stSec = section_ranges[s].startSec; break;
            }
        }
        // 4 glitch stutters into the drop
        for (int g = 0; g < 4; g++) {
            const double tg = stSec - (4 - g) * 0.075;
            PlaySweptOpts po = {0};
            po.start_rate = 0.70; po.end_rate = 0.70;
            po.max_dur_ms = 80; po.pan = (g % 2 == 0) ? -0.25 : 0.25;
            po.wet_send = 0.45;
            play_sample_swept(tg, triTone, dgn, 0.70, po);
        }
        // Main ding ON the drop
        PlaySampleOpts mp = {0};
        mp.rate = 0.5; mp.pan = 0.0; mp.wet_send = 0.55;
        play_sample(stSec, triTone, dgn, 1.10, mp);
        // 3 decaying echoes
        const double edt[3] = {0.42, 0.86, 1.42};
        const double eg[3]  = {0.34, 0.21, 0.13};
        const double epan[3] = {-0.45, 0.45, -0.30};
        for (int e = 0; e < 3; e++) {
            PlaySampleOpts po = mp; po.pan = epan[e]; po.wet_send = 0.85;
            play_sample(stSec + edt[e], triTone, dgn, 0.55 * eg[e], po);
        }
        free(triTone);
        report("→ Tri-Tone · drop stamp (4 glitches + main + 3 echoes) @ %.2fs", stSec);
    }

    // ── card-flip lead-in + grenade #2 on the LAST DROP ────────────────
    // 4 card flips zip into the climax drop (4 cards*.wav at decreasing
    // offsets), then the grenade lands EXACTLY on the climax downbeat.
    double climaxSec = 0;
    for (int s = 0; s < n_section_ranges; s++) {
        if (!strcmp(section_ranges[s].name, "climax")) {
            climaxSec = section_ranges[s].startSec; break;
        }
    }
    if (climaxSec > 0) {
        const char *cardNames[4] = {
            "cards-burst-a.wav", "cards-hard.wav",
            "cards-burst-b.wav", "cards-fast.wav",
        };
        const double offsets[4] = {-0.62, -0.46, -0.30, -0.14};
        const double rates[4]   = {1.10, 1.30, 1.00, 1.40};
        const double pans[4]    = {-0.6, 0.55, -0.4, 0.5};
        for (int f = 0; f < 4; f++) {
            long cn = 0;
            float *cb = try_load_sample(cardNames[f], &cn);
            if (!cb) continue;
            PlaySampleOpts po = {0};
            po.rate = rates[f]; po.pan = pans[f]; po.wet_send = 0.50; po.fade = 0.02;
            play_sample(climaxSec + offsets[f], cb, cn, 0.55, po);
            free(cb);
        }
        report("→ card-flip lead-in · 4 zips into the climax drop");
    }

    // ── tape noise bed (analog warmth across first 24 s) ───────────────
    // pink-ish warm rumble + tape-hiss high band + slow LFO breathing.
    // Per-sample additive synthesis directly into the L/R/WL/WR buses.
    {
        const double bedDur = 24.0;
        const long noiseEnd = (long)(bedDur * SR); long endI = noiseEnd < N ? noiseEnd : N;
        double n1L=0,n2L=0,n3L=0, n1R=0,n2R=0,n3R=0;
        double hL=0, hR=0;
        for (long i = 0; i < endI; i++) {
            const double wL = rng() * 2.0 - 1.0;
            const double wR = rng() * 2.0 - 1.0;
            n1L = 0.99*n1L + 0.01*wL; n2L = 0.96*n2L + 0.04*n1L; n3L = 0.86*n3L + 0.14*n2L;
            n1R = 0.99*n1R + 0.01*wR; n2R = 0.96*n2R + 0.04*n1R; n3R = 0.86*n3R + 0.14*n2R;
            const double warmL = n3L * 6.0, warmR = n3R * 6.0;
            const double hssL = wL - hL; hL = hL * 0.85 + wL * 0.15;
            const double hssR = wR - hR; hR = hR * 0.85 + wR * 0.15;
            const double t = (double)i / SR;
            double env;
            if (t < 2.0) env = t / 2.0;
            else if (t < 15.82) env = 1.0;
            else {
                env = 1.0 - (t - 15.82) / 8.18;
                if (env < 0) env = 0;
            }
            const double breathe = 0.88 + 0.12 * sin(TAU * 0.11 * t);
            // JS-exact tape noise levels (matches DistroKid release).
            const double vL = (warmL * 0.017 + hssL * 0.005) * env * breathe;
            const double vR = (warmR * 0.017 + hssR * 0.005) * env * breathe;
            L[i]  += (float)vL;
            R[i]  += (float)vR;
            WL[i] += (float)(vL * 0.30);
            WR[i] += (float)(vR * 0.30);
        }
        report("→ tape noise bed · 0-24s analog warmth");
    }

    // ── crow + cards "bones" — burst of cards-burst + rattle-intro right
    //    after the crow caw at 20s (~bb=20 in statement)
    long crowBoneN = 0;
    float *crowBone = try_load_sample("crow.wav", &crowBoneN);
    if (crowBone) {
        // bones bursts at crowT + offsets
        const double crowT = 20.0;
        const struct { const char *name; double dt, g, pan, rate, wet; } bursts[4] = {
            { "cards-burst-a.wav", 0.45, 0.18,  0.55, 1.00, 0.70 },
            { "cards-hard.wav",    0.95, 0.22, -0.45, 1.10, 0.65 },
            { "cards-burst-b.wav", 1.55, 0.16,  0.40, 0.92, 0.75 },
            { "cards-hard.wav",    2.20, 0.14, -0.55, 1.20, 0.80 },
        };
        for (int i = 0; i < 4; i++) {
            long bn = 0;
            float *bb = try_load_sample(bursts[i].name, &bn);
            if (!bb) continue;
            PlaySampleOpts po = {0};
            po.rate = bursts[i].rate; po.pan = bursts[i].pan;
            po.wet_send = bursts[i].wet; po.fade = 0.03;
            play_sample(crowT + bursts[i].dt, bb, bn, bursts[i].g, po);
            free(bb);
        }
        // rattle-intro bones
        long rin = 0;
        float *rib = try_load_sample("rattle-intro.wav", &rin);
        if (rib) {
            PlaySampleOpts po = {0};
            po.rate = 1.05; po.pan =  0.15; po.wet_send = 0.70; po.fade = 0.05;
            play_sample(crowT + 0.65, rib, rin, 0.22, po);
            po.rate = 0.88; po.pan = -0.25; po.wet_send = 0.80;
            play_sample(crowT + 1.80, rib, rin, 0.18, po);
            free(rib);
        }
        free(crowBone);
        report("→ crow bones · cards + rattle-intro burst after caw");
    }

    // ── crowd D-minor arpeggio — EXTENDED + DENSER for the 1:06 zone ──
    // Original 9 notes from 58s, now 18 notes spanning 58-66s, then a
    // SECOND ascending arpeggio at 66-72s. Plus crow chops layered on
    // every other beat for the "1:06 scratch amp-up" the user asked for.
    long roaN = 0;
    float *roarArp = try_load_sample("crowd-roar.wav", &roaN);
    if (roarArp) {
        // Pass 1 — 58-66s, original D-min arp expanded to 18 notes
        const int arpSemis1[18] = {
            0, 3, 7, 12, 15, 12, 7, 3,
            0, 5, 10, 15, 17, 15, 10, 5, 3, 0,
        };
        for (int i = 0; i < 18; i++) {
            const double t = 58.0 + i * (SPB_G * 0.5);     // 8th notes
            const double rate = pow(2.0, arpSemis1[i] / 12.0);
            const double pan = (i % 2 == 0) ? -0.65 : 0.65;
            const double off = 1.5 + fmod(i * 1.8, 14);
            const double arpG = 0.18 + (double)i / 18.0 * 0.30;
            PlaySweptOpts po = {0};
            po.start_rate = rate; po.end_rate = rate;
            po.max_dur_ms = 320; po.pan = pan;
            po.wet_send = 0.75; po.buf_offset = off; po.fade = 0.04;
            play_sample_swept(t, roarArp, roaN, arpG, po);
        }
        // Pass 2 — 66-72s climbing arp, denser, more aggressive
        const int arpSemis2[16] = {
            0, 3, 7, 10, 12, 15, 17, 19,
            15, 12, 17, 19, 22, 19, 15, 12,
        };
        for (int i = 0; i < 16; i++) {
            const double t = 66.0 + i * (SPB_G * 0.5);
            const double rate = pow(2.0, arpSemis2[i] / 12.0);
            const double pan = (i % 3 == 0) ? -0.75 : ((i % 3 == 1) ? 0.0 : 0.75);
            const double off = 2.5 + fmod(i * 1.3, 12);
            const double arpG = 0.32 + (double)i / 32.0;
            PlaySweptOpts po = {0};
            po.start_rate = rate; po.end_rate = rate;
            po.max_dur_ms = 280; po.pan = pan;
            po.wet_send = 0.80; po.buf_offset = off; po.fade = 0.035;
            play_sample_swept(t, roarArp, roaN, arpG, po);
        }
        free(roarArp);
        report("→ crowd D-minor arpeggio · EXTENDED · 34 notes 58-72s");
    }
    // Crow scratch overlay 58-72s — sparse counterpoint to the crowd arp
    long crArpN = 0;
    float *crowExtraArp = try_load_sample("crow.wav", &crArpN);
    if (crowExtraArp) {
        const struct { double t; double rate; double ms; double off; double pan; double g; } crowChops[8] = {
            { 60.5, 1.40, 200, 0.10, -0.55, 0.55 },
            { 62.0, 0.65, 260, 2.30,  0.55, 0.50 },
            { 63.8, 1.55, 180, 1.55, -0.40, 0.52 },
            { 65.5, 0.85, 240, 3.05,  0.45, 0.55 },
            { 67.2, 1.70, 160, 0.85, -0.65, 0.58 },
            { 68.9, 0.95, 220, 2.30,  0.60, 0.55 },
            { 70.5, 1.35, 200, 1.55, -0.50, 0.50 },
            { 71.8, 0.55, 320, 3.05,  0.30, 0.45 },
        };
        for (int i = 0; i < 8; i++) {
            PlaySweptOpts po = {0};
            po.start_rate = crowChops[i].rate; po.end_rate = crowChops[i].rate;
            po.max_dur_ms = crowChops[i].ms; po.pan = crowChops[i].pan;
            po.wet_send = 0.85; po.buf_offset = crowChops[i].off; po.fade = 0.025;
            play_sample_swept(crowChops[i].t, crowExtraArp, crArpN,
                              crowChops[i].g, po);
        }
        free(crowExtraArp);
        report("→ crow scratch overlay · 8 chops 60-72s (1:06 zone amp-up)");
    }

    // ── grand piano portal @ climax bars 7-8 (~120-123s) ──────────────
    // 2-bar window: sustained chord on each downbeat + 8-eighth arpeggio,
    // with a lead-in shimmer before and vortex cascade after.
    if (climaxSec > 0) {
        const double portalStart = climaxSec + 7 * SPBAR_G;
        // D major then E minor (per JS)
        const struct { int root; int q3; int q5; } pc[2] = {
            { 50, 4, 7 }, { 52, 3, 7 },
        };
        for (int b = 0; b < 2; b++) {
            const double tBar = portalStart + b * SPBAR_G;
            const int qs[3] = { 0, pc[b].q3, pc[b].q5 };
            // sustained triad
            for (int t = 0; t < 3; t++) {
                PianoOpts po = {0}; po.sus = 1.0; po.bits = 16; po.hold = 1;
                po.pan = hum(0.15);
                piano_render(tBar + hum(0.004), SPBAR_G * 0.98, pc[b].root + qs[t] + 12, 0.22, po);
            }
            // bass octave
            PianoOpts pob = {0}; pob.sus = 1.0; pob.bits = 16; pob.hold = 1; pob.pan = -0.30;
            piano_render(tBar, SPBAR_G * 0.95, pc[b].root, 0.16, pob);
            // arpeggio
            const int seq[8] = {
                pc[b].root,             pc[b].root + pc[b].q3,      pc[b].root + pc[b].q5,    pc[b].root + 12,
                pc[b].root + pc[b].q3 + 12, pc[b].root + pc[b].q5,  pc[b].root + pc[b].q3,    pc[b].root,
            };
            for (int k = 0; k < 8; k++) {
                const double tk = tBar + k * (SPB_G / 2.0) + hum(0.004);
                PianoOpts pp = {0}; pp.sus = 1.0; pp.bits = 16; pp.hold = 1;
                pp.pan = (k % 2 == 0) ? -0.35 : 0.35;
                piano_render(tk, SPB_G * 0.55, seq[k] + 24, 0.14, pp);
            }
        }
        // lead-in shimmer
        const double leadInStart = portalStart - SPBAR_G * 0.5;
        const int leadInSeq[8] = {50, 54, 57, 62, 66, 69, 74, 78};
        for (int k = 0; k < 8; k++) {
            PianoOpts pp = {0}; pp.sus = 1.0; pp.bits = 16; pp.hold = 1;
            pp.pan = (k - 4) * 0.10;
            piano_render(leadInStart + k * 0.07, 0.40, leadInSeq[k], 0.10, pp);
        }
        // vortex cascade
        const double vortexStart = portalStart + 2 * SPBAR_G;
        const int vortexSeq[8] = {86, 81, 78, 74, 69, 66, 62, 57};
        for (int k = 0; k < 8; k++) {
            PianoOpts pp = {0}; pp.sus = 1.0; pp.bits = 16; pp.hold = 1;
            pp.pan = (4 - k) * 0.10;
            piano_render(vortexStart + k * 0.06, 0.35, vortexSeq[k], 0.10, pp);
        }
        report("→ grand piano PORTAL · 2 bars @ %.2fs + lead-in + vortex", portalStart);
    }

    // ── drippy flower transition @ ~142s ───────────────────────────────
    // Cascade of descending bell pings smear the climax → coda seam.
    {
        const double drippyT0 = 140.5;
        const int drippyNotes[7] = {98, 93, 89, 86, 82, 78, 74};
        for (int i = 0; i < 7; i++) {
            BellOpts bo = {0};
            bo.pan = (i % 2 == 0 ? -0.30 : 0.30);
            bo.dec_tau = 2.5 - i * 0.15; bo.atk = 0.030; bo.wet_send = 0.80;
            bo.fizzle_on = 1;
            bell_render(drippyT0 + i * 0.32, drippyNotes[i], 0.10, bo);
        }
        report("→ drippy flower · 7-bell descending cascade @ %.2fs", 140.5);
    }

    // ── grenade #1 @ 12.90s sub layer (D1 shockwave) ───────────────────
    // The grenade itself is loaded above (post_arrangement_grenade); here
    // we add the sub shockwave that goes WITH it.
    sub_render(12.90, 0.55, 26, 0.65);
    if (climaxSec > 0) sub_render(climaxSec, 0.65, 26, 1.05);

    // ── 2nd DROP: BIG POUND + WHIP + NEIGH @ CLIMAX_START ─────────
    // A massive impact stack right at the AC-stamp climax drop:
    //   • Deep sub-shockwave (D1 @ 26 MIDI) for the BIG POUND
    //   • Single whip CRACK on the downbeat
    //   • Horse neigh moved from 132.65 → CLIMAX_START + 0.18 so the
    //     rider's voice rides the drop instead of the outro.
    // (@jeffrey "2nd drop more of a big pound / whip sound right at
    //  the drop / move the horse neigh to the drop too")
    sub_render(CLIMAX_START, 0.85, 26, 0.70);     // 1.10 → 0.70 (headroom)
    sub_render(CLIMAX_START, 0.65, 14, 0.55);     // 0.85 → 0.55
    long whn = 0;
    float *whip = try_load_sample("whip.wav", &whn);
    if (whip) {
        // Whip CRACK on the downbeat removed — the UT2004 shock rifle
        // BANG is doing the impact job now. (@jeffrey "whip crack can
        // go away now")
        // 7-chop windup burst moved from 132.00 → CLIMAX_START - 1.30,
        // so the burst leads INTO the drop instead of decorating the
        // post-drop. Each chop a 16th apart → 7 × ~0.082 s ≈ 0.57 s of
        // chops landing on the downbeat. (@jeffrey "gallop and sound
        //  right before that should all go around the drop again")
        const double SP16W = SPB_G / 4.0;
        const double windupStart = CLIMAX_START - 1.30;
        for (int i = 0; i < 7; i++) {
            const double tW = windupStart + i * SP16W;
            const double rate = 1.0 + (i % 2 == 0 ? -0.05 : 0.05);
            const double pan  = (i % 2 == 0) ? -0.55 : 0.55;
            const double g    = 1.20 + i * 0.12;     // crescendo INTO drop
            PlaySweptOpts po = {0};
            po.start_rate = rate; po.end_rate = rate;
            po.max_dur_ms = 95; po.pan = pan; po.wet_send = 0.20; po.fade = 0.003;
            play_sample_swept(tW, whip, whn, g, po);
        }
        free(whip);
        report("→ whip · windup burst %.2fs→drop + CRACK @ %.2fs",
               windupStart, CLIMAX_START);
    }
    long nen = 0;
    float *neigh = try_load_sample("neigh.wav", &nen);
    if (neigh) {
        // Held back to the next downbeat (~2:00 = CLIMAX_START + 7 bars)
        // so the shock rifle BANG has clean space at the drop itself.
        // (@jeffrey "hold the horse neigh and gallop until the next
        //  beat at 2:00 / so the shock rifle has space")
        const double horseT = CLIMAX_START + 7.0 * SPBAR_G;
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0.10; po.wet_send = 0.25; po.fade = 0.02;
        play_sample(horseT + 0.18, neigh, nen, 1.60, po);
        free(neigh);
        report("→ neigh · held to next downbeat @ %.2fs (rider's voice)",
               horseT + 0.18);
    }
    long trn = 0;
    float *train = try_load_sample("train.wav", &trn);
    if (train) {
        // Train comes in SOONER (152 → 144) and ends LOUDER (0.40 →
        // 0.90). 6s of build before the outro bells. (@jeffrey "train
        // should come in a bit sooner and end louder")
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0; po.wet_send = 0.45; po.fade = 3.0;
        play_sample(144.0, train, trn, 0.90, po);
        free(train);
        report("→ train · sweeping in @ 144s (sooner+louder outro)");
    }

    // ── END-OF-TRACK OUTRO STACK ───────────────────────────────────
    // High sine-bell cascade + opening-meow callback + fast card flick.
    // Sits over the train bed in 147-158s. (@jeffrey "high pitched
    // sine bells at the end / meow reminder of the beginning / fast
    // card flick")
    {
        // 5-bell ascending-then-descending sine cascade
        const int bellNotes[5] = {96, 100, 103, 100, 96};
        const double bellT0 = 147.0;
        const double bellStep = 1.40;
        for (int i = 0; i < 5; i++) {
            BellOpts bo = {0};
            bo.atk = 0.020; bo.dec_tau = 3.0; bo.wet_send = 0.65;
            bo.fizzle_on = 0;    // clean sine bell, no fizzle tail
            bo.pan = (i % 2 == 0) ? -0.30 : 0.30;
            bell_render(bellT0 + i * bellStep, bellNotes[i], 0.18, bo);
        }
        report("→ outro bells · 5 high sines (mid-96..103) @ 147-152s");
    }
    {
        // Opening-meow callback at ~155s
        long mN = 0;
        // Different kitten this time — meow-3 instead of meow, MOVED
        // to land RIGHT on the final kick (158.5s). (@jeffrey "can
        // the cat meow at the end happen RIGHT on the kick there?")
        float *meow = try_load_sample("meow-3.wav", &mN);
        if (!meow) meow = try_load_sample("meow.wav", &mN);
        if (meow) {
            PlaySampleOpts po = {0};
            po.rate = 1.0; po.pan = -0.30;
            po.wet_send = 0.55; po.fade = 0.04;
            play_sample(158.5, meow, mN, 0.55, po);
            free(meow);
            report("→ outro meow · meow-3 LANDS ON final kick @ 158.50s");
        }
    }
    {
        // Fast card flick at ~156s — short squinchy outro detail
        long cfn = 0;
        float *cf = try_load_sample("cards-fast.wav", &cfn);
        if (cf) {
            PlaySampleOpts po = {0};
            po.rate = 1.45; po.pan = 0.35;
            po.wet_send = 0.45; po.fade = 0.005;
            play_sample(156.5, cf, cfn, 0.40, po);
            free(cf);
            report("→ outro card flick · @ 156.5s");
        }
    }

    // ── OPENING BUBBLE — REMOVED (@jeffrey "lets lose the bubble
    //    in the start"). Kept the BUBBLE RETURNS at 55-62s only.
    if (0) {
        const double tBubble = 1.12;
        bubble_render(tBubble,        8.5, 0.55, 0.045, -0.65, 0.95, 1.0);
        bubble_render(tBubble + 0.02, 9.5, 0.55, 0.040, +0.65, 0.95, 1.0);

        // RETURN OF THE BUBBLES — 3 wet bubbles scattered across
        // 55-63 s, beat-quantized, alternating pans + sizes.
        // (@jeffrey "they could return a bit around :55-1:03")
        const double beatLen = 60.0 / BPM;
        const double rBeats[3] = { 167.0, 173.0, 187.0 };
        const double rRads[3]  = { 8.0, 11.0, 6.5 };
        const double rPans[3]  = { -0.30, +0.45, -0.20 };
        for (int i = 0; i < 3; i++) {
            const double tr = rBeats[i] * beatLen;
            bubble_render(tr, rRads[i], 0.45, 0.11, rPans[i], 0.90, 1.0);
        }
        report("→ wet bubble returns · 3 bubbles @ 55-62s");
    }

    // ── "BUNNY" VOICE STAMP @ ~1:18 — freesound mrgreaper #223484
    //    (CC-BY 4.0), trimmed to just the "bunny" word, dropped onto
    //    the beat near the j=11 bunnies phrase (78.73s) so a real
    //    speaking voice punctuates the synth bunnies. (@jeffrey "can
    //    we freesound a girl saying 'bunnies' / super clear bunny
    //    saying / around the 1:20 bunnies phrase")
    {
        long bvN = 0;
        float *bv = try_load_sample("bunny-word.wav", &bvN);
        if (bv) {
            // Beat-quantize: 78.87s = beat 239 at 182 BPM
            const double beatLen = 60.0 / BPM;
            const double tBunny = round(78.73 / beatLen) * beatLen;
            PlaySampleOpts po = {0};
            po.rate = 1.05;             // slight up-pitch for clarity
            po.pan  = 0.10;
            po.wet_send = 0.18;         // dry — needs to land clearly
            po.fade = 0.005;
            play_sample(tBunny, bv, bvN, 0.85, po);
            free(bv);
            report("→ bunny voice · CC-BY mrgreaper @ %.3fs (clear stamp)", tBunny);
        }
    }

    // ── TOM BREAK @ 1:12 — quick bongo/floor-tom thuds (Logicogonist
    //    209877 CC0 floor tom + MrRentAPercussionist 455505 LP hi-bongo
    //    CC-BY 4.0). Just one bar of thuds tucked into the bridge groove
    //    where the user heard the "cool vibe with that vocal".
    //    (@jeffrey "at 1:12 it's a cool vibe with that vocal / TOM TOM
    //    drums like bongo thuds / source from freesound / plot one or
    //    two for a quick tom break")
    {
        long tlN = 0, thN = 0;
        float *tomLo = try_load_sample("tom-low.wav", &tlN);
        float *tomHi = try_load_sample("tom-high.wav", &thN);
        if (tomLo || tomHi) {
            // 6 thuds across ~1.2 s starting at 72.00 s — alternating
            // pitches + pans for a stereo flurry.
            const struct { double t; int hi; double rate; double pan; double g; } thuds[6] = {
                { 72.00, 0, 1.00, -0.40, 0.72 },
                { 72.20, 1, 1.00,  0.45, 0.62 },
                { 72.40, 0, 0.95, -0.25, 0.66 },
                { 72.74, 0, 1.05,  0.20, 0.78 },
                { 72.94, 1, 1.10, -0.45, 0.58 },
                { 73.22, 0, 0.92,  0.35, 0.70 },
            };
            for (int i = 0; i < 6; i++) {
                float *buf = thuds[i].hi ? tomHi : tomLo;
                const long nN   = thuds[i].hi ? thN   : tlN;
                if (!buf || nN <= 0) continue;
                PlaySampleOpts po = {0};
                po.rate = thuds[i].rate; po.pan = thuds[i].pan;
                po.wet_send = 0.30; po.fade = 0.004;
                play_sample(thuds[i].t, buf, nN, thuds[i].g, po);
            }
            if (tomLo) free(tomLo);
            if (tomHi) free(tomHi);
            report("→ tom break · 6 bongo thuds 72.00-73.22s (low+high alternating)");
        }
    }

    // ── ENCORE SCRATCH @ ~2:08 — quick crow + crowd-win flurry
    //    riding the snare rush tail (after the wiped kicks, into the
    //    last drop area). 4 crow chops + 2 crowd-win swept stabs.
    //    PLUS a SCREWED tail: each successive chop slows + pitches
    //    DOWN with exponentially growing reverb send, "flying away"
    //    into the beyond. (@jeffrey "the 2:08 crow be scratched and
    //    screwed into the beyond / like exponential reverb flying away")
    {
        long encN = 0;
        float *encCrow = try_load_sample("crow.wav", &encN);
        if (encCrow) {
            // 4 scratchy crow chops 127.0 → 128.6s (normal flurry)
            const double encStart = 127.0;
            const double encStep  = 0.40;
            const double encOffs[4] = { 0.85, 1.55, 0.10, 2.30 };
            const double encRates[4] = { 1.45, 0.65, 1.30, 0.55 };
            const double encPans[4]  = { -0.55, 0.55, -0.30, 0.40 };
            for (int i = 0; i < 4; i++) {
                PlaySweptOpts po = {0};
                const double base = encRates[i];
                if (i % 2 == 0) { po.start_rate = base * 1.20; po.end_rate = base * 0.80; }
                else            { po.start_rate = base * 0.85; po.end_rate = base * 1.25; }
                po.max_dur_ms = 180; po.pan = encPans[i];
                po.wet_send = 0.25; po.buf_offset = encOffs[i]; po.fade = 0.012;
                play_sample_swept(encStart + i * encStep, encCrow, encN, 1.20, po);
            }
            // SCREWED TAIL — 5 progressively slower/lower chops with
            // exponentially growing wet send. Spaced 0.45 → 0.95s
            // apart so they spread out + thin into the distance.
            const double screwStart = 128.80;
            const double screwOffs[5] = { 1.20, 1.80, 2.50, 0.40, 1.95 };
            const double screwRates[5] = { 0.55, 0.42, 0.32, 0.24, 0.18 };
            const double screwPans[5]  = { -0.40, +0.55, -0.65, +0.45, 0.0 };
            const double screwSpacing[5] = { 0.45, 0.60, 0.78, 0.95, 1.20 };
            double tScrew = screwStart;
            for (int i = 0; i < 5; i++) {
                PlaySweptOpts po = {0};
                const double base = screwRates[i];
                // Each successive chop sweeps progressively wider DOWN
                po.start_rate = base * (1.10 - i * 0.04);
                po.end_rate   = base * (0.75 - i * 0.06);
                po.max_dur_ms = 280 + i * 70;     // longer chops
                po.pan        = screwPans[i];
                // Exponential wet bloom: 0.30 → 0.65 → 0.85 → 0.95 → 1.0
                po.wet_send   = 0.30 + 0.18 * i;
                if (po.wet_send > 1.0) po.wet_send = 1.0;
                po.buf_offset = screwOffs[i];
                po.fade = 0.020 + i * 0.010;
                // Gain tapers DOWN as wet grows (perceptual: reverb tail expands)
                const double gn = 0.95 - i * 0.13;
                play_sample_swept(tScrew, encCrow, encN, gn, po);
                tScrew += screwSpacing[i];
            }
            free(encCrow);
            report("→ encore scratch · 4 crow chops + 5 SCREWED tail 128.80s→%.2fs (flying away)",
                   tScrew);
        }
        long encWN = 0;
        float *encWin = try_load_sample("crowd-win.wav", &encWN);
        if (encWin) {
            // 2 crowd-win swept stabs framing the crow flurry
            PlaySweptOpts po1 = {0};
            po1.start_rate = 1.20; po1.end_rate = 0.85;
            po1.max_dur_ms = 280; po1.pan = 0.0;
            po1.wet_send = 0.30; po1.fade = 0.020;
            play_sample_swept(127.20, encWin, encWN, 0.55, po1);
            PlaySweptOpts po2 = {0};
            po2.start_rate = 0.85; po2.end_rate = 1.30;
            po2.max_dur_ms = 240; po2.pan = -0.20;
            po2.wet_send = 0.30; po2.fade = 0.020;
            play_sample_swept(128.80, encWin, encWN, 0.45, po2);
            free(encWin);
            report("→ encore scratch · 2 crowd-win stabs @ 127.20s + 128.80s");
        }
    }
    long gln = 0;
    float *gallop = try_load_sample("gallop.wav", &gln);
    if (gallop) {
        // Gallop MOVED to the very end — starts after the final kick
        // at 158.5s and fades the track out. Bake fade extended below
        // to give it time to ride. (@jeffrey "move horse gallop to
        //  the very end / start after the very last kick to fade
        //  us out")
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0; po.wet_send = 0.45; po.fade = 0.25;
        play_sample(158.65, gallop, gln, 0.80, po);
        free(gallop);
        report("→ gallop · single pass @ 158.65s (post-final-kick outro)");
    }

    // ── accordion @ 137.5s — late-climax folk-pump warmth before coda ──
    long an = 0;
    float *accordion = try_load_sample("accordion.wav", &an);
    if (accordion) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0.10; po.wet_send = 0.45; po.fade = 0.25;
        play_sample(137.50, accordion, an, 0.45, po);
        free(accordion);
        report("→ accordion · 4-bar chord phrase @ 137.5s");
    }

    // ── train pitch-down arpeggio (154-162s, chromatic descent into sub) ──
    long ttn = 0;
    float *tArp = try_load_sample("train.wav", &ttn);
    if (tArp) {
        const struct { double t; double rate; double ms; double g; double pan; } trainArp[6] = {
            { 154.5, 0.94, 1600, 0.45,  0.15 },
            { 156.2, 0.84, 1700, 0.48, -0.25 },
            { 157.8, 0.71, 1800, 0.50,  0.20 },
            { 159.3, 0.59, 1900, 0.50, -0.20 },
            { 160.7, 0.50, 1800, 0.50,  0.10 },
            { 162.0, 0.38, 1600, 0.55,  0.00 },
        };
        for (int i = 0; i < 6; i++) {
            PlaySweptOpts sp = {0};
            sp.start_rate = trainArp[i].rate; sp.end_rate = trainArp[i].rate;
            sp.max_dur_ms = trainArp[i].ms; sp.pan = trainArp[i].pan;
            sp.wet_send = 0.55; sp.fade = 0.20;
            play_sample_swept(trainArp[i].t, tArp, ttn, trainArp[i].g, sp);
        }
        free(tArp);
        report("→ train arpeggio · 6-chop chromatic descent into sub 154-162s");
    }

    // ── PERC ACCENT @ engine 124.80-127.45s (master 2:04) — additive layer
    //    over the climax: halftime kicks, open hats on 2+4, 16th closed-hat
    //    shaker, chord-tone sub on downbeat, 3-against-4 wood-block.
    {
        const double breakStart = 124.80;
        for (int b2 = 0; b2 < 2; b2++) {
            const double tBar = breakStart + b2 * SPBAR_G;
            kick_render(tBar,                      HELL * 1.10, 0.95, 0.30);
            kick_render(tBar + 2 * SPB_G,          HELL * 1.10, 0.95, 0.30);
            tick_render(tBar + 1 * SPB_G,          0.45, 1);
            tick_render(tBar + 3 * SPB_G,          0.45, 1);
            for (int s = 0; s < 16; s++) {
                if (s % 4 != 0) tick_render(tBar + s * (SPB_G / 4.0), 0.16, 0);
            }
            const char *names[8] = {"Dm","Dm","Bb","Bb","F","F","C","C"};
            const ChordH *ch = hchord(names[(10 + b2) % 8]);
            sub_render(tBar, 0.30, ch->root + 14 - 12, 0.55);
            for (int p = 0; p < 3; p++) {
                woodtick_render(tBar + (p * 4.0 / 3.0) * SPB_G, 0.18);
            }
        }
        report("→ PERC ACCENT · 2 bars @ 124.80s (additive over climax)");
    }

    // ── POST-2:00 ADVENTURE — kick burst + sub+tick punch (additive only) ──
    {
        // (A) Kick burst @ engine 120.5 — 4 fast kicks in 0.5s
        for (int s = 0; s < 4; s++) {
            kick_render(120.50 + s * (SPB_G / 4.0), HELL * 0.95, 0.78, 0.28);
        }
        // (B) Sub punch + tick triplet @ engine 131.0
        const double pB = 131.00;
        kick_render(pB,           HELL * 1.10, 0.92, 0.30);
        sub_render(pB,            0.40, 38, 0.55);
        woodtick_render(pB + 0.10, 0.30);
        woodtick_render(pB + 0.22, 0.28);
        woodtick_render(pB + 0.36, 0.26);
        tick_render(pB + 0.48,     0.45, 1);
        report("→ POST-2:00 adventure · kick burst @ 120.5s + sub+tick punch @ 131.0s");
    }

    // ── TTS singing mantra (unified choir + soprano descant) ───────────
    // Matches the DistroKid release master (commit 495903bea, 2026-05-24):
    // 21-combo pool (7 voices × 3 variants), scales 6 → 21 voices over
    // the build, mostly unison with sparse oct-up/oct-down per chordIntervals,
    // plus a guaranteed soprano descant at rate 2.0 every 8 bars.
    const char *ttsVoices[7] = {"Zarvox", "Albert", "Fred", "Alex", "Samantha", "Daniel", "Bells"};
    const char *ttsVariants[3] = {"money", "honey", "bunnies"};
    typedef struct { float *buf; long n; } TTSEntry;
    TTSEntry ttsCombos[32]; int nCombos = 0;
    for (int v = 0; v < 7; v++) {
        for (int vt = 0; vt < 3; vt++) {
            char path[160];
            snprintf(path, sizeof(path), "tts-singing-%s-%s.wav",
                     ttsVoices[v], ttsVariants[vt]);
            long pn = 0;
            float *pb = try_load_sample(path, &pn);
            if (pb && nCombos < 32) ttsCombos[nCombos++] = (TTSEntry){pb, pn};
            else if (pb) free(pb);
        }
    }
    const int ttsTotal = nCombos;
    // TTS choir muted — focusing on the instrumental mix
    if (0 && ttsTotal > 0) {
        double stForTTS = 15.82;
        for (int s = 0; s < n_section_ranges; s++) {
            if (!strcmp(section_ranges[s].name, "statement")) {
                stForTTS = section_ranges[s].startSec; break;
            }
        }
        double acStampSec = AC_STAMP_TIME;
        double cxForTTS = acStampSec - 3 * SPBAR_G;
        const double vocalDelay = 8 * SPBAR_G;      // skip loop 0 — vocals on bar 8 after drop
        const double STEP = 8 * SPBAR_G;            // every brass-theme loop
        // Unison-mostly chord intervals (matches DistroKid hellsine.mjs).
        // 14× unison + 4× oct-up + 2× oct-down across 21 voice slots.
        const int chordIntervals[21] = {
             0,  0,  0,  0,  0,  12,
             0,  0,  0,  0,  0, -12,
             0,  0,  0, 12,  0,  0,
             0,  0,  0,
        };
        int ttsCount = 0; int i = 0;
        for (double t = stForTTS + vocalDelay; t < cxForTTS - 0.5; t += STEP, i++) {
            const double sinceStart = t - stForTTS;
            const double tilEnd = cxForTTS - t;
            double envG = 0.50;     // DistroKid: BIG choir (was 0.32, was inaudible)
            if (tilEnd < 8) {
                const double k = tilEnd / 8.0;
                envG *= (k > 0.20) ? k : 0.20;
            }
            const double wetMax = 0.55;
            double wet = 0.28 + sinceStart * 0.003;
            if (wet > wetMax) wet = wetMax;
            const double buildP = (t - stForTTS) / (cxForTTS - stForTTS);
            const double buildC = buildP > 1.0 ? 1.0 : (buildP < 0 ? 0 : buildP);
            int voiceN = (int)(6 + buildC * 15 + 0.5);
            if (voiceN > ttsTotal) voiceN = ttsTotal;
            const double perVoiceG = envG * pow((double)voiceN, -0.22);

            for (int k = 0; k < voiceN; k++) {
                const int idx = ((i * 5) + k * 7) % ttsTotal;
                const TTSEntry *e = &ttsCombos[idx];
                if (!e->buf) continue;
                const int interval = chordIntervals[k % 21];
                const double rate = pow(2.0, interval / 12.0);
                // Pan: spread voices, higher voices floated toward center,
                // octave-down toward sides
                const double panBase = voiceN > 1 ? ((double)k / (voiceN - 1) - 0.5) * 1.9 : 0;
                const double intervalLean = interval > 6 ? 0.5 : (interval < -6 ? 1.2 : 1.0);
                double pan = panBase * intervalLean + (rng() * 0.08 - 0.04);
                if (pan > 0.95) pan = 0.95; if (pan < -0.95) pan = -0.95;
                PlaySampleOpts po = {0};
                po.rate = rate; po.pan = pan; po.wet_send = wet; po.fade = 0.04;
                play_sample(t, e->buf, e->n, perVoiceG, po);
                ttsCount++;
            }
            // SOPRANO DESCANT — guaranteed oct-up centered voice every pass.
            const TTSEntry *sopE = &ttsCombos[(i * 11) % ttsTotal];
            if (sopE->buf) {
                double sopWet = wet + 0.10; if (sopWet > 0.65) sopWet = 0.65;
                PlaySampleOpts po = {0};
                po.rate = 2.0; po.pan = 0.0; po.wet_send = sopWet; po.fade = 0.04;
                play_sample(t, sopE->buf, sopE->n, perVoiceG * 0.85, po);
                ttsCount++;
            }
        }
        report("→ TTS mantra · %d voices (unified + soprano descant) %.2fs → %.2fs",
               ttsCount, stForTTS + vocalDelay, cxForTTS);
        for (int k = 0; k < nCombos; k++) free(ttsCombos[k].buf);
    }

    // ── helper: load word boundaries from .words.txt sidecar ──────────
    // Format: each line = "word\tfromMs\ttoMs"
    typedef struct { double fromS, toS; } WordRange;

    // ── Jeffrey-PVC lead vocal — DistroKid-version (simple, no teaser) ──
    // Matches commit 495903bea: 1 file per variant, 8-bar delay + 8-bar
    // step, plain playSample at rate 1.0, no DUCK sidechain, no foley.
    float *jvoc[3] = {0,0,0};
    long   jvocN[3] = {0,0,0};
    WordRange jvocWords[3][16] = {{{0}}};
    int       jvocWordCount[3] = {0};
    double    jvocMidi[3][16] = {{0}};        // per-word f0 in MIDI (0=unvoiced)
    // Wizard layer — live jeffrey takes from the 102516 wave-wizard
    // session, used to harmonize with the ElevenLabs vocal.
    float *jwiz[3] = {0,0,0};
    long   jwizN[3] = {0,0,0};
    WordRange jwizWords[3][16] = {{{0}}};
    int       jwizWordCount[3] = {0};
    double    jwizMidi[3][16] = {{0}};        // per-word f0 in MIDI (0=unvoiced)
    // GLITCH CHOIR — 12 Apple `say` novelty voices × 3 variants. Loaded
    // at render time; mixed with stutter + bit-crush + pitch variation.
    static const char *CHOIR_VOICES[12] = {
        "cellos", "bells", "good-news", "bad-news",
        "whisper", "bahh", "trinoids", "zarvox",
        "organ",  "boing", "wobble",    "bubbles"
    };
    // Per-layer base f0 (MIDI) from layer-pitches.txt — every layer gets
    // shifted to TARGET_MIDI so they all harmonize on the same root.
    // 0 = not measured / fall back to no shift.
    double jvocBaseMidi[3] = {0,0,0};
    double jwizBaseMidi[3] = {0,0,0};
    double jchoirBaseMidi[12][3] = {{0}};
    // TARGET = jeffrey's natural ElevenLabs pitch (~D#3). Using the
    // cross-layer median (47.10/B2) pulled high voices down too far →
    // PSOLA mush. D#3 is jeffrey's home key; everyone else harmonizes
    // around it. Shifts clamped to ±7 st (perfect fifth) with octave
    // folding for voices outside that range.
    double TARGET_MIDI = 51.0;
    // Helper inlined below: octave-fold base into [TARGET-6, TARGET+6]
    // then return shift to TARGET (always ≤ 6 st = tritone).
    #define CLAMP_SHIFT(base) ({                                   \
        double b = (base);                                         \
        while (b > TARGET_MIDI + 6) b -= 12.0;                     \
        while (b < TARGET_MIDI - 6) b += 12.0;                     \
        (TARGET_MIDI - b);                                         \
    })
    float *jchoir[12][3] = {{0}};
    long   jchoirN[12][3] = {{0}};
    WordRange jchoirWords[12][3][16] = {{{{0}}}};
    int    jchoirWordCount[12][3] = {{0}};
    int    choirLoadedCount = 0, choirWordedCount = 0;
    for (int cv = 0; cv < 12; cv++) {
        for (int vt = 0; vt < 3; vt++) {
            char cp[256];
            snprintf(cp, sizeof(cp), "say-choir/%s-%s.wav",
                     CHOIR_VOICES[cv], ttsVariants[vt]);
            jchoir[cv][vt] = try_load_sample(cp, &jchoirN[cv][vt]);
            if (jchoir[cv][vt]) {
                choirLoadedCount++;
                char wpath[256];
                snprintf(wpath, sizeof(wpath),
                         HELLSINE_SAMPLES_DIR "/say-choir/%s-%s.words.txt",
                         CHOIR_VOICES[cv], ttsVariants[vt]);
                FILE *wf = fopen(wpath, "r");
                if (wf) {
                    char line[256];
                    while (fgets(line, sizeof(line), wf) && jchoirWordCount[cv][vt] < 16) {
                        char wd[64]; double fMs, tMs;
                        if (sscanf(line, "%63s %lf %lf", wd, &fMs, &tMs) == 3) {
                            const int idx = jchoirWordCount[cv][vt]++;
                            jchoirWords[cv][vt][idx].fromS = fMs / 1000.0;
                            jchoirWords[cv][vt][idx].toS   = tMs / 1000.0;
                        }
                    }
                    fclose(wf);
                    if (jchoirWordCount[cv][vt] > 0) choirWordedCount++;
                }
            }
        }
    }
    if (choirLoadedCount > 0) {
        report("→ glitch choir · %d say-voice samples loaded · %d with word boundaries",
               choirLoadedCount, choirWordedCount);
    }
    // Load per-layer absolute pitch baselines so every layer can be
    // shifted to TARGET_MIDI for cross-layer harmony.
    {
        FILE *lpf = fopen(HELLSINE_SAMPLES_DIR "/layer-pitches.txt", "r");
        if (lpf) {
            char ln[256]; int rows = 0;
            while (fgets(ln, sizeof(ln), lpf)) {
                if (ln[0] == '#' || ln[0] == '\n') continue;
                char layer[128]; double base, hz, shift;
                if (sscanf(ln, "%127s %lf %lf %lf", layer, &base, &hz, &shift) >= 2) {
                    // layer format examples:
                    //   eleven:money / wizard:honey / choir:cellos-bunnies
                    for (int vt = 0; vt < 3; vt++) {
                        char prefix[64];
                        snprintf(prefix, sizeof(prefix), "eleven:%s", ttsVariants[vt]);
                        if (!strcmp(layer, prefix)) { jvocBaseMidi[vt] = base; rows++; goto nextline; }
                        snprintf(prefix, sizeof(prefix), "wizard:%s", ttsVariants[vt]);
                        if (!strcmp(layer, prefix)) { jwizBaseMidi[vt] = base; rows++; goto nextline; }
                    }
                    for (int cv = 0; cv < 12; cv++) for (int vt = 0; vt < 3; vt++) {
                        char prefix[64];
                        snprintf(prefix, sizeof(prefix), "choir:%s-%s",
                                 CHOIR_VOICES[cv], ttsVariants[vt]);
                        if (!strcmp(layer, prefix)) { jchoirBaseMidi[cv][vt] = base; rows++; goto nextline; }
                    }
                    nextline:;
                }
            }
            fclose(lpf);
            report("→ layer-pitches · %d baselines loaded · TARGET = %.2f MIDI",
                   rows, TARGET_MIDI);
        }
    }
    for (int vt = 0; vt < 3; vt++) {
        char path[160];
        snprintf(path, sizeof(path), "jeffrey-vocal-%s.wav", ttsVariants[vt]);
        jvoc[vt] = try_load_sample(path, &jvocN[vt]);
        // Load .words.txt sidecar
        char wpath[256];
        snprintf(wpath, sizeof(wpath), HELLSINE_SAMPLES_DIR "/jeffrey-vocal-%s.words.txt",
                 ttsVariants[vt]);
        FILE *wf = fopen(wpath, "r");
        if (wf) {
            char line[256];
            while (fgets(line, sizeof(line), wf) && jvocWordCount[vt] < 16) {
                char word[64]; double fromMs, toMs;
                if (sscanf(line, "%63s %lf %lf", word, &fromMs, &toMs) == 3) {
                    const int idx = jvocWordCount[vt]++;
                    jvocWords[vt][idx].fromS = fromMs / 1000.0;
                    jvocWords[vt][idx].toS   = toMs / 1000.0;
                }
            }
            fclose(wf);
            report("→ jeffrey words · %s · %d boundaries loaded",
                   ttsVariants[vt], jvocWordCount[vt]);
        }
        // ElevenLabs per-word f0 (.pitches.txt sidecar)
        char ppath[256];
        snprintf(ppath, sizeof(ppath), HELLSINE_SAMPLES_DIR "/jeffrey-vocal-%s.pitches.txt",
                 ttsVariants[vt]);
        FILE *pf = fopen(ppath, "r");
        if (pf) {
            char line[256]; int pi = 0;
            while (fgets(line, sizeof(line), pf) && pi < 16) {
                char word[64]; double fromMs, toMs, midi, hz;
                if (sscanf(line, "%63s %lf %lf %lf %lf", word, &fromMs, &toMs, &midi, &hz) == 5) {
                    jvocMidi[vt][pi++] = midi;
                }
            }
            fclose(pf);
        }
        // Wizard layer: live jeffrey full-mantra recordings.
        char wzpath[200];
        snprintf(wzpath, sizeof(wzpath),
                 "jeffrey-live-archived-102516/jeffrey-live-that-%s.wav",
                 ttsVariants[vt]);
        jwiz[vt] = try_load_sample(wzpath, &jwizN[vt]);
        if (jwiz[vt]) {
            char wzwords[256];
            snprintf(wzwords, sizeof(wzwords),
                     HELLSINE_SAMPLES_DIR "/jeffrey-live-archived-102516/"
                     "jeffrey-live-that-%s.words.txt", ttsVariants[vt]);
            FILE *wzf = fopen(wzwords, "r");
            if (wzf) {
                char ln[256];
                while (fgets(ln, sizeof(ln), wzf) && jwizWordCount[vt] < 16) {
                    char wd[64]; double fMs, tMs;
                    if (sscanf(ln, "%63s %lf %lf", wd, &fMs, &tMs) == 3) {
                        const int idx = jwizWordCount[vt]++;
                        jwizWords[vt][idx].fromS = fMs / 1000.0;
                        jwizWords[vt][idx].toS   = tMs / 1000.0;
                    }
                }
                fclose(wzf);
            }
            // Wizard per-word f0 (.pitches.txt sidecar)
            char wzpitches[256];
            snprintf(wzpitches, sizeof(wzpitches),
                     HELLSINE_SAMPLES_DIR "/jeffrey-live-archived-102516/"
                     "jeffrey-live-that-%s.pitches.txt", ttsVariants[vt]);
            FILE *wzpf = fopen(wzpitches, "r");
            if (wzpf) {
                char ln[256]; int pi = 0;
                while (fgets(ln, sizeof(ln), wzpf) && pi < 16) {
                    char wd[64]; double fMs, tMs, midi, hz;
                    if (sscanf(ln, "%63s %lf %lf %lf %lf", wd, &fMs, &tMs, &midi, &hz) == 5) {
                        jwizMidi[vt][pi++] = midi;
                    }
                }
                fclose(wzpf);
            }
            report("→ jeffrey wizard · %s · %.2fs live take · %d word boundaries",
                   ttsVariants[vt], (double)jwizN[vt] / SR, jwizWordCount[vt]);
        }
    }
    int haveJvoc = jvoc[0] || jvoc[1] || jvoc[2];
    // Jeffrey-PVC re-enabled (TTS choir still muted — jeffrey is the
    // SOLE vocal layer now per @jeffrey 2026-05-25)
    // Jeffrey ElevenLabs vocals back on — wizard + choir layers stay
    // off. The lead jeffrey pass is duplicated with two additional
    // pitched/panned/delayed passes acting as choral backing vocals.
    // (@jeffrey 2026-05-26 "add the vocals back / just the jeffrey
    // eleventy / duplicate em a bit so they can be choral and
    // harmonious / like backing vocals")
    #define JVOX_ENABLED       1
    // Wizard live jeffrey takes back on — layered into the choral
    // arrangement alongside ElevenLabs lead + backings + apple choir.
    // (@jeffrey "add the jeffrey takes from the wizard now / the live
    // vocals / into this choral arrangement if possible")
    #define WIZARD_LAYER_ON    1
    #define CHOIR_LAYER_ON     1
    if (JVOX_ENABLED && haveJvoc) {
        double stForJ = 15.82;
        for (int s = 0; s < n_section_ranges; s++) {
            if (!strcmp(section_ranges[s].name, "statement")) {
                stForJ = section_ranges[s].startSec; break;
            }
        }
        const double acStampJ = AC_STAMP_TIME;
        // Extended cutoff right up to the AC stamp + removed the
        // 0.20 gain clamp below so the last vocal bar CONTINUES and
        // FADES smoothly into the drop instead of cutting at 3 bars
        // before the stamp. (@jeffrey "vocal bar before the drop —
        //  continue and fade them out, not just drop on the bar
        //  before the (aesthetic dot computer) stamp")
        double cxForJ = acStampJ;
        // TIME-WARPED MANTRA: continuous read of the variant audio, but
        // playback rate varies per-word so each word's natural START
        // lands on the matching THEME note's beat. Single global pitch
        // shift via PSOLA-style grain pitch-up on top. No per-word slicing
        // → no chops, no buzz-loops from over-stretching short words.
        // 4-bar step (was 8 bars) → 2 mantras per brass THEME loop, so
        // jeffrey keeps singing with the brass instead of going silent
        // for 7s between utterances (@jeffrey 2026-05-25).
        const double STEPJ = 4 * SPBAR_G;
        // Vocals trimmed another ~25% for balance — the stack reads
        // present at lower individual gain because backings + wizard
        // + natural-driven jeffrey all add up.
        const double variantGain[3] = {2.35, 2.15, 2.00};
        // Per-word pitch match: each ElevenLabs word shifts to the
        // corresponding wizard-take word's f0 (so the synth voice rides
        // jeffrey's actual melodic contour). Computed at render time as
        // pow(2, (wizardMidi[i] - elevenlabsMidi[i]) / 12) per word.
        // Fallback global shift if pitch sidecars missing.
        const double JVOC_PITCH_FALLBACK = pow(2.0, 7.0 / 12.0);
        double themeBeatPos[THEME_N];
        {
            double bp = 0;
            for (int i = 0; i < THEME_N; i++) {
                themeBeatPos[i] = bp;
                bp += THEME[i].beats;
            }
        }
        int jvCount = 0; int j = 0;
        for (double t = stForJ; t < cxForJ - 0.5; t += STEPJ, j++) {
            const int vt = j % 3;
            if (!jvoc[vt] || jvocN[vt] <= 0 || jvocWordCount[vt] <= 0) continue;
            const double tilEnd = cxForJ - t;
            double passG = 0.90 * variantGain[vt];
            // Smooth fade to silence in the last 10 s — no floor clamp
            // so the last vocal bar continues and fades fully into the
            // drop instead of cutting hard.
            if (tilEnd < 10.0) {
                const double kk = tilEnd / 10.0;
                passG *= (kk > 0.0) ? kk : 0.0;
            }
            // Wide pan sweep so the vocal layers fly around the stereo
            // field instead of sitting center. Each pass picks a fresh
            // pan position from a continuous sine + triangle blend.
            // (@jeffrey "pan around / robotic children all around")
            const double pan = sin(j * 1.3) * 0.55 + sin(j * 0.41) * 0.25;
            const double panClamped = (pan > 0.85 ? 0.85 : (pan < -0.85 ? -0.85 : pan));
            const double pL = (panClamped > 0 ? 1.0 - panClamped : 1.0);
            const double pR = (panClamped < 0 ? 1.0 + panClamped : 1.0);
            // Per-pass pitch ROBOT-CHILD variation: each pass picks an
            // additional shift from a discrete set so the voices read as
            // "robotic children" — chipmunked octave + fifth + tritone
            // characters. Some passes drop entirely (silence) so the
            // vocal isn't constantly on. (@jeffrey "voices should change
            // pitch / switch off / sounds like robotic children")
            // No +12 (octave) — was too twangy stacked with the +7/+5
            // backings. Smaller intervals + an occasional drop give
            // variation without going chipmunky on every other pass.
            // (@jeffrey "sounds too twangy now")
            static const double ROBOT_SHIFT_ST[8] = {
                0, +5, +3, -5, 0, +7, -3, +5
            };
            // PASS_ACTIVE removed — replaced by explicit per-j drops
            // below so cycling via j%8 doesn't accidentally drop later
            // passes the user wants kept.
            static const int    PASS_ACTIVE[8] = { 1, 1, 1, 1, 1, 1, 1, 1 };
            // Specific passes to drop entirely:
            //   j=4  (36.92s)  — "drop that bar at 37"
            //   j=13 (84.40s)  — "drop 1:24, keep 1:30 onward"
            {
                static const int dropPasses[] = { 4, 13, -1 };
                int isDropped = 0;
                for (int k = 0; dropPasses[k] >= 0; k++) {
                    if (j == dropPasses[k]) { isDropped = 1; break; }
                }
                if (isDropped) { jvCount++; continue; }
            }
            if (!PASS_ACTIVE[j % 8]) {
                // Skip this pass entirely — leave space in the vocal grid.
                jvCount++;
                continue;
            }
            const double robotShiftSt = ROBOT_SHIFT_ST[j % 8];

            const int nW = jvocWordCount[vt];
            // Map words to consecutive THEME notes (natural speech cadence).
            // Time-warping keeps each word's natural duration intact —
            // wider spacing turns short words into vowel-buzz mush.
            int themeIdxFor[32];
            if (nW > 32) continue;
            for (int wi = 0; wi < nW; wi++) {
                int ti = (wi < THEME_N) ? wi : (THEME_N - 1);
                themeIdxFor[wi] = ti;
            }
            const double tOutStart = t + themeBeatPos[themeIdxFor[0]] * SPB_G;
            const int lastIdx = themeIdxFor[nW - 1];
            const double tOutEnd   = t + (themeBeatPos[lastIdx] + THEME[lastIdx].beats) * SPB_G;

            // Build target_t markers AND corresponding source_t markers
            // (the word START times). Walking output, we linearly warp the
            // SOURCE playback position between adjacent marker pairs.
            const int mN = nW + 1;
            double outM[32];
            double srcM[32];
            if (mN > 32) continue;
            for (int wi = 0; wi < nW; wi++) {
                outM[wi] = t + themeBeatPos[themeIdxFor[wi]] * SPB_G;
                srcM[wi] = jvocWords[vt][wi].fromS;
            }
            outM[nW] = tOutEnd;
            srcM[nW] = jvocWords[vt][nW - 1].toS;

            // LAST PASS BEFORE 2-MIN DROP — drop "that we want" tail,
            // elongate the noun (word 8) into a drone across the
            // remaining themeBeats. (@jeffrey "drop off the last 'that
            //  we want' / elongate the noun / so it drones in")
            if (j == 17 && nW == 12) {
                const double wMS = jvocWords[vt][8].fromS;
                const double wME = jvocWords[vt][8].toS;
                // Linearly stretch the noun audio across word-8..end
                srcM[8]  = wMS;
                srcM[9]  = wMS + (wME - wMS) * 0.30;
                srcM[10] = wMS + (wME - wMS) * 0.60;
                srcM[11] = wMS + (wME - wMS) * 0.85;
                srcM[12] = wME;
            }

            // ── VOCAL FADE-IN ENVELOPE — quieter + sparser at the
            // start, build to full stack by ~pass 5. Lead alone for
            // first 2 passes (no backings, no natural), then backings
            // join, then natural up-front voice last.
            // (@jeffrey 2026-05-26 "voices quieter at first then fade
            // in more and more / only 1 voice at first")
            // Fade-in 0→5, sustain 5-11, gradual fade-down 12+ so the
            // late passes get quieter. Pass j=12 (1:19) gets a SPOTLIGHT
            // boost — the user wanted that particular phrase louder.
            // (@jeffrey "the voice at 1:19 / that verse / that
            //  particular phrase / much louder")
            double vocalRamp;
            if (j < 5)            vocalRamp = 0.35 + 0.65 * (double)j / 5.0;
            else if (j < 12)      vocalRamp = 1.0;
            else if (j == 12)     vocalRamp = 1.65;     // SPOTLIGHT @ 1:19
            else                  vocalRamp = 1.0 - 0.55 * ((double)(j - 12) / 6.0);
            if (vocalRamp < 0.20) vocalRamp = 0.20;
            const int backingsOn = (j >= 2);
            const int naturalOn  = (j >= 3);
            passG *= vocalRamp;

            // ABSOLUTE PITCH MATCH — shift this layer from its measured
            // base (jvocBaseMidi) to the global TARGET_MIDI. Every layer
            // gets shifted to the same root, so jeffrey + wizard + every
            // choir voice harmonize on B2 (or whatever TARGET is) instead
            // of drifting because each has a different intrinsic pitch.
            const double vocBase = (jvocBaseMidi[vt] > 0) ? jvocBaseMidi[vt] : 48.0;
            // Melody-follow bias: +5 st above the absolute pitch target.
            // Plus per-pass ROBOT_SHIFT variation so successive passes
            // hop between octave / fifth / root for the "robotic
            // children" chorus effect.
            const double vocShiftSt = CLAMP_SHIFT(vocBase) + 5.0 + robotShiftSt;
            const double constMatchRate = pow(2.0, vocShiftSt / 12.0);
            (void)JVOC_PITCH_FALLBACK;
            double segRate[32];
            for (int wi = 0; wi < nW; wi++) segRate[wi] = constMatchRate;

            // PSOLA-style pitch grains over the WARPED source position so
            // we get pitch shift independent of the time warp. Bigger
            // grain (70 ms) preserves formants → smoother, more legible
            // vocal (@jeffrey "smoother + more legible").
            // ── WSOLA (Waveform Similarity Overlap-Add) ──
            // Upgrade from plain PSOLA: keep the time-warp-computed
            // idealSrcCenter, but at each grain search ±10 ms for the
            // offset that maximizes normalized cross-correlation with
            // the previous grain's tail. Kills micro-clicks at rate
            // changes + comb-color on sustained vowels. Pure C, no FFT.
            // (@jeffrey 2026-05-26 "much smoother pitch shifting / WSOLA")
            const long GRAIN_LEN = (long)(0.070 * SR);
            const long HOP       = GRAIN_LEN / 2;
            const long outStartI = (long)(tOutStart * SR);
            const long outEndI   = (long)(tOutEnd   * SR);
            const long WSOLA_RADIUS = (long)(0.010 * SR);   // ±10 ms search
            const long WSOLA_STEP   = 4;                     // sample-stride
            double prevTail[2048];
            int havePrevTail = 0;
            for (long g = outStartI; g < outEndI; g += HOP) {
                const double outT = (double)g / SR;
                int seg = 0;
                while (seg < mN - 2 && outM[seg + 1] <= outT) seg++;
                const double f = (outT - outM[seg]) / (outM[seg + 1] - outM[seg]);
                const double idealSrcCenter = (srcM[seg] + f * (srcM[seg + 1] - srcM[seg])) * SR;
                const double pitchRate = segRate[seg];

                // WSOLA search: find offset minimizing phase discontinuity
                // with prevTail (previous grain's last HOP source samples,
                // pre-window). Skip search for the first grain (no tail).
                long bestOffset = 0;
                if (havePrevTail && HOP <= 2048) {
                    double bestScore = -1e18;
                    for (long off = -WSOLA_RADIUS; off <= WSOLA_RADIUS; off += WSOLA_STEP) {
                        const double queryStart = idealSrcCenter + (double)off
                                                  - (double)HOP * pitchRate;
                        double sumXY = 0, sumXX = 0, sumYY = 0;
                        for (long k = 0; k < HOP; k++) {
                            const double srcPos = queryStart + (double)k * pitchRate;
                            if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) continue;
                            const long ri = (long)srcPos;
                            const double frac = srcPos - ri;
                            const double s = jvoc[vt][ri] * (1.0 - frac)
                                           + jvoc[vt][ri + 1] * frac;
                            sumXY += s * prevTail[k];
                            sumXX += s * s;
                            sumYY += prevTail[k] * prevTail[k];
                        }
                        const double denom = sqrt(sumXX * sumYY);
                        const double score = (denom > 1e-12) ? (sumXY / denom) : 0.0;
                        if (score > bestScore) {
                            bestScore = score;
                            bestOffset = off;
                        }
                    }
                }
                const double srcCenter = idealSrcCenter + (double)bestOffset;

                // Render grain at the WSOLA-aligned srcCenter
                for (long s = 0; s < GRAIN_LEN; s++) {
                    const long outIdx = g + s - GRAIN_LEN / 2;
                    if (outIdx < outStartI || outIdx >= outEndI) continue;
                    if (outIdx < 0 || outIdx >= N) continue;
                    const double srcPos = srcCenter + (double)(s - GRAIN_LEN / 2) * pitchRate;
                    if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) continue;
                    const long ri = (long)srcPos;
                    const double frac = srcPos - ri;
                    const double sm = jvoc[vt][ri] * (1.0 - frac) + jvoc[vt][ri + 1] * frac;
                    const double win = 0.5 - 0.5 * cos(2.0 * M_PI * s / (GRAIN_LEN - 1));
                    const double v = sm * win * passG;
                    L[outIdx]  += (float)(v * pL);
                    R[outIdx]  += (float)(v * pR);
                    SL[outIdx] += (float)(v * 0.05);
                    SR_[outIdx]+= (float)(v * 0.05);
                }

                // Save this grain's tail (HOP source samples post-center)
                // for next iteration's WSOLA correlation reference.
                if (HOP <= 2048) {
                    for (long k = 0; k < HOP; k++) {
                        const double srcPos = srcCenter + (double)k * pitchRate;
                        if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) {
                            prevTail[k] = 0.0;
                            continue;
                        }
                        const long ri = (long)srcPos;
                        const double frac = srcPos - ri;
                        prevTail[k] = jvoc[vt][ri] * (1.0 - frac)
                                    + jvoc[vt][ri + 1] * frac;
                    }
                    havePrevTail = 1;
                }
            }
            // ── DOUBLED VARIANT WORD @ 1:19 (j=12) — render word 8
            // (the variant: money/honey/bunnies) AGAIN at +12 st on
            // top of the existing lead. Octave-up "money" doubler.
            // (@jeffrey "at 1:19 can the word 'moneys' be doubled and
            //  pitched up a bit")
            if (j == 12 && nW >= 12) {
                const int wi = 8;       // variant word index
                const double doubleShiftSt = vocShiftSt + 12.0;
                const double dRate = pow(2.0, doubleShiftSt / 12.0);
                const long inStart = (long)(jvocWords[vt][wi].fromS * SR);
                const long inEnd   = (long)(jvocWords[vt][wi].toS   * SR);
                if (inEnd > inStart && inEnd <= jvocN[vt]) {
                    const double wordOutT = t + themeBeatPos[themeIdxFor[wi]] * SPB_G;
                    const long oStart = (long)(wordOutT * SR);
                    const long oLen   = (long)((double)(inEnd - inStart) / dRate);
                    const long fadeN  = (long)(0.015 * SR);
                    const double dGain = passG * 0.55;   // moderate doubler level
                    for (long w = 0; w < oLen; w++) {
                        const long oi = oStart + w;
                        if (oi < 0 || oi >= N) continue;
                        const double readPos = (double)inStart + (double)w * dRate;
                        if (readPos + 1 >= (double)jvocN[vt]) break;
                        const long ri = (long)readPos;
                        const double frac = readPos - ri;
                        double s = jvoc[vt][ri] * (1.0 - frac) + jvoc[vt][ri + 1] * frac;
                        if (w < fadeN)             s *= (double)w / fadeN;
                        if (oLen - w < fadeN)      s *= (double)(oLen - w) / fadeN;
                        const double v = s * dGain;
                        L[oi]  += (float)(v * 0.90);
                        R[oi]  += (float)(v * 1.10);    // brighten right side
                        SL[oi] += (float)(v * 0.18);
                        SR_[oi]+= (float)(v * 0.18);
                    }
                }
            }

            // ── CHOP-AND-SCREW OVERLAY — on selected passes, layer a
            // pitched-down (rate 0.60 = ~−9 st + 1.67× slower) full
            // mantra playback with stuttered "we" → "we-we-we-want"
            // tail chops. Two of every 8 passes get the treatment.
            // (@jeffrey 2026-05-26 "every other utterance or like on a
            // handful of them chop and screw them")
            static const int CHOP_SCREW[8] = { 0, 0, 1, 0, 0, 1, 0, 0 };
            if (CHOP_SCREW[j % 8] && nW >= 12) {
                const double csRate = 0.60;                    // pitch + tempo down
                const double csGain = passG * 0.55;
                const long csStart  = (long)(t * SR);
                const long csOutLen = (long)((double)jvocN[vt] / csRate);
                const long csFade   = (long)(0.040 * SR);
                // Chop window — stutter "we" (word 10) into 60 ms slices
                // repeated 4× before continuing into "want" (word 11).
                const long weStartSrc  = (long)(jvocWords[vt][10].fromS * SR);
                const long weOutStart  = (long)((double)weStartSrc / csRate);
                const long stutterLen  = (long)(0.060 * SR);
                const int  stutterReps = 4;
                const long stutterEnd  = weOutStart + stutterLen * stutterReps;
                for (long w = 0; w < csOutLen; w++) {
                    const long oi = csStart + w;
                    if (oi < 0 || oi >= N) continue;
                    long readOut = w;
                    if (w >= weOutStart && w < stutterEnd) {
                        const long off = (w - weOutStart) % stutterLen;
                        readOut = weOutStart + off;
                    } else if (w >= stutterEnd) {
                        readOut = w - (stutterLen * (stutterReps - 1));
                    }
                    const double readPos = (double)readOut * csRate;
                    if (readPos < 0 || readPos + 1 >= (double)jvocN[vt]) break;
                    const long ri = (long)readPos;
                    const double frac = readPos - ri;
                    double s = jvoc[vt][ri] * (1.0 - frac)
                             + jvoc[vt][ri + 1] * frac;
                    double env = 1.0;
                    if (w < csFade)                env = (double)w / csFade;
                    if (csOutLen - w < csFade)     env *= (double)(csOutLen - w) / csFade;
                    // Soft fade on each stutter slice boundary to kill clicks
                    if (w >= weOutStart && w < stutterEnd) {
                        const long off = (w - weOutStart) % stutterLen;
                        const long edge = stutterLen / 8;
                        if (off < edge)              env *= (double)off / edge;
                        if (stutterLen - off < edge) env *= (double)(stutterLen - off) / edge;
                    }
                    const double v = s * env * csGain;
                    L[oi]  += (float)v;
                    R[oi]  += (float)v;
                    SL[oi] += (float)(v * 0.32);    // wetter — sits behind
                    SR_[oi]+= (float)(v * 0.32);
                }
            }

            // ── "WANT" JPEG-DETERIORATION — runs across the WHOLE
            // "want" word as a continuous read with progressive bit-
            // decay (16-bit → 3-bit) + amplitude fade-out. Like a 56k
            // modem connection degrading mid-call. No XOR (that was
            // the "fart"). (@jeffrey "less of a fart / more 56k modem
            //  like JPEG deterioration / over the whole 'want' word /
            //  feel them die off")
            if (nW >= 12) {
                const double wantOutStart = outM[11];
                const double wantOutEnd   = outM[12];
                const long wsI = (long)(wantOutStart * SR);
                const long weI = (long)(wantOutEnd   * SR);
                const double wantSrcStart = jvocWords[vt][11].fromS;
                const double wantSrcEnd   = jvocWords[vt][11].toS;
                const long wsrcI = (long)(wantSrcStart * SR);
                const long wsrcLen = (long)((wantSrcEnd - wantSrcStart) * SR);
                if (weI > wsI && wsrcLen > 0) {
                    const double byteGain = passG * 0.32;
                    const long totalLen = weI - wsI;
                    for (long k = 0; k < totalLen; k++) {
                        const long readIdx = wsrcI + (k % wsrcLen);
                        if (readIdx < 0 || readIdx >= jvocN[vt]) continue;
                        double s = jvoc[vt][readIdx];
                        // Progressive bit-decay: 16-bit (fr=0) → 3-bit (fr=1)
                        const double fr = (double)k / (double)totalLen;
                        const double bitsNow = 16.0 - 13.0 * fr;     // 16 → 3 bits
                        const double Q = pow(2.0, bitsNow);
                        s = floor(s * Q + 0.5) / Q;
                        // Amplitude FADE OUT: linear 1 → 0 (dies off)
                        const double envFade = 1.0 - fr;
                        // Slow stereo wobble — modem-like phasing
                        const double bvPan = sin(fr * M_PI * 1.7) * 0.55;
                        const double bvPL = (bvPan > 0) ? (1.0 - bvPan) : 1.0;
                        const double bvPR = (bvPan < 0) ? (1.0 + bvPan) : 1.0;
                        const long o = wsI + k;
                        if (o < 0 || o >= N) continue;
                        const double v = s * envFade * byteGain;
                        L[o]  += (float)(v * bvPL);
                        R[o]  += (float)(v * bvPR);
                        SL[o] += (float)(v * 0.32);
                        SR_[o]+= (float)(v * 0.32);
                    }
                }
            }

            // ── UNPITCHED JEFFREY — natural rate=1.0, dry, RIGHT UP
            // FRONT of the mix. Compressed + driven via tanh so the
            // raw voice has presence, not just amplitude. The sat curve
            // brings up quiet tails (compression effect) AND adds
            // harmonic edge (drive). (@jeffrey 2026-05-26 "compressed
            // / intensified / still needs some drive")
            if (naturalOn) {
                const double natGain = passG * 1.10;
                const double natDrive = 2.20;            // input gain into tanh
                const double natMakeup = 0.55;           // output trim after tanh
                const long natFade = (long)(0.020 * SR);
                for (int wi = 0; wi < nW; wi++) {
                    const long inStart = (long)(jvocWords[vt][wi].fromS * SR);
                    const long inEnd   = (long)(jvocWords[vt][wi].toS   * SR);
                    if (inEnd <= inStart || inEnd > jvocN[vt]) continue;
                    const long inLen = inEnd - inStart;
                    const double wordOutT = t + themeBeatPos[themeIdxFor[wi]] * SPB_G;
                    const long natStart = (long)(wordOutT * SR);
                    for (long w = 0; w < inLen; w++) {
                        const long oi = natStart + w;
                        if (oi < 0 || oi >= N) continue;
                        double s = jvoc[vt][inStart + w];
                        // Drive + soft compression: tanh saturates peaks,
                        // brings up quiet content, adds harmonic bite.
                        s = tanh(s * natDrive) * natMakeup;
                        if (w < natFade)         s *= (double)w / natFade;
                        if (inLen - w < natFade) s *= (double)(inLen - w) / natFade;
                        const double v = s * natGain;
                        L[oi]  += (float)v;
                        R[oi]  += (float)v;
                        SL[oi] += (float)(v * 0.03);
                        SR_[oi]+= (float)(v * 0.03);
                    }
                }
            }

            // ── BACKING VOCALS — bumped to +7 / +12 st (brighter
            // shimmer above the lead) for the requested brightness.
            // Lead pitched +5 above target by vocShiftSt, so backings
            // land on the 5th and octave above the lead's new register.
            // (@jeffrey "and have a little more brightness")
            if (backingsOn) {
                // High backing dropped from +12 → +5 — octave-up was way
                // too high in the stack (especially when the per-pass
                // robot-shift already added another octave). Now a 5th +
                // 4th harmony pair sitting just above the lead.
                // (@jeffrey 2026-05-26 "the high one is way too high")
                static const double BACKING_ST[2]   = { +7.0, +5.0 };
                static const double BACKING_GAIN[2] = { 0.45, 0.38 };
                static const double BACKING_PAN[2]  = { -0.55, +0.55 };
                static const double BACKING_DLY[2]  = {  0.018, 0.032 };  // ms time-offset
                for (int bv = 0; bv < 2; bv++) {
                    const double bvRate = pow(2.0, (vocShiftSt + BACKING_ST[bv]) / 12.0);
                    const double bvGain = passG * BACKING_GAIN[bv];
                    const double bvPan  = BACKING_PAN[bv];
                    const double bvPL = (bvPan > 0) ? (1.0 - bvPan) : 1.0;
                    const double bvPR = (bvPan < 0) ? (1.0 + bvPan) : 1.0;
                    const long bvDly = (long)(BACKING_DLY[bv] * SR);
                    // WSOLA on each backing too — same algorithm as lead.
                    double bvPrevTail[2048];
                    int bvHavePrev = 0;
                    for (long g = outStartI; g < outEndI; g += HOP) {
                        const double outT = (double)g / SR;
                        int seg = 0;
                        while (seg < mN - 2 && outM[seg + 1] <= outT) seg++;
                        const double f = (outT - outM[seg]) / (outM[seg + 1] - outM[seg]);
                        const double idealSrcCenter = (srcM[seg] + f * (srcM[seg + 1] - srcM[seg])) * SR;

                        long bvBestOff = 0;
                        if (bvHavePrev && HOP <= 2048) {
                            double bestScore = -1e18;
                            for (long off = -WSOLA_RADIUS; off <= WSOLA_RADIUS; off += WSOLA_STEP) {
                                const double queryStart = idealSrcCenter + (double)off
                                                          - (double)HOP * bvRate;
                                double sumXY = 0, sumXX = 0, sumYY = 0;
                                for (long k = 0; k < HOP; k++) {
                                    const double srcPos = queryStart + (double)k * bvRate;
                                    if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) continue;
                                    const long ri = (long)srcPos;
                                    const double frac = srcPos - ri;
                                    const double sx = jvoc[vt][ri] * (1.0 - frac)
                                                    + jvoc[vt][ri + 1] * frac;
                                    sumXY += sx * bvPrevTail[k];
                                    sumXX += sx * sx;
                                    sumYY += bvPrevTail[k] * bvPrevTail[k];
                                }
                                const double denom = sqrt(sumXX * sumYY);
                                const double score = (denom > 1e-12) ? (sumXY / denom) : 0.0;
                                if (score > bestScore) { bestScore = score; bvBestOff = off; }
                            }
                        }
                        const double srcCenter = idealSrcCenter + (double)bvBestOff;

                        for (long s = 0; s < GRAIN_LEN; s++) {
                            const long outIdx = g + s - GRAIN_LEN / 2 + bvDly;
                            if (outIdx < outStartI || outIdx >= outEndI + bvDly) continue;
                            if (outIdx < 0 || outIdx >= N) continue;
                            const double srcPos = srcCenter + (double)(s - GRAIN_LEN / 2) * bvRate;
                            if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) continue;
                            const long ri = (long)srcPos;
                            const double frac = srcPos - ri;
                            const double sm = jvoc[vt][ri] * (1.0 - frac) + jvoc[vt][ri + 1] * frac;
                            const double win = 0.5 - 0.5 * cos(2.0 * M_PI * s / (GRAIN_LEN - 1));
                            const double v = sm * win * bvGain;
                            L[outIdx]  += (float)(v * bvPL);
                            R[outIdx]  += (float)(v * bvPR);
                            SL[outIdx] += (float)(v * 0.22);
                            SR_[outIdx]+= (float)(v * 0.22);
                        }
                        if (HOP <= 2048) {
                            for (long k = 0; k < HOP; k++) {
                                const double srcPos = srcCenter + (double)k * bvRate;
                                if (srcPos < 0 || srcPos + 1 >= (double)jvocN[vt]) {
                                    bvPrevTail[k] = 0.0;
                                    continue;
                                }
                                const long ri = (long)srcPos;
                                const double frac = srcPos - ri;
                                bvPrevTail[k] = jvoc[vt][ri] * (1.0 - frac)
                                              + jvoc[vt][ri + 1] * frac;
                            }
                            bvHavePrev = 1;
                        }
                    }
                }
            }
            // WIZARD HARMONY LAYER — live jeffrey take with the SAME
            // word-boundary time-warp as the ElevenLabs vocal: each word
            // in the live take is snapped to its THEME beat. Pitch is
            // rotated per-pass through D-minor scale degrees so the
            // wizard line dances around the ElevenLabs +7st.
            // Wizard layer dropped after 0:37 — per-word autotune wasn't
            // landing reliably (live takes have natural pitch sweep
            // within each word that resists clean shift-to-target).
            // First few passes (15-37s) keep wizard for the opening
            // call-and-response feel; after that, drop entirely.
            // (@jeffrey "its not well autotuned / drop the wizard takes
            //  after :37")
            if (WIZARD_LAYER_ON && t < 37.0 &&
                jwiz[vt] && jwizN[vt] > 0 && jwizWordCount[vt] > 0) {
                // HARMONIZE WIZARD TO LEAD — wizard tracks the lead's
                // exact per-pass pitch (TARGET + 5 + robotShift) and
                // adds a small harmony interval (3rd / 5th below or
                // unison) so the live takes sing IN CHORD with the
                // ElevenLabs lead instead of drifting independently.
                // (@jeffrey "wizard jeffrey samples to be well
                //  autotuned / more sung harmonic vibe among all voice
                //  / analyze output freq and make sure they harmonize")
                static const double wizHarmonyBelow[8] = {
                    -3, -7, 0, -5, -3, 0, -7, -5
                };
                const double wizBase = (jwizBaseMidi[vt] > 0) ? jwizBaseMidi[vt] : 52.0;
                // Mirror the lead's shift math: CLAMP to TARGET, add
                // the same melody-follow bias (+5) and per-pass
                // robotShiftSt, then a harmony interval BELOW.
                const double wizShift = CLAMP_SHIFT(wizBase) + 5.0
                                      + robotShiftSt + wizHarmonyBelow[j % 8];
                const double wpr  = pow(2.0, wizShift / 12.0);
                const double wizGain = 1.45 * passG;     // dialed back with rest of stack
                const double wizPan  = -pan * 1.5;
                const double wzL = (wizPan > 0 ? 1.0 - wizPan : 1.0);
                const double wzR = (wizPan < 0 ? 1.0 + wizPan : 1.0);

                const int nWz = jwizWordCount[vt];
                if (nWz <= 32) {
                    // Wizard says the OLD 10-word mantra ("i hope that we
                    // get all the X we want"). The new 12-word mantra
                    // inserts "of" at position 6 and "that" at position 9.
                    // Map wizard's 10 words to ElevenLabs's positions
                    // [0,1,2,3,4,5,7,8,10,11], skipping the new words.
                    static const int WIZ_TO_GRID_12[10] = {0,1,2,3,4,5,7,8,10,11};
                    int wzThemeFor[32];
                    for (int wi = 0; wi < nWz; wi++) {
                        int gridPos = (nWz == 10 && nW == 12)
                            ? WIZ_TO_GRID_12[wi]
                            : ((wi < nW) ? wi : (nW - 1));
                        int ti = (gridPos < THEME_N) ? gridPos : (THEME_N - 1);
                        wzThemeFor[wi] = ti;
                    }
                    const double wzOutStart = t + themeBeatPos[wzThemeFor[0]] * SPB_G;
                    const int wzLastIdx = wzThemeFor[nWz - 1];
                    const double wzOutEnd = t + (themeBeatPos[wzLastIdx] + THEME[wzLastIdx].beats) * SPB_G;
                    double wzOutM[32], wzSrcM[32];
                    for (int wi = 0; wi < nWz; wi++) {
                        wzOutM[wi] = t + themeBeatPos[wzThemeFor[wi]] * SPB_G;
                        wzSrcM[wi] = jwizWords[vt][wi].fromS;
                    }
                    wzOutM[nWz] = wzOutEnd;
                    wzSrcM[nWz] = jwizWords[vt][nWz - 1].toS;

                    // PER-WORD AUTOTUNE — each wizard word gets its OWN
                    // shift so its OUTPUT FREQUENCY lands at (lead's
                    // output for the same word) + harmony interval.
                    // Wizard pitches vary 45-65 MIDI across words, so a
                    // single pass-constant shift can't keep them locked
                    // to the lead. (@jeffrey "wizard isnt harmonized to
                    //  the lead / output freq needs to harmonize")
                    double wzSegRate[32];
                    const double harmBelow = wizHarmonyBelow[j % 8];
                    for (int wi = 0; wi < nWz; wi++) {
                        const int leadWordIdx = (nWz == 10 && nW == 12)
                                                ? WIZ_TO_GRID_12[wi]
                                                : ((wi < nW) ? wi : (nW - 1));
                        const double wzM = jwizMidi[vt][wi];
                        const double ldM = jvocMidi[vt][leadWordIdx];
                        if (wzM > 40 && wzM < 70 && ldM > 40 && ldM < 70) {
                            // Lead's actual output pitch for this word
                            const double leadOutMidi = ldM + vocShiftSt;
                            const double tgtMidi     = leadOutMidi + harmBelow;
                            const double semis       = tgtMidi - wzM;
                            wzSegRate[wi] = pow(2.0, semis / 12.0);
                        } else {
                            wzSegRate[wi] = wpr;     // fallback
                        }
                    }

                    const long wzGrain = (long)(0.040 * SR);
                    const long wzHop   = wzGrain / 2;
                    const long wzStartI = (long)(wzOutStart * SR);
                    const long wzEndI   = (long)(wzOutEnd   * SR);
                    for (long g = wzStartI; g < wzEndI; g += wzHop) {
                        const double outT = (double)g / SR;
                        int seg = 0;
                        while (seg < (nWz + 1) - 2 && wzOutM[seg + 1] <= outT) seg++;
                        const double f = (outT - wzOutM[seg]) / (wzOutM[seg + 1] - wzOutM[seg]);
                        const double srcCenter = (wzSrcM[seg] + f * (wzSrcM[seg + 1] - wzSrcM[seg])) * SR;
                        // CLAMP per-word rate to ±octave so AMDF pitch
                        // detection errors don't produce chipmunk shifts.
                        double wpr_seg = wzSegRate[seg];
                        if (wpr_seg > 2.0) wpr_seg = 2.0;
                        if (wpr_seg < 0.5) wpr_seg = 0.5;
                        for (long s = 0; s < wzGrain; s++) {
                            const long outIdx = g + s - wzGrain / 2;
                            if (outIdx < wzStartI || outIdx >= wzEndI) continue;
                            if (outIdx < 0 || outIdx >= N) continue;
                            const double srcPos = srcCenter + (double)(s - wzGrain / 2) * wpr_seg;
                            if (srcPos < 0 || srcPos + 1 >= (double)jwizN[vt]) continue;
                            const long ri = (long)srcPos;
                            const double frac = srcPos - ri;
                            const double sm = jwiz[vt][ri] * (1.0 - frac) + jwiz[vt][ri + 1] * frac;
                            const double win = 0.5 - 0.5 * cos(2.0 * M_PI * s / (wzGrain - 1));
                            const double v = sm * win * wizGain;
                            L[outIdx]  += (float)(v * wzL);
                            R[outIdx]  += (float)(v * wzR);
                            SL[outIdx] += (float)(v * 0.10);
                            SR_[outIdx]+= (float)(v * 0.10);
                        }
                    }
                }
            }
            // UNISON CHOIR — for voices with word sidecars, time-warp
            // each choir word to land ON THE SAME THEME BEAT as the
            // jeffrey vocals. Bit-crushed + heavily spatialized so the
            // unison sits as a thick chorus around the lead voice.
            // 4 voices per pass for thickness; choir spans the full
            // mantra duration (not blips — actual harmony singing).
            if (CHOIR_LAYER_ON && choirLoadedCount > 0) {
                static const int unisonVoiceIdx[8] = {
                    0, 2, 3, 8, 6, 7, 4, 5    // cellos, good/bad-news, organ, trinoids, zarvox, whisper, bahh
                };
                static const double choirIntervalSt[8] = {
                    0, +12, -5, +7, +5, -7, +3, -12
                };
                for (int ck = 0; ck < 2; ck++) {     // 2 voices, not 4
                    const int cv  = unisonVoiceIdx[(j * 3 + ck * 2) % 8];
                    const int cvt = (j + ck) % 3;
                    if (!jchoir[cv][cvt] || jchoirWordCount[cv][cvt] != nW) continue;
                    const long sN = jchoirN[cv][cvt];
                    // NO bit-crush — clean smooth chorus, blends into the
                    // jeffrey leads instead of cutting through them.
                    const double crushQ  = 65536.0;
                    const double uGain   = 0.32 * passG;     // sit back
                    // ABSOLUTE pitch shift: this voice's base → TARGET,
                    // then add a harmony interval per pass.
                    const double cBase = (jchoirBaseMidi[cv][cvt] > 0)
                                       ? jchoirBaseMidi[cv][cvt] : 50.0;
                    const double cShiftSt = CLAMP_SHIFT(cBase) + choirIntervalSt[(j + ck) % 8];
                    const double choirRate = pow(2.0, cShiftSt / 12.0);
                    const double upan    = sin((j * 1.3 + ck * 1.9) * 0.7) * 0.90;
                    const double upL = (upan > 0) ? (1.0 - upan) : 1.0;
                    const double upR = (upan < 0) ? (1.0 + upan) : 1.0;
                    // Smaller Haas (4-10 ms) keeps stereo width without
                    // creating phasey flam.
                    const long haasMs = 4 + ((j * 3 + ck * 5) % 6);
                    const long haasD  = (long)((haasMs / 1000.0) * SR);
                    // Build choir's own warp markers using same THEME beats
                    double cOutM[32], cSrcM[32];
                    for (int wi = 0; wi < nW; wi++) {
                        cOutM[wi] = outM[wi];     // same target as jeffrey → UNISON
                        cSrcM[wi] = jchoirWords[cv][cvt][wi].fromS;
                    }
                    cOutM[nW] = outM[nW];
                    cSrcM[nW] = jchoirWords[cv][cvt][nW - 1].toS;
                    const long uOutStartI = (long)(outM[0] * SR);
                    const long uOutEndI   = (long)(outM[nW] * SR);
                    // Bigger grain = smoother PSOLA, less granular artifact
                    const long uGrain = (long)(0.070 * SR);    // 70 ms (was 40)
                    const long uHop   = uGrain / 2;
                    for (long g = uOutStartI; g < uOutEndI; g += uHop) {
                        const double outT = (double)g / SR;
                        int seg = 0;
                        while (seg < mN - 2 && cOutM[seg + 1] <= outT) seg++;
                        const double f = (outT - cOutM[seg]) / (cOutM[seg + 1] - cOutM[seg]);
                        const double srcCenter = (cSrcM[seg] + f * (cSrcM[seg + 1] - cSrcM[seg])) * SR;
                        for (long s = 0; s < uGrain; s++) {
                            const long outIdx = g + s - uGrain / 2;
                            if (outIdx < uOutStartI || outIdx >= uOutEndI) continue;
                            if (outIdx < 0 || outIdx >= N) continue;
                            const double srcPos = srcCenter + (double)(s - uGrain / 2) * choirRate;
                            if (srcPos < 0 || srcPos + 1 >= (double)sN) continue;
                            const long ri = (long)srcPos;
                            const double frac = srcPos - ri;
                            double sm = jchoir[cv][cvt][ri] * (1.0 - frac)
                                      + jchoir[cv][cvt][ri + 1] * frac;
                            sm = floor(sm * crushQ + 0.5) / crushQ;
                            const double win = 0.5 - 0.5 * cos(2.0 * M_PI * s / (uGrain - 1));
                            const double v = sm * win * uGain;
                            L[outIdx]  += (float)(v * upL);
                            R[outIdx]  += (float)(v * upR);
                            const long oD = outIdx + haasD;
                            if (oD < N) {
                                L[oD] += (float)(v * 0.55 * upR);
                                R[oD] += (float)(v * 0.55 * upL);
                            }
                            SL[outIdx] += (float)(v * 0.20);
                            SR_[outIdx]+= (float)(v * 0.20);
                        }
                    }
                }
                // GLITCH BLIPS DISABLED — was bahh/bubbles stuttered FX
                // but pushed the mix into glitchy/noisy territory. The
                // word-aligned unison choir gives all the texture we need.
                static const int blipVoiceIdx[2] = { 5, 11 };
                (void)blipVoiceIdx;
                for (int ck = 0; ck < 0; ck++) {
                    const int cv  = blipVoiceIdx[ck % 2];
                    const int cvt = (j + ck) % 3;
                    if (!jchoir[cv][cvt] || jchoirN[cv][cvt] <= 0) continue;
                    const long sN = jchoirN[cv][cvt];
                    static const double cpitch[8] = {
                        0.6, 0.75, 0.85, 1.0, 1.18, 1.41, 1.68, 2.0
                    };
                    const double rate    = cpitch[(j * 11 + ck * 3) % 8];
                    const int    reverse = ((j + ck * 2) % 4 == 0);
                    const double crushQ  = (j % 2 == 0) ? 8.0 : 16.0;
                    const double cGain   = 1.80 * passG;     // FRONT of mix
                    // Pan swept across blips via continuous sine, not fixed
                    const double cpan    = sin((j * 1.3 + ck * 1.7) * 0.7) * 0.85;
                    const double pLc = (cpan > 0) ? (1.0 - cpan) : 1.0;
                    const double pRc = (cpan < 0) ? (1.0 + cpan) : 1.0;
                    // Haas L/R delay (4-18 ms) - opposite ear gets a delay
                    const long haasMs = 4 + ((j * 5 + ck * 7) % 14);
                    const long haasDelay = (long)((haasMs / 1000.0) * SR);
                    static const double blipOffset[5] = {
                        0.15, 1.10, 2.10, 3.10, 4.05
                    };
                    const double tChoir = t + blipOffset[ck];
                    const long   oStart = (long)(tChoir * SR);
                    const long sliceStartSrc =
                        (long)((0.20 + ((j + ck) % 5) * 0.12) * sN);
                    const long stutterLen   = (long)(0.045 * SR);
                    const int  stutterReps  = 4 + ((j + ck) % 4);
                    const long fadeLen      = (long)(0.004 * SR);
                    long writeOff = 0;
                    for (int r = 0; r < stutterReps; r++) {
                        for (long w = 0; w < stutterLen; w++) {
                            const long o = oStart + writeOff + w;
                            if (o < 0 || o >= N) continue;
                            const long srcOff = (long)(w * rate);
                            const long srcRaw = sliceStartSrc + srcOff;
                            const long src    = reverse ? (sN - 1 - srcRaw) : srcRaw;
                            if (src < 0 || src >= sN) continue;
                            double s = jchoir[cv][cvt][src];
                            s = floor(s * crushQ + 0.5) / crushQ;
                            double env = 1.0;
                            if (w < fadeLen) env = (double)w / fadeLen;
                            if (stutterLen - w < fadeLen) env = (double)(stutterLen - w) / fadeLen;
                            const double v = s * env * cGain;
                            // Primary ear (no delay) full pan-weighted gain
                            L[o]  += (float)(v * pLc);
                            R[o]  += (float)(v * pRc);
                            // Haas delay tap on the opposite ear (60% of dry)
                            const long oD = o + haasDelay;
                            if (oD < N) {
                                L[oD] += (float)(v * 0.60 * pRc);     // swap pan
                                R[oD] += (float)(v * 0.60 * pLc);
                            }
                            // Big spatial bus send for wide reverb wash
                            SL[o] += (float)(v * 0.85);
                            SR_[o]+= (float)(v * 0.85);
                        }
                        writeOff += stutterLen;
                    }
                }
            }
            jvCount++;
        }
        report("→ jeffrey-pvc · %d mantras · time-warped + per-word pitch-matched to wizard f0", jvCount);
    }
    for (int vt = 0; vt < 3; vt++) if (jvoc[vt]) free(jvoc[vt]);
    for (int vt = 0; vt < 3; vt++) if (jwiz[vt]) free(jwiz[vt]);
    for (int cv = 0; cv < 12; cv++) for (int vt = 0; vt < 3; vt++)
        if (jchoir[cv][vt]) free(jchoir[cv][vt]);

    // ── three kitten meows clustered 7-13s ─────────────────────────────
    const struct { const char *path; double t; double gain; double pan; double wet; } meows[3] = {
        { "meow.wav",   7.00,  0.10, -0.55, 0.40 },
        { "meow-2.wav", 10.00, 0.09,  0.45, 0.55 },
        { "meow-3.wav", 12.40, 0.10, -0.25, 0.50 },
    };

    // ── UT2004 SHOCK-RIFLE — CHORDED TOOT @ the 2nd drop ───────────
    // Three pitched copies of the same sample stacked as a power-fifth
    // chord (root + 5th + octave), each panned + with slightly
    // different start times for chorus thickness. Lower rates +
    // longer fades make it read more "whistling toot" than dry zap.
    // (@jeffrey "chorded / harmonize it when it fires / more
    //  whistling / more of a toot / using the same sample of course")
    {
        long srfN = 0;
        float *srf = try_load_sample("ut2004-shock-rifle.wav", &srfN);
        if (srf) {
            // 3-voice chord: -5 st (low root), 0 st (root), +7 st (5th up)
            // — slower rates = more whistling sustain.
            const double semis[3] = { -5.0, 0.0, +7.0 };
            const double pans[3]  = { -0.45, 0.0, +0.45 };
            // Gains pulled back ~45% — the chord trio was contributing
            // to a 5.76 engine peak which made normalize cut vocals to
            // -21 dB below the bed. Still loud but headroom-aware.
            const double gains[3] = {  0.78, 1.00, 0.66 };
            const double offs[3]  = {  0.000, 0.005, 0.012 };   // chorus stagger
            for (int v = 0; v < 3; v++) {
                PlaySampleOpts po = {0};
                // Lower rate base (0.85) = longer/whistlier toot
                po.rate = 0.85 * pow(2.0, semis[v] / 12.0);
                po.pan = pans[v];
                po.wet_send = 0.55;       // wetter for whistle/toot character
                po.fade = 0.020;          // softer attack for less zap
                play_sample(CLIMAX_START + offs[v], srf, srfN, gains[v], po);
            }
            free(srf);
            report("→ shock-rifle · CHORDED TOOT @ %.2fs (3 voices: -5/0/+7 st)",
                   CLIMAX_START);
        }
    }

    // ── GRAND PIANO ELABORATION off the climax drop ───────────────
    // 16th-note D-minor arpeggio cascade ascending then descending,
    // capped by a 32nd-note flourish — gestural burst that elaborates
    // OFF the lead at the 2nd drop. Uses the ac-native sample bank
    // (fedac/native/samples/piano/*.raw, 26 anchors decimated 192k→48k).
    // (@jeffrey "grand piano elaborations / off the last drop / 16th
    //  / 32nd note sequences coming out of the lead / burst of
    //  gestural grand piano / use the same sample set ac-native uses")
    {
        piano_bank_load();
        int anyLoaded = 0;
        for (int i = 0; i < PIANO_BANK_COUNT; i++)
            if (piano_samples[i]) { anyLoaded = 1; break; }
        if (anyLoaded) {
            // EXTENDED PIANO CASCADE that PERSISTS until 2:15 (135s),
            // SLOWING DOWN as notes progress. Starts at 16th-note
            // density and exponentially widens to ~half-note pace by
            // the end. D-minor pentatonic cycling through octaves so
            // the figure stays musical across the full ~24s span.
            // (@jeffrey "piano elaborations super cool but they should
            //  persist until like 2:15 / and slow down as they progress")
            const double cStart = CLIMAX_START + 0.50;      // 111.27s
            const double cEnd   = 126.0;                     // 2:06 cascade end
            const double startStep = SPB_G / 4.0;            // 16th @ 182 BPM (~82 ms)
            const double endStep   = SPB_G * 0.85;           // ~quarter note
            // D-minor pentatonic across multiple octaves (D=62 root):
            //   D, F, A, C, D, F, A, D, A, F, D, A, F, D, ...
            // Use a rotating arp pattern with octave variation.
            const int dminScale[7] = { 62, 65, 69, 72, 74, 77, 81 };  // D4 F4 A4 C5 D5 F5 A5
            const int octJumps[8]  = { 0, +12, 0, -7, 0, +12, +7, 0 };
            double t = cStart;
            int i = 0;
            while (t < cEnd) {
                // Exponential slowdown: linear-interpolated power curve
                const double prog = (t - cStart) / (cEnd - cStart);
                const double step = startStep + (endStep - startStep) * pow(prog, 1.6);
                // Pitch from rotating D-minor pentatonic + octave jump
                const int basePitch = dminScale[i % 7];
                const int octShift  = octJumps[i % 8];
                const int midi      = basePitch + octShift;
                // Gain bell: peaks early-middle, tapers off toward 2:15
                const double bell = sin(prog * M_PI);
                const double gn = 1.45 * (0.35 + 0.55 * bell);
                // Pan slowly sweeps across the cascade
                const double pn = sin(t * 0.7) * 0.55;
                play_grand_piano(t + hum(0.003), (double)midi, gn, pn);
                t += step;
                i++;
            }
            report("→ piano cascade · %d notes %.2fs→%.2fs (D-min pentatonic, exp slowdown)",
                   i, cStart, cEnd);

            // ── HELD CHORDS @ 2:06-2:15 (126→135s) with old swing ──
            // After the cascade dies down, switch to sustained bluesy
            // piano chords with slight triplet-swing late offset. Each
            // chord = 3-note voicing held for ~1.6s. Sequence walks a
            // ii-V-I-ish blues progression in D minor.
            // (@jeffrey "around 2:06 the piano elaborations should
            //  switch to held chords / and have a bit old swing to em")
            const double chordTimes[6] = {126.10, 127.60, 129.10, 130.60, 132.10, 133.60};
            // Each row = [root, 3rd-equiv, 7th-equiv] MIDI for piano triad
            const int chordVoicings[6][3] = {
                { 50, 53, 57 },     // D3 F3 A3   — Dm
                { 51, 53, 56 },     // D#3 F3 G#3 — bluesy dom
                { 48, 52, 55 },     // C3 E3 G3   — C
                { 50, 54, 57 },     // D3 F#3 A3  — D
                { 53, 57, 60 },     // F3 A3 C4   — F
                { 50, 53, 57 },     // Dm again (resolution)
            };
            for (int c = 0; c < 6; c++) {
                // Old-swing: slight late offset on chord trigger
                const double swingT = chordTimes[c] + 0.018 * (c % 2);
                for (int v = 0; v < 3; v++) {
                    play_grand_piano(swingT + 0.005 * v,
                                     (double)chordVoicings[c][v],
                                     1.30, ((v - 1) * 0.30));
                }
            }
            report("→ piano held-chords · 6 voicings 126.10-133.60s (D-min blues with swing)");
        }
    }
    // ── INSTRUMENT DIP @ 126-134s — duck L/R + wet so the held piano
    //    chords sit out front. (@jeffrey "and other instruments kind
    //    of dip to hear that")
    {
        const double dipStart = 125.8;
        const double dipEnd   = 134.5;
        const long dI = (long)(dipStart * SR);
        const long dE = (long)(dipEnd   * SR);
        const long span = dE - dI;
        for (long i = dI; i < dE && i < N; i++) {
            const double fr = (double)(i - dI) / (double)span;
            // Trapezoid duck: fade in 0..0.10, hold 0.10..0.85, fade out 0.85..1
            double dip;
            if      (fr < 0.10) dip = 1.0 - 0.40 * (fr / 0.10);
            else if (fr < 0.85) dip = 0.60;
            else                dip = 0.60 + 0.40 * ((fr - 0.85) / 0.15);
            L[i]  *= (float)dip;
            R[i]  *= (float)dip;
            WL[i] *= (float)(dip * dip);
            WR[i] *= (float)(dip * dip);
            SL[i] *= (float)(dip * dip);
            SR_[i]*= (float)(dip * dip);
        }
        report("→ instrument dip · %.1f-%.1fs (-4 dB hold for held piano)",
               dipStart, dipEnd);
    }

    int meowCount = 0;
    for (int m = 0; m < 3; m++) {
        long mn = 0;
        float *mb = try_load_sample(meows[m].path, &mn);
        if (!mb) continue;
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = meows[m].pan;
        po.wet_send = meows[m].wet; po.fade = 0.05;
        play_sample(meows[m].t, mb, mn, meows[m].gain, po);
        free(mb);
        meowCount++;
    }
    if (meowCount) report("→ %d kitten meows clustered 7-13s", meowCount);

    // ── overture distant birdies (cards-fast roll + L↔R chirps) ────────
    long cfn = 0;
    float *cardsFast = try_load_sample("cards-fast.wav", &cfn);
    long csn = 0;
    float *cardsSlow = try_load_sample("cards-slow.wav", &csn);
    long cmn = 0;
    float *cardsMed = try_load_sample("cards-medium.wav", &cmn);
    if (cardsFast || cardsSlow || cardsMed) {
        float *birdyBuf = cardsSlow ? cardsSlow : (cardsMed ? cardsMed : cardsFast);
        long   birdyN  = cardsSlow ? csn      : (cardsMed ? cmn     : cfn);
        float *birdyAlt = cardsMed ? cardsMed : (cardsFast ? cardsFast : birdyBuf);
        long   birdyAltN= cardsMed ? cmn      : (cardsFast ? cfn       : birdyN);
        float *rollBuf  = cardsFast ? cardsFast : birdyBuf;
        long   rollN    = cardsFast ? cfn      : birdyN;
        const struct { double t; int which; double sr1; double sr2; double pan; double g; double wet; double ms; } birdies[8] = {
            { 0.00, 2, 1.00, 1.05,  0.00, 0.30,  0.70, 1500 }, // roll opener
            { 1.40, 1, 2.20, 2.10,  0.85, 0.050, 0.90, 500 },
            { 2.20, 0, 1.75, 1.90, -0.75, 0.055, 0.88, 700 },
            { 3.00, 1, 2.30, 2.50,  0.90, 0.045, 0.92, 450 },
            { 3.80, 0, 1.80, 1.70, -0.85, 0.050, 0.90, 650 },
            { 4.60, 1, 2.00, 2.20,  0.70, 0.048, 0.92, 550 },
            { 5.40, 0, 1.95, 2.05, -0.95, 0.042, 0.93, 500 },
            { 6.20, 1, 2.40, 2.30,  0.80, 0.038, 0.94, 450 },
        };
        int bc = 0;
        for (int i = 0; i < 8; i++) {
            float *b = (birdies[i].which == 2) ? rollBuf : (birdies[i].which == 1 ? birdyAlt : birdyBuf);
            long bn  = (birdies[i].which == 2) ? rollN   : (birdies[i].which == 1 ? birdyAltN : birdyN);
            if (!b) continue;
            PlaySweptOpts po = {0};
            po.start_rate = birdies[i].sr1; po.end_rate = birdies[i].sr2;
            po.pan = birdies[i].pan; po.wet_send = birdies[i].wet;
            po.max_dur_ms = birdies[i].ms; po.fade = 0.040;
            play_sample_swept(birdies[i].t + hum(0.02), b, bn, birdies[i].g, po);
            bc++;
        }
        report("→ overture distant birdies · %d pitched card flaps", bc);
        // keep cards loaded for use below in WALL OF SOUND
    }

    // ── church bell — strike 0.80 s before the drop. 1.4 was too
    //    early, 0.5 was too late — split the difference so the bell's
    //    attack + initial bloom sit just ahead of the downbeat without
    //    feeling premature. (@jeffrey 2026-05-26 iterated)
    long bgn = 0;
    float *bellGong = try_load_sample("church-bell.wav", &bgn);
    if (bellGong) {
        PlaySampleOpts po = {0};
        po.rate = 2.6; po.pan = 0.0; po.wet_send = 0.75; po.fade = 0.005;
        play_sample(CLIMAX_START - 0.80, bellGong, bgn, 1.30, po);
        free(bellGong);
        report("→ church-bell · strike @ %.2fs → bloom lands on drop @ %.2fs",
               CLIMAX_START - 0.80, CLIMAX_START);
    }

    // ── GUITAR @ 23s + @ 43s — single sustained chord at each spot,
    //    drawn out over ~4 bars with a slight pick-bend at attack for
    //    real-guitar feel. The 9-hit riff was removed (felt too
    //    sample-loopy). A SECOND chord lands at 43s alongside a crow
    //    scratch to confuse the listener.
    //    (@jeffrey "keep that one initial one but slow it down so it
    //     draws out over a few bars / feel more like real electric
    //     guitar riff / throw another one at :43 along with a crow
    //     to confuse ppl")
    if (electric_guitar_buf && electric_guitar_n > 0) {
        // Helper: play one sustained chord with pick-bend attack + fade.
        // Inlined twice with different start times / pans / pitch.
        const long sampN = electric_guitar_n;
        const double bendDur = 0.045;     // tighter 45 ms pick-bend
        const long atkN  = (long)(0.003 * SR);     // shorter 3 ms attack

        // Beat-aligned positions at 182 BPM (beat = 60/182 ≈ 0.3297s)
        // chord1 = beat 70 = 23.077s, chord2 = beat 131 = 43.187s
        const double beatLenG = 60.0 / BPM;
        // Two chords with DIFFERENT pitch + DIFFERENT length so they
        // don't read as one repeated sample. (@jeffrey "should both
        // have a difference in pitch and length / start right on a
        // beat / shorter attack")
        struct { double t; double dur; double rateBase;
                 double bendStart; double gain; double panL; double panR; }
        chords[2] = {
            // chord 1 @ beat 70 (23.077s) — D root, 6-bar long sustain
            { 70.0 * beatLenG,  7.91, 1.00, 0.96, 0.55, 0.95, 1.05 },
            // chord 2 @ beat 131 (43.187s) — +5 st (G), shorter 4-bar
            { 131.0 * beatLenG, 5.27, 1.335, 0.88, 0.48, 1.15, 0.85 },
        };

        for (int c = 0; c < 2; c++) {
            const double t0  = chords[c].t;
            const double dur = chords[c].dur;
            const long iS   = (long)(t0 * SR);
            const long durSamp = (long)(dur * SR);
            const long bendN = (long)(bendDur * SR);
            const double gain = chords[c].gain;
            // Track read position manually so we can apply pick-bend rate
            double readPos = 0.0;
            for (long w = 0; w < durSamp; w++) {
                const long oi = iS + w;
                if (oi < 0 || oi >= N) continue;
                // Pick-bend at the start: rate sweeps from bendStart →
                // rateBase across first 80 ms (real guitar "bend up").
                const double rate = (w < bendN)
                    ? chords[c].bendStart + (chords[c].rateBase - chords[c].bendStart)
                                            * ((double)w / bendN)
                    : chords[c].rateBase;
                readPos += rate;
                if (readPos + 1 >= (double)sampN) break;
                const long ri = (long)readPos;
                const double frac = readPos - ri;
                double s = electric_guitar_buf[ri] * (1.0 - frac)
                         + electric_guitar_buf[ri + 1] * frac;
                const double envFade = 1.0 - ((double)w / (double)durSamp);
                const double atk = (w < atkN) ? ((double)w / atkN) : 1.0;
                const double v = s * gain * envFade * atk;
                L[oi]  += (float)(v * chords[c].panL);
                R[oi]  += (float)(v * chords[c].panR);
                SL[oi] += (float)(v * 0.22);
                SR_[oi]+= (float)(v * 0.22);
            }
        }
        report("→ 1st-drop guitar · 2 sustained chords @ 23.07s + 43.23s (bend attacks, ~4-bar each)");

        // CROW SCRATCH companion to the 43s chord — small flurry to
        // confuse the listener as the guitar chord rings.
        long encN = 0;
        float *encCrow = try_load_sample("crow.wav", &encN);
        if (encCrow) {
            const double cawOffs[3] = { 0.85, 1.55, 0.10 };
            const double cawRates[3] = { 1.40, 0.65, 1.30 };
            const double cawPans[3]  = { -0.55, 0.55, -0.30 };
            for (int i = 0; i < 3; i++) {
                PlaySweptOpts po2 = {0};
                const double base = cawRates[i];
                if (i % 2 == 0) { po2.start_rate = base * 1.20; po2.end_rate = base * 0.80; }
                else            { po2.start_rate = base * 0.85; po2.end_rate = base * 1.25; }
                po2.max_dur_ms = 200; po2.pan = cawPans[i];
                po2.wet_send = 0.25; po2.buf_offset = cawOffs[i]; po2.fade = 0.012;
                play_sample_swept(43.30 + i * 0.32, encCrow, encN, 0.90, po2);
            }
            free(encCrow);
            report("→ crow confusion · 3 chops 43.30-43.94s w/ 2nd guitar chord");
        }
    }

    // ── DROP SALVO @ 15.82s — TAMED (was glitchy). Reduced to 3 card
    //    flips + 2 low claps with lower gain, slower rates, and wider
    //    spacing so they punctuate without rattling.
    //    (@jeffrey "i would just tame it / make it less glitchy")
    {
        long cfN = 0;
        float *cf = try_load_sample("cards-fast.wav", &cfN);
        if (cf) {
            const double dropT = 15.82;
            const double step  = SPB_G / 2.0;       // 8th (was 16th)
            for (int k = 0; k < 3; k++) {           // 8 → 3 flips
                PlaySampleOpts po = {0};
                po.rate     = 1.10 + (k % 3) * 0.10;
                po.pan      = (k % 2 == 0) ? -0.45 : 0.45;
                po.wet_send = 0.40;
                po.fade     = 0.008;
                play_sample(dropT + k * step, cf, cfN, 0.28, po);   // 0.55 → 0.28
            }
            free(cf);
            report("→ drop salvo · 3 tame card flips @ %.2fs", dropT);
        }
        long clN = 0;
        float *clap = try_load_sample("clap.wav", &clN);
        if (clap) {
            const double dropT = 15.82;
            for (int k = 0; k < 2; k++) {           // 6 → 2 claps
                PlaySampleOpts po = {0};
                po.rate     = 0.65;                 // single rate, less wobbly
                po.pan      = (k == 0) ? -0.30 : 0.30;
                po.wet_send = 0.35;
                po.fade     = 0.006;
                play_sample(dropT + 0.10 + k * SPB_G, clap, clN, 0.22, po);
            }
            free(clap);
            report("→ drop salvo · 2 tame low claps");
        }
    }

    // ── chain drag @ 1:45 — REMOVED entirely (@jeffrey "lets lose the
    //    sound effect at 1:45"). Keep the loader code wrapped in if(0)
    //    so it's easy to revive.
    if (0) {
        long chN = 0;
        float *chBuf = try_load_sample("chain-trim.wav", &chN);
        if (chBuf) {
            PlaySampleOpts po = {0};
            po.rate = 0.65; po.pan = -0.20; po.wet_send = 0.55; po.fade = 0.05;
            play_sample(105.00, chBuf, chN, 0.65, po);
            free(chBuf);
        }
    }

    // ── iOS keyboard clicks @ 82s (develop callback to typewriter) ─────
    long iN = 0;
    float *iBuf = try_load_sample("ios-click.wav", &iN);
    if (iBuf) {
        PlaySampleOpts po = {0};
        po.rate = 1.10; po.pan = -0.20; po.wet_send = 0.35; po.fade = 0.02;
        play_sample(82.00, iBuf, iN, 0.55, po);
        free(iBuf);
        report("→ iOS keyboard clicks @ 82s");
    }

    // ── zipper + sipper @ ~1:16 ────────────────────────────────────────
    long zN = 0;
    float *zBuf = try_load_sample("zipper.wav", &zN);
    if (zBuf) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = -0.30; po.wet_send = 0.40; po.fade = 0.03;
        play_sample(76.00, zBuf, zN, 0.45, po);
        free(zBuf);
        report("→ zipper @ 76s");
    }
    long sN = 0;
    float *sBuf = try_load_sample("sipper.wav", &sN);
    if (sBuf) {
        PlaySampleOpts po = {0};
        po.rate = 1.0; po.pan = 0.30; po.wet_send = 0.40; po.fade = 0.03;
        play_sample(77.50, sBuf, sN, 0.45, po);
        free(sBuf);
        report("→ sipper @ 77.5s");
    }

    // ── crowd WOOO scratched @ climax→coda boundary (140.3-142.1s) ─────
    long wN = 0;
    float *wooBuf = try_load_sample("crowd-win.wav", &wN);
    if (wooBuf) {
        const struct { double t; double sr; double ms; double pan; double g; } woos[7] = {
            { 140.30, 0.65, 250, -0.55, 0.70 },
            { 140.55, 1.40, 180,  0.55, 0.72 },
            { 140.75, 0.85, 220, -0.30, 0.70 },
            { 140.97, 1.45, 180,  0.55, 0.62 },
            { 141.18, 1.00, 320,  0.00, 0.80 },
            { 141.60, 0.70, 380,  0.30, 0.55 },
            { 142.02, 1.50, 140, -0.55, 0.50 },
        };
        for (int i = 0; i < 7; i++) {
            PlaySweptOpts po = {0};
            po.start_rate = woos[i].sr;
            po.end_rate   = woos[i].sr * (0.95 + rng() * 0.10);
            po.max_dur_ms = woos[i].ms; po.pan = woos[i].pan;
            po.wet_send = 0.55; po.fade = 0.035;
            play_sample_swept(woos[i].t, wooBuf, wN, woos[i].g, po);
        }
        free(wooBuf);
        report("→ crowd WOOO scratched · 7 pitched chops 140.3-142.1s");
    }

    // ── crowd-roar scratched @ climax→coda boundary ────────────────────
    long rrN = 0;
    float *rrBuf = try_load_sample("crowd-roar.wav", &rrN);
    if (rrBuf) {
        const struct { double t; double sr; double ms; double off; double pan; double g; } roars[5] = {
            { 140.40, 0.60, 420, 2.0, -0.65, 0.48 },
            { 140.90, 1.30, 280, 5.0,  0.65, 0.48 },
            { 141.35, 0.78, 380, 1.0, -0.40, 0.48 },
            { 141.80, 1.10, 320, 8.0,  0.40, 0.50 },
            { 142.20, 0.55, 360, 3.0,  0.00, 0.42 },
        };
        for (int i = 0; i < 5; i++) {
            PlaySweptOpts po = {0};
            po.start_rate = roars[i].sr;
            po.end_rate   = roars[i].sr * (0.92 + rng() * 0.16);
            po.max_dur_ms = roars[i].ms; po.pan = roars[i].pan;
            po.wet_send = 0.70; po.buf_offset = roars[i].off; po.fade = 0.045;
            play_sample_swept(roars[i].t, rrBuf, rrN, roars[i].g, po);
        }
        free(rrBuf);
        report("→ crowd-roar scratched · 5 pitched chops 140.4-142.7s");
    }

    // ── WALL OF SOUND drop — drum grid + cards 3rd layer @ statement ───
    // 6-bar 16th grid using drum-1/2 + scratches. Exponential fade across
    // 96 steps (tau=32). Arpeggiated rates (D-min triad cycle).
    long dh1N=0, dh2N=0, dl1N=0, dl2N=0, dsA1N=0, dsB1N=0, dsA2N=0, dsB2N=0;
    float *dh1 = try_load_sample("drum-1.wav",          &dh1N);
    float *dh2 = try_load_sample("drum-2.wav",          &dh2N);
    float *dl1 = try_load_sample("drum-1-low.wav",      &dl1N);
    float *dl2 = try_load_sample("drum-2-low.wav",      &dl2N);
    float *dsA1= try_load_sample("drum-1-scratch-a.wav",&dsA1N);
    float *dsB1= try_load_sample("drum-1-scratch-b.wav",&dsB1N);
    float *dsA2= try_load_sample("drum-2-scratch-a.wav",&dsA2N);
    float *dsB2= try_load_sample("drum-2-scratch-b.wav",&dsB2N);
    int drumCount = (dh1?1:0)+(dh2?1:0)+(dl1?1:0)+(dl2?1:0)+(dsA1?1:0)+(dsB1?1:0)+(dsA2?1:0)+(dsB2?1:0);
    double stSecW = 15.82;
    for (int s = 0; s < n_section_ranges; s++) {
        if (!strcmp(section_ranges[s].name, "statement")) {
            stSecW = section_ranges[s].startSec; break;
        }
    }
    if (drumCount > 0) {
        const char *wallPat[6] = {
            "Bhth tKth Bhth tKth",
            "Bhth tKth BhBh tKtK",
            "Bhth tKth Bhth tKth",
            "BhBh KhBh BhBh KKKK",
            "Bhth tKth Bhth tKth",
            "BhBh KhBh BhBh KKKK",
        };
        const int arpSemis[16] = {0,3,7,12,0,3,7,12,15,19,12,7,3,12,7,0};
        int arpIdx = 0; int hits = 0;
        for (int bar = 0; bar < 6; bar++) {
            const char *pat = wallPat[bar];
            int stepIdx = 0;
            for (int p = 0; pat[p]; p++) {
                char c = pat[p];
                if (c == ' ' || c == '.') continue;
                const double stepT = stSecW + bar * SPBAR_G + stepIdx * (SPB_G / 4.0);
                const double ws = 0.65;
                const double fade = exp(-(bar * 16 + stepIdx) / 32.0);
                const double r = pow(2.0, arpSemis[arpIdx % 16] / 12.0);
                arpIdx++;
                if (c == 'B') {
                    if (dh1) { PlaySampleOpts po = {0}; po.rate = r; po.pan = -0.15 + hum(0.05); po.wet_send = ws; po.fade = 0.015;
                        play_sample(stepT + hum(0.003), dh1, dh1N, 0.88 * fade, po); hits++; }
                    if (dl1) { PlaySampleOpts po = {0}; po.rate = r; po.pan = 0.15 + hum(0.05); po.wet_send = ws; po.fade = 0.015;
                        play_sample(stepT + hum(0.005), dl1, dl1N, 0.82 * fade, po); hits++; }
                    if (dsB1) { PlaySampleOpts po = {0}; po.rate = r; po.pan = -0.55 + hum(0.06); po.wet_send = ws + 0.10; po.fade = 0.015;
                        play_sample(stepT + 0.012 + hum(0.004), dsB1, dsB1N, 0.72 * fade, po); hits++; }
                    if (dsA2) { PlaySampleOpts po = {0}; po.rate = r; po.pan = 0.55 + hum(0.06); po.wet_send = ws + 0.10; po.fade = 0.015;
                        play_sample(stepT + 0.014 + hum(0.004), dsA2, dsA2N, 0.72 * fade, po); hits++; }
                } else if (c == 'K') {
                    if (dl2) { PlaySampleOpts po = {0}; po.rate = r; po.pan = 0.20 + hum(0.05); po.wet_send = ws; po.fade = 0.015;
                        play_sample(stepT + hum(0.004), dl2, dl2N, 0.80 * fade, po); hits++; }
                    if (dsA2) { PlaySampleOpts po = {0}; po.rate = r; po.pan = -0.45 + hum(0.06); po.wet_send = ws + 0.05; po.fade = 0.015;
                        play_sample(stepT + 0.010 + hum(0.003), dsA2, dsA2N, 0.68 * fade, po); hits++; }
                } else if (c == 'h') {
                    if (dh2) { PlaySampleOpts po = {0}; po.rate = r; po.pan = hum(0.35); po.wet_send = ws; po.fade = 0.015;
                        play_sample(stepT + hum(0.003), dh2, dh2N, 0.62 * fade, po); hits++; }
                } else if (c == 't') {
                    float *sc = (stepIdx % 2 == 0) ? (dsB1 ? dsB1 : dsA2) : (dsA2 ? dsA2 : dsB1);
                    long scN = (sc == dsB1) ? dsB1N : (sc == dsA2 ? dsA2N : 0);
                    if (sc) { PlaySampleOpts po = {0}; po.rate = r; po.pan = (stepIdx % 2 == 0 ? -0.42 : 0.42) + hum(0.05); po.wet_send = ws; po.fade = 0.015;
                        play_sample(stepT + hum(0.003), sc, scN, 0.52 * fade, po); hits++; }
                }
                stepIdx++;
            }
        }
        report("→ WALL OF SOUND · %d drum-grid hits across 6 bars @ %.2fs", hits, stSecW);
    }
    if (dh1) free(dh1); if (dh2) free(dh2);
    if (dl1) free(dl1); if (dl2) free(dl2);
    if (dsA1) free(dsA1); if (dsB1) free(dsB1);
    if (dsA2) free(dsA2); if (dsB2) free(dsB2);

    // ── Kick rattle warps — every kick fires 1-2 shake samples that sweep
    //    in pitch (start brighter, end deeply pitched-down) inside the
    //    kick's body (~140-180 ms cap). Per-section gain/sweep config.
    {
        typedef struct { float *buf; long n; int durMs; } ShakeEntry;
        ShakeEntry pool[80]; int poolN = 0;
        DIR *d = opendir(HELLSINE_SAMPLES_DIR "/shakes");
        if (d) {
            struct dirent *ent;
            while ((ent = readdir(d)) && poolN < 80) {
                int idx, durMs;
                if (sscanf(ent->d_name, "shake-%d-%dms.wav", &idx, &durMs) != 2) continue;
                char rel[160];
                snprintf(rel, sizeof(rel), "shakes/%s", ent->d_name);
                long sn = 0;
                float *sb = try_load_sample(rel, &sn);
                if (!sb) continue;
                pool[poolN++] = (ShakeEntry){sb, sn, durMs};
            }
            closedir(d);
        }
        // Filter pool: ≥300 ms preferred; fallback ≥150 ms.
        ShakeEntry filt[80]; int filtN = 0;
        for (int i = 0; i < poolN; i++) if (pool[i].durMs >= 300) filt[filtN++] = pool[i];
        if (filtN < 4) {
            filtN = 0;
            for (int i = 0; i < poolN; i++) if (pool[i].durMs >= 150) filt[filtN++] = pool[i];
        }
        ShakeEntry ball[80]; int ballN = 0;
        ShakeEntry sweep[80]; int sweepN = 0;
        for (int i = 0; i < filtN; i++) {
            if (filt[i].durMs <= 250) ball[ballN++] = filt[i];
            if (filt[i].durMs >= 400) sweep[sweepN++] = filt[i];
        }
        if (ballN == 0) { for (int i = 0; i < filtN; i++) ball[ballN++] = filt[i]; }
        if (sweepN == 0) { for (int i = 0; i < filtN; i++) sweep[sweepN++] = filt[i]; }
        if (filtN > 0 && n_kick_events > 0) {
            // Independent rng so the kick rattles are deterministic and don't
            // disturb the shared rng state used elsewhere.
            uint32_t tRngState = fnv1a("hellsine-rattle-warp-hellsine");
            const double RATTLE_INTRO_GAIN = 0.48;
            int warps = 0;
            for (int k = 0; k < n_kick_events; k++) {
                const double t = kick_events[k];
                // pick section
                const SectionRange *sec = NULL;
                for (int s = 0; s < n_section_ranges; s++) {
                    if (t >= section_ranges[s].startSec && t < section_ranges[s].endSec) {
                        sec = &section_ranges[s]; break;
                    }
                }
                if (!sec) sec = &section_ranges[1];   // fallback to statement
                // section settings (matches JS SEC_RATTLE)
                double startSemi, endSemi, gain, maxMs; int n;
                if      (!strcmp(sec->name, "overture"))  { startSemi=-5; endSemi=-26; n=1; gain=0.20; maxMs=180; }
                else if (!strcmp(sec->name, "statement")) { startSemi=-2; endSemi=-24; n=1; gain=0.34; maxMs=180; }
                else if (!strcmp(sec->name, "bridge"))    { startSemi= 0; endSemi=-22; n=1; gain=0.30; maxMs=180; }
                else if (!strcmp(sec->name, "develop"))   { startSemi= 2; endSemi=-22; n=2; gain=0.40; maxMs=170; }
                else if (!strcmp(sec->name, "climax"))    { startSemi= 4; endSemi=-20; n=2; gain=0.46; maxMs=170; }
                else                                       { startSemi=-2; endSemi=-24; n=1; gain=0.26; maxMs=190; }
                // ramp per section
                const double local = (t - sec->startSec) / (sec->endSec - sec->startSec);
                double r;
                if (!strcmp(sec->name, "statement")) r = 0.30 + 0.70 * local;
                else if (!strcmp(sec->name, "coda")) r = 1.0 - 0.30 * local;
                else r = 1.0;
                const double baseG = gain * RATTLE_INTRO_GAIN * 1.8 * r;
                for (int i = 0; i < n; i++) {
                    // tRng: inline xorshift32
                    #define TRNG() ({ uint32_t _s = tRngState; _s ^= _s << 13; _s ^= _s >> 17; _s ^= _s << 5; tRngState = _s; (double)_s / 4294967296.0; })
                    const int sIdx = (int)(TRNG() * ballN); const ShakeEntry *sh = &ball[sIdx % ballN];
                    const double semiStart = startSemi + (TRNG() * 2.0 - 1.0) * 1.5;
                    const double semiEnd   = endSemi   + (TRNG() * 2.0 - 1.0) * 1.5;
                    const double startR = pow(2.0, semiStart / 12.0);
                    const double endR   = pow(2.0, semiEnd / 12.0);
                    const double offset = (i == 0) ? 0.002 : 0.018;
                    const double pan = ((i & 1) ? 1 : -1) * (0.40 + TRNG() * 0.35);
                    const double g = baseG * (0.80 + TRNG() * 0.30);
                    PlaySweptOpts po = {0};
                    po.start_rate = startR; po.end_rate = endR;
                    po.max_dur_ms = maxMs + (int)(TRNG() * 20);
                    po.pan = pan; po.wet_send = 0.95; po.fade = 0.015;
                    play_sample_swept(t + offset, sh->buf, sh->n, g, po);
                    warps++;
                }
                // climax: every 16th rattle gets a deep long sweep underneath
                if (sweepN > 0 && !strcmp(sec->name, "climax") && warps % 16 == 0) {
                    const ShakeEntry *sw = &sweep[(int)(TRNG() * sweepN) % sweepN];
                    PlaySweptOpts po = {0};
                    po.start_rate = pow(2.0, -2.0 / 12.0);
                    po.end_rate   = pow(2.0, -22.0 / 12.0);
                    po.max_dur_ms = 280;
                    po.pan = (TRNG() * 2.0 - 1.0) * 0.5;
                    po.wet_send = 1.0; po.fade = 0.015;
                    play_sample_swept(t + 0.005, sw->buf, sw->n, baseG * 0.55, po);
                    warps++;
                }
                #undef TRNG
            }
            report("→ kick-rattle warps · %d swept rattles inside %d kicks (pool %d)",
                   warps, n_kick_events, filtN);
        }
        for (int i = 0; i < poolN; i++) free(pool[i].buf);
    }

    // ── Star Wars blaster — reverse-flanged riser + forward-flanged hit ──
    // Flange = single feedforward comb modulated by slow cos LFO.
    long blN = 0;
    float *blast = try_load_sample("starwars-blaster.wav", &blN);
    if (blast) {
        // build reverse
        float *blRev = (float*)malloc(blN * sizeof(float));
        for (long i = 0; i < blN; i++) blRev[i] = blast[blN - 1 - i];
        float *revFlanged = flange_buf(blRev, blN, 6.0, 0.3, 0.7);
        float *fwdFlanged = flange_buf(blast, blN, 4.0, 1.2, 0.6);
        const double revRate = 0.45;
        const double revDur = (double)blN / SR / revRate;
        PlaySampleOpts po = {0};
        po.rate = revRate; po.pan = 0.0; po.wet_send = 0.85; po.fade = 0.050;
        play_sample(stSecW - revDur + hum(0.004), revFlanged, blN, 0.60, po);
        PlaySampleOpts po2 = {0};
        po2.rate = 1.2; po2.pan = 0.0; po2.wet_send = 0.55; po2.fade = 0.015;
        play_sample(stSecW + hum(0.004), fwdFlanged, blN, 0.85, po2);
        free(revFlanged); free(fwdFlanged); free(blRev); free(blast);
        report("→ Star Wars blaster · reverse riser + forward hit @ drop");
    }
}

// ── 32-bit float stereo WAV writer ────────────────────────────────────
static void write_wav_f32_stereo(const char *path, const float *l, const float *r, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { perror("fopen"); exit(1); }
    const uint32_t dataLen = (uint32_t)(n * 2 * 4);
    const uint32_t riffSize = 36 + dataLen;
    const uint32_t fmtSize = 16;
    const uint16_t fmtCode = 3;          // IEEE float
    const uint16_t ch = 2;
    const uint32_t sr = (uint32_t)SR;
    const uint32_t byteRate = SR * 2 * 4;
    const uint16_t blockAlign = 2 * 4;
    const uint16_t bps = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riffSize, 4, 1, f);
    fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fmtSize, 4, 1, f);
    fwrite(&fmtCode, 2, 1, f); fwrite(&ch, 2, 1, f);
    fwrite(&sr, 4, 1, f); fwrite(&byteRate, 4, 1, f);
    fwrite(&blockAlign, 2, 1, f); fwrite(&bps, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dataLen, 4, 1, f);
    float *interleaved = (float*)malloc((size_t)n * 2 * sizeof(float));
    for (long i = 0; i < n; i++) {
        interleaved[i * 2 + 0] = l[i];
        interleaved[i * 2 + 1] = r[i];
    }
    fwrite(interleaved, sizeof(float), (size_t)n * 2, f);
    free(interleaved);
    fclose(f);
}

// ── allocate shared buffers for a given duration ──────────────────────
static void alloc_buffers(double totalSec) {
    N = (long)ceil(totalSec * SR);
    L  = (float*)calloc((size_t)N, sizeof(float));
    R  = (float*)calloc((size_t)N, sizeof(float));
    WL = (float*)calloc((size_t)N, sizeof(float));
    WR = (float*)calloc((size_t)N, sizeof(float));
    SL = (float*)calloc((size_t)N, sizeof(float));
    SR_ = (float*)calloc((size_t)N, sizeof(float));
    DUCK = (float*)malloc((size_t)N * sizeof(float));
    if (!L || !R || !WL || !WR || !SL || !SR_ || !DUCK) {
        fprintf(stderr, "alloc failed (%.1f MB)\n", 7.0 * N * 4 / 1.0e6);
        exit(1);
    }
    for (long i = 0; i < N; i++) DUCK[i] = 1.0f;
}

// ── SPATIAL RESONATOR — bright metallic slap-back FDN ─────────────────
// 4 parallel comb filters with shorter delays (12-38 ms) + 2 series
// allpasses with higher FB. Hadamard-mixed feedback for diffusion.
// Distinct from the cathedral Schroeder: NO lowpass damping → metallic
// "concrete room" character. Used as the brass+click wet bus for NIN /
// Skinny-Puppy industrial space.
static const double SP_COMB_L_D[4] = {0.0123, 0.0181, 0.0257, 0.0319};
static const double SP_COMB_R_D[4] = {0.0131, 0.0191, 0.0269, 0.0331};
static const double SP_COMB_FB = 0.78;
static const double SP_AP_D[2]  = {0.0083, 0.0029};
static const double SP_AP_FB    = 0.55;

static void *spatial_thread(void *arg) {
    ReverbJob *job = (ReverbJob*)arg;
    int combLens[4]; float *combLines[4]; int combIdx[4] = {0,0,0,0};
    int apLens[2];   float *apLines[2];   int apIdx[2]   = {0,0};
    for (int c = 0; c < 4; c++) {
        combLens[c] = (int)(job->combs[c] * SR);
        combLines[c] = (float*)calloc(combLens[c], sizeof(float));
    }
    for (int a = 0; a < 2; a++) {
        apLens[a] = (int)(SP_AP_D[a] * SR);
        apLines[a] = (float*)calloc(apLens[a], sizeof(float));
    }
    for (long i = 0; i < N; i++) {
        const double in = job->in[i];
        // Read all 4 comb outputs first (for Hadamard mixing of feedback)
        double y[4];
        for (int c = 0; c < 4; c++) y[c] = combLines[c][combIdx[c]];
        // 4×4 Hadamard mix matrix / 2 normalization — diffuses the feedback
        double m[4];
        m[0] = (y[0] + y[1] + y[2] + y[3]) * 0.5;
        m[1] = (y[0] - y[1] + y[2] - y[3]) * 0.5;
        m[2] = (y[0] + y[1] - y[2] - y[3]) * 0.5;
        m[3] = (y[0] - y[1] - y[2] + y[3]) * 0.5;
        // Write input + mixed feedback into delay lines
        double combOut = 0.0;
        for (int c = 0; c < 4; c++) {
            combLines[c][combIdx[c]] = (float)(in + m[c] * SP_COMB_FB);
            combIdx[c] = (combIdx[c] + 1) % combLens[c];
            combOut += y[c];
        }
        combOut *= 0.25;
        // Allpass smear
        double apOut = combOut;
        for (int a = 0; a < 2; a++) {
            const int idx = apIdx[a];
            const double delayed = apLines[a][idx];
            const double newS = apOut + delayed * SP_AP_FB;
            apLines[a][idx] = (float)newS;
            apOut = delayed - newS * SP_AP_FB;
            apIdx[a] = (idx + 1) % apLens[a];
        }
        job->out[i] = (float)apOut;
    }
    for (int c = 0; c < 4; c++) free(combLines[c]);
    for (int a = 0; a < 2; a++) free(apLines[a]);
    return NULL;
}

// ── apply reverb + normalize + write ──────────────────────────────────
static void finalize_and_write(const char *path, double wet_mix) {
    // SANITIZE — replace any NaN/Inf in the dry + wet + spatial buses with
    // zero. A couple of voices hit a div-by-zero at the climax/coda
    // boundary (≈ 142.4 s) and propagate NaN through reverb + tanh + bit-
    // crush, silencing everything after that point. Cheap defensive scrub.
    long nanCount = 0;
    for (long i = 0; i < N; i++) {
        if (!isfinite(L[i]))  { L[i] = 0;  nanCount++; }
        if (!isfinite(R[i]))  { R[i] = 0;  nanCount++; }
        if (!isfinite(WL[i])) { WL[i] = 0; nanCount++; }
        if (!isfinite(WR[i])) { WR[i] = 0; nanCount++; }
        if (!isfinite(SL[i])) { SL[i] = 0; nanCount++; }
        if (!isfinite(SR_[i])){ SR_[i] = 0; nanCount++; }
    }
    if (nanCount > 0) report("· sanitized %ld NaN/Inf samples", nanCount);

    float *wetL = (float*)calloc((size_t)N, sizeof(float));
    float *wetR = (float*)calloc((size_t)N, sizeof(float));
    float *spatL = (float*)calloc((size_t)N, sizeof(float));
    float *spatR = (float*)calloc((size_t)N, sizeof(float));
    ReverbJob jobL = { .in = WL, .out = wetL, .combs = COMB_L_D };
    ReverbJob jobR = { .in = WR, .out = wetR, .combs = COMB_R_D };
    ReverbJob jobSL = { .in = SL,  .out = spatL, .combs = SP_COMB_L_D };
    ReverbJob jobSR = { .in = SR_, .out = spatR, .combs = SP_COMB_R_D };
    pthread_t thL, thR, thSL, thSR;
    pthread_create(&thL, NULL, reverb_thread, &jobL);
    pthread_create(&thR, NULL, reverb_thread, &jobR);
    pthread_create(&thSL, NULL, spatial_thread, &jobSL);
    pthread_create(&thSR, NULL, spatial_thread, &jobSR);
    pthread_join(thL, NULL);
    pthread_join(thR, NULL);
    pthread_join(thSL, NULL);
    pthread_join(thSR, NULL);

    // Time-varying wet mix matches hellsine.mjs — reverb fades in from
    // t=0 → drop (statement startSec, ≈15.82 s @ 182 BPM) so the intro
    // builds bloom and the drop arrives in full reverb.
    double revFadeEnd = 15.82;
    for (int s = 0; s < n_section_ranges; s++) {
        if (!strcmp(section_ranges[s].name, "statement")) {
            revFadeEnd = section_ranges[s].startSec; break;
        }
    }
    if (revFadeEnd <= 0) revFadeEnd = 15.82;
    // Mix BOTH the cathedral Schroeder (slow lush hall) AND the spatial
    // resonator (bright metallic slap) back into L/R. The cathedral fades
    // in from t=0; the spatial bus stays at constant wet level since it's
    // an "always-on" room — voices that send to it are always in the room.
    const double SPATIAL_MIX = 0.55;        // strong — defines the room
    for (long i = 0; i < N; i++) {
        const double t = (double)i / SR;
        const double wet = (t < revFadeEnd)
            ? wet_mix * (t / revFadeEnd)
            : wet_mix;
        L[i] += (float)(wetL[i] * wet + spatL[i] * SPATIAL_MIX);
        R[i] += (float)(wetR[i] * wet + spatR[i] * SPATIAL_MIX);
    }
    free(wetL); free(wetR);
    free(spatL); free(spatR);

    double peak = 0.0;
    for (long i = 0; i < N; i++) {
        const double a = fabs(L[i]); const double b = fabs(R[i]);
        if (a > peak) peak = a; if (b > peak) peak = b;
    }
    const double g = peak > 0.0 ? fmin(1.0, 0.89 / peak) : 1.0;
    // INDUSTRIAL MASTER — tanh glue × 1.04 (the original "DistroKid glue")
    // PLUS subtle bit-crush (13-bit quantize) for the NIN/Skinny-Puppy
    // bitty character. 13 bits = 8192 steps; just audible as grit on tails
    // without destroying transient fidelity.
    const double steps = 8192.0;
    for (long i = 0; i < N; i++) {
        double xl = tanh((double)L[i] * g * 1.04);
        double xr = tanh((double)R[i] * g * 1.04);
        xl = round(xl * steps) / steps;
        xr = round(xr * steps) / steps;
        L[i] = (float)xl;
        R[i] = (float)xr;
    }
    report("normalize · peak %.3f → gain %.3f", peak, g);
    report("write · %s", path);
    write_wav_f32_stereo(path, L, R, N);
}

// ── test phrases for compare.mjs ──────────────────────────────────────
// Each renders a deterministic set of isolated notes at known times so
// compare.mjs can A/B against a JS reference render.
static void test_voice(void) {
    // 8 notes, 0.5 s each, ascending Dm scale starting at D4
    const int scale[8] = {62, 64, 65, 67, 69, 70, 72, 74};
    for (int i = 0; i < 8; i++) {
        const double t = 0.05 + i * 0.55;
        VoiceOpts vo = {0};
        vo.atk = 0.05; vo.rel = 0.18;
        vo.vibR = 5.2; vo.vibD = 0.006;
        vo.drive = 1.0; vo.wet_send = 0.0;
        voice_render(t, 0.40, scale[i], 0.18, vo);
    }
}

static void test_bell(void) {
    const int notes[5] = {62, 65, 69, 72, 74};      // D-min-7 voicing
    for (int i = 0; i < 5; i++) {
        const double t = 0.1 + i * 0.30;            // overlapping bells
        BellOpts bo = {0};
        bo.atk = 0.080; bo.dec_tau = 2.5; bo.wet_send = 0.0; bo.fizzle_on = 1;
        bell_render(t, notes[i], 0.10, bo);
    }
}

static void test_sub(void) {
    // 8 sub hits at 16th-note grid, fundamental D2-ish
    const double SPB_test = 60.0 / 174.0;          // 174 BPM, like hellsine
    for (int i = 0; i < 8; i++) {
        const double t = 0.05 + i * SPB_test;
        sub_render(t, SPB_test * 0.7, 38, 0.6);    // D2
    }
}

static void test_kick(void) {
    // 4 hole-kicks @ HELL=11 drive
    for (int i = 0; i < 4; i++) {
        const double t = 0.1 + i * 1.0;
        kick_render(t, 11.0, 1.0, 0.0);
    }
}

static void test_snare(void) {
    // 4 snares — rng consumed deterministically (96 + 96 per hit)
    for (int i = 0; i < 4; i++) {
        const double t = 0.1 + i * 1.0;
        snare_render(t, 0.5, 175.0);
    }
}

static void test_steam(void) {
    // one 4-s steam release
    steam_render(0.1, 4.0, 0.12, 130, 400.0, 5500.0, 0.6, 1.2, 0.5, 0.35);
}

static void test_woodtick(void) {
    // 16 wood ticks at ~80 ms apart
    for (int i = 0; i < 16; i++) {
        woodtick_render(0.05 + i * 0.08, 0.13);
    }
}

static void test_tick(void) {
    // 16 alternating closed/open hat ticks
    for (int i = 0; i < 16; i++) {
        tick_render(0.05 + i * 0.08, 0.22, i % 2);
    }
}

static void test_piano(void) {
    // 5 piano notes, ascending D minor
    const int scale[5] = {62, 65, 69, 72, 74};
    for (int i = 0; i < 5; i++) {
        PianoOpts po = {0};
        po.sus = 1.0; po.bits = 6; po.hold = 4;
        piano_render(0.05 + i * 1.0, 0.70, scale[i], 0.14, po);
    }
}

static void test_saw(void) {
    // 4 sawLead notes (no gate, no sidechain since DUCK is 1.0)
    const int notes[4] = {62, 65, 69, 72};
    for (int i = 0; i < 4; i++) {
        SawOpts so = {0};
        so.partials = 18; so.detune = 0.006;
        so.atk = 0.012; so.rel = 0.06; so.drive = 0.8;
        saw_render(0.05 + i * 1.2, 1.0, notes[i], 0.16, so);
    }
}

static void test_hoover(void) {
    // 3 hoover blasts
    const int notes[3] = {50, 53, 57};
    for (int i = 0; i < 3; i++) {
        hoover_render(0.1 + i * 1.6, 1.4, notes[i], 0.30);
    }
}

static void test_stab(void) {
    // 8 fast stabs at 16th-grid
    const int notes[8] = {62, 62, 65, 67, 62, 65, 67, 69};
    const double SPB_test = 60.0 / 174.0;
    for (int i = 0; i < 8; i++) {
        stab_render(0.05 + i * SPB_test * 0.5, notes[i], 0.34);
    }
}

static void test_riser(void) {
    // one 4-s riser D2 → D5
    riser_render(0.1, 4.0, 38, 86, 0.26);
}

static void test_bubble(void) {
    // 8 bubbles with varying radius, evenly spaced
    for (int i = 0; i < 8; i++) {
        const double radiusMM = 1.5 + i * 0.6;
        bubble_render(0.05 + i * 0.5, radiusMM, 0.3, 0.012, 0.0, 0.0, 1.0);
    }
}

// Test samples — load pop/hellsine/samples/clap.wav and play it 6× with
// alternating rate/pan to exercise both play_sample and play_sample_swept.
#define TEST_SAMPLE_PATH "pop/hellsine/samples/clap.wav"
static void test_sample(void) {
    long buf_n = 0;
    float *clap = load_wav_mono(TEST_SAMPLE_PATH, &buf_n);
    if (!clap) { fprintf(stderr, "[test_sample] clap.wav missing — skip\n"); return; }
    report("loaded clap.wav · %ld samples (%.3f s)", buf_n, (double)buf_n / SR);
    for (int i = 0; i < 6; i++) {
        PlaySampleOpts po = {0};
        po.rate = 1.0 + i * 0.15;     // 1.0 → 1.75 (pitch up)
        po.pan  = (i % 2) ? -0.4 : 0.4;
        po.wet_send = 0.0;
        po.fade = 0.015;
        play_sample(0.10 + i * 0.6, clap, buf_n, 0.5, po);
    }
    free(clap);
}
static void test_sample_swept(void) {
    long buf_n = 0;
    float *clap = load_wav_mono(TEST_SAMPLE_PATH, &buf_n);
    if (!clap) { fprintf(stderr, "[test_sample_swept] clap.wav missing — skip\n"); return; }
    for (int i = 0; i < 6; i++) {
        PlaySweptOpts po = {0};
        po.start_rate = 0.5 + i * 0.3;
        po.end_rate   = po.start_rate * 1.5;
        po.pan = (i % 2) ? -0.3 : 0.3;
        po.max_dur_ms = 220;
        po.fade = 0.028;
        po.buf_offset = 0.0;
        play_sample_swept(0.10 + i * 0.6, clap, buf_n, 0.6, po);
    }
    free(clap);
}

// ── main ──────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    t0_wall = now_wall();

    char *out_default = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) { OUT_PATH = argv[++i]; }
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) { BPM = atof(argv[++i]); }
        else if (!strcmp(argv[i], "--seed") && i + 1 < argc) { SEED_STR = argv[++i]; }
        else if (!strcmp(argv[i], "--test") && i + 1 < argc) { TEST_NAME = argv[++i]; }
        else if (!strcmp(argv[i], "--hell") && i + 1 < argc) { HELL = atof(argv[++i]); }
        else if (!strcmp(argv[i], "--nokick")) { NOKICK = 1; }
        else if (!strcmp(argv[i], "--ultimate")) { ULTIMATE = 1; }
        else if (!strcmp(argv[i], "--strategy") && i + 1 < argc) {
            const char *v = argv[++i];
            if (!strcmp(v, "ultimate")) ULTIMATE = 1;
            // other strategies not yet ported — silently ignored
        }
        else if (!strcmp(argv[i], "--rattle") && i + 1 < argc) { RATTLE_MODE = argv[++i]; }
        else if (!strcmp(argv[i], "--rattle-gain") && i + 1 < argc) { RATTLE_GAIN = atof(argv[++i]); }
        else if (!strcmp(argv[i], "--lead") && i + 1 < argc) {
            const char *lv = argv[++i];
            if (!strcmp(lv, "powersine"))  LEAD_KIND = LEAD_POWERSINE;
            else if (!strcmp(lv, "brass")) LEAD_KIND = LEAD_BRASS;
        }
        else if (!strcmp(argv[i], "--humanize") && i + 1 < argc) {
            HUMANIZE_MULT = atof(argv[++i]);
        }
        else if (!strcmp(argv[i], "--wet-mix") && i + 1 < argc) {
            WET_MIX_OVERRIDE = atof(argv[++i]);
        }
    }
    if (!OUT_PATH) {
        const char *home = getenv("HOME"); if (!home) home = ".";
        const size_t LEN = strlen(home) + 100;
        out_default = (char*)malloc(LEN);
        if (TEST_NAME) {
            snprintf(out_default, LEN, "./hellsine-test-%s.wav", TEST_NAME);
        } else {
            snprintf(out_default, LEN,
                     "%s/Documents/Working Desktop/hellsine/.hellsine-c-pre.wav", home);
        }
        OUT_PATH = out_default;
    }
    xorshift_state = fnv1a(SEED_STR);

    if (TEST_NAME) {
        // test mode: short fixed-length isolated voice render
        double testDur = 7.5;
        alloc_buffers(testDur);
        report("hellsine.c · test=%s · %.1fs · SR=%d · seed=%s",
               TEST_NAME, testDur, SR, SEED_STR);
        if (!strcmp(TEST_NAME, "voice"))         test_voice();
        else if (!strcmp(TEST_NAME, "bell"))     test_bell();
        else if (!strcmp(TEST_NAME, "sub"))      test_sub();
        else if (!strcmp(TEST_NAME, "kick"))     test_kick();
        else if (!strcmp(TEST_NAME, "snare"))    test_snare();
        else if (!strcmp(TEST_NAME, "steam"))    test_steam();
        else if (!strcmp(TEST_NAME, "woodtick")) test_woodtick();
        else if (!strcmp(TEST_NAME, "tick"))     test_tick();
        else if (!strcmp(TEST_NAME, "piano"))    test_piano();
        else if (!strcmp(TEST_NAME, "saw"))      test_saw();
        else if (!strcmp(TEST_NAME, "hoover"))   test_hoover();
        else if (!strcmp(TEST_NAME, "stab"))     test_stab();
        else if (!strcmp(TEST_NAME, "riser"))    test_riser();
        else if (!strcmp(TEST_NAME, "bubble"))   test_bubble();
        else if (!strcmp(TEST_NAME, "sample"))   test_sample();
        else if (!strcmp(TEST_NAME, "sampleswept")) test_sample_swept();
        else if (!strcmp(TEST_NAME, "all")) {
            test_voice();
            test_bell();
            test_sub();
            test_kick();
            test_snare();
            test_steam();
            test_woodtick();
            test_tick();
            test_piano();
            test_saw();
            test_hoover();
            test_stab();
            test_riser();
            test_bubble();
            test_sample();
            test_sample_swept();
        } else {
            fprintf(stderr, "unknown --test: %s\n", TEST_NAME);
            return 1;
        }
        finalize_and_write(OUT_PATH, 0.0);   // no reverb in isolation tests
        const double wall = now_wall() - t0_wall;
        report("done · %.2fs audio in %.2fs (%.1fx realtime)",
               testDur, wall, testDur / wall);
    } else {
        SPB_G   = 60.0 / BPM;
        SPBAR_G = 4.0 * SPB_G;
        // make sure the output directory exists
        char *dir = strdup(OUT_PATH);
        char *slash = strrchr(dir, '/');
        if (slash) {
            *slash = 0;
            char cmd[1024];
            snprintf(cmd, sizeof(cmd), "mkdir -p '%s'", dir);
            int unused = system(cmd); (void)unused;
        }
        free(dir);
        render_full_track();
        post_arrangement_grenade();
        post_arrangement_rattle();
        post_arrangement_ultimate();
        // ── WET-BUS ENVELOPE — three-stage:
        //   77-100s: progressively suck the cathedral + spatial reverb
        //    out (high-pass filter style — wet drops, dry stays).
        //   100-110.77s: bloom back UP for the climb to the church-bell
        //    + AC stamp drop at climax.
        //   110.77-145s (climax): drop wet back DOWN to 0.45× so the
        //    deep kicks + sampled guitar have more space.
        //   (@jeffrey "kick should be more low after the 2:00 drop /
        //    we need to give that part of the mix more space")
        {
            const double suckIn  = 77.0;
            const double suckBot = 100.0;
            const double bloomTop = 110.77;
            const double climaxEnd = 145.0;
            const double minWet  = 0.18;
            const double climaxWet = 0.45;       // attenuate during climax for space
            for (long i = (long)(suckIn * SR); i < N && i < (long)(climaxEnd * SR); i++) {
                const double t = (double)i / SR;
                double w;
                if (t < suckBot) {
                    const double p = (t - suckIn) / (suckBot - suckIn);
                    w = 1.0 - (1.0 - minWet) * p;
                } else if (t < bloomTop) {
                    const double p = (t - suckBot) / (bloomTop - suckBot);
                    w = minWet + (1.20 - minWet) * p;        // re-bloom into drop
                } else {
                    // Drop from bloom (1.20) down to climaxWet over 6s,
                    // then hold until climaxEnd.
                    const double p = (t - bloomTop) / 6.0;
                    const double ramp = (p < 1.0) ? p : 1.0;
                    w = 1.20 + (climaxWet - 1.20) * ramp;
                }
                WL[i]  *= (float)w;
                WR[i]  *= (float)w;
                SL[i]  *= (float)w;
                SR_[i] *= (float)w;
            }
            report("→ wet-bus 3-stage · suck %.1f→%.1f (%.2f) · bloom→drop · climax %.2f for space",
                   suckIn, suckBot, minWet, climaxWet);
        }

        // ── REVERB DIP @ ~18s ────────────────────────────────────────
        // Drier moment right after the drop — wet buses ducked so the
        // statement entry feels close + dry instead of cathedral.
        // (@jeffrey 2026-05-26 "around second 18 i want less reverb")
        {
            const double dipStart = 16.5;
            const double dipEnd   = 22.0;
            const long dI = (long)(dipStart * SR);
            const long dE = (long)(dipEnd   * SR);
            const long span = dE - dI;
            for (long i = dI; i < dE && i < N; i++) {
                const double tFr = (double)(i - dI) / (double)span;
                // Trapezoid: fade in 0..0.2, hold 0.2..0.8, fade out 0.8..1
                double dip;
                if      (tFr < 0.2)  dip = 1.0 - 0.65 * (tFr / 0.2);
                else if (tFr < 0.8)  dip = 0.35;
                else                 dip = 0.35 + 0.65 * ((tFr - 0.8) / 0.2);
                WL[i]  *= (float)dip;
                WR[i]  *= (float)dip;
                SL[i]  *= (float)dip;
                SR_[i] *= (float)dip;
            }
            report("→ reverb dip · %.1f-%.1fs · wet buses → 35%%", dipStart, dipEnd);
        }
        // ── PERCUSSION BREAK @ 1:30 — DISABLED ───────────────────────
        // Both the simple and trap-style rebuilds didn't land (@jeffrey
        // "that break is bad lets get rid of that break"). Code kept
        // behind `if (0)` so it's easy to revive if we want to try a
        // different break later.
        if (0) {
            const double brkStart = 88.5;
            const double brkEnd   = 94.5;
            const long bI = (long)(brkStart * SR);
            const long eI = (long)(brkEnd   * SR);
            const long span = eI - bI;
            // Trapezoid duck: fade-in 0..0.15, deep 0.15..0.75, climb out 0.75..1
            for (long i = bI; i < eI && i < N; i++) {
                const double tFr = (double)(i - bI) / (double)span;
                double duck;
                if      (tFr < 0.15) duck = 1.0 - 0.78 * (tFr / 0.15);
                else if (tFr < 0.75) duck = 0.22;
                else                 duck = 0.22 + 0.78 * ((tFr - 0.75) / 0.25);
                L[i]  *= (float)duck;
                R[i]  *= (float)duck;
                WL[i]  *= (float)(duck * duck * 0.45);
                WR[i]  *= (float)(duck * duck * 0.45);
                SL[i]  *= (float)(duck * duck * 0.45);
                SR_[i] *= (float)(duck * duck * 0.45);
            }
            // ── KICK PATTERN — 4-on-floor first half, double-time roll
            //    into the climb-out
            for (int k = 0; k < 8; k++) {
                const double tk = brkStart + 0.40 + k * 0.55;
                if (tk >= brkEnd - 0.60) break;
                kick_render(tk, 1.10, 0.66, 0.30);
            }
            // Build-roll kicks at 32nd-notes in the last bar (4 in 0.50s)
            for (int k = 0; k < 8; k++) {
                const double tk = brkEnd - 0.80 + k * 0.10;
                if (tk >= brkEnd - 0.05) break;
                kick_render(tk, 1.20, 0.52 + k * 0.04, 0.35);
            }
            // ── SNARES — backbeats + 16th ghost flams
            for (int k = 0; k < 6; k++) {
                const double ts = brkStart + 0.95 + k * 0.95;
                if (ts >= brkEnd) break;
                snare_render(ts, 0.48, 180);
                // Ghost flam right before the backbeat
                snare_render(ts - 0.08, 0.18, 175);
            }
            // Final snare ROLL into the back side (16ths accelerating to 32nds)
            for (int k = 0; k < 12; k++) {
                const double pp = (double)k / 11.0;
                const double step = 0.14 - 0.090 * pp;   // 140 → 50 ms
                const double ts = brkEnd - 1.20 + k * step;
                if (ts >= brkEnd) break;
                snare_render(ts, 0.22 + 0.20 * pp, 200);
            }
            // ── TRAP HI-HAT ROLLS — accelerating 16th → 32nd → 64th
            //    closed-hat ticks across the whole break
            int hatCount = 0;
            for (double th = brkStart + 0.08; th < brkEnd - 0.05; ) {
                const double phaseFr = (th - brkStart) / (brkEnd - brkStart);
                // Step shrinks across the break: 0.18s → 0.045s (8th → 32nd)
                const double step = 0.18 - 0.135 * phaseFr;
                // Velocity ramps so the roll BUILDS energy
                const double vel  = 0.28 + 0.40 * phaseFr;
                tick_render(th, vel, 1);   // open=1 = brighter
                th += step;
                hatCount++;
            }
            // ── RISER sweep into the back side ──
            riser_render(brkEnd - 1.20, 1.20, ROOT_MEL_H - 12, ROOT_MEL_H + 14, 0.32);
            report("→ trap break · 88.5-94.5s · ducked + 8+8 kicks + 6 snares + roll + %d hat ticks + riser", hatCount);
        }
        // ── SNARE RUSH @ ~2:02 — pushed back further (was 121.0,
        // now starts at 122.2) so the neigh @ 120.18 has clear space
        // to breathe before the rush kicks in. Velocity now FADES IN
        // across the first 25% of the rush instead of starting hot.
        // (@jeffrey "snare rush not happen until a bit after the neigh
        //  / fade it in")
        {
            const double rushStart = 122.2;
            const double rushEnd   = 128.5;
            const double whineStart = 125.4;
            const double whineEnd   = 126.70;
            int snareCount = 0;
            for (double t = rushStart; t < rushEnd - 0.04; ) {
                const double pp = (t - rushStart) / (rushEnd - rushStart);
                const double step = 0.18 - 0.135 * pp;
                // Velocity: fade IN first 25% (0→1) → peak → fade OUT
                // last 70% (1→0.18). Smooth attack into climax tail.
                const double fadeIn = (pp < 0.25) ? (pp / 0.25) : 1.0;
                const double decay  = 1.0 - (pp > 0.30 ? (pp - 0.30) / 0.70 * 0.80 : 0.0);
                const double vel    = 0.72 * fadeIn * decay;
                // Body pitch sweeps via sine across the rush — center
                // 175 Hz ±60 Hz with several cycles for movement.
                const double bodyF = 175.0 + 60.0 * sin(snareCount * 0.55);
                // Pan sweeps faster and wider across the field.
                SNARE_PAN_BIAS = sin(snareCount * 0.83) * 0.75;
                snare_render(t, vel, bodyF);
                if (snareCount % 4 == 0 && t + 0.025 < rushEnd) {
                    SNARE_PAN_BIAS = -SNARE_PAN_BIAS;        // opposite for the flam
                    snare_render(t + 0.025, vel * 0.40, bodyF * 1.18);
                }
                t += step;
                snareCount++;
            }
            SNARE_PAN_BIAS = 0.0;     // reset
            // Tight whine REMOVED — the "eeeeoooo" sound the user
            // wanted killed. (@jeffrey "at 2:05 there is a weird high
            //  pitched sound coming in like a eeeeoooo lets kill that")
            (void)whineStart; (void)whineEnd;
            report("→ snare rush · %.1f-%.1fs · %d snares fading into tight whine %.2f-%.2fs",
                   rushStart, rushEnd, snareCount, whineStart, whineEnd);
        }
        // ── FINAL LOUD KICK ON THE FADE — last hit, big and deep
        //    (bypasses the t>=140 cutoff via direct inline render so
        //    the bass-chord coda has time to ring out into the fade,
        //    capped by one final BOOM. (@jeffrey "very final sound
        //    should be a kick / loud kick on the fade / finish out
        //    the bass chords")
        {
            const double tFK   = 158.5;          // at fade start
            const double durFK = 0.55;
            const double pStart = 200.0, pEnd = 38.0, pT = 0.040;
            const double bodyTau = 0.18;
            const double gain    = 1.40;
            double ph = 0.0;
            long iS = (long)(tFK * SR); if (iS < 0) iS = 0;
            long iE = (long)((tFK + durFK) * SR + 1); if (iE > N) iE = N;
            for (long i = iS; i < iE; i++) {
                const double lt = (double)i / SR - tFK;
                const double f = pEnd + (pStart - pEnd) * exp(-lt / pT);
                ph += TAU * f / SR;
                const double atk   = 1.0 - exp(-lt / 0.008);
                const double decay = exp(-lt / bodyTau);
                const double amp   = atk * decay;
                double x = sin(ph);
                x = tanh(x * 1.30);
                const double v = x * amp * 0.92 * gain;
                L[i] += (float)v;
                R[i] += (float)v;
            }
            // Deep sub layer underneath for extra weight
            sub_render(tFK, 0.50, 26, 0.85);
            report("→ FINAL KICK · loud on fade @ %.2fs (bass-chord tail extended)", tFK);
        }
        const double wetMix = (WET_MIX_OVERRIDE >= 0.0) ? WET_MIX_OVERRIDE : 0.42;
        finalize_and_write(OUT_PATH, wetMix);
        const double wall = now_wall() - t0_wall;
        report("done · %.2f min audio in %.2fs (%.1fx realtime)",
               TOTAL_SEC_G / 60.0, wall, TOTAL_SEC_G / wall);
    }

    free(L); free(R); free(WL); free(WR); free(DUCK);
    if (out_default) free(out_default);
    return 0;
}
