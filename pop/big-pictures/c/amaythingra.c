// amaythingra.c — Loukeman-style melodic deep house remix of
// Amazing Grace, 120 BPM 4/4, ~4:00.
//
// Style targets: All Day I Dream / Sol Selectas vibes — long sustained
// pad chords with massive Schroeder reverb tails, sub-heavy 4-on-floor
// kick, organic percussion (shakers + open hats), filtered sine-cluster
// chord stabs every 4 bars, glock atmospherics. The vocal floats over
// the 4/4 grid in its native 70 BPM 3/4 phrasing — Loukeman's classic
// polyrhythmic float.
//
// Structure (G major, 4-bar chord rotation I-IV-I-V):
//   0..32 s   (bars 0-15)   INTRO  — sine pads only, slow swell
//   32..64 s  (bars 16-31)  BUILD  — kick + shaker enter, vocal pickup
//   32..87 s                VOCAL  — amaythingra-wavewizard.wav layered in
//                                    by the bake script (delayed +32 s)
//   64..96 s  (bars 32-47)  DROP 1 — full kit + clap + chord stab
//   96..128 s (bars 48-63)  BREAK  — pads only, vocal tails reverb out
//  128..192 s (bars 64-95)  DROP 2 — return of kick + bass, glock layer
//  192..240 s (bars 96-119) OUTRO  — filtered pad fade
//
// Voices: kick · sub-bass · sine-pad (low + mid octaves) · open-hat ·
//         shaker · clap · chord-stab (sine cluster + sharp envelope) ·
//         glock · Schroeder reverb (wet 0.55 — much wetter than hymn)
//
// Build: ./build-dance.sh
// Run:   ./amaythingra --out out/amaythingra.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

// ── config ────────────────────────────────────────────────────────────
static const int SR = 48000;
static double BPM = 120.0;
static double TOTAL_SEC = 274.0;   // ~4:34 — winds down + resolves (was a hard 5:00 cut, @jeffrey)
static const char *OUT_PATH = NULL;
// --drive <x>: pre-tanh scale. 1.0 (default) keeps the original hard
// tanh "limiter" used by the released cover. The vowel bake passes a
// small value (~0.30) so the master sums sit in tanh's gentle knee —
// soft saturation instead of brick-wall clipping (kills the "maxing
// out"/flat-topping). --mono-kick disables the 3 ms HAAS kick pair that
// smears the low end's phase; the bed's bass then folds cleanly to mono.
static double DRIVE = 1.0;
static int MONO_KICK = 0;
// --seed <n>: seeds the per-render variation (chord progression, hihat
// freakout, bell melodies). 0 (default) seeds from the wall clock, so
// every render is different; pass a fixed value to reproduce a take.
static unsigned SEED_ARG = 0;
static uint32_t RNG_STATE = 0x9e3779b9u;
static void rng_seed(uint32_t s) { RNG_STATE = s ? s : 0x9e3779b9u; }
static uint32_t rng_u32(void) {
    uint32_t x = RNG_STATE;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    RNG_STATE = x; return x;
}
static double rng_d(void) { return (double)rng_u32() / 4294967296.0; }
// inclusive integer range [lo, hi]
static int rng_range(int lo, int hi) {
    if (hi <= lo) return lo;
    return lo + (int)(rng_d() * (hi - lo + 1));
}

// ── shared mix buffers ────────────────────────────────────────────────
static long N = 0;
static float *L = NULL;
static float *R = NULL;
static float *WL = NULL;
static float *WR = NULL;

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

static inline double m2f(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── HUMANIZE — lazy-late timing offsets for percussion ───────────────
// Returns a small offset in SECONDS biased POSITIVE (drummer in the
// pocket, slightly behind the beat). Deterministic per-onset via FNV-1a
// of the position so the offsets don't change between renders.
//   max_ms = peak amplitude of the random component (e.g. 14 ms)
//   bias_ms = baseline late-ness (e.g. +4 ms)
static inline double humanize(double t, double max_ms, double bias_ms) {
    uint32_t h = (uint32_t)(t * 100000.0) | 1;
    // FNV scramble
    h ^= h << 13; h ^= h >> 17; h ^= h << 5;
    // Triangular distribution (rng + rng) - 1 → peaks around 0
    const double r1 = (double)h / 4294967296.0;
    h ^= h << 13; h ^= h >> 17; h ^= h << 5;
    const double r2 = (double)h / 4294967296.0;
    const double tri = (r1 + r2 - 1.0);     // -1..+1 triangle
    // FEEL ARC (@jeffrey): LAZY (behind the beat) through the first half,
    // easing to EAGER (ahead, rushing) in the back half. e ramps 0 @≤110 s
    // → 1 @≥180 s; feel +1=lazy(behind)…-1=eager(ahead); spread grows late.
    double e = (t - 110.0) / 70.0; if (e < 0.0) e = 0.0; if (e > 1.0) e = 1.0;
    const double feel = 1.0 - 2.0 * e;
    const double spread = 1.0 + 0.4 * e;
    return (feel * bias_ms + tri * max_ms * spread) / 1000.0;
}

// ── pipe-organ voice (drawbar additive, ported from amazinhym) ───────
typedef struct {
    double atk, rel;
    double vib_rate, vib_depth;
    double pan;
    double gain;
    double wet_send;
    double sub_amt;
    double upper_amt;
} OrganOpts;
static const double ORGAN_PARTIALS[8][2] = {
    {0.5, 0.55}, {1.0, 1.00}, {1.5, 0.42}, {2.0, 0.78},
    {3.0, 0.32}, {4.0, 0.55}, {5.0, 0.20}, {6.0, 0.18},
};
static void organ_render(double t0, double dur, double midi, OrganOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.08;
    const double rel = opt.rel > 0 ? opt.rel : 0.30;
    const double vibR = opt.vib_rate > 0 ? opt.vib_rate : 4.6;
    const double vibD_cents = opt.vib_depth > 0 ? opt.vib_depth : 5.0;
    const double vibD = pow(2.0, vibD_cents / 1200.0) - 1.0;
    const double sub_amt = opt.sub_amt >= 0 ? opt.sub_amt : 1.0;
    const double upper_amt = opt.upper_amt >= 0 ? opt.upper_amt : 1.0;
    double phs[8] = {0};
    double vibPhase = 0.0;
    const double dVib = vibR / (double)SR;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;
        vibPhase += dVib; if (vibPhase >= 1.0) vibPhase -= 1.0;
        const double vib = 1.0 + vibD * sin(TAU * vibPhase);
        double sample = 0.0;
        double ampsum = 0.0;
        for (int p = 0; p < 8; p++) {
            const double ratio = ORGAN_PARTIALS[p][0];
            double amp = ORGAN_PARTIALS[p][1];
            if (p == 0)      amp *= sub_amt;
            else if (p >= 4) amp *= upper_amt;
            ampsum += amp;
            const double dPhs = (f * ratio * vib) / (double)SR;
            phs[p] += dPhs; if (phs[p] >= 1.0) phs[p] -= 1.0;
            sample += amp * sin(TAU * phs[p]);
        }
        sample *= (env * opt.gain) / ampsum;
        const double sL = sample * (1.0 - 0.5 * opt.pan);
        const double sR = sample * (1.0 + 0.5 * opt.pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * opt.wet_send;
        WR[i] += sR * opt.wet_send;
    }
}

// ── reverse kick (rising sweep + crescendo envelope) ─────────────────
// Classic deep-house lead-in: pitch sweeps UP from low to mid, amplitude
// builds from near-silent to a sharp cutoff at the end — perfect "build"
// accent right before a downbeat. Used in place of the previous shuffle
// snare / crash patterns.
typedef struct {
    double pan;
    double gain;
    double pitch_lo;     // start frequency
    double pitch_hi;     // end frequency
    double dur;          // total duration (e.g. 0.5 s = 1 beat at 120 BPM)
} ReverseKickOpts;
static void reverse_kick_render(double t0, ReverseKickOpts opt) {
    const double pitch_lo = opt.pitch_lo > 0 ? opt.pitch_lo : 60.0;
    const double pitch_hi = opt.pitch_hi > 0 ? opt.pitch_hi : 220.0;
    const double dur = opt.dur > 0 ? opt.dur : 0.50;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double phase = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRel = t / dur;
        // Exponential pitch sweep UP
        const double freq = pitch_lo * pow(pitch_hi / pitch_lo, tRel);
        // Quadratic build envelope with sharp cutoff in the last 5%
        double env;
        if (tRel < 0.95) env = tRel * tRel;
        else env = (1.0 - tRel) / 0.05 * 0.95;
        if (env < 0) env = 0;
        const double dPhs = freq / (double)SR;
        phase += dPhs; if (phase >= 1.0) phase -= 1.0;
        const double sample = sin(TAU * phase) * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── powersaw voice (7-voice detuned supersaw — hymnal chord stack) ───
// JP-8000-style supersaw: 7 detuned sawtooths centered on the target
// pitch (±detune_cents spread). Sharp attack, sustained body, gentle
// release. Heavy reverb wet for the trance-hymnal vibe.
typedef struct {
    double atk, rel;
    double pan;
    double gain;
    double wet_send;
    double detune_cents;   // peak detune (0 = use default 15)
} PowersawOpts;
static inline double saw_wave(double phase) { return 2.0 * phase - 1.0; }
static void powersaw_render(double t0, double dur, double midi, PowersawOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.04;
    const double rel = opt.rel > 0 ? opt.rel : 0.40;
    const double detune = opt.detune_cents > 0 ? opt.detune_cents : 15.0;
    double phs[7] = {0};
    double freqs[7];
    for (int v = 0; v < 7; v++) {
        const double cents = detune * (v - 3) / 3.0;   // -detune .. +detune
        freqs[v] = f * pow(2.0, cents / 1200.0);
    }
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;
        double sample = 0.0;
        for (int v = 0; v < 7; v++) {
            phs[v] += freqs[v] / (double)SR;
            if (phs[v] >= 1.0) phs[v] -= 1.0;
            sample += saw_wave(phs[v]);
        }
        sample *= env * opt.gain / 7.0;
        const double sL = sample * (1.0 - 0.5 * opt.pan);
        const double sR = sample * (1.0 + 0.5 * opt.pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * opt.wet_send;
        WR[i] += sR * opt.wet_send;
    }
}

// ── kick (pitched sine sweep + click) ─────────────────────────────────
typedef struct {
    double pan;
    double gain;
    double pitch_hi;
    double pitch_lo;
    double pitch_dur;
    double decay_s;
    int    muffled;   // 1 = skip the click transient (faint, "through-wall" kick)
} KickOpts;
static void kick_render(double t0, KickOpts opt) {
    const double pitch_hi = opt.pitch_hi > 0 ? opt.pitch_hi : 140.0;
    const double pitch_lo = opt.pitch_lo > 0 ? opt.pitch_lo : 48.0;
    const double pitch_dur = opt.pitch_dur > 0 ? opt.pitch_dur : 0.055;
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.36;
    const double max_dur = decay * 5.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double phase = 0.0;
    double click_phase = 0.0;
    // Click reduced further for the electro-tonal sound — the body
    // sine + sustained decay carry the "thump", click is just a hint
    // of attack.
    const double click_freq = 1800.0;
    const double click_dur  = 0.0015;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double freq;
        if (t < pitch_dur) freq = pitch_hi * pow(pitch_lo / pitch_hi, t / pitch_dur);
        else freq = pitch_lo;
        double env;
        if (t < 0.002) env = t / 0.002;
        else env = exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-5) break;
        const double dPhs = freq / (double)SR;
        phase += dPhs; if (phase >= 1.0) phase -= 1.0;
        double sample = sin(TAU * phase) * env;
        if (!opt.muffled && t < click_dur) {
            const double cenv = (click_dur - t) / click_dur;
            click_phase += click_freq / (double)SR;
            if (click_phase >= 1.0) click_phase -= 1.0;
            sample += sin(TAU * click_phase) * cenv * 0.18;
        }
        sample *= opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── sub bass (deep sustained sine, slow attack) ──────────────────────
typedef struct {
    double atk, rel;
    double pan;
    double gain;
} BassOpts;
static void bass_render(double t0, double dur, double midi, BassOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.20;
    const double rel = opt.rel > 0 ? opt.rel : 0.60;
    const double pan = opt.pan;
    double phs1 = 0, phs2 = 0;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;
        const double dP1 = f / (double)SR;
        const double dP2 = (f * 2.0) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;
        const double sample = env * opt.gain *
                              (0.92 * sin(TAU * phs1) +
                               0.18 * sin(TAU * phs2));
        L[i] += sample * (1.0 - 0.5 * pan);
        R[i] += sample * (1.0 + 0.5 * pan);
    }
}

// ── sine pad (3-osc chord cluster + LFO + heavy reverb send) ─────────
typedef struct {
    double atk, rel;
    double pan;
    double gain;
    double wet_send;
    double lfo_rate;
    double lfo_depth;
} SinePadOpts;
static void sine_pad_render(double t0, double dur,
                            int midi_root, int midi_third, int midi_fifth,
                            SinePadOpts opt) {
    const double atk = opt.atk > 0 ? opt.atk : 0.30;
    const double rel = opt.rel > 0 ? opt.rel : 0.50;
    const double lfo_rate = opt.lfo_rate > 0 ? opt.lfo_rate : 0.18;
    const double lfo_depth = opt.lfo_depth >= 0 ? opt.lfo_depth : 0.15;

    double phs[3] = {0};
    double lfoPhase = 0.0;
    const double dLfo = lfo_rate / (double)SR;
    const double f[3] = {
        m2f(midi_root), m2f(midi_third), m2f(midi_fifth)
    };

    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;

        lfoPhase += dLfo; if (lfoPhase >= 1.0) lfoPhase -= 1.0;
        const double lfo = 1.0 - lfo_depth + lfo_depth *
                           (0.5 * (1.0 + sin(TAU * lfoPhase)));

        double s[3];
        for (int k = 0; k < 3; k++) {
            const double dPhs = f[k] / (double)SR;
            phs[k] += dPhs; if (phs[k] >= 1.0) phs[k] -= 1.0;
            s[k] = sin(TAU * phs[k]);
        }
        // Wide stereo: root centered, third left, fifth right
        const double sL = (s[0] * 0.50 + s[1] * 0.70 + s[2] * 0.35);
        const double sR = (s[0] * 0.50 + s[1] * 0.35 + s[2] * 0.70);
        const double scale = env * lfo * opt.gain;
        L[i] += sL * scale;
        R[i] += sR * scale;
        WL[i] += sL * scale * opt.wet_send;
        WR[i] += sR * scale * opt.wet_send;
    }
}

// ── chord stab (sharp-attack pad — sine cluster with envelope decay) ─
// House-style filtered stab. Lower-octave triad + upper-octave sparkle
// + fast envelope. Acts as the harmonic "punch" on bar boundaries.
typedef struct {
    double decay_s;
    double pan;
    double gain;
    double wet_send;
    double brightness;   // 0..1 — controls upper partial level
} ChordStabOpts;
static void chord_stab_render(double t0, int midi_root, int midi_third, int midi_fifth,
                              ChordStabOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.80;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.55;
    const double pan = opt.pan;
    const double max_dur = decay * 4.0;

    double phs[6] = {0};
    const double f[6] = {
        m2f(midi_root),       // low triad
        m2f(midi_third),
        m2f(midi_fifth),
        m2f(midi_root + 12),  // octave up sparkle
        m2f(midi_third + 12),
        m2f(midi_fifth + 12),
    };

    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;

    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        // Fast attack, exponential decay
        double env;
        if (t < 0.008) env = t / 0.008;
        else env = exp(-(t - 0.008) / decay);
        if (t > 0.005 && env < 1e-6) break;

        double sample = 0.0;
        for (int k = 0; k < 6; k++) {
            const double dPhs = f[k] / (double)SR;
            phs[k] += dPhs; if (phs[k] >= 1.0) phs[k] -= 1.0;
            const double amp = (k < 3) ? 1.0 : bright;
            sample += amp * sin(TAU * phs[k]);
        }
        sample *= env * opt.gain / 6.0;

        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * opt.wet_send;
        WR[i] += sR * opt.wet_send;
    }
}

// ── hi-hat (AC-native DOUBLE) ────────────────────────────────────────
// Mirrors notepat's makeHihat (system/disks/notepat.mjs ~line 7150):
// two layered noise bursts. One "main" + one "shimmer" delayed by ~4 ms
// for the doubled "tt-tch" character. double_delay_ms can be widened
// (10-25 ms) for a flam feel.
typedef struct {
    double pan;
    double gain;
    double decay_s;
    double brightness;
    double double_delay_ms;  // 0 = use default 4 ms
} HatOpts;
static void hat_burst(double t0, double pan, double gain, double decay,
                      double bright, uint32_t seed) {
    const double max_dur = decay * 6.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = seed | 1;
    double hp_in_prev = 0.0, hp_out_prev = 0.0;
    double hp2_in_prev = 0.0, hp2_out_prev = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.001) ? (t / 0.001) : exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp1 = bright * (hp_out_prev + noise - hp_in_prev);
        hp_in_prev = noise; hp_out_prev = hp1;
        const double hp2 = bright * (hp2_out_prev + hp1 - hp2_in_prev);
        hp2_in_prev = hp1; hp2_out_prev = hp2;
        const double sample = hp2 * env * gain;
        L[i] += sample * (1.0 - 0.5 * pan);
        R[i] += sample * (1.0 + 0.5 * pan);
    }
}
// Hi-hats are silent for the first HAT_IN_START seconds, then fade in
// over the next (HAT_IN_END - HAT_IN_START) (@jeffrey "no hihats for the
// first 5 seconds, then fade them in"). Applies to every hat globally;
// a no-op once past the fade.
#define HAT_IN_START 4.0
#define HAT_IN_END   20.0
// The ~2:00-2:14 rainstorm (accelerando→ritardando droplets) owns this
// window; steady offbeat hats step aside so it reads cleanly.
#define HAT_SLOW_START 124.0
#define HAT_SLOW_END   130.5
static double hat_in_gate(double t) {
    if (t < HAT_IN_START) return 0.0;
    if (t >= HAT_IN_END)  return 1.0;
    const double x = (t - HAT_IN_START) / (HAT_IN_END - HAT_IN_START);
    return x * x * x;   // cubic ease-in: very subtle at first, gradual swell
}
static void hat_render(double t0, HatOpts opt) {
    const double gate = hat_in_gate(t0);
    if (gate <= 0.0) return;            // muted intro window
    opt.gain *= gate;
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.028;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.95;
    const double gap = (opt.double_delay_ms > 0 ? opt.double_delay_ms : 4.0) / 1000.0;
    // Burst 1 — bright main
    hat_burst(t0, opt.pan, opt.gain, decay, bright,
              (uint32_t)(t0 * 7919.0 + 31));
    // Burst 2 — softer shimmer, slightly narrower, delayed
    hat_burst(t0 + gap, opt.pan, opt.gain * 0.65, decay * 0.85,
              bright * 0.92, (uint32_t)(t0 * 8087.0 + 17));
}

// ── AC-native perc kit hits (ported from lib/percussion.mjs) ─────────
// A single decaying voice: sine / triangle / square / lowpassed-noise
// with attack + exponential decay. woodblock / cowbell / tambo stack a
// few of these, matching the menuband 12-drum kit specs.
enum PercWave { PW_SINE, PW_TRI, PW_SQR, PW_NOISE };
static void perc_hit(double t0, int wave, double tone, double dur,
                     double vol, double atk, double dec, double pan, double wet) {
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double phase = 0.0;
    const double dP = tone / (double)SR;
    uint32_t s = (uint32_t)(t0 * 99877.0) | 1;
    double lp = 0.0;
    const double lpc = exp(-TAU * tone / (double)SR);   // crude noise colour
    const double a = atk > 0 ? atk : 1e-6;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double env = (t < a) ? (t / a) : exp(-(t - a) / dec);
        if (t > a + 0.0005 && env < 1e-5) break;
        double osc;
        if (wave == PW_NOISE) {
            s ^= s << 13; s ^= s >> 17; s ^= s << 5;
            const double w = ((double)s / 4294967296.0) * 2.0 - 1.0;
            lp = lp * lpc + (1.0 - lpc) * w;
            osc = lp * 2.0;
        } else {
            phase += dP; if (phase >= 1.0) phase -= 1.0;
            if (wave == PW_SINE)      osc = sin(TAU * phase);
            else if (wave == PW_TRI)  osc = 2.0 * fabs(2.0 * (phase - floor(phase + 0.5))) - 1.0;
            else                      osc = (phase < 0.5) ? 1.0 : -1.0;   // square
        }
        const double smp = osc * env * vol;
        const double sL = smp * (1.0 - 0.5 * pan);
        const double sR = smp * (1.0 + 0.5 * pan);
        L[i] += sL; R[i] += sR;
        WL[i] += sL * wet; WR[i] += sR * wet;
    }
}
// pf = pitch factor (1.0 = stock); the placement passes wild values so the
// hits land on weird, characterful pitches.
static void woodblock(double t0, double v, double pan, double pf) {   // g# — wood block
    perc_hit(t0, PW_NOISE, 5000 * pf, 0.002, 0.35 * v, 0.0001, 0.0018, pan, 0.0);
    perc_hit(t0, PW_TRI,   2500 * pf, 0.060, 0.52 * v, 0.0003, 0.048,  pan, 0.12);
    perc_hit(t0, PW_TRI,   1250 * pf, 0.060, 0.18 * v, 0.0005, 0.048,  pan, 0.12);
}
static void cowbell(double t0, double v, double pan, double pf) {     // f# — TR-808 cowbell
    perc_hit(t0, PW_SQR, 1800 * pf, 0.004, 0.35 * v, 0.0002, 0.0038, pan, 0.0);
    perc_hit(t0, PW_TRI,  800 * pf, 0.300, 0.42 * v, 0.0008, 0.275,  pan, 0.15);
    perc_hit(t0, PW_TRI,  540 * pf, 0.300, 0.36 * v, 0.0008, 0.275,  pan, 0.15);
}
static void tambo(double t0, double v, double pan, double pf) {       // a# — tambourine
    perc_hit(t0, PW_NOISE, 7000 * pf, 0.080, 0.38 * v, 0.002, 0.075, pan, 0.18);
    perc_hit(t0, PW_NOISE, 6500 * pf, 0.100, 0.25 * v, 0.030, 0.070, pan, 0.18);
    perc_hit(t0, PW_SQR,   6000 * pf, 0.030, 0.14 * v, 0.001, 0.028, pan, 0.10);
}
// TR-808 snare — two tuned body tones (~180 + ~330 Hz) + a snappy noise
// burst with its own longer decay. The 808 "tssh".
static void snare808(double t0, double v, double pan) {
    perc_hit(t0, PW_SINE,  180, 0.20, 0.55 * v, 0.0005, 0.17, pan, 0.10);
    perc_hit(t0, PW_SINE,  330, 0.14, 0.32 * v, 0.0005, 0.11, pan, 0.10);
    perc_hit(t0, PW_NOISE, 3800, 0.22, 0.70 * v, 0.0004, 0.18, pan, 0.14);
}

// ── snare voice (filtered noise burst + low tone — the "BIZZ") ───────
typedef struct {
    double pan;
    double gain;
    double decay_s;
    double brightness;
} SnareOpts;
static void snare_render(double t0, SnareOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.10;
    const double max_dur = decay * 6.0;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.88;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    double tonePhase = 0.0;
    const double toneFreq = 240.0;
    uint32_t s = (uint32_t)(t0 * 1000003.0) | 1;
    double hp_in_prev = 0.0, hp_out_prev = 0.0;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env;
        if (t < 0.001) env = t / 0.001;
        else env = exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp_out = bright * (hp_out_prev + noise - hp_in_prev);
        hp_in_prev = noise; hp_out_prev = hp_out;
        const double dPhs = toneFreq / (double)SR;
        tonePhase += dPhs; if (tonePhase >= 1.0) tonePhase -= 1.0;
        const double tone = sin(TAU * tonePhase);
        const double sample = (0.75 * hp_out + 0.25 * tone) * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── ocean crash voice (pink-noise wash with slow attack + long tail) ─
// Replaces the clap backbeat per @jeffrey 2026-05-29. Wide-band pink
// noise (cascade of LP filters approximating 1/f spectrum) with a
// 120 ms attack swell and 1.6 s exponential decay. Stereo width via
// two independent noise streams panned hard L/R. Subtle 0.3 Hz LFO
// on the envelope adds wave-like surge.
typedef struct {
    double pan;       // ignored — crash is always wide stereo
    double gain;
    double attack_s;
    double decay_s;
    double surge_rate; // LFO Hz for wave surge (default 0.35)
    double tilt;       // -1 = darker / lower-mid heavy; +1 = brighter / hissier
    uint32_t seed;     // varies the noise pattern between crashes
} OceanOpts;
static void ocean_crash_render(double t0, OceanOpts opt) {
    const double atk = opt.attack_s > 0 ? opt.attack_s : 0.12;
    const double dec = opt.decay_s > 0 ? opt.decay_s : 1.6;
    const double surge_hz = opt.surge_rate > 0 ? opt.surge_rate : 0.35;
    const double tilt = opt.tilt;     // -1..+1
    const double max_dur = atk + dec * 5.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t baseSeed = opt.seed ? opt.seed : (uint32_t)(t0 * 33391.0 + 19);
    uint32_t sL = baseSeed | 1;
    uint32_t sR = (baseSeed * 2654435761u + 17) | 1;
    double lpL1 = 0, lpL2 = 0, lpL3 = 0;
    double lpR1 = 0, lpR2 = 0, lpR3 = 0;
    // Tilt the cascade weights: negative tilt → emphasize slow band
    // (more body/low rumble), positive tilt → emphasize fast band
    // (more hiss/spray).
    const double w1 = 2.0 - tilt * 0.8;   // slow / dark band
    const double w2 = 1.0;
    const double w3 = 0.5 + tilt * 0.8;   // fast / bright band
    const double w_sum = w1 + w2 + w3;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env;
        if (t < atk) env = t / atk;
        else         env = exp(-(t - atk) / dec);
        if (t > 0.02 && env < 1e-5) break;
        const double surge = 0.78 + 0.22 * sin(TAU * surge_hz * t);
        sL ^= sL << 13; sL ^= sL >> 17; sL ^= sL << 5;
        sR ^= sR << 13; sR ^= sR >> 17; sR ^= sR << 5;
        const double nL = ((double)sL / 4294967296.0) * 2.0 - 1.0;
        const double nR = ((double)sR / 4294967296.0) * 2.0 - 1.0;
        lpL1 = lpL1 * 0.93 + nL * 0.07;
        lpL2 = lpL2 * 0.78 + nL * 0.22;
        lpL3 = lpL3 * 0.55 + nL * 0.45;
        lpR1 = lpR1 * 0.93 + nR * 0.07;
        lpR2 = lpR2 * 0.78 + nR * 0.22;
        lpR3 = lpR3 * 0.55 + nR * 0.45;
        const double pinkL = (lpL1 * w1 + lpL2 * w2 + lpL3 * w3) / w_sum;
        const double pinkR = (lpR1 * w1 + lpR2 * w2 + lpR3 * w3) / w_sum;
        const double sampleL = pinkL * env * surge * opt.gain;
        const double sampleR = pinkR * env * surge * opt.gain;
        L[i] += sampleL;
        R[i] += sampleR;
    }
}

// ── cymbal voice (filtered noise + inharmonic high partials) ─────────
// Replaces the reverse_kick "crash" at section endpoints — much
// cleaner than the noise-sweep, which sounded crunchy. Real-cymbal
// model: bandpass-shaped white noise with a sharp attack + slow decay,
// plus 4 inharmonic sine partials at typical cymbal mode frequencies
// for the metallic ring on top. @jeffrey 2026-05-29.
typedef struct {
    double pan;
    double gain;
    double decay_s;   // long tail (1.2-2.5 s)
    double brightness;
} CymbalOpts;
static void cymbal_render(double t0, CymbalOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 1.6;
    const double max_dur = decay * 4.5;
    const double bright = opt.brightness > 0 ? opt.brightness : 0.93;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(t0 * 1009003.0) | 1;
    double hp1_in = 0.0, hp1_out = 0.0;
    double hp2_in = 0.0, hp2_out = 0.0;
    // Four inharmonic partials in the cymbal ring range. Ratios
    // mimic the inharmonic modes of a thin metal disk.
    const double partials[4] = { 4187.0, 5872.0, 7349.0, 9520.0 };
    double phs[4] = { 0.0, 0.0, 0.0, 0.0 };
    const double dphs[4] = {
        partials[0] / SR, partials[1] / SR,
        partials[2] / SR, partials[3] / SR
    };
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        // Sharp attack (3 ms) then exponential decay
        double env;
        if (t < 0.003) env = t / 0.003;
        else env = exp(-(t - 0.003) / decay);
        if (t > 0.01 && env < 1e-6) break;
        // Filtered noise — two cascaded highpass stages for cymbal hiss
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp1 = bright * (hp1_out + noise - hp1_in);
        hp1_in = noise; hp1_out = hp1;
        const double hp2 = bright * (hp2_out + hp1 - hp2_in);
        hp2_in = hp1; hp2_out = hp2;
        // Partial sum — each partial has its own slower envelope
        double partialSum = 0.0;
        for (int p = 0; p < 4; p++) {
            phs[p] += dphs[p];
            if (phs[p] >= 1.0) phs[p] -= 1.0;
            // Each partial decays at a slightly different rate
            const double pEnv = exp(-(t) / (decay * (1.2 + p * 0.15)));
            partialSum += sin(TAU * phs[p]) * pEnv * (0.18 - p * 0.025);
        }
        const double sample = (0.55 * hp2 + 0.45 * partialSum) * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── triangle voice (true triangle wave, low harmonic backing) ────────
typedef struct {
    double atk, rel;
    double pan;
    double gain;
    double wet_send;
} TriOpts;
static inline double tri_wave(double phase) {
    if (phase < 0.25)      return phase * 4.0;
    else if (phase < 0.75) return 2.0 - phase * 4.0;
    else                   return phase * 4.0 - 4.0;
}
static void triangle_render(double t0, double dur, double midi, TriOpts opt) {
    const double f = m2f(midi);
    const double atk = opt.atk > 0 ? opt.atk : 0.50;
    const double rel = opt.rel > 0 ? opt.rel : 0.80;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    double phase = 0;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        const double tRem = dur - t;
        double env;
        if (t < atk)         env = t / atk;
        else if (tRem < rel) env = tRem / rel;
        else                 env = 1.0;
        if (env < 0) env = 0;
        const double dPhs = f / (double)SR;
        phase += dPhs; if (phase >= 1.0) phase -= 1.0;
        const double sample = tri_wave(phase) * env * opt.gain;
        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── shaker (band-passed noise) ───────────────────────────────────────
typedef struct {
    double pan;
    double gain;
    double decay_s;
} ShakerOpts;
static void shaker_render(double t0, ShakerOpts opt) {
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.06;
    const double max_dur = decay * 6.0;
    long iStart = (long)(t0 * SR);
    long iEnd = iStart + (long)(max_dur * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(t0 * 5381.0 + 17) | 1;
    double hp_in = 0, hp_out = 0;
    double lp_out = 0;
    const double hp_a = 0.92;
    const double lp_a = 0.55;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.002) ? (t / 0.002) : exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp = hp_a * (hp_out + noise - hp_in);
        hp_in = noise; hp_out = hp;
        lp_out = lp_out * (1.0 - lp_a) + hp * lp_a;
        const double sample = lp_out * env * opt.gain;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── clap (multi-burst noise: 3 quick noise hits then tail) ───────────
typedef struct {
    double pan;
    double gain;
    double decay_s;
} ClapOpts;
static void clap_render(double t0, ClapOpts opt) {
    // TIGHTER per @jeffrey 2026-05-29 ("white noise crunch at end of
    // every bar"). 3 bursts → 2; default decay 0.18 → 0.04 (just a
    // snap, no long noise tail).
    const double bursts = 2;
    const double burstGap = 0.008;
    const double decay = opt.decay_s > 0 ? opt.decay_s : 0.04;
    // 3 short bursts then long tail
    for (int b = 0; b < bursts; b++) {
        const double tb = t0 + b * burstGap;
        long iStart = (long)(tb * SR);
        long iEnd = iStart + (long)(0.012 * SR);
        if (iStart < 0) iStart = 0;
        if (iEnd > N) iEnd = N;
        uint32_t s = (uint32_t)(tb * 9973.0 + b * 23) | 1;
        for (long i = iStart; i < iEnd; i++) {
            const double t = (i - iStart) / (double)SR;
            double env = (t < 0.001) ? (t / 0.001) : exp(-(t - 0.001) / 0.012);
            if (t > 0.005 && env < 1e-5) break;
            s ^= s << 13; s ^= s >> 17; s ^= s << 5;
            const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
            const double sample = noise * env * opt.gain * 0.65;
            L[i] += sample * (1.0 - 0.5 * opt.pan);
            R[i] += sample * (1.0 + 0.5 * opt.pan);
        }
    }
    // Tail burst
    const double tt = t0 + bursts * burstGap;
    long iStart = (long)(tt * SR);
    long iEnd = iStart + (long)(decay * 4 * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    uint32_t s = (uint32_t)(tt * 8191.0 + 7) | 1;
    double hp_in = 0, hp_out = 0;
    const double hp_a = 0.90;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.001) ? (t / 0.001) : exp(-(t - 0.001) / decay);
        if (t > 0.005 && env < 1e-5) break;
        s ^= s << 13; s ^= s >> 17; s ^= s << 5;
        const double noise = ((double)s / 4294967296.0) * 2.0 - 1.0;
        const double hp = hp_a * (hp_out + noise);
        hp_out = hp;
        const double sample = hp * env * opt.gain * 1.05;
        L[i] += sample * (1.0 - 0.5 * opt.pan);
        R[i] += sample * (1.0 + 0.5 * opt.pan);
    }
}

// ── glockenspiel (atmospheric bell sparkle) ──────────────────────────
typedef struct {
    double decay_s;
    double pan;
    double gain;
    double wet_send;
    int    sidechain;   // 1 = duck to the backbeat (clap/"snare" on beats 2 & 4)
    double atk;         // attack seconds (0 = default 0.002)
    double vib_rate;    // vibrato Hz (0 = default ~5)
    double vib_depth;   // vibrato depth as fraction of pitch (0 = default wobble)
} GlockOpts;
// Backbeat sidechain gain at absolute time t: dips hard on beats 2 & 4
// (the clap that plays the snare role) and recovers — so ducked bells
// pump to the snare, not the kick.
static double backbeat_duck(double t) {
    const double spb = 60.0 / BPM;
    const double barlen = 4.0 * spb;
    const double pos = fmod(t, barlen);
    const double trigs[2] = { 1.0 * spb, 3.0 * spb };  // beats 2 and 4
    const double depth = 0.85, rel = 0.30;
    double g = 1.0;
    for (int z = 0; z < 2; z++) {
        double dt = pos - trigs[z];
        if (dt < 0) dt += barlen;                       // wrap: a trigger always precedes
        const double gz = 1.0 - depth * exp(-dt / rel);
        if (gz < g) g = gz;
    }
    return g;   // 0..1, dips toward 0 right on the backbeat
}
static void glock_render(double t0, double dur, double midi, GlockOpts opt) {
    const double f = m2f(midi);
    const double decay = opt.decay_s > 0 ? opt.decay_s : 1.5;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    const double atk = opt.atk > 0 ? opt.atk : 0.002;
    // WUB — a tempo-synced LFO. Like a dubstep wobble (resonant LP cutoff
    // swept by a synced LFO), but on an additive bell the equivalent is to
    // sweep the upper partials (the 2.76× and 5.40× sines = "brightness")
    // — a filter wobble — plus a gentle pitch vibrato. Default rate is a
    // 1/8-note "wub-wub" locked to the track tempo (@jeffrey "wub wub").
    const double vibRate  = opt.vib_rate  > 0 ? opt.vib_rate  : (BPM / 60.0) * 2.0;
    const double vibDepth = opt.vib_depth > 0 ? opt.vib_depth : 0.010;
    double phs1 = 0, phs2 = 0, phs3 = 0;
    double vphase = 0.0;   // vibrato LFO phase (accumulated; rate varies over time)
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < atk) ? (t / atk) : exp(-(t - atk) / decay);
        if (t > 0.005 && env < 1e-6) break;
        const double ta   = (double)i / (double)SR;
        const double tRel = (dur > 0) ? (t / dur) : 0.0;  // 0 at the hit → 1 at the tail
        // backbeat sidechain envelope (1 open, →0 on the snare)
        const double g = opt.sidechain ? backbeat_duck(ta) : 1.0;
        // VIBRATO grows toward the tail (not at the hit), and the sidechain
        // bends it MORE — wobblier, "like the walls are bending" (@jeffrey).
        const double sc     = 1.0 - g;                    // 0 open, →1 on backbeat
        double vRate  = vibRate  * (0.40 + 1.60 * tRel) * (1.0 + sc * 0.7);
        double vDepth = vibDepth * (0.15 + 1.50 * tRel) * (1.0 + sc * 1.3);
        vphase += vRate / (double)SR; if (vphase >= 1.0) vphase -= 1.0;
        const double lfo = sin(TAU * vphase);
        const double wob = 0.5 + 0.5 * lfo;               // 0..1 brightness sweep
        const double vib = 1.0 + vDepth * lfo;            // pitch wobble
        const double dP1 = f * vib / (double)SR;
        const double dP2 = (f * 2.76 * vib) / (double)SR;
        const double dP3 = (f * 5.40 * vib) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;
        phs3 += dP3; if (phs3 >= 1.0) phs3 -= 1.0;
        // WUB filter wobble: sweep the upper partials with the LFO.
        double a1 = 0.80, a2 = 0.40 * (0.30 + 0.70 * wob), a3 = 0.20 * (0.06 + 0.94 * wob);
        // Backbeat sidechain — mostly a FILTER duck (the bell goes darker on
        // the snare) plus a gentle amplitude dip, not just a pumpy gate.
        double ampDuck = 1.0;
        if (opt.sidechain) {
            ampDuck = 0.60 + 0.40 * g;                       // gentle: dips to ~0.6
            a2 *= 0.25 + 0.75 * g;                           // 2nd partial dims when ducked
            a3 *= 0.05 + 0.95 * g;                           // brightest dims most → lowpass feel
        }
        const double sample = ampDuck * env * opt.gain *
                              (a1 * sin(TAU * phs1) +
                               a2 * sin(TAU * phs2) +
                               a3 * sin(TAU * phs3));
        const double sL = sample * (1.0 - 0.5 * pan);
        const double sR = sample * (1.0 + 0.5 * pan);
        L[i] += sL;
        R[i] += sR;
        WL[i] += sL * wetSend;
        WR[i] += sR * wetSend;
    }
}

// ── Schroeder reverb (long deep-house cathedral) ─────────────────────
// Wetter feedback (0.88) + longer comb delays for sustain.
typedef struct { float *buf; long size; long idx; float feedback; } Comb;
typedef struct { float *buf; long size; long idx; } Allpass;
static void comb_init(Comb *c, long size, float fb) {
    c->buf = calloc(size, sizeof(float));
    c->size = size; c->idx = 0; c->feedback = fb;
}
static inline float comb_tick(Comb *c, float in) {
    float out = c->buf[c->idx];
    c->buf[c->idx] = in + out * c->feedback;
    c->idx++; if (c->idx >= c->size) c->idx = 0;
    return out;
}
static void allpass_init(Allpass *a, long size) {
    a->buf = calloc(size, sizeof(float));
    a->size = size; a->idx = 0;
}
static inline float allpass_tick(Allpass *a, float in) {
    float bufout = a->buf[a->idx];
    float out = -in + bufout;
    a->buf[a->idx] = in + bufout * 0.5f;
    a->idx++; if (a->idx >= a->size) a->idx = 0;
    return out;
}
static void apply_reverb(double wet_mix) {
    static const int comb_dl_L[4] = { 1789, 1717, 2243, 2459 };
    static const int comb_dl_R[4] = { 1843, 1771, 2297, 2503 };
    const double sr_scale = (double)SR / 44100.0;
    Comb cL[4], cR[4];
    for (int i = 0; i < 4; i++) {
        comb_init(&cL[i], (long)(comb_dl_L[i] * sr_scale), 0.88f);
        comb_init(&cR[i], (long)(comb_dl_R[i] * sr_scale), 0.88f);
    }
    Allpass apL[2], apR[2];
    allpass_init(&apL[0], (long)(572 * sr_scale));
    allpass_init(&apL[1], (long)(449 * sr_scale));
    allpass_init(&apR[0], (long)(619 * sr_scale));
    allpass_init(&apR[1], (long)(493 * sr_scale));
    for (long i = 0; i < N; i++) {
        float inL = WL[i], inR = WR[i];
        float sumL = 0, sumR = 0;
        for (int k = 0; k < 4; k++) {
            sumL += comb_tick(&cL[k], inL);
            sumR += comb_tick(&cR[k], inR);
        }
        sumL = allpass_tick(&apL[0], sumL);
        sumL = allpass_tick(&apL[1], sumL);
        sumR = allpass_tick(&apR[0], sumR);
        sumR = allpass_tick(&apR[1], sumR);
        L[i] += sumL * wet_mix;
        R[i] += sumR * wet_mix;
    }
    for (int i = 0; i < 4; i++) { free(cL[i].buf); free(cR[i].buf); }
    for (int i = 0; i < 2; i++) { free(apL[i].buf); free(apR[i].buf); }
}

// ── SCORE: 4-bar chord rotation I-IV-I-V in G major ─────────────────
typedef struct {
    int bass;
    int root, third, fifth;
    const char *name;
} Chord;
// G major progression — slow 4-bar changes. Populated per-render by
// init_prog() from the diatonic palette below, so the bells, powersaw,
// pads, organ and stabs (all of which read chord_for_bar→PROG) pitch to
// a fresh progression every playback.
static Chord PROG[4] = {
    { 31, 43, 47, 50, "G" },   // defaults (overwritten by init_prog)
    { 36, 48, 52, 55, "C" },
    { 31, 43, 47, 50, "G" },
    { 26, 38, 42, 45, "D" },
};
// Diatonic chords of G major (each: bass, root, third, fifth triad).
static const Chord PALETTE[] = {
    { 31, 43, 47, 50, "G"  },  // I
    { 33, 45, 48, 52, "Am" },  // ii   A C E
    { 35, 47, 50, 54, "Bm" },  // iii  B D F#
    { 36, 48, 52, 55, "C"  },  // IV   C E G
    { 26, 38, 42, 45, "D"  },  // V    D F# A  (low for sub feel)
    { 28, 40, 43, 47, "Em" },  // vi   E G B
};
// Build a fresh 4-bar progression. Slot 0 anchors the tonic (G) so the
// loop still feels grounded; slots 1-3 pick diatonic chords with no
// immediate repeats.
static void init_prog(void) {
    const int NP = (int)(sizeof(PALETTE) / sizeof(PALETTE[0]));
    PROG[0] = PALETTE[0];           // tonic anchor
    int prev = 0;
    for (int i = 1; i < 4; i++) {
        int pick;
        do { pick = rng_range(0, NP - 1); } while (pick == prev);
        PROG[i] = PALETTE[pick];
        prev = pick;
    }
}

// ── helpers for the dance pattern ────────────────────────────────────
// SECTIONS — return which chord index applies for a given bar position.
// Progression cycles every 4 bars.
static const Chord *chord_for_bar(int bar) {
    return &PROG[bar & 3];
}

// Linear 0→1 ramp between a and b — for slow, gradual section changes
// (e.g. the 3:15 "mix switch" where perc steps back + bells come forward).
static double ramp01(double t, double a, double b) {
    if (t <= a) return 0.0;
    if (t >= b) return 1.0;
    return (t - a) / (b - a);
}

// ── render the full track ────────────────────────────────────────────
// Bar layout (4/4):
//   beat=0.5s, bar=2.0s.  Total bars = 120 for 240s.
//   INTRO    bars  0..15  (0..32 s)   pads only
//   BUILD    bars 16..31  (32..64s)   kick + shaker enter, vocal at 32
//   DROP 1   bars 32..47  (64..96s)   full kit + clap + chord stab
//   BREAK    bars 48..63  (96..128s)  pads only, vocal tail reverbs
//   DROP 2   bars 64..95  (128..192s) full kit + glock layer
//   OUTRO    bars 96..119 (192..240s) pad fade
static void render_track(void) {
    const double SPB = 60.0 / BPM;       // 0.5 s at 120 BPM
    const double BAR = 4.0 * SPB;         // 2.0 s in 4/4
    const int    TOTAL_BARS = (int)(TOTAL_SEC / BAR);

    report("amaythingra: bpm=%.1f, bar=%.3fs, total %.2fs (%d bars)",
           BPM, BAR, TOTAL_SEC, TOTAL_BARS);

    // ── 1 · PADS + ORGAN THROUGHOUT (every chord change) ─────────────
    // Two sine-pad layers + a chorded organ pad per chord. Sustained
    // through the 4-bar chord block with long crossfade overlap.
    // Skip bar 0 (the bell intro) so it's JUST the field-recorded bells
    // at the very start of the track — pad/organ enter at bar 4 (8 s).
    for (int bar = 4; bar < TOTAL_BARS; bar++) {
        if ((bar & 3) != 0) continue;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        const double dur = 4.0 * BAR + 0.5;

        // Low sine pad
        sine_pad_render(t0, dur,
                        c->root - 12, c->third - 12, c->fifth - 12,
                        (SinePadOpts){ .atk = 1.4, .rel = 1.5,
                                       .pan = 0.0, .gain = 0.08,
                                       .wet_send = 0.55,
                                       .lfo_rate = 0.13, .lfo_depth = 0.22 });
        // Mid sine pad
        sine_pad_render(t0, dur,
                        c->root, c->third, c->fifth,
                        (SinePadOpts){ .atk = 1.8, .rel = 2.0,
                                       .pan = 0.0, .gain = 0.05,
                                       .wet_send = 0.60,
                                       .lfo_rate = 0.10, .lfo_depth = 0.25 });

        // Organ — full triad chord at root octave. Three calls stacked
        // (root + third + fifth) with slight stereo spread. Slow attack
        // so it breathes with the chord change, sustained throughout.
        organ_render(t0, dur, c->root,
            (OrganOpts){ .atk = 0.30, .rel = 0.50,
                         .vib_rate = 4.5, .vib_depth = 6.0,
                         .pan = -0.18, .gain = 0.10,
                         .wet_send = 0.35,
                         .sub_amt = 0.7, .upper_amt = 0.45 });
        organ_render(t0, dur, c->third,
            (OrganOpts){ .atk = 0.35, .rel = 0.50,
                         .vib_rate = 4.8, .vib_depth = 7.0,
                         .pan = 0.0, .gain = 0.08,
                         .wet_send = 0.35,
                         .sub_amt = 0.6, .upper_amt = 0.45 });
        organ_render(t0, dur, c->fifth,
            (OrganOpts){ .atk = 0.40, .rel = 0.50,
                         .vib_rate = 5.0, .vib_depth = 7.0,
                         .pan = +0.18, .gain = 0.08,
                         .wet_send = 0.35,
                         .sub_amt = 0.6, .upper_amt = 0.45 });
    }
    report("  pads + organ laid down (every 4 bars)");

    // ── 1.45 · LONG INTRO HIGH BELLS (~16-24 s) ──────────────────────
    // @jeffrey: "the high sine bells around 17 with muuuch longer decay,
    // rather than the little tiny things they are now." A pair of high sine
    // bells struck at ~16 s and ~24 s with a HUGE 38 s decay, so they ring
    // in under the opening filter sweep and bloom all the way through the
    // first WOOP — long sustained bells, not short plinks.
    {
        const Chord *ca = chord_for_bar(8);    // chord at ~16 s
        glock_render(16.0, 42.0, ca->fifth + 12,
            (GlockOpts){ .decay_s = 38.0, .pan = -0.25, .gain = 0.090, .wet_send = 0.72 });
        const Chord *cb = chord_for_bar(12);   // ~24 s
        glock_render(24.0, 42.0, cb->root + 12,
            (GlockOpts){ .decay_s = 38.0, .pan = 0.25, .gain = 0.090, .wet_send = 0.72 });
        report("  long intro high bells @16 s + @24 s (38 s decay)");
    }

    // ── 1.5 · TRIANGLE LOW + SINE BELL HIGH (vocal harmony backing) ──
    // Delayed to bar 12 (t=24 s) along with chord stab + powersaw —
    // before bar 12 the build stays gentle, just bed + drums + bells.
    // No big sine bells before the 32 s WOOP — they enter at bar 16
    // with the powersaws (@jeffrey "first bells at 32 s, don't play any of
    // the big sine church bells before that").
    for (int bar = 16; bar < 64; bar += 4) {
        if (bar >= TOTAL_BARS) break;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        const double dur = 4.0 * BAR + 0.5;
        // LOW triangle — pitch narrative just like the high bells:
        // root → third → fifth → root as the 4-bar cycles progress
        // (@jeffrey 2026-05-29 "lower pitched sine bells too should
        // change over time").
        const int lo_variant = ((bar - 16) / 4) % 4;
        int lo_a, lo_b;
        switch (lo_variant) {
            case 0: lo_a = c->root  - 12; lo_b = c->fifth - 12; break;
            case 1: lo_a = c->third - 12; lo_b = c->root  - 12; break;
            case 2: lo_a = c->fifth - 12; lo_b = c->third - 12; break;
            default:lo_a = c->root  - 12; lo_b = c->third - 12; break;
        }
        triangle_render(t0, dur, lo_a,
            (TriOpts){ .atk = 0.40, .rel = 0.70,
                       .pan = 0.0, .gain = 0.10, .wet_send = 0.32 });
        triangle_render(t0, dur, lo_b,
            (TriOpts){ .atk = 0.50, .rel = 0.70,
                       .pan = -0.15, .gain = 0.06, .wet_send = 0.32 });
        // HIGH sine bells — SEEDED melodic sparkle. Each 4-bar block
        // draws a fresh set of 2-4 bells from a 3-octave pool of chord
        // tones, with varied pitch, timing, decay, gain and pan, so the
        // sparkle never settles into a 4-cycle loop (@jeffrey "bells need
        // more variation still"). Differs every render.
        // kept in the mid octave — the +19/+24 tiers were too high/cringy.
        const int hipool[] = { c->root, c->third, c->fifth,
                               c->root + 12, c->third + 12, c->fifth + 12 };
        const int nhi = (int)(sizeof(hipool) / sizeof(hipool[0]));
        if (bar >= 36) {
            // From ~1:14 (@jeffrey): the high bells become a RHYTHMIC,
            // spaced, long-ringing line — exactly ONE bell per 4-bar block,
            // locked to the DOWNBEAT, stepping through chord tones and
            // ringing 24-34 s so it carries through the minute. Sides
            // alternate. Spaced + rhythmic + long, not random sparkle.
            // MORE SPREAD OUT (@jeffrey): one bell every OTHER block (every
            // 8 bars ≈ 16 s) instead of every 4 — long tails bridge the gaps.
            const int block = (bar - 36) / 4;
            if ((block & 1) == 0) {
                const int seq[4] = { c->fifth + 12, c->root + 12, c->third + 12, c->root + 12 };
                const int off = seq[(block / 2) & 3];
                const double dec = 26.0 + rng_d() * 10.0;
                const double pan = ((block / 2) & 1) ? 0.30 : -0.30;
                glock_render(t0 + humanize(t0, 10, 3), dec + 1.0, off,
                    (GlockOpts){ .decay_s = dec, .pan = pan,
                                 .gain = 0.052, .wet_send = 0.66, .sidechain = 1 });
            }
        } else {
            // Before ~1:14: sparse random sparkle (1 per block, ~25% skip).
            const int nbells = (rng_d() < 0.25) ? 0 : 1;
            for (int b = 0; b < nbells; b++) {
                const int off  = hipool[rng_range(0, nhi - 1)];
                const double at = t0 + rng_range(0, 7) * (BAR / 4.0) + humanize(t0 + b, 16, 4);
                const double dec = 16.0 + rng_d() * 14.0;
                const double pan = (rng_d() - 0.5) * 0.6;
                const double gain = 0.035 + rng_d() * 0.030;
                glock_render(at, dec + 1.0, off,
                    (GlockOpts){ .decay_s = dec, .pan = pan,
                                 .gain = gain, .wet_send = 0.60, .sidechain = 1 });
            }
        }
    }
    report("  triangle low + sine bell high harmony backing (bars 4..63)");

    // ── 1.6 · SUSTAINED VOCAL-PITCH SINE BRIDGES ─────────────────────
    // "stick a sine in between em" — sustained pure sine tones at each
    // syllable's pitch, filling the gaps between AH / MAY / ZING / GRACE
    // so the vocal line reads as one continuous sung phrase instead of
    // a sequence of held vowels with airy silences. Tracks the melody:
    //   ah    D3 (50) at 0..27.5 s
    //   may   G3 (55) at 27.5..31.5 s
    //   zing  B3 (59) at 32..38   s
    //   grace G3 (55) at 38.5..53.8 s
    // Slow fade-in/out + soft amp envelope on each so they sit under,
    // not on top of, the held vowels.
    {
        const struct {
            double t0; double dur; int midi;
            double gain; double wet_send;
        } bridges[] = {
            //  t0     dur   midi   gain   wet
            {   0.0,  28.0,   50,  0.08,  0.40 },  // ah   D3
            {  27.0,   5.0,   55,  0.14,  0.35 },  // may  G3
            {  31.5,   5.0,   59,  0.18,  0.35 },  // zing B3 (4 s + tail)
            {  35.5,   6.0,   55,  0.14,  0.40 },  // grace G3
            {  39.5,   3.0,   59,  0.14,  0.35 },  // how  B3
            {  41.5,   5.0,   57,  0.14,  0.35 },  // sweet A3
            {  45.5,   3.0,   55,  0.12,  0.35 },  // the  G3
            {  47.5,  12.0,   59,  0.18,  0.40 },  // sound B3
        };
        const int n = sizeof(bridges) / sizeof(bridges[0]);
        for (int k = 0; k < n; k++) {
            // First bridge (k=0, the "ah" at t=0) drops its high-octave
            // partial — @jeffrey 2026-05-29 called it the "high pitch
            // first synth sample sound" and wanted it removed. Keep
            // root + 5th only for that one.
            int midiHigh = (k == 0) ? bridges[k].midi + 7 : bridges[k].midi + 12;
            sine_pad_render(bridges[k].t0, bridges[k].dur,
                            bridges[k].midi, bridges[k].midi + 7, midiHigh,
                            (SinePadOpts){ .atk = 1.5, .rel = 1.5,
                                           .pan = 0.0,
                                           .gain = bridges[k].gain,
                                           .wet_send = bridges[k].wet_send,
                                           .lfo_rate = 0.18,
                                           .lfo_depth = 0.18 });
        }
        report("  sustained sine bridges under ah/may/zing/grace");
    }

    // ── 2 · DRUMS ─────────────────────────────────────────────────────
    // Beat right off the bat (bar 0) — no intro section. Break at bars
    // 48..63 (96..128s) drops kick + clap. Outro at bars 104..119 drops
    // kick for the final filter fade. Hats + shaker run THROUGHOUT.
    for (int bar = 0; bar < TOTAL_BARS; bar++) {
        const double t0 = bar * BAR;
        // 3:15 MIX SWITCH (@jeffrey "nice gradual changes / mix switch"):
        // percussion gradually steps back over ~195-225 s so the bell
        // layers sit on top. percMult eases 1.0 → ~0.45.
        // …and then the perc DIES DOWN entirely over ~250-268 s so the
        // track winds down musically to just bells/pads before the ~4:34
        // resolve (@jeffrey "die it down musically and end around 4:33").
        double percMult = (1.0 - 0.55 * ramp01(t0, 195.0, 225.0));   // 3:15 mix-switch
        percMult *= (1.0 - 0.80 * ramp01(t0, 214.0, 230.0));         // final ~60s: perc MUCH quieter (@jeffrey)
        percMult *= (1.0 - ramp01(t0, 255.0, 268.0));                // wind-down to silence
        // 2:45-3:00 KICK ARPEGGIO (@jeffrey "kicks pitch up to arpeggiate
        // scales") — first step of evolving the perc into melodies. In bars
        // 82..89 the kick's pitch climbs a G-pentatonic run, so the tonal
        // kicks spell out a scale. kickArp multiplies the kick pitch sweep.
        double kickArp = 1.0;
        if (bar >= 82 && bar < 90) {
            static const int PENT[8] = { 0, 2, 4, 7, 9, 7, 4, 2 };  // G A B D E … (semitones)
            kickArp = pow(2.0, PENT[(bar - 82) & 7] / 12.0);
        }
        // First 3 bars (0..6 s) = soft church-bell intro (no kit), then
        // the beat enters from bar 3 onwards. Break drops kick at the
        // bars 48..63 mid-track break, outro drops it at bar 104+.
        // No empty intro — the kick drives from the FIRST measure
        // (@jeffrey "sounds weird with no kick"). The intro instead opens
        // under a lowpass filter sweep (see write_wav), so it starts dark
        // and low and brightens by ~32 s.
        const int sec_intro = 0;
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= TOTAL_BARS - 2;   // full groove to the end
        const int kick_on   = !sec_intro && !sec_break && !sec_outro;
        const int clap_on   = !sec_intro && !sec_break && !sec_outro;
        const int hat_on    = !sec_intro;
        const int shaker_on = !sec_intro;

        // INTRO PERCUSSION (bars 0-2) — full drum kit playing super
        // quiet + "behind the wall" character: muffled kick (no click),
        // dim hats (low brightness, low gain), dim claps (low gain),
        // dim shakers. The global 32 s fade then sweeps them up.
        if (sec_intro) {
            // Muffled kick — gain dropped 3.00 → 0.55 + skip bar 0
            // entirely so t=0 is truly silent for the bed
            // (@jeffrey 2026-05-29 "very loud intro sound right at 00").
            if (bar >= 1) {
                for (int beat = 0; beat < 4; beat++) {
                    kick_render(t0 + beat * SPB,
                        (KickOpts){ .pan = 0.0, .gain = 0.55,
                                    .pitch_hi = 95, .pitch_lo = 55,
                                    .pitch_dur = 0.05, .decay_s = 0.24,
                                    .muffled = 1 });
                }
            }
            // Quiet "behind-wall" hats on offbeats — low brightness =
            // less highpass = more dampened sound; gain 0.04.
            for (int beat = 0; beat < 4; beat++) {
                const double th = t0 + (beat + 0.5) * SPB;
                hat_render(th + humanize(th, 12, 5),
                    (HatOpts){ .pan = (beat % 2 == 0 ? -0.18 : +0.18),
                               .gain = 0.04, .decay_s = 0.035,
                               .brightness = 0.45,           // dampened
                               .double_delay_ms = 4.0 });
            }
            // One quiet intro clap per bar on beat 2 (matches normal pattern).
            const double clap_pan = (bar & 1) ? -0.42 : +0.42;
            clap_render(t0 + 1 * SPB + humanize(t0 + 1, 18, 8),
                (ClapOpts){ .pan = clap_pan, .gain = 0.04, .decay_s = 0.05 });
            // Quiet shakers — let the global fade do the rest
            for (int eighth = 0; eighth < 8; eighth++) {
                const double base = t0 + eighth * (SPB * 0.5);
                shaker_render(base + humanize(base, 10, 4),
                    (ShakerOpts){ .pan = (eighth % 2 == 0 ? +0.16 : -0.16),
                                  .gain = 0.04, .decay_s = 0.055 });
            }
        }
        if (kick_on) {
            // YEEEOOOO kick, but with a gradual gain ramp instead of a
            // binary early/late step — felt "too loud too fast" otherwise.
            // Linear ramp from 1.45 (bar 3, kick entry) to 3.30 (bar 24,
            // ~10 bars after the drop). pitch/decay ramp alongside.
            // SOFTER + WIDER at first: gain starts at 0.85 (was 1.45);
            // wider pitch sweep (110→160 Hz) and Haas-pair stereo
            // spread on early/build bars that collapses to mono punch
            // when phase saturates to 1.
            const double phase = (bar >= 3)
                ? fmin(1.0, (double)(bar - 3) / 21.0)
                : 0.0;
            // Peak dropped 3.30 → 2.20 (was "too extreme" — @jeffrey 2026-05-29)
            const double kgain = (0.75 + (2.20 - 0.75) * phase) * percMult;
            const double kphi  = (110.0 + (160.0 - 110.0) * phase) * kickArp;
            const double kplo  = 56.0 * kickArp;
            const double kpdur = 0.11  + (0.13  - 0.11)  * phase;
            const double kdec  = 0.34  + (0.44  - 0.34)  * phase;
            for (int beat = 0; beat < 4; beat++) {
                // Lazy-human kick: ±8 ms with +3 ms positive bias.
                // Downbeats stay tight (less wiggle).
                const double tHit = t0 + beat * SPB +
                                    humanize(t0 + beat, beat == 0 ? 7 : 11, 3);
                // SUBTLE per-kick pitch variation — every kick wobbles a
                // little (±~4%) so the low end breathes (@jeffrey "kick
                // pitch variably on every kick, just in a subtle way").
                uint32_t kr = (uint32_t)((bar * 4 + beat) * 374761393u) | 1;
                kr ^= kr << 13; kr ^= kr >> 17; kr ^= kr << 5;
                const double kpf  = 1.0 + ((double)kr / 4294967296.0 - 0.5) * 0.08;
                const double kphi2 = kphi * kpf;
                const double kplo2 = kplo * kpf;
                if (phase < 0.6 && !MONO_KICK) {
                    // HAAS pair widens the kick image at low energy
                    const double haasPan = 0.35 * (1.0 - phase / 0.6);
                    const double sideGain = kgain * 0.55;
                    const double centerGain = kgain * 0.85;
                    kick_render(tHit,
                        (KickOpts){ .pan = -haasPan, .gain = sideGain,
                                    .pitch_hi = kphi2, .pitch_lo = kplo2,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                    kick_render(tHit + 0.003,
                        (KickOpts){ .pan = +haasPan, .gain = sideGain,
                                    .pitch_hi = kphi2, .pitch_lo = kplo2,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                    kick_render(tHit,
                        (KickOpts){ .pan = 0.0, .gain = centerGain,
                                    .pitch_hi = kphi2, .pitch_lo = kplo2,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                } else if (phase < 0.6) {
                    // MONO_KICK: single dead-center kick — no HAAS phase smear.
                    kick_render(tHit,
                        (KickOpts){ .pan = 0.0, .gain = kgain * 0.95,
                                    .pitch_hi = kphi2, .pitch_lo = kplo2,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                } else {
                    kick_render(tHit,
                        (KickOpts){ .pan = 0.0, .gain = kgain,
                                    .pitch_hi = kphi2, .pitch_lo = kplo2,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                }
            }
            // HIGHER HEART-RATE (@jeffrey "speed up the kick frequency so
            // it's much more like a higher heart rate"): an extra kick on
            // every off-beat 8th, doubling the pulse into a driving
            // heartbeat. Softer than the downbeats so it propels without
            // becoming a wall; inherits the kick-arp pitch + percMult.
            for (int beat = 0; beat < 4; beat++) {
                const double tOff = t0 + (beat + 0.5) * SPB + humanize(t0 + beat + 0.37, 8, 2);
                uint32_t orr = (uint32_t)((bar * 4 + beat) * 2246822519u) | 1;
                orr ^= orr << 13; orr ^= orr >> 17; orr ^= orr << 5;
                const double opf = 1.0 + ((double)orr / 4294967296.0 - 0.5) * 0.06;
                kick_render(tOff,
                    (KickOpts){ .pan = 0.0, .gain = kgain * 0.5,
                                .pitch_hi = kphi * opf * 0.97, .pitch_lo = kplo * opf,
                                .pitch_dur = kpdur, .decay_s = kdec * 0.8 });
            }
            // REVERSE KICK — SPRINKLED, never earlier than ~30 s
            // (@jeffrey 2026-05-29). Picks 6 strategic bar boundaries
            // through the rest of the track instead of every 4 bars.
            // Each is a 2-beat rising sweep building into the next bar.
            const int rev_bars[] = { 23, 79, 95 };   // dropped 39 & 47 (1:18/1:34 "big crashes" — too dramatic, @jeffrey)
            const int n_rev = sizeof(rev_bars) / sizeof(rev_bars[0]);
            for (int q = 0; q < n_rev; q++) {
                if (bar == rev_bars[q]) {
                    // Bigger sweeps on the major transition bars
                    const int big = (bar == 15 || bar == 47 || bar == 79);
                    reverse_kick_render(t0 + (big ? 0 : 2 * SPB),
                        (ReverseKickOpts){ .pan = 0.0,
                                           .gain = big ? 2.0 : 1.6,
                                           .pitch_lo = big ? 50 : 60,
                                           .pitch_hi = big ? 260 : 220,
                                           .dur = big ? 4 * SPB : 2 * SPB });
                    break;
                }
            }
        }
        // Synth ocean crash removed entirely (@jeffrey 2026-05-29
        // "id rather they sound less affected more natural").
        // Real ocean recordings (CC0) layered in via bake-c-dance.mjs:
        // okeechobee-surf as continuous wave bed + seagulls-distant-
        // surf as sparser atmospheric overlay.
        if (hat_on) {
            // ON-GRID EXPRESSIVE HATS — a 16th-note ride locked to the
            // grid (NOTHING off-beat/swung). Each bar seeds a fresh pattern:
            // 8ths almost always play, 16th ghosts come and go, the
            // occasional 16th drops out — so it keeps "changing around but
            // stays regular, playing and expressive" (@jeffrey). Beats are
            // accented; a darker foot "chick" lands on 2 & 4. Only a few ms
            // of humanize for feel — still on the beat.
            const double g16 = SPB * 0.25;            // 16th grid step
            for (int s = 0; s < 16; s++) {
                const double tg = t0 + s * g16;
                if (tg >= HAT_SLOW_START && tg < HAT_SLOW_END) continue;
                const int onBeat   = (s % 4) == 0;    // quarter = kick
                const int onEighth = (s % 2) == 0;
                // seeded hit probability per (bar, step)
                uint32_t r = (uint32_t)((bar * 64 + s) * 2654435761u) | 1;
                r ^= r << 13; r ^= r >> 17; r ^= r << 5;
                const double rr = (double)r / 4294967296.0;
                const double prob = onBeat ? 0.96 : (onEighth ? 0.90 : 0.30);
                if (rr > prob) continue;              // dropout = variation
                uint32_t vr = r * 2246822519u + 1; vr ^= vr << 13; vr ^= vr >> 17;
                const double vrand = (double)vr / 4294967296.0;
                const double accent = onBeat ? 1.0 : (onEighth ? 0.80 : 0.50);
                const double vel = 0.16 * accent * (0.60 + 0.55 * vrand) * percMult;
                const double th = tg + humanize(tg, 15, 4);   // loose, laid-back feel
                hat_render(th,
                    (HatOpts){ .pan = (s % 2 == 0 ? -0.16 : +0.16),
                               .gain = vel, .decay_s = 0.018 + 0.012 * vrand,
                               .brightness = 0.86 + 0.12 * vrand,
                               .double_delay_ms = 3.0 });
                // foot hi-hat "chick" on 2 & 4 (s=4, s=12) — darker
                if (s == 4 || s == 12) {
                    hat_render(tg + humanize(tg, 12, 4),
                        (HatOpts){ .pan = 0.0, .gain = 0.13 * percMult, .decay_s = 0.030,
                                   .brightness = 0.45, .double_delay_ms = 5.0 });
                }
            }
            // HI-HAT RUSHES — only at bars 44 and 76 now. The bar-12
            // rush was landing exactly when MAY enters (@jeffrey flagged
            // "white noise crashing"); removed.
            const int rush_anchor_bars[] = { 79 };   // dropped bar-44 rush (the ~1:28-1:40 noise "crunch", @jeffrey)
            const int n_rush = sizeof(rush_anchor_bars) / sizeof(rush_anchor_bars[0]);
            for (int q = 0; q < n_rush; q++) {
                // Anchor bar minus 3 = start of slow rush
                if (bar == rush_anchor_bars[q] - 3) {
                    const double rush_start = t0;
                    const double rush_dur   = 4.0 * BAR;     // 8 s
                    const int rush_hits     = 64;            // dense
                    // OCEAN CRASH — even quieter now. Max gain dropped
                    // from 0.122 → 0.060 so the wash sits well under
                    // everything else.
                    for (int h = 0; h < rush_hits; h++) {
                        const double th = rush_start + (rush_dur * h / rush_hits);
                        const double phase = (double)h / rush_hits;
                        const double gain = 0.005 + 0.055 * phase * phase;
                        const double dec  = 0.15 + 0.30 * phase;
                        hat_render(th,
                            (HatOpts){ .pan = (h % 2 == 0 ? -0.30 : +0.30),
                                       .gain = gain, .decay_s = dec,
                                       .brightness = 0.40 + 0.20 * phase });
                    }
                    break;
                }
            }
        }
        if (shaker_on) {
            // Lazy-human shaker 8ths: ±10 ms with +4 ms late bias.
            for (int eighth = 0; eighth < 8; eighth++) {
                const double base = t0 + eighth * (SPB * 0.5);
                const double ts = base + humanize(base, 10, 4);
                const double accent = (eighth % 2 == 0) ? 0.06 : 0.04;   // less gushy / tighter (@jeffrey)
                shaker_render(ts,
                    (ShakerOpts){ .pan = (eighth % 2 == 0 ? +0.16 : -0.16),
                                  .gain = accent * percMult, .decay_s = 0.045 });
            }
        }
    }
    report("  drums patterned (beat enters at bar 3 after the bell intro)");

    // ── 2.6 · SINGLE COWBELL ─────────────────────────────────────────
    // Just one lone cowbell on the downbeat at ~24 s — no other perc
    // sprinkles for now (@jeffrey "just a single cowbell around 24 seconds,
    // on a beat, remove all the other ones").
    cowbell(12 * BAR, 0.22, -0.2, pow(2.0, 7.0 / 12.0));
    // The cowbell kicks off a LONG dramatic build — an 8 s rising sweep
    // ("eoeooooo") from 24 s climbing into the 32 s WOOP/drop, so there's
    // real tension there (@jeffrey "make it a much longer build, more
    // drama"). Quadratic envelope = quiet at first, swelling to the peak.
    reverse_kick_render(12 * BAR,
        (ReverseKickOpts){ .pan = 0.0, .gain = 2.1,
                           .pitch_lo = 42.0, .pitch_hi = 280.0,
                           .dur = 4.0 * BAR });   // 24 s → 32 s
    report("  single cowbell @ 24 s + 8 s build into the WOOP");

    // ── 2.7 · WOODBLOCK RATTLE MELODY ~1:15 ──────────────────────────
    // A fast, rattly woodblock riff (open-hat-ish) over ~2 bars from
    // ~1:15 — on the 16th grid, seeded gaps, pitched through a little
    // pentatonic melody, with a soft open hat shadowing it (@jeffrey).
    {
        const double SPB = 60.0 / BPM;
        const double g16 = SPB * 0.25;
        const int sem[] = { 0, 2, 4, 7, 9, 12, 14, 16 };   // G-pentatonic degrees
        const int nsem = (int)(sizeof(sem) / sizeof(sem[0]));
        for (int s = 0; s < 32; s++) {                      // 2 bars of 16ths
            const double t = 37 * BAR + s * g16;            // bar 37 ≈ 1:14
            if (rng_d() < 0.40) continue;                   // rattly gaps
            const double pf = pow(2.0, sem[rng_range(0, nsem - 1)] / 12.0) * 1.4;
            const int onB = (s % 4) == 0;
            woodblock(t, (onB ? 0.18 : 0.11) + rng_d() * 0.05, (rng_d() - 0.5) * 0.9, pf);
            // soft open-hat shimmer shadowing the rattle
            if (rng_d() < 0.5)
                hat_render(t, (HatOpts){ .pan = (rng_d() - 0.5) * 0.8,
                    .gain = 0.05 + rng_d() * 0.04, .decay_s = 0.06 + rng_d() * 0.10,
                    .brightness = 0.80, .double_delay_ms = 4.0 });
        }
        report("  woodblock rattle melody ~1:15");
    }

    // ── 2.8 · 808 SNARE FROM ~1:25 ───────────────────────────────────
    // An 808 snare enters on the backbeats (2 & 4) at bar 42 (~1:24) and
    // drives the second half (skips the break + final outro) (@jeffrey).
    {
        const double SPB = 60.0 / BPM;
        for (int bar = 42; bar < TOTAL_BARS - 2; bar++) {
            if (bar >= 48 && bar < 64) continue;            // break
            const double t0 = bar * BAR;
            snare808(t0 + 1.0 * SPB + humanize(t0 + 1, 15, 5), 0.50, -0.05);  // beat 2, laid back
            snare808(t0 + 3.0 * SPB + humanize(t0 + 3, 15, 5), 0.50, +0.05);  // beat 4, laid back
        }
        report("  808 snare on backbeats from ~1:25");
    }

    // OPENING SINE BELL REMOVED — the long G5/G6 sine tails read as
    // a "squeegee" sound at t=0 (@jeffrey 2026-05-29). Field bells +
    // sine-bridge "ah" carry the intro instead.

    // ── 3 · SUB BASS ──────────────────────────────────────────────────
    // Long sustained sub-bass note per chord block. Drops out only
    // during the mid-break (bars 48..63) and final outro fade.
    for (int bar = 0; bar < TOTAL_BARS; bar += 4) {
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= TOTAL_BARS - 2;   // full groove to the end
        if (sec_break || sec_outro) continue;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        const double dur = 4.0 * BAR;
        bass_render(t0, dur, c->bass,
            (BassOpts){ .atk = 0.40, .rel = 1.5, .pan = 0.0, .gain = 0.30 });
    }
    report("  sub-bass per chord (skipped in break + outro)");

    // ── 4 · CHORD STAB + POWERSAW HYMNAL CHORDS ──────────────────────
    // Delayed to bar 12 (t=24 s) — the bar-4 entry was the loud
    // "white noise crash @ 8 s" @jeffrey flagged. Now they enter
    // 4 bars before the vocal drop for a clean swell.
    int topIdx = 0;   // persists across bars so the top band rarely jumps
    // The power-saws don't enter until the first WOOP at ~32 s (bar 16) —
    // the intro stays just dark filtered drums + pads (@jeffrey).
    for (int bar = 16; bar < TOTAL_BARS; bar++) {
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= TOTAL_BARS - 2;   // full groove to the end
        if (sec_break || sec_outro) continue;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        // Chord stab (piano-like) — on each 4-bar chord change.
        if (((bar - 16) & 3) == 0) {
            chord_stab_render(t0, c->root, c->third, c->fifth,
                (ChordStabOpts){ .decay_s = 1.6, .pan = 0.0,
                                 .gain = 0.42, .wet_send = 0.45,
                                 .brightness = 0.60 });
        }
        // POWERSAW HYMNAL — 3 detuned-saw bands (7 saws each), re-attacked
        // every bar so they pulse rather than holding one chord for 8 s.
        // LOWER register now (a sub band an octave under the root + a
        // low-mid root) for weight, and the top band shifts only RARELY
        // and within a tight range — subtle voicing, not a melody.
        const double dur = BAR + 0.20;          // 1-bar sustain, slight overlap
        const int tops[4] = { c->third, c->fifth, c->root + 12, c->third };
        if (rng_d() < 0.12) topIdx = rng_range(0, 3);   // fewer changes
        const int top = tops[topIdx];
        // CHAINSAW ZONE around 1:30 — now nearly OFF (@jeffrey 2026-06-02
        // "not such a drop-drama thing… better musical resolution there").
        // Just a whisper of extra detune; no gain rev. The powersaws stay a
        // steady hymnal bed the high-bell line resolves over.
        double cs = 0.0;
        if (bar >= 38 && bar < 48) { cs = 1.0 - fabs((double)bar - 44.0) / 6.0; if (cs < 0) cs = 0; }
        const double csDet  = cs * 3.0;         // faint buzz only
        const double csGain = 1.0 + cs * 0.05;  // no real swell
        const double csAtk  = 1.0 - cs * 0.1;
        powersaw_render(t0, dur, c->root - 12,           // sub saw — more low
            (PowersawOpts){ .atk = 0.06 * csAtk, .rel = 0.30, .pan = -0.16,
                            .gain = 0.13 * csGain, .wet_send = 0.35, .detune_cents = 12.0 + csDet });
        powersaw_render(t0, dur, c->root,                // low-mid root
            (PowersawOpts){ .atk = 0.08 * csAtk, .rel = 0.30, .pan = +0.12,
                            .gain = 0.10 * csGain, .wet_send = 0.40, .detune_cents = 16.0 + csDet });
        powersaw_render(t0, dur, top,                    // subtle upper voice
            (PowersawOpts){ .atk = 0.10 * csAtk, .rel = 0.30, .pan = 0.0,
                            .gain = 0.07 * csGain, .wet_send = 0.40, .detune_cents = 18.0 + csDet });
    }
    report("  chord stabs + powersaw (chainsaw snarl ~1:30)");

    // ── 5 · MUSICAL MOVEMENTS — multiple structured glock layers ─────
    //
    // 5a · BREAK glock atmospherics (bars 48..63): sparse pinged bells
    //      drawn from chord tones (root, fifth, octave) with random pan.
    {
        const struct { int bar; int midi_off; double pan; } breakhits[] = {
            { 48,  0, -0.30 },
            { 52,  7,  0.28 },
            { 56,  0,  0.18 },
            { 60, 12, -0.25 },
        };
        const int n = sizeof(breakhits) / sizeof(breakhits[0]);
        for (int k = 0; k < n; k++) {
            const int bar = breakhits[k].bar;
            const Chord *c = chord_for_bar(bar);
            const int base = (breakhits[k].midi_off == 7) ? c->fifth :
                             (breakhits[k].midi_off == 12) ? c->root + 12 :
                             c->root;
            glock_render(bar * BAR, 4.0, base,
                (GlockOpts){ .decay_s = 3.5, .pan = breakhits[k].pan,
                             .gain = 0.20, .wet_send = 0.60 });
        }
    }

    // 5a· · RESOLVED BREAK MELODY (~1:36-2:08) — REPLACES the old random
    //       water-droplet pings, which read as cacophonous. A deliberate,
    //       consonant bell phrase built ONLY from the CURRENT chord's tones
    //       (so it's always in harmony), arcing up and RESOLVING DOWN to the
    //       chord root each phrase, in a warm mid bell register, wide and
    //       wet for space. A real musical movement (@jeffrey "much more
    //       resolved 1:30-2:00, bells more musical, not cacophonous").
    {
        const double SPB = 60.0 / BPM;
        // music-box contour as indices into the chord-tone array below — a
        // gentle rise that always lands back on the root (index 0) = the
        // resolution. The 8-step phrase spans 4 bars so it breathes over
        // the chord changes, then repeats.
        const int contour[8] = { 0, 2, 4, 3, 2, 1, 2, 0 };
        for (int bar = 48; bar < 56; bar++) {   // first half of the break; vortex takes over at 56
            const Chord *c = chord_for_bar(bar);
            // low→high chord tones in a warm mid bell register
            const int tones[5] = { c->root + 12, c->third + 12, c->fifth + 12,
                                   c->root + 24, c->fifth + 24 };
            const double t0 = bar * BAR;
            // two bells per bar (beats 0 and 2) walking the contour
            for (int half = 0; half < 2; half++) {
                const int step = (((bar - 48) * 2) + half) % 8;
                const int onRoot = (contour[step] == 0);
                const int m = tones[contour[step]];
                const double dec = onRoot ? 3.4 : 1.9;        // root rings longest = resolution
                glock_render(t0 + half * 2 * SPB, dec + 0.5, m,
                    (GlockOpts){ .decay_s = dec, .pan = (half == 0 ? -0.16 : 0.16),
                                 .gain = onRoot ? 0.10 : 0.075, .wet_send = 0.78 });
            }
            // (reverberant noise-hat REMOVED — @jeffrey heard it as
            //  "bit-crunching" around 1:40; the break is just clean bells now.)
        }
        report("  resolved break melody: chord-tone bells (clean, no noise hat)");
    }

    // 5a·· · BELL VORTEX (~1:52-2:08) — @jeffrey: "the 2:00 mark with no
    //        kicks and just the sine bells… our chance for a banging bell
    //        vortex! use all our bells and whistles!" The empty break
    //        erupts into a dense, swirling, scratch-warbled bell storm that
    //        accelerates (8ths→16ths) and builds straight into drop 2:
    //        quick pitch-glissando "scratch" flicks + a spinning stereo pan.
    {
        const double SPB = 60.0 / BPM;
        // a WIDE G-pentatonic pool across octaves = all the bells/whistles
        const int pool[] = { 55,57,59,62,64,67,69,71,74,76,79,81,83,86 }; // G3..D6
        const int npool = (int)(sizeof(pool) / sizeof(pool[0]));
        uint32_t vr = 0xBE11C0DEu;
        for (int bar = 56; bar < 64; bar++) {
            const double t0 = bar * BAR;
            const double build = (double)(bar - 56) / 7.0;       // 0..1 toward the drop
            const int subdiv = (bar < 60) ? 8 : 16;              // accelerate into the vortex
            for (int s = 0; s < subdiv; s++) {
                vr ^= vr << 13; vr ^= vr >> 17; vr ^= vr << 5;
                const double r0 = (double)vr / 4294967296.0;
                vr ^= vr << 13; vr ^= vr >> 17; vr ^= vr << 5;
                const double r1 = (double)vr / 4294967296.0;
                const double tb = t0 + s * (BAR / subdiv);
                // SCRATCH flick: a quick 3-note pitch glissando (up or down)
                // from a seed tone — reads like a turntable flick.
                const int dir = (r0 < 0.5) ? 1 : -1;
                int idx = (int)(r1 * (npool - 3)); if (idx < 0) idx = 0;
                for (int f = 0; f < 3; f++) {
                    const int m = pool[((idx + dir * f) % npool + npool) % npool];
                    // spinning pan = the vortex
                    const double pan = sin(tb * 6.0 + f * 0.8) * 0.9;
                    const double g = (0.045 + 0.05 * build) * (1.0 - 0.2 * f);
                    glock_render(tb + f * 0.02, 0.14 + 0.12 * build, m,
                        (GlockOpts){ .decay_s = 0.11 + 0.12 * build, .pan = pan,
                                     .gain = g, .wet_send = 0.55 });
                }
            }
        }
        report("  bell vortex ~1:52-2:08: scratch glissandos + spinning pan, building into drop 2");
    }

    // 5a+ · HIHAT SPEED-UP — fast hi-hats in a metric accelerando through
    //       the late break into drop 2 (~2:04..2:08): tick · tick · t·t·t·
    //       ttttsssss — speeds up through the subdivisions (1/4 → 1/8 →
    //       1/16 → 1/32 → 1/64) into a dense buzz that resolves on the
    //       drop. All locked to the beat grid (in time, very regular).
    //       Each hat gets seeded tone (brightness ≈ pitch), decay and
    //       double-delay so attack/decay length and colour vary.
    {
        const double SPB = 60.0 / BPM;
        // hits-per-beat gears: accelerate up to 1/64 (16/beat) and stop —
        // the drop's groove takes over. `beats` = gear length (on-grid).
        const struct { int perBeat; double beats; } gears[] = {
            { 1, 1.5 }, { 2, 1.5 }, { 4, 1.5 }, { 8, 1.5 },
            { 16, 6.0 },                                     // hold the buzz ~3 s
        };
        const int ngears = (int)(sizeof(gears) / sizeof(gears[0]));
        // 12 beats (~6 s); start so the buzz holds across drop 2 entry.
        double t = 124.0;                                    // ~2:04 start
        int hits = 0;
        for (int gi = 0; gi < ngears; gi++) {
            const double interval = SPB / gears[gi].perBeat;
            const int n = (int)(gears[gi].beats * gears[gi].perBeat + 0.5);
            // louder at the fast gears (the downpour swells)
            const double speedNorm = (gears[gi].perBeat - 1) / 15.0; // 0..1
            for (int k = 0; k < n; k++, t += interval) {
                // CRISP & rhythmic (not arhythmic): accent the on-beat hit
                // of each gear, tight ping-pong pan, consistent tone — so
                // the acceleration reads as a clean buildup (@jeffrey).
                const int onBeat = (k % gears[gi].perBeat) == 0;
                const double dec  = 0.010;                    // consistent tight tick
                const double brt  = 0.92;
                const double pan  = (k % 2 == 0 ? -0.38 : +0.38);
                const double gain = (0.085 + 0.075 * speedNorm) * (onBeat ? 1.0 : 0.55);
                hat_render(t,
                    (HatOpts){ .pan = pan, .gain = gain, .decay_s = dec,
                               .brightness = brt, .double_delay_ms = 3.0 });
                hits++;
            }
        }
        report("  hihat speed-up accel into drop 2 (~2:04-2:10, up to 1/64, %d hits)", hits);
    }

    // 5b · DROP 2 melodic bell runs — now run all the way to the end so
    //      the last 70 s stays full (@jeffrey). For each 4-bar chord block,
    //      a seeded melodic arpeggio over chord tones across 3 octaves —
    //      up / down / zig-zag, varied note count, occasional long bell.
    //      EVERY note lands on a beat (kick grid), nothing off-beat.
    for (int bar = 64; bar < TOTAL_BARS; bar += 4) {
        if (bar >= 48 && bar < 64) continue;
        const Chord *c = chord_for_bar(bar);
        const double SPB = 60.0 / BPM;
        // 3:15 mix switch: bells come FORWARD as the perc steps back.
        const double bellMult = 1.0 + 0.8 * ramp01(bar * BAR, 195.0, 225.0);
        // chord-tone pool, low→high across 3 octaves
        // top octave dropped — the +24 tier read as too high/cringy.
        const int pool[] = { c->root, c->third, c->fifth,
                             c->root + 12, c->third + 12, c->fifth + 12,
                             c->root + 12, c->fifth + 12 };
        const int npool = (int)(sizeof(pool) / sizeof(pool[0]));
        // direction: 0=up, 1=down, 2=zigzag. ~2:20 (bar 68) forced up.
        int dir = (bar == 68) ? 0 : rng_range(0, 2);
        const int nnotes = (bar == 68) ? 6 : rng_range(4, 7);
        // ONE hit per beat — every bell aligned to a kick (@jeffrey
        // "all sine bells aligned to kicks, nothing outside of beat").
        const double hitStep = SPB;
        int idx = (dir == 1) ? npool - 1 : 0;   // down starts high
        for (int n = 0; n < nnotes; n++) {
            if (idx < 0) idx = 0; if (idx >= npool) idx = npool - 1;
            // ~15% ring out long; otherwise the bell length tracks the gap,
            // so fast bursts are SHORT pings that don't pile into mush
            // (@jeffrey "shorter if they burst that way").
            const int longBell = (rng_d() < 0.15);
            const double dec = longBell ? (3.0 + rng_d() * 2.0)
                                        : (hitStep * 2.8 + 0.20 + rng_d() * 0.3);
            const double dur = longBell ? 5.0 : dec + 0.3;
            const double pan = (rng_d() - 0.5) * 0.5;
            glock_render(bar * BAR + n * hitStep + humanize(bar * BAR + n, 15, 4), dur, pool[idx],
                (GlockOpts){ .decay_s = dec, .pan = pan,
                             .gain = 0.10 * bellMult, .wet_send = longBell ? 0.65 : 0.45,
                             .sidechain = 1 });
            // advance the melodic index per direction
            if (dir == 0)      idx += rng_range(1, 2);            // climbing
            else if (dir == 1) idx -= rng_range(1, 2);            // descending
            else               idx += (rng_d() < 0.5 ? 1 : -1) * rng_range(1, 2); // zigzag
        }
    }
    report("  drop 2 bell melodies (seeded up/down runs + long tails)");

    // 5b+ · 2:30-3:30 PLAY-MORE LAYER — long high sine bells drifting in
    //       over drop 2, with cool auto-panning that sweeps the stereo
    //       field bar-to-bar and level variation (some prominent, some
    //       distant), sidechained to the snare so they breathe with the
    //       backbeat (@jeffrey "play with the tracks more 2:30-3:30, some
    //       louder/quieter, cooler panning, long high sine bells sidechained
    //       to the snare").
    {
        const double SPB = 60.0 / BPM;
        for (int bar = 75; bar < 105; bar += 3) {       // ~150-210 s, sparse
            if (bar >= TOTAL_BARS - 2) break;
            const Chord *c = chord_for_bar(bar);
            const int tones[4] = { c->root + 12, c->fifth + 12, c->root + 24, c->third + 24 };
            const int step = ((bar - 75) / 3) % 4;
            const int m = tones[step];
            // cool panning: a slow sine sweep across the field, bar-to-bar
            const double pan = sin((double)(bar - 75) * 0.7) * 0.85;
            // level variation: every third bell sits prominent, the rest distant
            const int loud = (step % 3) == 0;
            const double bellMult = 1.0 + 0.8 * ramp01(bar * BAR, 195.0, 225.0);
            const double gain = (loud ? 0.085 : 0.045) * bellMult;
            const double dec  = loud ? 10.0 : 6.5;       // long tails
            glock_render(bar * BAR + (bar % 2 ? SPB * 2.0 : 0.0), dec + 1.0, m,
                (GlockOpts){ .decay_s = dec, .pan = pan,
                             .gain = gain, .wet_send = 0.72, .sidechain = 1 });
        }
        report("  2:30-3:30 play-more: long high sine bells, auto-pan + level var, snare-ducked");
    }

    // 5c · FINAL TOLLS — the groove now drives to the very end, so this is
    //      just a pair of big deep bells on the last two downbeats to land
    //      the ending, plus a held tonic powersaw chord ringing out.
    {
        const Chord *c = chord_for_bar(0);                 // tonic (G)
        const int b1 = TOTAL_BARS - 2, b2 = TOTAL_BARS - 1;
        glock_render(b1 * BAR, 6.0, c->root, (GlockOpts){ .decay_s = 5.0,
            .pan = -0.2, .gain = 0.26, .wet_send = 0.80 });
        glock_render(b2 * BAR, 6.0, c->root - 12, (GlockOpts){ .decay_s = 5.0,
            .pan = 0.2, .gain = 0.28, .wet_send = 0.80 });
        // held tonic powersaw chord ringing out the ending
        const double endDur = 2.0 * BAR + 1.0;
        powersaw_render(b1 * BAR, endDur, c->root,      (PowersawOpts){ .atk = 0.30,
            .rel = 1.5, .pan = -0.2, .gain = 0.12, .wet_send = 0.45, .detune_cents = 14.0 });
        powersaw_render(b1 * BAR, endDur, c->fifth + 12, (PowersawOpts){ .atk = 0.40,
            .rel = 1.5, .pan = +0.2, .gain = 0.09, .wet_send = 0.45, .detune_cents = 18.0 });
        report("  final tolls + held tonic chord (ending)");
    }

    // 5c+ · OUTRO RESOLVE — a slow DESCENDING G-pentatonic bell phrase
    //       through the ~4:16-4:34 fade, so the fade-out has musical
    //       MOVEMENT, not just a volume decay (@jeffrey "the 4:33 musical
    //       changes in the fade out"). Resolves down to the tonic, long
    //       tails, gentle alternating pan, riding out as everything fades.
    {
        const Chord *c = chord_for_bar(0);                 // tonic G
        const double SPB = 60.0 / BPM;
        const int line[6] = { c->fifth + 24, c->third + 24, c->root + 24,
                              c->fifth + 12, c->third + 12, c->root + 12 };
        double t = 256.0;                                  // ~4:16
        for (int i = 0; i < 6; i++) {
            if (t >= TOTAL_SEC) break;
            const double dec = 6.0 + i * 1.4;              // later notes ring longer
            glock_render(t, dec + 1.0, line[i],
                (GlockOpts){ .decay_s = dec, .pan = (i & 1 ? 0.30 : -0.30),
                             .gain = 0.085, .wet_send = 0.76, .sidechain = 0 });
            t += 2.0 * SPB;                                // every 2 beats (~1 s)
        }
        report("  outro resolve: descending pentatonic bells through the fade");
    }

    // 5d · TRANSITION RISERS — noise sweep before break + before drop 2.
    //      Builds tension. Uses shaker-style burst with progressive
    //      volume ramp over 2 bars.
    {
        const int risers[] = { 62 };  // dropped the bar-46 riser (~1:32 noise, @jeffrey "crunching ~1:40")
        const int n = sizeof(risers) / sizeof(risers[0]);
        for (int k = 0; k < n; k++) {
            const int bar = risers[k];
            if (bar >= TOTAL_BARS) break;
            const double SPB = 60.0 / BPM;
            // 16 shaker burst across 2 bars — GENTLE ramp now (@jeffrey
            // "not such a drop-drama thing"): a soft lift, not a riser slam.
            for (int s = 0; s < 16; s++) {
                const double t = bar * BAR + s * SPB * 0.5;
                const double gain = 0.025 + 0.05 * (s / 15.0);  // ramp 0.025→0.075
                shaker_render(t,
                    (ShakerOpts){ .pan = (s % 2 == 0 ? -0.25 : +0.25),
                                  .gain = gain, .decay_s = 0.08 });
            }
            // CRASH REMOVED — both the reverse-kick noise sweep and
            // the synthetic cymbal model were "wrong sound" (@jeffrey
            // 2026-05-29). Section transitions rely on the shaker
            // riser + reverse_kick swells alone for now.
        }
        report("  transition risers before break (bar 46) + drop 2 (bar 62)");
    }

    // 5e · WOODBLOCK ACCENTS — punctual knocks at the section transitions,
    //      REPLACING the old crash/smash noise (@jeffrey "more punctual, add
    //      wood block to replace the crashes/smashes"). A crisp triple
    //      pickup landing on each downbeat instead of a noisy riser.
    {
        const double SPB = 60.0 / BPM;
        const int hits[] = { 16, 32, 64 };   // the WOOP, drop 1, drop 2
        for (int h = 0; h < 3; h++) {
            const double td = hits[h] * BAR;
            woodblock(td - 1.5 * SPB, 0.16, -0.30, 1.00);
            woodblock(td - 1.0 * SPB, 0.20,  0.30, 1.05);
            woodblock(td - 0.5 * SPB, 0.26,  0.00, 1.10);
            woodblock(td,             0.34,  0.00, 1.00);   // landing knock
        }
        report("  woodblock accents at transitions (replacing crashes)");
    }

    // ── 6 · APPLY REVERB (wet 0.55 — much wetter than the hymn) ──────
    // (Earlier sidechain duck attempt removed — it was ducking the kick
    // itself along with the pads, defeating the purpose. Real sidechain
    // needs the kick rendered to a separate bus; for now we just let
    // the boosted kick gain do the work.)
    apply_reverb(0.55);
    report("  reverb applied (wet 0.55 — long deep-house tail)");
}

// ── WAV writer ───────────────────────────────────────────────────────
static void write_wav(const char *path) {
    // ── INTRO FILTER SWEEP ──────────────────────────────────────────
    // Open the whole bed from a dark lowpass up to full bandwidth over
    // the first ~32 s, so the track starts on low pitches under a filter
    // and brightens into the first WOOP (@jeffrey "start with low pitches,
    // low pass filter on them"). One-pole LP with an exponentially rising
    // cutoff (≈250 Hz → ≈20 kHz).
    {
        const double openSec = 32.0;
        double lpL = 0.0, lpR = 0.0;
        const long iOpen = (long)(openSec * SR);
        // Amplitude swell — the instruments entering in the first seconds
        // come in QUIET and grow over ~22 s, so the 7-8 s entries aren't
        // loud at first (@jeffrey). Cubic ease from 0.30 → 1.0.
        const double swellSec = 22.0;
        for (long i = 0; i < N && i < iOpen; i++) {
            const double tt = (double)i / (double)SR;
            const double frac = (double)i / (double)iOpen;        // 0..1
            const double cut = 250.0 * pow(20000.0 / 250.0, frac);
            const double a = 1.0 - exp(-TAU * cut / (double)SR);
            lpL += a * (L[i] - lpL);
            lpR += a * (R[i] - lpR);
            double g = 1.0;
            if (tt < swellSec) { const double x = tt / swellSec; g = 0.30 + 0.70 * x * x * x; }
            L[i] = (float)(lpL * g);
            R[i] = (float)(lpR * g);
        }
    }
    double peak = 0.0;
    for (long i = 0; i < N; i++) {
        double a = fabs(L[i]); if (a > peak) peak = a;
        double b = fabs(R[i]); if (b > peak) peak = b;
    }
    report("  peak before tanh: %.3f (drive %.3f → %.3f into tanh)",
           peak, DRIVE, peak * DRIVE);
    double post_peak = 0.0;
    for (long i = 0; i < N; i++) {
        L[i] = tanhf(L[i] * DRIVE);
        R[i] = tanhf(R[i] * DRIVE);
        double a = fabs(L[i]); if (a > post_peak) post_peak = a;
        double b = fabs(R[i]); if (b > post_peak) post_peak = b;
    }
    const double target = pow(10.0, -3.0 / 20.0);
    const double gain = post_peak > 0 ? target / post_peak : 1.0;
    report("  peak after tanh: %.3f → gain %.3f (target -3 dB)", post_peak, gain);
    for (long i = 0; i < N; i++) { L[i] *= gain; R[i] *= gain; }

    // ── BITCRUSH — square-wavey + bubbly ─────────────────────────────
    // Gentle 4-bit reduction (square steps, NOT brutal) + a short bubbly
    // reverb on the crushed signal, NO hard sample-slicing (decimation),
    // and a wet cap so the dry mix ALWAYS bleeds through — never that
    // extreme (@jeffrey). Time-varying w(t): intro haze → dry over ~8 s,
    // and a swing-back on the ~1:28 build.
    {
        const double WMAX = 0.55;                  // cap: always ≥45% dry
        const double LV   = pow(2.0, 4.0 - 1.0);   // 4-bit → squarish steps
        const double sr_scale = (double)SR / 44100.0;
        Comb cL[4], cR[4]; Allpass apL[2], apR[2];
        const int dl_L[4] = { 1116, 1188, 1277, 1356 };   // short = bubbly
        const int dl_R[4] = { 1139, 1211, 1300, 1379 };
        for (int k = 0; k < 4; k++) {
            comb_init(&cL[k], (long)(dl_L[k] * sr_scale), 0.70f);
            comb_init(&cR[k], (long)(dl_R[k] * sr_scale), 0.70f);
        }
        allpass_init(&apL[0], (long)(225 * sr_scale)); allpass_init(&apL[1], (long)(341 * sr_scale));
        allpass_init(&apR[0], (long)(244 * sr_scale)); allpass_init(&apR[1], (long)(322 * sr_scale));
        for (long i = 0; i < N; i++) {
            const double tt = (double)i / (double)SR;
            double w = 0.0;
            // (intro bitcrush haze REMOVED — @jeffrey "remove all
            //  bitcrunching from the start of track"; the start is clean now.)
            // (mid-track 1:23-1:34 crunch swing REMOVED — @jeffrey "not such
            //  a drop-drama thing"; only the intro haze crush remains.)
            w *= WMAX;
            // square-wave bit reduction (no decimation = no hard slicing)
            const double dlv = L[i], drv = R[i];
            const double cl = round(dlv * LV) / LV;
            const double cr = round(drv * LV) / LV;
            // bubbly reverb fed continuously (keeps the tail smooth)
            float sL = 0, sR = 0;
            for (int k = 0; k < 4; k++) { sL += comb_tick(&cL[k], (float)cl); sR += comb_tick(&cR[k], (float)cr); }
            sL = allpass_tick(&apL[0], sL); sL = allpass_tick(&apL[1], sL);
            sR = allpass_tick(&apR[0], sR); sR = allpass_tick(&apR[1], sR);
            if (w > 0.0) {
                const double wetL = cl * 0.70 + sL * 0.55;     // crushed + bubbly wash
                const double wetR = cr * 0.70 + sR * 0.55;
                L[i] = (float)(dlv * (1.0 - w) + wetL * w);
                R[i] = (float)(drv * (1.0 - w) + wetR * w);
            }
        }
        for (int k = 0; k < 4; k++) { free(cL[k].buf); free(cR[k].buf); }
        for (int k = 0; k < 2; k++) { free(apL[k].buf); free(apR[k].buf); }
        report("  bitcrush: 4-bit square + bubbly reverb, dry always bleeds (≤%.0f%% wet)", WMAX * 100);
    }

    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "amaythingra: cannot open %s\n", path); exit(1); }
    const long data_size = N * 2 * 2;
    uint8_t header[44];
    memcpy(header + 0, "RIFF", 4);
    *(uint32_t*)(header + 4) = (uint32_t)(36 + data_size);
    memcpy(header + 8, "WAVE", 4);
    memcpy(header + 12, "fmt ", 4);
    *(uint32_t*)(header + 16) = 16;
    *(uint16_t*)(header + 20) = 1;
    *(uint16_t*)(header + 22) = 2;
    *(uint32_t*)(header + 24) = (uint32_t)SR;
    *(uint32_t*)(header + 28) = (uint32_t)(SR * 2 * 2);
    *(uint16_t*)(header + 32) = 4;
    *(uint16_t*)(header + 34) = 16;
    memcpy(header + 36, "data", 4);
    *(uint32_t*)(header + 40) = (uint32_t)data_size;
    fwrite(header, 1, 44, f);
    int16_t *out = malloc(N * 2 * sizeof(int16_t));
    for (long i = 0; i < N; i++) {
        double sL = L[i], sR = R[i];
        if (sL > 1.0) sL = 1.0; else if (sL < -1.0) sL = -1.0;
        if (sR > 1.0) sR = 1.0; else if (sR < -1.0) sR = -1.0;
        out[2*i+0] = (int16_t)lrint(sL * 32767.0);
        out[2*i+1] = (int16_t)lrint(sR * 32767.0);
    }
    fwrite(out, sizeof(int16_t), N * 2, f);
    free(out);
    fclose(f);
    report("✓ wrote %s (%.1f MB · %.2f s · %d Hz · stereo)",
           path, data_size / 1024.0 / 1024.0, (double)N / SR, SR);
}

// ── main ─────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    t0_wall = now_wall();
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) OUT_PATH = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPM = atof(argv[++i]);
        else if (!strcmp(argv[i], "--seconds") && i + 1 < argc) TOTAL_SEC = atof(argv[++i]);
        else if (!strcmp(argv[i], "--drive") && i + 1 < argc) DRIVE = atof(argv[++i]);
        else if (!strcmp(argv[i], "--mono-kick")) MONO_KICK = 1;
        else if (!strcmp(argv[i], "--seed") && i + 1 < argc) SEED_ARG = (unsigned)strtoul(argv[++i], NULL, 10);
        else {
            fprintf(stderr, "amaythingra: unknown arg %s\n", argv[i]);
            return 1;
        }
    }
    if (!OUT_PATH) { fprintf(stderr, "amaythingra: --out required\n"); return 1; }

    // Seed per-render variation, then roll a fresh chord progression.
    const uint32_t seed = SEED_ARG ? SEED_ARG : (uint32_t)time(NULL);
    rng_seed(seed);
    init_prog();
    report("  seed %u · progression %s-%s-%s-%s",
           seed, PROG[0].name, PROG[1].name, PROG[2].name, PROG[3].name);

    N = (long)(TOTAL_SEC * SR);
    L  = calloc(N, sizeof(float));
    R  = calloc(N, sizeof(float));
    WL = calloc(N, sizeof(float));
    WR = calloc(N, sizeof(float));
    if (!L || !R || !WL || !WR) {
        fprintf(stderr, "amaythingra: alloc failed\n"); return 1;
    }
    report("amaythingra: rendering %.2f s @ %d Hz", TOTAL_SEC, SR);
    render_track();
    write_wav(OUT_PATH);
    free(L); free(R); free(WL); free(WR);
    return 0;
}
