// amazing-dance.c — Loukeman-style melodic deep house remix of
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
//   32..87 s                VOCAL  — amazing-wavewizard.wav layered in
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
// Run:   ./amazing-dance --out out/amazing-dance.wav

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
static double TOTAL_SEC = 300.0;
static const char *OUT_PATH = NULL;

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
    return (bias_ms + tri * max_ms) / 1000.0;
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
static void hat_render(double t0, HatOpts opt) {
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
} GlockOpts;
static void glock_render(double t0, double dur, double midi, GlockOpts opt) {
    const double f = m2f(midi);
    const double decay = opt.decay_s > 0 ? opt.decay_s : 1.5;
    const double pan = opt.pan;
    const double wetSend = opt.wet_send;
    double phs1 = 0, phs2 = 0, phs3 = 0;
    long iStart = (long)(t0 * SR);
    long iEnd   = (long)((t0 + dur) * SR);
    if (iStart < 0) iStart = 0;
    if (iEnd > N) iEnd = N;
    for (long i = iStart; i < iEnd; i++) {
        const double t = (i - iStart) / (double)SR;
        double env = (t < 0.002) ? (t / 0.002) : exp(-(t - 0.002) / decay);
        if (t > 0.005 && env < 1e-6) break;
        const double dP1 = f / (double)SR;
        const double dP2 = (f * 2.76) / (double)SR;
        const double dP3 = (f * 5.40) / (double)SR;
        phs1 += dP1; if (phs1 >= 1.0) phs1 -= 1.0;
        phs2 += dP2; if (phs2 >= 1.0) phs2 -= 1.0;
        phs3 += dP3; if (phs3 >= 1.0) phs3 -= 1.0;
        const double sample = env * opt.gain *
                              (0.80 * sin(TAU * phs1) +
                               0.40 * sin(TAU * phs2) +
                               0.20 * sin(TAU * phs3));
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
// G major progression — slow 4-bar changes.
static const Chord PROG[4] = {
    { 31, 43, 47, 50, "G" },   // I  — G2 bass, G2 B2 D3 triad (one octave below hymn pad)
    { 36, 48, 52, 55, "C" },   // IV — C2 bass, C3 E3 G3
    { 31, 43, 47, 50, "G" },   // I
    { 26, 38, 42, 45, "D" },   // V — D2 bass, D2 F#2 A2 (low for sub feel)
};

// ── helpers for the dance pattern ────────────────────────────────────
// SECTIONS — return which chord index applies for a given bar position.
// Progression cycles every 4 bars.
static const Chord *chord_for_bar(int bar) {
    return &PROG[bar & 3];
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

    report("amazing-dance: bpm=%.1f, bar=%.3fs, total %.2fs (%d bars)",
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

    // ── 1.5 · TRIANGLE LOW + SINE BELL HIGH (vocal harmony backing) ──
    // Delayed to bar 12 (t=24 s) along with chord stab + powersaw —
    // before bar 12 the build stays gentle, just bed + drums + bells.
    for (int bar = 12; bar < 64; bar += 4) {
        if (bar >= TOTAL_BARS) break;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        const double dur = 4.0 * BAR + 0.5;
        // LOW triangle — pitch narrative just like the high bells:
        // root → third → fifth → root as the 4-bar cycles progress
        // (@jeffrey 2026-05-29 "lower pitched sine bells too should
        // change over time").
        const int lo_variant = ((bar - 12) / 4) % 4;
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
        // HIGH sine bells — pitch narrative across 4 chord cycles
        // (@jeffrey 2026-05-29 "sine bells too repetitive / change
        // pitch over time"). Each 4-bar block picks different chord
        // tones so the sparkle moves around the chord, not just
        // root+fifth on every cycle.
        const int variant = ((bar - 12) / 4) % 4;
        int hi1_off, hi2_off, hi3_off;
        switch (variant) {
            case 0: hi1_off = c->fifth + 12; hi2_off = c->root  + 24; hi3_off = c->third + 19; break;
            case 1: hi1_off = c->third + 12; hi2_off = c->fifth + 24; hi3_off = c->root  + 19; break;
            case 2: hi1_off = c->root  + 12; hi2_off = c->third + 24; hi3_off = c->fifth + 19; break;
            default:hi1_off = c->fifth + 12; hi2_off = c->third + 24; hi3_off = c->root  + 24; break;
        }
        // Volumes pulled down ~40% (@jeffrey 2026-05-29 "sine bells
        // lower volume"). 0.16 → 0.10, 0.12 → 0.07, 0.10 → 0.06.
        glock_render(t0, 3.5, hi1_off,
            (GlockOpts){ .decay_s = 3.0, .pan = +0.25,
                         .gain = 0.10, .wet_send = 0.55 });
        glock_render(t0 + 1.0, 3.0, hi2_off,
            (GlockOpts){ .decay_s = 2.5, .pan = -0.20,
                         .gain = 0.07, .wet_send = 0.55 });
        if (variant == 1 || variant == 3) {
            glock_render(t0 + 2.2, 2.0, hi3_off,
                (GlockOpts){ .decay_s = 2.0, .pan = +0.10,
                             .gain = 0.06, .wet_send = 0.50 });
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
        // First 3 bars (0..6 s) = soft church-bell intro (no kit), then
        // the beat enters from bar 3 onwards. Break drops kick at the
        // bars 48..63 mid-track break, outro drops it at bar 104+.
        const int sec_intro = bar < 3;
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= 104;
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
            const double kgain = 0.75 + (2.20 - 0.75) * phase;
            const double kphi  = 110.0 + (160.0 - 110.0) * phase;
            const double kplo  = 56.0;
            const double kpdur = 0.11  + (0.13  - 0.11)  * phase;
            const double kdec  = 0.34  + (0.44  - 0.34)  * phase;
            for (int beat = 0; beat < 4; beat++) {
                // Lazy-human kick: ±8 ms with +3 ms positive bias.
                // Downbeats stay tight (less wiggle).
                const double tHit = t0 + beat * SPB +
                                    humanize(t0 + beat, beat == 0 ? 4 : 8, 3);
                if (phase < 0.6) {
                    // HAAS pair widens the kick image at low energy
                    const double haasPan = 0.35 * (1.0 - phase / 0.6);
                    const double sideGain = kgain * 0.55;
                    const double centerGain = kgain * 0.85;
                    kick_render(tHit,
                        (KickOpts){ .pan = -haasPan, .gain = sideGain,
                                    .pitch_hi = kphi, .pitch_lo = kplo,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                    kick_render(tHit + 0.003,
                        (KickOpts){ .pan = +haasPan, .gain = sideGain,
                                    .pitch_hi = kphi, .pitch_lo = kplo,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                    kick_render(tHit,
                        (KickOpts){ .pan = 0.0, .gain = centerGain,
                                    .pitch_hi = kphi, .pitch_lo = kplo,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                } else {
                    kick_render(tHit,
                        (KickOpts){ .pan = 0.0, .gain = kgain,
                                    .pitch_hi = kphi, .pitch_lo = kplo,
                                    .pitch_dur = kpdur, .decay_s = kdec });
                }
            }
            // REVERSE KICK — SPRINKLED, never earlier than ~30 s
            // (@jeffrey 2026-05-29). Picks 6 strategic bar boundaries
            // through the rest of the track instead of every 4 bars.
            // Each is a 2-beat rising sweep building into the next bar.
            const int rev_bars[] = { 15, 23, 39, 47, 79, 95 };
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
            // Open hat (double) on offbeats. Lazy-human: ±12 ms with
            // +5 ms late bias. Each hat is internally a doubled burst
            // (two layered hits 4 ms apart — the AC-native sound).
            for (int beat = 0; beat < 4; beat++) {
                const double base = t0 + (beat + 0.5) * SPB;
                const double th = base + humanize(base, 12, 5);
                // Occasionally widen the double-delay for variety —
                // ~25% chance of a 12 ms flam instead of the tight 4 ms.
                uint32_t r = (uint32_t)(base * 10007.0) | 1;
                r ^= r << 13; r ^= r >> 17; r ^= r << 5;
                const int flam = (r & 0x3) == 0;   // ~25% probability
                // CLICKY: decay 0.045 → 0.022 ("too reverby" — @jeffrey 2026-05-29)
                hat_render(th,
                    (HatOpts){ .pan = (beat % 2 == 0 ? -0.18 : +0.18),
                               .gain = 0.18, .decay_s = 0.022,
                               .brightness = 0.96,
                               .double_delay_ms = flam ? 6.0 : 3.0 });
            }
            // HI-HAT RUSHES — only at bars 44 and 76 now. The bar-12
            // rush was landing exactly when MAY enters (@jeffrey flagged
            // "white noise crashing"); removed.
            const int rush_anchor_bars[] = { 47, 79 };
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
                const double accent = (eighth % 2 == 0) ? 0.14 : 0.10;
                shaker_render(ts,
                    (ShakerOpts){ .pan = (eighth % 2 == 0 ? +0.16 : -0.16),
                                  .gain = accent, .decay_s = 0.055 });
            }
        }
    }
    report("  drums patterned (beat enters at bar 3 after the bell intro)");

    // OPENING SINE BELL REMOVED — the long G5/G6 sine tails read as
    // a "squeegee" sound at t=0 (@jeffrey 2026-05-29). Field bells +
    // sine-bridge "ah" carry the intro instead.

    // ── 3 · SUB BASS ──────────────────────────────────────────────────
    // Long sustained sub-bass note per chord block. Drops out only
    // during the mid-break (bars 48..63) and final outro fade.
    for (int bar = 0; bar < TOTAL_BARS; bar += 4) {
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= 104;
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
    for (int bar = 12; bar < TOTAL_BARS; bar += 4) {
        const int sec_break = bar >= 48 && bar < 64;
        const int sec_outro = bar >= 104;
        if (sec_break || sec_outro) continue;
        const Chord *c = chord_for_bar(bar);
        const double t0 = bar * BAR;
        // Chord stab (piano-like)
        chord_stab_render(t0, c->root, c->third, c->fifth,
            (ChordStabOpts){ .decay_s = 1.6, .pan = 0.0,
                             .gain = 0.42, .wet_send = 0.45,
                             .brightness = 0.60 });
        // POWERSAW HYMNAL — 3 detuned-saw voices stacked into a chord.
        // Sustained through the 4-bar chord block, panned wide for the
        // hugeness, heavy reverb wet.
        const double dur = 4.0 * BAR + 0.15;
        powersaw_render(t0, dur, c->root,
            (PowersawOpts){ .atk = 0.06, .rel = 0.45,
                            .pan = -0.20, .gain = 0.12,
                            .wet_send = 0.40, .detune_cents = 14.0 });
        powersaw_render(t0, dur, c->third,
            (PowersawOpts){ .atk = 0.08, .rel = 0.45,
                            .pan =  0.00, .gain = 0.10,
                            .wet_send = 0.40, .detune_cents = 16.0 });
        powersaw_render(t0, dur, c->fifth,
            (PowersawOpts){ .atk = 0.10, .rel = 0.45,
                            .pan = +0.20, .gain = 0.10,
                            .wet_send = 0.40, .detune_cents = 18.0 });
    }
    report("  chord stabs + powersaw hymnal chords (bars 4..47, 64..103)");

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

    // 5b · DROP 2 melodic counter-melody (bars 64..103). For each 4-bar
    //      chord block, play a 4-note ascending arpeggio at the chord
    //      tones (root → third → fifth → octave). This gives DROP 2 a
    //      distinct musical identity vs the earlier vocal-only PART 1.
    for (int bar = 64; bar < 104 && bar < TOTAL_BARS; bar += 4) {
        const Chord *c = chord_for_bar(bar);
        const int notes[4] = { c->root + 12, c->third + 12,
                               c->fifth + 12, c->root + 24 };
        const double pans[4] = { -0.22, +0.05, +0.18, -0.10 };
        const double SPB = 60.0 / BPM;
        for (int n = 0; n < 4; n++) {
            // Place hits on beats 1, 2, 3, 4 of bar
            glock_render(bar * BAR + n * SPB, 2.0, notes[n],
                (GlockOpts){ .decay_s = 1.6, .pan = pans[n],
                             .gain = 0.20, .wet_send = 0.45 });
        }
    }
    report("  drop 2 counter-melody (4-note arpeggios across the chord)");

    // 5c · OUTRO bell tolls (bars 104..119) — single deep bell every
    //      4 bars to mirror the intro. Closes the piece symmetrically.
    {
        const int outhits[] = { 104, 108, 112, 116 };
        const int n = sizeof(outhits) / sizeof(outhits[0]);
        for (int k = 0; k < n; k++) {
            const int bar = outhits[k];
            if (bar >= TOTAL_BARS) break;
            const Chord *c = chord_for_bar(bar);
            // Bell at chord root, low register — same flavor as intro.
            glock_render(bar * BAR, 5.0, c->root - 12,
                (GlockOpts){ .decay_s = 4.0, .pan = (k % 2 == 0 ? -0.2 : 0.2),
                             .gain = 0.24, .wet_send = 0.75 });
        }
        report("  outro bell tolls (every 4 bars, mirrors intro)");
    }

    // 5d · TRANSITION RISERS — noise sweep before break + before drop 2.
    //      Builds tension. Uses shaker-style burst with progressive
    //      volume ramp over 2 bars.
    {
        const int risers[] = { 46, 62 };  // bars: end of part1 + end of break
        const int n = sizeof(risers) / sizeof(risers[0]);
        for (int k = 0; k < n; k++) {
            const int bar = risers[k];
            if (bar >= TOTAL_BARS) break;
            const double SPB = 60.0 / BPM;
            // 16 shaker burst across 2 bars, progressively louder
            for (int s = 0; s < 16; s++) {
                const double t = bar * BAR + s * SPB * 0.5;
                const double gain = 0.05 + 0.15 * (s / 15.0);   // ramp 0.05→0.20
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
    double peak = 0.0;
    for (long i = 0; i < N; i++) {
        double a = fabs(L[i]); if (a > peak) peak = a;
        double b = fabs(R[i]); if (b > peak) peak = b;
    }
    report("  peak before tanh: %.3f", peak);
    double post_peak = 0.0;
    for (long i = 0; i < N; i++) {
        L[i] = tanhf(L[i]);
        R[i] = tanhf(R[i]);
        double a = fabs(L[i]); if (a > post_peak) post_peak = a;
        double b = fabs(R[i]); if (b > post_peak) post_peak = b;
    }
    const double target = pow(10.0, -3.0 / 20.0);
    const double gain = post_peak > 0 ? target / post_peak : 1.0;
    report("  peak after tanh: %.3f → gain %.3f (target -3 dB)", post_peak, gain);
    for (long i = 0; i < N; i++) { L[i] *= gain; R[i] *= gain; }

    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "amazing-dance: cannot open %s\n", path); exit(1); }
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
        else {
            fprintf(stderr, "amazing-dance: unknown arg %s\n", argv[i]);
            return 1;
        }
    }
    if (!OUT_PATH) { fprintf(stderr, "amazing-dance: --out required\n"); return 1; }

    N = (long)(TOTAL_SEC * SR);
    L  = calloc(N, sizeof(float));
    R  = calloc(N, sizeof(float));
    WL = calloc(N, sizeof(float));
    WR = calloc(N, sizeof(float));
    if (!L || !R || !WL || !WR) {
        fprintf(stderr, "amazing-dance: alloc failed\n"); return 1;
    }
    report("amazing-dance: rendering %.2f s @ %d Hz", TOTAL_SEC, SR);
    render_track();
    write_wav(OUT_PATH);
    free(L); free(R); free(WL); free(WR);
    return 0;
}
