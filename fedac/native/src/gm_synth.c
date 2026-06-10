// gm_synth.c — Standalone General-MIDI synthesis (see gm_synth.h).
//
// Extracted verbatim from audio.c. The DSP math is unchanged; the only edits are
// mechanical decoupling renames so the module owns its own state:
//   ACVoice* v            → GMVoice* v
//   v->noise_seed         → v->rng_seed         (own RNG stream)
//   v->whistle_bore_buf   → v->bore_buf         (own short KS delay line)
//   v->whistle_bore_w     → v->bore_w
//   v->noise_b0..a2       → v->nb0..na2         (own attack-burst biquad)
//   v->noise_x1..y2       → v->nx1..ny2
//   v->gun_click_env      → v->atk_env          (own attack-burst envelope)
//   v->gun_click_decay_mult → v->atk_dec
//   v->gun_secondary_trig → v->sec_trig         (own secondary-excitation timer)
//   v->gun_secondary_amp  → v->sec_amp
//   v->type = p->wave     → v->engine = GM_ENGINE_*
//   compute_envelope(v)   → passed-in `env`
//   v->frequency          → passed-in `frequency`
// AUDIO_SAMPLE_RATE fallback is inlined as GM_FALLBACK_SR.

#include "gm_synth.h"

#include <math.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// Sample-rate fallback when a caller passes <= 0 (was AUDIO_SAMPLE_RATE).
#define GM_FALLBACK_SR 192000.0

// ============================================================
// PRNG + small math helpers (copied from audio.c)
// ============================================================
static inline uint32_t xorshift32(uint32_t *state) {
    uint32_t x = *state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    *state = x;
    return x;
}

static inline double clampd(double x, double lo, double hi) {
    if (x < lo) return lo;
    if (x > hi) return hi;
    return x;
}

// ============================================================
// Sine wavetable — phase-increment lookup (GM synthesis library)
// ============================================================
#define WT_SIN_SIZE 4096
static float wt_sin_table[WT_SIN_SIZE + 1];  // +1 guard for interp wrap
static int   wt_sin_ready = 0;

void gm_synth_init(void) {
    if (wt_sin_ready) return;
    for (int i = 0; i <= WT_SIN_SIZE; i++) {
        wt_sin_table[i] = (float)sin(2.0 * M_PI * (double)i / (double)WT_SIN_SIZE);
    }
    wt_sin_ready = 1;
}

// Read the sine wavetable at a normalized phase in [0,1). Wraps any phase.
static inline double wt_sin(double phase) {
    phase -= (double)(int)phase;          // fractional part
    if (phase < 0.0) phase += 1.0;
    double fpos = phase * (double)WT_SIN_SIZE;
    int    i0 = (int)fpos;
    double f = fpos - (double)i0;
    return (double)wt_sin_table[i0] * (1.0 - f)
         + (double)wt_sin_table[i0 + 1] * f;
}

// ============================================================
// Bounded per-note stochasticism (docs/gm-synthesis/00-stochasticism.md)
// ============================================================
static double g_organic_amount = 0.6;

void gm_set_organic(double amt) { g_organic_amount = amt; }

// Uniform [0,1) from the voice PRNG.
static inline double voice_rand_unit(GMVoice *v) {
    return (double)xorshift32(&v->rng_seed) / (double)UINT32_MAX;
}

// Bipolar [-1,1] from the voice PRNG. Workhorse for every jitter lever.
static inline double voice_rand_bipolar(GMVoice *v) {
    return voice_rand_unit(v) * 2.0 - 1.0;
}

// Cents → frequency ratio. 1200 cents = 1 octave. cents_to_ratio(0)==1.0.
static inline double cents_to_ratio(double cents) {
    return pow(2.0, cents / 1200.0);
}

// Bounded multiplicative jitter around `center` by ±`frac`.
static inline double voice_jitter(GMVoice *v, double center,
                                  double frac, double mul) {
    double u = voice_rand_bipolar(v);
    return center * (1.0 + frac * g_organic_amount * mul * u);
}

// Bounded pitch detune in cents → ratio, HARD-CAPPED at ±6 cents.
#define ORGANIC_MAX_CENTS 6.0
static inline double voice_detune(GMVoice *v, double freq,
                                  double spread_cents, double mul) {
    double cents = spread_cents * g_organic_amount * mul * voice_rand_bipolar(v);
    if (cents >  ORGANIC_MAX_CENTS) cents =  ORGANIC_MAX_CENTS;
    if (cents < -ORGANIC_MAX_CENTS) cents = -ORGANIC_MAX_CENTS;
    return freq * cents_to_ratio(cents);
}

// Random start phase [0,1) for an additive/modal partial.
static inline double voice_rand_phase(GMVoice *v) {
    return voice_rand_unit(v);
}

// Fractional-delay read from a ring buffer.
static inline double gm_frac_read(const float *buf, int N, int w, double delay) {
    if (delay < 0.0) delay = 0.0;
    if (delay > (double)(N - 2)) delay = (double)(N - 2);
    double rd = (double)w - delay;
    while (rd < 0.0) rd += (double)N;
    int i0 = (int)rd;
    int i1 = (i0 + 1) % N;
    double f = rd - (double)i0;
    return (double)buf[i0] * (1.0 - f) + (double)buf[i1] * f;
}

// ============================================================
// GM synthesis library — Family 1: Piano (GM programs 1-8)
// ============================================================
typedef struct {
    GMEngine engine;        // which engine renders this program
    // -- Modal acoustic-piano params (GMPIANO) --
    int    partials;
    double B;
    double partial_tilt;
    double tilt_from;
    double tau0;
    double hammer_amp;
    double hammer_ms;
    double dual_cents;
    double drive;
    // -- FM tine/reed params (EPIANO) --
    double fm_ratio;
    double fm_index0;
    double fm_index_ms;
    double fm_tine_ratio;
    double fm_tine_index0;
    double fm_tine_ms;
    double fm_pickup;
    // -- Extended-KS params (PLUCK) --
    double ks_stretch;
    double ks_loop_b;
    double ks_beta;
    double ks_pick;
    double ks_drive;
    // -- Extended-KS batch-2 params (guitar / bass / ethnic plucked) --
    int    ks_big;
    int    ks_hard;
    double ks_exc_smooth;
    double ks_jawari;
    double ks_attack_amp;
    double ks_attack_ms;
    double ks_attack_bp;
    double ks_sec_ms;
    double ks_sec_amp;
    double bodyf[3];
    double bodyq[3];
    double bodyg[3];
    // -- Subtractive synth-bass / reed-as-saw params (SYNTHBASS) --
    int    sb_o2_sq;
    double sb_o2_cents;
    double sb_o2_mix;
    double sb_sub;
    double sb_fm0;
    double sb_fm_ms;
    double sb_cut0;
    double sb_cut1;
    double sb_cut_ms;
    double sb_res;
    double sb_psweep;
    double sb_psweep_ms;
    int    sb_sustained;
    double sb_drone_mix;
    double sb_breath;
    double sb_vib_hz;
    double sb_vib_depth;
} GMProgramParams;

#define GM_PIANO_PROGRAM_COUNT 8
static const GMProgramParams gm_piano_programs[GM_PIANO_PROGRAM_COUNT] = {
    // GM 1 — Acoustic Grand
    { .engine = GM_ENGINE_GMPIANO, .partials = 10, .B = 0.0009, .partial_tilt = 1.0,
      .tilt_from = 0, .tau0 = 9.0, .hammer_amp = 0.18, .hammer_ms = 5.0,
      .dual_cents = 0.0, .drive = 0.0 },
    // GM 2 — Bright Acoustic
    { .engine = GM_ENGINE_GMPIANO, .partials = 11, .B = 0.0010, .partial_tilt = 1.45,
      .tilt_from = 4, .tau0 = 9.5, .hammer_amp = 0.24, .hammer_ms = 3.5,
      .dual_cents = 0.0, .drive = 0.0 },
    // GM 3 — Electric Grand
    { .engine = GM_ENGINE_GMPIANO, .partials = 7, .B = 0.00045, .partial_tilt = 1.1,
      .tilt_from = 1, .tau0 = 7.0, .hammer_amp = 0.10, .hammer_ms = 4.0,
      .dual_cents = 0.0, .drive = 0.10 },
    // GM 4 — Honky-tonk
    { .engine = GM_ENGINE_GMPIANO, .partials = 9, .B = 0.0011, .partial_tilt = 1.15,
      .tilt_from = 2, .tau0 = 6.0, .hammer_amp = 0.26, .hammer_ms = 4.5,
      .dual_cents = 14.0, .drive = 0.0 },
    // GM 5 — Electric Piano 1 (Rhodes tine)
    { .engine = GM_ENGINE_EPIANO, .fm_ratio = 1.0, .fm_index0 = 1.2, .fm_index_ms = 700.0,
      .fm_tine_ratio = 14.0, .fm_tine_index0 = 0.9, .fm_tine_ms = 18.0,
      .fm_pickup = 0.18 },
    // GM 6 — Electric Piano 2 (Wurli reed)
    { .engine = GM_ENGINE_EPIANO, .fm_ratio = 2.0, .fm_index0 = 1.6, .fm_index_ms = 420.0,
      .fm_tine_ratio = 10.0, .fm_tine_index0 = 0.7, .fm_tine_ms = 14.0,
      .fm_pickup = 0.34 },
    // GM 7 — Harpsichord
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9965, .ks_loop_b = 0.18,
      .ks_beta = 0.13, .ks_pick = 0.95, .ks_drive = 0.0 },
    // GM 8 — Clavi
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.990, .ks_loop_b = 0.12,
      .ks_beta = 0.05, .ks_pick = 0.9, .ks_drive = 0.5 },
};

// ── Modal bank (MODAL) ──
#define GM_MODAL_MAX_MODES 8
typedef struct {
    const char *name;
    int    nmodes;
    double ratio[GM_MODAL_MAX_MODES];
    double amp[GM_MODAL_MAX_MODES];
    double t60[GM_MODAL_MAX_MODES];
    double strike_amp;
    double strike_ms;
    double trem_hz;
    double trem_depth;
    double bloom;
    int    pitched;
} GMModalParams;

// Chromatic Percussion (GM 9-15)
#define GM_CHROMPERC_FIRST 8
#define GM_CHROMPERC_COUNT 7
static const GMModalParams gm_chromperc_programs[GM_CHROMPERC_COUNT] = {
    { .name = "celesta", .nmodes = 3, .ratio = {1.0, 4.0, 10.8},
      .amp = {1.0, 0.30, 0.10}, .t60 = {1.6, 0.6, 0.25},
      .strike_amp = 0.06, .strike_ms = 2.5, .pitched = 1 },
    { .name = "glockenspiel", .nmodes = 4, .ratio = {1.0, 2.76, 5.40, 8.90},
      .amp = {1.0, 0.55, 0.32, 0.18}, .t60 = {2.2, 0.45, 0.25, 0.15},
      .strike_amp = 0.14, .strike_ms = 1.8, .pitched = 1 },
    { .name = "musicbox", .nmodes = 3, .ratio = {1.0, 6.27, 17.55},
      .amp = {1.0, 0.22, 0.08}, .t60 = {1.4, 0.4, 0.18},
      .strike_amp = 0.05, .strike_ms = 2.0, .pitched = 1 },
    { .name = "vibraphone", .nmodes = 3, .ratio = {1.0, 4.0, 9.6},
      .amp = {1.0, 0.35, 0.12}, .t60 = {5.0, 1.6, 0.8},
      .strike_amp = 0.04, .strike_ms = 3.0, .trem_hz = 5.0, .trem_depth = 0.3,
      .pitched = 1 },
    { .name = "marimba", .nmodes = 3, .ratio = {1.0, 4.0, 9.2},
      .amp = {1.0, 0.22, 0.08}, .t60 = {0.9, 0.35, 0.18},
      .strike_amp = 0.06, .strike_ms = 2.5, .pitched = 1 },
    { .name = "xylophone", .nmodes = 3, .ratio = {1.0, 3.0, 6.0},
      .amp = {1.0, 0.45, 0.20}, .t60 = {0.55, 0.25, 0.14},
      .strike_amp = 0.12, .strike_ms = 1.8, .pitched = 1 },
    { .name = "tubularbells", .nmodes = 6,
      .ratio = {2.0, 3.0, 4.16, 5.43, 6.79, 8.21},
      .amp = {1.0, 0.7, 0.5, 0.35, 0.22, 0.14},
      .t60 = {9.0, 7.0, 5.0, 3.5, 2.2, 1.4},
      .strike_amp = 0.16, .strike_ms = 2.2, .pitched = 1 },
};

// Percussive family (GM 113-119)
#define GM_PERC_FIRST 112
#define GM_PERC_COUNT 7
static const GMModalParams gm_perc_programs[GM_PERC_COUNT] = {
    { .name = "tinklebell", .nmodes = 5, .ratio = {0.5, 1.0, 1.19, 1.5, 2.0},
      .amp = {0.3, 1.0, 0.6, 0.5, 0.45}, .t60 = {0.7, 0.6, 0.4, 0.35, 0.3},
      .strike_amp = 0.10, .strike_ms = 1.5, .pitched = 1 },
    { .name = "agogo", .nmodes = 3, .ratio = {1.0, 1.52, 2.66},
      .amp = {1.0, 0.5, 0.3}, .t60 = {0.4, 0.25, 0.15},
      .strike_amp = 0.14, .strike_ms = 1.2, .pitched = 1 },
    { .name = "steeldrum", .nmodes = 5, .ratio = {1.0, 2.0, 2.6, 3.0, 4.2},
      .amp = {1.0, 0.6, 0.55, 0.4, 0.25}, .t60 = {1.4, 1.0, 0.9, 0.6, 0.4},
      .strike_amp = 0.10, .strike_ms = 2.0, .bloom = 0.25, .pitched = 1 },
    { .name = "woodblock", .nmodes = 3, .ratio = {1.0, 2.7, 5.4},
      .amp = {1.0, 0.4, 0.18}, .t60 = {0.09, 0.05, 0.035},
      .strike_amp = 0.18, .strike_ms = 0.9, .pitched = 1 },
    { .name = "taiko", .nmodes = 5, .ratio = {1.0, 1.59, 2.14, 2.30, 2.65},
      .amp = {1.0, 0.4, 0.25, 0.18, 0.12}, .t60 = {0.55, 0.3, 0.2, 0.16, 0.12},
      .strike_amp = 0.30, .strike_ms = 6.0, .pitched = 1 },
    { .name = "melodictom", .nmodes = 4, .ratio = {1.0, 1.59, 2.14, 2.30},
      .amp = {1.0, 0.3, 0.16, 0.10}, .t60 = {0.7, 0.35, 0.22, 0.16},
      .strike_amp = 0.16, .strike_ms = 4.0, .pitched = 1 },
    { .name = "synthdrum", .nmodes = 2, .ratio = {1.0, 2.0},
      .amp = {1.0, 0.15}, .t60 = {0.35, 0.12},
      .strike_amp = 0.12, .strike_ms = 1.0, .pitched = 1 },
};

// Kalimba (GM 109)
static const GMModalParams gm_kalimba_program = {
    .name = "kalimba", .nmodes = 3, .ratio = {1.0, 5.4, 14.7},
    .amp = {1.0, 0.35, 0.12}, .t60 = {0.8, 0.18, 0.06},
    .strike_amp = 0.07, .strike_ms = 2.0, .pitched = 1
};

// ── Guitar family (GM 25-32) ──
#define GM_GUITAR_FIRST 24
#define GM_GUITAR_COUNT 8
static const GMProgramParams gm_guitar_programs[GM_GUITAR_COUNT] = {
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9975, .ks_loop_b = 0.30,
      .ks_beta = 0.13, .ks_pick = 0.85, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.7, .ks_attack_amp = 0.04, .ks_attack_ms = 4.0,
      .bodyf = {100.0, 200.0}, .bodyq = {6.0, 8.0}, .bodyg = {0.12, 0.07} },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9982, .ks_loop_b = 0.16,
      .ks_beta = 0.10, .ks_pick = 0.95, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.2, .ks_attack_amp = 0.07, .ks_attack_ms = 2.5,
      .bodyf = {110.0, 220.0, 400.0}, .bodyq = {7.0, 9.0, 10.0},
      .bodyg = {0.14, 0.09, 0.05} },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9985, .ks_loop_b = 0.34,
      .ks_beta = 0.18, .ks_pick = 0.80, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.45, .ks_attack_amp = 0.05, .ks_attack_ms = 3.0 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9980, .ks_loop_b = 0.14,
      .ks_beta = 0.12, .ks_pick = 0.90, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.25, .ks_attack_amp = 0.06, .ks_attack_ms = 2.5 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.985, .ks_loop_b = 0.55,
      .ks_beta = 0.12, .ks_pick = 0.90, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.3, .ks_attack_amp = 0.08, .ks_attack_ms = 2.0 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9988, .ks_loop_b = 0.12,
      .ks_beta = 0.11, .ks_pick = 0.90, .ks_drive = 0.6,
      .ks_big = 1, .ks_exc_smooth = 0.2, .ks_attack_amp = 0.06, .ks_attack_ms = 2.5 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9991, .ks_loop_b = 0.10,
      .ks_beta = 0.10, .ks_pick = 0.90, .ks_drive = 1.0, .ks_hard = 1,
      .ks_big = 1, .ks_exc_smooth = 0.15, .ks_attack_amp = 0.05, .ks_attack_ms = 2.5 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9990, .ks_loop_b = 0.10,
      .ks_beta = 0.5, .ks_pick = 1.0, .ks_drive = 0.2,
      .ks_big = 1, .ks_exc_smooth = 0.4, .ks_attack_amp = 0.03, .ks_attack_ms = 2.0 },
};

// ── Bass plucked (GM 33-38) ──
#define GM_BASS_FIRST 32
#define GM_BASS_PLUCK_COUNT 6
static const GMProgramParams gm_bass_programs[GM_BASS_PLUCK_COUNT] = {
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.996, .ks_loop_b = 0.45,
      .ks_beta = 0.35, .ks_pick = 0.70, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.85, .ks_attack_amp = 0.05, .ks_attack_ms = 6.0,
      .bodyf = {100.0}, .bodyq = {5.0}, .bodyg = {0.15} },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.998, .ks_loop_b = 0.28,
      .ks_beta = 0.25, .ks_pick = 0.80, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.6, .ks_attack_amp = 0.05, .ks_attack_ms = 4.0 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.997, .ks_loop_b = 0.18,
      .ks_beta = 0.12, .ks_pick = 0.92, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.2, .ks_attack_amp = 0.10, .ks_attack_ms = 1.5 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9985, .ks_loop_b = 0.36,
      .ks_beta = 0.30, .ks_pick = 0.78, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.7, .ks_attack_amp = 0.04, .ks_attack_ms = 5.0 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.997, .ks_loop_b = 0.20,
      .ks_beta = 0.20, .ks_pick = 0.88, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.15, .ks_attack_amp = 0.22, .ks_attack_ms = 5.0,
      .ks_attack_bp = 14.0 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.995, .ks_loop_b = 0.16,
      .ks_beta = 0.10, .ks_pick = 0.92, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.10, .ks_attack_amp = 0.24, .ks_attack_ms = 3.0,
      .ks_attack_bp = 22.0, .ks_sec_ms = 10.0, .ks_sec_amp = 0.5 },
};

// ── Ethnic plucked (GM 105-108) ──
#define GM_ETHNIC_FIRST 104
static const GMProgramParams gm_ethnic_pluck_programs[4] = {
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9985, .ks_loop_b = 0.14,
      .ks_beta = 0.12, .ks_pick = 0.92, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.2, .ks_jawari = 0.5,
      .ks_attack_amp = 0.05, .ks_attack_ms = 2.5 },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.992, .ks_loop_b = 0.10,
      .ks_beta = 0.14, .ks_pick = 0.92, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.15, .ks_attack_amp = 0.08, .ks_attack_ms = 2.0,
      .bodyf = {300.0, 480.0, 720.0}, .bodyq = {5.0, 7.0, 8.0},
      .bodyg = {0.16, 0.10, 0.06} },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.993, .ks_loop_b = 0.18,
      .ks_beta = 0.15, .ks_pick = 0.90, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.1, .ks_jawari = 0.2,
      .ks_attack_amp = 0.14, .ks_attack_ms = 2.0,
      .bodyf = {250.0, 520.0}, .bodyq = {4.0, 6.0}, .bodyg = {0.12, 0.06} },
    { .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9982, .ks_loop_b = 0.26,
      .ks_beta = 0.18, .ks_pick = 0.85, .ks_drive = 0.0,
      .ks_big = 1, .ks_exc_smooth = 0.5, .ks_attack_amp = 0.04, .ks_attack_ms = 3.5 },
};

// ── Subtractive (SYNTHBASS): Synth Bass 1/2 (GM 39-40) + reed approximations ──
typedef struct {
    int    program;
    GMProgramParams p;
} GMSynthBassRow;
static const GMSynthBassRow gm_synthbass_programs[] = {
    { 38, { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = -7.0, .sb_o2_mix = 0.7,
            .sb_sub = 0.4, .sb_cut0 = 2600.0, .sb_cut1 = 380.0, .sb_cut_ms = 150.0,
            .sb_res = 0.55, .sb_psweep = 1.12, .sb_psweep_ms = 10.0,
            .sb_sustained = 0 } },
    { 39, { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 0.0, .sb_o2_mix = 0.6,
            .sb_o2_sq = 1, .sb_sub = 0.3, .sb_fm0 = 1.4, .sb_fm_ms = 80.0,
            .sb_cut0 = 3200.0, .sb_cut1 = 320.0, .sb_cut_ms = 110.0,
            .sb_res = 0.7, .sb_psweep = 1.12, .sb_psweep_ms = 8.0,
            .sb_sustained = 0 } },
    { 109, { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 4.0, .sb_o2_mix = 0.5,
             .sb_cut0 = 4200.0, .sb_cut1 = 4200.0, .sb_cut_ms = 5.0, .sb_res = 0.3,
             .sb_psweep = 1.0, .sb_sustained = 1, .sb_drone_mix = 0.5,
             .sb_breath = 0.10 } },
    { 110, { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 3.0, .sb_o2_mix = 0.4,
             .sb_cut0 = 3200.0, .sb_cut1 = 3000.0, .sb_cut_ms = 40.0, .sb_res = 0.4,
             .sb_psweep = 1.0, .sb_sustained = 1, .sb_vib_hz = 5.5,
             .sb_vib_depth = 0.006, .sb_breath = 0.04 } },
    { 111, { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 6.0, .sb_o2_mix = 0.45,
             .sb_fm0 = 0.6, .sb_fm_ms = 400.0, .sb_cut0 = 5000.0, .sb_cut1 = 4600.0,
             .sb_cut_ms = 30.0, .sb_res = 0.45, .sb_psweep = 1.0, .sb_sustained = 1,
             .sb_vib_hz = 6.0, .sb_vib_depth = 0.01, .sb_breath = 0.12 } },
};
#define GM_SYNTHBASS_ROWS (int)(sizeof(gm_synthbass_programs)/sizeof(gm_synthbass_programs[0]))

// ── Batch-2 note-on helpers ──

// Set up up-to-3 parallel body-resonance band-pass biquads.
static void gm_setup_body_resonance(GMVoice *v, const GMProgramParams *p,
                                    double sr, double mul) {
    v->ks_body_n = 0;
    for (int i = 0; i < 3; i++) {
        if (p->bodyf[i] <= 0.0) break;
        double f = p->bodyf[i];
        if (f > sr * 0.45) f = sr * 0.45;
        double Q = p->bodyq[i] > 0.1 ? p->bodyq[i] : 1.0;
        double w0 = 2.0 * M_PI * f / sr;
        double alpha = sin(w0) / (2.0 * Q);
        double a0 = 1.0 + alpha;
        v->ks_body_a1[i] = (-2.0 * cos(w0)) / a0;
        v->ks_body_a2[i] = (1.0 - alpha) / a0;
        v->ks_body_g[i]  = voice_jitter(v, p->bodyg[i], 0.15, mul) * (alpha / a0);
        v->ks_body_y1[i] = 0.0;
        v->ks_body_y2[i] = 0.0;
        v->ks_body_n = i + 1;
    }
}

// Modal-bank note-on.
static void gm_modal_init(GMVoice *v, const GMModalParams *m, double f0,
                          double sr) {
    const double mul = 1.0;
    int N = m->nmodes;
    if (N > GM_MAX_PARTIALS) N = GM_MAX_PARTIALS;
    if (N > GM_MODAL_MAX_MODES) N = GM_MODAL_MAX_MODES;
    if (N < 1) N = 1;
    v->p_count = N;
    v->gm_dual = 0;
    v->gm_drive = 0.0;
    v->gm_modal_pitched = m->pitched;
    v->gm_modal_bloom = m->bloom;
    v->gm_modal_fund = 0.0;
    double norm = 0.0;
    for (int k = 0; k < N; k++) norm += m->amp[k];
    if (norm < 1e-6) norm = 1.0;
    norm = 1.0 / norm;
    for (int k = 0; k < N; k++) {
        double fk = f0 * m->ratio[k];
        fk = voice_detune(v, fk, 6.0, mul);
        if (fk > sr * 0.45) fk = sr * 0.45;
        v->p_finc[k] = fk / sr;
        v->p_amp[k]  = voice_jitter(v, m->amp[k] * norm, 0.15, mul);
        v->p_phase[k] = 0.0;
        double tau = voice_jitter(v, m->t60[k], 0.15, mul);
        if (tau < 0.01) tau = 0.01;
        v->p_dec_mult[k] = exp(-1.0 / (tau * sr));
    }
    v->gm_trem_depth = m->trem_depth;
    v->gm_trem_phase = voice_rand_phase(v);
    v->gm_trem_inc   = (m->trem_hz > 0.0) ? (m->trem_hz / sr) : 0.0;
    double stau = m->strike_ms * 0.001;  if (stau < 0.0005) stau = 0.0005;
    v->gm_hammer_amp = voice_jitter(v, m->strike_amp, 0.15, mul);
    v->gm_hammer_env = 1.0;
    v->gm_hammer_dec = exp(-1.0 / (stau * sr));
    v->gm_hammer_lp  = 0.0;
}

// Long-KS note-on (guitar/bass/ethnic).
static void gm_ks_big_init(GMVoice *v, const GMProgramParams *p, double f0,
                           double sr) {
    const double mul = 0.9;
    v->harp_lp1 = 0.0;
    v->ks_use_big = 1;
    v->ks_hard_clip = p->ks_hard;
    v->ks_stretch = p->ks_stretch;
    v->ks_loop_b  = p->ks_loop_b;
    v->ks_beta    = voice_jitter(v, p->ks_beta, 0.08, mul);
    v->ks_pick_amt = p->ks_pick;
    v->ks_drive   = p->ks_drive;
    v->ks_jawari_depth  = p->ks_jawari;
    v->ks_jawari_thresh = 0.25;
    gm_setup_body_resonance(v, p, sr, mul);
    memset(v->ks_buf, 0, sizeof(v->ks_buf));
    double string_delay = sr / f0;
    if (string_delay > (double)(GM_KS_BIG_N - 2)) string_delay = (double)(GM_KS_BIG_N - 2);
    if (string_delay < 2.0) string_delay = 2.0;
    int n = (int)string_delay;
    double smooth = p->ks_exc_smooth;  if (smooth < 0.0) smooth = 0.0; if (smooth > 1.0) smooth = 1.0;
    double last = 0.0;
    for (int i = 0; i < n; i++) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double filt = (1.0 - smooth) * white + smooth * last;
        last = filt;
        v->ks_buf[i] = (float)filt;
    }
    int beta_tap = (int)(v->ks_beta * (double)n + 0.5);
    if (beta_tap >= 1 && beta_tap < n) {
        for (int i = n - 1; i >= beta_tap; i--) {
            v->ks_buf[i] -= (float)(v->ks_pick_amt * (double)v->ks_buf[i - beta_tap]);
        }
    }
    v->ks_w = n;
    if (p->ks_attack_amp > 0.0) {
        double atau = p->ks_attack_ms * 0.001;  if (atau < 0.0003) atau = 0.0003;
        v->atk_env = voice_jitter(v, p->ks_attack_amp, 0.15, mul);
        v->atk_dec = exp(-1.0 / (atau * sr));
        double cf = (p->ks_attack_bp > 0.0) ? (f0 * p->ks_attack_bp) : 1200.0;
        if (cf > sr * 0.45) cf = sr * 0.45;
        if (cf < 80.0) cf = 80.0;
        double Q = (p->ks_attack_bp > 0.0) ? 3.0 : 0.707;
        double w0 = 2.0 * M_PI * cf / sr;
        double al = sin(w0) / (2.0 * Q);
        double a0 = 1.0 + al;
        if (p->ks_attack_bp > 0.0) {
            v->nb0 = (al) / a0; v->nb1 = 0.0; v->nb2 = (-al) / a0;
        } else {
            double c = (1.0 - cos(w0));
            v->nb0 = (c / 2.0) / a0; v->nb1 = c / a0; v->nb2 = (c / 2.0) / a0;
        }
        v->na1 = (-2.0 * cos(w0)) / a0;
        v->na2 = (1.0 - al) / a0;
        v->nx1 = v->nx2 = v->ny1 = v->ny2 = 0.0;
    } else {
        v->atk_env = 0.0;
        v->atk_dec = 0.0;
    }
    if (p->ks_sec_ms > 0.0) {
        v->sec_trig = (p->ks_sec_ms * 0.001) * sr;
        v->sec_amp  = p->ks_sec_amp;
    } else {
        v->sec_trig = -1.0;
        v->sec_amp  = 0.0;
    }
}

// Subtractive note-on.
static void gm_synthbass_init(GMVoice *v, const GMProgramParams *p, double f0,
                              double sr) {
    const double mul = 0.7;
    double fc = voice_detune(v, f0, 6.0, mul);
    v->sb_o1_phase = 0.0; v->sb_o1_inc = fc / sr;
    if (p->sb_o2_mix > 0.0) {
        double f2 = fc * cents_to_ratio(p->sb_o2_cents);
        v->sb_o2_phase = voice_rand_phase(v);
        v->sb_o2_inc   = f2 / sr;
        v->sb_o2_square = p->sb_o2_sq;
        v->sb_o2_mix   = p->sb_o2_mix;
    } else {
        v->sb_o2_inc = 0.0; v->sb_o2_mix = 0.0; v->sb_o2_square = 0;
        v->sb_o2_phase = 0.0;
    }
    v->sb_sub_phase = voice_rand_phase(v);
    v->sb_sub_inc   = (fc * 0.5) / sr;
    v->sb_sub_mix   = p->sb_sub;
    if (p->sb_fm0 > 0.0) {
        v->sb_fm_index = voice_jitter(v, p->sb_fm0, 0.06, mul);
        double fdec = p->sb_fm_ms * 0.001;  if (fdec < 0.001) fdec = 0.001;
        v->sb_fm_dec = exp(-1.0 / (fdec * sr));
    } else {
        v->sb_fm_index = 0.0; v->sb_fm_dec = 1.0;
    }
    v->sb_lp1 = v->sb_lp2 = 0.0;
    v->sb_bp1 = v->sb_bp2 = 0.0;
    v->sb_cut = p->sb_cut0;
    v->sb_cut_target = p->sb_cut1;
    double cms = p->sb_cut_ms * 0.001;  if (cms < 0.0005) cms = 0.0005;
    v->sb_cut_dec = exp(-1.0 / (cms * sr));
    v->sb_res = voice_jitter(v, p->sb_res, 0.05, mul);
    v->sb_pitch_mult = p->sb_psweep > 0.0 ? p->sb_psweep : 1.0;
    double pms = p->sb_psweep_ms * 0.001;  if (pms < 0.0005) pms = 0.0005;
    v->sb_pitch_dec = (p->sb_psweep > 1.0) ? exp(-1.0 / (pms * sr)) : 1.0;
    v->sb_sustain = p->sb_sustained;
    if (p->sb_drone_mix > 0.0) {
        v->sb_drone_phase = voice_rand_phase(v);
        v->sb_drone_inc   = (fc * 0.5) / sr;
        v->sb_drone_mix   = p->sb_drone_mix;
    } else {
        v->sb_drone_inc = 0.0; v->sb_drone_mix = 0.0; v->sb_drone_phase = 0.0;
    }
    v->sb_breath_lp = 0.0;
    v->sb_breath_amt = p->sb_breath;
    if (p->sb_vib_hz > 0.0) {
        v->sb_vib_phase = voice_rand_phase(v);
        double vhz = voice_jitter(v, p->sb_vib_hz, 0.10, mul);
        v->sb_vib_inc = vhz / sr;
        v->sb_vib_depth = p->sb_vib_depth;
    } else {
        v->sb_vib_inc = 0.0; v->sb_vib_depth = 0.0; v->sb_vib_phase = 0.0;
    }
}

// Forward declare batch-2 dispatch (defined after gm_voice_init).
static int gm_voice_init_batch2(GMVoice *v, int program, double freq,
                                double sample_rate);

int gm_program_implemented(int program) {
    if (program < 0) return 0;
    if (program < GM_PIANO_PROGRAM_COUNT) return 1;           // 0-7
    if (program >= 8 && program <= 15) return 1;              // Chromatic Perc
    if (program >= 24 && program <= 31) return 1;             // Guitar
    if (program >= 32 && program <= 39) return 1;             // Bass
    if (program >= 104 && program <= 111) return 1;           // Ethnic
    if (program >= 112 && program <= 119) return 1;           // Percussive
    return 0;
}

int gm_voice_init(GMVoice *v, int program, double freq, double sample_rate,
                  uint32_t seed) {
    gm_synth_init();
    if (!v) return -1;
    // Clear all per-voice state so a recycled GMVoice never carries stale fields.
    memset(v, 0, sizeof(*v));
    v->rng_seed = seed;
    v->engine = GM_ENGINE_NONE;
    if (program < 0) return -1;
    // Programs 8+ route to the batch-2 families.
    if (program >= GM_PIANO_PROGRAM_COUNT) return gm_voice_init_batch2(v, program, freq, sample_rate);
    const GMProgramParams *p = &gm_piano_programs[program];
    double sr = sample_rate > 0.0 ? sample_rate : GM_FALLBACK_SR;
    double f0 = freq < 20.0 ? 20.0 : freq;
    v->program = program;
    v->engine = p->engine;

    if (p->engine == GM_ENGINE_GMPIANO) {
        const double mul = 0.8;
        int N = p->partials;
        if (N > GM_MAX_PARTIALS) N = GM_MAX_PARTIALS;
        if (N < 1) N = 1;
        v->p_count = N;
        v->gm_dual = (p->dual_cents > 0.0) ? 1 : 0;
        v->gm_drive = p->drive;
        double norm = 1.0 / sqrt((double)N);
        double sum_check = 0.0;
        for (int k = 0; k < N; k++) {
            int n = k + 1;
            double fn = (double)n * f0 * sqrt(1.0 + p->B * (double)(n * n));
            fn = voice_detune(v, fn, 6.0, mul);
            if (fn > sr * 0.45) fn = sr * 0.45;
            v->p_finc[k] = fn / sr;
            double a = norm / (double)n;
            if (k >= (int)p->tilt_from) a *= p->partial_tilt;
            a = voice_jitter(v, a, 0.15, mul);
            v->p_amp[k] = a;
            sum_check += a;
            v->p_phase[k] = voice_rand_phase(v);
            double tau = p->tau0 / (1.0 + 0.6 * (double)(n - 1));
            tau = voice_jitter(v, tau, 0.15, mul);
            if (tau < 0.02) tau = 0.02;
            v->p_dec_mult[k] = exp(-1.0 / (tau * sr));
            if (v->gm_dual) {
                double f2 = fn * cents_to_ratio(p->dual_cents);
                if (f2 > sr * 0.45) f2 = sr * 0.45;
                v->p2_finc[k] = f2 / sr;
                v->p2_phase[k] = voice_rand_phase(v);
            }
        }
        (void)sum_check;
        double htau = (p->hammer_ms * 0.001);
        if (htau < 0.0005) htau = 0.0005;
        v->gm_hammer_amp = voice_jitter(v, p->hammer_amp, 0.15, mul);
        v->gm_hammer_env = 1.0;
        v->gm_hammer_dec = exp(-1.0 / (htau * sr));
        v->gm_hammer_lp = 0.0;
    } else if (p->engine == GM_ENGINE_EPIANO) {
        const double mul = 0.7;
        double fc = voice_detune(v, f0, 6.0, mul);
        double fm = voice_detune(v, f0 * p->fm_ratio, 2.0, mul);
        double ft = f0 * p->fm_tine_ratio;
        if (fc > sr * 0.45) fc = sr * 0.45;
        v->fm_cphase = 0.0; v->fm_cinc = fc / sr;
        v->fm_mphase = 0.0; v->fm_minc = fm / sr;
        v->fm_tphase = 0.0; v->fm_tinc = ft / sr;
        v->fm_index = voice_jitter(v, p->fm_index0, 0.06, mul);
        v->fm_tindex = voice_jitter(v, p->fm_tine_index0, 0.06, mul);
        double idec = p->fm_index_ms * 0.001;  if (idec < 0.001) idec = 0.001;
        double tdec = p->fm_tine_ms * 0.001;   if (tdec < 0.0005) tdec = 0.0005;
        v->fm_index_dec  = exp(-1.0 / (idec * sr));
        v->fm_tindex_dec = exp(-1.0 / (tdec * sr));
        v->fm_pickup_bias = p->fm_pickup;
        v->fm_hp_x1 = 0.0; v->fm_hp_y1 = 0.0;
    } else if (p->engine == GM_ENGINE_PLUCK) {
        const double mul = 0.9;
        v->harp_lp1 = 0.0;
        v->ks_use_big = 0;
        v->ks_stretch = p->ks_stretch;
        v->ks_loop_b  = p->ks_loop_b;
        v->ks_beta = voice_jitter(v, p->ks_beta, 0.08, mul);
        v->ks_pick_amt = p->ks_pick;
        v->ks_drive = p->ks_drive;
        memset(v->bore_buf, 0, sizeof(v->bore_buf));
        double string_delay = sr / f0;
        const int STRING_N = GM_BORE_N;
        if (string_delay > (double)(STRING_N - 2)) string_delay = (double)(STRING_N - 2);
        if (string_delay < 2.0) string_delay = 2.0;
        int n = (int)string_delay;
        double last = 0.0;
        for (int i = 0; i < n; i++) {
            double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            double filt = 0.75 * white + 0.25 * last;
            last = white;
            v->bore_buf[i] = (float)filt;
        }
        v->bore_w = n;
    }
    return 0;
}

static int gm_voice_init_batch2(GMVoice *v, int program, double freq,
                                double sample_rate) {
    double sr = sample_rate > 0.0 ? sample_rate : GM_FALLBACK_SR;
    double f0 = freq < 20.0 ? 20.0 : freq;
    v->program = program;

    // ── Chromatic Percussion (GM 9-15 → modal; GM 16 Dulcimer → KS) ──
    if (program >= GM_CHROMPERC_FIRST &&
        program < GM_CHROMPERC_FIRST + GM_CHROMPERC_COUNT) {
        v->engine = GM_ENGINE_MODAL;
        gm_modal_init(v, &gm_chromperc_programs[program - GM_CHROMPERC_FIRST], f0, sr);
        return 0;
    }
    if (program == 15) {
        GMProgramParams dul = {
            .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9990, .ks_loop_b = 0.14,
            .ks_beta = 0.18, .ks_pick = 0.85, .ks_drive = 0.0,
            .ks_big = 1, .ks_exc_smooth = 0.25,
            .ks_attack_amp = 0.10, .ks_attack_ms = 2.0
        };
        v->engine = GM_ENGINE_PLUCK;
        gm_ks_big_init(v, &dul, f0, sr);
        return 0;
    }

    // ── Guitar (GM 25-32 → extended KS) ──
    if (program >= GM_GUITAR_FIRST &&
        program < GM_GUITAR_FIRST + GM_GUITAR_COUNT) {
        v->engine = GM_ENGINE_PLUCK;
        gm_ks_big_init(v, &gm_guitar_programs[program - GM_GUITAR_FIRST], f0, sr);
        return 0;
    }

    // ── Bass (GM 33-38 → KS, GM 39-40 → subtractive) ──
    if (program >= GM_BASS_FIRST &&
        program < GM_BASS_FIRST + GM_BASS_PLUCK_COUNT) {
        v->engine = GM_ENGINE_PLUCK;
        gm_ks_big_init(v, &gm_bass_programs[program - GM_BASS_FIRST], f0, sr);
        return 0;
    }

    // ── Ethnic plucked (GM 105-108) + Kalimba (109 → modal) ──
    if (program >= GM_ETHNIC_FIRST && program <= GM_ETHNIC_FIRST + 3) {
        v->engine = GM_ENGINE_PLUCK;
        gm_ks_big_init(v, &gm_ethnic_pluck_programs[program - GM_ETHNIC_FIRST], f0, sr);
        return 0;
    }
    if (program == 108) {  // GM 109 — Kalimba (modal tine)
        v->engine = GM_ENGINE_MODAL;
        gm_modal_init(v, &gm_kalimba_program, f0, sr);
        return 0;
    }

    // ── Percussive (GM 113-119 → modal/membrane) ──
    if (program >= GM_PERC_FIRST &&
        program < GM_PERC_FIRST + GM_PERC_COUNT) {
        v->engine = GM_ENGINE_MODAL;
        gm_modal_init(v, &gm_perc_programs[program - GM_PERC_FIRST], f0, sr);
        (void)f0;
        return 0;
    }

    // ── Subtractive: Synth Bass 1/2 (39-40) + reed approximations (110-112) ──
    for (int i = 0; i < GM_SYNTHBASS_ROWS; i++) {
        if (gm_synthbass_programs[i].program == program) {
            v->engine = GM_ENGINE_SYNTHBASS;
            gm_synthbass_init(v, &gm_synthbass_programs[i].p, f0, sr);
            return 0;
        }
    }

    v->engine = GM_ENGINE_NONE;
    return -1;   // not implemented in this batch
}

// ============================================================
// Generators (operate on GMVoice; env + frequency supplied by host)
// ============================================================

static inline double generate_gmpiano_sample(GMVoice *v, double sample_rate,
                                              double env) {
    (void)sample_rate;
    double s = 0.0;
    int N = v->p_count;
    for (int k = 0; k < N; k++) {
        double a = v->p_amp[k];
        s += a * wt_sin(v->p_phase[k]);
        v->p_phase[k] += v->p_finc[k];
        if (v->p_phase[k] >= 1.0) v->p_phase[k] -= 1.0;
        if (v->gm_dual) {
            s += a * wt_sin(v->p2_phase[k]);
            v->p2_phase[k] += v->p2_finc[k];
            if (v->p2_phase[k] >= 1.0) v->p2_phase[k] -= 1.0;
        }
        v->p_amp[k] *= v->p_dec_mult[k];
    }
    if (v->gm_dual) s *= 0.5;
    if (v->gm_hammer_env > 0.0001) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        v->gm_hammer_lp = 0.2 * white + 0.8 * v->gm_hammer_lp;
        s += v->gm_hammer_amp * v->gm_hammer_env * v->gm_hammer_lp;
        v->gm_hammer_env *= v->gm_hammer_dec;
    }
    if (v->gm_drive > 0.0) {
        double pre = 1.0 + 4.0 * v->gm_drive;
        s = tanh(pre * s) * (1.0 / pre) * (1.0 + v->gm_drive);
    }
    return s * env;
}

static inline double generate_epiano_sample(GMVoice *v, double sample_rate,
                                             double env) {
    (void)sample_rate;
    double bodymod = wt_sin(v->fm_mphase) * v->fm_index;
    double tinemod = wt_sin(v->fm_tphase) * v->fm_tindex;
    double car = wt_sin(v->fm_cphase + bodymod + tinemod);
    v->fm_cphase += v->fm_cinc;  if (v->fm_cphase >= 1.0) v->fm_cphase -= 1.0;
    v->fm_mphase += v->fm_minc;  if (v->fm_mphase >= 1.0) v->fm_mphase -= 1.0;
    v->fm_tphase += v->fm_tinc;  if (v->fm_tphase >= 1.0) v->fm_tphase -= 1.0;
    v->fm_index  *= v->fm_index_dec;
    v->fm_tindex *= v->fm_tindex_dec;
    double x = car;
    if (v->fm_pickup_bias > 0.0) {
        double biased = tanh(x + v->fm_pickup_bias);
        double y = biased - v->fm_hp_x1 + 0.999 * v->fm_hp_y1;
        v->fm_hp_x1 = biased;
        v->fm_hp_y1 = y;
        x = y;
    }
    return x * env;
}

static inline double generate_pluck_sample(GMVoice *v, double sample_rate,
                                           double env, double frequency) {
    float *buf;  int N;  int *wptr;
    double fmin;
    if (v->ks_use_big) {
        buf = v->ks_buf;  N = GM_KS_BIG_N;  wptr = &v->ks_w;  fmin = 25.0;
    } else {
        buf = v->bore_buf;  N = GM_BORE_N;  wptr = &v->bore_w;  fmin = 50.0;
    }
    double freq = clampd(frequency, fmin, sample_rate * 0.20);
    double string_delay = sample_rate / freq;
    if (string_delay > (double)(N - 2)) string_delay = (double)(N - 2);
    if (string_delay < 2.0) string_delay = 2.0;

    double delayed = gm_frac_read(buf, N, *wptr, string_delay);
    double tap_delay = v->ks_beta * string_delay;
    if (tap_delay < 1.0) tap_delay = 1.0;
    double picked = delayed - v->ks_pick_amt *
        gm_frac_read(buf, N, *wptr, tap_delay);
    double damp = (1.0 - v->ks_loop_b) * picked + v->ks_loop_b * v->harp_lp1;
    v->harp_lp1 = damp;
    double y = v->ks_stretch * damp;
    if (v->ks_jawari_depth > 0.0) {
        double a = fabs(y);
        if (a > v->ks_jawari_thresh) {
            double over = (a - v->ks_jawari_thresh);
            double buzz = v->ks_jawari_depth * tanh(6.0 * over) * (y < 0.0 ? -1.0 : 1.0);
            y += buzz;
        }
    }
    buf[*wptr] = (float)y;
    *wptr = (*wptr + 1) % N;

    double s = y;
    if (v->atk_env > 0.0001) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double nf = v->nb0 * white + v->nb1 * v->nx1 + v->nb2 * v->nx2
                    - v->na1 * v->ny1 - v->na2 * v->ny2;
        v->nx2 = v->nx1; v->nx1 = white;
        v->ny2 = v->ny1; v->ny1 = nf;
        s += v->atk_env * nf;
        v->atk_env *= v->atk_dec;
    }
    if (v->sec_trig > 0.0) {
        v->sec_trig -= 1.0;
        if (v->sec_trig <= 0.0) {
            double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
            buf[*wptr] += (float)(v->sec_amp * white);
            v->sec_trig = -1.0;
        }
    }

    double out = s;
    if (v->ks_drive > 0.0) {
        double pre = 1.0 + (v->ks_hard_clip ? 9.0 : 4.0) * v->ks_drive;
        out = tanh(pre * s) * (1.0 / pre) * (1.0 + v->ks_drive);
    }
    for (int i = 0; i < v->ks_body_n; i++) {
        double by = v->ks_body_g[i] * out
                    - v->ks_body_a1[i] * v->ks_body_y1[i]
                    - v->ks_body_a2[i] * v->ks_body_y2[i];
        v->ks_body_y2[i] = v->ks_body_y1[i];
        v->ks_body_y1[i] = by;
        out += by;
    }
    return 2.5 * out * env;
}

static inline double generate_modal_sample(GMVoice *v, double sample_rate,
                                           double env) {
    (void)sample_rate;
    double s = 0.0;
    int N = v->p_count;
    double fund = 0.0;
    for (int k = 0; k < N; k++) {
        double a = v->p_amp[k];
        double partial = a * wt_sin(v->p_phase[k]);
        s += partial;
        if (k == 0) fund = partial;
        v->p_phase[k] += v->p_finc[k];
        if (v->p_phase[k] >= 1.0) v->p_phase[k] -= 1.0;
        v->p_amp[k] *= v->p_dec_mult[k];
    }
    if (v->gm_modal_bloom > 0.0) {
        v->gm_modal_fund = 0.99 * v->gm_modal_fund + 0.01 * (fund * fund);
        s += v->gm_modal_bloom * v->gm_modal_fund * 4.0;
    }
    if (v->gm_hammer_env > 0.0001) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        v->gm_hammer_lp = 0.3 * white + 0.7 * v->gm_hammer_lp;
        s += v->gm_hammer_amp * v->gm_hammer_env * v->gm_hammer_lp;
        v->gm_hammer_env *= v->gm_hammer_dec;
    }
    if (v->gm_trem_inc > 0.0) {
        double trem = 1.0 - v->gm_trem_depth * 0.5 * (1.0 - wt_sin(v->gm_trem_phase));
        v->gm_trem_phase += v->gm_trem_inc;
        if (v->gm_trem_phase >= 1.0) v->gm_trem_phase -= 1.0;
        s *= trem;
    }
    return s * env;
}

static inline double generate_synthbass_sample(GMVoice *v, double sample_rate,
                                                double env) {
    double vib = 1.0;
    if (v->sb_vib_inc > 0.0) {
        vib = 1.0 + v->sb_vib_depth * wt_sin(v->sb_vib_phase);
        v->sb_vib_phase += v->sb_vib_inc;
        if (v->sb_vib_phase >= 1.0) v->sb_vib_phase -= 1.0;
    }
    double pmult = v->sb_pitch_mult * vib;

    double fmod = 0.0;
    int has_fm = (v->sb_fm_dec < 1.0 && v->sb_fm_index > 0.00001);
    if (has_fm) {
        fmod = v->sb_fm_index * wt_sin(v->sb_o1_phase);
        v->sb_fm_index *= v->sb_fm_dec;
    }

    double o1 = has_fm ? wt_sin(v->sb_o1_phase + fmod) : (2.0 * v->sb_o1_phase - 1.0);
    v->sb_o1_phase += v->sb_o1_inc * pmult;
    if (v->sb_o1_phase >= 1.0) v->sb_o1_phase -= 1.0;

    double sig = o1;
    if (v->sb_o2_mix > 0.0) {
        double o2;
        if (v->sb_o2_square) {
            o2 = v->sb_o2_phase < 0.5 ? 1.0 : -1.0;
        } else {
            o2 = 2.0 * v->sb_o2_phase - 1.0;
        }
        sig += v->sb_o2_mix * o2;
        v->sb_o2_phase += v->sb_o2_inc * pmult;
        if (v->sb_o2_phase >= 1.0) v->sb_o2_phase -= 1.0;
    }
    if (v->sb_sub_mix > 0.0) {
        sig += v->sb_sub_mix * wt_sin(v->sb_sub_phase);
        v->sb_sub_phase += v->sb_sub_inc * pmult;
        if (v->sb_sub_phase >= 1.0) v->sb_sub_phase -= 1.0;
    }
    if (v->sb_drone_mix > 0.0) {
        sig += v->sb_drone_mix * (2.0 * v->sb_drone_phase - 1.0);
        v->sb_drone_phase += v->sb_drone_inc;
        if (v->sb_drone_phase >= 1.0) v->sb_drone_phase -= 1.0;
    }
    if (v->sb_breath_amt > 0.0) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        v->sb_breath_lp = 0.15 * white + 0.85 * v->sb_breath_lp;
        sig += v->sb_breath_amt * v->sb_breath_lp;
    }
    sig *= 0.5;

    if (v->sb_pitch_dec < 1.0) {
        v->sb_pitch_mult = 1.0 + (v->sb_pitch_mult - 1.0) * v->sb_pitch_dec;
    }

    v->sb_cut = v->sb_cut_target + (v->sb_cut - v->sb_cut_target) * v->sb_cut_dec;
    double fc = clampd(v->sb_cut, 30.0, sample_rate * 0.45);
    double g = 1.0 - exp(-2.0 * M_PI * fc / sample_rate);
    double in = sig - v->sb_res * 4.0 * (v->sb_lp2 - v->sb_bp2);
    v->sb_lp1 += g * (in - v->sb_lp1);
    v->sb_lp2 += g * (v->sb_lp1 - v->sb_lp2);
    v->sb_bp2 = v->sb_lp1;
    double out = v->sb_lp2;
    return clampd(out, -1.5, 1.5) * env;
}

double gm_voice_render(GMVoice *v, double sample_rate, double env,
                       double frequency) {
    switch (v->engine) {
    case GM_ENGINE_GMPIANO:   return generate_gmpiano_sample(v, sample_rate, env);
    case GM_ENGINE_EPIANO:    return generate_epiano_sample(v, sample_rate, env);
    case GM_ENGINE_PLUCK:     return generate_pluck_sample(v, sample_rate, env, frequency);
    case GM_ENGINE_MODAL:     return generate_modal_sample(v, sample_rate, env);
    case GM_ENGINE_SYNTHBASS: return generate_synthbass_sample(v, sample_rate, env);
    default:                  return 0.0;
    }
}
