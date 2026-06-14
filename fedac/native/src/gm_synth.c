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
    // -- Digital waveguide (WAVEGUIDE): bowed / brass / reed / flute --
    int    wg_mode;            // GMWaveguideMode
    double wg_loop_damp;       // loop-loss coeff (darker = larger; bigger bore)
    double wg_breath_max;      // pressure/bow-speed ceiling
    double wg_noise;           // turbulence / bow-grind / chiff
    double wg_attack_ms;       // onset ramp (slow bow / soft horn)
    double wg_vib_hz;          // vibrato rate
    double wg_vib_depth;       // vibrato depth (fraction of bore delay)
    double wg_bow_beta;        // BOWED: bow position
    double wg_bow_slope;       // BOWED: bow force
    double wg_lip_pole;        // LIP: lip-resonance pole radius (0.997 trumpet)
    double wg_lip_gain;        // LIP: lip filter gain
    double wg_reed_offset;     // REED: STK reed table offset (0.6)
    double wg_reed_slope;      // REED: STK reed table slope (-0.8)
    int    wg_bore_invert;     // REED: 1 = clarinet cylinder (odd harmonics)
    double wg_jet_ratio;       // JET: jet delay / bore delay
    double wg_fmt_f;           // output formant/mute bandpass freq (0 = none)
    double wg_fmt_q;           // output formant Q
    double wg_fmt_gain;        // output formant mix
    double wg_out_lp_hz;       // output LP cutoff (mute/dark bell; 0 = bypass)
    double wg_out_scale;       // output gain
    double wg_trem_hz;         // section/tremolo amplitude LFO rate (0 = none)
    double wg_trem_depth;      // amplitude LFO depth
    double wg_bore_mult;       // bore-length multiplier (1=string, 2=brass half-wave)
    // -- Additive Hammond drawbar / free-reed organ (ORGAN) --
    double org_amps[9];        // drawbar registration (footage ratios are fixed)
    int    org_freereed;       // 1 = saw/pulse reed banks (accordion/harmonica)
    int    org_reed_sq[9];     // free-reed: per-bank 1 = pulse, 0 = saw
    double org_drive;          // rock-organ overdrive
    double org_perc_amp;       // percussive ping amplitude (0 = none)
    double org_perc_ms;        // percussive ping decay
    double org_perc_ratio;     // percussion harmonic ratio (4=2nd, 6=3rd)
    double org_click_amp;      // key-click burst amplitude
    double org_click_ms;       // key-click decay
    double org_leslie_hz;      // Leslie rotary rate (0 = none)
    double org_leslie_depth;   // Leslie amp+pitch depth
    double org_breath;         // free-reed breath noise
    double org_lp_hz;          // free-reed output LPF cutoff (0 = bypass)
    double org_detune_cents;   // free-reed musette detune spread
    // -- Detuned multi-osc subtractive (SUPERSAW): ensemble / lead / pad --
    int    ss_nosc;            // oscillator count (1..7); 7 = full Szabo supersaw
    double ss_detune_x;        // Szabo detune knob 0..1 (drives 7-saw spread)
    double ss_mix_m;           // Szabo mix knob 0..1 (center/side gain law)
    double ss_spread_cents;    // simple per-osc detune when ss_nosc<7 (lead/pad)
    int    ss_square;          // 1 = pulse/square oscillators (else saw)
    double ss_pwm_hz, ss_pwm_depth;  // PWM LFO on square width
    double ss_cut0;            // base LPF cutoff (Hz)
    double ss_cut_env;         // attack filter-envelope amount (Hz added at onset)
    double ss_cut_env_ms;      // filter-envelope decay
    double ss_res;             // SVF resonance (0..0.98)
    double ss_sweep_hz, ss_sweep_oct; // slow cutoff LFO rate + depth (octaves)
    double ss_attack_ms;       // amplitude attack ramp (slow for pads)
    double ss_vib_hz, ss_vib_depth;   // collective vibrato
    double ss_chorus_hz, ss_chorus_depth, ss_chorus_mix; // ensemble chorus
    double ss_drive;           // charang/overdrive
    double ss_sub_mix; int ss_sub_sq; // bass+lead sub-octave
    double ss_fifth_gain;      // fifths lead: +7 semitone (1.5x) layer gain
    double ss_chiff_amt, ss_chiff_ms, ss_chiff_bp;  // chiff/transient burst
    double ss_trem_hz, ss_trem_depth;  // halo amplitude tremolo
    double ss_body_ms;         // orchestra-hit body decay (0 = sustained)
    double ss_pitch_blip;      // orchestra-hit downward pitch blip (cents)
    double ss_out_scale;       // output gain
    // -- Vocal formant bank (FORMANT): choir / voice / synth-voice --
    int    fmt_nsrc;           // source-osc count (1=solo voice, 3=choir)
    int    fmt_src_sq;         // 1 = pulse source, 0 = saw
    double fmt_detune_cents;   // per-source choir detune
    double fmt_f[3];           // formant frequencies F1..F3 (Hz, absolute)
    double fmt_bw[3];          // formant bandwidths (Hz)
    double fmt_gain_db[3];     // formant gains (dB)
    double fmt_attack_ms;      // slow vocal onset
    double fmt_breath;         // aspiration noise
    double fmt_vib_hz, fmt_vib_depth;  // decorrelated vibrato
    double fmt_out_scale;      // output gain
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

// ============================================================
// FX families — Synth Effects (GM 96-103) + Sound Effects (GM 120-127)
// ============================================================
// One data-driven param row drives BOTH FX engines. The engine + mode select
// which generic primitives (tonal core / texture bed / time effect / PhISEM /
// gate / sweep) participate. Code stays generic; variation lives here.
typedef struct {
    GMEngine engine;       // GM_ENGINE_SYNTHFX or GM_ENGINE_SOUNDFX
    int      mode;         // GMSynthFxMode or GMSoundFxMode
    // Tonal core (saw/sine/FM): detune of 2nd/3rd osc + FM ratio/index.
    double core_o2_cents;  // detune of partner osc (cents); 0 = none
    double core_o3_cents;  // detune of 3rd osc (cents); 0 = none
    double fm_ratio;       // C:M ratio for FM core (0 = no FM)
    double fm_index0;      // initial FM index
    double fm_index_ms;    // FM index env time-constant (ms)
    int    fm_rising;      // 1 = index ramps up into the note
    // Filter (SVF) — base cutoff Hz, resonance, sweep envelope.
    double cut0;           // base/relative cutoff (Hz). For SOUNDFX noise BP center.
    double res;            // SVF / BP resonance
    double cut_env;        // cutoff sweep amount (Hz, added then decays)
    double cut_env_ms;     // sweep decay
    int    cut_sweep_down; // 1 = sweep cutoff downward (sci-fi/fret)
    // LFO + S&H rates.
    double lfo_hz, lfo_depth;   // slow modulation (cutoff/amp)
    double sh_hz;               // sample-and-hold clock (0 = none)
    // Ring modulation.
    double ring_hz, ring_mix;   // ring-mod carrier (0 = none)
    // Delay / echo.
    double delay_ms, delay_fb, delay_mix, delay_damp; // (delay_ms 0 = none)
    // Texture / filtered-noise bed.
    double noise_amt;      // noise level
    double noise_lp_hz;    // noise low-pass (Hz)
    double noise_hp_hz;    // noise high-pass (Hz, 0 = none)
    int    noise_use_bp;   // 1 = band-pass biquad at cut0 instead of LP/HP
    // PhISEM particle engine.
    double ph_sys_decay, ph_snd_decay, ph_num, ph_gain;
    double ph_res_f[3], ph_res_R[3], ph_res_g[3]; int ph_nres;
    double ph_energy0, ph_energy_floor;
    int    ph_nswell; double ph_swell_hz[3]; double ph_swell_depth;
    // Pitch sweep (bird chirp / sci-fi zap / boom).
    double pitch_start;    // start pitch multiplier (e.g. 5.0 = 5x then glide to 1)
    double pitch_ms;       // glide time-constant
    // Internal AD envelopes (bird/gunshot/sci-fi transients).
    double amp_ms;         // primary internal decay (ms; 0 = sustained)
    double amp2_ms;        // secondary (crack vs boom)
    // Gate / cadence (telephone / bird syllables / helicopter AM).
    double gate_hz;        // cadence rate (Hz)
    double gate_on_frac;   // duty cycle that is on
    int    gate_n;         // syllable/event count (bird); 0 = continuous
    // Helicopter periodic-AM rotor.
    double am_hz, am_sharp, am_depth;
    // Boom / sub oscillator (sci-fi/gunshot).
    double boom_hz;        // absolute boom freq (Hz); 0 = none
    double out_scale;
} GMFxParams;

// ── Synth Effects (GM 96-103) ──
#define GM_SYNTHFX_FIRST 96
#define GM_SYNTHFX_COUNT 8
static const GMFxParams gm_synthfx_programs[GM_SYNTHFX_COUNT] = {
    // 96 FX1 rain — dense PhISEM droplets + ring-modded shimmer pair, S&H bend.
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_RAIN,
      .core_o2_cents = 7.0, .ring_hz = 6.0, .ring_mix = 0.35,
      .sh_hz = 7.0, .lfo_hz = 0.13, .lfo_depth = 0.4,
      .ph_sys_decay = 0.9995, .ph_snd_decay = 0.92, .ph_num = 1200.0, .ph_gain = 0.6,
      .ph_res_f = {2400.0, 0.0, 0.0}, .ph_res_R = {0.6, 0.0, 0.0}, .ph_res_g = {1.0, 0.0, 0.0},
      .ph_nres = 1, .ph_energy0 = 0.00030, .ph_energy_floor = 0.00012,
      .noise_amt = 0.0, .out_scale = 0.8 },
    // 97 FX2 soundtrack — 3 detuned saws, slow LFO-swept LPF (sweeping pad).
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_SOUNDTRACK,
      .core_o2_cents = -8.0, .core_o3_cents = 11.0,
      .cut0 = 900.0, .res = 0.35, .lfo_hz = 0.18, .lfo_depth = 0.8,
      .delay_ms = 0.0, .out_scale = 0.45 },
    // 98 FX3 crystal — inharmonic FM bell (1:3.5) + bright feedback delay twinkle.
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_CRYSTAL,
      .core_o2_cents = 5.0, .fm_ratio = 3.5, .fm_index0 = 3.0, .fm_index_ms = 280.0,
      .delay_ms = 110.0, .delay_fb = 0.55, .delay_mix = 0.5, .delay_damp = 4500.0,
      .amp_ms = 1400.0, .out_scale = 0.55 },
    // 99 FX4 atmosphere — soft pad + filtered-noise bed, slow amp LFO.
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_ATMOSPHERE,
      .core_o2_cents = -6.0, .cut0 = 1400.0, .res = 0.2,
      .lfo_hz = 0.09, .lfo_depth = 0.6,
      .noise_amt = 0.22, .noise_lp_hz = 2200.0, .noise_hp_hz = 250.0,
      .delay_ms = 0.0, .out_scale = 0.5 },
    // 100 FX5 brightness — saw + high-ratio FM with RISING index (brightens in).
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_BRIGHTNESS,
      .core_o2_cents = 6.0, .fm_ratio = 7.0, .fm_index0 = 2.2, .fm_index_ms = 600.0,
      .fm_rising = 1, .cut0 = 2600.0, .res = 0.3,
      .lfo_hz = 0.35, .lfo_depth = 0.5, .out_scale = 0.42 },
    // 101 FX6 goblins — dark ring-modded saw pair + S&H cutoff (ominous voice).
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_GOBLINS,
      .core_o2_cents = -9.0, .ring_hz = 70.0, .ring_mix = 0.55,
      .cut0 = 700.0, .res = 0.55, .sh_hz = 2.5, .lfo_depth = 0.7,
      .noise_amt = 0.05, .noise_lp_hz = 1500.0, .out_scale = 0.5 },
    // 102 FX7 echoes — bright FM ping into long regenerating dark delay.
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_ECHOES,
      .fm_ratio = 2.0, .fm_index0 = 2.0, .fm_index_ms = 120.0,
      .delay_ms = 160.0, .delay_fb = 0.7, .delay_mix = 0.65, .delay_damp = 3000.0,
      .amp_ms = 600.0, .out_scale = 0.5 },
    // 103 FX8 sci-fi — swept saw + resonant downward filter + pitch glide + noise crack.
    { .engine = GM_ENGINE_SYNTHFX, .mode = GM_FX_SCIFI,
      .ring_hz = 120.0, .ring_mix = 0.3,
      .cut0 = 600.0, .res = 0.7, .cut_env = 5000.0, .cut_env_ms = 220.0, .cut_sweep_down = 1,
      .pitch_start = 3.0, .pitch_ms = 180.0,
      .noise_amt = 0.3, .noise_lp_hz = 6000.0, .amp2_ms = 40.0,
      .boom_hz = 0.0, .out_scale = 0.5 },
};

// ── Sound Effects (GM 120-127) ──
#define GM_SOUNDFX_FIRST 120
#define GM_SOUNDFX_COUNT 8
static const GMFxParams gm_soundfx_programs[GM_SOUNDFX_COUNT] = {
    // 120 Guitar Fret Noise — short swept band-pass noise squeak (rising glide).
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_FRET,
      .cut0 = 2200.0, .res = 4.0, .noise_use_bp = 1,
      .cut_env = 1800.0, .cut_env_ms = 70.0, .cut_sweep_down = 0,
      .noise_amt = 1.0, .amp_ms = 110.0, .out_scale = 0.7 },
    // 121 Breath Noise — band-passed noise puff + faint PhISEM grain, soft AD.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_BREATH,
      .cut0 = 1600.0, .res = 1.2, .noise_use_bp = 1,
      .noise_amt = 1.0, .amp_ms = 260.0, .lfo_hz = 6.0, .lfo_depth = 0.25,
      .ph_sys_decay = 0.9990, .ph_snd_decay = 0.90, .ph_num = 600.0, .ph_gain = 0.4,
      .ph_res_f = {1800.0, 0.0, 0.0}, .ph_res_R = {0.5, 0.0, 0.0}, .ph_res_g = {1.0, 0.0, 0.0},
      .ph_nres = 1, .ph_energy0 = 0.00040, .ph_energy_floor = 0.0, .out_scale = 0.6 },
    // 122 Seashore — PhISEM surf (huge numObjects) + 3 slow swell LFOs.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_SEASHORE,
      .ph_sys_decay = 0.99985, .ph_snd_decay = 0.94, .ph_num = 3000.0, .ph_gain = 0.32,
      .ph_res_f = {1200.0, 3200.0, 0.0}, .ph_res_R = {0.55, 0.45, 0.0},
      .ph_res_g = {1.0, 0.5, 0.0}, .ph_nres = 2,
      .ph_energy0 = 0.00006, .ph_energy_floor = 0.000025,
      .ph_nswell = 3, .ph_swell_hz = {0.16, 0.11, 0.071}, .ph_swell_depth = 0.00008,
      .out_scale = 0.7 },
    // 123 Bird Tweet — pitch-swept FM chirps, fast trill, 3 gated syllables.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_BIRD,
      .fm_ratio = 1.0, .fm_index0 = 0.6, .fm_index_ms = 30.0,
      .pitch_start = 0.7, .pitch_ms = 18.0,
      .lfo_hz = 45.0, .lfo_depth = 0.12,
      .gate_hz = 9.0, .gate_on_frac = 0.55, .gate_n = 3,
      .out_scale = 0.5 },
    // 124 Telephone Ring — gated dual sine 440+480, 2 s on / 4 s off cadence.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_TELEPHONE,
      .gate_hz = 1.0 / 6.0, .gate_on_frac = 2.0 / 6.0,
      .lfo_hz = 20.0, .lfo_depth = 0.0, .out_scale = 0.4 },
    // 125 Helicopter — periodic-AM broadband noise (rotor chop) + low rumble.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_HELICOPTER,
      .noise_amt = 1.0, .noise_lp_hz = 2600.0, .noise_hp_hz = 120.0,
      .am_hz = 14.0, .am_sharp = 3.0, .am_depth = 0.95,
      .boom_hz = 55.0, .out_scale = 0.6 },
    // 126 Applause — PhISEM crowd claps (hundreds), clap BP ~1.5k, global swell.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_APPLAUSE,
      .ph_sys_decay = 0.9994, .ph_snd_decay = 0.88, .ph_num = 800.0, .ph_gain = 0.7,
      .ph_res_f = {1500.0, 0.0, 0.0}, .ph_res_R = {0.5, 0.0, 0.0}, .ph_res_g = {1.0, 0.0, 0.0},
      .ph_nres = 1, .ph_energy0 = 0.00035, .ph_energy_floor = 0.00012,
      .ph_nswell = 1, .ph_swell_hz = {0.08, 0.0, 0.0}, .ph_swell_depth = 0.00040,
      .out_scale = 0.75 },
    // 127 Gunshot — broadband noise crack burst + low boom with downward sweep.
    { .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_GUNSHOT,
      .noise_amt = 1.0, .noise_lp_hz = 9000.0, .noise_hp_hz = 400.0,
      .amp_ms = 8.0, .amp2_ms = 140.0,
      .boom_hz = 90.0, .pitch_start = 2.2, .pitch_ms = 60.0, .out_scale = 0.9 },
};

// ── Reverse Cymbal (GM 120 / 0-based 119) — bright noise under a RISING swell.
// Pre-existing gap in the Percussive family (112-118 covered modal perc; 119
// was unimplemented). It is a noise effect, so it rides the SOUNDFX engine. ──
static const GMFxParams gm_revcymbal_program = {
    .engine = GM_ENGINE_SOUNDFX, .mode = GM_SFX_FRET,   // reuse fret's noise path
    .cut0 = 7000.0, .res = 1.0, .noise_use_bp = 1,
    .noise_amt = 1.0, .out_scale = 0.6,
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

// ============================================================
// Batch-3 families: digital-waveguide winds & strings.
//   Strings 40-47, Brass 56-63, Reed 64-71, Pipe 72-79.
// One bidirectional bore delay line + a mode-specific junction nonlinearity.
// Variants within a family differ ONLY by data (loop damping, bore multiplier,
// bow/lip/reed/jet coefficients, output formant/mute). See dossiers 02 & 03.
// ============================================================

// ── Strings (GM 41-48 / 0-based 40-47) ──
// 40-43 Violin/Viola/Cello/Contrabass = bowed waveguide; 44 Tremolo = bowed +
// amplitude LFO; 45 Pizzicato + 46 Harp = plucked (PLUCK engine, see init);
// 47 Timpani = modal (see init). Bowed rows below.
#define GM_STRINGS_FIRST 40
static const GMProgramParams gm_strings_programs[5] = {
    // 41 Violin — shortest bore, bright. Body = air (A0) ~280, main wood (B1)
    // ~460, + a mid wood mode; the wg_fmt is the bridge-hill cluster on top.
    // The multi-mode body is what stops it sounding like a hollow tube.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_BOWED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.30, .wg_breath_max = 0.65, .wg_noise = 0.04,
      .wg_attack_ms = 45.0, .wg_vib_hz = 5.5, .wg_vib_depth = 0.008,
      .wg_bow_beta = 0.13, .wg_bow_slope = 3.0,
      .bodyf = {280.0, 460.0, 580.0}, .bodyq = {4.0, 5.5, 7.0}, .bodyg = {0.26, 0.20, 0.13},
      .wg_fmt_f = 2600.0, .wg_fmt_q = 1.2, .wg_fmt_gain = 0.30,
      .wg_out_scale = 2.2 },
    // 42 Viola — fifth lower, nasal; lower body modes, weakened bridge hill.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_BOWED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.36, .wg_breath_max = 0.63, .wg_noise = 0.045,
      .wg_attack_ms = 48.0, .wg_vib_hz = 5.2, .wg_vib_depth = 0.008,
      .wg_bow_beta = 0.13, .wg_bow_slope = 3.1,
      .bodyf = {220.0, 350.0, 440.0}, .bodyq = {4.0, 5.5, 7.0}, .bodyg = {0.28, 0.22, 0.14},
      .wg_fmt_f = 2000.0, .wg_fmt_q = 1.0, .wg_fmt_gain = 0.18,
      .wg_out_scale = 2.2 },
    // 43 Cello — long bore, warm woody corpus; low high-Q body modes.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_BOWED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.42, .wg_breath_max = 0.62, .wg_noise = 0.05,
      .wg_attack_ms = 55.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.009,
      .wg_bow_beta = 0.10, .wg_bow_slope = 3.2,
      .bodyf = {95.0, 175.0, 250.0}, .bodyq = {5.0, 6.5, 8.0}, .bodyg = {0.30, 0.22, 0.15},
      .wg_fmt_f = 1200.0, .wg_fmt_q = 2.0, .wg_fmt_gain = 0.34,
      .wg_out_scale = 2.4 },
    // 44 Contrabass — lowest, darkest loss, fundamental-dominated; bow grind.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_BOWED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.52, .wg_breath_max = 0.60, .wg_noise = 0.09,
      .wg_attack_ms = 60.0, .wg_vib_hz = 4.8, .wg_vib_depth = 0.010,
      .wg_bow_beta = 0.08, .wg_bow_slope = 3.4,
      .bodyf = {62.0, 100.0, 150.0}, .bodyq = {5.0, 6.5, 8.0}, .bodyg = {0.30, 0.22, 0.15},
      .wg_fmt_f = 100.0, .wg_fmt_q = 2.0, .wg_fmt_gain = 0.30,
      .wg_out_scale = 2.6 },
    // 45 Tremolo Strings — mid bowed voice + fast amplitude LFO + per-stroke grit.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_BOWED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.38, .wg_breath_max = 0.64, .wg_noise = 0.12,
      .wg_attack_ms = 20.0, .wg_vib_hz = 5.3, .wg_vib_depth = 0.008,
      .wg_bow_beta = 0.12, .wg_bow_slope = 3.0,
      .bodyf = {260.0, 430.0, 560.0}, .bodyq = {4.0, 5.5, 7.0}, .bodyg = {0.24, 0.18, 0.12},
      .wg_fmt_f = 2200.0, .wg_fmt_q = 1.2, .wg_fmt_gain = 0.25,
      .wg_out_scale = 2.2, .wg_trem_hz = 9.0, .wg_trem_depth = 0.5 },
};

// 45 Pizzicato + 46 Harp use PLUCK; 47 Timpani uses MODAL. KS rows.
// NOTE: the shared generate_pluck_sample applies the pluck-position comb inside
// the feedback loop, which pushes loop gain marginally >1 for higher notes —
// existing driven guitars stay bounded only because their tanh waveshaper caps
// the loop (e.g. clavi prog 7). We give pizz/harp a small ks_drive for the same
// guaranteed bound, and keep body resonance OFF (its parallel biquad is the
// other pre-existing runaway path). The seed-baked comb still shapes the attack.
static const GMProgramParams gm_pizz_program = {  // GM 46 Pizzicato Strings
    .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.992, .ks_loop_b = 0.30,
    .ks_beta = 0.15, .ks_pick = 0.85, .ks_drive = 0.22,
    .ks_big = 1, .ks_exc_smooth = 0.35, .ks_attack_amp = 0.06, .ks_attack_ms = 2.0,
};
static const GMProgramParams gm_harp_program = {  // GM 47 Orchestral Harp
    .engine = GM_ENGINE_PLUCK, .ks_stretch = 0.9985, .ks_loop_b = 0.28,
    .ks_beta = 0.20, .ks_pick = 0.85, .ks_drive = 0.18,
    .ks_big = 1, .ks_exc_smooth = 0.45, .ks_attack_amp = 0.04, .ks_attack_ms = 3.0,
};
// GM 48 Timpani — Rossing diametric modes (1,1)…(5,1): 1:1.5:2:2.44:2.9.
static const GMModalParams gm_timpani_program = {
    .name = "timpani", .nmodes = 5,
    .ratio = {1.0, 1.5, 1.99, 2.44, 2.90},
    .amp = {1.0, 0.6, 0.4, 0.28, 0.18},
    .t60 = {1.8, 1.1, 0.8, 0.55, 0.4},
    .strike_amp = 0.30, .strike_ms = 6.0, .bloom = 0.10, .pitched = 1,
};

// ── Brass (GM 57-64 / 0-based 56-63) ──
// 56-60 Trumpet/Trombone/Tuba/MutedTrumpet/FrenchHorn = lip waveguide;
// 61 Brass Section = lip + section shimmer; 62-63 SynthBrass = subtractive.
#define GM_BRASS_FIRST 56
// Brass lip model needs breath_max ~2.5-3.5 (the STK maxPressure region where
// the valve self-oscillates) and lip_gain ~6-12; below that the bore is barely
// excited, above it the valve saturates shut. Output scaled ~0.8-1.0.
static const GMProgramParams gm_brass_programs[6] = {
    // 57 Trumpet — short bright bore; high pressure, low loss.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.06, .wg_breath_max = 3.2, .wg_noise = 0.05,
      .wg_attack_ms = 28.0, .wg_vib_hz = 5.5, .wg_vib_depth = 0.004,
      .wg_lip_pole = 0.997, .wg_lip_gain = 10.0, .wg_out_scale = 1.0 },
    // 58 Trombone — longer bore, darker loss, glissando-capable (slew at host).
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.10, .wg_breath_max = 3.0, .wg_noise = 0.05,
      .wg_attack_ms = 40.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.004,
      .wg_lip_pole = 0.997, .wg_lip_gain = 8.0, .wg_out_scale = 1.0 },
    // 59 Tuba — longest bore, darkest, round soft attack.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.14, .wg_breath_max = 2.7, .wg_noise = 0.05,
      .wg_attack_ms = 65.0, .wg_vib_hz = 4.5, .wg_vib_depth = 0.003,
      .wg_lip_pole = 0.995, .wg_lip_gain = 6.0, .wg_out_scale = 1.0 },
    // 60 Muted Trumpet — trumpet + mute LP + nasal mid bandpass.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.09, .wg_breath_max = 3.0, .wg_noise = 0.06,
      .wg_attack_ms = 26.0, .wg_vib_hz = 5.5, .wg_vib_depth = 0.004,
      .wg_lip_pole = 0.996, .wg_lip_gain = 9.0,
      .wg_fmt_f = 1700.0, .wg_fmt_q = 2.5, .wg_fmt_gain = 0.45,
      .wg_out_lp_hz = 3200.0, .wg_out_scale = 1.1 },
    // 61 French Horn — long, mellow lip, dark loss, soft attack.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.12, .wg_breath_max = 2.8, .wg_noise = 0.04,
      .wg_attack_ms = 55.0, .wg_vib_hz = 4.8, .wg_vib_depth = 0.003,
      .wg_lip_pole = 0.995, .wg_lip_gain = 7.0,
      .wg_out_lp_hz = 5000.0, .wg_out_scale = 1.0 },
    // 62 Brass Section — bright lip + section amplitude shimmer.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_LIP, .wg_bore_mult = 2.0,
      .wg_loop_damp = 0.07, .wg_breath_max = 3.1, .wg_noise = 0.06,
      .wg_attack_ms = 35.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.005,
      .wg_lip_pole = 0.997, .wg_lip_gain = 9.0, .wg_out_scale = 1.0,
      .wg_trem_hz = 6.0, .wg_trem_depth = 0.18 },
};
// 63-64 SynthBrass — subtractive saw + resonant LPF with brass filter-swell.
static const GMProgramParams gm_synthbrass_programs[2] = {
    // 63 SynthBrass 1 — 2 detuned saws → LPF swell 5.5k→1.8k over ~120 ms.
    { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 8.0, .sb_o2_mix = 0.7,
      .sb_sub = 0.0, .sb_cut0 = 5500.0, .sb_cut1 = 1800.0, .sb_cut_ms = 120.0,
      .sb_res = 0.45, .sb_psweep = 1.06, .sb_psweep_ms = 12.0, .sb_sustained = 1,
      .sb_vib_hz = 5.0, .sb_vib_depth = 0.004 },
    // 64 SynthBrass 2 — harder/brighter, faster snappier filter env, more res.
    { .engine = GM_ENGINE_SYNTHBASS, .sb_o2_cents = 10.0, .sb_o2_mix = 0.6,
      .sb_o2_sq = 1, .sb_sub = 0.0, .sb_cut0 = 6500.0, .sb_cut1 = 2200.0,
      .sb_cut_ms = 70.0, .sb_res = 0.6, .sb_psweep = 1.08, .sb_psweep_ms = 9.0,
      .sb_sustained = 1, .sb_vib_hz = 5.5, .sb_vib_depth = 0.005 },
};

// ── Reed (GM 65-72 / 0-based 64-71) ──
// STK reed table (offset 0.6, slope ~-0.8); conical sax/oboe/bassoon = full
// harmonics, clarinet cylinder = inverting reflection (odd harmonics).
#define GM_REED_FIRST 64
static const GMProgramParams gm_reed_programs[8] = {
    // 65 Soprano Sax — short bright conical bore.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.40, .wg_breath_max = 0.85, .wg_noise = 0.10,
      .wg_attack_ms = 18.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.005,
      .wg_reed_offset = 0.6, .wg_reed_slope = -0.85, .wg_bore_invert = 0,
      .wg_out_scale = 0.9 },
    // 66 Alto Sax — classic sax buzz.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.45, .wg_breath_max = 0.85, .wg_noise = 0.12,
      .wg_attack_ms = 20.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.005,
      .wg_reed_offset = 0.6, .wg_reed_slope = -0.80, .wg_bore_invert = 0,
      .wg_out_scale = 0.9 },
    // 67 Tenor Sax — warmer/breathier.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.52, .wg_breath_max = 0.83, .wg_noise = 0.13,
      .wg_attack_ms = 22.0, .wg_vib_hz = 4.8, .wg_vib_depth = 0.006,
      .wg_reed_offset = 0.6, .wg_reed_slope = -0.75, .wg_bore_invert = 0,
      .wg_out_scale = 0.95 },
    // 68 Baritone Sax — dark, big noise floor.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.58, .wg_breath_max = 0.80, .wg_noise = 0.15,
      .wg_attack_ms = 26.0, .wg_vib_hz = 4.6, .wg_vib_depth = 0.006,
      .wg_reed_offset = 0.6, .wg_reed_slope = -0.70, .wg_bore_invert = 0,
      .wg_out_scale = 1.0 },
    // 69 Oboe — stiff double reed, thin nasal; singer's formant ~1.4 kHz.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.34, .wg_breath_max = 0.80, .wg_noise = 0.05,
      .wg_attack_ms = 16.0, .wg_vib_hz = 5.5, .wg_vib_depth = 0.005,
      .wg_reed_offset = 0.55, .wg_reed_slope = -0.90, .wg_bore_invert = 0,
      .wg_fmt_f = 1400.0, .wg_fmt_q = 2.0, .wg_fmt_gain = 0.30, .wg_out_scale = 0.85 },
    // 70 English Horn — alto oboe, rounder; formant ~1.1 kHz.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.40, .wg_breath_max = 0.80, .wg_noise = 0.06,
      .wg_attack_ms = 18.0, .wg_vib_hz = 5.2, .wg_vib_depth = 0.005,
      .wg_reed_offset = 0.55, .wg_reed_slope = -0.88, .wg_bore_invert = 0,
      .wg_fmt_f = 1100.0, .wg_fmt_q = 2.0, .wg_fmt_gain = 0.28, .wg_out_scale = 0.9 },
    // 71 Bassoon — bass double reed, hollow low; woody formant ~470 Hz + mild LP.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.50, .wg_breath_max = 0.80, .wg_noise = 0.07,
      .wg_attack_ms = 22.0, .wg_vib_hz = 4.8, .wg_vib_depth = 0.005,
      .wg_reed_offset = 0.58, .wg_reed_slope = -0.82, .wg_bore_invert = 0,
      .wg_fmt_f = 470.0, .wg_fmt_q = 2.0, .wg_fmt_gain = 0.30,
      .wg_out_lp_hz = 3500.0, .wg_out_scale = 0.95 },
    // 72 Clarinet — CYLINDRICAL: invert bore reflection → odd harmonics, woody.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_REED, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.42, .wg_breath_max = 0.82, .wg_noise = 0.04,
      .wg_attack_ms = 18.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.004,
      .wg_reed_offset = 0.6, .wg_reed_slope = -0.80, .wg_bore_invert = 1,
      .wg_out_scale = 0.9 },
};

// ── Pipe (GM 73-80 / 0-based 72-79) ──
// Cook flute jet waveguide: jet delay + cubic + blowing noise. Bottle/ocarina
// (76/79) are Helmholtz vessels → single resonant bandpass (helmholtz branch
// modeled via a high loop damping + strong output formant).
#define GM_PIPE_FIRST 72
static const GMProgramParams gm_pipe_programs[8] = {
    // 73 Piccolo — octave up; short bore handled by f0, very bright.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.10, .wg_breath_max = 0.55, .wg_noise = 0.06,
      .wg_attack_ms = 12.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.004,
      .wg_jet_ratio = 0.30, .wg_out_scale = 1.4 },
    // 74 Flute — canonical Cook model.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.16, .wg_breath_max = 0.55, .wg_noise = 0.08,
      .wg_attack_ms = 16.0, .wg_vib_hz = 5.0, .wg_vib_depth = 0.005,
      .wg_jet_ratio = 0.32, .wg_out_scale = 1.5 },
    // 75 Recorder — pure, minimal noise/vibrato.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.20, .wg_breath_max = 0.55, .wg_noise = 0.04,
      .wg_attack_ms = 14.0, .wg_vib_hz = 4.5, .wg_vib_depth = 0.002,
      .wg_jet_ratio = 0.32, .wg_out_scale = 1.5 },
    // 76 Pan Flute — strong breathy chiff.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.18, .wg_breath_max = 0.55, .wg_noise = 0.22,
      .wg_attack_ms = 18.0, .wg_vib_hz = 4.0, .wg_vib_depth = 0.005,
      .wg_jet_ratio = 0.40, .wg_out_scale = 1.5 },
    // 77 Blown Bottle — Helmholtz: heavy damping, strong resonant formant.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.55, .wg_breath_max = 0.55, .wg_noise = 0.18,
      .wg_attack_ms = 24.0, .wg_vib_hz = 3.5, .wg_vib_depth = 0.004,
      .wg_jet_ratio = 0.45, .wg_fmt_f = 0.0, .wg_fmt_q = 8.0, .wg_fmt_gain = 0.5,
      .wg_out_scale = 1.6 },
    // 78 Shakuhachi — very breathy, airy edge, expressive.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.20, .wg_breath_max = 0.55, .wg_noise = 0.25,
      .wg_attack_ms = 22.0, .wg_vib_hz = 6.0, .wg_vib_depth = 0.010,
      .wg_jet_ratio = 0.38, .wg_out_scale = 1.5 },
    // 79 Whistle — tin/penny whistle, bright, higher jet ratio, lively vibrato.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.12, .wg_breath_max = 0.55, .wg_noise = 0.05,
      .wg_attack_ms = 12.0, .wg_vib_hz = 6.0, .wg_vib_depth = 0.006,
      .wg_jet_ratio = 0.45, .wg_out_scale = 1.4 },
    // 80 Ocarina — Helmholtz vessel, pure, slightly hollow.
    { .engine = GM_ENGINE_WAVEGUIDE, .wg_mode = GM_WG_JET, .wg_bore_mult = 1.0,
      .wg_loop_damp = 0.50, .wg_breath_max = 0.55, .wg_noise = 0.06,
      .wg_attack_ms = 18.0, .wg_vib_hz = 4.5, .wg_vib_depth = 0.004,
      .wg_jet_ratio = 0.50, .wg_fmt_f = 0.0, .wg_fmt_q = 6.0, .wg_fmt_gain = 0.4,
      .wg_out_scale = 1.5 },
};

// ============================================================
// Batch-4 families: additive organs, supersaw ensemble/lead/pad, vocal formant.
//   Organ 16-23, Ensemble 48-55, Synth Lead 80-87, Synth Pad 88-95.
// Three new engines: ORGAN (Hammond drawbars + free-reed), SUPERSAW (detuned
// multi-osc subtractive + SVF), FORMANT (source-filter vocal bank). Variants are
// DATA; the generators are generic. See dossiers 01 (organ), 02 (ensemble/choir),
// 03 (synth lead / synth pad), 00 (per-family stochasticism mul).
// ============================================================

// ── Organ (GM 17-24 / 0-based 16-23) ──
// Hammond drawbar footage ratios (fixed): 16′ 5⅓′ 8′ 4′ 2⅔′ 2′ 1⅗′ 1⅓′ 1′.
static const double GM_ORGAN_RATIOS[9] =
    { 0.5, 1.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0 };
#define GM_ORGAN_FIRST 16
static const GMProgramParams gm_organ_programs[8] = {
    // 17 Drawbar — full registration 888 800 000, slow Leslie + faint click.
    { .engine = GM_ENGINE_ORGAN,
      .org_amps = {0.8,0.8,0.8,0.8,0.0,0.0,0.0,0.0,0.0},
      .org_click_amp = 0.04, .org_click_ms = 3.0,
      .org_leslie_hz = 0.8, .org_leslie_depth = 0.10 },
    // 18 Percussive — 888 000 000 + 2nd-harmonic ping (4′) fast decay.
    { .engine = GM_ENGINE_ORGAN,
      .org_amps = {0.7,0.0,0.9,0.6,0.0,0.0,0.0,0.0,0.0},
      .org_perc_amp = 0.5, .org_perc_ms = 180.0, .org_perc_ratio = 2.0,
      .org_click_amp = 0.06, .org_click_ms = 2.5,
      .org_leslie_hz = 0.8, .org_leslie_depth = 0.10 },
    // 19 Rock — full bright drawbars + overdrive + fast Leslie.
    { .engine = GM_ENGINE_ORGAN,
      .org_amps = {0.9,0.7,0.9,0.8,0.5,0.7,0.4,0.3,0.6},
      .org_drive = 0.55, .org_click_amp = 0.05, .org_click_ms = 2.0,
      .org_leslie_hz = 6.9, .org_leslie_depth = 0.18 },
    // 20 Church (Pipe) — octave-spaced ranks 8+4+2+mixtures, slow chiff, no Leslie.
    { .engine = GM_ENGINE_ORGAN,
      .org_amps = {0.4,0.3,0.9,0.7,0.4,0.6,0.3,0.0,0.4},
      .org_click_amp = 0.03, .org_click_ms = 8.0,
      .org_detune_cents = 4.0 },
    // 21 Reed Organ — free-reed, sawtooth-ish + gentle LP + reed beating.
    { .engine = GM_ENGINE_ORGAN, .org_freereed = 1,
      .org_amps = {0.0,0.0,1.0,0.5,0.0,0.0,0.0,0.0,0.0},
      .org_detune_cents = 8.0, .org_breath = 0.04, .org_lp_hz = 3500.0,
      .org_click_amp = 0.02, .org_click_ms = 6.0 },
    // 22 Accordion — free-reed, 3 detuned banks (musette), buzzy bright.
    { .engine = GM_ENGINE_ORGAN, .org_freereed = 1,
      .org_amps = {0.0,0.0,1.0,0.7,0.0,0.5,0.0,0.0,0.0},
      .org_detune_cents = 18.0, .org_breath = 0.05, .org_lp_hz = 5000.0,
      .org_click_amp = 0.03, .org_click_ms = 4.0,
      .org_leslie_hz = 4.5, .org_leslie_depth = 0.06 },
    // 23 Harmonica — free-reed, breath-driven, strong air + tremolo.
    { .engine = GM_ENGINE_ORGAN, .org_freereed = 1,
      .org_amps = {0.0,0.0,1.0,0.6,0.0,0.4,0.0,0.0,0.0},
      .org_reed_sq = {0,0,1,0,0,0,0,0,0}, .org_detune_cents = 6.0,
      .org_breath = 0.14, .org_lp_hz = 4500.0,
      .org_leslie_hz = 5.5, .org_leslie_depth = 0.10,
      .org_click_amp = 0.02, .org_click_ms = 3.0 },
    // 24 Tango Accordion (Bandoneon) — drier/sharper, dual-reed octave, more buzz.
    { .engine = GM_ENGINE_ORGAN, .org_freereed = 1,
      .org_amps = {0.0,0.0,1.0,0.8,0.0,0.6,0.0,0.0,0.3},
      .org_reed_sq = {0,0,0,1,0,1,0,0,0}, .org_detune_cents = 9.0,
      .org_breath = 0.05, .org_lp_hz = 6000.0,
      .org_click_amp = 0.05, .org_click_ms = 3.0 },
};

// ── Ensemble (GM 49-56 / 0-based 48-55) ──
// 48-51 supersaw string/synth-string pads; 52-54 vocal formant (init routes to
// FORMANT); 55 Orchestra Hit = detuned cluster + transient + fast decay.
#define GM_ENSEMBLE_FIRST 48
static const GMProgramParams gm_ensemble_programs[5] = {
    // 49 String Ensemble 1 — warm, slow attack, lower LPF, heavy chorus.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 7, .ss_detune_x = 0.30, .ss_mix_m = 0.70,
      .ss_cut0 = 4500.0, .ss_res = 0.20, .ss_attack_ms = 160.0,
      .ss_vib_hz = 5.0, .ss_vib_depth = 0.004,
      .ss_chorus_hz = 0.45, .ss_chorus_depth = 0.004, .ss_chorus_mix = 0.5,
      .ss_out_scale = 0.5 },
    // 50 String Ensemble 2 — wider detune, brighter, slower swell, more chorus.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 7, .ss_detune_x = 0.45, .ss_mix_m = 0.78,
      .ss_cut0 = 7000.0, .ss_res = 0.18, .ss_attack_ms = 250.0,
      .ss_vib_hz = 4.8, .ss_vib_depth = 0.005,
      .ss_chorus_hz = 0.4, .ss_chorus_depth = 0.006, .ss_chorus_mix = 0.6,
      .ss_out_scale = 0.5 },
    // 51 SynthStrings 1 — overtly synthetic; filter-env sweep + heavy chorus.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 7, .ss_detune_x = 0.30, .ss_mix_m = 0.60,
      .ss_cut0 = 1400.0, .ss_cut_env = 4000.0, .ss_cut_env_ms = 200.0, .ss_res = 0.35,
      .ss_attack_ms = 90.0, .ss_chorus_hz = 0.5, .ss_chorus_depth = 0.007,
      .ss_chorus_mix = 0.7, .ss_out_scale = 0.5 },
    // 52 SynthStrings 2 — more resonance, square layer, touch faster.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 7, .ss_detune_x = 0.40, .ss_mix_m = 0.65,
      .ss_cut0 = 1800.0, .ss_cut_env = 4500.0, .ss_cut_env_ms = 160.0, .ss_res = 0.55,
      .ss_attack_ms = 70.0, .ss_pwm_hz = 0.6, .ss_pwm_depth = 0.25,
      .ss_chorus_hz = 0.5, .ss_chorus_depth = 0.006, .ss_chorus_mix = 0.6,
      .ss_out_scale = 0.5 },
    // 56 Orchestra Hit — dense detuned cluster + chiff transient + fast decay + blip.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 7, .ss_detune_x = 0.55, .ss_mix_m = 0.80,
      .ss_cut0 = 6000.0, .ss_res = 0.25, .ss_attack_ms = 3.0,
      .ss_fifth_gain = 0.5, .ss_chiff_amt = 0.5, .ss_chiff_ms = 18.0, .ss_chiff_bp = 8.0,
      .ss_body_ms = 320.0, .ss_pitch_blip = 100.0, .ss_out_scale = 0.45 },
};

// ── Choir / Voice (GM 53-55 / 0-based 52-54) — vocal formant bank ──
// /a/ "Aah" (bass row) and /u/ "Ooh" (bass row), dossier 02 Csound tables.
static const GMProgramParams gm_choir_program = {  // 53 Choir Aahs — /a/, 3 voices
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 3, .fmt_src_sq = 0, .fmt_detune_cents = 7.0,
    .fmt_f = {600.0, 1040.0, 2250.0}, .fmt_bw = {60.0, 70.0, 110.0},
    .fmt_gain_db = {0.0, -7.0, -9.0}, .fmt_attack_ms = 120.0, .fmt_breath = 0.05,
    .fmt_vib_hz = 5.5, .fmt_vib_depth = 0.012, .fmt_out_scale = 1.6,
};
static const GMProgramParams gm_voiceoohs_program = {  // 54 Voice Oohs — /u/, 3 voices
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 3, .fmt_src_sq = 0, .fmt_detune_cents = 7.0,
    .fmt_f = {350.0, 600.0, 2400.0}, .fmt_bw = {40.0, 80.0, 100.0},
    .fmt_gain_db = {0.0, -20.0, -32.0}, .fmt_attack_ms = 140.0, .fmt_breath = 0.04,
    .fmt_vib_hz = 5.2, .fmt_vib_depth = 0.012, .fmt_out_scale = 1.8,
};
static const GMProgramParams gm_synthvoice_program = {  // 55 Synth Voice — solo, neutral
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 1, .fmt_src_sq = 0, .fmt_detune_cents = 0.0,
    .fmt_f = {500.0, 900.0, 2500.0}, .fmt_bw = {60.0, 90.0, 120.0},
    .fmt_gain_db = {0.0, -8.0, -14.0}, .fmt_attack_ms = 40.0, .fmt_breath = 0.02,
    .fmt_vib_hz = 5.0, .fmt_vib_depth = 0.008, .fmt_out_scale = 1.5,
};

// ── Synth Lead (GM 81-88 / 0-based 80-87) — subtractive named waveforms ──
// 86 Voice routes to FORMANT (init); the rest are SUPERSAW (1-2 osc subtractive).
#define GM_LEAD_FIRST 80
static const GMProgramParams gm_lead_programs[8] = {
    // 81 Square — 1 square + PWM, light static LPF, hollow odd-harmonic.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 1, .ss_square = 1,
      .ss_pwm_hz = 0.4, .ss_pwm_depth = 0.35,
      .ss_cut0 = 6000.0, .ss_res = 0.20, .ss_attack_ms = 6.0, .ss_out_scale = 0.7 },
    // 82 Sawtooth — 2 detuned saws + LPF env sweep, bright buzzy lead.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 2, .ss_spread_cents = 7.0,
      .ss_cut0 = 2500.0, .ss_cut_env = 4500.0, .ss_cut_env_ms = 120.0, .ss_res = 0.30,
      .ss_attack_ms = 5.0, .ss_out_scale = 0.7 },
    // 83 Calliope — triangle-ish (filtered saw) + soft saw, slow attack, breath.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 2, .ss_spread_cents = 4.0,
      .ss_cut0 = 2000.0, .ss_res = 0.10, .ss_attack_ms = 60.0,
      .ss_vib_hz = 5.0, .ss_vib_depth = 0.005, .ss_chiff_amt = 0.06, .ss_chiff_ms = 30.0,
      .ss_chiff_bp = 6.0, .ss_out_scale = 0.8 },
    // 84 Chiff — saw + noise burst on attack, fast env, breathy chiff transient.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 2, .ss_spread_cents = 6.0,
      .ss_cut0 = 3000.0, .ss_res = 0.25, .ss_attack_ms = 8.0,
      .ss_chiff_amt = 0.45, .ss_chiff_ms = 40.0, .ss_chiff_bp = 5.0, .ss_out_scale = 0.7 },
    // 85 Charang — 2 hard-detuned saws + drive + resonant LPF, guitar-ish.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 2, .ss_spread_cents = 12.0,
      .ss_cut0 = 3500.0, .ss_res = 0.45, .ss_attack_ms = 5.0, .ss_drive = 0.6,
      .ss_out_scale = 0.65 },
    // 86 Voice — formant "aah" lead (routes to FORMANT in init; row unused).
    { .engine = GM_ENGINE_FORMANT },
    // 87 Fifths — lead osc + perfect-fifth (1.5x) layer, LPF, organum.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 1, .ss_fifth_gain = 0.9,
      .ss_cut0 = 3500.0, .ss_res = 0.25, .ss_attack_ms = 6.0, .ss_out_scale = 0.6 },
    // 88 Bass+Lead — saw + sub-octave square, keytracked LPF, fat split.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 2, .ss_spread_cents = 6.0,
      .ss_sub_mix = 0.7, .ss_sub_sq = 1,
      .ss_cut0 = 3000.0, .ss_res = 0.30, .ss_attack_ms = 6.0, .ss_out_scale = 0.6 },
};

// ── Synth Pad (GM 89-96 / 0-based 88-95) — detuned multi-osc + slow sweep ──
// 92 Choir + 95 Halo route to FORMANT (init); the rest SUPERSAW with slow attacks.
#define GM_PAD_FIRST 88
static const GMProgramParams gm_pad_programs[8] = {
    // 89 New Age — detuned saws, slow LPF sweep, shimmery slow swell.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 5, .ss_spread_cents = 10.0,
      .ss_cut0 = 2200.0, .ss_res = 0.30, .ss_sweep_hz = 0.15, .ss_sweep_oct = 1.0,
      .ss_attack_ms = 500.0, .ss_chorus_hz = 0.3, .ss_chorus_depth = 0.005,
      .ss_chorus_mix = 0.5, .ss_out_scale = 0.5 },
    // 90 Warm — 3 detuned saws, low cutoff, soft attack, rounded.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 3, .ss_spread_cents = 8.0,
      .ss_cut0 = 1100.0, .ss_res = 0.20, .ss_attack_ms = 600.0,
      .ss_chorus_hz = 0.25, .ss_chorus_depth = 0.005, .ss_chorus_mix = 0.5,
      .ss_out_scale = 0.55 },
    // 91 Polysynth — bright detuned saws + PWM, medium attack.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 5, .ss_spread_cents = 12.0,
      .ss_pwm_hz = 0.4, .ss_pwm_depth = 0.2,
      .ss_cut0 = 4500.0, .ss_res = 0.25, .ss_attack_ms = 200.0,
      .ss_chorus_hz = 0.35, .ss_chorus_depth = 0.004, .ss_chorus_mix = 0.5,
      .ss_out_scale = 0.5 },
    // 92 Choir — vocal-formant pad (routes to FORMANT in init; row unused).
    { .engine = GM_ENGINE_FORMANT },
    // 93 Bowed Glass — saw + slow noisy attack + high-Q body resonance swell.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 3, .ss_spread_cents = 6.0,
      .ss_cut0 = 1800.0, .ss_res = 0.55, .ss_attack_ms = 700.0,
      .ss_chiff_amt = 0.10, .ss_chiff_ms = 400.0, .ss_chiff_bp = 4.0,
      .ss_chorus_hz = 0.2, .ss_chorus_depth = 0.004, .ss_chorus_mix = 0.4,
      .ss_out_scale = 0.5 },
    // 94 Metallic — FM inharmonic (ratio 1.4) bell-like + slow attack.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 3, .ss_spread_cents = 5.0,
      .ss_square = 1, .ss_pwm_hz = 0.3, .ss_pwm_depth = 0.4,
      .ss_cut0 = 5000.0, .ss_res = 0.40, .ss_attack_ms = 400.0,
      .ss_sweep_hz = 0.18, .ss_sweep_oct = 1.2,
      .ss_out_scale = 0.5 },
    // 95 Halo — bright formant choir pad + slow tremolo (routes to FORMANT).
    { .engine = GM_ENGINE_FORMANT },
    // 96 Sweep — detuned saws + dramatic resonant LPF sweep LFO.
    { .engine = GM_ENGINE_SUPERSAW, .ss_nosc = 5, .ss_spread_cents = 12.0,
      .ss_cut0 = 1500.0, .ss_res = 0.70, .ss_sweep_hz = 0.22, .ss_sweep_oct = 2.5,
      .ss_attack_ms = 300.0, .ss_chorus_hz = 0.3, .ss_chorus_depth = 0.005,
      .ss_chorus_mix = 0.5, .ss_out_scale = 0.45 },
};
// Halo = bright formant pad: /a/ bright (alto row) + slow amplitude tremolo.
static const GMProgramParams gm_halo_program = {
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 3, .fmt_src_sq = 0, .fmt_detune_cents = 9.0,
    .fmt_f = {800.0, 1150.0, 2800.0}, .fmt_bw = {50.0, 60.0, 170.0},
    .fmt_gain_db = {0.0, -4.0, -16.0}, .fmt_attack_ms = 500.0, .fmt_breath = 0.05,
    .fmt_vib_hz = 5.0, .fmt_vib_depth = 0.012, .fmt_out_scale = 1.5,
    .ss_trem_hz = 4.0, .ss_trem_depth = 0.25,
};
// Choir pad (92) = "ooh/aah" SoS formant + chorus + slow attack (alto /o/-ish).
static const GMProgramParams gm_choirpad_program = {
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 3, .fmt_src_sq = 0, .fmt_detune_cents = 8.0,
    .fmt_f = {450.0, 800.0, 2830.0}, .fmt_bw = {70.0, 80.0, 100.0},
    .fmt_gain_db = {0.0, -9.0, -16.0}, .fmt_attack_ms = 450.0, .fmt_breath = 0.05,
    .fmt_vib_hz = 5.0, .fmt_vib_depth = 0.012, .fmt_out_scale = 1.6,
};
// Synth-lead Voice (86) = formant "aah" lead, solo, fast attack.
static const GMProgramParams gm_leadvoice_program = {
    .engine = GM_ENGINE_FORMANT, .fmt_nsrc = 1, .fmt_src_sq = 0, .fmt_detune_cents = 0.0,
    .fmt_f = {660.0, 1700.0, 2400.0}, .fmt_bw = {80.0, 90.0, 120.0},
    .fmt_gain_db = {0.0, -8.0, -12.0}, .fmt_attack_ms = 14.0, .fmt_breath = 0.02,
    .fmt_vib_hz = 5.5, .fmt_vib_depth = 0.010, .fmt_out_scale = 1.4,
};

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

// Digital-waveguide note-on (bowed / brass / reed / flute). Shares ks_buf as the
// bidirectional bore delay line. Draws all parametric jitter first (dossier 00).
static void gm_waveguide_init(GMVoice *v, const GMProgramParams *p, double f0,
                              double sr) {
    // Bowed strings & reeds & pipes sit at mul ~1.0 (dossier 00 §6); brass ~0.9.
    double mul = (p->wg_mode == GM_WG_LIP) ? 0.9 : 1.0;
    v->wg_mode = (GMWaveguideMode)p->wg_mode;

    // -- Pitch micro-detune (hard-capped ±6c) baked into the bore length. --
    double fdet = voice_detune(v, f0, 6.0, mul);
    if (fdet < 20.0) fdet = 20.0;

    double bore_mult = (p->wg_bore_mult > 0.0) ? p->wg_bore_mult : 1.0;
    double base_delay = (sr / fdet) * bore_mult;
    if (p->wg_mode == GM_WG_LIP) base_delay += 3.0;   // STK half-wave correction
    else if (p->wg_mode == GM_WG_BOWED) base_delay -= 4.0; // filter group delay
    if (base_delay < 4.0) base_delay = 4.0;
    if (base_delay > (double)(GM_KS_BIG_N - 4)) base_delay = (double)(GM_KS_BIG_N - 4);
    v->wg_base_delay = base_delay;

    memset(v->ks_buf, 0, sizeof(v->ks_buf));
    v->wg_w = 0;
    v->wg_loop_lp = 0.0;
    v->wg_loop_damp = clampd(p->wg_loop_damp, 0.0, 0.95);
    v->wg_hp_x1 = v->wg_hp_y1 = 0.0;

    // -- Breath/bow pressure: ceiling jittered (velocity/amp micro-jitter h). --
    v->wg_breath = 0.0;
    v->wg_breath_max = voice_jitter(v, p->wg_breath_max, 0.08, mul);
    v->wg_noise_gain = p->wg_noise;

    // -- Attack-time micro-jitter (g, ±12%). --
    double atk_ms = voice_jitter(v, p->wg_attack_ms, 0.12, mul);
    if (atk_ms < 1.0) atk_ms = 1.0;
    v->wg_attack_ms = atk_ms;
    v->wg_attack_env = 0.0;
    v->wg_attack_inc = 1.0 / (atk_ms * 0.001 * sr);

    // -- Vibrato: rate jitter (a), random phase (c). --
    if (p->wg_vib_hz > 0.0) {
        double vhz = voice_jitter(v, p->wg_vib_hz, 0.10, mul);
        v->wg_vib_inc = vhz / sr;
        v->wg_vib_depth = p->wg_vib_depth;
        v->wg_vib_phase = voice_rand_phase(v);
    } else {
        v->wg_vib_inc = 0.0; v->wg_vib_depth = 0.0; v->wg_vib_phase = 0.0;
    }

    // -- Mode-specific junction setup. --
    if (p->wg_mode == GM_WG_BOWED) {
        v->wg_bow_beta = clampd(p->wg_bow_beta, 0.02, 0.5);
        // Bow force / pluck-position-like jitter (e, ±8%) on the friction slope.
        v->wg_bow_slope = voice_jitter(v, p->wg_bow_slope, 0.08, mul);
        // Proper STK bowed string uses TWO delay lines split at the bow point:
        // ks_buf = neck segment (bow→nut), bore_buf = bridge segment (bow→bridge).
        // (Reading two taps off ONE line, as before, made the loop lock to the
        // short bridge tap → pitch ~1/beta too high, i.e. +2-3 octaves.) Clear
        // the bridge line + its write pointer; ks_buf was cleared above.
        memset(v->bore_buf, 0, sizeof(v->bore_buf));
        v->bore_w = 0;
        // Multi-mode body resonator (air + wood modes) so the string sounds
        // like a real instrument body, not a hollow tube. Applied parallel to
        // the bridge tap in the render (mirrors the plucked-string body bank).
        gm_setup_body_resonance(v, p, sr, mul);
    } else if (p->wg_mode == GM_WG_LIP) {
        // Lip-resonance biquad bandpass tracking f0 (RBJ), near-unit pole.
        double pole = p->wg_lip_pole > 0.0 ? p->wg_lip_pole : 0.997;
        double flip = fdet;
        if (flip > sr * 0.45) flip = sr * 0.45;
        double w0 = 2.0 * M_PI * flip / sr;
        double r = pole;
        double cw = cos(w0);
        // Resonator: H(z) with poles at r·e^{±jw0}; bandpass-ish 1-zero numerator.
        v->wg_lip_a1 = -2.0 * r * cw;
        v->wg_lip_a2 = r * r;
        v->wg_lip_b0 = (1.0 - r);     // normalized gain at resonance
        v->wg_lip_b1 = 0.0;
        v->wg_lip_b2 = -(1.0 - r);
        v->wg_lip_x1 = v->wg_lip_x2 = v->wg_lip_y1 = v->wg_lip_y2 = 0.0;
        v->wg_lip_gain = p->wg_lip_gain > 0.0 ? p->wg_lip_gain : 0.03;
    } else if (p->wg_mode == GM_WG_REED) {
        v->wg_reed_offset = p->wg_reed_offset;
        // Reed slope ≈ brightness/energy lever; jitter like FM index (f, ±6%).
        v->wg_reed_slope = voice_jitter(v, p->wg_reed_slope, 0.06, mul);
        v->wg_bore_invert = p->wg_bore_invert;
    } else { // GM_WG_JET
        v->wg_jet_ratio = p->wg_jet_ratio > 0.0 ? p->wg_jet_ratio : 0.32;
    }

    // -- Output formant/mute bandpass (RBJ bandpass) + LP + bell scale. --
    v->wg_fmt_gain = 0.0;
    if (p->wg_fmt_gain > 0.0) {
        double ff = p->wg_fmt_f;
        if (ff <= 0.0) ff = fdet;              // 0 => track the played pitch
        if (ff > sr * 0.45) ff = sr * 0.45;
        if (ff < 40.0) ff = 40.0;
        double Q = p->wg_fmt_q > 0.1 ? p->wg_fmt_q : 1.0;
        double w0 = 2.0 * M_PI * ff / sr;
        double al = sin(w0) / (2.0 * Q);
        double a0 = 1.0 + al;
        v->wg_fmt_b0 = al / a0;
        v->wg_fmt_b2 = -al / a0;
        v->wg_fmt_a1 = (-2.0 * cos(w0)) / a0;
        v->wg_fmt_a2 = (1.0 - al) / a0;
        v->wg_fmt_x1 = v->wg_fmt_x2 = v->wg_fmt_y1 = v->wg_fmt_y2 = 0.0;
        v->wg_fmt_gain = p->wg_fmt_gain;
    }
    if (p->wg_out_lp_hz > 0.0) {
        double fc = p->wg_out_lp_hz;
        if (fc > sr * 0.45) fc = sr * 0.45;
        v->wg_out_lp_g = 1.0 - exp(-2.0 * M_PI * fc / sr);
    } else {
        v->wg_out_lp_g = 0.0;
    }
    v->wg_out_lp = 0.0;
    v->wg_out_scale = p->wg_out_scale > 0.0 ? p->wg_out_scale : 1.5;

    // -- Section / tremolo amplitude LFO. --
    if (p->wg_trem_hz > 0.0) {
        double thz = voice_jitter(v, p->wg_trem_hz, 0.10, mul);
        v->wg_trem_inc = thz / sr;
        v->wg_trem_depth = p->wg_trem_depth;
        v->wg_trem_phase = voice_rand_phase(v);
    } else {
        v->wg_trem_inc = 0.0; v->wg_trem_depth = 0.0; v->wg_trem_phase = 0.0;
    }
}

// ── Batch-4 note-on helpers (ORGAN / SUPERSAW / FORMANT) ──

// One-pole LPF coefficient for a cutoff in Hz (g = 1 - exp(-2π·fc/sr)).
static inline double gm_onepole_g(double fc, double sr) {
    if (fc <= 0.0) return 0.0;
    if (fc > sr * 0.45) fc = sr * 0.45;
    return 1.0 - exp(-2.0 * M_PI * fc / sr);
}

// Additive Hammond drawbar / free-reed organ note-on. Organs are the MOST
// consistent GM family (dossier 00 §7: mul 0.3 for Hammond, 0.5 for free-reed) —
// the electromechanical machine. Variation is mostly per-bar phase (lever c, free)
// + a hair of tonewheel detune + key-click seed.
static void gm_organ_init(GMVoice *v, const GMProgramParams *p, double f0,
                          double sr) {
    const double mul = p->org_freereed ? 0.5 : 0.3;
    v->org_freereed = p->org_freereed;
    v->org_drive = p->org_drive;
    int nb = 0;
    for (int d = 0; d < 9; d++) {
        if (p->org_amps[d] <= 0.0) continue;
        double ratio = GM_ORGAN_RATIOS[d];
        double fk = f0 * ratio;
        // Free-reed musette detune is intrinsic to the patch; Hammond gets only a
        // hair of tonewheel detune (lever a, hard-capped ±6c regardless of mul).
        double spread = p->org_freereed ? p->org_detune_cents : (p->org_detune_cents + 1.0);
        fk = voice_detune(v, fk, spread, mul);
        if (fk > sr * 0.45) fk = sr * 0.45;
        v->org_inc[nb] = fk / sr;
        v->org_phase[nb] = voice_rand_phase(v);                 // lever c (free)
        v->org_amp[nb] = voice_jitter(v, p->org_amps[d], 0.10, mul); // lever b
        v->org_bar_square[nb] = p->org_reed_sq[d];
        nb++;
    }
    if (nb < 1) {  // never silent: guarantee at least the 8′ fundamental
        v->org_inc[0] = voice_detune(v, f0, 2.0, mul) / sr;
        v->org_phase[0] = voice_rand_phase(v);
        v->org_amp[0] = 1.0;
        v->org_bar_square[0] = 0;
        nb = 1;
    }
    v->org_nbars = nb;
    // Normalize so the summed registration stays bounded.
    double sum = 0.0;
    for (int i = 0; i < nb; i++) sum += fabs(v->org_amp[i]);
    if (sum < 1e-6) sum = 1.0;
    double norm = 1.0 / sum;
    for (int i = 0; i < nb; i++) v->org_amp[i] *= norm;
    // Percussive-organ 2nd/3rd harmonic ping (fast decay, B3 percussion).
    if (p->org_perc_amp > 0.0) {
        double pr = p->org_perc_ratio > 0.0 ? p->org_perc_ratio : 2.0;
        double pf = voice_detune(v, f0 * pr, 2.0, mul);
        if (pf > sr * 0.45) pf = sr * 0.45;
        v->org_perc_inc = pf / sr;
        v->org_perc_phase = voice_rand_phase(v);
        v->org_perc_amp = voice_jitter(v, p->org_perc_amp, 0.10, mul);
        v->org_perc_env = 1.0;
        double pms = p->org_perc_ms * 0.001;  if (pms < 0.001) pms = 0.001;
        v->org_perc_dec = exp(-1.0 / (pms * sr));
    } else {
        v->org_perc_amp = 0.0; v->org_perc_env = 0.0; v->org_perc_dec = 0.0;
        v->org_perc_inc = 0.0; v->org_perc_phase = 0.0;
    }
    // Key-click: short HF noise burst at note-on (contact bounce, seed varies, g).
    if (p->org_click_amp > 0.0) {
        v->org_click_amp = voice_jitter(v, p->org_click_amp, 0.20, mul);
        v->org_click_env = 1.0;
        double cms = voice_jitter(v, p->org_click_ms, 0.20, mul) * 0.001;
        if (cms < 0.0003) cms = 0.0003;
        v->org_click_dec = exp(-1.0 / (cms * sr));
        v->org_click_lp = 0.0;
    } else {
        v->org_click_amp = 0.0; v->org_click_env = 0.0; v->org_click_dec = 0.0;
    }
    // Leslie rotary: amplitude + slight pitch LFO; rate jittered, random phase.
    if (p->org_leslie_hz > 0.0) {
        double lhz = voice_jitter(v, p->org_leslie_hz, 0.10, mul);
        v->org_leslie_inc = lhz / sr;
        v->org_leslie_depth = p->org_leslie_depth;
        v->org_leslie_phase = voice_rand_phase(v);
    } else {
        v->org_leslie_inc = 0.0; v->org_leslie_depth = 0.0; v->org_leslie_phase = 0.0;
    }
    v->org_breath_amt = p->org_breath;
    v->org_breath_lp = 0.0;
    v->org_lp_g = gm_onepole_g(p->org_lp_hz, sr);
    v->org_lp = 0.0;
}

// 11th-order Szabo polynomial mapping the detune knob x∈[0,1] → spread d.
static double gm_szabo_detune(double x) {
    return 10028.7312891634*pow(x,11) - 50818.8652045924*pow(x,10)
         + 111363.4808729368*pow(x,9) - 138150.6761080548*pow(x,8)
         + 106649.6679158292*pow(x,7) - 53046.9642751875*pow(x,6)
         + 17019.9518580080*pow(x,5) - 3425.0836591318*pow(x,4)
         + 404.2703938388*pow(x,3) - 24.1878824391*pow(x,2)
         + 0.6717417634*x + 0.0030115596;
}

// Detuned multi-osc subtractive note-on: supersaw ensemble / synth lead / pad.
// Per-family mul: ensemble 0.6, lead 0.6, pad 0.5 (dossier 00 §7). Supersaw is
// built from detune; organic add is per-note phase (c) + small detune wobble (a)
// + filter-cutoff drift (j).
static void gm_supersaw_init(GMVoice *v, const GMProgramParams *p, double f0,
                             double sr) {
    double mul = 0.6;
    if (p->ss_attack_ms > 250.0) mul = 0.5;   // pad-ish → tighter
    double fc = voice_detune(v, f0, 6.0, mul);
    int n = p->ss_nosc;  if (n < 1) n = 1;  if (n > 7) n = 7;
    v->ss_nosc = n;

    if (n == 7 && p->ss_detune_x > 0.0) {
        // Szabo 7-saw supersaw: fixed relative offsets × poly-fitted spread.
        static const double off[7] = {
            -0.11002313, -0.06288439, -0.01952356, 0.0,
             0.01991221,  0.06216538,  0.10745242 };
        double d = gm_szabo_detune(clampd(p->ss_detune_x, 0.0, 1.0));
        double m = clampd(p->ss_mix_m, 0.0, 1.0);
        double centerGain = -0.55366*m + 0.99785;
        double sideGain   = -0.73764*m*m + 1.2841*m + 0.044372;
        for (int i = 0; i < 7; i++) {
            // Per-osc detune wobble (lever a) on top of the fixed Szabo spread.
            double jit = 1.0 + 0.0008 * mul * voice_rand_bipolar(v);
            double fi = fc * (1.0 + off[i] * d) * jit;
            if (fi > sr * 0.45) fi = sr * 0.45;
            v->ss_inc[i] = fi / sr;
            v->ss_phase[i] = voice_rand_phase(v);               // lever c
            v->ss_gain[i] = (i == 3) ? centerGain : sideGain;
            v->ss_square[i] = p->ss_square;
        }
    } else {
        // Simple detuned stack (lead/pad): symmetric spread around center.
        for (int i = 0; i < n; i++) {
            double frac = (n > 1) ? ((double)i / (double)(n - 1) - 0.5) * 2.0 : 0.0;
            double cents = frac * p->ss_spread_cents;
            cents += 0.5 * mul * voice_rand_bipolar(v);          // lever a wobble
            double fi = fc * cents_to_ratio(cents);
            if (i == 0 && p->ss_fifth_gain > 0.0) {}             // base stays center
            if (fi > sr * 0.45) fi = sr * 0.45;
            v->ss_inc[i] = fi / sr;
            v->ss_phase[i] = voice_rand_phase(v);
            v->ss_gain[i] = 1.0 / sqrt((double)n);
            v->ss_square[i] = p->ss_square;
        }
        // Fifths lead: replace/augment with a +7-semitone (1.5×) partner layer.
        if (p->ss_fifth_gain > 0.0 && n < 7) {
            int idx = n;
            double fi = fc * 1.5;
            if (fi > sr * 0.45) fi = sr * 0.45;
            v->ss_inc[idx] = fi / sr;
            v->ss_phase[idx] = voice_rand_phase(v);
            v->ss_gain[idx] = p->ss_fifth_gain / sqrt((double)n);
            v->ss_square[idx] = p->ss_square;
            v->ss_nosc = n + 1;
        }
    }

    // PWM LFO (square width motion).
    if (p->ss_pwm_hz > 0.0) {
        v->ss_pwm_inc = voice_jitter(v, p->ss_pwm_hz, 0.10, mul) / sr;
        v->ss_pwm_depth = p->ss_pwm_depth;
        v->ss_pwm_phase = voice_rand_phase(v);
    } else {
        v->ss_pwm_inc = 0.0; v->ss_pwm_depth = 0.0; v->ss_pwm_phase = 0.0;
    }
    // Pitch-tracked 1-pole HP at the fundamental (Szabo mud cut).
    v->ss_hp_g = gm_onepole_g(fc, sr);
    v->ss_hp_x1 = v->ss_hp_y1 = 0.0;

    // SVF resonant LPF + cutoff filter-envelope + slow sweep LFO (cutoff drift j).
    v->ss_svf_lp = v->ss_svf_bp = 0.0;
    v->ss_cut_base = voice_jitter(v, p->ss_cut0 > 0.0 ? p->ss_cut0 : 4000.0, 0.05, mul);
    v->ss_cut = v->ss_cut_base;
    v->ss_res = clampd(p->ss_res, 0.0, 0.97);
    if (p->ss_cut_env > 0.0) {
        v->ss_cut_env = p->ss_cut_env;
        v->ss_cut_env_amt = p->ss_cut_env;
        double cms = p->ss_cut_env_ms * 0.001;  if (cms < 0.001) cms = 0.001;
        v->ss_cut_env_dec = exp(-1.0 / (cms * sr));
    } else {
        v->ss_cut_env = 0.0; v->ss_cut_env_amt = 0.0; v->ss_cut_env_dec = 1.0;
    }
    if (p->ss_sweep_hz > 0.0) {
        v->ss_sweep_inc = voice_jitter(v, p->ss_sweep_hz, 0.10, mul) / sr;
        v->ss_sweep_depth = p->ss_sweep_oct;
        v->ss_sweep_phase = voice_rand_phase(v);
    } else {
        v->ss_sweep_inc = 0.0; v->ss_sweep_depth = 0.0; v->ss_sweep_phase = 0.0;
    }

    // Slow amplitude attack ramp (pads/strings); jittered (lever g).
    double atk = voice_jitter(v, p->ss_attack_ms > 0.0 ? p->ss_attack_ms : 4.0, 0.12, mul);
    if (atk < 1.0) atk = 1.0;
    v->ss_attack_env = 0.0;
    v->ss_attack_inc = 1.0 / (atk * 0.001 * sr);

    // Collective vibrato.
    if (p->ss_vib_hz > 0.0) {
        v->ss_vib_inc = voice_jitter(v, p->ss_vib_hz, 0.10, mul) / sr;
        v->ss_vib_depth = p->ss_vib_depth;
        v->ss_vib_phase = voice_rand_phase(v);
    } else {
        v->ss_vib_inc = 0.0; v->ss_vib_depth = 0.0; v->ss_vib_phase = 0.0;
    }

    // Ensemble chorus (short modulated delay line).
    memset(v->ss_chorus_buf, 0, sizeof(v->ss_chorus_buf));
    v->ss_chorus_w = 0;
    if (p->ss_chorus_mix > 0.0) {
        v->ss_chorus_inc = voice_jitter(v, p->ss_chorus_hz, 0.10, mul) / sr;
        v->ss_chorus_depth = p->ss_chorus_depth;   // seconds
        v->ss_chorus_mix = p->ss_chorus_mix;
        v->ss_chorus_phase = voice_rand_phase(v);
    } else {
        v->ss_chorus_inc = 0.0; v->ss_chorus_depth = 0.0; v->ss_chorus_mix = 0.0;
        v->ss_chorus_phase = 0.0;
    }

    v->ss_drive = p->ss_drive;

    // Sub-octave (bass+lead).
    if (p->ss_sub_mix > 0.0) {
        v->ss_sub_inc = (fc * 0.5) / sr;
        v->ss_sub_phase = voice_rand_phase(v);
        v->ss_sub_mix = p->ss_sub_mix;
        v->ss_sub_square = p->ss_sub_sq;
    } else {
        v->ss_sub_inc = 0.0; v->ss_sub_mix = 0.0; v->ss_sub_phase = 0.0;
        v->ss_sub_square = 0;
    }

    // Chiff / orchestra-hit transient noise burst through a band-pass.
    if (p->ss_chiff_amt > 0.0) {
        v->ss_chiff_amt = voice_jitter(v, p->ss_chiff_amt, 0.15, mul);
        v->ss_chiff_env = 1.0;
        double cms = p->ss_chiff_ms * 0.001;  if (cms < 0.001) cms = 0.001;
        v->ss_chiff_dec = exp(-1.0 / (cms * sr));
        double cf = (p->ss_chiff_bp > 0.0) ? (f0 * p->ss_chiff_bp) : 1500.0;
        if (cf > sr * 0.45) cf = sr * 0.45;  if (cf < 80.0) cf = 80.0;
        double Q = 2.0;
        double w0 = 2.0 * M_PI * cf / sr;
        double al = sin(w0) / (2.0 * Q);
        double a0 = 1.0 + al;
        v->ss_chiff_nb0 = al / a0;  v->ss_chiff_nb2 = -al / a0;
        v->ss_chiff_na1 = (-2.0 * cos(w0)) / a0;
        v->ss_chiff_na2 = (1.0 - al) / a0;
        v->ss_chiff_x1 = v->ss_chiff_x2 = v->ss_chiff_y1 = v->ss_chiff_y2 = 0.0;
    } else {
        v->ss_chiff_amt = 0.0; v->ss_chiff_env = 0.0; v->ss_chiff_dec = 0.0;
    }

    // Halo amplitude tremolo.
    if (p->ss_trem_hz > 0.0) {
        v->ss_trem_inc = voice_jitter(v, p->ss_trem_hz, 0.10, mul) / sr;
        v->ss_trem_depth = p->ss_trem_depth;
        v->ss_trem_phase = voice_rand_phase(v);
    } else {
        v->ss_trem_inc = 0.0; v->ss_trem_depth = 0.0; v->ss_trem_phase = 0.0;
    }

    // Orchestra-hit body decay + downward pitch blip (a fast pitch multiplier
    // that relaxes from +blip cents to 0 over ~15 ms — the "punch").
    v->ss_body_amp = 1.0;
    if (p->ss_body_ms > 0.0) {
        double bms = p->ss_body_ms * 0.001;  if (bms < 0.001) bms = 0.001;
        v->ss_body_dec = exp(-1.0 / (bms * sr));
    } else {
        v->ss_body_dec = 1.0;
    }
    if (p->ss_pitch_blip > 0.0) {
        v->ss_pitch_mult = cents_to_ratio(p->ss_pitch_blip);
        v->ss_pitch_dec = exp(-1.0 / (0.015 * sr));   // 15 ms blip
    } else {
        v->ss_pitch_mult = 1.0; v->ss_pitch_dec = 1.0;
    }
    v->ss_out_scale = p->ss_out_scale > 0.0 ? p->ss_out_scale : 0.5;
}

// Vocal formant note-on: source saws → 3 parallel formant bandpass biquads.
// Per-family mul: choir/voice 0.9 (dossier 00 §7) — per-singer detune +
// aspiration noise + decorrelated vibrato make a believable section.
static void gm_formant_init(GMVoice *v, const GMProgramParams *p, double f0,
                            double sr) {
    const double mul = 0.9;
    int n = p->fmt_nsrc;  if (n < 1) n = 1;  if (n > 3) n = 3;
    v->fmt_nsrc = n;
    for (int i = 0; i < n; i++) {
        double cents = (n > 1) ? ((double)i / (double)(n - 1) - 0.5) * 2.0
                                   * p->fmt_detune_cents : 0.0;
        // Per-singer detune (lever a) + a little extra wobble.
        double fi = voice_detune(v, f0 * cents_to_ratio(cents), 6.0, mul);
        if (fi > sr * 0.45) fi = sr * 0.45;
        v->fmt_src_inc[i] = fi / sr;
        v->fmt_src_phase[i] = voice_rand_phase(v);              // lever c
        // Decorrelated per-source vibrato (essential — else it reads as an organ).
        v->fmt_vib_inc[i] = voice_jitter(v, p->fmt_vib_hz > 0.0 ? p->fmt_vib_hz : 5.0,
                                         0.12, mul) / sr;
        v->fmt_vib_phase[i] = voice_rand_phase(v);
    }
    v->fmt_vib_depth = p->fmt_vib_depth;
    // 3 parallel formant bandpass biquads (RBJ), absolute frequencies.
    int nbands = 0;
    for (int k = 0; k < 3; k++) {
        if (p->fmt_f[k] <= 0.0) { v->fmt_g[k] = 0.0; continue; }
        double ff = p->fmt_f[k];
        // Tiny formant-frequency wobble (per-singer vocal-tract variation, b).
        ff = voice_jitter(v, ff, 0.03, mul);
        if (ff > sr * 0.45) ff = sr * 0.45;  if (ff < 40.0) ff = 40.0;
        double bw = p->fmt_bw[k] > 1.0 ? p->fmt_bw[k] : 80.0;
        double Q = ff / bw;  if (Q < 0.5) Q = 0.5;  if (Q > 40.0) Q = 40.0;
        double w0 = 2.0 * M_PI * ff / sr;
        double al = sin(w0) / (2.0 * Q);
        double a0 = 1.0 + al;
        v->fmt_b0[k] = al / a0;  v->fmt_b2[k] = -al / a0;
        v->fmt_a1[k] = (-2.0 * cos(w0)) / a0;
        v->fmt_a2[k] = (1.0 - al) / a0;
        v->fmt_x1[k] = v->fmt_x2[k] = v->fmt_y1[k] = v->fmt_y2[k] = 0.0;
        double g = pow(10.0, p->fmt_gain_db[k] / 20.0);
        v->fmt_g[k] = voice_jitter(v, g, 0.08, mul);            // lever b
        nbands = k + 1;
    }
    v->fmt_nbands = (double)nbands;
    double atk = voice_jitter(v, p->fmt_attack_ms > 0.0 ? p->fmt_attack_ms : 40.0,
                              0.12, mul);
    if (atk < 1.0) atk = 1.0;
    v->fmt_attack_env = 0.0;
    v->fmt_attack_inc = 1.0 / (atk * 0.001 * sr);
    v->fmt_breath_amt = p->fmt_breath;
    v->fmt_breath_lp = 0.0;
    // Halo tremolo (rides on the formant output amplitude).
    if (p->ss_trem_hz > 0.0) {
        v->ss_trem_inc = voice_jitter(v, p->ss_trem_hz, 0.10, mul) / sr;
        v->ss_trem_depth = p->ss_trem_depth;
        v->ss_trem_phase = voice_rand_phase(v);
    } else {
        v->ss_trem_inc = 0.0; v->ss_trem_depth = 0.0; v->ss_trem_phase = 0.0;
    }
    v->ss_out_scale = p->fmt_out_scale > 0.0 ? p->fmt_out_scale : 1.5;
}

// ── Build a 2-pole resonator (band-pass numerator) from f, pole-radius R. ──
static void gm_fx_make_res(double f, double R, double sr,
                           double *b0, double *a1, double *a2) {
    if (f > sr * 0.45) f = sr * 0.45;
    if (f < 20.0) f = 20.0;
    if (R > 0.999) R = 0.999;  if (R < 0.0) R = 0.0;
    double w = 2.0 * M_PI * f / sr;
    *a1 = -2.0 * R * cos(w);
    *a2 = R * R;
    *b0 = 1.0 - R;   // simple normalized 2-pole resonator
}

// ── RBJ band-pass biquad (constant-skirt) into the fx noise biquad slots. ──
static void gm_fx_make_bp(GMVoice *v, double f, double Q, double sr) {
    if (f > sr * 0.45) f = sr * 0.45;  if (f < 40.0) f = 40.0;
    if (Q < 0.3) Q = 0.3;  if (Q > 40.0) Q = 40.0;
    double w0 = 2.0 * M_PI * f / sr;
    double al = sin(w0) / (2.0 * Q);
    double a0 = 1.0 + al;
    v->fx_noise_bp0 = al / a0;
    v->fx_noise_bp2 = -al / a0;
    v->fx_noise_bpa1 = (-2.0 * cos(w0)) / a0;
    v->fx_noise_bpa2 = (1.0 - al) / a0;
    v->fx_noise_bx1 = v->fx_noise_bx2 = v->fx_noise_by1 = v->fx_noise_by2 = 0.0;
}

// FX note-on (GM_ENGINE_SYNTHFX + GM_ENGINE_SOUNDFX). Every lever takes bounded
// per-trigger stochasticism — these FX are inherently probabilistic, so we lean
// in (generous mul ~1.0 for the noise/particle families).
static void gm_fx_init(GMVoice *v, const GMFxParams *p, double f0, double sr) {
    const double mul = 1.0;   // FX families: lean into the spread.
    v->fx_out_scale = p->out_scale > 0.0 ? p->out_scale : 0.5;

    // Tonal core oscillators (FM modulator / detuned partners / shimmer).
    double fc = voice_detune(v, f0, 6.0, mul);
    v->fx_o1_phase = voice_rand_phase(v);
    v->fx_o1_inc = fc / sr;
    if (p->core_o2_cents != 0.0) {
        double f2 = voice_detune(v, f0 * cents_to_ratio(p->core_o2_cents), 4.0, mul);
        v->fx_o2_phase = voice_rand_phase(v);
        v->fx_o2_inc = f2 / sr;
    } else if (p->fm_ratio > 0.0) {
        // o2 doubles as the FM modulator when there's no detuned partner.
        v->fx_o2_phase = voice_rand_phase(v);
        v->fx_o2_inc = (fc * p->fm_ratio) / sr;
    }
    if (p->core_o3_cents != 0.0) {
        double f3 = voice_detune(v, f0 * cents_to_ratio(p->core_o3_cents), 5.0, mul);
        v->fx_o3_phase = voice_rand_phase(v);
        v->fx_o3_inc = f3 / sr;
    }
    if (p->fm_ratio > 0.0 && p->core_o2_cents != 0.0) {
        // Dedicated FM modulator on o3 when o2 is busy being a detuned partner.
        v->fx_o3_phase = voice_rand_phase(v);
        v->fx_o3_inc = (fc * p->fm_ratio) / sr;
    }
    if (p->fm_ratio > 0.0) {
        v->fx_fm_index = voice_jitter(v, p->fm_index0, 0.12, mul);
        v->fx_fm_index_env = p->fm_rising ? 0.0 : v->fx_fm_index;
        double idec = (p->fm_index_ms > 0.0 ? p->fm_index_ms : 300.0) * 0.001;
        if (idec < 0.001) idec = 0.001;
        v->fx_fm_index_dec = exp(-1.0 / (idec * sr));
        v->fx_fm_rising = p->fm_rising;
    }

    // SVF filter (pad / sci-fi sweeps).
    v->fx_cut_base = (p->cut0 > 0.0) ? p->cut0 : 2000.0;
    v->fx_cut = voice_jitter(v, v->fx_cut_base, 0.15, mul);
    v->fx_res = p->res;
    v->fx_svf_lp = v->fx_svf_bp = 0.0;
    if (p->cut_env > 0.0) {
        v->fx_cut_env = p->cut_env;
        double cdec = (p->cut_env_ms > 0.0 ? p->cut_env_ms : 200.0) * 0.001;
        v->fx_cut_env_dec = exp(-1.0 / (cdec * sr));
        v->fx_cut_sweep_down = p->cut_sweep_down;
    }

    // LFO + sample-and-hold.
    if (p->lfo_hz > 0.0) {
        v->fx_lfo_inc = voice_jitter(v, p->lfo_hz, 0.2, mul) / sr;
        v->fx_lfo_depth = p->lfo_depth;
        v->fx_lfo_phase = voice_rand_phase(v);
    }
    if (p->sh_hz > 0.0) {
        v->fx_sh_inc = voice_jitter(v, p->sh_hz, 0.25, mul) / sr;
        v->fx_sh_phase = voice_rand_phase(v);
        v->fx_sh_value = voice_rand_bipolar(v);
    }

    // Ring modulator.
    if (p->ring_hz > 0.0) {
        v->fx_ring_inc = voice_jitter(v, p->ring_hz, 0.15, mul) / sr;
        v->fx_ring_mix = p->ring_mix;
        v->fx_ring_phase = voice_rand_phase(v);
    }

    // Delay / echo ring.
    if (p->delay_ms > 0.0) {
        memset(v->fx_delay, 0, sizeof(v->fx_delay));
        v->fx_delay_w = 0;
        double ds = voice_jitter(v, p->delay_ms, 0.08, mul) * 0.001 * sr;
        if (ds > (double)(GM_FX_DELAY_N - 2)) ds = (double)(GM_FX_DELAY_N - 2);
        if (ds < 1.0) ds = 1.0;
        v->fx_delay_samps = ds;
        v->fx_delay_fb = p->delay_fb;
        v->fx_delay_mix = p->delay_mix;
        if (p->delay_damp > 0.0) {
            double fc2 = clampd(p->delay_damp, 100.0, sr * 0.45);
            v->fx_delay_damp = 1.0 - exp(-2.0 * M_PI * fc2 / sr);
        }
        v->fx_delay_lp = 0.0;
    }

    // Texture / filtered-noise bed.
    v->fx_noise_amt = p->noise_amt;
    if (p->noise_use_bp) {
        v->fx_noise_use_bp = 1;
        double Q = (p->res > 0.5) ? p->res : 2.0;
        gm_fx_make_bp(v, voice_jitter(v, p->cut0 > 0.0 ? p->cut0 : 1500.0, 0.12, mul),
                      Q, sr);
    } else {
        v->fx_noise_use_bp = 0;
        v->fx_noise_lp = v->fx_noise_lp2 = 0.0;
        v->fx_noise_hp_x1 = v->fx_noise_hp_y1 = 0.0;
    }

    // PhISEM particle engine.
    if (p->ph_num > 0.0) {
        v->ph_sys_decay = p->ph_sys_decay > 0.0 ? p->ph_sys_decay : 0.999;
        v->ph_snd_decay = p->ph_snd_decay > 0.0 ? p->ph_snd_decay : 0.95;
        v->ph_num = voice_jitter(v, p->ph_num, 0.1, mul);
        v->ph_gain = p->ph_gain > 0.0 ? p->ph_gain : 1.0;
        v->ph_energy = voice_jitter(v, p->ph_energy0 > 0.0 ? p->ph_energy0 : 0.3,
                                    0.2, mul);
        v->ph_energy_floor = p->ph_energy_floor;
        v->ph_snd_level = 0.0;
        int nr = p->ph_nres;  if (nr < 1) nr = 1;  if (nr > 3) nr = 3;
        v->ph_nres = nr;
        for (int k = 0; k < nr; k++) {
            double f = voice_jitter(v, p->ph_res_f[k] > 0.0 ? p->ph_res_f[k] : 1500.0,
                                    0.1, mul);
            gm_fx_make_res(f, p->ph_res_R[k], sr,
                           &v->ph_res_b0[k], &v->ph_res_a1[k], &v->ph_res_a2[k]);
            v->ph_res_y1[k] = v->ph_res_y2[k] = 0.0;
        }
        int ns = p->ph_nswell;  if (ns < 0) ns = 0;  if (ns > 3) ns = 3;
        v->ph_nswell = ns;
        for (int k = 0; k < ns; k++) {
            v->ph_swell_inc[k] = voice_jitter(v, p->ph_swell_hz[k] > 0.0
                                              ? p->ph_swell_hz[k] : 0.1, 0.3, mul) / sr;
            v->ph_swell_phase[k] = voice_rand_phase(v);
        }
        v->ph_swell_depth = p->ph_swell_depth;
    }

    // Pitch sweep (bird chirp / sci-fi zap / gunshot boom).
    if (p->pitch_start > 0.0) {
        v->fx_pitch_mult = voice_jitter(v, p->pitch_start, 0.15, mul);
        double pdec = (p->pitch_ms > 0.0 ? p->pitch_ms : 50.0) * 0.001;
        if (pdec < 0.001) pdec = 0.001;
        v->fx_pitch_dec = exp(-1.0 / (pdec * sr));
    } else {
        v->fx_pitch_mult = 1.0;
        v->fx_pitch_dec = 1.0;
    }

    // Internal AD envelopes.
    if (p->amp_ms > 0.0) {
        v->fx_amp_env = 1.0;
        double adec = p->amp_ms * 0.001;  if (adec < 0.0005) adec = 0.0005;
        v->fx_amp_dec = exp(-1.0 / (adec * sr));
    } else {
        v->fx_amp_env = 1.0;
        v->fx_amp_dec = 1.0;   // sustained
    }
    if (p->amp2_ms > 0.0) {
        v->fx_amp_env2 = 1.0;
        double adec = p->amp2_ms * 0.001;  if (adec < 0.0005) adec = 0.0005;
        v->fx_amp_dec2 = exp(-1.0 / (adec * sr));
    } else {
        v->fx_amp_env2 = 0.0;
        v->fx_amp_dec2 = 1.0;
    }

    // Gate / cadence clock.
    if (p->gate_hz > 0.0) {
        v->fx_gate_inc = voice_jitter(v, p->gate_hz, 0.08, mul) / sr;
        v->fx_gate_on_frac = p->gate_on_frac > 0.0 ? p->gate_on_frac : 0.5;
        v->fx_gate_phase = 0.0;
        v->fx_gate_max = p->gate_n;
        v->fx_gate_n = 0;
    }

    // Helicopter periodic-AM rotor.
    if (p->am_hz > 0.0) {
        v->fx_am_inc = voice_jitter(v, p->am_hz, 0.08, mul) / sr;
        v->fx_am_sharp = p->am_sharp > 0.0 ? p->am_sharp : 2.0;
        v->fx_am_depth = p->am_depth;
        v->fx_am_phase = voice_rand_phase(v);
    }

    // Boom / sub oscillator (absolute Hz).
    if (p->boom_hz > 0.0) {
        v->fx_boom_inc = voice_jitter(v, p->boom_hz, 0.1, mul) / sr;
        v->fx_boom_phase = voice_rand_phase(v);
    }

    // Reuse cut_base as base; keep noise LP/HP Hz in the BP fields when not BP.
    // (handled in generator via p-derived coefficients computed once here.)
    if (!p->noise_use_bp && p->noise_amt > 0.0) {
        // store LP coeff in fx_noise_bp0, HP coeff in fx_noise_bp2 (1-pole g's).
        double lpf = (p->noise_lp_hz > 0.0) ? p->noise_lp_hz : (sr * 0.4);
        lpf = clampd(lpf, 50.0, sr * 0.45);
        v->fx_noise_bp0 = 1.0 - exp(-2.0 * M_PI * lpf / sr);   // LP g
        if (p->noise_hp_hz > 0.0) {
            double hpf = clampd(p->noise_hp_hz, 20.0, sr * 0.4);
            v->fx_noise_bp2 = exp(-2.0 * M_PI * hpf / sr);     // HP pole
        } else {
            v->fx_noise_bp2 = 0.0;                              // no HP
        }
    }

    // ── Mode-specific absolute-frequency overrides ──
    // Several SFX have fixed pitches independent of the MIDI note.
    if (p->engine == GM_ENGINE_SOUNDFX) {
        if (p->mode == GM_SFX_TELEPHONE) {
            // Dual ring tones 440 + 480 Hz (US), tiny per-trigger detune.
            v->fx_o1_inc = voice_detune(v, 440.0, 4.0, mul) / sr;
            v->fx_o2_inc = voice_detune(v, 480.0, 4.0, mul) / sr;
            v->fx_o1_phase = voice_rand_phase(v);
            v->fx_o2_phase = voice_rand_phase(v);
        } else if (p->mode == GM_SFX_BIRD) {
            // Carrier in the 2-8 kHz bird band; modulator follows the FM ratio.
            double base = voice_jitter(v, 3500.0, 0.25, mul);  // stochastic per chirp
            base = clampd(base, 2000.0, 6500.0);
            v->fx_o1_inc = base / sr;
            v->fx_o2_inc = (base * (p->fm_ratio > 0.0 ? p->fm_ratio : 1.0)) / sr;
            v->fx_o1_phase = voice_rand_phase(v);
            v->fx_o2_phase = voice_rand_phase(v);
        }
    }
}

// Forward declare batch-2 dispatch (defined after gm_voice_init).
static int gm_voice_init_batch2(GMVoice *v, int program, double freq,
                                double sample_rate);

int gm_program_implemented(int program) {
    if (program < 0) return 0;
    if (program < GM_PIANO_PROGRAM_COUNT) return 1;           // 0-7
    if (program >= 8 && program <= 15) return 1;              // Chromatic Perc
    if (program >= 16 && program <= 23) return 1;             // Organ
    if (program >= 24 && program <= 31) return 1;             // Guitar
    if (program >= 32 && program <= 39) return 1;             // Bass
    if (program >= 40 && program <= 47) return 1;             // Strings
    if (program >= 48 && program <= 55) return 1;             // Ensemble
    if (program >= 56 && program <= 63) return 1;             // Brass
    if (program >= 80 && program <= 87) return 1;             // Synth Lead
    if (program >= 88 && program <= 95) return 1;             // Synth Pad
    if (program >= 64 && program <= 71) return 1;             // Reed
    if (program >= 72 && program <= 79) return 1;             // Pipe
    if (program >= 96 && program <= 103) return 1;            // Synth FX
    if (program >= 104 && program <= 111) return 1;           // Ethnic
    if (program >= 112 && program <= 119) return 1;           // Percussive
    if (program >= 120 && program <= 127) return 1;           // Sound FX
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
        // Pluck-position comb baked into the excitation (Jaffe-Smith), matching
        // gm_ks_big_init — keeps the pluck-position notch now that the comb no
        // longer runs in the (formerly unstable) feedback loop.
        int beta_tap = (int)(v->ks_beta * (double)n + 0.5);
        if (beta_tap >= 1 && beta_tap < n) {
            for (int i = n - 1; i >= beta_tap; i--) {
                v->bore_buf[i] -= (float)(v->ks_pick_amt * (double)v->bore_buf[i - beta_tap]);
            }
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
    if (program == 119) {   // GM 120 Reverse Cymbal — rising-swell noise.
        v->engine = GM_ENGINE_SOUNDFX;
        gm_fx_init(v, &gm_revcymbal_program, f0, sr);
        // Start at zero amplitude and rise (set in the generator via amp_env).
        v->fx_amp_env = 0.0;
        v->fx_amp_dec = exp(-1.0 / (0.7 * sr));  // ~0.7 s rise time-constant
        return 0;
    }

    // ── Strings (GM 41-48) ──
    if (program >= GM_STRINGS_FIRST && program <= GM_STRINGS_FIRST + 7) {
        int idx = program - GM_STRINGS_FIRST;   // 0..7
        if (idx <= 4) {                          // 40-44 bowed (44 = tremolo)
            v->engine = GM_ENGINE_WAVEGUIDE;
            gm_waveguide_init(v, &gm_strings_programs[idx], f0, sr);
            return 0;
        }
        if (idx == 5) {                          // 45 Pizzicato → KS
            v->engine = GM_ENGINE_PLUCK;
            gm_ks_big_init(v, &gm_pizz_program, f0, sr);
            return 0;
        }
        if (idx == 6) {                          // 46 Harp → KS
            v->engine = GM_ENGINE_PLUCK;
            gm_ks_big_init(v, &gm_harp_program, f0, sr);
            return 0;
        }
        // idx == 7 → 47 Timpani → modal
        v->engine = GM_ENGINE_MODAL;
        gm_modal_init(v, &gm_timpani_program, f0, sr);
        return 0;
    }

    // ── Brass (GM 57-64) ──
    if (program >= GM_BRASS_FIRST && program <= GM_BRASS_FIRST + 7) {
        int idx = program - GM_BRASS_FIRST;     // 0..7
        if (idx <= 5) {                          // 56-61 lip waveguide
            v->engine = GM_ENGINE_WAVEGUIDE;
            gm_waveguide_init(v, &gm_brass_programs[idx], f0, sr);
            return 0;
        }
        // 62-63 SynthBrass → subtractive
        v->engine = GM_ENGINE_SYNTHBASS;
        gm_synthbass_init(v, &gm_synthbrass_programs[idx - 6], f0, sr);
        return 0;
    }

    // ── Reed (GM 65-72) — all reed waveguide ──
    if (program >= GM_REED_FIRST && program <= GM_REED_FIRST + 7) {
        v->engine = GM_ENGINE_WAVEGUIDE;
        gm_waveguide_init(v, &gm_reed_programs[program - GM_REED_FIRST], f0, sr);
        return 0;
    }

    // ── Pipe (GM 73-80) — all flute jet waveguide ──
    if (program >= GM_PIPE_FIRST && program <= GM_PIPE_FIRST + 7) {
        v->engine = GM_ENGINE_WAVEGUIDE;
        gm_waveguide_init(v, &gm_pipe_programs[program - GM_PIPE_FIRST], f0, sr);
        return 0;
    }

    // ── Organ (GM 17-24) — additive drawbars / free-reed ──
    if (program >= GM_ORGAN_FIRST && program <= GM_ORGAN_FIRST + 7) {
        v->engine = GM_ENGINE_ORGAN;
        gm_organ_init(v, &gm_organ_programs[program - GM_ORGAN_FIRST], f0, sr);
        return 0;
    }

    // ── Ensemble (GM 49-56) — supersaw pads / vocal formant / orchestra hit ──
    if (program >= GM_ENSEMBLE_FIRST && program <= GM_ENSEMBLE_FIRST + 7) {
        int idx = program - GM_ENSEMBLE_FIRST;   // 0..7
        if (idx <= 3) {                           // 48-51 supersaw pads
            v->engine = GM_ENGINE_SUPERSAW;
            gm_supersaw_init(v, &gm_ensemble_programs[idx], f0, sr);
            return 0;
        }
        if (idx == 4) {                           // 52 Choir Aahs
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_choir_program, f0, sr);
            return 0;
        }
        if (idx == 5) {                           // 53 Voice Oohs
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_voiceoohs_program, f0, sr);
            return 0;
        }
        if (idx == 6) {                           // 54 Synth Voice
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_synthvoice_program, f0, sr);
            return 0;
        }
        // idx == 7 → 55 Orchestra Hit (supersaw cluster + transient + decay)
        v->engine = GM_ENGINE_SUPERSAW;
        gm_supersaw_init(v, &gm_ensemble_programs[4], f0, sr);
        return 0;
    }

    // ── Synth Lead (GM 81-88) — subtractive named waveforms; 86 Voice = formant ──
    if (program >= GM_LEAD_FIRST && program <= GM_LEAD_FIRST + 7) {
        int idx = program - GM_LEAD_FIRST;        // 0..7
        if (idx == 5) {                           // 86 Voice → formant lead
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_leadvoice_program, f0, sr);
            return 0;
        }
        v->engine = GM_ENGINE_SUPERSAW;
        gm_supersaw_init(v, &gm_lead_programs[idx], f0, sr);
        return 0;
    }

    // ── Synth Pad (GM 89-96) — detuned multi-osc; 92 Choir + 95 Halo = formant ──
    if (program >= GM_PAD_FIRST && program <= GM_PAD_FIRST + 7) {
        int idx = program - GM_PAD_FIRST;         // 0..7
        if (idx == 3) {                           // 92 Choir → formant pad
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_choirpad_program, f0, sr);
            return 0;
        }
        if (idx == 6) {                           // 95 Halo → bright formant + tremolo
            v->engine = GM_ENGINE_FORMANT;
            gm_formant_init(v, &gm_halo_program, f0, sr);
            return 0;
        }
        v->engine = GM_ENGINE_SUPERSAW;
        gm_supersaw_init(v, &gm_pad_programs[idx], f0, sr);
        return 0;
    }

    // ── Synth FX (GM 97-104 / 0-based 96-103) — layered sound-design ──
    if (program >= GM_SYNTHFX_FIRST &&
        program < GM_SYNTHFX_FIRST + GM_SYNTHFX_COUNT) {
        const GMFxParams *p = &gm_synthfx_programs[program - GM_SYNTHFX_FIRST];
        v->engine = GM_ENGINE_SYNTHFX;
        gm_fx_init(v, p, f0, sr);
        return 0;
    }

    // ── Sound FX (GM 121-128 / 0-based 120-127) — stochastic/noise effects ──
    if (program >= GM_SOUNDFX_FIRST &&
        program < GM_SOUNDFX_FIRST + GM_SOUNDFX_COUNT) {
        const GMFxParams *p = &gm_soundfx_programs[program - GM_SOUNDFX_FIRST];
        v->engine = GM_ENGINE_SOUNDFX;
        gm_fx_init(v, p, f0, sr);
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
    // Pluck-position comb lives in the EXCITATION (baked once into the string
    // buffer at init), NOT in the feedback loop. Applying it per-sample inside
    // the loop pushed loop gain marginally above unity and blew the string up
    // to inf/NaN at high notes. The loop is now a pure extended-KS delay +
    // loss filter — unconditionally stable for ks_loop_b in [0,1].
    double picked = delayed;
    double damp = (1.0 - v->ks_loop_b) * picked + v->ks_loop_b * v->harp_lp1;
    v->harp_lp1 = damp;
    double y = v->ks_stretch * damp;
    // Jawari (sitar/shamisen buzzing-bridge) nonlinearity colours the OUTPUT
    // only — it must NOT be fed back into the string, or it pumps energy into
    // the loop and the sitar runs away. Compute the buzz here, add it to the
    // output below; the clean `y` goes back into the delay.
    double jbuzz = 0.0;
    if (v->ks_jawari_depth > 0.0) {
        double a = fabs(y);
        if (a > v->ks_jawari_thresh) {
            double over = (a - v->ks_jawari_thresh);
            jbuzz = v->ks_jawari_depth * tanh(6.0 * over) * (y < 0.0 ? -1.0 : 1.0);
        }
    }
    // Belt-and-suspenders feedback guard. With the pluck comb moved to the
    // excitation and jawari kept output-only, the loop is now stable and this
    // never fires in the normal range — kept purely against pathological input.
    if (y > 4.0) y = 4.0; else if (y < -4.0) y = -4.0;
    buf[*wptr] = (float)y;
    *wptr = (*wptr + 1) % N;

    double s = y + jbuzz;
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

// White noise in [-1,1] from the voice PRNG (structural excitation).
static inline double wg_white(GMVoice *v) {
    return ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
}

// ── Digital waveguide: bowed / brass / reed / flute. One shared bidirectional
//    bore delay line + a mode-specific junction nonlinearity. ──
static inline double generate_waveguide_sample(GMVoice *v, double sample_rate,
                                               double env, double frequency) {
    double sr = sample_rate;
    const int N = GM_KS_BIG_N;

    // Onset ramp → drives loudness AND (for lip/reed) brightness.
    if (v->wg_attack_env < 1.0) {
        v->wg_attack_env += v->wg_attack_inc;
        if (v->wg_attack_env > 1.0) v->wg_attack_env = 1.0;
    }
    double onset = v->wg_attack_env;

    // Vibrato modulates the effective bore delay.
    double vib = 0.0;
    if (v->wg_vib_inc > 0.0) {
        vib = v->wg_vib_depth * wt_sin(v->wg_vib_phase);
        v->wg_vib_phase += v->wg_vib_inc;
        if (v->wg_vib_phase >= 1.0) v->wg_vib_phase -= 1.0;
    }

    // Track host frequency (glissando / pitch glide) while honoring the baked
    // bore multiplier (brass half-wave). Re-derive delay each sample (cheap).
    double bore_mult = (v->wg_mode == GM_WG_LIP) ? 2.0 : 1.0;
    // Clarinet (cylindrical, inverting bore): a closed-open cylinder resonates
    // at λ/4, so a full-wavelength bore plays an OCTAVE LOW. Halve the bore so
    // the played pitch lands on the requested note (verified: −1198¢ → ~0¢).
    // The conical reeds (sax/oboe/bassoon, wg_bore_invert=0) are already right.
    if (v->wg_mode == GM_WG_REED && v->wg_bore_invert) bore_mult = 0.5;
    double delay = v->wg_base_delay;
    if (frequency > 20.0) {
        double f = clampd(frequency, 20.0, sr * 0.20);
        double d = (sr / f) * bore_mult;
        if (v->wg_mode == GM_WG_LIP) d += 3.0;
        // (bowed: the old −4 group-delay comp was tuned for the broken
        // single-buffer topology; the two-delay-line rewrite needs no offset.)
        delay = d;
    }
    delay *= (1.0 + vib);
    if (delay < 4.0) delay = 4.0;
    if (delay > (double)(N - 4)) delay = (double)(N - 4);

    double white = wg_white(v);
    double out = 0.0;

    if (v->wg_mode == GM_WG_BOWED) {
        // STK BowedString proper: TWO travelling-wave delay lines meeting at the
        // bow. ks_buf carries the neck segment (bow→nut→bow), bore_buf the
        // bridge segment (bow→bridge→bow); their lengths sum to the full string
        // so the fundamental is SR/(neck+bridge) = the requested pitch. The
        // friction junction injects velocity between them.
        double neck_delay   = delay * (1.0 - v->wg_bow_beta);   // longer → ks_buf
        double bridge_delay = delay * v->wg_bow_beta;           // shorter → bore_buf
        if (neck_delay < 1.0) neck_delay = 1.0;
        if (neck_delay > (double)(N - 4)) neck_delay = (double)(N - 4);
        if (bridge_delay < 1.0) bridge_delay = 1.0;
        if (bridge_delay > (double)(GM_BORE_N - 4)) bridge_delay = (double)(GM_BORE_N - 4);

        double bow_velocity = v->wg_breath_max * onset
                              * (1.0 + v->wg_noise_gain * white * (1.0 - onset));

        // Read each segment's returning wave (lastOut, before we write).
        double neck_out   = gm_frac_read(v->ks_buf,   N,          v->wg_w,   neck_delay);
        double bridge_out = gm_frac_read(v->bore_buf, GM_BORE_N,  v->bore_w, bridge_delay);
        // String (loop-loss) filter on the bridge reflection; nut is rigid.
        v->wg_loop_lp = (1.0 - v->wg_loop_damp) * bridge_out + v->wg_loop_damp * v->wg_loop_lp;
        double bridge_refl = -v->wg_loop_lp;
        double nut_refl    = -neck_out;
        double string_vel  = bridge_refl + nut_refl;     // velocity at the bow
        double dv = bow_velocity - string_vel;
        // STK BowTable friction: pow(|slope·dv|+0.75, -4), clamped [0.01,0.98].
        double bt = fabs(v->wg_bow_slope * dv) + 0.75;
        bt = pow(bt, -4.0);
        if (bt < 0.01) bt = 0.01;
        if (bt > 0.98) bt = 0.98;
        double new_vel = dv * bt;
        // Scatter back into BOTH lines (STK: neck ← bridgeRefl+newVel,
        // bridge ← nutRefl+newVel) and advance each line's own pointer.
        v->ks_buf[v->wg_w]   = (float)(bridge_refl + new_vel);
        v->wg_w   = (v->wg_w + 1) % N;
        v->bore_buf[v->bore_w] = (float)(nut_refl + new_vel);
        v->bore_w = (v->bore_w + 1) % GM_BORE_N;
        // Output the bridge tap (drives the body) through a DC blocker.
        double y = bridge_out - v->wg_hp_x1 + 0.995 * v->wg_hp_y1;
        v->wg_hp_x1 = bridge_out; v->wg_hp_y1 = y;
        out = y;
        // Parallel multi-mode body resonator (air + wood modes) — the woody
        // richness that makes it read as a real bowed instrument rather than a
        // hollow tube. Same stable bank the plucked strings use; on the OUTPUT
        // only (not in the string loop) so it never affects pitch/stability.
        for (int bi = 0; bi < v->ks_body_n; bi++) {
            double by = v->ks_body_g[bi] * out
                        - v->ks_body_a1[bi] * v->ks_body_y1[bi]
                        - v->ks_body_a2[bi] * v->ks_body_y2[bi];
            v->ks_body_y2[bi] = v->ks_body_y1[bi];
            v->ks_body_y1[bi] = by;
            out += by;
        }
    } else if (v->wg_mode == GM_WG_LIP) {
        // STK Brass: breath pressure → lip-resonance biquad → quadratic valve.
        double breath = v->wg_breath_max * onset;
        breath += v->wg_noise_gain * white * 0.05;   // breath turbulence
        double mouth = 0.3 * breath;
        double bore_out = gm_frac_read(v->ks_buf, N, v->wg_w, delay);
        v->wg_loop_lp = (1.0 - v->wg_loop_damp) * bore_out
                        + v->wg_loop_damp * v->wg_loop_lp;
        double bore_pressure = 0.85 * v->wg_loop_lp;
        double dp = mouth - bore_pressure;
        // Lip resonance biquad (tracks f0).
        double lf = v->wg_lip_b0 * dp + v->wg_lip_b1 * v->wg_lip_x1
                    + v->wg_lip_b2 * v->wg_lip_x2
                    - v->wg_lip_a1 * v->wg_lip_y1 - v->wg_lip_a2 * v->wg_lip_y2;
        v->wg_lip_x2 = v->wg_lip_x1; v->wg_lip_x1 = dp;
        v->wg_lip_y2 = v->wg_lip_y1; v->wg_lip_y1 = lf;
        double opening = dp + v->wg_lip_gain * lf;
        opening = opening * opening;                 // quadratic NL (pressure→area)
        if (opening > 1.0) opening = 1.0;            // valve opens only so far
        double mix = opening * mouth + (1.0 - opening) * bore_pressure;
        double y = mix - v->wg_hp_x1 + 0.995 * v->wg_hp_y1;  // DC block
        v->wg_hp_x1 = mix; v->wg_hp_y1 = y;
        v->ks_buf[v->wg_w] = (float)y;
        v->wg_w = (v->wg_w + 1) % N;
        out = y;
    } else if (v->wg_mode == GM_WG_REED) {
        // STK ReedTable bore: breath → reed reflection table → bore round trip.
        double pTarget = 0.55 + 0.45 * onset;
        v->wg_breath += (pTarget - v->wg_breath) * 0.01;
        double Pm = v->wg_breath * v->wg_breath_max
                    * (1.0 + v->wg_noise_gain * white);
        double bore_out = gm_frac_read(v->ks_buf, N, v->wg_w, delay);
        v->wg_loop_lp = (1.0 - v->wg_loop_damp) * bore_out
                        + v->wg_loop_damp * v->wg_loop_lp;
        // Clarinet (cylindrical) inverts → odd harmonics; conical does not.
        double refl = v->wg_bore_invert ? -v->wg_loop_lp : v->wg_loop_lp;
        double pDiff = Pm - refl;
        double reedRefl = v->wg_reed_offset + v->wg_reed_slope * pDiff;
        if (reedRefl > 1.0) reedRefl = 1.0;
        if (reedRefl < -1.0) reedRefl = -1.0;
        double into_bore = refl + reedRefl * pDiff;
        double y = into_bore - v->wg_hp_x1 + 0.995 * v->wg_hp_y1;  // DC block
        v->wg_hp_x1 = into_bore; v->wg_hp_y1 = y;
        v->ks_buf[v->wg_w] = (float)y;
        v->wg_w = (v->wg_w + 1) % N;
        // STK reed output scaling (dossier 03) + soft saturation. Reeds genuinely
        // overblow brighter at high notes; tanh self-limits instead of railing.
        out = tanh(0.3 * y);
    } else { // GM_WG_JET — Cook flute: jet delay + cubic + blowing noise.
        double jet_delay = delay * v->wg_jet_ratio;
        if (jet_delay < 1.0) jet_delay = 1.0;
        double breath = v->wg_breath_max * onset;
        // Stronger chiff at onset (1-env style turbulence weighting).
        double turb = v->wg_noise_gain * white * (0.4 + 0.6 * (1.0 - onset));
        double excitation = breath + breath * turb;
        double bore_out = gm_frac_read(v->ks_buf, N, v->wg_w, delay);
        v->wg_loop_lp = (1.0 - v->wg_loop_damp) * bore_out
                        + v->wg_loop_damp * v->wg_loop_lp;
        double feedback = v->wg_loop_lp;
        // Jet pressure: excitation + a reflected portion of the bore.
        double jet_in = excitation + feedback * 0.6;
        // Read jet delay from the same line one wavelength fraction back.
        double jet = gm_frac_read(v->ks_buf, N, v->wg_w, jet_delay);
        double pd = jet_in - 0.5 * jet;
        // Cubic limit-cycle nonlinearity (Cook flute), clamped.
        if (pd > 1.0) pd = 1.0; else if (pd < -1.0) pd = -1.0;
        double nl = pd * (pd * pd - 1.0);
        double into_bore = nl + feedback;
        double y = into_bore - v->wg_hp_x1 + 0.995 * v->wg_hp_y1;  // DC block
        v->wg_hp_x1 = into_bore; v->wg_hp_y1 = y;
        v->ks_buf[v->wg_w] = (float)y;
        v->wg_w = (v->wg_w + 1) % N;
        out = y;
    }

    // -- Output shaping: formant/mute bandpass, LP, bell scale. --
    if (v->wg_fmt_gain > 0.0) {
        double fb = v->wg_fmt_b0 * out + v->wg_fmt_b2 * v->wg_fmt_x2
                    - v->wg_fmt_a1 * v->wg_fmt_y1 - v->wg_fmt_a2 * v->wg_fmt_y2;
        v->wg_fmt_x2 = v->wg_fmt_x1; v->wg_fmt_x1 = out;
        v->wg_fmt_y2 = v->wg_fmt_y1; v->wg_fmt_y1 = fb;
        out += v->wg_fmt_gain * fb;
    }
    if (v->wg_out_lp_g > 0.0) {
        v->wg_out_lp += v->wg_out_lp_g * (out - v->wg_out_lp);
        out = v->wg_out_lp;
    }
    // Section / tremolo amplitude LFO.
    if (v->wg_trem_inc > 0.0) {
        double trem = 1.0 - v->wg_trem_depth * 0.5 * (1.0 - wt_sin(v->wg_trem_phase));
        v->wg_trem_phase += v->wg_trem_inc;
        if (v->wg_trem_phase >= 1.0) v->wg_trem_phase -= 1.0;
        out *= trem;
    }

    out = clampd(out * v->wg_out_scale, -2.0, 2.0);
    return out * env;
}

// ── Additive Hammond drawbar / free-reed organ. Sum of sine partials at fixed
//    footage ratios (or detuned saw/pulse reed banks) + key-click + percussion
//    ping + Leslie rotary. Sustained — no decay on the drawbars. ──
static inline double generate_organ_sample(GMVoice *v, double sample_rate,
                                            double env) {
    (void)sample_rate;
    double s = 0.0;
    int N = v->org_nbars;
    for (int d = 0; d < N; d++) {
        double w;
        if (v->org_freereed) {
            // Free-reed banks: bandlimited-ish saw / pulse for the buzzy reed tone.
            if (v->org_bar_square[d]) {
                w = v->org_phase[d] < 0.5 ? 1.0 : -1.0;
            } else {
                w = 2.0 * v->org_phase[d] - 1.0;
            }
        } else {
            w = wt_sin(v->org_phase[d]);     // Hammond = pure sine drawbars
        }
        s += v->org_amp[d] * w;
        v->org_phase[d] += v->org_inc[d];
        if (v->org_phase[d] >= 1.0) v->org_phase[d] -= 1.0;
    }
    // Percussive-organ ping (fast-decaying extra harmonic at note-on).
    if (v->org_perc_env > 0.0001) {
        s += v->org_perc_amp * v->org_perc_env * wt_sin(v->org_perc_phase);
        v->org_perc_phase += v->org_perc_inc;
        if (v->org_perc_phase >= 1.0) v->org_perc_phase -= 1.0;
        v->org_perc_env *= v->org_perc_dec;
    }
    // Free-reed breath noise (filtered) + gentle output LPF.
    if (v->org_breath_amt > 0.0) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        v->org_breath_lp = 0.2 * white + 0.8 * v->org_breath_lp;
        s += v->org_breath_amt * v->org_breath_lp;
    }
    if (v->org_lp_g > 0.0) {
        v->org_lp += v->org_lp_g * (s - v->org_lp);
        s = v->org_lp;
    }
    // Rock-organ overdrive.
    if (v->org_drive > 0.0) {
        double pre = 1.0 + 4.0 * v->org_drive;
        s = tanh(pre * s) * (1.0 / pre) * (1.0 + v->org_drive);
    }
    // Key-click: short HF noise burst at note-on (HF-tilted via differencing).
    if (v->org_click_env > 0.0001) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double hf = white - v->org_click_lp;          // crude HP (HF emphasis)
        v->org_click_lp = white;
        s += v->org_click_amp * v->org_click_env * hf;
        v->org_click_env *= v->org_click_dec;
    }
    // Leslie rotary: amplitude tremolo (the pitch/Doppler part is approximated by
    // the amplitude swirl here; bounded depth).
    if (v->org_leslie_inc > 0.0) {
        double l = 1.0 - v->org_leslie_depth * 0.5 * (1.0 - wt_sin(v->org_leslie_phase));
        v->org_leslie_phase += v->org_leslie_inc;
        if (v->org_leslie_phase >= 1.0) v->org_leslie_phase -= 1.0;
        s *= l;
    }
    return clampd(s, -1.8, 1.8) * env;
}

// One naive saw or PWM-square oscillator value for the supersaw stack.
static inline double gm_ss_osc(double phase, int square, double pw) {
    if (square) return phase < pw ? 1.0 : -1.0;
    return 2.0 * phase - 1.0;
}

// ── Detuned multi-osc subtractive: supersaw ensemble / synth lead / synth pad.
//    7-saw Szabo stack (or simple detuned stack) → pitch-HP → resonant SVF LPF
//    (with attack filter-envelope + slow sweep LFO) → chorus → drive. ──
static inline double generate_supersaw_sample(GMVoice *v, double sample_rate,
                                               double env) {
    double sr = sample_rate;
    // Slow amplitude attack ramp.
    if (v->ss_attack_env < 1.0) {
        v->ss_attack_env += v->ss_attack_inc;
        if (v->ss_attack_env > 1.0) v->ss_attack_env = 1.0;
    }
    // Collective vibrato + orchestra-hit pitch blip → a global pitch multiplier.
    double pmult = 1.0;
    if (v->ss_vib_inc > 0.0) {
        pmult *= 1.0 + v->ss_vib_depth * wt_sin(v->ss_vib_phase);
        v->ss_vib_phase += v->ss_vib_inc;
        if (v->ss_vib_phase >= 1.0) v->ss_vib_phase -= 1.0;
    }
    if (v->ss_pitch_mult > 1.0) {
        pmult *= v->ss_pitch_mult;
        v->ss_pitch_mult = 1.0 + (v->ss_pitch_mult - 1.0) * v->ss_pitch_dec;
        if (v->ss_pitch_mult < 1.000001) v->ss_pitch_mult = 1.0;
    }
    // PWM width (square oscillators).
    double pw = 0.5;
    if (v->ss_pwm_inc > 0.0) {
        pw = 0.5 + v->ss_pwm_depth * 0.45 * wt_sin(v->ss_pwm_phase);
        v->ss_pwm_phase += v->ss_pwm_inc;
        if (v->ss_pwm_phase >= 1.0) v->ss_pwm_phase -= 1.0;
        if (pw < 0.05) pw = 0.05;  if (pw > 0.95) pw = 0.95;
    }
    // Sum the detuned oscillator stack.
    double sig = 0.0;
    int n = v->ss_nosc;
    for (int i = 0; i < n; i++) {
        sig += v->ss_gain[i] * gm_ss_osc(v->ss_phase[i], v->ss_square[i], pw);
        v->ss_phase[i] += v->ss_inc[i] * pmult;
        if (v->ss_phase[i] >= 1.0) v->ss_phase[i] -= 1.0;
    }
    // Sub-octave (bass+lead).
    if (v->ss_sub_mix > 0.0) {
        sig += v->ss_sub_mix * gm_ss_osc(v->ss_sub_phase, v->ss_sub_square, 0.5);
        v->ss_sub_phase += v->ss_sub_inc * pmult;
        if (v->ss_sub_phase >= 1.0) v->ss_sub_phase -= 1.0;
    }
    // Pitch-tracked 1-pole highpass (Szabo mud cut).
    if (v->ss_hp_g > 0.0) {
        double lp = v->ss_hp_y1 + v->ss_hp_g * (sig - v->ss_hp_y1);
        v->ss_hp_y1 = lp;
        sig = sig - lp;     // highpass = signal − lowpass
    }
    // Chiff / orchestra-hit transient (band-passed noise burst at onset).
    if (v->ss_chiff_env > 0.0001) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double nf = v->ss_chiff_nb0 * white + v->ss_chiff_nb2 * v->ss_chiff_x2
                    - v->ss_chiff_na1 * v->ss_chiff_y1 - v->ss_chiff_na2 * v->ss_chiff_y2;
        v->ss_chiff_x2 = v->ss_chiff_x1; v->ss_chiff_x1 = white;
        v->ss_chiff_y2 = v->ss_chiff_y1; v->ss_chiff_y1 = nf;
        sig += v->ss_chiff_amt * v->ss_chiff_env * nf;
        v->ss_chiff_env *= v->ss_chiff_dec;
    }

    // Cutoff: base + decaying attack envelope + slow sweep LFO (octaves).
    if (v->ss_cut_env > 0.1) v->ss_cut_env *= v->ss_cut_env_dec;
    double cut = v->ss_cut_base + v->ss_cut_env;
    if (v->ss_sweep_inc > 0.0) {
        double oct = v->ss_sweep_depth * wt_sin(v->ss_sweep_phase);
        v->ss_sweep_phase += v->ss_sweep_inc;
        if (v->ss_sweep_phase >= 1.0) v->ss_sweep_phase -= 1.0;
        cut *= pow(2.0, oct);
    }
    cut = clampd(cut, 30.0, sr * 0.45);
    // Chamberlin SVF (f = 2·sin(π·fc/sr), q = 1/res-ish → use 2(1-res) damping).
    double f = 2.0 * sin(M_PI * cut / sr);  if (f > 1.0) f = 1.0;
    double q = 2.0 * (1.0 - v->ss_res);     // resonance: smaller q = more peak
    double hp = sig - v->ss_svf_lp - q * v->ss_svf_bp;
    v->ss_svf_bp += f * hp;
    v->ss_svf_lp += f * v->ss_svf_bp;
    double out = v->ss_svf_lp;

    // Charang / overdrive.
    if (v->ss_drive > 0.0) {
        double pre = 1.0 + 5.0 * v->ss_drive;
        out = tanh(pre * out) * (1.0 / pre) * (1.0 + v->ss_drive);
    }

    // Ensemble chorus (short modulated delay line, mixed with dry).
    if (v->ss_chorus_mix > 0.0) {
        const int CN = 1024;
        v->ss_chorus_buf[v->ss_chorus_w] = (float)out;
        double depth_samp = v->ss_chorus_depth * sr;        // seconds → samples
        double base = depth_samp + 2.0;
        double mod = base + depth_samp * wt_sin(v->ss_chorus_phase);
        v->ss_chorus_phase += v->ss_chorus_inc;
        if (v->ss_chorus_phase >= 1.0) v->ss_chorus_phase -= 1.0;
        if (mod < 1.0) mod = 1.0;  if (mod > (double)(CN - 2)) mod = (double)(CN - 2);
        double wet = gm_frac_read(v->ss_chorus_buf, CN, v->ss_chorus_w, mod);
        v->ss_chorus_w = (v->ss_chorus_w + 1) % CN;
        out = (1.0 - v->ss_chorus_mix) * out + v->ss_chorus_mix * wet;
    }

    // Halo amplitude tremolo.
    if (v->ss_trem_inc > 0.0) {
        double t = 1.0 - v->ss_trem_depth * 0.5 * (1.0 - wt_sin(v->ss_trem_phase));
        v->ss_trem_phase += v->ss_trem_inc;
        if (v->ss_trem_phase >= 1.0) v->ss_trem_phase -= 1.0;
        out *= t;
    }

    // Orchestra-hit body decay (percussive stab; 1.0 = sustained).
    if (v->ss_body_dec < 1.0) {
        out *= v->ss_body_amp;
        v->ss_body_amp *= v->ss_body_dec;
    }

    out *= v->ss_out_scale * v->ss_attack_env;
    return clampd(out, -2.0, 2.0) * env;
}

// ── Vocal formant bank: detuned saw sources → 3 parallel formant bandpass
//    biquads, summed with the vowel-table gains. Choir / voice / synth-voice. ──
static inline double generate_formant_sample(GMVoice *v, double sample_rate,
                                              double env) {
    (void)sample_rate;
    if (v->fmt_attack_env < 1.0) {
        v->fmt_attack_env += v->fmt_attack_inc;
        if (v->fmt_attack_env > 1.0) v->fmt_attack_env = 1.0;
    }
    // Buzz source: sum of detuned saws, each with decorrelated vibrato.
    double src = 0.0;
    int n = v->fmt_nsrc;
    for (int i = 0; i < n; i++) {
        double vib = 1.0 + v->fmt_vib_depth * wt_sin(v->fmt_vib_phase[i]);
        v->fmt_vib_phase[i] += v->fmt_vib_inc[i];
        if (v->fmt_vib_phase[i] >= 1.0) v->fmt_vib_phase[i] -= 1.0;
        src += (2.0 * v->fmt_src_phase[i] - 1.0);
        v->fmt_src_phase[i] += v->fmt_src_inc[i] * vib;
        if (v->fmt_src_phase[i] >= 1.0) v->fmt_src_phase[i] -= 1.0;
    }
    src /= (double)n;
    // Aspiration / breath noise into the formants (the airy choral texture).
    if (v->fmt_breath_amt > 0.0) {
        double white = ((double)xorshift32(&v->rng_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        v->fmt_breath_lp = 0.3 * white + 0.7 * v->fmt_breath_lp;
        src += v->fmt_breath_amt * v->fmt_breath_lp;
    }
    // 3 parallel formant bandpass biquads, summed with vowel gains.
    int nb = (int)v->fmt_nbands;
    double out = 0.0;
    for (int k = 0; k < nb; k++) {
        if (v->fmt_g[k] <= 0.0) continue;
        double fb = v->fmt_b0[k] * src + v->fmt_b2[k] * v->fmt_x2[k]
                    - v->fmt_a1[k] * v->fmt_y1[k] - v->fmt_a2[k] * v->fmt_y2[k];
        v->fmt_x2[k] = v->fmt_x1[k]; v->fmt_x1[k] = src;
        v->fmt_y2[k] = v->fmt_y1[k]; v->fmt_y1[k] = fb;
        out += v->fmt_g[k] * fb;
    }
    // Halo tremolo (rides on the output amplitude).
    if (v->ss_trem_inc > 0.0) {
        double t = 1.0 - v->ss_trem_depth * 0.5 * (1.0 - wt_sin(v->ss_trem_phase));
        v->ss_trem_phase += v->ss_trem_inc;
        if (v->ss_trem_phase >= 1.0) v->ss_trem_phase -= 1.0;
        out *= t;
    }
    out *= v->ss_out_scale * v->fmt_attack_env;
    return clampd(out, -2.0, 2.0) * env;
}

// ── Shared FX helpers ──────────────────────────────────────────────────────

// PhISEM one-sample tick: leak shake energy, stochastically fire collision
// grains, ring them through the resonator bank. Inherently stochastic — the
// random particle timing IS the stochasticism (no two renders alike).
static inline double gm_phisem_tick(GMVoice *v) {
    // Slow swell LFOs re-pump energy (ocean waves / applause swell).
    double swell = 0.0;
    for (int k = 0; k < v->ph_nswell; k++) {
        swell += 0.5 * (1.0 + wt_sin(v->ph_swell_phase[k]));
        v->ph_swell_phase[k] += v->ph_swell_inc[k];
        if (v->ph_swell_phase[k] >= 1.0) v->ph_swell_phase[k] -= 1.0;
    }
    if (v->ph_nswell > 0) {
        swell /= (double)v->ph_nswell;
        double tgt = v->ph_energy_floor + v->ph_swell_depth * swell;
        v->ph_energy += 0.0005 * (tgt - v->ph_energy);  // ease energy toward swell
    }
    v->ph_energy *= v->ph_sys_decay;
    if (v->ph_energy < v->ph_energy_floor) v->ph_energy = v->ph_energy_floor;

    // Poisson-ish collision: probability ∝ numObjects · energy. On a collision,
    // inject a normalized impulse (energy sets density, not grain loudness).
    double prob = v->ph_num * v->ph_energy;
    if (voice_rand_unit(v) < prob) {
        v->ph_snd_level += 1.0;
    }
    v->ph_snd_level *= v->ph_snd_decay;
    double grain = v->ph_snd_level * voice_rand_bipolar(v);

    double out = 0.0;
    for (int k = 0; k < v->ph_nres; k++) {
        double y = v->ph_res_b0[k] * grain
                 - v->ph_res_a1[k] * v->ph_res_y1[k]
                 - v->ph_res_a2[k] * v->ph_res_y2[k];
        v->ph_res_y2[k] = v->ph_res_y1[k];
        v->ph_res_y1[k] = y;
        out += y;
    }
    return out * v->ph_gain;
}

// Filtered-noise texture bed (LP cascade + optional HP, or BP biquad). Returns
// shaped noise in roughly [-1,1]. Coefficients are baked in gm_fx_init.
static inline double gm_fx_noise_bed(GMVoice *v) {
    double w = wg_white(v);
    if (v->fx_noise_use_bp) {
        double y = v->fx_noise_bp0 * w + v->fx_noise_bp2 * v->fx_noise_bx2
                 - v->fx_noise_bpa1 * v->fx_noise_by1
                 - v->fx_noise_bpa2 * v->fx_noise_by2;
        v->fx_noise_bx2 = v->fx_noise_bx1; v->fx_noise_bx1 = w;
        v->fx_noise_by2 = v->fx_noise_by1; v->fx_noise_by1 = y;
        return y;
    }
    // 1-pole LP coeff held in fx_noise_bp0; HP pole in fx_noise_bp2.
    double lpg = v->fx_noise_bp0 > 0.0 ? v->fx_noise_bp0 : 0.3;
    v->fx_noise_lp += lpg * (w - v->fx_noise_lp);
    v->fx_noise_lp2 += lpg * (v->fx_noise_lp - v->fx_noise_lp2);
    double s = v->fx_noise_lp2;
    if (v->fx_noise_bp2 > 0.0) {   // 1-pole HP via leaky differentiator
        double a = v->fx_noise_bp2;
        double y = a * (v->fx_noise_hp_y1 + s - v->fx_noise_hp_x1);
        v->fx_noise_hp_x1 = s;
        v->fx_noise_hp_y1 = y;
        s = y;
    }
    return s;
}

// Per-voice delay/echo: read wet, mix dry+feedback back into the ring (with
// optional HF damping so repeats dull as they fade).
static inline double gm_fx_delay_tick(GMVoice *v, double dry) {
    double wet = gm_frac_read(v->fx_delay, GM_FX_DELAY_N, v->fx_delay_w,
                              v->fx_delay_samps);
    if (v->fx_delay_damp > 0.0) {   // damp the feedback path
        v->fx_delay_lp += v->fx_delay_damp * (wet - v->fx_delay_lp);
        wet = v->fx_delay_lp;
    }
    double into = dry + v->fx_delay_fb * wet;
    if (into > 4.0) into = 4.0;  if (into < -4.0) into = -4.0;
    v->fx_delay[v->fx_delay_w] = (float)into;
    v->fx_delay_w = (v->fx_delay_w + 1) % GM_FX_DELAY_N;
    return dry * (1.0 - v->fx_delay_mix) + wet * v->fx_delay_mix;
}

// Advance + read the LFO (returns [-1,1]); advance the S&H clock.
static inline double gm_fx_lfo_tri(GMVoice *v) {
    double t = v->fx_lfo_phase;             // triangle in [-1,1]
    double tri = (t < 0.5) ? (4.0 * t - 1.0) : (3.0 - 4.0 * t);
    v->fx_lfo_phase += v->fx_lfo_inc;
    if (v->fx_lfo_phase >= 1.0) v->fx_lfo_phase -= 1.0;
    return tri;
}
static inline double gm_fx_sh(GMVoice *v) {
    v->fx_sh_phase += v->fx_sh_inc;
    if (v->fx_sh_phase >= 1.0) {
        v->fx_sh_phase -= 1.0;
        v->fx_sh_value = voice_rand_bipolar(v);  // resample (stochastic)
    }
    return v->fx_sh_value;
}

// Chamberlin SVF low-pass step. `cut` Hz, resonance v->fx_res.
static inline double gm_fx_svf_lp(GMVoice *v, double in, double cut, double sr) {
    cut = clampd(cut, 20.0, sr * 0.45);
    double f = 2.0 * sin(M_PI * cut / sr);
    if (f > 1.4) f = 1.4;
    double q = 1.0 - v->fx_res;  if (q < 0.05) q = 0.05;
    double hp = in - v->fx_svf_lp - q * v->fx_svf_bp;
    v->fx_svf_bp += f * hp;
    v->fx_svf_lp += f * v->fx_svf_bp;
    return v->fx_svf_lp;
}

// ── GM_ENGINE_SYNTHFX: layered tonal-core + texture + time-effect ──
static inline double generate_synthfx_sample(GMVoice *v, double sample_rate,
                                             double env) {
    double sr = sample_rate;
    double s = 0.0;

    if (v->engine == GM_ENGINE_SYNTHFX && v->program == 96) {
        // FX1 rain: PhISEM droplets + ring-modded detuned shimmer pair (+S&H bend).
        double drops = gm_phisem_tick(v);
        double bend = 1.0 + 0.01 * gm_fx_sh(v);   // subtle pitch S&H on shimmer
        double sh1 = wt_sin(v->fx_o1_phase);
        double sh2 = wt_sin(v->fx_o2_phase);
        v->fx_o1_phase += v->fx_o1_inc * bend;  if (v->fx_o1_phase >= 1.0) v->fx_o1_phase -= 1.0;
        v->fx_o2_phase += v->fx_o2_inc * bend;  if (v->fx_o2_phase >= 1.0) v->fx_o2_phase -= 1.0;
        double shimmer = 0.5 * (sh1 + sh2);
        double ring = wt_sin(v->fx_ring_phase);
        v->fx_ring_phase += v->fx_ring_inc;  if (v->fx_ring_phase >= 1.0) v->fx_ring_phase -= 1.0;
        shimmer = (1.0 - v->fx_ring_mix) * shimmer + v->fx_ring_mix * shimmer * ring;
        s = drops + 0.18 * shimmer;
        return s * v->fx_out_scale * env;
    }

    // Generic layered FX for soundtrack/crystal/atmosphere/brightness/goblins/
    // echoes/sci-fi. Build a tonal core, optionally FM/ring/filter/delay it,
    // add a texture bed, apply internal envelopes.
    double pmult = v->fx_pitch_mult;
    if (v->fx_pitch_dec < 1.0) v->fx_pitch_mult = 1.0 + (v->fx_pitch_mult - 1.0) * v->fx_pitch_dec;

    // FM index envelope (rising for brightness, decaying otherwise).
    double fmidx = v->fx_fm_index;
    if (v->fx_fm_index_dec > 0.0 && v->fx_fm_index_dec < 1.0) {
        if (v->fx_fm_rising) {
            v->fx_fm_index_env += (v->fx_fm_index - v->fx_fm_index_env)
                                   * (1.0 - v->fx_fm_index_dec);
            fmidx = v->fx_fm_index_env;
        } else {
            v->fx_fm_index_env *= v->fx_fm_index_dec;
            fmidx = v->fx_fm_index_env;
        }
    }

    // Modulator (o2 if it carries FM, else o3) → carrier.
    double core;
    if (v->fx_fm_index_dec != 0.0 && v->fx_fm_index > 0.0) {
        double modph = (v->fx_o3_inc > 0.0) ? v->fx_o3_phase : v->fx_o2_phase;
        double fmod = fmidx * wt_sin(modph);
        if (v->fx_o3_inc > 0.0) {
            v->fx_o3_phase += v->fx_o3_inc; if (v->fx_o3_phase >= 1.0) v->fx_o3_phase -= 1.0;
        } else {
            v->fx_o2_phase += v->fx_o2_inc; if (v->fx_o2_phase >= 1.0) v->fx_o2_phase -= 1.0;
        }
        core = wt_sin(v->fx_o1_phase + fmod);
    } else {
        // Detuned-saw pad core (1-3 saws).
        double saw = 2.0 * v->fx_o1_phase - 1.0;
        if (v->fx_o2_inc > 0.0) {
            saw += 2.0 * v->fx_o2_phase - 1.0;
            v->fx_o2_phase += v->fx_o2_inc * pmult; if (v->fx_o2_phase >= 1.0) v->fx_o2_phase -= 1.0;
        }
        if (v->fx_o3_inc > 0.0) {
            saw += 2.0 * v->fx_o3_phase - 1.0;
            v->fx_o3_phase += v->fx_o3_inc * pmult; if (v->fx_o3_phase >= 1.0) v->fx_o3_phase -= 1.0;
        }
        core = saw * 0.4;
    }
    v->fx_o1_phase += v->fx_o1_inc * pmult; if (v->fx_o1_phase >= 1.0) v->fx_o1_phase -= 1.0;

    // Ring modulation (goblins/sci-fi growl).
    if (v->fx_ring_inc > 0.0) {
        double ring = wt_sin(v->fx_ring_phase);
        v->fx_ring_phase += v->fx_ring_inc; if (v->fx_ring_phase >= 1.0) v->fx_ring_phase -= 1.0;
        core = (1.0 - v->fx_ring_mix) * core + v->fx_ring_mix * core * ring;
    }

    // Filter (SVF) with LFO + S&H + sweep-envelope cutoff modulation.
    double cut = v->fx_cut;
    if (v->fx_lfo_inc > 0.0) {
        double lfo = gm_fx_lfo_tri(v);
        cut *= 1.0 + v->fx_lfo_depth * lfo;
    }
    if (v->fx_sh_inc > 0.0) {
        double sh = gm_fx_sh(v);
        cut *= 1.0 + 0.6 * sh;
    }
    if (v->fx_cut_env_dec > 0.0 && v->fx_cut_env_dec < 1.0) {
        cut += v->fx_cut_sweep_down ? v->fx_cut_env : 0.0;
        v->fx_cut_env *= v->fx_cut_env_dec;
    }
    if (v->fx_cut_base > 0.0) {
        s = gm_fx_svf_lp(v, core, cut, sr);
    } else {
        s = core;
    }

    // Texture / filtered-noise bed.
    if (v->fx_noise_amt > 0.0) {
        double bedlfo = 1.0;
        s += v->fx_noise_amt * gm_fx_noise_bed(v) * bedlfo;
    }

    // Internal amplitude envelope (crystal/echoes ping fade; sci-fi crack).
    if (v->fx_amp_dec < 1.0) {
        s *= v->fx_amp_env;
        v->fx_amp_env *= v->fx_amp_dec;
    }
    if (v->fx_amp_dec2 < 1.0 && v->fx_amp_env2 > 0.0001) {
        // sci-fi attack crack: short bright noise burst on top.
        s += v->fx_amp_env2 * 0.5 * wg_white(v);
        v->fx_amp_env2 *= v->fx_amp_dec2;
    }

    // Boom layer (sci-fi low thump).
    if (v->fx_boom_inc > 0.0) {
        s += 0.4 * wt_sin(v->fx_boom_phase);
        v->fx_boom_phase += v->fx_boom_inc; if (v->fx_boom_phase >= 1.0) v->fx_boom_phase -= 1.0;
    }

    // Time effect: feedback delay (crystal twinkle / echoes).
    if (v->fx_delay_samps > 0.0) {
        s = gm_fx_delay_tick(v, s);
    }

    return clampd(s, -2.0, 2.0) * v->fx_out_scale * env;
}

// ── GM_ENGINE_SOUNDFX: stochastic / noise sound effects ──
static inline double generate_soundfx_sample(GMVoice *v, double sample_rate,
                                             double env) {
    double sr = sample_rate;
    double s = 0.0;
    int mode = -1;
    // Recover mode from program (stable + cheap).
    int prog = v->program;
    if (prog >= GM_SOUNDFX_FIRST && prog < GM_SOUNDFX_FIRST + GM_SOUNDFX_COUNT)
        mode = prog - GM_SOUNDFX_FIRST;

    if (prog == 119) {   // Reverse Cymbal — bright BP noise under a RISING swell.
        double bed = gm_fx_noise_bed(v);
        // amp_env climbs from 0 toward 1 (1-pole rise), giving the reverse swell.
        v->fx_amp_env += (1.0 - v->fx_amp_dec) * (1.0 - v->fx_amp_env);
        s = bed * v->fx_amp_env;
        return clampd(s, -2.0, 2.0) * v->fx_out_scale * env;
    }

    switch (mode) {
    case GM_SFX_SEASHORE:
    case GM_SFX_APPLAUSE: {
        s = gm_phisem_tick(v);
        break;
    }
    case GM_SFX_BREATH: {
        // Band-passed noise puff + faint PhISEM grain, soft AD + amp wobble.
        double bed = gm_fx_noise_bed(v);
        double grain = (v->ph_num > 0.0) ? gm_phisem_tick(v) : 0.0;
        double wob = 1.0;
        if (v->fx_lfo_inc > 0.0) {
            wob = 1.0 + v->fx_lfo_depth * wt_sin(v->fx_lfo_phase);
            v->fx_lfo_phase += v->fx_lfo_inc; if (v->fx_lfo_phase >= 1.0) v->fx_lfo_phase -= 1.0;
        }
        s = (bed + grain) * wob;
        if (v->fx_amp_dec < 1.0) { s *= v->fx_amp_env; v->fx_amp_env *= v->fx_amp_dec; }
        break;
    }
    case GM_SFX_FRET: {
        // Short swept band-pass noise squeak. Slide the BP center via cut env.
        double bed = gm_fx_noise_bed(v);
        s = bed;
        if (v->fx_amp_dec < 1.0) { s *= v->fx_amp_env; v->fx_amp_env *= v->fx_amp_dec; }
        // Re-tune the BP center each block-ish (cheap: every sample, small step).
        if (v->fx_cut_env_dec > 0.0 && v->fx_cut_env_dec < 1.0) {
            double center = v->fx_cut_base
                          + (v->fx_cut_sweep_down ? -v->fx_cut_env : v->fx_cut_env);
            v->fx_cut_env *= v->fx_cut_env_dec;
            gm_fx_make_bp(v, clampd(center, 200.0, sr * 0.45),
                          v->fx_res > 0.5 ? v->fx_res : 4.0, sr);
        }
        break;
    }
    case GM_SFX_BIRD: {
        // Pitch-swept FM chirp, fast trill, gated into syllables.
        // Syllable gate: phase clock; count syllables, silence after gate_n.
        double on = 1.0;
        if (v->fx_gate_inc > 0.0) {
            if (v->fx_gate_max > 0 && v->fx_gate_n >= v->fx_gate_max) {
                on = 0.0;
            } else {
                on = (v->fx_gate_phase < v->fx_gate_on_frac) ? 1.0 : 0.0;
            }
            v->fx_gate_phase += v->fx_gate_inc;
            if (v->fx_gate_phase >= 1.0) { v->fx_gate_phase -= 1.0; v->fx_gate_n++;
                // Re-arm the chirp pitch sweep at each new syllable (stochastic).
                v->fx_pitch_mult = 0.6 + 0.4 * voice_rand_unit(v);
            }
        }
        double trill = 0.0;
        if (v->fx_lfo_inc > 0.0) {
            trill = v->fx_lfo_depth * wt_sin(v->fx_lfo_phase);
            v->fx_lfo_phase += v->fx_lfo_inc; if (v->fx_lfo_phase >= 1.0) v->fx_lfo_phase -= 1.0;
        }
        double pm = v->fx_pitch_mult * (1.0 + trill);
        if (v->fx_pitch_dec < 1.0) v->fx_pitch_mult = 1.0 + (v->fx_pitch_mult - 1.0) * v->fx_pitch_dec;
        double fmod = v->fx_fm_index * wt_sin(v->fx_o2_phase);
        v->fx_o2_phase += v->fx_o2_inc * pm; if (v->fx_o2_phase >= 1.0) v->fx_o2_phase -= 1.0;
        s = wt_sin(v->fx_o1_phase + fmod) * on;
        v->fx_o1_phase += v->fx_o1_inc * pm; if (v->fx_o1_phase >= 1.0) v->fx_o1_phase -= 1.0;
        break;
    }
    case GM_SFX_TELEPHONE: {
        // Gated dual sine 440+480 with ring cadence.
        double on = (v->fx_gate_phase < v->fx_gate_on_frac) ? 1.0 : 0.0;
        v->fx_gate_phase += v->fx_gate_inc;
        if (v->fx_gate_phase >= 1.0) v->fx_gate_phase -= 1.0;
        double t1 = wt_sin(v->fx_o1_phase);
        double t2 = wt_sin(v->fx_o2_phase);
        v->fx_o1_phase += v->fx_o1_inc; if (v->fx_o1_phase >= 1.0) v->fx_o1_phase -= 1.0;
        v->fx_o2_phase += v->fx_o2_inc; if (v->fx_o2_phase >= 1.0) v->fx_o2_phase -= 1.0;
        s = 0.5 * (t1 + t2) * on;
        break;
    }
    case GM_SFX_HELICOPTER: {
        // Periodic-AM broadband noise (rotor chop) + low rumble.
        double bed = gm_fx_noise_bed(v);
        // Pulse train: raise a sine to a power for a sharp blade-slap pulse.
        double ph = v->fx_am_phase;
        double pulse = 0.5 * (1.0 + wt_sin(ph));        // 0..1
        pulse = pow(pulse, v->fx_am_sharp);
        v->fx_am_phase += v->fx_am_inc; if (v->fx_am_phase >= 1.0) v->fx_am_phase -= 1.0;
        double am = (1.0 - v->fx_am_depth) + v->fx_am_depth * pulse;
        double chop = bed * am;
        double rumble = 0.0;
        if (v->fx_boom_inc > 0.0) {
            rumble = 0.5 * (2.0 * v->fx_boom_phase - 1.0);  // low buzzy saw
            v->fx_boom_phase += v->fx_boom_inc; if (v->fx_boom_phase >= 1.0) v->fx_boom_phase -= 1.0;
        }
        s = 0.7 * chop + 0.5 * rumble;
        break;
    }
    case GM_SFX_GUNSHOT: {
        // Broadband noise crack burst + low boom with downward pitch sweep.
        double crack = gm_fx_noise_bed(v) * v->fx_amp_env;
        v->fx_amp_env *= v->fx_amp_dec;
        double pm = v->fx_pitch_mult;
        if (v->fx_pitch_dec < 1.0) v->fx_pitch_mult = 1.0 + (v->fx_pitch_mult - 1.0) * v->fx_pitch_dec;
        double boom = 0.0;
        if (v->fx_boom_inc > 0.0 && v->fx_amp_env2 > 0.0001) {
            boom = wt_sin(v->fx_boom_phase) * v->fx_amp_env2;
            v->fx_boom_phase += v->fx_boom_inc * pm; if (v->fx_boom_phase >= 1.0) v->fx_boom_phase -= 1.0;
            v->fx_amp_env2 *= v->fx_amp_dec2;
        }
        s = 0.8 * crack + 0.9 * boom;
        break;
    }
    default:
        s = 0.0;
        break;
    }
    return clampd(s, -2.0, 2.0) * v->fx_out_scale * env;
}

double gm_voice_render(GMVoice *v, double sample_rate, double env,
                       double frequency) {
    double s;
    switch (v->engine) {
    case GM_ENGINE_GMPIANO:   s = generate_gmpiano_sample(v, sample_rate, env); break;
    case GM_ENGINE_EPIANO:    s = generate_epiano_sample(v, sample_rate, env); break;
    case GM_ENGINE_PLUCK:     s = generate_pluck_sample(v, sample_rate, env, frequency); break;
    case GM_ENGINE_MODAL:     s = generate_modal_sample(v, sample_rate, env); break;
    case GM_ENGINE_SYNTHBASS: s = generate_synthbass_sample(v, sample_rate, env); break;
    case GM_ENGINE_WAVEGUIDE: s = generate_waveguide_sample(v, sample_rate, env, frequency); break;
    case GM_ENGINE_ORGAN:     s = generate_organ_sample(v, sample_rate, env); break;
    case GM_ENGINE_SUPERSAW:  s = generate_supersaw_sample(v, sample_rate, env); break;
    case GM_ENGINE_FORMANT:   s = generate_formant_sample(v, sample_rate, env); break;
    case GM_ENGINE_SYNTHFX:   s = generate_synthfx_sample(v, sample_rate, env); break;
    case GM_ENGINE_SOUNDFX:   s = generate_soundfx_sample(v, sample_rate, env); break;
    default:                  return 0.0;
    }
    // Final NaN/Inf trap. A couple of the feedback engines (Chamberlin SVF in
    // supersaw, the conical-reed waveguide bore loop) can go unstable for some
    // program/pitch settings and diverge to Inf→NaN; the per-engine output
    // clampd() does NOT catch this (NaN compares false against both bounds), so
    // the poison would otherwise reach the host mixer/limiter and crash it.
    // No-op for every well-behaved voice (their sample is already finite); a
    // diverged voice is muted to silence rather than taking down the engine.
    return isfinite(s) ? s : 0.0;
}
