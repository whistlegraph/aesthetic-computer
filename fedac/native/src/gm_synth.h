// gm_synth.h — Standalone, dependency-free General-MIDI synthesis module.
//
// Extracted from audio.c so the algorithmic GM voice set (modal-additive piano,
// FM electric piano, extended Karplus-Strong plucked strings, modal-bank
// chromatic/percussive, subtractive synth bass / reed approximations) can be
// built and tested WITHOUT the ALSA/engine dependencies of audio.c — and later
// shared into the Menu Band macOS app.
//
// This header pulls in ONLY standard C (stdint, math, string). It does NOT
// include audio.h, alsa, or any engine header. A GMVoice owns ALL per-voice GM
// state (partial arrays, FM operators, KS string state, modal bank, subtractive
// filter state, its own KS delay buffers, its own noise biquad + attack/secondary
// burst envelopes, and its own xorshift RNG seed). The host engine supplies only
// the amplitude envelope and the (smoothed) current frequency at render time.
//
// DSP math is byte-for-byte the same as the original audio.c implementation —
// this is a MOVE + DECOUPLE, not a redesign. No per-sample sinf(): sustained
// tones read the shared wt_sin wavetable (phase-increment rule).
//
// Public API:
//   void gm_synth_init(void);                 // build wt_sin once (idempotent)
//   void gm_set_organic(double amt);          // global organic knob (0..1)
//   int  gm_program_implemented(int program); // 1 if program 0..127 has a voice
//   int  gm_voice_init(GMVoice*, int program, double freq, double sr, uint32_t seed);
//   double gm_voice_render(GMVoice*, double sr, double env, double frequency);

#ifndef GM_SYNTH_H
#define GM_SYNTH_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// ── Compile-time sizes (must match the original ACVoice GM scratch) ──
#define GM_MAX_PARTIALS    12     // modal-additive partial cap
#define GM_KS_BIG_N        8192   // long KS delay line (guitar/bass — low notes)
#define GM_BORE_N          2048   // short KS delay line (harpsichord/clavi)

// gm_synth's own engine enum. The host engine no longer needs to know which
// WaveType a program maps to — gm_voice_init() stores the chosen engine here and
// gm_voice_render() dispatches on it internally.
typedef enum {
    GM_ENGINE_NONE = 0,
    GM_ENGINE_GMPIANO,    // modal additive + inharmonicity (acoustic pianos)
    GM_ENGINE_EPIANO,     // 2-op FM tine/reed (electric pianos)
    GM_ENGINE_PLUCK,      // extended Karplus-Strong (harpsichord/clavi/guitar/bass/ethnic)
    GM_ENGINE_MODAL,      // modal bank (chromatic perc / kalimba / percussive)
    GM_ENGINE_SYNTHBASS   // subtractive (synth bass / reed approximations)
} GMEngine;

// Per-voice GM synthesis state. Self-contained: holds every field the GM voices
// touched in ACVoice plus its own copies of the buffers/biquad/burst envelopes
// that previously aliased shared ACVoice scratch (whistle_bore_buf, the noise
// biquad, gun_click_*, gun_secondary_*). The engine embeds one of these per voice.
typedef struct {
    GMEngine engine;        // which generator renders this voice
    int    program;         // GM program (0-based) — debug / routing

    // Own randomness — xorshift32 stream seeded per-trigger from the host seed.
    uint32_t rng_seed;

    // -- Modal additive partials (gmpiano + modal bank) --
    int    p_count;
    double p_phase[GM_MAX_PARTIALS];
    double p_amp[GM_MAX_PARTIALS];
    double p_finc[GM_MAX_PARTIALS];
    double p_dec_mult[GM_MAX_PARTIALS];
    int    gm_dual;                       // honky-tonk: 2nd detuned string set
    double p2_phase[GM_MAX_PARTIALS];
    double p2_finc[GM_MAX_PARTIALS];
    double gm_drive;                      // per-voice tanh drive (electric grand)
    double gm_hammer_env;                 // hammer/strike noise-burst envelope
    double gm_hammer_dec;
    double gm_hammer_amp;
    double gm_hammer_lp;                  // 1-pole LPF state for hammer noise

    // -- FM tine/reed operators (epiano) --
    double fm_cphase, fm_cinc;
    double fm_mphase, fm_minc;
    double fm_index, fm_index_dec;
    double fm_tphase, fm_tinc;
    double fm_tindex, fm_tindex_dec;
    double fm_pickup_bias;
    double fm_hp_x1, fm_hp_y1;            // DC blocker after pickup nonlinearity

    // -- Extended Karplus-Strong string state (pluck) --
    double harp_lp1;                      // loop-LPF state
    double ks_stretch;
    double ks_loop_b;
    double ks_beta;
    double ks_pick_amt;
    double ks_drive;
    float  bore_buf[GM_BORE_N];           // short delay line (harpsichord/clavi)
    int    bore_w;
    float  ks_buf[GM_KS_BIG_N];           // long delay line (guitar/bass/ethnic)
    int    ks_w;
    int    ks_use_big;                    // 1 = render from ks_buf
    int    ks_hard_clip;                  // 1 = hard waveshaper (distortion guitar)
    double ks_jawari_depth;               // buzzing-bridge nonlinearity depth
    double ks_jawari_thresh;
    double ks_body_a1[3], ks_body_a2[3];  // body-resonance biquad coeffs
    double ks_body_g[3];
    double ks_body_y1[3], ks_body_y2[3];  // body-resonance biquad state
    int    ks_body_n;

    // Attack-noise burst (finger thump / pick click / slap clack) — own biquad +
    // envelope (these aliased the engine noise biquad + gun_click_* in audio.c).
    double atk_env;                       // burst amplitude envelope
    double atk_dec;                       // per-sample decay multiplier
    double nb0, nb1, nb2, na1, na2;       // burst biquad coefficients
    double nx1, nx2, ny1, ny2;            // burst biquad state
    // Secondary excitation (slap snap-back) — own countdown (aliased gun_secondary_*).
    double sec_trig;                      // sample countdown (<=0 = fired/idle)
    double sec_amp;

    // -- Modal-bank extras (modal) --
    double gm_trem_phase;
    double gm_trem_inc;
    double gm_trem_depth;
    double gm_modal_bloom;
    double gm_modal_fund;
    int    gm_modal_pitched;

    // -- Subtractive synth bass / reed-as-saw (synthbass) --
    double sb_o1_phase, sb_o1_inc;
    double sb_o2_phase, sb_o2_inc;
    int    sb_o2_square;
    double sb_o2_mix;
    double sb_sub_phase, sb_sub_inc;
    double sb_sub_mix;
    double sb_fm_index, sb_fm_dec;
    double sb_lp1, sb_lp2;
    double sb_bp1, sb_bp2;
    double sb_cut, sb_cut_target;
    double sb_cut_dec;
    double sb_res;
    double sb_pitch_mult, sb_pitch_dec;
    int    sb_sustain;
    double sb_drone_phase, sb_drone_inc;
    double sb_drone_mix;
    double sb_breath_lp;
    double sb_breath_amt;
    double sb_vib_phase, sb_vib_inc;
    double sb_vib_depth;
} GMVoice;

// Build the wt_sin wavetable. Idempotent; safe to call repeatedly. gm_voice_init
// calls it internally so callers need not, but it is exposed for explicit setup.
void gm_synth_init(void);

// Global "organic" amount: 0 = bit-identical, 1 = max tasteful spread. Default 0.6.
void gm_set_organic(double amt);

// 1 if `program` (0-based GM) has a bespoke algorithmic voice in this build.
// Implemented: Piano 0-7, Chromatic Perc 8-15, Guitar 24-31, Bass 32-39,
// Ethnic 104-111, Percussive 112-119.
int gm_program_implemented(int program);

// Note-on init. Selects the engine, fills per-voice state from the matching
// program row, and applies bounded per-trigger stochasticism drawn from `seed`
// (every voice probabilistic — the same note never renders identically). Returns
// 0 on success, -1 for an unimplemented program (leaves engine = GM_ENGINE_NONE).
int gm_voice_init(GMVoice *v, int program, double freq, double sample_rate,
                  uint32_t seed);

// Render one sample. The host engine supplies the amplitude envelope `env`
// (0..1) and the current (smoothed) `frequency` in Hz — gm_synth owns everything
// else. Dispatches on the voice's stored engine. Returns the mono sample.
double gm_voice_render(GMVoice *v, double sample_rate, double env,
                       double frequency);

#ifdef __cplusplus
}
#endif

#endif // GM_SYNTH_H
