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
#define GM_FX_DELAY_N      8192   // per-voice FX echo ring (~170 ms @ 48k, ~43 @ 192k)

// gm_synth's own engine enum. The host engine no longer needs to know which
// WaveType a program maps to — gm_voice_init() stores the chosen engine here and
// gm_voice_render() dispatches on it internally.
typedef enum {
    GM_ENGINE_NONE = 0,
    GM_ENGINE_GMPIANO,    // modal additive + inharmonicity (acoustic pianos)
    GM_ENGINE_EPIANO,     // 2-op FM tine/reed (electric pianos)
    GM_ENGINE_PLUCK,      // extended Karplus-Strong (harpsichord/clavi/guitar/bass/ethnic)
    GM_ENGINE_MODAL,      // modal bank (chromatic perc / kalimba / percussive / timpani)
    GM_ENGINE_SYNTHBASS,  // subtractive (synth bass / reed approximations / synth brass)
    GM_ENGINE_WAVEGUIDE,  // bidirectional digital waveguide (bowed/brass/reed/flute)
    GM_ENGINE_ORGAN,      // additive Hammond drawbars + Leslie; free-reed accordion/harmonica
    GM_ENGINE_SUPERSAW,   // detuned multi-osc subtractive: ensemble / synth lead / synth pad
    GM_ENGINE_FORMANT,    // source-filter vocal formant bank: choir / voice / synth-voice
    GM_ENGINE_SYNTHFX,    // layered sound-design FX: rain/soundtrack/crystal/atmosphere/...
    GM_ENGINE_SOUNDFX     // stochastic/noise sound effects: seashore/bird/applause/gunshot/...
} GMEngine;

// Sub-mode inside GM_ENGINE_SYNTHFX (GM 96-103). Each shares the generic
// tonal-core + texture-bed + time-effect chain; the mode selects which time
// effect and texture flavor dominate.
typedef enum {
    GM_FX_RAIN = 0,     // 96  FX1 — PhISEM droplets + S&H'd shimmer pair
    GM_FX_SOUNDTRACK,   // 97  FX2 — detuned-saw pad, slow LFO-swept LPF
    GM_FX_CRYSTAL,      // 98  FX3 — inharmonic FM bell + feedback delay
    GM_FX_ATMOSPHERE,   // 99  FX4 — pad + filtered-noise bed
    GM_FX_BRIGHTNESS,   // 100 FX5 — bright rising-FM pad
    GM_FX_GOBLINS,      // 101 FX6 — dark ring-modded saw + S&H cutoff
    GM_FX_ECHOES,       // 102 FX7 — bright source + regenerating dark delay
    GM_FX_SCIFI         // 103 FX8 — swept osc + resonant filter + noise burst
} GMSynthFxMode;

// Sub-mode inside GM_ENGINE_SOUNDFX (GM 120-127).
typedef enum {
    GM_SFX_FRET = 0,    // 120 Guitar Fret Noise — swept filtered-noise squeak
    GM_SFX_BREATH,      // 121 Breath Noise — band-passed noise puff (+PhISEM grain)
    GM_SFX_SEASHORE,    // 122 Seashore — PhISEM surf + slow swell LFOs
    GM_SFX_BIRD,        // 123 Bird Tweet — pitch-swept FM chirps, trilled syllables
    GM_SFX_TELEPHONE,   // 124 Telephone Ring — gated dual sine (440+480) cadence
    GM_SFX_HELICOPTER,  // 125 Helicopter — periodic-AM broadband noise + rumble
    GM_SFX_APPLAUSE,    // 126 Applause — PhISEM crowd claps + swell
    GM_SFX_GUNSHOT      // 127 Gunshot — noise crack burst + low boom decay
} GMSoundFxMode;

// Nonlinearity / excitation mode inside GM_ENGINE_WAVEGUIDE. The shared
// bidirectional bore delay line is constant; the junction physics differ:
//   BOWED — stick-slip friction (McIntyre-Schumacher-Woodhouse / STK BowTable)
//   LIP   — pressure-controlled lip valve (STK Brass), quadratic NL
//   REED  — saturating reed reflection (STK ReedTable); clarinet inverts bore
//   JET   — Cook flute jet delay + cubic + blowing noise
typedef enum {
    GM_WG_BOWED = 0,
    GM_WG_LIP,
    GM_WG_REED,
    GM_WG_JET
} GMWaveguideMode;

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

    // -- Digital waveguide (GM_ENGINE_WAVEGUIDE): bowed / brass / reed / flute --
    GMWaveguideMode wg_mode;
    // Shared bore delay line. Reuses ks_buf (GM_KS_BIG_N) so low brass/strings
    // (contrabass ~33 Hz, tuba ~D1) fit at 192 kHz. wg_w is its write index.
    int    wg_w;
    double wg_base_delay;      // bore length in samples (sr/f or sr/f*2+3)
    double wg_loop_lp;         // 1-pole loop-loss LPF state
    double wg_loop_damp;       // loop-loss coefficient (darker = larger)
    double wg_hp_x1, wg_hp_y1; // DC blocker after the junction
    double wg_breath;          // smoothed breath/bow pressure target follower
    double wg_breath_max;      // pressure/velocity ceiling (loudness+brightness)
    double wg_noise_gain;      // turbulence / bow-grind / chiff amount
    double wg_attack_ms;       // pressure ramp time (slow bow / soft horn onset)
    double wg_attack_env;      // 0..1 onset ramp, climbs to 1
    double wg_attack_inc;      // per-sample onset ramp increment
    // Vibrato (all modes) — phase-increment LFO baked rate, depth in delay frac.
    double wg_vib_phase, wg_vib_inc, wg_vib_depth;
    // BOWED — friction junction.
    double wg_bow_beta;        // bow position 0..1 (bridge..nut split ratio)
    double wg_bow_slope;       // bow force / pressure (steeper = more pressure)
    // LIP (brass) — lip resonance biquad tracking f0 + scatter gains.
    double wg_lip_b0, wg_lip_b1, wg_lip_b2, wg_lip_a1, wg_lip_a2;
    double wg_lip_x1, wg_lip_x2, wg_lip_y1, wg_lip_y2;
    double wg_lip_gain;
    // REED — STK reed table affine map + bore-reflection sign.
    double wg_reed_offset, wg_reed_slope;
    int    wg_bore_invert;     // 1 = clarinet (cylindrical, odd harmonics)
    // JET (flute) — jet delay ratio (jet length / bore length).
    double wg_jet_ratio;
    // Output formant/mute/bell shaping: one biquad bandpass + a 1-pole tilt.
    double wg_fmt_b0, wg_fmt_b2, wg_fmt_a1, wg_fmt_a2;
    double wg_fmt_x1, wg_fmt_x2, wg_fmt_y1, wg_fmt_y2;
    double wg_fmt_gain;        // 0 = no output formant biquad
    double wg_out_lp;          // output LP state (mute / dark bell)
    double wg_out_lp_g;        // output LP coefficient (0 = bypass)
    double wg_out_scale;       // output gain
    // Section shimmer (brass section / tremolo strings) — amplitude LFO.
    double wg_trem_phase, wg_trem_inc, wg_trem_depth;

    // -- Additive Hammond drawbars + free-reed (GM_ENGINE_ORGAN) --
    // 9 sine drawbars at fixed footage ratios; or detuned saw/pulse reed banks.
    int    org_nbars;                 // active drawbar / reed-bank count
    double org_phase[9];              // per-bar oscillator phase
    double org_inc[9];                // per-bar phase increment (f0*ratio/sr)
    double org_amp[9];                // per-bar registration amplitude
    int    org_freereed;             // 1 = saw/pulse reed banks (accordion/harmonica)
    int    org_bar_square[9];        // free-reed: 1 = pulse, 0 = saw for this bank
    double org_drive;                 // rock-organ overdrive (tanh)
    double org_perc_env, org_perc_dec, org_perc_amp;  // percussive-organ 2nd-harm ping
    double org_perc_phase, org_perc_inc;
    double org_click_env, org_click_dec, org_click_amp; // key-click HF noise burst
    double org_click_lp;              // 1-pole HP-ish state for click
    double org_leslie_phase, org_leslie_inc, org_leslie_depth; // Leslie amp+pitch LFO
    double org_breath_lp, org_breath_amt;             // free-reed breath noise
    double org_lp;                    // free-reed gentle output LPF state
    double org_lp_g;                  // free-reed LPF coefficient (0 = bypass)

    // -- Detuned multi-osc subtractive (GM_ENGINE_SUPERSAW): ensemble/lead/pad --
    int    ss_nosc;                   // active oscillator count (1..7)
    double ss_phase[7];               // per-osc phase
    double ss_inc[7];                 // per-osc phase increment
    double ss_gain[7];                // per-osc gain (Szabo center/side or unity)
    int    ss_square[7];              // 1 = pulse/square osc, 0 = saw
    double ss_pwm_phase, ss_pwm_inc, ss_pwm_depth;    // PWM LFO (square width)
    double ss_hp_x1, ss_hp_y1;        // pitch-tracked 1-pole HP (supersaw mud cut)
    double ss_hp_g;                   // HP coefficient
    // State-variable LPF (Chamberlin) + slow cutoff sweep LFO + attack env.
    double ss_svf_lp, ss_svf_bp;      // SVF integrator state
    double ss_cut, ss_cut_base;       // current + base cutoff (Hz)
    double ss_cut_env, ss_cut_env_dec, ss_cut_env_amt; // attack filter-envelope (Hz add)
    double ss_res;                    // resonance (0..~0.98)
    double ss_sweep_phase, ss_sweep_inc, ss_sweep_depth; // slow cutoff LFO (octaves)
    double ss_attack_env, ss_attack_inc;  // slow amplitude attack ramp 0..1
    double ss_vib_phase, ss_vib_inc, ss_vib_depth;       // collective vibrato
    float  ss_chorus_buf[1024];       // short modulated chorus delay line
    int    ss_chorus_w;
    double ss_chorus_phase, ss_chorus_inc, ss_chorus_depth, ss_chorus_mix;
    double ss_drive;                  // charang/overdrive tanh
    double ss_sub_phase, ss_sub_inc, ss_sub_mix;         // bass+lead sub-octave
    int    ss_sub_square;
    double ss_chiff_env, ss_chiff_dec, ss_chiff_amt;     // chiff/transient noise burst
    double ss_chiff_nb0, ss_chiff_nb2, ss_chiff_na1, ss_chiff_na2; // chiff BPF
    double ss_chiff_x1, ss_chiff_x2, ss_chiff_y1, ss_chiff_y2;
    double ss_trem_phase, ss_trem_inc, ss_trem_depth;    // halo amplitude tremolo
    double ss_body_dec;               // orchestra-hit body decay (1 = sustained)
    double ss_body_amp;               // orchestra-hit body amplitude env
    double ss_pitch_mult, ss_pitch_dec; // orchestra-hit downward pitch blip
    double ss_out_scale;              // output gain

    // -- Vocal formant bank (GM_ENGINE_FORMANT): choir / voice / synth-voice --
    // Up to 3 detuned saw/pulse glottal sources → 3 parallel formant biquads.
    int    fmt_nsrc;                  // source-oscillator count (1=solo, 3=choir)
    double fmt_src_phase[3];          // per-source phase
    double fmt_src_inc[3];            // per-source phase increment
    double fmt_nbands;                // active formant band count (3)
    double fmt_b0[3], fmt_b2[3], fmt_a1[3], fmt_a2[3];  // 3 formant bandpass biquads
    double fmt_x1[3], fmt_x2[3], fmt_y1[3], fmt_y2[3];
    double fmt_g[3];                  // per-formant linear gain
    double fmt_attack_env, fmt_attack_inc;             // slow onset ramp
    double fmt_breath_lp, fmt_breath_amt;              // aspiration noise into formants
    double fmt_vib_phase[3], fmt_vib_inc[3], fmt_vib_depth; // decorrelated per-source vibrato

    // -- Shared FX primitives (GM_ENGINE_SYNTHFX / GM_ENGINE_SOUNDFX) --
    // Per-voice delay/echo ring (crystal/echoes/atmosphere feedback repeats).
    float  fx_delay[GM_FX_DELAY_N];
    int    fx_delay_w;
    double fx_delay_samps;     // current delay length in samples
    double fx_delay_fb;        // feedback gain (0..~0.85)
    double fx_delay_mix;       // wet mix
    double fx_delay_damp;      // 1-pole HF damping in the feedback path (0=bypass)
    double fx_delay_lp;        // damping filter state
    // LFO + sample-and-hold (slow modulation of cutoff/pitch/shimmer).
    double fx_lfo_phase, fx_lfo_inc, fx_lfo_depth;
    double fx_sh_phase, fx_sh_inc;   // S&H clock
    double fx_sh_value;              // held random value [-1,1]
    // Ring-mod carrier (goblins/rain shimmer/sci-fi).
    double fx_ring_phase, fx_ring_inc, fx_ring_mix;
    // Generic tonal-core oscillators (saw/sine/FM) shared by the FX engines.
    double fx_o1_phase, fx_o1_inc;
    double fx_o2_phase, fx_o2_inc;   // detuned partner / FM modulator / 2nd sine
    double fx_o3_phase, fx_o3_inc;   // 3rd saw / shimmer partner
    double fx_fm_index, fx_fm_index_env, fx_fm_index_dec; // FM mod depth + env
    int    fx_fm_rising;             // 1 = index ramps UP into the note (brightness)
    // State-variable LPF (Chamberlin) for pad/sci-fi sweeps.
    double fx_svf_lp, fx_svf_bp;
    double fx_cut, fx_cut_base, fx_res;
    double fx_cut_env, fx_cut_env_dec;  // pitch/cutoff sweep envelope (sci-fi)
    int    fx_cut_sweep_down;           // 1 = cutoff sweeps downward over the note
    // Filtered-noise texture bed (atmosphere/rain/seashore/breath/helicopter).
    double fx_noise_lp, fx_noise_lp2;   // cascaded 1-pole LP state
    double fx_noise_hp_x1, fx_noise_hp_y1; // 1-pole HP state
    double fx_noise_amt;                // texture/noise level
    double fx_noise_bp0, fx_noise_bp2, fx_noise_bpa1, fx_noise_bpa2; // BP biquad
    double fx_noise_bx1, fx_noise_bx2, fx_noise_by1, fx_noise_by2;
    int    fx_noise_use_bp;             // 1 = route noise through the BP biquad
    // Pitch / cutoff sweep multiplier (bird chirp, sci-fi zap, gunshot boom).
    double fx_pitch_mult, fx_pitch_dec, fx_pitch_target;
    // Amplitude envelopes internal to a trigger (bird syllables, gunshot boom).
    double fx_amp_env, fx_amp_dec;      // self-decaying internal AD envelope
    double fx_amp_env2, fx_amp_dec2;    // 2nd internal envelope (crack vs boom)
    // Gate/cadence counter (telephone ring on/off, bird syllable gating).
    double fx_gate_phase, fx_gate_inc;  // phase-increment cadence clock
    double fx_gate_on_frac;             // duty fraction that is "on"
    int    fx_gate_n, fx_gate_max;      // syllable/event counters
    // Periodic-AM pulse (helicopter rotor blade-passage).
    double fx_am_phase, fx_am_inc, fx_am_sharp, fx_am_depth;
    // Boom oscillator (sci-fi / gunshot low thump).
    double fx_boom_phase, fx_boom_inc;
    double fx_out_scale;

    // -- PhISEM particle engine (rain / seashore / applause / breath grains) --
    double ph_energy;          // shake energy, leaks each sample
    double ph_sys_decay;       // systemDecay (energy leak per sample)
    double ph_snd_decay;       // soundDecay (per-collision ring decay)
    double ph_num;             // numObjects (collision-probability scale)
    double ph_snd_level;       // accumulated collision sound level
    double ph_gain;            // output gain
    // Up to 3 collision resonators (clap/drop/shore band-passes).
    int    ph_nres;
    double ph_res_a1[3], ph_res_a2[3], ph_res_b0[3];
    double ph_res_y1[3], ph_res_y2[3];
    // Slow swell LFOs that re-pump energy (seashore waves / applause swell).
    int    ph_nswell;
    double ph_swell_phase[3], ph_swell_inc[3];
    double ph_swell_depth;
    double ph_energy_floor;    // sustained density floor (rain/seashore vs one-shot)
} GMVoice;

// Build the wt_sin wavetable. Idempotent; safe to call repeatedly. gm_voice_init
// calls it internally so callers need not, but it is exposed for explicit setup.
void gm_synth_init(void);

// Global "organic" amount: 0 = bit-identical, 1 = max tasteful spread. Default 0.6.
void gm_set_organic(double amt);

// 1 if `program` (0-based GM) has a bespoke algorithmic voice in this build.
// Implemented: ALL 128 GM programs 0-127. Families: Piano 0-7, Chromatic Perc
// 8-15, Organ 16-23, Guitar 24-31, Bass 32-39, Strings 40-47, Ensemble 48-55,
// Brass 56-63, Reed 64-71, Pipe 72-79, Synth Lead 80-87, Synth Pad 88-95,
// Synth FX 96-103, Ethnic 104-111, Percussive 112-119, Sound FX 120-127.
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
