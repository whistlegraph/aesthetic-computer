// synth_types.h — Shared voice/wave/gun types for the AC synth.
// Included by both the Linux ALSA driver (audio.h) and the macOS SDL3
// driver (macos/audio.c) so voice layout never diverges. Intentionally
// free of pthread / audio-decode / ffmpeg deps; only <stdint.h>.
#ifndef AC_SYNTH_TYPES_H
#define AC_SYNTH_TYPES_H

#include <stdint.h>

#define AUDIO_SAMPLE_RATE 192000
#define AUDIO_CHANNELS 2
#define AUDIO_PERIOD_SIZE 192   // ~1ms at 192kHz — minimal latency
#define AUDIO_MAX_VOICES 32
#define AUDIO_WAVEFORM_SIZE 512
#define AUDIO_MAX_SAMPLE_VOICES 12

typedef enum {
    VOICE_INACTIVE = 0,
    VOICE_ACTIVE,
    VOICE_KILLING
} VoiceState;

typedef enum {
    WAVE_SINE = 0,
    WAVE_TRIANGLE,
    WAVE_SAWTOOTH,
    WAVE_SQUARE,
    WAVE_NOISE,
    WAVE_WHISTLE,
    WAVE_GUN
} WaveType;

typedef enum {
    GUN_MODEL_CLASSIC = 0,
    GUN_MODEL_PHYSICAL = 1
} GunModel;

typedef enum {
    GUN_PISTOL = 0, GUN_RIFLE, GUN_SHOTGUN, GUN_SMG, GUN_SUPPRESSED,
    GUN_LMG, GUN_SNIPER, GUN_GRENADE, GUN_RPG, GUN_RELOAD, GUN_COCK,
    GUN_RICOCHET, GUN_PRESET_COUNT
} GunPreset;

// Per-voice state. Layout must match src/audio.h ACVoice exactly while that
// file still declares its own copy — both sides include this header now,
// but for safety during the migration the struct lives here and audio.h
// re-exports it via typedef alias.
typedef struct {
    VoiceState state;
    WaveType type;
    double phase;
    double frequency;
    double target_frequency;
    double volume;
    double pan;
    double attack;
    double decay;
    double duration;
    double elapsed;
    double fade_duration;
    double fade_elapsed;
    double started_at;
    uint64_t id;
    // Noise filter state (biquad LPF applied to xorshift32 output).
    double noise_b0, noise_b1, noise_b2, noise_a1, noise_a2;
    double noise_x1, noise_x2, noise_y1, noise_y2;
    uint32_t noise_seed;
    // Whistle (Perry Cook STK flute): bore delay + jet delay + cubic NL.
    double whistle_breath;
    double whistle_vibrato_phase;
    double whistle_lp1;
    double whistle_hp_x1, whistle_hp_y1;
    float  whistle_bore_buf[2048];
    int    whistle_bore_w;
    float  whistle_jet_buf[512];
    int    whistle_jet_w;
    // Gun (shared fields between classic & physical).
    int    gun_preset;
    double gun_bore_delay;
    double gun_bore_loss;
    double gun_bore_lp;
    double gun_breech_reflect;
    double gun_pressure;
    double gun_pressure_env;
    double gun_env_decay_mult;
    double gun_noise_gain;
    double gun_radiation_a;
    double gun_rad_prev;
    double gun_secondary_trig;
    double gun_secondary_amp;
    int    gun_sustain_fire;
    double gun_retrig_timer;
    double gun_retrig_period;
    double gun_body_a1[3], gun_body_a2[3];
    double gun_body_amp[3];
    double gun_body_y1[3], gun_body_y2[3];
    double gun_pitch_mult;
    double gun_pitch_target;
    double gun_pitch_slew;
    // Gun classic layers.
    int    gun_model;
    double gun_boom_phase;
    double gun_boom_freq;
    double gun_boom_freq_start;
    double gun_boom_freq_end;
    double gun_boom_pitch_mult;
    double gun_boom_env;
    double gun_boom_decay_mult;
    double gun_tail_env;
    double gun_tail_attack_inc;
    double gun_tail_decay_mult;
    double gun_crack_b0;
    double gun_tail_b0, gun_tail_b1, gun_tail_b2;
    double gun_click_env;
    double gun_click_decay_mult;
    double gun_click_amp;
    double gun_click_prev;
    // Gun physical Friedlander pulse.
    double gun_phys_t;
    double gun_phys_t_plus;
    double gun_phys_friedlander_a;
    double gun_phys_neg_amp;
    double gun_phys_echo_delay;
    double gun_phys_echo_amp;
    double gun_phys_echo_buf[1024];
    int    gun_phys_echo_w;
} ACVoice;

typedef struct {
    int active;
    int loop;
    double position;
    double speed;
    double volume;
    double pan;
    double fade;
    double fade_target;
    uint64_t id;
} SampleVoice;

#endif
