// audio.h — macOS SDL3 audio driver for the AC synth.
// Wraps fedac/native/src/synth_core with an SDL3 device + callback. Public
// API is similar to the Linux ALSA side but intentionally minimal — only
// what the SDL3 host + piece.c currently need.
#ifndef AC_MACOS_AUDIO_H
#define AC_MACOS_AUDIO_H

#include "synth_types.h"

typedef struct Audio Audio;

Audio *audio_init(void);
void   audio_destroy(Audio *a);

// Basic oscillator voice. Mirrors synth_synth() in synth_core.
uint64_t audio_synth(Audio *a, WaveType wave, double freq_hz,
                     double duration_s, double volume, double attack_s,
                     double decay_s, double pan);

// Gun voice (all 12 presets). force_model: -1 = preset default, 0 = classic,
// 1 = physical. pressure_scale multiplies gun_pressure (1.0 = preset default).
uint64_t audio_synth_gun(Audio *a, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model);

void audio_kill  (Audio *a, uint64_t id, double fade_s);
void audio_update(Audio *a, uint64_t id, double freq_hz, double volume, double pan);
void audio_gun_set_param(Audio *a, uint64_t id, const char *key, double value);

// Parse "sine"/"triangle"/"sawtooth"/"saw"/"square"/"noise"/"whistle"/"gun".
WaveType audio_parse_wave(const char *s);

#endif
