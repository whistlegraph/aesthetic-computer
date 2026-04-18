// audio.h — minimal SDL3-backed synth for the macOS ac-native host.
// Phase B MVP: sine/triangle/sawtooth/square/noise oscillators, ADSR-ish
// envelope, voice pool, stereo pan. Intentionally narrower than the Linux
// ALSA engine in ../src/audio.c; enough for notepat to make sound.
#ifndef AC_MACOS_AUDIO_H
#define AC_MACOS_AUDIO_H

#include <stdint.h>

typedef enum {
    AC_WAVE_SINE = 0,
    AC_WAVE_TRIANGLE,
    AC_WAVE_SAWTOOTH,
    AC_WAVE_SQUARE,
    AC_WAVE_NOISE,
} ACWaveType;

typedef struct Audio Audio;

Audio *audio_init(void);
void   audio_destroy(Audio *a);

// Allocate a voice. Returns an opaque id > 0 (0 = failure or silent).
// `duration_s` < 0 is treated as infinite (sustained until killed).
uint64_t audio_synth(Audio *a,
                     ACWaveType wave,
                     double freq_hz,
                     double duration_s,
                     double volume,
                     double attack_s,
                     double decay_s,
                     double pan);

// Kill an active voice with an optional linear fade (seconds).
void audio_kill(Audio *a, uint64_t id, double fade_s);

// Update freq/volume/pan on an active voice. NaN/negative skips a field.
void audio_update(Audio *a, uint64_t id, double freq_hz, double volume, double pan);

// Parse a wave-type string ("sine", "triangle", "sawtooth"/"saw",
// "square", "noise"). Unknown strings fall back to sine.
ACWaveType audio_parse_wave(const char *s);

#endif
