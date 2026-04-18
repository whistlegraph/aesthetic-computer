// synth_core.h — Platform-agnostic AC synth engine.
// Holds oscillators (sine/triangle/sawtooth/square/noise/whistle/gun),
// gun presets + init, envelope + fade math, note-to-freq lookup, and
// voice lifecycle helpers operating on an externally-owned voice array.
//
// Consumers:
//   - Linux ALSA driver (fedac/native/src/audio.c) — work-in-progress
//     migration; for now this file can co-exist with audio.c's copies.
//   - macOS SDL3 driver (fedac/native/macos/audio.c) — uses this directly.
#ifndef AC_SYNTH_CORE_H
#define AC_SYNTH_CORE_H

#include "synth_types.h"
#include <pthread.h>

// Externally-owned voice pool the synth operates on. The driver allocates
// the voice array and the mutex; the synth reads/writes under the lock.
typedef struct {
    ACVoice          *voices;
    int               max_voices;
    pthread_mutex_t  *lock;
    uint64_t         *next_id;
    double            sample_rate;
} SynthCore;

void synth_core_init(SynthCore *s,
                     ACVoice *voices, int max_voices,
                     pthread_mutex_t *lock, uint64_t *next_id,
                     double sample_rate);

// Allocate a standard oscillator voice. Returns new id, or 0 on failure.
// `duration` may be INFINITY for sustained voices (held until synth_kill).
uint64_t synth_synth(SynthCore *s, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan);

// Allocate a gun voice. `force_model` -1 = use preset default, 0 = classic,
// 1 = physical. `pressure_scale` scales gun_pressure (1.0 = preset default).
uint64_t synth_synth_gun(SynthCore *s, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model);

void synth_kill  (SynthCore *s, uint64_t id, double fade);
void synth_update(SynthCore *s, uint64_t id, double freq, double vol, double pan);
void synth_gun_set_param(SynthCore *s, uint64_t id, const char *key, double val);

// Pull N stereo float32 frames into `out` (L/R interleaved). Accumulates —
// pre-zero out[] if you want a clean buffer. Advances voice elapsed/phase
// each sample, retires voices that finish (duration expired or kill-fade
// completed). Thread-safe: takes the pool lock internally.
//
// `dt_advance` controls whether elapsed time advances per-sample (true for
// realtime rendering). Pass false only when pre-rendering.
void synth_render(SynthCore *s, float *out, int frames);

// Utilities.
double   synth_note_to_freq(const char *note);
WaveType synth_parse_wave(const char *s);

// Low-level per-voice APIs (driver owns the loop / mixing).
double synth_generate_sample(ACVoice *v, double sample_rate);
double synth_compute_envelope(const ACVoice *v);
double synth_compute_fade(const ACVoice *v);

// Precompute the noise biquad LPF coefficients from v->frequency.
// Call after setting v->frequency on a WAVE_NOISE voice.
void synth_setup_noise_filter(ACVoice *v, double sample_rate);

#endif
