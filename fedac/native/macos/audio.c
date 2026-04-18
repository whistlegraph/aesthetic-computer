// audio.c — SDL3 audio device + tiny synth engine for the macOS host.
// Mixes up to AUDIO_MAX_VOICES stereo float voices in a pull-callback.
// Not drop-in compatible with ../src/audio.c; intentionally simpler.

#include "audio.h"
#include <SDL3/SDL.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#define AUDIO_MAX_VOICES 32
#define AUDIO_SAMPLE_RATE 48000
#define AUDIO_CHANNELS    2

typedef enum {
    VOICE_OFF = 0,
    VOICE_ACTIVE,
    VOICE_KILLING,
} VoiceState;

typedef struct {
    VoiceState state;
    ACWaveType wave;
    double phase;         // 0..1 wave phase
    double freq;          // Hz (smoothed toward target)
    double freq_target;
    double volume;        // 0..1
    double vol_target;    // smoothed toward
    double pan;           // -1..+1 (smoothed)
    double pan_target;
    double attack_s;      // seconds to reach full volume
    double decay_s;       // seconds at the tail of `duration` to fade out
    double duration_s;    // < 0 → infinite
    double elapsed_s;
    double kill_fade_s;   // linear release in seconds (0 = instant)
    double kill_elapsed_s;
    uint32_t noise_state; // xorshift for WAVE_NOISE
    uint64_t id;
} Voice;

struct Audio {
    SDL_AudioStream *stream;
    pthread_mutex_t  lock;
    Voice            voices[AUDIO_MAX_VOICES];
    uint64_t         next_id;
    int              rate;
    // Diagnostics: peak absolute sample emitted since init. Any non-zero
    // value proves the callback produced audio; useful for headless tests.
    volatile float   peak_out;
    volatile uint64_t samples_out;
};

// ── Oscillators ─────────────────────────────────────────────────────────────

static double osc_sine(double p) { return sin(p * 2.0 * M_PI); }
static double osc_tri(double p)  { return fabs(4.0 * (p - floor(p + 0.5))) - 1.0; }  // -1..+1
static double osc_saw(double p)  { double s = 2.0 * p - 1.0; return s > 1.0 ? s - 2.0 : s; }
static double osc_sq(double p)   { return p < 0.5 ? 1.0 : -1.0; }

static double osc_noise(Voice *v) {
    // xorshift32 — cheap, good enough for percussion noise.
    uint32_t x = v->noise_state ? v->noise_state : 0xA5A5A5A5u;
    x ^= x << 13; x ^= x >> 17; x ^= x << 5;
    v->noise_state = x;
    return ((double)x / (double)UINT32_MAX) * 2.0 - 1.0;
}

static double osc_sample(Voice *v) {
    switch (v->wave) {
        case AC_WAVE_SINE:     return osc_sine(v->phase);
        case AC_WAVE_TRIANGLE: return osc_tri(v->phase);
        case AC_WAVE_SAWTOOTH: return osc_saw(v->phase);
        case AC_WAVE_SQUARE:   return osc_sq(v->phase);
        case AC_WAVE_NOISE:    return osc_noise(v);
    }
    return 0.0;
}

// ── Envelope ────────────────────────────────────────────────────────────────

static double envelope(Voice *v, double dt) {
    // Attack: linear ramp 0→1 over attack_s (sticks at 1 after).
    double atk = 1.0;
    if (v->attack_s > 0.0 && v->elapsed_s < v->attack_s) {
        atk = v->elapsed_s / v->attack_s;
    }

    // Duration decay: if duration_s > 0, fade down over decay_s at end.
    double dec = 1.0;
    if (v->duration_s > 0.0) {
        double remaining = v->duration_s - v->elapsed_s;
        if (remaining <= 0.0) return 0.0;  // sample will be zeroed, voice kills below
        if (v->decay_s > 0.0 && remaining < v->decay_s) {
            dec = remaining / v->decay_s;
        }
    }

    // Kill fade (linear): 1 → 0 over kill_fade_s.
    double kill = 1.0;
    if (v->state == VOICE_KILLING) {
        if (v->kill_fade_s <= 0.0) return 0.0;
        kill = 1.0 - (v->kill_elapsed_s / v->kill_fade_s);
        if (kill < 0.0) kill = 0.0;
    }
    (void)dt;
    return atk * dec * kill;
}

static void smooth(double *cur, double tgt, double rate) {
    // 1-pole low-pass toward target. `rate` in (0,1], larger = faster.
    *cur += (tgt - *cur) * rate;
}

// ── Callback ────────────────────────────────────────────────────────────────

static void SDLCALL audio_callback(void *userdata, SDL_AudioStream *stream,
                                   int additional, int total) {
    (void)total;
    Audio *a = (Audio *)userdata;
    if (additional <= 0) return;
    int frames = additional / (int)(sizeof(float) * AUDIO_CHANNELS);
    if (frames <= 0) return;

    // Allocate on the stack for small buffers; fall back to heap otherwise.
    float stack_buf[1024 * AUDIO_CHANNELS];
    float *buf = stack_buf;
    float *heap = NULL;
    size_t need = (size_t)frames * AUDIO_CHANNELS;
    if (need > sizeof(stack_buf) / sizeof(float)) {
        heap = (float *)malloc(need * sizeof(float));
        if (!heap) return;
        buf = heap;
    }
    memset(buf, 0, need * sizeof(float));

    const double dt = 1.0 / (double)a->rate;

    pthread_mutex_lock(&a->lock);
    for (int vi = 0; vi < AUDIO_MAX_VOICES; vi++) {
        Voice *v = &a->voices[vi];
        if (v->state == VOICE_OFF) continue;
        for (int f = 0; f < frames; f++) {
            // Smooth params toward targets every sample — cheap, no zipper.
            smooth(&v->freq,   v->freq_target,   0.002);
            smooth(&v->volume, v->vol_target,    0.002);
            smooth(&v->pan,    v->pan_target,    0.002);

            double env = envelope(v, dt);
            if (env > 0.0) {
                double s = osc_sample(v) * env * v->volume;
                // Equal-power pan law: L = s * cos(theta), R = s * sin(theta)
                // where theta = (pan + 1) * pi/4  maps pan -1..+1 → 0..pi/2.
                double th = (v->pan + 1.0) * (M_PI / 4.0);
                buf[f * 2 + 0] += (float)(s * cos(th));
                buf[f * 2 + 1] += (float)(s * sin(th));
            }

            // Advance.
            v->phase += v->freq * dt;
            if (v->phase >= 1.0) v->phase -= floor(v->phase);
            v->elapsed_s += dt;
            if (v->state == VOICE_KILLING) v->kill_elapsed_s += dt;
        }

        // Voice-end detection after processing the block.
        int expired = 0;
        if (v->duration_s > 0.0 && v->elapsed_s >= v->duration_s) expired = 1;
        if (v->state == VOICE_KILLING && v->kill_elapsed_s >= v->kill_fade_s) expired = 1;
        if (expired) v->state = VOICE_OFF;
    }
    pthread_mutex_unlock(&a->lock);

    // Update diagnostics.
    float peak = 0.0f;
    for (size_t i = 0; i < need; i++) {
        float v = buf[i]; if (v < 0) v = -v;
        if (v > peak) peak = v;
    }
    if (peak > a->peak_out) a->peak_out = peak;
    a->samples_out += (uint64_t)frames;

    SDL_PutAudioStreamData(stream, buf, additional);
    if (heap) free(heap);
}

// ── Public API ──────────────────────────────────────────────────────────────

ACWaveType audio_parse_wave(const char *s) {
    if (!s) return AC_WAVE_SINE;
    if (!strcmp(s, "sine"))      return AC_WAVE_SINE;
    if (!strcmp(s, "triangle"))  return AC_WAVE_TRIANGLE;
    if (!strcmp(s, "sawtooth"))  return AC_WAVE_SAWTOOTH;
    if (!strcmp(s, "saw"))       return AC_WAVE_SAWTOOTH;
    if (!strcmp(s, "square"))    return AC_WAVE_SQUARE;
    if (!strcmp(s, "noise"))     return AC_WAVE_NOISE;
    // unknown families (whistle, cmp, sample, gun, percussion etc.) fall back
    // to sine for Phase B. Phase C can port the richer models.
    return AC_WAVE_SINE;
}

Audio *audio_init(void) {
    Audio *a = (Audio *)calloc(1, sizeof(Audio));
    if (!a) return NULL;
    pthread_mutex_init(&a->lock, NULL);
    a->rate = AUDIO_SAMPLE_RATE;
    a->next_id = 1;

    if (!SDL_InitSubSystem(SDL_INIT_AUDIO)) {
        fprintf(stderr, "[audio] SDL_InitSubSystem: %s\n", SDL_GetError());
        free(a);
        return NULL;
    }
    SDL_AudioSpec spec = { SDL_AUDIO_F32, AUDIO_CHANNELS, AUDIO_SAMPLE_RATE };
    a->stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
                                          &spec, audio_callback, a);
    if (!a->stream) {
        fprintf(stderr, "[audio] SDL_OpenAudioDeviceStream: %s\n", SDL_GetError());
        pthread_mutex_destroy(&a->lock);
        free(a);
        return NULL;
    }
    SDL_ResumeAudioStreamDevice(a->stream);
    fprintf(stderr, "[audio] ready @ %d Hz, %d channels\n", a->rate, AUDIO_CHANNELS);
    return a;
}

void audio_destroy(Audio *a) {
    if (!a) return;
    fprintf(stderr, "[audio] stop: %llu samples emitted, peak=%.3f\n",
            (unsigned long long)a->samples_out, a->peak_out);
    if (a->stream) SDL_DestroyAudioStream(a->stream);
    pthread_mutex_destroy(&a->lock);
    free(a);
}

uint64_t audio_synth(Audio *a, ACWaveType wave, double freq, double duration,
                     double volume, double attack, double decay, double pan) {
    if (!a) return 0;
    pthread_mutex_lock(&a->lock);
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (a->voices[i].state == VOICE_OFF) { slot = i; break; }
    }
    if (slot < 0) {
        // Voice theft: drop the oldest KILLING voice, else the oldest ACTIVE.
        int oldest = 0; double max_t = -1.0;
        for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
            if (a->voices[i].state != VOICE_OFF && a->voices[i].elapsed_s > max_t) {
                max_t = a->voices[i].elapsed_s; oldest = i;
            }
        }
        slot = oldest;
    }
    Voice *v = &a->voices[slot];
    memset(v, 0, sizeof(*v));
    v->state        = VOICE_ACTIVE;
    v->wave         = wave;
    v->freq         = freq;
    v->freq_target  = freq;
    v->volume       = 0.0;           // start silent; attack ramps up
    v->vol_target   = volume;
    v->pan          = pan;
    v->pan_target   = pan;
    v->attack_s     = attack;
    v->decay_s      = decay;
    v->duration_s   = (duration < 0 || !isfinite(duration)) ? -1.0 : duration;
    v->noise_state  = (uint32_t)(slot + 1) * 2654435761u;
    v->id           = a->next_id++;
    pthread_mutex_unlock(&a->lock);
    return v->id;
}

void audio_kill(Audio *a, uint64_t id, double fade) {
    if (!a || !id) return;
    pthread_mutex_lock(&a->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (a->voices[i].id == id && a->voices[i].state != VOICE_OFF) {
            a->voices[i].state = VOICE_KILLING;
            a->voices[i].kill_fade_s = fade > 0.0 ? fade : 0.005;
            a->voices[i].kill_elapsed_s = 0.0;
            break;
        }
    }
    pthread_mutex_unlock(&a->lock);
}

void audio_update(Audio *a, uint64_t id, double freq, double volume, double pan) {
    if (!a || !id) return;
    pthread_mutex_lock(&a->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        Voice *v = &a->voices[i];
        if (v->id != id || v->state == VOICE_OFF) continue;
        if (!isnan(freq)   && freq   >= 0.0) v->freq_target   = freq;
        if (!isnan(volume) && volume >= 0.0) v->vol_target    = volume;
        if (!isnan(pan))                     v->pan_target    = pan;
        break;
    }
    pthread_mutex_unlock(&a->lock);
}
