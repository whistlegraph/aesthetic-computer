// audio.c — SDL3 audio driver for the macOS host.
// Thin shell around fedac/native/src/synth_core: owns the SDL3 audio stream
// and pulls stereo float frames out of the shared synth engine.

#include "audio.h"
#include "synth_core.h"

#include <SDL3/SDL.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

// Lower than the Linux 192 kHz target — 48 kHz keeps CPU/scheduling headroom
// on the laptop, and the DSP models behave cleanly at 48 kHz as well.
#define DRIVER_SAMPLE_RATE 48000
#define DRIVER_CHANNELS    2

struct Audio {
    SDL_AudioStream *stream;
    SynthCore       synth;
    pthread_mutex_t lock;
    uint64_t        next_id;
    ACVoice         voices[AUDIO_MAX_VOICES];
    // Diagnostics for headless tests. Peak of any sample emitted since init,
    // total sample count. Useful to detect silence regressions in CI runs.
    volatile float    peak_out;
    volatile uint64_t samples_out;
};

static void SDLCALL audio_callback(void *userdata, SDL_AudioStream *stream,
                                   int additional, int total) {
    (void)total;
    Audio *a = (Audio *)userdata;
    if (additional <= 0) return;
    int frames = additional / (int)(sizeof(float) * DRIVER_CHANNELS);
    if (frames <= 0) return;

    // Scratch stereo buffer. Stack path covers the common case; fallback to
    // heap for larger pulls (some audio drivers ask for big blocks up front).
    float stack_buf[1024 * DRIVER_CHANNELS];
    float *buf = stack_buf;
    float *heap = NULL;
    size_t need = (size_t)frames * DRIVER_CHANNELS;
    if (need > sizeof(stack_buf) / sizeof(float)) {
        heap = (float *)malloc(need * sizeof(float));
        if (!heap) return;
        buf = heap;
    }
    memset(buf, 0, need * sizeof(float));

    synth_render(&a->synth, buf, frames);

    // Track peak + sample count for test observability.
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

Audio *audio_init(void) {
    Audio *a = (Audio *)calloc(1, sizeof(Audio));
    if (!a) return NULL;
    pthread_mutex_init(&a->lock, NULL);
    a->next_id = 0;
    synth_core_init(&a->synth, a->voices, AUDIO_MAX_VOICES,
                    &a->lock, &a->next_id, (double)DRIVER_SAMPLE_RATE);

    if (!SDL_InitSubSystem(SDL_INIT_AUDIO)) {
        fprintf(stderr, "[audio] SDL_InitSubSystem: %s\n", SDL_GetError());
        pthread_mutex_destroy(&a->lock);
        free(a);
        return NULL;
    }
    SDL_AudioSpec spec = { SDL_AUDIO_F32, DRIVER_CHANNELS, DRIVER_SAMPLE_RATE };
    a->stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
                                          &spec, audio_callback, a);
    if (!a->stream) {
        fprintf(stderr, "[audio] SDL_OpenAudioDeviceStream: %s\n", SDL_GetError());
        pthread_mutex_destroy(&a->lock);
        free(a);
        return NULL;
    }
    SDL_ResumeAudioStreamDevice(a->stream);
    fprintf(stderr, "[audio] ready @ %d Hz, %d channels (synth_core)\n",
            DRIVER_SAMPLE_RATE, DRIVER_CHANNELS);
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

uint64_t audio_synth(Audio *a, WaveType w, double freq, double dur, double vol,
                     double att, double dec, double pan) {
    return a ? synth_synth(&a->synth, w, freq, dur, vol, att, dec, pan) : 0;
}

uint64_t audio_synth_gun(Audio *a, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model) {
    return a ? synth_synth_gun(&a->synth, preset, duration, volume, attack,
                               decay, pan, pressure_scale, force_model) : 0;
}

void audio_kill(Audio *a, uint64_t id, double fade) {
    if (a) synth_kill(&a->synth, id, fade);
}

void audio_update(Audio *a, uint64_t id, double freq, double vol, double pan) {
    if (a) synth_update(&a->synth, id, freq, vol, pan);
}

void audio_gun_set_param(Audio *a, uint64_t id, const char *key, double value) {
    if (a) synth_gun_set_param(&a->synth, id, key, value);
}

WaveType audio_parse_wave(const char *s) { return synth_parse_wave(s); }
