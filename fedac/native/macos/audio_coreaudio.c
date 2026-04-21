// audio_coreaudio.c — direct CoreAudio AudioUnit backend for macOS.
// Wraps fedac/native/src/synth_core with a kAudioUnitSubType_DefaultOutput
// AudioUnit and sets kAudioDevicePropertyBufferFrameSize on the hardware
// device for the tightest realtime path we can take without writing a
// HAL plugin. Chosen over SDL3 audio to test whether SDL's audio stream
// indirection adds latency.

#include "audio.h"
#include "synth_core.h"

#include <AudioUnit/AudioUnit.h>
#include <AudioToolbox/AudioToolbox.h>
#include <CoreAudio/CoreAudio.h>

#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#define DRIVER_SAMPLE_RATE 48000

static uint64_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

struct Audio {
    AudioUnit        au;
    SynthCore        synth;
    pthread_mutex_t  lock;
    uint64_t         next_id;
    ACVoice          voices[AUDIO_MAX_VOICES];

    volatile float    peak_out;
    volatile uint64_t samples_out;
    volatile uint64_t trigger_ns;
    volatile uint64_t emit_ns;
    volatile float    emit_threshold;

    // Scratch interleaved stereo buffer for synth_render output (it writes
    // L/R interleaved; CoreAudio expects non-interleaved on the two buffers
    // of the AudioBufferList).
    float           *scratch;
    int              scratch_cap;

    int              actual_frames;   // negotiated device buffer size
};

static OSStatus render_cb(void *inRef,
                          AudioUnitRenderActionFlags *flags,
                          const AudioTimeStamp *ts,
                          UInt32 bus,
                          UInt32 frames,
                          AudioBufferList *io) {
    (void)flags; (void)ts; (void)bus;
    Audio *a = (Audio *)inRef;
    if (!io || io->mNumberBuffers < 2 || frames == 0) return noErr;

    float *L = (float *)io->mBuffers[0].mData;
    float *R = (float *)io->mBuffers[1].mData;
    if (!L || !R) return noErr;

    int need = (int)frames * 2;
    if (need > a->scratch_cap) {
        // Grow under lock — callback thread is the only caller, so this is
        // safe without synchronization; allocation cost is paid once per
        // buffer-size change (usually just the first callback).
        free(a->scratch);
        a->scratch = (float *)malloc((size_t)need * sizeof(float));
        a->scratch_cap = a->scratch ? need : 0;
        if (!a->scratch) {
            memset(L, 0, frames * sizeof(float));
            memset(R, 0, frames * sizeof(float));
            return noErr;
        }
    }
    memset(a->scratch, 0, (size_t)need * sizeof(float));
    synth_render(&a->synth, a->scratch, (int)frames);

    float peak = 0.0f;
    for (UInt32 i = 0; i < frames; i++) {
        float l = a->scratch[i * 2 + 0];
        float r = a->scratch[i * 2 + 1];
        L[i] = l; R[i] = r;
        float al = l < 0 ? -l : l; if (al > peak) peak = al;
        float ar = r < 0 ? -r : r; if (ar > peak) peak = ar;
    }
    if (peak > a->peak_out) a->peak_out = peak;
    a->samples_out += (uint64_t)frames;

    if (a->trigger_ns && !a->emit_ns && peak > a->emit_threshold) {
        a->emit_ns = now_ns();
    }
    return noErr;
}

// Request a specific hardware buffer size on the default output device.
// CoreAudio clamps to [min, max] reported by the driver; actual negotiated
// value is queried back for logging. Returns the accepted size, or 0.
static UInt32 set_hw_buffer(UInt32 target) {
    AudioObjectPropertyAddress addr = {
        kAudioHardwarePropertyDefaultOutputDevice,
        kAudioObjectPropertyScopeGlobal,
        kAudioObjectPropertyElementMain,
    };
    AudioDeviceID dev = 0;
    UInt32 sz = sizeof(dev);
    if (AudioObjectGetPropertyData(kAudioObjectSystemObject, &addr, 0, NULL, &sz, &dev) != noErr)
        return 0;

    AudioValueRange range = {0};
    addr.mSelector = kAudioDevicePropertyBufferFrameSizeRange;
    addr.mScope    = kAudioObjectPropertyScopeOutput;
    sz = sizeof(range);
    if (AudioObjectGetPropertyData(dev, &addr, 0, NULL, &sz, &range) == noErr) {
        if (target < (UInt32)range.mMinimum) target = (UInt32)range.mMinimum;
        if (target > (UInt32)range.mMaximum) target = (UInt32)range.mMaximum;
    }

    addr.mSelector = kAudioDevicePropertyBufferFrameSize;
    sz = sizeof(target);
    if (AudioObjectSetPropertyData(dev, &addr, 0, NULL, sz, &target) != noErr) return 0;

    UInt32 got = 0; sz = sizeof(got);
    AudioObjectGetPropertyData(dev, &addr, 0, NULL, &sz, &got);
    return got;
}

Audio *audio_init(void) {
    Audio *a = (Audio *)calloc(1, sizeof(Audio));
    if (!a) return NULL;
    pthread_mutex_init(&a->lock, NULL);
    a->next_id = 0;
    synth_core_init(&a->synth, a->voices, AUDIO_MAX_VOICES,
                    &a->lock, &a->next_id, (double)DRIVER_SAMPLE_RATE);

    // Set hardware buffer size BEFORE wiring the AU — CoreAudio applies it
    // to the shared device, so our AU inherits the tighter schedule.
    const char *buf_env = getenv("AC_AUDIO_BUFFER");
    UInt32 target = buf_env ? (UInt32)atoi(buf_env) : 32;
    UInt32 got = set_hw_buffer(target);

    AudioComponentDescription desc = {
        .componentType         = kAudioUnitType_Output,
        .componentSubType      = kAudioUnitSubType_DefaultOutput,
        .componentManufacturer = kAudioUnitManufacturer_Apple,
    };
    AudioComponent comp = AudioComponentFindNext(NULL, &desc);
    if (!comp) { fprintf(stderr, "[audio] no default output component\n"); pthread_mutex_destroy(&a->lock); free(a); return NULL; }
    OSStatus s = AudioComponentInstanceNew(comp, &a->au);
    if (s != noErr) { fprintf(stderr, "[audio] AudioComponentInstanceNew: %d\n", (int)s); pthread_mutex_destroy(&a->lock); free(a); return NULL; }

    // Non-interleaved F32 stereo @ 48 kHz — CoreAudio's native path, no
    // conversion overhead.
    AudioStreamBasicDescription fmt = {
        .mSampleRate       = DRIVER_SAMPLE_RATE,
        .mFormatID         = kAudioFormatLinearPCM,
        .mFormatFlags      = kAudioFormatFlagIsFloat
                           | kAudioFormatFlagIsPacked
                           | kAudioFormatFlagIsNonInterleaved,
        .mBitsPerChannel   = 32,
        .mChannelsPerFrame = 2,
        .mFramesPerPacket  = 1,
        .mBytesPerFrame    = 4,
        .mBytesPerPacket   = 4,
    };
    s = AudioUnitSetProperty(a->au, kAudioUnitProperty_StreamFormat,
                             kAudioUnitScope_Input, 0, &fmt, sizeof(fmt));
    if (s != noErr) { fprintf(stderr, "[audio] SetProperty StreamFormat: %d\n", (int)s); goto fail; }

    AURenderCallbackStruct cb = { .inputProc = render_cb, .inputProcRefCon = a };
    s = AudioUnitSetProperty(a->au, kAudioUnitProperty_SetRenderCallback,
                             kAudioUnitScope_Input, 0, &cb, sizeof(cb));
    if (s != noErr) { fprintf(stderr, "[audio] SetProperty RenderCallback: %d\n", (int)s); goto fail; }

    s = AudioUnitInitialize(a->au);
    if (s != noErr) { fprintf(stderr, "[audio] AudioUnitInitialize: %d\n", (int)s); goto fail; }
    s = AudioOutputUnitStart(a->au);
    if (s != noErr) { fprintf(stderr, "[audio] AudioOutputUnitStart: %d\n", (int)s); AudioUnitUninitialize(a->au); goto fail; }

    a->actual_frames = (int)got;
    double latency_ms = got > 0 ? (1000.0 * (double)got / (double)DRIVER_SAMPLE_RATE) : 0.0;
    fprintf(stderr, "[audio] CoreAudio AU @ %d Hz, 2 ch, buf=%u frames (~%.2f ms device latency)\n",
            DRIVER_SAMPLE_RATE, (unsigned)got, latency_ms);
    return a;

fail:
    AudioComponentInstanceDispose(a->au);
    pthread_mutex_destroy(&a->lock);
    free(a);
    return NULL;
}

void audio_destroy(Audio *a) {
    if (!a) return;
    fprintf(stderr, "[audio] stop: %llu samples emitted, peak=%.3f\n",
            (unsigned long long)a->samples_out, a->peak_out);
    if (a->au) {
        AudioOutputUnitStop(a->au);
        AudioUnitUninitialize(a->au);
        AudioComponentInstanceDispose(a->au);
    }
    free(a->scratch);
    pthread_mutex_destroy(&a->lock);
    free(a);
}

// ── Pass-throughs to synth_core ────────────────────────────────────────────

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
void audio_kill(Audio *a, uint64_t id, double fade) { if (a) synth_kill(&a->synth, id, fade); }
void audio_update(Audio *a, uint64_t id, double freq, double vol, double pan) {
    if (a) synth_update(&a->synth, id, freq, vol, pan);
}
void audio_gun_set_param(Audio *a, uint64_t id, const char *key, double value) {
    if (a) synth_gun_set_param(&a->synth, id, key, value);
}
WaveType audio_parse_wave(const char *s) { return synth_parse_wave(s); }

void audio_arm_latency(Audio *a, float threshold) {
    if (!a) return;
    a->emit_ns = 0;
    a->emit_threshold = threshold > 0.0f ? threshold : 0.02f;
    a->trigger_ns = now_ns();
}
uint64_t audio_latency_ns(Audio *a) {
    if (!a || !a->trigger_ns || !a->emit_ns) return 0;
    return a->emit_ns - a->trigger_ns;
}
