// audio_musicdevice.c — AUMIDISynth-backed audio backend for "Notepat Grand".
// Uses Apple's kAudioUnitSubType_MIDISynth which ships with the default
// General MIDI soundbank (gs_instruments.dls). Program 0 = Acoustic Grand
// Piano — same engine GarageBand/Logic use for quick MIDI playback before
// swapping to their proprietary sample libraries. No asset bundling.
//
// Mapping: sound.synth({tone, volume, duration, ...}) → NoteOn; kill() or
// duration expiry → NoteOff. Tone (Hz) → nearest integer MIDI note; volume
// (0..1) → MIDI velocity. Polyphonic by default (MIDI handles voice mgmt).

#include "audio.h"
#include "synth_core.h"

#include <AudioUnit/AudioUnit.h>
#include <AudioToolbox/AudioToolbox.h>
#include <CoreAudio/CoreAudio.h>

#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <time.h>

#define MIDI_CHANNEL         0
#define DEFAULT_GM_PROGRAM   0   /* Acoustic Grand Piano */

static uint64_t now_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

// Per-voice slot that tracks which MIDI note is currently sounding so kill()
// can turn the right one off. Matches synth_types' AUDIO_MAX_VOICES.
typedef struct {
    int      active;
    uint64_t id;
    int      note;       // MIDI note number
    // If duration was finite, a detached thread schedules the NoteOff.
    // The flag below lets kill() / process-shutdown pre-empt the timer.
    int      duration_fired;
} GrandVoice;

struct Audio {
    AUGraph         graph;
    AUNode          synth_node, out_node;
    AudioUnit       synth_au, out_au;
    pthread_mutex_t lock;
    GrandVoice      voices[AUDIO_MAX_VOICES];
    uint64_t        next_id;
    // Still honor the latency instrumentation surface so AC_LATENCY_TEST
    // works against this backend too, even though the timing chain is
    // different (MIDI event → AU render).
    volatile uint64_t trigger_ns;
    volatile uint64_t emit_ns;
    volatile float    emit_threshold;  // unused; emit is stamped at NoteOn
    volatile uint64_t samples_out;
    volatile float    peak_out;
};

// We don't own the render callback (MIDISynth does), so latency is stamped
// at NoteOn dispatch — pessimistic upper bound relative to SDL3/CoreAudio
// direct (which stamps in the audio callback). Worst-case the difference is
// the AU's internal buffer period (~1–3 ms on CoreAudio default).

// ── Helpers ────────────────────────────────────────────────────────────────

static int hz_to_midi(double hz) {
    if (hz < 8.0) hz = 8.0;
    int note = (int)lround(12.0 * log2(hz / 440.0) + 69.0);
    if (note < 0) note = 0;
    if (note > 127) note = 127;
    return note;
}

static int vol_to_velocity(double v) {
    // notepat passes most note volumes in the 0.3–0.6 range. Raw mapping
    // (v*127) would give velocity 38–76, which on a piano bank reads as
    // "barely touching the key." Boost so a typical hit lands around the
    // 90–120 range where the bank's loudest samples get selected.
    if (v < 0) v = 0;
    double boosted = v * 180.0;
    int vel = (int)lround(boosted);
    if (vel < 1) vel = 1;
    if (vel > 127) vel = 127;
    return vel;
}

static void send_note_on(Audio *a, int note, int velocity) {
    MusicDeviceMIDIEvent(a->synth_au, 0x90 | MIDI_CHANNEL, note, velocity, 0);
}
static void send_note_off(Audio *a, int note) {
    MusicDeviceMIDIEvent(a->synth_au, 0x80 | MIDI_CHANNEL, note, 0, 0);
}
static void send_program_change(Audio *a, int program) {
    MusicDeviceMIDIEvent(a->synth_au, 0xC0 | MIDI_CHANNEL, program & 0x7F, 0, 0);
}

// ── Duration timer ─────────────────────────────────────────────────────────
// Finite-duration notes need a NoteOff scheduled at `duration` seconds from
// NoteOn. MusicDeviceMIDIEvent with `inOffsetSampleFrame` only schedules
// within a single render cycle, so for arbitrary durations we use detached
// threads. Light enough for notepat's voice counts (max 32 active). Each
// thread captures its slot index; the lock guards concurrent access.

typedef struct {
    Audio   *a;
    int      slot;
    uint64_t id;
    double   duration_s;
} TimerArg;

static void *timer_off_thread(void *arg) {
    TimerArg *t = (TimerArg *)arg;
    Audio *a = t->a;
    int slot = t->slot;
    uint64_t id = t->id;
    double s = t->duration_s;
    free(t);

    struct timespec ts;
    ts.tv_sec = (time_t)s;
    ts.tv_nsec = (long)((s - (double)ts.tv_sec) * 1e9);
    nanosleep(&ts, NULL);

    pthread_mutex_lock(&a->lock);
    if (a->voices[slot].active && a->voices[slot].id == id && !a->voices[slot].duration_fired) {
        send_note_off(a, a->voices[slot].note);
        a->voices[slot].active = 0;
        a->voices[slot].duration_fired = 1;
    }
    pthread_mutex_unlock(&a->lock);
    return NULL;
}

// ── Public API ──────────────────────────────────────────────────────────────

Audio *audio_init(void) {
    Audio *a = (Audio *)calloc(1, sizeof(Audio));
    if (!a) return NULL;
    pthread_mutex_init(&a->lock, NULL);
    a->next_id = 0;

    if (NewAUGraph(&a->graph) != noErr) { free(a); return NULL; }
    AudioComponentDescription synthDesc = {
        .componentType         = kAudioUnitType_MusicDevice,
        .componentSubType      = kAudioUnitSubType_MIDISynth,
        .componentManufacturer = kAudioUnitManufacturer_Apple,
    };
    AudioComponentDescription outDesc = {
        .componentType         = kAudioUnitType_Output,
        .componentSubType      = kAudioUnitSubType_DefaultOutput,
        .componentManufacturer = kAudioUnitManufacturer_Apple,
    };
    if (AUGraphAddNode(a->graph, &synthDesc, &a->synth_node) != noErr
     || AUGraphAddNode(a->graph, &outDesc,   &a->out_node) != noErr
     || AUGraphOpen(a->graph) != noErr
     || AUGraphNodeInfo(a->graph, a->synth_node, NULL, &a->synth_au) != noErr
     || AUGraphNodeInfo(a->graph, a->out_node,   NULL, &a->out_au)   != noErr
     || AUGraphConnectNodeInput(a->graph, a->synth_node, 0, a->out_node, 0) != noErr) {
        fprintf(stderr, "[audio] AUGraph construction failed\n");
        DisposeAUGraph(a->graph);
        free(a);
        return NULL;
    }

    // Stream-from-disk ON so we don't wait for the full bank to preload.
    UInt32 enable = 1;
    AudioUnitSetProperty(a->synth_au, kMusicDeviceProperty_StreamFromDisk,
                         kAudioUnitScope_Input, 0, &enable, sizeof(enable));

    if (AUGraphInitialize(a->graph) != noErr) {
        fprintf(stderr, "[audio] AUGraphInitialize failed\n");
        DisposeAUGraph(a->graph);
        free(a);
        return NULL;
    }
    if (AUGraphStart(a->graph) != noErr) {
        fprintf(stderr, "[audio] AUGraphStart failed\n");
        AUGraphUninitialize(a->graph);
        DisposeAUGraph(a->graph);
        free(a);
        return NULL;
    }

    int program = DEFAULT_GM_PROGRAM;
    const char *env = getenv("AC_GM_PROGRAM");
    if (env) program = atoi(env);
    send_program_change(a, program);

    fprintf(stderr, "[audio] AUMIDISynth ready (GM program %d = %s)\n",
            program, program == 0 ? "Acoustic Grand Piano" : "custom");
    return a;
}

void audio_destroy(Audio *a) {
    if (!a) return;
    // All-notes-off so the synth doesn't dangle held voices.
    for (int ch = 0; ch < 16; ch++) {
        MusicDeviceMIDIEvent(a->synth_au, 0xB0 | ch, 123, 0, 0);  // CC#123 all notes off
    }
    AUGraphStop(a->graph);
    AUGraphUninitialize(a->graph);
    DisposeAUGraph(a->graph);
    pthread_mutex_destroy(&a->lock);
    fprintf(stderr, "[audio] stop: MIDISynth\n");
    free(a);
}

uint64_t audio_synth(Audio *a, WaveType w, double freq, double dur, double vol,
                     double att, double dec, double pan) {
    (void)w; (void)att; (void)dec; (void)pan;  // MIDI piano ignores oscillator shape + envelope shape
    if (!a) return 0;

    pthread_mutex_lock(&a->lock);
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (!a->voices[i].active) { slot = i; break; }
    }
    if (slot < 0) slot = 0;  // voice theft: reuse slot 0
    // If stealing, mute that slot's current note first.
    if (a->voices[slot].active) {
        send_note_off(a, a->voices[slot].note);
    }

    int note = hz_to_midi(freq);
    int vel  = vol_to_velocity(vol);

    uint64_t id = ++a->next_id;
    a->voices[slot].active          = 1;
    a->voices[slot].id              = id;
    a->voices[slot].note            = note;
    a->voices[slot].duration_fired  = 0;

    if (a->trigger_ns && !a->emit_ns) a->emit_ns = now_ns();

    send_note_on(a, note, vel);

    // Finite duration → detached timer thread that fires NoteOff.
    // INFINITY or <= 0 stays sustained until kill().
    if (dur > 0 && isfinite(dur)) {
        TimerArg *t = (TimerArg *)malloc(sizeof(TimerArg));
        if (t) {
            t->a = a; t->slot = slot; t->id = id; t->duration_s = dur;
            pthread_t th;
            pthread_attr_t attr;
            pthread_attr_init(&attr);
            pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
            pthread_create(&th, &attr, timer_off_thread, t);
            pthread_attr_destroy(&attr);
        }
    }

    pthread_mutex_unlock(&a->lock);
    return id;
}

uint64_t audio_synth_gun(Audio *a, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model) {
    (void)preset; (void)attack; (void)decay; (void)pan;
    (void)pressure_scale; (void)force_model;
    // Route gun-preset triggers to percussion notes (GM channel 10 has its
    // own drum map). For simplicity map all to the current program's kick-
    // drum equivalent (C2 = MIDI 36) at the requested volume.
    return audio_synth(a, WAVE_NOISE, 65.41, duration, volume, 0.0, 0.0, 0.0);
}

void audio_kill(Audio *a, uint64_t id, double fade) {
    (void)fade;  // MIDI NoteOff is immediate; release is baked into the AU patch
    if (!a || !id) return;
    pthread_mutex_lock(&a->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (a->voices[i].active && a->voices[i].id == id) {
            send_note_off(a, a->voices[i].note);
            a->voices[i].active = 0;
            a->voices[i].duration_fired = 1;
            break;
        }
    }
    pthread_mutex_unlock(&a->lock);
}

void audio_update(Audio *a, uint64_t id, double freq, double vol, double pan) {
    (void)a; (void)id; (void)freq; (void)vol; (void)pan;
    // Update is a no-op in MIDI-land — pitch/vel fixed at NoteOn for piano.
}

void audio_gun_set_param(Audio *a, uint64_t id, const char *key, double value) {
    (void)a; (void)id; (void)key; (void)value;  // n/a for MIDI backend
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
