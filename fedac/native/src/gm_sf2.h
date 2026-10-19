// gm_sf2.h — General MIDI from a SoundFont bank (TinySoundFont).
//
// One TSF instance, one channel per note slot so chords and repeated keys
// never share a note-off. The JS/main thread only queues commands; the audio
// thread drains them at the top of each render, so TSF is touched by exactly
// one thread after load and the audio thread never waits on a lock.
//
// Like gm_synth.c this pulls in no engine headers, so it builds standalone.

#ifndef GM_SF2_H
#define GM_SF2_H

#define GM_SF2_RATE 48000   // TSF's internal rate; gm_sf2_render resamples

// Blocking: reads the whole bank. Call from a loader thread, once.
// Returns 1 on success. gm_sf2_ready() flips to 1 only after the bank is live.
int gm_sf2_load(const char *path, int sample_rate);
int gm_sf2_ready(void);
const char *gm_sf2_name(void);     // basename of the loaded bank, or ""

// Main thread. `midi` may be fractional (tuning is applied per channel).
// Finite duration_s schedules its own note-off; pass INFINITY for held notes.
// Drums use bank 128 with `program` as the kit and `midi` as the key.
// Returns a nonzero handle, or 0 when not ready or the queue is full.
int  gm_sf2_note_on(int program, double midi, float velocity, float pan,
                    double duration_s, int is_drum);
// fade_s <= GM_SF2_RELEASE_MAX → plain note-off, the preset's own release
// plays. Longer fades ramp the channel to silence first (notepat's linger).
#define GM_SF2_RELEASE_MAX 0.6
void gm_sf2_note_off(int handle, double fade_s);
// midi < 0, volume < 0, pan < -1 leave that field alone. volume is in
// note-on velocity units; the change rides channel gain, so the velocity
// layer chosen at note-on stays.
void gm_sf2_update(int handle, double midi, float volume, float pan);

// Audio thread only. WRITES `frames` interleaved stereo frames at out_rate
// (overwrites, does not add). Renders TSF at GM_SF2_RATE and linearly
// upsamples, so a 192 kHz engine pays for 48 kHz of sampler work.
void gm_sf2_render(float *out, int frames, int out_rate);

#endif
