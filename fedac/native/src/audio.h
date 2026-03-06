#ifndef AC_AUDIO_H
#define AC_AUDIO_H

#include <stdint.h>
#include <pthread.h>

#define AUDIO_SAMPLE_RATE 48000
#define AUDIO_CHANNELS 2
#define AUDIO_PERIOD_SIZE 256
#define AUDIO_MAX_VOICES 32
#define AUDIO_WAVEFORM_SIZE 512

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
    WAVE_NOISE
} WaveType;

typedef struct {
    VoiceState state;
    WaveType type;
    double phase;           // 0.0-1.0 phase accumulator
    double frequency;       // Hz
    double volume;          // 0.0-1.0
    double pan;             // -1.0 to 1.0
    double attack;          // seconds
    double decay;           // seconds (time before end to start fading)
    double duration;        // seconds (INFINITY for sustained)
    double elapsed;         // seconds since start
    double fade_duration;   // for kill(fade)
    double fade_elapsed;    // progress through fade
    double started_at;      // monotonic time reference
    uint64_t id;            // unique voice ID
    // Noise filter state
    double noise_b0, noise_b1, noise_b2, noise_a1, noise_a2;
    double noise_x1, noise_x2, noise_y1, noise_y2;
    uint32_t noise_seed;
} ACVoice;

typedef struct {
    void *pcm;              // snd_pcm_t* (void to avoid header dep)
    pthread_t thread;
    volatile int running;

    ACVoice voices[AUDIO_MAX_VOICES];
    pthread_mutex_t lock;

    uint64_t next_id;
    double time;            // current audio time
    uint64_t total_frames;

    // Speaker poll data
    float waveform_left[AUDIO_WAVEFORM_SIZE];
    float waveform_right[AUDIO_WAVEFORM_SIZE];
    float amplitude_left;
    float amplitude_right;
    int waveform_pos;

    // BPM / metronome
    double bpm;
    double beat_elapsed;
    volatile int beat_triggered;

    // Effects
    int room_enabled;
    float *room_buf_l, *room_buf_r;
    int room_pos;
    int room_size;
    float room_mix;  // 0.0 to 1.0 wet mix (controllable from JS)

    int glitch_enabled;
    float glitch_hold_l, glitch_hold_r;
    int glitch_counter;
    int glitch_rate;        // samples between holds

    // System mixer volume (0-100 percent)
    int system_volume;
    int card_index;  // ALSA card number (0 or 1)
} ACAudio;

// Initialize ALSA audio engine (returns NULL if no audio device)
ACAudio *audio_init(void);

// Add a new voice, returns voice ID
uint64_t audio_synth(ACAudio *audio, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan);

// Kill a voice with fade
void audio_kill(ACAudio *audio, uint64_t id, double fade);

// Update a voice's parameters
void audio_update(ACAudio *audio, uint64_t id, double freq,
                  double volume, double pan);

// Check if beat was triggered (and clear flag)
int audio_beat_check(ACAudio *audio);

// Set BPM
void audio_set_bpm(ACAudio *audio, double bpm);

// Toggle effects
void audio_room_toggle(ACAudio *audio);
void audio_glitch_toggle(ACAudio *audio);
void audio_set_room_mix(ACAudio *audio, float mix);

// Adjust system volume: delta is -5 to +5 (percentage points), 0 = toggle mute
void audio_volume_adjust(ACAudio *audio, int delta);

// Cleanup
void audio_destroy(ACAudio *audio);

// Convert note name to frequency
double audio_note_to_freq(const char *note);

#endif
