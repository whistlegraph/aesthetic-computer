#ifndef AC_AUDIO_H
#define AC_AUDIO_H

#include <stdint.h>
#include <pthread.h>

#define AUDIO_SAMPLE_RATE 192000
#define AUDIO_CHANNELS 2
#define AUDIO_PERIOD_SIZE 192   // ~1ms at 192kHz — minimal latency
#define AUDIO_MAX_VOICES 32
#define AUDIO_WAVEFORM_SIZE 512
#define AUDIO_MAX_SAMPLE_VOICES 12
#define AUDIO_MAX_SAMPLE_SECS 10

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
    double frequency;       // Hz (smoothed toward target)
    double target_frequency; // Hz (set by update, smoothed per sample)
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
    int active;
    double position;        // fractional sample index
    double speed;           // playback rate (1.0 = original pitch)
    double volume;
    double pan;
    double fade;            // 0-1 envelope
    double fade_target;     // 0 = killing, 1 = playing
    uint64_t id;
} SampleVoice;

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
    float room_mix;         // 0.0 to 1.0 wet mix (smoothed toward target)
    float target_room_mix;  // target wet mix (set by JS, smoothed per sample)

    int glitch_enabled;
    float glitch_hold_l, glitch_hold_r;
    int glitch_counter;
    int glitch_rate;        // samples between holds

    // FX mix: dry/wet blend for entire FX chain (reverb + glitch)
    float fx_mix;           // 0.0 = fully dry, 1.0 = fully wet (smoothed)
    float target_fx_mix;    // target (set by JS, smoothed per sample)

    // System mixer volume (0-100 percent)
    int system_volume;
    int card_index;  // ALSA card number (0 or 1)
    unsigned int actual_rate;  // Negotiated ALSA sample rate (may differ from requested)

    // TTS PCM buffer (resampled to output rate, mono → stereo in mix)
    float *tts_buf;             // ring buffer of mono float samples at output rate
    volatile int tts_write_pos; // producer (tts thread) writes here
    volatile int tts_read_pos;  // consumer (audio thread) reads here
    int tts_buf_size;           // ring buffer size
    float tts_volume;           // 0.0-1.0
    float tts_fade;             // 0.0-1.0 per-sample envelope (prevents click on start/stop)

    // Microphone capture + sample playback
    float *sample_buf;          // recorded sample (mono, at capture rate)
    int sample_len;             // length in samples (0 = no sample)
    int sample_max_len;         // buffer capacity
    unsigned int sample_rate;   // capture sample rate (for speed calc)
    volatile int recording;     // 1 = actively recording
    int sample_write_pos;       // write cursor during recording
    pthread_t capture_thread;
    SampleVoice sample_voices[AUDIO_MAX_SAMPLE_VOICES];
    uint64_t sample_next_id;

    // HDMI audio output (secondary, low-pass filtered clone)
    void *hdmi_pcm;             // snd_pcm_t* for HDMI audio device (NULL if not found)
    unsigned int hdmi_rate;     // negotiated HDMI sample rate
    int hdmi_downsample_n;      // primary_rate / hdmi_rate (round)
    int hdmi_downsample_pos;    // counter for downsampling
    float hdmi_lp_l, hdmi_lp_r; // LP filter state (simple 1-pole IIR)
    int16_t hdmi_period[512*2]; // interleaved S16 staging buffer
    int hdmi_period_pos;        // samples written so far
    int hdmi_period_size;       // target period size in frames
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
void audio_set_fx_mix(ACAudio *audio, float mix);

// Microphone recording
int audio_mic_start(ACAudio *audio);   // start capture thread, returns 0 on success
int audio_mic_stop(ACAudio *audio);    // stop capture, returns sample length

// Sample playback with pitch shifting
uint64_t audio_sample_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan);
void audio_sample_kill(ACAudio *audio, uint64_t id, double fade);
void audio_sample_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan);

// Adjust system volume: delta is -5 to +5 (percentage points), 0 = toggle mute
void audio_volume_adjust(ACAudio *audio, int delta);

// Play a short boot beep (immediately after audio init)
void audio_boot_beep(ACAudio *audio);

// Play a ready melody (when piece is loaded and ready to play)
void audio_ready_melody(ACAudio *audio);

// Play a shutdown sound (before cleanup)
void audio_shutdown_sound(ACAudio *audio);

// Cleanup
void audio_destroy(ACAudio *audio);

// Convert note name to frequency
double audio_note_to_freq(const char *note);

#endif
