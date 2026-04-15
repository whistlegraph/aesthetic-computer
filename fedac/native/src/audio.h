#ifndef AC_AUDIO_H
#define AC_AUDIO_H

#include <stdint.h>
#include <stdio.h>
#include <pthread.h>
#include "audio-decode.h"

#define AUDIO_SAMPLE_RATE 192000
#define AUDIO_CHANNELS 2
#define AUDIO_PERIOD_SIZE 192   // ~1ms at 192kHz — minimal latency
#define AUDIO_MAX_VOICES 32
#define AUDIO_WAVEFORM_SIZE 512
#define AUDIO_MAX_SAMPLE_VOICES 12
#define AUDIO_MAX_SAMPLE_SECS 10
#define AUDIO_OUTPUT_HISTORY_SECS 12
#define AUDIO_OUTPUT_HISTORY_RATE 48000
#define AUDIO_MAX_DECKS 2

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
    WAVE_NOISE,
    WAVE_WHISTLE,
    WAVE_GUN
} WaveType;

// Gun voice presets. Two synthesis models are available per preset:
//   GUN_MODEL_CLASSIC  — three-layer kick/snare-style synthesis:
//     crack (BPF noise burst), boom (sine with downward pitch sweep),
//     tail (LPF noise with attack-decay). Cheap, predictable, sounds
//     like a "gun sound effect" the way classic sound libraries do.
//   GUN_MODEL_PHYSICAL — digital waveguide barrel resonance + body
//     modes (parallel biquads) + radiation HPF. Physically motivated;
//     better for cavity-dominated sounds (grenade, RPG).
// Per-weapon model choice + parameters live in gun_presets[] in audio.c.
typedef enum {
    GUN_MODEL_CLASSIC = 0,
    GUN_MODEL_PHYSICAL = 1
} GunModel;

typedef enum {
    GUN_PISTOL = 0,     // 9mm — short barrel, bright crack
    GUN_RIFLE,          // AR/AK — medium barrel + supersonic N-wave
    GUN_SHOTGUN,        // 12ga — wide bore, heavy low-end
    GUN_SMG,            // MP5 — short barrel, fast rattle
    GUN_SUPPRESSED,     // silenced pistol — muffled "pfft"
    GUN_LMG,            // M60 auto-fire — retriggers while held
    GUN_SNIPER,         // .50 cal — huge pressure, long tail
    GUN_GRENADE,        // explosion — low cavity, slow release
    GUN_RPG,            // rocket — long burn, delayed boom
    GUN_RELOAD,         // magazine clack — metallic click
    GUN_COCK,           // bolt cock — two-click (primary + delayed)
    GUN_RICOCHET,       // metallic ping — pitch-drops on release
    GUN_PRESET_COUNT
} GunPreset;

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
    // Digital waveguide flute/whistle state (Perry Cook STK Flute model)
    // See audio.c:generate_whistle_sample for algorithm notes. The bore
    // delay line is the primary resonator — its length sets pitch and
    // its feedback loop generates all the harmonics. The jet delay +
    // cubic nonlinearity drives the loop into sustained oscillation.
    double whistle_breath;            // envelope-smoothed breath pressure
    double whistle_vibrato_phase;     // 0..1 vibrato LFO phase
    double whistle_lp1;               // 1-pole loop LPF state
    double whistle_hp_x1, whistle_hp_y1; // 1-pole DC blocker state
    // Bore delay line — up to ~2048 samples at 192kHz covers down to ~94 Hz.
    // Write cursor advances by 1 each tick; reads use fractional delay
    // indexing for smooth pitch.
    float whistle_bore_buf[2048];
    int whistle_bore_w;
    // Jet delay line — shorter, models embouchure travel time (~0.32×bore).
    float whistle_jet_buf[512];
    int whistle_jet_w;
    // === Gun DWG state (see generate_gun_sample) ===
    // Most of these are copied from the preset on note-on; mutable ones
    // (pressure_env, body_y1/y2, bore_lp, rad_prev) evolve each sample.
    // The bore delay buffer is shared with `whistle_bore_buf` since a
    // voice can only be one wave type at a time.
    int    gun_preset;              // GunPreset index (for debug)
    double gun_bore_delay;          // samples (= bore_length_s * sr)
    double gun_bore_loss;           // 1-pole LPF alpha in bore loop
    double gun_bore_lp;             // LPF state
    double gun_breech_reflect;      // closed-breech reflection gain (0..1)
    double gun_pressure;            // excitation peak (weapon power)
    double gun_pressure_env;        // live excitation envelope 0..1
    double gun_env_decay_mult;      // per-sample decay multiplier (exp)
    double gun_noise_gain;          // turbulent gas noise modulation depth
    double gun_radiation_a;         // muzzle HPF 1-zero coefficient (0..1)
    double gun_rad_prev;            // HPF previous input
    // Secondary excitation — fires once more at secondary_trig samples
    // elapsed. Used for supersonic N-wave (rifle/sniper) and for the
    // second click of a cock/reload two-click gesture.
    double gun_secondary_trig;      // sample countdown (<=0 = fired)
    double gun_secondary_amp;       // relative amplitude of 2nd shot
    // Sustained fire (LMG) — retrigger the excitation on cadence while
    // the voice is held (infinite-duration voice, released via kill).
    int    gun_sustain_fire;
    double gun_retrig_timer;        // seconds
    double gun_retrig_period;       // seconds (60 / RPM)
    // Body mode resonators — 3 parallel biquads excited by same pulse.
    // Coefficients precomputed from preset on note-on.
    double gun_body_a1[3], gun_body_a2[3];
    double gun_body_amp[3];
    double gun_body_y1[3], gun_body_y2[3];
    // Pitch sweep (ricochet) — multiplier applied to bore delay (physical)
    // or to boom freq (classic). When voice enters VOICE_KILLING, target
    // flips so the bore stretches → doppler drop during release.
    double gun_pitch_mult;          // current (smoothed)
    double gun_pitch_target;        // target (set on trigger / release)
    double gun_pitch_slew;          // per-sample approach rate
    // === Gun classic-model state (used when gun_model == GUN_MODEL_CLASSIC) ===
    // Layered synthesis: crack (BPF noise burst, decays via gun_pressure_env
    // and gun_env_decay_mult, filtered through body[0] biquad), boom (pitched
    // sine/triangle with exponential pitch sweep + amp decay), tail (LPF noise
    // with linear attack ramp + exponential decay, filtered through body[1]).
    int    gun_model;               // GunModel: 0=classic, 1=physical
    double gun_boom_phase;          // 0..1 oscillator phase
    double gun_boom_freq;           // current Hz (sweeps toward gun_boom_freq_end)
    double gun_boom_freq_start;     // Hz at trigger (for LMG sustain-fire retrigger)
    double gun_boom_freq_end;       // settled Hz (target after pitch sweep)
    double gun_boom_pitch_mult;     // per-sample geometric approach (closer to 0 = faster)
    double gun_boom_env;            // amp envelope (decays each sample)
    double gun_boom_decay_mult;     // per-sample amp decay multiplier
    double gun_tail_env;            // amp envelope (rises during attack, then decays)
    double gun_tail_attack_inc;     // per-sample envelope increment during attack (0 = instant)
    double gun_tail_decay_mult;     // per-sample amp decay multiplier (after attack done)
    double gun_crack_b0;            // BPF input gain (state lives in body[0])
    double gun_tail_b0, gun_tail_b1, gun_tail_b2;  // LPF feed-forward coefs (state in body[1])
    // Click layer — sub-millisecond high-frequency transient. Adds the
    // "tk" snap to the front of the envelope so the crack reads as
    // crisp instead of as a shaped noise burst. Layered before crack.
    double gun_click_env;           // amp envelope (decays each sample)
    double gun_click_decay_mult;    // per-sample multiplier (typ ~exp(-1/(0.5ms*sr)))
    double gun_click_amp;           // mix gain
    double gun_click_prev;          // 1-zero HPF state (white_noise[n-1])
    // Physical-model excitation state — Friedlander blast wave shape.
    // `t_samples` counts up from 0 each trigger; the muzzle pulse follows
    // P(t) = peak·(1−t/t+)·exp(−A·t/t+) for t in [0,t+], then a small
    // negative phase, then silence. This replaces the old white-noise +
    // exp-decay excitation with the actual shape of a blast wave.
    double gun_phys_t;              // samples since last trigger
    double gun_phys_t_plus;         // positive-phase duration (samples)
    double gun_phys_friedlander_a;  // decay exponent (typ. 1.5)
    double gun_phys_neg_amp;        // negative-phase peak (relative)
    double gun_phys_echo_delay;     // ground-reflection delay (samples)
    double gun_phys_echo_amp;       // ground-reflection gain
    double gun_phys_echo_buf[1024]; // small ring for echo tap (~5ms @ 192kHz)
    int    gun_phys_echo_w;
} ACVoice;

typedef struct {
    int active;
    int loop;               // 1 = loop sample, 0 = one-shot
    double position;        // fractional sample index
    double speed;           // playback rate (1.0 = original pitch)
    double volume;
    double pan;
    double fade;            // 0-1 envelope
    double fade_target;     // 0 = killing, 1 = playing
    uint64_t id;
} SampleVoice;

typedef struct {
    volatile int     active;        // deck loaded and ready
    volatile int     playing;       // currently producing audio
    float            volume;        // 0.0–1.0
    ACDeckDecoder   *decoder;       // streaming decoder instance
} ACDeck;

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
    float glitch_mix;       // 0.0 = clean, 1.0 = full sample-hold + bitcrush
    float target_glitch_mix;// target mix from JS, smoothed per sample

    // FX mix: dry/wet blend for entire FX chain (reverb + glitch)
    float fx_mix;           // 0.0 = fully dry, 1.0 = fully wet (smoothed)
    float target_fx_mix;    // target (set by JS, smoothed per sample)

    // System mixer volume (0-100 percent)
    int system_volume;
    int card_index;  // ALSA card number (0 or 1)
    unsigned int actual_rate;    // Negotiated ALSA sample rate (may differ from requested)
    unsigned int actual_period;  // Negotiated ALSA period size in frames

    // TTS PCM buffer (resampled to output rate, mono → stereo in mix)
    float *tts_buf;             // ring buffer of mono float samples at output rate
    volatile int tts_write_pos; // producer (tts thread) writes here
    volatile int tts_read_pos;  // consumer (audio thread) reads here
    int tts_buf_size;           // ring buffer size
    float tts_volume;           // 0.0-1.0
    float tts_fade;             // 0.0-1.0 per-sample envelope (prevents click on start/stop)

    // Microphone capture + sample playback
    float *sample_buf;          // recorded sample (mono, at capture rate) — audio thread reads
    float *sample_buf_back;     // back buffer for double-buffering — JS thread writes here
    volatile int sample_len;    // length in samples (0 = no sample)
    int sample_max_len;         // buffer capacity
    unsigned int sample_rate;   // capture sample rate (for speed calc)
    volatile int recording;     // 1 = buffering mic input to sample_buf
    volatile int sample_write_pos; // write cursor during recording
    volatile int mic_connected; // 1 = capture device currently open
    volatile int mic_hot;       // 1 = hot-mic thread running (device stays open)
    volatile float mic_level;   // raw peak level (0.0-1.0) per chunk
    volatile int mic_last_chunk;// last captured frame count
    char mic_device[64];        // active ALSA capture device string
    char mic_last_error[128];   // last capture error message
    pthread_t capture_thread;
    volatile int capture_thread_running; // 1 while capture thread is alive

    // Continuous capture ring buffer (always written by capture thread)
    float *mic_ring;            // ring buffer, same capacity as sample_buf
    volatile int mic_ring_pos;  // monotonic write position (mod sample_max_len)
    volatile int rec_start_ring_pos; // ring position when recording started

    // Live mic waveform ring buffer (for visualization)
    #define MIC_WAVEFORM_SIZE 128
    float mic_waveform[128];    // circular buffer of recent samples (downsampled)
    volatile int mic_waveform_pos; // write position in ring
    SampleVoice sample_voices[AUDIO_MAX_SAMPLE_VOICES];
    uint64_t sample_next_id;

    // Dedicated global replay voice/buffer so reverse playback does not
    // steal or overwrite the regular sample bank.
    float *replay_buf;
    float *replay_buf_back;
    volatile int replay_len;
    int replay_max_len;
    unsigned int replay_rate;
    SampleVoice replay_voice;

    // Recent rendered-output history for true reverse replay.
    float *output_history_buf;         // mono output ring tapped before room/glitch/TTS
    int output_history_size;           // ring capacity in samples
    unsigned int output_history_rate;  // capture rate exposed to JS
    unsigned int output_history_downsample_n;   // output-rate -> history-rate stride
    unsigned int output_history_downsample_pos; // current stride counter
    uint64_t output_history_write_pos;          // monotonic write position

    // DJ deck audio (persistent across piece switches)
    ACDeck decks[AUDIO_MAX_DECKS];
    float crossfader;           // 0.0 = deck A, 1.0 = deck B
    float deck_master_volume;   // overall deck volume (default 0.8)

    // Parallel headphone PCM (sof-rt5682+max98360a auto-route).
    // Open in addition to the main speaker PCM so both the SSP0 (RT5682
    // headset) and SSP1 (MAX98360A speaker) DAIs receive the same audio
    // stream. The codec's DAPM jack-sense mutes the inactive side, so
    // unplugged → speaker plays, headphones plugged → headphone plays.
    void *headphone_pcm;        // snd_pcm_t* for headphone PCM (NULL if same as main)

    // HDMI audio output (secondary, low-pass filtered clone)
    void *hdmi_pcm;             // snd_pcm_t* for HDMI audio device (NULL if not found)
    unsigned int hdmi_rate;     // negotiated HDMI sample rate
    int hdmi_downsample_n;      // primary_rate / hdmi_rate (round)
    int hdmi_downsample_pos;    // counter for downsampling
    float hdmi_lp_l, hdmi_lp_r; // LP filter state (simple 1-pole IIR)
    int16_t hdmi_period[512*2]; // interleaved S16 staging buffer
    int hdmi_period_pos;        // samples written so far
    int hdmi_period_size;       // target period size in frames

    // Recording tap: if set, called after each mixed period with final int16 PCM
    void (*rec_callback)(const int16_t *pcm, int frames, void *userdata);
    void *rec_userdata;

    // Diagnostic info (exposed to JS via system.hw)
    char audio_device[32];      // ALSA device name that opened successfully
    char audio_status[64];      // human-readable status ("ok", "no card", etc.)
    int audio_init_retries;     // how many devices we tried before success
} ACAudio;

// Initialize ALSA audio engine (returns NULL if no audio device)
ACAudio *audio_init(void);

// Add a new voice, returns voice ID
uint64_t audio_synth(ACAudio *audio, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan);

// Add a new gun voice with a specific preset (applies DWG parameters).
// `volume` scales the output, `pan` places it in stereo. `duration` is
// normally INFINITY for held guns (LMG sustain fire) or finite for
// one-shots; the internal DWG excitation handles the bang envelope.
// `force_model` overrides the preset's default GunModel: pass -1 to use
// the preset's choice, 0 to force CLASSIC (3-layer synthesis), or 1 to
// force PHYSICAL (DWG bore + body modes). Lets the same weapon be A/B-
// compared between models without separate presets.
uint64_t audio_synth_gun(ACAudio *audio, GunPreset preset, double duration,
                         double volume, double attack, double decay,
                         double pan, double pressure_scale, int force_model);

// Override one preset-derived parameter on a freshly-created gun voice.
// Call between audio_synth_gun() and the next audio thread tick to
// retune the next shot. Unknown keys are ignored. Used by the inspector
// drag-to-edit cards. Key names match the gun_presets[] field names.
void audio_gun_voice_set_param(ACAudio *audio, uint64_t id,
                               const char *key, double value);

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
void audio_set_glitch_mix(ACAudio *audio, float mix);
void audio_set_fx_mix(ACAudio *audio, float mix);

// Microphone — hot-mic mode (device stays open, recording toggles buffering)
int audio_mic_open(ACAudio *audio);    // open device + start hot-mic thread
void audio_mic_close(ACAudio *audio);  // stop thread + close device
int audio_mic_start(ACAudio *audio);   // begin buffering (instant, no device open)
int audio_mic_stop(ACAudio *audio);    // stop buffering, returns sample length

// Sample playback with pitch shifting (loop=1 for infinite loop, 0 for one-shot)
uint64_t audio_sample_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan, int loop);
void audio_sample_kill(ACAudio *audio, uint64_t id, double fade);
void audio_sample_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan);
void audio_replay_load_data(ACAudio *audio, const float *data, int len, unsigned int rate);
uint64_t audio_replay_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan, int loop);
void audio_replay_kill(ACAudio *audio, uint64_t id, double fade);
void audio_replay_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan);

// Sample bank: get/load data for per-key sample storage
int audio_sample_get_data(ACAudio *audio, float *out, int max_len);
void audio_sample_load_data(ACAudio *audio, const float *data, int len, unsigned int rate);
int audio_output_get_recent(ACAudio *audio, float *out, int max_len, unsigned int *out_rate);

// Adjust system volume: delta is -5 to +5 (percentage points), 0 = toggle mute
void audio_volume_adjust(ACAudio *audio, int delta);

// Play a short boot beep (immediately after audio init)
void audio_boot_beep(ACAudio *audio);

// Play a ready melody (when piece is loaded and ready to play)
void audio_prewarm(ACAudio *audio);
void audio_ready_melody(ACAudio *audio);

// Play a shutdown sound (before cleanup)
void audio_shutdown_sound(ACAudio *audio);

// Sample persistence (save/load to disk)
int audio_sample_save(ACAudio *audio, const char *path);
int audio_sample_load(ACAudio *audio, const char *path);

// DJ deck control
int audio_deck_load(ACAudio *audio, int deck, const char *path);
void audio_deck_play(ACAudio *audio, int deck);
void audio_deck_pause(ACAudio *audio, int deck);
void audio_deck_seek(ACAudio *audio, int deck, double seconds);
void audio_deck_set_speed(ACAudio *audio, int deck, double speed);
void audio_deck_set_volume(ACAudio *audio, int deck, float vol);
void audio_deck_set_crossfader(ACAudio *audio, float value);
void audio_deck_set_master_volume(ACAudio *audio, float value);

// Cleanup
void audio_destroy(ACAudio *audio);

// Convert note name to frequency
double audio_note_to_freq(const char *note);

#endif
