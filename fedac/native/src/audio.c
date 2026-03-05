// audio.c — ALSA sound engine for ac-native
// Dedicated audio thread with multi-voice synthesis, envelopes, and effects.

#include "audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <alsa/asoundlib.h>

// Forward declarations
static int read_system_volume(void);

// ============================================================
// Note frequency table (octave 0 base frequencies)
// ============================================================

static const struct { const char *name; double freq; } note_table[] = {
    {"c",  16.3516}, {"cs", 17.3239}, {"db", 17.3239},
    {"d",  18.3540}, {"ds", 19.4454}, {"eb", 19.4454},
    {"e",  20.6017}, {"f",  21.8268}, {"fs", 23.1247},
    {"gb", 23.1247}, {"g",  24.4997}, {"gs", 25.9565},
    {"ab", 25.9565}, {"a",  27.5000}, {"as", 29.1352},
    {"bb", 29.1352}, {"b",  30.8677},
};
#define NOTE_TABLE_SIZE (sizeof(note_table) / sizeof(note_table[0]))

double audio_note_to_freq(const char *note) {
    if (!note || !*note) return 440.0;

    // Try parsing as a number first
    char *end;
    double d = strtod(note, &end);
    if (end != note && *end == '\0') return d;

    // Parse note string: "C4", "4C#", "C#4", "5A", etc.
    int octave = 4;
    char name_buf[8] = {0};
    int ni = 0;
    const char *p = note;

    // Check if starts with digit (octave prefix: "4C#")
    if (*p >= '0' && *p <= '9') {
        octave = *p - '0';
        p++;
    }

    // Read note name
    while (*p && ni < 3) {
        char ch = *p;
        if (ch >= 'A' && ch <= 'G') ch += 32; // lowercase
        if ((ch >= 'a' && ch <= 'g') || ch == '#' || ch == 's' || ch == 'b') {
            // Map 'f' for flat and '#' for sharp
            if (ch == '#') { name_buf[ni++] = 's'; }
            else { name_buf[ni++] = ch; }
            p++;
        } else break;
    }
    name_buf[ni] = '\0';

    // Trailing octave number
    if (*p >= '0' && *p <= '9') {
        octave = *p - '0';
    }

    // Lookup base frequency
    double base = 440.0; // fallback
    for (int i = 0; i < (int)NOTE_TABLE_SIZE; i++) {
        if (strcmp(name_buf, note_table[i].name) == 0) {
            base = note_table[i].freq;
            break;
        }
    }

    return base * pow(2.0, octave);
}

// ============================================================
// Oscillator sample generation
// ============================================================

static inline uint32_t xorshift32(uint32_t *state) {
    uint32_t x = *state;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    *state = x;
    return x;
}

static inline double generate_sample(ACVoice *v) {
    double s;
    switch (v->type) {
    case WAVE_SINE:
        s = sin(2.0 * M_PI * v->phase);
        break;
    case WAVE_SQUARE:
        s = v->phase < 0.5 ? 1.0 : -1.0;
        break;
    case WAVE_TRIANGLE: {
        // Offset phase by 0.25 to start at zero crossing (matches synth.mjs)
        double tp = v->phase + 0.25;
        if (tp >= 1.0) tp -= 1.0;
        s = 4.0 * fabs(tp - 0.5) - 1.0;
        break;
    }
    case WAVE_SAWTOOTH:
        s = 2.0 * v->phase - 1.0;
        break;
    case WAVE_NOISE: {
        // Filtered white noise using biquad LPF
        double white = ((double)xorshift32(&v->noise_seed) / (double)UINT32_MAX) * 2.0 - 1.0;
        double y = v->noise_b0 * white + v->noise_b1 * v->noise_x1 + v->noise_b2 * v->noise_x2
                   - v->noise_a1 * v->noise_y1 - v->noise_a2 * v->noise_y2;
        v->noise_x2 = v->noise_x1;
        v->noise_x1 = white;
        v->noise_y2 = v->noise_y1;
        v->noise_y1 = y;
        s = y;
        break;
    }
    default:
        s = 0.0;
    }

    // Advance phase
    v->phase += v->frequency / (double)AUDIO_SAMPLE_RATE;
    if (v->phase >= 1.0) v->phase -= 1.0;

    return s;
}

static inline double compute_envelope(ACVoice *v) {
    double env = 1.0;

    // Attack ramp
    if (v->attack > 0.0 && v->elapsed < v->attack) {
        env = v->elapsed / v->attack;
    }

    // Decay (near end of duration)
    if (!isinf(v->duration) && v->decay > 0.0) {
        double decay_start = v->duration - v->decay;
        if (decay_start < 0.0) decay_start = 0.0;
        if (v->elapsed > decay_start) {
            double decay_progress = (v->elapsed - decay_start) / v->decay;
            if (decay_progress > 1.0) decay_progress = 1.0;
            env *= (1.0 - decay_progress);
        }
    }

    return env;
}

static inline double compute_fade(ACVoice *v) {
    if (v->state != VOICE_KILLING) return 1.0;
    if (v->fade_duration <= 0.0) return 0.0;
    double progress = v->fade_elapsed / v->fade_duration;
    if (progress >= 1.0) return 0.0;
    return 1.0 - progress;
}

// Setup biquad LPF coefficients for noise voice
static void setup_noise_filter(ACVoice *v) {
    double cutoff = v->frequency;
    if (cutoff < 20.0) cutoff = 20.0;
    if (cutoff > AUDIO_SAMPLE_RATE / 2.0) cutoff = AUDIO_SAMPLE_RATE / 2.0;

    double Q = 1.0;
    double w0 = 2.0 * M_PI * cutoff / AUDIO_SAMPLE_RATE;
    double alpha = sin(w0) / (2.0 * Q);

    double b0 = (1.0 - cos(w0)) / 2.0;
    double b1 = 1.0 - cos(w0);
    double b2 = (1.0 - cos(w0)) / 2.0;
    double a0 = 1.0 + alpha;
    double a1 = -2.0 * cos(w0);
    double a2 = 1.0 - alpha;

    v->noise_b0 = b0 / a0;
    v->noise_b1 = b1 / a0;
    v->noise_b2 = b2 / a0;
    v->noise_a1 = a1 / a0;
    v->noise_a2 = a2 / a0;
    v->noise_x1 = v->noise_x2 = v->noise_y1 = v->noise_y2 = 0.0;
}

// ============================================================
// Audio thread
// ============================================================

#define ROOM_DELAY_SAMPLES (int)(0.12 * AUDIO_SAMPLE_RATE) // 120ms
#define ROOM_SIZE (ROOM_DELAY_SAMPLES * 3)
#define ROOM_FEEDBACK 0.55
#define ROOM_MIX 0.35

// Soft clamp (tanh-like) to prevent harsh digital clipping
static inline double soft_clip(double x) {
    if (x > 1.0) return 1.0 - 1.0 / (1.0 + (x - 1.0) * 3.0);
    if (x < -1.0) return -1.0 + 1.0 / (1.0 - (x + 1.0) * 3.0);
    return x;
}

static void *audio_thread_fn(void *arg) {
    ACAudio *audio = (ACAudio *)arg;
    int16_t buffer[AUDIO_PERIOD_SIZE * AUDIO_CHANNELS];
    const double dt = 1.0 / AUDIO_SAMPLE_RATE;
    double mix_divisor = 1.0; // Smooth auto-mix (matches speaker.mjs)

    while (audio->running) {
        memset(buffer, 0, sizeof(buffer));

        pthread_mutex_lock(&audio->lock);

        for (int i = 0; i < AUDIO_PERIOD_SIZE; i++) {
            double mix_l = 0.0, mix_r = 0.0;
            double voice_sum = 0.0; // Total voice weight for auto-mix

            for (int v = 0; v < AUDIO_MAX_VOICES; v++) {
                ACVoice *voice = &audio->voices[v];
                if (voice->state == VOICE_INACTIVE) continue;

                double s = generate_sample(voice);
                double env = compute_envelope(voice);
                double fade = compute_fade(voice);
                double amp = s * env * fade * voice->volume;

                double left_gain = (1.0 - voice->pan) * 0.5;
                double right_gain = (1.0 + voice->pan) * 0.5;
                mix_l += amp * left_gain;
                mix_r += amp * right_gain;

                // Track voice weight for smooth auto-mix
                if (voice->state == VOICE_KILLING) {
                    voice_sum += voice->volume * (1.0 - voice->fade_elapsed / voice->fade_duration);
                } else {
                    voice_sum += voice->volume;
                }

                voice->elapsed += dt;
                if (voice->state == VOICE_KILLING) {
                    voice->fade_elapsed += dt;
                    if (voice->fade_elapsed >= voice->fade_duration)
                        voice->state = VOICE_INACTIVE;
                } else if (!isinf(voice->duration) && voice->elapsed >= voice->duration) {
                    voice->state = VOICE_INACTIVE;
                }
            }

            // Smooth auto-mix divisor (matches speaker.mjs approach)
            double target = voice_sum > 1.0 ? voice_sum : 1.0;
            if (mix_divisor < target) {
                mix_divisor *= 1.002; // Ramp up gently
                if (mix_divisor > target) mix_divisor = target;
            } else if (mix_divisor > target) {
                mix_divisor *= 0.9997; // Ramp down slowly
                if (mix_divisor < target) mix_divisor = target;
            }

            mix_l /= mix_divisor;
            mix_r /= mix_divisor;

            // Room (reverb) effect
            if (audio->room_enabled && audio->room_buf_l) {
                int tap1 = (audio->room_pos - ROOM_DELAY_SAMPLES + ROOM_SIZE) % ROOM_SIZE;
                int tap2 = (audio->room_pos - ROOM_DELAY_SAMPLES * 2 + ROOM_SIZE) % ROOM_SIZE;
                int tap3 = (audio->room_pos - ROOM_DELAY_SAMPLES * 3 + ROOM_SIZE) % ROOM_SIZE;

                float wet_l = (audio->room_buf_l[tap1] + audio->room_buf_l[tap2] * 0.7f
                               + audio->room_buf_l[tap3] * 0.5f) * ROOM_FEEDBACK;
                float wet_r = (audio->room_buf_r[tap1] + audio->room_buf_r[tap2] * 0.7f
                               + audio->room_buf_r[tap3] * 0.5f) * ROOM_FEEDBACK;

                audio->room_buf_l[audio->room_pos] = (float)mix_l + wet_l * ROOM_FEEDBACK;
                audio->room_buf_r[audio->room_pos] = (float)mix_r + wet_r * ROOM_FEEDBACK;
                audio->room_pos = (audio->room_pos + 1) % ROOM_SIZE;

                float rmix = audio->room_mix;
                mix_l = mix_l * (1.0 - rmix) + wet_l * rmix;
                mix_r = mix_r * (1.0 - rmix) + wet_r * rmix;
            }

            // Glitch (sample-hold + bitcrush)
            if (audio->glitch_enabled) {
                audio->glitch_counter++;
                if (audio->glitch_counter >= audio->glitch_rate) {
                    audio->glitch_counter = 0;
                    // Bitcrush to 6 bits
                    int levels = 64;
                    audio->glitch_hold_l = roundf((float)mix_l * levels) / levels;
                    audio->glitch_hold_r = roundf((float)mix_r * levels) / levels;
                }
                mix_l = audio->glitch_hold_l;
                mix_r = audio->glitch_hold_r;
            }

            // Soft clip and convert to int16
            mix_l = soft_clip(mix_l);
            mix_r = soft_clip(mix_r);

            buffer[i * 2] = (int16_t)(mix_l * 32000);
            buffer[i * 2 + 1] = (int16_t)(mix_r * 32000);

            // Store waveform for visualization
            int wp = audio->waveform_pos;
            audio->waveform_left[wp] = (float)mix_l;
            audio->waveform_right[wp] = (float)mix_r;
            audio->waveform_pos = (wp + 1) % AUDIO_WAVEFORM_SIZE;

            // Track amplitude
            float al = fabsf((float)mix_l);
            float ar = fabsf((float)mix_r);
            audio->amplitude_left = audio->amplitude_left * 0.99f + al * 0.01f;
            audio->amplitude_right = audio->amplitude_right * 0.99f + ar * 0.01f;
        }

        // BPM metronome
        audio->beat_elapsed += (double)AUDIO_PERIOD_SIZE * dt;
        double beat_interval = 60.0 / audio->bpm;
        if (audio->beat_elapsed >= beat_interval) {
            audio->beat_elapsed -= beat_interval;
            audio->beat_triggered = 1;
        }

        pthread_mutex_unlock(&audio->lock);

        audio->total_frames += AUDIO_PERIOD_SIZE;
        audio->time = (double)audio->total_frames / AUDIO_SAMPLE_RATE;

        // Write to ALSA
        snd_pcm_t *pcm = (snd_pcm_t *)audio->pcm;
        int frames = snd_pcm_writei(pcm, buffer, AUDIO_PERIOD_SIZE);
        if (frames < 0) {
            snd_pcm_recover(pcm, frames, 1);
        }
    }

    return NULL;
}

// ============================================================
// Public API
// ============================================================

ACAudio *audio_init(void) {
    ACAudio *audio = calloc(1, sizeof(ACAudio));
    if (!audio) return NULL;

    audio->bpm = 120.0;
    audio->glitch_rate = AUDIO_SAMPLE_RATE / 1600; // 1600 Hz default
    pthread_mutex_init(&audio->lock, NULL);

    // Allocate reverb buffers
    audio->room_size = ROOM_SIZE;
    audio->room_mix = 0.0f;  // Start dry, trackpad Y controls
    audio->room_enabled = 1; // Always on, mix controls wet amount
    audio->room_buf_l = calloc(ROOM_SIZE, sizeof(float));
    audio->room_buf_r = calloc(ROOM_SIZE, sizeof(float));

    // Open ALSA — try multiple devices
    snd_pcm_t *pcm = NULL;
    const char *devices[] = {
        "hw:0,0", "hw:0,1", "hw:0,2", "hw:0,3",
        "plughw:0,0", "plughw:0,1",
        "default", NULL
    };
    int err = -1;
    for (int i = 0; devices[i]; i++) {
        err = snd_pcm_open(&pcm, devices[i], SND_PCM_STREAM_PLAYBACK, 0);
        if (err >= 0) {
            fprintf(stderr, "[audio] Opened ALSA device: %s\n", devices[i]);
            break;
        }
        fprintf(stderr, "[audio] Failed %s: %s\n", devices[i], snd_strerror(err));
    }
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot open any ALSA device\n");
        // Audio is optional — return the struct but with no PCM
        audio->pcm = NULL;
        return audio;
    }

    // Configure ALSA
    snd_pcm_hw_params_t *params;
    snd_pcm_hw_params_alloca(&params);
    snd_pcm_hw_params_any(pcm, params);
    snd_pcm_hw_params_set_access(pcm, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_format(pcm, params, SND_PCM_FORMAT_S16_LE);
    snd_pcm_hw_params_set_channels(pcm, params, AUDIO_CHANNELS);

    unsigned int rate = AUDIO_SAMPLE_RATE;
    snd_pcm_hw_params_set_rate_near(pcm, params, &rate, 0);

    snd_pcm_uframes_t period = AUDIO_PERIOD_SIZE;
    snd_pcm_hw_params_set_period_size_near(pcm, params, &period, 0);

    snd_pcm_uframes_t buffer_size = AUDIO_PERIOD_SIZE * 4;
    snd_pcm_hw_params_set_buffer_size_near(pcm, params, &buffer_size);

    err = snd_pcm_hw_params(pcm, params);
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot configure ALSA: %s\n", snd_strerror(err));
        snd_pcm_close(pcm);
        audio->pcm = NULL;
        return audio;
    }

    snd_pcm_prepare(pcm);
    audio->pcm = pcm;

    fprintf(stderr, "[audio] ALSA: %uHz, period=%lu, buffer=%lu\n",
            rate, (unsigned long)period, (unsigned long)buffer_size);

    // Unmute master and PCM outputs (HDA Intel often starts muted)
    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) >= 0) {
        snd_mixer_attach(mixer, "hw:0");
        snd_mixer_selem_register(mixer, NULL, NULL);
        snd_mixer_load(mixer);

        const char *unmute_names[] = {"Master", "PCM", "Speaker", "Headphone", NULL};
        snd_mixer_elem_t *elem;
        for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
            const char *name = snd_mixer_selem_get_name(elem);
            if (!snd_mixer_selem_is_active(elem)) continue;

            // Log all mixer elements
            fprintf(stderr, "[audio] Mixer: %s", name);
            if (snd_mixer_selem_has_playback_volume(elem)) fprintf(stderr, " [vol]");
            if (snd_mixer_selem_has_playback_switch(elem)) fprintf(stderr, " [sw]");
            fprintf(stderr, "\n");

            // Unmute and set volume for known output controls
            for (int i = 0; unmute_names[i]; i++) {
                if (strcasecmp(name, unmute_names[i]) == 0) {
                    if (snd_mixer_selem_has_playback_switch(elem)) {
                        snd_mixer_selem_set_playback_switch_all(elem, 1);
                        fprintf(stderr, "[audio] Unmuted: %s\n", name);
                    }
                    if (snd_mixer_selem_has_playback_volume(elem)) {
                        long min, max;
                        snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
                        long vol = max; // 100%
                        snd_mixer_selem_set_playback_volume_all(elem, vol);
                        fprintf(stderr, "[audio] Volume %s: %ld/%ld\n", name, vol, max);
                    }
                    break;
                }
            }
        }
        snd_mixer_close(mixer);
    } else {
        fprintf(stderr, "[audio] Cannot open mixer\n");
    }

    // Read initial system volume
    audio->system_volume = read_system_volume();
    fprintf(stderr, "[audio] System volume: %d%%\n", audio->system_volume);

    // Start audio thread
    audio->running = 1;
    pthread_create(&audio->thread, NULL, audio_thread_fn, audio);

    fprintf(stderr, "[audio] Ready\n");
    return audio;
}

uint64_t audio_synth(ACAudio *audio, WaveType type, double freq,
                     double duration, double volume, double attack,
                     double decay, double pan) {
    if (!audio) return 0;

    pthread_mutex_lock(&audio->lock);

    // Find free voice slot
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].state == VOICE_INACTIVE) {
            slot = i;
            break;
        }
    }
    if (slot < 0) {
        // Steal oldest voice
        double oldest = 0;
        slot = 0;
        for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
            if (audio->voices[i].elapsed > oldest) {
                oldest = audio->voices[i].elapsed;
                slot = i;
            }
        }
    }

    ACVoice *v = &audio->voices[slot];
    memset(v, 0, sizeof(ACVoice));
    v->state = VOICE_ACTIVE;
    v->type = type;
    v->phase = 0.0;
    v->frequency = freq;
    v->volume = volume;
    v->pan = pan;
    v->attack = attack > 0 ? attack : 0.005;
    v->decay = decay > 0 ? decay : 0.1;
    v->duration = duration;
    v->id = ++audio->next_id;
    v->started_at = audio->time;

    if (type == WAVE_NOISE) {
        v->noise_seed = (uint32_t)(audio->next_id * 2654435761u);
        setup_noise_filter(v);
    }

    pthread_mutex_unlock(&audio->lock);
    return v->id;
}

void audio_kill(ACAudio *audio, uint64_t id, double fade) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id && audio->voices[i].state == VOICE_ACTIVE) {
            audio->voices[i].state = VOICE_KILLING;
            audio->voices[i].fade_duration = fade > 0 ? fade : 0.025;
            audio->voices[i].fade_elapsed = 0.0;
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

void audio_update(ACAudio *audio, uint64_t id, double freq,
                  double volume, double pan) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_VOICES; i++) {
        if (audio->voices[i].id == id && audio->voices[i].state != VOICE_INACTIVE) {
            if (freq > 0) audio->voices[i].frequency = freq;
            if (volume >= 0) audio->voices[i].volume = volume;
            if (pan > -2.0) audio->voices[i].pan = pan;
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

int audio_beat_check(ACAudio *audio) {
    if (!audio) return 0;
    int triggered = audio->beat_triggered;
    if (triggered) audio->beat_triggered = 0;
    return triggered;
}

void audio_set_bpm(ACAudio *audio, double bpm) {
    if (!audio || bpm <= 0) return;
    pthread_mutex_lock(&audio->lock);
    audio->bpm = bpm;
    pthread_mutex_unlock(&audio->lock);
}

void audio_room_toggle(ACAudio *audio) {
    if (!audio) return;
    audio->room_enabled = !audio->room_enabled;
    fprintf(stderr, "[audio] Room: %s\n", audio->room_enabled ? "ON" : "OFF");
}

void audio_glitch_toggle(ACAudio *audio) {
    if (!audio) return;
    audio->glitch_enabled = !audio->glitch_enabled;
    fprintf(stderr, "[audio] Glitch: %s\n", audio->glitch_enabled ? "ON" : "OFF");
}

void audio_set_room_mix(ACAudio *audio, float mix) {
    if (!audio) return;
    if (mix < 0.0f) mix = 0.0f;
    if (mix > 1.0f) mix = 1.0f;
    audio->room_mix = mix;
}

// Read current Master mixer volume as 0-100 percentage
static int read_system_volume(void) {
    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return -1;
    snd_mixer_attach(mixer, "hw:0");
    snd_mixer_selem_register(mixer, NULL, NULL);
    snd_mixer_load(mixer);

    int pct = -1;
    snd_mixer_elem_t *elem;
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        if (strcasecmp(snd_mixer_selem_get_name(elem), "Master") != 0) continue;
        if (snd_mixer_selem_has_playback_volume(elem)) {
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            if (max > min) pct = (int)((cur - min) * 100 / (max - min));
        }
        break;
    }
    snd_mixer_close(mixer);
    return pct;
}

void audio_volume_adjust(ACAudio *audio, int delta) {
    if (!audio || !audio->pcm) return;

    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return;
    snd_mixer_attach(mixer, "hw:0");
    snd_mixer_selem_register(mixer, NULL, NULL);
    snd_mixer_load(mixer);

    snd_mixer_elem_t *elem;
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        const char *name = snd_mixer_selem_get_name(elem);
        if (strcasecmp(name, "Master") != 0) continue;

        if (delta == 0) {
            // Toggle mute
            if (snd_mixer_selem_has_playback_switch(elem)) {
                int sw = 0;
                snd_mixer_selem_get_playback_switch(elem, 0, &sw);
                snd_mixer_selem_set_playback_switch_all(elem, !sw);
            }
        } else if (snd_mixer_selem_has_playback_volume(elem)) {
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            long step = (max - min) * 5 / 100; // 5% per step
            long newvol = cur + step * delta;
            if (newvol < min) newvol = min;
            if (newvol > max) newvol = max;
            snd_mixer_selem_set_playback_volume_all(elem, newvol);
            // Also ensure unmuted
            if (snd_mixer_selem_has_playback_switch(elem))
                snd_mixer_selem_set_playback_switch_all(elem, 1);
        }
        break;
    }
    snd_mixer_close(mixer);

    // Update cached system volume
    audio->system_volume = read_system_volume();
}

void audio_destroy(ACAudio *audio) {
    if (!audio) return;
    audio->running = 0;
    if (audio->pcm) {
        pthread_join(audio->thread, NULL);
        snd_pcm_close((snd_pcm_t *)audio->pcm);
    }
    free(audio->room_buf_l);
    free(audio->room_buf_r);
    pthread_mutex_destroy(&audio->lock);
    free(audio);
}
