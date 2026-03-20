// audio.c — ALSA sound engine for ac-native
// Dedicated audio thread with multi-voice synthesis, envelopes, and effects.

#include "audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <dirent.h>
#include <unistd.h>
#include <sched.h>
#include <alsa/asoundlib.h>

// Defined in ac-native.c — writes to USB log and stderr.
extern void ac_log(const char *fmt, ...);

// Forward declarations
static int read_system_volume_card(int card);

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

static inline double generate_sample(ACVoice *v, double sample_rate) {
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

    // Smooth frequency toward target (uses precomputed alpha from caller)
    if (v->target_frequency > 0 && v->frequency != v->target_frequency) {
        v->frequency += (v->target_frequency - v->frequency) * 0.0003; // ~5ms at 192kHz
    }

    // Advance phase
    v->phase += v->frequency / sample_rate;
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
static void setup_noise_filter(ACVoice *v, double sample_rate) {
    double cutoff = v->frequency;
    if (cutoff < 20.0) cutoff = 20.0;
    if (cutoff > sample_rate / 2.0) cutoff = sample_rate / 2.0;

    double Q = 1.0;
    double w0 = 2.0 * M_PI * cutoff / sample_rate;
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
#define ROOM_FEEDBACK 0.3
#define ROOM_MIX 0.35

// Soft clamp (tanh-style) to prevent harsh digital clipping
// Smooth curve: starts compressing gently above 0.6, hard-limits at ~0.95
static inline double soft_clip(double x) {
    if (x > 0.6) {
        double over = x - 0.6;
        return 0.6 + 0.35 * (1.0 - 1.0 / (1.0 + over * 2.5));
    }
    if (x < -0.6) {
        double over = -x - 0.6;
        return -0.6 - 0.35 * (1.0 - 1.0 / (1.0 + over * 2.5));
    }
    return x;
}

// Compressor state (per-channel peak follower)
static double comp_env = 0.0;  // envelope follower level
static unsigned long xrun_count = 0;
static unsigned long short_write_count = 0;

static void *audio_thread_fn(void *arg) {
    ACAudio *audio = (ACAudio *)arg;
    int16_t buffer[AUDIO_PERIOD_SIZE * AUDIO_CHANNELS];
    const double rate = (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE);
    const double dt = 1.0 / rate;
    double mix_divisor = 1.0; // Smooth auto-mix (matches speaker.mjs)
    // Auto-mix smoothing: fast-ish attack, slower release to avoid zipper clicks.
    const double mix_att_coeff = 1.0 - exp(-1.0 / (0.004 * rate)); // ~4ms
    const double mix_rel_coeff = 1.0 - exp(-1.0 / (0.060 * rate)); // ~60ms

    // Set real-time priority to prevent audio glitches from background tasks
    struct sched_param sp = { .sched_priority = 50 };
    if (pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp) != 0)
        fprintf(stderr, "[audio] Warning: couldn't set RT priority\n");

    while (audio->running) {
        memset(buffer, 0, sizeof(buffer));

        pthread_mutex_lock(&audio->lock);

        for (int i = 0; i < AUDIO_PERIOD_SIZE; i++) {
            double mix_l = 0.0, mix_r = 0.0;
            double voice_sum = 0.0; // Total voice weight for auto-mix

            for (int v = 0; v < AUDIO_MAX_VOICES; v++) {
                ACVoice *voice = &audio->voices[v];
                if (voice->state == VOICE_INACTIVE) continue;

                double s = generate_sample(voice, rate);
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

            // Smooth auto-mix divisor — fast attack, slow release
            double target = voice_sum > 1.0 ? voice_sum : 1.0;
            if (mix_divisor < target)
                mix_divisor += (target - mix_divisor) * mix_att_coeff;
            else if (mix_divisor > target)
                mix_divisor += (target - mix_divisor) * mix_rel_coeff;
            if (mix_divisor < 1.0) mix_divisor = 1.0;

            mix_l /= mix_divisor;
            mix_r /= mix_divisor;

            // Mix sample voices (pitch-shifted playback)
            for (int v = 0; v < AUDIO_MAX_SAMPLE_VOICES; v++) {
                SampleVoice *sv = &audio->sample_voices[v];
                if (!sv->active) continue;

                // Fade envelope (5ms attack/release at output rate)
                double fade_speed = 1.0 / (0.005 * rate);
                if (sv->fade < sv->fade_target) {
                    sv->fade += fade_speed;
                    if (sv->fade > sv->fade_target) sv->fade = sv->fade_target;
                } else if (sv->fade > sv->fade_target) {
                    sv->fade -= fade_speed;
                    if (sv->fade <= 0.0) { sv->fade = 0.0; sv->active = 0; continue; }
                }

                // Stereo spread: micro-delay between L/R (Haas effect)
                // Pan controls both amplitude and a small time offset
                // giving mono samples a wide stereo image.
                double delay_samps = sv->pan * 0.0004 * rate; // ~0.4ms max at pan=1
                double pos_l = sv->position - (delay_samps > 0 ? delay_samps : 0);
                double pos_r = sv->position + (delay_samps > 0 ? 0 : delay_samps);
                if (pos_l < 0) pos_l = 0;
                if (pos_r < 0) pos_r = 0;

                // Interpolated read for L channel
                int slen = audio->sample_len;
                if (slen <= 0) { sv->active = 0; continue; }
                int p0l = (int)pos_l;
                if (sv->loop) {
                    p0l = ((p0l % slen) + slen) % slen;
                } else if (p0l >= slen) {
                    sv->active = 0; continue;
                }
                int p1l = p0l + 1; if (p1l >= slen) p1l = sv->loop ? 0 : p0l;
                double fl = pos_l - p0l;
                double samp_l = audio->sample_buf[p0l] * (1.0 - fl)
                              + audio->sample_buf[p1l] * fl;

                // Interpolated read for R channel
                int p0r = (int)pos_r;
                if (sv->loop) {
                    p0r = ((p0r % slen) + slen) % slen;
                } else if (p0r >= slen) {
                    p0r = slen - 1;
                }
                int p1r = p0r + 1; if (p1r >= slen) p1r = sv->loop ? 0 : p0r;
                double fr = pos_r - p0r;
                double samp_r = audio->sample_buf[p0r] * (1.0 - fr)
                              + audio->sample_buf[p1r] * fr;

                double vol = sv->volume * sv->fade;
                double l_gain = sv->pan <= 0 ? 1.0 : 1.0 - sv->pan * 0.6;
                double r_gain = sv->pan >= 0 ? 1.0 : 1.0 + sv->pan * 0.6;
                mix_l += samp_l * vol * l_gain;
                mix_r += samp_r * vol * r_gain;

                sv->position += sv->speed;
                if (sv->position >= audio->sample_len) {
                    if (sv->loop && audio->sample_len > 0) {
                        // Wrap for seamless looping
                        while (sv->position >= audio->sample_len)
                            sv->position -= audio->sample_len;
                    } else {
                        sv->active = 0; // one-shot
                    }
                }
            }

            // Smooth room_mix toward target (~10ms at 192kHz)
            if (audio->room_mix != audio->target_room_mix) {
                audio->room_mix += (audio->target_room_mix - audio->room_mix) * 0.00005f;
            }

            // Smooth fx_mix toward target
            if (audio->fx_mix != audio->target_fx_mix) {
                audio->fx_mix += (audio->target_fx_mix - audio->fx_mix) * 0.00005f;
            }

            // Save dry signal before FX chain
            double dry_l = mix_l, dry_r = mix_r;

            // Room (reverb) effect — tap delays based on actual sample rate
            if (audio->room_enabled && audio->room_buf_l) {
                float rmix = audio->room_mix;
                int rs = audio->room_size;

                // At 0% mix, skip all reverb processing (no buffer feed, no output)
                if (rmix > 0.001f) {
                    int room_delay = (int)(0.12 * rate); // 120ms in samples
                    int tap1 = (audio->room_pos - room_delay + rs) % rs;
                    int tap2 = (audio->room_pos - room_delay * 2 + rs) % rs;
                    int tap3 = (audio->room_pos - room_delay * 3 + rs) % rs;

                    // Weighted sum of taps, normalized
                    float wet_l = (audio->room_buf_l[tap1] * 0.5f
                                 + audio->room_buf_l[tap2] * 0.3f
                                 + audio->room_buf_l[tap3] * 0.2f);
                    float wet_r = (audio->room_buf_r[tap1] * 0.5f
                                 + audio->room_buf_r[tap2] * 0.3f
                                 + audio->room_buf_r[tap3] * 0.2f);

                    // Feed buffer: dry input + attenuated wet feedback
                    float fb_l = (float)mix_l + wet_l * ROOM_FEEDBACK;
                    float fb_r = (float)mix_r + wet_r * ROOM_FEEDBACK;
                    // Damping — ensures reverb tail always decays
                    fb_l *= 0.995f;
                    fb_r *= 0.995f;
                    // Soft-limit feedback to avoid hard-clamp discontinuities under transients.
                    fb_l = tanhf(fb_l * 0.65f) / 0.65f;
                    fb_r = tanhf(fb_r * 0.65f) / 0.65f;
                    audio->room_buf_l[audio->room_pos] = fb_l;
                    audio->room_buf_r[audio->room_pos] = fb_r;

                    // Mix wet into output
                    mix_l = mix_l * (1.0 - rmix) + wet_l * rmix;
                    mix_r = mix_r * (1.0 - rmix) + wet_r * rmix;
                } else {
                    // Mix is ~0%: just clear the current buffer position (drain residue)
                    audio->room_buf_l[audio->room_pos] = 0.0f;
                    audio->room_buf_r[audio->room_pos] = 0.0f;
                }
                audio->room_pos = (audio->room_pos + 1) % rs;
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

            // Blend dry/wet based on FX mix
            {
                float fxm = audio->fx_mix;
                if (fxm < 0.999f) {
                    mix_l = dry_l * (1.0 - fxm) + mix_l * fxm;
                    mix_r = dry_r * (1.0 - fxm) + mix_r * fxm;
                }
            }

            // Mix in TTS audio after FX chain (bypasses reverb/glitch)
            // Fade envelope prevents hard-start/stop clicks
            {
                int tts_has_data = audio->tts_buf &&
                    (audio->tts_read_pos != audio->tts_write_pos);
                // ~3ms ramp at 192kHz (1/576 per sample)
                float ramp = 1.0f / 576.0f;
                if (tts_has_data) {
                    audio->tts_fade += ramp;
                    if (audio->tts_fade > 1.0f) audio->tts_fade = 1.0f;
                    float tts_sample = audio->tts_buf[audio->tts_read_pos]
                        * audio->tts_volume * audio->tts_fade;
                    mix_l += tts_sample;
                    mix_r += tts_sample;
                    audio->tts_read_pos = (audio->tts_read_pos + 1) % audio->tts_buf_size;
                } else {
                    // Fade out: keep adding the last scaled zero-ish sample
                    if (audio->tts_fade > 0.0f) {
                        audio->tts_fade -= ramp;
                        if (audio->tts_fade < 0.0f) audio->tts_fade = 0.0f;
                    }
                }
            }

            // Compressor: peak-following gain reduction (threshold 0.4, ratio ~8:1)
            {
                double peak = fabs(mix_l);
                double pr = fabs(mix_r);
                if (pr > peak) peak = pr;
                // Attack: very fast (0.2ms), Release: medium (40ms)
                double att_coeff = 1.0 - exp(-1.0 / (0.0002 * rate));
                double rel_coeff = 1.0 - exp(-1.0 / (0.04 * rate));
                if (peak > comp_env)
                    comp_env += att_coeff * (peak - comp_env);
                else
                    comp_env += rel_coeff * (peak - comp_env);
                if (comp_env > 0.4) {
                    double gain = 0.4 + (comp_env - 0.4) * 0.125; // ~8:1 ratio above threshold
                    double reduction = gain / comp_env;
                    mix_l *= reduction;
                    mix_r *= reduction;
                }
            }

            // Apply system volume (software gain — hardware mixer may not attenuate)
            {
                double vol = audio->system_volume * 0.01; // 0-100 → 0.0-1.0
                // Use squared curve for more natural volume perception
                vol = vol * vol;
                mix_l *= vol;
                mix_r *= vol;
            }

            // Soft clip and convert to int16
            mix_l = soft_clip(mix_l);
            mix_r = soft_clip(mix_r);

            buffer[i * 2] = (int16_t)(mix_l * 26000);
            buffer[i * 2 + 1] = (int16_t)(mix_r * 26000);

            // HDMI audio: 1-pole low-pass filter + downsample
            // (volume already applied above to mix_l/mix_r)
            if (audio->hdmi_pcm) {
                // LP filter (alpha ≈ 0.18 → ~3kHz cutoff at 48kHz)
                float alpha = 0.18f;
                audio->hdmi_lp_l = alpha * (float)mix_l + (1.0f - alpha) * audio->hdmi_lp_l;
                audio->hdmi_lp_r = alpha * (float)mix_r + (1.0f - alpha) * audio->hdmi_lp_r;
                // Downsample: one HDMI sample per N primary samples
                audio->hdmi_downsample_pos++;
                if (audio->hdmi_downsample_pos >= audio->hdmi_downsample_n) {
                    audio->hdmi_downsample_pos = 0;
                    int pp = audio->hdmi_period_pos;
                    if (pp + 1 < (int)(sizeof(audio->hdmi_period) / sizeof(int16_t)) / 2) {
                        audio->hdmi_period[pp * 2]     = (int16_t)(audio->hdmi_lp_l * 28000);
                        audio->hdmi_period[pp * 2 + 1] = (int16_t)(audio->hdmi_lp_r * 28000);
                        audio->hdmi_period_pos++;
                        if (audio->hdmi_period_pos >= audio->hdmi_period_size) {
                            snd_pcm_t *hpcm = (snd_pcm_t *)audio->hdmi_pcm;
                            int hw = snd_pcm_writei(hpcm, audio->hdmi_period,
                                                     audio->hdmi_period_size);
                            if (hw == -EPIPE || hw == -ESTRPIPE)
                                snd_pcm_recover(hpcm, hw, 1);
                            audio->hdmi_period_pos = 0;
                        }
                    }
                }
            }

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
        audio->time = (double)audio->total_frames / rate;

        // Recording tap: send mixed PCM to recorder (if active)
        if (audio->rec_callback)
            audio->rec_callback(buffer, AUDIO_PERIOD_SIZE, audio->rec_userdata);

        // Write to ALSA (handle short writes to avoid dropped samples/clicks)
        snd_pcm_t *pcm = (snd_pcm_t *)audio->pcm;
        int remaining = AUDIO_PERIOD_SIZE;
        int offset = 0;
        while (remaining > 0) {
            int frames = snd_pcm_writei(pcm, buffer + offset * AUDIO_CHANNELS, remaining);
            if (frames == -EAGAIN) continue;
            if (frames < 0) {
                int rec = snd_pcm_recover(pcm, frames, 1);
                if (frames == -EPIPE || frames == -ESTRPIPE) {
                    xrun_count++;
                    if ((xrun_count % 32) == 1) {
                        fprintf(stderr, "[audio] XRUN recovered x%lu\n", xrun_count);
                    }
                }
                if (rec < 0) {
                    fprintf(stderr, "[audio] ALSA write failed: %s\n", snd_strerror(rec));
                    break;
                }
                continue;
            }
            if (frames == 0) continue;
            if (frames < remaining) {
                short_write_count++;
                if ((short_write_count % 64) == 1) {
                    fprintf(stderr, "[audio] Short write x%lu (%d/%d)\n",
                            short_write_count, frames, remaining);
                }
            }
            remaining -= frames;
            offset += frames;
        }
    }

    return NULL;
}

// ============================================================
// Public API
// ============================================================

// Seed a small default sample so sample mode is playable before first mic recording.
// This roughly matches the "startup" one-shot feel used in web notepat.
static void seed_default_sample(ACAudio *audio) {
    if (!audio || !audio->sample_buf || audio->sample_max_len <= 0) return;

    const unsigned int rate = 48000;
    int len = (int)(0.55 * (double)rate); // 550ms one-shot
    if (len > audio->sample_max_len) len = audio->sample_max_len;

    double p1 = 0.0, p2 = 0.0, p3 = 0.0;
    for (int i = 0; i < len; i++) {
        double t = (double)i / (double)rate;
        double env = exp(-6.0 * t) * (1.0 - exp(-35.0 * t)); // fast attack, exponential decay
        double f0 = 240.0 + 50.0 * sin(t * 6.0);             // slight wobble
        double f1 = f0 * 2.01;
        double f2 = f0 * 3.02;
        p1 += 2.0 * M_PI * f0 / (double)rate;
        p2 += 2.0 * M_PI * f1 / (double)rate;
        p3 += 2.0 * M_PI * f2 / (double)rate;

        double s = 0.78 * sin(p1) + 0.22 * sin(p2 + 0.25) + 0.08 * sin(p3 + 0.15);
        audio->sample_buf[i] = (float)(s * env * 0.85);
    }
    for (int i = len; i < audio->sample_max_len; i++) audio->sample_buf[i] = 0.0f;

    audio->sample_len = len;
    audio->sample_rate = rate;
    ac_log("[sample] seeded default startup sample (%d frames @ %u Hz)\n",
           audio->sample_len, audio->sample_rate);
}

ACAudio *audio_init(void) {
    ACAudio *audio = calloc(1, sizeof(ACAudio));
    if (!audio) return NULL;

    audio->bpm = 120.0;
    audio->actual_rate = AUDIO_SAMPLE_RATE; // default, overwritten after ALSA negotiation
    audio->glitch_rate = AUDIO_SAMPLE_RATE / 1600;
    pthread_mutex_init(&audio->lock, NULL);

    // Allocate reverb buffers
    audio->room_size = ROOM_SIZE;
    audio->room_mix = 0.0f;  // Start dry, trackpad Y controls
    audio->room_enabled = 1; // Always on, mix controls wet amount
    audio->fx_mix = 1.0f;    // FX chain fully wet by default
    audio->target_fx_mix = 1.0f;
    audio->room_buf_l = calloc(ROOM_SIZE, sizeof(float));
    audio->room_buf_r = calloc(ROOM_SIZE, sizeof(float));

    // Sample buffer (10 seconds at max 48kHz capture rate)
    audio->sample_max_len = 48000 * AUDIO_MAX_SAMPLE_SECS;
    audio->sample_buf = calloc(audio->sample_max_len, sizeof(float));
    audio->sample_len = 0;
    audio->sample_rate = 48000; // default, overwritten by actual capture rate
    audio->sample_next_id = 1;
    audio->mic_connected = 0;
    audio->mic_hot = 0;
    audio->mic_level = 0.0f;
    audio->mic_last_chunk = 0;
    audio->capture_thread_running = 0;
    memset(audio->mic_waveform, 0, sizeof(audio->mic_waveform));
    audio->mic_waveform_pos = 0;
    audio->mic_ring = calloc(audio->sample_max_len, sizeof(float));
    audio->mic_ring_pos = 0;
    audio->rec_start_ring_pos = 0;
    snprintf(audio->mic_device, sizeof(audio->mic_device), "none");
    audio->mic_last_error[0] = 0;
    seed_default_sample(audio);

    // TTS PCM ring buffer (5 seconds at output rate)
    audio->tts_buf_size = AUDIO_SAMPLE_RATE * 5;
    audio->tts_buf = calloc(audio->tts_buf_size, sizeof(float));
    audio->tts_read_pos = 0;
    audio->tts_write_pos = 0;
    audio->tts_volume = 2.5f;  // Boost flite output (naturally quiet)

    snprintf(audio->audio_device, sizeof(audio->audio_device), "none");
    snprintf(audio->audio_status, sizeof(audio->audio_status), "initializing");
    audio->audio_init_retries = 0;

    // Wait for sound card to appear
    fprintf(stderr, "[audio] Waiting for sound card...\n");
    int card_found = 0;
    for (int w = 0; w < 400; w++) { // up to 8 seconds
        if (access("/dev/snd/pcmC0D0p", F_OK) == 0 ||
            access("/dev/snd/pcmC1D0p", F_OK) == 0 ||
            access("/dev/snd/pcmC2D0p", F_OK) == 0) { card_found = 1; break; }
        usleep(20000);
    }
    if (!card_found) {
        // Distinguish: HDA controller present (codec probe failed) vs no hardware at all
        if (access("/dev/snd/controlC0", F_OK) == 0) {
            fprintf(stderr, "[audio] WARNING: HDA controller present but codec not probed after 8s\n");
            snprintf(audio->audio_status, sizeof(audio->audio_status), "HDA ctrl ok, codec not probed");
        } else {
            fprintf(stderr, "[audio] WARNING: no sound card after 8s wait\n");
            snprintf(audio->audio_status, sizeof(audio->audio_status), "no card (8s timeout)");
        }
    }

    // Dump sound card info for diagnostics (write to USB log if mounted)
    FILE *alog = fopen("/mnt/ac-audio.log", "w");
    if (!alog) alog = stderr;  // fallback to stderr
    {
        FILE *cards = fopen("/proc/asound/cards", "r");
        if (cards) {
            char line[256];
            fprintf(alog, "[audio] === /proc/asound/cards ===\n");
            while (fgets(line, sizeof(line), cards))
                fprintf(alog, "[audio] %s", line);
            fclose(cards);
        } else {
            fprintf(alog, "[audio] WARNING: /proc/asound/cards not found!\n");
        }
        // Also check /dev/snd/
        DIR *snddir = opendir("/dev/snd");
        if (snddir) {
            struct dirent *ent;
            fprintf(alog, "[audio] /dev/snd/:");
            while ((ent = readdir(snddir))) {
                if (ent->d_name[0] != '.') fprintf(alog, " %s", ent->d_name);
            }
            fprintf(alog, "\n");
            closedir(snddir);
        } else {
            fprintf(alog, "[audio] WARNING: /dev/snd/ not found!\n");
        }
    }

    // Open ALSA — try multiple cards and devices, with retries for race conditions.
    // On fast NVMe boots the HDA codec may not be fully probed when we first try.
    snd_pcm_t *pcm = NULL;
    const char *devices[] = {
        "hw:0,0", "hw:1,0", "hw:0,1", "hw:1,1",
        "hw:0,2", "hw:0,3", "hw:1,2", "hw:1,3",
        "plughw:0,0", "plughw:1,0",
        "default", NULL
    };
    int err = -1;
    int card_idx = 0;
    for (int attempt = 0; attempt < 5 && err < 0; attempt++) {
        if (attempt > 0) {
            fprintf(alog, "[audio] Retry %d/4 — waiting 2s for codec probe...\n", attempt);
            fprintf(stderr, "[audio] Retry %d/4 — waiting 2s for codec probe...\n", attempt);
            usleep(2000000); // 2 seconds between retries
        }
        for (int i = 0; devices[i]; i++) {
            audio->audio_init_retries++;
            err = snd_pcm_open(&pcm, devices[i], SND_PCM_STREAM_PLAYBACK, 0);
            if (err >= 0) {
                fprintf(alog, "[audio] Opened ALSA device: %s (attempt %d)\n", devices[i], attempt);
                fprintf(stderr, "[audio] Opened ALSA device: %s (attempt %d)\n", devices[i], attempt);
                snprintf(audio->audio_device, sizeof(audio->audio_device), "%s", devices[i]);
                if (sscanf(devices[i], "hw:%d", &card_idx) != 1 &&
                    sscanf(devices[i], "plughw:%d", &card_idx) != 1)
                    card_idx = 0;
                break;
            }
            if (attempt == 0)
                fprintf(alog, "[audio] Failed %s: %s\n", devices[i], snd_strerror(err));
        }
    }
    audio->card_index = card_idx;
    if (alog != stderr) { fflush(alog); fclose(alog); }
    if (err < 0) {
        fprintf(stderr, "[audio] Cannot open any ALSA device after 5 attempts\n");
        snprintf(audio->audio_status, sizeof(audio->audio_status), "no ALSA device found");
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

    snd_pcm_uframes_t buffer_size = AUDIO_PERIOD_SIZE * 3;  // 3 periods (~3ms total at 192kHz)
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
    audio->actual_rate = rate;

    // Update glitch rate for actual sample rate
    audio->glitch_rate = rate / 1600;

    // Reallocate room buffers for actual rate
    int actual_room_size = (int)(0.12 * rate) * 3;
    if (actual_room_size != audio->room_size) {
        free(audio->room_buf_l); free(audio->room_buf_r);
        audio->room_size = actual_room_size;
        audio->room_buf_l = calloc(actual_room_size, sizeof(float));
        audio->room_buf_r = calloc(actual_room_size, sizeof(float));
        audio->room_pos = 0;
    }

    fprintf(stderr, "[audio] ALSA: requested %dHz, got %uHz, period=%lu, buffer=%lu (%.1fms latency)\n",
            AUDIO_SAMPLE_RATE, rate, (unsigned long)period, (unsigned long)buffer_size,
            (double)period / rate * 1000.0);
    snprintf(audio->audio_status, sizeof(audio->audio_status),
             "ok %uHz %lufrm", rate, (unsigned long)period);
    if (rate != AUDIO_SAMPLE_RATE)
        fprintf(stderr, "[audio] WARNING: got %uHz instead of %dHz\n", rate, AUDIO_SAMPLE_RATE);

    // Unmute ALL outputs (HDA Intel codecs have many controls that can mute)
    char mixer_card[16];
    snprintf(mixer_card, sizeof(mixer_card), "hw:%d", card_idx);
    fprintf(stderr, "[audio] Using mixer: %s\n", mixer_card);

    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) >= 0) {
        snd_mixer_attach(mixer, mixer_card);
        snd_mixer_selem_register(mixer, NULL, NULL);
        snd_mixer_load(mixer);

        snd_mixer_elem_t *elem;
        for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
            const char *name = snd_mixer_selem_get_name(elem);
            if (!snd_mixer_selem_is_active(elem)) continue;

            // Log all mixer elements
            fprintf(stderr, "[audio] Mixer: %s", name);
            if (snd_mixer_selem_has_playback_volume(elem)) fprintf(stderr, " [vol]");
            if (snd_mixer_selem_has_playback_switch(elem)) fprintf(stderr, " [sw]");
            if (snd_mixer_selem_has_capture_switch(elem)) fprintf(stderr, " [cap-sw]");
            if (snd_mixer_selem_has_capture_volume(elem)) fprintf(stderr, " [cap-vol]");
            fprintf(stderr, "\n");

            // Unmute every playback switch we find
            if (snd_mixer_selem_has_playback_switch(elem)) {
                snd_mixer_selem_set_playback_switch_all(elem, 1);
                fprintf(stderr, "[audio] Unmuted: %s\n", name);
            }

            // Set volume to max for output controls
            if (snd_mixer_selem_has_playback_volume(elem)) {
                long min, max;
                snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
                snd_mixer_selem_set_playback_volume_all(elem, max);
                fprintf(stderr, "[audio] Volume %s: %ld/%ld\n", name, max, max);
            }

            // NOTE: Do NOT touch capture mixer controls here — on the 11e Yoga
            // Gen 5, enabling capture switches at boot (before any capture PCM
            // is open) puts the HDA codec into a bad state that causes EIO when
            // the capture stream is later opened with period/buffer params.
        }
        snd_mixer_close(mixer);
    } else {
        fprintf(stderr, "[audio] Cannot open mixer\n");
    }

    // Read initial system volume
    audio->system_volume = read_system_volume_card(card_idx);
    fprintf(stderr, "[audio] System volume: %d%%\n", audio->system_volume);

    // HDMI audio disabled — opening HDMI PCM streams on the same HDA controller
    // can exhaust controller streams and cause EIO on capture.
    audio->hdmi_pcm = NULL;
    fprintf(stderr, "[audio] HDMI audio: disabled\n");

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
    v->target_frequency = freq;
    v->volume = volume;
    v->pan = pan;
    v->attack = attack > 0 ? attack : 0.005;
    v->decay = decay > 0 ? decay : 0.1;
    v->duration = duration;
    v->id = ++audio->next_id;
    v->started_at = audio->time;

    if (type == WAVE_NOISE) {
        v->noise_seed = (uint32_t)(audio->next_id * 2654435761u);
        setup_noise_filter(v, (double)(audio->actual_rate ? audio->actual_rate : AUDIO_SAMPLE_RATE));
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
            if (freq > 0) audio->voices[i].target_frequency = freq;
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
    audio->target_room_mix = mix;
}

void audio_set_fx_mix(ACAudio *audio, float mix) {
    if (!audio) return;
    if (mix < 0.0f) mix = 0.0f;
    if (mix > 1.0f) mix = 1.0f;
    audio->target_fx_mix = mix;
}

// --- Hot-mic capture thread ---
// Device opens once (on wave-enter), stays running. Always reads to keep
// ALSA happy and the level meter live. Only writes to sample_buf when
// recording flag is set. Instant recording with zero device-open latency.
//
// IMPORTANT: HDMI audio must be DISABLED in audio_init and playback buffer
// must be 3 periods (not 6) — otherwise the HDA controller runs out of
// streams and capture gets EIO.
static void *capture_thread_func(void *arg) {
    ACAudio *audio = (ACAudio *)arg;
    snd_pcm_t *cap = NULL;

    const char *devices[] = {"hw:0,0", "hw:1,0", "hw:0,6", "hw:0,7",
                             "plughw:0,0", "plughw:1,0", "default", NULL};
    for (int i = 0; devices[i]; i++) {
        if (snd_pcm_open(&cap, devices[i], SND_PCM_STREAM_CAPTURE, 0) == 0) {
            snprintf(audio->mic_device, sizeof(audio->mic_device), "%s", devices[i]);
            ac_log("[mic] opened capture device: %s\n", devices[i]);
            break;
        }
        cap = NULL;
    }
    if (!cap) {
        snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                 "no capture device found");
        ac_log("[mic] no capture device found\n");
        audio->mic_hot = 0;
        return NULL;
    }

    // Configure — NO period/buffer setting (ALSA defaults work on this HDA)
    snd_pcm_hw_params_t *hw;
    snd_pcm_hw_params_alloca(&hw);
    snd_pcm_hw_params_any(cap, hw);
    snd_pcm_hw_params_set_access(cap, hw, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_format(cap, hw, SND_PCM_FORMAT_S16_LE);

    unsigned int channels = 1;
    if (snd_pcm_hw_params_set_channels(cap, hw, 1) < 0) {
        channels = 2;
        snd_pcm_hw_params_set_channels(cap, hw, 2);
    }

    unsigned int rate = 48000;
    snd_pcm_hw_params_set_rate_near(cap, hw, &rate, NULL);

    if (snd_pcm_hw_params(cap, hw) < 0) {
        snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                 "failed to configure capture");
        ac_log("[mic] failed to configure capture\n");
        snd_pcm_close(cap);
        audio->mic_hot = 0;
        return NULL;
    }

    // Enable capture mixer (safe here — after PCM open, in capture thread)
    {
        int cnum = 0;
        const char *d = audio->mic_device;
        while (*d && (*d < '0' || *d > '9')) d++;
        if (*d) cnum = atoi(d);
        char ccard[16];
        snprintf(ccard, sizeof(ccard), "hw:%d", cnum);
        snd_mixer_t *cmix = NULL;
        if (snd_mixer_open(&cmix, 0) >= 0) {
            snd_mixer_attach(cmix, ccard);
            snd_mixer_selem_register(cmix, NULL, NULL);
            snd_mixer_load(cmix);
            snd_mixer_elem_t *elem;
            for (elem = snd_mixer_first_elem(cmix); elem; elem = snd_mixer_elem_next(elem)) {
                if (!snd_mixer_selem_is_active(elem)) continue;
                if (snd_mixer_selem_has_capture_switch(elem)) {
                    snd_mixer_selem_set_capture_switch_all(elem, 1);
                    ac_log("[mic] capture switch ON: %s\n", snd_mixer_selem_get_name(elem));
                }
                if (snd_mixer_selem_has_capture_volume(elem)) {
                    long cmin, cmax;
                    snd_mixer_selem_get_capture_volume_range(elem, &cmin, &cmax);
                    long cset = cmin + ((cmax - cmin) * 9) / 10;
                    snd_mixer_selem_set_capture_volume_all(elem, cset);
                    ac_log("[mic] capture volume %s: %ld/%ld\n",
                           snd_mixer_selem_get_name(elem), cset, cmax);
                }
            }
            snd_mixer_close(cmix);
        }
    }

    audio->sample_rate = rate;
    audio->mic_connected = 1;
    ac_log("[mic] hot-mic running at %u Hz, %u ch\n", rate, channels);

    int16_t buf[1024 * 2];
    while (audio->mic_hot) {
        int n = snd_pcm_readi(cap, buf, 512);
        if (n < 0) {
            n = snd_pcm_recover(cap, n, 0);
            if (n < 0) {
                snprintf(audio->mic_last_error, sizeof(audio->mic_last_error),
                         "capture read failed: %s", snd_strerror(n));
                ac_log("[mic] capture read failed: %s\n", snd_strerror(n));
                break;
            }
            continue;
        }

        float peak = 0.0f;
        for (int s = 0; s < n; s++) {
            float sample;
            if (channels == 1) {
                sample = buf[s] / 32768.0f;
            } else {
                sample = (buf[s * 2] + buf[s * 2 + 1]) / 65536.0f;
            }
            float abs_s = fabsf(sample);
            if (abs_s > peak) peak = abs_s;

            // Always write to ring buffer
            audio->mic_ring[audio->mic_ring_pos % audio->sample_max_len] = sample;
            audio->mic_ring_pos++;

            // Direct-write when recording
            if (audio->recording && audio->sample_write_pos < audio->sample_max_len) {
                audio->sample_buf[audio->sample_write_pos++] = sample;
            }
        }
        audio->mic_level = peak;

        if (audio->recording && audio->sample_write_pos >= audio->sample_max_len) {
            audio->sample_len = audio->sample_write_pos;
            audio->recording = 0;
            ac_log("[mic] recording buffer full (%d samples)\n", audio->sample_len);
        }
    }

    ac_log("[mic] hot-mic thread exiting, device=%s\n", audio->mic_device);
    snd_pcm_close(cap);
    audio->mic_connected = 0;
    audio->recording = 0;
    return NULL;
}

int audio_mic_open(ACAudio *audio) {
    if (!audio || audio->mic_hot || audio->capture_thread_running) return -1;
    audio->mic_hot = 1;
    audio->capture_thread_running = 1;
    audio->mic_last_error[0] = 0;
    ac_log("[mic] opening hot-mic\n");
    if (pthread_create(&audio->capture_thread, NULL, capture_thread_func, audio) != 0) {
        audio->mic_hot = 0;
        audio->capture_thread_running = 0;
        ac_log("[mic] failed to create capture thread\n");
        return -1;
    }
    return 0;
}

void audio_mic_close(ACAudio *audio) {
    if (!audio) return;
    audio->recording = 0;
    audio->mic_hot = 0;
    if (audio->capture_thread_running) {
        pthread_join(audio->capture_thread, NULL);
        audio->capture_thread_running = 0;
    }
    ac_log("[mic] hot-mic closed\n");
}

int audio_mic_start(ACAudio *audio) {
    if (!audio || audio->recording) return -1;
    if (!audio->mic_hot) {
        int rc = audio_mic_open(audio);
        if (rc != 0) return rc;
    }
    // Kill any playing sample voices
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++)
        audio->sample_voices[i].active = 0;
    audio->rec_start_ring_pos = audio->mic_ring_pos;
    audio->sample_len = 0;
    audio->sample_write_pos = 0;
    __sync_synchronize();
    audio->recording = 1;
    ac_log("[mic] recording started (instant), ring_pos=%d\n", audio->rec_start_ring_pos);
    return 0;
}

int audio_mic_stop(ACAudio *audio) {
    if (!audio) return 0;
    audio->recording = 0;
    __sync_synchronize();

    int direct_len = audio->sample_write_pos;
    if (direct_len > 0) {
        audio->sample_len = direct_len;
        ac_log("[mic] recording stopped (direct), sample_len=%d sample_rate=%u\n",
               audio->sample_len, audio->sample_rate);
    } else {
        // Fallback: extract from ring buffer
        int start = audio->rec_start_ring_pos;
        int end = audio->mic_ring_pos;
        int len = end - start;
        if (len < 0) len = 0;
        if (len > audio->sample_max_len) len = audio->sample_max_len;
        for (int i = 0; i < len; i++) {
            audio->sample_buf[i] = audio->mic_ring[(start + i) % audio->sample_max_len];
        }
        audio->sample_len = len;
        ac_log("[mic] recording stopped (ring), sample_len=%d ring_span=%d sample_rate=%u\n",
               audio->sample_len, end - start, audio->sample_rate);
    }
    return audio->sample_len;
}

// --- Sample playback ---
uint64_t audio_sample_play(ACAudio *audio, double freq, double base_freq,
                           double volume, double pan, int loop) {
    if (!audio || audio->sample_len == 0) return 0;
    pthread_mutex_lock(&audio->lock);

    // Find free slot (or steal oldest)
    int slot = -1;
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        if (!audio->sample_voices[i].active) { slot = i; break; }
    }
    if (slot < 0) slot = 0; // steal first

    SampleVoice *sv = &audio->sample_voices[slot];
    sv->active = 1;
    sv->loop = loop;
    sv->position = 0.0;
    // Speed: pitch ratio * rate conversion (capture rate → output rate)
    sv->speed = (freq / base_freq) * ((double)audio->sample_rate / (double)audio->actual_rate);
    sv->volume = volume;
    sv->pan = pan;
    sv->fade = 0.0;
    sv->fade_target = 1.0;
    sv->id = audio->sample_next_id++;

    pthread_mutex_unlock(&audio->lock);
    fprintf(stderr, "[sample] play freq=%.1f base=%.1f speed=%.4f id=%lu\n",
            freq, base_freq, sv->speed, (unsigned long)sv->id);
    return sv->id;
}

void audio_sample_kill(ACAudio *audio, uint64_t id, double fade) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        if (audio->sample_voices[i].active && audio->sample_voices[i].id == id) {
            if (fade <= 0.001) {
                audio->sample_voices[i].active = 0;
            } else {
                audio->sample_voices[i].fade_target = 0.0;
            }
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

void audio_sample_update(ACAudio *audio, uint64_t id, double freq,
                         double base_freq, double volume, double pan) {
    if (!audio) return;
    pthread_mutex_lock(&audio->lock);
    for (int i = 0; i < AUDIO_MAX_SAMPLE_VOICES; i++) {
        SampleVoice *sv = &audio->sample_voices[i];
        if (sv->active && sv->id == id) {
            if (freq > 0 && base_freq > 0)
                sv->speed = (freq / base_freq) * ((double)audio->sample_rate / (double)audio->actual_rate);
            if (volume >= 0) sv->volume = volume;
            if (pan > -2) sv->pan = pan;
            break;
        }
    }
    pthread_mutex_unlock(&audio->lock);
}

// Read current Master mixer volume as 0-100 percentage
static int read_system_volume_card(int card) {
    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return -1;
    char card_name[16];
    snprintf(card_name, sizeof(card_name), "hw:%d", card);
    snd_mixer_attach(mixer, card_name);
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

static int muted = 0;
static long pre_mute_volume = -1;

// Unmute all playback switches in the mixer
static void unmute_all_switches(snd_mixer_t *mixer) {
    snd_mixer_elem_t *elem;
    for (elem = snd_mixer_first_elem(mixer); elem; elem = snd_mixer_elem_next(elem)) {
        if (!snd_mixer_selem_is_active(elem)) continue;
        if (snd_mixer_selem_has_playback_switch(elem))
            snd_mixer_selem_set_playback_switch_all(elem, 1);
    }
}

void audio_volume_adjust(ACAudio *audio, int delta) {
    if (!audio || !audio->pcm) return;

    char card_name[16];
    snprintf(card_name, sizeof(card_name), "hw:%d", audio->card_index);

    snd_mixer_t *mixer = NULL;
    if (snd_mixer_open(&mixer, 0) < 0) return;
    snd_mixer_attach(mixer, card_name);
    snd_mixer_selem_register(mixer, NULL, NULL);
    snd_mixer_load(mixer);

    // Adjust ALL playback volume elements — on Realtek ALC codecs,
    // Master controls digital gain, Speaker/Headphone control the amplifier.
    // Both need to be set for audible volume change.
    const char *try_names[] = {"Master", "Speaker", "Headphone", "PCM", NULL};
    int adjusted = 0;
    for (int n = 0; try_names[n]; n++) {
        snd_mixer_elem_t *elem = NULL;
        for (snd_mixer_elem_t *e = snd_mixer_first_elem(mixer); e; e = snd_mixer_elem_next(e)) {
            if (!snd_mixer_selem_is_active(e)) continue;
            const char *name = snd_mixer_selem_get_name(e);
            if (strcasecmp(name, try_names[n]) == 0 && snd_mixer_selem_has_playback_volume(e))
                { elem = e; break; }
        }
        if (!elem) continue;

        if (delta == 0) {
            // Toggle mute
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            if (!muted) {
                pre_mute_volume = cur;
                snd_mixer_selem_set_playback_volume_all(elem, min);
            } else {
                long restore = (pre_mute_volume > min) ? pre_mute_volume : max * 80 / 100;
                snd_mixer_selem_set_playback_volume_all(elem, restore);
            }
            ac_log("[audio] volume: mute toggle '%s' on %s\n", try_names[n], card_name);
        } else {
            long min, max, cur;
            snd_mixer_selem_get_playback_volume_range(elem, &min, &max);
            snd_mixer_selem_get_playback_volume(elem, 0, &cur);
            long step = (max - min) * 5 / 100;
            if (step < 1) step = 1;
            long newvol = cur + step * delta;
            if (newvol < min) newvol = min;
            if (newvol > max) newvol = max;
            snd_mixer_selem_set_playback_volume_all(elem, newvol);
            ac_log("[audio] volume: '%s' %ld→%ld (range %ld-%ld)\n", try_names[n], cur, newvol, min, max);
        }
        adjusted++;
    }
    if (delta == 0) { muted = !muted; }
    if (adjusted) {
        unmute_all_switches(mixer);
        if (delta != 0) muted = 0;
    } else {
        // No elements found — log what's available
        ac_log("[audio] volume: no playback elements on %s. Available:\n", card_name);
        for (snd_mixer_elem_t *e = snd_mixer_first_elem(mixer); e; e = snd_mixer_elem_next(e))
            ac_log("[audio]   %s%s\n", snd_mixer_selem_get_name(e),
                    snd_mixer_selem_has_playback_volume(e) ? " [vol]" : "");
    }
    snd_mixer_close(mixer);

    // Update cached system volume
    audio->system_volume = muted ? 0 : read_system_volume_card(audio->card_index);
}

void audio_boot_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Two-tone "doo-dah" — distinct from old single ping (OTA test marker)
    audio_synth(audio, WAVE_SINE, 660.0, 0.12, 0.8, 0.002, 0.08, -0.15);  // E5
    usleep(80000);
    audio_synth(audio, WAVE_SINE, 990.0, 0.15, 0.9, 0.002, 0.10,  0.15);  // B5
}

// Prewarm: play a near-silent note so ALSA buffers are filled and ready
void audio_prewarm(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_SINE, 440.0, 0.05, 0.001, 0.001, 0.04, 0.0);
}

void audio_ready_melody(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Quick ascending 3-note toot: C5 → E5 → G5 (major triad) at full volume
    audio_synth(audio, WAVE_TRIANGLE, 523.25, 0.15, 0.7, 0.003, 0.10, -0.2);  // C5
    usleep(60000); // 60ms gap
    audio_synth(audio, WAVE_TRIANGLE, 659.25, 0.15, 0.7, 0.003, 0.10,  0.0);  // E5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 783.99, 0.20, 0.8, 0.003, 0.14,  0.2);  // G5
}

void audio_shutdown_sound(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    // Descending 3-note chime: G5 → E5 → C5 at full volume
    audio_synth(audio, WAVE_TRIANGLE, 783.99, 0.15, 0.7, 0.003, 0.10,  0.2);  // G5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 659.25, 0.15, 0.7, 0.003, 0.10,  0.0);  // E5
    usleep(60000);
    audio_synth(audio, WAVE_TRIANGLE, 523.25, 0.20, 0.8, 0.003, 0.14, -0.2);  // C5
    // Wait for notes to finish playing before shutdown
    usleep(250000);
}

// Save sample buffer to disk as raw floats with a small header
// Format: [uint32_t sample_rate] [uint32_t sample_len] [float * sample_len]
int audio_sample_save(ACAudio *audio, const char *path) {
    if (!audio || !audio->sample_buf || audio->sample_len <= 0) return -1;
    FILE *f = fopen(path, "wb");
    if (!f) return -1;
    uint32_t rate = (uint32_t)audio->sample_rate;
    uint32_t len = (uint32_t)audio->sample_len;
    fwrite(&rate, sizeof(rate), 1, f);
    fwrite(&len, sizeof(len), 1, f);
    fwrite(audio->sample_buf, sizeof(float), len, f);
    fclose(f);
    sync();
    return (int)len;
}

// Load sample buffer from disk
int audio_sample_load(ACAudio *audio, const char *path) {
    if (!audio || !audio->sample_buf) return -1;
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    uint32_t rate, len;
    if (fread(&rate, sizeof(rate), 1, f) != 1 ||
        fread(&len, sizeof(len), 1, f) != 1) {
        fclose(f);
        return -1;
    }
    if (len > (uint32_t)audio->sample_max_len) len = (uint32_t)audio->sample_max_len;
    if (fread(audio->sample_buf, sizeof(float), len, f) != len) {
        fclose(f);
        return -1;
    }
    fclose(f);
    audio->sample_len = (int)len;
    audio->sample_rate = (int)rate;
    return (int)len;
}

void audio_destroy(ACAudio *audio) {
    if (!audio) return;
    audio->running = 0;
    audio_mic_close(audio);
    if (audio->pcm) {
        pthread_join(audio->thread, NULL);
        snd_pcm_close((snd_pcm_t *)audio->pcm);
    }
    if (audio->hdmi_pcm) snd_pcm_close((snd_pcm_t *)audio->hdmi_pcm);
    free(audio->room_buf_l);
    free(audio->room_buf_r);
    free(audio->sample_buf);
    free(audio->tts_buf);
    pthread_mutex_destroy(&audio->lock);
    free(audio);
}
