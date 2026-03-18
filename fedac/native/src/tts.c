// tts.c — Text-to-speech using flite, mixed into ALSA audio thread
// Generates PCM in a background thread, resamples to output rate,
// and feeds into audio->tts_buf ring buffer for mixing.

#define _DEFAULT_SOURCE
#include "tts.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <flite/flite.h>

// Forward-declare the voice registrations
cst_voice *register_cmu_us_slt(void);  // female
cst_voice *register_cmu_us_kal(void);  // male

#define TTS_QUEUE_SIZE 16
#define TTS_MAX_TEXT 256
#define TTS_CACHE_SLOTS 128  // ASCII range for single-char cache

typedef struct {
    float *samples;   // resampled PCM at output rate
    int num_samples;
} TtsCacheEntry;

struct ACTts {
    ACAudio *audio;
    cst_voice *voice;       // default (female)
    cst_voice *voice_male;  // male voice
    pthread_t thread;
    pthread_mutex_t lock;
    pthread_cond_t cond;
    volatile int running;
    volatile int speaking;

    // Simple text queue (with per-entry voice selection)
    char queue[TTS_QUEUE_SIZE][TTS_MAX_TEXT];
    int queue_voice[TTS_QUEUE_SIZE]; // 0 = default/female, 1 = male
    int queue_head;
    int queue_tail;
    int queue_count;

    // Pre-rendered single-character cache for instant playback
    TtsCacheEntry cache[TTS_CACHE_SLOTS];
    // Named cache for special keys
    TtsCacheEntry cache_space, cache_back, cache_enter, cache_clear, cache_tab;
};

static void *tts_thread_fn(void *arg) {
    ACTts *tts = (ACTts *)arg;

    while (tts->running) {
        // Wait for text in queue
        pthread_mutex_lock(&tts->lock);
        while (tts->queue_count == 0 && tts->running) {
            pthread_cond_wait(&tts->cond, &tts->lock);
        }
        if (!tts->running) {
            pthread_mutex_unlock(&tts->lock);
            break;
        }

        // Dequeue text + voice selection
        char text[TTS_MAX_TEXT];
        strncpy(text, tts->queue[tts->queue_head], TTS_MAX_TEXT - 1);
        text[TTS_MAX_TEXT - 1] = 0;
        int use_male = tts->queue_voice[tts->queue_head];
        tts->queue_head = (tts->queue_head + 1) % TTS_QUEUE_SIZE;
        tts->queue_count--;
        tts->speaking = 1;
        pthread_mutex_unlock(&tts->lock);

        // Select voice
        cst_voice *v = (use_male && tts->voice_male) ? tts->voice_male : tts->voice;

        // Synthesize to wave buffer
        cst_wave *wave = flite_text_to_wave(text, v);
        if (!wave) {
            tts->speaking = 0;
            continue;
        }

        // Resample from flite rate (usually 8000 or 16000) to output rate
        // and write into audio ring buffer
        int src_rate = wave->sample_rate;
        int dst_rate = (int)tts->audio->actual_rate;
        if (dst_rate <= 0) dst_rate = AUDIO_SAMPLE_RATE;
        int buf_size = tts->audio->tts_buf_size;
        float *buf = tts->audio->tts_buf;

        double ratio = (double)dst_rate / (double)src_rate;
        int out_samples = (int)(wave->num_samples * ratio);

        for (int i = 0; i < out_samples && tts->running; i++) {
            // Linear interpolation resampling
            double src_pos = i / ratio;
            int idx = (int)src_pos;
            double frac = src_pos - idx;

            float s0 = (idx < wave->num_samples)
                ? wave->samples[idx] / 32768.0f : 0.0f;
            float s1 = (idx + 1 < wave->num_samples)
                ? wave->samples[idx + 1] / 32768.0f : 0.0f;
            float sample = s0 + (s1 - s0) * (float)frac;

            // Wait if ring buffer is full (simple spin with yield)
            int next_wp = (tts->audio->tts_write_pos + 1) % buf_size;
            while (next_wp == tts->audio->tts_read_pos && tts->running) {
                usleep(100); // ~100us yield
            }
            if (!tts->running) break;

            buf[tts->audio->tts_write_pos] = sample;
            tts->audio->tts_write_pos = next_wp;
        }

        delete_wave(wave);
        tts->speaking = 0;
    }

    return NULL;
}

ACTts *tts_init(ACAudio *audio) {
    if (!audio) return NULL;

    flite_init();
    cst_voice *voice = register_cmu_us_slt();
    if (!voice) {
        fprintf(stderr, "[tts] Failed to register female voice\n");
        return NULL;
    }
    feat_set_float(voice->features, "duration_stretch", 0.9);

    cst_voice *voice_male = register_cmu_us_kal();
    if (voice_male) {
        feat_set_float(voice_male->features, "duration_stretch", 0.9);
    } else {
        fprintf(stderr, "[tts] Male voice not available, using female only\n");
    }

    ACTts *tts = calloc(1, sizeof(ACTts));
    tts->audio = audio;
    tts->voice = voice;
    tts->voice_male = voice_male;
    tts->running = 1;
    tts->speaking = 0;
    tts->queue_head = 0;
    tts->queue_tail = 0;
    tts->queue_count = 0;

    pthread_mutex_init(&tts->lock, NULL);
    pthread_cond_init(&tts->cond, NULL);
    pthread_create(&tts->thread, NULL, tts_thread_fn, tts);

    return tts;
}

void tts_speak(ACTts *tts, const char *text) {
    tts_speak_voice(tts, text, 0);
}

void tts_speak_voice(ACTts *tts, const char *text, int male) {
    if (!tts || !text || !text[0]) return;

    pthread_mutex_lock(&tts->lock);
    if (tts->queue_count < TTS_QUEUE_SIZE) {
        strncpy(tts->queue[tts->queue_tail], text, TTS_MAX_TEXT - 1);
        tts->queue[tts->queue_tail][TTS_MAX_TEXT - 1] = 0;
        tts->queue_voice[tts->queue_tail] = male;
        tts->queue_tail = (tts->queue_tail + 1) % TTS_QUEUE_SIZE;
        tts->queue_count++;
        pthread_cond_signal(&tts->cond);
    }
    pthread_mutex_unlock(&tts->lock);
}

// Pre-render a single utterance into a cache entry
static void tts_cache_render(ACTts *tts, TtsCacheEntry *entry, const char *text) {
    cst_wave *wave = flite_text_to_wave(text, tts->voice);
    if (!wave) { entry->samples = NULL; entry->num_samples = 0; return; }

    int src_rate = wave->sample_rate;
    int dst_rate = (int)tts->audio->actual_rate;
    if (dst_rate <= 0) dst_rate = AUDIO_SAMPLE_RATE;
    double ratio = (double)dst_rate / (double)src_rate;
    int out_n = (int)(wave->num_samples * ratio);

    entry->samples = malloc(out_n * sizeof(float));
    entry->num_samples = out_n;

    for (int i = 0; i < out_n; i++) {
        double src_pos = i / ratio;
        int idx = (int)src_pos;
        double frac = src_pos - idx;
        float s0 = (idx < wave->num_samples) ? wave->samples[idx] / 32768.0f : 0.0f;
        float s1 = (idx + 1 < wave->num_samples) ? wave->samples[idx + 1] / 32768.0f : 0.0f;
        entry->samples[i] = s0 + (s1 - s0) * (float)frac;
    }
    delete_wave(wave);
}

void tts_precache(ACTts *tts) {
    if (!tts) return;
    fprintf(stderr, "[tts] Pre-caching letters...\n");
    // Cache printable ASCII a-z, 0-9
    for (int c = 'a'; c <= 'z'; c++) {
        char s[2] = { (char)c, 0 };
        tts_cache_render(tts, &tts->cache[c], s);
    }
    for (int c = '0'; c <= '9'; c++) {
        char s[2] = { (char)c, 0 };
        tts_cache_render(tts, &tts->cache[c], s);
    }
    // Cache special keys
    tts_cache_render(tts, &tts->cache_space, "space");
    tts_cache_render(tts, &tts->cache_back, "back");
    tts_cache_render(tts, &tts->cache_enter, "enter");
    tts_cache_render(tts, &tts->cache_clear, "clear");
    tts_cache_render(tts, &tts->cache_tab, "tab");
    fprintf(stderr, "[tts] Pre-cache done (36 letters + 5 keys)\n");
}

void tts_speak_cached(ACTts *tts, const char *key) {
    if (!tts || !key || !key[0]) return;

    TtsCacheEntry *entry = NULL;

    // Single printable char
    if (key[1] == 0 && (unsigned char)key[0] < TTS_CACHE_SLOTS) {
        char c = key[0];
        // Map uppercase to lowercase cache
        if (c >= 'A' && c <= 'Z') c = c - 'A' + 'a';
        entry = &tts->cache[(unsigned char)c];
    }
    // Named special keys
    else if (strcmp(key, "space") == 0) entry = &tts->cache_space;
    else if (strcmp(key, "back") == 0) entry = &tts->cache_back;
    else if (strcmp(key, "enter") == 0) entry = &tts->cache_enter;
    else if (strcmp(key, "clear") == 0) entry = &tts->cache_clear;
    else if (strcmp(key, "tab") == 0) entry = &tts->cache_tab;

    if (!entry || !entry->samples || entry->num_samples == 0) {
        // Fallback to live synthesis
        tts_speak(tts, key);
        return;
    }

    // Write cached PCM directly to ring buffer (instant, no synthesis)
    int buf_size = tts->audio->tts_buf_size;
    float *buf = tts->audio->tts_buf;

    // Cancel any currently playing TTS by resetting write position
    tts->audio->tts_write_pos = tts->audio->tts_read_pos;

    for (int i = 0; i < entry->num_samples; i++) {
        int next_wp = (tts->audio->tts_write_pos + 1) % buf_size;
        if (next_wp == tts->audio->tts_read_pos) break; // buffer full, skip rest
        buf[tts->audio->tts_write_pos] = entry->samples[i];
        tts->audio->tts_write_pos = next_wp;
    }
}

int tts_is_speaking(ACTts *tts) {
    if (!tts) return 0;
    return tts->speaking || tts->queue_count > 0;
}

void tts_wait(ACTts *tts) {
    if (!tts) return;
    while (tts_is_speaking(tts)) {
        usleep(10000); // 10ms poll
    }
}

void tts_destroy(ACTts *tts) {
    if (!tts) return;
    tts->running = 0;
    pthread_cond_signal(&tts->cond);
    pthread_join(tts->thread, NULL);
    pthread_mutex_destroy(&tts->lock);
    pthread_cond_destroy(&tts->cond);
    free(tts);
}
