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

// Forward-declare the voice registration (avoids needing flite internal headers)
cst_voice *register_cmu_us_kal(void);

#define TTS_QUEUE_SIZE 16
#define TTS_MAX_TEXT 256

struct ACTts {
    ACAudio *audio;
    cst_voice *voice;
    pthread_t thread;
    pthread_mutex_t lock;
    pthread_cond_t cond;
    volatile int running;
    volatile int speaking;

    // Simple text queue
    char queue[TTS_QUEUE_SIZE][TTS_MAX_TEXT];
    int queue_head;
    int queue_tail;
    int queue_count;
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

        // Dequeue text
        char text[TTS_MAX_TEXT];
        strncpy(text, tts->queue[tts->queue_head], TTS_MAX_TEXT - 1);
        text[TTS_MAX_TEXT - 1] = 0;
        tts->queue_head = (tts->queue_head + 1) % TTS_QUEUE_SIZE;
        tts->queue_count--;
        tts->speaking = 1;
        pthread_mutex_unlock(&tts->lock);

        // Synthesize to wave buffer
        cst_wave *wave = flite_text_to_wave(text, tts->voice);
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
    cst_voice *voice = register_cmu_us_kal();
    if (!voice) {
        fprintf(stderr, "[tts] Failed to register voice\n");
        return NULL;
    }

    // Set voice parameters for a crisp computer voice
    feat_set_float(voice->features, "duration_stretch", 0.9);  // slightly faster

    ACTts *tts = calloc(1, sizeof(ACTts));
    tts->audio = audio;
    tts->voice = voice;
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
    if (!tts || !text || !text[0]) return;

    pthread_mutex_lock(&tts->lock);
    if (tts->queue_count < TTS_QUEUE_SIZE) {
        strncpy(tts->queue[tts->queue_tail], text, TTS_MAX_TEXT - 1);
        tts->queue[tts->queue_tail][TTS_MAX_TEXT - 1] = 0;
        tts->queue_tail = (tts->queue_tail + 1) % TTS_QUEUE_SIZE;
        tts->queue_count++;
        pthread_cond_signal(&tts->cond);
    }
    pthread_mutex_unlock(&tts->lock);
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
