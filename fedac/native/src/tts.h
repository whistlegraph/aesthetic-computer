#ifndef AC_TTS_H
#define AC_TTS_H

#include "audio.h"

typedef struct ACTts ACTts;

// Initialize TTS engine (call after audio_init)
ACTts *tts_init(ACAudio *audio);

// Speak text asynchronously (queued, mixed into audio thread)
void tts_speak(ACTts *tts, const char *text);

// Speak with voice selection (male=0 for female/default, male=1 for male)
void tts_speak_voice(ACTts *tts, const char *text, int male);

// Speak a single cached letter/word instantly (pre-rendered at init)
void tts_speak_cached(ACTts *tts, const char *key);

// Pre-cache common single-character utterances (called at init)
void tts_precache(ACTts *tts);

// Check if currently speaking
int tts_is_speaking(ACTts *tts);

// Wait for current speech to finish
void tts_wait(ACTts *tts);

// Cleanup
void tts_destroy(ACTts *tts);

#endif
