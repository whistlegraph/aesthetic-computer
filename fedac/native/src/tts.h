#ifndef AC_TTS_H
#define AC_TTS_H

#include "audio.h"

typedef struct ACTts ACTts;

// Initialize TTS engine (call after audio_init)
ACTts *tts_init(ACAudio *audio);

// Speak text asynchronously (queued, mixed into audio thread)
void tts_speak(ACTts *tts, const char *text);

// Check if currently speaking
int tts_is_speaking(ACTts *tts);

// Wait for current speech to finish
void tts_wait(ACTts *tts);

// Cleanup
void tts_destroy(ACTts *tts);

#endif
