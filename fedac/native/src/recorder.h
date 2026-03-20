// recorder.h — On-device video recording (framebuffer + audio → MP4)
// Uses libavcodec/libavformat for H.264 + AAC encoding in fragmented MP4.

#ifndef AC_RECORDER_H
#define AC_RECORDER_H

#ifdef HAVE_AVCODEC

#include <stdint.h>

typedef struct ACRecorder ACRecorder;

// Create recorder for given framebuffer dimensions and audio sample rate.
// fps is the target video frame rate (typically 60).
// audio_rate is the source PCM sample rate (e.g., 192000).
ACRecorder *recorder_create(int width, int height, int fps, unsigned int audio_rate);

// Start recording to the given path (e.g., "/mnt/rec/2026-03-20_14-30-00.mp4").
// Returns 0 on success, -1 on failure.
int recorder_start(ACRecorder *rec, const char *path);

// Submit a video frame (ARGB32 pixels, stride in pixels).
// Non-blocking: copies pixels and signals the encoder thread.
void recorder_submit_video(ACRecorder *rec, const uint32_t *pixels, int stride);

// Submit audio samples (interleaved int16 stereo PCM at source rate).
// Non-blocking: copies into ring buffer for the encoder thread.
void recorder_submit_audio(ACRecorder *rec, const int16_t *pcm, int frames);

// Stop recording: flush encoder, finalize MP4, join thread.
// Blocks until the file is fully written.
void recorder_stop(ACRecorder *rec);

// Returns 1 if currently recording, 0 otherwise.
int recorder_is_recording(ACRecorder *rec);

// Returns recording duration in seconds (0 if not recording).
double recorder_elapsed(ACRecorder *rec);

// Destroy recorder and free all resources.
void recorder_destroy(ACRecorder *rec);

#else /* !HAVE_AVCODEC */

// Stub API when ffmpeg is not available
typedef struct ACRecorder ACRecorder;
static inline ACRecorder *recorder_create(int w, int h, int fps, unsigned int ar) { (void)w; (void)h; (void)fps; (void)ar; return (void*)0; }
static inline int recorder_start(ACRecorder *r, const char *p) { (void)r; (void)p; return -1; }
static inline void recorder_submit_video(ACRecorder *r, const uint32_t *px, int s) { (void)r; (void)px; (void)s; }
static inline void recorder_submit_audio(ACRecorder *r, const int16_t *pcm, int f) { (void)r; (void)pcm; (void)f; }
static inline void recorder_stop(ACRecorder *r) { (void)r; }
static inline int recorder_is_recording(ACRecorder *r) { (void)r; return 0; }
static inline double recorder_elapsed(ACRecorder *r) { (void)r; return 0.0; }
static inline void recorder_destroy(ACRecorder *r) { (void)r; }

#endif /* HAVE_AVCODEC */

#endif /* AC_RECORDER_H */
