// audio-decode.h — Streaming audio file decoder for DJ decks
// Uses FFmpeg (libavformat/libavcodec/libswresample) to decode MP3/WAV/FLAC/OGG/AAC
// into a lock-free stereo float ring buffer consumed by the audio thread.

#ifndef AC_AUDIO_DECODE_H
#define AC_AUDIO_DECODE_H

#ifdef HAVE_AVCODEC

#include <pthread.h>
#include <stdint.h>

// ~5 seconds of stereo float at 192kHz = ~7.7 MB per decoder
#define DECK_RING_SECONDS 5

typedef struct ACDeckDecoder {
    // Ring buffer: interleaved stereo float (L, R, L, R, ...)
    float           *ring;
    int              ring_size;      // capacity in frames (not samples)
    volatile int     ring_write;     // monotonic write position (frames)
    volatile int     ring_read;      // monotonic read position (frames)

    // Decoder thread
    pthread_t        thread;
    pthread_mutex_t  mutex;
    pthread_cond_t   cond;
    volatile int     thread_running; // thread alive
    volatile int     decoding;       // actively decoding (vs paused/finished)

    // Seek request (set by main thread, consumed by decoder thread)
    volatile int     seek_requested;
    volatile double  seek_target;    // seconds

    // Speed control (pitch-coupled, like vinyl)
    volatile double  speed;          // 1.0 = normal, 0.5-2.0 range

    // Track metadata
    double           duration;       // total duration in seconds
    volatile double  position;       // current playback position in seconds
    int              src_sample_rate; // source file sample rate
    int              src_channels;   // source file channels
    unsigned int     out_rate;       // output sample rate (e.g. 192000)
    char             path[512];
    char             title[256];
    char             artist[256];

    // Status
    volatile int     loaded;         // file opened successfully
    volatile int     finished;       // reached end of file
    volatile int     error;
    char             error_msg[128];

    // FFmpeg state (opaque, managed internally)
    void            *fmt_ctx;        // AVFormatContext*
    void            *codec_ctx;      // AVCodecContext*
    void            *swr;            // SwrContext*
    int              stream_idx;     // audio stream index
} ACDeckDecoder;

// Create a decoder instance for the given output sample rate
ACDeckDecoder *deck_decoder_create(unsigned int output_rate);

// Load an audio file. Starts decoder thread. Returns 0 on success, -1 on error.
int deck_decoder_load(ACDeckDecoder *d, const char *path);

// Playback control
void deck_decoder_play(ACDeckDecoder *d);
void deck_decoder_pause(ACDeckDecoder *d);
void deck_decoder_seek(ACDeckDecoder *d, double seconds);
void deck_decoder_set_speed(ACDeckDecoder *d, double speed); // 0.5–2.0

// Unload current file and stop thread (decoder can be reused with another load)
void deck_decoder_unload(ACDeckDecoder *d);

// Destroy decoder and free all resources
void deck_decoder_destroy(ACDeckDecoder *d);

#else // !HAVE_AVCODEC

// Stubs when FFmpeg is not available
typedef struct ACDeckDecoder {
    volatile int loaded;
    volatile int playing;
    volatile int finished;
    volatile double position;
    double duration;
    volatile double speed;
    char title[256];
    char artist[256];
    char error_msg[128];
    volatile int error;
    // Ring buffer stubs for audio thread
    float *ring;
    int ring_size;
    volatile int ring_write;
    volatile int ring_read;
} ACDeckDecoder;

static inline ACDeckDecoder *deck_decoder_create(unsigned int output_rate) {
    (void)output_rate; return 0;
}
static inline int deck_decoder_load(ACDeckDecoder *d, const char *path) {
    (void)d; (void)path; return -1;
}
static inline void deck_decoder_play(ACDeckDecoder *d) { (void)d; }
static inline void deck_decoder_pause(ACDeckDecoder *d) { (void)d; }
static inline void deck_decoder_seek(ACDeckDecoder *d, double s) { (void)d; (void)s; }
static inline void deck_decoder_set_speed(ACDeckDecoder *d, double s) { (void)d; (void)s; }
static inline void deck_decoder_unload(ACDeckDecoder *d) { (void)d; }
static inline void deck_decoder_destroy(ACDeckDecoder *d) { (void)d; }

#endif // HAVE_AVCODEC
#endif // AC_AUDIO_DECODE_H
