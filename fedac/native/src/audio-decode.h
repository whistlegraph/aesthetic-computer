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
    volatile double  speed;          // 1.0 = normal, negative = reverse
    double           ring_frac;      // fractional read position for interpolated speed

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

    // Waveform peaks (decimated max-amplitude samples for visualization)
    // Generated once on load by scanning the entire file via separate pass.
    float           *peaks;          // [0..1] amplitude peaks
    int              peak_count;     // number of peaks (typically 1024)

    // Optional downscaled video preview frames for pieces like tapes.mjs.
    // Frames are stored as opaque ARGB32 pixels (0xAARRGGBB) in a flat array:
    // [frame0 pixels][frame1 pixels]...
    uint32_t        *video_frames;
    int              video_frame_count;
    int              video_width;
    int              video_height;
    double           video_fps;
    volatile int     video_ready;
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

// Generate peaks for the loaded file (call after deck_decoder_load).
// Decodes the entire file once and writes max-amplitude peaks per chunk.
// Safe to call from main thread; takes a few hundred ms for typical tracks.
int deck_decoder_generate_peaks(ACDeckDecoder *d, int target_count);

// Generate a lightweight downscaled video preview strip for the loaded file.
// Safe to call from main thread; intended for UI playback rather than full-res video.
int deck_decoder_generate_video_preview(ACDeckDecoder *d, int width, int height, int fps);

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
    uint32_t *video_frames;
    int video_frame_count;
    int video_width;
    int video_height;
    double video_fps;
    volatile int video_ready;
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
static inline int deck_decoder_generate_video_preview(ACDeckDecoder *d, int width, int height, int fps) {
    (void)d; (void)width; (void)height; (void)fps; return -1;
}

#endif // HAVE_AVCODEC
#endif // AC_AUDIO_DECODE_H
