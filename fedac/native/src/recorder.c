// recorder.c — On-device MP4 recording (H.264 + AAC in fragmented MP4)
// Encoder runs in a dedicated thread so the main 60fps loop is never blocked.

#ifdef HAVE_AVCODEC

#include "recorder.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdatomic.h>
#include <time.h>
#include <sys/stat.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/opt.h>
#include <libavutil/imgutils.h>
#include <libavutil/channel_layout.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>

// ── Ring buffer for audio ──
#define AUDIO_RING_SAMPLES (192000 * 2)  // ~1 second at 192kHz stereo (interleaved)
#define AUDIO_RING_MASK    (AUDIO_RING_SAMPLES - 1)
// Ensure power of 2 (192000*2=384000 is not power of 2, so we use modulo instead)

// ── Triple-buffer for video frames ──
#define VIDEO_SLOTS 3

extern void ac_log(const char *fmt, ...);

struct ACRecorder {
    // Config
    int width, height, fps;
    unsigned int audio_src_rate;

    // State
    volatile int recording;
    volatile int stopping;
    struct timespec start_time;

    // Video triple-buffer
    uint32_t *video_buf[VIDEO_SLOTS];
    int video_stride;
    atomic_int video_write_idx;  // main thread writes here (mod VIDEO_SLOTS)
    atomic_int video_read_idx;   // encoder reads here (mod VIDEO_SLOTS)
    atomic_int video_ready;      // number of frames ready to encode

    // Audio ring buffer (interleaved int16 stereo at source rate)
    int16_t *audio_ring;
    int audio_ring_size;
    atomic_int audio_write_pos;  // monotonic write position
    atomic_int audio_read_pos;   // encoder thread read position

    // Encoder thread
    pthread_t thread;
    volatile int thread_running;

    // ffmpeg contexts
    AVFormatContext *fmt_ctx;
    AVCodecContext *video_enc;
    AVCodecContext *audio_enc;
    AVStream *video_st;
    AVStream *audio_st;
    struct SwsContext *sws;
    SwrContext *swr;
    AVFrame *video_frame;   // YUV420P frame for encoder
    AVFrame *audio_frame;   // float planar frame for AAC encoder
    int64_t video_pts;
    int64_t audio_pts;

    // Audio resampler output buffer
    int16_t *audio_resample_buf;
    int audio_resample_buf_size;
};

// ── Forward declarations ──
static void *encoder_thread(void *arg);
static int setup_video_stream(ACRecorder *rec);
static int setup_audio_stream(ACRecorder *rec);
static void encode_video_frame(ACRecorder *rec, const uint32_t *pixels, int stride);
static void encode_audio_chunk(ACRecorder *rec);
static void flush_encoders(ACRecorder *rec);

// ── Public API ──

ACRecorder *recorder_create(int width, int height, int fps, unsigned int audio_rate) {
    ACRecorder *rec = calloc(1, sizeof(ACRecorder));
    if (!rec) return NULL;

    rec->width = width;
    rec->height = height;
    rec->fps = fps;
    rec->audio_src_rate = audio_rate;

    // Allocate video triple-buffer
    size_t frame_bytes = (size_t)width * height * sizeof(uint32_t);
    for (int i = 0; i < VIDEO_SLOTS; i++) {
        rec->video_buf[i] = malloc(frame_bytes);
        if (!rec->video_buf[i]) {
            for (int j = 0; j < i; j++) free(rec->video_buf[j]);
            free(rec);
            return NULL;
        }
    }
    rec->video_stride = width;

    // Allocate audio ring buffer
    rec->audio_ring_size = AUDIO_RING_SAMPLES;
    rec->audio_ring = calloc(rec->audio_ring_size, sizeof(int16_t));
    if (!rec->audio_ring) {
        for (int i = 0; i < VIDEO_SLOTS; i++) free(rec->video_buf[i]);
        free(rec);
        return NULL;
    }

    return rec;
}

int recorder_start(ACRecorder *rec, const char *path) {
    if (!rec || rec->recording) return -1;

    ac_log("[recorder] starting: %s (%dx%d @ %dfps, audio %uHz→48kHz)\n",
           path, rec->width, rec->height, rec->fps, rec->audio_src_rate);

    // Create output format context (fragmented MP4)
    int ret = avformat_alloc_output_context2(&rec->fmt_ctx, NULL, "mp4", path);
    if (ret < 0 || !rec->fmt_ctx) {
        ac_log("[recorder] failed to create output context\n");
        return -1;
    }

    // Setup streams
    if (setup_video_stream(rec) < 0) {
        avformat_free_context(rec->fmt_ctx);
        rec->fmt_ctx = NULL;
        return -1;
    }
    if (setup_audio_stream(rec) < 0) {
        avcodec_free_context(&rec->video_enc);
        avformat_free_context(rec->fmt_ctx);
        rec->fmt_ctx = NULL;
        return -1;
    }

    // Open output file
    if (!(rec->fmt_ctx->oformat->flags & AVFMT_NOFILE)) {
        ret = avio_open(&rec->fmt_ctx->pb, path, AVIO_FLAG_WRITE);
        if (ret < 0) {
            ac_log("[recorder] failed to open output file: %s\n", path);
            avcodec_free_context(&rec->video_enc);
            avcodec_free_context(&rec->audio_enc);
            avformat_free_context(rec->fmt_ctx);
            rec->fmt_ctx = NULL;
            return -1;
        }
    }

    // Set fragmented MP4 options (crash-safe)
    AVDictionary *opts = NULL;
    av_dict_set(&opts, "movflags", "frag_keyframe+empty_moov+default_base_moof", 0);

    ret = avformat_write_header(rec->fmt_ctx, &opts);
    av_dict_free(&opts);
    if (ret < 0) {
        ac_log("[recorder] failed to write header\n");
        avio_closep(&rec->fmt_ctx->pb);
        avcodec_free_context(&rec->video_enc);
        avcodec_free_context(&rec->audio_enc);
        avformat_free_context(rec->fmt_ctx);
        rec->fmt_ctx = NULL;
        return -1;
    }

    // Reset state
    rec->video_pts = 0;
    rec->audio_pts = 0;
    atomic_store(&rec->video_write_idx, 0);
    atomic_store(&rec->video_read_idx, 0);
    atomic_store(&rec->video_ready, 0);
    atomic_store(&rec->audio_write_pos, 0);
    atomic_store(&rec->audio_read_pos, 0);
    rec->stopping = 0;

    clock_gettime(CLOCK_MONOTONIC, &rec->start_time);
    rec->recording = 1;

    // Start encoder thread
    rec->thread_running = 1;
    if (pthread_create(&rec->thread, NULL, encoder_thread, rec) != 0) {
        ac_log("[recorder] failed to create encoder thread\n");
        rec->recording = 0;
        rec->thread_running = 0;
        av_write_trailer(rec->fmt_ctx);
        avio_closep(&rec->fmt_ctx->pb);
        avcodec_free_context(&rec->video_enc);
        avcodec_free_context(&rec->audio_enc);
        avformat_free_context(rec->fmt_ctx);
        rec->fmt_ctx = NULL;
        return -1;
    }

    ac_log("[recorder] recording started\n");
    return 0;
}

void recorder_submit_video(ACRecorder *rec, const uint32_t *pixels, int stride) {
    if (!rec || !rec->recording) return;

    // Copy into next write slot
    int slot = atomic_load(&rec->video_write_idx) % VIDEO_SLOTS;

    // Copy row by row in case stride differs
    for (int y = 0; y < rec->height; y++) {
        memcpy(rec->video_buf[slot] + y * rec->width,
               pixels + y * stride,
               rec->width * sizeof(uint32_t));
    }

    atomic_fetch_add(&rec->video_write_idx, 1);
    atomic_fetch_add(&rec->video_ready, 1);
}

void recorder_submit_audio(ACRecorder *rec, const int16_t *pcm, int frames) {
    if (!rec || !rec->recording) return;

    int samples = frames * 2;  // stereo interleaved
    int wp = atomic_load(&rec->audio_write_pos);

    for (int i = 0; i < samples; i++) {
        rec->audio_ring[(wp + i) % rec->audio_ring_size] = pcm[i];
    }
    atomic_fetch_add(&rec->audio_write_pos, samples);
}

void recorder_stop(ACRecorder *rec) {
    if (!rec || !rec->recording) return;

    ac_log("[recorder] stopping...\n");
    rec->stopping = 1;
    rec->recording = 0;

    // Wait for encoder thread to finish
    if (rec->thread_running) {
        pthread_join(rec->thread, NULL);
        rec->thread_running = 0;
    }

    // Flush remaining frames
    flush_encoders(rec);

    // Finalize MP4
    if (rec->fmt_ctx) {
        av_write_trailer(rec->fmt_ctx);
        if (!(rec->fmt_ctx->oformat->flags & AVFMT_NOFILE))
            avio_closep(&rec->fmt_ctx->pb);
    }

    // Free encoder resources
    if (rec->sws) { sws_freeContext(rec->sws); rec->sws = NULL; }
    if (rec->swr) { swr_free(&rec->swr); }
    if (rec->video_frame) { av_frame_free(&rec->video_frame); }
    if (rec->audio_frame) { av_frame_free(&rec->audio_frame); }
    if (rec->video_enc) { avcodec_free_context(&rec->video_enc); }
    if (rec->audio_enc) { avcodec_free_context(&rec->audio_enc); }
    if (rec->fmt_ctx) { avformat_free_context(rec->fmt_ctx); rec->fmt_ctx = NULL; }
    if (rec->audio_resample_buf) { free(rec->audio_resample_buf); rec->audio_resample_buf = NULL; }

    ac_log("[recorder] stopped, file finalized\n");
}

int recorder_is_recording(ACRecorder *rec) {
    return rec ? rec->recording : 0;
}

double recorder_elapsed(ACRecorder *rec) {
    if (!rec || !rec->recording) return 0.0;
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    return (now.tv_sec - rec->start_time.tv_sec) +
           (now.tv_nsec - rec->start_time.tv_nsec) / 1e9;
}

void recorder_destroy(ACRecorder *rec) {
    if (!rec) return;
    if (rec->recording) recorder_stop(rec);

    for (int i = 0; i < VIDEO_SLOTS; i++) free(rec->video_buf[i]);
    free(rec->audio_ring);
    free(rec);
}

// ── Internal: stream setup ──

static int setup_video_stream(ACRecorder *rec) {
    const AVCodec *codec = avcodec_find_encoder(AV_CODEC_ID_H264);
    if (!codec) {
        // Fallback to MPEG4 if H.264 not available (ffmpeg-free on Fedora)
        codec = avcodec_find_encoder(AV_CODEC_ID_MPEG4);
        if (!codec) {
            ac_log("[recorder] no video encoder found\n");
            return -1;
        }
        ac_log("[recorder] H.264 not available, using MPEG-4\n");
    }

    rec->video_st = avformat_new_stream(rec->fmt_ctx, NULL);
    if (!rec->video_st) return -1;

    rec->video_enc = avcodec_alloc_context3(codec);
    if (!rec->video_enc) return -1;

    rec->video_enc->width = rec->width;
    rec->video_enc->height = rec->height;
    rec->video_enc->time_base = (AVRational){1, rec->fps};
    rec->video_enc->framerate = (AVRational){rec->fps, 1};
    rec->video_enc->pix_fmt = AV_PIX_FMT_YUV420P;
    rec->video_enc->gop_size = rec->fps;  // Keyframe every second
    rec->video_enc->max_b_frames = 0;     // No B-frames for low latency

    // Encoder-specific options
    if (codec->id == AV_CODEC_ID_H264) {
        av_opt_set(rec->video_enc->priv_data, "preset", "ultrafast", 0);
        av_opt_set(rec->video_enc->priv_data, "tune", "zerolatency", 0);
        rec->video_enc->bit_rate = 4000000;  // 4 Mbps for crisp pixel art
    } else {
        rec->video_enc->bit_rate = 4000000;
    }

    if (rec->fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        rec->video_enc->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

    int ret = avcodec_open2(rec->video_enc, codec, NULL);
    if (ret < 0) {
        ac_log("[recorder] failed to open video encoder\n");
        return -1;
    }

    ret = avcodec_parameters_from_context(rec->video_st->codecpar, rec->video_enc);
    if (ret < 0) return -1;
    rec->video_st->time_base = rec->video_enc->time_base;

    // Allocate YUV frame
    rec->video_frame = av_frame_alloc();
    rec->video_frame->format = AV_PIX_FMT_YUV420P;
    rec->video_frame->width = rec->width;
    rec->video_frame->height = rec->height;
    av_frame_get_buffer(rec->video_frame, 0);

    // Setup color space converter (ARGB → YUV420P)
    // Note: our ARGB32 is stored as 0xAARRGGBB in memory, which on little-endian
    // is byte order B, G, R, A → AV_PIX_FMT_BGRA
    rec->sws = sws_getContext(
        rec->width, rec->height, AV_PIX_FMT_BGRA,
        rec->width, rec->height, AV_PIX_FMT_YUV420P,
        SWS_FAST_BILINEAR, NULL, NULL, NULL);
    if (!rec->sws) {
        ac_log("[recorder] failed to create sws context\n");
        return -1;
    }

    ac_log("[recorder] video: %s %dx%d @ %dfps\n",
           codec->name, rec->width, rec->height, rec->fps);
    return 0;
}

static int setup_audio_stream(ACRecorder *rec) {
    const AVCodec *codec = avcodec_find_encoder(AV_CODEC_ID_AAC);
    if (!codec) {
        ac_log("[recorder] AAC encoder not found, trying mp2\n");
        codec = avcodec_find_encoder(AV_CODEC_ID_MP2);
        if (!codec) {
            ac_log("[recorder] no audio encoder found\n");
            return -1;
        }
    }

    rec->audio_st = avformat_new_stream(rec->fmt_ctx, NULL);
    if (!rec->audio_st) return -1;

    rec->audio_enc = avcodec_alloc_context3(codec);
    if (!rec->audio_enc) return -1;

    rec->audio_enc->sample_rate = 48000;
    rec->audio_enc->bit_rate = 128000;
    AVChannelLayout stereo = AV_CHANNEL_LAYOUT_STEREO;
    av_channel_layout_copy(&rec->audio_enc->ch_layout, &stereo);
    rec->audio_enc->sample_fmt = codec->sample_fmts ? codec->sample_fmts[0] : AV_SAMPLE_FMT_FLTP;
    rec->audio_enc->time_base = (AVRational){1, 48000};

    if (rec->fmt_ctx->oformat->flags & AVFMT_GLOBALHEADER)
        rec->audio_enc->flags |= AV_CODEC_FLAG_GLOBAL_HEADER;

    int ret = avcodec_open2(rec->audio_enc, codec, NULL);
    if (ret < 0) {
        ac_log("[recorder] failed to open audio encoder\n");
        return -1;
    }

    ret = avcodec_parameters_from_context(rec->audio_st->codecpar, rec->audio_enc);
    if (ret < 0) return -1;
    rec->audio_st->time_base = rec->audio_enc->time_base;

    // Allocate audio frame
    rec->audio_frame = av_frame_alloc();
    rec->audio_frame->format = rec->audio_enc->sample_fmt;
    av_channel_layout_copy(&rec->audio_frame->ch_layout, &rec->audio_enc->ch_layout);
    rec->audio_frame->sample_rate = 48000;
    rec->audio_frame->nb_samples = rec->audio_enc->frame_size;
    if (rec->audio_frame->nb_samples == 0)
        rec->audio_frame->nb_samples = 1024;
    av_frame_get_buffer(rec->audio_frame, 0);

    // Setup resampler: source rate stereo int16 → 48kHz stereo float planar
    ret = swr_alloc_set_opts2(&rec->swr,
        &stereo, rec->audio_enc->sample_fmt, 48000,
        &stereo, AV_SAMPLE_FMT_S16, rec->audio_src_rate,
        0, NULL);
    if (ret < 0 || !rec->swr) {
        ac_log("[recorder] failed to create resampler\n");
        return -1;
    }
    ret = swr_init(rec->swr);
    if (ret < 0) {
        ac_log("[recorder] failed to init resampler\n");
        return -1;
    }

    ac_log("[recorder] audio: %s %uHz→48kHz, %d-sample frames\n",
           codec->name, rec->audio_src_rate, rec->audio_frame->nb_samples);
    return 0;
}

// ── Internal: encoder thread ──

static void *encoder_thread(void *arg) {
    ACRecorder *rec = (ACRecorder *)arg;

    while (rec->recording || atomic_load(&rec->video_ready) > 0) {
        int did_work = 0;

        // Encode pending video frames
        while (atomic_load(&rec->video_ready) > 0) {
            int slot = atomic_load(&rec->video_read_idx) % VIDEO_SLOTS;
            encode_video_frame(rec, rec->video_buf[slot], rec->width);
            atomic_fetch_add(&rec->video_read_idx, 1);
            atomic_fetch_sub(&rec->video_ready, 1);
            did_work = 1;
        }

        // Encode pending audio
        int avail = atomic_load(&rec->audio_write_pos) - atomic_load(&rec->audio_read_pos);
        if (avail >= rec->audio_frame->nb_samples * 2) {  // *2 for stereo
            encode_audio_chunk(rec);
            did_work = 1;
        }

        if (!did_work) {
            // Sleep ~2ms to avoid busy-waiting
            struct timespec ts = {0, 2000000};
            nanosleep(&ts, NULL);
        }
    }

    rec->thread_running = 0;
    return NULL;
}

static void encode_video_frame(ACRecorder *rec, const uint32_t *pixels, int stride) {
    // Convert ARGB32 → YUV420P
    const uint8_t *src_data[1] = { (const uint8_t *)pixels };
    int src_linesize[1] = { stride * 4 };

    av_frame_make_writable(rec->video_frame);
    sws_scale(rec->sws, src_data, src_linesize, 0, rec->height,
              rec->video_frame->data, rec->video_frame->linesize);

    rec->video_frame->pts = rec->video_pts++;

    // Send frame to encoder
    int ret = avcodec_send_frame(rec->video_enc, rec->video_frame);
    if (ret < 0) return;

    // Read all available packets
    AVPacket *pkt = av_packet_alloc();
    while (avcodec_receive_packet(rec->video_enc, pkt) == 0) {
        av_packet_rescale_ts(pkt, rec->video_enc->time_base, rec->video_st->time_base);
        pkt->stream_index = rec->video_st->index;
        av_interleaved_write_frame(rec->fmt_ctx, pkt);
        av_packet_unref(pkt);
    }
    av_packet_free(&pkt);
}

static void encode_audio_chunk(ACRecorder *rec) {
    int frame_samples = rec->audio_frame->nb_samples;
    int src_samples_needed = frame_samples * 2;  // stereo interleaved

    // How many source samples do we need for one output frame?
    // At 192kHz→48kHz that's a 4:1 ratio, so we need 4x the output frame size
    int ratio = (rec->audio_src_rate + 47999) / 48000;  // ceil
    int src_needed = frame_samples * ratio * 2;  // stereo interleaved

    int rp = atomic_load(&rec->audio_read_pos);
    int avail = atomic_load(&rec->audio_write_pos) - rp;
    if (avail < src_needed) return;

    // Copy source samples from ring buffer into a contiguous buffer
    if (!rec->audio_resample_buf || rec->audio_resample_buf_size < src_needed) {
        free(rec->audio_resample_buf);
        rec->audio_resample_buf_size = src_needed * 2;  // over-allocate
        rec->audio_resample_buf = malloc(rec->audio_resample_buf_size * sizeof(int16_t));
    }

    for (int i = 0; i < src_needed; i++) {
        rec->audio_resample_buf[i] = rec->audio_ring[(rp + i) % rec->audio_ring_size];
    }
    atomic_fetch_add(&rec->audio_read_pos, src_needed);

    // Resample and encode
    av_frame_make_writable(rec->audio_frame);

    const uint8_t *in_data[1] = { (const uint8_t *)rec->audio_resample_buf };
    int in_samples = src_needed / 2;  // frames (not samples)

    int out_samples = swr_convert(rec->swr,
        rec->audio_frame->data, frame_samples,
        in_data, in_samples);

    if (out_samples <= 0) return;

    rec->audio_frame->nb_samples = out_samples;
    rec->audio_frame->pts = rec->audio_pts;
    rec->audio_pts += out_samples;

    int ret = avcodec_send_frame(rec->audio_enc, rec->audio_frame);
    if (ret < 0) return;

    AVPacket *pkt = av_packet_alloc();
    while (avcodec_receive_packet(rec->audio_enc, pkt) == 0) {
        av_packet_rescale_ts(pkt, rec->audio_enc->time_base, rec->audio_st->time_base);
        pkt->stream_index = rec->audio_st->index;
        av_interleaved_write_frame(rec->fmt_ctx, pkt);
        av_packet_unref(pkt);
    }
    av_packet_free(&pkt);
}

static void flush_encoders(ACRecorder *rec) {
    AVPacket *pkt = av_packet_alloc();

    // Flush video encoder
    avcodec_send_frame(rec->video_enc, NULL);
    while (avcodec_receive_packet(rec->video_enc, pkt) == 0) {
        av_packet_rescale_ts(pkt, rec->video_enc->time_base, rec->video_st->time_base);
        pkt->stream_index = rec->video_st->index;
        av_interleaved_write_frame(rec->fmt_ctx, pkt);
        av_packet_unref(pkt);
    }

    // Flush remaining audio through resampler
    if (rec->swr) {
        av_frame_make_writable(rec->audio_frame);
        int flushed = swr_convert(rec->swr,
            rec->audio_frame->data, rec->audio_frame->nb_samples,
            NULL, 0);
        if (flushed > 0) {
            rec->audio_frame->nb_samples = flushed;
            rec->audio_frame->pts = rec->audio_pts;
            rec->audio_pts += flushed;
            avcodec_send_frame(rec->audio_enc, rec->audio_frame);
        }
    }

    // Flush audio encoder
    avcodec_send_frame(rec->audio_enc, NULL);
    while (avcodec_receive_packet(rec->audio_enc, pkt) == 0) {
        av_packet_rescale_ts(pkt, rec->audio_enc->time_base, rec->audio_st->time_base);
        pkt->stream_index = rec->audio_st->index;
        av_interleaved_write_frame(rec->fmt_ctx, pkt);
        av_packet_unref(pkt);
    }

    av_packet_free(&pkt);
}

#endif /* HAVE_AVCODEC */
