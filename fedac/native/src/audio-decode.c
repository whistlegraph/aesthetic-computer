// audio-decode.c — Streaming audio file decoder for DJ decks
// Decodes MP3/WAV/FLAC/OGG/AAC via FFmpeg into a stereo float ring buffer.
// Single-producer (decoder thread) / single-consumer (audio thread) lock-free design.

#ifdef HAVE_AVCODEC

#include "audio-decode.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/opt.h>
#include <libavutil/imgutils.h>
#include <libavutil/channel_layout.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>

extern void ac_log(const char *fmt, ...);

// --- Internal helpers ---

static void extract_metadata(ACDeckDecoder *d, AVFormatContext *fmt) {
    const AVDictionaryEntry *tag = NULL;
    tag = av_dict_get(fmt->metadata, "title", NULL, 0);
    if (tag) snprintf(d->title, sizeof(d->title), "%s", tag->value);
    tag = av_dict_get(fmt->metadata, "artist", NULL, 0);
    if (tag) snprintf(d->artist, sizeof(d->artist), "%s", tag->value);
}

static int ring_available(ACDeckDecoder *d) {
    return d->ring_write - d->ring_read;
}

static int ring_free(ACDeckDecoder *d) {
    return d->ring_size - ring_available(d);
}

static void free_video_preview(ACDeckDecoder *d) {
    if (!d) return;
    if (d->video_frames) {
        free(d->video_frames);
        d->video_frames = NULL;
    }
    d->video_frame_count = 0;
    d->video_width = 0;
    d->video_height = 0;
    d->video_fps = 0.0;
    d->video_ready = 0;
}

// --- Decoder thread ---

static void *decoder_thread_fn(void *arg) {
    ACDeckDecoder *d = (ACDeckDecoder *)arg;
    AVFormatContext *fmt_ctx = (AVFormatContext *)d->fmt_ctx;
    AVCodecContext *codec_ctx = (AVCodecContext *)d->codec_ctx;
    SwrContext *swr = (SwrContext *)d->swr;

    AVPacket *pkt = av_packet_alloc();
    AVFrame *frame = av_frame_alloc();
    int max_out_samples = 8192;
    float *resample_buf = NULL;

    if (!pkt || !frame) {
        d->error = 1;
        snprintf(d->error_msg, sizeof(d->error_msg), "alloc failed");
        goto done;
    }

    // Temporary buffer for resampled output (enough for one decoded frame)
    resample_buf = malloc(max_out_samples * 2 * sizeof(float)); // stereo
    if (!resample_buf) {
        d->error = 1;
        snprintf(d->error_msg, sizeof(d->error_msg), "resample buf alloc failed");
        goto done;
    }

    while (d->thread_running) {
        // Handle seek requests
        if (d->seek_requested) {
            int64_t ts = (int64_t)(d->seek_target * AV_TIME_BASE);
            av_seek_frame(fmt_ctx, -1, ts, AVSEEK_FLAG_BACKWARD);
            avcodec_flush_buffers(codec_ctx);
            // Reset ring buffer
            d->ring_write = 0;
            d->ring_read = 0;
            d->ring_frac = 0.0;
            d->position = d->seek_target;
            d->finished = 0;
            d->seek_requested = 0;
        }

        // Pause: wait for signal
        if (!d->decoding) {
            pthread_mutex_lock(&d->mutex);
            while (!d->decoding && d->thread_running && !d->seek_requested) {
                pthread_cond_wait(&d->cond, &d->mutex);
            }
            pthread_mutex_unlock(&d->mutex);
            continue;
        }

        // Ring buffer full: wait until consumer drains below 80%
        if (ring_free(d) < d->ring_size / 5) {
            pthread_mutex_lock(&d->mutex);
            while (ring_free(d) < d->ring_size / 2 && d->thread_running && !d->seek_requested) {
                pthread_cond_wait(&d->cond, &d->mutex);
            }
            pthread_mutex_unlock(&d->mutex);
            continue;
        }

        // Read next packet
        int ret = av_read_frame(fmt_ctx, pkt);
        if (ret < 0) {
            if (ret == AVERROR_EOF) {
                d->finished = 1;
                d->decoding = 0;
            } else {
                d->error = 1;
                snprintf(d->error_msg, sizeof(d->error_msg), "read error: %d", ret);
            }
            continue;
        }

        if (pkt->stream_index != d->stream_idx) {
            av_packet_unref(pkt);
            continue;
        }

        ret = avcodec_send_packet(codec_ctx, pkt);
        av_packet_unref(pkt);
        if (ret < 0) continue;

        while (ret >= 0) {
            ret = avcodec_receive_frame(codec_ctx, frame);
            if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF) break;
            if (ret < 0) { d->error = 1; break; }

            // Resample: source rate -> output rate (always 1x, speed applied at playback)
            int out_samples = av_rescale_rnd(
                swr_get_delay(swr, d->src_sample_rate) + frame->nb_samples,
                d->out_rate,
                d->src_sample_rate,
                AV_ROUND_UP
            );

            if (out_samples > max_out_samples) {
                max_out_samples = out_samples + 1024;
                float *newbuf = realloc(resample_buf, max_out_samples * 2 * sizeof(float));
                if (!newbuf) { d->error = 1; break; }
                resample_buf = newbuf;
            }

            uint8_t *out_planes[1] = { (uint8_t *)resample_buf };
            int converted = swr_convert(swr,
                out_planes, out_samples,
                (const uint8_t **)frame->data, frame->nb_samples);

            if (converted <= 0) continue;

            // Write to ring buffer
            int free_frames = ring_free(d);
            int to_write = converted < free_frames ? converted : free_frames;

            for (int i = 0; i < to_write; i++) {
                int wi = (d->ring_write % d->ring_size) * 2;
                d->ring[wi]     = resample_buf[i * 2];
                d->ring[wi + 1] = resample_buf[i * 2 + 1];
                d->ring_write++;
            }

            // Update position based on decoded frame PTS
            if (frame->pts != AV_NOPTS_VALUE) {
                AVStream *st = fmt_ctx->streams[d->stream_idx];
                d->position = frame->pts * av_q2d(st->time_base);
            }

            av_frame_unref(frame);
        }
    }

done:
    free(resample_buf);
    av_packet_free(&pkt);
    av_frame_free(&frame);
    return NULL;
}

// --- Public API ---

ACDeckDecoder *deck_decoder_create(unsigned int output_rate) {
    ACDeckDecoder *d = calloc(1, sizeof(ACDeckDecoder));
    if (!d) return NULL;

    d->out_rate = output_rate;
    d->speed = 1.0;
    d->ring_frac = 0.0;
    d->ring_size = output_rate * DECK_RING_SECONDS;
    d->ring = calloc(d->ring_size * 2, sizeof(float)); // stereo interleaved
    if (!d->ring) {
        free(d);
        return NULL;
    }

    pthread_mutex_init(&d->mutex, NULL);
    pthread_cond_init(&d->cond, NULL);

    return d;
}

int deck_decoder_load(ACDeckDecoder *d, const char *path) {
    if (!d) return -1;

    // Unload any previous file
    deck_decoder_unload(d);

    // Reset state
    d->loaded = 0;
    d->finished = 0;
    d->error = 0;
    d->error_msg[0] = '\0';
    d->title[0] = '\0';
    d->artist[0] = '\0';
    d->position = 0.0;
    d->duration = 0.0;
    d->ring_write = 0;
    d->ring_read = 0;
    d->seek_requested = 0;
    d->speed = 1.0;
    snprintf(d->path, sizeof(d->path), "%s", path);
    free_video_preview(d);

    // Open file
    AVFormatContext *fmt_ctx = NULL;
    int ret = avformat_open_input(&fmt_ctx, path, NULL, NULL);
    if (ret < 0) {
        snprintf(d->error_msg, sizeof(d->error_msg), "cannot open: %s", path);
        d->error = 1;
        return -1;
    }

    ret = avformat_find_stream_info(fmt_ctx, NULL);
    if (ret < 0) {
        snprintf(d->error_msg, sizeof(d->error_msg), "no stream info");
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }

    // Find best audio stream
    int stream_idx = av_find_best_stream(fmt_ctx, AVMEDIA_TYPE_AUDIO, -1, -1, NULL, 0);
    if (stream_idx < 0) {
        snprintf(d->error_msg, sizeof(d->error_msg), "no audio stream");
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }

    AVStream *st = fmt_ctx->streams[stream_idx];
    const AVCodec *codec = avcodec_find_decoder(st->codecpar->codec_id);
    if (!codec) {
        snprintf(d->error_msg, sizeof(d->error_msg), "unsupported codec");
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }

    AVCodecContext *codec_ctx = avcodec_alloc_context3(codec);
    avcodec_parameters_to_context(codec_ctx, st->codecpar);
    ret = avcodec_open2(codec_ctx, codec, NULL);
    if (ret < 0) {
        snprintf(d->error_msg, sizeof(d->error_msg), "codec open failed");
        avcodec_free_context(&codec_ctx);
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }

    // Setup resampler: source format -> stereo float at output rate
    SwrContext *swr = NULL;
    AVChannelLayout out_layout = AV_CHANNEL_LAYOUT_STEREO;
    ret = swr_alloc_set_opts2(&swr,
        &out_layout, AV_SAMPLE_FMT_FLT, d->out_rate,
        &codec_ctx->ch_layout, codec_ctx->sample_fmt, codec_ctx->sample_rate,
        0, NULL);
    if (ret < 0 || !swr) {
        snprintf(d->error_msg, sizeof(d->error_msg), "resampler alloc failed");
        avcodec_free_context(&codec_ctx);
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }
    ret = swr_init(swr);
    if (ret < 0) {
        snprintf(d->error_msg, sizeof(d->error_msg), "resampler init failed");
        swr_free(&swr);
        avcodec_free_context(&codec_ctx);
        avformat_close_input(&fmt_ctx);
        d->error = 1;
        return -1;
    }

    // Store FFmpeg state
    d->fmt_ctx = fmt_ctx;
    d->codec_ctx = codec_ctx;
    d->swr = swr;
    d->stream_idx = stream_idx;
    d->src_sample_rate = codec_ctx->sample_rate;
    d->src_channels = codec_ctx->ch_layout.nb_channels;

    // Duration
    if (fmt_ctx->duration > 0)
        d->duration = (double)fmt_ctx->duration / AV_TIME_BASE;
    else if (st->duration > 0)
        d->duration = st->duration * av_q2d(st->time_base);

    // Metadata
    extract_metadata(d, fmt_ctx);

    // Extract filename as fallback title
    if (d->title[0] == '\0') {
        const char *slash = strrchr(path, '/');
        const char *name = slash ? slash + 1 : path;
        snprintf(d->title, sizeof(d->title), "%s", name);
        // Strip extension
        char *dot = strrchr(d->title, '.');
        if (dot) *dot = '\0';
    }

    d->loaded = 1;
    d->decoding = 0; // paused until play() is called

    // Start decoder thread
    d->thread_running = 1;
    pthread_create(&d->thread, NULL, decoder_thread_fn, d);

    ac_log("[deck] loaded: %s (%s - %s, %.1fs, %dHz %dch)\n",
           path, d->artist, d->title, d->duration,
           d->src_sample_rate, d->src_channels);

    return 0;
}

void deck_decoder_play(ACDeckDecoder *d) {
    if (!d || !d->loaded) return;
    if (d->finished) {
        // Restart from beginning
        d->seek_requested = 1;
        d->seek_target = 0.0;
    }
    d->decoding = 1;
    pthread_mutex_lock(&d->mutex);
    pthread_cond_signal(&d->cond);
    pthread_mutex_unlock(&d->mutex);
}

void deck_decoder_pause(ACDeckDecoder *d) {
    if (!d) return;
    d->decoding = 0;
}

void deck_decoder_seek(ACDeckDecoder *d, double seconds) {
    if (!d || !d->loaded) return;
    if (seconds < 0) seconds = 0;
    if (seconds > d->duration) seconds = d->duration;
    d->seek_target = seconds;
    d->seek_requested = 1;
    // Wake thread if paused
    pthread_mutex_lock(&d->mutex);
    pthread_cond_signal(&d->cond);
    pthread_mutex_unlock(&d->mutex);
}

void deck_decoder_set_speed(ACDeckDecoder *d, double speed) {
    if (!d) return;
    if (speed < -4.0) speed = -4.0;
    if (speed > 4.0) speed = 4.0;
    d->speed = speed;
}

void deck_decoder_unload(ACDeckDecoder *d) {
    if (!d) return;

    // Stop thread
    if (d->thread_running) {
        d->thread_running = 0;
        d->decoding = 0;
        pthread_mutex_lock(&d->mutex);
        pthread_cond_signal(&d->cond);
        pthread_mutex_unlock(&d->mutex);
        pthread_join(d->thread, NULL);
    }

    // Free FFmpeg state
    if (d->swr) {
        SwrContext *swr = (SwrContext *)d->swr;
        swr_free(&swr);
        d->swr = NULL;
    }
    if (d->codec_ctx) {
        AVCodecContext *ctx = (AVCodecContext *)d->codec_ctx;
        avcodec_free_context(&ctx);
        d->codec_ctx = NULL;
    }
    if (d->fmt_ctx) {
        AVFormatContext *fmt = (AVFormatContext *)d->fmt_ctx;
        avformat_close_input(&fmt);
        d->fmt_ctx = NULL;
    }

    // Free peaks
    if (d->peaks) {
        free(d->peaks);
        d->peaks = NULL;
        d->peak_count = 0;
    }
    free_video_preview(d);

    d->loaded = 0;
    d->finished = 0;
    d->decoding = 0;
    d->ring_write = 0;
    d->ring_read = 0;
}

// Generate decimated max-amplitude peaks for the loaded file.
// Opens a SECOND independent FFmpeg context (so it doesn't disturb playback)
// and decodes the entire file once, recording max abs value per chunk.
int deck_decoder_generate_peaks(ACDeckDecoder *d, int target_count) {
    if (!d || !d->loaded || !d->path[0]) return -1;
    if (d->peaks) { free(d->peaks); d->peaks = NULL; d->peak_count = 0; }
    if (target_count <= 0) target_count = 1024;

    AVFormatContext *fmt = NULL;
    if (avformat_open_input(&fmt, d->path, NULL, NULL) < 0) return -1;
    if (avformat_find_stream_info(fmt, NULL) < 0) {
        avformat_close_input(&fmt);
        return -1;
    }
    int sidx = av_find_best_stream(fmt, AVMEDIA_TYPE_AUDIO, -1, -1, NULL, 0);
    if (sidx < 0) { avformat_close_input(&fmt); return -1; }
    AVStream *st = fmt->streams[sidx];
    const AVCodec *codec = avcodec_find_decoder(st->codecpar->codec_id);
    if (!codec) { avformat_close_input(&fmt); return -1; }
    AVCodecContext *cctx = avcodec_alloc_context3(codec);
    avcodec_parameters_to_context(cctx, st->codecpar);
    if (avcodec_open2(cctx, codec, NULL) < 0) {
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }

    // Estimate total samples for chunking
    double duration = d->duration;
    if (duration <= 0) duration = 60.0; // fallback
    int sample_rate = cctx->sample_rate;
    long total_samples = (long)(duration * sample_rate);
    if (total_samples < target_count) total_samples = target_count;
    long samples_per_chunk = total_samples / target_count;
    if (samples_per_chunk < 1) samples_per_chunk = 1;

    d->peaks = (float *)calloc(target_count, sizeof(float));
    if (!d->peaks) {
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }
    d->peak_count = target_count;

    // Setup converter to mono float
    SwrContext *swr = NULL;
    AVChannelLayout out_layout = AV_CHANNEL_LAYOUT_MONO;
    swr_alloc_set_opts2(&swr, &out_layout, AV_SAMPLE_FMT_FLT, sample_rate,
                        &cctx->ch_layout, cctx->sample_fmt, cctx->sample_rate,
                        0, NULL);
    swr_init(swr);

    AVPacket *pkt = av_packet_alloc();
    AVFrame *frame = av_frame_alloc();
    float chunk_max = 0.0f;
    long sample_idx = 0;
    int peak_idx = 0;
    float *outbuf = (float *)malloc(sample_rate * sizeof(float));
    int outbuf_size = sample_rate;

    while (av_read_frame(fmt, pkt) >= 0 && peak_idx < target_count) {
        if (pkt->stream_index != sidx) { av_packet_unref(pkt); continue; }
        if (avcodec_send_packet(cctx, pkt) < 0) { av_packet_unref(pkt); continue; }
        while (avcodec_receive_frame(cctx, frame) >= 0) {
            int max_out = frame->nb_samples + 256;
            if (max_out > outbuf_size) {
                outbuf = (float *)realloc(outbuf, max_out * sizeof(float));
                outbuf_size = max_out;
            }
            uint8_t *out_ptr[1] = { (uint8_t *)outbuf };
            int out_n = swr_convert(swr, out_ptr, max_out,
                                    (const uint8_t **)frame->data, frame->nb_samples);
            for (int i = 0; i < out_n; i++) {
                float v = outbuf[i];
                if (v < 0) v = -v;
                if (v > chunk_max) chunk_max = v;
                sample_idx++;
                if (sample_idx >= samples_per_chunk) {
                    if (peak_idx < target_count) {
                        d->peaks[peak_idx++] = chunk_max;
                    }
                    chunk_max = 0.0f;
                    sample_idx = 0;
                }
            }
        }
        av_packet_unref(pkt);
    }
    // Final chunk
    if (peak_idx < target_count && chunk_max > 0) {
        d->peaks[peak_idx++] = chunk_max;
    }
    // Fill remaining
    while (peak_idx < target_count) d->peaks[peak_idx++] = 0;

    free(outbuf);
    av_frame_free(&frame);
    av_packet_free(&pkt);
    swr_free(&swr);
    avcodec_free_context(&cctx);
    avformat_close_input(&fmt);

    return target_count;
}

int deck_decoder_generate_video_preview(ACDeckDecoder *d, int width, int height, int fps) {
    if (!d || !d->loaded || !d->path[0]) return -1;
    if (width <= 0) width = 96;
    if (height <= 0) height = 54;
    if (fps <= 0) fps = 12;
    if (fps > 24) fps = 24;

    // Reuse cached preview when the request matches the existing buffer.
    if (d->video_ready &&
        d->video_frames &&
        d->video_width == width &&
        d->video_height == height &&
        fabs(d->video_fps - (double)fps) < 0.001) {
        return d->video_frame_count;
    }

    free_video_preview(d);

    AVFormatContext *fmt = NULL;
    if (avformat_open_input(&fmt, d->path, NULL, NULL) < 0) return -1;
    if (avformat_find_stream_info(fmt, NULL) < 0) {
        avformat_close_input(&fmt);
        return -1;
    }

    int sidx = av_find_best_stream(fmt, AVMEDIA_TYPE_VIDEO, -1, -1, NULL, 0);
    if (sidx < 0) {
        avformat_close_input(&fmt);
        return -1;
    }

    AVStream *st = fmt->streams[sidx];
    const AVCodec *codec = avcodec_find_decoder(st->codecpar->codec_id);
    if (!codec) {
        avformat_close_input(&fmt);
        return -1;
    }

    AVCodecContext *cctx = avcodec_alloc_context3(codec);
    if (!cctx) {
        avformat_close_input(&fmt);
        return -1;
    }
    avcodec_parameters_to_context(cctx, st->codecpar);
    if (avcodec_open2(cctx, codec, NULL) < 0) {
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }

    double duration = d->duration;
    if (duration <= 0.0 && fmt->duration > 0)
        duration = (double)fmt->duration / AV_TIME_BASE;
    if (duration <= 0.0 && st->duration > 0)
        duration = st->duration * av_q2d(st->time_base);
    if (duration <= 0.0)
        duration = 30.0;

    int target_count = (int)ceil(duration * (double)fps) + 1;
    if (target_count < 1) target_count = 1;
    if (target_count > 900) target_count = 900;

    size_t pixels_per_frame = (size_t)width * (size_t)height;
    size_t total_pixels = pixels_per_frame * (size_t)target_count;
    uint32_t *frames = (uint32_t *)calloc(total_pixels, sizeof(uint32_t));
    if (!frames) {
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }

    struct SwsContext *sws = sws_getContext(
        cctx->width, cctx->height, cctx->pix_fmt,
        width, height, AV_PIX_FMT_BGRA,
        SWS_BILINEAR, NULL, NULL, NULL);
    if (!sws) {
        free(frames);
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }

    AVPacket *pkt = av_packet_alloc();
    AVFrame *frame = av_frame_alloc();
    if (!pkt || !frame) {
        av_packet_free(&pkt);
        av_frame_free(&frame);
        sws_freeContext(sws);
        free(frames);
        avcodec_free_context(&cctx);
        avformat_close_input(&fmt);
        return -1;
    }

    uint8_t *last_frame = NULL;
    int frame_idx = 0;
    double next_time = 0.0;

    while (av_read_frame(fmt, pkt) >= 0 && frame_idx < target_count) {
        if (pkt->stream_index != sidx) {
            av_packet_unref(pkt);
            continue;
        }
        if (avcodec_send_packet(cctx, pkt) < 0) {
            av_packet_unref(pkt);
            continue;
        }
        av_packet_unref(pkt);

        while (avcodec_receive_frame(cctx, frame) >= 0 && frame_idx < target_count) {
            double frame_time = 0.0;
            if (frame->best_effort_timestamp != AV_NOPTS_VALUE)
                frame_time = frame->best_effort_timestamp * av_q2d(st->time_base);
            else if (frame->pts != AV_NOPTS_VALUE)
                frame_time = frame->pts * av_q2d(st->time_base);

            uint8_t *dst_data[4] = {0};
            int dst_linesize[4] = {0};
            uint32_t *dst = frames + ((size_t)frame_idx * pixels_per_frame);
            av_image_fill_arrays(dst_data, dst_linesize, (uint8_t *)dst,
                                 AV_PIX_FMT_BGRA, width, height, 1);

            // Fill every preview sample point that this decoded frame covers.
            while (frame_idx < target_count &&
                   (frame_time >= next_time || frame_idx == 0)) {
                dst = frames + ((size_t)frame_idx * pixels_per_frame);
                av_image_fill_arrays(dst_data, dst_linesize, (uint8_t *)dst,
                                     AV_PIX_FMT_BGRA, width, height, 1);
                sws_scale(sws, (const uint8_t * const *)frame->data, frame->linesize,
                          0, cctx->height, dst_data, dst_linesize);
                last_frame = (uint8_t *)dst;
                frame_idx++;
                next_time = (double)frame_idx / (double)fps;
            }

            av_frame_unref(frame);
        }
    }

    // Flush decoder for the tail of the file.
    avcodec_send_packet(cctx, NULL);
    while (avcodec_receive_frame(cctx, frame) >= 0 && frame_idx < target_count) {
        uint8_t *dst_data[4] = {0};
        int dst_linesize[4] = {0};
        uint32_t *dst = frames + ((size_t)frame_idx * pixels_per_frame);
        av_image_fill_arrays(dst_data, dst_linesize, (uint8_t *)dst,
                             AV_PIX_FMT_BGRA, width, height, 1);
        sws_scale(sws, (const uint8_t * const *)frame->data, frame->linesize,
                  0, cctx->height, dst_data, dst_linesize);
        last_frame = (uint8_t *)dst;
        frame_idx++;
        av_frame_unref(frame);
    }

    // Pad remaining preview slots with the final decoded frame so playback
    // keeps showing a still image once audio reaches the tail.
    if (last_frame) {
        while (frame_idx < target_count) {
            uint32_t *dst = frames + ((size_t)frame_idx * pixels_per_frame);
            memcpy(dst, last_frame, pixels_per_frame * sizeof(uint32_t));
            frame_idx++;
        }
    }

    av_packet_free(&pkt);
    av_frame_free(&frame);
    sws_freeContext(sws);
    avcodec_free_context(&cctx);
    avformat_close_input(&fmt);

    if (frame_idx <= 0) {
        free(frames);
        return -1;
    }

    d->video_frames = frames;
    d->video_frame_count = frame_idx;
    d->video_width = width;
    d->video_height = height;
    d->video_fps = (double)fps;
    d->video_ready = 1;

    ac_log("[deck] video preview ready: %s (%d frames @ %dx%d %.1ffps)\n",
           d->path, d->video_frame_count, d->video_width, d->video_height, d->video_fps);
    return d->video_frame_count;
}

void deck_decoder_destroy(ACDeckDecoder *d) {
    if (!d) return;
    deck_decoder_unload(d);
    free(d->ring);
    pthread_mutex_destroy(&d->mutex);
    pthread_cond_destroy(&d->cond);
    free(d);
}

#endif // HAVE_AVCODEC
