// camera.c — V4L2 camera capture + quirc QR code scanning for AC native
// Designed for single-frame grab-and-scan (not continuous streaming).

#include "camera.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <linux/videodev2.h>
#include "../lib/quirc/lib/quirc.h"

#ifdef HAVE_AVCODEC
#include <libavcodec/avcodec.h>
#include <libswscale/swscale.h>
#endif

extern void ac_log(const char *fmt, ...);

// Preferred capture resolution (small = fast QR scan)
#define CAM_W 640
#define CAM_H 480
#define CAM_BUF_COUNT 2

static int xioctl(int fd, int request, void *arg) {
    int r;
    do { r = ioctl(fd, request, arg); }
    while (r == -1 && errno == EINTR);
    return r;
}

int camera_list(char paths[][16], int max) {
    char devpath[16];
    int count = 0;
    for (int i = 0; i < 10 && count < max; i++) {
        snprintf(devpath, sizeof(devpath), "/dev/video%d", i);
        int fd = open(devpath, O_RDWR | O_NONBLOCK);
        if (fd < 0) continue;
        // Capture + streaming caps only. Must use device_caps when the
        // driver sets V4L2_CAP_DEVICE_CAPS: `capabilities` describes the
        // whole physical device, so a UVC *metadata* node also advertises
        // VIDEO_CAPTURE there and would sneak into the list (then fail
        // S_FMT forever — "no cameras found" on a working webcam).
        struct v4l2_capability cap;
        int ok = xioctl(fd, VIDIOC_QUERYCAP, &cap) >= 0;
        if (ok) {
            uint32_t c = (cap.capabilities & V4L2_CAP_DEVICE_CAPS)
                             ? cap.device_caps : cap.capabilities;
            ok = (c & V4L2_CAP_VIDEO_CAPTURE) && (c & V4L2_CAP_STREAMING);
        }
        close(fd);
        if (ok) snprintf(paths[count++], 16, "%s", devpath);
    }
    return count;
}

int camera_open_path(ACCamera *cam, const char *devpath) {
    // Reset per-session state WITHOUT memsetting the whole struct — the
    // display mutex is initialized once and must survive plug-and-play
    // open/close cycles (a memset would corrupt a locked mutex).
    cam->fd = -1;
    cam->width = cam->height = 0;
    cam->pixfmt = 0;
    memset(cam->buffers, 0, sizeof(cam->buffers));
    cam->buffer_count = 0;
    cam->streaming = 0;
    cam->gray = NULL;
    cam->gray_ready = 0;
    cam->display = NULL;
    cam->display_rgb = NULL;
    cam->display_ready = 0;
    cam->display_rgb_ready = 0;
    cam->scan_pending = 0;
    cam->scan_done = 0;
    cam->scan_result[0] = 0;
    cam->scan_error[0] = 0;
    cam->av_ctx = NULL;
    cam->av_frame = NULL;
    cam->av_pkt = NULL;
    cam->sws = NULL;
    if (!cam->display_mu_init) {
        pthread_mutex_init(&cam->display_mu, NULL);
        cam->display_mu_init = 1;
    }

    cam->fd = open(devpath, O_RDWR | O_NONBLOCK);
    if (cam->fd < 0) {
        snprintf(cam->scan_error, sizeof(cam->scan_error),
                 "can't open %s", devpath);
        return -1;
    }
    struct v4l2_capability caps;
    uint32_t ecaps = 0;
    if (xioctl(cam->fd, VIDIOC_QUERYCAP, &caps) >= 0)
        ecaps = (caps.capabilities & V4L2_CAP_DEVICE_CAPS)
                    ? caps.device_caps : caps.capabilities;
    if (!(ecaps & V4L2_CAP_VIDEO_CAPTURE) || !(ecaps & V4L2_CAP_STREAMING)) {
        snprintf(cam->scan_error, sizeof(cam->scan_error),
                 "%s is not a capture device", devpath);
        close(cam->fd);
        cam->fd = -1;
        return -1;
    }
    ac_log("[camera] opened %s\n", devpath);

    // Set format: YUYV (most common for USB webcams), fallback to MJPEG
    struct v4l2_format fmt = {0};
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fmt.fmt.pix.width = CAM_W;
    fmt.fmt.pix.height = CAM_H;
    fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    fmt.fmt.pix.field = V4L2_FIELD_NONE;

    if (xioctl(cam->fd, VIDIOC_S_FMT, &fmt) < 0) {
        // Try MJPEG as fallback
        fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_MJPEG;
        if (xioctl(cam->fd, VIDIOC_S_FMT, &fmt) < 0) {
            snprintf(cam->scan_error, sizeof(cam->scan_error),
                     "can't set camera format");
            close(cam->fd);
            cam->fd = -1;
            return -1;
        }
    }

    cam->width = fmt.fmt.pix.width;
    cam->height = fmt.fmt.pix.height;
    cam->pixfmt = fmt.fmt.pix.pixelformat;
    ac_log("[camera] format: %dx%d pixfmt=0x%08x\n",
           cam->width, cam->height, fmt.fmt.pix.pixelformat);

    // Request buffers
    struct v4l2_requestbuffers req = {0};
    req.count = CAM_BUF_COUNT;
    req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req.memory = V4L2_MEMORY_MMAP;
    if (xioctl(cam->fd, VIDIOC_REQBUFS, &req) < 0 || req.count < 1) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "buffer request failed");
        close(cam->fd);
        cam->fd = -1;
        return -1;
    }
    cam->buffer_count = (int)req.count;
    if (cam->buffer_count > 4) cam->buffer_count = 4;

    // Map buffers and queue them
    for (int i = 0; i < cam->buffer_count; i++) {
        struct v4l2_buffer buf = {0};
        buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        buf.memory = V4L2_MEMORY_MMAP;
        buf.index = i;
        if (xioctl(cam->fd, VIDIOC_QUERYBUF, &buf) < 0) {
            snprintf(cam->scan_error, sizeof(cam->scan_error), "querybuf failed");
            close(cam->fd);
            cam->fd = -1;
            return -1;
        }
        cam->buffers[i] = mmap(NULL, buf.length,
                               PROT_READ | PROT_WRITE, MAP_SHARED,
                               cam->fd, buf.m.offset);
        if (cam->buffers[i] == MAP_FAILED) {
            cam->buffers[i] = NULL;
            snprintf(cam->scan_error, sizeof(cam->scan_error), "mmap failed");
            close(cam->fd);
            cam->fd = -1;
            return -1;
        }
        xioctl(cam->fd, VIDIOC_QBUF, &buf);
    }

    // Start streaming
    enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (xioctl(cam->fd, VIDIOC_STREAMON, &type) < 0) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "streamon failed");
        close(cam->fd);
        cam->fd = -1;
        return -1;
    }
    cam->streaming = 1;

    // Allocate grayscale buffer
    cam->gray = malloc(cam->width * cam->height);
    if (!cam->gray) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "gray alloc failed");
        camera_close(cam);
        return -1;
    }

    // Allocate display buffer (mutex-protected copy for main thread rendering)
    cam->display = malloc(cam->width * cam->height);
    if (!cam->display) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "display alloc failed");
        camera_close(cam);
        return -1;
    }

    // Color display buffer (filled from YUYV directly or decoded MJPEG)
    cam->display_rgb = malloc((size_t)cam->width * cam->height * sizeof(uint32_t));
    if (!cam->display_rgb) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "rgb alloc failed");
        camera_close(cam);
        return -1;
    }
    cam->display_ready = 0;
    cam->display_rgb_ready = 0;

    ac_log("[camera] ready: %dx%d, %d buffers\n",
           cam->width, cam->height, cam->buffer_count);
    return 0;
}

int camera_open(ACCamera *cam) {
    // Highest-numbered device: USB cams enumerate above the built-in
    // webcam, so a freshly plugged camera wins ("switch out cams per shot").
    char paths[10][16];
    int count = camera_list(paths, 10);
    if (count == 0) {
        cam->fd = -1;
        snprintf(cam->scan_error, sizeof(cam->scan_error), "no camera found");
        return -1;
    }
    return camera_open_path(cam, paths[count - 1]);
}

void camera_close(ACCamera *cam) {
    if (cam->streaming && cam->fd >= 0) {
        enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        xioctl(cam->fd, VIDIOC_STREAMOFF, &type);
        cam->streaming = 0;
    }
    // Unmap buffers (we don't track individual sizes, but munmap is safe
    // since we queried them — in practice the kernel handles cleanup on close)
    if (cam->fd >= 0) {
        close(cam->fd);
        cam->fd = -1;
    }
    if (cam->gray) {
        free(cam->gray);
        cam->gray = NULL;
    }
    if (cam->display || cam->display_rgb) {
        // Free the display buffers under the mutex so a concurrent
        // cameraBlit on the main thread can't read freed memory. The
        // mutex itself is never destroyed — it survives reopen cycles.
        pthread_mutex_lock(&cam->display_mu);
        free(cam->display);
        cam->display = NULL;
        free(cam->display_rgb);
        cam->display_rgb = NULL;
        cam->display_ready = 0;
        cam->display_rgb_ready = 0;
        pthread_mutex_unlock(&cam->display_mu);
    }
#ifdef HAVE_AVCODEC
    if (cam->av_ctx) {
        AVCodecContext *ctx = (AVCodecContext *)cam->av_ctx;
        avcodec_free_context(&ctx);
        cam->av_ctx = NULL;
    }
    if (cam->av_frame) {
        AVFrame *frame = (AVFrame *)cam->av_frame;
        av_frame_free(&frame);
        cam->av_frame = NULL;
    }
    if (cam->av_pkt) {
        AVPacket *pkt = (AVPacket *)cam->av_pkt;
        av_packet_free(&pkt);
        cam->av_pkt = NULL;
    }
    if (cam->sws) {
        sws_freeContext((struct SwsContext *)cam->sws);
        cam->sws = NULL;
    }
#endif
    cam->gray_ready = 0;
    cam->display_ready = 0;
    cam->display_rgb_ready = 0;
}

// Decode one MJPEG frame (complete JPEG from V4L2) into the gray +
// display buffers via libavcodec/libswscale. Lazily builds the decoder.
// Returns 0 on success, -1 on decode failure (frame skipped).
static int mjpeg_decode(ACCamera *cam, const uint8_t *data, int size) {
#ifdef HAVE_AVCODEC
    if (size <= 0) return -1;
    if (!cam->av_ctx) {
        const AVCodec *codec = avcodec_find_decoder(AV_CODEC_ID_MJPEG);
        if (!codec) return -1;
        AVCodecContext *ctx = avcodec_alloc_context3(codec);
        if (!ctx) return -1;
        if (avcodec_open2(ctx, codec, NULL) < 0) {
            avcodec_free_context(&ctx);
            return -1;
        }
        cam->av_ctx = ctx;
        cam->av_frame = av_frame_alloc();
        cam->av_pkt = av_packet_alloc();
        if (!cam->av_frame || !cam->av_pkt) return -1;
    }
    AVCodecContext *ctx = (AVCodecContext *)cam->av_ctx;
    AVFrame *frame = (AVFrame *)cam->av_frame;
    AVPacket *pkt = (AVPacket *)cam->av_pkt;

    if (av_new_packet(pkt, size) < 0) return -1;
    memcpy(pkt->data, data, size);
    int rc = avcodec_send_packet(ctx, pkt);
    av_packet_unref(pkt);
    if (rc < 0) return -1;
    if (avcodec_receive_frame(ctx, frame) < 0) return -1;

    if (!cam->sws) {
        cam->sws = sws_getContext(frame->width, frame->height, frame->format,
                                  cam->width, cam->height, AV_PIX_FMT_BGRA,
                                  SWS_BILINEAR, NULL, NULL, NULL);
        if (!cam->sws) return -1;
    }

    pthread_mutex_lock(&cam->display_mu);
    if (cam->display_rgb) {
        uint8_t *dst_data[4] = { (uint8_t *)cam->display_rgb, NULL, NULL, NULL };
        int dst_stride[4] = { cam->width * 4, 0, 0, 0 };
        sws_scale((struct SwsContext *)cam->sws,
                  (const uint8_t *const *)frame->data, frame->linesize,
                  0, frame->height, dst_data, dst_stride);
        cam->display_rgb_ready = 1;
        // Derive gray (for QR) + gray display from the decoded color
        int pixels = cam->width * cam->height;
        if (cam->gray) {
            for (int i = 0; i < pixels; i++) {
                uint32_t p = cam->display_rgb[i];
                cam->gray[i] = (uint8_t)((((p >> 16) & 0xFF) * 77 +
                                          ((p >> 8) & 0xFF) * 150 +
                                          (p & 0xFF) * 29) >> 8);
            }
            cam->gray_ready = 1;
        }
        if (cam->display && cam->gray) {
            memcpy(cam->display, cam->gray, pixels);
            cam->display_ready = 1;
        }
    }
    pthread_mutex_unlock(&cam->display_mu);
    return 0;
#else
    (void)cam; (void)data; (void)size;
    return -1;
#endif
}

int camera_grab(ACCamera *cam) {
    if (cam->fd < 0 || !cam->streaming || !cam->gray) return -1;

    struct v4l2_buffer buf = {0};
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;

    if (xioctl(cam->fd, VIDIOC_DQBUF, &buf) < 0) {
        if (errno == EAGAIN) return -1; // no frame ready yet
        // ENODEV/EIO etc. — device unplugged or wedged; caller should
        // camera_close() and rescan (plug-and-play).
        snprintf(cam->scan_error, sizeof(cam->scan_error),
                 "dqbuf failed: %s", strerror(errno));
        return -2;
    }

    uint8_t *src = cam->buffers[buf.index];
    int pixels = cam->width * cam->height;

    if (cam->pixfmt == V4L2_PIX_FMT_MJPEG) {
        // MJPEG-only USB cams: decode the full JPEG (fills gray + color)
        mjpeg_decode(cam, src, (int)buf.bytesused);
    } else {
        // YUYV: Y channel is every other byte
        for (int i = 0; i < pixels; i++) {
            cam->gray[i] = src[i * 2];
        }
        cam->gray_ready = 1;

        // Copy to display buffers for main thread rendering. The color
        // pass decodes full YUYV → ARGB32 (BT.601): each 4-byte group
        // Y0 U Y1 V yields two pixels sharing chroma.
        if (cam->display) {
            pthread_mutex_lock(&cam->display_mu);
            memcpy(cam->display, cam->gray, pixels);
            cam->display_ready = 1;
            if (cam->display_rgb) {
                uint32_t *dst = cam->display_rgb;
                for (int i = 0; i < pixels; i += 2) {
                    int y0 = src[i * 2 + 0], u = src[i * 2 + 1];
                    int y1 = src[i * 2 + 2], v = src[i * 2 + 3];
                    int d = u - 128, e = v - 128;
                    for (int k = 0; k < 2; k++) {
                        int c = (k ? y1 : y0) - 16;
                        int r = (298 * c + 409 * e + 128) >> 8;
                        int g = (298 * c - 100 * d - 208 * e + 128) >> 8;
                        int b = (298 * c + 516 * d + 128) >> 8;
                        if (r < 0) r = 0; else if (r > 255) r = 255;
                        if (g < 0) g = 0; else if (g > 255) g = 255;
                        if (b < 0) b = 0; else if (b > 255) b = 255;
                        dst[i + k] = 0xFF000000u | ((uint32_t)r << 16) |
                                     ((uint32_t)g << 8) | (uint32_t)b;
                    }
                }
                cam->display_rgb_ready = 1;
            }
            pthread_mutex_unlock(&cam->display_mu);
        }
    }

    // Re-queue the buffer
    xioctl(cam->fd, VIDIOC_QBUF, &buf);
    return 0;
}

int camera_scan_qr(ACCamera *cam) {
    if (!cam->gray || !cam->gray_ready) return 0;

    struct quirc *qr = quirc_new();
    if (!qr) return 0;

    if (quirc_resize(qr, cam->width, cam->height) < 0) {
        quirc_destroy(qr);
        return 0;
    }

    // Copy grayscale into quirc's image buffer
    int w, h;
    uint8_t *image = quirc_begin(qr, &w, &h);
    int copy_w = cam->width < w ? cam->width : w;
    int copy_h = cam->height < h ? cam->height : h;
    for (int y = 0; y < copy_h; y++) {
        memcpy(image + y * w, cam->gray + y * cam->width, copy_w);
    }
    quirc_end(qr);

    int count = quirc_count(qr);
    cam->scan_result[0] = 0;

    for (int i = 0; i < count; i++) {
        struct quirc_code code;
        struct quirc_data data;
        quirc_extract(qr, i, &code);
        if (quirc_decode(&code, &data) == QUIRC_SUCCESS) {
            int len = data.payload_len;
            if (len > (int)sizeof(cam->scan_result) - 1)
                len = sizeof(cam->scan_result) - 1;
            memcpy(cam->scan_result, data.payload, len);
            cam->scan_result[len] = 0;
            ac_log("[camera] QR decoded: %d bytes\n", len);
            break; // take first successful decode
        }
    }

    int found = (cam->scan_result[0] != 0) ? 1 : 0;
    quirc_destroy(qr);
    return found;
}
