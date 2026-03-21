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

int camera_open(ACCamera *cam) {
    memset(cam, 0, sizeof(*cam));
    cam->fd = -1;

    // Try /dev/video0 through /dev/video3
    char devpath[32];
    for (int i = 0; i < 4; i++) {
        snprintf(devpath, sizeof(devpath), "/dev/video%d", i);
        int fd = open(devpath, O_RDWR | O_NONBLOCK);
        if (fd < 0) continue;

        // Check it's a capture device
        struct v4l2_capability cap;
        if (xioctl(fd, VIDIOC_QUERYCAP, &cap) < 0 ||
            !(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE) ||
            !(cap.capabilities & V4L2_CAP_STREAMING)) {
            close(fd);
            continue;
        }

        cam->fd = fd;
        ac_log("[camera] opened %s (%s)\n", devpath, cap.card);
        break;
    }

    if (cam->fd < 0) {
        snprintf(cam->scan_error, sizeof(cam->scan_error), "no camera found");
        return -1;
    }

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
    pthread_mutex_init(&cam->display_mu, NULL);
    cam->display_ready = 0;

    ac_log("[camera] ready: %dx%d, %d buffers\n",
           cam->width, cam->height, cam->buffer_count);
    return 0;
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
    if (cam->display) {
        pthread_mutex_destroy(&cam->display_mu);
        free(cam->display);
        cam->display = NULL;
    }
    cam->gray_ready = 0;
    cam->display_ready = 0;
}

int camera_grab(ACCamera *cam) {
    if (cam->fd < 0 || !cam->streaming || !cam->gray) return -1;

    struct v4l2_buffer buf = {0};
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;

    if (xioctl(cam->fd, VIDIOC_DQBUF, &buf) < 0) {
        if (errno == EAGAIN) return -1; // no frame ready yet
        snprintf(cam->scan_error, sizeof(cam->scan_error),
                 "dqbuf failed: %s", strerror(errno));
        return -1;
    }

    // Convert YUYV to grayscale (Y channel = every other byte)
    uint8_t *src = cam->buffers[buf.index];
    int pixels = cam->width * cam->height;
    for (int i = 0; i < pixels; i++) {
        cam->gray[i] = src[i * 2]; // Y from YUYV
    }
    cam->gray_ready = 1;

    // Copy to display buffer for main thread rendering
    if (cam->display) {
        pthread_mutex_lock(&cam->display_mu);
        memcpy(cam->display, cam->gray, cam->width * cam->height);
        cam->display_ready = 1;
        pthread_mutex_unlock(&cam->display_mu);
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
