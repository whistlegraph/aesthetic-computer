#ifndef AC_CAMERA_H
#define AC_CAMERA_H

#include <stdint.h>
#include <pthread.h>

// Forward declare graph for camera_blit
struct ACGraph;

typedef struct {
    int fd;                     // V4L2 device fd (-1 = closed)
    int width, height;          // capture resolution
    uint32_t pixfmt;            // negotiated V4L2 pixel format (YUYV or MJPEG)
    uint8_t *buffers[4];        // mmap'd V4L2 buffers
    int buffer_count;
    int streaming;              // 1 = V4L2 streaming active

    // Grayscale frame for QR detection (width * height)
    uint8_t *gray;
    int gray_ready;             // 1 = new frame available

    // Display frame: mutex-protected copy for main thread rendering
    uint8_t *display;           // copy of gray for rendering (width * height)
    pthread_mutex_t display_mu; // protects display buffers
    volatile int display_ready; // 1 = new display frame available

    // Color display frame (ARGB32, width * height) — filled from YUYV (or
    // decoded MJPEG when ffmpeg is available); the 'cap' piece renders this.
    // Guarded by display_mu alongside the grayscale copy.
    uint32_t *display_rgb;
    volatile int display_rgb_ready;

    // display_mu is initialized once and survives open/close cycles so a
    // renderer holding the lock never races a plug-and-play reopen.
    int display_mu_init;

    // MJPEG decode state (libavcodec/libswscale, only under HAVE_AVCODEC).
    // void* keeps ffmpeg headers out of this file.
    void *av_ctx;               // AVCodecContext*
    void *av_frame;             // AVFrame*
    void *av_pkt;               // AVPacket*
    void *sws;                  // SwsContext*

    // QR scan results
    volatile int scan_pending;  // 1 = scan requested
    volatile int scan_done;     // 1 = scan complete (check scan_result)
    char scan_result[2048];     // decoded QR text (empty = no QR found)
    char scan_error[256];       // error message if open/capture fails
} ACCamera;

// Open a V4L2 camera (scans /dev/video0-9, prefers the highest-numbered
// capture device so a freshly plugged USB cam wins over the built-in).
// Returns 0 on success, -1 on failure (check cam->scan_error)
int camera_open(ACCamera *cam);

// Close camera and free resources (safe to reopen afterwards)
void camera_close(ACCamera *cam);

// Grab one frame into the gray + display buffers.
// Returns 0 on success, -1 if no frame is ready yet (EAGAIN),
// -2 on a fatal device error (unplugged) — close and rescan.
int camera_grab(ACCamera *cam);

// Scan the current grayscale frame for QR codes using quirc
// Result stored in cam->scan_result (empty string = no QR found)
// Returns number of QR codes detected
int camera_scan_qr(ACCamera *cam);

#endif
