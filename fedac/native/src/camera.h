#ifndef AC_CAMERA_H
#define AC_CAMERA_H

#include <stdint.h>
#include <pthread.h>

// Forward declare graph for camera_blit
struct ACGraph;

typedef struct {
    int fd;                     // V4L2 device fd (-1 = closed)
    int width, height;          // capture resolution
    uint8_t *buffers[4];        // mmap'd V4L2 buffers
    int buffer_count;
    int streaming;              // 1 = V4L2 streaming active

    // Grayscale frame for QR detection (width * height)
    uint8_t *gray;
    int gray_ready;             // 1 = new frame available

    // Display frame: mutex-protected copy for main thread rendering
    uint8_t *display;           // copy of gray for rendering (width * height)
    pthread_mutex_t display_mu; // protects display buffer
    volatile int display_ready; // 1 = new display frame available

    // QR scan results
    volatile int scan_pending;  // 1 = scan requested
    volatile int scan_done;     // 1 = scan complete (check scan_result)
    char scan_result[2048];     // decoded QR text (empty = no QR found)
    char scan_error[256];       // error message if open/capture fails
} ACCamera;

// Open the first available V4L2 camera (tries /dev/video0-3)
// Returns 0 on success, -1 on failure (check cam->scan_error)
int camera_open(ACCamera *cam);

// Close camera and free resources
void camera_close(ACCamera *cam);

// Grab one frame and convert to grayscale
// Returns 0 on success, -1 on failure
int camera_grab(ACCamera *cam);

// Scan the current grayscale frame for QR codes using quirc
// Result stored in cam->scan_result (empty string = no QR found)
// Returns number of QR codes detected
int camera_scan_qr(ACCamera *cam);

#endif
