#ifndef AC_DRM_DISPLAY_H
#define AC_DRM_DISPLAY_H

#include <stdint.h>
#include <xf86drm.h>
#include <xf86drmMode.h>

typedef struct {
    int fd;                     // DRM or fbdev device fd
    uint32_t connector_id;
    uint32_t crtc_id;
    uint32_t encoder_id;
    drmModeModeInfo mode;       // Display mode (resolution, refresh)
    int width, height;

    // Double buffering
    struct {
        uint32_t handle;
        uint32_t fb_id;
        uint32_t pitch;
        uint32_t size;
        uint32_t *map;          // mmap'd pixel buffer
    } buffers[2];
    int front;                  // Currently displayed buffer index

    // Saved CRTC for cleanup
    drmModeCrtc *saved_crtc;

    // fbdev fallback
    int is_fbdev;               // 1 if using /dev/fb0 instead of DRM
    uint32_t *fbdev_map;        // fbdev mmap
    uint32_t fbdev_size;        // fbdev map size
    int fbdev_stride;           // fbdev line length in pixels
    int fbdev_swap_rb;          // 1 if need to swap R and B channels (BGR format)
} ACDisplay;

// Initialize DRM display, returns NULL on failure
ACDisplay *drm_init(void);

// Flip to the back buffer (makes it visible)
void drm_flip(ACDisplay *d);

// Get pointer to the back buffer (the one to draw into)
uint32_t *drm_back_buffer(ACDisplay *d);
int drm_back_stride(ACDisplay *d);

// Cleanup
void drm_destroy(ACDisplay *d);

#endif
