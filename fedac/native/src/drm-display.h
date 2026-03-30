#ifndef AC_DRM_DISPLAY_H
#define AC_DRM_DISPLAY_H

#include <stdint.h>
#include <xf86drm.h>
#include <xf86drmMode.h>
#include "framebuffer.h"
#include "graph.h"

#ifdef USE_SDL
#include <SDL3/SDL.h>
#endif

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

    // SDL3 GPU-accelerated display
#ifdef USE_SDL
    int is_sdl;                 // 1 if using SDL3 backend
    SDL_Window *sdl_window;
    SDL_Renderer *sdl_renderer;
    SDL_Texture *sdl_texture;
    int sdl_tex_w, sdl_tex_h;  // texture dimensions (matches small framebuffer)
    char sdl_renderer_name[32]; // renderer driver name (e.g. "opengl", "vulkan")
#endif
} ACDisplay;

// Get display driver name ("sdl3:opengl", "drm", "fbdev", "wayland")
const char *drm_display_driver(ACDisplay *d);

// Initialize display (tries SDL3 GPU first if available, then DRM, then fbdev)
ACDisplay *drm_init(void);

// Present the small framebuffer scaled up to the display
// This is the primary display function — handles GPU texture upload or CPU scaling
void display_present(ACDisplay *d, ACFramebuffer *screen, int scale);

// Flip to the back buffer (makes it visible) — used by non-SDL3 paths
void drm_flip(ACDisplay *d);

// Get pointer to the back buffer (the one to draw into)
uint32_t *drm_back_buffer(ACDisplay *d);
int drm_back_stride(ACDisplay *d);

// Get pointer to the front buffer (currently displayed — writes are immediate)
uint32_t *drm_front_buffer(ACDisplay *d);
int drm_front_stride(ACDisplay *d);

// Secondary display (HDMI out) — double-buffered, low-res internal render
typedef struct {
    int active;
    int fd;
    uint32_t connector_id;
    uint32_t crtc_id;
    drmModeModeInfo mode;
    int width, height;
    struct {
        uint32_t handle, fb_id, pitch, size;
        uint32_t *map;
    } bufs[2];              // Double buffers (back/front)
    int buf_front;          // Which buffer is currently displayed
    drmModeCrtc *saved_crtc;
    ACFramebuffer *small_fb; // 1/8-scale internal render target (fast)
} ACSecondaryDisplay;

// Initialize secondary HDMI display (call after drm_init, returns NULL if no HDMI)
ACSecondaryDisplay *drm_init_secondary(ACDisplay *primary);

// Check if an HDMI/DP connector (other than primary) is connected
int drm_secondary_is_connected(ACDisplay *primary);

// Render waveform + resolution text to HDMI back buffer and flip
void drm_secondary_present_waveform(ACSecondaryDisplay *s, ACGraph *g,
                                     float *waveform, int wf_size, int wf_pos);

// Fill secondary display with solid color (legacy)
void drm_secondary_fill(ACSecondaryDisplay *s, uint8_t r, uint8_t g, uint8_t b);

// Cleanup secondary display
void drm_secondary_destroy(ACSecondaryDisplay *s);

// Cleanup
void drm_destroy(ACDisplay *d);

#endif
