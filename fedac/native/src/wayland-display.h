// wayland-display.h — Wayland SHM display backend for ac-native
// Used when running under cage compositor (WAYLAND_DISPLAY set)
#ifndef AC_WAYLAND_DISPLAY_H
#define AC_WAYLAND_DISPLAY_H

#ifdef USE_WAYLAND

#include <stdint.h>
#include <wayland-client.h>
#include "framebuffer.h"

typedef struct {
    struct wl_display *display;
    struct wl_registry *registry;
    struct wl_compositor *compositor;
    struct wl_shm *shm;
    struct wl_seat *seat;
    struct wl_surface *surface;

    // xdg-shell (from generated protocol)
    void *xdg_wm_base;       // struct xdg_wm_base*
    void *xdg_surface;       // struct xdg_surface*
    void *xdg_toplevel;      // struct xdg_toplevel*

    // SHM double buffers
    struct wl_buffer *buffers[2];
    uint32_t *shm_data[2];   // mmap'd pixel data for each buffer
    int buf_busy[2];         // 1 if compositor still using this buffer
    int current_buf;         // which buffer to draw into next
    int buf_width, buf_height;

    // Display dimensions (from configure events)
    int width, height;
    int configured;          // xdg_surface has been configured

    // Input (Wayland seat devices — pointers stored for input.c)
    struct wl_keyboard *keyboard;
    struct wl_pointer *pointer;
    struct wl_touch *touch;
    void *input;             // ACInput* — set by input_init_wayland()
} ACWaylandDisplay;

// Initialize Wayland display — connect to compositor, create surface, allocate SHM buffers
// Returns NULL on failure (not running under Wayland)
ACWaylandDisplay *wayland_display_init(void);

// Present framebuffer to Wayland surface (copies scaled pixels to SHM, commits)
void wayland_display_present(ACWaylandDisplay *wd, ACFramebuffer *screen, int scale);

// Dispatch pending Wayland events (call once per frame)
int wayland_display_dispatch(ACWaylandDisplay *wd);

// Get display dimensions (physical display size)
void wayland_display_get_size(ACWaylandDisplay *wd, int *w, int *h);

// Cleanup
void wayland_display_destroy(ACWaylandDisplay *wd);

#endif // USE_WAYLAND
#endif // AC_WAYLAND_DISPLAY_H
