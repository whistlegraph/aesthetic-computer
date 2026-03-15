// wayland-display.c — Wayland SHM display backend for ac-native
// Renders to wl_shm buffers under a Wayland compositor (cage).
// Pixel format: ARGB32 matches WL_SHM_FORMAT_ARGB8888 — direct copy.

#ifdef USE_WAYLAND

#define _GNU_SOURCE  // for memfd_create
#include "wayland-display.h"
#include "framebuffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <poll.h>
#include <wayland-client.h>
#include "xdg-shell-client-protocol.h"

// ── xdg_wm_base listener ──

static void xdg_wm_base_ping(void *data, struct xdg_wm_base *base, uint32_t serial) {
    (void)data;
    xdg_wm_base_pong(base, serial);
}

static const struct xdg_wm_base_listener xdg_wm_base_listener = {
    .ping = xdg_wm_base_ping,
};

// ── xdg_surface listener ──

static void xdg_surface_configure(void *data, struct xdg_surface *surface, uint32_t serial) {
    ACWaylandDisplay *wd = data;
    xdg_surface_ack_configure(surface, serial);
    wd->configured = 1;
}

static const struct xdg_surface_listener xdg_surface_listener = {
    .configure = xdg_surface_configure,
};

// ── xdg_toplevel listener ──

static void xdg_toplevel_configure(void *data, struct xdg_toplevel *toplevel,
                                    int32_t width, int32_t height,
                                    struct wl_array *states) {
    (void)toplevel;
    (void)states;
    ACWaylandDisplay *wd = data;
    if (width > 0 && height > 0) {
        wd->width = width;
        wd->height = height;
    }
}

static void xdg_toplevel_close(void *data, struct xdg_toplevel *toplevel) {
    (void)data;
    (void)toplevel;
    // cage kiosk — close means compositor wants us to exit
    fprintf(stderr, "[wayland] toplevel close requested\n");
}

static const struct xdg_toplevel_listener xdg_toplevel_listener = {
    .configure = xdg_toplevel_configure,
    .close = xdg_toplevel_close,
};

// ── wl_buffer listener ──

static void buffer_release(void *data, struct wl_buffer *buffer) {
    ACWaylandDisplay *wd = data;
    for (int i = 0; i < 2; i++) {
        if (wd->buffers[i] == buffer) {
            wd->buf_busy[i] = 0;
            break;
        }
    }
}

static const struct wl_buffer_listener buffer_listener = {
    .release = buffer_release,
};

// ── Registry listener ──

static void registry_global(void *data, struct wl_registry *registry,
                             uint32_t name, const char *interface, uint32_t version) {
    ACWaylandDisplay *wd = data;
    (void)version;

    if (strcmp(interface, wl_compositor_interface.name) == 0) {
        wd->compositor = wl_registry_bind(registry, name, &wl_compositor_interface, 4);
    } else if (strcmp(interface, wl_shm_interface.name) == 0) {
        wd->shm = wl_registry_bind(registry, name, &wl_shm_interface, 1);
    } else if (strcmp(interface, xdg_wm_base_interface.name) == 0) {
        wd->xdg_wm_base = wl_registry_bind(registry, name, &xdg_wm_base_interface, 1);
        xdg_wm_base_add_listener(wd->xdg_wm_base, &xdg_wm_base_listener, wd);
    } else if (strcmp(interface, wl_seat_interface.name) == 0) {
        wd->seat = wl_registry_bind(registry, name, &wl_seat_interface, 5);
    }
}

static void registry_global_remove(void *data, struct wl_registry *registry, uint32_t name) {
    (void)data; (void)registry; (void)name;
}

static const struct wl_registry_listener registry_listener = {
    .global = registry_global,
    .global_remove = registry_global_remove,
};

// ── SHM buffer allocation ──

static int create_shm_buffers(ACWaylandDisplay *wd, int width, int height) {
    int stride = width * 4;  // ARGB32 = 4 bytes per pixel
    int buf_size = stride * height;
    int pool_size = buf_size * 2;  // double buffer

    // Create anonymous shared memory
    int fd = memfd_create("ac-wl-shm", MFD_CLOEXEC);
    if (fd < 0) {
        perror("[wayland] memfd_create");
        return -1;
    }
    if (ftruncate(fd, pool_size) < 0) {
        perror("[wayland] ftruncate");
        close(fd);
        return -1;
    }

    uint8_t *data = mmap(NULL, pool_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (data == MAP_FAILED) {
        perror("[wayland] mmap");
        close(fd);
        return -1;
    }

    struct wl_shm_pool *pool = wl_shm_create_pool(wd->shm, fd, pool_size);
    close(fd);  // pool keeps reference

    for (int i = 0; i < 2; i++) {
        wd->shm_data[i] = (uint32_t *)(data + i * buf_size);
        wd->buffers[i] = wl_shm_pool_create_buffer(pool, i * buf_size,
                                                      width, height, stride,
                                                      WL_SHM_FORMAT_ARGB8888);
        wl_buffer_add_listener(wd->buffers[i], &buffer_listener, wd);
        wd->buf_busy[i] = 0;
    }

    wl_shm_pool_destroy(pool);  // buffers keep reference

    wd->buf_width = width;
    wd->buf_height = height;
    wd->current_buf = 0;

    return 0;
}

// ── Public API ──

ACWaylandDisplay *wayland_display_init(void) {
    ACWaylandDisplay *wd = calloc(1, sizeof(ACWaylandDisplay));
    if (!wd) return NULL;

    // Connect to Wayland compositor
    wd->display = wl_display_connect(NULL);
    if (!wd->display) {
        fprintf(stderr, "[wayland] Cannot connect to Wayland compositor\n");
        free(wd);
        return NULL;
    }

    // Get registry and bind globals
    wd->registry = wl_display_get_registry(wd->display);
    wl_registry_add_listener(wd->registry, &registry_listener, wd);
    wl_display_roundtrip(wd->display);

    if (!wd->compositor || !wd->shm || !wd->xdg_wm_base) {
        fprintf(stderr, "[wayland] Missing required globals (compositor=%p shm=%p xdg=%p)\n",
                (void *)wd->compositor, (void *)wd->shm, (void *)wd->xdg_wm_base);
        wayland_display_destroy(wd);
        return NULL;
    }

    // Create surface
    wd->surface = wl_compositor_create_surface(wd->compositor);
    if (!wd->surface) {
        fprintf(stderr, "[wayland] Failed to create surface\n");
        wayland_display_destroy(wd);
        return NULL;
    }

    // Create xdg_surface and xdg_toplevel
    struct xdg_wm_base *xdg_base = wd->xdg_wm_base;
    struct xdg_surface *xsurf = xdg_wm_base_get_xdg_surface(xdg_base, wd->surface);
    wd->xdg_surface = xsurf;
    xdg_surface_add_listener(xsurf, &xdg_surface_listener, wd);

    struct xdg_toplevel *xtop = xdg_surface_get_toplevel(xsurf);
    wd->xdg_toplevel = xtop;
    xdg_toplevel_add_listener(xtop, &xdg_toplevel_listener, wd);
    xdg_toplevel_set_title(xtop, "ac-native");
    xdg_toplevel_set_fullscreen(xtop, NULL);

    // Commit surface to trigger configure
    wl_surface_commit(wd->surface);
    wl_display_roundtrip(wd->display);

    // Wait for configure event
    int retries = 0;
    while (!wd->configured && retries < 50) {
        wl_display_roundtrip(wd->display);
        retries++;
    }

    if (!wd->configured) {
        fprintf(stderr, "[wayland] Never received configure (proceeding with defaults)\n");
    }

    // Default to 1920x1080 if compositor didn't specify size
    if (wd->width <= 0) wd->width = 1920;
    if (wd->height <= 0) wd->height = 1080;

    fprintf(stderr, "[wayland] Display: %dx%d\n", wd->width, wd->height);

    // Create SHM buffers at full display resolution
    if (create_shm_buffers(wd, wd->width, wd->height) < 0) {
        wayland_display_destroy(wd);
        return NULL;
    }

    fprintf(stderr, "[wayland] Initialized: %dx%d double-buffered SHM\n",
            wd->width, wd->height);

    return wd;
}

void wayland_display_present(ACWaylandDisplay *wd, ACFramebuffer *screen, int scale) {
    if (!wd || !wd->surface || !screen) return;

    // Pick a non-busy buffer
    int buf = wd->current_buf;
    if (wd->buf_busy[buf]) {
        buf = 1 - buf;
        if (wd->buf_busy[buf]) {
            // Both busy — do a blocking dispatch to wait for release
            wl_display_dispatch(wd->display);
            // If still busy after dispatch, try the other
            if (wd->buf_busy[buf]) {
                buf = 1 - buf;
                if (wd->buf_busy[buf]) return;  // drop frame
            }
        }
    }

    // Scale the small framebuffer into the SHM buffer
    uint32_t *dst = wd->shm_data[buf];
    int dst_w = wd->buf_width;
    int dst_h = wd->buf_height;

    if (scale <= 1) {
        // Direct copy (1:1)
        int copy_w = screen->width < dst_w ? screen->width : dst_w;
        int copy_h = screen->height < dst_h ? screen->height : dst_h;
        for (int y = 0; y < copy_h; y++) {
            memcpy(dst + y * dst_w, screen->pixels + y * screen->stride, copy_w * 4);
        }
    } else {
        // Nearest-neighbor scale up (same as fb_copy_scaled)
        int src_w = screen->width;
        int src_h = screen->height;
        for (int sy = 0; sy < src_h; sy++) {
            int dy_start = sy * scale;
            if (dy_start >= dst_h) break;
            int dy_end = dy_start + scale;
            if (dy_end > dst_h) dy_end = dst_h;

            // Scale one source row into first destination row
            uint32_t *src_row = screen->pixels + sy * screen->stride;
            uint32_t *dst_row = dst + dy_start * dst_w;
            for (int sx = 0; sx < src_w; sx++) {
                int dx_start = sx * scale;
                if (dx_start >= dst_w) break;
                int dx_end = dx_start + scale;
                if (dx_end > dst_w) dx_end = dst_w;
                uint32_t pixel = src_row[sx];
                for (int dx = dx_start; dx < dx_end; dx++)
                    dst_row[dx] = pixel;
            }
            // Duplicate scaled row for remaining rows in this block
            int row_bytes = dst_w * 4;
            for (int dy = dy_start + 1; dy < dy_end; dy++) {
                memcpy(dst + dy * dst_w, dst_row, row_bytes);
            }
        }
    }

    // Attach buffer and commit
    wl_surface_attach(wd->surface, wd->buffers[buf], 0, 0);
    wl_surface_damage_buffer(wd->surface, 0, 0, dst_w, dst_h);
    wl_surface_commit(wd->surface);

    // Flush to ensure compositor sees this frame immediately
    wl_display_flush(wd->display);

    wd->buf_busy[buf] = 1;
    wd->current_buf = 1 - buf;
}

int wayland_display_dispatch(ACWaylandDisplay *wd) {
    if (!wd || !wd->display) return -1;

    // Prepare read — ensures we can check for events on the fd
    while (wl_display_prepare_read(wd->display) != 0) {
        wl_display_dispatch_pending(wd->display);
    }

    wl_display_flush(wd->display);

    // Non-blocking check for events on the Wayland fd
    struct pollfd pfd = { .fd = wl_display_get_fd(wd->display), .events = POLLIN };
    if (poll(&pfd, 1, 0) > 0) {
        wl_display_read_events(wd->display);
    } else {
        wl_display_cancel_read(wd->display);
    }

    return wl_display_dispatch_pending(wd->display);
}

void wayland_display_get_size(ACWaylandDisplay *wd, int *w, int *h) {
    if (wd) {
        if (w) *w = wd->width;
        if (h) *h = wd->height;
    }
}

void wayland_display_destroy(ACWaylandDisplay *wd) {
    if (!wd) return;

    for (int i = 0; i < 2; i++) {
        if (wd->buffers[i]) wl_buffer_destroy(wd->buffers[i]);
    }
    // SHM data is mmap'd — unmap the pool region
    if (wd->shm_data[0]) {
        int buf_size = wd->buf_width * wd->buf_height * 4;
        munmap(wd->shm_data[0], buf_size * 2);
    }

    if (wd->xdg_toplevel) xdg_toplevel_destroy(wd->xdg_toplevel);
    if (wd->xdg_surface) xdg_surface_destroy(wd->xdg_surface);
    if (wd->surface) wl_surface_destroy(wd->surface);

    if (wd->keyboard) wl_keyboard_destroy(wd->keyboard);
    if (wd->pointer) wl_pointer_destroy(wd->pointer);
    if (wd->touch) wl_touch_destroy(wd->touch);
    if (wd->seat) wl_seat_destroy(wd->seat);

    if (wd->xdg_wm_base) xdg_wm_base_destroy(wd->xdg_wm_base);
    if (wd->shm) wl_shm_destroy(wd->shm);
    if (wd->compositor) wl_compositor_destroy(wd->compositor);
    if (wd->registry) wl_registry_destroy(wd->registry);

    if (wd->display) wl_display_disconnect(wd->display);

    free(wd);
}

#endif // USE_WAYLAND
