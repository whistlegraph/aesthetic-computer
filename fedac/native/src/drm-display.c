#include "drm-display.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

static int try_open_drm(void) {
    // Try common DRM device paths
    const char *paths[] = {
        "/dev/dri/card0",
        "/dev/dri/card1",
        NULL
    };
    for (int i = 0; paths[i]; i++) {
        int fd = open(paths[i], O_RDWR | O_CLOEXEC);
        if (fd >= 0) {
            // Check if we can become DRM master
            if (drmSetMaster(fd) == 0) {
                fprintf(stderr, "[drm] Opened %s\n", paths[i]);
                return fd;
            }
            // Try without master (might work for dumb buffers)
            fprintf(stderr, "[drm] Opened %s (no master)\n", paths[i]);
            return fd;
        }
    }
    return -1;
}

static int create_dumb_buffer(ACDisplay *d, int idx) {
    struct drm_mode_create_dumb create = {
        .width = (uint32_t)d->width,
        .height = (uint32_t)d->height,
        .bpp = 32,
    };

    if (drmIoctl(d->fd, DRM_IOCTL_MODE_CREATE_DUMB, &create) < 0) {
        perror("[drm] Create dumb buffer failed");
        return -1;
    }

    d->buffers[idx].handle = create.handle;
    d->buffers[idx].pitch = create.pitch;
    d->buffers[idx].size = create.size;

    // Add framebuffer
    if (drmModeAddFB(d->fd, (uint32_t)d->width, (uint32_t)d->height, 24, 32,
                     create.pitch, create.handle, &d->buffers[idx].fb_id) < 0) {
        perror("[drm] AddFB failed");
        return -1;
    }

    // Map buffer
    struct drm_mode_map_dumb map = { .handle = create.handle };
    if (drmIoctl(d->fd, DRM_IOCTL_MODE_MAP_DUMB, &map) < 0) {
        perror("[drm] Map dumb buffer failed");
        return -1;
    }

    d->buffers[idx].map = mmap(0, create.size, PROT_READ | PROT_WRITE,
                                MAP_SHARED, d->fd, map.offset);
    if (d->buffers[idx].map == MAP_FAILED) {
        perror("[drm] mmap failed");
        return -1;
    }

    // Clear to black
    memset(d->buffers[idx].map, 0, create.size);

    return 0;
}

ACDisplay *drm_init(void) {
    ACDisplay *d = calloc(1, sizeof(ACDisplay));
    if (!d) return NULL;

    d->fd = try_open_drm();
    if (d->fd < 0) {
        fprintf(stderr, "[drm] No DRM device found\n");
        free(d);
        return NULL;
    }

    // Check dumb buffer capability
    uint64_t has_dumb;
    if (drmGetCap(d->fd, DRM_CAP_DUMB_BUFFER, &has_dumb) < 0 || !has_dumb) {
        fprintf(stderr, "[drm] No dumb buffer support\n");
        close(d->fd);
        free(d);
        return NULL;
    }

    // Get resources
    drmModeRes *res = drmModeGetResources(d->fd);
    if (!res) {
        fprintf(stderr, "[drm] No resources\n");
        close(d->fd);
        free(d);
        return NULL;
    }

    // Find connected connector
    drmModeConnector *conn = NULL;
    for (int i = 0; i < res->count_connectors; i++) {
        conn = drmModeGetConnector(d->fd, res->connectors[i]);
        if (conn && conn->connection == DRM_MODE_CONNECTED && conn->count_modes > 0)
            break;
        if (conn) drmModeFreeConnector(conn);
        conn = NULL;
    }

    if (!conn) {
        fprintf(stderr, "[drm] No connected display found\n");
        drmModeFreeResources(res);
        close(d->fd);
        free(d);
        return NULL;
    }

    d->connector_id = conn->connector_id;

    // Use preferred mode (first mode is usually native/preferred)
    d->mode = conn->modes[0];
    d->width = d->mode.hdisplay;
    d->height = d->mode.vdisplay;
    fprintf(stderr, "[drm] Display: %dx%d @ %dHz\n",
            d->width, d->height, d->mode.vrefresh);

    // Find encoder + CRTC
    drmModeEncoder *enc = NULL;
    if (conn->encoder_id) {
        enc = drmModeGetEncoder(d->fd, conn->encoder_id);
    }
    if (!enc) {
        for (int i = 0; i < conn->count_encoders; i++) {
            enc = drmModeGetEncoder(d->fd, conn->encoders[i]);
            if (enc) break;
        }
    }
    if (!enc) {
        fprintf(stderr, "[drm] No encoder\n");
        drmModeFreeConnector(conn);
        drmModeFreeResources(res);
        close(d->fd);
        free(d);
        return NULL;
    }

    d->crtc_id = enc->crtc_id;
    if (!d->crtc_id) {
        // Find available CRTC
        for (int i = 0; i < res->count_crtcs; i++) {
            if (enc->possible_crtcs & (1u << i)) {
                d->crtc_id = res->crtcs[i];
                break;
            }
        }
    }

    // Save current CRTC for restore on exit
    d->saved_crtc = drmModeGetCrtc(d->fd, d->crtc_id);

    drmModeFreeEncoder(enc);
    drmModeFreeConnector(conn);
    drmModeFreeResources(res);

    // Create double buffers
    if (create_dumb_buffer(d, 0) < 0 || create_dumb_buffer(d, 1) < 0) {
        drm_destroy(d);
        return NULL;
    }

    d->front = 0;

    // Set initial mode
    if (drmModeSetCrtc(d->fd, d->crtc_id, d->buffers[0].fb_id, 0, 0,
                       &d->connector_id, 1, &d->mode) < 0) {
        perror("[drm] SetCrtc failed");
        drm_destroy(d);
        return NULL;
    }

    fprintf(stderr, "[drm] Ready\n");
    return d;
}

void drm_flip(ACDisplay *d) {
    int back = 1 - d->front;
    drmModeSetCrtc(d->fd, d->crtc_id, d->buffers[back].fb_id, 0, 0,
                   &d->connector_id, 1, &d->mode);
    d->front = back;
}

uint32_t *drm_back_buffer(ACDisplay *d) {
    return d->buffers[1 - d->front].map;
}

int drm_back_stride(ACDisplay *d) {
    return (int)(d->buffers[1 - d->front].pitch / sizeof(uint32_t));
}

void drm_destroy(ACDisplay *d) {
    if (!d) return;

    // Restore saved CRTC
    if (d->saved_crtc) {
        drmModeSetCrtc(d->fd, d->saved_crtc->crtc_id, d->saved_crtc->buffer_id,
                       d->saved_crtc->x, d->saved_crtc->y,
                       &d->connector_id, 1, &d->saved_crtc->mode);
        drmModeFreeCrtc(d->saved_crtc);
    }

    for (int i = 0; i < 2; i++) {
        if (d->buffers[i].map && d->buffers[i].map != MAP_FAILED)
            munmap(d->buffers[i].map, d->buffers[i].size);
        if (d->buffers[i].fb_id)
            drmModeRmFB(d->fd, d->buffers[i].fb_id);
        if (d->buffers[i].handle) {
            struct drm_mode_destroy_dumb destroy = { .handle = d->buffers[i].handle };
            drmIoctl(d->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
        }
    }

    if (d->fd >= 0) close(d->fd);
    free(d);
}
