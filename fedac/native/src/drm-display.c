#include "drm-display.h"
#include "font.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <linux/fb.h>

#ifdef USE_SDL
// ============================================================
// SDL2 GPU-accelerated display (uses KMSDRM backend on bare metal)
// ============================================================

static ACDisplay *sdl_init(void) {
    // Set KMSDRM hints for bare metal (no X11/Wayland)
    if (getpid() == 1) {
        setenv("SDL_VIDEODRIVER", "kmsdrm", 0);
    }

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        fprintf(stderr, "[sdl] SDL_Init failed: %s\n", SDL_GetError());
        return NULL;
    }

    // Get display mode to know the resolution
    SDL_DisplayMode dm;
    if (SDL_GetDesktopDisplayMode(0, &dm) < 0) {
        fprintf(stderr, "[sdl] GetDesktopDisplayMode failed: %s\n", SDL_GetError());
        SDL_Quit();
        return NULL;
    }

    SDL_Window *win = SDL_CreateWindow("ac-native",
        SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
        dm.w, dm.h,
        SDL_WINDOW_FULLSCREEN_DESKTOP | SDL_WINDOW_SHOWN);
    if (!win) {
        fprintf(stderr, "[sdl] CreateWindow failed: %s\n", SDL_GetError());
        SDL_Quit();
        return NULL;
    }

    SDL_ShowCursor(SDL_DISABLE);

    SDL_Renderer *ren = SDL_CreateRenderer(win, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!ren) {
        fprintf(stderr, "[sdl] CreateRenderer (accel) failed: %s, trying software\n", SDL_GetError());
        ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_SOFTWARE);
    }
    if (!ren) {
        fprintf(stderr, "[sdl] CreateRenderer failed: %s\n", SDL_GetError());
        SDL_DestroyWindow(win);
        SDL_Quit();
        return NULL;
    }

    // Log renderer info
    SDL_RendererInfo info;
    SDL_GetRendererInfo(ren, &info);
    fprintf(stderr, "[sdl] Renderer: %s (flags: 0x%x)\n", info.name, info.flags);
    if (info.flags & SDL_RENDERER_ACCELERATED)
        fprintf(stderr, "[sdl] GPU-accelerated rendering active\n");

    // Set nearest-neighbor scaling for pixel-art look
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "0");

    ACDisplay *d = calloc(1, sizeof(ACDisplay));
    d->fd = -1;
    d->is_sdl = 1;
    d->width = dm.w;
    d->height = dm.h;
    d->sdl_window = win;
    d->sdl_renderer = ren;
    // Texture created lazily in display_present (needs framebuffer dimensions)
    d->sdl_texture = NULL;
    d->sdl_tex_w = 0;
    d->sdl_tex_h = 0;

    fprintf(stderr, "[sdl] Ready (%dx%d)\n", d->width, d->height);
    return d;
}
#endif /* USE_SDL */

// ============================================================
// fbdev fallback — uses /dev/fb0 (EFI framebuffer via efifb)
// ============================================================

static ACDisplay *fbdev_init(void) {
    // Try opening fb0 with retries (may take time to appear in devtmpfs)
    int fd = -1;
    for (int attempt = 0; attempt < 100; attempt++) {
        fd = open("/dev/fb0", O_RDWR);
        if (fd >= 0) break;
        usleep(20000); // 20ms
    }
    if (fd < 0) {
        fprintf(stderr, "[fbdev] Cannot open /dev/fb0 after retries\n");
        // List what's in /dev for debugging
        return NULL;
    }

    struct fb_var_screeninfo vinfo;
    struct fb_fix_screeninfo finfo;
    if (ioctl(fd, FBIOGET_VSCREENINFO, &vinfo) < 0 ||
        ioctl(fd, FBIOGET_FSCREENINFO, &finfo) < 0) {
        fprintf(stderr, "[fbdev] Cannot get screen info\n");
        close(fd);
        return NULL;
    }

    fprintf(stderr, "[fbdev] Display: %dx%d, %d bpp, stride %d\n",
            vinfo.xres, vinfo.yres, vinfo.bits_per_pixel, finfo.line_length);
    fprintf(stderr, "[fbdev] RGBA: %d/%d/%d/%d offset %d/%d/%d/%d\n",
            vinfo.red.length, vinfo.green.length, vinfo.blue.length, vinfo.transp.length,
            vinfo.red.offset, vinfo.green.offset, vinfo.blue.offset, vinfo.transp.offset);

    if (vinfo.bits_per_pixel != 32) {
        // Try setting 32bpp
        vinfo.bits_per_pixel = 32;
        vinfo.red.offset = 16; vinfo.red.length = 8;
        vinfo.green.offset = 8; vinfo.green.length = 8;
        vinfo.blue.offset = 0; vinfo.blue.length = 8;
        vinfo.transp.offset = 24; vinfo.transp.length = 8;
        if (ioctl(fd, FBIOPUT_VSCREENINFO, &vinfo) < 0) {
            fprintf(stderr, "[fbdev] Cannot set 32bpp, using native %dbpp\n",
                    vinfo.bits_per_pixel);
            // Re-read after failed set
            ioctl(fd, FBIOGET_VSCREENINFO, &vinfo);
            ioctl(fd, FBIOGET_FSCREENINFO, &finfo);
            if (vinfo.bits_per_pixel != 32) {
                fprintf(stderr, "[fbdev] Unsupported bpp: %d\n", vinfo.bits_per_pixel);
                close(fd);
                return NULL;
            }
        }
        ioctl(fd, FBIOGET_FSCREENINFO, &finfo);
        fprintf(stderr, "[fbdev] Set 32bpp: stride now %d\n", finfo.line_length);
    }

    uint32_t map_size = finfo.line_length * vinfo.yres;
    uint32_t *map = mmap(0, map_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (map == MAP_FAILED) {
        perror("[fbdev] mmap failed");
        close(fd);
        return NULL;
    }

    // Clear to black
    memset(map, 0, map_size);

    // Blank the console cursor / disable console
    int tty_fd = open("/dev/tty0", O_RDWR);
    if (tty_fd >= 0) {
        // KDSETMODE KD_GRAPHICS = 1
        ioctl(tty_fd, 0x4B3A, 1);
        close(tty_fd);
    }

    ACDisplay *d = calloc(1, sizeof(ACDisplay));
    d->fd = fd;
    d->is_fbdev = 1;
    d->width = (int)vinfo.xres;
    d->height = (int)vinfo.yres;
    d->fbdev_map = map;
    d->fbdev_size = map_size;
    d->fbdev_stride = (int)(finfo.line_length / sizeof(uint32_t));

    // Detect if framebuffer is BGR (blue at high offset, red at low)
    // Our internal format is ARGB: A<<24 | R<<16 | G<<8 | B
    // EFI framebuffers are often XBGR: X<<24 | B<<16 | G<<8 | R
    if (vinfo.blue.offset > vinfo.red.offset) {
        d->fbdev_swap_rb = 1;
        fprintf(stderr, "[fbdev] BGR format detected — will swap R/B\n");
    }

    // Use buffers[0].map as the software back buffer (we copy to fbdev_map on flip)
    uint32_t buf_size = (uint32_t)(d->width * d->height * 4);
    d->buffers[0].map = malloc(buf_size);
    d->buffers[0].size = buf_size;
    d->buffers[0].pitch = (uint32_t)(d->width * 4);
    memset(d->buffers[0].map, 0, buf_size);

    fprintf(stderr, "[fbdev] Ready (%dx%d, swap_rb=%d)\n",
            d->width, d->height, d->fbdev_swap_rb);
    return d;
}

// ============================================================
// DRM display
// ============================================================

static int try_open_drm(void) {
    const char *paths[] = {
        "/dev/dri/card0",
        "/dev/dri/card1",
        NULL
    };
    for (int i = 0; paths[i]; i++) {
        int fd = open(paths[i], O_RDWR | O_CLOEXEC);
        if (fd >= 0) {
            if (drmSetMaster(fd) == 0) {
                fprintf(stderr, "[drm] Opened %s\n", paths[i]);
                return fd;
            }
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

    if (drmModeAddFB(d->fd, (uint32_t)d->width, (uint32_t)d->height, 24, 32,
                     create.pitch, create.handle, &d->buffers[idx].fb_id) < 0) {
        perror("[drm] AddFB failed");
        return -1;
    }

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

    memset(d->buffers[idx].map, 0, create.size);
    return 0;
}

ACDisplay *drm_init(void) {
#ifdef USE_SDL
    ACDisplay *sdl = sdl_init();
    if (sdl) return sdl;
    fprintf(stderr, "[drm] SDL2 failed, falling back to DRM dumb buffers\n");
#endif

    ACDisplay *d = calloc(1, sizeof(ACDisplay));
    if (!d) return NULL;

    d->fd = try_open_drm();
    if (d->fd < 0) {
        fprintf(stderr, "[drm] No DRM device found, trying fbdev...\n");
        free(d);
        return fbdev_init();
    }

    uint64_t has_dumb;
    if (drmGetCap(d->fd, DRM_CAP_DUMB_BUFFER, &has_dumb) < 0 || !has_dumb) {
        fprintf(stderr, "[drm] No dumb buffer support, trying fbdev...\n");
        close(d->fd);
        free(d);
        return fbdev_init();
    }

    drmModeRes *res = drmModeGetResources(d->fd);
    if (!res) {
        fprintf(stderr, "[drm] No resources, trying fbdev...\n");
        close(d->fd);
        free(d);
        return fbdev_init();
    }

    // Prefer internal panel (eDP/LVDS) over external (HDMI/DP) to avoid slow EDID probes
    drmModeConnector *conn = NULL;
    // First pass: look for internal panel
    for (int i = 0; i < res->count_connectors; i++) {
        drmModeConnector *c = drmModeGetConnector(d->fd, res->connectors[i]);
        if (!c) continue;
        if (c->connection == DRM_MODE_CONNECTED && c->count_modes > 0 &&
            (c->connector_type == DRM_MODE_CONNECTOR_eDP ||
             c->connector_type == DRM_MODE_CONNECTOR_LVDS ||
             c->connector_type == DRM_MODE_CONNECTOR_DSI)) {
            conn = c;
            fprintf(stderr, "[drm] Using internal panel (type %d)\n", c->connector_type);
            break;
        }
        drmModeFreeConnector(c);
    }
    // Second pass: any connected display
    if (!conn) {
        for (int i = 0; i < res->count_connectors; i++) {
            conn = drmModeGetConnector(d->fd, res->connectors[i]);
            if (conn && conn->connection == DRM_MODE_CONNECTED && conn->count_modes > 0)
                break;
            if (conn) drmModeFreeConnector(conn);
            conn = NULL;
        }
    }

    if (!conn) {
        fprintf(stderr, "[drm] No connected display, trying fbdev...\n");
        drmModeFreeResources(res);
        close(d->fd);
        free(d);
        return fbdev_init();
    }

    d->connector_id = conn->connector_id;
    d->mode = conn->modes[0];
    d->width = d->mode.hdisplay;
    d->height = d->mode.vdisplay;
    fprintf(stderr, "[drm] Display: %dx%d @ %dHz\n",
            d->width, d->height, d->mode.vrefresh);

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
        fprintf(stderr, "[drm] No encoder, trying fbdev...\n");
        drmModeFreeConnector(conn);
        drmModeFreeResources(res);
        close(d->fd);
        free(d);
        return fbdev_init();
    }

    d->crtc_id = enc->crtc_id;
    if (!d->crtc_id) {
        for (int i = 0; i < res->count_crtcs; i++) {
            if (enc->possible_crtcs & (1u << i)) {
                d->crtc_id = res->crtcs[i];
                break;
            }
        }
    }

    d->saved_crtc = drmModeGetCrtc(d->fd, d->crtc_id);

    drmModeFreeEncoder(enc);
    drmModeFreeConnector(conn);
    drmModeFreeResources(res);

    if (create_dumb_buffer(d, 0) < 0 || create_dumb_buffer(d, 1) < 0) {
        drm_destroy(d);
        return fbdev_init();
    }

    d->front = 0;

    if (drmModeSetCrtc(d->fd, d->crtc_id, d->buffers[0].fb_id, 0, 0,
                       &d->connector_id, 1, &d->mode) < 0) {
        perror("[drm] SetCrtc failed, trying fbdev...");
        drm_destroy(d);
        return fbdev_init();
    }

    fprintf(stderr, "[drm] Ready\n");
    return d;
}

void drm_flip(ACDisplay *d) {
    if (d->is_fbdev) {
        uint32_t *src = d->buffers[0].map;
        uint32_t *dst = d->fbdev_map;
        int src_stride = d->width;
        int dst_stride = d->fbdev_stride;
        if (d->fbdev_swap_rb) {
            // Convert ARGB → ABGR (swap R and B channels)
            for (int y = 0; y < d->height; y++) {
                uint32_t *s = src + y * src_stride;
                uint32_t *dd = dst + y * dst_stride;
                for (int x = 0; x < d->width; x++) {
                    uint32_t p = s[x];
                    dd[x] = (p & 0xFF00FF00u) |       // keep A and G
                            ((p & 0x00FF0000u) >> 16) | // R → B
                            ((p & 0x000000FFu) << 16);  // B → R
                }
            }
        } else {
            for (int y = 0; y < d->height; y++) {
                memcpy(dst + y * dst_stride, src + y * src_stride, (size_t)(d->width * 4));
            }
        }
        return;
    }
    int back = 1 - d->front;
    // Page flip synchronized to vblank (prevents tearing)
    if (drmModePageFlip(d->fd, d->crtc_id, d->buffers[back].fb_id,
                        DRM_MODE_PAGE_FLIP_EVENT, d) == 0) {
        // Wait for the flip event (blocks until vblank)
        fd_set fds;
        FD_ZERO(&fds);
        FD_SET(d->fd, &fds);
        struct timeval tv = { .tv_sec = 0, .tv_usec = 50000 }; // 50ms timeout
        if (select(d->fd + 1, &fds, NULL, NULL, &tv) > 0) {
            drmEventContext ev = {
                .version = 2,
                .page_flip_handler = NULL, // we just need to consume the event
            };
            drmHandleEvent(d->fd, &ev);
        }
    } else {
        // Fallback to SetCrtc if page flip not supported
        drmModeSetCrtc(d->fd, d->crtc_id, d->buffers[back].fb_id, 0, 0,
                       &d->connector_id, 1, &d->mode);
    }
    d->front = back;
}

uint32_t *drm_back_buffer(ACDisplay *d) {
    if (d->is_fbdev) {
        return d->buffers[0].map;
    }
    return d->buffers[1 - d->front].map;
}

int drm_back_stride(ACDisplay *d) {
    if (d->is_fbdev) {
        return d->width;
    }
    return (int)(d->buffers[1 - d->front].pitch / sizeof(uint32_t));
}

uint32_t *drm_front_buffer(ACDisplay *d) {
    if (d->is_fbdev) return d->fbdev_map;
    return d->buffers[d->front].map;
}

int drm_front_stride(ACDisplay *d) {
    if (d->is_fbdev) return d->fbdev_stride;
    return (int)(d->buffers[d->front].pitch / sizeof(uint32_t));
}

void display_present(ACDisplay *d, ACFramebuffer *screen, int scale) {
    if (!d || !screen) return;

#ifdef USE_SDL
    if (d->is_sdl) {
        // Create/recreate texture if framebuffer size changed
        if (!d->sdl_texture || d->sdl_tex_w != screen->width || d->sdl_tex_h != screen->height) {
            if (d->sdl_texture) SDL_DestroyTexture(d->sdl_texture);
            d->sdl_texture = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
                screen->width, screen->height);
            d->sdl_tex_w = screen->width;
            d->sdl_tex_h = screen->height;
            fprintf(stderr, "[sdl] Created texture %dx%d\n", screen->width, screen->height);
        }
        // Upload pixels to GPU texture
        SDL_UpdateTexture(d->sdl_texture, NULL,
                          screen->pixels, screen->stride * (int)sizeof(uint32_t));
        // GPU-accelerated scale to fullscreen
        SDL_RenderClear(d->sdl_renderer);
        SDL_RenderCopy(d->sdl_renderer, d->sdl_texture, NULL, NULL);
        SDL_RenderPresent(d->sdl_renderer);
        return;
    }
#endif
    (void)scale;
    // CPU fallback: scale to back buffer and flip
    fb_copy_scaled(screen, drm_back_buffer(d),
                   d->width, d->height, drm_back_stride(d), scale);
    drm_flip(d);
}

// ============================================================
// Secondary HDMI display — solid color fill
// ============================================================

ACSecondaryDisplay *drm_init_secondary(ACDisplay *primary) {
    if (!primary || primary->is_fbdev || primary->fd < 0) return NULL;
#ifdef USE_SDL
    if (primary->is_sdl) return NULL;
#endif

    drmModeRes *res = drmModeGetResources(primary->fd);
    if (!res) return NULL;

    drmModeConnector *conn = NULL;
    for (int i = 0; i < res->count_connectors; i++) {
        drmModeConnector *c = drmModeGetConnector(primary->fd, res->connectors[i]);
        if (!c) continue;
        if (c->connector_id == primary->connector_id) { drmModeFreeConnector(c); continue; }
        if (c->connection == DRM_MODE_CONNECTED && c->count_modes > 0 &&
            (c->connector_type == DRM_MODE_CONNECTOR_HDMIA ||
             c->connector_type == DRM_MODE_CONNECTOR_HDMIB ||
             c->connector_type == DRM_MODE_CONNECTOR_DisplayPort)) {
            conn = c;
            break;
        }
        drmModeFreeConnector(c);
    }
    drmModeFreeResources(res);

    if (!conn) {
        fprintf(stderr, "[drm-secondary] No HDMI/DP display found\n");
        return NULL;
    }

    ACSecondaryDisplay *s = calloc(1, sizeof(ACSecondaryDisplay));
    s->fd = primary->fd;
    s->connector_id = conn->connector_id;
    s->mode = conn->modes[0];
    s->width = s->mode.hdisplay;
    s->height = s->mode.vdisplay;
    fprintf(stderr, "[drm-secondary] HDMI: %dx%d @ %dHz\n",
            s->width, s->height, s->mode.vrefresh);

    drmModeEncoder *enc = NULL;
    if (conn->encoder_id) enc = drmModeGetEncoder(primary->fd, conn->encoder_id);
    if (!enc) {
        for (int i = 0; i < conn->count_encoders; i++) {
            enc = drmModeGetEncoder(primary->fd, conn->encoders[i]);
            if (enc && enc->crtc_id != primary->crtc_id) break;
            if (enc) { drmModeFreeEncoder(enc); enc = NULL; }
        }
    }
    drmModeFreeConnector(conn);

    if (!enc) { fprintf(stderr, "[drm-secondary] No encoder\n"); free(s); return NULL; }

    s->crtc_id = enc->crtc_id;
    if (!s->crtc_id) {
        drmModeRes *r2 = drmModeGetResources(primary->fd);
        if (r2) {
            for (int i = 0; i < r2->count_crtcs; i++) {
                if (r2->crtcs[i] != primary->crtc_id && (enc->possible_crtcs & (1 << i))) {
                    s->crtc_id = r2->crtcs[i];
                    break;
                }
            }
            drmModeFreeResources(r2);
        }
    }
    drmModeFreeEncoder(enc);

    if (!s->crtc_id) { fprintf(stderr, "[drm-secondary] No free CRTC\n"); free(s); return NULL; }

    s->saved_crtc = drmModeGetCrtc(primary->fd, s->crtc_id);

    // Allocate two dumb buffers for double-buffering
    for (int b = 0; b < 2; b++) {
        struct drm_mode_create_dumb create = { .width = s->width, .height = s->height, .bpp = 32 };
        if (drmIoctl(s->fd, DRM_IOCTL_MODE_CREATE_DUMB, &create) < 0) {
            fprintf(stderr, "[drm-secondary] Create dumb buffer %d failed\n", b); free(s); return NULL;
        }
        s->bufs[b].handle = create.handle;
        s->bufs[b].pitch  = create.pitch;
        s->bufs[b].size   = create.size;

        if (drmModeAddFB(s->fd, s->width, s->height, 24, 32,
                         s->bufs[b].pitch, s->bufs[b].handle, &s->bufs[b].fb_id) < 0) {
            fprintf(stderr, "[drm-secondary] AddFB %d failed\n", b);
            struct drm_mode_destroy_dumb destroy = { .handle = s->bufs[b].handle };
            drmIoctl(s->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
            free(s); return NULL;
        }

        struct drm_mode_map_dumb map_req = { .handle = s->bufs[b].handle };
        if (drmIoctl(s->fd, DRM_IOCTL_MODE_MAP_DUMB, &map_req) < 0) {
            fprintf(stderr, "[drm-secondary] Map %d failed\n", b);
            drmModeRmFB(s->fd, s->bufs[b].fb_id);
            struct drm_mode_destroy_dumb destroy = { .handle = s->bufs[b].handle };
            drmIoctl(s->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
            free(s); return NULL;
        }
        s->bufs[b].map = mmap(0, s->bufs[b].size, PROT_READ | PROT_WRITE,
                               MAP_SHARED, s->fd, map_req.offset);
        if (s->bufs[b].map == MAP_FAILED) {
            fprintf(stderr, "[drm-secondary] mmap %d failed\n", b);
            drmModeRmFB(s->fd, s->bufs[b].fb_id);
            struct drm_mode_destroy_dumb destroy = { .handle = s->bufs[b].handle };
            drmIoctl(s->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
            free(s); return NULL;
        }
        // Clear to black
        memset(s->bufs[b].map, 0, s->bufs[b].size);
    }

    s->buf_front = 0;
    if (drmModeSetCrtc(s->fd, s->crtc_id, s->bufs[0].fb_id, 0, 0,
                       &s->connector_id, 1, &s->mode) < 0) {
        fprintf(stderr, "[drm-secondary] SetCrtc failed\n");
        for (int b = 0; b < 2; b++) {
            munmap(s->bufs[b].map, s->bufs[b].size);
            drmModeRmFB(s->fd, s->bufs[b].fb_id);
            struct drm_mode_destroy_dumb destroy = { .handle = s->bufs[b].handle };
            drmIoctl(s->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
        }
        free(s); return NULL;
    }

    // Small internal render target (1/8 native res for fast CPU rendering)
    int sw = (s->width  + 7) / 8;
    int sh = (s->height + 7) / 8;
    s->small_fb = fb_create(sw, sh);
    if (!s->small_fb) {
        fprintf(stderr, "[drm-secondary] fb_create small failed\n");
        // non-fatal — will fall back to full-res if NULL
    }

    s->active = 1;
    fprintf(stderr, "[drm-secondary] HDMI output active %dx%d (small %dx%d)\n",
            s->width, s->height, sw, sh);
    return s;
}

int drm_secondary_is_connected(ACDisplay *primary) {
    if (!primary || primary->is_fbdev || primary->fd < 0) return 0;
    drmModeRes *res = drmModeGetResources(primary->fd);
    if (!res) return 0;
    int found = 0;
    for (int i = 0; i < res->count_connectors && !found; i++) {
        drmModeConnector *c = drmModeGetConnector(primary->fd, res->connectors[i]);
        if (!c) continue;
        if (c->connector_id != primary->connector_id &&
            c->connection == DRM_MODE_CONNECTED && c->count_modes > 0 &&
            (c->connector_type == DRM_MODE_CONNECTOR_HDMIA ||
             c->connector_type == DRM_MODE_CONNECTOR_HDMIB ||
             c->connector_type == DRM_MODE_CONNECTOR_DisplayPort))
            found = 1;
        drmModeFreeConnector(c);
    }
    drmModeFreeResources(res);
    return found;
}

void drm_secondary_present_waveform(ACSecondaryDisplay *s, ACGraph *g,
                                     float *waveform, int wf_size, int wf_pos) {
    if (!s || !s->active || !g) return;

    // Use small_fb as the render target (1/8 native res — fast)
    ACFramebuffer *render_fb = s->small_fb;
    if (!render_fb) return; // nothing to render into

    int rw = render_fb->width;
    int rh = render_fb->height;

    // Save graph's original target and switch to small buffer
    ACFramebuffer *orig_target = g->fb;
    graph_page(g, render_fb);

    // Dark background
    graph_wipe(g, (ACColor){8, 8, 16, 255});

    // Draw waveform at small-buffer dimensions — two-tone style (bars + thick line)
    if (waveform) {
        int N = rw; // one column per pixel
        int cy = rh / 2;

        // Pre-compute sample positions
        int xs[N], ys[N];
        for (int i = 0; i < N; i++) {
            int idx = (wf_pos + wf_size - N + i) % wf_size;
            float sample = waveform[idx];
            xs[i] = i;
            ys[i] = cy - (int)(sample * rh * 0.44f);
        }

        // Pass 1: vertical bars from center to waveform (dim, opaque fill)
        graph_ink(g, (ACColor){30, 90, 160, 255});
        for (int i = 0; i < N; i++) {
            int top = ys[i] < cy ? ys[i] : cy;
            int bot = ys[i] >= cy ? ys[i] : cy;
            int bh = bot - top + 1;
            if (bh < 1) bh = 1;
            graph_box(g, xs[i], top, 1, bh, 1);
        }

        // Pass 2: bright connecting line, 3px thick (draw at y-1, y, y+1)
        for (int dy = -1; dy <= 1; dy++) {
            graph_ink(g, dy == 0
                ? (ACColor){100, 200, 255, 255}   // center: brightest
                : (ACColor){60, 160, 220, 180});   // edge: dimmer
            int prev_x = xs[0], prev_y = ys[0] + dy;
            for (int i = 1; i < N; i++) {
                graph_line(g, prev_x, prev_y, xs[i], ys[i] + dy);
                prev_x = xs[i];
                prev_y = ys[i] + dy;
            }
        }
    }

    // Resolution text (shows native res) at small scale
    char res_str[32];
    snprintf(res_str, sizeof(res_str), "%dx%d", s->width, s->height);
    graph_ink(g, (ACColor){160, 160, 180, 255});
    font_draw(g, res_str, 2, 2, 1);

    // Restore original render target
    graph_page(g, orig_target);

    // Scale small_fb up to HDMI back buffer
    int back = 1 - s->buf_front;
    int dst_stride = (int)(s->bufs[back].pitch / sizeof(uint32_t));
    fb_copy_scaled(render_fb, s->bufs[back].map, s->width, s->height, dst_stride, 8);

    // Async page flip
    int ret = drmModePageFlip(s->fd, s->crtc_id, s->bufs[back].fb_id,
                               DRM_MODE_PAGE_FLIP_ASYNC, NULL);
    if (ret != 0) {
        drmModeSetCrtc(s->fd, s->crtc_id, s->bufs[back].fb_id, 0, 0,
                       &s->connector_id, 1, &s->mode);
    }
    s->buf_front = back;
}

void drm_secondary_fill(ACSecondaryDisplay *s, uint8_t r, uint8_t g, uint8_t b) {
    if (!s || !s->active || !s->bufs[0].map) return;
    uint32_t pixel = ((uint32_t)r << 16) | ((uint32_t)g << 8) | (uint32_t)b;
    int back = 1 - s->buf_front;
    uint32_t *p = s->bufs[back].map;
    int total = s->width * s->height;
    for (int i = 0; i < total; i++) p[i] = pixel;
    drmModeSetCrtc(s->fd, s->crtc_id, s->bufs[back].fb_id, 0, 0,
                   &s->connector_id, 1, &s->mode);
    s->buf_front = back;
}

void drm_secondary_destroy(ACSecondaryDisplay *s) {
    if (!s) return;
    if (s->saved_crtc) {
        drmModeSetCrtc(s->fd, s->saved_crtc->crtc_id, s->saved_crtc->buffer_id,
                       s->saved_crtc->x, s->saved_crtc->y,
                       &s->connector_id, 1, &s->saved_crtc->mode);
        drmModeFreeCrtc(s->saved_crtc);
    }
    if (s->small_fb) fb_destroy(s->small_fb);
    for (int b = 0; b < 2; b++) {
        if (s->bufs[b].map && s->bufs[b].map != MAP_FAILED)
            munmap(s->bufs[b].map, s->bufs[b].size);
        if (s->bufs[b].fb_id) drmModeRmFB(s->fd, s->bufs[b].fb_id);
        if (s->bufs[b].handle) {
            struct drm_mode_destroy_dumb destroy = { .handle = s->bufs[b].handle };
            drmIoctl(s->fd, DRM_IOCTL_MODE_DESTROY_DUMB, &destroy);
        }
    }
    free(s);
}

void drm_destroy(ACDisplay *d) {
    if (!d) return;

#ifdef USE_SDL
    if (d->is_sdl) {
        if (d->sdl_texture) SDL_DestroyTexture(d->sdl_texture);
        if (d->sdl_renderer) SDL_DestroyRenderer(d->sdl_renderer);
        if (d->sdl_window) SDL_DestroyWindow(d->sdl_window);
        SDL_Quit();
        free(d);
        return;
    }
#endif

    if (d->is_fbdev) {
        // Restore console text mode
        int tty_fd = open("/dev/tty0", O_RDWR);
        if (tty_fd >= 0) {
            ioctl(tty_fd, 0x4B3A, 0); // KD_TEXT
            close(tty_fd);
        }
        if (d->fbdev_map && d->fbdev_map != MAP_FAILED)
            munmap(d->fbdev_map, d->fbdev_size);
        if (d->buffers[0].map)
            free(d->buffers[0].map);
        if (d->fd >= 0) close(d->fd);
        free(d);
        return;
    }

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
