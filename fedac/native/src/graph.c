#include "graph.h"
#include "qrcodegen.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef USE_SDL
#include "drm-display.h"
#endif

static inline uint8_t clamp_u8(int v) {
    if (v < 0) return 0;
    if (v > 255) return 255;
    return (uint8_t)v;
}

void graph_init(ACGraph *g, ACFramebuffer *screen) {
    g->fb = screen;
    g->screen = screen;
    g->ink = (ACColor){255, 255, 255, 255};
    g->ink_packed = 0xFFFFFFFF;
    g->gpu_display = NULL;
}

void graph_init_gpu(ACGraph *g, void *display) {
    g->gpu_display = display;
}

void graph_wipe(ACGraph *g, ACColor color) {
    fb_clear(g->fb, color_pack(color));
}

void graph_ink(ACGraph *g, ACColor color) {
    g->ink = color;
    g->ink_packed = color_pack(color);
}

void graph_plot(ACGraph *g, int x, int y) {
    if (g->ink.a == 255)
        fb_put_pixel(g->fb, x, y, g->ink_packed);
    else
        fb_blend_pixel(g->fb, x, y, g->ink_packed);
}

// Bresenham's line algorithm
void graph_line(ACGraph *g, int x0, int y0, int x1, int y1) {
    int dx = abs(x1 - x0);
    int dy = -abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1;
    int sy = y0 < y1 ? 1 : -1;
    int err = dx + dy;

    for (;;) {
        graph_plot(g, x0, y0);
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 >= dy) { err += dy; x0 += sx; }
        if (e2 <= dx) { err += dx; y0 += sy; }
    }
}

// Thick line: draw filled circles along the Bresenham path
void graph_line_thick(ACGraph *g, int x0, int y0, int x1, int y1, int thickness) {
    if (thickness <= 1) { graph_line(g, x0, y0, x1, y1); return; }
    int r = (thickness - 1) / 2;
    int dx = abs(x1 - x0);
    int dy = -abs(y1 - y0);
    int sx = x0 < x1 ? 1 : -1;
    int sy = y0 < y1 ? 1 : -1;
    int err = dx + dy;
    for (;;) {
        graph_circle(g, x0, y0, r, 1);
        if (x0 == x1 && y0 == y1) break;
        int e2 = 2 * err;
        if (e2 >= dy) { err += dy; x0 += sx; }
        if (e2 <= dx) { err += dx; y0 += sy; }
    }
}

void graph_box(ACGraph *g, int x, int y, int w, int h, int filled) {
    if (filled) {
        // Clip to framebuffer bounds once
        int x0 = x < 0 ? 0 : x;
        int y0 = y < 0 ? 0 : y;
        int x1 = x + w > g->fb->width ? g->fb->width : x + w;
        int y1 = y + h > g->fb->height ? g->fb->height : y + h;
        if (x0 >= x1 || y0 >= y1) return;
        uint32_t color = g->ink_packed;
        uint8_t sa = g->ink.a;
        if (sa == 255) {
            // Opaque fast path — memset-style
            int span = x1 - x0;
            for (int row = y0; row < y1; row++) {
                uint32_t *dst = &g->fb->pixels[row * g->fb->stride + x0];
                for (int i = 0; i < span; i++) dst[i] = color;
            }
        } else if (sa > 0) {
            // Alpha blend — no per-pixel bounds check
            uint8_t sr = (color >> 16) & 0xFF;
            uint8_t sg_c = (color >> 8) & 0xFF;
            uint8_t sb = color & 0xFF;
            uint16_t inv = 255 - sa;
            for (int row = y0; row < y1; row++) {
                uint32_t *dst = &g->fb->pixels[row * g->fb->stride + x0];
                for (int col = x0; col < x1; col++, dst++) {
                    uint32_t d = *dst;
                    uint8_t r = (sr * sa + ((d >> 16) & 0xFF) * inv) / 255;
                    uint8_t gc = (sg_c * sa + ((d >> 8) & 0xFF) * inv) / 255;
                    uint8_t b = (sb * sa + (d & 0xFF) * inv) / 255;
                    *dst = (255u << 24) | ((uint32_t)r << 16) | ((uint32_t)gc << 8) | b;
                }
            }
        }
    } else {
        // Outline rectangle
        graph_line(g, x, y, x + w - 1, y);
        graph_line(g, x + w - 1, y, x + w - 1, y + h - 1);
        graph_line(g, x + w - 1, y + h - 1, x, y + h - 1);
        graph_line(g, x, y + h - 1, x, y);
    }
}

// Midpoint circle algorithm
void graph_circle(ACGraph *g, int cx, int cy, int r, int filled) {
    int x = 0, y = r;
    int d = 1 - r;

    while (x <= y) {
        if (filled) {
            // Draw horizontal spans for filled circle
            for (int i = cx - x; i <= cx + x; i++) {
                graph_plot(g, i, cy + y);
                graph_plot(g, i, cy - y);
            }
            for (int i = cx - y; i <= cx + y; i++) {
                graph_plot(g, i, cy + x);
                graph_plot(g, i, cy - x);
            }
        } else {
            // Draw 8 symmetric points
            graph_plot(g, cx + x, cy + y);
            graph_plot(g, cx - x, cy + y);
            graph_plot(g, cx + x, cy - y);
            graph_plot(g, cx - x, cy - y);
            graph_plot(g, cx + y, cy + x);
            graph_plot(g, cx - y, cy + x);
            graph_plot(g, cx + y, cy - x);
            graph_plot(g, cx - y, cy - x);
        }
        if (d < 0) {
            d += 2 * x + 3;
        } else {
            d += 2 * (x - y) + 5;
            y--;
        }
        x++;
    }
}

void graph_scroll(ACGraph *g, int dx, int dy) {
    ACFramebuffer *fb = g->fb;
    size_t buf_size = (size_t)fb->width * fb->height * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    for (int y = 0; y < fb->height; y++) {
        int src_y = y - dy;
        // Wrap
        while (src_y < 0) src_y += fb->height;
        while (src_y >= fb->height) src_y -= fb->height;

        for (int x = 0; x < fb->width; x++) {
            int src_x = x - dx;
            while (src_x < 0) src_x += fb->width;
            while (src_x >= fb->width) src_x -= fb->width;

            fb->pixels[y * fb->stride + x] = tmp[src_y * fb->stride + src_x];
        }
    }
    free(tmp);
}

void graph_blur(ACGraph *g, int strength) {
    ACFramebuffer *fb = g->fb;
    if (strength <= 0) return;

#ifdef USE_SDL
    // GPU path: downscale → upscale with bilinear filtering = fast blur
    if (g->gpu_display) {
        ACDisplay *d = (ACDisplay *)g->gpu_display;
        if (d->is_sdl && d->sdl_renderer) {
            // Divisor controls blur amount: higher = blurrier
            int divisor = strength + 1;
            if (divisor > 8) divisor = 8;
            int small_w = fb->width / divisor;
            int small_h = fb->height / divisor;
            if (small_w < 1) small_w = 1;
            if (small_h < 1) small_h = 1;

            SDL_Texture *src = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
                fb->width, fb->height);
            SDL_Texture *small = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET,
                small_w, small_h);
            SDL_Texture *result = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET,
                fb->width, fb->height);

            if (src && small && result) {
                // Upload framebuffer → source texture
                SDL_UpdateTexture(src, NULL, fb->pixels,
                                  fb->stride * (int)sizeof(uint32_t));
                // Set bilinear filtering for smooth blur
                SDL_SetTextureScaleMode(src, SDL_SCALEMODE_LINEAR);
                SDL_SetTextureScaleMode(small, SDL_SCALEMODE_LINEAR);

                // Pass 1: downscale (src → small)
                SDL_SetRenderTarget(d->sdl_renderer, small);
                SDL_RenderTexture(d->sdl_renderer, src, NULL, NULL);

                // Pass 2: upscale (small → result)
                SDL_SetRenderTarget(d->sdl_renderer, result);
                SDL_RenderTexture(d->sdl_renderer, small, NULL, NULL);

                // Read back from result render target
                SDL_Rect full = {0, 0, fb->width, fb->height};
                SDL_Surface *rsurf = SDL_RenderReadPixels(d->sdl_renderer, &full);
                SDL_SetRenderTarget(d->sdl_renderer, NULL);
                if (rsurf) {
                    const uint32_t *sp = (const uint32_t *)rsurf->pixels;
                    int spitch = rsurf->pitch / 4;
                    for (int y = 0; y < fb->height && y < rsurf->h; y++) {
                        memcpy(&fb->pixels[y * fb->stride],
                               &sp[y * spitch],
                               (size_t)fb->width * sizeof(uint32_t));
                    }
                    SDL_DestroySurface(rsurf);
                }
            }
            if (src) SDL_DestroyTexture(src);
            if (small) SDL_DestroyTexture(small);
            if (result) SDL_DestroyTexture(result);
            return;
        }
    }
#endif

    // CPU fallback: box blur
    size_t buf_size = (size_t)fb->width * fb->height * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    int radius = strength;
    for (int y = 0; y < fb->height; y++) {
        for (int x = 0; x < fb->width; x++) {
            int r = 0, gr = 0, b = 0, count = 0;
            for (int dy = -radius; dy <= radius; dy++) {
                for (int dx = -radius; dx <= radius; dx++) {
                    int sx = x + dx, sy = y + dy;
                    if (sx >= 0 && sx < fb->width && sy >= 0 && sy < fb->height) {
                        uint32_t p = tmp[sy * fb->stride + sx];
                        r += (p >> 16) & 0xFF;
                        gr += (p >> 8) & 0xFF;
                        b += p & 0xFF;
                        count++;
                    }
                }
            }
            fb->pixels[y * fb->stride + x] = (0xFFu << 24) |
                (((uint32_t)(r / count)) << 16) |
                (((uint32_t)(gr / count)) << 8) |
                (uint32_t)(b / count);
        }
    }
    free(tmp);
}

void graph_zoom(ACGraph *g, double level) {
    ACFramebuffer *fb = g->fb;
    if (!fb || level <= 0.0 || fabs(level - 1.0) < 1e-6) return;

#ifdef USE_SDL
    // GPU path: render texture with scaled src rect for zoom
    if (g->gpu_display) {
        ACDisplay *d = (ACDisplay *)g->gpu_display;
        if (d->is_sdl && d->sdl_renderer) {
            const int w = fb->width;
            const int h = fb->height;
            const float inv = (float)(1.0 / level);

            SDL_Texture *src = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h);
            SDL_Texture *dst = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET, w, h);

            if (src && dst) {
                SDL_UpdateTexture(src, NULL, fb->pixels,
                                  fb->stride * (int)sizeof(uint32_t));
                SDL_SetTextureScaleMode(src, SDL_SCALEMODE_NEAREST);

                // Source rect: the portion of the texture visible after zoom
                float crop_w = w * inv;
                float crop_h = h * inv;
                SDL_FRect srcrect = {
                    (w - crop_w) * 0.5f, (h - crop_h) * 0.5f,
                    crop_w, crop_h
                };
                SDL_FRect dstrect = {0, 0, (float)w, (float)h};

                SDL_SetRenderTarget(d->sdl_renderer, dst);
                SDL_SetRenderDrawColor(d->sdl_renderer, 0, 0, 0, 255);
                SDL_RenderClear(d->sdl_renderer);
                SDL_RenderTexture(d->sdl_renderer, src, &srcrect, &dstrect);

                // Read back
                SDL_Rect full = {0, 0, w, h};
                SDL_Surface *rsurf = SDL_RenderReadPixels(d->sdl_renderer, &full);
                SDL_SetRenderTarget(d->sdl_renderer, NULL);
                if (rsurf) {
                    const uint32_t *sp = (const uint32_t *)rsurf->pixels;
                    int spitch = rsurf->pitch / 4;
                    for (int y = 0; y < h && y < rsurf->h; y++) {
                        memcpy(&fb->pixels[y * fb->stride],
                               &sp[y * spitch],
                               (size_t)w * sizeof(uint32_t));
                    }
                    SDL_DestroySurface(rsurf);
                }
            }
            if (src) SDL_DestroyTexture(src);
            if (dst) SDL_DestroyTexture(dst);
            return;
        }
    }
#endif

    // CPU fallback
    const int w = fb->width;
    const int h = fb->height;
    size_t buf_size = (size_t)w * h * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    const double cx = (w - 1) * 0.5;
    const double cy = (h - 1) * 0.5;
    const double inv = 1.0 / level;

    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            const double src_xf = cx + (x - cx) * inv;
            const double src_yf = cy + (y - cy) * inv;
            int sx = (int)llround(src_xf) % w;
            int sy = (int)llround(src_yf) % h;
            if (sx < 0) sx += w;
            if (sy < 0) sy += h;

            fb->pixels[y * fb->stride + x] = tmp[sy * fb->stride + sx];
        }
    }
    free(tmp);
}

void graph_contrast(ACGraph *g, double level) {
    ACFramebuffer *fb = g->fb;
    if (!fb || fabs(level - 1.0) < 1e-6) return;

    const size_t total = (size_t)fb->width * fb->height;
    for (size_t i = 0; i < total; i++) {
        uint32_t p = fb->pixels[i];
        int a = (p >> 24) & 0xFF;
        int r = (p >> 16) & 0xFF;
        int gr = (p >> 8) & 0xFF;
        int b = p & 0xFF;

        r = (int)llround((r - 128) * level + 128);
        gr = (int)llround((gr - 128) * level + 128);
        b = (int)llround((b - 128) * level + 128);

        fb->pixels[i] = ((uint32_t)a << 24) |
                        ((uint32_t)clamp_u8(r) << 16) |
                        ((uint32_t)clamp_u8(gr) << 8) |
                        (uint32_t)clamp_u8(b);
    }
}

void graph_spin(ACGraph *g, double angle_radians) {
    ACFramebuffer *fb = g->fb;
    if (!fb || fabs(angle_radians) < 1e-6) return;

#ifdef USE_SDL
    // GPU path: SDL3 texture rotation
    if (g->gpu_display) {
        ACDisplay *d = (ACDisplay *)g->gpu_display;
        if (d->is_sdl && d->sdl_renderer) {
            const int w = fb->width;
            const int h = fb->height;
            double angle_degrees = angle_radians * (180.0 / M_PI);

            SDL_Texture *src = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h);
            SDL_Texture *dst = SDL_CreateTexture(d->sdl_renderer,
                SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_TARGET, w, h);

            if (src && dst) {
                SDL_UpdateTexture(src, NULL, fb->pixels,
                                  fb->stride * (int)sizeof(uint32_t));
                SDL_SetTextureScaleMode(src, SDL_SCALEMODE_NEAREST);

                SDL_FRect dstrect = {0, 0, (float)w, (float)h};
                SDL_FPoint center = {w * 0.5f, h * 0.5f};

                SDL_SetRenderTarget(d->sdl_renderer, dst);
                SDL_SetRenderDrawColor(d->sdl_renderer, 0, 0, 0, 255);
                SDL_RenderClear(d->sdl_renderer);
                SDL_RenderTextureRotated(d->sdl_renderer, src, NULL, &dstrect,
                                         angle_degrees, &center, SDL_FLIP_NONE);

                // Read back
                SDL_Rect full = {0, 0, w, h};
                SDL_Surface *rsurf = SDL_RenderReadPixels(d->sdl_renderer, &full);
                SDL_SetRenderTarget(d->sdl_renderer, NULL);
                if (rsurf) {
                    const uint32_t *sp = (const uint32_t *)rsurf->pixels;
                    int spitch = rsurf->pitch / 4;
                    for (int y = 0; y < h && y < rsurf->h; y++) {
                        memcpy(&fb->pixels[y * fb->stride],
                               &sp[y * spitch],
                               (size_t)w * sizeof(uint32_t));
                    }
                    SDL_DestroySurface(rsurf);
                }
            }
            if (src) SDL_DestroyTexture(src);
            if (dst) SDL_DestroyTexture(dst);
            return;
        }
    }
#endif

    // CPU fallback
    size_t buf_size = (size_t)fb->width * fb->height * sizeof(uint32_t);
    uint32_t *tmp = malloc(buf_size);
    if (!tmp) return;
    memcpy(tmp, fb->pixels, buf_size);

    const int w = fb->width;
    const int h = fb->height;
    const double cx = (w - 1) * 0.5;
    const double cy = (h - 1) * 0.5;
    const double c = cos(angle_radians);
    const double s = sin(angle_radians);

    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            const double dx = x - cx;
            const double dy = y - cy;
            const double src_xf = cx + (dx * c + dy * s);
            const double src_yf = cy + (-dx * s + dy * c);
            int sx = (int)llround(src_xf) % w;
            int sy = (int)llround(src_yf) % h;
            if (sx < 0) sx += w;
            if (sy < 0) sy += h;

            fb->pixels[y * fb->stride + x] = tmp[sy * fb->stride + sx];
        }
    }
    free(tmp);
}

ACFramebuffer *graph_painting(int w, int h) {
    return fb_create(w, h);
}

void graph_paste(ACGraph *g, ACFramebuffer *src, int dx, int dy) {
    for (int y = 0; y < src->height; y++) {
        for (int x = 0; x < src->width; x++) {
            uint32_t pixel = src->pixels[y * src->stride + x];
            fb_blend_pixel(g->fb, dx + x, dy + y, pixel);
        }
    }
}

void graph_page(ACGraph *g, ACFramebuffer *target) {
    g->fb = target ? target : g->screen;
}

void graph_qr(ACGraph *g, const char *text, int x, int y, int scale) {
    if (!g || !text || !text[0]) return;
    if (scale < 1) scale = 1;

    uint8_t qr_buf[qrcodegen_BUFFER_LEN_FOR_VERSION(10)];
    uint8_t tmp_buf[qrcodegen_BUFFER_LEN_FOR_VERSION(10)];

    if (!qrcodegen_encodeText(text, tmp_buf, qr_buf,
            qrcodegen_Ecc_LOW, qrcodegen_VERSION_MIN, 10,
            qrcodegen_Mask_AUTO, true)) {
        return; // encode failed (text too long for version 10)
    }

    int size = qrcodegen_getSize(qr_buf);
    int margin = 2; // quiet zone

    // Draw white background with margin
    int total = (size + margin * 2) * scale;
    ACColor saved = g->ink;
    graph_ink(g, (ACColor){255, 255, 255, 255});
    graph_box(g, x, y, total, total, 1);

    // Draw black modules
    graph_ink(g, (ACColor){0, 0, 0, 255});
    for (int qy = 0; qy < size; qy++) {
        for (int qx = 0; qx < size; qx++) {
            if (qrcodegen_getModule(qr_buf, qx, qy)) {
                graph_box(g, x + (qx + margin) * scale, y + (qy + margin) * scale,
                          scale, scale, 1);
            }
        }
    }
    g->ink = saved;
}
