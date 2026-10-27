// bootanim.c — loop an indexed boot animation on /dev/fb0 until told to stop.
//
// Usage: bootanim FRAMES W H FPS [SCALE]
//   FRAMES  raw ffmpeg `-pix_fmt pal8` frames: W*H index bytes, then a
//           1024-byte palette (256 × 0xAARRGGBB) per frame
//   SCALE   integer upscale (default: largest that fits half the screen)
//
// Draws each frame centred on a backdrop of the frame's own top-left colour,
// so the animation's background runs edge to edge. Exits when
// /run/bootanim.stop appears (init creates it just before ac-native takes
// the display) or after 60 s.
#include <fcntl.h>
#include <linux/fb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char **argv) {
    if (argc < 5) {
        fprintf(stderr, "usage: bootanim FRAMES W H FPS [SCALE]\n");
        return 2;
    }
    int w = atoi(argv[2]), h = atoi(argv[3]), fps = atoi(argv[4]);
    if (w <= 0 || h <= 0 || fps <= 0) return 2;

    FILE *f = fopen(argv[1], "rb");
    if (!f) { perror(argv[1]); return 1; }
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    const long frame_bytes = (long)w * h + 1024;
    int nframes = (int)(size / frame_bytes);
    uint8_t *data = malloc(size);
    if (!data || nframes < 1 || fread(data, 1, size, f) != (size_t)size) return 1;
    fclose(f);

    int fd = open("/dev/fb0", O_RDWR);
    if (fd < 0) { perror("/dev/fb0"); return 1; }
    struct fb_var_screeninfo v;
    struct fb_fix_screeninfo fx;
    if (ioctl(fd, FBIOGET_VSCREENINFO, &v) || ioctl(fd, FBIOGET_FSCREENINFO, &fx)) return 1;
    uint32_t *fb = mmap(NULL, fx.smem_len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (fb == MAP_FAILED) return 1;
    int sw = v.xres, sh = v.yres, stride = fx.line_length / 4;

    int scale = argc > 5 ? atoi(argv[5]) : 0;
    if (scale <= 0) {
        int sx = (sw / 2) / w, sy = (sh / 2) / h;
        scale = sx < sy ? sx : sy;
        if (scale < 1) scale = 1;
    }
    int ox = (sw - w * scale) / 2, oy = (sh - h * scale) / 2;

    uint32_t *row = malloc((size_t)w * scale * sizeof(uint32_t));
    struct timespec next;
    clock_gettime(CLOCK_MONOTONIC, &next);
    const long frame_ns = 1000000000L / fps;
    uint32_t last_bg = 0;
    int filled = 0;

    for (long tick = 0; tick < 60L * fps; tick++) {
        if (access("/run/bootanim.stop", F_OK) == 0) break;
        const uint8_t *idx = data + (tick % nframes) * frame_bytes;
        const uint32_t *pal = (const uint32_t *)(idx + (long)w * h);
        uint32_t bg = pal[idx[0]] | 0xFF000000u;
        if (!filled || bg != last_bg) {  // backdrop: once, and if it shifts
            for (int y = 0; y < sh; y++)
                for (int x = 0; x < sw; x++) fb[(size_t)y * stride + x] = bg;
            filled = 1;
            last_bg = bg;
        }
        for (int y = 0; y < h; y++) {
            const uint8_t *src = idx + (long)y * w;
            for (int x = 0; x < w; x++) {
                uint32_t p = pal[src[x]] | 0xFF000000u;
                for (int k = 0; k < scale; k++) row[x * scale + k] = p;
            }
            for (int k = 0; k < scale; k++)
                memcpy(fb + (size_t)(oy + y * scale + k) * stride + ox, row,
                       (size_t)w * scale * sizeof(uint32_t));
        }
        next.tv_nsec += frame_ns;
        while (next.tv_nsec >= 1000000000L) { next.tv_nsec -= 1000000000L; next.tv_sec++; }
        clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &next, NULL);
    }
    return 0;
}
