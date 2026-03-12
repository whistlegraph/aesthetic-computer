// ac-native.c — Sub-second boot AC piece runner
// Runs as PID 1 in a minimal initramfs.
// UEFI → EFI stub kernel → this binary → piece.mjs

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <math.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/reboot.h>
#include <linux/reboot.h>
#include <linux/input.h>
#include <sys/ioctl.h>

#include "drm-display.h"
#include "framebuffer.h"
#include "graph.h"
#include "font.h"
#include "input.h"
#include "audio.h"
#include "wifi.h"
#include "tts.h"
#include "js-bindings.h"

static volatile int running = 1;
static FILE *logfile = NULL;
static int is_removable(const char *blkname);
static void get_parent_block(const char *part, char *out, int out_sz);

// ── Performance logger (crash-resilient chunked files) ──
// Writes /mnt/perf/NNNN.csv every 30s, each chunk fsync'd and closed.
// On hard crash you lose at most 30 seconds. Keeps last 5 minutes (10 chunks).
#define PERF_CHUNK_SECS   30
#define PERF_CHUNK_FRAMES (60 * PERF_CHUNK_SECS)  // 1800 frames per chunk
#define PERF_MAX_CHUNKS   10                       // 10 × 30s = 5 minutes
#define PERF_BUF_SIZE     PERF_CHUNK_FRAMES

typedef struct {
    uint32_t frame;
    uint16_t total_us;    // total frame time in microseconds (capped at 65535)
    uint16_t act_us;
    uint16_t sim_us;
    uint16_t paint_us;
    uint16_t present_us;
    uint8_t  voices;      // active synth voices
    uint8_t  events;      // input events this frame
    uint8_t  js_heap_mb;  // QuickJS heap in MB (capped at 255)
    uint8_t  flags;       // bit0=trackpadFX, bit1=cursor_visible
} PerfRecord;

static PerfRecord *perf_buf = NULL;
static int perf_buf_count = 0;     // records in current chunk buffer
static int perf_chunk_seq = 0;     // monotonic chunk sequence number
static int perf_flush_frame = 0;   // last frame we flushed

static void perf_init(void) {
    perf_buf = calloc(PERF_BUF_SIZE, sizeof(PerfRecord));
    if (!perf_buf) fprintf(stderr, "[perf] alloc failed\n");
    mkdir("/mnt/perf", 0755);  // ensure directory exists
}

static void perf_record(PerfRecord *r) {
    if (!perf_buf || perf_buf_count >= PERF_BUF_SIZE) return;
    perf_buf[perf_buf_count++] = *r;
}

// Write current buffer as a numbered chunk file, fsync, close, then
// delete the oldest chunk if we exceed PERF_MAX_CHUNKS.
void perf_flush(void) {
    if (!perf_buf || perf_buf_count == 0) return;

    char path[128];
    snprintf(path, sizeof(path), "/mnt/perf/%04d.csv", perf_chunk_seq);

    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) return;

    FILE *f = fdopen(fd, "w");
    if (!f) { close(fd); return; }

    fprintf(f, "frame,total_us,act_us,sim_us,paint_us,present_us,voices,events,heap_mb,flags\n");
    for (int i = 0; i < perf_buf_count; i++) {
        PerfRecord *r = &perf_buf[i];
        fprintf(f, "%u,%u,%u,%u,%u,%u,%u,%u,%u,%u\n",
                r->frame, r->total_us, r->act_us, r->sim_us,
                r->paint_us, r->present_us, r->voices, r->events,
                r->js_heap_mb, r->flags);
    }
    fflush(f);
    fsync(fd);
    fclose(f);  // also closes fd

    perf_buf_count = 0;

    // Delete oldest chunk beyond retention window
    int old_seq = perf_chunk_seq - PERF_MAX_CHUNKS;
    if (old_seq >= 0) {
        char old_path[128];
        snprintf(old_path, sizeof(old_path), "/mnt/perf/%04d.csv", old_seq);
        unlink(old_path);
    }

    perf_chunk_seq++;
}

static void perf_destroy(void) {
    perf_flush();
    free(perf_buf);
    perf_buf = NULL;
}

static void signal_handler(int sig) {
    (void)sig;
    running = 0;
}

// Log to both stderr and logfile
void ac_log(const char *fmt, ...) {
    va_list args, args2;
    va_start(args, fmt);
    va_copy(args2, args);
    vfprintf(stderr, fmt, args);
    va_end(args);
    if (logfile) {
        vfprintf(logfile, fmt, args2);
        fflush(logfile);
        fsync(fileno(logfile));
    }
    va_end(args2);
}

// Flush log file to disk without closing it
void ac_log_flush(void) {
    if (logfile) {
        fflush(logfile);
        fsync(fileno(logfile));
    }
}

// Temporarily close the log file (e.g. before flash writes to same partition)
void ac_log_pause(void) {
    if (logfile) {
        fflush(logfile);
        fsync(fileno(logfile));
        fclose(logfile);
        logfile = NULL;
    }
}

// Reopen the log file in append mode after a pause
void ac_log_resume(void) {
    if (!logfile) {
        logfile = fopen("/mnt/ac-native.log", "a");
        // If reopen fails, logging continues to stderr only
    }
}

// Mount minimal filesystems (PID 1 only)
static void mount_minimal_fs(void) {
    mkdir("/proc", 0755);
    mkdir("/sys", 0755);
    mkdir("/dev", 0755);
    mkdir("/tmp", 0755);

    mount("proc", "/proc", "proc", 0, NULL);
    mount("sysfs", "/sys", "sysfs", 0, NULL);
    mount("devtmpfs", "/dev", "devtmpfs", 0, NULL);
    mount("tmpfs", "/tmp", "tmpfs", 0, NULL);

    // Wait for display device (up to 1s)
    for (int i = 0; i < 100; i++) {
        if (access("/dev/dri/card0", F_OK) == 0 ||
            access("/dev/fb0", F_OK) == 0) break;
        usleep(10000);
    }

    // Set performance power mode
    FILE *gov = fopen("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor", "w");
    if (gov) { fputs("performance", gov); fclose(gov); }
    // Set all CPUs to performance
    for (int c = 1; c < 16; c++) {
        char path[128];
        snprintf(path, sizeof(path), "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_governor", c);
        gov = fopen(path, "w");
        if (gov) { fputs("performance", gov); fclose(gov); }
    }
}

// Try to mount boot USB for log writing (non-blocking, best-effort)
char log_dev[32] = "";  // non-static: accessed by js-bindings.c for flash target check
static void try_mount_log(void) {
    mkdir("/mnt", 0755);
    // Wait for USB block devices to appear (up to 2s after EFI handoff)
    fprintf(stderr, "[ac-native] Waiting for USB block devices...\n");
    for (int w = 0; w < 100; w++) {
        if (access("/dev/sda1", F_OK) == 0 || access("/dev/sdb1", F_OK) == 0) break;
        usleep(20000);
    }
    fprintf(stderr, "[ac-native] sda1=%s sdb1=%s\n",
            access("/dev/sda1", F_OK) == 0 ? "yes" : "no",
            access("/dev/sdb1", F_OK) == 0 ? "yes" : "no");
    const char *devs[] = {
        "/dev/sda1", "/dev/sdb1", "/dev/sdc1", "/dev/sdd1",
        "/dev/nvme0n1p1", "/dev/nvme1n1p1",
        NULL
    };

    // Pass 0: removable media first (USB install source).
    // Pass 1: fallback to internal ESP (ensures config.json loads on disk boots).
    for (int pass = 0; pass < 2; pass++) {
        for (int i = 0; devs[i]; i++) {
            if (access(devs[i], F_OK) != 0) continue;

            char blk[32] = "";
            get_parent_block(devs[i] + 5, blk, sizeof(blk)); // skip "/dev/"
            int rem = blk[0] ? is_removable(blk) : -1;
            if (pass == 0 && rem != 1) continue;
            if (pass == 1 && rem == 1) continue;

            if (mount(devs[i], "/mnt", "vfat", 0, NULL) == 0) {
                logfile = fopen("/mnt/ac-native.log", "w");
                if (logfile) {
                    // Immediate test write to verify logging works
                    fprintf(logfile, "[ac-native] Log opened on %s (removable=%d)\n", devs[i], rem);
                    fflush(logfile);
                    fsync(fileno(logfile));
                    strncpy(log_dev, devs[i], sizeof(log_dev) - 1);
                    fprintf(stderr, "[ac-native] Log: %s -> /mnt/ac-native.log (removable=%d)\n", devs[i], rem);
                    return;
                }
                umount("/mnt");
            }
            fprintf(stderr, "[ac-native] Log mount failed: %s\n", devs[i]);
        }
    }
    // Fallback: log to tmpfs (won't survive reboot but stderr goes to console)
    fprintf(stderr, "[ac-native] No USB log mount available\n");
}

// Boot title — defaults to "notepat", overridden by config.json handle
static char boot_title[80] = "notepat";
static ACColor boot_title_colors[80];
static int boot_title_colors_len = 0;

static uint8_t clamp_u8(int v) {
    if (v < 0) return 0;
    if (v > 255) return 255;
    return (uint8_t)v;
}

static int parse_config_string(const char *json, const char *key, char *out, int out_sz) {
    if (!json || !key || !out || out_sz < 2) return 0;
    const char *kp = strstr(json, key);
    if (!kp) return 0;
    const char *colon = strchr(kp, ':');
    if (!colon) return 0;
    const char *q1 = strchr(colon, '"');
    if (!q1) return 0;
    const char *q2 = strchr(q1 + 1, '"');
    if (!q2) return 0;
    int len = (int)(q2 - q1 - 1);
    if (len <= 0 || len >= out_sz) return 0;
    memcpy(out, q1 + 1, len);
    out[len] = 0;
    return 1;
}

static int parse_json_int_field(const char *start, const char *limit, const char *key, int *out) {
    if (!start || !limit || !key || !out || start >= limit) return 0;
    const char *kp = strstr(start, key);
    if (!kp || kp >= limit) return 0;
    const char *colon = strchr(kp, ':');
    if (!colon || colon >= limit) return 0;
    const char *p = colon + 1;
    while (p < limit && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p >= limit) return 0;
    char *endp = NULL;
    long v = strtol(p, &endp, 10);
    if (!endp || endp == p || endp > limit) return 0;
    *out = (int)v;
    return 1;
}

static void parse_boot_title_colors(const char *json) {
    boot_title_colors_len = 0;
    if (!json) return;

    // Prefer explicit title_colors, fallback to handle colors from API payload.
    const char *cp = strstr(json, "\"title_colors\"");
    if (!cp) cp = strstr(json, "\"colors\"");
    if (!cp) return;

    const char *arr0 = strchr(cp, '[');
    if (!arr0) return;
    const char *arr1 = strchr(arr0 + 1, ']');
    if (!arr1) return;

    const char *p = arr0 + 1;
    while (p < arr1 && boot_title_colors_len < (int)(sizeof(boot_title_colors) / sizeof(boot_title_colors[0]))) {
        const char *obj0 = strchr(p, '{');
        if (!obj0 || obj0 >= arr1) break;
        const char *obj1 = strchr(obj0, '}');
        if (!obj1 || obj1 > arr1) break;

        int r = -1, g = -1, b = -1;
        if (parse_json_int_field(obj0, obj1, "\"r\"", &r) &&
            parse_json_int_field(obj0, obj1, "\"g\"", &g) &&
            parse_json_int_field(obj0, obj1, "\"b\"", &b)) {
            int i = boot_title_colors_len++;
            boot_title_colors[i] = (ACColor){clamp_u8(r), clamp_u8(g), clamp_u8(b), 255};
        }
        p = obj1 + 1;
    }
}

static void load_boot_visual_config(void) {
    // Try USB/HD config first, fall back to initramfs-baked default
    FILE *cfg = fopen("/mnt/config.json", "r");
    if (!cfg) cfg = fopen("/default-config.json", "r");
    if (!cfg) return;

    char buf[4096] = {0};
    size_t n = fread(buf, 1, sizeof(buf) - 1, cfg);
    fclose(cfg);
    buf[n] = '\0';

    char handle[64] = {0};
    if (parse_config_string(buf, "\"handle\"", handle, sizeof(handle))) {
        if ((int)strlen(handle) < (int)sizeof(boot_title) - 4) {
            snprintf(boot_title, sizeof(boot_title), "hi @%s", handle);
        }
    }
    parse_boot_title_colors(buf);

    ac_log("[ac-native] Boot title: %s (colors=%d)\n", boot_title, boot_title_colors_len);
}

static ACColor rainbow_title_color(int ci, int frame, int alpha) {
    double hue = fmod((double)ci / 7.0 * 360.0 + frame * 2.0, 360.0);
    double h6 = hue / 60.0;
    int hi = (int)h6 % 6;
    double fr = h6 - (int)h6;
    double sv = 0.7, vv = 1.0;
    double p = vv * (1.0 - sv), q = vv * (1.0 - sv * fr), tt = vv * (1.0 - sv * (1.0 - fr));
    double cr, cg, cb;
    switch (hi) {
        case 0: cr = vv; cg = tt; cb = p; break;
        case 1: cr = q; cg = vv; cb = p; break;
        case 2: cr = p; cg = vv; cb = tt; break;
        case 3: cr = p; cg = q; cb = vv; break;
        case 4: cr = tt; cg = p; cb = vv; break;
        default: cr = vv; cg = p; cb = q; break;
    }
    return (ACColor){(uint8_t)(cr * 255), (uint8_t)(cg * 255), (uint8_t)(cb * 255), clamp_u8(alpha)};
}

static ACColor title_char_color(int ci, int frame, int alpha) {
    if (boot_title_colors_len <= 0) return rainbow_title_color(ci, frame, alpha);

    int title_len = (int)strlen(boot_title);
    int idx = ci;
    // Map palette to the handle portion of "hi @handle" (skip "hi @" = 4 chars)
    const char *at = strchr(boot_title, '@');
    int handle_start = at ? (int)(at - boot_title) + 1 : 0; // char after @
    if (handle_start > 0 && ci >= handle_start && boot_title_colors_len > 0) {
        idx = ci - handle_start;
    } else if (ci < handle_start) {
        // "hi " and "@" get rainbow colors
        return rainbow_title_color(ci, frame, alpha);
    }
    if (idx < 0) idx = 0;
    if (boot_title_colors_len > 0) idx %= boot_title_colors_len;
    ACColor c = boot_title_colors[idx];

    // Keep custom colors visible over dark boot backgrounds.
    int pulse = (int)(18.0 * sin((double)(frame + ci * 6) * 0.08));
    int r = (c.r * 7 + 255 * 3) / 10 + pulse;
    int g = (c.g * 7 + 255 * 3) / 10 + pulse;
    int b = (c.b * 7 + 255 * 3) / 10 + pulse;
    return (ACColor){clamp_u8(r), clamp_u8(g), clamp_u8(b), clamp_u8(alpha)};
}

// Forward declarations
static void draw_boot_status(ACGraph *graph, ACFramebuffer *screen,
                             ACDisplay *display, const char *status);

// Check if a block device is removable (USB = 1, internal = 0)
static int is_removable(const char *blkname) {
    char path[128];
    snprintf(path, sizeof(path), "/sys/block/%s/removable", blkname);
    FILE *f = fopen(path, "r");
    if (!f) return -1; // unknown
    int val = 0;
    if (fscanf(f, "%d", &val) != 1) val = -1;
    fclose(f);
    return val;
}

// Copy a file from src to dst path, returns bytes copied or -1 on error
static long copy_file(const char *src, const char *dst) {
    FILE *in = fopen(src, "rb");
    if (!in) return -1;
    FILE *out = fopen(dst, "wb");
    if (!out) { fclose(in); return -1; }
    char buf[65536];
    long total = 0;
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) { total = -1; break; }
        total += n;
    }
    fclose(out);
    fclose(in);
    return total;
}

// Auto-install kernel to internal drive's EFI System Partition
// Returns 1 on success, 0 on failure.
static int auto_install_to_hd(ACGraph *graph, ACFramebuffer *screen,
                              ACDisplay *display) {
    char source_mount[32] = "/mnt";
    char source_dev[32] = "";
    int source_mounted_tmp = 0;
    char kernel_src[96] = "";
    char config_src[64] = "";

    if (display)
        draw_boot_status(graph, screen, display, "installing to disk...");

    // Prefer current /mnt only when it is removable and contains a kernel.
    if (log_dev[0]) {
        char blk[32] = "";
        get_parent_block(log_dev + 5, blk, sizeof(blk));
        if (blk[0] && is_removable(blk) == 1 &&
            access("/mnt/EFI/BOOT/BOOTX64.EFI", F_OK) == 0) {
            strncpy(source_dev, log_dev, sizeof(source_dev) - 1);
            source_dev[sizeof(source_dev) - 1] = '\0';
        }
    }

    // Fallback: scan removable partitions for BOOTX64.EFI
    if (!source_dev[0]) {
        const char *src_candidates[] = {
            "/dev/sda1", "/dev/sdb1", "/dev/sdc1", "/dev/sdd1", NULL
        };
        mkdir("/tmp/src", 0755);
        for (int i = 0; src_candidates[i]; i++) {
            if (access(src_candidates[i], F_OK) != 0) continue;
            char blk[32] = "";
            get_parent_block(src_candidates[i] + 5, blk, sizeof(blk));
            if (!blk[0] || is_removable(blk) != 1) continue;
            if (mount(src_candidates[i], "/tmp/src", "vfat", 0, NULL) != 0) continue;
            if (access("/tmp/src/EFI/BOOT/BOOTX64.EFI", F_OK) == 0) {
                strncpy(source_dev, src_candidates[i], sizeof(source_dev) - 1);
                source_dev[sizeof(source_dev) - 1] = '\0';
                strncpy(source_mount, "/tmp/src", sizeof(source_mount) - 1);
                source_mount[sizeof(source_mount) - 1] = '\0';
                source_mounted_tmp = 1;
                break;
            }
            umount("/tmp/src");
        }
    }

    snprintf(kernel_src, sizeof(kernel_src), "%s/EFI/BOOT/BOOTX64.EFI", source_mount);
    snprintf(config_src, sizeof(config_src), "%s/config.json", source_mount);
    if (!source_dev[0] || access(kernel_src, F_OK) != 0) {
        fprintf(stderr, "[ac-native] No removable install source with kernel found\n");
        if (source_mounted_tmp) umount("/tmp/src");
        return 0;
    }

    mkdir("/tmp/hd", 0755);

    // Determine which block device install source is on (skip it as destination)
    char usb_blk[16] = "";
    if (source_dev[0]) {
        const char *p = source_dev + 5; // skip "/dev/"
        int len = 0;
        while (p[len] && (p[len] < '0' || p[len] > '9')) len++;
        // For nvme: "nvme0n1p1" → parent "nvme0n1"
        // For sd: "sda1" → parent "sda"
        if (len > 0) {
            if (len > (int)sizeof(usb_blk) - 1) len = sizeof(usb_blk) - 1;
            memcpy(usb_blk, p, len);
            usb_blk[len] = '\0';
        }
    }

    // Scan for internal (non-removable) block devices with partitions
    // Try NVMe first (always internal), then SATA/USB
    const char *part_candidates[] = {
        "nvme0n1", "nvme1n1",     // NVMe SSDs
        "sda", "sdb", "sdc", "sdd", // SATA/USB
        NULL
    };

    int installed = 0;
    for (int i = 0; part_candidates[i] && !installed; i++) {
        const char *blk = part_candidates[i];

        // Skip the USB boot device
        if (usb_blk[0] && strcmp(blk, usb_blk) == 0) continue;

        // For sd* devices, skip if removable
        if (blk[0] == 's' && blk[1] == 'd') {
            int rem = is_removable(blk);
            if (rem == 1) continue; // removable = USB
        }

        // Try partitions 1-8
        for (int p = 1; p <= 8 && !installed; p++) {
            char devpath[32];
            if (blk[0] == 'n') // NVMe: nvme0n1p1
                snprintf(devpath, sizeof(devpath), "/dev/%sp%d", blk, p);
            else // SATA: sda1
                snprintf(devpath, sizeof(devpath), "/dev/%s%d", blk, p);

            if (access(devpath, F_OK) != 0) continue;

            // Try mounting as FAT (ESP is always FAT32)
            if (mount(devpath, "/tmp/hd", "vfat", 0, NULL) != 0) continue;

            // Create EFI boot directories
            mkdir("/tmp/hd/EFI", 0755);
            mkdir("/tmp/hd/EFI/BOOT", 0755);

            // Copy kernel as fallback boot entry
            long sz = copy_file(kernel_src, "/tmp/hd/EFI/BOOT/BOOTX64.EFI");
            if (sz > 0) {
                // Also overwrite Windows Boot Manager path (ThinkPad BIOS often
                // boots this first regardless of boot order)
                mkdir("/tmp/hd/EFI/Microsoft", 0755);
                mkdir("/tmp/hd/EFI/Microsoft/Boot", 0755);
                copy_file(kernel_src, "/tmp/hd/EFI/Microsoft/Boot/bootmgfw.efi");

                // Preserve user handle/piece config on installed disk.
                if (access(config_src, F_OK) == 0) {
                    copy_file(config_src, "/tmp/hd/config.json");
                }

                // Write install marker so next boot knows it's installed
                FILE *marker = fopen("/tmp/hd/EFI/BOOT/ac-installed", "w");
                if (marker) { fputs("1\n", marker); fclose(marker); }

                sync();
                installed = 1;
                fprintf(stderr, "[ac-native] Installed %ld bytes from %s to %s\n",
                        sz, source_dev, devpath);
            }

            umount("/tmp/hd");
        }
    }

    if (installed && display) {
        draw_boot_status(graph, screen, display, "installed to disk!");
        usleep(800000);
    } else if (!installed) {
        fprintf(stderr, "[ac-native] No suitable HD partition found for install\n");
    }
    if (source_mounted_tmp) umount("/tmp/src");
    return installed;
}

static void frame_sync_60fps(struct timespec *next) {
    next->tv_nsec += 16666667;
    if (next->tv_nsec >= 1000000000) {
        next->tv_nsec -= 1000000000;
        next->tv_sec++;
    }
    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, next, NULL);
}

// Check if a specific key is currently held using pre-opened fds (fast path)
static int check_key_held_fds(int keycode, int *fds, int nfds) {
    for (int i = 0; i < nfds; i++) {
        unsigned long bits[(KEY_MAX + 7) / 8 / sizeof(unsigned long) + 1] = {0};
        if (ioctl(fds[i], EVIOCGKEY(sizeof(bits)), bits) >= 0) {
            if (bits[keycode / (sizeof(unsigned long) * 8)] &
                (1UL << (keycode % (sizeof(unsigned long) * 8))))
                return 1;
        }
    }
    return 0;
}

// Get LA offset from UTC: 7 for PDT, 8 for PST
// DST: second Sunday of March 2am PT → first Sunday of November 2am PT
static int get_la_offset(void) {
    time_t now = time(NULL);
    struct tm *utc = gmtime(&now);
    int m = utc->tm_mon, y = utc->tm_year + 1900;
    if (m > 2 && m < 10) return 7;  // Apr-Oct: PDT
    if (m < 2 || m > 10) return 8;  // Jan-Feb, Dec: PST
    if (m == 2) { // March: find second Sunday
        struct tm mar1 = {0}; mar1.tm_year = y - 1900; mar1.tm_mon = 2; mar1.tm_mday = 1;
        mktime(&mar1);
        int secondSun = 8 + (7 - mar1.tm_wday) % 7;
        // DST starts at 2am PST = 10:00 UTC on that day
        if (utc->tm_mday > secondSun) return 7;
        if (utc->tm_mday < secondSun) return 8;
        return (utc->tm_hour >= 10) ? 7 : 8;
    }
    // November: find first Sunday
    struct tm nov1 = {0}; nov1.tm_year = y - 1900; nov1.tm_mon = 10; nov1.tm_mday = 1;
    mktime(&nov1);
    int firstSun = 1 + (7 - nov1.tm_wday) % 7;
    // DST ends at 2am PDT = 9:00 UTC on that day
    if (utc->tm_mday < firstSun) return 7;
    if (utc->tm_mday > firstSun) return 8;
    return (utc->tm_hour < 9) ? 7 : 8;
}

static int get_la_hour(void) {
    time_t now = time(NULL);
    struct tm *utc = gmtime(&now);
    return (utc->tm_hour - get_la_offset() + 24) % 24;
}

static void play_install_prompt_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_SINE, 620.0, 0.06, 0.40, 0.001, 0.05, 0.0);
    usleep(35000);
    audio_synth(audio, WAVE_SINE, 760.0, 0.06, 0.40, 0.001, 0.05, 0.0);
}

static void play_install_accept_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_TRIANGLE, 880.0, 0.08, 0.55, 0.001, 0.06, -0.1);
    usleep(45000);
    audio_synth(audio, WAVE_TRIANGLE, 1175.0, 0.10, 0.65, 0.001, 0.08, 0.1);
}

static void play_install_reject_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_SAWTOOTH, 260.0, 0.08, 0.45, 0.001, 0.06, 0.0);
    usleep(45000);
    audio_synth(audio, WAVE_SAWTOOTH, 180.0, 0.12, 0.45, 0.001, 0.09, 0.0);
}

static void play_install_success_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_TRIANGLE, 659.25, 0.12, 0.55, 0.001, 0.08, -0.2);
    usleep(50000);
    audio_synth(audio, WAVE_TRIANGLE, 783.99, 0.12, 0.60, 0.001, 0.08, 0.0);
    usleep(50000);
    audio_synth(audio, WAVE_TRIANGLE, 987.77, 0.16, 0.68, 0.001, 0.12, 0.2);
}

static void play_install_failure_beep(ACAudio *audio) {
    if (!audio || !audio->pcm) return;
    audio_synth(audio, WAVE_SAWTOOTH, 180.0, 0.11, 0.50, 0.001, 0.08, 0.0);
    usleep(55000);
    audio_synth(audio, WAVE_SAWTOOTH, 140.0, 0.15, 0.50, 0.001, 0.11, 0.0);
}

// Draw y/n install confirmation screen
// Returns 1 if user confirms with Y, 0 if N/Escape
static int draw_install_confirm(ACGraph *graph, ACFramebuffer *screen,
                                 ACDisplay *display, int *fds, int nfds,
                                 ACTts *tts, ACAudio *audio) {
    int is_dark = 1; // always dark
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

    if (tts) tts_speak(tts, "install to disk? press Y for yes, N for no");
    play_install_prompt_beep(audio);

    for (;;) {
        uint8_t bg = is_dark ? 20 : 255;
        graph_wipe(graph, (ACColor){bg, bg, (uint8_t)(bg + (is_dark ? 5 : 0)), 255});

        uint8_t fg = is_dark ? 220 : 0;
        graph_ink(graph, (ACColor){fg, fg, fg, 255});

        int tw = font_measure_matrix("install to disk?", 2);
        font_draw_matrix(graph, "install to disk?",
                         (screen->width - tw) / 2, screen->height / 2 - 24, 2);

        // Warning text
        graph_ink(graph, (ACColor){220, 80, 80, 255});
        const char *warn = "this will overwrite EFI boot!";
        int ww = font_measure_matrix(warn, 1);
        font_draw_matrix(graph, warn, (screen->width - ww) / 2,
                         screen->height / 2 + 2, 1);

        // Y/N prompt
        graph_ink(graph, (ACColor){60, 200, 60, 255});
        const char *yn = "Y = yes    N = no";
        int yw = font_measure_matrix(yn, 1);
        font_draw_matrix(graph, yn, (screen->width - yw) / 2,
                         screen->height / 2 + 20, 1);

        display_present(display, screen, 3);
        frame_sync_60fps(&anim_time);

        // Read key events
        struct input_event ev;
        for (int ki = 0; ki < nfds; ki++) {
            while (read(fds[ki], &ev, sizeof(ev)) == sizeof(ev)) {
                if (ev.type == EV_KEY && ev.value == 1) {
                    if (ev.code == KEY_Y) {
                        play_install_accept_beep(audio);
                        return 1;
                    }
                    if (ev.code == KEY_N || ev.code == KEY_ESC) {
                        play_install_reject_beep(audio);
                        return 0;
                    }
                }
            }
        }
    }
}

// Pause after install attempt so USB boots do not continue into the piece.
// Returns 1 if reboot requested, 0 if user chose to continue boot (failure-only path).
static int draw_install_reboot_prompt(ACGraph *graph, ACFramebuffer *screen,
                                      ACDisplay *display, ACInput *input,
                                      ACTts *tts, ACAudio *audio,
                                      int install_ok) {
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

    if (install_ok) {
        if (tts) tts_speak(tts, "install complete. remove USB stick. press R to reboot.");
        play_install_success_beep(audio);
    } else {
        if (tts) tts_speak(tts, "install failed. press R to reboot or escape to continue.");
        play_install_failure_beep(audio);
    }

    int blink = 0;
    int no_input_frames = 0;
    while (running) {
        blink++;
        graph_wipe(graph, (ACColor){20, 20, 25, 255});

        const char *title = install_ok ? "disk install complete" : "disk install failed";
        ACColor title_color = install_ok
            ? (ACColor){90, 220, 120, 255}
            : (ACColor){220, 100, 90, 255};
        graph_ink(graph, title_color);
        int tw = font_measure_matrix(title, 2);
        font_draw_matrix(graph, title, (screen->width - tw) / 2, screen->height / 2 - 34, 2);

        graph_ink(graph, (ACColor){220, 220, 220, 255});
        const char *line1 = install_ok
            ? "remove USB stick now"
            : "check EFI target and try again";
        int l1w = font_measure_matrix(line1, 1);
        font_draw_matrix(graph, line1, (screen->width - l1w) / 2, screen->height / 2 + 0, 1);

        graph_ink(graph, (ACColor){180, 180, 210, 255});
        const char *line2 = "R/Enter = reboot";
        int l2w = font_measure_matrix(line2, 1);
        font_draw_matrix(graph, line2, (screen->width - l2w) / 2, screen->height / 2 + 16, 1);

        if (!install_ok) {
            graph_ink(graph, (ACColor){150, 150, 170, 255});
            const char *line3 = "Esc = continue USB boot";
            int l3w = font_measure_matrix(line3, 1);
            font_draw_matrix(graph, line3, (screen->width - l3w) / 2, screen->height / 2 + 30, 1);
        }

        if ((blink % 60) < 36) {
            graph_ink(graph, (ACColor){120, 120, 140, 255});
            const char *line4 = install_ok ? "waiting for reboot..." : "waiting for key...";
            int l4w = font_measure_matrix(line4, 1);
            font_draw_matrix(graph, line4, (screen->width - l4w) / 2, screen->height / 2 + 48, 1);
        }

        display_present(display, screen, 3);
        frame_sync_60fps(&anim_time);

        if (!input) {
            no_input_frames++;
            if (no_input_frames > 900) return install_ok ? 1 : 0; // ~15s fallback
            continue;
        }
        input_poll(input);
        for (int i = 0; i < input->event_count; i++) {
            ACEvent *ev = &input->events[i];
            if (ev->type != AC_EVENT_KEYBOARD_DOWN) continue;

            if (ev->key_code == KEY_R || ev->key_code == KEY_ENTER || ev->key_code == KEY_KPENTER) {
                play_install_accept_beep(audio);
                return 1;
            }
            if (!install_ok && (ev->key_code == KEY_ESC || ev->key_code == KEY_SPACE)) {
                play_install_reject_beep(audio);
                return 0;
            }
        }
    }
    return install_ok ? 1 : 0;
}

// Check if we booted from an installed (non-removable) disk
// by looking for ac-installed marker on an internal ESP
// Extract parent block device name from a partition path
// "nvme0n1p1" → "nvme0n1", "sda1" → "sda"
static void get_parent_block(const char *part, char *out, int out_sz) {
    out[0] = 0;
    int len = (int)strlen(part);
    if (len >= out_sz) return;
    // NVMe: strip trailing "pN" (e.g. nvme0n1p1 → nvme0n1)
    if (strncmp(part, "nvme", 4) == 0) {
        // Find last 'p' followed by digits
        for (int i = len - 1; i > 4; i--) {
            if (part[i - 1] == 'p' && part[i] >= '0' && part[i] <= '9') {
                memcpy(out, part, i - 1);
                out[i - 1] = 0;
                return;
            }
        }
    }
    // SATA/USB: strip trailing digits (e.g. sda1 → sda)
    int i = len;
    while (i > 0 && part[i - 1] >= '0' && part[i - 1] <= '9') i--;
    if (i > 0 && i < out_sz) {
        memcpy(out, part, i);
        out[i] = 0;
    }
}

static int is_installed_on_hd(void) {
    if (getpid() != 1) return 0; // not bare metal
    mkdir("/tmp/chk", 0755);
    const char *parts[] = {
        "/dev/nvme0n1p1", "/dev/nvme0n1p2",
        "/dev/sda1", "/dev/sdb1", NULL
    };
    for (int i = 0; parts[i]; i++) {
        // Extract parent block device and skip removable (USB) drives
        char blk[32] = "";
        get_parent_block(parts[i] + 5, blk, sizeof(blk));
        fprintf(stderr, "[install-check] %s → parent=%s\n", parts[i], blk);
        if (blk[0] && is_removable(blk) == 1) {
            fprintf(stderr, "[install-check]   → removable, skipping\n");
            continue;
        }
        if (mount(parts[i], "/tmp/chk", "vfat", MS_RDONLY, NULL) != 0) {
            fprintf(stderr, "[install-check]   → mount failed\n");
            continue;
        }
        int found = access("/tmp/chk/EFI/BOOT/ac-installed", F_OK) == 0;
        umount("/tmp/chk");
        fprintf(stderr, "[install-check]   → mounted, ac-installed=%s\n", found ? "YES" : "no");
        if (found) return 1;
    }
    fprintf(stderr, "[install-check] not installed on HD\n");
    return 0;
}

// Draw startup fade animation (black → white with title)
// Returns 1 if user pressed W and confirmed install, 0 otherwise
static int draw_startup_fade(ACGraph *graph, ACFramebuffer *screen,
                              ACDisplay *display, ACTts *tts, ACAudio *audio) {
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);
    // Show install option whenever booting from USB (even if already installed —
    // user may want to update). Detect USB by checking if boot device is removable.
    int show_install = 0;
    if (getpid() == 1 && log_dev[0]) {
        char boot_blk[32] = "";
        get_parent_block(log_dev + 5, boot_blk, sizeof(boot_blk));
        show_install = (boot_blk[0] && is_removable(boot_blk) == 1) ? 1 : 0;
        fprintf(stderr, "[boot-anim] boot_dev=%s parent=%s removable=%d show_install=%d\n",
                log_dev, boot_blk, is_removable(boot_blk), show_install);
    } else if (getpid() == 1) {
        // No log_dev — fallback: show if not installed
        show_install = !is_installed_on_hd();
        fprintf(stderr, "[boot-anim] no log_dev, show_install=%d (fallback)\n", show_install);
    }

    // Open evdev fds once for key checking (avoid per-frame open/close)
    int key_fds[8];
    int key_fd_count = 0;
    DIR *dir = opendir("/dev/input");
    if (dir) {
        struct dirent *ent;
        while ((ent = readdir(dir)) && key_fd_count < 8) {
            if (strncmp(ent->d_name, "event", 5) != 0) continue;
            char path[64];
            snprintf(path, sizeof(path), "/dev/input/%s", ent->d_name);
            int fd = open(path, O_RDONLY | O_NONBLOCK);
            if (fd >= 0) {
                key_fds[key_fd_count++] = fd;
                fprintf(stderr, "[boot-anim] opened %s (fd %d)\n", path, fd);
            }
        }
        closedir(dir);
    }
    fprintf(stderr, "[boot-anim] %d event devices\n", key_fd_count);

    // Start with a black frame immediately (hides any kernel text)
    graph_wipe(graph, (ACColor){0, 0, 0, 255});
    display_present(display, screen, 3);

    // Check if this is a fresh boot of a new version
    int is_new_version = 0;
#ifdef AC_GIT_HASH
#ifdef AC_BUILD_TS
    {
        const char *current_ver = AC_GIT_HASH "-" AC_BUILD_TS;
        char prev_ver[128] = "";
        FILE *vf = fopen("/mnt/booted-version", "r");
        if (vf) {
            if (fgets(prev_ver, sizeof(prev_ver), vf)) {
                // Strip trailing newline
                char *nl = strchr(prev_ver, '\n');
                if (nl) *nl = 0;
            }
            fclose(vf);
        }
        is_new_version = (strcmp(prev_ver, current_ver) != 0);
        ac_log("[boot] version=%s prev=%s fresh=%s\n", current_ver, prev_ver, is_new_version ? "YES" : "no");
        // Write current version immediately so next boot sees it
        vf = fopen("/mnt/booted-version", "w");
        if (vf) {
            fprintf(vf, "%s", current_ver);
            fclose(vf);
        }
        // Append to boot history log (persists across reboots)
        vf = fopen("/mnt/boot-history.log", "a");
        if (vf) {
            time_t now_t = time(NULL);
            struct tm *tm = gmtime(&now_t);
            char ts[32];
            strftime(ts, sizeof(ts), "%Y-%m-%dT%H:%M:%SZ", tm);
            fprintf(vf, "%s %s %s\n", ts, current_ver, is_new_version ? "FRESH" : "same");
            fclose(vf);
        }
        sync();
    }
#endif
#endif

    // 180 frames = 3 second animation.
    // W press → halt and show y/n confirmation
    // Any other key → skip animation and start playing
    int total_frames = 300; // 5 seconds — room for TTS greeting
    int skip_anim = 0;
    int w_pressed = 0;
    for (int f = 0; f < total_frames && !skip_anim && !w_pressed; f++) {
        double t = (double)f / (double)total_frames;

        // Drain all key events — detect W press or other-key skip
        {
            struct input_event ev;
            for (int ki = 0; ki < key_fd_count; ki++) {
                while (read(key_fds[ki], &ev, sizeof(ev)) == sizeof(ev)) {
                    if (ev.type == EV_KEY && ev.value == 1) {
                        // First 60 frames (1 second): ignore all keys
                        // Ensures W hint is visible before accepting input
                        if (f < 60) {
                            fprintf(stderr, "[boot-anim] drained key %d at f=%d (hold period)\n", ev.code, f);
                            continue;
                        }
                        fprintf(stderr, "[boot-anim] key %d at f=%d\n", ev.code, f);
                        if (ev.code == KEY_W && show_install) {
                            w_pressed = 1;
                        } else if (ev.code != KEY_RESERVED) {
                            skip_anim = 1;
                        }
                    }
                }
            }
        }

        // Startup greeting — use handle if available, include build name
        if (f == 10 && tts) {
            char greet[160];
            const char *at = strchr(boot_title, '@');
#ifdef AC_BUILD_NAME
            // Replace hyphens with spaces for natural TTS
            char name_tts[64];
            strncpy(name_tts, AC_BUILD_NAME, sizeof(name_tts) - 1);
            name_tts[sizeof(name_tts) - 1] = 0;
            for (char *p = name_tts; *p; p++) { if (*p == '-') *p = ' '; }
            if (at)
                snprintf(greet, sizeof(greet), "hi %s. %s.", at + 1, name_tts);
            else
                snprintf(greet, sizeof(greet), "%s.", name_tts);
#else
            if (at)
                snprintf(greet, sizeof(greet), "hi %s", at + 1);
            else
                snprintf(greet, sizeof(greet), "hi");
#endif
            tts_speak(tts, greet);
        }
        // W hint is visual only — no TTS

        // Fade from black to purple bg (complete in first 0.3s)
        double fade_t = t * 3.33;
        if (fade_t > 1.0) fade_t = 1.0;
        int bg_r = (int)(40 * fade_t);
        int bg_g = (int)(20 * fade_t);
        int bg_b = (int)(60 * fade_t);
        graph_wipe(graph, (ACColor){(uint8_t)bg_r, (uint8_t)bg_g, (uint8_t)bg_b, 255});

        // Title — per-handle palette (fallback rainbow), animated pulse
        int alpha = (int)(255.0 * fade_t);
        if (alpha > 0) {
            const char *title = boot_title;
            int tw = font_measure_matrix(title, 3);
            int tx = (screen->width - tw) / 2;
            int ty = screen->height / 2 - 20;
            for (int ci = 0; title[ci]; ci++) {
                ACColor cc = title_char_color(ci, f, alpha);
                graph_ink(graph, cc);
                char ch[2] = { title[ci], 0 };
                tx = font_draw_matrix(graph, ch, tx, ty, 3);
            }
        }

        // Version + build name + build date (high-contrast panel, top-right)
#ifdef AC_GIT_HASH
        if (alpha > 40) {
            char ver[64];
            char bts[64];
            char bname[64] = "";
            snprintf(ver, sizeof(ver), "version %s", AC_GIT_HASH);
#ifdef AC_BUILD_TS
            snprintf(bts, sizeof(bts), "%s", AC_BUILD_TS);
#else
            snprintf(bts, sizeof(bts), "build unknown");
#endif
#ifdef AC_BUILD_NAME
            snprintf(bname, sizeof(bname), "%s", AC_BUILD_NAME);
#endif
            int wv = font_measure_matrix(ver, 1);
            int wt = font_measure_matrix(bts, 1);
            int wn = bname[0] ? font_measure_matrix(bname, 1) : 0;
            int max_w = wv;
            if (wt > max_w) max_w = wt;
            if (wn > max_w) max_w = wn;
            int panel_w = max_w + 8;
            int panel_h = bname[0] ? 28 : 20;
            int panel_x = screen->width - panel_w - 3;
            int panel_y = 3;
            graph_ink(graph, (ACColor){0, 0, 0, (uint8_t)(alpha * 0.82)});
            graph_box(graph, panel_x, panel_y, panel_w, panel_h, 1);
            // Build name (top line, golden)
            if (bname[0]) {
                graph_ink(graph, (ACColor){255, 200, 60, (uint8_t)alpha});
                font_draw_matrix(graph, bname, panel_x + 4, panel_y + 3, 1);
            }
            int line_y = panel_y + (bname[0] ? 11 : 3);
            graph_ink(graph, (ACColor){255, 255, 255, (uint8_t)alpha});
            font_draw_matrix(graph, ver, panel_x + 4, line_y, 1);
            graph_ink(graph, (ACColor){210, 235, 220, (uint8_t)alpha});
            font_draw_matrix(graph, bts, panel_x + 4, line_y + 8, 1);
            // "FRESH" badge when first boot of this version
            if (is_new_version) {
                graph_ink(graph, (ACColor){80, 255, 120, (uint8_t)alpha});
                font_draw_matrix(graph, "FRESH", panel_x - font_measure_matrix("FRESH", 1) - 4, panel_y + 6, 1);
            }
        }
#endif

        // Subtitle appears after frame 130, synced with male TTS at 140
        if (f > 130) {
            double sub_t = (double)(f - 130) / 30.0; // fade in over 0.5s
            if (sub_t > 1.0) sub_t = 1.0;
            int sub_alpha = (int)(180.0 * sub_t);
            graph_ink(graph, (ACColor){180, 140, 160, (uint8_t)sub_alpha});
            int sw = font_measure_matrix("aesthetic.computer", 1);
            font_draw_matrix(graph, "aesthetic.computer",
                             (screen->width - sw) / 2, screen->height / 2 + 10, 1);
        }

        // Bottom: shrinking time bar + W hint
        int bar_full = screen->width - 40;
        int bar_remaining = (int)((1.0 - t) * bar_full);
        if (bar_remaining > 0) {
            graph_ink(graph, (ACColor){200, 150, 180, (uint8_t)(80 * (1.0 - t))});
            graph_box(graph, 20, screen->height - 6, bar_remaining, 3, 1);
        }

        // Show W hint after initial fade
        if (alpha > 100 && show_install) {
            graph_ink(graph, (ACColor){140, 100, 120, (uint8_t)(alpha / 3)});
            const char *hint = is_installed_on_hd()
                ? "W: update disk install" : "W: install to disk";
            int hw = font_measure_matrix(hint, 1);
            font_draw_matrix(graph, hint,
                             (screen->width - hw) / 2, screen->height - 18, 1);
        }

        display_present(display, screen, 3);
        frame_sync_60fps(&anim_time);
    }

    // If W was pressed, show y/n confirmation
    int result = 0;
    if (w_pressed) {
        result = draw_install_confirm(graph, screen, display, key_fds, key_fd_count, tts, audio);
    }

    for (int i = 0; i < key_fd_count; i++) close(key_fds[i]);
    return result;
}

// Draw a status frame during boot (white bg, bouncy title + status text)
// Renders multiple frames with a bounce animation
static void draw_boot_status(ACGraph *graph, ACFramebuffer *screen,
                             ACDisplay *display, const char *status) {
    static int boot_frame = 0;
    int dk = 1; // always dark

    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

    // Animate for 20 frames (~333ms) per status change
    for (int af = 0; af < 20; af++) {
        boot_frame++;
        uint8_t bg = dk ? 20 : 255;
        graph_wipe(graph, (ACColor){bg, bg, (uint8_t)(bg + (dk ? 5 : 0)), 255});

        // Expanding circles — concentric rings radiating from center
        {
            int cx = screen->width / 2;
            int cy = screen->height / 2;
            int max_r = (screen->width > screen->height ? screen->width : screen->height);
            int ring_gap = 18;
            int num_rings = max_r / ring_gap + 2;
            int anim_offset = boot_frame * 2;
            for (int r = 0; r < num_rings; r++) {
                int radius = (r * ring_gap + anim_offset) % (max_r + ring_gap);
                uint8_t v = (r % 2 == 0) ? 240 : 10;
                uint8_t alpha = (uint8_t)(60 - (radius * 30 / max_r));
                if (alpha > 60) alpha = 0;
                graph_ink(graph, (ACColor){v, v, v, alpha});
                graph_circle(graph, cx, cy, radius, 0);
                graph_circle(graph, cx, cy, radius + 1, 0);
            }
        }

        // Bounce: title oscillates with a decaying sine
        double bounce_t = (double)af / 20.0;
        int bounce_y = (int)(6.0 * sin(bounce_t * 3.14159 * 2) * (1.0 - bounce_t));

        // Title: boot_title with per-handle colors
        int tw = font_measure_matrix(boot_title, 3);
        int tx = (screen->width - tw) / 2;
        int ty = screen->height / 2 - 20 + bounce_y;
        for (int ci = 0; boot_title[ci]; ci++) {
            ACColor cc = title_char_color(ci, boot_frame, 255);
            graph_ink(graph, cc);
            char ch[2] = { boot_title[ci], 0 };
            tx = font_draw_matrix(graph, ch, tx, ty, 3);
        }

        // Subtitle (slight counter-bounce)
        uint8_t sub = dk ? 140 : 120;
        graph_ink(graph, (ACColor){sub, sub, sub, 255});
        int sw = font_measure_matrix("aesthetic.computer", 1);
        font_draw_matrix(graph, "aesthetic.computer",
                         (screen->width - sw) / 2,
                         screen->height / 2 + 10 - bounce_y / 3, 1);

        // Status text — slides in from right
        if (status) {
            int slide = (int)((1.0 - bounce_t) * 40);
            if (slide < 0) slide = 0;
            uint8_t sc = dk ? 120 : 160;
            graph_ink(graph, (ACColor){sc, sc, sc, (uint8_t)(255 * bounce_t)});
            int stw = font_measure_matrix(status, 1);
            font_draw_matrix(graph, status,
                             (screen->width - stw) / 2 + slide,
                             screen->height / 2 + 26, 1);
        }

        // Spinning dot indicator (rotates each boot_frame)
        {
            int cx = screen->width / 2;
            int cy = screen->height / 2 + 42;
            double angle = boot_frame * 0.15;
            for (int d = 0; d < 4; d++) {
                double a = angle + d * 1.5708; // 90° apart
                int dx = (int)(6.0 * cos(a));
                int dy = (int)(3.0 * sin(a));
                uint8_t bright = (d == 0) ? (dk ? 200 : 40) : (dk ? 80 : 180);
                graph_ink(graph, (ACColor){bright, bright, bright, 255});
                graph_box(graph, cx + dx - 1, cy + dy - 1, 2, 2, 1);
            }
        }

        display_present(display, screen, 3);
        frame_sync_60fps(&anim_time);
    }
}

int main(int argc, char *argv[]) {
    struct timespec boot_start;
    clock_gettime(CLOCK_MONOTONIC, &boot_start);

    // Mount filesystems if PID 1
    if (getpid() == 1) {
        mount_minimal_fs();
        // Ensure PATH includes all standard binary directories
        setenv("PATH", "/bin:/sbin:/usr/bin:/usr/sbin", 1);
        // Keep curl/OpenSSL trust lookup stable in initramfs.
        setenv("SSL_CERT_FILE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("CURL_CA_BUNDLE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("SSL_CERT_DIR", "/etc/ssl/certs", 1);
    }

    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);

    // Determine piece path (ignore kernel cmdline args passed to init)
    const char *piece_path = "/piece.mjs";
    int headless = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--headless") == 0) headless = 1;
        else if (argv[i][0] == '/' || argv[i][0] == '.') piece_path = argv[i];
    }

    ACDisplay *display = NULL;
    ACFramebuffer *screen = NULL;
    ACInput *input = NULL;
    int pixel_scale = 3;  // Default: 1/3 display resolution (3x nearest-neighbor)

    if (!headless) {
        display = drm_init();
        if (!display) {
            fprintf(stderr, "[ac-native] FATAL: No display\n");
            if (getpid() == 1) { sleep(5); reboot(LINUX_REBOOT_CMD_POWER_OFF); }
            return 1;
        }

        screen = fb_create(display->width / pixel_scale, display->height / pixel_scale);
        if (!screen) {
            drm_destroy(display);
            return 1;
        }
    } else {
        screen = fb_create(320, 200);
    }

    // Init graphics + font
    ACGraph graph;
    graph_init(&graph, screen);
    font_init();

    // Mount USB log early so boot animation can detect USB boot
    if (getpid() == 1) try_mount_log();

    // Read boot visuals (handle + optional per-char colors) from /mnt/config.json
    load_boot_visual_config();

    // Init audio + TTS early (needed for boot animation speech)
    ACAudio *audio = audio_init();
    // Load persisted sample (overrides default seed if file exists)
    if (audio && audio_sample_load(audio, "/mnt/ac-sample.raw") > 0) {
        ac_log("[audio] loaded persisted sample (%d samples)\n", audio->sample_len);
    }
    audio_boot_beep(audio);
    ACTts *tts = tts_init(audio);

    // Startup fade animation (black → white, hides kernel text)
    // TTS speaks "notepat" and "aesthetic.computer" in sync with visuals
    // Returns 1 if user held W to request disk install
    int want_install = 0;
    if (!headless) {
        want_install = draw_startup_fade(&graph, screen, display, tts, audio);
        if (!want_install)
            draw_boot_status(&graph, screen, display, "starting input...");
        // status beep instead of TTS (boot sounds handle it)
    }

    // Init input
    if (!headless) {
        input = input_init(display->width, display->height, pixel_scale);
    }

    // Find backlight path (scan /sys/class/backlight/ for any device)
    char bl_path[128] = "";
    int bl_max = 0;
    {
        DIR *bldir = opendir("/sys/class/backlight");
        if (bldir) {
            struct dirent *ent;
            while ((ent = readdir(bldir)) && !bl_path[0]) {
                if (ent->d_name[0] == '.') continue;
                char tmp[160];
                snprintf(tmp, sizeof(tmp), "/sys/class/backlight/%s/max_brightness", ent->d_name);
                FILE *f = fopen(tmp, "r");
                if (f) {
                    if (fscanf(f, "%d", &bl_max) == 1 && bl_max > 0) {
                        snprintf(bl_path, sizeof(bl_path), "/sys/class/backlight/%s", ent->d_name);
                    }
                    fclose(f);
                }
            }
            closedir(bldir);
        }
        if (bl_path[0])
            fprintf(stderr, "[ac-native] Backlight: %s (max=%d)\n", bl_path, bl_max);
        else
            fprintf(stderr, "[ac-native] No backlight found\n");
    }

    // Install kernel to internal drive (only if user held W during boot)
    if (getpid() == 1 && want_install) {
        int install_ok = auto_install_to_hd(&graph, screen, display);
        int should_reboot = 1;
        if (!headless && display)
            should_reboot = draw_install_reboot_prompt(&graph, screen, display, input, tts, audio, install_ok);

        // Successful install should never continue USB boot into the piece.
        if (install_ok) should_reboot = 1;

        if (should_reboot && getpid() == 1) {
            if (tts) {
                tts_speak(tts, "rebooting");
                tts_wait(tts);
            }
            audio_shutdown_sound(audio);
            usleep(600000); // let chime play out
            sync();
            reboot(LINUX_REBOOT_CMD_RESTART);
            // If reboot syscall fails, hold instead of continuing from USB.
            while (running) sleep(1);
        }
    }

    // Init WiFi
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "starting wifi...");
    // wifi start: audio handled by boot beep sequence
    ACWifi *wifi = wifi_init();

    // Init secondary HDMI display (if connected)
    ACSecondaryDisplay *hdmi = NULL;
    if (display && !display->is_fbdev) {
        hdmi = drm_init_secondary(display);
        if (hdmi) ac_log("[ac-native] HDMI secondary: %dx%d\n", hdmi->width, hdmi->height);
    }

    // Init JS
    ACRuntime *rt = js_init(&graph, input, audio, wifi, tts);
    if (!rt) {
        fprintf(stderr, "[ac-native] FATAL: Cannot init JS\n");
        wifi_destroy(wifi);
        audio_destroy(audio);
        fb_destroy(screen);
        if (display) drm_destroy(display);
        return 1;
    }
    rt->hdmi = hdmi;

    // Read user config from EFI partition (/mnt/config.json)
    {
        FILE *cfg = fopen("/mnt/config.json", "r");
        if (cfg) {
            char buf[4096] = {0};
            size_t n = fread(buf, 1, sizeof(buf) - 1, cfg);
            buf[n] = '\0';
            fclose(cfg);
            parse_config_string(buf, "\"handle\"", rt->handle, sizeof(rt->handle));
            parse_config_string(buf, "\"piece\"", rt->piece, sizeof(rt->piece));
            ac_log("[ac-native] Config: handle=%s piece=%s\n",
                   rt->handle[0] ? rt->handle : "(none)",
                   rt->piece[0] ? rt->piece : "(none)");
        }
    }

    // Load piece
    if (js_load_piece(rt, piece_path) < 0) {
        fprintf(stderr, "[ac-native] FATAL: Cannot load %s\n", piece_path);
        if (!headless) {
            graph_wipe(&graph, (ACColor){200, 0, 0, 255});
            graph_ink(&graph, (ACColor){255, 255, 255, 255});
            char msg[256];
            snprintf(msg, sizeof(msg), "Cannot load: %s", piece_path);
            font_draw(&graph, msg, 20, 20, 2);
            display_present(display, screen, pixel_scale);
            sleep(10);
        }
        js_destroy(rt);
        audio_destroy(audio);
        fb_destroy(screen);
        if (display) drm_destroy(display);
        if (logfile) { fclose(logfile); logfile = NULL; }
        sync();
        if (getpid() == 1) reboot(LINUX_REBOOT_CMD_POWER_OFF);
        return 1;
    }

    struct timespec boot_end;
    clock_gettime(CLOCK_MONOTONIC, &boot_end);
    double boot_ms = (boot_end.tv_sec - boot_start.tv_sec) * 1000.0 +
                     (boot_end.tv_nsec - boot_start.tv_nsec) / 1000000.0;
    ac_log("[ac-native] Booted in %.1fms\n", boot_ms);

    // Log audio and backlight status
    ac_log("[ac-native] Audio: %s\n", audio && audio->pcm ? "OK" : "NO PCM");
    ac_log("[ac-native] Backlight: %s (max=%d)\n", bl_path[0] ? bl_path : "none", bl_max);
    ac_log("[ac-native] Input devices: %d\n", input ? input->count : 0);
    ac_log("[ac-native] NuPhy: has_analog=%d hidraw=%d\n",
           input ? input->has_analog : -1, input ? input->hidraw_count : -1);
    ac_log("[ac-native] JS: boot=%d paint=%d act=%d sim=%d\n",
           JS_IsFunction(rt->ctx, rt->boot_fn),
           JS_IsFunction(rt->ctx, rt->paint_fn),
           JS_IsFunction(rt->ctx, rt->act_fn),
           JS_IsFunction(rt->ctx, rt->sim_fn));
    if (logfile) { fflush(logfile); }

    // Ready: wait for speech to finish + ring buffer to drain, then melody
    if (tts) {
        tts_wait(tts);
        usleep(300000); // Let TTS ring buffer drain
    }
    audio_ready_melody(audio);
    usleep(400000); // Let melody ring out before playing

    // Prewarm audio engine so first keypress has zero latency
    audio_prewarm(audio);

    // Drain any queued input events from boot animation (prevents first-key stick)
    input_poll(input);
    input->event_count = 0;

    // Call boot()
    js_call_boot(rt);

    // Init performance logger
    perf_init();

    if (headless) {
        for (int i = 0; i < 10 && running; i++) {
            js_call_sim(rt);
            js_call_paint(rt);
        }
    } else {
        // Main loop
        struct timespec frame_time;
        clock_gettime(CLOCK_MONOTONIC, &frame_time);

        int main_frame = 0;
        while (running) {
            input_poll(input);
            main_frame++;
            // Log input state periodically (every 5 sec)
            if (logfile && main_frame % 300 == 1) {
                int analog_active = 0;
                for (int k = 0; k < MAX_ANALOG_KEYS; k++)
                    if (input->analog_keys[k].active) analog_active++;
                ac_log("[input] frame=%d events=%d has_analog=%d hidraw=%d analog_active=%d evdev=%d\n",
                       main_frame, input->event_count, input->has_analog,
                       input->hidraw_count, analog_active, input->count);
            }
            // Log key events to USB
            for (int i = 0; i < input->event_count; i++) {
                if (logfile && (input->events[i].type == AC_EVENT_KEYBOARD_DOWN ||
                                input->events[i].type == AC_EVENT_KEYBOARD_UP)) {
                    ac_log("[key] %s code=%d name=%s pressure=%.3f\n",
                           input->events[i].type == AC_EVENT_KEYBOARD_DOWN ? "DOWN" : "UP",
                           input->events[i].key_code,
                           input->events[i].key_name,
                           input->events[i].pressure);
                }
            }
            // Hardware keys
            static int ctrl_held = 0;
            int power_pressed = 0;
            int scale_change = 0;  // -1 = decrease density (bigger pixels), +1 = increase
            for (int i = 0; i < input->event_count; i++) {
                // Track ctrl modifier
                if (input->events[i].key_code == KEY_LEFTCTRL || input->events[i].key_code == KEY_RIGHTCTRL) {
                    ctrl_held = (input->events[i].type == AC_EVENT_KEYBOARD_DOWN) ? 1 : 0;
                }
                if (input->events[i].type == AC_EVENT_KEYBOARD_DOWN) {
                    // Ctrl+= (or Ctrl++) → bigger pixels (increase scale number)
                    // Ctrl+- → smaller pixels (decrease scale number)
                    if (ctrl_held && (input->events[i].key_code == KEY_EQUAL ||
                                      input->events[i].key_code == KEY_KPPLUS)) {
                        scale_change = -1;  // bigger pixels (lower res)
                        input->events[i].type = 0; // suppress from JS
                    } else if (ctrl_held && (input->events[i].key_code == KEY_MINUS ||
                                             input->events[i].key_code == KEY_KPMINUS)) {
                        scale_change = 1;  // smaller pixels (higher res)
                        input->events[i].type = 0; // suppress from JS
                    } else if (ctrl_held && input->events[i].key_code == KEY_0) {
                        scale_change = 99;  // reset to default (3)
                        input->events[i].type = 0; // suppress from JS
                    }
                    // Volume: KEY_VOLUMEUP/DOWN/MUTE or F1/F2/F3 as fallback
                    else if (strcmp(input->events[i].key_name, "audiovolumeup") == 0 ||
                        strcmp(input->events[i].key_name, "f3") == 0) {
                        audio_volume_adjust(audio, 1);
                        audio_synth(audio, WAVE_SINE, 1200.0, 0.04, 0.15, 0.001, 0.03, 0.0);
                    } else if (strcmp(input->events[i].key_name, "audiovolumedown") == 0 ||
                             strcmp(input->events[i].key_name, "f2") == 0) {
                        audio_volume_adjust(audio, -1);
                        audio_synth(audio, WAVE_SINE, 800.0, 0.04, 0.15, 0.001, 0.03, 0.0);
                    } else if (strcmp(input->events[i].key_name, "audiomute") == 0 ||
                             strcmp(input->events[i].key_name, "f1") == 0) {
                        audio_volume_adjust(audio, 0);
                        audio_synth(audio, WAVE_SINE, 440.0, 0.08, 0.15, 0.001, 0.07, 0.0);
                    // Brightness: KEY_BRIGHTNESS or F5/F6 as fallback
                    } else if ((strcmp(input->events[i].key_name, "brightnessup") == 0 ||
                              strcmp(input->events[i].key_name, "brightnessdown") == 0 ||
                              strcmp(input->events[i].key_name, "f6") == 0 ||
                              strcmp(input->events[i].key_name, "f5") == 0) && bl_path[0]) {
                        int up = (strcmp(input->events[i].key_name, "brightnessup") == 0 ||
                                  strcmp(input->events[i].key_name, "f6") == 0);
                        char tmp[160];
                        snprintf(tmp, sizeof(tmp), "%s/brightness", bl_path);
                        FILE *f = fopen(tmp, "r");
                        int cur = bl_max / 2;
                        if (f) { fscanf(f, "%d", &cur); fclose(f); }
                        int step = bl_max / 20; // 5% steps
                        if (step < 1) step = 1;
                        cur += up ? step : -step;
                        if (cur < 1) cur = 1; // never fully off
                        if (cur > bl_max) cur = bl_max;
                        f = fopen(tmp, "w");
                        if (f) { fprintf(f, "%d", cur); fclose(f); }
                        // Click sound: higher = brighter, lower = dimmer
                        audio_synth(audio, WAVE_SINE, up ? 1000.0 : 600.0,
                                    0.04, 0.12, 0.001, 0.03, 0.0);
                    }
                    else if (strcmp(input->events[i].key_name, "power") == 0 ||
                             input->events[i].key_code == KEY_POWER)
                        power_pressed = 1;
                }
            }

            // Pixel density scale change
            if (scale_change && display) {
                int new_scale = pixel_scale;
                if (scale_change == 99) {
                    new_scale = 3;  // reset to default
                } else if (scale_change == 1) {
                    // Increase density (smaller pixels): 12→10→8→6→4→3→2→1
                    if (pixel_scale > 6) new_scale = pixel_scale - 2;
                    else if (pixel_scale > 3) new_scale = pixel_scale - 2;
                    else if (pixel_scale > 1) new_scale = pixel_scale - 1;
                } else if (scale_change == -1) {
                    // Decrease density (bigger pixels): 1→2→3→4→6→8→10→12
                    if (pixel_scale < 3) new_scale = pixel_scale + 1;
                    else if (pixel_scale < 6) new_scale = pixel_scale + 2;
                    else new_scale = pixel_scale + 2;
                }
                if (new_scale != pixel_scale && new_scale >= 1 && new_scale <= 12) {
                    pixel_scale = new_scale;
                    // Recreate framebuffer at new resolution
                    ACFramebuffer *new_screen = fb_create(display->width / pixel_scale,
                                                          display->height / pixel_scale);
                    if (new_screen) {
                        fb_destroy(screen);
                        screen = new_screen;
                        graph_init(&graph, screen);
                        input->scale = pixel_scale;
                        // Update JS runtime's graph reference (holds framebuffer)
                        if (rt) {
                            rt->graph = &graph;
                        }
                        ac_log("[scale] pixel_scale=%d resolution=%dx%d",
                               pixel_scale, screen->width, screen->height);
                        audio_synth(audio, WAVE_SINE,
                                    440.0 + (6 - pixel_scale) * 150.0,
                                    0.06, 0.15, 0.002, 0.05, 0.0);
                    }
                }
            }

            if (power_pressed) {
                // Say bye IMMEDIATELY (TTS + shutdown chime start before animation)
                char bye_title[80];
                {
                    const char *at = strchr(boot_title, '@');
                    if (at)
                        snprintf(bye_title, sizeof(bye_title), "bye @%s", at + 1);
                    else
                        snprintf(bye_title, sizeof(bye_title), "bye");
                    char bye_speech[96];
                    if (at)
                        snprintf(bye_speech, sizeof(bye_speech), "bye %s", at + 1);
                    else
                        snprintf(bye_speech, sizeof(bye_speech), "bye");
                    if (tts) tts_speak(tts, bye_speech);
                    audio_shutdown_sound(audio);
                }

                // Shutdown animation — chaotic red/white strobe with bye title
                struct timespec anim_time;
                clock_gettime(CLOCK_MONOTONIC, &anim_time);

                for (int f = 0; f < 90; f++) { // 90 frames @ 60fps = 1.5s
                    double t = (double)f / 90.0;

                    // Chaotic strobe: alternate red/white/black with randomish pattern
                    int phase = (f * 7 + f / 3) % 6; // pseudo-random cycle
                    uint8_t br, bg_g, bb;
                    if (phase < 2) { br = 220; bg_g = 20; bb = 20; }       // red
                    else if (phase < 3) { br = 255; bg_g = 255; bb = 255; } // white
                    else if (phase < 5) { br = 180; bg_g = 0; bb = 0; }     // dark red
                    else { br = 10; bg_g = 10; bb = 10; }                   // near black

                    // Fade intensity toward end
                    double fade = 1.0 - t * t;
                    br = (uint8_t)(br * fade);
                    bg_g = (uint8_t)(bg_g * fade);
                    bb = (uint8_t)(bb * fade);
                    graph_wipe(&graph, (ACColor){br, bg_g, bb, 255});

                    // Title text — jitter position, flicker between red and white
                    if (t < 0.85) {
                        int alpha = (int)(255.0 * (1.0 - t / 0.85));
                        int jx = (f * 13 % 7) - 3; // -3 to +3 pixel jitter
                        int jy = (f * 17 % 5) - 2; // -2 to +2
                        uint8_t tr = (f % 3 == 0) ? 255 : 200;
                        uint8_t tg = (f % 3 == 0) ? 255 : 40;
                        uint8_t tb = (f % 3 == 0) ? 255 : 40;
                        graph_ink(&graph, (ACColor){tr, tg, tb, (uint8_t)alpha});
                        int tw = font_measure_matrix(bye_title, 3);
                        font_draw_matrix(&graph, bye_title,
                            (screen->width - tw) / 2 + jx,
                            screen->height / 2 - 20 + jy, 3);
                        graph_ink(&graph, (ACColor){(uint8_t)(120 * fade), 40, 40, (uint8_t)(alpha / 2)});
                        int sw = font_measure_matrix("aesthetic.computer", 1);
                        font_draw_matrix(&graph, "aesthetic.computer",
                            (screen->width - sw) / 2 + jx / 2,
                            screen->height / 2 + 10 + jy / 2, 1);
                    }

                    display_present(display, screen, pixel_scale);
                    frame_sync_60fps(&anim_time);
                }

                // Final black frame
                graph_wipe(&graph, (ACColor){0, 0, 0, 255});
                display_present(display, screen, pixel_scale);

                running = 0;
                break;
            }

            struct timespec _pf_start, _pf_act0, _pf_act1;
            clock_gettime(CLOCK_MONOTONIC, &_pf_start);
            clock_gettime(CLOCK_MONOTONIC, &_pf_act0);
            js_call_act(rt);
            clock_gettime(CLOCK_MONOTONIC, &_pf_act1);

            // Handle piece jump requests from system.jump()
            if (rt->jump_requested) {
                rt->jump_requested = 0;
                ac_log("[ac-native] Jumping to piece: %s\n", rt->jump_target);

                // Call leave() on current piece
                js_call_leave(rt);

                // Free old lifecycle functions
                JS_FreeValue(rt->ctx, rt->boot_fn);  rt->boot_fn = JS_UNDEFINED;
                JS_FreeValue(rt->ctx, rt->paint_fn); rt->paint_fn = JS_UNDEFINED;
                JS_FreeValue(rt->ctx, rt->act_fn);   rt->act_fn = JS_UNDEFINED;
                JS_FreeValue(rt->ctx, rt->sim_fn);   rt->sim_fn = JS_UNDEFINED;
                JS_FreeValue(rt->ctx, rt->leave_fn); rt->leave_fn = JS_UNDEFINED;
                JS_FreeValue(rt->ctx, rt->beat_fn);  rt->beat_fn = JS_UNDEFINED;

                // Clear globalThis lifecycle refs so new piece starts clean
                JSValue global = JS_GetGlobalObject(rt->ctx);
                const char *lc_names[] = {"boot","paint","act","sim","leave","beat","configureAutopat",NULL};
                for (int i = 0; lc_names[i]; i++) {
                    JSAtom a = JS_NewAtom(rt->ctx, lc_names[i]);
                    JS_DeleteProperty(rt->ctx, global, a, 0);
                    JS_FreeAtom(rt->ctx, a);
                }
                JS_FreeValue(rt->ctx, global);

                // Reset counters
                rt->paint_count = 0;
                rt->sim_count = 0;

                // Construct piece path: /pieces/<name>.mjs
                char jump_path[256];
                snprintf(jump_path, sizeof(jump_path), "/pieces/%s.mjs", rt->jump_target);

                // Load new piece
                if (js_load_piece(rt, jump_path) < 0) {
                    ac_log("[ac-native] Failed to load %s, falling back to /piece.mjs\n", jump_path);
                    // Fall back to default piece
                    if (js_load_piece(rt, "/piece.mjs") < 0) {
                        ac_log("[ac-native] FATAL: Cannot reload default piece\n");
                        running = 0;
                        break;
                    }
                }

                // Clear screen and call boot() on new piece
                graph_wipe(&graph, (ACColor){0, 0, 0, 255});
                js_call_boot(rt);
            }

            struct timespec _pf_sim0, _pf_sim1, _pf_paint0, _pf_paint1, _pf_pres0, _pf_pres1;
            if (audio_beat_check(audio)) js_call_beat(rt);
            clock_gettime(CLOCK_MONOTONIC, &_pf_sim0);
            js_call_sim(rt);
            clock_gettime(CLOCK_MONOTONIC, &_pf_sim1);
            clock_gettime(CLOCK_MONOTONIC, &_pf_paint0);
            js_call_paint(rt);

            // Software cursor (AC precise cursor: teal crosshair + shadow + white center)
            if (input && (input->pointer_x || input->pointer_y)) {
                int cx = input->pointer_x / pixel_scale, cy = input->pointer_y / pixel_scale;
                // Shadow (black, offset +1,+1)
                graph_ink(&graph, (ACColor){0, 0, 0, 180});
                graph_line(&graph, cx+1, cy-9, cx+1, cy-4);  // top
                graph_line(&graph, cx+1, cy+6, cx+1, cy+11); // bottom
                graph_line(&graph, cx-9, cy+1, cx-4, cy+1);  // left
                graph_line(&graph, cx+6, cy+1, cx+11, cy+1); // right
                graph_plot(&graph, cx+1, cy+1); // center shadow
                // Teal crosshair
                graph_ink(&graph, (ACColor){0, 255, 255, 255});
                graph_line(&graph, cx, cy-10, cx, cy-5);  // top
                graph_line(&graph, cx, cy+5, cx, cy+10);  // bottom
                graph_line(&graph, cx-10, cy, cx-5, cy);  // left
                graph_line(&graph, cx+5, cy, cx+10, cy);  // right
                // White center dot
                graph_ink(&graph, (ACColor){255, 255, 255, 255});
                graph_plot(&graph, cx, cy);
            }
            clock_gettime(CLOCK_MONOTONIC, &_pf_paint1);

            clock_gettime(CLOCK_MONOTONIC, &_pf_pres0);
            display_present(display, screen, pixel_scale);
            clock_gettime(CLOCK_MONOTONIC, &_pf_pres1);

            // HDMI: render waveform at ~7.5Hz (every 8 frames) — 4K dumb-buf is slow
            if (hdmi && audio && main_frame % 8 == 0) {
                drm_secondary_present_waveform(hdmi, &graph,
                    audio->waveform_left, AUDIO_WAVEFORM_SIZE, audio->waveform_pos);
            }

            // HDMI hotplug detection every ~180 frames (~3s)
            if (main_frame % 180 == 0 && display && !display->is_fbdev) {
                int hdmi_connected = drm_secondary_is_connected(display);
                if (hdmi_connected && !hdmi) {
                    hdmi = drm_init_secondary(display);
                    rt->hdmi = hdmi;
                    if (hdmi) {
                        ac_log("[ac-native] HDMI plugged in: %dx%d\n", hdmi->width, hdmi->height);
                        // HDMI on: rising chord
                        if (audio) { audio_synth(audio, WAVE_SINE, 523.25, 0.15, 0.2, 0.005, 0.12, 0.0);
                                     audio_synth(audio, WAVE_SINE, 783.99, 0.15, 0.15, 0.02,  0.11, 0.0); }
                    }
                } else if (!hdmi_connected && hdmi) {
                    ac_log("[ac-native] HDMI unplugged\n");
                    // HDMI off: descending two-tone
                    if (audio) { audio_synth(audio, WAVE_SINE, 523.25, 0.12, 0.18, 0.005, 0.10, 0.0);
                                 audio_synth(audio, WAVE_SINE, 392.00, 0.12, 0.14, 0.02,  0.09, 0.0); }
                    drm_secondary_destroy(hdmi);
                    hdmi = NULL;
                    rt->hdmi = NULL;
                }
            }

            // ── Record frame perf ──
            {
                struct timespec _pf_end;
                clock_gettime(CLOCK_MONOTONIC, &_pf_end);
                #define TS_US(a, b) (uint16_t)({ \
                    long _d = ((b).tv_sec - (a).tv_sec) * 1000000L + \
                              ((b).tv_nsec - (a).tv_nsec) / 1000L; \
                    _d < 0 ? 0 : (_d > 65535 ? 65535 : _d); })
                int _voices = 0;
                if (audio) {
                    for (int _v = 0; _v < AUDIO_MAX_VOICES; _v++)
                        if (audio->voices[_v].state != VOICE_INACTIVE) _voices++;
                }
                PerfRecord pr = {
                    .frame = (uint32_t)main_frame,
                    .total_us = TS_US(_pf_start, _pf_end),
                    .act_us = TS_US(_pf_act0, _pf_act1),
                    .sim_us = TS_US(_pf_sim0, _pf_sim1),
                    .paint_us = TS_US(_pf_paint0, _pf_paint1),
                    .present_us = TS_US(_pf_pres0, _pf_pres1),
                    .voices = (uint8_t)(_voices > 255 ? 255 : _voices),
                    .events = (uint8_t)(input->event_count > 255 ? 255 : input->event_count),
                    .js_heap_mb = 0,
                    .flags = (uint8_t)((input->pointer_x || input->pointer_y ? 2 : 0)),
                };
                perf_record(&pr);
                // Flush chunk to disk every 30 seconds (fsync'd, crash-safe)
                if (main_frame - perf_flush_frame >= PERF_CHUNK_FRAMES) {
                    perf_flush();
                    perf_flush_frame = main_frame;
                }
                #undef TS_US
            }

            frame_sync_60fps(&frame_time);
        }
    }

    // Flush final perf data
    perf_destroy();

    // Cleanup (TTS bye + shutdown chime already fired at power-press time)
    ac_log("[ac-native] Shutting down\n");

    if (logfile) { fclose(logfile); logfile = NULL; }
    sync();
    // Unmount USB log
    umount("/mnt");

    js_call_leave(rt);
    js_destroy(rt);
    wifi_destroy(wifi);
    if (tts) {
        tts_wait(tts);
        usleep(300000); // Let TTS ring buffer drain
    }
    usleep(600000); // Let shutdown chime ring out
    tts_destroy(tts);
    audio_destroy(audio);
    if (input) input_destroy(input);
    if (hdmi) drm_secondary_destroy(hdmi);
    fb_destroy(screen);
    if (display) drm_destroy(display);

    if (getpid() == 1) {
        sync();
        reboot(LINUX_REBOOT_CMD_POWER_OFF);
    }

    return 0;
}
