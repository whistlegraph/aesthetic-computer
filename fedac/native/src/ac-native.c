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
#include <sys/wait.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <sys/mount.h>
#include <sys/statvfs.h>
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
#include "machines.h"
#ifdef USE_WAYLAND
#include "wayland-display.h"
#endif

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

// Forward declaration — defined after init_log_mount()
extern char g_machine_id[64];

// DRM handoff for xdg-open browser popup
// SIGUSR1 = release DRM master (browser takes over display)
// SIGUSR2 = reboot request from cage child (or reclaim DRM in DRM mode)
static volatile int drm_handoff_release = 0;
static volatile int drm_handoff_reclaim = 0;
static volatile int reboot_requested = 0;
static volatile int poweroff_requested = 0;

static void sigusr_handler(int sig) {
    if (sig == SIGUSR1) drm_handoff_release = 1;
    if (sig == SIGUSR2) reboot_requested = 1;
}

static void sigterm_handler(int sig) {
    (void)sig;
    poweroff_requested = 1;
}

static void signal_handler(int sig) {
    running = 0;

    // Best-effort crash report to /mnt/crash.json for next-boot upload
    if (sig == SIGSEGV || sig == SIGBUS || sig == SIGABRT || sig == SIGFPE) {
        const char *signame = "UNKNOWN";
        switch (sig) {
            case SIGSEGV: signame = "SIGSEGV"; break;
            case SIGBUS:  signame = "SIGBUS";  break;
            case SIGABRT: signame = "SIGABRT"; break;
            case SIGFPE:  signame = "SIGFPE";  break;
        }
        FILE *f = fopen("/mnt/crash.json", "w");
        if (f) {
            time_t now = time(NULL);
            fprintf(f, "{\"signal\":\"%s\",\"machineId\":\"%s\",\"time\":%ld}\n",
                    signame, g_machine_id, (long)now);
            fclose(f);
            sync();
        }
        // Re-raise to get default behavior (core dump / termination)
        signal(sig, SIG_DFL);
        raise(sig);
    }
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
    // Re-mount devtmpfs — safe here because by DRM fallback time i915 is fully loaded,
    // so the fresh devtmpfs will have /dev/dri/card0. (Init does NOT re-mount devtmpfs
    // so cage can see the kernel's original card0 early.)
    mount("devtmpfs", "/dev", "devtmpfs", 0, NULL);
    mkdir("/dev/pts", 0755);
    mount("devpts", "/dev/pts", "devpts", 0, "ptmxmode=0666");
    mkdir("/dev/shm", 0755);
    mount("tmpfs", "/dev/shm", "tmpfs", 0, NULL);
    // Don't re-mount /tmp — init already mounted it and may have put logs there.

    // Enable zram swap (compressed RAM — effectively doubles available memory)
    // Firefox + GTK needs significant memory beyond the initramfs tmpfs
    system("modprobe zram 2>/dev/null; "
           "echo 1G > /sys/block/zram0/disksize 2>/dev/null && "
           "mkswap /dev/zram0 >/dev/null 2>&1 && "
           "swapon /dev/zram0 2>/dev/null");

    // Bring up loopback interface (needed for Claude OAuth callback server)
    system("/bin/ip link set lo up 2>/dev/null || /usr/sbin/ip link set lo up 2>/dev/null || ifconfig lo up 2>/dev/null");

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
                logfile = fopen("/mnt/ac-native.log", "a");
                if (logfile) {
                    // Separator between boots for multi-boot log history
                    fprintf(logfile, "\n=== BOOT %s ===\n", devs[i]);
                    fprintf(logfile, "[ac-native] Log opened on %s (removable=%d)\n", devs[i], rem);
                    fflush(logfile);
                    fsync(fileno(logfile));
                    strncpy(log_dev, devs[i], sizeof(log_dev) - 1);
                    fprintf(stderr, "[ac-native] Log: %s -> /mnt/ac-native.log (removable=%d)\n", devs[i], rem);
                    // Log available block devices for storage diagnostics
                    {
                        const char *bdevs[] = {"sda","sdb","sdc","sdd","nvme0n1","nvme1n1","mmcblk0","mmcblk1",NULL};
                        for (int b = 0; bdevs[b]; b++) {
                            char bp[48]; snprintf(bp, sizeof(bp), "/sys/block/%s", bdevs[b]);
                            if (access(bp, F_OK) == 0) {
                                int brem = is_removable(bdevs[b]);
                                ac_log("[storage] /dev/%s removable=%d\n", bdevs[b], brem);
                            }
                        }
                    }
                    // Dump init debug lines to USB from /tmp/ac-init.log (written by init)
                    // and also try kmsg as backup
                    {
                        FILE *initlog = fopen("/mnt/init.log", "w");
                        if (initlog) {
                            // Diagnostics: what does /tmp look like?
                            fprintf(initlog, "diag: pid=%d\n", getpid());
                            fprintf(initlog, "diag: /tmp/ac-init.log access=%d\n",
                                    access("/tmp/ac-init.log", F_OK));
                            fprintf(initlog, "diag: /tmp/cage-stderr.log access=%d\n",
                                    access("/tmp/cage-stderr.log", F_OK));
                            // List /tmp contents
                            DIR *tmpdir = opendir("/tmp");
                            if (tmpdir) {
                                struct dirent *te;
                                while ((te = readdir(tmpdir)) != NULL) {
                                    if (te->d_name[0] != '.')
                                        fprintf(initlog, "diag: /tmp/%s\n", te->d_name);
                                }
                                closedir(tmpdir);
                            } else {
                                fprintf(initlog, "diag: opendir /tmp failed\n");
                            }
                            // Primary: read tmpfs log written by init script
                            FILE *tmplog = fopen("/tmp/ac-init.log", "r");
                            if (tmplog) {
                                char kbuf[512];
                                while (fgets(kbuf, sizeof(kbuf), tmplog))
                                    fputs(kbuf, initlog);
                                fclose(tmplog);
                            }
                            // Also dump cage stderr if it exists
                            FILE *cage_err = fopen("/tmp/cage-stderr.log", "r");
                            if (cage_err) {
                                fprintf(initlog, "--- cage stderr ---\n");
                                char kbuf[512];
                                while (fgets(kbuf, sizeof(kbuf), cage_err))
                                    fputs(kbuf, initlog);
                                fclose(cage_err);
                            }
                            // Backup: scan kmsg for ac-init lines
                            int kmsg = open("/dev/kmsg", O_RDONLY | O_NONBLOCK);
                            if (kmsg >= 0) {
                                lseek(kmsg, 0, SEEK_SET);
                                char kbuf[512];
                                ssize_t r;
                                int found = 0;
                                while ((r = read(kmsg, kbuf, sizeof(kbuf) - 1)) > 0) {
                                    kbuf[r] = 0;
                                    if (strstr(kbuf, "ac-init:")) {
                                        if (!found) { fprintf(initlog, "--- kmsg ---\n"); found = 1; }
                                        char *msg = strstr(kbuf, "ac-init:");
                                        fprintf(initlog, "%s\n", msg);
                                    }
                                }
                                close(kmsg);
                            }
                            fflush(initlog);
                            fsync(fileno(initlog));
                            fclose(initlog);
                        }
                    }
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

// ── Persistent machine ID ──
// Generated on first boot, read back on subsequent boots.
// Accessible from js-bindings.c via extern.
char g_machine_id[64] = {0};
static ACMachines g_machines = {0};

static void init_machine_id(void) {
    FILE *f = fopen("/mnt/.machine-id", "r");
    if (f) {
        if (fgets(g_machine_id, sizeof(g_machine_id), f)) {
            char *nl = strchr(g_machine_id, '\n');
            if (nl) *nl = '\0';
        }
        fclose(f);
        ac_log("[machine] ID loaded: %s\n", g_machine_id);
    } else {
        unsigned int rval = 0;
        FILE *urand = fopen("/dev/urandom", "r");
        if (urand) {
            fread(&rval, sizeof(rval), 1, urand);
            fclose(urand);
        } else {
            rval = (unsigned int)(time(NULL) ^ getpid());
        }
        snprintf(g_machine_id, sizeof(g_machine_id), "notepat-%08x", rval);
        f = fopen("/mnt/.machine-id", "w");
        if (f) {
            fprintf(f, "%s\n", g_machine_id);
            fclose(f);
            ac_log("[machine] New ID generated: %s\n", g_machine_id);
        } else {
            ac_log("[machine] WARNING: Could not write /mnt/.machine-id\n");
        }
    }
}

// Forward declarations for time-of-day functions (defined later)
static int get_la_offset(void);
static int get_la_hour(void);

// Global display pointer — exposed to js-bindings for browser DRM handoff
void *g_display = NULL;

#ifdef USE_WAYLAND
// Global Wayland display — used by ac_display_present dispatch
static ACWaylandDisplay *g_wayland_display = NULL;
#endif

// Unified display present — dispatches to Wayland or DRM backend
static void ac_display_present(ACDisplay *display, ACFramebuffer *screen, int scale) {
#ifdef USE_WAYLAND
    if (g_wayland_display) {
        wayland_display_present(g_wayland_display, screen, scale);
        return;
    }
#endif
    display_present(display, screen, scale);
}

// DRM master release/acquire (defined in drm-display.c)
extern int drm_release_master(void *display);
extern int drm_acquire_master(void *display);

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
        setenv("AC_HANDLE", handle, 1);
        if ((int)strlen(handle) < (int)sizeof(boot_title) - 20) {
            // Time-of-day greeting based on LA time
            int hour = get_la_hour();
            const char *greeting;
            if (hour >= 5 && hour < 12)       greeting = "good morning";
            else if (hour >= 12 && hour < 17)  greeting = "good afternoon";
            else                               greeting = "good evening";
            snprintf(boot_title, sizeof(boot_title), "%s @%s", greeting, handle);
        }
    }
    parse_boot_title_colors(buf);

    // Extract claudeCreds JSON and write to /tmp for PTY to pick up
    const char *cc = strstr(buf, "\"claudeCreds\"");
    if (cc) {
        const char *start = strchr(cc, '{');
        if (start) {
            int depth = 0;
            const char *end = start;
            while (*end) {
                if (*end == '{') depth++;
                else if (*end == '}') { depth--; if (depth == 0) { end++; break; } }
                end++;
            }
            if (depth == 0 && end > start) {
                mkdir("/tmp/.claude", 0755);
                FILE *cf = fopen("/tmp/.claude/.credentials.json", "w");
                if (cf) {
                    fwrite(start, 1, end - start, cf);
                    fclose(cf);
                    ac_log("[ac-native] Claude credentials written (%d bytes)\n", (int)(end - start));
                }
            }
        }
    }

    // Extract claudeState JSON and write to /tmp/.claude.json
    const char *cs = strstr(buf, "\"claudeState\"");
    if (cs) {
        const char *start = strchr(cs, '{');
        if (start) {
            int depth = 0;
            const char *end = start;
            while (*end) {
                if (*end == '{') depth++;
                else if (*end == '}') { depth--; if (depth == 0) { end++; break; } }
                end++;
            }
            if (depth == 0 && end > start) {
                FILE *sf = fopen("/tmp/.claude.json", "w");
                if (sf) {
                    fwrite(start, 1, end - start, sf);
                    fclose(sf);
                    ac_log("[ac-native] Claude state written (%d bytes)\n", (int)(end - start));
                }
            }
        }
    }

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
    // Map palette to the handle portion (everything after @)
    const char *at = strchr(boot_title, '@');
    int handle_start = at ? (int)(at - boot_title) + 1 : 0; // char after @
    if (handle_start > 0 && ci >= handle_start && boot_title_colors_len > 0) {
        idx = ci - handle_start;
    } else if (ci < handle_start) {
        // greeting prefix and "@" get rainbow colors
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

    ac_log("[install] auto_install_to_hd starting\n");
    if (display)
        draw_boot_status(graph, screen, display, "installing to disk...");

    // Detect source layout: monolithic (BOOTX64.EFI is kernel) or
    // systemd-boot (BOOTX64.EFI is bootloader, kernel at EFI/Linux/*)
    int systemd_boot_layout = 0;

    // Prefer current /mnt only when it is removable and has a bootable layout.
    if (log_dev[0]) {
        char blk[32] = "";
        get_parent_block(log_dev + 5, blk, sizeof(blk));
        if (blk[0] && is_removable(blk) == 1 &&
            (access("/mnt/EFI/BOOT/BOOTX64.EFI", F_OK) == 0 ||
             access("/mnt/EFI/Linux/vmlinuz-ac-native", F_OK) == 0)) {
            strncpy(source_dev, log_dev, sizeof(source_dev) - 1);
            source_dev[sizeof(source_dev) - 1] = '\0';
        }
    }

    // Fallback: scan removable partitions for bootable layout
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
            if (access("/tmp/src/EFI/BOOT/BOOTX64.EFI", F_OK) == 0 ||
                access("/tmp/src/EFI/Linux/vmlinuz-ac-native", F_OK) == 0) {
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

    // Detect systemd-boot layout (separate kernel + initramfs + loader config)
    {
        char sbl_check[128];
        snprintf(sbl_check, sizeof(sbl_check), "%s/EFI/Linux/vmlinuz-ac-native", source_mount);
        if (access(sbl_check, F_OK) == 0) {
            systemd_boot_layout = 1;
            ac_log("[install] Detected systemd-boot layout\n");
        }
    }

    snprintf(kernel_src, sizeof(kernel_src), "%s/EFI/BOOT/BOOTX64.EFI", source_mount);
    snprintf(config_src, sizeof(config_src), "%s/config.json", source_mount);
    if (!source_dev[0] || (access(kernel_src, F_OK) != 0 && !systemd_boot_layout)) {
        ac_log("[install] No removable install source with kernel found\n");
        // Log available block devices for diagnostics
        ac_log("[install] Block devices:\n");
        const char *scan[] = {"sda","sdb","sdc","sdd","nvme0n1","nvme1n1","mmcblk0",NULL};
        for (int i = 0; scan[i]; i++) {
            char dp[32]; snprintf(dp, sizeof(dp), "/sys/block/%s", scan[i]);
            if (access(dp, F_OK) == 0) {
                int rem = is_removable(scan[i]);
                ac_log("[install]   /dev/%s removable=%d\n", scan[i], rem);
            }
        }
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
            ac_log("[install] trying %s\n", devpath);
            if (mount(devpath, "/tmp/hd", "vfat", 0, NULL) != 0) {
                ac_log("[install] mount failed: %s (errno=%d)\n", devpath, errno);
                continue;
            }

            // Create EFI boot directories
            mkdir("/tmp/hd/EFI", 0755);
            mkdir("/tmp/hd/EFI/BOOT", 0755);

            // Check free space and kernel size before copy
            {
                struct stat ksrc_st;
                struct statvfs hd_vfs;
                long kernel_bytes = 0, free_mb = 0;
                if (stat(kernel_src, &ksrc_st) == 0) kernel_bytes = ksrc_st.st_size;
                if (statvfs("/tmp/hd", &hd_vfs) == 0)
                    free_mb = (long)hd_vfs.f_bavail * (long)hd_vfs.f_bsize / 1048576;
                ac_log("[install] kernel=%ldMB free=%ldMB on %s\n",
                       kernel_bytes / 1048576, free_mb, devpath);
                if (kernel_bytes > 0 && free_mb < kernel_bytes / 1048576 + 10) {
                    long need_mb = kernel_bytes / 1048576 + 10;
                    ac_log("[install] NOT ENOUGH SPACE — need %ldMB, have %ldMB\n", need_mb, free_mb);
                    // Repartition: expand ESP to 1024MB
                    char parent_blk[32] = "";
                    // Extract parent device: /dev/nvme0n1p1 → nvme0n1, /dev/sda1 → sda
                    {
                        const char *d = devpath + 5; // skip "/dev/"
                        strncpy(parent_blk, d, sizeof(parent_blk) - 1);
                        // Remove partition suffix: "nvme0n1p1" → "nvme0n1", "sda1" → "sda"
                        char *pp = strstr(parent_blk, "p");
                        if (pp && pp > parent_blk && *(pp-1) >= '0' && *(pp-1) <= '9' && *(pp+1) >= '1' && *(pp+1) <= '9')
                            *pp = 0; // NVMe: nvme0n1p1 → nvme0n1
                        else {
                            // SATA: sda1 → sda (strip trailing digits)
                            int len = strlen(parent_blk);
                            while (len > 0 && parent_blk[len-1] >= '0' && parent_blk[len-1] <= '9') len--;
                            parent_blk[len] = 0;
                        }
                    }
                    ac_log("[install] repartitioning /dev/%s → 1024MB ESP\n", parent_blk);
                    if (display) {
                        char msg[80];
                        snprintf(msg, sizeof(msg), "expanding to 1024MB...");
                        draw_boot_status(graph, screen, display, msg);
                    }
                    umount("/tmp/hd");
                    // Repartition: create 1024MB EFI System Partition
                    char rcmd[256];
                    snprintf(rcmd, sizeof(rcmd),
                        "echo 'label: gpt\ntype=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=1024M' | sfdisk --force /dev/%s 2>&1",
                        parent_blk);
                    int rrc = system(rcmd);
                    ac_log("[install] sfdisk rc=%d\n", rrc);
                    usleep(500000);
                    // Reformat
                    snprintf(rcmd, sizeof(rcmd), "mkfs.vfat -F 32 -n AC-NATIVE %s 2>&1", devpath);
                    rrc = system(rcmd);
                    ac_log("[install] mkfs rc=%d\n", rrc);
                    usleep(500000);
                    // Remount and retry
                    if (mount(devpath, "/tmp/hd", "vfat", 0, NULL) != 0) {
                        ac_log("[install] remount failed after repartition\n");
                        if (display)
                            draw_boot_status(graph, screen, display, "repartition failed!");
                        usleep(2000000);
                        continue;
                    }
                    mkdir("/tmp/hd/EFI", 0755);
                    mkdir("/tmp/hd/EFI/BOOT", 0755);
                    ac_log("[install] repartitioned OK, retrying copy\n");
                }
            }
            // Copy kernel (and initramfs/loader for systemd-boot layout)
            long sz = 0;
            if (systemd_boot_layout) {
                // systemd-boot: copy bootloader, kernel, initramfs, and loader config
                char src[256], dst[256];

                // Copy bootloader as BOOTX64.EFI
                snprintf(src, sizeof(src), "%s/EFI/BOOT/BOOTX64.EFI", source_mount);
                sz = copy_file(src, "/tmp/hd/EFI/BOOT/BOOTX64.EFI");
                ac_log("[install] bootloader: %ld bytes\n", sz);

                // Copy kernel
                mkdir("/tmp/hd/EFI/Linux", 0755);
                snprintf(src, sizeof(src), "%s/EFI/Linux/vmlinuz-ac-native", source_mount);
                long ksz = copy_file(src, "/tmp/hd/EFI/Linux/vmlinuz-ac-native");
                ac_log("[install] kernel: %ld bytes\n", ksz);

                // Copy initramfs
                snprintf(src, sizeof(src), "%s/initramfs.cpio.lz4", source_mount);
                long isz = copy_file(src, "/tmp/hd/initramfs.cpio.lz4");
                ac_log("[install] initramfs: %ld bytes\n", isz);

                // Copy loader config
                mkdir("/tmp/hd/loader", 0755);
                mkdir("/tmp/hd/loader/entries", 0755);
                snprintf(src, sizeof(src), "%s/loader/loader.conf", source_mount);
                copy_file(src, "/tmp/hd/loader/loader.conf");
                snprintf(src, sizeof(src), "%s/loader/entries/ac-native.conf", source_mount);
                copy_file(src, "/tmp/hd/loader/entries/ac-native.conf");

                sz = ksz > 0 ? ksz : sz; // use kernel size as success indicator
            } else {
                // Monolithic: single BOOTX64.EFI is the kernel
                sz = copy_file(kernel_src, "/tmp/hd/EFI/BOOT/BOOTX64.EFI");
                ac_log("[install] copy result: %ld bytes\n", sz);
            }

            if (sz > 0) {
                // Also overwrite Windows Boot Manager path (ThinkPad BIOS often
                // boots this first regardless of boot order) — but only if space permits
                struct statvfs vfs;
                long free_bytes = 0;
                if (statvfs("/tmp/hd", &vfs) == 0)
                    free_bytes = (long)vfs.f_bavail * (long)vfs.f_bsize;
                if (!systemd_boot_layout && free_bytes > sz + 1048576) {
                    mkdir("/tmp/hd/EFI/Microsoft", 0755);
                    mkdir("/tmp/hd/EFI/Microsoft/Boot", 0755);
                    copy_file(kernel_src, "/tmp/hd/EFI/Microsoft/Boot/bootmgfw.efi");
                }

                // Preserve user handle/piece config on installed disk.
                if (access(config_src, F_OK) == 0) {
                    copy_file(config_src, "/tmp/hd/config.json");
                }

                // Write install marker so next boot knows it's installed
                FILE *marker = fopen("/tmp/hd/EFI/BOOT/ac-installed", "w");
                if (marker) { fputs("1\n", marker); fclose(marker); }

                sync();
                installed = 1;
                ac_log("[install] Installed from %s to %s (systemd-boot=%d)\n",
                        source_dev, devpath, systemd_boot_layout);
            }

            umount("/tmp/hd");
        }
    }

    if (installed && display) {
        draw_boot_status(graph, screen, display, "installed to disk!");
        usleep(800000);
    } else if (!installed) {
        ac_log("[install] No suitable HD partition found for install\n");
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

        ac_display_present(display, screen, 3);
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

        ac_display_present(display, screen, 3);
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
        ac_log("[boot-anim] boot_dev=%s parent=%s removable=%d show_install=%d\n",
                log_dev, boot_blk, is_removable(boot_blk), show_install);
    } else if (getpid() == 1) {
        // No log_dev — fallback: show if not installed
        show_install = !is_installed_on_hd();
        ac_log("[boot-anim] no log_dev, show_install=%d (fallback)\n", show_install);
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
    ac_display_present(display, screen, 3);

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

    // Generate or load persistent machine ID (needs /mnt mounted)
    init_machine_id();

    // Initialize machines monitoring daemon
    machines_init(&g_machines);

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

        // Startup greeting — time-of-day + handle + "enjoy Los Angeles" + build name
        if (f == 10 && tts) {
            char greet[256];
            const char *at = strchr(boot_title, '@');
            int hour = get_la_hour();
            const char *tod;
            if (hour >= 5 && hour < 12)       tod = "good morning";
            else if (hour >= 12 && hour < 17)  tod = "good afternoon";
            else                               tod = "good evening";
#ifdef AC_BUILD_NAME
            char name_tts[64];
            strncpy(name_tts, AC_BUILD_NAME, sizeof(name_tts) - 1);
            name_tts[sizeof(name_tts) - 1] = 0;
            for (char *p = name_tts; *p; p++) { if (*p == '-') *p = ' '; }
            if (at)
                snprintf(greet, sizeof(greet), "%s %s. enjoy Los Angeles! %s.", tod, at + 1, name_tts);
            else
                snprintf(greet, sizeof(greet), "%s. %s.", tod, name_tts);
#else
            if (at)
                snprintf(greet, sizeof(greet), "%s %s. enjoy Los Angeles!", tod, at + 1);
            else
                snprintf(greet, sizeof(greet), "%s", tod);
#endif
            tts_speak(tts, greet);
        }
        // W hint is visual only — no TTS

        // Fade from black to time-of-day themed bg (complete in first 0.3s)
        double fade_t = t * 3.33;
        if (fade_t > 1.0) fade_t = 1.0;
        int hour = get_la_hour();
        int target_r, target_g, target_b;
        if (hour >= 5 && hour < 8) {
            target_r = 100; target_g = 45; target_b = 20;  // sunrise orange
        } else if (hour >= 8 && hour < 12) {
            target_r = 25; target_g = 50; target_b = 90;   // morning sky blue
        } else if (hour >= 12 && hour < 17) {
            target_r = 90; target_g = 65; target_b = 20;   // afternoon gold
        } else if (hour >= 17 && hour < 20) {
            target_r = 80; target_g = 25; target_b = 60;   // sunset purple
        } else {
            target_r = 15; target_g = 15; target_b = 40;   // night deep blue
        }
        int bg_r = (int)(target_r * fade_t);
        int bg_g = (int)(target_g * fade_t);
        int bg_b = (int)(target_b * fade_t);
        graph_wipe(graph, (ACColor){(uint8_t)bg_r, (uint8_t)bg_g, (uint8_t)bg_b, 255});

        // Title — per-handle palette (fallback rainbow), animated pulse
        int alpha = (int)(255.0 * fade_t);
        if (alpha > 0) {
            const char *title = boot_title;
            // Auto-scale: use 3x unless title is too wide, then 2x
            int scale = 3;
            int tw = font_measure_matrix(title, scale);
            if (tw > screen->width - 20) {
                scale = 2;
                tw = font_measure_matrix(title, scale);
            }
            int tx = (screen->width - tw) / 2;
            int ty = screen->height / 2 - 20;
            for (int ci = 0; title[ci]; ci++) {
                ACColor cc = title_char_color(ci, f, alpha);
                graph_ink(graph, cc);
                char ch[2] = { title[ci], 0 };
                tx = font_draw_matrix(graph, ch, tx, ty, scale);
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

        // Subtitle: "enjoy Los Angeles!" appears after frame 130
        if (f > 130) {
            double sub_t = (double)(f - 130) / 30.0;
            if (sub_t > 1.0) sub_t = 1.0;
            int sub_alpha = (int)(180.0 * sub_t);
            graph_ink(graph, (ACColor){220, 180, 140, (uint8_t)sub_alpha});
            const char *subtitle = "enjoy Los Angeles!";
            int sw = font_measure_matrix(subtitle, 1);
            font_draw_matrix(graph, subtitle,
                             (screen->width - sw) / 2, screen->height / 2 + 10, 1);
        }

        // Auth badges (bottom-left): pixel crab = Claude, pixel octocat = GitHub
        if (f > 60 && alpha > 80) {
            int badge_x = 6;
            int badge_y = screen->height - 22;
            double badge_t = (double)(f - 60) / 40.0;
            if (badge_t > 1.0) badge_t = 1.0;
            int ba = (int)(220.0 * badge_t); // badge alpha

            // 11x9 pixel crab (Claude/Anthropic)
            if (access("/claude-token", F_OK) == 0 || getenv("CLAUDE_CODE_OAUTH_TOKEN")) {
                static const char crab[9][12] = {
                    " .       . ",
                    "  .     .  ",
                    " ..##.##.. ",
                    ".# #### #.",
                    ". ####### .",
                    "  #######  ",
                    "  ## . ##  ",
                    "  .     .  ",
                    " .       . ",
                };
                for (int cy = 0; cy < 9; cy++)
                    for (int cx = 0; cx < 11; cx++) {
                        char c = crab[cy][cx];
                        if (c == '#')
                            graph_ink(graph, (ACColor){255, 120, 50, (uint8_t)ba});
                        else if (c == '.')
                            graph_ink(graph, (ACColor){200, 90, 30, (uint8_t)(ba*2/3)});
                        else continue;
                        graph_box(graph, badge_x + cx*2, badge_y + cy*2, 2, 2, 1);
                    }
                badge_x += 28;
            }
            // 11x11 pixel octocat (GitHub)
            if (access("/github-pat", F_OK) == 0 || getenv("GH_TOKEN")) {
                static const char octo[11][12] = {
                    "   .###.   ",
                    "  #######  ",
                    " ## o#o ## ",
                    " ######### ",
                    " ## ### ## ",
                    "  #######  ",
                    "   #####   ",
                    "  .# . #.  ",
                    " .#  .  #. ",
                    " .   .   . ",
                    ".    .    .",
                };
                for (int cy = 0; cy < 11; cy++)
                    for (int cx = 0; cx < 11; cx++) {
                        char c = octo[cy][cx];
                        if (c == '#')
                            graph_ink(graph, (ACColor){180, 210, 255, (uint8_t)ba});
                        else if (c == 'o')
                            graph_ink(graph, (ACColor){60, 80, 120, (uint8_t)ba});
                        else if (c == '.')
                            graph_ink(graph, (ACColor){120, 150, 200, (uint8_t)(ba*2/3)});
                        else continue;
                        graph_box(graph, badge_x + cx*2, badge_y + cy*2, 2, 2, 1);
                    }
                badge_x += 28;
            }
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

        ac_display_present(display, screen, 3);
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
    // Time-of-day themed background
    int la_hour = get_la_hour();
    uint8_t bg_r, bg_g, bg_b;
    if (la_hour >= 5 && la_hour < 8) {
        bg_r = 100; bg_g = 45; bg_b = 20;  // sunrise orange
    } else if (la_hour >= 8 && la_hour < 12) {
        bg_r = 25; bg_g = 50; bg_b = 90;   // morning sky
    } else if (la_hour >= 12 && la_hour < 17) {
        bg_r = 90; bg_g = 65; bg_b = 20;   // afternoon gold
    } else if (la_hour >= 17 && la_hour < 20) {
        bg_r = 80; bg_g = 25; bg_b = 60;   // sunset purple
    } else {
        bg_r = 15; bg_g = 15; bg_b = 40;   // night deep blue
    }

    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

    // Animate for 20 frames (~333ms) per status change
    for (int af = 0; af < 20; af++) {
        boot_frame++;
        graph_wipe(graph, (ACColor){bg_r, bg_g, bg_b, 255});

        // Rainbow stripes at top and bottom
        {
            int stripe_h = 3;
            int num_stripes = 8;
            int band = num_stripes * stripe_h;
            for (int s = 0; s < num_stripes; s++) {
                double hue = fmod((double)s / num_stripes * 360.0 + boot_frame * 3.0, 360.0);
                double h6 = hue / 60.0;
                int hi = (int)h6 % 6;
                double fr = h6 - (int)h6;
                double cr, cg, cb;
                switch (hi) {
                    case 0: cr = 1; cg = fr; cb = 0; break;
                    case 1: cr = 1-fr; cg = 1; cb = 0; break;
                    case 2: cr = 0; cg = 1; cb = fr; break;
                    case 3: cr = 0; cg = 1-fr; cb = 1; break;
                    case 4: cr = fr; cg = 0; cb = 1; break;
                    default: cr = 1; cg = 0; cb = 1-fr; break;
                }
                int y_top = s * stripe_h;
                int y_bot = screen->height - band + s * stripe_h;
                graph_ink(graph, (ACColor){(uint8_t)(cr*200), (uint8_t)(cg*200), (uint8_t)(cb*200), 80});
                graph_box(graph, 0, y_top, screen->width, stripe_h, 1);
                graph_box(graph, 0, y_bot, screen->width, stripe_h, 1);
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
        uint8_t sub = 140;
        graph_ink(graph, (ACColor){sub, sub, sub, 255});
        int sw = font_measure_matrix("aesthetic.computer", 1);
        font_draw_matrix(graph, "aesthetic.computer",
                         (screen->width - sw) / 2,
                         screen->height / 2 + 10 - bounce_y / 3, 1);

        // Status text — slides in from right
        if (status) {
            int slide = (int)((1.0 - bounce_t) * 40);
            if (slide < 0) slide = 0;
            uint8_t sc = 120;
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
                uint8_t bright = (d == 0) ? 200 : 80;
                graph_ink(graph, (ACColor){bright, bright, bright, 255});
                graph_box(graph, cx + dx - 1, cy + dy - 1, 2, 2, 1);
            }
        }

        ac_display_present(display, screen, 3);
        frame_sync_60fps(&anim_time);
    }
}

int main(int argc, char *argv[]) {
    struct timespec boot_start;
    clock_gettime(CLOCK_MONOTONIC, &boot_start);

    // Mount filesystems if PID 1 (direct DRM boot)
    if (getpid() == 1) {
        mount_minimal_fs();
        // Ensure PATH includes all standard binary directories
        setenv("PATH", "/bin:/sbin:/usr/bin:/usr/sbin", 1);
        // Keep curl/OpenSSL trust lookup stable in initramfs.
        setenv("SSL_CERT_FILE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("CURL_CA_BUNDLE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("SSL_CERT_DIR", "/etc/ssl/certs", 1);
    } else {
        // Under cage: filesystems already mounted by init script,
        // but still need USB log mount and PATH
        if (!getenv("PATH"))
            setenv("PATH", "/bin:/sbin:/usr/bin:/usr/sbin", 1);
        setenv("SSL_CERT_FILE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("CURL_CA_BUNDLE", "/etc/pki/tls/certs/ca-bundle.crt", 1);
        setenv("SSL_CERT_DIR", "/etc/ssl/certs", 1);
        // Log to USB directly (parent has /mnt mounted, we inherit it)
        // Use a separate file so we don't truncate parent's log
        logfile = fopen("/mnt/cage-child.log", "w");
        if (!logfile) logfile = fopen("/tmp/ac-native-cage.log", "w");
        if (logfile) {
            fprintf(logfile, "[ac-native] Running under cage (pid=%d)\n", getpid());
            fflush(logfile);
        }
    }

    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);
#ifdef USE_WAYLAND
    // Under Wayland: no DRM handoff signals needed (browser is sibling client)
    if (!getenv("WAYLAND_DISPLAY"))
#endif
    {
        signal(SIGUSR1, sigusr_handler);
        signal(SIGUSR2, sigusr_handler);
        signal(SIGTERM, sigterm_handler);
    }

    // Determine piece path (ignore kernel cmdline args passed to init)
    const char *piece_path = "/piece.mjs";
    int headless = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--headless") == 0) headless = 1;
        else if (argv[i][0] == '/' || argv[i][0] == '.') piece_path = argv[i];
    }

    ACDisplay *display = NULL;
    extern void *g_display;  // expose to js-bindings for browser DRM handoff
    ACFramebuffer *screen = NULL;
    ACInput *input = NULL;
    int pixel_scale = 3;  // Default: 1/3 display resolution (3x nearest-neighbor)
#ifdef USE_WAYLAND
    ACWaylandDisplay *wayland_display = NULL;
    int is_wayland = 0;
#endif

    if (!headless) {
#ifdef USE_WAYLAND
        // Prefer Wayland if running under cage compositor
        ac_log("[ac-native] WAYLAND_DISPLAY=%s XDG_RUNTIME_DIR=%s\n",
               getenv("WAYLAND_DISPLAY") ? getenv("WAYLAND_DISPLAY") : "(null)",
               getenv("XDG_RUNTIME_DIR") ? getenv("XDG_RUNTIME_DIR") : "(null)");
        if (getenv("WAYLAND_DISPLAY")) {
            wayland_display = wayland_display_init();
            if (wayland_display) {
                is_wayland = 1;
                g_wayland_display = wayland_display;
                // Create a minimal ACDisplay for code that needs width/height
                display = calloc(1, sizeof(ACDisplay));
                display->width = wayland_display->width;
                display->height = wayland_display->height;
                g_display = display;
                fprintf(stderr, "[ac-native] Wayland display: %dx%d\n",
                        display->width, display->height);
            } else {
                ac_log("[ac-native] Wayland init failed — exiting (cage will restart or fallback)\n");
                return 1;  // exit so cage exits and init falls through to DRM
            }
        }
        if (!is_wayland)
#endif
        {
            display = drm_init();
            g_display = display;
        }
        if (!display) {
            fprintf(stderr, "[ac-native] FATAL: No display\n");
            if (getpid() == 1) { sleep(5); reboot(LINUX_REBOOT_CMD_POWER_OFF); }
            return 1;
        }

        screen = fb_create(display->width / pixel_scale, display->height / pixel_scale);
        if (!screen) {
#ifdef USE_WAYLAND
            if (is_wayland) wayland_display_destroy(wayland_display);
            else
#endif
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

    // Cursor overlay buffer — drawn separately so KidLisp effects don't smear it
    ACFramebuffer *cursor_fb = fb_create(screen->width, screen->height);

    // Mount USB log early so boot animation can detect USB boot
#ifdef USE_WAYLAND
    if (!is_wayland)  // Under cage: parent already has USB mounted at /mnt
#endif
        try_mount_log();

    // Read boot visuals (handle + optional per-char colors) from /mnt/config.json
    load_boot_visual_config();

    // Init audio + TTS early (needed for boot animation speech)
    ACAudio *audio = audio_init();
    ACTts *tts = NULL;
    ACWifi *wifi = NULL;
    ACSecondaryDisplay *hdmi = NULL;
    char bl_path[128] = "";
    int bl_max = 0;

#ifdef USE_WAYLAND
    if (is_wayland) {
        // ── Cage session: skip boot animation, install, wifi (parent did all that) ──
        ac_log("[ac-native] Wayland session — skipping boot sequence\n");

        // Input (Wayland path)
        if (!headless)
            input = input_init_wayland(wayland_display, display->width, display->height, pixel_scale);

        // WiFi is already running from parent — just connect to it
        wifi = wifi_init();
    } else
#endif
    {
        // ── DRM boot: full boot sequence ──
        // Load persisted sample (overrides default seed if file exists)
        if (audio && audio_sample_load(audio, "/mnt/ac-sample.raw") > 0) {
            ac_log("[audio] loaded persisted sample (%d samples)\n", audio->sample_len);
        }
        audio_boot_beep(audio);
        tts = tts_init(audio);
        tts_precache(tts);

        // Startup fade animation (black → white, hides kernel text)
        ac_log("[ac-native] pre-fade: display=%p screen=%p w=%d h=%d\n",
               (void*)display, (void*)screen,
               display ? display->width : -1, display ? display->height : -1);
        int want_install = 0;
        if (!headless) {
            want_install = draw_startup_fade(&graph, screen, display, tts, audio);
            if (!want_install)
                draw_boot_status(&graph, screen, display, "starting input...");
        }

        // Init input (DRM path)
        if (!headless)
            input = input_init(display->width, display->height, pixel_scale);

        // Find backlight path
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
                        if (fscanf(f, "%d", &bl_max) == 1 && bl_max > 0)
                            snprintf(bl_path, sizeof(bl_path), "/sys/class/backlight/%s", ent->d_name);
                        fclose(f);
                    }
                }
                closedir(bldir);
            }
        }

        // Install kernel to internal drive (only if user held W during boot)
        if (getpid() == 1 && want_install) {
            int install_ok = auto_install_to_hd(&graph, screen, display);
            int should_reboot = 1;
            if (!headless && display)
                should_reboot = draw_install_reboot_prompt(&graph, screen, display, input, tts, audio, install_ok);
            if (install_ok) should_reboot = 1;
            if (should_reboot && getpid() == 1) {
                if (tts) { tts_speak(tts, "rebooting"); tts_wait(tts); }
                audio_shutdown_sound(audio);
                usleep(600000);
                sync();
                reboot(LINUX_REBOOT_CMD_RESTART);
                while (running) sleep(1);
            }
        }

        // Init WiFi
        if (!headless && display)
            draw_boot_status(&graph, screen, display, "starting wifi...");
        wifi = wifi_init();

        // Init secondary HDMI display (if connected)
        if (display && !display->is_fbdev) {
            hdmi = drm_init_secondary(display);
            if (hdmi) ac_log("[ac-native] HDMI secondary: %dx%d\n", hdmi->width, hdmi->height);
        }
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

    // Override piece_path from config.json boot piece (pieces bundled at /pieces/)
    // Resolve aliases: "claude" and "cc" → terminal (with param "claude")
    if (strcmp(rt->piece, "claude") == 0 || strcmp(rt->piece, "cc") == 0) {
        strcpy(rt->piece, "terminal");
        // Set initial jump params so terminal.mjs gets "claude" as param
        strcpy(rt->jump_params[0], "claude");
        rt->jump_param_count = 1;
    }
    char piece_path_buf[256];
    if (rt->piece[0]) {
        snprintf(piece_path_buf, sizeof(piece_path_buf), "/pieces/%s.mjs", rt->piece);
        if (access(piece_path_buf, R_OK) == 0) {
            piece_path = piece_path_buf;
            ac_log("[ac-native] Boot piece from config: %s\n", piece_path);
        } else {
            ac_log("[ac-native] Config piece '%s' not found, falling back to %s\n",
                   rt->piece, piece_path);
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
            ac_display_present(display, screen, pixel_scale);
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
    // Log each input device name for debugging keyboard issues
    if (input) {
        for (int i = 0; i < input->count; i++) {
            char dname[256] = "?";
            ioctl(input->fds[i], EVIOCGNAME(sizeof(dname)), dname);
            ac_log("[input] dev%d: %s\n", i, dname);
        }
    }
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

    // ── Graceful DRM → cage transition ──
    // After boot completes in DRM mode, try to launch cage compositor
    // and re-exec ac-native under it. This gives us a Wayland session
    // for browser popups (Firefox, OAuth) while keeping fast DRM boot.
#ifdef USE_WAYLAND
    if (!is_wayland && !headless && getpid() == 1 &&
        access("/bin/cage", X_OK) == 0 && access("/dev/dri/card0", F_OK) == 0) {
        ac_log("[cage-transition] Starting cage compositor...\n");

        // Close audio so cage child can open ALSA
        audio_destroy(audio);
        audio = NULL;

        // Release DRM master so cage can take it
        drm_release_master(display);
        ac_log("[cage-transition] Released DRM master\n");

        pid_t cage_pid = fork();
        if (cage_pid == 0) {
            // === CHILD: start seatd + cage + ac-native ===
            setenv("HOME", "/tmp", 1);
            setenv("XDG_RUNTIME_DIR", "/tmp/xdg", 1);
            setenv("WLR_RENDERER", "pixman", 1);
            setenv("WLR_BACKENDS", "drm", 1);
            setenv("LIBGL_ALWAYS_SOFTWARE", "1", 1);
            setenv("WLR_LIBINPUT_NO_DEVICES", "1", 1);
            mkdir("/tmp/xdg", 0700);

            // Log /dev/input state for debugging (write to USB)
            {
                int dfd = open("/mnt/cage-diag.log", O_WRONLY | O_CREAT | O_TRUNC, 0644);
                if (dfd >= 0) {
                    char dbuf[1024];
                    int dlen = 0;
                    dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, "pid=%d\n", getpid());
                    DIR *d = opendir("/dev/input");
                    if (d) {
                        struct dirent *e;
                        dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, "dev-input:");
                        while ((e = readdir(d))) {
                            if (e->d_name[0] != '.')
                                dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, " %s", e->d_name);
                        }
                        dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, "\n");
                        closedir(d);
                    } else {
                        dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen,
                                "dev-input: MISSING errno=%d\n", errno);
                    }
                    // Also list /dev/dri
                    d = opendir("/dev/dri");
                    if (d) {
                        struct dirent *e;
                        dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, "dev-dri:");
                        while ((e = readdir(d))) {
                            if (e->d_name[0] != '.')
                                dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, " %s", e->d_name);
                        }
                        dlen += snprintf(dbuf + dlen, sizeof(dbuf) - dlen, "\n");
                        closedir(d);
                    }
                    write(dfd, dbuf, dlen);
                    fsync(dfd);
                    close(dfd);
                }
            }

            // Ensure /run exists (may be missing if mount_minimal_fs re-mounted rootfs)
            mkdir("/run", 0755);
            mount("tmpfs", "/run", "tmpfs", 0, NULL);

            // Start seatd
            system("seatd -g root > /tmp/seatd.log 2>&1 &");
            for (int si = 0; si < 30; si++) {
                usleep(100000);
                if (access("/run/seatd.sock", F_OK) == 0) break;
            }

            if (access("/run/seatd.sock", F_OK) != 0) {
                FILE *ef = fopen("/tmp/cage-stderr.log", "w");
                if (ef) {
                    fprintf(ef, "seatd failed to start after 3s\n");
                    fprintf(ef, "run-dir-exists=%d run-writable=%d\n",
                            access("/run", F_OK) == 0, access("/run", W_OK) == 0);
                    // Dump seatd log
                    FILE *sl = fopen("/tmp/seatd.log", "r");
                    if (sl) {
                        char sbuf[256];
                        fprintf(ef, "--- seatd.log ---\n");
                        while (fgets(sbuf, sizeof(sbuf), sl)) fputs(sbuf, ef);
                        fclose(sl);
                    }
                    fclose(ef);
                }
                _exit(1);
            }

            // Redirect stderr to file so parent can read cage errors
            FILE *cage_log = fopen("/tmp/cage-stderr.log", "w");
            if (cage_log) {
                dup2(fileno(cage_log), STDERR_FILENO);
                fclose(cage_log);
            }
            fprintf(stderr, "[cage-transition] seatd ok, launching cage...\n");

            execlp("cage", "cage", "-s", "--", "/ac-native", "/piece.mjs", NULL);
            fprintf(stderr, "[cage-transition] exec cage failed: %s\n", strerror(errno));
            _exit(127);
        }

        if (cage_pid > 0) {
            // === PARENT: wait for cage session to end ===
            ac_log("[cage-transition] cage pid=%d, waiting...\n", cage_pid);
            int status = 0;
            waitpid(cage_pid, &status, 0);
            int rc = WIFEXITED(status) ? WEXITSTATUS(status) : -1;
            ac_log("[cage-transition] cage exited: %d\n", rc);

            // Copy cage stderr to USB log
            FILE *cage_err = fopen("/tmp/cage-stderr.log", "r");
            if (cage_err) {
                char buf[256];
                while (fgets(buf, sizeof(buf), cage_err))
                    ac_log("[cage-err] %s", buf);
                fclose(cage_err);
            }
            // Copy child ac-native log to USB log
            FILE *cage_child = fopen("/tmp/ac-native-cage.log", "r");
            if (cage_child) {
                char buf[256];
                while (fgets(buf, sizeof(buf), cage_child))
                    ac_log("[cage-child] %s", buf);
                fclose(cage_child);
            }

            // Cleanup seatd
            system("killall seatd 2>/dev/null");
        } else {
            ac_log("[cage-transition] fork failed: %s\n", strerror(errno));
        }

        // Check if cage child requested reboot/poweroff
        if (reboot_requested) {
            ac_log("[cage-transition] Reboot requested by cage child\n");
            ac_log_flush();
            sync(); usleep(500000); sync();
            reboot(LINUX_REBOOT_CMD_RESTART);
        }
        if (poweroff_requested) {
            ac_log("[cage-transition] Poweroff requested by cage child\n");
            ac_log_flush();
            sync(); usleep(500000); sync();
            reboot(LINUX_REBOOT_CMD_POWER_OFF);
        }

        // Reclaim DRM and continue in DRM mode (fallback)
        drm_acquire_master(display);
        audio = audio_init();
        ac_log("[cage-transition] Reclaimed DRM, continuing in DRM mode\n");
    }
#endif

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
#ifdef USE_WAYLAND
            // Under Wayland: input_poll handles all Wayland event dispatch
            // (reading from socket + firing listeners in correct order)
            if (is_wayland) {
                // nothing — input_poll does full dispatch
            } else
#endif
            {
                // DRM handoff: xdg-open sends SIGUSR1 to release, SIGUSR2 to reclaim
                if (drm_handoff_release && display) {
                    drm_handoff_release = 0;

                    char browser_url[2048] = "";
                    FILE *uf = fopen("/tmp/.browser-url", "r");
                    if (uf) {
                        if (fgets(browser_url, sizeof(browser_url), uf))
                            browser_url[strcspn(browser_url, "\n")] = 0;
                        fclose(uf);
                        unlink("/tmp/.browser-url");
                    }

                    if (browser_url[0]) {
                        ac_log("[browser] URL: %s", browser_url);
                        drm_release_master(display);
                        ac_log("[browser] Released DRM master");

                        // Fork a child for the entire browser session.
                        // Child sets env, starts seatd+cage+firefox, then exits.
                        // Parent waits, then reclaims DRM. If child crashes, parent survives.
                        pid_t bpid = fork();
                        if (bpid == 0) {
                            // === CHILD PROCESS ===
                            mkdir("/tmp/xdg-browser", 0700);
                            mkdir("/tmp/.mozilla", 0755);
                            mkdir("/run", 0755);

                            setenv("HOME", "/tmp", 1);
                            setenv("XDG_RUNTIME_DIR", "/tmp/xdg-browser", 1);
                            setenv("WLR_BACKENDS", "drm", 1);
                            setenv("WLR_RENDERER", "pixman", 1);
                            setenv("LD_LIBRARY_PATH", "/lib64:/opt/firefox", 1);
                            setenv("LIBGL_ALWAYS_SOFTWARE", "1", 1);
                            setenv("MOZ_ENABLE_WAYLAND", "1", 1);
                            setenv("GDK_BACKEND", "wayland", 1);
                            setenv("MOZ_APP_LAUNCHER", "/opt/firefox/firefox", 1);
                            setenv("GRE_HOME", "/opt/firefox", 1);
                            setenv("DBUS_SESSION_BUS_ADDRESS", "disabled:", 1);
                            setenv("MOZ_DBUS_REMOTE", "0", 1);
                            setenv("MOZ_DISABLE_CONTENT_SANDBOX", "1", 1);

                            // Ensure /etc/group exists for seatd
                            {
                                FILE *grp = fopen("/etc/group", "a");
                                if (grp) {
                                    fseek(grp, 0, SEEK_END);
                                    if (ftell(grp) == 0) fprintf(grp, "root:x:0:\n");
                                    fclose(grp);
                                }
                            }

                            // Start seatd, wait for socket
                            system("seatd -g root > /tmp/seatd.log 2>&1 &");
                            for (int si = 0; si < 15; si++) {
                                usleep(200000);
                                if (access("/run/seatd.sock", F_OK) == 0) break;
                            }

                            // Run cage+firefox (blocks until browser exits)
                            char cmd[4096];
                            snprintf(cmd, sizeof(cmd),
                                "cd /opt/firefox && cage -s -- ./firefox --kiosk --no-remote "
                                "--new-instance '%s' > /tmp/cage-out.log 2>&1", browser_url);
                            int rc = system(cmd);

                            // Cleanup
                            system("killall seatd 2>/dev/null");
                            // Copy logs to USB
                            FILE *lf = fopen("/tmp/cage-out.log", "r");
                            if (lf) {
                                FILE *mf = fopen("/mnt/cage.log", "w");
                                if (mf) {
                                    char buf[512];
                                    while (fgets(buf, sizeof(buf), lf)) fputs(buf, mf);
                                    fclose(mf);
                                    sync();
                                }
                                fclose(lf);
                            }
                            _exit(rc);
                        }

                        // === PARENT PROCESS ===
                        if (bpid > 0) {
                            int bstatus = 0;
                            ac_log("[browser] child pid=%d, waiting...", bpid);
                            waitpid(bpid, &bstatus, 0);
                            if (WIFEXITED(bstatus))
                                ac_log("[browser] child exited: %d", WEXITSTATUS(bstatus));
                            else if (WIFSIGNALED(bstatus))
                                ac_log("[browser] child killed by signal %d", WTERMSIG(bstatus));
                        } else {
                            ac_log("[browser] fork failed: %s", strerror(errno));
                        }

                        // Always reclaim DRM, even if child crashed
                        drm_acquire_master(display);
                        ac_log("[browser] Reclaimed DRM master");
                    } else {
                        ac_log("[browser] SIGUSR1 but no URL in /tmp/.browser-url");
                    }
                }
            }

            input_poll(input);
            main_frame++;
            // Check for device token API response (from wifi-connect fetch)
            if (main_frame % 300 == 0) {
                FILE *rf = fopen("/tmp/claude-api-resp.json", "r");
                if (rf) {
                    char rbuf[2048] = {0};
                    fread(rbuf, 1, sizeof(rbuf) - 1, rf);
                    fclose(rf);
                    unlink("/tmp/claude-api-resp.json");
                    // Extract Claude token: {"token":"sk-ant-...","githubPat":"ghp_..."}
                    const char *tk = strstr(rbuf, "\"token\"");
                    if (tk) {
                        const char *tv = strchr(tk + 7, ':');
                        if (tv) {
                            tv++; while (*tv == ' ' || *tv == '"') tv++;
                            const char *te = strchr(tv, '"');
                            if (te && te > tv && (te - tv) > 10) {
                                FILE *tf = fopen("/claude-token", "w");
                                if (tf) { fwrite(tv, 1, te - tv, tf); fclose(tf); }
                                ac_log("[tokens] claude token from API (%d bytes)\n", (int)(te - tv));
                            }
                        }
                    }
                    // Extract GitHub PAT
                    const char *gk = strstr(rbuf, "\"githubPat\"");
                    if (gk) {
                        const char *gv = strchr(gk + 11, ':');
                        if (gv) {
                            gv++; while (*gv == ' ' || *gv == '"') gv++;
                            const char *ge = strchr(gv, '"');
                            if (ge && ge > gv && (ge - gv) > 5) {
                                FILE *gf = fopen("/github-pat", "w");
                                if (gf) { fwrite(gv, 1, ge - gv, gf); fclose(gf); }
                                ac_log("[tokens] github pat from API (%d bytes)\n", (int)(ge - gv));
                            }
                        }
                    }
                }
            }
            // Copy tmpfs debug logs to USB periodically (every 5 sec)
            if (logfile && main_frame % 300 == 0) {
                FILE *xf = fopen("/tmp/xdg-open.log", "r");
                if (xf) {
                    char xbuf[512];
                    while (fgets(xbuf, sizeof(xbuf), xf))
                        ac_log("[xdg] %s", xbuf);
                    fclose(xf);
                    unlink("/tmp/xdg-open.log"); // only report once
                }
                FILE *ff = fopen("/tmp/firefox-debug.log", "r");
                if (ff) {
                    char fbuf[512];
                    while (fgets(fbuf, sizeof(fbuf), ff))
                        ac_log("[firefox] %s", fbuf);
                    fclose(ff);
                    unlink("/tmp/firefox-debug.log");
                }
            }
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
                        // Recreate cursor overlay at new resolution
                        if (cursor_fb) fb_destroy(cursor_fb);
                        cursor_fb = fb_create(screen->width, screen->height);
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

                    ac_display_present(display, screen, pixel_scale);
                    frame_sync_60fps(&anim_time);
                }

                // Final black frame
                graph_wipe(&graph, (ACColor){0, 0, 0, 255});
                ac_display_present(display, screen, pixel_scale);

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

                // Parse colon-separated params: "chat:clock" → name="chat", params=["clock"]
                rt->jump_param_count = 0;
                {
                    char *colon = strchr(rt->jump_target, ':');
                    if (colon) {
                        *colon = 0; // terminate piece name at first colon
                        char *rest = colon + 1;
                        while (rest && *rest && rt->jump_param_count < 8) {
                            char *next = strchr(rest, ':');
                            if (next) *next = 0;
                            strncpy(rt->jump_params[rt->jump_param_count], rest, 63);
                            rt->jump_params[rt->jump_param_count][63] = 0;
                            rt->jump_param_count++;
                            rest = next ? next + 1 : NULL;
                        }
                    }
                }

                ac_log("[ac-native] Jumping to piece: %s (%d params)\n",
                       rt->jump_target, rt->jump_param_count);

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

                // NOTE: "claude" and "cc" aliases removed — claude.mjs handles auth curtain
                // before jumping to terminal:claude itself.

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

            clock_gettime(CLOCK_MONOTONIC, &_pf_paint1);

            // Software cursor on its own overlay buffer (unaffected by KidLisp effects)
            if (cursor_fb && input && (input->pointer_x || input->pointer_y)) {
                fb_clear(cursor_fb, 0x00000000); // transparent
                graph_page(&graph, cursor_fb);
                int cx = input->pointer_x / pixel_scale, cy = input->pointer_y / pixel_scale;
                // Shadow (black, offset +1,+1)
                graph_ink(&graph, (ACColor){0, 0, 0, 180});
                graph_line(&graph, cx+1, cy-9, cx+1, cy-4);
                graph_line(&graph, cx+1, cy+6, cx+1, cy+11);
                graph_line(&graph, cx-9, cy+1, cx-4, cy+1);
                graph_line(&graph, cx+6, cy+1, cx+11, cy+1);
                graph_plot(&graph, cx+1, cy+1);
                // Crosshair color reflects WiFi state
                {
                    ACColor cross_color = {128, 128, 128, 255}; // gray = no wifi
                    if (wifi) {
                        switch (wifi->state) {
                            case WIFI_STATE_CONNECTED:
                                cross_color = (ACColor){0, 255, 255, 255};   // cyan
                                break;
                            case WIFI_STATE_SCANNING:
                            case WIFI_STATE_CONNECTING:
                                cross_color = (ACColor){255, 255, 0, 255};   // yellow
                                break;
                            case WIFI_STATE_FAILED:
                                cross_color = (ACColor){255, 60, 60, 255};   // red
                                break;
                            default:
                                cross_color = (ACColor){128, 128, 128, 255}; // gray
                                break;
                        }
                    }
                    graph_ink(&graph, cross_color);
                }
                graph_line(&graph, cx, cy-10, cx, cy-5);
                graph_line(&graph, cx, cy+5, cx, cy+10);
                graph_line(&graph, cx-10, cy, cx-5, cy);
                graph_line(&graph, cx+5, cy, cx+10, cy);
                // White center dot
                graph_ink(&graph, (ACColor){255, 255, 255, 255});
                graph_plot(&graph, cx, cy);
                // Composite cursor region onto screen (only the area around cursor)
                graph_page(&graph, screen);
                int bx = cx - 12, by = cy - 12, bw = 25, bh = 25;
                if (bx < 0) bx = 0;
                if (by < 0) by = 0;
                if (bx + bw > cursor_fb->width) bw = cursor_fb->width - bx;
                if (by + bh > cursor_fb->height) bh = cursor_fb->height - by;
                for (int py = by; py < by + bh; py++) {
                    for (int px = bx; px < bx + bw; px++) {
                        uint32_t pixel = cursor_fb->pixels[py * cursor_fb->stride + px];
                        if (pixel >> 24) // only blit non-transparent
                            fb_blend_pixel(screen, px, py, pixel);
                    }
                }
            }

            // WiFi "online" TTS announcement
            {
                static int was_connected = 0;
                int is_connected = (wifi && wifi->state == WIFI_STATE_CONNECTED);
                if (is_connected && !was_connected) {
                    if (tts) tts_speak(tts, "online");
                    ac_log("[wifi-tts] connected — announcing 'online'");

                    // Fetch device tokens (Claude + GitHub) from API (authenticated)
                    {
                        FILE *cf = fopen("/mnt/config.json", "r");
                        if (cf) {
                            char cbuf[4096] = {0};
                            fread(cbuf, 1, sizeof(cbuf) - 1, cf);
                            fclose(cf);
                            char handle[64] = {0}, actoken[1024] = {0};
                            parse_config_string(cbuf, "\"handle\"", handle, sizeof(handle));
                            parse_config_string(cbuf, "\"token\"", actoken, sizeof(actoken));
                            if (handle[0] && actoken[0]) {
                                char cmd[2048];
                                snprintf(cmd, sizeof(cmd),
                                    "curl -fsSL -H 'Authorization: Bearer %s' "
                                    "'https://aesthetic.computer/.netlify/functions/claude-token' "
                                    "-o /tmp/claude-api-resp.json 2>/dev/null &", actoken);
                                system(cmd);
                                ac_log("[tokens] fetching for @%s\n", handle);
                            }
                        }
                    }
                    ac_log_flush();
                }
                was_connected = is_connected;
            }

            // Machines monitoring daemon (connects, heartbeats, handles commands)
            {
                static int fps_counter = 0, fps_display = 0;
                static struct timespec fps_last = {0};
                fps_counter++;
                struct timespec fps_now;
                clock_gettime(CLOCK_MONOTONIC, &fps_now);
                if (fps_now.tv_sec > fps_last.tv_sec) {
                    fps_display = fps_counter;
                    fps_counter = 0;
                    fps_last = fps_now;
                }
                machines_tick(&g_machines, wifi, main_frame, fps_display,
                              rt->jump_target[0] ? rt->jump_target : rt->piece);

                // Handle commands forwarded from machines daemon
                if (g_machines.cmd_pending) {
                    g_machines.cmd_pending = 0;
                    if (strcmp(g_machines.cmd_type, "jump") == 0 && g_machines.cmd_target[0]) {
                        strncpy(rt->jump_target, g_machines.cmd_target, sizeof(rt->jump_target) - 1);
                        rt->jump_requested = 1;
                        ac_log("[machines] jump → %s\n", g_machines.cmd_target);
                    } else if (strcmp(g_machines.cmd_type, "update") == 0) {
                        // Jump to notepat which handles OTA updates
                        strncpy(rt->jump_target, "notepat", sizeof(rt->jump_target) - 1);
                        rt->jump_requested = 1;
                        ac_log("[machines] update requested, jumping to notepat\n");
                    }
                }
            }

            clock_gettime(CLOCK_MONOTONIC, &_pf_pres0);
            ac_display_present(display, screen, pixel_scale);
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
    machines_destroy(&g_machines);

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
