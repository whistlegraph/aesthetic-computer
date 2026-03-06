// ac-native.c — Sub-second boot AC piece runner
// Runs as PID 1 in a minimal initramfs.
// UEFI → EFI stub kernel → this binary → piece.mjs

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <time.h>
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
static char log_dev[32] = "";
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
    const char *devs[] = {"/dev/sda1", "/dev/sdb1", "/dev/sdc1", NULL};
    for (int i = 0; devs[i]; i++) {
        if (access(devs[i], F_OK) != 0) continue;
        if (mount(devs[i], "/mnt", "vfat", 0, NULL) == 0) {
            logfile = fopen("/mnt/ac-native.log", "w");
            if (logfile) {
                // Immediate test write to verify logging works
                fprintf(logfile, "[ac-native] Log opened on %s\n", devs[i]);
                fflush(logfile);
                fsync(fileno(logfile));
                strncpy(log_dev, devs[i], sizeof(log_dev) - 1);
                fprintf(stderr, "[ac-native] Log: %s -> /mnt/ac-native.log\n", devs[i]);
                return;
            }
            umount("/mnt");
        }
        fprintf(stderr, "[ac-native] Log mount failed: %s\n", devs[i]);
    }
    // Fallback: log to tmpfs (won't survive reboot but stderr goes to console)
    fprintf(stderr, "[ac-native] No USB log mount available\n");
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
static void auto_install_to_hd(ACGraph *graph, ACFramebuffer *screen,
                                ACDisplay *display) {
    const char *kernel_src = "/mnt/EFI/BOOT/BOOTX64.EFI";

    // Check if kernel exists on USB
    if (access(kernel_src, F_OK) != 0) {
        fprintf(stderr, "[ac-native] No kernel on USB, skipping HD install\n");
        return;
    }

    if (display)
        draw_boot_status(graph, screen, display, "installing to disk...");

    mkdir("/tmp/hd", 0755);

    // Determine which block device the USB log is on (skip it)
    // log_dev is like "/dev/sda1" → parent is "sda"
    char usb_blk[16] = "";
    if (log_dev[0]) {
        const char *p = log_dev + 5; // skip "/dev/"
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
        "sda", "sdb", "sdc",     // SATA/USB
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

        // Try partitions 1-4
        for (int p = 1; p <= 4 && !installed; p++) {
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

                sync();
                installed = 1;
                fprintf(stderr, "[ac-native] Installed %ld bytes to %s\n", sz, devpath);
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

// Draw y/n install confirmation screen
// Returns 1 if user confirms with Y, 0 if N/Escape
static int draw_install_confirm(ACGraph *graph, ACFramebuffer *screen,
                                 ACDisplay *display, int *fds, int nfds) {
    int is_dark = (get_la_hour() >= 19 || get_la_hour() < 7);
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

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

        fb_copy_scaled(screen, drm_front_buffer(display),
                       display->width, display->height,
                       drm_front_stride(display), 3);
        frame_sync_60fps(&anim_time);

        // Read key events
        struct input_event ev;
        for (int ki = 0; ki < nfds; ki++) {
            while (read(fds[ki], &ev, sizeof(ev)) == sizeof(ev)) {
                if (ev.type == EV_KEY && ev.value == 1) {
                    if (ev.code == KEY_Y) return 1;
                    if (ev.code == KEY_N || ev.code == KEY_ESC) return 0;
                }
            }
        }
    }
}

// Draw startup fade animation (black → white with title)
// Returns 1 if user pressed W and confirmed install, 0 otherwise
static int draw_startup_fade(ACGraph *graph, ACFramebuffer *screen,
                              ACDisplay *display) {
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);
    int la_hour = get_la_hour();
    int is_dark = (la_hour >= 19 || la_hour < 7);

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
            if (fd >= 0) key_fds[key_fd_count++] = fd;
        }
        closedir(dir);
    }

    // Start with a black frame immediately (hides any kernel text)
    graph_wipe(graph, (ACColor){0, 0, 0, 255});
    fb_copy_scaled(screen, drm_front_buffer(display),
                   display->width, display->height,
                   drm_front_stride(display), 3);

    // 60 frames = 1 second animation.
    // W press → halt and show y/n confirmation
    // Any other key → skip animation and start playing
    int skip_anim = 0;
    int w_pressed = 0;
    for (int f = 0; f < 60 && !skip_anim && !w_pressed; f++) {
        double t = (double)f / 60.0;

        // Drain all key events — detect W press or other-key skip
        {
            struct input_event ev;
            for (int ki = 0; ki < key_fd_count; ki++) {
                while (read(key_fds[ki], &ev, sizeof(ev)) == sizeof(ev)) {
                    // First 10 frames: drain stale events silently
                    if (f < 10) continue;
                    if (ev.type == EV_KEY && ev.value == 1) {
                        if (ev.code == KEY_W) {
                            w_pressed = 1;
                        } else if (ev.code != KEY_RESERVED) {
                            skip_anim = 1;
                        }
                    }
                }
            }
        }

        // Fade from black to target bg (complete in first 0.3s)
        double fade_t = t * 3.33;
        if (fade_t > 1.0) fade_t = 1.0;
        int bg_target = is_dark ? 20 : 255;
        int bg = (int)(bg_target * fade_t);
        graph_wipe(graph, (ACColor){(uint8_t)bg, (uint8_t)bg, (uint8_t)(bg + (is_dark ? (int)(5 * fade_t) : 0)), 255});

        // Title + subtitle appear with fade
        int alpha = (int)(255.0 * fade_t);
        if (alpha > 0) {
            int fg = is_dark ? 220 : 0;
            graph_ink(graph, (ACColor){(uint8_t)fg, (uint8_t)fg, (uint8_t)fg, (uint8_t)alpha});
            int tw = font_measure_matrix("notepat", 3);
            font_draw_matrix(graph, "notepat", (screen->width - tw) / 2,
                             screen->height / 2 - 20, 3);

            int sub = is_dark ? 140 : 120;
            graph_ink(graph, (ACColor){(uint8_t)sub, (uint8_t)sub, (uint8_t)sub, (uint8_t)(alpha / 2)});
            int sw = font_measure_matrix("aesthetic.computer", 1);
            font_draw_matrix(graph, "aesthetic.computer",
                             (screen->width - sw) / 2, screen->height / 2 + 10, 1);
        }

        // Bottom: shrinking time bar + W hint
        int bar_full = screen->width - 40;
        int bar_remaining = (int)((1.0 - t) * bar_full);
        if (bar_remaining > 0) {
            graph_ink(graph, (ACColor){200, 200, 200, (uint8_t)(80 * (1.0 - t))});
            graph_box(graph, 20, screen->height - 6, bar_remaining, 3, 1);
        }

        // Show W hint after initial fade
        if (alpha > 100) {
            graph_ink(graph, (ACColor){(uint8_t)(is_dark ? 80 : 160),
                                       (uint8_t)(is_dark ? 80 : 160),
                                       (uint8_t)(is_dark ? 90 : 170),
                                       (uint8_t)(alpha / 3)});
            const char *hint = "W: install to disk";
            int hw = font_measure_matrix(hint, 1);
            font_draw_matrix(graph, hint,
                             (screen->width - hw) / 2, screen->height - 18, 1);
        }

        fb_copy_scaled(screen, drm_front_buffer(display),
                       display->width, display->height,
                       drm_front_stride(display), 3);
        frame_sync_60fps(&anim_time);
    }

    // If W was pressed, show y/n confirmation
    int result = 0;
    if (w_pressed) {
        result = draw_install_confirm(graph, screen, display, key_fds, key_fd_count);
    }

    for (int i = 0; i < key_fd_count; i++) close(key_fds[i]);
    return result;
}

// Draw a status frame during boot (white bg, title + status text)
static void draw_boot_status(ACGraph *graph, ACFramebuffer *screen,
                             ACDisplay *display, const char *status) {
    int dk = (get_la_hour() >= 19 || get_la_hour() < 7);
    uint8_t bg = dk ? 20 : 255;
    graph_wipe(graph, (ACColor){bg, bg, (uint8_t)(bg + (dk ? 5 : 0)), 255});

    // Title: "notepat" in MatrixChunky8 at scale 3
    uint8_t fg = dk ? 220 : 0;
    graph_ink(graph, (ACColor){fg, fg, fg, 255});
    int tw = font_measure_matrix("notepat", 3);
    font_draw_matrix(graph, "notepat", (screen->width - tw) / 2,
                     screen->height / 2 - 20, 3);

    // Subtitle
    uint8_t sub = dk ? 140 : 120;
    graph_ink(graph, (ACColor){sub, sub, sub, 255});
    int sw = font_measure_matrix("aesthetic.computer", 1);
    font_draw_matrix(graph, "aesthetic.computer",
                     (screen->width - sw) / 2, screen->height / 2 + 10, 1);

    // Status text below subtitle
    if (status) {
        graph_ink(graph, (ACColor){(uint8_t)(dk ? 120 : 160), (uint8_t)(dk ? 120 : 160), (uint8_t)(dk ? 120 : 160), 255});
        int stw = font_measure_matrix(status, 1);
        font_draw_matrix(graph, status, (screen->width - stw) / 2,
                         screen->height / 2 + 26, 1);
    }

    fb_copy_scaled(screen, drm_back_buffer(display),
                   display->width, display->height,
                   drm_back_stride(display), 3);
    drm_flip(display);
}

int main(int argc, char *argv[]) {
    struct timespec boot_start;
    clock_gettime(CLOCK_MONOTONIC, &boot_start);

    // Mount filesystems if PID 1
    if (getpid() == 1) {
        mount_minimal_fs();
        // Ensure PATH includes all standard binary directories
        setenv("PATH", "/bin:/sbin:/usr/bin:/usr/sbin", 1);
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

    if (!headless) {
        display = drm_init();
        if (!display) {
            fprintf(stderr, "[ac-native] FATAL: No display\n");
            if (getpid() == 1) { sleep(5); reboot(LINUX_REBOOT_CMD_POWER_OFF); }
            return 1;
        }

        // 1/3 display resolution (3x nearest-neighbor scale)
        screen = fb_create(display->width / 3, display->height / 3);
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

    // Startup fade animation (black → white, hides kernel text)
    // Returns 1 if user held W to request disk install
    int want_install = 0;
    if (!headless) {
        want_install = draw_startup_fade(&graph, screen, display);
        draw_boot_status(&graph, screen, display, "starting input...");
    }

    // Init input
    if (!headless) {
        input = input_init(display->width, display->height, 3);
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

    // Try to mount USB for logging (starts waiting for USB enumeration)
    if (getpid() == 1) try_mount_log();

    // Init audio
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "starting audio...");
    ACAudio *audio = audio_init();
    audio_boot_beep(audio); // Early beep: "I'm alive!"

    // Init TTS (warmup with empty string to preload voice data)
    ACTts *tts = tts_init(audio);
    if (tts) {
        tts_speak(tts, ".");  // Warmup: load voice data into memory
        tts_wait(tts);        // Wait for warmup to complete
        tts_speak(tts, "aesthetic dot computer");
    }

    // Retry log mount if first attempt failed (USB may have appeared during audio init)
    if (getpid() == 1 && !logfile) try_mount_log();

    // Install kernel to internal drive (only if user held W during boot)
    if (getpid() == 1 && want_install && logfile)
        auto_install_to_hd(&graph, screen, display);

    // Init WiFi
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "starting wifi...");
    if (tts) { tts_wait(tts); tts_speak(tts, "checking wifi"); }
    ACWifi *wifi = wifi_init();
    if (wifi && wifi->state == WIFI_STATE_CONNECTED && tts) {
        tts_wait(tts);
        tts_speak(tts, "wifi connected");
    } else if (wifi && tts) {
        tts_wait(tts);
        tts_speak(tts, "wifi scanning");
    } else if (tts) {
        tts_wait(tts);
        tts_speak(tts, "no wifi hardware");
    }

    // Init JS
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "starting javascript...");
    ACRuntime *rt = js_init(&graph, input, audio, wifi, tts);
    if (!rt) {
        fprintf(stderr, "[ac-native] FATAL: Cannot init JS\n");
        wifi_destroy(wifi);
        audio_destroy(audio);
        fb_destroy(screen);
        if (display) drm_destroy(display);
        return 1;
    }

    // Load piece
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "loading piece...");
    if (js_load_piece(rt, piece_path) < 0) {
        fprintf(stderr, "[ac-native] FATAL: Cannot load %s\n", piece_path);
        if (!headless) {
            graph_wipe(&graph, (ACColor){200, 0, 0, 255});
            graph_ink(&graph, (ACColor){255, 255, 255, 255});
            char msg[256];
            snprintf(msg, sizeof(msg), "Cannot load: %s", piece_path);
            font_draw(&graph, msg, 20, 20, 2);
            fb_copy_scaled(screen, drm_back_buffer(display),
                           display->width, display->height,
                           drm_back_stride(display), 3);
            drm_flip(display);
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

    // Ready melody + speech: piece loaded, about to start!
    if (tts) { tts_wait(tts); tts_speak(tts, "notepat ready"); tts_wait(tts); }
    audio_ready_melody(audio);

    // Call boot()
    js_call_boot(rt);

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
            int power_pressed = 0;
            for (int i = 0; i < input->event_count; i++) {
                if (input->events[i].type == AC_EVENT_KEYBOARD_DOWN) {
                    // Volume: KEY_VOLUMEUP/DOWN/MUTE or F1/F2/F3 as fallback
                    if (strcmp(input->events[i].key_name, "audiovolumeup") == 0 ||
                        strcmp(input->events[i].key_name, "f3") == 0)
                        audio_volume_adjust(audio, 1);
                    else if (strcmp(input->events[i].key_name, "audiovolumedown") == 0 ||
                             strcmp(input->events[i].key_name, "f2") == 0)
                        audio_volume_adjust(audio, -1);
                    else if (strcmp(input->events[i].key_name, "audiomute") == 0 ||
                             strcmp(input->events[i].key_name, "f1") == 0)
                        audio_volume_adjust(audio, 0);
                    // Brightness: KEY_BRIGHTNESS or F5/F6 as fallback
                    else if ((strcmp(input->events[i].key_name, "brightnessup") == 0 ||
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
                    }
                    else if (strcmp(input->events[i].key_name, "power") == 0 ||
                             input->events[i].key_code == KEY_POWER)
                        power_pressed = 1;
                }
            }

            if (power_pressed) {
                // Shutdown animation (~1.5 seconds)
                struct timespec anim_time;
                clock_gettime(CLOCK_MONOTONIC, &anim_time);
                int sd_dark = (get_la_hour() >= 19 || get_la_hour() < 7);
                int sd_bg = sd_dark ? 20 : 255;

                for (int f = 0; f < 60; f++) { // 60 frames @ 60fps = 1s
                    double t = (double)f / 60.0;

                    // Fade to black with centered text
                    int fade = (int)(sd_bg * (1.0 - t));
                    graph_wipe(&graph, (ACColor){(uint8_t)(fade), (uint8_t)(fade), (uint8_t)(fade), 255});

                    if (t < 0.8) {
                        int alpha = (int)(255.0 * (1.0 - t / 0.8));
                        uint8_t fg = sd_dark ? 220 : 0;
                        graph_ink(&graph, (ACColor){fg, fg, fg, (uint8_t)alpha});
                        int tw = font_measure_matrix("notepat", 3);
                        font_draw_matrix(&graph, "notepat", (screen->width - tw) / 2, screen->height / 2 - 20, 3);
                        uint8_t sub = sd_dark ? 140 : 120;
                        graph_ink(&graph, (ACColor){sub, sub, sub, (uint8_t)(alpha / 2)});
                        int sw = font_measure_matrix("aesthetic.computer", 1);
                        font_draw_matrix(&graph, "aesthetic.computer", (screen->width - sw) / 2, screen->height / 2 + 10, 1);
                    }

                    fb_copy_scaled(screen, drm_back_buffer(display),
                                   display->width, display->height,
                                   drm_back_stride(display), 3);
                    drm_flip(display);
                    frame_sync_60fps(&anim_time);
                }

                // Final black frame
                graph_wipe(&graph, (ACColor){0, 0, 0, 255});
                fb_copy_scaled(screen, drm_back_buffer(display),
                               display->width, display->height,
                               drm_back_stride(display), 3);
                drm_flip(display);

                running = 0;
                break;
            }

            js_call_act(rt);
            if (audio_beat_check(audio)) js_call_beat(rt);
            js_call_sim(rt);
            js_call_paint(rt);

            // Software cursor (AC precise cursor: teal crosshair + shadow + white center)
            if (input && (input->pointer_x || input->pointer_y)) {
                int cx = input->pointer_x / 3, cy = input->pointer_y / 3;
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

            fb_copy_scaled(screen, drm_back_buffer(display),
                           display->width, display->height,
                           drm_back_stride(display), 3);
            drm_flip(display);
            frame_sync_60fps(&frame_time);
        }
    }

    // Cleanup
    ac_log("[ac-native] Shutting down\n");
    if (logfile) { fclose(logfile); logfile = NULL; }
    sync();
    // Unmount USB log
    umount("/mnt");

    js_call_leave(rt);
    js_destroy(rt);
    wifi_destroy(wifi);
    if (tts) { tts_speak(tts, "goodbye"); tts_wait(tts); }
    audio_shutdown_sound(audio);
    tts_destroy(tts);
    audio_destroy(audio);
    if (input) input_destroy(input);
    fb_destroy(screen);
    if (display) drm_destroy(display);

    if (getpid() == 1) {
        sync();
        reboot(LINUX_REBOOT_CMD_POWER_OFF);
    }

    return 0;
}
