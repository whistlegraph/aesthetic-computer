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

#include "drm-display.h"
#include "framebuffer.h"
#include "graph.h"
#include "font.h"
#include "input.h"
#include "audio.h"
#include "js-bindings.h"

static volatile int running = 1;
static FILE *logfile = NULL;

static void signal_handler(int sig) {
    (void)sig;
    running = 0;
}

// Log to both stderr and logfile
static void ac_log(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    if (logfile) {
        va_start(args, fmt);
        vfprintf(logfile, fmt, args);
        va_end(args);
        fflush(logfile);
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
static char log_dev[32] = "";
static void try_mount_log(void) {
    mkdir("/mnt", 0755);
    // Wait for USB block devices to appear (up to 5s after EFI handoff)
    fprintf(stderr, "[ac-native] Waiting for USB block devices...\n");
    for (int w = 0; w < 250; w++) {
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

static void frame_sync_60fps(struct timespec *next) {
    next->tv_nsec += 16666667;
    if (next->tv_nsec >= 1000000000) {
        next->tv_nsec -= 1000000000;
        next->tv_sec++;
    }
    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, next, NULL);
}

// Draw startup fade animation (black → white with title)
static void draw_startup_fade(ACGraph *graph, ACFramebuffer *screen,
                              ACDisplay *display) {
    struct timespec anim_time;
    clock_gettime(CLOCK_MONOTONIC, &anim_time);

    // Start with a black frame immediately (hides any kernel text)
    graph_wipe(graph, (ACColor){0, 0, 0, 255});
    fb_copy_scaled(screen, drm_back_buffer(display),
                   display->width, display->height,
                   drm_back_stride(display), 3);
    drm_flip(display);

    for (int f = 0; f < 60; f++) { // 60 frames @ 60fps = 1s
        double t = (double)f / 60.0;

        // Fade from black to white
        int bg = (int)(255.0 * t);
        graph_wipe(graph, (ACColor){(uint8_t)bg, (uint8_t)bg, (uint8_t)bg, 255});

        if (t > 0.2) {
            double text_t = (t - 0.2) / 0.8;
            int alpha = (int)(255.0 * (text_t < 1.0 ? text_t : 1.0));

            // Title: "notepat" in MatrixChunky8 at scale 3
            graph_ink(graph, (ACColor){0, 0, 0, (uint8_t)alpha});
            int tw = font_measure_matrix("notepat", 3);
            font_draw_matrix(graph, "notepat", (screen->width - tw) / 2,
                             screen->height / 2 - 20, 3);

            // Subtitle: "aesthetic.computer" in MatrixChunky8 at scale 1
            graph_ink(graph, (ACColor){120, 120, 120, (uint8_t)(alpha / 2)});
            int sw = font_measure_matrix("aesthetic.computer", 1);
            font_draw_matrix(graph, "aesthetic.computer",
                             (screen->width - sw) / 2, screen->height / 2 + 10, 1);
        }

        fb_copy_scaled(screen, drm_back_buffer(display),
                       display->width, display->height,
                       drm_back_stride(display), 3);
        drm_flip(display);
        frame_sync_60fps(&anim_time);
    }
}

// Draw a status frame during boot (white bg, title + status text)
static void draw_boot_status(ACGraph *graph, ACFramebuffer *screen,
                             ACDisplay *display, const char *status) {
    graph_wipe(graph, (ACColor){255, 255, 255, 255});

    // Title: "notepat" in MatrixChunky8 at scale 3
    graph_ink(graph, (ACColor){0, 0, 0, 255});
    int tw = font_measure_matrix("notepat", 3);
    font_draw_matrix(graph, "notepat", (screen->width - tw) / 2,
                     screen->height / 2 - 20, 3);

    // Subtitle
    graph_ink(graph, (ACColor){120, 120, 120, 255});
    int sw = font_measure_matrix("aesthetic.computer", 1);
    font_draw_matrix(graph, "aesthetic.computer",
                     (screen->width - sw) / 2, screen->height / 2 + 10, 1);

    // Status text below subtitle
    if (status) {
        graph_ink(graph, (ACColor){160, 160, 160, 255});
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
    }

    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);

    // Determine piece path
    const char *piece_path = "/piece.mjs";
    int headless = 0;
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--headless") == 0) headless = 1;
        else piece_path = argv[i];
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
    if (!headless) {
        draw_startup_fade(&graph, screen, display);
        draw_boot_status(&graph, screen, display, "starting input...");
    }

    // Init input
    if (!headless) {
        input = input_init(display->width, display->height);
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

    // Retry log mount if first attempt failed (USB may have appeared during audio init)
    if (getpid() == 1 && !logfile) try_mount_log();

    // Init JS
    if (!headless && display)
        draw_boot_status(&graph, screen, display, "starting javascript...");
    ACRuntime *rt = js_init(&graph, input, audio);
    if (!rt) {
        fprintf(stderr, "[ac-native] FATAL: Cannot init JS\n");
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
    if (logfile) { fflush(logfile); }

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

        while (running) {
            input_poll(input);
            // Log key events to USB
            for (int i = 0; i < input->event_count; i++) {
                if (logfile && (input->events[i].type == AC_EVENT_KEYBOARD_DOWN ||
                                input->events[i].type == AC_EVENT_KEYBOARD_UP)) {
                    ac_log("[key] %s code=%d name=%s\n",
                           input->events[i].type == AC_EVENT_KEYBOARD_DOWN ? "DOWN" : "UP",
                           input->events[i].key_code,
                           input->events[i].key_name);
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
                    else if (strcmp(input->events[i].key_name, "power") == 0)
                        power_pressed = 1;
                }
            }

            if (power_pressed) {
                // Shutdown animation (~1.5 seconds)
                struct timespec anim_time;
                clock_gettime(CLOCK_MONOTONIC, &anim_time);

                for (int f = 0; f < 60; f++) { // 60 frames @ 60fps = 1s
                    double t = (double)f / 60.0;

                    // Fade to black with centered text
                    int fade = (int)(255.0 * (1.0 - t));
                    graph_wipe(&graph, (ACColor){(uint8_t)(fade), (uint8_t)(fade), (uint8_t)(fade), 255});

                    if (t < 0.8) {
                        int alpha = (int)(255.0 * (1.0 - t / 0.8));
                        graph_ink(&graph, (ACColor){0, 0, 0, (uint8_t)alpha});
                        int tw = font_measure_matrix("notepat", 3);
                        font_draw_matrix(&graph, "notepat", (screen->width - tw) / 2, screen->height / 2 - 20, 3);
                        graph_ink(&graph, (ACColor){120, 120, 120, (uint8_t)(alpha / 2)});
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
