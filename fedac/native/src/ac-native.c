// ac-native.c — Sub-second boot AC piece runner
// Runs as PID 1 in a minimal initramfs.
// UEFI → EFI stub kernel → this binary → piece.mjs

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/reboot.h>
#include <linux/reboot.h>

#include "drm-display.h"
#include "framebuffer.h"
#include "graph.h"
#include "font.h"
#include "input.h"
#include "js-bindings.h"

static volatile int running = 1;

static void signal_handler(int sig) {
    (void)sig;
    running = 0;
}

// Mount minimal filesystems (only needed when running as PID 1)
static void mount_minimal_fs(void) {
    mkdir("/proc", 0755);
    mkdir("/sys", 0755);
    mkdir("/dev", 0755);
    mkdir("/tmp", 0755);

    mount("proc", "/proc", "proc", 0, NULL);
    mount("sysfs", "/sys", "sysfs", 0, NULL);
    mount("devtmpfs", "/dev", "devtmpfs", 0, NULL);
    mount("tmpfs", "/tmp", "tmpfs", 0, NULL);

    // Wait briefly for DRM device to appear
    for (int i = 0; i < 50; i++) {
        if (access("/dev/dri/card0", F_OK) == 0) break;
        usleep(10000); // 10ms
    }
}

static void frame_sync_60fps(struct timespec *next) {
    // 16.666ms per frame
    next->tv_nsec += 16666667;
    if (next->tv_nsec >= 1000000000) {
        next->tv_nsec -= 1000000000;
        next->tv_sec++;
    }
    clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, next, NULL);
}

int main(int argc, char *argv[]) {
    struct timespec boot_start;
    clock_gettime(CLOCK_MONOTONIC, &boot_start);

    fprintf(stderr, "[ac-native] Starting...\n");

    // If running as PID 1, mount filesystems
    if (getpid() == 1) {
        mount_minimal_fs();
        fprintf(stderr, "[ac-native] Filesystems mounted\n");
    }

    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);

    // Determine piece path
    const char *piece_path = "/piece.mjs";
    if (argc > 1) piece_path = argv[1];

    // Initialize display
    ACDisplay *display = drm_init();
    if (!display) {
        fprintf(stderr, "[ac-native] FATAL: No display\n");
        if (getpid() == 1) reboot(LINUX_REBOOT_CMD_POWER_OFF);
        return 1;
    }

    // Create framebuffer matching display resolution
    ACFramebuffer *screen = fb_create(display->width, display->height);
    if (!screen) {
        fprintf(stderr, "[ac-native] FATAL: Cannot create framebuffer\n");
        drm_destroy(display);
        return 1;
    }

    // Initialize subsystems
    ACGraph graph;
    graph_init(&graph, screen);
    font_init();
    ACInput *input = input_init(display->width, display->height);

    // Initialize JS runtime
    ACRuntime *rt = js_init(&graph, input);
    if (!rt) {
        fprintf(stderr, "[ac-native] FATAL: Cannot init JS\n");
        fb_destroy(screen);
        drm_destroy(display);
        return 1;
    }

    // Load piece
    if (js_load_piece(rt, piece_path) < 0) {
        fprintf(stderr, "[ac-native] FATAL: Cannot load piece: %s\n", piece_path);
        // Show error on screen
        graph_wipe(&graph, (ACColor){200, 0, 0, 255});
        graph_ink(&graph, (ACColor){255, 255, 255, 255});
        char msg[256];
        snprintf(msg, sizeof(msg), "Cannot load: %s", piece_path);
        font_draw(&graph, msg, 20, 20, 2);
        font_draw(&graph, "Check piece path", 20, 60, 2);
        fb_copy_to(screen, drm_back_buffer(display), drm_back_stride(display));
        drm_flip(display);
        sleep(10);
        js_destroy(rt);
        fb_destroy(screen);
        drm_destroy(display);
        if (getpid() == 1) reboot(LINUX_REBOOT_CMD_POWER_OFF);
        return 1;
    }

    struct timespec boot_end;
    clock_gettime(CLOCK_MONOTONIC, &boot_end);
    double boot_ms = (boot_end.tv_sec - boot_start.tv_sec) * 1000.0 +
                     (boot_end.tv_nsec - boot_start.tv_nsec) / 1000000.0;
    fprintf(stderr, "[ac-native] Ready in %.1fms\n", boot_ms);

    // Call boot()
    js_call_boot(rt);

    // Main loop
    struct timespec frame_time;
    clock_gettime(CLOCK_MONOTONIC, &frame_time);

    while (running) {
        // Input
        input_poll(input);

        // Act (dispatch events)
        js_call_act(rt);

        // Sim
        js_call_sim(rt);

        // Paint
        js_call_paint(rt);

        // Copy software framebuffer to DRM back buffer and flip
        fb_copy_to(screen, drm_back_buffer(display), drm_back_stride(display));
        drm_flip(display);

        // Frame sync
        frame_sync_60fps(&frame_time);
    }

    // Cleanup
    js_call_leave(rt);
    js_destroy(rt);
    input_destroy(input);
    fb_destroy(screen);
    drm_destroy(display);

    fprintf(stderr, "[ac-native] Shutdown\n");
    if (getpid() == 1) {
        sync();
        reboot(LINUX_REBOOT_CMD_POWER_OFF);
    }

    return 0;
}
