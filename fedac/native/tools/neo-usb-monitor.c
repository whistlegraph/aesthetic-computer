// SPDX-License-Identifier: GPL-2.0-or-later
// Experimental T8140 RAM-boot diagnostics over the firmware DebugUSB FIFO.
// Register definitions follow Asahi m1n1 src/dockchannel_uart.c (e798b719).
// Fixed diagnostics and an isolated ALSA loopback check; no arbitrary shell.
#define _GNU_SOURCE
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/klog.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define UART_BASE UINT64_C(0x308128000)
#define UART_SIZE 0x10000
#define TX8 0x4004
#define TX_FREE 0x4014
#define RX8 0x401c
#define RX_COUNT 0x402c
#define MAX_REPLY (2 * 1024 * 1024)

static volatile uint32_t *uart;
static int stdio_mode, memfd = -1;
static uint64_t fb_base;
static unsigned fb_width, fb_height, fb_stride, fb_depth;

static double now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1e9;
}
static uint32_t crc32_bytes(const unsigned char *p, size_t n) {
    uint32_t c = UINT32_MAX;
    while (n--) {
        c ^= *p++;
        for (int b = 0; b < 8; b++) c = (c >> 1) ^ (0xedb88320u & -(c & 1));
    }
    return ~c;
}
static int transmit(const void *data, size_t length) {
    const unsigned char *p = data;
    double deadline = now() + 45;
    while (length) {
        if (stdio_mode) {
            ssize_t n = write(STDOUT_FILENO, p, length);
            if (n < 0 && errno == EINTR) continue;
            if (n <= 0) return -1;
            p += n; length -= (size_t)n;
        } else {
            uint32_t space = uart[TX_FREE / 4];
            if (!space) {
                if (now() > deadline) return -1;
                usleep(1000);
                continue;
            }
            if (space > 2048) return -1; // Refuse implausible/offline MMIO.
            size_t n = length < space ? length : space;
            for (size_t i = 0; i < n; i++) uart[TX8 / 4] = *p++;
            length -= n;
            deadline = now() + 45;
        }
    }
    return 0;
}
// Read exactly `length` payload bytes that follow a PUT command line.
static int receive(unsigned char *p, size_t length) {
    double deadline = now() + 60;
    while (length) {
        if (stdio_mode) {
            ssize_t n = read(STDIN_FILENO, p, length);
            if (n < 0 && errno == EINTR) continue;
            if (n <= 0) return -1;
            p += n; length -= (size_t)n;
        } else {
            uint32_t count = uart[RX_COUNT / 4];
            if (!count) {
                if (now() > deadline) return -1;
                usleep(1000);
                continue;
            }
            if (count > 2048) return -1;
            size_t n = length < count ? length : count;
            for (size_t i = 0; i < n; i++) *p++ = (uart[RX8 / 4] >> 8) & 255;
            length -= n;
            deadline = now() + 60;
        }
    }
    return 0;
}
static void reply(const char *kind, const void *data, size_t length) {
    if (length > MAX_REPLY) return;
    char header[100];
    int n = snprintf(header, sizeof(header), "\nACOS1 %s %zu %08x\n", kind, length,
                     crc32_bytes(data, length));
    if (transmit(header, (size_t)n) || transmit(data, length)) return;
    transmit("\nACOS1 END\n", 11);
}
static void error_reply(const char *message) { reply("ERROR", message, strlen(message)); }
static void append_file(FILE *out, const char *path, size_t limit) {
    FILE *f = fopen(path, "r");
    if (!f) { fprintf(out, "%s: %s\n", path, strerror(errno)); return; }
    fprintf(out, "--- %s ---\n", path);
    struct stat st;
    if (!fstat(fileno(f), &st) && st.st_size > (off_t)limit)
        fseek(f, st.st_size - (off_t)limit, SEEK_SET);
    char buf[4096]; size_t n;
    while (limit && (n = fread(buf, 1, limit < sizeof(buf) ? limit : sizeof(buf), f))) {
        fwrite(buf, 1, n, out); limit -= n;
    }
    fputc('\n', out); fclose(f);
}
static void list_dir(FILE *out, const char *path) {
    fprintf(out, "%s:", path);
    DIR *d = opendir(path);
    if (d) {
        struct dirent *e;
        while ((e = readdir(d))) if (e->d_name[0] != '.') fprintf(out, " %s", e->d_name);
        closedir(d);
    }
    fputc('\n', out);
}
static void text_response(int logs) {
    char *data = NULL; size_t size = 0;
    FILE *out = open_memstream(&data, &size);
    if (!out) { error_reply("out of memory"); return; }
    if (logs) {
        append_file(out, "/run/ac-native-console.log", 65536);
        append_file(out, "/mnt/ac-native.log", 65536);
        append_file(out, "/run/ac-native.exit", 1024);
    } else {
        fprintf(out, "AC OS DebugUSB monitor pid=%d\n", getpid());
        append_file(out, "/proc/version", 4096);
        append_file(out, "/proc/uptime", 256);
        int pid = 0;
        FILE *f = fopen("/run/ac-native.pid", "r");
        if (f) { if (fscanf(f, "%d", &pid) != 1) pid = 0; fclose(f); }
        fprintf(out, "ac-native pid=%d alive=%s\n", pid,
                pid > 1 && (!kill(pid, 0) || errno == EPERM) ? "yes" : "no");
        fprintf(out, "firmware framebuffer=%ux%u stride=%u depth=%u\n",
                fb_width, fb_height, fb_stride, fb_depth);
        list_dir(out, "/sys/class/graphics"); list_dir(out, "/sys/class/drm");
        list_dir(out, "/sys/class/net"); list_dir(out, "/sys/class/udc");
        list_dir(out, "/dev/input");
        list_dir(out, "/sys/class/backlight"); list_dir(out, "/sys/class/ieee80211");
        list_dir(out, "/sys/bus/pci/devices");
        append_file(out, "/proc/bus/input/devices", 16384);
        append_file(out, "/proc/asound/cards", 4096);
        append_file(out, "/proc/asound/pcm", 4096);
        if (pid > 1) {
            char path[80]; snprintf(path, sizeof(path), "/proc/%d/stat", pid);
            append_file(out, path, 2048);
        }
        append_file(out, "/run/ac-native.exit", 1024);
    }
    fclose(out); reply(logs ? "LOG" : "STATUS", data, size); free(data);
}
static void hardware_checks(void) {
    FILE *out = tmpfile();
    if (!out) { error_reply(strerror(errno)); return; }
    pid_t child = fork();
    if (child < 0) { fclose(out); error_reply(strerror(errno)); return; }
    if (!child) {
        if (dup2(fileno(out), STDOUT_FILENO) < 0) _exit(126);
        int nullfd = open("/dev/null", O_RDWR);
        if (nullfd >= 0) {
            dup2(nullfd, STDIN_FILENO); dup2(nullfd, STDERR_FILENO);
            if (nullfd > STDERR_FILENO) close(nullfd);
        }
        if (memfd >= 0) close(memfd);
        execl("/neo-hardware-check", "neo-hardware-check", (char *)NULL);
        _exit(127);
    }
    double deadline = now() + 10;
    int status = 0;
    pid_t waited;
    while ((waited = waitpid(child, &status, WNOHANG)) == 0 && now() < deadline) usleep(10000);
    if (waited == 0) {
        kill(child, SIGKILL);
        while (waitpid(child, &status, 0) < 0 && errno == EINTR) {}
        fclose(out); error_reply("hardware checks timed out"); return;
    }
    if (waited < 0 || !WIFEXITED(status) || WEXITSTATUS(status) > 1) {
        fclose(out); error_reply("hardware check helper unavailable"); return;
    }
    char data[16384];
    rewind(out);
    size_t n = fread(data, 1, sizeof(data), out);
    fclose(out);
    if (!n || n == sizeof(data)) error_reply("invalid hardware check response");
    else reply("CHECKS", data, n);
}
static void framebuffer(void) {
    if (memfd < 0 || !fb_base || fb_width < 4 || fb_height < 4 || fb_width > 8192 ||
        fb_height > 8192 || fb_depth != 32 || fb_stride < fb_width * 4 || fb_stride > 65536) {
        error_reply("framebuffer metadata unavailable or unsupported"); return;
    }
    long page = sysconf(_SC_PAGESIZE);
    uint64_t start = fb_base & ~((uint64_t)page - 1);
    size_t delta = (size_t)(fb_base - start), length = (size_t)fb_stride * fb_height;
    void *mapping = mmap(NULL, length + delta, PROT_READ, MAP_SHARED, memfd, (off_t)start);
    if (mapping == MAP_FAILED) { error_reply(strerror(errno)); return; }
    const unsigned char *pixels = (const unsigned char *)mapping + delta;
    unsigned scale = 4;
    while ((size_t)(fb_width / scale) * (fb_height / scale) * 3 + 64 > MAX_REPLY) scale++;
    unsigned w = fb_width / scale, h = fb_height / scale;
    size_t capacity = (size_t)w * h * 3 + 64;
    unsigned char *ppm = malloc(capacity);
    if (!ppm) { munmap(mapping, length + delta); error_reply("out of memory"); return; }
    size_t n = (size_t)snprintf((char *)ppm, 64, "P6\n%u %u\n255\n", w, h);
    for (unsigned y = 0; y < h; y++) for (unsigned x = 0; x < w; x++) {
        uint32_t pixel;
        memcpy(&pixel, pixels + (size_t)y * scale * fb_stride + x * scale * 4, 4);
        ppm[n++] = (pixel >> 16) & 255;
        ppm[n++] = (pixel >> 8) & 255;
        ppm[n++] = pixel & 255;
    }
    munmap(mapping, length + delta);
    reply("FRAME", ppm, n); free(ppm);
}
// PUT <path> <size> <crc32hex>, payload follows the line: development-only
// replacement of piece files.  Only /pieces/*.mjs|.lisp and /usr/lib/*.mjs,
// plain characters, no '..'.  The payload is consumed even when the path is
// rejected so the link stays in sync.  Permitted by Jeffrey 2026-10-14; the
// link is the physical DebugUSB cable to the host Mac.
static int piece_path_ok(const char *path) {
    size_t len = strlen(path);
    if (len < 6 || len > 200 || path[0] != '/' || strstr(path, "..") || strstr(path, "//")) return 0;
    if (strncmp(path, "/pieces/", 8) && strncmp(path, "/usr/lib/", 9)) return 0;
    for (const char *c = path; *c; c++)
        if (!((*c >= 'a' && *c <= 'z') || (*c >= 'A' && *c <= 'Z') || (*c >= '0' && *c <= '9') ||
              *c == '/' || *c == '-' || *c == '_' || *c == '.')) return 0;
    return (len > 4 && !strcmp(path + len - 4, ".mjs")) || (len > 5 && !strcmp(path + len - 5, ".lisp"));
}
static void put_file(const char *args) {
    char path[256]; unsigned long size; unsigned crc;
    if (sscanf(args, "%255s %lu %x", path, &size, &crc) != 3 || size > MAX_REPLY) {
        error_reply("usage: ACOS PUT <path> <size> <crc32>"); return;
    }
    unsigned char *data = malloc(size ? size : 1);
    if (!data) { error_reply("out of memory"); return; }
    if (receive(data, size)) { free(data); error_reply("payload timeout"); return; }
    if (crc32_bytes(data, size) != crc) { free(data); error_reply("payload crc mismatch"); return; }
    if (!piece_path_ok(path)) { free(data); error_reply("path not allowed (pieces only)"); return; }
    char tmp[300];
    snprintf(tmp, sizeof(tmp), "%s.tmp", path);
    FILE *f = fopen(tmp, "wb");
    if (!f || fwrite(data, 1, size, f) != size) {
        if (f) fclose(f);
        unlink(tmp); free(data); error_reply(strerror(errno)); return;
    }
    free(data);
    if (fclose(f) || rename(tmp, path)) { unlink(tmp); error_reply(strerror(errno)); return; }
    char msg[320];
    int n = snprintf(msg, sizeof(msg), "%s %lu bytes", path, size);
    reply("PUT", msg, (size_t)n);
}
// RESTART: end the running ac-native so /init's respawn loop relaunches it
// (used after replacing piece files during development).
static void restart_runtime(void) {
    FILE *f = fopen("/run/ac-native.pid", "r");
    int pid = 0;
    if (f) { if (fscanf(f, "%d", &pid) != 1) pid = 0; fclose(f); }
    if (pid <= 1) { error_reply("ac-native pid unknown"); return; }
    if (kill(pid, SIGTERM)) { error_reply(strerror(errno)); return; }
    for (int i = 0; i < 50 && !kill(pid, 0); i++) usleep(100000);
    if (!kill(pid, 0)) kill(pid, SIGKILL);
    reply("RESTART", "ac-native relaunching", 21);
}
static void dispatch(const char *command) {
    if (!strcmp(command, "ACOS PING")) reply("PONG", "linux-userspace", 15);
    else if (!strcmp(command, "ACOS RESTART")) restart_runtime();
    else if (!strncmp(command, "ACOS PUT ", 9)) put_file(command + 9);
    else if (!strcmp(command, "ACOS STATUS")) text_response(0);
    else if (!strcmp(command, "ACOS LOG")) text_response(1);
    else if (!strcmp(command, "ACOS FRAME")) framebuffer();
    else if (!strcmp(command, "ACOS CHECKS")) hardware_checks();
    else if (!strcmp(command, "ACOS DMESG")) {
        char *data = malloc(131072);
        if (!data) { error_reply("out of memory"); return; }
        int n = klogctl(3, data, 131072);
        if (n < 0) error_reply(strerror(errno)); else reply("DMESG", data, (size_t)n);
        free(data);
    } else if (!strncmp(command, "ACOS ", 5)) error_reply("unknown command");
}
int main(int argc, char **argv) {
    stdio_mode = argc == 2 && !strcmp(argv[1], "--stdio");
    signal(SIGPIPE, SIG_IGN);
    if (!stdio_mode) {
        char compatible[256] = {0}, cmdline[4096] = {0};
        FILE *f = fopen("/proc/device-tree/compatible", "r");
        size_t n = f ? fread(compatible, 1, sizeof(compatible), f) : 0;
        if (f) fclose(f);
        if (!memmem(compatible, n, "apple,t8140", 11)) {
            fprintf(stderr, "neo-usb-monitor: target must be T8140\n"); return 1;
        }
        f = fopen("/proc/cmdline", "r");
        if (f) { if (!fgets(cmdline, sizeof(cmdline), f)) cmdline[0] = 0; fclose(f); }
        char *arg = strstr(cmdline, "ac_debug_uart=");
        uint64_t base = 0;
        if (!arg || sscanf(arg, "ac_debug_uart=%" SCNx64, &base) != 1 || base != UART_BASE) {
            fprintf(stderr, "neo-usb-monitor: verified UART address missing\n"); return 1;
        }
        arg = strstr(cmdline, "ac_debug_fb=");
        if (arg && sscanf(arg, "ac_debug_fb=%" SCNx64 ",%u,%u,%u,%u", &fb_base,
                          &fb_width, &fb_height, &fb_stride, &fb_depth) != 5) fb_base = 0;
        memfd = open("/dev/mem", O_RDWR | O_SYNC);
        if (memfd < 0) { perror("neo-usb-monitor /dev/mem"); return 1; }
        uart = mmap(NULL, UART_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, memfd, UART_BASE);
        if ((void *)uart == MAP_FAILED) { perror("neo-usb-monitor mmap"); return 1; }
        reply("READY", "linux-userspace", 15);
    }
    char command[320]; size_t used = 0; int overflow = 0; // PUT lines carry a path
    for (;;) {
        unsigned char c;
        if (stdio_mode) {
            ssize_t n = read(STDIN_FILENO, &c, 1);
            if (n < 0 && errno == EINTR) continue;
            if (n <= 0) break;
        } else {
            uint32_t count = uart[RX_COUNT / 4];
            if (!count) { usleep(2000); continue; }
            if (count > 2048) { fprintf(stderr, "Invalid RX count\n"); return 1; }
            c = (uart[RX8 / 4] >> 8) & 255;
        }
        if (c == '\r') continue;
        if (c == '\n') {
            command[used] = 0;
            if (!overflow) dispatch(command);
            used = 0; overflow = 0;
        } else if (c >= 32 && c < 127) {
            if (used < sizeof(command) - 1) command[used++] = (char)c;
            else overflow = 1;
        } else { used = 0; overflow = 1; }
    }
    return 0;
}
