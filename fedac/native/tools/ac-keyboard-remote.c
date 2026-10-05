// Keyboard input over an authenticated SSH stdin stream; no listening socket.
// K <evdev code> <0=up|1=down|2=repeat>, R=release, P <nonce>=heartbeat.
// Requires an existing evdev keyboard. Releases owned keys on EOF or timeout.
#define _POSIX_C_SOURCE 200809L
#include <linux/input.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int keyboard = -1;
static unsigned char held[KEY_MAX + 1];
static volatile sig_atomic_t stopping;
static void stop(int sig) { (void)sig; stopping = 1; }
static int bit(const unsigned char *bits, unsigned n) {
    return !!(bits[n / 8] & (1u << (n % 8)));
}
static int allowed(unsigned code) {
    return (code >= KEY_ESC && code <= KEY_KPDOT) ||
           (code >= KEY_102ND && code <= KEY_DELETE) ||
           (code >= KEY_MUTE && code <= KEY_VOLUMEUP) ||
           code == KEY_KPEQUAL || code == KEY_PAUSE || code == KEY_KPCOMMA ||
           code == KEY_LEFTMETA || code == KEY_RIGHTMETA ||
           code == KEY_COMPOSE || (code >= KEY_F13 && code <= KEY_F24);
}
static int emit(unsigned code, int value) {
    struct input_event events[2] = {
        {.type = EV_KEY, .code = (unsigned short)code, .value = value},
        {.type = EV_SYN, .code = SYN_REPORT, .value = 0}
    };
    ssize_t n;
    do { n = write(keyboard, events, sizeof(events)); } while (n < 0 && errno == EINTR);
    return n == sizeof(events) ? 0 : -1;
}
static int release(void) {
    int result = 0;
    for (unsigned code = 1; code <= KEY_MAX; code++) {
        if (held[code] && emit(code, 0)) result = -1;
        held[code] = 0;
    }
    return result;
}
static int open_keyboard(void) {
    int fallback = -1;
    for (int i = 0; i < 64; i++) {
        char path[64], name[256] = {0};
        snprintf(path, sizeof(path), "/dev/input/event%d", i);
        int fd = open(path, O_RDWR | O_CLOEXEC);
        if (fd < 0) continue;
        unsigned char bits[(KEY_MAX + 8) / 8] = {0};
        if (ioctl(fd, EVIOCGBIT(EV_KEY, sizeof(bits)), bits) < 0 ||
            !bit(bits, KEY_A) || !bit(bits, KEY_ENTER) || !bit(bits, KEY_LEFTSHIFT)) {
            close(fd); continue;
        }
        ioctl(fd, EVIOCGNAME(sizeof(name)), name);
        if (strstr(name, "AT Translated")) {
            if (fallback >= 0) close(fallback);
            return fd;
        }
        if (fallback < 0) fallback = fd; else close(fd);
    }
    return fallback;
}
static int command(const char *line) {
    if (!strcmp(line, "R")) return release();
    char nonce[65], extra;
    if (sscanf(line, "P %64[A-Za-z0-9._-] %c", nonce, &extra) == 1) {
        printf("P %s\n", nonce); return fflush(stdout);
    }
    unsigned code; int value;
    if (sscanf(line, "K %u %d %c", &code, &value, &extra) != 2 ||
        !allowed(code) || value < 0 || value > 2) return -1;
    if (value == 1 && !held[code]) {
        unsigned char physical[(KEY_MAX + 8) / 8] = {0};
        if (ioctl(keyboard, EVIOCGKEY(sizeof(physical)), physical) < 0) return -1;
        // Do not take ownership of a key already held on the physical laptop.
        if (bit(physical, code)) return 0;
        if (emit(code, 1)) return -1;
        held[code] = 1;
    } else if (held[code]) {
        if (emit(code, value == 0 ? 0 : 2)) return -1;
        if (!value) held[code] = 0;
    }
    return 0;
}
int main(void) {
    int lock = open("/tmp/ac-keyboard-remote.lock", O_CREAT | O_RDWR | O_CLOEXEC, 0600);
    if (lock < 0 || flock(lock, LOCK_EX | LOCK_NB)) {
        fputs("Remote keyboard already in use\n", stderr); return 1;
    }
    keyboard = open_keyboard();
    if (keyboard < 0) { fputs("No writable evdev keyboard\n", stderr); return 1; }
    struct sigaction action = {.sa_handler = stop};
    sigemptyset(&action.sa_mask);
    sigaction(SIGTERM, &action, NULL); sigaction(SIGINT, &action, NULL);
    sigaction(SIGHUP, &action, NULL); signal(SIGPIPE, SIG_IGN);
    puts("READY"); fflush(stdout);
    char line[128], chunk[512]; size_t used = 0; int result = 0;
    while (!stopping) {
        struct pollfd input = {.fd = STDIN_FILENO, .events = POLLIN};
        int ready = poll(&input, 1, 3500);
        if (ready < 0 && errno == EINTR) continue;
        if (ready <= 0) break;
        ssize_t n = read(STDIN_FILENO, chunk, sizeof(chunk));
        if (n <= 0) break;
        for (ssize_t i = 0; i < n; i++) {
            if (chunk[i] == '\n') {
                line[used] = 0;
                if (command(line)) { result = 1; stopping = 1; break; }
                used = 0;
            } else if (used + 1 < sizeof(line) && chunk[i] >= 32 && chunk[i] <= 126) {
                line[used++] = chunk[i];
            } else { result = 1; stopping = 1; break; }
        }
    }
    if (release()) result = 1;
    close(keyboard); close(lock);
    return result;
}
