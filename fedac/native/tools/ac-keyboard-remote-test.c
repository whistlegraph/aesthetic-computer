// Compile on Linux: gcc -Wall -Wextra -Werror ac-keyboard-remote-test.c -o /tmp/ac-remote-test
#define ioctl test_ioctl
#define main receiver_main
#include "ac-keyboard-remote.c"
#undef main
#undef ioctl
#include <assert.h>
#include <stdarg.h>

static unsigned physical_key;
int test_ioctl(int fd, unsigned long request, ...) {
    (void)fd;
    va_list args; va_start(args, request);
    unsigned char *bits = va_arg(args, unsigned char *); va_end(args);
    if (physical_key) bits[physical_key / 8] |= 1u << (physical_key % 8);
    return 0;
}
static void expect(int fd, unsigned code, int value) {
    struct input_event events[2];
    assert(read(fd, events, sizeof(events)) == sizeof(events));
    assert(events[0].type == EV_KEY && events[0].code == code && events[0].value == value);
    assert(events[1].type == EV_SYN && events[1].code == SYN_REPORT);
}
static void empty(int fd) {
    struct input_event event;
    assert(read(fd, &event, sizeof(event)) == -1 && errno == EAGAIN);
}
int main(void) {
    int events[2]; assert(pipe(events) == 0);
    keyboard = events[1]; assert(fcntl(events[0], F_SETFL, O_NONBLOCK) == 0);
    assert(command("K 30 1") == 0); expect(events[0], KEY_A, 1);
    assert(command("K 30 2") == 0); expect(events[0], KEY_A, 2);
    assert(command("K 30 0") == 0); expect(events[0], KEY_A, 0);
    assert(command("K 30 0") == 0); empty(events[0]);
    // A local physical hold must not become owned/released by the remote.
    physical_key = KEY_LEFTSHIFT;
    assert(command("K 42 1") == 0); assert(command("R") == 0); empty(events[0]);
    physical_key = 0;
    assert(command("K 42 1") == 0); expect(events[0], KEY_LEFTSHIFT, 1);
    assert(command("K 30 1") == 0); expect(events[0], KEY_A, 1);
    assert(command("R") == 0); expect(events[0], KEY_A, 0); expect(events[0], KEY_LEFTSHIFT, 0);
    // Disconnect uses the same release path; repeats without an owned hold vanish.
    assert(command("K 30 2") == 0); empty(events[0]);
    assert(command("K 30 1") == 0); expect(events[0], KEY_A, 1);
    assert(release() == 0); expect(events[0], KEY_A, 0);
    const char *bad[] = {"K 116 1", "K 65535 1", "K -1 1", "K 30 3", "K 30 -1", "K 30 1 extra", "R extra", "garbage", "P bad nonce"};
    for (unsigned i = 0; i < sizeof(bad) / sizeof(bad[0]); i++) assert(command(bad[i]) == -1);
    empty(events[0]);
    close(events[0]); close(events[1]);
    puts("PASS: key lifecycle, repeats, physical holds, release, invalid commands");
    return 0;
}
