#include "input.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <linux/input.h>
#include <linux/hidraw.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <errno.h>

const char *input_key_name(int code) {
    switch (code) {
        case KEY_A: return "a"; case KEY_B: return "b"; case KEY_C: return "c";
        case KEY_D: return "d"; case KEY_E: return "e"; case KEY_F: return "f";
        case KEY_G: return "g"; case KEY_H: return "h"; case KEY_I: return "i";
        case KEY_J: return "j"; case KEY_K: return "k"; case KEY_L: return "l";
        case KEY_M: return "m"; case KEY_N: return "n"; case KEY_O: return "o";
        case KEY_P: return "p"; case KEY_Q: return "q"; case KEY_R: return "r";
        case KEY_S: return "s"; case KEY_T: return "t"; case KEY_U: return "u";
        case KEY_V: return "v"; case KEY_W: return "w"; case KEY_X: return "x";
        case KEY_Y: return "y"; case KEY_Z: return "z";
        case KEY_0: return "0"; case KEY_1: return "1"; case KEY_2: return "2";
        case KEY_3: return "3"; case KEY_4: return "4"; case KEY_5: return "5";
        case KEY_6: return "6"; case KEY_7: return "7"; case KEY_8: return "8";
        case KEY_9: return "9";
        case KEY_SPACE: return "space";
        case KEY_ENTER: return "enter";
        case KEY_BACKSPACE: return "backspace";
        case KEY_TAB: return "tab";
        case KEY_ESC: return "escape";
        case KEY_UP: return "arrowup";
        case KEY_DOWN: return "arrowdown";
        case KEY_LEFT: return "arrowleft";
        case KEY_RIGHT: return "arrowright";
        case KEY_LEFTSHIFT: case KEY_RIGHTSHIFT: return "shift";
        case KEY_LEFTCTRL: case KEY_RIGHTCTRL: return "control";
        case KEY_LEFTALT: case KEY_RIGHTALT: return "alt";
        case KEY_MINUS: return "-";
        case KEY_EQUAL: return "=";
        case KEY_LEFTBRACE: return "[";
        case KEY_RIGHTBRACE: return "]";
        case KEY_SEMICOLON: return ";";
        case KEY_APOSTROPHE: return "'";
        case KEY_COMMA: return ",";
        case KEY_DOT: return ".";
        case KEY_SLASH: return "/";
        case KEY_BACKSLASH: return "\\";
        case KEY_GRAVE: return "`";
        case KEY_DELETE: return "delete";
        case KEY_HOME: return "home";
        case KEY_END: return "end";
        case KEY_PAGEUP: return "pageup";
        case KEY_PAGEDOWN: return "pagedown";
        case KEY_F1: return "f1"; case KEY_F2: return "f2";
        case KEY_F3: return "f3"; case KEY_F4: return "f4";
        case KEY_F5: return "f5"; case KEY_F6: return "f6";
        case KEY_F7: return "f7"; case KEY_F8: return "f8";
        case KEY_F9: return "f9"; case KEY_F10: return "f10";
        case KEY_F11: return "f11"; case KEY_F12: return "f12";
        case KEY_MUTE: return "audiomute";
        case KEY_VOLUMEDOWN: return "audiovolumedown";
        case KEY_VOLUMEUP: return "audiovolumeup";
        case KEY_POWER: return "power";
        case KEY_BRIGHTNESSDOWN: return "brightnessdown";
        case KEY_BRIGHTNESSUP: return "brightnessup";
        case KEY_MICMUTE: return "micmute";
        case KEY_SWITCHVIDEOMODE: return "switchvideo";
        case KEY_WLAN: return "wlan";
        case KEY_MEDIA: return "media";
        case KEY_KBDILLUMTOGGLE: return "kbdlight";
        default: return NULL;
    }
}

// NuPhy HE scancode → Linux keycode mapping
// NuPhy uses HID scancodes; modifiers use special bitmask codes
static int nuphy_scancode_to_keycode(int sc) {
    // Modifier keys (NuPhy bitmask codes)
    switch (sc) {
        case 0x100: return KEY_LEFTCTRL;
        case 0x200: return KEY_LEFTSHIFT;
        case 0x400: return KEY_LEFTALT;
        case 0x800: return KEY_LEFTMETA;
        case 0x1000: return KEY_RIGHTCTRL;
        case 0x2000: return KEY_RIGHTSHIFT;
        case 0x4000: return KEY_RIGHTALT;
        case 0x8000: return KEY_RIGHTMETA;
        case 0xff05: return KEY_FN;  // Fn key
    }
    // Standard HID usage → Linux keycode (HID usage page 0x07)
    // Letters A-Z: HID 0x04-0x1D → KEY_A-KEY_Z
    if (sc >= 0x04 && sc <= 0x1D) {
        static const int letter_map[] = {
            KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I,
            KEY_J, KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R,
            KEY_S, KEY_T, KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z
        };
        return letter_map[sc - 0x04];
    }
    // Numbers 1-0: HID 0x1E-0x27
    if (sc >= 0x1E && sc <= 0x27) {
        static const int num_map[] = {
            KEY_1, KEY_2, KEY_3, KEY_4, KEY_5, KEY_6, KEY_7, KEY_8, KEY_9, KEY_0
        };
        return num_map[sc - 0x1E];
    }
    // Common keys
    switch (sc) {
        case 0x28: return KEY_ENTER;
        case 0x29: return KEY_ESC;
        case 0x2A: return KEY_BACKSPACE;
        case 0x2B: return KEY_TAB;
        case 0x2C: return KEY_SPACE;
        case 0x2D: return KEY_MINUS;
        case 0x2E: return KEY_EQUAL;
        case 0x2F: return KEY_LEFTBRACE;
        case 0x30: return KEY_RIGHTBRACE;
        case 0x31: return KEY_BACKSLASH;
        case 0x33: return KEY_SEMICOLON;
        case 0x34: return KEY_APOSTROPHE;
        case 0x35: return KEY_GRAVE;
        case 0x36: return KEY_COMMA;
        case 0x37: return KEY_DOT;
        case 0x38: return KEY_SLASH;
        // Arrow keys
        case 0x4F: return KEY_RIGHT;
        case 0x50: return KEY_LEFT;
        case 0x51: return KEY_DOWN;
        case 0x52: return KEY_UP;
        // F-keys
        case 0x3A: return KEY_F1;  case 0x3B: return KEY_F2;
        case 0x3C: return KEY_F3;  case 0x3D: return KEY_F4;
        case 0x3E: return KEY_F5;  case 0x3F: return KEY_F6;
        case 0x40: return KEY_F7;  case 0x41: return KEY_F8;
        case 0x42: return KEY_F9;  case 0x43: return KEY_F10;
        case 0x44: return KEY_F11; case 0x45: return KEY_F12;
        case 0x46: return KEY_SYSRQ;
        case 0x47: return KEY_SCROLLLOCK;
        case 0x48: return KEY_PAUSE;
        case 0x49: return KEY_INSERT;
        case 0x4A: return KEY_HOME;
        case 0x4B: return KEY_PAGEUP;
        case 0x4C: return KEY_DELETE;
        case 0x4D: return KEY_END;
        case 0x4E: return KEY_PAGEDOWN;
    }
    return -1; // Unknown
}

// Send analog activation commands to a NuPhy HE keyboard
static void nuphy_activate_analog(int fd, const char *path) {
    // NuPhy HE requires two output reports to start streaming analog data:
    // [0x55, 0xA8] — enable analog mode
    // [0x55, 0xA0] — start analog streaming
    unsigned char buf[64];
    for (int i = 0; i < 2; i++) {
        memset(buf, 0, sizeof(buf));
        buf[0] = 0x55;
        buf[1] = (i == 0) ? 0xA8 : 0xA0;
        int ret = write(fd, buf, sizeof(buf));
        fprintf(stderr, "[input] NuPhy activate [0x55, 0x%02X] on %s: %s\n",
                buf[1], path, ret > 0 ? "OK" : "failed");
    }
}

// Scan /dev/hidraw* for NuPhy analog keyboards
static void hidraw_scan(ACInput *input) {
    input->hidraw_count = 0;
    input->has_analog = 0;

    DIR *dir = opendir("/dev");
    if (!dir) return;

    struct dirent *ent;
    while ((ent = readdir(dir)) && input->hidraw_count < MAX_HIDRAW_DEVICES) {
        if (strncmp(ent->d_name, "hidraw", 6) != 0) continue;

        char path[64];
        snprintf(path, sizeof(path), "/dev/%s", ent->d_name);
        // Open read-write so we can send activation commands
        int fd = open(path, O_RDWR | O_NONBLOCK | O_CLOEXEC);
        if (fd < 0) {
            // Fall back to read-only if RW fails
            fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
            if (fd < 0) continue;
        }

        // Check vendor ID via HIDIOCGRAWINFO
        struct hidraw_devinfo info;
        if (ioctl(fd, HIDIOCGRAWINFO, &info) >= 0) {
            fprintf(stderr, "[input] hidraw: %s vendor=0x%04x product=0x%04x\n",
                    path, info.vendor, info.product);
            if (info.vendor == NUPHY_VENDOR_ID) {
                // Send analog activation commands to all NuPhy interfaces
                nuphy_activate_analog(fd, path);
                input->hidraw_fds[input->hidraw_count++] = fd;
                input->has_analog = 1;
                fprintf(stderr, "[input] NuPhy HE analog keyboard found: %s\n", path);
                continue;
            }
        }
        close(fd);
    }
    closedir(dir);

    if (input->has_analog)
        fprintf(stderr, "[input] Analog keyboard: %d hidraw device(s)\n", input->hidraw_count);
}

static int hidraw_debug_count = 0;

// Process a NuPhy HID report and generate AC events
// Protocol (reverse-engineered from NuPhy Air60 HE):
//   Byte 0:   0xA0 — analog report marker
//   Byte 1:   0x10 — normal report (0xF0 = special/config)
//   Bytes 2-3: HID scancode (big-endian), e.g. 0x000E = K key
//   Bytes 4-5: 16-bit pressure (big-endian), 0x0000=released, ~0x0640=full press
//   Byte 9:   0x01 = pressing down, 0xFF = releasing
static void hidraw_process_nuphy(ACInput *input, unsigned char *buf, int len) {
    // Log first 20 reports for debugging
    if (hidraw_debug_count < 20) {
        fprintf(stderr, "[hidraw] report len=%d:", len);
        for (int i = 0; i < len && i < 16; i++)
            fprintf(stderr, " %02x", buf[i]);
        fprintf(stderr, "\n");
        hidraw_debug_count++;
    }

    if (len < 10 || buf[0] != 0xA0) return; // Not an analog report
    if (buf[1] == 0xF0) return; // Config/special report, skip

    int scancode = (buf[2] << 8) | buf[3];
    int raw_pressure = (buf[4] << 8) | buf[5]; // 0 to ~1600 (0x0640)
    int direction = buf[9]; // 0x01 = pressing, 0xFF = releasing

    // Normalize pressure to 0.0-1.0 (max observed ~1600 = 0x0640)
    float pressure = raw_pressure / 1600.0f;
    if (pressure > 1.0f) pressure = 1.0f;

    int keycode = nuphy_scancode_to_keycode(scancode);
    if (keycode < 0) return; // Unknown scancode

    // Find existing analog key slot
    int slot = -1;
    for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
        if (input->analog_keys[i].active && input->analog_keys[i].key_code == keycode) {
            slot = i;
            break;
        }
    }

    // Activation threshold: ignore very light touches (noise/accidental)
    // raw_pressure ~1600 = full press, threshold ~80 = ~5%
    #define ANALOG_ACTIVATE_THRESHOLD 80
    #define ANALOG_DEACTIVATE_THRESHOLD 40  // Lower than activate (hysteresis)

    if (raw_pressure >= ANALOG_ACTIVATE_THRESHOLD && slot < 0) {
        // New key press above threshold — find empty slot
        for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
            if (!input->analog_keys[i].active) { slot = i; break; }
        }
        if (slot < 0) return; // No slots

        input->analog_keys[slot].active = 1;
        input->analog_keys[slot].releasing = 0;
        input->analog_keys[slot].key_code = keycode;
        input->analog_keys[slot].pressure = pressure;
        input->analog_keys[slot].target = pressure;
        input->analog_keys[slot].raw_accum = 0;
        input->analog_keys[slot].raw_count = 0;

        // Generate key down event with initial pressure
        if (input->event_count < MAX_EVENTS_PER_FRAME) {
            ACEvent *ae = &input->events[input->event_count];
            memset(ae, 0, sizeof(ACEvent));
            ae->type = AC_EVENT_KEYBOARD_DOWN;
            ae->key_code = keycode;
            ae->pressure = pressure;
            const char *name = input_key_name(keycode);
            if (name) strncpy(ae->key_name, name, sizeof(ae->key_name) - 1);
            else snprintf(ae->key_name, sizeof(ae->key_name), "?%d", keycode);
            input->event_count++;
        }
    } else if (raw_pressure >= ANALOG_DEACTIVATE_THRESHOLD && slot >= 0) {
        // Key still pressed — accumulate raw pressure for per-frame averaging
        input->analog_keys[slot].raw_accum += pressure;
        input->analog_keys[slot].raw_count++;
        input->analog_keys[slot].target = pressure; // Track latest raw for when reports stop
        // Cancel any in-progress release fade
        if (input->analog_keys[slot].releasing)
            input->analog_keys[slot].releasing = 0;
    } else {
        // Pressure below deactivate threshold or zero — mark as releasing
        if (slot >= 0 && !input->analog_keys[slot].releasing) {
            input->analog_keys[slot].releasing = 1;
        }
    }
}

ACInput *input_init(int screen_w, int screen_h) {
    ACInput *input = calloc(1, sizeof(ACInput));
    if (!input) return NULL;

    input->screen_w = screen_w;
    input->screen_h = screen_h;

    // Scan /dev/input/ for event devices
    DIR *dir = opendir("/dev/input");
    if (!dir) {
        fprintf(stderr, "[input] Cannot open /dev/input\n");
        return input;
    }

    struct dirent *ent;
    while ((ent = readdir(dir)) && input->count < MAX_INPUT_DEVICES) {
        if (strncmp(ent->d_name, "event", 5) != 0) continue;

        char path[64];
        snprintf(path, sizeof(path), "/dev/input/%s", ent->d_name);
        int fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
        if (fd < 0) continue;

        // Check if this device has keys or touch
        unsigned long evbits = 0;
        ioctl(fd, EVIOCGBIT(0, sizeof(evbits)), &evbits);
        if (evbits & ((1 << EV_KEY) | (1 << EV_ABS) | (1 << EV_REL))) {
            // Check if this is a NuPhy device (will be handled via hidraw for analog)
            struct input_id devid;
            int is_nuphy = 0;
            if (ioctl(fd, EVIOCGID, &devid) >= 0 && devid.vendor == NUPHY_VENDOR_ID) {
                is_nuphy = 1;
                fprintf(stderr, "[input] Opened %s (NuPhy — evdev keys suppressed, using hidraw)\n", path);
            } else {
                fprintf(stderr, "[input] Opened %s\n", path);
            }
            input->fd_is_analog[input->count] = is_nuphy;
            input->fds[input->count++] = fd;
        } else {
            close(fd);
        }
    }
    closedir(dir);

    fprintf(stderr, "[input] %d evdev devices\n", input->count);

    // Scan for analog keyboards (NuPhy HE via hidraw)
    hidraw_scan(input);

    return input;
}

static int input_debug_frames = 0;

void input_poll(ACInput *input) {
    if (!input) return;
    input->event_count = 0;
    input->delta_x = 0;
    input->delta_y = 0;

    // Debug: log analog state every 5 seconds
    input_debug_frames++;
    if (input_debug_frames % 300 == 0 && input->has_analog) {
        int active = 0, releasing = 0;
        for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
            if (input->analog_keys[i].active) active++;
            if (input->analog_keys[i].releasing) releasing++;
        }
        fprintf(stderr, "[input] analog: %d active, %d releasing, has_analog=%d, hidraw=%d, evdev=%d\n",
                active, releasing, input->has_analog, input->hidraw_count, input->count);
    }

    struct input_event ev;
    for (int d = 0; d < input->count; d++) {
        ssize_t rr;
        while ((rr = read(input->fds[d], &ev, sizeof(ev))) == sizeof(ev)) {
            if (input->event_count >= MAX_EVENTS_PER_FRAME) break;

            ACEvent *ae = &input->events[input->event_count];
            memset(ae, 0, sizeof(ACEvent));

            if (ev.type == EV_KEY) {
                // Mouse/touch button
                if (ev.code == BTN_LEFT || ev.code == BTN_TOUCH) {
                    if (ev.value == 1) {
                        ae->type = AC_EVENT_TOUCH;
                        input->pointer_down = 1;
                    } else if (ev.value == 0) {
                        ae->type = AC_EVENT_LIFT;
                        input->pointer_down = 0;
                    }
                    ae->x = input->pointer_x;
                    ae->y = input->pointer_y;
                    input->event_count++;
                }
                // Keyboard
                else if (ev.code < BTN_MISC) {
                    // Skip duplicate evdev events from NuPhy (handled via hidraw analog)
                    if (input->fd_is_analog[d] && input->has_analog)
                        continue;
                    const char *name = input_key_name(ev.code);
                    ae->type = ev.value ? AC_EVENT_KEYBOARD_DOWN : AC_EVENT_KEYBOARD_UP;
                    ae->key_code = ev.code;
                    ae->repeat = (ev.value == 2);
                    if (name) {
                        strncpy(ae->key_name, name, sizeof(ae->key_name) - 1);
                    } else {
                        // Unknown key — store code as name for debug
                        snprintf(ae->key_name, sizeof(ae->key_name), "?%d", ev.code);
                    }
                    input->event_count++;
                }
            } else if (ev.type == EV_REL) {
                // Relative mouse movement
                if (ev.code == REL_X) { input->pointer_x += ev.value; input->delta_x += ev.value; }
                if (ev.code == REL_Y) { input->pointer_y += ev.value; input->delta_y += ev.value; }
                // Clamp
                if (input->pointer_x < 0) input->pointer_x = 0;
                if (input->pointer_y < 0) input->pointer_y = 0;
                if (input->pointer_x >= input->screen_w) input->pointer_x = input->screen_w - 1;
                if (input->pointer_y >= input->screen_h) input->pointer_y = input->screen_h - 1;

                if (input->pointer_down) {
                    ae->type = AC_EVENT_DRAW;
                    ae->x = input->pointer_x;
                    ae->y = input->pointer_y;
                    input->event_count++;
                }
            } else if (ev.type == EV_ABS) {
                // Absolute touch/tablet
                if (ev.code == ABS_X || ev.code == ABS_MT_POSITION_X) {
                    // Scale to screen (TODO: get abs info for proper scaling)
                    input->pointer_x = ev.value;
                }
                if (ev.code == ABS_Y || ev.code == ABS_MT_POSITION_Y) {
                    input->pointer_y = ev.value;
                }
            }
        }
        // Detect dead evdev fd (device unplugged)
        if (rr < 0 && errno == ENODEV) {
            fprintf(stderr, "[input] evdev device %d disconnected\n", d);
            close(input->fds[d]);
            for (int j = d; j < input->count - 1; j++) {
                input->fds[j] = input->fds[j + 1];
                input->fd_is_analog[j] = input->fd_is_analog[j + 1];
            }
            input->count--;
            d--;
        }
    }

    // Poll NuPhy hidraw devices for analog key data
    for (int d = 0; d < input->hidraw_count; d++) {
        unsigned char buf[64];
        int n;
        while ((n = read(input->hidraw_fds[d], buf, sizeof(buf))) > 0) {
            hidraw_process_nuphy(input, buf, n);
            if (input->event_count >= MAX_EVENTS_PER_FRAME) break;
        }
        // Detect disconnected hidraw (read returns -1 with ENODEV)
        if (n < 0 && errno == ENODEV) {
            fprintf(stderr, "[input] NuPhy hidraw device %d disconnected\n", d);
            close(input->hidraw_fds[d]);
            // Shift remaining fds down
            for (int j = d; j < input->hidraw_count - 1; j++)
                input->hidraw_fds[j] = input->hidraw_fds[j + 1];
            input->hidraw_count--;
            if (input->hidraw_count == 0) {
                input->has_analog = 0;
                // Release all analog keys immediately on disconnect
                for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
                    if (input->analog_keys[i].active) {
                        input->analog_keys[i].active = 0;
                        input->analog_keys[i].releasing = 0;
                        input->analog_keys[i].pressure = 0;
                        if (input->event_count < MAX_EVENTS_PER_FRAME) {
                            ACEvent *ae = &input->events[input->event_count];
                            memset(ae, 0, sizeof(ACEvent));
                            ae->type = AC_EVENT_KEYBOARD_UP;
                            ae->key_code = input->analog_keys[i].key_code;
                            const char *name = input_key_name(ae->key_code);
                            if (name) strncpy(ae->key_name, name, sizeof(ae->key_name) - 1);
                            input->event_count++;
                        }
                    }
                }
                // Also close dead NuPhy evdev fds and rescan
                for (int i = input->count - 1; i >= 0; i--) {
                    if (input->fd_is_analog[i]) {
                        close(input->fds[i]);
                        for (int j = i; j < input->count - 1; j++) {
                            input->fds[j] = input->fds[j + 1];
                            input->fd_is_analog[j] = input->fd_is_analog[j + 1];
                        }
                        input->count--;
                    }
                }
            }
            d--;
        }
    }

    // Per-frame analog key processing: apply averaged pressure + smooth release fade
    for (int i = 0; i < MAX_ANALOG_KEYS; i++) {
        if (!input->analog_keys[i].active) continue;

        // Apply accumulated HID reports as averaged pressure via asymmetric EMA
        if (input->analog_keys[i].raw_count > 0) {
            float avg = input->analog_keys[i].raw_accum / input->analog_keys[i].raw_count;
            input->analog_keys[i].target = avg; // Update target from latest reports
            input->analog_keys[i].raw_accum = 0;
            input->analog_keys[i].raw_count = 0;
        }
        // Always drive pressure toward target (NuPhy stops sending when stable)
        if (!input->analog_keys[i].releasing) {
            float prev = input->analog_keys[i].pressure;
            float tgt = input->analog_keys[i].target;
            float alpha = (tgt > prev) ? 0.3f : 0.15f; // Fast attack, smooth ease-off
            input->analog_keys[i].pressure = prev + alpha * (tgt - prev);
        }

        // Fade releasing keys (NuPhy sends only one pressure=0 report)
        if (!input->analog_keys[i].releasing) continue;
        float prev = input->analog_keys[i].pressure;
        if (prev > 0.005f) {
            // Slow exponential fade: 0.92x per frame = ~50 frames (~830ms) to silence
            input->analog_keys[i].pressure = prev * 0.92f;
        } else {
            // Done fading — fire key-up
            input->analog_keys[i].active = 0;
            input->analog_keys[i].releasing = 0;
            input->analog_keys[i].pressure = 0;
            if (input->event_count < MAX_EVENTS_PER_FRAME) {
                ACEvent *ae = &input->events[input->event_count];
                memset(ae, 0, sizeof(ACEvent));
                ae->type = AC_EVENT_KEYBOARD_UP;
                ae->key_code = input->analog_keys[i].key_code;
                ae->pressure = 0;
                const char *name = input_key_name(ae->key_code);
                if (name) strncpy(ae->key_name, name, sizeof(ae->key_name) - 1);
                else snprintf(ae->key_name, sizeof(ae->key_name), "?%d", ae->key_code);
                input->event_count++;
            }
        }
    }

    // Late evdev rescan: pick up ACPI power button and other late devices (once, ~3s after boot)
    if (!input->evdev_rescan_done) {
        input->evdev_rescan_counter++;
        if (input->evdev_rescan_counter >= 180) { // ~3 seconds at 60fps
            input->evdev_rescan_done = 1;
            DIR *dir = opendir("/dev/input");
            if (dir) {
                struct dirent *ent;
                while ((ent = readdir(dir)) && input->count < MAX_INPUT_DEVICES) {
                    if (strncmp(ent->d_name, "event", 5) != 0) continue;
                    char path[64];
                    snprintf(path, sizeof(path), "/dev/input/%s", ent->d_name);
                    // Check if we already have this fd open
                    int already_open = 0;
                    // Compare by trying to open and checking device identity
                    int fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
                    if (fd < 0) continue;
                    // Check if any existing fd points to same device (by dev number)
                    struct stat st_new;
                    fstat(fd, &st_new);
                    for (int i = 0; i < input->count; i++) {
                        struct stat st_old;
                        fstat(input->fds[i], &st_old);
                        if (st_new.st_rdev == st_old.st_rdev) { already_open = 1; break; }
                    }
                    if (already_open) { close(fd); continue; }
                    unsigned long evbits = 0;
                    ioctl(fd, EVIOCGBIT(0, sizeof(evbits)), &evbits);
                    if (evbits & (1 << EV_KEY)) {
                        char name[256] = "";
                        ioctl(fd, EVIOCGNAME(sizeof(name)), name);
                        fprintf(stderr, "[input] Late device: %s (%s)\n", path, name);
                        input->fds[input->count++] = fd;
                    } else {
                        close(fd);
                    }
                }
                closedir(dir);
            }
        }
    }

    // Hot-plug: periodically re-scan for NuPhy if not connected
    if (!input->has_analog) {
        input->hotplug_counter++;
        if (input->hotplug_counter >= 120) { // ~2 seconds at 60fps
            input->hotplug_counter = 0;
            hidraw_scan(input);
            // If NuPhy reconnected, also rescan evdev for its new event devices
            if (input->has_analog) {
                DIR *dir = opendir("/dev/input");
                if (dir) {
                    struct dirent *ent;
                    while ((ent = readdir(dir)) && input->count < MAX_INPUT_DEVICES) {
                        if (strncmp(ent->d_name, "event", 5) != 0) continue;
                        char path[64];
                        snprintf(path, sizeof(path), "/dev/input/%s", ent->d_name);
                        int fd = open(path, O_RDONLY | O_NONBLOCK | O_CLOEXEC);
                        if (fd < 0) continue;
                        struct stat st_new;
                        fstat(fd, &st_new);
                        int already_open = 0;
                        for (int i = 0; i < input->count; i++) {
                            struct stat st_old;
                            fstat(input->fds[i], &st_old);
                            if (st_new.st_rdev == st_old.st_rdev) { already_open = 1; break; }
                        }
                        if (already_open) { close(fd); continue; }
                        unsigned long evbits = 0;
                        ioctl(fd, EVIOCGBIT(0, sizeof(evbits)), &evbits);
                        if (evbits & (1 << EV_KEY)) {
                            struct input_id devid;
                            int is_nuphy = 0;
                            if (ioctl(fd, EVIOCGID, &devid) >= 0 && devid.vendor == NUPHY_VENDOR_ID)
                                is_nuphy = 1;
                            input->fd_is_analog[input->count] = is_nuphy;
                            input->fds[input->count++] = fd;
                            char name[256] = "";
                            ioctl(fd, EVIOCGNAME(sizeof(name)), name);
                            fprintf(stderr, "[input] Hotplug evdev: %s (%s)%s\n", path, name,
                                    is_nuphy ? " [NuPhy]" : "");
                        } else {
                            close(fd);
                        }
                    }
                    closedir(dir);
                }
            }
        }
    }
}

void input_destroy(ACInput *input) {
    if (!input) return;
    for (int i = 0; i < input->count; i++)
        close(input->fds[i]);
    for (int i = 0; i < input->hidraw_count; i++)
        close(input->hidraw_fds[i]);
    free(input);
}
