#include "input.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <linux/input.h>

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
        default: return NULL;
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
            input->fds[input->count++] = fd;
            fprintf(stderr, "[input] Opened %s\n", path);
        } else {
            close(fd);
        }
    }
    closedir(dir);

    fprintf(stderr, "[input] %d devices\n", input->count);
    return input;
}

void input_poll(ACInput *input) {
    if (!input) return;
    input->event_count = 0;
    input->delta_x = 0;
    input->delta_y = 0;

    struct input_event ev;
    for (int d = 0; d < input->count; d++) {
        while (read(input->fds[d], &ev, sizeof(ev)) == sizeof(ev)) {
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
    }
}

void input_destroy(ACInput *input) {
    if (!input) return;
    for (int i = 0; i < input->count; i++)
        close(input->fds[i]);
    free(input);
}
