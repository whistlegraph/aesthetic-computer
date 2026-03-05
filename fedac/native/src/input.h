#ifndef AC_INPUT_H
#define AC_INPUT_H

#include <linux/input.h>

#define MAX_INPUT_DEVICES 8
#define MAX_EVENTS_PER_FRAME 64

// AC event types
typedef enum {
    AC_EVENT_NONE = 0,
    AC_EVENT_KEYBOARD_DOWN,
    AC_EVENT_KEYBOARD_UP,
    AC_EVENT_TOUCH,
    AC_EVENT_DRAW,
    AC_EVENT_LIFT,
    AC_EVENT_REFRAMED,
} ACEventType;

typedef struct {
    ACEventType type;
    int x, y;           // Pointer coordinates
    int key_code;       // Linux key code
    char key_name[32];  // AC key name ("a", "space", "arrowup", etc.)
    int repeat;         // Key repeat flag
} ACEvent;

typedef struct {
    int fds[MAX_INPUT_DEVICES];
    int count;
    int pointer_x, pointer_y;
    int pointer_down;

    // Event queue for current frame
    ACEvent events[MAX_EVENTS_PER_FRAME];
    int event_count;

    // Screen dimensions for touch scaling
    int screen_w, screen_h;
} ACInput;

ACInput *input_init(int screen_w, int screen_h);
void input_poll(ACInput *input);
void input_destroy(ACInput *input);

// Map Linux keycode to AC key name
const char *input_key_name(int code);

#endif
