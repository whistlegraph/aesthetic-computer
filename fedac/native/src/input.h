#ifndef AC_INPUT_H
#define AC_INPUT_H

#include <linux/input.h>

#define MAX_INPUT_DEVICES 8
#define MAX_HIDRAW_DEVICES 4
#define MAX_EVENTS_PER_FRAME 64

// NuPhy Air60 HE vendor ID
#define NUPHY_VENDOR_ID 0x19f5

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
    float pressure;     // 0.0-1.0 analog pressure (1.0 = full press, NuPhy HE)
} ACEvent;

// Per-key analog pressure state (for NuPhy HE)
#define MAX_ANALOG_KEYS 128
typedef struct {
    int active;          // Is this key currently pressed via analog?
    int releasing;       // Set when raw_pressure==0, fade out over frames
    float pressure;      // Current smoothed pressure 0.0-1.0
    float target;        // Last raw pressure target (NuPhy stops sending when stable)
    float raw_accum;     // Accumulated raw pressure this frame (for averaging)
    int raw_count;       // Number of HID reports this frame
    int key_code;        // Linux keycode equivalent
} ACAnalogKey;

typedef struct {
    int fds[MAX_INPUT_DEVICES];
    int fd_is_analog[MAX_INPUT_DEVICES]; // Set if this evdev belongs to an analog keyboard
    int count;
    int pointer_x, pointer_y;
    int pointer_down;
    int delta_x, delta_y;  // Per-frame pointer delta

    // HID raw devices (for analog keyboards like NuPhy HE)
    int hidraw_fds[MAX_HIDRAW_DEVICES];
    int hidraw_count;

    // Per-key analog pressure tracking
    ACAnalogKey analog_keys[MAX_ANALOG_KEYS];
    int has_analog;  // Set if an analog keyboard is detected
    int hotplug_counter;  // Frames since last hidraw scan
    int evdev_rescan_done;   // Set after late evdev rescan
    int evdev_rescan_counter; // Frame counter for late rescan

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
