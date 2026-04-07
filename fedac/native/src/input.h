#ifndef AC_INPUT_H
#define AC_INPUT_H

#include <linux/input.h>
#ifdef USE_WAYLAND
#include <wayland-client.h>
#endif

#define MAX_INPUT_DEVICES 24
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
    int releasing;       // Set when raw_pressure==0, release decay active
    float pressure;      // Current smoothed pressure 0.0-1.0
    float target;        // Last raw pressure target (NuPhy stops sending when stable)
    float raw_accum;     // Accumulated raw pressure this poll (for averaging)
    int raw_count;       // Number of HID reports this poll cycle
    int key_code;        // Linux keycode equivalent
} ACAnalogKey;

typedef struct {
    int fds[MAX_INPUT_DEVICES];
    int fd_is_analog[MAX_INPUT_DEVICES]; // Set if this evdev belongs to an analog keyboard
    int count;
    int pointer_x, pointer_y;
    int pointer_down;
    int delta_x, delta_y;  // Per-frame pointer delta

    // Trackpad absolute→relative conversion (BCM5974 etc.)
    int abs_prev_x, abs_prev_y;  // Previous absolute position (INT_MIN = not set)
    int abs_x_min, abs_x_max;    // Axis ranges from EVIOCGABS
    int abs_y_min, abs_y_max;
    int abs_x_res, abs_y_res;    // Resolution (units/mm), 0 = unknown
    int fd_is_trackpad[MAX_INPUT_DEVICES]; // This evdev is an abs trackpad

    // HID raw devices (for analog keyboards like NuPhy HE)
    int hidraw_fds[MAX_HIDRAW_DEVICES];
    int hidraw_count;

    // Per-key analog pressure tracking
    ACAnalogKey analog_keys[MAX_ANALOG_KEYS];
    int has_analog;  // Set if an analog keyboard is detected
    int hotplug_counter;  // Polls since last hidraw scan
    double last_poll_time; // Monotonic time of last poll (seconds)
    int evdev_rescan_done;   // Set after late evdev rescan
    int evdev_rescan_counter; // Frame counter for late rescan

    // Event queue for current frame
    ACEvent events[MAX_EVENTS_PER_FRAME];
    int event_count;

    // Screen dimensions for touch scaling
    int screen_w, screen_h;
    int scale;  // Display-to-piece scale factor (e.g. 3)

    // Tablet mode (lid folded back on convertible laptops)
    int tablet_mode;  // 0 = laptop, 1 = tablet (from EV_SW or sysfs)

#ifdef USE_WAYLAND
    // Wayland input state
    int is_wayland;           // 1 if using Wayland input instead of evdev
    void *wayland_display;    // ACWaylandDisplay* — for event dispatch
    // Key repeat state (Wayland doesn't send EV_KEY value=2)
    int repeat_rate;          // keys per second (from compositor)
    int repeat_delay;         // ms before repeat starts
    int repeat_key;           // currently repeating key (-1 if none)
    double repeat_next;       // monotonic time of next repeat event
    double repeat_start;      // when repeat was armed
#endif
} ACInput;

ACInput *input_init(int screen_w, int screen_h, int scale);
void input_poll(ACInput *input);
void input_destroy(ACInput *input);

#ifdef USE_WAYLAND
// Initialize Wayland input — binds keyboard/pointer/touch from seat
// Call after wayland_display_init() returns successfully
struct ACWaylandDisplay;
ACInput *input_init_wayland(void *wayland_display, int screen_w, int screen_h, int scale);
#endif

// Map Linux keycode to AC key name
const char *input_key_name(int code);

#endif
