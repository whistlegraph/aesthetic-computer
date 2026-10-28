#ifndef AC_MACHINE_KEYS_H
#define AC_MACHINE_KEYS_H

// The built-in keyboard's top row, per machine: which evdev key names do
// mute / volume / brightness. One static row per laptop, picked at boot.
typedef struct {
    const char *name;   // "generic" | "minibook" | "apple" (= system.hw.keyboard)
    const char *mute, *voldn, *volup, *brdn, *brup;
    int consume_row;    // stop these at the OS so notepat's DJ F-keys don't fire too
} MachineKeys;

// Detect from DMI product_name, then the devicetree (Apple Silicon has no DMI).
// Logs the choice once; later calls return the same row.
const MachineKeys *machine_keys(void);

// The machine's model: DMI product_name, else the devicetree model
// ("Apple MacBook Neo (DT)"), else "unknown". Static storage.
const char *machine_model(void);

#endif
