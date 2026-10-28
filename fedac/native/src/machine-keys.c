// machine-keys.c — the per-machine top-row layout table (see machine-keys.h).

#include "machine-keys.h"

#include <stdio.h>
#include <string.h>

extern void ac_log(const char *fmt, ...);

static const MachineKeys layouts[] = {
    // The generic fallback (ThinkPads and most PCs send real media keys;
    // these F-keys only catch boards that don't).
    { "generic",  "f1",  "f2",  "f3",  "f5", "f6", 0 },
    // The MiniBook X keeps media keys behind Fn and its Fn+F6/F7 brightness
    // never reaches the OS, so plain F-keys do both. docs/machines/minibook-x.md
    { "minibook", "f2",  "f3",  "f4",  "f6", "f7", 1 },
    // Apple Silicon (the MacBook Neo): the top row arrives as plain F-keys in
    // Apple's printed order — F1/F2 brightness, F10 mute, F11/F12 volume.
    { "apple",    "f10", "f11", "f12", "f1", "f2", 1 },
};

// Reads a sysfs text file; devicetree strings are NUL-separated, so NULs
// become spaces.
static void read_text(const char *path, char *out, size_t cap) {
    out[0] = 0;
    FILE *f = fopen(path, "r");
    if (!f) return;
    size_t n = fread(out, 1, cap - 1, f);
    fclose(f);
    for (size_t k = 0; k < n; k++) if (!out[k] || out[k] == '\n') out[k] = ' ';
    while (n > 0 && out[n - 1] == ' ') n--;
    out[n] = 0;
}

const MachineKeys *machine_keys(void) {
    static const MachineKeys *picked;
    if (picked) return picked;
    char product[64], compat[256];
    read_text("/sys/class/dmi/id/product_name", product, sizeof product);
    read_text("/sys/firmware/devicetree/base/compatible", compat, sizeof compat);
    picked = &layouts[0];
    if (strcmp(product, "MiniBook X") == 0) picked = &layouts[1];
    else if (strstr(compat, "apple,")) picked = &layouts[2];
    if (picked != &layouts[0])
        ac_log("[machine] %s keys: %s mute, %s/%s volume, %s/%s brightness\n",
               picked->name, picked->mute, picked->voldn, picked->volup,
               picked->brdn, picked->brup);
    return picked;
}

const char *machine_model(void) {
    static char model[96];
    if (model[0]) return model;
    read_text("/sys/class/dmi/id/product_name", model, sizeof model);
    if (!model[0]) read_text("/sys/firmware/devicetree/base/model", model, sizeof model);
    if (!model[0]) strcpy(model, "unknown");
    return model;
}
