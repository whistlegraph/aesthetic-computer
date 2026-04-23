// demo_script.h — parser for AC Native demo scripts.
// See scripts/DEMO_SCRIPT_FORMAT.md for the format.
//
// A demo script is a single Markdown file with YAML-ish front-matter
// (# key: value lines starting with '#') followed by timestamped
// events of the form:
//
//   [M:SS.mmm] KIND ARG
//
// where KIND is one of: key | say | caption | wait | env.
// Absolute timestamps in seconds-from-start. Everything the parser
// returns is owned by the caller and freed via demo_script_free().
#ifndef AC_DEMO_SCRIPT_H
#define AC_DEMO_SCRIPT_H

#include <stddef.h>

typedef enum {
    DEMO_EV_KEY,
    DEMO_EV_SAY,
    DEMO_EV_CAPTION,   // empty arg → clear current caption
    DEMO_EV_WAIT,      // no-op; just an anchor
    DEMO_EV_ENV,       // arg = "NAME=value"
} DemoEventKind;

typedef struct {
    double        t;        // seconds from start
    DemoEventKind kind;
    char         *arg;      // malloc'd; NULL for WAIT
} DemoEvent;

typedef struct {
    char  *title;
    char  *voice;           // default "Samantha"
    int    rate;            // default 160 (wpm for `say`)
    char  *handle;
    char  *city;
    int    hour;
    int    win_w, win_h;
    double duration_sec;    // 0 = auto (inferred from last event + 2s)
    int    subtitles;
    int    narration;

    DemoEvent *events;
    int        n_events;
    int        cap_events;
} DemoScript;

// Parse a demo script from disk. Returns NULL on error (prints reason
// to stderr). Caller must demo_script_free().
DemoScript *demo_script_load(const char *path);

void demo_script_free(DemoScript *s);

// Return total demo duration — max(duration_sec, last_event_t + 2).
double demo_script_duration(const DemoScript *s);

#endif
