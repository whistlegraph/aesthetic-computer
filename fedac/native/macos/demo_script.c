// demo_script.c — parser for AC Native demo scripts.
#include "demo_script.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

static char *xstrdup(const char *s) {
    if (!s) return NULL;
    size_t n = strlen(s);
    char *out = (char *)malloc(n + 1);
    if (!out) return NULL;
    memcpy(out, s, n + 1);
    return out;
}

// Trim leading + trailing whitespace in-place, returning a pointer into
// the buffer (which may be the original start or shifted forward).
static char *trim(char *s) {
    while (*s && isspace((unsigned char)*s)) s++;
    char *end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) { *--end = 0; }
    return s;
}

// Parse "[M:SS(.mmm)?]" at the start of line. Returns seconds and writes
// the tail pointer to *rest (past the closing bracket + whitespace).
// Returns -1 on parse failure.
static double parse_timestamp(const char *line, const char **rest) {
    if (*line != '[') return -1;
    line++;
    char *endp;
    long mm = strtol(line, &endp, 10);
    if (endp == line || *endp != ':') return -1;
    line = endp + 1;
    double ss = strtod(line, &endp);
    if (endp == line) return -1;
    line = endp;
    if (*line != ']') return -1;
    line++;
    while (*line && isspace((unsigned char)*line)) line++;
    if (rest) *rest = line;
    return (double)mm * 60.0 + ss;
}

static void push_event(DemoScript *s, DemoEvent ev) {
    if (s->n_events == s->cap_events) {
        int ncap = s->cap_events ? s->cap_events * 2 : 32;
        DemoEvent *ne = (DemoEvent *)realloc(s->events, ncap * sizeof(DemoEvent));
        if (!ne) { free(ev.arg); return; }
        s->events = ne;
        s->cap_events = ncap;
    }
    s->events[s->n_events++] = ev;
}

// Front-matter line: "# key: value" — consumed only while no non-comment
// event has appeared yet. Returns 1 if handled, 0 otherwise.
static int try_front_matter(DemoScript *s, const char *line) {
    if (line[0] != '#') return 0;
    // Skip the leading '#' + whitespace; find ':' to decide if it's a kv.
    const char *p = line + 1;
    while (*p && isspace((unsigned char)*p)) p++;
    const char *colon = strchr(p, ':');
    if (!colon) return 1;   // plain comment, consume but do nothing
    char key[64];
    size_t klen = (size_t)(colon - p);
    if (klen >= sizeof(key)) return 1;
    memcpy(key, p, klen);
    key[klen] = 0;
    char *tk = trim(key);
    const char *val_start = colon + 1;
    while (*val_start && isspace((unsigned char)*val_start)) val_start++;
    // Strip trailing newline.
    char val[512];
    strncpy(val, val_start, sizeof(val) - 1);
    val[sizeof(val) - 1] = 0;
    char *tv = trim(val);

    if      (!strcmp(tk, "title"))      { free(s->title);  s->title  = xstrdup(tv); }
    else if (!strcmp(tk, "voice"))      { free(s->voice);  s->voice  = xstrdup(tv); }
    else if (!strcmp(tk, "rate"))       s->rate      = atoi(tv);
    else if (!strcmp(tk, "handle"))     { free(s->handle); s->handle = xstrdup(tv); }
    else if (!strcmp(tk, "city"))       { free(s->city);   s->city   = xstrdup(tv); }
    else if (!strcmp(tk, "hour"))       s->hour      = atoi(tv);
    else if (!strcmp(tk, "duration"))   s->duration_sec = atof(tv);
    else if (!strcmp(tk, "subtitles"))  s->subtitles = (!strcmp(tv, "true") || atoi(tv));
    else if (!strcmp(tk, "narration"))  s->narration = (!strcmp(tv, "true") || atoi(tv));
    else if (!strcmp(tk, "window")) {
        int w = 0, h = 0;
        if (sscanf(tv, "%dx%d", &w, &h) == 2) { s->win_w = w; s->win_h = h; }
    }
    return 1;
}

static DemoEventKind kind_from_token(const char *tok) {
    if (!strcmp(tok, "key"))     return DEMO_EV_KEY;
    if (!strcmp(tok, "say"))     return DEMO_EV_SAY;
    if (!strcmp(tok, "caption")) return DEMO_EV_CAPTION;
    if (!strcmp(tok, "wait"))    return DEMO_EV_WAIT;
    if (!strcmp(tok, "env"))     return DEMO_EV_ENV;
    return -1;
}

DemoScript *demo_script_load(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "[demo] cannot read %s: %s\n", path, strerror(errno));
        return NULL;
    }
    DemoScript *s = (DemoScript *)calloc(1, sizeof(DemoScript));
    if (!s) { fclose(f); return NULL; }
    // Defaults
    s->voice = xstrdup("Samantha");
    s->rate = 160;
    s->hour = 13;
    s->win_w = 1280;
    s->win_h = 800;
    s->subtitles = 1;
    s->narration = 1;

    char buf[2048];
    int saw_event = 0;
    while (fgets(buf, sizeof(buf), f)) {
        // Strip trailing newline/\r.
        size_t L = strlen(buf);
        while (L > 0 && (buf[L-1] == '\n' || buf[L-1] == '\r')) buf[--L] = 0;
        char *line = trim(buf);
        if (!*line) continue;

        // Front-matter (# key: value) only valid before the first event.
        if (!saw_event && line[0] == '#') {
            try_front_matter(s, line);
            continue;
        }
        // Comment after events start — skip.
        if (line[0] == '#') continue;

        if (line[0] != '[') continue;   // ignore stray lines
        const char *after_ts = NULL;
        double t = parse_timestamp(line, &after_ts);
        if (t < 0) {
            fprintf(stderr, "[demo] skip bad line: %s\n", line);
            continue;
        }
        saw_event = 1;

        // Next token is the KIND.
        const char *p = after_ts;
        while (*p && isspace((unsigned char)*p)) p++;
        const char *kstart = p;
        while (*p && !isspace((unsigned char)*p)) p++;
        size_t klen = (size_t)(p - kstart);
        char kind_tok[32];
        if (klen == 0 || klen >= sizeof(kind_tok)) continue;
        memcpy(kind_tok, kstart, klen);
        kind_tok[klen] = 0;
        while (*p && isspace((unsigned char)*p)) p++;

        DemoEventKind k = kind_from_token(kind_tok);
        if ((int)k < 0) {
            fprintf(stderr, "[demo] unknown kind '%s'\n", kind_tok);
            continue;
        }
        DemoEvent ev = { .t = t, .kind = k, .arg = NULL };
        if (k != DEMO_EV_WAIT && *p) ev.arg = xstrdup(p);
        push_event(s, ev);
    }
    fclose(f);

    // Sort events by t (simple insertion sort; demo lists are small).
    for (int i = 1; i < s->n_events; i++) {
        DemoEvent cur = s->events[i];
        int j = i - 1;
        while (j >= 0 && s->events[j].t > cur.t) {
            s->events[j+1] = s->events[j];
            j--;
        }
        s->events[j+1] = cur;
    }
    return s;
}

void demo_script_free(DemoScript *s) {
    if (!s) return;
    free(s->title); free(s->voice); free(s->handle); free(s->city);
    for (int i = 0; i < s->n_events; i++) free(s->events[i].arg);
    free(s->events);
    free(s);
}

double demo_script_duration(const DemoScript *s) {
    if (!s || s->n_events == 0) return s ? s->duration_sec : 0;
    double last = s->events[s->n_events - 1].t;
    double inferred = last + 2.0;
    return (s->duration_sec > inferred) ? s->duration_sec : inferred;
}
