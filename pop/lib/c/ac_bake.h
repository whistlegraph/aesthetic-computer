// ac_bake.h — what a baked-score engine needs and nothing else.
//
// A lane's JS renderer is the composition. Run with --bake it writes every
// resolved event, in the order it played them, to a text score; the lane's C
// engine reads that score and makes the same samples. This header is the
// part every such engine shares:
//
//   - the arithmetic that has to agree with JS (round, float32 accumulation,
//     the xorshift noise, equal-power pan)
//   - a line tokenizer for the score
//   - a sample table: `smp <id> <path>` lines name raw f32 files the events
//     then refer to by id
//   - a WAV reader that walks chunks the way the JS one does
//
// Header-only, libm only. Build the engine with -ffp-contract=off: a fused
// multiply-add rounds once where JS rounds twice, and the two renders drift.

#ifndef AC_BAKE_H
#define AC_BAKE_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#define AC_TAU (2.0 * M_PI)

static const char *ac_engine = "engine";
static void ac_die(const char *what, const char *detail) {
    fprintf(stderr, "%s: %s%s%s\n", ac_engine, what, detail ? " " : "", detail ? detail : "");
    exit(1);
}

// ── arithmetic that matches JS ──────────────────────────────────────────
// Math.round: halves go up, not away from zero.
static inline long ac_round(double x) { return (long)floor(x + 0.5); }
static inline double ac_mtof(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }
static inline double ac_pan_l(double p) { return cos((p + 1.0) * M_PI / 4.0); }
static inline double ac_pan_r(double p) { return sin((p + 1.0) * M_PI / 4.0); }
static inline double ac_min3(double a, double b, double c) { return fmin(a, fmin(b, c)); }

// A Float32Array bus: the sum is taken in double and stored as float.
// Writes before the start of the bus are dropped, as a typed array drops them.
static inline void ac_add(float *bus, long i, double v) {
    if (i >= 0) bus[i] = (float)((double)bus[i] + v);
}

// The renderers' noise: xorshift32, read as [-1, 1).
typedef struct { uint32_t s; } AcNoise;
static inline uint32_t ac_xorshift(AcNoise *n) {
    n->s ^= n->s << 13; n->s ^= n->s >> 17; n->s ^= n->s << 5;
    return n->s;
}
static inline double ac_noise(AcNoise *n) { return ac_xorshift(n) / 2147483648.0 - 1.0; }

static void *ac_alloc(size_t count, size_t size) {
    void *p = calloc(count ? count : 1, size);
    if (!p) ac_die("out of memory", NULL);
    return p;
}

// ── files ───────────────────────────────────────────────────────────────
static uint8_t *ac_slurp(const char *path, long *bytes) {
    FILE *f = fopen(path, "rb");
    if (!f) ac_die("cannot open", path);
    fseek(f, 0, SEEK_END);
    long n = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = ac_alloc((size_t)n + 1, 1);
    if (fread(buf, 1, (size_t)n, f) != (size_t)n) ac_die("short read", path);
    fclose(f);
    *bytes = n;
    return buf;
}

// Raw little-endian f32, any channel layout — the caller knows which.
static float *ac_read_f32(const char *path, long *samples) {
    long bytes;
    float *d = (float *)ac_slurp(path, &bytes);
    *samples = bytes / 4;
    return d;
}

static inline uint32_t ac_u32(const uint8_t *p) { return p[0] | p[1] << 8 | p[2] << 16 | (uint32_t)p[3] << 24; }
static inline uint16_t ac_u16(const uint8_t *p) { return (uint16_t)(p[0] | p[1] << 8); }

// A WAV as two channels (mono is copied to both). Float when the format tag
// says float, 16-bit otherwise — the same two cases the JS reader knows.
// (Not every engine reads a WAV; the attribute keeps -Wextra quiet for those.)
__attribute__((unused)) static void ac_read_wav(const char *path, float **L, float **R, long *frames) {
    long len;
    uint8_t *buf = ac_slurp(path, &len);
    long i = 12;
    int ch = 2, bits = 32, fmt = 3;
    while (i < len - 8) {
        uint32_t size = ac_u32(buf + i + 4);
        long body = i + 8;
        if (!memcmp(buf + i, "fmt ", 4)) {
            fmt = ac_u16(buf + body); ch = ac_u16(buf + body + 2); bits = ac_u16(buf + body + 14);
        } else if (!memcmp(buf + i, "data", 4)) {
            long avail = len - body, take = (long)size < avail ? (long)size : avail;
            long stride = ch * bits / 8, n = take / stride;
            float *l = ac_alloc((size_t)n, sizeof(float)), *r = ac_alloc((size_t)n, sizeof(float));
            for (long f = 0; f < n; f++) {
                const uint8_t *o = buf + body + f * stride;
                for (int k = 0; k < 2; k++) {
                    float v;
                    if (k == 1 && ch < 2) { r[f] = l[f]; break; }
                    if (fmt == 3) memcpy(&v, o + k * 4, 4);
                    else v = (float)((int16_t)ac_u16(o + k * 2) / 32768.0);
                    (k ? r : l)[f] = v;
                }
            }
            free(buf);
            *L = l; *R = r; *frames = n;
            return;
        }
        i = body + size + (size & 1);
    }
    ac_die("no data chunk in", path);
}

// ── the score ───────────────────────────────────────────────────────────
// Split a line on spaces in place. Returns the token count.
static int ac_split(char *line, char **tok, int max) {
    int n = 0;
    for (char *p = strtok(line, " \t\r\n"); p && n < max; p = strtok(NULL, " \t\r\n")) tok[n++] = p;
    return n;
}
static inline double ac_num(const char *s) { return strtod(s, NULL); }

// ── samples ─────────────────────────────────────────────────────────────
typedef struct { float *d; long n; } AcSmp;
#define AC_SMP_MAX 4096
static AcSmp ac_smp[AC_SMP_MAX];

static void ac_smp_load(int id, const char *path) {
    if (id < 0 || id >= AC_SMP_MAX) ac_die("sample id out of range for", path);
    ac_smp[id].d = ac_read_f32(path, &ac_smp[id].n);
}
static AcSmp *ac_smp_get(const char *id) {
    int i = atoi(id);
    if (i < 0 || i >= AC_SMP_MAX || !ac_smp[i].d) ac_die("event names a sample the score never loaded:", id);
    return &ac_smp[i];
}

static void ac_write_f32(const char *path, const float *data, long samples) {
    FILE *f = fopen(path, "wb");
    if (!f) ac_die("cannot write", path);
    if (fwrite(data, sizeof(float), (size_t)samples, f) != (size_t)samples) ac_die("short write", path);
    fclose(f);
}

#endif
