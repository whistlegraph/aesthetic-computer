// americomputadora.c — C engine for the americomputadora track, following
// the hellsine pattern (pop/hellsine/c): JS bakes vocals + score.h, this
// renders the full song to a float32 WAV; acdsp masters it; ffmpeg only
// encodes mp3 at the very end.
//
// Faithful port of bin/render.mjs (rock-ballad / ramp era, 2026-06-09):
// phase-increment synths, layered-merge vocal envelopes with the 0.3 s
// america↔computer blend, percussion intensity ramp, sidechain duck,
// dotted-8th vocal echo, Schroeder reverb, tanh master.
//
// Build:  ./build.sh         Run:  ./americomputadora --out out/c.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

#include "score.h"

static const int SR = 48000;
static double BEAT, BAR;

// ── deterministic RNG (xorshift32) — noise voices stay reproducible ────
static uint32_t rng_s = 0x41435f5f; // "AC__"
static inline double rng(void) {
    rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5;
    return (double)rng_s / 4294967296.0;
}
static inline double rnd2(void) { return rng() * 2.0 - 1.0; } // -1..1

// ── humanize: deterministic per-hit timing + velocity jitter so the grid
// breathes instead of clicking like a sequencer. transients get a few ms of
// push/pull and a little velocity spread; sustained beds (sub/pad/choir)
// stay tight to avoid phasing.
static inline double hum_t(double t, double ms)   { return t + rnd2() * ms * 0.001; }
static inline double hum_g(double g, double frac) { return g * (1.0 + rnd2() * frac); }

static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── buffers ─────────────────────────────────────────────────────────────
static long N;
static float *drm, *kik, *snr, *bel, *sqr, *sb, *toy, *arp, *pds, *cho, *voc;

static inline void addb(float *buf, long i, double v) {
    if (i >= 0 && i < N) buf[i] += (float)v;
}

// ── wav io ──────────────────────────────────────────────────────────────
// reads mono PCM16 or float32 wav (the baked vocals are float32 48k mono).
static float *load_wav_mono(const char *path, long *out_n) {
    FILE *f = fopen(path, "rb");
    if (!f) { fprintf(stderr, "  ! missing %s\n", path); return NULL; }
    uint8_t hdr[12];
    if (fread(hdr, 1, 12, f) != 12 || memcmp(hdr, "RIFF", 4) || memcmp(hdr + 8, "WAVE", 4)) {
        fclose(f); return NULL;
    }
    uint16_t fmt = 0, ch = 1, bits = 16;
    float *out = NULL; long n = 0;
    for (;;) {
        uint8_t ck[8];
        if (fread(ck, 1, 8, f) != 8) break;
        uint32_t sz = ck[4] | (ck[5] << 8) | (ck[6] << 16) | ((uint32_t)ck[7] << 24);
        if (!memcmp(ck, "fmt ", 4)) {
            uint8_t b[16];
            fread(b, 1, 16, f);
            fmt = b[0] | (b[1] << 8); ch = b[2] | (b[3] << 8); bits = b[14] | (b[15] << 8);
            if (sz > 16) fseek(f, sz - 16, SEEK_CUR);
        } else if (!memcmp(ck, "data", 4)) {
            long frames = sz / (bits / 8) / ch;
            out = malloc(sizeof(float) * frames);
            n = frames;
            for (long i = 0; i < frames; i++) {
                double acc = 0;
                for (int c = 0; c < ch; c++) {
                    if (fmt == 3 && bits == 32) {
                        float v; fread(&v, 4, 1, f); acc += v;
                    } else {
                        int16_t v; fread(&v, 2, 1, f); acc += v / 32768.0;
                    }
                }
                out[i] = (float)(acc / ch);
            }
            break;
        } else {
            fseek(f, sz + (sz & 1), SEEK_CUR);
        }
    }
    fclose(f);
    *out_n = n;
    return out;
}

static int write_wav_f32(const char *path, const float *s, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 4), riff = 36 + dsz, sr = SR, br = SR * 4;
    uint16_t fmt = 3, ch = 1, ba = 4, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); uint32_t fsz = 16; fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    fwrite(s, 4, n, f);
    fclose(f);
    return 1;
}

// ── vocal cache: load each baked wav once. three treatment tiers ramp
// with the song: 0 = raw stitch, 1 = autotuned, 2 = full character.
typedef struct { float *s; long n; } Clip;
static Clip clips[3][N_WORDS][MAX_ROSTER][N_VARIANTS];
static const char *voc_base = "";

static Clip syl_cache[2][N_SYL ? N_SYL : 1][N_SYLT];
static Clip *get_syl(int voice, int si, int ti) {
#if N_SYL > 0
    Clip *c = &syl_cache[voice][si][ti];
    if (!c->s) {
        char path[512];
        const char *rel = voice ? SYL_PATHS_C[si][ti] : SYL_PATHS[si][ti];
        snprintf(path, sizeof path, "%s%s", voc_base, rel);
        c->s = load_wav_mono(path, &c->n);
    }
    return c->s ? c : NULL;
#else
    (void)voice; (void)si; (void)ti; return NULL;
#endif
}

static Clip *get_clip(int tier, int w, int r, int v) {
    if (tier == 0) v = 0; // raw tier is variant-independent
    Clip *c = &clips[tier][w][r][v];
    if (!c->s) {
        const char *rel = tier == 0 ? VOC_RAW[w][r]
                       : tier == 1 ? VOC_PLAIN[w][r][v]
                       : VOC_FULL[w][r][v];
        char path[512];
        snprintf(path, sizeof path, "%s%s", voc_base, rel);
        c->s = load_wav_mono(path, &c->n);
    }
    return c->s ? c : NULL;
}

// ── synth voices (ports of render.mjs, same constants) ─────────────────
static void kick(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize
    int n = (int)(0.18 * SR); long s0 = (long)(t * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = 55 + (140 - 55) * exp(-tt * 38);
        ph += TAU * f / SR;
        double body = sin(ph) * exp(-tt * 11);
        double click = i < SR * 0.002 ? rnd2() * 0.4 * (1 - i / (SR * 0.002)) : 0;
        addb(buf, s0 + i, (body + click) * g);
    }
}

static void kick_serious(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize
    int n = (int)(0.32 * SR); long s0 = (long)(t * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = 42 + (190 - 42) * exp(-tt * 30);
        ph += TAU * f / SR;
        double body = tanh(sin(ph) * 2.4) * exp(-tt * 7.5);
        double click = i < SR * 0.003 ? rnd2() * 0.6 * (1 - i / (SR * 0.003)) : 0;
        addb(buf, s0 + i, (body + click) * g);
    }
}

static void snare_hit(float *buf, double t, double g) {
    t = hum_t(t, 9); g = hum_g(g, 0.12); // humanize
    int n = (int)(0.22 * SR); long s0 = (long)(t * SR);
    double prev = 0, ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        ph += TAU * 190 / SR;
        double body = sin(ph) * exp(-tt * 30) * 0.7;
        addb(buf, s0 + i, (hp * exp(-tt * 24) + body) * g);
    }
}

static void clap(float *buf, double t, double g) {
    t = hum_t(t, 10); g = hum_g(g, 0.12); // humanize
    for (int b = 0; b < 4; b++) {
        double bt = t + b * 0.007;
        long s0 = (long)(bt * SR); int n = (int)(0.012 * SR);
        double prev = 0;
        for (int i = 0; i < n; i++) {
            double nz = rnd2();
            double hp = nz - prev; prev = nz;
            addb(buf, s0 + i, hp * exp(-((double)i / SR) * 120) * g * (b == 3 ? 1.0 : 0.55));
        }
    }
    long s0 = (long)(t * SR); int tail = (int)(0.13 * SR);
    double prev = 0;
    for (int i = 0; i < tail; i++) {
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        addb(buf, s0 + i, hp * exp(-((double)i / SR) * 22) * g * 0.18);
    }
}

static void tamb(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.18); // humanize
    int n = (int)(0.05 * SR); long s0 = (long)(t * SR);
    double prev = 0;
    for (int i = 0; i < n; i++) {
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        addb(buf, s0 + i, hp * exp(-((double)i / SR) * 80) * g);
    }
}

static void crash(float *buf, double t, double g) {
    int n = (int)(1.4 * SR); long s0 = (long)(t * SR);
    double prev = 0;
    for (int i = 0; i < n; i++) {
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        addb(buf, s0 + i, hp * exp(-((double)i / SR) * 3.2) * g);
    }
}

static void bell(float *buf, double t, double midi, double dur, double g) {
    t = hum_t(t, 8); g = hum_g(g, 0.08); // humanize
    double fc = midi_hz(midi), fm = fc * 3.5;
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    double phc = 0, phm = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double env = exp(-tt * 5.5);
        double mi = 4.5 * exp(-tt * 9);
        phm += TAU * fm / SR;
        phc += TAU * fc / SR + sin(phm) * mi / SR;
        addb(buf, s0 + i, sin(phc) * env * g);
    }
}

static void sine_lead(float *buf, double t, double midi, double dur, double g) {
    double f = midi_hz(midi);
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    int att = (int)(0.05 * SR), rel = (int)(0.12 * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double vib = tt > 0.25 ? sin(TAU * 5.2 * tt) * 0.004 * fmin(1, (tt - 0.25) * 3) : 0;
        ph += TAU * f * (1 + vib) / SR;
        double env = 1;
        if (i < att) env = (double)i / att;
        else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        addb(buf, s0 + i, sin(ph) * env * g);
    }
}

static void sub_bass(float *buf, double t, double midi, double dur, double g) {
    double f = midi_hz(midi - 12);
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    int att = (int)(0.012 * SR), rel = (int)(0.04 * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = 1;
        if (i < att) env = (double)i / att;
        else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        addb(buf, s0 + i, tanh(sin(ph) * 1.4) * env * g);
    }
}

static void toy_piano(float *buf, double t, double midi, double dur, double g) {
    t = hum_t(t, 9); g = hum_g(g, 0.12); // humanize — slight strum across chord notes
    double f = midi_hz(midi);
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    double p1 = 0, p2 = 0, p3 = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        p1 += TAU * f / SR;
        p2 += TAU * f * 2.01 / SR;
        p3 += TAU * f * 3.04 / SR;
        double env = exp(-tt * 4.5);
        addb(buf, s0 + i, (sin(p1) * 0.55 + sin(p2) * 0.30 + sin(p3) * 0.18) * env * g);
    }
}

static void pluck(float *buf, double t, double midi, double dur, double g) {
    t = hum_t(t, 8); g = hum_g(g, 0.10); // humanize
    double f = midi_hz(midi);
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    double ph = 0, lp = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += TAU * f / SR;
        double saw = 2.0 * fmod(ph / TAU, 1.0) - 1.0;
        double raw = saw * 0.55 + sin(ph) * 0.45;
        double a = 0.12 + 0.55 * exp(-tt * 14);
        lp += a * (raw - lp);
        addb(buf, s0 + i, lp * exp(-tt * 7) * g);
    }
}

static void bach_arp(float *buf, double bar_start, int root, int minor,
                     int density, int octave, double g) {
    int T = minor ? 3 : 4;
    int s16[16] = {0, T, 7, 12, 7, T, 0, T, 7, 12, 14, 12, 7, T, 7, 12};
    int s8[8]   = {0, 7, T, 12, 14, 12, 7, T};
    int *steps = density == 16 ? s16 : s8;
    int ns = density == 16 ? 16 : 8;
    double step_len = BAR / ns;
    for (int i = 0; i < ns; i++) {
        pluck(buf, bar_start + i * step_len, root + octave + steps[i],
              step_len * 1.6, g * (i % 4 == 0 ? 1.15 : 0.85));
    }
}

// choir — soft sustained harmony: detuned sines on the chord tones with
// slow vibrato. always present, every bar of the song, no matter what.
static void choir(float *buf, double t, int root, int third, int fifth, double g) {
    int tones[3] = {root + 12, third + 12, fifth + 12};
    int n = (int)(BAR * 1.04 * SR); long s0 = (long)(t * SR);
    int att = (int)(0.30 * SR), rel = (int)(0.25 * SR);
    for (int m = 0; m < 3; m++) {
        double f0 = midi_hz(tones[m]);
        for (int d = 0; d < 2; d++) {
            double f = f0 * (1 + (d ? 0.004 : -0.004));
            double ph = rng() * TAU;
            for (int i = 0; i < n; i++) {
                double tt = (double)i / SR;
                double vib = sin(TAU * 4.6 * tt + m) * 0.003;
                ph += TAU * f * (1 + vib) / SR;
                double env = 1;
                if (i < att) env = (double)i / att;
                else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
                addb(buf, s0 + i, sin(ph) * env * g);
            }
        }
    }
}

static void pad(float *buf, double t, const int *midis, int nm, double dur, double g) {
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    int att = (int)(0.06 * SR), rel = (int)(0.12 * SR);
    static const double DET[3] = {-0.006, 0, 0.007};
    for (int m = 0; m < nm; m++) {
        double f0 = midi_hz(midis[m]);
        for (int d = 0; d < 3; d++) {
            double f = f0 * (1 + DET[d]);
            double ph = rng() * TAU, lp = 0;
            for (int i = 0; i < n; i++) {
                ph += TAU * f / SR;
                double saw = 2.0 * fmod(ph / TAU, 1.0) - 1.0;
                lp += 0.10 * (saw - lp);
                double env = 1;
                if (i < att) env = (double)i / att;
                else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
                addb(buf, s0 + i, lp * env * g);
            }
        }
    }
}

// ── paintShaped: piecewise gain envelope, the layered word merge ────────
typedef struct { double at, to, over; } Break;

static void paint_shaped(float *buf, const float *s, long sn, double start_sec,
                         double g, const Break *breaks, int nb, double attack) {
    long s0 = (long)(start_sec * SR);
    double level = attack > 0 ? 0 : 1;
    double from = level, ramp_start = 0, ramp_len = attack, target = 1;
    int bi = 0;
    for (long i = 0; i < sn; i++) {
        double tt = (double)i / SR;
        if (bi < nb && tt >= breaks[bi].at) {
            from = level; target = breaks[bi].to;
            ramp_start = tt;
            ramp_len = breaks[bi].over > 1e-4 ? breaks[bi].over : 1e-4;
            bi++;
        }
        if (level != target) {
            double w = (tt - ramp_start) / ramp_len;
            if (w > 1) w = 1;
            level = from + (target - from) * (0.5 - 0.5 * cos(M_PI * w));
            if (w >= 1) level = target;
        }
        if (level == 0 && bi >= nb) break;
        addb(buf, s0 + i, s[i] * g * level);
    }
}

// ── main ────────────────────────────────────────────────────────────────
typedef struct { double t; int serious; } KickT;
static KickT kick_times[4096];
static int n_kicks = 0;
static void push_kick(double t, int serious) {
    if (n_kicks < 4096) { kick_times[n_kicks].t = t; kick_times[n_kicks].serious = serious; n_kicks++; }
}

static const ChordDef *chord_at_bar(int bar_idx) {
    int b = bar_idx % PROG_BARS;
    for (int i = 0; i < N_PROG; i++) {
        if (b < PROG[i].bars) return &PROG[i];
        b -= PROG[i].bars;
    }
    return &PROG[0];
}

static int vocals_only = 0;
static int stem_voc = 0;

int main(int argc, char **argv) {
    const char *out_path = "out/americomputadora-c.wav";
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
        else if (!strcmp(argv[i], "--vocals") && i + 1 < argc) voc_base = argv[++i];
        else if (!strcmp(argv[i], "--vocals-only")) vocals_only = 1;
        else if (!strcmp(argv[i], "--stem-voc")) stem_voc = 1;
    }

    BEAT = 60.0 / SCORE_BPM;
    BAR = BEAT * 4;

    int total_bars = 0;
    for (int s = 0; s < N_SECS; s++) total_bars += STRUCTURE[s].bars * STRUCTURE[s].reps;
    double total_sec = total_bars * BAR + 0.5;
    N = (long)ceil(total_sec * SR);

    drm = calloc(N, 4); kik = calloc(N, 4); snr = calloc(N, 4); bel = calloc(N, 4);
    sqr = calloc(N, 4); sb  = calloc(N, 4); toy = calloc(N, 4);
    arp = calloc(N, 4); pds = calloc(N, 4); cho = calloc(N, 4); voc = calloc(N, 4);

    int total_hook_reps = 0;
    for (int s = 0; s < N_SECS; s++)
        if (!strcmp(STRUCTURE[s].section, "hook")) total_hook_reps += STRUCTURE[s].reps;

    fprintf(stderr, "# americomputadora.c · %g BPM · %d bars · %.1fs\n",
            (double)SCORE_BPM, total_bars, total_sec);

    int bar_cursor = 0, hook_count = 0, phrase_counter = 0;
    int verse_beat = 0;  // drives the verse-2 jeffrey syllable chant
    int intro_beat = 0;  // drives the quiet intro chant

    for (int si = 0; si < N_SECS; si++) {
        const Sec *sec = &STRUCTURE[si];
        for (int r = 0; r < sec->reps; r++) {
            for (int b = 0; b < sec->bars; b++) {
                double bt = (bar_cursor + b) * BAR;
                const ChordDef *ch = chord_at_bar(bar_cursor + b);
                int root = ch->root, third = root + (ch->minor ? 3 : 4), fifth = root + 7;

                // very faint harmony bed — every bar, no matter what
                choir(cho, bt, root, third, fifth, 0.022);

                if (strcmp(sec->feel, "claps-only") && strcmp(sec->feel, "fade")) {
                    double sg = (!strcmp(sec->feel, "thin") || !strcmp(sec->feel, "sparse")) ? 0.22 : 0.35;
                    if (!strcmp(sec->section, "intro")) sg = 0.10; // hushed under the hats
                    sub_bass(sb, bt, root, BAR * 0.95, sg);
                }

                if (!strcmp(sec->section, "stop") || !strcmp(sec->feel, "claps-only")) {
                    clap(drm, bt + BEAT, 1.05);
                    clap(drm, bt + BEAT * 3, 1.05);
                } else if (!strcmp(sec->section, "intro")) {
                    // the opener: soft hats + the toy-piano melody (the same
                    // every-other-beat figure from verse 2) + a very quiet,
                    // smooth jeffrey syllable chant. spare and pretty.
                    for (int i = 0; i < 8; i++) tamb(drm, bt + i * (BEAT / 2), 0.08);
                    // toy piano — fuller now: a soft chord on 1 and 3 (triad
                    // + inversion) under a stepping melody line on every beat.
                    toy_piano(toy, bt,            root,      BEAT * 1.9, 0.06);
                    toy_piano(toy, bt,            third,     BEAT * 1.9, 0.055);
                    toy_piano(toy, bt,            fifth,     BEAT * 1.9, 0.05);
                    toy_piano(toy, bt + BEAT * 2, fifth,     BEAT * 1.9, 0.055);
                    toy_piano(toy, bt + BEAT * 2, root + 12, BEAT * 1.9, 0.055);
                    toy_piano(toy, bt + BEAT * 2, third + 12, BEAT * 1.9, 0.045);
                    // melody line on top — gives it motion / complexity
                    toy_piano(toy, bt,            third + 12, BEAT * 0.85, 0.07);
                    toy_piano(toy, bt + BEAT,     fifth + 12, BEAT * 0.85, 0.06);
                    toy_piano(toy, bt + BEAT * 2, root + 24, BEAT * 0.85, 0.06);
                    toy_piano(toy, bt + BEAT * 3, fifth + 12, BEAT * 0.85, 0.06);
#if N_SYL > 0
                    // OPENER — human + machine in unison: jeffrey (ElevenLabs)
                    // and the computer voice chant the word TOGETHER from the
                    // very first note (america + computadora, said as one),
                    // then a harmony line (both voices, a third/fifth above)
                    // blooms in over the last two words as the intro lifts
                    // into the first hook.
                    {
                        const int loDeg[4] = {root, third, fifth, third};
                        const int hiDeg[4] = {third, fifth, root + 12, fifth};
                        for (int k = 0; k < 4; k++) {
                            long s0 = (long)((bt + k * BEAT) * SR);
                            long slot = (long)(BEAT * 1.7 * SR);
                            int af = (int)(0.05 * SR), rf = (int)(0.30 * SR);
                            // the unison carries the first 3 words; the harmony
                            // line eases in over the next 2.
                            double harmG = intro_beat < 3 * N_SYL ? 0.0
                                : fmin(1.0, (double)(intro_beat - 3 * N_SYL) / (2.0 * N_SYL));
                            int nLines = harmG > 0 ? 2 : 1;
                            int si = intro_beat % N_SYL;
                            for (int ln = 0; ln < nLines; ln++) {
                                int deg = ln == 0 ? loDeg[k] : hiDeg[k];
                                int ti = -1;
                                for (int q = 0; q < N_SYLT; q++) if (SYL_NOTES[q] == deg) { ti = q; break; }
                                if (ti < 0) continue;
                                double lineG = ln == 0 ? 1.0 : harmG;
                                // both voices in unison: jeffrey (0) + computer (1)
                                for (int vx = 0; vx < 2; vx++) {
                                    Clip *sc = get_syl(vx, si, ti);
                                    if (!sc) continue;
                                    long lim = sc->n < slot ? sc->n : slot;
                                    double g = (vx == 0 ? 0.50 : 0.52) * lineG;
                                    for (long i = 0; i < lim; i++) {
                                        double env = 1;
                                        if (i < af) env = (double)i / af;
                                        else if (i > lim - rf) env = (double)(lim - i) / rf;
                                        addb(voc, s0 + i, sc->s[i] * g * env);
                                    }
                                }
                            }
                            intro_beat++;
                        }
                    }
#endif
                } else if (!strcmp(sec->feel, "fade")) {
                    if (!(b & 1)) kick(kik, bt, 0.55);
                    for (int i = 0; i < 8; i++) tamb(drm, bt + i * (BEAT / 2), 0.05);
                } else if (!strcmp(sec->section, "verse")) {
                    // no kick under the vowel stitch: computer enters on the
                    // odd bar's downbeat, leave that one out.
                    if (!(b & 1)) { kick(kik, bt, 0.75); push_kick(bt, 0); }
                    kick(kik, bt + BEAT * 2, 0.75); push_kick(bt + BEAT * 2, 0);
                    snare_hit(snr, bt + BEAT, 0.5);
                    snare_hit(snr, bt + BEAT * 3, 0.5);
#if N_SYL > 0
                    // verse-2 chant (the sparse every-other-beat melody at
                    // ~1:40): one syllable per beat, pitched to the tones the
                    // melody walks (third → octave), on top of the normal verse
                    // vocal — same human + machine unison as the opener:
                    // jeffrey (ElevenLabs) + the computer voice together.
                    if (!strcmp(sec->feel, "sparse"))
                    for (int k = 0; k < 4; k++) {
                        int deg = k < 2 ? third : root + 12;
                        int ti = -1;
                        for (int q = 0; q < N_SYLT; q++) if (SYL_NOTES[q] == deg) { ti = q; break; }
                        if (ti >= 0) {
                            int si = verse_beat % N_SYL;
                            long s0 = (long)((bt + k * BEAT) * SR);
                            long slot = (long)(BEAT * 0.98 * SR);
                            for (int vx = 0; vx < 2; vx++) {
                                Clip *sc = get_syl(vx, si, ti); // jeffrey (0) + computer (1)
                                if (!sc) continue;
                                double g = vx == 0 ? 0.48 : 0.50;
                                long lim = sc->n < slot ? sc->n : slot;
                                for (long i = 0; i < lim; i++) {
                                    double env = i > lim - 480 ? (double)(lim - i) / 480 : 1.0;
                                    addb(voc, s0 + i, sc->s[i] * g * env);
                                }
                            }
                        }
                        verse_beat++;
                    }
#endif
                    if (!strcmp(sec->feel, "thin")) {
                        toy_piano(toy, bt,            third,     BEAT * 0.9, 0.16);
                        toy_piano(toy, bt + BEAT,     fifth,     BEAT * 0.9, 0.13);
                        toy_piano(toy, bt + BEAT * 2, root + 12, BEAT * 0.9, 0.13);
                        toy_piano(toy, bt + BEAT * 3, fifth,     BEAT * 0.9, 0.13);
                    } else {
                        toy_piano(toy, bt,            third,     BEAT * 1.5, 0.14);
                        toy_piano(toy, bt + BEAT * 2, root + 12, BEAT * 1.5, 0.14);
                    }
                } else if (!strcmp(sec->section, "bridge")) {
                    kick_serious(kik, bt, 0.9); push_kick(bt, 1);
                    bach_arp(arp, bt, root, ch->minor, 8, 0, 0.15);
                } else {
                    // hook — the percussion RAMP (see render.mjs)
                    double tins = total_hook_reps > 1
                        ? (double)hook_count / (total_hook_reps - 1) : 1;
                    if (tins < 0.34) {
                        if (!(b & 1)) { kick(kik, bt, 0.8); push_kick(bt, 0); }
                        kick(kik, bt + BEAT * 2, 0.8); push_kick(bt + BEAT * 2, 0);
                        snare_hit(snr, bt + BEAT, 0.55);
                        snare_hit(snr, bt + BEAT * 3, 0.55);
                    } else if (tins < 0.67) {
                        if (!(b & 1)) { kick_serious(kik, bt, 0.95); push_kick(bt, 1); }
                        kick_serious(kik, bt + BEAT * 2, 0.95); push_kick(bt + BEAT * 2, 1);
                        snare_hit(snr, bt + BEAT, 0.8);
                        snare_hit(snr, bt + BEAT * 3, 0.8);
                        for (int i = 0; i < 8; i++) tamb(drm, bt + i * (BEAT / 2), 0.04);
                    } else {
                        int floor4 = tins > 0.95;
                        for (int i = 0; i < 4; i++) {
                            if ((b & 1) && i == 0) continue; // vowel stitch
                            if (i % 2 == 0 || floor4) {
                                kick_serious(kik, bt + BEAT * i, floor4 ? 1.1 : 1.05);
                                push_kick(bt + BEAT * i, 1);
                            }
                        }
                        snare_hit(snr, bt + BEAT, 0.95);
                        snare_hit(snr, bt + BEAT * 3, 0.95);
                        for (int i = 0; i < 8; i++) tamb(drm, bt + i * (BEAT / 2), 0.055);
                    }
                    bell(bel, bt, root + 12, BAR * 0.95, 0.10 + 0.05 * tins);
                    bach_arp(arp, bt, root, ch->minor, 16, 12, 0.07 + 0.04 * tins);
                    int chord4[4] = {root - 12, root, third, fifth};
                    pad(pds, bt, chord4, 4, BAR * 1.02, 0.05 + 0.02 * tins);
                }
            }

            // vocal phrases run through the WHOLE song — no silence in
            // vocals, ever. the phrase starts at bar 0 (intro included),
            // each section sets its own vocal level, and the treatment
            // tier ramps raw → autotuned → full with the song.
            // the opener is toy piano + the quiet chant ONLY — the lead
            // vocal stays out of the intro and enters with the first hook
            // (~21s). everything except intro+stop carries the lead.
            if (strcmp(sec->section, "stop") && strcmp(sec->section, "intro")) {
                int is_hook = !strcmp(sec->section, "hook");
                double sec_gain = is_hook ? 1.0
                    : !strcmp(sec->section, "intro") ? 0.65
                    : !strcmp(sec->section, "bridge") ? 0.5
                    : !strcmp(sec->feel, "fade") ? 0.7
                    : 0.55; // verses — soft, present
                const int *notes = VARIANT_NOTES[hook_count % N_VARIANTS];
                int vi = hook_count % N_VARIANTS;
                int phrase_beats = 0;
                for (int w = 0; w < N_WORDS; w++) phrase_beats += BPW[w];
                int phrase_bars = phrase_beats / 4 > 0 ? phrase_beats / 4 : 1;
                double phrase_len = phrase_beats * BEAT;

                int tier;
                if (is_hook) {
                    double tins_v = total_hook_reps > 1
                        ? (double)hook_count / (total_hook_reps - 1) : 1;
                    tier = tins_v < 0.34 ? 0 : tins_v < 0.67 ? 1 : 2;
                } else {
                    // off-hook sections follow the song's position coarsely
                    tier = hook_count == 0 ? 0 : hook_count <= total_hook_reps / 2 ? 1 : 2;
                }

                for (int p = 0; p + phrase_bars <= sec->bars; p += phrase_bars) {
                    double phrase_start = (bar_cursor + p) * BAR;
                    int phrase_idx = phrase_counter++;
                    if (is_hook) crash(drm, phrase_start, p == 0 ? 0.26 : 0.16);

                    double onsets[N_WORDS];
                    onsets[0] = 0;
                    for (int w = 0; w < N_WORDS - 1; w++) onsets[w + 1] = onsets[w] + BPW[w] * BEAT;

                    int max_w = N_WORDS - 1;
                    if (!strcmp(sec->section, "intro")) max_w = p / phrase_bars; // one sample per phrase
                    for (int w = 0; w < N_WORDS; w++) {
                        if (w > max_w) break;
                        double note_len = BPW[w] * BEAT;
                        int target = notes[w];
                        int last = w == N_WORDS - 1;
                        // vowel stitch: the aaa→ooo crossfade COMPLETES
                        // before the barline so the kick never lands inside
                        // the morph. the incoming word blooms over BLEND and
                        // is at full PRE seconds before its grid onset; the
                        // outgoing vowel eases to 0.55 across exactly that
                        // window (near equal-power), then chokes later.
                        const double BLEND = tier == 0 ? 0.18 : tier == 1 ? 0.25 : 0.45;
                        const double PRE = 0.12;
                        double lead = (!last && w > 0) ? BLEND + PRE : 0;

                        Clip *c = get_clip(tier, w, phrase_idx % ROSTER_N[w], vi);
                        if (c) {
                            Break breaks[2]; int nb; double attack, gain;
                            if (last) {
                                breaks[0] = (Break){phrase_len - onsets[w] + 0.06, 0, 0.22};
                                nb = 1; attack = 0.008; gain = 1.22 * sec_gain; /* dora! */
                            } else {
                                double duck_at = onsets[w + 1] - onsets[w] + lead;
                                double choke_at = (w + 2 < N_WORDS ? onsets[w + 2] : phrase_len) - onsets[w] + lead;
                                if (w == 0) {
                                    // ease down in lockstep with the next
                                    // word's bloom window, then a long
                                    // gentle fade-out of the aaaa into dora
                                    breaks[0] = (Break){duck_at - BLEND - PRE, 0.68, BLEND};
                                    breaks[1] = (Break){choke_at - 0.45, 0, 0.50};
                                } else {
                                    breaks[0] = (Break){duck_at - 0.06, 0.30, 0.20};
                                    breaks[1] = (Break){choke_at - 0.06, 0, 0.22};
                                }
                                nb = 2;
                                attack = w == 0 ? 0 : BLEND;
                                gain = 0.95 * sec_gain;
                            }
                            paint_shaped(voc, c->s, c->n, phrase_start + onsets[w] - lead,
                                         gain, breaks, nb, attack);
                        }
                        if (is_hook)
                            sine_lead(sqr, phrase_start + onsets[w], target + 12, note_len * 0.95, 0.10);
                    }

                    // FINALE (last third of choruses): arpeggiate the
                    // america sample into a stacked harmony — pitched
                    // americas on a D-major arpeggio (the plain-tier clips
                    // are already tuned to those notes), each entering a
                    // beat apart and ringing together under the lead.
                    if (is_hook) {
                        double tinsH = total_hook_reps > 1
                            ? (double)hook_count / (total_hook_reps - 1) : 1;
                        if (tinsH > 0.62) {
                            const int ARP_VI[4] = {0, 2, 6, 5}; // D4 F#4 A4 D5
                            double swell = (tinsH - 0.62) / 0.38; // 0..1 into the finale
                            for (int a = 0; a < 4; a++) {
                                Clip *hc = get_clip(1, 0, 0, ARP_VI[a]); // plain america
                                if (!hc) continue;
                                double at = phrase_start + a * BEAT; // arpeggiated entry
                                double g = 0.16 * swell;
                                long s0 = (long)(at * SR);
                                long lim = hc->n;
                                int afin = (int)(0.04 * SR), rfin = (int)(0.5 * SR);
                                for (long i = 0; i < lim; i++) {
                                    double env = 1;
                                    if (i < afin) env = (double)i / afin;
                                    else if (i > lim - rfin) env = (double)(lim - i) / rfin;
                                    addb(voc, s0 + i, hc->s[i] * g * env);
                                }
                            }
                        }
                    }
                }
                if (is_hook) hook_count++;
            }
            bar_cursor += sec->bars;
        }
    }

    // ── sidechain duck ────────────────────────────────────────────────
    float *duck = malloc(N * 4);
    for (long i = 0; i < N; i++) duck[i] = 1;
    for (int k = 0; k < n_kicks; k++) {
        double depth = kick_times[k].serious ? 0.32 : 0.78;
        long len = (long)((kick_times[k].serious ? BEAT * 0.85 : 0.10) * SR);
        long s0 = (long)(kick_times[k].t * SR);
        for (long i = 0; i < len && s0 + i < N; i++) {
            double w = (double)i / len;
            double d = depth + (1 - depth) * (0.5 - 0.5 * cos(M_PI * w));
            if (d < duck[s0 + i]) duck[s0 + i] = (float)d;
        }
    }

    // ── fade-out across the outro (last 8 bars) ───────────────────────
    long fade_s = (long)((total_bars - 8) * BAR * SR);
    // ── vocals start at bar 0 — fade in across the intro, but from a
    // 0.35 floor so every syllable of the first "america" is heard ──────
    // the opener chant stays up front (just a tiny declick at sample 0).
    // it's the LEAD vocal that fades in — over the first 2 bars of the
    // first hook, where voc holds only the lead (no chant overlaps there).
    {
        long dc = (long)(0.02 * SR);
        for (long i = 0; i < dc && i < N; i++) voc[i] *= (float)i / dc;
    }
    {
        int hb = 0;
        for (int si = 0; si < N_SECS; si++) {
            if (!strcmp(STRUCTURE[si].section, "hook")) break;
            hb += STRUCTURE[si].bars * STRUCTURE[si].reps;
        }
        long lead0 = (long)(hb * BAR * SR);
        // the drop hits at full: the lead vocal slams in at MAX volume — no
        // swell, no fade — just a 12 ms declick so the onset doesn't pop.
        long lfin = (long)(0.012 * SR);
        for (long i = 0; i < lfin && lead0 + i < N; i++)
            voc[lead0 + i] *= (float)i / lfin;
    }

    // ── FX one-shots: gong + record scratches (freesound, c/fx/) ──────
    // scratches sprinkle the first 30s and cluster around ~2:15; the gong
    // marks the top of the finale. mixed into the drum bus.
    {
        long gn = 0, s1n = 0, s2n = 0;
        char fp[512];
        snprintf(fp, sizeof fp, "%sfx/gong.wav", voc_base);   float *gong = load_wav_mono(fp, &gn);
        snprintf(fp, sizeof fp, "%sfx/scratch1.wav", voc_base); float *sc1 = load_wav_mono(fp, &s1n);
        snprintf(fp, sizeof fp, "%sfx/scratch2.wav", voc_base); float *sc2 = load_wav_mono(fp, &s2n);
        // place a clip into drm at time t with gain
        #define PLACE(CLIP, CN, T, G) do { \
            if (CLIP) { long _s0 = (long)((T) * SR); \
                for (long _i = 0; _i < (CN); _i++) addb(drm, _s0 + _i, (CLIP)[_i] * (G)); } } while (0)
        // scratches arrive WITH the main vocal (first chorus, after the
        // quiet intro) — not over the opener.
        double choir_in = (!strcmp(STRUCTURE[0].section, "intro")
            ? STRUCTURE[0].bars * STRUCTURE[0].reps : 0) * BAR;
        PLACE(sc1, s1n, choir_in + 0.0, 0.5);
        PLACE(sc2, s2n, choir_in + 4 * BAR, 0.45);
        PLACE(sc1, s1n, choir_in + 8 * BAR + 2.5 * BEAT, 0.5);
        // ~2:15 cluster
        PLACE(sc2, s2n, 133.0, 0.6);
        PLACE(sc1, s1n, 135.0, 0.55);
        PLACE(sc2, s2n, 136.5, 0.5);
        // gong marks the lift OUT of the intro into the first chorus, and
        // the finale push (~2:18). a soft swell, not a hit at bar 0.
        PLACE(gong, gn, choir_in - 0.4, 0.55);
        PLACE(gong, gn, 138.0, 0.7);
        #undef PLACE
        free(gong); free(sc1); free(sc2);
    }

    if (stem_voc) {
        // pure vocal bus, pre-everything — for c/analyze.mjs shape checks
        if (!write_wav_f32(out_path, voc, N)) { fprintf(stderr, "write failed\n"); return 1; }
        fprintf(stderr, "stem-voc %s\n", out_path);
        return 0;
    }

    // ── reverb send → schroeder ───────────────────────────────────────
    float *send = malloc(N * 4);
    for (long i = 0; i < N; i++)
        send[i] = voc[i] * 0.85f + snr[i] * 0.65f + bel[i] * 0.55f + sqr[i] * 0.5f + arp[i] * 0.3f;
    float *room = calloc(N, 4);
    {
        static const double CDEL[4] = {0.0297, 0.0371, 0.0411, 0.0437};
        float *cb[4]; long cl[4], ci[4] = {0, 0, 0, 0};
        for (int c = 0; c < 4; c++) { cl[c] = (long)(CDEL[c] * SR); cb[c] = calloc(cl[c], 4); }
        for (long i = 0; i < N; i++) {
            double s = 0;
            for (int c = 0; c < 4; c++) {
                double y = cb[c][ci[c]];
                cb[c][ci[c]] = send[i] + (float)(y * 0.62);
                ci[c] = (ci[c] + 1) % cl[c];
                s += y;
            }
            room[i] = (float)(s * 0.25);
        }
        for (int c = 0; c < 4; c++) free(cb[c]);
        static const double ADEL[2] = {0.005, 0.0017};
        for (int a = 0; a < 2; a++) {
            long al = (long)(ADEL[a] * SR), ai = 0;
            float *ab = calloc(al, 4);
            for (long i = 0; i < N; i++) {
                double y = ab[ai], x = room[i];
                ab[ai] = (float)(x + y * 0.5);
                room[i] = (float)(y - x * 0.5);
                ai = (ai + 1) % al;
            }
            free(ab);
        }
    }

    // ── dotted-8th vocal echo ─────────────────────────────────────────
    long dsamp = (long)(BEAT * 0.75 * SR);
    float *echo = calloc(N, 4);
    for (long i = 0; i < N; i++)
        echo[i] = voc[i] + (i >= dsamp ? echo[i - dsamp] * 0.40f : 0);

    // ── master ────────────────────────────────────────────────────────
    float *mix = malloc(N * 4);
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double dk = duck[i];
        double s;
        if (vocals_only) {
            // isolated vocal mix: words + echo tail + room share, plus
            // the kick (pumping the words) and the faint harmony bed.
            double tail0 = (echo[i] - voc[i]) * 0.28;
            s = kik[i] + voc[i] * (0.75 + 0.25 * dk) + tail0 * dk
              + cho[i] * 0.7 + room[i] * 0.35;
        } else {
            // mix balance: drums punchy, the dense melodic bus pulled back a
            // touch to clear room for the voice, vocals brought forward and
            // sitting on a tighter (less washy) reverb.
            double drums = (drm[i] + kik[i] + snr[i]) * 0.96;
            double melodic = (bel[i] + sqr[i] + toy[i] + arp[i] + pds[i] + cho[i]) * 0.86 * dk;
            double subb = sb[i] * (0.55 + 0.45 * dk);
            double tail = (echo[i] - voc[i]) * 0.28;
            double vocal = voc[i] * (0.78 + 0.22 * dk) + tail * dk;
            double rm = room[i] * 0.48 * (0.7 + 0.3 * dk);
            s = drums + melodic + subb + vocal * 0.90 + rm;
        }
        double fade = i >= fade_s ? fmax(0, 1.0 - (double)(i - fade_s) / (N - fade_s)) : 1.0;
        double v = tanh(s * 0.95) * fade;
        mix[i] = (float)v;
        double a = fabs(v); if (a > peak) peak = a;
    }
    if (peak > 0) {
        double g = 0.85 / peak;
        for (long i = 0; i < N; i++) mix[i] *= (float)g;
    }

    if (!write_wav_f32(out_path, mix, N)) {
        fprintf(stderr, "✗ write failed: %s\n", out_path);
        return 1;
    }
    fprintf(stderr, "✓ %s\n", out_path);
    return 0;
}
