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
static float *drm, *kik, *snr, *bel, *sqr, *sb, *toy, *arp, *pds, *cho, *voc, *org;

static inline void addb(float *buf, long i, double v) {
    if (i >= 0 && i < N) buf[i] += (float)v;
}

// ── crisp: one-pole highpass (clears mud below hp_fc) + first-order
// presence lift (pres_amt above pres_fc) applied in place to a whole bus.
static void crisp(float *b, long n, double hp_fc, double pres_fc, double pres_amt) {
    double kh = 1.0 - exp(-TAU * hp_fc / SR), lo = 0;
    double kp = 1.0 - exp(-TAU * pres_fc / SR), mid = 0;
    for (long i = 0; i < n; i++) {
        double x = b[i];
        lo += kh * (x - lo);
        double h = x - lo;          // highpassed
        mid += kp * (h - mid);
        b[i] = (float)(h + pres_amt * (h - mid)); // + top-band lift
    }
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

// church organ — additive drawbar chord: fundamental an octave under the
// root plus octave/quint/super-octave partials, slow swell, a gentle
// tremulant. shares the reverb send so it sits in the stone room.
static void organ(float *buf, double t, int root, int third, int fifth, double dur, double g) {
    int tones[4] = {root - 12, root, third, fifth};
    static const double DRAW[5] = {1.0, 0.55, 0.38, 0.45, 0.20}; // 8' 4' 2 2/3' 2' 1'
    static const double MULT[5] = {1.0, 2.0, 3.0, 4.0, 8.0};
    int n = (int)(dur * SR); long s0 = (long)(t * SR);
    int att = (int)(0.22 * SR), rel = (int)(0.35 * SR);
    for (int m = 0; m < 4; m++) {
        double f0 = midi_hz(tones[m]);
        for (int h = 0; h < 5; h++) {
            double f = f0 * MULT[h];
            if (f > 11000) continue;
            double ph = rng() * TAU;
            for (int i = 0; i < n; i++) {
                double tt = (double)i / SR;
                double trem = 1.0 + 0.035 * sin(TAU * 5.6 * tt + m * 1.7);
                ph += TAU * f / SR;
                double env = 1;
                if (i < att) env = (double)i / att;
                else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
                addb(buf, s0 + i, sin(ph) * DRAW[h] * trem * env * g * 0.30);
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
// dora onsets (last word of each hook phrase) — collected during the render
// so the FX pass can throw a record scratch on some of them.
static double dora_times[256];
static int n_doras = 0;
// light shuffle: nudge the off-8th (odd half-beat) later so the hats swing.
static inline double swing8(double t, int i) { return (i & 1) ? t + 0.085 * BEAT : t; }

// place a clip with a time-varying pitch ratio (linear-interp resample), for
// whistley swoops + theremin waver. ratio>1 = higher & shorter; <1 = stretched
// & lower. slide bends the pitch across the note (portamento); vib adds a sine
// waver. af_s/rf_s are attack/release fades in seconds.
static void place_pitched(float *buf, const float *clip, long cn, double t,
                          double ratio, double slide, double vibHz, double vibDepth,
                          double g, double af_s, double rf_s) {
    if (!clip || cn < 2) return;
    long s0 = (long)(t * SR);
    long af = (long)(af_s * SR), rf = (long)(rf_s * SR);
    long outEst = (long)(cn / (ratio + 0.5 * slide > 0.25 ? ratio + 0.5 * slide : 0.25));
    if (outEst < 1) outEst = 1;
    double pos = 0;
    for (long i = 0; pos < cn - 1; i++) {
        double frac = (double)i / outEst;
        double r = ratio + slide * frac + vibDepth * sin(2.0 * M_PI * vibHz * i / SR);
        if (r < 0.25) r = 0.25;
        long j = (long)pos; double fr = pos - j;
        double s = clip[j] * (1 - fr) + clip[j + 1] * fr;
        double env = 1;
        if (i < af) env = (double)i / af;
        else if (i > outEst - rf) env = (double)(outEst - i) / (rf > 0 ? rf : 1);
        if (env < 0) env = 0;
        addb(buf, s0 + i, s * g * env);
        pos += r;
    }
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
    org = calloc(N, 4);

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

                // very faint harmony bed — every bar EXCEPT the first 3 bars of
                // the opener: those are bone-dry, just the vocal, no pads.
                if (!(!strcmp(sec->section, "intro") && b < 3))
                    choir(cho, bt, root, third, fifth, 0.022);

                // church organ chords — the opener rests on them (fuller while
                // the room is empty, easing back as the toy piano enters), and
                // they return to hold up the bridge breakdown.
                if (!strcmp(sec->section, "intro"))
                    organ(org, bt, root, third, fifth, BAR * 1.02, b < 6 ? 0.085 : 0.06);
                else if (!strcmp(sec->section, "bridge"))
                    organ(org, bt, root, third, fifth, BAR * 1.02, 0.07);

                if (strcmp(sec->feel, "claps-only") && strcmp(sec->feel, "fade")) {
                    double sg = (!strcmp(sec->feel, "thin") || !strcmp(sec->feel, "sparse")) ? 0.22 : 0.35;
                    if (!strcmp(sec->section, "intro")) {
                        // opener is just the vocal — sub holds out, then eases
                        // in over the last 4 bars toward the drop.
                        sg = 0.10 * fmax(0.0, fmin(1.0, (b - 5) / 4.0));
                    }
                    if (sg > 0) sub_bass(sb, bt, root, BAR * 0.95, sg);
                }

                if (!strcmp(sec->section, "stop") || !strcmp(sec->feel, "claps-only")) {
                    clap(drm, bt + BEAT, 1.05);
                    clap(drm, bt + BEAT * 3, 1.05);
                } else if (!strcmp(sec->section, "intro")) {
                    // the opener: for the first 6 bars it's JUST jeffrey's
                    // vocal (whistley + stretchy). the instrumental — soft hats
                    // + toy-piano melody — holds out and only enters over the
                    // last 4 bars, building into the drop.
                    // hats enter from the 2nd bar; the toy piano (and the rest)
                    // hold out until bar 6.
                    if (b >= 2) for (int i = 0; i < 8; i++) tamb(drm, bt + i * (BEAT / 2), 0.08);
                    if (b >= 6) {
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
                    }
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
                                // OPENER ARC: word 1 is the whistley swoop (kept —
                                // it's the hook of the intro). From word 2 on
                                // jeffrey EAGERLY becomes musical: pitching IN to
                                // tune (slide → 0), lengthening into sustained sung
                                // notes, the waver settling to a gentle musical
                                // vibrato — blooming into the drop.
                                // both voices in unison: jeffrey (0) + computer (1)
                                // the voice sneaks in: the opener starts at ~25%
                                // and swells to full carry over the first 4 words.
                                double introRamp = fmin(1.0, 0.25 + 0.75 * (double)intro_beat / (4.0 * N_SYL));
                                for (int vx = 0; vx < 2; vx++) {
                                    Clip *sc = get_syl(vx, si, ti);
                                    if (!sc) continue;
                                    double g = (vx == 0 ? 0.60 : 0.07) * lineG * introRamp; // jeffrey carries it; computer way back in the background
                                    if (vx == 0 && intro_beat < N_SYL) {
                                        // word 1 — whistley swoop
                                        double slide = (intro_beat & 1) ? 0.20 : -0.16;
                                        place_pitched(voc, sc->s, sc->n, bt + k * BEAT,
                                                      0.90, slide, 5.5, 0.05, g, 0.05, 0.34);
                                    } else if (vx == 0) {
                                        // words 2+ — eagerly turn musical over ~2 words:
                                        // stay IN TUNE (ratio 1.0 — no pitching lower
                                        // and lower), the portamento settles onto the
                                        // note, the waver becomes a musical vibrato,
                                        // and the legato tail lengthens.
                                        double p = fmin(1.0, (double)(intro_beat - N_SYL) / (2.0 * N_SYL));
                                        double slide = (1.0 - p) * ((intro_beat & 1) ? 0.12 : -0.10); // pitch in
                                        double vib   = 0.045 - 0.026 * p;                      // waver → musical vibrato
                                        double rel   = 0.34 + 0.18 * p;                        // legato tail grows
                                        place_pitched(voc, sc->s, sc->n, bt + k * BEAT,
                                                      1.0, slide, 5.2, vib, g * (1.0 + 0.12 * p), 0.05, rel);
                                    } else {
                                        // the bg computer voice — straight
                                        long lim = sc->n < slot ? sc->n : slot;
                                        for (long i = 0; i < lim; i++) {
                                            double env = 1;
                                            if (i < af) env = (double)i / af;
                                            else if (i > lim - rf) env = (double)(lim - i) / rf;
                                            addb(voc, s0 + i, sc->s[i] * g * env);
                                        }
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
                                double g = vx == 0 ? 0.52 : 0.07; // jeffrey carries it; computer way back in the background
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
                        for (int i = 0; i < 8; i++) tamb(drm, swing8(bt + i * (BEAT / 2), i), 0.04);
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
                        for (int i = 0; i < 8; i++) tamb(drm, swing8(bt + i * (BEAT / 2), i), 0.055);
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
                            // first Whitney hit at the drop lands LOUD — the
                            // opening "america" of the very first hook gets a
                            // big lift, the rest of that phrase a smaller one.
                            double firstHookG = (is_hook && hook_count == 0 && p == 0)
                                ? (w == 0 ? 1.40 : 1.12) : 1.0;
                            if (last) {
                                breaks[0] = (Break){phrase_len - onsets[w] + 0.06, 0, 0.22};
                                nb = 1; attack = 0.008; gain = 1.22 * sec_gain * firstHookG; /* dora! */
                                if (is_hook && n_doras < 256)
                                    dora_times[n_doras++] = phrase_start + onsets[w]; // for the scratch
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
                                gain = 0.95 * sec_gain * firstHookG;
                            }
                            // soften "computer": skip the hard "co" transient so
                            // the long crossfade swells in as "...uhmmmputer".
                            long coSkip = (w == 1) ? (long)(0.085 * SR) : 0;
                            paint_shaped(voc, c->s + coSkip, c->n - coSkip,
                                         phrase_start + onsets[w] - lead,
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
        long gn = 0, s1n = 0, s2n = 0, shn = 0;
        char fp[512];
        snprintf(fp, sizeof fp, "%sfx/gong.wav", voc_base);   float *gong = load_wav_mono(fp, &gn);
        snprintf(fp, sizeof fp, "%sfx/scratch1.wav", voc_base); float *sc1 = load_wav_mono(fp, &s1n);
        snprintf(fp, sizeof fp, "%sfx/scratch2.wav", voc_base); float *sc2 = load_wav_mono(fp, &s2n);
        snprintf(fp, sizeof fp, "%sfx/shock-rifle.wav", voc_base); float *shk = load_wav_mono(fp, &shn);
        // place a clip into drm at time t with gain
        #define PLACE(CLIP, CN, T, G) do { \
            if (CLIP) { long _s0 = (long)((T) * SR); \
                for (long _i = 0; _i < (CN); _i++) addb(drm, _s0 + _i, (CLIP)[_i] * (G)); } } while (0)
        // TWO deliberate moments only (computed from the structure so they
        // stay aligned when the arrangement changes) — kept sparse on purpose:
        //   1) the DROP — first hook after the quiet opener
        //   2) the FINALE LAUNCH — the stop/pad-dropout into the last hook
        double choir_in = (!strcmp(STRUCTURE[0].section, "intro")
            ? STRUCTURE[0].bars * STRUCTURE[0].reps : 0) * BAR;
        int lastHook = -1;
        for (int s = 0; s < N_SECS; s++) if (!strcmp(STRUCTURE[s].section, "hook")) lastHook = s;
        double finale = 0;
        { int bb = 0; for (int s = 0; s < lastHook; s++) bb += STRUCTURE[s].bars * STRUCTURE[s].reps;
          finale = bb * BAR; }
        // 1) drop
        PLACE(gong, gn, choir_in - 0.40, 0.55); // soft swell into the drop
        PLACE(shk, shn, choir_in - 0.18, 0.80); // shock rifle fires the first Whitney hit in
        PLACE(sc1, s1n, choir_in + 0.00, 0.90); // one loud scratch announces it
        // 2) finale launch — shock rifle + scratch + gong all land together as
        // the pads drop out and the last hook hits.
        PLACE(gong, gn, finale - 0.45, 0.65);
        PLACE(shk, shn, finale - 0.18, 0.85);
        PLACE(sc1, s1n, finale + 0.00, 0.75);
        #undef PLACE
        free(gong); free(sc1); free(sc2); free(shk);
    }

    if (stem_voc) {
        // pure vocal bus, pre-everything — for c/analyze.mjs shape checks
        if (!write_wav_f32(out_path, voc, N)) { fprintf(stderr, "write failed\n"); return 1; }
        fprintf(stderr, "stem-voc %s\n", out_path);
        return 0;
    }

    // ── crisp pass (post-stem so analyze.mjs sees the raw stitch) ──────
    // the voice loses its rumble and gets a presence lift; the beds (choir,
    // pads) and toy piano shed low mud so the mix stops feeling bassy —
    // kick + sub keep their full weight untouched.
    crisp(voc, N, 160, 3500, 0.35);
    crisp(cho, N, 220, 5000, 0.10);
    crisp(pds, N, 180, 5000, 0.10);
    crisp(toy, N, 150, 4000, 0.20);
    crisp(org, N, 90, 4500, 0.10); // organ keeps its 8' warmth, just no rumble

    // ── reverb send → schroeder ───────────────────────────────────────
    // the main vocal starts DRY — its reverb send fades in across the first
    // ~75s, so the opener and the drop punch dry and the space only opens up
    // as the song develops. (instruments keep their constant send.)
    // the main vocal is bone-DRY (zero reverb) through the whole opener, then
    // its send fades in over ~60s after the drop — the space opens up only as
    // the song develops. (instruments keep their constant send.)
    double introEnd = STRUCTURE[0].bars * STRUCTURE[0].reps * BAR;
    double rvStart = introEnd * SR, rvLen = 60.0 * SR;
    float *send = malloc(N * 4);
    for (long i = 0; i < N; i++) {
        double vfade = i < rvStart ? 0.0 : fmin(1.0, (i - rvStart) / rvLen);
        send[i] = (float)(voc[i] * 0.85 * vfade)
                + snr[i] * 0.65f + bel[i] * 0.55f + sqr[i] * 0.5f + arp[i] * 0.3f
                + org[i] * 0.55f;
    }
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

    // ── vocal-keyed sidechain: the backing DIPS while the lead vocal sounds
    // so the voice sits forward and the track flows (envelope follower on the
    // dry vocal — fast attack, slow release). ────────────────────────────
    float *vSide = malloc(N * 4);
    {
        double e = 0;
        double atk = exp(-1.0 / (0.004 * SR)); // 4 ms attack
        double rel = exp(-1.0 / (0.18 * SR));  // 180 ms release
        for (long i = 0; i < N; i++) {
            double x = fabs(voc[i]);
            double c = x > e ? atk : rel;
            e = x + c * (e - x);
            vSide[i] = (float)fmin(1.0, e * 2.2); // normalize → ~0..1 presence
        }
    }

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
            // vocal sidechain: melodic bus ducks hardest, drums a touch, so the
            // lead vocal carves its own space (flow).
            double vd = vSide[i];
            double drums = (drm[i] + kik[i] + snr[i]) * 0.96 * (1.0 - 0.16 * vd);
            double melodic = (bel[i] + sqr[i] + toy[i] + arp[i] + pds[i] + cho[i] + org[i]) * 0.86 * dk * (1.0 - 0.45 * vd);
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
