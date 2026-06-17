// maytrax.c — C engine for the maytrax track (americomputadora/hellsine
// pattern): JS bakes c/score.h (the freesound sample roster + drop shouts),
// this renders the full stereo song to a float32 WAV; render-c.mjs runs the
// ffmpeg pop-master (loudnorm + alimiter) and encodes the mp3.
//
// Faithful port of bin/maytrax.mjs (160 BPM big-beat techno, jungle mode):
// sample-driven kit + pitched one-shots over the F-minor note data, Schroeder
// reverb, kick sidechain, crunch + state-variable filter sweep, healing
// drone, sine-tinge droplets, payphone intro, tape-stop ending, phone-world
// loop tail. The drums are the TAMED breakbeat: a steady kick/snare anchor
// with no ghost snares and almost no swing/jitter (the JS scatter read as
// "wild / skippy"). Synth bodies stay as fallbacks if a sample is missing.
//
// Build:  ./build.sh        Run:  ./maytrax --out out/c-raw.wav

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
static double BPMV = SCORE_BPM, BEAT, BAR, SX;

// tamed-feel globals — the JS defaults were DEC .65 / LAZY .45 / HUMAN .4;
// the breakbeat read as "wild / skippy", so the drag + micro-timing jitter
// are pulled WAY down here (the break pattern itself is anchored below).
static const double DEC = 0.65;
static double LAZY = 0.22;
static double HUMAN = 0.15;
static int g_payphone = 0; // lo-fi telephone intro/loop — off by default (clean start)

// ── deterministic RNG (xorshift32) — reproducible "humanize" + scatter ──
static uint32_t rng_s = 0x6d617978; // "mayx"
static inline double rnd(void) {
    rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5;
    return (double)rng_s / 4294967296.0;
}
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double hz(void) { return rnd2() * 0.004 * HUMAN; }

static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

// ── buses ───────────────────────────────────────────────────────────────
// busL/busR — the ducked music bus.  drm — dry kicks/snares (bypass duck,
// center).  revL/revR — reverb send.  vocL/vocR — un-ducked shout bus.
static long N;
static float *busL, *busR, *drm, *revL, *revR, *vocL, *vocR;

static inline void addb(float *b, long i, double v) { if (i >= 0 && i < N) b[i] += (float)v; }
static inline void adds(float *L, float *R, long i, double l, double r) {
    if (i >= 0 && i < N) { L[i] += (float)l; R[i] += (float)r; }
}

// ── wav io ──────────────────────────────────────────────────────────────
// reads PCM16 / PCM24 / float32, mono or stereo → mono float at native SR.
// trims leading near-silence (<0.005) so one-shots start on the transient.
typedef struct { float *s; long n; int sr; int det; int nom; int ok; } Smp;

static float *load_wav(const char *path, long *out_n, int *out_sr) {
    FILE *f = fopen(path, "rb");
    if (!f) { fprintf(stderr, "  ! missing %s\n", path); return NULL; }
    uint8_t hdr[12];
    if (fread(hdr, 1, 12, f) != 12 || memcmp(hdr, "RIFF", 4) || memcmp(hdr + 8, "WAVE", 4)) { fclose(f); return NULL; }
    uint16_t fmt = 1, ch = 1, bits = 16; uint32_t sr = 44100;
    float *out = NULL; long n = 0;
    for (;;) {
        uint8_t ck[8];
        if (fread(ck, 1, 8, f) != 8) break;
        uint32_t sz = ck[4] | (ck[5] << 8) | (ck[6] << 16) | ((uint32_t)ck[7] << 24);
        if (!memcmp(ck, "fmt ", 4)) {
            uint8_t b[40]; uint32_t rd = sz < 40 ? sz : 40; fread(b, 1, rd, f);
            fmt = b[0] | (b[1] << 8); ch = b[2] | (b[3] << 8);
            sr = b[4] | (b[5] << 8) | (b[6] << 16) | ((uint32_t)b[7] << 24);
            bits = b[14] | (b[15] << 8);
            if (sz > rd) fseek(f, sz - rd, SEEK_CUR);
        } else if (!memcmp(ck, "data", 4)) {
            long frames = sz / (bits / 8) / ch;
            out = malloc(sizeof(float) * (frames > 0 ? frames : 1)); n = frames;
            for (long i = 0; i < frames; i++) {
                double acc = 0;
                for (int c = 0; c < ch; c++) {
                    if (fmt == 3 && bits == 32) { float v; fread(&v, 4, 1, f); acc += v; }
                    else if (bits == 24) { uint8_t p3[3]; fread(p3, 1, 3, f); int s = p3[0] | (p3[1] << 8) | (p3[2] << 16); if (s & 0x800000) s |= ~0xffffff; acc += s / 8388608.0; }
                    else { int16_t v; fread(&v, 2, 1, f); acc += v / 32768.0; }
                }
                out[i] = (float)(acc / ch);
            }
            break;
        } else fseek(f, sz + (sz & 1), SEEK_CUR);
    }
    fclose(f);
    // trim leading silence
    long lead = 0; while (lead < n && fabs(out[lead]) < 0.005) lead++;
    if (lead > 0 && lead < n) { memmove(out, out + lead, sizeof(float) * (n - lead)); n -= lead; }
    *out_n = n; *out_sr = (int)sr;
    return out;
}

// f0 via autocorrelation around the peak (port of detectF0) → smpNote root.
static double detect_f0(const float *s, long n, int sr) {
    double pk = 0; long pidx = 0;
    for (long i = 0; i < n; i++) { double a = fabs(s[i]); if (a > pk) { pk = a; pidx = i; } }
    long w = n < 8192 ? n : 8192, st = pidx - (w >> 1);
    if (st < 0) st = 0; if (st > n - w) st = n - w; if (st < 0) st = 0;
    long minLag = sr / 1200, maxLag = sr / 45;
    double best = -1; long bestLag = 0;
    for (long lag = minLag; lag <= maxLag; lag++) {
        double sum = 0; for (long i = 0; i + lag < w; i++) sum += s[st + i] * s[st + i + lag];
        if (sum > best) { best = sum; bestLag = lag; }
    }
    return bestLag > 0 ? (double)sr / bestLag : 0;
}

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32, fsz = 16;
    uint32_t fsz32 = 16;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz32, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    (void)fsz;
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── sample roster ─────────────────────────────────────────────────────────
static Smp samp[N_ROLES];
static Smp wolf_rev, animal_rev; // reversed risers

static Smp *smpl(const char *name) {
    if (!strcmp(name, "wolf_rev")) return wolf_rev.ok ? &wolf_rev : NULL;
    if (!strcmp(name, "animal_rev")) return animal_rev.ok ? &animal_rev : NULL;
    for (int i = 0; i < N_ROLES; i++) if (!strcmp(ROLE_NAME[i], name)) return samp[i].ok ? &samp[i] : NULL;
    return NULL;
}

static void load_roster(void) {
    for (int i = 0; i < N_ROLES; i++) {
        long n; int sr; float *s = load_wav(ROLE_PATH[i], &n, &sr);
        if (!s || n <= 0) { samp[i].ok = 0; continue; }
        samp[i] = (Smp){ s, n, sr, 60, ROLE_NOMINAL[i], 1 };
        if (ROLE_PITCHED[i]) {
            double f = detect_f0(s, n, sr);
            samp[i].det = (f > 30 && f < 2000) ? (int)lround(69 + 12 * log2(f / 440.0)) : (ROLE_NOMINAL[i] < 0 ? 60 : ROLE_NOMINAL[i]);
        }
    }
    Smp *w = smpl("wolf"), *a = smpl("animal");
    if (w) { wolf_rev = *w; wolf_rev.s = malloc(sizeof(float) * w->n); for (long i = 0; i < w->n; i++) wolf_rev.s[i] = w->s[w->n - 1 - i]; wolf_rev.ok = 1; }
    if (a) { animal_rev = *a; animal_rev.s = malloc(sizeof(float) * a->n); for (long i = 0; i < a->n; i++) animal_rev.s[i] = a->s[a->n - 1 - i]; animal_rev.ok = 1; }
}

// ── sample players ────────────────────────────────────────────────────────
// stereo placement (playSampleStereo): pan via gain + light haas when pan>0.
// `attack` (seconds) eases the onset in so a one-shot swells rather than
// slams — used to de-claw the roars/gong/screams so they aren't violent.
static void place_a(float *L, float *R, Smp *s, double t, double gain, double rate, double pan, double maxDur, double attack) {
    if (!s) return;
    long s0 = (long)(t * SR);
    double step = ((double)s->sr / SR) * rate;
    double lg = gain * (1 - pan * 0.5), rg = gain * (1 + pan * 0.5);
    long haas = pan > 0 ? (long)(0.012 * SR) : 0;
    long outMax = maxDur >= 1e8 ? (long)1e18 : (long)(maxDur * SR);
    long relN = (long)(0.02 * SR), attN = (long)(attack * SR);
    for (long i = 0; s0 + i < N; i++) {
        double p = i * step; long j = (long)p;
        if (j >= s->n - 1 || i >= outMax) break;
        double f = p - j, v = s->s[j] * (1 - f) + s->s[j + 1] * f;
        if (attN > 0 && i < attN) v *= 0.5 - 0.5 * cos(M_PI * (double)i / attN); // raised-cosine swell
        if (outMax < (long)1e17 && i > outMax - relN) v *= (double)(outMax - i) / relN;
        adds(L, R, s0 + i, v * lg, 0);
        if (haas > 0) adds(L, R, s0 + i + haas, 0, v * rg); else adds(L, R, s0 + i, 0, v * rg);
    }
}
static void place(float *L, float *R, Smp *s, double t, double gain, double rate, double pan, double maxDur) {
    place_a(L, R, s, t, gain, rate, pan, maxDur, 0);
}

// mono placement (smpMono). returns 1 if the sample existed.
static int mono(float *buf, Smp *s, double t, double gain, double rate, double maxDur) {
    if (!s) return 0;
    long s0 = (long)(t * SR);
    double step = ((double)s->sr / SR) * rate;
    long outMax = maxDur >= 1e8 ? (long)1e18 : (long)(maxDur * SR);
    long relN = (long)(0.02 * SR);
    long i = 0;
    for (double p = 0; p < s->n - 1 && i < outMax; i++, p += step) {
        long idx = (long)p; double fr = p - idx;
        double v = (s->s[idx] * (1 - fr) + s->s[idx + 1] * fr) * gain;
        if (outMax < (long)1e17 && i > outMax - relN) v *= (double)(outMax - i) / relN;
        addb(buf, s0 + i, v);
    }
    return 1;
}
static int mono_note(float *buf, Smp *s, double note, double t, double gain, double maxDur) {
    if (!s) return 0;
    return mono(buf, s, t, gain, pow(2.0, (note - s->det) / 12.0), maxDur);
}
static inline double rate_nom(Smp *s, double note) { return pow(2.0, (note - s->nom) / 12.0); }

// ── synth voices (active: the rest fall back to samples) ──────────────────
static void square_note(float *L, float *R, double midi, double t, double dur, double g, double pan) {
    double f = midi_hz(midi); long n = (long)(dur * SR), s0 = (long)(t * SR);
    double lg = g * (1 - pan * 0.5), rg = g * (1 + pan * 0.5);
    long atk = (long)(0.004 * SR), rel = (long)(0.04 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pw = 0.5 + 0.06 * sin(TAU * 5 * tt);
        double ph = fmod(f * tt, 1.0), ph2 = fmod(f * 1.003 * tt, 1.0);
        double s = (ph < pw ? 1 : -1) * 0.6 + (ph2 < 0.5 ? 1 : -1) * 0.4;
        double env = 1; if (i < atk) env = (double)i / atk; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        adds(L, R, s0 + i, s * env * lg, s * env * rg);
    }
}

static void riser(float *L, float *R, double t0, double dur, double g) {
    long n = (long)(dur * SR), s0 = (long)(t0 * SR);
    double prev = 0, ph = 0;
    for (long i = 0; i < n; i++) {
        double w = (double)i / n;
        double cutoff = 800 + (8000 - 800) * w;
        double a = exp(-1.0 / (SR / cutoff));
        double nz = rnd2(); double filt = nz - prev * a; prev = nz;
        double f = 80 + (1200 - 80) * w * w;
        ph += TAU * f / SR;
        double sine = sin(ph) * 0.5, env = w * w;
        double v = (filt * 0.6 + sine * 0.4) * env * g, spread = w * 0.6;
        adds(L, R, s0 + i, v * (1 - spread), v * (1 + spread));
    }
}

static void reverse_swell(float *L, float *R, double t0, double dur, double g) {
    long n = (long)(dur * SR), s0 = (long)(t0 * SR);
    double lpL = 0, lpR = 0;
    for (long i = 0; i < n; i++) {
        double w = (double)i / n, cutoff = 200 + (6000 - 200) * w;
        double a = 1 - exp(-TAU * cutoff / SR);
        lpL += a * (rnd2() - lpL); lpR += a * (rnd2() - lpR);
        double env = w * w;
        adds(L, R, s0 + i, lpL * env * g, lpR * env * g);
    }
}

// ── drum voices (sample-backed; synth bodies are the fallbacks) ───────────
static void kick909(float *buf, double t, double g) {
    if (mono(buf, smpl("kick"), t, g * 0.9, 1, 0.20 * DEC)) {
        long n = (long)(0.20 * SR), s0 = (long)(t * SR); double ph = 0;
        for (long i = 0; i < n; i++) {
            double tt = (double)i / SR, f = 110 * exp(-tt * 26) + 40;
            ph += TAU * f / SR;
            addb(buf, s0 + i, tanh(sin(ph) * 1.3) * exp(-tt * 6.5) * 0.72 * g);
        }
        return;
    }
    long n = (long)(0.28 * SR), s0 = (long)(t * SR); double ph = 0, sub = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, f = 52 + (165 - 52) * exp(-tt * 50);
        ph += TAU * f / SR; double body = sin(ph) * exp(-tt * 11);
        sub += TAU * 38 / SR; double subL = sin(sub) * exp(-tt * 5) * 0.55;
        double click = i < SR * 0.003 ? rnd2() * 0.9 * (1 - i / (SR * 0.003)) : 0;
        addb(buf, s0 + i, tanh((body + click) * 2.2) * g + subL * g * 0.7);
    }
}
static void snare_brk(float *buf, double t, double g) {
    if (mono(buf, smpl("snare"), t, g * 0.95, 1, 0.18 * DEC)) return;
    long n = (long)(0.18 * SR), s0 = (long)(t * SR); double prev = 0, ph = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2(), hp = nz - prev * 0.62; prev = nz;
        ph += TAU * 195 / SR;
        double body = sin(ph) * exp(-tt * 28) * 0.55, tail = hp * exp(-tt * 18);
        addb(buf, s0 + i, (body + tail) * g);
    }
}
static void hat_mono(float *buf, double t, double g) {
    if (mono(buf, smpl("hat"), t, g * 1.4, 1, 0.05 * DEC)) return;
    long n = (long)(0.045 * SR), s0 = (long)(t * SR); double prev = 0;
    for (long i = 0; i < n; i++) { double tt = (double)i / SR, nz = rnd2(), hp = nz - prev * 0.78; prev = nz; addb(buf, s0 + i, hp * exp(-tt * 90) * g); }
}
static void ohat_mono(float *buf, double t, double g) {
    if (mono(buf, smpl("ohat"), t, g * 1.3, 1, 0.13 * DEC)) return;
    long n = (long)(0.22 * SR), s0 = (long)(t * SR); double prev = 0;
    for (long i = 0; i < n; i++) { double tt = (double)i / SR, nz = rnd2(), hp = nz - prev * 0.82; prev = nz; addb(buf, s0 + i, hp * exp(-tt * 14) * g); }
}
static void shaker_mono(float *buf, double t, double g) {
    if (mono(buf, smpl("shaker"), t, g * 1.6, 1, 0.06 * DEC)) return;
    long n = (long)(0.09 * SR), s0 = (long)(t * SR); double prev = 0, prev2 = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2(), hp = nz - prev * 0.86; prev = nz;
        double hp2 = hp - prev2 * 0.4; prev2 = hp;
        double att = fmin(1, tt / 0.003);
        addb(buf, s0 + i, hp2 * att * exp(-tt * 52) * g);
    }
}

// place a mono hat once and pan it into the stereo bus (tightHatStereo).
static void tight_hat(float *L, float *R, double t, int open, double g, double pan) {
    double dur = open ? 0.22 : 0.045; long ln = (long)(dur * SR) + 4;
    float *tmp = calloc(ln, sizeof(float));
    if (open) ohat_mono(tmp, 0, g); else hat_mono(tmp, 0, g);
    long s0 = (long)(t * SR); double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < ln; i++) { long d = s0 + i; if (d < 0 || d >= N) continue; double v = tmp[i]; L[d] += (float)(v * lg); R[d] += (float)(v * rg); }
    free(tmp);
}

// ── TAMED breakbeat ────────────────────────────────────────────────────────
// the JS breakBar cycled four funky-drummer variations with scattered kicks,
// sprinkled ghost snares, per-offbeat swing, whole-kit drag and random gains
// — that stack read as "wild / skippy / not flowy". Here: a steady kick on
// 1 + the classic "& of 3" push (and a 6-push on the alt bar), a FIRM 2 & 4
// backbeat (no ghosts), continuous closed hats on every offbeat with open
// lifts on the &s (fills the gaps → flow), and almost no swing/jitter.
static void break_bar(float *L, float *R, float *dr, double t0, double energy, double hatPan) {
    static const int KV[2][16] = {
        { 1,0,0,1, 0,0,0,0, 0,0,1,0, 0,0,0,0 },   // 1 · & of 1 · & of 3
        { 1,0,0,0, 0,0,1,0, 0,0,1,0, 0,0,0,0 },   // 1 · & of 2 · & of 3
    };
    static const int SV[16] = { 0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0 }; // firm 2 & 4
    static const char HP[16] = { 'h','h','h','H','h','h','h','h','h','H','h','h','h','h','H','h' };
    int vi = ((int)llround(t0 / BAR) / 4) & 1;
    const int *kP = KV[vi];
    double drag = 0.006 * LAZY;
    for (int e = 0; e < 16; e++) {
        double t = t0 + e * SX;
        if (kP[e]) kick909(dr, t + drag * 0.4, 1.18 * energy);
        if (SV[e]) snare_brk(dr, t + drag, 0.84 * energy);
        double pan = (e % 2 == 0 ? -1 : 1) * hatPan, th = t + drag;
        if (HP[e] == 'h') tight_hat(L, R, th, 0, 0.11 * energy, pan);
        else tight_hat(L, R, th, 1, 0.13 * energy, pan);
    }
}

// sunk-in swung-16th shaker groove + open-hat lifts → reverb (percGroove).
static void perc_groove(float *bL, float *bR, float *rL, float *rR, double t0, double energy, double swing, double base, double revSend) {
    long shLen = (long)(0.09 * SR) + 4, ohLen = (long)(0.22 * SR) + 4;
    float *tmp = calloc(ohLen, sizeof(float));
    for (int e = 0; e < 16; e++) {
        double t = t0 + e * SX + (e % 2 == 1 ? swing : 0);
        long s0 = (long)(t * SR);
        int accent = e % 4 == 2;
        double jitter = 0.75 + 0.25 * (((e * 7) % 5) / 4.0);
        double g = (accent ? base * 1.5 : base) * energy * jitter;
        double pan = (e % 2 == 0 ? -1 : 1) * 0.28;
        memset(tmp, 0, sizeof(float) * shLen);
        shaker_mono(tmp, 0, g);
        double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
        for (long i = 0; i < shLen; i++) { double v = tmp[i]; if (!v) continue; long d = s0 + i; if (d < 0 || d >= N) continue; bL[d] += (float)(v * lg); bR[d] += (float)(v * rg); rL[d] += (float)(v * lg * revSend); rR[d] += (float)(v * rg * revSend); }
        if (e == 6 || e == 14) {
            memset(tmp, 0, sizeof(float) * ohLen);
            ohat_mono(tmp, 0, 0.085 * energy);
            double op = (e == 6 ? -1 : 1) * 0.22, olg = 1 - op * 0.5, org = 1 + op * 0.5;
            for (long i = 0; i < ohLen; i++) { double v = tmp[i]; if (!v) continue; long d = s0 + i; if (d < 0 || d >= N) continue; bL[d] += (float)(v * olg); bR[d] += (float)(v * org); rL[d] += (float)(v * olg * 0.5); rR[d] += (float)(v * org * 0.5); }
        }
    }
    free(tmp);
}

// ── pitched bed voices (sample-backed) ────────────────────────────────────
static void sub_bass(float *buf, double t, double midi, double dur, double g) {
    if (mono_note(buf, smpl("sub"), midi, t, g * 1.1, dur)) return;
    double f = midi_hz(midi); long n = (long)(dur * SR), s0 = (long)(t * SR);
    long att = (long)(0.012 * SR), rel = (long)(0.08 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR; double env = 1;
        if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double s = sin(ph) + sin(ph * 2) * 0.12;
        addb(buf, s0 + i, tanh(s * 1.05) * env * g);
    }
}
static void dark_pad(float *L, float *R, double t, const int *midis, int nm, double dur, double g) {
    Smp *pad = smpl("pad");
    if (pad) { for (int k = 0; k < nm; k++) place(L, R, pad, t, g * 0.6, pow(2.0, (midis[k] - pad->det) / 12.0), (k / fmax(1, nm - 1) - 0.5) * 0.6, 1e9); return; }
    (void)dur;
}
static void acid303(float *buf, double t, double midi, double dur, double gain, int accent) {
    if (mono_note(buf, smpl("reese"), midi, t, gain * (accent ? 1.3 : 1.0), dur)) return;
    long n = (long)(dur * SR), s0 = (long)(t * SR);
    double f0 = midi_hz(midi), lp = 0, bp = 0, q = accent ? 0.08 : 0.14, saw = 0;
    double envDecay = accent ? 9 : 18, cutMax = accent ? 4800 : 2600;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR; saw += f0 / SR; saw -= floor(saw);
        double osc = saw * 2 - 1, env = exp(-tt * envDecay);
        double cutoff = 320 + (cutMax - 320) * env, fc = tan(M_PI * fmin(0.45, cutoff / SR));
        double hp = osc - lp - q * bp; bp += fc * hp; lp += fc * bp;
        double amp = exp(-tt * (accent ? 5 : 8)) * gain;
        addb(buf, s0 + i, tanh(lp * 1.2) * amp);
    }
}
static void snare_roll(float *buf, double t0, int bars, double baseGain) {
    int total = bars * 16;
    for (int e = 0; e < total; e++) { double t = t0 + e * SX, w = (double)e / total; snare_brk(buf, t, baseGain + 0.5 * w); }
}

// place a mono 303/reese voice into stereo with a short haas (drop/break use).
static void place303_stereo(double t, double midi, double dur, double gain, int accent, double harmG, int harmInt, double haasSec) {
    float *tmp = calloc(N, sizeof(float)); float *tmpH = calloc(N, sizeof(float));
    acid303(tmp, t, midi, dur, gain, accent);
    if (harmG > 0) acid303(tmpH, t, midi + harmInt, dur, harmG, accent);
    long s0 = (long)(t * SR), h = (long)(haasSec * SR), ex = (long)((t + dur * 1.3) * SR);
    if (ex > N) ex = N;
    for (long i = s0; i < ex; i++) { double v = tmp[i], vh = tmpH[i]; if (v || vh) { busL[i] += (float)(v + vh * 0.55); if (i + h < N) busR[i + h] += (float)(v + vh); } }
    free(tmp); free(tmpH);
}

// ── kick sidechain times ──────────────────────────────────────────────────
static double kick_times[8192]; static int n_kt = 0;
static void push_kt(double t) { if (n_kt < 8192) kick_times[n_kt++] = t; }

// hoover stab — a soft sampled-reese triad voice (the JS synth hoover is a
// no-op in jungle mode; here the stab body is the reese sample, kept soft).
static void hoover_stab(double t, double midi, double dur, double gain, double gL, double gR) {
    float *tmp = calloc(N, sizeof(float));
    mono_note(tmp, smpl("reese"), midi, t, gain, dur);
    long s0 = (long)(t * SR), haas = (long)(0.009 * SR), ex = (long)((t + dur) * SR);
    if (ex > N) ex = N;
    for (long i = s0; i < ex; i++) { double v = tmp[i]; if (v) { busL[i] += (float)(v * gL); if (i + haas < N) busR[i + haas] += (float)(v * gR); } }
    free(tmp);
}

// ── F-minor harmony ────────────────────────────────────────────────────────
static const int FM_ROOTS[4] = { 53, 51, 49, 48 };
static const int FM_PAD[4] = { 41, 53, 56, 60 };
static const int EB_PAD[4] = { 39, 51, 54, 58 };
static const int DB_PAD[4] = { 37, 49, 52, 56 };
static const int C_PAD[4]  = { 36, 48, 51, 55 };

// section table
static const char *ORDER[8] = { "intro", "buildA", "drop1", "breakA", "drop2", "breakB", "drop3", "outro" };
static const int SECBARS[8] = { 8, 4, 16, 8, 16, 8, 16, 12 };
static int START[8];
static int sec_index(const char *k) { for (int i = 0; i < 8; i++) if (!strcmp(ORDER[i], k)) return i; return 0; }
static const char *sec_of_bar(int b) { for (int i = 0; i < 8; i++) { if (b < START[i] + SECBARS[i]) return ORDER[i]; } return "outro"; }

static int total_bars(void) { int t = 0; for (int i = 0; i < 8; i++) t += SECBARS[i]; return t; }

int main(int argc, char **argv) {
    const char *out_path = "out/c-raw.wav";
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]);
        else if (!strcmp(argv[i], "--lazy") && i + 1 < argc) LAZY = atof(argv[++i]);
        else if (!strcmp(argv[i], "--human") && i + 1 < argc) HUMAN = atof(argv[++i]);
        else if (!strcmp(argv[i], "--payphone")) g_payphone = 1;
    }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4; SX = BEAT / 4;
    { int c = 0; for (int i = 0; i < 8; i++) { START[i] = c; c += SECBARS[i]; } }
    int TB = total_bars();
    double totalSec = TB * BAR + 4.0;
    N = (long)(totalSec * SR);

    busL = calloc(N, 4); busR = calloc(N, 4); drm = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); vocL = calloc(N, 4); vocR = calloc(N, 4);
    load_roster();
    Smp *bellS; { long bn; int bsr; float *bs = load_wav(BELL_PATH, &bn, &bsr); static Smp bellSt; if (bs) { bellSt = (Smp){ bs, bn, bsr, 60, 60, 1 }; bellS = &bellSt; } else bellS = NULL; }

    fprintf(stderr, "# maytrax.c · %g BPM · %d bars · %.1fs · stereo\n", BPMV, TB, totalSec);

    // ════════════════ ARRANGEMENT ════════════════
    int cursor;

    // ── INTRO (8) — sub pulse + sparse hats + dark pad swell ──
    cursor = START[sec_index("intro")];
    {
        double t0 = cursor * BAR;
        // staggered intro — instruments enter ONE AT A TIME so the track
        // builds layer by layer:
        //   bar 0  pad (harmonic bed)
        //   bar 2  flute shanty melody  (in the sampled-instrument layer)
        //   bar 4  sub-bass pulse
        //   bar 6  hats (the pulse ticks in)
        dark_pad(busL, busR, t0, FM_PAD, 4, BAR * 4 - 0.05, 0.10);
        dark_pad(busL, busR, t0 + BAR * 4, EB_PAD, 4, BAR * 4 - 0.05, 0.12);
        for (int b = 4; b < 8; b++) {                       // sub enters bar 4
            int root = 27;
            for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + (b * 4 + q) * BEAT, root, BEAT * 0.9, 0.18); sub_bass(busR, t0 + (b * 4 + q) * BEAT, root, BEAT * 0.9, 0.18); }
        }
        for (int b = 6; b < 8; b++) for (int e = 0; e < 16; e++) if (e == 6 || e == 14) tight_hat(busL, busR, t0 + b * BAR + e * SX, 1, 0.08, e == 6 ? -0.08 : 0.08); // hats enter bar 6
    }

    // ── BUILD A (4) — pad rises, snare roll, riser ──
    cursor = START[sec_index("buildA")];
    {
        double t0 = cursor * BAR;
        dark_pad(busL, busR, t0, DB_PAD, 4, BAR * 2 - 0.05, 0.22);
        dark_pad(busL, busR, t0 + BAR * 2, C_PAD, 4, BAR * 2 - 0.05, 0.26);
        for (int b = 0; b < 4; b++) { int root = b < 2 ? 25 : 24; for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + (b * 4 + q) * BEAT, root, BEAT * 0.9, 0.22); sub_bass(busR, t0 + (b * 4 + q) * BEAT, root, BEAT * 0.9, 0.22); } }
        snare_roll(drm, t0 + BAR * 2, 2, 0.32);
        riser(busL, busR, t0 + BAR * 3, BAR - 0.02, 0.34);
        reverse_swell(busL, busR, t0, BAR * 4 - 0.02, 0.20);
    }

    // ── DROP 1 (16) ──
    cursor = START[sec_index("drop1")];
    for (int b = 0; b < 16; b++) {
        double t0 = (cursor + b) * BAR;
        break_bar(busL, busR, drm, t0, 1.0, 0.35);
        perc_groove(busL, busR, revL, revR, t0, 0.85, 0.018, 0.052, 0.38);
        push_kt(t0); push_kt(t0 + 3 * SX); push_kt(t0 + 10 * SX);
        int root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]);
        for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + q * BEAT, root, BEAT * 0.9, 0.30); sub_bass(busR, t0 + q * BEAT, root, BEAT * 0.9, 0.30); }
        int stab = FM_ROOTS[b % 4];
        hoover_stab(t0 + BEAT * 2, stab, BEAT * 2, 0.32, 1.0, 0.75);
        hoover_stab(t0 + BEAT * 2, stab + 7, BEAT * 2, 0.20, 0.6, 1.0);
        // 303 under
        int pent[8] = { 53, 56, 58, 60, 63, 60, 58, 56 };
        for (int e = 0; e < 16; e += 2) {
            int midi = pent[e % 8] - 12;
            place303_stereo(t0 + e * SX, midi, SX * 0.85, 0.14, e % 4 == 0, e % 4 == 0 ? 0.09 : 0, 7, 0.006);
        }
    }

    // ── BREAK A (8) — kick out, 303 acid ──
    cursor = START[sec_index("breakA")];
    for (int b = 0; b < 8; b++) {
        double t0 = (cursor + b) * BAR;
        perc_groove(busL, busR, revL, revR, t0, 0.55, 0.018, 0.045, 0.5);
        for (int e = 0; e < 16; e++) if (e % 4 == 2) tight_hat(busL, busR, t0 + e * SX, 1, 0.14, (e % 8 == 2 ? -0.1 : 0.1));
        snare_brk(drm, t0 + BEAT, 0.5); snare_brk(drm, t0 + BEAT * 3, 0.5);
        int pent[8] = { 53, 56, 58, 60, 63, 60, 58, 56 };
        for (int e = 0; e < 16; e++) {
            int midi = pent[e % 8] + (b % 2 == 0 ? 0 : -12);
            int accent = e % 4 == 0;
            place303_stereo(t0 + e * SX, midi, SX * 0.9, 0.26, accent, accent ? 0.16 : 0.08, accent ? 7 : 3, 0.005);
        }
        if (b == 0) dark_pad(busL, busR, t0, FM_PAD, 4, BAR * 4 - 0.05, 0.16);
        if (b == 4) dark_pad(busL, busR, t0, EB_PAD, 4, BAR * 4 - 0.05, 0.16);
    }
    riser(busL, busR, (cursor + 6) * BAR, BAR * 2 - 0.05, 0.38);

    // ── DROP 2 (16) ──
    cursor = START[sec_index("drop2")];
    for (int b = 0; b < 16; b++) {
        double t0 = (cursor + b) * BAR;
        break_bar(busL, busR, drm, t0, 1.0, 0.35);
        perc_groove(busL, busR, revL, revR, t0, 1.0, 0.018, 0.052, 0.38);
        push_kt(t0); push_kt(t0 + 3 * SX); push_kt(t0 + 10 * SX);
        int root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]);
        for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + q * BEAT, root, BEAT * 0.9, 0.32); sub_bass(busR, t0 + q * BEAT, root, BEAT * 0.9, 0.32); }
        int stab = FM_ROOTS[b % 4];
        hoover_stab(t0 + BEAT * 2, stab, BEAT * 2, 0.36, 1.0, 0.75);
        hoover_stab(t0 + BEAT * 2, stab + 7, BEAT * 2, 0.22, 0.6, 1.0);
        int pent[8] = { 53, 56, 58, 60, 63, 60, 58, 56 };
        for (int e = 0; e < 16; e += 2) { int midi = pent[e % 8] - 12; place303_stereo(t0 + e * SX, midi, SX * 0.85, 0.18, e % 4 == 0, e % 4 == 0 ? 0.10 : 0.06, e % 4 == 0 ? 7 : 3, 0.006); }
        if (b == 8 && bellS) { place(busL, busR, bellS, t0, 0.30, 1.0, 0.5, 1e9); place(revL, revR, bellS, t0, 0.20, 1.0, 0, 1e9); }
    }

    // ── BREAK B (8) — atmospheric ──
    cursor = START[sec_index("breakB")];
    {
        double t0 = cursor * BAR;
        dark_pad(busL, busR, t0, DB_PAD, 4, BAR * 4 - 0.05, 0.22);
        dark_pad(busL, busR, t0 + BAR * 4, C_PAD, 4, BAR * 4 - 0.05, 0.22);
        for (int b = 0; b < 8; b++) perc_groove(busL, busR, revL, revR, t0 + b * BAR, 0.35 + b * 0.04, 0.022, 0.04, 0.6);
        for (int b = 0; b < 8; b++) { int root = b < 4 ? 25 : 24; sub_bass(busL, t0 + b * BAR, root, BAR * 0.85, 0.26); sub_bass(busR, t0 + b * BAR, root, BAR * 0.85, 0.26); }
        if (bellS) { place(busL, busR, bellS, t0, 0.32, 1.0, 0.55, 1e9); place(revL, revR, bellS, t0, 0.24, 1.0, 0, 1e9); place(busL, busR, bellS, t0 + BAR * 4, 0.28, 1.05, 0.55, 1e9); place(revL, revR, bellS, t0 + BAR * 4, 0.22, 1.05, 0, 1e9); }
        reverse_swell(busL, busR, t0 + BAR * 4, BAR * 4 - 0.05, 0.30);
        snare_roll(drm, t0 + BAR * 6, 2, 0.30);
        riser(busL, busR, t0 + BAR * 7, BAR - 0.02, 0.40);
    }

    // ── DROP 3 (16) — biggest ──
    cursor = START[sec_index("drop3")];
    for (int b = 0; b < 16; b++) {
        double t0 = (cursor + b) * BAR;
        break_bar(busL, busR, drm, t0, 1.05, 0.35);
        perc_groove(busL, busR, revL, revR, t0, 1.15, 0.018, 0.06, 0.42);
        push_kt(t0); push_kt(t0 + 3 * SX); push_kt(t0 + 10 * SX);
        int root = 29 + (FM_ROOTS[b % 4] - FM_ROOTS[0]);
        for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + q * BEAT, root, BEAT * 0.9, 0.36); sub_bass(busR, t0 + q * BEAT, root, BEAT * 0.9, 0.36); }
        int stab = FM_ROOTS[b % 4];
        hoover_stab(t0 + BEAT * 2, stab, BEAT * 2, 0.40, 1.0, 0.72);
        hoover_stab(t0 + BEAT * 2, stab + 7, BEAT * 2, 0.24, 0.62, 1.0);
        hoover_stab(t0 + BEAT * 2, stab + 12, BEAT * 2, 0.18, 0.55, 0.55);
        if (b % 2 == 0) { int pent[5] = { 53, 56, 58, 60, 63 }; int midi = pent[b % 5] - 12; place303_stereo(t0 + BEAT * 2.5, midi, BEAT * 0.5, 0.22, 1, 0.12, 7, 0.006); }
    }

    // ── OUTRO (12) — decay ──
    cursor = START[sec_index("outro")];
    for (int b = 0; b < 12; b++) {
        double t0 = (cursor + b) * BAR;
        double energy = fmax(0.15, 1.0 - b * 0.075);
        break_bar(busL, busR, drm, t0, energy, 0.35);
        perc_groove(busL, busR, revL, revR, t0, energy, 0.018, 0.052, 0.45);
        push_kt(t0); push_kt(t0 + 3 * SX); push_kt(t0 + 10 * SX);
        int root = b < 6 ? 29 : 24;
        for (int q = 0; q < 4; q++) { sub_bass(busL, t0 + q * BEAT, root, BEAT * 0.9, 0.30 * energy); sub_bass(busR, t0 + q * BEAT, root, BEAT * 0.9, 0.30 * energy); }
    }
    {
        double t0 = cursor * BAR;
        dark_pad(busL, busR, t0, FM_PAD, 4, BAR * 6 - 0.05, 0.20);
        dark_pad(busL, busR, t0 + BAR * 6, C_PAD, 4, BAR * 6 - 0.05, 0.18);
        if (bellS) { place(busL, busR, bellS, t0 + BAR * 10, 0.45, 0.95, 0.6, 1e9); place(revL, revR, bellS, t0 + BAR * 10, 0.45, 0.95, 0, 1e9); }
    }

    // ════════════════ SAMPLED-INSTRUMENT LAYER ════════════════
    {
        // ambience bed
        Smp *rain = smpl("rain"), *jng = smpl("jungle"), *crk = smpl("cricket");
        if (rain) place(busL, busR, rain, 0, 0.10, 1, rnd2() * 0.3, 1e9);   // softer opener
        if (jng) place(busL, busR, jng, 0, 0.07, 1, rnd2() * 0.3, 1e9);
        if (crk) place(busL, busR, crk, 0.5, 0.06, 1, rnd2() * 0.3, 1e9);
        for (double t = 8; t < TB * BAR; t += 18) { if (rain) place(busL, busR, rain, t, 0.05, 1, rnd2() * 0.3, 1e9); if (crk && rnd() < 0.5) place(busL, busR, crk, t + 4, 0.04, 1, rnd2() * 0.3, 1e9); }
        if (rain) place(busL, busR, rain, (TB - 10) * BAR, 0.2, 1, rnd2() * 0.3, 1e9);
        if (crk) place(busL, busR, crk, (TB - 9) * BAR, 0.14, 1, rnd2() * 0.3, 1e9);
        if (jng) place(busL, busR, jng, (TB - 8) * BAR, 0.12, 1, rnd2() * 0.3, 1e9);

        // perc + hats throughout
        Smp *hatS = smpl("hat"), *ohatS = smpl("ohat"), *cow = smpl("cowbell"), *tomS = smpl("tom");
        double lag = 0.010 * LAZY;
        for (int bAbs = 0; bAbs < TB; bAbs++) {
            const char *sec = sec_of_bar(bAbs);
            int inDrop = !strncmp(sec, "drop", 4), quiet = !strcmp(sec, "intro") || !strcmp(sec, "outro");
            // staggered build — no perc until bar 6 of the intro (hats are the
            // last thing to enter before the build).
            if (!strcmp(sec, "intro") && bAbs < START[sec_index("intro")] + 6) continue;
            double dens = inDrop ? 1 : !strncmp(sec, "break", 5) ? 0.55 : quiet ? 0.3 : 0.7;
            for (int s = 0; s < 16; s++) {
                double sw = (s % 2 ? 0.020 * LAZY : 0);
                double t = bAbs * BAR + s * SX + lag + sw + hz();
                if (hatS && s % 2 == 1) place(busL, busR, hatS, t, (s % 4 == 3 ? 0.055 : 0.038) * dens, 1, 0.2, 0.05 * DEC);
                // clicks dropped — they were a thrashy chatter on the grid
            }
            if (ohatS && rnd() < 0.4 * dens) place(busL, busR, ohatS, bAbs * BAR + 6 * SX + lag + hz(), 0.05 * dens, 1, 0.25, 0.12 * DEC);
            // cowbell only on the & of 4, much sparser + softer (was a busy rattle)
            if (cow && inDrop && rnd() < 0.10 * dens) place(busL, busR, cow, bAbs * BAR + 14 * SX + lag + hz(), 0.045 * dens, 1, rnd2() * 0.4, 0.14 * DEC);
            // tom fill only every 8 bars, softer + swelled, as a gentle turnaround
            if (tomS && bAbs % 8 == 7 && !quiet) { int ss[3] = { 10, 12, 14 }; for (int k = 0; k < 3; k++) place_a(busL, busR, tomS, bAbs * BAR + ss[k] * SX + lag + hz(), 0.11 * dens, pow(2.0, -k / 6.0), (k - 1) * 0.3, 0.6 * DEC, 0.006); }
        }

        // drops: gong, taiko, flute bleeps, reverse-wolf riser, animal roar, choir
        const char *dropNames[3] = { "drop1", "drop2", "drop3" };
        Smp *gong = smpl("gong"), *taiko = smpl("taiko"), *lead = smpl("lead"), *animal = smpl("animal"), *choir = smpl("choir");
        for (int d = 0; d < 3; d++) {
            const char *name = dropNames[d]; int s0b = START[sec_index(name)]; int isD3 = !strcmp(name, "drop3");
            if (gong) { double t = s0b * BAR; place_a(busL, busR, gong, t, 0.26, 1, 0.5, 1.2 * DEC, 0.06); place_a(revL, revR, gong, t, 0.24, 1, 0, 1.2 * DEC, 0.06); }
            for (int b = 0; b < 16; b++) {
                double t0 = (s0b + b) * BAR; int root = FM_ROOTS[b % 4]; int phraseStart = b % 4 == 0;
                if (taiko) { place(busL, busR, taiko, t0, 0.42, 1, 0.15, 0.40 * DEC); place(busL, busR, taiko, t0 + 2 * BEAT, 0.32, 1, -0.15, 0.40 * DEC); }
                if (lead) {
                    int steps5[5] = { 0, 3, 6, 10, 13 }, steps3[3] = { 0, 6, 10 };
                    int *steps = isD3 ? steps5 : steps3, ns = isD3 ? 5 : 3;
                    // softer + fewer steps + tamed high partials (was a bright,
                    // thrashy bleep cluster); a gentle onset so they don't click.
                    double BLEEP[5][2] = { { 12, 0.5 }, { 15, 0.28 }, { 19, 0.3 }, { 24, 0.12 }, { 31, 0.06 } };
                    for (int si2 = 0; si2 < ns; si2++) {
                        int st = steps[si2]; double t = t0 + st * SX, g = (st == 0 ? 0.14 : 0.09);
                        for (int z = 0; z < 5; z++) place_a(busL, busR, lead, t, g * BLEEP[z][1], rate_nom(lead, root + BLEEP[z][0]), rnd2() * 0.55, 0.14 * DEC, 0.004);
                        place(revL, revR, lead, t, g * 0.3, rate_nom(lead, root + 12), 0, 0.16 * DEC);
                        if (animal_rev.ok && rnd() < 0.5) place(busL, busR, &animal_rev, t, 0.1, rate_nom(animal ? animal : &animal_rev, root + 12), rnd2() * 0.6, 0.12 * DEC);
                    }
                }
                if (wolf_rev.ok && phraseStart) {
                    // bigger pitch swing per phrase + an octave harmonic above —
                    // softer now, and it already swells (reversed), so it glides in.
                    double hold = isD3 ? 2.6 : 2.0; int up[4] = { 12, 19, 7, 24 }; int leadN = root + up[(b / 4) % 4];
                    place(busL, busR, &wolf_rev, t0 - hold, 0.20, rate_nom(&wolf_rev, leadN), 0.2, hold);
                    place(revL, revR, &wolf_rev, t0 - hold, 0.16, rate_nom(&wolf_rev, leadN), 0, hold);
                    place(busL, busR, &wolf_rev, t0 - hold, 0.09, rate_nom(&wolf_rev, leadN + 12), -0.25, hold);
                }
                if (animal) {
                    if (phraseStart) {
                        // bigger pitch swing per phrase + a 5th-harmonic layer, but
                        // SWELLED IN (long attack) and well softer so the roar reads
                        // as a deep cinematic rise, not a violent stab.
                        int down[4] = { -24, -31, -19, -28 }; int dn = down[(b / 4) % 4];
                        double rl = rate_nom(animal, root + dn), hold = isD3 ? 5.5 : 4.5;
                        place_a(busL, busR, animal, t0, 0.30, rl, 0.2, hold, 0.18);
                        place_a(revL, revR, animal, t0, 0.18, rl, 0, hold, 0.18);
                        place_a(busL, busR, animal, t0, 0.12, rate_nom(animal, root + dn + 7), -0.3, hold * 0.8, 0.18); // 5th harmonic
                        place_a(busL, busR, animal, t0, 0.16, rate_nom(animal, root), -0.2, 1.2 * DEC, 0.05);          // mid body
                    } else if (rnd() < 0.18) {
                        int up[3] = { 0, 7, 12 };
                        place_a(busL, busR, animal, t0 + 6 * SX, 0.22, rate_nom(animal, root + up[(int)(rnd() * 3)]), rnd2() * 0.4, 0.7 * DEC, 0.04);
                    }
                }
            }
            if (choir && isD3) for (int b = 0; b < 16; b += 2) { double t0 = (s0b + b) * BAR; int ns[3] = { FM_PAD[1], FM_PAD[2], FM_PAD[3] }; for (int z = 0; z < 3; z++) { place(busL, busR, choir, t0, 0.07, rate_nom(choir, ns[z]), 0.4, 1e9); place(revL, revR, choir, t0, 0.05, rate_nom(choir, ns[z]), 0, 1e9); } }
        }
        // choir over breaks
        if (choir) { const char *brk[2] = { "breakA", "breakB" }; for (int z2 = 0; z2 < 2; z2++) { int s0b = START[sec_index(brk[z2])]; for (int b = 0; b < 8; b += 2) { double t0 = (s0b + b) * BAR; int ns[3] = { FM_PAD[1], FM_PAD[2], FM_PAD[3] }; for (int z = 0; z < 3; z++) { place(busL, busR, choir, t0, 0.08, rate_nom(choir, ns[z]), 0.45, 1e9); place(revL, revR, choir, t0, 0.06, rate_nom(choir, ns[z]), 0, 1e9); } } } }

        // pirate-shanty opener (flute + faint square ghost)
        {
            int s0b = START[sec_index("intro")];
            double SHANTY[23][3] = {
                {0,65,0.5},{0.5,65,0.5},{1,68,0.5},{1.5,72,0.5},{2,72,1},{3,70,1},
                {4,68,0.5},{4.5,68,0.5},{5,67,0.5},{5.5,65,0.5},{6,63,1.5},{7.5,65,0.5},
                {8,65,0.5},{8.5,68,0.5},{9,72,0.5},{9.5,75,0.5},{10,73,1},{11,72,1},
                {12,68,0.5},{12.5,67,0.5},{13,65,0.5},{13.5,63,0.5},{14,65,2} };
            for (int z = 0; z < 23; z++) {
                double bt = SHANTY[z][0], midi = SHANTY[z][1], dur = SHANTY[z][2];
                double t = ((s0b + 2) * BAR) + bt * 2 * BEAT + 0.012 * LAZY + hz(); // shanty enters bar 2
                double m = midi - 12, ring = fmax(1.1, dur * 2 * BEAT);
                if (lead) { place(busL, busR, lead, t, 0.18, rate_nom(lead, m), 0.25, ring * 0.95); place(revL, revR, lead, t, 0.14, rate_nom(lead, m), 0, ring); }
                square_note(busL, busR, m, t, dur * 2 * BEAT * 0.9, 0.035, rnd2() * 0.25);
            }
            if (lead) { int bo[2] = { 2, 6 }; for (int z = 0; z < 2; z++) { double t = (s0b + bo[z]) * BAR; place(busL, busR, lead, t, 0.10, rate_nom(lead, 41), 0.1, 3.0); place(revL, revR, lead, t, 0.07, rate_nom(lead, 41), 0, 3.0); } }
        }

        // john williams strings
        Smp *strings = smpl("strings");
        if (strings) {
            // strings stay OUT of the intro (the staggered build is pad→flute→
            // sub→hats); they re-enter from breakA onward.
            const char *segN[5] = { "breakA", "drop2", "breakB", "drop3", "outro" };
            int segB[5] = { 8, 16, 8, 16, 12 };
            int chord[3] = { 41, 48, 56 };
            for (int z = 0; z < 5; z++) { int s0b = START[sec_index(segN[z])]; for (int b = 0; b < segB[z]; b += 4) { double t0 = (s0b + b) * BAR, g = !strncmp(segN[z], "drop", 4) ? 0.13 : 0.16; for (int c = 0; c < 3; c++) { place(busL, busR, strings, t0, g, rate_nom(strings, chord[c]), 0.5, 6.0); place(revL, revR, strings, t0, g * 0.5, rate_nom(strings, chord[c]), 0, 6.0); } } }
        }

        // sampled flute lead — the main voice over the drops
        if (lead) {
            double LEAD[8][3] = { {0,0,65},{0,2,72},{1,0,75},{1,1,72},{1,2,68},{2,0,65},{2,2,68},{3,0,72} };
            double HARM[4][2] = { {0,0.5},{12,0.34},{7,0.26},{3,0.2} };
            for (int d = 0; d < 3; d++) {
                const char *name = dropNames[d]; int s0b = START[sec_index(name)]; double gain = !strcmp(name, "drop3") ? 0.34 : 0.28;
                for (int phr = 0; phr < 4; phr++) for (int z = 0; z < 8; z++) {
                    double bo = LEAD[z][0], beatO = LEAD[z][1], note = LEAD[z][2];
                    double t = (s0b + phr * 4 + bo) * BAR + beatO * BEAT, pan = rnd2() * 0.3;
                    for (int h = 0; h < 4; h++) { double jt = rnd2() * 0.012, jp = rnd2() * 0.02; place(busL, busR, lead, t + jt, gain * HARM[h][1], rate_nom(lead, note + HARM[h][0]) * (1 + jp / 12 * 0.05), pan, 0.6 * DEC); place(revL, revR, lead, t + jt, gain * HARM[h][1] * 0.3, rate_nom(lead, note + HARM[h][0]), 0, 0.6 * DEC); }
                }
            }
        }
    }

    // ════════════════ SHOUTS (un-ducked vocal bus) ════════════════
    {
        Smp shJ[N_SHOUTS], shA[N_SHOUTS];
        for (int i = 0; i < N_SHOUTS; i++) {
            shJ[i].ok = shA[i].ok = 0;
            if (SHOUT_J[i][0]) { long n; int sr; float *s = load_wav(SHOUT_J[i], &n, &sr); if (s) { double pk = 0; for (long j = 0; j < n; j++) pk = fmax(pk, fabs(s[j])); double g = pk > 0 ? 0.97 / pk : 1; for (long j = 0; j < n; j++) s[j] *= g; shJ[i] = (Smp){ s, n, sr, 60, 60, 1 }; } }
            if (SHOUT_A[i][0]) { long n; int sr; float *s = load_wav(SHOUT_A[i], &n, &sr); if (s) { double pk = 0; for (long j = 0; j < n; j++) pk = fmax(pk, fabs(s[j])); double g = pk > 0 ? 0.97 / pk : 1; for (long j = 0; j < n; j++) s[j] *= g; shA[i] = (Smp){ s, n, sr, 60, 60, 1 }; } }
        }
        // key order matches SHOUT_KEY: wake real hold let rabbit now
        #define KI(k) ( !strcmp(k,"wake")?0 : !strcmp(k,"real")?1 : !strcmp(k,"hold")?2 : !strcmp(k,"let")?3 : !strcmp(k,"rabbit")?4 : 5 )
        // sing(key, barAbs, gain, pan)
        const char *seqA[8] = { "wake", "real", "hold", "let", "wake", "real", "hold", "let" };
        const char *seqB[8] = { "rabbit", "now", "rabbit", "now", "rabbit", "now", "rabbit", "now" };
        const char *dnames[2] = { "drop1", "drop3" };
        for (int d = 0; d < 2; d++) {
            int s0b = START[sec_index(dnames[d])];
            for (int i = 0; i < 8; i++) {
                int ki = KI(seqA[i]); double t = (s0b + i * 2) * BAR, g = 1.05, pan = (i % 2 ? 0.12 : -0.12);
                if (shJ[ki].ok) { place(vocL, vocR, &shJ[ki], t, g, 1, pan, 1e9); place(revL, revR, &shJ[ki], t, g * 0.18, 1, 0, 1e9); }
                if (shA[ki].ok) { place(vocL, vocR, &shA[ki], t, g * 0.32, 1, -pan - 0.2, 1e9); place(revL, revR, &shA[ki], t, g * 0.14, 1, 0, 1e9); }
            }
        }
        { int s0b = START[sec_index("drop2")]; for (int i = 0; i < 8; i++) { int ki = KI(seqB[i]); double t = (s0b + i * 2) * BAR, g = 1.05, pan = (i % 2 ? 0.12 : -0.12); if (shJ[ki].ok) { place(vocL, vocR, &shJ[ki], t, g, 1, pan, 1e9); place(revL, revR, &shJ[ki], t, g * 0.18, 1, 0, 1e9); } if (shA[ki].ok) { place(vocL, vocR, &shA[ki], t, g * 0.32, 1, -pan - 0.2, 1e9); place(revL, revR, &shA[ki], t, g * 0.14, 1, 0, 1e9); } } }
    }

    // ════════════════ REVERB (Schroeder) → bus ════════════════
    {
        // tighter space — shorter, drier, darker tail than the JS (.80/.55/.42)
        double decay = 0.66, wet = 0.38, damp = 0.52;
        int CD[6]; double cds[6] = { 0.0297, 0.0371, 0.0411, 0.0437, 0.0497, 0.0581 };
        for (int k = 0; k < 6; k++) CD[k] = (int)(cds[k] * SR);
        int AD[2] = { (int)(0.005 * SR), (int)(0.0017 * SR) }; double apFb = 0.5;
        float *cbL[6], *cbR[6]; int ciL[6] = {0}, ciR[6] = {0}; double cLPL[6] = {0}, cLPR[6] = {0};
        for (int k = 0; k < 6; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        float *abL[2], *abR[2]; int aiL[2] = {0}, aiR[2] = {0};
        for (int k = 0; k < 2; k++) { abL[k] = calloc(AD[k], 4); abR[k] = calloc(AD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 6; k++) {
                double dL = cbL[k][ciL[k]], dR = cbR[k][ciR[k]]; cL += dL; cR += dR;
                cLPL[k] = dL * (1 - damp) + cLPL[k] * damp; cLPR[k] = dR * (1 - damp) + cLPR[k] * damp;
                cbL[k][ciL[k]] = (float)(inL + cLPL[k] * decay); cbR[k][ciR[k]] = (float)(inR + cLPR[k] * decay);
                ciL[k] = (ciL[k] + 1) % CD[k]; ciR[k] = (ciR[k] + 1) % CD[k];
            }
            cL /= 6; cR /= 6;
            for (int k = 0; k < 2; k++) {
                double dL = abL[k][aiL[k]], dR = abR[k][aiR[k]];
                double oL = -apFb * cL + dL, oR = -apFb * cR + dR;
                abL[k][aiL[k]] = (float)(cL + apFb * oL); abR[k][aiR[k]] = (float)(cR + apFb * oR);
                aiL[k] = (aiL[k] + 1) % AD[k]; aiR[k] = (aiR[k] + 1) % AD[k];
                cL = oL; cR = oR;
            }
            busL[i] += (float)(cL * wet); busR[i] += (float)(cR * wet);
        }
        for (int k = 0; k < 6; k++) { free(cbL[k]); free(cbR[k]); }
        for (int k = 0; k < 2; k++) { free(abL[k]); free(abR[k]); }
    }

    // ════════════════ SIDECHAIN DUCK ════════════════
    float *duck = malloc(N * 4); for (long i = 0; i < N; i++) duck[i] = 1;
    for (int k = 0; k < n_kt; k++) { long s0 = (long)(kick_times[k] * SR), len = (long)(0.14 * SR); for (long i = 0; i < len && s0 + i < N; i++) { double w = (double)i / len, d = 0.55 + 0.45 * w; if (d < duck[s0 + i]) duck[s0 + i] = (float)d; } }

    // ════════════════ CRUNCH + SVF SWEEP + vocals ════════════════
    long MAXOUT = N + (long)(30 * SR) + 16;
    float *outL = calloc(MAXOUT, 4), *outR = calloc(MAXOUT, 4);
    {
        // smooth: near-clean drive + gentle wobble (JS was CRUNCH 1 / WOB .4)
        double CRUNCH = 0.15, WOB = 0.2, wobHz = (BPMV / 60) / 2, Q = 0.8, qc = 1 / Q, fcMax = SR * 0.16;
        // slow build — the opening swell now ramps across intro + build + the
        // first 4 bars of drop 1, on a steeper curve, so the track eases in.
        double swellT = (SECBARS[0] + SECBARS[1] + 4) * BAR;
        // vocals come in SLOW — the shouts first land at drop 1; ease them up
        // from silence across the first 6 bars so they don't slam in.
        double vocStart = START[sec_index("drop1")] * BAR, vocFadeLen = 6 * BAR;
        struct { const char *k; double s, e; } segs[8];
        { double c = 0; for (int i = 0; i < 8; i++) { segs[i].k = ORDER[i]; segs[i].s = c * BAR; segs[i].e = (c + SECBARS[i]) * BAR; c += SECBARS[i]; } }
        double lowL = 0, bandL = 0, lowR = 0, bandR = 0; int si = 0;
        for (long i = 0; i < N; i++) {
            double t = (double)i / SR;
            while (si < 7 && t >= segs[si].e) si++;
            double p = (t - segs[si].s) / fmax(1e-6, segs[si].e - segs[si].s), cut;
            const char *k = segs[si].k;
            if (!strncmp(k, "drop", 4)) { double lfo = 0.5 + 0.5 * sin(TAU * wobHz * t); cut = 13000 - WOB * 6000 * lfo; }
            else if (!strncmp(k, "break", 5)) cut = 9000 - 8500 * sin(M_PI * p);
            else if (!strcmp(k, "intro")) cut = 6000;
            else if (!strcmp(k, "buildA")) cut = 1200 * pow(24000.0 / 1200, p);
            else cut = 12000 - 9500 * p;
            cut = fmax(220, fmin(fcMax, cut));
            double f = 2 * sin(M_PI * cut / SR);
            // bigger grooves — the dry kick/snare bus drives harder into the
            // crunch (more weight + saturation) than the JS 0.96.
            double mL = (busL[i] * duck[i] + drm[i] * 1.18) * 1.05;
            double mR = (busR[i] * duck[i] + drm[i] * 1.18) * 1.05;
            // crunch: drive → asym fold → mild bitcrush
            double yL = tanh(mL * (1.1 + 1.0 * CRUNCH)); yL = yL - 0.10 * CRUNCH * yL * yL * yL; yL = round(yL * 512) / 512;
            double yR = tanh(mR * (1.1 + 1.0 * CRUNCH)); yR = yR - 0.10 * CRUNCH * yR * yR * yR; yR = round(yR * 512) / 512;
            double hL = yL - lowL - qc * bandL; bandL += f * hL; lowL += f * bandL;
            double hR = yR - lowR - qc * bandR; bandR += f * hR; lowR += f * bandR;
            double swell = t < swellT ? 0.02 + 0.98 * pow(t / swellT, 3.6) : 1;
            // vocals smoother + softer — lower level and a shallower pump (the
            // JS slammed them at 1.5 with a deep 0.74+0.26 duck).
            double vduck = 0.88 + 0.12 * duck[i];
            double vfade = t < vocStart ? 0 : t < vocStart + vocFadeLen ? 0.5 - 0.5 * cos(M_PI * (t - vocStart) / vocFadeLen) : 1;
            outL[i] = (float)(lowL * swell + vocL[i] * 1.02 * vduck * vfade);
            outR[i] = (float)(lowR * swell + vocR[i] * 1.02 * vduck * vfade);
        }
    }

    // ════════════════ HEALING DRONE ════════════════
    {
        double durS = (double)N / SR;
        double Hf[4] = { 174.61, 261.63, 392.00, 523.25 }, Hpr[4] = { 0.013, 0.017, 0.011, 0.019 }, Hpd[4] = { 2.0, 1.5, 3.0, 2.0 }, Hpanr[4] = { 0.05, 0.07, 0.09, 0.06 }, Hampr[4] = { 0.06, 0.04, 0.05, 0.07 };
        double ph[4] = {0};
        for (long i = 0; i < N; i++) {
            double t = (double)i / SR, fade = fmin(1, t / 6) * fmin(1, (durS - t) / 6);
            for (int k = 0; k < 4; k++) {
                double f = Hf[k] * pow(2.0, (Hpd[k] / 12) * sin(TAU * Hpr[k] * t + k));
                ph[k] += TAU * f / SR;
                double amp = 0.019 * (0.5 + 0.5 * sin(TAU * Hampr[k] * t + k * 1.7)) * fade;
                double pan = sin(TAU * Hpanr[k] * t + k * 2.1), v = sin(ph[k]) * amp;
                outL[i] += (float)(v * (0.5 - pan * 0.5)); outR[i] += (float)(v * (0.5 + pan * 0.5));
            }
        }
    }

    // ════════════════ SINE TINGE DROPLETS ════════════════
    {
        double dropF[5] = { 523.25, 587.33, 659.25, 783.99, 880.0 };
        for (int b = 4; b < TB; b++) {
            if (rnd() < 0.55) continue;
            double t0 = b * BAR + rnd() * BAR, f0 = dropF[(int)(rnd() * 5)];
            long n = (long)(0.4 * SR), s0 = (long)(t0 * SR); double pan = rnd2(), p = 0;
            for (long i = 0; i < n; i++) { double t = (double)i / SR, f = f0 * pow(2.0, -1.6 * t); p += TAU * f / SR; double v = sin(p) * exp(-t * 7) * 0.085; if (s0 + i < N) { outL[s0 + i] += (float)(v * (0.5 - pan * 0.5)); outR[s0 + i] += (float)(v * (0.5 + pan * 0.5)); } }
        }
    }

    // ════════════════ PAYPHONE INTRO (opt-in via --payphone) ════════════════
    if (g_payphone) {
        double PHONE = 6.0, XF = 1.6; long endI = (long)((PHONE + XF) * SR);
        double s1L = 0, s2L = 0, s1R = 0, s2R = 0;
        double aLo = 1 - exp(-2 * M_PI * 400 / SR), aHi = 1 - exp(-2 * M_PI * 3000 / SR);
        for (long i = 0; i < endI && i < N; i++) {
            double t = (double)i / SR, xL = outL[i], xR = outR[i];
            s1L += aLo * (xL - s1L); double bL = xL - s1L; s2L += aHi * (bL - s2L); bL = s2L;
            s1R += aLo * (xR - s1R); double bR = xR - s1R; s2R += aHi * (bR - s2R); bR = s2R;
            double wetL = (round(bL * 28) / 28) * 0.42, wetR = (round(bR * 28) / 28) * 0.42;
            double mix = t < PHONE ? 0 : (t - PHONE) / XF;
            outL[i] = (float)(wetL * (1 - mix) + xL * mix); outR[i] = (float)(wetR * (1 - mix) + xR * mix);
        }
    }

    // normalize headroom
    { double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(outL[i]), fabs(outR[i])); if (a > peak) peak = a; } if (peak > 0) { double g = 0.78 / peak; for (long i = 0; i < N; i++) { outL[i] *= (float)g; outR[i] *= (float)g; } } }

    // ════════════════ TAPE-STOP ENDING ════════════════
    long outN = N;
    {
        double TAPE = 24.0; long ts0 = (long)((TB * BAR - TAPE) * SR); if (ts0 < 0) ts0 = 0;
        long srcLen = N - ts0; long maxTail = 2 * srcLen + 8;
        float *tailL = malloc(maxTail * 4), *tailR = malloc(maxTail * 4); long tn = 0;
        double pos = 0;
        while (ts0 + pos < N - 1 && tn < maxTail) {
            double prog = pos / srcLen, rate = fmax(0.5, 1 - prog * prog * 0.5);
            double sp = ts0 + pos; long j = (long)sp; double fr = sp - j;
            tailL[tn] = (float)(outL[j] * (1 - fr) + outL[j + 1] * fr);
            tailR[tn] = (float)(outR[j] * (1 - fr) + outR[j + 1] * fr);
            tn++; pos += rate;
        }
        outN = ts0 + tn;
        if (outN > MAXOUT) outN = MAXOUT;
        for (long i = 0; i < tn && ts0 + i < MAXOUT; i++) { outL[ts0 + i] = tailL[i]; outR[ts0 + i] = tailR[i]; }
        free(tailL); free(tailR);
    }

    // ════════════════ TRIM ════════════════
    long tailEnd = outN - 1;
    while (tailEnd > 0 && fabs(outL[tailEnd]) < 0.0003 && fabs(outR[tailEnd]) < 0.0003) tailEnd--;
    long trimN = outN < tailEnd + (long)SR ? outN : tailEnd + (long)SR;

    // ════════════════ PHONE-WORLD LOOP TAIL (opt-in) + FADE ════════════════
    {
        if (g_payphone) {
            double PH = 6.0; long ph0 = trimN - (long)(PH * SR); if (ph0 < 0) ph0 = 0;
            double s1L = 0, s2L = 0, s1R = 0, s2R = 0;
            double aLo = 1 - exp(-2 * M_PI * 400 / SR), aHi = 1 - exp(-2 * M_PI * 3000 / SR);
            for (long i = ph0; i < trimN; i++) {
                double xL = outL[i], xR = outR[i];
                s1L += aLo * (xL - s1L); double bL = xL - s1L; s2L += aHi * (bL - s2L); bL = s2L;
                s1R += aLo * (xR - s1R); double bR = xR - s1R; s2R += aHi * (bR - s2R); bR = s2R;
                double mix = (double)(i - ph0) / (trimN - ph0);
                outL[i] = (float)(xL * (1 - mix) + (round(bL * 28) / 28) * 0.42 * mix);
                outR[i] = (float)(xR * (1 - mix) + (round(bR * 28) / 28) * 0.42 * mix);
            }
        }
        long fadeN = trimN < (long)(1.2 * SR) ? trimN : (long)(1.2 * SR);
        for (long i = 0; i < fadeN; i++) { double g = cos((M_PI / 2) * ((double)i / fadeN)); long idx = trimN - fadeN + i; outL[idx] *= (float)g; outR[idx] *= (float)g; }
    }

    if (!write_wav_f32_stereo(out_path, outL, outR, trimN)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)trimN / SR);
    return 0;
}
