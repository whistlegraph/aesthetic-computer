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
static float *zmL, *zmR; // stereo zoom-fx bus — jeffrey fly-bys with real pans

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

// stereo twin — interleaved L/R float32. the release master is stereo;
// the mono writer stays for the --stem-voc analysis path.
static int write_wav_f32_st(const char *path, const float *l, const float *r, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); uint32_t fsz = 16; fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&l[i], 4, 1, f); fwrite(&r[i], 4, 1, f); }
    fclose(f);
    return 1;
}

// ── constant-power pan gains: p in [-1 (hard L) .. +1 (hard R)] ─────────
static inline void pan_gains(double p, double *gl, double *gr) {
    double a = (p + 1.0) * M_PI / 4.0;
    *gl = cos(a); *gr = sin(a);
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
static int classic_drums = 1; // default kit; --novel-drums swaps in the novelizer voices

static void kick_classic(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize
    int n = (int)(0.18 * SR); long s0 = (long)(t * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = 55 + (110 - 55) * exp(-tt * 38); // lower sweep start — less pitchy
        ph += TAU * f / SR;
        double body = sin(ph) * exp(-tt * 11);
        double click = i < SR * 0.002 ? rnd2() * 0.4 * (1 - i / (SR * 0.002)) : 0;
        addb(buf, s0 + i, (body + click) * g);
    }
}

static void kick_serious_classic(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize
    int n = (int)(0.32 * SR); long s0 = (long)(t * SR);
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = 42 + (150 - 42) * exp(-tt * 30); // lower sweep start — less pitchy
        ph += TAU * f / SR;
        double body = tanh(sin(ph) * 2.4) * exp(-tt * 7.5);
        double click = i < SR * 0.003 ? rnd2() * 0.6 * (1 - i / (SR * 0.003)) : 0;
        addb(buf, s0 + i, (body + click) * g);
    }
}

static void snare_classic(float *buf, double t, double g) {
    t = hum_t(t, 9); g = hum_g(g, 0.12); // humanize
    int n = (int)(0.22 * SR); long s0 = (long)(t * SR);
    double prev = 0, ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        ph += TAU * 165 / SR;
        double body = sin(ph) * exp(-tt * 30) * 0.8; // deeper, meatier body
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

// ── novelizer percussion (pop/novelizer batch 2, via pop/novelette) ─────
// three keepers replace the stock kit: memkick (tension-modulated Bessel
// membrane — the pitch drop is emergent from strike energy), cavikick
// (Helmholtz port + orifice loss — the AIR side of a drum, flow-gated
// chuff), gransnare (stochastic pulsaret cloud — the tail granulates
// instead of hissing). each hit reseeds deterministically.

static inline double nv_hash01(unsigned s) {
    s ^= s >> 16; s *= 0x7feb352du; s ^= s >> 15; s *= 0x846ca68bu; s ^= s >> 16;
    return (double)s / 4294967296.0;
}
static uint32_t nv_hit_counter = 0;

static void memkick_hit(float *buf, double t, double f0, double vel, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize like the classic kit
    if (vel > 1.0) vel = 1.0; if (vel < 0.15) vel = 0.15;
    long s0 = (long)(t * SR);
    unsigned seed = (++nv_hit_counter) * 2654435761u ^ (unsigned)(f0 * 64.0);

    static const double RATIO[6] = { 1.0, 1.594, 2.136, 2.296, 2.653, 2.918 };
    const double GAMMA = 6.0, CLICK_TAU = 0.0012, CLICK_LEN = 0.012;
    double reg = pow(65.406 / f0, 0.25);
    if (reg > 1.6) reg = 1.6; if (reg < 0.25) reg = 0.25;
    double tau0 = (0.060 - 0.026 * vel) * reg;
    double bright = 2.2 - 1.0 * vel;

    double w[6], tau[6], det[6], ph[6], s0w = 0.0;
    for (int k = 0; k < 6; k++) {
        w[k] = pow(RATIO[k], -bright);
        tau[k] = tau0 / (1.0 + 0.9 * (RATIO[k] - 1.0));
        det[k] = 1.0 + 0.004 * (nv_hash01(seed + 7u * (unsigned)k) - 0.5);
        ph[k] = 0.0;
        s0w += w[k] * w[k];
    }
    double tstrike = 0.0032 - 0.0016 * vel;
    double amp = 0.9 * pow(vel, 1.4);
    double fc = 22.0 * f0;
    if (fc < 900.0) fc = 900.0; if (fc > 3800.0) fc = 3800.0;
    double f1 = 2.0 * sin(M_PI * fc / SR);
    double svf_lp = 0.0, svf_bp = 0.0;
    double click_amp = 0.11 * pow(vel, 1.6);
    uint32_t r = seed ^ 0x9e3779b9u;

    long len = (long)(6.9 * tau0 * SR) + (long)(0.06 * SR);
    for (long i = 0; i < len; i++) {
        double tt = (double)i / SR;
        double s = (tt < tstrike) ? 0.5 * (1.0 - cos(M_PI * tt / tstrike)) : 1.0;
        double ek[6], E = 0.0;
        for (int k = 0; k < 6; k++) { ek[k] = w[k] * s * exp(-tt / tau[k]); E += ek[k] * ek[k]; }
        E *= vel * vel / s0w;
        double gg = sqrt(1.0 + GAMMA * E);
        double y = 0.0;
        for (int k = 0; k < 6; k++) {
            y += ek[k] * sin(TAU * ph[k]);
            ph[k] += RATIO[k] * det[k] * f0 * gg / SR;
            if (ph[k] >= 1.0) ph[k] -= 1.0;
        }
        if (tt < CLICK_LEN) {
            double ce = (tt < 0.0003) ? tt / 0.0003 : exp(-(tt - 0.0003) / CLICK_TAU);
            r = r * 1664525u + 1013904223u;
            double nzv = ((double)(r >> 9) / 4194304.0) - 1.0;
            svf_lp += f1 * svf_bp;
            double hp = nzv - svf_lp - 0.8 * svf_bp;
            svf_bp += f1 * hp;
            y += click_amp * ce * svf_bp;
        }
        addb(buf, s0 + i, amp * y * g);
    }
}

static void cavikick_hit(float *buf, double t, double freq, double vel, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.09); // humanize
    if (vel > 1.0) vel = 1.0; if (vel < 0.15) vel = 0.15;
    const double dt = 1.0 / SR;
    long s0 = (long)(t * SR);
    long len = (long)(0.85 * SR);
    long fade = (long)(0.010 * SR);

    unsigned h = (++nv_hit_counter) * 2654435761u ^ (unsigned)(freq * 64.0);
    uint32_t r = h | 1u;
    double detune = 1.0 + (((h >> 8) & 0xffff) / 65535.0 - 0.5) * 0.006;
    double jit = (((h >> 20) & 0xff) / 255.0 - 0.5) * 0.10;

    const double D_LIN = 2.0 * 6.9078 / 0.30, D_NL = 105.0;
    double wt = TAU * freq * detune;
    double a = 0.5 * sqrt(wt * wt + 0.25 * D_LIN * D_LIN) * dt;
    if (a > 1.5) a = 1.5;
    double w0 = 2.0 * sin(a) / dt, w02 = w0 * w0;
    double tau_r = 0.0007, tau_f = (0.006 - 0.0025 * vel) * (1.0 + jit);
    double P = pow(vel, 1.35) * wt * wt;
    double fk = freq * 6.27;
    int knock_on = fk < 18000.0;
    double ak = 0.5 * TAU * fk * dt;
    if (ak > 1.5) ak = 1.5;
    double wk = knock_on ? 2.0 * sin(ak) / dt : 1.0;
    double wk2 = wk * wk, dkn = 2.0 * 6.9078 / 0.055;
    double klp = 1.0 - exp(-TAU * 1250.0 * dt), khp = 1.0 - exp(-TAU * 250.0 * dt);
    double lp1 = 0, lp2 = 0, hpt = 0;
    double x = 0, v = 0, xk = 0, vk = 0;

    for (long i = 0; i < len; i++) {
        double tt = i * dt;
        double F = P * (1.0 - exp(-tt / tau_r)) * exp(-tt / tau_f);
        double u = v / w0, au = fabs(u);
        double d = D_LIN + D_NL * (au > 1.4 ? 1.4 : au);
        v *= exp(-d * dt);
        v += dt * (-w02 * x + F);
        x += dt * v;
        double y = v / w0;
        if (knock_on) {
            vk *= exp(-dkn * dt);
            vk += dt * (-wk2 * xk + F);
            xk += dt * vk;
            y += 0.13 * (vk / wk);
        }
        r ^= r << 13; r ^= r >> 17; r ^= r << 5;
        double wnz = ((double)(r >> 8) / 8388608.0) - 1.0;
        lp1 += klp * (wnz - lp1);
        lp2 += klp * (lp1 - lp2);
        hpt += khp * (lp2 - hpt);
        double gate = u * u;
        if (gate > 1.5) gate = 1.5;
        y += 0.55 * (lp2 - hpt) * gate * exp(-tt / 0.070);
        if (i >= len - fade) y *= (double)(len - i) / fade;
        addb(buf, s0 + i, 0.9 * y * g);
    }
}

static inline double gs_win(double x) {
    const double att = 0.12, tail = 0.10, k = 5.0;
    if (x <= 0.0 || x >= 1.0) return 0.0;
    double w = (x < att) ? 0.5 - 0.5 * cos(M_PI * x / att)
                         : exp(-k * (x - att) / (1.0 - att));
    if (x > 1.0 - tail) w *= 0.5 + 0.5 * cos(M_PI * (x - (1.0 - tail)) / tail);
    return w;
}

static void gransnare_hit(float *buf, double t, double fbody, double vel, double g) {
    t = hum_t(t, 9); g = hum_g(g, 0.12); // humanize
    if (vel > 1.0) vel = 1.0; if (vel < 0.15) vel = 0.15;
    while (fbody < 120.0) fbody *= 2.0;
    long s0 = (long)(t * SR);
    uint32_t r = (++nv_hit_counter) * 0x9E3779B9u ^ 0x5F3759DFu;
    if (!r) r = 0xACu;
    #define GS_NEXT() (r ^= r << 13, r ^= r >> 17, r ^= r << 5, r)
    #define GS_FRAND() ((double)(GS_NEXT() >> 8) * (1.0 / 16777216.0))
    // sequenced two-draw triangle (a bare a+b of two GS_FRANDs has
    // unspecified evaluation order — the draws must happen left-to-right)
    double gs_t1, gs_t2;
    #define GS_TRI() (gs_t1 = GS_FRAND(), gs_t2 = GS_FRAND(), gs_t1 + gs_t2 - 1.0)

    double hitdur = 0.16 + 0.24 * vel;
    int ngrains = 50 + (int)(250.0 * pow(vel, 1.6));
    int nburst = 6 + (int)(30.0 * vel);
    double tau = 0.25 * hitdur;
    double bright_tau = 0.030 + 0.020 * vel;
    double fmax = 2200.0 + 6800.0 * pow(vel, 1.2);
    double amp_tau = 0.35 * hitdur;
    double hit_amp = 0.40 * pow(vel, 1.35);
    double trunc = 1.0 - exp(-hitdur / tau);

    for (int gi = 0; gi < ngrains; gi++) {
        double onset = (gi < nburst) ? GS_FRAND() * 0.002
                                     : -tau * log(1.0 - GS_FRAND() * trunc);
        double br = exp(-onset / bright_tau);
        double fw, phase, cycles, amp;
        int is_body = GS_FRAND() < pow(1.0 - br, 1.2);
        if (is_body) {
            fw = fbody * (GS_FRAND() < 0.30 ? 1.58 : 1.0);
            fw *= 1.0 + 0.004 * GS_TRI();
            phase = fw * onset;
            phase -= floor(phase);
            cycles = 3.0 + 1.5 * GS_FRAND();
            amp = 1.4;
        } else {
            double fcg = fbody * pow(fmax / fbody, br);
            double oct = GS_TRI() * (1.6 * br + 0.06);
            fw = fcg * pow(2.0, oct);
            if (fw < fbody * 0.8) fw = fbody * 0.8;
            if (fw > 16000.0) fw = 16000.0;
            phase = GS_FRAND();
            cycles = 6.0 + 4.0 * GS_FRAND();
            amp = 1.0;
        }
        double glen = cycles / fw;
        double lo = is_body ? 0.004 : 0.0015, hi = is_body ? 0.030 : 0.008;
        if (glen < lo) glen = lo;
        if (glen > hi) glen = hi;
        amp *= exp(-onset / amp_tau) * (0.75 + 0.5 * GS_FRAND()) * hit_amp;

        long gs0 = s0 + (long)(onset * SR);
        long glen_s = (long)(glen * SR);
        if (glen_s < 8) glen_s = 8;
        double inc = fw / SR, ph = phase;
        for (long i = 0; i < glen_s; i++) {
            double xw = (double)(i + 1) / (double)(glen_s + 1);
            addb(buf, gs0 + i, gs_win(xw) * sin(TAU * ph) * amp * g);
            ph += inc;
            if (ph >= 1.0) ph -= 1.0;
        }
    }
    #undef GS_TRI
    #undef GS_FRAND
    #undef GS_NEXT
}

// ── kit dispatch: novelizer voices by default, --classic-drums reverts ──
static void kick(float *buf, double t, double g) {
    if (classic_drums) { kick_classic(buf, t, g); return; }
    cavikick_hit(buf, t, 55.0, g, g * 1.6);
}
static void kick_serious(float *buf, double t, double g) {
    if (classic_drums) { kick_serious_classic(buf, t, g); return; }
    memkick_hit(buf, t, 43.65, 0.9 * g, g * 1.5);
}
static void snare_hit(float *buf, double t, double g) {
    if (classic_drums) { snare_classic(buf, t, g); return; }
    gransnare_hit(buf, t, 190.0, g, g * 2.2);
}

// the original soft tambourine — kept for the quiet bookends (intro +
// outro fade) so the opener stays subtle while the body gets the crunch.
static void tamb_soft(float *buf, double t, double g) {
    t = hum_t(t, 6); g = hum_g(g, 0.18); // humanize
    int n = (int)(0.05 * SR); long s0 = (long)(t * SR);
    double prev = 0;
    for (int i = 0; i < n; i++) {
        double nz = rnd2();
        double hp = nz - prev; prev = nz;
        addb(buf, s0 + i, hp * exp(-((double)i / SR) * 80) * g);
    }
}

static double gTotalSec = 1; // set in main — lets voices ride song position

static void tamb(float *buf, double t, double g) {
    // crunchy hat that EVOLVES with the song: early hits sit darker and
    // looser, late hits get brighter (hold rate up = pitch up), tighter
    // (faster decay = harder attack) and more driven. sample-held noise,
    // first-diff highpass, soft clip.
    double prog = t / gTotalSec; if (prog > 1) prog = 1;
    t = hum_t(t, 6); g = hum_g(g, 0.18); // humanize
    int n = (int)((0.042 - 0.012 * prog) * SR); long s0 = (long)(t * SR);
    double prev = 0, held = 0;
    int hold = (int)(SR / (9000.0 + 5500.0 * prog)) + 1, hc = 0;
    double dec = 95.0 + 55.0 * prog, drv = 2.6 + 1.1 * prog;
    for (int i = 0; i < n; i++) {
        if (--hc <= 0) { held = rnd2(); hc = hold; }
        double hp = held - prev; prev = held;
        double v = tanh(hp * drv) * 0.55 * exp(-((double)i / SR) * dec);
        addb(buf, s0 + i, v * g * 1.9);
    }
}

static void tamb_open(float *buf, double t, double g) {
    // open hat — same crunch DNA, long sizzle, breathes on the offbeat
    t = hum_t(t, 6); g = hum_g(g, 0.15);
    int n = (int)(0.14 * SR); long s0 = (long)(t * SR);
    double prev = 0, held = 0;
    int hold = (int)(SR / 9500.0) + 1, hc = 0;
    for (int i = 0; i < n; i++) {
        if (--hc <= 0) { held = rnd2(); hc = hold; }
        double hp = held - prev; prev = held;
        double v = tanh(hp * 2.8) * 0.5 * exp(-((double)i / SR) * 26);
        addb(buf, s0 + i, v * g * 1.6);
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
                         double g, const Break *breaks, int nb, double attack,
                         double ratio) {
    // ratio > 1 pitches the sample UP (linear-interp resample, shorter);
    // envelope break times stay in OUTPUT seconds so phrase timing holds.
    long s0 = (long)(start_sec * SR);
    long on = (long)(sn / (ratio > 0.25 ? ratio : 0.25));
    double level = attack > 0 ? 0 : 1;
    double from = level, ramp_start = 0, ramp_len = attack, target = 1;
    int bi = 0;
    for (long i = 0; i < on; i++) {
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
        // resample read (linear interp)
        double pos = i * ratio;
        long j = (long)pos;
        if (j >= sn - 1) break;
        double fr = pos - j;
        double sv = s[j] * (1 - fr) + s[j + 1] * fr;
        // declick: whatever the envelope says, the clip's own end always
        // fades — a dora shorter than its scheduled break was cutting hard.
        long left = on - 1 - i;
        double edge = left < (long)(0.012 * SR) ? (double)left / (0.012 * SR) : 1.0;
        addb(buf, s0 + i, sv * g * level * edge);
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
// stamp window (seconds) — set when the FX pass places the voice stamp, so
// the master loop can run its EQ gesture across the same moment.
static double g_stamp0 = -1, g_stamp1 = -1;
static double g_finale = -1; // finale-launch time — drives the drum blackout
// light shuffle: nudge the off-8th (odd half-beat) later so the hats swing.
static inline double swing8(double t, int i) { return (i & 1) ? t + 0.085 * BEAT : t; }

// reversed serious kick swelling INTO t_end — the "whoop" suck-in that
// lands its attack exactly on the downbeat it leads into.
static void kick_rev(float *buf, double t_end, double g) {
    int n = (int)(0.30 * SR);
    long s0 = (long)(t_end * SR) - n;
    double ph = 0;
    for (int i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double f = 42 + (150 - 42) * exp(-tt * 30);
        ph += TAU * f / SR;
        double v = tanh(sin(ph) * 2.4) * exp(-tt * 7.5);
        addb(buf, s0 + (n - 1 - i), v * g);
    }
}

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
        // attack and release as independent slopes, env = their MIN — the
        // old if/else jumped from 1.0 to mid-release the instant the attack
        // ended whenever the release window overlapped it (short clip +
        // long release), which popped audibly mid-syllable.
        double ea = af > 0 ? fmin(1.0, (double)i / af) : 1.0;
        double er = fmin(1.0, (double)(outEst - i) / (rf > 0 ? rf : 1));
        double env = fmin(ea, er);
        if (env < 0) env = 0;
        // declick against SOURCE exhaustion: outEst is only an estimate —
        // slide/vibrato can drain the clip early, cutting mid-envelope.
        double rem = (cn - 1 - pos) / r; // output samples left in the source
        double edge = rem < 0.04 * SR ? rem / (0.04 * SR) : 1.0;
        addb(buf, s0 + i, s * g * env * edge);
        pos += r;
    }
}

// a clip fired ACROSS the stereo field — resampled fast, pan sweeping
// hard side to side with a pass-by gain swell, like it's zooming past
// the listener. writes straight into the stereo zoom bus.
static void place_zoom(const float *clip, long cn, double t, double ratio,
                       int dir, double g) {
    if (!clip || cn < 2) return;
    long s0 = (long)(t * SR);
    long on = (long)(cn / (ratio > 0.25 ? ratio : 0.25));
    if (on < 32) return;
    for (long i = 0; i < on; i++) {
        double pos = i * ratio;
        long j = (long)pos;
        if (j >= cn - 1) break;
        double fr = pos - j;
        double sv = clip[j] * (1 - fr) + clip[j + 1] * fr;
        double x = (double)i / on;
        double pan = dir * (x * 2.0 - 1.0);       // sweep across the field
        double sw = 0.35 + 0.65 * sin(M_PI * x);  // pass-by swell
        double a = (pan + 1.0) * M_PI / 4.0;
        double eIn = fmin(1.0, (double)i / (0.008 * SR));
        double eOut = fmin(1.0, (double)(on - 1 - i) / (0.030 * SR));
        double v = sv * g * sw * eIn * eOut;
        long idx = s0 + i;
        if (idx >= 0 && idx < N) {
            zmL[idx] += (float)(v * cos(a));
            zmR[idx] += (float)(v * sin(a));
        }
    }
}

// a clip HUNG in the field — pitched high, rocking left↔right on a slow
// sine, flanging against a delayed copy of itself (sine-modulated comb).
// the outro voice: jeffrey as a departing signal.
static void place_wave(const float *clip, long cn, double t, double ratio,
                       double panHz, double g) {
    if (!clip || cn < 2) return;
    long s0 = (long)(t * SR);
    long on = (long)(cn / (ratio > 0.25 ? ratio : 0.25));
    if (on < 64) return;
    float *nb = malloc(on * 4);
    for (long i = 0; i < on; i++) {
        double pos = i * ratio;
        long j = (long)pos;
        if (j >= cn - 1) { on = i; break; }
        double fr = pos - j;
        nb[i] = (float)(clip[j] * (1 - fr) + clip[j + 1] * fr);
    }
    for (long i = 0; i < on; i++) {
        double tt = (double)i / SR;
        double dmod = 0.0035 + 0.0025 * sin(TAU * 0.6 * tt); // sine flange
        long dj = i - (long)(dmod * SR);
        double v = nb[i] + (dj >= 0 ? nb[dj] * 0.6 : 0);
        double eIn = fmin(1.0, tt / 0.10);
        double eOut = fmin(1.0, (double)(on - 1 - i) / (0.25 * SR));
        double pan = 0.85 * sin(TAU * panHz * tt);
        double a = (pan + 1.0) * M_PI / 4.0;
        double vv = v * g * eIn * eOut;
        long idx = s0 + i;
        if (idx >= 0 && idx < N) {
            zmL[idx] += (float)(vv * cos(a));
            zmR[idx] += (float)(vv * sin(a));
        }
    }
    free(nb);
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
        else if (!strcmp(argv[i], "--classic-drums")) classic_drums = 1;
        else if (!strcmp(argv[i], "--novel-drums")) classic_drums = 0;
    }

    BEAT = 60.0 / SCORE_BPM;
    BAR = BEAT * 4;

    int total_bars = 0;
    for (int s = 0; s < N_SECS; s++) total_bars += STRUCTURE[s].bars * STRUCTURE[s].reps;
    double total_sec = total_bars * BAR + 0.5;
    N = (long)ceil(total_sec * SR);
    gTotalSec = total_sec;

    drm = calloc(N, 4); kik = calloc(N, 4); snr = calloc(N, 4); bel = calloc(N, 4);
    sqr = calloc(N, 4); sb  = calloc(N, 4); toy = calloc(N, 4);
    arp = calloc(N, 4); pds = calloc(N, 4); cho = calloc(N, 4); voc = calloc(N, 4);
    org = calloc(N, 4); zmL = calloc(N, 4); zmR = calloc(N, 4);

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

                // very faint harmony bed — every bar EXCEPT the first 4 bars of
                // the opener: those are bone-dry, just the vocal, no pads.
                if (!(!strcmp(sec->section, "intro") && b < 4))
                    choir(cho, bt, root, third, fifth, 0.022);

                // DRUM BREAK (~1:24): one bar where the drums take the room —
                // accelerating snare roll + syncopated kicks; the mixdown
                // steps the backing aside across the same bar.
                if (bar_cursor + b == 39) {
                    // sparse and TIGHT with real air between the hits — one
                    // kick, syncopated snares with space, a short pickup
                    // roll, then the reverse kick pulls everything out.
                    static const int HIT16[5] = {0, 3, 6, 8, 11};
                    for (int i2 = 0; i2 < 5; i2++)
                        snare_hit(snr, bt + BEAT * (HIT16[i2] / 4.0), 0.34 + 0.07 * i2);
                    for (int i2 = 13; i2 < 16; i2++) // pickup roll into the door
                        snare_hit(snr, bt + BEAT * (i2 / 4.0), 0.28 + 0.5 * (i2 - 13) / 3.0);
                    kick_serious(kik, bt, 1.05); push_kick(bt, 1);
                    kick_rev(kik, bt + BAR, 0.7);
                    crash(drm, bt + BAR, 0.22);
                }

                // church organ chords — bridge breakdown only; the opener
                // stays bare (vocal + choir bed), no organ under it.
                if (!strcmp(sec->section, "bridge"))
                    organ(org, bt, root, third, fifth, BAR * 1.02, 0.07);

                if (strcmp(sec->feel, "claps-only") && strcmp(sec->feel, "fade")) {
                    double sg = (!strcmp(sec->feel, "thin") || !strcmp(sec->feel, "sparse")) ? 0.22 : 0.35;
                    if (!strcmp(sec->section, "intro")) {
                        // opener is just the vocal — sub holds out, then eases
                        // in over the last 3 bars toward the drop.
                        sg = 0.10 * fmax(0.0, fmin(1.0, (b - 6) / 3.0));
                    }
                    if (sg > 0) sub_bass(sb, bt, root, BAR * 0.95, sg);
                }

                if (!strcmp(sec->section, "stop") || !strcmp(sec->feel, "claps-only")) {
                    clap(drm, bt + BEAT, 1.05);
                    clap(drm, bt + BEAT * 3, 1.05);
                } else if (!strcmp(sec->section, "intro")) {
                    // the opener: a SLOW STAIRCASE reveal across all 10 bars —
                    // bars 0-3 are jeffrey's vocal utterly alone, then one
                    // instrument steps in at a time: choir bar 4 (above), soft
                    // hats bar 5, sub bar 7, toy-piano melody bar 8, and the
                    // full chord stack only on the final bar before the drop.
                    if (b >= 5) for (int i = 0; i < 8; i++) tamb_soft(drm, bt + i * (BEAT / 2), 0.06);
                    if (b >= 8) {
                    // melody line first — motion without weight
                    toy_piano(toy, bt,            third + 12, BEAT * 0.85, 0.07);
                    toy_piano(toy, bt + BEAT,     fifth + 12, BEAT * 0.85, 0.06);
                    toy_piano(toy, bt + BEAT * 2, root + 24, BEAT * 0.85, 0.06);
                    toy_piano(toy, bt + BEAT * 3, fifth + 12, BEAT * 0.85, 0.06);
                    }
                    if (b >= 9) {
                    // the chord stack lands only on the last bar — the fullest
                    // moment of the intro, one bar before the drop hits.
                    toy_piano(toy, bt,            root,      BEAT * 1.9, 0.06);
                    toy_piano(toy, bt,            third,     BEAT * 1.9, 0.055);
                    toy_piano(toy, bt,            fifth,     BEAT * 1.9, 0.05);
                    toy_piano(toy, bt + BEAT * 2, fifth,     BEAT * 1.9, 0.055);
                    toy_piano(toy, bt + BEAT * 2, root + 12, BEAT * 1.9, 0.055);
                    toy_piano(toy, bt + BEAT * 2, third + 12, BEAT * 1.9, 0.045);
                    }
#if N_SYL > 0
                    // OPENER — the CHORAL → SOLO arc. the chant begins as a
                    // hushed cloud of harmonious duplicate voices (jeffrey
                    // multiplied: detuned unisons, a harmony line, octave
                    // colors), swells as it goes, then NARROWS — the choir
                    // thins away to one subtle solo voice — and that solo
                    // SURGES loud right before the shock-rifle drop. the solo
                    // voice still wilds out / deteriorates as it emerges.
                    {
                        const int loDeg[4] = {root, third, fifth, third};
                        const int hiDeg[4] = {third, fifth, root + 12, fifth};
                        for (int k = 0; k < 4; k++) {
                            long s0 = (long)((bt + k * BEAT) * SR);
                            long slot = (long)(BEAT * 1.7 * SR);
                            int af = (int)(0.05 * SR), rf = (int)(0.30 * SR);
                            int si = intro_beat % N_SYL;
                            double q = fmin(1.0, intro_beat / 36.0);
                            // volume story: barely-there → gentle swell →
                            // near-absence → SURGE. he's not really in the
                            // room until the very end — it's the bed of him.
                            double cg;
                            if (q < 0.55)      cg = 0.16 + 0.30 * (q / 0.55);
                            else if (q < 0.78) cg = 0.46 - 0.16 * ((q - 0.55) / 0.23);
                            else               cg = 0.30 + 0.85 * ((q - 0.78) / 0.22);
                            // choir size: five voices at the start, solo by q≈0.7
                            int nDup = 1 + (int)(4.0 * fmax(0.0, 1.0 - q / 0.7) + 0.5);
                            double norm = 1.0 / sqrt((double)nDup);
                            int ti = -1, tiH = -1;
                            for (int t2 = 0; t2 < N_SYLT; t2++) {
                                if (SYL_NOTES[t2] == loDeg[k] && ti < 0) ti = t2;
                                if (SYL_NOTES[t2] == hiDeg[k] && tiH < 0) tiH = t2;
                            }
                            if (ti >= 0) {
                                Clip *sc = get_syl(0, si, ti);
                                Clip *scH = tiH >= 0 ? get_syl(0, si, tiH) : NULL;
                                Clip *scC = get_syl(1, si, ti);
                                if (sc) {
                                    // center voice: sung walk, wilding out with q
                                    static const double SINGR[8] =
                                        {1.0, 0.891, 1.122, 0.841, 1.0, 1.189, 0.75, 0.944};
                                    double ratio, slide, vib;
                                    if (intro_beat < N_SYL) { // word 1: whistley swoop
                                        ratio = 0.80; // stretched deeper
                                        slide = (intro_beat & 1) ? 0.20 : -0.16;
                                        vib = 0.05;
                                    } else {
                                        ratio = 0.78 * pow(SINGR[intro_beat & 7], 1.0 + 1.6 * q)
                                              * (1.0 + rnd2() * 0.05 * q); // detune rot
                                        slide = ((intro_beat & 1) ? 0.13 : -0.11) * (1.0 + 2.2 * q);
                                        vib = 0.030 + 0.055 * q;
                                    }
                                    double rel = 0.75 + 0.45 * q;
                                    // the center voice hangs BACK inside the
                                    // bed (slow bloom, long tail) until the
                                    // surge carries him forward
                                    place_pitched(voc, sc->s, sc->n, bt + k * BEAT,
                                                  ratio, slide, 4.4 + 2.2 * q, vib,
                                                  cg * (0.55 + 0.35 * fmax(0.0, (q - 0.78) / 0.22)) * norm,
                                                  0.14, rel);
                                    // the choir: harmonious duplicates around the
                                    // center — detuned unisons, the harmony line,
                                    // octave-down and octave-up colors — thinning
                                    // away as the solo emerges
                                    for (int d = 1; d < nDup; d++) {
                                        Clip *cd = ((d & 1) && scH) ? scH : sc;
                                        double det = 1.0 + 0.016 * ((d + 1) / 2) * ((d & 1) ? 1.0 : -1.0);
                                        double col = d == 3 ? 0.5 : d == 4 ? 2.0 : 1.0;
                                        // the octave-down voice sits FAR back
                                        double colG = d == 3 ? 0.22 : 0.45;
                                        place_pitched(voc, cd->s, cd->n,
                                                      bt + k * BEAT + d * 0.014,
                                                      0.76 * det * col, 0.0, 3.8, 0.018,
                                                      cg * colG * norm, 0.20, 0.75);
                                    }
                                    // angel halo: blooms with the choir, dies as
                                    // the field narrows to the solo
                                    double haloG = cg * 0.30 * fmin(1.0, q * 3.0)
                                                 * fmax(0.0, 1.0 - fmax(0.0, (q - 0.7) / 0.3));
                                    if (haloG > 1e-3)
                                        place_pitched(voc, sc->s, sc->n, bt + k * BEAT,
                                                      1.88 * (1.0 + 0.04 * q * rnd2()), 0.04 * q,
                                                      5.5, 0.02 + 0.03 * q, haloG, 0.15, 0.60);
                                }
                                if (scC) { // the bg computer voice — straight, way back
                                    double g = 0.07 * cg;
                                    long lim = scC->n < slot ? scC->n : slot;
                                    for (long i = 0; i < lim; i++) {
                                        double env = 1;
                                        if (i < af) env = (double)i / af;
                                        else if (i > lim - rf) env = (double)(lim - i) / rf;
                                        addb(voc, s0 + i, scC->s[i] * g * env);
                                    }
                                }
                            }
                            intro_beat++;
                        }
                    }
#endif
                } else if (!strcmp(sec->feel, "fade")) {
                    if (!(b & 1)) kick(kik, bt, 0.55);
                    for (int i = 0; i < 8; i++) tamb_soft(drm, bt + i * (BEAT / 2), 0.05);
#if N_SYL > 0
                    // jeffrey returns for the departure — octaves higher,
                    // staggered doubles so the notes hang long, rocking
                    // left↔right, sine-flanging against himself
                    {
                        int fdegs[3] = {root + 12, fifth + 12, third + 12};
                        int fdeg = fdegs[b % 3], fti = -1;
                        for (int q = 0; q < N_SYLT; q++)
                            if (SYL_NOTES[q] == fdeg - 12) { fti = q; break; }
                        if (fti < 0) for (int q = 0; q < N_SYLT; q++)
                            if (SYL_NOTES[q] == root) { fti = q; break; }
                        Clip *fsc = fti >= 0 ? get_syl(0, (bar_cursor + b) % N_SYL, fti) : NULL;
                        if (fsc) {
                            double rr = (b & 1) ? 2.0 : 1.68;
                            place_wave(fsc->s, fsc->n, bt + BEAT * 0.5, rr, 0.40, 0.34);
                            place_wave(fsc->s, fsc->n, bt + BEAT * 0.68, rr, 0.40, 0.24);
                            if (!(b & 1)) {
                                place_wave(fsc->s, fsc->n, bt + BEAT * 2.75, rr * 1.189, 0.55, 0.28);
                                place_wave(fsc->s, fsc->n, bt + BEAT * 2.93, rr * 1.189, 0.55, 0.18);
                            }
                        }
                    }
#endif
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
                                // angel halo on jeffrey's line: octave up,
                                // quiet, slow bloom, ringing past the slot
                                if (vx == 0)
                                    place_pitched(voc, sc->s, sc->n, bt + k * BEAT,
                                                  1.88, 0.0, 5.5, 0.02, g * 0.28, 0.12, 0.50);
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
                        tamb_open(drm, bt + BEAT * 2.5, 0.05); // one breath per bar
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
                        // hot hooks densify: 16ths with inner accents + open
                        // hats breathing on the two offbeats
                        for (int i = 0; i < 16; i++)
                            tamb(drm, swing8(bt + i * (BEAT / 4), i >> 1),
                                 (i & 3) == 0 ? 0.055 : 0.030);
                        tamb_open(drm, bt + BEAT * 1.5, 0.06);
                        tamb_open(drm, bt + BEAT * 3.5, 0.07);
                    }
                    // the backing DEVELOPS instead of looping: the bell walks
                    // a 4-bar melody (and rests on bar 4), the arp alternates
                    // high 16ths with a low 8th figure and sits out every 8th
                    // bar, the pad voicing lifts every other pair of bars,
                    // and a little snare turnaround closes each 4-bar phrase.
                    int gb = bar_cursor + b;
                    int bellDeg[4] = {root + 12, fifth + 12, third + 12, root + 24};
                    if ((gb & 3) != 3)
                        bell(bel, bt, bellDeg[gb & 3], BAR * 0.95, 0.10 + 0.05 * tins);
                    if ((gb & 7) != 7) {
                        if (gb & 1) bach_arp(arp, bt, root, ch->minor, 8, 0, 0.06 + 0.03 * tins);
                        else        bach_arp(arp, bt, root, ch->minor, 16, 12, 0.07 + 0.04 * tins);
                    }
                    int chord4a[4] = {root - 12, root, third, fifth};
                    int chord4b[4] = {root - 12, third, fifth, root + 12};
                    pad(pds, bt, (gb & 2) ? chord4b : chord4a, 4, BAR * 1.02, 0.05 + 0.02 * tins);
                    if ((gb & 3) == 3 && tins >= 0.34) {
                        snare_hit(snr, bt + BEAT * 3.5, 0.35);
                        snare_hit(snr, bt + BEAT * 3.75, 0.5);
                    }
                    // kick / REVERSE-kick: on the hot hooks the turnaround
                    // bar sucks air into the next downbeat — but NOT inside
                    // the ~1:45 crunch zone, where the reverse whoosh got
                    // bitcrushed/flanged into a weird smeared launch.
                    if ((gb & 3) == 3 && tins >= 0.67
                        && fabs((bt + BAR) - 105.0) > 8.5)
                        kick_rev(kik, bt + BAR, 0.6);
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
                        const double BLEND = tier == 0 ? 0.30 : tier == 1 ? 0.42 : 0.65;
                        const double PRE = 0.30; // computer's vowel pours in EARLY —
                                                 // both vowels co-sound so the
                                                 // junction reads as ONE vowelization
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
                                    // gentle fade-out of the aaaa into dora —
                                    // america's vowel CARRIES through the
                                    // junction so it pours as "...cawwwm..."
                                    breaks[0] = (Break){duck_at - BLEND - PRE, 0.78, BLEND};
                                    breaks[1] = (Break){choke_at - 0.45, 0, 0.70};
                                } else {
                                    // computer holds UP under dora (0.45, was
                                    // 0.30) — the ooommm keeps ringing
                                    breaks[0] = (Break){duck_at - 0.06, 0.45, 0.20};
                                    breaks[1] = (Break){choke_at - 0.06, 0, 0.22};
                                }
                                nb = 2;
                                attack = w == 0 ? 0 : BLEND;
                                // computer runs HOT — its "awwwm" vowel has to
                                // bloom through america's carried tail.
                                gain = (w == 1 ? 1.35 : 0.95) * sec_gain * firstHookG;
                            }
                            // soften "computer": skip the hard "co" transient so
                            // the long crossfade swells in as "...uhmmmputer".
                            // raw/plain tiers aren't decapped in the bake, so
                            // they need a much deeper skip to lose the /k/;
                            // the full tier is already vowel-onset ("ahmputer").
                            long coSkip = (w == 1)
                                ? (long)((tier < 2 ? 0.15 : 0.03) * SR) : 0;
                            // +2 semitones (1.122) — the hook samples pitch UP,
                            // brighter bubblegum; the vowel-bridge overlap
                            // absorbs the slightly shorter clips.
                            paint_shaped(voc, c->s + coSkip, c->n - coSkip,
                                         phrase_start + onsets[w] - lead,
                                         gain, breaks, nb, attack, 1.122);
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

#if N_SYL > 0
    // ── ornament pass: jeffrey's voice littered through the first 90s —
    // elongated syllables here and there, half as long low stretched sighs
    // (ratio 0.68 ≈ 1.5× longer), half as high angel calls. they live on
    // the CHOIR bus so they take the bed's crisp + stereo width and sit in
    // the atmosphere instead of driving the vocal sidechain.
    {
        double tmax = fmin(90.0, total_bars * BAR);
        uint32_t orn = 0xC0FFEE21u;
        for (int bar = 3; bar * BAR < tmax; bar += 2) {
            orn ^= orn << 13; orn ^= orn >> 17; orn ^= orn << 5;
            if ((orn & 7) < 3) continue; // ~60% of candidate bars get one
            const ChordDef *ch = chord_at_bar(bar);
            int root = ch->root, third = root + (ch->minor ? 3 : 4), fifth = root + 7;
            int degs[3] = {root, third, fifth};
            int deg = degs[orn % 3];
            int ti = -1;
            for (int q = 0; q < N_SYLT; q++) if (SYL_NOTES[q] == deg) { ti = q; break; }
            if (ti < 0) continue;
            int si = (int)((orn >> 4) % N_SYL);
            Clip *sc = get_syl(0, si, ti);
            if (!sc) continue;
            double beatoff = ((orn >> 8) & 3) * 0.5 + 1.0; // offbeat landings
            double t = bar * BAR + beatoff * BEAT;
            // SUNG, not droned: each ornament picks a melodic interval off
            // its family base and slides INTO the note (portamento), and
            // some get a second legato note — a little two-note melisma.
            static const double STEP[4] = {0.891, 1.0, 1.122, 1.189};
            double base = ((orn >> 12) & 1) ? 1.5 : 0.68; // angel call / long sigh
            double r1 = base * STEP[(orn >> 16) & 3];
            double slide = ((orn >> 18) & 1) ? 0.05 : -0.05; // scoop up or fall in
            double gg = base > 1.0 ? 0.16 : 0.20;
            place_pitched(cho, sc->s, sc->n, t, r1, slide, 4.6, 0.022, gg, 0.20, 0.75);
            if (((orn >> 20) & 7) < 3) { // ~38%: answer note, step away, legato
                double r2 = r1 * (((orn >> 23) & 1) ? 1.122 : 0.891);
                double d1 = (sc->n / (r1 * SR)) * 0.7; // overlap the tail
                place_pitched(cho, sc->s, sc->n, t + d1, r2, -slide, 4.6, 0.022,
                              gg * 0.8, 0.15, 0.80);
            }
        }
    }

    // ── the zoom-by THREAD: skip-a-boo jeffreys whipping across the field,
    // strung compositionally through four sections — the first appearance
    // at ~0:38, dreamier at the second verse, smearing long through the
    // 1:45 crunch zone, and deep slow fly-bys on the road to the finale.
    // each return is more STRETCHED (lower ratio = longer flight).
    {
        static const struct { int bar; int n; double ratio; double g; } ZTHREAD[4] = {
            {17, 9, 1.10, 0.34},  // ≈0:38 — skip-a-boo, stretched
            {33, 6, 0.95, 0.30},  // verse 2 — slower, dreamier
            {48, 7, 0.82, 0.32},  // crunch zone — long smears in the mangle
            {58, 5, 0.72, 0.30},  // pre-finale — deep slow fly-bys
        };
        static const double SKIP[9] = {0, 0.75, 1.0, 1.75, 2.0, 2.5, 3.0, 3.25, 3.75};
        for (int zi = 0; zi < 4; zi++) {
            const ChordDef *zch = chord_at_bar(ZTHREAD[zi].bar);
            int zroot = zch->root, zthird = zroot + (zch->minor ? 3 : 4), zfifth = zroot + 7;
            int zdegs[3] = {zroot, zthird, zfifth};
            uint32_t zr = 0x5A00B001u + (uint32_t)zi * 0x9E3779B9u;
            for (int z = 0; z < ZTHREAD[zi].n; z++) {
                zr ^= zr << 13; zr ^= zr >> 17; zr ^= zr << 5;
                int deg = zdegs[zr % 3];
                int ti = -1;
                for (int q = 0; q < N_SYLT; q++) if (SYL_NOTES[q] == deg) { ti = q; break; }
                if (ti < 0) continue;
                Clip *sc = get_syl(0, (int)((zr >> 4) % N_SYL), ti);
                if (!sc) continue;
                double ratio = ZTHREAD[zi].ratio * (1.0 + 0.12 * (int)((zr >> 8) & 3));
                place_zoom(sc->s, sc->n,
                           ZTHREAD[zi].bar * BAR + SKIP[z % 9] * BEAT + (z / 9) * BAR,
                           ratio, (z & 1) ? 1 : -1, ZTHREAD[zi].g);
            }
        }
    }
#endif

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
        kick_rev(kik, choir_in, 0.85);          // reverse kick sucks the drop in
        // 2) finale launch — shock rifle + scratch + gong all land together as
        // the pads drop out and the last hook hits.
        g_finale = finale;
        PLACE(gong, gn, finale - 0.45, 0.65);
        PLACE(sc1, s1n, finale - 0.42, 0.80); // scratch LEADS the downbeat
        PLACE(shk, shn, finale - 0.18, 0.85);
        kick_rev(kik, finale, 0.9);
        // "aesthetic dot computer" voice stamp — announces the SECOND drop:
        // a DUET (the pitched-up stamp + jeffrey's ElevenLabs voice under it,
        // slightly stretched, so it choruses), pitched + glitched around —
        // stuttering chops flick before and after at scattered pitches. on
        // the voc bus so the vocal sidechain parts the mix for it.
        {
            long stn = 0, sjn = 0;
            snprintf(fp, sizeof fp, "%sfx/ac-stamp.wav", voc_base);
            float *stamp = load_wav_mono(fp, &stn);
            snprintf(fp, sizeof fp, "%sfx/ac-stamp-jeffrey.wav", voc_base);
            float *stampJ = load_wav_mono(fp, &sjn);
            if (stampJ) {
                // jeffrey's ElevenLabs take speaks it SOLO — clean, full —
                // and the pitched-up machine version only exists as glitch
                // flavor darting around it (both are the same take, so
                // stacking two full phrases just garbled).
                double sdur = (double)sjn / SR;
                double st0 = finale - sdur - 0.12;
                g_stamp0 = st0 - 0.40; g_stamp1 = st0 + sdur + 0.25;
                long ss0 = (long)(st0 * SR);
                long sfd = (long)(0.010 * SR);
                for (long i = 0; i < sjn; i++) {
                    double env = 1;
                    if (i < sfd) env = (double)i / sfd;
                    else if (i > sjn - sfd) env = (double)(sjn - i) / sfd;
                    addb(voc, ss0 + i, stampJ[i] * 0.95 * env);
                }
                // glitch scatter: pitched stutter-chops of the machine
                // version flicking across the field before the phrase lands
                if (stamp) {
                    uint32_t gr = 0x611C4ED5u;
                    static const double GR[4] = {0.75, 1.5, 2.0, 0.5};
                    for (int gch = 0; gch < 4; gch++) {
                        gr ^= gr << 13; gr ^= gr >> 17; gr ^= gr << 5;
                        long off = (long)((gr % 900) * 0.001 * SR);
                        long len = (long)((0.06 + 0.08 * ((gr >> 10) & 1)) * SR);
                        if (off + len >= stn) continue;
                        double gt = st0 - 0.38 + 0.09 * gch;
                        place_zoom(stamp + off, len, gt, GR[gch & 3],
                                   (gch & 1) ? 1 : -1, 0.28);
                    }
                }
                if (stamp) free(stamp);
                free(stampJ);
            }
        }

        // ── national-anthem echoes: the FEM stone bells toll the opening
        // phrase ("o say can you see" — A F# D F# A D') long and quiet
        // across the bridge, then a fainter echo two bars later — echoes
        // of the nationalism, rung on real modelled bronze... er, stone.
        {
            double bridgeT = -1;
            { int bb = 0;
              for (int s = 0; s < N_SECS; s++) {
                  int len = STRUCTURE[s].bars * STRUCTURE[s].reps;
                  if (!strcmp(STRUCTURE[s].section, "bridge")) { bridgeT = bb * BAR; break; }
                  bb += len; } }
            if (bridgeT >= 0) {
                const char *NOTE_FILE[4] = {"anthem-D4.wav", "anthem-F#4.wav",
                                            "anthem-A4.wav", "anthem-D5.wav"};
                float *bells[4]; long bn[4];
                for (int nb2 = 0; nb2 < 4; nb2++) {
                    snprintf(fp, sizeof fp, "%sfx/%s", voc_base, NOTE_FILE[nb2]);
                    bells[nb2] = load_wav_mono(fp, &bn[nb2]);
                }
                static const int PHRASE[6] = {2, 1, 0, 1, 2, 3}; // A F# D F# A D'
                for (int rep = 0; rep < 2; rep++) {
                    double t0 = bridgeT + rep * 4.0 * BAR;
                    double gg = rep ? 0.15 : 0.30; // the echo echoes
                    for (int nn = 0; nn < 6; nn++) {
                        int v = PHRASE[nn];
                        if (!bells[v]) continue;
                        long s0b = (long)((t0 + nn * BAR * 0.5) * SR);
                        for (long i = 0; i < bn[v]; i++)
                            addb(bel, s0b + i, bells[v][i] * gg);
                    }
                }
                for (int nb2 = 0; nb2 < 4; nb2++) if (bells[nb2]) free(bells[nb2]);
            }
        }
        // crispy zone ~1:45 — the super-scratching cluster (the master loop
        // adds bitcrush + flange across the same window)
        PLACE(sc1, s1n, 103.6, 0.70);
        PLACE(sc2, s2n, 104.9, 0.60);
        PLACE(sc1, s1n, 106.2, 0.80);
        PLACE(sc2, s2n, 107.4, 0.55);
        PLACE(sc1, s1n, 109.1, 0.75);
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
    crisp(voc, N, 130, 3500, 0.35); // hp lowered 160→130 — keep the ooom's chest
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
    // two decorrelated rooms — same send, comb delays scaled ~9% apart per
    // channel, so the reverb is the stereo width of the mix while every
    // direct voice stays phase-coherent.
    float *roomL = NULL, *roomR = NULL;
    for (int side = 0; side < 2; side++) {
        double scale = side ? 1.093 : 1.0;
        float *room = calloc(N, 4);
        static const double CDEL[4] = {0.0297, 0.0371, 0.0411, 0.0437};
        float *cb[4]; long cl[4], ci[4] = {0, 0, 0, 0};
        for (int c = 0; c < 4; c++) { cl[c] = (long)(CDEL[c] * scale * SR); cb[c] = calloc(cl[c], 4); }
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
            long al = (long)(ADEL[a] * scale * SR), ai = 0;
            float *ab = calloc(al, 4);
            for (long i = 0; i < N; i++) {
                double y = ab[ai], x = room[i];
                ab[ai] = (float)(x + y * 0.5);
                room[i] = (float)(y - x * 0.5);
                ai = (ai + 1) % al;
            }
            free(ab);
        }
        if (side) roomR = room; else roomL = room;
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

    // ── master (stereo) ───────────────────────────────────────────────
    // the power section — kick, sub, snare, lead vocal + echo — is dead
    // center. color voices take constant-power pans: bells left, toy piano
    // right, organ left, square right, and the bach arp drifts slowly across
    // the field on an LFO. sustained beds (pads, choir) and the hat/tamb bus
    // widen with a delayed side component (L adds it, R subtracts — mono
    // fold-down cancels the side exactly). the reverb is the widest thing in
    // the mix: two decorrelated rooms hard L/R.
    double belGL, belGR, toyGL, toyGR, orgGL, orgGR, sqrGL, sqrGR;
    pan_gains(-0.30, &belGL, &belGR);
    pan_gains(+0.35, &toyGL, &toyGR);
    pan_gains(-0.20, &orgGL, &orgGR);
    pan_gains(+0.25, &sqrGL, &sqrGR);
    const double CTR = 0.70710678; // constant-power center
    const long dPds = (long)(0.013 * SR), dCho = (long)(0.017 * SR),
               dDrm = (long)(0.006 * SR);
    // ── SPATIAL STORY — the distanced listener ────────────────────────
    // the song starts far away and nearly MONO (a small radio in another
    // room); the field creeps open as the staircase instruments join; at
    // the drop the world SNAPS wide; the bridge narrows to a corridor;
    // and across the outro the track walks back into the distance,
    // folding toward mono as it fades. implemented as a per-sample
    // mid/side width automation folded over the whole stereo mix.
    double bridge0 = -1, bridge1 = -1;
    {
        int bb = 0;
        for (int s = 0; s < N_SECS; s++) {
            int len = STRUCTURE[s].bars * STRUCTURE[s].reps;
            if (!strcmp(STRUCTURE[s].section, "bridge") && bridge0 < 0) {
                bridge0 = bb * BAR; bridge1 = (bb + len) * BAR;
            }
            bb += len;
        }
    }

    // SUBSTRATE PRINT (tape) — the medium the mix is baked onto: tanh tape
    // drive with an even-harmonic tube bias (asymmetric warmth, DC removed)
    // and a low-passed hiss floor. Params from pop/lib/substrate.mjs 'tape'.
    const double SDRV = 1.6, SBIAS = 0.22, SHISS = 0.0055, SNORM = tanh(1.6);
    uint32_t hss = 0x51ed2701u;
    double hlpL = 0, hlpR = 0; const double HLP = 0.22;
    // crunch zone (~1:45): bitcrush + flange blended in over a smooth window
    const double ZC = 105.0, ZW = 8.0;
    #define ZMASK 16383
    float *zbufL = calloc(ZMASK + 1, 4), *zbufR = calloc(ZMASK + 1, 4);
    double zheldL = 0, zheldR = 0; int zhc = 0;
    // stamp EQ gesture state (telephone band blooming open to full range)
    double eqLpL = 0, eqLpR = 0, eqHpL = 0, eqHpR = 0;
    float *mixL = malloc(N * 4), *mixR = malloc(N * 4);
    double peak = 0;
    for (long i = 0; i < N; i++) {
        double dk = duck[i];
        double tsec = (double)i / SR;
        // drum-break clearing (~1:24): the backing steps aside for one bar
        // while the drums take the room, cosine shoulders so nothing clicks
        double brk = 0;
        {
            double b0 = 39 * BAR, b1 = 40 * BAR, e = 0.03;
            if (tsec > b0 && tsec < b1 + e) {
                if (tsec < b0 + e) brk = 0.5 - 0.5 * cos(M_PI * (tsec - b0) / e);
                else if (tsec > b1) brk = 0.5 + 0.5 * cos(M_PI * (tsec - b1) / e);
                else brk = 1.0;
            }
        }
        double L, R;
        if (vocals_only) {
            // isolated vocal mix: words + echo tail + room share, plus
            // the kick (pumping the words) and the faint harmony bed.
            double tail0 = (echo[i] - voc[i]) * 0.28;
            double c = kik[i] + voc[i] * (0.75 + 0.25 * dk) + tail0 * dk
                     + cho[i] * 0.7;
            L = c * CTR + roomL[i] * 0.35;
            R = c * CTR + roomR[i] * 0.35;
        } else {
            // mix balance: drums punchy, the dense melodic bus pulled back a
            // touch to clear room for the voice, vocals brought forward and
            // sitting on a tighter (less washy) reverb.
            // vocal sidechain: melodic bus ducks hardest, drums a touch, so the
            // lead vocal carves its own space (flow).
            double vd = vSide[i];
            double dg = 0.96 * (1.0 - 0.16 * vd) * (1.0 + 0.18 * brk);
            double mg = 0.86 * dk * (1.0 - 0.45 * vd) * (1.0 - 0.85 * brk);
            // pre-finale drum blackout: kicks + snares fall away for the
            // last bar (stamp + shock rifle fire in the clearing, hats keep
            // ticking) and slam back with the reverse kick into the drop
            double kdrop = 0;
            if (g_finale > 0) {
                double d0 = g_finale - BAR, d1 = g_finale - 0.32, e = 0.05;
                if (tsec > d0 && tsec < d1) {
                    if (tsec < d0 + e) kdrop = 0.5 - 0.5 * cos(M_PI * (tsec - d0) / e);
                    else if (tsec > d1 - e) kdrop = 0.5 + 0.5 * cos(M_PI * (tsec - (d1 - e)) / e);
                    else kdrop = 1.0;
                }
            }
            // center power section
            double subb = sb[i] * (0.55 + 0.45 * dk) * (1.0 - 0.6 * brk);
            double tail = (echo[i] - voc[i]) * 0.28;
            double vocal = (voc[i] * (0.78 + 0.22 * dk) + tail * dk) * 0.90
                         * (1.0 - 0.7 * brk);
            double center = (kik[i] + snr[i]) * dg * (1.0 - kdrop) + subb + vocal;
            L = center * CTR; R = center * CTR;
            // hat/tamb/FX bus: center + a touch of side width
            double dv = drm[i] * dg;
            double dsd = (i >= dDrm ? drm[i - dDrm] : 0) * dg * 0.22;
            L += dv * CTR + dsd; R += dv * CTR - dsd;
            // panned color voices
            double b = bel[i] * mg, ty = toy[i] * mg, og = org[i] * mg, sq = sqr[i] * mg;
            L += b * belGL + ty * toyGL + og * orgGL + sq * sqrGL;
            R += b * belGR + ty * toyGR + og * orgGR + sq * sqrGR;
            // jeffrey zoom-bys + stamp glitches — already stereo with their
            // own flight paths, added straight in
            L += zmL[i]; R += zmR[i];
            // bach arp: slow LFO drift across the field (~14 s per sweep)
            double ap = 0.35 * sin(TAU * 0.07 * ((double)i / SR));
            double aGL, aGR; pan_gains(ap, &aGL, &aGR);
            double av = arp[i] * mg;
            L += av * aGL; R += av * aGR;
            // sustained beds: center + delayed side (opposite signs so the
            // pads lean one way, the choir the other)
            double pv = pds[i] * mg, cv = cho[i] * mg;
            double psd = (i >= dPds ? pds[i - dPds] : 0) * mg * 0.35;
            double csd = (i >= dCho ? cho[i - dCho] : 0) * mg * 0.35;
            L += pv * CTR + psd + cv * CTR - csd;
            R += pv * CTR - psd + cv * CTR + csd;
            // stereo room
            double rg = 0.48 * (0.7 + 0.3 * dk);
            L += roomL[i] * rg; R += roomR[i] * rg;
        }
        double fade = i >= fade_s ? fmax(0, 1.0 - (double)(i - fade_s) / (N - fade_s)) : 1.0;
        // spatial story: width by song position
        {
            double ts = tsec;
            double wid;
            if (ts < introEnd) {
                // width follows the choral→solo arc: the voice cloud BLOOMS
                // across the field, then the field condenses with the choir
                // until the loud solo is a single mono point — which is what
                // detonates into full width at the drop.
                double qq = ts / introEnd;
                if (qq < 0.55)      wid = 0.15 + 0.55 * (qq / 0.55);
                else if (qq < 0.80) wid = 0.70 - 0.55 * ((qq - 0.55) / 0.25);
                else                wid = 0.15;
            } else {
                wid = 1.0;                              // the drop: wide open
                if (bridge0 >= 0) {                     // bridge: the corridor
                    if (ts >= bridge0 && ts <= bridge1) wid = 0.65;
                    else if (ts > bridge0 - 0.4 && ts < bridge0)
                        wid = 1.0 - 0.35 * (1.0 - (bridge0 - ts) / 0.4);
                    else if (ts > bridge1 && ts < bridge1 + 0.4)
                        wid = 0.65 + 0.35 * ((ts - bridge1) / 0.4);
                }
            }
            if (i >= fade_s) wid *= 0.25 + 0.75 * fade; // walking away
            double m = 0.5 * (L + R), sd = 0.5 * (L - R) * wid;
            L = m + sd; R = m - sd;
        }
        // crunch zone (~1:45): sample-hold bitcrush + a slow flange tap read
        // back from the mix's own recent past, blended by a smooth window
        {
            double za = 1.0 - fabs(tsec - ZC) / ZW;
            if (za > 0) {
                za = za * za * (3.0 - 2.0 * za); // smoothstep
                if (--zhc <= 0) { zheldL = L; zheldR = R; zhc = 1 + (int)(6.0 * za); }
                const double qs = 18.0;
                double cl = floor(zheldL * qs + 0.5) / qs;
                double cr = floor(zheldR * qs + 0.5) / qs;
                // L and R flange taps ride the LFO a quarter-turn apart so
                // the mangle THROBS across the field instead of sitting still
                long fdL = (long)((0.0012 + 0.0024 * (0.5 + 0.5 * sin(TAU * 0.27 * tsec))) * SR);
                long fdR = (long)((0.0012 + 0.0024 * (0.5 + 0.5 * sin(TAU * 0.27 * tsec + 1.5708))) * SR);
                double flL = i > fdL ? zbufL[(i - fdL) & ZMASK] : 0;
                double flR = i > fdR ? zbufR[(i - fdR) & ZMASK] : 0;
                double amt = 0.55 * za;
                L = L * (1.0 - amt) + (cl * 0.7 + flL * 0.5) * amt;
                R = R * (1.0 - amt) + (cr * 0.7 + flR * 0.5) * amt;
            }
            zbufL[i & ZMASK] = (float)L; zbufR[i & ZMASK] = (float)R;
        }
        // SELF-SCRATCH: the substrate breaking — at gesture moments the
        // TRACK ITSELF gets scrubbed, the playhead dragged back and forth
        // through the mix's own recent past (moving tap = real pitch bend,
        // a turntablist's hand on the record). clustered in the crunch zone.
        {
            static const struct { double t, dur, freq, depth; } SCR[5] = {
                {103.4, 0.45, 2.8, 0.20}, {104.9, 0.35, 4.0, 0.12},
                {106.2, 0.55, 2.2, 0.28}, {107.6, 0.30, 5.0, 0.10},
                {109.0, 0.60, 1.8, 0.30},
            };
            for (int sg = 0; sg < 5; sg++) {
                if (tsec < SCR[sg].t || tsec >= SCR[sg].t + SCR[sg].dur) continue;
                double ph = (tsec - SCR[sg].t) / SCR[sg].dur;
                double env2 = sin(M_PI * ph); // hand on, hand off
                double off = SCR[sg].depth * (0.5 - 0.5 * cos(TAU * SCR[sg].freq * (tsec - SCR[sg].t)));
                long j2 = i - (long)(off * SR);
                if (j2 > 0) {
                    L = L * (1.0 - env2) + zbufL[j2 & ZMASK] * env2 * 1.1;
                    R = R * (1.0 - env2) + zbufR[j2 & ZMASK] * env2 * 1.1;
                }
                break;
            }
        }
        // stamp EQ gesture: while the "aesthetic dot computer" super sample
        // speaks, the whole mix squeezes into a telephone band and BLOOMS
        // back open to full range across the phrase
        if (g_stamp0 >= 0 && tsec >= g_stamp0 && tsec < g_stamp1) {
            double u = (tsec - g_stamp0) / (g_stamp1 - g_stamp0);
            double lpf = 2200.0 * pow(8.0, u);   // 2.2k → 17.6k
            double hpf = 650.0 * pow(0.05, u);   // 650 → 32 Hz
            double klp2 = 1.0 - exp(-TAU * lpf / SR);
            double khp2 = 1.0 - exp(-TAU * hpf / SR);
            eqLpL += klp2 * (L - eqLpL); eqHpL += khp2 * (eqLpL - eqHpL);
            eqLpR += klp2 * (R - eqLpR); eqHpR += khp2 * (eqLpR - eqHpR);
            double bpL = eqLpL - eqHpL, bpR = eqLpR - eqHpR;
            // 40ms edge blend so entering the band never clicks
            double win = fmin(1.0, (tsec - g_stamp0) / 0.04);
            L = L + (bpL * 1.15 - L) * win;
            R = R + (bpR * 1.15 - R) * win;
        }
        // substrate amount: full tape print through the opener, LIFTED at the
        // drop (hi-fi, the room turns real), settling to a partial print
        double sa;
        {
            double dropEnd = introEnd + 8 * BAR;
            if (tsec < introEnd) sa = 1.0;
            else if (tsec < dropEnd) sa = 0.30;
            else if (tsec < dropEnd + 1.5) sa = 0.30 + 0.45 * ((tsec - dropEnd) / 1.5);
            else sa = 0.75;
        }
        // print onto the tape substrate: drive + bias, then the hiss floor —
        // blended against the clean path by the substrate amount
        double pl = (tanh(L * 0.95 * SDRV + SBIAS) - tanh(SBIAS)) / SNORM;
        double pr = (tanh(R * 0.95 * SDRV + SBIAS) - tanh(SBIAS)) / SNORM;
        double cl0 = tanh(L * 0.95), cr0 = tanh(R * 0.95);
        double vl = cl0 + (pl - cl0) * sa;
        double vr = cr0 + (pr - cr0) * sa;
        hss ^= hss << 13; hss ^= hss >> 17; hss ^= hss << 5;
        double n1 = ((double)(hss >> 8) / 8388608.0) - 1.0;
        hss ^= hss << 13; hss ^= hss >> 17; hss ^= hss << 5;
        double n2 = ((double)(hss >> 8) / 8388608.0) - 1.0;
        hlpL += (n1 - hlpL) * HLP; hlpR += (n2 - hlpR) * HLP;
        vl = (vl + hlpL * SHISS * sa) * fade;
        vr = (vr + hlpR * SHISS * sa) * fade;
        mixL[i] = (float)vl; mixR[i] = (float)vr;
        double a = fabs(vl); if (a > peak) peak = a;
        a = fabs(vr); if (a > peak) peak = a;
    }
    // ── accelerando outro: the last ~30s keep speeding up as they fade —
    // the train leaves the station, pitch rising with the rails. the tail
    // is re-read at an accelerating rate and N trims to where it runs out.
    {
        long acc0 = N - (long)(30.0 * SR);
        if (acc0 > 0 && acc0 < N) {
            long tin = N - acc0;
            float *tL = malloc(tin * 4), *tR = malloc(tin * 4);
            memcpy(tL, mixL + acc0, tin * 4);
            memcpy(tR, mixR + acc0, tin * 4);
            double pos = 0;
            long o = acc0;
            for (; o < N && pos < tin - 1; o++) {
                double u = (double)(o - acc0) / tin;
                double rate = 1.0 + 0.55 * u * u; // eases in, sprints out
                long j = (long)pos; double fr = pos - j;
                mixL[o] = (float)(tL[j] * (1 - fr) + tL[j + 1] * fr);
                mixR[o] = (float)(tR[j] * (1 - fr) + tR[j + 1] * fr);
                pos += rate;
            }
            free(tL); free(tR);
            N = o; // the ride ends when the tail runs out
        }
    }

    if (peak > 0) {
        double g = 0.85 / peak;
        for (long i = 0; i < N; i++) { mixL[i] *= (float)g; mixR[i] *= (float)g; }
    }

    if (!write_wav_f32_st(out_path, mixL, mixR, N)) {
        fprintf(stderr, "✗ write failed: %s\n", out_path);
        return 1;
    }
    fprintf(stderr, "✓ %s (stereo)\n", out_path);
    return 0;
}
