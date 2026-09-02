// cultremix.c — "whistlegraph cult --- remix (v10, kicks first)", the C engine.
//
// A faithful port of pop/cult/bin/render10.mjs — the ~2200-line Node score —
// onto the fleet-standard single-file C renderer the other /pop lanes carry
// (pop/boombaboom/c, pop/hellsine/c, pop/hopehop/c). Same SR/BPM/BARS, same
// nine acts, same five buses and bus law (drums never duck; the bed breathes
// at 0.50; vox rides bedEnv^0.25 with VOXG 1.42 makeup; the tube takes the
// deep kick+snare pump; the signal layer sits in bedEnv^0.5 at SIGG 2.60),
// same voices — thick electro kick, revKick, wub, sine-bump bass, DTMF pairs,
// bops, clicks/taps, the TrackDrum friction voice and its path-tracing twin,
// pitch-wiggled sample playback, granular stretch, subharmonic doubling —
// and the same ending: measure the peak, ONE linear trim to 0.92, no tanh
// anywhere near the master.
//
// Determinism is the contract: the score's rnd() is the exact JS LCG
// (seed 20220120, seed = (seed*1664525+1013904223) >>> 0, then /2^32 — the
// product stays under 2^53 so the JS double math IS integer math, and a
// uint32 LCG reproduces it bit-for-bit), and the noise voices run the same
// xorshift32 at nseed 987654321 divided by 4294967295 (not 4296…96 — the
// Node file divides by 2^32-1 and so do we). Every jit()/vel()/nrnd() call
// happens in the same order as the Node score, so the two renders place
// every hit on the same sample.
//
// What this v1 intentionally does NOT do (the Node renderer remains the
// source of truth for these):
//   · the events JSON receipt — render10.mjs stays the receipt generator;
//   · --stems / MUTE= / ONLY= debug lanes;
//   · mp3 decoding — the perc/sweep demos load from the ffmpeg caches the
//     Node renderer leaves in pop/cult/out/.cache-*.wav (run the Node render
//     once, or ffmpeg is invoked to build a missing cache).
//
// Build:  bash pop/cult/c/build.sh
// Run:    pop/cult/c/cultremix          # → pop/cult/c/out/cult-remix-c.wav

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <dirent.h>
#include <libgen.h>
#include <sys/stat.h>
#include <time.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#define TAU (2.0 * M_PI)

static const int SR = 48000;
static const double BPM = 120.0;
static const double BEAT = 60.0 / 120.0;   // 0.5 s
static const double BAR = 4.0 * (60.0 / 120.0); // 2.0 s
static const int BARS = 112;               // 224 s
static long N;                             // round((BARS*BAR + 3.2) * SR)

static char LANE[4096];                    // pop/cult, derived from argv[0]

// JS Math.round rounds half toward +inf; C round() rounds half away from
// zero. The score rounds negative quantities (ITD offsets), so match JS.
static inline long jsround(double x) { return (long)floor(x + 0.5); }
static inline double clampd(double v, double a, double b) { return v < a ? a : v > b ? b : v; }
static inline double smoothstep01(double u) { return u <= 0 ? 0 : u >= 1 ? 1 : u * u * (3 - 2 * u); }
static inline double midihz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }
static inline double at(double bar, double beat) { return bar * BAR + beat * BEAT; }
static inline double dmin(double a, double b) { return a < b ? a : b; }
static inline double dmax(double a, double b) { return a > b ? a : b; }
// Repeated instruments breathe in phrases: two mismatched periods, deterministic.
// swing: off-eighths / off-sixteenths late by a share of the gap, more toward the end
static inline double swingAmt(int bar) { return 0.34 * smoothstep01(clampd((bar - 44) / 60.0, 0, 1)); }   // a continuous ramp, ~0.5% a bar
static inline double sw8(int bar) { return swingAmt(bar) * BEAT * 0.5; }
static inline double sw16(int bar) { return swingAmt(bar) * BEAT * 0.25; }
// feel: each bus a few ms off the clock in its own direction, breathing over seven bars
static inline double feelOf(double base, double t) { return base * (1 + 0.5 * sin(TAU * (t / BAR) / 7)); }
static inline double phraseLevel(double bar, double phase, double depth) {
    return clampd(1 + depth * 1.35 * (0.64 * sin(TAU * bar / 9 + phase) + 0.36 * sin(TAU * bar / 5 + phase * 1.7)), 0.66, 1.18);
}

// ── the two generators, bit-exact against the Node file ────────────────
static uint32_t seed = 20220120;           // the cult post date
static inline double rnd(void) { seed = seed * 1664525u + 1013904223u; return (double)seed / 4294967296.0; }
// humanized (live-show pass): scatter 1.7x, velocity spread 1.35x —
// same draw count, the parity stream holds.
static inline double jit(double ms) { return ((rnd() - 0.5) * 2.0 * ms * 2.3) / 1000.0; }   // "felt and played"
static inline double vel(double spread) { return 1.0 - rnd() * spread * 1.7; }

static uint32_t nseed = 987654321;         // xorshift32, MenuBandPercussion's
static inline double nrnd(void) {
    nseed ^= nseed << 13; nseed ^= nseed >> 17; nseed ^= nseed << 5;
    return ((double)nseed / 4294967295.0) * 2.0 - 1.0;
}

// The click amnesty — every voice exits through 10 ms of raised cosine.
static inline double tailFade(long i, long n) {
    double u = (double)(n - 1 - i) / (0.010 * SR);
    return u >= 1 ? 1 : u <= 0 ? 0 : u * u * (3 - 2 * u);
}

// ── WAV IO ─────────────────────────────────────────────────────────────
// Loader mirrors pop/lib/wav.mjs readWavMono: 16/24/32-bit and float32,
// channels averaged down to mono.
static float *load_wav_mono(const char *path, long *out_n, int *out_sr) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t *)malloc(sz);
    if (!buf || (long)fread(buf, 1, sz, f) != sz) { fclose(f); free(buf); return NULL; }
    fclose(f);
    if (sz < 12 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) { free(buf); return NULL; }
    long p = 12; int fmt = 1, channels = 1, bits = 16; uint32_t rate = SR;
    long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        uint32_t s = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8 | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(buf + p, "fmt ", 4)) {
            fmt = buf[p+8] | (buf[p+9] << 8); channels = buf[p+10] | (buf[p+11] << 8);
            rate = (uint32_t)buf[p+12] | (uint32_t)buf[p+13] << 8 | (uint32_t)buf[p+14] << 16 | (uint32_t)buf[p+15] << 24;
            bits = buf[p+22] | (buf[p+23] << 8);
        } else if (!memcmp(buf + p, "data", 4)) { dOff = p + 8; dLen = s; break; }
        p += 8 + s + (s & 1);
    }
    if (!channels || !bits || !dOff) { free(buf); return NULL; }
    const int bps = bits / 8, fb = bps * channels;
    const long frames = dLen / fb;
    float *mono = (float *)malloc(frames * sizeof(float));
    for (long i = 0; i < frames; i++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
            const long o = dOff + (i * channels + c) * bps;
            if (fmt == 3 && bits == 32) { float v; memcpy(&v, buf + o, 4); acc += v; }
            else if (bits == 32) { int32_t v; memcpy(&v, buf + o, 4); acc += (double)v / 2147483648.0; }
            else if (bits == 24) { int32_t v = buf[o] | (buf[o+1] << 8) | ((int32_t)(int8_t)buf[o+2] << 16); acc += (double)v / 8388608.0; }
            else { int16_t v; memcpy(&v, buf + o, 2); acc += (double)v / 32768.0; }
        }
        mono[i] = (float)(acc / channels);
    }
    free(buf);
    *out_n = frames; *out_sr = (int)rate;
    return mono;
}

static void write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb");
    if (!f) { fprintf(stderr, "! cannot write %s\n", path); exit(1); }
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    float *inter = (float *)malloc(n * 2 * sizeof(float));
    for (long i = 0; i < n; i++) { inter[i*2] = L[i]; inter[i*2+1] = R[i]; }
    fwrite(inter, sizeof(float), n * 2, f);
    free(inter); fclose(f);
}

// ── sample bank ────────────────────────────────────────────────────────
// Same law as the Node loader: trim leading silence under 0.008 (keeping
// 2 ms of runway), then normalize the peak to 0.95.
#define MAX_BANK 512
typedef struct { char name[64]; float *s; long n; } Sample;
static Sample BANK[MAX_BANK];
static int bankN = 0;
static int missingWarned = 0;

static Sample *bank_get(const char *name) {
    for (int i = 0; i < bankN; i++) if (!strcmp(BANK[i].name, name)) return &BANK[i];
    return NULL;
}
static int has(const char *name) { return bank_get(name) != NULL; }
static void bank_missing(const char *name) {
    fprintf(stderr, "  ! missing sample %s\n", name); missingWarned++;
}

static void bank_load(const char *name, const char *path) {
    long n = 0; int sr = 0;
    float *raw = load_wav_mono(path, &n, &sr);
    if (!raw) { fprintf(stderr, "  ! missing sample %s\n", path); return; }
    if (sr != SR) {
        // The Node loader hands this to ffmpeg; every bank wav on disk is
        // already 48k so this is a guard, not a lane.
        fprintf(stderr, "  ! %s is %d Hz, resampling via ffmpeg cache\n", path, sr);
        char cache[4400]; snprintf(cache, sizeof cache, "%s/out/.cache-%s-rs.wav", LANE, name);
        char cmd[9000]; snprintf(cmd, sizeof cmd, "ffmpeg -y -v error -i '%s' -ac 1 -ar %d '%s'", path, SR, cache);
        if (system(cmd) != 0) { free(raw); return; }
        free(raw); raw = load_wav_mono(cache, &n, &sr);
        if (!raw) return;
    }
    long a = 0;
    while (a < n - 1 && fabs((double)raw[a]) < 0.008) a++;
    long from = a - jsround(0.002 * SR); if (from < 0) from = 0;
    long tn = n - from;
    double peak = 0;
    for (long i = 0; i < tn; i++) { double v = fabs((double)raw[from + i]); if (v > peak) peak = v; }
    double g = peak > 1e-6 ? 0.95 / peak : 1.0;
    float *out = (float *)malloc(tn * sizeof(float));
    for (long i = 0; i < tn; i++) out[i] = (float)((double)raw[from + i] * g);
    free(raw);
    Sample *slot = bank_get(name);
    if (!slot) {
        if (bankN >= MAX_BANK) { fprintf(stderr, "! bank full\n"); exit(1); }
        slot = &BANK[bankN++];
        snprintf(slot->name, sizeof slot->name, "%s", name);
    } else free(slot->s);
    slot->s = out; slot->n = tn;
}

static int cmp_str(const void *a, const void *b) { return strcmp(*(const char *const *)a, *(const char *const *)b); }

static void bank_load_dir(const char *rel) {
    char dirp[4400]; snprintf(dirp, sizeof dirp, "%s/%s", LANE, rel);
    DIR *d = opendir(dirp); if (!d) return;
    char *names[MAX_BANK]; int cnt = 0;
    struct dirent *e;
    while ((e = readdir(d))) {
        size_t l = strlen(e->d_name);
        if (l > 4 && !strcmp(e->d_name + l - 4, ".wav") && cnt < MAX_BANK) names[cnt++] = strdup(e->d_name);
    }
    closedir(d);
    qsort(names, cnt, sizeof(char *), cmp_str);
    for (int i = 0; i < cnt; i++) {
        char nm[64]; snprintf(nm, sizeof nm, "%.*s", (int)(strlen(names[i]) - 4), names[i]);
        char path[4800]; snprintf(path, sizeof path, "%s/%s", dirp, names[i]);
        bank_load(nm, path);
        free(names[i]);
    }
}

// The perc/sweep demos are mp3 in pop/demos/samples; the Node loader caches
// them as 48k mono wav in pop/cult/out/.cache-<name>.wav and we read the
// same caches (building any missing one with ffmpeg, same command).
static void bank_load_demo(const char *name, const char *mp3) {
    char cache[4400]; snprintf(cache, sizeof cache, "%s/out/.cache-%s.wav", LANE, name);
    struct stat st;
    if (stat(cache, &st) != 0) {
        char src[4800]; snprintf(src, sizeof src, "%s/../demos/samples/%s", LANE, mp3);
        char cmd[14000]; snprintf(cmd, sizeof cmd, "ffmpeg -y -v error -i '%s' -ac 1 -ar %d '%s'", src, SR, cache);
        if (system(cmd) != 0) { fprintf(stderr, "  ! missing sample %s\n", src); return; }
    }
    bank_load(name, cache);
}

// ── buses ──────────────────────────────────────────────────────────────
enum { BUS_MUSIC = 0, BUS_DRUMS, BUS_VOX, BUS_TUBE, BUS_SIG };
static const double VOXG = 1.28, TUBEG = 1.00, SIGG = 2.60;   /* VOXG was 1.42: the vocals sit back */
static float *musicL, *musicR, *drumsL, *drumsR, *voxL, *voxR,
             *tubeL, *tubeR, *sigL, *sigR, *airL, *airR,
             *sideB, *sideV, *sideT, *sideS, *dlySend;

typedef struct { long itd; double gl, gr; int on; } Sp;
static inline Sp spatial(double az) {
    Sp sp; sp.itd = jsround(0.00027 * SR * sin(az));
    double shadow = 0.35 * sin(az);
    sp.gl = 1 - shadow; sp.gr = 1 + shadow; sp.on = 1;
    return sp;
}
static const Sp NOSP = {0, 0, 0, 0};

// ── whistlecultspatial: room choreography (--spatial) ──
// The Juke's room split puts LEFT on one Mac and RIGHT on another, so pan
// stops being imaging and becomes ADDRESS: which machine speaks. --spatial
// re-choreographs every emit for that stage: drums trade machines beat by
// beat, the signal layer (dots, DTMF, phones) answers two beats behind from
// the opposite side, the band slowly orbits the room with the tube layer
// counter-rotating against it (the crossing reads as spin), and the vox
// stay near center so the withheld words, when they finally arrive, reach
// both listeners at once. Orbits tighten through the act-VI build and relax
// after the words land at bar 76. Same score, same voices, same master
// chain — only the address changes.
static int SPATIAL = 0;
// --radio: the FM fantasy — everything a shade closer to center, the bed
// breathing off the voice (not only the kick), the voice sat back into the
// band, and a continuous sub throughline under the whole record.
static int RADIO = 0;
static inline double choreoPan(int bus, long i, double pan) {
    double t = (double)i / SR;
    const double beatSec = 60.0 / BPM;
    const double barSec = beatSec * 4.0;
    long beatIx = (long)floor(t / beatSec);
    double period = t < 120 ? 16.0 : (t < 152 ? 10.0 : 24.0);
    switch (bus) {
    case BUS_DRUMS: {
        // Beats back and forth: the grid itself hops between machines.
        double side = (beatIx & 1) ? 0.92 : -0.92;
        return clampd(side * 0.85 + pan * 0.15, -1, 1);
    }
    case BUS_SIG: {
        // Call and response: the dot crowd answers from the other side,
        // two beats behind the drums' phase.
        double side = ((beatIx >> 1) & 1) ? -0.9 : 0.9;
        return clampd(side * 0.7 + pan * 0.3, -1, 1);
    }
    case BUS_VOX:
        // The words reach both listeners: center with the gentlest lean —
        // the lyric narrative stays planted while the band moves.
        return clampd(pan * 0.3 + 0.15 * sin(2 * M_PI * t / (8 * barSec)), -1, 1);
    case BUS_TUBE:
        return clampd(pan * 0.3 - 0.8 * sin(2 * M_PI * t / period), -1, 1);
    default:
        // The band orbits the room, blending one machine into the other.
        return clampd(pan * 0.4 + 0.62 * sin(2 * M_PI * t / period), -1, 1);
    }
}

static inline void emit(int bus, long i, double mono, double pan, Sp sp, double sideAmt, double dly) {
    if (i < 0 || i >= N) return;
    if (SPATIAL) pan = choreoPan(bus, i, pan);
    if (RADIO) pan *= 0.75;   // radio: denser center, no hard edges
    double a = (M_PI / 4.0) * (1.0 + pan);
    double cl = cos(a), cr = sin(a);
    switch (bus) {
        case BUS_DRUMS: drumsL[i] += (float)(mono * cl); drumsR[i] += (float)(mono * cr); break;
        case BUS_VOX:   voxL[i]   += (float)(mono * cl); voxR[i]   += (float)(mono * cr); break;
        case BUS_TUBE:  tubeL[i]  += (float)(mono * cl); tubeR[i]  += (float)(mono * cr); break;
        case BUS_SIG:   sigL[i]   += (float)(mono * cl); sigR[i]   += (float)(mono * cr); break;
        default:        musicL[i] += (float)(mono * cl); musicR[i] += (float)(mono * cr); break;
    }
    if (dly != 0) dlySend[i] += (float)(mono * dly);
    if (sp.on && sideAmt != 0) {
        long li = i + sp.itd, ri = i - sp.itd;
        double l = (li >= 0 && li < N) ? mono * sp.gl : 0;
        double r = (ri >= 0 && ri < N) ? mono * sp.gr : 0;
        double s = 0.5 * (l - r) * sideAmt;
        if (bus == BUS_VOX) sideV[i] += (float)s;
        else if (bus == BUS_TUBE) sideT[i] += (float)s;
        else if (bus == BUS_SIG) sideS[i] += (float)s;
        else sideB[i] += (float)s;
    }
}

// ── sidechain ──────────────────────────────────────────────────────────
#define MAX_TRIG 4096
static double kicksT[MAX_TRIG]; static int kicksN = 0;
static double snaresT[MAX_TRIG]; static int snaresN = 0;

typedef struct { double t, depth, atk, rel; } Trig;
static void buildEnv(float *e, const Trig *tr, int cnt) {
    for (long i = 0; i < N; i++) e[i] = 1.0f;
    const long pre = jsround(0.010 * SR);
    for (int k = 0; k < cnt; k++) {
        const long i0 = jsround(tr[k].t * SR) - pre;
        const long span = pre + jsround((tr[k].atk + tr[k].rel) * SR);
        for (long i = 0; i < span; i++) {
            const long j = i0 + i;
            if (j < 0) continue;
            if (j >= N) break;
            const double dt = (double)i / SR - 0.010;
            double g;
            if (dt < tr[k].atk) g = 1 - tr[k].depth * clampd((dt + 0.010) / (0.010 + tr[k].atk), 0, 1);
            else { double u = clampd((dt - tr[k].atk) / tr[k].rel, 0, 1); g = (1 - tr[k].depth) + tr[k].depth * smoothstep01(u); }
            if (g < e[j]) e[j] = (float)g;
        }
    }
}

// ── the thick electro kick ─────────────────────────────────────────────
static const double SATN_DRIVE = 2.4;
static void kicks(double t, double gain, double weight, double pitch, double sharp) {
    if (kicksN < MAX_TRIG) kicksT[kicksN++] = t;
    const double SATN = tanh(SATN_DRIVE);
    const long n = jsround(0.52 * SR), i0 = jsround(t * SR);
    double ph = 0, sub = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double f = (47 + 153 * exp(-u * 62)) * pitch;
        ph += (TAU * f) / SR;
        sub += (TAU * 44 * pitch) / SR;
        const double slam = exp(-u * (34 + 30 * sharp)), tail = exp(-u * (7.0 + 5 * sharp));
        const double env = (0.62 * slam + 0.52 * tail) * dmin(1, u / 0.0009);
        const double body = tanh(sin(ph) * env * 2.4) / SATN;
        const double low = sin(sub) * exp(-u * 5.6) * 0.40 * weight;
        const double click = (exp(-u * 300) * 0.13 * sin(TAU * 1600 * u)
            + exp(-u * 760) * 0.075 * sin(TAU * 3900 * u)) * (1 + 1.6 * sharp);
        emit(BUS_DRUMS, i0 + i, (body + low + click) * 0.74 * gain * tailFade(i, n), 0, NOSP, 0, 0);
    }
}

static void kickp(double t, double gain, double weight, double pitch) { kicks(t, gain, weight, pitch, 0); }
static void kick(double t, double gain, double weight) { kicks(t, gain, weight, 1.0, 0); }

// revKick: the sweep run backwards, rising into the downbeat it announces.
static void revKick(double t, double dur, double gain) {
    const long n = jsround(dur * SR), i0 = jsround((t - dur) * SR);
    double p = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / n;
        const double f = 47 + (200 - 47) * u * u;
        p += (TAU * f) / SR;
        const double env = pow(u, 2.4);
        const double v = tanh(sin(p) * (1 + 1.5 * u)) * 0.5;
        emit(BUS_DRUMS, i0 + i, v * env * gain * tailFade(i, n), 0, NOSP, 0.2, 0);
    }
}

// wub: the root's sub octave pumped by a 4 Hz amplitude-and-lowpass LFO.
static void wub(double t, int midi, double bars, double gain, double rate) {
    const long n = jsround(bars * BAR * SR), i0 = jsround(t * SR);
    const double f = midihz(midi);
    double p = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        p += (TAU * f) / SR;
        const double lfo = 0.5 - 0.5 * cos(TAU * rate * u);
        const double raw = tanh(sin(p) * (1.2 + 2.2 * lfo));
        const double k = 0.12 + 0.55 * lfo;
        lp += k * (raw - lp);
        const double env = dmin(1, u / 0.02);
        emit(BUS_MUSIC, i0 + i, lp * env * 0.5 * gain * tailFade(i, n), 0, NOSP, 0.2, 0);
    }
}

// A wooden tap: filtered noise, fast decay, two shallow body resonances.
// form 0 = the wooden tap; 1 = the clop (a hollow tock under the wood); 2 = the clip
static void woodTapF(double t, double gain, double pan, int form) {
    const long n = jsround(0.075 * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double lp = 0, b1 = 0, b2 = 0, ph = 0;
    const double a1 = 1 - exp((-TAU * (form == 1 ? 1400 : 900)) / SR), a2 = 1 - exp((-TAU * (form == 1 ? 620 : 1900)) / SR);
    const double aw = 1 - exp((-TAU * 5200) / SR);
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double env = exp(-u * (form == 1 ? 70 : 105)) * dmin(1, u / 0.0006);
        const double white = nrnd();
        lp += aw * (white - lp);
        b1 += a1 * (lp - b1); b2 += a2 * (lp - b2);
        double v = b1 * 0.62 + b2 * 0.30 + lp * 0.16;
        if (form == 1) { ph += (TAU * (900 + 350 * exp(-u * 60))) / SR; v = v * 0.8 + sin(ph) * 0.42 * exp(-u * 48); }
        else if (form == 2) v = clampd(v * 3.2, -0.55, 0.55);
        emit(BUS_DRUMS, i0 + i, v * env * gain * 1.5 * tailFade(i, n), pan, sp, 0.55, 0);
    }
}
static void woodTap(double t, double gain, double pan) { woodTapF(t, gain, pan, 0); }
static inline int percForm(double t) { const int bar = (int)floor(t / BAR); return bar < 48 ? (bar >> 2) % 3 : 1 + ((bar >> 2) % 2); }

// The snare is a quiet woody tap — mostly the pump's second trigger.
static void snare(double t, double gain) {
    t += feelOf(0.007, t);   // the snare sits behind the kick
    if (snaresN < MAX_TRIG) snaresT[snaresN++] = t;
    woodTapF(t, 0.17 * gain * (percForm(t) == 2 ? 1.25 : 1), 0.26, percForm(t));
}

// Sine bumps: fundamental + sub octave + a whisper of the 2nd.
#define NO_SLIDE (-999)
static void bass(double t, int midi, double dur, double gain, int slideFrom) {
    t += feelOf(0.005, t);   // the bass a touch late
    const long n = jsround((dur + 0.12) * SR), i0 = jsround(t * SR);
    const double f1 = midihz(midi), f0 = slideFrom != NO_SLIDE ? midihz(slideFrom) : f1;
    double p1 = 0, p2 = 0, p3 = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double glide = smoothstep01(clampd(u / 0.075, 0, 1));
        const double f = f0 + (f1 - f0) * glide;
        p1 += (TAU * f) / SR; p2 += (TAU * f * 0.5) / SR; p3 += (TAU * f * 2) / SR;
        double env = dmin(1, u / 0.012);
        if (u > dur) env *= dmax(0, 1 - (u - dur) / 0.12);
        const double s = sin(p1) + 0.52 * sin(p2) + 0.10 * sin(p3);
        lp += 0.50 * (s - lp);
        emit(BUS_MUSIC, i0 + i, lp * 0.40 * env * gain * tailFade(i, n), 0, NOSP, 0, 0);
    }
}

// Harmonized sines — the pad and the stab.
static void sines(double t, const int *midis, int nm, double dur, double gain,
                  double pan, double sideAmt, double bright, double dly, double attack) {
    const long n = jsround((dur + 0.40) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double ph[8][3] = {{0}};
    double lp = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        double s = 0;
        for (int v = 0; v < nm; v++) {
            const double f = midihz(midis[v]);
            ph[v][0] += (TAU * f) / SR;
            ph[v][1] += (TAU * f * 1.0035) / SR;
            ph[v][2] += (TAU * f * 2) / SR;
            s += sin(ph[v][0]) + 0.7 * sin(ph[v][1]) + 0.08 * bright * sin(ph[v][2]);
        }
        s /= nm * 1.8;
        lp += 0.28 * (s - lp);
        double env = dmin(1, u / attack);
        if (u > dur) env *= dmax(0, 1 - (u - dur) / 0.40);
        emit(BUS_MUSIC, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, sideAmt, dly);
    }
}

// ── the guitar: Karplus-Strong, its own seed per note ─────────────────
// String excitation owns its seed (t*1000 and midi hashed the way the Node
// file does it), so adding a chord cannot shift the drum-friction noise.
static void guitar(double t, int midi, double dur, double gain, double pan, double fb) {
    long period = jsround(SR / midihz(midi) - 0.5); if (period < 2) period = 2;   // half-sample delay compensated
    const long n = jsround(dur * SR), i0 = jsround(t * SR);
    float *line = (float *)malloc(period * sizeof(float));
    const uint64_t pa = (uint64_t)(int64_t)jsround(t * 1000) * 2654435761ull;
    const uint64_t pb = (uint64_t)(int64_t)midi * 2246822519ull;
    uint32_t gseed = (uint32_t)pa ^ (uint32_t)pb;
    for (long k = 0; k < period; k++) {
        gseed ^= gseed << 13; gseed ^= gseed >> 17; gseed ^= gseed << 5;
        line[k] = (float)(((double)gseed / 4294967295.0) * 2.0 - 1.0);
    }
    const Sp sp = spatial(pan * 1.2);
    long idx = 0; double prev = 0, lp = 0;
    for (long i = 0; i < n; i++) {
        const double v = line[idx];
        line[idx] = (float)(fb * 0.5 * (v + prev));
        prev = v;
        idx = (idx + 1) % period;
        const double d1 = tanh(v * 6.5 + 0.35) - tanh(0.35);
        const double d2 = tanh(d1 * 1.8) * 0.5;
        lp += 0.16 * (d2 - lp);
        const double env = dmin(1, (double)i / (0.002 * SR));
        emit(BUS_MUSIC, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, 0.4, 0.20);
    }
    free(line);
}
// A chord is four separately modelled strings; the 17 ms rake is the hand.
static void guitarChord(double t, const int *midis, int nm, double dur, double gain, double pan, int up) {
    for (int k = 0; k < nm; k++) {
        const int note = up ? midis[nm - 1 - k] : midis[k];
        guitar(t + k * 0.017, note, dur - k * 0.025,
               gain * (k == 0 ? 1 : 0.72 + 0.055 * sin(t * 2.3 + k * 1.9)),
               pan + (k - 1.5) * 0.055, 0.9972);
    }
}
// Short runs tearing out of the chord wall, accelerating into the downbeat.
static void guitarShred(double t, const int *notes, int nn, double gain, double pan) {
    double u = t;
    for (int k = 0; k < nn; k++) {
        const int last = k == nn - 1;
        const double bow = 0.76 + 0.24 * sin(M_PI * k / (double)(nn - 1 > 1 ? nn - 1 : 1));
        const double accent = k % 3 == 2 ? 1.08 : 0.94;
        guitar(u, notes[k], last ? 1.45 : 0.30, gain * bow * accent * (last ? 1.22 : 1),
               pan * (k & 1 ? -1 : 1), last ? 0.9974 : 0.9948);
        u += (0.115 - 0.0045 * k) * BEAT;
    }
}
// The strumming hand: down strums take the whole voicing from the lowest
// string; up strums start at the top, catch only the upper strings, faster,
// shorter, quieter. `from` skips the pattern's first strokes (odd bars).
static const struct { double beat; int down; double force; } STRUM[6] = {
    {0.00, 1, 1.00}, {0.75, 1, 0.60}, {1.50, 0, 0.46},
    {2.00, 0, 0.42}, {2.50, 1, 0.80}, {3.25, 0, 0.40}};
static void guitarStrum(double t, const int *midis, int nm, double gain, double pan, int from, double rake) {
    for (int p = from; p < 6; p++) {
        const int down = STRUM[p].down;
        const double beat = STRUM[p].beat, force = STRUM[p].force;
        const int cnt = down ? nm : nm - 1;
        const double step = down ? rake : rake * 0.58;
        for (int k = 0; k < cnt; k++) {
            const int note = down ? midis[k] : midis[nm - 1 - k];
            guitar(t + beat * BEAT + k * step, note,
                   down ? 1.30 - 0.06 * k : 0.60 - 0.04 * k,
                   gain * force * (down ? 1 : 0.76) * (1 - 0.05 * k),
                   pan + (k - (cnt - 1) / 2.0) * 0.05,
                   down ? 0.9968 : 0.9950);
        }
    }
}

// ── air: the room, on its own generator and its own bus ────────────────
static uint32_t airSeed = 0x5f3a71c9u;
static inline double arnd(void) { airSeed = airSeed * 1664525u + 1013904223u; return (double)airSeed / 4294967296.0 - 0.5; }
static void air(double t, double dur, double gain, double body, double hiss, double wander, double deepMul) {
    const long i0 = jsround(t * SR), n = jsround(dur * SR);
    long inN = jsround(dmin(dur * 0.30, 1.4) * SR); if (inN < 1) inN = 1;
    long outN = jsround(dmin(dur * 0.34, 1.9) * SR); if (outN < 1) outN = 1;
    const double aL = (M_PI / 4) * (1 - 0.62), aR = (M_PI / 4) * (1 + 0.62);
    const double PL_L = cos(aL), PL_R = sin(aL), PR_L = cos(aR), PR_R = sin(aR);
    double lL = 0, lR = 0, hL = 0, hR = 0, rL = 0, rR = 0, qL = 0, qR = 0;
    for (long i = 0; i < n; i++) {
        const double s = (double)i / SR;
        const double env = smoothstep01(dmin(1, (double)i / inN)) * smoothstep01(dmin(1, (double)(n - i) / outN))
            * (1 - wander + wander * (0.5 + 0.5 * sin(TAU * 0.055 * s + t * 0.7)));
        const double deep = 0.55 + 0.45 * sin(TAU * 0.023 * s + t * 0.31);   // the rumble's own clock
        const double nL = arnd(), nR = arnd();
        lL += body * (nL - lL); lR += body * (nR - lR);
        hL += hiss * (nL - hL); hR += hiss * (nR - hR);
        rL += 0.006 * (nL - rL); qL += 0.006 * (rL - qL);   // the deep scary air: ~45 Hz, twice
        rR += 0.006 * (nR - rR); qR += 0.006 * (rR - qR);
        const double g = env * gain;
        const long j = i0 + i;
        if (j >= 0 && j < N) {
            const double vL = (lL * 2.6 + (nL - hL) * 0.30 + qL * 14 * deep * deepMul) * g;
            const double vR = (lR * 2.6 + (nR - hR) * 0.30 + qR * 14 * deep * deepMul) * g;
            airL[j] += (float)(vL * PL_L + vR * PR_L);
            airR[j] += (float)(vL * PL_R + vR * PR_R);
        }
    }
}

// ── the staircase: the Shepard tone's spatial cousin ───────────────────
// `voices` staggered copies, each windowed silent at both edges, so the
// word appears to keep sliding one way for as long as it runs.
static void staircasePanX(const char *name, double t, double gain, int voices, double cycles,
                          int dir, double semis, double dark, int bus, double dly, double dur, int reverse) {
    if (bus == BUS_VOX && t < 29 * BAR - 0.02) return;
    Sample *smp = bank_get(name);
    if (!smp) { bank_missing(name); return; }
    const long len = smp->n;
    float *rev = NULL;
    if (reverse) { rev = malloc(len * sizeof(float)); for (long r = 0; r < len; r++) rev[r] = smp->s[len - 1 - r]; }
    const float *s = reverse ? rev : smp->s;
    const double step = pow(2, semis / 12.0);
    long n = jsround((dur > 0 ? dur : (double)len / SR / step) * SR);
    const long room = N - jsround(t * SR);
    if (room < n) n = room;
    if (n <= 0) { free(rev); return; }
    const long i0 = jsround(t * SR);
    const double norm = gain * sqrt(2.0 / voices);
    double lp = 0, pos = 0;
    for (long i = 0; i < n; i++) {
        const long q = (long)pos;
        if (q + 1 >= len) break;
        double v = s[q] + (s[q + 1] - s[q]) * (pos - q);
        if (dark > 0) { lp += (1 - dark) * (v - lp); v = lp; }
        const double u = (double)i / n;
        for (int k = 0; k < voices; k++) {
            const double phase = fmod(u * cycles + (double)k / voices, 1.0);
            const double win = 0.5 - 0.5 * cos(TAU * phase);
            emit(bus, i0 + i, v * norm * win, dir * (2 * phase - 1), NOSP, 0, dly * win);
        }
        pos += step;
    }
    free(rev);
}
static void staircasePan(const char *name, double t, double gain, int voices, double cycles,
                         int dir, double semis, double dark, int bus, double dly, double dur) {
    staircasePanX(name, t, gain, voices, cycles, dir, semis, dark, bus, dly, dur, 0);
}

// ── the signal layer: DTMF beeps, bops, clicks, taps ───────────────────
static const double DTMF_ROW[4] = {697, 770, 852, 941};
static const double DTMF_COL[4] = {1209, 1336, 1477, 1633};
// keypad: '1'..'9','*','0','#' → row/col
static int dtmf_key(char digit, int *row, int *col) {
    const char *keys = "123456789*0#";
    static const int R[12] = {0,0,0,1,1,1,2,2,2,3,3,3};
    static const int C[12] = {0,1,2,0,1,2,0,1,2,0,1,2};
    const char *p = strchr(keys, digit);
    if (!p) return 0;
    *row = R[p - keys]; *col = C[p - keys];
    return 1;
}
static const char CULT_DIAL[4] = {'2', '8', '5', '8'};   // C · U · L · T

static void beepx(double t, double f1, double f2, double dur,
                  double gain, double pan, double side, double dly, int bus,
                  double attack, double rel) {
    const long n = jsround((dur + rel) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double p1 = 0, p2 = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        p1 += (TAU * f1) / SR;
        if (f2) p2 += (TAU * f2) / SR;
        const double atk = 0.5 - 0.5 * cos(M_PI * clampd(u / attack, 0, 1));
        const double off = u > dur ? 0.5 + 0.5 * cos(M_PI * clampd((u - dur) / rel, 0, 1)) : 1;
        const double s = (sin(p1) + (f2 ? 0.85 * sin(p2) : 0)) * (f2 ? 0.5 : 0.9);
        emit(bus, i0 + i, s * atk * off * 0.30 * gain * tailFade(i, n), pan, sp, side, dly);
    }
}
static void beep(double t, double f1, double f2, double dur,
                 double gain, double pan, double side, double dly, int bus) {
    beepx(t, f1, f2, dur, gain, pan, side, dly, bus, 0.003, 0.012);
}
static void dtmfx(double t, char digit, double dur, double gain, double pan, double side, double dly,
                  double attack, double rel) {
    int r, c;
    if (!dtmf_key(digit, &r, &c)) return;
    beepx(t, DTMF_ROW[r], DTMF_COL[c], dur, gain, pan, side, dly, BUS_SIG, attack, rel);
}
static void dtmf(double t, char digit, double dur, double gain, double pan, double side, double dly) {
    dtmfx(t, digit, dur, gain, pan, side, dly, 0.003, 0.012);
}

// A "bop": a sine that drops a fifth in 60 ms. UI, not music.
static void bop(double t, double f, double gain, double pan, double side, double dur, double dly) {
    if (SPATIAL && t >= 24 * BAR) return; // room cut: once the words are in,
                                          // the lyric layer is ONLY the sentence
    if (SPATIAL) gain *= 0.60;   // (intro texture stays, recessed)
    const long n = jsround((dur + 0.02) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double p = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double ff = f * (1 - 0.33 * smoothstep01(clampd(u / 0.060, 0, 1)));
        p += (TAU * ff) / SR;
        const double env = dmin(1, u / 0.002) * exp(-u * 26);
        emit(BUS_SIG, i0 + i, sin(p) * env * 0.34 * gain * tailFade(i, n), pan, sp, side, dly);
    }
}

// Clicks and taps: bandpassed xorshift noise, 4 ms and 18 ms.
static void noiseHit(double t, double gain, double pan, double side, double dur,
                     double tone, double q, double dly) {
    const long n = jsround((dur + 0.006) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double hp = 0, prev = 0, lp = 0, p = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double w = nrnd();
        hp = 0.86 * (hp + w - prev); prev = w;
        lp += 0.42 * (hp - lp);
        double v = lp;
        if (tone) { p += (TAU * tone) / SR; v = v * (1 - q) + sin(p) * q * exp(-u * 120); }
        const double env = dmin(1, u / 0.0009) * exp(-u * (tone ? 150 : 620));
        emit(BUS_SIG, i0 + i, v * env * 1.10 * gain * tailFade(i, n), pan, sp, side, dly);
    }
}
static void click(double t, double gain, double pan, double side, double dly) {
    noiseHit(t, gain, pan, side, 0.004, 0, 0.35, dly);
}
static void tap(double t, double gain, double pan, double side, double dly) {
    noiseHit(t, gain, pan, side, 0.018, percForm(t) == 1 ? 1500 : 900, percForm(t) == 1 ? 0.62 : 0.40, dly);
}

// ── TrackDrum friction — the path-tracing voice ────────────────────────
static double surfaceDistance(double x, double y) {
    const double sx = (x - 0.5) * 2, sy = (y - 0.5) * 2;
    const double hw = 0.82, hh = 0.50, corner = 0.055;
    const double qx = fabs(sx) * hw - (hw - corner);
    const double qy = fabs(sy) * hh - (hh - corner);
    const double outside = hypot(dmax(qx, 0), dmax(qy, 0));
    const double inside = dmin(dmax(qx, qy), 0);
    const double signedd = outside + inside - corner;
    const double depth = dmax(0, -signedd);
    return dmax(0, dmin(1, 1 - depth / 0.5));
}
static inline double sstep(double a, double b, double v) {
    double u = dmax(0, dmin(1, (v - a) / (b - a))); return u * u * (3 - 2 * u);
}
static inline double mixv(double a, double b, double m) { return a + (b - a) * m; }

enum { PATH_EDGE, PATH_EDGEBACK, PATH_SPIRAL, PATH_SPIRALIN, PATH_CORNER, PATH_SCRUB };
static void path_at(int path, double u, double *x, double *y) {
    switch (path) {
        case PATH_EDGE:     *x = 0.12 + 0.76 * u; *y = 0.72 - 0.03 * sin(u * M_PI * 3); break;
        case PATH_EDGEBACK: *x = 0.88 - 0.76 * u; *y = 0.28 + 0.03 * sin(u * M_PI * 3); break;
        case PATH_SPIRALIN: { double a = (1 - u) * M_PI * 5.2, r = 0.52 - 0.30 * u;
                              *x = 0.5 + cos(a) * r; *y = 0.5 + sin(a) * r * 0.78; break; }
        case PATH_CORNER:   *x = 0.5 + 0.40 * u * u; *y = 0.5 + 0.34 * u * u; break;
        case PATH_SCRUB:    *x = 0.14 + 0.72 * u; *y = 0.5 + 0.40 * sin(u * M_PI * 6.5); break;
        default:            { double a = u * M_PI * 5.2, r = 0.22 + 0.30 * u;
                              *x = 0.5 + cos(a) * r; *y = 0.5 + sin(a) * r * 0.78; break; }
    }
}

enum { SHAPE_SLIDE, SHAPE_SKID, SHAPE_DRAG };

static void frictionPath(double t, double dur, int path, double gain, double side,
                         double dly, double rough, int synthetic, double rel,
                         int shape, double speed) {
    const long n = jsround((dur * 0.55 + rel * 0.7 + 0.06) * SR), i0 = jsround(t * SR);
    const double span = dur * 0.55;
    const double atkA = 1 - exp(-1 / (SR * 0.0025));
    const double relA = 1 - exp(-1 / (SR * rel * 0.7));
    double lvl = 0, nf = 0, ns = 0, ph = 0, px = 0.5, py = 0.5;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR, x = u / span;
        const double k = dmax(0, dmin(1, x));
        double ptx, pty; path_at(path, k, &ptx, &pty);
        const double dx = ptx - px, dy = pty - py; px = ptx; py = pty;
        const double travel = hypot(dx * 1.64, dy) * SR;
        const double d = surfaceDistance(ptx, pty);
        const double toSnare = sstep(0.23, 0.31, d), toRim = sstep(0.40, 0.48, d);
        const double toHat = sstep(0.62, 0.70, d) * 0.35;
        const double toClick = sstep(0.88, 0.965, d) * 0.25;
        double cut = mixv(175, 430, toSnare);
        cut = mixv(cut, 680, toRim); cut = mixv(cut, 1250, toHat); cut = mixv(cut, 2050, toClick);
        double res = mixv(mixv(mixv(mixv(48, 90, toSnare), 185, toRim), 360, toHat), 560, toClick);
        double seam = 0;
        const double seams[4] = {0.30, 0.46, 0.64, 0.88};
        for (int sdx = 0; sdx < 4; sdx++) {
            double g = exp(-pow((d - seams[sdx]) / 0.045, 2));
            if (g > seam) seam = g;
        }
        const double ripple = 1 + 0.055 * sin(((ptx - 0.5) * 2 * 2.7 + (pty - 0.5) * 2 * 3.9) * M_PI);
        const double lift = 1 + dmin(0.55, travel * 0.00016) * speed;
        cut = cut * (1 + seam * 0.20) * lift;
        res = dmin(255, res * ripple * (1 + seam * 0.06));
        if (synthetic) { cut = 1200 + 9000 * d; res = 1100 + cut * 0.42; }
        double target = 0;
        if (x < 1) target = shape == SHAPE_SKID ? exp(-x * 4.2)
            : shape == SHAPE_DRAG ? pow(x < 0.92 ? x / 0.92 : 1, 1.6)
                : sin(M_PI * dmin(1, x));
        target *= 0.55 + 0.45 * dmin(1, travel * 0.0011);
        if (shape == SHAPE_DRAG) target *= 0.62;
        lvl += (target > lvl ? atkA : relA) * (target - lvl);
        const double fa = 1 - exp((-TAU * cut) / SR);
        const double sa = 1 - exp((-TAU * dmax(35, cut * 0.18)) / SR);
        const double white = nrnd();
        nf += fa * (white - nf); ns += sa * (white - ns);
        const double band = nf - ns;
        const double motion = synthetic ? 1 : 1 + tanh(band * 8) * 0.055;
        ph += (res * motion) / SR; if (ph >= 1) ph -= floor(ph);
        const double carrier = sin(TAU * ph);
        const double gnarl = tanh(band * (5 + rough * 5));
        const double texture = synthetic ? nf * carrier * 1.35
            : gnarl * 0.44 + carrier * (0.08 + fabs(gnarl) * (0.42 + rough * 0.30));
        const double pan = (ptx - 0.5) * 1.5;
        emit(BUS_DRUMS, i0 + i, texture * lvl * 0.15 * gain * tailFade(i, n),
             pan, spatial(pan * 1.2), side, dly);
    }
}

// …and its parameter-sweep twin: the Continuous membrane friction voice,
// SCUFF constants applied exactly as the Node file leaves them.
static void friction(double t, double dur, int shape, double gain, double pan,
                     double side, double dly, double cut0, double cut1,
                     double res0, double res1, double rough, int synthetic, double rel) {
    // SCUFF_CUT = SCUFF_RES = 1.0, SCUFF_FAST = 0.55, rel *= 0.7
    dur *= 0.55; rel *= 0.7;
    const double c1 = cut1 > 0 ? cut1 : cut0, r1 = res1 > 0 ? res1 : res0;
    const long n = jsround((dur + rel + 0.06) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    const double atkA = 1 - exp(-1 / (SR * (synthetic ? 0.006 : 0.0025)));
    const double relA = 1 - exp(-1 / (SR * rel));
    double lvl = 0, nf = 0, ns = 0, ph = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR, x = u / dur;
        double target = 0;
        if (x < 1) {
            target = shape == SHAPE_SKID ? exp(-x * 4.2)
                : shape == SHAPE_SLIDE ? sin(M_PI * x)
                    : pow(smoothstep01(clampd(x / 0.92, 0, 1)), 1.6);   // drag
        }
        lvl += (target > lvl ? atkA : relA) * (target - lvl);
        const double k = clampd(x, 0, 1);
        const double cut = cut0 + (c1 - cut0) * k;
        const double res = res0 + (r1 - res0) * k;
        const double fa = 1 - exp((-TAU * cut) / SR);
        const double sa = 1 - exp((-TAU * dmax(35, cut * 0.18)) / SR);
        const double white = nrnd();
        nf += fa * (white - nf);
        ns += sa * (white - ns);
        const double band = nf - ns;
        const double motion = synthetic ? 1 : 1 + tanh(band * 8) * 0.055;
        ph += (res * motion) / SR;
        if (ph >= 1) ph -= floor(ph);
        const double carrier = sin(TAU * ph);
        double texture;
        if (synthetic) texture = nf * carrier * 1.35;
        else {
            const double gnarl = tanh(band * (5 + rough * 5));
            texture = gnarl * 0.44 + carrier * (0.08 + fabs(gnarl) * (0.42 + rough * 0.30));
        }
        emit(BUS_DRUMS, i0 + i, texture * lvl * 0.15 * gain * tailFade(i, n), pan, sp, side, dly);
    }
}

// SOS, beeped — act V's answer, the same figure coming back as a machine.
static void beepSOS(double t, double gain, double pan, double dly) {
    const double row = DTMF_ROW[1], col = DTMF_COL[1];
    static const double seq[9][2] = {
        {0.00, 0.09}, {0.20, 0.09}, {0.40, 0.09},
        {0.70, 0.26}, {1.10, 0.26}, {1.50, 0.26},
        {1.95, 0.09}, {2.15, 0.09}, {2.35, 0.09}};
    for (int i = 0; i < 9; i++)
        beep(t + seq[i][0], row, col, seq[i][1],
             gain * (i < 3 ? 0.9 : i < 6 ? 1.0 : 0.72),
             pan * (i % 2 ? -1 : 1), 0.85, dly, BUS_SIG);
}

// ── the one-shot player, with pitch wiggle ─────────────────────────────
typedef struct {
    double gain, pan, semis, side, dark, bright, edge, dur, dly, off, atk;
    int arcDark;
    double run, runCycles, runPhase;
    double bitTune, bitTuneTo, bitTuneRate;
    int reverse;
    double wig, wigHz, wigPhase, wigDrift, wigIn;
    int bus;
} Shot;
static Shot shot_def(int bus, double side) {
    Shot o; o.gain = 1; o.pan = 0; o.semis = 0; o.side = side; o.dark = 0; o.bright = 0; o.edge = 0; o.arcDark = 1; o.run = 0; o.runCycles = 1; o.runPhase = 0; o.bitTune = 0; o.bitTuneTo = 2; o.bitTuneRate = 0.6; o.reverse = 0;
    o.dur = 0; o.dly = 0; o.off = 0; o.atk = 0.0015;
    o.wig = 0; o.wigHz = 5.0; o.wigPhase = 0; o.wigDrift = 0; o.wigIn = 0.45;
    o.bus = bus; return o;
}
#define SHOT_DRUM() shot_def(BUS_DRUMS, 0.35)
#define SHOT_SUNG() shot_def(BUS_VOX, 0.6)
#define SHOT_HELD() shot_def(BUS_TUBE, 0.7)

static void shot(const char *name, double t, Shot o) {
    // @jeffrey: no vocals until 0:42 — the voice bus is silent until the
    // sentence's first dash at the bar-29 hook. (The watery-hole dink
    // rides the drum bus, so it survives.)
    if (o.bus == BUS_VOX && t < 29 * BAR - 0.02) return;
    // The door (both critics): 400 ms of negative space before the
    // sentence — decorations step aside, the kick keeps the floor.
    if (t >= 29 * BAR - 0.42 && t < 29 * BAR - 0.02) return;
    // The vox macro arc: every sung thing starts at ~0.72 of its written
    // gain with a darkness floor, and earns its full level and light only as
    // the record approaches the whole message at bar 76.
    if (o.bus == BUS_VOX) {
        const double u = smoothstep01(clampd((t / BAR - 8) / (76.0 - 8), 0, 1));
        o.gain *= 0.72 + 0.28 * u;
        if (o.arcDark) o.dark = dmax(o.dark, 0.26 * (1 - u));
    }
    t += feelOf(o.bus == BUS_DRUMS ? -0.004 : o.bus == BUS_VOX ? 0.010 : o.bus == BUS_TUBE ? 0.012 : o.bus == BUS_MUSIC ? 0.003 : 0, t);
    Sample *smp = bank_get(name);
    if (!smp) { bank_missing(name); return; }
    const long len = smp->n;
    float *rev = NULL;
    if (o.reverse) { rev = malloc(len * sizeof(float)); for (long r = 0; r < len; r++) rev[r] = smp->s[len - 1 - r]; }
    const float *s = o.reverse ? rev : smp->s;
    const double step = pow(2, o.semis / 12.0);
    long start = jsround(o.off * SR);
    if (start < 0) start = 0;
    if (start > len - 2) start = len - 2;
    double availD = floor((double)(len - 2 - start) / step);
    if (o.wig || o.wigDrift || o.run) availD = floor(availD * 0.965);
    long n = o.dur > 0 ? (long)dmin(availD, (double)jsround(o.dur * SR)) : (long)availD;
    if (n <= 4) { free(rev); return; }
    const long i0 = jsround(t * SR);
    const Sp sp = spatial(o.pan * 1.2);
    const double span = (double)n / SR;
    const double ramp = dmax(1e-4, o.wigIn);
    double lp = 0, hp = 0, hp2 = 0, lpRun = 0, pos = start;
    // bittune: bit depth falls on every 16th of the record's grid
    const double grid16 = ((double)SR * BAR) / 16;
    const long step16_0 = (long)floor(i0 / grid16);
    double lpBit = 0;
    for (long i = 0; i < n; i++) {
        const long q = (long)pos;
        if (q + 1 >= len) break;
        const double f = pos - q;
        double v = s[q] + (s[q + 1] - s[q]) * f;
        if (o.dark > 0) { lp += (1 - o.dark) * (v - lp); v = lp; }
        // `run`: the performer runs at the microphone and past it — far is
        // quieter, darker, wetter; the pitch bends up on the approach and
        // down on the way out.
        double g = o.gain, dl = o.dly, runCents = 0;
        if (o.run > 0) {
            const double ph = o.runPhase + o.runCycles * ((double)i / n);
            const double far = 0.5 + 0.5 * cos(TAU * ph);
            lpRun += (1 - 0.70 * far * o.run) * (v - lpRun); v = lpRun;
            g *= 1 - 0.74 * far * o.run;
            dl *= 1 + 1.8 * far * o.run;
            runCents = o.run * 26 * sin(TAU * ph);
        }
        if (o.bitTune > 0) {
            const long k = (long)floor((i0 + i) / grid16) - step16_0;
            const double bits = dmax(o.bitTuneTo, o.bitTune - floor(k * o.bitTuneRate));
            const double q = pow(2, bits - 1);
            const double crunch = clampd((o.bitTune - bits) / (o.bitTune - o.bitTuneTo), 0, 1);
            const double vq = floor(v * q + 0.5) / q;
            lpBit += (1 - 0.72 * crunch) * (vq - lpBit);
            v = lpBit;
        }
        // `bright`: a one-pole high shelf (~3.2 kHz), dark's missing opposite
        if (o.bright > 0) { hp += 0.34 * (v - hp); v += o.bright * (v - hp); }
        if (o.edge > 0) { hp2 += 0.58 * (v - hp2); v += o.edge * (v - hp2); }
        const double env = smoothstep01(dmin(1, (double)i / (o.atk * SR)));
        emit(o.bus, i0 + i, v * env * g * tailFade(i, n), o.pan, sp, o.side, dl);
        if (o.wig || o.wigDrift || o.run) {
            const double u = (double)i / SR;
            const double d = smoothstep01(u / ramp);
            const double cents = d * o.wig * sin(TAU * o.wigHz * u + o.wigPhase)
                + o.wigDrift * smoothstep01(u / span) + runCents;
            pos += step * pow(2, cents / 1200.0);
        } else pos += step;
    }
    free(rev);
}

// Subharmonic doubling: the same take an octave (or a twelfth) below itself,
// darkened, centred, set 12 ms back — a floor, not a second singer.
static void subDouble(const char *name, double t, const Shot *o, double amount) {
    if (amount <= 0) return;
    Shot d = *o;
    d.bus = BUS_VOX;
    d.gain = o->gain * amount;
    d.semis = o->semis - 12;
    d.dark = dmax(o->dark, 0.42);
    d.bright = 0; d.edge = 0; d.arcDark = 1;   // the floor stays a floor
    d.side = dmin(o->side, 0.30);
    d.pan = o->pan * 0.35;
    shot(name, t + 0.012, d);
}
static void sungSub(const char *name, double t, Shot o, double amount) {
    shot(name, t, o);
    subDouble(name, t, &o, amount);
}

// ── voice as material ──────────────────────────────────────────────────
static int degAt(int bar);
static void triad_of(int deg, int base, int *out);

static void material(int bar, const char *name, double g,
                     int steps, double grain, double dark, double side, double dly, double span) {
    Sample *src = bank_get(name);
    if (!src) { bank_missing(name); return; }
    const double len = (double)src->n / SR;
    int tri[3]; triad_of(degAt(bar), 59, tri);
    for (int k = 0; k < steps; k++) {
        if ((k * 7 + bar * 3) % 5 == 0) continue;
        const double t = at(bar, 0) + k * (BEAT / 4);
        const double off = (double)((k * 5 + bar * 3) % 11) / 11 * dmax(0, len - grain) * span;
        const int semis = tri[(k + bar) % 3] - tri[0] + (k % 8 == 4 ? 12 : 0);
        const double accent = k % 4 == 0 ? 1.0 : k % 2 == 0 ? 0.72 : 0.52;
        Shot o = SHOT_SUNG();
        o.gain = 0.30 * g * accent; o.semis = semis;
        o.pan = (k % 2 ? 0.55 : -0.55) * (0.6 + 0.4 * ((double)(k % 3) / 2));
        o.side = side; o.dark = dark; o.dly = dly; o.off = off;
        o.dur = grain * (k % 4 == 0 ? 1.6 : 1);
        shot(name, t, o);
    }
}

// Granular stretch — the fallback texture when a long render is unbuilt.
static void stretched(const char *name, double t, double gain, double pan,
                      double semis, double stretch, double dur, double side,
                      double dark, int bus, double dly) {
    if (bus == BUS_VOX && t < 29 * BAR - 0.02) return;  // same hold as shot()
    Sample *smp = bank_get(name);
    if (!smp) { bank_missing(name); return; }
    const float *s = smp->s; const long len = smp->n;
    const double step = pow(2, semis / 12.0);
    const long n = jsround(dur * SR);
    const long grain = jsround(0.055 * SR), hopOut = grain >> 1;
    const double hopIn = ((double)hopOut * step) / stretch;
    double *acc = (double *)calloc(n + grain, sizeof(double));
    double read = 0;
    for (long g = 0; g * hopOut < n; g++) {
        if (read + grain * step >= len - 2) break;
        for (long k = 0; k < grain; k++) {
            const long o = g * hopOut + k;
            if (o >= n + grain - 1) break;
            const double pos = read + k * step;
            const long q = (long)pos; const double f = pos - q;
            acc[o] += (s[q] + (s[q + 1] - s[q]) * f) * (0.5 - 0.5 * cos((TAU * k) / (grain - 1)));
        }
        read += hopIn;
    }
    const long i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.3);
    double lp = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        lp += (1 - dark) * (acc[i] - lp);
        double env = dmin(1, u / 0.030);
        const double left = (double)(n - i) / SR;
        if (left < 0.12) env *= left / 0.12;
        emit(bus, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, side, dly);
    }
    free(acc);
}

// ── harmony ────────────────────────────────────────────────────────────
// B natural minor, four rows of PROGRESSIONS_CHILL, two bars a chord;
// act VII pins the harmony to the home progression Bm · D · G · Em.
static const int SCALE[7] = {0, 2, 3, 5, 7, 8, 10};
static inline int sd(int i) { return SCALE[((i % 7) + 7) % 7] + 12 * (int)floor((double)i / 7); }
static const int ROWS[4][4] = {{0, 5, 2, 6}, {0, 6, 3, 5}, {0, 4, 5, 3}, {0, 2, 5, 3}};
static const int HOME[4] = {0, 2, 5, 3};
static inline int bassRoot(int deg) { return 35 + sd(deg); }
static void triad_of(int deg, int base, int *out) {
    out[0] = base + sd(deg); out[1] = base + sd(deg + 2); out[2] = base + sd(deg + 4);
}

// ── form: the nine acts ────────────────────────────────────────────────
enum { S_CARRIER, S_THREE, S_MESSAGE, S_SECRET, S_REPLY, S_SPREAD, S_WHOLE, S_RECOGNISE, S_CARRIEROFF };
static const int SB[9][2] = {
    {0, 8}, {8, 24}, {24, 40}, {40, 48}, {48, 64}, {64, 76}, {76, 96}, {96, 104}, {104, 112}};
static inline int inS(int bar, int k) { return bar >= SB[k][0] && bar < SB[k][1]; }
static int sectionAt(int bar) {
    for (int k = 0; k < 9; k++) if (inS(bar, k)) return k;
    return S_CARRIEROFF;
}
static inline int introBar(int b) { return b >= 8 && b < 16; }
static inline int sosBar(int b) { return b >= 16 && b < 24; }
static inline int soloBar(int b) { return b >= 54 && b < 56; }
static inline int releaseGapBar(int b) { return b >= 68 && b < 72; }
static inline int dotFieldBar(int b) { return b >= 72 && b < 76; }
// bars 64-67 and 72-75 are adjacent in the shipped edit: one composition
static inline int releaseSpreadBar(int b) { return (b >= 64 && b < 68) || dotFieldBar(b); }
static inline int releaseSpreadIndex(int b) { return b < 68 ? b - 64 : b - 68; }
static inline int sparseSpreadBar(int b) { return releaseGapBar(b); }
static inline int kickOn(int b) { return !(inS(b, S_CARRIER) || inS(b, S_SECRET) || inS(b, S_CARRIEROFF)
    || soloBar(b) || releaseGapBar(b)); }
static inline int hatOn(int b) { return kickOn(b); }
static inline int dense(int b) { return inS(b, S_REPLY) || inS(b, S_WHOLE); }
static inline int wordsIn(int bar) {
    // The withholding is over, but the lyrics hold until the first hook.
    // v10.1 held the words until bar 76 and the first utterance came out
    // truncated; opening at bar 24 let the chorus leak "run real fast" at
    // 0:32, ahead of the sentence. @jeffrey: "hold all lyrics until 0:40" —
    // so the loop runs morse-only through bar 27, and the first words are
    // the whole sentence at the bar-28 hook: dash · i wanna · dash ·
    // i wanna · run real fast · dot dot dot.
    return bar >= SB[S_MESSAGE][0] + 4;
}

static int degAt(int bar) {
    if (soloBar(bar)) return 6;                    // exposed A-major pivot
    static const int DF[4] = {2, 2, 6, 6};         // D, D, A, A across the dot field
    if (dotFieldBar(bar)) return DF[bar - 72];
    if (inS(bar, S_WHOLE)) return HOME[((bar - SB[S_WHOLE][0]) % 8) / 2];
    return ROWS[(bar / 8) % 4][(bar % 8) / 2];
}

// the strum machine's chord name for a bar, from the bed's degree
static const char *gtChord(int bar) {
    switch (degAt(bar)) { case 0: return "bm"; case 2: return "d"; case 3: return "em"; case 4: return "fsm"; case 5: return "g"; case 6: return "a"; default: return "bm"; }
}

// The crossover wiggle: depth is a function of where in the story we are.
static double wigDepth(int bar) {
    switch (sectionAt(bar)) {
        case S_THREE: return 4; case S_MESSAGE: return 10; case S_SECRET: return 7;
        case S_REPLY: return 16; case S_SPREAD: return 22; case S_WHOLE: return 14;
        case S_RECOGNISE: return 9; case S_CARRIEROFF: return 5; case S_CARRIER: return 0;
        default: return 10;
    }
}

// ── the choir keyboards ────────────────────────────────────────────────
// CULT_PITCH in Node insertion order (already ascending by pitch, no ties).
typedef struct { const char *name; int midi; } Key;
static const Key CULT_PITCH[11] = {
    {"b2", 47}, {"d3", 50}, {"fs3", 54}, {"g3", 55}, {"a3", 57}, {"b3", 59},
    {"cs4", 61}, {"d4", 62}, {"e4", 64}, {"fs4", 66}, {"g4", 67}};

// choirFor: keep names whose pitch class sits in the current triad, then
// pick [lowest-low ?? second-high ?? first-high, first-high, last-high],
// deduped in order.
static int choirFor(int deg, const char *out[3]) {
    int tri[3]; triad_of(deg, 59, tri);
    int pcs[3]; for (int i = 0; i < 3; i++) pcs[i] = ((tri[i] % 12) + 12) % 12;
    const char *ok[11]; int okc = 0;
    for (int i = 0; i < 11; i++) {
        int pc = CULT_PITCH[i].midi % 12;
        for (int j = 0; j < 3; j++) if (pcs[j] == pc) { ok[okc++] = CULT_PITCH[i].name; break; }
    }
    if (!okc) { out[0] = "b3"; return 1; }
    const char *low[11]; int lowc = 0; const char *high[11]; int highc = 0;
    for (int i = 0; i < okc; i++) {
        int m = 0; for (int j = 0; j < 11; j++) if (CULT_PITCH[j].name == ok[i]) m = CULT_PITCH[j].midi;
        if (m <= 55) low[lowc++] = ok[i];
        if (m >= 57) high[highc++] = ok[i];
    }
    const char *pick[3]; int pc2 = 0;
    const char *first = lowc ? low[0] : (highc > 1 ? high[1] : (highc ? high[0] : NULL));
    const char *cands[3] = {first, highc ? high[0] : NULL, highc ? high[highc - 1] : NULL};
    for (int i = 0; i < 3; i++) {
        if (!cands[i]) continue;
        int dup = 0; for (int j = 0; j < pc2; j++) if (pick[j] == cands[i]) dup = 1;
        if (!dup) pick[pc2++] = cands[i];
    }
    for (int i = 0; i < pc2; i++) out[i] = pick[i];
    return pc2;
}

// DOT_PITCH, pre-sorted by pitch with ties in Node insertion order.
static const Key DOT_SORTED[19] = {
    {"dot-j-b2", 47}, {"dot-j-d3", 50}, {"dot-j-e3", 52},
    {"dot-j-fs3", 54}, {"dot-a-fs3", 54}, {"dot-j-g3", 55}, {"dot-a-g3", 55},
    {"dot-j-a3", 57}, {"dot-a-a3", 57}, {"dot-a-b3", 59}, {"dot-c-b3", 59},
    {"dot-c-cs4", 61}, {"dot-a-d4", 62}, {"dot-c-d4", 62},
    {"dot-a-e4", 64}, {"dot-c-e4", 64}, {"dot-c-fs4", 66}, {"dot-c-g4", 67}, {"dot-c-a4", 69}};
static int dotsFor(int deg, const char *out[19]) {
    int tri[3]; triad_of(deg, 59, tri);
    int pcs[3]; for (int i = 0; i < 3; i++) pcs[i] = ((tri[i] % 12) + 12) % 12;
    int c = 0;
    for (int i = 0; i < 19; i++) {
        if (!has(DOT_SORTED[i].name)) continue;
        int pc = DOT_SORTED[i].midi % 12;
        for (int j = 0; j < 3; j++) if (pcs[j] == pc) { out[c++] = DOT_SORTED[i].name; break; }
    }
    if (!c) { out[0] = "dot-b3"; c = 1; }
    return c;
}

// VOXDOT_PITCH in Node insertion order (filtered, never sorted).
static const Key VOXDOT_PITCH[6] = {
    {"voxdot-j-b2", 47}, {"voxdot-j-fs3", 54}, {"voxdot-a-a3", 57},
    {"voxdot-a-d4", 62}, {"voxdot-c-b3", 59}, {"voxdot-c-fs4", 66}};

// ── other people's cults / the dots from everywhere ────────────────────
static const char *ALT_CULTS[6] = {
    "alt-71018-cult", "alt-70555-cult", "alt-71244-cult",
    "alt-70551-cult", "alt-71441-cult", "alt-71195-cult"};
// ALT_DOTS: nine posts, grouped; the post id keys the altdot-<id>-long render.
static const char *ALT_DOTS[9][3] = {
    {"alt-70551-dot1", "alt-70551-dot2", NULL},
    {"alt-70555-dot1", "alt-70555-dot2", "alt-70555-dot3"},
    {"alt-71018-dot1", "alt-71018-dot2", "alt-71018-dot3"},
    {"alt-71195-dot1", "alt-71195-dot2", "alt-71195-dot3"},
    {"alt-71244-dot1", "alt-71244-dot2", "alt-71244-dot3"},
    {"alt-71437-dot1", "alt-71437-dot2", NULL},
    {"alt-71441-dot1", "alt-71441-dot2", "alt-71441-dot3"},
    {"alt-71448-dot", NULL, NULL},
    {"alt-71560-dot1", "alt-71560-dot2", NULL}};
static const char *ALT_DOT_IDS[9] = {
    "70551", "70555", "71018", "71195", "71244", "71437", "71441", "71448", "71560"};
// the sung pitch of each post's long dot — `deep` drops the high ones an octave
static const int ALT_DOT_MIDI[9] = {59, 47, 55, 47, 61, 59, 62, 66, 61};

// One video says ONE dot, held long — the WORLD-chain render when built,
// the granular stretch as fallback.
static void dotDrift(double t, int vid, double gain, double pan, double dur,
                     double stretch, double dly, double dark, double semis, int deep) {
    if (SPATIAL && t >= 24 * BAR) return; // room cut: sentence only
    if (SPATIAL) gain *= 0.55;   // intro dots stay, recessed
    const int gi = ((vid % 9) + 9) % 9;
    const char *takes[3]; int tc = 0;
    for (int i = 0; i < 3; i++)
        if (ALT_DOTS[gi][i] && has(ALT_DOTS[gi][i])) takes[tc++] = ALT_DOTS[gi][i];
    if (!tc) return;
    char lname[64]; snprintf(lname, sizeof lname, "altdot-%s-long", ALT_DOT_IDS[gi]);
    const double dsemis = semis + ((deep && ALT_DOT_MIDI[gi] >= 59) ? -12 : 0);
    if (has(lname)) {
        Shot o = SHOT_SUNG();
        o.gain = gain * 1.15 * vel(0.12); o.pan = pan; o.semis = dsemis;
        o.side = 0.85; o.dark = dark; o.dly = dly;
        o.wig = 6; o.wigHz = 0.5 + 0.3 * (vid % 3); o.wigPhase = vid * 1.9;
        o.wigDrift = (vid % 2 ? -4 : 4); o.wigIn = 0.8;
        shot(lname, t, o);
        return;
    }
    const char *pick = takes[0];
    for (int i = 0; i < tc; i++)
        if (bank_get(takes[i])->n > bank_get(pick)->n) pick = takes[i];
    stretched(pick, t, gain * vel(0.12), pan, semis, stretch, dur, 0.85, dark, BUS_VOX, dly);
}

// …and the aesthetivoxed kind: Camille and Alex only, held at a chord tone
// — Camille listed twice ("more camille dots"), which changes the pick.
static void dotDriftVox(double t, int bar, double gain, double pan, double dur,
                        double stretch, double dly, double dark, double semis) {
    if (SPATIAL && t >= 24 * BAR) return; // room cut: sentence only
    if (SPATIAL) gain *= 0.55;   // intro dots stay, recessed
    int tri[3]; triad_of(degAt(bar), 59, tri);
    int pcs[3]; for (int i = 0; i < 3; i++) pcs[i] = ((tri[i] % 12) + 12) % 12;
    const char *lng[12]; int lc = 0;
    for (int i = 0; i < 6; i++) {
        if (!has(VOXDOT_PITCH[i].name)) continue;
        int pc = VOXDOT_PITCH[i].midi % 12;
        int in = 0; for (int j = 0; j < 3; j++) if (pcs[j] == pc) in = 1;
        if (!in) continue;
        if (!strncmp(VOXDOT_PITCH[i].name, "voxdot-j", 8)) continue;
        lng[lc++] = VOXDOT_PITCH[i].name;
        if (!strncmp(VOXDOT_PITCH[i].name, "voxdot-c", 8)) lng[lc++] = VOXDOT_PITCH[i].name;
    }
    if (lc) {
        const char *nm = lng[abs(bar * 7 + 3) % lc];
        Shot o = SHOT_SUNG();
        o.gain = gain * 1.15 * vel(0.12); o.pan = pan; o.semis = semis; o.side = 0.8; o.dark = dark; o.dly = dly;
        o.wig = 5; o.wigHz = 0.45; o.wigPhase = bar * 1.3;
        o.wigDrift = (bar % 2 ? -3 : 3); o.wigIn = 0.8;
        shot(nm, t, o);
        return;
    }
    const char *bank[19]; int bc = dotsFor(degAt(bar), bank);
    const char *nm = bank[bc - 1 < (bc >> 1) ? bc - 1 : (bc >> 1)];
    stretched(nm, t, gain * vel(0.12), pan, 0, stretch, dur, 0.8, dark, BUS_VOX, dly);
}

// Camille's sung dot bank holds the whole B natural minor scale B3..A4, so
// dotArp() runs the scale as syllables, up or down, lightly swung.
static const char *CAM_SCALE[7] = {"b3", "cs4", "d4", "e4", "fs4", "g4", "a4"};
static void dotTail(double t, int count, double gain, double pan, double dly);   // defined with the field, used by the hook
static void dotArp(double t, int count, int up, double gap, double gain, double pan, double dly) {
    char nm[64];
    for (int k = 0; k < count; k++) {
        snprintf(nm, sizeof nm, "dot-c-%s", CAM_SCALE[up ? k % 7 : 6 - (k % 7)]);
        if (!has(nm)) continue;
        // jit in the time argument is drawn before the vel in the options
        const double tt = t + k * gap + jit(4) + (k % 2 ? 0.012 : 0);
        Shot o = SHOT_SUNG();
        o.gain = gain * (1 - 0.05 * k) * vel(0.15); o.pan = pan * (k % 2 ? -1 : 1);
        o.side = 0.7; o.dly = dly; o.atk = 0.012;
        shot(nm, tt, o);
    }
}

// @jeffrey at 1:25: "alex's 'cult' ones are too normal should be more weird
// / pitched around buynched up" — "deturned clusters and stacked entries".
// Six takes inside three quarters of a beat, each a different fraction of
// a semitone off true; two ride the staircase in opposite directions and
// the last is an octave up.
static const struct { const char *nm; double beat, semis, pan, gain; int stair; } CULT_CLUSTER[6] = {
    {"alt-71018-cult", 0.00, -0.34, -0.10, 0.30, 0},
    {"alt-70555-cult", 0.09, +0.47, 0.38, 0.27, 0},
    {"alt-71441-cult", 0.17, -0.72, -0.44, 0.25, -1},
    {"alt-70551-cult", 0.28, +0.28, 0.26, 0.24, 0},
    {"alt-71244-cult", 0.41, -0.91, -0.30, 0.21, 1},
    {"alt-71195-cult", 0.56, +12.3, 0.14, 0.13, 0}};
static void cultCluster(int bar, double beat, double g) {
    for (int i = 0; i < 6; i++) {
        if (!has(CULT_CLUSTER[i].nm)) continue;
        const double semis = CULT_CLUSTER[i].semis, b = CULT_CLUSTER[i].beat;
        const double t = at(bar, beat + b) + jit(6);
        if (CULT_CLUSTER[i].stair) {
            staircasePan(CULT_CLUSTER[i].nm, t, CULT_CLUSTER[i].gain * g * 1.05, 3, 1.9,
                         CULT_CLUSTER[i].stair, semis, 0.22, BUS_VOX, 0.52, 0);
        } else {
            Shot o = SHOT_SUNG();
            o.gain = CULT_CLUSTER[i].gain * g; o.semis = semis; o.pan = CULT_CLUSTER[i].pan;
            o.side = 0.94; o.dark = 0.20; o.dly = 0.50;
            o.reverse = (b == 0.09 || b == 0.28);   // two of the heap say it backwards
            o.wig = 9 + 4 * fabs(fmod(semis, 3.0)); o.wigHz = 0.8 + 0.5 * fabs(semis);
            o.wigPhase = b * 7.3; o.wigDrift = semis > 0 ? -5 : 5; o.wigIn = 0.55;
            shot(CULT_CLUSTER[i].nm, t, o);
        }
    }
}

// cultTake: prefer the 5 s cultlong-* render over the 4 s cult-* take.
static const char *cultTake(const char *nm, char *buf, size_t bufsz) {
    snprintf(buf, bufsz, "cultlong-%s", nm);
    if (has(buf)) return buf;
    snprintf(buf, bufsz, "cult-%s", nm);
    return buf;
}

// One syllable, three people, one pitch, 28 ms apart — each wiggling at
// their own rate and phase, so where they cross they beat. Jeffrey's sub
// octave gives the unison a bottom instead of a fourth singer.
// `slow` swaps the 1.5 s holds for the 4 s dashlong-* renders (Camille at
// pitch, Jeffrey in his own octave, Alex where his one long take fits),
// held slowDur, and lifts the stack so the long vowel does not bury it.
static void dashStack(double t, const char *which, double G, int bar, int slow, double slowDur) {
    if (t < 29 * BAR - 0.02) return;
    const double w = wigDepth(bar);
    char cam[64], alx[64], jef[64], nb[64];
    const char *J = !strcmp(which, "fs4") ? "fs3" : !strcmp(which, "d4") ? "d3" : !strcmp(which, "b3") ? "b2" : !strcmp(which, "e4") ? "e3" : which;
    snprintf(nb, sizeof nb, "dashlong-camille-%s", which);
    if (slow && has(nb)) snprintf(cam, sizeof cam, "%s", nb); else snprintf(cam, sizeof cam, "dash-camille-%s-hold", which);
    if (slow && (!strcmp(which, "d4") || !strcmp(which, "d3")) && has("dashlong-alex-d3")) snprintf(alx, sizeof alx, "dashlong-alex-d3");
    else snprintf(alx, sizeof alx, "dash-alex-%s-hold", which);
    snprintf(jef, sizeof jef, "dash-jeffrey-%s-hold", which);   // Jeffrey keeps the 1.5 s hold …
    if (!has(jef)) snprintf(jef, sizeof jef, "dash-jeffrey-%s-hold", J);   // … in his own octave where needed
    const double jT = slow ? t + 1.20 : t;                         // … and waits for the long wanna to end
    if (slow) G *= 0.92;   // the slow stack sits back: wider, wetter, a shade under the plain one
    static const struct { double semis, dur; } JVT[4] = {{0, 1.5}, {2, 1.15}, {-3, 1.85}, {5, 1.3}};
    const double jvS = JVT[(bar >> 3) % 4].semis, jvD = JVT[(bar >> 3) % 4].dur;   // each slow hook its own pitch and length
    Shot o = SHOT_HELD();
    o.gain = 0.50 * G; o.pan = -0.42; o.side = 0.70; o.dly = 0.10; if (slow) { o.dur = slowDur; o.side = 0.82; o.dly = 0.24; }
    o.wig = w; o.wigHz = 4.3; o.wigPhase = 0.0; o.wigDrift = +0.42 * w; o.wigIn = 0.50;
    shot(cam, t + 0.000 + jit(3), o);

    o = SHOT_HELD();
    o.gain = 0.47 * G; o.pan = 0.42; o.side = 0.70; o.dly = 0.10; if (slow) { o.dur = slowDur; o.side = 0.82; o.dly = 0.24; }
    o.wig = w * 1.15; o.wigHz = 5.9; o.wigPhase = 2.1; o.wigDrift = -0.5 * w; o.wigIn = 0.42;
    shot(alx, t + 0.028 + jit(3), o);

    // Jeffrey: lowpass off, shelf on — same weight, an edge on the vowel.
    Shot j = SHOT_HELD();
    j.gain = 0.58 * G; j.pan = 0.00; j.side = 0.38; j.dark = 0; j.bright = 1.25; j.edge = 0.6; j.arcDark = 0; j.dly = 0.08;
    j.atk = 0.0009;
    if (slow) { j.bitTune = 16; j.bitTuneTo = 2; j.bitTuneRate = 0.6; j.semis = jvS; j.dur = jvD; }   // into a square wave, in time
    j.wig = w * 0.7; j.wigHz = 3.4; j.wigPhase = 4.3; j.wigDrift = +0.3 * w; j.wigIn = 0.60;
    shot(jef, jT + 0.056 + jit(3), j);
    if (slow) {   // …then reversed, right after ("play then reverse")
        Shot r = j; r.gain = j.gain * 0.75; r.reverse = 1; r.dly = 0.18;
        r.bitTune = 6; r.bitTuneTo = 2; r.bitTuneRate = 0.5; r.semis = jvS; r.dur = jvD;
        shot(jef, jT + 0.056 + jvD, r);
    }
    if (slow) {   // …and, in the slow stack, ALSO sung on the vox bus so the pump cannot bury him
        Shot v = SHOT_SUNG(); v.gain = 0.36 * G; v.pan = 0.0; v.side = 0.45; v.dly = 0.20; v.semis = jvS; v.dur = jvD;
        v.bright = 1.1; v.edge = 0.5; v.arcDark = 0; v.atk = 0.0009; v.bitTune = 16; v.bitTuneTo = 2; v.bitTuneRate = 0.6;
        v.wig = w * 0.7; v.wigHz = 3.4; v.wigPhase = 4.3; v.wigDrift = +0.3 * w; v.wigIn = 0.60;
        shot(jef, jT + 0.062, v);
    }

    // …and his sub double keeps its darkness: that layer is the floor.
    Shot js = j;
    js.gain = j.gain * 0.40; js.semis = -12; js.dark = 0.46; js.bright = 0; js.edge = 0; js.arcDark = 1; js.bitTune = 0; js.dur = 0; js.semis = -12; js.reverse = 0;
    js.atk = 0.0015; js.side = 0.24; js.wig = w * 0.35;
    shot(jef, jT + 0.068 + jit(3), js);
}

// The choir: sung "cult" held at three chord tones, 45 ms apart. `dur`
// (0 = the whole take) cuts the last one short at the end of the record.
static void choir(int bar, double g, double dur) {
    const double t = at(bar, 0);
    const char *picks[3]; const int pc = choirFor(degAt(bar), picks);
    static const double gains[3] = {0.46, 0.38, 0.30};
    static const double pans[3] = {0.0, -0.50, 0.50};
    static const double sides[3] = {0.40, 0.85, 0.85};
    const double w = dmin(7, wigDepth(bar) * 0.4);
    char buf[64];
    for (int i = 0; i < pc; i++) {
        Shot o = SHOT_SUNG();
        o.gain = g * (i < 3 ? gains[i] : 0.28); o.pan = i < 3 ? pans[i] : 0;
        o.side = i < 3 ? sides[i] : 0.7; o.dark = 0.32;
        if (dur > 0) o.dur = dur;
        o.run = 0.85; o.runCycles = 1; o.runPhase = 0.05 + 0.28 * i;   // three runners, staggered
        o.wig = 62 + 18 * i; o.wigHz = 0.16 + 0.06 * i; o.wigPhase = i * 2.3;   // the slow sway
        o.wigDrift = (i % 2 ? -1 : 1) * w * 0.6; o.wigIn = 1.5;
        shot(cultTake(picks[i], buf, sizeof buf), t + i * 0.045, o);
    }
    { Shot s = SHOT_SUNG(); s.gain = g * 0.55; s.semis = -12; s.dark = 0.52;
      s.side = 0.28; s.pan = 0; s.run = 0.55; s.runCycles = 1; s.runPhase = 0.10;
      s.wig = w * 0.5; s.wigHz = 0.5; s.wigIn = 1.6;
      shot(cultTake(picks[0], buf, sizeof buf), t + 0.09, s); }
}

// ACT IV is the perceptual reset: one Camille take at a time, never a chord
// stack. Four phrases cross the chord tones while the gravity impact bends
// their space.
static void secretCamille(int bar, double beat, double gain, double phase) {
    const char *picks[3]; const int pc = choirFor(degAt(bar), picks);
    const int phrase = (bar - SB[S_SECRET][0]) / 2;
    const char *nm = picks[(phrase + 1) % pc];
    char take[64]; snprintf(take, sizeof take, "cult-%s", nm);
    char buf[64];
    const char *use = has(take) ? take : cultTake(nm, buf, sizeof buf);
    static const double pans[4] = {-0.18, 0.14, -0.06, 0.22};
    Shot o = SHOT_SUNG();
    o.gain = gain; o.pan = pans[phrase % 4]; o.side = 0.58; o.dark = 0.24; o.dly = 0.46;
    o.run = 0.70; o.runCycles = 1; o.runPhase = 0.12 + 0.20 * (phrase % 2);
    o.atk = 0.16; o.wig = 7 + phrase * 1.5; o.wigHz = 0.44 + phrase * 0.07;
    o.wigPhase = phase + phrase * 1.7; o.wigDrift = phrase % 2 ? -4 : 4; o.wigIn = 0.9;
    shot(use, at(bar, beat), o);
}

// The bops walk a counterline against the bass root, rotated a step every
// eight bars — the phone's little answers form a tune of their own.
static const int BLIP_LINE[8] = {2, 4, 6, 4, 2, 0, 5, 3};
static int blipMidi(int bar, int k) {
    return 71 + sd(degAt(bar) + ((BLIP_LINE[(bar + k * 3) % 8] + (bar >> 3)) % 7));
}

// …and the phone knows the song: the four-line chorus melody as single
// sine beeps on the signal bus, two bars, up an octave, swung wide.
static void phoneTune(int bar, double g) {
    static const double K[11] = {0.00, 0.50, 1.00, 2.00, 2.50, 3.00, 4.00, 4.75, 6.00, 6.50, 7.00};
    static const int M[11] = {67, 66, 62, 62, 64, 66, 67, 69, 59, 55, 59};
    static const int H[11] = {64, 62, 59, 59, 61, 62, 64, 66, 55, 52, 55};
    for (int i = 0; i < 11; i++) {
        const double tt = at(bar, 0) + K[i] * BEAT + jit(6);   // jit before vel,
        const double gg = g * vel(0.2);                        // like the Node args
        beep(tt, midihz(M[i] + 12), midihz(H[i] + 12), 0.11, gg, i % 2 ? 0.4 : -0.4, 0.85, 0.45, BUS_SIG);
        beep(tt + 0.030, midihz(M[i] + 12) * 1.004, midihz(H[i] + 12) * 0.9955, 0.11,
             gg * 0.55, i % 2 ? -0.45 : 0.45, 0.95, 0.60, BUS_SIG);
    }
}

// The 1:42 ornament — the crossover wiggle deep enough to read as gamaka:
// the LONG dash take at a chord tone, wiggle twice as deep and slower,
// drifting toward the next scale tone and back.
static const Key DASHLONG_PITCH[4] = {{"b3", 59}, {"d4", 62}, {"e4", 64}, {"fs4", 66}};
static void raga(int bar, double beat, double g) {
    int tri[3]; triad_of(degAt(bar), 59, tri);
    int pcs[3]; for (int i = 0; i < 3; i++) pcs[i] = ((tri[i] % 12) + 12) % 12;
    const char *ok[4]; int okc = 0;
    char nb[64];
    for (int i = 0; i < 4; i++) {
        snprintf(nb, sizeof nb, "dashlong-camille-%s", DASHLONG_PITCH[i].name);
        if (!has(nb)) continue;
        int pc = DASHLONG_PITCH[i].midi % 12;
        for (int j = 0; j < 3; j++) if (pcs[j] == pc) { ok[okc++] = DASHLONG_PITCH[i].name; break; }
    }
    const char *nm = okc ? ok[bar % okc] : "b3";
    const double w = dmax(24, wigDepth(bar) * 1.8);
    Shot o = SHOT_HELD();
    o.gain = g; o.pan = bar % 2 ? 0.24 : -0.24; o.side = 0.7; o.dly = 0.30;
    o.wig = w; o.wigHz = 3.1; o.wigPhase = bar * 1.7;
    o.wigDrift = (bar % 2 ? -1 : 1) * w * 1.4; o.wigIn = 0.35;
    snprintf(nb, sizeof nb, "dashlong-camille-%s", nm);
    shot(nb, at(bar, beat) + jit(8), o);
}

// ── the hook, sung — and the chorus, with the words withheld ───────────
static void hook_fn(int bar, int full) {
    const double t = at(bar, 0);
    const double G = full ? 1.0 : 0.70;
    const double F = SPATIAL ? 1.22 : 1.0;
    const double P = SPATIAL ? 0.6 : 1.0;
    const int hv = (bar >> 3) % 3;
    dashStack(t + 0.00, hv == 1 ? "fs3" : "fs4", G * F, bar, 0, 0);
    { Shot o = SHOT_HELD(); o.gain = 0.26 * G; o.pan = 0.30; o.side = 0.70; o.dly = 0.35; o.dur = 0.28; o.atk = 0.006;
      char an[64]; snprintf(an, sizeof an, "dash-camille-%s-hold", hv == 1 ? "fs4" : "fs3");
      shot(an, t + 0.75, o); }
    { Shot o = SHOT_HELD(); o.gain = 0.18 * G; o.pan = -0.34; o.side = 0.75; o.dly = 0.45; o.dur = 0.22; o.atk = 0.006;
      shot("dash-camille-d4-hold", t + 1.06, o); }
    if (((bar >> 3) & 1) == 0 && has("iwannalong-a")) {   // every other hook: the WORLD-sung "wannnnaaa" into the dash
        Shot o = SHOT_SUNG(); o.gain = 0.80 * G; o.pan = -0.18; o.side = 0.50; o.dly = 0.26; o.atk = 0.03;
        shot("iwannalong-a", t + 1.50 + jit(4), o);
    } else if (((bar >> 3) & 1) == 0)
        stretched("iwanna-a-sung", t + 1.50 + jit(4), 0.78 * G, -0.18, 0, 2.3, 1.45, 0.50, 0.12, BUS_VOX, 0.28);
    else { Shot o = SHOT_SUNG(); o.gain = 0.82 * G * F; o.pan = -0.18 * P; o.side = 0.50; o.dly = 0.20;
      shot("iwanna-a-sung", t + 1.50 + sw8(bar) + jit(4), o); }
    { const int slowWanna = ((bar >> 3) & 1) == 0 && has("iwannalong-a");
      dashStack(t + 2.00, slowWanna ? "e4" : hv == 2 ? "b3" : hv == 1 ? "d3" : "d4", G * 0.95 * F, bar, slowWanna, 3.0); }
    { Shot o = SHOT_SUNG(); o.gain = 0.82 * G * F; o.pan = 0.18 * P; o.side = 0.50; o.dly = 0.20;
      shot("iwanna-b-sung", t + 3.50 + sw8(bar) + jit(4), o); }
    if (wordsIn(bar)) {
        { Shot o = SHOT_SUNG(); o.gain = 1.26 * G * F; o.pan = 0.00; o.side = 0.35; o.dly = 0.10;
          o.bright = 1.2; o.edge = 0.5; o.arcDark = 0; o.atk = 0.0008;
          sungSub((((bar >> 3) & 1) == 0 && has("runrealfast-fastlong")) ? "runrealfast-fastlong" : "runrealfast-hi",
                  t + 4.00 + jit(4), o, 0.30); }
        { Shot o = SHOT_SUNG(); o.gain = 0.50 * G * F; o.pan = -0.10 * P; o.side = 0.60; o.dly = 0.35;
          shot("runrealfast-long-hi", t + 4.80 + jit(4), o); }
    } else {
        dotDriftVox(t + 4.00 + jit(20), bar, 0.30 * G, 0.10, 1.9, 4.2, 0.35, 0.30, 0);
        int tri[3]; triad_of(degAt(bar), 59, tri);
        bop(t + 5.50 + jit(4), midihz(tri[0] + 12), 0.20 * G, -0.28, 0.7, 0.085, 0.40);
    }
    { Shot o = SHOT_SUNG(); o.gain = 0.90 * G * F; o.pan = -0.45 * P; o.side = 0.75; o.dly = 0.38; o.atk = 0.025;
      shot("dot-b3", t + 6.00 + jit(3), o); }
    { Shot o = SHOT_SUNG(); o.gain = 0.90 * G * F; o.pan = 0.45 * P; o.side = 0.75; o.dly = 0.38; o.atk = 0.025;
      shot("dot-fs3", t + 6.50 + sw8(bar) + jit(3), o); }
    { Shot o = SHOT_SUNG(); o.gain = 0.80 * G * F; o.pan = 0.0; o.side = 0.60; o.dly = 0.45;
      shot("dot-d4", t + 7.00, o); }
    { Shot o = SHOT_SUNG(); o.gain = 0.62 * G * F; o.pan = -0.20 * P; o.side = 0.70; o.dly = 0.50;
      shot("dot-fs3", t + 7.50 + sw8(bar), o); }
    dotTail(t + 7.90, 8, 0.40 * G, 0.30, 0.42);   // …and the trail fractals out of the loop
    if (full && ((bar >> 3) & 1))
        stretched("dash-camille-fs4-hold", t + 7.00, 0.28 * G, -0.30, 0, 1.9, 3.0, 0.80, 0.25, BUS_VOX, 0.50);
}

enum { LEAD_BOTH, LEAD_HI, LEAD_LO };
enum { ANS_NONE, ANS_DOTS, ANS_SOS };
typedef struct { int lead, fast, answer, tagFast, choirUnder; double g; int drop2, drop3, drop4, thin; } Chorus;

static void chorus_fn(int bar, Chorus c) {
    const double t = at(bar, 0), G = c.g;
    const int both = c.lead == LEAD_BOTH, lo = c.lead == LEAD_LO;

    // line 1 · "run real fast" (never dropped at these call sites)
    if (!wordsIn(bar)) {
        dotDrift(t + 0.30 + jit(20), bar >> 3, 0.26 * G, -0.30, 1.5, 4.0, 0.40, 0.35, 0, 0);
    } else {
        const double F = SPATIAL ? 1.22 : 1.0;
        if (c.fast) {
            { Shot o = SHOT_SUNG(); o.gain = 1.25 * G * F; o.pan = -0.12; o.side = 0.42; o.dly = 0.12;
              o.bright = 1.1; o.edge = 0.4; o.arcDark = 0; o.atk = 0.0008;
              shot("runrealfast-fast-hi", t + 0.00 + jit(3), o); }
            { Shot o = SHOT_SUNG(); o.gain = 0.95 * G * F; o.pan = 0.12; o.side = 0.42; o.dly = 0.12;
              o.bright = 0.8; o.edge = 0.4; o.arcDark = 0; o.atk = 0.0008;
              shot("runrealfast-fast-lo", t + 1.00 + jit(3), o); }
        } else if (lo) {
            Shot o = SHOT_SUNG(); o.gain = 1.30 * G * F; o.pan = 0.00; o.side = 0.40; o.dly = 0.12;
            shot("runrealfast-long-lo", t + jit(4), o);
        } else {
            // the syllabic take SAYS the line and takes the shelf; the melisma
            // under it stays smooth on purpose
            { Shot o = SHOT_SUNG(); o.gain = 1.30 * G * F; o.pan = 0.00; o.side = 0.35; o.dly = 0.10;
              o.bright = 1.2; o.edge = 0.5; o.arcDark = 0; o.atk = 0.0008;
              sungSub((((bar >> 3) & 1) == 0 && has("runrealfast-fastlong")) ? "runrealfast-fastlong" : "runrealfast-hi",
                      t + jit(4), o, 0.30); }
            { Shot o = SHOT_SUNG(); o.gain = 0.55 * G * F; o.pan = both ? -0.14 : -0.08; o.side = 0.60; o.dly = 0.30;
              shot("runrealfast-long-hi", t + 0.80 + jit(4), o); }
            if (inS(bar, S_REPLY)) {
                { Shot o = SHOT_SUNG(); o.gain = 0.52 * G * F; o.pan = 0.22; o.side = 0.55; o.dly = 0.20; o.semis = 7;
                  shot("runrealfast-long-hi", t + 0.82, o); }
                { Shot o = SHOT_SUNG(); o.gain = 0.34 * G * F; o.pan = -0.24; o.side = 0.60; o.dly = 0.28; o.semis = 12;
                  shot("runrealfast-long-lo", t + 0.86, o); }
            }
            if (both) { Shot p = SHOT_SUNG(); p.gain = 0.44 * G * F; p.pan = 0.16; p.side = 0.55; p.dly = 0.28;
              shot("runrealfast-long-lo", t + 0.84 + jit(4), p); }
        }
    }

    // line 2 · "i wanna hide a — waaaay"
    if (!c.drop2 && !wordsIn(bar)) {
        dotDriftVox(t + 2.20 + jit(20), bar, 0.22 * G, 0.24, 1.4, 3.8, 0.40, 0.35, 0);
        int tri[3]; triad_of(degAt(bar), 59, tri);
        bop(t + 3.00 + jit(4), midihz(tri[1] + 12), 0.22 * G, -0.20, 0.7, 0.085, 0.40);
    } else if (!c.drop2 && !SPATIAL) {
        { Shot o = SHOT_SUNG(); o.gain = 0.94 * G; o.pan = -0.10; o.side = 0.5; o.dly = 0.24;
          sungSub("hideaway-hi", t + 2.00 + jit(4), o, c.thin ? 0 : 0.30); }
        { Shot o = SHOT_HELD(); o.gain = 0.80 * G; o.pan = -0.06; o.side = 0.62; o.dly = 0.26;
          o.wig = wigDepth(bar) * 0.8; o.wigHz = 4.9; o.wigPhase = 1.1;
          o.wigDrift = 0.3 * wigDepth(bar); o.wigIn = 0.55;
          shot("away-hi", t + 3.45 + jit(3), o); }
        if (both) { Shot o = SHOT_HELD(); o.gain = 0.52 * G; o.pan = 0.22; o.side = 0.62; o.dly = 0.26;
          o.wig = wigDepth(bar) * 0.9; o.wigHz = 6.3; o.wigPhase = 3.4;
          o.wigDrift = -0.35 * wigDepth(bar); o.wigIn = 0.48;
          shot("away-lo", t + 4.20 + jit(3), o); }
    }

    // line 3 · "i wanna dash"
    if (!c.drop3) {
        if (at(bar, 0) >= 54) {
            if (((bar >> 3) & 1) == 0 && has("iwannalong-c")) {
                Shot o = SHOT_SUNG(); o.gain = 0.70 * G; o.pan = 0.18; o.side = 0.5; o.dly = 0.28; o.atk = 0.03;
                shot("iwannalong-c", t + 3.92 + jit(4), o);
            } else if (((bar >> 3) & 1) == 0)
                stretched("iwanna-c-sung", t + 3.92 + jit(4), 0.68 * G, 0.18, 0, 2.4, 1.50, 0.5, 0.12, BUS_VOX, 0.30);
            else {
                Shot o = SHOT_SUNG(); o.gain = 0.72 * G; o.pan = 0.18; o.side = 0.5; o.dly = 0.24; o.atk = 0.05;
                shot(has("iwannaslow-c") ? "iwannaslow-c" : "iwanna-c-sung", t + 3.92 + jit(4), o);
            }
        }
        dashStack(t + 4.50, "d4", G * 0.98, bar, !c.thin && at(bar, 0) >= 54 && ((bar >> 3) & 1) == 0 && has("iwannalong-c"), 2.6);
    } else {
        dashStack(t + 4.00, "d4", G * 0.90, bar, 0, 0);
    }

    // line 4 · "dot dot dash" — the staccato dots enter on a 30 ms cosine
    if (!c.drop4) {
        { Shot o = SHOT_SUNG(); o.gain = 0.74 * G; o.pan = -0.45; o.side = 0.75; o.dly = 0.36; o.atk = 0.03;
          shot("dot-c-b3", t + 6.00 + jit(3), o); }
        { Shot o = SHOT_SUNG(); o.gain = 0.70 * G; o.pan = 0.45; o.side = 0.75; o.dly = 0.36; o.atk = 0.03;
          shot("dot-j-g3", t + 6.50 + sw8(bar) + jit(3), o); }
        dashStack(t + 7.00, "b3", G * 0.92, bar, 0, 0);
    }

    // the answer figure — a different one each statement
    if (c.answer == ANS_DOTS) {
        { Shot o = SHOT_SUNG(); o.gain = 0.30 * G; o.pan = 0.38; o.side = 0.8; o.dly = 0.5; o.atk = 0.02;
          shot("dot-c-b3", t + 1.30 + jit(4), o); }
        { Shot o = SHOT_SUNG(); o.gain = 0.26 * G; o.pan = -0.38; o.side = 0.8; o.dly = 0.5; o.atk = 0.03;
          shot(has("voxdot-c-b3") ? "voxdot-c-b3" : "dot-c-d4", t + 1.65 + jit(4), o); }
    } else if (c.answer == ANS_SOS) {
        const char *nms[3] = {"dot-b3", "dot-fs3", "dot-d4"};
        for (int k = 0; k < 3; k++) {
            Shot o = SHOT_SUNG(); o.gain = 0.26 * G;
            o.pan = k == 1 ? 0 : k ? 0.4 : -0.4; o.side = 0.85; o.dly = 0.55;
            shot(nms[k], t + 1.20 + k * 0.25 + jit(3), o);
        }
    }

    // the tag: the double-time line as punctuation — or a drifting dot
    if (c.tagFast) {
        if (wordsIn(bar)) {
            { Shot o = SHOT_SUNG(); o.gain = 0.54 * G; o.pan = 0.3; o.side = 0.7; o.dly = 0.45;
              shot("runrealfast-fast-hi", t + 7.0 + 0.00 + jit(3), o); }
            { Shot o = SHOT_SUNG(); o.gain = 0.42 * G; o.pan = -0.3; o.side = 0.7; o.dly = 0.45;
              shot("runrealfast-fast-lo", t + 7.0 + 0.50 + jit(3), o); }
        } else {
            dotDrift(t + 7.0 + jit(20), bar >> 2, 0.18 * G, 0.30, 0.95, 3.0, 0.45, 0.40, 0, 0);
        }
    }

    if (c.choirUnder) choir(bar, 0.12 * G, 0);   // deeper, more inset
}
// ── the trap hats ──────────────────────────────────────────────────────
static void trapHats(int bar, double level) {
    const double t = at(bar, 0);
    const int up = (bar & 1) == 0;
    static const double G16[16] = {0.10, 0.05, 0.07, 0.05, 0.09, 0.05, 0.07, 0.05, 0.10, 0.05, 0.07, 0.05, 0.09, 0.05, 0.07, 0.05};
    int hit = 0;
    for (int s = 0; s < 16; s++) {
        const int roll = s >= 12 ? 2 : (s == 6 && (bar & 2)) ? 3 : 1;
        for (int r = 0; r < roll; r++, hit++) {
            const double tt = t + ((double)s / 16) * BAR + ((double)r / roll) * (BAR / 16) + ((s & 1) ? sw16(bar) : 0);
            const int step = up ? hit % 12 : 11 - (hit % 12);
            Shot o = SHOT_DRUM(); o.gain = level * G16[s] * (r ? 0.7 : 1); o.pan = 0.55 * sin(hit * 0.9);
            o.side = 0.65; o.dur = roll > 1 ? 0.035 : 0.05; o.semis = step - 5;
            shot("hatC", tt, o);
        }
    }
}

// ── the field: Fibonacci slices of sung dots, its own generator ────────
static uint32_t fibSeed = 0x9e3779b1u;
static inline double frnd(void) { fibSeed = fibSeed * 1664525u + 1013904223u; return (double)fibSeed / 4294967296.0; }
#define NFIB 13
static const int FIB[NFIB] = {1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233};   // the math, fixed: see the JS
static const char *FIELD_DOTS[12] = {"dot-c-b3", "dot-c-cs4", "dot-c-d4", "dot-c-e4", "dot-c-fs4", "dot-c-g4", "dot-c-a4",
    "dot-j-b2", "dot-j-d3", "dot-j-fs3", "dot-a-d4", "dot-a-a3"};
static void dotField(double t0, double dur, double gain, int reverse) {
    double W[NFIB], tot = 0;
    for (int s = 0; s < NFIB; s++) { W[s] = reverse ? pow(1.12, s) : 1; tot += W[s]; }
    double tcur = t0; int hit = 0;
    for (int s = 0; s < NFIB; s++) {
        const int idx = reverse ? NFIB - 1 - s : s;
        const int count = FIB[idx]; const double slice = dur * W[s] / tot, gap = slice / count;
        const double g = gain / pow(dmax(1, count / 5.0), 0.3);
        const double prog = (double)s / (NFIB - 1);
        for (int k = 0; k < count; k++, hit++) {
            const char *nm = FIELD_DOTS[(int)floor(frnd() * 12)];
            const double semis = (frnd() - 0.5) * 10 + (reverse ? -6 * prog : 0);
            if (!has(nm)) continue;
            Shot o = SHOT_SUNG(); o.gain = g; o.pan = 0.92 * sin(hit * 2.39996); o.side = 0.85;
            o.dly = 0.25 + 0.35 * (reverse ? 1 - prog : prog); o.dur = dmin(0.14, dmax(0.04, gap * 1.6)); o.atk = 0.003; o.semis = semis;
            shot(nm, tcur + k * gap, o);
        }
        tcur += slice;
    }
}

// ── the power saw: seven detuned saws into an opening one-pole ────────
static void powerSaw(double t, int midi, double dur, double gain, double pan) {
    const long n = jsround((dur + 0.25) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    static const double DET[7] = {-14, -9, -4, 0, 4, 9, 14};
    double ph[7]; for (int k = 0; k < 7; k++) ph[k] = fmod(k * 0.137, 1.0);
    const double f0 = midihz(midi);
    double lp = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        double v = 0;
        for (int k = 0; k < 7; k++) {
            ph[k] += f0 * pow(2, DET[k] / 1200.0) / SR;
            if (ph[k] >= 1) ph[k] -= 1;
            v += 2 * ph[k] - 1;
        }
        v /= 7;
        const double open = 0.10 + 0.22 * smoothstep01(dmin(1, u / dmax(0.2, dur)));
        lp += open * (v - lp);
        const double env = dmin(1, u / 0.03) * (u > dur ? dmax(0, 1 - (u - dur) / 0.25) : 1);
        emit(BUS_MUSIC, i0 + i, lp * env * gain * tailFade(i, n), pan, sp, 0.55, 0.14);
    }
}

// ── the firework: a particle burst where the field ends ────────────────
static uint32_t fwSeed = 0x7a1c9e3du;
static inline double wrnd(void) { fwSeed = fwSeed * 1664525u + 1013904223u; return (double)fwSeed / 4294967296.0; }
static void firework(double t0, int count, double gain) {
    for (int k = 0; k < count; k++) {
        const double launch = wrnd() * 0.18;
        const double flight = 0.30 + wrnd() * 0.90;
        const double dir = wrnd() < 0.5 ? -1 : 1;
        const double reach = (0.25 + 0.75 * wrnd()) * dir;
        const char *nm = FIELD_DOTS[(int)floor(wrnd() * 12)];
        const double up = 4 + wrnd() * 10;
        const int crackle = wrnd() < 0.3 ? 3 + (int)floor(wrnd() * 2) : 1;
        if (!has(nm)) continue;
        for (int c = 0; c < crackle; c++) {
            const double u = crackle == 1 ? 0 : (double)c / (crackle - 1);
            const double at0 = t0 + launch + u * flight * 0.8;
            const double arc = sin(M_PI * dmin(1, u + 0.15));
            Shot o = SHOT_SUNG();
            o.gain = gain * (0.55 + 0.45 * wrnd()) * pow(0.72, c) * (1 - 0.45 * u);
            o.pan = reach * (0.15 + 0.85 * u); o.semis = up * arc - 3 * u;
            o.side = 0.7 + 0.28 * u; o.dly = 0.20 + 0.55 * u; o.dur = 0.06 + 0.04 * (1 - u); o.atk = 0.002; o.bright = 0.5;
            shot(nm, at0, o);
        }
    }
}

// ── the trail: a golden-ratio tail of Camille's dots ──────────────────
#define PHI 0.6180339887
static void dotTail(double t, int count, double gain, double pan, double dly) {
    double u = t, g = gain, gap = 0.5;
    char nm[64];
    for (int k = 0; k < count; k++) {
        snprintf(nm, sizeof nm, "dot-c-%s", CAM_SCALE[k % 7]);
        if (has(nm)) {
            Shot o = SHOT_SUNG(); o.gain = g; o.pan = pan * ((k & 1) ? -1 : 1); o.side = 0.72;
            o.dly = dly + 0.04 * k; o.atk = 0.012; o.dur = dmin(0.19, gap * 1.3);
            shot(nm, u, o);
        }
        u += gap; gap *= PHI; g *= pow(PHI, 0.6);
    }
}

// ── bubbles: sines that rise 1.6x over 45 ms and are gone in 80 ──────
static uint32_t bubSeed = 0x2f6e2b1u;
static inline double brnd(void) { bubSeed = bubSeed * 1664525u + 1013904223u; return (double)bubSeed / 4294967296.0; }
static void bubbles(double t, double dur, int count, double gain) {
    for (int k = 0; k < count; k++) {
        const double at0 = t + brnd() * dur;
        const double f0 = 250 + brnd() * 2150;
        const double pan = (brnd() - 0.5) * 1.6;
        const double fadeIn = (at0 - t) / dur;
        const double g = gain * (0.45 + 0.55 * brnd()) * (0.12 + 0.88 * fadeIn * fadeIn);
        const double glide = brnd() * 2.2 - 0.6;
        const int reps = brnd() < 0.35 ? 2 + (int)floor(brnd() * 3) : 1;
        const double hop = 0.018 + brnd() * 0.012;
        for (int r = 0; r < reps; r++) {
            const long n = jsround(0.085 * SR), i0 = jsround((at0 + r * hop) * SR);
            const Sp sp = spatial(pan * 1.2);
            double ph = 0;
            for (long i = 0; i < n; i++) {
                const double u = (double)i / SR;
                const double f = f0 * (1 + glide * smoothstep01(dmin(1, u / 0.045)));
                ph += (TAU * f) / SR;
                const double env = dmin(1, u / 0.0015) * exp(-u * 48);
                emit(BUS_SIG, i0 + i, sin(ph) * env * g * pow(0.7, r) * tailFade(i, n), pan, sp, 0.8, 0.40);
            }
        }
    }
}

// ── score ──────────────────────────────────────────────────────────────
// The 112-bar loop, translated block-for-block from render10.mjs with the
// SAME evaluation order — every jit()/vel()/nrnd() lands on the same draw.
static void score(void) {
    // the dinner bell — B6 off the FEM engine, on the kick-ducked music bus,
    // struck at bar 8 and again at the bar-29 seam the release edit makes
    if (has("gong-b6")) {
        { Shot o = shot_def(BUS_MUSIC, 0.55); o.gain = 0.18; o.pan = -0.06; o.dly = 0.30; o.dark = 0.06; o.atk = 0.004;
          shot("gong-b6", at(8, 0) - 0.12, o); }
        { Shot o = shot_def(BUS_MUSIC, 0.62); o.gain = 0.155; o.pan = 0.08; o.dly = 0.38; o.dark = 0.20; o.atk = 0.35;
          shot("gong-b6", at(29, 0), o); }
    }
    // the room: four-bar air tiles, overlapped by a bar, level by act
    for (int b = 8; b < BARS; b += 4) {
        const double act = b < 24 ? 0.55 : b < 40 ? 0.85 : b < 48 ? 0.62
            : b < 64 ? 1.00 : b < 76 ? 1.18 : b < 96 ? 1.05 : 0.80;
        const int hums = b >= 40 && b < 48;   // deep scary air under the cult hums
        air(b * BAR, BAR * 5, 0.0085 * act * (hums ? 1.4 : 1),
            0.085 + 0.02 * ((b >> 2) % 3), 0.30 + 0.05 * ((b >> 2) % 4), 0.26, hums ? 2.2 : 1);
    }
    // The first sound: the watery-hole ding, at the cut's first sample.
    // accordion at 0:34 — one G bellows across the bar-47 line, and its opposite-bellows answer
    { Shot o = SHOT_HELD(); o.gain = 0.17 * phraseLevel(46, 0.8, 0.18); o.pan = 0.22; o.side = 0.72; o.dly = 0.42; o.dark = 0.26; o.atk = 0.34; o.dur = 3.4;
      shot("accordion-g", at(46, 2.0), o); }
    { Shot o = SHOT_HELD(); o.gain = 0.075 * phraseLevel(47, 1.3, 0.18); o.pan = -0.24; o.side = 0.80; o.dly = 0.50; o.dark = 0.36; o.semis = 0.06; o.atk = 0.40; o.dur = 2.6;
      shot("accordion-g", at(47, 1.0), o); }
    // accordion at 0:34 — one G bellows across the bar-47 line, and its opposite-bellows answer
    { Shot o = SHOT_HELD(); o.gain = 0.17 * phraseLevel(46, 0.8, 0.18); o.pan = 0.22; o.side = 0.72; o.dly = 0.42; o.dark = 0.26; o.atk = 0.34; o.dur = 3.4;
      shot("accordion-g", at(46, 2.0), o); }
    { Shot o = SHOT_HELD(); o.gain = 0.075 * phraseLevel(47, 1.3, 0.18); o.pan = -0.24; o.side = 0.80; o.dly = 0.50; o.dark = 0.36; o.semis = 0.06; o.atk = 0.40; o.dur = 2.6;
      shot("accordion-g", at(47, 1.0), o); }
    // one strike, on the first sung dash (a hair after the 400 ms door)
    { Shot o = SHOT_DRUM(); o.gain = 0.95; o.pan = 0.0; o.side = 0.30; o.dly = 0.48;
      shot("waterhole", 29 * BAR - 0.02, o); }
    // The station ident — bar 18 (cut by the release edit), then bar 8 in
    // the bars that ship, with an octave-up angel a breath behind it.
    { Shot o = SHOT_HELD(); o.gain = 0.55; o.pan = 0.0; o.side = 0.40; o.dly = 0.30; o.dark = 0.25;
      shot("dotorg", 18 * BAR + 0.25, o); }
    const char *STAMP = has("dotorg-long") ? "dotorg-long" : "dotorg";   // the aesthetivoxed stretch
    { Shot o = shot_def(BUS_DRUMS, 0.52); o.gain = 0.66; o.pan = 0.0; o.dly = 0.34; o.dark = 0.05; o.bright = 0.6; o.edge = 0.3;
      shot(STAMP, 8 * BAR + 0.62, o); }   // the drum bus never ducks
    { Shot o = SHOT_HELD(); o.gain = 0.115; o.semis = 12; o.pan = 0.0; o.side = 0.96; o.dly = 0.62; o.dark = 0; o.bright = 0.65; o.atk = 0.09;
      shot(STAMP, 8 * BAR + 0.70, o); }
    // doooooot harmonies: the answer chord stretched five-fold into slow
    // swells drifting through the intro — B minor sung glacially.
    { static const struct { double b; const char *n; double p; } SD[6] = {
        {12.5, "dot-b3", -0.35}, {12.53, "dot-fs3", 0.35}, {12.56, "dot-d4", 0.0},
        {20.5, "dot-b3", 0.35}, {20.53, "dot-fs3", -0.35}, {20.56, "dot-d4", 0.0}};
      for (int k = 0; k < 6; k++)   // tamed: quieter, shorter, darker
        stretched(SD[k].n, SD[k].b * BAR, 0.10, SD[k].p, 0, 3.5, 4.0, 0.85, 0.45, BUS_TUBE, 0.55); }
    // The dot dot dots, seeded into the first 30 seconds. Fixed times.
    { static const int DB[4] = {10, 14, 16, 22};
      for (int k = 0; k < 4; k++) {
        const double u = DB[k] * BAR; const double g = 0.34 + 0.09 * k;
        { Shot o = SHOT_HELD(); o.gain = g; o.pan = -0.45; o.side = 0.75; o.dly = 0.45; o.dark = 0.20;
          shot("dot-b3", u + 3.0 * BEAT, o); }
        { Shot o = SHOT_HELD(); o.gain = g; o.pan = 0.45; o.side = 0.75; o.dly = 0.45; o.dark = 0.20;
          shot("dot-fs3", u + 3.5 * BEAT, o); }
        { Shot o = SHOT_HELD(); o.gain = g * 0.45; o.pan = 0.0; o.side = 0.60; o.dly = 0.60; o.dark = 0.25;
          shot("dot-d4", u + 4.0 * BEAT, o); }
      } }
    // Camille's clearing reveals these colors one at a time: one accordion
    // breath in its first half, one violin line in its second. No doubles.
    { Shot o = SHOT_HELD(); o.gain = 0.22; o.pan = -0.08; o.side = 0.70; o.dly = 0.44; o.dark = 0.30;
      shot("accordion-secret", 41 * BAR, o); }
    { Shot o = SHOT_HELD(); o.gain = 0.24; o.pan = 0.10; o.side = 0.64; o.dly = 0.52; o.dark = 0.34;
      shot("violin-secret", 44.4 * BAR, o); }
    { static const char *AN[4] = {"accordion-b", "accordion-d", "accordion-g", "accordion-e"};
      // the bellows begin answering in IT SPREADS…
      for (int ab = 64; ab < 68; ab += 2) {
        const char *name = AN[((ab - 64) / 2) % 4];
        const double entrance = ab == 64 ? 0.16 : 0;
        Shot o = SHOT_HELD(); o.gain = (ab == 64 ? 0.10 : 0.12) * phraseLevel(ab, 0.8, 0.18);
        o.pan = ((ab >> 1) & 1) ? -0.30 : 0.30; o.side = 0.72; o.dly = 0.36; o.dark = 0.28;
        if (ab == 64) o.atk = 0.22; else o.dur = 3.45;
        shot(name, ab * BAR + entrance, o);
      }
      // …enter after the splice with a soft bellows attack…
      { static const int AB2[2] = {72, 74}; static const char *AN2[2] = {"accordion-g", "accordion-b"};
        for (int q = 0; q < 2; q++) {
          const int ab = AB2[q]; const double pan = ab == 72 ? 0.24 : -0.24;
          { Shot o = SHOT_HELD(); o.gain = 0.090 * phraseLevel(ab, 0.8, 0.18); o.pan = pan;
            o.side = 0.72; o.dly = 0.38; o.dark = 0.26; o.atk = 0.34; o.dur = 3.45;
            shot(AN2[q], ab * BAR + 0.10, o); }
          { Shot o = SHOT_HELD(); o.gain = 0.034 * phraseLevel(ab, 1.3, 0.18); o.pan = -pan;
            o.side = 0.80; o.dly = 0.48; o.dark = 0.38; o.semis = 0.06; o.atk = 0.38; o.dur = 3.30;
            shot(AN2[q], ab * BAR + 0.15, o); }
        } }
      // …and become a continuous push/pull chord section in THE WHOLE MESSAGE
      for (int ab = 76; ab < 96; ab += 2) {
        const char *name = AN[((ab - 76) / 2) % 4];
        const double pan = ((ab >> 1) & 1) ? -0.27 : 0.27;
        const double breath = phraseLevel(ab, 0.8, 0.20);
        const int seamTrim = ab == 82;
        { Shot o = SHOT_HELD(); o.gain = 0.18 * breath; o.pan = pan; o.side = 0.68; o.dly = 0.30; o.dark = 0.18;
          if (seamTrim) o.dur = 3.55;
          shot(name, ab * BAR, o); }
        { Shot o = SHOT_HELD(); o.gain = 0.075 * breath; o.pan = -pan; o.side = 0.78; o.dly = 0.44; o.dark = 0.34; o.semis = 0.07;
          if (seamTrim) o.dur = 3.42;
          shot(name, ab * BAR + 0.032, o); }
        { Shot o = SHOT_HELD(); o.gain = 0.095 * phraseLevel(ab + 1, 0.8, 0.20); o.pan = -pan * 0.75;
          o.side = 0.72; o.dly = 0.38; o.dark = 0.26; o.off = 0.32;
          if (seamTrim) o.dur = 1.65;
          shot(name, (ab + 1) * BAR, o); }
      } }
    // boing boing: a springy sproing on every act-VII chord change
    { static const char *BN[4] = {"boing-b", "boing-d", "boing-g", "boing-e"};
      for (int bb = 76; bb < 96; bb += 2) {
        Shot o = SHOT_HELD(); o.gain = 0.30; o.pan = ((bb >> 1) & 1) ? 0.32 : -0.32;
        o.side = 0.65; o.dly = 0.35; o.dark = 0.10;
        shot(BN[(bb - 76) / 2 % 4], bb * BAR, o);
      } }
    // the guitar, reorchestrated on the strum machine (GUITAR-CRITIQUE.md):
    // chord follows the bed, every take on the grid
    {
        char nm[64];
        for (int gb = 29; gb < 37; gb++) {   // folky acoustic under the first hooks
            snprintf(nm, sizeof nm, "gt-folk-%s", gtChord(gb));
            if (has(nm)) { Shot o = shot_def(BUS_MUSIC, 0.6); o.gain = 0.14 * phraseLevel(gb, 0.2, 0.22); o.pan = (gb & 1) ? 0.22 : -0.18; o.dly = 0.22; o.dark = 0.06; o.dur = 2.2; shot(nm, gb * BAR, o); }
        }
        for (int gb = 48; gb < 64; gb++) {   // the reply: palm-muted electric on the bed's chord
            if (soloBar(gb)) continue;
            snprintf(nm, sizeof nm, "gt-palm-%s", gtChord(gb));
            if (has(nm)) { Shot o = shot_def(BUS_MUSIC, 0.5); o.gain = 0.19 * phraseLevel(gb, 0.2, 0.22); o.pan = (gb & 1) ? 0.14 : -0.14; o.dly = 0.18; o.dark = 0.10; o.dur = 2.05; shot(nm, gb * BAR, o); }
        }
        if (has("gt-flower")) { Shot o = shot_def(BUS_MUSIC, 0.62); o.gain = 0.16; o.pan = 0.10; o.dly = 0.28; o.dark = 0.04; o.dur = 8.6; shot("gt-flower", 64 * BAR, o); }
        for (int gb = 76; gb < 96; gb++) {   // the finale hand, on the grid
            snprintf(nm, sizeof nm, "gt-%s-%s", (gb & 1) ? "rockx" : "rock", gtChord(gb));
            if (has(nm)) { Shot o = shot_def(BUS_MUSIC, 0.55); o.gain = 0.15 * phraseLevel(gb, 0.2, 0.22); o.pan = (gb & 2) ? 0.12 : -0.12; o.dly = 0.24; o.dark = 0.08; o.dur = 2.1; shot(nm, gb * BAR, o); }
        }
    }
    {
        // …and one line that actually moves through them
        static const struct { double b; int m; double d; } FLOWER[5] = {
            {66.00, 66, 0.95}, {66.52, 69, 0.85}, {66.88, 71, 1.15}, {67.30, 73, 0.75}, {67.62, 69, 1.65}};
        for (int i = 0; i < 5; i++)
            guitar(FLOWER[i].b * BAR, FLOWER[i].m, FLOWER[i].d,
                   (0.070 - 0.004 * i) * phraseLevel(66, 0.2, 0.18), (i & 1) ? 0.30 : -0.24, 0.9976);

        static const int SHRED[9] = {59, 62, 64, 66, 69, 71, 74, 76, 78};
        static const int SHRED_R[9] = {78, 76, 74, 71, 69, 66, 64, 62, 59};
        static const struct { int bar, flip; } SH[3] = {{55, 1}, {63, 0}, {91, 1}};
        for (int q = 0; q < 3; q++)
            guitarShred(at(SH[q].bar, 2.75), SH[q].flip ? SHRED_R : SHRED, 9,
                        (SH[q].bar == 55 ? 0.13 : SH[q].bar >= 76 ? 0.135 : 0.115) * phraseLevel(SH[q].bar, 0.2, 0.18),
                        SH[q].flip ? -0.42 : 0.42);
    }
    // One bowed answer crosses the middle pivot.
    { Shot o = SHOT_HELD(); o.gain = 0.17 * phraseLevel(54, 1.9, 0.18); o.pan = 0.12;
      o.side = 0.68; o.dly = 0.48; o.dark = 0.31;
      shot("violin-secret", 54.45 * BAR, o); }

    for (int bar = 0; bar < BARS; bar++) {
        const double t = at(bar, 0);
        const int deg = degAt(bar);
        const int root = bassRoot(deg);
        int chord[3]; triad_of(deg, 59, chord);
        // Bar 34 / 64: the same harmony in close inversions instead of a
        // root-position leap
        int padChord[3], stabChord[3];
        if (bar == 34) { padChord[0] = 47; padChord[1] = 50; padChord[2] = 55; stabChord[0] = 59; stabChord[1] = 62; stabChord[2] = 67; }
        else if (bar == 64) { padChord[0] = 50; padChord[1] = 54; padChord[2] = 59; stabChord[0] = 62; stabChord[1] = 66; stabChord[2] = 71; }
        else { for (int q = 0; q < 3; q++) { padChord[q] = chord[q] - 12; stabChord[q] = chord[q]; } }
        const int four = bar % 4, eight = bar % 8;
        const double push = 0.0045 * sin(TAU * (bar % 32) / 32.0 + 0.7);
        const int D = dense(bar);

        // ---- kick: 4/4, POW per hit, never a drop ----------------------
        if (kickOn(bar)) {
            static const double R8[4] = {0.42, 0.46, 0.50, 0.56}, R9[4] = {0.56, 0.64, 0.74, 0.86};
            const double *ramp = bar == 8 ? R8 : bar == 9 ? R9 : NULL;
            const double g = inS(bar, S_RECOGNISE) ? 0.90 - 0.06 * (bar - SB[S_RECOGNISE][0]) : 0.96;
            const double sharp = 0.25 + 0.75 * (swingAmt(bar) / 0.34);   // sharper as the record swings harder
            for (int b = 0; b < 4; b++) {
                const double hit = ramp ? ramp[b] : g * (b == 0 ? 1 : 0.95);
                kicks(t + b * BEAT + jit(2.5), hit, b == 0 ? 1.0 : 0.86, 1.0, sharp);
            }
            if (swingAmt(bar) > 0) {   // the swung ghosts
                kicks(t + 3.5 * BEAT + sw8(bar), 0.24 * (swingAmt(bar) / 0.34) * g, 0.5, 1.0, sharp);
                if (swingAmt(bar) >= 0.26) kicks(t + 1.5 * BEAT + sw8(bar), 0.18 * (swingAmt(bar) / 0.34) * g, 0.5, 1.0, sharp);
            }
        }

        // a soft kick and the trap hats through the chants (fixed times, no jit)
        if (inS(bar, S_SECRET) || inS(bar, S_CARRIEROFF)) {
            const double fadeK = inS(bar, S_CARRIEROFF) ? 1 - (double)(bar - SB[S_CARRIEROFF][0]) / 9 : 1;
            const int k0 = inS(bar, S_SECRET) ? SB[S_SECRET][0] : SB[S_CARRIEROFF][0];
            const double pitch = 0.80 - 0.10 * (bar - k0) / 8.0;   // deeper, sinking through the act
            for (int b = 0; b < 4; b++)
                kickp(t + b * BEAT, (b == 0 ? 0.26 : 0.21) * fadeK, 0.55, pitch);
        }
        if (inS(bar, S_SECRET) || (inS(bar, S_CARRIEROFF) && bar < SB[S_CARRIEROFF][0] + 6))
            trapHats(bar, inS(bar, S_SECRET) ? 0.62 : 0.50 * (1 - (double)(bar - SB[S_CARRIEROFF][0]) / 8));

        // ---- hats ------------------------------------------------------
        if (hatOn(bar)) {
            for (int s = 0; s < 8; s++) {
                const double swing = (s & 1) ? 0.035 : 0;
                const double u = t + (s * 0.5 + swing) * BEAT + push + ((s & 1) ? sw8(bar) : 0) + jit(5);
                if (s & 1) { Shot o = SHOT_DRUM(); o.gain = 0.20 * vel(0.20); o.pan = 0.20; o.side = 0.5; o.dur = 0.085; shot("hatC", u, o); }
                else if (s % 4 == 2) { Shot o = SHOT_DRUM(); o.gain = 0.11 * vel(0.20); o.pan = -0.18; o.side = 0.5; o.dur = 0.065; shot("hatC", u, o); }
            }
            if (D || eight >= 2)
                for (int s = 0; s < 16; s++)
                    if (s % 4 == 1 || s % 4 == 3) {
                        const double u = t + ((double)s / 16) * BAR + push + sw16(bar) + jit(4);
                        Shot o = SHOT_DRUM(); o.gain = 0.055 * vel(0.5); o.pan = (s & 2) ? 0.35 : -0.35; o.side = 0.6; o.dur = 0.045;
                        shot("hatC", u, o);
                    }
            if ((D || inS(bar, S_SPREAD)) && four == 3 && bar != 63) {
                Shot o = SHOT_DRUM(); o.gain = 0.20; o.pan = -0.28; o.side = 0.65; o.dur = 0.34;
                shot("hatO", t + 3.5 * BEAT + 0.02, o);
            }
            if (four == 3 && kickOn(bar) && bar != 63)   // bar 63's fill is cut by the edit
                for (int r = 0; r < 6; r++) {
                    const double u = t + 3.25 * BEAT + r * BEAT / 8;
                    Shot o = SHOT_DRUM(); o.gain = (0.034 + 0.015 * r) * vel(0.3);
                    o.pan = 0.22 - 0.09 * r; o.side = 0.6; o.dur = 0.05;
                    shot("hatC", u, o);
                }
        }

        // ---- the pump's second trigger: rim on 3, clap when full -------
        if (kickOn(bar)) {
            { const double tt = t + 2 * BEAT + push + jit(5); snare(tt, vel(0.20)); }
            if (D) {
                { Shot o = SHOT_DRUM(); o.gain = 0.22; o.pan = -0.12; o.side = 0.72; shot("clap", t + 2 * BEAT + push + jit(5), o); }
                { Shot o = SHOT_DRUM(); o.gain = 0.09; o.pan = 0.30; o.side = 0.55; shot("snap", t + 2 * BEAT + 0.012, o); }
            }
        }
        if ((inS(bar, S_SPREAD) || inS(bar, S_WHOLE)) && !soloBar(bar) && !sparseSpreadBar(bar))
            for (int s = 0; s < 4; s++) {
                const double u = t + (s + 0.5) * BEAT + sw8(bar) + jit(8);
                Shot o = SHOT_DRUM(); o.gain = 0.055 * vel(0.4); o.pan = (s & 1) ? 0.38 : -0.38; o.side = 0.7; o.dur = 0.22;
                shot("ride", u, o);
            }
        if (hatOn(bar) && !D)
            for (int s = 0; s < 4; s++) {
                const double u = t + (s + 0.5) * BEAT + sw8(bar) + jit(9);
                Shot o = SHOT_DRUM(); o.gain = 0.055 * vel(0.4); o.pan = (s & 1) ? 0.42 : -0.42; o.side = 0.7; o.dur = 0.09;
                shot("tambo", u, o);
            }

        // ---- "i want more perc play" — ghosts, blocks, a turnaround ----
        if (kickOn(bar)) {
            if (D || eight >= 4) {
                { const double tt = t + 1.25 * BEAT + jit(6); snare(tt, 0.22 * vel(0.3)); }
                if (bar % 2) { const double tt = t + 2.75 * BEAT + jit(6); snare(tt, 0.16 * vel(0.3)); }
            }
            if (D) {
                static const double bs[3] = {0.75, 1.5, 3.25};
                static const int boff[3] = {3, 6, 13};   // s*4, exactly
                for (int k = 0; k < 3; k++)
                    if ((bar + boff[k]) % 3 < 2) {
                        const double u = t + bs[k] * BEAT + (bs[k] == 1.5 ? sw8(bar) : sw16(bar)) + jit(8);
                        Shot o = SHOT_DRUM(); o.gain = 0.10 * vel(0.4); o.pan = bs[k] > 2 ? 0.5 : -0.34; o.side = 0.7; o.dur = 0.09;
                        shot("block", u, o);
                    }
            }
            if (eight == 7) {
                for (int k = 0; k < 3; k++) {
                    const double tt = t + (3 + (double)k / 3) * BEAT + jit(4);
                    snare(tt, (0.30 + 0.14 * k) * vel(0.2));
                }
                Shot o = SHOT_DRUM(); o.gain = 0.16; o.pan = 0.2; o.side = 0.6; o.dur = 0.3;
                shot("hatO", t + 3.9 * BEAT, o);
            }
        }

        // ---- the drive under the warble (bars 64-67): density climbs ---
        if (bar >= 65 && bar < 68) {
            const int k = bar - 65;
            const int div = k == 0 ? 8 : 16;
            for (int s = 1; s < div; s += 2) {
                const double lean = -0.004 * k;
                const double u = t + ((double)s / div) * BAR + lean + (div == 8 ? sw8(bar) : sw16(bar)) + jit(5);
                Shot o = SHOT_DRUM(); o.gain = (0.055 + 0.020 * k) * vel(0.4);
                o.pan = (s & 2) ? 0.34 : -0.30; o.side = 0.62; o.dur = 0.042;
                shot("hatC", u, o);
            }
            if (k >= 1) { static const int bts[2] = {1, 3};
                for (int q = 0; q < 2; q++)
                    kick(t + bts[q] * BEAT - 0.012 - 0.004 * k, 0.34 + 0.06 * k, 0.7); }
            if (k == 2) { Shot o = SHOT_DRUM(); o.gain = 0.19; o.pan = 0.24; o.side = 0.7; o.dur = 0.30;
                shot("hatO", t + 3.75 * BEAT, o); }
        }

        // ---- the re-entries announce themselves ------------------------
        if (bar == SB[S_REPLY][0] || bar == SB[S_WHOLE][0])
            revKick(t, bar == SB[S_WHOLE][0] ? 3.0 : 0.9, bar == SB[S_WHOLE][0] ? 0.52 : 0.48);
        if (bar == 9)
            revKick(at(10, 0), 3.75, 0.60);   // two-bar wooooop into the vocal snap
        if (bar == SB[S_MESSAGE][0] + 5)
            revKick(t, 0.9, 0.48);
        if ((inS(bar, S_REPLY) && bar - SB[S_REPLY][0] < 2)
            || (!dotFieldBar(bar) && (bar == SB[S_WHOLE][0] - 2 || bar == SB[S_WHOLE][0] - 1)))
            wub(t, bassRoot(deg), 1, 0.26, 4);

        // ---- bass ------------------------------------------------------
        if (kickOn(bar)) {
            const double g = introBar(bar) ? 0.52 : inS(bar, S_RECOGNISE) ? 0.72 : 0.86;
            for (int b = 0; b < 4; b++) {
                const int fifth = four == 3 && b == 3;
                bass(t + (b + 0.5) * BEAT + sw8(bar) + jit(3), root + (fifth ? 7 : 0), 0.26, g, fifth ? root : NO_SLIDE);
            }
            if (bar % 2 == 0) {
                const int subNote = bar == 34 ? root - 24 : bar == 64 ? root : root - 12;
                const double subTime = bar == 34 ? t + 0.10 : bar == 64 ? t + 0.12 : t;
                const int subFrom = bar == 34 ? bassRoot(degAt(bar - 1)) - 12
                    : bar == 64 ? bassRoot(degAt(59)) : NO_SLIDE;
                bass(subTime, subNote, BAR * 0.90, g * 0.60, subFrom);
            }
        }

        // ---- pad + stabs -----------------------------------------------
        if (bar % 2 == 0 && !inS(bar, S_SECRET) && !soloBar(bar) && !releaseGapBar(bar))
            sines(t + (bar == 34 ? 0.08 : bar == 64 ? 0.12 : 0), padChord, 3, BAR * 1.85,
                  inS(bar, S_CARRIER) ? 0.055 + 0.012 * bar : 0.085,
                  bar % 4 ? 0.22 : -0.22, 0.75, 0.5, 0,
                  bar == 34 ? 0.46 : bar == 64 ? 0.52 : 0.30);
        if ((inS(bar, S_MESSAGE) || inS(bar, S_REPLY) || inS(bar, S_WHOLE) || inS(bar, S_SPREAD))
            && !soloBar(bar) && !sparseSpreadBar(bar)) {
            static const double bs[2] = {0.5, 2.5};
            for (int k = 0; k < 2; k++)
                sines(t + bs[k] * BEAT + sw8(bar) + jit(4), stabChord, 3, 0.20, 0.075, bs[k] > 1 ? 0.36 : -0.36, 0.7, 0.9, 0.34, 0.020);
        }
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar) && four == 1) {
            int hic[3] = {chord[0] + 12, chord[1] + 12, chord[2] + 12};
            sines(t + 3.5 * BEAT, hic, 3, 0.13, 0.070, rnd() > 0.5 ? 0.5 : -0.5, 0.85, 0.7, 0.70, 0.020);
        }
        if (dotFieldBar(bar)) {
            // a reduced ensemble, not a drop
            const double gather = (double)(bar - 72) / 3;
            static const double bs[2] = {0.5, 2.5};
            for (int k = 0; k < 2; k++)
                sines(t + bs[k] * BEAT + jit(4), chord, 3, 0.28, 0.036 + gather * 0.020,
                      bs[k] > 1 ? 0.30 : -0.30, 0.72, 0.76, 0.38, 0.020);
            char un[64]; snprintf(un, sizeof un, "gt-up-%s", gtChord(bar));
            if (has(un)) { Shot o = shot_def(BUS_MUSIC, 0.7); o.gain = 0.11 + gather * 0.03; o.pan = (bar & 1) ? -0.20 : 0.20; o.dly = 0.36; o.dark = 0.05; o.dur = 2.4; shot(un, t, o); }
            else if ((bar & 1) == 0) {
                const int gc[4] = {chord[0] - 12, chord[1] - 12, chord[2] - 12, chord[0]};
                guitarChord(t + 0.08, gc, 4, 3.52, 0.058 + gather * 0.018, bar == 72 ? 0.18 : -0.18, bar == 74);
            }
        }

        // bar 9 becomes an instrumental pickup toward the vocal downbeat
        if (bar == 9) {
            const int rise[4] = {chord[0] - 12, chord[1] - 12, chord[2] - 12, chord[0]};
            for (int k = 0; k < 4; k++) {
                const int one[1] = {rise[k]};
                sines(t + k * BEAT, one, 1, BEAT * 0.76, 0.034 + k * 0.014,
                      k % 2 ? 0.22 : -0.22, 0.66, 0.58 + k * 0.10, 0.22, 0.025);
            }
            wub(t, root, 1, 0.12, 2);
            static const int RUN[6] = {47, 50, 54, 59, 62, 66};
            guitarShred(t + 1.32, RUN, 6, 0.075, 0.25);
            if (has("gt-pickup-bm")) { Shot o = shot_def(BUS_MUSIC, 0.6); o.gain = 0.12; o.pan = 0.16; o.dly = 0.30; o.dark = 0.05; o.dur = 2.6; shot("gt-pickup-bm", t, o); }
        }
        if (bar == 8 || bar == 9) {
            // a rising C-U-L-T keypad exchange fills the two setup bars
            static const struct { double beat; char digit; double pan, gain, dur; } C8[2] = {
                {1.72, '2', -0.46, 0.24, 0.22}, {3.18, '8', 0.42, 0.27, 0.26}};
            static const struct { double beat; char digit; double pan, gain, dur; } C9[4] = {
                {0.62, '5', -0.38, 0.27, 0.25}, {1.48, '8', 0.34, 0.30, 0.28},
                {2.42, '*', -0.24, 0.32, 0.31}, {3.28, '2', 0.18, 0.35, 0.36}};
            const int nc = bar == 8 ? 2 : 4;
            for (int k = 0; k < nc; k++) {
                const double beat = bar == 8 ? C8[k].beat : C9[k].beat;
                const char digit = bar == 8 ? C8[k].digit : C9[k].digit;
                const double pan = bar == 8 ? C8[k].pan : C9[k].pan;
                const double gain = bar == 8 ? C8[k].gain : C9[k].gain;
                const double dur = bar == 8 ? C8[k].dur : C9[k].dur;
                const double bt = t + beat * BEAT + jit(3);
                dtmfx(bt, digit, dur, gain, pan, 0.86, 0.72, 0.012, 0.16);
                const int a = chord[k % 3] + 12;
                const int b = chord[(k + 1) % 3] + 12;
                beepx(bt + 0.026, midihz(a), midihz(b), dur * 1.18,
                      gain * 0.62, -pan * 0.72, 0.92, 0.82, BUS_SIG, 0.020, 0.28);
                // interlocking echo taps; bounded so no tail crosses the 20 s edit
                double taps[2][2]; int nt = 0;
                if (bar == 8) {
                    if (k == 0) { taps[0][0] = 0.72; taps[0][1] = 0.36; taps[1][0] = 1.42; taps[1][1] = 0.18; nt = 2; }
                    else { taps[0][0] = 0.42; taps[0][1] = 0.25; nt = 1; }
                } else {
                    if (k == 0) { taps[0][0] = 0.38; taps[0][1] = 0.34; taps[1][0] = 0.72; taps[1][1] = 0.17; nt = 2; }
                    else if (k == 1) { taps[0][0] = 0.50; taps[0][1] = 0.30; taps[1][0] = 0.78; taps[1][1] = 0.15; nt = 2; }
                    else if (k == 2) { taps[0][0] = 0.34; taps[0][1] = 0.26; taps[1][0] = 0.62; taps[1][1] = 0.13; nt = 2; }
                    else nt = 0;
                }
                for (int e = 0; e < nt; e++) {
                    const double offBeat = taps[e][0], level = taps[e][1];
                    const double et = bt + offBeat * BEAT;
                    const double ed = dmin(0.16 + e * 0.03, dmax(0.06, 19.86 - et));
                    if (et + ed >= 19.92) continue;
                    dtmfx(et, digit, ed, gain * level, -pan * (0.85 + 0.1 * e), 0.94, 0.86, 0.012, 0.28);
                    beepx(et + 0.018, midihz(b), midihz(a), ed * 1.12,
                          gain * level * 0.68, pan * (e ? 1 : -1), 0.98, 0.92, BUS_SIG, 0.018, 0.36);
                }
            }
            if (bar == 9)
                bop(t + 3.58 * BEAT, midihz(chord[2] + 12), 0.20, 0.34, 0.76, 0.085, 0.42);
        }
        if (bar >= 29 && bar < 33) {
            // the phone harmony — now the OPENING of the shipped record:
            // lower, bar 29 without its downbeat tone, replies and dial back
            const int k = bar - 29;
            static const double FADE[4] = {0.17, 0.15, 0.11, 0.07};
            const double fade = FADE[k];
            double lb[3]; int nl;
            if (k == 0) { lb[0] = 1.62; lb[1] = 3.08; nl = 2; }
            else if (k == 1) { lb[0] = 0.18; lb[1] = 1.62; lb[2] = 3.08; nl = 3; }
            else { lb[0] = 0.32; lb[1] = 2.58; nl = 2; }
            for (int q = 0; q < nl; q++) {
                const int a = chord[(q + k) % 3] + 12;
                const int b = chord[(q + k + 1) % 3] + 12;
                const double bt = t + lb[q] * BEAT + jit(3);
                const double pan = (q & 1) ? 0.32 : -0.32;
                beepx(bt, midihz(a), midihz(b), 0.30 + 0.05 * (2 - q),
                      fade, pan, 0.92, 0.80, BUS_SIG, 0.018, 0.30);
                const double echoBeat = (q & 1) ? 0.42 : 0.68;
                beepx(bt + echoBeat * BEAT, midihz(b + 12), midihz(a + 12), 0.18 + 0.03 * (2 - q),
                      fade * (0.28 - 0.04 * k), -pan * 1.35, 0.98, 0.94, BUS_SIG, 0.024, 0.44);
            }
            dtmfx(t + (3.55 - 0.18 * k) * BEAT, CULT_DIAL[k], 0.16 + 0.025 * (3 - k),
                  fade * 0.52, (k & 1) ? -0.42 : 0.42, 0.92, 0.82, 0.010, 0.28);
        }
        // two bars of phone harmonics tighten into the bar-31 "run real fast"
        if (bar == 30) {
            const double dropT = at(31, 0);
            revKick(dropT, 1.90, 0.68);
            static const int climb[6] = {59, 62, 66, 69, 71, 74};
            static const double beats[6] = {0.18, 0.92, 1.48, 2.04, 2.62, 3.30};
            for (int k = 0; k < 6; k++) {
                const double bt = t + beats[k] * BEAT;
                const double g = 0.075 + 0.020 * k;
                beepx(bt, midihz(climb[k] + 12), midihz(climb[k - 2 > 0 ? k - 2 : 0] + 12), 0.12 + 0.014 * k,
                      g, (k & 1) ? 0.48 : -0.48, 0.96, 0.88, BUS_SIG, 0.012, 0.28);
            }
        }
        if (bar == 31) {
            wub(t, bassRoot(deg), 1, 0.22, 6);
            if (has("gt-stroke-bm")) { Shot o = shot_def(BUS_MUSIC, 0.5); o.gain = 0.22; o.pan = -0.06; o.dly = 0.20; o.dark = 0.05; shot("gt-stroke-bm", t, o); }
            else { static const int B31[4] = {47, 54, 59, 62}; guitarChord(t + 0.035, B31, 4, 2.35, 0.105, -0.06, 0); }
            static const struct { double o; int m; double p, g; } AF[3] = {
                {0.34, 78, -0.58, 0.12}, {0.72, 81, 0.58, 0.09}, {1.18, 86, -0.42, 0.06}};
            for (int q = 0; q < 3; q++)
                beepx(t + AF[q].o, midihz(AF[q].m), midihz(AF[q].m - 5), 0.20,
                      AF[q].g, AF[q].p, 0.99, 0.96, BUS_SIG, 0.022, 0.52);
        }

        // ══ ACT I · CARRIER ═════════════════════════════════════════════
        if (inS(bar, S_CARRIER)) {
            if (bar % 2 == 0) {
                const char digit = CULT_DIAL[(bar / 2) % 4];
                click(at(bar, 0.94), 0.34, -0.5, 0.8, 0.20);
                dtmf(at(bar, 1), digit, 0.16, 0.62, (bar / 2) % 2 ? 0.45 : -0.45, 0.85, 0.34);
            }
            if (bar % 4 == 3) tap(at(bar, 3.5), 0.30, 0.3, 0.7, 0.3);
        }

        // ══ ACT II · THE MACHINE ALONE — AND A DOT DRIFTING THROUGH ═════
        if (bar == 8 && has("phone-pickup-a")) {
            Shot o = SHOT_SUNG(); o.gain = 0.15; o.pan = -0.12; o.side = 0.5; o.dark = 0.35;
            o.dur = 1.4; o.atk = 0.05; o.wig = 6; o.wigHz = 0.8; o.wigIn = 0.4;
            shot("phone-pickup-a", at(8, 0) + 0.02, o);
        }
        if (bar == 12) phoneTune(bar, 0.20);   // the phone hums the song first
        if (introBar(bar) && bar % 2 == 0) {
            const int k = (bar - 8) / 2;
            dotDrift(at(bar, 2.0) + jit(30), k, 0.09 + 0.007 * k, k % 2 ? 0.40 : -0.40,
                     BAR * 0.85, 4.5, 0.52, 0.62, 0, 1);
        }
        if (bar == 13)
            dotDriftVox(at(bar, 2.5) + jit(30), bar, 0.085, 0.28, BAR * 0.8, 4.0, 0.5, 0.55, -12);
        if (introBar(bar)) {
            if (bar % 2 == 0) {
                const char digit = CULT_DIAL[(bar / 2) % 4];
                click(at(bar, 0.94), 0.30, -0.5, 0.8, 0.18);
                dtmf(at(bar, 1), digit, 0.16, 0.54, (bar / 2) % 2 ? 0.45 : -0.45, 0.85, 0.30);
            }
            for (int s = 0; s < 4; s++)
                if ((s + bar) % 2 == 0) {
                    const double tt = t + (s + 0.25) * BEAT + jit(6);
                    click(tt, 0.70 * vel(0.4), (s & 1) ? 0.4 : -0.4, 0.7, 0);
                }
            if (bar % 4 == 2) bop(at(bar, 3.5), midihz(blipMidi(bar, 0)), 0.30, 0.35, 0.7, 0.085, 0.35);
            if (bar % 2 == 0)   // (the Node call passes a pan the voice never reads)
                frictionPath(at(bar, 0.5), BAR * 0.80, PATH_SPIRAL, 0.42, 0.26, 0, 0.50, 0, 0.10, SHAPE_SLIDE, 1);
            if (bar == 13)
                friction(at(13, 2.2), 1.7, SHAPE_DRAG, 0.62, 0, 0.20, 0, 759, 2300, 103, 214, 0.62, 0, 0.10);
        }
        if (sosBar(bar)) {
            const int k = bar - 16;
            if (bar % 2 == 0)
                dotDrift(at(bar, 1.5) + jit(30), 4 + k / 2, 0.12, (k / 2) % 2 ? -0.42 : 0.42,
                         BAR * 0.9, 4.2, 0.48, 0.58, 0, 1);
            if (bar == 19)
                dotDriftVox(at(bar, 2.5) + jit(30), bar, 0.09, -0.26, BAR * 0.8, 4.0, 0.5, 0.55, -12);
            if (bar == 23)
                dotDrift(at(bar, 2.0) + jit(30), 8, 0.13, 0.0, BAR * 0.9, 4.2, 0.5, 0.55, 0, 1);
            if (bar == 20 || bar == 22) beepSOS(at(bar, 1), 0.50, bar == 20 ? 0.55 : -0.55, 0.40);
            static const double ss[2] = {1.5, 3.5};
            for (int s = 0; s < 2; s++) {
                const double tt = t + ss[s] * BEAT + jit(6);
                tap(tt, 0.68 * vel(0.3), ss[s] > 2 ? 0.42 : -0.42, 0.75, 0.30);
            }
            if (bar % 8 == 7) { click(at(bar, 3.0), 0.58, 0, 0.6, 0); click(at(bar, 3.25), 0.46, 0.3, 0.6, 0); }
            if (bar % 4 == 2)
                friction(at(bar, 1.0), 0.62, SHAPE_SKID, 0.34, -0.30, 0.24, 0, 1900, 1000, 239, 149, 0.58, 0, 0.10);
        }

        // ══ ACT III · THE MESSAGE ═══════════════════════════════════════
        if (inS(bar, S_MESSAGE)) {
            const int k = bar - SB[S_MESSAGE][0];
            if (k == 2) phoneTune(bar, 0.26);
            if (k == 0) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 0, 1.0, 0, 0, 0, 0}; chorus_fn(bar, c); }
            if (k == 9) hook_fn(bar, 1);
            if (k % 8 == 5) hook_fn(bar, 0);
        }
        if (inS(bar, S_MESSAGE)) {
            { const double tt = at(bar, 3.5) + jit(6); tap(tt, 0.62 * vel(0.3), 0.40, 0.7, 0.26); }
            if (four == 3) {
                dotArp(at(bar, 2.0) + jit(8), 5, (bar >> 2) % 2 == 0, 0.16, 0.30, 0.30, 0.34);
                dotTail(at(bar, 2.0) + 5 * 0.16 + 0.18, 7, 0.22, 0.34, 0.36);
            }
            if (four == 3) {
                click(at(bar, 1.0), 0.50, -0.42, 0.7, 0.3);
                bop(at(bar, 1.5), midihz(blipMidi(bar, 1)), 0.26, 0.42, 0.75, 0.085, 0.4);
                friction(at(bar, 2.4), 1.6, SHAPE_DRAG, 0.46, 0.12, 0.22, 0, 819, 2200, 109, 203, 0.58, 0, 0.10);
            }
            if (four == 1)
                frictionPath(at(bar, 2.0), 0.50, PATH_EDGE, 0.30, 0.24, 0, 0.55, 0, 0.10, SHAPE_SKID, 1);
        }

        // ══ ACT IV · THE SECRET ═════════════════════════════════════════
        if (bar == SB[S_SECRET][0]) {
            click(at(bar, 0) - 0.06, 0.40, 0, 0.35, 0);
            if (has("phone-pickup-b")) {
                Shot o = SHOT_SUNG(); o.gain = 0.17; o.pan = 0.10; o.side = 0.45; o.dark = 0.40;
                o.dur = 1.2; o.atk = 0.04; o.wig = 6; o.wigHz = 0.7; o.wigIn = 0.4;
                shot("phone-pickup-b", at(bar, 0) - 0.10, o);
            }
        }
        if (inS(bar, S_SECRET) && (bar - SB[S_SECRET][0]) % 2 == 0) {
            static const double SG[4] = {0.46, 0.52, 0.56, 0.50};
            const int phrase = (bar - SB[S_SECRET][0]) / 2;
            secretCamille(bar, 0.15, SG[phrase], phrase * 0.7);
        }
        if (bar == SB[S_SECRET][0] + 3)
            frictionPath(at(bar, 2.2), BAR * 0.72, PATH_SPIRALIN, 0.28, 0.24, 0.16, 0.42, 0, 0.10, SHAPE_SLIDE, 0.8);
        if (bar == SB[S_SECRET][0] || bar == SB[S_SECRET][0] + 4) {
            int gch[3]; triad_of(degAt(bar), 59, gch);
            const int phrase = (bar - SB[S_SECRET][0]) / 4;
            guitar(at(bar, 0.65) + jit(6), gch[phrase ? 1 : 0] - 24,
                   BAR * 2.6, phrase ? 0.14 : 0.16, phrase ? 0.20 : -0.14, 0.9974);
        }

        // ══ ACT V · THE REPLY ═══════════════════════════════════════════
        if (inS(bar, S_REPLY) && !soloBar(bar)) {
            const int k = bar - SB[S_REPLY][0];
            if (k == 0) { Chorus c = {LEAD_LO, 0, ANS_SOS, 0, 0, 0.96, 0, 0, 0, 1}; chorus_fn(bar, c); }
            if (k == 8) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 0, 1.0, 0, 0, 1, 0}; chorus_fn(bar, c); }
            if (k % 8 == 4) hook_fn(bar, k == 4);
        }
        if (inS(bar, S_REPLY) && !soloBar(bar) && four == 3) {
            const char *nms[2] = {"dot-d4", "dot-a3"};
            const double ps[2] = {0.34, -0.34};
            for (int k = 0; k < 2; k++) {
                Shot o = SHOT_SUNG(); o.gain = 0.34; o.pan = ps[k]; o.side = 0.8; o.dly = 0.5;
                shot(nms[k], t + (2 + k) * BEAT + jit(4), o);
            }
        }
        if (bar == SB[S_REPLY][0] + 12) phoneTune(bar, 0.24);
        if (inS(bar, S_REPLY) && !soloBar(bar) && eight == 7) {
            beepSOS(at(bar, 1), 0.52, 0.55, 0.45);
            dotArp(at(bar, 2.5) + jit(8), 7, 0, 0.18, 0.26, -0.25, 0.34);
        }
        if (inS(bar, S_REPLY) && !soloBar(bar)) {
            static const double ss[2] = {0.75, 2.75};
            for (int s = 0; s < 2; s++) {
                const double tt = t + ss[s] * BEAT + sw16(bar) + jit(6);
                tap(tt, 0.38 * vel(0.3), ss[s] > 2 ? -0.4 : 0.4, 0.7, 0.24);
            }
            if (four == 1) click(at(bar, 3.75), 0.44, 0.35, 0.7, 0.35);
            if (four == 3)
                friction(at(bar, 2.3), 1.7, SHAPE_DRAG, 0.44, -0.14, 0.22, 0, 859, 2400, 114, 223, 0.60, 0, 0.10);
            if (eight == 7)
                frictionPath(at(bar, 2.6), 0.75, PATH_SPIRALIN, 0.42, 0.30, 0, 0.66, 0, 0.10, SHAPE_SKID, 1);
        }
        if (inS(bar, S_REPLY) && !soloBar(bar) && bar % 4 == 2) material(bar, "dash-camille-fs4-hold", 0.62, 16, 0.055, 0.18, 0.85, 0.22, 0.9);
        // the two-bar discovery window
        if (bar == 52) { powerSaw(t, 54, 2.0, 0.11, -0.2); powerSaw(t, 42, 2.0, 0.07, 0.2); }   // the 0:45 saws
        if (bar == 53) { powerSaw(t, 52, 2.6, 0.11, 0.2); powerSaw(t, 40, 2.6, 0.07, -0.2); }
        if (bar == 54) raga(bar, 0.60, 0.40);
        if (bar == 55) dotArp(at(bar, 0.35), 5, 0, 0.22, 0.18, -0.28, 0.48);
        if (bar == SB[S_REPLY][0] + 10) raga(bar, 0.15, 0.40);
        if (bar == SB[S_REPLY][0] + 14) raga(bar, 1.5, 0.44);
        if (bar == SB[S_SPREAD][0] + 1) raga(bar, 0.5, 0.32);

        // ══ ACT VI · IT SPREADS ═════════════════════════════════════════
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar) && !dotFieldBar(bar)) {
            const int k = bar - SB[S_SPREAD][0];
            if (k == 0) { Chorus c = {LEAD_HI, 0, ANS_NONE, 0, 0, 0.62, 1, 1, 1, 0}; chorus_fn(bar, c); }
            if (k == 8) { Chorus c = {LEAD_LO, 0, ANS_NONE, 0, 0, 0.60, 1, 1, 0, 0}; chorus_fn(bar, c); }
        }
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar) && !releaseSpreadBar(bar) && bar % 2 == 0) {
            const char *nm = ((bar / 2) % 2) ? "d4" : "fs4";
            const double w = wigDepth(bar);
            char nb[64];
            { Shot o = SHOT_HELD(); o.gain = 0.40; o.pan = -0.45; o.side = 0.85; o.dly = 0.30;
              o.wig = w; o.wigHz = 4.3; o.wigPhase = 0.4; o.wigDrift = +0.5 * w; o.wigIn = 0.5;
              snprintf(nb, sizeof nb, "dash-camille-%s-hold", nm); shot(nb, at(bar, 1), o); }
            { Shot o = SHOT_HELD(); o.gain = 0.37; o.pan = 0.45; o.side = 0.85; o.dly = 0.30;
              o.wig = w * 1.2; o.wigHz = 5.9; o.wigPhase = 2.6; o.wigDrift = -0.6 * w; o.wigIn = 0.45;
              snprintf(nb, sizeof nb, "dash-alex-%s-hold", nm); shot(nb, at(bar, 1.1), o); }
            const char *low = deg == 2 ? "a2" : deg == 3 ? "e2" : deg == 5 ? "g2" : deg == 6 ? "a2" : "b2";
            { Shot o = SHOT_SUNG(); o.gain = 0.26; o.pan = 0; o.side = 0.25; o.dark = 0.42; o.bright = 0.7; o.edge = 0.3;
              snprintf(nb, sizeof nb, "bassdash-%s", low); shot(nb, at(bar, 0), o); }
        }
        // controlled divergence for the eight bars that survive the release
        if (releaseSpreadBar(bar)) {
            const int k = releaseSpreadIndex(bar);
            const char *nm = k < 4 ? "d4" : k < 6 ? "fs4" : "b3";
            char nb[64];
            if (k == 0) raga(bar, 0.50, 0.34);   // Camille opens the bridge on the LONG take
            if (k == 4) raga(bar, 1.60, 0.24);
            if (k == 2) { Shot o = SHOT_HELD(); o.gain = 0.24; o.pan = -0.30; o.side = 0.76; o.dly = 0.34; o.wig = 5; o.wigHz = 4.3; o.wigIn = 0.65;
              snprintf(nb, sizeof nb, "dash-camille-%s-hold", nm); shot(nb, at(bar, 0.65), o); }
            if (k == 4) { Shot o = SHOT_HELD(); o.gain = 0.21; o.pan = 0.30; o.side = 0.78; o.dly = 0.38; o.wig = 6; o.wigHz = 5.9; o.wigIn = 0.70;
              snprintf(nb, sizeof nb, "dash-alex-%s-hold", nm); shot(nb, at(bar, 0.75), o); }
            if (k == 6) {
                { Shot o = SHOT_HELD(); o.gain = 0.23; o.pan = -0.28; o.side = 0.78; o.dly = 0.34; o.wig = 6; o.wigHz = 4.3; o.wigIn = 0.60;
                  snprintf(nb, sizeof nb, "dash-camille-%s-hold", nm); shot(nb, at(bar, 0.55), o); }
                { Shot o = SHOT_HELD(); o.gain = 0.19; o.pan = 0.28; o.side = 0.80; o.dly = 0.40; o.wig = 7; o.wigHz = 5.9; o.wigIn = 0.65;
                  snprintf(nb, sizeof nb, "dash-alex-%s-hold", nm); shot(nb, at(bar, 0.82), o); }
            }
        }
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar)) {
            static const char digits[8] = {'2', '8', '5', '8', '*', '8', '5', '2'};
            const int bridge = releaseSpreadBar(bar);
            const int bk = bridge ? releaseSpreadIndex(bar) : 0;
            static const int CNT[8] = {1, 1, 1, 2, 1, 2, 2, 3};
            const int count = bridge ? CNT[bk] : 3;
            for (int k = 0; k < count; k++) {
                const double u = t + ((double)k * 4 / (count > 1 ? count : 1)) * BEAT + jit(8);
                dtmf(u, digits[(bar * 3 + k) % 8], bridge ? 0.075 : 0.055,
                     (bridge ? 0.25 : 0.46) * vel(0.3), count == 1 ? 0 : (k & 1) ? 0.42 : -0.42, 0.85, 0.40);
            }
            if (!bridge && four == 2) bop(at(bar, 3.25), midihz(blipMidi(bar, 2)), 0.26, -0.4, 0.8, 0.085, 0.45);
            if (!bridge && four == 1) dotArp(at(bar, 1.5) + jit(8), 6, bar % 8 < 4, 0.15, 0.24, 0.35, 0.42);
            if (bar == SB[S_SPREAD][0] + 4 && has("phone-rotary-a")) {
                Shot o = SHOT_SUNG(); o.gain = 0.14; o.pan = -0.55; o.side = 0.85; o.dark = 0.45;
                o.dly = 0.45; o.atk = 0.08; o.wig = 9; o.wigHz = 0.35; o.wigIn = 0.6;
                shot("phone-rotary-a", at(bar, 0.5), o);
            }
            if (bar == SB[S_SPREAD][0] + 9 && has("phone-rotary-b")) {
                Shot o = SHOT_SUNG(); o.gain = 0.13; o.pan = 0.55; o.side = 0.85; o.dark = 0.45;
                o.dly = 0.45; o.atk = 0.08; o.wig = 9; o.wigHz = 0.30; o.wigPhase = 2.1; o.wigIn = 0.6;
                shot("phone-rotary-b", at(bar, 1.0), o);
            }
            if (!bridge || bk == 3 || bk == 7)
                friction(at(bar, 2.5 + (bar % 3) * 0.25), 0.58, SHAPE_SKID,
                         (bridge ? 0.24 : 0.34) * vel(0.3), bar % 2 ? 0.46 : -0.46, 0.30, 0,
                         2200, 1050, 280 - (bar % 4) * 30, 149, 0.62, 0, 0.10);
            if (!bridge && four == 0)
                friction(at(bar, 3.1), 0.95, SHAPE_DRAG, 0.30, bar % 4 ? -0.2 : 0.2,
                         0.24, 0, 900, 1900, 119, 189, 0.55, 1, 0.10);
        }
        // (the extended-cult block at spread+2 is gated on !releaseSpreadBar,
        // which bar 66 is — kept for fidelity, it never fires)
        if (bar == SB[S_SPREAD][0] + 2 && !releaseSpreadBar(bar)) {
            const char *picks[3]; const int pc = choirFor(degAt(bar), picks);
            const char *hi = picks[pc - 1], *mid = picks[pc - 2 > 0 ? pc - 2 : 0];
            char hbuf[64], mbuf[64], lbuf[64];
            snprintf(hbuf, sizeof hbuf, "cultlong-%s", hi);
            if (has(hbuf)) {
                { Shot o = SHOT_SUNG(); o.gain = 0.36; o.pan = 0.30; o.side = 0.92; o.dark = 0.05;
                  o.dly = 0.55; o.atk = 0.35; o.wig = 7; o.wigHz = 0.4; o.wigPhase = 0.6;
                  o.wigDrift = 4; o.wigIn = 1.2;
                  shot(hbuf, at(bar, 1) + jit(10), o); }
                snprintf(mbuf, sizeof mbuf, "cultlong-%s", mid);
                { Shot o = SHOT_SUNG(); o.gain = 0.23; o.pan = -0.34; o.side = 0.92; o.dark = 0.12;
                  o.dly = 0.55; o.atk = 0.45; o.wig = 8; o.wigHz = 0.31; o.wigPhase = 2.8;
                  o.wigDrift = -5; o.wigIn = 1.0;
                  shot(mbuf, at(bar, 1.5) + jit(10), o); }
                const char *low = picks[0];
                snprintf(lbuf, sizeof lbuf, "cultlong-%s", low);
                if (low && low != mid && has(lbuf)) {
                    Shot o = SHOT_SUNG(); o.gain = 0.17; o.pan = 0.16; o.side = 0.88; o.dark = 0.20;
                    o.dly = 0.60; o.atk = 0.55; o.wig = 6; o.wigHz = 0.26; o.wigPhase = 4.4;
                    o.wigDrift = 3.5; o.wigIn = 1.3;
                    shot(lbuf, at(bar, 2.0) + jit(10), o);
                }
                { Shot o = SHOT_SUNG(); o.gain = 0.105; o.pan = -0.20; o.semis = 12; o.side = 0.96; o.dark = 0;
                  o.bright = 0.35; o.dly = 0.66; o.atk = 0.70; o.wig = 5; o.wigHz = 0.22; o.wigPhase = 1.4;
                  o.wigDrift = -3; o.wigIn = 1.5;
                  shot(hbuf, at(bar, 2.5) + jit(10), o); }
            } else {
                snprintf(hbuf, sizeof hbuf, "cult-%s", hi);
                stretched(hbuf, at(bar, 1) + jit(10), 0.34, 0.30, 0, 2.3, 8.0, 0.92, 0.05, BUS_VOX, 0.55);
                snprintf(mbuf, sizeof mbuf, "cult-%s", mid);
                stretched(mbuf, at(bar, 1.5) + jit(10), 0.22, -0.34, 0, 2.6, 8.0, 0.92, 0.12, BUS_VOX, 0.55);
            }
        }
        // the word itself spreading — on the staircase
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar)) {
            const char *nm = ALT_CULTS[(bar - SB[S_SPREAD][0]) % 6];
            const int bridge = releaseSpreadBar(bar);
            const int bk = bridge ? releaseSpreadIndex(bar) : 0;
            if (has(nm) && (!bridge || bk == 1 || bk == 3 || bk == 5 || bk == 7))
                staircasePan(nm, at(bar, 2.5 + ((bar % 3) * 0.25)),
                             bridge ? 0.24 : 0.32 + 0.02 * ((bar - SB[S_SPREAD][0]) % 4),
                             3, bridge ? 1.15 : 1.6, bar % 2 ? 1 : -1, bridge ? -7 : 0, 0.16, BUS_VOX, 0.48, 0);
            if (has(nm) && bridge && (bk == 1 || bk == 3 || bk == 5 || bk == 7))   // the octave-down shadow
                staircasePan(nm, at(bar, 2.5 + ((bar % 3) * 0.25)) + 0.06,
                             0.15, 3, 0.8, bar % 2 ? -1 : 1, -12, 0.34, BUS_VOX, 0.62, 0);
            if (has(nm) && bridge && (bk == 1 || bk == 3 || bk == 5 || bk == 7))   // CULT … TLUC
                staircasePanX(nm, at(bar, 2.5 + ((bar % 3) * 0.25)) + (double)bank_get(nm)->n / SR * pow(2, 7 / 12.0) * 0.92,
                              0.20, 3, 1.15, bar % 2 ? -1 : 1, -7, 0.22, BUS_VOX, 0.55, 0, 1);
        }
        if (inS(bar, S_SPREAD) && !sparseSpreadBar(bar) && !releaseSpreadBar(bar) && bar % 2 == 1)
            material(bar, "cult-d4", 0.72, 16, 0.085, 0.18, 0.85, 0.30, 0.9);
        // the dot field: four bars of dots over a band that keeps going
        if (bar == 72) dotDriftVox(at(bar, 1.05), bar, 0.15, 0.22, 2.6, 4.0, 0.50, 0.24, 0);
        if (bar == 73) dotDrift(at(bar, 1.20), 7, 0.13, -0.22, 2.25, 3.8, 0.52, 0.30, 0, 0);
        if (bar == 74) dotDriftVox(at(bar, 1.00), bar, 0.16, -0.18, 2.35, 3.9, 0.48, 0.20, 0);
        if (bar == 75) dotArp(at(bar, 1.15), 5, 1, 0.22, 0.14, 0.18, 0.46);
        if (bar == 74) { bubbles(at(bar, 2.0), 3.4, 34, 0.20); bubbles(at(bar, 3.4), 2.2, 18, 0.14); }

        // ══ ACT VII · THE WHOLE MESSAGE ═════════════════════════════════
        if (bar == SB[S_WHOLE][0] - 1) dotArp(at(bar, 2.0), 7, 1, 0.20, 0.40, 0.15, 0.28);
        if (bar == SB[S_WHOLE][0]) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 1, 1.0, 0, 0, 0, 0}; chorus_fn(bar, c); }
        if (bar == SB[S_WHOLE][0] + 8) { Chorus c = {LEAD_BOTH, 1, ANS_NONE, 1, 1, 1.0, 0, 0, 0, 0}; chorus_fn(bar, c); }
        if (bar == SB[S_WHOLE][0] + 16) hook_fn(bar, 1);
        if (inS(bar, S_WHOLE)) {
            static const double ss[3] = {0.75, 1.75, 3.25};
            for (int s = 0; s < 3; s++) {
                const double tt = t + ss[s] * BEAT + sw16(bar) + jit(6);
                tap(tt, 0.52 * vel(0.4), ss[s] > 2 ? -0.42 : 0.42, 0.7, 0.22);
            }
            if (eight == 7) beepSOS(at(bar, 1), 0.34, -0.55, 0.5);
            if (eight == 7)
                frictionPath(at(bar, 2.2), 1.8, PATH_SCRUB, 0.52, 0.20, 0, 0.64, 0, 0.10, SHAPE_DRAG, 1);
            if ((bar - SB[S_WHOLE][0]) % 8 == 2)
                friction(at(bar, 1.3), 0.66, SHAPE_SKID, 0.36, 0.40, 0.28, 0, 2300, 1000, 289, 143, 0.60, 0, 0.10);
        }
        if (bar == SB[S_WHOLE][0] + 1) cultCluster(bar, 1.0, 1.0);    // 1:25: the heap
        if (bar == SB[S_WHOLE][0] + 5) cultCluster(bar, 2.5, 0.62);
        if (inS(bar, S_WHOLE) && bar % 4 == 3) material(bar, "cult-fs4", 0.66, 16, 0.060, 0.18, 0.9, 0.22, 0.9);

        // ══ ACT VIII · RECOGNITION ══════════════════════════════════════
        if (bar == SB[S_RECOGNISE][0]) { Chorus c = {LEAD_HI, 0, ANS_NONE, 0, 0, 0.62, 1, 0, 0, 0}; chorus_fn(bar, c); }
        if (bar == SB[S_RECOGNISE][0] + 4) hook_fn(bar, 0);
        if (bar == SB[S_RECOGNISE][0] + 6) phoneTune(bar, 0.22);
        if (inS(bar, S_RECOGNISE) && bar % 2 == 0) material(bar, "cult-b3", 0.50, 16, 0.10, 0.18, 0.85, 0.34, 0.9);
        if (inS(bar, S_RECOGNISE) && four == 3)
            friction(at(bar, 2.6), 1.4, SHAPE_DRAG, 0.40 * (1 - (double)(bar - SB[S_RECOGNISE][0]) / 10),
                     0.10, 0.22, 0, 819, 1900, 109, 174, 0.52, 0, 0.10);
        if (inS(bar, S_RECOGNISE) && four == 1)
            click(at(bar, 2.5), 0.44 * (1 - (double)(bar - SB[S_RECOGNISE][0]) / 9), -0.4, 0.7, 0.35);

        // ══ ACT IX · CARRIER OFF ════════════════════════════════════════
        if (bar == SB[S_CARRIEROFF][0] - 2)
            frictionPath(at(bar, 1.0), BAR * 3.2, PATH_EDGEBACK, 0.34, 0.22, 0, 0.42, 0, 0.30, SHAPE_SLIDE, 1);
        if (inS(bar, S_CARRIEROFF)) {
            const int k = bar - SB[S_CARRIEROFF][0];
            if (k % 2 == 0 && k <= 6)
                dtmf(at(bar, 1), CULT_DIAL[(k / 2) % 4], 0.14,
                     0.44 * (1 - (double)k / 10), k % 4 ? 0.5 : -0.5, 0.85, 0.42);
            if (k == 2) phoneTune(bar, 0.16);
            if (k == 6) bop(at(bar, 3), midihz(59), 0.24, 0, 0.6, 0.085, 0.5);
            if (k == 4)
                dotDrift(at(bar, 2) + jit(30), 8, 0.16, 0, BAR * 0.9, 4.5, 0.55, 0.45, 0, 0);
        }

        if (bar == SB[S_CARRIEROFF][0]) {   // the finale: the field out, then the POP
            dotField(t, 9.0, 0.30, 0);           // bars 104 → 108.5: the pop lands at 1:49
            firework(at(108.5, 0), 220, 0.32);
            kickp(at(108.5, 0), 0.30, 0.3, 2.4);
            kickp(at(108.5, 0) + 0.055, 0.22, 0.3, 2.9);
            kickp(at(108.5, 0) + 0.125, 0.16, 0.3, 3.4);
        }
        // ---- the drone walking out: the last chant stops after the bop --
        if (inS(bar, S_CARRIEROFF) && bar % 2 == 0)
            choir(bar, 0.48 * (1 - (double)(bar - SB[S_CARRIEROFF][0]) / 11),
                  bar == SB[S_CARRIEROFF][0] + 6 ? 3.0 + 0.75 * BEAT : 0);

        // one very soft noise wash at the act boundaries that need one
        if (bar == SB[S_THREE][0] || bar == SB[S_MESSAGE][0] || bar == SB[S_REPLY][0]
            || bar == SB[S_WHOLE][0] || bar == SB[S_CARRIEROFF][0]) {
            Shot o = shot_def(BUS_MUSIC, 0.9);
            o.gain = 0.055; o.pan = 0; o.dur = 2.2; o.dark = 0.35;
            shot("sweep", t - 0.9, o);
        }
    }

    // The last sound on the record: the hang-up, after the music has gone.
    if (has("phone-hangup-a")) {
        Shot o = SHOT_SUNG(); o.gain = 0.26; o.pan = 0; o.side = 0.35; o.dark = 0.20;
        o.atk = 0.012; o.wig = 5; o.wigHz = 0.9; o.wigIn = 0.3;
        shot("phone-hangup-a", at(BARS, 0) + 0.50, o);
    }
    click(at(BARS, 0) + 0.55, 0.34, 0, 0.30, 0);
    click(at(BARS, 0) + 0.62, 0.20, 0.2, 0.30, 0);
    if (has("phone-busy-us")) {
        Shot o = SHOT_SUNG(); o.gain = 0.06; o.pan = 0.30; o.side = 0.85; o.dark = 0.55;
        o.dly = 0.55; o.atk = 0.15; o.wig = 10; o.wigHz = 0.25; o.wigIn = 0.8;
        shot("phone-busy-us", at(BARS, 0) + 1.35, o);
    }
}

// ── the elastic field ──────────────────────────────────────────────────
// Before either duck, the five buses become bodies in one elastic field:
// an impulse supplies velocity, spring motion the crossings and overshoot,
// damping settles it, distance becomes propagation delay/level, and the
// first excursion fractures a few milliseconds of the bus into a shard.
typedef struct { double bar; double duration, strength, hz, damping, glitch, attack, release, dispersion, offset, keepTime, t; } Explosion;
static Explosion EXPLOSIONS[8] = {
    {29,  1.35, 0.58, 2.45, 1.85, 0.34, 0.035, 0.30, 0,    0,    1,    0},
    {40,  6.20, 0.74, 0.46, 0.34, 0.10, 0.20,  1.00, 0,    0,    0.45, 0},
    {48,  3.60, 0.96, 1.18, 0.72, 0.22, 0.09,  0.72, 0,    0,    1,    0},
    {64,  7.60, 0.60, 0.54, 0.72, 0.12, 0.40,  1.40, 0.10, 0.12, 0.15, 0},
    {76,  4.80, 1.42, 0.92, 0.58, 0.34, 0.030, 0.62, 0.24, 0,    1,    0},
    {92,  2.20, 1.14, 2.10, 1.18, 0.44, 0.030, 0.42, 0,    0,    1,    0},
    {104, 4.60, 0.42, 0.56, 0.55, 0.06, 0.22,  1.10, 0,    0,    1,    0},
    {108.5, 3.20, 1.55, 1.60, 0.90, 0.50, 0.020, 0.50, 0.30, 0,    1,    0}};
typedef struct { const char *name; float *L, *R; double dir, mass, shard; } Body;
static inline double readLinear(const float *a, long len, double p) {
    if (p <= 0) return len ? a[0] : 0;
    const long q = (long)p;
    if (q + 1 >= len) return len ? a[len - 1] : 0;
    const double f = p - q;
    return a[q] + (a[q + 1] - a[q]) * f;
}
static void elasticizeBus(Body *body, int bodyIndex) {
    for (int e = 0; e < 8; e++) {
        const Explosion *ex = &EXPLOSIONS[e];
        long start = jsround((ex->t - 0.08) * SR); if (start < 0) start = 0;
        long end = jsround((ex->t + ex->duration + 0.10) * SR); if (end > N) end = N;
        const long len = end - start;
        float *srcL = malloc(len * 4), *srcR = malloc(len * 4);
        memcpy(srcL, body->L + start, len * 4); memcpy(srcR, body->R + start, len * 4);
        long anchor = jsround((ex->t + 0.035 + bodyIndex * 0.009) * SR) - start; if (anchor < 0) anchor = 0;
        long grain = jsround(body->shard * SR); if (grain < 16) grain = 16;
        long from = jsround(ex->t * SR); if (from < start) from = start;
        for (long i = from; i < end; i++) {
            const double age = (double)i / SR - ex->t;
            if (age < 0 || age > ex->duration) continue;
            const double edge = smoothstep01(clampd(age / ex->attack, 0, 1))
                * smoothstep01(clampd((ex->duration - age) / ex->release, 0, 1));
            const double bodyHz = ex->hz * (1 + ex->dispersion * (bodyIndex - 2) / 2.0);
            const double spring = ex->strength / body->mass * exp(-ex->damping * age)
                * sin(TAU * bodyHz * age) * edge;
            const double travel = fabs(spring);
            const double delay = travel * (0.012 + 0.021 / body->mass) * SR;
            const long local = i - start;
            const double pos = dmax(0, local - delay);
            double ml = readLinear(srcL, len, pos), mr = readLinear(srcR, len, pos);
            const double distance = (1 + 0.11 * travel) / (1 + 0.24 * travel);
            const double pan = clampd(body->dir * spring * 0.86, -0.92, 0.92);
            ml *= sqrt(1 - pan) * distance;
            mr *= sqrt(1 + pan) * distance;
            const double fracture = ex->glitch * exp(-2.7 * age)
                * smoothstep01(clampd(age / 0.035, 0, 1)) * edge;
            if (fracture > 0.012) {
                long gp = anchor + ((local - anchor + grain * 64) % grain);
                if (gp > len - 2) gp = len - 2;
                const double gl = readLinear(srcL, len, gp), gr = readLinear(srcR, len, gp);
                const int swap = bodyIndex & 1;
                ml = ml * (1 - 0.34 * fracture) + (swap ? gr : gl) * 0.34 * fracture;
                mr = mr * (1 - 0.34 * fracture) + (swap ? gl : gr) * 0.34 * fracture;
            }
            const double grounded = !strcmp(body->name, "drums") ? ex->keepTime : 1;
            const double wet = grounded * edge * clampd(0.14 + 0.58 * travel + 0.08 * fracture, 0, 0.90);
            body->L[i] = (float)(srcL[local] * (1 - wet) + ml * wet);
            body->R[i] = (float)(srcR[local] * (1 - wet) + mr * wet);
        }
        free(srcL); free(srcR);
    }
}

// ── mixdown ────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    for (int a = 1; a < argc; a++)
        if (!strcmp(argv[a], "--spatial")) SPATIAL = 1;
        else if (!strcmp(argv[a], "--radio")) RADIO = 1;
    clock_t t0 = clock();
    char self[4096];
    if (!realpath(argv[0], self)) snprintf(self, sizeof self, "%s", argv[0]);
    char tmp[4096]; snprintf(tmp, sizeof tmp, "%s", self);
    snprintf(LANE, sizeof LANE, "%s", dirname(dirname(tmp)));

    N = jsround((BARS * BAR + 3.2) * SR);
    musicL = calloc(N, 4); musicR = calloc(N, 4);
    drumsL = calloc(N, 4); drumsR = calloc(N, 4);
    voxL = calloc(N, 4); voxR = calloc(N, 4);
    tubeL = calloc(N, 4); tubeR = calloc(N, 4);
    sigL = calloc(N, 4); sigR = calloc(N, 4);
    airL = calloc(N, 4); airR = calloc(N, 4);
    sideB = calloc(N, 4); sideV = calloc(N, 4);
    sideT = calloc(N, 4); sideS = calloc(N, 4);
    dlySend = calloc(N, 4);
    if (!dlySend || !airR) { fprintf(stderr, "! out of memory\n"); return 1; }
    for (int e = 0; e < 8; e++) EXPLOSIONS[e].t = at(EXPLOSIONS[e].bar, 0) + EXPLOSIONS[e].offset;

    bank_load_dir("samples");
    bank_load_dir("sung");
    bank_load_dir("alt/samples");
    bank_load_dir("phone");
    static const char *DEMOS[9][2] = {
        {"hatC", "perc-hat-c.mp3"}, {"hatO", "perc-hat-o.mp3"}, {"clap", "perc-clap.mp3"},
        {"ride", "perc-ride.mp3"}, {"snap", "perc-snap.mp3"}, {"snare", "perc-snare.mp3"},
        {"tambo", "perc-tambo.mp3"}, {"block", "perc-block.mp3"}, {"sweep", "bed-noise-sweep.mp3"}};
    for (int i = 0; i < 9; i++) bank_load_demo(DEMOS[i][0], DEMOS[i][1]);
    printf("-> %d samples in the bank\n", bankN);

    printf("-> scoring %d bars @ %.0f BPM . B minor . 9 acts . %.1fs\n", BARS, BPM, BARS * BAR);
    score();
    printf("  %d kicks . %d snares\n", kicksN, snaresN);
    double tScore = (double)(clock() - t0) / CLOCKS_PER_SEC;

    // The tube's DC blocker (the tubular colour itself is OFF in v10).
    {
        const double dcRc = 1 / (TAU * 18), dcA = dcRc / (dcRc + 1.0 / SR);
        double dcL = 0, dcR = 0, pL = 0, pR = 0;
        for (long i = 0; i < N; i++) {
            const double l = tubeL[i], r = tubeR[i];
            dcL = dcA * (dcL + l - pL); pL = l;
            dcR = dcA * (dcR + r - pR); pR = r;
            tubeL[i] = (float)dcL; tubeR[i] = (float)dcR;
        }
    }

    // Dub delay: dotted-eighth ping-pong, damped, high-passed, into the bed.
    {
        const long D = jsround(0.75 * BEAT * SR);   // 0.375 s
        const double FB = 0.42;
        const double damp = 1 - exp((-TAU * 2600) / SR);
        const double hpRc = 1 / (TAU * 180), hpA = hpRc / (hpRc + 1.0 / SR);
        float *bL = calloc(D, 4), *bR = calloc(D, 4);
        double dL = 0, dR = 0, hpL = 0, hpR = 0, pL = 0, pR = 0;
        for (long i = 0; i < N; i++) {
            const long ix = i % D;
            const double tapL = i >= D ? bL[ix] : 0;
            const double tapR = i >= D ? bR[ix] : 0;
            dL += damp * (tapR - dL);
            dR += damp * (tapL - dR);
            const double wl = dlySend[i] + dR * FB;
            const double wr = dL * FB;
            bL[ix] = (float)wl; bR[ix] = (float)wr;
            hpL = hpA * (hpL + wl - pL); pL = wl;
            hpR = hpA * (hpR + wr - pR); pR = wr;
            musicL[i] += (float)(hpL * 0.50);
            musicR[i] += (float)(hpR * 0.50);
        }
        free(bL); free(bR);
    }

    // The elastic field, before either duck.
    {
        Body bodies[5] = {
            {"music", musicL, musicR, -0.86, 1.10, 0.021},
            {"drums", drumsL, drumsR, 0.70, 1.42, 0.017},
            {"vox",   voxL,   voxR,   -0.42, 0.74, 0.026},
            {"tube",  tubeL,  tubeR,  0.96, 0.92, 0.031},
            {"sig",   sigL,   sigR,   -0.98, 0.58, 0.013}};
        for (int k = 0; k < 5; k++) elasticizeBus(&bodies[k], k);
    }

    // The two ducks: bedEnv (kick only, 0.50) and pumpEnv (kick+snare, deep).
    float *bedEnv = malloc(N * 4), *pumpEnv = malloc(N * 4);
    {
        static Trig tr[MAX_TRIG * 2]; int c = 0;
        for (int i = 0; i < kicksN; i++) tr[c++] = (Trig){kicksT[i], 0.50, 0.009, 0.31};
        buildEnv(bedEnv, tr, c);
        c = 0;
        for (int i = 0; i < kicksN; i++) tr[c++] = (Trig){kicksT[i], 0.72, 0.009, 0.26};
        for (int i = 0; i < snaresN; i++) tr[c++] = (Trig){snaresT[i], 0.44, 0.009, 0.20};
        buildEnv(pumpEnv, tr, c);
    }

    float *voxDuck = NULL;
    if (RADIO) {
        voxDuck = malloc(N * 4);
        const double ka = 1 - exp(-1.0 / (0.005 * SR));
        const double kr = 1 - exp(-1.0 / (0.150 * SR));
        double env = 0;
        for (long i = 0; i < N; i++) {
            const double x = fabs(voxL[i]) + fabs(voxR[i]);
            env += (x > env ? ka : kr) * (x - env);
            const double d = dmin(1.0, env * 3.2);
            voxDuck[i] = (float)(1.0 - 0.28 * d);
        }
    }

    // Side return + final sum, one pass. The air rides pumpEnv^1.35 — the
    // deep kick+snare pump, so the wind exhales on every hit.
    float *L = malloc(N * 4), *R = malloc(N * 4);
    double peak = 0;
    {
        const double hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1.0 / SR);
        const double lpK = 1 - exp((-TAU * 11500) / SR);
        double hp = 0, lp = 0, prev = 0, send = 0.9;
        double subPh = 0, subF = 0, subG = 0;
        for (long i = 0; i < N; i++) {
            const double be = bedEnv[i], pe = pumpEnv[i];
            const double vd = RADIO ? voxDuck[i] : 1.0;
            const double vg = RADIO ? 0.82 : 1.0;
            const double dv = pow(be, 0.25), ds = pow(be, 0.5), da = pow(pe, 1.35);
            const double s = sideB[i] * be * vd + sideV[i] * dv + sideT[i] * pe * TUBEG * vd + sideS[i] * ds;
            hp = hpA * (hp + s - prev); prev = s;
            lp += lpK * (hp - lp);
            const double barPos = ((double)i / SR) / BAR;
            const double target =
                barPos < 8 ? 0.92 : barPos < 24 ? 0.70 : barPos < 40 ? 0.56 :
                barPos < 48 ? 0.90 : barPos < 64 ? 0.52 : barPos < 76 ? 0.86 :
                barPos < 96 ? 0.46 : barPos < 104 ? 0.60 : 0.90;
            send += 0.00004 * (target - send);
            const double so = lp * send;
            const double fadeIn = dmin(1, (double)i / (0.014 * SR));
            const double fadeOut = dmin(1, (double)(N - 1 - i) / (2.6 * SR));
            const double fade = dmax(0, dmin(fadeIn, fadeOut));
            double sub = 0;
            if (RADIO) {
                const int sbar = (int)barPos;
                const double ft = midihz(bassRoot(degAt(sbar < BARS ? sbar : BARS - 1)));
                if (subF <= 0) subF = ft;
                subF += 0.00012 * (ft - subF);
                subPh += TAU * subF / SR;
                subG += 0.00008 * (0.050 - subG);
                sub = sin(subPh) * subG * (0.55 + 0.45 * be);
            }
            // the tour (source bars 54.5-63.9): voices to a third, the band one turn round
            const double tb = ((double)i / SR) / BAR;
            const double tour = smoothstep01(clampd((tb - 54.5) / 0.75, 0, 1)) * smoothstep01(clampd((63.9 - tb) / 1.0, 0, 1));
            const double farV = 1 - 0.64 * tour;
            double mL = musicL[i] * be * vd, mR = musicR[i] * be * vd;
            if (tour > 0) {
                const double thT = tour * TAU * smoothstep01(clampd((tb - 54.5) / 9.4, 0, 1));
                const double cs = cos(thT), sn = sin(thT);
                const double rl = mL * cs - mR * sn, rr = mL * sn + mR * cs;
                mL = rl; mR = rr;
            }
            const double l = (mL + voxL[i] * dv * VOXG * vg * farV + tubeL[i] * pe * TUBEG * vd * farV
                + sigL[i] * ds * SIGG + airL[i] * da + drumsL[i] + sub + so * (1 - 0.4 * tour)) * fade;
            const double r = (mR + voxR[i] * dv * VOXG * vg * farV + tubeR[i] * pe * TUBEG * vd * farV
                + sigR[i] * ds * SIGG + airR[i] * da + drumsR[i] + sub - so * (1 - 0.4 * tour)) * fade;
            const double lt = (double)i / SR;
            const double th = 0.10 * sin(TAU * 0.021 * lt) + 0.05 * sin(TAU * 0.009 * lt + 1.3);
            const double br = 1.0 + 0.035 * sin(TAU * 0.016 * lt + 0.5);
            const double lr = (l * cos(th) - r * sin(th)) * br;
            const double rr = (l * sin(th) + r * cos(th)) * br;
            L[i] = (float)lr; R[i] = (float)rr;
            if (fabs(lr) > peak) peak = fabs(lr);
            if (fabs(rr) > peak) peak = fabs(rr);
        }
    }
    const double norm = peak > 1e-9 ? 0.92 / peak : 1;
    for (long i = 0; i < N; i++) { L[i] = (float)(L[i] * norm); R[i] = (float)(R[i] * norm); }
    fprintf(stderr, "# pre-master peak %.6f . linear trim %.3f\n", peak, norm);

    char outdir[4200]; snprintf(outdir, sizeof outdir, "%s/c/out", LANE);
    mkdir(outdir, 0755);
    char outp[4400]; snprintf(outp, sizeof outp, "%s/%s.wav", outdir,
                              SPATIAL ? "whistlecultspatial" : RADIO ? "cult-remix-radio" : "cult-remix-c");
    write_wav_f32_stereo(outp, L, R, N);
    if (missingWarned) fprintf(stderr, "  ! %d missing-sample warnings\n", missingWarned);
    const double tAll = (double)(clock() - t0) / CLOCKS_PER_SEC;
    printf("ok %s  (%.1fs)  score %.2fs . total %.2fs\n", outp, BARS * BAR, tScore, tAll);
    return 0;
}
