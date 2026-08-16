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

// ── the two generators, bit-exact against the Node file ────────────────
static uint32_t seed = 20220120;           // the cult post date
static inline double rnd(void) { seed = seed * 1664525u + 1013904223u; return (double)seed / 4294967296.0; }
static inline double jit(double ms) { return ((rnd() - 0.5) * 2.0 * ms) / 1000.0; }
static inline double vel(double spread) { return 1.0 - rnd() * spread; }

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
static const double VOXG = 1.42, TUBEG = 1.00, SIGG = 2.60;
static float *musicL, *musicR, *drumsL, *drumsR, *voxL, *voxR,
             *tubeL, *tubeR, *sigL, *sigR,
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
static void kick(double t, double gain, double weight) {
    if (kicksN < MAX_TRIG) kicksT[kicksN++] = t;
    const double SATN = tanh(SATN_DRIVE);
    const long n = jsround(0.52 * SR), i0 = jsround(t * SR);
    double ph = 0, sub = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double f = 47 + 153 * exp(-u * 62);
        ph += (TAU * f) / SR;
        sub += (TAU * 44) / SR;
        const double slam = exp(-u * 34), tail = exp(-u * 7.0);
        const double env = (0.62 * slam + 0.52 * tail) * dmin(1, u / 0.0009);
        const double body = tanh(sin(ph) * env * 2.4) / SATN;
        const double low = sin(sub) * exp(-u * 5.6) * 0.40 * weight;
        const double click = exp(-u * 300) * 0.13 * sin(TAU * 1600 * u)
            + exp(-u * 760) * 0.075 * sin(TAU * 3900 * u);
        emit(BUS_DRUMS, i0 + i, (body + low + click) * 0.74 * gain * tailFade(i, n), 0, NOSP, 0, 0);
    }
}

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
static void woodTap(double t, double gain, double pan) {
    const long n = jsround(0.075 * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double lp = 0, b1 = 0, b2 = 0;
    const double a1 = 1 - exp((-TAU * 900) / SR), a2 = 1 - exp((-TAU * 1900) / SR);
    const double aw = 1 - exp((-TAU * 5200) / SR);
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        const double env = exp(-u * 105) * dmin(1, u / 0.0006);
        const double white = nrnd();
        lp += aw * (white - lp);
        b1 += a1 * (lp - b1); b2 += a2 * (lp - b2);
        const double v = b1 * 0.62 + b2 * 0.30 + lp * 0.16;
        emit(BUS_DRUMS, i0 + i, v * env * gain * 1.5 * tailFade(i, n), pan, sp, 0.55, 0);
    }
}

// The snare is a quiet woody tap — mostly the pump's second trigger.
static void snare(double t, double gain) {
    if (snaresN < MAX_TRIG) snaresT[snaresN++] = t;
    woodTap(t, 0.17 * gain, 0.26);
}

// Sine bumps: fundamental + sub octave + a whisper of the 2nd.
#define NO_SLIDE (-999)
static void bass(double t, int midi, double dur, double gain, int slideFrom) {
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

static void beep(double t, double f1, double f2, double dur,
                 double gain, double pan, double side, double dly, int bus) {
    const double rel = 0.012;
    const long n = jsround((dur + rel) * SR), i0 = jsround(t * SR);
    const Sp sp = spatial(pan * 1.2);
    double p1 = 0, p2 = 0;
    for (long i = 0; i < n; i++) {
        const double u = (double)i / SR;
        p1 += (TAU * f1) / SR;
        if (f2) p2 += (TAU * f2) / SR;
        const double atk = 0.5 - 0.5 * cos(M_PI * clampd(u / 0.003, 0, 1));
        const double off = u > dur ? 0.5 + 0.5 * cos(M_PI * clampd((u - dur) / rel, 0, 1)) : 1;
        const double s = (sin(p1) + (f2 ? 0.85 * sin(p2) : 0)) * (f2 ? 0.5 : 0.9);
        emit(bus, i0 + i, s * atk * off * 0.30 * gain * tailFade(i, n), pan, sp, side, dly);
    }
}
static void dtmf(double t, char digit, double dur, double gain, double pan, double side, double dly) {
    int r, c;
    if (!dtmf_key(digit, &r, &c)) return;
    beep(t, DTMF_ROW[r], DTMF_COL[c], dur, gain, pan, side, dly, BUS_SIG);
}

// A "bop": a sine that drops a fifth in 60 ms. UI, not music.
static void bop(double t, double f, double gain, double pan, double side, double dur, double dly) {
    if (SPATIAL) gain *= 0.60;   // room cut: less lyric-layer chatter
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
    noiseHit(t, gain, pan, side, 0.018, 900, 0.40, dly);
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
    double gain, pan, semis, side, dark, dur, dly, off, atk;
    double wig, wigHz, wigPhase, wigDrift, wigIn;
    int bus;
} Shot;
static Shot shot_def(int bus, double side) {
    Shot o; o.gain = 1; o.pan = 0; o.semis = 0; o.side = side; o.dark = 0;
    o.dur = 0; o.dly = 0; o.off = 0; o.atk = 0.0015;
    o.wig = 0; o.wigHz = 5.0; o.wigPhase = 0; o.wigDrift = 0; o.wigIn = 0.45;
    o.bus = bus; return o;
}
#define SHOT_DRUM() shot_def(BUS_DRUMS, 0.35)
#define SHOT_SUNG() shot_def(BUS_VOX, 0.6)
#define SHOT_HELD() shot_def(BUS_TUBE, 0.7)

static void shot(const char *name, double t, Shot o) {
    Sample *smp = bank_get(name);
    if (!smp) { bank_missing(name); return; }
    const float *s = smp->s; const long len = smp->n;
    const double step = pow(2, o.semis / 12.0);
    long start = jsround(o.off * SR);
    if (start < 0) start = 0;
    if (start > len - 2) start = len - 2;
    double availD = floor((double)(len - 2 - start) / step);
    if (o.wig || o.wigDrift) availD = floor(availD * 0.965);
    long n = o.dur > 0 ? (long)dmin(availD, (double)jsround(o.dur * SR)) : (long)availD;
    if (n <= 4) return;
    const long i0 = jsround(t * SR);
    const Sp sp = spatial(o.pan * 1.2);
    const double span = (double)n / SR;
    const double ramp = dmax(1e-4, o.wigIn);
    double lp = 0, pos = start;
    for (long i = 0; i < n; i++) {
        const long q = (long)pos;
        if (q + 1 >= len) break;
        const double f = pos - q;
        double v = s[q] + (s[q + 1] - s[q]) * f;
        if (o.dark > 0) { lp += (1 - o.dark) * (v - lp); v = lp; }
        const double env = smoothstep01(dmin(1, (double)i / (o.atk * SR)));
        emit(o.bus, i0 + i, v * env * o.gain * tailFade(i, n), o.pan, sp, o.side, o.dly);
        if (o.wig || o.wigDrift) {
            const double u = (double)i / SR;
            const double d = smoothstep01(u / ramp);
            const double cents = d * o.wig * sin(TAU * o.wigHz * u + o.wigPhase)
                + o.wigDrift * smoothstep01(u / span);
            pos += step * pow(2, cents / 1200.0);
        } else pos += step;
    }
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
static inline int kickOn(int b) { return !(inS(b, S_CARRIER) || inS(b, S_SECRET) || inS(b, S_CARRIEROFF)); }
static inline int hatOn(int b) { return kickOn(b) || inS(b, S_SECRET); }
static inline int dense(int b) { return inS(b, S_REPLY) || inS(b, S_WHOLE); }
static inline int wordsIn(int bar) {
    // whistlecultspatial: the room version doesn't withhold — "run real
    // fast" sings in the loops from act III on, because hearing the words
    // orbit between the machines IS the piece. The straight render keeps
    // v10.1's withholding-until-bar-76.
    return bar >= SB[SPATIAL ? S_MESSAGE : S_WHOLE][0];
}

static int degAt(int bar) {
    if (inS(bar, S_WHOLE)) return HOME[((bar - SB[S_WHOLE][0]) % 8) / 2];
    return ROWS[(bar / 8) % 4][(bar % 8) / 2];
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

// One video says ONE dot, held long — the WORLD-chain render when built,
// the granular stretch as fallback.
static void dotDrift(double t, int vid, double gain, double pan, double dur,
                     double stretch, double dly, double dark, double semis) {
    if (SPATIAL) gain *= 0.55;   // room cut: the drifting dots recede so
                                 // the worded sentence owns the foreground
    const int gi = ((vid % 9) + 9) % 9;
    const char *takes[3]; int tc = 0;
    for (int i = 0; i < 3; i++)
        if (ALT_DOTS[gi][i] && has(ALT_DOTS[gi][i])) takes[tc++] = ALT_DOTS[gi][i];
    if (!tc) return;
    char lname[64]; snprintf(lname, sizeof lname, "altdot-%s-long", ALT_DOT_IDS[gi]);
    if (has(lname)) {
        Shot o = SHOT_SUNG();
        o.gain = gain * 1.15 * vel(0.12); o.pan = pan; o.semis = semis;
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

// …and the aesthetivoxed kind: Camille and Alex only, held at a chord tone.
static void dotDriftVox(double t, int bar, double gain, double pan, double dur,
                        double stretch, double dly, double dark) {
    if (SPATIAL) gain *= 0.55;   // room cut: same recession as dotDrift
    int tri[3]; triad_of(degAt(bar), 59, tri);
    int pcs[3]; for (int i = 0; i < 3; i++) pcs[i] = ((tri[i] % 12) + 12) % 12;
    const char *lng[6]; int lc = 0;
    for (int i = 0; i < 6; i++) {
        if (!has(VOXDOT_PITCH[i].name)) continue;
        int pc = VOXDOT_PITCH[i].midi % 12;
        int in = 0; for (int j = 0; j < 3; j++) if (pcs[j] == pc) in = 1;
        if (!in) continue;
        if (!strncmp(VOXDOT_PITCH[i].name, "voxdot-j", 8)) continue;
        lng[lc++] = VOXDOT_PITCH[i].name;
    }
    if (lc) {
        const char *nm = lng[abs(bar * 7 + 3) % lc];
        Shot o = SHOT_SUNG();
        o.gain = gain * 1.15 * vel(0.12); o.pan = pan; o.side = 0.8; o.dark = dark; o.dly = dly;
        o.wig = 5; o.wigHz = 0.45; o.wigPhase = bar * 1.3;
        o.wigDrift = (bar % 2 ? -3 : 3); o.wigIn = 0.8;
        shot(nm, t, o);
        return;
    }
    const char *bank[19]; int bc = dotsFor(degAt(bar), bank);
    const char *nm = bank[bc - 1 < (bc >> 1) ? bc - 1 : (bc >> 1)];
    stretched(nm, t, gain * vel(0.12), pan, 0, stretch, dur, 0.8, dark, BUS_VOX, dly);
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
static void dashStack(double t, const char *which, double G, int bar) {
    const double w = wigDepth(bar);
    char nm[64];
    Shot o = SHOT_HELD();
    o.gain = 0.50 * G; o.pan = -0.42; o.side = 0.70; o.dly = 0.10;
    o.wig = w; o.wigHz = 4.3; o.wigPhase = 0.0; o.wigDrift = +0.42 * w; o.wigIn = 0.50;
    snprintf(nm, sizeof nm, "dash-camille-%s-hold", which);
    shot(nm, t + 0.000 + jit(3), o);

    o = SHOT_HELD();
    o.gain = 0.47 * G; o.pan = 0.42; o.side = 0.70; o.dly = 0.10;
    o.wig = w * 1.15; o.wigHz = 5.9; o.wigPhase = 2.1; o.wigDrift = -0.5 * w; o.wigIn = 0.42;
    snprintf(nm, sizeof nm, "dash-alex-%s-hold", which);
    shot(nm, t + 0.028 + jit(3), o);

    Shot j = SHOT_HELD();
    j.gain = 0.58 * G; j.pan = 0.00; j.side = 0.38; j.dark = 0.22; j.dly = 0.08;
    j.wig = w * 0.7; j.wigHz = 3.4; j.wigPhase = 4.3; j.wigDrift = +0.3 * w; j.wigIn = 0.60;
    snprintf(nm, sizeof nm, "dash-jeffrey-%s-hold", which);
    shot(nm, t + 0.056 + jit(3), j);

    Shot js = j;
    js.gain = j.gain * 0.40; js.semis = -12; js.dark = 0.46; js.side = 0.24; js.wig = w * 0.35;
    shot(nm, t + 0.068 + jit(3), js);
}

// The choir: sung "cult" held at three chord tones, 45 ms apart.
static void choir(int bar, double g) {
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
        o.wig = w; o.wigHz = 0.7 + 0.25 * i; o.wigPhase = i * 2.3;
        o.wigDrift = (i % 2 ? -1 : 1) * w * 0.6; o.wigIn = 1.2;
        shot(cultTake(picks[i], buf, sizeof buf), t + i * 0.045, o);
    }
    // v10.1: "downpitched / lower octaves … maybe get chordal" — the slow
    // harmonies grow a floor: the root take an octave below itself.
    { Shot s = SHOT_SUNG(); s.gain = g * 0.55; s.semis = -12; s.dark = 0.52;
      s.side = 0.28; s.pan = 0; s.wig = w * 0.5; s.wigHz = 0.5; s.wigIn = 1.6;
      shot(cultTake(picks[0], buf, sizeof buf), t + 0.09, s); }
}

// Act IV's flanged stacks: each voice gets a twin 7 ms behind itself with a
// slow deep wiggle the original doesn't share — a tape flange, ~4 dB down.
static void secretChoir(int bar, double beat, const double gains[3], double phase) {
    const char *picks[3]; const int pc = choirFor(degAt(bar), picks);
    static const double pans[3] = {0, -0.46, 0.46};
    static const double sides[3] = {0.40, 0.85, 0.85};
    char buf[64];
    for (int i = 0; i < pc; i++) {
        Shot o = SHOT_SUNG();
        o.gain = (i < 3 ? gains[i] : 0.30) * 0.62; o.pan = i < 3 ? pans[i] : 0;
        o.side = i < 3 ? sides[i] : 0.7; o.dark = 0.45; o.dly = 0.42;
        Shot a = o;
        a.wig = 5; a.wigHz = 0.6 + 0.2 * i; a.wigPhase = phase + i * 2.3;
        a.wigDrift = (i % 2 ? -3 : 3); a.wigIn = 1.4;
        shot(cultTake(picks[i], buf, sizeof buf), at(bar, beat) + i * 0.045, a);
        Shot b = o;
        b.gain = o.gain * 0.85;
        b.wig = 11; b.wigHz = 0.33; b.wigPhase = phase + 2.4 + i * 1.7;
        b.wigDrift = (i % 2 ? 9 : -9); b.wigIn = 0.8;
        shot(cultTake(picks[i], buf, sizeof buf), at(bar, beat) + i * 0.045 + 0.007, b);
    }
    // …and the flanged stack's own sub floor.
    { Shot s = SHOT_SUNG(); s.gain = gains[0] * 0.38; s.semis = -12; s.dark = 0.55;
      s.side = 0.28; s.pan = 0; s.dly = 0.42;
      shot(cultTake(picks[0], buf, sizeof buf), at(bar, beat) + 0.09, s); }
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
    for (int i = 0; i < 11; i++) {
        const double tt = at(bar, 0) + K[i] * BEAT + jit(6);   // jit before vel,
        const double gg = g * vel(0.2);                        // like the Node args
        beep(tt, midihz(M[i] + 12), 0, 0.11, gg, i % 2 ? 0.4 : -0.4, 0.85, 0.45, BUS_SIG);
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
    // Room cut: the hook IS the sentence — dash dash · "i wanna run real
    // fast" · dot dot dot — so the whole arc steps forward together and
    // its pans tighten (the sentence walks the room as one voice instead
    // of scattering across it).
    const double F = SPATIAL ? 1.22 : 1.0;
    const double P = SPATIAL ? 0.6 : 1.0;
    dashStack(t + 0.00, "fs4", G * F, bar);
    // v10.1: the pickup is the slowed take when longdots.sh has built it —
    // 1.5x, soft attack, a shade lower and wetter ("sorta rushed" fixed).
    { Shot o = SHOT_SUNG(); o.gain = 0.70 * G * F; o.pan = -0.18 * P; o.side = 0.50; o.dly = 0.24; o.atk = 0.05;
      shot(has("iwannaslow-a") ? "iwannaslow-a" : "iwanna-a-sung", t + 1.42 + jit(4), o); }
    dashStack(t + 2.00, "d4", G * 0.95 * F, bar);
    { Shot o = SHOT_SUNG(); o.gain = 0.70 * G * F; o.pan = 0.18 * P; o.side = 0.50; o.dly = 0.24; o.atk = 0.05;
      shot(has("iwannaslow-b") ? "iwannaslow-b" : "iwanna-b-sung", t + 3.42 + jit(4), o); }
    if (wordsIn(bar)) {
        // v10.1 fourth pass: the syllabic take SAYS the line, dry and
        // center; the melisma enters under it as the vowel it always was.
        { Shot o = SHOT_SUNG(); o.gain = 1.26 * G * F; o.pan = 0.00; o.side = 0.35; o.dly = 0.10;
          sungSub("runrealfast-hi", t + 4.00 + jit(4), o, 0.30); }
        { Shot o = SHOT_SUNG(); o.gain = 0.50 * G * F; o.pan = -0.10 * P; o.side = 0.60; o.dly = 0.35;
          shot("runrealfast-long-hi", t + 4.80 + jit(4), o); }
    } else {
        dotDriftVox(t + 4.00 + jit(20), bar, 0.30 * G, 0.10, 1.9, 4.2, 0.35, 0.30);
        int tri[3]; triad_of(degAt(bar), 59, tri);
        bop(t + 5.50 + jit(4), midihz(tri[0] + 12), 0.20 * G, -0.28, 0.7, 0.085, 0.40);
    }
    { Shot o = SHOT_SUNG(); o.gain = 0.76 * G * F; o.pan = -0.45 * P; o.side = 0.75; o.dly = 0.38;
      shot("dot-b3", t + 6.00 + jit(3), o); }
    { Shot o = SHOT_SUNG(); o.gain = 0.76 * G * F; o.pan = 0.45 * P; o.side = 0.75; o.dly = 0.38;
      shot("dot-fs3", t + 6.50 + jit(3), o); }
    if (full) { Shot o = SHOT_SUNG(); o.gain = 0.34 * G * F; o.pan = 0.0; o.side = 0.60; o.dly = 0.55;
      shot("dot-d4", t + 7.00, o); }
}

enum { LEAD_BOTH, LEAD_HI, LEAD_LO };
enum { ANS_NONE, ANS_DOTS, ANS_SOS };
typedef struct { int lead, fast, answer, tagFast, choirUnder; double g; int drop2, drop3; } Chorus;

static void chorus_fn(int bar, Chorus c) {
    const double t = at(bar, 0), G = c.g;
    const int both = c.lead == LEAD_BOTH, lo = c.lead == LEAD_LO;

    // line 1 · "run real fast" (drop[] at these call sites never drops 1)
    if (!wordsIn(bar)) {
        dotDrift(t + 0.30 + jit(20), bar >> 3, 0.26 * G, -0.30, 1.5, 4.0, 0.40, 0.35, 0);
    } else {
        const double F = SPATIAL ? 1.22 : 1.0;   // room cut: line 1 forward
        if (c.fast) {
            { Shot o = SHOT_SUNG(); o.gain = 1.25 * G * F; o.pan = -0.12; o.side = 0.42; o.dly = 0.12;
              shot("runrealfast-fast-hi", t + 0.00 + jit(3), o); }
            { Shot o = SHOT_SUNG(); o.gain = 0.95 * G * F; o.pan = 0.12; o.side = 0.42; o.dly = 0.12;
              shot("runrealfast-fast-lo", t + 1.00 + jit(3), o); }
        } else if (lo) {
            Shot o = SHOT_SUNG(); o.gain = 1.30 * G * F; o.pan = 0.00; o.side = 0.40; o.dly = 0.12;
            shot("runrealfast-long-lo", t + jit(4), o);
        } else {
            // v10.1 fourth pass ("still can't hear the run real fast
            // words"): the level was never the problem — the long take is
            // a melisma, so every boost boosted a drone. The syllabic take
            // SAYS the line, dry and center; the melisma enters under it a
            // beat later as the vowel it always was.
            { Shot o = SHOT_SUNG(); o.gain = 1.30 * G * F; o.pan = 0.00; o.side = 0.35; o.dly = 0.10;
              sungSub("runrealfast-hi", t + jit(4), o, 0.30); }
            { Shot o = SHOT_SUNG(); o.gain = 0.55 * G * F; o.pan = both ? -0.14 : -0.08; o.side = 0.60; o.dly = 0.30;
              shot("runrealfast-long-hi", t + 0.80 + jit(4), o); }
            if (both) { Shot p = SHOT_SUNG(); p.gain = 0.44 * G * F; p.pan = 0.16; p.side = 0.55; p.dly = 0.28;
              shot("runrealfast-long-lo", t + 0.84 + jit(4), p); }
        }
    }

    // line 2 · "i wanna hide a — waaaay"
    if (!c.drop2 && !wordsIn(bar)) {
        dotDriftVox(t + 2.20 + jit(20), bar, 0.22 * G, 0.24, 1.4, 3.8, 0.40, 0.35);
        int tri[3]; triad_of(degAt(bar), 59, tri);
        bop(t + 3.00 + jit(4), midihz(tri[1] + 12), 0.22 * G, -0.20, 0.7, 0.085, 0.40);
    } else if (!c.drop2) {
        { Shot o = SHOT_SUNG(); o.gain = 0.94 * G; o.pan = -0.10; o.side = 0.5; o.dly = 0.24;
          sungSub("hideaway-hi", t + 2.00 + jit(4), o, 0.30); }
        { Shot o = SHOT_HELD(); o.gain = 0.80 * G; o.pan = -0.06; o.side = 0.62; o.dly = 0.26;
          o.wig = wigDepth(bar) * 0.8; o.wigHz = 4.9; o.wigPhase = 1.1;
          o.wigDrift = 0.3 * wigDepth(bar); o.wigIn = 0.55;
          shot("away-hi", t + 3.45 + jit(3), o); }
        if (both) { Shot o = SHOT_HELD(); o.gain = 0.52 * G; o.pan = 0.22; o.side = 0.62; o.dly = 0.26;
          o.wig = wigDepth(bar) * 0.9; o.wigHz = 6.3; o.wigPhase = 3.4;
          o.wigDrift = -0.35 * wigDepth(bar); o.wigIn = 0.48;
          shot("away-lo", t + 4.20 + jit(3), o); }
    }

    // line 3 · "i wanna dash" — no "i wanna" until after 0:40 (bar 27+),
    // and the slowed take when built
    if (!c.drop3) {
        if (at(bar, 0) >= 54) {
            Shot o = SHOT_SUNG(); o.gain = 0.72 * G; o.pan = 0.18; o.side = 0.5; o.dly = 0.24; o.atk = 0.05;
            shot(has("iwannaslow-c") ? "iwannaslow-c" : "iwanna-c-sung", t + 3.92 + jit(4), o);
        }
        dashStack(t + 4.50, "d4", G * 0.98, bar);
    } else {
        dashStack(t + 4.00, "d4", G * 0.90, bar);
    }

    // line 4 · "dot dot dash" (never dropped at these call sites)
    { Shot o = SHOT_SUNG(); o.gain = 0.90 * G; o.pan = -0.45; o.side = 0.75; o.dly = 0.36;
      shot("dot-c-b3", t + 6.00 + jit(3), o); }
    { Shot o = SHOT_SUNG(); o.gain = 0.86 * G; o.pan = 0.45; o.side = 0.75; o.dly = 0.36;
      shot("dot-j-g3", t + 6.50 + jit(3), o); }
    dashStack(t + 7.00, "b3", G * 0.92, bar);

    // the answer figure — a different one each statement
    if (c.answer == ANS_DOTS) {
        { Shot o = SHOT_SUNG(); o.gain = 0.30 * G; o.pan = 0.38; o.side = 0.8; o.dly = 0.5;
          shot("dot-c-b3", t + 1.30 + jit(4), o); }
        { Shot o = SHOT_SUNG(); o.gain = 0.30 * G; o.pan = -0.38; o.side = 0.8; o.dly = 0.5;
          shot("dot-j-g3", t + 1.65 + jit(4), o); }
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
            dotDrift(t + 7.0 + jit(20), bar >> 2, 0.18 * G, 0.30, 0.95, 3.0, 0.45, 0.40, 0);
        }
    }

    if (c.choirUnder) choir(bar, 0.18 * G);
}

// ── score ──────────────────────────────────────────────────────────────
// The 112-bar loop, translated block-for-block from render10.mjs with the
// SAME evaluation order — every jit()/vel()/nrnd() lands on the same draw.
static void score(void) {
    for (int bar = 0; bar < BARS; bar++) {
        const double t = at(bar, 0);
        const int deg = degAt(bar);
        const int root = bassRoot(deg);
        int chord[3]; triad_of(deg, 59, chord);
        const int four = bar % 4, eight = bar % 8;
        const int D = dense(bar);

        // ---- kick: 4/4, POW per hit, never a drop ----------------------
        if (kickOn(bar)) {
            const double g = introBar(bar) ? 0.66 + 0.035 * (bar - 8)
                : inS(bar, S_RECOGNISE) ? 0.90 - 0.06 * (bar - SB[S_RECOGNISE][0]) : 0.96;
            for (int b = 0; b < 4; b++)
                kick(t + b * BEAT + jit(2.5), g * (b == 0 ? 1 : 0.95), b == 0 ? 1.0 : 0.86);
        }

        // ---- hats (the jit for silent slots is still drawn — the Node
        // file computes u before deciding whether to play) ---------------
        if (hatOn(bar)) {
            for (int s = 0; s < 8; s++) {
                const double swing = (s & 1) ? 0.035 : 0;
                const double u = t + (s * 0.5 + swing) * BEAT + jit(5);
                if (s & 1) { Shot o = SHOT_DRUM(); o.gain = 0.20 * vel(0.20); o.pan = 0.20; o.side = 0.5; o.dur = 0.085; shot("hatC", u, o); }
                else if (s % 4 == 2) { Shot o = SHOT_DRUM(); o.gain = 0.11 * vel(0.20); o.pan = -0.18; o.side = 0.5; o.dur = 0.065; shot("hatC", u, o); }
            }
            if (D && eight >= 4)
                for (int s = 0; s < 16; s++)
                    if (s % 4 == 1 || s % 4 == 3) {
                        const double u = t + ((double)s / 16) * BAR + jit(4);
                        Shot o = SHOT_DRUM(); o.gain = 0.055 * vel(0.5); o.pan = (s & 2) ? 0.35 : -0.35; o.side = 0.6; o.dur = 0.045;
                        shot("hatC", u, o);
                    }
            if ((D || inS(bar, S_SPREAD)) && four == 3) {
                Shot o = SHOT_DRUM(); o.gain = 0.20; o.pan = -0.28; o.side = 0.65; o.dur = 0.34;
                shot("hatO", t + 3.5 * BEAT + 0.02, o);
            }
        }

        // ---- the pump's second trigger: rim on 3, clap when full -------
        if (kickOn(bar)) {
            // (rnd order: JS evaluates args left-to-right — jit, then vel.
            // C leaves that unspecified, so every jit+vel pair is sequenced.)
            { const double tt = t + 2 * BEAT + jit(5); snare(tt, vel(0.20)); }
            if (D) {
                { Shot o = SHOT_DRUM(); o.gain = 0.22; o.pan = -0.12; o.side = 0.72; shot("clap", t + 2 * BEAT + jit(5), o); }
                { Shot o = SHOT_DRUM(); o.gain = 0.09; o.pan = 0.30; o.side = 0.55; shot("snap", t + 2 * BEAT + 0.012, o); }
            }
        }
        if (inS(bar, S_SPREAD) || inS(bar, S_WHOLE))
            for (int s = 0; s < 4; s++) {
                const double u = t + (s + 0.5) * BEAT + jit(8);
                Shot o = SHOT_DRUM(); o.gain = 0.055 * vel(0.4); o.pan = (s & 1) ? 0.38 : -0.38; o.side = 0.7; o.dur = 0.22;
                shot("ride", u, o);
            }
        if (hatOn(bar) && !D)
            for (int s = 0; s < 4; s++) {
                const double u = t + (s + 0.5) * BEAT + jit(9);
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
                        const double u = t + bs[k] * BEAT + jit(8);
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

        // ---- the re-entries announce themselves ------------------------
        if (bar == SB[S_REPLY][0] || bar == SB[S_WHOLE][0])
            revKick(t, 0.9, bar == SB[S_WHOLE][0] ? 0.55 : 0.48);
        if ((inS(bar, S_REPLY) && bar - SB[S_REPLY][0] < 2) || bar == SB[S_WHOLE][0] - 2 || bar == SB[S_WHOLE][0] - 1)
            wub(t, bassRoot(deg), 1, 0.26, 4);

        // ---- bass ------------------------------------------------------
        if (kickOn(bar) || inS(bar, S_SECRET)) {
            const double g = introBar(bar) ? 0.52 : inS(bar, S_SECRET) ? 0.44 : inS(bar, S_RECOGNISE) ? 0.72 : 0.86;
            for (int b = 0; b < 4; b++) {
                const int fifth = four == 3 && b == 3;
                bass(t + (b + 0.5) * BEAT + jit(3), root + (fifth ? 7 : 0), 0.26, g, fifth ? root : NO_SLIDE);
            }
            if (bar % 2 == 0) bass(t, root - 12, BAR * 0.90, g * 0.42, NO_SLIDE);
        }

        // ---- pad + stabs -----------------------------------------------
        if (bar % 2 == 0 && !inS(bar, S_SECRET)) {
            int lowc[3] = {chord[0] - 12, chord[1] - 12, chord[2] - 12};
            sines(t, lowc, 3, BAR * 1.85, inS(bar, S_CARRIER) ? 0.055 + 0.012 * bar : 0.085,
                  bar % 4 ? 0.22 : -0.22, 0.75, 0.5, 0, 0.30);
        }
        if (inS(bar, S_MESSAGE) || inS(bar, S_REPLY) || inS(bar, S_WHOLE) || inS(bar, S_SPREAD)) {
            static const double bs[2] = {0.5, 2.5};
            for (int k = 0; k < 2; k++)
                sines(t + bs[k] * BEAT + jit(4), chord, 3, 0.20, 0.075, bs[k] > 1 ? 0.36 : -0.36, 0.7, 0.9, 0.34, 0.020);
        }
        if (inS(bar, S_SPREAD) && four == 1) {
            int hic[3] = {chord[0] + 12, chord[1] + 12, chord[2] + 12};
            sines(t + 3.5 * BEAT, hic, 3, 0.13, 0.070, rnd() > 0.5 ? 0.5 : -0.5, 0.85, 0.7, 0.70, 0.020);
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
            Shot o = SHOT_SUNG(); o.gain = 0.20; o.pan = -0.12; o.side = 0.5; o.dark = 0.35;
            o.dur = 1.4; o.atk = 0.05; o.wig = 6; o.wigHz = 0.8; o.wigIn = 0.4;
            shot("phone-pickup-a", at(8, 0) + 0.02, o);
        }
        if (bar == 12) phoneTune(bar, 0.20);   // the phone hums the song first
        if (introBar(bar) && bar % 2 == 0) {
            const int k = (bar - 8) / 2;
            dotDrift(at(bar, 2.0) + jit(30), k, 0.13 + 0.01 * k, k % 2 ? 0.40 : -0.40,
                     BAR * 0.85, 4.5, 0.52, 0.48, 0);
        }
        if (bar == 13)
            dotDriftVox(at(bar, 2.5) + jit(30), bar, 0.12, 0.28, BAR * 0.8, 4.0, 0.5, 0.4);
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
            if (bar % 2 == 0)
                frictionPath(at(bar, 0.5), BAR * 0.80, PATH_SPIRAL, 0.42, 0.26, 0, 0.50, 0, 0.10, SHAPE_SLIDE, 1);
            if (bar == 13)
                friction(at(13, 2.2), 1.7, SHAPE_DRAG, 0.62, 0, 0.20, 0, 759, 2300, 103, 214, 0.62, 0, 0.10);
        }
        if (sosBar(bar)) {
            const int k = bar - 16;
            if (bar % 2 == 0)
                dotDrift(at(bar, 1.5) + jit(30), 4 + k / 2, 0.17, (k / 2) % 2 ? -0.42 : 0.42,
                         BAR * 0.9, 4.2, 0.48, 0.42, 0);
            if (bar == 19)
                dotDriftVox(at(bar, 2.5) + jit(30), bar, 0.13, -0.26, BAR * 0.8, 4.0, 0.5, 0.4);
            if (bar == 23)
                dotDrift(at(bar, 2.0) + jit(30), 8, 0.18, 0.0, BAR * 0.9, 4.2, 0.5, 0.40, 0);
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
            if (k == 0) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 0, 1.0, 0, 0}; chorus_fn(bar, c); }
            if (k == 8) { Chorus c = {LEAD_BOTH, 1, ANS_NONE, 1, 0, 1.0, 0, 0}; chorus_fn(bar, c); }
            if (k % 8 == 4) hook_fn(bar, 0);
        }
        if (inS(bar, S_MESSAGE)) {
            { const double tt = at(bar, 3.5) + jit(6); tap(tt, 0.62 * vel(0.3), 0.40, 0.7, 0.26); }
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
        if (bar == SB[S_SECRET][0] || bar == SB[S_SECRET][0] + 4) {
            const double g3[3] = {0.62, 0.50, 0.40}; secretChoir(bar, 0, g3, 0);
        }
        if (bar == SB[S_SECRET][0] + 2 || bar == SB[S_SECRET][0] + 6) {
            const double g3[3] = {0.52, 0.44, 0.36}; secretChoir(bar, 2, g3, 1);
        }
        if (inS(bar, S_SECRET) && bar % 2 == 0) {
            const int k = (bar - SB[S_SECRET][0]) / 2;
            static const struct { int path; double dur, gain, rough; int shape; double speed; } G[4] = {
                {PATH_SPIRAL,   BAR * 0.95, 0.60, 0.44, SHAPE_SLIDE, 0.9},
                {PATH_SCRUB,    BAR * 0.70, 0.66, 0.52, SHAPE_DRAG,  1.2},
                {PATH_SPIRALIN, BAR * 1.10, 0.58, 0.40, SHAPE_SLIDE, 0.8},
                {PATH_CORNER,   BAR * 0.60, 0.70, 0.56, SHAPE_SKID,  1.4}};
            const int gi = k % 4;
            frictionPath(at(bar, 0.25), G[gi].dur, G[gi].path, G[gi].gain, 0.26, 0.12,
                         G[gi].rough, 0, 0.10, G[gi].shape, G[gi].speed);
        }
        if (inS(bar, S_SECRET) && bar % 2 == 1)
            frictionPath(at(bar, 2.6), BAR * 0.34, PATH_SCRUB, 0.40, 0.30, 0.10, 0.60, 0, 0.10, SHAPE_SKID, 1.6);
        if (inS(bar, S_SECRET) && bar % 2 == 1) material(bar, "cult-b3", 0.62, 16, 0.070, 0.18, 0.85, 0.22, 0.9);
        if (inS(bar, S_SECRET) && bar % 2 == 0) choir(bar, 0.22);

        // ══ ACT V · THE REPLY ═══════════════════════════════════════════
        if (inS(bar, S_REPLY)) {
            const int k = bar - SB[S_REPLY][0];
            if (k == 0) { Chorus c = {LEAD_LO, 0, ANS_SOS, 0, 0, 0.96, 0, 0}; chorus_fn(bar, c); }
            if (k == 8) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 0, 1.0, 0, 0}; chorus_fn(bar, c); }
            if (k % 8 == 4) hook_fn(bar, k == 4);
        }
        if (inS(bar, S_REPLY) && four == 3) {
            const char *nms[2] = {"dot-d4", "dot-a3"};
            const double ps[2] = {0.34, -0.34};
            for (int k = 0; k < 2; k++) {
                Shot o = SHOT_SUNG(); o.gain = 0.34; o.pan = ps[k]; o.side = 0.8; o.dly = 0.5;
                shot(nms[k], t + (2 + k) * BEAT + jit(4), o);
            }
        }
        if (bar == SB[S_REPLY][0] + 12) phoneTune(bar, 0.24);
        if (inS(bar, S_REPLY) && eight == 7)
            beepSOS(at(bar, 1), 0.52, 0.55, 0.45);
        if (inS(bar, S_REPLY)) {
            static const double ss[2] = {0.75, 2.75};
            for (int s = 0; s < 2; s++) {
                const double tt = t + ss[s] * BEAT + jit(6);
                tap(tt, 0.38 * vel(0.3), ss[s] > 2 ? -0.4 : 0.4, 0.7, 0.24);
            }
            if (four == 1) click(at(bar, 3.75), 0.44, 0.35, 0.7, 0.35);
            if (four == 3)
                friction(at(bar, 2.3), 1.7, SHAPE_DRAG, 0.44, -0.14, 0.22, 0, 859, 2400, 114, 223, 0.60, 0, 0.10);
            if (eight == 7)
                frictionPath(at(bar, 2.6), 0.75, PATH_SPIRALIN, 0.42, 0.30, 0, 0.66, 0, 0.10, SHAPE_SKID, 1);
        }
        if (inS(bar, S_REPLY) && bar % 4 == 2) material(bar, "dash-camille-fs4-hold", 0.62, 16, 0.055, 0.18, 0.85, 0.22, 0.9);
        // the 1:42 ornament, featured: twice in the reply, twice where it spreads
        if (bar == SB[S_REPLY][0] + 10 || bar == SB[S_REPLY][0] + 14) raga(bar, 1.5, 0.44);
        if (bar == SB[S_SPREAD][0] + 3 || bar == SB[S_SPREAD][0] + 7) raga(bar, 2.0, 0.38);

        // ══ ACT VI · IT SPREADS ═════════════════════════════════════════
        if (inS(bar, S_SPREAD)) {
            const int k = bar - SB[S_SPREAD][0];
            if (k == 0) { Chorus c = {LEAD_HI, 0, ANS_NONE, 0, 0, 0.55, 1, 0}; chorus_fn(bar, c); }
            if (k == 8) { Chorus c = {LEAD_LO, 0, ANS_NONE, 0, 0, 0.60, 1, 1}; chorus_fn(bar, c); }
        }
        if (inS(bar, S_SPREAD) && bar % 2 == 0) {
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
            { Shot o = SHOT_SUNG(); o.gain = 0.26; o.pan = 0; o.side = 0.25; o.dark = 0.6;
              snprintf(nb, sizeof nb, "bassdash-%s", low); shot(nb, at(bar, 0), o); }
        }
        if (inS(bar, S_SPREAD)) {
            static const char digits[8] = {'2', '8', '5', '8', '*', '8', '5', '2'};
            for (int k = 0; k < 3; k++) {
                const double u = t + ((double)k * 4 / 3) * BEAT + jit(8);
                dtmf(u, digits[(bar * 3 + k) % 8], 0.055,
                     0.46 * vel(0.3), k == 1 ? 0 : k ? 0.5 : -0.5, 0.85, 0.40);
            }
            if (four == 2) bop(at(bar, 3.25), midihz(blipMidi(bar, 2)), 0.26, -0.4, 0.8, 0.085, 0.45);
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
            friction(at(bar, 2.5 + (bar % 3) * 0.25), 0.58, SHAPE_SKID,
                     0.34 * vel(0.3), bar % 2 ? 0.46 : -0.46, 0.30, 0,
                     2200, 1050, 280 - (bar % 4) * 30, 149, 0.62, 0, 0.10);
            if (four == 0)
                friction(at(bar, 3.1), 0.95, SHAPE_DRAG, 0.30, bar % 4 ? -0.2 : 0.2,
                         0.24, 0, 900, 1900, 119, 189, 0.55, 1, 0.10);
        }
        if (bar == SB[S_SPREAD][0] + 2 || bar == SB[S_SPREAD][0] + 6) {
            const char *picks[3]; const int pc = choirFor(degAt(bar), picks);
            const char *hi = picks[pc - 1], *mid = picks[pc - 2 > 0 ? pc - 2 : 0];
            char hbuf[64], mbuf[64];
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
            } else {
                snprintf(hbuf, sizeof hbuf, "cult-%s", hi);
                stretched(hbuf, at(bar, 1) + jit(10), 0.34, 0.30, 0, 2.3, 8.0, 0.92, 0.05, BUS_VOX, 0.55);
                snprintf(mbuf, sizeof mbuf, "cult-%s", mid);
                stretched(mbuf, at(bar, 1.5) + jit(10), 0.22, -0.34, 0, 2.6, 8.0, 0.92, 0.12, BUS_VOX, 0.55);
            }
        }
        if (inS(bar, S_SPREAD)) {
            const char *nm = ALT_CULTS[(bar - SB[S_SPREAD][0]) % 6];
            if (has(nm)) {
                Shot o = SHOT_SUNG();
                o.gain = 0.30 + 0.02 * ((bar - SB[S_SPREAD][0]) % 4);
                o.pan = bar % 2 ? 0.62 : -0.62; o.side = 0.90; o.dark = 0.16; o.dly = 0.48;
                shot(nm, at(bar, 2.5 + ((bar % 3) * 0.25)), o);
            }
        }
        if (inS(bar, S_SPREAD) && bar % 2 == 1) material(bar, "cult-d4", 0.72, 16, 0.085, 0.18, 0.85, 0.30, 0.9);

        // ══ ACT VII · THE WHOLE MESSAGE ═════════════════════════════════
        if (bar == SB[S_WHOLE][0]) { Chorus c = {LEAD_BOTH, 0, ANS_DOTS, 0, 1, 1.0, 0, 0}; chorus_fn(bar, c); }
        if (bar == SB[S_WHOLE][0] + 8) { Chorus c = {LEAD_BOTH, 1, ANS_NONE, 1, 1, 1.0, 0, 0}; chorus_fn(bar, c); }
        if (bar == SB[S_WHOLE][0] + 16) hook_fn(bar, 1);
        if (inS(bar, S_WHOLE)) {
            static const double ss[3] = {0.75, 1.75, 3.25};
            for (int s = 0; s < 3; s++) {
                const double tt = t + ss[s] * BEAT + jit(6);
                tap(tt, 0.52 * vel(0.4), ss[s] > 2 ? -0.42 : 0.42, 0.7, 0.22);
            }
            if (eight == 7) beepSOS(at(bar, 1), 0.34, -0.55, 0.5);
            if (eight == 7)
                frictionPath(at(bar, 2.2), 1.8, PATH_SCRUB, 0.52, 0.20, 0, 0.64, 0, 0.10, SHAPE_DRAG, 1);
            if ((bar - SB[S_WHOLE][0]) % 8 == 2)
                friction(at(bar, 1.3), 0.66, SHAPE_SKID, 0.36, 0.40, 0.28, 0, 2300, 1000, 289, 143, 0.60, 0, 0.10);
        }
        if (inS(bar, S_WHOLE) && bar % 4 == 3) material(bar, "cult-fs4", 0.66, 16, 0.060, 0.18, 0.9, 0.22, 0.9);

        // ══ ACT VIII · RECOGNITION ══════════════════════════════════════
        if (bar == SB[S_RECOGNISE][0]) { Chorus c = {LEAD_HI, 0, ANS_NONE, 0, 0, 0.62, 1, 0}; chorus_fn(bar, c); }
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
            if (k == 2) phoneTune(bar, 0.16);    // hums it once more on the way out
            if (k == 6) bop(at(bar, 3), midihz(59), 0.24, 0, 0.6, 0.085, 0.5);
            if (k == 4)
                dotDrift(at(bar, 2) + jit(30), 8, 0.16, 0, BAR * 0.9, 4.5, 0.55, 0.45, 0);
        }

        // ---- the drone walking out -------------------------------------
        if (inS(bar, S_CARRIEROFF) && bar % 2 == 0)
            choir(bar, 0.48 * (1 - (double)(bar - SB[S_CARRIEROFF][0]) / 11));

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

// ── mixdown ────────────────────────────────────────────────────────────
int main(int argc, char **argv) {
    for (int a = 1; a < argc; a++)
        if (!strcmp(argv[a], "--spatial")) SPATIAL = 1;
    clock_t t0 = clock();
    // LANE = pop/cult, two levels up from the binary (pop/cult/c/cultremix)
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
    sideB = calloc(N, 4); sideV = calloc(N, 4);
    sideT = calloc(N, 4); sideS = calloc(N, 4);
    dlySend = calloc(N, 4);
    if (!dlySend) { fprintf(stderr, "! out of memory\n"); return 1; }

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

    // The tubular colour is OFF in v10 (TUBE_COLOUR = 0 — "i hate the tube
    // bus"): the combs and saturation would be multiplied to nothing, so we
    // skip them entirely. The DC blocker still runs — the Node file applies
    // it to the clean dashes too, and the bus keeps its deep pump.
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

    // Special Sign side return + final sum, one pass. Band-limited
    // 80 Hz – 11.5 kHz, antisymmetric (L=+, R=−), slewed per-act send;
    // each bus's share of the side field carries that bus's duck.
    float *L = malloc(N * 4), *R = malloc(N * 4);
    double peak = 0;
    {
        const double hpRc = 1 / (TAU * 80), hpA = hpRc / (hpRc + 1.0 / SR);
        const double lpK = 1 - exp((-TAU * 11500) / SR);
        double hp = 0, lp = 0, prev = 0, send = 0.9;
        for (long i = 0; i < N; i++) {
            const double be = bedEnv[i], pe = pumpEnv[i];
            const double dv = pow(be, 0.25), ds = pow(be, 0.5);
            const double s = sideB[i] * be + sideV[i] * dv + sideT[i] * pe * TUBEG + sideS[i] * ds;
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
            const double l = (musicL[i] * be + voxL[i] * dv * VOXG + tubeL[i] * pe * TUBEG
                + sigL[i] * ds * SIGG + drumsL[i] + so) * fade;
            const double r = (musicR[i] * be + voxR[i] * dv * VOXG + tubeR[i] * pe * TUBEG
                + sigR[i] * ds * SIGG + drumsR[i] - so) * fade;
            L[i] = (float)l; R[i] = (float)r;
            if (fabs(l) > peak) peak = fabs(l);
            if (fabs(r) > peak) peak = fabs(r);
        }
    }
    const double norm = peak > 1e-9 ? 0.92 / peak : 1;
    for (long i = 0; i < N; i++) { L[i] = (float)(L[i] * norm); R[i] = (float)(R[i] * norm); }
    fprintf(stderr, "# pre-master peak %.6f . linear trim %.3f\n", peak, norm);

    char outdir[4200]; snprintf(outdir, sizeof outdir, "%s/c/out", LANE);
    mkdir(outdir, 0755);
    char outp[4400]; snprintf(outp, sizeof outp, "%s/%s.wav", outdir,
                              SPATIAL ? "whistlecultspatial" : "cult-remix-c");
    write_wav_f32_stereo(outp, L, R, N);
    if (missingWarned) fprintf(stderr, "  ! %d missing-sample warnings\n", missingWarned);
    const double tAll = (double)(clock() - t0) / CLOCKS_PER_SEC;
    printf("ok %s  (%.1fs)  score %.2fs . total %.2fs\n", outp, BARS * BAR, tScore, tAll);
    return 0;
}
