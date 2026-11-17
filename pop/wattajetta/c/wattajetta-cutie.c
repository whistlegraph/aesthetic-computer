// wattajetta-cutie.c — the cutie club engine. The composition lives in
// bin/render-wattajetta-cutie.mjs (the feel in CFG, the form in ROWS); run
// with --bake it writes every note it played and this file plays them again.
//
// Still all water, and still the lane's two materials:
//
//   SINE  — water holding a shape: kick, bass, bloop, pad, pluck, lead,
//           flyby, tick; and the saws of the acid line, its strings and the
//           wub, each closed down by a four-pole ladder
//   SPRAY — water losing its shape: one xorshift noise through a
//           state-variable filter — claps, hats, shakers, air, the riser
//
// plus three things frozen before they got here and replayed from disk:
// the FEM bells, the jeffrey drops, and jeffrey's sung syllables.
//
// Every voice below is its JS twin line for line. The spray voices share one
// noise stream, so events must be played in the order the score lists them.
//
// Build:  ./build-cutie.sh
// Run:    ./wattajetta-cutie <score.txt> --raw out.f32    (f32le stereo 48k premaster)
//
// Score (text, one item per line, written by the renderer's --bake):
//   sr / frames / material <name|none> <amount> / cfg <dotted.name> <x>
//   smp <id> <path.f32>
//   kick t g tight                    spray t fc q type att dec len gain pan wet
//   tick t hz g pan                   bloop t midi g pan len
//   bass t dur midi g soft            pad t dur midi g pan bright
//   air t dur midi g pan              pluck t midi g pan decay
//   lead t dur midi vel g pan         flyby t dir          riser t dur g
//   acidrow t0 level cut0 cut1 reso n   then n lines:  . t midi accent slide
//   acidchord t dur level cut0 cut1 n midi…
//   wub t0 level dropLast bass×8
//   bell <smp> t dur amp pan wet      vox <smp> t rate gain pan from to wet echo
//   sung <smp> t hold gain pan attack wet echo
//   under <row>                       — the music bus goes underwater for that row

#define _POSIX_C_SOURCE 200809L
#include "../../lib/c/ac_bake.h"
#include "../../lib/c/ac_material.h"

static double SR = 48000, BEAT, BAR, SIX;
static long N;
static float *kickBus, *bassBus, *acidBus, *dly;   // mono
static float *drums, *music, *bells, *rev;         // stereo, interleaved
static AcNoise nz = { 0x1badcafe };

// ── cfg: the renderer's CFG, flattened to dotted names ──────────────────
typedef struct { char name[40]; double v; } Cfg;
static Cfg cfgs[96];
static int ncfg;
static double cfg(const char *name) {
    for (int i = 0; i < ncfg; i++) if (!strcmp(cfgs[i].name, name)) return cfgs[i].v;
    ac_die("the score has no cfg", name);
    return 0;
}
static struct { double start, end, attack, decay, gain; } K;
static struct { double bloop, air, arp, fly, riser, vox; } G;
static void cfg_freeze(void) {
    BEAT = 60 / cfg("bpm"); BAR = 4 * BEAT; SIX = BEAT / 4;
    K.start = cfg("kick.start"); K.end = cfg("kick.end"); K.attack = cfg("kick.attack");
    K.decay = cfg("kick.decay"); K.gain = cfg("kick.gain");
    G.bloop = cfg("gain.bloop"); G.air = cfg("gain.air"); G.arp = cfg("gain.arp");
    G.fly = cfg("gain.fly"); G.riser = cfg("gain.riser"); G.vox = cfg("gain.vox");
}

static inline double clampd(double v, double a, double b) { return fmax(a, fmin(b, v)); }
static inline double panL(double p) { return cos((clampd(p, -1, 1) + 1) * M_PI / 4); }
static inline double panR(double p) { return sin((clampd(p, -1, 1) + 1) * M_PI / 4); }
// linear release after `dur`
static inline double rel_env(double x, double dur, double rel) { return x < dur ? 1 : fmax(0, 1 - (x - dur) / rel); }
// a naive saw at phase p with the polyBLEP correction for increment dt
static inline double blep_saw(double p, double dt) {
    double w = 2 * p - 1;
    if (p < dt) { const double q = p / dt; w -= q + q - q * q - 1; }
    else if (p > 1 - dt) { const double q = (p - 1) / dt; w -= q * q + q + q + 1; }
    return w;
}

// sidechain: every kick, in time order
typedef struct { double t, g; } Hit;
static Hit *kicks;
static long nkicks, capkicks;

// ── voices ──────────────────────────────────────────────────────────────
static void kick(double t, double g, int tight) {
    const long i0 = ac_round(t * SR), n = (long)floor((tight ? 0.26 : 0.55) * SR);
    const double dec = tight ? 0.09 : K.decay;
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += (K.end + (K.start - K.end) * exp(-x / 0.045)) / SR;
        ac_add(kickBus, i0 + k, sin(AC_TAU * ph) * fmin(1, x / K.attack) * exp(-x / dec) * g * K.gain);
    }
    if (nkicks == capkicks) { capkicks = capkicks ? capkicks * 2 : 1024; kicks = realloc(kicks, (size_t)capkicks * sizeof *kicks); if (!kicks) ac_die("out of memory", NULL); }
    kicks[nkicks++] = (Hit){ t, g };
}

// Spray: white noise through one state-variable filter. type 0 band, 1 high.
static void spray(double t, double fc, double q, int type, double att, double dec, double len, double gain, double pan, double wet) {
    const long i0 = ac_round(t * SR), n = (long)floor(len * SR);
    const double f = 2 * sin(M_PI * fmin(fc, 7600) / SR), damp = 1 / q;
    const double gl = gain * panL(pan), gr = gain * panR(pan);
    double lo = 0, bd = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR, hi = ac_noise(&nz) - lo - damp * bd;
        bd += f * hi; lo += f * bd;
        const double v = (type ? hi : bd) * fmin(1, x / att) * exp(-x / dec);
        const long j = (i0 + k) * 2;
        ac_add(drums, j, v * gl); ac_add(drums, j + 1, v * gr);
        if (wet) { ac_add(rev, j, v * gl * wet); ac_add(rev, j + 1, v * gr * wet); }
    }
}

static void tick(double t, double hz, double g, double pan) {
    const long i0 = ac_round(t * SR), n = (long)floor(0.05 * SR);
    const double gl = g * panL(pan), gr = g * panR(pan);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += hz / SR;
        const double v = sin(AC_TAU * ph) * exp(-x / 0.009);
        const long j = (i0 + k) * 2;
        ac_add(drums, j, v * gl); ac_add(drums, j + 1, v * gr);
        ac_add(rev, j, v * gl * 0.2); ac_add(rev, j + 1, v * gr * 0.2);
    }
}

// Bloop: a sine that rises into its note — a drip. The lane's signature percussion.
static void bloop(double t, double midi, double g, double pan, double len) {
    const double f = ac_mtof(midi), a = g * G.bloop, gl = a * panL(pan), gr = a * panR(pan);
    const long i0 = ac_round(t * SR), n = (long)floor(len * 4 * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += f * (1 - 0.48 * exp(-x / 0.028)) / SR;
        const double v = sin(AC_TAU * ph) * fmin(1, x / 0.004) * exp(-x / (len * 0.42));
        const long j = (i0 + k) * 2;
        ac_add(music, j, v * gl); ac_add(music, j + 1, v * gr);
        ac_add(rev, j, v * gl * 0.45); ac_add(rev, j + 1, v * gr * 0.45);
        ac_add(dly, i0 + k, v * a * 0.42);
    }
}

static void bass_note(double t, double dur, double midi, double g, int soft) {
    const double f = ac_mtof(midi), att = soft ? 0.12 : 0.006;
    const long i0 = ac_round(t * SR), n = (long)floor((dur + 0.12) * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += f / SR; if (ph > 1) ph -= 1;
        const double p = AC_TAU * ph, pl = soft ? 0.25 : exp(-x / 0.14);
        const double tone = sin(p) + pl * (0.42 * sin(2 * p) + 0.2 * sin(3 * p) + 0.1 * sin(4 * p));
        const double env = fmin(1, x / att) * rel_env(x, dur, 0.12) * (soft ? 1 : 0.55 + 0.45 * exp(-x / 0.25));
        ac_add(bassBus, i0 + k, tone * env * g);
    }
}

// Acid strings: the chord itself as long notes — two detuned saws a tone
// through a ladder that opens slowly instead of snapping.
static void acid_chord(double t, double dur, double level, double cut0, double cut1, int nn, const double *notes) {
    const double rel = 0.5, k = 0.5 * 3.8;
    const long i0 = ac_round(t * SR), n = (long)floor((dur + rel) * SR);
    double inc[16], ph[16];
    const int voices = nn * 2;
    for (int v = 0; v < nn; v++) {
        inc[v * 2] = ac_mtof(notes[v]) * pow(2.0, -7.0 / 1200) / SR;
        inc[v * 2 + 1] = ac_mtof(notes[v]) * pow(2.0, 7.0 / 1200) / SR;
    }
    for (int v = 0; v < voices; v++) ph[v] = fmod(v * 0.37, 1);
    double y1 = 0, y2 = 0, y3 = 0, y4 = 0;
    for (long i = 0; i < n && i0 + i < N; i++) {
        const double x0 = i / SR, u = fmin(1, x0 / dur);
        double saw = 0;
        for (int v = 0; v < voices; v++) {
            const double dt = inc[v];
            double p = ph[v] + dt; if (p >= 1) p -= 1; ph[v] = p;
            saw += blep_saw(p, dt);
        }
        saw /= voices;
        const double swell = 0.5 - 0.5 * cos(M_PI * u);
        const double fc = fmin(8000, 220 * pow(2.0, (cut0 + (cut1 - cut0) * swell) * 4.2 + 0.5 * sin(AC_TAU * (t + x0) / (2 * BAR))));
        const double g = 1 - exp(-AC_TAU * fc / SR);
        const double x = tanh(saw * 1.6 - k * y4);
        y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
        ac_add(acidBus, i0 + i, y4 * 1.6 * fmin(1, x0 / 0.09) * rel_env(x0, dur, rel) * level);
    }
}

// Acid: one saw through a four-pole resonant ladder whose cutoff snaps open
// on every note and harder on the accents; tied steps slide instead of
// retriggering. One continuous voice per row, so slides and filter state carry.
typedef struct { double t, midi; int acc, slide; } Step;
static void acid_row(double t0, double level, double cut0, double cut1, double reso, const Step *evs, int nevs) {
    const long i0 = ac_round(t0 * SR), n = ac_round(8 * BAR * SR);
    const double k = reso * 3.8;
    double ph = 0, f = 0, ft = 0, y1 = 0, y2 = 0, y3 = 0, y4 = 0, fenv = 0, aenv = 0, gateEnd = 0;
    int acc = 0, e = 0, tied = 0;
    for (long i = 0; i < n && i0 + i < N; i++) {
        const double t = t0 + i / SR;
        while (e < nevs && evs[e].t <= t) {
            const Step *ev = &evs[e++];
            ft = ac_mtof(ev->midi);
            if (!tied || f == 0) { f = ft; fenv = 1; acc = ev->acc; }
            gateEnd = ev->t + (ev->slide ? SIX * 1.1 : SIX * 0.62);
            tied = ev->slide;
        }
        if (f == 0) continue;
        f += (ft - f) * 0.0008;
        const int gate = t < gateEnd;
        aenv += ((gate ? 1 : 0) - aenv) * (gate ? 0.02 : 0.004);
        fenv *= acc ? 0.99975 : 0.99988;
        const double dt = f / SR;
        ph += dt; if (ph >= 1) ph -= 1;
        const double saw = blep_saw(ph, dt);
        const double fc = fmin(9000, 150 * pow(2.0, (cut0 + (cut1 - cut0) * ((double)i / n)) * 4.5 + (1.6 + acc) * fenv));
        const double g = 1 - exp(-AC_TAU * fc / SR);
        const double x = tanh(saw * 0.8 - k * y4);
        y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
        ac_add(acidBus, i0 + i, tanh(y4 * (1 + k * 0.6) * 1.4) * aenv * (0.75 + 0.35 * acc) * level);
    }
}

// Wub: a held saw over its own sub-octave square, the ladder's cutoff swung
// by an LFO locked to the beat. The rate changes by the bar, so the bass talks.
static void wub_row(double t0, double level, int dropLast, const double *barBass) {
    static const double RATES[8] = { 1, 1, 2, 1, 2, 2, 3, 4 };     // wobbles per beat, by bar of the row
    const int bars = dropLast ? 7 : 8;
    const long i0 = ac_round(t0 * SR), n = ac_round(bars * BAR * SR);
    const double k = 0.55 * 3.8, barN = BAR * SR, beatN = BEAT * SR;
    double ph = 0, f = 0, y1 = 0, y2 = 0, y3 = 0, y4 = 0;
    for (long i = 0; i < n && i0 + i < N; i++) {
        int bar = (int)floor(i / barN); if (bar > 7) bar = 7;
        const double bm = barBass[bar], ft = ac_mtof(bm + (bm < 36 ? 12 : 0));
        f = f ? f + (ft - f) * 0.0006 : ft;
        const double inBar = i - bar * barN, lfo = 0.5 - 0.5 * cos(AC_TAU * RATES[bar] * inBar / beatN);
        const double dt = f / SR;
        ph += dt * 0.5; if (ph >= 1) ph -= 1;                 // phase runs at the sub-octave
        const double p2 = fmod(ph * 2, 1), saw = blep_saw(p2, dt), sub = ph < 0.5 ? 0.5 : -0.5;
        const double fc = 85 * pow(2.0, pow(lfo, 1.4) * 4.9);
        const double g = 1 - exp(-AC_TAU * fc / SR);
        const double x = tanh((saw + sub) * 1.3 - k * y4);
        y1 += g * (x - y1); y2 += g * (y1 - y2); y3 += g * (y2 - y3); y4 += g * (y3 - y4);
        const double edge = ac_min3(1, i / 600.0, (n - i) / 2400.0);
        ac_add(acidBus, i0 + i, tanh(y4 * 2.2) * (0.55 + 0.45 * lfo) * edge * level);
    }
}

static void pad_note(double t, double dur, double midi, double g, double pan, double bright) {
    const double f = ac_mtof(midi), rel = 1.6, gl = g * panL(pan), gr = g * panR(pan);
    const long i0 = ac_round(t * SR), n = (long)floor((dur + rel) * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += f * (1 + 0.0016 * sin(AC_TAU * 0.13 * (t + x) + midi)) / SR; if (ph > 1) ph -= 1;
        const double p = AC_TAU * ph;
        const double v = (0.7 * sin(p) + 0.2 * sin(2 * p + 0.4) + bright * 0.1 * sin(3 * p))
            * fmin(1, x / 0.35) * rel_env(x, dur, rel);
        const long j = (i0 + k) * 2;
        ac_add(music, j, v * gl); ac_add(music, j + 1, v * gr);
        ac_add(rev, j, v * gl * 0.5); ac_add(rev, j + 1, v * gr * 0.5);
    }
}

// Air: breath with a pitch — noise through a narrow band at the note, a
// different noise in each ear. This is the layer that keeps the room open.
static void air_note(double t, double dur, double midi, double g, double pan) {
    const double fc = fmin(ac_mtof(midi), 6000), rel = 1.8;
    const long i0 = ac_round(t * SR), n = (long)floor((dur + rel) * SR);
    const double f = 2 * sin(M_PI * fc / SR), damp = 1.0 / 22, a = g * G.air * 0.05;
    const double gl = a * panL(pan * 0.45), gr = a * panR(pan * 0.45);
    double l1 = 0, b1 = 0, l2 = 0, b2 = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        double hi = ac_noise(&nz) - l1 - damp * b1; b1 += f * hi; l1 += f * b1;
        hi = ac_noise(&nz) - l2 - damp * b2; b2 += f * hi; l2 += f * b2;
        const double env = fmin(1, x / 0.7) * rel_env(x, dur, rel) * (0.72 + 0.28 * sin(AC_TAU * 0.21 * (t + x) + midi));
        const long j = (i0 + k) * 2;
        ac_add(music, j, b1 * env * gl); ac_add(music, j + 1, b2 * env * gr);
        ac_add(rev, j, b1 * env * gl * 0.8); ac_add(rev, j + 1, b2 * env * gr * 0.8);
    }
}

// Pluck: a hollow little square whose upper partials close fast.
static void pluck(double t, double midi, double g, double pan, double decay) {
    const double f = ac_mtof(midi), a = g * G.arp, gl = a * panL(pan), gr = a * panR(pan);
    const long i0 = ac_round(t * SR), n = (long)floor(decay * 5 * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        ph += f / SR; if (ph > 1) ph -= 1;
        const double p = AC_TAU * ph, c = exp(-x / 0.06);
        const double v = (sin(p) + c * (0.33 * sin(3 * p) + 0.16 * sin(5 * p))) * fmin(1, x / 0.003) * exp(-x / decay);
        const long j = (i0 + k) * 2;
        ac_add(music, j, v * gl); ac_add(music, j + 1, v * gr);
        ac_add(rev, j, v * gl * 0.3); ac_add(rev, j + 1, v * gr * 0.3);
        ac_add(dly, i0 + k, v * a * 0.5);
    }
}

// Lead: the hook's voice — a bloop that stays, with a late soft vibrato.
static void lead(double t, double dur, double midi, double vel, double g, double pan) {
    const double f = ac_mtof(midi), rel = 0.45, a = vel * g, gl = a * panL(pan), gr = a * panR(pan);
    const long i0 = ac_round(t * SR), n = (long)floor((dur + rel) * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double x = k / SR;
        const double vib = x > 0.22 ? 1 + 0.0045 * fmin(1, (x - 0.22) / 0.3) * sin(AC_TAU * 5.1 * x) : 1;
        ph += f * (1 - 0.3 * exp(-x / 0.014)) * vib / SR; if (ph > 1) ph -= 1;
        const double p = AC_TAU * ph;
        const double v = (sin(p) + 0.2 * sin(2 * p) + 0.09 * exp(-x / 0.18) * sin(3 * p))
            * fmin(1, x / 0.008) * (0.6 + 0.4 * exp(-x / 0.4)) * rel_env(x, dur, rel);
        const long j = (i0 + k) * 2;
        ac_add(music, j, v * gl); ac_add(music, j + 1, v * gr);
        ac_add(rev, j, v * gl * 0.5); ac_add(rev, j + 1, v * gr * 0.5);
        ac_add(dly, i0 + k, v * a * 0.3);
    }
}

// Flyby: the jet, as one sine passing overhead.
static void flyby(double t, double dir) {
    const double dur = 2.5 * BAR;
    const long i0 = ac_round(t * SR), n = (long)floor(dur * SR);
    double ph = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double u = (double)k / n, s = 1 / (1 + exp(-(u - 0.5) * 14));
        ph += ac_mtof(dir > 0 ? 76 : 71) * (1.06 - 0.12 * s) * (1 + 0.003 * sin(AC_TAU * 6 * u * dur)) / SR; if (ph > 1) ph -= 1;
        const double v = sin(AC_TAU * ph) * exp(-((u - 0.5) * (u - 0.5)) / 0.03) * G.fly, pan = dir * (2 * u - 1) * 0.9;
        const long j = (i0 + k) * 2;
        ac_add(music, j, v * panL(pan)); ac_add(music, j + 1, v * panR(pan));
        ac_add(rev, j, v * 0.5); ac_add(rev, j + 1, v * 0.5);
    }
}

// Riser: spray climbing through a moving band, into a seam.
static void riser(double t, double dur, double g) {
    const long i0 = ac_round(t * SR), n = (long)floor(dur * SR);
    double lo = 0, bd = 0, lo2 = 0, bd2 = 0;
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double u = (double)k / n, f = 2 * sin(M_PI * (350 * pow(16.0, u)) / SR);
        double hi = ac_noise(&nz) - lo - 0.3 * bd; bd += f * hi; lo += f * bd;
        hi = ac_noise(&nz) - lo2 - 0.3 * bd2; bd2 += f * hi; lo2 += f * bd2;
        const double a = u * u * g * G.riser;
        const long j = (i0 + k) * 2;
        ac_add(music, j, bd * a); ac_add(music, j + 1, bd2 * a);
        ac_add(rev, j, bd * a * 0.6); ac_add(rev, j + 1, bd2 * a * 0.6);
    }
}

// Bells: FEM strikes solved once and kept on disk — the left ring, then the
// right. Short notes are the same ring choked early. Never crunched.
static void bell(const AcSmp *s, double t, double dur, double a, double pan, double wet) {
    const long h = s->n >> 1, i0 = ac_round(t * SR), fade = (long)floor(0.08 * SR);
    const long want = (long)floor(dur * SR), n = h < want ? h : want;
    const float *L = s->d, *R = s->d + h;
    const double gl = a * fmin(1, 1 - pan), gr = a * fmin(1, 1 + pan);
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double e = k > n - fade ? (double)(n - k) / fade : 1;
        const long j = (i0 + k) * 2;
        const double l = L[k] * gl * e, r = R[k] * gr * e;
        ac_add(bells, j, l); ac_add(bells, j + 1, r);
        ac_add(rev, j, l * wet); ac_add(rev, j + 1, r * wet);
    }
}

// Vocal drops: the cached jeffrey-pvc one-shots, a slice replayed at a rate.
static void vox(const AcSmp *s, double t, double rate, double gain, double pan, double from, double to, double wet, double echo) {
    const double s0 = from * s->n, s1 = to * s->n;
    const long n = (long)floor((s1 - s0) / rate), i0 = ac_round(t * SR);
    const double fade = (n >> 2) < 400 ? (double)(n >> 2) : 400;
    const double a = gain * G.vox, gl = a * panL(pan), gr = a * panR(pan);
    for (long k = 0; k < n && i0 + k < N; k++) {
        const double p = s0 + k * rate, q = floor(p), fr = p - q;
        const long qi = (long)q;
        const double x0 = qi >= 0 && qi < s->n ? s->d[qi] : 0, x1 = qi + 1 >= 0 && qi + 1 < s->n ? s->d[qi + 1] : 0;
        const double v = (x0 * (1 - fr) + x1 * fr) * ac_min3(1, k / fade, (n - k) / fade);
        const long j = (i0 + k) * 2;
        ac_add(bells, j, v * gl); ac_add(bells, j + 1, v * gr);
        ac_add(rev, j, v * gl * wet); ac_add(rev, j + 1, v * gr * wet);
        ac_add(dly, i0 + k, v * a * echo);
    }
}

// jeffrey, sung: one syllable already pitched and held to length (sing-phonemes.py).
static void sung(const AcSmp *s, double t, double hold, double gain, double pan, double attack, double wet, double echo) {
    const long i0 = ac_round(t * SR), want = (long)floor((hold + 0.12) * SR), len = s->n < want ? s->n : want;
    const double fade = floor(0.1 * SR), att = fmax(1, floor(attack * SR));
    const double gl = gain * panL(pan), gr = gain * panR(pan);
    for (long k = 0; k < len && i0 + k < N; k++) {
        const double v = s->d[k] * ac_min3(1, k / att, (len - k) / fade);
        const long j = (i0 + k) * 2;
        ac_add(bells, j, v * gl); ac_add(bells, j + 1, v * gr);
        ac_add(rev, j, v * gl * wet); ac_add(rev, j + 1, v * gr * wet);
        ac_add(dly, i0 + k, v * gain * echo);
    }
}

// ── post ────────────────────────────────────────────────────────────────
// Underwater: a low-pass over the music bus that closes for one row and
// opens again a bar into the next.
static void underwater(int row) {
    const long a = ac_round(row * 8 * BAR * SR), z = ac_round((row + 1) * 8 * BAR * SR);
    const double ramp = BAR * SR;
    double l = 0, r = 0;
    for (long i = a; i < z + ramp && i < N; i++) {
        const double depth = ac_min3(1, (i - a) / ramp, fmax(0, (z + ramp - i) / ramp));
        const double fc = 16000 * (1 - depth) + 900 * depth, k = 1 - exp(-AC_TAU * fc / SR);
        l += k * (music[i * 2] - l); r += k * (music[i * 2 + 1] - r);
        music[i * 2] = (float)l; music[i * 2 + 1] = (float)r;
    }
}

// Phaser on everything acid: six swept allpass stages a side, the two ears a
// quarter-cycle apart, one sweep every four bars. Dry stays in the middle.
static void phaser(double send) {
    const double rate = 1 / (4 * BAR), fb = 0.5;
    for (int ch = 0; ch < 2; ch++) {
        double x1[6] = { 0 }, y1[6] = { 0 }, a = 0, last = 0;
        for (long i = 0; i < N; i++) {
            if ((i & 31) == 0) {
                const double l = 0.5 + 0.5 * sin(AC_TAU * rate * i / SR + ch * M_PI / 2);
                const double tn = tan(M_PI * 220 * pow(14.0, l) / SR);
                a = (1 - tn) / (1 + tn);
            }
            const double dry = acidBus[i];
            double v = dry + last * fb;
            for (int st = 0; st < 6; st++) { const double y = -a * v + x1[st] + a * y1[st]; x1[st] = v; y1[st] = y; v = y; }
            last = v;
            ac_add(music, i * 2 + ch, (dry + v) * 0.85 * send);
            if (ch == 0) ac_add(dly, i, dry * 0.08 * send);
        }
    }
}

// Ping-pong delay from the mono send, into the music bus and a little into the room.
static void ping_pong(void) {
    const long D = ac_round(cfg("delay.beats") * BEAT * SR);
    const double fb = cfg("delay.feedback"), damp = cfg("delay.damp"), level = cfg("delay.level");
    float *L = ac_alloc((size_t)D, sizeof(float)), *R = ac_alloc((size_t)D, sizeof(float));
    double lpL = 0, lpR = 0;
    for (long i = 0; i < N; i++) {
        const long w = i % D;
        const double oL = L[w], oR = R[w];
        lpR += (1 - damp) * (oR - lpR); lpL += (1 - damp) * (oL - lpL);
        L[w] = (float)(dly[i] + fb * lpR); R[w] = (float)lpL;
        ac_add(music, i * 2, oL * level); ac_add(music, i * 2 + 1, oR * level);
        ac_add(rev, i * 2, oL * 0.25); ac_add(rev, i * 2 + 1, oR * 0.25);
    }
    free(L); free(R);
}

// Reverb: eight damped combs and four allpasses per ear, processed in place.
static void reverb(void) {
    static const int COMB[8] = { 1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617 }, AP[4] = { 556, 441, 341, 225 };
    const double feedback = cfg("reverb.feedback"), damp = cfg("reverb.damp"), level = cfg("reverb.level");
    const long pre = ac_round(cfg("reverb.predelay") * SR);
    const double scale = SR / 44100;
    for (int ch = 0; ch < 2; ch++) {
        float *cb[8], *ab[4], *line = ac_alloc((size_t)pre + 1, sizeof(float));
        long cn[8], ci[8] = { 0 }, an[4], ai[4] = { 0 };
        double clp[8] = { 0 };
        for (int c = 0; c < 8; c++) { cn[c] = ac_round((COMB[c] + ch * 23) * scale); cb[c] = ac_alloc((size_t)cn[c], sizeof(float)); }
        for (int a = 0; a < 4; a++) { an[a] = ac_round((AP[a] + ch * 23) * scale); ab[a] = ac_alloc((size_t)an[a], sizeof(float)); }
        for (long i = 0; i < N; i++) {
            const long w = i % (pre + 1);
            const double x = line[w];
            line[w] = rev[i * 2 + ch];
            double y = 0;
            for (int c = 0; c < 8; c++) {
                const double o = cb[c][ci[c]];
                y += o;
                clp[c] = o * (1 - damp) + clp[c] * damp;
                cb[c][ci[c]] = (float)(x + clp[c] * feedback);
                if (++ci[c] == cn[c]) ci[c] = 0;
            }
            for (int a = 0; a < 4; a++) {
                const double o = ab[a][ai[a]];
                ab[a][ai[a]] = (float)(y + o * 0.5); y = o - y;
                if (++ai[a] == an[a]) ai[a] = 0;
            }
            rev[i * 2 + ch] = (float)(y * 0.12 * level);
        }
        for (int c = 0; c < 8; c++) free(cb[c]);
        for (int a = 0; a < 4; a++) free(ab[a]);
        free(line);
    }
}

int main(int argc, char **argv) {
    ac_engine = "wattajetta-cutie";
    const char *scorePath = NULL, *rawPath = NULL;
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--raw") && i + 1 < argc) rawPath = argv[++i];
        else scorePath = argv[i];
    }
    if (!scorePath || !rawPath) ac_die("usage: wattajetta-cutie <score.txt> --raw <out.f32>", NULL);

    FILE *f = fopen(scorePath, "r");
    if (!f) ac_die("cannot open", scorePath);
    char material[64] = "tape", line[4096], *tok[32];
    double amount = 1;
    int under[32], nunder = 0, frozen = 0;
    long events = 0;
    while (fgets(line, sizeof line, f)) {
        const int n = ac_split(line, tok, 32);
        if (!n) continue;
        const char *v = tok[0];
#define ARGS(k) if (n < (k) + 1) ac_die("short line for", v)
#define A(i) ac_num(tok[i])
        if (!strcmp(v, "sr")) { ARGS(1); SR = A(1); }
        else if (!strcmp(v, "material")) { ARGS(2); snprintf(material, sizeof material, "%s", tok[1]); amount = A(2); }
        else if (!strcmp(v, "cfg")) {
            ARGS(2);
            if (ncfg == 96) ac_die("too many cfg lines", NULL);
            snprintf(cfgs[ncfg].name, sizeof cfgs[ncfg].name, "%s", tok[1]); cfgs[ncfg++].v = A(2);
        }
        else if (!strcmp(v, "frames")) {
            ARGS(1);
            N = (long)A(1);
            kickBus = ac_alloc((size_t)N, sizeof(float)); bassBus = ac_alloc((size_t)N, sizeof(float));
            acidBus = ac_alloc((size_t)N, sizeof(float)); dly = ac_alloc((size_t)N, sizeof(float));
            drums = ac_alloc((size_t)N * 2, sizeof(float)); music = ac_alloc((size_t)N * 2, sizeof(float));
            bells = ac_alloc((size_t)N * 2, sizeof(float)); rev = ac_alloc((size_t)N * 2, sizeof(float));
        }
        else if (!strcmp(v, "smp")) { ARGS(2); ac_smp_load(atoi(tok[1]), tok[2]); }
        else if (!strcmp(v, "under")) { ARGS(1); if (nunder < 32) under[nunder++] = atoi(tok[1]); }
        else {
            if (!N) ac_die("an event came before `frames`:", v);
            if (!frozen) { cfg_freeze(); frozen = 1; }
            events++;
            if (!strcmp(v, "kick")) { ARGS(3); kick(A(1), A(2), atoi(tok[3])); }
            else if (!strcmp(v, "spray")) { ARGS(10); spray(A(1), A(2), A(3), atoi(tok[4]), A(5), A(6), A(7), A(8), A(9), A(10)); }
            else if (!strcmp(v, "tick")) { ARGS(4); tick(A(1), A(2), A(3), A(4)); }
            else if (!strcmp(v, "bloop")) { ARGS(5); bloop(A(1), A(2), A(3), A(4), A(5)); }
            else if (!strcmp(v, "bass")) { ARGS(5); bass_note(A(1), A(2), A(3), A(4), atoi(tok[5])); }
            else if (!strcmp(v, "pad")) { ARGS(6); pad_note(A(1), A(2), A(3), A(4), A(5), A(6)); }
            else if (!strcmp(v, "air")) { ARGS(5); air_note(A(1), A(2), A(3), A(4), A(5)); }
            else if (!strcmp(v, "pluck")) { ARGS(5); pluck(A(1), A(2), A(3), A(4), A(5)); }
            else if (!strcmp(v, "lead")) { ARGS(6); lead(A(1), A(2), A(3), A(4), A(5), A(6)); }
            else if (!strcmp(v, "flyby")) { ARGS(2); flyby(A(1), A(2)); }
            else if (!strcmp(v, "riser")) { ARGS(3); riser(A(1), A(2), A(3)); }
            else if (!strcmp(v, "acidchord")) {
                ARGS(6);
                const int nn = atoi(tok[6]);
                double notes[8];
                if (nn < 1 || nn > 8 || n < 7 + nn) ac_die("bad acidchord line", NULL);
                for (int i = 0; i < nn; i++) notes[i] = A(7 + i);
                acid_chord(A(1), A(2), A(3), A(4), A(5), nn, notes);
            }
            else if (!strcmp(v, "acidrow")) {
                ARGS(6);
                const double t0 = A(1), level = A(2), cut0 = A(3), cut1 = A(4), reso = A(5);
                const int nevs = atoi(tok[6]);
                Step *evs = ac_alloc((size_t)nevs, sizeof *evs);
                char sub[256], *st[8];
                for (int i = 0; i < nevs; i++) {
                    if (!fgets(sub, sizeof sub, f) || ac_split(sub, st, 8) < 5 || strcmp(st[0], ".")) ac_die("acidrow is missing a step", NULL);
                    evs[i] = (Step){ ac_num(st[1]), ac_num(st[2]), atoi(st[3]), atoi(st[4]) };
                }
                acid_row(t0, level, cut0, cut1, reso, evs, nevs);
                free(evs);
            }
            else if (!strcmp(v, "wub")) {
                ARGS(11);
                double bm[8];
                for (int i = 0; i < 8; i++) bm[i] = A(4 + i);
                wub_row(A(1), A(2), atoi(tok[3]), bm);
            }
            else if (!strcmp(v, "bell")) { ARGS(6); bell(ac_smp_get(tok[1]), A(2), A(3), A(4), A(5), A(6)); }
            else if (!strcmp(v, "vox")) { ARGS(9); vox(ac_smp_get(tok[1]), A(2), A(3), A(4), A(5), A(6), A(7), A(8), A(9)); }
            else if (!strcmp(v, "sung")) { ARGS(8); sung(ac_smp_get(tok[1]), A(2), A(3), A(4), A(5), A(6), A(7), A(8)); }
            else ac_die("unknown event", v);
        }
#undef ARGS
#undef A
    }
    fclose(f);
    if (!N) ac_die("the score has no `frames` line", NULL);
    if (!frozen) cfg_freeze();

    const double Gkick = cfg("gain.kick"), Gbass = cfg("gain.bass"), Gacid = cfg("gain.acid"), Gdrums = cfg("gain.drums");
    const double Gmusic = cfg("gain.music"), Gbells = cfg("gain.bells");
    for (int i = 0; i < nunder; i++) underwater(under[i]);
    phaser(Gacid / Gmusic);
    ping_pong();
    reverb();

    // Sum, in place on the music bus. The kick opens a hole in everything that isn't the kick.
    float *mix = music;
    const double Dmusic = cfg("duck.music"), Dbass = cfg("duck.bass"), Dbells = cfg("duck.bells"), Dwet = cfg("duck.wet"), Drel = cfg("duck.release");
    const double cr = cfg("crunch"), nrm = 1 / tanh(cr);
    long kp = 0;
    double peak = 0;
    for (long i = 0; i < N; i++) {
        const double t = i / SR;
        while (kp + 1 < nkicks && kicks[kp + 1].t <= t) kp++;
        double d = 0;
        if (nkicks && kicks[kp].t <= t) {
            const double dt = t - kicks[kp].t;
            d = kicks[kp].g * (dt < 0.012 ? dt / 0.012 : exp(-(dt - 0.012) / Drel));
        }
        const double k = tanh(kickBus[i] * cr * 1.6) * nrm * Gkick;
        const double b = tanh(bassBus[i] * cr * Gbass * 1.4) * nrm * (1 - Dbass * d) * 0.7;
        for (int ch = 0; ch < 2; ch++) {
            const long j = i * 2 + ch;
            const double dr = tanh(drums[j] * cr * 2) * nrm * 0.5 * Gdrums * 2;
            const double v = k + b + dr + music[j] * Gmusic * (1 - Dmusic * d)
                + bells[j] * Gbells * (1 - Dbells * d) + rev[j] * (1 - Dwet * d);
            mix[j] = (float)v;
            if (fabs(v) > peak) peak = fabs(v);
        }
    }
    // The record's material: the summed mix is printed to plastic tape before the limiter.
    if (strcmp(material, "none")) {
        const double pre = 0.5 / fmax(peak, 1e-9);           // a known level into the oxide
        for (long j = 0; j < N * 2; j++) mix[j] = (float)(mix[j] * pre);
        if (ac_material_apply(mix, N, SR, material, amount)) ac_die("unknown material", material);
        peak = 0;
        for (long j = 0; j < N * 2; j++) if (fabs(mix[j]) > peak) peak = fabs(mix[j]);
    }
    const double g = 0.89 / fmax(peak, 1e-9);
    for (long j = 0; j < N * 2; j++) mix[j] = (float)(mix[j] * g);

    ac_write_f32(rawPath, mix, N * 2);
    fprintf(stderr, "wattajetta-cutie: %ld events · %.1f s · premaster peak %.3f · material %s\n", events, N / SR, peak, material);
    return 0;
}
