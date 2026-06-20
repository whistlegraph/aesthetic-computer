// boombaboom.c — @jeffrey's "boom-ba-boom" vocal take, sung over the most
// minimal possible techno bed: SINE BELLS + a SINE KICK, nothing else. No
// hats, no claps, no saws, no noise. The whole instrumentation is pure sin()
// (plus a gentle tanh round-off on the kick), so it stays chill and out of the
// way — his original take is the song, the bed just holds a D-minor pocket
// under it.
//
// THE LAW (AC house style, per pop/hellsine/c/hellsine.c): every generated
// sample is a sum of sin() terms or a memoryless tanh of such a sum. Voices
// keep a normalized phase advanced by frequency/SR each tick.
//
// Vocal: his take is autotuned to D minor first (bin/autotune.py note-mode,
// contour preserved → still him, just in key) → sources/boombaboom-tuned.wav,
// which THIS engine loads and lays over the bed. So the render is all C; the
// only preprocessing is the pitch-correction of his own performance.
//
// Build:  bash boombaboom/c/build.sh
// Run:    ./boombaboom --out out/boombaboom-raw.wav --vocal ../sources/boombaboom-tuned.wav

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

static const int SR = 48000;
static double BPMV = 140, BEAT, BAR;

static uint32_t rng_s = 0x626f6f6d; // "boom"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// two buses: the kick is dry; the bells + vocal share a music bus that the
// kick ducks (a gentle sidechain breathe). revL/R is a light bell reverb send.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *trig;
static inline void addD(long i, double l, double r) { if (i >= 0 && i < N) { drumL[i] += (float)l; drumR[i] += (float)r; } }
static inline void addM(long i, double l, double r) { if (i >= 0 && i < N) { musL[i] += (float)l; musR[i] += (float)r; } }
static inline void addR(long i, double l, double r) { if (i >= 0 && i < N) { revL[i] += (float)l; revR[i] += (float)r; } }

static int write_wav_f32_stereo(const char *path, const float *L, const float *R, long n) {
    FILE *f = fopen(path, "wb"); if (!f) return 0;
    uint32_t dsz = (uint32_t)(n * 8), riff = 36 + dsz, sr = SR, br = SR * 8, fsz = 16;
    uint16_t fmt = 3, ch = 2, ba = 8, bits = 32;
    fwrite("RIFF", 1, 4, f); fwrite(&riff, 4, 1, f); fwrite("WAVE", 1, 4, f);
    fwrite("fmt ", 1, 4, f); fwrite(&fsz, 4, 1, f);
    fwrite(&fmt, 2, 1, f); fwrite(&ch, 2, 1, f); fwrite(&sr, 4, 1, f);
    fwrite(&br, 4, 1, f); fwrite(&ba, 2, 1, f); fwrite(&bits, 2, 1, f);
    fwrite("data", 1, 4, f); fwrite(&dsz, 4, 1, f);
    for (long i = 0; i < n; i++) { fwrite(&L[i], 4, 1, f); fwrite(&R[i], 4, 1, f); }
    fclose(f); return 1;
}

// ── WAV loader (mono, resampled to SR) — lifted from hellsine.c ────────────
static float *load_wav_mono(const char *path, long *out_n) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t*)malloc(sz);
    if ((long)fread(buf, 1, sz, f) != sz) { fclose(f); free(buf); return NULL; }
    fclose(f);
    if (sz < 12 || memcmp(buf, "RIFF", 4) || memcmp(buf + 8, "WAVE", 4)) { fprintf(stderr, "bad WAV: %s\n", path); free(buf); return NULL; }
    long p = 12; int format = 0, channels = 0, bits = 0; uint32_t fmtSR = 0; long dOff = 0, dLen = 0;
    while (p + 8 <= sz) {
        const char *id = (const char*)(buf + p);
        uint32_t s = (uint32_t)buf[p+4] | (uint32_t)buf[p+5] << 8 | (uint32_t)buf[p+6] << 16 | (uint32_t)buf[p+7] << 24;
        if (!memcmp(id, "fmt ", 4)) { format = buf[p+8] | (buf[p+9]<<8); channels = buf[p+10] | (buf[p+11]<<8);
            fmtSR = (uint32_t)buf[p+12] | (uint32_t)buf[p+13]<<8 | (uint32_t)buf[p+14]<<16 | (uint32_t)buf[p+15]<<24;
            bits = buf[p+22] | (buf[p+23]<<8); }
        else if (!memcmp(id, "data", 4)) { dOff = p + 8; dLen = s; }
        p += 8 + s + (s & 1);
    }
    if (!channels || !bits || !dOff) { fprintf(stderr, "bad WAV: %s\n", path); free(buf); return NULL; }
    const int fb = (bits / 8) * channels; const long frames = dLen / fb;
    float *mono = (float*)calloc(frames, sizeof(float));
    for (long i = 0; i < frames; i++) {
        double acc = 0;
        for (int c = 0; c < channels; c++) {
            const long o = dOff + i * fb + c * (bits / 8);
            if (format == 3 && bits == 32) { float v; memcpy(&v, buf + o, 4); acc += v; }
            else if (bits == 16) { int16_t v; memcpy(&v, buf + o, 2); acc += (double)v / 32768.0; }
            else if (bits == 24) { int32_t v = buf[o] | (buf[o+1]<<8) | (int32_t)((int8_t)buf[o+2])<<16; acc += (double)v / 8388608.0; }
            else if (bits == 32) { int32_t v; memcpy(&v, buf + o, 4); acc += (double)v / 2147483648.0; }
        }
        mono[i] = (float)(acc / channels);
    }
    free(buf);
    long n = frames;
    if (fmtSR && fmtSR != (uint32_t)SR) {
        const long outN = (long)((double)frames * SR / fmtSR + 0.5);
        float *rs = (float*)calloc(outN, sizeof(float));
        for (long i = 0; i < outN; i++) {
            double sp = (double)i * fmtSR / SR; long i0 = (long)sp; double fr = sp - i0;
            float a = mono[i0 < frames ? i0 : frames - 1], b = mono[i0 + 1 < frames ? i0 + 1 : frames - 1];
            rs[i] = (float)(a + (b - a) * fr);
        }
        free(mono); mono = rs; n = outN;
    }
    *out_n = n; return mono;
}

// ── voices — SINE ONLY ────────────────────────────────────────────────────
// sine kick — pure sine with a fast pitch sweep (96→44 Hz) and a warm body, a
// hair of tanh round-off + a soft sine "click". Round and chill. Stamps the
// sidechain trigger so the bells breathe.
static void kick(double t, double g, double floorf) {
    long s0 = (long)(t * SR), n = (long)(0.42 * SR); double ph = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = floorf + 58 * exp(-tt * 38.0);   // floorf = the "tick"/"tock" pitch
        ph += TAU * pf / SR;
        double amp = exp(-tt * 7.8);                                      // tighter body
        double click = sin(TAU * 2200.0 * tt) * exp(-tt * 320.0) * 0.46;  // brighter, faster click
        double v = tanh((sin(ph) + click) * 1.6) * amp * g;
        addD(s0 + i, v, v);
    }
}

// sine bell — a pure sine struck with a bell envelope (near-instant attack,
// exponential decay), plus a quiet octave shimmer (still a sine) for sparkle.
// Reverb send makes it ring. This is the ONLY melodic/harmonic voice.
static void bell(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, ph2 = 0, ph3 = 0, phi = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.10 * SR);                    // SLOW attack — the bell swells/rolls in
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        // FLANGE on the tail (slow LFO detunes an inharmonic partial → swoosh),
        // HARMONICS (2nd + 3rd + glassy inharmonic 4.2×), LONG decay so the
        // tails bloom + overlap.
        double fl = 1.0 + 0.006 * sin(TAU * 0.5 * tt);
        ph  += TAU * f / SR;
        ph2 += TAU * f * 2.0 / SR;
        ph3 += TAU * f * 3.0 / SR;
        phi += TAU * f * 4.2 * fl / SR;
        double env = exp(-tt * 3.2);                 // long ring-down (was 6.8)
        if (i < att) env *= (double)i / att;
        double tail = exp(-tt * 1.3);
        double v = (sin(ph) + 0.16 * sin(ph2) + 0.09 * sin(ph3) + 0.08 * sin(phi) * tail) * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.36 * lg, v * 0.36 * rg);  // wetter = deeper tails
    }
}

// digital square bell — a decaying pulse wave for the "startup trio" boot-chime
// and digital sparkle (à la amaythingra's square voice). Bright, 8-bit-ish.
static void sqbell(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5; long att = (long)(0.002 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += f / SR; if (ph >= 1) ph -= 1;
        double sq = (ph < 0.5) ? 1.0 : -1.0;
        double env = exp(-tt * 5.5); if (i < att) env *= (double)i / att;
        double v = sq * env * g * 0.42;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.28 * lg, v * 0.28 * rg);
    }
}

// square LEAD — a pulse wave through a resonant lowpass with a CUTOFF SWEEP
// envelope (the filter sweep) and a long release: a digital melodic voice that
// sings its own evolving line (not a static chord stab).
static void sqlead(double note, double t, double dur, double g, double cut0, double cut1, double res, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.02 * SR), rel = (long)(0.18 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += f / SR; if (ph >= 1) ph -= 1;
        double sq = (ph < 0.5) ? 1.0 : -1.0;
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 2.2);       // sweep cut0 → cut1
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = sq - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = low * env * g;
        addM(s0 + i, v * lg, v * rg); addR(s0 + i, v * 0.3 * lg, v * 0.3 * rg);
    }
}

// whistle — the vocal taken ALL THE WAY to a whistle: a near-pure sine with a
// gentle vibrato + a breath of noise, slow attack / long release, drenched in
// reverb. Featured for a few ethereal bars in the breakdown.
static void whistle(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.28 * SR), rel = (long)(0.6 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double vib = 1.0 + 0.006 * sin(TAU * 5.2 * tt);   // soft whistle vibrato
        ph += TAU * f * vib / SR;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double v = (sin(ph) + rnd2() * 0.012) * env * g;  // a breath of noise
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.42 * lg, v * 0.42 * rg);        // very wet = ethereal
    }
}

// sine sub-bell — a low, long-decaying sine that carries the bass root. Still a
// bell (struck sine), just deep, with a little tanh weight. No saw, no noise.
static void subbell(double note, double t, double dur, double g) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0; long att = (long)(0.06 * SR), rel = (long)(0.10 * SR);   // slower roll-in
    for (long i = 0; i < n; i++) {
        ph += TAU * f / SR;
        double env = exp(-((double)i / SR) * 1.3);                          // a touch longer too
        if (i < att) env *= (double)i / att; else if (i > n - rel) env *= fmax(0, (double)(n - i) / rel);
        double v = tanh(sin(ph) * 1.2) * env * g;
        addM(s0 + i, v, v);
    }
}

// chord pad — DEEPER CHORDED SINES: a sustained triad of pure sines in the
// low-mid (octave 3), soft attack + release so it breathes a warm bed under
// the voice. Still strictly sin(). One triad per bar following the roots.
static void chordpad(const int *notes, int count, double t, double dur, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(0.30 * SR), rel = (long)(0.40 * SR);
    double lg = 1 - pan * 0.4, rg = 1 + pan * 0.4;
    double ph[4] = {0};
    for (long i = 0; i < n; i++) {
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double s = 0;
        for (int k = 0; k < count; k++) { ph[k] += TAU * midi_hz(notes[k]) / SR; s += sin(ph[k]); }
        double v = tanh((s / count) * 1.2) * env * g;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.18 * lg, v * 0.18 * rg);
    }
}

// impact — a deep sub-boom for the drop: a long pitch-swept sine that lands
// hard when the kick enters on the first BOOM. Pure sine + tanh weight.
static void impact(double t, double g) {
    long s0 = (long)(t * SR), n = (long)(1.1 * SR); double ph = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 40 + 70 * exp(-tt * 9.0);
        ph += TAU * pf / SR;
        double v = tanh(sin(ph) * 1.5) * exp(-tt * 2.6) * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.25, v * 0.25);
    }
}

// snare — a BANGING backbeat: a punchy tonal body (~185 Hz) + a bright
// bandpassed noise crack, tanh-saturated for weight. Sends to the verb.
static void snare(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.24 * SR);
    double ph = 0, low = 0, band = 0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        ph += TAU * 185.0 / SR;
        double body = sin(ph) * exp(-tt * 32.0) * 0.55;
        double hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 3200.0 / SR), q = 1.0 / 0.85;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double crack = band * exp(-tt * 15.0);
        double v = tanh((body + crack) * 1.5) * g;
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.22, v * 0.22);
    }
}

// reverse WUB kick — a DEEP swelling sub-whoosh that crescendos INTO time t
// (ends at t). A slow pitch WUB (LFO on a low ~34→52 Hz sub) + tanh weight =
// a deep wubby reverse. Anticipation = more swing.
static void revkick(double t, double dur, double g) {
    long s1 = (long)(t * SR), n = (long)(dur * SR), s0 = s1 - n; double ph = 0, prev = 0;
    for (long i = 0; i < n; i++) {
        double p = (double)i / n;                     // 0→1 over the swell (reversed env)
        double tt = (double)i / SR;
        double wub = 34 + 18 * p + 7 * sin(TAU * 6.0 * tt);   // deep sub + wobble
        ph += TAU * wub / SR;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double v = (tanh(sin(ph) * 1.5) * 0.82 + hp * 0.18) * p * p * g;
        addM(s0 + i, v, v); addR(s0 + i, v * 0.3, v * 0.3);
    }
}

// perc — a tight bandpassed click for skippy shuffle ghost-hits.
static void perc(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.04 * SR);
    double prev = 0, low = 0, band = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * 2600.0 / SR), q = 1.0 / 1.5;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = band * exp(-tt * 60.0) * g * 0.6;
        addD(s0 + i, v * lg, v * rg); addR(s0 + i, v * 0.2, v * 0.2);
    }
}

// reverse bell — a bell ringing UP into the hit (reversed envelope), drenched.
static void revbell(double note, double t, double dur, double g, double pan) {
    long s1 = (long)(t * SR), n = (long)(dur * SR), s0 = s1 - n;
    double f = midi_hz(note), ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double p = (double)i / n; ph += TAU * f / SR;
        double v = (sin(ph) + 0.2 * sin(ph * 2)) * p * p * p * g;
        addM(s0 + i, v * lg, v * rg); addR(s0 + i, v * 0.45 * lg, v * 0.45 * rg);
    }
}

// hat — a soft closed hat / shaker. LOW-PITCHED: the noise is lowpassed to a
// dark "shh" (not a bright "tss") so it sits down out of the way.
static void hat(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.05 * SR); double low = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, nz = rnd2();
        low += 0.32 * (nz - low);                    // lowpass → darker, lower-pitched
        double v = low * exp(-tt * 120.0) * g * 0.30;
        addD(s0 + i, v * lg, v * rg);
    }
}

// sweep — a filtered-noise riser whose bandpass climbs (400 Hz → 6 kHz) and
// swells across `dur`, building tension into a section. Goes through the verb.
static void sweep(double t, double dur, double g) {
    long s0 = (long)(t * SR), n = (long)(dur * SR); double prev = 0, low = 0, band = 0;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, p = tt / dur;
        double nz = rnd2(), hp = nz - prev; prev = nz;
        double fc = 2.0 * sin(M_PI * (400 + 6000 * p) / SR), q = 1.0 / 1.3;
        double high = hp - low - q * band; band += fc * high; low += fc * band;
        double v = band * p * p * g;
        addM(s0 + i, v, v);
        addR(s0 + i, v * 0.4, v * 0.4);
    }
}

// ── D-minor harmony ────────────────────────────────────────────────────────
// Roots walk i–VI–III–VII (Dm–Bb–F–C) over 4 bars — the same pole his sung
// melody orbits. Bell arpeggio = chord tones up the octave; sub-bell = root.
static const int ROOT[4]   = { 38, 34, 41, 36 };                 // D2 Bb1 F2 C2 (sub-bell)
static const int ARP[4][4] = { {62,65,69,74}, {58,62,65,70}, {65,69,72,77}, {60,64,67,72} };
//                              Dm: D4 F4 A4 D5  Bb: Bb3 D4 F4 Bb4  F: F4 A4 C5 F5  C: C4 E4 G4 C5
// deep triads (octave 3) for the chord pad — Dm, Bb, F, C.
static const int CHORD[4][3] = { {50,53,57}, {46,50,53}, {53,57,60}, {48,52,55} };
// square-lead melody — a low D-minor line in the VOCAL'S register (oct 3–4) so
// it pitch-matches and sits UNDER the voice. Dm·Bb·F·C chord/melody tones.
static const int SQMEL[4][4] = { {50,57,62,65}, {46,53,58,62}, {53,60,57,65}, {48,55,60,64} };
//                                Dm:D3 A3 D4 F4  Bb:Bb2 F3 Bb3 D4  F:F3 C4 A3 F4  C:C3 G3 C4 E4

// place a mono vocal layer onto the music bus: a one-pole ~110 Hz highpass
// clears its rumble out of the kick's lane, gain + pan position it, and its
// first onset (after `trim` lead-in) lands on bar `startBar`'s downbeat.
// ~110 Hz one-pole highpass (clear the kick lane) + a gentle ~6.5 kHz one-pole
// lowpass that ROUNDS OFF the gritty top so the voice is smooth/deep, not
// textured-and-forward.
#define HP_A 0.985
#define LP_C 0.34    // darker roll-off → soft, rounded, whistle-like (not textured)
static void place_layer(float *buf, long bn, long trim, int startBar, double g, double pan, double rev) {
    if (!buf || g <= 0) return;
    long off = (long)(startBar * BAR * SR) - trim;
    double lg = (1 - pan * 0.5) * g, rg = (1 + pan * 0.5) * g, yp = 0, xp = 0, lp = 0;
    for (long i = trim; i < bn; i++) {
        double x = buf[i], y = HP_A * (yp + x - xp); xp = x; yp = y;
        lp += LP_C * (y - lp);                       // smooth the top = less texture
        long j = off + i;
        if (j >= 0 && j < N) {
            musL[j] += (float)(lp * lg); musR[j] += (float)(lp * rg);
            if (rev > 0) addR(j, lp * lg * rev, lp * rg * rev);
        }
    }
}

// place a layer at a fractional-bar position (for scattered prelude fragments).
static void place_at(float *buf, long bn, long trim, double atSec, double g, double pan, double rev) {
    if (!buf || g <= 0) return;
    const double hp_a = 0.985;
    long off = (long)(atSec * SR) - trim;
    double lg = (1 - pan * 0.5) * g, rg = (1 + pan * 0.5) * g, yp = 0, xp = 0;
    for (long i = trim; i < bn; i++) {
        double x = buf[i], y = hp_a * (yp + x - xp); xp = x; yp = y;
        long j = off + i;
        if (j >= 0 && j < N) {
            musL[j] += (float)(y * lg); musR[j] += (float)(y * rg);
            if (rev > 0) addR(j, y * lg * rev, y * rg * rev);
        }
    }
}

int main(int argc, char **argv) {
    const char *out_path = "out/boombaboom-raw.wav";
    const char *vocal_path = "../sources/boombaboom-aligned.wav";
    const char *harm5_path = "../sources/boombaboom-harm5.wav";   // a fifth up
    const char *harmo_path = "../sources/boombaboom-harmoct.wav"; // an octave up
    const char *harm3_path = "../sources/boombaboom-harm3.wav";   // a diatonic third up
    const char *harmdn_path = "../sources/boombaboom-harmdn.wav"; // an octave DOWN (for the ending)
    const char *osc_path = "../sources/boombaboom-osc.wav";       // the "ooowwoowwoo" feature
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--out") && i + 1 < argc) out_path = argv[++i];
        else if (!strcmp(argv[i], "--vocal") && i + 1 < argc) vocal_path = argv[++i];
        else if (!strcmp(argv[i], "--harm5") && i + 1 < argc) harm5_path = argv[++i];
        else if (!strcmp(argv[i], "--harmoct") && i + 1 < argc) harmo_path = argv[++i];
        else if (!strcmp(argv[i], "--harm3") && i + 1 < argc) harm3_path = argv[++i];
        else if (!strcmp(argv[i], "--harmdn") && i + 1 < argc) harmdn_path = argv[++i];
        else if (!strcmp(argv[i], "--osc") && i + 1 < argc) osc_path = argv[++i];
        else if (!strcmp(argv[i], "--bpm") && i + 1 < argc) BPMV = atof(argv[++i]);
    }
    BEAT = 60.0 / BPMV; BAR = BEAT * 4;

    // ── load the beat-aligned, autotuned take + harmony layers ────────────
    long voxN = 0; float *vox = load_wav_mono(vocal_path, &voxN);
    if (!vox) { fprintf(stderr, "✗ could not load vocal: %s\n", vocal_path); return 1; }
    long h5N = 0, hoN = 0, h3N = 0, hdN = 0, oscN = 0;
    float *h5 = load_wav_mono(harm5_path, &h5N);     // may be NULL → skipped
    float *ho = load_wav_mono(harmo_path, &hoN);
    float *h3 = load_wav_mono(harm3_path, &h3N);
    float *hd = load_wav_mono(harmdn_path, &hdN);
    float *osc = load_wav_mono(osc_path, &oscN);
    long oscuN = 0, oscdN = 0, boomN = 0, maN = 0;
    float *oscu = load_wav_mono("../sources/boombaboom-osc-up.wav", &oscuN);
    float *oscd = load_wav_mono("../sources/boombaboom-osc-dn.wav", &oscdN);
    float *boomS = load_wav_mono("../sources/boombaboom-boom.wav", &boomN);
    float *maS = load_wav_mono("../sources/boombaboom-ma.wav", &maN);
    fprintf(stderr, "# samples: oscu=%s oscd=%s boom=%s ma=%s\n",
            oscu?"ok":"MISS", oscd?"ok":"MISS", boomS?"ok":"MISS", maS?"ok":"MISS");
    double voxSec = (double)voxN / SR;
    // trim leading near-silence so the take's first onset can sit on a downbeat.
    long vox0 = 0; { double thr = 0.02; while (vox0 < voxN && fabs(vox[vox0]) < thr) vox0++; if (vox0 > (long)(0.05*SR)) vox0 -= (long)(0.02*SR); else vox0 = 0; }

    // ── ARRANGEMENT (bars) — stretched out: 3 passes with breakdowns ───────
    // cold-open OSC hook → pass1 (kick FADES IN) → bridge → pass2 → bridge →
    // pass3 (octave LOWER, the true ending) → outro. Short intro so the voice
    // starts soon. Each pass gets 20 bars so the take floats freely (only the
    // booms anchor to the kick).
    // PRELUDE = lingering "lost vocal" ambient (no beat) · INTRO = build/sweep ·
    // then the drop on the first BOOM. Start lingers; kick hits strong at PB0.
    const int PRELUDE = 8, INTRO = 8, PASS = 26, BR = 8, OUTRO = 10;   // longer passes fit the stretched vocal
    const int NP = 3;
    const int DROP = PRELUDE + INTRO;                     // first BOOM / strong kick / sweep ends
    int passBar[3] = { DROP, DROP + PASS + BR, DROP + PASS + BR + PASS + BR };
    const int TB = passBar[2] + PASS + OUTRO;                  // total bars
    double totalSec = TB * BAR + 2.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# boombaboom.c · %g BPM · %d bars · %.1fs · sine bells + kick + choral + hats\n", BPMV, TB, totalSec);
    fprintf(stderr, "# take %.1fs (trim %.2fs) · passes @ bars %d %d %d\n", voxSec, (double)vox0/SR, passBar[0], passBar[1], passBar[2]);

    // ── place the take + a CHORAL harmony stack on each pass ───────────────
    // The lead IS the song: loud + centred. A chord builds across passes —
    // octave + fifth + a diatonic third = a full Dm chord of his own voice,
    // spread L↔R. On the LAST pass the stack drops to the octave BELOW (the
    // grounded "true ending") instead of the bright octave above.
    //                              pass0  pass1  pass2(end)
    const double G_OUP[3] = { 0.70, 0.90, 0.28 };   // octave UP (pulled back at the end)
    const double G_ODN[3] = { 0.55, 0.65, 1.00 };   // octave DOWN — a deep "throat" double UNDER the lead, all passes
    const double G_FIF[3] = { 0.50, 0.85, 0.85 };
    const double G_THR[3] = { 0.00, 0.55, 0.80 };
    for (int pass = 0; pass < NP; pass++) {
        int sb = passBar[pass];
        // vocals sit DEEP and SOFT — pulled back, darkened to a whistle-like
        // tone, with LOTS of reverb/space so they're embedded, not out front.
        // A deep octave-down "throat" voice always sits UNDER the main one.
        place_layer(vox, voxN, vox0, sb, 1.45,        0.00, 0.50);  // lead — centre, deep + soft
        place_layer(hd,  hdN,  vox0, sb, G_ODN[pass] * 0.80, 0.00, 0.40);  // deep throat — UNDER, centre
        place_layer(ho,  hoN,  vox0, sb, G_OUP[pass] * 0.70, 0.20, 0.46);  // octave up
        place_layer(h5,  h5N,  vox0, sb, G_FIF[pass] * 0.70, -0.45, 0.48); // fifth — left
        place_layer(h3,  h3N,  vox0, sb, G_THR[pass] * 0.70, 0.45, 0.48);  // third — right
    }
    // LOST-VOCAL PRELUDE — a SINGLE lost-vocal phrase, sung at three octaves
    // AT THE SAME TIME (no confusing time-offsets): the wobble + its octave-up
    // and octave-down, stacked in sync and drenched in reverb. One coherent
    // lost voice in a haze, then the build. (A second, quieter restatement near
    // the end of the prelude leads into the sweep.)
    {
        // SOUND FROM THE START — the VOCAL itself opens the track (no startup
        // chime): the phrase + its deep octave-down, CENTRED, soft + very wet,
        // landing almost immediately and restated once.
        double at1 = 0.12 * BAR, at2 = 4.0 * BAR;
        for (int pass = 0; pass < 2; pass++) {
            double at = pass ? at2 : at1, g = pass ? 0.30 : 0.42;
            place_at(osc,  oscN,  0, at, g,        0.00, 0.95);   // the phrase, centre, drenched
            place_at(oscd, oscdN, 0, at, g * 0.60, 0.00, 0.90);   // octave down — centre, deep
        }
    }
    // SQUARE-WAVE STARTUP TRIO — a digital boot-chime that lands WITH THE KICK
    // at the drop (not at t=0), a LOWER octave (D3·A3·D4) so it sits under the
    // voice: three ascending pulse notes across the first beat of the drop.
    {
        double d0 = passBar[0] * BAR;
        sqbell(50, d0,                 0.50, 0.55, -0.25);   // D3
        sqbell(57, d0 + BEAT * 0.5,    0.50, 0.55, 0.05);    // A3
        sqbell(62, d0 + BEAT * 1.0,    1.00, 0.60, 0.25);    // D4
    }
    // OSC FEATURE — the "ooowwoowwoo" wobble as a hook in each bridge breakdown.
    place_layer(osc, oscN, 0, passBar[0] + PASS,  1.5, -0.2, 0.4);  // bridge 1
    place_layer(osc, oscN, 0, passBar[1] + PASS,  1.5, 0.2, 0.4);   // bridge 2

    // ANGELS — a high, airy octave-up vocal swelling in on the back half to lift
    // the track, very wet + panned wide (alongside the deep throat below).
    place_layer(ho, hoN, vox0, passBar[1], 0.45, -0.55, 0.60);
    place_layer(ho, hoN, vox0, passBar[2], 0.58, 0.55, 0.62);

    // BOOM BOOM BOOM BOOM — four single-beat stretched booms as a fill leading
    // into the final drop (the bar before pass 3).
    {
        double b0 = (passBar[2] - 1) * BAR;
        for (int k = 0; k < 4; k++)
            place_at(boomS, boomN, 0, b0 + k * BEAT, 0.7, (k & 1) ? 0.2 : -0.2, 0.30);
    }
    free(vox); free(h5); free(ho); free(h3); free(hd); free(osc); free(oscu); free(oscd);

    // ── HEALING SINE SWARM — his voice re-cast as a swarm of sustained sines ─
    // ONE healing frequency (432 Hz) hums throughout; over it a cloud of gently
    // detuned sines tuned to the current chord tones "dips pitches into the
    // vocal." The whole swarm's loudness FOLLOWS his vocal energy (fast-attack /
    // slow-release follower) read from a DELAYED tap — a deep echo that bounces
    // off his phrasing. Voices spread across the stereo field.
    {
        float *env = calloc(N, 4);
        double e = 0, atk = exp(-1.0 / (0.012 * SR)), rel = exp(-1.0 / (0.50 * SR));
        for (long i = 0; i < N; i++) {                       // follower on the placed vocal
            double v = (fabs(musL[i]) + fabs(musR[i])) * 0.5;
            e = (v > e) ? atk * e + (1 - atk) * v : rel * e + (1 - rel) * v;
            env[i] = (float)e;
        }
        long dly = (long)(BEAT * 1.5 * SR);                  // deep echo tap
        double phH = 0, phHs = 0;                            // 432 + 216 healing drone
        double phc[3] = {0}, phd[3] = {0};                   // chord-tone swarm (+detuned twin)
        for (long i = 0; i < N; i++) {
            long di = i - dly; double ev = di >= 0 ? env[di] : 0.0;
            double drive = 0.35 * env[i] + 1.0 * ev;         // present + echoed
            int bar = (int)(i / (BAR * SR)); int phr = bar % 4;
            // healing drone — always there, very soft, steady
            phH  += TAU * 432.0 / SR; phHs += TAU * 216.0 / SR;
            double heal = (sin(phH) * 0.5 + sin(phHs) * 0.5) * 0.030;
            // chord-tone swarm — three sines (mid octave of the bar's triad) +
            // a detuned twin each, panned out, gated by the delayed energy.
            double sl = 0, sr = 0;
            for (int k = 0; k < 3; k++) {
                double f = midi_hz(CHORD[phr][k] + 12);      // an octave up = airy
                phc[k] += TAU * f / SR; phd[k] += TAU * (f * 1.006) / SR;
                double s = (sin(phc[k]) + sin(phd[k])) * 0.5;
                double pan = (k - 1) * 0.7;
                double amp = s * drive * 0.085;
                sl += amp * (1 - pan * 0.5); sr += amp * (1 + pan * 0.5);
            }
            // swarm plays from the start now (instrumentation from the open).
            musL[i] += (float)(heal + sl); musR[i] += (float)(heal + sr);
            addR(i, (heal + sl) * 0.3, (heal + sr) * 0.3);
        }
        free(env);
    }

    // ── POWER SINES — deep sustained root+fifth that BUILD through the track,
    // strongest under the harder second half (a slow swell in the back) ──────
    {
        double phD = 0, phA = 0;
        for (long i = 0; i < N; i++) {
            int bar = (int)(i / (BAR * SR));
            double build = fmin(1.0, fmax(0.0, (bar - passBar[1]) / 24.0));  // ramp in over pass2
            phD += TAU * midi_hz(26) / SR;                   // D1
            phA += TAU * midi_hz(33) / SR;                   // A1 (fifth)
            double v = (tanh(sin(phD) * 1.3) * 0.7 + sin(phA) * 0.3) * build * 0.10;
            musL[i] += (float)v; musR[i] += (float)v;
        }
    }

    // ── SOFT LOW SQUARE BED — a warm low pulse-wave drone (D pedal) humming
    // under the whole track, heavily lowpassed so it's a soft buzz, not harsh ─
    {
        double ph = 0, lp = 0;
        for (long i = 0; i < N; i++) {
            ph += midi_hz(38) / SR; if (ph >= 1) ph -= 1;        // D2 pedal
            double sq = (ph < 0.5) ? 1.0 : -1.0;
            lp += 0.018 * (sq - lp);                             // ~heavy LP → soft
            double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (2 * BAR)));
            double v = lp * 0.075 * pg;                          // out of the opening
            musL[i] += (float)v; musR[i] += (float)v;
        }
    }

    // ── GRANULAR HISS — an airy bed of tiny filtered-noise grains scattered
    // across the whole track (tape-air atmosphere), drifting slowly L↔R ──────
    {
        double prev = 0; long gstart = 0, gend = 0; double gdur = 1, gamp = 0;
        for (long i = 0; i < N; i++) {
            if (i >= gend) {                                     // retrigger a grain
                gstart = i; gdur = (0.02 + rnd() * 0.07) * SR;
                gamp = 0.4 + rnd() * 0.6;
                gend = i + (long)gdur + (long)(rnd() * 0.05 * SR); // grain + small gap
            }
            double nz = rnd2(), hp = nz - prev; prev = nz;       // highpass = hiss
            double tt = (double)(i - gstart) / gdur;
            double env = (tt < 1.0) ? sin(M_PI * tt) : 0.0;      // grain window
            double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (2 * BAR)));
            double v = hp * env * gamp * 0.020 * pg;             // very soft, out of the opening
            double pan = sin((double)i * 0.00007) * 0.5;         // gentler drift (less panning)
            musL[i] += (float)(v * (1 - pan * 0.4)); musR[i] += (float)(v * (1 + pan * 0.4));
            addR(i, v * 0.2, v * 0.2);
        }
    }

    // ── FAINT DEEP RAINFOREST RAIN (freesound) — looped, very faint, slowly
    // "pitched around" (drifting playback rate) for a living, dreamy ambience.
    // Held out of the opening like the other beds. (Skipped if the file is
    // missing — re-render once it's fetched.) ──────────────────────────────
    {
        long rainN = 0; float *rain = load_wav_mono("../sources/rain.wav", &rainN);
        if (rain && rainN > SR) {
            double pos = 0;
            for (long i = 0; i < N; i++) {
                double rate = 1.0 + 0.05 * sin(TAU * 0.04 * i / SR);   // slow pitch drift
                pos += rate; if (pos >= rainN - 1) pos -= (rainN - 1);
                long i0 = (long)pos; double fr = pos - i0;
                double s = rain[i0] * (1 - fr) + rain[(i0 + 1 < rainN) ? i0 + 1 : i0] * fr;
                double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (2 * BAR)));
                double v = s * 0.045 * pg;                            // very faint
                musL[i] += (float)v; musR[i] += (float)v;
                addR(i, v * 0.3, v * 0.3);
            }
            free(rain);
        }
    }

    // ── LA HELICOPTER flyby (freesound) — a close chopper passes overhead,
    // panning L→R, at a couple of dramatic moments (into bridge 1 + into the
    // final drop). Skipped if the file is missing. ──────────────────────────
    {
        long heliN = 0; float *heli = load_wav_mono("../sources/heli.wav", &heliN);
        if (heli && heliN > SR) {
            double when[2] = { (passBar[0] + PASS) * BAR, (passBar[2] - 3) * BAR };
            double gain[2] = { 0.16, 0.22 };
            for (int w = 0; w < 2; w++) {
                long off = (long)(when[w] * SR);
                for (long i = 0; i < heliN; i++) {
                    long j = off + i; if (j < 0 || j >= N) continue;
                    double p = (double)i / heliN;             // 0→1 = flies across
                    double pan = p * 2 - 1;                    // L→R sweep
                    double v = heli[i] * gain[w];
                    musL[j] += (float)(v * (1 - pan * 0.5)); musR[j] += (float)(v * (1 + pan * 0.5));
                    addR(j, v * 0.25, v * 0.25);
                }
            }
            free(heli);
        }
    }

    // ── the sine bed ──────────────────────────────────────────────────────
    for (int bar = 0; bar < TB; bar++) {
        int phr = bar % 4;
        int pass = -1;
        for (int p = 0; p < NP; p++) if (bar >= passBar[p] && bar < passBar[p] + PASS) pass = p;
        int inPass = pass >= 0;
        int last   = (pass == NP - 1);                          // the final verse
        int inPrelude = (bar < PRELUDE);                        // lost-vocal haze
        int inOutro = (bar >= passBar[NP - 1] + PASS);
        int inBridge = (!inPass && !inOutro && bar >= passBar[0]); // gaps BETWEEN passes
        int boct = last ? -12 : 0;                              // bells drop an octave at the end
        // GO HARDER once the subtlety wears off: pass 0 sits back, later passes push.
        double intens = (pass <= 0) ? 0.85 : 1.0 + 0.16 * pass;
        // INTELLIGENT PANNING — a slow stereo LFO the moving voices ride. Kept
        // gentle (less panning movement overall).
        double panLFO = sin(TAU * (bar % 8) / 8.0) * 0.6;

        // SWING — push the offbeat 8ths late for groove. swung() takes an 8th
        // index (0..7); odd = offbeat = nudged back by SWING of an 8th.
        #define SWING 0.18
        #define swung(b8) (bar * BAR + (b8) * (BEAT / 2.0) + (((b8) & 1) ? SWING * (BEAT / 2.0) : 0.0))
        // HUMANIZE — a few ms of random timing scatter on every hit so nothing
        // lands dead-on the grid (live, breathing feel).
        #define hum ((rnd() - 0.5) * 0.013)

        // kick — STRONG on the first BOOM (the drop), no fade. TICK-TOCK pitch:
        // tock = D (low, beats 0&2), tick = A (a fifth up, beats 1&3) — both
        // tuned to the vocal's key so the boom sits close to the voice.
        int kickOn = inPass || (inOutro && bar < passBar[NP - 1] + PASS + 4);
        if (bar == passBar[0]) impact(bar * BAR, 0.95);         // the drop hit
        if (kickOn)
            for (int beat = 0; beat < 4; beat++) {
                if (bar % 8 == 6 && beat == 3) continue;        // SKIP a kick for a stutter
                double floorf = (beat & 1) ? 55.0 : 38.0;       // A (tick) / D (tock)
                kick(bar * BAR + beat * BEAT + hum, 0.74 * fmin(1.18, intens), floorf);
                // a syncopated ghost-kick "skip" pushes some bars forward
                if (bar % 4 == 2 && beat == 1) kick(bar * BAR + beat * BEAT + BEAT * 0.5 + hum, 0.40, 40.0);
            }

        // TIGHT closed hat — centred, on the offbeat 8ths, no swing wobble, no
        // panning. Simple and locked.
        double hatg = (inPass ? 1.0 : 0.0) * (0.30 + 0.06 * (pass > 0 ? pass : 0));
        if (hatg > 0)
            for (int st = 1; st < 8; st += 2)
                hat(bar * BAR + st * (BEAT / 2.0) + hum, hatg, 0.0);

        // sub-bell — root once per bar, from the very start (softer in prelude).
        subbell(ROOT[phr], bar * BAR + hum, BAR * (phr == 0 ? 1.4 : 1.0), inPrelude ? 0.16 : 0.26);

        // deep chorded sine pad — warm bed, present (soft) from the start.
        chordpad(CHORD[phr], 3, bar * BAR, BAR * 1.02,
                 inPrelude ? 0.09 : (inBridge ? 0.20 : 0.13), (phr & 1) ? 0.22 : -0.22);

        // bell arpeggio — from the INTRO build onward (no bells in the prelude).
        // Gentle pluck on the 8ths; busier + brighter on later passes, dropped
        // an octave on the final (grounded) verse.
        {   // sine bells play from the very start (sparser/softer in the prelude)
            const int *arp = ARP[phr];
            const char *pat = (pass >= 1) ? "x.x.x.x." : "x..x..x.";
            int ai = 0;
            for (int st = 0; st < 8; st++) {
                if (pat[st] != 'x') continue;
                int note = arp[ai % 4] + boct + ((pass == 1) && (st % 4 == 2) ? 12 : 0);
                ai++;
                double bg = ((inBridge ? 0.22 : 0.18) + (pass >= 1 ? 0.04 : 0.0)) * fmin(1.2, intens);
                if (inPrelude) bg *= 0.7;                       // gentler in the opening
                double bpan = panLFO * 0.45 + ((st & 2) ? 0.25 : -0.25);   // movement
                bell(note, swung(st) + hum, BEAT * 4.0, bg, bpan);   // SWUNG + humanized + long ring
            }
            if (inBridge) bell(ARP[phr][3] + 12, bar * BAR + 2 * BEAT, BEAT * 4.0, 0.16, -panLFO * 0.5);
            // DIGITAL square bell sparkle — a bright accent on the phrase
            // downbeat in the passes (echoes the startup-trio timbre).
            // SQUARE LEAD MELODY — a long, evolving digital line (its OWN melody,
            // two long legato notes every bar — not a 4-bar stab) with a FILTER
            // SWEEP per note; the cutoff creeps open across the track so it morphs.
            if (inPass) {
                // DRIBBLY, eager square line UNDER the voice — an irregular,
                // playful 16th pattern (on-off-on-off-on-on-on-off + "dash"
                // longer notes), nudged EARLY/eager + humanized, a touch more
                // resonant bite. Darker + quiet so it stays inside the mix.
                double openness = 520 + 1200 * fmin(1.0, (double)(bar - passBar[0]) / 40.0);
                const char *SQ[2] = { "x.xxx.x.x.x.xx..", "x.x.xx.xx..x.x.x" }; // dribbly, flips per 2 bars
                const char *pp = SQ[(bar / 2) % 2];
                int li = 0;
                for (int s = 0; s < 16; s++) {
                    if (pp[s] != 'x') continue;
                    double t = bar * BAR + s * (BEAT / 4.0) - 0.014 + hum;       // eager (ahead) + human
                    double dur = (s % 8 == 4) ? BEAT * 1.3 : BEAT * 0.6;          // "dash" long notes vs dribbles
                    int note = SQMEL[phr][(bar + li) % 4]; li++;
                    sqlead(note, t, dur, 0.085, 380, openness, 3.0, (s & 2) ? 0.25 : -0.25);
                }
            }
        }

        // BANGING SNARE — backbeat (beats 2 & 4) once the track goes harder.
        if (inPass && pass >= 1)
            for (int b2 = 1; b2 < 4; b2 += 2) snare(bar * BAR + b2 * BEAT + hum, 0.62 * intens, (rnd() - 0.5) * 0.3);

        // REVERSE swells for SWING — a reverse kick whooshes into the downbeat of
        // the 2nd & 4th bar of each phrase; a reverse bell rises into phrase tops.
        if (inPass && (phr == 1 || phr == 3)) revkick(bar * BAR, BEAT * 1.5, 0.50 * intens);
        if (inPass && phr == 0)             revbell(ARP[phr][2], bar * BAR, BEAT * 2.0, 0.13, -panLFO * 0.4);

        // WHISTLE FEATURE — the breakdown before the final drop goes FULL
        // ethereal: a pure whistle climbs the chord, drenched, taking the
        // soft/deep/whistle direction all the way for these few bars.
        int inBridge2 = (!inPass && !inOutro && bar >= passBar[1] + PASS && bar < passBar[2]);
        if (inBridge2) {
            whistle(CHORD[phr][2] + 12, bar * BAR, BAR * 1.7, 0.34, sin(TAU * (bar % 4) / 4.0) * 0.45);
            if (phr == 2) whistle(CHORD[phr][1] + 24, bar * BAR + 2 * BEAT, BAR * 1.2, 0.20, -0.35);
        }

        // filter-sweep risers. The FIRST one resolves on the DROP (the first
        // BOOM): it climbs across the 4 bars before DROP and ends as the kick
        // hits. Later passes get a 2-bar sweep into the pass downbeat.
        if (bar == passBar[0] - 4) sweep((passBar[0] - 4) * BAR, 4 * BAR, 0.11);   // pulled back
        for (int p = 1; p < NP; p++)
            if (bar == passBar[p] - 2) sweep(bar * BAR, 2 * BAR, 0.08 + 0.02 * p);
    }

    // ── scattered single-beat BOOM / MA vocal hits — rhythmic accents placed
    // in time across the passes (the boom-ba-boom syllables, sprinkled). ──────
    {
        for (int bar = passBar[0]; bar < passBar[NP - 1] + PASS; bar++) {
            int inP = 0; for (int p = 0; p < NP; p++) if (bar >= passBar[p] && bar < passBar[p] + PASS) inP = 1;
            if (!inP) continue;
            if (bar % 4 == 2 && boomS) place_at(boomS, boomN, 0, bar * BAR + 1 * BEAT, 0.5, -0.3, 0.40);
            if (bar % 4 == 3 && maS)   place_at(maS,   maN,   0, bar * BAR + 3 * BEAT, 0.45, 0.3, 0.45);
            if (bar % 8 == 5 && boomS) place_at(boomS, boomN, 0, bar * BAR + 2 * BEAT + 0.25 * BEAT, 0.42, 0.2, 0.40);
        }
    }
    free(boomS); free(maS);

    // ── sidechain: the kick ducks the whole music bus (bells + chord + VOCAL)
    // so the voice pumps WITH the beat and sits in the pocket, not floating ──
    { double depth = 0.45, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── light Schroeder reverb on the bell send ──────────────────────────────
    {
        double decay = 0.84, wet = 0.44, damp = 0.38;   // more space / longer tail
        int CD[4]; double cds[4] = { 0.0297, 0.0353, 0.0431, 0.0497 };
        for (int k = 0; k < 4; k++) CD[k] = (int)(cds[k] * SR);
        float *cbL[4], *cbR[4]; int ci[4] = {0}; double lpL[4] = {0}, lpR[4] = {0};
        for (int k = 0; k < 4; k++) { cbL[k] = calloc(CD[k], 4); cbR[k] = calloc(CD[k], 4); }
        for (long i = 0; i < N; i++) {
            double inL = revL[i], inR = revR[i], cL = 0, cR = 0;
            for (int k = 0; k < 4; k++) {
                double dL = cbL[k][ci[k]], dR = cbR[k][ci[k]]; cL += dL; cR += dR;
                lpL[k] = dL * (1 - damp) + lpL[k] * damp; lpR[k] = dR * (1 - damp) + lpR[k] * damp;
                cbL[k][ci[k]] = (float)(inL + lpL[k] * decay); cbR[k][ci[k]] = (float)(inR + lpR[k] * decay);
                ci[k] = (ci[k] + 1) % CD[k];
            }
            musL[i] += (float)(cL / 4 * wet); musR[i] += (float)(cR / 4 * wet);
        }
        for (int k = 0; k < 4; k++) { free(cbL[k]); free(cbR[k]); }
    }

    // ── mix, normalize, fades ────────────────────────────────────────────────
    float *busL = drumL, *busR = drumR;
    for (long i = 0; i < N; i++) { busL[i] += musL[i]; busR[i] += musR[i]; }
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.92 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(0.4 * SR), fout = (long)(2.2 * SR);
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) { double g = (double)(fout - i) / fout; long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g; }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
