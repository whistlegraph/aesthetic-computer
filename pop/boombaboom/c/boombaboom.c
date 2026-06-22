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
static int SINES = 0;   // --sines: pure-sine variant (no square/noise/samples in the bed)

static uint32_t rng_s = 0x626f6f6d; // "boom"
static inline double rnd(void) { rng_s ^= rng_s << 13; rng_s ^= rng_s >> 17; rng_s ^= rng_s << 5; return (double)rng_s / 4294967296.0; }
static inline double rnd2(void) { return rnd() * 2.0 - 1.0; }
static inline double midi_hz(double m) { return 440.0 * pow(2.0, (m - 69.0) / 12.0); }

static long N;
// two buses: the kick is dry; the bells + vocal share a music bus that the
// kick ducks (a gentle sidechain breathe). revL/R is a light bell reverb send.
static float *drumL, *drumR, *musL, *musR, *revL, *revR, *trig;
static float *vocL, *vocR;   // vocals on their own bus (kick-synced wub sidechain + comp)
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
    long s0 = (long)(t * SR), n = (long)(0.50 * SR); double ph = 0, phs = 0;
    if (s0 >= 0 && s0 < N) trig[s0] = 1.0f;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = floorf + 70 * exp(-tt * 34.0);   // deeper start, big pitch drop
        ph += TAU * pf / SR;
        phs += TAU * (floorf * 0.66) / SR;           // a pure DEEP sub layer under it
        double amp = exp(-tt * 5.6);                                      // longer body = bassier
        double sub = sin(phs) * exp(-tt * 3.6) * 0.55;                    // sustained sub weight
        double click = sin(TAU * 1900.0 * tt) * exp(-tt * 280.0) * 0.48;  // punchier attack
        double v = tanh((sin(ph) + sub + click) * 2.5) * amp * g;         // harder tanh = CRUNCH
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
        double sq = SINES ? sin(TAU * ph) : ((ph < 0.5) ? 1.0 : -1.0);
        double env = exp(-tt * 5.5); if (i < att) env *= (double)i / att;
        double v = sq * env * g * 0.42;
        addM(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.28 * lg, v * 0.28 * rg);
    }
}

// square LEAD — a pulse wave through a resonant lowpass with a CUTOFF SWEEP
// envelope (the filter sweep) and a long release: a digital melodic voice that
// sings its own evolving line (not a static chord stab).
// a CONTINUOUS flanger shared across all square-lead notes (a slow-swept short
// comb delay with feedback) so the square line swooshes.
static float fln_buf[2048]; static int fln_pos = 0; static double fln_lfo = 0;
static void sqlead(double note, double t, double dur, double g, double cut0, double cut1, double res, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double ph = 0, low = 0, band = 0, q = 1.0 / fmax(0.5, res), lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    long att = (long)(0.02 * SR), rel = (long)(0.18 * SR);
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        ph += f / SR; if (ph >= 1) ph -= 1;
        double sq = SINES ? sin(TAU * ph) : ((ph < 0.5) ? 1.0 : -1.0);   // sine in --sines mode
        double cut = cut1 + (cut0 - cut1) * exp(-tt * 2.2);       // sweep cut0 → cut1
        double fc = 2.0 * sin(M_PI * fmin(cut, SR * 0.45) / SR);
        double high = sq - low - q * band; band += fc * high; low += fc * band;
        double env = 1; if (i < att) env = (double)i / att; else if (i > n - rel) env = fmax(0, (double)(n - i) / rel);
        double dry = low * env * g;
        // FLANGE — modulated comb delay (1–6 ms) with feedback, continuous.
        fln_lfo += TAU * 0.22 / SR;
        int dt = (int)((1.0 + 5.0 * (0.5 + 0.5 * sin(fln_lfo))) * 0.001 * SR);
        float wet = fln_buf[(fln_pos - dt + 2048) & 2047];
        double out = dry + 0.7 * wet;
        fln_buf[fln_pos & 2047] = (float)(dry + 0.45 * wet);     // feedback
        fln_pos++;
        addM(s0 + i, out * lg, out * rg); addR(s0 + i, out * 0.3 * lg, out * 0.3 * rg);
    }
}

// ding — play a sampled triangle resampled UP by `semis` (high shimmer),
// letting its own metallic ring decay. Overlays the music bus.
static void ding(float *buf, long bn, double t, double semis, double g, double pan) {
    if (!buf || bn < 2) return;
    long s0 = (long)(t * SR); double rate = pow(2.0, semis / 12.0); long n = (long)(bn / rate);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double pos = i * rate; if (pos > bn - 2) break;
        long i0 = (long)pos; double fr = pos - i0;
        double s = (buf[i0] * (1 - fr) + buf[i0 + 1] * fr) * g;
        long j = s0 + i; if (j >= 0 && j < N) { musL[j] += (float)(s * lg); musR[j] += (float)(s * rg); addR(j, s * 0.3 * lg, s * 0.3 * rg); }
    }
}

// grand piano — a struck-string tone (still all sine): a stack of 6 partials
// with slight inharmonicity + a fast hammer attack + a piano-ish decay. For the
// high classical frills on top.
static void piano(double note, double t, double dur, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(dur * SR);
    double lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    double ph[6] = {0};
    const double amp[6] = { 1.0, 0.52, 0.32, 0.18, 0.10, 0.06 };
    const double inh[6] = { 1.0, 2.001, 3.003, 4.006, 5.01, 6.02 };   // slight inharmonicity
    long att = (long)(0.0015 * SR) + 1;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, s = 0;
        for (int k = 0; k < 6; k++) { ph[k] += TAU * f * inh[k] / SR; s += amp[k] * sin(ph[k]); }
        s /= 2.2;
        double env = exp(-tt * 1.5);                 // longer sustain (rings out)
        if (i < att) env *= (double)i / att;         // hammer attack
        double v = s * env * g;
        addM(s0 + i, v * lg, v * rg); addR(s0 + i, v * 0.28 * lg, v * 0.28 * rg);
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
// donk — the classic early-2000s UK "donk": a pitchy, bouncy resonant blip with
// a quick downward pitch snap + tanh saturation, plopped on the offbeats.
static void donk(double note, double t, double g, double pan) {
    double f = midi_hz(note); long s0 = (long)(t * SR), n = (long)(0.16 * SR);
    double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = f * (1.0 + 1.4 * exp(-tt * 48.0));   // quick "doonk" pitch drop
        ph += TAU * pf / SR;
        double env = exp(-tt * 24.0);                    // short + bouncy
        double v = tanh(sin(ph) * 2.2) * env * g;        // saturated = resonant donk
        addD(s0 + i, v * lg, v * rg); addR(s0 + i, v * 0.15, v * 0.15);
    }
}

// snare — a tight "pffp" HIP-HOP snare: noise-FORWARD bright bandpassed crack
// (the "pff") with a quick snappy decay and only a touch of tonal body. Drier.
static void snare(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.15 * SR);          // tight / short
    double ph = 0, ph2 = 0, low = 0, band = 0, prev = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR, v;
        if (SINES) {                                          // sine-burst snare approximation
            double pf = 220.0 + 380.0 * exp(-tt * 60.0);      // quick pitch drop "pp"
            ph += TAU * pf / SR; ph2 += TAU * pf * 2.7 / SR;  // + an inharmonic partial for snap
            v = (sin(ph) + 0.5 * sin(ph2)) * exp(-tt * 34.0) * g * 0.7;
        } else {
            double nz = rnd2();
            ph += TAU * 210.0 / SR;
            double body = sin(ph) * exp(-tt * 60.0) * 0.22;   // just a hint of tone, fast
            double hp = nz - prev; prev = nz;
            double fc = 2.0 * sin(M_PI * 3800.0 / SR), q = 1.0 / 0.8;   // brighter "pff"
            double high = hp - low - q * band; band += fc * high; low += fc * band;
            double crack = band * exp(-tt * 26.0) * 1.4;      // snappy, noise-forward
            v = tanh((body + crack) * 1.4) * g;
        }
        addD(s0 + i, v * lg, v * rg);
        addR(s0 + i, v * 0.12, v * 0.12);
    }
}

// pitched vocal one-shot — resamples a vocal sample to a pitch offset (semis)
// for vocal arpeggios/ornaments. Pad-ish decay; `atkMs` sets the attack (small
// = sharp start). Goes to the vocal bus (so it rides the wub + comp).
static void vocnote(float *buf, long bn, double t, double dur, double semis, double g, double atkMs, double pan) {
    if (!buf || bn < 2) return;
    long s0 = (long)(t * SR), n = (long)(dur * SR), att = (long)(atkMs * 0.001 * SR) + 1;
    double rate = pow(2.0, semis / 12.0), pos = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        if (pos > bn - 2) pos = bn - 2;
        long i0 = (long)pos; double fr = pos - i0;
        double s = buf[i0] * (1 - fr) + buf[i0 + 1] * fr;
        double env = exp(-((double)i / SR) * 3.0);       // pad-ish (longer) tail
        if (i < att) env *= (double)i / att;             // attack — sharp when atkMs small
        double v = s * env * g;
        long j = s0 + i; if (j >= 0 && j < N) { vocL[j] += (float)(v * lg); vocR[j] += (float)(v * rg); addR(j, v * 0.3 * lg, v * 0.3 * rg); }
        pos += rate;
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

// supersampling scratch — plays a sample with a rapidly oscillating back-and-
// forth playback rate, linearly interpolated (supersampled) = a turntable
// scratch on a vocal hit.
static void scratch(float *buf, long bn, double t, double dur, double g, double pan) {
    if (!buf || bn < 4) return;
    long s0 = (long)(t * SR), n = (long)(dur * SR); double pos = bn * 0.2, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double rate = 1.7 * sin(TAU * 6.5 * tt);            // forward/back scratch motion
        pos += rate; if (pos < 0) pos = 0; if (pos > bn - 2) pos = bn - 2;
        long i0 = (long)pos; double fr = pos - i0;
        double s = buf[i0] * (1 - fr) + buf[i0 + 1] * fr;   // supersampled interp
        double v = s * exp(-tt * 4.5) * g;
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
// hat — now a low THUD (not a tss/tick): a low sine thump with a tiny pitch
// drop + tanh weight, on the offbeats. Works in both cuts (it's a sine).
static void hat(double t, double g, double pan) {
    long s0 = (long)(t * SR), n = (long)(0.08 * SR); double ph = 0, lg = 1 - pan * 0.5, rg = 1 + pan * 0.5;
    for (long i = 0; i < n; i++) {
        double tt = (double)i / SR;
        double pf = 92.0 + 42.0 * exp(-tt * 60.0);   // low thud, small pitch drop
        ph += TAU * pf / SR;
        double v = tanh(sin(ph) * 1.5) * exp(-tt * 40.0) * g * 0.5;
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
// ARP tops echo the vocal HOOK (E/F) + plant C#, but trace a SMOOTH settling
// apex (C#5→A4→C5→C5) instead of a jagged zig-zag (B's voice-leading fix).
static const int ARP[4][4] = { {62,65,69,73}, {58,62,65,69}, {65,64,69,72}, {60,64,67,72} };
//                              Dm: D4 F4 A4 C#5  Bb: Bb3 D4 F4 A4  F: F4 E4 A4 C5  C: C4 E4 G4 C5
// chord pad — C#4 on the Dm bar (so the swarm, CHORD+12, sings C#5 = his home
// note) AND F3 HELD common across Dm→Bb→F (B's common-tone voice-leading) so
// the pad/swarm stop sliding in parallel blocks.
static const int CHORD[4][3] = { {50,53,61}, {46,53,58}, {48,53,57}, {48,52,55} };
//                                Dm:D3 F3 C#4  Bb:Bb2 F3 Bb3  F:C3 F3 A3  C:C3 E3 G3
// square lead SHADOWS the vocal line per chord (its actual sung pitches) so it
// answers the voice directly — C#4→D4 resolve on the Dm bar, the F4-E4 hook on
// the F bar, the B3-C4-D4-E4 verse tail on the C bar.
static const int SQMEL[4][4] = { {62,61,57,60}, {58,57,53,57}, {65,64,60,57}, {60,59,62,64} };
//                                Dm:D4 C#4 A3 C4  Bb:Bb3 A3 F3 A3  F:F4 E4 C4 A3  C:C4 B3 D4 E4

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
            vocL[j] += (float)(lp * lg); vocR[j] += (float)(lp * rg);   // → vocal bus
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
            vocL[j] += (float)(y * lg); vocR[j] += (float)(y * rg);   // → vocal bus
            if (rev > 0) addR(j, y * lg * rev, y * rg * rev);
        }
    }
}

// place a sample onto the DRUM bus (dry, punchy, NOT wub-sidechained) — for the
// short percussive boom/ma hits so they cut like little drums.
static void place_drum(float *buf, long bn, double atSec, double g, double pan, double rev) {
    if (!buf || g <= 0) return;
    long off = (long)(atSec * SR);
    double lg = (1 - pan * 0.5) * g, rg = (1 + pan * 0.5) * g;
    for (long i = 0; i < bn; i++) {
        long j = off + i;
        if (j >= 0 && j < N) {
            drumL[j] += (float)(buf[i] * lg); drumR[j] += (float)(buf[i] * rg);
            if (rev > 0) addR(j, buf[i] * lg * rev, buf[i] * rg * rev);
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
        else if (!strcmp(argv[i], "--sines")) SINES = 1;   // pure-sine variant
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
    long boompN = 0, mapN = 0;
    float *boomP = load_wav_mono("../sources/boombaboom-boomp.wav", &boompN);  // short percussive boom
    float *maP = load_wav_mono("../sources/boombaboom-map.wav", &mapN);        // short percussive ma
    long triN = 0;
    float *tri = load_wav_mono("../sources/triangle.wav", &triN);              // sampled triangle (overlay)
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
    const int PRELUDE = 4, INTRO = 10, PASS = 30, BR = 8, OUTRO = 4;   // longer build (beat ~24s), short outro (shorter track)
    const int NP = 3;
    const int DROP = PRELUDE + INTRO;                     // first BOOM / strong kick / sweep ends
    int passBar[3] = { DROP, DROP + PASS + BR, DROP + PASS + BR + PASS + BR };
    const int TB = passBar[2] + PASS + OUTRO;                  // total bars
    double totalSec = TB * BAR + 2.0; N = (long)(totalSec * SR);
    drumL = calloc(N, 4); drumR = calloc(N, 4); musL = calloc(N, 4); musR = calloc(N, 4);
    vocL = calloc(N, 4); vocR = calloc(N, 4);
    revL = calloc(N, 4); revR = calloc(N, 4); trig = calloc(N, 4);
    fprintf(stderr, "# boombaboom.c · %g BPM · %d bars · %.1fs · sine bells + kick + choral + hats\n", BPMV, TB, totalSec);
    fprintf(stderr, "# take %.1fs (trim %.2fs) · passes @ bars %d %d %d\n", voxSec, (double)vox0/SR, passBar[0], passBar[1], passBar[2]);

    // ── place the take + a CHORAL harmony stack on each pass ───────────────
    // The lead IS the song: loud + centred. A chord builds across passes —
    // octave + fifth + a diatonic third = a full Dm chord of his own voice,
    // spread L↔R. On the LAST pass the stack drops to the octave BELOW (the
    // grounded "true ending") instead of the bright octave above.
    //                              pass0  pass1  pass2(end)
    // fuller, more POWERFUL choral arrangement — harmonies pushed up.
    const double G_OUP[3] = { 0.00, 1.10, 0.45 };   // octave UP — OFF on the 1st pass (one strong vocal at the drop)
    const double G_FIF[3] = { 0.75, 1.10, 1.10 };
    const double G_THR[3] = { 0.40, 0.85, 1.05 };
    // SINGULAR vocal that MOVES THROUGH OCTAVES across the track — one voice at
    // a time, but a different octave each pass: pass1 up (cuts), pass2 down to
    // his own octave, pass3 back up. (Still no harmonies/layers.)
    float *voct[3]  = { vox,  ho,  hd  };   // a JOURNEY: original → up → down (not always up)
    long   voctN[3] = { voxN, hoN, hdN };
    double voctG[3] = { 1.95, 1.7, 1.9 };
    for (int pass = 0; pass < NP; pass++)
        place_layer(voct[pass], voctN[pass], vox0, passBar[pass], voctG[pass], 0.00, 0.44);
    // LOST-VOCAL PRELUDE — a SINGLE lost-vocal phrase, sung at three octaves
    // AT THE SAME TIME (no confusing time-offsets): the wobble + its octave-up
    // and octave-down, stacked in sync and drenched in reverb. One coherent
    // lost voice in a haze, then the build. (A second, quieter restatement near
    // the end of the prelude leads into the sweep.)
    {
        // SOUND FROM THE START — the VOCAL itself opens the track (no startup
        // chime): the phrase + its deep octave-down, CENTRED, soft + very wet,
        // landing almost immediately and restated once.
        // ONE voice — the lone lost phrase, restated across the whole longer
        // build (drop ~24s) so the vocal carries the intro until the beat lands.
        double at[4] = { 0.12, 4.0, 8.0, 11.0 };          // bars
        double g[4]  = { 0.44, 0.34, 0.40, 0.48 };        // builds toward the drop
        float *pre[4]  = { osc,  oscu,  oscd,  oscu };     // moves through octaves
        long   preN[4] = { oscN, oscuN, oscdN, oscuN };
        for (int k = 0; k < 4; k++) place_at(pre[k], preN[k], 0, at[k] * BAR, g[k], 0.00, 0.95);
    }
    // OSC FEATURE — the "ooowwoowwoo" wobble (pitched up = the single voice) as
    // the ONE vocal hook in each bridge breakdown. (No angels, no boom-fill —
    // singular vocal, simpler track.)
    place_at(oscu, oscuN, 0, (passBar[0] + PASS) * BAR, 1.4, 0.0, 0.4);  // bridge 1 — up
    place_at(oscd, oscdN, 0, (passBar[1] + PASS) * BAR, 1.4, 0.0, 0.4);  // bridge 2 — down
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
            double v = (fabs(vocL[i]) + fabs(vocR[i])) * 0.5;
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
            // VOCALS-ONLY OPENING — hold the swarm (+ drone) out of the prelude;
            // ramp in across the first 2 bars of the build.
            double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (2 * BAR)));
            musL[i] += (float)((heal + sl) * pg); musR[i] += (float)((heal + sr) * pg);
            addR(i, (heal + sl) * 0.3 * pg, (heal + sr) * 0.3 * pg);
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
            double sq = SINES ? sin(TAU * ph) : ((ph < 0.5) ? 1.0 : -1.0);
            lp += 0.018 * (sq - lp);                             // ~heavy LP → soft
            double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (2 * BAR)));
            double v = lp * 0.075 * pg;                          // out of the opening
            musL[i] += (float)v; musR[i] += (float)v;
        }
    }

    // ── GRANULAR HISS — an airy bed of tiny filtered-noise grains scattered
    // across the whole track (tape-air atmosphere), drifting slowly L↔R ──────
    if (!SINES) {                                            // noise — off in the all-sines cut
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
    if (!SINES) {                                            // sample — off in the all-sines cut
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

    // ── DEEP HIGHWAY DRIVING (freesound) — the low rushing roar the vocal cuts
    // across. Looped, deep + present (more than the rain), from the build on.
    if (!SINES) {
        long hwN = 0; float *hw = load_wav_mono("../sources/highway.wav", &hwN);
        if (hw && hwN > SR) {
            double pos = 0;
            for (long i = 0; i < N; i++) {
                pos += 1.0; if (pos >= hwN - 1) pos -= (hwN - 1);
                long i0 = (long)pos; double fr = pos - i0;
                double s = hw[i0] * (1 - fr) + hw[(i0 + 1 < hwN) ? i0 + 1 : i0] * fr;
                double pg = fmin(1.0, fmax(0.0, ((double)i / SR - PRELUDE * BAR) / (3 * BAR)));
                double v = s * 0.16 * pg;                            // deep + present
                musL[i] += (float)v; musR[i] += (float)v;
            }
            free(hw);
        }
    }

    // ── LA HELICOPTER flyby (freesound) — a close chopper passes overhead,
    // panning L→R, at a couple of dramatic moments (into bridge 1 + into the
    // final drop). Skipped if the file is missing. ──────────────────────────
    if (!SINES) {                                            // sample — off in the all-sines cut
        long heliN = 0; float *heli = load_wav_mono("../sources/heli.wav", &heliN);
        fprintf(stderr, "# heli: %s (%.1fs)\n", heli ? "LOADED" : "MISSING", heliN / (double)SR);
        if (heli && heliN > SR) {
            // LOUD enough to clearly hear the chopper pass overhead. Three
            // flybys: into bridge 1, the big breakdown, and the final drop.
            double when[3] = { (passBar[0] + PASS) * BAR, (passBar[1] + PASS) * BAR, (passBar[2] - 3) * BAR };
            double gain[3] = { 0.42, 0.46, 0.52 };
            for (int w = 0; w < 3; w++) {
                long off = (long)(when[w] * SR);
                for (long i = 0; i < heliN; i++) {
                    long j = off + i; if (j < 0 || j >= N) continue;
                    double p = (double)i / heliN;             // 0→1 = flies across
                    double pan = p * 2 - 1;                    // hard L→R sweep
                    double v = heli[i] * gain[w];
                    // mostly DRY (close + present) + a touch of verb. Goes on the
                    // DRUM bus so the sidechain doesn't duck it away.
                    drumL[j] += (float)(v * (1 - pan * 0.6)); drumR[j] += (float)(v * (1 + pan * 0.6));
                    addR(j, v * 0.18, v * 0.18);
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
                double floorf = (beat & 1) ? 46.0 : 31.0;       // deeper A (tick) / D (tock)
                kick(bar * BAR + beat * BEAT + hum, 0.98 * fmin(1.18, intens), floorf);
            }

        // classic 2000s DONK — used PLAYFULLY: a pattern of which offbeats get a
        // donk, flipping each bar (not a mechanical donk on every offbeat).
        if (inPass) {
            const char *DK[4] = { "..x.", "x..x", ".x..", "x.x." };
            const char *dk = DK[bar % 4];
            for (int beat = 0; beat < 4; beat++)
                if (dk[beat] == 'x')
                    donk(ROOT[phr] + 24, bar * BAR + beat * BEAT + BEAT * 0.5 + hum, 0.55 * fmin(1.15, intens), (beat & 1) ? 0.3 : -0.3);
        }

        // reverse-kick used PLAYFULLY — a whoosh into the big section starts, and
        // the odd lift into a phrase. NOT on every kick.
        if (bar == passBar[0] || bar == passBar[1] || bar == passBar[2]) revkick(bar * BAR, BEAT * 2.0, 0.55);
        else if (inPass && bar % 8 == 7) revkick((bar + 1) * BAR, BEAT * 1.0, 0.34);

        // TIGHT closed hat — centred, on the offbeat 8ths, no swing wobble, no
        // panning. Simple and locked.
        double hatg = (inPass ? 1.0 : 0.0) * (0.09 + 0.03 * (pass > 0 ? pass : 0));   // quieter
        if (hatg > 0)
            for (int st = 1; st < 8; st += 2)
                hat(bar * BAR + st * (BEAT / 2.0) + hum, hatg, 0.0);

        // VOCALS-ONLY OPENING — sub-bell + pad are SILENT in the prelude.
        if (!inPrelude) subbell(ROOT[phr], bar * BAR + hum, BAR * (phr == 0 ? 1.4 : 1.0), 0.20);
        if (!inPrelude)
            chordpad(CHORD[phr], 3, bar * BAR, BAR * 1.02,
                     (inBridge ? 0.14 : 0.095), (phr & 1) ? 0.22 : -0.22);

        // bell arpeggio — from the INTRO build onward (no bells in the prelude).
        // Gentle pluck on the 8ths; busier + brighter on later passes, dropped
        // an octave on the final (grounded) verse.
        // sine bells — NOT in the prelude (vocals-only opening). Lower register
        // now + a LESS REGULAR, syncopated pattern that flips each bar.
        if (!inPrelude) {
            const int *arp = ARP[phr];
            const char *PAT[2] = { "x..x.x..", "x.x...x." };   // irregular, flips per bar
            const char *pat = PAT[bar & 1];
            int ai = 0;
            for (int st = 0; st < 8; st++) {
                if (pat[st] != 'x') continue;
                int note = arp[ai % 4] + boct - 12;             // an octave LOWER (less high)
                ai++;
                double bg = ((inBridge ? 0.15 : 0.12) + (pass >= 1 ? 0.03 : 0.0)) * fmin(1.2, intens);   // sines lower
                double bpan = panLFO * 0.45 + ((st & 2) ? 0.25 : -0.25);   // movement
                bell(note, swung(st) + hum, BEAT * 4.0, bg, bpan);   // SWUNG + humanized + long ring
            }
            if (inBridge) bell(ARP[phr][3], bar * BAR + 2 * BEAT, BEAT * 4.0, 0.16, -panLFO * 0.5);
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
                    sqlead(note, t, dur, 0.060, 380, openness, 3.0, (s & 2) ? 0.25 : -0.25);
                }
            }
        }

        // BANGING SNARE — backbeat (beats 2 & 4) once the track goes harder.
        if (inPass && pass >= 1)
            for (int b2 = 1; b2 < 4; b2 += 2) snare(bar * BAR + b2 * BEAT + hum, 0.62 * intens, (rnd() - 0.5) * 0.3);

        // (reverse-swell risers removed — the "shheeep" every ~4 bars was too repetitive)

        // (scratch + bird/elf arp removed — simpler track, no regulated arps)

        // GRAND PIANO — occasional single sustained high note (NOT a run/arp).
        // Once every 8 bars, a lone ringing piano tone up high.
        if (inPass && (bar % 8 == 0)) {
            static const int hi[4] = { 81, 77, 84, 79 };   // A5 / F5 / C6 / G5 (D minor)
            piano(hi[(bar / 8) % 4], bar * BAR + hum, BEAT * 5.0, 0.18, 0.2 * panLFO);
        }

        // TRIANGLE — just ONCE IN A WHILE (not a pattern): a lone high shimmer
        // roughly every 16 bars, on an irregular beat. (sample → off in --sines.)
        if (inPass && tri && !SINES && (bar % 16 == 9))
            ding(tri, triN, bar * BAR + 2.5 * BEAT + hum, 12.0, 0.34, 0.3);

        // WHISTLE FEATURE — the breakdown before the final drop goes FULL
        // ethereal: a pure whistle climbs the chord, drenched, taking the
        // soft/deep/whistle direction all the way for these few bars.
        int inBridge2 = (!inPass && !inOutro && bar >= passBar[1] + PASS && bar < passBar[2]);
        if (inBridge2) {
            whistle(CHORD[phr][2] + 12, bar * BAR, BAR * 1.7, 0.34, sin(TAU * (bar % 4) / 4.0) * 0.45);
            if (phr == 2) whistle(CHORD[phr][1] + 24, bar * BAR + 2 * BEAT, BAR * 1.2, 0.20, -0.35);
        }

        // (filter-sweep risers removed — they were too repetitive into every pass)
    }

    // (boom/ma percussion scatter removed — simpler track, singular vocal)
    free(boomS); free(maS); free(boomP); free(maP); free(tri);

    // ── sidechain: the kick ducks the music bed (bells/chord/swarm/etc.) ──────
    { double depth = 0.45, rel = exp(-1.0 / (0.13 * SR)), env = 0;
      for (long i = 0; i < N; i++) { if (trig[i] > 0) env = 1; else env *= rel; double d = 1.0 - depth * env; musL[i] *= (float)d; musR[i] *= (float)d; } }

    // ── VOCAL WUB SIDECHAIN — the voice is "connected" to the kick: on every
    // kick it DUCKS and a lowpass cutoff DIPS down then springs back open, so
    // the vocal pitches/wobbles down into each kick (wub-wub). ───────────────
    { double env = 0, atk = exp(-1.0 / (0.004 * SR)), rel = exp(-1.0 / (0.20 * SR));
      double ll = 0, lr = 0;
      for (long i = 0; i < N; i++) {
          double trg = (trig[i] > 0) ? 1.0 : 0.0;
          env = trg > env ? atk * env + (1 - atk) * trg : rel * env + (1 - rel) * trg;
          double duck = 1.0 - 0.55 * env;                       // amplitude dip
          double cut = 1100.0 + 16000.0 * (1.0 - env);          // cutoff dives to ~1.1k on the kick
          double fc = 1.0 - exp(-TAU * cut / SR);
          ll += fc * (vocL[i] - ll); lr += fc * (vocR[i] - lr);
          vocL[i] = (float)(ll * duck); vocR[i] = (float)(lr * duck);
      } }

    // ── COMPRESSOR over the vocal bus — glue + push it forward evenly. ───────
    { double env = 0, atk = exp(-1.0 / (0.006 * SR)), rel = exp(-1.0 / (0.16 * SR));
      double thr = 0.18, ratio = 3.2, makeup = 1.5;
      for (long i = 0; i < N; i++) {
          double a = fmax(fabs(vocL[i]), fabs(vocR[i]));
          env = a > env ? atk * env + (1 - atk) * a : rel * env + (1 - rel) * a;
          double g = makeup;
          if (env > thr) g = makeup * (thr + (env - thr) / ratio) / env;
          vocL[i] *= (float)g; vocR[i] *= (float)g;
      } }

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
    // BLEND — instruments quieter (drums + bed) so everything folds UNDER the
    // voice into one whole; the vocal stays forward.
    float *busL = drumL, *busR = drumR;
    const double IG = 0.85;   // instrument (drum + music bed) level vs the vocal
    for (long i = 0; i < N; i++) {
        busL[i] = (float)(drumL[i] * IG + musL[i] * IG + vocL[i]);
        busR[i] = (float)(drumR[i] * IG + musR[i] * IG + vocR[i]);
    }
    free(vocL); free(vocR);
    double peak = 0; for (long i = 0; i < N; i++) { double a = fmax(fabs(busL[i]), fabs(busR[i])); if (a > peak) peak = a; }
    if (peak > 0) { double g = 0.92 / peak; for (long i = 0; i < N; i++) { busL[i] *= (float)g; busR[i] *= (float)g; } }
    long fin = (long)(0.4 * SR), fout = (long)(1.0 * SR);   // short safety taper; the real
    for (long i = 0; i < fin && i < N; i++) { double g = (double)i / fin; busL[i] *= (float)g; busR[i] *= (float)g; }
    for (long i = 0; i < fout && i < N; i++) {              // long graceful fade is applied
        double g = (double)(fout - i) / fout;               // AFTER loudnorm in render.sh so
        long idx = N - 1 - i; busL[idx] *= (float)g; busR[idx] *= (float)g;   // it isn't re-normalized
    }

    if (!write_wav_f32_stereo(out_path, busL, busR, N)) { fprintf(stderr, "✗ write failed\n"); return 1; }
    fprintf(stderr, "✓ %s · %.1fs\n", out_path, (double)N / SR);
    return 0;
}
