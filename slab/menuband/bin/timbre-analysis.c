// timbre-analysis.c — see timbre-analysis.h for what this measures and what
// it deliberately does not claim.
//
// Extracted from bin/gm-timbre-probe.c so the Fluoddity path harness measures
// on exactly the same scale. This is a MOVE, not a redesign: the math is
// unchanged, the statics became the public API, and the hard-coded sample
// rate became a parameter.

#include "timbre-analysis.h"

#include <math.h>
#include <stdlib.h>

// Analysis window sizes. Changing any of these changes every measured
// coordinate in the fleet, including the checked-in gm-timbre-space.json.
#define FFT_N       2048    // brightness analysis window
#define FFT_HOP     512
#define ONSET_N     256     // onset analysis window (5.3 ms @ 48k)
#define ONSET_HOP   32      // 0.67 ms — fine enough to separate attack rates
#define ENV_WIN     128     // amplitude-envelope RMS window
#define ENV_HOP     32
#define NBANDS      4       // onset-asynchrony bands

// ── Minimal iterative radix-2 FFT (in-place, complex interleaved) ──

void timbre_fft(double *re, double *im, int n) {
    for (int i = 1, j = 0; i < n; i++) {
        int bit = n >> 1;
        for (; j & bit; bit >>= 1) j ^= bit;
        j ^= bit;
        if (i < j) {
            double tr = re[i]; re[i] = re[j]; re[j] = tr;
            double ti = im[i]; im[i] = im[j]; im[j] = ti;
        }
    }
    for (int len = 2; len <= n; len <<= 1) {
        double ang = -2.0 * M_PI / len;
        double wr = cos(ang), wi = sin(ang);
        for (int i = 0; i < n; i += len) {
            double cr = 1.0, ci = 0.0;
            for (int k = 0; k < len / 2; k++) {
                double ur = re[i + k],           ui = im[i + k];
                double vr = re[i + k + len / 2] * cr - im[i + k + len / 2] * ci;
                double vi = re[i + k + len / 2] * ci + im[i + k + len / 2] * cr;
                re[i + k] = ur + vr;             im[i + k] = ui + vi;
                re[i + k + len / 2] = ur - vr;   im[i + k + len / 2] = ui - vi;
                double nr = cr * wr - ci * wi;
                ci = cr * wi + ci * wr;
                cr = nr;
            }
        }
    }
}

// ── Bark scale + spread of masking ──

// Traunmüller's analytic Bark approximation.
double timbre_hz_to_bark(double hz) {
    if (hz < 1.0) hz = 1.0;
    return (26.81 * hz) / (1960.0 + hz) - 0.53;
}

// Schroeder's spreading function, in dB, for a masker-maskee Bark separation.
// Asymmetric: energy spreads upward in frequency far more readily than down,
// which is the auditory asymmetry Wessel's Zwicker compensation is there for.
static double spread_db(double dz) {
    return 15.81 + 7.5 * (dz + 0.474) - 17.5 * sqrt(1.0 + (dz + 0.474) * (dz + 0.474));
}

static double bark_center[TIMBRE_NBARK];
static double spread_lut[TIMBRE_NBARK][TIMBRE_NBARK];

void timbre_analysis_init(void) {
    static int ready = 0;
    if (ready) return;
    ready = 1;
    for (int b = 0; b < TIMBRE_NBARK; b++) bark_center[b] = b + 0.5;
    for (int m = 0; m < TIMBRE_NBARK; m++)
        for (int b = 0; b < TIMBRE_NBARK; b++)
            spread_lut[m][b] = pow(10.0, spread_db(bark_center[b] - bark_center[m]) / 10.0);
}

// Loudness-weighted mean Bark centroid across the whole note.
double timbre_brightness(const float *x, long n, double sample_rate) {
    static double re[FFT_N], im[FFT_N], win[FFT_N];
    static int win_ready = 0;
    if (!win_ready) {
        for (int i = 0; i < FFT_N; i++)
            win[i] = 0.5 * (1.0 - cos(2.0 * M_PI * i / (FFT_N - 1)));
        win_ready = 1;
    }
    long nframes = 0;
    for (long start = 0; start + FFT_N <= n; start += FFT_HOP) nframes++;
    if (nframes < 1) return 0.0;

    double *fc = malloc(sizeof(double) * nframes);   // per-frame Bark centroid
    double *fl = malloc(sizeof(double) * nframes);   // per-frame Zwicker loudness
    double *fe = malloc(sizeof(double) * nframes);   // per-frame linear energy

    long f = 0;
    double emax = 0.0;
    for (long start = 0; start + FFT_N <= n; start += FFT_HOP, f++) {
        for (int i = 0; i < FFT_N; i++) { re[i] = x[start + i] * win[i]; im[i] = 0.0; }
        timbre_fft(re, im, FFT_N);

        // Power into Bark bands.
        double band[TIMBRE_NBARK] = {0};
        for (int k = 1; k < FFT_N / 2; k++) {
            double hz = k * sample_rate / FFT_N;
            if (hz > 15500.0) break;
            int b = (int)timbre_hz_to_bark(hz);
            if (b < 0) b = 0;
            if (b >= TIMBRE_NBARK) continue;
            band[b] += re[k] * re[k] + im[k] * im[k];
        }
        // Spread of masking → excitation pattern. This is Wessel's
        // "compensated spectral energy distribution".
        double exc[TIMBRE_NBARK] = {0};
        for (int m = 0; m < TIMBRE_NBARK; m++) {
            if (band[m] <= 0.0) continue;
            for (int b = 0; b < TIMBRE_NBARK; b++) exc[b] += band[m] * spread_lut[m][b];
        }
        // The centroid is taken over the excitation ENERGY, exactly as the
        // paper says — "the centroid or mean of this compensated spectral
        // energy distribution". Taking it over compressed specific loudness
        // instead lets a whisper of breath noise in the top bands outvote the
        // fundamental, which reads a flute as brighter than a fuzz guitar.
        double e = 0.0, c = 0.0;
        for (int b = 0; b < TIMBRE_NBARK; b++) { e += exc[b]; c += exc[b] * bark_center[b]; }
        fc[f] = e > 0.0 ? c / e : 0.0;
        fe[f] = e;
        if (e > emax) emax = e;
        // Frame WEIGHT is loudness (Zwicker's compressive exponent), so a
        // decaying voice is still described by its audible tail and not only
        // by its loudest instant.
        double sl = 0.0;
        for (int b = 0; b < TIMBRE_NBARK; b++) sl += pow(exc[b], 0.23);
        fl[f] = sl;
    }

    // Frames below -45 dB of the loudest frame are not part of the tone.
    // Without this gate a one-shot's near-silent tail — 80% of a fixed
    // analysis window — dominates the average.
    double floor_e = emax * 3.1623e-5;
    double num = 0.0, den = 0.0;
    for (long i = 0; i < nframes; i++) {
        if (fe[i] <= floor_e) continue;
        num += fc[i] * fl[i];
        den += fl[i];
    }
    free(fc); free(fl); free(fe);
    return den > 0.0 ? num / den : 0.0;
}

// 10→90% rise time of the amplitude envelope, in ms.
double timbre_rise_ms(const float *x, long n, double sample_rate) {
    long frames = (n - ENV_WIN) / ENV_HOP;
    if (frames < 4) return 0;
    double *env = malloc(sizeof(double) * frames);
    double peak = 0.0;
    for (long f = 0; f < frames; f++) {
        double s = 0.0;
        long start = f * ENV_HOP;
        for (int i = 0; i < ENV_WIN; i++) s += (double)x[start + i] * x[start + i];
        env[f] = sqrt(s / ENV_WIN);
        if (env[f] > peak) peak = env[f];
    }
    if (peak <= 0.0) { free(env); return 0; }

    long i10 = -1, i90 = -1;
    for (long f = 0; f < frames; f++) {
        if (i10 < 0 && env[f] >= 0.10 * peak) i10 = f;
        if (env[f] >= 0.90 * peak) { i90 = f; break; }
    }
    free(env);
    if (i10 < 0 || i90 < 0 || i90 < i10) return 0.0;
    return (double)(i90 - i10) * ENV_HOP * 1000.0 / sample_rate;
}

// Spread (population stddev) of per-band 50%-of-band-peak onset times, in ms.
// Wessel's second axis is partly onset SYNCHRONICITY across the spectrum;
// this is that quantity.
double timbre_async_ms(const float *x, long n, double sample_rate) {
    static double re[ONSET_N], im[ONSET_N], win[ONSET_N];
    static int win_ready = 0;
    if (!win_ready) {
        for (int i = 0; i < ONSET_N; i++)
            win[i] = 0.5 * (1.0 - cos(2.0 * M_PI * i / (ONSET_N - 1)));
        win_ready = 1;
    }
    // Only the first 250 ms matters for onset.
    long limit = (long)(sample_rate * 0.25);
    if (limit > n) limit = n;
    long frames = (limit - ONSET_N) / ONSET_HOP;
    if (frames < 8) return 0.0;

    // Band edges in Hz: low / low-mid / high-mid / high.
    const double edge[NBANDS + 1] = { 20.0, 500.0, 2000.0, 5000.0, 16000.0 };
    double *benv[NBANDS];
    for (int b = 0; b < NBANDS; b++) benv[b] = calloc(frames, sizeof(double));

    for (long f = 0; f < frames; f++) {
        long start = f * ONSET_HOP;
        for (int i = 0; i < ONSET_N; i++) { re[i] = x[start + i] * win[i]; im[i] = 0.0; }
        timbre_fft(re, im, ONSET_N);
        for (int k = 1; k < ONSET_N / 2; k++) {
            double hz = k * sample_rate / ONSET_N;
            double p = re[k] * re[k] + im[k] * im[k];
            for (int b = 0; b < NBANDS; b++)
                if (hz >= edge[b] && hz < edge[b + 1]) { benv[b][f] += p; break; }
        }
    }

    double t[NBANDS];
    int used = 0;
    for (int b = 0; b < NBANDS; b++) {
        double peak = 0.0;
        for (long f = 0; f < frames; f++) if (benv[b][f] > peak) peak = benv[b][f];
        if (peak <= 0.0) continue;
        // A band that never carries real energy should not vote.
        for (long f = 0; f < frames; f++) {
            if (benv[b][f] >= 0.5 * peak) {
                t[used++] = (double)f * ONSET_HOP * 1000.0 / sample_rate;
                break;
            }
        }
    }
    for (int b = 0; b < NBANDS; b++) free(benv[b]);
    if (used < 2) return 0.0;

    double mean = 0.0;
    for (int i = 0; i < used; i++) mean += t[i];
    mean /= used;
    double var = 0.0;
    for (int i = 0; i < used; i++) var += (t[i] - mean) * (t[i] - mean);
    return sqrt(var / used);
}


double timbre_bark_to_hz(double z) {
    return 600.0 * sinh(z / 6.0);
}

int timbre_loudness_equalize(float *x, long n, double target) {
    double sum = 0.0;
    for (long i = 0; i < n; i++) sum += (double)x[i] * x[i];
    double rms = sqrt(sum / n);
    if (rms < 1e-7) return 0;
    double g = target / rms;
    for (long i = 0; i < n; i++) x[i] = (float)(x[i] * g);
    return 1;
}
