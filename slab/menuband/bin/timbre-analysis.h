// timbre-analysis.h — the timbre axes, shared by the measurement harnesses.
//
// After David Wessel, "Timbre Space as a Musical Control Structure", Computer
// Music Journal 3(2), 1979, pp. 45-52. Wessel's space came from listener
// dissimilarity judgments run through multidimensional scaling; its axes were
// then INTERPRETED by correlating point projections with acoustic measures —
// the centroid of a Zwicker loudness-model excitation pattern for brightness,
// the "bite" of the onset for the other axis.
//
// This file computes those acoustic correlates. It does not collect judgments
// and does not run MDS, so what it produces is a proxy for a timbre space
// rather than one. Anywhere its numbers are reported, say that.
//
// The JavaScript side of AC carries a numerically identical port at
// `toolchain/timbre/brightness.mjs`; the two agree to three decimals on a
// sine, a square, a sawtooth and white noise (see `--selftest`). Keep them in
// step or the two halves of the fleet stop sharing a scale.

#ifndef TIMBRE_ANALYSIS_H
#define TIMBRE_ANALYSIS_H

// Build the Bark and spread-of-masking tables. Idempotent; call once before
// any measurement.
void timbre_analysis_init(void);

// Wessel §B — "the tones should be equalized with respect to the properties
// that are not to influence the judgments". Scales `x` in place to `target`
// RMS so a loud voice cannot read as a bright one. Returns 0 (and leaves the
// buffer alone) when the signal is silent.
int timbre_loudness_equalize(float *x, long n, double target);

// Loudness-weighted mean Bark centroid of the excitation pattern. Higher is
// brighter. Feed it a loudness-equalized signal.
double timbre_brightness(const float *x, long n, double sample_rate);

// 10 → 90% rise time of the amplitude envelope, in milliseconds.
double timbre_rise_ms(const float *x, long n, double sample_rate);

// Spread (population stddev) of per-band onset times, in milliseconds — the
// "extent of synchronicity among the various components" Wessel names as part
// of his second axis and never quantifies.
double timbre_async_ms(const float *x, long n, double sample_rate);

// Approximate centre frequency of a Bark value, for readable output.
double timbre_bark_to_hz(double z);

// In-place iterative radix-2 FFT, exposed for harnesses that want their own
// spectra (e.g. the per-band dump in gm-timbre-probe).
void timbre_fft(double *re, double *im, int n);

// Bark band index for a frequency (Traunmüller's analytic approximation).
double timbre_hz_to_bark(double hz);

#define TIMBRE_NBARK 24

#endif // TIMBRE_ANALYSIS_H
