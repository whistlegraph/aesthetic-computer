// brightness.mjs — the timbre axes, for JavaScript-side synthesis.
//
// A port of the analysis half of `slab/menuband/bin/gm-timbre-probe.c`, kept
// numerically identical (same Bark mapping, same spreading function, same
// energy-domain centroid, same frame gate) so a wave measured here and a GM
// program measured there sit on ONE scale and can be compared.
//
// After David Wessel, "Timbre Space as a Musical Control Structure", Computer
// Music Journal 3(2), 1979. Wessel validated his measured vertical axis by
// correlating it with the centroid of a Zwicker excitation pattern, and his
// horizontal axis with the "bite" of the onset. There are no listeners here,
// so we compute those correlates directly and use them as coordinates. That
// is a proxy for a timbre space, not one — no dissimilarity judgments were
// collected and no multidimensional scaling was run.

const { PI, cos, sin, sqrt, log10, pow, sinh } = Math;

// ── FFT (iterative radix-2, in place) ──

export function fft(re, im) {
  const n = re.length;
  for (let i = 1, j = 0; i < n; i += 1) {
    let bit = n >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;
    if (i < j) {
      [re[i], re[j]] = [re[j], re[i]];
      [im[i], im[j]] = [im[j], im[i]];
    }
  }
  for (let len = 2; len <= n; len <<= 1) {
    const ang = (-2 * PI) / len;
    const wr = cos(ang), wi = sin(ang);
    for (let i = 0; i < n; i += len) {
      let cr = 1, ci = 0;
      for (let k = 0; k < len / 2; k += 1) {
        const ur = re[i + k], ui = im[i + k];
        const vr = re[i + k + len / 2] * cr - im[i + k + len / 2] * ci;
        const vi = re[i + k + len / 2] * ci + im[i + k + len / 2] * cr;
        re[i + k] = ur + vr; im[i + k] = ui + vi;
        re[i + k + len / 2] = ur - vr; im[i + k + len / 2] = ui - vi;
        const nr = cr * wr - ci * wi;
        ci = cr * wi + ci * wr;
        cr = nr;
      }
    }
  }
}

// ── Bark scale + spread of masking ──

export const NBARK = 24;

/** Traunmüller's analytic Bark approximation. */
export function hzToBark(hz) {
  const f = hz < 1 ? 1 : hz;
  return (26.81 * f) / (1960 + f) - 0.53;
}

/** Approximate centre frequency of a Bark band, for readable output. */
export function barkToHz(z) {
  return 600 * sinh(z / 6);
}

// Schroeder's spreading function in dB across a Bark separation. Asymmetric:
// energy spreads upward in frequency far more readily than down. This is the
// auditory property Wessel's Zwicker compensation exists to model.
function spreadDb(dz) {
  const x = dz + 0.474;
  return 15.81 + 7.5 * x - 17.5 * sqrt(1 + x * x);
}

const barkCenter = Array.from({ length: NBARK }, (_, b) => b + 0.5);
const spreadLut = barkCenter.map((zm) =>
  barkCenter.map((zb) => pow(10, spreadDb(zb - zm) / 10)),
);

// ── Brightness ──

const FFT_N = 2048;
const FFT_HOP = 512;

/**
 * Loudness-weighted mean Bark centroid of the excitation pattern.
 * Higher = brighter. Feed it a loudness-equalized signal.
 */
export function brightness(x, sampleRate) {
  const win = Array.from({ length: FFT_N }, (_, i) =>
    0.5 * (1 - cos((2 * PI * i) / (FFT_N - 1))),
  );
  const centroids = [], loudness = [], energies = [];
  let emax = 0;

  for (let start = 0; start + FFT_N <= x.length; start += FFT_HOP) {
    const re = new Float64Array(FFT_N), im = new Float64Array(FFT_N);
    for (let i = 0; i < FFT_N; i += 1) re[i] = x[start + i] * win[i];
    fft(re, im);

    const band = new Float64Array(NBARK);
    for (let k = 1; k < FFT_N / 2; k += 1) {
      const hz = (k * sampleRate) / FFT_N;
      if (hz > 15500) break;
      const b = Math.floor(hzToBark(hz));
      if (b >= 0 && b < NBARK) band[b] += re[k] * re[k] + im[k] * im[k];
    }
    const exc = new Float64Array(NBARK);
    for (let m = 0; m < NBARK; m += 1) {
      if (band[m] <= 0) continue;
      for (let b = 0; b < NBARK; b += 1) exc[b] += band[m] * spreadLut[m][b];
    }
    // Centroid over excitation ENERGY — the paper's "centroid or mean of this
    // compensated spectral energy distribution". Taking it over compressed
    // specific loudness instead lets a whisper of breath noise in the top
    // bands outvote the fundamental.
    let e = 0, c = 0, sl = 0;
    for (let b = 0; b < NBARK; b += 1) {
      e += exc[b];
      c += exc[b] * barkCenter[b];
      sl += pow(exc[b], 0.23);   // Zwicker specific loudness — the frame WEIGHT
    }
    centroids.push(e > 0 ? c / e : 0);
    loudness.push(sl);
    energies.push(e);
    if (e > emax) emax = e;
  }

  // Frames below -45 dB of the loudest frame are not part of the tone.
  const floor = emax * 3.1623e-5;
  let num = 0, den = 0;
  for (let i = 0; i < centroids.length; i += 1) {
    if (energies[i] <= floor) continue;
    num += centroids[i] * loudness[i];
    den += loudness[i];
  }
  return den > 0 ? num / den : 0;
}

// ── Bite: onset rate and cross-band onset spread ──

const ENV_WIN = 128;
const ENV_HOP = 32;

/** 10 → 90% rise time of the amplitude envelope, in milliseconds. */
export function riseMs(x, sampleRate) {
  const frames = Math.floor((x.length - ENV_WIN) / ENV_HOP);
  if (frames < 4) return 0;
  const env = new Float64Array(frames);
  let peak = 0;
  for (let f = 0; f < frames; f += 1) {
    let s = 0;
    const start = f * ENV_HOP;
    for (let i = 0; i < ENV_WIN; i += 1) s += x[start + i] * x[start + i];
    env[f] = sqrt(s / ENV_WIN);
    if (env[f] > peak) peak = env[f];
  }
  if (peak <= 0) return 0;
  let i10 = -1, i90 = -1;
  for (let f = 0; f < frames; f += 1) {
    if (i10 < 0 && env[f] >= 0.1 * peak) i10 = f;
    if (env[f] >= 0.9 * peak) { i90 = f; break; }
  }
  if (i10 < 0 || i90 < 0 || i90 < i10) return 0;
  return ((i90 - i10) * ENV_HOP * 1000) / sampleRate;
}

const ONSET_N = 256;
const ONSET_HOP = 32;
const BAND_EDGES = [20, 500, 2000, 5000, 16000];

/**
 * Spread (population stddev) of per-band onset times, in milliseconds.
 * Wessel's second axis is partly the "extent of synchronicity among the
 * various components"; this is that quantity.
 */
export function asyncMs(x, sampleRate) {
  const win = Array.from({ length: ONSET_N }, (_, i) =>
    0.5 * (1 - cos((2 * PI * i) / (ONSET_N - 1))),
  );
  const limit = Math.min(x.length, Math.floor(sampleRate * 0.25));
  const frames = Math.floor((limit - ONSET_N) / ONSET_HOP);
  if (frames < 8) return 0;

  const nbands = BAND_EDGES.length - 1;
  const benv = Array.from({ length: nbands }, () => new Float64Array(frames));
  for (let f = 0; f < frames; f += 1) {
    const re = new Float64Array(ONSET_N), im = new Float64Array(ONSET_N);
    const start = f * ONSET_HOP;
    for (let i = 0; i < ONSET_N; i += 1) re[i] = x[start + i] * win[i];
    fft(re, im);
    for (let k = 1; k < ONSET_N / 2; k += 1) {
      const hz = (k * sampleRate) / ONSET_N;
      const p = re[k] * re[k] + im[k] * im[k];
      for (let b = 0; b < nbands; b += 1) {
        if (hz >= BAND_EDGES[b] && hz < BAND_EDGES[b + 1]) { benv[b][f] += p; break; }
      }
    }
  }

  const times = [];
  for (let b = 0; b < nbands; b += 1) {
    let peak = 0;
    for (let f = 0; f < frames; f += 1) if (benv[b][f] > peak) peak = benv[b][f];
    if (peak <= 0) continue;   // a band with no real energy does not vote
    for (let f = 0; f < frames; f += 1) {
      if (benv[b][f] >= 0.5 * peak) {
        times.push((f * ONSET_HOP * 1000) / sampleRate);
        break;
      }
    }
  }
  if (times.length < 2) return 0;
  const mean = times.reduce((a, b) => a + b, 0) / times.length;
  const varr = times.reduce((a, t) => a + (t - mean) ** 2, 0) / times.length;
  return sqrt(varr);
}

// ── Helpers ──

/**
 * Wessel §B — equalize the properties that must not influence the judgment.
 * Scales to a fixed RMS so a hot voice cannot read as a bright one. Returns a
 * new array; the input is untouched.
 */
export function loudnessEqualize(x, target = 0.1) {
  let s = 0;
  for (let i = 0; i < x.length; i += 1) s += x[i] * x[i];
  const rms = sqrt(s / x.length);
  if (rms < 1e-9) return { signal: Float32Array.from(x), rms, silent: true };
  const g = target / rms;
  const out = new Float32Array(x.length);
  for (let i = 0; i < x.length; i += 1) out[i] = x[i] * g;
  return { signal: out, rms, silent: false };
}

/**
 * Measure a rendered tone on both axes. `bite` is left un-normalized here —
 * it only means something relative to the rest of a measured set, so the
 * caller normalizes across its own catalogue (the space is stimulus-set
 * relative; Wessel is explicit about this).
 */
export function measure(signal, sampleRate) {
  const { signal: eq, rms, silent } = loudnessEqualize(signal);
  if (silent) return { brightness: 0, riseMs: 0, asyncMs: 0, rms, silent: true };
  return {
    brightness: brightness(eq, sampleRate),
    riseMs: riseMs(eq, sampleRate),
    asyncMs: asyncMs(eq, sampleRate),
    rms,
    silent: false,
  };
}

/**
 * Turn raw rise/async measurements into a 0…1 bite score across a set.
 * Attack rate is heard logarithmically, hence the log10.
 */
export function normalizeBite(measurements) {
  const live = measurements.filter((m) => !m.silent);
  if (!live.length) return measurements.map(() => 0);
  const lr = live.map((m) => log10(m.riseMs + 0.5));
  const la = live.map((m) => log10(m.asyncMs + 0.5));
  const span = (v) => [Math.min(...v), Math.max(...v)];
  const [rmin, rmax] = span(lr);
  const [amin, amax] = span(la);
  return measurements.map((m) => {
    if (m.silent) return 0;
    const fast = rmax > rmin ? 1 - (log10(m.riseMs + 0.5) - rmin) / (rmax - rmin) : 0;
    const sync = amax > amin ? 1 - (log10(m.asyncMs + 0.5) - amin) / (amax - amin) : 0;
    // Rise time is the dominant cue; onset asynchrony is the refinement
    // Wessel names but never quantifies.
    return 0.7 * fast + 0.3 * sync;
  });
}
