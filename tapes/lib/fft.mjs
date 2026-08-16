// radix-2 fft, in place, on split re/im buffers. length must be a power of two.

const { cos, sin, PI, hypot, max } = Math;

const tables = new Map(); // twiddles memoized per size

function twiddles(n) {
  let t = tables.get(n);
  if (!t) {
    const c = new Float64Array(n >> 1),
      s = new Float64Array(n >> 1);
    for (let i = 0; i < n >> 1; i += 1) {
      const a = (-2 * PI * i) / n;
      c[i] = cos(a);
      s[i] = sin(a);
    }
    t = { c, s };
    tables.set(n, t);
  }
  return t;
}

function bitReverse(re, im, n) {
  for (let i = 1, j = 0; i < n; i += 1) {
    let bit = n >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;
    if (i < j) {
      let t = re[i];
      re[i] = re[j];
      re[j] = t;
      t = im[i];
      im[i] = im[j];
      im[j] = t;
    }
  }
}

export function fft(re, im, inverse = false) {
  const n = re.length;
  const { c, s } = twiddles(n);
  bitReverse(re, im, n);
  for (let len = 2; len <= n; len <<= 1) {
    const half = len >> 1,
      step = n / len;
    for (let i = 0; i < n; i += len) {
      for (let j = 0; j < half; j += 1) {
        const k = j * step;
        const wr = c[k],
          wi = inverse ? -s[k] : s[k];
        const a = i + j,
          b = a + half;
        const vr = re[b] * wr - im[b] * wi;
        const vi = re[b] * wi + im[b] * wr;
        re[b] = re[a] - vr;
        im[b] = im[a] - vi;
        re[a] += vr;
        im[a] += vi;
      }
    }
  }
  if (inverse)
    for (let i = 0; i < n; i += 1) {
      re[i] /= n;
      im[i] /= n;
    }
}

// magnitude spectrum of a real frame. returns n/2+1 bins.
export function spectrum(frame) {
  const n = frame.length;
  const re = Float64Array.from(frame),
    im = new Float64Array(n);
  fft(re, im);
  const mag = new Float64Array((n >> 1) + 1);
  for (let k = 0; k <= n >> 1; k += 1) mag[k] = hypot(re[k], im[k]);
  return mag;
}

export function hann(n) {
  const w = new Float64Array(n);
  for (let i = 0; i < n; i += 1) w[i] = 0.5 - 0.5 * cos((2 * PI * i) / n);
  return w;
}

// hann's first sidelobe is only -31dB down, which caps how dark a pixel can
// read next to a bright one. blackman-harris trades a wider main lobe for
// -92dB sidelobes — the right deal when dynamic range is the whole point.
export function blackmanHarris(n) {
  const a = [0.35875, 0.48829, 0.14128, 0.01168];
  const w = new Float64Array(n);
  for (let i = 0; i < n; i += 1) {
    const t = (2 * PI * i) / n;
    w[i] = a[0] - a[1] * cos(t) + a[2] * cos(2 * t) - a[3] * cos(3 * t);
  }
  return w;
}

export const windows = { hann, blackmanHarris };

// where does b sit inside a? cross-correlation with PHAT weighting — the
// cross-spectrum is divided by its own magnitude, so only phase agreement
// counts. plain correlation just finds the loudest stretch instead.
// `wide` restores full-band whitening — kept so the failure it causes can be
// demonstrated side by side, not because anything should use it.
export function bestLag(a, b, { wide = false } = {}) {
  let n = 1;
  while (n < a.length + b.length) n <<= 1;
  const ar = new Float64Array(n),
    ai = new Float64Array(n);
  const br = new Float64Array(n),
    bi = new Float64Array(n);
  ar.set(a);
  br.set(b);
  fft(ar, ai);
  fft(br, bi);

  // whiten only where the reference actually has energy. full-band PHAT
  // divides by magnitude everywhere, which amplifies whatever noise sits in
  // the bands the reference never occupied — and after a tape rolls off the
  // top and a codec throws away more, that noise wins.
  let refPeak = 0;
  for (let i = 0; i < n; i += 1) refPeak = max(refPeak, hypot(br[i], bi[i]));
  const floor = wide ? 0 : refPeak * 0.05;

  for (let i = 0; i < n; i += 1) {
    if (hypot(br[i], bi[i]) < floor) {
      ar[i] = 0;
      ai[i] = 0;
      continue;
    }
    const r = ar[i] * br[i] + ai[i] * bi[i]; // a * conj(b)
    const m = ai[i] * br[i] - ar[i] * bi[i];
    const w = hypot(r, m) || 1;
    ar[i] = r / w;
    ai[i] = m / w;
  }
  fft(ar, ai, true);
  let best = 0,
    peak = -Infinity;
  for (let i = 0; i < a.length; i += 1) {
    const v = ar[i] * ar[i] + ai[i] * ai[i];
    if (v > peak) {
      peak = v;
      best = i;
    }
  }
  return best;
}
