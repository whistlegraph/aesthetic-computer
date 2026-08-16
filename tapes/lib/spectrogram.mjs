// an independent look at what is actually in the audio. this does not use the
// codec's plan or its pilots — it just takes an stft and plots every bin, so
// whatever shows up here is really in the waveform.

import { spectrum, hann } from "./fft.mjs";

const { log10, max, min, round, floor } = Math;

export function render(samples, { n = 2048, hop = 512, rate = 44100, floorDb = -70 } = {}) {
  const w = hann(n);
  const frame = new Float64Array(n);
  const cols = floor((samples.length - n) / hop);
  const bins = n / 2;
  const px = new Float64Array(cols * bins);

  let peak = -Infinity;
  for (let x = 0; x < cols; x += 1) {
    for (let i = 0; i < n; i += 1) frame[i] = samples[x * hop + i] * w[i];
    const mag = spectrum(frame);
    for (let k = 0; k < bins; k += 1) {
      const v = 20 * log10(max(mag[k], 1e-12));
      px[x * bins + k] = v;
      peak = max(peak, v);
    }
  }

  // bottom row of the image is the bottom of the spectrum.
  const data = new Uint8Array(cols * bins * 4).fill(255);
  for (let x = 0; x < cols; x += 1)
    for (let k = 0; k < bins; k += 1) {
      const norm = min(1, max(0, (px[x * bins + k] - peak - floorDb) / -floorDb));
      const i = ((bins - 1 - k) * cols + x) * 4;
      const [r, g, b] = heat(norm);
      data[i] = r;
      data[i + 1] = g;
      data[i + 2] = b;
    }
  return { width: cols, height: bins, data, rate, nyquist: rate / 2 };
}

// dark blue → magenta → white. keeps the quiet end legible without washing
// out the loud end, which matters when the point is to see faint structure.
function heat(t) {
  const c = min(1, max(0, t));
  const r = round(255 * min(1, c * 2.2));
  const g = round(255 * max(0, min(1, c * 2.0 - 0.9)));
  const b = round(255 * (c < 0.5 ? c * 1.6 : 0.8 + (c - 0.5) * 0.4));
  return [r, g, b];
}
