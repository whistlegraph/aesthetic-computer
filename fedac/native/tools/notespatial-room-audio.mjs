// Electrical preview of the Windows SUB receiver, before room acoustics.
// Same score selection, octave drop, envelope, filter settings and level.
// The compressor is an envelope approximation of Web Audio's compressor.
import { makeSubScore } from './sub-receiver/core.mjs';

export function filterFeed(input, sampleRate, type, hz, q = .707) {
  const w = 2 * Math.PI * hz / sampleRate, c = Math.cos(w), a = Math.sin(w) / (2 * q);
  const high = type === 'highpass', b0 = (1 + (high ? c : -c)) / 2;
  const b1 = high ? -(1 + c) : 1 - c, a0 = 1 + a;
  let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
  for (let n = 0; n < input.length; n++) {
    const x = input[n], y = (b0 * x + b1 * x1 + b0 * x2 + 2 * c * y1 - (1 - a) * y2) / a0;
    input[n] = y; x2 = x1; x1 = x; y2 = y1; y1 = y;
  }
  return input;
}

export function renderSubFeed(score, { sampleRate = 44100, from = 0, to = score.dur, tail = 1.5, level = .25, cutoff = 80 } = {}) {
  if (!(level >= 0 && level <= 1 && cutoff >= 40 && cutoff <= 160 && to > from)) throw Error('Invalid SUB render settings');
  const sub = makeSubScore(score);
  // Render pre-roll so filters and overlapping notes have their actual state
  // when a section starts. No audio before `from` reaches the output.
  const start = Math.max(0, from - 1), size = Math.ceil((to - start + tail) * sampleRate);
  const feed = new Float32Array(size);
  for (const e of sub.events) {
    if (e.t + e.dur <= start || e.t >= to) continue;
    const duration = Math.max(.02, e.dur), attack = Math.min(duration * .3, e.attack || .008);
    const release = Math.min(duration * .6, e.decay || .06), releaseAt = Math.max(attack, duration - release);
    const first = Math.max(0, Math.ceil((start - e.t) * sampleRate)), count = Math.ceil(duration * sampleRate);
    const offset = Math.round((e.t - start) * sampleRate);
    for (let i = first; i < count && offset + i < size; i++) {
      const t = i / sampleRate, phase = t * e.hz, f = phase % 1;
      const env = t < attack ? t / attack : t >= releaseAt ? Math.max(0, (duration - t) / (duration - releaseAt)) : 1;
      const wave = e.wave === 'triangle' ? 1 - 4 * Math.abs(Math.round(phase) - phase)
        : e.wave === 'square' ? (f < .5 ? 1 : -1) : e.wave === 'sawtooth' ? 2 * f - 1 : Math.sin(phase * Math.PI * 2);
      feed[offset + i] += wave * env * e.g;
    }
  }
  filterFeed(feed, sampleRate, 'highpass', 25);
  filterFeed(feed, sampleRate, 'lowpass', cutoff);
  filterFeed(feed, sampleRate, 'lowpass', cutoff);
  let gainDb = 0;
  for (let n = 0; n < feed.length; n++) {
    const db = 20 * Math.log10(Math.max(1e-9, Math.abs(feed[n]))), over = db + 10;
    const target = over < -3 ? 0 : over <= 3 ? (1 / 6 - 1) * (over + 3) ** 2 / 12 : (1 / 6 - 1) * over;
    const smooth = Math.exp(-1 / (sampleRate * (target < gainDb ? .003 : .15)));
    gainDb = target + smooth * (gainDb - target);
    feed[n] = Math.max(-.89, Math.min(.89, feed[n] * 10 ** (gainDb / 20) * level));
  }
  const output = new Float32Array(Math.ceil((to - from + tail) * sampleRate));
  const offset = Math.round((from - start) * sampleRate);
  output.set(feed.subarray(offset, offset + output.length));
  return { feed: output, events: sub.events };
}
