// fx.mjs — buffer post-effects for the dance lane. Wobble (LFO mod) and
// bitcrush (depth + sample-rate reduction), both modulated by envelopes
// so a section can ramp the effect in or out.
//
// All effects mutate the buffer in place over [startSec, endSec]. If
// you want a dry/wet blend, render to a temp buffer first and mix.
//
// Envelopes are { time: secondsFromStart, ...params }[] breakpoints,
// linearly interpolated. A single static value also works.

const DEFAULT_SAMPLE_RATE = 48_000;

function resolveEnv(envOrValue, key, t) {
  if (typeof envOrValue === "number") return envOrValue;
  if (!Array.isArray(envOrValue) || envOrValue.length === 0) return undefined;
  if (t <= envOrValue[0].time) return envOrValue[0][key];
  if (t >= envOrValue[envOrValue.length - 1].time) return envOrValue[envOrValue.length - 1][key];
  for (let i = 0; i < envOrValue.length - 1; i++) {
    const a = envOrValue[i], b = envOrValue[i + 1];
    if (t >= a.time && t <= b.time) {
      const f = (t - a.time) / Math.max(1e-9, b.time - a.time);
      return a[key] + (b[key] - a[key]) * f;
    }
  }
  return envOrValue[envOrValue.length - 1][key];
}

// ── wobble ────────────────────────────────────────────────────────────
// LFO that modulates either amplitude ("amp") or one-pole lowpass cutoff
// ("filter"). Trance + dubstep crossover effect — slow sine LFO at
// ~0.5–8 Hz creates the "wub wub" feel. Both rate (Hz) and depth (0..1)
// can be enveloped over the section.
//
// opts: {
//   target:   "amp" | "filter"           (default "filter")
//   rate:     number | env breakpoints   (Hz, default 2)
//   depth:    number | env breakpoints   (0..1, default 0.6)
//   baseCutoffHz: number                 (filter mode only; default 2000)
//   waveform: "sine" | "tri" | "square"  (default "sine")
//   sampleRate: number                   (default 48000)
//   startSec, endSec: optional region    (default whole buffer)
// }
//
// Envelope rows: { time, rate, depth }
export function applyWobble(buf, opts = {}) {
  if (!(buf instanceof Float32Array)) return;
  const sr        = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const target    = opts.target ?? "filter";
  const waveform  = opts.waveform ?? "sine";
  const startSec  = opts.startSec ?? 0;
  const endSec    = opts.endSec ?? (buf.length / sr);
  const startIdx  = Math.max(0, Math.floor(startSec * sr));
  const endIdx    = Math.min(buf.length, Math.floor(endSec * sr));
  if (endIdx <= startIdx) return;

  const baseCutoff = opts.baseCutoffHz ?? 2000;
  const dt = 1 / sr;

  // One-pole LP state for filter mode.
  let lp = 0;
  let phase = 0;

  for (let i = startIdx; i < endIdx; i++) {
    const t = (i - startIdx) / sr;
    const rate  = resolveEnv(opts.rate  ?? 2.0,  "rate",  t);
    const depth = resolveEnv(opts.depth ?? 0.6,  "depth", t);

    phase += rate * dt;
    if (phase >= 1) phase -= Math.floor(phase);
    let lfo;
    switch (waveform) {
      case "tri":    lfo = Math.abs(phase * 2 - 1) * 2 - 1; break;
      case "square": lfo = phase < 0.5 ? 1 : -1; break;
      default:       lfo = Math.sin(phase * 2 * Math.PI);
    }
    // Map LFO from [-1,1] → [0,1] for depth (so depth=1 fully closes the gate)
    const modUnit = (lfo + 1) * 0.5;

    if (target === "amp") {
      // amp gain swings from (1-depth) to 1.
      const gain = 1 - depth + depth * modUnit;
      buf[i] *= gain;
    } else {
      // filter cutoff swings exponentially across baseCutoff*[1-depth..1]
      // — closing the filter when depth is high.
      const minCut = baseCutoff * (1 - depth);
      const cutoff = minCut + (baseCutoff - minCut) * modUnit;
      const rc = 1 / (2 * Math.PI * Math.max(20, cutoff));
      const alpha = dt / (rc + dt);
      lp += alpha * (buf[i] - lp);
      buf[i] = lp;
    }
  }
}

// ── bitcrush ──────────────────────────────────────────────────────────
// Quantize to `bits` and hold each sample for `downsample` samples
// (sample-rate reduction). Both can be enveloped — e.g. start clean,
// crush hard at the build climax, return to clean on the drop.
//
// opts: {
//   bits:       int | env breakpoints   (1..16, default 8)
//   downsample: int | env breakpoints   (1..N, default 1)
//   mix:        0..1                     (dry/wet, default 1)
//   sampleRate: number                   (default 48000)
//   startSec, endSec: optional region
// }
//
// Envelope rows: { time, bits, downsample }
export function applyBitcrush(buf, opts = {}) {
  if (!(buf instanceof Float32Array)) return;
  const sr       = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const startSec = opts.startSec ?? 0;
  const endSec   = opts.endSec ?? (buf.length / sr);
  const startIdx = Math.max(0, Math.floor(startSec * sr));
  const endIdx   = Math.min(buf.length, Math.floor(endSec * sr));
  if (endIdx <= startIdx) return;
  const mix      = Math.max(0, Math.min(1, opts.mix ?? 1));

  let held = 0;
  let holdCount = 0;

  for (let i = startIdx; i < endIdx; i++) {
    const t = (i - startIdx) / sr;
    const bits = Math.max(1, Math.min(16, Math.round(resolveEnv(opts.bits ?? 8, "bits", t))));
    const ds   = Math.max(1, Math.floor(resolveEnv(opts.downsample ?? 1, "downsample", t)));

    if (holdCount <= 0) {
      const levels = Math.pow(2, bits - 1);
      const q = Math.round(buf[i] * levels) / levels;
      held = Math.max(-1, Math.min(1, q));
      holdCount = ds;
    }
    holdCount--;
    buf[i] = buf[i] * (1 - mix) + held * mix;
  }
}

// ── flange ────────────────────────────────────────────────────────────
// Short delay-line modulated by an LFO, mixed back with the dry signal.
// Fast flange = LFO rate ~4-8 Hz with depth swinging ±3 ms; classic
// trance/jet sweep on supersaw + lead chords.
//
// opts: {
//   rate:        Hz                       (default 5)
//   depthMs:     ms delay swing (peak)    (default 3)
//   baseDelayMs: ms center delay          (default 4)
//   feedback:    -0.95..0.95              (default 0.45)
//   mix:         0..1 (dry/wet)           (default 0.55)
//   sampleRate:  number                   (default 48000)
//   startSec, endSec: optional region
// }
export function applyFlange(buf, opts = {}) {
  if (!(buf instanceof Float32Array)) return;
  const sr          = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const rate        = opts.rate        ?? 5;
  const depthMs     = opts.depthMs     ?? 3;
  const baseDelayMs = opts.baseDelayMs ?? 4;
  const feedback    = Math.max(-0.95, Math.min(0.95, opts.feedback ?? 0.45));
  const mix         = Math.max(0,    Math.min(1,    opts.mix      ?? 0.55));
  const startIdx    = Math.max(0, Math.floor((opts.startSec ?? 0) * sr));
  const endIdx      = Math.min(buf.length, Math.floor((opts.endSec ?? (buf.length / sr)) * sr));
  if (endIdx <= startIdx) return;

  // Delay-line large enough for base + depth + slack.
  const maxDelaySamples = Math.ceil((baseDelayMs + depthMs + 1) * 0.001 * sr) + 4;
  const delayLine = new Float32Array(maxDelaySamples);
  let writeIdx = 0;
  let phase = 0;
  const dt = 1 / sr;

  for (let i = startIdx; i < endIdx; i++) {
    phase += rate * dt;
    if (phase >= 1) phase -= Math.floor(phase);
    const lfo = Math.sin(phase * 2 * Math.PI); // -1..1
    const delaySamples = ((baseDelayMs + lfo * depthMs) * 0.001 * sr);

    // Linear interpolation read from the delay line.
    const readPos = writeIdx - delaySamples;
    const readPosWrapped = (readPos + maxDelaySamples * 2) % maxDelaySamples;
    const r0 = Math.floor(readPosWrapped);
    const r1 = (r0 + 1) % maxDelaySamples;
    const frac = readPosWrapped - r0;
    const delayed = delayLine[r0] * (1 - frac) + delayLine[r1] * frac;

    // Write input + feedback to the delay line, then output dry+wet mix.
    const input = buf[i];
    delayLine[writeIdx] = input + delayed * feedback;
    writeIdx = (writeIdx + 1) % maxDelaySamples;

    buf[i] = input * (1 - mix) + delayed * mix;
  }
}

// ── tape-style soft clip (gentle saturation) ──────────────────────────
// Bonus — useful for taming peaks after sidechain + wobble + bitcrush
// without harsh digital clipping.
export function softClip(buf, drive = 1.0) {
  if (!(buf instanceof Float32Array)) return;
  for (let i = 0; i < buf.length; i++) {
    buf[i] = Math.tanh(buf[i] * drive);
  }
}
