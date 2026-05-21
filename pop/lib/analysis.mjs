// analysis.mjs — audio → control-signal primitives for /pop. Turns a
// rendered buffer into something musical you can act on: a pitch curve,
// or a list of onset triggers.
//
// Companion to the envelope follower in dance/synths/fx.mjs — together
// these are the `analysis` category of the pop menu.
//
//   readWavMono(path) -> { samples: Float32Array, sampleRate }
//   pitchTrack(buf, opts) -> [{ time, hz, midi, clarity }]
//   audioGate(buf, opts)  -> [{ time, level }]

import { readFileSync } from "node:fs";

const DEFAULT_SAMPLE_RATE = 48_000;

// ── WAV reader ────────────────────────────────────────────────────────
// Reads a RIFF/WAVE file to a mono Float32Array. Supports 16-bit and
// 32-bit-float PCM; stereo (or more) is downmixed by averaging channels.
export function readWavMono(path) {
  const buf = readFileSync(path);
  if (buf.toString("ascii", 0, 4) !== "RIFF")
    throw new Error(`not a RIFF file: ${path}`);

  let channels = 1, sampleRate = DEFAULT_SAMPLE_RATE, bits = 16, fmt = 1;
  let i = 12;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4);
    const size = buf.readUInt32LE(i + 4);
    const body = i + 8;
    if (id === "fmt ") {
      fmt        = buf.readUInt16LE(body);
      channels   = buf.readUInt16LE(body + 2);
      sampleRate = buf.readUInt32LE(body + 4);
      bits       = buf.readUInt16LE(body + 14);
    } else if (id === "data") {
      const bytesPerSample = bits / 8;
      const frames = Math.floor(size / (bytesPerSample * channels));
      const out = new Float32Array(frames);
      for (let f = 0; f < frames; f++) {
        let acc = 0;
        for (let c = 0; c < channels; c++) {
          const o = body + (f * channels + c) * bytesPerSample;
          if (fmt === 3 && bits === 32)      acc += buf.readFloatLE(o);
          else if (bits === 32)              acc += buf.readInt32LE(o) / 2147483648;
          else if (bits === 24)              acc += ((buf.readUInt8(o) | (buf.readUInt8(o + 1) << 8) | (buf.readInt8(o + 2) << 16))) / 8388608;
          else                               acc += buf.readInt16LE(o) / 32768;
        }
        out[f] = acc / channels;
      }
      return { samples: out, sampleRate };
    }
    i = body + size + (size & 1); // chunks are word-aligned
  }
  throw new Error(`no data chunk in ${path}`);
}

// ── pitch tracking ────────────────────────────────────────────────────
// Frame-by-frame fundamental-frequency detection by normalized
// autocorrelation. Returns one row per hop with the detected pitch in
// Hz, the nearest (fractional) MIDI note, and a 0..1 clarity score —
// silent / unpitched frames come back with hz = null.
//
// opts: {
//   sampleRate (default 48000)
//   fmin, fmax  Hz search range  (default 65 .. 1200)
//   hopMs       frame stride     (default 10)
//   winMs       analysis window  (default 40)
//   rmsGate     silence floor    (default 0.01)
//   clarityGate min peak to call (default 0.5)
// }
export function pitchTrack(buf, opts = {}) {
  if (!(buf instanceof Float32Array)) return [];
  const sr   = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const fmin = opts.fmin ?? 65;
  const fmax = opts.fmax ?? 1200;
  const hop  = Math.max(1, Math.floor((opts.hopMs ?? 10) * 0.001 * sr));
  const win  = Math.max(64, Math.floor((opts.winMs ?? 40) * 0.001 * sr));
  const rmsGate     = opts.rmsGate ?? 0.01;
  const clarityGate = opts.clarityGate ?? 0.5;
  const lagMin = Math.floor(sr / fmax);
  const lagMax = Math.min(Math.floor(sr / fmin), Math.floor(win / 2));

  const frames = [];
  for (let start = 0; start + win <= buf.length; start += hop) {
    const time = start / sr;

    let rms = 0;
    for (let j = 0; j < win; j++) rms += buf[start + j] * buf[start + j];
    rms = Math.sqrt(rms / win);
    if (rms < rmsGate) { frames.push({ time, hz: null, midi: null, clarity: 0 }); continue; }

    // energy at zero lag, for normalization
    const ac = (lag) => {
      let s = 0;
      for (let j = 0; j < win - lag; j++) s += buf[start + j] * buf[start + j + lag];
      return s;
    };
    const ac0 = ac(0) || 1e-9;

    let bestLag = lagMin, bestScore = -Infinity;
    for (let lag = lagMin; lag <= lagMax; lag++) {
      const score = ac(lag) / ac0;
      if (score > bestScore) { bestScore = score; bestLag = lag; }
    }

    // parabolic interpolation for sub-sample lag precision
    let lagF = bestLag;
    if (bestLag > lagMin && bestLag < lagMax) {
      const a = ac(bestLag - 1), b = ac(bestLag), c = ac(bestLag + 1);
      const denom = a - 2 * b + c;
      if (Math.abs(denom) > 1e-9) lagF = bestLag - 0.5 * (c - a) / denom;
    }

    if (bestScore < clarityGate) {
      frames.push({ time, hz: null, midi: null, clarity: bestScore });
    } else {
      const hz = sr / lagF;
      frames.push({ time, hz, midi: 69 + 12 * Math.log2(hz / 440), clarity: bestScore });
    }
  }
  return frames;
}

// ── audio gate / onset trigger ────────────────────────────────────────
// Follows the signal's amplitude and emits a trigger every time it
// crosses up through `threshold` after dipping below it — a beatbox /
// percussion onset detector. `minGapMs` debounces fast retriggers.
// Feed the trigger list to a sampler to fire drum hits, or to
// audio-to-rhythm to build a .np score.
//
// opts: {
//   sampleRate (default 48000)
//   threshold   open level 0..1   (default 0.06)
//   attackMs    follower attack   (default 2)
//   releaseMs   follower release  (default 40)
//   minGapMs    retrigger lockout (default 60)
// }
export function audioGate(buf, opts = {}) {
  if (!(buf instanceof Float32Array)) return [];
  const sr        = opts.sampleRate ?? DEFAULT_SAMPLE_RATE;
  const threshold = opts.threshold ?? 0.06;
  const minGap    = Math.floor((opts.minGapMs ?? 60) * 0.001 * sr);
  const aA = Math.exp(-1 / (Math.max(0.01, opts.attackMs  ?? 2)  * 0.001 * sr));
  const aR = Math.exp(-1 / (Math.max(0.01, opts.releaseMs ?? 40) * 0.001 * sr));
  // hysteresis: must fall below this before a new trigger can fire
  const releaseLevel = threshold * 0.6;

  const triggers = [];
  let env = 0, armed = true, lastTrig = -Infinity;
  for (let i = 0; i < buf.length; i++) {
    const x = Math.abs(buf[i]);
    const a = x > env ? aA : aR;
    env = a * env + (1 - a) * x;

    if (armed && env >= threshold && i - lastTrig >= minGap) {
      triggers.push({ time: i / sr, level: Math.min(1, env) });
      lastTrig = i;
      armed = false;
    } else if (!armed && env < releaseLevel) {
      armed = true;
    }
  }
  return triggers;
}
