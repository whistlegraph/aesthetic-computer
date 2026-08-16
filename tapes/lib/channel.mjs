// things that happen to audio on its way somewhere. each one takes samples
// and gives samples back, so they compose into a route.

import { execFileSync } from "node:child_process";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import * as wav from "./wav.mjs";

const { sin, cos, PI, sqrt, random, floor, max, min, abs, round, tanh, pow } =
  Math;

// hiss is seeded by default. an unseeded run makes every measurement a
// single sample of a random variable, which is how a marginal result gets
// mistaken for a fixed one — `seed(null)` opts back into real randomness,
// and `trials()` is how you should ask whether something actually works.
let rng = mulberry(0x5eed);

export function seed(n) {
  rng = n === null ? random : mulberry(n);
}

function mulberry(a) {
  return () => {
    a |= 0;
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export function noise(x, snrDb) {
  let power = 0;
  for (let i = 0; i < x.length; i += 1) power += x[i] * x[i];
  power /= x.length;
  const amp = sqrt(power / pow(10, snrDb / 10));
  const out = new Float64Array(x.length);
  for (let i = 0; i < x.length; i += 1) {
    // box-muller, so the hiss is actually gaussian
    const u = rng() || 1e-12;
    out[i] = x[i] + amp * sqrt(-2 * Math.log(u)) * cos(2 * PI * rng());
  }
  return out;
}

// run something across n different hiss draws and report the spread.
export function trials(n, fn) {
  const out = [];
  for (let i = 0; i < n; i += 1) {
    seed(0x5eed + i);
    out.push(fn(i));
  }
  seed(0x5eed);
  return out;
}

export function quantize(x, bits) {
  const peak = pow(2, bits - 1) - 1;
  const out = new Float64Array(x.length);
  for (let i = 0; i < x.length; i += 1)
    out[i] = round(max(-1, min(1, x[i])) * peak) / peak;
  return out;
}

// one-pole cascade. crude, but tape rolloff is crude.
export function lowpass(x, hz, rate, poles = 4) {
  const a = 1 - Math.exp((-2 * PI * hz) / rate);
  let out = Float64Array.from(x);
  for (let p = 0; p < poles; p += 1) {
    let z = 0;
    for (let i = 0; i < out.length; i += 1) {
      z += a * (out[i] - z);
      out[i] = z;
    }
  }
  return out;
}

export function highpass(x, hz, rate) {
  const lo = lowpass(x, hz, rate, 1);
  const out = new Float64Array(x.length);
  for (let i = 0; i < x.length; i += 1) out[i] = x[i] - lo[i];
  return out;
}

export function saturate(x, drive = 1.5) {
  const out = new Float64Array(x.length);
  for (let i = 0; i < x.length; i += 1) out[i] = tanh(x[i] * drive) / tanh(drive);
  return out;
}

export function gain(x, g) {
  const out = new Float64Array(x.length);
  for (let i = 0; i < x.length; i += 1) out[i] = x[i] * g;
  return out;
}

// speed that wanders (wow, ~0.5-6Hz) and shivers (flutter, ~10-100Hz),
// read back with cubic interpolation. this is the one impairment a tape has
// that a file never does.
export function wowFlutter(x, rate, { wow = 0.003, flutter = 0.0008 } = {}) {
  const out = new Float64Array(x.length);
  let t = 0;
  for (let i = 0; i < out.length; i += 1) {
    const s = i / rate;
    const rateMul =
      1 +
      wow * sin(2 * PI * 1.7 * s) +
      wow * 0.5 * sin(2 * PI * 4.3 * s + 1.1) +
      flutter * sin(2 * PI * 34 * s) +
      flutter * 0.7 * sin(2 * PI * 61 * s + 0.6);
    out[i] = cubic(x, t);
    t += rateMul;
  }
  return out;
}

function cubic(x, t) {
  const i = floor(t),
    f = t - i;
  const p = (k) => x[min(x.length - 1, max(0, k))] ?? 0;
  const a = p(i - 1),
    b = p(i),
    c = p(i + 1),
    d = p(i + 2);
  return (
    b +
    0.5 *
      f *
      (c - a + f * (2 * a - 5 * b + 4 * c - d + f * (3 * (b - c) + d - a)))
  );
}

// resample by an arbitrary ratio — stands in for a 44.1↔48 round trip.
export function resample(x, ratio) {
  const out = new Float64Array(floor(x.length / ratio));
  for (let i = 0; i < out.length; i += 1) out[i] = cubic(x, i * ratio);
  return out;
}

// a composite compact-cassette route: band limit, speed wander, hiss,
// a little tape compression, and the level mismatch of a real deck.
export function cassette(x, rate, { snr = 50, wow = 0.003, hiss = true } = {}) {
  let y = highpass(x, 40, rate);
  y = lowpass(y, 14000, rate, 3);
  y = wowFlutter(y, rate, { wow, flutter: wow / 4 });
  y = saturate(y, 1.3);
  if (hiss) y = noise(y, snr);
  return y;
}

// ── real codecs, via ffmpeg ───────────────────────────────────────────────

export function transcode(x, rate, spec) {
  const dir = mkdtempSync(join(tmpdir(), "tapes-"));
  try {
    const src = join(dir, "in.wav"),
      mid = join(dir, "mid." + spec.ext),
      dst = join(dir, "out.wav");
    wav.write(src, x, rate);
    execFileSync("ffmpeg", ["-y", "-i", src, ...spec.args, mid], {
      stdio: "ignore",
    });
    execFileSync("ffmpeg", ["-y", "-i", mid, "-ar", String(rate), "-ac", "1", dst], {
      stdio: "ignore",
    });
    return wav.read(dst).samples;
  } finally {
    rmSync(dir, { recursive: true, force: true });
  }
}

export const codecs = {
  "mp3 320": { ext: "mp3", args: ["-c:a", "libmp3lame", "-b:a", "320k"] },
  "mp3 192": { ext: "mp3", args: ["-c:a", "libmp3lame", "-b:a", "192k"] },
  "mp3 128": { ext: "mp3", args: ["-c:a", "libmp3lame", "-b:a", "128k"] },
  "aac 256": { ext: "m4a", args: ["-c:a", "aac", "-b:a", "256k"] },
  "aac 128": { ext: "m4a", args: ["-c:a", "aac", "-b:a", "128k"] },
  "opus 128": { ext: "opus", args: ["-c:a", "libopus", "-b:a", "128k"] },
  "opus 64": { ext: "opus", args: ["-c:a", "libopus", "-b:a", "64k"] },
};
