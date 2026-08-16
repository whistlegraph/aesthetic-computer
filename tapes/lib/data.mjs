// the same tone grid as the picture codec, but each cell is a bit instead of
// a grey. this is the mode a *program* tape wants: a piece and its timeline
// are kilobytes, and kilobytes survive a channel that pixels do not.

import { fft, spectrum, blackmanHarris } from "./fft.mjs";
import { plan, chirp, defaults } from "./spectro.mjs";
import { protect, recover } from "./rs.mjs";

const { cos, sin, PI, log10, pow, round, max, min, abs, floor } = Math;

const ON = 1,
  OFF = pow(10, -30 / 20); // a quiet tone, not silence — nulls ring oddly

export const dataDefaults = { ...defaults, spacing: 2, mark: -15 };

export function encode(bytes, opts = {}) {
  const p = plan({ ...dataDefaults, ...opts });
  const bits = [];
  // a byte of alternating bits, then a 32-bit length, then the payload.
  // `raw` skips it for the protected path, where reed-solomon carries its own.
  const len = bytes.length;
  const framed = opts.raw
    ? [...bytes]
    : [
        0xa5,
        0x5a,
        (len >>> 24) & 255,
        (len >>> 16) & 255,
        (len >>> 8) & 255,
        len & 255,
        ...bytes,
      ];
  for (const b of framed) for (let i = 7; i >= 0; i -= 1) bits.push((b >> i) & 1);

  const cols = [];
  for (let i = 0; i < bits.length; i += p.rows) {
    const col = new Float64Array(p.rows);
    for (let r = 0; r < p.rows; r += 1) col[r] = bits[i + r] ?? 0;
    cols.push(col);
  }

  const body = new Float64Array((cols.length + 2) * p.hop + p.n);
  const re = new Float64Array(p.n),
    im = new Float64Array(p.n);
  const phase = new Float64Array(p.n / 2 + 1);
  for (let k = 0; k <= p.n / 2; k += 1) phase[k] = startPhase(k);

  cols.forEach((col, x) => {
    re.fill(0);
    im.fill(0);
    place(re, im, p.pilotLo, 1, phase, p.n);
    place(re, im, p.pilotHi, 1, phase, p.n);
    for (let r = 0; r < p.rows; r += 1)
      place(re, im, p.hi - r * p.spacing, col[r] ? ON : OFF, phase, p.n);
    fft(re, im, true);
    for (let i = 0; i < p.n; i += 1) body[x * p.hop + i] += re[i];
    for (let k = 0; k <= p.n / 2; k += 1)
      phase[k] = (phase[k] + (2 * PI * p.hop * k) / p.n) % (2 * PI);
  });

  let peak = 0;
  for (let i = 0; i < body.length; i += 1) peak = max(peak, abs(body[i]));
  for (let i = 0; i < body.length; i += 1) body[i] = (body[i] / peak) * 0.7;

  const pre = chirp(p);
  const gap = round(p.rate * 0.05);
  const out = new Float64Array(pre.length + gap + body.length);
  out.set(pre, 0);
  out.set(body, pre.length + gap);
  return {
    samples: out,
    plan: p,
    start: pre.length + gap,
    cols: cols.length,
    bps: p.rows * p.colRate,
  };
}

export function decode(samples, { at, win = "rect", ...opts } = {}) {
  const p = plan({ ...dataDefaults, ...opts });
  const start = at ?? 0;
  // every tone sits exactly on a bin centre and every column is exactly one
  // block long, so a rectangular window leaks nothing at all. it only stays
  // that way while the clock does — tape wobble is what buys blackman-harris
  // its keep.
  const w =
    win === "rect" ? new Float64Array(p.n).fill(1) : blackmanHarris(p.n);
  const frame = new Float64Array(p.n);
  const bits = [];
  const at0 = start;
  let pos = start; // tracked, because a tape does not keep our clock

  const magAt = (off) => {
    for (let i = 0; i < p.n; i += 1) frame[i] = samples[off + i] * w[i];
    return spectrum(frame);
  };

  // the pilots run continuously, so their *level* says nothing about where a
  // block starts — but their *frequency* says how fast the tape is running,
  // and that gives block length for free. no timing search, no drift.
  let scale = 1;
  const span = p.pilotHi - p.pilotLo;

  for (let x = 0; ; x += 1) {
    const off = round(pos);
    if (off < 0 || off + p.n > samples.length) break;
    const mag = magAt(off);

    const lo = peakNear(mag, round(p.pilotLo * scale), p.span ?? 3),
      hi = peakNear(mag, round(p.pilotHi * scale), p.span ?? 3);
    const s = (hi - lo) / span;
    if (isFinite(s) && s > 0.9 && s < 1.1) scale = scale * 0.6 + s * 0.4;

    const gLo = db(mag[round(lo)]),
      gHi = db(mag[round(hi)]);

    for (let r = 0; r < p.rows; r += 1) {
      const k = p.hi - r * p.spacing;
      const at = lo + (k - p.pilotLo) * scale;
      const g = gLo + ((gHi - gLo) * (k - p.pilotLo)) / span;
      bits.push(db(near(mag, at)) - g > p.mark ? 1 : 0);
    }
    pos += p.hop / scale; // a tape running fast makes every block shorter
  }
  void at0;
  void peakMag;

  const bytes = [];
  for (let i = 0; i + 8 <= bits.length; i += 8) {
    let b = 0;
    for (let j = 0; j < 8; j += 1) b = (b << 1) | bits[i + j];
    bytes.push(b);
  }
  if (opts.raw) return { bytes: Uint8Array.from(bytes), bad: false };
  if (bytes[0] !== 0xa5 || bytes[1] !== 0x5a) return { bytes: null, bad: true };
  const len =
    (bytes[2] << 24) | (bytes[3] << 16) | (bytes[4] << 8) | bytes[5];
  return { bytes: Uint8Array.from(bytes.slice(6, 6 + len)), len, bad: false };
}

const db = (m) => 20 * log10(max(m, 1e-12));

function peakMag(mag, k, span) {
  let peak = 0;
  for (let i = k - span; i <= k + span; i += 1) peak = max(peak, mag[i] ?? 0);
  return peak;
}

// sub-bin peak location, by parabola through the winner and its neighbours.
function peakNear(mag, k, span) {
  let best = k,
    peak = -1;
  for (let i = k - span; i <= k + span; i += 1)
    if (mag[i] > peak) {
      peak = mag[i];
      best = i;
    }
  const a = mag[best - 1] ?? 0,
    b = mag[best],
    c = mag[best + 1] ?? 0;
  const d = a - 2 * b + c;
  return d === 0 ? best : best + (0.5 * (a - c)) / d;
}

// read at the (possibly fractional) bin the tone drifted to, interpolating
// rather than reaching for a neighbour — a neighbour is a different bit.
function near(mag, at) {
  const i = floor(at),
    f = at - i;
  return (mag[i] ?? 0) * (1 - f) + (mag[i + 1] ?? 0) * f;
}

function startPhase(k) {
  const x = sin(k * 12.9898) * 43758.5453;
  return (x - floor(x)) * 2 * PI;
}

function place(re, im, k, mag, phase, n) {
  const a = (mag * n) / 2;
  const c = a * cos(phase[k]),
    s = a * sin(phase[k]);
  re[k] += c;
  im[k] += s;
  re[n - k] += c;
  im[n - k] -= s;
}

// ── protected: the mode a program tape should actually ship in ────────────

// the header can't be reed-solomon'd — you need it to know how to decode the
// blocks. so it is repeated nine times and voted on bit by bit, which is
// cheap and survives anything the payload survives.
const HEAD = 8,
  REPEAT = 9;

function header(blocks, len) {
  const h = [0xa5, 0x5a, (blocks >>> 8) & 255, blocks & 255,
    (len >>> 24) & 255, (len >>> 16) & 255, (len >>> 8) & 255, len & 255];
  const out = [];
  for (let i = 0; i < REPEAT; i += 1) out.push(...h);
  return out;
}

function vote(wire) {
  const h = new Uint8Array(HEAD);
  for (let i = 0; i < HEAD; i += 1) {
    for (let bit = 0; bit < 8; bit += 1) {
      let ones = 0;
      for (let r = 0; r < REPEAT; r += 1)
        ones += ((wire[r * HEAD + i] ?? 0) >> bit) & 1;
      if (ones * 2 > REPEAT) h[i] |= 1 << bit;
    }
  }
  return h;
}

export function encodeProtected(bytes, opts = {}) {
  const p = protect(bytes, opts);
  const wire = Uint8Array.from([...header(p.blocks, p.len), ...p.bytes]);
  const enc = encode(wire, { ...opts, raw: true });
  return { ...enc, blocks: p.blocks, overhead: wire.length / bytes.length };
}

export function decodeProtected(samples, opts = {}) {
  const got = decode(samples, { ...opts, raw: true });
  if (!got.bytes) return { bytes: null, bad: true };
  const h = vote(got.bytes);
  if (h[0] !== 0xa5 || h[1] !== 0x5a) return { bytes: null, bad: true };
  const blocks = (h[2] << 8) | h[3];
  const len = (h[4] << 24) | (h[5] << 16) | (h[6] << 8) | h[7];
  if (!blocks || len <= 0) return { bytes: null, bad: true };
  const r = recover(got.bytes.subarray(HEAD * REPEAT), { ...opts, blocks, len });
  return { ...r, bad: false };
}

export function ber(a, b) {
  const n = min(a.length, b.length);
  let wrong = 0;
  for (let i = 0; i < n; i += 1) {
    let x = a[i] ^ b[i];
    while (x) {
      wrong += x & 1;
      x >>= 1;
    }
  }
  return { ber: wrong / (n * 8), wrong, compared: n };
}
