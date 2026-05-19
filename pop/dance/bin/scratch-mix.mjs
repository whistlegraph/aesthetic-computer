// Apply the gnarly center turntable scratch to a WHOLE stereo mix
// (jas: "i still want the scratch to effect everything"). Decode via
// ffmpeg → f32le stereo, scratch the centre, re-encode. Same gesture
// as the engine: 9 beat-snapped deep accelerating RIP + fast TEAR
// strokes, transformer chop, raised-cosine edges (endpoints stay
// sample-aligned so length/continuity around the window is intact).
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, unlinkSync } from "node:fs";

const [inWav, outWav, stampWav] = process.argv.slice(2);
const SR = 44100;
const rawIn = "/tmp/.scr-in.f32", rawOut = "/tmp/.scr-out.f32";

spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", inWav,
  "-f", "f32le", "-ar", String(SR), "-ac", "2", rawIn]);
const buf = readFileSync(rawIn);
const x = new Float32Array(buf.buffer, buf.byteOffset, buf.byteLength / 4);
const frames = Math.floor(x.length / 2);

const winN = Math.min(Math.floor(3.0 * SR), frames - 2);
const w0 = Math.max(0, Math.floor(frames / 2) - Math.floor(winN / 2));
const guard = Math.floor(1.0 * SR);
const g0 = Math.max(0, w0 - guard);
const g1 = Math.min(frames, w0 + winN + guard);
const N = g1 - g0;
const sl = new Float32Array(N), sr = new Float32Array(N);
for (let k = 0; k < N; k++) { sl[k] = x[(g0 + k) * 2]; sr[k] = x[(g0 + k) * 2 + 1]; }

const STROKES = 9, strokeLen = winN / STROKES, A = 0.62 * SR, baseOff = w0 - g0;
for (let k = 0; k < winN; k++) {
  const tau = k / winN;
  const env = 0.5 - 0.5 * Math.cos(2 * Math.PI * tau);
  const s = Math.floor(k / strokeLen);
  const u = (k - s * strokeLen) / strokeLen;
  let exc;
  if (u < 0.7) exc = Math.pow(u / 0.7, 1.8);
  else { const tu = (u - 0.7) / 0.3; exc = (1 - tu) * (1 - tu); }
  const deepen = 0.45 + 0.55 * env;
  const dir = s % 2 === 0 ? 1 : 0.62;
  const drift = 0.18 * SR * Math.sin(Math.PI * tau);
  let srcF = baseOff + k + drift + A * exc * deepen * dir * env;
  if (srcF < 0) srcF = 0; else if (srcF > N - 2) srcF = N - 2;
  const i0 = Math.floor(srcF), frc = srcF - i0;
  const L = sl[i0] * (1 - frc) + sl[i0 + 1] * frc;
  const R = sr[i0] * (1 - frc) + sr[i0 + 1] * frc;
  let chop;
  if (u < 0.7) { const tr = Math.sin(2 * Math.PI * (k / SR) * 41) > -0.25 ? 1 : 0.35; chop = 0.58 + 0.42 * tr; }
  else chop = 0.16;
  const gmix = (1 - env) + env * chop;
  x[(w0 + k) * 2] = L * gmix;
  x[(w0 + k) * 2 + 1] = R * gmix;
}

// CLEAN "aesthetic dot computer" ID — overlaid dead-centre ON TOP of
// the just-scratched mix (jas: "should not be scratched ... overlay
// over the scratch", and confirmed inaudible when buried in the
// low-passed fold). Full-range, loud enough to clearly hear over the
// dominant sine. Mixed here (post-scratch) so it is NOT scratched; the
// final master then normalises it together with the program.
if (stampWav) {
  const rawSt = "/tmp/.scr-stamp.f32";
  spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", stampWav,
    "-f", "f32le", "-ar", String(SR), "-ac", "1", rawSt]);
  const sb = readFileSync(rawSt);
  const st = new Float32Array(sb.buffer, sb.byteOffset, sb.byteLength / 4);
  let pk = 0;
  for (let i = 0; i < st.length; i++) { const a = Math.abs(st[i]); if (a > pk) pk = a; }
  const norm = pk > 0 ? 0.92 / pk : 1;
  const PITCH = 0.95, GAIN = 0.85;            // near-natural + loud = clearly audible
  const outLen = Math.floor(st.length / PITCH);
  const fz = Math.floor(0.03 * SR);
  const start = Math.floor(frames / 2) - Math.floor(outLen / 2);
  for (let i = 0; i < outLen; i++) {
    const j = start + i;
    if (j < 0 || j >= frames) continue;
    let f = 1;
    if (i < fz) f = i / fz;
    else if (i > outLen - fz) f = Math.max(0, (outLen - i) / fz);
    const v = (st[Math.floor(i * PITCH)] || 0) * norm * GAIN * f;
    x[j * 2] += v;
    x[j * 2 + 1] += v;
  }
  try { unlinkSync(rawSt); } catch {}
  console.log(`clean ID overlaid @ ~${(frames / 2 / SR).toFixed(1)}s (post-scratch, GAIN ${GAIN})`);
}

// CHAOTIC PITCH-SLIDES — ~4 short gestures that "pick a tone and run
// after it before snapping back" (jas), 0.25–1 s, STOCHASTIC on every
// generation (Math.random, not seeded). Raised-cosine endpoints → no
// length change / clicks; avoids the centre-scratch window.
{
  const NSL = 4;
  const scratchLo = w0 - SR, scratchHi = w0 + winN + SR;
  for (let n = 0; n < NSL; n++) {
    const durF = Math.floor((0.25 + Math.random() * 0.75) * SR);
    let a0 = Math.floor((10 + Math.random() * (frames / SR - 24)) * SR);
    if (a0 + durF > scratchLo && a0 < scratchHi) a0 = (scratchHi + Math.floor(Math.random() * 6 * SR)) % Math.max(1, frames - durF - SR);
    a0 = Math.max(SR, Math.min(frames - durF - SR, a0));
    const dep = (0.05 + Math.random() * 0.30) * SR * (Math.random() < 0.5 ? -1 : 1);
    const gd = Math.ceil(Math.abs(dep)) + 4;
    const lo = Math.max(0, a0 - gd), hi = Math.min(frames, a0 + durF + gd), M = hi - lo;
    const cl = new Float32Array(M), cr = new Float32Array(M);
    for (let k = 0; k < M; k++) { cl[k] = x[(lo + k) * 2]; cr[k] = x[(lo + k) * 2 + 1]; }
    for (let k = 0; k < durF; k++) {
      const tau = k / durF;
      let sh; // run out, then a fast snap back to real-time
      if (tau < 0.78) sh = Math.pow(tau / 0.78, 1.6);
      else { const s = (tau - 0.78) / 0.22; sh = Math.pow(1 - s, 2); }
      const edge = Math.min(1, Math.min(k, durF - 1 - k) / 200);
      let srcF = (a0 - lo) + k + dep * sh;
      if (srcF < 0) srcF = 0; else if (srcF > M - 2) srcF = M - 2;
      const i0 = Math.floor(srcF), fr = srcF - i0;
      const L = cl[i0] * (1 - fr) + cl[i0 + 1] * fr;
      const R = cr[i0] * (1 - fr) + cr[i0 + 1] * fr;
      const mix = edge;
      x[(a0 + k) * 2] = x[(a0 + k) * 2] * (1 - mix) + L * mix;
      x[(a0 + k) * 2 + 1] = x[(a0 + k) * 2 + 1] * (1 - mix) + R * mix;
    }
  }
}

writeFileSync(rawOut, Buffer.from(x.buffer, x.byteOffset, x.byteLength));
spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawOut,
  "-ar", String(SR), "-sample_fmt", "s16", outWav]);
try { unlinkSync(rawIn); unlinkSync(rawOut); } catch {}
console.log(`scratched whole mix @ ~${(frames / 2 / SR).toFixed(1)}s -> ${outWav}`);
