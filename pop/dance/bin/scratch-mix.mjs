// Apply the gnarly center turntable scratch to a WHOLE stereo mix
// (jas: "i still want the scratch to effect everything"). Decode via
// ffmpeg → f32le stereo, scratch the centre, re-encode. Same gesture
// as the engine: 9 beat-snapped deep accelerating RIP + fast TEAR
// strokes, transformer chop, raised-cosine edges (endpoints stay
// sample-aligned so length/continuity around the window is intact).
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, unlinkSync } from "node:fs";

const [inWav, outWav, stampWav, structPath] = process.argv.slice(2);

// Super-sampled read: 4-point Catmull-Rom interpolation (much less
// aliasing than linear when pitch-shifting — jas: "with super
// sampling"). a = mono Float32, i = integer index, f = 0..1 frac.
function c4(a, i, f) {
  const N = a.length;
  const x0 = a[i > 0 ? i - 1 : 0];
  const x1 = a[i];
  const x2 = a[i + 1 < N ? i + 1 : N - 1];
  const x3 = a[i + 2 < N ? i + 2 : N - 1];
  const c0 = -0.5 * x0 + 1.5 * x1 - 1.5 * x2 + 0.5 * x3;
  const c1 = x0 - 2.5 * x1 + 2 * x2 - 0.5 * x3;
  const c2 = -0.5 * x0 + 0.5 * x2;
  return ((c0 * f + c1) * f + c2) * f + x1;
}

// SKRILL GROWL — FM wub with a moving formant + grit, tuned low in G
// (jas: "within the glitches can we throw in some skrill growls?").
// Synthesised straight into the interleaved buffer `x`, additive.
function skrillGrowl(x, frames, sr, a0, nF, midi, gain) {
  const f0 = 440 * Math.pow(2, (midi - 69) / 12);
  const wub = 4 + Math.random() * 8;            // 4–12 Hz wub
  const ratio = Math.random() < 0.5 ? 2 : 3;
  const fcLo = 240, fcHi = 1900, fLfo = 0.6 + Math.random() * 1.4;
  let pc = 0, pm = 0, lp = 0, bp = 0;
  const atk = Math.floor(0.008 * sr), rel = Math.floor(0.05 * sr);
  for (let k = 0; k < nF; k++) {
    if (a0 + k >= frames) break;
    const t = k / sr;
    const idx = 3.2 + 3.2 * (0.5 + 0.5 * Math.sin(2 * Math.PI * wub * t));
    pc += (2 * Math.PI * f0) / sr;
    pm += (2 * Math.PI * f0 * ratio) / sr;
    let s = Math.tanh(Math.sin(pc + idx * Math.sin(pm)) * 1.9);
    // swept state-variable band-pass = the "talking" formant
    const fc = fcLo + (fcHi - fcLo) * (0.5 + 0.5 * Math.sin(2 * Math.PI * fLfo * t));
    const fcoef = 2 * Math.sin((Math.PI * Math.min(0.45 * sr, fc)) / sr);
    const hp = s - lp - 1.2 * bp;
    bp += fcoef * hp;
    lp += fcoef * bp;
    let env = 1;
    if (k < atk) env = k / atk;
    else if (k > nF - rel) env = Math.max(0, (nF - k) / rel);
    const v = (0.6 * bp + 0.4 * s) * env * gain;
    x[(a0 + k) * 2] += v;
    x[(a0 + k) * 2 + 1] += v;
  }
}
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
  const NSL = 2;                            // fewer — sparing (jas)
  const scratchLo = w0 - SR, scratchHi = w0 + winN + SR;
  // Keep slides OUT of 70–110 s (jas: "the 1:19 / 1:35 scratch can go
  // away") and away from the centre scratch.
  const exLo = 70 * SR, exHi = 110 * SR;
  for (let n = 0; n < NSL; n++) {
    const durF = Math.floor((0.25 + Math.random() * 0.75) * SR);
    let a0 = Math.floor((10 + Math.random() * (frames / SR - 24)) * SR);
    if (a0 + durF > scratchLo && a0 < scratchHi) a0 = (scratchHi + Math.floor(Math.random() * 6 * SR)) % Math.max(1, frames - durF - SR);
    if (a0 + durF > exLo && a0 < exHi) a0 = (exHi + Math.floor(Math.random() * 8 * SR)) % Math.max(1, frames - durF - SR);
    a0 = Math.max(SR, Math.min(frames - durF - SR, a0));
    if (a0 + durF > exLo && a0 < exHi) continue; // still inside → skip this one
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

// BEAT-LOCKED BREAKBEAT — musical chop&screw (jas: "not musical ...
// persists soooo long ... sprinkled in ... on beats fr half bars ...
// crescendo then dissipate before the last 20 s ... hip hop style ...
// it slides ... pop n pop n ... break beats with it"). Anchors come
// from struct.json's real kick/snare onsets (the actual beat grid):
// at sprinkled anchors a SHORT (1 beat / half-bar) burst either tiles
// a beat-subdivision grain ("pop n pop n") or screw-slides the beat
// (pitch glide). Probability CRESCENDOS through the back, then is
// fully gone before the last 20 s so the fade stays clean. Clean
// audio between bursts = sprinkled, never a wall.
if (structPath) {
  let anchors = [];
  try {
    const sj = JSON.parse(readFileSync(structPath, "utf8"));
    const seen = new Set();
    for (const e of sj.events?.kick ?? []) {
      const q = Math.round(e.t * 200) / 200; // ~5 ms dedupe
      if (!seen.has(q)) { seen.add(q); anchors.push(e.t); }
    }
    anchors.sort((a, b) => a - b);
  } catch {}
  const tEnd = frames / SR;
  const chopFrom = Math.max(0, tEnd - 80);  // longer, gentler lead-in
  const chopTo = tEnd - 20;                 // dissipate BEFORE the fade
  const xf = Math.max(4, Math.floor(0.0025 * SR));
  for (let ai = 0; ai < anchors.length - 1; ai++) {
    const tA = anchors[ai];
    if (tA < chopFrom || tA >= chopTo) continue;
    const prog = (tA - chopFrom) / (chopTo - chopFrom);
    // SPARING crescendo in, taper out over the section's last 5 s
    // basic, gradual ramp UP into the glitches (jas: "more basic ramp
    // up to the glitches" — fixes the hard ~2:02 transition)
    const p = (0.02 + 0.26 * Math.pow(prog, 1.5)) * Math.min(1, (chopTo - tA) / 5);
    if (Math.random() > p) continue;        // sprinkled — rarely
    let unit = anchors[ai + 1] - tA;        // local beat
    if (!(unit > 0.12 && unit < 0.95)) unit = 0.34;
    const span = unit * (Math.random() < 0.6 ? 0.5 : 1); // SHORT: ½ beat / 1 beat
    const a0 = Math.floor(tA * SR);
    const wN = Math.min(Math.floor(span * SR), frames - 1 - a0);
    if (wN < 256) continue;
    // capture enough for the burst + a pitch/echo-out tail
    const tailN = Math.floor((0.18 + Math.random() * 0.30) * SR);
    const cap = Math.min(wN + 8, frames - a0);
    const sL = new Float32Array(cap), sR = new Float32Array(cap);
    for (let k = 0; k < cap; k++) { sL[k] = x[(a0 + k) * 2]; sR[k] = x[(a0 + k) * 2 + 1]; }
    const slide = Math.random() < 0.4;
    const flangeOut = Math.random() < 0.4;   // jas: "flange out at times"
    const fEdge = Math.min(Math.floor(0.003 * SR), wN >> 2);
    if (slide) {
      // hip-hop screw-slide: read the captured beat at a ramping rate
      const rEnd = Math.random() < 0.7 ? 0.78 + Math.random() * 0.14   // gentler down
                                       : 1.14 + Math.random() * 0.16;  // up
      let rp = 0;
      for (let k = 0; k < wN; k++) {
        const r = 1 + (rEnd - 1) * (k / wN);
        const i0 = Math.floor(rp), fr = rp - i0;
        const L = c4(sL, i0, fr);
        const Rr = c4(sR, i0, fr);
        const e = Math.min(1, Math.min(k, wN - 1 - k) / Math.max(1, fEdge));
        x[(a0 + k) * 2] = x[(a0 + k) * 2] * (1 - e) + L * e;
        x[(a0 + k) * 2 + 1] = x[(a0 + k) * 2 + 1] * (1 - e) + Rr * e;
        rp += r; if (rp > cap - 2) rp = cap - 2;
      }
    } else {
      // pop n pop n: retrigger a grain — sometimes down to ~32nds
      const sub = Math.random() < 0.3 ? 5 + Math.floor(Math.random() * 4) // 5..8 (fine, ~32nd)
                                      : 2 + Math.floor(Math.random() * 3); // 2..4
      const gN = Math.max(Math.floor(0.014 * SR), Math.floor(wN / sub));
      for (let k = 0; k < wN; k++) {
        const gi = k % gN;
        let L = sL[Math.min(cap - 1, gi)], Rr = sR[Math.min(cap - 1, gi)];
        if (gi < xf) { const f = gi / xf; L *= f; Rr *= f; }
        else if (gi > gN - xf) { const f = (gN - gi) / xf; L *= f; Rr *= f; }
        const e = Math.min(1, Math.min(k, wN - 1 - k) / Math.max(1, fEdge));
        x[(a0 + k) * 2] = x[(a0 + k) * 2] * (1 - e) + L * e;
        x[(a0 + k) * 2 + 1] = x[(a0 + k) * 2 + 1] * (1 - e) + Rr * e;
      }
    }
    // SKRILL GROWL within the glitches — sprinkled, denser later, tuned
    // to G (track is G major). Layered under the burst.
    if (prog > 0.3 && Math.random() < 0.32) {
      skrillGrowl(x, frames, SR, a0, wN + Math.floor(0.12 * SR),
                  Math.random() < 0.5 ? 31 : 43, 0.5);
    }
    // PITCH-OUT / ECHO-OUT tail (jas: "they could even pitch out and
    // echo out") — re-read the captured beat slowing down (pitch
    // descends) with an exponential decay, mixed gently ON TOP of the
    // dry that resumes, so it dissolves instead of hard-cutting.
    {
      const t0 = a0 + wN;
      const tN = Math.min(tailN, frames - 1 - t0);
      let rp = 0;
      for (let k = 0; k < tN; k++) {
        const r = 1 - 0.45 * (k / tN);            // 1.0 → 0.55 (pitch out)
        const i0 = Math.floor(rp), fr = rp - i0;
        const g = Math.pow(1 - k / tN, 1.8) * 0.6; // echo decay
        let L = c4(sL, i0, fr), Rr = c4(sR, i0, fr);
        if (flangeOut) {
          // swept comb — flanges out, deeper toward the tail end
          const dep = (0.0006 + 0.0022 * (k / tN)) * SR;   // 0.6 → 2.8 ms
          const dl = dep * (0.5 + 0.5 * Math.sin(2 * Math.PI * 0.5 * k / SR));
          const jp = rp - dl;
          if (jp > 1) {
            const j0 = Math.floor(jp), jf = jp - j0;
            L = 0.6 * L + 0.6 * c4(sL, j0, jf);
            Rr = 0.6 * Rr + 0.6 * c4(sR, j0, jf);
          }
        }
        x[(t0 + k) * 2] += L * g;
        x[(t0 + k) * 2 + 1] += Rr * g;
        rp += r; if (rp > cap - 2) rp = cap - 2;
      }
    }
  }
}

writeFileSync(rawOut, Buffer.from(x.buffer, x.byteOffset, x.byteLength));
spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "2", "-i", rawOut,
  "-ar", String(SR), "-sample_fmt", "s16", outWav]);
try { unlinkSync(rawIn); unlinkSync(rawOut); } catch {}
console.log(`scratched whole mix @ ~${(frames / 2 / SR).toFixed(1)}s -> ${outWav}`);
