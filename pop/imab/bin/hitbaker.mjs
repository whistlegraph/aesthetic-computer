#!/usr/bin/env node
// hitbaker.mjs — the imab CHART arrangement, calibrated by pop/study.
//
// The single-study chart pass (papers/study-one-step, "This week's
// chart") found five production invariants across the Hot 100 top ten
// and this bake aims at all of them, on the same 124 grid and in the
// same key the takes are sung in:
//
//   LUFS  −8.5      (chart −6.7..−8.9; floor demo1 was −11.7)
//   LRA   ≤ 6       (chart 2.6–6.6 — a wall, not a mountain range)
//   perc  ~0.2      (backbeat every 2 & 4, heavier hats; floor was 0.06)
//   mid   within 7dB (chord stabs + chant keep 1–4k lit under the voice)
//   width 0.7–0.87  (hats/stabs/shaker staged wide; kick/sub/bass/vox mono)
//
// One Step's arrangement lessons ride along: near-full arrival by bar 4,
// a 4-bar break by subtraction on the DOMINANT (G pedal — the hook cycle
// hands it to us), new material in the finale, and the finale is the
// loudest sustained passage (terminal lift).
//
// Form (104 bars ≈ 3:21 + tail):
//   0 intro · 4 VERSE1 · 20 pre · 24 CHORUS1 · 40 verse2 · 52 pre
//   56 CHORUS2 · 72 BREAK (G, kickless) · 76 FINALE (24, loudest)
//   100 peel · 104 last hit
//
// Vocals: chorus doors at 24 / 56 / 76. If the sacred phrase exists it
// is placed there untouched (demo6 gain law); vocal-sets.json addresses
// still resolve (A = bar 24 here). Without a vocal the bake renders as
// the instrumental bed the takes will drop into.
//
//   node pop/imab/bin/hitbaker.mjs
//   → out/imab-hitbaker-demo1.wav + .mp3   (then verify:
//     pop/.venv/bin/python pop/study/study.py out/imab-hitbaker-demo1.wav ...)

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const KIT = resolve(HERE, "../samples/kit");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });

const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const BARS = 104, TAIL = 2.5;
const NT = Math.ceil((BARS * BAR + TAIL) * SR);
const T = (b) => b * BAR;

const readF32 = (wav) => {
  const raw = `${WORK}/.hb.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav,
    "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  const n = Math.floor(b.length / 4);
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + n * 4));
};
const S = {};
for (const name of ["kick", "hat-closed", "hat-open", "shaker", "snare",
  "click-door", "reverse-kick", "reverse-bell"]) S[name] = readF32(`${KIT}/${name}.wav`);
const XY = {};
for (const n of ["C5", "G5", "C6"]) XY[n] = readF32(`${KIT}/xylo-${n}.wav`);

let seed = 808;
const rnd = () => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed / 0x7fffffff - 0.5; };
const eager = () => Math.max(-0.006, Math.min(0.005, rnd() * 0.01));
const repitch = (y, rate) => {
  const n = Math.floor((y.length - 1) / rate), out = new Float32Array(n);
  for (let i = 0; i < n; i++) { const x = i * rate, k = Math.floor(x), f = x - k; out[i] = y[k] * (1 - f) + (y[k + 1] ?? 0) * f; }
  return out;
};
const put = (buf, y, t, g) => {
  const a = Math.floor(t * SR); if (a < 0 || a >= NT) return;
  const z = Math.min(NT, a + y.length);
  for (let j = 0; a + j < z; j++) buf[a + j] += y[j] * g;
};

// ── FORM ──────────────────────────────────────────────────────────────
const SEC = (bar) =>
  bar < 4 ? "intro" : bar < 20 ? "verse1" : bar < 24 ? "pre1"
  : bar < 40 ? "chorus1" : bar < 52 ? "verse2" : bar < 56 ? "pre2"
  : bar < 72 ? "chorus2" : bar < 76 ? "break" : bar < 100 ? "finale"
  : "peel";
const inBreak = (bar) => bar >= 72 && bar < 76;
const CHORUS_DOORS = [24, 56, 76];

// ── HARMONY: verse = README hook cycle (Am Am G G C C F G),
// chorus = the sacred bed's own map (C C F F C C C C), pre = F F G G,
// break = G pedal (the dominant hands the drop back to C) ─────────────
const VERSE8 = ["Am", "Am", "G", "G", "C", "C", "F", "G"];
const CHOR8 = ["C", "C", "F", "F", "C", "C", "C", "C"];
const chord = (bar) => {
  const s = SEC(bar);
  if (s === "intro") return "C";
  if (s === "verse1") return VERSE8[(bar - 4) % 8];
  if (s === "verse2") return VERSE8[(bar - 40) % 8];
  if (s === "pre1" || s === "pre2") return ["F", "F", "G", "G"][bar % 4];
  if (s === "break") return "G";
  if (s === "peel") return "C";
  const base = s === "chorus1" ? 24 : s === "chorus2" ? 56 : 76;
  return CHOR8[(bar - base) % 8];
};
const BASSM = { Am: 45, G: 43, C: 48, F: 41 };            // A2 G2 C3 F2
const SUBM = { Am: 33, G: 31, C: 24, F: 29 };             // A1 G1 C1 F1
const STAB = {                                             // mid-band homes (A5–E6)
  Am: [81, 84, 88], G: [79, 83, 86], C: [79, 84, 88], F: [81, 84, 89] };
const CH = { C: [48, 55, 64, 72, 79], F: [53, 57, 60, 69, 77],
  G: [43, 55, 62, 71, 79], Am: [45, 57, 64, 69, 76] };

// ── BREATH: shallow now — chart LRA is a wall. Last half-bar of each
// 8-bar phrase dips decoratives to 0.68, never deeper ─────────────────
const RESTS = [12, 20, 32, 48, 64, 92].map((d) => [T(d) - BAR / 2, T(d), 0.68]);
const gateAt = (t) => {
  for (const [a, b, depth] of RESTS) if (t >= a && t < b) return depth;
  return 1;
};

// ── KICK: four on the floor from bar 0, out only for the 4-bar break ──
const kickBuf = new Float32Array(NT);
const KICKG = 0.5;
for (let b = 0; b < BARS * 4; b++) {
  const bar = Math.floor(b / 4), beat = b % 4;
  if (inBreak(bar)) continue;
  const g = KICKG * (beat === 0 ? 1.05 : 1) * (SEC(bar) === "finale" ? 1.06 : 1);
  put(kickBuf, S.kick, b * BEAT, g);
}
put(kickBuf, S.kick, T(BARS), 1.0);

const duck = new Float32Array(NT).fill(1);
{
  const win = Math.floor(0.5 * SR), tau = 0.09 * SR;
  for (let b = 0; b < BARS * 4; b++) {
    if (inBreak(Math.floor(b / 4))) continue;
    const at = Math.floor(b * BEAT * SR);
    for (let j = 0; j < win && at + j < NT; j++)
      duck[at + j] = Math.min(duck[at + j], 1 - 0.55 * Math.exp(-j / tau));
  }
}

// ── BACKBEAT: the chart's perc-share lever. Snare+clap stack on 2 & 4
// everywhere but the break; finale doubles the clap ───────────────────
const backBuf = new Float32Array(NT);
const CLAP = repitch(S.snare, 1.19), CLAP2 = repitch(S.snare, 1.31);
// noise body under the tonal snare — the kit's one-shots are modal
// synths and ring pitched, which HPSS (and ears) read as tonal; a real
// backbeat is mostly shaped noise
const NSNARE = (() => {
  const n = Math.floor(0.16 * SR), y = new Float32Array(n);
  let lp = 0, hp = 0;
  const aL = 1 - Math.exp(-2 * Math.PI * 9000 / SR);
  const aH = 1 - Math.exp(-2 * Math.PI * 1500 / SR);
  for (let j = 0; j < n; j++) {
    const w = rnd() * 2;
    lp += aL * (w - lp); hp += aH * (w - hp);
    const t = j / SR;
    y[j] = (lp - hp) * Math.min(t / 0.002, 1) * Math.exp(-t / 0.045);
  }
  return y;
})();
for (let bar = 4; bar < 100; bar++) {
  if (inBreak(bar)) continue;
  const fin = SEC(bar) === "finale";
  for (const beat of [1, 3]) {
    const t = T(bar) + beat * BEAT;
    put(backBuf, S.snare, t + eager(), 3.2);
    put(backBuf, NSNARE, t + eager(), 3.8);
    put(backBuf, CLAP, t + 0.004 + eager(), 2.0);
    if (fin) put(backBuf, CLAP2, t + 0.009 + eager(), 1.6);
  }
}
{ // the roll into the finale door
  const steps = [];
  for (let b = 74; b < 75; b += 0.25) steps.push(b);
  for (let b = 75; b < 76; b += 0.125) steps.push(b);
  for (const b of steps) put(backBuf, S.snare, T(b) + eager(), 0.10 + 0.20 * ((b - 74) / 2) ** 1.4);
  put(backBuf, S["reverse-kick"], T(76) - S["reverse-kick"].length / SR, 0.45);
}

// ── SUB: offbeat tanh sine on the roots, everywhere but the break ─────
const subBuf = new Float32Array(NT);
for (let b = 0; b < 100 * 4; b++) {
  const bar = Math.floor(b / 4);
  if (inBreak(bar) || SEC(bar) === "intro") continue;
  const f = 440 * 2 ** ((SUBM[chord(bar)] - 69) / 12);
  const at = Math.floor((b * BEAT + BEAT / 2) * SR), n = Math.floor(0.34 * BEAT * SR);
  for (let j = 0; j < n && at + j < NT; j++) {
    const t = j / SR;
    const env = Math.min(t / 0.005, 1) * Math.exp(-t / (0.34 * BEAT * 0.55));
    subBuf[at + j] += Math.tanh(2.2 * Math.sin(2 * Math.PI * f * t)) * env * 0.15;
  }
}

// ── BASS: marimba roots, half-note pulse, in from bar 4 ───────────────
const bassBuf = new Float32Array(NT);
for (let bar = 4; bar < 100; bar++) {
  if (inBreak(bar)) continue;
  for (const half of [0, 2])
    mixEventMarimba({ startSec: T(bar) + half * BEAT, midi: BASSM[chord(bar)],
      durSec: 2 * BEAT, gain: 0.18, preset: "bass", decayMul: 0.8 }, bassBuf, { sampleRate: SR });
}

// ── STABS: the mid-band stays home. Offbeat chord stabs (the "and" of
// 2 and beat 4) in A4–E5, vibraphone bite — the chart's lowmid/mid ────
const stabBuf = new Float32Array(NT), stabEchoBuf = new Float32Array(NT);
for (let bar = 4; bar < 100; bar++) {
  if (inBreak(bar)) continue;
  const s = SEC(bar);
  const g = s.startsWith("verse") ? 1.0 : s.startsWith("pre") ? 1.2
    : s === "finale" ? 1.4 : 1.2;
  for (const [beat, gg] of [[1.5, 1], [3, 0.85]]) {
    const t = T(bar) + beat * BEAT;
    for (const midi of STAB[chord(bar)])
      mixEventMarimba({ startSec: t + eager(), midi, durSec: 0.6 * BEAT,
        gain: g * gg * gateAt(t), preset: "vibraphone", decayMul: 0.55,
      }, stabBuf, { sampleRate: SR });
    // the echo throw, 3/16 later, opposite side of the stage
    for (const midi of STAB[chord(bar)])
      mixEventMarimba({ startSec: t + 0.75 * BEAT + eager(), midi: midi + 12,
        durSec: 0.4 * BEAT, gain: g * gg * 0.35 * gateAt(t),
        preset: "vibraphone", decayMul: 0.4 }, stabEchoBuf, { sampleRate: SR });
  }
}

// ── HATS: eighths with a lifted gain law (chart weight), sixteenth
// fills at phrase turns; opens breathe on the offbeats ────────────────
const hatBuf = new Float32Array(NT), openBuf = new Float32Array(NT);
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (bar >= 100) continue;
  if (inBreak(bar) && !off) continue;                    // break keeps offbeats only
  const g = (off ? 3.3 : 1.8) * (SEC(bar) === "finale" ? 1.1 : 1) * gateAt(t);
  put(hatBuf, S["hat-closed"], t + eager(), g);
}
for (let bar = 7; bar < 100; bar += 4) {                 // sixteenth turn fills
  if (inBreak(bar)) continue;
  for (let k = 0; k < 4; k++)
    put(hatBuf, S["hat-closed"], T(bar) + (3 + k / 4) * BEAT + eager(), 0.7 + 0.2 * k);
}
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (!off || bar < 4 || bar >= 100 || inBreak(bar)) continue;
  const beatIdx = Math.floor((e % 8) / 2);
  if (beatIdx === 1 || beatIdx === 3)
    put(openBuf, S["hat-open"], t + eager(), 1.8 * gateAt(t));
}

// ── SHAKER: sixteenths, staged wide by alternating sides ──────────────
const shakL = new Float32Array(NT), shakR = new Float32Array(NT);
for (let s = 4 * 16; s < 100 * 16; s++) {
  const t = s * BEAT / 4, bar = Math.floor(t / BAR);
  const wave = 0.5 + 0.5 * Math.sin(s * Math.PI / 8 + 0.7);
  let g = (0.5 + 0.4 * wave);
  if (inBreak(bar)) g *= 1.25;                           // the break keeps ticking
  put(s % 2 ? shakL : shakR, S.shaker, t - 0.004 + Math.abs(rnd()) * 0.006, g * gateAt(t));
}

// ── WASH: the chart's sustained-brightness bed. Band-passed noise
// sixteenths with a long exhale — the layer our mixes never had ───────
const washBuf = new Float32Array(NT), washBuf2 = new Float32Array(NT);
{
  // white noise → crude 5–12k bandpass (one-pole HP at 5k on noise,
  // then difference against a 12k LP); two independent hits so the two
  // sides of the stage decorrelate honestly
  const mkHit = () => {
    const hitN = Math.floor(0.14 * SR);
    const hit = new Float32Array(hitN);
    let lp1 = 0, lp2 = 0;
    const a1 = 1 - Math.exp(-2 * Math.PI * 5000 / SR);
    const a2 = 1 - Math.exp(-2 * Math.PI * 12000 / SR);
    for (let j = 0; j < hitN; j++) {
      const w = rnd() * 2;
      lp1 += a1 * (w - lp1); lp2 += a2 * (w - lp2);
      const band = lp2 - lp1;
      const t = j / SR;
      hit[j] = band * Math.min(t / 0.004, 1) * Math.exp(-t / 0.09);
    }
    return hit;
  };
  const hitL = mkHit(), hitR = mkHit();
  for (let s = 4 * 16; s < 100 * 16; s++) {
    const t = s * BEAT / 4, bar = Math.floor(t / BAR);
    const on16 = s % 4;                                  // 0=beat 2=offbeat
    const g = (on16 === 2 ? 1.2 : on16 === 0 ? 0.7 : 0.5)
      * (SEC(bar) === "finale" ? 1.25 : inBreak(bar) ? 0.7 : 1);
    put(washBuf, hitL, t + eager(), g * gateAt(t));
    put(washBuf2, hitR, t + eager(), g * 0.85 * gateAt(t));
  }
}

// ── XYLO CHANT: the hook cell echoed through every chorus; the finale
// adds the LIFT answer (new material late — One Step lesson 4) ────────
const xyloBuf = new Float32Array(NT);
const FIG = [
  [0, 1, "C5"], [0, 1.75, "G5"], [0, 2, "C5"], [0, 3, "C5"], [0, 4, "C5"],
  [1, 1.5, "C5"], [1, 2.5, "C6"], [1, 3.5, "C5"], [1, 4, "C5"], [1, 4.5, "C5"],
];
const LIFT = [ // bar-5 flavor: the flare, up and out
  [0, 1, "C5"], [0, 1.75, "G5"], [0, 2, "C6"], [0, 3.5, "C6"], [0, 4, "G5"],
  [1, 1, "C6"], [1, 2, "G5"], [1, 3, "C5"], [1, 4, "G5"],
];
for (const base of [32, 64]) // back half of each chorus
  for (let rep = 0; rep < 4; rep += 2)
    for (const [bo, beat, note] of FIG) {
      const t = T(base + rep + bo) + (beat - 1) * BEAT;
      put(xyloBuf, XY[note], t + eager(), 0.45 * (1 + rnd() * 0.15) * gateAt(t));
    }
for (let base = 84; base < 100; base += 4) {  // the finale answer, new
  for (const [bo, beat, note] of FIG) {
    const t = T(base + bo) + (beat - 1) * BEAT;
    put(xyloBuf, XY[note], t + eager(), 0.5 * (1 + rnd() * 0.12));
  }
  for (const [bo, beat, note] of LIFT) {
    const t = T(base + 2 + bo) + (beat - 1) * BEAT;
    put(xyloBuf, XY[note], t + eager(), 0.5 * (1 + rnd() * 0.12));
  }
}

// ── CHOIR SINES: verse tissue + break swell (the G pedal breathes) ────
const sineBuf = new Float32Array(NT);
for (let bar = 4; bar < 100; bar++) {
  const s = SEC(bar);
  const nv = s.startsWith("verse") ? 3 : s.startsWith("pre") ? 4
    : s === "break" ? 5 : s === "finale" ? 4 : s.startsWith("chorus") ? 4 : 0;
  if (!nv) continue;
  const voices = CH[chord(bar)].slice(1, nv + 1);        // skip the low root — mud
  const swell = s === "break" ? 1.4 : 1;
  const a = Math.floor(T(bar) * SR), n = Math.floor((BAR + 0.6) * SR);
  for (let vi = 0; vi < voices.length; vi++) {
    const f = 440 * 2 ** ((voices[vi] - 69) / 12);
    const g = 0.018 * (1 - vi * 0.12) * swell;
    const lfo = 0.11 + 0.02 * vi;
    for (let j = 0; j < n && a + j < NT; j++) {
      const t = j / SR;
      const env = Math.min(t / 0.4, 1) * Math.min((n / SR - t) / 0.55, 1);
      sineBuf[a + j] += Math.sin(2 * Math.PI * f * t) * g * env
        * (0.8 + 0.2 * Math.sin(2 * Math.PI * lfo * (T(bar) + t)));
    }
  }
}

// ── DOORS: click rushes + reverse bells at the act doors ──────────────
const clickBuf = new Float32Array(NT), bellBuf = new Float32Array(NT);
const rush = (door, gain, n = 9, span = 1.25) => {
  for (let i = 0; i < n; i++) {
    const frac = (i / (n - 1)) ** 1.6;
    put(clickBuf, S["click-door"], T(door) - span * (1 - frac) - 0.02, gain * (0.5 + 0.5 * frac));
  }
};
rush(4, 0.07, 7); rush(24, 0.10); rush(40, 0.07, 7); rush(56, 0.10);
rush(72, 0.08); rush(76, 0.11, 12, 1.6); rush(100, 0.06, 6, 1.5);
put(clickBuf, S["click-door"], 0, 0.09);
for (const [door, g] of [[24, 0.45], [56, 0.45], [76, 0.5]])
  put(bellBuf, S["reverse-bell"], T(door) - S["reverse-bell"].length / SR, g);

// ── THE VOICE: sacred phrase at the chorus doors if it exists ─────────
const voxStem = new Float32Array(NT);
const VOXF = process.env.IMAB_VOX ? resolve(process.env.IMAB_VOX) : `${OUT}/imab-sacredvox.wav`;
let haveVox = existsSync(VOXF);
if (haveVox) {
  const vox = readF32(VOXF);
  for (const door of CHORUS_DOORS) put(voxStem, vox, T(door), 1);
  const SETSJ = resolve(HERE, "../vocal-sets.json");
  if (existsSync(SETSJ))
    for (const s of (JSON.parse(readFileSync(SETSJ, "utf8")).sets ?? [])) {
      if (!s.at) continue;
      const m = /^([A-Z])(\d+(?:\.\d+)?)$/.exec(s.at.trim().toUpperCase());
      const at = T(24 + (m[1].charCodeAt(0) - 65)) + (parseFloat(m[2]) - 1) * BEAT;
      put(voxStem, readF32(`${OUT}/imab-set-${s.take}.wav`), at, s.gain ?? 1);
    }
} else console.log("· no vocal found — baking the instrumental bed");

// ── THE STAGE: kick/sub/bass/vox mono; everything decorative WIDE.
// Chart stereo correlation runs 0.68–0.87 — stage for it ──────────────
const L = new Float32Array(NT), R = new Float32Array(NT);
const addPlaced = (src, deg = 0, depth = 0, gain = 1, perSample = null) => {
  let m = src;
  if (depth > 0) {
    const a = 1 - Math.exp(-2 * Math.PI * (9000 - 6500 * depth) / SR);
    const y = new Float32Array(m.length); let acc = 0;
    for (let i = 0; i < m.length; i++) { acc += a * (m[i] - acc); y[i] = acc; }
    m = y; gain *= 1 - 0.25 * depth;
  }
  // ILD-only panning: delayed copies anti-correlate broadband highs,
  // which is exactly the phasey width the chart never has. Width comes
  // from placing different sources on different sides.
  const itd = 0;
  const ild = 10 ** (-Math.abs(deg) / 40 * 12 / 20);
  for (let i = 0; i < m.length && i < NT; i++) {
    const e = gain * (perSample ? perSample(i) : 1);
    const near = m[i] * e, far = (i >= itd ? m[i - itd] : 0) * e * ild;
    if (deg > 0) { R[i] += near; L[i] += far; }
    else if (deg < 0) { L[i] += near; R[i] += far; }
    else { L[i] += near; R[i] += near; }
  }
};
const pump = (i) => duck[i];
const pumpHalf = (i) => 1 - (1 - duck[i]) * 0.5;

addPlaced(kickBuf, 0, 0, 1);
addPlaced(subBuf, 0, 0, 1, pumpHalf);
addPlaced(bassBuf, 0, 0.15, 1, pump);
addPlaced(backBuf, -4, 0.08, 1);
addPlaced(stabBuf, -20, 0.18, 1, pump);
addPlaced(stabEchoBuf, 24, 0.3, 1, pump);
addPlaced(hatBuf, 18, 0.1, 1);
addPlaced(openBuf, -16, 0.1, 1);
addPlaced(shakL, -30, 0.15, 1);
addPlaced(shakR, 30, 0.15, 1);
addPlaced(washBuf, 26, 0, 1);
addPlaced(washBuf2, -26, 0, 1);
addPlaced(sineBuf, 0, 0.35, 1, pump);
addPlaced(xyloBuf, -16, 0.2, 1, pump);
addPlaced(clickBuf, 10, 0, 1);
addPlaced(bellBuf, 12, 0.3, 1);

if (haveVox) {
  const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
  const inst = new Float32Array(NT);
  for (let i = 0; i < NT; i++) inst[i] = (L[i] + R[i]) / 2;
  const vg = Math.min(6, (rms(inst) * 2.0) / Math.max(1e-9, rms(voxStem)));
  addPlaced(voxStem, 0, 0, vg);
  console.log(`voice: sacred ×${vg.toFixed(2)} at doors ${CHORUS_DOORS.join("/")}`);
}

// ── premaster: body-peak normalize (floor law), fade the tail ─────────
const fadeN = Math.floor(2 * SR);
for (let i = 0; i < fadeN; i++) { const g = i / fadeN; L[NT - 1 - i] *= g; R[NT - 1 - i] *= g; }
const mags = [];
for (let i = 0; i < NT; i += 4) { const a = Math.abs(L[i]); if (a > 1e-4) mags.push(a); }
mags.sort((a, b) => a - b);
const p999 = mags[Math.floor(mags.length * 0.999)] || 1;
const scale = 0.85 / p999;
for (let i = 0; i < NT; i++) { L[i] *= scale; R[i] *= scale; }
const st = new Float32Array(NT * 2);
for (let i = 0; i < NT; i++) { st[2 * i] = L[i]; st[2 * i + 1] = R[i]; }
writeFileSync(`${WORK}/.hitbaker.f32`, Buffer.from(st.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "2",
  "-i", `${WORK}/.hitbaker.f32`, "-c:a", "pcm_f32le", `${WORK}/hitbaker-premaster.wav`]);

// ── master: the cut-wax material chain, retuned for the chart print:
// wider highs, more excitement, 16.5k ceiling, TARGET −8.5 ────────────
const TARGET = -8.5;
const MATERIAL =
  "acrossover=split=120:order=4th[low][high];" +
  "[low]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lowm];" +
  "[high]stereotools=slev=1.15,aexciter=amount=2.2:drive=7:blend=0:freq=5500[hip];" +
  "[lowm][hip]amix=inputs=2:normalize=0," +
  "volume=1.1dB,asoftclip=type=tanh,volume=-0.8dB," +
  "acompressor=threshold=0.30:ratio=1.9:attack=8:release=180:makeup=1.4:knee=8," +
  "equalizer=f=400:t=q:w=1.4:g=-4," +
  "equalizer=f=150:t=q:w=1.0:g=-1.5," +
  "equalizer=f=2800:t=q:w=1.6:g=1.5," +
  "treble=g=5:f=7500," +
  "highpass=f=28,lowpass=f=16500";
sh("ffmpeg", ["-y", "-v", "error", "-i", `${WORK}/hitbaker-premaster.wav`,
  "-filter_complex", `[0:a]${MATERIAL}[out]`, "-map", "[out]",
  "-ar", String(SR), "-c:a", "pcm_f32le", `${WORK}/hitbaker-wax.wav`]);

const measure = (file) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", file,
    "-af", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
  const text = r.stderr || "";
  const I = [...text.matchAll(/I:\s+(-?[\d.]+) LUFS/g)].pop()?.[1];
  const P = [...text.matchAll(/Peak:\s+(-?[\d.]+) dBFS/g)].pop()?.[1];
  const LRA = [...text.matchAll(/LRA:\s+([\d.]+) LU\b/g)].pop()?.[1];
  return { I: Number(I), P: Number(P), LRA: Number(LRA) };
};
let gain = TARGET - measure(`${WORK}/hitbaker-wax.wav`).I;
let final = null;
for (let round = 0; round < 5; round++) {
  sh("ffmpeg", ["-y", "-v", "error", "-i", `${WORK}/hitbaker-wax.wav`,
    "-af", `volume=${gain.toFixed(2)}dB,alimiter=limit=0.85:attack=5:release=90:level=disabled`,
    "-ar", String(SR), "-c:a", "pcm_s24le", `${WORK}/hitbaker-master.wav`]);
  final = measure(`${WORK}/hitbaker-master.wav`);
  console.log(`master round ${round}: static ${gain.toFixed(2)} dB → I ${final.I} LUFS · LRA ${final.LRA} · TP ${final.P} dBFS`);
  if (Math.abs(final.I - TARGET) <= 0.35) break;
  gain += Math.max(-3, Math.min(3, TARGET - final.I)) * 0.9;
}

for (const [ext, args] of [["wav", ["-c:a", "pcm_s16le"]],
  ["mp3", ["-c:a", "libmp3lame", "-q:a", "2"]]])
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", `${WORK}/hitbaker-master.wav`,
    "-metadata", "title=imab-hitbaker-demo1", "-metadata", "artist=Whistlegraph Dot Org",
    ...args, `${OUT}/imab-hitbaker-demo1.${ext}`]);
console.log(`✓ ${OUT}/imab-hitbaker-demo1.wav + .mp3 — I ${final.I} LUFS · LRA ${final.LRA} LU · TP ${final.P} dBFS`);
console.log(`  verify: pop/.venv/bin/python pop/study/study.py pop/imab/out/imab-hitbaker-demo1.wav --out pop/study/out/imab-hitbaker --title "imab hitbaker" --artist "Whistlegraph Dot Org"`);
