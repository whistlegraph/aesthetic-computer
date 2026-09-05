#!/usr/bin/env node
// floor.mjs — the imab dance floor, draft 1, under the SACRED vocal.
// One 124 grid. The floor assembles act by act from samples/kit/ (kick
// alone → closed hats → open → full kit), the sacredvox hook phrase
// lands at the act doors untouched (no pitch work, no halo — demo6
// law), holyvox floats through the kickless break as half-time tissue,
// a vocal-keyed wub swells only in the gaps the voice leaves, the bed
// pumps against the kick, and the whole thing peels in reverse to kick
// alone. Palindrome: bars 0–4 and 68–72 are the same room.
//
//   72 bars at 124 ≈ 2:19 —
//   0 kick alone · 4 +hats · 8 +open/shaker · 12 +sub/sines
//   16 PASS1 · 24 lift · 32 BREAK (kickless, holyvox from bar 30)
//   40 DROP PASS2 · 48 peak (xylo chant echo) · 56 PASS3 farewell
//   64 peel · 68 kick alone · 72 one last hit
//
//   node pop/imab/bin/floor.mjs
//   → out/imab-floor-demo1.wav + .mp3 (−11.5 LUFS juke print,
//     cut-wax material posture: MEASURE → one static dB → limit 0.82)

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
const PY = `${REPO}/pop/.venv/bin/python`;
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });

const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const BARS = 72, TAIL = 2.5;
const NT = Math.ceil((BARS * BAR + TAIL) * SR);
const T = (b) => b * BAR;

// ── ingredients ───────────────────────────────────────────────────────
const VOX = process.env.IMAB_VOX          // the phrase (dry, 124); env picks the chain
  ? resolve(process.env.IMAB_VOX)          //   e.g. IMAB_VOX=out/imab-aesthetivox.wav
  : `${OUT}/imab-sacredvox.wav`;
const HOLY = `${OUT}/imab-holyvox.wav`;    // half-time, note-locked
for (const [f, fix] of [[VOX, "sacredvox.mjs"], [HOLY, "holyvox.mjs"]])
  if (!existsSync(f)) { console.error(`✗ missing ${f} — run ${fix}`); process.exit(1); }

const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav,
    "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  const n = Math.floor(b.length / 4);
  return new Float32Array(b.buffer.slice(b.byteOffset, b.byteOffset + n * 4));
};
const S = {};   // the kit
for (const name of ["kick", "hat-closed", "hat-open", "shaker", "snare",
  "click-door", "reverse-kick", "reverse-bell"]) S[name] = readF32(`${KIT}/${name}.wav`);
const XY = {};  // xylo one-shots for the chant echo (C5 G5 C6 — all in kit)
for (const n of ["C5", "G5", "C6"]) XY[n] = readF32(`${KIT}/xylo-${n}.wav`);

let seed = 124;
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

// ── harmony: the sacred bed's own 8-bar cycle in C (accomp ×3) ────────
const MAP8 = ["C", "C", "F", "F", "C", "C", "C", "C"];
const CH = { C: [36, 48, 55, 64, 72, 79], F: [41, 53, 57, 60, 69, 77] };  // choir voicings
const BASS = { C: 48, F: 53 };      // marimba bass roots (C3 / F3)
const VIBE = { C: [60, 64, 67], F: [60, 65, 69] };
const SUBM = { C: 24, F: 29 };      // C1 / F1 offbeat sub
const chord = (bar) => MAP8[bar % 8];

// ── BREATH: the last half-bar of each 8-bar phrase floors the
// decoratives (kick, bass, sub and every voice are never gated) ───────
const RESTS = [8, 16, 24, 32, 48, 56, 64].map((d, k) =>
  [T(d) - BAR / 2, T(d), k % 2 === 0 ? 0.34 : 0.52]);
const gateAt = (t) => {
  for (const [a, b, depth] of RESTS) if (t >= a && t < b) return depth;
  return 1;
};
const gateBuf = new Float32Array(NT).fill(1);
{
  const ramp = Math.floor(0.05 * SR);
  for (const [a, b, depth] of RESTS) {
    const lo = Math.floor(a * SR), hi = Math.min(NT, Math.floor(b * SR));
    for (let i = lo; i < hi; i++) {
      let g = depth;
      if (i - lo < ramp) g = 1 + (depth - 1) * (i - lo) / ramp;
      else if (hi - i < ramp) g = 1 + (depth - 1) * (hi - i) / ramp;
      gateBuf[i] = Math.min(gateBuf[i], g);
    }
  }
}

// ── KICK: four on the floor, kickless 32–40, turns before doors.
// One level the whole record — the club stacks AROUND the kick ────────
const kickBuf = new Float32Array(NT);
const KICKG = 0.9;
const TURNS = new Set([15, 23, 47, 55]);
for (let b = 0; b < BARS * 4; b++) {
  const bar = Math.floor(b / 4), beat = b % 4;
  if (bar >= 32 && bar < 40) continue;                  // the kickless break
  put(kickBuf, S.kick, b * BEAT, KICKG * (beat === 0 ? 1.05 : 1));
  if (TURNS.has(bar) && beat === 3)
    put(kickBuf, S.kick, b * BEAT + BEAT * 0.75, 0.5);  // the lean into the door
}
put(kickBuf, S.kick, T(BARS), 1.0);                     // one last hit, ringing

// the pump: every kick presses the bed down 45% and lets it back up
const duck = new Float32Array(NT).fill(1);
{
  const win = Math.floor(0.5 * SR), tau = 0.09 * SR;
  for (let b = 0; b < BARS * 4; b++) {
    const bar = Math.floor(b / 4);
    if (bar >= 32 && bar < 40) continue;
    const at = Math.floor(b * BEAT * SR);
    for (let j = 0; j < win && at + j < NT; j++)
      duck[at + j] = Math.min(duck[at + j], 1 - 0.45 * Math.exp(-j / tau));
  }
}

// ── SUB: offbeat tanh sine on the roots, C1 region (imabclub voice) ───
const subBuf = new Float32Array(NT);
for (let b = 8 * 4; b < 60 * 4; b++) {
  const bar = Math.floor(b / 4);
  if (bar >= 32 && bar < 40) continue;
  const f = 440 * 2 ** ((SUBM[chord(bar)] - 69) / 12);
  const g = bar >= 56 ? 0.24 : 0.30;
  const at = Math.floor((b * BEAT + BEAT / 2) * SR), n = Math.floor(0.34 * BEAT * SR);
  for (let j = 0; j < n && at + j < NT; j++) {
    const t = j / SR;
    const env = Math.min(t / 0.005, 1) * Math.exp(-t / (0.34 * BEAT * 0.55));
    subBuf[at + j] += Math.tanh(2.2 * Math.sin(2 * Math.PI * f * t)) * env * g;
  }
}

// ── BASS + VIBES: the marimba bed, in HIS key (C) ─────────────────────
const bassBuf = new Float32Array(NT), vibeBuf = new Float32Array(NT);
for (let bar = 12; bar < 60; bar++) {
  if (bar >= 32 && bar < 40) continue;
  mixEventMarimba({ startSec: T(bar), midi: BASS[chord(bar)], durSec: 2 * BEAT,
    gain: bar < 16 ? 0.35 : 0.6, preset: "bass", decayMul: 0.8 }, bassBuf, { sampleRate: SR });
}
for (let bar = 16; bar < 56; bar++) {
  if (bar >= 24 && bar < 40) continue;                  // hook passes + peak only
  for (const midi of VIBE[chord(bar)])
    mixEventMarimba({ startSec: T(bar), midi, durSec: 3.5 * BEAT,
      gain: 0.16, preset: "vibraphone", decayMul: 1.6 }, vibeBuf, { sampleRate: SR });
}

// ── SINES: the choir assembles act by act, swells in the break ────────
const sineBuf = new Float32Array(NT);
const voicesAt = (bar) => bar < 12 ? 0 : bar < 24 ? 3 : bar < 32 ? 4
  : bar < 40 ? 5 : bar < 48 ? 6 : bar < 56 ? 5 : bar < 60 ? 4 : bar < 64 ? 3 : 0;
for (let bar = 12; bar < 64; bar++) {
  const voices = CH[chord(bar)].slice(0, voicesAt(bar));
  const breakSwell = bar >= 32 && bar < 40 ? 1.25 : 1;
  const fade = bar >= 60 ? 1 - (bar - 60) / 4.5 : 1;
  const a = Math.floor(T(bar) * SR), n = Math.floor((BAR + 0.6) * SR);
  for (let vi = 0; vi < voices.length; vi++) {
    const f = 440 * 2 ** ((voices[vi] - 69) / 12);
    const g = 0.045 * (1 - vi * 0.12) * breakSwell * fade;
    const lfo = 0.11 + 0.02 * vi;
    for (let j = 0; j < n && a + j < NT; j++) {
      const t = j / SR;
      const env = Math.min(t / 0.4, 1) * Math.min((n / SR - t) / 0.55, 1);
      sineBuf[a + j] += Math.sin(2 * Math.PI * f * t) * g * env
        * (0.8 + 0.2 * Math.sin(2 * Math.PI * lfo * (T(bar) + t)));
    }
  }
}

// ── HATS: closed eighths learn the room, opens exhale then chatter ────
const hatBuf = new Float32Array(NT), openBuf = new Float32Array(NT);
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (bar < 4 || (bar >= 32 && bar < 40) || bar >= 68) continue;
  if (bar < 8 && !off) continue;                        // offbeats first
  if (bar >= 64 && !off) continue;                      // and offbeats last
  const g = (off ? (bar < 8 ? 0.15 : bar < 64 ? 0.16 : 0.12)
                 : 0.08)
    * (bar >= 64 ? 1 - (bar - 64) / 5 : 1) * gateAt(t);
  put(hatBuf, S["hat-closed"], t + eager(), g);
}
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (!off || bar < 8 || (bar >= 32 && bar < 40) || bar >= 60) continue;
  const beatIdx = Math.floor((e % 8) / 2);              // which beat's offbeat
  let ok = false, g = 0.13;
  if (bar < 16) { ok = beatIdx === 3 && bar % 2 === 1; }          // the exhale
  else if (bar < 32) { ok = beatIdx === 1 || beatIdx === 3; }
  else if (bar < 56) { ok = true; g = 0.14; }                     // every offbeat
  else { ok = beatIdx === 3 && bar % 2 === 1; g = 0.12; }         // exhale again
  if (ok) put(openBuf, S["hat-open"], t + eager(), g * gateAt(t));
}

// ── SHAKER: the rhythm's seed — arrives early, ticks through the
// break, gathers toward the doors, leaves late ────────────────────────
const shakBuf = new Float32Array(NT);
for (let s = 8 * 16; s < 66 * 16; s++) {
  const t = s * BEAT / 4, bar = t / BAR;
  const wave = 0.5 + 0.5 * Math.sin(s * Math.PI / 8 + 0.7);
  let g = (0.05 + 0.05 * wave) * 0.6;
  if (bar < 16) g *= 1.3;                                     // early company
  if (bar >= 28 && bar < 32) g *= 1 + 1.2 * (bar - 28) / 4;   // toward the break
  if (bar >= 36 && bar < 40) g *= 1 + 1.6 * (bar - 36) / 4;   // toward the drop
  if (bar >= 32 && bar < 36) g *= 0.85;                       // alone, ticking
  if (bar >= 60) g *= Math.max(0, 1 - (bar - 60) / 6);
  put(shakBuf, S.shaker, t - 0.004 + Math.abs(rnd()) * 0.006, g * gateAt(t));
}

// ── SNARE: sparse answers in the lift and the peak, the roll into
// the drop, and the reverse kick at the door ──────────────────────────
const snareBuf = new Float32Array(NT), rollBuf = new Float32Array(NT);
const SN_HI = repitch(S.snare, 1.19), SN_UP = repitch(S.snare, 1.5);
for (let bar = 24; bar < 56; bar++) {
  if (bar >= 32 && bar < 48) continue;
  if (bar % 2 === 1) put(snareBuf, SN_HI, T(bar) + 1.5 * BEAT + eager(), 0.12 * gateAt(T(bar) + 1.5 * BEAT));
  if (bar % 8 === 6) put(snareBuf, SN_UP, T(bar) + 3.75 * BEAT + eager(), 0.09);
}
{ // the roll: beats → halves → quarters across bars 36–40
  const steps = [];
  for (let b = 36; b < 38; b += 0.25) steps.push(b);
  for (let b = 38; b < 39; b += 0.125) steps.push(b);
  for (let b = 39; b < 40; b += 0.0625) steps.push(b);
  for (const b of steps) {
    const frac = (b - 36) / 4;
    put(rollBuf, S.snare, T(b) + eager(), 0.05 + 0.17 * frac ** 1.5);
  }
  put(rollBuf, S["reverse-kick"], T(40) - S["reverse-kick"].length / SR, 0.42);
}

// ── DOORS: click rushes ease in (a tick, never a roll) + bells ────────
const clickBuf = new Float32Array(NT), bellBuf = new Float32Array(NT);
const rush = (door, gain, n = 9, span = 1.25) => {
  for (let i = 0; i < n; i++) {
    const frac = (i / (n - 1)) ** 1.6;
    put(clickBuf, S["click-door"], T(door) - span * (1 - frac) - 0.02, gain * (0.5 + 0.5 * frac));
  }
};
rush(8, 0.07, 7); rush(16, 0.10); rush(24, 0.07, 7); rush(32, 0.08);
rush(40, 0.11, 12, 1.6); rush(48, 0.08, 7); rush(56, 0.09);
rush(64, 0.065, 7, 1.4); rush(72, 0.05, 6, 1.8);
put(clickBuf, S["click-door"], 0, 0.09);                // the first tick of the record
for (const [door, g] of [[16, 0.45], [40, 0.5], [56, 0.35]])
  put(bellBuf, S["reverse-bell"], T(door) - S["reverse-bell"].length / SR, g);

// ── XYLO: the chant echoed on the kit's own bars (GT lines 1–2,
// up an octave in C: C5 G5 C5 C5 C5 · C5 C6 C5 C5 C5) ─────────────────
const xyloBuf = new Float32Array(NT);
const FIG = [ // [barOffset, beat, note]
  [0, 1, "C5"], [0, 1.75, "G5"], [0, 2, "C5"], [0, 3, "C5"], [0, 4, "C5"],
  [1, 1.5, "C5"], [1, 2.5, "C6"], [1, 3.5, "C5"], [1, 4, "C5"], [1, 4.5, "C5"],
];
for (let base = 48; base < 56; base += 2)
  for (const [bo, beat, note] of FIG) {
    const t = T(base + bo) + (beat - 1) * BEAT;
    put(xyloBuf, XY[note], t + eager(), 0.30 * (1 + rnd() * 0.15) * gateAt(t));
  }

// ── THE VOICE: sacred phrase at the pass doors, untouched ─────────────
const vox = readF32(VOX);
const voxStem = new Float32Array(NT);
const PASSES = [16, 40, 56];
for (const door of PASSES) put(voxStem, vox, T(door), 1);   // i'm ON the door (@jeffrey: "16:A1")

// placed vocal sets — regulated takes by global address (vocal-sets.json; A = bar 16)
const SETSJ = resolve(HERE, "../vocal-sets.json");
if (existsSync(SETSJ))
  for (const s of (JSON.parse(readFileSync(SETSJ, "utf8")).sets ?? [])) {
    if (!s.at) continue;
    const m = /^([A-Z])(\d+(?:\.\d+)?)$/.exec(s.at.trim().toUpperCase());
    const at = T(16 + (m[1].charCodeAt(0) - 65)) + (parseFloat(m[2]) - 1) * BEAT;
    put(voxStem, readF32(`${OUT}/imab-set-${s.take}.wav`), at, s.gain ?? 1);
    console.log(`set ${s.take.slice(0, 6)}… placed at ${s.at} (${at.toFixed(2)}s)`);
  }

// the follower that keys the wub: swell only in the gaps the voice leaves
const fo = new Float32Array(NT);
{
  const atk = 1 - Math.exp(-1 / (0.015 * SR)), rel = 1 - Math.exp(-1 / (0.23 * SR));
  let f = 0;
  for (let i = 0; i < NT; i++) {
    const e = Math.abs(voxStem[i]);
    f += (e > f ? atk : rel) * (e - f); fo[i] = f;
  }
  const nz = [];
  for (let i = 0; i < NT; i += 16) if (fo[i] > 1e-5) nz.push(fo[i]);
  nz.sort((a, b) => a - b);
  const p98 = nz[Math.floor(nz.length * 0.98)] || 1;
  for (let i = 0; i < NT; i++) fo[i] = Math.min(1, fo[i] / p98);
}

// ── WUB: tanh sub on the roots, wob alternating by bar, its own
// per-beat duck, then sidechained TO HIM (gen-wub law) ────────────────
const wubBuf = new Float32Array(NT);
{
  const PTS = [[0, 0], [T(15.5), 0], [T(16), 0.14], [T(23.5), 0.14], [T(24), 0],
    [T(39.5), 0], [T(40), 0.17], [T(55.5), 0.17], [T(56), 0.10], [T(63), 0.10], [T(64), 0], [NT / SR, 0]];
  const genv = (t) => {
    for (let k = 1; k < PTS.length; k++) if (t <= PTS[k][0]) {
      const [t0, g0] = PTS[k - 1], [t1, g1] = PTS[k];
      return g0 + (g1 - g0) * (t - t0) / Math.max(1e-9, t1 - t0);
    }
    return 0;
  };
  const C2 = 65.406;
  let ph = 0, smoothedSt = 0;
  const stA = 1 - Math.exp(-1 / (0.03 * SR));
  for (let i = 0; i < NT; i++) {
    const t = i / SR, bar = Math.floor(t / BAR);
    const g = genv(t);
    if (g <= 0) { ph = 0; continue; }
    const st = chord(bar) === "F" ? 5 : 0;
    smoothedSt += stA * (st - smoothedSt);
    const f = C2 * 2 ** (smoothedSt / 12);
    ph += 2 * Math.PI * f / SR;
    const rate = bar % 2 ? 6.10 : 4.07;
    const wob = 0.55 + 0.45 * Math.sin(2 * Math.PI * rate * (t % BAR) - Math.PI / 2);
    const tb = t % BEAT;                                 // per-beat self-duck
    const selfDuck = tb < 0.09 ? 1 - 0.7 * Math.exp(-tb / 0.03) : 1;
    wubBuf[i] = Math.tanh(2.6 * Math.sin(ph)) * wob * selfDuck * g * (1 - 0.28 * fo[i]);
  }
}

// ── HOLYVOX: half-time tissue, haloed, rising through the break ───────
sh(PY, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", HOLY, `${WORK}/floor-holy-halo.wav`, "-14", "1.6"]);
const holy = readF32(existsSync(`${WORK}/floor-holy-halo.wav`) ? `${WORK}/floor-holy-halo.wav` : HOLY);
const holyStem = new Float32Array(NT);
{
  const at = Math.floor(T(30) * SR);
  const fadeIn = Math.floor(2 * BAR * SR);
  for (let j = 0; j < holy.length && at + j < NT; j++)
    holyStem[at + j] += holy[j] * (j < fadeIn ? j / fadeIn : 1);
}

// ── THE STAGE: azimuth via ITD+ILD, distance via shelf+level ──────────
const L = new Float32Array(NT), R = new Float32Array(NT);
const addPlaced = (src, deg = 0, depth = 0, gain = 1, perSample = null) => {
  let m = src;
  if (depth > 0) {
    const a = 1 - Math.exp(-2 * Math.PI * (9000 - 6500 * depth) / SR);
    const y = new Float32Array(m.length); let acc = 0;
    for (let i = 0; i < m.length; i++) { acc += a * (m[i] - acc); y[i] = acc; }
    m = y; gain *= 1 - 0.25 * depth;
  }
  const itd = Math.round(Math.abs(deg) / 40 * 0.0006 * SR);
  const ild = 10 ** (-Math.abs(deg) / 40 * 3 / 20);
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
const pumpGate = (i) => duck[i] * gateBuf[i];

addPlaced(kickBuf, 0, 0, 1);
addPlaced(subBuf, 0, 0, 1);
addPlaced(bassBuf, 0, 0.15, 1, pumpHalf);
addPlaced(vibeBuf, -18, 0.25, 1, pumpGate);
addPlaced(sineBuf, 0, 0.35, 1, pumpGate);
addPlaced(hatBuf, 20, 0.1, 1);
addPlaced(openBuf, -14, 0.1, 1);
addPlaced(shakBuf, 24, 0.15, 1);
addPlaced(snareBuf, -26, 0.2, 1);
addPlaced(rollBuf, 0, 0.05, 1);
addPlaced(clickBuf, 8, 0, 1);
addPlaced(bellBuf, 10, 0.3, 1);
addPlaced(xyloBuf, -15, 0.2, 1, pump);
addPlaced(wubBuf, 0, 0, 1, pump);

// the voices last, RMS-matched to the room they land in
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const inst = new Float32Array(NT);
for (let i = 0; i < NT; i++) inst[i] = (L[i] + R[i]) / 2;
const ri = rms(inst);
const vg = Math.min(6, (ri * 2.0) / Math.max(1e-9, rms(vox)));      // demo6 ratio
const hg = Math.min(8, (ri * 1.3) / Math.max(1e-9, rms(holy)));     // tissue sits back
addPlaced(voxStem, 0, 0, vg);
addPlaced(holyStem, 0, 0.3, hg);
console.log(`voices: sacred ×${vg.toFixed(2)} · holy ×${hg.toFixed(2)}`);

// ── premaster: fade the tail. Normalize by the BODY's peak (p99.9),
// not the one hot vocal consonant — the material chain wants to be fed
// at level, and its tanh stage exists to fold the rare overshoot.
// Float WAV throughout so nothing hard-clips before the limiter. ──────
const fadeN = Math.floor(2 * SR);
for (let i = 0; i < fadeN; i++) { const g = i / fadeN; L[NT - 1 - i] *= g; R[NT - 1 - i] *= g; }
let pk = 0, pkAt = 0;
for (let i = 0; i < NT; i++) {
  const a = Math.max(Math.abs(L[i]), Math.abs(R[i]));
  if (a > pk) { pk = a; pkAt = i; }
}
const mags = [];
for (let i = 0; i < NT; i += 4) { const a = Math.abs(L[i]); if (a > 1e-4) mags.push(a); }
mags.sort((a, b) => a - b);
const p999 = mags[Math.floor(mags.length * 0.999)] || pk;
const scale = 0.85 / p999;
for (let i = 0; i < NT; i++) { L[i] *= scale; R[i] *= scale; }
console.log(`premaster peak ${pk.toFixed(3)} at ${(pkAt / SR).toFixed(2)}s · body p99.9 ${p999.toFixed(3)} → 0.85 (outlier rides to ${(pk * scale).toFixed(2)}, tanh folds it)`);
const st = new Float32Array(NT * 2);
for (let i = 0; i < NT; i++) { st[2 * i] = L[i]; st[2 * i + 1] = R[i]; }
writeFileSync(`${WORK}/.floor.f32`, Buffer.from(st.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "2",
  "-i", `${WORK}/.floor.f32`, "-c:a", "pcm_f32le", `${WORK}/floor-premaster.wav`]);

// ── master: cut-wax material posture, all temps in the lane's WORK.
// MEASURE (ebur128) → one static dB → true-peak limit 0.82. Never a
// second loudnorm. The inhale folds the sides at the drop door. ───────
const TARGET = -11.5;
const inhale = `between(t,${(T(40) - 1.75).toFixed(2)},${T(40).toFixed(2)})`;
const MATERIAL =
  "acrossover=split=120:order=4th[low][high];" +
  "[low]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lowm];" +
  "[high]stereotools=slev=1.3,apulsator=hz=0.06:amount=0.14:mode=sine," +
  "aexciter=amount=0.8:drive=6:blend=0:freq=7500[hip];" +
  "[lowm][hip]amix=inputs=2:normalize=0," +
  "vibrato=f=0.4:d=0.0012," +
  "volume=1.1dB,asoftclip=type=tanh,volume=-0.8dB," +
  "acompressor=threshold=0.28:ratio=1.8:attack=10:release=200:makeup=1.2:knee=8," +
  "equalizer=f=90:t=q:w=0.9:g=1.0," +
  "equalizer=f=2800:t=q:w=1.6:g=0.8," +
  "highpass=f=28,lowpass=f=15000," +
  `stereotools=slev=0.18:enable='${inhale}'`;
sh("ffmpeg", ["-y", "-v", "error", "-i", `${WORK}/floor-premaster.wav`,
  "-filter_complex", `[0:a]${MATERIAL}[out]`, "-map", "[out]",
  "-ar", String(SR), "-c:a", "pcm_f32le", `${WORK}/floor-wax.wav`]);

const measure = (file) => {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", file,
    "-af", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
  const text = r.stderr || "";
  const I = [...text.matchAll(/I:\s+(-?[\d.]+) LUFS/g)].pop()?.[1];
  const P = [...text.matchAll(/Peak:\s+(-?[\d.]+) dBFS/g)].pop()?.[1];
  const LRA = [...text.matchAll(/LRA:\s+([\d.]+) LU\b/g)].pop()?.[1];
  return { I: Number(I), P: Number(P), LRA: Number(LRA) };
};
let gain = TARGET - measure(`${WORK}/floor-wax.wav`).I;
let final = null;
for (let round = 0; round < 5; round++) {
  sh("ffmpeg", ["-y", "-v", "error", "-i", `${WORK}/floor-wax.wav`,
    "-af", `volume=${gain.toFixed(2)}dB,alimiter=limit=0.82:attack=5:release=100:level=disabled`,
    "-ar", String(SR), "-c:a", "pcm_s24le", `${WORK}/floor-master.wav`]);
  final = measure(`${WORK}/floor-master.wav`);
  console.log(`master round ${round}: static ${gain.toFixed(2)} dB → I ${final.I} LUFS · TP ${final.P} dBFS`);
  if (Math.abs(final.I - TARGET) <= 0.35) break;
  gain += Math.max(-3, Math.min(3, TARGET - final.I)) * 0.9;   // damped — the limiter eats
}

sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", `${WORK}/floor-master.wav`,
  "-metadata", "title=imab-floor-demo1", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "pcm_s16le", `${OUT}/imab-floor-demo1.wav`]);
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", `${WORK}/floor-master.wav`,
  "-metadata", "title=imab-floor-demo1", "-metadata", "artist=Whistlegraph Dot Org",
  "-c:a", "libmp3lame", "-q:a", "2", `${OUT}/imab-floor-demo1.mp3`]);
console.log(`✓ ${OUT}/imab-floor-demo1.wav + .mp3 — I ${final.I} LUFS · LRA ${final.LRA} LU · TP ${final.P} dBFS`);
