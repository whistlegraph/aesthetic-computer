#!/usr/bin/env node
// imabclub.mjs — the imab club track, draft 1. Everything the lane
// learned in one arrangement: the note-locked half-time vocal (holyvox,
// verified against @jeffrey's melody), a sine choir that assembles act
// by act, kick5 four-on-floor with a kickless break, offbeat sub in C,
// eager hats/shaker, click-rush doors, a reversed-vocal riser into the
// drop, and the cut-wax master with the inhale at the drop door.
//
//   96 bars at 124 ≈ 3:06 —
//   0–16 intro · 16 pass1 · 32 lift · 40 pass2 · 56 BREAK · 64 DROP
//   pass3 · 80 outro peel
//
//   node pop/imab/bin/imabclub.mjs      (needs holyvox.mjs run first)
//   → out/imabclub-draft1.mp3 (−11.5 juke print)

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
const PY = `${REPO}/pop/.venv/bin/python`;
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });
const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const BARS = 96, NT = Math.ceil((BARS * BAR + 4) * SR);
const T = (b) => b * BAR;
const mixL = new Float32Array(NT);

const VOX = `${OUT}/imab-holyvox.wav`;
if (!existsSync(VOX)) { console.error("✗ run holyvox.mjs first"); process.exit(1); }
const readF32 = (wav) => {
  const raw = `${WORK}/.r.f32`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav, "-f", "f32le", "-ac", "1", "-ar", String(SR), raw]);
  const b = readFileSync(raw);
  return new Float32Array(b.buffer, b.byteOffset, Math.floor(b.length / 4));
};

// ── acts ──────────────────────────────────────────────────────────────
const PASS = [16, 40, 64];                    // vocal doors
const KICKLESS = [56, 64];
const DOORS = [16, 32, 40, 56, 64, 80];

// ── sine choir: voices assemble act by act, breathe, pump vs the kick ─
const CH = { C: [36, 48, 55, 64, 72, 79], F: [41, 53, 57, 60, 69, 77], G: [43, 55, 59, 62, 67, 74] };
const MAP16 = ["C", "C", "C", "C", "C", "F", "F", "G", "C", "C", "C", "C", "C", "C", "C", "C"];
const voicesAt = (b) => (b < 8 ? 2 : b < 16 ? 3 : b < 40 ? 4 : b < 56 ? 5 : b < 64 ? 6 : b < 80 ? 5 : b < 88 ? 4 : 2);
for (let bar = 0; bar < BARS; bar++) {
  const chord = CH[MAP16[bar % 16]].slice(0, voicesAt(bar));
  const a = Math.floor(T(bar) * SR), n = Math.floor((BAR + 0.6) * SR);
  for (let vi = 0; vi < chord.length; vi++) {
    const f = 440 * 2 ** ((chord[vi] - 69) / 12);
    const g = 0.042 * (1 - vi * 0.1);
    const lfo = 0.11 + 0.02 * vi;
    for (let j = 0; j < n && a + j < NT; j++) {
      const t = j / SR;
      const env = Math.min(t / 0.4, 1) * Math.min((n / SR - t) / 0.55, 1);
      mixL[a + j] += Math.sin(2 * Math.PI * f * t) * g * env * (0.8 + 0.2 * Math.sin(2 * Math.PI * lfo * (T(bar) + t)));
    }
  }
}

// ── kick5 ─────────────────────────────────────────────────────────────
const kn = Math.floor(0.42 * SR), K = new Float32Array(kn);
{
  let ph = 0, acc = 0;
  const aa = 1 - Math.exp(-2 * Math.PI * 2200 / SR);
  for (let j = 0; j < kn; j++) {
    const t = j / SR;
    ph += 2 * Math.PI * (40 + 80 * Math.exp(-t / 0.034)) / SR;
    const raw = Math.tanh(2.0 * (Math.sin(ph) * Math.exp(-t / 0.17) + Math.sin(2 * ph) * Math.exp(-t / 0.05) * 0.22 + Math.sin(2 * Math.PI * 150 * t) * Math.exp(-t / 0.024) * 0.30));
    acc += aa * (raw - acc); K[j] = acc;
  }
}
const duck = new Float32Array(NT).fill(1);
for (let b = 4 * 4; b < 95 * 4; b++) {
  const bar = b / 4;
  if (bar >= KICKLESS[0] && bar < KICKLESS[1]) continue;
  const at = Math.floor(b * BEAT * SR);
  const g = (bar < 16 ? 0.5 : bar < 64 ? 0.6 : 0.66) * (b % 4 === 0 ? 1.05 : 1.0);
  for (let j = 0; j < kn && at + j < NT; j++) mixL[at + j] += K[j] * g;
  for (let j = 0; j < Math.floor(0.5 * SR) && at + j < NT; j++)
    duck[at + j] = Math.min(duck[at + j], 1 - 0.35 * Math.exp(-j / (0.09 * SR)));
}

// ── offbeat sub in C (root follows the chord map) ─────────────────────
const ROOT = { C: 36, F: 41, G: 43 };
for (let b = 8 * 4; b < 92 * 4; b++) {
  const bar = Math.floor(b / 4);
  if (bar >= KICKLESS[0] && bar < KICKLESS[1]) continue;
  const f = 440 * 2 ** ((ROOT[MAP16[bar % 16]] - 12 - 69) / 12);   // C1 region
  const at = Math.floor((b * BEAT + BEAT / 2) * SR), n = Math.floor(0.34 * BEAT * SR);
  for (let j = 0; j < n && at + j < NT; j++) {
    const t = j / SR;
    const env = Math.min(t / 0.005, 1) * Math.exp(-t / (0.34 * BEAT * 0.55));
    mixL[at + j] += Math.tanh(2.2 * Math.sin(2 * Math.PI * f * t)) * env * 0.3;
  }
}

// ── hats + shaker (noise, eager) ──────────────────────────────────────
let seed = 360;
const rnd = () => { seed = (seed * 1103515245 + 12345) & 0x7fffffff; return seed / 0x7fffffff - 0.5; };
const hat = (dur) => {
  const n = Math.floor(dur * SR), out = new Float32Array(n);
  let prev = 0;
  for (let j = 0; j < n; j++) {
    const w = rnd();
    out[j] = (w - prev) * Math.exp(-j / (n * 0.3));  // crude highpass + decay
    prev = w;
  }
  return out;
};
const HC = hat(0.05), HO = hat(0.22), SHK = hat(0.07);
for (let b = 0; b < 92 * 8; b++) {                    // eighth grid
  const t = b * BEAT / 2, bar = t / BAR;
  if (bar < 32) continue;                             // hats arrive at the lift
  const inBreak = bar >= KICKLESS[0] && bar < KICKLESS[1];
  const at = Math.floor((t + (Math.abs(rnd()) * 0.008 - 0.005)) * SR);
  const src = (b % 2 === 1 && bar >= 64 && !inBreak) ? HO : HC;
  const g = (b % 2 === 1 ? 0.16 : 0.09) * (inBreak ? 0.3 : 1);
  for (let j = 0; j < src.length && at + j < NT; j++) mixL[at + j] += src[j] * g;
}
for (let b = 0; b < 92 * 16; b++) {                   // sixteenth shaker
  const t = b * BEAT / 4, bar = t / BAR;
  if (bar < 16) continue;
  const wave = 0.5 + 0.5 * Math.sin(b * Math.PI / 8 + 0.7);
  const at = Math.floor((t - 0.004 + Math.abs(rnd()) * 0.006) * SR);
  const g = (0.05 + 0.05 * wave) * (bar >= KICKLESS[0] && bar < KICKLESS[1] ? 0.6 : 1);
  for (let j = 0; j < SHK.length && at + j < NT; j++) mixL[at + j] += SHK[j] * g * 0.6;
}

// ── click-rush doors ──────────────────────────────────────────────────
const tick = (t, freq, gain) => {
  const n = Math.floor(0.035 * SR), a = Math.floor(t * SR);
  for (let i = 0; i < n && a + i < NT; i++) {
    const tt = i / SR;
    mixL[a + i] += Math.tanh(1.4 * Math.sin(2 * Math.PI * freq * tt) * Math.exp(-tt / 0.006)) * gain;
  }
};
for (const door of DOORS) {
  const N = door === 64 ? 12 : 9, span = door === 64 ? 1.6 : 1.25;
  for (let i = 0; i < N; i++) {
    const frac = (i / (N - 1)) ** 1.6;
    tick(T(door) - span * (1 - frac) - 0.02, 2600, (door === 56 ? 0.07 : 0.1) * (0.5 + 0.5 * frac));
  }
}

// ── the vocal: three passes + reversed tail as the drop riser ─────────
sh(PY, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", VOX, `${WORK}/club-halo.wav`, "-14", "1.6"]);
const vox = readF32(`${WORK}/club-halo.wav`);
const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
const vg = Math.min(8, (rms(mixL) * 2.1) / Math.max(1e-9, rms(vox)));
for (const door of PASS) {
  const off = Math.floor((T(door) + 0.05) * SR);
  for (let j = 0; j < vox.length; j++) { const d = off + j; if (d < NT) mixL[d] += vox[j] * vg; }
}
{ // riser: the vocal's last 2 s reversed, swelling into the drop
  const nR = Math.floor(2 * SR), tail = vox.slice(-nR).reverse();
  const at = Math.floor((T(64) - 2) * SR);
  for (let j = 0; j < tail.length && at + j < NT; j++) mixL[at + j] += tail[j] * vg * (j / tail.length) * 0.9;
}
for (let i = 0; i < NT; i++) mixL[i] *= duck[i] * 0.55 + 0.45;   // gentle global pump feel

const fadeN = Math.floor(3 * SR);
for (let i = 0; i < fadeN; i++) mixL[NT - 1 - i] *= i / fadeN;
let pk = 0; for (let i = 0; i < NT; i++) pk = Math.max(pk, Math.abs(mixL[i]));
if (pk > 0.85) for (let i = 0; i < NT; i++) mixL[i] *= 0.85 / pk;
const stb = new Float32Array(NT * 2);
for (let i = 0; i < NT; i++) { stb[2 * i] = mixL[i]; stb[2 * i + 1] = mixL[i]; }
writeFileSync(`${WORK}/.club.f32`, Buffer.from(stb.buffer));
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-f", "f32le", "-ar", String(SR), "-ac", "2",
  "-i", `${WORK}/.club.f32`, "-c:a", "pcm_s24le", `${WORK}/club-premaster.wav`]);

console.log("→ cut-wax master (inhale at the drop door)");
const inhale = `between(t,${(T(64) - 1.75).toFixed(2)},${T(64).toFixed(2)})`;
const r = spawnSync("bash", [`${REPO}/pop/loner/c/cut-wax.sh`, `${WORK}/club-premaster.wav`, `${OUT}/imabclub-draft1.mp3`],
  { env: { ...process.env, INHALE: inhale, TARGET: "-11.5" }, stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ master failed"); process.exit(1); }
sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", `${OUT}/imabclub-draft1.mp3`, "-c", "copy",
  "-metadata", "title=imabclub draft1", "-metadata", "artist=Whistlegraph Dot Org", `${WORK}/t.mp3`]);
sh("mv", [`${WORK}/t.mp3`, `${OUT}/imabclub-draft1.mp3`]);
console.log(`✓ ${OUT}/imabclub-draft1.mp3 (vox gain ${vg.toFixed(2)})`);
