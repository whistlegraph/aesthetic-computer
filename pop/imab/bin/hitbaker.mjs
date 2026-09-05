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

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { mixEventMarimba } from "../../marimba/synths/marimba.mjs";
import { applyWobble } from "../../dance/synths/fx.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const OUT = resolve(HERE, "../out");
const KIT = resolve(HERE, "../samples/kit");
const WORK = `${process.env.HOME}/.cache/ac/imab`;
mkdirSync(WORK, { recursive: true });
const sh = (cmd, args, opts = {}) => spawnSync(cmd, args, { stdio: ["ignore", "ignore", "inherit"], ...opts });

const SR = 48_000, BPM = 124, BEAT = 60 / BPM, BAR = 4 * BEAT;
const BARS = 100, TAIL = 2.5;
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
// the REAL kit (CC0 Freesound one-shots via fetch-real-kit.mjs) wins
// where it exists — real percussion is noise + transient, which is the
// chart's percussive weight; the synth kit keeps the lane's doors/bells
const REAL = resolve(HERE, "../samples/real");
const S = {};
for (const name of ["kick", "hat-closed", "hat-open", "shaker", "snare",
  "clap", "click-door", "reverse-kick", "reverse-bell"]) {
  const real = `${REAL}/${name}.wav`, kit = `${KIT}/${name}.wav`;
  const path = existsSync(real) ? real : existsSync(kit) ? kit : null;
  if (path) S[name] = readF32(path);
}
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

// ── FORM: no intro at all — the record opens ON the voice, bar 0,
// full band under it ─────────────────────────────────────────────────
const SEC = (bar) =>
  bar < 8 ? "chorus0" : bar < 20 ? "verse1"
  : bar < 24 ? "pre1" : bar < 40 ? "chorus1" : bar < 52 ? "verse2"
  : bar < 56 ? "pre2" : bar < 72 ? "chorus2" : bar < 76 ? "break"
  : bar < 96 ? "finale" : "peel";
const inBreak = (bar) => bar >= 72 && bar < 76;
const CHORUS_DOORS = [0, 24, 56, 76];

// ── HARMONY: verse = README hook cycle (Am Am G G C C F G),
// chorus = the sacred bed's own map (C C F F C C C C), pre = F F G G,
// break = G pedal (the dominant hands the drop back to C) ─────────────
const VERSE8 = ["Am", "Am", "G", "G", "C", "C", "F", "G"];
const CHOR8 = ["C", "C", "F", "F", "C", "C", "C", "C"];
const chord = (bar) => {
  const s = SEC(bar);
  if (s === "verse1") return VERSE8[(bar - 8) % 8];
  if (s === "verse2") return VERSE8[(bar - 40) % 8];
  if (s === "pre1" || s === "pre2") return ["F", "F", "G", "G"][bar % 4];
  if (s === "break") return "G";
  if (s === "peel") return "C";
  const base = s === "chorus0" ? 0 : s === "chorus1" ? 24
    : s === "chorus2" ? 56 : 76;
  return CHOR8[(bar - base) % 8];
};
const BASSM = { Am: 45, G: 43, C: 48, F: 41 };            // A2 G2 C3 F2
const SUBM = { Am: 33, G: 31, C: 24, F: 29 };             // A1 G1 C1 F1
// rootless jazz voicings, comping register E4–E5 — the bass owns the
// root, the comp owns the color (7ths and 9ths, not block triads)
const COMP = {
  Am: [[64, 67, 72], [67, 72, 76, 79]],                   // Am7 · Am7(9)
  G:  [[65, 71, 74], [62, 65, 71]],                       // G7 · G7 low
  C:  [[64, 67, 71], [64, 69, 72]],                       // Cmaj7 · C6
  F:  [[65, 69, 72], [69, 72, 76]],                       // Fmaj7 · Fmaj7 rootless
};
const CH = { C: [48, 55, 64, 72, 79], F: [53, 57, 60, 69, 77],
  G: [43, 55, 62, 71, 79], Am: [45, 57, 64, 69, 76] };

// ── BREATH: shallow now — chart LRA is a wall. Last half-bar of each
// 8-bar phrase dips decoratives to 0.68, never deeper ─────────────────
const RESTS = [8, 24, 40, 56, 88].map((d) => [T(d) - BAR / 2, T(d), 0.68]);
const gateAt = (t) => {
  for (const [a, b, depth] of RESTS) if (t >= a && t < b) return depth;
  return 1;
};

// ── KICK: four on the floor from bar 0, out only for the 4-bar break ──
const kickBuf = new Float32Array(NT);
const KICKG = 0.8;
for (let b = 0; b < BARS * 4; b++) {
  const bar = Math.floor(b / 4), beat = b % 4;
  if (inBreak(bar)) continue;
  const g = KICKG * (beat === 0 ? 1.05 : 1) * (SEC(bar) === "finale" ? 1.06 : 1);
  put(kickBuf, S.kick, b * BEAT, g);
}
put(kickBuf, S.kick, T(BARS), 1.0);

const duck = new Float32Array(NT).fill(1);
{
  // THE PUMP is a feature: deep press, slow exhale — the bed breathes
  // against the kick the whole record
  const win = Math.floor(0.5 * SR), tau = 0.16 * SR;
  for (let b = 0; b < BARS * 4; b++) {
    if (inBreak(Math.floor(b / 4))) continue;
    const at = Math.floor(b * BEAT * SR);
    for (let j = 0; j < win && at + j < NT; j++)
      duck[at + j] = Math.min(duck[at + j], 1 - 0.72 * Math.exp(-j / tau));
  }
}

// ── BACKBEAT: the chart's perc-share lever. Snare+clap stack on 2 & 4
// everywhere but the break; finale doubles the clap ───────────────────
const backBuf = new Float32Array(NT);
const CLAP = S.clap ?? repitch(S.snare, 1.19);
const CLAP2 = repitch(CLAP, 1.12);
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
for (let bar = 0; bar < 96; bar++) {
  if (inBreak(bar)) continue;
  const fin = SEC(bar) === "finale";
  for (const beat of [1, 3]) {
    const t = T(bar) + beat * BEAT;
    put(backBuf, S.snare, t + eager(), 3.6);
    put(backBuf, NSNARE, t + eager(), 1.6);
    put(backBuf, CLAP, t + 0.004 + eager(), 2.8);
    if (fin) put(backBuf, CLAP2, t + 0.009 + eager(), 1.8);
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
for (let b = 0; b < 96 * 4; b++) {
  const bar = Math.floor(b / 4);
  if (inBreak(bar)) continue;
  const f = 440 * 2 ** ((SUBM[chord(bar)] - 69) / 12);
  const at = Math.floor((b * BEAT + BEAT / 2) * SR), n = Math.floor(0.34 * BEAT * SR);
  for (let j = 0; j < n && at + j < NT; j++) {
    const t = j / SR;
    const env = Math.min(t / 0.005, 1) * Math.exp(-t / (0.34 * BEAT * 0.55));
    subBuf[at + j] += Math.tanh(2.2 * Math.sin(2 * Math.PI * f * t)) * env * 0.15;
  }
}

// ── BASS: marimba root+fifth dyads, half-note pulse — bass CHORDS,
// not lone roots (the fifth carries the harmony down low) ─────────────
const bassBuf = new Float32Array(NT);
for (let bar = 0; bar < 96; bar++) {
  if (inBreak(bar)) continue;
  const root = BASSM[chord(bar)];
  for (const half of [0, 2]) {
    mixEventMarimba({ startSec: T(bar) + half * BEAT, midi: root,
      durSec: 2 * BEAT, gain: 0.3, preset: "bass", decayMul: 0.8 }, bassBuf, { sampleRate: SR });
    mixEventMarimba({ startSec: T(bar) + half * BEAT, midi: root + 7,
      durSec: 2 * BEAT, gain: 0.18, preset: "bass", decayMul: 0.8 }, bassBuf, { sampleRate: SR });
  }
  // the tenth on beat 3 leans the chord's color into the bar
  mixEventMarimba({ startSec: T(bar) + 2 * BEAT, midi: root + 12 +
    (chord(bar) === "Am" ? 3 : 4),
    durSec: 1.5 * BEAT, gain: 0.14, preset: "bass", decayMul: 0.7 }, bassBuf, { sampleRate: SR });
}

// ── COMP: jazz accompaniment, not a machine. Rootless 7th/9th
// voicings comped in patterns a player would choose — Charleston,
// pushes into the next bar, laid-back strums, velocities that breathe.
// One pattern per two-bar phrase, seeded, so it grooves but never
// loops mechanically ─────────────────────────────────────────────────
const stabBuf = new Float32Array(NT), stabEchoBuf = new Float32Array(NT);
// each pattern: [beat, weight] — beats > 4 are ANTICIPATIONS and take
// the NEXT bar's chord (the push is what makes it jazz)
const COMP_PATTERNS = [
  [[1, 1], [2.5, 0.8]],                     // Charleston
  [[1.5, 0.9], [4.5, 1]],                   // and-of-1, push into next
  [[2.5, 0.85], [4, 0.7]],
  [[1, 0.9], [3.5, 0.75], [4.5, 0.9]],      // busy turn w/ push
  [[2, 0.8]],                               // one lean chord
  [[1.5, 0.85], [3, 0.7]],
  [[1, 0.9, "hold"]],                       // whole bar rings — sustain
  [[1, 0.85, "hold"], [4.5, 0.8]],          // held chord, then the push
];
const lay = () => 0.006 + Math.abs(rnd()) * 0.022;        // behind the beat
for (let bar = 0; bar < 96; bar++) {
  if (inBreak(bar)) continue;
  const s = SEC(bar);
  const g = s.startsWith("verse") ? 1.0 : s.startsWith("pre") ? 1.2
    : s === "finale" ? 1.3 : 1.15;
  const pat = COMP_PATTERNS[Math.floor((0.5 + rnd()) * COMP_PATTERNS.length)
    % COMP_PATTERNS.length];
  for (const [beat, w, hold] of pat) {
    const push = beat > 4;
    const ch = chord(push ? Math.min(bar + 1, BARS - 1) : bar);
    const voicing = COMP[ch][Math.abs(Math.floor(rnd() * 4)) % COMP[ch].length];
    const t = T(bar) + (beat - 1) * BEAT + lay();
    const dur = (hold ? 3.6 : push ? 1.1 : 0.55 + Math.abs(rnd()) * 0.5) * BEAT;
    // a grace slide from a half-step below, sometimes — the player leans in
    const grace = !hold && rnd() > 0.22;
    // strummed, top note leads, each voice its own touch
    voicing.forEach((midi, vi) => {
      const vel = w * (0.75 + Math.abs(rnd()) * 0.45) * (1 - vi * 0.08);
      if (grace)
        mixEventMarimba({ startSec: t - 0.07 + vi * 0.01, midi: midi - 1,
          durSec: 0.08, gain: g * vel * 0.4 * gateAt(t),
          preset: "vibraphone", decayMul: 0.3 }, stabBuf, { sampleRate: SR });
      mixEventMarimba({ startSec: t + vi * (0.012 + Math.abs(rnd()) * 0.014),
        midi, durSec: dur, gain: g * vel * gateAt(t),
        preset: "vibraphone", decayMul: hold ? 2.4 : 0.9 }, stabBuf, { sampleRate: SR });
      // octave pop on the top note, occasionally — a little grin
      if (vi === voicing.length - 1 && rnd() > 0.3)
        mixEventMarimba({ startSec: t + 0.02, midi: midi + 12,
          durSec: 0.3 * BEAT, gain: g * vel * 0.45 * gateAt(t),
          preset: "vibraphone", decayMul: 0.4 }, stabBuf, { sampleRate: SR });
    });
    // soft echo throw opposite side, one comp late
    if (!push && rnd() > 0.1)
      voicing.forEach((midi, vi) => {
        mixEventMarimba({ startSec: t + 0.75 * BEAT + vi * 0.012, midi,
          durSec: dur * 0.6, gain: g * w * 0.28 * gateAt(t),
          preset: "vibraphone", decayMul: 0.6 }, stabEchoBuf, { sampleRate: SR });
      });
  }
  // a playful little run into the next phrase — every 4th bar, usually
  if (bar % 4 === 3 && rnd() > -0.2) {
    const scale = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76];
    const startIdx = 3 + Math.abs(Math.floor(rnd() * 8)) % 5;
    const dirn = rnd() > 0 ? 1 : -1;
    const nNotes = 3 + Math.abs(Math.floor(rnd() * 4)) % 3;
    for (let k = 0; k < nNotes; k++) {
      const midi = scale[Math.max(0, Math.min(scale.length - 1,
        startIdx + k * dirn))];
      const tt = T(bar) + (3 + k * 0.33) * BEAT + lay();
      mixEventMarimba({ startSec: tt, midi, durSec: 0.4 * BEAT,
        gain: g * (0.5 + Math.abs(rnd()) * 0.3) * gateAt(tt),
        preset: "vibraphone", decayMul: 0.5 }, stabBuf, { sampleRate: SR });
    }
  }
}

// ── HATS: eighths with a lifted gain law (chart weight), sixteenth
// fills at phrase turns; opens breathe on the offbeats ────────────────
const hatBuf = new Float32Array(NT), openBuf = new Float32Array(NT);
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (bar >= 96) continue;
  if (inBreak(bar) && !off) continue;                    // break keeps offbeats only
  const g = (off ? 3.3 : 1.8) * (SEC(bar) === "finale" ? 1.1 : 1) * gateAt(t);
  put(hatBuf, S["hat-closed"], t + eager(), g);
}
for (let bar = 7; bar < 96; bar += 4) {                 // sixteenth turn fills
  if (inBreak(bar)) continue;
  for (let k = 0; k < 4; k++)
    put(hatBuf, S["hat-closed"], T(bar) + (3 + k / 4) * BEAT + eager(), 0.7 + 0.2 * k);
}
for (let e = 0; e < BARS * 8; e++) {
  const t = e * BEAT / 2, bar = Math.floor(t / BAR), off = e % 2 === 1;
  if (!off || bar >= 96 || inBreak(bar)) continue;
  const beatIdx = Math.floor((e % 8) / 2);
  if (beatIdx === 1 || beatIdx === 3)
    put(openBuf, S["hat-open"], t + eager(), 1.8 * gateAt(t));
}

// ── SHAKER: sixteenths, staged wide by alternating sides ──────────────
const shakL = new Float32Array(NT), shakR = new Float32Array(NT);
for (let s = 0; s < 96 * 16; s++) {
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
  for (let s = 0; s < 96 * 16; s++) {
    const t = s * BEAT / 4, bar = Math.floor(t / BAR);
    const on16 = s % 4;                                  // 0=beat 2=offbeat
    const g = (on16 === 2 ? 1.0 : on16 === 0 ? 0.6 : 0.4)
      * (SEC(bar) === "finale" ? 1.25 : inBreak(bar) ? 0.7 : 1);
    put(washBuf, hitL, t + eager(), g * gateAt(t));
    put(washBuf2, hitR, t + eager(), g * 0.85 * gateAt(t));
  }
}

// ── TEXTURES: the bicycle childhood — spokes tick through the verses,
// cards riffle at the doors, a fan whirs the break and the outro,
// cloth moves close in the verses, synthesized air swells the doors ──
const texSpokes = new Float32Array(NT), texCards = new Float32Array(NT);
const texFan = new Float32Array(NT), texMat = new Float32Array(NT);
const texAir = new Float32Array(NT);
{
  const sp = existsSync(`${REAL}/spokes.wav`) ? readF32(`${REAL}/spokes.wav`) : null;
  if (sp) for (let st = 0; st < 96 * 16; st++) {
    const t = st * BEAT / 4, bar = Math.floor(t / BAR);
    const sec = SEC(bar);
    if (!sec.startsWith("verse") && sec !== "break") continue;
    if (st % 4 === 1 || st % 4 === 3)
      put(texSpokes, sp, t + eager(), 0.5 + 0.3 * Math.abs(rnd()));
  }
  const cards = existsSync(`${REAL}/cards.wav`) ? readF32(`${REAL}/cards.wav`) : null;
  if (cards) for (const door of [8, 24, 40, 56, 72, 76, 96])
    put(texCards, cards, T(door) - (cards.length / SR) * 0.9, 0.9);
  const fan = existsSync(`${REAL}/fan.wav`) ? readF32(`${REAL}/fan.wav`) : null;
  if (fan) for (const [b0, b1, g] of [[72, 96, 0.45], [96, 100, 0.32]]) {
    const span = (b1 - b0) * BAR, step = fan.length / SR - 1.0;
    for (let off = 0; off < span; off += step) {
      const at = Math.floor((T(b0) + off) * SR);
      const fadeN = Math.floor(1.0 * SR);
      for (let j = 0; j < fan.length && at + j < NT; j++) {
        const fade = Math.min(j / fadeN, 1, Math.max(0, (fan.length - j) / fadeN));
        texFan[at + j] += fan[j] * fade * g;
      }
    }
  }
  const mat = existsSync(`${REAL}/material.wav`) ? readF32(`${REAL}/material.wav`) : null;
  if (mat) for (let bar = 8; bar < 96; bar += 8) {
    if (!SEC(bar).startsWith("verse")) continue;
    put(texMat, mat, T(bar) + BEAT * (1 + Math.abs(rnd()) * 2), 0.38);
  }
  { // AIR — soft band-passed noise swells, synthesized
    const swellN = Math.floor(1.6 * SR);
    const swell = new Float32Array(swellN);
    let lp = 0; const aa = 1 - Math.exp(-2 * Math.PI * 1800 / SR);
    for (let j = 0; j < swellN; j++) {
      lp += aa * (rnd() * 2 - lp);
      const x = j / swellN;
      swell[j] = lp * Math.sin(Math.PI * x) ** 2;
    }
    for (const door of [24, 56, 76]) put(texAir, swell, T(door) - 1.5, 1.1);
    for (let bar = 0; bar < 96; bar++) {
      if (!SEC(bar).startsWith("chorus")) continue;
      put(texAir, swell, T(bar) + Math.abs(rnd()) * 2, 0.16);
    }
  }
}
// wobbles, wannadash-style: the fan breathes through a slow filter,
// the spokes shimmer in amplitude
applyWobble(texFan, { sampleRate: SR, target: "filter", rate: 0.13,
  depth: 0.7, baseCutoffHz: 2600 });
applyWobble(texSpokes, { sampleRate: SR, target: "amp", rate: 4.07, depth: 0.25 });

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
for (const base of [32, 64]) // back half of each full chorus
  for (let rep = 0; rep < 4; rep += 2)
    for (const [bo, beat, note] of FIG) {
      const t = T(base + rep + bo) + (beat - 1) * BEAT;
      put(xyloBuf, XY[note], t + eager(), 0.45 * (1 + rnd() * 0.15) * gateAt(t));
    }
for (let base = 80; base < 96; base += 4) {  // the finale answer, new
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
for (let bar = 0; bar < 96; bar++) {
  const s = SEC(bar);
  const nv = s.startsWith("verse") ? 3 : s.startsWith("pre") ? 4
    : s === "break" ? 5 : s === "finale" ? 4 : s.startsWith("chorus") ? 4 : 0;
  if (!nv) continue;
  const voices = CH[chord(bar)].slice(0, nv + 1);        // low root back — bass chords
  const swell = s === "break" ? 1.4 : 1;
  const a = Math.floor(T(bar) * SR), n = Math.floor((BAR + 0.6) * SR);
  for (let vi = 0; vi < voices.length; vi++) {
    const f = 440 * 2 ** ((voices[vi] - 69) / 12);
    const g = 0.034 * (1 - vi * 0.12) * swell;
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
rush(8, 0.07, 7); rush(24, 0.10); rush(40, 0.07, 7);
rush(56, 0.10); rush(72, 0.08); rush(76, 0.11, 12, 1.6); rush(96, 0.06, 6, 1.5);
for (const [door, g] of [[24, 0.45], [56, 0.45], [76, 0.5]])
  put(bellBuf, S["reverse-bell"], T(door) - S["reverse-bell"].length / SR, g);

// ── THE VOICE: lead bus + group unison + stretched butterfly choir ───
const voxStem = new Float32Array(NT);
const choirStem = new Float32Array(NT);
const haloStem = new Float32Array(NT);
const PY = `${REPO}/pop/.venv/bin/python`;
// aesthetivox is the chain (lane law) — prefer the note-locked retimed
// stem when it exists; sacredvox is the fallback preview voice
const VOXF = process.env.IMAB_VOX ? resolve(process.env.IMAB_VOX)
  : existsSync(`${OUT}/imab-aesthetivox-retimed.wav`)
    ? `${OUT}/imab-aesthetivox-retimed.wav` : `${OUT}/imab-sacredvox.wav`;
let haveVox = existsSync(VOXF);
if (haveVox) {
  // lead bus: high-pass the rumble, presence for listenability, one
  // gentle 2:1 so the phrase sits level on the wall
  const leadF = `${WORK}/hb-lead.wav`;
  sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", VOXF,
    "-af", "highpass=f=90,equalizer=f=3000:t=q:w=1.4:g=2.5," +
    "acompressor=threshold=0.25:ratio=2:attack=12:release=160:makeup=1.2",
    "-ar", String(SR), "-ac", "1", leadF]);
  const vox = readF32(leadF);
  for (const door of CHORUS_DOORS) put(voxStem, vox, T(door), 1);

  // the GROUP: every regulated set take doubles the lead at the doors,
  // slightly eager/late — a room of people saying it together
  const sets = readdirSync(OUT).filter((f) => /^imab-set-.*\.wav$/.test(f));
  for (const [si, f] of sets.entries())
    for (const door of CHORUS_DOORS)
      put(voxStem, readF32(`${OUT}/${f}`), T(door) + (si % 2 ? 0.014 : -0.011), 0.45);

  // ── THE WORD SCORE: pop/imab/vocal-score.json places single words
  // and phrases anywhere on the grid — placement, duration (stretch),
  // pitch, and harmony stacks are all playable. Timings come from
  // lyrictrack's syllable boundaries against the retimed stem ────────
  const LYR = `${OUT}/imab-sacredvox.lyrics.json`;
  const SCOREF = resolve(HERE, "../vocal-score.json");
  if (existsSync(LYR) && existsSync(SCOREF)) {
    const syl = JSON.parse(readFileSync(LYR, "utf8")).syllables;
    const findSyl = (spec) => {
      const [label, nth] = spec.split("#");
      const hits = syl.filter((x) => x.label === label);
      return hits[Number(nth ?? 0)] ?? null;
    };
    const sliceCache = {};
    const wordWav = (fromMs, toMs, stretch, pitch) => {
      const key = `${fromMs}-${toMs}-${stretch}-${pitch}`;
      if (sliceCache[key]) return sliceCache[key];
      const t0 = Math.max(0, fromMs / 1000 - 0.04);
      const t1 = toMs / 1000 + 0.08;
      const dur = t1 - t0;
      const cut = `${WORK}/hb-w-${key}.wav`;
      if (!existsSync(cut)) {
        const raw = `${WORK}/hb-wraw-${key}.wav`;
        sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
          "-i", leadF, "-af",
          `atrim=start=${t0.toFixed(3)}:end=${t1.toFixed(3)},asetpts=PTS-STARTPTS,` +
          `afade=t=in:d=0.02,afade=t=out:st=${Math.max(0, dur - 0.07).toFixed(3)}:d=0.07`,
          "-ar", String(SR), "-ac", "1", raw]);
        if (stretch !== 1 || pitch !== 0)
          sh("rubberband", ["-t", String(stretch), "-p", String(pitch), "-F", "-q", raw, cut]);
        else sh("cp", [raw, cut]);
      }
      return (sliceCache[key] = existsSync(cut) ? readF32(cut) : null);
    };
    const score = JSON.parse(readFileSync(SCOREF, "utf8"));
    let placed = 0;
    for (const ev of score.events ?? []) {
      const stretch = ev.stretch ?? 1, pitch = ev.pitch ?? 0;
      let fromMs, toMs;
      if (ev.src.startsWith("words:")) {          // span: words:just..tume
        const [a2, b2] = ev.src.slice(6).split("..");
        const s1 = findSyl(a2), s2 = findSyl(b2);
        if (!s1 || !s2) continue;
        fromMs = s1.fromMs; toMs = s2.toMs;
      } else if (ev.src.startsWith("word:")) {
        const w = findSyl(ev.src.slice(5));
        if (!w) continue;
        fromMs = w.fromMs; toMs = w.toMs;
      } else continue;
      const t = T(ev.bar) + ((ev.beat ?? 1) - 1) * BEAT;
      const layers = [[pitch, 1], ...(ev.harm ?? [])
        .filter((h) => h !== 0).map((h) => [pitch + h, 0.55])];
      for (const [pp, hg] of layers) {
        const y = wordWav(fromMs, toMs, stretch, pp);
        if (!y) continue;
        put(stretch >= 2.5 ? choirStem : voxStem, y, t, (ev.gain ?? 1) * hg);
        placed++;
      }
    }
    console.log(`word score: ${placed} placements from vocal-score.json`);
  }

  // halo: the lead through the church send, wide and behind
  sh(PY, [`${REPO}/spinging/lib/vocal_bus.py`, "reverb", leadF,
    `${WORK}/hb-halo.wav`, "-14", "1.6"]);
  if (existsSync(`${WORK}/hb-halo.wav`)) {
    const halo = readF32(`${WORK}/hb-halo.wav`);
    for (const door of CHORUS_DOORS) put(haloStem, halo, T(door), 1);
  }

  // BUTTERFLY CHOIR: takes stretched LONG (rubberband, formants kept)
  // at unison / +4 / +7 — ×2, ×3 and ×4 stretches so the syllables
  // dissolve into sustained tone. Runs from the second chorus onward:
  // tissue first, bloom at the break, bed under the finale
  const choirSrc = [VOXF, ...sets.map((f) => `${OUT}/${f}`)].slice(0, 3);
  const variants = [];
  choirSrc.forEach((src, i) => {
    const recipe = i === 0
      ? [[2.0, 0], [2.0, 4], [2.0, 7], [3.0, 0], [3.0, 7], [4.0, 0]]
      : [[2.0, 0], [3.0, 0]];
    for (const [stretch, pitch] of recipe) {
      const out = `${WORK}/hb-choir-${i}-${stretch}-${pitch}.wav`;
      if (!existsSync(out))
        sh("rubberband", ["-t", String(stretch), "-p", String(pitch), "-F",
          "-q", src, out]);
      if (existsSync(out)) variants.push({ out, stretch });
    }
  });
  const CHOIR_AT = [[56, 0.3], [64, 0.4], [72, 0.9], [76, 0.7], [84, 0.55]];
  for (const [vi, v] of variants.entries()) {
    const y = readF32(v.out);
    for (const [at, w] of CHOIR_AT) {
      if (v.stretch >= 3 && at < 72) continue;  // deepest drones save for the bloom
      const t0 = Math.floor(T(at) * SR);
      const fadeN = Math.floor(2.5 * SR);
      for (let j = 0; j < y.length && t0 + j < NT; j++) {
        const fade = Math.min(j / fadeN, 1);
        choirStem[t0 + j] += y[j] * fade * w * (0.4 - vi * 0.03);
      }
    }
  }

  // HOLYVOX: the angelic cut — note-locked half-time with its church
  // halo already printed. Sustained tissue through verse 2, the break
  // bloom, and the whole finale
  const HOLY = `${OUT}/imab-holyvox.wav`;
  if (existsSync(HOLY)) {
    const holy = readF32(HOLY);
    for (const [at, w] of [[40, 0.5], [72, 1.0], [76, 0.7], [88, 0.7]]) {
      const t0 = Math.floor(T(at) * SR);
      const fadeN = Math.floor(2 * SR);
      for (let j = 0; j < holy.length && t0 + j < NT; j++)
        choirStem[t0 + j] += holy[j] * Math.min(j / fadeN, 1) * w * 0.5;
    }
  }
  // the angels sway — slow amplitude wobble across the whole choir
  applyWobble(choirStem, { sampleRate: SR, target: "amp", rate: 0.09, depth: 0.22 });
  console.log(`vox: lead + ${sets.length} group takes + ${variants.length} choir strands + holyvox`);
} else console.log("· no vocal found — baking the instrumental bed");

// the follower: everything decorative leans away while the voice sings
const fo = new Float32Array(NT);
{
  const atk = 1 - Math.exp(-1 / (0.015 * SR)), rel = 1 - Math.exp(-1 / (0.25 * SR));
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

// ── THE STAGE: kick/sub/bass/vox mono; everything decorative WIDE.
// Chart stereo correlation runs 0.68–0.87 — stage for it ──────────────
const L = new Float32Array(NT), R = new Float32Array(NT);
const sideB = new Float32Array(NT);
// Special Sign space (wannadash render.mjs): the direct voice stays dry
// on a stable equal-power pan — mono-safe — while a cheap interaural
// model (ITD + head shadow) feeds a band-limited side bus returned
// antisymmetrically in the premaster. Space as a return, not a
// replacement.
const depthFilter = (m, depth) => {
  const a = 1 - Math.exp(-2 * Math.PI * (9000 - 6500 * depth) / SR);
  const y = new Float32Array(m.length); let acc = 0;
  for (let i = 0; i < m.length; i++) { acc += a * (m[i] - acc); y[i] = acc; }
  return y;
};
const addPlaced = (src, deg = 0, depth = 0, gain = 1, perSample = null, sideAmt = 0) => {
  let m = src;
  if (depth > 0) { m = depthFilter(m, depth); gain *= 1 - 0.25 * depth; }
  const pan = Math.max(-1, Math.min(1, deg / 40));
  const a = (Math.PI / 4) * (1 + pan);
  const gl = Math.cos(a), gr = Math.sin(a);
  const az = pan * 1.2;
  const itd = Math.round(0.00027 * SR * Math.sin(az));
  const shadow = 0.35 * Math.sin(az);
  for (let i = 0; i < m.length && i < NT; i++) {
    const e = gain * (perSample ? perSample(i) : 1);
    const v = m[i] * e;
    L[i] += v * gl; R[i] += v * gr;
    if (sideAmt) {
      const li = i + itd, ri = i - itd;
      const l = li >= 0 && li < NT ? v * (1 - shadow) : 0;
      const r = ri >= 0 && ri < NT ? v * (1 + shadow) : 0;
      sideB[i] += 0.5 * (l - r) * sideAmt;
    }
  }
};
// a texture that circles the head: equal-power pan swept by a slow LFO
const addOrbit = (src, rateHz, depthDeg, gain = 1, perSample = null, sideAmt = 0.5) => {
  const phase = Math.abs(rnd()) * Math.PI * 2;
  for (let i = 0; i < src.length && i < NT; i++) {
    const e = gain * (perSample ? perSample(i) : 1);
    const v = src[i] * e;
    if (!v) continue;
    const pan = (depthDeg / 40) * Math.sin(2 * Math.PI * rateHz * (i / SR) + phase);
    const a = (Math.PI / 4) * (1 + pan);
    L[i] += v * Math.cos(a); R[i] += v * Math.sin(a);
    if (sideAmt) sideB[i] += 0.5 * v * -Math.sin(pan * 1.2) * 0.35 * sideAmt;
  }
};
const pump = (i) => duck[i];
const pumpHalf = (i) => 1 - (1 - duck[i]) * 0.5;
const pumpVox = (i) => duck[i] * (1 - 0.35 * fo[i]);      // lean away from him
const voxDuck = (i) => 1 - 0.3 * fo[i];

addPlaced(kickBuf, 0, 0, 1);
addPlaced(subBuf, 0, 0, 1, pump);   // the sub rides the pump — classic sidechain swell
addPlaced(bassBuf, 0, 0.15, 1, pump);
addPlaced(backBuf, -4, 0.08, 1, null, 0.3);
addPlaced(stabBuf, -20, 0.18, 1, pumpVox, 0.5);
addPlaced(stabEchoBuf, 24, 0.3, 1, pumpVox, 0.6);
addPlaced(hatBuf, 18, 0.1, 1, null, 0.35);
addPlaced(openBuf, -16, 0.1, 1, null, 0.4);
addPlaced(shakL, -30, 0.15, 1, null, 0.5);
addPlaced(shakR, 30, 0.15, 1, null, 0.5);
addPlaced(washBuf, 26, 0, 1, (i) => pump(i) * voxDuck(i), 0.4);
addPlaced(washBuf2, -26, 0, 1, (i) => pump(i) * voxDuck(i), 0.4);
addPlaced(sineBuf, 0, 0.35, 1, pumpVox, 0.3);
addPlaced(xyloBuf, -16, 0.2, 1, pumpVox, 0.5);
addPlaced(clickBuf, 10, 0, 1);
addPlaced(bellBuf, 12, 0.3, 1, null, 0.4);
// the bicycle childhood, spatialized: spokes and fan circle the head,
// cards riffle from alternating wings, cloth close, air everywhere
addOrbit(texSpokes, 0.06, 34, 0.8, (i) => voxDuck(i), 0.6);
addOrbit(texFan, 0.04, 28, 1, (i) => pump(i) * voxDuck(i), 0.5);
addPlaced(texCards, 30, 0.15, 0.9, null, 0.6);
addPlaced(texMat, -8, 0.4, 0.7, (i) => voxDuck(i), 0.2);
addOrbit(texAir, 0.05, 36, 0.8, null, 0.5);

if (haveVox) {
  const rms = (a) => { let s = 0, n = 0; for (let i = 0; i < a.length; i++) if (Math.abs(a[i]) > 1e-4) { s += a[i] * a[i]; n++; } return Math.sqrt(s / Math.max(1, n)); };
  const inst = new Float32Array(NT);
  for (let i = 0; i < NT; i++) inst[i] = (L[i] + R[i]) / 2;
  const ri = rms(inst);
  const vg = Math.min(10, (ri * 3.2) / Math.max(1e-9, rms(voxStem)));
  addPlaced(voxStem, 0, 0, vg);
  addPlaced(haloStem, 24, 0.25, vg * 0.3);
  addPlaced(haloStem, -24, 0.25, vg * 0.24);
  const cg = Math.min(8, (ri * 1.3) / Math.max(1e-9, rms(choirStem) || 1));
  addPlaced(choirStem, 0, 0.3, cg, pumpVox);   // the angels pump too
  console.log(`voice: lead ×${vg.toFixed(2)} · choir ×${cg.toFixed(2)} at doors ${CHORUS_DOORS.join("/")}`);
}

// ── the side RETURN: band-limit (250–8k), breathe with the pump,
// fold antisymmetrically into the mains ───────────────────────────────
{
  let lpLo = 0, lpHi = 0;
  const aLo = 1 - Math.exp(-2 * Math.PI * 250 / SR);
  const aHi = 1 - Math.exp(-2 * Math.PI * 8000 / SR);
  for (let i = 0; i < NT; i++) {
    lpLo += aLo * (sideB[i] - lpLo);
    lpHi += aHi * (sideB[i] - lpHi);
    const band = (lpHi - lpLo) * 0.6 * (0.5 + 0.5 * duck[i]);
    L[i] += band; R[i] -= band;
  }
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
const TARGET = -10.0;   // house master law: −10 ±1 LUFS, ≤ −2 dBTP
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
  "treble=g=3:f=7500," +
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
    "-af", `volume=${gain.toFixed(2)}dB,aresample=192000,` +
      "alimiter=limit=0.74:attack=5:release=90:level=disabled,aresample=48000",
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
