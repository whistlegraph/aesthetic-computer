#!/usr/bin/env node
// render.mjs — the hippyhayzard bed builder. Mirrors the recap/bin
// trance.mjs / trap.mjs cli shape: read the arrangement, mix the bed in
// node from AC instruments, write one mp3. No Web Audio, no Suno —
// every sample is synthesized here (bottom-up posture, pop/SCORE.md).
//
// The lane is "happy hardcore × nightcore", blended intelligently:
//   • HIPPY  — bright major euphoria: chunky 4-on-floor, reverse bass,
//              the hoover hook, glock/M1-bell doubling an octave up
//              (the nightcore sugar), swung 16th hats.
//   • HAZARD — the swingy switch: drop to half-time + relative minor,
//              kill the glock, bring the skrill in as a rave siren /
//              talking-bass. Same theme, dark. Then spring back to hippy.
//
// Voices: ../synths/hoover.mjs (new, gating), ../../dance/synths/
// skrill.mjs (the talking bass we built), ../../dance/synths/
// sinepower.mjs (clean sine stack → glock bells). Drums are synthesized
// inline (kick / reverse-bass / snare / swung hats / riser).
//
//   node pop/hippyhayzard/bin/render.mjs            # → out/hippyhayzard.mp3
//   node pop/hippyhayzard/bin/render.mjs --bpm 172 --out ~/hh.mp3

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixEventHoover } from "../synths/hoover.mjs";
import { mixEventZitar } from "../synths/zitar.mjs";
import { mixEventSkrill } from "../../dance/synths/skrill.mjs";
import { mixEventSinePower } from "../../dance/synths/sinepower.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p) return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const BPM = Number(flags.bpm ?? 174);
const beat = 60 / BPM;
const bar = beat * 4;
const sx = beat / 4;                 // one 16th note
const SWING = 0.16;                   // delay every off-16th by 16% (the "swingy")

// ── inline drum synthesis ──────────────────────────────────────────────
function add(buf, idx, v) { if (idx >= 0 && idx < buf.length) buf[idx] += v; }

function kick(buf, startSec, g = 1.0) {
  const dur = 0.20, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const f = 46 + (132 - 46) * Math.exp(-t * 40);     // chunky pitch drop
    ph += (2 * Math.PI * f) / SR;
    const body = Math.sin(ph) * Math.exp(-t * 15);
    const click = i < SR * 0.004 ? (Math.random() * 2 - 1) * 0.7 * (1 - i / (SR * 0.004)) : 0;
    add(buf, s0 + i, (body + click) * g);
  }
}

function snare(buf, startSec, g = 0.8) {
  const dur = 0.16, n = Math.floor(dur * SR), s0 = Math.floor(startSec * SR);
  let prev = 0, ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR;
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;                   // crude high-pass
    ph += (2 * Math.PI * 185) / SR;
    const tone = Math.sin(ph) * Math.exp(-t * 28) * 0.5;
    add(buf, s0 + i, (hp * Math.exp(-t * 22) + tone) * g);
  }
}

function hat(buf, startSec, g = 0.32, len = 0.028) {
  const n = Math.floor(len * SR), s0 = Math.floor(startSec * SR);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz;
    add(buf, s0 + i, hp * Math.exp(-(i / SR) * 220) * g);
  }
}

// Reverse bass: swells IN then is cut by the next kick — the rave
// off-beat engine. One note per off-beat, A1-ish.
function reverseBass(buf, startSec, durSec, midi, g = 0.7) {
  const n = Math.floor(durSec * SR), s0 = Math.floor(startSec * SR);
  const f = 440 * Math.pow(2, (midi - 69) / 12);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const env = Math.pow(w, 1.7) * (1 - Math.pow(w, 8));  // ramp up, hard cut
    ph += (2 * Math.PI * f) / SR;
    const saw = (((ph / (2 * Math.PI)) % 1) * 2 - 1);
    add(buf, s0 + i, (Math.sin(ph) * 0.6 + saw * 0.4) * env * g);
  }
}

function riser(buf, startSec, durSec, g = 0.5) {
  const n = Math.floor(durSec * SR), s0 = Math.floor(startSec * SR);
  let lp = 0;
  for (let i = 0; i < n; i++) {
    const w = i / n;
    const nz = Math.random() * 2 - 1;
    const a = 0.02 + 0.5 * w;                          // open the filter
    lp += a * (nz - lp);
    add(buf, s0 + i, (nz - lp) * Math.pow(w, 1.5) * g); // rising hiss
  }
}

// ── the arrangement (the .np mirror) ───────────────────────────────────
// A-major "hippy" hook, 2-bar phrase = 32 sixteenths: [midi, 16ths].
const HOOK = [
  [69, 2], [76, 2], [73, 2], [69, 2], [71, 2], [74, 2], [76, 4],
  [78, 2], [76, 2], [73, 2], [69, 2], [71, 2], [69, 2], [64, 4],
];

// Diatonic harmonizer in A major — keeps the skrill consonant with the
// hoover instead of a clashing low siren. `steps` in scale degrees
// (-2 = a diatonic third below).
const AMAJ = [0, 2, 4, 5, 7, 9, 11];
function diatonic(midi, steps) {
  const semi = midi - 69;
  const octs = Math.floor(semi / 12);
  const pc = semi - octs * 12;
  let idx = AMAJ.indexOf(pc);
  if (idx < 0) { idx = 0; for (let k = 0; k < AMAJ.length; k++) if (AMAJ[k] <= pc) idx = k; }
  const pos = octs * 7 + idx + steps;
  const nOct = Math.floor(pos / 7);
  const nIdx = ((pos % 7) + 7) % 7;
  return 69 + nOct * 12 + AMAJ[nIdx];
}

// ── another demo: skrill harmonized + in right away + up in register ───
const MODE = flags.mode || "arc";
if (MODE === "harmony") {
  // slower + held skipaloo whoop + growl bass + low long-decay zitar
  // drone + a 16-bar Bach-ish chorale progression in A.
  const BPMH = 152;
  const hbeat = 60 / BPMH, hbar = hbeat * 4, hsx = hbeat / 4;
  const PHRASE = 16;
  const BARS = 38;                       // ≈ 60s at 152 BPM (phrase looped)
  const Nh = Math.ceil((BARS * hbar + 3.0) * SR);  // +tail for the long drone
  const busH = new Float32Array(Nh), drmH = new Float32Array(Nh), kt = [];

  // Hand-voiced chorale. bass = stepwise chorale bass (the growl bass +
  // the low zitar drone an octave under it). pad = 3 mid-register chord
  // tones. lead = held/skippy top voice [offBeat, midi, durBeats].
  // I  V6  vi  iii6  IV  I6  ii7  V7 | I  V7/IV  IV  iv6  I64  V7  vi  V7
  const PROG = [
    { bass: 45, pad: [57, 61, 64], lead: [[0, 69, 2.5], [2.5, 76, 1.5]] }, // A
    { bass: 44, pad: [56, 59, 64], lead: [[0, 71, 4]] },                   // E/G#
    { bass: 42, pad: [57, 61, 66], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] }, // F#m
    { bass: 40, pad: [56, 61, 64], lead: [[0, 76, 4]] },                   // C#m/E
    { bass: 38, pad: [57, 62, 66], lead: [[0, 78, 2.5], [2.5, 74, 1.5]] }, // D
    { bass: 37, pad: [57, 61, 64], lead: [[0, 76, 4]] },                   // A/C#
    { bass: 35, pad: [57, 62, 66], lead: [[0, 74, 2.5], [2.5, 78, 1.5]] }, // Bm7
    { bass: 40, pad: [56, 59, 62], lead: [[0, 74, 4]] },                   // E7
    { bass: 45, pad: [57, 61, 64], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] }, // A
    { bass: 43, pad: [57, 61, 67], lead: [[0, 76, 4]] },                   // A7/G
    { bass: 38, pad: [57, 62, 66], lead: [[0, 78, 2.5], [2.5, 74, 1.5]] }, // D
    { bass: 41, pad: [57, 62, 65], lead: [[0, 77, 4]] },                   // Dm/F (iv6, F♮)
    { bass: 40, pad: [57, 61, 64], lead: [[0, 76, 2.5], [2.5, 73, 1.5]] }, // A/E
    { bass: 40, pad: [56, 59, 62], lead: [[0, 74, 4]] },                   // E7
    { bass: 42, pad: [57, 61, 66], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] }, // F#m (deceptive)
    { bass: 40, pad: [56, 59, 62], lead: [[0, 71, 2], [2, 76, 2]] },       // E7 turnaround
  ];

  // Nearest chord tone a third-or-more below the lead — keeps the skrill
  // consonant through every change (incl. the borrowed iv).
  function harmUnder(lead, pad) {
    let best = null;
    for (const base of pad) {
      for (let oct = -1; oct <= 2; oct++) {
        const c = base + 12 * oct;
        if (c <= lead - 3 && (best === null || c > best)) best = c;
      }
    }
    return best === null ? lead - 4 : best;
  }

  for (let b = 0; b < BARS; b++) {
    const bt = b * hbar;
    // loop the 16-bar phrase; final bar resolves to a held tonic A
    const ch = b === BARS - 1
      ? { bass: 45, pad: [57, 61, 64], lead: [[0, 69, 4]] }
      : PROG[b % PHRASE];

    // low super-long-decay zitar drone — struck every 2 bars, an octave
    // under the chorale bass, washing across the changes (tanpura-ish)
    if (b % 2 === 0) {
      mixEventZitar(
        { startSec: bt, midi: ch.bass - 12, durSec: hbar * 2, gain: 0.32, preset: "drone" },
        busH, { sampleRate: SR, decay: 6.0 },
      );
    }
    // growl bass on the chorale bass note
    mixEventSkrill(
      { startSec: bt, midi: ch.bass, durSec: hbar * 0.96, gain: 0.44, preset: "growl", lfo: "1/4" },
      busH, { sampleRate: SR, bpm: BPMH },
    );
    // chord pad — the Bach harmonic motion, behind everything
    for (const pm of ch.pad) {
      mixEventSinePower(
        { startSec: bt, midi: pm, durSec: hbar * 1.08, gain: 0.13 },
        busH, { sampleRate: SR, preset: "pad" },
      );
    }
    // lead voice — held skipaloo whoop + buzzing zitar + skrill 3rd + glock
    for (const [offB, m, dB] of ch.lead) {
      const t = bt + offB * hbeat, d = dB * hbeat;
      mixEventHoover({ startSec: t, midi: m, durSec: d * 0.98, gain: 0.56 },
        busH, { sampleRate: SR, preset: "whoop" });
      mixEventZitar({ startSec: t, midi: m, durSec: d * 1.35, gain: 0.5 },
        busH, { sampleRate: SR, preset: "sitar" });
      mixEventSkrill({ startSec: t, midi: harmUnder(m, ch.pad), durSec: d * 0.98,
        gain: 0.36, preset: "growl", lfo: "1/8" }, busH, { sampleRate: SR, bpm: BPMH });
      mixEventSinePower({ startSec: t, midi: m + 12, durSec: d * 0.5, gain: 0.18 },
        busH, { sampleRate: SR, preset: "stab" });
    }

    // drums
    for (let q = 0; q < 4; q++) {
      kick(drmH, bt + q * hbeat, 1.0); kt.push(bt + q * hbeat);
      reverseBass(busH, bt + q * hbeat + hbeat * 0.5, hbeat * 0.5, 33, 0.34);
    }
    snare(drmH, bt + hbeat, 0.64); snare(drmH, bt + hbeat * 3, 0.64);
    for (let e = 0; e < 16; e++) {
      const sw = e % 2 === 1 ? SWING * hsx : 0;
      if (e % 4 !== 0) hat(drmH, bt + e * hsx + sw, e % 2 ? 0.13 : 0.21);
    }
  }
  const dk = new Float32Array(Nh).fill(1);
  for (const k of kt) {
    const s0 = Math.floor(k * SR), len = Math.floor(0.16 * SR);
    for (let i = 0; i < len && s0 + i < Nh; i++) {
      const d = 0.42 + 0.58 * (i / len);
      if (d < dk[s0 + i]) dk[s0 + i] = d;
    }
  }
  const mh = new Float32Array(Nh);
  let pkh = 0;
  for (let i = 0; i < Nh; i++) { mh[i] = Math.tanh((busH[i] * dk[i] + drmH[i]) * 1.05); const a = Math.abs(mh[i]); if (a > pkh) pkh = a; }
  if (pkh > 0) { const nrm = 0.8 / pkh; for (let i = 0; i < Nh; i++) mh[i] *= nrm; }
  const outH = expandHome(flags.out) || resolve(HERE, "../out/hippyhayzard-harmony.mp3");
  mkdirSync(dirname(outH), { recursive: true });
  const rawH = `${outH}.f32.raw`, bb = Buffer.alloc(Nh * 4);
  for (let i = 0; i < Nh; i++) bb.writeFloatLE(mh[i], i * 4);
  writeFileSync(rawH, bb);
  console.log(`→ hippyhayzard [harmony] · ${BPMH} BPM · ${BARS} bars · ${(BARS * hbar).toFixed(0)}s · bachian · whoop+zitar+skrill3rd+growlbass+low-drone`);
  const ffh = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawH,
    "-c:a", "libmp3lame", "-q:a", "2", outH], { stdio: "inherit" });
  try { unlinkSync(rawH); } catch {}
  if (ffh.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outH}`);
  process.exit(0);
}

// ── the 1:28 song: earthbound/mother sorrow-ballad with a break ────────
if (MODE === "song") {
  const BPMH = 152;                       // ~76 half-time ballad feel
  const hbeat = 60 / BPMH, hbar = hbeat * 4, hsx = hbeat / 4;

  // Same 16-bar Bach chorale as the harmony demo (hand-voiced).
  const PROG = [
    { bass: 45, pad: [57, 61, 64], lead: [[0, 69, 2.5], [2.5, 76, 1.5]] },
    { bass: 44, pad: [56, 59, 64], lead: [[0, 71, 4]] },
    { bass: 42, pad: [57, 61, 66], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] },
    { bass: 40, pad: [56, 61, 64], lead: [[0, 76, 4]] },
    { bass: 38, pad: [57, 62, 66], lead: [[0, 78, 2.5], [2.5, 74, 1.5]] },
    { bass: 37, pad: [57, 61, 64], lead: [[0, 76, 4]] },
    { bass: 35, pad: [57, 62, 66], lead: [[0, 74, 2.5], [2.5, 78, 1.5]] },
    { bass: 40, pad: [56, 59, 62], lead: [[0, 74, 4]] },
    { bass: 45, pad: [57, 61, 64], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] },
    { bass: 43, pad: [57, 61, 67], lead: [[0, 76, 4]] },
    { bass: 38, pad: [57, 62, 66], lead: [[0, 78, 2.5], [2.5, 74, 1.5]] },
    { bass: 41, pad: [57, 62, 65], lead: [[0, 77, 4]] },        // borrowed iv
    { bass: 40, pad: [57, 61, 64], lead: [[0, 76, 2.5], [2.5, 73, 1.5]] },
    { bass: 40, pad: [56, 59, 62], lead: [[0, 74, 4]] },
    { bass: 42, pad: [57, 61, 66], lead: [[0, 73, 2.5], [2.5, 69, 1.5]] },
    { bass: 40, pad: [56, 59, 62], lead: [[0, 71, 2], [2, 76, 2]] },
  ];
  function harmUnder(lead, pad) {
    let best = null;
    for (const base of pad) for (let o = -1; o <= 2; o++) {
      const c = base + 12 * o;
      if (c <= lead - 3 && (best === null || c > best)) best = c;
    }
    return best === null ? lead - 4 : best;
  }

  // structure ≈ 1:28: intro 4 · v1 12 · refrain 8 · v2 12 · break 8 ·
  // refrain 12  → 56 bars × 1.579s ≈ 88s, +tail for the drone ring-out.
  const SONG = [
    { kind: "intro",   bars: 4  },
    { kind: "verse",   bars: 12 },
    { kind: "refrain", bars: 8  },
    { kind: "verse",   bars: 12 },
    { kind: "break",   bars: 8  },
    { kind: "refrain", bars: 12 },
  ];
  const totBars = SONG.reduce((a, s) => a + s.bars, 0);
  const Nh = Math.ceil((totBars * hbar + 5.0) * SR);
  const busH = new Float32Array(Nh), drmH = new Float32Array(Nh), kt = [];

  let bar0 = 0;
  for (const sec of SONG) {
    const full = sec.kind === "refrain";
    const sing = sec.kind === "verse" || sec.kind === "refrain";
    for (let i = 0; i < sec.bars; i++) {
      const b = bar0 + i, bt = b * hbar, ch = PROG[b % 16];

      // low super-long-decay zitar drone — the spine; loud + bare in break
      if (i % 2 === 0) {
        mixEventZitar(
          { startSec: bt, midi: ch.bass - 12, durSec: hbar * 2,
            gain: sec.kind === "break" ? 0.42 : 0.26, preset: "drone" },
          busH, { sampleRate: SR, decay: sec.kind === "break" ? 7.5 : 5.5 },
        );
      }
      // soft pad everywhere — the Bach motion, quietest in the break
      for (const pm of ch.pad) {
        mixEventSinePower(
          { startSec: bt, midi: pm, durSec: hbar * 1.1,
            gain: sec.kind === "break" ? 0.085 : 0.12 },
          busH, { sampleRate: SR, preset: "pad" },
        );
      }

      if (sec.kind !== "break" && sec.kind !== "intro") {
        // gentle growl bass on the chorale bass
        mixEventSkrill(
          { startSec: bt, midi: ch.bass, durSec: hbar * 0.95,
            gain: full ? 0.34 : 0.28, preset: "growl", lfo: "1/4" },
          busH, { sampleRate: SR, bpm: BPMH },
        );
      }

      // instrumental lead voice (the sung line tracks these notes via
      // the .np). quiet in verses, a touch fuller in refrains. silent
      // in the break — that space is for the whispered vocal.
      if (sec.kind !== "break") {
        for (const [offB, m, dB] of ch.lead) {
          const t = bt + offB * hbeat, d = dB * hbeat;
          mixEventZitar({ startSec: t, midi: m, durSec: d * 1.35,
            gain: sec.kind === "intro" ? 0.42 : full ? 0.46 : 0.34 },
            busH, { sampleRate: SR, preset: "sitar" });
          if (sec.kind !== "intro") {
            mixEventHoover({ startSec: t, midi: m, durSec: d * 0.97,
              gain: full ? 0.34 : 0.24 }, busH, { sampleRate: SR, preset: "whoop" });
            mixEventSkrill({ startSec: t, midi: harmUnder(m, ch.pad),
              durSec: d * 0.97, gain: full ? 0.20 : 0.15, preset: "growl", lfo: "1/8" },
              busH, { sampleRate: SR, bpm: BPMH });
          }
        }
      }

      // soft half-time kit — only in verse/refrain, never intro/break
      if (sing) {
        kick(drmH, bt, 0.62); kt.push(bt);
        kick(drmH, bt + hbeat * 2, 0.62); kt.push(bt + hbeat * 2);
        if (full) snare(drmH, bt + hbeat * 2, 0.34);
        for (let e = 0; e < 8; e++) {
          const sw = e % 2 === 1 ? SWING * hsx * 1.2 : 0;
          if (e % 2 === 0) hat(drmH, bt + e * (hbeat / 2) + sw, full ? 0.12 : 0.09);
        }
      }
    }
    bar0 += sec.bars;
  }

  // gentle sidechain (ballad — shallow duck), soft master
  const dk = new Float32Array(Nh).fill(1);
  for (const k of kt) {
    const s0 = Math.floor(k * SR), len = Math.floor(0.14 * SR);
    for (let i = 0; i < len && s0 + i < Nh; i++) {
      const d = 0.62 + 0.38 * (i / len);
      if (d < dk[s0 + i]) dk[s0 + i] = d;
    }
  }
  const mh = new Float32Array(Nh);
  let pkh = 0;
  for (let i = 0; i < Nh; i++) { mh[i] = Math.tanh((busH[i] * dk[i] + drmH[i]) * 1.0); const a = Math.abs(mh[i]); if (a > pkh) pkh = a; }
  if (pkh > 0) { const n = 0.78 / pkh; for (let i = 0; i < Nh; i++) mh[i] *= n; }
  const outS = expandHome(flags.out) || resolve(HERE, "../out/hippyhayzard.mp3");
  mkdirSync(dirname(outS), { recursive: true });
  const rawS = `${outS}.f32.raw`, bbs = Buffer.alloc(Nh * 4);
  for (let i = 0; i < Nh; i++) bbs.writeFloatLE(mh[i], i * 4);
  writeFileSync(rawS, bbs);
  console.log(`→ hippyhayzard [song] · ${BPMH} BPM · ${totBars} bars · ${(totBars * hbar).toFixed(0)}s · earthbound ballad w/ break (instrumental bed — vocal via pop/dance/bin/sing.mjs)`);
  const ffs = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawS,
    "-c:a", "libmp3lame", "-q:a", "2", outS], { stdio: "inherit" });
  try { unlinkSync(rawS); } catch {}
  if (ffs.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
  console.log(`✓ ${outS}`);
  process.exit(0);
}

const SECTIONS = [
  { name: "build",  bars: 4,  mode: "build"  },
  { name: "hippy1", bars: 8,  mode: "hippy"  },
  { name: "hazard", bars: 4,  mode: "hazard" },
  { name: "hippy2", bars: 4,  mode: "hippy"  },
];

const totalBars = SECTIONS.reduce((a, s) => a + s.bars, 0);
const totalSec = totalBars * bar + 0.6;
const N = Math.ceil(totalSec * SR);

const bus = new Float32Array(N);    // melodic (ducked under kick)
const drm = new Float32Array(N);    // drums (not ducked)
const kickTimes = [];

function playHookInto(t0, opts, durMul, gain) {
  let t = t0;
  for (const [midi, s16] of HOOK) {
    const d = sx * s16 * durMul;
    opts.mix({ startSec: t, midi: midi + (opts.oct || 0), durSec: d, gain }, opts);
    t += sx * s16;
  }
  return t;
}

let cursor = 0;
for (const sec of SECTIONS) {
  const t0 = cursor * bar;
  const secSec = sec.bars * bar;

  if (sec.mode === "build") {
    // glock bells alone (nightcore sugar) + riser + soft swung hats
    let tt = t0;
    let phr = 0;
    while (tt < t0 + secSec - 1e-6 && phr < 2) {
      for (const [midi, s16] of HOOK) {
        mixEventSinePower(
          { startSec: tt, midi: midi + 12, durSec: sx * s16 * 0.9, gain: 0.5 },
          bus, { sampleRate: SR, preset: "stab" },
        );
        tt += sx * s16;
      }
      phr++;
    }
    riser(drm, t0, secSec, 0.45);
    for (let b = 0; b < sec.bars; b++) {
      for (let e = 0; e < 8; e++) {                    // swung 8ths
        const sw = e % 2 === 1 ? SWING * sx : 0;
        hat(drm, t0 + b * bar + e * (beat / 2) + sw, 0.18);
      }
    }
    kick(drm, t0 + secSec - sx * 0, 1.0);              // one pickup kick
  }

  else if (sec.mode === "hippy") {
    const reps = Math.round(sec.bars / 2);
    let tt = t0;
    for (let r = 0; r < reps; r++) {
      // hoover stab hook
      playHookInto(tt, { mix: mixEventHoover, sampleRate: SR, preset: "stab" }, 0.95, 0.85);
      // glock doubling an octave up — the nightcore shimmer; and the
      // skrill harmonizing a diatonic third UNDER the hook, in register
      // (the locked-in move from the harmony demo).
      let g = tt;
      for (const [midi, s16] of HOOK) {
        mixEventSinePower(
          { startSec: g, midi: midi + 12, durSec: sx * s16 * 0.6, gain: 0.42 },
          bus, { sampleRate: SR, preset: "stab" },
        );
        mixEventSkrill(
          { startSec: g, midi: diatonic(midi, -2), durSec: sx * s16 * 0.95,
            gain: 0.42, preset: "growl", lfo: "1/8" },
          bus, { sampleRate: SR, bpm: BPM },
        );
        g += sx * s16;
      }
      tt += 2 * bar;
    }
    for (let b = 0; b < sec.bars; b++) {
      const bt = t0 + b * bar;
      for (let q = 0; q < 4; q++) {                    // 4-on-the-floor
        kick(drm, bt + q * beat, 1.0);
        kickTimes.push(bt + q * beat);
        reverseBass(bus, bt + q * beat + beat * 0.5, beat * 0.5, 33, 0.7);
      }
      snare(drm, bt + beat, 0.7);
      snare(drm, bt + beat * 3, 0.7);
      for (let e = 0; e < 16; e++) {                   // swung 16th hats
        const sw = e % 2 === 1 ? SWING * sx : 0;
        if (e % 4 !== 0) hat(drm, bt + e * sx + sw, e % 2 ? 0.16 : 0.26);
      }
    }
  }

  else if (sec.mode === "hazard") {
    // the switch: half-time, relative-minor siren. skrill talking-bass
    // descends F#m; hoover holds the dark siren. glock drops out.
    const minorRoots = [54, 50, 45, 49];               // F#3 D3 A2 C#3-ish
    for (let b = 0; b < sec.bars; b++) {
      const bt = t0 + b * bar;
      mixEventSkrill(
        { startSec: bt, midi: minorRoots[b % minorRoots.length], durSec: bar * 0.95, gain: 0.5,
          preset: b % 2 ? "reese" : "growl", lfo: "1/8" },
        bus, { sampleRate: SR, bpm: BPM },
      );
      mixEventHoover(
        { startSec: bt, midi: minorRoots[b % minorRoots.length] - 12, durSec: bar * 0.95, gain: 0.38 },
        bus, { sampleRate: SR, preset: "hazard" },
      );
      kick(drm, bt, 1.05);                             // half-time
      kickTimes.push(bt);
      kick(drm, bt + beat * 2, 1.05);
      kickTimes.push(bt + beat * 2);
      snare(drm, bt + beat * 2, 0.8);
      for (let e = 0; e < 8; e++) {                    // sparse swung 8ths
        const sw = e % 2 === 1 ? SWING * sx * 1.4 : 0;
        if (e % 2 === 0) hat(drm, bt + e * (beat / 2) + sw, 0.14);
      }
    }
    riser(drm, t0 + secSec - bar, bar, 0.4);           // lift back to hippy
  }

  cursor += sec.bars;
}

// ── sidechain duck (the pump) + master ─────────────────────────────────
const duck = new Float32Array(N).fill(1);
for (const kt of kickTimes) {
  const s0 = Math.floor(kt * SR), len = Math.floor(0.16 * SR);
  for (let i = 0; i < len && s0 + i < N; i++) {
    const w = i / len;
    const d = 0.42 + 0.58 * w;                         // dip to .42, recover
    if (d < duck[s0 + i]) duck[s0 + i] = d;
  }
}

const mix = new Float32Array(N);
for (let i = 0; i < N; i++) {
  const s = bus[i] * duck[i] + drm[i];
  mix[i] = Math.tanh(s * 1.05);                        // gentle glue
}
let peak = 0;
for (let i = 0; i < N; i++) { const a = Math.abs(mix[i]); if (a > peak) peak = a; }
// 0.8 target leaves headroom so lame's inter-sample overshoot stays < 0 dBFS.
if (peak > 0) { const n = 0.8 / peak; for (let i = 0; i < N; i++) mix[i] *= n; }

const outPath = expandHome(flags.out) || resolve(HERE, "../out/hippyhayzard.mp3");
mkdirSync(dirname(outPath), { recursive: true });
const rawPath = `${outPath}.f32.raw`;
const b = Buffer.alloc(N * 4);
for (let i = 0; i < N; i++) b.writeFloatLE(mix[i], i * 4);
writeFileSync(rawPath, b);
console.log(
  `→ hippyhayzard · ${BPM} BPM · ${totalBars} bars · ${totalSec.toFixed(1)}s · ` +
  `build→hippy→HAZARD→hippy`,
);
const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "2", outPath,
], { stdio: "inherit" });
try { unlinkSync(rawPath); } catch {}
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
console.log(`✓ ${outPath}`);
