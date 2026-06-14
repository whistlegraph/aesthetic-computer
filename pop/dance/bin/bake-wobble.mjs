#!/usr/bin/env node
// bake-wobble.mjs — bakes three short dubstep sketches that show off the
// new wobble bass (pop/dance/synths/wobble.mjs):
//
//   wobblewoe   — D minor lament, slow 1/4 triangle wob, supersaw pad
//   wobblebomp  — E minor bounce, hard 1/8 square gate, supersaw stabs
//   wobblrow    — A minor roll, 1/8-triplet sine wob, sinepower lead
//
// Bottom-up compositional (cf. the /pop rule): every voice is an AC
// instrument driven through the buffer-synth bus — the wobble for the
// bass, supersaw/sinepower for the toppings, hand-rolled kick/snare/hat
// for the kit. One mono buffer per track → loudnorm → 320k mp3.
//
// Usage:
//   node pop/dance/bin/bake-wobble.mjs                 # all three → ~/Documents/Shelf/wobble-out
//   node pop/dance/bin/bake-wobble.mjs --only woe      # just wobblewoe
//   node pop/dance/bin/bake-wobble.mjs --out ~/Desktop --no-open

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve } from "node:path";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

import { mixEventWobble } from "../synths/wobble.mjs";
import { mixEventSupersaw } from "../synths/supersaw.mjs";
import { mixEventSinePower } from "../synths/sinepower.mjs";

const SR = 48_000;

// ── tiny deterministic RNG (for hat/snare noise) ───────────────────────
function makeRng(seed) {
  let s = seed >>> 0 || 1;
  return () => {
    s ^= s << 13; s >>>= 0; s ^= s >>> 17; s >>>= 0; s ^= s << 5; s >>>= 0;
    return (s >>> 0) / 0xffffffff;
  };
}

// ── hand-rolled kit (kick/snare/hat) straight into the buffer ──────────
function kick(out, t, gain = 1.0) {
  const start = Math.floor(t * SR);
  const dur = 0.34, decay = 0.30;
  const n = Math.ceil(dur * SR);
  let phase = 0;
  for (let i = 0; i < n; i++) {
    const dst = start + i; if (dst < 0 || dst >= out.length) continue;
    const k = i / SR;
    const env = Math.exp(-k / decay);
    // pitch drop 115 → 46 Hz over the first ~70 ms (the "thump").
    const f = 46 + (115 - 46) * Math.exp(-k / 0.030);
    phase += f / SR; if (phase >= 1) phase -= 1;
    let s = Math.sin(2 * Math.PI * phase);
    s = Math.tanh(s * 1.8); // a touch of click/drive
    out[dst] += s * env * gain;
  }
}

function snare(out, t, gain = 1.0, seed = 1) {
  const start = Math.floor(t * SR);
  const dur = 0.22, decay = 0.13;
  const n = Math.ceil(dur * SR);
  const rng = makeRng(seed * 2654435761);
  let low = 0, band = 0; const f = 2 * Math.sin(Math.PI * 1800 / SR), damp = 0.4;
  let body = 0;
  for (let i = 0; i < n; i++) {
    const dst = start + i; if (dst < 0 || dst >= out.length) continue;
    const k = i / SR;
    const env = Math.exp(-k / decay);
    // band-passed noise (the rattle) ...
    const w = rng() * 2 - 1;
    const high = w - low - damp * band; band += f * high; low += f * band;
    // ... over a short 190 Hz tonal body (the crack).
    body += 190 / SR; if (body >= 1) body -= 1;
    const tone = Math.sin(2 * Math.PI * body) * Math.exp(-k / 0.05);
    out[dst] += (band * 0.9 + tone * 0.5) * env * gain;
  }
}

function hat(out, t, gain = 0.4, seed = 7) {
  const start = Math.floor(t * SR);
  const dur = 0.05, decay = 0.022;
  const n = Math.ceil(dur * SR);
  const rng = makeRng(seed * 40503);
  let prev = 0;
  for (let i = 0; i < n; i++) {
    const dst = start + i; if (dst < 0 || dst >= out.length) continue;
    const env = Math.exp(-(i / SR) / decay);
    const w = rng() * 2 - 1;
    const hp = w - prev; prev = w; // crude one-pole highpass → metallic
    out[dst] += hp * env * gain;
  }
}

// ── track specs ────────────────────────────────────────────────────────
// roots: one MIDI bass root per bar (a 4-bar progression, looped).
// chord: semitone stack added above each root for the topping voice.
const TRACKS = {
  woe: {
    title: "wobblewoe", bpm: 140, reps: 6,
    roots: [38, 36, 34, 33],            // D2 C2 Bb1 A1 — minor lament descent
    chordOffsets: [12, 15, 19],          // root+oct, +min3, +5 (above)
    wobble: { preset: "woe", lfo: "1/4" },
    topping: "pad",                      // supersaw pad chords
  },
  bomp: {
    title: "wobblebomp", bpm: 140, reps: 7,
    roots: [40, 40, 43, 38],            // E2 E2 G2 D2 — Em bounce
    chordOffsets: [12, 15, 19],
    wobble: { preset: "bomp", lfo: "1/8" },
    topping: "stab",                     // supersaw offbeat stabs
  },
  row: {
    title: "wobblrow", bpm: 140, reps: 7,
    roots: [33, 33, 41, 43],            // A1 A1 F2 G2 — Am roll
    chordOffsets: [12, 15, 19],
    wobble: { preset: "row", lfo: "1/8t" },
    topping: "lead",                     // sinepower rolling arp
  },
};

function arrange(spec) {
  const beat = 60 / spec.bpm;
  const bar = beat * 4;
  const introBars = 2;
  const bodyBars = spec.roots.length * spec.reps;
  const totalBars = introBars + bodyBars;
  const tailSec = 1.2;
  const totalSec = totalBars * bar + tailSec;
  const out = new Float32Array(Math.ceil(totalSec * SR));
  const bpm = spec.bpm;

  const rootAt = (barIdx) => spec.roots[((barIdx % spec.roots.length) + spec.roots.length) % spec.roots.length];

  for (let b = 0; b < totalBars; b++) {
    const t0 = b * bar;
    const isIntro = b < introBars;
    const root = rootAt(b - introBars);

    // ── drums (half-time): kick on 1 + ghost, snare on 3, hats on 8ths ──
    if (!isIntro || b === introBars - 1) {
      kick(out, t0 + 0 * beat, 1.0);
      kick(out, t0 + 2.5 * beat, 0.55);            // syncopated ghost
      snare(out, t0 + 2 * beat, 0.85, b + 1);       // backbeat
      for (let h = 0; h < 8; h++) {
        const accent = h % 2 === 0 ? 0.32 : 0.5;    // offbeat lift
        hat(out, t0 + h * 0.5 * beat, accent, b * 8 + h + 3);
      }
    }

    // ── wobble bass: two half-bar notes so it re-attacks mid-bar ──
    const bassGain = isIntro ? 0.7 : 1.0;
    for (let half = 0; half < 2; half++) {
      mixEventWobble(
        { startSec: t0 + half * 2 * beat, midi: root, gain: bassGain, durSec: 2 * beat * 0.98,
          preset: spec.wobble.preset, lfo: spec.wobble.lfo },
        out, { sampleRate: SR, bpm },
      );
    }

    if (isIntro) continue;

    // ── topping ──
    if (spec.topping === "pad") {
      // one slow chord per bar, low in the mix, an octave-ish up.
      for (const off of spec.chordOffsets) {
        mixEventSupersaw(
          { startSec: t0, midi: root + off + 12, gain: 0.16, durSec: bar * 0.98 },
          out, { sampleRate: SR, preset: "pad" },
        );
      }
    } else if (spec.topping === "stab") {
      // supersaw stabs on the offbeats (the "& of 2" and "& of 4").
      for (const beatPos of [1.5, 3.5]) {
        for (const off of spec.chordOffsets) {
          mixEventSupersaw(
            { startSec: t0 + beatPos * beat, midi: root + off + 12, gain: 0.22, durSec: beat * 0.5 },
            out, { sampleRate: SR, preset: "stab" },
          );
        }
      }
    } else if (spec.topping === "lead") {
      // rolling 1/8-triplet arp over the chord tones — answers the wob.
      const tones = [root + 12, root + 15, root + 19, root + 24];
      const tripletSec = beat / 3;
      for (let s = 0; s < 12; s++) {
        const midi = tones[s % tones.length];
        mixEventSinePower(
          { startSec: t0 + s * tripletSec, midi, gain: 0.16, durSec: tripletSec * 0.9 },
          out, { sampleRate: SR, preset: "lead" },
        );
      }
    }
  }

  return out;
}

// ── master + encode ─────────────────────────────────────────────────────
function bakeTrack(key, outDir, openIt) {
  const spec = TRACKS[key];
  console.log(`\n[bake-wobble] ${spec.title} · ${spec.bpm} BPM · wobble:${spec.wobble.preset}`);
  const buf = arrange(spec);

  // peak-normalize before handing to ffmpeg loudnorm.
  let peak = 0;
  for (let i = 0; i < buf.length; i++) { const a = Math.abs(buf[i]); if (a > peak) peak = a; }
  if (peak > 0) { const g = 0.89 / peak; for (let i = 0; i < buf.length; i++) buf[i] *= g; }

  mkdirSync(outDir, { recursive: true });
  const raw = resolve(outDir, `.${spec.title}.f32.raw`);
  const mp3 = resolve(outDir, `${spec.title}.mp3`);
  const bytes = Buffer.alloc(buf.length * 4);
  for (let i = 0; i < buf.length; i++) bytes.writeFloatLE(buf[i], i * 4);
  writeFileSync(raw, bytes);

  // f32 → loudnorm -14 LUFS + gentle limit → 320k mp3 (Spotify-ready glue).
  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", raw,
    "-af", "loudnorm=I=-14:TP=-1.5:LRA=11,alimiter=limit=0.94:attack=8:release=120:level=disabled",
    "-c:a", "libmp3lame", "-b:a", "320k", mp3,
  ], { stdio: "inherit" });
  try { unlinkSync(raw); } catch {}
  if (ff.status !== 0) { console.error(`✗ ffmpeg failed for ${spec.title}`); return null; }

  const dur = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "csv=p=0", mp3], { encoding: "utf8" });
  console.log(`✓ ${mp3}  (${parseFloat(dur.stdout || "0").toFixed(1)}s)`);
  if (openIt) spawnSync("open", ["-a", "QuickTime Player", mp3]);
  return mp3;
}

// ── CLI ──────────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
    else flags[k] = true;
  }
}
function expandHome(p) {
  if (!p || p === true) return undefined;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const outDir = expandHome(flags.out) || resolve(homedir(), "Documents/Shelf/wobble-out");
const openIt = !flags["no-open"];
const aliases = { woe: "woe", wobblewoe: "woe", bomp: "bomp", wobblebomp: "bomp", row: "row", wobblrow: "row" };
const only = flags.only ? aliases[String(flags.only).toLowerCase()] : null;
const keys = only ? [only] : ["woe", "bomp", "row"];

const made = [];
for (const k of keys) { const p = bakeTrack(k, outDir, openIt); if (p) made.push(p); }
console.log(`\n[bake-wobble] done → ${made.length}/${keys.length} track(s) in ${outDir}`);
