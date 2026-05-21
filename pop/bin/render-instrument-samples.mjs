#!/usr/bin/env node
// render-instrument-samples.mjs — render a short demo mp3 for each pop
// instrument synth. One file per instrument, with preset segments played
// back to back (small gap between them). Used by the pop dashboard page.
//
// Usage:  node pop/bin/render-instrument-samples.mjs
// Output: pop/demos/samples/<id>.mp3  (one per instrument)

import { writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

import { mixEventSupersaw }  from "../dance/synths/supersaw.mjs";
import { mixEventSinePower } from "../dance/synths/sinepower.mjs";
import { mixEventSkrill }    from "../dance/synths/skrill.mjs";
import { mixEventHoover }    from "../hippyhayzard/synths/hoover.mjs";
import { mixEventZitar }     from "../hippyhayzard/synths/zitar.mjs";
import { mixEventMarimba }   from "../marimba/synths/marimba.mjs";

const HERE    = dirname(fileURLToPath(import.meta.url));
const POP     = resolve(HERE, "..");
const OUT_DIR = resolve(POP, "demos/samples");
const SR      = 48_000;
const GAP_SEC = 0.25;   // silence between preset segments

// ── D-minor arpeggio + held chord ─────────────────────────────────────
// [midi, durSec] pairs — one phrase that shows melodic + sustained character.
// MIDI 62=D4, 65=F4, 69=A4, 72=D5, 74=E5
const PHRASE = [
  [62, 0.35], [65, 0.35], [69, 0.35], [72, 0.35],
  [74, 0.40], [72, 0.40], [69, 0.40],
  // held triad
  [62, 1.50], [65, 1.50], [69, 1.50],
];

// A lower register phrase for bass-voiced presets (skrill sub, etc.)
const PHRASE_LOW = [
  [50, 0.40], [53, 0.40], [57, 0.40], [60, 0.40],
  [62, 0.45], [60, 0.45], [57, 0.45],
  [50, 1.60], [53, 1.60], [57, 1.60],
];

// Build a list of {startSec, midi, durSec, gain, preset} events from
// multiple preset segments, laying them out sequentially with a gap.
function buildEvents(presets, phrase = PHRASE, gain = 0.85) {
  const events = [];
  let cursor = 0;
  for (const preset of presets) {
    for (const [midi, durSec] of phrase) {
      events.push({ startSec: cursor, midi, durSec, gain, preset });
      cursor += durSec;
    }
    cursor += GAP_SEC;
  }
  return { events, totalSec: cursor };
}

// Peak-normalize a Float32Array to ~0.9.
function normalize(buf) {
  let peak = 0;
  for (let i = 0; i < buf.length; i++) { const a = Math.abs(buf[i]); if (a > peak) peak = a; }
  if (peak > 0) { const nrm = 0.9 / peak; for (let i = 0; i < buf.length; i++) buf[i] *= nrm; }
}

// Write raw f32le, encode to mp3 via ffmpeg, then delete the raw file.
function encodeToMp3(buf, outPath) {
  const rawPath = `${outPath}.f32.raw`;
  const b = Buffer.alloc(buf.length * 4);
  for (let i = 0; i < buf.length; i++) b.writeFloatLE(buf[i], i * 4);
  writeFileSync(rawPath, b);

  const ff = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "f32le", "-ar", String(SR), "-ac", "1", "-i", rawPath,
    "-c:a", "libmp3lame", "-q:a", "4", outPath,
  ], { stdio: "inherit" });

  try { unlinkSync(rawPath); } catch {}
  if (ff.status !== 0) throw new Error(`ffmpeg failed for ${outPath}`);
}

// Render one instrument: mix all events, normalize, encode.
function renderInstrument({ id, fn, presets, phrase, gain, tail = 2.0, opts = {} }) {
  const { events, totalSec } = buildEvents(presets, phrase, gain);
  const bufLen = Math.ceil((totalSec + tail) * SR);
  const out = new Float32Array(bufLen);

  console.log(`→ ${id}  presets=[${presets.join(", ")}]  events=${events.length}`);
  for (const ev of events) {
    try {
      fn(ev, out, { sampleRate: SR, preset: ev.preset, ...opts });
    } catch (err) {
      console.warn(`  ⚠ ${id} preset=${ev.preset} midi=${ev.midi}: ${err.message}`);
    }
  }

  normalize(out);
  const outPath = resolve(OUT_DIR, `${id}.mp3`);
  encodeToMp3(out, outPath);
  console.log(`✓ ${outPath}`);
}

// ── instrument table ───────────────────────────────────────────────────
const INSTRUMENTS = [
  {
    id: "supersaw",
    fn: mixEventSupersaw,
    presets: ["lead", "pad", "stab"],
    phrase: PHRASE,
  },
  {
    id: "sinepower",
    fn: mixEventSinePower,
    presets: ["lead", "pad", "stab"],
    phrase: PHRASE,
  },
  {
    id: "skrill",
    fn: mixEventSkrill,
    presets: ["growl", "reese", "sub"],
    phrase: PHRASE_LOW,
    opts: { bpm: 138 },
  },
  {
    id: "hoover",
    fn: mixEventHoover,
    presets: ["whoop", "stab", "pad"],
    phrase: PHRASE,
    opts: { bpm: 174 },
  },
  {
    id: "zitar",
    fn: mixEventZitar,
    presets: ["sitar", "lead", "drone"],
    phrase: PHRASE,
  },
  {
    id: "marimba",
    fn: mixEventMarimba,
    presets: ["rosewood", "vibraphone", "glockenspiel"],
    phrase: PHRASE,
  },
];

// ── main ───────────────────────────────────────────────────────────────
mkdirSync(OUT_DIR, { recursive: true });

let ok = 0, fail = 0;
for (const inst of INSTRUMENTS) {
  try {
    renderInstrument(inst);
    ok++;
  } catch (err) {
    console.error(`✗ ${inst.id}: ${err.message}`);
    fail++;
  }
}

console.log(`\ndone — ${ok} rendered, ${fail} failed → ${OUT_DIR}`);
