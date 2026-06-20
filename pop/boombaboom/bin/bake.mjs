#!/usr/bin/env node
// bake.mjs — assemble "Boombaboom": @jeffrey's boom-ba-boom chant, WORLD-
// autotuned + stretched onto the boombaboom.c peak-time techno bed.
//
// Pipeline (mirrors big-pictures/c/bake-short.mjs, the canonical vocal+C-bed
// reference):
//   1. read cells.json (onset-sliced source syllables + detected pitch)
//   2. build the ARRANGEMENT — hook cells kept as-sung (gentle lock to the
//      E/F they already sang), verse cells re-mapped onto the D-minor line,
//      a held cell as the breakdown pad. Placements are (cell, note, at,
//      holdBeats) over the bed's section drops.
//   3. WORLD f0-replace each distinct (cell,note,dur) to its target note
//      (pitchsnap_world.py — formants preserved → still jeffrey), then
//      rubberband --time stretch onto the 140 BPM grid. Cached per combo.
//   4. render the vocal stem (asplit → adelay → amix), forward-bus EQ.
//   5. render the bed (boombaboom.c), mix, club-master → mp3 + wav.
//
// Usage: node boombaboom/bin/bake.mjs [outDir]

import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { existsSync, statSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");                 // pop/boombaboom
const POP  = resolve(ROOT, "..");                 // pop

const outDir = process.argv[2] || `${homedir()}/Documents/Shelf/boombaboom`;
mkdirSync(outDir, { recursive: true });

const ENGINE   = resolve(ROOT, "c/boombaboom");
const ENGINE_C = resolve(ROOT, "c/boombaboom.c");
const BED_WAV   = resolve(outDir, ".boombaboom-bed.wav");
const VOX_WAV   = resolve(outDir, ".boombaboom-vox.wav");
const PRE_WAV   = resolve(outDir, ".boombaboom-pre.wav");
const FINAL_WAV = resolve(outDir, "boombaboom-MASTER.wav");
const FINAL_MP3 = resolve(outDir, "boombaboom.mp3");

const SUNG_DIR = resolve(ROOT, "snapped/sung");
mkdirSync(SUNG_DIR, { recursive: true });

const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);
if (!USE_WORLD) console.warn("[bake] WARNING: WORLD vocoder missing — vocals won't be pitch-locked");

const run = (cmd, a, label, opts = {}) => {
  console.log(`\n[bake] ${label}`);
  const r = spawnSync(cmd, a, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`[bake] FAILED: ${label}`); process.exit(1); }
};

// ── grid (must match boombaboom.c: 140 BPM) ──────────────────────────
const BPM = 140, BEAT = 60 / BPM, BAR = BEAT * 4;
const barT = (bar, beat = 0) => bar * BAR + beat * BEAT;   // seconds

// ── source cells ─────────────────────────────────────────────────────
const cellsDoc = JSON.parse(readFileSync(resolve(ROOT, "cells.json"), "utf8"));
const CELLS = cellsDoc.cells;
const cell = (i) => CELLS[i];

// ── ARRANGEMENT ──────────────────────────────────────────────────────
// Bed section start bars (boombaboom.c form: intro8 build8 grvA16 brkdn8
// grvB16 bigbrk8 grvC16 outro4): build@8 grvA@16 brkdn@32 grvB@40
// bigbrk@56 grvC@64 outro@80.
const placements = [];   // { cellIdx, note, at, holdBeats, slot }
const place = (cellIdx, note, at, holdBeats, slot) =>
  placements.push({ cellIdx, note, at: +at.toFixed(3), holdBeats, slot });

// HOOK — kept as-sung. cells 01/02/03 are the E E F "boom-ba-boom".
// Lock GENTLY to the very notes he sang (E4,E4,F4) so it just steadies.
// One hook unit per 2-bar block: boom(E4 beat0) ba(E4 beat1) booom(F4 beat2, rings).
const HOOK = [
  { cellIdx: 1, note: "E4", beat: 0, hold: 1, slot: "boom1" },
  { cellIdx: 2, note: "E4", beat: 1, hold: 1, slot: "ba"    },
  { cellIdx: 3, note: "F4", beat: 2, hold: 3, slot: "boom2" },
];
const placeHook = (startBar, blocks) => {
  for (let k = 0; k < blocks; k++) {
    const b = startBar + k * 2;
    for (const h of HOOK) place(h.cellIdx, h.note, barT(b, h.beat), h.hold, h.slot);
  }
};
placeHook(16, 8);   // grvA  — first drop  (bars 16–31)
placeHook(40, 8);   // grvB  — second drop (bars 40–55)
placeHook(64, 8);   // grvC  — final drop  (bars 64–79)

// teaser — one held "booom" (F4) foreshadows the hook at the end of build.
place(3, "F4", barT(14, 2), 4, "teaser");

// VERSE — re-mapped. jeffrey's verse-cell phonemes locked onto the rolling
// D-minor answer line (matches boombaboom.np `verse`). Floats over the
// drum-less brkdn (bars 32–40) so the topline reads clearly.
// (cell, target, beat-within-brkdn) — spaced 1.5 beats, 2 bars per phrase.
const VERSE = [
  { c: 5,  n: "D4",  bar: 33, beat: 0,   hold: 2 },   // boom
  { c: 16, n: "C4",  bar: 33, beat: 2,   hold: 1 },   // ba
  { c: 11, n: "A3",  bar: 33, beat: 3,   hold: 1 },   // boom
  { c: 18, n: "A3",  bar: 34, beat: 1,   hold: 1.5 }, // ba
  { c: 7,  n: "F3",  bar: 34, beat: 2.5, hold: 2 },   // boom
  { c: 20, n: "A3",  bar: 35, beat: 1,   hold: 1.5 }, // ba
  { c: 9,  n: "D4",  bar: 36, beat: 0,   hold: 2 },   // boom
  { c: 10, n: "C#4", bar: 36, beat: 2,   hold: 2 },   // ba
  { c: 24, n: "A3",  bar: 37, beat: 0,   hold: 1 },   // boom
  { c: 25, n: "D4",  bar: 37, beat: 1,   hold: 1 },   // ba
  { c: 8,  n: "F4",  bar: 37, beat: 2,   hold: 4 },   // booom (held into grvB build)
];
for (const v of VERSE) place(v.c, v.n, barT(v.bar, v.beat), v.hold, `verse_${v.c}`);

// PAD — a long sustained D4, drenched, holds through the big breakdown
// (bars 56–64) building tension into the final drop. Plus a verse reprise
// phrase rising into grvC.
place(9, "D4", barT(57, 0), 12, "pad");          // held pad
place(4, "C#4", barT(60, 0), 4, "verse_4r");     // tension note (leading tone)
place(8, "F4", barT(62, 2), 4, "verse_8r");      // lift into the final drop

// outro — the hook one last time, sparse, trailing off.
place(1, "E4", barT(80, 0), 1, "boom1");
place(2, "E4", barT(80, 1), 1, "ba");
place(3, "F4", barT(80, 2), 5, "boom2");

console.log(`[bake] ${placements.length} vocal placements, ` +
  `${barT(80, 2).toFixed(1)}s last onset`);

// ── slice + WORLD-lock + stretch each DISTINCT (cell,note,holdBeats) ──
// Source slices already exist in snapped/ (cells.json .file). For each
// distinct target we WORLD-lock to the note then rubberband-stretch to the
// hold duration. Hook locks are gentle (note == as-sung) → minimal shift.
const PRE = 0.015, TAIL = 0.040;
const sungCache = new Map();   // key -> sung wav path
function sungFor(cellIdx, note, holdBeats) {
  const key = `cell${String(cellIdx).padStart(2, "0")}_${note}_h${holdBeats}`;
  if (sungCache.has(key)) return sungCache.get(key);
  const c = cell(cellIdx);
  const srcSlice = resolve(ROOT, c.file);       // clean mono slice from slice-cells.py
  const sung = resolve(SUNG_DIR, `${key}.wav`);
  const fresh = existsSync(sung) && statSync(srcSlice).mtimeMs <= statSync(sung).mtimeMs;
  if (!fresh) {
    let src = srcSlice;
    if (USE_WORLD) {
      const pitched = resolve(SUNG_DIR, `.${key}-pitched.wav`);
      const r = spawnSync(WORLD_PY, [PITCHSNAP, srcSlice, pitched,
        "--notes", note, "--xfade-ms", "30", "--voicing-ramp-ms", "20"],
        { encoding: "utf8" });
      if (r.status === 0) src = pitched;
      else console.warn(`[bake] WORLD failed cell${cellIdx}→${note}: ${(r.stderr||"").trim().split("\n").pop()}`);
    }
    const srcDur = (c.dur_ms / 1000) + PRE + TAIL;
    const targetDur = holdBeats * BEAT;
    const ratio = targetDur / srcDur;
    run("rubberband", ["--time", ratio.toFixed(4), "--formant", "--crisp", "5", src, sung],
        `cell${cellIdx} ${c.note}→${note}  ${srcDur.toFixed(2)}s ×${ratio.toFixed(2)} → ${targetDur.toFixed(2)}s`);
  }
  sungCache.set(key, sung);
  return sung;
}

// resolve the sung stem for each placement (WORLD-lock + stretch, cached).
const TOTAL = 147.0;
for (const pl of placements) pl.file = sungFor(pl.cellIdx, pl.note, pl.holdBeats);

// placements spec for the sample-accurate numpy placer (place-vocal.py).
const VOX_RAW = resolve(outDir, ".boombaboom-voxraw.wav");
const PLACE_JSON = resolve(outDir, ".boombaboom-placements.json");
const placeSpec = {
  sr: 48000,
  total_s: TOTAL,
  placements: placements.map((pl) => ({ file: pl.file, at_s: pl.at, gain: 1.0 })),
};
writeFileSync(PLACE_JSON, JSON.stringify(placeSpec));
// forward-bus EQ applied AFTER placement: clear rumble, short close room,
// drive the quiet source slices up to lead level, presence lift on the consonant.
const VOX_BUS = "highpass=f=120," +
  "aecho=0.85:0.85:60|150:0.12|0.07," +
  "acompressor=threshold=0.05:ratio=4:attack=5:release=160:makeup=4," +
  "equalizer=f=3200:t=q:w=1.2:g=2.2,volume=8dB";

// ── 1 · build engine if stale ────────────────────────────────────────
if (!existsSync(ENGINE) || statSync(ENGINE_C).mtimeMs > statSync(ENGINE).mtimeMs)
  run("cc", ["-O3", "-std=c11", "-Wall", "-Wextra", "-Wno-unused-parameter",
             "-o", ENGINE, ENGINE_C, "-lm"], "build C engine");

// ── 2 · render bed ───────────────────────────────────────────────────
run(ENGINE, ["--out", BED_WAV], "render techno bed (boombaboom.c)");

// ── 3 · render vocal stem ────────────────────────────────────────────
// sample-accurate placement (numpy) → forward-bus EQ (ffmpeg).
run(WORLD_PY, [resolve(HERE, "place-vocal.py"), PLACE_JSON, VOX_RAW],
    `place ${placements.length} vocal stems (sample-accurate)`);
run("ffmpeg", ["-y", "-loglevel", "error", "-i", VOX_RAW, "-af", VOX_BUS,
               "-ar", "48000", "-c:a", "pcm_f32le", VOX_WAV], "vocal forward-bus EQ");

// ── 4 · mix bed + vocal ──────────────────────────────────────────────
// Bed leads the low end; vocal sits on top as the hook. Master fade in/out.
const BED_GAIN = 0.92, VOX_GAIN = 1.5;
const mixComplex = [
  `[0:a]atrim=duration=${TOTAL},volume=${BED_GAIN}[bed]`,
  `[1:a]atrim=duration=${TOTAL},volume=${VOX_GAIN}[vx]`,
  `[bed][vx]amix=inputs=2:duration=longest:dropout_transition=0:normalize=0,` +
    `afade=t=in:st=0:d=1.5,afade=t=out:st=${(TOTAL - 3).toFixed(1)}:d=3[out]`,
].join(";");
run("ffmpeg", ["-y", "-loglevel", "error", "-i", BED_WAV, "-i", VOX_WAV,
               "-filter_complex", mixComplex, "-map", "[out]",
               "-ar", "48000", "-c:a", "pcm_f32le", PRE_WAV], "mix bed + vocal");

// ── 5 · club master (the minitek punchy-and-loud chain) ──────────────
const MASTER_CORE = [
  "highpass=f=28",
  "equalizer=f=50:t=q:w=0.9:g=2.2",        // sub weight under the chill kick
  "equalizer=f=250:t=q:w=1.1:g=-1.4",      // de-mud
  "equalizer=f=2800:t=q:w=1.4:g=0.8",      // presence
  "equalizer=f=10000:t=q:w=0.8:g=1.2",     // crisp hats
  "lowpass=f=17500",
  "acompressor=threshold=-16dB:ratio=2.6:attack=12:release=180:makeup=3.0:knee=6",
  "asoftclip=type=tanh:threshold=0.96",
  "stereotools=slev=1.0",
].join(",");
const MASTER = `${MASTER_CORE},loudnorm=I=-9:TP=-1.0:LRA=9,alimiter=limit=0.97:attack=3:release=60`;
run("ffmpeg", ["-y", "-loglevel", "error", "-i", PRE_WAV, "-af", MASTER,
               "-ar", "44100", "-sample_fmt", "s16", FINAL_WAV], "club master → wav");
run("ffmpeg", ["-y", "-loglevel", "error", "-i", FINAL_WAV, "-c:a", "libmp3lame", "-b:a", "320k",
               FINAL_MP3], "320k mp3");

console.log(`\n[bake] ✓ ${FINAL_MP3}`);
console.log(`[bake] ✓ ${FINAL_WAV}`);
