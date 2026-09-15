#!/usr/bin/env node
// sing-amazing.mjs — the sung jeffrey-pvc lead for "amazing grace".
//
// The cult/wannadash pattern: every word of the spoken jeffrey-pvc take
// goes through cult/bin/sing.py (the Saitou recipe on WORLD — phonemes
// lengthened to fit the note, f0 REPLACED by the score, a modest
// singer's formant) and comes back exactly as long as its notes. Then
// each sung word is dropped at its beat on the 70 BPM grid. The whole
// lead is byte-deterministic from the archived stems + the score.
//
// Sources (the billable stems, restored from the assets archive):
//   system/public/assets/pop/big-pictures/amazing/vocal/amazing-vocal.mp3
//   system/public/assets/pop/big-pictures/amazing-7verse/vocal/…-vocal.mp3
// Both carry ElevenLabs word timestamps. For each of the 26 words we
// take whichever take spoke it LONGER — more vowel for sing.py to hold.
//
// Score: pop/big-pictures/voice-takes/manifest.json (28 notes, from
// amaythingra.np). "amazing" is one word sung over three notes.
//
//   node pop/big-pictures/bin/sing-amazing.mjs [--force] [--dry]
//   → pop/big-pictures/out/amazing-grace/vox.wav (+ vox-receipt.json)

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const ARCH = resolve(REPO, "system/public/assets/pop/big-pictures");
const OUT = resolve(LANE, "out/amazing-grace");
const WORDS_DIR = resolve(OUT, "words");
const SUNG_DIR = resolve(OUT, "sung");
const PY = resolve(REPO, "pop/.venv/bin/python");
const SING = resolve(REPO, "pop/cult/bin/sing.py");
const MANIFEST = resolve(LANE, "voice-takes/manifest.json");
const FORCE = process.argv.includes("--force");
const DRY = process.argv.includes("--dry");

const BPM = 70;
const SPB = 60 / BPM;
const GAP_S = 0.06;          // breath before the next word's consonant
const FLOOR = 65;            // jeffrey's f0 floor (cult/bin/sing.mjs)

const TAKES = {
  v1: {
    audio: `${ARCH}/amazing/vocal/amazing-vocal.mp3`,
    words: `${ARCH}/amazing/vocal/amazing-vocal-words.json`,
  },
  v7: {
    audio: `${ARCH}/amazing-7verse/vocal/amazing-7verse-vocal.mp3`,
    words: `${ARCH}/amazing-7verse/vocal/amazing-7verse-vocal-words.json`,
  },
};

for (const p of [PY, SING, MANIFEST, TAKES.v1.audio, TAKES.v7.audio]) {
  if (!existsSync(p)) { console.error(`✗ missing ${p}`); process.exit(1); }
}
mkdirSync(WORDS_DIR, { recursive: true });
mkdirSync(SUNG_DIR, { recursive: true });

// ── score → words ─────────────────────────────────────────────────────
const manifest = JSON.parse(readFileSync(MANIFEST, "utf8"));
const notes = manifest.notes.map((n, i) => ({ ...n, index: i }));
let beat = 0;
for (const n of notes) { n.startBeat = beat; beat += n.durBeats; }
const TOTAL_BEATS = beat;                                       // 64

// "amazing" = a- -ma- -zing (three notes, one spoken word); the rest 1:1
const words = [];
{
  let i = 0;
  words.push({ text: "amazing", notes: notes.slice(0, 3) });
  i = 3;
  while (i < notes.length) { words.push({ text: notes[i].word, notes: [notes[i]] }); i++; }
}
if (words.length !== 26) { console.error(`✗ expected 26 words, got ${words.length}`); process.exit(1); }

// ── pick the longer spoken take per word ──────────────────────────────
const takeWords = {};
for (const [id, t] of Object.entries(TAKES)) takeWords[id] = JSON.parse(readFileSync(t.words, "utf8"));
if (takeWords.v7.length < 26) { console.error("✗ 7-verse take has too few words"); process.exit(1); }

const clean = (s) => s.toLowerCase().replace(/[^a-z']/g, "");
for (let i = 0; i < 26; i++) {
  const w = words[i];
  const cands = [];
  for (const id of ["v1", "v7"]) {
    const list = takeWords[id];
    const cur = list[i];
    const prev = list[i - 1], next = list[i + 1];
    const text = clean(cur.text);
    // the 7-verse take says "I'm" where verse 1 says "am" — same slot, keep it
    if (text !== clean(w.text) && !(clean(w.text) === "am" && text === "i'm")) {
      console.warn(`  ⚠ ${id} word ${i}: "${cur.text}" ≠ "${w.text}" — skipped`);
      continue;
    }
    const pre = prev ? Math.min(25, (cur.fromMs - prev.toMs) / 2) : 25;
    const post = next ? Math.min(45, (next.fromMs - cur.toMs) / 2) : 60;
    const from = Math.max(0, cur.fromMs - pre);
    const to = cur.toMs + post;
    cands.push({ id, from, to, spoken: cur.toMs - cur.fromMs });
  }
  cands.sort((a, b) => b.spoken - a.spoken);
  w.take = cands[0];
  // "a" (saved A wretch) is a 20 ms schwa in both takes — nothing for
  // WORLD to hold. Borrow the opening "a-" of the 7-verse "Amazing".
  if (w.text === "a" && w.take.spoken < 60) {
    const am = takeWords.v7[0];
    w.take = { id: "v7", from: am.fromMs, to: am.fromMs + 110, spoken: 110, borrowed: "a- of amazing" };
  }
  w.start = w.notes[0].startBeat * SPB;
  w.durBeats = w.notes.reduce((s, n) => s + n.durBeats, 0);
}

// ── slice ─────────────────────────────────────────────────────────────
const slug = (s) => s.replace(/[^a-z]/g, "");
for (let i = 0; i < 26; i++) {
  const w = words[i];
  const nn = String(i).padStart(2, "0");
  w.slice = `${WORDS_DIR}/${nn}-${slug(w.text)}-${w.take.id}${w.take.borrowed ? "-borrowed" : ""}.wav`;
  if (existsSync(w.slice) && !FORCE) continue;
  if (DRY) continue;
  execFileSync("ffmpeg", [
    "-y", "-loglevel", "error",
    "-ss", (w.take.from / 1000).toFixed(3), "-t", ((w.take.to - w.take.from) / 1000).toFixed(3),
    "-i", TAKES[w.take.id].audio,
    "-ac", "1", "-ar", "48000", "-c:a", "pcm_f32le",
    w.slice,
  ]);
}

// ── sing ──────────────────────────────────────────────────────────────
const cachePath = `${SUNG_DIR}/.manifest.json`;
const cache = existsSync(cachePath) ? JSON.parse(readFileSync(cachePath, "utf8")) : {};
let rendered = 0;
for (let i = 0; i < 26; i++) {
  const w = words[i];
  const nn = String(i).padStart(2, "0");
  w.sung = `${SUNG_DIR}/${nn}-${slug(w.text)}.wav`;
  // NOTE:seconds — the word owns its notes' beats, minus a breath at the end
  const spec = w.notes.map((n, k) => {
    const last = k === w.notes.length - 1;
    const sec = n.durBeats * SPB - (last ? GAP_S : 0);
    return `${n.note}:${sec.toFixed(3)}`;
  }).join(",");
  const held = w.durBeats >= 3;
  const args = [
    "-W", "ignore", SING, w.slice, w.sung, "--notes", spec,
    "--f0-floor", String(FLOOR), "--f0-ceil", "520",
    "--vibrato-hz", "5.2",
    "--vibrato-cents", held ? "20" : "9",
    "--vibrato-onset-ms", held ? "420" : "200",
    "--overshoot-cents", held ? "34" : "22",
    "--formant-db", "2.4",
    "--attack-ms", "18", "--release-ms", "150",
    "--deess", "0.05", "--gain", "0.9",
    "--verify",
  ];
  w.spec = spec;
  const key = createHash("sha256")
    .update(args.slice(3).join(" ") + (existsSync(w.slice) ? statSync(w.slice).size : 0))
    .digest("hex").slice(0, 16);
  if (!FORCE && cache[w.sung]?.key === key && existsSync(w.sung)) { w.verify = cache[w.sung].verify; continue; }
  if (DRY) { console.log(`  ${nn} ${w.text.padEnd(8)} ${w.take.id} ${spec}`); continue; }
  process.stdout.write(`  ${nn} ${w.text.padEnd(8)} ${w.take.id} ${spec}\n`);
  const line = execFileSync(PY, args, { encoding: "utf8" }).trim().split("\n").pop();
  console.log(`     ${line}`);
  w.verify = line;
  cache[w.sung] = { key, verify: line };
  rendered++;
}
if (DRY) process.exit(0);
writeFileSync(cachePath, JSON.stringify(cache, null, 2));
console.log(`  sung ${rendered} new, ${26 - rendered} cached`);

// ── place on the grid ─────────────────────────────────────────────────
// Each sung word starts on its first note's beat. Summed in float,
// then peak-normalised to −3 dBFS; the mix decides the level.
const plan = words.map((w) => ({ file: w.sung, at: w.start }));
const planPath = `${OUT}/vox-plan.json`;
writeFileSync(planPath, JSON.stringify({ sr: 48000, total: TOTAL_BEATS * SPB + 1.5, words: plan }));
const VOX = `${OUT}/vox.wav`;
execFileSync(PY, ["-W", "ignore", "-c", `
import json, sys, numpy as np, soundfile as sf
plan = json.load(open(sys.argv[1])); sr = plan["sr"]
n = int(round(plan["total"] * sr)); mix = np.zeros(n)
for w in plan["words"]:
    y, fs = sf.read(w["file"], dtype="float64")
    if y.ndim > 1: y = y.mean(axis=1)
    if fs != sr: raise SystemExit(f"rate {fs} != {sr}: {w['file']}")
    at = int(round(w["at"] * sr)); end = min(n, at + len(y))
    mix[at:end] += y[:end - at]
peak = np.abs(mix).max()
mix *= (10 ** (-3 / 20)) / max(peak, 1e-9)
sf.write(sys.argv[2], mix.astype(np.float32), sr, subtype="FLOAT")
print(f"  placed {len(plan['words'])} words · {n / sr:.2f}s · peak was {peak:.3f}")
`, planPath, VOX], { stdio: "inherit" });

// ── receipt ───────────────────────────────────────────────────────────
const receipt = {
  track: "amazing-grace", bpm: BPM, beatSec: SPB, totalBeats: TOTAL_BEATS,
  engine: "WORLD (pyworld) via pop/cult/bin/sing.py — Saitou 2007 recipe",
  words: words.map((w, i) => ({
    i, text: w.text, start: +w.start.toFixed(4), beats: w.durBeats,
    take: w.take.id, spokenMs: w.take.spoken, notes: w.notes.map((n) => ({
      note: n.note, midi: n.midi, syl: n.word, start: +(n.startBeat * SPB).toFixed(4), beats: n.durBeats,
    })), spec: w.spec, verify: w.verify,
  })),
};
writeFileSync(`${OUT}/vox-receipt.json`, JSON.stringify(receipt, null, 2));
console.log(`✓ ${VOX}\n  receipt → ${OUT}/vox-receipt.json`);
