#!/usr/bin/env node
// tune-voice.mjs — WORLD-autotune jeffrey's vowel takes for the harmony
// engine. Each take is f0-REPLACED (pitchsnap_world.py — formants stay,
// pitch curve becomes the steady target) onto a flat B2, so the renderer's
// constant-ratio resampling lands every choir voice EXACTLY on the score's
// chord tones — genuinely locked, not transposed wobble.
//
// A whisper of late-onset vibrato (6 cents @ 4.8 Hz after 0.8 s) keeps the
// choir human without breaking the lock.
//
// Reads:  wave-wizard/samples/momabobasheep/takes/*.wav
// Writes: pop/momboba/voice/locked/<vowel>.wav
//         pop/momboba/voice/manifest.json  (midi locked at exactly 47.0)
//
// Run:  node pop/momboba/bin/tune-voice.mjs

import { writeFileSync, readdirSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const TAKES = resolve(REPO, "wave-wizard/samples/momabobasheep/takes");
const OUT_DIR = resolve(HERE, "../voice");
const LOCKED = resolve(OUT_DIR, "locked");
mkdirSync(LOCKED, { recursive: true });

const VOWELS = ["oo", "oh", "ah", "eh", "mm"]; // spec.json take order
const NOTE = "B2", NOTE_MIDI = 47.0;           // centre of his natural register
const PY = resolve(REPO, "pop/.venv/bin/python");
const SNAP = resolve(REPO, "pop/bin/pitchsnap_world.py");

const files = existsSync(TAKES) ? readdirSync(TAKES).filter((f) => f.endsWith(".wav")).sort() : [];
if (!files.length) { console.error(`✗ no takes in ${TAKES}`); process.exit(1); }

const rows = [];
files.forEach((f, i) => {
  const vowel = VOWELS[i] ?? `take${i}`;
  const out = resolve(LOCKED, `${vowel}.wav`);
  const flat = resolve(LOCKED, `${vowel}-flat.wav`);
  // vibrato set — tenor/upper choir voices (reads as a singer)
  execFileSync(PY, [SNAP, resolve(TAKES, f), out,
    "--notes", NOTE,
    "--xfade-ms", "30", "--voicing-ramp-ms", "20",
    "--vibrato-hz", "4.8", "--vibrato-cents", "6", "--vibrato-onset-ms", "800",
  ], { stdio: ["ignore", "inherit", "inherit"] });
  // dead-flat set — LOW choir voices: any vibrato down there beats against
  // the walking bass / drone at the same pitch (slow phase churn)
  execFileSync(PY, [SNAP, resolve(TAKES, f), flat,
    "--notes", NOTE,
    "--xfade-ms", "30", "--voicing-ramp-ms", "20",
    "--vibrato-cents", "0",
  ], { stdio: ["ignore", "inherit", "inherit"] });
  rows.push({ file: out.replace(REPO + "/", ""), flatFile: flat.replace(REPO + "/", ""), vowel, midi: NOTE_MIDI, locked: NOTE });
  console.log(`  ${vowel} → locked ${NOTE} (+flat)`);
});

writeFileSync(resolve(OUT_DIR, "manifest.json"), JSON.stringify(rows, null, 2) + "\n");
console.log(`✓ ${rows.length} vowels WORLD-locked to ${NOTE} → voice/manifest.json`);
