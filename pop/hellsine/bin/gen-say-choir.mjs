#!/usr/bin/env node
// gen-say-choir.mjs — render the hellsine mantra through Apple `say`
// novelty/singing voices to seed a glitch-choir layer. Each voice ×
// variant (money/honey/bunnies) becomes a sample on disk; the C engine
// loads them and applies stutter/reverse/bit-crush at render time.
//
// Voices (Apple TTS novelty + musical):
//   Cellos     — sings with cello-like tones
//   Bells      — sings each syllable as a bell
//   Good News  — chipper sung speech
//   Bad News   — gloomy sung speech
//   Whisper    — whispered, percussive consonants
//   Bahh       — sheep-bleat sustained tones
//   Trinoids   — robot alien
//   Zarvox     — deep robot
//   Organ      — pipe organ sung tones
//
// Output: pop/hellsine/samples/say-choir/<voice>-<variant>.wav
//         48 kHz mono float WAV, ready for try_load_sample.

import { spawnSync } from "node:child_process";
import { mkdirSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "../samples/say-choir");
mkdirSync(OUT, { recursive: true });

const VOICES = [
  "Cellos", "Bells", "Good News", "Bad News",
  "Whisper", "Bahh", "Trinoids", "Zarvox",
  "Organ", "Boing", "Wobble", "Bubbles",
];
const VARIANTS = ["money", "honey", "bunnies"];
// 12-word mantra (extended 2026-05-26 to match THEME note density).
const MANTRA = (v) => `i hope that we get all of the ${v} that we want`;
// Slow rate so the singing voices have time to pitch each syllable.
const RATE = 110;

const run = (cmd, args, label) => {
  const r = spawnSync(cmd, args, { stdio: "pipe" });
  if (r.status !== 0) {
    console.error(`[gen-choir] FAILED: ${label}\n${r.stderr.toString().slice(-200)}`);
    return false;
  }
  return true;
};

let rendered = 0, skipped = 0;
for (const voice of VOICES) {
  for (const variant of VARIANTS) {
    const slug = voice.toLowerCase().replace(/\s+/g, "-");
    const aiff = `/tmp/choir-${slug}-${variant}.aiff`;
    const wav  = `${OUT}/${slug}-${variant}.wav`;
    // Always re-render so mantra changes (10 → 12 words) actually take.
    if (!run("say", ["-v", voice, "-r", String(RATE), "-o", aiff,
                     MANTRA(variant)],
             `say ${voice} ${variant}`)) continue;
    if (!run("ffmpeg", ["-y", "-i", aiff, "-ar", "48000", "-ac", "1",
                        "-c:a", "pcm_f32le", wav],
             `convert ${slug}-${variant}`)) continue;
    rendered++;
    console.log(`[gen-choir] ${voice.padEnd(11)} · ${variant.padEnd(7)} → ${slug}-${variant}.wav`);
  }
}
console.log(`\n[gen-choir] rendered=${rendered}  skipped=${skipped}  voices=${VOICES.length}  variants=${VARIANTS.length}`);
console.log(`[gen-choir] dir: ${OUT}`);
