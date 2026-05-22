#!/usr/bin/env node
// chillwave/bin/harmonize.mjs — render a HARMONY vocal stem against an
// alternate .vocal.np (e.g. helpabeach.vocal-harmony.np = diatonic 3rd
// up). Reuses the cached lead vocal MP3 + words.json from sing.mjs;
// no ElevenLabs call.
//
// Pipeline:
//   1. score-pitch (WORLD f0 → helpabeach.vocal-harmony.np)
//   2. score-stretch (rubberband → 70 BPM beat grid)
//
// Output: out/helpabeach-vocal-<suffix>-sung.mp3 (suffix defaults to "harmony")
//
// Usage:
//   node pop/chillwave/bin/harmonize.mjs                              # 3rd up default
//   node pop/chillwave/bin/harmonize.mjs --score helpabeach.vocal-octave.np --suffix octave
//   node pop/chillwave/bin/harmonize.mjs --transpose 12 --suffix octave-up
//
// Prereq: sing.mjs has run at least once (need cached vocal mp3 + words.json).

import { existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n === undefined || n.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = n; i++; }
}

const SCORE_FILE = flags.score || "helpabeach.vocal-harmony.np";
const SUFFIX     = flags.suffix || "harmony";
const TRANSPOSE  = String(flags.transpose ?? 0);
const BPM        = String(flags.bpm ?? 84);

const VOCAL_MP3   = `${LANE}/out/helpabeach-vocal.mp3`;
const WORDS_JSON  = `${LANE}/out/helpabeach-vocal-words.json`;
if (!existsSync(VOCAL_MP3) || !existsSync(WORDS_JSON)) {
  console.error(`✗ missing cached source — run sing.mjs first`);
  console.error(`  expected: ${VOCAL_MP3}`);
  console.error(`            ${WORDS_JSON}`);
  process.exit(1);
}
const SCORE_PATH = resolve(LANE, SCORE_FILE);
if (!existsSync(SCORE_PATH)) {
  console.error(`✗ score not found: ${SCORE_PATH}`);
  process.exit(1);
}

const PITCHED_MP3   = `${LANE}/out/helpabeach-vocal-${SUFFIX}-pitched.mp3`;
const PITCHED_ALIGN = `${LANE}/out/helpabeach-vocal-${SUFFIX}-pitched-alignment.json`;
const SUNG_MP3      = `${LANE}/out/helpabeach-vocal-${SUFFIX}-sung.mp3`;

console.log(`▸ harmony stem · score=${SCORE_FILE} · suffix=${SUFFIX} · transpose=${TRANSPOSE}`);

console.log(`▸ score-pitch (WORLD f0 → ${SCORE_FILE})`);
const r = spawnSync("node", [
  `${REPO}/pop/bin/score-pitch.mjs`,
  "--slug", "helpabeach", "--section", "all",
  "--score", SCORE_PATH,
  "--vocal", VOCAL_MP3,
  "--words", WORDS_JSON,
  "--transpose", TRANSPOSE,
  "--vibrato-hz", "5.0",
  "--vibrato-cents", "14",
  "--out", PITCHED_MP3,
], { cwd: `${REPO}/pop`, stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ score-pitch failed"); process.exit(1); }

console.log(`▸ score-stretch (rubberband → ${BPM} BPM beat grid)`);
const r2 = spawnSync("node", [
  `${REPO}/pop/bin/score-stretch.mjs`,
  "--slug", "helpabeach", "--section", "all",
  "--score", SCORE_PATH,
  "--in", PITCHED_MP3,
  "--alignment", PITCHED_ALIGN,
  "--bpm", BPM,
  "--max-stretch", "12.0",
  "--out", SUNG_MP3,
], { cwd: `${REPO}/pop`, stdio: ["ignore", "inherit", "inherit"] });
if (r2.status !== 0) { console.error("✗ score-stretch failed"); process.exit(1); }

console.log(`\n✓ ${SUFFIX} stem → ${SUNG_MP3.replace(REPO + "/", "")}`);
console.log(`  mix:  node bin/render.mjs --slug helpabeach \\`);
console.log(`        --vocal-stem out/helpabeach-vocal-sung.mp3 \\`);
console.log(`        --vocal-harmony out/helpabeach-vocal-${SUFFIX}-sung.mp3`);
