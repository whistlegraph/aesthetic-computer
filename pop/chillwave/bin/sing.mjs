#!/usr/bin/env node
// chillwave/bin/sing.mjs — jeffrey-pvc SUNG vocal stem for helpabeach.
//
//   1. POST the lyric to the LOCAL say endpoint (say-local.mjs) with
//      ElevenLabs /with-timestamps → vocal.mp3 + per-word alignment.
//   2. alignment → words.json
//   3. score-pitch.mjs (WORLD f0 replacement) snaps jeffrey's natural
//      read onto helpabeach.vocal.np → a soft sung performance layer.
//
// Requires say-local.mjs running (node pop/chillwave/bin/say-local.mjs).
//
// Usage: node pop/chillwave/bin/sing.mjs [--force] [--transpose 0]

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
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
const FORCE = flags.force === true;
const TRANSPOSE = String(flags.transpose ?? 0);
const SAY_URL = process.env.SAY_ENDPOINT || "http://127.0.0.1:8899/api/say";

const OUT = `${LANE}/out`;
mkdirSync(OUT, { recursive: true });
const VOCAL_MP3   = `${OUT}/helpabeach-vocal.mp3`;
const WORDS_JSON  = `${OUT}/helpabeach-vocal-words.json`;
const PITCHED_MP3 = `${OUT}/helpabeach-vocal-pitched.mp3`;
const VOCAL_NP    = `${LANE}/helpabeach.vocal.np`;

// Lyric = the content lines of helpabeach.txt (skip the "whisper" tag).
const lyric = readFileSync(`${LANE}/helpabeach.txt`, "utf8")
  .split("\n").map((l) => l.trim())
  .filter((l) => l && l.toLowerCase() !== "whisper")
  .join(" ");
console.log(`▸ lyric: "${lyric}"`);

// ── 1+2. local say (jeffrey-pvc + timestamps) → mp3 + words.json ─────
if (FORCE || !existsSync(VOCAL_MP3) || !existsSync(WORDS_JSON)) {
  console.log(`▸ say-local → ${SAY_URL}`);
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      from: lyric, provider: "jeffrey", voice: "neutral:0",
      stability: 0.6, similarity: 0.92, style: 0.28,
      withTimestamps: true,
    }),
  });
  if (!res.ok) {
    console.error(`✗ say-local ${res.status}: ${(await res.text()).slice(0, 300)}`);
    console.error("  is say-local.mjs running?  node pop/chillwave/bin/say-local.mjs");
    process.exit(1);
  }
  const j = await res.json();
  writeFileSync(VOCAL_MP3, Buffer.from(j.audio, "base64"));
  // chars → words (runs of non-space), [first start, last end]
  const a = j.alignment || {};
  const ch = a.characters || [];
  const st = a.character_start_times_seconds || [];
  const en = a.character_end_times_seconds || [];
  const words = [];
  let i = 0;
  while (i < ch.length) {
    if (/\s/.test(ch[i])) { i++; continue; }
    const from = st[i]; let txt = "", last = en[i];
    while (i < ch.length && !/\s/.test(ch[i])) { txt += ch[i]; last = en[i]; i++; }
    words.push({ text: txt, fromMs: Math.round(from * 1000), toMs: Math.round(last * 1000) });
  }
  writeFileSync(WORDS_JSON, JSON.stringify(words, null, 0));
  console.log(`✓ ${VOCAL_MP3.replace(REPO + "/", "")}  (${words.length} words)`);
} else {
  console.log(`✓ cached vocal + words (use --force to regen)`);
}

// ── 3. score-pitch → sung stem ───────────────────────────────────────
console.log(`▸ score-pitch (WORLD f0 → helpabeach.vocal.np · transpose ${TRANSPOSE})`);
const r = spawnSync("node", [
  `${REPO}/pop/bin/score-pitch.mjs`,
  "--slug", "helpabeach", "--section", "all",
  "--score", VOCAL_NP,
  "--vocal", VOCAL_MP3,
  "--words", WORDS_JSON,
  "--transpose", TRANSPOSE,
  "--vibrato-hz", "5.0",
  "--vibrato-cents", "14",
  "--out", PITCHED_MP3,
], { cwd: `${REPO}/pop`, stdio: ["ignore", "inherit", "inherit"] });
if (r.status !== 0) { console.error("✗ score-pitch failed"); process.exit(1); }
console.log(`✓ pitched → ${PITCHED_MP3.replace(REPO + "/", "")}`);

// ── 4. score-stretch → HOLD notes on the 70 BPM beat grid ────────────
// This is what makes it actually SUNG and beat-aligned: each word is
// rubberband-stretched (formant-preserving, no pitch change) to its
// target beats from helpabeach.vocal.np at the track tempo, so the *5
// phrase-ends sustain on pitch instead of reading as fast speech.
const BPM = String(flags.bpm ?? 84);
const PITCHED_ALIGN = `${OUT}/helpabeach-vocal-pitched-alignment.json`;
const SUNG_MP3 = `${OUT}/helpabeach-vocal-sung.mp3`;
console.log(`▸ score-stretch (rubberband → ${BPM} BPM beat grid · held notes)`);
const r2 = spawnSync("node", [
  `${REPO}/pop/bin/score-stretch.mjs`,
  "--slug", "helpabeach", "--section", "all",
  "--score", VOCAL_NP,
  "--in", PITCHED_MP3,
  "--alignment", PITCHED_ALIGN,
  "--bpm", BPM,
  "--max-stretch", "12.0",
  "--out", SUNG_MP3,
], { cwd: `${REPO}/pop`, stdio: ["ignore", "inherit", "inherit"] });
if (r2.status !== 0) { console.error("✗ score-stretch failed"); process.exit(1); }
console.log(`\n✓ SUNG (beat-aligned) → ${SUNG_MP3.replace(REPO + "/", "")}`);
console.log(`  mix:  node bin/render.mjs --slug helpabeach --vocal-stem ${SUNG_MP3.replace(LANE + "/", "")}`);
