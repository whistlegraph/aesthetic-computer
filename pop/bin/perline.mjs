#!/usr/bin/env node
// perline.mjs — generate per-line ElevenLabs takes with score-aware
// final-syllable elongation.
//
// Why: phrase-end held syllables in the score (e.g. row.np `dream*4`,
// amazing.np `me*5` / `sound*5` / `found*5` / `see*5`) need to actually
// SUSTAIN in the source vocal so pitchsnap doesn't have to do a 6×
// rubberband stretch (which exceeds WORLD's clean range, and confuses
// whisper into merging the held vowel with the next line's onset —
// "me I" boundary collapsed to zero gap, etc.).
//
// What: read the .np score + .txt lyric file, identify each line's
// final word's score weight, repeat the final vowel cluster
// `(weight - 1)` times to coax ElevenLabs into a longer sustain, then
// generate one say.mjs call per line with a long inter-line silence.
// Concat into `${slug}-perline.mp3`.
//
// Usage:
//   node bin/perline.mjs --slug amazing
//     [--bpm 70] [--gap 1.5]          inter-line silence in seconds
//     [--style 0.6] [--stability 0.7] [--speed 0.7]

import { execSync, spawnSync } from "node:child_process";
import { writeFileSync, readFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { tmpdir } from "node:os";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}

const SLUG       = flags.slug || "amazing";
const POP        = "/Users/jas/aesthetic-computer/pop";
const SCORE_PATH = `${POP}/big-pictures/${SLUG}.np`;
const LYRIC_PATH = `${POP}/big-pictures/${SLUG}.txt`;
const SECTION    = (flags.section || "verse 1").toLowerCase();
// Default to v1 settings: 0.3s gap, no elongation. Elongation looked
// promising on paper but ElevenLabs ignores doubled vowels, so it
// only inflated the source duration without sustaining held notes —
// regression in audible output. Opt in with --elongate to retry.
const GAP_S      = Number(flags.gap ?? 0.3);
const STYLE      = flags.style     ?? "0.55";
const STABILITY  = flags.stability ?? "0.7";
const SPEED      = flags.speed     ?? "0.7";
const ELONGATE   = flags.elongate === true || flags.elongate === "true";
const OUT_PATH   = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}-perline.mp3`;

if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }
if (!existsSync(LYRIC_PATH)) { console.error(`✗ lyrics missing: ${LYRIC_PATH}`); process.exit(1); }

// ── Parse the score: collect (line-index, word-index, weight) tuples ──
// Score lines map 1:1 to lyric lines under the same section header.
const scoreLines = readFileSync(SCORE_PATH, "utf8").split("\n");
const scoreSecStart = scoreLines.findIndex(
  (l) => l.trim().toLowerCase() === SECTION,
);
if (scoreSecStart < 0) { console.error(`✗ section '${SECTION}' missing in score`); process.exit(1); }

// scoreWords[lineIdx] = [{ raw, weight }, ...] — one per token.
const scoreWords = [];
for (let i = scoreSecStart + 1; i < scoreLines.length; i++) {
  const l = scoreLines[i].trim();
  if (!l) break;
  if (l.startsWith("#")) continue;
  if (/^[a-z]+ \d/i.test(l)) break;
  const lineEntries = [];
  for (const tok of l.split(/\s+/)) {
    const m = tok.match(/^[A-Ga-g][#b]?-?\d:(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
    if (!m) continue;
    lineEntries.push({ raw: m[1], weight: Number(m[2] ?? 1) });
  }
  scoreWords.push(lineEntries);
}
console.log(`→ score has ${scoreWords.length} line(s) under '${SECTION}'`);

// ── Parse lyric file: per-section line array ──────────────────────────
const lyricText = readFileSync(LYRIC_PATH, "utf8");
const lyricLines = lyricText.split("\n");
const lyricSecStart = lyricLines.findIndex(
  (l) => l.trim().toLowerCase() === SECTION,
);
if (lyricSecStart < 0) { console.error(`✗ section '${SECTION}' missing in lyrics`); process.exit(1); }
const lines = [];
for (let i = lyricSecStart + 1; i < lyricLines.length; i++) {
  const t = lyricLines[i].trim();
  if (!t) break;
  if (/^[a-z]+ \d/i.test(t)) break;
  lines.push(t);
}
console.log(`→ ${lines.length} lyric line(s)`);

if (lines.length !== scoreWords.length) {
  console.warn(`⚠ score lines (${scoreWords.length}) != lyric lines (${lines.length}) — final-word elongation may misalign`);
}

// ── Per-word elongation by score weight ───────────────────────────────
// Any score syllable with weight >= 3 gets its final vowel cluster
// repeated (weight - 2) times so ElevenLabs actually sustains it.
// Phrase-end held syllables AND mid-line held notes both qualify
// (e.g. amazing.np line 3 has D4:now*3 which is mid-line — without
// elongation whisper merges 'now' into adjacent words and pitchsnap
// has to do a 20× stretch).
//
// Also inserts a comma + space ", " between any two consecutive *N>=3
// words on the same line so whisper doesn't merge them across vowel
// boundaries.
function elongateOneWord(word, weight) {
  if (weight < 3) return word;
  const extras = Math.max(1, Math.round(weight - 2));
  const trailMatch = word.match(/^([a-zA-Z']+)([.,!?;:]*)$/);
  if (!trailMatch) return word;
  const w = trailMatch[1], trail = trailMatch[2];
  const vowels = [...w.matchAll(/[aeiouy]+/gi)];
  if (vowels.length === 0) return word;
  const v = vowels[vowels.length - 1];
  const vEnd = v.index + v[0].length;
  return w.slice(0, vEnd) + v[0].slice(-1).repeat(extras) + w.slice(vEnd) + trail;
}

function buildLine(lyricLine, scoreLine) {
  // Each scoreLine entry maps 1:1 to a SYLLABLE, but lyricLine has
  // WORDS. Group consecutive syllables that share a hyphen-marker
  // chain into one word, then assign that word the MAX weight of its
  // syllable group (so multi-syllable words like 'amazing'/'merrily'
  // get their held syllable's weight).
  const groups = [];
  let cur = null;
  for (const e of scoreLine) {
    const r = e.raw;
    const startsContinuation = r.startsWith("-");
    if (cur && startsContinuation) {
      cur.weight = Math.max(cur.weight, e.weight);
      cur.syls.push(r);
    } else {
      cur = { syls: [r], weight: e.weight };
      groups.push(cur);
    }
  }
  // Group weight = MAX of its constituent syllable weights.
  const groupWeights = groups.map(g => g.weight);

  const tokens = lyricLine.split(/\s+/);
  if (tokens.length !== groupWeights.length) {
    console.warn(`  ⚠ word count ${tokens.length} != score-group count ${groupWeights.length} for "${lyricLine}"`);
  }
  const out = [];
  for (let j = 0; j < tokens.length; j++) {
    const w = groupWeights[j] ?? 1;
    const elongated = elongateOneWord(tokens[j], w);
    // If this word AND the next are both *3+ holds, force a comma so
    // whisper doesn't merge them across the sustained-vowel boundary.
    const nextW = groupWeights[j + 1] ?? 1;
    const trail = (w >= 3 && nextW >= 3) ? "," : "";
    out.push(elongated + trail);
  }
  return out.join(" ");
}

const elongatedLines = lines.map((line, i) => {
  if (!ELONGATE) {
    console.log(`  L${i + 1}    "${line}"`);
    return line;
  }
  const sl = scoreWords[i] || [];
  const out = buildLine(line, sl);
  const changed = out !== line;
  console.log(`  L${i + 1} ${changed ? "→ " : "  "} "${out}"`);
  return out;
});

// ── Generate one ElevenLabs take per line via say.mjs ────────────────
// Per-line style/stability variation gives different prosody per phrase
// (the v1 "much better" recipe).
const PER_LINE_STYLE     = ["0.55", "0.45", "0.65", "0.50"];
const PER_LINE_STABILITY = ["0.70", "0.75", "0.65", "0.70"];

const tmp = mkdtempSync(`${tmpdir()}/perline-${SLUG}-`);
const lineMp3s = [];
for (let i = 0; i < elongatedLines.length; i++) {
  const idx = i + 1;
  const lineFile = `${tmp}/${SLUG}-l${idx}.txt`;
  writeFileSync(lineFile, `verse 1\n${elongatedLines[i]}\n`);
  const lineOut = `${POP}/big-pictures/out/${SLUG}-l${idx}.mp3`;
  const lstyle = flags.style != null ? STYLE : (PER_LINE_STYLE[i] ?? STYLE);
  const lstab  = flags.stability != null ? STABILITY : (PER_LINE_STABILITY[i] ?? STABILITY);
  console.log(`→ say.mjs L${idx}: speed=${SPEED} style=${lstyle} stability=${lstab}`);
  const r = spawnSync("node", [
    `${POP}/bin/say.mjs`, lineFile,
    "--speed",     String(SPEED),
    "--style",     String(lstyle),
    "--stability", String(lstab),
    "--out",       lineOut,
    "--force",
  ], { stdio: "inherit" });
  if (r.status !== 0) { console.error(`✗ say.mjs failed on L${idx}`); process.exit(1); }
  lineMp3s.push(lineOut);
}

// ── Concat with ${GAP_S}s silence between lines ───────────────────────
const silWav = `${tmp}/silence.mp3`;
spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "lavfi", "-t", String(GAP_S), "-i", "anullsrc=r=44100:cl=mono",
  silWav,
], { stdio: "inherit" });

const concatTxt = `${tmp}/concat.txt`;
const concatLines = [];
for (let i = 0; i < lineMp3s.length; i++) {
  concatLines.push(`file '${lineMp3s[i]}'`);
  if (i < lineMp3s.length - 1) concatLines.push(`file '${silWav}'`);
}
writeFileSync(concatTxt, concatLines.join("\n") + "\n");

const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "concat", "-safe", "0", "-i", concatTxt,
  "-c:a", "libmp3lame", "-q:a", "4",
  OUT_PATH,
], { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ concat failed"); process.exit(1); }

rmSync(tmp, { recursive: true, force: true });
const dur = execSync(`ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ${OUT_PATH}`).toString().trim();
console.log(`✓ ${OUT_PATH} (${Number(dur).toFixed(2)}s, ${elongatedLines.length} lines, ${GAP_S}s gaps)`);
