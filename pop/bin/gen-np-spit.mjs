#!/usr/bin/env node
// gen-np-spit.mjs — generate a .np score from a big-pictures .txt lyric
// with a tight rap-spit contour (narrow range, root-heavy, occasional
// color tones for accent). Word-level pitches, no syllable splitting.
//
// Spit contour design (C minor pentatonic):
//   - root C3 dominates (~60%)
//   - Eb3 on rhythmic accents (~20%)
//   - F3 on emphasis words (~10%)
//   - G3 on phrase peaks (~5%)
//   - drop to Bb2/C3 at end-of-line for natural fall
//   - hook is one step brighter (C3 → Eb3)
//
// Usage:
//   node bin/gen-np-spit.mjs ../big-pictures/ac24-may-26.txt
//   node bin/gen-np-spit.mjs ../big-pictures/ac24-may-26.txt --out ../big-pictures/ac24-may-26.np
//   node bin/gen-np-spit.mjs ../big-pictures/ac24-may-26.txt --key Cm --hook-shift 3

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, basename, dirname } from "node:path";

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2);
    const n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; }
    else flags[k] = true;
  } else positional.push(a);
}

const lyricPath = positional[0] && resolve(process.cwd(), positional[0]);
if (!lyricPath || !existsSync(lyricPath)) {
  console.error("usage: gen-np-spit.mjs <lyric.txt> [--out path.np] [--key Cm]");
  process.exit(2);
}

const slug = basename(lyricPath).replace(/\.[^.]+$/, "");
const outPath = flags.out
  ? resolve(process.cwd(), flags.out)
  : resolve(dirname(lyricPath), `${slug}.np`);

// ── scale ─────────────────────────────────────────────────────────────
// C minor pentatonic spit set; verse stays low, hook bumps an octave-fifth.
const VERSE_NOTES = {
  root:   "C3",
  color:  "Eb3",
  push:   "F3",
  peak:   "G3",
  drop:   "Bb2",
};
const HOOK_NOTES = {
  root:   "Eb3",
  color:  "G3",
  push:   "F3",
  peak:   "Bb3",
  drop:   "C3",
};

// Deterministic word-position pattern (mod 8) → pitch role.
//   pos 0 1 2 3 4 5 6 7
// role R R C R R P R K  (R=root, C=color, P=push, K=peak)
// Last word of a line → drop (low fall) for natural cadence.
const ROLE_BY_POS = ["root","root","color","root","root","push","root","peak"];

// "Emphasis" words — slot in PUSH regardless of position.
const EMPH = new Set([
  "hellsine","c","port","shipped","release","piece","mastered","ship","ships",
  "fling","back","tint","tints","every","space","headless","clean","ready",
  "distrokid","wav","mp3","music","real","aesthetic","computer","push","enter","news",
  "drop","drop","mantra","spotlight","climax","bang","tom","break","crow",
  "maytrax","prodigy","slab","klokken","marimbaba","helpabeach","chrome-shot",
]);

// ── parse lyric ───────────────────────────────────────────────────────
const raw = readFileSync(lyricPath, "utf8");
const lines = raw.split("\n");

const blocks = []; // { name, lines: [[word, ...], ...] }
let cur = null;
for (const ln of lines) {
  const s = ln.trim();
  if (!s) { continue; }
  const isHeader = /^(hook|verse \d+|chorus|bridge|outro|intro)$/i.test(s);
  if (isHeader) {
    cur = { name: s.toLowerCase(), lines: [] };
    blocks.push(cur);
    continue;
  }
  if (!cur) {
    cur = { name: "verse 1", lines: [] };
    blocks.push(cur);
  }
  // Tokenize: drop em-dashes/commas as separators, keep apostrophes/digits.
  const tokens = s
    .replace(/[—,]/g, " ")
    .split(/\s+/)
    .filter(Boolean)
    .map((w) => w.toLowerCase().replace(/[.;:!?"]/g, ""));
  if (tokens.length) cur.lines.push(tokens);
}

// ── assign pitches ────────────────────────────────────────────────────
function pitchFor(block, lineIdx, posInLine, lineLen, word) {
  const isHook = /hook|chorus|outro/.test(block.name);
  const palette = isHook ? HOOK_NOTES : VERSE_NOTES;

  // last word of line → drop
  if (posInLine === lineLen - 1 && lineLen > 2) return palette.drop;

  // explicit emphasis word
  if (EMPH.has(word)) return palette.push;

  // first word of line in verses sometimes lifts a beat
  if (posInLine === 0 && !isHook && lineIdx % 3 === 0) return palette.color;

  const role = ROLE_BY_POS[(posInLine + lineIdx) % ROLE_BY_POS.length];
  return palette[role];
}

// ── emit ──────────────────────────────────────────────────────────────
const out = [];
out.push(`# big pictures: ${slug} — notepat score (auto-generated, spit contour)`);
out.push(`# notation: NOTE:word — word-level (no syllable splits)`);
out.push(`# key: C minor pentatonic. verse sits C3/Eb3; hook lifts Eb3/G3.`);
out.push(`# gen: pop/bin/gen-np-spit.mjs`);
out.push("");

let verseCounter = 0;
for (const block of blocks) {
  let name = block.name;
  if (name === "hook") name = "hook";
  if (/^verse/.test(name)) { verseCounter += 1; name = `verse ${verseCounter}`; }
  out.push(name);
  block.lines.forEach((words, li) => {
    const tokens = words.map((w, i) => {
      const p = pitchFor(block, li, i, words.length, w);
      return `${p}:${w}`;
    });
    out.push(tokens.join(" "));
  });
  out.push("");
}

writeFileSync(outPath, out.join("\n"));
const wordCount = blocks.reduce((acc, b) => acc + b.lines.reduce((a, l) => a + l.length, 0), 0);
console.log(`✓ ${outPath} (${blocks.length} blocks · ${wordCount} words)`);
