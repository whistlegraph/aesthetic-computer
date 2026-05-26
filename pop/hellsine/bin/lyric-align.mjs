#!/usr/bin/env node
// lyric-align.mjs — interactive lyric ↔ note alignment checker.
//
// Pass a lyric string (one word per syllable, dashes mark internal
// syllable splits) and an optional phrasing name, and the tool prints
// a side-by-side table of THEME notes ↔ syllables so you can EYEBALL
// whether the mapping reads correctly before re-rendering.
//
// Usage:
//   node bin/lyric-align.mjs "i hope you get all the mo-ney you want"
//   node bin/lyric-align.mjs --check that      # validates the existing "that" phrasing
//   node bin/lyric-align.mjs --check you       # validates the existing "you" phrasing
//   node bin/lyric-align.mjs --check we        # validates the existing "we" phrasing
//
// The tool also shows whether the syllable count matches the
// THEME-antecedent slot count (11 slots, beats 1+1.5+0.5+1+1+1+1+1+2+1+1
// = 12 beats), and flags any melisma (a word stretched across multiple
// notes — encoded by inserting a "·" syllable that sustains the prior).

import { readFileSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT_MEL = 62;                                       // D4
const BPM = 182;
const SPB = 60 / BPM;

// Antecedent of THE THEME — 11 notes the mantra sings to. Each entry:
// [offsetFromRootInSemis, beatsHeld, niceMidiLabel].
const THEME_ANTECEDENT = [
  [-5, 1.0],   // A3
  [ 0, 1.5],   // D4
  [ 3, 0.5],   // F4
  [ 7, 1.0],   // A4
  [ 7, 1.0],   // A4
  [ 8, 1.0],   // Bb4
  [ 7, 1.0],   // A4
  [ 5, 1.0],   // G4
  [ 3, 2.0],   // F4 sustain
  [ 2, 1.0],   // E4
  [ 0, 1.0],   // D4
];
const TOTAL_NOTES = THEME_ANTECEDENT.length;
const TOTAL_BEATS = THEME_ANTECEDENT.reduce((a, [, b]) => a + b, 0);

const NOTE_NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
function midiLabel(m) {
  const n = NOTE_NAMES[m % 12];
  const oct = Math.floor(m / 12) - 1;
  return `${n}${oct}`;
}

// Parse a lyric string into syllables. Hyphens split a word into
// multiple syllables ("mo-ney" → ["mo","ney"]). Underscore is a
// melisma marker ("hope_" or "_") meaning "sustain the previous
// syllable across this note".
function parseLyric(line) {
  const tokens = line.trim().split(/\s+/);
  const syls = [];
  for (const tok of tokens) {
    const parts = tok.split("-");
    for (const p of parts) {
      if (p === "_" || p === "·") syls.push({ syl: "·", sustain: true });
      else if (p.endsWith("_")) {
        syls.push({ syl: p.slice(0, -1), sustain: false });
        // The trailing underscore marks "this word sustains into the
        // next note" — recorded as an implicit "·" follower.
        syls.push({ syl: "·", sustain: true });
      } else {
        syls.push({ syl: p, sustain: false });
      }
    }
  }
  return syls;
}

// Lay the syllables across the antecedent notes. If the syllable count
// is less than the note count, return a warning. If more, the trailing
// syllables get tagged as overflow.
function align(syllables, noteCount = TOTAL_NOTES) {
  const rows = [];
  for (let i = 0; i < Math.max(noteCount, syllables.length); i++) {
    rows.push({
      noteIdx: i < noteCount ? i : null,
      syl: syllables[i]?.syl ?? "—",
      sustain: !!syllables[i]?.sustain,
    });
  }
  return rows;
}

// ── built-in phrasing checks ──────────────────────────────────────────
const BUILTIN_PHRASINGS = {
  // 11-slot phrasings (current "that" baseline) — variants substituted at print time.
  that: "i hope that we get all the mo-ney we want",
  // 10-slot phrasing — "hope" sustains an extra slot (melisma) so 10
  // syllables map to 11 notes.
  you:  "i hope_ you get all the mo-ney you want",
  we:   "i hope_ we get all the mo-ney we want",
};

function printAlignment(label, lyric, variant = "money") {
  const syls = parseLyric(lyric.replace(/mo-?ney|mo-?ne?ys?/i, "mo-ney"));
  // For display, substitute the actual variant into the [mo-ney] slot
  const variantParts = variant === "money" ? ["mo", "ney"]
                     : variant === "honey" ? ["ho", "ney"]
                     : variant === "bunnies" ? ["bun", "nies"]
                     : [variant, "_"];
  let vIdx = 0;
  for (const s of syls) {
    if (s.syl === "mo") { s.syl = variantParts[0]; }
    else if (s.syl === "ney") { s.syl = variantParts[1] || "_"; }
  }
  const rows = align(syls, TOTAL_NOTES);
  const cyc = ["Dm","Dm","Bb","Bb","F","F","C","C"];   // statement chord cycle (for color)
  console.log(`\n=== ${label} · variant="${variant}" ===`);
  console.log(`syllables=${syls.length}  notes=${TOTAL_NOTES}  total-beats=${TOTAL_BEATS}`);
  if (syls.length !== TOTAL_NOTES) {
    console.log(`⚠  syllable count (${syls.length}) ≠ note count (${TOTAL_NOTES})`);
  }
  console.log("");
  console.log("  i  note   midi  beats  beat-cursor  word");
  console.log("  ─  ─────  ────  ─────  ───────────  ─────");
  let cursor = 0;
  for (let i = 0; i < rows.length; i++) {
    const r = rows[i];
    if (r.noteIdx === null) {
      console.log(`  -  (off)  --    --     --           ${r.syl}  (overflow — no note for this syllable)`);
      continue;
    }
    const [off, beats] = THEME_ANTECEDENT[r.noteIdx];
    const m = ROOT_MEL + off;
    const cursorStr = cursor.toFixed(1).padStart(4) + "→" + (cursor + beats).toFixed(1).padStart(4);
    const marker = r.sustain ? "↳ (sustain)" : r.syl;
    console.log(`  ${String(i).padStart(2)} ${String(r.noteIdx).padStart(2)}   ${midiLabel(m).padEnd(4)}  ${beats.toFixed(1)}    ${cursorStr}  ${marker}`);
    cursor += beats;
  }
  console.log("");
}

// ── CLI ───────────────────────────────────────────────────────────────
const argv = process.argv.slice(2);
const checkFlag = argv.indexOf("--check");
if (checkFlag >= 0) {
  const which = argv[checkFlag + 1];
  const lyric = BUILTIN_PHRASINGS[which];
  if (!lyric) { console.error(`unknown phrasing: ${which}`); process.exit(1); }
  for (const variant of ["money", "honey", "bunnies"]) {
    printAlignment(`built-in "${which}"`, lyric, variant);
  }
} else if (argv.length >= 1) {
  const lyric = argv.join(" ");
  for (const variant of ["money", "honey", "bunnies"]) {
    printAlignment(`custom lyric`, lyric, variant);
  }
} else {
  console.log("lyric-align — check word↔note mapping for the mantra.\n");
  console.log("usage:");
  console.log("  node bin/lyric-align.mjs --check that|you|we");
  console.log("  node bin/lyric-align.mjs 'i hope you get all the mo-ney you want'\n");
  console.log("conventions:");
  console.log("  word boundaries: spaces");
  console.log("  internal syllable split: hyphen     (mo-ney = 2 syllables)");
  console.log("  melisma sustain: trailing _ or ·   (hope_ = hope holds an extra note)");
  console.log("");
  for (const p of Object.keys(BUILTIN_PHRASINGS)) {
    printAlignment(`built-in "${p}"`, BUILTIN_PHRASINGS[p], "money");
  }
}
