#!/usr/bin/env node
// best-of-takes.mjs — distributed source generation for pitchsnap.
//
// Why: a single ElevenLabs take of the whole verse compresses each
// word's natural duration. A per-word take strips natural prosody.
// Per-line takes have the prosody but force a fixed source length.
//
// Solution: generate MULTIPLE overlapping phrase takes, each giving
// ElevenLabs different context, then for each canonical lyric word
// pick the take where that word has the LONGEST natural source
// duration. Splice the picked occurrences together with crossfade.
//
// The user's framing: "overlapping phrases of the lyrics, then choose
// best word — a distributed method".
//
// Strategy:
//   • Generate 3 full-verse takes with varied style/stability seeds,
//     plus 2 phrase-window takes (lines 1+2, lines 3+4) with longer
//     speed (when allowed by ElevenLabs clamp).
//   • Whisper-align each, get per-word boundaries.
//   • For each canonical lyric word position, pick the take whose
//     occurrence of that word has the longest natural duration.
//   • Extract each picked segment via ffmpeg.
//   • Concat with 200ms crossfade between segments.
//
// Output: ${slug}-bestof.mp3 + ${slug}-bestof-words.json (synthesized).

import { execSync, spawnSync } from "node:child_process";
import { writeFileSync, readFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
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
const LYRIC_PATH = `${POP}/big-pictures/${SLUG}.txt`;
const SECTION    = (flags.section || "verse 1").toLowerCase();
const N_TAKES    = Number(flags["n-takes"] ?? 5);
const CROSSFADE_MS = Number(flags["crossfade-ms"] ?? 80);
const OUT_PATH   = flags.out || `${POP}/big-pictures/out/${SLUG}-bestof.mp3`;

if (!existsSync(LYRIC_PATH)) { console.error(`✗ lyrics missing: ${LYRIC_PATH}`); process.exit(1); }

// ── Parse lyric into a flat word array ───────────────────────────────
const lyricLines = readFileSync(LYRIC_PATH, "utf8").split("\n");
const lStart = lyricLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
if (lStart < 0) { console.error(`✗ section '${SECTION}' missing in lyrics`); process.exit(1); }
const lines = [];
for (let i = lStart + 1; i < lyricLines.length; i++) {
  const t = lyricLines[i].trim();
  if (!t) break;
  if (/^[a-z]+ \d/i.test(t)) break;
  lines.push(t);
}
const canonicalWords = lines.flatMap(l => l.split(/\s+/).map(w => w.toLowerCase().replace(/[^a-z']/g, "")));
console.log(`→ canonical: ${canonicalWords.length} words across ${lines.length} lines`);

// ── Define takes: 3 full verses + 2 phrase windows ───────────────────
// Each take has a unique style/stability seed for prosodic variation.
const takes = [
  { name: "full-A",  text: lines.join(", "),         style: "0.55", stability: "0.70", lineRange: [0, lines.length] },
  { name: "full-B",  text: lines.join(", "),         style: "0.65", stability: "0.65", lineRange: [0, lines.length] },
  { name: "full-C",  text: lines.join(", "),         style: "0.45", stability: "0.75", lineRange: [0, lines.length] },
];
// Phrase windows for first / second halves with overlap on middle line.
if (lines.length >= 3) {
  takes.push({
    name: "win-12", text: lines.slice(0, 2).join(", "),
    style: "0.55", stability: "0.70", lineRange: [0, 2],
  });
  takes.push({
    name: "win-23", text: lines.slice(1, 3).join(", "),
    style: "0.60", stability: "0.70", lineRange: [1, 3],
  });
  if (lines.length >= 4) {
    takes.push({
      name: "win-34", text: lines.slice(2, 4).join(", "),
      style: "0.55", stability: "0.70", lineRange: [2, 4],
    });
  }
}
takes.length = Math.min(takes.length, N_TAKES);

// ── Generate each take via say.mjs ───────────────────────────────────
const tmp = mkdtempSync(`${tmpdir()}/bestof-${SLUG}-`);
const takeData = [];
for (const t of takes) {
  const txtFile = `${tmp}/${t.name}.txt`;
  writeFileSync(txtFile, `verse 1\n${t.text}\n`);
  const mp3 = `${POP}/big-pictures/out/${SLUG}-take-${t.name}.mp3`;
  console.log(`→ generate ${t.name} (style=${t.style} stab=${t.stability}): "${t.text.slice(0, 50)}…"`);
  const r = spawnSync("node", [
    `${POP}/bin/say.mjs`, txtFile,
    "--speed", "0.7", "--style", t.style, "--stability", t.stability,
    "--out", mp3, "--force",
  ], { stdio: ["ignore", "ignore", "inherit"] });
  if (r.status !== 0) { console.error(`✗ say failed for ${t.name}`); process.exit(1); }
  // Whisper align
  const wordsJson = mp3.replace(/\.mp3$/, "-words.json");
  spawnSync("rm", ["-f", wordsJson, wordsJson + ".hash"]);
  spawnSync("node", [`${POP}/bin/align.mjs`, mp3, "--force"], { stdio: ["ignore", "ignore", "inherit"] });
  if (!existsSync(wordsJson)) { console.error(`✗ align failed for ${t.name}`); process.exit(1); }
  const words = JSON.parse(readFileSync(wordsJson, "utf8"));
  // Tag each word with the take's lineRange so we know which canonical
  // words this take is allowed to contribute to.
  takeData.push({ name: t.name, mp3, words, lineRange: t.lineRange });
  console.log(`  whisper detected ${words.length} words`);
}

// ── For each canonical word, find best occurrence across takes ───────
// "Best" = longest natural duration (gives pitchsnap minimal stretching).
// Constrain candidates to takes whose lineRange includes the word's
// canonical line.
function lineForWordIdx(idx) {
  let cum = 0;
  for (let li = 0; li < lines.length; li++) {
    const wc = lines[li].split(/\s+/).length;
    if (idx < cum + wc) return li;
    cum += wc;
  }
  return lines.length - 1;
}
function normalize(s) { return s.toLowerCase().replace(/[^a-z']/g, ""); }

const bestPicks = []; // [{ canonicalWord, takeIdx, wordIdx, fromMs, toMs }]
for (let ci = 0; ci < canonicalWords.length; ci++) {
  const cw = canonicalWords[ci];
  const cline = lineForWordIdx(ci);
  // Candidates: from each take whose lineRange covers cline,
  //   find an unclaimed whisper word whose normalized text matches cw,
  //   in the take's word-order region appropriate for ci.
  const candidates = [];
  for (let ti = 0; ti < takeData.length; ti++) {
    const t = takeData[ti];
    if (cline < t.lineRange[0] || cline >= t.lineRange[1]) continue;
    // Compute this canonical word's index within the take's lyric scope.
    const lineWordsBefore = lines.slice(t.lineRange[0], cline)
      .reduce((acc, l) => acc + l.split(/\s+/).length, 0);
    const wordsBeforeInLine = ci - lines.slice(0, cline)
      .reduce((acc, l) => acc + l.split(/\s+/).length, 0);
    const expectedTakeIdx = lineWordsBefore + wordsBeforeInLine;
    // Look in the take's whisper output near expectedTakeIdx for a word
    // matching cw. Allow ±2 slack for whisper drift / merged tokens.
    for (let wi = Math.max(0, expectedTakeIdx - 2); wi < Math.min(t.words.length, expectedTakeIdx + 3); wi++) {
      const w = t.words[wi];
      if (normalize(w.text) === cw) {
        const dur = w.toMs - w.fromMs;
        candidates.push({ takeIdx: ti, wordIdx: wi, dur, fromMs: w.fromMs, toMs: w.toMs });
        break; // take first match within slack window
      }
    }
  }
  if (candidates.length === 0) {
    console.warn(`⚠ no match for canonical "${cw}" (idx ${ci}) in any take`);
    continue;
  }
  // Pick longest natural duration
  candidates.sort((a, b) => b.dur - a.dur);
  const pick = candidates[0];
  bestPicks.push({ canonicalWord: cw, ...pick });
}
console.log(`→ matched ${bestPicks.length}/${canonicalWords.length} canonical words across takes`);

// ── Extract & concat picked segments via ffmpeg ──────────────────────
// Use a tiny bit of pre-roll/post-roll on each cut so plosives aren't
// chopped, then concat with a brief crossfade.
const PRE_PAD_MS = 30;
const POST_PAD_MS = 60;
const segments = [];
const wordBoundaries = []; // for synthesized words.json
let cursor_ms = 0;
for (let pi = 0; pi < bestPicks.length; pi++) {
  const p = bestPicks[pi];
  const t = takeData[p.takeIdx];
  const cutFrom = Math.max(0, p.fromMs - PRE_PAD_MS) / 1000.0;
  const cutTo = (p.toMs + POST_PAD_MS) / 1000.0;
  const cutDur = cutTo - cutFrom;
  const segMp3 = `${tmp}/seg-${String(pi).padStart(2, "0")}.mp3`;
  const r = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-ss", String(cutFrom), "-i", t.mp3, "-t", String(cutDur),
    "-af", "afade=t=in:st=0:d=0.02,afade=t=out:st=" + Math.max(0, cutDur - 0.04).toFixed(3) + ":d=0.04",
    "-c:a", "libmp3lame", "-q:a", "4", segMp3,
  ]);
  if (r.status !== 0) { console.error(`✗ ffmpeg cut failed for word ${p.canonicalWord}`); continue; }
  segments.push(segMp3);
  // Word boundary in concat: word audio is offset PRE_PAD_MS into the segment.
  wordBoundaries.push({
    text: p.canonicalWord,
    fromMs: cursor_ms + PRE_PAD_MS,
    toMs: cursor_ms + PRE_PAD_MS + (p.toMs - p.fromMs),
  });
  cursor_ms += Math.round(cutDur * 1000);
}

const concatTxt = `${tmp}/concat.txt`;
writeFileSync(concatTxt, segments.map(s => `file '${s}'`).join("\n") + "\n");
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "concat", "-safe", "0", "-i", concatTxt,
  "-c:a", "libmp3lame", "-q:a", "4", OUT_PATH,
]);
if (r.status !== 0) { console.error("✗ concat failed"); process.exit(1); }

const wordsJsonPath = OUT_PATH.replace(/\.mp3$/, "-words.json");
writeFileSync(wordsJsonPath, JSON.stringify(wordBoundaries, null, 2));
spawnSync("rm", ["-f", wordsJsonPath + ".hash"]);

const outDur = execSync(`ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ${OUT_PATH}`).toString().trim();
console.log(`✓ ${OUT_PATH} (${Number(outDur).toFixed(2)}s)`);
console.log(`✓ ${wordsJsonPath} (${wordBoundaries.length} words)`);
console.log();
console.log("=== picks summary ===");
const takeUsage = {};
for (const p of bestPicks) {
  takeUsage[takeData[p.takeIdx].name] = (takeUsage[takeData[p.takeIdx].name] ?? 0) + 1;
}
for (const [n, c] of Object.entries(takeUsage)) {
  console.log(`  ${n}: ${c} word${c === 1 ? "" : "s"} picked`);
}

rmSync(tmp, { recursive: true, force: true });
