#!/usr/bin/env node
// perword.mjs — generate per-WORD ElevenLabs takes (one API call per
// lyric word) and concat them with tiny gaps between.
//
// Why: per-line takes ("that saved a wretch like me") give ElevenLabs
// the whole phrase to perform, which means held phrase-end syllables
// (`me*5` = 4.29s target) only get ~0.2s of natural source duration.
// Pitchsnap then has to do 18× stretch, which exceeds WORLD's clean
// range and produces garbled vowel sustain. The user can't hear "me"
// in the rendered audio.
//
// Per-word takes give each syllable ~0.5-1.5s of natural ElevenLabs
// prosody (a single word read aloud has natural release/decay), so
// pitchsnap needs only 3-5× stretch — clean WORLD range, audible word.
//
// Trade-off: more API calls (26 instead of 4 for amazing's verse 1).
// At jeffrey-pvc rates, ~$0.30 vs $0.05 per regen.
//
// Usage:
//   node bin/perword.mjs --slug amazing
//     [--gap-ms 80] [--style 0.55] [--stability 0.7] [--speed 0.7]

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
const SCORE_PATH = `${POP}/big-pictures/${SLUG}.np`;
const LYRIC_PATH = `${POP}/big-pictures/${SLUG}.txt`;
const SECTION    = (flags.section || "verse 1").toLowerCase();
const BPM        = Number(flags.bpm ?? 70);
// 80ms was too tight — whisper merged single-char words ('i', 'a')
// into adjacent words during the source-side segmentation, causing
// pitchsnap to skip those score positions and shift every subsequent
// word by one slot. 250ms minimum keeps word boundaries distinct.
const GAP_MS     = Number(flags["gap-ms"] ?? 250);
const HELD_GAP_MS = Number(flags["held-gap-ms"] ?? 400);
const STYLE      = flags.style     ?? "0.55";
const STABILITY  = flags.stability ?? "0.7";
const SPEED      = flags.speed     ?? "0.7";
// Validator + retry: each generated word must have natural duration
// >= max(MIN_NATURAL_S, target_dur / MAX_STRETCH). For a *5 hold at
// BPM=70, target=4.29s; with MAX_STRETCH=4 we need natural >= 1.07s.
// Short *1 words just need >= 0.4s. Retries cycle through prosody
// strategies until threshold hits or we run out.
const MIN_NATURAL_S = Number(flags["min-natural"] ?? 0.40);
const MAX_STRETCH   = Number(flags["max-stretch"] ?? 4.0);
const MAX_RETRIES   = Number(flags["max-retries"] ?? 5);
const OUT_PATH   = flags.out
  ? flags.out
  : `${POP}/big-pictures/out/${SLUG}-perword.mp3`;

if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }
if (!existsSync(LYRIC_PATH)) { console.error(`✗ lyrics missing: ${LYRIC_PATH}`); process.exit(1); }

// ── Parse score: per-line list of {raw, weight} ───────────────────────
const scoreLines = readFileSync(SCORE_PATH, "utf8").split("\n");
const sStart = scoreLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
if (sStart < 0) { console.error(`✗ section '${SECTION}' missing in score`); process.exit(1); }
const scoreSyls = []; // flat array of {raw, weight} for the section
for (let i = sStart + 1; i < scoreLines.length; i++) {
  const l = scoreLines[i].trim();
  if (!l) break;
  if (l.startsWith("#")) continue;
  if (/^[a-z]+ \d/i.test(l)) break;
  for (const tok of l.split(/\s+/)) {
    const m = tok.match(/^[A-Ga-g][#b]?-?\d:(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
    if (!m) continue;
    scoreSyls.push({ raw: m[1], weight: Number(m[2] ?? 1) });
  }
}

// Group syllables into words by hyphen markers — each lyric word maps
// to one or more score syllables. Take MAX weight as the word's hold
// (so multi-syllable words like 'a-ma-zing' inherit the held syllable's
// weight if any).
const wordWeights = [];
let cur = null;
for (const s of scoreSyls) {
  if (s.raw.startsWith("-") && cur) {
    cur.weight = Math.max(cur.weight, s.weight);
  } else {
    cur = { weight: s.weight };
    wordWeights.push(cur);
  }
}
console.log(`→ score: ${scoreSyls.length} syllables grouped into ${wordWeights.length} words`);

// ── Parse lyric file: flat array of words ─────────────────────────────
const lyricLines = readFileSync(LYRIC_PATH, "utf8").split("\n");
const lStart = lyricLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
if (lStart < 0) { console.error(`✗ section '${SECTION}' missing in lyrics`); process.exit(1); }
const lyricWords = [];
for (let i = lStart + 1; i < lyricLines.length; i++) {
  const t = lyricLines[i].trim();
  if (!t) break;
  if (/^[a-z]+ \d/i.test(t)) break;
  for (const w of t.split(/\s+/)) lyricWords.push(w);
}
console.log(`→ lyrics: ${lyricWords.length} words`);

if (lyricWords.length !== wordWeights.length) {
  console.warn(`⚠ word counts mismatch (lyrics=${lyricWords.length}, score=${wordWeights.length})`);
}

// ── Helpers ──────────────────────────────────────────────────────────
function probeDur(path) {
  try {
    const out = execSync(
      `ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ${path}`,
      { encoding: "utf8" }
    ).trim();
    return Number(out);
  } catch { return 0; }
}

function elongateVowel(word, n) {
  const vowels = [...word.matchAll(/[aeiouy]+/gi)];
  if (vowels.length === 0) return word + "e".repeat(n);
  const v = vowels[vowels.length - 1];
  const vEnd = v.index + v[0].length;
  return word.slice(0, vEnd) + v[0].slice(-1).repeat(n) + word.slice(vEnd);
}

// Retry strategies — each tweaks one or more knobs of the request.
// Returns [{label, text, style, stability}, ...]
function retryStrategies(word) {
  return [
    { label: "ellipsis",    text: `${word}...`,                     style: STYLE,      stability: STABILITY },
    { label: "double-vowel",text: elongateVowel(word, 2) + "...",   style: STYLE,      stability: STABILITY },
    { label: "soft-low-stab",text: `${word}, ${word}.`,             style: "0.45",     stability: "0.55" },
    { label: "many-dots",   text: `${word}............`,            style: "0.50",     stability: STABILITY },
    { label: "echo+vowel",  text: `${elongateVowel(word, 4)}.`,     style: "0.60",     stability: "0.55" },
  ];
}

// ── Generate one ElevenLabs take per word with validate+retry ────────
const tmp = mkdtempSync(`${tmpdir()}/perword-${SLUG}-`);
const wordMp3s = [];
for (let i = 0; i < lyricWords.length; i++) {
  const word = lyricWords[i].replace(/[.,!?;:]/g, "");
  const wt = (wordWeights[i] && wordWeights[i].weight) ?? 1;
  const targetDur = wt * (60 / BPM);
  // Required natural duration: at least MIN_NATURAL_S, but if that's
  // not enough to keep stretch <= MAX_STRETCH, raise the bar.
  const needed = Math.max(MIN_NATURAL_S, targetDur / MAX_STRETCH);

  // Try: original first, then ellipsis-augmented, then progressively
  // more aggressive prosody hints. Keep the LONGEST that meets `needed`,
  // or the longest of all attempts if none do.
  const wordOut = `${POP}/big-pictures/out/${SLUG}-w${String(i).padStart(2, "0")}.mp3`;
  const attempts = [
    { label: "plain", text: word,                  style: STYLE, stability: STABILITY },
    ...retryStrategies(word).slice(0, MAX_RETRIES),
  ];
  let best = { dur: 0, attempt: -1, path: null, label: null };
  for (let a = 0; a < attempts.length; a++) {
    const att = attempts[a];
    const candPath = `${tmp}/${SLUG}-w${String(i).padStart(2, "0")}-a${a}.mp3`;
    const candFile = `${tmp}/${SLUG}-w${String(i).padStart(2, "0")}-a${a}.txt`;
    writeFileSync(candFile, `verse 1\n${att.text}\n`);
    const r = spawnSync("node", [
      `${POP}/bin/say.mjs`, candFile,
      "--speed",     String(SPEED),
      "--style",     String(att.style),
      "--stability", String(att.stability),
      "--out",       candPath,
      "--force",
    ], { stdio: ["ignore", "ignore", "inherit"] });
    if (r.status !== 0) continue;
    const d = probeDur(candPath);
    if (d > best.dur) best = { dur: d, attempt: a, path: candPath, label: att.label };
    if (d >= needed) break;  // good enough — stop trying
  }
  if (!best.path) { console.error(`✗ all retries failed on "${word}"`); process.exit(1); }

  // Copy winner to canonical location
  spawnSync("cp", [best.path, wordOut]);
  const ok = best.dur >= needed ? "✓" : "⚠";
  console.log(
    `${ok} [${String(i + 1).padStart(2, " ")}/${lyricWords.length}] "${word.padEnd(8)}" hold=${wt}× ` +
    `target=${targetDur.toFixed(2)}s need=${needed.toFixed(2)}s ` +
    `got=${best.dur.toFixed(2)}s [a${best.attempt}:${best.label}]`
  );
  wordMp3s.push({ path: wordOut, weight: wt });
}

// ── Concat with per-word silence gaps ────────────────────────────────
// Held words get longer LEADING silence so whisper keeps them as
// separate utterances when aligning the concat.
function silMp3(durS, name) {
  const path = `${tmp}/${name}.mp3`;
  spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-f", "lavfi", "-t", String(durS), "-i", "anullsrc=r=44100:cl=mono",
    path,
  ]);
  return path;
}
const shortSil = silMp3(GAP_MS / 1000, "sil-short");
const heldSil  = silMp3(HELD_GAP_MS / 1000, "sil-held");

const concatTxt = `${tmp}/concat.txt`;
const lines = [];
for (let i = 0; i < wordMp3s.length; i++) {
  const w = wordMp3s[i];
  if (i > 0) {
    // Use the LONGER gap before any word that's part of a held syllable
    // (matches user pattern: held notes need clean entry boundary).
    lines.push(`file '${w.weight >= 3 ? heldSil : shortSil}'`);
  }
  lines.push(`file '${w.path}'`);
}
writeFileSync(concatTxt, lines.join("\n") + "\n");

const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "concat", "-safe", "0", "-i", concatTxt,
  "-c:a", "libmp3lame", "-q:a", "4", OUT_PATH,
], { stdio: "inherit" });
if (r.status !== 0) { console.error("✗ concat failed"); process.exit(1); }

rmSync(tmp, { recursive: true, force: true });
const dur = execSync(`ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ${OUT_PATH}`).toString().trim();
console.log(`✓ ${OUT_PATH} (${Number(dur).toFixed(2)}s, ${lyricWords.length} words, gap=${GAP_MS}ms / held-gap=${HELD_GAP_MS}ms)`);

// ── Synthesize words.json from KNOWN word durations + gaps ───────────
// Don't rely on whisper to segment — it routinely drops short words
// like 'i' and 'a' even with generous inter-word gaps, which then
// causes pitchsnap to skip score syllables and shift every subsequent
// word by one slot ("me/i confusion"). Since we generated each word
// as its own mp3 with known gaps, the boundaries are deterministic.
const wordsJsonPath = OUT_PATH.replace(/\.mp3$/, "-words.json");
const wordsJsonHash = wordsJsonPath + ".hash";
const synthWords = [];
let cursor_ms = 0;
for (let i = 0; i < wordMp3s.length; i++) {
  const w = wordMp3s[i];
  const wd_s = probeDur(w.path);
  const wd_ms = Math.round(wd_s * 1000);
  // Apply LEADING gap (this word's entry pause)
  if (i > 0) {
    const isHeld = w.weight >= 3;
    cursor_ms += isHeld ? HELD_GAP_MS : GAP_MS;
  }
  synthWords.push({
    text: lyricWords[i].replace(/[.,!?;:]/g, ""),
    fromMs: cursor_ms,
    toMs: cursor_ms + wd_ms,
  });
  cursor_ms += wd_ms;
}
writeFileSync(wordsJsonPath, JSON.stringify(synthWords, null, 2));
// Drop any stale whisper hash so downstream callers don't think this
// is a whisper-aligned file.
if (existsSync(wordsJsonHash)) {
  try { execSync(`rm ${wordsJsonHash}`); } catch (_) {}
}
console.log(`  ✓ synthesized ${synthWords.length} word boundaries → ${wordsJsonPath}`);
