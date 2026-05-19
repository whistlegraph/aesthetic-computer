#!/usr/bin/env node
// sing.mjs — generate an autotuned "sung" version of the recap
// narration using pop/bin/pitchsnap.mjs.
//
// Reads:
//   recap/out/recap.mp3      — TTS narration (jeffrey-pvc)
//   recap/out/words.json     — whisper word alignment
//   recap/out/waltz-events.json — bar / scale info for choosing notes
//
// Writes:
//   recap/out/recap.np       — generated .np score (one syllable per
//                              whisper word, melody from a simple
//                              4-note cycle in the waltz's key)
//   recap/out/recap-sung.mp3 — pitch-snapped vocal
//
// The melody walks G3 → B3 → A3 → D4 → repeating, transposed up 2
// semitones during the waltz's lydian section and up 3 in dorian
// (matches audience.waltz.morph). Each word is one beat, snapped to
// the nearest 8th-note grid (light snap, preserves speech feel).
//
// Usage: node bin/sing.mjs <audience-name>

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..");

const audienceName = process.argv[2];
if (!audienceName) { console.error("usage: sing.mjs <audience-name>"); process.exit(2); }

const wordsPath = `${ROOT}/out/words.json`;
// Prefer the stable whisper snapshot (transcribe.mjs writes this on
// each fresh run). Falls back to words.json — but words.json gets
// rewritten after sing, so reading from it on subsequent runs would
// feed us our own previous output and progressively eat words.
const whisperWordsPath = `${ROOT}/out/words.whisper.json`;
const inputWordsPath = existsSync(whisperWordsPath) ? whisperWordsPath : wordsPath;
const vocalPath = `${ROOT}/out/recap.mp3`;
const waltzPath = `${ROOT}/out/waltz-events.json`;
const npPath = `${ROOT}/out/recap.np`;
const outPath = `${ROOT}/out/recap-sung.mp3`;

if (!existsSync(inputWordsPath)) { console.error(`✗ missing ${inputWordsPath} — run align.mjs first`); process.exit(1); }
if (!existsSync(vocalPath)) { console.error(`✗ missing ${vocalPath} — run tts.mjs first`); process.exit(1); }

console.log(`  ← reading words from ${inputWordsPath.replace(REPO + "/", "")} (${existsSync(whisperWordsPath) ? "stable whisper snapshot" : "current words.json"})`);
const words = JSON.parse(readFileSync(inputWordsPath, "utf8"));
const waltz = existsSync(waltzPath) ? JSON.parse(readFileSync(waltzPath, "utf8")) : null;
const BPM = (waltz && waltz.bpm) || 78;
const beatSec = 60 / BPM;
const barSec = beatSec * 3;

// ── melody walk ────────────────────────────────────────────────────────
// Major-pentatonic arpeggio pattern in jeffrey-pvc's baritone (~C3
// ref). Each 8-step cycle traces a rise-and-fall arc so phrases have
// musical shape instead of looping monotone.
const BASE_CYCLE = ["G2", "B2", "D3", "G3", "D3", "B2", "A2", "G2"];

// Crude syllable counter — counts vowel clusters. Same idea pitchsnap
// uses internally, but mirrored here so OUR score tokens match what
// pitchsnap CLAIMS per word (otherwise words eat each other's pitches).
function syllableCount(word) {
  if (!word) return 1;
  const cleaned = word.toLowerCase().replace(/[^a-z]/g, "");
  if (!cleaned) return 1;
  // Drop trailing silent 'e' (e.g., 'home', 'bone')
  const stripped = cleaned.replace(/e$/, "");
  const groups = stripped.match(/[aeiouy]+/g) || [];
  return Math.max(1, groups.length);
}

// Split a word into roughly N syllable chunks for the .np score.
// Heuristic: split at vowel-cluster boundaries. Result tokens
// concatenate back to the original word. First chunk gets a trailing
// hyphen, middle chunks get hyphens both sides, last chunk gets a
// leading hyphen — same convention amazing.np uses.
function splitSyllables(word, n) {
  if (n <= 1) return [word];
  const cleaned = word.toLowerCase();
  const matches = [...cleaned.matchAll(/[aeiouy]+[^aeiouy]*/g)];
  if (matches.length < n) return [word]; // fall back to single token
  // Cluster adjacent vowel-groups into n chunks.
  const chunks = [];
  const per = matches.length / n;
  let cursor = 0;
  for (let k = 0; k < n; k++) {
    const end = (k === n - 1) ? cleaned.length : matches[Math.min(matches.length - 1, Math.floor((k + 1) * per))].index;
    const piece = cleaned.slice(cursor, end);
    if (piece) chunks.push(piece);
    cursor = end;
  }
  if (cursor < cleaned.length) chunks[chunks.length - 1] += cleaned.slice(cursor);
  // Decorate with leading/trailing hyphens (pop convention).
  return chunks.map((c, i) => {
    let s = c;
    if (i > 0) s = "-" + s;
    if (i < chunks.length - 1) s = s + "-";
    return s;
  });
}
const NOTE_NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];

function noteToMidi(name) {
  const m = name.match(/^([A-G])(#|b)?(\d+)$/);
  if (!m) return 60;
  const base = { C:0,D:2,E:4,F:5,G:7,A:9,B:11 }[m[1]];
  const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
  const oct = Number(m[3]);
  return 12 * (oct + 1) + base + acc;
}
function midiToNote(midi) {
  const oct = Math.floor(midi / 12) - 1;
  const idx = midi % 12;
  return `${NOTE_NAMES[idx]}${oct}`.replace("#", "#"); // keep sharps
}

// Determine the transpose offset for a given start time, mapping into
// the audience.waltz.morph sections proportionally if available.
async function morphTransposeAt(secs) {
  if (!waltz) return 0;
  // Walk bars at the waltz BPM and figure out which morph section
  // contains this second by cumulative bar count.
  try {
    const mod = await import(`${ROOT}/audience/${audienceName}.mjs`);
    const audience = mod.audience || mod.default;
    const morph = audience && audience.waltz && audience.waltz.morph;
    if (!Array.isArray(morph) || morph.length === 0) return 0;
    const totalBars = waltz.bars || (audience.waltz.bars) || Math.ceil((waltz.totalSec || 0) / barSec);
    const totalWeight = morph.reduce((a, s) => a + (s.weight ?? 1), 0);
    let cumBars = 0;
    const barIdx = Math.floor(secs / barSec);
    for (const s of morph) {
      const bars = Math.max(1, Math.floor(((s.weight ?? 1) / totalWeight) * totalBars));
      cumBars += bars;
      if (barIdx < cumBars) return Number(s.transpose ?? 0);
    }
    return Number(morph[morph.length - 1].transpose ?? 0);
  } catch { return 0; }
}

// ── build score lines ──────────────────────────────────────────────────
// One score line per phrase (long pause or sentence-end punctuation).
// Each WORD splits into its syllable count (matching pitchsnap's own
// `syllableCount()`), so the score has one token per syllable. This
// lets pitchsnap's syllable-aware claim-N-tokens-per-word logic work
// correctly — without it, multi-syllable words would steal pitches
// from following words. Last syllable of each phrase gets a long
// sustain (*4) so the phrase BREATHES — the single biggest thing
// that makes pitched speech read as "singing" instead of "metronome".
const PHRASE_END = /[.!?]$/;       // sustains only on full-sentence ends
const SOFT_PHRASE_END = /[,—;:]$/; // commas → medium sustain
const PAUSE_MS = 350;
const SUSTAIN_HARD = 4;            // *4 for full-sentence ends
const SUSTAIN_SOFT = 2;            // *2 for commas / dashes
const lines = [];
let lineTokens = [];
let cycleIdx = 0;

function flushLine(sustainAtEnd) {
  if (!lineTokens.length) return;
  if (sustainAtEnd && lineTokens.length) {
    // Replace last token's *1 with *<sustainAtEnd>.
    const last = lineTokens[lineTokens.length - 1];
    lineTokens[lineTokens.length - 1] = last.replace(/\*\d+$/, `*${sustainAtEnd}`);
  }
  lines.push(lineTokens.join(" "));
  lineTokens = [];
}

let totalSyllables = 0;
for (let i = 0; i < words.length; i++) {
  const w = words[i];
  const text = (w.text || "").trim();
  if (!text) continue;
  // Strip outer punctuation but preserve a clean word for syllable splits.
  const clean = text.replace(/[.!?,;:—()"'`]+$/g, "").replace(/^[—\-"'`]+/, "");
  if (!clean) continue;
  const ns = syllableCount(clean);
  const sylls = splitSyllables(clean, ns);
  const transpose = await morphTransposeAt(w.fromMs / 1000);

  for (const syl of sylls) {
    const cycleNote = BASE_CYCLE[cycleIdx % BASE_CYCLE.length];
    const midi = noteToMidi(cycleNote) + transpose;
    const note = midiToNote(midi);
    lineTokens.push(`${note}:${syl}*1`);
    cycleIdx++;
    totalSyllables++;
  }

  // Phrase break on long pause OR sentence-end punctuation.
  const next = words[i + 1];
  const longPause = next && next.fromMs - w.toMs > PAUSE_MS;
  const hard = PHRASE_END.test(text) && lineTokens.length >= 3;
  const soft = SOFT_PHRASE_END.test(text) && lineTokens.length >= 4;
  if (hard || !next) flushLine(SUSTAIN_HARD);
  else if (longPause || soft) flushLine(SUSTAIN_SOFT);
}
flushLine(SUSTAIN_HARD);
console.log(`  syllable count: ${totalSyllables} across ${lines.length} phrases`);

// ── write .np ──────────────────────────────────────────────────────────
const np =
  `# auto-generated by recap/bin/sing.mjs from words.json + waltz events\n` +
  `# bpm=${BPM} cycle=${BASE_CYCLE.join(" ")} ref-note=C3\n` +
  `# sustains: hard sentence-end *${SUSTAIN_HARD}, soft pause *${SUSTAIN_SOFT}\n\n` +
  `verse\n` +
  lines.join("\n") + "\n";
writeFileSync(npPath, np);
console.log(`✓ ${npPath} · ${totalSyllables} syllables · ${lines.length} phrases (with sustains)`);

// ── invoke pitchsnap ───────────────────────────────────────────────────
const pitchsnap = `${REPO}/pop/bin/pitchsnap.mjs`;
if (!existsSync(pitchsnap)) { console.error(`✗ missing ${pitchsnap}`); process.exit(1); }

// Beat-mode singing: place each word at its cumulative-beat position
// from the score (not speech-time) so sustains (`*4`) actually hold.
// Without --beat-mode, sustains were ignored — words played out at
// natural speech rate with pitch overlay. With it, the score's beat
// values DRIVE the timeline.
console.log(`→ pitchsnap · vocal=${vocalPath.replace(REPO + "/", "")} score=${npPath.replace(REPO + "/", "")} bpm=${BPM} mode=beat curve=glide`);
const result = spawnSync("node", [
  pitchsnap,
  "--vocal", vocalPath,
  "--words", wordsPath,
  "--score", npPath,
  "--section", "verse",
  "--bpm", String(BPM),
  "--beat-mode",
  "--curve", "glide",
  "--ref-note", "C3",
  "--out", outPath,
], { stdio: "inherit" });

if (result.status !== 0) {
  console.error(`✗ pitchsnap exited ${result.status} — recap-sung.mp3 not produced`);
  process.exit(result.status || 1);
}
console.log(`✓ ${outPath}`);

// Re-emit words.json from the sung events.json so downstream steps
// (align, scout, slides, subtitles, waltz) see the stretched word
// timings and rebuild segments / chapter durations correctly. We
// preserve the original under words.original.json for reference.
const eventsJsonPath = `${ROOT}/out/recap-sung.events.json`;
if (existsSync(eventsJsonPath)) {
  const ev = JSON.parse(readFileSync(eventsJsonPath, "utf8"));
  if (Array.isArray(ev.events) && ev.events.length) {
    if (!existsSync(`${ROOT}/out/words.original.json`) && existsSync(wordsPath)) {
      writeFileSync(`${ROOT}/out/words.original.json`, readFileSync(wordsPath));
    }
    const sungWords = ev.events.map((e) => ({
      text: e.text,
      fromMs: Math.round(e.snappedStart * 1000),
      toMs: Math.round((e.snappedStart + e.durSec) * 1000),
    }));
    writeFileSync(wordsPath, JSON.stringify(sungWords, null, 2));
    console.log(`✓ ${wordsPath} ← rewritten from sung events (${sungWords.length} words, total ${ev.totalDur.toFixed(2)}s)`);
  }
}
