#!/usr/bin/env node
// score-render.mjs — score-as-truth audio render.
//
// Pivot from the previous "stretch a whole-verse take to fit the score
// then realign whisper transcripts" pipeline. The score already knows
// every word's start time, target duration, and target pitch — render
// each word independently and place it at score time. No transcription
// step needed; the score IS the alignment.
//
// Pipeline:
//   1. Parse score (.np) → flat array of words with (start_s, dur_s,
//      target_midi). Group multi-syllable score tokens into one word.
//   2. For each word, take its per-word clip from best-of-takes' picks
//      (already cached as ${slug}-take-*.mp3 + per-word selections).
//      Or just use ${slug}-perword.mp3 segments via the words.json
//      that perword.mjs synthesized.
//   3. Rubberband each clip:
//        --time = target_dur / source_dur (stretch to score duration)
//        --pitch = target_midi - source_midi (semitone shift)
//   4. Place each stretched clip at score_start_s using ffmpeg adelay.
//   5. Mix all clips into one track (amix or just sequential delay+concat).
//
// Output: ${slug}-score-rendered.mp3 — clean, score-aligned, no whisper.
//
// Usage:
//   node bin/score-render.mjs --slug amazing
//     [--source perword|bestof]    pick the per-word source set
//     [--bpm 70]
//     [--ref-note C3]              source baritone reference
//     [--out path.mp3]

import { execSync, spawnSync } from "node:child_process";
import { readFileSync, existsSync, mkdtempSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { tmpdir } from "node:os";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}

const SLUG = flags.slug || "amazing";
const POP  = "/Users/jas/aesthetic-computer/pop";
const SCORE_PATH = `${POP}/big-pictures/${SLUG}.np`;
// `--section all` walks every `verse N` block in order, treating each
// verse-marker as a phrase break (configurable via --inter-verse-beats).
const SECTION = (flags.section || "verse 1").toLowerCase();
const INTER_VERSE_BEATS = Number(flags["inter-verse-beats"] ?? 2);
const BPM = Number(flags.bpm ?? 70);
const REF_NOTE = flags["ref-note"] ?? "C3";
const SOURCE_KIND = flags.source ?? "perword"; // perword | bestof
// User feedback: pitches feel too high / not deep enough. Score is
// mostly D3-D4 but jeffrey-pvc's natural baritone is C3-G3. Default
// transpose -12 (one octave down) puts targets in his comfortable
// range, gives the deep ballooned baritone instead of nasal tenor.
const TRANSPOSE = Number(flags.transpose ?? -12);
// True autotune: WORLD f0 replacement per clip (not rubberband -p
// which only shifts the contour). Set false to use cheaper rubberband.
const USE_WORLD = flags["no-world"] ? false : true;
const OUT_PATH = flags.out || `${POP}/big-pictures/out/${SLUG}-score-rendered.mp3`;

if (!existsSync(SCORE_PATH)) { console.error(`✗ score missing: ${SCORE_PATH}`); process.exit(1); }

// ── Note → MIDI ──────────────────────────────────────────────────────
const NOTE_BASE = { "C":0,"C#":1,"DB":1,"D":2,"D#":3,"EB":3,"E":4,"F":5,
                    "F#":6,"GB":6,"G":7,"G#":8,"AB":8,"A":9,"A#":10,"BB":10,"B":11 };
function noteToMidi(s) {
  const u = s.toUpperCase();
  const oct = parseInt(u.slice(-1), 10);
  const name = u.slice(0, -1);
  return 12 * (oct + 1) + NOTE_BASE[name];
}
const REF_MIDI = noteToMidi(REF_NOTE);

// ── Parse score: flat list of {note, raw, weight} per syllable ───────
const scoreLines = readFileSync(SCORE_PATH, "utf8").split("\n");
const syllables = [];
function parseRange(startIdx) {
  for (let i = startIdx; i < scoreLines.length; i++) {
    const l = scoreLines[i].trim();
    if (!l) continue;
    if (l.startsWith("#")) continue;
    if (/^[a-z]+ \d+$/i.test(l)) break;
    for (const tok of l.split(/\s+/)) {
      const m = tok.match(/^([A-Ga-g][#b]?-?\d):(.+?)(?:\*(\d+(?:\.\d+)?))?$/);
      if (!m) continue;
      syllables.push({ note: m[1], raw: m[2], weight: Number(m[3] ?? 1) });
    }
  }
}
if (SECTION === "all") {
  // Walk every `verse N` header in order; insert a rest weight between
  // verses so phrase breaks read musically.
  const headers = [];
  for (let i = 0; i < scoreLines.length; i++) {
    if (/^verse \d+$/i.test(scoreLines[i].trim())) headers.push(i);
  }
  if (!headers.length) { console.error("✗ section=all but no `verse N` headers found"); process.exit(1); }
  headers.forEach((h, idx) => {
    const before = syllables.length;
    parseRange(h + 1);
    if (idx < headers.length - 1 && syllables.length > before && INTER_VERSE_BEATS > 0) {
      // Mark a rest by inflating the previous syllable's weight (no audio
      // shifts; just gives the next word a later start_beat).
      syllables[syllables.length - 1].weight += INTER_VERSE_BEATS;
    }
  });
} else {
  const sStart = scoreLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
  if (sStart < 0) { console.error(`✗ section missing: ${SECTION}`); process.exit(1); }
  parseRange(sStart + 1);
}
console.log(`→ score: ${syllables.length} syllables`);

// Group syllables → words by hyphen-marker chains. Word's effective note
// is its FIRST syllable's note (the attack); duration = sum of its
// syllables' weights × beat_s.
const beat_s = 60.0 / BPM;
let beat_pos = 0;
const words = [];
let curWord = null;
for (const s of syllables) {
  const startsContinuation = s.raw.startsWith("-");
  if (startsContinuation && curWord) {
    curWord.weights.push(s.weight);
    curWord.notes.push(s.note);
  } else {
    if (curWord) words.push(curWord);
    curWord = {
      raw: s.raw.replace(/^-|-$/g, ""),
      notes: [s.note],
      weights: [s.weight],
      start_beat: beat_pos,
    };
  }
  beat_pos += s.weight;
}
if (curWord) words.push(curWord);
for (const w of words) {
  w.text = w.raw.replace(/[.,!?;:]/g, "").toLowerCase();
  w.start_s = w.start_beat * beat_s;
  w.dur_s = w.weights.reduce((a, b) => a + b, 0) * beat_s;
  // Apply global TRANSPOSE so the score sits in jeffrey's baritone
  // range. All target_midis (and the per-syllable melody curve below)
  // shift by the same amount.
  w.target_midi = noteToMidi(w.notes[0]) + TRANSPOSE;
  w.target_midis_per_syllable = w.notes.map(n => noteToMidi(n) + TRANSPOSE);
  w.target_notes_per_syllable = w.notes.map(n => {
    const m = noteToMidi(n) + TRANSPOSE;
    const oct = Math.floor(m / 12) - 1;
    const name = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"][m % 12];
    return `${name}${oct}`;
  });
}
console.log(`→ ${words.length} words; total duration = ${(beat_pos * beat_s).toFixed(2)}s`);

// ── Locate per-word source clips ─────────────────────────────────────
// Both perword.mjs and best-of-takes.mjs save individual word audio.
// perword: ${slug}-perword.mp3 + ${slug}-perword-words.json (concat'd
//   single take per word, with synthesized boundaries).
// best-of-takes: ${slug}-bestof.mp3 + ${slug}-bestof-words.json (best
//   pick per word from multiple takes).
// Custom source overrides — point at any vocal stem + words sidecar.
// Useful for the 7-verse cut where the source is the streaming TTS take
// (amazing-7verse-vocal.mp3) rather than the per-word/best-of caches.
const sourceMp3 = flags["source-mp3"]
  ? resolve(process.cwd(), flags["source-mp3"])
  : `${POP}/big-pictures/out/${SLUG}-${SOURCE_KIND}.mp3`;
const sourceWordsJson = flags["source-words"]
  ? resolve(process.cwd(), flags["source-words"])
  : `${POP}/big-pictures/out/${SLUG}-${SOURCE_KIND}-words.json`;
if (!existsSync(sourceMp3))    { console.error(`✗ source mp3 missing: ${sourceMp3}`); process.exit(1); }
if (!existsSync(sourceWordsJson)) { console.error(`✗ source words missing: ${sourceWordsJson}`); process.exit(1); }
const sourceWords = JSON.parse(readFileSync(sourceWordsJson, "utf8"));

if (sourceWords.length !== words.length) {
  console.warn(`⚠ source has ${sourceWords.length} words, score has ${words.length}; aligning by index`);
}
const n = Math.min(sourceWords.length, words.length);

// ── Source pitch (jeffrey-pvc baritone) ──────────────────────────────
// Use REF_MIDI as the assumed source pitch. pitchsnap measures exact f0
// per word; we just use a constant baritone reference for simplicity.
// Per-word semitone shift = target_midi - REF_MIDI.

// ── Generate stretched + pitched per-word clips ──────────────────────
const tmp = mkdtempSync(`${tmpdir()}/score-render-${SLUG}-`);
const wordClips = [];
for (let i = 0; i < n; i++) {
  const w = words[i];
  const sw = sourceWords[i];
  const src_dur = (sw.toMs - sw.fromMs) / 1000;
  const stretch = w.dur_s / Math.max(0.05, src_dur);
  const semitones = w.target_midi - REF_MIDI;
  // Extract this word's audio from the source mp3
  const cutWav = `${tmp}/w${String(i).padStart(2, "0")}-cut.wav`;
  spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-ss", String(sw.fromMs / 1000), "-i", sourceMp3,
    "-t", String(src_dur), "-ar", "44100", "-ac", "1",
    cutWav,
  ]);
  // 1) Rubberband stretch (formant-preserving) to target duration —
  //    NO pitch shift here; pitch is handled by WORLD next.
  const stretchedWav = `${tmp}/w${String(i).padStart(2, "0")}-stretch.wav`;
  spawnSync("rubberband", [
    "-t", String(stretch), "--formant",
    cutWav, stretchedWav,
  ], { stdio: ["ignore", "ignore", "ignore"] });
  if (!existsSync(stretchedWav)) {
    console.error(`✗ rubberband stretch failed on word ${i} '${w.text}'`);
    continue;
  }
  let rendWav = stretchedWav;
  // 2) WORLD f0 replacement (true autotune) — replaces the source f0
  //    contour with the target note(s) entirely, preserving formants.
  //    Multi-syllable words pass --notes A,B,C with --weights for the
  //    per-syllable melody curve.
  if (USE_WORLD) {
    const worldWav = `${tmp}/w${String(i).padStart(2, "0")}-world.wav`;
    const notesArg = w.target_notes_per_syllable.join(",");
    const weightsArg = w.weights.join(",");
    const r = spawnSync(`${POP}/.venv/bin/python`, [
      `${POP}/bin/pitchsnap_world.py`, stretchedWav, worldWav,
      "--notes", notesArg,
      "--weights", weightsArg,
      "--retain", "1.0",
    ], { stdio: ["ignore", "ignore", "ignore"] });
    if (r.status === 0 && existsSync(worldWav)) {
      rendWav = worldWav;
    } else {
      console.warn(`  ! WORLD failed for '${w.text}', falling back to rubberband -p`);
      const rbWav = `${tmp}/w${String(i).padStart(2, "0")}-rb.wav`;
      spawnSync("rubberband", [
        "-p", String(semitones), "--formant",
        stretchedWav, rbWav,
      ], { stdio: ["ignore", "ignore", "ignore"] });
      if (existsSync(rbWav)) rendWav = rbWav;
    }
  } else if (semitones !== 0) {
    const rbWav = `${tmp}/w${String(i).padStart(2, "0")}-rb.wav`;
    spawnSync("rubberband", [
      "-p", String(semitones), "--formant",
      stretchedWav, rbWav,
    ], { stdio: ["ignore", "ignore", "ignore"] });
    if (existsSync(rbWav)) rendWav = rbWav;
  }
  wordClips.push({
    text: w.text,
    start_s: w.start_s,
    dur_s: w.dur_s,
    src_dur,
    stretch,
    semitones,
    path: rendWav,
  });
  console.log(
    `  [${String(i + 1).padStart(2)}/${n}] '${w.text.padEnd(8)}' ` +
    `start=${w.start_s.toFixed(2)}s dur=${w.dur_s.toFixed(2)}s ` +
    `src=${src_dur.toFixed(2)}s stretch=${stretch.toFixed(2)}× ` +
    `pitch=${semitones >= 0 ? "+" : ""}${semitones}st`
  );
}

// ── Place each stretched clip at its score start_s and mix ───────────
// Use ffmpeg with N inputs, each delayed via adelay, then amix=N. Or for
// large N, do iterative mixdown to avoid command-line bloat.
const totalDur = beat_pos * beat_s;
console.log(`→ mixing ${wordClips.length} clips into ${totalDur.toFixed(2)}s base`);

// Build the filter graph
const inputs = wordClips.flatMap((c) => ["-i", c.path]);
const filters = wordClips.map((c, idx) => {
  const delay_ms = Math.round(c.start_s * 1000);
  return `[${idx}:a]adelay=${delay_ms}|${delay_ms}[d${idx}]`;
});
const sumLabels = wordClips.map((_, idx) => `[d${idx}]`).join("");
const filterGraph =
  filters.join(";") +
  `;${sumLabels}amix=inputs=${wordClips.length}:duration=longest:dropout_transition=0:normalize=0[out]`;

const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  ...inputs,
  "-filter_complex", filterGraph,
  "-map", "[out]",
  "-c:a", "libmp3lame", "-q:a", "4",
  OUT_PATH,
]);
if (r.status !== 0) { console.error("✗ mix failed"); process.exit(1); }

const outDur = execSync(`ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 ${OUT_PATH}`).toString().trim();
console.log(`✓ ${OUT_PATH} (${Number(outDur).toFixed(2)}s)`);

rmSync(tmp, { recursive: true, force: true });
