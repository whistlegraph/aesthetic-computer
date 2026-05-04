#!/usr/bin/env node
// pitchsnap.mjs — aggressive per-word post-prod, no elongation.
//
// For each whisper-aligned word:
//   1. Snap its START to the nearest 16th-note slot at the target BPM
//      (no time-stretch — word duration stays natural)
//   2. Pitch-shift to the target note from the .np score (proportional
//      word→syllable mapping, formant-preserving via rubberband)
//   3. Place the pitched slice into a fresh buffer at the snapped start
//
// Inter-word gaps end up shifted slightly (sometimes longer, sometimes
// shorter) — that's the snap. Words themselves keep natural speech
// rate, so jeffrey-pvc doesn't sound rushed; only their *placement*
// quantizes to the grid.
//
// Stretch ("lazy" mode): pass `--stretch FACTOR` to time-stretch every
// word by FACTOR using rubberband (formant + pitch preserving), then
// re-snap the stretched starts to the grid. 1.0 = natural, 1.5 = lazy
// (50% longer per word), 2.0 = drone. Total track duration grows.
//
// Usage:
//   node bin/pitchsnap.mjs --vocal big-pictures/out/ac-vocal.mp3 \
//        --score big-pictures/plork.np --section hook \
//        --bpm 140 --grid 16 --ref-note C3 \
//        --stretch 1.4 \
//        --out big-pictures/out/ac-snapped-pitched.mp3

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync, rmSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP_ROOT = resolve(HERE, "..");

function parseArgs(argv) {
  const flags = {};
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (!a.startsWith("--")) continue;
    const k = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[k] = next; i++; }
    else flags[k] = true;
  }
  return flags;
}

const flags = parseArgs(process.argv.slice(2));

const vocalPath = resolve(process.cwd(), flags.vocal || "");
if (!existsSync(vocalPath)) {
  console.error("usage: --vocal <stem.mp3> --score <path.np> [--section hook] [--bpm 140] [--grid 16] [--ref-note C3] [--out path.mp3]");
  process.exit(1);
}
const wordsPath = resolve(process.cwd(), flags.words || vocalPath.replace(/\.mp3$/, "-words.json"));
if (!existsSync(wordsPath)) {
  console.error(`✗ words.json not found at ${wordsPath}. run bin/align.mjs first.`);
  process.exit(1);
}
const scorePath = resolve(process.cwd(), flags.score || "");
if (!existsSync(scorePath)) {
  console.error(`✗ --score file required (path to .np)`);
  process.exit(1);
}
const SECTION = (flags.section || "hook").toLowerCase();
const BPM = Number(flags.bpm) || 140;
const GRID = Number(flags.grid) || 16; // 16 = sixteenth notes per bar
const REF_NOTE = flags["ref-note"] || "C3";
const STRETCH = Number(flags.stretch) || 1.0; // 1.0 = natural, >1 = lazier
const CURVE = flags.curve || "flat";          // "flat" | "linear" | "bezier"
// Engine: "rubberband" (default — segmented per-syllable rubberband
// shifts, preserves source prosody shifted up/down) or "world" (calls
// pitchsnap_world.py for WORLD-vocoder f0 replacement, fully clamps
// pitch to target with vocal timbre intact). World requires the .venv
// at pop/.venv with pyworld + soundfile installed.
const ENGINE = flags.engine || "rubberband";
const RETAIN = Number(flags.retain ?? 1.0);    // world only: 0 = source, 1 = clamp
const VIBRATO_HZ = Number(flags["vibrato-hz"] ?? 0);
const VIBRATO_CENTS = Number(flags["vibrato-cents"] ?? 0);
// Transpose every target note by N semitones at runtime (no need to
// rewrite the .np). Useful when the score is in a register too high
// for the source voice (jeffrey-pvc baritone ≈ C3, so kid-songs at
// C4-E4 sound chipmunky — try --transpose -12 to drop them an octave).
const TRANSPOSE = Number(flags.transpose ?? 0);
// Beat mode: interpret syllable `*weight` as BEATS at the given BPM,
// not as relative multipliers. Each word's stretch becomes
// (sum(beats) * 60/BPM) / naturalWordDuration. Required for real
// song timing — speech speeds rarely match the song's meter.
const BEAT_MODE = flags["beat-mode"] === true;
// Detect syllable boundaries within each word via librosa onset
// detection (in pitchsnap_world.py). For multi-syllable words this
// snaps the per-syllable pitch targets to natural energy peaks in
// the audio rather than weighted-proportional splits.
const DETECT_BOUNDARIES = flags["detect-boundaries"] === true;
// Scale walk: comma-separated notes (e.g. "C3,D3,Eb3,F3,G3,Ab3,Bb3,C4")
// that override the .np score's syllable pitches. The full scale is
// laid across each word's duration as evenly-spaced pitchmap
// waypoints — useful for single-word melody experiments where you
// want a slur through more notes than the word has syllables.
const SCALE_WALK = flags["scale-walk"]
  ? String(flags["scale-walk"]).split(",").map((s) => s.trim()).filter(Boolean)
  : null;
// autotune: off | "global" (median source pitch — uniform shifts, robust)
// | "word" (per-word source — accurate but octave-error-prone).
// Default "global" because autocorrelation f0-detection occasionally
// finds 2× or ½× on individual words, causing audible octave jumps.
const AUTOTUNE = flags.autotune === true ? "global"
                : flags.autotune === false ? "off"
                : (flags.autotune || "off");
const SAMPLE_RATE = 48_000;
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : vocalPath.replace(/\.mp3$/, "-snapped-pitched.mp3");

// ── helpers ───────────────────────────────────────────────────────────
const NOTE_TO_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function noteToMidi(p) {
  const m = p.trim().toLowerCase().match(/^([a-g])([#b]?)(-?\d+)$/);
  if (!m) throw new Error(`bad note: ${p}`);
  let semi = NOTE_TO_SEMI[m[1]];
  if (m[2] === "#") semi += 1;
  if (m[2] === "b") semi -= 1;
  const oct = parseInt(m[3], 10);
  return 12 * (oct + 1) + semi;
}

function parseNp(text) {
  const sections = {};
  let current = null;
  for (const raw of text.split("\n")) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    if (!line.includes(":") && /^[a-z][a-z0-9 ]*$/.test(line)) {
      current = line.toLowerCase();
      if (!sections[current]) sections[current] = [];
      continue;
    }
    if (!current) { current = "default"; sections[current] = []; }
    const tokens = line.split(/\s+/).filter(Boolean);
    for (const tok of tokens) {
      // Note: letter + optional sharp/flat + optional octave digit + ":" + syllable
      // Token grammar: <note>:<syllable>[*<weight>]
      // Weight is a relative duration multiplier (default 1). E.g.
      // `G3:-ma-*2` makes "ma" twice as long as a default syllable
      // when distributing time across the parent word.
      const m = tok.match(/^([A-Ga-g][#b]?\d?):(.+?)(?:\*([\d.]+))?$/);
      if (!m) continue;
      const note = m[1].charAt(0).toUpperCase() + m[1].slice(1);
      const weight = m[3] ? Number(m[3]) : 1;
      sections[current].push({ pitch: note, syl: m[2], weight });
    }
  }
  return sections;
}

function probeDuration(p) {
  const r = spawnSync(
    "ffprobe",
    ["-v", "error", "-show_entries", "format=duration",
     "-of", "default=noprint_wrappers=1:nokey=1", p],
    { encoding: "utf8" }
  );
  return Number(r.stdout.trim());
}

// Heuristic syllable count for an English word — counts vowel groups,
// drops trailing silent 'e'. Good enough for lyric-to-score alignment;
// failure modes (e.g. "fire" → 1 vs. true 2) drift by ~1 syllable per
// word but the proportional mapping recovers across line lengths.
function syllableCount(word) {
  const w = word.toLowerCase().replace(/[^a-z]/g, "");
  if (!w) return 1;
  const groups = w.match(/[aeiouy]+/g) || [];
  let count = groups.length;
  // Silent 'ed' suffix in past-tense verbs (saved, called, loved).
  // Exception: "ed" preceded by t/d IS pronounced (wanted, rated).
  if (w.endsWith("ed") && w.length > 2 && count > 1) {
    const beforeEd = w.charAt(w.length - 3);
    if (beforeEd !== "t" && beforeEd !== "d") count--;
  }
  // Silent trailing 'e' (rake, love, since, more) — but not after the
  // ed-rule has already fired.
  else if (w.endsWith("e") && count > 1) count--;
  // 'le' creates an extra syllable when preceded by a consonant
  // (apple, little, bottle). After silent-e adjustment.
  if (w.endsWith("le") && w.length > 2 && !"aeiouy".includes(w.charAt(w.length - 3))) count++;
  return Math.max(1, count);
}

// Autocorrelation pitch detection — same algorithm as pitchcheck.mjs.
// Restricted to voice range, uses central window, parabolic interp.
function detectPitch(samples, sr, fmin = 80, fmax = 600) {
  if (samples.length < sr * 0.05) return null;
  const start = Math.floor(samples.length * 0.2);
  const end = Math.floor(samples.length * 0.8);
  const win = samples.slice(start, end);
  let rms = 0;
  for (let i = 0; i < win.length; i++) rms += win[i] * win[i];
  rms = Math.sqrt(rms / win.length);
  if (rms < 0.005) return null;
  const lagMin = Math.floor(sr / fmax);
  const lagMax = Math.min(Math.floor(sr / fmin), Math.floor(win.length / 2));
  let bestLag = lagMin, bestScore = -Infinity;
  for (let lag = lagMin; lag <= lagMax; lag++) {
    let sum = 0;
    const n = win.length - lag;
    for (let i = 0; i < n; i++) sum += win[i] * win[i + lag];
    sum /= n;
    if (sum > bestScore) { bestScore = sum; bestLag = lag; }
  }
  let lagF = bestLag;
  if (bestLag > lagMin && bestLag < lagMax) {
    const acAt = (k) => {
      let s = 0;
      const n = win.length - k;
      for (let i = 0; i < n; i++) s += win[i] * win[i + k];
      return s / n;
    };
    const a = acAt(bestLag - 1), b = acAt(bestLag), c = acAt(bestLag + 1);
    const denom = a - 2 * b + c;
    if (Math.abs(denom) > 1e-9) lagF = bestLag - 0.5 * (c - a) / denom;
  }
  return sr / lagF;
}
function freqToMidi(f) { return 69 + 12 * Math.log2(f / 440); }

function readWav(path) {
  // Read a 16-bit PCM mono wav written by ffmpeg into a Float32Array
  // of normalized samples. Skips RIFF/fmt headers via the data chunk.
  const buf = readFileSync(path);
  // Find 'data' chunk
  let i = 12;
  while (i < buf.length - 8) {
    const id = buf.toString("ascii", i, i + 4);
    const size = buf.readUInt32LE(i + 4);
    if (id === "data") {
      i += 8;
      const samples = new Float32Array(size / 2);
      for (let j = 0; j < samples.length; j++) {
        samples[j] = buf.readInt16LE(i + j * 2) / 32768;
      }
      return samples;
    }
    i += 8 + size;
  }
  throw new Error(`no data chunk in ${path}`);
}

// ── load inputs ───────────────────────────────────────────────────────
const words = JSON.parse(readFileSync(wordsPath, "utf8"));
const score = parseNp(readFileSync(scorePath, "utf8"));
const syllables = score[SECTION];
if (!syllables || !syllables.length) {
  console.error(`✗ section '${SECTION}' empty in ${scorePath}`);
  process.exit(1);
}

const refMidi = noteToMidi(REF_NOTE);
const beatSec = 60 / BPM;
const stepSec = beatSec * 4 / GRID; // 16th = beatSec / 4

const totalNaturalDur = probeDuration(vocalPath);

const tmpDir = `${dirname(OUT_PATH)}/.${basename(OUT_PATH).replace(/\..*$/, "")}-ps-tmp`;
rmSync(tmpDir, { recursive: true, force: true });
mkdirSync(tmpDir, { recursive: true });

// ── Pre-pass: measure source pitch across all words for "global" autotune ──
// Slice every word once with ffmpeg, run autocorrelation, take median.
// One pre-pass means the per-word loop below uses a stable reference.
let globalSourceMidi = null;
if (AUTOTUNE === "global") {
  const detected = [];
  for (let i = 0; i < words.length; i++) {
    const w = words[i];
    const startSec = w.fromMs / 1000;
    const endSec = i < words.length - 1 ? words[i + 1].fromMs / 1000 : totalNaturalDur;
    if (startSec >= totalNaturalDur - 0.005) continue;
    const safeEnd = Math.min(endSec, totalNaturalDur);
    const probeWav = `${tmpDir}/probe${i.toString().padStart(3, "0")}.wav`;
    spawnSync(
      "ffmpeg",
      ["-hide_banner", "-y", "-loglevel", "error",
       "-ss", startSec.toFixed(4), "-to", safeEnd.toFixed(4),
       "-i", vocalPath,
       "-c:a", "pcm_s16le", "-ar", String(SAMPLE_RATE), "-ac", "1", probeWav],
      { stdio: ["ignore", "ignore", "ignore"] }
    );
    if (!existsSync(probeWav)) continue;
    const samples = readWav(probeWav);
    const f = detectPitch(samples, SAMPLE_RATE, 80, 280);
    if (f !== null) detected.push(freqToMidi(f));
  }
  if (detected.length) {
    detected.sort((a, b) => a - b);
    globalSourceMidi = detected[Math.floor(detected.length / 2)];
    console.log(`  global source pitch: median MIDI ${globalSourceMidi.toFixed(2)} from ${detected.length} words`);
  } else {
    console.warn("  ! global autotune: no pitch detections — falling back to ref-relative shifts");
  }
}

console.log(
  `→ pitchsnap · ${words.length} whisper words → ${syllables.length} score syllables\n` +
  `  bpm=${BPM} grid=1/${GRID}-note (${(stepSec * 1000).toFixed(1)}ms) ref=${REF_NOTE} ` +
  `stretch=${STRETCH.toFixed(2)}× curve=${CURVE} autotune=${AUTOTUNE}`
);

// ── per-word: extract slice → pitch shift → record snapped start ─────
const slices = []; // { snappedStart, samples, text }
let maxEndSec = 0;
let sylCursor = 0; // syllable-aware mapping: advance by each word's syllable count

// Beat-mode pre-pass: walk the score syllable cursor exactly the same
// way the per-word loop will, so we can produce a beats-cumulative
// start time per word. Without this, words begin at speech-time and
// individually-stretched durations cause overlap.
let beatStarts = null;
if (BEAT_MODE) {
  beatStarts = [];
  let beats = 0;
  let cur = 0;
  const beatSec = 60 / BPM;
  for (let i = 0; i < words.length; i++) {
    beatStarts.push(beats * beatSec);
    const ws = syllableCount(words[i].text);
    let wordBeats = 0;
    for (let k = cur; k < cur + ws && k < syllables.length; k++) {
      const sw = syllables[k] && typeof syllables[k].weight === "number"
        ? syllables[k].weight : 1;
      wordBeats += sw;
    }
    beats += wordBeats;
    cur += ws;
  }
  console.log(`  beat-mode timeline: ${beats} beats total = ${(beats * beatSec).toFixed(2)}s @ ${BPM} BPM`);
}

for (let i = 0; i < words.length; i++) {
  const w = words[i];
  const naturalStart = w.fromMs / 1000;
  const naturalEnd = i < words.length - 1 ? words[i + 1].fromMs / 1000 : totalNaturalDur;

  // Snap target: in BEAT_MODE, place each word at its cumulative beat
  // position from the score. Otherwise preserve the speech timeline
  // (scaled by global STRETCH and snapped to grid).
  const snappedStart = beatStarts
    ? beatStarts[i]
    : Math.round((naturalStart * STRETCH) / stepSec) * stepSec;

  // Syllable-aware mapping: each word claims `syllableCount(word)`
  // entries from the score, starting at sylCursor. Use the FIRST
  // syllable's pitch as the word's main target; the LAST syllable's
  // pitch as the next-target for curve glide.
  const wordSyls = syllableCount(w.text);
  const startSylIdx = Math.min(syllables.length - 1, sylCursor);
  const endSylIdx = Math.min(syllables.length - 1, sylCursor + wordSyls - 1);
  sylCursor += wordSyls;

  const syl = syllables[startSylIdx];
  const noteStr = /\d/.test(syl.pitch) ? syl.pitch : syl.pitch + "3";
  const targetMidi = noteToMidi(noteStr) + TRANSPOSE;
  let semitones = targetMidi - refMidi;

  // Next-target for curve mode: end syllable of this word (intra-word
  // glide for multi-syllable words) or the first syllable of the next word.
  let nextSemitones = semitones;
  if (endSylIdx > startSylIdx) {
    // Multi-syllable word — glide to its own last syllable
    const lastSyl = syllables[endSylIdx];
    const lastNoteStr = /\d/.test(lastSyl.pitch) ? lastSyl.pitch : lastSyl.pitch + "3";
    nextSemitones = noteToMidi(lastNoteStr) - refMidi;
  } else if (sylCursor < syllables.length) {
    // Single-syllable word — glide toward next word's first syllable
    const nextSyl = syllables[Math.min(syllables.length - 1, sylCursor)];
    const nextNoteStr = /\d/.test(nextSyl.pitch) ? nextSyl.pitch : nextSyl.pitch + "3";
    nextSemitones = noteToMidi(nextNoteStr) - refMidi;
  }

  // Guard: skip words whose start has gone past the natural duration.
  if (naturalStart >= totalNaturalDur - 0.005) {
    console.warn(`  ! word ${i} (${w.text}) starts past audio end — skipping`);
    continue;
  }
  // Extend each word's slice end by 60ms so trailing consonants /
  // releases don't get cut. The next word will overlap slightly on
  // playback (handled by additive mixing) — better than chopped tails.
  const safeEnd = Math.min(naturalEnd + 0.06, totalNaturalDur);
  const sliceWav = `${tmpDir}/w${i.toString().padStart(3, "0")}.wav`;
  spawnSync(
    "ffmpeg",
    ["-hide_banner", "-y", "-loglevel", "error",
     "-ss", naturalStart.toFixed(4), "-to", safeEnd.toFixed(4),
     "-i", vocalPath,
     "-c:a", "pcm_s16le", "-ar", String(SAMPLE_RATE), "-ac", "1", sliceWav],
    { stdio: ["ignore", "ignore", "inherit"] }
  );
  if (!existsSync(sliceWav)) {
    console.warn(`  ! word ${i} (${w.text}) ffmpeg slice failed — skipping`);
    continue;
  }

  // Trim leading/trailing silence within the slice so WORLD analyses
  // only the actual word content. Whisper word boundaries can land
  // mid-vowel of the previous word or mid-consonant of the current
  // one; trimming on RMS-envelope at 5% of peak finds the real
  // start/end. Adds 8ms of padding either side so we don't cut into
  // attack transients.
  {
    const buf = readWav(sliceWav);
    const hop = Math.floor(0.010 * SAMPLE_RATE);
    const nFrames = Math.max(1, Math.floor(buf.length / hop));
    const env = new Float32Array(nFrames);
    let peakE = 0;
    for (let f = 0; f < nFrames; f++) {
      let r = 0;
      const a = f * hop;
      const b = Math.min(buf.length, a + hop);
      for (let j = a; j < b; j++) r += buf[j] * buf[j];
      env[f] = Math.sqrt(r / (b - a));
      if (env[f] > peakE) peakE = env[f];
    }
    if (peakE > 0.005) {
      const thr = peakE * 0.05;
      let s = 0; while (s < nFrames && env[s] < thr) s++;
      let e = nFrames - 1; while (e > s && env[e] < thr) e--;
      // ATTACK DETECTION (only for short slot allocations).
      // For words with allocated weight ≥ 3 beats (sustained notes
      // like 'found', 'see' on *5), keep the natural ramp-in intact
      // because rubberband needs that material to stretch into the
      // long sustain. Aggressively trimming a slow-attack 5-beat note
      // produces a clipped sustain — the word ends early in its slot
      // and the listener hears silence. Short words (1-2 beats) get
      // the full attack-detection trim so their attack lands on beat.
      const wordBeats = syllables
        .slice(startSylIdx, endSylIdx + 1)
        .reduce((sum, s2) => sum + (s2.weight || 1), 0);
      if (wordBeats <= 2) {
        const lookaheadFrames = Math.min(25, e - s);  // 250ms @ 10ms hop
        let maxRise = 0;
        let attackFrame = s;
        for (let af = s + 1; af < s + lookaheadFrames; af++) {
          const rise = env[af] - env[Math.max(s, af - 3)];
          if (rise > maxRise) {
            maxRise = rise;
            attackFrame = af;
          }
        }
        if (attackFrame - s > 1) {
          const preRollFrames = Math.max(0, Math.floor(0.015 / 0.010));
          s = Math.max(s, attackFrame - preRollFrames);
        }
      }
      const pad = Math.floor(0.008 * SAMPLE_RATE);
      const startSamp = Math.max(0, s * hop - pad);
      const endSamp = Math.min(buf.length, (e + 1) * hop + pad);
      if (endSamp > startSamp + Math.floor(0.030 * SAMPLE_RATE)) {
        // Write trimmed back to sliceWav so downstream steps see the cleaned cut.
        const trimmed = buf.slice(startSamp, endSamp);
        const sampleBytes = Buffer.alloc(trimmed.length * 2);
        for (let k = 0; k < trimmed.length; k++) {
          const v = Math.max(-1, Math.min(1, trimmed[k]));
          sampleBytes.writeInt16LE(Math.floor(v * 32767), k * 2);
        }
        // Minimal RIFF header for 48kHz mono 16-bit PCM
        const dataLen = sampleBytes.length;
        const header = Buffer.alloc(44);
        header.write("RIFF", 0); header.writeUInt32LE(36 + dataLen, 4);
        header.write("WAVE", 8); header.write("fmt ", 12);
        header.writeUInt32LE(16, 16); header.writeUInt16LE(1, 20);
        header.writeUInt16LE(1, 22); header.writeUInt32LE(SAMPLE_RATE, 24);
        header.writeUInt32LE(SAMPLE_RATE * 2, 28); header.writeUInt16LE(2, 32);
        header.writeUInt16LE(16, 34); header.write("data", 36);
        header.writeUInt32LE(dataLen, 40);
        writeFileSync(sliceWav, Buffer.concat([header, sampleBytes]));
      }
    }
  }

  // Autotune: shift to land ON the target note.
  //   - global: shift by (target − globalSourceMidi). Uniform across
  //     all words, robust against per-word f0-detection octave errors.
  //   - word:   shift by (target − measured per-word source). Most
  //     accurate when detection is clean, but susceptible to octave
  //     errors that produce audible jumps.
  let sourceMidi = null;
  if (AUTOTUNE === "global" && globalSourceMidi !== null) {
    sourceMidi = globalSourceMidi;
    semitones = targetMidi - sourceMidi;
  } else if (AUTOTUNE === "word") {
    const probeSamples = readWav(sliceWav);
    const sourceF = detectPitch(probeSamples, SAMPLE_RATE, 80, 280);
    if (sourceF !== null) {
      sourceMidi = freqToMidi(sourceF);
      semitones = targetMidi - sourceMidi;
    }
    // If pitch detection failed, fall back to ref-based shift.
  }

  // Waypoints describe the pitch curve through this word; consumed by
  // both the rubberband segmented rendering and the sine + tick overlay below.
  let waypoints = [];

  // Segmented rendering — chops the source slice into N equal pieces
  // and pitch-shifts each to its own target. Step pitch changes that
  // are audibly distinct per segment. Bypasses rubberband's --pitchmap
  // which produced ambiguous timing on this version.
  //
  // Notes for the segments come from one of:
  //   1. SCALE_WALK (CLI override, walks notes regardless of syllables)
  //   2. .np per-syllable pitches (multi-syllable word mapped to its
  //      syllable count's worth of score notes)
  let segNotes = null;
  let segWeights = null;
  if (SCALE_WALK && SCALE_WALK.length >= 2) {
    segNotes = SCALE_WALK.map((s) => /\d/.test(s) ? s : s + "3");
    segWeights = segNotes.map(() => 1);
  } else if (CURVE !== "flat" && (endSylIdx - startSylIdx + 1) >= 1) {
    // Populate for ALL words (1+ syllables) so single-syllable words
    // also flow through the WORLD / beat-mode path. Previously gated
    // on >= 2, which silently skipped half the lyric.
    segNotes = [];
    segWeights = [];
    for (let k = startSylIdx; k <= endSylIdx; k++) {
      const sylk = syllables[k];
      const baseNote = /\d/.test(sylk.pitch) ? sylk.pitch : sylk.pitch + "3";
      if (TRANSPOSE !== 0) {
        const m = noteToMidi(baseNote) + TRANSPOSE;
        const names = ["C","C#","D","Eb","E","F","F#","G","G#","A","Bb","B"];
        const oct = Math.floor(m / 12) - 1;
        const idx = ((m % 12) + 12) % 12;
        segNotes.push(`${names[idx]}${oct}`);
      } else {
        segNotes.push(baseNote);
      }
      segWeights.push(typeof sylk.weight === "number" ? sylk.weight : 1);
    }
  }

  // ── WORLD engine: replace f0 wholesale, no segmenting ────────────
  if (segNotes && segNotes.length >= 1 && ENGINE === "world") {
    // Compute per-word stretch.
    let perWordStretch = STRETCH;
    if (BEAT_MODE && segWeights) {
      const naturalWordDur = readWav(sliceWav).length / SAMPLE_RATE;
      const beatSec = 60 / BPM;
      const targetDur = segWeights.reduce((a, b) => a + b, 0) * beatSec;
      if (naturalWordDur > 0.01) {
        // Cap raised from 8× to 20× — ElevenLabs utterances of short
        // words like "see" are ~250-350ms naturally and need to fill
        // 4-5 beat sustain slots. With the old 8× ceiling, sustained
        // notes ended early and the listener heard silence after.
        perWordStretch = Math.max(0.5, Math.min(20.0, targetDur / naturalWordDur));
      }
      console.log(`  beat-mode · '${w.text}' ${segWeights.join("+")}b → ${targetDur.toFixed(2)}s @ ${perWordStretch.toFixed(2)}× stretch (nat=${naturalWordDur.toFixed(2)}s)`);
    }

    // Pre-stretch with rubberband (formant-preserving) so the stretched
    // wav is what WORLD analyses. This delivers per-word stretch on the
    // world engine path (WORLD itself doesn't stretch).
    let worldInputWav = sliceWav;
    if (Math.abs(perWordStretch - 1.0) >= 0.001) {
      worldInputWav = `${tmpDir}/w${i.toString().padStart(3,"0")}-stretched.wav`;
      const rb = spawnSync(
        "rubberband",
        ["-t", String(perWordStretch), sliceWav, worldInputWav],
        { stdio: ["ignore", "ignore", "ignore"] }
      );
      if (rb.status !== 0 || !existsSync(worldInputWav)) worldInputWav = sliceWav;
    }
    const pieceWavWorld = `${tmpDir}/w${i.toString().padStart(3,"0")}-world.wav`;
    const venvPython = resolve(POP_ROOT, ".venv/bin/python");
    const helperPath = resolve(POP_ROOT, "bin/pitchsnap_world.py");
    const args = [
      helperPath, worldInputWav, pieceWavWorld,
      "--notes", segNotes.join(","),
      "--retain", String(RETAIN),
    ];
    if (segWeights && segWeights.some((w) => w !== 1)) {
      args.push("--weights", segWeights.join(","));
    }
    if (DETECT_BOUNDARIES) args.push("--detect-boundaries");
    if (VIBRATO_HZ > 0) {
      args.push("--vibrato-hz", String(VIBRATO_HZ));
      args.push("--vibrato-cents", String(VIBRATO_CENTS));
    }
    const r = spawnSync(venvPython, args, { stdio: ["ignore", "inherit", "inherit"] });
    if (r.status !== 0 || !existsSync(pieceWavWorld)) {
      console.warn(`  ! word ${i} world engine failed — falling back to rubberband`);
    } else {
      // Build waypoints (for sine overlay + events trace)
      const refForShift = (AUTOTUNE === "global" && globalSourceMidi !== null)
        ? globalSourceMidi : (sourceMidi !== null ? sourceMidi : refMidi);
      const wlen = readWav(pieceWavWorld).length;
      for (let k = 0; k < segNotes.length; k++) {
        const midiK = noteToMidi(segNotes[k]);
        waypoints.push({
          sample: Math.floor((k / segNotes.length) * wlen),
          midi: midiK,
          semi: midiK - refForShift,
        });
      }
      let samples = readWav(pieceWavWorld);
      // Hard-trim each word to its beat allocation (BEAT_MODE) so
      // stretched WORLD output doesn't bleed into the next word's slot
      // and create overlap/echo. 30ms cosine fade-out at the trim point
      // smooths the cut. Without this, words that ended up slightly
      // longer than their beat target leaked tonal tail into following
      // words → audible echo on the single voice.
      if (BEAT_MODE && segWeights) {
        const beatSec = 60 / BPM;
        const allocatedSec = segWeights.reduce((a, b) => a + b, 0) * beatSec;
        const allocatedSamples = Math.floor(allocatedSec * SAMPLE_RATE);
        if (samples.length > allocatedSamples) {
          const trimmed = samples.slice(0, allocatedSamples);
          const fadeS = Math.min(Math.floor(0.030 * SAMPLE_RATE), Math.floor(trimmed.length / 8));
          for (let k = 0; k < fadeS; k++) {
            const j = trimmed.length - fadeS + k;
            const env = 0.5 + 0.5 * Math.cos((Math.PI * k) / fadeS);
            trimmed[j] *= env;
          }
          samples = trimmed;
        }
      }
      slices.push({
        snappedStart, samples, text: w.text, naturalStart,
        semitones, noteStr, targetMidi, sourceMidi, waypoints,
      });
      const endSec = snappedStart + samples.length / SAMPLE_RATE;
      if (endSec > maxEndSec) maxEndSec = endSec;
      continue;
    }
  }

  if (segNotes && segNotes.length >= 2) {
    const naturalSamples = readWav(sliceWav).length;
    const refForShift = (AUTOTUNE === "global" && globalSourceMidi !== null)
      ? globalSourceMidi
      : (sourceMidi !== null ? sourceMidi : refMidi);

    const N = segNotes.length;
    // Weighted segment boundaries — per-syllable `*weight` from the .np
    // score lets us hold longer notes (e.g. "a-MAAA-zing").
    const weights = (segWeights && segWeights.length === N) ? segWeights : segNotes.map(() => 1);
    const wTotal = weights.reduce((a, b) => a + b, 0) || N;
    const cumW = [0];
    for (let k = 0; k < N; k++) cumW.push(cumW[k] + weights[k] / wTotal);

    const segPieces = [];
    for (let k = 0; k < N; k++) {
      const noteAtK = segNotes[k];
      const midiAtK = noteToMidi(noteAtK);
      const semiAtK = midiAtK - refForShift;
      waypoints.push({
        sample: Math.floor(cumW[k] * naturalSamples * STRETCH),
        midi: midiAtK,
        semi: semiAtK,
      });

      // Slice source segment k by its weighted boundaries.
      const segStartSec = cumW[k]     * (naturalSamples / SAMPLE_RATE);
      const segEndSec   = cumW[k + 1] * (naturalSamples / SAMPLE_RATE);
      const segSrcWav = `${tmpDir}/w${i.toString().padStart(3,"0")}-seg${k}.wav`;
      spawnSync(
        "ffmpeg",
        ["-hide_banner", "-y", "-loglevel", "error",
         "-ss", segStartSec.toFixed(4), "-to", segEndSec.toFixed(4),
         "-i", sliceWav,
         "-c:a", "pcm_s16le", "-ar", String(SAMPLE_RATE), "-ac", "1", segSrcWav],
        { stdio: ["ignore", "ignore", "ignore"] }
      );
      if (!existsSync(segSrcWav)) continue;

      // Pitch-shift + stretch this segment.
      const segOutWav = `${tmpDir}/w${i.toString().padStart(3,"0")}-seg${k}-p.wav`;
      const rb = spawnSync(
        "rubberband",
        ["-p", String(semiAtK), "-t", String(STRETCH), segSrcWav, segOutWav],
        { stdio: ["ignore", "ignore", "ignore"] }
      );
      if (rb.status === 0 && existsSync(segOutWav)) segPieces.push(segOutWav);
      else segPieces.push(segSrcWav);
    }

    // Concat segments with overlap-add crossfade (20ms) so segment
    // boundaries don't click. Read each piece as samples, mix into a
    // running buffer with the tail of segment k overlapping the head
    // of segment k+1.
    const xfade = Math.floor(0.020 * SAMPLE_RATE);
    const segBufs = segPieces.map((p) => readWav(p));
    let totalLen = 0;
    for (const b of segBufs) totalLen += b.length;
    totalLen -= xfade * Math.max(0, segBufs.length - 1);
    const walked = new Float32Array(Math.max(0, totalLen) + xfade);
    let cursor = 0;
    for (let k = 0; k < segBufs.length; k++) {
      const b = segBufs[k];
      const startK = cursor;
      for (let j = 0; j < b.length; j++) {
        let env = 1;
        if (k > 0 && j < xfade) env = j / xfade;
        if (k < segBufs.length - 1 && j >= b.length - xfade) {
          env = Math.min(env, (b.length - j) / xfade);
        }
        const dst = startK + j;
        if (dst >= 0 && dst < walked.length) walked[dst] += b[j] * env;
      }
      cursor += b.length - xfade;
    }
    // Trim trailing zeros if we overcounted.
    let endIdx = walked.length;
    while (endIdx > 0 && Math.abs(walked[endIdx - 1]) < 1e-9) endIdx--;
    const samples = walked.slice(0, endIdx);
    slices.push({
      snappedStart, samples, text: w.text, naturalStart,
      semitones, noteStr, targetMidi, sourceMidi, waypoints,
    });
    const endSec = snappedStart + samples.length / SAMPLE_RATE;
    if (endSec > maxEndSec) maxEndSec = endSec;
    continue; // skip the rest of the per-word loop body for SCALE_WALK
  }

  let pieceWav = sliceWav;
  const needsPitch = Math.abs(semitones) >= 0.01 || (CURVE !== "flat" && Math.abs(nextSemitones - semitones) >= 0.01);
  const needsStretch = Math.abs(STRETCH - 1.0) >= 0.001;
  if (needsPitch || needsStretch) {
    const target = `${tmpDir}/w${i.toString().padStart(3, "0")}-p.wav`;
    const rbArgs = [];

    // (waypoints declared at the per-word scope below)
    if (CURVE !== "flat") {
      const sliceSamples = readWav(sliceWav).length;
      const stretchedSamples = Math.floor(sliceSamples * STRETCH);

      const refForShift = (AUTOTUNE === "global" && globalSourceMidi !== null)
        ? globalSourceMidi
        : (sourceMidi !== null ? sourceMidi : refMidi);

      if (SCALE_WALK && SCALE_WALK.length >= 2) {
        for (let k = 0; k < SCALE_WALK.length; k++) {
          const noteAtK = /\d/.test(SCALE_WALK[k]) ? SCALE_WALK[k] : SCALE_WALK[k] + "3";
          const midiAtK = noteToMidi(noteAtK);
          const sample = Math.floor((k / (SCALE_WALK.length - 1)) * stretchedSamples);
          waypoints.push({ sample, midi: midiAtK, semi: midiAtK - refForShift });
        }
      } else if (CURVE === "linear" && (endSylIdx - startSylIdx + 1) >= 2) {
        const sylsInWord = endSylIdx - startSylIdx + 1;
        for (let k = 0; k < sylsInWord; k++) {
          const sylAtK = syllables[startSylIdx + k];
          const noteAtK = /\d/.test(sylAtK.pitch) ? sylAtK.pitch : sylAtK.pitch + "3";
          const midiAtK = noteToMidi(noteAtK);
          const sample = Math.floor((k / (sylsInWord - 1)) * stretchedSamples);
          waypoints.push({ sample, midi: midiAtK, semi: midiAtK - refForShift });
        }
      } else if (CURVE === "linear") {
        waypoints.push({ sample: 0, midi: targetMidi, semi: semitones });
        const nextMidi = (sylCursor < syllables.length)
          ? noteToMidi(/\d/.test(syllables[Math.min(syllables.length - 1, sylCursor)].pitch)
              ? syllables[Math.min(syllables.length - 1, sylCursor)].pitch
              : syllables[Math.min(syllables.length - 1, sylCursor)].pitch + "3")
          : targetMidi;
        waypoints.push({ sample: stretchedSamples, midi: nextMidi, semi: nextSemitones });
      } else if (CURVE === "bezier") {
        const fifthOffset = 7;
        const midSemi = semitones + Math.sign(nextSemitones - semitones || 1) * (fifthOffset / 4);
        waypoints.push({ sample: 0, midi: targetMidi, semi: semitones });
        waypoints.push({ sample: Math.floor(stretchedSamples / 2), midi: targetMidi + midSemi - semitones, semi: midSemi });
        waypoints.push({ sample: stretchedSamples, midi: targetMidi + nextSemitones - semitones, semi: nextSemitones });
      }

      const pitchmap = `${tmpDir}/w${i.toString().padStart(3, "0")}.pmap`;
      writeFileSync(pitchmap, waypoints.map((w) => `${w.sample} ${w.semi.toFixed(3)}`).join("\n") + "\n");
      rbArgs.push("--pitchmap", pitchmap);
      rbArgs.push("-t", String(STRETCH));
    } else {
      if (needsPitch) rbArgs.push("-p", String(semitones));
      if (needsStretch) rbArgs.push("-t", String(STRETCH));
    }

    rbArgs.push(sliceWav, target);
    const r = spawnSync("rubberband", rbArgs, { stdio: ["ignore", "ignore", "ignore"] });
    if (r.status === 0 && existsSync(target)) {
      pieceWav = target;
    } else {
      // rubberband can fail on very short slices — fall back to natural.
      console.warn(`  ! word ${i} (${w.text}) rubberband fell back to natural slice`);
    }
  }

  const samples = readWav(pieceWav);
  slices.push({
    snappedStart, samples, text: w.text, naturalStart,
    semitones, noteStr, targetMidi, sourceMidi,
    waypoints, // for sine overlay + events trace
  });
  const endSec = snappedStart + samples.length / SAMPLE_RATE;
  if (endSec > maxEndSec) maxEndSec = endSec;
}

// ── assemble buffer ───────────────────────────────────────────────────
const totalSamples = Math.ceil((maxEndSec + 0.5) * SAMPLE_RATE);
const out = new Float32Array(totalSamples);

// Word-boundary fade — 5ms cosine in/out per word slice. Just enough
// to prevent splice clicks; longer fade-ins were softening attacks of
// already-trimmed words, defeating the perceptual-onset alignment in
// the per-word silence trim above. The LAST word doesn't fade out so
// the song resolves naturally on its final note.
const wordFadeS = Math.floor(0.005 * SAMPLE_RATE);
for (let sIdx = 0; sIdx < slices.length; sIdx++) {
  const s = slices[sIdx];
  const isLast = sIdx === slices.length - 1;
  const startIdx = Math.floor(s.snappedStart * SAMPLE_RATE);
  const len = s.samples.length;
  const fadeIn = Math.min(wordFadeS, Math.floor(len / 8));
  const fadeOut = isLast ? 0 : Math.min(wordFadeS, Math.floor(len / 8));
  const sustainEnd = len - fadeOut;
  for (let i = 0; i < len; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;
    let env = 1;
    if (i < fadeIn) env = 0.5 - 0.5 * Math.cos((Math.PI * i) / fadeIn);
    else if (fadeOut > 0 && i >= sustainEnd) env = 0.5 - 0.5 * Math.cos((Math.PI * (len - i)) / fadeOut);
    out[dst] += s.samples[i] * env;
  }
}

// Normalize to ~ -3 dBFS peak.
let peak = 0;
for (let i = 0; i < out.length; i++) {
  const a = Math.abs(out[i]);
  if (a > peak) peak = a;
}
if (peak > 0) {
  const norm = 0.85 / peak;
  for (let i = 0; i < out.length; i++) out[i] *= norm;
}

// Write float32 raw + ffmpeg → mp3.
const rawPath = `${tmpDir}/out.f32.raw`;
const buf = Buffer.alloc(out.length * 4);
for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
writeFileSync(rawPath, buf);

const ff = spawnSync(
  "ffmpeg",
  ["-hide_banner", "-y", "-loglevel", "error",
   "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
   "-i", rawPath,
   "-c:a", "libmp3lame", "-q:a", "3", OUT_PATH],
  { stdio: "inherit" }
);
if (ff.status !== 0) {
  console.error("✗ ffmpeg encode failed");
  process.exit(1);
}

// ── summary ───────────────────────────────────────────────────────────
console.log(`\n  ${"i".padStart(3)} ${"word".padEnd(14)} ${"natural→snapped".padEnd(20)} pitch`);
console.log(`  ${"─".repeat(55)}`);
for (let i = 0; i < slices.length; i++) {
  const s = slices[i];
  const arrow = s.semitones >= 0 ? "+" : "";
  const drift = s.snappedStart - s.naturalStart;
  const driftMs = Math.round(drift * 1000);
  const driftStr = `${s.naturalStart.toFixed(2)}→${s.snappedStart.toFixed(2)}s (${driftMs >= 0 ? "+" : ""}${driftMs}ms)`;
  console.log(
    `  ${i.toString().padStart(3)} ${s.text.padEnd(14)} ${driftStr.padEnd(20)} ${s.noteStr} ${arrow}${s.semitones} st`
  );
}

// ── Optional: render a sine reference under the vocal ─────────────────
// Pure sine at each event's target pitch for the event's duration,
// gated by a gentle envelope. Mixed at -12 dB by default. Useful for
// auditing whether pitch shifts landed — you should hear the vocal
// "tracking" the sine. Set --sine-overlay 0 to mute (still emits the
// file), or --sine-overlay <gain> to override (e.g. 0.3 = -10 dB).
const SINE_OVERLAY = flags["sine-overlay"] !== undefined;
if (SINE_OVERLAY) {
  const sineGain = flags["sine-overlay"] === true ? 0.25 : Number(flags["sine-overlay"]);
  const sineBuf = new Float32Array(totalSamples);
  const twoPiOverSr = (2 * Math.PI) / SAMPLE_RATE;
  const ATTACK_SEC = 0.015;
  const RELEASE_SEC = 0.060;
  const sineXfade = Math.floor(0.030 * SAMPLE_RATE); // 30ms
  for (const s of slices) {
    const startIdx = Math.floor(s.snappedStart * SAMPLE_RATE);
    const len = s.samples.length;

    const wps = (s.waypoints && s.waypoints.length >= 2)
      ? s.waypoints.map((w) => ({ pos: Math.min(len, w.sample), midi: w.midi }))
      : [{ pos: 0, midi: s.targetMidi }, { pos: len, midi: s.targetMidi }];

    // Sine reference — phase-continuous, with linear pitch crossfade
    // in a 30ms window centered on each waypoint boundary. The pitch
    // glides smoothly between adjacent held notes so there are no
    // glitches/clicks. Outer attack/release envelope applies only at
    // the very start/end of the whole word.
    let phase = 0;
    let segIdx = 0;
    const wordAtt = Math.floor(ATTACK_SEC * SAMPLE_RATE);
    const wordRel = Math.floor(RELEASE_SEC * SAMPLE_RATE);
    const wordSustainEnd = len - wordRel;
    for (let i = 0; i < len; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= sineBuf.length) continue;
      while (segIdx + 1 < wps.length && i >= wps[segIdx + 1].pos) segIdx++;
      const cur = wps[segIdx];
      // Determine pitch with optional crossfade near the next waypoint.
      let midiNow = cur.midi;
      const nxt = wps[segIdx + 1];
      if (nxt) {
        const distToNext = nxt.pos - i;
        if (distToNext < sineXfade) {
          const t = 1 - distToNext / sineXfade;
          midiNow = cur.midi * (1 - t) + nxt.midi * t;
        }
      }
      const freq = 440 * Math.pow(2, (midiNow - 69) / 12);
      phase += twoPiOverSr * freq;

      let env;
      if (i < wordAtt) env = i / wordAtt;
      else if (i >= wordSustainEnd) env = Math.max(0, (len - i) / wordRel);
      else env = 1;
      sineBuf[dst] += Math.sin(phase) * env * sineGain;
    }

    // Ticks at each waypoint position so the listener can HEAR the
    // pitch grid changing. Loud and unambiguous — 1 kHz tone, 50 ms,
    // sharp exponential decay. Amplitude clipped to 0.95 so it always
    // pokes through the mix.
    const tickLen = Math.floor(0.050 * SAMPLE_RATE);
    for (const wp of wps) {
      const tickStart = startIdx + wp.pos;
      for (let j = 0; j < tickLen; j++) {
        const dst = tickStart + j;
        if (dst < 0 || dst >= sineBuf.length) continue;
        const env = Math.exp(-j / (tickLen * 0.20));
        const tone = Math.sin(2 * Math.PI * 1000 * j / SAMPLE_RATE);
        sineBuf[dst] += tone * env * 0.95;
      }
    }
  }

  // Diagnostic: write the sine + tick layer alone so it can be
  // auditioned without the vocal masking it.
  const refOut = OUT_PATH.replace(/\.mp3$/, "-ref.mp3");
  const refRaw = `${tmpDir}/ref.f32.raw`;
  let refPeak = 0;
  for (let i = 0; i < sineBuf.length; i++) {
    const a = Math.abs(sineBuf[i]);
    if (a > refPeak) refPeak = a;
  }
  const refScale = refPeak > 0 ? 0.9 / refPeak : 1;
  const refBuf = Buffer.alloc(sineBuf.length * 4);
  for (let i = 0; i < sineBuf.length; i++) refBuf.writeFloatLE(sineBuf[i] * refScale, i * 4);
  writeFileSync(refRaw, refBuf);
  spawnSync(
    "ffmpeg",
    ["-hide_banner", "-y", "-loglevel", "error",
     "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
     "-i", refRaw,
     "-c:a", "libmp3lame", "-q:a", "3", refOut],
    { stdio: "inherit" }
  );
  console.log(`  sine+tick reference: ${refOut}`);
  // Mix sineBuf into out (additively).
  for (let i = 0; i < out.length && i < sineBuf.length; i++) out[i] += sineBuf[i];
  // Re-normalize because we just added energy.
  let p2 = 0;
  for (let i = 0; i < out.length; i++) {
    const a = Math.abs(out[i]);
    if (a > p2) p2 = a;
  }
  if (p2 > 0) {
    const norm = 0.85 / p2;
    for (let i = 0; i < out.length; i++) out[i] *= norm;
  }
  console.log(`  sine overlay: enabled at gain ${sineGain.toFixed(2)}`);
}

// Emit an events file alongside the mp3 so pitchcheck.mjs and other
// downstream tools can compare measured pitch vs intended target
// without re-aligning the rendered output (whisper degrades on
// heavily-shifted audio).
const eventsPath = OUT_PATH.replace(/\.mp3$/, ".events.json");
const eventsObj = {
  source: vocalPath,
  score: scorePath,
  section: SECTION,
  bpm: BPM,
  grid: GRID,
  refNote: REF_NOTE,
  refMidi,
  stretch: STRETCH,
  curve: CURVE,
  totalDur: maxEndSec,
  events: slices.map((s, i) => ({
    i,
    text: s.text,
    naturalStart: s.naturalStart,
    snappedStart: s.snappedStart,
    durSec: s.samples.length / SAMPLE_RATE,
    targetNote: s.noteStr,
    targetMidi: s.targetMidi,
    targetFreq: 440 * Math.pow(2, (s.targetMidi - 69) / 12),
    sourceMidi: s.sourceMidi,
    semitones: s.semitones,
  })),
};
writeFileSync(eventsPath, JSON.stringify(eventsObj, null, 2));

rmSync(tmpDir, { recursive: true, force: true });
console.log(`\n✓ ${OUT_PATH} · ${maxEndSec.toFixed(2)}s`);
console.log(`  events: ${eventsPath}`);
