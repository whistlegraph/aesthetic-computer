#!/usr/bin/env node
// melody-bells.mjs — render the .np score as a melodic sinebells line.
//
// The waltz bed (recap/bin/waltz.mjs) only carries chord roots + a
// random ornament; it doesn't play THE melody. This script reads the
// notepat score and renders each syllable as a struck bell at the
// score's note (with optional --transpose), so the accompaniment
// doubles the New Britain tune above the vocal.
//
// Bell synth lifted verbatim from recap/bin/waltz.mjs (sinebells voice)
// — fundamental + lightly inharmonic partials, cosine attack, T60 decay.
//
// Usage:
//   node bin/melody-bells.mjs --slug amazing --section all \
//        --transpose 0 --bpm 70 --inter-verse-beats 2 \
//        --gain 0.35 --out big-pictures/out/amazing-melody-bells.mp3

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { tmpdir } from "node:os";
import { alignWords } from "./align-words.mjs";

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else flags[a.slice(2)] = next;
}

const POP = "/Users/jas/aesthetic-computer/pop";
const SLUG = flags.slug || "amazing";
const SCORE_PATH = `${POP}/big-pictures/${SLUG}.np`;
const SECTION = (flags.section || "all").toLowerCase();
const BPM = Number(flags.bpm ?? 70);
const TRANSPOSE = Number(flags.transpose ?? 0);
const INTER_VERSE_BEATS = Number(flags["inter-verse-beats"] ?? 2);
const GAIN = Number(flags.gain ?? 0.35);
// Voice character:
//   bell — sinebells with inharmonic partials + sharp attack (default —
//          the same synth recap/bin/waltz.mjs uses for the waltz bed)
//   pad  — soft sine pad: fundamental + octave only, slow cosine attack,
//          gentle release. No "ding". Better for sustained accompaniment.
const VOICE = (flags.voice || "bell").toLowerCase();
const OUT_PATH = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}-melody-bells.mp3`;

const SAMPLE_RATE = 48_000;

const NOTE_BASE = { C:0,"C#":1,DB:1,D:2,"D#":3,EB:3,E:4,F:5,
                    "F#":6,GB:6,G:7,"G#":8,AB:8,A:9,"A#":10,BB:10,B:11 };
function noteToMidi(s) {
  s = s.toUpperCase();
  const oct = parseInt(s.slice(-1), 10);
  return 12 * (oct + 1) + NOTE_BASE[s.slice(0, -1)];
}

// ── parse score (mirrors score-render.mjs --section all path) ────────
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
  const headers = [];
  for (let i = 0; i < scoreLines.length; i++) {
    if (/^verse \d+$/i.test(scoreLines[i].trim())) headers.push(i);
  }
  headers.forEach((h, idx) => {
    const before = syllables.length;
    parseRange(h + 1);
    if (idx < headers.length - 1 && syllables.length > before && INTER_VERSE_BEATS > 0) {
      syllables[syllables.length - 1].weight += INTER_VERSE_BEATS;
    }
  });
} else {
  const sStart = scoreLines.findIndex((l) => l.trim().toLowerCase() === SECTION);
  if (sStart < 0) { console.error(`✗ section missing: ${SECTION}`); process.exit(1); }
  parseRange(sStart + 1);
}

// Build word-level event list. Two timing modes:
//
//   1. score-driven (default): hyphenated tail merges into head word;
//      each strike fires at beat_pos * beat_s.
//   2. whisper-driven (--whisper-words file.json): one strike per
//      whisper word at its fromMs, using the score's per-word target
//      note. Length-aligned by index when counts differ.
const events = [];
const WHISPER_WORDS = flags["whisper-words"]
  ? resolve(process.cwd(), flags["whisper-words"])
  : null;

if (WHISPER_WORDS) {
  // Group syllables → score-words, keeping per-syllable notes + weights
  // so multi-note words ("amazing" D3-G3-B3, "above" G4-F4) get a
  // strike per syllable distributed across the whisper word window.
  const scoreWords = [];
  let cur = null;
  for (const s of syllables) {
    if (s.raw.startsWith("-") && cur) {
      cur.notes.push(s.note);
      cur.weights.push(s.weight);
    } else {
      if (cur) scoreWords.push(cur);
      cur = { notes: [s.note], weights: [s.weight] };
    }
  }
  if (cur) scoreWords.push(cur);
  const whisper = JSON.parse(readFileSync(WHISPER_WORDS, "utf8"));
  // Reconcile contractions ("twas" ≠ "to as", "tis" ≠ "to his") via
  // the shared aligner so each strike fires on the right word.
  const tokens = [];
  let acc = "";
  for (const s of syllables) {
    if (s.raw.startsWith("-")) acc += s.raw.replace(/-/g, "");
    else { if (acc) tokens.push(acc); acc = s.raw.replace(/-/g, ""); }
  }
  if (acc) tokens.push(acc);
  const aligned = alignWords(tokens, whisper);
  for (let i = 0; i < scoreWords.length; i++) {
    const w = scoreWords[i];
    const winMs = Math.max(40, aligned[i].toMs - aligned[i].fromMs);
    const totalWeight = w.weights.reduce((x, y) => x + y, 0);
    let cum = 0;
    for (let s = 0; s < w.notes.length; s++) {
      const startSec = (aligned[i].fromMs + (cum / totalWeight) * winMs) / 1000;
      const durSec = (w.weights[s] / totalWeight) * winMs / 1000;
      const midi = noteToMidi(w.notes[s]) + TRANSPOSE;
      events.push({ startSec, midi, durSec, gain: GAIN });
      cum += w.weights[s];
    }
  }
  var totalSec = whisper[whisper.length - 1].toMs / 1000;
  console.log(`→ ${events.length} bell strikes · ${totalSec.toFixed(1)}s · whisper-driven · transpose=${TRANSPOSE}st`);
} else {
  // Score-driven: ONE strike per syllable so multi-note words ring out
  // every note. (Was previously skipping continuation syllables — that
  // collapsed melismas to single tones.)
  const beat_s = 60.0 / BPM;
  let beat_pos = 0;
  for (const s of syllables) {
    const startSec = beat_pos * beat_s;
    const durSec = s.weight * beat_s;
    const midi = noteToMidi(s.note) + TRANSPOSE;
    events.push({ startSec, midi, durSec, gain: GAIN });
    beat_pos += s.weight;
  }
  var totalSec = beat_pos * beat_s;
  console.log(`→ ${events.length} strikes · ${totalSec.toFixed(1)}s · score-driven · transpose=${TRANSPOSE}st`);
}

// ── voice presets ────────────────────────────────────────────────────
// bell — sinebells with inharmonic clang (recap/bin/waltz.mjs synth)
// pad  — soft sine pad: fundamental + octave, slow attack, gentle
//        plateau, slow release. No "ding". Sustained accompaniment.
const BELL_PARTIALS = [
  { ratio: 0.5,  amp: 0.28, decayT60: 5.5 },
  { ratio: 1.0,  amp: 1.00, decayT60: 4.5 },
  { ratio: 2.0,  amp: 0.32, decayT60: 2.6 },
  { ratio: 2.4,  amp: 0.10, decayT60: 1.2 },
  { ratio: 3.0,  amp: 0.09, decayT60: 1.0 },
  { ratio: 4.5,  amp: 0.04, decayT60: 0.6 },
  { ratio: 5.4,  amp: 0.02, decayT60: 0.4 },
];
const PAD_PARTIALS = [
  { ratio: 1.0, amp: 1.00 },   // fundamental
  { ratio: 2.0, amp: 0.18 },   // octave (gentle warmth)
];

const RING_TAIL_BELL = 6.0;
const RING_TAIL_PAD  = 1.2;
const BELL_GAIN      = 0.42;
const PAD_GAIN       = 0.55;

function midiToFreq(m) { return 440 * Math.pow(2, (m - 69) / 12); }

const ringTail = VOICE === "pad" ? RING_TAIL_PAD : RING_TAIL_BELL;
const lenSec = totalSec + ringTail + 1.0;
const out = new Float32Array(Math.ceil(lenSec * SAMPLE_RATE));

for (const ev of events) {
  const startIdx = Math.floor(ev.startSec * SAMPLE_RATE);
  const fundFreq = midiToFreq(ev.midi);
  const twoPiOverSr = (2 * Math.PI) / SAMPLE_RATE;

  if (VOICE === "pad") {
    // ADSR-style pad: 60ms cosine attack → flat sustain (event dur) →
    // 250ms cosine release. Pure sine + octave. No ding.
    const ATTACK_S  = 0.060;
    const RELEASE_S = 0.250;
    const attackSamp  = Math.floor(ATTACK_S * SAMPLE_RATE);
    const sustainSamp = Math.floor(ev.durSec * SAMPLE_RATE);
    const releaseSamp = Math.floor(RELEASE_S * SAMPLE_RATE);
    const totalSamp = attackSamp + sustainSamp + releaseSamp;
    const partials = PAD_PARTIALS.map((p) => ({
      omega: twoPiOverSr * fundFreq * p.ratio,
      amp: p.amp,
    }));
    for (let i = 0; i < totalSamp; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= out.length) continue;
      let env;
      if (i < attackSamp) {
        env = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackSamp);
      } else if (i < attackSamp + sustainSamp) {
        env = 1.0;
      } else {
        const r = i - (attackSamp + sustainSamp);
        env = 0.5 + 0.5 * Math.cos((Math.PI * r) / releaseSamp);
      }
      let s = 0;
      for (const p of partials) s += Math.sin(p.omega * i) * p.amp;
      out[dst] += s * env * ev.gain * PAD_GAIN;
    }
  } else {
    // bell (default) — original sinebells decay synthesis
    const ringSamples = Math.floor((ev.durSec + ringTail) * SAMPLE_RATE);
    const ATTACK_SEC = 0.012;
    const attackS = ATTACK_SEC * SAMPLE_RATE;
    const partials = BELL_PARTIALS.map((p) => ({
      omega: twoPiOverSr * fundFreq * p.ratio,
      amp: p.amp,
      decay: Math.exp(-Math.log(1000) / (p.decayT60 * SAMPLE_RATE)),
    }));
    for (let i = 0; i < ringSamples; i++) {
      const dst = startIdx + i;
      if (dst < 0 || dst >= out.length) continue;
      let s = 0;
      for (const p of partials) {
        const env = p.amp * Math.pow(p.decay, i);
        if (env < 1e-5) continue;
        s += Math.sin(p.omega * i) * env;
      }
      let att = 1;
      if (i < attackS) att = 0.5 - 0.5 * Math.cos((Math.PI * i) / attackS);
      out[dst] += s * att * ev.gain * BELL_GAIN;
    }
  }
}

// peak normalize to -1.5 dBFS
let peak = 0;
for (let i = 0; i < out.length; i++) if (Math.abs(out[i]) > peak) peak = Math.abs(out[i]);
const tgt = Math.pow(10, -1.5 / 20);
const norm = peak > 0 ? Math.min(1, tgt / peak) : 1;
if (norm < 1) for (let i = 0; i < out.length; i++) out[i] *= norm;

// ── write out via ffmpeg (raw f32 → mp3) ─────────────────────────────
const tmp = mkdtempSync(`${tmpdir()}/melody-bells-`);
const rawPath = `${tmp}/bells.f32.raw`;
writeFileSync(rawPath, Buffer.from(out.buffer, out.byteOffset, out.byteLength));
const r = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
  "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "2",
  OUT_PATH,
]);
rmSync(tmp, { recursive: true, force: true });
if (r.status !== 0) { console.error("✗ ffmpeg encode failed"); process.exit(1); }
console.log(`✓ ${OUT_PATH}  (peak norm ${norm.toFixed(3)} · ${events.length} strikes)`);
