#!/usr/bin/env node
// vocal.mjs — AC-native formant-synthesized singing voice. EXPERIMENTAL.
//
// Status (2026-05-03): smoke-tested, dropped from the big-pictures
// vocal pipeline. The 3-formant synth at this fidelity reads as
// "melodic tones," not voice — getting real vocoder/talkbox character
// would need glottal pulse + F4/F5 + pitch jitter + breath +
// consonant articulation, a real research lane. Big-pictures vocal
// now routes through jeffrey-pvc via /api/say (see pop/RESEARCH-DIRECTION.md).
//
// This file stays in the repo as an experimental lane — may resurface
// as a *melodic instrument* layer (formant-shaped lead) rather than
// as a vocal substitute.
//
// Bottom-up vocal: same primitive bag as percussion.mjs. A sawtooth
// "glottal" source (rich harmonics, 1/n falloff) is shaped through 3
// parallel 2-pole resonant filters tuned to a vowel's formant
// frequencies (F1/F2/F3), gated by a per-syllable envelope. Renders
// deterministically into a Float32 buffer and ffmpeg → mp3.
//
// Input: a phrase as [{ syl, vowel, pitch, dur, level? }, ...]
//   vowel: key of VOWELS map (ah / eh / ih / ee / oh / oo / uh)
//   pitch: Hz, MIDI number, or note string ("C3", "Eb3", "F#4")
//   dur:   seconds
//
// Smoke run (no args): renders "the music is real" on a 5-note pitch
// curve to recap/out/vocal.mp3.
//
// Score mode: --score <path>.np reads a notepat-format score (matches
// papers/arxiv-folk-songs/folk-songs.tex §3) and renders the named
// section (default "hook"). Per-syllable vowels are inferred from the
// syllable text. Per-syllable durations are computed by splitting a
// bar's worth of time across each line, with the line-final syllable
// getting 1.5× weight.
//
// Usage:
//   node bin/vocal.mjs                                 # smoke phrase
//   node bin/vocal.mjs --out ~/Desktop/vocal.mp3
//   node bin/vocal.mjs --over out/trap.mp3             # mix over trap bed
//   node bin/vocal.mjs --score ../pop/big-pictures/plork.np \
//        --section hook --bpm 140 --octave 3 --over out/trap.mp3

import {
  writeFileSync, readFileSync, mkdirSync, unlinkSync, existsSync,
} from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");

const SAMPLE_RATE = 48_000;

// ── Vowel formants (Hz) — F1, F2, F3 with bandwidths and amps ────────
// Approximate male/neutral speaker. Tweakable per syllable via override.
const VOWELS = {
  ah: { F: [730, 1090, 2440], BW: [60, 90, 150], A: [1.0, 0.6, 0.3] }, // father, ah
  eh: { F: [530, 1840, 2480], BW: [60, 90, 150], A: [1.0, 0.7, 0.3] }, // bet, eh
  ih: { F: [390, 1990, 2550], BW: [50, 90, 150], A: [1.0, 0.7, 0.4] }, // bit, ih
  ee: { F: [270, 2290, 3010], BW: [50, 100, 150], A: [1.0, 0.8, 0.4] }, // beat, real
  oh: { F: [570,  840, 2410], BW: [60, 80, 150], A: [1.0, 0.5, 0.2] }, // boat
  oo: { F: [300,  870, 2240], BW: [50, 80, 150], A: [1.0, 0.4, 0.2] }, // boot, music
  uh: { F: [500, 1500, 2500], BW: [60, 90, 150], A: [1.0, 0.5, 0.25] }, // schwa, the
};

// ── Smoke phrase: "the music is real" ────────────────────────────────
const SMOKE = [
  { syl: "the",  vowel: "uh", pitch: "C3",  dur: 0.18 },
  { syl: "mu",   vowel: "oo", pitch: "D3",  dur: 0.18 },
  { syl: "sic",  vowel: "ih", pitch: "Eb3", dur: 0.22 },
  { syl: "is",   vowel: "ih", pitch: "D3",  dur: 0.20 },
  { syl: "real", vowel: "ee", pitch: "C3",  dur: 0.55 },
];

// ── Pitch parsing: Hz / MIDI / note name ──────────────────────────────
const NOTE_TO_SEMI = { c: 0, d: 2, e: 4, f: 5, g: 7, a: 9, b: 11 };
function midiToFreq(midi) { return 440 * Math.pow(2, (midi - 69) / 12); }

function parsePitch(p) {
  if (typeof p === "number") return p > 20 ? p : midiToFreq(p);
  if (typeof p !== "string") return 130.81;
  const m = p.trim().toLowerCase().match(/^([a-g])([#b]?)(-?\d+)$/);
  if (!m) {
    const n = Number(p);
    return Number.isFinite(n) ? (n > 20 ? n : midiToFreq(n)) : 130.81;
  }
  let semi = NOTE_TO_SEMI[m[1]];
  if (m[2] === "#") semi += 1;
  if (m[2] === "b") semi -= 1;
  const oct = parseInt(m[3], 10);
  const midi = 12 * (oct + 1) + semi;
  return midiToFreq(midi);
}

// ── Render one syllable ───────────────────────────────────────────────
// Sawtooth source → 3 parallel 2-pole resonant filters → sum.
// Filter: y[n] = gain*x[n] + a*y[n-1] + b*y[n-2]
//   r = exp(-π*BW/sr), a = 2*r*cos(2π*fc/sr), b = -r²
//   gain = 1 - r² to normalize peak.
// Envelope: 12ms attack, hold, 30ms release.
function renderSyllable(syl, out, startSec) {
  const vowel = VOWELS[syl.vowel] || VOWELS.uh;
  const freq = parsePitch(syl.pitch);
  const dur = syl.dur ?? 0.2;
  const level = syl.level ?? 1.0;

  const startIdx = Math.floor(startSec * SAMPLE_RATE);
  const totalSamples = Math.ceil(dur * SAMPLE_RATE);
  const attackSamples = Math.min(Math.floor(totalSamples / 3), Math.floor(0.012 * SAMPLE_RATE));
  const releaseSamples = Math.min(Math.floor(totalSamples / 3), Math.floor(0.030 * SAMPLE_RATE));
  const sustainEnd = totalSamples - releaseSamples;

  const phaseInc = freq / SAMPLE_RATE;
  let phase = 0;

  const filters = vowel.F.map((fc, i) => {
    const bw = vowel.BW[i];
    const r = Math.exp(-Math.PI * bw / SAMPLE_RATE);
    const a = 2 * r * Math.cos(2 * Math.PI * fc / SAMPLE_RATE);
    const b = -r * r;
    const gain = 1 - r * r;
    return { a, b, gain, amp: vowel.A[i], y1: 0, y2: 0 };
  });

  for (let i = 0; i < totalSamples; i++) {
    const dst = startIdx + i;
    if (dst < 0 || dst >= out.length) continue;

    phase += phaseInc;
    if (phase >= 1) phase -= 1;
    const src = 2 * phase - 1;

    let s = 0;
    for (const f of filters) {
      const y = f.gain * src + f.a * f.y1 + f.b * f.y2;
      f.y2 = f.y1;
      f.y1 = y;
      s += y * f.amp;
    }

    let env;
    if (i < attackSamples) env = i / attackSamples;
    else if (i >= sustainEnd) env = Math.max(0, (totalSamples - i) / releaseSamples);
    else env = 1;

    out[dst] += s * env * level;
  }
}

function renderPhrase(phrase) {
  const total = phrase.reduce((t, s) => t + (s.dur ?? 0.2), 0);
  const samples = Math.ceil((total + 0.3) * SAMPLE_RATE);
  const out = new Float32Array(samples);
  let t = 0;
  for (const syl of phrase) {
    renderSyllable(syl, out, t);
    t += syl.dur ?? 0.2;
  }
  return out;
}

// ── notepat (.np) score parser ────────────────────────────────────────
// Format (folk-songs paper §3): NOTE:syllable whitespace-separated.
// Hyphens mark syllable continuation. Section headers are lowercase
// lines without colons. Comments start with '#'.
function parseNp(text) {
  const sections = { _order: [] };
  let current = null;
  const lines = text.split("\n");
  for (const raw of lines) {
    const line = raw.trim();
    if (!line || line.startsWith("#")) continue;
    // Section header: lowercase, no colon, words/digits/spaces only
    if (!line.includes(":") && /^[a-z][a-z0-9 ]*$/.test(line)) {
      current = line;
      if (!sections[current]) {
        sections[current] = [];
        sections._order.push(current);
      }
      continue;
    }
    if (!current) {
      current = "default";
      if (!sections[current]) {
        sections[current] = [];
        sections._order.push(current);
      }
    }
    const tokens = line.split(/\s+/).filter(Boolean);
    const lineSyllables = [];
    for (const tok of tokens) {
      const m = tok.match(/^([A-Ga-g][#b]?\d?):(.+)$/);
      if (!m) continue;
      const note = m[1].charAt(0).toUpperCase() + m[1].slice(1);
      lineSyllables.push({ pitch: note, syl: m[2] });
    }
    if (lineSyllables.length) sections[current].push(lineSyllables);
  }
  return sections;
}

// Vowel heuristic: pick the most plausible vowel for a syllable's
// orthography. Order matters — check digraphs before single letters.
function pickVowel(rawSyl) {
  const s = rawSyl.toLowerCase().replace(/^-+|-+$/g, "").replace(/[^a-z]/g, "");
  if (!s) return "uh";
  if (/ee|ea|ie$|y$/.test(s)) return "ee";
  if (/oo|ui|ue/.test(s)) return "oo";
  if (/oa|ow$|o[^aeiouy]?$/.test(s)) return "oh";
  if (/i/.test(s)) return "ih";
  if (/a/.test(s)) return "ah";
  if (/e/.test(s)) return "eh";
  if (/u/.test(s)) return "uh";
  if (/o/.test(s)) return "oh";
  return "uh";
}

// Time a single line: split lineSec across N syllables, line-final gets 1.5x.
function timeLine(line, lineSec, octave) {
  const n = line.length;
  if (!n) return [];
  const units = (n - 1) + 1.5;
  const unitSec = lineSec / units;
  return line.map((tok, i) => ({
    syl: tok.syl,
    pitch: /\d/.test(tok.pitch) ? tok.pitch : tok.pitch + String(octave),
    vowel: pickVowel(tok.syl),
    dur: i === n - 1 ? unitSec * 1.5 : unitSec,
  }));
}

// Build a phrase from a section by stretching each line to one bar.
function phraseFromSection(section, barSec, octave, gapSec = 0) {
  const phrase = [];
  for (let li = 0; li < section.length; li++) {
    const timed = timeLine(section[li], barSec, octave);
    phrase.push(...timed);
    if (gapSec > 0 && li < section.length - 1) {
      phrase.push({ syl: "", pitch: "C0", vowel: "uh", dur: gapSec, level: 0 });
    }
  }
  return phrase;
}

// ── arg parse + main ──────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  }
}

function expandHome(p) {
  if (!p || typeof p !== "string") return p;
  if (p === "~") return homedir();
  if (p.startsWith("~/")) return resolve(homedir(), p.slice(2));
  return p;
}

const OUT_PATH = expandHome(flags.out) || `${ROOT}/out/vocal.mp3`;

let phrase;
if (flags.score) {
  const scorePath = expandHome(flags.score);
  if (!existsSync(scorePath)) {
    console.error(`✗ --score file not found: ${scorePath}`);
    process.exit(1);
  }
  const sections = parseNp(readFileSync(scorePath, "utf8"));
  const sectionName = flags.section || "hook";
  if (!sections[sectionName]) {
    console.error(`✗ section '${sectionName}' not in score. available: ${sections._order.join(", ")}`);
    process.exit(1);
  }
  const bpm = Number(flags.bpm) || 140;
  const barSec = (60 / bpm) * 4;
  const octave = Number(flags.octave) || 3;
  const gapSec = Number(flags.gap) || 0;
  phrase = phraseFromSection(sections[sectionName], barSec, octave, gapSec);
  console.log(`→ score · ${scorePath} · section=${sectionName} · ${sections[sectionName].length} lines @ ${bpm} BPM`);
} else {
  phrase = SMOKE;
}

console.log(`→ vocal · ${phrase.length} syllables · ${phrase.reduce((t,s)=>t+s.dur,0).toFixed(2)}s`);
console.log("  " + phrase.filter(s => s.syl).map(s => `${s.syl}[${s.vowel}/${s.pitch}]`).join(" "));

const out = renderPhrase(phrase);

let peak = 0;
for (let i = 0; i < out.length; i++) {
  const a = Math.abs(out[i]);
  if (a > peak) peak = a;
}
if (peak > 0) {
  const target = 0.7;
  const norm = target / peak;
  for (let i = 0; i < out.length; i++) out[i] *= norm;
}

const outDir = dirname(OUT_PATH);
mkdirSync(outDir, { recursive: true });
const rawPath = `${outDir}/.vocal-smoke.f32.raw`;
const buf = Buffer.alloc(out.length * 4);
for (let i = 0; i < out.length; i++) buf.writeFloatLE(out[i], i * 4);
writeFileSync(rawPath, buf);

const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-f", "f32le", "-ar", String(SAMPLE_RATE), "-ac", "1",
  "-i", rawPath,
  "-c:a", "libmp3lame", "-q:a", "3",
  OUT_PATH,
], { stdio: "inherit" });
if (ff.status !== 0) { console.error("✗ ffmpeg failed"); process.exit(1); }
try { unlinkSync(rawPath); } catch {}
console.log(`✓ ${OUT_PATH}`);

// Optional: mix over a trap bed (or any audio file).
if (flags.over) {
  const overPath = expandHome(flags.over);
  if (!existsSync(overPath)) {
    console.error(`✗ --over file not found: ${overPath}`);
    process.exit(1);
  }
  const mixOut = OUT_PATH.replace(/\.mp3$/, "-with-bed.mp3");
  console.log(`→ mixing vocal over ${overPath} → ${mixOut}`);
  const mix = spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", overPath, "-i", OUT_PATH,
    "-filter_complex", "[0:a]volume=0.55[bed];[1:a]volume=1.0[voc];[bed][voc]amix=inputs=2:duration=longest:dropout_transition=0[a]",
    "-map", "[a]", "-c:a", "libmp3lame", "-q:a", "3",
    mixOut,
  ], { stdio: "inherit" });
  if (mix.status !== 0) { console.error("✗ ffmpeg mix failed"); process.exit(1); }
  console.log(`✓ ${mixOut}`);
}
