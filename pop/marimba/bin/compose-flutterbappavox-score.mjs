#!/usr/bin/env node
// Fit Jeffrey's lyric words to Flutterbap's actual lead pitches and beat grid.
// Held words become melismas whenever the marimba changes note underneath them.

import { readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flag = (name, fallback = null) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : fallback;
};
const alignmentPath = resolve(argv.find((a) => !a.startsWith("--")) || "");
const outScore = resolve(flag("out", "flutterbappavox-vocal.np"));
const outWords = resolve(flag("words", "flutterbappavox-vocal-words.json"));
const outMap = resolve(flag("map", "flutterbappavox-vocal-map.json"));
const marimbaScore = resolve(flag("marimba-score", "pop/marimba/flutterbap.np"));

const alignment = JSON.parse(readFileSync(alignmentPath, "utf8"));
const words = alignment.words;
const sentences = [];
let sentence = [];
for (const word of words) {
  sentence.push(word);
  if (/[.!?]$/.test(word.text)) { sentences.push(sentence); sentence = []; }
}
if (sentence.length) sentences.push(sentence);
if (sentences.length !== 13 || words.length !== 192) {
  throw new Error(`expected the authored 13 sentences / 192 words, got ${sentences.length} / ${words.length}`);
}

const NOTES = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
function transpose(note, semitones) {
  const m = /^([A-G])(#?)(-?\d)$/.exec(note);
  if (!m) throw new Error(`bad note ${note}`);
  const midi = (Number(m[3]) + 1) * 12 + NOTES.indexOf(m[1] + m[2]) + semitones;
  return `${NOTES[(midi % 12 + 12) % 12]}${Math.floor(midi / 12) - 1}`;
}

const melody = [];
const add = (startBeat, note) => melody.push({ startBeat, note });

// The doubled drum intro has no continuous xylophone lead. These quarter-note
// pitches follow its C/G tonal beacons and the kick grid, so the voice stays in
// the record's rhythm until the authored lead arrives at output bar 8.
for (let bar = 0; bar < 8; bar++) {
  const intro = bar % 2 ? ["C4", "E4", "G4", "E4"] : ["C4", "G3", "C4", "G3"];
  intro.forEach((note, beat) => add(bar * 4 + beat, note));
}

// Parse the 32 actual lead bars from flutterbap.np. They are written one bar
// per non-comment line; output adds eight intro bars, and Jeffrey sings them an
// octave below the mallet so the pitch classes and contour remain identical.
const bodyLines = readFileSync(marimbaScore, "utf8").split("\n")
  .map((line) => line.trim())
  .filter((line) => line && !line.startsWith("#") && /[A-G][#b]?\d:_/.test(line));
if (bodyLines.length !== 32) throw new Error(`expected 32 Flutterbap lead bars, got ${bodyLines.length}`);
for (let bar = 0; bar < bodyLines.length; bar++) {
  let within = 0;
  for (const token of bodyLines[bar].split(/\s+/)) {
    const m = /^([A-G][#b]?\d):_\*(\d+(?:\.\d+)?)$/.exec(token);
    if (!m) continue;
    add((8 + bar) * 4 + within, transpose(m[1].replace("b", "#"), -12));
    within += Number(m[2]);
  }
  if (Math.abs(within - 4) > 1e-6) throw new Error(`lead bar ${bar + 1} totals ${within} beats`);
}

// Coda lead from render-flutterbap.mjs, shifted four bars by the doubled intro.
for (const [beat, note] of [
  [40 * 4 + 1, "E4"], [40 * 4 + 2.5, "A4"], [42 * 4 + 1, "C5"], [42 * 4 + 2.5, "G4"],
  [44 * 4, "A3"], [44 * 4 + 1, "C4"], [44 * 4 + 2, "E4"], [44 * 4 + 3, "C4"],
  [45 * 4, "G3"], [45 * 4 + 1, "B3"], [45 * 4 + 2, "D4"], [45 * 4 + 3, "G4"],
  [46 * 4, "C5"], [46 * 4 + 1, "G3"], [46 * 4 + 2, "E4"],
  [47 * 4, "B4"], [47 * 4 + 1, "D4"], [47 * 4 + 2, "B4"],
  [48 * 4, "A4"], [48 * 4 + 1, "E4"], [48 * 4 + 2, "C5"],
  [49 * 4, "A4"], [49 * 4 + 1, "C4"], [49 * 4 + 2, "A4"],
  [50 * 4, "C5"],
]) add(beat, note);
melody.sort((a, b) => a.startBeat - b.startBeat);

const noteAt = (beat) => {
  let note = melody[0].note;
  for (const e of melody) { if (e.startBeat > beat + 1e-8) break; note = e.note; }
  return note;
};
const boundariesWithin = (start, end) => melody
  .map((e) => e.startBeat)
  .filter((beat) => beat > start + 1e-8 && beat < end - 1e-8);

const scoreTokens = [];
const wordMap = [];
let cursor = 0;
for (let si = 0, wi = 0; si < sentences.length; si++) {
  const line = sentences[si];
  const targetBeats = si < 12 ? 16 : 13; // twelve four-bar phrases + 3.25-bar final cadence
  let weights;
  if (line.length <= targetBeats) {
    weights = line.map((_, i) => i === line.length - 1 ? targetBeats - (line.length - 1) : 1);
  } else {
    const halfCount = Math.round((line.length - targetBeats) * 2);
    weights = line.map((_, i) => i < halfCount ? 0.5 : 1);
  }
  if (Math.abs(weights.reduce((a, b) => a + b, 0) - targetBeats) > 1e-6) {
    throw new Error(`sentence ${si + 1} cannot fit ${targetBeats} beats`);
  }
  for (let i = 0; i < line.length; i++, wi++) {
    const clean = line[i].text.replace(/[^A-Za-z']/g, "") || "la";
    const start = cursor;
    const end = cursor + weights[i];
    const cuts = [start, ...boundariesWithin(start, end), end];
    const segments = [];
    for (let j = 0; j < cuts.length - 1; j++) {
      const segStart = cuts[j], duration = cuts[j + 1] - cuts[j];
      const note = noteAt(segStart);
      const lyric = j === 0 ? clean : `-${clean}`;
      scoreTokens.push(`${note}:${lyric}*${Number(duration.toFixed(3))}`);
      segments.push({ note, startBeat: segStart, durationBeats: duration });
    }
    wordMap.push({ index: wi, text: line[i].text, startBeat: start, durationBeats: weights[i], segments });
    cursor = end;
  }
}
if (Math.abs(cursor - 205) > 1e-6) throw new Error(`score totals ${cursor} beats, expected 205`);

writeFileSync(outScore, [
  "# flutterbappavox — Jeffrey sings Flutterbap's actual lead, one octave down.",
  "# Word onsets are beat-grid aligned; repeated -word tokens are marimba-following melismas.",
  "", "verse", scoreTokens.join(" "), "",
].join("\n"));
writeFileSync(outWords, JSON.stringify(words, null, 2));
writeFileSync(outMap, JSON.stringify({ bpm: 124, totalBeats: cursor, words: wordMap }, null, 2) + "\n");
console.log(`✓ ${outScore} — ${words.length} words · ${scoreTokens.length} note segments · ${cursor} beats`);
console.log(`✓ ${outMap}`);
