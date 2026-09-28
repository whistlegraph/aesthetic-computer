#!/usr/bin/env node
// Electable /pop skeletons: chords + topline + rhythm + rate, no production.

import { mkdirSync, readFileSync, unlinkSync, writeFileSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const argument = (flag) => {
  const index = process.argv.indexOf(flag);
  if (index < 0) return undefined;
  const value = process.argv[index + 1];
  if (!value || value.startsWith("--")) throw new Error(`${flag} needs a value`);
  return value;
};
const OUT = resolve(argument("--out") || resolve(HERE, "../out/composition-beds"));
mkdirSync(OUT, { recursive: true });

const SR = 48_000;
const TAU = Math.PI * 2;
const ROOT = 64; // E4; selection is transposable after election.
const SCALES = {
  minor: [0, 2, 3, 5, 7, 8, 10], dorian: [0, 2, 3, 5, 7, 9, 10],
  major: [0, 2, 4, 5, 7, 9, 11], phrygian: [0, 1, 3, 5, 7, 8, 10],
  mixolydian: [0, 2, 4, 5, 7, 9, 10], lydian: [0, 2, 4, 6, 7, 9, 11],
  harmonicMinor: [0, 2, 3, 5, 7, 8, 11],
};
const PATTERNS = {
  arch: [0, 2, 4, 6, 4, 3, 2, 0, 2, 4, 7, 6, 4, 2, 1, 0],
  answer: [4, 2, 0, 3, 2, 1, 0, -1, 0, 2, 5, 4, 3, 1, 0, 0],
  wide: [0, 4, 1, 6, 4, 7, 3, 2, 0, 5, 2, 8, 6, 3, 1, 0],
  orbit: [0, 2, 1, 4, 2, 5, 3, 1, 0, 3, 2, 6, 4, 2, 1, 0],
  fall: [7, 6, 4, 3, 2, 1, 0, 2, 5, 4, 3, 1, 2, 0, -1, 0],
  beacon: [0, 0, 4, 3, 0, 5, 4, 2, 0, 7, 6, 4, 2, 1, 0, 0],
};
const chord = (name, bass, notes) => ({ name, bass, notes });
const DEFAULT_BEDS = [
  { id: "01", name: "minor doorway", bpm: 92, rhythm: "halves", scale: "harmonicMinor", melody: "arch",
    chords: [chord("Em(add9)", 40, [52,59,64,66,67]), chord("Cmaj7/E", 40, [48,55,59,64]), chord("Am9", 33, [45,48,52,55,59]), chord("B7(b9)", 35, [47,51,54,57,60])] },
  { id: "02", name: "dorian window", bpm: 118, rhythm: "syncopated", scale: "dorian", melody: "answer",
    chords: [chord("Em7", 40, [52,55,59,62]), chord("A(add9)/E", 40, [45,52,57,59,64]), chord("Cmaj7", 36, [48,52,55,59]), chord("B7", 35, [47,51,54,57])] },
  { id: "03", name: "descending daylight", bpm: 104, rhythm: "quarters", scale: "major", melody: "arch",
    chords: [chord("E(add9)", 40, [52,56,59,66]), chord("B/D#", 39, [47,51,54,59]), chord("C#m7", 37, [49,52,56,59]), chord("Amaj9", 33, [45,52,56,59,64])] },
  { id: "04", name: "phrygian gravity", bpm: 78, rhythm: "three", scale: "phrygian", melody: "fall",
    chords: [chord("Em(add9)", 40, [52,59,64,66,67]), chord("Fmaj7/E", 40, [53,57,60,64]), chord("Cmaj7/E", 40, [48,55,59,64]), chord("B7(b9)", 35, [47,51,54,57,60])] },
  { id: "05", name: "chromatic heart", bpm: 112, rhythm: "syncopated", scale: "minor", melody: "wide",
    chords: [chord("Emaj7", 40, [52,56,59,63]), chord("Gmaj7", 43, [55,59,62,66]), chord("Am6", 45, [57,60,64,66]), chord("B7", 35, [47,51,54,57])] },
  { id: "06", name: "lydian runway", bpm: 136, rhythm: "gallop", scale: "lydian", melody: "beacon",
    chords: [chord("Emaj7(#11)", 40, [52,56,59,63,66]), chord("F#7sus", 42, [54,59,61,64]), chord("G#m7", 44, [56,59,63,66]), chord("Amaj7/E", 40, [45,52,56,61])] },
  { id: "07", name: "pedal colors", bpm: 86, rhythm: "sparse", scale: "minor", melody: "orbit",
    chords: [chord("Em(add9)", 40, [52,59,64,66,67]), chord("Cmaj7/E", 40, [48,55,59,64]), chord("A/E", 40, [45,52,57,61]), chord("B7sus/E", 40, [47,52,54,57])] },
  { id: "08", name: "deceptive orbit", bpm: 126, rhythm: "three", scale: "harmonicMinor", melody: "orbit",
    chords: [chord("Em", 40, [52,55,59,64]), chord("Am7", 33, [45,48,52,55]), chord("D7", 38, [50,54,57,60]), chord("Cmaj7–B7", 36, [48,51,54,59])] },
  { id: "09", name: "mixolydian road", bpm: 144, rhythm: "quarters", scale: "mixolydian", melody: "wide",
    chords: [chord("E7", 40, [52,56,59,62]), chord("D/A", 33, [50,54,57,62]), chord("A(add9)", 33, [45,52,57,59,64]), chord("B7sus", 35, [47,52,54,57])] },
  { id: "10", name: "minor plagal", bpm: 72, rhythm: "halves", scale: "minor", melody: "answer",
    chords: [chord("Em9", 40, [52,55,59,66]), chord("Cmaj7", 36, [48,52,55,59]), chord("Am6", 33, [45,48,52,54]), chord("Em/B", 35, [52,55,59,64])] },
  { id: "11", name: "two-room tension", bpm: 154, rhythm: "gallop", scale: "phrygian", melody: "beacon",
    chords: [chord("Em(add9)", 40, [52,59,64,66,67]), chord("Fmaj7/E", 40, [53,57,60,64]), chord("Em(add9)", 40, [52,59,64,66,67]), chord("Fmaj7/E", 40, [53,57,60,64])] },
  { id: "12", name: "suspended home", bpm: 98, rhythm: "sparse", scale: "major", melody: "fall",
    chords: [chord("E(sus2 add9)", 40, [52,54,59,66]), chord("G#m7", 44, [56,59,63,66]), chord("Am6", 45, [57,60,64,66]), chord("E5", 40, [52,59,64])] },
];

// A take study can supply its own timed notes and harmonies. Keep these in
// seconds: the recorded rubato must not silently become a quantized pattern.
const input = argument("--input");
const BEDS = input ? JSON.parse(readFileSync(resolve(input), "utf8")) : DEFAULT_BEDS;
if (!Array.isArray(BEDS) || !BEDS.length) throw new Error("expected a nonempty bed array");
for (const bed of BEDS) {
  if (!/^[a-zA-Z0-9-]+$/.test(bed.id) || !/^[a-zA-Z0-9 -]+$/.test(bed.name))
    throw new Error("bed id/name must be a safe filename");
  if (bed.notes) {
    if (!(bed.duration > 0 && bed.duration <= 600)) throw new Error("invalid duration");
    for (const note of bed.notes) {
      if (![note.start, note.dur, note.midi].every(Number.isFinite) || note.start < 0 ||
          note.dur <= 0 || note.midi < 0 || note.midi > 127 || note.start + note.dur > bed.duration)
        throw new Error(`invalid timed note in ${bed.id}`);
    }
    for (const harmony of bed.chords) {
      if (![harmony.at, harmony.duration, harmony.bass, ...harmony.notes].every(Number.isFinite) ||
          harmony.at < 0 || harmony.duration <= 0 || harmony.at + harmony.duration > bed.duration)
        throw new Error(`invalid timed harmony in ${bed.id}`);
    }
  }
}

const electAt = process.argv.indexOf("--elect");
if (electAt >= 0) {
  const id = process.argv[electAt + 1];
  const bed = BEDS.find((candidate) => candidate.id === id);
  if (!bed) throw new Error(`unknown bed ${id}; choose ${BEDS.map((b) => b.id).join(", ")}`);
  writeFileSync(resolve(OUT, "elected.json"), JSON.stringify({ ...bed, electedAt: new Date().toISOString() }, null, 2));
  console.log(`✓ elected ${bed.id} — ${bed.name}`);
  process.exit(0);
}

const rhythmEvents = (name) => {
  const offsets = name === "halves" ? [0, 2]
    : name === "quarters" ? [0, 1, 2, 3]
    : name === "syncopated" ? [0, 1.5, 2.5, 3.5]
    : name === "three" ? [0, 1.5, 3]
    : name === "gallop" ? [0, 0.75, 1.5, 2.5, 3.25]
    : [0, 2.75];
  const duration = name === "halves" ? 1.55 : name === "sparse" ? 2.2 : name === "gallop" ? 0.46 : 0.72;
  const events = [];
  for (let bar = 0; bar < 8; bar++) for (const offset of offsets) events.push([bar * 4 + offset, duration]);
  return events;
};
const degreeMidi = (scale, degree) => {
  const octave = Math.floor(degree / scale.length);
  const index = ((degree % scale.length) + scale.length) % scale.length;
  return ROOT + scale[index] + octave * 12;
};
const hz = (midi) => 440 * 2 ** ((midi - 69) / 12);
const gain = (dB) => 10 ** (dB / 20);
const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const add = (mix, frame, sample, pan = 0) => {
  if (frame < 0 || frame >= mix.length / 2) return;
  const angle = (clamp(pan, -1, 1) + 1) * Math.PI / 4;
  mix[frame * 2] += sample * Math.cos(angle);
  mix[frame * 2 + 1] += sample * Math.sin(angle);
};
function tone(mix, at, duration, midi, level, pan, kind) {
  const start = Math.floor(at * SR), freq = hz(midi), tail = kind === "lead" ? 0.5 : 1.2;
  for (let i = 0, n = Math.floor((duration + tail) * SR); i < n; i++) {
    const t = i / SR, attack = Math.min(1, t / (kind === "lead" ? 0.01 : 0.12));
    const release = t < duration ? 1 : Math.max(0, 1 - (t - duration) / tail);
    const decay = kind === "lead" ? Math.exp(-t / Math.max(0.3, duration)) : 1;
    const phase = TAU * freq * t;
    const wave = kind === "bass" ? Math.sin(phase) + 0.12 * Math.sin(phase * 2)
      : kind === "lead" ? 0.82 * Math.sin(phase) + 0.18 * Math.sin(phase * 2.01)
      : 0.72 * Math.sin(phase) + 0.2 * Math.sin(phase * 2) + 0.08 * Math.sin(phase * 0.5);
    add(mix, start + i, wave * attack * release * decay * level, pan);
  }
}
function renderBed(bed) {
  const beat = bed.bpm ? 60 / bed.bpm : 0, bar = beat * 4;
  const duration = bed.notes ? bed.duration : 8 * bar + 2;
  const mix = new Float32Array(Math.ceil(duration * SR) * 2);
  bed.chords.forEach((harmony, index) => {
    const at = harmony.at ?? index * 2 * bar, length = harmony.duration ?? 2 * bar - 0.05;
    tone(mix, at, length, harmony.bass, gain(-20), 0, "bass");
    harmony.notes.forEach((midi, voice) => tone(mix, at + voice * 0.025, length, midi, gain(-28), (voice - 2) * 0.16, "pad"));
  });
  if (bed.notes) {
    for (const note of bed.notes) {
      tone(mix, note.start, note.dur, note.midi,
        gain(-11) * ((note.velocity ?? 100) / 100), note.pan ?? 0, "lead");
    }
  } else {
    const events = rhythmEvents(bed.rhythm), degrees = PATTERNS[bed.melody], scale = SCALES[bed.scale];
    events.forEach(([atBeat, durationBeats], index) => {
      const phrase = Math.floor(index / degrees.length);
      const degree = degrees[index % degrees.length] + (phrase % 2 ? 0 : phrase);
      tone(mix, atBeat * beat, durationBeats * beat, degreeMidi(scale, degree), gain(-11), 0.08 * Math.sin(index), "lead");
    });
    for (let b = 0; b < 8; b++) for (const pulse of [0, 2])
      tone(mix, (b * 4 + pulse) * beat, 0.06, 28, gain(-32), 0, "bass");
  }
  let peak = 0;
  for (const sample of mix) peak = Math.max(peak, Math.abs(sample));
  const trim = peak ? 0.82 / peak : 1;
  for (let i = 0; i < mix.length; i++) mix[i] = Math.tanh(mix[i] * trim * 1.04) / Math.tanh(1.04);
  return { mix, duration, beat };
}
function writeWav(path, mix) {
  const bytes = mix.length * 4, header = Buffer.alloc(44);
  header.write("RIFF", 0); header.writeUInt32LE(36 + bytes, 4); header.write("WAVEfmt ", 8);
  header.writeUInt32LE(16, 16); header.writeUInt16LE(3, 20); header.writeUInt16LE(2, 22);
  header.writeUInt32LE(SR, 24); header.writeUInt32LE(SR * 8, 28); header.writeUInt16LE(8, 32);
  header.writeUInt16LE(32, 34); header.write("data", 36); header.writeUInt32LE(bytes, 40);
  writeFileSync(path, Buffer.concat([header, Buffer.from(mix.buffer)]));
}
function encode(mix, target) {
  const wav = `${target}.wav`;
  writeWav(wav, mix);
  if (input) {
    // A static gain makes take auditions comparable without changing the
    // recorded phrasing or adding a mastering/compression treatment.
    const scan = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", wav,
      "-af", "loudnorm=I=-18:TP=-1.5:LRA=11:print_format=json", "-f", "null", "-"],
      { encoding: "utf8" });
    if (scan.status !== 0) throw new Error(`loudness scan failed: ${scan.stderr}`);
    const match = scan.stderr.match(/\{\s*"input_i"[\s\S]*?\}/);
    if (!match) throw new Error("missing loudness measurement");
    const measured = JSON.parse(match[0]);
    const adjustment = Math.min(-18 - Number(measured.input_i), -1.5 - Number(measured.input_tp));
    if (!Number.isFinite(adjustment)) throw new Error("non-finite audition loudness");
    const scale = gain(adjustment);
    for (let i = 0; i < mix.length; i++) mix[i] *= scale;
    writeWav(wav, mix);
  }
  const result = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", wav,
    "-codec:a", "libmp3lame", "-b:a", "256k", `${target}.mp3`], { stdio: "inherit" });
  unlinkSync(wav);
  if (result.status !== 0) throw new Error(`ffmpeg failed for ${target}`);
}

const rendered = BEDS.map((bed) => ({ bed, ...renderBed(bed) }));
for (const item of rendered) encode(item.mix, resolve(OUT, `${item.bed.id}-${item.bed.name.replaceAll(" ", "-")}`));

const gap = 1.25, total = rendered.reduce((sum, item) => sum + item.duration + gap, 0);
const reel = new Float32Array(Math.ceil(total * SR) * 2);
const index = [];
let cursor = 0;
for (const item of rendered) {
  index.push({ id: item.bed.id, name: item.bed.name, at: +cursor.toFixed(3), bpm: item.bed.bpm,
    duration: +item.duration.toFixed(3),
    rhythm: item.bed.rhythm, progression: item.bed.chords.map((h) => h.name),
    source: item.bed.source, sections: item.bed.sections });
  reel.set(item.mix, Math.floor(cursor * SR) * 2);
  cursor += item.duration + gap;
}
encode(reel, resolve(OUT, "composition-beds-audition"));
writeFileSync(resolve(OUT, "beds.json"), JSON.stringify(index, null, 2));
writeFileSync(resolve(OUT, "genomes.json"), JSON.stringify(BEDS, null, 2));
writeFileSync(resolve(OUT, "playlist.m3u"), BEDS.map((bed) => `${bed.id}-${bed.name.replaceAll(" ", "-")}.mp3`).join("\n") + "\n");
const stamp = (seconds) => `${Math.floor(seconds / 60)}:${String(Math.floor(seconds % 60)).padStart(2, "0")}`;
writeFileSync(resolve(OUT, "README.txt"), index.map((item) =>
  `${stamp(item.at)}  ${item.id}  ${item.name}  ${item.bpm ? `${item.bpm} BPM` : "played timing"} · ${item.rhythm}\n      ${item.progression.join(" · ")}`).join("\n") +
  "\n\nElect with: node pop/bin/render-composition-beds.mjs" +
  (input ? ` --input ${JSON.stringify(resolve(input))}` : "") +
  ` --out ${JSON.stringify(OUT)} --elect NN\n`);
console.log(`✓ ${BEDS.length} beds + audition reel → ${OUT}`);
