#!/usr/bin/env node
// render-waltz-audio.mjs — the waltz audio leg for machines without Apple.
//
// A transliteration of render-menu-band-waltz.swift: identical score math
// (same manifest merge, same white-key display mapping, same development
// and cadence rules — the notes.json it writes must agree note-for-note,
// because the video sim draws from it), with FluidSynth standing in for
// AVAudioUnitSampler. Each distinct Menu Band key gets its own MIDI channel
// so the per-key stereo pan survives the trip through General MIDI; the
// drum channel (10) is skipped, and at most fourteen keys exist, so
// channels are never scarce. Loudness differences between the two synths
// wash out downstream — the mastering pass loudnorms to −18 LUFS.
//
//   node render-waltz-audio.mjs --id ID [--manifest FILE] [--out-dir DIR]
//
// Soundfont resolution: $AC_GM_SOUNDFONT, else the Ubuntu fluid-soundfont-gm
// path, else the first .sf2 under ~/.local/share/soundfonts.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync }
  from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";

const argv = process.argv.slice(2);
const opts = { manifest: "pop/menuband/waltzes/menu-band-waltzes.json" };
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--manifest") opts.manifest = argv[++i];
  else if (argv[i] === "--id") opts.id = argv[++i];
  else if (argv[i] === "--out-dir") opts.outDir = argv[++i];
  else { console.error("usage: render-waltz-audio.mjs --id ID [--manifest FILE] [--out-dir DIR]"); process.exit(2); }
}
if (!opts.id) { console.error("--id is required"); process.exit(2); }

function merged(base, override) {
  const result = { ...base };
  for (const [key, value] of Object.entries(override)) {
    result[key] =
      value && typeof value === "object" && !Array.isArray(value) &&
      result[key] && typeof result[key] === "object" && !Array.isArray(result[key])
        ? merged(result[key], value) : value;
  }
  return result;
}

const root = JSON.parse(readFileSync(opts.manifest, "utf8"));
const selected = (root.variations || []).find((v) => v.id === opts.id);
if (!selected) { console.error(`waltz not found in manifest: ${opts.id}`); process.exit(1); }
const spec = merged(root.defaults || {}, selected);

const VISIBLE_MIDIS = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76, 77, 79, 81, 83];
const LOWER_BY_PITCH_CLASS = { 0: 60, 2: 62, 4: 64, 5: 65, 7: 67, 9: 69, 11: 71 };

function displayMidi(soundMidi) {
  if (VISIBLE_MIDIS.includes(soundMidi)) return soundMidi;
  const pitchClass = ((soundMidi % 12) + 12) % 12;
  const lower = LOWER_BY_PITCH_CLASS[pitchClass];
  if (lower === undefined) throw new Error(`waltz note is not a white Menu Band key: ${soundMidi}`);
  return soundMidi >= 72 && VISIBLE_MIDIS.includes(lower + 12) ? lower + 12 : lower;
}

function scaleIntervals(mode) {
  switch (String(mode).toLowerCase()) {
    case "minor": case "aeolian": return [0, 2, 3, 5, 7, 8, 10];
    case "dorian": return [0, 2, 3, 5, 7, 9, 10];
    default: return [0, 2, 4, 5, 7, 9, 11];
  }
}
const tonicMidi = (tonic) =>
  ({ A: 45, D: 50 })[String(tonic).toUpperCase()] ?? 48;
const floorDiv = (value, divisor) => Math.floor(value / divisor);

const id = spec.id ?? opts.id;
const name = spec.name ?? id;
const bpm = Number(spec.bpm ?? 84);
const bars = Math.trunc(Number(spec.bars ?? 28));
const duration = Number(spec.durationSec ?? 60);
const beat = 60 / bpm;
const bar = beat * 3;
if (Math.abs(bars * bar - duration) >= 0.001)
  throw new Error("bars × 3 beats must equal the requested duration");

const tonic = tonicMidi(spec.tonic ?? "C");
const intervals = scaleIntervals(spec.mode ?? "major");
const program = Math.trunc(Number(spec.instrumentProgram ?? 0));
const development = spec.development ?? "lift";
const harmony = spec.harmonyDegrees;
const melodyBars = spec.melodyBars;
if (!harmony?.length || !melodyBars?.length)
  throw new Error("waltz requires harmonyDegrees and melodyBars");

function scaleMidi(degree, octaveShift = 0) {
  const octave = floorDiv(degree, 7);
  const index = ((degree % 7) + 7) % 7;
  return tonic + intervals[index] + 12 * (octave + octaveShift);
}

const notes = [];
function add(at, dur, soundMidi, velocity) {
  if (!(at >= 0 && at < duration - 0.05)) return;
  notes.push({
    t: at,
    dur: Math.min(dur, duration - at - 0.02),
    midi: displayMidi(soundMidi),
    soundMidi,
    vel: Math.max(0.06, Math.min(0.9, velocity)),
    lane: "tone",
  });
}

function melodyOffsets(count) {
  switch (count) {
    case 1: return [0];
    case 2: return [0, 1.5];
    case 3: return [0, 1, 2];
    case 4: return [0, 0.5, 1.5, 2];
    default: return Array.from({ length: count }, (_, i) => (i * 3) / count);
  }
}

function developed(source, barIndex) {
  const section = Math.floor(barIndex / 8);
  if (barIndex >= bars - 4) {
    const cadences = [[11, 9, 7], [10, 8], [9, 8], [7]];
    return cadences[barIndex - (bars - 4)];
  }
  if (section <= 0) return source;
  switch (development) {
    case "mirror": {
      // Swift's integer division truncates; keep that, not a float mean.
      const center = Math.trunc(source.reduce((a, b) => a + b, 0) / Math.max(1, source.length));
      return source.map((d) => Math.max(0, Math.min(13, center - (d - center) + (section === 2 ? 1 : 0))));
    }
    case "turn":
      if (section % 2 === 1) return [...source.slice(1), source[0]];
      return source.map((d, i) => (i % 2 === 0 ? Math.min(13, d + 1) : d));
    default:
      return source.map((d, i) =>
        i === source.length - 1 || (section >= 2 && i === 0) ? Math.min(13, d + 7) : d);
  }
}

for (let barIndex = 0; barIndex < bars; barIndex++) {
  const barStart = barIndex * bar;
  const rootDegree = barIndex >= bars - 2 ? 0 : harmony[barIndex % harmony.length];

  // Beat one is the bass; beats two and three are the same quiet triad.
  add(barStart, beat * 0.78, scaleMidi(rootDegree), 0.30);
  for (const chordBeat of [1.0, 2.0]) {
    [rootDegree, rootDegree + 2, rootDegree + 4].forEach((chordDegree, voice) => {
      add(barStart + chordBeat * beat, beat * 0.62,
        scaleMidi(chordDegree, 1), 0.14 + voice * 0.018);
    });
  }

  const melody = developed(melodyBars[barIndex % melodyBars.length], barIndex);
  const offsets = melodyOffsets(melody.length);
  melody.forEach((degree, index) => {
    const at = barStart + offsets[index] * beat;
    const next = index + 1 < offsets.length ? offsets[index + 1] : 3.0;
    const durationBeats = Math.max(0.32, next - offsets[index]);
    add(at, durationBeats * beat * 0.82, scaleMidi(degree, 1),
      index === 0 ? 0.46 : 0.40);
  });
}

notes.sort((a, b) => (a.t === b.t ? a.soundMidi - b.soundMidi : a.t - b.t));
if (!notes.length) throw new Error("empty waltz");
for (const note of notes) {
  if (!VISIBLE_MIDIS.includes(note.midi)) throw new Error("visual note escaped Menu Band");
  if (!(note.t >= 0 && note.t + note.dur <= duration + 0.001)) throw new Error("note escaped duration");
}

// ── the pan the QWERTY strip implies, same table as the Swift renderer ──
const KEYCODE_BY_DISPLAY = {
  60: 8, 62: 2, 64: 14, 65: 3, 67: 5, 69: 0, 71: 11,
  72: 4, 74: 34, 76: 38, 77: 40, 79: 37, 81: 46, 83: 45,
};
const QWERTY_ROWS = [[12, 13, 14, 15, 17, 16, 32, 34, 31, 35],
  [0, 1, 2, 3, 5, 4, 38, 40, 37, 41, 39],
  [6, 7, 8, 9, 11, 45, 46]];
const ROW_OFFSETS = [0.0, 0.5, 1.0];

function menuBandPan(display) {
  const keyCode = KEYCODE_BY_DISPLAY[display];
  if (keyCode === undefined) return 0;
  for (let row = 0; row < QWERTY_ROWS.length; row++) {
    const column = QWERTY_ROWS[row].indexOf(keyCode);
    if (column !== -1) return ((column + ROW_OFFSETS[row]) / 10.5 * 2 - 1) * 0.78;
  }
  return 0;
}

// ── a type-0 SMF carrying the whole performance ──────────────────────
// One channel per distinct display key (skipping GM's drum channel 10) so
// pan and the Swift renderer's edge-gain droop ride channel controllers.
const PPQ = 480;
const displays = [...new Set(notes.map((n) => n.midi))].sort((a, b) => a - b);
const channelOf = {};
displays.forEach((display, i) => { channelOf[display] = i >= 9 ? i + 1 : i; });
if (Math.max(...Object.values(channelOf)) > 15) throw new Error("out of MIDI channels");

const events = [];
const tick = (seconds) => Math.round((seconds / beat) * PPQ);
for (const display of displays) {
  const ch = channelOf[display];
  const pan = menuBandPan(display);
  const gainDb = (-2.5 * Math.abs(pan)) / 0.78;
  events.push({ at: 0, data: [0xc0 | ch, Math.max(0, Math.min(127, program))] });
  events.push({ at: 0, data: [0xb0 | ch, 10, Math.round((pan * 0.5 + 0.5) * 127)] });
  events.push({ at: 0, data: [0xb0 | ch, 7, Math.round(100 * 10 ** (gainDb / 40))] });
}
for (const note of notes) {
  const ch = channelOf[note.midi];
  const velocity = Math.max(1, Math.min(127, Math.round(note.vel * 127)));
  events.push({ at: tick(note.t), on: true, data: [0x90 | ch, note.soundMidi, velocity] });
  events.push({ at: tick(note.t + note.dur), on: false, data: [0x80 | ch, note.soundMidi, 0] });
}
events.sort((a, b) => (a.at === b.at ? (a.on === true ? 1 : -1) - (b.on === true ? 1 : -1) : a.at - b.at));

const bytes = [];
const varlen = (value) => {
  const stack = [value & 0x7f];
  while ((value >>= 7)) stack.push((value & 0x7f) | 0x80);
  return stack.reverse();
};
bytes.push(0x00, 0xff, 0x51, 0x03, ...[
  (Math.round(60e6 / bpm) >> 16) & 0xff,
  (Math.round(60e6 / bpm) >> 8) & 0xff,
  Math.round(60e6 / bpm) & 0xff,
]);
let cursor = 0;
for (const event of events) {
  bytes.push(...varlen(event.at - cursor), ...event.data);
  cursor = event.at;
}
bytes.push(...varlen(tick(duration) - cursor + PPQ), 0xff, 0x2f, 0x00);
const track = Buffer.from(bytes);
const header = Buffer.alloc(14 + 8);
header.write("MThd", 0);
header.writeUInt32BE(6, 4);
header.writeUInt16BE(0, 8);
header.writeUInt16BE(1, 10);
header.writeUInt16BE(PPQ, 12);
header.write("MTrk", 14);
header.writeUInt32BE(track.length, 18);

const outDir = resolve(opts.outDir ?? `pop/menuband/out/menu-band-waltzes/${id}`);
mkdirSync(outDir, { recursive: true });
const midPath = join(outDir, `${id}.mid`);
writeFileSync(midPath, Buffer.concat([header, track]));

// The score is written before the synth runs: the video sim needs it even
// when a soundfont is missing, and parity with the Swift renderer can be
// checked without any synthesis at all.
const sortKeys = (value) =>
  Array.isArray(value) ? value.map(sortKeys)
    : value && typeof value === "object"
      ? Object.fromEntries(Object.keys(value).sort().map((k) => [k, sortKeys(value[k])]))
      : value;
const score = sortKeys({
  id, name, bpm, meter: "3/4", beatSec: beat, barSec: bar, durationSec: duration,
  revealAtSec: Number(spec.revealAtSec ?? 0.5),
  revealDurationSec: Number(spec.revealDurationSec ?? 1.2),
  exitAtSec: Number(spec.exitAtSec ?? 58.2),
  exitDurationSec: Number(spec.exitDurationSec ?? 1.4),
  instrumentProgram: program,
  instrumentName: spec.instrumentName ?? "Acoustic Grand Piano",
  notes,
});
const scorePath = join(outDir, `${id}.notes.json`);
writeFileSync(scorePath, JSON.stringify(score, null, 2) + "\n");
if (process.env.AC_WALTZ_SCORE_ONLY === "1") {
  console.log(`✓ ${id} · score only · ${notes.length} notes`);
  process.exit(0);
}

// ── soundfont + fluidsynth ───────────────────────────────────────────
function findSoundfont() {
  if (process.env.AC_GM_SOUNDFONT && existsSync(process.env.AC_GM_SOUNDFONT))
    return process.env.AC_GM_SOUNDFONT;
  const candidates = [
    "/usr/share/sounds/sf2/FluidR3_GM.sf2",
    "/usr/share/sounds/sf2/default-GM.sf2",
    "/opt/homebrew/share/soundfonts/default.sf2",
  ];
  for (const path of candidates) if (existsSync(path)) return path;
  const local = join(homedir(), ".local/share/soundfonts");
  if (existsSync(local))
    for (const file of readdirSync(local).sort())
      if (file.endsWith(".sf2")) return join(local, file);
  throw new Error("no GM soundfont — set AC_GM_SOUNDFONT or install fluid-soundfont-gm");
}

const wavPath = join(outDir, `${id}.raw.wav`);
const soundfont = findSoundfont();
const synth = spawnSync("fluidsynth", [
  "-ni", "-r", "48000", "-g", "0.6",
  "-o", "synth.reverb.active=yes",
  "-o", "synth.reverb.room-size=0.6",
  // The Swift chain ran a medium hall at 16% wet (10% for accordion).
  "-o", `synth.reverb.level=${program === 21 ? 0.10 : 0.16}`,
  "-F", wavPath, soundfont, midPath,
], { stdio: ["ignore", "pipe", "pipe"], encoding: "utf8" });
if (synth.status !== 0) {
  console.error(synth.stderr || synth.stdout);
  throw new Error(`fluidsynth exited ${synth.status}`);
}

console.log(`✓ ${id} · ${name} · ${bars} bars · ${duration.toFixed(1)}s · ${notes.length} notes (fluidsynth · ${soundfont.split("/").pop()})`);
console.log(`✓ ${wavPath}`);
console.log(`✓ ${scorePath}`);
