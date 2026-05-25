#!/usr/bin/env node
// gen-tts-singing.mjs — robot Mac voices singing the antecedent of the
// THEME with the lyric "i hope that you get all the mo-ney you want".
// Each syllable maps to a THEME note; macOS `say [[pbas N]]` sets the
// base pitch per syllable. Renders ONE WAV per voice into samples/
// (e.g. tts-singing-Zarvox.wav) so hellsine.mjs can rotate through
// voices across the long mantra build.
//
// CLI:
//   node gen-tts-singing.mjs            # renders all voices in VOICES
//   node gen-tts-singing.mjs Alex       # renders just one voice

import { writeFileSync, readFileSync, mkdtempSync, mkdirSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SR = 48000;
const BPM = 182;
const SPB = 60 / BPM;

const VOICES = ["Zarvox", "Albert", "Fred", "Alex", "Samantha", "Daniel", "Bells"];
const SINGLE_VOICE = process.argv[2];

const ROOT_MEL = 62;
// LYRIC: "i hope that we get all the (MO-NEY|HO-NEY|BUN-NIES) we want"
// 11 syllables across the FULL THEME (incl. A3 pickup). The word in
// beats 7+8 morphs across repeats (mo-ney → ho-ney → bun-nies). "that"
// lands on the quick F4 sixteenth, "hope" gets the 1.5-beat D4 sustain,
// "ney" gets the 2-beat F4 sustain.
const VARIANTS = {
  money:   [["mo",   1.0,   5], ["ney",   2.0, 3]],
  honey:   [["ho",   1.0,   5], ["ney",   2.0, 3]],
  bunnies: [["bun",  1.0,   5], ["nies",  2.0, 3]],
};
const LINE_BASE = [
  ["i",    1.0,   ROOT_MEL - 5 ],   // A3 pickup
  ["hope", 1.5,   ROOT_MEL     ],   // D4 sustain
  ["that", 0.5,   ROOT_MEL + 3 ],   // F4 sixteenth
  ["we",   1.0,   ROOT_MEL + 7 ],   // A4
  ["get",  1.0,   ROOT_MEL + 7 ],   // A4
  ["all",  1.0,   ROOT_MEL + 8 ],   // Bb4
  ["the",  1.0,   ROOT_MEL + 7 ],   // A4
  // VARIANT slot — replaced per render (mo/ney | ho/ney | bun/nies)
  ["__v1", 1.0,   ROOT_MEL + 5 ],   // G4
  ["__v2", 2.0,   ROOT_MEL + 3 ],   // F4 SUSTAIN
  ["we",   1.0,   ROOT_MEL + 2 ],   // E4
  ["want", 1.0,   ROOT_MEL     ],   // D4
];
function lineForVariant(name) {
  const v = VARIANTS[name];
  return LINE_BASE.map((row) => {
    if (row[0] === "__v1") return [v[0][0], row[1], row[2]];
    if (row[0] === "__v2") return [v[1][0], row[1], row[2]];
    return row;
  });
}

function midiToPbas(m) {
  const ref = 60;
  const pbas = 36 + (m - ref) * 1.5;
  return Math.max(25, Math.min(65, Math.round(pbas)));
}

function loadAiffMonoF32(path) {
  // Always re-encode through ffmpeg to a sibling .mono.wav so we can read
  // the same way whether the source is .aiff or .wav.
  const wav = path.replace(/\.(aiff?|wav)$/i, ".mono.wav");
  const r = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
    "-i", path, "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", wav]);
  if (r.status !== 0) throw new Error(`ffmpeg failed on ${path}`);
  const buf = readFileSync(wav);
  let off = 12, dOff = 0, dLen = 0;
  while (off + 8 <= buf.length) {
    const id = buf.toString("ascii", off, off + 4);
    const sz = buf.readUInt32LE(off + 4);
    if (id === "data") { dOff = off + 8; dLen = sz; break; }
    off += 8 + sz + (sz & 1);
  }
  const frames = dLen / 2;
  const f = new Float32Array(frames);
  for (let i = 0; i < frames; i++) f[i] = buf.readInt16LE(dOff + i * 2) / 32768;
  return f;
}

function renderVoice(voice, variantName) {
  const LINE = lineForVariant(variantName);
  const tmp = mkdtempSync(`${tmpdir()}/tts-${voice}-${variantName}-`);
  const totalBeats = LINE.reduce((a, [, b]) => a + b, 0);
  const totalSec = totalBeats * SPB + 0.5;
  const N = Math.floor(totalSec * SR);
  const out = new Float32Array(N);
  let cursorBeats = 0;
  for (let i = 0; i < LINE.length; i++) {
    const [syl, beats, midi] = LINE[i];
    const pbas = midiToPbas(midi);
    const aiff = `${tmp}/syl-${i}.aiff`;
    const speakArg = `[[pbas ${pbas}]] [[pmod 0]] [[rate 180]] ${syl}`;
    const r = spawnSync("say", ["-v", voice, "-o", aiff, speakArg]);
    if (r.status !== 0) { console.error(`say failed for "${syl}" voice=${voice}`); return false; }
    // PITCH-PRESERVING TIME-STRETCH — atempo holds pitch constant while
    // scaling duration to fit the note slot. atempo is clamped to [0.5,2.0]
    // so chain it for extreme factors.
    const sylWav = `${tmp}/syl-${i}.wav`;
    const noteSec = beats * SPB;
    // probe say output duration
    const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "csv=p=0", aiff]);
    const sylSec = parseFloat(probe.stdout.toString().trim()) || noteSec;
    // factor > 1 ⇒ play faster (shorter); factor < 1 ⇒ play slower (longer)
    let factor = sylSec / noteSec;
    // (1) Trim leading silence first via ffmpeg.
    const trimmedAiff = `${tmp}/syl-${i}-trim.wav`;
    const r1 = spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error",
      "-i", aiff, "-af",
      "silenceremove=start_periods=1:start_threshold=-38dB:start_silence=0.005",
      "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", trimmedAiff]);
    if (r1.status !== 0) { console.error(`ffmpeg trim failed for "${syl}"`); return false; }
    // Re-probe trimmed duration (silence removal changes it).
    const probe2 = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
      "-of", "csv=p=0", trimmedAiff]);
    const sylSecTrim = parseFloat(probe2.stdout.toString().trim()) || noteSec;
    const timeRatio = noteSec / sylSecTrim;        // rubberband -t: >1 = slower
    // (2) ONE-PASS rubberband — stretch + pitch-shift simultaneously,
    // formant-preserving. Better vowel sustain than atempo for big
    // time-stretches because rubberband holds the vowel through the note
    // duration instead of slowing the whole utterance proportionally.
    // Pitch-lock — let pbas set the pitch via say's TTS engine and let
    // rubberband leave it alone (shift 0). The +12 lift we tried put
    // vocals an octave above the brass lead, breaking the unison feel.
    const shiftSemis = 0;
    const r3 = spawnSync("rubberband", [
      "-t", String(timeRatio.toFixed(4)),
      "-p", String(shiftSemis.toFixed(3)),
      "-F", "-c", "6",
      trimmedAiff, sylWav]);
    if (r3.status !== 0) { console.error(`rubberband failed for "${syl}"`); return false; }
    const samp = loadAiffMonoF32(sylWav);
    const noteN = Math.floor(noteSec * SR);
    const start = Math.floor(cursorBeats * SPB * SR);
    const fadeN = Math.floor(0.008 * SR);
    const playN = Math.min(noteN, samp.length);
    for (let j = 0; j < playN; j++) {
      const v = samp[j];
      let env = 1;
      if (j < fadeN) env = j / fadeN;
      if (playN - j < fadeN) env = (playN - j) / fadeN;
      if (start + j < N) out[start + j] += v * env;
    }
    cursorBeats += beats;
  }
  let peak = 0;
  for (let i = 0; i < N; i++) if (Math.abs(out[i]) > peak) peak = Math.abs(out[i]);
  const targetPeak = 0.71;
  const norm = peak > 0 ? targetPeak / peak : 1;
  const samples = Buffer.alloc(N * 2);
  for (let i = 0; i < N; i++) {
    let v = Math.round(out[i] * norm * 32767);
    v = Math.max(-32768, Math.min(32767, v));
    samples.writeInt16LE(v, i * 2);
  }
  const fmt = Buffer.alloc(44);
  fmt.write("RIFF", 0);
  fmt.writeUInt32LE(36 + samples.length, 4);
  fmt.write("WAVEfmt ", 8);
  fmt.writeUInt32LE(16, 16);
  fmt.writeUInt16LE(1, 20); fmt.writeUInt16LE(1, 22);
  fmt.writeUInt32LE(SR, 24); fmt.writeUInt32LE(SR * 2, 28);
  fmt.writeUInt16LE(2, 32); fmt.writeUInt16LE(16, 34);
  fmt.write("data", 36); fmt.writeUInt32LE(samples.length, 40);
  mkdirSync(`${LANE}/samples`, { recursive: true });
  const OUT = `${LANE}/samples/tts-singing-${voice}-${variantName}.wav`;
  writeFileSync(OUT, Buffer.concat([fmt, samples]));
  console.log(`✓ ${voice.padEnd(10)} ${variantName.padEnd(8)} · ${(N / SR).toFixed(2)}s → samples/tts-singing-${voice}-${variantName}.wav`);
  return true;
}

const list = SINGLE_VOICE ? [SINGLE_VOICE] : VOICES;
const variantNames = Object.keys(VARIANTS);
for (const v of list) {
  for (const variant of variantNames) {
    renderVoice(v, variant);
  }
}
