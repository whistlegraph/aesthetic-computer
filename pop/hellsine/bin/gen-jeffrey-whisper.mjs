#!/usr/bin/env node
// gen-jeffrey-whisper.mjs — beat-align jeffrey-pvc per WORD using
// whisper-cli word-level timestamps. Each word's slice is rubberband-
// stretched to its target THEME note duration and pasted onto the beat
// grid, producing samples/jeffrey-vocal-{variant}.wav (4.0s, 48k mono).

import { writeFileSync, readFileSync, mkdtempSync, existsSync } from "node:fs";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SR = 48000;
const BPM = 182;
const SPB = 60 / BPM;
const WMODEL = `${process.env.HOME}/.whisper-models/ggml-base.en.bin`;

// LINE: "i hope that we get all the (money|honey|bunnies) we want" — 11
// syllables. Whisper returns 10 word boundaries; the 8th word (money/
// honey/bunnies) splits in half to become two syllables (mo+ney etc.).
// Beat durations match THEME[0..10].
// THEME[0..10] target MIDI notes (octave below the THEME for natural baritone fit).
// jeffrey's natural speech sits around MIDI 50 (D3-A3); shifting to (theme - 12)
// keeps the rubberband shift in the 0-8 semitone range so voice quality stays
// natural. This = AUTOTUNE: each syllable pitched to its target THEME chord tone.
// @jeffrey wanted jeffrey-pvc to "pitch to the note more aggressively"
// — the old targets sat a full octave below THEME (theme_midi - 12,
// roughly A2-Bb3), which left him reading as a low murmur underneath
// the brass. New targets land at theme_midi - 5 (a perfect fourth below
// THEME, A3-Bb4 register) so each syllable is unmistakably on the
// chord-tone the brass is playing, while still keeping jeffrey-pvc's
// voice quality intact via rubberband formant preservation.
const JEFFREY_BASE_MIDI = 50;
// -7 (perfect-fifth below THEME) sweet spot — deep enough to feel
// baritone but the rubberband shift stays small enough that voice
// quality + formants survive.
const TARGET_OFFSET = -7;

// LYRIC PHRASINGS — each phrasing maps the same 11-note THEME to a
// slightly different sentence. @jeffrey: "switch the phrasing up".
// Source mp3s exist at /tmp/jeffrey-<src>-<variant>.mp3 for each
// (lyric phrasing × variant word).
//   "that" → "I hope that we get all the [V] we want"
//   "you"  → "I hope that you get all the [V] you want"
//   "we"   → "I hope we get all the [V] we want"  (10 syllables, skip slot 3)
const PHRASINGS = {
  "that": {
    expected: ["I","hope","that","we","get","all","the","__V__","we","want"],
    syllables: [
      { syl: "i",    beats: 1.0, fromWord: 0, half: null,    midi: 57 + TARGET_OFFSET },
      { syl: "hope", beats: 1.5, fromWord: 1, half: null,    midi: 62 + TARGET_OFFSET },
      { syl: "that", beats: 1.0, fromWord: 2, half: null,    midi: 65 + TARGET_OFFSET },
      { syl: "we",   beats: 1.0, fromWord: 3, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "get",  beats: 1.0, fromWord: 4, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "all",  beats: 1.0, fromWord: 5, half: null,    midi: 70 + TARGET_OFFSET },
      { syl: "the",  beats: 1.0, fromWord: 6, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "v1",   beats: 1.0, fromWord: 7, half: "first", midi: 67 + TARGET_OFFSET },
      { syl: "v2",   beats: 1.5, fromWord: 7, half: "second",midi: 65 + TARGET_OFFSET },
      { syl: "we",   beats: 1.0, fromWord: 8, half: null,    midi: 64 + TARGET_OFFSET },
      { syl: "want", beats: 1.0, fromWord: 9, half: null,    midi: 62 + TARGET_OFFSET },
    ],
  },
  // 10-syllable "I hope you get all the [V] you want" — @jeffrey: this
  // is the canonical phrasing now. "hope" holds for 2 beats, absorbing
  // what was the "that" 0.5-beat slot + the prior 1.5-beat slot, so
  // the 10 syllables cover the 11-note antecedent via one melisma.
  "you": {
    expected: ["I","hope","you","get","all","the","__V__","you","want"],
    syllables: [
      { syl: "i",    beats: 1.0, fromWord: 0, half: null,    midi: 57 + TARGET_OFFSET },
      { syl: "hope", beats: 2.5, fromWord: 1, half: null,    midi: 62 + TARGET_OFFSET },
      { syl: "you",  beats: 1.0, fromWord: 2, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "get",  beats: 1.0, fromWord: 3, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "all",  beats: 1.0, fromWord: 4, half: null,    midi: 70 + TARGET_OFFSET },
      { syl: "the",  beats: 1.0, fromWord: 5, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "v1",   beats: 1.0, fromWord: 6, half: "first", midi: 67 + TARGET_OFFSET },
      { syl: "v2",   beats: 2.0, fromWord: 6, half: "second",midi: 65 + TARGET_OFFSET },
      { syl: "you",  beats: 1.0, fromWord: 7, half: null,    midi: 64 + TARGET_OFFSET },
      { syl: "want", beats: 1.0, fromWord: 8, half: null,    midi: 62 + TARGET_OFFSET },
    ],
  },
  "we": {
    // 10-syllable phrasing — no "that" / "you", "hope" stretches over
    // 2 beats to cover the missing slot. Maps to source /tmp/jeffrey-we-*.
    expected: ["I","hope","we","get","all","the","__V__","we","want"],
    syllables: [
      { syl: "i",    beats: 1.0, fromWord: 0, half: null,    midi: 57 + TARGET_OFFSET },
      { syl: "hope", beats: 2.5, fromWord: 1, half: null,    midi: 62 + TARGET_OFFSET },
      { syl: "we",   beats: 1.0, fromWord: 2, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "get",  beats: 1.0, fromWord: 3, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "all",  beats: 1.0, fromWord: 4, half: null,    midi: 70 + TARGET_OFFSET },
      { syl: "the",  beats: 1.0, fromWord: 5, half: null,    midi: 69 + TARGET_OFFSET },
      { syl: "v1",   beats: 1.0, fromWord: 6, half: "first", midi: 67 + TARGET_OFFSET },
      { syl: "v2",   beats: 2.0, fromWord: 6, half: "second",midi: 65 + TARGET_OFFSET },
      { syl: "we",   beats: 1.0, fromWord: 7, half: null,    midi: 64 + TARGET_OFFSET },
      { syl: "want", beats: 1.0, fromWord: 8, half: null,    midi: 62 + TARGET_OFFSET },
    ],
  },
};

function runWhisper(mp3Path, tmp) {
  // Convert to 16 kHz mono WAV (whisper-cli requirement)
  const wav16 = `${tmp}/wsp-in.wav`;
  spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-i", mp3Path, "-ar", "16000", "-ac", "1", wav16]);
  const outBase = `${tmp}/wsp-out`;
  const r = spawnSync("whisper-cli", ["-m", WMODEL, "-f", wav16, "-ml", "1",
    "-oj", "-of", outBase]);
  if (r.status !== 0) {
    console.error("whisper-cli failed:", r.stderr.toString());
    process.exit(1);
  }
  const json = JSON.parse(readFileSync(`${outBase}.json`, "utf8"));
  // Each entry has offsets.from / offsets.to (in ms) and text.
  // Filter to actual words (drop empty + punctuation-only).
  const raw = json.transcription
    .map((s) => ({ from: s.offsets.from / 1000, to: s.offsets.to / 1000, text: s.text.trim() }))
    .filter((w) => /[a-zA-Z]/.test(w.text));
  // GREEDY MATCH whisper tokens against the expected lyric sequence.
  // Whisper sometimes splits long words ("bunnies" → "b | unn | ies"),
  // so we concatenate consecutive tokens until they match an expected
  // word (case-insensitive, ignoring punctuation), then advance.
  return raw;
}
function matchToExpected(rawTokens, expectedWords) {
  const clean = (t) => t.toLowerCase().replace(/[^a-z]/g, "");
  const words = [];
  let j = 0;
  for (let e = 0; e < expectedWords.length; e++) {
    if (j >= rawTokens.length) break;
    const target = clean(expectedWords[e]);
    const start = j;
    let acc = "";
    // Greedily consume tokens that build target's prefix.
    while (j < rawTokens.length) {
      const tk = clean(rawTokens[j].text);
      if (acc + tk === target) { acc += tk; j++; break; }
      if (target.startsWith(acc + tk) && tk.length > 0) { acc += tk; j++; continue; }
      // Fallback: if we haven't matched anything, just take this token and bail.
      if (!acc) { acc = tk; j++; }
      break;
    }
    if (start < rawTokens.length) {
      words.push({
        from: rawTokens[start].from,
        to: rawTokens[Math.min(j - 1, rawTokens.length - 1)].to,
        text: acc,
      });
    }
  }
  return words;
}

function loadMonoF32(path) {
  const wav = path.replace(/(\.[a-z0-9]+)?$/i, ".mono.wav");
  spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-i", path, "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", wav]);
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

// NATURAL_ONLY (set per-call) skips the autotune pitch shift so the
// rendered sample is jeffrey's raw baritone with beat-locked syllables
// — used as a "natural lead" layer over the autotuned harmony version.
let NATURAL_ONLY = false;

function renderVariant(variant, phrasing = "that") {
  // Prefer LIVE recordings at pop/hellsine/samples/jeffrey-live/.
  // Otherwise pull from the appropriate ElevenLabs render at /tmp.
  // The "you" phrasing's ElevenLabs source is now the 10-syllable
  // "I hope you get..." take (jeffrey-iyou-*) — the old jeffrey-*.mp3
  // had "that" in it (11 syllables). 2026-05-25.
  const liveWav = `${LANE}/samples/jeffrey-live/jeffrey-live-${phrasing}-${variant}.wav`;
  const fallbackSrc = phrasing === "that" ? `jeffrey-that-${variant}`
                    : phrasing === "you"  ? `jeffrey-iyou-${variant}`
                    :                       `jeffrey-we-${variant}`;
  const fallbackMp3 = `/tmp/${fallbackSrc}.mp3`;
  const mp3 = existsSync(liveWav) ? liveWav : fallbackMp3;
  if (!existsSync(mp3)) { console.error(`✗ missing ${mp3}`); return; }
  console.log(`  source: ${existsSync(liveWav) ? "LIVE" : "elevenlabs"} → ${mp3}`);
  const phr = PHRASINGS[phrasing];
  const SYLLABLES = phr.syllables;
  const expected = phr.expected.map((w) => w === "__V__" ? variant : w);
  console.log(`▸ ${variant}/${phrasing}`);
  const tmp = mkdtempSync(`${tmpdir()}/jeffrey-wsp-${variant}-${phrasing}-`);
  const raw = runWhisper(mp3, tmp);
  const words = matchToExpected(raw, expected);
  console.log(`  whisper raw (${raw.length}): ${raw.map((w) => w.text).join(" | ")}`);
  console.log(`  matched   (${words.length}): ${words.map((w) => w.text).join(" | ")}`);
  if (words.length < expected.length) {
    console.error(`  ✗ expected ≥${expected.length} words, got ${words.length} — skipping`);
    return;
  }
  // Source WAV at 48k mono for slicing
  const srcWav48 = `${tmp}/src48.wav`;
  spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-i", mp3, "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", srcWav48]);

  const totalBeats = SYLLABLES.reduce((a, s) => a + s.beats, 0);
  const targetSec = totalBeats * SPB;
  const totalN = Math.ceil((targetSec + 0.5) * SR);
  const out = new Float32Array(totalN);

  // ── per-note OVERRIDE — wave-wizard ARM mode writes per-pad takes
  // to `<base>-note-N.wav`. If one exists for this (variant, phrasing)
  // and slot index N, use IT instead of the whisper-sliced syllable.
  // Pipeline rule: per-note overrides + full-take fallback compose into
  // one final jeffrey-vocal-<variant>[-suffix].wav that the engine loads.
  const liveBase = liveWav.replace(/\.wav$/, "");
  let cursorBeat = 0;
  for (let i = 0; i < SYLLABLES.length; i++) {
    const slot = SYLLABLES[i];
    const overridePath = `${liveBase}-note-${i}.wav`;
    let slice;
    let sylSec;
    if (existsSync(overridePath)) {
      // Use the held-pad take directly. Probe its duration so the time
      // stretch lands the slot exactly on the beat grid.
      slice = overridePath;
      const probe = spawnSync("ffprobe", ["-v", "error",
        "-show_entries", "format=duration", "-of", "csv=p=0", overridePath]);
      sylSec = parseFloat(probe.stdout.toString().trim()) || (slot.beats * SPB);
      console.log(`  slot ${i} "${slot.syl}" ← per-note override (${sylSec.toFixed(2)}s) ← ${overridePath.split("/").pop()}`);
    } else {
      const word = words[slot.fromWord];
      if (!word) { cursorBeat += slot.beats; continue; }
      let s0 = word.from, s1 = word.to;
      if (slot.half === "first")  s1 = (word.from + word.to) / 2;
      if (slot.half === "second") s0 = (word.from + word.to) / 2;
      slice = `${tmp}/syl-${i}.wav`;
      spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
        "-i", srcWav48, "-ss", String(s0.toFixed(4)), "-to", String(s1.toFixed(4)),
        "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", slice]);
      sylSec = s1 - s0;
    }
    const noteSec = slot.beats * SPB;
    // No more 2.2× cap — @jeffrey: "words aren't quite stretched out".
    // Let each syllable fill its full beat slot (sustained vowels on
    // the long notes like the 2-beat "ney" / "nies"). rubberband
    // formant preservation handles ratios up to ~6× without too much
    // smearing as long as -F is on.
    const timeRatio = noteSec / sylSec;
    const stretched = `${tmp}/syl-${i}-stretched.wav`;
    // AUTOTUNE — @jeffrey: "split, aligned, pitch shifted to match,
    // verified via whisper". Each syllable shifted to its THEME-derived
    // target MIDI (theme − 7 semis = baritone register). The matchToExpected
    // call above already verifies via whisper which token landed in each
    // slot (printed in the per-syllable log).
    const shiftSemis = slot.midi - JEFFREY_BASE_MIDI;
    spawnSync("rubberband", ["-t", String(timeRatio.toFixed(4)),
      "-p", String(shiftSemis), "-F", "-c", "6", slice, stretched]);
    const samp = loadMonoF32(stretched);
    const startSample = Math.floor(cursorBeat * SPB * SR);
    const noteN = Math.floor(noteSec * SR);
    const playN = Math.min(noteN, samp.length, totalN - startSample);
    const fadeN = Math.floor(0.010 * SR);
    for (let j = 0; j < playN; j++) {
      let env = 1;
      if (j < fadeN) env = j / fadeN;
      if (playN - j < fadeN) env = (playN - j) / fadeN;
      out[startSample + j] += samp[j] * env;
    }
    console.log(`  ${i.toString().padStart(2)} "${slot.syl}" (${sylSec.toFixed(2)}s, ×${timeRatio.toFixed(2)}) → beat ${cursorBeat} (${noteSec.toFixed(2)}s)`);
    cursorBeat += slot.beats;
  }
  let peak = 0;
  for (const v of out) if (Math.abs(v) > peak) peak = Math.abs(v);
  const norm = peak > 0 ? 0.85 / peak : 1;
  const samples = Buffer.alloc(totalN * 2);
  for (let i = 0; i < totalN; i++) {
    const v = Math.max(-32768, Math.min(32767, Math.round(out[i] * norm * 32767)));
    samples.writeInt16LE(v, i * 2);
  }
  const hdr = Buffer.alloc(44);
  hdr.write("RIFF", 0); hdr.writeUInt32LE(36 + samples.length, 4);
  hdr.write("WAVEfmt ", 8); hdr.writeUInt32LE(16, 16);
  hdr.writeUInt16LE(1, 20); hdr.writeUInt16LE(1, 22);
  hdr.writeUInt32LE(SR, 24); hdr.writeUInt32LE(SR * 2, 28);
  hdr.writeUInt16LE(2, 32); hdr.writeUInt16LE(16, 34);
  hdr.write("data", 36); hdr.writeUInt32LE(samples.length, 40);
  // File names: phrasing "that" stays at the legacy path so existing
  // engine code keeps loading the same names; other phrasings get a
  // suffix so the engine can rotate through them.
  const suffix = phrasing === "that" ? "" : `-${phrasing}`;
  const outPath = `${LANE}/samples/jeffrey-vocal-${variant}${suffix}.wav`;
  writeFileSync(outPath, Buffer.concat([hdr, samples]));
  console.log(`✓ wrote ${outPath} (${(totalN / SR).toFixed(2)}s)\n`);
}

const variants = process.argv.slice(2);
const list = variants.length ? variants : ["money", "honey", "bunnies"];
for (const v of list) {
  for (const phrasing of ["that", "you", "we"]) {
    renderVariant(v, phrasing);
  }
}
