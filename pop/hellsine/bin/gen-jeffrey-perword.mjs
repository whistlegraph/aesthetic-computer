#!/usr/bin/env node
// gen-jeffrey-perword.mjs — split the existing jeffrey-pvc raw recordings
// (/tmp/jeffrey-that-{variant}.mp3) into 11 syllables via silencedetect,
// then time-stretch each slice (rubberband, formant-preserving) to its
// target THEME note duration, and stitch back at the exact beat grid.
//
// Output: pop/hellsine/samples/jeffrey-vocal-{variant}.wav  (4.4s, 48k mono)
// Variants: money, honey, bunnies.

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

// THEME note durations in beats (i hope that we get all the mo ney we want)
const NOTE_BEATS = [1.0, 1.5, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0];
const TOTAL_BEATS = NOTE_BEATS.reduce((a, b) => a + b, 0);
const TARGET_SEC = TOTAL_BEATS * SPB;          // ≈ 4.0s

function detectWords(mp3Path, expectedN, tmp) {
  // Convert to wav first
  const wav = `${tmp}/raw.wav`;
  spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-i", mp3Path, "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", wav]);
  // Try silencedetect with progressively lower thresholds until we get
  // close to expectedN words.
  for (const thr of [-32, -36, -40, -44, -48, -52]) {
    const sd = spawnSync("ffmpeg", ["-hide_banner", "-i", wav, "-af",
      `silencedetect=noise=${thr}dB:d=0.05`, "-f", "null", "-"]);
    const out = sd.stderr.toString();
    const ends = [...out.matchAll(/silence_end:\s*([\d.]+)/g)].map((m) => parseFloat(m[1]));
    const starts = [...out.matchAll(/silence_start:\s*([\d.]+)/g)].map((m) => parseFloat(m[1]));
    // Word boundaries: speech starts at each silence_end and ends at each silence_start
    const dur = parseFloat(spawnSync("ffprobe", ["-v","error","-show_entries","format=duration",
      "-of","csv=p=0", wav]).stdout.toString().trim());
    // Build word spans
    const spans = [];
    let speakStart = 0;
    if (out.match(/silence_start:\s*0/)) speakStart = ends[0] || 0;
    let i = 0, j = (out.match(/silence_start:\s*0/) ? 1 : 0);
    while (j < ends.length || i < starts.length) {
      const ws = j === 0 ? 0 : ends[j - 1];
      const we = starts[i] !== undefined ? starts[i] : dur;
      if (we > ws) spans.push([ws, we]);
      i++; j++;
      if (i >= starts.length && j > ends.length) break;
    }
    // dedupe
    const cleaned = spans.filter((s, k, arr) => !k || s[0] !== arr[k - 1][0]).filter((s) => s[1] - s[0] > 0.04);
    if (cleaned.length >= expectedN) {
      // Take first expectedN
      console.log(`  thr=${thr}dB → ${cleaned.length} word spans, taking first ${expectedN}`);
      return { wav, dur, spans: cleaned.slice(0, expectedN) };
    }
    if (cleaned.length === expectedN - 1 || cleaned.length === expectedN - 2) {
      console.log(`  thr=${thr}dB → ${cleaned.length} spans (close, using)`);
      // Need to subdivide longest spans to reach expectedN
      while (cleaned.length < expectedN) {
        const idx = cleaned.reduce((bi, sp, ix) => (sp[1] - sp[0]) > (cleaned[bi][1] - cleaned[bi][0]) ? ix : bi, 0);
        const sp = cleaned[idx];
        const mid = (sp[0] + sp[1]) / 2;
        cleaned.splice(idx, 1, [sp[0], mid], [mid, sp[1]]);
      }
      return { wav, dur, spans: cleaned };
    }
  }
  throw new Error(`couldn't detect ${expectedN} words in ${mp3Path}`);
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

function renderVariant(variant) {
  const mp3 = `/tmp/jeffrey-that-${variant}.mp3`;
  if (!existsSync(mp3)) {
    console.error(`✗ missing ${mp3} — run pop/bin/say.mjs first`);
    return;
  }
  console.log(`▸ ${variant}`);
  const tmp = mkdtempSync(`${tmpdir()}/jeffrey-perword-${variant}-`);
  const { wav: srcWav, spans } = detectWords(mp3, NOTE_BEATS.length, tmp);
  // Per-syllable: extract, rubberband-stretch to note duration, place at beat
  const totalN = Math.ceil((TARGET_SEC + 0.5) * SR);
  const out = new Float32Array(totalN);
  let cursorBeat = 0;
  for (let i = 0; i < NOTE_BEATS.length; i++) {
    const [s0, s1] = spans[i];
    const noteSec = NOTE_BEATS[i] * SPB;
    const slice = `${tmp}/syl-${i}.wav`;
    spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
      "-i", srcWav, "-ss", String(s0.toFixed(4)), "-to", String(s1.toFixed(4)),
      "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s16le", slice]);
    const sylSec = s1 - s0;
    const timeRatio = noteSec / sylSec;
    const stretched = `${tmp}/syl-${i}-stretched.wav`;
    spawnSync("rubberband", ["-t", String(timeRatio.toFixed(4)),
      "-p", "5", "-F", "-c", "6", slice, stretched]);
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
    console.log(`  ${i.toString().padStart(2)} syl ${s0.toFixed(2)}-${s1.toFixed(2)} (${sylSec.toFixed(2)}s) → beat ${cursorBeat} (${noteSec.toFixed(2)}s)`);
    cursorBeat += NOTE_BEATS[i];
  }
  // Normalize + write
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
  const outPath = `${LANE}/samples/jeffrey-vocal-${variant}.wav`;
  writeFileSync(outPath, Buffer.concat([hdr, samples]));
  console.log(`✓ wrote ${outPath} (${(totalN / SR).toFixed(2)}s)`);
}

const variants = process.argv.slice(2);
const list = variants.length ? variants : ["money", "honey", "bunnies"];
for (const v of list) renderVariant(v);
