#!/usr/bin/env node
// slice.mjs — cut the /pop `loner` sample bank.
//
// One work, four takes off the assets mirror (posts.json tags NINETEEN
// posts with the `lonr` work; these are the four the harvest measured):
//
//   f-  7108062006980201771  Ten Whistlegraphs / Feral File — clean solo
//                            voice, the whole lyric, ~80 BPM, A# minor
//                            sitting ~+30 cents sharp of A440
//   n-  7021262898479549702  the 13.8M "not again!" take — Jeffrey asks
//                            "Camille, are you doing emo whistlegraphs
//                            again?" and Camille answers by singing it
//   o-  6988619239657622790  the origin take — "Here's a whistlegraph by
//                            Camille called loner. Ready?"
//   s-  6988954628167585030  the 1.4M solo take, a lower register
//
// The lyric, whole:
//
//     sitting curled up in myself, i think of a stone,
//     just waiting very patiently for time to pass
//
// Word timings come from whisper.cpp (bin/analyze.py wrote them to
// harvest.json with per-word pyin f0); the spans below are those words
// with small pads, and each slice's manifest entry carries the median f0
// of the words it contains. Writes 48 kHz mono 32-bit-float WAVs to
// pop/loner/samples/ plus samples/.manifest.json (tracked).
//
//   node pop/loner/bin/slice.mjs

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SAMPLES = resolve(LANE, "samples");
const SOURCE = resolve(LANE, "source");
const SR = 48_000;

mkdirSync(SAMPLES, { recursive: true });
mkdirSync(SOURCE, { recursive: true });

const sh = (cmd, args) => execFileSync(cmd, args, { stdio: ["ignore", "pipe", "pipe"] });

// ── sources ───────────────────────────────────────────────────────────
const TAKES = {
  f: "7108062006980201771",
  n: "7021262898479549702",
  o: "6988619239657622790",
  s: "6988954628167585030",
};

for (const id of Object.values(TAKES)) {
  const mp4 = resolve(SOURCE, `${id}.mp4`);
  if (!existsSync(mp4)) {
    console.log(`→ fetching ${id} from the AC mirror`);
    sh("curl", ["-sL", "-o", mp4,
      `https://assets.aesthetic.computer/whistlegraph/index/posts/${id}.mp4`]);
  }
  const wav = resolve(SOURCE, `${id}-48k.wav`);
  if (!existsSync(wav))
    sh("ffmpeg", ["-y", "-v", "error", "-i", mp4, "-ac", "1", "-ar", String(SR), wav]);
}

// ── cuts ──────────────────────────────────────────────────────────────
// [take, start, end, "the words"] — seconds in the take's own clock.
const CUTS = {
  // the Feral File take: every phrase, plus the joins the melody actually
  // sings through, plus the whole line for the unaccompanied ending
  "f-sitting-curled":     ["f",  0.28,  5.80, "sitting curled up in myself"],
  "f-i-think":            ["f",  5.70,  7.50, "i think"],
  "f-of-a-stone":         ["f",  7.40, 11.35, "of a stone"],
  "f-think-stone":        ["f",  5.70, 11.35, "i think of a stone"],
  "f-stone":              ["f",  8.55, 11.35, "stone"],
  "f-waiting-patiently":  ["f", 11.35, 18.85, "just waiting very patiently"],
  "f-just-waiting":       ["f", 11.35, 14.75, "just waiting"],
  "f-very-patiently":     ["f", 14.68, 18.85, "very patiently"],
  "f-for-time-to-pass":   ["f", 18.78, 25.20, "for time to pass"],
  "f-pass":               ["f", 23.05, 25.40, "pass"],
  "f-whole-line":         ["f",  0.28, 25.45, "the whole lyric, one take"],

  // the "not again!" take: the spoken question, the sung answer, and the
  // spoken button at the very end
  "n-emo-again":          ["n",  0.05,  2.62, "Camille, are you doing emo whistlegraphs again? (spoken)"],
  "n-getting-curled":     ["n",  3.08,  8.40, "getting curled up in myself i think"],
  "n-of-a-stone":         ["n",  9.10, 11.25, "of a stone"],
  "n-stone-waiting":      ["n",  9.10, 17.40, "of a stone just waiting very patiently"],
  "n-for-time-to-pass":   ["n", 18.82, 22.40, "for time to pass"],
  "n-i-knew-it":          ["n", 23.02, 24.40, "i knew it (spoken)"],

  // the origin take: the introduction, and the three-friends line for later
  "o-heres-loner":        ["o",  0.18,  4.50, "here's a whistlegraph by camille called loner — ready? (spoken)"],
  "o-whole-line":         ["o",  4.55, 25.90, "the whole lyric, performed with jeffrey and alex"],

  // the solo take, whole — a lower register, kept for a later version
  "s-whole-line":         ["s",  0.00, 20.45, "the whole lyric, roomier and lower"],
};

// ── dress + write (cult's slicer, unchanged) ──────────────────────────
function writeWavF32(path, data) {
  const bytes = data.length * 4;
  const buf = Buffer.alloc(44 + bytes);
  buf.write("RIFF", 0, "ascii"); buf.writeUInt32LE(36 + bytes, 4); buf.write("WAVE", 8, "ascii");
  buf.write("fmt ", 12, "ascii"); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 4, 28);
  buf.writeUInt16LE(4, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36, "ascii"); buf.writeUInt32LE(bytes, 40);
  for (let i = 0; i < data.length; i++) buf.writeFloatLE(data[i], 44 + i * 4);
  writeFileSync(path, buf);
}

// Trim leading/trailing near-silence, normalize, and top-and-tail with
// short raised-cosine ramps so no slice can ever click.
function dress(seg) {
  const th = 0.012;
  let a = 0, b = seg.length - 1;
  while (a < b && Math.abs(seg[a]) < th) a++;
  while (b > a && Math.abs(seg[b]) < th) b--;
  a = Math.max(0, a - Math.round(0.004 * SR));
  b = Math.min(seg.length - 1, b + Math.round(0.030 * SR));
  const out = seg.slice(a, b + 1);
  let peak = 0;
  for (const v of out) peak = Math.max(peak, Math.abs(v));
  const g = peak > 1e-6 ? 0.90 / peak : 1;
  const ramp = Math.round(0.006 * SR);
  const tail = Math.round(0.012 * SR);
  for (let i = 0; i < out.length; i++) {
    let w = 1;
    if (i < ramp) w = 0.5 - 0.5 * Math.cos((Math.PI * i) / ramp);
    const t = out.length - 1 - i;
    if (t < tail) w *= 0.5 - 0.5 * Math.cos((Math.PI * t) / tail);
    out[i] = out[i] * g * w;
  }
  return out;
}

// ── cut, and receipt each slice with the harvest's word reads ─────────
const harvest = existsSync(resolve(LANE, "harvest.json"))
  ? JSON.parse(readFileSync(resolve(LANE, "harvest.json"), "utf8"))
  : {};

const manifest = {};
const decoded = {};
for (const [name, [take, t0, t1, words]] of Object.entries(CUTS)) {
  const id = TAKES[take];
  decoded[id] ??= readWavMono(resolve(SOURCE, `${id}-48k.wav`));
  const { samples, sampleRate } = decoded[id];
  const a = Math.round(t0 * sampleRate);
  const b = Math.min(samples.length, Math.round(t1 * sampleRate));
  if (b <= a) { console.warn(`  ! ${name}: empty span`); continue; }
  const out = dress(samples.slice(a, b));
  writeWavF32(resolve(SAMPLES, `${name}.wav`), out);

  const reads = (harvest[id]?.word_timestamps ?? [])
    .filter((w) => w.f0_hz && w.start >= t0 - 0.05 && w.end <= t1 + 0.05);
  const f0s = reads.map((w) => w.f0_hz).sort((x, y) => x - y);
  const med = f0s.length ? f0s[f0s.length >> 1] : null;
  manifest[name] = {
    source: id, start: t0, end: t1, words,
    dur: +(out.length / sampleRate).toFixed(3),
    median_f0_hz: med,
    word_f0: reads.map((w) => ({ t: w.t.trim(), start: w.start, end: w.end,
      f0_hz: w.f0_hz, note: w.note })),
  };
  console.log(`  ${take}  ${name.padEnd(22)} ${(out.length / sampleRate).toFixed(3)}s`
    + (med ? `  f0~${med}` : ""));
}

writeFileSync(resolve(SAMPLES, ".manifest.json"), JSON.stringify(manifest, null, 1));
console.log(`✓ bank → ${SAMPLES} (${Object.keys(manifest).length} slices + .manifest.json)`);
