#!/usr/bin/env node
// slice.mjs — cut the /pop `cult` sample bank.
//
// Two sources feed the remix:
//
//   1. The cult whistlegraph itself — "The Three of Us Are in a Cult"
//      (@whistlegraph, 2022-01-20, TikTok 7055106286232325423). TikTok
//      blocks this IP, so the mp4 comes from the AC asset mirror that
//      whistlegraph.org already points `posts.json` at. Whisper +
//      an RMS/autocorrelation probe put three distinct vocal registers
//      on the six chant hits — one dash and one dot each from Jeffrey,
//      Camille and Alex — then the sung tagline.
//
//   2. ElevenLabs jeffrey-pvc, for the words the TikTok never said:
//      "i wanna" and "run it fast". Sliced by the free per-character
//      alignment /api/say returns with --timestamps (no STT guessing).
//
// Writes 48 kHz mono 32-bit-float WAVs to pop/cult/samples/.
//
//   node pop/cult/bin/slice.mjs

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");
const SAMPLES = resolve(LANE, "samples");
const SR = 48_000;

const CULT_ID = "7055106286232325423";
const CULT_MP4 = `https://assets.aesthetic.computer/whistlegraph/index/posts/${CULT_ID}.mp4`;
const CACHE = resolve(REPO, "toolchain/whistlegraph/downloads");

mkdirSync(SAMPLES, { recursive: true });
mkdirSync(CACHE, { recursive: true });

const sh = (cmd, args) => execFileSync(cmd, args, { stdio: ["ignore", "pipe", "pipe"] });

// ── Source A: the whistlegraph ────────────────────────────────────────
const mp4 = resolve(CACHE, `${CULT_ID}.mp4`);
if (!existsSync(mp4)) {
  console.log(`→ fetching cult mp4 from the AC mirror`);
  sh("curl", ["-sL", "-o", mp4, CULT_MP4]);
}
const cultWav = resolve(CACHE, `${CULT_ID}-48k.wav`);
sh("ffmpeg", ["-y", "-v", "error", "-i", mp4, "-ac", "1", "-ar", String(SR), cultWav]);

// [start, end] in seconds inside the 8.06 s clip. Registers measured by
// autocorrelation: ~127 Hz (Jeffrey), ~193 Hz (Alex), ~245 Hz (Camille).
const CULT_CUTS = {
  "dash-jeffrey":  [0.575, 1.070],
  "dash-camille":  [1.155, 1.460],
  "dash-alex":     [1.680, 2.010],
  "dot-jeffrey":   [2.730, 3.040],
  "dot-camille":   [3.250, 3.580],
  "dot-alex":      [3.750, 4.050],
  "three-of-us":   [4.500, 6.660],   // "the three of us are in a"
  "cult":          [6.800, 7.330],   // the word itself — the choir seed
  "chant-full":    [0.575, 4.060],   // all six hits, for the intro
  "tagline":       [4.500, 7.330],
};

// ── Source B: ElevenLabs jeffrey-pvc ──────────────────────────────────
// Word spans come straight from the alignment sidecars.
const VOX = resolve(LANE, "vox");
const evenSpans = (file, picks) => {
  const a = JSON.parse(readFileSync(resolve(VOX, `${file}.mp3.alignment.json`), "utf8"));
  const out = {};
  for (const [name, [i, j, padA = 0.03, padB = 0.06]] of Object.entries(picks)) {
    out[name] = [Math.max(0, a.words[i].fromMs / 1000 - padA), a.words[j].toMs / 1000 + padB];
  }
  return out;
};

const partsCuts = evenSpans("parts", {
  "i-wanna-a":    [0, 1],
  "i-wanna-b":    [2, 3],
  "run-it-fast":  [4, 6],
  "run-it-fast-long": [7, 9],
});
const hookCuts = evenSpans("hook", {
  "hook-spoken":  [0, 10],
  "eleven-dash":  [3, 3],
  "eleven-dotdot":[9, 10],
});

// v3's chorus, dictated line by line:
//   run real fast / i wanna hide away / i wanna dash / dot dot dash
// One generation ("chorus3"), one alignment, eight spans. "Away." is
// tracked alone at the end of the take so the held vowel of "hide awaaaay"
// comes from a syllable that was already free-standing.
const chorusCuts = existsSync(resolve(VOX, "chorus3.mp3.alignment.json"))
  ? evenSpans("chorus3", {
      "run-real-fast":   [0, 2],
      "iwanna-hide":     [3, 5],
      "iwanna-c":        [7, 8],
      "dash-eleven-c":   [9, 9],
      "dotdotdash":      [10, 12],
      "run-real-fast-b": [13, 15],
      "hide-away":       [16, 17],
      "away":            [18, 18],
    })
  : null;

// ── Cut ───────────────────────────────────────────────────────────────
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
// short raised-cosine ramps so no slice can ever click. @jeffrey's note:
// these takes need their dead air cut before they sit in a mix.
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

function cutFrom(wavPath, cuts, tag) {
  const { samples, sampleRate } = readWavMono(wavPath);
  for (const [name, [t0, t1]] of Object.entries(cuts)) {
    const a = Math.round(t0 * sampleRate);
    const b = Math.min(samples.length, Math.round(t1 * sampleRate));
    if (b <= a) { console.warn(`  ! ${name}: empty span`); continue; }
    const out = dress(samples.slice(a, b));
    writeWavF32(resolve(SAMPLES, `${name}.wav`), out);
    console.log(`  ${tag}  ${name.padEnd(18)} ${(out.length / sampleRate).toFixed(3)}s`);
  }
}

console.log("→ slicing the whistlegraph");
cutFrom(cultWav, CULT_CUTS, "wg");

for (const [file, cuts] of [["parts", partsCuts], ["hook", hookCuts],
  ...(chorusCuts ? [["chorus3", chorusCuts]] : [])]) {
  const src = resolve(VOX, `${file}.mp3`);
  if (!existsSync(src)) { console.warn(`  ! missing ${src} — run bin/say.mjs first`); continue; }
  const wav = resolve(VOX, `${file}-48k.wav`);
  sh("ffmpeg", ["-y", "-v", "error", "-i", src, "-ac", "1", "-ar", String(SR), wav]);
  console.log(`→ slicing ElevenLabs jeffrey-pvc: ${file}`);
  cutFrom(wav, cuts, "11");
}

console.log(`✓ bank → ${SAMPLES}`);
