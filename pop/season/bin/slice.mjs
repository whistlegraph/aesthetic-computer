#!/usr/bin/env node
// slice.mjs — cut the /pop `season` sample bank.
//
// The source is the `h0t` whistlegraph work (posts.json tags six posts,
// spring 2022), and every take says the same three lines:
//
//     it's too hot / no it's not / now I'm back in season
//
// plus a sung "doo doo doo" walk between the argument and the tagline.
// Three takes feed the bank:
//
//   P  7079639110025088298  "all four seasons in 9 seconds" — the primary.
//      Cleanest full statement: chant, doo walk (F4 F4 G4 G4 E4 D4 C4),
//      sung tagline (E4 D4 C#4 B3 A3 — a descending pentachord that lands
//      on A, which is why the remix lives in A).
//   H  7080453509149134126  "spring flower" — the most-viewed take (28.3M),
//      and the one where "no it's not" is answered HIGH, ~A4. The argument
//      crosses registers here, not just people.
//   M  7087134943930846506  "springy vibesies" — a mid answer (E3→C3) and
//      a second doo walk for variety.
//
// TikTok blocks this IP, so the mp4s come from the AC asset mirror that
// whistlegraph.org's own posts.json points at. Same files, no substitution.
// Spans were placed against whisper-cli word timestamps (txt/<id>.json) and
// pyin — the receipts are analysis/harvest.json and analysis/melody.json.
//
// Writes 48 kHz mono 32-bit-float WAVs to pop/season/samples/ plus a
// tracked .manifest.json of every cut with its measured f0.
//
//   node pop/season/bin/slice.mjs

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const SAMPLES = resolve(LANE, "samples");
const SOURCE = resolve(LANE, "source");
const WAV = resolve(LANE, "wav");
const SR = 48_000;
const MIRROR = "https://assets.aesthetic.computer/whistlegraph/index/posts";

mkdirSync(SAMPLES, { recursive: true });
mkdirSync(SOURCE, { recursive: true });
mkdirSync(WAV, { recursive: true });

const sh = (cmd, args) => execFileSync(cmd, args, { stdio: ["ignore", "pipe", "pipe"] });

// [start, end] seconds in the take, plus the pyin note where one holds.
// Spoken chant hits carry no reliable pitch — their f0 is null on purpose.
const TAKES = {
  "7079639110025088298": {
    tag: "P",
    cuts: {
      "its-too-hot":   { span: [0.08, 2.10], f0: null, note: "spoken chant, one bar at 122" },
      "its":           { span: [0.08, 0.80], f0: null },
      "too":           { span: [0.76, 1.50], f0: null },
      "hot":           { span: [1.48, 2.10], f0: 178.4, note: "F3-ish shout" },
      "no-its-not":    { span: [2.08, 2.60], f0: 134.5, note: "≈C#3" },
      "no":            { span: [2.08, 2.22], f0: null },
      "not":           { span: [2.30, 2.60], f0: null },
      "chant-full":    { span: [0.08, 2.60], f0: null, note: "the whole argument" },
      "doo-run":       { span: [2.70, 4.95], f0: null, note: "F4 F4 G4 G4 E4 D4 C4" },
      "doo-f":         { span: [2.72, 3.31], f0: 345.0, note: "F4" },
      "doo-g":         { span: [3.31, 3.90], f0: 385.0, note: "G4" },
      "doo-ed":        { span: [3.90, 4.45], f0: 286.7, note: "E4→D4 fall" },
      "doo-c":         { span: [4.45, 4.90], f0: 258.4, note: "C4" },
      "season-line":   { span: [5.55, 8.62], f0: null, note: "E4 D4 C#4 B3 A3, lands on A" },
      "now-im":        { span: [5.60, 6.40], f0: 339.0, note: "E4" },
      "back-in":       { span: [6.45, 7.60], f0: 280.0, note: "C#4 territory" },
      "season":        { span: [7.55, 8.62], f0: 252.5, note: "B3 falling to A3" },
    },
  },
  "7080453509149134126": {
    tag: "H",
    cuts: {
      "its-too-hot-yell": { span: [0.12, 3.42], f0: null, note: "the long shouted version" },
      "hot-yell":         { span: [2.30, 3.42], f0: null },
      "no-its-not-high":  { span: [3.38, 4.10], f0: 449.6, note: "the A4 answer" },
      "season-b":         { span: [8.10, 10.40], f0: 160.8, note: "low season, E3-ish" },
    },
  },
  "7087134943930846506": {
    tag: "M",
    cuts: {
      "no-its-not-mid":  { span: [1.24, 2.26], f0: 167.0, note: "E3 falling to C3" },
      "doo-run-b":       { span: [2.48, 4.30], f0: null, note: "C#4 D#4 A#3 walk" },
      "season-line-b":   { span: [4.90, 7.95], f0: 270.5, note: "C#4 down to G3" },
    },
  },
};

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
// short raised-cosine ramps so no slice can ever click — same dressing
// the cult bank gets. These takes were shot outside; the dead air goes.
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

const manifest = {};
for (const [id, { tag, cuts }] of Object.entries(TAKES)) {
  const mp4 = resolve(SOURCE, `${id}.mp4`);
  if (!existsSync(mp4)) {
    console.log(`→ fetching ${id}.mp4 from the AC mirror`);
    sh("curl", ["-sL", "-o", mp4, `${MIRROR}/${id}.mp4`]);
  }
  const wav = resolve(WAV, `${id}.wav`);
  if (!existsSync(wav))
    sh("ffmpeg", ["-y", "-v", "error", "-i", mp4, "-ac", "1", "-ar", String(SR), wav]);

  const { samples, sampleRate } = readWavMono(wav);
  console.log(`→ slicing ${tag} ${id}`);
  for (const [name, { span: [t0, t1], f0, note }] of Object.entries(cuts)) {
    const a = Math.round(t0 * sampleRate);
    const b = Math.min(samples.length, Math.round(t1 * sampleRate));
    if (b <= a) { console.warn(`  ! ${name}: empty span`); continue; }
    const out = dress(samples.slice(a, b));
    writeWavF32(resolve(SAMPLES, `${name}.wav`), out);
    manifest[name] = {
      take: id, span: [t0, t1], dur: +(out.length / sampleRate).toFixed(3),
      ...(f0 ? { median_f0_hz: f0 } : {}), ...(note ? { note } : {}),
    };
    console.log(`  ${tag}  ${name.padEnd(18)} ${(out.length / sampleRate).toFixed(3)}s`
      + (f0 ? `  f0=${f0}` : ""));
  }
}

writeFileSync(resolve(SAMPLES, ".manifest.json"), JSON.stringify({
  work: "h0t — it's too hot / no it's not / now I'm back in season (Whistlegraph, spring 2022)",
  mirror: MIRROR,
  f0_provenance: "librosa.pyin over whisper word spans — analysis/harvest.json + analysis/melody.json",
  slices: manifest,
}, null, 2));
console.log(`✓ bank → ${SAMPLES}`);
