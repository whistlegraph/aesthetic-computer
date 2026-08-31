#!/usr/bin/env node
// master.mjs — the podcast mastering step (analogous to /pop's master stage).
//
// August '26: the episode master follows the /pop house law learned on the
// wax/FM lane (pop/loner/c/cut-wax.sh): a voice-tuned MATERIAL chain, then
// MEASURE → one static dB → true-peak limiter. Never a second loudnorm — the
// dynamic 2-pass loudnorm rewrote gain across the program; the law prints the
// whole episode at one gain so the reading's own dynamics survive.
//
// The material chain, voice-tuned (no wow — vibrato is seasick on speech):
//   1. BASS MONO below 120 Hz — keeps the bed's kick centered.
//   2. WIDTH + MOTION above 120 Hz — sides lifted slightly, and a very slow
//      L/R drift so the image breathes instead of sitting frozen.
//   3. MATERIAL — a gentle tanh soft-clip (tape density) + exciter air.
//   4. DENSITY — one program compressor gluing voice and bed together.
//   5. CEILING — lowpass at 15 kHz, highpass at 40 Hz (the FM print).
//
// Usage:
//   node bin/master.mjs <in.mp3|wav> [out.mp3]   # master a file
//   node bin/master.mjs check <file>              # print its loudness (LUFS/TP)
//   import { master, measure } from "./master.mjs"

import { execFileSync, spawnSync } from "node:child_process";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { substrateChain, DEFAULT_SUBSTRATE } from "../lib/substrates.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// Substrate pre-chain (the medium the episode is printed on: tone EQ +
// character + bus glue), applied before the material chain and the law.
const preChain = (substrate = DEFAULT_SUBSTRATE) =>
  ["highpass=f=70", ...substrateChain(substrate)].join(",");

// Voice-tuned wax/FM material chain (a filter_complex graph — the bass-mono
// crossover needs a split).
const MATERIAL =
  "acrossover=split=120:order=4th[low][high];" +
  "[low]pan=stereo|c0=0.5*c0+0.5*c1|c1=0.5*c0+0.5*c1[lowm];" +
  "[high]stereotools=slev=1.15,apulsator=hz=0.06:amount=0.08:mode=sine," +
  "aexciter=amount=0.5:drive=4:blend=0:freq=7500[hip];" +
  "[lowm][hip]amix=inputs=2:normalize=0," +
  "volume=0.8dB,asoftclip=type=tanh,volume=-0.6dB," +
  "acompressor=threshold=0.28:ratio=1.8:attack=10:release=200:makeup=1.2:knee=8," +
  "highpass=f=40,lowpass=f=15000";

// The law: one integrated-loudness target, one static gain, one limiter.
// -14 LUFS integrated (the streaming print, a half-LU shy of the /pop release
// law's -13.5 so speech peaks keep headroom) · limiter at 0.82 ≈ -1.7 dBTP.
export const TARGET = { I: -14, TP: -1.7, LRA: 9 };
const LIMIT = 0.82;

// Measure loudness (LUFS/TP/LRA) with loudnorm's analysis pass.
export function measure(input, pre = "") {
  // loudnorm prints its JSON analysis to stderr. `pre` is a filter prefix to
  // account for before measuring; pass "" to measure a file raw.
  const af = (pre ? pre + "," : "") +
    `loudnorm=I=${TARGET.I}:TP=${TARGET.TP}:LRA=${TARGET.LRA}:print_format=json`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-i", input, "-af", af, "-f", "null", "-"],
    { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
  const out = r.stderr || "";
  const json = out.slice(out.lastIndexOf("{"), out.lastIndexOf("}") + 1);
  return JSON.parse(json);
}

// Master input → output: substrate + material chain, then the law.
export function master(input, output, substrate = DEFAULT_SUBSTRATE) {
  const waxed = output.replace(/\.(mp3|wav)$/, ".wax-pre.wav");
  execFileSync("ffmpeg", [
    "-y", "-i", input,
    "-filter_complex", `[0:a]${preChain(substrate)},${MATERIAL}[out]`,
    "-map", "[out]", "-ar", "44100", "-c:a", "pcm_s24le", waxed,
  ], { stdio: "ignore" });

  const m = measure(waxed);
  const gain = (TARGET.I - Number(m.input_i)).toFixed(2);
  console.log(`  measured I=${m.input_i} → static ${gain} dB, then limit`);

  execFileSync("ffmpeg", [
    "-y", "-i", waxed, "-af",
    `volume=${gain}dB,alimiter=limit=${LIMIT}:attack=5:release=100:level=disabled`,
    "-ar", "44100", "-c:a", "libmp3lame", "-b:a", "256k", output,
  ], { stdio: "ignore" });
  execFileSync("rm", ["-f", waxed]);
  return output;
}

// ── CLI ─────────────────────────────────────────────────────────────────
if (import.meta.url === `file://${process.argv[1]}`) {
  const a = process.argv.slice(2);
  if (a[0] === "check") {
    const m = measure(a[1], "");   // raw loudness, no pre-chain
    console.log(`${a[1].split("/").pop()}:`);
    console.log(`  integrated ${m.input_i} LUFS · true-peak ${m.input_tp} dBTP · LRA ${m.input_lra}`);
    console.log(`  (episode law: ${TARGET.I} LUFS / ${TARGET.TP} dBTP, static gain + limit)`);
  } else if (a[0]) {
    const si = a.indexOf("--substrate");
    const substrate = si >= 0 ? a[si + 1] : DEFAULT_SUBSTRATE;
    const pos = a.filter((x, i) => !x.startsWith("--") && i !== si + 1);
    const input = resolve(process.cwd(), pos[0]);
    const output = pos[1] ? resolve(process.cwd(), pos[1]) : input.replace(/\.(mp3|wav)$/, ".mastered.mp3");
    console.log(`▸ mastering → ${output} (substrate: ${substrate})`);
    master(input, output, substrate);
    const m = measure(output, "");
    console.log(`✓ ${m.input_i} LUFS · ${m.input_tp} dBTP`);
  } else {
    console.error("usage: master.mjs <in> [out] [--substrate name] | check <file>");
    process.exit(1);
  }
}
