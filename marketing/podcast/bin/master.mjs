#!/usr/bin/env node
// master.mjs — the podcast mastering step (analogous to /pop's master stage).
//
// A clean, inspectable spoken-word master: high-pass rumble, a gentle glue
// compressor, 2-pass loudnorm to the podcast target (-16 LUFS integrated,
// -1.5 dBTP), and a true-peak limiter for safety. Same chain every episode.
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

// Mastering pre-chain = high-pass rumble + the episode's SUBSTRATE (the medium
// it's printed on: tone EQ + character + bus glue), before loudnorm/limiter.
const preChain = (substrate = DEFAULT_SUBSTRATE) =>
  ["highpass=f=70", ...substrateChain(substrate)].join(",");

export const TARGET = { I: -16, TP: -1.5, LRA: 11 };

// Measure loudness (LUFS/TP/LRA) with loudnorm's analysis pass.
export function measure(input, pre = preChain()) {
  // loudnorm prints its JSON analysis to stderr. `pre` is the pre-chain to
  // account for during the master's first pass; pass "" to measure a file raw.
  const af = (pre ? pre + "," : "") +
    `loudnorm=I=${TARGET.I}:TP=${TARGET.TP}:LRA=${TARGET.LRA}:print_format=json`;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-i", input, "-af", af, "-f", "null", "-"],
    { encoding: "utf8", maxBuffer: 64 * 1024 * 1024 });
  const out = r.stderr || "";
  const json = out.slice(out.lastIndexOf("{"), out.lastIndexOf("}") + 1);
  return JSON.parse(json);
}

// Master input → output on a given SUBSTRATE, with accurate 2-pass loudnorm
// + true-peak limiter.
export function master(input, output, substrate = DEFAULT_SUBSTRATE) {
  const pre = preChain(substrate);
  const m = measure(input, pre);
  const ln =
    `loudnorm=I=${TARGET.I}:TP=${TARGET.TP}:LRA=${TARGET.LRA}` +
    `:measured_I=${m.input_i}:measured_TP=${m.input_tp}:measured_LRA=${m.input_lra}` +
    `:measured_thresh=${m.input_thresh}:offset=${m.target_offset}:linear=true`;
  execFileSync("ffmpeg", [
    "-y", "-i", input, "-af",
    `${pre},${ln},alimiter=limit=0.891:level=false`,   // ~-1 dBTP safety
    "-ar", "44100", "-c:a", "libmp3lame", "-b:a", "256k", output,
  ], { stdio: "ignore" });
  return output;
}

// ── CLI ─────────────────────────────────────────────────────────────────
if (import.meta.url === `file://${process.argv[1]}`) {
  const a = process.argv.slice(2);
  if (a[0] === "check") {
    const m = measure(a[1], "");   // raw loudness, no pre-chain
    console.log(`${a[1].split("/").pop()}:`);
    console.log(`  integrated ${m.input_i} LUFS · true-peak ${m.input_tp} dBTP · LRA ${m.input_lra}`);
    console.log(`  (podcast target: ${TARGET.I} LUFS / ${TARGET.TP} dBTP)`);
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
