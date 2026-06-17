#!/usr/bin/env node
// analyze.mjs — look at the SHAPE of the vocal stitch instead of guessing.
//
//   node c/analyze.mjs --clip c/vocals/dora-0-v0.wav     # onset shape of one clip
//   node c/analyze.mjs --stem c/out/c-voc-stem.wav       # junction shapes per phrase
//
// stem mode knows the phrase grid (melody.json) and reports, for each of the
// first few phrases: the america→computer junction dip, whether computer is
// at full level BEFORE the barline (the kick spot), and dora's attack slope.

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { readWavMono } from "../../lib/wav.mjs";

const SR = 48_000;
const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const melody = JSON.parse(readFileSync(join(ROOT, "melody.json"), "utf8"));
const BEAT = 60 / melody.bpm;

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) flags[argv[i].slice(2)] = argv[i + 1];
}

function envelope(samples, win = 0.01) {
  const W = Math.floor(win * SR);
  const out = [];
  for (let i = 0; i + W <= samples.length; i += W) {
    let e = 0;
    for (let j = i; j < i + W; j++) e += samples[j] * samples[j];
    out.push(Math.sqrt(e / W));
  }
  return out;
}

function spark(env, hi = null) {
  const blocks = " ▁▂▃▄▅▆▇█";
  const mx = hi ?? Math.max(...env, 1e-9);
  return env.map((v) => blocks[Math.min(8, Math.floor((v / mx) * 8.999))]).join("");
}

if (flags.clip) {
  const { samples } = readWavMono(flags.clip);
  const env = envelope(samples);
  const peak = Math.max(...env);
  console.log(`# ${flags.clip}  (${(samples.length / SR).toFixed(2)}s, env peak ${peak.toFixed(3)})`);
  console.log(`  first 400ms : ${spark(env.slice(0, 40), peak)}`);
  console.log(`  whole clip  : ${spark(env)}`);
  // attack: ms to reach 50% of peak
  const half = env.findIndex((v) => v > peak * 0.5);
  console.log(`  attack→50% peak: ${half * 10}ms`);
  process.exit(0);
}

const stemPath = flags.stem ?? join(HERE, "out", "c-voc-stem.wav");
const { samples } = readWavMono(stemPath);
const env = envelope(samples);
const E = (sec) => env[Math.floor(sec / 0.01)] ?? 0;

// phrase grid: phrases run back-to-back from bar 0, 2 bars each
const lens = melody.hook.beats_per_word;
const onsets = [0];
for (let i = 0; i < lens.length - 1; i++) onsets.push(onsets[i] + lens[i] * BEAT);
const phraseLen = lens.reduce((a, c) => a + c, 0) * BEAT;

console.log(`# vocal stem ${stemPath}`);
console.log(`# phrase ${phraseLen.toFixed(2)}s · computer @ +${onsets[1].toFixed(2)}s · dora @ +${onsets[2].toFixed(2)}s\n`);

const nPhrases = Number(flags.phrases ?? 6);
for (let p = 0; p < nPhrases; p++) {
  const t0 = p * phraseLen;
  if ((t0 + phraseLen) * SR > samples.length) break;
  const cAt = t0 + onsets[1], dAt = t0 + onsets[2];

  // junction window: ±0.6s around computer onset
  const jEnv = [];
  for (let t = cAt - 0.6; t < cAt + 0.4; t += 0.01) jEnv.push(E(t));
  const before = Math.max(...jEnv.slice(0, 30));      // america vowel level
  const after = Math.max(...jEnv.slice(70));          // computer settled level
  const floor = Math.min(before, after);
  // a HOLE is a continuous run of near-silence in the splice zone — natural
  // intra-word dips are brief; a stitch gap is long.
  let hole = 0, run = 0;
  for (const v of jEnv.slice(30, 70)) {
    if (v < floor * 0.18) { run++; hole = Math.max(hole, run); } else run = 0;
  }
  const holeMs = hole * 10;
  // bloom: computer must be audibly arriving before the barline
  let bloom = 0;
  for (let t = cAt - 0.35; t < cAt - 0.08; t += 0.01) bloom = Math.max(bloom, E(t));
  const bloomPct = after > 0 ? (bloom / after) * 100 : 0;

  // dora attack: rise over the first 40ms vs its peak in 300ms
  const dPeak = Math.max(...envelope(samples.subarray(Math.floor(dAt * SR), Math.floor((dAt + 0.3) * SR))));
  const d40 = E(dAt + 0.03);
  const dAtk = dPeak > 0 ? (d40 / dPeak) * 100 : 0;

  // syllable pulses in the america region: local maxima ≥45% of the
  // region peak, ≥120ms apart. "a-ME-ri-ca" should give ≥3.
  const aEnv = [];
  for (let t = t0; t < t0 + 1.9; t += 0.01) aEnv.push(E(t));
  const aPeak = Math.max(...aEnv);
  let syl = 0, lastP = -99;
  for (let i = 2; i < aEnv.length - 2; i++) {
    if (aEnv[i] > aPeak * 0.45 && aEnv[i] >= aEnv[i - 1] && aEnv[i] >= aEnv[i + 1]
        && aEnv[i - 2] < aEnv[i] && i - lastP >= 12) { syl++; lastP = i; }
  }

  console.log(`phrase ${p} @ ${t0.toFixed(1)}s · america pulses: ${syl}`);
  console.log(`  junction  : ${spark(jEnv)}`);
  console.log(`  hole ${holeMs}ms · computer bloom ${bloomPct.toFixed(0)}% of settled before barline · dora 30ms attack ${dAtk.toFixed(0)}% of peak`);
  const ok = holeMs <= 60 && bloomPct > 25 && dAtk > 35;
  console.log(`  ${ok ? "✓ stitch reads continuous" : "✗ NEEDS WORK" + (holeMs > 60 ? " [hole in junction]" : "") + (bloomPct <= 25 ? " [computer late]" : "") + (dAtk <= 35 ? " [dora attack soft]" : "")}\n`);
}
