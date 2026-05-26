#!/usr/bin/env node
// build pitch / stretch / reverse / chipmunk variations for every utterance.
//
// for each utterances/<group>/<name>.wav we produce:
//   variations/<group>/<name>/pitch+3.wav  pitch+5.wav  pitch+7.wav  pitch-5.wav  pitch-12.wav
//   variations/<group>/<name>/stretch0.75.wav  stretch1.5.wav  stretch2.0.wav
//   variations/<group>/<name>/chipmunk+5.wav   chipmunk+12.wav  chipmunk-12.wav
//   variations/<group>/<name>/reverse.wav
//
// pitch-preserved shifts use the asetrate-then-atempo trick (no librubberband
// dependency). chipmunk = asetrate only (pitch + tempo both shift).
//
// usage:
//   node bin/variations.mjs              # all utterance groups
//   node bin/variations.mjs america      # one group
//   node bin/variations.mjs --force      # regenerate even if outputs exist

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync, statSync } from "node:fs";
import { basename, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = dirname(here);
const uttRoot = join(root, "utterances");
const varRoot = join(root, "variations");

// semitone → ratio. 2^(n/12).
const semi = (n) => Math.pow(2, n / 12);

// pitch-preserved shift: asetrate moves pitch+tempo, then atempo restores tempo.
// atempo per-stage clamped [0.5, 2.0] — chain when needed.
function chainAtempo(target) {
  // produce a chain of "atempo=x" stages whose product = target, each in [0.5,2.0].
  const stages = [];
  let r = target;
  while (r > 2.0) { stages.push(2.0); r /= 2.0; }
  while (r < 0.5) { stages.push(0.5); r /= 0.5; }
  stages.push(+r.toFixed(6));
  return stages.map((s) => `atempo=${s}`).join(",");
}

const PITCH_STEPS = [-12, -7, -5, -3, +3, +5, +7, +12];
const STRETCH_RATES = [0.5, 0.75, 1.5, 2.0]; // tempo only, pitch preserved
const CHIPMUNK_STEPS = [-12, -5, +5, +12];   // pitch + tempo together

function run(args) {
  const res = spawnSync("ffmpeg", ["-y", ...args], { stdio: ["ignore", "ignore", "ignore"] });
  return res.status === 0;
}

function pitchShift(srcAbs, outAbs, semitones) {
  const ratio = semi(semitones);
  const filter = `asetrate=48000*${ratio},aresample=48000,${chainAtempo(1 / ratio)},loudnorm=I=-14:TP=-1:LRA=11`;
  return run(["-i", srcAbs, "-af", filter, "-ac", "1", "-ar", "48000", outAbs]);
}

function stretch(srcAbs, outAbs, rate) {
  // rate < 1 = slower; rate > 1 = faster. pitch preserved.
  const filter = `${chainAtempo(rate)},loudnorm=I=-14:TP=-1:LRA=11`;
  return run(["-i", srcAbs, "-af", filter, "-ac", "1", "-ar", "48000", outAbs]);
}

function chipmunk(srcAbs, outAbs, semitones) {
  // pitch + tempo both move (classic "speed up the tape" sound).
  const ratio = semi(semitones);
  const filter = `asetrate=48000*${ratio},aresample=48000,loudnorm=I=-14:TP=-1:LRA=11`;
  return run(["-i", srcAbs, "-af", filter, "-ac", "1", "-ar", "48000", outAbs]);
}

function reverse(srcAbs, outAbs) {
  return run(["-i", srcAbs, "-af", "areverse,loudnorm=I=-14:TP=-1:LRA=11", "-ac", "1", "-ar", "48000", outAbs]);
}

function walk() {
  const out = [];
  if (!existsSync(uttRoot)) return out;
  for (const g of readdirSync(uttRoot)) {
    const gd = join(uttRoot, g);
    if (!statSync(gd).isDirectory()) continue;
    for (const f of readdirSync(gd)) {
      if (!f.endsWith(".wav")) continue;
      out.push({ group: g, file: join(gd, f), name: basename(f, ".wav") });
    }
  }
  return out;
}

function main() {
  const argv = process.argv.slice(2);
  const force = argv.includes("--force");
  const groupFilter = argv.find((a) => !a.startsWith("--"));
  const items = walk().filter((x) => !groupFilter || x.group === groupFilter);
  if (!items.length) { console.log("no utterances found in", uttRoot); return; }
  let made = 0, skipped = 0;
  for (const u of items) {
    const outDir = join(varRoot, u.group, u.name);
    mkdirSync(outDir, { recursive: true });
    const tasks = [
      ...PITCH_STEPS.map((s) => ({ kind: "pitch", arg: s, name: `pitch${s > 0 ? "+" : ""}${s}` })),
      ...STRETCH_RATES.map((r) => ({ kind: "stretch", arg: r, name: `stretch${r}` })),
      ...CHIPMUNK_STEPS.map((s) => ({ kind: "chipmunk", arg: s, name: `chipmunk${s > 0 ? "+" : ""}${s}` })),
      { kind: "reverse", arg: null, name: "reverse" },
    ];
    for (const t of tasks) {
      const outAbs = join(outDir, `${t.name}.wav`);
      if (!force && existsSync(outAbs)) { skipped++; continue; }
      let ok;
      if (t.kind === "pitch") ok = pitchShift(u.file, outAbs, t.arg);
      else if (t.kind === "stretch") ok = stretch(u.file, outAbs, t.arg);
      else if (t.kind === "chipmunk") ok = chipmunk(u.file, outAbs, t.arg);
      else ok = reverse(u.file, outAbs);
      if (ok) made++;
      else console.log(`  ✗ ${u.group}/${u.name}/${t.name}`);
    }
  }
  console.log(`# variations: ${made} made, ${skipped} skipped, ${items.length} utterances`);
}

main();
