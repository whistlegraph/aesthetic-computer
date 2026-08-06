#!/usr/bin/env node
// Build the adversarial Special Sign sine-stack test and its matched probes.
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../../..");
const OUT = resolve(REPO, "pop/nullabye/out/review/variants/squeakysines");
const SOURCE = resolve(HERE, "squeakysines.c");
const BIN = resolve(OUT, ".squeakysines");
const WAV = resolve(OUT, "squeakysines.wav");
const MP3 = resolve(OUT, "squeakysines.mp3");
const EXPLOIT_WAV = resolve(OUT, "squeakysines-residue-exploit.wav");
const EXPLOIT_MP3 = resolve(OUT, "squeakysines-residue-exploit.mp3");
const RECEIPT = resolve(OUT, "squeakysines.provenance.json");
const PROBES = ["raw", "clean", "blend", "hrtf", "residue", "full"];

function run(command, args) {
  const result = spawnSync(command, args, { cwd: REPO, stdio: "inherit" });
  if (result.status !== 0) throw new Error(`${command} exited ${result.status}`);
}

mkdirSync(OUT, { recursive: true });
run("clang", ["-O3", "-std=c11", SOURCE, "-lm", "-o", BIN]);
run(BIN, ["--out", WAV]);
run(BIN, ["--exploit", "--out", EXPLOIT_WAV]);
for (const stage of PROBES) run(BIN, ["--probe", stage, "--out", resolve(OUT, `probe-${stage}.wav`)]);
run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", WAV,
  "-af", "highpass=f=28,lowpass=f=18000,alimiter=limit=.95:attack=5:release=80",
  "-c:a", "libmp3lame", "-b:a", "320k", MP3]);
run("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", EXPLOIT_WAV,
  "-af", "highpass=f=28,lowpass=f=18000,alimiter=limit=.95:attack=5:release=80",
  "-c:a", "libmp3lame", "-b:a", "320k", EXPLOIT_MP3]);

const sha256 = (path) => createHash("sha256").update(readFileSync(path)).digest("hex");
writeFileSync(RECEIPT, JSON.stringify({
  title: "squeakysines",
  status: "diagnostic composition — never a Special Sign release replacement",
  source: "pop/nullabye/variants/squeakysines/squeakysines.c",
  productionHrtf: "pop/nullabye/c/ac_hrtf.h",
  hypothesis: "moving elevation-dependent pinna comb / fractional binaural delay produces the audible squeak",
  durationSeconds: { stackSong: 48, residueExploit: 72 },
  sampleRate: 48000,
  sections: [
    { at: 0, stage: "raw carrier" },
    { at: 8, stage: "clean listener-relative pan and distance" },
    { at: 16, stage: "Special Sign blend: 76% clean + 24% procedural HRTF" },
    { at: 24, stage: "isolated 24% HRTF-minus-clean residue, with makeup gain" },
    { at: 32, stage: "release blend + moving propagation delay + immutable FIR room" },
    { at: 40, stage: "two-register adversarial pinna-residue finale" }
  ],
  probes: PROBES.map((stage) => `probe-${stage}.wav`),
  residueExploit: {
    description: "nine HRTF-minus-clean passes; motion rises every eight seconds; final two passes add octave layers",
    wav: "squeakysines-residue-exploit.wav",
    mp3: "squeakysines-residue-exploit.mp3"
  },
  output: { wav: "squeakysines.wav", mp3: "squeakysines.mp3" },
  sha256: { wav: sha256(WAV), mp3: sha256(MP3), exploitWav: sha256(EXPLOIT_WAV), exploitMp3: sha256(EXPLOIT_MP3) }
}, null, 2) + "\n");

console.log(`✓ ${MP3}`);
console.log(`✓ ${EXPLOIT_MP3}`);
console.log(`✓ ${RECEIPT}`);
