#!/usr/bin/env node
// render.mjs — cc → ./bracelet → ffmpeg master → mp3.
//
// The master chain is deliberately NOT the minitek club chain. This is binaural
// material: the left/right differences ARE the content, so anything that touches
// the stereo relationship destroys the thing the track is about.
//
//   no stereotools / no width           — widening scrambles ILD
//   no mid-side EQ                      — same reason
//   channel-linked compression only     — per-channel gain would move sources
//   moderate loudness (-14 not -9)      — headroom keeps transient cues intact
//
// usage: node c/render.mjs [--out ../out/bracelet.mp3] [--bpm 138] [--play]

import { spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
const expand = (p) => (p && p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);
const outMp3 = resolve(expand(flags.out) ?? join(ROOT, "out", "bracelet.mp3"));
mkdirSync(dirname(outMp3), { recursive: true });
mkdirSync(join(ROOT, "out"), { recursive: true });

function run(label, cmd, args, opts = {}) {
  console.log(`# ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}

run("cc", "cc", ["-O3", "-std=c11", "-Wall", "-Wextra", "-DBELL_NO_MAIN", "-o", join(HERE, "bracelet"), join(HERE, "bracelet.c"), join(HERE, "..", "..", "bell", "c", "bell.c"), "-lm"]);

const rawWav = join(ROOT, "out", "bracelet-raw.wav");
const engineArgs = ["--out", rawWav];
if (flags.bpm) engineArgs.push("--bpm", String(flags.bpm));
run("render (C engine)", join(HERE, "bracelet"), engineArgs);

// Gentle, stereo-preserving master.
const MASTER = [
  "highpass=f=25",
  "equalizer=f=55:t=q:w=1.0:g=1.6",         // weight under the centre kick
  "equalizer=f=320:t=q:w=1.2:g=-1.2",       // de-mud so the ring stays clear
  "equalizer=f=6500:t=q:w=1.2:g=1.0",       // pinna-cue band: helps localisation
  "lowpass=f=18000",
  "acompressor=threshold=-18dB:ratio=2.0:attack=25:release=250:makeup=2.0:knee=6:link=maximum",
  "loudnorm=I=-14:TP=-1.5:LRA=11",
  "alimiter=limit=0.95:attack=5:release=80",
].join(",");

run("master (ffmpeg) → mp3", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", rawWav, "-af", MASTER,
  "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", "title=bracelet",
  "-metadata", "artist=Aesthetic Dot Computer",
  "-metadata", "album=pixsies",
  "-metadata", "comment=binaural — headphones required",
  outMp3,
]);
console.log(`✓ ${outMp3} (binaural master · 320k · HEADPHONES)`);

if (flags.play) spawnSync("open", ["-a", "QuickTime Player", outMp3], { stdio: "inherit" });
