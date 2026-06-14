#!/usr/bin/env node
// render.mjs — the minitek render flow: cc → ./<engine> (renders a float32
// stereo WAV) → ffmpeg club-master (punchy: sub weight, tight glue pump,
// soft-clip density, crisp-but-not-harsh top, loud -9 LUFS) → mp3.
//
// usage: node c/render.mjs [--engine <name>] [--out ../out/<name>.mp3] [--wav x.wav]

import { spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE); // pop/minitek

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) { const k = a.slice(2), n = argv[i + 1]; if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true; }
}
const expand = (p) => (p && p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);
const ENGINE = (typeof flags.engine === "string" && flags.engine) || "minitek";
const outMp3 = resolve(expand(flags.out) ?? join(ROOT, "out", `${ENGINE}.mp3`));
mkdirSync(dirname(outMp3), { recursive: true });
mkdirSync(join(HERE, "out"), { recursive: true });

function run(label, cmd, args, opts = {}) {
  console.log(`# ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}

run("cc", "cc", ["-O3", "-std=c11", "-Wall", "-Wextra", "-Wno-unused-parameter", "-o", join(HERE, ENGINE), join(HERE, `${ENGINE}.c`), "-lm"]);

const rawWav = join(HERE, "out", `${ENGINE}-raw.wav`);
run("render (C engine)", join(HERE, ENGINE), ["--out", rawWav]);

// ── club master chain ─────────────────────────────────────────────────────
// punchy and loud, but the kick stays a kick: tight sub weight, a touch of
// de-mud, a small presence lift, crisp (not harsh) top, a glue comp with a
// fast-ish attack to pump, soft-clip for density, then loudnorm + limiter.
const MASTER_CORE = [
  "highpass=f=28",
  "equalizer=f=50:t=q:w=0.9:g=2.2",        // sub weight under the kick
  "equalizer=f=250:t=q:w=1.1:g=-1.4",      // de-mud
  "equalizer=f=2800:t=q:w=1.4:g=0.8",      // presence on the blip
  "equalizer=f=10000:t=q:w=0.8:g=1.2",     // crisp hats
  "lowpass=f=17500",
  "acompressor=threshold=-16dB:ratio=2.6:attack=12:release=180:makeup=3.0:knee=6", // glue + pump
  "asoftclip=type=tanh:threshold=0.96",
  "stereotools=slev=1.0",
].join(",");
const MASTER_MP3 = `${MASTER_CORE},loudnorm=I=-9:TP=-1.0:LRA=9,alimiter=limit=0.97:attack=3:release=60`;

run("master (ffmpeg) → mp3", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", rawWav, "-af", MASTER_MP3,
  "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", `title=${ENGINE}`, "-metadata", "artist=jeffrey", "-metadata", "album=pixsies",
  outMp3,
]);
console.log(`✓ ${outMp3} (club-mastered · 320k · stereo)`);

const wavOut = expand(flags.wav);
if (wavOut) {
  mkdirSync(dirname(wavOut), { recursive: true });
  run("master → DistroKid wav", "ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", rawWav,
    "-af", `${MASTER_CORE},loudnorm=I=-10:TP=-1.0:LRA=9,alimiter=limit=0.98:attack=2:release=45`,
    "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut,
  ]);
  console.log(`✓ ${wavOut} (DistroKid master · 44.1k/16-bit)`);
}
