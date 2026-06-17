#!/usr/bin/env node
// render-c.mjs — the C-backend render flow (hellsine / americomputadora
// pattern): bake (JS preps score.h) → cc → ./maytrax (C renders the song to
// a float32 stereo WAV) → ffmpeg pop-master (the same MASTER chain as
// bin/maytrax.mjs: tonal EQ + glue comp + soft-clip + width, then loudnorm +
// alimiter) → mp3.
//
// usage: node c/render-c.mjs [--no-bake] [--out ../out/maytrax.mp3] [--wav x.wav]

import { spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE); // pop/maytrax

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) { const k = a.slice(2), n = argv[i + 1]; if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true; }
}
const expand = (p) => (p && p.startsWith("~/") ? resolve(homedir(), p.slice(2)) : p);
// --sine renders the sinetrax fork (pure sine-bell, no samples/drums/vocals).
// --engine <name> renders any sine-family fork (dewtrax, mosstrax, …): a
// self-contained <name>.c that needs no bake and masters on the sine chain.
const SINE = !!flags.sine;
const ENGINE = (typeof flags.engine === "string" && flags.engine) || (SINE ? "sinetrax" : "maytrax");
// everything except the sample/drum/vocal maytrax engine is sine-family.
const SINEFAM = SINE || ENGINE !== "maytrax";
const outMp3 = resolve(expand(flags.out) ?? join(ROOT, "out", `${ENGINE}.mp3`));
mkdirSync(dirname(outMp3), { recursive: true });
mkdirSync(join(HERE, "out"), { recursive: true });

function run(label, cmd, args, opts = {}) {
  console.log(`# ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}

if (!SINEFAM && !flags["no-bake"]) run("bake (score.h)", "node", [join(HERE, "bake.mjs")]);
run("cc", "cc", ["-O3", "-std=c11", "-Wall", "-Wextra", "-Wno-unused-parameter", "-o", join(HERE, ENGINE), join(HERE, `${ENGINE}.c`), "-lm"]);

const rawWav = join(HERE, "out", `${ENGINE}-raw.wav`);
run("render (C engine)", join(HERE, ENGINE), ["--out", rawWav]);

// ── pop master chain (ported from bin/maytrax.mjs) ────────────────────────
// chiller, warmer, more dynamic master than the old hot pop chain: lower
// loudness target (room to breathe), softer presence, gentle air, a glue
// comp with a slow attack so transients punch through, and a soft limiter.
const MASTER_CORE = [
  "highpass=f=30",
  "equalizer=f=55:t=q:w=0.8:g=1.4",        // tight sub weight
  "equalizer=f=300:t=q:w=1.1:g=-1.0",      // gentle de-mud (warm)
  "equalizer=f=3200:t=q:w=1.6:g=0.6",      // barely-there presence
  "equalizer=f=8500:t=q:w=0.8:g=-1.6",     // tame harsh upper-treble (smooth)
  "treble=g=-0.6:f=11000",                 // gentle high-shelf DOWN — de-harsh
  "lowpass=f=16500",                       // shave the brittle top
  "acompressor=threshold=-20dB:ratio=2.0:attack=30:release=260:makeup=2.2:knee=8", // smooth glue; slow attack keeps the groove punching
  "asoftclip=type=tanh:threshold=0.98",    // minimal density
  "stereotools=slev=1.04",                 // tighter width
].join(",");
// quiet, slow entrance — a 13s fade-in AFTER the limiter so loudnorm can't
// pull the intro back up. cub (cubic) stays very quiet early then rises into
// the build, so the opening sounds stay soft far longer than a linear ramp.
const INTRO_FADE = "afade=t=in:curve=cub:st=0:d=13";
// sinetrax is gentle bells (and fades itself) — master it softer + more
// dynamic with no extra fade; maytrax gets the slow cubic intro fade.
const MASTER_MP3 = SINEFAM
  ? `${MASTER_CORE},loudnorm=I=-13:TP=-1.5:LRA=13,alimiter=limit=0.95:attack=8:release=120`
  : `${MASTER_CORE},loudnorm=I=-11:TP=-1.5:LRA=11,alimiter=limit=0.93:attack=5:release=90,${INTRO_FADE}`;

run("master (ffmpeg) → mp3", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", rawWav, "-af", MASTER_MP3,
  "-c:a", "libmp3lame", "-b:a", "320k",
  "-metadata", `title=${ENGINE}`, "-metadata", "artist=jeffrey", "-metadata", "album=pixsies",
  outMp3,
]);
console.log(`✓ ${outMp3} (pop-mastered · 320k · stereo)`);

const wavOut = expand(flags.wav);
if (wavOut) {
  mkdirSync(dirname(wavOut), { recursive: true });
  run("master → DistroKid wav", "ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-i", rawWav,
    "-af", `${MASTER_CORE},loudnorm=I=-14:TP=-1.5:LRA=11,alimiter=limit=0.95:attack=3:release=55`,
    "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", wavOut,
  ]);
  console.log(`✓ ${wavOut} (DistroKid master · 44.1k/16-bit · -14 LUFS)`);
}
