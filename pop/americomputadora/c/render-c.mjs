#!/usr/bin/env node
// render-c.mjs — the C-backend render flow (hellsine pattern):
//   bake (JS preps vocals + score.h) → cc → ./americomputadora (C renders
//   the song to f32 WAV) → acdsp (C mastering: 1176 + EQ) → ffmpeg mp3.
//
// usage: node c/render-c.mjs [--no-bake] [--out ../out/americomputadora-bach-whisperz.mp3]

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = dirname(HERE);
const POP = dirname(ROOT);

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const k = a.slice(2), n = argv[i + 1];
    if (n !== undefined && !n.startsWith("--")) { flags[k] = n; i++; } else flags[k] = true;
  }
}
const outMp3 = resolve(flags.out ?? join(ROOT, "out", "americomputadora-bach-whisperz.mp3"));
mkdirSync(dirname(outMp3), { recursive: true });
mkdirSync(join(HERE, "out"), { recursive: true });

function run(label, cmd, args, opts = {}) {
  console.log(`# ${label}`);
  const res = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (res.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); }
}

if (!flags["no-bake"]) run("bake (vocals + score.h)", "node", [join(HERE, "bake.mjs")]);
run("cc", "sh", [join(HERE, "build.sh")]);

const rawWav = join(HERE, "out", "c-raw.wav");
run("render (C engine)", join(HERE, "americomputadora"),
    ["--out", rawWav, "--vocals", HERE + "/"]);

// master through acdsp — the C 1176 + EQ chain, not ffmpeg loudnorm.
const mastered = join(HERE, "out", "c-mastered.wav");
const acdsp = join(POP, "dsp", "c", "acdsp");
if (existsSync(acdsp)) {
  run("master (acdsp 1176 + eq)", acdsp, [
    rawWav, mastered, "--chain",
    "1176:ratio=4:in=-8:out=+9:attack=4:release=4 eq:warmth=+1 eq:air=+1.5",
    "--bits", "24",
  ]);
} else {
  console.warn("! acdsp not built (pop/dsp/c) — skipping mastering stage");
}

const src = existsSync(mastered) ? mastered : rawWav;
run("encode mp3 (ffmpeg, io only)", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", src,
  "-c:a", "libmp3lame", "-q:a", "2",
  "-metadata", "title=americomputadora",
  "-metadata", "artist=jeffrey",
  "-metadata", "album=pixsies",
  outMp3,
]);
console.log(`✓ ${outMp3}`);
