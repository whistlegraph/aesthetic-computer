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
import { masterChain } from "../../lib/substrate.mjs";

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

// master through the pop SUBSTRATE (tape) — the mix is already sat-printed
// in the C engine (drive/bias/hiss); this is the medium's mixdown chain:
// EQ tilt + air, wow/flutter, two-stage tube-glue compression. Single
// source: pop/lib/substrate.mjs.
const SUBSTRATE = "tape";
const mastered = join(HERE, "out", "c-mastered.wav");
run(`master (substrate: ${SUBSTRATE})`, "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", rawWav,
  "-af", masterChain(SUBSTRATE),
  "-c:a", "pcm_s24le", mastered,
]);

const src = existsSync(mastered) ? mastered : rawWav;
// DistroKid-ready master: EBU R128 normalization to -11 LUFS / -1 dBTP, one
// even present level end to end. We render TWO deliverables from the same
// loudnorm pass — a 320 kbps CBR mp3, and a 16-bit/44.1 kHz WAV (the format
// DistroKid prefers for upload).
// Static gain + brickwall limiter, NOT loudnorm. Single-pass loudnorm is a
// dynamic normalizer — it audibly ducked and recovered right after the loud
// first-hook drop (the "volume bump" around 0:27). Instead: measure the
// integrated loudness once, apply the one static gain that lands -11 LUFS,
// and let a fast transparent limiter catch the true peaks at ~-1 dB. The
// mix's own dynamics survive; only the tallest kick transients kiss the wall.
console.log("# loudness (measure pass)");
const meas = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-nostats",
  "-i", src,
  "-af", "loudnorm=I=-9:TP=-1.0:LRA=11:print_format=json",
  "-f", "null", "-",
], { encoding: "utf8" });
const mjson = JSON.parse((meas.stderr.match(/\{[\s\S]*\}/) ?? ["{}"])[0]);
const gainDb = (-9 - parseFloat(mjson.input_i)).toFixed(2);
// limit at 4x oversampling so the ceiling holds for TRUE (inter-sample)
// peaks, not just sample peaks — flat-topped limiting at 1x overshot
// ~2.5 dB after reconstruction. -2 dB ceiling leaves margin for the final
// downsample + mp3 encode, landing true peak under -1.
// 0.4s head pad — the song starts on a vocal at t=0; without a breath of
// silence the first syllable reads as cut off on players that eat the head.
const LOUDNORM = `adelay=400|400,volume=${gainDb}dB,aresample=192000,` +
  `alimiter=attack=1:release=60:limit=0.794:level=false`;
console.log(`  measured I=${mjson.input_i} TP=${mjson.input_tp} → static gain ${gainDb} dB + 4x-oversampled limiter -2 dB`);
const META = [
  "-metadata", "title=americomputadora",
  "-metadata", "artist=jeffrey",
  "-metadata", "album=pixsies",
];
run("encode mp3 (320k CBR + loudnorm)", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", src,
  "-af", LOUDNORM, "-ar", "48000",
  "-c:a", "libmp3lame", "-b:a", "320k",
  ...META, outMp3,
]);
console.log(`✓ ${outMp3}`);

const outWav = join(ROOT, "out", "americomputadora-master.wav");
run("master wav (16-bit / 44.1k, DistroKid)", "ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error",
  "-i", src,
  "-af", LOUDNORM, "-ar", "44100", "-sample_fmt", "s16",
  ...META, outWav,
]);
console.log(`✓ ${outWav}`);
