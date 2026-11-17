#!/usr/bin/env node
// run-cutie.mjs — render a baked wattajetta cutie club score through the C
// engine and master it. The composition lives in
// bin/render-wattajetta-cutie.mjs; this is the engine + finalize path, with
// the renderer's own master chain.
//
//   node pop/wattajetta/c/run-cutie.mjs                      bake, render, master → out/wattajetta-cutie-c.mp3
//   node pop/wattajetta/c/run-cutie.mjs <score.txt>          render a score already baked
//   node pop/wattajetta/c/run-cutie.mjs <score.txt> --raw <out.f32>   premaster only, no master
//   ... --out <mp3>                                          master somewhere else

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const ENGINE = resolve(HERE, "wattajetta-cutie");
const FFMPEG = existsSync("/opt/homebrew/bin/ffmpeg") ? "/opt/homebrew/bin/ffmpeg" : "ffmpeg";

const args = process.argv.slice(2);
const val = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const flagged = new Set(["--raw", "--out"].map(val).filter(Boolean));
let score = args.find((a) => !a.startsWith("--") && !flagged.has(a));
const rawOnly = val("--raw"), outMp3 = resolve(val("--out") ?? resolve(LANE, "out/wattajetta-cutie-c.mp3"));
const sh = (label, cmd, a) => { const r = spawnSync(cmd, a, { stdio: "inherit" }); if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); } };

// no score named: bake one from the composition as it stands
if (!score) {
  score = resolve(LANE, "out/wattajetta-cutie.score.txt");
  sh("bake", "node", [resolve(LANE, "bin/render-wattajetta-cutie.mjs"), "--bake", score]);
}

// build the engine if missing or older than anything it is made of
const sources = [resolve(HERE, "wattajetta-cutie.c"), resolve(HERE, "../../lib/c/ac_bake.h"), resolve(HERE, "../../lib/c/ac_material.h")];
if (!existsSync(ENGINE) || sources.some((s) => statSync(s).mtimeMs > statSync(ENGINE).mtimeMs)) {
  console.log("[run-cutie] building wattajetta-cutie…");
  sh("build", "bash", [resolve(HERE, "build-cutie.sh")]);
}

const raw = resolve(rawOnly ?? `${outMp3}.f32.raw`);
mkdirSync(dirname(raw), { recursive: true });
sh("wattajetta-cutie", ENGINE, [resolve(score), "--raw", raw]);
if (rawOnly) process.exit(0);

// Master: gentle glue, a little weight at 54 Hz, a limiter. No treble lift —
// it reads as tang on laptop speakers.
const head = readFileSync(resolve(score), "utf8").slice(0, 400);
const sr = Number(head.match(/^sr (\d+)/m)[1]), dur = Number(head.match(/^frames (\d+)/m)[1]) / sr;
const chain = [
  "highpass=f=26",
  "acompressor=threshold=-18dB:ratio=1.8:attack=18:release=180:makeup=1.6:knee=8",
  "equalizer=f=54:t=q:w=0.8:g=1.6",
  "alimiter=limit=0.89:attack=4:release=80:level=disabled",
  `afade=t=out:st=${(dur - 3).toFixed(2)}:d=3`,
].join(",");
const wav = outMp3.replace(/\.mp3$/, "-master.wav"), cover = resolve(LANE, "canvas/cover-3000.jpg");
const ff = (a) => sh("ffmpeg", FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...a]);
ff(["-f", "f32le", "-ar", String(sr), "-ac", "2", "-i", raw, "-af", chain, "-ar", String(sr), "-c:a", "pcm_s24le", wav]);
ff(["-i", wav, ...(existsSync(cover) ? ["-i", cover, "-map", "0:a", "-map", "1:v", "-c:v", "mjpeg", "-vf", "scale=1200:1200", "-disposition:v", "attached_pic", "-id3v2_version", "3"] : []),
  "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=wattajetta cutie club",
  "-metadata", "artist=Whistlegraph Dot Org", "-metadata", "album=pixsies", outMp3]);
try { unlinkSync(raw); } catch {}
console.log(`✓ ${outMp3} (C engine)`);
