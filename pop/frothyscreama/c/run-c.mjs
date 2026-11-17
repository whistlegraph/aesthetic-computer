#!/usr/bin/env node
// run-c.mjs — render a baked frothyscreama score through the C engine and
// master it. This is the track's renderer: bin/render-frothyscreama.mjs is
// the composition, writes the score, and hands it here.
//
//   node pop/frothyscreama/c/run-c.mjs                       bake, render, master → out/frothyscreama.mp3
//   node pop/frothyscreama/c/run-c.mjs <score.txt>           render a score already baked
//   node pop/frothyscreama/c/run-c.mjs <score.txt> --raw <out.f32>   premaster only, no master
//   ... --out <mp3>                                          master somewhere else

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, unlinkSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const ENGINE = resolve(HERE, "frothyscreama");
const FFMPEG = existsSync("/opt/homebrew/bin/ffmpeg") ? "/opt/homebrew/bin/ffmpeg" : "ffmpeg";
const TARGET_LUFS = -10.5;

const args = process.argv.slice(2);
const val = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const flagged = new Set(["--raw", "--out"].map(val).filter(Boolean));
let score = args.find((a) => !a.startsWith("--") && !flagged.has(a));
const rawOnly = val("--raw"), outMp3 = resolve(val("--out") ?? resolve(LANE, "out/frothyscreama.mp3"));
const sh = (label, cmd, a) => { const r = spawnSync(cmd, a, { stdio: "inherit" }); if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); } };

// no score named: bake one from the composition as it stands
if (!score) {
  score = resolve(LANE, "out/frothyscreama.score.txt");
  sh("bake", "node", [resolve(LANE, "bin/render-frothyscreama.mjs"), "--bake", score]);
}

// build the engine if missing or older than anything it is made of
const sources = [resolve(HERE, "frothyscreama.c"), resolve(HERE, "../../lib/c/ac_bake.h"), resolve(HERE, "../../lib/c/ac_material.h")];
if (!existsSync(ENGINE) || sources.some((s) => statSync(s).mtimeMs > statSync(ENGINE).mtimeMs)) {
  console.log("[run-c] building frothyscreama…");
  sh("build", "bash", [resolve(HERE, "build.sh")]);
}

const raw = resolve(rawOnly ?? `${outMp3}.f32.raw`), voxRaw = `${raw}.vox`;
mkdirSync(dirname(raw), { recursive: true });
sh("frothyscreama", ENGINE, [resolve(score), "--raw", raw, ...(rawOnly ? [] : ["--vox", voxRaw])]);
if (rawOnly) process.exit(0);

// Master: glue, then measured static gain into a limiter (pop/MASTERING.md) — no loudness rider.
const head = readFileSync(resolve(score), "utf8").slice(0, 400);
const sr = Number(head.match(/^sr (\d+)/m)[1]), dur = Number(head.match(/^frames (\d+)/m)[1]) / sr;
const input = ["-f", "f32le", "-ar", sr, "-ac", 2, "-i", raw].map(String);
const glue = "highpass=f=32,acompressor=threshold=-16dB:ratio=2.2:attack=12:release=140:makeup=1.5:knee=6";
const probe = spawnSync(FFMPEG, ["-hide_banner", "-nostats", ...input, "-af", `${glue},ebur128`, "-f", "null", "-"], { encoding: "utf8" });
const lufs = Number(probe.stderr.match(/I:\s+(-?[\d.]+) LUFS(?![\s\S]*I:\s)/)?.[1]);
const gain = Number.isFinite(lufs) ? Math.min(14, TARGET_LUFS - lufs + 0.8) : 0;
const chain = `${glue},volume=${gain.toFixed(2)}dB,alimiter=limit=0.84:attack=3:release=60:level=disabled,afade=t=out:st=${(dur - 3).toFixed(2)}:d=3`;
const wav = outMp3.replace(/\.mp3$/, "-master.wav");
const ff = (a) => sh("ffmpeg", FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...a]);
// the premaster and the vocal bus, kept beside the master as float wavs
ff([...input, "-c:a", "pcm_f32le", outMp3.replace(/\.mp3$/, "-premaster.wav")]);
ff(["-f", "f32le", "-ar", String(sr), "-ac", "2", "-i", voxRaw, "-c:a", "pcm_f32le", outMp3.replace(/\.mp3$/, "-vox.wav")]);
ff([...input, "-af", chain, "-ar", String(sr), "-c:a", "pcm_s24le", wav]);
ff(["-i", wav, "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=frothyscreama", "-metadata", "artist=Whistlegraph Dot Org", "-metadata", "album=pixsies", outMp3]);
for (const f of [raw, voxRaw]) try { unlinkSync(f); } catch {}
console.log(`glue ${lufs} LUFS → ${gain.toFixed(1)} dB into the limiter\n✓ ${outMp3} (C engine)`);
