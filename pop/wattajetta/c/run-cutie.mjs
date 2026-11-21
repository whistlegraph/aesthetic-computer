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
//   ... --accel <bpm>                                        the tempo climbs to this by the finale (pitch kept)
//   ... --long | --waltz | --rock                            passed to the bake (the full form; the waltz lilt; the rock kit)

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, statSync, unlinkSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { masterGraph, COVER } from "../bin/cutie-master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const ENGINE = resolve(HERE, "wattajetta-cutie");
const FFMPEG = "ffmpeg";

const args = process.argv.slice(2);
const val = (k) => { const i = args.indexOf(k); return i >= 0 ? args[i + 1] : null; };
const flagged = new Set(["--raw", "--out", "--accel"].map(val).filter(Boolean));
let score = args.find((a) => !a.startsWith("--") && !flagged.has(a));
const rawOnly = val("--raw"), outMp3 = resolve(val("--out") ?? resolve(LANE, "out/wattajetta-cutie-c.mp3"));
const sh = (label, cmd, a) => { const r = spawnSync(cmd, a, { stdio: "inherit" }); if (r.status !== 0) { console.error(`✗ ${label} failed`); process.exit(1); } };

// no score named: bake one from the composition as it stands
if (!score) {
  score = resolve(LANE, "out/wattajetta-cutie.score.txt");
  sh("bake", "node", [resolve(LANE, "bin/render-wattajetta-cutie.mjs"), "--bake", score, ...["--long", "--waltz", "--rock", "--no-scream", "--no-stone", "--dense", "--rap"].filter((f) => args.includes(f))]);
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

// Master: the same pop chain the renderer uses (bin/cutie-master.mjs).
const head = readFileSync(resolve(score), "utf8").slice(0, 400);
const sr = Number(head.match(/^sr (\d+)/m)[1]);
let dur = Number(head.match(/^frames (\d+)/m)[1]) / sr;
const widthFile = resolve(dirname(resolve(score)), "wattajetta-cutie.width.json");
let width = existsSync(widthFile) ? JSON.parse(readFileSync(widthFile, "utf8")) : null;
const ff = (a) => sh("ffmpeg", FFMPEG, ["-y", "-hide_banner", "-loglevel", "error", ...a]);

// --accel: the score is a fixed grid; the finished premaster is warped through
// rubberband-r3 on a time map (pitch kept), as pop/cult/bin/tempo.py does. The
// tempo holds for the first eight bars, then climbs on a smoothstep and is at
// its target sixteen bars before the end. The width curve moves with it.
let source = ["-f", "f32le", "-ar", String(sr), "-ac", "2", "-i", raw];
const accel = Number(val("--accel") ?? 0), warped = `${outMp3}.accel.wav`, dry = `${outMp3}.dry.wav`;
if (accel) {
  const bpm0 = Number(readFileSync(resolve(score), "utf8").slice(0, 4000).match(/^cfg bpm ([\d.]+)/m)[1]);
  const BAR = 240 / bpm0, bars = Math.ceil(dur / BAR) + 1;
  const smooth = (u) => { u = Math.max(0, Math.min(1, u)); return u * u * (3 - 2 * u); };
  const bpm = (b) => bpm0 + (accel - bpm0) * smooth((b - 8) / Math.max(1, bars - 8 - 16));
  const edges = [0];
  for (let b = 0; b < bars; b++) edges.push(edges[b] + BAR * bpm0 / bpm(b));
  const warp = (t) => { const b = Math.min(bars - 1, Math.floor(t / BAR)); return edges[b] + (t - b * BAR) * bpm0 / bpm(b); };
  const map = `${outMp3}.accel.map`;
  writeFileSync(map, edges.map((e, b) => `${Math.round(b * BAR * sr)} ${Math.round(e * sr)}`).join("\n") + "\n");
  ff([...source, "-c:a", "pcm_f32le", dry]);
  const D = warp(dur);
  sh("rubberband", "rubberband-r3", ["-q", "--timemap", map, "-D", D.toFixed(4), dry, warped]);
  for (const f of [dry, map]) try { unlinkSync(f); } catch {}
  console.log(`[run-cutie] tempo ${bpm0} → ${accel} BPM · ${dur.toFixed(1)} s → ${D.toFixed(1)} s`);
  if (width) width = width.map(([t, w]) => [warp(t), w]);
  dur = D; source = ["-i", warped];
}
const wav = outMp3.replace(/\.mp3$/, "-master.wav"), cover = COVER;
ff([...source, "-filter_complex", masterGraph(sr, [dur - 3, 3], undefined, width), "-map", "[out]", "-ar", String(sr), "-c:a", "pcm_s24le", wav]);
ff(["-i", wav, ...(existsSync(cover) ? ["-i", cover, "-map", "0:a", "-map", "1:v", "-c:v", "mjpeg", "-vf", "scale=1200:1200", "-disposition:v", "attached_pic", "-id3v2_version", "3"] : []),
  "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", "title=wada jetter QD UR",
  "-metadata", "artist=Aesthetic Dot Computer", "-metadata", "album=pixsies", outMp3]);
for (const f of [raw, warped]) try { unlinkSync(f); } catch {}
console.log(`✓ ${outMp3} (C engine)`);
