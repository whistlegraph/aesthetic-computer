#!/usr/bin/env node
// bake-solafiya.mjs — mastering bake for the DistroKid drop, now via
// pop/lib/master.mjs (acdsp 1176 + EQ knowledge graph) instead of an
// all-ffmpeg shelf chain. Matches the hellsine / trancenwaltzi style of
// "let the 1176's GR-modulated FET saturation do the brightening work
// and shape the top with named EQ intents from eq-graph.json".
//
// Source: pop/jungle/out/solafiya.mp3 — the full instrumental+vocal mix
// from render.mjs (fía's voice forced-aligned into vocal/vocalAd/
// vocalDuet/vocalH/throat lanes via the WORLD pipeline). The render-side
// vocal chain was retuned for the "less airy, sits IN the space" mix
// (see render.mjs: 2400 Hz presence not 2700, 220 Hz chest body, deeper
// duck, tighter harmonies, more aggressive 8 kHz fizz cut). Master here
// restores a measured amount of air after that source-side fizz cut.
//
// Acdsp character chain (run on the decoded WAV):
//   eq:sub                                — strip subsonic
//   eq:warmth +1                          — body without mud
//   1176 (ratio 4, in -3, out +3, iron 0.5)  — bus glue + GR-modulated FET upper-harmonic brightness
//   eq:mud -1.5                           — 250 Hz cloud trim
//   eq:boxy -1                            — 500 Hz cardboard-tube honk
//   eq:presence +1.5                      — 4 kHz forward-in-the-room
//   eq:air +1.2                           — silk at 11 k (NOT 8 k — fizz lives at 8 k)
//
// Then ffmpeg finalizes: loudnorm I=-14 / TP=-1.5 / LRA=8 → alimiter 0.95.
//
// Outputs (~/Documents/Shelf/solafiya-DISTROKID/):
//   solafiya-MASTER.wav             — 16-bit / 44.1 kHz stereo
//   solafiya-MASTER-preBright.wav   — pre-acdsp reference for A/B
//   solafiya.mp3                    — 320 k + 3000² cover + ID3
//   solafiya-cover-3000.jpg         — DistroKid cover
//
// Cached: if final WAV + cover + mp3 exist and BAKE_FORCE is unset,
// exits 0. Re-bake: BAKE_FORCE=1 node pop/jungle/bin/bake-solafiya.mjs

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, statSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { homedir } from "node:os";
import { fileURLToPath } from "node:url";
import { acdspAvailable, processWav, chain, eq, compressor } from "../../lib/master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");

const SRC_MP3  = `${REPO}/pop/jungle/out/solafiya.mp3`;
const SRC_ILLY = `${REPO}/pop/jungle/out/solafiya.illy.png`;

const OUT   = `${homedir()}/Documents/Shelf/solafiya-DISTROKID`;
const PRE   = `${OUT}/solafiya-MASTER-preBright.wav`;
const AC    = `${OUT}/.solafiya-acdsp.wav`;
const FINAL = `${OUT}/solafiya-MASTER.wav`;
const COVER = `${OUT}/solafiya-cover-3000.jpg`;
const MP3   = `${OUT}/solafiya.mp3`;

mkdirSync(OUT, { recursive: true });

if (!process.env.BAKE_FORCE && existsSync(FINAL) && existsSync(COVER) && existsSync(MP3)) {
  console.log(`[bake-solafiya] cached → ${FINAL} (BAKE_FORCE=1 to re-bake)`);
  process.exit(0);
}
if (!existsSync(SRC_MP3)) {
  console.error(`✗ source missing: ${SRC_MP3}`);
  console.error(`  run: node pop/jungle/bin/render.mjs --slug solafiya --cover`);
  process.exit(1);
}
if (!existsSync(SRC_ILLY)) { console.error(`✗ illustration missing: ${SRC_ILLY}`); process.exit(1); }
if (!acdspAvailable()) {
  console.error(`✗ pop/dsp/c/acdsp not built — (cd pop/dsp/c && make)`);
  process.exit(1);
}

const run = (label, cmd, args, opts = {}) => {
  console.log(`\n[bake-solafiya] ${label}`);
  const r = spawnSync(cmd, args, { stdio: "inherit", ...opts });
  if (r.status !== 0) { console.error(`[bake-solafiya] FAILED: ${label}`); process.exit(1); }
};

// 1 — decode source mp3 → pre-master reference WAV (no EQ, no comp)
run("decode source → pre-bright reference",
  "ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", SRC_MP3,
    "-ac", "2", "-ar", "44100", "-c:a", "pcm_s16le", PRE]);

// 2 — acdsp character pass (1176 + EQ via C lib) on the pre-bright WAV
// Master chain — pulled the low-end out (was "too much low tones
// everywhere" in the bed). Drop the warmth shelf entirely, cut deeper
// at 250 Hz mud, keep presence + a touch of air on top.
const spec = chain(
  eq("rumble"),                  // 60 Hz HP — kill the sub-mud (was eq:sub at 30 Hz)
  compressor("1176", { ratio: 4, in: -3, out: +3, attack: 5, release: 3, iron: 0.5 }),
  eq("mud", -2.5),               // deeper 250 Hz cut (was -1.5)
  eq("boxy", -1),
  eq("presence", +1.5),
  eq("air", +1.0),               // gentle (was +1.2)
);
console.log(`\n[bake-solafiya] acdsp character pass (1176 + EQ via C lib)\n        chain: ${spec}`);
const r = processWav(PRE, AC, spec, { float: true });
if (!r.ok) { console.error(`[bake-solafiya] acdsp failed:\n${r.stderr}`); process.exit(1); }
process.stderr.write(r.stderr);

// 3 — ffmpeg finalize: loudnorm + true-peak limit → 44.1 / 16-bit stereo
run("finalize → -14 LUFS / -1.5 dBTP / LRA 8",
  "ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", AC,
    "-af", "loudnorm=I=-14:TP=-1.5:LRA=8,alimiter=limit=0.95",
    "-ac", "2", "-ar", "44100", "-c:a", "pcm_s16le", FINAL]);

// 4 — cover: lanczos 1024² illy → 3000² JPG, sRGB, q=92
run("cover (lanczos 1024 → 3000²)",
  "magick", [SRC_ILLY, "-filter", "Lanczos", "-resize", "3000x3000",
    "-colorspace", "sRGB", "-quality", "92", COVER]);

// 5 — 320 k mp3 + embedded cover + ID3 (matches other pixsies singles)
run("mp3 (320 k + 3000² cover + ID3)",
  "ffmpeg", ["-y", "-hide_banner", "-loglevel", "error",
    "-i", FINAL, "-i", COVER,
    "-map", "0:a", "-map", "1:v", "-c:a", "libmp3lame", "-b:a", "320k",
    "-c:v", "copy", "-id3v2_version", "3",
    "-metadata", "title=solafiya",
    "-metadata", "artist=fía",
    "-metadata", "album=pixsies",
    "-metadata", "date=2026",
    "-metadata", "genre=Jungle",
    "-metadata", "publisher=Aesthetic.Computer",
    "-metadata", "copyright=Aesthetic.Computer",
    "-metadata:s:v", "title=Album cover",
    "-metadata:s:v", "comment=Cover (front)",
    MP3]);

// 6 — measure final master (ebur128 summary)
console.log(`\n[bake-solafiya] ── loudness ──────────────────────────────`);
const lm = spawnSync("ffmpeg",
  ["-hide_banner", "-nostats", "-i", FINAL, "-af", "ebur128=peak=true", "-f", "null", "-"],
  { encoding: "utf8" });
const out = (lm.stderr || "");
for (const line of out.split("\n")) {
  if (/Integrated loudness:|^  I:|^  LRA:|^  Peak:|^  Threshold:/.test(line)) console.log(line);
}

const dur = spawnSync("ffprobe",
  ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", FINAL],
  { encoding: "utf8" }).stdout.trim();

// cleanup intermediate
try { if (existsSync(AC)) statSync(AC); } catch {}
spawnSync("rm", ["-f", AC]);

console.log(`\n[bake-solafiya] done`);
console.log(`  master : ${FINAL}  (${dur}s)`);
console.log(`  pre    : ${PRE}`);
console.log(`  cover  : ${COVER}`);
console.log(`  mp3    : ${MP3}`);
