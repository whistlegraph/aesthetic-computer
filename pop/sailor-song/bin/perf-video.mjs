#!/usr/bin/env node
// perf-video.mjs — her performance video, retimed onto the record's clock, with the remix under it.
//
// The record plays her stems through regularize.mjs's time map (src/vox/reg/timemap.txt:
// take samples → regularized samples, piecewise linear between her beats) and opens at the
// receipt's startSec. So every output frame k at FPS asks: record time k/FPS → reg time
// (+ startSec) → take time (the map, inverted) → the nearest source frame. Frames are pulled
// from a one-time extraction of the take (ffmpeg, FPS, scaled) and sequenced with the concat
// demuxer, the way marketing/talking-head/bin/warp-to-sing.mjs rode the YC talking head
// onto a sung take. Nothing is interpolated: a held bar holds frames, a rushed bar skips.
//
//   node pop/sailor-song/bin/perf-video.mjs [--audio out/sailor-song-v22.mp3] [--fps 30] [--height 1080] [--up 1080] [--cover]
//     → out/sailor-song-v22-perf.mp4
import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, resolve, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url)), LANE = resolve(HERE, "..");
const OUT = resolve(LANE, "out"), SR = 48000;
const arg = (k, d = null) => { const i = process.argv.indexOf(`--${k}`); return i >= 0 && process.argv[i + 1] && !process.argv[i + 1].startsWith("--") ? process.argv[i + 1] : (process.argv.includes(`--${k}`) ? true : d); };
const newestCut = () => readdirSync(OUT).filter((f) => /^sailor-song-v\d+[a-z]?\.mp3$/.test(f)).map((f) => resolve(OUT, f))
  .sort((a, b) => execFileSync("stat", ["-f%m", b]) - execFileSync("stat", ["-f%m", a]))[0];
const AUDIO = resolve(arg("audio") || newestCut());
const stem = basename(AUDIO).replace(/\.(mp3|wav|flac)$/, "");
const FPS = Number(arg("fps", 30)), HEIGHT = Number(arg("height", 1080)), UP = Number(arg("up", 0));   // --up 1080: cache at --height, deliver scaled up (lanczos + a touch of unsharp)
const SRC = resolve(LANE, "src/take.mov");
const receiptPath = resolve(OUT, `${stem}.events.json`);
if (!existsSync(SRC)) { console.error(`✗ ${SRC} missing — her original (IMG_8699.mov) goes there`); process.exit(1); }
if (!existsSync(receiptPath)) { console.error(`✗ ${receiptPath} missing — bake first`); process.exit(1); }
const R = JSON.parse(readFileSync(receiptPath, "utf8"));
const startSec = R.startSec;
const recordDur = Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", AUDIO]).toString());

// the map, inverted: reg seconds → take seconds (both monotonic, so one walk)
const pairs = readFileSync(resolve(LANE, "src/vox/reg/timemap.txt"), "utf8").trim().split("\n").map((l) => l.trim().split(/\s+/).map(Number)).filter((p) => p.length === 2);
const takeOf = (reg) => { const x = reg * SR; let i = 1; while (i < pairs.length - 1 && pairs[i][1] < x) i++;
  const [s0, d0] = pairs[i - 1], [s1, d1] = pairs[i]; const f = d1 === d0 ? 0 : (x - d0) / (d1 - d0); return (s0 + (s1 - s0) * f) / SR; };

// the take's frames, once, on the output grid (scratch beside the lane's out/)
const probe = execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0", "-show_entries", "stream=width,height,r_frame_rate", "-of", "csv=p=0", SRC]).toString().trim().split(",");
const SRC_FPS = eval(probe[2]) || 30, XFPS = Math.min(FPS, SRC_FPS);     // the cache holds each take frame once, even for a 60 fps output
const FRAMES = resolve(OUT, `.take-frames-${XFPS}-${HEIGHT}`);
if (!existsSync(FRAMES) || readdirSync(FRAMES).length < 100) {
  mkdirSync(FRAMES, { recursive: true });
  console.log(`▸ extracting ${probe[0]}×${probe[1]} @ ${probe[2]} → ${XFPS} fps, ${HEIGHT}p`);
  execFileSync("ffmpeg", ["-v", "error", "-y", "-i", SRC, "-vf", `fps=${XFPS},scale=-2:${HEIGHT}`, "-q:v", "2", resolve(FRAMES, "f%06d.jpg")], { stdio: "inherit" });
}
const nSrc = readdirSync(FRAMES).filter((f) => f.endsWith(".jpg")).length;

// one source frame per output frame
const nOut = Math.ceil(recordDur * FPS);
const lines = ["ffconcat version 1.0"]; let held = 0, skipped = 0, prev = -1;
for (let k = 0; k < nOut; k++) {
  const take = takeOf(k / FPS + startSec);
  let idx = Math.min(nSrc, Math.max(1, Math.round(take * XFPS) + 1));
  if (idx === prev) held++; else if (prev >= 0 && idx > prev + 1) skipped += idx - prev - 1;
  prev = idx;
  lines.push(`file 'f${String(idx).padStart(6, "0")}.jpg'`, `duration ${(1 / FPS).toFixed(6)}`);
}
lines.push(`file 'f${String(prev).padStart(6, "0")}.jpg'`);
const list = resolve(FRAMES, `${stem}.ffconcat`);
writeFileSync(list, lines.join("\n") + "\n");
console.log(`▸ ${nOut} frames · ${held} held · ${skipped} skipped · record opens at take ${takeOf(startSec).toFixed(2)} s`);

// picture + the remix. --strip <mp4> lays perf-strip.mjs's lanes along the bottom (translucent);
// --grade shifts the picture's colour slowly with the sections (hue / saturation / brightness,
// eased over 3 s at each boundary) — "slowly affect the colors of the video as it changes sections"
const outPath = resolve(OUT, `${stem}-perf.mp4`);
const STRIP = arg("strip") ? resolve(arg("strip")) : null, COVER = !!arg("cover"), GRADE = !arg("no-grade") && !COVER;
// --cover: the single's cover look (bin/cover.py v8) instead of the section hue drift — the room near neutral, the shirt
// pushed deep red and the magentas pulled toward it, wood warm, a hard S with the blacks down, saturation up, and on
// the upscale a real sharpen (CAS + unsharp). "sharper, deeper red, contrast — like the cover" (jeffrey, v103).
const COVER_GRADE = ",selectivecolor=reds=-0.35 0.15 0.12 0.12:magentas=-0.1 0.2 0.35 0.1:yellows=-0.1 0.05 0.15 0:whites=0 0 0 -0.04,curves=master='0/0 0.1/0.06 0.5/0.5 0.9/0.95 1/1',eq=saturation=1.22:contrast=1.06";
const LOOK = { intro: [0, 0.85, -0.03], verse1: [4, 0.95, 0], chorus1: [14, 1.18, 0.04], verse2: [-10, 0.95, 0], chorus2: [30, 1.42, 0.08], break: [-28, 0.8, -0.03], bridge: [18, 1.2, 0.04], outro: [36, 1.48, 0.09] };
const secs = (R.sections || []).map((x) => ({ name: x.name, a: x.start - startSec })).filter((x) => LOOK[x.name]).sort((x, y) => x.a - y.a);
const gradeExpr = (k) => { let e = String(LOOK[secs[0]?.name || "intro"][k]); for (let i = 1; i < secs.length; i++) { const d = LOOK[secs[i].name][k] - LOOK[secs[i - 1].name][k];
  if (d) e += `+(${d.toFixed(3)})*clip((t-${Math.max(0, secs[i].a).toFixed(2)})/3,0,1)`; } return e; };
const grade = COVER ? COVER_GRADE : GRADE ? `,hue=h='${gradeExpr(0)}':s='${gradeExpr(1)}':b='${gradeExpr(2)}'` : "";
const inputs = ["-f", "concat", "-safe", "0", "-i", list, "-i", AUDIO, ...(STRIP ? ["-i", STRIP] : [])];
const up = UP ? `,scale=-2:${UP}:flags=lanczos${COVER ? ",cas=0.55,unsharp=5:5:0.5:5:5:0" : ",unsharp=5:5:0.4:5:5:0"}` : COVER ? ",cas=0.4" : "";
const graph = STRIP
  ? `[0:v]fps=${FPS},format=yuv420p${grade}${up}[v];[2:v]format=rgba,colorchannelmixer=aa=0.82[s];[v][s]overlay=0:main_h-overlay_h:format=auto,format=yuv420p[o]`
  : `[0:v]fps=${FPS},format=yuv420p${grade}${up}[o]`;
execFileSync("ffmpeg", ["-v", "error", "-y", ...inputs, "-filter_complex", graph, "-map", "[o]", "-map", "1:a",
  "-c:v", "libx264", "-crf", "15", "-preset", "medium", "-c:a", "aac", "-b:a", "256k", "-movflags", "+faststart", "-shortest", outPath], { stdio: "inherit" });
console.log(`✓ ${outPath}${STRIP ? " (+strip)" : ""}${COVER ? " (cover grade)" : GRADE ? " (graded)" : ""}${UP ? ` (up ${UP})` : ""}`);
