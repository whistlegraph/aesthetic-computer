// run a real video through the tone grid and watch it come back.
//
//   node tapes/bin/videotest.mjs [source.mp4]
//
// output lands on the desktop: one mp4 per grade per route, each carrying the
// modem audio as its own soundtrack — you are hearing the picture you see.

import { execFileSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync, readFileSync, mkdirSync } from "node:fs";
import { tmpdir, homedir } from "node:os";
import { join, basename, extname } from "node:path";
import { encode, decode, budget, fpsFor } from "../lib/video.mjs";
import * as C from "../lib/channel.mjs";
import * as wav from "../lib/wav.mjs";

const SRC = process.argv[2] ?? "system/public/whistlegraph.org/canonical/imab.mp4";
const OUT = join(homedir(), "Desktop", "tapes");
mkdirSync(OUT, { recursive: true });
const tmp = mkdtempSync(join(tmpdir(), "tapesvid-"));
const STEM = basename(SRC, extname(SRC));

const probe = (k) =>
  execFileSync("ffprobe", ["-v", "error", "-select_streams", "v:0",
    "-show_entries", `stream=${k}`, "-of", "csv=p=0", SRC]).toString().trim();
const srcW = +probe("width"), srcH = +probe("height");
const dur = +execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", SRC]).toString().trim();
const aspect = srcH / srcW;

console.log(`🎬 ${SRC}\n   ${srcW}×${srcH}, ${dur.toFixed(1)}s`);

// each grade spends the same cells/sec differently — detail or motion.
const grades = [
  { name: "detail", w: 56, opts: { spacing: 1, win: "rect" } },
  { name: "motion", w: 34, opts: { spacing: 1, win: "rect" } },
  { name: "tape", w: 24, opts: { spacing: 8, win: "bh", range: 36 } },
];

const routes = {
  clean: (x) => x,
  "mp3-192": (x, r) => C.transcode(x, r, C.codecs["mp3 192"]),
  "cassette-walkman": (x, r) => C.cassette(x, r, { snr: 45, wow: 0.003 }),
};

const plan = [
  ["detail", "clean"], ["detail", "mp3-192"],
  ["motion", "clean"], ["motion", "mp3-192"],
  ["tape", "cassette-walkman"],
];

const even = (n) => Math.max(2, Math.round(n / 2) * 2);

function framesOf(w, h, fps) {
  const raw = join(tmp, `f-${w}x${h}-${fps}.gray`);
  execFileSync("ffmpeg", ["-y", "-i", SRC, "-vf",
    `fps=${fps},scale=${w}:${h}:flags=area,format=gray`,
    "-f", "rawvideo", "-pix_fmt", "gray", raw], { stdio: "ignore" });
  const buf = readFileSync(raw);
  const size = w * h;
  const out = [];
  for (let i = 0; i + size <= buf.length; i += size) {
    const f = new Float64Array(size);
    for (let j = 0; j < size; j += 1) f[j] = buf[i + j] / 255;
    out.push(f);
  }
  return out;
}

function toMp4(frames, w, h, fps, audioPath, dest, scale) {
  const raw = join(tmp, "dec.gray");
  const buf = Buffer.alloc(frames.length * w * h);
  frames.forEach((f, i) => {
    for (let j = 0; j < w * h; j += 1)
      buf[i * w * h + j] = Math.max(0, Math.min(255, Math.round(f[j] * 255)));
  });
  writeFileSync(raw, buf);
  execFileSync("ffmpeg", ["-y",
    "-f", "rawvideo", "-pix_fmt", "gray", "-s", `${w}x${h}`, "-r", String(fps), "-i", raw,
    "-i", audioPath,
    "-vf", `scale=${even(w * scale)}:${even(h * scale)}:flags=neighbor`,
    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18",
    "-c:a", "aac", "-b:a", "192k", "-shortest", dest], { stdio: "ignore" });
}

const done = [];

for (const [gradeName, routeName] of plan) {
  const g = grades.find((x) => x.name === gradeName);
  const h = even(g.w * aspect); // yuv420p refuses odd dimensions
  const fps = Math.max(1, Math.round(fpsFor(g.w, h, g.opts) * 10) / 10);
  const b = budget(g.opts);

  const frames = framesOf(g.w, h, fps);
  const enc = encode(frames, g.opts);

  C.seed(0x5eed);
  const played = routes[routeName](enc.samples, enc.plan.rate);
  const back = decode(played, {
    at: enc.start, count: frames.length, size: g.w * h, ...g.opts,
  });

  // error against the source frames, as a plain rms over all pixels
  let se = 0, n = 0;
  frames.forEach((f, i) => {
    for (let j = 0; j < f.length; j += 1) {
      const d = f[j] - (back[i]?.[j] ?? 0);
      se += d * d; n += 1;
    }
  });
  const psnr = 10 * Math.log10(1 / (se / n));

  const audio = join(tmp, `a-${gradeName}-${routeName}.wav`);
  wav.write(audio, played, enc.plan.rate);
  const dest = join(OUT, `${STEM}-${gradeName}-${routeName}.mp4`);
  toMp4(back, g.w, h, fps, audio, dest, Math.max(4, Math.round(480 / g.w)));

  const realtime = enc.secs / dur;
  console.log(
    `\n📼 ${gradeName} / ${routeName}` +
    `\n   ${g.w}×${h} @ ${fps} fps · ${Math.round(b.cells)} greys/sec · ${b.rows} tones` +
    `\n   ${frames.length} frames → ${enc.secs.toFixed(1)}s audio (${realtime.toFixed(2)}× the video's length)` +
    `\n   ${psnr.toFixed(1)} dB → ${dest.replace(homedir(), "~")}`,
  );
  done.push(dest);
}

// one side-by-side so the source and the decode can be compared honestly
const g = grades[0], h = even(g.w * aspect);
const fps = Math.max(1, Math.round(fpsFor(g.w, h, g.opts) * 10) / 10);
const side = join(OUT, `${STEM}-sidebyside.mp4`);
execFileSync("ffmpeg", ["-y",
  "-i", SRC, "-i", join(OUT, `${STEM}-detail-clean.mp4`),
  "-filter_complex",
  `[0:v]fps=${fps},scale=-2:640,format=gray[a];[1:v]scale=-2:640:flags=neighbor,format=gray[b];[a][b]hstack=inputs=2`,
  "-map", "1:a", "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18",
  "-c:a", "aac", "-shortest", side], { stdio: "ignore" });
console.log(`\n🎞  side-by-side → ${side.replace(homedir(), "~")}`);

rmSync(tmp, { recursive: true, force: true });
console.log(`\n📂 ${done.length + 1} files in ~/Desktop/tapes`);
