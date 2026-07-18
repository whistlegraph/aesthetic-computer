// warp-to-sing.mjs — stretch a talking-head video so the mouth rides a sung take.
//
// The sick test. We have the same 169 words twice: once as the man actually
// spoke them (his lips, in the video) and once sung (ElevenLabs → WORLD, on a
// melody). Singing stretches words unevenly — a held note on "designer" runs
// four times the length of the spoken word. So we rubber-band the VIDEO onto the
// sung clock: every spoken word's frames are stretched or squeezed to fill its
// sung word's slot, and his mouth ends up roughly tracking the melody.
//
//   node warp-to-sing.mjs <video> \
//     --spoken yc.json           (whisper on the ORIGINAL audio — spoken timing)
//     --sung   sung-alignment.json  (the sung word timing)
//     --audio  jeffrey-sung.mp3   (laid under the warped picture)
//     --out    warped.mp4
//
// The map is piecewise-linear through (sungStart[i] → spokenStart[i]) anchors,
// monotonic, so the source can be streamed: for each output frame we pull source
// frames until we reach its mapped time — holding the last one when the sung word
// runs long (stretch), skipping ahead when it runs short (compress).

import { spawn, spawnSync } from "node:child_process";
import { readFileSync, existsSync } from "node:fs";
import { resolve } from "node:path";

const argv = process.argv.slice(2);
const flag = (n, d = null) => {
  const i = argv.indexOf(`--${n}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : d;
};

const SRC = resolve(argv.find((a) => !a.startsWith("--")));
const SPOKEN = resolve(flag("spoken"));
const SUNG = resolve(flag("sung"));
const AUDIO = resolve(flag("audio"));
const OUT = resolve(flag("out", "warped.mp4"));
const FPS = 30;
for (const [p, n] of [[SRC, "video"], [SPOKEN, "--spoken"], [SUNG, "--sung"], [AUDIO, "--audio"]]) {
  if (!existsSync(p)) { console.error(`✗ ${n} not found: ${p}`); process.exit(1); }
}

const sh = (c, a) => {
  const r = spawnSync(c, a, { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`${c}: ${r.stderr?.slice(0, 300)}`);
  return r.stdout;
};

// whisper words (seconds) from the spoken side, with START and END. words.mjs'
// merge logic inline: a leading space (or the first token) begins a word, and
// each token extends the current word's end.
function spokenWords() {
  const segs = JSON.parse(readFileSync(SPOKEN, "utf8")).transcription;
  const out = [];
  for (const s of segs) {
    if (!s.text.trim()) continue;
    if (s.text.startsWith(" ") || out.length === 0) {
      out.push({ from: s.offsets.from / 1000, to: s.offsets.to / 1000 });
    } else {
      out[out.length - 1].to = s.offsets.to / 1000;
    }
  }
  return out;
}
// sung words — accept either a bare word array or an alignment doc with `.words`
const sungDoc = JSON.parse(readFileSync(SUNG, "utf8"));
const sungWords = Array.isArray(sungDoc) ? sungDoc : sungDoc.words;
const sung = sungWords.map((w) => ({ from: (w.fromMs ?? w.from) / 1000, to: (w.toMs ?? w.to) / 1000 }));
const spoken = spokenWords();

const n = Math.min(spoken.length, sung.length);

// Each WORD is a segment: the sung word's slot maps to the spoken word's video,
// at one steady rate across the whole word (no mid-word speed change — that was
// the glitch). Boundaries are pinned at BOTH ends so a held note stretches the
// whole word's mouth movement rather than freezing on a frame.
//
// The boundaries are SNAPPED to the frame grid — output times to the output
// frame period, source times to the source frame period — so every segment maps
// an integer run of source frames onto an integer run of output frames. That is
// the "quantize to frame timings" step: it removes the sub-frame drift where a
// word boundary lands 40% into a frame and the remap fights the sampler.
//
// All times stay in double-precision seconds. A double resolves time to well
// under a nanosecond across a 60-second clip, so the frame/sample grids never
// disagree at the boundary — the snap is the only quantization, and it's exact.
const sungDur = parseFloat(sh("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", AUDIO]).trim());
const srcDur = parseFloat(sh("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "csv=p=0", SRC]).trim());

// Build explicit SEGMENTS, each mapping an output-frame span to a source-frame
// span, snapped to the frame grid (so every segment is a whole number of frames
// on both sides — the "quantize to frame timings" step). Two kinds:
//
//   word — the frames while a word is spoken. We play its real frames, in order,
//          because that's where the mouth actually moves; a held note just plays
//          them slower.
//   gap  — a pause between words. The source is a near-still face here, so
//          playing it looks frozen. Instead we CROSSFADE evenly between the two
//          connecting frames — the last frame of the word before and the first
//          of the word after — so the mouth morphs continuously across the pause
//          instead of stopping. This is the "interpolate equally between the
//          connected frames" fix.
const OF = (t) => Math.round(t * FPS); // seconds → frame index (nanosecond-safe)
const segs = [];
let oCur = 0;
let sCur = 0;
const addSeg = (oEnd, sEnd, gap) => {
  if (oEnd <= oCur) return;
  segs.push({ o0: oCur, o1: oEnd, s0: sCur, s1: sEnd, gap });
  oCur = oEnd;
  sCur = sEnd;
};
for (let i = 0; i < n; i += 1) {
  // lead-in to the first word, and every pause, is a gap
  if (OF(sung[i].from) > oCur) addSeg(OF(sung[i].from), OF(spoken[i].from), true);
  addSeg(OF(sung[i].to), OF(spoken[i].to), false); // the word itself
}
addSeg(OF(sungDur), OF(srcDur), true); // tail

const [W, Hh] = sh("ffprobe", ["-v", "error", "-select_streams", "v",
  "-show_entries", "stream=width,height", "-of", "csv=p=0", SRC]).trim().split(",").map(Number);
const FB = W * Hh * 4;
const gaps = segs.filter((s) => s.gap).length;
console.log(`· ${W}×${Hh} · ${segs.length} segments (${gaps} gaps morphed) · sung ${sungDur.toFixed(1)}s ← spoken ${srcDur.toFixed(1)}s`);

const dec = spawn("ffmpeg", ["-v", "error", "-i", SRC, "-f", "rawvideo", "-pix_fmt", "rgba",
  "-vf", `fps=${FPS}`, "-"], { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${W}x${Hh}`, "-r", String(FPS), "-i", "-",
  "-i", AUDIO, "-map", "0:v", "-map", "1:a",
  "-c:v", "libx264", "-preset", "ultrafast", "-crf", "20", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUT],
  { stdio: ["pipe", "inherit", "inherit"] });

const write = (b) => new Promise((r) => (enc.stdin.write(b) ? r() : enc.stdin.once("drain", r)));

const outFrames = OF(sungDur);

// A rolling cache of decoded source frames, keyed by frame index. We read forward
// and evict below the oldest frame still needed. Gaps need their far endpoint
// frame held while the near one is shown, so eviction lags by the current
// segment's start.
const it = dec.stdout[Symbol.asyncIterator]();
let buf = Buffer.alloc(0);
let decDone = false;
let maxRead = -1;
const cache = new Map();
const readTo = async (idx) => {
  while (maxRead < idx && !decDone) {
    while (buf.length < FB && !decDone) {
      const { value, done } = await it.next();
      if (done) { decDone = true; break; }
      buf = buf.length ? Buffer.concat([buf, value]) : value;
    }
    if (buf.length < FB) { decDone = true; break; }
    maxRead += 1;
    cache.set(maxRead, Uint8Array.prototype.slice.call(buf, 0, FB));
    buf = buf.subarray(FB);
  }
};
const frame = async (idx) => {
  const i = Math.max(0, idx);
  await readTo(i);
  return cache.get(Math.min(i, maxRead));
};

// Per-pixel linear crossfade a→b by f, into outBuf.
const outBuf = Buffer.allocUnsafe(FB);
const blendInto = (a, b, f) => {
  if (f < 0.001 || a === b) { outBuf.set(a); return; }
  if (f > 0.999) { outBuf.set(b); return; }
  const inv = 1 - f;
  for (let p = 0; p < FB; p += 1) outBuf[p] = (a[p] * inv + b[p] * f) | 0;
};

let minNeeded = 0;
for (const s of segs) {
  const oLen = s.o1 - s.o0;
  for (let k = 0; k < oLen; k += 1) {
    const of = s.o0 + k;
    if (of >= outFrames) break;
    const p = oLen > 0 ? k / oLen : 0; // 0..1 through the segment

    if (s.gap) {
      // even crossfade between the two connecting frames — no dwell
      minNeeded = s.s0;
      const a = await frame(s.s0);
      const b = await frame(s.s1);
      blendInto(a, b, p);
    } else {
      // step through the word's real frames at constant rate, blending neighbors
      const sf = s.s0 + p * (s.s1 - s.s0);
      const i0 = Math.floor(sf);
      minNeeded = i0;
      const a = await frame(i0);
      const b = await frame(i0 + 1);
      blendInto(a, b, sf - i0);
    }

    await write(outBuf);
    for (const key of cache.keys()) if (key < minNeeded) cache.delete(key);
    if (of % 150 === 0) process.stdout.write(`\r  ${of}/${outFrames}`);
  }
}
// The output is done, but the decoder still has frames to hand over (a spoken
// clip is longer than we consumed once time is squeezed). Nobody's reading them,
// so its pipe would block forever and the process would hang until something
// kills it. Kill it ourselves.
enc.stdin.end();
dec.kill("SIGKILL");
await new Promise((r) => enc.on("close", r));
process.stdout.write(`\r  ${outFrames}/${outFrames}\n✓ ${OUT}\n`);
