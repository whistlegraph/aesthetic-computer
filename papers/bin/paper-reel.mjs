#!/usr/bin/env node
// paper-reel.mjs — a paper told in under a minute: narrated slides with a
// slow push on each picture, captions that ease in, and a soft bed.
//
// The format (first made for PLOrk'ing the Planet, 2026-11-20):
//   - portrait 1080x1350, one claim per slide, five slides or so
//   - each slide: one picture in the top box, a title in the paper's title
//     face, one or two lines under it, a small foot line
//   - each slide carries one spoken line in @jeffrey's voice (/api/say); the
//     slide lasts as long as its line
//   - the picture pushes in or out a few percent, alternating; a QR or any
//     slide marked "pixel" barely moves so it still scans
//   - a bed from the podcast's generator sits well under the voice
//
// Usage:
//   node papers/bin/paper-reel.mjs papers/arxiv-plork/mint/reel.json
//   node papers/bin/paper-reel.mjs reel.json --out ~/Desktop/reel.mp4 --silent
//
// reel.json, paths relative to the file:
//   { "out": "plork-reel.mp4", "bed": "felt",
//     "slides": [ { "image": "../figures/cover-nov.png", "title": "…",
//                   "sub": "line one\nline two", "foot": "…", "say": "…",
//                   "pixel": false } ] }
//
// Write "say" the way it should be spoken: "prompt dot A C, slash O S", not
// "prompt.ac/os". After the build the mixed audio is transcribed with
// whisper-cli when it is installed, so a line the voice garbled shows up
// before the file is posted. Needs ffmpeg and ImageMagick.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "..", "..");
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  if (argv[i] === "--silent") flags.silent = true;
  else if (argv[i].startsWith("--")) flags[argv[i].slice(2)] = argv[++i];
  else positional.push(argv[i]);
}
const fail = (message) => { console.error(`✗ ${message}`); process.exit(1); };
if (!positional[0] || !existsSync(positional[0])) fail("usage: paper-reel.mjs <reel.json> [--out file.mp4] [--silent]");

const specPath = resolve(positional[0]);
const base = dirname(specPath);
const spec = JSON.parse(readFileSync(specPath, "utf8"));
if (!Array.isArray(spec.slides) || !spec.slides.length) fail("reel.json needs a slides array");
const OUT = resolve((flags.out || resolve(base, spec.out || "reel.mp4")).replace(/^~/, homedir()));
const WORK = resolve(base, ".reel");
mkdirSync(WORK, { recursive: true });

const W = 1080, H = 1350, FPS = 30;
const BOX = { w: 980, h: 800, top: 60 };   // the picture's box
const FADE = 0.5, PAD = 0.9, LEAD = 0.45;  // crossfade, air after a line, air before it
const TITLE_FACE = resolve(REPO, "system/public/type/webfonts/ywft-processing-bold.ttf");
// The title face has unreliable punctuation, so the lines under it use a mono
// face that carries every mark.
const LINE_FACE = [process.env.REEL_LINE_FACE, "/System/Library/Fonts/Menlo.ttc", "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"].find((face) => face && existsSync(face));
if (!LINE_FACE) fail("no mono face found for the caption lines; set REEL_LINE_FACE");

const run = (command, args) => execFileSync(command, args, { stdio: ["ignore", "pipe", "pipe"], maxBuffer: 64 * 1024 * 1024 }).toString();
const seconds = (file) => Number(run("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", file]).trim());

// ── narration, one line per slide, cached by what was asked for ─────────────
async function say(text) {
  const body = { from: text, provider: spec.voice?.provider || "jeffrey", voice: spec.voice?.voice || "neutral:0", stability: 0.5, similarity: 0.8 };
  const file = resolve(WORK, `say-${createHash("sha256").update(JSON.stringify(body)).digest("hex").slice(0, 16)}.mp3`);
  if (existsSync(file)) return file;
  const res = await fetch(process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(body), redirect: "follow" });
  if (!res.ok) fail(`/api/say ${res.status}: ${(await res.text()).slice(0, 200)}`);
  writeFileSync(file, Buffer.from(await res.arrayBuffer()));
  return file;
}

// ── the two layers of a slide: a picture at twice its shown size, and a
//    transparent sheet of captions ───────────────────────────────────────────
function layers(slide, index) {
  const image = resolve(base, slide.image);
  if (!existsSync(image)) fail(`slide ${index + 1}: no such image ${slide.image}`);
  const picture = resolve(WORK, `pic-${index}.png`);
  const fit = `${BOX.w * 2}x${BOX.h * 2}`;
  run("magick", slide.pixel ? [image, "-filter", "point", "-resize", fit, picture] : [image, "-resize", fit, "-background", "white", "-alpha", "remove", "-alpha", "off", picture]);
  const [width, height] = run("magick", ["identify", "-format", "%w %h", picture]).split(" ").map((n) => Math.round(Number(n) / 2));
  const captions = resolve(WORK, `txt-${index}.png`);
  run("magick", ["-size", `${W}x${H}`, "xc:none",
    "-font", TITLE_FACE, "-pointsize", "88", "-fill", "#40384A", "-gravity", "south", "-annotate", "+0+330", slide.title || "",
    "-font", LINE_FACE, "-pointsize", "42", "-fill", "#B44887", "-gravity", "south", "-annotate", "+0+175", slide.sub || "",
    "-font", LINE_FACE, "-pointsize", "30", "-fill", "#555555", "-gravity", "south", "-annotate", "+0+70", slide.foot || "",
    `PNG32:${captions}`]);
  return { picture, captions, width, height };
}

const slides = [];
for (const [index, slide] of spec.slides.entries()) {
  const voice = flags.silent || !slide.say ? null : await say(slide.say);
  const length = (voice ? seconds(voice) : Number(slide.seconds || 3.4)) + PAD + (index === spec.slides.length - 1 ? 1.0 : 0);
  slides.push({ ...slide, ...layers(slide, index), voice, length });
  console.log(`  slide ${index + 1}: ${length.toFixed(2)}s  ${slide.title || ""}`);
}
const start = [0];
for (let i = 1; i < slides.length; i++) start.push(start[i - 1] + slides[i - 1].length - FADE);
const total = start.at(-1) + slides.at(-1).length;

// ── one ffmpeg graph: push each picture, ease its captions in, crossfade the
//    slides, then lay the lines over the bed ─────────────────────────────────
const inputs = [], graph = [];
slides.forEach((slide, i) => {
  const frames = Math.round(slide.length * FPS);
  const amount = slide.pixel ? 0.015 : Number(slide.zoom || 0.06);
  const zoom = i % 2 ? `${1 + amount}-${amount}*on/${frames}` : `1+${amount}*on/${frames}`;
  inputs.push("-i", slide.picture, "-loop", "1", "-t", slide.length.toFixed(3), "-i", slide.captions);
  graph.push(
    `[${2 * i}]zoompan=z='${zoom}':x='iw/2-(iw/zoom/2)':y='ih/2-(ih/zoom/2)':d=${frames}:s=${slide.width}x${slide.height}:fps=${FPS}[p${i}]`,
    `[${2 * i + 1}]format=rgba,fade=in:st=0.2:d=0.6:alpha=1[t${i}]`,
    `color=white:s=${W}x${H}:r=${FPS}:d=${slide.length.toFixed(3)}[b${i}]`,
    `[b${i}][p${i}]overlay=x=${Math.round((W - slide.width) / 2)}:y=${BOX.top}:shortest=1[c${i}]`,
    `[c${i}][t${i}]overlay=x=0:y='max(0,26*(1-t/0.8))':shortest=1,format=yuv420p,setsar=1[s${i}]`,
  );
});
let last = "[s0]";
for (let i = 1; i < slides.length; i++) { graph.push(`${last}[s${i}]xfade=transition=fade:duration=${FADE}:offset=${start[i].toFixed(3)}[x${i}]`); last = `[x${i}]`; }
graph.push(`${last}format=yuv420p[v]`);

let next = 2 * slides.length;
const voiced = slides.map((slide, i) => ({ slide, i })).filter(({ slide }) => slide.voice);
const audio = [];
if (voiced.length) {
  for (const { slide, i } of voiced) {
    const delay = Math.round((start[i] + LEAD) * 1000);
    inputs.push("-i", slide.voice);
    graph.push(`[${next}]aresample=44100,adelay=${delay}|${delay}[a${i}]`);
    next++;
  }
  graph.push(`${voiced.map(({ i }) => `[a${i}]`).join("")}amix=inputs=${voiced.length}:normalize=0,loudnorm=I=-16:TP=-1.5:LRA=11,apad=whole_dur=${total.toFixed(3)},pan=stereo|c0=c0|c1=c0[vo]`);
  audio.push("[vo]");
}
if (spec.bed !== false) {
  const bed = resolve(WORK, "bed.wav");
  const { renderBed } = await import(resolve(REPO, "marketing/podcast/bin/jingle.mjs"));
  renderBed(total + 2, bed, { kit: typeof spec.bed === "string" ? spec.bed : "felt" });
  inputs.push("-i", bed);
  // Under a voice the bed sits low; alone it can come up.
  graph.push(`[${next}]aresample=44100,atrim=0:${total.toFixed(3)},volume=${voiced.length ? 0.2 : 0.5},afade=t=in:st=0:d=0.8,afade=t=out:st=${(total - 1.6).toFixed(3)}:d=1.6[bed]`);
  audio.push("[bed]");
}
if (!audio.length) { inputs.push("-f", "lavfi", "-t", total.toFixed(3), "-i", "anullsrc=r=44100:cl=stereo"); graph.push(`[${next}]anull[a]`); }
else graph.push(`${audio.join("")}${audio.length > 1 ? `amix=inputs=${audio.length}:normalize=0,` : ""}alimiter=limit=0.89[a]`);

mkdirSync(dirname(OUT), { recursive: true });
run("ffmpeg", ["-y", "-loglevel", "error", ...inputs, "-filter_complex", graph.join(";"), "-map", "[v]", "-map", "[a]",
  "-r", String(FPS), "-c:v", "libx264", "-crf", "19", "-preset", "medium", "-c:a", "aac", "-b:a", "160k", "-ar", "44100",
  "-t", total.toFixed(3), "-movflags", "+faststart", OUT]);
console.log(`✓ ${OUT}  ${total.toFixed(1)}s`);

// ── hear it back: what the mix says, against what was asked for ─────────────
const model = [process.env.WHISPER_MODEL, resolve(homedir(), ".whisper-models/ggml-small.bin")].find((path) => path && existsSync(path));
if (voiced.length && model) {
  try {
    const wav = resolve(WORK, "heard.wav");
    run("ffmpeg", ["-y", "-loglevel", "error", "-i", OUT, "-ar", "16000", "-ac", "1", wav]);
    const heard = run("whisper-cli", ["-m", model, "-l", "en", "-np", "-nt", wav]).replace(/\s+/g, " ").trim();
    console.log(`  heard: ${heard}`);
    console.log("  Read it against the say lines before posting.");
  } catch { console.log("  (whisper-cli not available; listen before posting)"); }
}
