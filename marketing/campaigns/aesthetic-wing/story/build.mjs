#!/usr/bin/env node
// Build the still-motion Aesthetic Wing v1 reel.
//
// Redesigned GPT Image 2 panels, word-aligned Jeffrey voiceover, original AC
// synth/music/SFX, restrained rostrum movement, and burned captions. Panels
// remain drop-in compatible with the Seedance motion pass.

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const CAMPAIGN = resolve(HERE, "..");
const REPO = resolve(HERE, "../../../..");
const GENS = join(CAMPAIGN, "gens");
const BUILD = join(HERE, "build");
const MOTION = join(HERE, "motion");
mkdirSync(BUILD, { recursive: true });

const FPS = 30;
const W = 1080;
const H = 1920;
const TAIL = 1.15;

const VO = join(HERE, "vo-v1.mp3");
const ALIGNMENT = `${VO}.alignment.json`;
const SOUND = join(HERE, "aesthetic-wing-sound.wav");
const OUT = join(HERE, "aesthetic-wing-reel-v1.mp4");

for (const required of [VO, ALIGNMENT, SOUND]) {
  if (!existsSync(required)) throw new Error(`missing ${required}`);
}

const run = (bin, args) => execFileSync(bin, args, {
  cwd: HERE,
  stdio: ["ignore", "inherit", "inherit"],
});
const ff = (args) => run("ffmpeg", ["-y", "-loglevel", "error", ...args]);

const align = JSON.parse(readFileSync(ALIGNMENT, "utf8"));
const words = align.words;
const chunks = [
  { text: "this is Aesthetic Wing.", n: 4, slide: "road" },
  { text: "on the street, it's a one-wheel.", n: 6, slide: "road" },
  { text: "then the street runs out.", n: 5, slide: "road" },
  { text: "the wing opens.", n: 3, slide: "unfold" },
  { text: "the wheel gets light.", n: 4, slide: "unfold" },
  { text: "press your feet, and the ground falls away.", n: 8, slide: "lift" },
  { text: "one foot. four feet. eight.", n: 5, slide: "lift" },
  { text: "lean into the air.", n: 4, slide: "lift" },
  { text: "rise over the interruption.", n: 4, slide: "lift" },
  { text: "settle when you're ready.", n: 4, slide: "settle" },
  { text: "not a flying car.", n: 4, slide: "settle" },
  { text: "street aviation.", n: 2, slide: "settle" },
  { text: "Aesthetic Wing from aesthetic.computer.", n: 6, slide: "design" },
];

let wi = 0;
for (const chunk of chunks) {
  chunk.from = words[wi].fromMs / 1000;
  chunk.to = words[wi + chunk.n - 1].toMs / 1000;
  wi += chunk.n;
}
if (wi !== words.length) {
  throw new Error(`caption chunks cover ${wi} words; alignment has ${words.length}`);
}

const voEnd = words.at(-1).toMs / 1000;
const total = voEnd + TAIL;
const slides = [];
for (const chunk of chunks) {
  if (!slides.length || slides.at(-1).name !== chunk.slide) {
    slides.push({ name: chunk.slide, from: chunk.from });
  }
}
slides.forEach((slide, i) => {
  slide.to = slides[i + 1]?.from ?? total;
});

const sources = {
  road: join(GENS, "v8-road-mode.png"),
  unfold: join(GENS, "v5-wing-wakes.png"),
  lift: join(GENS, "v6-more-lift.png"),
  settle: join(GENS, "v7-settle.png"),
  design: join(GENS, "v4-design-bible.png"),
};
const takesPath = join(MOTION, "takes.json");
const takes = existsSync(takesPath) ? JSON.parse(readFileSync(takesPath, "utf8")) : {};

for (const source of Object.values(sources)) {
  if (!existsSync(source)) throw new Error(`missing ${source}`);
}

const segs = [];
for (let i = 0; i < slides.length; i++) {
  const slide = slides[i];
  const duration = slide.to - slide.from;
  const frames = Math.max(1, Math.ceil(duration * FPS));
  const seg = join(BUILD, `seg-${i}-${slide.name}.mp4`);
  segs.push(seg);

  const defaultTake = join(MOTION, `aesthetic-wing-v1-shot-${i}-${slide.name}.mp4`);
  const motion = takes[slide.name] ? resolve(MOTION, takes[slide.name]) : defaultTake;
  if (existsSync(motion)) {
    ff([
      "-i", motion,
      "-t", duration.toFixed(3),
      "-vf", `scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},fps=${FPS},format=yuv420p`,
      "-an", "-r", String(FPS),
      "-c:v", "libx264", "-preset", "medium", "-crf", "17",
      seg,
    ]);
    continue;
  }

  const close = slide.name === "unfold";
  const z0 = close ? 1.08 : 1.0;
  const dz = close ? 0.00065 : 0.00028;
  const filter = [
    `scale=2160:3840:force_original_aspect_ratio=increase`,
    "crop=2160:3840",
    `zoompan=z='min(${z0}+on*${dz},${close ? 1.19 : 1.06})':x='iw/2-(iw/zoom/2)':y='ih/2-(ih/zoom/2)':d=${frames}:s=${W}x${H}:fps=${FPS}`,
    "format=yuv420p",
  ].join(",");

  ff([
    "-loop", "1", "-i", sources[slide.name],
    "-t", duration.toFixed(3),
    "-vf", filter,
    "-an", "-r", String(FPS),
    "-c:v", "libx264", "-preset", "medium", "-crf", "17",
    seg,
  ]);
}

const concat = join(BUILD, "concat.txt");
writeFileSync(concat, segs.map((p) => `file '${p.replaceAll("'", "'\\''")}'`).join("\n") + "\n");
const base = join(BUILD, "base.mp4");
ff(["-f", "concat", "-safe", "0", "-i", concat, "-c", "copy", base]);

// This host's lean ffmpeg build has no libass/drawtext, so captions are
// rendered once with ImageMagick and overlaid as timed transparent images.
// That keeps typography deterministic and avoids platform subtitle drift.
const captionFont = join(REPO, "system/public/type/webfonts/ywft-processing-bold.ttf");
const captionPngs = chunks.map((chunk, i) => {
  const out = join(BUILD, `caption-${i}.png`);
  run("magick", [
    "-background", "none",
    "-fill", "white",
    "-stroke", "#1D1D1D",
    "-strokewidth", "5",
    "-font", captionFont,
    "-pointsize", "52",
    "-gravity", "center",
    "-size", "900x230",
    `caption:${chunk.text}`,
    out,
  ]);
  return out;
});

const finalArgs = ["-i", base, "-i", VO, "-i", SOUND];
for (const cap of captionPngs) finalArgs.push("-loop", "1", "-i", cap);

const overlays = [];
let prior = "0:v";
for (let i = 0; i < chunks.length; i++) {
  const chunk = chunks[i];
  const out = `cap${i}`;
  overlays.push(
    `[${prior}][${i + 3}:v]overlay=x='(W-w)/2':y='H-h-285':` +
    `enable='between(t,${chunk.from.toFixed(3)},${(chunk.to + 0.12).toFixed(3)})'[${out}]`,
  );
  prior = out;
}

overlays.push(
  `[1:a]aformat=sample_rates=48000:channel_layouts=stereo,apad[vo]`,
  `[2:a]aformat=sample_rates=48000:channel_layouts=stereo,volume=1.20,` +
  `afade=t=in:d=0.8,afade=t=out:st=${(total - 1.4).toFixed(3)}:d=1.4[score]`,
  `[vo][score]amix=inputs=2:duration=longest:normalize=0,` +
  `loudnorm=I=-16:TP=-1.5:LRA=11[aout]`,
);

finalArgs.push(
  "-filter_complex", overlays.join(";"),
  "-map", `[${prior}]`, "-map", "[aout]",
  "-c:v", "libx264", "-preset", "slow", "-crf", "17",
  "-c:a", "aac", "-b:a", "192k", "-ar", "48000", "-ac", "2",
  "-t", total.toFixed(3),
  "-movflags", "+faststart",
  OUT,
);
ff(finalArgs);

const meta = {
  title: "Aesthetic Wing",
  format: `${W}x${H}@${FPS}`,
  total,
  voEnd,
  slides,
  chunks,
  output: OUT,
  note: "Speculative concept reel. Original AC synth score and sound design. Human-carrying Phase I testing is not proposed.",
};
writeFileSync(join(BUILD, "meta.json"), JSON.stringify(meta, null, 2));
writeFileSync(join(HERE, "motion-struct.json"), JSON.stringify({
  sections: slides.map(({ name, from, to }) => ({ name, startSec: from, endSec: to })),
}, null, 2));

console.log(`✓ ${OUT}`);
console.log(`  ${total.toFixed(2)}s · ${slides.length} panels · ${chunks.length} captions · ${words.length} aligned words`);
