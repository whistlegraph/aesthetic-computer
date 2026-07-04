#!/usr/bin/env node
// video.mjs — caption-only reading video.
//
// No cover, no chrome — just the words, centered on paper, appearing in sync
// with the reading. The whole video is the text. (This ffmpeg has no
// libass/drawtext, so frames are baked with ImageMagick and played as a
// slideshow synced to produce.mjs's exact-timeline SRT.)
//
// Usage: node bin/video.mjs <slug>   (reads out/<slug>.{mp3,srt,json})

import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { existsSync, readFileSync, writeFileSync, mkdirSync, rmSync } from "node:fs";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const REPO = resolve(ROOT, "..", "..");
const slug = process.argv[2];
if (!slug) { console.error("usage: video.mjs <slug>"); process.exit(1); }

const mp3 = resolve(OUT, `${slug}.mp3`);
const srt = resolve(OUT, `${slug}.srt`);
for (const f of [mp3, srt]) if (!existsSync(f)) { console.error(`✗ missing ${f}`); process.exit(1); }
const meta = existsSync(resolve(OUT, `${slug}.json`)) ? JSON.parse(readFileSync(resolve(OUT, `${slug}.json`), "utf8")) : {};
const title = meta.title || slug;

const CAP_FONT = ["/System/Library/Fonts/HelveticaNeue.ttc", "/System/Library/Fonts/Helvetica.ttc"].find(existsSync) || "Helvetica";
const TITLE_FONT = resolve(REPO, "system/public/type/webfonts/ywft-processing-light.ttf");

// Per-episode colour theme (bg / text / title accent). Each reading gets its
// own look; override any with --bg / --fg / --title.
const THEMES = {
  "may-26":  { bg: "#FFF9FC", fg: "#1e1e28", title: "#b44887" }, // paper · ink · pink
  "june-26": { bg: "#101822", fg: "#EAF0F2", title: "#8CE99A" }, // night · mist · mint
};
const argFlag = (k) => { const i = process.argv.indexOf(`--${k}`); return i >= 0 ? process.argv[i + 1] : null; };
const theme = THEMES[slug] || { bg: "#FFF9FC", fg: "#1e1e28", title: "#b44887" };
const PAPER = argFlag("bg") || theme.bg;
const INK = argFlag("fg") || theme.fg;
const PINK = argFlag("title") || theme.title;

const dur = (f) => Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", f]).toString().trim()) || 0;

// ── parse SRT → cues ───────────────────────────────────────────────────
const t2s = (t) => { const [h, m, r] = t.split(":"); const [s, ms] = r.split(","); return +h * 3600 + +m * 60 + +s + +ms / 1000; };
const cues = readFileSync(srt, "utf8").trim().split(/\n\s*\n/).map((block) => {
  const lines = block.split("\n");
  const tc = lines[1].split(" --> ");
  return { start: t2s(tc[0]), end: t2s(tc[1]), text: lines.slice(2).join(" ").trim() };
});

const build = resolve(OUT, `.video-${slug}`);
rmSync(build, { recursive: true, force: true });
mkdirSync(build, { recursive: true });

// ── blank paper + title card (title held during the intro) ─────────────
const blank = resolve(build, "blank.png");
execFileSync("magick", ["-size", "1920x1080", `xc:${PAPER}`, blank]);

const titleFrame = resolve(build, "title.png");
execFileSync("magick", [
  "-size", "1920x1080", `xc:${PAPER}`,
  "(", "-background", "none", "-fill", PINK,
  "-font", existsSync(TITLE_FONT) ? TITLE_FONT : CAP_FONT, "-pointsize", "96",
  "-size", "1500x", "-gravity", "center", `caption:${title}`, ")",
  "-gravity", "center", "-geometry", "+0+0", "-composite",
  titleFrame,
]);

// ── one centered frame per cue ─────────────────────────────────────────
console.log(`▸ baking ${cues.length} caption frames…`);
cues.forEach((c, i) => {
  c.frame = resolve(build, `f${String(i).padStart(3, "0")}.png`);
  execFileSync("magick", [
    "-size", "1920x1080", `xc:${PAPER}`,
    "(", "-background", "none", "-fill", INK, "-font", CAP_FONT, "-pointsize", "62",
    "-interline-spacing", "8", "-size", "1480x", "-gravity", "center", `caption:${c.text}`, ")",
    "-gravity", "center", "-geometry", "+0+0", "-composite",
    c.frame,
  ]);
});

// ── segments: title over the intro, then each caption held until the next ──
// The concat *demuxer* silently drops still-image durations, so we encode each
// segment to its own clip and concat the clips (reliable, byte-copy).
const total = dur(mp3);
const segs = [];
if (cues.length) {
  segs.push({ frame: titleFrame, dur: cues[0].start });
  for (let i = 0; i < cues.length; i++) {
    const end = i + 1 < cues.length ? cues[i + 1].start : total; // linger until next
    segs.push({ frame: cues[i].frame, dur: Math.max(0.1, end - cues[i].start) });
  }
} else {
  segs.push({ frame: blank, dur: total });
}

console.log(`▸ encoding ${segs.length} segments…`);
const clips = segs.map((s, i) => {
  const clip = resolve(build, `seg${String(i).padStart(3, "0")}.mp4`);
  execFileSync("ffmpeg", [
    "-y", "-loop", "1", "-i", s.frame, "-t", s.dur.toFixed(3),
    "-r", "10", "-pix_fmt", "yuv420p",
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "20", "-g", "20",
    clip,
  ], { stdio: "ignore" });
  return clip;
});
const vlist = resolve(build, "vlist.txt");
writeFileSync(vlist, clips.map((c) => `file '${c}'`).join("\n") + "\n");
const silent = resolve(build, "silent.mp4");
execFileSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0", "-i", vlist, "-c", "copy", silent], { stdio: "ignore" });

mkdirSync(resolve(OUT, "youtube"), { recursive: true });
const outMp4 = resolve(OUT, "youtube", `${slug}.mp4`);
console.log(`▸ muxing audio…`);
execFileSync("ffmpeg", [
  "-y", "-i", silent, "-i", mp3, "-map", "0:v", "-map", "1:a",
  "-c:v", "copy", "-c:a", "aac", "-b:a", "256k", "-shortest",
  outMp4,
], { stdio: "ignore" });

rmSync(build, { recursive: true, force: true });
console.log(`✓ ${outMp4}`);
