#!/usr/bin/env node
// build.mjs — assemble the 2FA Brush instagram story video.
//
// 1080x1920 @30fps, ~31s. Slides are the campaign gens (colored-pencil
// house style), ken-burns'd with zoompan; the last slide is page 1 of
// the product sheet on cream. Voiceover is jeffrey-pvc (vo.mp3) and
// every cut + caption is timed from the ElevenLabs alignment sidecar
// (vo.mp3.alignment.json) — word-accurate, no hand-tuned seconds.
//
// Captions are magick-prerendered YWFT Processing — ink over a sharp
// white drop shadow, no card — kept inside the IG story safe zone.
//
// Usage:  node build.mjs            (re-renders everything)
// Needs:  ../gens/{v3-hero,v5-coding,v6-notepat,v7-auth}.png
//         vo.mp3 + vo.mp3.alignment.json   (pop/bin/say.mjs --timestamps)
//         ../product/sheet.pdf (for the end card)

import { execFileSync } from "node:child_process";
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import * as progress from "../../../pop/lib/render-progress.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const CAMPAIGN = resolve(HERE, "..");
const REPO = resolve(HERE, "../../..");
const GENS = join(CAMPAIGN, "gens");
const TMP = join(HERE, "build");
mkdirSync(TMP, { recursive: true });

const FPS = 30;
const W = 1080, H = 1920;
const SHEET_PDF = join(REPO, "comodiddies/twofa/product/sheet.pdf");
// build.mjs assembles the chrome-less BASE; chrome.mjs (spawned at the end)
// draws the amaythingra side-stamp chrome over it → 2fa-brush-story.mp4.
const OUT = join(TMP, "base.mp4");

const ff = (args) =>
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", ...args], { stdio: ["ignore", "inherit", "inherit"] });

// ── Alignment → chunk timings ──────────────────────────────────────────
const align = JSON.parse(readFileSync(join(HERE, "vo.mp3.alignment.json"), "utf8"));
const words = align.words; // [{text, fromMs, toMs}]

// Caption chunks in order; wordCount slices the aligned word list.
// `slide` names which image is on screen when the chunk begins.
// No commas in caption text — YWFT Processing's comma is a 2px speck
// that reads as a stray period, so the lines are written around it.
const CHUNKS = [
  // vo says "toofah" (one word, rhymes with loofah — that's the bit);
  // captions print the product name, 2FA Brush
  { text: "this is the 2FA Brush.", n: 5, slide: "brush" },
  { text: "it's an electric toothbrush.", n: 4, slide: "brush" },
  { text: "it's also a hardware security key.", n: 6, slide: "brush" },
  { text: "you brush for two minutes every morning and that's the passphrase.", n: 11, slide: "brush" },
  { text: "then it plugs into your laptop bottom first like an oversized yubikey.", n: 12, slide: "auth" },
  { text: "i log in with it.", n: 5, slide: "auth" },
  { text: "i code with it.", n: 4, slide: "coding" },
  { text: "it knows my brushprint.", n: 4, slide: "brush-close" },
  { text: "nobody borrows your toothbrush.", n: 4, slide: "brush-close" },
  { text: "that's the whole security model.", n: 5, slide: "brush-close" },
  { text: "the 2FA Brush from aesthetic.computer.", n: 7, slide: "sheet" },
];

let wi = 0;
for (const c of CHUNKS) {
  c.from = words[wi].fromMs / 1000;
  c.to = words[wi + c.n - 1].toMs / 1000;
  wi += c.n;
}
if (wi !== words.length)
  throw new Error(`chunk word counts (${wi}) don't cover alignment (${words.length})`);

const VO_END = words[words.length - 1].toMs / 1000;
const TAIL = 1.4; // hold the end card after the last word
const TOTAL = VO_END + TAIL;

// ── Slides ─────────────────────────────────────────────────────────────
// Each slide runs from the first chunk that names it to the next slide's
// first chunk (the last runs to TOTAL).
const SLIDE_SRC = {
  brush: join(GENS, "v3-hero.png"),
  auth: join(GENS, "v7-auth.png"),
  coding: join(GENS, "v5-coding.png"),
  notepat: join(GENS, "v6-notepat.png"),
  "brush-close": join(GENS, "v3-hero.png"),
  sheet: join(TMP, "endcard.png"),
};

const slides = [];
for (const c of CHUNKS) {
  if (!slides.length || slides[slides.length - 1].name !== c.slide)
    slides.push({ name: c.slide, from: c.from });
}
slides.forEach((s, i) => (s.to = i + 1 < slides.length ? slides[i + 1].from : TOTAL));

// ── End card: sheet page 1 on cream ────────────────────────────────────
if (!existsSync(SHEET_PDF)) throw new Error(`missing ${SHEET_PDF}`);
execFileSync("pdftoppm", ["-png", "-r", "150", "-f", "1", "-l", "1", SHEET_PDF, join(TMP, "sheet-p")]);
const sheetPng = join(TMP, "sheet-p-1.png");
// scale to sit inside the 9:16 frame with cream margins (2x working res)
ff(["-i", sheetPng,
  "-vf", "scale=1900:-1,pad=2160:3840:(ow-iw)/2:(oh-ih)/2:0xFBFAFF",
  join(TMP, "endcard.png")]);

// ── Motion takes (Seedance via gen-motion.mjs) ─────────────────────────
// A slide with a generated motion clip in motion/ uses it instead of the
// zoompan still; takes.json picks (ClipWizard) win over
// the default shot file. The sheet never has one — typeset copy stays a
// still. Missing takes fall back to zoompan so the cut always completes.
const MOTION = join(HERE, "motion");
const TAKES = existsSync(join(MOTION, "takes.json"))
  ? JSON.parse(readFileSync(join(MOTION, "takes.json"), "utf8"))
  : {};
const motionClip = (name, i) => {
  const file = TAKES[name]
    ? join(MOTION, TAKES[name])
    : join(MOTION, `2fa-story-shot-${i}-${name}.mp4`);
  return existsSync(file) ? file : null;
};
// shots.json (the pipeline's inputs manifest) says which shots are
// MORPHS (endImage set) — those arrive on the next cut's panel in
// their final frames, so the excess trims from the HEAD, not the tail.
const SHOT_DEFS = existsSync(join(MOTION, "shots.json"))
  ? JSON.parse(readFileSync(join(MOTION, "shots.json"), "utf8"))
  : [];
const clipSeconds = (p) =>
  Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "default=noprint_wrappers=1:nokey=1", p]).toString().trim()) || 0;

// ── Render each slide segment with zoompan ─────────────────────────────
// Alternate in/out; the brush-close reuse starts tighter and lower so it
// reads as a different shot of the same drawing.
const MOVES = {
  brush: (n) => `z='1+0.10*on/${n}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2'`,
  auth: (n) => `z='1.10-0.10*on/${n}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2'`,
  coding: (n) => `z='1+0.10*on/${n}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)*0.35'`,
  notepat: (n) => `z='1.10-0.10*on/${n}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)/2'`,
  "brush-close": (n) => `z='1.22+0.08*on/${n}':x='(iw-iw/zoom)/2':y='(ih-ih/zoom)*0.72'`,
  // start tight on the sheet's masthead, then pull back while drifting
  // down the page — reads like the sheet being taken in, not a static card
  sheet: (n) => `z='1.45-0.35*on/${n}':x='(iw-iw/zoom)*0.30':y='(ih-ih/zoom)*(0.04+0.55*on/${n})'`,
};

// heartbeat → ~/.ac-pop-renders → Slab menubar bar + the launching Claude
// session's pink `rendering` state (chrome.mjs hands off to its own)
progress.begin({ type: "video", label: "2fa-brush story base" });
const segPaths = [];
for (const [i, s] of slides.entries()) {
  const dur = s.to - s.from;
  const frames = Math.round(dur * FPS);
  const seg = join(TMP, `seg-${i}-${s.name}.mp4`);
  segPaths.push(seg);
  const clip = motionClip(s.name, i);
  if (clip) {
    // Seedance clips run ceil(dur) at 720x1280/24fps — trim to the exact
    // slide length and upscale to frame. Cut shots open on their input
    // panel (keep the head); morph shots land on the next cut's panel
    // (keep the arrival — trim the head).
    const isMorph = !!SHOT_DEFS.find((d) => d.name === s.name)?.endImage;
    const head = isMorph ? Math.max(0, clipSeconds(clip) - dur) : 0;
    ff(["-i", clip,
      ...(head > 0.01 ? ["-ss", head.toFixed(3)] : []),
      "-vf", `scale=${W}:${H}:force_original_aspect_ratio=increase,crop=${W}:${H},fps=${FPS},format=yuv420p`,
      "-t", dur.toFixed(3), "-an",
      "-c:v", "libx264", "-preset", "fast", "-crf", "17", seg]);
    console.log(`✓ seg ${i} ${s.name} ${dur.toFixed(2)}s (motion${isMorph ? " morph" : ""})`);
  } else {
    // 2x working res keeps zoompan's subpixel walk from shimmering
    const pre = s.name === "sheet"
      ? "scale=2160:3840" // endcard is already composed at 2x
      : "scale=2560:3840:force_original_aspect_ratio=increase,crop=2160:3840";
    ff(["-i", SLIDE_SRC[s.name],
      "-vf", `${pre},zoompan=${MOVES[s.name](frames)}:d=${frames}:s=${W}x${H}:fps=${FPS},format=yuv420p`,
      "-frames:v", String(frames), "-c:v", "libx264", "-preset", "fast", "-crf", "17", seg]);
    console.log(`✓ seg ${i} ${s.name} ${dur.toFixed(2)}s (zoompan)`);
  }
  progress.update(((i + 1) / slides.length) * 80, { done: i + 1, total: slides.length });
}

// ── Captions: ink text over a sharp white drop shadow ──────────────────
// This ffmpeg build has no libass/drawtext, so captions render to PNG
// (YWFT draws correctly in ImageMagick — same trick as
// marketing/bin/compose-widescreen.mjs) and overlay with time windows.
// No card behind the text — a hard-offset white copy underneath keeps it
// legible on any slide.
const FONT = `${process.env.HOME}/Library/Fonts/ywft-processing-bold.ttf`;
const INK = "#1E1E1E", PAPER = "#FBFAFF";
const SHADOW = 7; // px, hard offset, no blur
const capPngs = [];
for (const [i, c] of CHUNKS.entries()) {
  const ink = join(TMP, `cap-${i}-ink.png`);
  const white = join(TMP, `cap-${i}-white.png`);
  const png = join(TMP, `cap-${i}.png`);
  const capFile = join(TMP, `cap-${i}.txt`);
  writeFileSync(capFile, c.text);   // proper punctuation, as written
  // rendered at 2x (with extra interline air) then halved — the downscale
  // is what makes the type read SHARP on the final frame
  for (const [fill, out] of [[INK, ink], [PAPER, white]])
    execFileSync("magick", ["-background", "none", "-fill", fill, "-font", FONT,
      "-pointsize", "112", "-interline-spacing", "26", "-size", "1840x",
      "-gravity", "Center", `caption:@${capFile}`, "-trim", "+repage", out]);
  const [tw2, th2] = execFileSync("magick", ["identify", "-format", "%w %h", ink])
    .toString().trim().split(" ").map(Number);
  const pw2 = tw2 + SHADOW * 2, ph2 = th2 + SHADOW * 2;
  execFileSync("magick", ["-size", `${pw2}x${ph2}`, "xc:none",
    white, "-geometry", `+${SHADOW * 2}+${SHADOW * 2}`, "-composite",
    ink, "-geometry", "+0+0", "-composite", "-resize", "50%", png]);
  capPngs.push({ png, w: Math.round(pw2 / 2), h: Math.round(ph2 / 2) });
}

// ── Side pals stamps — pals + "Twofa" up the frame edges ──────────────
// Same chrome as the marimbaba insta story (pop/marimba preview-score):
// a pals logo hugging the bottom-left and top-right edges with the
// product word rotated 90° beside it — here static, ink-tinted, with
// the captions' sharp white shadow so all the chrome reads as one set.
// ── Ambient bed: mombobasleep under the VO ─────────────────────────────
// The sine-bell ambient behind the narration is the actual mombobasleep
// master (pop/momboba) — its opening drift (drone + pad + reverso bells)
// ducked low. Render it first if missing:
//   node pop/momboba/bin/render-mombobasleep.mjs
const AMB = join(REPO, "pop/momboba/out/mombobasleep.mp3");
const AMB_SS = 10;          // skip the sneak-in, land in the settled drift
const AMB_VOL = 0.32;       // well under the voice
if (!existsSync(AMB))
  throw new Error(`missing ambient bed ${AMB} — render mombobasleep first`);

// ── Concat + captions + audio ──────────────────────────────────────────
const listPath = join(TMP, "concat.txt");
writeFileSync(listPath, segPaths.map((p) => `file '${p}'`).join("\n") + "\n");

// chained overlays, one per caption, gated by enable=between(t,…)
// bottom edge pinned at H-340 so varying card heights stay in safe zone
const inputs = ["-f", "concat", "-safe", "0", "-i", listPath, "-i", join(HERE, "vo.mp3")];
for (const c of capPngs) inputs.push("-i", c.png);
const ambIdx = 2 + capPngs.length;
inputs.push("-ss", String(AMB_SS), "-t", (TOTAL + 1).toFixed(2), "-i", AMB);
let chain = "";
let cur = "0:v";
CHUNKS.forEach((c, i) => {
  const end = Math.min(c.to + 0.25, i + 1 < CHUNKS.length ? CHUNKS[i + 1].from : TOTAL);
  const out = `v${i}`;
  chain += `[${cur}][${i + 2}:v]overlay=x=(W-w)/2:y=${H - 340}-h:enable='between(t,${c.from.toFixed(3)},${end.toFixed(3)})'[${out}];`;
  cur = out;
});
// no visual fades — hard in, hard out (jas, 2026-06-11)
chain += `[${cur}]null[vout];`;
// audio: VO clean (never faded); only the mombobasleep bed fades in/out
chain += `[1:a]apad[vo];` +
  `[${ambIdx}:a]volume=${AMB_VOL},afade=t=in:d=1.5,` +
  `afade=t=out:st=${(TOTAL - 1.6).toFixed(2)}:d=1.6[amb];` +
  `[vo][amb]amix=inputs=2:duration=longest:normalize=0[aout]`;
ff([...inputs,
  "-filter_complex", chain,
  "-map", "[vout]", "-map", "[aout]",
  "-t", TOTAL.toFixed(3),
  "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k", "-movflags", "+faststart",
  OUT]);

console.log(`✓ base ${OUT} (${TOTAL.toFixed(1)}s)`);
progress.end();

// slide timings for the chrome pass's per-slide tint, then the chrome
// itself (per-char Twofa columns + pals seep — see chrome.mjs; it runs
// its own heartbeat)
writeFileSync(join(TMP, "meta.json"), JSON.stringify({ total: TOTAL, slides }, null, 2));
execFileSync("node", [join(HERE, "chrome.mjs")], { stdio: ["ignore", "inherit", "inherit"] });
