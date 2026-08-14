#!/usr/bin/env node
// maytrax/bin/motion-score-femrag-reel.mjs — the SHAKEOUT reel FINISHER.
// Forked from the marimbaba/trancenwaltz motion-score pattern
// (pop/marimba/bin/motion-score-marimbaba-yt.mjs): the assembled Seedance
// cut is the live background, piped in frame by frame, and everything else
// is drawn on top in ONE canvas pass — so the pulsing sits BETWEEN the
// separation layers instead of being a flat post-grade:
//
//   frame → global ken-burns + beat-punch transform (camera)
//         → layer 1  bloom rising from behind the figure   [screen]
//         → layer 2  breathing contrast vignette           [multiply]
//         → layer 3  rim halo on snare/donk                [screen]
//         → side PALS watermarks (trancenwaltz treatment: tinted to the
//           section colour + 4-cycle hue spin, 4-pass multiply/burn/
//           overlay seep, LED pulse from the audio envelope, looping
//           wiggle + swivel, rotated 90° hugging the edges)
//         → encoder (unsharp for the felt fibre, audio muxed, no fades)
//
// Every envelope is read from out/<slug>.events.json — the same event list
// the audio was rendered from, so nothing drifts and nothing is detected.
//
//   node pop/maytrax/bin/motion-score-femrag-reel.mjs
//   node pop/maytrax/bin/motion-score-femrag-reel.mjs --in <cut.mp4> --open

import { existsSync, readFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawn, spawnSync } from "node:child_process";
import { createCanvas, loadImage, ImageData } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

const IN = typeof flags.in === "string" ? flags.in : `${OUT}/${SLUG}-shakeout-reel.mp4`;
const AUDIO = `${OUT}/reel/${SLUG}-reel.mp3`;
const FINAL = typeof flags.out === "string" ? flags.out : `${OUT}/${SLUG}-shakeout-reel-final.mp4`;
if (!existsSync(IN)) { console.error(`✗ no cut at ${IN}`); process.exit(1); }

const W = 720, H = 1280, FPS = 24;
const probe = spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration",
  "-of", "default=nw=1:nk=1", IN], { encoding: "utf8" });
const DUR = Number(probe.stdout.trim());
const FRAMES = Math.round(DUR * FPS);
const TAU = Math.PI * 2;

// ── the score — envelopes from the renderer's own event list ─────────────
const feed = JSON.parse(readFileSync(`${OUT}/${SLUG}.events.json`, "utf8"));
const BPM = feed.bpm || 144;
const FROM = Number(flags.from ?? 26.67);      // reel slice offset into the track
const DECAY = { boom: .20, snare: .16, donk: .13, sub: .30, hat: .07, bell: .09, riser: .5 };
// Per-frame envelopes, split by which layer they drive.
const envBehind = new Float32Array(FRAMES);    // kick + sub + riser → bloom
const envRim = new Float32Array(FRAMES);       // snare + donk → halo
const envAll = new Float32Array(FRAMES);       // everything → pals LED pulse
for (const e of feed.events) {
  const d = DECAY[e.i]; if (!d) continue;
  const span = e.i === "riser" ? (e.dur || 0) : 0;
  const g = (e.gain ?? .1);
  const f0 = Math.max(0, Math.floor((e.t - FROM) * FPS));
  const f1 = Math.min(FRAMES, Math.ceil((e.t - FROM + span + d * 3) * FPS));
  for (let f = f0; f < f1; f++) {
    const t = f / FPS + FROM;
    const env = Math.exp(-Math.max(0, t - e.t - span) / d) * g;
    envAll[f] += env * 2.4;
    if (e.i === "boom" || e.i === "sub" || e.i === "riser") envBehind[f] += env * 3.4;
    if (e.i === "snare" || e.i === "donk") envRim[f] += env * 3.4;
  }
}

// Section tints for the pals + bloom colour, read from the reel struct.
const struct = JSON.parse(readFileSync(`${OUT}/reel/${SLUG}-reel.struct.json`, "utf8"));
const SECTION_RGB = {
  find: [70, 90, 140], thread: [90, 80, 150], coil: [140, 70, 130],
  ignite: [255, 240, 200], hammer: [255, 70, 190], run: [255, 150, 40],
  skank: [120, 220, 90], orbit: [90, 200, 255],
};
function sectionTcRgb(t) {
  let cur = struct.sections[0];
  for (const s of struct.sections) if (t >= s.startSec) cur = s;
  return SECTION_RGB[cur.name] || [200, 200, 200];
}

function hslToRgb(h, s, l) {
  const c = (1 - Math.abs(2 * l - 1)) * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; } else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; } else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; } else { r = c; b = x; }
  return [Math.round((r + m) * 255), Math.round((g + m) * 255), Math.round((b + m) * 255)];
}

// ── pals stamp — rasterize purple-pals.svg, white-key the png fallback ───
const assetsDir = `${OUT}/reel/assets`;
mkdirSync(assetsDir, { recursive: true });
const PALS_WM_SIZE = 212;
let palsImg = null;
{
  const svg = `${REPO}/system/public/purple-pals.svg`;
  const png = `${assetsDir}/pals-watermark.png`;
  if (existsSync(svg)) {
    const r = spawnSync("rsvg-convert", ["-w", String(PALS_WM_SIZE * 2), "-h", String(PALS_WM_SIZE * 2), "-o", png, svg]);
    if (r.status === 0 && existsSync(png)) palsImg = await loadImage(png);
  }
  if (!palsImg) {
    // fallback: the raster logo ships on WHITE — key it to alpha here.
    const raster = `${REPO}/pop/wattajetta/assets/pals-logo.png`;
    if (existsSync(raster)) {
      const src = await loadImage(raster);
      const c = createCanvas(src.width, src.height), cx = c.getContext("2d");
      cx.drawImage(src, 0, 0);
      const d = cx.getImageData(0, 0, c.width, c.height);
      for (let i = 0; i < d.data.length; i += 4) {
        const [r, g, b] = [d.data[i], d.data[i + 1], d.data[i + 2]];
        const whiteness = Math.min(r, g, b) / 255;
        if (whiteness > .82) d.data[i + 3] = Math.round((1 - (whiteness - .82) / .18) * 255);
      }
      cx.putImageData(d, 0, 0);
      palsImg = c;
    }
  }
  console.log(`  pals stamp: ${palsImg ? "ready" : "MISSING (skipped)"}`);
}
const wmCanvas = createCanvas(8, 8);
const wmCtx = wmCanvas.getContext("2d");
function palsTinted(rgb, src) {
  wmCanvas.width = src.width; wmCanvas.height = src.height;
  wmCtx.globalCompositeOperation = "source-over";
  wmCtx.clearRect(0, 0, src.width, src.height);
  wmCtx.drawImage(src, 0, 0);
  wmCtx.globalCompositeOperation = "source-in";
  wmCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
  wmCtx.fillRect(0, 0, src.width, src.height);
  wmCtx.globalCompositeOperation = "source-over";
  return wmCanvas;
}

// trancenwaltz side stamps: rotated 90°, hugging the edges, wiggle+swivel
// locked to the video timeline, tinted, 4-pass seep, LED pulse.
function drawWatermark(ctx, fi) {
  if (!palsImg) return;
  const t = fi / FPS;
  const u = fi / FRAMES;
  const s = 122;
  const hue = ((u * 360 * 4) % 360 + 360) % 360;
  const hRgb = hslToRgb(hue, .9, .62);
  const [sr, sg, sb] = sectionTcRgb(t);
  const col = [
    Math.round(sr * .6 + hRgb[0] * .4),
    Math.round(sg * .6 + hRgb[1] * .4),
    Math.round(sb * .6 + hRgb[2] * .4),
  ];
  const env = Math.min(1, envAll[fi] ?? 0);
  const glow = env * env;
  const hotRgb = hslToRgb(hue, 1, Math.min(.88, .6 + .34 * glow));
  const ledCol = [
    Math.round(col[0] + (hotRgb[0] - col[0]) * glow),
    Math.round(col[1] + (hotRgb[1] - col[1]) * glow),
    Math.round(col[2] + (hotRgb[2] - col[2]) * glow),
  ];
  const wig = 10 * Math.sin(TAU * 30 * u) + 3 * Math.sin(TAU * 10 * u);
  const swiv = .05 * Math.sin(TAU * 19 * u) + .025 * Math.sin(TAU * 38 * u);
  const spots = [
    { cx: 54 - wig, cy: H * .78, rot: Math.PI / 2 + swiv },
    { cx: W - 54 + wig, cy: H * .22, rot: -Math.PI / 2 - swiv },
  ];
  const passes = [["multiply", .78], ["color-burn", .42], ["overlay", .58], ["source-over", .06]];
  for (const sp of spots) {
    ctx.save();
    ctx.translate(sp.cx, sp.cy);
    ctx.rotate(sp.rot);
    // tight dark drop shadow
    palsTinted([0, 0, 0], palsImg);
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = .26;
    ctx.drawImage(wmCanvas, -s / 2 + 3, -s / 2 + 4, s, s);
    // 4-pass seep, stained into the picture
    palsTinted(col, palsImg);
    for (const [op, a] of passes) {
      ctx.globalCompositeOperation = op;
      ctx.globalAlpha = a;
      ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    }
    // LED pulse on top
    palsTinted(ledCol, palsImg);
    ctx.globalCompositeOperation = "screen";
    ctx.globalAlpha = .30 + .5 * glow;
    ctx.drawImage(wmCanvas, -s / 2, -s / 2, s, s);
    ctx.restore();
  }
  ctx.globalCompositeOperation = "source-over";
  ctx.globalAlpha = 1;
}

// ── canvas + gradient stamps (built once, drawn scaled) ──────────────────
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const bgCanvas = createCanvas(W, H);
const bgCtx = bgCanvas.getContext("2d");

// bloom (layer 1) — radial stamp, tinted per frame via section colour
const bloomC = createCanvas(256, 256);
{
  const g = bloomC.getContext("2d").createRadialGradient(128, 134, 0, 128, 134, 128);
  g.addColorStop(0, "rgba(255,255,255,1)");
  g.addColorStop(.55, "rgba(255,255,255,.45)");
  g.addColorStop(1, "rgba(255,255,255,0)");
  const c = bloomC.getContext("2d"); c.fillStyle = g; c.fillRect(0, 0, 256, 256);
}
// STAINED GLASS (the marimbaba law): the frame is a glass panel. Each
// pixel's LUMINANCE sets how much backlight passes through — bright wool
// transmits and glows; dark rafters and shadow act as leaded came and
// block it. The glow is stamped into an offscreen, GATED by the frame's
// transmission mask, then composited additively — so the light reads as
// genuinely behind the picture, never as a ring sitting on top.
const glowCanvas = createCanvas(W, H);
const glowCtx = glowCanvas.getContext("2d");
const maskCanvas = createCanvas(W, H);
const maskCtx = maskCanvas.getContext("2d");
const maskData = maskCtx.createImageData(W, H);
{ const d = maskData.data; for (let i = 0; i < d.length; i += 4) { d[i] = d[i+1] = d[i+2] = 255; } }
const T_LO = .30, T_HI = .92, T_GAMMA = 1.7, T_FLOOR = .04, T_CAP = .65;
const tintC = createCanvas(256, 256);
const tintCtx = tintC.getContext("2d");
function tinted(stamp, rgb) {
  tintCtx.globalCompositeOperation = "source-over";
  tintCtx.clearRect(0, 0, 256, 256);
  tintCtx.drawImage(stamp, 0, 0);
  tintCtx.globalCompositeOperation = "source-in";
  tintCtx.fillStyle = `rgb(${rgb[0]},${rgb[1]},${rgb[2]})`;
  tintCtx.fillRect(0, 0, 256, 256);
  return tintC;
}

// ── the render loop — decode pipe → canvas → encode pipe ─────────────────
console.log(`▸ finishing ${IN.split("/").pop()} · ${FRAMES}f · ${DUR.toFixed(1)}s`);
const dec = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error",
  "-i", IN, "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${W}x${H}`, "-r", String(FPS), "pipe:1"],
  { stdio: ["ignore", "pipe", "inherit"] });
const enc = spawn("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "rgba", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "pipe:0",
  "-i", AUDIO,
  "-map", "0:v", "-map", "1:a",
  // sharpen at the very end so the felt fibre survives the glow
  "-vf", "unsharp=5:5:0.9:3:3:0.35",
  "-c:v", "libx264", "-preset", "medium", "-crf", "17", "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "256k", "-shortest", "-movflags", "+faststart", FINAL],
  { stdio: ["pipe", "inherit", "inherit"] });
const encClosed = new Promise((r) => enc.on("close", r));

const frameBytes = W * H * 4;
let leftover = Buffer.alloc(0);
let fi = 0;
const beatsPerSec = BPM / 60;

async function processFrame(raw) {
  bgCtx.putImageData(new ImageData(new Uint8ClampedArray(raw.buffer, raw.byteOffset, frameBytes), W, H), 0, 0);
  const t = fi / FPS;
  const u = fi / FRAMES;

  // CAMERA — slow global ken burns push + a percussive punch on the beat.
  const punch = Math.pow(Math.max(0, Math.cos(TAU * t * beatsPerSec)), 8) * .022;
  const z = 1 + .06 * u + punch * Math.min(1, envAll[fi] ?? 0);
  ctx.save();
  ctx.translate(W / 2, H / 2);
  ctx.scale(z, z);
  ctx.drawImage(bgCanvas, -W / 2, -H / 2);
  ctx.restore();

  const [sr, sg, sb] = sectionTcRgb(t);

  // LAYER 1 — warm near-black contrast vignette [multiply], marimbaba
  // style: a tight pool of light around the figure, deeply dark corners
  // tinted with the section colour so the surround reads as coloured
  // shadow, never as a grey blur.
  const squeeze = Math.min(1, (envBehind[fi] ?? 0) * .6 + (envRim[fi] ?? 0) * .4);
  ctx.globalCompositeOperation = "multiply";
  ctx.globalAlpha = 1;
  const vg = ctx.createRadialGradient(W / 2, H * .52, H * (.16 - .03 * squeeze), W / 2, H * .52, H * (.74 - .06 * squeeze));
  vg.addColorStop(0, "rgba(255,255,255,1)");
  vg.addColorStop(.5, `rgba(${Math.round(120 + sr * .25)},${Math.round(112 + sg * .25)},${Math.round(118 + sb * .25)},${(.5 + .1 * squeeze).toFixed(3)})`);
  vg.addColorStop(1, `rgba(${Math.round(sr * .12)},${Math.round(sg * .12)},${Math.round(sb * .14)},.93)`);
  ctx.fillStyle = vg;
  ctx.fillRect(0, 0, W, H);
  ctx.globalCompositeOperation = "source-over";

  // LAYER 2 — TRANSMITTED backlight (stained glass). Build the frame's
  // transmission mask from its own luminance, stamp the coloured glow,
  // gate it by the mask, add it. Kick/sub swell the glow; snare/donk
  // snap a hotter, hue-spun pulse through the same glass.
  const behind = Math.min(1.4, envBehind[fi] ?? 0);
  const rim = Math.min(1.2, envRim[fi] ?? 0);
  if (behind > .02 || rim > .02) {
    const img = ctx.getImageData(0, 0, W, H);
    const px = img.data, md = maskData.data;
    for (let i = 0; i < px.length; i += 4) {
      const L = (.2126 * px[i] + .7152 * px[i + 1] + .0722 * px[i + 2]) / 255;
      let sN = (L - T_LO) / (T_HI - T_LO);
      sN = sN < 0 ? 0 : sN > 1 ? 1 : sN;
      sN = sN * sN * (3 - 2 * sN);
      md[i + 3] = (Math.min(T_CAP, T_FLOOR + (1 - T_FLOOR) * Math.pow(sN, T_GAMMA)) * 255) | 0;
    }
    maskCtx.putImageData(maskData, 0, 0);
    // candle dance — the transmitted light flutters where it falls
    const jx = 3.5 * Math.sin(t * 17.3) + 1.5 * Math.sin(t * 41);
    const jy = 3.5 * Math.cos(t * 21.7) + 1.5 * Math.cos(t * 37);
    glowCtx.globalCompositeOperation = "source-over";
    glowCtx.clearRect(0, 0, W, H);
    if (behind > .02) {
      glowCtx.globalAlpha = Math.min(.85, behind * .55);
      const sB = H * (1.05 + .3 * behind);
      glowCtx.drawImage(tinted(bloomC, [sr, sg, sb]), W / 2 - sB / 2 + jx, H * .56 - sB / 2 + jy, sB, sB);
    }
    if (rim > .02) {
      const hue = ((u * 360 * 4) % 360 + 360) % 360;
      glowCtx.globalAlpha = Math.min(.7, rim * .5);
      const sR = H * (.7 + .2 * rim);
      glowCtx.drawImage(tinted(bloomC, hslToRgb(hue, .85, .62)), W / 2 - sR / 2 - jx, H * .5 - sR / 2 - jy, sR, sR);
    }
    glowCtx.globalAlpha = 1;
    glowCtx.globalCompositeOperation = "destination-in";
    glowCtx.drawImage(maskCanvas, 0, 0);
    ctx.globalCompositeOperation = "lighter";
    ctx.drawImage(glowCanvas, 0, 0);
    ctx.globalCompositeOperation = "source-over";
  }
  ctx.globalAlpha = 1;

  drawWatermark(ctx, fi);

  const out = ctx.getImageData(0, 0, W, H);
  if (!enc.stdin.write(Buffer.from(out.data.buffer, 0, frameBytes))) {
    await new Promise((r) => enc.stdin.once("drain", r));
  }
  fi++;
  if (fi % 120 === 0) process.stdout.write(`\r  ${Math.round(fi / FRAMES * 100)}%  `);
}

for await (const chunk of dec.stdout) {
  leftover = leftover.length ? Buffer.concat([leftover, chunk]) : chunk;
  while (leftover.length >= frameBytes) {
    await processFrame(leftover.subarray(0, frameBytes));
    leftover = leftover.subarray(frameBytes);
  }
}
enc.stdin.end();
const code = await encClosed;
process.stdout.write("\r");
if (code !== 0) { console.error("✗ encode failed"); process.exit(1); }
console.log(`✓ ${FINAL} · ${fi} frames`);
if (flags.open) spawnSync("open", [FINAL]);
