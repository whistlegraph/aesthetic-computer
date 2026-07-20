#!/usr/bin/env node
// sim-announce.mjs — the "Menu Band is on the Mac App Store NOW" reel base.
//
// Three acts over the announce fanfare (render-jingles.mjs):
//   1 · the REAL captured menu-bar piano falls in front-and-center and plays
//       the fanfare (strip rig — real lit-key pixels), headline under it;
//   2 · the REAL About window floats up while the band climbs to the top and
//       rains notes over it;
//   3 · the end card — drawn app icon (AboutWindow geometry, keys pulsing
//       with the music), "now on the Mac App Store", the real scannable QR,
//       menuband.app.
//
// Output: out/base-menuband-announce.mp4 + meta-menuband-announce.json —
// then `node pop/menuband/bin/chrome-reel.mjs menuband-announce` stamps the
// pals chrome.
//
// Usage: node pop/menuband/bin/sim-announce.mjs   (render-jingles.mjs first)

import { loadImage } from "canvas";
import {
  W, H, FPS, OUT, INK, easeOut, clamp01, rgb, KEY_COLORS,
  makeStage, roundRect, text, drawDesktop, vignette, drawIcon,
  makeParticles, loadStripRig, drawStrip, stripKeyX, stripKeyColor, foldToStrip,
  drawFramedWindow, loadScore, leadOf, litAt, makeOnsets,
  renderVideo, writeMeta, makeScenes, sungMode, loadSungWords, makeKaraoke,
} from "./reel-lib.mjs";

const SLUG = "menuband-announce";
// --sung: jeffrey's sung vocal mix + karaoke captions (sing-jingle.mjs),
// writing the -sung base/meta so the originals stay untouched.
const { sung: SUNG, audioSuffix: AUDIO_VAR, suffix: VAR } = sungMode();
const karaoke = SUNG ? makeKaraoke(loadSungWords(SLUG)) : null;
const score = loadScore(SLUG);
const TOTAL = score.durationSec;
const BAR_SEC = score.barSec || 1.3;
const lead = leadOf(score);
const onsetsBetween = makeOnsets(lead);

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const aboutImg = await loadImage(`${OUT}/about-frames/about-en.png`);
const qrImg = await loadImage(`${OUT}/qr-menuband.png`);
const particles = makeParticles(ctx);

const { scenes: SCENES, sceneAt } = makeScenes([
  { name: "menu", from: 0.00, to: 0.40, tint: [97, 158, 255] },
  { name: "about", from: 0.40, to: 0.66, tint: [167, 139, 250] },
  { name: "end", from: 0.66, to: 1.00, tint: [255, 77, 107] },
], TOTAL);

// ── hero strip choreography (sim.mjs heroRect, retuned) ────────────────────
const HERO_W = W * 0.96, HERO_X = (W - HERO_W) / 2;
const HERO_BOB = 22, HERO_DROP = 1.2, HERO_TOP = 54, HERO_RISE = 1.1;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const middle = H * 0.40 - h / 2;
  const riseEnd = SCENES[0].to;
  const rise = easeOut((t - (riseEnd - HERO_RISE)) / HERO_RISE);
  const rest = middle + (HERO_TOP - middle) * rise;
  const enter = easeOut(clamp01(t / HERO_DROP));
  const bob = Math.sin((t / BAR_SEC) * Math.PI * 2) * HERO_BOB * (1 - rise * 0.6);
  const y = (-h - 40) + (rest - (-h - 40)) * enter + bob * enter;
  return { x: HERO_X, y, w: HERO_W, h, risen: rise > 0.5 };
}

// ── the end card ───────────────────────────────────────────────────────────
function drawEndCard(t, local, e) {
  const cw = W * 0.82, chh = H * 0.62;
  const cx = (W - cw) / 2, cy = H * 0.52 - chh / 2 + (1 - e) * H * 0.5;
  ctx.save(); ctx.globalAlpha = e;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ctx, cx, cy, cw, chh, 40); ctx.fillStyle = "rgba(250,249,253,0.99)"; ctx.fill();
  ctx.shadowColor = "transparent";

  // drawn app icon, its 5 keys pulsing with the lit melody (pc % 5 fold —
  // the same fold sim.mjs used on this icon)
  const litIcon = new Set(litAt(lead, t).map((mm) => ((mm % 12) + 12) % 12 % 5));
  const ipx = 340;
  drawIcon(ctx, W / 2 - ipx / 2, cy + 44, ipx, litIcon);

  text(ctx, "Menu Band", W / 2, cy + 460, 96, INK, 800);
  text(ctx, "now on the Mac App Store", W / 2, cy + 552, 46, "rgba(60,50,80,0.9)", 700);

  // the real scannable QR on a white tile
  const q = 300, qx = W / 2 - q / 2, qy = cy + 610;
  roundRect(ctx, qx - 18, qy - 18, q + 36, q + 36, 22);
  ctx.fillStyle = "white"; ctx.fill();
  ctx.lineWidth = 2; ctx.strokeStyle = "rgba(20,18,28,0.14)"; ctx.stroke();
  ctx.imageSmoothingEnabled = false;
  ctx.drawImage(qrImg, qx, qy, q, q);
  ctx.imageSmoothingEnabled = true;

  text(ctx, "menuband.app  ·  free", W / 2, qy + q + 74, 44, "rgba(60,50,80,0.85)", 700);
  ctx.restore();
}

function drawFrame(t) {
  drawDesktop(ctx);
  const sc = sceneAt(t);
  const local = (sc.to - sc.from) > 0 ? (t - sc.from) / (sc.to - sc.from) : 1;
  const dt = 1 / FPS;

  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(lead, t), hero.x, hero.y, hero.w);
  const edge = hero.risen ? hRect.y + hRect.h + 6 : hRect.y - 6;
  for (const n of onsetsBetween(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), edge, stripKeyColor(rig, n.midi), hero.risen);
  }

  if (sc.name === "menu") {
    // headline under the band, fading in once it has landed
    const a = easeOut(clamp01((t - 1.3) / 0.7));
    if (a > 0) {
      ctx.save(); ctx.globalAlpha = a;
      text(ctx, "Menu Band", W / 2, H * 0.60, 128, INK, 800);
      text(ctx, "your menu bar is a synthesizer", W / 2, H * 0.60 + 116, 46, "rgba(60,50,80,0.88)", 600);
      text(ctx, "out NOW on the Mac App Store", W / 2, H * 0.60 + 196, 52, INK, 800);
      ctx.restore();
    }
  } else if (sc.name === "about") {
    const e = easeOut(Math.min(1, local * 5));
    drawFramedWindow(ctx, aboutImg, { alpha: e, yOff: (H * 0.66) * (1 - e), cyFrac: 0.54, contentH: Math.min(H * 0.60, 1140) });
    const a = easeOut(clamp01((local - 0.25) / 0.3));
    if (a > 0 && !SUNG) {          // sung mode: the karaoke line owns this band
      ctx.save(); ctx.globalAlpha = a;
      text(ctx, "for macOS · free", W / 2, H * 0.925, 46, INK, 700);
      ctx.restore();
    }
  } else {
    const e = easeOut(Math.min(1, local * 5));
    drawEndCard(t, local, e);
  }

  particles.stepAndDraw(dt);
  vignette(ctx);
  karaoke?.draw(ctx, t);
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}${AUDIO_VAR}.mp3`, outPath: `${OUT}/base-${SLUG}${VAR}.mp4`,
  total: TOTAL, drawFrame, label: `menuband announce sim${VAR}`,
});
writeMeta(`${SLUG}${VAR}`, TOTAL, SCENES);
