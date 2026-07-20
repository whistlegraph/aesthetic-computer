#!/usr/bin/env node
// sim-features.mjs — the Menu Band FEATURE TOUR reel base.
//
// Three acts over the tour groove (render-jingles.mjs):
//   1 · the REAL popover drops from the band's ♪ status glyph — the popover
//       piano, instrument readout and full GM grid, straight out of the app
//       (PopoverCapture), captions cycling the feature beats;
//   2 · the REAL fullscreen keymap slides up and cycles all five instrument
//       families (the cached --render-keymap captures: piano → guitar →
//       violin → trumpet → flute), LED scope + big piano + QWERTY map;
//   3 · end tag — the strip rains notes over "menuband.app · free on the
//       Mac App Store".
//
// Output: out/base-menuband-features.mp4 + meta-menuband-features.json —
// then `node pop/menuband/bin/chrome-reel.mjs menuband-features`.
//
// Usage: node pop/menuband/bin/sim-features.mjs  (render-jingles.mjs first)

import { loadImage } from "canvas";
import {
  W, H, FPS, OUT, INK, easeOut, clamp01,
  makeStage, roundRect, text, drawDesktop, vignette, drawIcon,
  makeParticles, loadStripRig, drawStrip, stripKeyX, stripKeyColor,
  loadScore, leadOf, litAt, makeOnsets,
  renderVideo, writeMeta, makeScenes, sungMode, loadSungWords, makeKaraoke,
} from "./reel-lib.mjs";

const SLUG = "menuband-features";
// --sung: jeffrey's sung vocal mix + karaoke captions (sing-jingle.mjs).
const { sung: SUNG, audioSuffix: AUDIO_VAR, suffix: VAR } = sungMode();
const karaoke = SUNG ? makeKaraoke(loadSungWords(SLUG)) : null;
const score = loadScore(SLUG);
const TOTAL = score.durationSec;
const BAR_SEC = score.barSec || 2.07;
const lead = leadOf(score);
const onsetsBetween = makeOnsets(lead);

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const popoverImg = await loadImage(`${OUT}/popover-frames/popover-0.png`);
const KEYMAP_PROGRAMS = [0, 24, 40, 56, 73];   // piano guitar violin trumpet flute
const keymapImgs = [];
for (const p of KEYMAP_PROGRAMS) keymapImgs.push(await loadImage(`${OUT}/about-frames/keymap-${p}.png`));
const particles = makeParticles(ctx);

const { scenes: SCENES, sceneAt } = makeScenes([
  { name: "popover", from: 0.00, to: 0.42, tint: [97, 158, 255] },
  { name: "keymap", from: 0.42, to: 0.85, tint: [163, 230, 53] },
  { name: "end", from: 0.85, to: 1.00, tint: [255, 77, 107] },
], TOTAL);

// The band parks at the very top for the whole reel (it's the anchor the
// popover hangs from) — it just falls in during the first bar.
const HERO_W = W * 0.96, HERO_X = (W - HERO_W) / 2, HERO_TOP = 54;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const enter = easeOut(clamp01(t / 1.0));
  const bob = Math.sin((t / BAR_SEC) * Math.PI * 2) * 7 * enter;
  return { x: HERO_X, y: (-h - 40) + (HERO_TOP - (-h - 40)) * enter + bob, w: HERO_W, h };
}

// ── the popover, hanging off the ♪ glyph like the real NSPopover ───────────
// (arrow tip on the status item, panel below — app-store-real.mjs geometry).
const POP_CAPTIONS = [
  ["the popover piano", "click the ♪ in your menu bar"],
  ["128 instruments", "the full General MIDI palette"],
  ["type to play", "QWERTY is the keyboard · it sends real MIDI"],
];
function drawPopover(t, local, hero) {
  const e = easeOut(clamp01(local * 4.2));
  const ph = H * 0.545;
  const pw = ph * popoverImg.width / popoverImg.height;
  const anchorX = hero.x + hero.w * 0.965;              // the ♪ glyph
  const px = Math.min(W - pw - 24, Math.max(24, anchorX - pw / 2));
  const py = hero.y + hero.h + 16 + (1 - e) * (-40);
  ctx.save(); ctx.globalAlpha = e;
  // callout arrow up at the glyph
  ctx.beginPath();
  ctx.moveTo(anchorX, py - 18);
  ctx.lineTo(anchorX - 22, py + 2);
  ctx.lineTo(anchorX + 22, py + 2);
  ctx.closePath();
  ctx.fillStyle = "rgba(250,249,253,0.99)"; ctx.fill();
  ctx.shadowColor = "rgba(0,0,0,0.42)"; ctx.shadowBlur = 54; ctx.shadowOffsetY = 20;
  roundRect(ctx, px, py, pw, ph, 30); ctx.fillStyle = "rgba(250,249,253,0.99)"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.save(); roundRect(ctx, px, py, pw, ph, 30); ctx.clip();
  ctx.drawImage(popoverImg, px, py, pw, ph);
  ctx.restore();
  ctx.restore();

  // caption beats under the popover
  const idx = Math.min(POP_CAPTIONS.length - 1, Math.floor(local * POP_CAPTIONS.length));
  const capLocal = local * POP_CAPTIONS.length - idx;
  const a = easeOut(clamp01(capLocal * 5)) * (1 - easeOut(clamp01((capLocal - 0.82) / 0.18)));
  if (a > 0 && e > 0.8) {
    ctx.save(); ctx.globalAlpha = a;
    text(ctx, POP_CAPTIONS[idx][0], W / 2, H * 0.80, 84, INK, 800);
    text(ctx, POP_CAPTIONS[idx][1], W / 2, H * 0.80 + 84, 42, "rgba(60,50,80,0.88)", 600);
    ctx.restore();
  }
}

// ── the keymap act — the five family captures, crossfading ─────────────────
function drawKeymapAct(t, local) {
  const e = easeOut(Math.min(1, local * 5));
  const per = 1 / KEYMAP_PROGRAMS.length;
  const idx = Math.min(KEYMAP_PROGRAMS.length - 1, Math.floor(local / per));
  const segLocal = (local - idx * per) / per;
  const fade = easeOut(clamp01(segLocal * 4));           // crossfade into idx

  const aw = W * 0.94;
  const ah = aw * keymapImgs[0].height / keymapImgs[0].width;
  const drift = 1 + 0.015 * Math.sin(local * Math.PI);   // subtle breath
  const dw = aw * drift, dh = ah * drift;
  const ax = (W - dw) / 2, ay = H * 0.46 - dh / 2 + (H * 0.5) * (1 - e);
  ctx.save(); ctx.globalAlpha = e;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ctx, ax, ay, dw, dh, 26); ctx.fillStyle = "white"; ctx.fill();
  ctx.shadowColor = "transparent";
  ctx.save(); roundRect(ctx, ax, ay, dw, dh, 26); ctx.clip();
  if (fade < 1 && idx > 0) ctx.drawImage(keymapImgs[idx - 1], ax, ay, dw, dh);
  ctx.globalAlpha = e * fade;
  ctx.drawImage(keymapImgs[idx], ax, ay, dw, dh);
  ctx.restore();
  ctx.restore();

  if (e > 0.8) {
    const NAMES = ["Piano", "Guitar", "Violin", "Trumpet", "Flute"];
    text(ctx, "it goes fullscreen", W / 2, H * 0.80, 84, INK, 800);
    text(ctx, `LED waveform · big piano · QWERTY map · ${NAMES[idx]}`, W / 2, H * 0.80 + 84, 40, "rgba(60,50,80,0.88)", 600);
  }
}

function drawFrame(t) {
  drawDesktop(ctx);
  const sc = sceneAt(t);
  const local = (sc.to - sc.from) > 0 ? (t - sc.from) / (sc.to - sc.from) : 1;
  const dt = 1 / FPS;

  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(lead, t), hero.x, hero.y, hero.w);
  for (const n of onsetsBetween(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), hRect.y + hRect.h + 6, stripKeyColor(rig, n.midi), true);
  }

  if (sc.name === "popover") {
    drawPopover(t, local, hero);
  } else if (sc.name === "keymap") {
    drawKeymapAct(t, local);
  } else {
    const e = easeOut(Math.min(1, local * 5));
    ctx.save(); ctx.globalAlpha = e;
    const ipx = 300;
    drawIcon(ctx, W / 2 - ipx / 2, H * 0.30, ipx, new Set(litAt(lead, t).map((mm) => ((mm % 12) + 12) % 12 % 5)));
    text(ctx, "Menu Band", W / 2, H * 0.52, 110, INK, 800);
    text(ctx, "free on the Mac App Store", W / 2, H * 0.52 + 104, 50, "rgba(60,50,80,0.9)", 700);
    text(ctx, "menuband.app", W / 2, H * 0.52 + 184, 58, INK, 800);
    ctx.restore();
  }

  particles.stepAndDraw(dt);
  vignette(ctx);
  karaoke?.draw(ctx, t);
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}${AUDIO_VAR}.mp3`, outPath: `${OUT}/base-${SLUG}${VAR}.mp4`,
  total: TOTAL, drawFrame, label: `menuband features sim${VAR}`,
});
writeMeta(`${SLUG}${VAR}`, TOTAL, SCENES);
