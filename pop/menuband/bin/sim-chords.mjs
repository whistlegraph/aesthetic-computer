#!/usr/bin/env node
// sim-chords.mjs — the Menu Band CHORD GRAMMAR quick-hits reel base.
//
// The app's actual modifier grammar (MenuBandController.chordQuality /
// chordIntervals), one segment per quality, audio and visuals driven by the
// same score (out/menuband-chords.score.json from render-jingles.mjs):
//
//   ⌘ major · ⌥ minor · ⌘⌥ sus2 · ⌥⌃ diminished · ⌘⌥⌃ sus4
//
// The REAL captured strip lights every chord (strip-rig composites of the
// app's own lit-key captures), three drawn macOS keycaps press per segment,
// and a finale walks I–vi–IV–V–I with the modifiers flipping live.
//
// Output: out/base-menuband-chords.mp4 + meta-menuband-chords.json —
// then `node pop/menuband/bin/chrome-reel.mjs menuband-chords`.
//
// Usage: node pop/menuband/bin/sim-chords.mjs   (render-jingles.mjs first)

import { readFileSync } from "node:fs";
import {
  W, H, FPS, OUT, INK, easeOut, clamp01, rgb,
  makeStage, roundRect, text, drawDesktop, vignette,
  makeParticles, loadStripRig, drawStrip, stripKeyX, stripKeyColor,
  loadScore, leadOf, litAt, makeOnsets,
  renderVideo, writeMeta, makeScenes, sungMode, loadSungWords, makeKaraoke,
  IS_WEB,
} from "./reel-lib.mjs";

const SLUG = "menuband-chords";
// --sung: jeffrey sings the chord grammar (sing-jingle.mjs) + karaoke.
const { sung: SUNG, audioSuffix: AUDIO_VAR, suffix: VAR } = sungMode();
const karaoke = SUNG ? makeKaraoke(loadSungWords(SLUG), {
  y: H * 0.945,
  size: IS_WEB ? 38 : 54,
}) : null;
const score = loadScore(SLUG);
const { segs } = JSON.parse(readFileSync(`${OUT}/${SLUG}.score.json`, "utf8"));
const TOTAL = score.durationSec;
const lead = leadOf(score);
const onsetsBetween = makeOnsets(lead);

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const particles = makeParticles(ctx);

const { scenes: SCENES } = makeScenes([
  { name: "intro", from: 0.00, to: 0.12, tint: [97, 158, 255] },
  { name: "grammar", from: 0.12, to: 0.63, tint: [255, 214, 56] },
  { name: "finale", from: 0.63, to: 0.90, tint: [163, 230, 53] },
  { name: "end", from: 0.90, to: 1.00, tint: [255, 77, 107] },
], TOTAL);

function segAt(t) {
  const s = segs.find((x) => t >= x.t0 && t < x.t1);
  if (s) return s;
  return t >= segs.at(-1).t0 ? segs.at(-1) : null;
}

// ── the strip: parked upper-third, always playing the chord ────────────────
const HERO_W = W * 0.96, HERO_X = (W - HERO_W) / 2;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const enter = easeOut(clamp01(t / 1.0));
  const bob = Math.sin(t * 1.6) * 8 * enter;
  const rest = H * 0.26 - h / 2;
  return { x: HERO_X, y: (-h - 40) + (rest - (-h - 40)) * enter + bob, w: HERO_W, h };
}

// ── drawn macOS keycaps: ⌘ ⌥ ⌃, pressing per segment ──────────────────────
// The lit-key olive from the real strip captures, so pressed caps match the
// keys they light.
const CAP_LIT = [176, 190, 60];
const CAPS = [
  { id: "cmd", sym: "⌘", name: "command" },
  { id: "opt", sym: "⌥", name: "option" },
  { id: "ctl", sym: "⌃", name: "control" },
];
/// Per-cap press amount eased across segment changes.
const pressState = { cmd: 0, opt: 0, ctl: 0 };
function drawKeycaps(t, dt) {
  const s = segAt(t);
  const active = new Set(s?.mods ?? []);
  const size = 236, gap = 46;
  const rowW = CAPS.length * size + (CAPS.length - 1) * gap;
  const x0 = (W - rowW) / 2, y0 = H * 0.40;
  for (const cap of CAPS) {
    const target = active.has(cap.id) ? 1 : 0;
    pressState[cap.id] += (target - pressState[cap.id]) * Math.min(1, dt * 16);
  }
  CAPS.forEach((cap, i) => {
    const p = pressState[cap.id];
    const x = x0 + i * (size + gap), y = y0 + p * 10;
    ctx.save();
    ctx.shadowColor = "rgba(0,0,0,0.30)";
    ctx.shadowBlur = 26 * (1 - p * 0.7); ctx.shadowOffsetY = 12 * (1 - p * 0.7);
    roundRect(ctx, x, y, size, size, 42);
    // white cap → lit olive as it presses (the strip's own lit-key color)
    const mix = (a, b) => Math.round(a + (b - a) * p);
    ctx.fillStyle = `rgb(${mix(250, CAP_LIT[0])},${mix(249, CAP_LIT[1])},${mix(253, CAP_LIT[2])})`;
    ctx.fill();
    ctx.shadowColor = "transparent";
    ctx.lineWidth = 3; ctx.strokeStyle = "rgba(20,18,28,0.22)"; ctx.stroke();
    const ink = p > 0.5 ? "rgba(20,18,28,0.95)" : "rgba(20,18,28,0.85)";
    text(ctx, cap.sym, x + size / 2, y + size * 0.44, 110, ink, 500, "center", false);
    text(ctx, cap.name, x + size / 2, y + size * 0.80, 34, ink, 600, "center", false);
    ctx.restore();
  });
}

function drawFrame(t) {
  drawDesktop(ctx);
  const dt = 1 / FPS;
  const s = segAt(t);

  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(lead, t, 0.3), hero.x, hero.y, hero.w);
  for (const n of onsetsBetween(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), hRect.y - 6, stripKeyColor(rig, n.midi), false);
  }

  // kicker up top, all reel long
  const ka = easeOut(clamp01((t - 0.5) / 0.6));
  if (ka > 0) {
    ctx.save(); ctx.globalAlpha = ka;
    text(ctx, "chords live in your modifier keys", W / 2, H * 0.115, 54, INK, 800);
    ctx.restore();
  }

  drawKeycaps(t, dt);

  // segment readouts: the chord name huge, the grammar line under it
  if (s) {
    const segLocal = clamp01((t - s.t0) / Math.max(0.001, s.t1 - s.t0));
    const a = easeOut(clamp01(segLocal * 6));
    ctx.save(); ctx.globalAlpha = a;
    text(ctx, s.chordName, W / 2, H * 0.645, 132, INK, 800);
    text(ctx, s.label, W / 2, H * 0.645 + 118, 54, "rgba(60,50,80,0.9)", 700);
    ctx.restore();
  }

  // end tag over the ringing final chord
  const endA = easeOut(clamp01((t - (segs.at(-1).t0 + 1.2)) / 0.8));
  if (endA > 0) {
    ctx.save(); ctx.globalAlpha = endA;
    text(ctx, "menuband.app", W / 2, H * 0.815, 88, INK, 800);
    text(ctx, "free on the Mac App Store", W / 2, H * 0.815 + 88, 46, "rgba(60,50,80,0.9)", 700);
    ctx.restore();
  }

  particles.stepAndDraw(dt);
  vignette(ctx);
  karaoke?.draw(ctx, t);
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}${AUDIO_VAR}.mp3`, outPath: `${OUT}/base-${SLUG}${VAR}.mp4`,
  total: TOTAL, drawFrame, label: `menuband chords sim${VAR}`,
});
writeMeta(`${SLUG}${VAR}`, TOTAL, SCENES);
