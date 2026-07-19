#!/usr/bin/env node
// sim-scales.mjs — the Menu Band SCALES teaching-singalong reel base (v6).
//
// The reel that literally demonstrates the spinging speak→sing transform:
// a spoken frame ("Here's how to type out the C scale." … "Now sing it!")
// around the notepat two-octave letter ladder — c d e f g a b h i j k l m n
// up and back down (h..n = notepat's second octave) — each letter sung on
// its own scale degree while the REAL captured menu-bar strip lights that
// key (strip rig, now covering every white key C4..B5) and a BIG keycap
// shows the letter karaoke-chart style. Spoken lines get normal captions.
//
// Timeline comes from out/menuband-scales.score.json (render-jingles.mjs) —
// the same sidecar sing-jingle.mjs sings from, so audio, voice and visuals
// share ONE clock. Word timings: out/menuband-scales.words.sung.json.
//
// This reel only exists sung — run with --sung (the suffix keeps the
// naming pattern of the other campaign reels):
//   node pop/menuband/bin/sing-jingle.mjs menuband-scales
//   node pop/menuband/bin/sim-scales.mjs --sung
//   node pop/menuband/bin/chrome-reel.mjs menuband-scales-sung

import { readFileSync } from "node:fs";
import {
  W, H, FPS, OUT, INK, INK_RGB, easeOut, clamp01, rgb,
  makeStage, roundRect, text, drawDesktop, vignette, drawIcon,
  makeParticles, loadStripRig, drawStrip, stripKeyX, stripKeyColor,
  loadScore, leadOf, litAt, makeOnsets,
  renderVideo, writeMeta, makeScenes, sungMode, loadSungWords, makeKaraoke,
} from "./reel-lib.mjs";

const SLUG = "menuband-scales";
const { sung: SUNG, suffix: VAR } = sungMode();
if (!SUNG) {
  console.error("✗ the scales reel only exists sung — run with --sung");
  process.exit(1);
}
const score = loadScore(SLUG);
const sc = JSON.parse(readFileSync(`${OUT}/${SLUG}.score.json`, "utf8"));
const TOTAL = score.durationSec;
const lead = leadOf(score);
const onsetsBetween = makeOnsets(lead);

// spoken words → normal bottom captions; sung letters drive the big keycap
const allWords = loadSungWords(SLUG);
const karaoke = makeKaraoke(allWords.filter((w) => w.spoken), { y: H * 0.925 });

const { canvas, ctx } = makeStage();
const rig = await loadStripRig();
const particles = makeParticles(ctx);

const SING0 = sc.sing.t0, SING1 = sc.sing.t1;
const { scenes: SCENES, sceneAt } = makeScenes([
  { name: "intro", from: 0, to: sc.spoken.run.t / TOTAL, tint: [97, 158, 255] },
  { name: "speak", from: sc.spoken.run.t / TOTAL, to: SING0 / TOTAL, tint: [255, 214, 56] },
  { name: "sing-up", from: SING0 / TOTAL, to: (SING0 + (SING1 - SING0) * 0.52) / TOTAL, tint: [51, 209, 179] },
  { name: "sing-down", from: (SING0 + (SING1 - SING0) * 0.52) / TOTAL, to: (SING1 + 0.8) / TOTAL, tint: [167, 139, 250] },
  { name: "end", from: (SING1 + 0.8) / TOTAL, to: 1.0, tint: [255, 77, 107] },
], TOTAL);

// ── the strip: parked upper area, playing the ladder ───────────────────────
const HERO_W = W * 0.96, HERO_X = (W - HERO_W) / 2;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const enter = easeOut(clamp01(t / 1.1));
  const bob = Math.sin(t * 1.5) * 7 * enter;
  const rest = H * 0.165 - h / 2;
  return { x: HERO_X, y: (-h - 40) + (rest - (-h - 40)) * enter + bob, w: HERO_W, h };
}

// ── the ladder clock: which letter is active at t ──────────────────────────
const LADDER = sc.ladder;   // [{ letter, t, dur, strip, vocal, dir }]
function activeNote(t) {
  let cur = null;
  for (const n of LADDER) if (t >= n.t) cur = n; else break;
  if (!cur) return null;
  if (t > cur.t + Math.max(cur.dur, 0.5) + 0.35) return null;   // ladder done
  return cur;
}
const LETTERS = [...new Set(LADDER.map((n) => n.letter))];      // c..n in order

// ── the BIG teaching keycap — the letter you type, huge, in the key's own
// strip color, pressing on every onset ─────────────────────────────────────
function drawKeycap(t) {
  const n = activeNote(t);
  if (!n) return;
  const local = clamp01((t - n.t) / 0.14);
  const press = 1 - easeOut(local);                 // 1 at strike → 0 settled
  const inA = easeOut(clamp01((t - SING0 + 0.4) / 0.5)) *
    (1 - easeOut(clamp01((t - (SING1 + 0.35)) / 0.5)));
  if (inA <= 0) return;
  const size = 430;
  const x = W / 2 - size / 2, y = H * 0.335 - size / 2 + press * 14;
  const col = stripKeyColor(rig, n.strip);
  ctx.save();
  ctx.globalAlpha = inA;
  ctx.shadowColor = "rgba(0,0,0,0.32)";
  ctx.shadowBlur = 34 * (1 - press * 0.7); ctx.shadowOffsetY = 16 * (1 - press * 0.7);
  roundRect(ctx, x, y, size, size, 64);
  ctx.fillStyle = "rgb(250,249,253)"; ctx.fill();
  ctx.shadowColor = "transparent";
  // the key's strip color washes the cap as it presses
  ctx.globalAlpha = inA * (0.16 + 0.22 * press);
  roundRect(ctx, x, y, size, size, 64);
  ctx.fillStyle = rgb(col); ctx.fill();
  ctx.globalAlpha = inA;
  ctx.lineWidth = 3; ctx.strokeStyle = "rgba(20,18,28,0.22)";
  roundRect(ctx, x, y, size, size, 64); ctx.stroke();
  const pop = 1 + 0.09 * press;
  ctx.translate(W / 2, y + size * 0.47); ctx.scale(pop, pop);
  text(ctx, n.letter.toUpperCase(), 0, 0, 250, INK, 800);
  ctx.scale(1 / pop, 1 / pop);
  text(ctx, n.dir === "up" ? "↑" : "↓", 0, size * 0.36, 54, rgb(col), 800);
  ctx.restore();
}

// ── the ladder chart — all 14 letters, the singalong teaching row ──────────
function drawChart(t) {
  const a = easeOut(clamp01((t - SING0 + 0.6) / 0.5)) *
    (1 - easeOut(clamp01((t - (SING1 + 0.5)) / 0.5)));
  if (a <= 0) return;
  const n = activeNote(t);
  const activeIdx = n ? LETTERS.indexOf(n.letter) : -1;
  const goingUp = !n || n.dir === "up";
  const cellW = 68, y = H * 0.585;
  const x0 = W / 2 - (LETTERS.length * cellW) / 2;
  ctx.save();
  ctx.globalAlpha = a;
  for (let i = 0; i < LETTERS.length; i++) {
    const cx = x0 + i * cellW + cellW / 2;
    const isActive = i === activeIdx;
    const sungAlready = goingUp ? i < activeIdx : i > activeIdx;
    if (isActive) {
      const col = stripKeyColor(rig, LADDER[i].strip);
      roundRect(ctx, cx - 28, y - 34, 56, 68, 16);
      ctx.fillStyle = rgb(col); ctx.fill();
      text(ctx, LETTERS[i], cx, y + 1, 44, "rgba(255,255,255,0.98)", 800);
    } else {
      text(ctx, LETTERS[i], cx, y + 1, 40,
        sungAlready ? INK : "rgba(20,18,28,0.32)", 700);
    }
  }
  text(ctx, "c major · two octaves · h through n = the next c to b",
    W / 2, y + 74, 34, "rgba(60,50,80,0.85)", 600);
  ctx.restore();
}

// ── the end card — app icon + menuband.app ─────────────────────────────────
function drawEndCard(t) {
  const e = easeOut(clamp01((t - (SING1 + 1.0)) / 0.7));
  if (e <= 0) return;
  const cw = W * 0.78, chh = H * 0.40;
  const cx = (W - cw) / 2, cy = H * 0.44 - chh / 2 + (1 - e) * H * 0.35;
  ctx.save(); ctx.globalAlpha = e;
  ctx.shadowColor = "rgba(0,0,0,0.45)"; ctx.shadowBlur = 60; ctx.shadowOffsetY = 24;
  roundRect(ctx, cx, cy, cw, chh, 40); ctx.fillStyle = "rgba(250,249,253,0.99)"; ctx.fill();
  ctx.shadowColor = "transparent";
  const litIcon = new Set(litAt(lead, t).map((mm) => ((mm % 12) + 12) % 12 % 5));
  const ipx = 330;
  drawIcon(ctx, W / 2 - ipx / 2, cy + 40, ipx, litIcon);
  text(ctx, "menuband.app", W / 2, cy + 450, 88, INK, 800);
  text(ctx, "free on the Mac App Store", W / 2, cy + 540, 44, "rgba(60,50,80,0.9)", 700);
  text(ctx, "type it yourself", W / 2, cy + 610, 40, "rgba(60,50,80,0.75)", 600);
  ctx.restore();
}

function drawFrame(t) {
  drawDesktop(ctx);
  const dt = 1 / FPS;

  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(lead, t, 0.3), hero.x, hero.y, hero.w);
  for (const n of onsetsBetween(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), hRect.y + hRect.h + 6,
      stripKeyColor(rig, n.midi), true);
  }

  // kicker under the strip — it has its say during the spoken frame, then
  // clears the stage for the big teaching keycap
  const ka = easeOut(clamp01((t - 0.9) / 0.6)) *
    (1 - easeOut(clamp01((t - (SING0 - 0.5)) / 0.5)));
  if (ka > 0) {
    ctx.save(); ctx.globalAlpha = ka;
    text(ctx, "your keyboard knows the C scale", W / 2, H * 0.245, 52, INK, 800);
    ctx.restore();
  }

  drawKeycap(t);
  drawChart(t);
  drawEndCard(t);

  particles.stepAndDraw(dt);
  vignette(ctx);
  karaoke.draw(ctx, t);
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}${VAR}.mp3`, outPath: `${OUT}/base-${SLUG}${VAR}.mp4`,
  total: TOTAL, drawFrame, label: `menuband scales sim${VAR}`,
});
writeMeta(`${SLUG}${VAR}`, TOTAL, SCENES);
