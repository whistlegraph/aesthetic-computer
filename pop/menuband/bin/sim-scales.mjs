#!/usr/bin/env node
// sim-scales.mjs — the Menu Band SCALES teaching-singalong reel base (v7).
//
// The reel that literally demonstrates the spinging speak→sing transform:
// a spoken frame ("Here's how to type out the C scale." → straight into the
// singing) around the notepat two-octave letter ladder — c d e f g a b
// h i j k l m n up and back down — each letter sung on its own scale degree.
//
// v7 layout (jeffrey's round-7 spec): the video is ONLY the performance —
// no desktop scenery, no falling-in window animation, no end-card theatrics.
// Top to bottom in 1080x1920:
//   · the REAL captured menu-bar strip (strip rig), parked, keys lighting
//     per sung note — the Menu Band GUI as the centerpiece instrument
//   · the BIG teaching keycap (the letter you type, huge, karaoke-style)
//   · the 14-letter ladder chart
//   · a QWERTY keyboard modeled on the app's fullscreen-popover keymap view
//     (out/about-frames/keymap-*.png) but textured like the MacBook Neo
//     citrus keycaps (out/neo-key-refs/ — neo-keyboard-trackpad@2x.jpg for
//     cap geometry/gradient, pv_keyboard_endframe.jpg + neo-overhead-citrus
//     for the citrus deck + cream-citrus cap colors). The 14 scale letters
//     are literal typing keys — each key depresses + glows in its strip
//     color as it's sung; the visual IS the typing lesson.
//   · spoken lines as normal bottom captions
// The end beat stays inside the performance framing: keyboard + strip at
// rest, app icon + menuband.app where the big keycap sang.
//
// Timeline comes from out/menuband-scales.score.json (render-jingles.mjs) —
// the same sidecar sing-jingle.mjs sings from, so audio, voice and visuals
// share ONE clock. Word timings: out/menuband-scales.words.sung.json.
// AUDIO IS FINAL — this sim only muxes the existing mastered mp3.
//
// This reel only exists sung — run with --sung:
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
  { name: "intro", from: 0, to: SING0 / TOTAL, tint: [97, 158, 255] },
  { name: "sing-up", from: SING0 / TOTAL, to: (SING0 + (SING1 - SING0) * 0.52) / TOTAL, tint: [51, 209, 179] },
  { name: "sing-down", from: (SING0 + (SING1 - SING0) * 0.52) / TOTAL, to: (SING1 + 0.8) / TOTAL, tint: [167, 139, 250] },
  { name: "end", from: (SING1 + 0.8) / TOTAL, to: 1.0, tint: [255, 77, 107] },
], TOTAL);

// ── the strip: parked at the top, playing the ladder (no fly-in) ───────────
const HERO_W = W * 0.94, HERO_X = (W - HERO_W) / 2;
function heroRect(t) {
  const h = HERO_W / rig.aspect;
  const bob = Math.sin(t * 1.5) * 4;
  return { x: HERO_X, y: H * 0.10 - h / 2 + bob, w: HERO_W, h };
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
const LETTER_STRIP = new Map(LADDER.map((n) => [n.letter, n.strip]));

// ── the BIG teaching keycap — the letter you type, huge, in the key's own
// strip color, pressing on every onset ─────────────────────────────────────
const CAP_CY = H * 0.295;
function drawKeycap(t) {
  const n = activeNote(t);
  if (!n) return;
  const local = clamp01((t - n.t) / 0.14);
  const press = 1 - easeOut(local);                 // 1 at strike → 0 settled
  const inA = easeOut(clamp01((t - SING0 + 0.4) / 0.5)) *
    (1 - easeOut(clamp01((t - (SING1 + 0.35)) / 0.5)));
  if (inA <= 0) return;
  const size = 380;
  const x = W / 2 - size / 2, y = CAP_CY - size / 2 + press * 14;
  const col = stripKeyColor(rig, n.strip);
  ctx.save();
  ctx.globalAlpha = inA;
  ctx.shadowColor = "rgba(0,0,0,0.32)";
  ctx.shadowBlur = 34 * (1 - press * 0.7); ctx.shadowOffsetY = 16 * (1 - press * 0.7);
  roundRect(ctx, x, y, size, size, 58);
  ctx.fillStyle = "rgb(250,249,253)"; ctx.fill();
  ctx.shadowColor = "transparent";
  // the key's strip color washes the cap as it presses
  ctx.globalAlpha = inA * (0.16 + 0.22 * press);
  roundRect(ctx, x, y, size, size, 58);
  ctx.fillStyle = rgb(col); ctx.fill();
  ctx.globalAlpha = inA;
  ctx.lineWidth = 3; ctx.strokeStyle = "rgba(20,18,28,0.22)";
  roundRect(ctx, x, y, size, size, 58); ctx.stroke();
  const pop = 1 + 0.09 * press;
  ctx.translate(W / 2, y + size * 0.47); ctx.scale(pop, pop);
  text(ctx, n.letter.toUpperCase(), 0, 0, 220, INK, 800);
  ctx.scale(1 / pop, 1 / pop);
  text(ctx, n.dir === "up" ? "↑" : "↓", 0, size * 0.36, 48, rgb(col), 800);
  ctx.restore();
}

// ── the ladder chart — all 14 letters, the singalong teaching row ──────────
const CHART_Y = H * 0.475;
function drawChart(t) {
  const a = easeOut(clamp01((t - SING0 + 0.6) / 0.5)) *
    (1 - easeOut(clamp01((t - (SING1 + 0.5)) / 0.5)));
  if (a <= 0) return;
  const n = activeNote(t);
  const activeIdx = n ? LETTERS.indexOf(n.letter) : -1;
  const goingUp = !n || n.dir === "up";
  const cellW = 68, y = CHART_Y;
  const x0 = W / 2 - (LETTERS.length * cellW) / 2;
  ctx.save();
  ctx.globalAlpha = a;
  for (let i = 0; i < LETTERS.length; i++) {
    const cx = x0 + i * cellW + cellW / 2;
    const isActive = i === activeIdx;
    const sungAlready = goingUp ? i < activeIdx : i > activeIdx;
    if (isActive) {
      const col = stripKeyColor(rig, LETTER_STRIP.get(LETTERS[i]));
      roundRect(ctx, cx - 28, y - 34, 56, 68, 16);
      ctx.fillStyle = rgb(col); ctx.fill();
      text(ctx, LETTERS[i], cx, y + 1, 44, "rgba(255,255,255,0.98)", 800);
    } else {
      text(ctx, LETTERS[i], cx, y + 1, 40,
        sungAlready ? INK : "rgba(20,18,28,0.32)", 700);
    }
  }
  text(ctx, "c major · two octaves · h through n = the next c to b",
    W / 2, y + 66, 32, "rgba(60,50,80,0.85)", 600);
  ctx.restore();
}

// ── the neo QWERTY board — citrus deck + cream-citrus caps, full ANSI ──────
// Geometry/legends after the MacBook Neo overheads (neo-keyboard-trackpad@2x
// for the cap look: rounded caps, top-lit vertical gradient, soft shadow);
// colors sampled from the citrus refs: deck ≈ rgb(233,230,149)
// (pv_keyboard_endframe), caps ≈ rgb(244,243,187) with the @2x top-light.
const DECK_A = [238, 235, 162], DECK_B = [221, 217, 130];   // citrus deck
const WELL = [199, 195, 120];                                // recessed key well
const CAP_A = [250, 249, 208], CAP_B = [235, 233, 179];      // keycap gradient
const CAP_EDGE = "rgba(146,142,82,0.38)";
const KEY_INK = [56, 58, 36];                                // olive legend ink

const KB = buildKeyboard();
function buildKeyboard() {
  const U = (W * 0.88) / 14.5;           // key pitch (14.5u ANSI rows)
  const GAP = Math.max(5, Math.round(U * 0.09));
  const PAD = 30;                        // deck padding around the well
  const FROW = 0.62;                     // function row height (units)
  const rows = [
    { h: FROW, keys: [["esc", 1.5, "word"],
      ...Array.from({ length: 12 }, (_, i) => [`F${i + 1}`, 1, "f"]), ["", 1, "power"]] },
    { h: 1, keys: [["`", 1, "sym"], ...[..."1234567890"].map((d) => [d, 1, "sym"]),
      ["-", 1, "sym"], ["=", 1, "sym"], ["delete", 1.5, "word"]] },
    { h: 1, keys: [["tab", 1.5, "word"], ...[..."qwertyuiop"].map((l) => [l, 1, "letter"]),
      ["[", 1, "sym"], ["]", 1, "sym"], ["\\", 1, "sym"]] },
    { h: 1, keys: [["caps lock", 1.75, "word"], ...[..."asdfghjkl"].map((l) => [l, 1, "letter"]),
      [";", 1, "sym"], ["'", 1, "sym"], ["return", 1.75, "word"]] },
    { h: 1, keys: [["shift", 2.25, "word"], ...[..."zxcvbnm"].map((l) => [l, 1, "letter"]),
      [",", 1, "sym"], [".", 1, "sym"], ["/", 1, "sym"], ["shift", 2.25, "word"]] },
    { h: 1, keys: [["fn", 1, "word"], ["control", 1, "word"], ["option", 1, "word"],
      ["command", 1.25, "word"], ["", 5, "space"], ["command", 1.25, "word"],
      ["option", 1, "word"], ["", 3, "arrows"]] },
  ];
  const wellH = rows.reduce((s, r) => s + r.h * U, 0);
  const deckW = 14.5 * U + PAD * 2, deckH = wellH + PAD * 2;
  const deckX = (W - deckW) / 2, deckY = H * 0.535;
  const keys = [];
  let y = deckY + PAD;
  for (const row of rows) {
    let x = deckX + PAD;
    for (const [label, wUnits, style] of row.keys) {
      keys.push({
        label, style,
        x: x + GAP / 2, y: y + GAP / 2,
        w: wUnits * U - GAP, h: row.h * U - GAP,
      });
      x += wUnits * U;
    }
    y += row.h * U;
  }
  return { U, GAP, deckX, deckY, deckW, deckH, PAD, keys };
}

function drawCap(k, { press = 0, wash = null } = {}) {
  const r = KB.U * 0.14;
  const y = k.y + press * 4;
  ctx.save();
  if (wash && press > 0) {                       // colored glow cast while lit
    ctx.shadowColor = rgb(wash, 0.85 * press);
    ctx.shadowBlur = 30;
    roundRect(ctx, k.x, y, k.w, k.h, r);
    ctx.fillStyle = rgb(wash); ctx.fill();
    ctx.shadowColor = "transparent";
  }
  ctx.shadowColor = `rgba(96,94,50,${0.30 * (1 - press * 0.6)})`;
  ctx.shadowBlur = 7 * (1 - press * 0.5);
  ctx.shadowOffsetY = 3.5 * (1 - press * 0.6);
  const g = ctx.createLinearGradient(0, y, 0, y + k.h);
  g.addColorStop(0, rgb(CAP_A)); g.addColorStop(1, rgb(CAP_B));
  roundRect(ctx, k.x, y, k.w, k.h, r);
  ctx.fillStyle = g; ctx.fill();
  ctx.shadowColor = "transparent";
  if (wash) {                                    // pressed: darken + color wash
    roundRect(ctx, k.x, y, k.w, k.h, r);
    ctx.fillStyle = rgb(wash, 0.30 + 0.22 * press); ctx.fill();
    roundRect(ctx, k.x, y, k.w, k.h, r);
    ctx.fillStyle = `rgba(0,0,0,${0.06 * press})`; ctx.fill();
  }
  roundRect(ctx, k.x, y, k.w, k.h, r);
  ctx.lineWidth = 1.5; ctx.strokeStyle = CAP_EDGE; ctx.stroke();
  ctx.restore();
  return y;
}

function drawKeyboard(t) {
  const a = easeOut(clamp01((t - 0.25) / 0.6));
  if (a <= 0) return;
  const n = activeNote(t);
  const U = KB.U;
  ctx.save();
  ctx.globalAlpha = a;

  // the citrus deck, floating like the machine it is
  ctx.shadowColor = "rgba(40,30,60,0.35)"; ctx.shadowBlur = 44; ctx.shadowOffsetY = 18;
  const dg = ctx.createLinearGradient(KB.deckX, KB.deckY, KB.deckX, KB.deckY + KB.deckH);
  dg.addColorStop(0, rgb(DECK_A)); dg.addColorStop(1, rgb(DECK_B));
  roundRect(ctx, KB.deckX, KB.deckY, KB.deckW, KB.deckH, 34);
  ctx.fillStyle = dg; ctx.fill();
  ctx.shadowColor = "transparent";
  // the recessed key well
  roundRect(ctx, KB.deckX + KB.PAD - 8, KB.deckY + KB.PAD - 8,
    KB.deckW - 2 * KB.PAD + 16, KB.deckH - 2 * KB.PAD + 16, 14);
  ctx.fillStyle = rgb(WELL); ctx.fill();

  for (const k of KB.keys) {
    const isScale = k.style === "letter" && LETTER_STRIP.has(k.label);
    const lit = !!n && isScale && k.label === n.letter;
    const press = lit ? 0.55 + 0.45 * (1 - easeOut(clamp01((t - n.t) / 0.14))) : 0;
    const col = isScale ? stripKeyColor(rig, LETTER_STRIP.get(k.label)) : null;

    if (k.style === "arrows") {                  // ◄ ▲▼ ► cluster, half-height
      const hh = (k.h - KB.GAP) / 2, cw = (k.w - 2 * KB.GAP) / 3;
      drawCap({ x: k.x, y: k.y + hh + KB.GAP, w: cw, h: hh });
      drawCap({ x: k.x + cw + KB.GAP, y: k.y, w: cw, h: hh });
      drawCap({ x: k.x + cw + KB.GAP, y: k.y + hh + KB.GAP, w: cw, h: hh });
      drawCap({ x: k.x + 2 * (cw + KB.GAP), y: k.y + hh + KB.GAP, w: cw, h: hh });
      continue;
    }
    const y = drawCap(k, { press, wash: lit ? col : null });
    const cx = k.x + k.w / 2, cy = y + k.h / 2;
    if (k.style === "letter") {
      const alpha = lit ? 0.95 : isScale ? 0.88 : 0.40;
      text(ctx, k.label.toUpperCase(), cx, cy,
        isScale ? U * 0.40 : U * 0.34, rgb(KEY_INK, alpha), isScale ? 800 : 600);
      if (isScale) {                             // strip-color underline chip
        roundRect(ctx, cx - k.w * 0.28, y + k.h - 10, k.w * 0.56, lit ? 6 : 5, 3);
        ctx.fillStyle = rgb(col, lit ? 1 : 0.55); ctx.fill();
      }
    } else if (k.style === "sym") {
      text(ctx, k.label, cx, cy, U * 0.32, rgb(KEY_INK, 0.35), 600);
    } else if (k.style === "word") {
      text(ctx, k.label, cx, y + k.h - 13, U * 0.21, rgb(KEY_INK, 0.40), 500);
    } else if (k.style === "f") {
      text(ctx, k.label, cx, y + k.h - 12, U * 0.19, rgb(KEY_INK, 0.35), 500);
    } else if (k.style === "power") {
      ctx.beginPath(); ctx.arc(cx, cy, U * 0.15, 0, 7);
      ctx.lineWidth = 1.5; ctx.strokeStyle = rgb(KEY_INK, 0.35); ctx.stroke();
    }
  }
  ctx.restore();
}

// ── the end beat — icon + menuband.app inside the performance framing ──────
function drawEndBeat(t) {
  const e = easeOut(clamp01((t - (SING1 + 1.0)) / 0.7));
  if (e <= 0) return;
  ctx.save();
  ctx.globalAlpha = e;
  ctx.translate(0, (1 - e) * 40);
  const ipx = 260;
  const litIcon = new Set(litAt(lead, t).map((mm) => ((mm % 12) + 12) % 12 % 5));
  drawIcon(ctx, W / 2 - ipx / 2, CAP_CY - ipx / 2 - 50, ipx, litIcon);
  text(ctx, "menuband.app", W / 2, CAP_CY + ipx / 2 + 40, 70, INK, 800);
  text(ctx, "free on the Mac App Store", W / 2, CAP_CY + ipx / 2 + 110, 38,
    "rgba(60,50,80,0.85)", 600);
  ctx.restore();
}

function drawFrame(t) {
  drawDesktop(ctx);
  const dt = 1 / FPS;

  const hero = heroRect(t);
  const hRect = drawStrip(ctx, rig, litAt(lead, t, 0.3), hero.x, hero.y, hero.w,
    easeOut(clamp01(t / 0.5)));
  for (const n of onsetsBetween(t - dt, t)) {
    particles.spawnNote(stripKeyX(rig, n.midi, hRect), hRect.y + hRect.h + 6,
      stripKeyColor(rig, n.midi), true);
  }

  // kicker in the big keycap's spot — it has its say during the spoken frame,
  // then clears the stage for the teaching letter
  const ka = easeOut(clamp01((t - 0.9) / 0.6)) *
    (1 - easeOut(clamp01((t - (SING0 - 0.5)) / 0.5)));
  if (ka > 0) {
    ctx.save(); ctx.globalAlpha = ka;
    text(ctx, "your keyboard knows the C scale", W / 2, CAP_CY - 20, 52, INK, 800);
    ctx.restore();
  }

  drawKeyboard(t);
  drawKeycap(t);
  drawChart(t);
  drawEndBeat(t);

  particles.stepAndDraw(dt);
  vignette(ctx);
  karaoke.draw(ctx, t);
}

await renderVideo({
  canvas, audioPath: `${OUT}/${SLUG}${VAR}.mp3`, outPath: `${OUT}/base-${SLUG}${VAR}.mp4`,
  total: TOTAL, drawFrame, label: `menuband scales sim${VAR}`,
});
writeMeta(`${SLUG}${VAR}`, TOTAL, SCENES);
