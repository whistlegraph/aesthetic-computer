#!/usr/bin/env node
// jungle/bin/preview-spin.mjs — CUTE + SUBTLE moving-string score-train.
//
// The shared verlet string (full tracked physics) is plucked gently by
// each jungle lane; the lane CLIPS activate (brighten) as the playhead
// passes — soft, screen-blended, soaking into a calm (un-warped, un-
// lurched) illustration. The disc turns one slow rotation over the
// track. Chrome conforms to the other vertical videos: YWFT title +
// YWFT lyric word + the canonical colored SECTIONED progress bar (no
// text labels). Reuses pop/lib/{cover-engine,preview-shared}.mjs.
//
// Usage:
//   node bin/preview-spin.mjs --slug solafiya --desktop
//   node bin/preview-spin.mjs --slug solafiya --portrait --desktop

import { existsSync, readFileSync, copyFileSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";
import { once } from "node:events";
import { spawnSync } from "node:child_process";
import { createCanvas, loadImage } from "canvas";
import {
  checkYwftAvailable, decodeAudioMono, computeRmsEnvelope,
  prerenderTitleChars, drawCoverKenBurns, drawTitleBounce,
  spawnFFmpegEncode, AUDIO_SR_DEFAULT, magickMeasureWidth,
  setPreviewFont, getPreviewFont,
} from "../../lib/preview-shared.mjs";

// girly, less-"computery" type for this track (variable type — the
// shared pipeline picks the face; everything below follows it).
// cute but LEGIBLE — rounded Chalkboard, not a script face.
setPreviewFont(process.env.PREVIEW_FONT || "rounded");
const FONT = getPreviewFont();

// local YWFT word raster — uses the same `-annotate` path prerenderTitle
// uses (works), NOT magickRenderText's brittle -shadow clone chain.
async function renderWordYWFT(txt, pt, outPath) {
  mkdirSync(outPath.replace(/\/[^/]+$/, ""), { recursive: true });
  const w = Math.ceil((magickMeasureWidth(txt, pt) || txt.length * pt * 0.55) + 40);
  const h = Math.ceil(pt * 1.7);
  const by = Math.ceil(pt * 1.25);
  const r = spawnSync("magick", [
    "-size", `${w}x${h}`, "xc:none", "-font", FONT, "-pointsize", String(pt),
    "-fill", "rgba(0,0,0,0.82)", "-annotate", `+23+${by + 4}`, txt,
    "-fill", "#fff7d8", "-annotate", `+20+${by}`, txt, outPath,
  ]);
  if (r.status !== 0) throw new Error(`YWFT word render failed: ${txt}`);
  return await loadImage(outPath);
}

// flat single-fill YWFT plate (no baked shadow) — for the timecode, so
// the colored top pass can be section-recolored via source-in and a
// separate solid-black plate stamped behind. Mirrors the undabeach /
// waltz vertical's drawTimecode, but on the working `-annotate` path.
async function renderTcPlate(txt, pt, fillCss, outPath) {
  mkdirSync(outPath.replace(/\/[^/]+$/, ""), { recursive: true });
  const w = Math.ceil((magickMeasureWidth(txt, pt) || txt.length * pt * 0.55) + 24);
  const h = Math.ceil(pt * 1.7);
  const by = Math.ceil(pt * 1.25);
  const r = spawnSync("magick", [
    "-size", `${w}x${h}`, "xc:none", "-font", FONT, "-pointsize", String(pt),
    "-fill", fillCss, "-annotate", `+12+${by}`, txt, outPath,
  ]);
  if (r.status !== 0) throw new Error(`YWFT tc render failed: ${txt}`);
  return await loadImage(outPath);
}
import { makeVerletString, hexToRgb } from "../../lib/cover-engine.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "../..");

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n === undefined || n.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = n; i++; }
}

const SLUG  = flags.slug || "solafiya";
// vertical is THE output now — only the 9:16 cut gets re-rendered.
// pass --square to force the old 1:1 cut.
const PORTRAIT = flags.square !== true;
const TAG   = PORTRAIT ? "-portrait" : "";
const SIZE  = flags.size || (PORTRAIT ? "1080x1920" : "1080x1080");
const FPS   = Number(flags.fps ?? 30);
const TITLE = String(flags.title || SLUG);
// KARAOKE on by default now (real lyrics) — pass --no-lyrics to drop.
const SHOW_LYRICS = flags["no-lyrics"] !== true;
const [W, H] = SIZE.split("x").map(Number);

const STRUCT = `${LANE}/out/${SLUG}.struct.json`;
const AUDIO  = `${LANE}/out/${SLUG}.mp3`;
const ILLY   = `${LANE}/out/${SLUG}.illy.png`;
const COVER  = `${LANE}/out/${SLUG}-cover.png`;
const OUT    = `${LANE}/out/${SLUG}-preview-spin${TAG}.mp4`;
const BASE   = existsSync(ILLY) ? ILLY : COVER;

for (const [nm, p] of [["struct", STRUCT], ["audio", AUDIO], ["illustration", BASE]]) {
  if (!existsSync(p)) { console.error(`✗ ${nm} missing: ${p.replace(REPO + "/", "")} — run render.mjs --slug ${SLUG}`); process.exit(1); }
}
checkYwftAvailable();

const struct = JSON.parse(readFileSync(STRUCT, "utf8"));
const sections = struct.sections || [];
if (!struct.laneAudio?.paths) { console.error("✗ struct.json has no laneAudio — re-run render.mjs"); process.exit(1); }
const laneSr = struct.laneAudio.sampleRate || 4000;

// cute pastel section tints (conforms to the segmented-bar convention)
const SECTION_TINTS = {
  "intro":     [255, 226, 138],   // soft honey
  "roll-in":   [255, 178, 150],   // peach
  "drop 1":    [255, 150, 196],   // bubblegum pink
  "dub-break": [186, 162, 240],   // lavender
  "drop 2":    [150, 232, 200],   // mint
  "out":       [255, 206, 120],   // warm gold
};
const tintOf = (nm) => SECTION_TINTS[nm] || [220, 220, 220];

console.log("  decoding audio …");
const { audio, sr } = decodeAudioMono(AUDIO, AUDIO_SR_DEFAULT);
const DURATION = audio.length / sr;
const ENV_FPS = 60;
const env = computeRmsEnvelope(audio, sr, ENV_FPS, DURATION);
const envAt = (t) => { const i = Math.round(t * ENV_FPS); return i >= 0 && i < env.length ? env[i] : 0; };
const FRAMES = Math.ceil(DURATION * FPS);

// curated lanes (soft colours), top→bottom across the string
const LANE_DEFS = [                                   // 6 core lanes — calmer
  { key: "break", color: "#ff9ec9" }, { key: "sub",  color: "#bfa2f0" },
  { key: "skank", color: "#9fe6ff" }, { key: "bell", color: "#ffe08a" },
  { key: "meow",  color: "#ffb3da" }, { key: "vocal", color: "#ffffff" },
];
const LANES = [];
for (const d of LANE_DEFS) {
  const rel = struct.laneAudio.paths[d.key];
  if (!rel || !existsSync(`${LANE}/${rel}`)) continue;
  const b = readFileSync(`${LANE}/${rel}`);
  const arr = new Float32Array(b.buffer, b.byteOffset, Math.floor(b.byteLength / 4));
  let pk = 1e-6; for (let i = 0; i < arr.length; i++) if (arr[i] > pk) pk = arr[i];
  LANES.push({ ...d, buf: arr, peak: pk, rgb: hexToRgb(d.color) });
}
console.log(`▸ ${SLUG} spin (cute) · ${W}x${H} · ${DURATION.toFixed(1)}s · ${LANES.length} lanes · ${FRAMES} frames`);

// YWFT title
const TITLE_PALETTE = ["#7fe05a", "#ff5ab0", "#ffd400", "#4adfff", "#b07cff", "#ff8a3d", "#9cff2e", "#ffffff"];
const ptSize = Math.round(W * 0.086);
const { chars } = await prerenderTitleChars({
  text: TITLE, ptSize, palette: TITLE_PALETTE,
  shadowColor: "rgba(0,0,0,0.78)", assetsDir: `${LANE}/out/.${SLUG}-spinchars`,
});

// lyric words → YWFT pngs (conform; subtle — current word, soft fade)
const WORDS = `${LANE}/out/${SLUG}-fia-words.json`;
const VMAP  = `${LANE}/out/${SLUG}-vocal-map.json`;
let LW = [];
if (SHOW_LYRICS && existsSync(WORDS) && existsSync(VMAP)) {
  const raw = JSON.parse(readFileSync(WORDS, "utf8"));
  const m = JSON.parse(readFileSync(VMAP, "utf8"));
  // per-voice timebase maps (duet). fall back to the flat map = Voice A.
  const mapFor = (v) => (v === "B" && m.B) ? m.B
                      : (v === "A" && m.A) ? m.A
                      : { startSec: m.startSec || 0, stretch: m.stretch || 1 };
  const lyPt = Math.round(W * 0.115);   // MUCH larger — read it clearly
  const cache = new Map();
  for (const w of raw) {
    const txt = String(w.word || "").trim().toLowerCase();
    if (!txt) continue;
    const voice = w.voice === "B" ? "B" : "A";
    const mp = mapFor(voice);
    if (!cache.has(txt)) {
      cache.set(txt, await renderWordYWFT(txt, lyPt,
        `${LANE}/out/.${SLUG}-ly/${txt.replace(/[^a-z0-9]/g, "_") || "x"}.png`));
    }
    LW.push({ img: cache.get(txt), voice,
              t0: mp.startSec + w.start * mp.stretch,
              t1: mp.startSec + w.end * mp.stretch,
              line: Number.isFinite(w.line) ? w.line : 0 });
  }
}
// group karaoke words into lines, per VOICE (the duet shows two lines)
function buildKlines(voice) {
  const byLine = [];
  for (const w of LW) {
    if (w.voice !== voice) continue;
    (byLine[w.line] || (byLine[w.line] = [])).push(w);
  }
  return byLine.filter(Boolean).map((ws) => ({
    ws, t0: Math.min(...ws.map((w) => w.t0)), t1: Math.max(...ws.map((w) => w.t1)),
  }));
}
const KLINE_A = buildKlines("A");
const KLINE_B = buildKlines("B");

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const cover = await loadImage(BASE);
const vs = makeVerletString(ctx, { W, H, playheadX: Math.round(W / 2), duration: DURATION });
const Y0 = vs.NS_Y0, Y1 = vs.NS_Y1;
const laneY = LANES.map((_, i) => Y0 + (Y1 - Y0) * ((i + 0.7) / (LANES.length + 0.4)));
const LANE_H = Math.max(16, (Y1 - Y0) / (LANES.length + 1) * 0.42);
const PX_PER_SEC = W / 9;                            // faster: ~9s on screen
const PB_H = 22, PB_Y = H - PB_H;

// section index at a given time (shared by illy crossfade + timecode)
function sectionIndexAt(tt) {
  let idx = 0;
  for (let i = 0; i < sections.length; i++) if (tt >= sections[i].startSec) idx = i;
  return idx;
}

// per-section illustrations — Fía evolving section by section. Each
// section's own illy (out/<slug>-sec-N-<safe>.png); missing → the cover.
// We SLOW-crossfade across boundaries so she morphs, never jump-cuts.
const safeName = (n) => String(n).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
const secImgs = [];
for (let i = 0; i < sections.length; i++) {
  const p = `${LANE}/out/${SLUG}-sec-${i}-${safeName(sections[i].name)}.png`;
  secImgs.push(existsSync(p) ? await loadImage(p) : cover);
}
if (!secImgs.length) secImgs.push(cover);
const KB = { baseScale: 1.10, breathAmp: 0.03, breathPeriodSec: 34, env: 0, wobbleAmp: 3, envWobbleAmp: 0, punch: 0 };
const drawSecKB = (img, tt) => drawCoverKenBurns(ctx, img, tt, KB);
const CROSSFADE = 1.6;                               // slow soft morph (s)

// kawaii cat-morph STICKERS — one per section, pasted into the graphic
// layer (lower-left, above the bar), bobbing, crossfading like the illos.
const stickers = [];
for (let i = 0; i < sections.length; i++) {
  const sp = `${LANE}/out/stickers/sec${i}.png`;
  stickers.push(existsSync(sp) ? await loadImage(sp) : null);
}
const STK = Math.round(W * 0.26);

// ── timecode — inherited verbatim from the undabeach / waltz vertical:
// per-second YWFT plate, recolored to the current section tint (ramps
// toward white across the section) with a hard black drop-shadow plate.
const TC_PT = Math.round(W * 0.086);   // same size as the title
const tcCache = new Map();
{
  const totMm = Math.floor(DURATION / 60);
  const totSs = Math.floor(DURATION - totMm * 60).toString().padStart(2, "0");
  for (let s = 0; s <= Math.ceil(DURATION); s++) {
    const mm = Math.floor(s / 60), ss = (s - mm * 60).toString().padStart(2, "0");
    const key = `${mm}:${ss} / ${totMm}:${totSs}`;
    const safe = key.replace(/[^0-9]/g, "_");
    const img = await renderTcPlate(key, TC_PT, "#ffffff", `${LANE}/out/.${SLUG}-tc/${safe}.png`);
    const shadow = await renderTcPlate(key, TC_PT, "rgba(0,0,0,1)", `${LANE}/out/.${SLUG}-tc/${safe}.s.png`);
    tcCache.set(key, { img, shadow });
  }
}
const tcTint = createCanvas(8, 8);
const tcTintCtx = tcTint.getContext("2d");
function sectionTcRgb(tt) {
  const s = sections[sectionIndexAt(tt)];
  let [r, g, b] = tintOf(s?.name);
  const span = Math.max(0.001, (s?.endSec ?? DURATION) - (s?.startSec ?? 0));
  const lp = Math.max(0, Math.min(1, (tt - (s?.startSec ?? 0)) / span));
  const k = 0.30 * lp;                               // ramp toward white
  return [Math.round(r + (255 - r) * k), Math.round(g + (255 - g) * k), Math.round(b + (255 - b) * k)];
}
function drawTimecode(tt) {
  const totMm = Math.floor(DURATION / 60);
  const totSs = Math.floor(DURATION - totMm * 60).toString().padStart(2, "0");
  const sec = Math.min(Math.max(0, Math.floor(tt)), Math.ceil(DURATION));
  const mm = Math.floor(sec / 60), ss = (sec - mm * 60).toString().padStart(2, "0");
  const entry = tcCache.get(`${mm}:${ss} / ${totMm}:${totSs}`);
  if (!entry) return;
  const { img, shadow } = entry;
  const bounce = 16 * envAt(tt);
  const wobble = 2.2 * Math.sin(tt * 5.5);
  const x = W - img.width - 30;
  const y = PB_Y - img.height - 14 - bounce + wobble;
  ctx.save();
  ctx.globalAlpha = 0.95;
  ctx.drawImage(shadow, x + 3, y + 4);
  ctx.drawImage(shadow, x + 2, y + 3);
  ctx.globalAlpha = 1;
  const [tr, tg, tb] = sectionTcRgb(tt);
  tcTint.width = img.width; tcTint.height = img.height;
  tcTintCtx.clearRect(0, 0, img.width, img.height);
  tcTintCtx.globalCompositeOperation = "source-over";
  tcTintCtx.drawImage(img, 0, 0);
  tcTintCtx.globalCompositeOperation = "source-in";
  tcTintCtx.fillStyle = `rgb(${tr},${tg},${tb})`;
  tcTintCtx.fillRect(0, 0, img.width, img.height);
  ctx.drawImage(tcTint, x, y);
  ctx.restore();
}

function ampAt(L, t) {
  const i = Math.floor(t * laneSr);
  if (i < 0 || i >= L.buf.length) return 0;
  let p = 0;
  for (let s = Math.max(0, i - 30); s < Math.min(L.buf.length, i + 30); s++) if (L.buf[s] > p) p = L.buf[s];
  return p / L.peak;
}

const ff = spawnFFmpegEncode({ audioPath: AUDIO, w: W, h: H, fps: FPS, outPath: OUT });

for (let f = 0; f < FRAMES; f++) {
  const t = f / FPS;
  const e = envAt(t);

  // calm illustration — Fía evolving section by section, SLOW soft
  // crossfade across boundaries (no warp, no lurch, no jump-cut)
  const si = sectionIndexAt(t);
  const sStart = sections[si]?.startSec ?? 0;
  const cf = Math.max(0, Math.min(1, (t - sStart) / CROSSFADE));
  if (cf < 1 && si > 0) {
    ctx.globalAlpha = 1;
    drawSecKB(secImgs[si - 1] || cover, t);          // outgoing underneath
    ctx.globalAlpha = cf * cf * (3 - 2 * cf);        // smoothstep in
    drawSecKB(secImgs[si] || cover, t);
    ctx.globalAlpha = 1;
  } else {
    drawSecKB(secImgs[si] || cover, t);
  }

  // gentle plucks (subtle — soft amounts, no whip)
  for (let i = 0; i < LANES.length; i++) {
    const a = ampAt(LANES[i], t);
    if (a > 0.10) vs.pluck(laneY[i], a * (3.5 + 6 * a), i % 2 ? 1 : -1, LANES[i].rgb);
  }
  vs.step();   // NO vs.warp() — no distortion / stripping

  // top scrim for title, bottom scrim above the progress bar
  const gT = ctx.createLinearGradient(0, 0, 0, 240);
  gT.addColorStop(0, "rgba(0,0,0,0.42)"); gT.addColorStop(1, "rgba(0,0,0,0)");
  ctx.fillStyle = gT; ctx.fillRect(0, 0, W, 240);
  const gB = ctx.createLinearGradient(0, H - 200, 0, H - PB_H);
  gB.addColorStop(0, "rgba(0,0,0,0)"); gB.addColorStop(1, "rgba(0,0,0,0.5)");
  ctx.fillStyle = gB; ctx.fillRect(0, H - 200, W, 200 - PB_H);

  // rotating disc: soft activating CLIP blocks per lane + the string
  const halfSpan = (Math.hypot(W, H) / 2 + 80) / PX_PER_SEC;
  vs.withRotation(vs.trackTheta(t), () => {
    ctx.save();
    ctx.globalCompositeOperation = "screen";
    const BLK = 0.20;                                 // a touch more flow
    for (let li = 0; li < LANES.length; li++) {
      const L = LANES[li], cy = laneY[li], [r, g, b] = L.rgb;
      let tc = Math.floor((t - halfSpan) / BLK) * BLK;
      for (; tc < t + halfSpan; tc += BLK) {
        if (tc < 0 || tc > DURATION) continue;
        const amp = ampAt(L, tc);
        if (amp < 0.06) continue;
        const x = W / 2 + (tc - t) * PX_PER_SEC;
        const dt = t - tc;
        // more opaque overall, strongly BOOSTED right at the playhead
        let al;
        if (dt < 0) al = 0.22;                         // visible ahead
        else if (dt < 0.16) al = 0.95;                 // boosted when active
        else al = 0.34 + 0.22 * Math.max(0, 1 - (dt - 0.16) / 0.6);
        const hh = Math.max(2, amp * LANE_H * 0.92);
        ctx.fillStyle = `rgba(${r},${g},${b},${al.toFixed(3)})`;
        const bw = BLK * PX_PER_SEC - 3;
        ctx.fillRect(x - bw / 2, cy - hh / 2, bw, hh);
      }
    }
    ctx.restore();
    vs.draw();
  });

  // subtle YWFT title
  drawTitleBounce(ctx, {
    chars, ptSize, baseX: 44, baseY: Math.round(ptSize * 1.9),
    audioT: t, env: e, getEnvAt: envAt,
    charDelay: 0.03, bounceAmp: 30, restAlpha: 0.68, glowThreshold: 0.55, glowMax: 12, wobbleAmp: 2,
  });

  // KARAOKE — ONE tight SCROLLING row (minimal vertical space). the
  // line slides so the word being sung stays centred; words light up as
  // sung, the current word pops + glows. legible cute font, not script.
  const drawKaraoke = (KL, yFrac, sizeMul, glow) => {
    if (!KL.length) return;
    let li = -1;
    for (let k = 0; k < KL.length; k++) {
      if (t >= KL[k].t0 - 0.5 && t <= KL[k].t1 + 1.1) { li = k; break; }
    }
    if (li < 0) return;
    const L = KL[li], ws = L.ws;
    const wordH = Math.max(...ws.map((w) => w.img.height)) * sizeMul;
    const gp = Math.round(wordH * 0.10);                 // TIGHT spacing
    // lay the whole line on one row; record each word's centre x.
    const cx = []; let x = 0;
    for (let i = 0; i < ws.length; i++) {
      const iw = ws[i].img.width * sizeMul;
      cx.push(x + iw / 2);
      x += iw + gp;
    }
    // smooth scroll: interpolate the focus x between word centres by time.
    let focus = cx[0];
    if (t <= ws[0].t0) focus = cx[0];
    else if (t >= ws[ws.length - 1].t1) focus = cx[ws.length - 1];
    else for (let i = 0; i < ws.length; i++) {
      if (t >= ws[i].t0 && (i === ws.length - 1 || t < ws[i + 1].t0)) {
        const span = Math.max(0.05, (ws[i + 1]?.t0 ?? ws[i].t1) - ws[i].t0);
        const f = Math.max(0, Math.min(1, (t - ws[i].t0) / span));
        focus = cx[i] + (((cx[i + 1] ?? cx[i]) - cx[i]) * f);
        break;
      }
    }
    const scrollX = W / 2 - focus;
    const lineFade = Math.max(0, Math.min(1,
      Math.min((t - (L.t0 - 0.5)) * 3, ((L.t1 + 1.1) - t) * 2)));
    const cyRow = Math.round(H * yFrac);
    x = 0;
    for (let i = 0; i < ws.length; i++) {
      const w = ws[i];
      const iw = w.img.width * sizeMul, ih = w.img.height * sizeMul;
      const px = x + scrollX;
      x += iw + gp;
      if (px + iw < -20 || px > W + 20) continue;         // cull off-screen
      const sung = t >= w.t0 - 0.04;
      const isCur = t >= w.t0 - 0.04 && t <= w.t1 + 0.22;
      const pop = isCur ? 1.18 : 1.0;
      const a = (sung ? 1.0 : 0.28) * lineFade;
      ctx.save();
      ctx.globalAlpha = a;
      if (isCur) { ctx.shadowColor = glow; ctx.shadowBlur = 28; }
      const dw = iw * pop, dh = ih * pop;
      ctx.drawImage(w.img, Math.round(px + iw / 2 - dw / 2),
                    Math.round(cyRow - dh / 2), Math.round(dw), Math.round(dh));
      ctx.restore();
    }
    ctx.globalAlpha = 1;
  };
  // single voice (sequential verses) — one tight scrolling line, low.
  drawKaraoke(KLINE_B, 0.50, 1.0, "rgba(120,225,255,0.9)");
  drawKaraoke(KLINE_A, 0.80, 1.0, "rgba(255,120,200,0.9)");

  // kawaii cat-morph sticker — a DIFFERENT spot each segment, ANIMATED:
  // pops in, bobs, wiggles, and pulses to the beat; crossfades (hops)
  // from the previous spot to the new one at each section boundary.
  {
    // safe anchors (centre of the sticker, as W/H fractions) — kept off
    // the face, the title (top-left), the timecode (btm-right), the
    // karaoke line (~0.785H) and the progress bar.
    const SPOTS = [
      [0.165, 0.70],  // sec0 lower-left
      [0.825, 0.34],  // sec1 right-mid
      [0.820, 0.165], // sec2 upper-right
      [0.165, 0.40],  // sec3 left-mid
      [0.830, 0.205], // sec4 upper-right
      [0.170, 0.70],  // sec5 lower-left
    ];
    const ssi = sectionIndexAt(t);
    const ss = sections[ssi];
    const tss = t - (ss?.startSec ?? 0);
    const beat = 1 + 0.12 * envAt(t);                    // pulse to the music
    const hop = Math.max(0, Math.min(1, tss / 0.5));     // snappy sticker hop
    const drawStk = (img, a, idx, fresh) => {
      if (!img || a <= 0.001) return;
      const [fx, fy] = SPOTS[idx % SPOTS.length];
      const cx = fx * W, cy = fy * H;
      const appear = fresh ? hop : 1;                     // incoming pops; old stays full
      const overshoot = fresh ? 1 + 0.22 * Math.sin(Math.min(1, tss / 0.42) * Math.PI) : 1;
      const bob = Math.sin(t * 2.4 + idx) * (STK * 0.045);
      const wob = Math.sin(t * 3.1 + idx * 1.7) * 0.085;  // wiggle
      const sz = STK * (0.5 + 0.5 * appear) * overshoot * beat;
      ctx.save();
      ctx.globalAlpha = Math.min(1, a);
      ctx.translate(cx, cy + bob);
      ctx.rotate(wob);
      ctx.drawImage(img, -sz / 2, -sz / 2, sz, sz);
      ctx.restore();
    };
    if (hop < 1 && ssi > 0) {
      drawStk(stickers[ssi - 1], 1 - hop, ssi - 1, false); // old hops out (full size)
      drawStk(stickers[ssi], hop, ssi, true);              // new pops in
    } else {
      drawStk(stickers[ssi], 1, ssi, true);
    }
  }

  // canonical SECTIONED progress bar — colored, no labels
  ctx.save();
  const playedX = Math.max(0, t) / DURATION * W;
  for (let pi = 0; pi < sections.length; pi++) {
    const s = sections[pi];
    const x0 = (s.startSec / DURATION) * W;
    const x1 = pi === sections.length - 1 ? W : (s.endSec / DURATION) * W;
    const [r, g, b] = tintOf(s.name);
    ctx.fillStyle = `rgba(${Math.round(r * 0.18)},${Math.round(g * 0.18)},${Math.round(b * 0.18)},0.85)`;
    ctx.fillRect(x0, PB_Y, x1 - x0, PB_H);
    const fx = Math.min(x1, playedX);
    if (fx > x0) { ctx.fillStyle = `rgba(${r},${g},${b},0.96)`; ctx.fillRect(x0, PB_Y, fx - x0, PB_H); }
    ctx.fillStyle = "rgba(255,253,242,0.30)";
    ctx.fillRect(x1 - 1, PB_Y, 1, PB_H);
  }
  ctx.restore();

  // YWFT timecode — bottom-right, above the bar (waltz/undabeach style)
  drawTimecode(t);

  const buf = canvas.toBuffer("raw");
  if (!ff.stdin.write(buf)) await once(ff.stdin, "drain");
  if (f % 150 === 0) process.stdout.write(`\r  frame ${f}/${FRAMES}`);
}
ff.stdin.end();
const [code] = await once(ff, "close");
process.stdout.write("\r");
if (code !== 0) { console.error(`✗ ffmpeg exit ${code}`); process.exit(1); }

let dest = OUT;
if (flags.desktop) { dest = resolve(homedir(), "Desktop", `${SLUG}-preview-spin${TAG}.mp4`); copyFileSync(OUT, dest); }
console.log(`✓ ${dest.replace(REPO + "/", "")}`);
