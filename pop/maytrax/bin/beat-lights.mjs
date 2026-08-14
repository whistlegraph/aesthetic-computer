#!/usr/bin/env node
// beat-lights.mjs — the neon rig and the grade, driven by the score itself.
//
// render-femrag-plusplus.mjs writes every musical hit it plays to
// out/<slug>.events.json (kick, snare, hat, bell, sub, donk, riser, with
// exact times and gains). This reads that feed and builds the light — no
// model, no API, no cost, and exactly in time, because it reads the same
// event list the audio was rendered from rather than guessing at it with a
// beat detector.
//
// THREE-LAYER SEPARATION, the marimbaba/trancenwaltz treatment
// (pop/marimba/bin/motion-score-marimbaba-yt.mjs). Light stacked in three
// passes so the subject reads BACKLIT rather than washed over:
//   1. BEHIND — a coloured bloom rising from behind the figure [screen],
//      hue-cycling, punched by the kick and swelled by the sub.
//   2. CONTRAST — a vignette darkening the periphery [multiply] that
//      BREATHES with the beat, so the middle separates from the edges.
//   3. RIM — a tight hot halo hugging the centre mass [screen], snapped by
//      the snare, which is what actually reads as separation.
// Layer 2 is the one that does the work: without it the glow sits ON the
// picture; with it the picture sits INSIDE the glow.
//
// Also: HUE SHIFTING across the whole grade, and a PIXEL-DISSOLVE punch on
// every section boundary read from <slug>.struct.json.
//
//   node pop/maytrax/bin/beat-lights.mjs --over <cut.mp4>
//   node pop/maytrax/bin/beat-lights.mjs --over <cut.mp4> --dur 60 --opacity .5
//   node pop/maytrax/bin/beat-lights.mjs --over <cut.mp4> --no-pixel

import { existsSync, readFileSync } from "node:fs";
import { spawn, spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
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

const FPS = 24;
const W = 320, H = 180;            // soft light upscales cleanly
const OPACITY = Number(flags.opacity ?? .55);
const FROM = Number(flags.from ?? 0);
const PIXEL = flags["no-pixel"] !== true;

const eventsPath = `${OUT}/${SLUG}.events.json`;
if (!existsSync(eventsPath)) {
  console.error(`✗ no event feed at ${eventsPath} — run the renderer first`);
  process.exit(1);
}
const feed = JSON.parse(readFileSync(eventsPath, "utf8"));
const DUR = Number(flags.dur ?? feed.seconds);
const FRAMES = Math.ceil(DUR * FPS);

// ── the rig ────────────────────────────────────────────────────────────────
const LAMPS = {
  boom:  { rgb: [255, 150,  40], from: "bottom", spread: .95, decay: .20, gain: 5.2 },
  snare: { rgb: [255,  60, 190], from: "sides",  spread: .85, decay: .16, gain: 5.0 },
  donk:  { rgb: [120, 255,  90], from: "left",   spread: .90, decay: .13, gain: 5.6 },
  sub:   { rgb: [ 90, 120, 255], from: "bottom", spread: 1.1, decay: .30, gain: 2.0 },
  hat:   { rgb: [140, 235, 255], from: "top",    spread: .55, decay: .07, gain: 3.0 },
  bell:  { rgb: [255, 225, 150], from: "top",    spread: .45, decay: .09, gain: 1.1 },
  riser: { rgb: [255, 255, 255], from: "all",    spread: 1.3, decay: .50, gain: 1.6 },
};
// Which instruments drive which of the three layers.
const BEHIND = new Set(["boom", "sub", "riser"]);
const RIM = new Set(["snare", "donk"]);

const perFrame = Array.from({ length: FRAMES }, () => []);
for (const e of feed.events) {
  const lamp = LAMPS[e.i];
  if (!lamp) continue;
  const span = e.i === "riser" ? (e.dur || 0) : 0;
  const start = Math.floor((e.t - FROM) * FPS);
  const end = Math.ceil((e.t - FROM + span + lamp.decay * 3) * FPS);
  for (let f = Math.max(0, start); f < Math.min(FRAMES, end); f++) perFrame[f].push(e);
}

// Distance fields, precomputed so the inner loop is an envelope multiply.
// `centre` is radial from the middle — the behind-bloom and the rim both
// key off it, which is what puts the light around the figure.
const FIELD = {};
for (const key of [...new Set(Object.values(LAMPS).map((l) => l.from)), "centre"]) {
  const f = new Float32Array(W * H);
  for (let y = 0; y < H; y++) {
    for (let x = 0; x < W; x++) {
      const u = x / (W - 1), v = y / (H - 1);
      let d;
      switch (key) {
        case "bottom": d = 1 - v; break;
        case "top": d = v; break;
        case "left": d = u; break;
        case "sides": d = Math.min(u, 1 - u) * 2; break;
        case "centre": d = Math.hypot((u - .5) * 2, (v - .52) * 2); break;
        default: d = Math.min(Math.min(u, 1 - u), Math.min(v, 1 - v)) * 2;
      }
      f[y * W + x] = d;
    }
  }
  FIELD[key] = f;
}

// Slow hue rotation over the whole piece, the trancenwaltz/marimbaba trick.
function hueSpin(t) {
  const a = (t / DUR) * Math.PI * 2 * 3;        // three cycles across the cut
  return [Math.cos(a), Math.cos(a - 2.094), Math.cos(a - 4.189)].map((c) => .72 + .28 * c);
}

const buf = Buffer.alloc(W * H * 3);
const acc = new Float32Array(W * H * 3);

// layer: "glow" (screen: behind + rim) or "vign" (multiply: contrast)
function renderFrame(fi, layer) {
  const t = fi / FPS + FROM;
  if (layer === "vign") {
    // Layer 2 — the contrast pass. Dark at the edges, open in the middle,
    // and the squeeze BREATHES with whatever is hitting.
    let punch = 0;
    for (const e of perFrame[fi]) {
      if (!BEHIND.has(e.i) && !RIM.has(e.i)) continue;
      const lamp = LAMPS[e.i];
      punch += Math.exp(-Math.max(0, t - e.t) / lamp.decay) * (e.gain ?? .1) * 3.2;
    }
    punch = Math.min(1, punch);
    const centre = FIELD.centre;
    for (let p = 0, n = W * H; p < n; p++) {
      const d = centre[p];
      // 1 in the middle → falls off outward; deeper falloff on a hit.
      const k = Math.max(0, 1 - Math.pow(d / (1.32 - .22 * punch), 2.1));
      const v = 70 + 185 * k;                    // never crush fully to black
      const o = p * 3;
      buf[o] = buf[o + 1] = buf[o + 2] = v;
    }
    return buf;
  }
  // Layers 1 + 3 — behind-bloom and rim, both additive.
  acc.fill(0);
  const spin = hueSpin(t);
  for (const e of perFrame[fi]) {
    const lamp = LAMPS[e.i];
    const span = e.i === "riser" ? (e.dur || 0) : 0;
    const since = t - e.t;
    if (since < 0) continue;
    const env = Math.exp(-Math.max(0, since - span) / lamp.decay);
    const amp = env * (e.gain ?? .1) * lamp.gain;
    if (amp < .004) continue;
    const [r, g, b] = lamp.rgb;
    // edge wash — the rig in the rafters
    const field = FIELD[lamp.from];
    for (let p = 0, n = W * H; p < n; p++) {
      const fall = Math.max(0, 1 - field[p] / lamp.spread);
      const k = fall * fall * amp * .8;
      if (k < .002) continue;
      const o = p * 3;
      acc[o] += r * k * spin[0]; acc[o + 1] += g * k * spin[1]; acc[o + 2] += b * k * spin[2];
    }
    // layer 1 — bloom rising from BEHIND the figure
    if (BEHIND.has(e.i)) {
      const c = FIELD.centre;
      for (let p = 0, n = W * H; p < n; p++) {
        const k = Math.max(0, 1 - c[p] / .95) ** 2.4 * amp * .55;
        if (k < .002) continue;
        const o = p * 3;
        acc[o] += r * k * spin[0]; acc[o + 1] += g * k * spin[1]; acc[o + 2] += b * k * spin[2];
      }
    }
    // layer 3 — tight rim hugging the centre mass
    if (RIM.has(e.i)) {
      const c = FIELD.centre;
      for (let p = 0, n = W * H; p < n; p++) {
        const ring = 1 - Math.min(1, Math.abs(c[p] - .58) / .17);
        if (ring <= 0) continue;
        const k = ring * ring * amp * .5;
        const o = p * 3;
        acc[o] += r * k * spin[0]; acc[o + 1] += g * k * spin[1]; acc[o + 2] += b * k * spin[2];
      }
    }
  }
  for (let i = 0, n = W * H * 3; i < n; i++) {
    const v = acc[i];
    buf[i] = v >= 255 ? 255 : v <= 0 ? 0 : v;
  }
  return buf;
}

async function bake(layer, path) {
  const ff = spawn("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y",
    "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "pipe:0",
    "-vf", `scale=1280:720:flags=bicubic,gblur=sigma=${layer === "vign" ? 10 : 6}`,
    "-c:v", "libx264", "-preset", "veryfast", "-crf", "20", "-pix_fmt", "yuv420p", path,
  ], { stdio: ["pipe", "inherit", "inherit"] });
  const closed = new Promise((r) => ff.on("close", r));
  for (let f = 0; f < FRAMES; f++) {
    if (!ff.stdin.write(Buffer.from(renderFrame(f, layer)))) {
      await new Promise((r) => ff.stdin.once("drain", r));
    }
  }
  ff.stdin.end();
  if (await closed !== 0) { console.error(`✗ ${layer} encode failed`); process.exit(1); }
  console.log(`  ✓ layer ${layer}`);
}

const glowPath = `${OUT}/${SLUG}-beat-glow.mp4`;
const vignPath = `${OUT}/${SLUG}-beat-vign.mp4`;
console.log(`▸ beat-lights · ${FRAMES} frames · ${DUR.toFixed(1)}s · 3-layer separation`);
await bake("glow", glowPath);
await bake("vign", vignPath);

// ── composite ──────────────────────────────────────────────────────────────
const over = flags.over === true ? `${OUT}/${SLUG}-shakeout-yt.mp4` : flags.over;
if (!over) { console.log("  (pass --over <cut.mp4> to grade a cut)"); process.exit(0); }
if (!existsSync(over)) { console.error(`✗ no cut at ${over}`); process.exit(1); }

const probe = spawnSync("ffprobe", ["-v", "error", "-select_streams", "v:0",
  "-show_entries", "stream=width,height", "-of", "csv=p=0:s=x", over], { encoding: "utf8" });
const [vw, vh] = (probe.stdout || "1280x720").trim().split("x").map(Number);

// PIXEL-DISSOLVE punch on each section boundary, read from the struct the
// cut was actually built from — a short pixelize burst, marimbaba-style.
let pixExpr = "";
const structPath = flags.struct || `${OUT}/${SLUG}.struct.json`;
if (PIXEL && existsSync(structPath)) {
  const st = JSON.parse(readFileSync(structPath, "utf8"));
  const cuts = st.sections.map((s) => s.startSec - FROM)
    .filter((t) => t > .2 && t < DUR - .2);
  if (cuts.length) {
    pixExpr = cuts.map((t) => `between(t,${t.toFixed(2)},${(t + .18).toFixed(2)})`).join("+");
    console.log(`  pixel-dissolve on ${cuts.length} boundaries`);
  }
}
const fit = `scale=${vw}:${vh}:force_original_aspect_ratio=increase,crop=${vw}:${vh}`;
const chain = [
  `[1:v]${fit},format=yuv420p[g]`,
  `[2:v]${fit},format=yuv420p[v]`,
  // layer 2 first (multiply) so the picture sits INSIDE the light,
  // then layer 1+3 (screen) on top.
  `[0:v][v]blend=all_mode=multiply:all_opacity=0.9[base]`,
  `[base][g]blend=all_mode=screen:all_opacity=${OPACITY}` +
    (pixExpr ? `[lit];[lit]pixelize=w=24:h=24:enable='${pixExpr}'[out]` : `[out]`),
].join(";");

const litPath = over.replace(/\.mp4$/, "-lit.mp4");
console.log(`  grading ${over.split("/").pop()} · ${vw}x${vh} · opacity ${OPACITY} …`);
const mix = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", over, "-i", glowPath, "-i", vignPath,
  "-filter_complex", chain,
  "-map", "[out]", "-map", "0:a?",
  "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
  "-c:a", "copy", "-movflags", "+faststart", litPath,
], { stdio: ["ignore", "inherit", "inherit"] });
if (mix.status !== 0) { console.error("✗ grade failed"); process.exit(1); }
console.log(`✓ ${litPath}`);
