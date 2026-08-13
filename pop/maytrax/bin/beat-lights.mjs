#!/usr/bin/env node
// beat-lights.mjs — the neon rig, driven by the score itself.
//
// render-femrag-plusplus.mjs writes every musical hit it plays to
// out/<slug>.events.json (kick, snare, hat, bell, sub, donk, riser, with
// exact times and gains). This reads that feed and paints a light-overlay
// video: colored pools that BLAST from the frame edges the way the felted
// neon tubes in the rafters would if they were wired to the mixer.
//
// It is locally generated — no model, no API, no cost — and it is exactly
// in time because it is reading the same event list the audio was rendered
// from, not a beat-detector's guess about it.
//
// The overlay is rendered small (the light is all soft gradients, so it
// upscales cleanly) and streamed to ffmpeg as raw video, then screen-blended
// over the cut. Nothing large ever touches the disk.
//
//   node pop/maytrax/bin/beat-lights.mjs                      # overlay only
//   node pop/maytrax/bin/beat-lights.mjs --over <cut.mp4>     # + composite
//   node pop/maytrax/bin/beat-lights.mjs --opacity .5

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
const W = 320, H = 180;            // overlay resolution — soft light upscales fine
const OPACITY = Number(flags.opacity ?? .55);
// --from trims the overlay to a slice of the track (for the reel cut).
const FROM = Number(flags.from ?? 0);

const eventsPath = `${OUT}/${SLUG}.events.json`;
if (!existsSync(eventsPath)) {
  console.error(`✗ no event feed at ${eventsPath} — run the renderer first`);
  process.exit(1);
}
const feed = JSON.parse(readFileSync(eventsPath, "utf8"));
const DUR = Number(flags.dur ?? feed.seconds) ;
const FRAMES = Math.ceil(DUR * FPS);

// ── the rig ────────────────────────────────────────────────────────────────
// Each instrument owns a lamp: a color, a place in the frame, and a decay.
// `spread` is how far the pool reaches from its anchor (in frame heights),
// `decay` how many seconds the flash takes to fall away.
const LAMPS = {
  boom:  { rgb: [255, 150,  40], from: "bottom", spread: .95, decay: .20, gain: 5.2 },
  snare: { rgb: [255,  60, 190], from: "sides",  spread: .85, decay: .16, gain: 5.0 },
  donk:  { rgb: [120, 255,  90], from: "left",   spread: .90, decay: .13, gain: 5.6 },
  sub:   { rgb: [ 90, 120, 255], from: "bottom", spread: 1.1, decay: .30, gain: 2.0 },
  hat:   { rgb: [140, 235, 255], from: "top",    spread: .55, decay: .07, gain: 3.0 },
  bell:  { rgb: [255, 225, 150], from: "top",    spread: .45, decay: .09, gain: 1.1 },
  riser: { rgb: [255, 255, 255], from: "all",    spread: 1.3, decay: .50, gain: 1.6 },
};

// Bucket events into frames so each frame only walks its own recent hits.
// A hit can still light frames after it (its decay tail), so each event is
// filed into every frame its tail reaches.
const perFrame = Array.from({ length: FRAMES }, () => []);
let placed = 0;
for (const e of feed.events) {
  const lamp = LAMPS[e.i];
  if (!lamp) continue;
  // A riser lights its whole duration, not just its onset.
  const span = e.i === "riser" ? (e.dur || 0) : 0;
  const start = Math.floor(e.t * FPS);
  const end = Math.ceil((e.t + span + lamp.decay * 3) * FPS);
  for (let f = Math.max(0, start); f < Math.min(FRAMES, end); f++) {
    perFrame[f].push(e);
  }
  placed++;
}

// Precompute each pixel's distance from each anchor, so the per-frame work is
// just an envelope multiply rather than a distance calculation.
const FIELD = {};
for (const key of new Set(Object.values(LAMPS).map((l) => l.from))) {
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
        default: d = Math.min(Math.min(u, 1 - u), Math.min(v, 1 - v)) * 2; break;
      }
      f[y * W + x] = d;
    }
  }
  FIELD[key] = f;
}

const frame = Buffer.alloc(W * H * 3);
const acc = new Float32Array(W * H * 3);

function renderFrame(fi) {
  acc.fill(0);
  const t = fi / FPS + FROM;
  for (const e of perFrame[fi]) {
    const lamp = LAMPS[e.i];
    const span = e.i === "riser" ? (e.dur || 0) : 0;
    const since = t - e.t;
    if (since < 0) continue;
    // Inside a sustained event the envelope holds at 1; after it, it decays.
    const past = Math.max(0, since - span);
    const env = Math.exp(-past / lamp.decay);
    const amp = env * (e.gain ?? .1) * lamp.gain;
    if (amp < .004) continue;
    const field = FIELD[lamp.from];
    const [r, g, b] = lamp.rgb;
    for (let p = 0, n = W * H; p < n; p++) {
      // Light falls off with distance from the lamp's edge.
      const fall = Math.max(0, 1 - field[p] / lamp.spread);
      const k = fall * fall * amp;
      if (k < .002) continue;
      const o = p * 3;
      acc[o] += r * k; acc[o + 1] += g * k; acc[o + 2] += b * k;
    }
  }
  for (let i = 0, n = W * H * 3; i < n; i++) {
    const v = acc[i];
    frame[i] = v >= 255 ? 255 : v <= 0 ? 0 : v; // implicit floor
  }
  return frame;
}

const overlayPath = `${OUT}/${SLUG}-beat-lights.mp4`;
console.log(`▸ beat-lights · ${FRAMES} frames · ${DUR.toFixed(1)}s · ${placed} events wired`);

const ff = spawn("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-f", "rawvideo", "-pix_fmt", "rgb24", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "pipe:0",
  "-vf", "scale=1280:720:flags=bicubic,gblur=sigma=6",
  "-c:v", "libx264", "-preset", "veryfast", "-crf", "20", "-pix_fmt", "yuv420p",
  overlayPath,
], { stdio: ["pipe", "inherit", "inherit"] });

let done;
const finished = new Promise((r) => { done = r; });
ff.on("close", (code) => done(code));

for (let f = 0; f < FRAMES; f++) {
  if (!ff.stdin.write(Buffer.from(renderFrame(f)))) {
    await new Promise((r) => ff.stdin.once("drain", r));
  }
  if (f % 480 === 0) process.stdout.write(`\r  ${Math.round(f / FRAMES * 100)}%   `);
}
ff.stdin.end();
const code = await finished;
process.stdout.write("\r");
if (code !== 0) { console.error("✗ overlay encode failed"); process.exit(1); }
console.log(`✓ ${overlayPath}`);

// ── composite ──────────────────────────────────────────────────────────────
const over = flags.over === true ? `${OUT}/${SLUG}-shakeout-yt.mp4` : flags.over;
if (!over) {
  console.log(`  (pass --over <cut.mp4> to screen-blend it onto a cut)`);
  process.exit(0);
}
if (!existsSync(over)) { console.error(`✗ no cut at ${over}`); process.exit(1); }
const litPath = over.replace(/\.mp4$/, "-lit.mp4");
console.log(`  compositing over ${over.split("/").pop()} at opacity ${OPACITY} …`);
// Match the overlay to whatever frame the cut actually is — the same rig
// serves the landscape film and the 9:16 reel.
const probe = spawnSync("ffprobe", ["-v", "error", "-select_streams", "v:0",
  "-show_entries", "stream=width,height", "-of", "csv=p=0:s=x", over], { encoding: "utf8" });
const [vw, vh] = (probe.stdout || "1280x720").trim().split("x").map(Number);
console.log(`  target frame ${vw}x${vh}`);
const mix = spawnSync("ffmpeg", [
  "-hide_banner", "-loglevel", "error", "-y",
  "-i", over, "-i", overlayPath,
  "-filter_complex",
  `[1:v]scale=${vw}:${vh}:force_original_aspect_ratio=increase,crop=${vw}:${vh},format=yuv420p[o];[0:v][o]blend=all_mode=screen:all_opacity=${OPACITY}[v]`,
  "-map", "[v]", "-map", "0:a?",
  "-c:v", "libx264", "-preset", "medium", "-crf", "18", "-pix_fmt", "yuv420p",
  "-c:a", "copy", "-movflags", "+faststart", litPath,
], { stdio: ["ignore", "inherit", "inherit"] });
if (mix.status !== 0) { console.error("✗ composite failed"); process.exit(1); }
console.log(`✓ ${litPath}`);
