#!/usr/bin/env node
// maytrax/bin/beat-align.mjs — find each clip's visual downbeats and slide
// the clip so they land on the track's real ones.
//
// A Seedance take has its own internal rhythm: landings, direction changes,
// paw strikes. Those are IMPACTS — frames where inter-frame motion spikes.
// The generated clip runs longer than its section (ceil(exact)+crossfade),
// so there is SLACK: a choice of where inside the take the section's window
// sits. This measures per-frame motion for every picked take, then chooses
// the head-offset whose impacts line up best with the kicks inside that
// section — read from the renderer's own events.json, never detected.
//
//   node pop/maytrax/bin/beat-align.mjs            # analyze + write offsets
//   node pop/maytrax/bin/beat-align.mjs --chart <video>   # energy chart PNG
//
// The offsets land in out/reel/motion/offsets.json; the assembly step in
// pop/lib/motion-pipeline.mjs honours them on cut-shots.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { createCanvas } from "canvas";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const SLUG = "femrag-plusplus";
const MOTION_DIR = `${OUT}/reel/motion`;
const FPS = 24;
const FROM = 26.67;                    // reel slice offset into the track
const XF = 60 / 144 / 2;               // the assembly's eighth-note crossfade

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}

// Per-frame motion: decode small + gray, mean |Δ| between frames. The
// POSITIVE derivative of that curve is the impact signal — energy arriving.
function motionCurve(path) {
  const AW = 160, AH = 90;
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error",
    "-i", path, "-f", "rawvideo", "-pix_fmt", "gray", "-s", `${AW}x${AH}`,
    "-r", String(FPS), "pipe:1"],
    { maxBuffer: 1 << 30 });
  if (r.status !== 0 || !r.stdout?.length) return null;
  const raw = r.stdout, fb = AW * AH, n = Math.floor(raw.length / fb);
  const curve = new Float32Array(n);
  for (let f = 1; f < n; f++) {
    let acc = 0;
    const a = f * fb, b = (f - 1) * fb;
    for (let p = 0; p < fb; p += 2) acc += Math.abs(raw[a + p] - raw[b + p]);
    curve[f] = acc / (fb / 2) / 255;
  }
  return curve;
}
const impacts = (curve) => {
  const d = new Float32Array(curve.length);
  for (let i = 1; i < curve.length; i++) d[i] = Math.max(0, curve[i] - curve[i - 1]);
  return d;
};

const struct = JSON.parse(readFileSync(`${OUT}/reel/${SLUG}-reel.struct.json`, "utf8"));
const feed = JSON.parse(readFileSync(`${OUT}/${SLUG}.events.json`, "utf8"));
const shots = JSON.parse(readFileSync(`${MOTION_DIR}/shots.json`, "utf8"));
const takes = existsSync(`${MOTION_DIR}/takes.json`)
  ? JSON.parse(readFileSync(`${MOTION_DIR}/takes.json`, "utf8")) : {};

const clipSeconds = (p) => Number(spawnSync("ffprobe", ["-v", "error",
  "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", p],
  { encoding: "utf8" }).stdout?.trim()) || 0;

// ── chart mode — motion energy of a finished cut vs the score ────────────
if (flags.chart) {
  const video = flags.chart === true ? `${OUT}/${SLUG}-shakeout-reel-final.mp4` : flags.chart;
  const curve = motionCurve(video);
  if (!curve) { console.error("✗ could not read " + video); process.exit(1); }
  const N = curve.length;
  // the score's energy at each frame: decaying envelope over all hits
  const DECAY = { boom: .2, snare: .16, donk: .13, sub: .3, hat: .07, bell: .09, riser: .5 };
  const music = new Float32Array(N);
  for (const e of feed.events) {
    const d = DECAY[e.i]; if (!d) continue;
    const f0 = Math.max(0, Math.floor((e.t - FROM) * FPS));
    const f1 = Math.min(N, Math.ceil((e.t - FROM + d * 3) * FPS));
    for (let f = f0; f < f1; f++) {
      music[f] += Math.exp(-Math.max(0, f / FPS + FROM - e.t) / d) * (e.gain ?? .1);
    }
  }
  const kicks = feed.events.filter((e) => e.i === "boom")
    .map((e) => e.t - FROM).filter((t) => t >= 0 && t <= N / FPS);

  // Two stacked panels sharing one x — never a dual axis. Palette
  // validated: #d97706 (music) / #2563eb (motion) on white.
  const CW = 1400, CH = 620, PAD = 56, PH = 210, GAP = 56;
  const c = createCanvas(CW, CH), x = c.getContext("2d");
  x.fillStyle = "#ffffff"; x.fillRect(0, 0, CW, CH);
  const plotW = CW - PAD * 2;
  const tx = (f) => PAD + (f / N) * plotW;
  const panels = [
    { y0: PAD, data: music, col: "#d97706", label: "music energy (events.json)" },
    { y0: PAD + PH + GAP, data: curve, col: "#2563eb", label: "frame motion (mean |Δ|)" },
  ];
  x.font = "13px sans-serif";
  for (const p of panels) {
    const max = Math.max(...p.data) || 1;
    // section bands + boundaries (recessive)
    for (const s of struct.sections) {
      x.fillStyle = "rgba(0,0,0,.03)";
      if (struct.sections.indexOf(s) % 2) x.fillRect(tx(s.startSec * FPS), p.y0, (s.endSec - s.startSec) * FPS / N * plotW, PH);
      x.fillStyle = "#6b7280";
      x.fillText(s.name, tx(s.startSec * FPS) + 4, p.y0 + 14);
    }
    // kick downbeat ticks
    x.strokeStyle = "rgba(0,0,0,.16)";
    for (const k of kicks) {
      x.beginPath(); x.moveTo(tx(k * FPS), p.y0 + PH - 26); x.lineTo(tx(k * FPS), p.y0 + PH); x.stroke();
    }
    // the curve
    x.strokeStyle = p.col; x.lineWidth = 2; x.beginPath();
    for (let f = 0; f < N; f++) {
      const yy = p.y0 + PH - (p.data[f] / max) * (PH - 24);
      f ? x.lineTo(tx(f), yy) : x.moveTo(tx(f), yy);
    }
    x.stroke();
    x.fillStyle = "#111827";
    x.fillText(p.label, PAD, p.y0 - 8);
    x.lineWidth = 1;
  }
  // shared x axis: seconds
  x.fillStyle = "#6b7280";
  for (let sec = 0; sec <= N / FPS; sec += 5) {
    x.fillText(`${sec}s`, tx(sec * FPS) - 8, CH - 18);
  }
  x.fillStyle = "#111827"; x.font = "15px sans-serif";
  x.fillText("SHAKEOUT reel — where the energy is (ticks = kicks from the score)", PAD, 24);
  const chartPath = `${MOTION_DIR}/energy-chart.png`;
  writeFileSync(chartPath, c.toBuffer("image/png"));
  console.log(`✓ ${chartPath}`);
  if (flags.open) spawnSync("open", [chartPath]);
  process.exit(0);
}

// ── align mode — pick each cut-shot's head offset inside its slack ───────
const offsets = {};
for (const s of shots) {
  const sec = struct.sections.find((x) => x.name === s.name);
  if (!sec) continue;
  const shotPath = `${MOTION_DIR}/${SLUG}-reel-shot-${s.i}-${s.name}.mp4`;
  const picked = takes[s.name] ? resolve(MOTION_DIR, takes[s.name]) : shotPath;
  if (!existsSync(picked)) { console.log(`  ○ ${s.name}: no take`); continue; }
  if (s.endImage) { console.log(`  ○ ${s.name}: morph — keeps its arrival`); continue; }
  const clipLen = clipSeconds(picked);
  const need = (sec.endSec - sec.startSec) + XF;
  const slack = clipLen - need;
  if (slack < 1 / FPS) { console.log(`  ○ ${s.name}: no slack (${slack.toFixed(2)}s)`); continue; }
  const curve = motionCurve(picked);
  if (!curve) { console.log(`  ○ ${s.name}: unreadable`); continue; }
  const imp = impacts(curve);
  // the section's kicks, in section-local time, weighted by their gain —
  // downbeats (bar starts) hit harder in the score, so they weigh more.
  const kicks = feed.events.filter((e) => e.i === "boom" &&
    e.t >= FROM + sec.startSec && e.t < FROM + sec.endSec)
    .map((e) => ({ t: e.t - FROM - sec.startSec, g: e.gain ?? .15 }));
  if (!kicks.length) { console.log(`  ○ ${s.name}: no kicks in section`); continue; }
  let best = 0, bestScore = -1;
  const steps = Math.floor(slack * FPS);
  for (let o = 0; o <= steps; o++) {
    const off = o / FPS;
    let score = 0;
    for (const k of kicks) {
      const f = Math.round((k.t + off) * FPS);
      // a little tolerance: the impact may land a frame either side
      const v = Math.max(imp[f] ?? 0, imp[f - 1] ?? 0, imp[f + 1] ?? 0);
      score += v * k.g;
    }
    if (score > bestScore) { bestScore = score; best = off; }
  }
  offsets[s.name] = best;
  console.log(`  ✓ ${s.name}: offset ${best.toFixed(3)}s of ${slack.toFixed(2)}s slack (${kicks.length} kicks)`);
}
writeFileSync(`${MOTION_DIR}/offsets.json`, JSON.stringify(offsets, null, 2));
console.log(`✓ ${MOTION_DIR}/offsets.json — re-run --assemble to apply`);
