#!/usr/bin/env node
// viz.mjs — 3-D visualization of the bell physical model.
//
// Reads the modes JSON exported by the C engine (`bell ... --modes out.json`),
// the SAME data that produced the audio, and animates the bell shell deforming
// as the live sum of its excited modes decays. You literally watch the (m,n)
// rim-flexural modes ring and fade. A small software 3-D pipeline (no three.js,
// matching the repo's no-deps ethos) projects the surface of revolution; frames
// are piped as BGRA to ffmpeg via pop/lib/preview-shared.mjs and muxed with the
// rendered bell audio.
//
// Usage:
//   node viz.mjs --modes bell-modes.json --audio bell.wav --out bell-viz.mp4
//                [--dur 9] [--fps 30] [--portrait] [--w 1920 --h 1080]
//
// The audio's first strike is assumed at t=0 (as bell --out renders it).

import { createCanvas } from "canvas";
import { resolve } from "node:path";
import { readFileSync } from "node:fs";
import { spawn, spawnSync } from "node:child_process";
import {
  spawnFFmpegEncode,
  decodeAudioMono,
  computeRmsEnvelope,
} from "../../lib/preview-shared.mjs";

// ---- args ------------------------------------------------------------------
const args = process.argv.slice(2);
function arg(name, def) {
  const i = args.indexOf(name);
  return i >= 0 && i + 1 < args.length ? args[i + 1] : def;
}
const modesPath = resolve(arg("--modes", "/tmp/bell-modes.json"));
const audioPath = arg("--audio", null);
const outPath = resolve(arg("--out", "/tmp/bell-viz.mp4"));
const fps = parseInt(arg("--fps", "30"), 10);
const dur = parseFloat(arg("--dur", "9"));
const strikeAt = parseFloat(arg("--strike", "0.7")); // seconds of rest first
const portrait = args.includes("--portrait");
const W = parseInt(arg("--w", portrait ? "1080" : "1920"), 10);
const H = parseInt(arg("--h", portrait ? "1920" : "1080"), 10);

const model = JSON.parse(readFileSync(modesPath, "utf8"));
const geo = model.geometry;
const modes = model.modes;

// ---- metal palette by material --------------------------------------------
const METAL = {
  bronze: [176, 141, 58], brass: [201, 162, 75], steel: [154, 163, 173],
  aluminum: [200, 204, 208], silver: [216, 221, 227], glass: [127, 212, 224],
  gold: [227, 194, 0],
};
const metal = METAL[model.material.name] || [180, 150, 90];

// ===========================================================================
// Build the surface-of-revolution mesh from the meridian profile.
// ===========================================================================
const NS = geo.n;                  // meridian stations
const NT = portrait ? 80 : 96;     // angular divisions

// Center the bell vertically and find a scale that fills the frame.
let zmin = Infinity, zmax = -Infinity, rmax = 0;
for (let i = 0; i < NS; i++) {
  zmin = Math.min(zmin, geo.z[i]);
  zmax = Math.max(zmax, geo.z[i]);
  rmax = Math.max(rmax, geo.r[i]);
}
const zc = 0.5 * (zmin + zmax);
const span = Math.max(zmax - zmin, 2 * rmax);

// 2-D meridian tangent/normal per node (in the (r,z) plane).
const nr = new Float64Array(NS), nz = new Float64Array(NS);
const tr = new Float64Array(NS), tz = new Float64Array(NS);
for (let i = 0; i < NS; i++) {
  const a = Math.max(0, i - 1), b = Math.min(NS - 1, i + 1);
  let dr = geo.r[b] - geo.r[a], dz = geo.z[b] - geo.z[a];
  const L = Math.hypot(dr, dz) || 1;
  dr /= L; dz /= L;
  tr[i] = dr; tz[i] = dz;
  nr[i] = dz; nz[i] = -dr; // rotate tangent -> outward-ish normal
}

// Precompute angle tables and cos(m*theta) per mode.
const cosT = new Float64Array(NT), sinT = new Float64Array(NT);
for (let j = 0; j < NT; j++) {
  const th = (j / NT) * Math.PI * 2;
  cosT[j] = Math.cos(th); sinT[j] = Math.sin(th);
}
const cosMT = modes.map((md) => {
  const a = new Float64Array(NT);
  for (let j = 0; j < NT; j++) a[j] = Math.cos(md.m * (j / NT) * Math.PI * 2);
  return a;
});

// Visual deformation gain: largest excited mode reaches ~12% of the bell span.
let maxPart = 1e-6;
for (const md of modes) maxPart = Math.max(maxPart, Math.abs(md.part));
const GAIN = 0.12 * span / maxPart;

// ===========================================================================
// 3-D helpers (right-handed; Y up). Camera looks down -Z after we push +Z.
// ===========================================================================
const CAM_D = span * 2.5;          // camera distance
const FOC = Math.min(W, H) * 1.15; // focal length (px)
const TILT = 0.32;                 // slight downward look (radians)
const cosTilt = Math.cos(TILT), sinTilt = Math.sin(TILT);

function project(x, y, z, yaw) {
  // rotate about Y (yaw)
  const cx = Math.cos(yaw), sx = Math.sin(yaw);
  let X = cx * x + sx * z;
  let Z = -sx * x + cx * z;
  let Y = y;
  // tilt about X
  const Y2 = cosTilt * Y - sinTilt * Z;
  const Z2 = sinTilt * Y + cosTilt * Z;
  // push away from camera
  const Zc = Z2 + CAM_D;
  const inv = FOC / Math.max(Zc, 1e-3);
  return [W / 2 + X * inv, H / 2 - Y2 * inv, Zc];
}

// ===========================================================================
// Audio-reactive strike flash (optional).
// ===========================================================================
let rms = null, rmsFps = 60;
if (audioPath) {
  try {
    const { audio, sr } = decodeAudioMono(audioPath);
    rms = computeRmsEnvelope(audio, sr, rmsFps, dur);
  } catch (e) {
    console.warn("viz: audio envelope unavailable:", e.message);
  }
}

// ===========================================================================
// Render loop.
// ===========================================================================
const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");
const totalFrames = Math.round(dur * fps);

// Delay the audio so its strike lands at strikeAt (the bell rests first).
let muxAudio = audioPath;
if (audioPath && strikeAt > 0) {
  const ms = Math.round(strikeAt * 1000);
  const padded = `${outPath}.aud.wav`;
  const p = spawnSync("ffmpeg", [
    "-hide_banner", "-loglevel", "error", "-y", "-i", audioPath,
    "-af", `adelay=${ms}|${ms}`, padded,
  ], { stdio: "inherit" });
  if (p.status === 0) muxAudio = padded;
}

// With audio: use the shared encoder (muxes audio). Without: video-only.
const encoder = muxAudio
  ? spawnFFmpegEncode({ audioPath: muxAudio, w: W, h: H, fps, outPath, crf: 18 })
  : spawn("ffmpeg", [
      "-hide_banner", "-loglevel", "error", "-y",
      "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(fps),
      "-i", "-", "-c:v", "libx264", "-preset", "faster", "-crf", "18",
      "-pix_fmt", "yuv420p", "-movflags", "+faststart", outPath,
    ], { stdio: ["pipe", "inherit", "inherit"] });

// Reusable per-vertex scratch.
const PX = new Float64Array(NS * NT);
const PY = new Float64Array(NS * NT);
const PZ = new Float64Array(NS * NT);
const DN = new Float64Array(NS * NT); // signed normal displacement (for tint)

const strikeFreq = model.strike_freq;

function modalCoords(tm) {
  // tm = time since the strike. Before the strike the bell sits at rest; after,
  // q_k = part * exp(-tm/tau) * cos(2*pi*f*tm) — rings out and settles back.
  const q = new Float64Array(modes.length);
  if (tm < 0) return q; // rest
  for (let k = 0; k < modes.length; k++) {
    const md = modes[k];
    q[k] = md.part * Math.exp(-tm / md.tau) * Math.cos(2 * Math.PI * md.freq * tm);
  }
  return q;
}

function drawHUD(tm) {
  ctx.save();
  ctx.font = "600 30px monospace";
  ctx.fillStyle = "rgba(235,238,245,0.92)";
  ctx.textBaseline = "top";
  ctx.fillText("BELL · physical model", 40, 36);
  ctx.font = "400 22px monospace";
  ctx.fillStyle = "rgba(190,198,210,0.85)";
  ctx.fillText(`${geo.name} · ${model.material.name}`, 40, 78);
  ctx.fillText(`strike ${strikeFreq.toFixed(1)} Hz`, 40, 106);
  ctx.fillText(`E ${(model.material.E / 1e9).toFixed(0)} GPa  ρ ${model.material.rho.toFixed(0)}  ν ${model.material.nu}`, 40, 134);
  // state label: at rest before the strike, ringing after.
  if (tm < 0) {
    ctx.fillStyle = "rgba(120,130,145,0.8)";
    ctx.fillText("· at rest ·", 40, 168);
  } else {
    ctx.fillStyle = "rgba(235,210,150,0.9)";
    ctx.fillText(`· ringing  ${tm.toFixed(2)}s ·`, 40, 168);
  }
  ctx.textBaseline = "alphabetic";

  // Spectrum bars: live decaying amplitude per partial, hued by m.
  const N = Math.min(modes.length, 24);
  const x0 = 40, y0 = H - 56, bw = 26, gap = 6, bh = 150;
  for (let k = 0; k < N; k++) {
    const md = modes[k];
    const env = tm < 0 ? 0 : md.part * Math.exp(-tm / md.tau);
    const hh = Math.min(1, env) * bh;
    const hue = (md.m - 2) * 48; // m=2 red-ish, climbing
    ctx.fillStyle = `hsla(${hue},70%,55%,0.85)`;
    ctx.fillRect(x0 + k * (bw + gap), y0 - hh, bw, hh);
    ctx.fillStyle = "rgba(120,128,140,0.6)";
    ctx.font = "400 13px monospace";
    ctx.fillText(String(md.m), x0 + k * (bw + gap) + 8, y0 + 16);
  }
  ctx.restore();
}

async function writeFrame(buf) {
  if (!encoder.stdin.write(buf))
    await new Promise((r) => encoder.stdin.once("drain", r));
}

async function main() {
  for (let f = 0; f < totalFrames; f++) {
    const t = f / fps;
    const tm = t - strikeAt; // time since strike (<0 = at rest)
    const yaw = t * 0.5; // slow reveal
    const q = modalCoords(tm);

    // Deform + project every vertex.
    for (let i = 0; i < NS; i++) {
      const ri = geo.r[i], zi = geo.z[i] - zc;
      for (let j = 0; j < NT; j++) {
        let dN = 0, dM = 0;
        for (let k = 0; k < modes.length; k++) {
          const c = GAIN * q[k] * cosMT[k][j];
          dN += c * modes[k].w[i];
          dM += c * modes[k].u[i];
        }
        const idx = i * NT + j;
        DN[idx] = dN;
        // base point + normal/tangent displacement, expanded to 3-D by theta.
        const rr = ri + nr[i] * dN + tr[i] * dM;
        const zz = zi + nz[i] * dN + tz[i] * dM;
        const x = rr * cosT[j];
        const z = rr * sinT[j];
        const [sx, sy, depth] = project(x, zz, z, yaw);
        PX[idx] = sx; PY[idx] = sy; PZ[idx] = depth;
      }
    }

    // Background.
    const bg = ctx.createLinearGradient(0, 0, 0, H);
    bg.addColorStop(0, "#0b0e13");
    bg.addColorStop(1, "#05070a");
    ctx.fillStyle = bg;
    ctx.fillRect(0, 0, W, H);

    // strike flash vignette (indexed by time-since-strike)
    if (rms && tm >= 0) {
      const fi = Math.min(rms.length - 1, Math.round(tm * rmsFps));
      const a = Math.min(0.25, (rms[fi] || 0) * 0.5);
      if (a > 0.01) {
        const g = ctx.createRadialGradient(W / 2, H / 2, 0, W / 2, H / 2, Math.max(W, H) * 0.6);
        g.addColorStop(0, `rgba(${metal[0]},${metal[1]},${metal[2]},${a})`);
        g.addColorStop(1, "rgba(0,0,0,0)");
        ctx.fillStyle = g;
        ctx.fillRect(0, 0, W, H);
      }
    }

    // Build quads, sort back-to-front (painter's), shade + draw.
    const quads = [];
    for (let i = 0; i < NS - 1; i++) {
      for (let j = 0; j < NT; j++) {
        const j2 = (j + 1) % NT;
        const a = i * NT + j, b = i * NT + j2;
        const c = (i + 1) * NT + j2, d = (i + 1) * NT + j;
        const depth = (PZ[a] + PZ[b] + PZ[c] + PZ[d]) * 0.25;
        const disp = (DN[a] + DN[b] + DN[c] + DN[d]) * 0.25;
        quads.push([a, b, c, d, depth, disp]);
      }
    }
    quads.sort((p, q2) => q2[4] - p[4]); // far first

    const Ly = 0.72; // dominant light component (overhead-ish)
    for (const [a, b, c, d, , disp] of quads) {
      // face normal from screen-space cross product (cheap, good enough)
      const ux = PX[b] - PX[a], uy = PY[b] - PY[a];
      const vx = PX[d] - PX[a], vy = PY[d] - PY[a];
      const nzs = ux * vy - uy * vx; // z of cross -> facing
      // world-ish normal proxy using depth gradient for lighting
      let shade = 0.45 + 0.55 * Math.max(0, (nzs > 0 ? Ly : Ly * 0.6));
      shade = Math.min(1, shade);
      // displacement tint: outward warm, inward cool
      const dispN = Math.max(-1, Math.min(1, disp / (0.12 * span)));
      const warm = dispN > 0 ? dispN : 0;
      const cool = dispN < 0 ? -dispN : 0;
      const r = metal[0] * shade + warm * 70 - cool * 30;
      const g = metal[1] * shade - cool * 10;
      const bl = metal[2] * shade + cool * 90;
      ctx.fillStyle = `rgb(${Math.max(0, Math.min(255, r | 0))},${Math.max(0, Math.min(255, g | 0))},${Math.max(0, Math.min(255, bl | 0))})`;
      ctx.beginPath();
      ctx.moveTo(PX[a], PY[a]);
      ctx.lineTo(PX[b], PY[b]);
      ctx.lineTo(PX[c], PY[c]);
      ctx.lineTo(PX[d], PY[d]);
      ctx.closePath();
      ctx.fill();
      // subtle wireframe
      ctx.strokeStyle = "rgba(0,0,0,0.18)";
      ctx.lineWidth = 0.6;
      ctx.stroke();
    }

    drawHUD(tm);

    await writeFrame(canvas.toBuffer("raw"));
    if (f % 30 === 0)
      process.stderr.write(`\rviz: frame ${f}/${totalFrames}`);
  }
  encoder.stdin.end();
  await new Promise((r) => encoder.on("close", r));
  process.stderr.write(`\nviz: wrote ${outPath}\n`);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
