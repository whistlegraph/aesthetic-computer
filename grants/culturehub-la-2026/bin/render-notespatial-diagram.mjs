#!/usr/bin/env node
// render-notespatial-diagram.mjs — an mp4 diagram of *Note(s)pat(ial)
// Native*: six laptop+speaker sprites in a ring in 3D acoustic space, a
// listener at the center, and the 13 lanes of Special Sign as colored
// orbs that fly between machines following the live choreography modeled
// in ../NOTESPATIAL-COMPOSITION.md (enter / antiphony / spin / scatter /
// converge / rest). Events ripple across the floor from whichever
// machine sounds them. Frames stream raw to ffmpeg; the engine-rendered
// master lies underneath.
//
//   node render-notespatial-diagram.mjs <score.nsscore> <audio.wav> <out.mp4>

import { readFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { createCanvas } from "canvas";

const [, , scorePath, audioPath, outPath] = process.argv;
if (!outPath) {
  console.error("usage: render-notespatial-diagram.mjs <score.nsscore> <audio.wav> <out.mp4>");
  process.exit(1);
}
const S = JSON.parse(readFileSync(scorePath, "utf8"));

const W = 1920, H = 1080, FPS = 30;
const DUR = S.dur + 2;
const FRAMES = Math.ceil(DUR * FPS);
const SEATS = 6, RING = 2.8;

const canvas = createCanvas(W, H);
const ctx = canvas.getContext("2d");

// ---- camera: slow orbit, a touch above eye line -------------------------
const sub = (a, b) => [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
const norm = (a) => { const l = Math.hypot(...a); return [a[0] / l, a[1] / l, a[2] / l]; };

function project(p, t) {
  const az = 0.35 + t * 0.055, el = 0.38, dist = 7.2, fov = 900; // one lazy lap
  const look = [0, 0.6, 0];
  const cam = [Math.cos(az) * Math.cos(el) * dist, look[1] + Math.sin(el) * dist,
               Math.sin(az) * Math.cos(el) * dist];
  const fwd = norm(sub(look, cam));
  const right = norm(cross(fwd, [0, 1, 0]));
  const up = cross(right, fwd);
  const d = sub(p, cam);
  const z = dot(d, fwd);
  if (z < 0.5) return null;
  return { x: W / 2 + dot(d, right) * fov / z, y: H / 2 - dot(d, up) * fov / z, s: fov / z, d: z };
}

const seatPos = (i) => {
  const a = (i / SEATS) * Math.PI * 2 - Math.PI / 2;
  return [Math.cos(a) * RING, 0, Math.sin(a) * RING];
};

// ---- choreography: lane index → seat, per the composition model ---------
const M = S.movements; // I..VI
function laneSeat(lane, t) {
  const u = (m) => (t - m.t0) / (m.t1 - m.t0);
  if (t < M[0].t1) { // I Assembly — enter: 1 → 3 → 6 machines
    const k = u(M[0]);
    if (k < 0.34) return 0;
    if (k < 0.67) return [0, 2, 4][lane % 3];
    return lane % SEATS;
  }
  if (t < M[1].t1) { // II Signal — antiphony, question walks the ring
    const q = Math.floor(u(M[1]) * 4) % SEATS; // four walks across the dwell
    const QUESTION = [4, 5, 8, 9, 12]; // melody, echo, noses, vowels
    if (QUESTION.includes(lane)) return q;
    return (q + 3) % SEATS; // answers from across the ring
  }
  if (t < M[2].t1) { // III Super-Spin — orbit, doubling
    const k = u(M[2]);
    const turns = Math.floor(k * 8 * (1 + k)); // accelerating
    return (lane + turns) % SEATS;
  }
  if (t < M[3].t1) return lane % SEATS; // IV Constellation — max dispersion
  if (t < M[4].t1) { // V Home Sign — staggered convergence on seat 0
    const k = u(M[4]);
    return k * 13 > lane ? 0 : lane % SEATS;
  }
  return 0; // VI Run-Down — one body
}

// smoothed seat positions per lane (orbs fly, not teleport)
const orbAt = S.lanes.map((_, i) => seatPos(laneSeat(i, 0)).slice());
function orbTarget(lane, t) {
  const s = seatPos(laneSeat(lane, t));
  const stack = lane * 0.55; // hover height ladder so orbs never overlap
  return [s[0] * 0.86, 1.15 + (stack % 1.65), s[2] * 0.86];
}

// ---- event feed ---------------------------------------------------------
const cursors = S.lanes.map(() => 0);
const ripples = []; // { born, seat, color, g }
const glow = S.lanes.map(() => 0); // per-lane activity level

// ---- ffmpeg sink --------------------------------------------------------
const ff = spawn("ffmpeg", [
  "-y", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS),
  "-i", "-", "-i", audioPath,
  "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "medium", "-crf", "19",
  "-pix_fmt", "yuv420p", "-c:a", "aac", "-b:a", "192k", "-shortest", outPath,
], { stdio: ["pipe", "ignore", "inherit"] });

const write = (buf) => new Promise(res => ff.stdin.write(buf) ? res() : ff.stdin.once("drain", res));

function laptopSprite(p2, color, awake, flip) {
  const u = Math.max(14, 46 * p2.s / 130);
  ctx.save();
  ctx.translate(p2.x, p2.y);
  ctx.fillStyle = "#23252c";
  ctx.beginPath(); // keyboard base, slight parallelogram for depth
  ctx.moveTo(-u, 0); ctx.lineTo(u, 0); ctx.lineTo(u * 1.25, u * 0.42); ctx.lineTo(-u * 1.25, u * 0.42);
  ctx.closePath(); ctx.fill();
  // screen
  ctx.fillStyle = "#31343d";
  ctx.fillRect(-u, -u * 1.45, u * 2, u * 1.42);
  ctx.fillStyle = awake ? color : "#101014";
  ctx.fillRect(-u * 0.88, -u * 1.36, u * 1.76, u * 1.22);
  // speaker box beside the machine
  ctx.fillStyle = "#1c1e24";
  ctx.fillRect(u * 1.5, -u * 1.1, u * 0.72, u * 1.5);
  ctx.strokeStyle = awake ? color : "#33363e";
  ctx.lineWidth = Math.max(1, u * 0.08);
  ctx.beginPath(); ctx.arc(u * 1.86, -u * 0.55, u * 0.24, 0, Math.PI * 2); ctx.stroke();
  ctx.beginPath(); ctx.arc(u * 1.86, -u * 0.18, u * 0.13, 0, Math.PI * 2); ctx.stroke();
  ctx.restore();
}

function listenerSprite(p2, t) {
  const u = 40 * p2.s / 130;
  ctx.save();
  ctx.translate(p2.x, p2.y + Math.sin(t * 1.7) * 2);
  ctx.fillStyle = "#d8d4c8";
  ctx.beginPath(); ctx.arc(0, -u * 1.9, u * 0.52, 0, Math.PI * 2); ctx.fill(); // head
  ctx.beginPath(); // shoulders
  ctx.moveTo(-u, 0); ctx.quadraticCurveTo(0, -u * 1.7, u, 0); ctx.closePath(); ctx.fill();
  ctx.restore();
}

const rgba = (c, a) => `rgba(${c[0]},${c[1]},${c[2]},${a})`;

async function main() {
  console.log(`rendering ${FRAMES} frames…`);
  for (let f = 0; f < FRAMES; f++) {
    const t = f / FPS;

    // advance events → ripples + glow
    for (let i = 0; i < S.lanes.length; i++) {
      const evs = S.lanes[i].events;
      while (cursors[i] < evs.length && evs[cursors[i]].t <= t) {
        const e = evs[cursors[i]++];
        if (e.g > 0.06) ripples.push({ born: t, seat: laneSeat(i, t), color: S.lanes[i].color, g: e.g });
        glow[i] = Math.min(1, glow[i] + e.g);
      }
      glow[i] *= 0.90;
    }
    while (ripples.length && t - ripples[0].born > 1.6) ripples.shift();

    // orbs ease toward their choreographed seats
    for (let i = 0; i < S.lanes.length; i++) {
      const tgt = orbTarget(i, t);
      for (let k = 0; k < 3; k++) orbAt[i][k] += (tgt[k] - orbAt[i][k]) * 0.08;
    }

    // ---- draw ----
    ctx.fillStyle = "#07070b";
    ctx.fillRect(0, 0, W, H);

    // floor disc + ring
    const drawFloorCircle = (r, style, width) => {
      ctx.strokeStyle = style; ctx.lineWidth = width;
      ctx.beginPath();
      let started = false;
      for (let a = 0; a <= 64; a++) {
        const p = project([Math.cos(a / 32 * Math.PI) * r, 0, Math.sin(a / 32 * Math.PI) * r], t);
        if (!p) { started = false; continue; }
        if (!started) { ctx.moveTo(p.x, p.y); started = true; } else ctx.lineTo(p.x, p.y);
      }
      ctx.stroke();
    };
    drawFloorCircle(RING * 1.35, "#16181f", 2);
    drawFloorCircle(RING, "#1e2029", 2);
    drawFloorCircle(RING * 0.5, "#14161c", 1);

    // ripples (floor rings from sounding seats)
    for (const r of ripples) {
      const age = (t - r.born) / 1.6;
      const rad = 0.15 + age * 2.1;
      const [sx, , sz] = seatPos(r.seat);
      ctx.strokeStyle = rgba(r.color, (1 - age) * 0.55 * Math.min(1, r.g * 1.6));
      ctx.lineWidth = 2.5;
      ctx.beginPath();
      let started = false;
      for (let a = 0; a <= 48; a++) {
        const p = project([sx * 0.86 + Math.cos(a / 24 * Math.PI) * rad, 0.02, sz * 0.86 + Math.sin(a / 24 * Math.PI) * rad], t);
        if (!p) { started = false; continue; }
        if (!started) { ctx.moveTo(p.x, p.y); started = true; } else ctx.lineTo(p.x, p.y);
      }
      ctx.stroke();
    }

    // depth-sorted sprites: laptops, listener, orbs
    const items = [];
    for (let s = 0; s < SEATS; s++) {
      const pos = seatPos(s);
      const p2 = project(pos, t);
      if (!p2) continue;
      // screen color = strongest active owned lane
      let best = null, bl = 0.04;
      for (let i = 0; i < S.lanes.length; i++)
        if (laneSeat(i, t) === s && glow[i] > bl) { best = S.lanes[i].color; bl = glow[i]; }
      const awake = best !== null || t < S.dur;
      items.push({ d: p2.d, draw: () => laptopSprite(p2, best ? rgba(best, Math.min(1, 0.25 + bl)) : "#15161b", awake, pos[0] > 0) });
    }
    const lp = project([0, 0, 0], t);
    if (lp) items.push({ d: lp.d, draw: () => listenerSprite(lp, t) });
    for (let i = 0; i < S.lanes.length; i++) {
      const p2 = project(orbAt[i], t);
      if (!p2) continue;
      const c = S.lanes[i].color;
      const r = Math.max(2.5, (5 + glow[i] * 9) * p2.s / 130);
      items.push({ d: p2.d, draw: () => {
        ctx.fillStyle = rgba(c, 0.25 + glow[i] * 0.75);
        ctx.beginPath(); ctx.arc(p2.x, p2.y, r, 0, Math.PI * 2); ctx.fill();
        if (glow[i] > 0.3) {
          ctx.strokeStyle = rgba(c, glow[i] * 0.5);
          ctx.beginPath(); ctx.arc(p2.x, p2.y, r * 1.9, 0, Math.PI * 2); ctx.stroke();
        }
      }});
    }
    items.sort((a, b) => b.d - a.d);
    for (const it of items) it.draw();

    // ---- HUD ----
    const mv = M.find(m => t >= m.t0 && t < m.t1);
    ctx.fillStyle = "#e8e6df";
    ctx.font = "bold 44px Helvetica";
    ctx.fillText(mv ? mv.name : "·", 60, 92);
    ctx.fillStyle = "#9a97a3";
    ctx.font = "26px Helvetica";
    if (mv) ctx.fillText(mv.sub + "  ·  " + ["enter", "antiphony", "orbit", "scatter", "converge", "rest"][M.indexOf(mv)], 60, 130);
    ctx.textAlign = "right";
    ctx.fillStyle = "#787580";
    ctx.font = "24px Helvetica";
    ctx.fillText("Note(s)pat(ial) Native — Special Sign, live spatial plan", W - 60, 84);
    ctx.fillText("six machines · one listener · 8–12 min via held movements", W - 60, 116);
    ctx.textAlign = "left";

    // timeline with doors + playhead + rotation ribbon
    const tx = 60, tw = W - 120, ty = H - 70;
    ctx.fillStyle = "#1c1e26";
    ctx.fillRect(tx, ty, tw, 10);
    if (S.rotation) {
      ctx.strokeStyle = "#3E7C8A";
      ctx.lineWidth = 2;
      ctx.beginPath();
      for (let i = 0; i < S.rotation.length; i++) {
        const x = tx + (i / (S.rotation.length - 1)) * tw;
        const y = ty - 6 - S.rotation[i] * 26;
        i ? ctx.lineTo(x, y) : ctx.moveTo(x, y);
      }
      ctx.stroke();
    }
    for (const m of M) {
      const x = tx + (m.t0 / S.dur) * tw;
      ctx.fillStyle = "#4a4d59";
      ctx.fillRect(x, ty - 4, 2, 18);
      ctx.fillStyle = "#6c6975";
      ctx.font = "20px Helvetica";
      ctx.fillText(m.name.split(" ")[0], x + 6, ty + 34);
    }
    ctx.fillStyle = "#e8e6df";
    ctx.fillRect(tx + Math.min(1, t / S.dur) * tw - 1.5, ty - 8, 3, 26);

    await write(canvas.toBuffer("raw"));
    if (f % 300 === 0) console.log(`  ${f}/${FRAMES} (${(t).toFixed(0)}s)`);
  }
  ff.stdin.end();
  await new Promise(res => ff.on("close", res));
  console.log("wrote " + outPath);
}

main();
