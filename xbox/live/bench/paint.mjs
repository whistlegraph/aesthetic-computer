#!/usr/bin/env node
// Headless paint bench: oskiewar.js under a counting host, deterministic
// (seeded Math.random, frozen Date.now, scripted pads), so two sources can
// be compared draw call for draw call.
//
//   node xbox/live/bench/paint.mjs [source] [host] [scenario] [frames]
//     host      mac (triangle3d, triangleAlpha) | mac-batch (+ batchFaces)
//               | xbox (+ disc3d, capsule3d, meshUpload/meshDraw, sceneMesh)
//     scenario  fight | selfplay | park
//   HASH=1 adds `hash`: FNV-1a over the scene lane (faces as float32, boxes,
//   lines, alpha faces, native shapes and mesh uploads, in order) / the text
//   lane. Equal hashes mean the host was handed the same frame.
import { readFileSync } from "node:fs";
import { performance } from "node:perf_hooks";

const [src = "xbox/live/oskiewar.js", hostKind = "mac", scenario = "fight", framesArg = "240"] =
  process.argv.slice(2);
const frames = Number(framesArg), hashing = process.env.HASH === "1";
const xbox = hostKind === "xbox", batch = hostKind === "mac-batch";
let now = 0, seed = 7341;
Math.random = () => (seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296;
Date.now = () => 1785870000000 + Math.floor(now / 1000);

const counts = { tri: 0, text: 0, disc: 0, capsule: 0, meshDraw: 0, sceneMesh: 0, box: 0, line: 0, batches: 0 };
let scene = 2166136261, lane = 2166136261, frameText = 0, maxText = 0;
const fnv = (h, v) => { const t = String(v); for (let i = 0; i < t.length; i++) h = Math.imul(h ^ t.charCodeAt(i), 16777619); return h; };
const mix = (tag, values) => { if (!hashing) return; scene = fnv(scene, tag); for (const v of values) scene = fnv(scene, v); };
const F = Math.fround;
const face = (...v) => {
  counts.tri++;
  mix("t", [...v.slice(0, 9).map(F), F(v[9] ?? 255), F(v[10] ?? 255), F(v[11] ?? 255)]);
};
const text = (...v) => { counts.text++; frameText++; if (hashing) for (const x of v) lane = fnv(lane, x); };
const noOp = () => {};
let meshes = 0;
const extra = xbox ? {
  disc3d: (...v) => { counts.disc++; mix("d", v); },
  capsule3d: (...v) => { counts.capsule++; mix("c", v); },
  meshUpload: (...arrays) => { for (const a of arrays) if (a?.length) mix("u", a); return meshes++; },
  meshDraw: (handle, camera, ...v) => { counts.meshDraw++; mix("m", [handle, ...camera, ...v]); },
  meshFree: noOp, sceneMesh: () => { counts.sceneMesh++; },
} : { triangleAlpha: (...v) => mix("a", v.map(F)) };
const triangles3d = batch
  ? (buffer, n = buffer.length / 12) => { counts.batches++; for (let i = 0; i < n; i++) face(...buffer.subarray(i * 12, i * 12 + 12)); }
  : xbox ? (buffer, n = buffer.length / 12) => { counts.tri += n; } : undefined;

const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
const names = ["runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay",
  "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle", "triangle3d",
  "triangles3d", "write", "systemWrite", "gameView", ...Object.keys(extra)];
const game = new Function(...names, `${readFileSync(src, "utf8")}\nreturn { boot, sim, paint,
  enterGame: () => enterGame(runtime().monotonicUs),
  quietBall: () => { ballEnabled = false; for (const item of balls) item.active = false; },
  startFight: () => { shellMode = "GAME"; selecting = false; players[1].npc = false; players[1].bot = false; applyRoster(players[1], 2); startReplay(runtime().monotonicUs); matchBallType = "soccer"; resetRound(runtime().monotonicUs, true); },
  startSelfPlay: () => startSelfPlay(runtime().monotonicUs),
  enterFreeskate: () => beginFreeskate(runtime().monotonicUs),
  error: () => clientError };`)(
  () => ({ monotonicUs: now, unixMs: Date.now(), simCount: Math.floor(now / 16667), paintCount: 0 }),
  (i = 0) => ({ ...pads[i], down: pads[i].down.slice() }),
  () => ({ platform: xbox ? "xbox-uwp" : "macos", inputFamily: xbox ? "xbox" : "keyboard", batchFaces: batch }),
  noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp,
  (...v) => { counts.box++; mix("b", v); }, (...v) => { counts.line++; mix("l", v); },
  (...v) => face(v[0], v[1], 0, v[2], v[3], 0, v[4], v[5], 0, v[6], v[7], v[8]), face, triangles3d,
  text, text, () => ({ width: 1920, height: 1080 }), ...Object.values(extra));

Object.assign(globalThis, { __oskiewarAttractVariant: "still", __oskiewarParkIntro: false,
  __oskiewarLegacyButtons: true, __oskiewarOpponent: "dummy" });
game.boot(); game.enterGame(); game.quietBall();
if (scenario === "fight") game.startFight();
else if (scenario === "selfplay") game.startSelfPlay();
else if (scenario === "park") game.enterFreeskate();
const tick = (us = 16667) => { now += us; game.sim(); };
tick(3000001);
// Scripted hands, so the fighters move, swing and kick.
const hands = (f, s) => {
  const p = pads[s];
  p.leftX = Math.sin(f / (40 + s * 13)); p.down = [];
  if (f % (50 + s * 7) < 3) p.down.push("A");
  if (f % (90 + s * 11) < 2) p.down.push("X");
};
for (let f = 0; f < 60; f++) { hands(f, 0); hands(f, 1); tick(); game.paint(); }
for (const k in counts) counts[k] = 0;
let simMs = 0, paintMs = 0;
for (let f = 60; f < 60 + frames; f++) {
  hands(f, 0); hands(f, 1);
  let t = performance.now(); tick(); simMs += performance.now() - t;
  frameText = 0;
  t = performance.now(); game.paint(); paintMs += performance.now() - t;
  maxText = Math.max(maxText, frameText);
}
const per = (v) => +(v / frames).toFixed(2);
console.log(JSON.stringify({ host: hostKind, scenario, frames, simMs: per(simMs), paintMs: per(paintMs),
  faces: per(counts.tri), batches: per(counts.batches), texts: per(counts.text), maxTexts: maxText,
  discs: per(counts.disc), capsules: per(counts.capsule), meshDraws: per(counts.meshDraw),
  boxes: per(counts.box), lines: per(counts.line),
  ...(hashing ? { hash: `${(scene >>> 0).toString(16)}/${(lane >>> 0).toString(16)}` } : {}),
  error: game.error() ? String(game.error().message || game.error()) : null }));
