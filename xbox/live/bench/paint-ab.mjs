#!/usr/bin/env node
// Same-process A/B of one draw function: both sources booted into the same
// dojo fight, then timed in alternating bursts so whatever else the machine
// is doing lands on both sides alike. Prints the median per call.
//
//   node [--jitless] xbox/live/bench/paint-ab.mjs A.js B.js [host] [fn] [bursts] [calls]
//     host  mac | mac+ (adds disc3d/capsule3d) | xbox (+ meshes, sceneMesh)
//     fn    shadow | figure | board | paint
// --jitless is the closest Node gets to the console's interpreter.
import { readFileSync } from "node:fs";
import { performance } from "node:perf_hooks";

const [A, B, host = "mac", fn = "figure", burstsArg = "40", callsArg = "100"] = process.argv.slice(2);
const bursts = Number(burstsArg), calls = Number(callsArg);
function load(src) {
  let now = 0, seed = 7341;
  const noOp = () => {}, M = Object.create(Math);
  M.random = () => (seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0) / 4294967296;
  const extra = host === "xbox" ? { disc3d: noOp, capsule3d: noOp, meshUpload: (() => { let n = 0; return () => n++; })(),
    meshDraw: noOp, meshFree: noOp, sceneMesh: noOp } : host === "mac+" ? { disc3d: noOp, capsule3d: noOp } : {};
  const names = ["runtime", "gamepad", "capabilities", "telemetry", "gameSignal", "saveReplay",
    "publishLive", "analytics", "drum", "wipe", "box", "line", "triangle", "triangle3d",
    "triangles3d", "write", "systemWrite", "gameView", "Math", ...Object.keys(extra)];
  const game = new Function(...names, `${readFileSync(src, "utf8")}\nreturn { boot, sim, paint, players,
    enterGame: () => enterGame(runtime().monotonicUs),
    startFight: () => { shellMode = "GAME"; selecting = false; players[1].npc = false; players[1].bot = false; applyRoster(players[1], 2); startReplay(runtime().monotonicUs); resetRound(runtime().monotonicUs, true); },
    shadow: (p) => drawPoseShadow(p, runtime().monotonicUs / 1e6, [90, 90, 100]),
    figure: (p) => drawFlatFigure(p, runnerWorldGeometry(p, runtime().monotonicUs / 1e6), runtime().monotonicUs),
    board: (p) => { p.skateboard = true; drawSkateboard(p); } };`)(
    () => ({ monotonicUs: now, unixMs: 1785870000000, simCount: 0, paintCount: 0 }),
    () => ({ connected: true, down: [], leftX: 0, leftY: 0 }),
    () => ({ platform: host === "xbox" ? "xbox-uwp" : "macos", inputFamily: "keyboard" }),
    noOp, noOp, () => Promise.resolve(true), noOp, noOp, noOp, noOp, noOp, noOp, noOp, noOp,
    undefined, noOp, noOp, () => ({ width: 1920, height: 1080 }), M, ...Object.values(extra));
  globalThis.__oskiewarOpponent = "dummy";
  game.boot(); game.enterGame(); game.startFight();
  for (let i = 0; i < 200; i++) { now += 16667; game.sim(); }
  game.paint();
  return game;
}
const a = load(A), b = load(B);
const run = (game) => {
  const t = performance.now();
  for (let i = 0; i < calls; i++) {
    if (fn === "paint") game.paint();
    else for (const p of game.players) game[fn](p);
  }
  return performance.now() - t;
};
for (let i = 0; i < 5; i++) { run(a); run(b); }
const ta = [], tb = [];
for (let i = 0; i < bursts; i++) {
  if (i % 2) { tb.push(run(b)); ta.push(run(a)); } else { ta.push(run(a)); tb.push(run(b)); }
}
const median = (x) => [...x].sort((p, q) => p - q)[x.length >> 1];
const us = (x) => +(median(x) / calls * 1000).toFixed(1);
console.log(JSON.stringify({ host, fn, aMicroseconds: us(ta), bMicroseconds: us(tb),
  bOverA: +(median(tb) / median(ta)).toFixed(3) }));
