// fight, 26.07.09
// one punch kills. block to survive it, and tap block just before it lands to
// parry — that stuns the puncher and the kill is yours. blocking spends a
// guard pip; out of guard and the next punch goes through. two fists meeting
// on the same frame clash and bounce.
//
// four keys each, one hand each:
//
//     w              ↑           block  (tap = parry)
//   a   d      ←   →             move
//     s              ↓           punch
//
// phase 0 of the rollback plan: the sim is deterministic and snapshot-clean,
// and nothing here touches the network yet. `fight:boxes` overlays hitboxes.
// `fight:synctest` runs the rollback harness and prints the verdict.

import * as game from "../lib/fight/sim.mjs";
import { syncTest, report, drills } from "../lib/fight/rollback.mjs";

const { P, PN, G, ST, SFX, SUB, BODY_W, BODY_H, PUNCH_F } = game;

// key → [player, bit, which face of the d-pad it draws on]
const KEYS = {
  a: [0, game.LEFT, "l"],
  d: [0, game.RIGHT, "r"],
  w: [0, game.BLOCK, "b"],
  s: [0, game.PUNCH, "p"],
  arrowleft: [1, game.LEFT, "l"],
  arrowright: [1, game.RIGHT, "r"],
  arrowup: [1, game.BLOCK, "b"],
  arrowdown: [1, game.PUNCH, "p"],
};

const CAP = [
  { l: "a", r: "d", b: "w", p: "s" },
  { l: "<", r: ">", b: "^", p: "v" },
];

const SUIT = [
  [90, 170, 255],
  [255, 110, 90],
];

let s; // the whole simulation, one Int32Array
const held = [0, 0];
let half = 0; // ac sims at 120hz; the game ticks at 60
let boxes = false;
let verdict; // synctest result, when asked for

function boot({ colon }) {
  s = game.create(1);
  boxes = colon.includes("boxes");
  if (colon.includes("synctest")) {
    // one drill per mechanic — random inputs practically never parry or clash,
    // so a synctest over them is green across code it never entered.
    const D = drills(game);
    const bad = [];
    for (const name in D) {
      const r = syncTest(game, D[name], { frames: 900, distance: 8 });
      if (!r.ok) bad.push(`${name}: ${report(r)}`);
    }
    verdict = bad.length ? bad.join("\n") : "synctest ok — parry, block, clash";
  }
}

function sim({ sound }) {
  if ((half ^= 1)) return; // every other 120hz step
  if (s[G.MATCH]) return;
  game.step(s, held[0], held[1]);
  hear(sound); // leading frame only — a rollback's resimulated frames stay mute
}

function act({ event: e }) {
  for (const key in KEYS) {
    const [p, bit] = KEYS[key];
    if (e.is(`keyboard:down:${key}`)) held[p] |= bit;
    if (e.is(`keyboard:up:${key}`)) held[p] &= ~bit;
  }
  if (e.is("keyboard:down:r") && s[G.MATCH]) s = game.create(s[G.RNG]);
}

// the sim already decided what happened; this only gives it a voice.
function hear(sound) {
  const f = s[G.SFX];
  if (!f) return;
  const play = (o) => sound?.synth?.({ attack: 0.001, ...o });

  if (f & SFX.SWING)
    play({ type: "noise-white", duration: 0.03, decay: 0.6, volume: 0.1 });
  if (f & SFX.BLOCK)
    play({ type: "noise-white", duration: 0.07, decay: 0.5, volume: 0.24 });
  if (f & SFX.CLASH) {
    play({ type: "square", tone: 1320, duration: 0.05, decay: 0.4, volume: 0.2 });
    play({ type: "noise-white", duration: 0.09, decay: 0.5, volume: 0.22 });
  }
  if (f & SFX.PARRY) {
    play({ type: "sine", tone: 1760, duration: 0.09, decay: 0.35, volume: 0.3 });
    play({ type: "triangle", tone: 880, duration: 0.14, decay: 0.5, volume: 0.16 });
  }
  if (f & SFX.BREAK)
    play({ type: "sawtooth", tone: 165, duration: 0.22, decay: 0.7, volume: 0.26 });
  if (f & SFX.KILL) {
    play({ type: "square", tone: 110, duration: 0.3, decay: 0.7, volume: 0.32 });
    play({ type: "noise-white", duration: 0.16, decay: 0.5, volume: 0.24 });
  }
  if (f & SFX.ROUND)
    play({ type: "sine", tone: 660, duration: 0.12, decay: 0.5, volume: 0.18 });
  if (f & SFX.MATCH) {
    play({ type: "square", tone: 440, duration: 0.16, decay: 0.6, volume: 0.24 });
    play({ type: "square", tone: 660, duration: 0.34, attack: 0.14, decay: 0.7, volume: 0.24 });
  }
}

// render only. everything below reads state and writes pixels — it never
// writes back, which is what keeps the sim rollback-safe.
//
// the stage is a fixed 256 units wide in the sim and always will be: if it
// tracked the screen, two players on differently sized windows would simulate
// different fights. so the renderer scales instead.
function paint({ wipe, ink, screen }) {
  const { width: w, height: h } = screen;
  const pad = h < 112 ? 4 : 30; // the key pads want the bottom strip
  const floor = h - pad - 6;
  // scale to whichever runs out first. on a wide, short window the width alone
  // would make a 95px fighter and run him straight through the hud.
  const k = Math.max(0.12, Math.min(w / 256, (floor - 32) / 58));
  const ox = (w - ((256 * k) | 0)) >> 1;
  const u = (subs) => ((subs * k) / SUB) | 0; // a length
  const sx = (subs) => ox + u(subs); // a position on the stage
  const py = (v) => floor - u(v);

  wipe(18, 16, 26);
  ink(40, 38, 58).box(ox, floor, (256 * k) | 0, 1);
  for (let x = 0; x < 256; x += 24) ink(30, 28, 44).box(sx(x * SUB), floor + 1, 1, 2);

  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    const st = s[b + P.ST];
    const x = sx(s[b + P.X] - (BODY_W >> 1));
    const y = py(BODY_H);
    const bw = u(BODY_W);
    const bh = u(BODY_H);
    const face = s[b + P.FACE];

    let c = SUIT[p];
    if (st === ST.BLOCK) c = game.parrying(s, b) ? [255, 255, 255] : [170, 185, 210];
    if (st === ST.BLOCKSTUN) c = [190, 200, 220];
    if (st === ST.PARRIED) c = [190, 120, 255];
    if (st === ST.DEAD) c = [64, 56, 66];

    ink(...c).box(x, y, bw, bh);
    ink(255).box(face > 0 ? x + bw - 2 : x, y + 2, 2, 3); // facing notch

    if (st === ST.PUNCH) {
      const f = s[b + P.STF];
      const live = f >= PUNCH_F.startup && f < PUNCH_F.startup + PUNCH_F.active;
      const arm = live ? u(PUNCH_F.reach) : u(PUNCH_F.reach) >> 2;
      const ax = face > 0 ? x + bw : x - arm;
      const ay = py(28 * SUB);
      if (live) ink(255, 255, 255).box(ax, ay, arm, 2);
      else ink(120, 120, 140).box(ax, ay, arm, 2);
    }

    if (s[b + P.SPL] > 0) {
      const r = s[b + P.SPL];
      ink(255, 230, 120).box(sx(s[b + P.X] + s[b + P.SPX]) - (r >> 1), py(26 * SUB), r, r);
    }

    if (boxes) {
      ink(80, 255, 120, 80).box(x, y, bw, bh);
      if (st === ST.PUNCH) {
        const f = s[b + P.STF];
        if (f >= PUNCH_F.startup && f < PUNCH_F.startup + PUNCH_F.active) {
          const front = s[b + P.X] + face * (BODY_W >> 1);
          const hx = face > 0 ? front : front - PUNCH_F.reach;
          ink(255, 60, 90, 110).box(sx(hx), py(34 * SUB), u(PUNCH_F.reach), u(16 * SUB));
        }
      }
    }
  }

  hud(ink, w);
  if (pad > 4) {
    pads(ink, 4, h - 29, 0);
    pads(ink, w - 32, h - 29, 1);
  }

  if (boxes) ink(70, 66, 90).write(`t${s[G.TICK]} ${game.checksum(s).toString(16)}`, { x: 4, y: 25 });
  if (verdict) ink(120, 255, 160).write(verdict, { x: 4, y: 33 });
}

// guard pips, round pips, clock, and whatever just ended. everything flanks the
// clock rather than hugging the corners — ac draws its own label top-left and
// its own controls top-right, and the pips were sitting underneath them.
function hud(ink, w) {
  const cx = w >> 1;
  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    for (let i = 0; i < game.GUARD_MAX; i++) {
      const x = p === 0 ? cx - 13 - i * 7 : cx + 9 + i * 7;
      if (i < s[b + P.GUARD]) ink(...SUIT[p]).box(x, 17, 5, 5);
      else ink(48, 44, 58).box(x, 17, 5, 5);
    }
    for (let i = 0; i < game.WIN_TARGET; i++) {
      const x = p === 0 ? cx - 11 - i * 5 : cx + 9 + i * 5;
      ink(...(i < s[b + P.WINS] ? [255, 220, 60] : [46, 42, 56])).box(x, 25, 3, 3);
    }
  }
  ink(220).write(`${(s[G.TIMER] / 60) | 0}`, { x: cx - 5, y: 16 });

  const over = s[G.MATCH] || s[G.OVER];
  if (!over) return;
  const msg = s[G.MATCH]
    ? s[G.MATCH] === 3
      ? "draw"
      : `p${s[G.MATCH]} takes it`
    : s[G.OVER] === 3
      ? "time"
      : `p${s[G.OVER]}`;
  const y = 40;
  ink(0, 0, 0, 200).box((w >> 1) - 44, y - 3, 88, s[G.MATCH] ? 22 : 12);
  ink(255, 220, 60).write(msg, { x: (w >> 1) - msg.length * 3, y });
  if (s[G.MATCH]) ink(150).write("r to rematch", { x: (w >> 1) - 33, y: y + 9 });
}

// the four keys, drawn as a d-pad that lights on press. the block key gets a
// gold rim when a parry is armed — the cooldown is the whole reason mashing
// block doesn't work, so it should be visible.
function pads(ink, ox, oy, p) {
  const b = p * PN;
  const cell = 9;
  const at = { b: [cell, 0], l: [0, cell], r: [cell * 2, cell], p: [cell, cell * 2] };
  const bit = { l: game.LEFT, r: game.RIGHT, b: game.BLOCK, p: game.PUNCH };

  for (const key in at) {
    const [dx, dy] = at[key];
    const x = ox + dx,
      y = oy + dy;
    const down = (held[p] & bit[key]) !== 0;
    const armed = key === "b" && s[b + P.PCD] === 0;

    if (down) ink(...SUIT[p]).box(x, y, 8, 8);
    else ink(34, 32, 46).box(x, y, 8, 8);

    if (armed && !down) ink(255, 220, 60).box(x, y, 8, 1);
    ink(...(down ? [16, 14, 22] : [110, 106, 130])).write(CAP[p][key], { x: x + 2, y: y + 1 });
  }
}

export { boot, paint, act, sim };
