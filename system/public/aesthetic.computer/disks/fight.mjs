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
// gamepads: D-pad/left stick moves. Xbox A/X/RB punches and B/Y/LB blocks;
// on an 8BitDo M30 the lower A/B/C row punches and upper X/Y/Z row blocks.
//
// the sim is deterministic and snapshot-clean; nothing here touches a network.
//
//   fight:boxes           hit and hurt boxes, tick, checksum
//   fight:synctest        rewind every frame and check, for each mechanic
//   fight:lag             two rollback sessions over a hostile fake wire
//   fight:lag:10:25       …with 10 frames of latency and 25% packet loss

import * as game from "../lib/fight/sim.mjs";
import { syncTest, report, drills } from "../lib/fight/rollback.mjs";
import { createSession, createLink } from "../lib/fight/session.mjs";
import { getGamepadMapping } from "../lib/gamepad-mappings.mjs";

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
const keyHeld = [0, 0];
const padsHeld = [0, 0];
const padInput = new Map();
let half = 0; // ac sims at 120hz; the game ticks at 60
let boxes = false;
let verdict; // synctest result, when asked for
let net = null; // two sessions over a fake laggy wire, when asked for
let sfxIn = 0; // what the local session wants heard this frame

// `fight:lag` runs the real rollback session against itself over a hostile
// link, both players on this keyboard. we render player 0's view, so player 2
// is the one who snaps when a prediction was wrong. `fight:lag:10:25` is ten
// frames of latency and a quarter of the packets on the floor.
function open(colon) {
  const i = colon.indexOf("lag");
  if (i < 0) return null;
  const latency = Number(colon[i + 1]) || 6;
  const loss = (Number(colon[i + 2]) || 0) / 100;
  const link = createLink({ latency, jitter: 2, loss, seed: 7 });
  const mk = (player, onSfx) =>
    createSession(game, { player, seed: 1, send: (p) => link.ship(p), onSfx });
  const a = mk(0, (f) => (sfxIn |= f));
  const b = mk(1);
  link.join(a, b);
  return { link, a, b, latency, loss };
}

let opened = []; // the colon we booted with, so `r` can rebuild the same match

function boot({ colon }) {
  opened = colon;
  net = open(colon);
  s = net ? net.a.state : game.create(1);
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

function refreshPad(e) {
  const player = e.gamepad & 1;
  let pad = padInput.get(e.gamepad);
  if (!pad) {
    pad = { id: e.gamepadId || "standard", buttons: new Set(), axes: {} };
    padInput.set(e.gamepad, pad);
  }
  if (e.gamepadId) pad.id = e.gamepadId;
  if (e.button !== undefined) {
    if (e.action === "push") pad.buttons.add(e.button);
    else if (e.action === "release") pad.buttons.delete(e.button);
  }
  if (e.axis !== undefined) pad.axes[e.axis] = e.value;

  const mapping = getGamepadMapping(pad.id);
  const down = (list) => list.some((button) => pad.buttons.has(button));
  let bits = 0;
  if (pad.buttons.has(14) || (pad.axes[0] || 0) < -0.35) bits |= game.LEFT;
  if (pad.buttons.has(15) || (pad.axes[0] || 0) > 0.35) bits |= game.RIGHT;
  if (down(mapping.fight?.punch || [0, 2, 5])) bits |= game.PUNCH;
  if (down(mapping.fight?.block || [1, 3, 4])) bits |= game.BLOCK;
  padsHeld[player] = bits;
  held[player] = keyHeld[player] | padsHeld[player];
}

function sim({ sound }) {
  if ((half ^= 1)) return; // every other 120hz step
  if (s[G.MATCH]) return;

  let flags;
  if (net) {
    net.link.tick();
    sfxIn = 0;
    // a stall is a dropped frame, not an error — the peer waits rather than
    // predict past the window, and hands the same input back next tick.
    net.a.advance(held[0]);
    net.b.advance(held[1]);
    flags = sfxIn; // only the local session's leading frames speak
  } else {
    game.step(s, held[0], held[1]);
    flags = s[G.SFX];
  }
  hear(sound, flags); // a rollback's resimulated frames never reach here
}

function act({ event: e }) {
  for (const key in KEYS) {
    const [p, bit] = KEYS[key];
    if (e.is(`keyboard:down:${key}`)) keyHeld[p] |= bit;
    if (e.is(`keyboard:up:${key}`)) keyHeld[p] &= ~bit;
    held[p] = keyHeld[p] | padsHeld[p];
  }
  if (e.is("gamepad")) refreshPad(e);
  if ((e.is("keyboard:down:r") || (e.button === 9 && e.action === "push")) && s[G.MATCH]) {
    // a networked rematch is a fresh session on a fresh wire, not a reset state
    net = net ? open(opened) : null;
    s = net ? net.a.state : game.create(s[G.RNG]);
    sfxIn = 0;
  }
}

// the sim already decided what happened; this only gives it a voice.
function hear(sound, f) {
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

// a fighter is a posed stick figure, not a slab. joints live in a 22×46 body
// box facing right; face < 0 mirrors it. everything scales with the box so it
// reads at any window size. the front arm on a punch reaches to the real fist
// so the figure lines up with the hitbox.
const FB = {
  idle:  { head: [11, 7], neck: [11, 12], pelvis: [11, 27], elbF: [13, 19], handF: [13, 25], elbB: [9, 19],  handB: [9, 25],  kneeF: [12, 37], footF: [13, 46], kneeB: [10, 37], footB: [9, 46] },
  walkA: { head: [11, 7], neck: [11, 12], pelvis: [11, 27], elbF: [14, 18], handF: [16, 23], elbB: [8, 20],  handB: [7, 26],  kneeF: [15, 36], footF: [17, 46], kneeB: [8, 37],  footB: [5, 45] },
  walkB: { head: [11, 7], neck: [11, 12], pelvis: [11, 27], elbF: [8, 18],  handF: [6, 23],  elbB: [14, 20], handB: [15, 26], kneeF: [8, 36],  footF: [5, 46],  kneeB: [15, 37], footB: [17, 45] },
  punch: { head: [12, 7], neck: [12, 12], pelvis: [11, 27], elbB: [9, 19],  handB: [10, 24], kneeF: [15, 36], footF: [18, 46], kneeB: [8, 37],  footB: [6, 46] },
  block: { head: [10, 8], neck: [10, 13], pelvis: [10, 27], elbF: [14, 14], handF: [15, 21], elbB: [12, 16], handB: [14, 22], kneeF: [12, 37], footF: [13, 46], kneeB: [8, 37],  footB: [7, 46] },
  hurt:  { head: [8, 9],  neck: [9, 13],  pelvis: [11, 27], elbF: [6, 16],  handF: [3, 13],  elbB: [11, 18], handB: [13, 23], kneeF: [13, 37], footF: [15, 46], kneeB: [9, 37],  footB: [8, 46] },
  dead:  { head: [17, 41],neck: [14, 43], pelvis: [5, 43],  elbF: [13, 40], handF: [12, 38], elbB: [9, 45],  handB: [8, 46],  kneeF: [5, 41],  footF: [2, 40],  kneeB: [5, 45],  footB: [2, 46] },
};
const FB_BONES = [["neck", "pelvis"], ["neck", "elbF"], ["elbF", "handF"], ["neck", "elbB"], ["elbB", "handB"], ["pelvis", "kneeF"], ["kneeF", "footF"], ["pelvis", "kneeB"], ["kneeB", "footB"]];

function drawFighter(ink, x, y, bw, bh, face, st, stf, tick, c, u) {
  const mx = (nx) => (x + ((face > 0 ? nx : 22 - nx) * bw) / 22) | 0;
  const my = (ny) => (y + (ny * bh) / 46) | 0;

  let pose = FB.idle;
  if (st === ST.WALK) pose = (tick >> 2) & 1 ? FB.walkB : FB.walkA;
  else if (st === ST.PUNCH) pose = FB.punch;
  else if (st === ST.BLOCK) pose = FB.block;
  else if (st === ST.BLOCKSTUN || st === ST.PARRIED) pose = FB.hurt;
  else if (st === ST.DEAD) pose = FB.dead;

  for (const [a, b] of FB_BONES) {
    if (!pose[a] || !pose[b]) continue;
    ink(...c).line(mx(pose[a][0]), my(pose[a][1]), mx(pose[b][0]), my(pose[b][1]));
  }

  // front arm — the punch. reaches the true fist during active frames, tucked
  // otherwise, so the drawn jab and the hurtbox are the same length.
  if (st === ST.PUNCH) {
    const live = stf >= PUNCH_F.startup && stf < PUNCH_F.startup + PUNCH_F.active;
    const arm = live ? u(PUNCH_F.reach) : u(PUNCH_F.reach) >> 2;
    const sx0 = mx(pose.neck[0]);
    const sy0 = my(13);
    const fist = face > 0 ? x + bw + arm : x - arm;
    ink(...(live ? [255, 255, 255] : c)).line(sx0, sy0, fist, sy0);
  }

  const hr = Math.max(1, ((4 * bw) / 22) | 0);
  ink(...c).circle(mx(pose.head[0]), my(pose.head[1]), hr, true);
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

    drawFighter(ink, x, y, bw, bh, face, st, s[b + P.STF], s[G.TICK], c, u);

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

  if (boxes) ink(70, 66, 90).write(`t${s[G.TICK]} ${game.checksum(s).toString(16)}`, { x: 4, y: 25 });
  if (verdict) ink(120, 255, 160).write(verdict, { x: 4, y: 33 });
  if (net) wire(ink, w);

  hud(ink, w); // last, so the round-over card covers the readout behind it
  if (pad > 4) {
    pads(ink, 4, h - 29, 0);
    pads(ink, w - 32, h - 29, 1);
  }
}

// the netcode, out loud. depth is what you actually feel: it is how many frames
// get thrown away and resimulated when a guess turns out wrong.
//
// this sits below the pips, not in the corners — ac owns the top-left label and
// the top-right controls, and the readout was painting straight through both.
function wire(ink, w) {
  const { a, link, latency, loss } = net;
  const st = a.stats;
  const depth = st.rollbacks ? (st.resimFrames / st.rollbacks).toFixed(1) : "0";
  const ls = link.stats();
  const ahead = a.frame - a.confirmed();

  ink(90, 200, 255).write(`lag ${latency}f loss ${(loss * 100) | 0}%`, { x: 4, y: 33 });
  ink(70, 66, 90).write(`rb ${st.rollbacks} @${depth}f stall ${st.stalls}`, { x: 4, y: 41 });
  ink(70, 66, 90).write(`drop ${ls.dropped}/${ls.sent}`, { x: 4, y: 49 });

  // how far out on the limb we are, against the 8-frame prediction window
  const bar = Math.min(40, w - 12);
  ink(40, 38, 54).box(4, 58, bar, 3);
  const fill = Math.min(bar, ((ahead * bar) / 8) | 0);
  ink(...(ahead >= 8 ? [255, 90, 90] : [90, 200, 255])).box(4, 58, fill, 3);
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
