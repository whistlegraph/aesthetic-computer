// fight, 26.07.09
// six attacks, throws, jumping, crouching, dashes and hold-away blocking.
// Hold away to block; crouching kicks get underneath that guard. Up jumps,
// down crouches, and a quick second tap left/right dashes. Xbox X/Y/RB are
// light/medium/heavy punch; A/B/RT are light/medium/heavy kick; X+A throws.
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
import { getButtonColors, getGamepadMapping } from "../lib/gamepad-mappings.mjs";
import { createFightLobby } from "../lib/fight/lobby.mjs";
import { createFightLogin, pollFightLogin, adoptFightSession } from "../lib/fight/login.mjs";
import { qrcode as qr } from "../dep/@akamfoad/qr/qr.mjs";

const { P, PN, G, ST, SFX, SUB, BODY_W, BODY_H, PUNCH_F } = game;

// key → [player, bit, which face of the d-pad it draws on]
const KEYS = {
  a: [0, game.LEFT, "l"],
  d: [0, game.RIGHT, "r"],
  w: [0, game.UP, "u"], s: [0, game.DOWN, "d"],
  f: [0, game.LP, "LP"], g: [0, game.MP, "MP"], h: [0, game.HP, "HP"],
  v: [0, game.LK, "LK"], b: [0, game.MK, "MK"], n: [0, game.HK, "HK"],
  arrowleft: [1, game.LEFT, "l"],
  arrowright: [1, game.RIGHT, "r"],
  arrowup: [1, game.UP, "u"], arrowdown: [1, game.DOWN, "d"],
  j: [1, game.LP, "LP"], k: [1, game.MP, "MP"], l: [1, game.HP, "HP"],
  m: [1, game.LK, "LK"], comma: [1, game.MK, "MK"], period: [1, game.HK, "HK"],
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
let attract = true;
let attractPause = 0;
let fpsSystem = null;
let screenWidth = 256;
let fps = 0, fpsFrames = 0, fpsAt = 0;
const inputStream = [];
const INPUT_STREAM_MAX = 12;
const lastResult = [0, 0];
let lightMode = false;
const THEMES = {
  dark: { bg: [18, 16, 26], floor: [40, 38, 58], grid: [30, 28, 44], quiet: [145, 140, 165], empty: [48, 44, 58], panel: [0, 0, 0, 200] },
  light: { bg: [242, 239, 230], floor: [92, 88, 108], grid: [211, 206, 194], quiet: [73, 68, 88], empty: [196, 190, 181], panel: [255, 253, 247, 225] },
};
const colors = () => THEMES[lightMode ? "light" : "dark"];
let server = null;
let lobby = createFightLobby("guest");
let loginPair = null;
let loginCells = null;
let loginStatus = "idle";
let loginTimer = null;
let loggedInHandle = null;

async function beginFightLogin() {
  if (loggedInHandle || loginStatus === "creating" || loginStatus === "pending") return;
  loginStatus = "creating";
  try {
    loginPair = await createFightLogin();
    loginCells = qr(loginPair.loginUrl, { errorCorrectLevel: 1 }).modules;
    loginStatus = "pending";
    pollFightPair();
  } catch (error) {
    console.warn("fight login unavailable", error);
    loginStatus = "error";
  }
}

async function pollFightPair() {
  if (!loginPair || loginStatus !== "pending") return;
  try {
    const result = await pollFightLogin(loginPair);
    if (result.status === "claimed") {
      loginStatus = "claimed";
      adoptFightSession(result.session);
      return;
    }
    if (result.status === "expired") {
      loginStatus = "expired";
      return;
    }
  } catch (error) {
    console.warn("fight login poll failed", error);
  }
  loginTimer = setTimeout(pollFightPair, 2000);
}

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

function boot({ colon, system, net: { socket } = {}, handle }) {
  fpsSystem = system;
  loggedInHandle = handle?.() || null;
  const lobbyHandle = loggedInHandle || `guest-${Math.random().toString(36).slice(2, 7)}`;
  lobby = createFightLobby(lobbyHandle);
  if (socket) {
    server = socket((_id, type, content) => {
      if (type.startsWith("connected")) {
        lobby.join(server);
        return;
      }
      lobby.receive(type, content);
    });
  }
  if (!loggedInHandle) beginFightLogin();
  opened = colon;
  inputStream.length = 0;
  lastResult.fill(0);
  padInput.clear();
  held.fill(0);
  keyHeld.fill(0);
  padsHeld.fill(0);
  net = open(colon);
  s = net ? net.a.state : game.create(1);
  attract = !colon.includes("synctest");
  attractPause = 0;
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

function resetFight() {
  net = net ? open(opened) : null;
  s = net ? net.a.state : game.create(s[G.RNG]);
  held.fill(0);
  keyHeld.fill(0);
  padsHeld.fill(0);
  sfxIn = 0;
  attractPause = 0;
}

function demoInput(player) {
  const me = player * PN;
  const them = (1 - player) * PN;
  const distance = Math.abs(s[me + P.X] - s[them + P.X]);
  const phase = (s[G.TICK] + player * 43) % 120;
  let bits = 0;
  if (distance > 34 * SUB) bits |= s[me + P.X] < s[them + P.X] ? game.RIGHT : game.LEFT;
  if (phase >= 44 && phase < 50) bits |= (phase & 1) ? game.LP : game.LK;
  if (phase >= 82 && phase < 84) bits |= game.UP;
  return bits;
}

function refreshPad(e) {
  const player = e.gamepad & 1;
  let pad = padInput.get(e.gamepad);
  if (!pad) {
    pad = { id: e.gamepadId || "standard", buttons: new Set(), axes: {}, streamAxis: 0 };
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
  if (pad.buttons.has(12) || (pad.axes[1] || 0) < -0.35) bits |= game.UP;
  if (pad.buttons.has(13) || (pad.axes[1] || 0) > 0.35) bits |= game.DOWN;
  if (pad.buttons.has(2)) bits |= game.LP; // X
  if (pad.buttons.has(3)) bits |= game.MP; // Y
  if (pad.buttons.has(5)) bits |= game.HP; // RB
  if (pad.buttons.has(0)) bits |= game.LK; // A
  if (pad.buttons.has(1)) bits |= game.MK; // B
  if (pad.buttons.has(7)) bits |= game.HK; // RT
  padsHeld[player] = bits;
  held[player] = keyHeld[player] | padsHeld[player];
}

function streamInput(player, label, color = "gray") {
  inputStream.unshift({ player, label, color });
  if (inputStream.length > INPUT_STREAM_MAX) inputStream.pop();
}

const BUTTON_MOVES = { 0: "LK", 1: "MK", 2: "LP", 3: "MP", 5: "HP", 7: "HK" };

function playerPad(player) {
  for (const [index, pad] of padInput) {
    if ((index & 1) === player) return pad;
  }
  return null;
}

function padHelp(player) {
  const pad = playerPad(player);
  if (!pad) return { caps: CAP[player], hint: null };
  const mapping = getGamepadMapping(pad.id);
  const m30 = mapping?.product === "M30";
  return {
    caps: { l: "<", r: ">", b: m30 ? "X" : "B", p: "A" },
    mapping,
    pad,
  };
}

function sim({ sound }) {
  if ((half ^= 1)) return; // every other 120hz step
  if (attract && s[G.MATCH]) {
    if (++attractPause > 75) resetFight();
    return;
  }
  if (s[G.MATCH]) return;

  const input = attract ? [demoInput(0), demoInput(1)] : held;

  let flags;
  if (net) {
    net.link.tick();
    sfxIn = 0;
    // a stall is a dropped frame, not an error — the peer waits rather than
    // predict past the window, and hands the same input back next tick.
    net.a.advance(input[0]);
    net.b.advance(input[1]);
    flags = sfxIn; // only the local session's leading frames speak
  } else {
    game.step(s, input[0], input[1]);
    flags = s[G.SFX];
  }
  hear(sound, flags); // a rollback's resimulated frames never reach here
  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    const result = s[b + P.RESULT];
    if (result && result !== lastResult[p]) {
      const name = game.ATTACK_NAMES[s[b + P.ATK]] || "attack";
      streamInput(p, `${name} ${result > 0 ? "SUCCESS" : "FAILED"}`, result > 0 ? "lime" : "red");
    }
    lastResult[p] = result;
  }
}

function act({ event: e }) {
  const startsWithKey = Object.keys(KEYS).some((key) => e.is(`keyboard:down:${key}`));
  const startsWithPad = e.is("gamepad") &&
    ((e.button !== undefined && e.action === "push") ||
      (e.axis !== undefined && Math.abs(e.value) > 0.35));
  if (attract && (startsWithKey || startsWithPad)) {
    attract = false;
    resetFight();
  }
  for (const key in KEYS) {
    const [p, bit] = KEYS[key];
    if (e.is(`keyboard:down:${key}`)) {
      if (!(keyHeld[p] & bit)) streamInput(p, CAP[p][KEYS[key][2]], SUIT[p]);
      keyHeld[p] |= bit;
    }
    if (e.is(`keyboard:up:${key}`)) keyHeld[p] &= ~bit;
    held[p] = keyHeld[p] | padsHeld[p];
  }
  if (e.is("gamepad")) {
    refreshPad(e);
    const pad = padInput.get(e.gamepad);
    const player = e.gamepad & 1;
    if (e.button !== undefined && e.action === "push") {
      const label = controllerButtonLabel(pad.id, e.button);
      const move = BUTTON_MOVES[e.button];
      streamInput(player, move ? `${label} ${move}` : label, getButtonColors(pad.id, e.button).active);
      if (pad.buttons.has(0) && pad.buttons.has(2)) streamInput(player, "X+A GRAPPLE THROW", "violet");
    }
    if (e.axis === 0) {
      const direction = e.value < -0.35 ? -1 : e.value > 0.35 ? 1 : 0;
      if (direction && direction !== pad.streamAxis) {
        streamInput(player, direction < 0 ? "<" : ">", "limegreen");
      }
      pad.streamAxis = direction;
    }
  }
  if ((e.is("keyboard:down:r") || (e.button === 9 && e.action === "push")) && s[G.MATCH]) {
    // a networked rematch is a fresh session on a fresh wire, not a reset state
    net = net ? open(opened) : null;
    s = net ? net.a.state : game.create(s[G.RNG]);
    sfxIn = 0;
  }
}

function controllerButtonLabel(id = "", button) {
  const lower = id.toLowerCase();
  const xbox = lower.includes("xbox") || lower.includes("xinput") || lower.includes("8bitdo") || lower.includes("m30");
  const ps = lower.includes("playstation") || lower.includes("dualshock") || lower.includes("dualsense") || lower.includes("sony");
  const xboxLabels = ["A", "B", "X", "Y", "LB", "RB", "LT", "RT", "View", "Menu"];
  const psLabels = ["X", "O", "square", "triangle", "L1", "R1", "L2", "R2", "Share", "Options"];
  if (xbox) return xboxLabels[button] || `B${button}`;
  if (ps) return psLabels[button] || `B${button}`;
  return `B${button}`;
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
function paint({ wipe, ink, screen, dark }) {
  lightMode = dark === false;
  const { width: w, height: h } = screen;
  screenWidth = w;
  const now = globalThis.performance?.now?.() || 0;
  fpsFrames++;
  if (!fpsAt) fpsAt = now;
  if (now - fpsAt >= 500) {
    fps = Math.round((fpsFrames * 1000) / (now - fpsAt));
    fpsFrames = 0; fpsAt = now;
  }
  const pad = h < 112 ? 4 : 30; // the key pads want the bottom strip
  const floor = h - pad - 6;
  // scale to whichever runs out first. on a wide, short window the width alone
  // would make a 95px fighter and run him straight through the hud.
  const k = Math.max(0.12, Math.min(w / 256, (floor - 32) / 58));
  const ox = (w - ((256 * k) | 0)) >> 1;
  const u = (subs) => ((subs * k) / SUB) | 0; // a length
  const sx = (subs) => ox + u(subs); // a position on the stage
  const py = (v) => floor - u(v);

  wipe(...colors().bg);
  ink(...colors().floor).box(ox, floor, (256 * k) | 0, 1);
  for (let x = 0; x < 256; x += 24) ink(...colors().grid).box(sx(x * SUB), floor + 1, 1, 2);

  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    const st = s[b + P.ST];
    const x = sx(s[b + P.X] - (BODY_W >> 1));
    const y = py(BODY_H + s[b + P.Y]);
    const bw = u(BODY_W);
    const bh = u(BODY_H);
    const face = s[b + P.FACE];

    let c = SUIT[p];
    if (st === ST.BLOCK) c = game.parrying(s, b) ? [255, 255, 255] : [170, 185, 210];
    if (st === ST.BLOCKSTUN) c = [190, 200, 220];
    if (st === ST.PARRIED) c = [190, 120, 255];
    if (st === ST.DEAD) c = lightMode ? [132, 124, 132] : [64, 56, 66];

    drawFighter(ink, x, y, bw, bh, face, st, s[b + P.STF], s[G.TICK], c, u);

    if (s[b + P.SPL] > 0) {
      const r = s[b + P.SPL];
      ink(255, 230, 120).box(sx(s[b + P.X] + s[b + P.SPX]) - (r >> 1), py(26 * SUB), r, r);
    }

    if (boxes || st === ST.PUNCH) {
      ink(80, 255, 120, 80).box(x, y, bw, bh);
      if (st === ST.PUNCH) {
        const f = s[b + P.STF];
        if (f >= PUNCH_F.startup && f < PUNCH_F.startup + PUNCH_F.active) {
          const front = s[b + P.X] + face * (BODY_W >> 1);
          const hx = face > 0 ? front : front - PUNCH_F.reach;
          const low = !!s[b + P.LOW];
          ink(255, 60, 90, 110).box(sx(hx), py((low ? 16 : 34) * SUB), u(PUNCH_F.reach), u(low ? 8 * SUB : 16 * SUB));
        }
      }
    }
  }

  if (boxes) ink(70, 66, 90).write(`t${s[G.TICK]} ${game.checksum(s).toString(16)}`, { x: 4, y: 25, font: "MatrixChunky8" });
  if (verdict) ink(120, 255, 160).write(verdict, { x: 4, y: 33, font: "MatrixChunky8" });
  if (net) wire(ink, w);

  hud(ink, w); // last, so the round-over card covers the readout behind it
  if (pad > 4) {
    pads(ink, 4, h - 29, 0);
    pads(ink, w - 32, h - 29, 1);
  }
  paintInputStream(ink);
  paintPresence(ink, w);
  if (!loggedInHandle) paintLogin(ink, w);
  if (attract) {
    // A neutral wash makes the demo visibly separate from the live match.
    // The invitation is painted afterward so it stays crisp and warm.
    ink(...(lightMode ? [210, 207, 202, 190] : [74, 72, 82, 190])).box(0, 0, w, h);
    const msg = "press any key";
    const x = (w >> 1) - msg.length * 3;
    const y = Math.max(36, (floor >> 1) - 5);
    ink(...colors().panel).box(x - 5, y - 4, msg.length * 6 + 10, 15);
    ink(...(lightMode ? [125, 76, 0] : [255, 220, 60])).write(msg, { x, y, font: "MatrixChunky8" });
  }
}

function paintPresence(ink, w) {
  const state = lobby.state;
  const who = loggedInHandle ? `@${String(loggedInHandle).replace(/^@/, "")}` : state.handle;
  const text = `${who}  ${state.count || 1} in lobby`;
  ink(...colors().quiet).write(text, {
    x: Math.max(4, w - text.length * 6 - 4), y: 24, font: "MatrixChunky8",
  });
}

function paintLogin(ink, w) {
  if (!loginCells?.length) {
    const label = loginStatus === "error" ? "login unavailable" : "making login...";
    ink(150).write(label, { x: Math.max(4, w - label.length * 6 - 4), y: 34, font: "MatrixChunky8" });
    return;
  }
  const maxSize = Math.min(78, Math.max(42, (w / 4) | 0));
  const scale = Math.max(1, Math.floor(maxSize / loginCells.length));
  const size = loginCells.length * scale;
  const ox = w - size - 5, oy = 35;
  ink(245, 242, 232).box(ox - 2, oy - 11, size + 4, size + 13);
  ink(20, 18, 28).write("LOGIN", { x: ox, y: oy - 9, font: "MatrixChunky8" });
  for (let y = 0; y < loginCells.length; y++) for (let x = 0; x < loginCells.length; x++) {
    if (loginCells[y][x]) ink(12, 12, 18).box(ox + x * scale, oy + y * scale, scale, scale);
  }
}

function leave() {
  if (loginTimer) clearTimeout(loginTimer);
  loginTimer = null;
  lobby.leave(server);
  server = null;
}

function paintInputStream(ink) {
  for (const player of [0, 1]) inputStream.filter((e) => e.player === player).slice(0, 8).forEach((entry, row) => {
    const y = 30 + row * 10;
    const label = String(entry.label);
    const width = Math.max(9, label.length * 6 + 4);
    const x = player === 0 ? 18 : Math.max(18, screenWidth - width - 4);
    ink(...SUIT[player], 190).write(`P${player + 1}`, { x: player === 0 ? 4 : x - 14, y: y + 1, font: "MatrixChunky8" });
    if (Array.isArray(entry.color)) ink(...entry.color).box(x, y, width, 9);
    else ink(entry.color).box(x, y, width, 9);
    ink(12, 12, 18).write(label, { x: x + 2, y: y + 1, font: "MatrixChunky8" });
  });
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

  ink(90, 200, 255).write(`lag ${latency}f loss ${(loss * 100) | 0}%`, { x: 4, y: 33, font: "MatrixChunky8" });
  ink(70, 66, 90).write(`rb ${st.rollbacks} @${depth}f stall ${st.stalls}`, { x: 4, y: 41, font: "MatrixChunky8" });
  ink(70, 66, 90).write(`drop ${ls.dropped}/${ls.sent}`, { x: 4, y: 49, font: "MatrixChunky8" });

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
      else ink(...colors().empty).box(x, 17, 5, 5);
    }
    for (let i = 0; i < game.WIN_TARGET; i++) {
      const x = p === 0 ? cx - 11 - i * 5 : cx + 9 + i * 5;
      ink(...(i < s[b + P.WINS] ? [255, 220, 60] : [46, 42, 56])).box(x, 25, 3, 3);
    }
  }
  ink(lightMode ? 45 : 220).write(`${(s[G.TIMER] / 60) | 0}`, { x: cx - 5, y: 16, font: "MatrixChunky8" });
  const perf = `${fps}fps f${s[G.ROUND_FRAME]}`;
  ink(...colors().quiet).write(perf, { x: w - perf.length * 6 - 4, y: 16, font: "MatrixChunky8" });

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
  ink(...colors().panel).box((w >> 1) - 44, y - 3, 88, s[G.MATCH] ? 22 : 12);
  ink(255, 220, 60).write(msg, { x: (w >> 1) - msg.length * 3, y, font: "MatrixChunky8" });
  if (s[G.MATCH]) {
    const rematch = padInput.size ? "Start to rematch" : "r to rematch";
    ink(150).write(rematch, { x: (w >> 1) - rematch.length * 3, y: y + 9, font: "MatrixChunky8" });
  }
}

// the four keys, drawn as a d-pad that lights on press. the block key gets a
// gold rim when a parry is armed — the cooldown is the whole reason mashing
// block doesn't work, so it should be visible.
function pads(ink, ox, oy, p) {
  const pad = playerPad(p);
  // HORI NOLVA-inspired leverless geometry: four directions at left and two
  // arcing rows of three attack buttons. The dormant map stays quiet gray;
  // labels and move names light only while pressed.
  const dirs = [[14, "<", game.LEFT, 0, 9], [13, "v", game.DOWN, 9, 9], [15, ">", game.RIGHT, 18, 9], [12, "^", game.UP, 13, 0]];
  for (const [button, label, bit, dx, dy] of dirs) buttonDot(ink, ox + dx, oy + dy, label, !!(held[p] & bit), pad, button);
  const attacks = [[2, "X", "LP", game.LP], [3, "Y", "MP", game.MP], [5, "RB", "HP", game.HP], [0, "A", "LK", game.LK], [1, "B", "MK", game.MK], [7, "RT", "HK", game.HK]];
  const ax = p === 0 ? ox + 34 : ox - 53;
  attacks.forEach(([button, label, move, bit], i) => {
    const row = i > 2 ? 1 : 0, col = i % 3;
    buttonDot(ink, ax + col * 18, oy + row * 11 + (row ? 1 : 0), label, !!(held[p] & bit), pad, button, move);
  });
}

function buttonDot(ink, x, y, label, down, pad, button, move = "") {
  const colors = getButtonColors(pad?.id || "standard", button);
  ink(down ? colors.active : lightMode ? [205, 199, 190] : [38, 36, 48]).circle(x + 4, y + 4, 4, true);
  ink(...(down ? [8, 8, 12] : lightMode ? [78, 73, 88] : [105, 101, 120])).write(label, { x: x + 1, y: y + 1, font: "MatrixChunky8" });
  if (down && move) ink(colors.active).write(move, { x: x - 2, y: y - 8, font: "MatrixChunky8" });
  if (down && button === 2 && pad?.buttons.has(0)) ink("violet").write("THROW", { x: x - 5, y: y - 16, font: "MatrixChunky8" });
}

function paintPadTags(ink, ox, y, player, help) {
  const groups = [
    ["hit", help.mapping.fight?.punch || []],
    ["guard", help.mapping.fight?.block || []],
  ];
  const widths = groups.map(([action, buttons]) =>
    action.length * 6 + 4 + buttons.reduce((sum, button) => {
      const name = help.mapping.buttons?.[button]?.name || String(button);
      return sum + Math.max(9, name.length * 6 + 4) + 2;
    }, 0));
  let x = player === 0 ? ox : ox + 27 - widths.reduce((a, b) => a + b, 0) - 5;
  groups.forEach(([action, buttons], group) => {
    ink(110, 106, 130).write(action, { x, y: y + 1, font: "MatrixChunky8" });
    x += action.length * 6 + 4;
    buttons.forEach((button) => {
      const name = help.mapping.buttons?.[button]?.name || String(button);
      const width = Math.max(9, name.length * 6 + 4);
      const down = help.pad.buttons.has(button);
      const colors = getButtonColors(help.pad.id, button);
      ink(down ? colors.active : colors.inactive).box(x, y, width, 9);
      ink(down ? "black" : "white").write(name, { x: x + 2, y: y + 1, font: "MatrixChunky8" });
      x += width + 2;
    });
    if (group === 0) x += 5;
  });
}

export { boot, paint, act, sim, leave };
export const system = "fps";
