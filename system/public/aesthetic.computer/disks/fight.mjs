// fight, 26.07.09
// hotseat versus on one qwerty. phase 0 of the rollback plan: the sim is
// deterministic and snapshot-clean, and nothing here touches the network yet.
//
//   p1  w a s d  ·  f g h   (light / medium / heavy)
//   p2  ← ↑ ↓ →  ·  j k l
//
// `fight:boxes` overlays hit and hurt boxes. `fight:synctest` runs the
// rollback harness in the browser and prints the verdict.

import * as game from "../lib/fight/sim.mjs";
import { syncTest, report } from "../lib/fight/rollback.mjs";

const { P, PN, G, ST, SFX, SUB, BODY_W, BODY_H, CROUCH_H, MOVES } = game;

const KEYS = {
  w: [0, game.UP],
  a: [0, game.LEFT],
  s: [0, game.DOWN],
  d: [0, game.RIGHT],
  f: [0, game.LIGHT],
  g: [0, game.MEDIUM],
  h: [0, game.HEAVY],
  arrowup: [1, game.UP],
  arrowleft: [1, game.LEFT],
  arrowdown: [1, game.DOWN],
  arrowright: [1, game.RIGHT],
  j: [1, game.LIGHT],
  k: [1, game.MEDIUM],
  l: [1, game.HEAVY],
};

const SUIT = [
  [90, 170, 255],
  [255, 110, 90],
];

let s; // the whole simulation, one Int32Array
let held = [0, 0];
let half = 0; // ac sims at 120hz; the game ticks at 60
let boxes = false;
let verdict; // synctest result, when asked for

function boot({ colon }) {
  s = game.create(1);
  boxes = colon.includes("boxes");
  if (colon.includes("synctest")) {
    // buttons stay rare on purpose — tap one every frame and both fighters
    // spend the match in recovery, never meet, and the test proves nothing.
    const inputs = (f) => [
      (f % 7 < 4 ? game.RIGHT : f % 7 < 6 ? game.LEFT : game.UP) |
        (f % 23 === 0 ? game.MEDIUM : 0),
      (f % 5 < 3 ? game.LEFT : game.DOWN) | (f % 31 === 0 ? game.LIGHT : 0),
    ];
    verdict = report(syncTest(game, inputs, { frames: 600, distance: 8 }));
  }
}

function sim({ sound }) {
  if ((half ^= 1)) return; // every other 120hz step
  if (s[G.OVER]) return;
  game.step(s, held[0], held[1]);
  hear(sound); // leading frame only — a rollback's resimulated frames stay mute
}

// heavier hits land lower. the sim already decided what happened; this only
// gives it a voice.
const TONE = [220, 165, 110];

function hear(sound) {
  const f = s[G.SFX];
  if (!f) return;
  const mv = s[G.SFXMV];
  const play = (o) => sound?.synth?.({ attack: 0.001, ...o });

  if (f & SFX.SWING)
    play({ type: "noise-white", duration: 0.03, decay: 0.6, volume: 0.1 });
  if (f & SFX.JUMP)
    play({ type: "triangle", tone: 330, duration: 0.05, decay: 0.7, volume: 0.14 });
  if (f & SFX.BLOCK)
    play({ type: "noise-white", duration: 0.07, decay: 0.5, volume: 0.24 });
  if (f & SFX.HIT) {
    play({ type: "square", tone: TONE[mv], duration: 0.06 + mv * 0.02, decay: 0.55, volume: 0.3 });
    play({ type: "noise-white", duration: 0.05, decay: 0.4, volume: 0.18 });
  }
  if (f & SFX.KO) {
    play({ type: "sawtooth", tone: 110, duration: 0.5, decay: 0.9, volume: 0.3 });
    play({ type: "square", tone: 55, duration: 0.6, attack: 0.02, decay: 0.95, volume: 0.18 });
  }
}

function act({ event: e }) {
  for (const key in KEYS) {
    const [p, bit] = KEYS[key];
    if (e.is(`keyboard:down:${key}`)) held[p] |= bit;
    if (e.is(`keyboard:up:${key}`)) held[p] &= ~bit;
  }
  if (e.is("keyboard:down:r") && s[G.OVER]) s = game.create(s[G.RNG]);
}

// render only. everything below reads state and writes pixels — it never
// writes back, which is what keeps the sim rollback-safe.
//
// the stage is a fixed 256 subpixel-units wide in the sim and always will be:
// if it tracked the screen, two players on differently sized windows would
// simulate different fights. so the renderer scales instead.
function paint({ wipe, ink, screen }) {
  const { width: w, height: h } = screen;
  const floor = h - (h < 110 ? 10 : 14);
  const k = w / 256; // stage → screen
  const u = (subs) => ((subs * k) / SUB) | 0; // sim length → screen pixels
  const py = (v) => floor - u(v);

  wipe(18, 16, 26);
  ink(40, 38, 58).box(0, floor, w, 1);
  for (let x = 0; x < w; x += 24) ink(30, 28, 44).box(x, floor + 1, 1, 3);

  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    const st = s[b + P.ST];
    const tall = st === ST.CROUCH ? CROUCH_H : BODY_H;
    const x = u(s[b + P.X] - (BODY_W >> 1));
    const y = py(s[b + P.Y] + tall);
    const bw = u(BODY_W);
    const bh = u(tall);
    const face = s[b + P.FACE];

    let c = SUIT[p];
    if (st === ST.HITSTUN) c = [255, 240, 200];
    if (st === ST.BLOCKSTUN) c = [190, 200, 220];
    if (st === ST.KO) c = [70, 60, 70];

    ink(...c).box(x, y, bw, bh);
    // a notch on the leading edge, so facing reads at a glance
    ink(255).box(face > 0 ? x + bw - 2 : x, y + 2, 2, 3);

    if (st === ST.ATTACK) {
      const m = MOVES[s[b + P.MV]];
      const f = s[b + P.STF];
      const live = f >= m.startup && f < m.startup + m.active;
      const arm = live ? u(m.reach) : u(m.reach) >> 2;
      const ax = face > 0 ? x + bw : x - arm;
      const ay = py(28 * SUB);
      if (live) ink(255, 255, 255).box(ax, ay, arm, 2);
      else ink(120, 120, 140).box(ax, ay, arm, 2);
    }

    if (s[b + P.SPL] > 0) {
      const r = s[b + P.SPL];
      ink(255, 230, 120).box(
        u(s[b + P.X] + s[b + P.SPX]) - (r >> 1),
        py(s[b + P.SPY]) - (r >> 1),
        r,
        r,
      );
    }

    if (boxes) {
      ink(80, 255, 120, 80).box(x, y, bw, bh); // hurt
      if (st === ST.ATTACK) {
        const m = MOVES[s[b + P.MV]];
        const f = s[b + P.STF];
        if (f >= m.startup && f < m.startup + m.active) {
          const front = s[b + P.X] + face * (BODY_W >> 1);
          const hx = face > 0 ? front : front - m.reach;
          ink(255, 60, 90, 110).box(u(hx), py(34 * SUB), u(m.reach), u(16 * SUB));
        }
      }
    }
  }

  // health sits below the piece label the hud draws in the top-left corner.
  const bar = (w >> 1) - 12;
  for (let p = 0; p < 2; p++) {
    const hp = s[p * PN + P.HP];
    const fill = ((hp * bar) / game.START_HP) | 0;
    const x = p === 0 ? 4 : w - 4 - bar;
    ink(50, 45, 60).box(x, 17, bar, 5);
    ink(...SUIT[p]).box(p === 0 ? x : x + bar - fill, 17, fill, 5);
  }
  ink(220).write(`${(s[G.TIMER] / 60) | 0}`, { x: (w >> 1) - 5, y: 16 });

  if (s[G.OVER]) {
    const msg = s[G.OVER] === 3 ? "draw" : `p${s[G.OVER]} wins`;
    ink(0, 0, 0, 200).box((w >> 1) - 42, (h >> 1) - 13, 84, 26);
    ink(255, 220, 60).write(msg, { x: (w >> 1) - msg.length * 3, y: (h >> 1) - 7 });
    ink(150).write("r to rematch", { x: (w >> 1) - 33, y: (h >> 1) + 3 });
  }

  if (boxes) ink(70, 66, 90).write(`t${s[G.TICK]} ${game.checksum(s).toString(16)}`, { x: 4, y: 25 });
  if (verdict) ink(120, 255, 160).write(verdict, { x: 4, y: 33 });

  if (h >= 110) {
    ink(58, 56, 76).write("wasd+fgh", { x: 4, y: h - 8 });
    ink(58, 56, 76).write("arrows+jkl", { x: w - 62, y: h - 8 });
  }
}

export { boot, paint, act, sim };
