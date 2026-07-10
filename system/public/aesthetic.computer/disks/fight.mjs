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

const { P, PN, G, ST, SUB, BODY_W, BODY_H, CROUCH_H, MOVES } = game;

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

function sim() {
  if ((half ^= 1)) return; // every other 120hz step
  if (s[G.OVER]) return;
  game.step(s, held[0], held[1]);
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
function paint({ wipe, ink, screen, write }) {
  const { width: w, height: h } = screen;
  const ox = (w - 256) >> 1;
  const floor = h - 18;
  const px = (v) => ox + ((v / SUB) | 0);
  const py = (v) => floor - ((v / SUB) | 0);

  wipe(18, 16, 26);
  ink(40, 38, 58).box(ox, floor, 256, 1);
  for (let x = 0; x <= 256; x += 32) ink(30, 28, 44).box(ox + x, floor + 1, 1, 4);

  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    const st = s[b + P.ST];
    const tall = st === ST.CROUCH ? CROUCH_H : BODY_H;
    const x = px(s[b + P.X] - (BODY_W >> 1));
    const y = py(s[b + P.Y] + tall);
    const bw = BODY_W / SUB;
    const bh = tall / SUB;

    let c = SUIT[p];
    if (st === ST.HITSTUN) c = [255, 240, 200];
    if (st === ST.BLOCKSTUN) c = [190, 200, 220];
    if (st === ST.KO) c = [70, 60, 70];

    ink(...c).box(x, y, bw, bh);
    // a notch on the leading edge, so facing reads at a glance
    const nose = s[b + P.FACE] > 0 ? x + bw - 2 : x;
    ink(255).box(nose, y + 3, 2, 4);

    if (st === ST.ATTACK) {
      const m = MOVES[s[b + P.MV]];
      const f = s[b + P.STF];
      const live = f >= m.startup && f < m.startup + m.active;
      const arm = live ? m.reach / SUB : (m.reach / SUB) >> 2;
      const ax = s[b + P.FACE] > 0 ? x + bw : x - arm;
      if (live) ink(255, 255, 255).box(ax, py(28 * SUB), arm, 3);
      else ink(120, 120, 140).box(ax, py(28 * SUB), arm, 3);
    }

    if (s[b + P.SPL] > 0) {
      const sx = px(s[b + P.X] + s[b + P.SPX]);
      const sy = py(s[b + P.SPY]);
      const r = s[b + P.SPL];
      ink(255, 230, 120).box(sx - (r >> 1), sy - (r >> 1), r, r);
    }

    if (boxes) {
      ink(80, 255, 120, 90).box(x, y, bw, bh); // hurt
      if (st === ST.ATTACK) {
        const m = MOVES[s[b + P.MV]];
        const f = s[b + P.STF];
        if (f >= m.startup && f < m.startup + m.active) {
          const front = s[b + P.X] + s[b + P.FACE] * (BODY_W >> 1);
          const hx = s[b + P.FACE] > 0 ? front : front - m.reach;
          ink(255, 60, 90, 120).box(
            px(hx),
            py(34 * SUB),
            m.reach / SUB,
            (34 - 18),
          );
        }
      }
    }
  }

  // health, timer, and the numbers that matter while debugging.
  for (let p = 0; p < 2; p++) {
    const hp = s[p * PN + P.HP];
    const bw = ((hp * 110) / game.START_HP) | 0;
    const x = p === 0 ? 6 : w - 6 - 110;
    ink(50, 45, 60).box(x, 6, 110, 6);
    ink(...SUIT[p]).box(p === 0 ? x : x + 110 - bw, 6, bw, 6);
  }
  ink(220).write(`${(s[G.TIMER] / 60) | 0}`, { x: (w >> 1) - 6, y: 5 });

  ink(70, 66, 90).write(`t${s[G.TICK]}  ${game.checksum(s).toString(16)}`, {
    x: 6,
    y: 16,
  });

  if (s[G.OVER]) {
    const msg = s[G.OVER] === 3 ? "draw" : `p${s[G.OVER]} wins`;
    ink(0, 0, 0, 190).box((w >> 1) - 44, (h >> 1) - 14, 88, 28);
    ink(255, 220, 60).write(msg, { x: (w >> 1) - msg.length * 3, y: (h >> 1) - 8 });
    ink(150).write("r to rematch", { x: (w >> 1) - 33, y: (h >> 1) + 2 });
  }

  ink(60, 58, 78).write("wasd+fgh", { x: 6, y: h - 9 });
  ink(60, 58, 78).write("arrows+jkl", { x: w - 62, y: h - 9 });

  if (verdict) ink(120, 255, 160).write(verdict, { x: 6, y: 26 });
}

export { boot, paint, act, sim };
