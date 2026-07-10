// fight/sim.mjs — a deterministic integer fighting simulation.
//
// every value that matters lives in one Int32Array, so a snapshot is a slice
// and a checksum is a walk. no floats reach state, and nothing here calls
// Math.random or the transcendentals — the spec calls sin/cos/pow/atan2
// "implementation-approximated" and lets engines pick their own algorithm,
// so a single Math.sin would desync chrome against safari. the toolkit is
// + - * / and Math.imul/abs/min/max, all of which are exactly specified.
//
// positions are subpixels. y is height above the floor, so y === 0 is grounded.

export const SUB = 256; // subpixels per pixel
export const STAGE_W = 256 * SUB;

export const BODY_W = 22 * SUB;
export const BODY_H = 46 * SUB;
export const CROUCH_H = 28 * SUB;

const WALK = 384; // forward, ~1.5px per frame
const BACK = 288; // retreating is slower, as it should be
const GRAV = 60;
const JUMP_V = 1500;
const AIR = 320; // horizontal momentum locked in at takeoff

const HIT_LO = 18 * SUB; // every move swings at the same height for now
const HIT_HI = 34 * SUB;

const HIT_STOP = 8;
const BLOCK_STOP = 5;
const PUSH = 3 * SUB;

// input bits. one u16 per player per frame — this is the whole wire format.
export const UP = 1,
  DOWN = 2,
  LEFT = 4,
  RIGHT = 8,
  LIGHT = 16,
  MEDIUM = 32,
  HEAVY = 64;

export const ST = {
  IDLE: 0,
  WALK: 1,
  CROUCH: 2,
  JUMP: 3,
  ATTACK: 4,
  HITSTUN: 5,
  BLOCKSTUN: 6,
  KO: 7,
};

// frame data. constant, never snapshotted.
export const MOVES = [
  { startup: 3, active: 2, recovery: 6, dmg: 30, stun: 12, block: 6, reach: 30 * SUB },
  { startup: 6, active: 3, recovery: 12, dmg: 60, stun: 16, block: 8, reach: 36 * SUB },
  { startup: 10, active: 4, recovery: 20, dmg: 100, stun: 22, block: 10, reach: 44 * SUB },
];

// per-fighter field offsets.
export const P = {
  X: 0,
  Y: 1,
  VX: 2,
  VY: 3,
  FACE: 4,
  HP: 5,
  ST: 6,
  STF: 7, // frames elapsed in ATTACK, frames remaining in stun
  MV: 8,
  STOP: 9, // hitstop
  HIT: 10, // this attack already connected
  PIN: 11, // previous frame's input, for edge detection
  SPX: 12, // hit spark — visual, but derived from the rng so it must roll back
  SPY: 13,
  SPL: 14,
};
export const PN = 15;

// what the sim wants heard this tick. the sim never plays a sound itself — it
// records the intent and lets the caller decide. a rollback resimulates frames
// that already happened, and playing audio from those is the street fighter x
// tekken bug: sounds popping and cutting out. so only the leading frame reads
// this. cleared at the top of every step, which means a rewound frame that no
// longer lands a hit silently un-schedules its own sound.
export const SFX = { SWING: 1, HIT: 2, BLOCK: 4, JUMP: 8, KO: 16 };

export const G = { TICK: 30, RNG: 31, TIMER: 32, OVER: 33, SFX: 34, SFXMV: 35 };
export const SIZE = 36;

export const START_HP = 1000;
export const ROUND_TICKS = 99 * 60;

export function create(seed = 1) {
  const s = new Int32Array(SIZE);
  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    s[b + P.X] = p === 0 ? 80 * SUB : 176 * SUB;
    s[b + P.FACE] = p === 0 ? 1 : -1;
    s[b + P.HP] = START_HP;
  }
  s[G.RNG] = seed | 0;
  s[G.TIMER] = ROUND_TICKS;
  return s;
}

export const snapshot = (s) => s.slice();
export const restore = (s, snap) => s.set(snap);

// mulberry32. integer-only, so it survives the trip across engines, and its
// state sits in the array so a rollback rewinds the randomness too.
function rnd(s) {
  const a = (s[G.RNG] + 0x6d2b79f5) | 0;
  s[G.RNG] = a;
  let t = Math.imul(a ^ (a >>> 15), 1 | a);
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
  return (t ^ (t >>> 14)) >>> 0;
}

export function checksum(s) {
  let h = 0x811c9dc5;
  for (let i = 0; i < SIZE; i++) {
    const v = s[i];
    for (let k = 0; k < 32; k += 8) {
      h ^= (v >>> k) & 255;
      h = Math.imul(h, 0x01000193);
    }
  }
  return h >>> 0;
}

const clampX = (x) => Math.min(STAGE_W - (BODY_W >> 1), Math.max(BODY_W >> 1, x));
const grounded = (s, b) => s[b + P.Y] === 0;

export function step(s, i0, i1) {
  s[G.TICK]++;
  s[G.SFX] = 0; // before every early return, so silent frames stay silent
  if (s[G.OVER]) return;

  // hitstop freezes both fighters. inputs still latch so a button held
  // through the freeze doesn't read as a fresh press on the far side.
  if (s[P.STOP] > 0 || s[PN + P.STOP] > 0) {
    if (s[P.STOP] > 0) s[P.STOP]--;
    if (s[PN + P.STOP] > 0) s[PN + P.STOP]--;
    s[P.PIN] = i0;
    s[PN + P.PIN] = i1;
    return;
  }

  if (s[G.TIMER] > 0) s[G.TIMER]--;
  for (let p = 0; p < 2; p++) if (s[p * PN + P.SPL] > 0) s[p * PN + P.SPL]--;

  face(s);
  control(s, 0, i0);
  control(s, 1, i1);
  physics(s, 0);
  physics(s, 1);
  separate(s);

  // probe both before applying either, so a trade doesn't depend on index order.
  const h0 = probe(s, 0);
  const h1 = probe(s, 1);
  if (h0) connect(s, 0);
  if (h1) connect(s, 1);

  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    if (s[b + P.HP] <= 0) {
      s[b + P.HP] = 0;
      s[b + P.ST] = ST.KO;
      s[G.OVER] = 2 - p; // 1 → p0 wins, 2 → p1 wins
      s[G.SFX] |= SFX.KO;
    }
  }
  if (!s[G.OVER] && s[G.TIMER] === 0) {
    const a = s[P.HP],
      b = s[PN + P.HP];
    s[G.OVER] = a === b ? 3 : a > b ? 1 : 2;
  }
}

function face(s) {
  for (let p = 0; p < 2; p++) {
    const b = p * PN,
      st = s[b + P.ST];
    if (st !== ST.IDLE && st !== ST.WALK && st !== ST.CROUCH) continue;
    const d = s[(1 - p) * PN + P.X] - s[b + P.X];
    if (d !== 0) s[b + P.FACE] = d > 0 ? 1 : -1;
  }
}

function control(s, p, inp) {
  const b = p * PN;
  const press = inp & ~s[b + P.PIN];
  s[b + P.PIN] = inp;

  const st = s[b + P.ST];

  if (st === ST.HITSTUN || st === ST.BLOCKSTUN) {
    if (--s[b + P.STF] <= 0) s[b + P.ST] = grounded(s, b) ? ST.IDLE : ST.JUMP;
    return;
  }

  if (st === ST.ATTACK) {
    const m = MOVES[s[b + P.MV]];
    if (++s[b + P.STF] >= m.startup + m.active + m.recovery) {
      s[b + P.ST] = ST.IDLE;
      s[b + P.HIT] = 0;
    }
    return;
  }

  if (st === ST.JUMP) return; // no air attacks yet

  const btn = press & (LIGHT | MEDIUM | HEAVY);
  if (btn) {
    s[b + P.ST] = ST.ATTACK;
    s[b + P.MV] = btn & LIGHT ? 0 : btn & MEDIUM ? 1 : 2;
    s[b + P.STF] = 0;
    s[b + P.HIT] = 0;
    s[G.SFX] |= SFX.SWING;
    s[G.SFXMV] = s[b + P.MV];
    return;
  }

  if (inp & DOWN) {
    s[b + P.ST] = ST.CROUCH;
    return;
  }

  if (press & UP) {
    s[b + P.ST] = ST.JUMP;
    s[b + P.VY] = JUMP_V;
    s[b + P.VX] = inp & RIGHT ? AIR : inp & LEFT ? -AIR : 0;
    s[G.SFX] |= SFX.JUMP;
    return;
  }

  const dir = inp & RIGHT ? 1 : inp & LEFT ? -1 : 0;
  if (dir) {
    s[b + P.X] = clampX(s[b + P.X] + dir * (dir === s[b + P.FACE] ? WALK : BACK));
    s[b + P.ST] = ST.WALK;
  } else {
    s[b + P.ST] = ST.IDLE;
  }
}

function physics(s, p) {
  const b = p * PN;
  if (s[b + P.Y] === 0 && s[b + P.VY] === 0) return;
  s[b + P.VY] -= GRAV;
  s[b + P.Y] += s[b + P.VY];
  s[b + P.X] = clampX(s[b + P.X] + s[b + P.VX]);
  if (s[b + P.Y] <= 0) {
    s[b + P.Y] = 0;
    s[b + P.VY] = 0;
    s[b + P.VX] = 0;
    if (s[b + P.ST] === ST.JUMP) s[b + P.ST] = ST.IDLE;
  }
}

function separate(s) {
  const d = s[PN + P.X] - s[P.X];
  const ad = Math.abs(d);
  if (ad >= BODY_W) return;
  const push = ((BODY_W - ad) >> 1) + 1;
  const sgn = d >= 0 ? 1 : -1;
  s[P.X] = clampX(s[P.X] - sgn * push);
  s[PN + P.X] = clampX(s[PN + P.X] + sgn * push);
}

// does p's active hitbox overlap the other fighter's hurtbox this frame?
function probe(s, p) {
  const b = p * PN;
  if (s[b + P.ST] !== ST.ATTACK || s[b + P.HIT]) return 0;

  const m = MOVES[s[b + P.MV]];
  const f = s[b + P.STF];
  if (f < m.startup || f >= m.startup + m.active) return 0;

  const fc = s[b + P.FACE];
  const front = s[b + P.X] + fc * (BODY_W >> 1);
  const x0 = fc > 0 ? front : front - m.reach;
  const x1 = fc > 0 ? front + m.reach : front;

  const ob = (1 - p) * PN;
  const oy = s[ob + P.Y];
  const oh = s[ob + P.ST] === ST.CROUCH ? CROUCH_H : BODY_H;
  const ox0 = s[ob + P.X] - (BODY_W >> 1);
  const ox1 = s[ob + P.X] + (BODY_W >> 1);

  if (x1 <= ox0 || x0 >= ox1) return 0;
  if (HIT_HI <= oy || HIT_LO >= oy + oh) return 0;
  return 1;
}

function connect(s, p) {
  const b = p * PN,
    ob = (1 - p) * PN;
  const m = MOVES[s[b + P.MV]];
  const fc = s[b + P.FACE];
  s[b + P.HIT] = 1;

  const st = s[ob + P.ST];
  const canBlock =
    grounded(s, ob) && (st === ST.IDLE || st === ST.WALK || st === ST.CROUCH);
  // backing away from the attacker is a block. the attacker faces `fc`, so the
  // defender's escape direction carries the same sign.
  const away = fc > 0 ? RIGHT : LEFT;
  const blocking = canBlock && (s[ob + P.PIN] & away) !== 0;

  if (blocking) {
    s[ob + P.ST] = ST.BLOCKSTUN;
    s[ob + P.STF] = m.block;
    s[b + P.STOP] = BLOCK_STOP;
    s[ob + P.STOP] = BLOCK_STOP;
    s[G.SFX] |= SFX.BLOCK;
  } else {
    s[G.SFX] |= SFX.HIT;
    s[G.SFXMV] = s[b + P.MV];
    s[ob + P.HP] -= m.dmg;
    s[ob + P.ST] = ST.HITSTUN;
    s[ob + P.STF] = m.stun;
    s[b + P.STOP] = HIT_STOP;
    s[ob + P.STOP] = HIT_STOP;
    s[ob + P.SPX] = (rnd(s) % (9 * SUB)) - 4 * SUB;
    s[ob + P.SPY] = 20 * SUB + (rnd(s) % (12 * SUB));
    s[ob + P.SPL] = 10;
  }

  s[b + P.X] = clampX(s[b + P.X] - fc * PUSH);
  s[ob + P.X] = clampX(s[ob + P.X] + fc * PUSH);
}

// index → name, so a desync report can say which field drifted.
export function label(i) {
  if (i >= SIZE) return `?${i}`;
  if (i >= G.TICK) {
    const g = Object.keys(G).find((k) => G[k] === i);
    return g ?? `?${i}`;
  }
  const p = (i / PN) | 0;
  const f = Object.keys(P).find((k) => P[k] === i - p * PN);
  return `p${p}.${f}`;
}
