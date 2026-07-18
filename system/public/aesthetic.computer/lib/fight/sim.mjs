// fight/sim.mjs — a deterministic integer fighting simulation.
//
// one punch kills. that leaves a triangle: a punch beats a standing opponent,
// a block survives it but spends a guard pip, and a parry beats the punch
// outright and hands back a free kill. run out of guard and the next blocked
// punch goes through you.
//
// there is no parry button. tapping block arms a few frames of parry; holding
// it is just a block. mashing gets you nothing, because arming a parry starts
// a cooldown.
//
// Movement, gravity, dashes, attack strengths, throws and low attacks are all
// integer state so the richer move set remains safe to rewind.
//
// every value that matters lives in one Int32Array, so a snapshot is a slice
// and a checksum is a walk. no floats reach state, and nothing here calls
// Math.random or the transcendentals — the spec calls sin/cos/pow
// "implementation-approximated" and lets engines pick their own algorithm,
// so a single Math.sin would desync chrome against safari. the toolkit is
// + - * / and Math.imul/abs/min/max, all of which are exactly specified.

export const SUB = 256; // subpixels per pixel
export const STAGE_W = 256 * SUB;

export const BODY_W = 22 * SUB;
export const BODY_H = 46 * SUB;

const WALK = 384; // forward, ~1.5px per frame
const BACK = 288; // retreating is slower, as it should be
const JUMP_V = 1450;
const GRAVITY = 105;
const DASH = 1050;
export const DASH_WINDOW = 12;

export const PUNCH_F = { startup: 5, active: 3, recovery: 14, reach: 34 * SUB };
const PUNCH_TOTAL = PUNCH_F.startup + PUNCH_F.active + PUNCH_F.recovery;

export const PARRY_WINDOW = 5; // frames of a fresh block press that parry
export const PARRY_CD = 45; // …and how long until you may arm another
const PARRIED_STUN = 34; // long enough that the punish is guaranteed
const BLOCKSTUN = 10;

const KILL_STOP = 0; // the round freeze covers it
const PARRY_STOP = 14;
const BLOCK_STOP = 4;
const CLASH_STOP = 10;
const PUSH = 3 * SUB;
const PARRY_PUSH = 7 * SUB;
const CLASH_PUSH = 11 * SUB;

export const GUARD_MAX = 3;
export const WIN_TARGET = 3;
export const ROUND_TICKS = 20 * 60;
const WAIT_TICKS = 110; // the beat between rounds

// Input bits per player per frame — this is the whole wire format.
export const LEFT = 1, RIGHT = 2, BLOCK = 4, PUNCH = 8,
  UP = 16, DOWN = 32, LP = 64, MP = 128, HP = 256,
  LK = 512, MK = 1024, HK = 2048;
export const ATTACK_MASK = PUNCH | LP | MP | HP | LK | MK | HK;
export const ATK = { NONE: 0, LP: 1, MP: 2, HP: 3, LK: 4, MK: 5, HK: 6, THROW: 7 };
export const ATTACK_NAMES = ["", "light punch", "medium punch", "heavy punch", "light kick", "medium kick", "heavy kick", "grapple throw"];

export const ST = {
  IDLE: 0,
  WALK: 1,
  PUNCH: 2,
  BLOCK: 3,
  BLOCKSTUN: 4,
  PARRIED: 5,
  DEAD: 6,
  JUMP: 7,
  CROUCH: 8,
  DASH: 9,
};

// what the sim wants heard this tick. the sim never plays a sound itself — it
// records the intent and lets the caller decide. a rollback resimulates frames
// that already happened, and playing audio from those is the street fighter x
// tekken bug: sounds popping and cutting out. so only the leading frame reads
// this. cleared at the top of every step, which means a rewound frame that no
// longer lands a hit silently un-schedules its own sound.
export const SFX = {
  SWING: 1,
  KILL: 2,
  BLOCK: 4,
  PARRY: 8,
  BREAK: 16,
  CLASH: 32,
  ROUND: 64,
  MATCH: 128,
};

export const P = {
  X: 0,
  FACE: 1,
  ST: 2,
  STF: 3, // frames into PUNCH/BLOCK, frames left of stun
  PCD: 4, // parry cooldown
  HIT: 5, // this punch already connected
  PIN: 6, // previous frame's input, for edge detection
  GUARD: 7,
  WINS: 8,
  STOP: 9, // hitstop
  SPX: 10, // spark — visual, but drawn from the rng so it must roll back
  SPL: 11,
  Y: 12,
  VY: 13,
  TAPL: 14,
  TAPR: 15,
  ATK: 16,
  LOW: 17,
  RESULT: 18, // 1 success, -1 failed/blocked
};
export const PN = 19;

export const G = {
  TICK: 38,
  RNG: 39,
  TIMER: 40,
  OVER: 41,
  WAIT: 42,
  MATCH: 43,
  SFX: 44,
  ROUND_FRAME: 45,
};
export const SIZE = 46;

export function create(seed = 1) {
  const s = new Int32Array(SIZE);
  s[G.RNG] = seed | 0;
  spawn(s, 0, 0);
  s[G.TIMER] = ROUND_TICKS;
  return s;
}

// place the fighters; everything except wins, rng and the clock.
function spawn(s, i0, i1) {
  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    s[b + P.X] = p === 0 ? 80 * SUB : 176 * SUB;
    s[b + P.FACE] = p === 0 ? 1 : -1;
    s[b + P.ST] = ST.IDLE;
    s[b + P.STF] = 0;
    s[b + P.PCD] = 0;
    s[b + P.HIT] = 0;
    s[b + P.GUARD] = GUARD_MAX;
    s[b + P.STOP] = 0;
    s[b + P.SPX] = 0;
    s[b + P.SPL] = 0;
    s[b + P.Y] = s[b + P.VY] = 0;
    s[b + P.TAPL] = s[b + P.TAPR] = 99;
    s[b + P.ATK] = s[b + P.LOW] = s[b + P.RESULT] = 0;
    // seed the edge detector with what's actually held, or a punch key still
    // down from last round reads as a fresh press the instant we un-pause.
    s[b + P.PIN] = p === 0 ? i0 : i1;
  }
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

// is p in a fresh-enough block to parry?
export const parrying = (s, b) =>
  s[b + P.ST] === ST.BLOCK && s[b + P.STF] < PARRY_WINDOW;

export function step(s, i0, i1) {
  s[G.TICK]++;
  s[G.SFX] = 0; // before every early return, so silent frames stay silent
  if (s[G.MATCH]) return;
  s[G.ROUND_FRAME]++;

  if (s[G.OVER]) {
    fade(s);
    if (--s[G.WAIT] <= 0) {
      spawn(s, i0, i1);
      s[G.TIMER] = ROUND_TICKS;
      s[G.OVER] = 0;
      s[G.ROUND_FRAME] = 0;
      s[G.SFX] |= SFX.ROUND;
    }
    return;
  }

  // hitstop freezes both fighters. inputs still latch so a button held
  // through the freeze doesn't read as a fresh press on the far side.
  if (s[P.STOP] > 0 || s[PN + P.STOP] > 0) {
    if (s[P.STOP] > 0) s[P.STOP]--;
    if (s[PN + P.STOP] > 0) s[PN + P.STOP]--;
    s[P.PIN] = i0;
    s[PN + P.PIN] = i1;
    return;
  }

  fade(s);
  if (s[G.TIMER] > 0) s[G.TIMER]--;
  for (let p = 0; p < 2; p++) if (s[p * PN + P.PCD] > 0) s[p * PN + P.PCD]--;

  face(s);
  control(s, 0, i0);
  control(s, 1, i1);
  separate(s);

  // probe both before applying either, so the outcome doesn't depend on index
  // order. two live fists on the same tick is a clash, not two deaths.
  const h0 = probe(s, 0);
  const h1 = probe(s, 1);
  if (h0 && h1) clash(s);
  else if (h0) connect(s, 0);
  else if (h1) connect(s, 1);

  const d0 = s[P.ST] === ST.DEAD;
  const d1 = s[PN + P.ST] === ST.DEAD;
  if (d0 || d1) end(s, d0 && d1 ? 3 : d0 ? 2 : 1);
  else if (s[G.TIMER] === 0) {
    const g0 = s[P.GUARD],
      g1 = s[PN + P.GUARD];
    end(s, g0 === g1 ? 3 : g0 > g1 ? 1 : 2);
  }
}

function fade(s) {
  for (let p = 0; p < 2; p++) if (s[p * PN + P.SPL] > 0) s[p * PN + P.SPL]--;
}

function end(s, who) {
  s[G.OVER] = who;
  s[G.WAIT] = WAIT_TICKS;
  if (who === 1) s[P.WINS]++;
  else if (who === 2) s[PN + P.WINS]++;

  if (s[P.WINS] >= WIN_TARGET) s[G.MATCH] = 1;
  else if (s[PN + P.WINS] >= WIN_TARGET) s[G.MATCH] = 2;
  if (s[G.MATCH]) s[G.SFX] |= SFX.MATCH;
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
  if (st === ST.DEAD) return;

  if (st === ST.BLOCKSTUN || st === ST.PARRIED) {
    if (--s[b + P.STF] <= 0) s[b + P.ST] = ST.IDLE;
    return;
  }

  if (st === ST.PUNCH) {
    if (++s[b + P.STF] >= PUNCH_TOTAL) {
      if (!s[b + P.HIT]) s[b + P.RESULT] = -1;
      s[b + P.ST] = ST.IDLE;
      s[b + P.HIT] = 0;
    }
    return;
  }

  // Vertical motion is integer-only and therefore rollback safe.
  if (s[b + P.Y] || s[b + P.VY]) {
    s[b + P.Y] += s[b + P.VY];
    s[b + P.VY] -= GRAVITY;
    if (s[b + P.Y] <= 0) {
      s[b + P.Y] = s[b + P.VY] = 0;
      s[b + P.ST] = ST.IDLE;
    } else s[b + P.ST] = ST.JUMP;
    const airDir = inp & RIGHT ? 1 : inp & LEFT ? -1 : 0;
    if (airDir) s[b + P.X] = clampX(s[b + P.X] + airDir * BACK);
    return;
  }

  if (st === ST.BLOCK) {
    const blockDir = inp & RIGHT ? 1 : inp & LEFT ? -1 : 0;
    if (!(inp & BLOCK) && blockDir !== -s[b + P.FACE]) s[b + P.ST] = ST.IDLE;
    else s[b + P.STF]++;
    return;
  }

  // X+A on the same frame is a throw, before either single attack can win.
  const attackPress = press & ATTACK_MASK;
  if (attackPress) {
    let attack = ATK.LP;
    if ((inp & LP) && (inp & LK)) attack = ATK.THROW;
    else if (attackPress & HP) attack = ATK.HP;
    else if (attackPress & MP) attack = ATK.MP;
    else if (attackPress & LP) attack = ATK.LP;
    else if (attackPress & HK) attack = ATK.HK;
    else if (attackPress & MK) attack = ATK.MK;
    else if (attackPress & LK) attack = ATK.LK;
    s[b + P.ST] = ST.PUNCH;
    s[b + P.STF] = 0;
    s[b + P.HIT] = 0;
    s[b + P.ATK] = attack;
    s[b + P.LOW] = (inp & DOWN) && attack >= ATK.LK && attack <= ATK.HK ? 1 : 0;
    s[b + P.RESULT] = 0;
    s[G.SFX] |= SFX.SWING;
    return;
  }

  if (press & UP) {
    s[b + P.Y] = 1;
    s[b + P.VY] = JUMP_V;
    s[b + P.ST] = ST.JUMP;
    return;
  }

  const dir = inp & RIGHT ? 1 : inp & LEFT ? -1 : 0;
  const away = dir && dir === -s[b + P.FACE];
  if ((inp & BLOCK) || away) {
    s[b + P.ST] = ST.BLOCK;
    if (s[b + P.PCD] === 0) {
      s[b + P.STF] = 0; // armed: the next few frames parry
      s[b + P.PCD] = PARRY_CD;
    } else {
      s[b + P.STF] = PARRY_WINDOW; // already spent — this is only a block
    }
    return;
  }

  if (press & LEFT) {
    if (s[b + P.TAPL] <= DASH_WINDOW) s[b + P.STF] = 8;
    s[b + P.TAPL] = 0;
  }
  if (press & RIGHT) {
    if (s[b + P.TAPR] <= DASH_WINDOW) s[b + P.STF] = 8;
    s[b + P.TAPR] = 0;
  }
  s[b + P.TAPL]++; s[b + P.TAPR]++;
  if (inp & DOWN) {
    s[b + P.ST] = ST.CROUCH;
    return;
  }
  if (dir) {
    const dashing = s[b + P.STF] > 0;
    s[b + P.X] = clampX(s[b + P.X] + dir * (dashing ? DASH : dir === s[b + P.FACE] ? WALK : BACK));
    if (dashing) s[b + P.STF]--;
    s[b + P.ST] = dashing ? ST.DASH : ST.WALK;
  } else {
    s[b + P.ST] = ST.IDLE;
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

// does p's active fist overlap the other fighter this frame?
function probe(s, p) {
  const b = p * PN;
  if (s[b + P.ST] !== ST.PUNCH || s[b + P.HIT]) return 0;

  const f = s[b + P.STF];
  if (f < PUNCH_F.startup || f >= PUNCH_F.startup + PUNCH_F.active) return 0;

  const ob = (1 - p) * PN;
  if (s[ob + P.ST] === ST.DEAD) return 0;

  const fc = s[b + P.FACE];
  const front = s[b + P.X] + fc * (BODY_W >> 1);
  const reach = s[b + P.ATK] === ATK.THROW ? 18 * SUB : PUNCH_F.reach;
  const x0 = fc > 0 ? front : front - reach;
  const x1 = fc > 0 ? front + reach : front;

  const ox0 = s[ob + P.X] - (BODY_W >> 1);
  const ox1 = s[ob + P.X] + (BODY_W >> 1);
  return x1 <= ox0 || x0 >= ox1 ? 0 : 1;
}

// fists meet. nobody dies, both bounce, both come out neutral and free to act.
function clash(s) {
  for (let p = 0; p < 2; p++) {
    const b = p * PN;
    s[b + P.ST] = ST.IDLE;
    s[b + P.STF] = 0;
    s[b + P.HIT] = 0;
    s[b + P.STOP] = CLASH_STOP;
    s[b + P.SPX] = (rnd(s) % (7 * SUB)) - 3 * SUB;
    s[b + P.SPL] = 10;
  }
  // shove each fighter away from the other, whichever way they happen to face.
  const sgn = s[PN + P.X] >= s[P.X] ? 1 : -1;
  s[P.X] = clampX(s[P.X] - sgn * CLASH_PUSH);
  s[PN + P.X] = clampX(s[PN + P.X] + sgn * CLASH_PUSH);
  s[G.SFX] |= SFX.CLASH;
}

function connect(s, p) {
  const b = p * PN,
    ob = (1 - p) * PN;
  const fc = s[b + P.FACE];
  s[b + P.HIT] = 1;
  const attack = s[b + P.ATK];
  const lowKick = !!s[b + P.LOW];

  // parry: they tapped block just in time. the punch is thrown away and the
  // attacker is left standing in it.
  if (attack !== ATK.THROW && !lowKick && parrying(s, ob)) {
    s[b + P.ST] = ST.PARRIED;
    s[b + P.STF] = PARRIED_STUN;
    s[ob + P.ST] = ST.IDLE;
    s[ob + P.STF] = 0;
    s[b + P.STOP] = PARRY_STOP;
    s[ob + P.STOP] = PARRY_STOP;
    s[G.SFX] |= SFX.PARRY;
    s[b + P.X] = clampX(s[b + P.X] - fc * PARRY_PUSH);
    s[b + P.RESULT] = -1;
    return;
  }

  const blocking = s[ob + P.ST] === ST.BLOCK;

  if (attack !== ATK.THROW && !lowKick && blocking && s[ob + P.GUARD] > 0) {
    s[ob + P.GUARD]--;
    s[ob + P.ST] = ST.BLOCKSTUN;
    s[ob + P.STF] = BLOCKSTUN;
    s[b + P.STOP] = BLOCK_STOP;
    s[ob + P.STOP] = BLOCK_STOP;
    s[G.SFX] |= SFX.BLOCK;
    s[b + P.X] = clampX(s[b + P.X] - fc * PUSH);
    s[ob + P.X] = clampX(s[ob + P.X] + fc * PUSH);
    s[b + P.RESULT] = -1;
    return;
  }

  // out of guard, or never guarding at all.
  s[ob + P.ST] = ST.DEAD;
  s[b + P.RESULT] = 1;
  s[b + P.STOP] = KILL_STOP;
  s[ob + P.STOP] = KILL_STOP;
  s[G.SFX] |= blocking ? SFX.KILL | SFX.BREAK : SFX.KILL;
  s[ob + P.SPX] = (rnd(s) % (9 * SUB)) - 4 * SUB;
  s[ob + P.SPL] = 14;
}

// index → name, so a desync report can say which field drifted.
export function label(i) {
  if (i >= SIZE) return `?${i}`;
  if (i >= G.TICK) return Object.keys(G).find((k) => G[k] === i) ?? `?${i}`;
  const p = (i / PN) | 0;
  const f = Object.keys(P).find((k) => P[k] === i - p * PN);
  return `p${p}.${f}`;
}
