import { readFileSync } from "fs";
import * as sim from "../system/public/aesthetic.computer/lib/fight/sim.mjs";
import { syncTest, report, drills } from "../system/public/aesthetic.computer/lib/fight/rollback.mjs";

const SRC = "system/public/aesthetic.computer/lib/fight/sim.mjs";
const { P, PN, G, ST, SFX } = sim;
const p1 = (f) => PN + f;

// a scripted match. seeded separately from the sim's own rng so the input
// stream is fixed while the sim's randomness is still exercised.
//
// punches stay rare on purpose — throw one every frame and both fighters spend
// the match in recovery, never meet, and the synctest proves nothing.
function script(seed) {
  let a = seed | 0;
  const next = () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return (t ^ (t >>> 14)) >>> 0;
  };
  // weighted toward closing the distance, so the fighters actually meet.
  const dirs = (fwd, back) => [fwd, fwd, fwd, back, 0, 0];
  const tables = [dirs(sim.RIGHT, sim.LEFT), dirs(sim.LEFT, sim.RIGHT)];
  const held = [0, 0];
  // block has to be *held* across a stretch. tap it for a single frame and it
  // is always released before a fist arrives, so BLOCK and PARRY never fire
  // and the synctest is green over code it never entered.
  const guard = [-1, -1];
  return (tick) => {
    const out = [0, 0];
    for (const p of [0, 1]) {
      if (tick % (5 + p * 2) === 0) held[p] = tables[p][next() % tables[p].length];
      out[p] = held[p];
      if (next() % 23 === 0) out[p] |= sim.PUNCH;
      if (tick > guard[p] && next() % 17 === 0) guard[p] = tick + 9;
      if (tick <= guard[p]) out[p] |= sim.BLOCK;
    }
    return out;
  };
}

// walk p0 into range and leave both standing, ready to act.
function closeIn(s, frames = 40) {
  for (let f = 0; f < frames; f++) sim.step(s, sim.RIGHT, 0);
  return s;
}

// step until p0's punch resolves, collecting every sfx flag raised.
function trade(s, i1 = 0, frames = 24) {
  let flags = 0;
  for (let f = 0; f < frames; f++) {
    sim.step(s, f < 2 ? sim.PUNCH : 0, typeof i1 === "function" ? i1(f) : i1);
    flags |= s[G.SFX];
  }
  return flags;
}

describe("fight sim determinism", () => {
  it("never calls a nondeterministic builtin", () => {
    const src = readFileSync(SRC, "utf8").replace(/^\s*\/\/.*$/gm, "");
    const banned =
      /Math\.(random|sin|cos|tan|asin|acos|atan|atan2|exp|log|log2|log10|pow|hypot|cbrt|sinh|cosh|tanh)\b|Date\.now|performance\.now/;
    const hit = src.match(banned);
    expect(hit ? hit[0] : null).toBeNull();
  });

  it("produces an identical checksum stream from identical inputs", () => {
    const inputs = script(7);
    const a = sim.create(1);
    const b = sim.create(1);
    for (let f = 0; f < 900; f++) {
      const [i0, i1] = inputs(f);
      sim.step(a, i0, i1);
      sim.step(b, i0, i1);
      expect(sim.checksum(a)).toBe(sim.checksum(b));
    }
  });

  it("round-trips through snapshot and restore", () => {
    const inputs = script(11);
    const s = sim.create(1);
    for (let f = 0; f < 120; f++) sim.step(s, ...inputs(f));

    const snap = sim.snapshot(s);
    const before = sim.checksum(s);
    for (let f = 0; f < 30; f++) sim.step(s, ...inputs(f + 120));
    expect(sim.checksum(s)).not.toBe(before); // the sim actually moved

    sim.restore(s, snap);
    expect(sim.checksum(s)).toBe(before);
  });

  it("advances its rng, so the rollback tests are load-bearing", () => {
    const inputs = script(3);
    const s = sim.create(1);
    let moved = false;
    for (let f = 0; f < 900 && !moved; f++) {
      sim.step(s, ...inputs(f));
      if (s[G.RNG] !== 1) moved = true;
    }
    expect(moved).toBe(true);
  });

  // the real gate: ggpo's synctest across the whole prediction window.
  for (const distance of [1, 2, 4, 8]) {
    it(`survives a ${distance}-frame rollback every frame`, () => {
      const r = syncTest(sim, script(distance * 31), { frames: 900, distance });
      if (!r.ok) fail(report(r));
      expect(r.ok).toBe(true);
    });
  }
});

// A synctest is only worth what its inputs touch. Random play almost never
// parries (the block press has to land inside a five-frame window) and never
// clashes (two fists live on the same tick), so each mechanic gets a drill
// that provably reaches it, and each drill gets rolled back.
describe("fight sim rollback through every mechanic", () => {
  const D = drills(sim);

  const flagsOver = (inputs, frames = 900) => {
    const s = sim.create(1);
    let f = 0;
    const seen = {};
    for (; f < frames; f++) {
      sim.step(s, ...inputs(f));
      for (const k in SFX) if (s[G.SFX] & SFX[k]) seen[k] = (seen[k] ?? 0) + 1;
    }
    return seen;
  };

  it("the parry drill actually parries, and never kills", () => {
    const seen = flagsOver(D.parry);
    expect(seen.PARRY).toBeGreaterThan(4);
    expect(seen.KILL).toBeUndefined();
  });

  it("the block drill actually blocks, then breaks the guard", () => {
    const seen = flagsOver(D.block);
    expect(seen.BLOCK).toBeGreaterThan(2);
    expect(seen.BREAK).toBeGreaterThan(0);
    expect(seen.KILL).toBeGreaterThan(0);
  });

  it("the clash drill actually clashes, and never kills", () => {
    const seen = flagsOver(D.clash);
    expect(seen.CLASH).toBeGreaterThan(4);
    expect(seen.KILL).toBeUndefined();
  });

  for (const name of ["parry", "block", "clash"]) {
    for (const distance of [1, 8]) {
      it(`rolls back ${distance} frames through the ${name} drill`, () => {
        const r = syncTest(sim, D[name], { frames: 900, distance });
        if (!r.ok) fail(report(r));
        expect(r.ok).toBe(true);
      });
    }
  }
});

describe("fight sim rules", () => {
  it("kills with one punch", () => {
    const s = closeIn(sim.create(1));
    const flags = trade(s);
    expect(s[p1(P.ST)]).toBe(ST.DEAD);
    expect(flags & SFX.KILL).toBeTruthy();
    expect(s[G.OVER]).toBe(1);
    expect(s[P.WINS]).toBe(1);
  });

  it("survives a held block, at the cost of a guard pip", () => {
    const s = closeIn(sim.create(1));
    // hold block long enough that the parry window is already spent
    for (let f = 0; f < 10; f++) sim.step(s, 0, sim.BLOCK);
    expect(s[p1(P.ST)]).toBe(ST.BLOCK);

    const flags = trade(s, sim.BLOCK);
    expect(flags & SFX.BLOCK).toBeTruthy();
    expect(flags & SFX.KILL).toBeFalsy();
    expect(s[p1(P.ST)]).not.toBe(ST.DEAD);
    expect(s[p1(P.GUARD)]).toBe(sim.GUARD_MAX - 1);
  });

  it("kills through a block once guard runs out", () => {
    const s = closeIn(sim.create(1));
    s[p1(P.GUARD)] = 0;
    for (let f = 0; f < 10; f++) sim.step(s, 0, sim.BLOCK);
    const flags = trade(s, sim.BLOCK);
    expect(flags & SFX.BREAK).toBeTruthy();
    expect(flags & SFX.KILL).toBeTruthy();
    expect(s[p1(P.ST)]).toBe(ST.DEAD);
  });

  it("parries when block is tapped just before the punch lands", () => {
    const s = closeIn(sim.create(1));
    // p0's punch has 5 frames of startup; tap block on the punch frame so the
    // parry window is still open when the fist arrives.
    let flags = 0;
    for (let f = 0; f < 24; f++) {
      const i1 = f >= 3 && f < 9 ? sim.BLOCK : 0;
      sim.step(s, f < 2 ? sim.PUNCH : 0, i1);
      flags |= s[G.SFX];
    }
    expect(flags & SFX.PARRY).toBeTruthy();
    expect(flags & SFX.KILL).toBeFalsy();
    expect(s[P.ST]).toBe(ST.PARRIED); // the puncher is left standing in it
    expect(s[p1(P.ST)]).not.toBe(ST.DEAD);
  });

  it("gives the parry a free kill", () => {
    const s = closeIn(sim.create(1));
    for (let f = 0; f < 24; f++) sim.step(s, f < 2 ? sim.PUNCH : 0, f >= 3 && f < 9 ? sim.BLOCK : 0);
    expect(s[P.ST]).toBe(ST.PARRIED);
    // p1 punches the stunned p0
    for (let f = 0; f < 20; f++) sim.step(s, 0, f < 2 ? sim.PUNCH : 0);
    expect(s[P.ST]).toBe(ST.DEAD);
    expect(s[G.OVER]).toBe(2);
  });

  it("will not let you mash block into a parry", () => {
    const s = closeIn(sim.create(1));
    sim.step(s, 0, sim.BLOCK); // arms, and starts the cooldown
    expect(s[p1(P.PCD)]).toBeGreaterThan(0);
    sim.step(s, 0, 0); // release

    // re-press inside the cooldown: it's a block, never a parry
    sim.step(s, 0, sim.BLOCK);
    expect(sim.parrying(s, PN)).toBe(false);

    const flags = trade(s, sim.BLOCK);
    expect(flags & SFX.PARRY).toBeFalsy();
    expect(flags & SFX.BLOCK).toBeTruthy();
  });

  it("re-arms the parry once the cooldown expires", () => {
    const s = closeIn(sim.create(1));
    sim.step(s, 0, sim.BLOCK);
    for (let f = 0; f < sim.PARRY_CD + 2; f++) sim.step(s, 0, 0);
    expect(s[p1(P.PCD)]).toBe(0);
    sim.step(s, 0, sim.BLOCK);
    expect(sim.parrying(s, PN)).toBe(true);
  });

  it("clashes when two fists land on the same frame, and nobody dies", () => {
    const s = closeIn(sim.create(1));
    let flags = 0;
    for (let f = 0; f < 20; f++) {
      const i = f < 2 ? sim.PUNCH : 0;
      sim.step(s, i, i); // mirrored, so both fists go live together
      flags |= s[G.SFX];
    }
    expect(flags & SFX.CLASH).toBeTruthy();
    expect(flags & SFX.KILL).toBeFalsy();
    expect(s[P.ST]).not.toBe(ST.DEAD);
    expect(s[p1(P.ST)]).not.toBe(ST.DEAD);
    expect(s[G.OVER]).toBe(0);
  });

  it("bounces both fighters apart on a clash", () => {
    const s = closeIn(sim.create(1));
    const gap = () => Math.abs(s[p1(P.X)] - s[P.X]);
    let before = 0;
    for (let f = 0; f < 20; f++) {
      const i = f < 2 ? sim.PUNCH : 0;
      if (s[G.SFX] & SFX.CLASH) break;
      before = gap();
      sim.step(s, i, i);
    }
    expect(s[G.SFX] & SFX.CLASH).toBeTruthy();
    expect(gap()).toBeGreaterThan(before);
  });

  it("keeps fighters inside the stage and out of each other", () => {
    const s = sim.create(1);
    for (let f = 0; f < 400; f++) sim.step(s, sim.LEFT, sim.LEFT);
    expect(s[P.X]).toBeGreaterThanOrEqual(sim.BODY_W >> 1);
    expect(Math.abs(s[p1(P.X)] - s[P.X])).toBeGreaterThanOrEqual((sim.BODY_W >> 1) - 1);
  });
});

describe("fight sim rounds", () => {
  it("resets for the next round, keeping the wins", () => {
    const s = closeIn(sim.create(1));
    trade(s);
    expect(s[G.OVER]).toBe(1);
    const x0 = s[P.X];

    for (let f = 0; f < 130; f++) sim.step(s, 0, 0); // ride out the wait
    expect(s[G.OVER]).toBe(0);
    expect(s[P.WINS]).toBe(1); // kept
    expect(s[p1(P.GUARD)]).toBe(sim.GUARD_MAX); // restored
    expect(s[p1(P.ST)]).toBe(ST.IDLE);
    expect(s[P.X]).not.toBe(x0); // back to the spawn
  });

  it("does not let a held punch carry a fresh press into the new round", () => {
    const s = closeIn(sim.create(1));
    trade(s);
    for (let f = 0; f < 130; f++) sim.step(s, sim.PUNCH, 0); // never released
    expect(s[G.OVER]).toBe(0);
    expect(s[P.ST]).not.toBe(ST.PUNCH);
  });

  it("ends the match at three wins and freezes", () => {
    const s = sim.create(1);
    for (let round = 0; round < sim.WIN_TARGET; round++) {
      closeIn(s, 60);
      trade(s);
      if (s[G.MATCH]) break;
      for (let f = 0; f < 130; f++) sim.step(s, 0, 0);
    }
    expect(s[P.WINS]).toBe(sim.WIN_TARGET);
    expect(s[G.MATCH]).toBe(1);

    // the clock keeps counting; nothing else may move.
    const still = (x) => {
      const c = sim.snapshot(x);
      c[G.TICK] = 0;
      return sim.checksum(c);
    };
    const frozen = still(s);
    for (let f = 0; f < 60; f++) sim.step(s, sim.PUNCH, sim.PUNCH);
    expect(still(s)).toBe(frozen);
  });

  it("decides a timeout on guard remaining", () => {
    const s = sim.create(1);
    s[G.TIMER] = 3;
    s[p1(P.GUARD)] = 1; // p0 has more guard left
    for (let f = 0; f < 5; f++) sim.step(s, 0, 0);
    expect(s[G.OVER]).toBe(1);
  });
});

describe("fight sim sound", () => {
  it("records a swing as state rather than playing it", () => {
    const s = sim.create(1);
    sim.step(s, sim.PUNCH, 0);
    expect(s[G.SFX] & SFX.SWING).toBeTruthy();
  });

  it("clears the event every tick, so a silent frame stays silent", () => {
    const s = sim.create(1);
    sim.step(s, sim.PUNCH, 0);
    expect(s[G.SFX]).not.toBe(0);
    sim.step(s, sim.PUNCH, 0); // still held — no fresh press
    expect(s[G.SFX]).toBe(0);
  });

  // the ggpo case. we predicted the defender was standing there and played a
  // kill; the real input arrives and they had tapped block. after the rollback
  // that frame must sound like a parry, and the kill must never have happened.
  it("re-sounds a frame when a rollback corrects the defender's input", () => {
    const s = closeIn(sim.create(1));

    // stop one tick short of contact (the fist lands on frame 5), with p1
    // already inside the parry window
    for (let f = 0; f < 5; f++) sim.step(s, f < 2 ? sim.PUNCH : 0, f >= 3 ? sim.BLOCK : 0);
    const before = sim.snapshot(s);

    sim.step(s, 0, 0); // predicted: they let go of block
    expect(s[G.SFX] & SFX.KILL).toBeTruthy();
    expect(s[G.SFX] & SFX.PARRY).toBeFalsy();

    sim.restore(s, before); // confirmed: they were still holding it
    sim.step(s, 0, sim.BLOCK);
    expect(s[G.SFX] & SFX.PARRY).toBeTruthy();
    expect(s[G.SFX] & SFX.KILL).toBeFalsy();
  });

  it("stays mute while frozen in hitstop", () => {
    const s = closeIn(sim.create(1));
    let clashed = -1;
    for (let f = 0; f < 20 && clashed < 0; f++) {
      const i = f < 2 ? sim.PUNCH : 0;
      sim.step(s, i, i);
      if (s[G.SFX] & SFX.CLASH) clashed = f;
    }
    expect(clashed).toBeGreaterThan(-1);
    sim.step(s, 0, 0); // first hitstop frame after the clash
    expect(s[G.SFX]).toBe(0);
  });
});
