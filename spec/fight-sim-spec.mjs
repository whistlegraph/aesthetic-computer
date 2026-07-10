import { readFileSync } from "fs";
import * as sim from "../system/public/aesthetic.computer/lib/fight/sim.mjs";
import { syncTest, report } from "../system/public/aesthetic.computer/lib/fight/rollback.mjs";

const SRC = "system/public/aesthetic.computer/lib/fight/sim.mjs";

// a scripted match. seeded separately from the sim's own rng so the input
// stream is fixed while the sim's randomness is still exercised.
//
// directions are held for a stretch and buttons are tapped rarely — press a
// button every frame and both fighters spend the whole match in attack
// recovery, never walk, never touch, and the synctest proves nothing.
const BUTTONS = [sim.LIGHT, sim.MEDIUM, sim.HEAVY];

function script(seed) {
  let a = seed | 0;
  const next = () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return (t ^ (t >>> 14)) >>> 0;
  };
  // weighted toward closing the distance, so the fighters actually meet.
  const dirs = (fwd, back) => [fwd, fwd, fwd, back, 0, 0, sim.DOWN, sim.UP];
  const tables = [dirs(sim.RIGHT, sim.LEFT), dirs(sim.LEFT, sim.RIGHT)];
  const held = [0, 0];
  return (tick) => {
    const out = [0, 0];
    for (const p of [0, 1]) {
      if (tick % (5 + p * 2) === 0) held[p] = tables[p][next() % tables[p].length];
      out[p] = held[p];
      if (next() % 19 === 0) out[p] |= BUTTONS[next() % 3];
    }
    return out;
  };
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
    const seed = s[sim.G.RNG];
    let moved = false;
    for (let f = 0; f < 900 && !moved; f++) {
      sim.step(s, ...inputs(f));
      if (s[sim.G.RNG] !== seed) moved = true;
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

describe("fight sim sound", () => {
  it("records a swing as state rather than playing it", () => {
    const s = sim.create(1);
    sim.step(s, sim.LIGHT, 0);
    expect(s[sim.G.SFX] & sim.SFX.SWING).toBeTruthy();
    expect(s[sim.G.SFXMV]).toBe(0);
  });

  it("clears the event every tick, so a silent frame stays silent", () => {
    const s = sim.create(1);
    sim.step(s, sim.LIGHT, 0);
    expect(s[sim.G.SFX]).not.toBe(0);
    sim.step(s, sim.LIGHT, 0); // still held — no fresh press
    expect(s[sim.G.SFX]).toBe(0);
  });

  it("flags a hit, and a block instead when the defender holds away", () => {
    const hit = sim.create(1);
    for (let f = 0; f < 40; f++) sim.step(hit, sim.RIGHT, 0);
    let flags = 0;
    for (let f = 0; f < 20; f++) {
      sim.step(hit, f < 2 ? sim.HEAVY : 0, 0);
      flags |= hit[sim.G.SFX];
    }
    expect(flags & sim.SFX.HIT).toBeTruthy();
    expect(flags & sim.SFX.BLOCK).toBeFalsy();
    expect(hit[sim.G.SFXMV]).toBe(2); // heavy

    const blk = sim.create(1);
    for (let f = 0; f < 40; f++) sim.step(blk, sim.RIGHT, 0);
    let bflags = 0;
    for (let f = 0; f < 20; f++) {
      sim.step(blk, f < 2 ? sim.HEAVY : 0, sim.RIGHT);
      bflags |= blk[sim.G.SFX];
    }
    expect(bflags & sim.SFX.BLOCK).toBeTruthy();
    expect(bflags & sim.SFX.HIT).toBeFalsy();
  });

  // the ggpo case. we predicted the defender was holding nothing and played a
  // hit; the real input arrives and they were holding back. after the rollback
  // that frame must sound like a block, and the hit must never have happened.
  it("re-sounds a frame when a rollback corrects the defender's input", () => {
    const s = sim.create(1);
    for (let f = 0; f < 40; f++) sim.step(s, sim.RIGHT, 0);
    for (let f = 0; f < 10; f++) sim.step(s, f < 2 ? sim.HEAVY : 0, 0);

    const before = sim.snapshot(s); // the tick the heavy connects on

    sim.step(s, 0, 0); // predicted: defender idle
    expect(s[sim.G.SFX] & sim.SFX.HIT).toBeTruthy();
    expect(s[sim.G.SFX] & sim.SFX.BLOCK).toBeFalsy();

    sim.restore(s, before); // confirmed: they were holding back
    sim.step(s, 0, sim.RIGHT);
    expect(s[sim.G.SFX] & sim.SFX.BLOCK).toBeTruthy();
    expect(s[sim.G.SFX] & sim.SFX.HIT).toBeFalsy();
  });

  it("stays mute while frozen in hitstop", () => {
    const s = sim.create(1);
    for (let f = 0; f < 40; f++) sim.step(s, sim.RIGHT, 0);
    let hitAt = -1;
    for (let f = 0; f < 20 && hitAt < 0; f++) {
      sim.step(s, f < 2 ? sim.HEAVY : 0, 0);
      if (s[sim.G.SFX] & sim.SFX.HIT) hitAt = f;
    }
    expect(hitAt).toBeGreaterThan(-1);
    sim.step(s, 0, 0); // first hitstop frame
    expect(s[sim.G.SFX]).toBe(0);
  });
});

describe("fight sim rules", () => {
  const idle = () => [0, 0];

  it("lands a hit and takes health", () => {
    const s = sim.create(1);
    // walk p0 into range, then poke.
    for (let f = 0; f < 40; f++) sim.step(s, sim.RIGHT, 0);
    const hp = s[sim.PN + sim.P.HP];
    for (let f = 0; f < 20; f++) sim.step(s, f < 2 ? sim.LIGHT : 0, 0);
    expect(s[sim.PN + sim.P.HP]).toBeLessThan(hp);
  });

  it("blocks when the defender holds away", () => {
    const s = sim.create(1);
    for (let f = 0; f < 40; f++) sim.step(s, sim.RIGHT, 0);
    const hp = s[sim.PN + sim.P.HP];
    // p1 is on the right, so backing away is RIGHT. blockstun is short, so
    // watch for it rather than checking one frame after the fact.
    let blocked = false;
    for (let f = 0; f < 20; f++) {
      sim.step(s, f < 2 ? sim.LIGHT : 0, sim.RIGHT);
      if (s[sim.PN + sim.P.ST] === sim.ST.BLOCKSTUN) blocked = true;
    }
    expect(blocked).toBe(true);
    expect(s[sim.PN + sim.P.HP]).toBe(hp);
  });

  it("keeps fighters inside the stage and out of each other", () => {
    const s = sim.create(1);
    for (let f = 0; f < 400; f++) sim.step(s, sim.LEFT, sim.LEFT);
    expect(s[sim.P.X]).toBeGreaterThanOrEqual(sim.BODY_W >> 1);
    expect(Math.abs(s[sim.PN + sim.P.X] - s[sim.P.X])).toBeGreaterThanOrEqual(
      (sim.BODY_W >> 1) - 1,
    );
  });

  it("ends the round when health runs out, and says so", () => {
    const s = sim.create(1);
    s[sim.PN + sim.P.HP] = 10;
    for (let f = 0; f < 40; f++) sim.step(s, sim.RIGHT, 0);
    let ko = 0;
    for (let f = 0; f < 40; f++) {
      sim.step(s, f % 20 < 2 ? sim.HEAVY : 0, 0);
      ko |= s[sim.G.SFX] & sim.SFX.KO;
    }
    expect(s[sim.G.OVER]).toBe(1);
    expect(ko).toBeTruthy();
  });

  it("idles to a timeout decision", () => {
    const s = sim.create(1);
    s[sim.G.TIMER] = 3;
    s[sim.P.HP] = 500;
    for (let f = 0; f < 5; f++) sim.step(s, ...idle());
    expect(s[sim.G.OVER]).toBe(2); // p1 kept more health
  });
});
