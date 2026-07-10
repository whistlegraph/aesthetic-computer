import * as sim from "../system/public/aesthetic.computer/lib/fight/sim.mjs";
import { drills } from "../system/public/aesthetic.computer/lib/fight/rollback.mjs";
import { createSession, createLink } from "../system/public/aesthetic.computer/lib/fight/session.mjs";

const FRAMES = 700;

// Ground truth: one offline sim fed the same inputs the sessions will end up
// applying. Input delay means the input pressed on frame f lands on f + delay.
function truth(script, inputDelay, frames = FRAMES) {
  const s = sim.create(1);
  for (let f = 0; f < frames; f++) {
    const src = f - inputDelay;
    const [i0, i1] = src < 0 ? [0, 0] : script(src);
    sim.step(s, i0, i1);
  }
  return s;
}

// Two peers, one hostile wire. Each only ever supplies its own input.
function play(script, { inputDelay = 2, maxPrediction = 8, frames = FRAMES, ...net } = {}) {
  const link = createLink({ seed: 99, ...net });
  const sfx = [[], []];
  const leaked = [0, 0]; // sounds emitted while replaying — must stay zero

  const mk = (player) =>
    createSession(sim, {
      player,
      inputDelay,
      maxPrediction,
      seed: 1,
      send: (p) => link.ship(p),
      onSfx: (f) => {
        if (peers[player].isReplaying()) leaked[player]++;
        if (f) sfx[player].push(f);
      },
    });

  const peers = [mk(0), mk(1)];
  link.join(peers[0], peers[1]);

  for (let t = 0; t < frames * 3; t++) {
    link.tick();
    let done = 0;
    for (const p of [0, 1]) {
      const s = peers[p];
      if (s.frame >= frames) { done++; continue; }
      s.advance(script(s.frame)[p]);
    }
    if (done === 2) break;
  }

  // settle: deliver everything still on the wire, then let each peer apply the
  // correction it implies. only now is every simulated frame confirmed.
  link.flush();
  peers[0].resync();
  peers[1].resync();

  return { peers, link, sfx, leaked };
}

const eq = (a, b) => sim.checksum(a) === sim.checksum(b);

describe("fight rollback session", () => {
  const D = drills(sim);
  const script = D.parry; // parries, blocks, stuns — plenty of divergent state

  it("with a perfect wire, nobody ever predicts wrong", () => {
    const { peers } = play(script, { latency: 0 });
    expect(peers[0].frame).toBe(FRAMES);
    expect(peers[0].stats.rollbacks).toBe(0);
    expect(peers[0].stats.stalls).toBe(0);
    expect(eq(peers[0].state, truth(script, 2))).toBe(true);
    expect(eq(peers[0].state, peers[1].state)).toBe(true);
  });

  it("matches an offline sim through latency it must predict across", () => {
    const { peers } = play(script, { latency: 6 });
    // latency past the 2-frame input delay means real mispredictions. if this
    // is zero the test proves nothing.
    expect(peers[0].stats.rollbacks).toBeGreaterThan(0);
    expect(peers[0].stats.resimFrames).toBeGreaterThan(0);
    expect(eq(peers[0].state, truth(script, 2))).toBe(true);
    expect(eq(peers[1].state, truth(script, 2))).toBe(true);
  });

  it("survives jitter and out-of-order arrival", () => {
    const { peers } = play(script, { latency: 4, jitter: 5 });
    expect(peers[0].stats.rollbacks).toBeGreaterThan(0);
    expect(eq(peers[0].state, truth(script, 2))).toBe(true);
    expect(eq(peers[0].state, peers[1].state)).toBe(true);
  });

  it("survives heavy packet loss, because packets carry a window", () => {
    const { peers, link } = play(script, { latency: 4, jitter: 3, loss: 0.35 });
    expect(link.stats().dropped).toBeGreaterThan(20); // the wire really is bad
    expect(eq(peers[0].state, truth(script, 2))).toBe(true);
    expect(eq(peers[1].state, truth(script, 2))).toBe(true);
  });

  it("stalls rather than predicting past the window, and still converges", () => {
    const { peers } = play(script, { latency: 14, maxPrediction: 8 });
    expect(peers[0].stats.stalls).toBeGreaterThan(0);
    expect(peers[0].frame).toBe(FRAMES);
    expect(eq(peers[0].state, truth(script, 2))).toBe(true);
  });

  it("never predicts further ahead than the window allows", () => {
    const link = createLink({ latency: 20, seed: 3 });
    const a = createSession(sim, { player: 0, send: (p) => link.ship(p), maxPrediction: 8 });
    const b = createSession(sim, { player: 1, send: (p) => link.ship(p), maxPrediction: 8 });
    link.join(a, b);
    let worst = 0;
    for (let t = 0; t < 400; t++) {
      link.tick();
      for (const [s, p] of [[a, 0], [b, 1]]) {
        const ahead = s.frame - s.confirmed();
        if (ahead > worst) worst = ahead;
        s.advance(script(s.frame)[p]);
      }
    }
    expect(worst).toBeLessThanOrEqual(8 + 2); // window, plus the input delay
    expect(a.stats.stalls).toBeGreaterThan(0);
  });

  it("never sounds a frame it is only replaying", () => {
    const { leaked, sfx } = play(script, { latency: 6, jitter: 4, loss: 0.2 });
    expect(leaked).toEqual([0, 0]);
    expect(sfx[0].length).toBeGreaterThan(0); // and it did sound something
  });

  it("is reproducible: same seed, same rollbacks", () => {
    const one = play(script, { latency: 5, jitter: 4, loss: 0.25 });
    const two = play(script, { latency: 5, jitter: 4, loss: 0.25 });
    expect(one.peers[0].stats).toEqual(two.peers[0].stats);
    expect(one.link.stats()).toEqual(two.link.stats());
  });

  // Input delay does not reduce how *often* we mispredict — repeat-last-input
  // is wrong exactly when the opponent's input changes, however far ahead we
  // are. What it buys is shallower rewinds. The depth is the whole identity:
  //
  //     rollback frames ≈ one-way latency − input delay
  //
  // and once the delay covers the latency, the real input is always in hand.
  it("trades input delay for rollback depth, frame for frame", () => {
    const latency = 6;
    for (const inputDelay of [0, 2, 4, 5]) {
      const { peers } = play(script, { latency, inputDelay });
      const st = peers[0].stats;
      expect(st.rollbacks).toBeGreaterThan(0);
      expect(st.resimFrames / st.rollbacks).toBe(latency - inputDelay);
      expect(eq(peers[0].state, truth(script, inputDelay))).toBe(true);
    }
  });

  it("stops rolling back entirely once delay covers the latency", () => {
    const { peers } = play(script, { latency: 6, inputDelay: 6 });
    expect(peers[0].stats.rollbacks).toBe(0);
    expect(peers[0].stats.stalls).toBe(0);
    expect(eq(peers[0].state, truth(script, 6))).toBe(true);
  });

  // every mechanic, over a wire that is actively trying to break it.
  for (const name of ["parry", "block", "clash"]) {
    it(`rolls back a real ${name} across a lossy wire`, () => {
      const { peers } = play(D[name], { latency: 6, jitter: 4, loss: 0.2 });
      expect(peers[0].stats.rollbacks).toBeGreaterThan(0);
      expect(eq(peers[0].state, truth(D[name], 2))).toBe(true);
      expect(eq(peers[0].state, peers[1].state)).toBe(true);
    });
  }
});
