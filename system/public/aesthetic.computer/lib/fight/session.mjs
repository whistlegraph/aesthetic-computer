// fight/session.mjs — the rollback session. ggpo's shape, in about 200 lines.
//
// each peer simulates every frame for both players. it only ever *has* its own
// input on time, so it predicts the opponent's by repeating their last known
// one — which is right most of the time, because hands hold buttons. when the
// real input finally arrives and disagrees, the session rewinds to that frame,
// replays everything since with the correction, and lands back on the present.
// none of that is visible: the frames in between are never drawn and never
// sounded.
//
// two knobs trade against each other. `inputDelay` holds your own input back a
// few frames so the opponent's has time to arrive — it costs felt lag but buys
// fewer rollbacks. `maxPrediction` bounds how far ahead of the last confirmed
// frame we'll guess; past it we stall rather than predict into nonsense. ggpo
// caps that at 8 and so do we.
//
// this file knows nothing about transport. `send` hands a packet to whatever
// is out there; `receive` takes one back. a packet carries a *window* of recent
// inputs rather than one frame, so a dropped packet is covered by the next —
// never build retransmission on top of this, just widen the window.

const RING = 512; // frame history; a power of two so `& MASK` indexes it
const MASK = RING - 1;

export function createSession(g, opts) {
  const {
    player, // which side is local: 0 or 1
    inputDelay = 2,
    maxPrediction = 8,
    seed = 1,
    send = () => {},
    onSfx = null, // called once per *leading* frame, never during a replay
  } = opts;

  const other = 1 - player;
  const SNAP = maxPrediction + 2;
  const WINDOW = maxPrediction + inputDelay + 4; // inputs carried per packet

  // per player, per frame: the input we know, and the input we simulated with.
  // the frame tags let a ring slot say "this isn't the frame you asked for".
  const inp = [new Int32Array(RING), new Int32Array(RING)];
  const inpAt = [new Int32Array(RING).fill(-1), new Int32Array(RING).fill(-1)];
  const used = [new Int32Array(RING), new Int32Array(RING)];
  const usedAt = [new Int32Array(RING).fill(-1), new Int32Array(RING).fill(-1)];
  const lastReal = [-1, -1];

  const snaps = Array.from({ length: SNAP }, () => g.create(seed));
  const snapAt = new Int32Array(SNAP).fill(-1);

  let state = g.create(seed);
  let frame = 0;
  let firstWrong = -1; // earliest frame whose prediction turned out wrong
  let replaying = false;
  const stats = { rollbacks: 0, resimFrames: 0, stalls: 0, predicted: 0 };

  const realAt = (p, f) => (inpAt[p][f & MASK] === f ? inp[p][f & MASK] : null);

  // record a real input, and notice if we already simulated that frame wrong.
  function setReal(p, f, v) {
    if (f < 0) return;
    const i = f & MASK;
    if (inpAt[p][i] === f) return; // already have it; packets repeat on purpose

    if (f < frame && usedAt[p][i] === f && used[p][i] !== v) {
      if (firstWrong < 0 || f < firstWrong) firstWrong = f;
    }
    inpAt[p][i] = f;
    inp[p][i] = v;
    if (f > lastReal[p]) lastReal[p] = f;
  }

  // the whole predictor: repeat their last known input.
  function inputFor(p, f) {
    const real = realAt(p, f);
    if (real !== null) return real;
    stats.predicted++;
    return lastReal[p] < 0 ? 0 : (realAt(p, lastReal[p]) ?? 0);
  }

  function stepOne() {
    const f = frame;
    snaps[f % SNAP].set(state);
    snapAt[f % SNAP] = f;

    const i0 = inputFor(0, f);
    const i1 = inputFor(1, f);
    for (const [p, v] of [
      [0, i0],
      [1, i1],
    ]) {
      used[p][f & MASK] = v;
      usedAt[p][f & MASK] = f;
    }

    g.step(state, i0, i1);
    frame = f + 1;
    if (!replaying) onSfx?.(state[g.G.SFX]);
  }

  // rewind to the first bad prediction and replay the present back into place.
  function resync() {
    if (firstWrong < 0) return false;
    const target = firstWrong;
    firstWrong = -1;
    if (target >= frame) return false;
    if (snapAt[target % SNAP] !== target) return false; // fell out of the ring

    const end = frame;
    g.restore(state, snaps[target % SNAP]);
    frame = target;
    replaying = true;
    while (frame < end) stepOne();
    replaying = false;

    stats.rollbacks++;
    stats.resimFrames += end - target;
    return true;
  }

  function packet() {
    const last = lastReal[player];
    const start = Math.max(0, last - WINDOW + 1);
    const inputs = [];
    for (let f = start; f <= last; f++) inputs.push(realAt(player, f) ?? 0);
    return { from: player, start, inputs, ack: lastReal[other] };
  }

  // both peers agree the opening frames are empty, so nobody has to send them.
  for (let f = 0; f < inputDelay; f++) {
    setReal(0, f, 0);
    setReal(1, f, 0);
  }

  return {
    get frame() {
      return frame;
    },
    get state() {
      return state;
    },
    get stats() {
      return stats;
    },
    isReplaying: () => replaying,
    checksum: () => g.checksum(state),
    confirmed: () => Math.min(lastReal[0], lastReal[1]),

    // too far ahead of what we actually know? then wait — a prediction that
    // stale is worse than a dropped frame.
    stalled: () => frame - lastReal[other] > maxPrediction,

    // returns "ok" or "stalled". a stalled frame consumes no input: the caller
    // hands the same one back next tick.
    advance(localInput) {
      if (frame - lastReal[other] > maxPrediction) {
        stats.stalls++;
        return "stalled";
      }
      setReal(player, frame + inputDelay, localInput);
      send(packet());
      resync();
      stepOne();
      return "ok";
    },

    receive(p) {
      if (p.from === player) return;
      for (let k = 0; k < p.inputs.length; k++) setReal(p.from, p.start + k, p.inputs[k]);
    },

    // apply any pending correction without advancing. used to settle at the end
    // of a match, and by tests to compare against ground truth.
    resync,
  };
}

// A deliberately hostile transport, for testing and for the piece's lag demo.
// Latency and jitter are in frames; loss is a fraction. Everything is driven by
// a seeded integer prng so a failing run reproduces exactly.
export function createLink({ latency = 0, jitter = 0, loss = 0, seed = 1 } = {}) {
  let a = seed | 0;
  const next = () => {
    a = (a + 0x6d2b79f5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return (t ^ (t >>> 14)) >>> 0;
  };

  let now = 0;
  const wire = [];
  const peers = [];
  const dropped = [];
  let sent = 0;

  return {
    join(s0, s1) {
      peers[0] = s0;
      peers[1] = s1;
    },
    ship(packet) {
      sent++;
      if (loss > 0 && next() % 1000 < loss * 1000) {
        dropped.push(packet.from);
        return;
      }
      const delay = latency + (jitter ? next() % (jitter + 1) : 0);
      wire.push({ at: now + delay, to: 1 - packet.from, packet });
    },
    // deliver everything due. out-of-order arrival is normal and fine.
    tick() {
      now++;
      for (let i = wire.length - 1; i >= 0; i--) {
        if (wire[i].at <= now) {
          peers[wire[i].to]?.receive(wire[i].packet);
          wire.splice(i, 1);
        }
      }
    },
    // run the wire dry, ignoring latency. for settling at the end of a match.
    flush() {
      for (const m of wire) peers[m.to]?.receive(m.packet);
      wire.length = 0;
    },
    stats: () => ({ sent, dropped: dropped.length, inFlight: wire.length }),
  };
}
