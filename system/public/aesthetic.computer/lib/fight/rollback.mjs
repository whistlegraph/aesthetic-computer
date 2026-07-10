// fight/rollback.mjs — the safety net that has to be green before any of the
// networking gets written.
//
// syncTest is ggpo's `ggpo_start_synctest`, and factorio's "heavy mode" wearing
// a different hat: every frame, rewind `distance` frames and replay them from
// the snapshot. identical inputs must land on an identical checksum. if they
// don't, some state lives outside the snapshot — a module-level variable, an
// unrestored rng cursor, a cached derived value — and rollback over a wire
// would desync in a way that is miserable to debug at 60hz across two machines.
//
// this catches it locally, on one machine, in a single process.

// A deterministic input script that drills every mechanic in turn: closing the
// distance, parrying, blocking until guard breaks, and clashing.
//
// random inputs will not do this. a parry needs a fresh block press landing
// within a few frames of the fist, and a clash needs two fists live on the same
// tick — neither happens by chance often enough to trust. a synctest over
// inputs that never parry is green over code it never entered.
//
// `g` is the sim module, so the bit values come from one place.
// One drill per mechanic, rather than one script that tries to do everything —
// a round ending mid-script shifts every later phase out of alignment, so the
// combined version silently stopped reaching the clash and the guard break.
//
// p0 walks back in between punches: a parry shoves him seven pixels away and a
// block three, so without that the drills drift out of range and whiff.
//
// `g` is the sim module, so the bit values come from one place.
export function drills(g) {
  const jab = (c) => (c < 2 ? g.PUNCH : g.RIGHT);
  return {
    // p1 taps block so the window is open when the fist lands. the cycle has to
    // clear both the parry cooldown and the attacker's parried stun.
    parry: (f) => {
      const c = f % 60;
      return [jab(c), c >= 3 && c < 9 ? g.BLOCK : 0];
    },
    // p1 holds guard down. three pips, then the fourth punch breaks it through.
    block: (f) => [jab(f % 30), g.BLOCK],
    // mirrored punches: two live fists on the same tick.
    clash: (f) => {
      const punch = f % 26 < 2 ? g.PUNCH : 0;
      return [g.RIGHT | punch, g.LEFT | punch];
    },
  };
}

// a fixed ring of snapshots, sized to the prediction window.
export function ring(sim, len) {
  const slots = Array.from({ length: len }, () => sim.create());
  const ticks = new Int32Array(len).fill(-1);
  return {
    save(tick, s) {
      const i = tick % len;
      slots[i].set(s);
      ticks[i] = tick;
    },
    load(tick, s) {
      const i = tick % len;
      if (ticks[i] !== tick) return false;
      s.set(slots[i]);
      return true;
    },
    has: (tick) => ticks[tick % len] === tick,
  };
}

// walk two states and report the fields that differ.
export function diff(sim, a, b) {
  const out = [];
  for (let i = 0; i < sim.SIZE; i++)
    if (a[i] !== b[i]) out.push({ field: sim.label(i), live: a[i], replay: b[i] });
  return out;
}

// `inputs(tick)` returns [i0, i1]. returns { ok } or a desync report.
export function syncTest(sim, inputs, { frames = 600, distance = 2 } = {}) {
  const live = sim.create();
  const snaps = [];
  const ins = [];
  const sums = [];

  for (let f = 0; f < frames; f++) {
    snaps[f] = sim.snapshot(live);
    ins[f] = inputs(f);
    sim.step(live, ins[f][0], ins[f][1]);
    sums[f] = sim.checksum(live);

    const from = f - distance + 1;
    if (from < 0) continue;

    const replay = sim.snapshot(snaps[from]);
    for (let g = from; g <= f; g++) sim.step(replay, ins[g][0], ins[g][1]);

    if (sim.checksum(replay) !== sums[f]) {
      return {
        ok: false,
        frame: f,
        from,
        distance,
        fields: diff(sim, live, replay),
      };
    }
  }
  return { ok: true, frames, distance };
}

export function report(r) {
  if (r.ok) return `synctest ok — ${r.frames} frames, distance ${r.distance}`;
  const fields = r.fields
    .map((d) => `${d.field}: live ${d.live} ≠ replay ${d.replay}`)
    .join("\n  ");
  return `desync at frame ${r.frame} (replayed from ${r.from})\n  ${fields}`;
}
