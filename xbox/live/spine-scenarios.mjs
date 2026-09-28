// spine-scenarios.mjs — the pokes the spine lab gives the rope, and what we
// measure about how it answers. The lab plays them live; the tests run them
// headless; both read the same numbers.

import { createSpine, stepSpine, step, land, impulse, local, linkError, chestIndex } from "./spine.mjs";

export const warmup = 1.5;   // seconds of standing (or moving) before the poke
export const watch = 2.5;    // seconds measured after it

// Each scenario: how the body moves before the poke (`before`), what happens
// from the poke on (`poke`, every step, with seconds since), and the one
// signed number that says how it answered (`signal`, from a bead's offset in
// the body's own frame: forward, up, right).
export const scenarios = {
  land: {
    label: "land", before() {},
    poke(spine, t) {
      if (t === 0) land(spine, 700);
      spine.intent.curl = t < .15 ? .5 : 0;          // the knees-and-core reflex
    },
    signal: (o) => -o.up,                            // how far it sinks
  },
  stop: {
    label: "stop",
    before(spine) { spine.root.vx = 460; },
    // Legs brake the hips over a few frames; nothing stops in one.
    poke(spine, t) { spine.root.vx = 460 * Math.max(0, 1 - t / .12); },
    signal: (o) => o.forward,                        // the pitch forward
  },
  carve: {
    label: "carve",
    before(spine) { spine.rhythm.gliding = true; spine.root.vx = 700; },
    poke(spine, t) {
      const yaw = Math.min(1, t / .3) * Math.PI / 2;
      spine.root.yaw = yaw;
      spine.root.vx = Math.cos(yaw) * 700; spine.root.vz = Math.sin(yaw) * 700;
    },
    signal: (o) => o.right,                          // thrown to the outside
  },
  hit: {
    label: "hit", before() {},
    poke(spine, t) { if (t === 0) impulse(spine, chestIndex(spine), 0, 0, 520); },
    signal: (o) => o.right,
  },
  reach: {
    label: "reach", before() {},
    poke(spine, t) {
      const on = t < .8;
      spine.intent.curl = on ? 1.4 : 0; spine.intent.side = on ? .35 : 0; spine.intent.twist = on ? .3 : 0;
    },
    signal: (o) => o.forward,
  },
};

// Run one poke headless. Everything is measured against where the body comes
// to rest at the end, so a pose the poke leaves behind isn't read as motion.
export function measure(name, options = {}) {
  const scenario = scenarios[name], spine = createSpine(options), n = spine.n;
  for (let t = 0; t < warmup; t += step) { scenario.before(spine); stepSpine(spine); }
  const poses = [];
  let worstLink = 0;
  for (let frame = 0; frame * step < watch; frame++) {
    const t = frame * step;
    scenario.poke(spine, t);
    stepSpine(spine);
    poses.push({ t, signals: Array.from({ length: n }, (_, i) => scenario.signal(local(spine, i))) });
    worstLink = Math.max(worstLink, linkError(spine));
  }
  const rest = poses[poses.length - 1].signals;
  const peaks = new Array(n).fill(0), peakAt = new Array(n).fill(0);
  for (const { t, signals } of poses) for (let i = 0; i < n; i++) {
    const d = Math.abs(signals[i] - rest[i]);
    if (d > peaks[i]) { peaks[i] = d; peakAt[i] = t; }
  }
  const head = n - 1, trace = poses.map(({ t, signals }) => ({ t, value: signals[head] - rest[head] }));
  const peak = peaks[head], sign = Math.sign(trace.find((s) => s.t === peakAt[head])?.value || 1);
  // Overshoot: after the peak, how far the head swings back past rest, as a
  // share of the peak. Settle: when it last strays more than 5% of the peak.
  let back = 0, settle = 0;
  for (const s of trace) {
    if (s.t > peakAt[head]) back = Math.max(back, -s.value * sign);
    if (Math.abs(s.value) > Math.max(.5, peak * .05)) settle = s.t;
  }
  return {
    name, peak, peakAt: peakAt[head], overshoot: peak ? back / peak : 0, settle,
    // The whip: how much later the head peaks than the bead above the pelvis.
    wave: peakAt[head] - peakAt[1], peakTimes: peakAt, linkError: worstLink, trace,
  };
}

// Walking has no rest to return to; it's measured by its rhythm instead:
// how far the head sways and bobs, and how much hips and shoulders counter-turn.
export function measureWalk(options = {}) {
  const spine = createSpine(options), n = spine.n, chest = chestIndex(spine);
  let sway = 0, bobLow = Infinity, bobHigh = -Infinity, counter = 0, worstLink = 0;
  for (let frame = 0; frame * step < warmup + watch; frame++) {
    spine.root.vx = 420; stepSpine(spine);
    if (frame * step < warmup) continue;
    const head = local(spine, n - 1);
    sway = Math.max(sway, Math.abs(head.right));
    bobLow = Math.min(bobLow, head.up); bobHigh = Math.max(bobHigh, head.up);
    counter = Math.max(counter, Math.abs(spine.twist[0] - spine.twist[chest]));
    worstLink = Math.max(worstLink, linkError(spine));
  }
  return { name: "walk", sway, bob: bobHigh - bobLow, counterTwist: counter, linkError: worstLink };
}

export const measureAll = (options) => [...Object.keys(scenarios).map((name) => measure(name, options)), measureWalk(options)];

// ——— Actions: the procedural animator measured, one row per action ———

import { createActor, stepActor, trigger, setMode } from "./actions.mjs";
import { limbError, limbEnd } from "./spine.mjs";

// Run an actor through `frames`, calling `drive(actor, frame)` first each
// frame, and hand every frame's body to `read`.
let bodyOptions = {};
function play(frames, setup, drive, read) {
  const actor = createActor(bodyOptions);
  setup?.(actor);
  let stretch = 0;
  for (let f = 0; f < frames; f++) {
    drive?.(actor, f); stepActor(actor);
    stretch = Math.max(stretch, limbError(actor.body), linkError(actor.body));
    read?.(actor, f);
  }
  return { actor, stretch };
}
const speedOf = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z) / step;

export function measureActions(options = {}) {
  bodyOptions = options;
  const rows = [];
  { // Walking: planted feet shouldn't skate.
    let slide = 0, planted = 0, last = null;
    const { stretch } = play(300, (a) => (a.input.forward = 1), null, (a, f) => {
      const feet = a.body.legs.map(limbEnd);
      if (last && f > 60) feet.forEach((p, i) => { if (p.y < 4.5 && last[i].y < 4.5) { slide += Math.hypot(p.x - last[i].x, p.z - last[i].z) / step; planted++; } });
      last = feet;
    });
    rows.push({ name: "walk", stat: `planted feet slide ${(slide / Math.max(1, planted)).toFixed(0)} u/s`, value: slide / Math.max(1, planted), stretch });
  }
  { // Punch: the spine turns before the fist flies, and the fist gets out.
    let twistPeak = [0, 0], fistPeak = [0, 0], reach = 0, lastTwist = 0, lastFist = null;
    const { stretch } = play(40, null, (a, f) => f === 0 && trigger(a, "punch", 1), (a, f) => {
      const chest = chestIndex(a.body), tw = a.body.twist[chest], fist = limbEnd(a.body.arms[1]);
      if (f > 4) {
        const tv = Math.abs(tw - lastTwist) / step, fv = speedOf(fist, lastFist);
        if (tv > twistPeak[0]) twistPeak = [tv, f]; if (fv > fistPeak[0]) fistPeak = [fv, f];
      }
      const body = a.body; reach = Math.max(reach, (fist.x - body.x[chest]) * Math.cos(body.root.yaw) + (fist.z - body.z[chest]) * Math.sin(body.root.yaw));
      lastTwist = tw; lastFist = fist;
    });
    const lead = (fistPeak[1] - twistPeak[1]) * step;
    rows.push({ name: "punch", stat: `spine leads fist by ${(lead * 1000).toFixed(0)} ms · fist ${fistPeak[0].toFixed(0)} u/s · reach ${reach.toFixed(0)}`, lead, reach, stretch });
  }
  { // Kick: how high and far the foot gets.
    let high = 0, far = 0;
    const { stretch } = play(45, null, (a, f) => f === 0 && trigger(a, "kick", 1), (a) => {
      const foot = limbEnd(a.body.legs[1]); high = Math.max(high, foot.y); far = Math.max(far, foot.x - a.body.root.x);
    });
    rows.push({ name: "kick", stat: `foot up ${high.toFixed(0)} · out ${far.toFixed(0)}`, high, far, stretch });
  }
  { // Jump: airtime, height, and back on both feet after.
    let up = 0, air = 0;
    const { actor, stretch } = play(150, null, (a, f) => f === 0 && trigger(a, "jump"), (a) => { up = Math.max(up, a.body.root.y); if (a.body.root.air) air += step; });
    const feet = actor.body.legs.map(limbEnd);
    rows.push({ name: "jump", stat: `air ${air.toFixed(2)} s · hips up ${(up - actor.body.o.hipHeight).toFixed(0)} · landed ${feet.every((p) => p.y < 5) ? "on both feet" : "off balance"}`, air, stretch, landed: feet.every((p) => p.y < 5) });
  }
  { // Reach low: the hand gets there because the spine bends.
    const goal = { x: 60, y: 12, z: 30 };
    let curl = 0;
    const { actor, stretch } = play(120, (a) => { a.target = goal; a.input.reach = true; }, null, (a) => (curl = a.body.reflex.curl));
    const hand = limbEnd(actor.body.arms[1]), miss = Math.hypot(hand.x - goal.x, hand.y - goal.y, hand.z - goal.z);
    rows.push({ name: "reach", stat: `hand misses by ${miss.toFixed(1)} · spine curls ${curl.toFixed(2)} rad for it`, miss, curl, stretch });
  }
  { // Grab and drag: the body follows what it holds.
    let held = 0;
    const { actor, stretch } = play(240, (a) => { a.target = { x: 40, y: 110, z: 20 }; a.input.grab = true; },
      (a, f) => { if (f > 60) a.target.x += 2; }, (a, f) => { if (f > 60 && a.grabbed) held++; });
    const gap = actor.target.x - actor.body.root.x;
    rows.push({ name: "grab", stat: `held ${(held / 179 * 100).toFixed(0)}% of the drag · body ${gap.toFixed(0)} behind`, held: held / 179, stretch });
  }
  { // Board: sideways, feet on the deck, pushing gets speed.
    const { actor, stretch } = play(240, (a) => { setMode(a, "board"); a.input.forward = 1; });
    const feet = actor.body.legs.map(limbEnd), yaw = actor.body.shape.yaw;
    rows.push({ name: "board", stat: `speed ${actor.speed.toFixed(0)} · stance ${(yaw * 57.3).toFixed(0)}° · feet ${feet.map((p) => p.y.toFixed(0)).join("/")} up`, speed: actor.speed, yaw, stretch });
  }
  { // Carve: leans into the turn.
    let lean = 0;
    const { stretch } = play(120, (a) => { setMode(a, "board"); a.speed = 700; a.input.turn = 1; }, null, (a) => (lean = Math.max(lean, a.body.shape.curl)));
    rows.push({ name: "carve", stat: `leans ${lean.toFixed(2)} rad over the toes`, lean, stretch });
  }
  { // Ollie: pops and lands on the deck.
    let up = 0;
    const { actor, stretch } = play(120, (a) => { setMode(a, "board"); a.speed = 600; }, (a, f) => f === 10 && trigger(a, "jump"), (a) => (up = Math.max(up, a.body.root.y)));
    rows.push({ name: "ollie", stat: `hips up ${(up - actor.body.o.hipHeight).toFixed(0)}`, up, stretch });
  }
  { // Kart: seated, hands on the wheel.
    const { actor, stretch } = play(180, (a) => { setMode(a, "kart"); a.input.forward = 1; });
    rows.push({ name: "kart", stat: `hips at ${actor.body.y[0].toFixed(0)} · speed ${actor.speed.toFixed(0)}`, hip: actor.body.y[0], stretch });
  }
  { // Taking a hit: shoved, recovers.
    let moved = 0;
    const { actor, stretch } = play(90, null, (a, f) => f === 0 && trigger(a, "hit", 1), (a) => (moved = Math.max(moved, Math.abs(a.body.z[a.body.n - 1] - a.body.root.z))));
    rows.push({ name: "hit", stat: `head knocked ${moved.toFixed(0)} · back ${Math.abs(actor.body.z[actor.body.n - 1] - actor.body.root.z).toFixed(0)}`, moved, stretch });
  }
  return rows;
}
