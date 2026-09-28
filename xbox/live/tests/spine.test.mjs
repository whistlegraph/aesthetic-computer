import test from "node:test";
import assert from "node:assert/strict";
import { createSpine, stepSpine, local, land, impulse } from "../spine.mjs";
import { measure, measureWalk, scenarios } from "../spine-scenarios.mjs";

const run = (frames, drive) => {
  const spine = createSpine();
  for (let f = 0; f < frames; f++) { drive(spine, f); stepSpine(spine); }
  return spine;
};
// A busy minute: walking, turning, landing, hits and reaches, all scripted.
const busy = (spine, f) => {
  spine.root.yaw += Math.sin(f / 40) * .03;
  const speed = f % 300 < 200 ? 420 : 0;
  spine.root.vx = Math.cos(spine.root.yaw) * speed; spine.root.vz = Math.sin(spine.root.yaw) * speed;
  if (f % 97 === 0) land(spine, 700);
  if (f % 131 === 0) impulse(spine, 6, 0, 0, f % 2 ? 520 : -520);
  spine.intent.curl = f % 250 > 200 ? 1.4 : 0;
};

test("same inputs give the same body, bead for bead", () => {
  const a = run(3600, busy), b = run(3600, busy);
  assert.deepEqual([...a.x, ...a.y, ...a.z, ...a.twist], [...b.x, ...b.y, ...b.z, ...b.twist]);
});

test("a minute of rough handling never goes NaN, and links barely stretch", () => {
  const spine = createSpine();
  let worst = 0;
  for (let f = 0; f < 3600; f++) {
    busy(spine, f); stepSpine(spine);
    for (let i = 0; i < spine.n - 1; i++) {
      const d = Math.hypot(spine.x[i + 1] - spine.x[i], spine.y[i + 1] - spine.y[i], spine.z[i + 1] - spine.z[i]);
      worst = Math.max(worst, Math.abs(d - spine.segment) / spine.segment);
    }
  }
  assert.ok([...spine.x, ...spine.y, ...spine.z].every(Number.isFinite));
  assert.ok(worst < .08, `worst link stretch ${(worst * 100).toFixed(1)}%`);
});

test("left alone, it stands: head above the pelvis, nearly full height", () => {
  const spine = run(300, () => {}), head = local(spine, spine.n - 1);
  assert.ok(head.up > spine.o.length * .9, `head ${head.up.toFixed(1)} up`);
  assert.ok(Math.abs(head.right) < 1 && Math.abs(head.forward) < 12);
});

// The feel: loose (it moves and swings back), but it comes back.
for (const name of Object.keys(scenarios)) test(`${name}: moves, stays in one piece, and settles`, () => {
  const m = measure(name);
  assert.ok(m.peak > 5, `peak ${m.peak.toFixed(1)}`);
  assert.ok(m.linkError < .06, `link ${(m.linkError * 100).toFixed(1)}%`);
  assert.ok(m.settle < 2, `settle ${m.settle.toFixed(2)}s`);
});

test("stops and hits are springy: the head swings back past rest", () => {
  for (const name of ["stop", "hit", "carve"]) {
    const m = measure(name);
    assert.ok(m.overshoot > .15 && m.overshoot < 1, `${name} overshoot ${(m.overshoot * 100).toFixed(0)}%`);
  }
});

test("a stop whips: the head arrives after the lower back", () => {
  const m = measure("stop");
  assert.ok(m.wave >= .05, `wave ${(m.wave * 1000).toFixed(0)}ms`);
});

test("walking comes from the spine: hips and shoulders counter-turn, the head sways", () => {
  const m = measureWalk();
  assert.ok(m.counterTwist > .2, `counter-twist ${m.counterTwist.toFixed(2)} rad`);
  assert.ok(m.sway > 4 && m.sway < 40, `sway ${m.sway.toFixed(1)}`);
});

// ——— the procedural animator on top of it ———
import { measureActions } from "../spine-scenarios.mjs";
import { createActor, stepActor, trigger, setMode } from "../actions.mjs";

const actionRows = Object.fromEntries(measureActions().map((r) => [r.name, r]));

test("every action keeps bones and links in one piece", () => {
  for (const [name, row] of Object.entries(actionRows))
    assert.ok(row.stretch < .1, `${name} stretches ${(row.stretch * 100).toFixed(1)}%`);
});
test("walking feet stay planted (drift under 15% of walking speed)", () => {
  assert.ok(actionRows.walk.value < 30, `slide ${actionRows.walk.value.toFixed(0)} u/s`);
});
test("a punch starts in the spine: the twist peaks before the fist", () => {
  assert.ok(actionRows.punch.lead > .03, `lead ${(actionRows.punch.lead * 1000).toFixed(0)} ms`);
  assert.ok(actionRows.punch.reach > 45, `fist reaches ${actionRows.punch.reach.toFixed(0)}`);
});
test("jumps are short and heavy, and land on both feet", () => {
  assert.ok(actionRows.jump.air > .2 && actionRows.jump.air < .5, `air ${actionRows.jump.air.toFixed(2)} s`);
  assert.ok(actionRows.jump.landed);
});
test("a low reach bends the spine to get there", () => {
  assert.ok(actionRows.reach.curl > .5, `curl ${actionRows.reach.curl.toFixed(2)}`);
});
test("a grab holds on while it's dragged", () => {
  assert.ok(actionRows.grab.held > .8, `held ${(actionRows.grab.held * 100).toFixed(0)}%`);
});
test("on a board the rider stands sideways, pushes up to speed and leans into carves", () => {
  assert.ok(Math.abs(actionRows.board.yaw - Math.PI / 2) < .1);
  assert.ok(actionRows.board.speed > 600);
  assert.ok(actionRows.carve.lean > .4);
});
test("the whole demo run is deterministic", () => {
  const run = () => {
    const a = createActor();
    for (let f = 0; f < 1200; f++) {
      a.input.forward = f % 400 < 300 ? 1 : 0; a.input.turn = Math.sin(f / 50);
      if (f === 200) trigger(a, "punch", 1); if (f === 260) trigger(a, "jump"); if (f === 330) trigger(a, "hit", -1);
      if (f === 500) setMode(a, "board"); if (f === 700) trigger(a, "jump"); if (f === 900) setMode(a, "kart");
      stepActor(a);
    }
    const b = a.body;
    return [...b.x, ...b.y, ...b.z, ...b.arms.flatMap((l) => [...l.x, ...l.y, ...l.z]), ...b.legs.flatMap((l) => [...l.x, ...l.y, ...l.z])];
  };
  assert.deepEqual(run(), run());
});
