// fur.mjs's CPU side: the pup's sketches as fur primitives, the decals,
// the octahedral map, the comb's pick.   node --test xbox/fiapup/tests/fur.test.mjs

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { compile } from "../../live/object-lisp.mjs";
import { furry, octa, pickPrim, sketchPrims, surface } from "../fur.mjs";

const puppy = compile(readFileSync(new URL("../objects/puppy-flat.lisp", import.meta.url), "utf8"), "puppy");
const owner = { body: [0, 0, 0], head: [0, 0, 0], ears: [.3, 0, 0], tail: [0, 0, 0], fl: [0, 0, 0],
  fr: [0, 0, 0], bl: [0, 0, 0], br: [0, 0, 0], face: [1, 0, 0] };
const place = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1];
function prims() {
  const out = [];
  puppy({ owner }, place, { view: new Float64Array(24), sketch: (i, m, at) => {
    const s = puppy.sketches[i];
    if (furry(s.records, s.count)) sketchPrims(s.records, s.count, Array.from(m.slice(at, at + 12)), null, out);
  } });
  return out;
}

test("every part but the shadow becomes fur primitives", () => {
  const all = prims();
  assert.ok(all.length >= 24, `${all.length} primitives`);
  const shadow = puppy.sketches.find((s) => s.records[0] === 3);
  assert.equal(furry(shadow.records, shadow.count), false, "the shadow ring stays flat");
});

test("fur lengths: none on eyes, nose and paws; long on ears and tail; short on the body", () => {
  const all = prims();
  const eyes = all.filter((q) => q.kind === 1 && q.r > 1.7 && q.r < 1.9 && q.rgb[0] < 70);
  assert.equal(eyes.length, 2);
  assert.ok(eyes.every((q) => q.len === 0));
  const nose = all.find((q) => q.kind === 1 && q.r === 2.3);
  assert.equal(nose.len, 0);
  const paws = all.filter((q) => q.kind === 1 && q.r > 3.3 && q.r < 3.7);
  assert.equal(paws.length, 4);
  assert.ok(paws.every((q) => q.len === 0));
  const body = all.find((q) => q.kind === 2 && q.r === 9.5);
  const ears = all.filter((q) => q.kind === 2 && q.r === 3.3);
  const tail = all.find((q) => q.kind === 2 && q.r === 2.3);
  assert.equal(ears.length, 2);
  assert.ok(ears[0].len / ears[0].r > body.len / body.r * 1.5, "ears longer for their size");
  assert.ok(tail.len > body.len, "a bushy tail");
});

test("eyes and nose move out to the head's surface, as decals facing out", () => {
  const all = surface(prims());
  const head = all.find((q) => q.kind === 1 && q.r === 9);
  for (const eye of all.filter((q) => q.kind === 1 && q.r > 1.7 && q.r < 1.9 && q.rgb[0] < 70)) {
    const d = Math.hypot(...eye.a.map((x, i) => x - head.a[i]));
    assert.ok(d > head.r, "out past the skull");
    assert.ok(eye.decal && Math.abs(Math.hypot(...eye.decal) - 1) < 1e-6, "with its facing");
  }
});

test("octahedral coordinates cover the square without poles", () => {
  for (const n of [[0, 1, 0], [0, -1, 0], [1, 0, 0], [0, 0, -1], [.5, -.5, .7]]) {
    const [u, v] = octa(n);
    assert.ok(Math.abs(u) <= 1 && Math.abs(v) <= 1);
  }
  assert.deepEqual(octa([0, 1, 0]), [0, 0]);
  const south = octa([0, -1, 0]);
  assert.ok(Math.abs(Math.abs(south[0]) - 1) < 1e-9 && Math.abs(Math.abs(south[1]) - 1) < 1e-9);
});

test("a ray through the middle of the pup's back picks the body", () => {
  const all = prims();
  // A camera above and in front of the pup, looking down at the body's middle.
  const cam = new Float64Array(24);
  const eye = [0, 120, 60], at = [0, 21, 0];
  const f = at.map((x, i) => x - eye[i]), fl = Math.hypot(...f);
  const fwd = f.map((x) => x / fl), right = [1, 0, 0];
  const up = [right[1] * fwd[2] - right[2] * fwd[1], right[2] * fwd[0] - right[0] * fwd[2], right[0] * fwd[1] - right[1] * fwd[0]].map((x) => -x);
  cam.set([...eye, ...right, ...up, ...fwd, 500, 500, 1, 1000, 1, 2.8 / 16000, -1.4, 12]);
  const hit = pickPrim(all, cam, 500, 500);
  assert.ok(hit, "hit something");
  const q = all[hit.index];
  assert.ok(q.len > 0);
  assert.ok(Math.abs(Math.hypot(...hit.local) - 1) < 1e-6);
  assert.ok(hit.point[1] > 21, "on the top side");
});
