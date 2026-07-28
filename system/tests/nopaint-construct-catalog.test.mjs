import test from "node:test";
import assert from "node:assert/strict";
import { seededRandom } from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import { nonConflictingConstructProposals } from "../public/aesthetic.computer/lib/nopaint-construct-catalog.mjs";

const expected = ["aura", "banner", "breathe", "bubbles", "build", "caterpillar", "ellipse", "frame", "rainbow", "softy", "triangle", "vignette", "wafer", "walker"];
const base = Object.freeze({ color: Object.freeze([20, 40, 60, 128]), x: 20, y: 30, w: 180, h: 140, drift: 8, thickness: 3, points: Object.freeze([]), phase: 0 });

test("all non-conflicting recovered brush names have deterministic contracts", () => {
  assert.deepEqual(nonConflictingConstructProposals.map(({ slug }) => slug).sort(), expected);
  for (const contract of nonConflictingConstructProposals) {
    const make = () => contract.generate({ random: seededRandom(`construct:${contract.slug}`), width: 640, height: 480, base });
    const first = make();
    assert.deepEqual(first, make(), `${contract.slug} score is deterministic`);
    assert.equal(first.brush.slug, contract.slug);
    assert.ok(Object.isFrozen(first));
    assert.ok(Object.isFrozen(first.brush.parameters));
  }
});

test("every recovered contract renders through the bounded AC ink surface", () => {
  const calls = [];
  const method = (name) => (...args) => calls.push([name, ...args]);
  const ink = () => ({ box: method("box"), line: method("line"), oval: method("oval"), poly: method("poly") });
  for (const contract of nonConflictingConstructProposals) {
    calls.length = 0;
    const score = contract.generate({ random: seededRandom(contract.slug), width: 640, height: 480, base });
    contract.render({ ink }, score, 60);
    assert.ok(calls.length > 0, `${contract.slug} produces drawing calls`);
  }
});
