import assert from "node:assert/strict";
import test from "node:test";

import {
  clearPoints,
  compareNomRuns,
  munchPoints,
  normalizeNomRun,
} from "../system/public/aesthetic.computer/lib/nom-score.mjs";

test("Nom correct-munch points reward a combo", () => {
  assert.equal(munchPoints(1), 100);
  assert.equal(munchPoints(4), 175);
});

test("Nom level-clear points reward level and remaining beats", () => {
  assert.equal(clearPoints(1, 0), 250);
  assert.equal(clearPoints(3, 7), 820);
});

test("Nom run validation accepts bounded integers only", () => {
  assert.deepEqual(normalizeNomRun({ score: "425", level: 2, correct: 3 }), {
    score: 425,
    level: 2,
    correct: 3,
  });
  assert.equal(normalizeNomRun({ score: -1, level: 2, correct: 3 }), null);
  assert.equal(normalizeNomRun({ score: 10, level: 0, correct: 3 }), null);
});

test("Nom run ordering resolves score, level, correct, then first achievement", () => {
  const base = { score: 1000, level: 3, correct: 12, when: "2026-08-07T01:00:00Z" };
  assert.ok(compareNomRuns({ ...base, score: 1001 }, base) > 0);
  assert.ok(compareNomRuns({ ...base, level: 4 }, base) > 0);
  assert.ok(compareNomRuns({ ...base, correct: 13 }, base) > 0);
  assert.ok(compareNomRuns({ ...base, when: "2026-08-07T00:00:00Z" }, base) > 0);
});

// A catching game, a survival run — anything whose whole result is a number —
// ranks on the same ladder without inventing a level or a correct count.
test("a score-only run ranks without levels or correct answers", () => {
  assert.deepEqual(normalizeNomRun({ score: 42 }), { score: 42, level: 1, correct: 0 });
  assert.deepEqual(normalizeNomRun({ score: 0 }), { score: 0, level: 1, correct: 0 });

  // A field that is actually sent is still checked, so the old errors stand.
  assert.equal(normalizeNomRun({ score: 10, level: 0 }), null);
  assert.equal(normalizeNomRun({ score: 10, correct: -1 }), null);
  assert.equal(normalizeNomRun({ score: "nope" }), null);

  // And such runs still order against each other by score alone.
  const a = normalizeNomRun({ score: 12 });
  const b = normalizeNomRun({ score: 9 });
  assert.ok(compareNomRuns(a, b) > 0);
  assert.ok(compareNomRuns(a, null) > 0, "a first run always beats no run");
});
