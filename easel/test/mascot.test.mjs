import assert from "node:assert/strict";
import test from "node:test";
import {
  MASCOT_HEIGHT,
  MASCOT_REST_X,
  MASCOT_SETTLED_MS,
  MASCOT_WIDTH,
  mascotAt,
  mascotIsAnimating,
  mascotNextFrameIn,
  mascotRows,
} from "../src/mascot.mjs";

// A sprite that changes size shifts whatever sits beside it, so the size is
// the one thing that must hold across every frame of the performance.
test("the guy is the same size in every pose", () => {
  for (let ms = 0; ms < 12000; ms += 37) {
    const { lines } = mascotAt(ms);
    assert.equal(lines.length, MASCOT_HEIGHT, `height at ${ms}ms`);
    for (const line of lines)
      assert.equal(Array.from(line).length, MASCOT_WIDTH, `width at ${ms}ms: ${line}`);
  }
});

test("he walks in from off-frame and stops in the corner", () => {
  assert.ok(mascotAt(0).x + MASCOT_WIDTH <= 0, "starts fully outside the frame");
  assert.equal(mascotAt(0).phase, "walking");

  let previous = mascotAt(0).x;
  for (let ms = 0; ms < 1120; ms += 40) {
    const { x } = mascotAt(ms);
    assert.ok(x >= previous, `never walks backwards (${ms}ms)`);
    previous = x;
  }
  assert.equal(mascotAt(MASCOT_SETTLED_MS).x, MASCOT_REST_X, "comes to rest");
});

test("he waves after he arrives, not while walking", () => {
  const poses = new Set();
  for (let ms = 1120; ms < 1840; ms += 20) poses.add(mascotAt(ms).lines.join("|"));
  assert.ok(poses.size > 1, "the wave actually animates");
  assert.equal(mascotAt(1400).phase, "waving");
  assert.equal(mascotAt(1400).x, MASCOT_REST_X, "stands still to wave");
});

test("he settles, then only blinks", () => {
  assert.ok(mascotIsAnimating(500));
  assert.ok(!mascotIsAnimating(MASCOT_SETTLED_MS + 1));
  assert.equal(mascotAt(3000).phase, "idle");

  // An idle session must not ask for a repaint every frame — that is a mascot
  // spinning a CPU. Seconds between blinks, not milliseconds.
  assert.ok(mascotNextFrameIn(3000) > 1000, "idle repaints are seconds apart");
  for (let ms = 0; ms < 12000; ms += 53)
    assert.ok(mascotNextFrameIn(ms) > 0, `always schedules forward at ${ms}ms`);
});

test("the head is the mark and carries its own colour", () => {
  const { rows } = mascotRows(3000);
  assert.equal(rows.length, MASCOT_HEIGHT);
  assert.equal(rows[0].tone, "handle", "the head is painted in the mark's pink");
  assert.ok(rows.slice(1).every((row) => row.tone === "soft"));
  assert.match(rows[0].text, /[●▪]/, "the head is a dot");
});
