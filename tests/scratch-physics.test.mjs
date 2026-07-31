import test from "node:test";
import assert from "node:assert/strict";
import {
  directionalCruiseTarget,
  easeRateToward,
  scratchProgressDelta,
  scratchRateFromMotion,
} from "../system/public/aesthetic.computer/lib/scratch-physics.mjs";

test("scratch travel represents a physical time span, not the whole tape", () => {
  assert.equal(scratchProgressDelta(200, 200, 10), 0.175);
  assert.equal(scratchProgressDelta(-200, 200, 10), -0.175);
});

test("a released platter approaches signed cruise without crossing direction", () => {
  let reverse = -8;
  let forward = 8;
  for (let i = 0; i < 120; i += 1) {
    reverse = easeRateToward(reverse, -1, 0.94);
    forward = easeRateToward(forward, 1, 0.94);
    assert.ok(reverse <= -1);
    assert.ok(forward >= 1);
  }
  assert.ok(Math.abs(reverse + 1) < 0.01);
  assert.ok(Math.abs(forward - 1) < 0.01);
});

test("scratch velocity has stable direction and event-rate-independent units", () => {
  assert.equal(scratchRateFromMotion(20, 0.1, 200), 1.75);
  assert.equal(scratchRateFromMotion(-10, 0.05, 200), -1.75);
});

test("release cruise preserves the direction of the throw", () => {
  assert.equal(directionalCruiseTarget(4, -2), -1);
  assert.equal(directionalCruiseTarget(-4, 2), 1);
  assert.equal(directionalCruiseTarget(-0.2, 0), -1);
  assert.equal(directionalCruiseTarget(0.2, 0), 1);
});
