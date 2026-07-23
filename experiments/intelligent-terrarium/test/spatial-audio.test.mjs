import assert from "node:assert/strict";
import test from "node:test";
import { SonicDeduper, spatialize } from "../web/spatial-audio.mjs";

test("semantic coordinates render left/right and attenuate monotonically", () => {
  const listener = { x: 0, y: 0, z: 0, yaw: 0 };
  assert.ok(spatialize([-3, 0, 0], listener).pan < 0);
  assert.ok(spatialize([3, 0, 0], listener).pan > 0);
  assert.ok(spatialize([1, 0, 0], listener, 12).gain > spatialize([8, 0, 0], listener, 12).gain);
});

test("sonic event IDs are accepted once across reconnect replay", () => {
  const deduper = new SonicDeduper(2);
  assert.equal(deduper.accept("one"), true);
  assert.equal(deduper.accept("one"), false);
  assert.equal(deduper.accept("two"), true);
  assert.equal(deduper.accept("three"), true);
  assert.equal(deduper.accept("one"), true);
});
