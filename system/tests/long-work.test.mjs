import test from "node:test";
import assert from "node:assert/strict";
import { trackLongWork, drainLongWork, draining } from "../backend/long-work.mjs";

test("a restart waits for long work, turns new work away, and gives up at its limit", async () => {
  let finish;
  const move = trackLongWork(new Promise((resolve) => { finish = resolve; }));
  assert.equal(draining(), false);
  const drained = drainLongWork(5000);
  assert.equal(draining(), true);
  setTimeout(() => finish("painted"), 50);
  assert.equal(await drained, 0);
  assert.equal(await move, "painted");

  trackLongWork(new Promise(() => {}));
  assert.equal(await drainLongWork(30), 1);
  globalThis.__acLongWork = undefined;
});
