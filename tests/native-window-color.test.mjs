import assert from "node:assert/strict";
import test from "node:test";
import { createWindowColorReporter } from "../system/public/aesthetic.computer/lib/native-window-color.mjs";

test("window colors coalesce to the newest frame and send the final static color", () => {
  const sent = [], timers = [];
  const report = createWindowColorReporter(message => sent.push(message), (fn, ms) => {
    assert.equal(ms, 100);
    timers.push(fn);
  });
  report([10, 20, 30]);
  report([200, 210, 220]);
  assert.equal(timers.length, 1);
  timers.shift()();
  assert.deepEqual(sent, [{ type: "window:color", body: [200, 210, 220] }]);
  report([200, 210, 220]);
  timers.shift()();
  assert.equal(sent.length, 1);
  report([NaN, 0, 0]);
  report([Infinity, 0, 0]);
  report([0, 0]);
  assert.equal(timers.length, 0);
  report([-1, 300, 50.4]);
  timers.shift()();
  assert.deepEqual(sent[1].body, [0, 255, 50]);
});
