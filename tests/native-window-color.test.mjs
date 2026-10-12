import assert from "node:assert/strict";
import test from "node:test";
import { readFileSync } from "node:fs";
import vm from "node:vm";
import { createWindowColorReporter } from "../system/public/aesthetic.computer/lib/native-window-color.mjs";

test("native frames have a color before a piece or wipe exists", () => {
  const source = readFileSync(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
  const assignment = source.match(/sendData\.windowColor = [\s\S]*?;/)[0];
  const context = { sendData: {}, nativeWindowWipe: null, $commonApi: {}, screen: { pixels: new Uint8ClampedArray([32, 40, 50, 255]) } };
  vm.runInNewContext(assignment, context);
  assert.deepEqual(Array.from(context.sendData.windowColor), [32, 40, 50]);
  context.$commonApi.slug = "prompt";
  context.nativeWindowWipe = { slug: "prompt", rgb: [240, 220, 200] };
  vm.runInNewContext(assignment, context);
  assert.deepEqual(context.sendData.windowColor, [240, 220, 200]);
  context.$commonApi.slug = "notepat";
  vm.runInNewContext(assignment, context);
  assert.deepEqual(Array.from(context.sendData.windowColor), [32, 40, 50]);
});

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
