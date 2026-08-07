import assert from "node:assert/strict";
import test from "node:test";
import { createFrameDriver } from "../frame-driver.mjs";

function harness(options = {}) {
  let time = 0;
  let nextHandle = 1;
  let raf = null;
  const timers = new Map();
  const paints = [];
  const simulations = [];
  const samples = [];
  const driver = createFrameDriver({
    paint: (at, alpha) => paints.push([time, at, alpha]),
    simulate: (at) => simulations.push(at),
    sample: () => samples.push(time),
    now: () => time,
    requestFrame(callback) {
      raf = callback;
      return nextHandle++;
    },
    cancelFrame() { raf = null; },
    setTimer(callback, delay) {
      const handle = nextHandle++;
      timers.set(handle, { at: time + delay, callback });
      return handle;
    },
    clearTimer(handle) { timers.delete(handle); },
    ...options,
  });
  return {
    driver,
    paints,
    simulations,
    samples,
    setTime(value) { time = value; },
    fireRaf(value) {
      time = value;
      const callback = raf;
      assert.equal(typeof callback, "function");
      callback(value);
    },
    fireNextTimer() {
      const next = [...timers.entries()].sort((a, b) => a[1].at - b[1].at)[0];
      assert.ok(next, "expected an armed simulation timer");
      timers.delete(next[0]);
      time = next[1].at;
      next[1].callback();
    },
    timerCount() { return timers.size; },
  };
}

test("renders at an uncapped 120 Hz while combat remains fixed at 60 Hz", () => {
  const h = harness();
  h.driver.start();
  for (let frame = 1; frame <= 4; frame++) h.fireRaf(frame * 1000 / 120);

  assert.equal(h.paints.length, 4);
  assert.equal(h.simulations.length, 3); // Initial tick, then 16.67 and 33.33 ms.
  assert.equal(h.driver.stats.renderFrames, 4);
  assert.equal(h.driver.stats.simulationTicks, 3);
  assert.deepEqual(h.simulations.map((at) => Math.round(at * 100) / 100),
    [0, 16.67, 33.33]);
});

test("a 30 Hz display still advances the 60 Hz simulation between paints", () => {
  const h = harness();
  h.driver.start();
  h.fireNextTimer();
  h.fireRaf(1000 / 30);
  h.fireNextTimer();
  h.fireRaf(2000 / 30);

  assert.equal(h.paints.length, 2);
  assert.equal(h.simulations.length, 5);
  assert.equal(h.driver.stats.simulationTicks, 5);
  assert.equal(h.driver.stats.renderFrames, 2);
});

test("samples controls at display rate and immediately before timer ticks", () => {
  const h = harness();
  h.driver.start();
  h.fireRaf(1000 / 120);
  h.fireRaf(2000 / 120);
  h.fireNextTimer();

  assert.equal(h.samples.length, 4);
  assert.equal(h.driver.stats.inputSamples, 4);
});

test("bounds catch-up work and drops stale wall-clock ticks", () => {
  const h = harness({ maxCatchUpTicks: 3 });
  h.driver.start();
  h.fireRaf(1000);

  assert.equal(h.simulations.length, 4);
  assert.equal(h.paints.length, 1);
  assert.ok(h.driver.stats.droppedSimulationTicks > 50);
});

test("pauses simulation timers while hidden and resumes without fast-forwarding", () => {
  const h = harness();
  h.driver.start();
  h.driver.setVisible(false);
  assert.equal(h.timerCount(), 0);
  h.setTime(5000);
  h.driver.setVisible(true);

  assert.equal(h.simulations.length, 2);
  assert.deepEqual(h.simulations.map((at) => Math.round(at * 100) / 100),
    [0, 16.67]);
  assert.equal(h.timerCount(), 1);
  h.driver.stop();
  assert.equal(h.timerCount(), 0);
});
