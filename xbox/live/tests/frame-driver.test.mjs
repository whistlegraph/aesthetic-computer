import assert from "node:assert/strict";
import test from "node:test";
import { createFrameDriver } from "../frame-driver.mjs";

function harness() {
  let time = 0;
  let nextHandle = 1;
  let raf = null;
  const timers = new Map();
  const paints = [];
  const driver = createFrameDriver({
    paint: () => paints.push(time),
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
  });
  return {
    driver,
    paints,
    setTime(value) { time = value; },
    fireRaf(value) {
      time = value;
      const callback = raf;
      assert.equal(typeof callback, "function");
      callback(value);
    },
    fireNextTimer() {
      const next = [...timers.entries()].sort((a, b) => a[1].at - b[1].at)[0];
      assert.ok(next, "expected an armed deadline timer");
      timers.delete(next[0]);
      time = next[1].at;
      next[1].callback();
    },
    timerCount() { return timers.size; },
  };
}

test("fills missing deadlines when requestAnimationFrame is capped at 30 Hz", () => {
  const h = harness();
  h.driver.start();
  h.fireNextTimer(); // 18.67 ms fallback between 30 Hz rAF callbacks.
  h.fireRaf(1000 / 30);
  h.fireNextTimer();
  h.fireRaf(2000 / 30);

  assert.equal(h.paints.length, 5);
  assert.equal(h.driver.stats.timerFrames, 2);
  assert.equal(h.driver.stats.rafFrames, 2);
  assert.equal(h.driver.stats.frames, 5);
});

test("lets timely 60 Hz requestAnimationFrame win without double painting", () => {
  const h = harness();
  h.driver.start();
  h.fireRaf(1000 / 60);
  h.fireRaf(2000 / 60);
  h.fireRaf(3000 / 60);

  assert.equal(h.paints.length, 4);
  assert.equal(h.driver.stats.rafFrames, 3);
  assert.equal(h.driver.stats.timerFrames, 0);
  assert.equal(h.timerCount(), 1);
});

test("pauses fallback paints while hidden and resumes from a fresh deadline", () => {
  const h = harness();
  h.driver.start();
  h.driver.setVisible(false);
  assert.equal(h.timerCount(), 0);
  h.setTime(5000);
  h.driver.setVisible(true);

  assert.deepEqual(h.paints, [0, 5000]);
  assert.equal(h.timerCount(), 1);
  h.driver.stop();
  assert.equal(h.timerCount(), 0);
});
