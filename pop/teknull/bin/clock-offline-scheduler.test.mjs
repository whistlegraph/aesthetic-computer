import assert from "node:assert/strict";
import test from "node:test";
import { compileClockPattern } from "./clock-offline-scheduler.mjs";

test("schedules Clock parallel tracks, sticky durations and swing", () => {
  const r = compileClockPattern({
    source: "{saw:0.7}3a.._3a]c {square:0.4}2a,2e",
    unitSec: 0.1,
    cycles: 2,
    section: "test",
    voice: "acid",
  });
  assert.equal(r.parsedType, "parallel");
  assert.equal(r.stateType, "parallel");
  assert.equal(r.durationSec, 1.6);
  assert.equal(r.events.length, 10);
  assert.equal(r.events[0].tone, "3A");
  assert.equal(r.events[0].waveType, "sawtooth");
  assert.ok(r.events.some((e) => e.swing === "late"));
  assert.ok(r.events.every((e) => e.startSec >= 0 && e.startSec < r.endSec));
});

test("schedules Clock sequential sections", () => {
  const r = compileClockPattern({ source: "4a.4c > 4e.4g", unitSec: 0.2, cycles: 1 });
  assert.equal(r.parsedType, "sequential");
  assert.equal(r.events.length, 4);
  assert.equal(r.events[2].tone, "4E");
  assert.equal(r.events[2].startSec, 0.4);
});
