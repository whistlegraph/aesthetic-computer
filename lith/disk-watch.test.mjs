import test from "node:test";
import assert from "node:assert/strict";
import { parseDf, level, judge } from "./disk-watch.mjs";

const GB = 1024 * 1024;
const disk = (percent, freeGb, sizeGb = 24) => ({ percent, freeKb: freeGb * GB, sizeKb: sizeGb * GB, usedKb: 0 });

test("df's last line becomes sizes and a percentage", () => {
  const d = parseDf("Filesystem 1024-blocks Used Available Capacity Mounted on\n/dev/vda1 24299968 17992012 6291572 75% /\n");
  assert.deepEqual(d, { sizeKb: 24299968, usedKb: 17992012, freeKb: 6291572, percent: 75 });
  assert.throws(() => parseDf("Permission denied"), /unreadable/);
});

test("levels go by percentage or by gigabytes left, whichever is worse", () => {
  assert.equal(level(disk(75, 6)), "ok");
  assert.equal(level(disk(89, 9, 77)), "warning");
  assert.equal(level(disk(60, 2.5)), "warning");
  assert.equal(level(disk(100, 0)), "critical");
  assert.equal(level(disk(80, 1)), "critical");
});

test("email when a disk gets worse, on a reminder clock while bad, once on recovery", () => {
  const t0 = Date.parse("2026-10-20T04:00:00Z"), hour = 3600_000;
  let { next, say } = judge({}, disk(75, 6), t0);
  assert.equal(say, null);
  ({ next, say } = judge(next, disk(88, 3.5), t0));
  assert.equal(say, "warning");
  ({ next, say } = judge(next, disk(89, 3.2), t0 + hour));
  assert.equal(say, null);
  ({ next, say } = judge(next, disk(89, 3.2), t0 + 25 * hour));
  assert.equal(say, "warning");
  ({ next, say } = judge(next, disk(99, 0.2), t0 + 26 * hour));
  assert.equal(say, "critical");
  ({ next, say } = judge(next, disk(99, 0.2), t0 + 27 * hour));
  assert.equal(say, null);
  ({ next, say } = judge(next, disk(99, 0.2), t0 + 28 * hour));
  assert.equal(say, "critical");
  ({ next, say } = judge(next, disk(70, 7), t0 + 29 * hour));
  assert.equal(say, "recovered");
  ({ next, say } = judge(next, disk(70, 7), t0 + 30 * hour));
  assert.equal(say, null);
});

test("a server that stops answering is reported on its second miss only", () => {
  let { next, say } = judge({ level: "ok" }, { error: "timeout" });
  assert.equal(say, null);
  ({ next, say } = judge(next, { error: "timeout" }));
  assert.equal(say, "unreachable");
  ({ next, say } = judge(next, { error: "timeout" }));
  assert.equal(say, null);
});
