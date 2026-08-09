// The shared dummy tally: which window gets quoted, and when to say nothing.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import { chooseWindow, MAX_HOURS, WINDOWS } from
  "../netlify/functions/oskiewar-pops.mjs";

const source = await readFile(
  new URL("../netlify/functions/oskiewar-pops.mjs", import.meta.url), "utf8");

test("the quoted window is the shortest one with anything in it", () => {
  // Busy right now: the last hour carries it, so that is what gets said.
  assert.deepEqual(chooseWindow([37]), { pops: 37, hours: 1 });
  // Nothing this hour, plenty two hours back: widen to the next window and
  // count everything inside it, not just the bucket that had the pops.
  assert.deepEqual(chooseWindow([0, 0, 9]), { pops: 9, hours: 3 });
  // A quiet couple of days still has something to say.
  const sparse = [];
  sparse[40] = 2;
  assert.deepEqual(chooseWindow(sparse), { pops: 2, hours: MAX_HOURS });
});

test("windows are ordered and stop at the two-day ceiling", () => {
  assert.deepEqual(WINDOWS, [...WINDOWS].sort((a, b) => a - b));
  assert.equal(Math.max(...WINDOWS), 48);
  assert.equal(MAX_HOURS, 48);
  // Rows outlive the longest window they can be quoted in, and no longer.
  assert.match(source, /const TTL_SECONDS = \(MAX_HOURS \+ 1\) \* 60 \* 60/);
  assert.match(source, /expireAfterSeconds: TTL_SECONDS/);
});

test("an empty tally reports zero rather than inventing a window", () => {
  assert.deepEqual(chooseWindow([]), { pops: 0, hours: MAX_HOURS });
  assert.deepEqual(chooseWindow([0, 0, 0, 0]), { pops: 0, hours: MAX_HOURS });
});

test("a pop carries nothing but its hour", () => {
  // The row is the whole privacy story: a timestamp, and a salted digest that
  // exists only to rate-limit. No player, no match, no address.
  assert.match(source,
    /insertOne\(\{ at: new Date\(now\), sourceDigest: digest \}\)/);
  // And the digest is salted and one-way, so the rate limit cannot be read
  // back out as a list of who played.
  assert.match(source, /createHmac\("sha256", secret\)\.update\(ip\)\.digest/);
  // Nothing but the count and its window leaves the building.
  assert.match(source, /respond\(200, \{ pops, hours, maxHours: MAX_HOURS \}/);
  assert.match(source, /if \(recent >= POPS_PER_HOUR\)/);
});
