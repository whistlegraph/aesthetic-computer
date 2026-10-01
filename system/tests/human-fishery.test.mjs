import test from "node:test";
import assert from "node:assert/strict";
import { fisheryOptions, fisherySnapshot } from "../../toolchain/analytics/human-fishery.mjs";

const now = new Date("2026-09-30T23:00:00Z");
test("fishery requires interaction, excludes automation and clients by default, and bounds queries", () => {
  const options = fisheryOptions({ startedAfter: "2026-09-30T22:58:00Z" }, now);
  assert.equal(options.match.automated, false);
  assert.equal(options.match.interacted, true);
  assert.ok(!options.match.property.$in.includes("false.work"));
  assert.equal(options.match.startedAt.$gte.toISOString(), "2026-09-30T22:58:00.000Z");
  assert.equal(options.since.toISOString(), "2026-09-30T22:55:00.000Z");
  for (const args of [{minutes:0},{minutes:61},{limit:201},{limit:1.5},{scope:"typo"},{startedAfter:"bad"},{startedAfter:"2026-10-01"}])
    assert.throws(() => fisheryOptions(args, now));
});

test("fishery uses temporary visit aliases and never exposes raw identifiers or unreviewed fields", () => {
  const row = { _id: "private-visit-id", property: "aesthetic.computer", surface: "play",
    startedAt: now, lastSeenAt: now, activeSeconds: 60, engaged: true,
    actions: { note_played: true, private: true }, handle: "private-handle", ip: "private-address" };
  const options = fisheryOptions({limit:1}, now);
  const snapshot = fisherySnapshot([row,row], options);
  assert.equal(snapshot.truncated, true);
  assert.equal(snapshot.fish.length, 1);
  assert.deepEqual(snapshot.fish[0].actions, ["note_played"]);
  assert.ok(!JSON.stringify(snapshot).includes("private"));
  assert.equal(snapshot.fish[0].fish, fisherySnapshot([row], options).fish[0].fish);
  assert.notEqual(snapshot.fish[0].fish, fisherySnapshot([{...row,_id:"another-visit"}],options).fish[0].fish);
  assert.notEqual(snapshot.fish[0].fish, fisherySnapshot([row],fisheryOptions({},new Date("2026-10-01T00:00:00Z"))).fish[0].fish);
});
