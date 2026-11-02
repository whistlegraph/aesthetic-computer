import test from "node:test";
import assert from "node:assert/strict";
import { fisheryOptions, fisherySnapshot } from "../../toolchain/analytics/human-fishery.mjs";
import { mimeFisheryQueries, mimeFisherySnapshot, mimeFishery } from "../../toolchain/analytics/human-fishery-mime.mjs";

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

test("MIME queries respect scope and start time, and include direct arrivals before interaction", () => {
  const options = fisheryOptions({ startedAfter: "2026-09-30T22:58:00Z" }, now);
  const query = mimeFisheryQueries(options);
  assert.equal(query.events.at.$gte.toISOString(), "2026-09-30T22:58:00.000Z");
  assert.equal(query.events.at.$lte, now);
  assert.equal(query.events.tenant, "aesthetic");
  assert.equal(query.events.activityVersion, 1);
  assert.equal(query.arrivals.automated, false);
  assert.equal(query.arrivals.interacted, undefined);
  assert.deepEqual(query.arrivals.property.$in, ["mime.ac"]);
  assert.deepEqual(mimeFisheryQueries(fisheryOptions({scope:"clients"},now)).arrivals.property.$in, []);
  assert.equal(mimeFisheryQueries(fisheryOptions({startedAfter:"2026-01-01"},now)).events.at.$gte.toISOString(), options.since.toISOString());
});

test("MIME account events remain separate from anonymous fish, with bounded public metadata and stable deduplication", () => {
  const options = fisheryOptions({limit:2},now);
  const event = { _id: "private-event", user: "private-user", session: "private-session", tenant: "aesthetic",
    piece: "mime", activityVersion: 1, at: now, property: "aesthetic.computer", sequence: 4,
    action: "mime_piece_viewed", entry: "prompt_swipe", contentId: "piece_0123456789abcdef01234567", focusedMs: 1500,
    source: "private-source", token: "private-token", ip: "private-address" };
  const visit = { _id: "private-visit", property: "mime.ac", surface: "home", startedAt: now, lastSeenAt: now, activeSeconds: 0 };
  const snapshot = mimeFisherySnapshot([event,{...event,_id:"second",action:"mime_closed",exitReason:"prompt"}], [visit], new Map([[event.user,"artist"]]), options);
  assert.equal(snapshot.events[0].account, "@artist");
  assert.equal(snapshot.events[0].focusedMs, 1500);
  assert.equal(snapshot.events[1].exitReason, "prompt");
  assert.equal(snapshot.events[1].contentId, undefined);
  assert.equal(snapshot.events[1].session, snapshot.events[0].session);
  assert.notEqual(snapshot.events[1].event, snapshot.events[0].event);
  assert.equal(snapshot.directPageArrivals[0].fish, fisherySnapshot([visit], options).fish[0].fish);
  assert.equal(snapshot.directPageArrivals[0].account, undefined);
  assert.doesNotMatch(JSON.stringify(snapshot), /private-|sourceCode|Authorization/);
  const nextDay = mimeFisherySnapshot([event],[],new Map(),fisheryOptions({},new Date("2026-10-01T00:00:00Z")));
  assert.equal(nextDay.events[0].event,snapshot.events[0].event,"account event deduplication survives UTC midnight");
  const bad = mimeFisherySnapshot([{...event,contentId:"private-code",focusedMs:30001}],[],new Map(),options);
  assert.equal(bad.events[0].contentId,undefined); assert.equal(bad.events[0].focusedMs,undefined);
  assert.deepEqual(mimeFisherySnapshot([{...event,tenant:"sotce"}],[],new Map(),options).events,[]);
});

test("MIME read path bounds each collection and resolves only selected accounts", async () => {
  const reads = [];
  const event = { _id: "event", user: "owner", session: "session", tenant: "aesthetic", piece: "mime",
    activityVersion: 1, action: "mime_opened", entry: "direct", property: "mime.ac", at: now };
  const db = { collection(name) { return { find(query, options) {
    const read = {name,query,options}; reads.push(read);
    return { sort(value) {read.sort=value;return this;}, limit(value) {read.limit=value;return this;},
      async toArray() {return name==="account-activity"?[event,{...event,_id:"event2",user:"excluded"}]:name==="@handles"?[{_id:"owner",handle:"artist"}]:[];} };
  } }; } };
  const result = await mimeFishery(db,fisheryOptions({limit:1},now));
  assert.equal(result.events.length,1); assert.equal(result.eventsTruncated,true);
  assert.equal(result.events[0].account,"@artist");
  assert.deepEqual(reads.find(r=>r.name==="@handles").query._id.$in,["owner"]);
  assert.ok(reads.filter(r=>r.name!=="@handles").every(r=>r.limit===2&&r.options.maxTimeMS===10000));
});
