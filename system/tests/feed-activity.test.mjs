import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { createFeedActivity } from "../public/aesthetic.computer/lib/feed-activity.mjs";
import { validateAccountActivity } from "../public/aesthetic.computer/lib/account-activity-model.mjs";
import { startAccountActivity } from "../public/aesthetic.computer/lib/account-activity.mjs";
import { createAccountActivityHandler } from "../backend/account-activity-handler.mjs";

const one = "piece_0123456789abcdef01234567", two = "kidlisp_0123456789abcdef01234567";
function fixture() {
  let time = 0, user = "one", allow = true;
  const rows = [];
  const api = createFeedActivity({ owner: () => user, allowed: () => allow, now: () => time,
    emit: (action, fields) => rows.push({ action, ...fields, user }) });
  return { api, rows, advance(ms) { time += ms; }, user(value) { user = value; }, allow(value) { allow = value; } };
}
test("feed measures per-card foreground time, capped pauses, repeat entries and prompt returns", () => {
  const f = fixture();
  f.api.open("prompt_swipe"); f.api.open("prompt_swipe");
  f.api.sample(one, true); f.advance(1200); f.api.sample(two, true);
  f.advance(1000); f.api.sample(two, false); f.advance(100000); f.api.sample(two, true);
  f.advance(100000); f.api.close("prompt");
  assert.deepEqual(f.rows.map(r => r.action), ["mime_opened", "mime_piece_viewed", "mime_piece_viewed", "mime_piece_viewed", "mime_closed"]);
  assert.deepEqual(f.rows.filter(r => r.focusedMs).map(r => [r.contentId, r.focusedMs]), [[one, 1200], [two, 1000], [two, 2000]]);
  assert.equal(f.rows.at(-1).exitReason, "prompt");
  f.api.open("prompt_wheel"); assert.equal(f.rows.at(-1).entry, "prompt_wheel");
});
test("feed discards anonymous, previous-account and opted-out time", () => {
  const f = fixture(); f.user(null); f.api.open("direct"); f.api.sample(one, true); f.advance(1000);
  f.user("one"); f.api.sample(one, true); f.advance(800); f.api.sample(one, true);
  f.user("two"); f.api.sample(one, true); f.advance(600); f.api.sample(two, true);
  assert.equal(f.rows.filter(r => r.focusedMs).length, 1);
  assert.equal(f.rows.at(-1).user, "two"); assert.equal(f.rows.at(-1).focusedMs, 600);
  assert.equal(f.rows[0].entry, "already_open");
  f.allow(false); f.api.sample(two, true); f.advance(1000); f.api.close();
  assert.equal(f.rows.filter(r => r.focusedMs).length, 1);
});
test("feed flushes during long stationary viewing and rejects non-piece content", () => {
  const f = fixture(); f.api.open("direct"); f.api.sample(one, true);
  for (let i = 0; i < 35; i++) { f.advance(1000); f.api.sample(one, true); }
  f.api.sample("private-message-text", true); f.advance(1000); f.api.close();
  assert.deepEqual(f.rows.filter(r => r.focusedMs).map(r => r.focusedMs), [15000, 15000, 5000]);
  assert.doesNotMatch(JSON.stringify(f.rows), /private-message/);
});

const body = () => ({ version: 1, id: randomUUID(), session: randomUUID(), sequence: 1, automated: false,
  piece: "mime", action: "mime_piece_viewed", activityVersion: 1, entry: "prompt_swipe", contentId: one, focusedMs: 1200 });
test("feed schema bounds durations and identity fields; verified account owns persisted events", async () => {
  const valid = body(), origin = "https://aesthetic.computer";
  assert.equal(validateAccountActivity(valid, origin).contentId, one);
  for (const patch of [{ focusedMs: -1 }, { focusedMs: 30001 }, { focusedMs: 1.2 }, { contentId: "chat_0123456789abcdef01234567" },
    { entry: "arbitrary-url" }, { piece: "notepat" }, { activityVersion: 2 }]) assert.equal(validateAccountActivity({ ...valid, ...patch }, origin), null);
  assert.equal(validateAccountActivity(valid, "https://sotce.net"), null);
  const writes = [];
  const handler = createAccountActivityHandler({ authorize: async () => ({ sub: "verified" }), connect: async () => ({ db: {
    collection: () => ({ createIndex: async () => {}, updateOne: async (...args) => writes.push(args) }),
  } }) });
  const request = { httpMethod: "POST", headers: { origin }, body: JSON.stringify({ ...valid, user: "forged", handle: "@forged", source: "private-code", visit: randomUUID() }) };
  assert.equal((await handler(request)).statusCode, 204); await handler(request);
  assert.deepEqual(writes[0][0], writes[1][0]);
  const row = writes[0][1].$setOnInsert;
  assert.equal(row.user, "verified"); assert.equal(row.focusedMs, 1200);
  assert.equal(+row.expiresAt - +row.at, 35 * 86400000);
  assert.doesNotMatch(JSON.stringify(row), /forged|private-code|"visit"/);
});

test("authenticated feed client shares the clock session, flushes on hidden exit, excludes automation and opt-outs", async () => {
  let time = 0, user = { sub: "one" };
  const sent = [], doc = { visibilityState: "visible", referrer: "" };
  const win = { navigator: {}, location: { hostname: "aesthetic.computer", pathname: "/", search: "" }, crypto: { randomUUID },
    performance: { now: () => time }, setTimeout, clearTimeout, setInterval: () => 1, clearInterval() {},
    fetch: async (_, options) => { sent.push(JSON.parse(options.body)); return { ok: true }; } };
  win.top = win;
  const api = startAccountActivity(win, doc, { getUser: () => user, getToken: () => "fixture-only" });
  const settle = () => new Promise(resolve => setImmediate(resolve));
  api.load("aesthetic.computer/disks/prompt"); api.ready(); await settle();
  api.feedOpen("prompt_swipe"); api.feedSample(one, true); time = 1500;
  doc.visibilityState = "hidden"; api.feedSample(one, false); api.feedClose("pagehide"); await settle();
  assert.equal(sent.find(r => r.action === "mime_piece_viewed").focusedMs, 1500);
  assert.equal(sent.at(-1).action, "mime_closed");
  doc.visibilityState = "visible"; api.load("aesthetic.computer/disks/laklok"); api.ready(); await settle();
  assert.equal(sent[1].session, sent.at(-1).session); assert.equal(sent.at(-1).piece, "laklok");
  for (const flag of ["acAutomation", "acVisitTrackingDisabled"]) {
    const count = sent.length; win[flag] = true; api.feedOpen("direct"); api.feedSample(one, true); time += 1000; api.feedClose(); await settle();
    assert.equal(sent.length, count); win[flag] = false;
  }
  const count = sent.length; user = null; api.feedOpen("direct"); api.feedSample(one, true); time += 1000; api.feedClose(); await settle();
  assert.equal(sent.length, count); api.stop();
});
