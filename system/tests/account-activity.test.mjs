import test from "node:test";
import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import { createAccountActivityHandler } from "../backend/account-activity-handler.mjs";
import { startAccountActivity } from "../public/aesthetic.computer/lib/account-activity.mjs";
import { visitReferrer } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { activityPiece, validateAccountActivity, SOTCE_ACTIONS } from "../public/aesthetic.computer/lib/account-activity-model.mjs";

const snapshot = () => ({ version: 1, id: randomUUID(), session: randomUUID(), sequence: 1, piece: "notepat", action: "note_played", automated: false, referrerHost: "example.org" });
test("referral reporting keeps only public site names", () => {
  assert.equal(visitReferrer("https://www.example.org/private?q=secret#fragment"), "example.org");
  for (const ref of [null,"","file:///secret","https://user:password@example.org/path","http://127.0.0.1/a","http://host.local/a","http://[::1]/a"])
    assert.equal(visitReferrer(ref), null, ref);
  assert.equal(activityPiece("aesthetic.computer/disks/notepat"), "notepat");
  assert.equal(activityPiece("aesthetic.computer/disks/chat"), null);
  assert.equal(activityPiece("private-inline-source"), "published-or-code");
  assert.equal(validateAccountActivity({ ...snapshot(), piece: "mail" }, "https://aesthetic.computer"), null);
  assert.equal(validateAccountActivity(snapshot(), "https://false.work"), null);
});

test("account recording requires verified auth and cannot be attributed by client-supplied user or handle", async () => {
  const writes = [], indexes = [];
  let subject = null, tenant;
  const handler = createAccountActivityHandler({
    authorize: async (_headers, name) => { tenant = name; return subject; },
    connect: async () => ({ db: { collection: () => ({
      createIndex: async (keys, options) => indexes.push({ keys, options }),
      updateOne: async (...args) => writes.push(args),
    }) } }),
  });
  const body = { ...snapshot(), user: "forged-user", handle: "forged-handle", referrerHost: "https://example.org/secret?token=private" };
  const event = { httpMethod: "POST", headers: { origin: "https://aesthetic.computer", "user-agent": "Mozilla/5.0" }, body: JSON.stringify(body) };
  assert.equal((await handler(event)).statusCode, 401);
  assert.equal(writes.length, 0);
  subject = { sub: "verified-subject" };
  assert.equal((await handler(event)).statusCode, 204);
  assert.equal(tenant, "aesthetic");
  const row = writes[0][1].$setOnInsert;
  assert.equal(row.user, "verified-subject");
  assert.equal(row.referrerHost, "example.org");
  assert.equal(+row.expiresAt - +row.at, 35 * 86400000);
  assert.doesNotMatch(JSON.stringify(writes), /forged|secret|token=private/);
  await handler(event);
  assert.deepEqual(writes[0][0], writes[1][0], "retry ID is stable and insert-only");
  assert.ok(indexes.some(i => i.options?.expireAfterSeconds === 0));
  await handler({ ...event, headers: { ...event.headers, origin: "https://sotce.net" } });
  assert.equal(tenant, "sotce", "tenant derives from reviewed origin");
  const count = writes.length;
  await handler({ ...event, body: JSON.stringify({ ...body, automated: true }) });
  assert.equal(writes.length, count, "known automation does not enter authenticated activity");
  assert.equal((await handler({ ...event, httpMethod: "GET" })).statusCode, 405);
});

function browserFixture() {
  const sent = [];
  let tick, user = null, token = () => "token", disabled = false;
  const doc = { visibilityState: "visible", referrer: "https://example.org/path?secret=1" };
  const win = { navigator: {}, crypto: { randomUUID }, location: { hostname: "aesthetic.computer", pathname: "/notepat", search: "" },
    setInterval(fn) { tick = fn; return 1; }, clearInterval() {}, setTimeout, clearTimeout,
    get acVisitTrackingDisabled() { return disabled; },
    fetch: async (_url, options) => { sent.push(options); return { ok: true }; },
  };
  win.top = win;
  const api = startAccountActivity(win, doc, { getUser: () => user, getToken: () => token() });
  return { win, doc, api, sent, tick: () => tick(), user: value => { user = value; }, token: fn => { token = fn; }, disable: () => { disabled = true; } };
}
const settle = () => new Promise(resolve => setImmediate(resolve));
test("authenticated client records public piece changes and deduplicates actions without replaying anonymous use", async () => {
  const f = browserFixture();
  f.api.load("aesthetic.computer/disks/notepat"); f.api.ready();
  f.api.action("note_played"); await settle(); assert.equal(f.sent.length, 0);
  f.user({ sub: "one" }); f.tick(); await settle();
  f.api.action("note_played"); f.api.action("note_played"); await settle();
  assert.deepEqual(f.sent.map(x => JSON.parse(x.body).action), ["piece_opened", "note_played"]);
  const first = JSON.parse(f.sent[0].body);
  assert.equal(first.referrerHost, "example.org");
  assert.equal(f.sent[0].headers.Authorization, "Bearer token");
  f.api.load("aesthetic.computer/disks/nopaint"); f.api.ready(); await settle();
  assert.equal(JSON.parse(f.sent.at(-1).body).session, first.session);
  f.win.location.pathname = "/mail"; f.api.action("painting_saved"); f.tick(); await settle();
  assert.equal(f.sent.length, 3);
  f.win.location.pathname = "/notepat"; f.user({ sub: "two" }); f.tick(); await settle();
  assert.notEqual(JSON.parse(f.sent.at(-1).body).session, first.session);
  const count = f.sent.length; f.disable(); f.api.action("note_played"); await settle(); assert.equal(f.sent.length, count);
  f.api.stop();
});
test("logout or opt-out during token retrieval prevents late account attribution", async () => {
  for (const change of [f => f.user(null), f => f.disable()]) {
    const f = browserFixture(); let release;
    f.user({ sub: "one" }); f.token(() => new Promise(resolve => { release = resolve; }));
    f.api.load("aesthetic.computer/disks/notepat"); f.api.ready(); await settle();
    change(f); release("token"); await settle(); assert.equal(f.sent.length, 0); f.api.stop();
  }
});

test("Sotce action sequences stay in their tenant and private editors permit only the submitted-question milestone", async () => {
  for (const action of SOTCE_ACTIONS) {
    const body = { ...snapshot(), piece: "sotce", action };
    assert.equal(validateAccountActivity(body, "https://aesthetic.computer"), null);
    assert.equal(validateAccountActivity(body, "https://sotce.net").tenant, "sotce");
  }
  const f = browserFixture();
  f.win.location.hostname = "sotce.net"; f.win.location.pathname = "/1";
  f.user({ sub: "sotce-user" }); f.api.load("aesthetic.computer/disks/sotce"); f.api.ready(); await settle();
  f.api.action("sotce_page_viewed"); await settle();
  f.api.action("sotce_page_viewed"); await settle();
  assert.equal(f.sent.filter(x => JSON.parse(x.body).action === "sotce_page_viewed").length, 2);
  f.win.location.pathname = "/ask";
  const count = f.sent.length;
  f.tick(); f.api.action("canvas_interacted"); f.api.action("sotce_page_viewed"); await settle();
  assert.equal(f.sent.length, count);
  f.api.action("sotce_question_submitted"); await settle(); assert.equal(f.sent.length, count + 1);
  f.win.location.pathname = "/comment"; f.tick(); f.api.action("sotce_page_touched"); await settle();
  assert.equal(f.sent.length, count + 1);
  f.win.location.pathname = "/"; f.disable(); f.api.action("sotce_page_viewed"); await settle();
  assert.equal(f.sent.length, count + 1);
  f.api.stop();
});
