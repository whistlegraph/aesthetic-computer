// Run: node --experimental-vm-modules spec/mail-nudge-spec.mjs
// Entity decoding in clean() and the email nudge a reader gets when no phone
// buzzed. Production modules over fake storage/transports; no credentials/network.
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import vm from "node:vm";
import * as privacy from "../shared/mail-privacy.mjs";
import * as media from "../system/backend/mail-media.mjs";
import * as mailEvents from "../system/backend/mail-events.mjs";

const marker = "PRIVATE_LETTER_CANARY";
const logs = [];
const log = (...args) => logs.push(args);
const context = vm.createContext({ console: { log, error: log }, URL, Buffer, process: { argv: [], env: {} } });
async function load(path, mocks) {
  const module = new vm.SourceTextModule(await readFile(new URL(path, import.meta.url), "utf8"), {
    context,
    importModuleDynamically: async (name) => synthetic(name, mocks[name]),
  });
  async function synthetic(name, exports) {
    if (!exports) throw new Error(`Missing mock: ${name}`);
    const m = new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
    await m.link(() => {});
    await m.evaluate();
    return m;
  }
  await module.link((name) => synthetic(name, mocks[name]));
  await module.evaluate();
  return module.namespace;
}
const eventMocks = {
  ...mailEvents,
  recordMailEvent: (_database, fields) => logs.push(mailEvents.mailEvent(fields)),
  observeMail: async (transport, options, _database, operation) => {
    const trace = options.trace || mailEvents.mailTrace();
    const event = (event, fields = {}) => logs.push(mailEvents.mailEvent({ ...fields, event, transport, trace }));
    event("started");
    try { return await operation({ ...options, trace }, event); }
    catch (error) { event("failed", { error: privacy.mailErrorCode(error) }); throw error; }
  },
};
const privateFree = (value) => assert.ok(!JSON.stringify(value).includes(marker), "letter leaked");
const nudgeEvents = () => logs.filter((e) => e?.event === "nudge" || e?.event === "nudge_failed");

// Fake Mongo: tells, users, the throttle rows and the newsletter opt-outs.
const stored = [];
const nudgeRows = new Map();
const unsubscribed = new Set();
let id = 0;
const collections = {
  tells: {
    createIndex: async () => {}, dropIndex: async () => {},
    insertOne: async (row) => { stored.push(row); return { insertedId: "a".repeat(23) + (++id % 10) }; },
  },
  users: { findOne: async () => ({ code: "ac25abcde" }) },
  "mail-nudges": {
    createIndex: async () => {},
    updateOne: async (filter, update, { upsert }) => {
      const cutoff = filter.$or[1].lastNudgedAt.$lt;
      const row = nudgeRows.get(filter.user);
      if (row && row.lastNudgedAt >= cutoff) {
        if (upsert) throw Object.assign(new Error(marker), { code: 11000 }); // unique `user`
        return { matchedCount: 0 };
      }
      nudgeRows.set(filter.user, { user: filter.user, lastNudgedAt: update.$set.lastNudgedAt, count: (row?.count || 0) + 1 });
      return { matchedCount: row ? 1 : 0, upsertedCount: row ? 0 : 1 };
    },
  },
  "email-blast-unsubscribes": { findOne: async ({ email }) => (unsubscribed.has(email) ? { email } : null) },
};
const database = { db: { collection: (name) => collections[name] }, disconnect: async () => {} };

let pushResult = { attempted: 0, succeeded: 0, failed: 0, pruned: 0 };
let lookup = async () => ({ email: "Reader@Example.invalid", email_verified: true });
const emails = [];
let transport = async (options) => { emails.push(options); return true; };
const backend = await load("../system/backend/mail.mjs", {
  "./mail-events.mjs": eventMocks,
  "./authorization.mjs": {
    handleFor: async (sub) => (sub === "sender" ? "prutti" : sub === "reader" ? "reader" : undefined),
    userIDFromHandleOrEmail: async () => "reader",
    userEmailFromID: (sub) => lookup(sub),
  },
  "./filter.mjs": { filter: (s) => s },
  "./mail-media.mjs": media,
  "./email.mjs": { email: (options) => transport(options) },
  "../../shared/mail-privacy.mjs": privacy,
  "../../shared/push.mjs": { sendToUser: async () => pushResult },
});

// (a) entities come undone before the filter sees the letter.
assert.equal(backend.clean("thank you &lt;3"), "thank you <3");
assert.equal(backend.clean("&amp;lt;"), "&lt;");
assert.equal(backend.clean("&#39;hi&#39; &#x263A; &#9829; &quot;q&quot; &apos;a&apos; &gt;"), "'hi' ☺ ♥ \"q\" 'a' >");
assert.equal(backend.clean("a &amp; b"), "a & b");
assert.equal(backend.clean("&#0; &#x110000; &bogus;"), "&#0; &#x110000; &bogus;", "invalid forms stay put");
assert.equal(backend.decodeEntities(undefined), "");

const letter = (extra = {}) => backend.deliver({ from: "sender", to: "reader", text: marker, subject: marker, ...extra }, database);

// (b) no phone buzzed, verified email → one nudge, naming the writer only.
let out = await letter();
assert.equal(out.nudge.status, "sent");
assert.equal(emails.length, 1);
assert.equal(emails[0].to, "reader@example.invalid");
assert.equal(emails[0].from, "aesthetic.computer <mail@aesthetic.computer>");
assert.equal(emails[0].subject, "@prutti wrote you a letter on aesthetic.computer");
assert.ok(emails[0].text.includes("https://aesthetic.computer/mail"));
assert.ok(/every few hours/.test(emails[0].text));
assert.deepEqual(nudgeEvents().map((e) => [e.event, e.status, e.letterId?.length]), [["nudge", "sent", 24]]);

// (d) the second letter inside six hours is throttled, and so is a burst.
out = await letter();
assert.equal(out.nudge.status, "throttled");
assert.equal(emails.length, 1);
nudgeRows.clear();
const burst = await Promise.all([letter(), letter(), letter()]);
assert.deepEqual(burst.map((r) => r.nudge.status).sort(), ["sent", "throttled", "throttled"]);
assert.equal(emails.length, 2);
// A window that has lapsed opens again.
nudgeRows.get("reader").lastNudgedAt = new Date(Date.now() - backend.NUDGE_EVERY - 1000);
assert.equal((await letter()).nudge.status, "sent");
assert.equal(nudgeRows.get("reader").count, 2);

// (c) a buzzed phone needs no email.
nudgeRows.clear();
pushResult = { attempted: 1, succeeded: 1, failed: 0, pruned: 0 };
const eventsBefore = nudgeEvents().length;
out = await letter();
assert.equal(out.nudge.status, "pushed");
assert.equal(emails.length, 3);
assert.equal(nudgeEvents().length, eventsBefore, "a pushed letter records no nudge event");
pushResult = { attempted: 2, succeeded: 0, failed: 2, pruned: 0 };

// (e) newsletter opt-out is honored; (f) no or unverified email.
nudgeRows.clear();
unsubscribed.add("reader@example.invalid");
assert.equal((await letter()).nudge.status, "unsubscribed");
unsubscribed.clear();
nudgeRows.clear();
lookup = async () => ({ email: "reader@example.invalid", email_verified: false });
assert.equal((await letter()).nudge.status, "no_email");
nudgeRows.clear();
lookup = async () => undefined;
assert.equal((await letter()).nudge.status, "no_email");
lookup = async () => ({ email: "reader@example.invalid", email_verified: true });
assert.equal(emails.length, 3);

// (h) a broken transport, a refused send, a broken lookup: letter still lands.
nudgeRows.clear();
const before = stored.length;
transport = async () => { throw Object.assign(new Error(marker), { code: "ECONNREFUSED" }); };
out = await letter();
assert.equal(out.nudge.status, "failed");
assert.equal(stored.length, before + 1);
assert.deepEqual(nudgeEvents().at(-1), { ...nudgeEvents().at(-1), event: "nudge_failed", error: "ECONNREFUSED" });
nudgeRows.clear();
transport = async () => false; // email.mjs swallows SMTP errors and says false
assert.equal((await letter()).nudge.status, "failed");
assert.equal(nudgeEvents().at(-1).status, "failed");
nudgeRows.clear();
lookup = async () => { throw new Error(marker); };
assert.equal((await letter()).nudge.status, "failed");
assert.equal(stored.length, before + 3);
lookup = async () => ({ email: "reader@example.invalid", email_verified: true });
transport = async (options) => { emails.push(options); return true; };

// Outside letters: the writer's display name, never their address.
nudgeRows.clear();
out = await backend.deliverFromOutside({ to: "reader", fromEmail: `${marker}@example.invalid`, fromName: "Aunt Mae", text: marker, subject: marker }, database);
assert.equal(out.nudge.status, "sent");
assert.equal(emails.at(-1).subject, "Aunt Mae wrote you a letter on aesthetic.computer");
nudgeRows.clear();
out = await backend.deliverFromOutside({ to: "reader", fromEmail: `${marker}@example.invalid`, text: marker }, database);
assert.equal(out.nudge.status, "sent");
assert.equal(emails.at(-1).subject, "Someone wrote you a letter on aesthetic.computer");
assert.ok(!emails.at(-1).text.includes("example.invalid"));
out = await backend.deliverFromOutside({ to: "reader", fromEmail: `${marker}@example.invalid`, text: marker, quiet: true }, database);
assert.equal(out.nudge.status, "skipped");
assert.equal(emails.length, 5);

// (g) nothing we mailed or logged carries the letter.
privateFree(emails);
privateFree(logs);

// The API echoes the nudge beside the push summary.
for (const api of ["mail", "tell"]) {
  nudgeRows.clear();
  const handler = await load(`../system/netlify/functions/${api}.mjs`, {
    "../../backend/mail-events.mjs": eventMocks,
    "../../backend/authorization.mjs": { authorize: async () => ({ sub: "sender" }) },
    "../../backend/database.mjs": { connect: async () => database },
    "../../backend/http.mjs": { respond: (status, body) => ({ statusCode: status, body }) },
    "../../backend/mail-media.mjs": media,
    "../../backend/mail.mjs": backend,
    "../../../shared/mail-privacy.mjs": privacy,
    mongodb: { ObjectId: class {} },
  });
  const res = await handler.handler({ httpMethod: "POST", headers: {}, body: JSON.stringify({ to: "@reader", text: `${marker} &lt;3` }) });
  assert.equal(res.statusCode, 200);
  assert.equal(res.body.nudge.status, "sent");
  assert.equal(res.body.push.attempted, 2);
  assert.equal(stored.at(-1).text, `${marker} <3`, `${api} decodes on the way in`);
  privateFree(res.body);
}
privateFree(emails);
console.log("mail nudge spec passed: entity decoding, nudge send/throttle/pushed/unsubscribed/no_email/failed, outside names, API echo");
