// Run: node --experimental-vm-modules spec/mail-conversation-spec.mjs
// GET /api/mail?with= and POST { action: "read", with } — one conversation,
// both directions, and only ever the caller's own letters.
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";

const context = vm.createContext({ console, Date, Map, Set, Promise, JSON, Number, String, Array, Object, Math, encodeURIComponent, decodeURIComponent, Error });
async function load(path, mocks) {
  const module = new vm.SourceTextModule(await readFile(new URL(path, import.meta.url), "utf8"), { context });
  await module.link(async (name) => {
    assert.ok(mocks[name], `missing mock ${name}`);
    const exports = mocks[name];
    const mock = new vm.SyntheticModule(Object.keys(exports), function () {
      for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
    }, { context });
    await mock.link(() => {});
    await mock.evaluate();
    return mock;
  });
  await module.evaluate();
  return module.namespace;
}

// A tiny Mongo: equality, $ne and nested $or are all these queries use.
const matches = (doc, filter) => Object.entries(filter).every(([key, value]) => {
  if (key === "$or") return value.some((f) => matches(doc, f));
  if (value && typeof value === "object" && "$ne" in value) return doc[key] !== value.$ne;
  return doc[key] === value;
});
const at = (n) => new Date(Date.UTC(2026, 10, 24, 12, n));
let rows;
function reset() {
  rows = [
    { _id: "1", from: "reader", to: "me", fromHandle: "@reader", toHandle: "@me", text: "hi", when: at(1), read: false },
    { _id: "2", from: "me", to: "reader", fromHandle: "@me", toHandle: "@reader", text: "hello back", when: at(2) },
    { _id: "3", from: "reader", to: "me", fromHandle: "@reader", toHandle: "@me", text: "you there?", when: at(3), read: false },
    { _id: "4", from: "other", to: "me", fromHandle: "@other", toHandle: "@me", text: "not this one", when: at(4), read: false },
    { _id: "5", from: "reader", to: "other", fromHandle: "@reader", toHandle: "@other", text: "someone else's letter", when: at(5), read: false },
    { _id: "6", from: null, to: "me", fromHandle: "Pen Pal", fromEmail: "pal@example.com", toHandle: "@me", text: "from outside", when: at(6), read: false },
    { _id: "7", from: "me", to: null, toEmail: "pal@example.com", fromHandle: "@me", toHandle: "pal@example.com", text: "to outside", when: at(7) },
  ];
}
const tells = {
  createIndex: async () => {},
  find: (filter) => {
    let found = rows.filter((d) => matches(d, filter));
    const cursor = {
      sort: (spec) => { const dir = Object.values(spec)[0]; found.sort((a, b) => (a.when - b.when) * dir); return cursor; },
      limit: (n) => { found = found.slice(0, n); return cursor; },
      toArray: async () => found.map((d) => ({ ...d })),
    };
    return cursor;
  },
  updateMany: async (filter, { $set }) => {
    const hit = rows.filter((d) => matches(d, filter));
    hit.forEach((d) => Object.assign(d, $set));
    return { modifiedCount: hit.length };
  },
  countDocuments: async (filter) => rows.filter((d) => matches(d, filter)).length,
};
const database = { disconnect: async () => {}, db: { collection: () => tells } };
const subs = { "@reader": "reader", "reader@aesthetic.computer": "reader", "@other": "other", "@me": "me" };

const api = await load("../system/netlify/functions/mail.mjs", {
  "../../backend/mail-events.mjs": { mailTrace: () => "test", recordMailEvent: () => {} },
  "../../backend/authorization.mjs": { authorize: async () => ({ sub: "me" }) },
  "../../backend/database.mjs": { connect: async () => database },
  "../../backend/http.mjs": { respond: (statusCode, body, headers) => ({ statusCode, body, headers }) },
  "../../backend/mail-media.mjs": { attachmentList: () => [], attachmentThumbnail: async () => null, resolveMailMedia: async () => [] },
  "../../backend/mail.mjs": {
    addressesFor: async () => [], clean: (s) => s, deliver: async () => ({}), sendOutside: async () => ({}),
    mailbox: async () => tells, INBOUND_DOMAINS: ["aesthetic.computer"], MAX_SUBJECT_LENGTH: 80,
    subFromAddress: async (address) => subs[String(address).trim().toLowerCase()],
    addressShape: () => "handle",
  },
  "../../../shared/mail-privacy.mjs": { mailErrorCode: () => "UNKNOWN" },
  mongodb: { ObjectId: class { constructor(id) { return id; } } },
});
const get = (query) => api.handler({ httpMethod: "GET", headers: {}, queryStringParameters: query });
const post = (body) => api.handler({ httpMethod: "POST", headers: {}, body: JSON.stringify(body) });

reset();
let res = await get({ with: "@reader" });
assert.equal(res.statusCode, 200);
assert.deepEqual(res.body.letters.map((l) => l.text), ["hi", "hello back", "you there?"], "both directions, oldest first");
assert.deepEqual(res.body.letters.map((l) => l.mine), [false, true, false]);
assert.ok(res.body.letters.every((l) => l.text !== "someone else's letter"), "a letter between two other people never appears");
assert.equal(res.body.letters[1].read, true, "your own letters read as read");
assert.equal(res.body.with, "@reader");

res = await get({ with: "reader@aesthetic.computer" });
assert.deepEqual(res.body.letters.map((l) => l.text), ["hi", "hello back", "you there?"], "any spelling of the address is the same person");

res = await get({ with: "pal@example.com" });
assert.deepEqual(res.body.letters.map((l) => l.text), ["from outside", "to outside"], "an outside email is a conversation too");

assert.equal((await get({ with: "@nobody" })).statusCode, 404);
rows.push({ _id: "8", from: "me", to: "me", fromHandle: "@me", toHandle: "@me", text: "note to self", when: at(8), read: false });
res = await get({ with: "@me" });
assert.deepEqual(res.body.letters.map((l) => l.text), ["note to self"], "notes to yourself are their own conversation");
rows.pop();
assert.equal((await get({ with: "" })).statusCode, 404);

// Reading one conversation leaves everyone else's letters unread.
res = await post({ action: "read", with: "@reader" });
assert.equal(res.statusCode, 200);
assert.equal(res.body.read, 2);
assert.deepEqual(rows.filter((d) => d.read === false).map((d) => d._id).sort(), ["4", "5", "6"]);
assert.ok(rows.find((d) => d._id === "1").readAt instanceof Date);
assert.equal((await post({ action: "read", with: "@nobody" })).statusCode, 404);

// The whole-box read still works and touches only the caller's inbox.
reset();
res = await post({ action: "read" });
assert.equal(res.body.read, 4);
assert.equal(rows.find((d) => d._id === "5").read, false, "another person's letter is never marked");
console.log("mail conversation spec passed: both directions, addresses, outside email, isolation, reading");
