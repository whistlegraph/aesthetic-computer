// The short device code (WOLF-3847) is a label, not a key: credentials leave
// only for the secret handed to the device, approvals come only from
// device-login, wrong secrets burn the code, and everything expires.
import assert from "node:assert/strict";
import test, { mock } from "node:test";

// A small in-memory stand-in for the one Mongo collection the handler uses.
const docs = new Map();
const matches = (doc, query) =>
  Object.entries(query).every(([key, want]) => {
    if (want && typeof want === "object" && !(want instanceof Date)) {
      if ("$gte" in want) return doc[key] >= want.$gte;
      if ("$lt" in want) return doc[key] < want.$lt;
    }
    return doc[key] === want;
  });
const find = (query) => [...docs.values()].find((doc) => matches(doc, query));
const collection = {
  createIndex: async () => {},
  countDocuments: async (query) => [...docs.values()].filter((d) => matches(d, query)).length,
  deleteMany: async (query) => {
    for (const doc of [...docs.values()]) if (matches(doc, query)) docs.delete(doc._id);
  },
  insertOne: async (doc) => {
    if (docs.has(doc._id)) throw Object.assign(new Error("duplicate"), { code: 11000 });
    docs.set(doc._id, { ...doc });
  },
  findOne: async (query) => find(query) || null,
  updateOne: async (query, update) => {
    const doc = find(query);
    if (!doc) return { matchedCount: 0 };
    Object.assign(doc, update.$set || {});
    for (const [key, by] of Object.entries(update.$inc || {})) doc[key] = (doc[key] || 0) + by;
    return { matchedCount: 1 };
  },
  deleteOne: async (query) => {
    const doc = find(query);
    if (doc) docs.delete(doc._id);
  },
  findOneAndDelete: async (query) => {
    const doc = find(query);
    if (doc) docs.delete(doc._id);
    return doc || null;
  },
};
mock.module("mongodb", {
  exports: {
    MongoClient: class {
      async connect() {}
      db() {
        return { collection: () => collection };
      }
    },
  },
});

process.env.AC_DEVICE_SECRET = "server-to-server";
const { handler } = await import("../netlify/functions/device-auth.mjs");
const INTERNAL = { "x-ac-device-secret": "server-to-server" };
const get = async (query, headers = {}) => {
  const res = await handler({ httpMethod: "GET", headers, queryStringParameters: query });
  return { ...JSON.parse(res.body), http: res.statusCode };
};
const post = async (body, headers = {}) => {
  const res = await handler({ httpMethod: "POST", headers, body: JSON.stringify(body) });
  return { ...JSON.parse(res.body), http: res.statusCode };
};
const CREDS = { claudeAiOauth: { accessToken: "tok" } };
const request = () => get({ action: "request" });

test("a request hands the device a code and a long secret, stored only as a hash", async () => {
  const r = await request();
  assert.equal(r.http, 200);
  assert.match(r.code, /^[A-Z]{4}-\d{4}$/);
  assert.ok(r.secret.length >= 40);
  assert.ok(r.pollUrl.includes(r.secret));
  assert.equal(JSON.stringify(docs.get(r.code)).includes(r.secret), false);
});

test("the code alone collects nothing", async () => {
  const r = await request();
  await post({ action: "approve", code: r.code, credentials: CREDS }, INTERNAL);
  const poll = await get({ action: "poll", code: r.code });
  assert.equal(poll.http, 401);
  assert.equal("credentials" in poll, false);
  assert.ok(docs.has(r.code)); // Still waiting for the real device.
});

test("only device-login may approve", async () => {
  const r = await request();
  const open = await post({ action: "approve", code: r.code, credentials: CREDS });
  assert.equal(open.http, 401);
  assert.equal(docs.get(r.code).status, "pending");
});

test("the device collects its credentials once", async () => {
  const r = await request();
  assert.equal((await get({ action: "poll", code: r.code, secret: r.secret })).http, 200);
  await post({ action: "approve", code: r.code, credentials: CREDS }, INTERNAL);
  const first = await get({ action: "poll", code: r.code, secret: r.secret });
  assert.deepEqual(first.credentials, CREDS);
  const again = await get({ action: "poll", code: r.code, secret: r.secret });
  assert.equal(again.http, 404);
});

test("wrong secrets look like a missing code and burn it", async () => {
  const r = await request();
  await post({ action: "approve", code: r.code, credentials: CREDS }, INTERNAL);
  for (let i = 0; i < 5; i++) {
    const guess = await get({ action: "poll", code: r.code, secret: "guess-" + i });
    assert.equal(guess.http, 404);
    assert.equal("credentials" in guess, false);
  }
  assert.equal(docs.has(r.code), false);
  assert.equal((await get({ action: "poll", code: r.code, secret: r.secret })).http, 404);
});

test("an expired code is gone, approved or not", async () => {
  const r = await request();
  await post({ action: "approve", code: r.code, credentials: CREDS }, INTERNAL);
  docs.get(r.code).createdAt = new Date(Date.now() - 11 * 60 * 1000);
  assert.equal((await get({ action: "poll", code: r.code, secret: r.secret })).http, 404);
  assert.equal((await post({ action: "approve", code: r.code, credentials: CREDS }, INTERNAL)).http, 404);
});

test("with no device secret configured, approvals fail closed", async () => {
  const r = await request();
  const saved = process.env.AC_DEVICE_SECRET;
  delete process.env.AC_DEVICE_SECRET;
  const res = await post({ action: "approve", code: r.code, credentials: CREDS }, { "x-ac-device-secret": "" });
  process.env.AC_DEVICE_SECRET = saved;
  assert.equal(res.http, 401);
});
