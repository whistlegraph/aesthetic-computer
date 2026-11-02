// Both storage adapters, isolated from Mongo, Datomic, Auth0 and inference.
import test, { mock } from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { kidlispSaveIdentity } from "../backend/kidlisp-save-identity.mjs";

let datomic = false, sequence = 0;
const mongo = [], entities = [], profileEvents = [];
const match = (row, query) => Object.entries(query).every(([k, v]) => row[k] === v);
const collection = {
  createIndex: async () => {},
  findOne: async query => mongo.find(row => match(row, query)) || null,
  insertOne: async row => {
    if (mongo.some(r => r.hash === row.hash)) throw Object.assign(Error("duplicate"), { code: 11000, keyPattern: { hash: 1 } });
    row._id = `id${++sequence}`; mongo.push(row); return { insertedId: row._id };
  },
  updateOne: async (query, update, options) => {
    let row = mongo.find(r => match(r, query));
    if (!row && options?.upsert) { row = { ...query, ...update.$setOnInsert }; mongo.push(row); return { upsertedCount: 1 }; }
    if (row) { Object.assign(row, update.$set); for (const [k,v] of Object.entries(update.$inc || {})) row[k] = (row[k] || 0) + v; }
    return { upsertedCount: 0 };
  },
};
mock.module("../backend/database.mjs", { exports: { connect: async () => ({ db: { collection: () => collection }, disconnect: async () => {} }) } });
mock.module("../backend/authorization.mjs", { exports: {
  authorize: async headers => { const id = headers?.authorization?.replace("Bearer ", ""); return ["alice", "bob", "nohandle"].includes(id) ? { sub: id } : null; },
  getHandleOrEmail: async id => id === "nohandle" ? "email@example.test" : `@${id}`,
} });
mock.module("../backend/generate-short-code.mjs", { exports: { generateUniqueCode: async () => `code${++sequence}` } });
mock.module("../backend/media-atproto.mjs", { exports: { createMediaRecord: async () => ({}), MediaTypes: { KIDLISP: "kidlisp" } } });
mock.module("../backend/profile-stream.mjs", { exports: { publishProfileEvent: async event => profileEvents.push(event) } });
mock.module("../backend/kidlisp-ast.mjs", { exports: { extractAst: () => [] } });
mock.module("../backend/kidlisp-sidecar.mjs", { exports: {
  kidlispDatomicEnabled: () => datomic,
  sidecar: {
    lookupByHash: async hash => entities.find(r => r.hash === hash) || null,
    lookupByCode: async code => entities.find(r => r.code === code) || null,
    create: async input => {
      const existing = entities.find(r => r.hash === input.hash);
      if (existing) return { code: existing.code, cached: true };
      entities.push({ ...input, user: input.user_sub, when: new Date().toISOString() });
      return { code: input.code, cached: false };
    },
    setAst: async () => {},
  },
} });
const { handler } = await import("../netlify/functions/store-kidlisp.mjs");
const save = async (source, user, handled = true) => {
  const reply = await handler({ httpMethod: "POST", headers: user ? { authorization: `Bearer ${user}` } : {}, body: JSON.stringify({ source, handled, handle: "@spoofed" }) });
  return { status: reply.statusCode, ...JSON.parse(reply.body) };
};

for (const mode of ["mongo", "datomic"]) test(`${mode}: handled saves isolate owners, deduplicate retries and preserve public copies`, async () => {
  datomic = mode === "datomic"; mongo.length = entities.length = profileEvents.length = 0;
  const source = "(wipe blue)";
  const shared = await save(source, null, false);
  const alice = await save(source, "alice");
  const again = await save(source, "alice");
  const bob = await save(source, "bob");
  assert.equal(alice.handled, true); assert.equal(alice.handle, "@alice");
  assert.notEqual(alice.code, shared.code); assert.notEqual(bob.code, alice.code);
  assert.equal(again.code, alice.code); assert.equal(again.cached, true);
  assert.equal((await save(source, null, false)).code, shared.code);
  assert.equal(profileEvents.filter(e => e.handle === "@alice").length, 1);
  const rows = datomic ? entities : mongo;
  assert.equal(rows.find(r => r.code === shared.code).user ?? null, null);
  assert.equal(rows.find(r => r.code === alice.code).user, "alice");
  assert.equal(rows.find(r => r.code === bob.code).source, source);
  for (const user of [null, "expired", "nohandle"]) {
    const before = rows.length;
    assert.ok([401, 403].includes((await save("(wipe red)", user)).status));
    assert.equal(rows.length, before);
  }
  const old = await save("(wipe green)", "alice", false);
  assert.equal((await save("(wipe green)", "alice")).code, old.code, "reuse the caller's earlier ordinary code");
  const race = await Promise.all([save("(wipe yellow)", "alice"), save("(wipe yellow)", "alice")]);
  assert.equal(race[0].code, race[1].code);
  assert.ok(race.every(r => r.handled && r.handle === "@alice"));
});

test("ordinary dedup hashes are unchanged and ownership keys survive handle renames", () => {
  const source = "(wipe blue)";
  assert.equal(kidlispSaveIdentity(source).hash, createHash("sha256").update(source).digest("hex"));
  const a = kidlispSaveIdentity(source, { handled: true, user: { sub: "alice" }, handle: "@alice" });
  const b = kidlispSaveIdentity(source, { handled: true, user: { sub: "alice" }, handle: "@renamed" });
  assert.equal(a.hash, b.hash);
  assert.throws(() => kidlispSaveIdentity(source, { handled: "true" }), /boolean/);
});
