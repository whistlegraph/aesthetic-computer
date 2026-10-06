// node --experimental-vm-modules --test system/tests/account-deletion.test.mjs
// The deletion job (backend/account-deletion.mjs) against in-memory fakes of
// Mongo, Redis, Spaces, Auth0 and mail; and the endpoint's safety rules.
import test from "node:test";
import assert from "node:assert/strict";
import vm from "node:vm";
import { readFile } from "node:fs/promises";
import {
  exportAccount,
  previewDeletion,
  requestDeletion,
  restoreDeletion,
  runDueDeletions,
  handleQuarantined,
  hash,
  GRACE_MS,
  HANDLE_QUARANTINE_MS,
  LEDGER,
  TOMBSTONES,
} from "../backend/account-deletion.mjs";

const DAY = 24 * 60 * 60 * 1000;
const T0 = new Date("2026-09-26T12:00:00Z");
const later = (ms) => new Date(T0.getTime() + ms);

// ── A small Mongo: enough query and update operators for the job ──
const path = (doc, key) => key.split(".").reduce((v, k) => (v == null ? undefined : v[k]), doc);
function setPath(doc, key, value) {
  const keys = key.split(".");
  let node = doc;
  for (const k of keys.slice(0, -1)) node = node[k] ??= {};
  node[keys.at(-1)] = value;
}
function unsetPath(doc, key) {
  const keys = key.split(".");
  let node = doc;
  for (const k of keys.slice(0, -1)) if (!(node = node?.[k])) return;
  delete node[keys.at(-1)];
}
function test1(value, cond) {
  if (cond && typeof cond === "object" && !(cond instanceof Date) && !Array.isArray(cond)) {
    return Object.entries(cond).every(([op, arg]) => {
      if (op === "$in") return arg.some((a) => test1(value, a));
      if (op === "$ne") return !test1(value, arg);
      if (op === "$lte") return value != null && value <= arg;
      if (op === "$gt") return value != null && value > arg;
      if (op === "$regex") return typeof value === "string" && new RegExp(arg).test(value);
      throw new Error(`fake mongo: ${op}`);
    });
  }
  if (Array.isArray(value)) return value.some((v) => test1(v, cond));
  if (value instanceof Date && cond instanceof Date) return value.getTime() === cond.getTime();
  return value === cond;
}
function matches(doc, query) {
  return Object.entries(query).every(([key, cond]) =>
    key === "$or" ? cond.some((q) => matches(doc, q)) : test1(path(doc, key), cond),
  );
}
function apply(doc, update, inserting) {
  for (const [op, fields] of Object.entries(update)) {
    for (const [key, value] of Object.entries(fields)) {
      if (op === "$set" || (op === "$setOnInsert" && inserting)) setPath(doc, key, value);
      else if (op === "$unset") unsetPath(doc, key);
      else if (op === "$inc") setPath(doc, key, (path(doc, key) || 0) + value);
    }
  }
}
function fakeDb(seed = {}) {
  const data = new Map(Object.entries(structuredClone(seed)));
  const collection = (name) => {
    if (!data.has(name)) data.set(name, []);
    const docs = () => data.get(name);
    const keep = (fn) => data.set(name, docs().filter(fn));
    return {
      findOne: async (q) => structuredClone(docs().find((d) => matches(d, q)) ?? null),
      find: (q) => ({ toArray: async () => structuredClone(docs().filter((d) => matches(d, q))) }),
      insertOne: async (doc) => {
        if (docs().some((d) => d._id === doc._id)) throw Object.assign(new Error("dup"), { code: 11000 });
        // @handles has a unique index on `handle` (handle.mjs).
        if (name === "@handles" && docs().some((d) => d.handle === doc.handle)) {
          throw Object.assign(new Error("dup handle"), { code: 11000 });
        }
        docs().push(structuredClone(doc));
      },
      replaceOne: async (q, doc, { upsert } = {}) => {
        const i = docs().findIndex((d) => matches(d, q));
        const next = { ...structuredClone(doc), _id: i >= 0 ? docs()[i]._id : q._id ?? doc._id };
        if (i >= 0) docs()[i] = next;
        else if (upsert) docs().push(next);
      },
      updateOne: async (q, u, { upsert } = {}) => {
        const doc = docs().find((d) => matches(d, q));
        if (doc) apply(doc, u, false);
        else if (upsert) {
          const fresh = { ...q };
          apply(fresh, u, true);
          docs().push(fresh);
        }
        return { matchedCount: doc ? 1 : 0 };
      },
      updateMany: async (q, u) => docs().filter((d) => matches(d, q)).forEach((d) => apply(d, u, false)),
      deleteOne: async (q) => {
        const i = docs().findIndex((d) => matches(d, q));
        if (i >= 0) docs().splice(i, 1);
        return { deletedCount: i >= 0 ? 1 : 0 };
      },
      deleteMany: async (q) => {
        const before = docs().length;
        keep((d) => !matches(d, q));
        return { deletedCount: before - docs().length };
      },
      findOneAndUpdate: async (q, u, { sort } = {}) => {
        const found = docs().filter((d) => matches(d, q));
        if (sort) {
          const [[key, dir]] = Object.entries(sort);
          found.sort((a, b) => (path(a, key) > path(b, key) ? dir : -dir));
        }
        if (!found[0]) return null;
        apply(found[0], u, false);
        return structuredClone(found[0]);
      },
    };
  };
  return { collection, all: (name) => structuredClone(data.get(name) || []) };
}

function fakeDeps(db, { atproto = () => ({ deleted: true }), sotce = null } = {}) {
  const calls = [];
  const kv = new Map();
  const deps = {};
  const hashFor = (name) => (kv.has(name) ? kv.get(name) : kv.set(name, new Map()).get(name));
  const glob = (pattern) =>
    new RegExp(`^${pattern.replace(/[.+?^${}()|[\]\\]/g, "\\$&").replace(/\*/g, ".*")}$`);
  return Object.assign(deps, {
    calls,
    kvStore: kv,
    db,
    log() {},
    kv: {
      connect: async () => {},
      del: async (name, key) => hashFor(name).delete(key),
      scan: async (name, match) => [...hashFor(name).keys()].filter((k) => glob(match).test(k)),
    },
    storage: {
      deleteKeys: async (bucket, keys) => (calls.push(["deleteKeys", bucket, ...keys]), keys.length),
      deletePrefix: async (bucket, prefix) => (calls.push(["deletePrefix", bucket, prefix]), 1),
      list: async (bucket, prefix) => [{ key: `${prefix}painting/p1.png`, size: 10, url: "https://example/p1.png" }],
    },
    stripe: { forget: async (email) => (calls.push(["stripe", email]), 1) },
    chat: { erase: async (sub) => (calls.push(["chat", sub]), true) },
    ipfs: { unpin: async (cid) => (calls.push(["unpin", cid]), true) },
    bluesky: { deletePost: async (rkey) => (calls.push(["bluesky", rkey]), true) },
    sidecar: { enabled: false },
    atproto: { deleteAccount: async () => (calls.push(["atproto"]), atproto()) },
    auth0: {
      sotceSister: async () => !!sotce,
      sotceSub: async () => sotce,
      remove: async (sub) => (calls.push(["identity", sub]), true),
    },
    lock: async (sub) => {
      if (deps.lockFails) throw new Error("redis down");
      calls.push(["lock", sub]);
    },
    unlock: async (sub, options = {}) => {
      if (deps.unlockFails) throw new Error("auth0 down");
      calls.push(["unlock", sub, !!options.identityDeleted]);
    },
    mail: {
      scheduled: async (m) => (calls.push(["mail:scheduled", m.to, m.restoreToken]), true),
      completed: async (m) => (calls.push(["mail:completed", m.to]), true),
    },
  });
}

const SUB = "auth0|me";
const OTHER = "auth0|them";
const user = { sub: SUB, email: "me@example.com", email_verified: true };

function world() {
  return {
    "@handles": [{ _id: SUB, handle: "me" }, { _id: OTHER, handle: "them" }],
    users: [{ _id: SUB, code: "ac12345" }, { _id: OTHER, code: "ac99999" }],
    paintings: [{ user: SUB, code: "p1" }, { user: OTHER, code: "p2" }],
    moods: [{ user: SUB, mood: "hi", bluesky: { rkey: "rk1" } }],
    tapes: [{ user: SUB, code: "t1" }],
    "chat-system": [{ user: SUB, text: "hello" }, { user: OTHER, text: "hey" }],
    "chat-clock": [{ user: SUB, text: "tick" }],
    "account-activity": [{ user: SUB, tenant: "aesthetic", action: "note_played" }, { user: OTHER, tenant: "aesthetic", action: "piece_opened" }, { user: SUB, tenant: "sotce", action: "piece_opened" }],
    logs: [{ users: [SUB], text: "hi @me" }, { users: [OTHER], text: "hi @them" }],
    "device-creds": [{ _id: SUB, claudeToken: "sk-ant-x", githubPat: "ghp_x" }],
    "news-posts": [{ user: SUB, code: "n1" }],
    kidlisp: [
      { user: SUB, code: "gone", source: "(wipe red)" },
      { user: SUB, code: "minted", source: "(ink)", kept: { tokenId: 7, keptBy: SUB } },
      { user: SUB, code: "shared", source: "(line)" },
      { user: OTHER, code: "theirs", source: "(embed $shared)" },
    ],
    "ac-credit-wallets": [{ _id: SUB, balance: 5 }],
    "walkieware-threads": [
      { _id: "my-thread", owner: SUB, code: "wgMine", ledger: { head: 1, versions: [{ id: 1, source: "(wipe red)", request: "red please" }] } },
      { _id: "their-thread", owner: OTHER, code: "wgOther", ledger: { head: 1, versions: [{ id: 1, source: "(wipe blue)", request: "blue please" }] } },
    ],
    "whistlegraph-roblox-rooms": [
      { _id: SUB, revision: 1, room: { name: "my room" } },
      { _id: OTHER, revision: 3, room: { name: "their room" } },
    ],
    boots: [{ meta: { user: { sub: SUB, handle: "@me" } }, server: { ip: "1.2.3.4", country: "US" } }],
  };
}

function purchaseWorld() {
  const receipt = (who, transactionId) => ({
    _id: `apple:whistlegraph:Production:${transactionId}`,
    user: who,
    appAccountToken: who === SUB ? "4adcdf8d-d1cb-4e68-a965-c6c4f0a57375" : "dd9d7e65-ef88-4c93-b279-5d072a5ad1f6",
    transactionId, productId: "computer.aesthetic.walkieware.braincells.1m",
    credits: 1_000_000, environment: "Production", createdAt: T0, deliveredAt: T0,
  });
  const purchases = [receipt(SUB, "1001"), receipt(OTHER, "1002")];
  return {
    ...world(),
    "whistlegraph-iap-accounts": purchases.map(p => ({ _id: p.user, appAccountToken: p.appAccountToken, createdAt: T0 })),
    "whistlegraph-iap-purchases": purchases,
    "whistlegraph-iap-notifications": purchases.map(p => ({
      _id: `Production:${p.transactionId}`, purchaseId: p._id, user: p.user,
      type: "REFUND_REVERSED", signedPayload: `private-jws-${p.transactionId}`,
      receivedAt: T0, status: "pending",
    })),
  };
}

test("asking locks the account, schedules the purge after the grace period and mails a restore link", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  const first = await requestDeletion(deps, { user, now: T0 });
  assert.equal(first.state, "scheduled");
  assert.equal(first.purgeAfter.getTime(), T0.getTime() + GRACE_MS);
  assert.deepEqual(deps.calls.map((c) => c[0]), ["lock", "mail:scheduled"]);
  const [job] = db.all(LEDGER);
  assert.equal(job.restoreTokenHash, hash(deps.calls[1][2]));
  assert.ok(!JSON.stringify(job).includes(deps.calls[1][2]), "the ledger never stores the token itself");

  const again = await requestDeletion(deps, { user, now: later(DAY) });
  assert.equal(again.purgeAfter.getTime(), first.purgeAfter.getTime());
  assert.deepEqual(deps.calls.map((c) => c[0]), ["lock", "mail:scheduled", "lock"], "asking twice re-locks, never re-mails");
});

test("the emailed token keeps the account until the purge begins, once", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  const token = deps.calls[1][2];

  assert.equal((await restoreDeletion(deps, { token: "x".repeat(43), now: later(DAY) })).restored, false);
  const kept = await restoreDeletion(deps, { token, now: later(DAY) });
  assert.deepEqual(kept, { restored: true, handle: "me" });
  assert.deepEqual(deps.calls.at(-1), ["unlock", SUB, false]);
  assert.equal(db.all(LEDGER).length, 0);
  assert.equal((await restoreDeletion(deps, { token, now: later(DAY) })).restored, false);
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS + DAY) }), []);
});

test("a restore link cannot stop a purge that is due", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  const token = deps.calls[1][2];
  assert.equal((await restoreDeletion(deps, { token, now: later(GRACE_MS + 1) })).restored, false);
});

test("nothing is purged during the grace period", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS - 1) }), []);
  assert.equal(db.all("paintings").length, 2);
});

test("the purge removes the account everywhere and keeps only what it must, without the person", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  deps.kvStore.set(
    "userCache",
    new Map([
      ["email:me@example.com:aesthetic:true:false", "{}"],
      ["email:@me:aesthetic:true:true", "{}"],
      ["code:ac12345:aesthetic:false:false", "{}"],
      ["email:@them:aesthetic:true:true", "{}"],
    ]),
  );
  await requestDeletion(deps, { user, now: T0 });
  const results = await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.deepEqual(results, [{ state: "completed" }]);

  for (const name of ["paintings", "moods", "tapes", "chat-system", "chat-clock", "logs", "users", "@handles", "walkieware-threads", "whistlegraph-roblox-rooms"]) {
    assert.ok(!JSON.stringify(db.all(name)).includes(SUB), `${name} still names the account`);
  }
  assert.equal(db.all("paintings").length, 1, "other people's work stays");
  assert.equal(db.all("chat-system").length, 1);
  assert.equal(db.all("walkieware-threads").length, 1, "other people's Whistlegraph drafts stay");
  assert.deepEqual(db.all("whistlegraph-roblox-rooms"), [{ _id: OTHER, revision: 3, room: { name: "their room" } }]);
  assert.deepEqual(db.all("account-activity"), [{ user: OTHER, tenant: "aesthetic", action: "piece_opened" }, { user: SUB, tenant: "sotce", action: "piece_opened" }]);
  assert.equal(db.all("device-creds").length, 0, "saved device secrets are deleted");

  const kidlisp = Object.fromEntries(db.all("kidlisp").map((k) => [k.code, k]));
  assert.equal(kidlisp.gone, undefined, "unshared, unminted KidLisp is deleted");
  assert.equal(kidlisp.minted.user, undefined);
  assert.equal(kidlisp.minted.kept.keptBy, undefined);
  assert.equal(kidlisp.shared.user, undefined, "KidLisp another piece uses stays without its author");
  assert.equal(kidlisp.theirs.user, OTHER);

  assert.equal(db.all("ac-credit-wallets")[0].balance, 5, "the purchase ledger is kept for the books");
  assert.ok(!JSON.stringify(db.all("ac-credit-wallets")).includes(SUB));
  assert.deepEqual(db.all("boots")[0], { meta: {}, server: { country: "US" } });

  assert.deepEqual([...deps.kvStore.get("userCache").keys()], ["email:@them:aesthetic:true:true"]);
  assert.ok(deps.calls.some((c) => c[0] === "deletePrefix" && c[2] === `${SUB}/`));
  assert.ok(deps.calls.some((c) => c[0] === "deleteKeys" && c.includes("tapes/t1.mp4")));
  assert.ok(deps.calls.some((c) => c[0] === "deleteKeys" && c.includes("og/news/n1.png")));
  assert.ok(deps.calls.some((c) => c[0] === "bluesky" && c[1] === "rk1"));
  assert.ok(deps.calls.some((c) => c[0] === "stripe" && c[1] === "me@example.com"));
  assert.ok(deps.calls.some((c) => c[0] === "chat" && c[1] === SUB));

  const order = deps.calls.map((c) => c[0]);
  assert.ok(order.indexOf("identity") > order.indexOf("atproto"), "the sign-in identity goes last");
  assert.deepEqual(deps.calls.slice(-2), [["mail:completed", "me@example.com"], ["unlock", SUB, true]]);

  assert.deepEqual(db.all(LEDGER), [], "the ledger is gone");
  assert.deepEqual(db.all(TOMBSTONES).map((t) => t._id), [hash(SUB)], "only a hash remains");
  assert.equal(await handleQuarantined(db, "@ME", later(GRACE_MS + DAY)), true);
  assert.equal(await handleQuarantined(db, "me", later(GRACE_MS + HANDLE_QUARANTINE_MS + DAY)), false);
  assert.equal(await handleQuarantined(db, "them", later(GRACE_MS)), false);
});

test("Apple account links and signed notifications are deleted but transaction anti-replay claims remain", async () => {
  const seed = purchaseWorld();
  const db = fakeDb(seed), deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS - 1) }), []);
  assert.deepEqual(db.all("whistlegraph-iap-accounts"), seed["whistlegraph-iap-accounts"], "the grace period preserves restorable purchases");
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "completed" }]);

  assert.deepEqual(db.all("whistlegraph-iap-accounts"), [seed["whistlegraph-iap-accounts"][1]]);
  assert.deepEqual(db.all("whistlegraph-iap-notifications"), [seed["whistlegraph-iap-notifications"][1]]);
  const [deleted, other] = db.all("whistlegraph-iap-purchases");
  const { user: removedUser, appAccountToken, ...receipt } = seed["whistlegraph-iap-purchases"][0];
  assert.deepEqual(deleted, { ...receipt, deletedAt: later(GRACE_MS), userHash: hash(SUB) });
  assert.deepEqual(other, seed["whistlegraph-iap-purchases"][1]);
  assert.equal(await db.collection("ac-credit-wallets").findOne({ _id: SUB }), null);
  assert.deepEqual(db.all(TOMBSTONES), [{ _id: hash(SUB), completedAt: later(GRACE_MS) }]);
});

test("a purchase cleanup failure keeps the deletion retryable before removing the wallet", async () => {
  const db = fakeDb(purchaseWorld()), deps = fakeDeps(db);
  const collection = db.collection;
  let unavailable = true;
  db.collection = name => {
    const result = collection(name);
    if (name === "whistlegraph-iap-purchases") {
      const update = result.updateMany;
      result.updateMany = async (...args) => {
        if (unavailable) throw new Error("purchase ledger unavailable");
        return update(...args);
      };
    }
    return result;
  };
  await requestDeletion(deps, { user, now: T0 });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "failed", step: "whistlegraph-purchases" }]);
  assert.equal(await db.collection("whistlegraph-iap-accounts").findOne({ _id: SUB }), null);
  assert.equal((await db.collection("ac-credit-wallets").findOne({ _id: SUB })).balance, 5);
  assert.ok(!deps.calls.some(call => call[0] === "identity"));
  unavailable = false;
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS + DAY) }), [{ state: "completed" }]);
  assert.equal(db.all("whistlegraph-iap-purchases")[0].userHash, hash(SUB));
  assert.ok(!db.all("whistlegraph-iap-notifications").some(row => row.user === SUB));
});

test("Whistlegraph mint drafts export without capabilities and retire without personal data", async () => {
  const seed = world();
  seed["whistlegraph-mints"] = [
    { _id: "draft-capability-hash", user: SUB, handle: "me", source: "(wipe red)", cover: "private-png", nonce: "secret-nonce", status: "packing", title: "my draft" },
    { _id: "minted-capability-hash", user: SUB, handle: "me", sender: "tz1MyWallet", status: "minted", artifactUri: "ipfs://QmMinted", operationHash: "ooMinted", tokenId: "77", title: "my artwork" },
    { _id: "their-capability-hash", user: OTHER, handle: "them", source: "(wipe blue)", status: "packing" },
  ];
  const db = fakeDb(seed), deps = fakeDeps(db);
  const copy = await exportAccount(deps, { user, now: T0 });
  assert.deepEqual(copy.records["whistlegraph-mints"], [
    { source: "(wipe red)", status: "packing", title: "my draft" },
    { sender: "tz1MyWallet", status: "minted", artifactUri: "ipfs://QmMinted", operationHash: "ooMinted", tokenId: "77", title: "my artwork" },
  ]);
  assert.ok(!JSON.stringify(copy).includes("capability-hash"));
  assert.ok(!JSON.stringify(copy).includes("secret-nonce"));
  await requestDeletion(deps, { user, now: T0 });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "completed" }]);
  assert.deepEqual(db.all("whistlegraph-mints"), [
    { _id: "draft-capability-hash", status: "deleted", deletedAt: later(GRACE_MS) },
    { _id: "minted-capability-hash", status: "deleted", deletedAt: later(GRACE_MS), operationHash: "ooMinted", tokenId: "77" },
    seed["whistlegraph-mints"][2],
  ]);
  assert.ok(!deps.calls.some(call => call[0] === "unpin" && call[1] === "QmMinted"), "on-chain artwork is left intact");
});

test("a failed step stops before the identity and resumes there after a backoff", async () => {
  const db = fakeDb(world());
  let pdsUp = false;
  const deps = fakeDeps(db, {
    atproto: () => (pdsUp ? { deleted: true } : { deleted: false, reason: "request-failed" }),
  });
  await requestDeletion(deps, { user, now: T0 });

  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "failed", step: "atproto" }]);
  assert.ok(!deps.calls.some((c) => c[0] === "identity"));
  assert.equal(db.all("@handles").length, 2, "the handle is held until the job finishes");
  const [job] = db.all(LEDGER);
  assert.equal(job.state, "failed");
  assert.equal(job.steps.records.status, "done");

  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS + 60_000) }), [], "backoff");
  pdsUp = true;
  const storageCalls = deps.calls.filter((c) => c[0] === "deletePrefix").length;
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS + DAY) }), [{ state: "completed" }]);
  assert.equal(
    deps.calls.filter((c) => c[0] === "deletePrefix").length,
    storageCalls,
    "finished steps are not repeated",
  );
  assert.ok(deps.calls.some((c) => c[0] === "identity"));
});

test("an account without a handle or email deletes cleanly and never turns an email into a handle", async () => {
  const seed = world();
  seed["@handles"] = [{ _id: OTHER, handle: "them" }];
  const db = fakeDb(seed);
  const deps = fakeDeps(db, { sotce: "sotce-id" });
  await requestDeletion(deps, { user: { sub: SUB }, now: T0 });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "completed" }]);
  assert.deepEqual(db.all("@handles"), [{ _id: OTHER, handle: "them" }]);
  assert.ok(!deps.calls.some((c) => c[0] === "stripe"), "no email, no payment lookup");
});

test("a Sotce Net account with the same verified email keeps the handle, unquarantined", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db, { sotce: "abc" });
  await requestDeletion(deps, { user, now: T0 });
  await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.deepEqual(db.all("@handles").find((h) => h.handle === "me"), { _id: "sotce-abc", handle: "me" });
  assert.equal(await handleQuarantined(db, "me", later(GRACE_MS)), false);
});

test("an unverified email is never used to find payments or a sister account", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db, { sotce: "abc" });
  await requestDeletion(deps, { user: { ...user, email_verified: false }, now: T0 });
  await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.ok(!deps.calls.some((c) => c[0] === "stripe"));
  assert.equal(db.all("@handles").some((h) => h._id === "sotce-abc"), false);
});

// ── The endpoint ──
async function endpoint({ authorized = true } = {}) {
  const calls = [];
  const context = vm.createContext({
    process: { env: {} },
    console: { log() {}, error() {} },
    URLSearchParams,
  });
  const mocks = {
    "../../backend/authorization.mjs": { authorize: async () => (authorized ? user : undefined) },
    "../../backend/database.mjs": {
      connect: async () => ({ db: {}, disconnect: async () => calls.push("disconnect") }),
    },
    "../../backend/http.mjs": { respond: (statusCode, body, headers = {}) => ({ statusCode, body, headers }) },
    "../../backend/account-deletion.mjs": {
      previewDeletion: async () => (calls.push("preview"), { braincells: 5 }),
      exportAccount: async () => (calls.push("export"), { records: {} }),
      requestDeletion: async () => (calls.push("request"), { state: "scheduled", purgeAfter: later(GRACE_MS) }),
      restoreDeletion: async (_, { token }) => (calls.push(["restore", token]), { restored: token === "good" }),
    },
    "../../backend/account-deletion-deps.mjs": { productionDeps: async () => ({}) },
  };
  const source = await readFile(
    new URL("../netlify/functions/delete-erase-and-forget-me.mjs", import.meta.url),
    "utf8",
  );
  const module = new vm.SourceTextModule(source, { context });
  await module.link(
    (name) =>
      new vm.SyntheticModule(
        Object.keys(mocks[name]),
        function () {
          for (const [key, value] of Object.entries(mocks[name])) this.setExport(key, value);
        },
        { context },
      ),
  );
  await module.evaluate();
  return { calls, handler: module.namespace.handler };
}

test("opening the emailed link changes nothing; its button does", async () => {
  const e = await endpoint();
  const page = await e.handler({ httpMethod: "GET", queryStringParameters: { restore: "good" }, headers: {} });
  assert.equal(page.statusCode, 200);
  assert.match(page.body, /name="restore" value="good"/);
  assert.deepEqual(e.calls, []);
  const kept = await e.handler({ httpMethod: "POST", body: "restore=good", headers: {} });
  assert.match(kept.body, /kept/);
  assert.deepEqual(e.calls, [["restore", "good"], "disconnect"]);
});

test("the restore page escapes the token", async () => {
  const e = await endpoint();
  const page = await e.handler({
    httpMethod: "GET",
    queryStringParameters: { restore: '"><script>' },
    headers: {},
  });
  assert.ok(!page.body.includes("<script>"));
});

test("asking needs a signed-in account and answers old clients the way they expect", async () => {
  const anonymous = await endpoint({ authorized: false });
  assert.equal((await anonymous.handler({ httpMethod: "POST", headers: {} })).statusCode, 401);
  assert.deepEqual(anonymous.calls, []);

  const e = await endpoint();
  const response = await e.handler({ httpMethod: "POST", headers: {} });
  assert.equal(response.statusCode, 200);
  assert.equal(response.body.result, "Deleted!");
  assert.equal(response.body.state, "scheduled");
  assert.deepEqual(e.calls, ["request", "disconnect"]);
});

// ── Regressions from review ──

test("a request that failed to lock is repaired by asking again", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  deps.lockFails = true;
  await assert.rejects(requestDeletion(deps, { user, now: T0 }));
  deps.lockFails = false;
  const schedule = await requestDeletion(deps, { user, now: later(60_000) });
  assert.equal(schedule.mailed, true);
  assert.equal(schedule.purgeAfter.getTime(), T0.getTime() + GRACE_MS, "the original schedule holds");
  assert.deepEqual(deps.calls.map((c) => c[0]), ["lock", "mail:scheduled"]);
});

test("without an email the schedule says no link was sent", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  const schedule = await requestDeletion(deps, { user: { sub: SUB }, now: T0 });
  assert.equal(schedule.mailed, false);
});

test("a restore whose unlock fails stays retryable with the same link", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  const token = deps.calls[1][2];
  deps.unlockFails = true;
  await assert.rejects(restoreDeletion(deps, { token, now: later(DAY) }));
  assert.equal(db.all(LEDGER)[0].state, "scheduled");
  deps.unlockFails = false;
  assert.equal((await restoreDeletion(deps, { token, now: later(DAY) })).restored, true);
});

test("a restore interrupted between its unlocks is finished by the runner, never purged", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  await db.collection(LEDGER).updateOne({ _id: SUB }, { $set: { state: "restoring", restoringSince: later(DAY) } });
  assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS + DAY) }), [{ state: "restored" }]);
  assert.equal(db.all(LEDGER).length, 0);
  assert.equal(db.all("paintings").length, 2);
});

test("the sotce hand-off respects the unique handle index and survives a retry", async () => {
  const db = fakeDb(world());
  const seed = world();
  seed["@handles"][0].handle = "MeMe";
  const db2 = fakeDb(seed);
  for (const database of [db, db2]) {
    const deps = fakeDeps(database, { sotce: "abc" });
    await requestDeletion(deps, { user, now: T0 });
    assert.deepEqual(await runDueDeletions(deps, { now: later(GRACE_MS) }), [{ state: "completed" }]);
  }
  assert.deepEqual(db2.all("@handles").find((h) => h._id === "sotce-abc"), { _id: "sotce-abc", handle: "MeMe" });
});

test("a keep in any state, and everything a kept piece uses, is never deleted", async () => {
  const seed = world();
  seed.kidlisp = [
    { user: SUB, code: "pending", source: "(uses $b)", pendingKeep: { txHash: "x" } },
    { user: SUB, code: "b", source: "(uses $c)" },
    { user: SUB, code: "c", source: "(dot)" },
    { user: SUB, code: "legacy", source: "(x)", tezos: { tokenId: 3 } },
    { user: SUB, code: "lonely", source: "(y)" },
  ];
  const db = fakeDb(seed);
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.deepEqual(db.all("kidlisp").map((k) => k.code).sort(), ["b", "c", "legacy", "pending"]);
  assert.ok(db.all("kidlisp").every((k) => k.user === undefined));
});

test("caches are cleared after the records that could refill them", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  const order = [];
  const del = deps.kv.del;
  deps.kv.del = async (...args) => (order.push(["kv", db.all("users").length, db.all("@handles").length]), del(...args));
  await requestDeletion(deps, { user, now: T0 });
  await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.ok(order.length && order.every(([, users, handles]) => users === 1 && handles === 1));
});

test("a worker that lost its lease stops instead of overwriting the new owner", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  const bluesky = deps.bluesky.deletePost;
  deps.bluesky.deletePost = async (rkey) => {
    await db.collection(LEDGER).updateOne({ _id: SUB }, { $set: { leaseOwner: "someone-else" } });
    return bluesky(rkey);
  };
  const [result] = await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.equal(result.state, "handed-off");
  assert.ok(!deps.calls.some((c) => c[0] === "identity"));
});

test("the preview names what goes, what stays and the braincells lost, and changes nothing", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  const preview = await previewDeletion(deps, { user, now: T0 });
  assert.deepEqual(preview, {
    handle: "me",
    email: "me@example.com",
    graceDays: 14,
    handleHoldDays: 90,
    handleGoesToSotce: false,
    braincells: 5,
    counts: { paintings: 1, pieces: 0, whistlegraphs: 1, moods: 1, tapes: 1, news: 1, chat: 2, kidlispDeleted: 1, kidlispKept: 2 },
  });
  assert.deepEqual(deps.calls, []);
  assert.deepEqual(db.all(LEDGER), []);
});

test("the preview needs a signed-in account", async () => {
  const anonymous = await endpoint({ authorized: false });
  assert.equal((await anonymous.handler({ httpMethod: "GET", queryStringParameters: { preview: "" }, headers: {} })).statusCode, 401);
  const e = await endpoint();
  const response = await e.handler({ httpMethod: "GET", queryStringParameters: { preview: "" }, headers: {} });
  assert.equal(response.statusCode, 200);
  assert.deepEqual(e.calls, ["preview", "disconnect"]);
});

test("bundles pinned for deleted, unminted KidLisp are unpinned; kept pieces keep theirs", async () => {
  const seed = world();
  seed.kidlisp = [
    { user: SUB, code: "draft", source: "(x)", ipfsMedia: { artifactUri: "ipfs://QmDraft", thumbnailUri: "ipfs://QmShared" } },
    { user: SUB, code: "minted", source: "(y)", kept: { tokenId: 1, artifactUri: "ipfs://QmMinted", thumbnailUri: "ipfs://QmShared" } },
  ];
  const db = fakeDb(seed);
  const deps = fakeDeps(db);
  await requestDeletion(deps, { user, now: T0 });
  await runDueDeletions(deps, { now: later(GRACE_MS) });
  assert.deepEqual(deps.calls.filter((c) => c[0] === "unpin"), [["unpin", "QmDraft"]]);
});

test("the export holds what the account made and nobody else's, and changes nothing", async () => {
  const db = fakeDb(world());
  const deps = fakeDeps(db);
  const copy = await exportAccount(deps, { user, now: T0 });
  assert.equal(copy.account.handle, "me");
  assert.deepEqual(copy.records.paintings.map((p) => p.code), ["p1"]);
  assert.deepEqual(copy.records["chat-system"].map((m) => m.text), ["hello"]);
  assert.deepEqual(copy.records["walkieware-threads"].map(row => row.code), ["wgMine"]);
  assert.equal(copy.records["walkieware-threads"][0].ledger.versions[0].request, "red please");
  assert.deepEqual(copy.records["whistlegraph-roblox-rooms"], [{ _id: SUB, revision: 1, room: { name: "my room" } }]);
  assert.ok(!JSON.stringify(copy).includes(OTHER), "no one else's records");
  assert.equal(copy.files[0].key, `${SUB}/painting/p1.png`);
  assert.deepEqual(deps.calls, []);
});

test("billing exports include only the account's receipts and status, without replayable tokens", async () => {
  const seed = purchaseWorld();
  seed["whistlegraph-iap-purchases"][0].futureProviderCredential = "secret-future-field";
  seed["whistlegraph-iap-purchases"][0].state = { signedDate: +T0, refundedCredits: 250_000, type: "REFUND", futureProviderCredential: "secret-nested-field" };
  Object.assign(seed["whistlegraph-iap-notifications"][0], { signedDate: +T0, refundedCredits: 0 });
  const db = fakeDb(seed), deps = fakeDeps(db);
  const copy = await exportAccount(deps, { user, now: T0 });
  assert.deepEqual(copy.records["whistlegraph-iap-accounts"], [{ createdAt: T0 }]);
  assert.deepEqual(copy.records["ac-credit-wallets"], [{ balance: 5 }]);
  const [purchase] = copy.records["whistlegraph-iap-purchases"];
  assert.equal(purchase.transactionId, "1001");
  assert.equal(purchase.credits, 1_000_000);
  assert.deepEqual(purchase.state, { signedDate: +T0, refundedCredits: 250_000, type: "REFUND" });
  assert.deepEqual(copy.records["whistlegraph-iap-notifications"], [{
    _id: "Production:1001", type: "REFUND_REVERSED", purchaseId: purchase._id,
    signedDate: +T0, refundedCredits: 0, receivedAt: T0, status: "pending",
  }]);
  const serialized = JSON.stringify(copy);
  for (const secret of [OTHER, "private-jws-", "secret-future-field", "secret-nested-field", seed["whistlegraph-iap-accounts"][0].appAccountToken]) {
    assert.ok(!serialized.includes(secret), `${secret} should not be exported`);
  }
  assert.deepEqual(db.all("whistlegraph-iap-purchases"), seed["whistlegraph-iap-purchases"], "export does not mutate billing records");
});

test("the export is a signed-in download", async () => {
  const anonymous = await endpoint({ authorized: false });
  assert.equal((await anonymous.handler({ httpMethod: "GET", queryStringParameters: { export: "" }, headers: {} })).statusCode, 401);
  const e = await endpoint();
  const response = await e.handler({ httpMethod: "GET", queryStringParameters: { export: "" }, headers: {} });
  assert.equal(response.statusCode, 200);
  assert.match(response.headers["Content-Disposition"], /attachment/);
  assert.deepEqual(e.calls, ["export", "disconnect"]);
});


test("speech billing receipts export without audio digests and disappear with the account", async () => {
  const seed = world();
  seed["whistlegraph-speech-requests"] = [
    {_id:"speech-mine",user:SUB,braincells:320,charged:320,status:"complete",hash:"private-audio-digest",startedAt:T0},
    {_id:"speech-other",user:OTHER,braincells:40,status:"complete"},
  ];
  const db=fakeDb(seed),deps=fakeDeps(db);
  const copy=await exportAccount(deps,{user,now:T0});
  assert.deepEqual(copy.records["whistlegraph-speech-requests"],[{_id:"speech-mine",braincells:320,charged:320,status:"complete",startedAt:T0}]);
  assert.ok(!JSON.stringify(copy).includes("private-audio-digest"));
  await requestDeletion(deps,{user,now:T0});
  await runDueDeletions(deps,{now:later(GRACE_MS)});
  assert.deepEqual(db.all("whistlegraph-speech-requests"),[seed["whistlegraph-speech-requests"][1]]);
});

test("No Paint billing exports exclude image digests and deletion preserves other accounts", async () => {
  const seed = world();
  seed["nopaint-move-requests"] = [
    {_id:"move-mine",user:SUB,model:"test-model",braincells:4000,charged:4000,status:"complete",hash:"private-painting-digest",startedAt:T0},
    {_id:"move-other",user:OTHER,braincells:4000,status:"complete"},
  ];
  const db=fakeDb(seed),deps=fakeDeps(db);
  const copy=await exportAccount(deps,{user,now:T0});
  assert.deepEqual(copy.records["nopaint-move-requests"],[{_id:"move-mine",model:"test-model",braincells:4000,charged:4000,status:"complete",startedAt:T0}]);
  assert.ok(!JSON.stringify(copy).includes("private-painting-digest"));
  await requestDeletion(deps,{user,now:T0});
  await runDueDeletions(deps,{now:later(GRACE_MS)});
  assert.deepEqual(db.all("nopaint-move-requests"),[seed["nopaint-move-requests"][1]]);
});
