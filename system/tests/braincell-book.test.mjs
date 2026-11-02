// The Braincell book: entries built from an allowlist, staged with the
// balance they explain, filed once however often they are retried, and an
// opening that makes an old wallet's book sum to its balance.
import test from "node:test";
import assert from "node:assert/strict";
import { bookEntry, entryId, staged, bookRows, drainOutbox, walletDrift, openingEntries, recoverIssue, grantSource, stripeSessionTimes } from "../backend/braincell-book.mjs";
import { fulfillGrant, reserve, settle, settleDurably, refundCheckout, revokeGrant, CREDIT_PACK } from "../backend/easel-paid-credits.mjs";
import { foldBraincells } from "../../toolchain/analytics/braincells-fold.mjs";

// Just enough Mongo for the wallet's updates and the drain.
const get = (doc, path) => path.split(".").reduce((v, k) => v?.[k], doc);
function put(doc, path, value) {
  const keys = path.split("."), last = keys.pop();
  let at = doc; for (const k of keys) at = at[k] ??= {};
  if (value === undefined) delete at[last]; else at[last] = value;
}
function expr(value, doc, vars = {}) {
  if (typeof value === "string" && value.startsWith("$$")) return get(vars, value.slice(2));
  if (typeof value === "string" && value.startsWith("$")) return get(doc, value.slice(1));
  if (Array.isArray(value)) return value.map(v => expr(v, doc, vars));
  if (!value || typeof value !== "object" || !Object.keys(value).some(k => k.startsWith("$"))) return value;
  const [[op, args]] = Object.entries(value);
  if (op === "$map") return expr(args.input, doc, vars).map(v => expr(args.in, doc, { ...vars, [args.as]: v }));
  const a = expr(args, doc, vars);
  if (op === "$ifNull") return a[0] ?? a[1];
  if (op === "$objectToArray") return Object.entries(a).map(([k, v]) => ({ k, v }));
  if (op === "$sum" || op === "$add") return a.reduce((n, v) => n + v, 0);
  if (op === "$lte") return a[0] <= a[1];
  throw Error("Unsupported mock operator " + op);
}
function matches(doc, filter) {
  return Object.entries(filter).every(([path, want]) => {
    if (path === "$expr") return expr(want, doc);
    if (path === "$or") return want.some(f => matches(doc, f));
    const have = get(doc, path);
    if (want && typeof want === "object" && !Array.isArray(want)) {
      if ("$gte" in want) return have >= want.$gte;
      if ("$exists" in want) return (have !== undefined) === want.$exists;
      if ("$ne" in want) return Array.isArray(have) ? !have.includes(want.$ne) : have !== want.$ne;
      if ("$size" in want) return Array.isArray(have) && have.length === want.$size;
    }
    return have === want;
  });
}
function collection(docs = []) {
  return {
    docs,
    async createIndex() {},
    async findOne(filter) { const d = docs.find(d => matches(d, filter)); return d ? structuredClone(d) : null; },
    find(filter) {
      let rows = docs.filter(d => matches(d, filter)).map(d => structuredClone(d));
      const cursor = { limit(n) { rows = rows.slice(0, n); return cursor; }, async toArray() { return rows; } };
      return cursor;
    },
    async insertOne(doc) {
      if (docs.some(d => d._id === doc._id)) throw Object.assign(Error("duplicate"), { code: 11000 });
      docs.push(structuredClone(doc));
    },
    async updateOne(filter, update, options = {}) {
      let doc = docs.find(d => matches(d, filter));
      if (!doc && options.upsert) { doc = { _id: filter._id, ...structuredClone(update.$setOnInsert) }; docs.push(doc); return { modifiedCount: 1 }; }
      if (!doc) return { modifiedCount: 0 };
      if (update.$setOnInsert && Object.keys(update).length === 1) return { modifiedCount: 0 };
      for (const [path, n] of Object.entries(update.$inc || {})) put(doc, path, (get(doc, path) || 0) + n);
      for (const [path, v] of Object.entries(update.$set || {})) put(doc, path, structuredClone(v));
      for (const path of Object.keys(update.$unset || {})) put(doc, path, undefined);
      for (const [path, v] of Object.entries(update.$addToSet || {})) { const list = get(doc, path) || []; if (!list.includes(v)) list.push(v); put(doc, path, list); }
      for (const [path, v] of Object.entries(update.$push || {})) put(doc, path, [...(get(doc, path) || []), ...structuredClone(v.$each || [v])]);
      for (const [path, v] of Object.entries(update.$pull || {})) put(doc, path, (get(doc, path) || []).filter(e => !v._id.$in.includes(e._id)));
      return { modifiedCount: 1 };
    },
  };
}
const book = (wallets, entries) => walletDrift(wallets.docs[0], entries.docs.filter(e => e.user === wallets.docs[0]._id));
const now = new Date("2026-11-02T12:00:00Z");

test("an entry keeps only what the book needs, and its id is its wallet and key", () => {
  const e = bookEntry({ user: "u", key: "spend:h1", kind: "spend", source: "inference", amount: -40, holdId: "h1", model: "m",
    product: "aesel", at: now, prompt: "secret", tokens: 99, ref: { issuer: "@jeffrey", password: "no" } });
  assert.deepEqual(e, { _id: entryId("u", "spend:h1"), key: "spend:h1", kind: "spend", source: "inference", amount: -40, at: now,
    product: "aesel", model: "m", holdId: "h1", ref: { issuer: "@jeffrey" } });
  assert.equal(entryId("u", "issue:cs_1"), entryId("u", "issue:cs_1"));
  assert.notEqual(entryId("u", "issue:cs_1"), entryId("v", "issue:cs_1"));
});

test("an entry's sign must match its kind", () => {
  const base = { user: "u", key: "k", source: "stripe", at: now };
  assert.throws(() => bookEntry({ ...base, kind: "issue", amount: -1 }));
  assert.throws(() => bookEntry({ ...base, kind: "refund", amount: 1 }));
  assert.throws(() => bookEntry({ ...base, kind: "issue", amount: 0 }));
  assert.throws(() => bookEntry({ ...base, kind: "issue", amount: 1.5 }));
  assert.throws(() => bookEntry({ ...base, kind: "gift", amount: 1 }));
  assert.throws(() => bookEntry({ ...base, kind: "issue", source: "paypal", amount: 1 }));
  assert.equal(bookEntry({ ...base, kind: "adjust", source: "backfill", amount: -5 }).amount, -5);
  const unknown = bookEntry({ ...base, kind: "issue", source: "staff", amount: 0, reconstructed: true, amountKnown: false });
  assert.equal(unknown.amountKnown, false);
});

test("staging folds into an update without losing its own operators", () => {
  const e = bookEntry({ user: "u", key: "issue:x", kind: "issue", source: "staff", amount: 5, at: now });
  assert.deepEqual(staged({ $inc: { balance: 5 }, $set: { updatedAt: now } }, [e], now),
    { $inc: { balance: 5, ledgerSeq: 1 }, $set: { updatedAt: now, outboxAt: now }, $push: { outbox: { $each: [e] } } });
  assert.deepEqual(staged({ $set: { a: 1 } }, []), { $set: { a: 1 } });
  // The outbox is the newest run of entries: its last one is ledgerSeq.
  assert.deepEqual(bookRows({ _id: "u", ledgerSeq: 7, outbox: [{ _id: "a" }, { _id: "b" }] }).map(r => [r.user, r.seq]), [["u", 6], ["u", 7]]);
});

test("every live balance change books once, however often it is retried", async () => {
  const wallets = collection(), entries = collection();
  const user = "u", session = { id: "cs_test_book", mode: "payment", payment_status: "paid", livemode: false, amount_total: 500, currency: "usd",
    client_reference_id: user, metadata: { type: "ac-credits", pack: CREDIT_PACK.id, userSub: user } };
  const grant = { user, id: "cs_test_book", credits: 1_000_000 };
  assert.equal(await fulfillGrant(grant, wallets, { now }), true);
  assert.equal(await fulfillGrant(grant, wallets, { now }), false);
  await fulfillGrant({ user, id: "staff-1", credits: 500, source: "staff", ref: { issuer: "@jeffrey", reason: "thanks" } }, wallets, { now });

  const hold = await reserve(user, 1000, wallets, { now, product: "aesel", model: "deepseek/deepseek-v4.1-flash" });
  assert.equal(await settle(hold, 250, wallets, { now }), true);
  assert.equal(await settle(hold, 250, wallets, { now }), false);
  const durable = await reserve(user, 100, wallets, { now, product: "choir", model: "m" });
  await Promise.all([settleDurably(durable, 0, wallets, { now }), settleDurably(durable, 0, wallets, { now })]);

  await refundCheckout(session, 250, wallets, { live: false });
  await refundCheckout(session, 250, wallets, { live: false });
  await revokeGrant({ user, id: "apple:42", credits: 300 }, wallets, { now });
  await revokeGrant({ user, id: "apple:42", credits: 300 }, wallets, { now });

  assert.equal(wallets.docs[0].balance, 1_000_000 + 500 - 250 - 500_000);
  assert.equal(await drainOutbox(wallets, entries), 10);
  assert.equal(await drainOutbox(wallets, entries), 0, "a second drain files nothing");
  assert.equal(wallets.docs[0].outbox, undefined);
  assert.equal(wallets.docs[0].outboxAt, undefined);

  const rows = entries.docs;
  assert.deepEqual(rows.map(r => `${r.kind}:${r.source}:${r.amount}`), [
    "issue:stripe:1000000", "issue:staff:500", "reserve:inference:-1000", "release:inference:1000", "spend:inference:-250",
    "reserve:inference:-100", "release:inference:100", "refund:stripe:-500000", "issue:apple:300", "revoke:apple:-300"]);
  assert.deepEqual(rows.map(r => r.seq), rows.map((_, i) => i + 1), "seq is the order the wallet changed in");
  assert.ok(rows.every(r => r.user === user));
  assert.deepEqual(rows[0].ref, { stripeSession: "cs_test_book" });
  assert.deepEqual(rows[1].ref, { issuer: "@jeffrey", reason: "thanks" });
  const spend = rows.find(r => r.kind === "spend");
  assert.equal(spend.model, "deepseek/deepseek-v4.1-flash");
  assert.equal(spend.product, "aesel");
  assert.equal(rows.filter(r => r.kind === "spend").length, 1, "a free settle books no spend");
  const refund = rows.find(r => r.kind === "refund");
  assert.equal(refund.balanceAfter, 1_000_000 + 500 - 250 - 500_000, "a refund knows the balance it left");
  assert.deepEqual(book(wallets, entries), { user, balance: wallets.docs[0].balance, book: wallets.docs[0].balance, drift: 0, entries: 10, staged: 0, opened: false });
});

test("a staged entry still counts before it is filed, and only once", async () => {
  const wallets = collection(), entries = collection();
  await fulfillGrant({ user: "u", id: "cs_a", credits: 10 }, wallets, { now });
  assert.equal(book(wallets, entries).drift, 0);
  // Copied but not yet pulled: a crash between the drain's two writes.
  for (const row of bookRows(wallets.docs[0])) await entries.insertOne(row);
  assert.equal(book(wallets, entries).book, 10);
  assert.equal(await drainOutbox(wallets, entries), 0);
  assert.equal(entries.docs.length, 1);
});

test("a balance changed behind the book shows as drift", async () => {
  const wallets = collection(), entries = collection();
  await fulfillGrant({ user: "u", id: "cs_a", credits: 10 }, wallets, { now });
  await drainOutbox(wallets, entries);
  wallets.docs[0].balance += 7;
  assert.equal(book(wallets, entries).drift, 7);
});

test("grant ids say their source and what they cost", () => {
  assert.equal(grantSource("cs_live_1"), "stripe");
  assert.equal(grantSource("apple:whistlegraph:Production:9"), "apple");
  assert.equal(grantSource("tezos:mainnet:42"), "tezos");
  assert.equal(grantSource("staff-2026-09-28-ab"), "staff");
  const lookups = { packCredits: 1_000_000, purchases: { "apple:7": { credits: 3_000_000, environment: "Production" } },
    tezos: { "tezos:mainnet:42": { credits: 600_000, operationHash: "oo1", at: now } } };
  assert.equal(recoverIssue("cs_live_1", lookups).amount, 1_000_000);
  assert.deepEqual(recoverIssue("apple:7", lookups).ref, { appleTransaction: "7", appleEnvironment: "Production" });
  assert.equal(recoverIssue("apple:8", lookups).amount, null, "an unrecorded Apple quantity is not guessed");
  assert.equal(recoverIssue("apple:whistlegraph:Sandbox:9", lookups).amount, 1_000_000);
  assert.deepEqual(recoverIssue("tezos:mainnet:42", lookups).ref, { tezosPayment: "tezos:mainnet:42", tezosOperation: "oo1" });
  assert.equal(recoverIssue("staff-x", lookups).amount, null);
});

test("the opening reconstructs history, sums to the balance, and never doubles a live entry", async () => {
  const opened = new Date("2026-09-01T00:00:00Z");
  const wallet = { _id: "u", createdAt: opened, balance: 2_123_400, spent: 380_000,
    grants: ["cs_live_1", "tezos:mainnet:42", "staff-x", "cs_live_2"],
    refunds: { cs_live_1: 500_000 }, holds: { open: { amount: 5000, at: now, product: "aesel", model: "m" } } };
  const wallets = collection([wallet]), entries = collection();
  // After the book shipped: cs_live_2 was bought, and a hold reserved before
  // the book settled under it (a release with no reserve to pair with).
  await wallets.updateOne({ _id: "u" }, staged({}, [
    bookEntry({ user: "u", key: "issue:cs_live_2", kind: "issue", source: "stripe", amount: 1_000_000, at: now }),
    bookEntry({ user: "u", key: "release:old", kind: "release", source: "inference", amount: 2000, holdId: "old", at: now }),
    bookEntry({ user: "u", key: "spend:old", kind: "spend", source: "inference", amount: -1500, holdId: "old", at: now }),
  ], now));
  await drainOutbox(wallets, entries);
  const lookups = { packCredits: 1_000_000, tezos: { "tezos:mainnet:42": { credits: 600_000, operationHash: "oo1", at: opened } }, now };
  const opening = openingEntries(wallets.docs[0], entries.docs, lookups);
  const kinds = opening.map(e => `${e.kind}:${e.source}:${e.amount}`);
  assert.deepEqual(kinds, ["issue:stripe:1000000", "issue:tezos:600000", "issue:staff:0", "refund:stripe:-500000",
    "reserve:inference:-5000", "reserve:inference:-2000", "spend:inference:-378500", "adjust:backfill:408400"]);
  assert.ok(opening.every(e => e.reconstructed));
  assert.equal(opening.filter(e => e.grantId === "cs_live_2").length, 0, "cs_live_2 was booked live");
  assert.equal(opening.find(e => e.grantId === "staff-x").amountKnown, false);
  assert.match(opening.at(-1).note, /1 grant of unknown size/);
  assert.equal(opening.at(-1).balanceAfter, 2_123_400);

  await wallets.updateOne({ _id: "u" }, staged({ $set: { ledgerOpenedAt: now } }, opening, now));
  await drainOutbox(wallets, entries);
  assert.equal(book(wallets, entries).drift, 0);
  assert.deepEqual(openingEntries(wallets.docs[0], entries.docs, lookups), [], "a second opening finds nothing left");
});

test("Stripe session times are fetched one at a time, and a failed lookup falls back", async () => {
  const waits = [];
  const sessions = { cs_live_a: { id: "cs_live_a", created: 1_760_000_000 }, cs_live_odd: { id: "cs_other", created: 1 } };
  const { times, failed } = await stripeSessionTimes(["cs_live_a", "cs_live_gone", "cs_live_odd"],
    async id => { if (!sessions[id]) throw Error("No such checkout.session"); return sessions[id]; },
    { gapMs: 250, sleep: async ms => { waits.push(ms); } });
  assert.deepEqual(times, { cs_live_a: { at: new Date(1_760_000_000_000) } });
  assert.deepEqual(failed, ["cs_live_gone", "cs_live_odd"]);
  assert.deepEqual(waits, [250, 250], "one pause between each lookup");

  const opened = new Date("2026-09-01T00:00:00Z");
  const wallet = { _id: "u", createdAt: opened, balance: 2_000_000, grants: ["cs_live_a", "cs_live_gone"] };
  const opening = openingEntries(wallet, [], { packCredits: 1_000_000, stripe: times, now });
  const [timed, untimed] = opening;
  assert.deepEqual([timed.at, timed.atKnown, timed.note], [new Date(1_760_000_000_000), true, undefined]);
  assert.deepEqual([untimed.at, untimed.atKnown], [opened, false]);
  assert.equal(opening.length, 2, "both packs explain the balance; no adjust");
});

test("an Apple reversal booked live is not refunded again by the opening", () => {
  const user = "u", id = "apple:whistlegraph:Production:1";
  const at = now, about = { user, source: "apple", grantId: id, at };
  const rows = [
    bookEntry({ ...about, key: `issue:${id}`, kind: "issue", amount: 1_000_000 }),
    bookEntry({ ...about, key: `apple-refund:${id}:2000:1000000`, kind: "revoke", amount: -1_000_000 }),
    bookEntry({ ...about, key: `apple-refund:${id}:2001:0`, kind: "adjust", amount: 1_000_000 }),
  ].map((e, i) => ({ ...e, user, seq: i + 1 }));
  const wallet = { _id: user, balance: 1_000_000, grants: [id], refunds: { [id]: 0 } };
  assert.deepEqual(openingEntries(wallet, rows, { packCredits: 1_000_000, now }), []);
});

test("the network report folds issuance by source from the book", () => {
  const report = foldBraincells({ book: [
    { kind: "issue", source: "stripe", amount: 1_000_000 }, { kind: "issue", source: "stripe", amount: 1_000_000 },
    { kind: "refund", source: "stripe", amount: -500_000 }, { kind: "issue", source: "staff", amount: 200_000 },
    { kind: "issue", source: "tezos", amount: 600_000, reconstructed: true }, { kind: "adjust", source: "backfill", amount: -5 },
  ] }, { start: new Date("2026-11-01T00:00:00Z"), end: new Date("2026-11-02T00:00:00Z") });
  assert.deepEqual(Object.keys(report.issuance.bySource), ["stripe", "staff"]);
  assert.deepEqual(report.issuance.bySource.stripe, { grants: 2, issued: { braincells: 2_000_000, usd: 10 }, takenBack: { braincells: 500_000, usd: 2.5 }, net: { braincells: 1_500_000, usd: 7.5 } });
  assert.equal(report.issuance.adjusted.braincells, -5);
});
