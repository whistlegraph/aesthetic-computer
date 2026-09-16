// Run with: node --test system/backend/tests/kidlisp-projection.test.mjs
//
// No MongoDB required: the projection takes a collection, so a recording fake
// is enough to pin the two properties that actually matter — the instant is a
// Date, and an existing row is never rewritten.

import assert from "node:assert/strict";
import { test } from "node:test";
import {
  projectionRow,
  projectKidlispPiece,
  mirrorKidlispPiece,
} from "../kidlisp-projection.mjs";

// What the sidecar hands back: ISO string instant, mutable state attached.
const entity = {
  code: "o4h",
  source: "(wipe black)\n(ink red)\n(line)",
  hash: "abc123",
  when: "2026-09-15T00:44:06Z",
  user: "auth0|fixture",
  hits: 42,
  keeps: [{ tokenId: 7, contractAddress: "KT1x" }],
  pendingRebake: { contractAddress: "KT1x" },
};

function fakeCollection(behavior = {}) {
  const calls = [];
  return {
    calls,
    async updateOne(filter, update, options) {
      calls.push({ filter, update, options });
      if (behavior.throws) throw behavior.throws;
      return { upsertedCount: behavior.upsertedCount ?? 1 };
    },
  };
}

test("coerces the sidecar's ISO instant to a Date", () => {
  const row = projectionRow(entity);
  // A string `when` sorts below every Date in BSON, which would bury the
  // newest pieces at the bottom of a {when: -1} feed.
  assert.ok(row.when instanceof Date);
  assert.equal(row.when.toISOString(), "2026-09-15T00:44:06.000Z");
});

test("passes a Date through unchanged", () => {
  const when = new Date("2026-09-15T00:44:06Z");
  assert.equal(projectionRow({ ...entity, when }).when.getTime(), when.getTime());
});

test("falls back to now rather than an Invalid Date", () => {
  const row = projectionRow({ ...entity, when: "not-an-instant" });
  assert.ok(row.when instanceof Date);
  assert.ok(!Number.isNaN(row.when.getTime()));
});

test("defaults a missing instant instead of dropping the piece", () => {
  const row = projectionRow({ ...entity, when: undefined });
  assert.ok(row.when instanceof Date);
  assert.ok(!Number.isNaN(row.when.getTime()));
});

test("projects identity only — no mutable state reaches Mongo", () => {
  const row = projectionRow(entity);
  assert.deepEqual(Object.keys(row).sort(), ["code", "hash", "source", "user", "when"]);
  for (const mutable of ["hits", "keeps", "kept", "tezos", "pendingRebake"]) {
    assert.ok(!(mutable in row), `${mutable} must stay Datomic's`);
  }
});

test("trims source, and normalizes an absent user to null", () => {
  const row = projectionRow({ ...entity, source: "  (wipe red)  ", user: undefined });
  assert.equal(row.source, "(wipe red)");
  assert.equal(row.user, null);
});

test("refuses a piece with no code or no source", async () => {
  for (const bad of [
    { ...entity, code: null },
    { ...entity, source: "" },
    { ...entity, source: "   " },
    { ...entity, source: undefined },
  ]) {
    assert.equal(projectionRow(bad), null);
    const collection = fakeCollection();
    const result = await projectKidlispPiece(collection, bad);
    assert.deepEqual(result, { projected: false, reason: "incomplete" });
    assert.equal(collection.calls.length, 0, "must not touch Mongo");
  }
});

test("upserts on code with $setOnInsert, so an existing row is untouched", async () => {
  const collection = fakeCollection();
  const result = await projectKidlispPiece(collection, entity);
  assert.deepEqual(result, { projected: true, inserted: true });

  const [call] = collection.calls;
  assert.deepEqual(call.filter, { code: "o4h" });
  assert.deepEqual(call.options, { upsert: true });
  // $set would flatten hits/kept/tezos on the pre-cutover rows this reruns over.
  assert.deepEqual(Object.keys(call.update), ["$setOnInsert"]);
});

test("reports an unchanged row rather than claiming an insert", async () => {
  const collection = fakeCollection({ upsertedCount: 0 });
  assert.deepEqual(await projectKidlispPiece(collection, entity), {
    projected: true,
    inserted: false,
  });
});

test("treats a hash collision as already-reachable, not an error", async () => {
  const duplicate = Object.assign(new Error("E11000 duplicate key"), { code: 11000 });
  const collection = fakeCollection({ throws: duplicate });
  assert.deepEqual(await projectKidlispPiece(collection, entity), {
    projected: false,
    reason: "duplicate",
  });
});

test("surfaces a real write failure to the backfill", async () => {
  const collection = fakeCollection({ throws: new Error("connection reset") });
  await assert.rejects(() => projectKidlispPiece(collection, entity), /connection reset/);
});

test("the mirror swallows failures so a write path cannot break", async () => {
  const database = {
    db: { collection: () => fakeCollection({ throws: new Error("connection reset") }) },
  };
  await mirrorKidlispPiece(database, entity); // resolves, does not reject
});

test("the mirror survives a database that cannot even hand out a collection", async () => {
  const database = { db: { collection: () => { throw new Error("no connection"); } } };
  await mirrorKidlispPiece(database, entity);
});
