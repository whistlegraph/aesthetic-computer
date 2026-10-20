import test from "node:test";
import assert from "node:assert/strict";
import {registerPiece} from "../backend/piece-registry.mjs";

function collection() {
  const rows = [];
  return {rows,
    findOne: async (query) => rows.find((row) => query._id ? String(row._id) === String(query._id)
      : row.user === query.user && query.slug.$in.includes(row.slug)) || null,
    updateOne: async (filter, update) => {
      const row = rows.find((r) => String(r._id) === String(filter._id));
      if (row) Object.assign(row, update.$set); else rows.push({_id: filter._id, ...update.$setOnInsert, ...update.$set});
    }};
}

test("the first registration fixes code, slug and position; a save only moves the bytes", async () => {
  const pieces = collection();
  const first = await registerPiece(pieces, {user: "auth0|1", slug: "hf2", ext: "mjs", bytes: Buffer.from("a"), origin: "notepaint", makeCode: async () => "abc", now: new Date(1)});
  assert.equal(first.code, "abc");
  assert.match(first.discussion, /^https:\/\/mime\.ac\/#\/t\/piece_[a-f0-9]{24}$/);
  const row = pieces.rows[0];
  assert.deepEqual([row.slug, row.type, row.origin, row.name, row.size], ["piece/hf2", "piece", "notepaint", "hf2", 1]);
  const again = await registerPiece(pieces, {user: "auth0|1", slug: "hf2", ext: "mjs", bytes: Buffer.from("bb"), makeCode: async () => "zzz", now: new Date(2)});
  assert.equal(again.code, "abc");
  assert.equal(again.id, first.id);
  assert.equal(pieces.rows.length, 1);
  assert.equal(pieces.rows[0].size, 2);
  assert.equal(pieces.rows[0].when.getTime(), 1);
});
