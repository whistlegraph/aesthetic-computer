import test from "node:test";
import assert from "node:assert/strict";
import { fileReport, resolveReport, removals, allowed } from "./mime-moderation.mjs";

// Just enough of a Mongo collection for these functions.
function collection(rows = []) {
  const match = (row, q) => Object.entries(q).every(([k, v]) => row[k] === v);
  return {
    rows,
    findOne: async (q) => rows.find((r) => match(r, q)) || null,
    insertOne: async (doc) => { rows.push(doc); },
    updateOne: async (q, { $set }, { upsert } = {}) => {
      const row = rows.find((r) => match(r, q));
      if (row) Object.assign(row, $set); else if (upsert) rows.push({ ...q, ...$set });
    },
    find: () => ({ toArray: async () => rows }),
  };
}

const post = { code: "painting_1", name: "@ana", text: "hello", when: "2026-10-13", media: { kind: "painting" } };

test("one open report per reporter per card, with the card copied in", async () => {
  const reports = collection();
  const a = await fileReport(reports, { code: "painting_1", reason: "Spam or a scam", reporter: "device:abc12345", post, id: () => "r1" });
  const b = await fileReport(reports, { code: "painting_1", reason: "Something else", reporter: "device:abc12345", post, id: () => "r2" });
  assert.equal(a.fresh, true);
  assert.equal(b.fresh, false);
  assert.equal(reports.rows.length, 1);
  assert.equal(reports.rows[0].post.name, "@ana");
  await assert.rejects(fileReport(reports, { code: "gone", reporter: "u", post: null }), /gone/);
});

test("removing a card and banning its author keep both out of the feed", async () => {
  const reports = collection(), removed = collection();
  await fileReport(reports, { code: "painting_1", reason: "Harassment or hate", reporter: "u1", post, id: () => "r1" });
  const done = await resolveReport(reports, removed, { reportID: "r1", status: "actioned", remove: true, ban: true, by: "admin" });
  assert.equal(done.status, "actioned");
  const keep = allowed(await removals(removed));
  assert.equal(keep({ code: "painting_1", author: "@bo" }), false);
  assert.equal(keep({ code: "painting_2", author: "@ana" }), false);
  assert.equal(keep({ code: "painting_2", author: "@bo" }), true);
});

test("a dismissed report removes nothing", async () => {
  const reports = collection(), removed = collection();
  await fileReport(reports, { code: "painting_1", reason: "hidden", reporter: "u1", post, id: () => "r1" });
  await resolveReport(reports, removed, { reportID: "r1", status: "dismissed" });
  assert.equal(removed.rows.length, 0);
  await assert.rejects(resolveReport(reports, removed, { reportID: "r1", status: "maybe" }), /status/);
});
