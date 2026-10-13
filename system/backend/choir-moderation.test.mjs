import test from "node:test";
import assert from "node:assert/strict";
import { makeScreen, screenPost, fileReport, resolveReport, acceptTerms, termsOf, TERMS_VERSION, REFUSED } from "./choir-moderation.mjs";
import { mongoChoirStore } from "./choir.mjs";

// The real slur list lives in lith's env, not in this repo; "rutabaga" and
// "turnip" stand in for it here.
const screen = makeScreen(["rutabaga", "turnip"]);

function collection() {
  const rows = [];
  const matches = (r, q) => Object.entries(q).every(([k, v]) => v && typeof v === "object" && !Array.isArray(v) ? true : r[k] === v);
  return { rows,
    createIndex: async () => {},
    insertOne: async (row) => { rows.push({ ...row }); },
    findOne: async (q) => rows.find((r) => matches(r, q)) || null,
    countDocuments: async (q) => rows.filter((r) => matches(r, q)).length,
    updateOne: async (q, u, o = {}) => {
      let r = rows.find((x) => matches(x, q));
      if (!r && o.upsert) { r = { ...q }; rows.push(r); }
      if (r) Object.assign(r, u.$set || {});
      return { modifiedCount: r ? 1 : 0 };
    } };
}

test("the filter stops listed words and self-harm taunts, stretched or disguised, and nothing else", () => {
  for (const bad of ["what a rutabaga", "RUTABAGAS everywhere", "ruuutabaaaga", "rut4b4ga", "turnip!", "just kill yourself", "kys", "go die"])
    assert.equal(screen(bad), false, bad);
  for (const fine of ["rutabagarden is a show title", "Turnipseed Gallery", "this fucking rules", "a killer opening", "keys", "Niger, Raccoon, Scunthorpe", ""])
    assert.equal(screen(fine), true, fine);
});

test("a post is screened across its words, transcripts and file names", () => {
  assert.doesNotThrow(() => screenPost({ text: "hello", attachments: [{ name: "cv.pdf" }] }, screen));
  assert.throws(() => screenPost({ text: "hi", attachments: [{ name: "voice.m4a", transcript: "you rutabaga" }] }, screen), { message: REFUSED });
  assert.throws(() => screenPost({ text: "", attachments: [{ name: "turnip.jpg" }] }, screen), /choir doesn't allow/);
});

test("the store refuses a screened post and a banned member, with a friendly message", async () => {
  const posts = collection(), prefs = collection();
  let n = 0;
  const s = mongoChoirStore(posts, { prefs, screen, id: () => String(++n).padStart(24, "0") });
  await assert.rejects(s.post("sub-a", "@a", { text: "rutabaga", tags: [] }), /another way/);
  await s.post("sub-a", "@a", { text: "hello", tags: [] });
  await prefs.insertOne({ _id: "sub-b", banned: true });
  await assert.rejects(s.post("sub-b", "@b", { text: "hello", tags: [] }), /can't post/);
  assert.equal(posts.rows.length, 1);
});

test("a report is filed once, snapshots the post, and the moderator can take it down and ban", async () => {
  const posts = collection(), reports = collection(), prefs = collection();
  const postID = "a".repeat(24);
  await posts.insertOne({ _id: postID, owner: "sub-troll", handle: "@troll", text: "something awful", when: "2026-10-12T00:00:00Z", replyTo: null });
  await assert.rejects(fileReport(reports, posts, { postID: "nope", member: "sub-fia" }), /Bad post/);
  await assert.rejects(fileReport(reports, posts, { postID, member: "sub-troll" }), /your own/);
  const first = await fileReport(reports, posts, { postID, reason: "harassment", member: "sub-fia", handle: "@fifi", id: () => "r1" });
  assert.equal(first.fresh, true);
  assert.equal(first.report.post.text, "something awful");
  const again = await fileReport(reports, posts, { postID, reason: "again", member: "sub-fia", id: () => "r2" });
  assert.equal(again.fresh, false, "one open report per reporter per post");
  assert.equal(reports.rows.length, 1);

  await assert.rejects(resolveReport(reports, posts, prefs, { reportID: "r1", status: "maybe" }), /status/);
  assert.equal(await resolveReport(reports, posts, prefs, { reportID: "zz", status: "dismissed" }), null);
  const done = await resolveReport(reports, posts, prefs, { reportID: "r1", status: "actioned", remove: true, ban: true, by: "@jeffrey" });
  assert.equal(done.status, "actioned");
  assert.equal(posts.rows[0].deleted, true);
  assert.equal(posts.rows[0].text, "");
  assert.equal(prefs.rows.find((r) => r._id === "sub-troll").banned, true);
});

test("terms are agreed per version", async () => {
  const prefs = collection();
  assert.deepEqual(await termsOf(prefs, "sub-a"), { termsVersion: null, current: false, banned: false });
  await assert.rejects(acceptTerms(prefs, "sub-a", { version: "1999-01-01" }), /current terms/);
  assert.deepEqual(await acceptTerms(prefs, "sub-a"), { termsVersion: TERMS_VERSION, current: true });
  assert.equal((await termsOf(prefs, "sub-a")).current, true);
});
