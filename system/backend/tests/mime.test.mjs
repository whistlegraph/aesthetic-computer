import assert from "node:assert/strict";
import { after, before, test } from "node:test";
import { MongoClient, ObjectId } from "mongodb";
import { createHandler, handler as canonicalHandler } from "../../netlify/functions/mime.mjs";
import { handler as legacyHandler } from "../../netlify/functions/mimechan.mjs";
import { mediaFile } from "../mime-media.mjs";

// Run against a disposable local MongoDB. Never populate a production database.
const uri = process.env.MIME_TEST_MONGO_URI;
if (uri && !/^mongodb:\/\/(127\.0\.0\.1|localhost|\[::1\])[:/]/.test(uri)) {
  throw new Error("Mime tests require a loopback MongoDB");
}
let client, db, handler;
const paintingId = new ObjectId();
const paintingThread = `painting_${paintingId}`;
const when = new Date("2026-09-01T12:00:00Z");
const fixture = (code, extra = {}) => ({ code, slug: code, when, ...extra });
const file = (name, type = "image/png") => ({ name, type, data: Buffer.from("fixture").toString("base64") });
const get = (query = {}) => handler({ httpMethod: "GET", queryStringParameters: query });
const post = (body) => handler({ httpMethod: "POST", body: JSON.stringify(body) });
const body = (response) => JSON.parse(response.body);

before(async () => {
  if (!uri) return;
  client = await new MongoClient(uri).connect();
  db = client.db(`mime_test_${process.pid}_${Date.now()}`);
  handler = createHandler(async () => ({ db }));
  await db.collection("@handles").insertOne({ _id: "auth0|fixture", handle: "artist" });
  await db.collection("paintings").insertMany([
    fixture("art", { _id: paintingId, user: "auth0|fixture", slug: "auth0|fixture/painting/drawing" }),
    fixture("guest", { slug: "image:recording" }),
    fixture("nuked", { nuked: true }), fixture("private", { private: true }),
    fixture("draft", { draft: true }), fixture("unlisted", { visibility: "unlisted" }),
    fixture("deleted", { deleted: true }), fixture("hidden", { hidden: true }),
    { _id: new ObjectId(), slug: "legacy", when },
  ]);
  await db.collection("tapes").insertMany([
    fixture("zip"), fixture("movie", { kind: "mp4", mp4Status: "complete", mp4Url: "https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/movie.mp4" }),
  ]);
  await db.collection("pieces").insertMany([
    fixture("js", { source: "export function paint() {}", extension: ".mjs" }),
    fixture("lua", { ext: "lua" }),
  ]);
  await db.collection("kidlisp").insertOne(fixture("lisp", { source: "(wipe red)", user: "auth0|fixture" }));
  await db.collection("tape-drafts").insertOne(fixture("never-published"));
});

after(async () => { if (db) await db.dropDatabase(); if (client) await client.close(); });

test("media storage preserves owner prefixes and anonymous recording slugs", () => {
  assert.equal(legacyHandler, canonicalHandler);
  assert.equal(mediaFile("painting", { slug: "image:recording" }).url,
    "https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/image.png");
  assert.equal(mediaFile("painting", { user: "auth0|u", slug: "auth0|u/painting/x" }).url,
    "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0%7Cu/painting/x.png");
  assert.throws(() => mediaFile("tape", { mp4Status: "complete", mp4Url: "https://evil.example/file" }));
  assert.throws(() => mediaFile("painting", { slug: "../private" }));
});

test("public media automatically become attributed posts without writes", { skip: !uri }, async () => {
  const response = await get();
  assert.equal(response.statusCode, 200);
  const index = body(response);
  assert.equal(index.recent.length, 8);
  assert.equal(index.boards.find((b) => b.board === "image/png").threads, 3);
  assert.deepEqual(new Set(index.boards.map((b) => b.board)), new Set([
    "image/png", "video/mp4", "application/zip", "text/javascript", "text/x-lua", "text/x-lisp",
  ]));
  const painting = index.recent.find((p) => p.code === paintingThread);
  assert.equal(painting.name, "@artist");
  assert.equal(painting.media.url, "https://aesthetic.computer/#art");
  assert.equal(painting.file.size, null);
  assert.doesNotMatch(response.body, /auth0|never-published|nuked|private|unlisted|draft|hidden|deleted/);
  assert.equal(await db.collection("mimechan").countDocuments(), 0);
  assert.equal(await db.collection("mime-media-threads").countDocuments(), 0);
  const resolved = body(await get({ media: "painting", code: "art" }));
  assert.equal(resolved.op.code, paintingThread);
  assert.deepEqual(resolved.replies, []);
});

test("simultaneous first replies share one thread, with current source metadata", { skip: !uri }, async () => {
  const responses = await Promise.all(Array.from({ length: 8 }, (_, i) => post({ parent: paintingThread, text: `reply ${i}` })));
  for (const r of responses) assert.equal(r.statusCode, 200, r.body);
  assert.equal(await db.collection("mime-media-threads").countDocuments({ _id: paintingThread }), 1);
  assert.equal(await db.collection("mimechan").countDocuments({ parent: null }), 0);
  let thread = body(await get({ thread: paintingThread }));
  assert.equal(thread.op.replies, 8);
  assert.equal(thread.replies.length, 8);
  const inFeed = body(await get()).recent.find((p) => p.code === paintingThread);
  assert.equal(inFeed.replies, 8);
  assert.deepEqual(inFeed.preview, thread.replies.slice(0, 1).map(({ name, text }) => ({ name, text })));
  assert.ok(thread.replies.every((r) => r.parent === paintingThread && r.board === "image/png"));
  const board = body(await get({ board: "image/png" }));
  assert.equal(board.threads[0].op.code, paintingThread);
  assert.equal(board.threads[0].replyCount, 8);
  assert.equal(board.threads[0].replies.length, 3);
  await db.collection("@handles").updateOne({ _id: "auth0|fixture" }, { $set: { handle: "renamed" } });
  await db.collection("paintings").updateOne({ _id: paintingId }, { $set: { code: "renamed-art" } });
  thread = body(await get({ media: "painting", code: "renamed-art" }));
  assert.equal(thread.op.code, paintingThread);
  assert.equal(thread.op.name, "@renamed");
  assert.equal(thread.replies.length, 8);
});

test("comments reject attachments without creating a post or changing activity", { skip: !uri }, async () => {
  const native = body(await post({ file: file("root.txt", "text/plain") })).code;
  for (const parent of [paintingThread, native]) {
    const before = body(await get({ thread: parent }));
    for (const text of ["", "with text"]) {
      const response = await post({ parent, text, file: file("sound.wav", "audio/wav") });
      assert.equal(response.statusCode, 400);
      assert.equal(body(response).error, "comments are text only");
    }
    assert.deepEqual(body(await get({ thread: parent })), before);
    assert.equal((await post({ parent, text: "   " })).statusCode, 400);
    assert.equal((await post({ parent, text: "text comment", file: null })).statusCode, 200);
  }
});

test("hiding a source removes its listing, thread and reply attachments; restore retains replies", { skip: !uri }, async () => {
  // Existing attachments remain readable, but no new comment can upload one.
  await db.collection("mimechan").insertOne({
    code: "legacy_attachment", parent: paintingThread, board: "image/png", when,
    text: "old attachment", file: { ...file("old.png"), size: 7 },
  });
  const replies = body(await get({ thread: paintingThread })).replies;
  const attachment = replies.find((r) => r.file);
  await db.collection("paintings").updateOne({ _id: paintingId }, { $set: { nuked: true } });
  for (const query of [{ thread: paintingThread }, { media: "painting", code: "renamed-art" }, { file: paintingThread }, { file: attachment.code }]) {
    const response = await get(query);
    assert.equal(response.statusCode, 404);
    assert.equal(response.headers["Cache-Control"], "no-store");
  }
  assert.equal((await post({ parent: paintingThread, text: "hidden" })).statusCode, 404);
  assert.ok(!body(await get()).recent.some((p) => p.code === paintingThread));
  assert.ok(!body(await get({ board: "image/png" })).threads.some((p) => p.op.code === paintingThread));
  await db.collection("paintings").updateOne({ _id: paintingId }, { $set: { nuked: false } });
  assert.equal(body(await get({ thread: paintingThread })).replies.length, replies.length);
});

test("signed-in authors use verified current handles and never expose account IDs", { skip: !uri }, async () => {
  const authenticated = createHandler(async () => ({ db }), async ({ authorization }) =>
    authorization === "Bearer fixture" ? { sub: "auth0|fixture", email: "private@example.test" } : null);
  const headers = { Authorization: "Bearer fixture" };
  const me = await authenticated({ httpMethod: "GET", queryStringParameters: { me: "1" }, headers });
  assert.equal(body(me).handle, "@renamed");
  assert.equal(me.headers["Cache-Control"], "no-store");
  const result = await authenticated({ httpMethod: "POST", headers, body: JSON.stringify({
    parent: paintingThread, text: "signed in", name: "@someone-else", user: "forged-id",
  }) });
  assert.equal(result.statusCode, 200);
  const stored = await db.collection("mimechan").findOne({ code: body(result).code });
  assert.equal(stored.user, "auth0|fixture");
  assert.equal(stored.name, null);
  let thread = await get({ thread: paintingThread });
  assert.equal(body(thread).replies.find((r) => r.code === stored.code).name, "@renamed");
  assert.doesNotMatch(thread.body, /auth0\||private@example|forged-id|someone-else/);
  await db.collection("@handles").updateOne({ _id: "auth0|fixture" }, { $set: { handle: "new-handle" } });
  thread = await get({ thread: paintingThread });
  assert.equal(body(thread).replies.find((r) => r.code === stored.code).name, "@new-handle");
  const count = await db.collection("mimechan").countDocuments();
  const invalid = await authenticated({ httpMethod: "POST", headers: { authorization: "Bearer invalid" },
    body: JSON.stringify({ parent: paintingThread, text: "must not post as guest" }) });
  assert.equal(invalid.statusCode, 401);
  assert.equal(await db.collection("mimechan").countDocuments(), count);
});

test("media files use existing storage and serve program source without executing it", { skip: !uri }, async () => {
  const image = await get({ file: paintingThread });
  assert.equal(image.statusCode, 302);
  assert.equal(image.headers.Location, "https://user-aesthetic-computer.sfo3.digitaloceanspaces.com/auth0%7Cfixture/painting/drawing.png");
  assert.equal(image.headers["Cache-Control"], "no-store");
  const lisp = body(await get({ media: "kidlisp", code: "lisp" })).op;
  const source = await get({ file: lisp.code });
  assert.equal(source.body, "(wipe red)");
  assert.equal(source.headers["Content-Type"], "text/x-lisp; charset=utf-8");
  assert.equal(source.headers["Content-Security-Policy"], "sandbox");
});

test("existing uploads coexist with media and invalid inputs cannot select sources", { skip: !uri }, async () => {
  const upload = await post({ file: file("old.png"), text: "old thread" });
  assert.equal(upload.statusCode, 200);
  const code = body(upload).code;
  assert.equal(body(await get({ thread: code })).op.text, "old thread");
  assert.equal((await get({ file: code })).isBase64Encoded, true);
  assert.equal((await post({ parent: { $ne: null }, text: "invalid" })).statusCode, 400);
  assert.equal((await post({ name: {}, file: file("x") })).statusCode, 400);
  assert.equal((await post({ text: "no file" })).statusCode, 400);
  assert.equal((await get({ media: "users", code: "auth0|fixture" })).statusCode, 404);
  assert.equal((await get({ media: "__proto__", code: "art" })).statusCode, 404);
  assert.equal((await post({ parent: `painting_${new ObjectId()}`, text: "missing" })).statusCode, 404);
});

test("archive pagination is stable and new uploads appear without a sync job", { skip: !uri }, async () => {
  await db.collection("paintings").insertMany(Array.from({ length: 24 }, (_, i) => fixture(`page-${i}`)));
  const first = body(await get({ board: "image/png" }));
  const second = body(await get({ board: "image/png", page: "1" }));
  assert.equal(first.hasMore, true);
  assert.equal(first.threads.length, 12);
  assert.equal(second.threads.length, 12);
  assert.equal(new Set([...first.threads, ...second.threads].map((t) => t.op.code)).size, 24);
  const recent = body(await get());
  assert.equal(recent.hasMore, true);
  for (const kind of ["painting", "tape", "kidlisp", "piece"]) {
    assert.ok(recent.recent.some((p) => p.media?.kind === kind), kind + " stays visible during a painting burst");
  }
  const next = body(await get({ page: "1" }));
  assert.equal(new Set([...recent.recent, ...next.recent].map((p) => p.code)).size,
    recent.recent.length + next.recent.length);
});

test("conversion changes the tape renderer while preserving its discussion", { skip: !uri }, async () => {
  const before = body(await get({ media: "tape", code: "zip" })).op;
  await post({ parent: before.code, text: "before conversion" });
  await db.collection("tapes").updateOne({ code: "zip" }, { $set: {
    mp4Status: "complete", mp4Url: "https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/zip.mp4",
  } });
  const after = body(await get({ thread: before.code }));
  assert.equal(after.op.board, "video/mp4");
  assert.equal(after.replies[0].text, "before conversion");
  assert.equal((await get({ file: before.code })).headers.Location,
    "https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/zip.mp4");
});

test("reply counts continue after the bump limit, and deletion cannot leave a public orphan", { skip: !uri }, async () => {
  const source = fixture("bump-limit");
  const { insertedId } = await db.collection("paintings").insertOne(source);
  const code = `painting_${insertedId}`;
  await db.collection("mimechan").insertMany(Array.from({ length: 300 }, (_, i) => ({
    code: `fixture_${i}`, parent: code, board: "image/png", when, text: "earlier reply", file: null,
  })));
  await db.collection("mime-media-threads").insertOne({ _id: code, replies: 300, bumped: when });
  assert.equal((await post({ parent: code, text: "reply 301" })).statusCode, 200);
  const activity = await db.collection("mime-media-threads").findOne({ _id: code });
  assert.equal(activity.replies, 301);
  assert.equal(activity.bumped.toISOString(), when.toISOString());
  await db.collection("paintings").deleteOne({ _id: insertedId });
  assert.equal((await get({ thread: code })).statusCode, 404);
  assert.equal((await post({ parent: code, text: "orphan" })).statusCode, 404);
});

test("engagement metadata is cumulative, retry-safe, anonymous and public-media-only", { skip: !uri }, async () => {
  const visit = "b08c2ef9-746d-4d42-a263-e21b35a64743";
  const counters = { code: paintingThread, visibleMs: 2000, partialMs: 500,
    majorityMs: 1500, focusedMs: 1700, weightedVisibleMs: 1600,
    maxVisiblePermille: 950, commentOpens: 1, originalOpens: 0 };
  const send = (posts, extra = {}) => handler({ httpMethod: "POST", queryStringParameters: { engagement: "1" },
    body: JSON.stringify({ visit, posts, ...extra }) });
  const responses = await Promise.all(Array.from({ length: 4 }, () => send([counters])));
  assert.ok(responses.every((r) => r.statusCode === 200));
  const newer = { ...counters, visibleMs: 3000, majorityMs: 2500, focusedMs: 2700, weightedVisibleMs: 2400 };
  await send([newer]);
  await send([counters]);
  let stats = body(await get({ thread: paintingThread })).metadata.engagement;
  assert.equal(stats.visibleMs, 3000);
  assert.equal(stats.focusedMs, 2700);
  assert.equal(stats.commentOpens, 1);
  assert.equal(stats.impressions, 1);
  await send([counters], { visit: "b08c2ef9-746d-4d42-a263-e21b35a64744" });
  stats = body(await get({ thread: paintingThread })).metadata.engagement;
  assert.equal(stats.visibleMs, 5000);
  assert.equal(stats.impressions, 2);
  assert.doesNotMatch(JSON.stringify(stats), /visit|auth0|b08c/);
  assert.equal((await send([{ ...counters, visibleMs: -1 }])).statusCode, 400);
  assert.equal((await send([{ ...counters, focusedMs: 3000 }])).statusCode, 400);
  assert.equal((await send([{ ...counters, partialMs: 42 }])).statusCode, 400);
  assert.equal((await send(Array(25).fill(counters))).statusCode, 400);
  assert.equal((await send([counters], { visit: "account-id" })).statusCode, 400);
  const privateId = (await db.collection("paintings").insertOne(fixture("engagement-private", { private: true }))).insertedId;
  await send([{ ...counters, code: `painting_${privateId}` }, { ...counters, code: "missing-post" }]);
  assert.equal(await db.collection("mime-engagement").countDocuments(), 2);
  const stored = await db.collection("mime-engagement").findOne({ code: paintingThread });
  assert.deepEqual(Object.keys(stored).sort(), ["_id", ...Object.keys(counters)].sort());
});
