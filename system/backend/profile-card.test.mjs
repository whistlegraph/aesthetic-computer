import test from "node:test";
import assert from "node:assert/strict";
import { createHandler } from "../netlify/functions/profile-card.mjs";

function fixture({ missing = false, fail = false } = {}) {
  const pipelines = [], matches = [];
  let disconnected = 0;
  const db = { collection(name) {
    if (name === "@handles") return {
      async findOne(query, options) {
        assert.ok(query.handle === "jeffrey" || query.handle.$regex === "^jeffrey$");
        assert.deepEqual(options.projection, { handle: 1, colors: 1 });
        return missing ? null : { _id: "private-user-id", handle: "jeffrey" };
      },
      find() { return { async toArray() { return [{ _id: "private-user-id", handle: "jeffrey" }]; } }; },
    };
    if (name === "moods") return { async countDocuments(query) { matches.push(query); return 288; }, find(query) {
      assert.equal(query.user, "private-user-id");
      assert.deepEqual(query.deleted, { $ne: true });
      return { sort() { return this; }, limit() { return this; }, async toArray() {
        return [{ mood: "hello\\nworld", when: "2026-10-12", secret: "never return" }];
      } };
    } };
    return { async countDocuments(query) { matches.push(query); return name === "paintings" ? 410 : 22; }, aggregate(pipeline) {
      pipelines.push({ name, pipeline });
      return { async toArray() {
        if (fail) throw new Error("database unavailable");
        if (name !== "paintings") return [];
        return Array.from({ length: 1 }, (_, i) => ({ code: "painting_" + i, board: "image/png", when: "2026-10-12",
          _media: { kind: "painting", id: String(i), code: "f" + i, user: "private-user-id" } }));
      } };
    } };
  } };
  return { pipelines, matches, get disconnected() { return disconnected; },
    handler: createHandler(async () => ({ db, disconnect() { disconnected++; } })) };
}
const get = (handler, query = {}) => handler({ httpMethod: "GET", queryStringParameters: { handle: "@Jeffrey", ...query } });

test("bounded public profile uses one identity and never exposes private fields", async () => {
  const f = fixture(), response = await get(f.handler), data = JSON.parse(response.body);
  assert.equal(response.statusCode, 200);
  assert.equal(data.handle, "@jeffrey");
  assert.equal(data.counts.painting, 410);
  assert.equal(data.counts.mood, 288);
  assert.equal(data.latestPainting.media.code, "f0");
  assert.equal(data.onlineTimeMs, null);
  assert.equal(data.posts, undefined);
  assert.equal(f.pipelines.length, 1);
  assert.equal(f.matches.length, 7);
  assert.equal(data.mood.text, "hello\nworld");
  assert.ok(!response.body.includes("private-user-id") && !response.body.includes("never return"));
  assert.equal(f.disconnected, 1);
  for (const { pipeline } of f.pipelines) {
    assert.equal(pipeline[0].$match.user, "private-user-id");
    for (const field of ["nuked", "deleted", "private", "draft", "hidden"]) assert.deepEqual(pipeline[0].$match[field], { $ne: true });
    assert.deepEqual(pipeline[0].$match.visibility, { $in: [null, "public"] });
    assert.deepEqual(pipeline[2], { $limit: 1 });
    assert.ok(!pipeline.some((stage) => stage.$lookup));
  }
});

test("membership fetch resolves the same identity without scanning media", async () => {
  const handler = createHandler(async () => ({ db: { collection(name) {
    assert.equal(name, "@handles");
    return { async findOne() { return { _id: "private-user-id", handle: "jeffrey" }; } };
  } } }), async user => {
    assert.equal(user, "private-user-id");
    return { registeredAt: "2023-02-17", loginCount: 814 };
  });
  const result = await get(handler, { membership: "1" });
  assert.equal(result.statusCode, 200);
  assert.deepEqual(JSON.parse(result.body), { registeredAt: "2023-02-17", loginCount: 814 });
});

test("invalid requests are rejected before opening a database connection", async () => {
  const handler = createHandler(() => { throw new Error("must not connect"); });
  for (const query of [{ membership: "2" }, { handle: "x.*" }, { handle: "mail@example.com" }, { kind: "secrets" }, { page: "1" },
    { kind: "painting", page: "-1" }, { kind: "painting", page: "1.5" }, { kind: "painting", page: "501" }]) {
    assert.equal((await get(handler, query)).statusCode, 400);
  }
  assert.equal((await handler({ httpMethod: "POST" })).statusCode, 405);
});

test("missing profiles and failed queries release the connection", async () => {
  const missing = fixture({ missing: true });
  assert.equal((await get(missing.handler)).statusCode, 404);
  assert.equal(missing.pipelines.length, 0);
  assert.equal(missing.disconnected, 1);
  const failed = fixture({ fail: true });
  assert.equal((await get(failed.handler)).statusCode, 503);
  assert.equal(failed.disconnected, 1);
});

test("handle lookup prefers exact spelling and escapes dots in the legacy fallback", async () => {
  const calls = [];
  const handler = createHandler(async () => ({ db: { collection(name) {
    assert.equal(name, "@handles");
    return { async findOne(query) { calls.push(query); return null; } };
  } } }));
  assert.equal((await get(handler, { handle: "@a.b" })).statusCode, 404);
  assert.deepEqual(calls, [{ handle: "a.b" }, { handle: { $regex: "^a\\.b$", $options: "i" } }]);
});

test("the MIMEs listing unions uploads with every media kind for one user, newest first, one page at a time", async () => {
  const pipelines = [], finds = [];
  const docs = Array.from({ length: 13 }, (_, i) => i < 2
    ? { code: "up" + i, parent: null, board: "image/png", when: "2026-10-1" + i, user: "private-user-id", file: { name: "a.png", type: "image/png", size: 1 }, replies: 3 }
    : { code: "painting_" + i, board: "image/png", when: "2026-10-0" + i, _media: { kind: "painting", id: String(i), code: "f" + i, user: "private-user-id" } });
  const db = { collection(name) {
    if (name === "@handles") return {
      async findOne() { return { _id: "private-user-id", handle: "jeffrey" }; },
      find() { return { async toArray() { return [{ _id: "private-user-id", handle: "jeffrey" }]; } }; },
    };
    if (name === "mime-media-threads") return { find(query, options) { finds.push({ query, options }); return { async toArray() {
      return [{ _id: "painting_2", replies: 5 }];
    } }; } };
    return { aggregate(pipeline) { pipelines.push({ name, pipeline }); return { async toArray() { return docs; } }; } };
  } };
  const handler = createHandler(async () => ({ db }));
  const response = await get(handler, { kind: "mime", page: "0" }), data = JSON.parse(response.body);
  assert.equal(response.statusCode, 200);
  assert.equal(pipelines.length, 1);
  assert.equal(pipelines[0].name, "mimechan");
  assert.deepEqual(pipelines[0].pipeline[0], { $match: { parent: null, user: "private-user-id" } });
  assert.deepEqual(pipelines[0].pipeline[1], { $project: { "file.data": 0 } });
  const unions = pipelines[0].pipeline.filter((stage) => stage.$unionWith);
  assert.deepEqual(unions.map((stage) => stage.$unionWith.coll), ["paintings", "tapes", "pieces", "kidlisp", "whistlegraph-mints"]);
  for (const stage of unions) assert.equal(stage.$unionWith.pipeline[0].$match.user, "private-user-id");
  assert.deepEqual(pipelines[0].pipeline.slice(-3), [{ $sort: { when: -1, code: 1 } }, { $skip: 0 }, { $limit: 13 }]);
  assert.equal(data.posts.length, 12);
  assert.equal(data.hasMore, true);
  assert.equal(data.page, 0);
  assert.equal(data.posts[0].replies, 3, "uploads keep their own reply count");
  assert.equal(data.posts[2].replies, 5, "media replies come from the thread ledger");
  assert.deepEqual(finds[0].query, { _id: { $in: docs.slice(2, 12).map((d) => d.code) } });
  assert.ok(!response.body.includes("private-user-id"));
  const kind = await get(handler, { kind: "painting", page: "2" });
  assert.equal(kind.statusCode, 200);
  assert.equal(pipelines[1].name, "paintings");
  assert.deepEqual(pipelines[1].pipeline.slice(-2), [{ $skip: 24 }, { $limit: 13 }]);
});
