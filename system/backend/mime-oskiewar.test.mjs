import test from "node:test";
import assert from "node:assert/strict";
import { replayPost, replayPosts, REPLAY_VISIBLE, REPLAY_FIELDS } from "./mime-oskiewar.mjs";
import { feedOnlyOp, feedPool, freshenFeed, rankFeed } from "./mime-feed-engine.mjs";
import { createHandler } from "../netlify/functions/mime.mjs";

const saved = { _id: "ow-zimmu299", format: "ac.oskiedemo", game: "oskiewar", version: 1,
  simulation: "oskiewar-physics-1", map: "station", build: 12,
  recordedAt: new Date("2026-10-13T06:50:00Z"), fighters: ["@JEFFREY", "@FIFI"],
  durationTicks: 3600, tickRate: 60, sourceDigest: "private", commands: [[0, 1, 1]] };
const cursor = rows => ({ sort() { return this; }, limit(n) { return cursor(rows.slice(0, n)); },
  toArray: async () => rows });
const db = { collection(name) { return {
  find: () => cursor(name === "oskiewar-replays" ? [saved, { ...saved, _id: "ow-guest123", fighters: ["", ""] }] : []),
  findOne: async q => name === "oskiewar-replays" && q._id === saved._id ? saved
    : name === "mime-media-threads" && q._id === saved._id ? { replies: 2 } : null,
  aggregate: () => cursor([]),
}; } };

test("replay cards keep round identity and both fighters, without private or bulk fields", () => {
  const post = replayPost(saved);
  assert.equal(post.code, saved._id);
  assert.equal(post.media.code, saved._id);
  assert.equal(post.name, "@jeffrey");
  assert.equal(post.text, "@jeffrey × @fifi");
  assert.deepEqual(post.media.fighters, saved.fighters);
  assert.equal(post.media.duration, 60);
  assert.equal(post.media.version, undefined, "format version is not an artwork revision");
  assert.equal(post.media.url, "https://oskiewar.com/zimmu299?mime=1");
  assert.equal(post.file.type, "application/x-oskiewar-demo");
  assert.equal(new URL(post.file.url).searchParams.get("id"), saved._id);
  assert.doesNotMatch(JSON.stringify(post), /sourceDigest|private|commands/);
  assert.equal(replayPost({ _id: "../../bad" }), null);
  assert.equal(replayPost({ ...saved, fighters: ["", ""] }).name, null);
  const legacy = replayPost({ ...saved, _id: "ow-abc123-def456" });
  assert.equal(new URL(legacy.media.url).pathname, "/mac-test.html");
  assert.equal(new URL(legacy.media.url).searchParams.get("replay"), "ow-abc123-def456");
});

test("replay reads are bounded and exclude hidden works and large streams", async () => {
  let query, options, limit;
  await replayPosts({ collection: () => ({ find(q, o) { query = q; options = o; return {
    sort: () => ({ limit(n) { limit = n; return { toArray: async () => [] }; } }),
  }; } }) }, 200);
  assert.equal(limit, 200);
  assert.deepEqual(query, REPLAY_VISIBLE);
  assert.equal(query.private.$ne, true);
  assert.deepEqual(query.visibility.$nin, ["private", "unlisted"]);
  assert.deepEqual(options.projection, REPLAY_FIELDS);
  assert.equal(options.projection.commands, undefined);
});

test("saved and guest rounds join the feed, and old IDs resolve outside its window", async () => {
  freshenFeed();
  const pool = await feedPool(db);
  assert.deepEqual(pool.live.map(i => i.code), [saved._id, "ow-guest123"]);
  const op = await feedOnlyOp(db, saved._id);
  assert.equal(op.code, saved._id);
  assert.equal(op.replies, 2);
  assert.equal(op._public, true);
  assert.equal(await feedOnlyOp(db, "ow-missing12"), null);
  const handler = createHandler(async () => ({ db }));
  const response = await handler({ httpMethod: "GET", queryStringParameters: { thread: saved._id } });
  assert.equal(response.statusCode, 200);
  const { op: publicOp } = JSON.parse(response.body);
  assert.equal(publicOp.media.kind, "oskiewar");
  assert.equal(publicOp.file.url, replayPost(saved).file.url);
  assert.equal(publicOp._public, undefined);
  assert.equal(publicOp._media, undefined);
});

test("latest replay is on page one without letting a replay burst occupy the opening", () => {
  const now = saved.recordedAt.getTime();
  const replays = Array.from({ length: 200 }, (_, i) => ({ code: "ow-round" + i, app: "oskiewar",
    author: i ? null : "@jeffrey", when: new Date(now - i * 1000), post: { media: { kind: "oskiewar" } } }));
  const art = Array.from({ length: 20 }, (_, i) => ({ code: "art" + i, app: "aesthetic", author: "@artist" + i,
    when: new Date(now - i * 100), post: { media: { kind: "painting" } } }));
  for (const seed of ["one", "two", "three"]) {
    const feed = rankFeed({ live: [...replays, ...art], archive: [] }, seed, 12, now);
    assert.equal(feed[1].code, "ow-round0");
    assert.equal(feed.filter(i => i.app === "oskiewar").length, 1);
    const all = rankFeed({ live: [...replays, ...art], archive: [] }, seed, 220, now);
    assert.equal(new Set(all.map(i => i.code)).size, 220);
  }
});
