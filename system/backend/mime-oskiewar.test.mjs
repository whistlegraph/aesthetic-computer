import test from "node:test";
import assert from "node:assert/strict";
import { replayPost, replayPosts, replayIsDense, REPLAY_VISIBLE, REPLAY_FIELDS } from "./mime-oskiewar.mjs";
import { feedOnlyOp, feedPool, freshenFeed, rankFeed } from "./mime-feed-engine.mjs";
import { createHandler } from "../netlify/functions/mime.mjs";

const saved = { _id: "ow-zimmu299", format: "ac.oskiedemo", game: "oskiewar", version: 1,
  simulation: "oskiewar-physics-1", map: "station", build: 12,
  recordedAt: new Date("2026-10-13T06:50:00Z"), fighters: ["@JEFFREY", "@FIFI"],
  durationTicks: 3600, tickRate: 60, sourceDigest: "private", commands: [[0, 1, 1]],
  _activity: { start: 0, end: 3600, inputs: [[180, 240, 300, 360], [180, 240, 300, 360]],
    actions: Array.from({ length: 115 }, (_, i) => 180 + i * 30),
    contacts: [300, 600, 900], frames: [180, 1200, 2400, 3600] } };
const cursor = rows => ({ sort() { return this; }, limit(n) { return cursor(rows.slice(0, n)); },
  toArray: async () => rows });
const idle = { ...saved, _id: "ow-idle123", _activity: { ...saved._activity, actions: [] } };
const db = { collection(name) { return {
  find: () => cursor(name === "oskiewar-replays" ? [saved, { ...saved, _id: "ow-guest123", fighters: ["", ""] }] : []),
  findOne: async q => name === "oskiewar-replays" && q._id === saved._id ? saved
    : name === "oskiewar-replays" && q._id === idle._id ? idle
    : name === "mime-media-threads" && q._id === saved._id ? { replies: 2 } : null,
  aggregate: () => cursor(name === "oskiewar-replays"
    ? [saved, { ...saved, _id: "ow-guest123", fighters: ["", ""] }, idle] : []),
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
  assert.doesNotMatch(JSON.stringify(post), /sourceDigest|private|commands|_activity/);
  assert.equal(replayPost({ _id: "../../bad" }), null);
  assert.equal(replayPost({ ...saved, fighters: ["", ""] }).name, null);
  const legacy = replayPost({ ...saved, _id: "ow-abc123-def456" });
  assert.equal(new URL(legacy.media.url).pathname, "/mac-test.html");
  assert.equal(new URL(legacy.media.url).searchParams.get("replay"), "ow-abc123-def456");
});

test("replay candidate reads are bounded, public and omit full streams", async () => {
  let pipeline, options;
  await replayPosts({ collection: () => ({ aggregate(p, o) {
    pipeline = p; options = o; return cursor([]);
  } }) }, 200);
  assert.equal(pipeline.find(stage => stage.$limit).$limit, 1000);
  assert.deepEqual(pipeline[0].$match, { ...REPLAY_VISIBLE, version: 1,
    simulation: "oskiewar-physics-1", tickRate: 60 });
  const { _activity, ...fields } = pipeline.at(-1).$project;
  assert.deepEqual(fields, REPLAY_FIELDS);
  assert.ok(_activity);
  assert.equal(options.maxTimeMS, 10000);
});

test("only dense rounds join the feed; guest rounds and old permalinks still work", async () => {
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
  const idleResponse = await handler({ httpMethod: "GET", queryStringParameters: { thread: idle._id } });
  assert.equal(idleResponse.statusCode, 200);
  assert.equal(JSON.parse(idleResponse.body).op.code, idle._id);
});

test("density needs both fighters, combat, usable frames and a prompt sustained fight", () => {
  const withActivity = change => ({ ...saved, _activity: { ...saved._activity, ...change } });
  assert.equal(replayIsDense(saved), true);
  assert.equal(replayIsDense({ ...saved, fighters: ["", ""] }), true);
  assert.equal(replayIsDense({ ...saved, _activity: undefined }), false);
  for (const change of [
    { actions: [] }, { inputs: [[180, 240, 300, 360], []] }, { frames: [180, 200] },
    { frames: [180, 190, 200, 210] }, { contacts: [] },
    { actions: Array(100).fill(180), contacts: Array(100).fill(180) },
    { actions: [180, 300, 600, 900] },
    { actions: Array.from({ length: 60 }, (_, i) => 180 + i) },
    { actions: saved._activity.actions.filter(t => t > 600) },
    { actions: saved._activity.actions.filter(t => t < 1200 || t > 1800) },
    { start: 3600, end: 5400 }, // historical events cannot qualify a new idle round
    { start: 0, end: 200 }, { start: NaN },
  ]) assert.equal(replayIsDense(withActivity(change)), false, JSON.stringify(change));
});

test("filter before limiting so newer idle rounds cannot crowd out active players", async () => {
  const idle = Array.from({ length: 250 }, (_, i) => ({ ...saved, _id: "ow-idle" + i,
    _activity: { ...saved._activity, actions: [] } }));
  const bots = Array.from({ length: 250 }, (_, i) => ({ ...saved, _id: "ow-bot" + i, fighters: ["RED", "BLUE"] }));
  const posts = await replayPosts({ collection: () => ({ aggregate: () => cursor([...idle, ...bots, saved]) }) }, 200);
  assert.equal(posts.length, 200);
  assert.equal(posts[0].code, saved._id);
  assert.equal(posts.some(post => post.code.startsWith("ow-idle")), false);
});

test("an active named player leads ahead of newer dense bot rounds", () => {
  const now = saved.recordedAt.getTime();
  const replays = [
    { code: "ow-bot123", author: null, when: new Date(now) },
    { code: "ow-player123", author: "@jeffrey", when: new Date(now - 86400000) },
  ].map(item => ({ ...item, app: "oskiewar", post: { media: { kind: "oskiewar" } } }));
  assert.equal(rankFeed({ live: replays, archive: [] }, "test", 2, now)[0].code, "ow-player123");
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
