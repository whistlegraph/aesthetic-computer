import test from "node:test";
import assert from "node:assert/strict";
import { mongoChoirStore, normalizeTag, linksIn, validatePost, publicPost, RATE, STARTER_TAGS, curate } from "./choir.mjs";

// The smallest Mongo choir needs: find/sort/limit, findOne, insertOne,
// updateOne with $set/$inc/upsert, countDocuments.
function collection() {
  const rows = [];
  const matches = (row, q) => Object.entries(q).every(([k, v]) => {
    const actual = row[k];
    if (v && typeof v === "object" && !Array.isArray(v)) {
      if ("$gt" in v && !(actual > v.$gt)) return false;
      if ("$lt" in v && !(actual < v.$lt)) return false;
      return true;
    }
    if (Array.isArray(actual)) return actual.includes(v);
    return actual === v;
  });
  return {
    rows,
    createIndex: async () => {},
    insertOne: async (row) => { rows.push({ ...row }); },
    deleteOne: async (q) => { const i = rows.findIndex((r) => matches(r, q)); if (i >= 0) rows.splice(i, 1); return { deletedCount: i >= 0 ? 1 : 0 }; },
    findOne: async (q) => rows.find((r) => matches(r, q)) || null,
    countDocuments: async (q) => rows.filter((r) => matches(r, q)).length,
    updateOne: async (q, u, o = {}) => {
      let row = rows.find((r) => matches(r, q));
      if (!row && o.upsert) { row = { ...q }; rows.push(row); }
      if (!row) return { modifiedCount: 0 };
      Object.assign(row, u.$set || {});
      for (const [k, n] of Object.entries(u.$inc || {})) row[k] = (row[k] || 0) + n;
      return { modifiedCount: 1 };
    },
    find(q) {
      let out = rows.filter((r) => matches(r, q));
      const chain = {
        sort(spec) { const [[k, d]] = Object.entries(spec); out = out.slice().sort((a, b) => (a[k] > b[k] ? 1 : a[k] < b[k] ? -1 : 0) * d); return chain; },
        limit(n) { out = out.slice(0, n); return chain; },
        toArray: async () => out.map((r) => ({ ...r })),
      };
      return chain;
    },
  };
}

function store() {
  let n = 0, t = Date.parse("2026-10-12T16:00:00Z");
  const posts = collection(), tags = collection(), prefs = collection();
  const clock = { tick: (ms = 60_000) => { t += ms; } };
  const s = mongoChoirStore(posts, { tags, prefs, id: () => String(++n).padStart(24, "0"), now: () => new Date(t).toISOString() });
  return { s, posts, tags, prefs, clock };
}

test("tags are one spelling, links are pulled out of the words", () => {
  assert.equal(normalizeTag(" Free & For Sale! "), "free-and-for-sale");
  assert.equal(normalizeTag("Los Angeles"), "los-angeles");
  assert.equal(normalizeTag("###"), null);
  assert.deepEqual(linksIn("see https://award.art/, and https://award.art/ again (https://x.y/z)."), ["https://award.art/", "https://x.y/z"]);
});

test("tags are optional; a reply takes its root's", () => {
  assert.deepEqual(validatePost({ text: "hi", tags: [] }).tags, []);
  assert.deepEqual(validatePost({ text: "hi", tags: ["news"] }).tags, ["news"]);
  assert.throws(() => validatePost({ text: "   ", tags: ["news", "la"] }), /Say something/);
  assert.throws(() => validatePost({ text: "hi", tags: ["a", "b", "c", "d", "e", "f", "g"] }), /At most/);
  assert.deepEqual(validatePost({ text: "ok", tags: [], replyTo: "0".repeat(24) }).tags, []);
});

test("the main chat is roots newest first, a thread is root plus replies oldest first, tags count", async () => {
  const { s, clock } = store();
  const a = await s.post("sub-fia", "@fia", { text: "Opening Friday https://award.art", tags: ["News", "Los Angeles"] });
  clock.tick();
  const b = await s.post("sub-harry", "@harry", { text: "Fair report", tags: ["news", "new-york"] });
  clock.tick();
  const r1 = await s.post("sub-jeff", "@jeffrey", { text: "going", replyTo: a._id });
  clock.tick();
  const r2 = await s.post("sub-fia", "@fia", { text: "see you there", replyTo: a._id });
  const feed = (await s.feed()).map(publicPost);
  assert.deepEqual(feed.map((p) => p.handle), ["@harry", "@fia"], "replies never surface as roots");
  assert.equal(feed[1].replies, 2);
  assert.equal(feed[1].lastReplyAt, r2.when);
  assert.deepEqual(feed[1].links, ["https://award.art"]);
  assert.deepEqual((await s.feed({ tag: "Los Angeles" })).map((p) => p._id), [a._id]);
  const thread = await s.thread(a._id);
  assert.deepEqual(thread.replies.map((p) => p._id), [r1._id, r2._id]);
  assert.deepEqual(r1.tags, a.tags, "a reply inherits the root's tags");
  assert.equal(await s.thread(r1._id), null, "a reply is not a thread root");
  const tags = await s.tagList();
  assert.deepEqual(tags.slice(0, 1), [{ tag: "news", count: 2, lastAt: b.when }]);
  assert.ok(STARTER_TAGS.every((t) => tags.some((x) => x.tag === t)), "starter tags show even at zero");
  assert.equal(publicPost(thread.root).id, a._id);
  assert.equal("owner" in publicPost(thread.root), false, "the Auth0 sub never leaves");
});

test("posting needs a handle, replies need a living root, deleting keeps the shape", async () => {
  const { s, clock } = store();
  await assert.rejects(s.post("sub-x", null, { text: "hi", tags: ["a", "b"] }), /handle/);
  await assert.rejects(s.post("sub-x", "@x", { text: "hi", replyTo: "f".repeat(24) }), /gone/);
  const a = await s.post("sub-x", "@x", { text: "hello", tags: ["a", "b"] });
  clock.tick();
  await s.post("sub-y", "@y", { text: "reply", replyTo: a._id });
  assert.equal(await s.remove("sub-y", a._id), false, "only the author deletes");
  assert.equal(await s.remove("sub-x", a._id), true);
  const t = await s.thread(a._id);
  assert.equal(publicPost(t.root).deleted, true);
  assert.equal(publicPost(t.root).text, "");
  assert.equal(t.replies.length, 1, "replies outlive a deleted root");
  assert.ok((await s.feed()).some((p) => p._id === a._id), "a deleted root with replies stays in the room");
  const lone = await s.post("sub-x", "@x", { text: "oops", tags: [] });
  await s.remove("sub-x", lone._id);
  assert.ok(!(await s.feed()).some((p) => p._id === lone._id), "a deleted message with no replies leaves the room");
  clock.tick();
  await assert.rejects(s.post("sub-z", "@z", { text: "late", replyTo: a._id }), /gone/);
});

test("the brake: a burst cap a minute, an hourly cap", async () => {
  const { s, clock } = store();
  const say = (i) => s.post("sub-h", "@harry", { text: "news " + i, tags: ["news", "discussion"] });
  let made = 0;
  while (made < RATE.burst.count) { await say(++made); clock.tick(1000); }
  await assert.rejects(say(99), /Slow down/);
  clock.tick(RATE.burst.ms + 1000);
  while (made < RATE.hourly.count) { clock.tick(RATE.burst.ms + 1000); await say(++made); }
  clock.tick(RATE.burst.ms + 1000);
  await assert.rejects(say(99), /this hour/);
  assert.equal((await s.feed({ limit: 100 })).length, RATE.hourly.count);
  assert.doesNotReject(s.post("sub-other", "@other", { text: "fine", tags: ["news", "discussion"] }), "the brake is per account");
});

test("notifications default on and can be turned off", async () => {
  const { s } = store();
  assert.deepEqual(await s.prefsOf("sub-a"), { notifications: true });
  await s.setPrefs("sub-a", { notifications: false });
  assert.deepEqual(await s.prefsOf("sub-a"), { notifications: false });
  await assert.rejects(s.setPrefs("sub-a", { notifications: "no" }), /true or false/);
});

test("news lives in Discover, not the chat", async () => {
  const { s, posts } = store();
  const chat = await s.post("sub-a", "@a", { text: "hello", tags: [] });
  await posts.insertOne({ _id: "a".repeat(24), owner: "choir|editor", handle: "@choir", text: "Headline https://x.y", tags: [], links: ["https://x.y"], replyTo: null, replies: 0, when: "2026-10-12T00:00:00.000Z", news: true });
  assert.deepEqual((await s.feed()).map((p) => p._id), [chat._id]);
  assert.deepEqual((await s.feed({ news: true })).map((p) => p._id), ["a".repeat(24)]);
  assert.ok(await s.thread("a".repeat(24)), "a news item still has a comment thread");
});

test("the curator adds new links, skips repeats and Artforum, and retires closed calls", async () => {
  const posts = collection(), archive = collection();
  let n = 0; const id = () => String(++n).padStart(24, "c");
  const now = new Date("2026-10-13T12:00:00Z");
  await posts.insertOne({ _id: "b".repeat(24), owner: "choir|editor", links: ["https://old.example/x"], kind: "opportunity", deadline: "2026-10-01", replies: 0, news: true, when: "2026-09-01T00:00:00Z", replyTo: null });
  const r = await curate(posts, archive, [
    { url: "https://e-flux.com/a", title: "A show", description: "About it.", siteName: "e-flux", via: "e-flux" },
    { url: "https://e-flux.com/a", title: "A show again" },
    { url: "https://www.artforum.com/x", title: "Nope" },
    { url: "https://grants.example/g", title: "A grant", kind: "opportunity", deadline: "2026-11-15" },
    { url: "https://grants.example/closed", title: "Closed", kind: "opportunity", deadline: "2026-10-01" },
  ], { now, id });
  assert.deepEqual(r.added, ["https://e-flux.com/a", "https://grants.example/g"]);
  assert.equal(r.skipped.length, 3);
  assert.deepEqual(r.expired, ["https://old.example/x"]);
  assert.equal(archive.rows.length, 1);
  const g = posts.rows.find((p) => p.links[0] === "https://grants.example/g");
  assert.equal(g.kind, "opportunity"); assert.equal(g.deadline, "2026-11-15"); assert.equal(g.news, true);
  assert.equal(posts.rows.find((p) => p.links[0] === "https://e-flux.com/a").preview.via, "e-flux");
});
