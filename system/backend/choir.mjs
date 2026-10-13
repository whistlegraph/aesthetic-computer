// choir, 26.10.12
// A room for art-world professionals. One main chat; any post can grow a
// thread; tags are optional (Fía, 10-12); anyone can start a tag.
//
// Pure store logic over a Mongo collection so it tests without a database
// (see choir.test.mjs). The lith route is netlify/functions/choir.mjs.

export const MAX_CHARS = 2000;
export const MIN_TAGS = 0;
export const MAX_TAGS = 6;
export const STARTER_TAGS = ["news", "discussion", "free-and-for-sale", "los-angeles", "new-york"];

// Fía's brake on over-posting, sized for chat: a burst cap and an hourly cap, per account.
export const RATE = { burst: { count: 8, ms: 60_000 }, hourly: { count: 60, ms: 60 * 60_000 } };

// "Free and for sale" → "free-and-for-sale". Tags are the navigation, so they
// are kept to one spelling.
export function normalizeTag(raw) {
  const tag = String(raw ?? "").trim().toLowerCase().replace(/&/g, "and")
    .replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").slice(0, 32);
  return tag || null;
}

export function normalizeTags(list) {
  return [...new Set((Array.isArray(list) ? list : []).map(normalizeTag).filter(Boolean))];
}

const URL_RE = /https?:\/\/[^\s<>()"']+/g;
export function linksIn(text) {
  return [...new Set((String(text ?? "").match(URL_RE) || []).map((u) => u.replace(/[.,;:!?]+$/, "")))].slice(0, 4);
}

export function validatePost({ text, tags, replyTo }) {
  const body = String(text ?? "").trim();
  if (!body) throw Error("Say something");
  if (body.length > MAX_CHARS) throw Error(`Keep it under ${MAX_CHARS} characters`);
  const clean = normalizeTags(tags);
  // A reply lives inside its root's tags, so it needs none of its own.
  if (!replyTo && clean.length < MIN_TAGS) throw Error(`Tag it at least ${MIN_TAGS} ways`);
  if (clean.length > MAX_TAGS) throw Error(`At most ${MAX_TAGS} tags`);
  if (replyTo != null && !validID(replyTo)) throw Error("Bad thread");
  return { text: body, tags: clean, links: linksIn(body), replyTo: replyTo ?? null };
}

export const validID = (v) => typeof v === "string" && /^[a-f0-9]{24}$/i.test(v);

// What leaves the server. The Auth0 sub stays home; the handle is the name.
export function publicPost(row) {
  if (!row) return null;
  return {
    id: String(row._id), handle: row.handle, text: row.deleted ? "" : row.text,
    tags: row.tags, links: row.deleted ? [] : row.links || [], replyTo: row.replyTo ? String(row.replyTo) : null,
    replies: row.replies || 0, lastReplyAt: row.lastReplyAt || null,
    when: row.when, deleted: row.deleted === true,
  };
}

export function mongoChoirStore(posts, { tags, prefs, id = () => hex24(), now = () => new Date().toISOString() } = {}) {
  let indexes;
  const ready = () => indexes ??= Promise.all([
    posts.createIndex({ when: -1 }), posts.createIndex({ replyTo: 1, when: 1 }),
    posts.createIndex({ tags: 1, when: -1 }), posts.createIndex({ owner: 1, when: -1 }),
  ]);

  async function paceOf(owner, at) {
    const t = Date.parse(at);
    for (const [name, { count, ms }] of Object.entries(RATE)) {
      const since = new Date(t - ms).toISOString();
      const recent = await posts.countDocuments({ owner, when: { $gt: since } });
      if (recent >= count) throw Error(name === "burst" ? "Slow down a little" : "That's enough for this hour");
    }
  }

  return {
    async post(owner, handle, input) {
      if (!handle) throw Error("Pick a handle first");
      await ready();
      const clean = validatePost(input);
      const when = now();
      await paceOf(owner, when);
      let root = null;
      if (clean.replyTo) {
        root = await posts.findOne({ _id: clean.replyTo, replyTo: null });
        if (!root || root.deleted) throw Error("That thread is gone");
        clean.tags = root.tags;
      }
      const row = { _id: id(), owner, handle, ...clean, when, replies: 0 };
      await posts.insertOne(row);
      if (root) await posts.updateOne({ _id: root._id }, { $inc: { replies: 1 }, $set: { lastReplyAt: when } });
      else if (tags) for (const tag of clean.tags) {
        await tags.updateOne({ _id: tag }, { $inc: { count: 1 }, $set: { lastAt: when } }, { upsert: true });
      }
      return row;
    },

    // The main chat: root posts, newest first, optionally under one tag.
    // News items (news: true) live in Discover instead of the chat.
    async feed({ tag, before, limit = 30, news = false } = {}) {
      await ready();
      const q = { replyTo: null };
      if (news) q.news = true;
      if (tag) q.tags = normalizeTag(tag);
      if (before) q.when = { $lt: before };
      const rows = await posts.find(q).sort({ when: -1 }).limit(Math.min(Math.max(limit, 1), 100)).toArray();
      // A deleted message with no replies has no thread to hold together.
      return rows.filter((r) => !(r.deleted && !r.replies) && (news || r.news !== true));
    },

    async thread(rootID) {
      if (!validID(rootID)) return null;
      const root = await posts.findOne({ _id: rootID, replyTo: null });
      if (!root) return null;
      const replies = await posts.find({ replyTo: rootID }).sort({ when: 1 }).limit(500).toArray();
      return { root, replies };
    },

    async tagList() {
      if (!tags) return STARTER_TAGS.map((t) => ({ tag: t, count: 0 }));
      const rows = await tags.find({}).sort({ count: -1 }).limit(200).toArray();
      const seen = new Set(rows.map((r) => r._id));
      return [...rows.map((r) => ({ tag: r._id, count: r.count, lastAt: r.lastAt })),
        ...STARTER_TAGS.filter((t) => !seen.has(t)).map((t) => ({ tag: t, count: 0 }))];
    },

    // Deleting keeps the row so a thread's shape survives; the words go.
    async remove(owner, postID) {
      if (!validID(postID)) return false;
      const r = await posts.updateOne({ _id: postID, owner }, { $set: { deleted: true, text: "", links: [] } });
      return r.modifiedCount === 1;
    },

    async prefsOf(owner) {
      const row = prefs ? await prefs.findOne({ _id: owner }) : null;
      return { notifications: row?.notifications !== false };
    },
    async setPrefs(owner, { notifications }) {
      if (typeof notifications !== "boolean") throw Error("notifications must be true or false");
      if (prefs) await prefs.updateOne({ _id: owner }, { $set: { notifications } }, { upsert: true });
      return { notifications };
    },
  };
}

function hex24() {
  let s = "";
  while (s.length < 24) s += Math.floor(Math.random() * 16).toString(16);
  return s;
}
