// choir, 26.10.12
// A room for art-world professionals. One main chat; any post can grow a
// thread; tags are optional (Fía, 10-12); anyone can start a tag.
//
// Pure store logic over a Mongo collection so it tests without a database
// (see choir.test.mjs). The lith route is netlify/functions/choir.mjs.

import { screen as defaultScreen, screenPost } from "./choir-moderation.mjs";

export const MAX_CHARS = 2000;
export const SLATE_MS = 20 * 60 * 60 * 1000;   // one curator run per day
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

// Uploads live in the "choir" Spaces bucket; attachments must point there.
export const UPLOAD_BASE = "https://choir.sfo3.digitaloceanspaces.com/";
export const UPLOAD_MAX = 25 * 1024 * 1024;
export const UPLOAD_TYPES = /^(image\/(jpeg|png|gif|webp|heic|heif)|application\/pdf|audio\/(mpeg|mp4|x-m4a|aac|wav|x-wav)|video\/(mp4|quicktime)|text\/plain|application\/zip)$/;

export function cleanAttachments(list) {
  if (list == null) return [];
  if (!Array.isArray(list) || list.length > 6) throw Error("At most 6 attachments");
  return list.map((a) => {
    const url = String(a?.url || "");
    if (!url.startsWith(UPLOAD_BASE) || /\s/.test(url)) throw Error("Attachments must be choir uploads");
    const type = String(a.type || "");
    if (!UPLOAD_TYPES.test(type)) throw Error("That file type isn't supported");
    const size = Number(a.size) || 0;
    if (size < 0 || size > UPLOAD_MAX) throw Error("Files can be up to 25 MB");
    const out = { url, type, size, name: String(a.name || url.split("/").pop()).slice(0, 120) };
    if (Number.isFinite(a.width) && Number.isFinite(a.height)) { out.width = Math.round(a.width); out.height = Math.round(a.height); }
    // Voice notes (Fía, 10-12): up to a minute, with an on-device transcript.
    if (type.startsWith("audio/")) {
      const d = Number(a.duration);
      if (Number.isFinite(d)) { if (d > 61) throw Error("Voice notes can be up to a minute"); out.duration = Math.round(d * 10) / 10; }
      if (typeof a.transcript === "string" && a.transcript.trim()) out.transcript = a.transcript.trim().slice(0, 2000);
    }
    // Videos (Fía, 10-12): up to a minute, with a poster frame from the same bucket.
    if (type.startsWith("video/")) {
      const d = Number(a.duration);
      if (Number.isFinite(d)) { if (d > 61) throw Error("Videos can be up to a minute"); out.duration = Math.round(d * 10) / 10; }
      if (a.poster != null) {
        const poster = String(a.poster);
        if (!poster.startsWith(UPLOAD_BASE) || /\s/.test(poster)) throw Error("Attachments must be choir uploads");
        out.poster = poster;
      }
    }
    return out;
  });
}

// What a push says for a post with no words: its first kind of attachment.
export function attachmentsLabel(attachments) {
  if (!attachments?.length) return "";
  if (attachments.some((a) => String(a.type).startsWith("video/"))) return "🎬 video";
  if (attachments.some((a) => String(a.type).startsWith("audio/"))) return "🎙 voice note";
  return "📷 photo";
}

export function validatePost({ text, tags, replyTo, attachments }) {
  const body = String(text ?? "").trim();
  const files = cleanAttachments(attachments);
  if (!body && !files.length) throw Error("Say something");
  if (body.length > MAX_CHARS) throw Error(`Keep it under ${MAX_CHARS} characters`);
  const clean = normalizeTags(tags);
  // A reply lives inside its root's tags, so it needs none of its own.
  if (!replyTo && clean.length < MIN_TAGS) throw Error(`Tag it at least ${MIN_TAGS} ways`);
  if (clean.length > MAX_TAGS) throw Error(`At most ${MAX_TAGS} tags`);
  if (replyTo != null && !validID(replyTo)) throw Error("Bad thread");
  return { text: body, tags: clean, links: linksIn(body), replyTo: replyTo ?? null, ...(files.length ? { attachments: files } : {}) };
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
    // News items carry their card (title, summary, image) so it never depends
    // on fetching a site that blocks previews.
    ...(row.preview && !row.deleted ? { preview: row.preview } : {}),
    ...(row.kind ? { kind: row.kind } : {}), // "opportunity" for open calls in Discover
    ...(row.attachments?.length && !row.deleted ? { attachments: row.attachments } : {}),
  };
}

export function mongoChoirStore(posts, { tags, prefs, id = () => hex24(), now = () => new Date().toISOString(), screen = defaultScreen } = {}) {
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
      const clean = screenPost(validatePost(input), screen);
      // A member the moderator removed (choir-moderation.mjs resolveReport) stays out.
      if (prefs && (await prefs.findOne({ _id: owner }))?.banned === true) throw Error("Your account can't post in choir. Write to mail@aesthetic.computer.");
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
    // Discover is one day's slate (Fía, 10-12 22:47: "a 24 hr cycle … the slate
    // wipes clean each day"): only what arrived within SLATE_MS of the newest
    // item, so a day the curator skips keeps yesterday's rather than going blank.
    // Chat posts that link to old items stay; `all` is for the curator's dedupe.
    async feed({ tag, before, limit = 30, news = false, all = false } = {}) {
      await ready();
      const q = { replyTo: null };
      if (news) q.news = true;
      if (news && !all && !before) {
        // Anchor on the newest item that isn't dated ahead (an old hand seed is).
        const newest = await posts.findOne({ replyTo: null, news: true, when: { $lte: now() } }, { sort: { when: -1 } });
        if (newest) q.when = { $gte: new Date(Date.parse(newest.when) - SLATE_MS).toISOString() };
      }
      if (tag) q.tags = normalizeTag(tag);
      if (before) q.when = { $lt: before };
      // Fía (10-13 20:32): "could we maybe not include openings in the discover
      // swipe through?" They stay in the store (a paid openings bulletin may want
      // them later) and in the curator's dedupe (`all`), but not on the slate.
      if (news && !all) q.kind = { $ne: "opening" };
      const rows = await posts.find(q).sort({ when: -1 }).limit(Math.min(Math.max(limit, 1), 100)).toArray();
      // A deleted message with no replies has no thread to hold together.
      const kept = rows.filter((r) => !(r.deleted && !r.replies) && (news || r.news !== true));
      // "fill it with some art news and opportunities with nice imagery":
      // the slate leads with the items that have a picture, then the rest.
      return news && !all ? [...kept.filter((r) => r.preview?.image), ...kept.filter((r) => !r.preview?.image)] : kept;
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

    // Fía (10-12): chat and Discover notifications switch separately. An older
    // single `notifications: false` still silences both.
    async prefsOf(owner) {
      const row = prefs ? await prefs.findOne({ _id: owner }) : null;
      const all = row?.notifications !== false;
      return { notifications: all, chat: all && row?.chat !== false, discover: all && row?.discover !== false };
    },
    async setPrefs(owner, body) {
      const set = {};
      for (const k of ["notifications", "chat", "discover"]) {
        if (body[k] === undefined) continue;
        if (typeof body[k] !== "boolean") throw Error(`${k} must be true or false`);
        set[k] = body[k];
      }
      if (!Object.keys(set).length) throw Error("notifications must be true or false");
      // Turning a channel on also lifts an old blanket "off".
      if (set.chat === true || set.discover === true) set.notifications = true;
      if (prefs) await prefs.updateOne({ _id: owner }, { $set: set }, { upsert: true });
      return this.prefsOf(owner);
    },
  };
}

function hex24() {
  let s = "";
  while (s.length < 24) s += Math.floor(Math.random() * 16).toString(16);
  return s;
}

// The curator's door (toolchain/choir/curator.mjs on jasellite): add news and
// open calls to Discover, skipping links already there, and archive open calls
// whose deadline has passed. Items are already chosen; this only shapes them.
export async function curate(posts, archive, items, { now = new Date(), id = () => hex24() } = {}) {
  const today = now.toISOString().slice(0, 10);
  const added = [], skipped = [];
  for (const [i, it] of (Array.isArray(items) ? items : []).slice(0, 30).entries()) {
    const url = String(it?.url || "");
    if (!/^https:\/\/[^\s]+$/.test(url) || /artforum\.com/i.test(url)) { skipped.push(url || "(no url)"); continue; }
    if (await posts.findOne({ links: url })) { skipped.push(url); continue; }
    const title = String(it.title || "").replace(/\s+/g, " ").trim().slice(0, 200);
    if (!title) { skipped.push(url); continue; }
    const kind = ["opportunity", "opening"].includes(it.kind) ? it.kind : null;
    const deadline = /^\d{4}-\d{2}-\d{2}$/.test(it.deadline || "") ? it.deadline : null;
    if (kind && deadline && deadline < today) { skipped.push(url); continue; }
    const v = validatePost({ text: `${title}\n${url}`, tags: [] });
    const row = {
      _id: id(), owner: "choir|editor", handle: "@choir", ...v, links: [url],
      when: new Date(now.getTime() - i * 60_000).toISOString(), replies: 0, news: true,
      ...(kind ? { kind } : {}), ...(deadline ? { deadline } : {}),
      preview: {
        title, description: String(it.description || "").slice(0, 600) || null,
        image: /^https:\/\//.test(it.image || "") ? it.image : null,
        siteName: String(it.siteName || new URL(url).hostname.replace(/^www\./, "")).slice(0, 80),
        ...(it.via ? { via: String(it.via).slice(0, 80) } : {}),
      },
    };
    await posts.insertOne(row);
    added.push(url);
  }
  // Calls that have closed and shows that have ended leave Discover for the
  // archive (replies keep them). For an opening, `deadline` is its last day.
  const expired = [
    ...await posts.find({ kind: "opportunity", deadline: { $lt: today }, replies: 0 }).toArray(),
    ...await posts.find({ kind: "opening", deadline: { $lt: today }, replies: 0 }).toArray(),
  ];
  for (const r of expired) {
    await archive.updateOne({ _id: r._id }, { $set: { ...r, archivedAt: now.toISOString(), archivedWhy: "deadline passed" } }, { upsert: true });
    await posts.deleteOne({ _id: r._id });
  }
  return { added, skipped, expired: expired.map((r) => r.links?.[0]) };
}

// A newcomer's first look at choir: one welcome line in the main chat, once per
// member (the `greeted` flag on their choir-prefs row is claimed atomically).
export async function greet(posts, prefs, member, handle, { now = new Date(), id = () => hex24() } = {}) {
  if (!member || !handle) return null;
  const claimed = await prefs.findOneAndUpdate({ _id: member, greeted: { $ne: true } }, { $set: { greeted: true, greetedAt: now.toISOString() } });
  const row0 = claimed?.value !== undefined ? claimed.value : claimed;   // driver v4 vs v5+ shapes
  if (!row0) return null;
  const row = { _id: id(), owner: "choir|greeter", handle: "@choir", text: `🌼 Welcome ${handle} — say hi!`, tags: [], links: [],
    replyTo: null, when: now.toISOString(), replies: 0, kind: "greeting", welcomes: handle };
  await posts.insertOne(row);
  return row;
}
