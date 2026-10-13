// Mime feed engine — the one consumption surface over the whole network.
//
// Many apps make things (AC, KidLisp, Whistlegraph, No Paint); mime.ac and
// the MIME app watch them here. Every minute the engine re-reads the newest
// work from each source into one in-memory pool; a request ranks that pool
// for its visit: fresh work first, lingered-on work lifted, no author or app
// twice in a row, and the curated No Paint archive braided in so the feed
// never runs dry. Ranking is a pure function of (pool, seed), so a visit's
// pages stay consistent while the pool refreshes underneath.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { ObjectId } from "mongodb";
import { MEDIA_KINDS, MEDIA_THREADS, mediaPipeline, publicPosts } from "./mime-media.mjs";
import { ensureMoodCode, moodIdForCode } from "./mood-code.mjs";
import { ensureChatCode, chatIdForCode } from "./chat-code.mjs";
import { captionOf } from "./whistlegraph.mjs";

const REFRESH_MS = 60_000;
const PER_SOURCE = 200; // newest items read from each live source per refresh
const HALF_LIFE_DAYS = 3;
const LOOKAHEAD = 6; // how far the diversity pass may reach for a different author/app
const WINDOW = 6; // recent cards a kind is damped by
const VARIETY = 0.35; // each recent appearance of a kind multiplies its next score by this
// New work leads every visit: each author's newest card from the last few
// hours, newest first. Whistlegraph pieces count as four times younger, so a
// new mime stays up top for two days instead of half of one.
const LEAD_HOURS = 12;
const LEAD_YOUTH = { whistlegraph: 4 };
// Live and archive slots, repeating: 2.5 live cards per archive card on average.
const BRAID = ["live", "live", "archive", "live", "live", "live", "archive"];
// No Paint styles worth surfacing more often than the abstract majority.
const FAVORED_STYLES = new Set(["portrait", "figurative", "landscape", "mixed", "pattern", "text"]);
const NOPAINT_PIX = "https://pix.nopaint.art/";
// For now the feed shows only work signed with an @handle, so anonymous
// posts and the (unsigned) No Paint archive stay out. Flip to bring them back.
const HANDLED_ONLY = true;
// Whistlegraph threads kept their pre-rename collection.
const WG_THREADS = "walkieware-threads";

// --- Seeded randomness -------------------------------------------------------

/** A stable 0..1 value for (seed, key): the same visit sees the same order. */
export function seeded(seed, key) {
  let hash = 2166136261 >>> 0;
  for (const char of String(seed) + "\u0000" + String(key)) {
    hash ^= char.codePointAt(0);
    hash = Math.imul(hash, 16777619) >>> 0;
  }
  hash ^= hash >>> 15; hash = Math.imul(hash, 2246822507) >>> 0;
  hash ^= hash >>> 13; hash = Math.imul(hash, 3266489909) >>> 0;
  return ((hash ^ (hash >>> 16)) >>> 0) / 4294967296;
}

// --- Ranking (pure) ----------------------------------------------------------

/** Freshness halves every few days; lingering lifts it; a seeded jitter keeps visits distinct. */
export function liveScore(item, seed, now = Date.now()) {
  const ageDays = Math.max(0, (now - new Date(item.when).getTime()) / 86400000);
  const freshness = Math.pow(0.5, ageDays / HALF_LIFE_DAYS) + 0.02;
  const lift = 1 + Math.min(1, (item.dwell || 0) / 10); // up to 2x at 10 s focused per visit
  return freshness * lift * (0.85 + 0.3 * seeded(seed, item.code));
}

/** Favored styles twice for every abstract, each list in the visit's own order. */
export function archiveOrder(archive, seed) {
  const order = (list) => list.map((item) => [seeded(seed, item.code), item]).sort((a, b) => a[0] - b[0]).map(([, item]) => item);
  const favored = order(archive.filter((item) => FAVORED_STYLES.has(item.style)));
  const rest = order(archive.filter((item) => !FAVORED_STYLES.has(item.style)));
  const out = [];
  for (let f = 0, r = 0; f < favored.length || r < rest.length;) {
    if (f < favored.length) out.push(favored[f++]);
    if (f < favored.length) out.push(favored[f++]);
    if (r < rest.length) out.push(rest[r++]);
  }
  return out;
}

/** Take the first item within reach that differs from the last card's author and app. */
function takeDiverse(queue, previous) {
  const differs = (item) => !previous || ((item.author || item.code) !== (previous.author || previous.code) && item.app !== previous.app);
  const reach = Math.min(LOOKAHEAD, queue.length);
  for (let i = 0; i < reach; i++) if (differs(queue[i])) return queue.splice(i, 1)[0];
  for (let i = 0; i < reach; i++) if ((queue[i].author || queue[i].code) !== (previous?.author || previous?.code)) return queue.splice(i, 1)[0];
  return queue.shift();
}

/** The kind a card is (painting, tape, mood, nopaint…), for variety across the feed. */
const kindOf = (item) => item.post?.media?.kind || item.app;

/**
 * Pick the next live card: each kind keeps its own ranked queue, and a kind
 * seen in the last few cards is damped, so a burst of one kind (a day of
 * paintings) cannot bury the others (a week-old mood still gets its turn).
 */
function takeLive(queues, recent, previous) {
  let best = null, bestScore = -1;
  for (const [kind, queue] of queues) {
    if (!queue.length) continue;
    const seen = recent.filter((item) => kindOf(item) === kind).length;
    const head = queue.find((item) => !previous || (item.author || item.code) !== (previous.author || previous.code)) || queue[0];
    const score = head.score * Math.pow(VARIETY, seen) * (kind === kindOf(previous || {}) ? 0.2 : 1);
    if (score > bestScore) { best = [queue, head]; bestScore = score; }
  }
  const [queue, head] = best;
  queue.splice(queue.indexOf(head), 1);
  return head;
}

/** Each author's newest recent card, newest (Whistlegraph counted younger) first. */
export function leadCards(live, now = Date.now()) {
  const age = (item) => (now - new Date(item.when).getTime()) / 3600000 / (LEAD_YOUTH[item.app] || 1);
  const newest = new Map();
  for (const item of live) {
    const hours = age(item), author = item.author || item.code;
    if (hours < 0 || hours > LEAD_HOURS) continue;
    if (!newest.has(author) || hours < newest.get(author)[0]) newest.set(author, [hours, item]);
  }
  return [...newest.values()].sort((a, b) => a[0] - b[0]).map(([, item]) => item);
}

/**
 * The visit's feed up to `count` cards: new work first, then live work ranked
 * within its kind and mixed across kinds, archive braided in, then archive
 * alone once live work runs out.
 */
export function rankFeed({ live, archive }, seed, count, now = Date.now()) {
  const out = leadCards(live, now).slice(0, count);
  const led = new Set(out.map((item) => item.code));
  live = live.filter((item) => !led.has(item.code));
  const queues = new Map();
  for (const item of live) {
    const scored = { ...item, score: liveScore(item, seed, now) };
    const kind = kindOf(item);
    if (!queues.has(kind)) queues.set(kind, []);
    queues.get(kind).push(scored);
  }
  for (const queue of queues.values()) queue.sort((a, b) => b.score - a.score);
  let liveLeft = live.length;
  const archiveQueue = archiveOrder(archive, seed);
  for (let slot = 0; out.length < count && (liveLeft || archiveQueue.length); slot++) {
    const wantLive = BRAID[slot % BRAID.length] === "live" ? liveLeft > 0 : !archiveQueue.length;
    if (wantLive) { out.push(takeLive(queues, out.slice(-WINDOW), out.at(-1))); liveLeft--; }
    else out.push(takeDiverse(archiveQueue, out.at(-1)));
  }
  return out;
}

// --- Sources -----------------------------------------------------------------

const APP_OF = { painting: "aesthetic", tape: "aesthetic", piece: "aesthetic", kidlisp: "kidlisp", whistlegraph: "whistlegraph" };

/** One public post as the client renders it, with ranking fields alongside. */
function feedItem(post, app, extra = {}) {
  return { code: post.code, app, author: post.name || null, when: post.when, post, ...extra };
}

async function liveMedia(db) {
  const streams = await Promise.all(Object.keys(MEDIA_KINDS).map((kind) =>
    db.collection(MEDIA_KINDS[kind]).aggregate([...mediaPipeline(kind, {}, false),
      { $sort: { when: -1 } }, { $limit: PER_SOURCE }]).toArray()));
  const docs = streams.flat();
  const posts = await publicPosts(db, docs);
  return posts.map((post) => feedItem(post, APP_OF[post.media?.kind] || "aesthetic"));
}

async function mimePosts(db) {
  const docs = await db.collection("mimechan").find({ parent: null }, { projection: { "file.data": 0 } })
    .sort({ when: -1 }).limit(PER_SOURCE).toArray();
  return (await publicPosts(db, docs)).map((post) => feedItem(post, "mime"));
}

/**
 * Pieces people made in Whistlegraph and published under their handle. A
 * piece is as new as its latest version, so one still being made stays up
 * top; one published by default (no handle stamped yet) takes its owner's.
 */
async function whistlegraphPieces(db) {
  const rows = await db.collection(WG_THREADS).find(
    { published: true, publishedAt: { $ne: null }, "ledger.head": { $gt: 0 } },
    { projection: { code: 1, owner: 1, handle: 1, publishedAt: 1, updatedAt: 1, "ledger.head": 1, "ledger.versions.id": 1,
      "ledger.versions.request": 1, "ledger.versions.createdAt": 1 } },
  ).sort({ updatedAt: -1 }).limit(PER_SOURCE).toArray();
  const owners = await handleMap(db, rows.filter((row) => !row.handle?.startsWith?.("@")).map((row) => row.owner));
  return rows.filter((row) => typeof row.code === "string" && /^wg[A-Za-z0-9]+$/.test(row.code)).map((row) => {
    const made = (row.ledger?.versions || []).filter((v) => v.id > 0);
    const caption = made.length ? captionOf(made.at(-1).request) : "";
    const handle = typeof row.handle === "string" && row.handle.startsWith("@") ? row.handle : owners.get(row.owner) || null;
    const latest = made.at(-1)?.createdAt;
    const when = latest && latest > row.publishedAt ? latest : row.publishedAt;
    return feedItem(wgPiecePost(row.code, handle, caption.slice(0, 300), when), "whistlegraph");
  });
}

export function wgPiecePost(code, handle, caption, when) {
  return {
    code: "wgpiece_" + code, parent: null, board: "text/javascript", name: handle, text: caption || "", when,
    replies: 0, file: { name: code + ".mjs", type: "text/javascript", size: null, url: "/api/mime-feed?code=" + code },
    origin: { name: "Whistlegraph", url: "https://whistlegraph.app/" },
    media: { kind: "wgpiece", id: code, code, url: "https://aesthetic.computer/mime-play/?code=" + encodeURIComponent(code), author: handle },
  };
}

/** Moods are already public statements on profiles; they come in as words. */
async function moods(db) {
  const rows = await db.collection("moods").find({ deleted: { $ne: true }, mood: { $type: "string", $ne: "" } },
    { projection: { user: 1, mood: 1, when: 1 } }).sort({ when: -1 }).limit(PER_SOURCE).toArray();
  const handles = await handleMap(db, rows.map((row) => row.user));
  return Promise.all(rows.filter((row) => handles.has(row.user)).map(async (row) =>
    feedItem(moodPost(row, handles.get(row.user), await ensureMoodCode(db, row._id)), "mood")));
}

export function moodPost(row, handle, code) {
  return {
    code: "mood_" + String(row._id), parent: null, board: "text/plain", name: handle, text: row.mood,
    when: row.when, replies: 0, file: null, origin: { name: "mood", url: "https://aesthetic.computer/" + encodeURIComponent(handle) },
    media: { kind: "mood", id: String(row._id), code, url: "https://aesthetic.computer/" + encodeURIComponent(handle), author: handle },
  };
}

// Only the two public chats participate; collection names never come from a URL.
const PUBLIC_CHATS = ["system", "clock"];
const PUBLIC_CHAT_TEXT = { deleted: { $ne: true }, text: { $type: "string", $ne: "" } };
async function chatMessages(db) {
  const groups = await Promise.all(PUBLIC_CHATS.map(async instance => {
    const rows = await db.collection("chat-" + instance).find(PUBLIC_CHAT_TEXT,
      { projection: { user: 1, text: 1, when: 1 } }).sort({ when: -1 }).limit(60).toArray();
    const handles = await handleMap(db, rows.map(row => row.user));
    return Promise.all(rows.filter(row => handles.has(row.user)).map(async row =>
      feedItem(chatPost(row, handles.get(row.user), instance, await ensureChatCode(db, instance, row._id)), "chat")));
  }));
  return groups.flat();
}

export function chatPost(row, handle, instance, code) {
  const url = "https://aesthetic.computer/" + (instance === "clock" ? "laer-klokken" : "chat");
  return { code: "chat_" + instance + "_" + String(row._id), parent: null, board: "text/plain", name: handle,
    text: row.text, when: row.when, replies: 0, file: null,
    origin: { name: instance === "clock" ? "laer-klokken" : "chat", url },
    media: { kind: "chat", id: String(row._id), instance, code, url, author: handle } };
}

// Edits/deletes must be read through even while the ranking pool is cached.
export async function currentChatPosts(db, posts) {
  const current = new Map();
  await Promise.all(PUBLIC_CHATS.map(async instance => {
    const cards = posts.filter(p => p.media?.kind === "chat" && p.media.instance === instance);
    if (!cards.length) return;
    const rows = await db.collection("chat-" + instance).find({ ...PUBLIC_CHAT_TEXT,
      _id: { $in: cards.map(p => new ObjectId(p.media.id)) } }).toArray();
    const handles = await handleMap(db, rows.map(row => row.user));
    for (const row of rows) {
      const handle = handles.get(row.user), card = cards.find(p => p.media.id === String(row._id));
      if (handle && card) current.set(card.code, { ...card, ...chatPost(row, handle, instance, card.media.code), replies: card.replies });
    }
  }));
  return posts.flatMap(p => p.media?.kind !== "chat" ? [p] : current.has(p.code) ? [current.get(p.code)] : []);
}

async function handleMap(db, users) {
  const ids = [...new Set(users.filter(Boolean))];
  if (!ids.length) return new Map();
  const rows = await db.collection("@handles").find({ _id: { $in: ids } }, { projection: { handle: 1 } }).toArray();
  return new Map(rows.filter((row) => typeof row.handle === "string").map((row) => [row._id, "@" + row.handle.replace(/^@/, "")]));
}

// The No Paint archive ships in the repo; blanks never enter the feed.
let archiveCache;
export function nopaintArchive(root = process.cwd()) {
  if (archiveCache) return archiveCache;
  const gallery = JSON.parse(readFileSync(join(root, "public", "nopaint.art", "gallery", "gallery.json"), "utf8"));
  archiveCache = gallery.paintings.filter((p) => p.s !== "blank" && /^[A-Za-z0-9]{4,16}$/.test(p.id))
    .map((p) => ({ code: "nopaint_" + p.id, app: "nopaint", author: null, when: p.d, style: p.s, post: nopaintPost(p) }));
  return archiveCache;
}

export function nopaintPost(p) {
  return {
    code: "nopaint_" + p.id, parent: null, board: "image/png", name: null, text: "", when: p.d, replies: 0,
    file: { name: p.id + ".png", type: "image/png", size: null, url: NOPAINT_PIX + p.id + ".png", alt: p.desc || "" },
    origin: { name: "No Paint", url: "https://nopaint.art/" },
    media: { kind: "nopaint", id: p.id, code: p.id, title: p.t || null, url: "https://nopaint.art/gallery/", author: null },
  };
}

/** Average focused seconds per visit for each post (rows are one post in one page visit). */
async function dwellTimes(db) {
  const rows = await db.collection("mime-engagement").aggregate([
    { $group: { _id: "$code", focused: { $sum: "$focusedMs" }, visits: { $sum: 1 } } },
  ]).toArray().catch(() => []);
  return new Map(rows.map((row) => [row._id, row.visits ? row.focused / row.visits / 1000 : 0]));
}

// --- The pool ----------------------------------------------------------------

let pool = null, refreshing = null;

async function refresh(db) {
  const [media, posts, pieces, moodItems, chatItems, dwell, replies] = await Promise.all([
    liveMedia(db), mimePosts(db), whistlegraphPieces(db), moods(db), chatMessages(db), dwellTimes(db),
    db.collection(MEDIA_THREADS).find({}, { projection: { replies: 1 } }).toArray(),
  ]);
  const counts = new Map(replies.map((row) => [row._id, row.replies || 0]));
  const live = [...media, ...posts, ...pieces, ...moodItems, ...chatItems]
    .filter((item) => !HANDLED_ONLY || (typeof item.author === "string" && item.author.startsWith("@")));
  for (const item of live) {
    item.dwell = dwell.get(item.code) || 0;
    if (counts.has(item.code)) item.post.replies = counts.get(item.code);
  }
  const archive = HANDLED_ONLY ? [] : nopaintArchive().map((item) => ({ ...item, dwell: dwell.get(item.code) || 0 }));
  return { live, archive, at: Date.now() };
}

/** The current pool, re-read at most once a minute; a failed refresh keeps the last one. */
export async function feedPool(db) {
  if (pool && Date.now() - pool.at < REFRESH_MS) return pool;
  refreshing ||= refresh(db).then((next) => { pool = next; return next; })
    .catch((error) => { if (!pool) throw error; console.error("mime feed refresh:", error); return pool; })
    .finally(() => { refreshing = null; });
  return pool && Date.now() - pool.at < REFRESH_MS * 5 ? pool : refreshing;
}

/** Re-read the pool on the next request, so a just-published piece shows at once. */
export function freshenFeed() {
  if (pool) pool.at = 0;
}

/** One page of a visit's feed, as client-ready posts. */
export async function feedPage(db, { seed, page, pageSize }) {
  const current = await feedPool(db);
  const end = (page + 1) * pageSize;
  const ranked = rankFeed(current, seed, end + 1);
  return { recent: await currentChatPosts(db, ranked.slice(page * pageSize, end).map((item) => item.post)), hasMore: ranked.length > end };
}

/** Resolve a feed-only thread (archive painting, mood, Whistlegraph piece) for comments and engagement. */
export async function feedOnlyOp(db, code) {
  if (typeof code !== "string") return null;
  if (/^wg[A-Za-z0-9]+$/.test(code)) code = "wgpiece_" + code;
  let match = /^nopaint_([A-Za-z0-9]{4,16})$/.exec(code);
  if (match) {
    const item = nopaintArchive().find((entry) => entry.code === code);
    return item ? withThread(db, item.post) : null;
  }
  const moodAlias = /^mood~([A-Za-z0-9]{3,16})$/.exec(code);
  if (moodAlias) {
    const id = await moodIdForCode(db, moodAlias[1]);
    if (!id) return null;
    code = "mood_" + id;
  }
  match = /^mood_([a-f0-9]{24})$/.exec(code);
  if (match) {
    const row = await db.collection("moods").findOne({ _id: new ObjectId(match[1]), deleted: { $ne: true } });
    const handle = row && (await handleMap(db, [row.user])).get(row.user);
    return handle ? withThread(db, moodPost(row, handle, await ensureMoodCode(db, row._id))) : null;
  }
  const chatAlias = /^chat~([A-Za-z0-9]{3,16})$/.exec(code);
  if (chatAlias) {
    const record = await chatIdForCode(db, chatAlias[1]);
    if (!record) return null;
    code = "chat_" + record.instance + "_" + record.id;
  }
  match = /^chat_(system|clock)_([a-f0-9]{24})$/.exec(code);
  if (match) {
    const instance = match[1];
    const row = await db.collection("chat-" + instance).findOne({ ...PUBLIC_CHAT_TEXT, _id: new ObjectId(match[2]) });
    const handle = row && (await handleMap(db, [row.user])).get(row.user);
    return handle ? withThread(db, chatPost(row, handle, instance, await ensureChatCode(db, instance, row._id))) : null;
  }
  match = /^wgpiece_(wg[A-Za-z0-9]+)$/.exec(code);
  if (match) {
    const pieces = (await feedPool(db)).live.filter((item) => item.code === code);
    if (pieces[0]) return withThread(db, pieces[0].post);
    const row = await db.collection(WG_THREADS).findOne({ code: match[1], published: true }, { projection: { code: 1, handle: 1, publishedAt: 1 } });
    return row ? withThread(db, wgPiecePost(row.code, row.handle?.startsWith?.("@") ? row.handle : null, "", row.publishedAt)) : null;
  }
  return null;
}

async function withThread(db, post) {
  const thread = await db.collection(MEDIA_THREADS).findOne({ _id: post.code });
  return { ...post, replies: thread?.replies || 0, _media: { kind: post.media.kind, feedOnly: true }, _public: true };
}
