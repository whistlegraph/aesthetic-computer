// Mime — discussion on public AC media, organized by MIME type.
import { connect } from "../../backend/database.mjs";
import { authorize } from "../../backend/authorization.mjs";
import { respond as httpRespond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import {
  MEDIA_KINDS, MEDIA_THREADS, mediaPipeline, parseMediaThread,
  mediaThread, resolveMedia, sourceRecord, publicPosts, mediaFile,
} from "../../backend/mime-media.mjs";

const respond = (status, body, headers = {}) => httpRespond(status, body, { "Cache-Control": "no-store", ...headers });

const MAX_FILE = 8 * 1024 * 1024; // 8MB raw bytes (docs must stay under mongo's 16MB).
const MAX_TEXT = 4000;
const MAX_NAME = 40;
const BUMP_LIMIT = 300; // Replies past this no longer bump the thread.
const PAGE_SIZE = 12;

// `type/subtype` per RFC 6838 token rules, lowercased before checking.
const MIME_RX = /^[a-z0-9][a-z0-9!#$&^_+.-]{0,63}\/[a-z0-9][a-z0-9!#$&^_+.*-]{0,63}$/;

// Fallbacks for files the browser hands over with an empty type.
const EXT_TYPES = {
  txt: "text/plain", md: "text/markdown", html: "text/html", css: "text/css",
  js: "text/javascript", mjs: "text/javascript", json: "application/json",
  lisp: "text/x-lisp", lua: "text/x-lua", png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg",
  gif: "image/gif", webp: "image/webp", svg: "image/svg+xml", mp3: "audio/mpeg",
  wav: "audio/wav", ogg: "audio/ogg", mid: "audio/midi", midi: "audio/midi",
  mp4: "video/mp4", webm: "video/webm", mov: "video/quicktime",
  pdf: "application/pdf", zip: "application/zip", wasm: "application/wasm",
  ttf: "font/ttf", otf: "font/otf", woff: "font/woff", woff2: "font/woff2",
};

const NO_DATA = { projection: { "file.data": 0 } }; // Never ship bytes in listings.

function sniffType(declared, filename) {
  const t = (declared || "").toLowerCase().trim();
  if (MIME_RX.test(t)) return t;
  const ext = (filename || "").split(".").pop()?.toLowerCase();
  return EXT_TYPES[ext] || "application/octet-stream";
}

async function ensureIndexes(collection) {
  await collection.createIndex({ code: 1 }, { unique: true, name: "mimechan_code" });
  await collection.createIndex({ board: 1, parent: 1, bumped: -1 }, { name: "mimechan_board_bump" });
  await collection.createIndex({ parent: 1, when: 1 }, { name: "mimechan_thread" });
}

export function createHandler(connectDb = connect, authorizeUser = authorize) {
  return async function handler(event) {
    if (event.httpMethod === "OPTIONS") return respond(200, {});
    if (!["GET", "POST"].includes(event.httpMethod)) return respond(405, { error: "method not allowed" });
    const q = event.queryStringParameters || {};
    try {
      const { db } = await connectDb();
      // Preserve existing uploads, replies and short codes through the rename.
      const posts = db.collection("mimechan");
      const authorization = event.headers?.authorization || event.headers?.Authorization;
      let user;
      if (authorization && (event.httpMethod === "POST" || q.me)) {
        user = await authorizeUser({ authorization });
        if (!user?.sub) return respond(401, { error: "sign in again to comment" });
      }
      if (event.httpMethod === "GET") {
        if (q.me) {
          const account = user ? await db.collection("@handles").findOne({ _id: user.sub }) : null;
          return respond(200, { handle: account?.handle ? "@" + account.handle.replace(/^@/, "") : null });
        }
        if (q.file) return await serveFile(db, posts, q.file);
        if (q.media) {
          const op = await resolveMedia(db, q.media, q.code);
          return op ? await getThread(db, posts, op.code) : respond(404, { error: "media not found" });
        }
        if (q.thread) return await getThread(db, posts, q.thread);
        const page = Math.max(0, Math.min(10000, parseInt(q.page) || 0));
        return q.board ? await getBoard(db, posts, q.board, page) : await getIndex(db, posts, page);
      }
      let body;
      try { body = JSON.parse(event.body || "{}"); }
      catch { return respond(400, { error: "invalid JSON" }); }
      if (!body || typeof body !== "object" || Array.isArray(body)) return respond(400, { error: "invalid post" });
      if (q.engagement === "1") return await recordEngagement(db, posts, body);
      await ensureIndexes(posts);
      return await createPost(db, posts, body, user);
    } catch (err) {
      console.error("mime:", err);
      return respond(500, { error: "mime unavailable" });
    }
  };
}
export const handler = createHandler();

async function findOp(db, posts, code) {
  const ref = parseMediaThread(code);
  return ref ? mediaThread(db, ref.kind, { _id: ref.id }) : posts.findOne({ code, parent: null }, NO_DATA);
}

async function serveFile(db, posts, code) {
  const ref = parseMediaThread(code);
  if (ref) {
    const op = await findOp(db, posts, code);
    if (!op) return respond(404, { error: "media not found" });
    const record = await sourceRecord(db, code);
    if (!record) return respond(404, { error: "media not found" });
    const file = mediaFile(ref.kind, record);
    const headers = { "Cache-Control": "no-store", "Content-Security-Policy": "sandbox", "X-Content-Type-Options": "nosniff" };
    return file.url ? respond(302, "", { ...headers, Location: file.url })
      : respond(200, file.text, { ...headers, "Content-Type": op.board + "; charset=utf-8" });
  }
  const doc = await posts.findOne({ code });
  if (!doc?.file?.data) return respond(404, { error: "no file" });
  if (doc.parent && !await findOp(db, posts, doc.parent)) return respond(404, { error: "no thread" });
  return {
    statusCode: 200,
    headers: {
      "Content-Type": doc.file.type,
      "Content-Disposition": `inline; filename="${(doc.file.name || code).replace(/["\\\r\n]/g, "")}"`,
      "Content-Security-Policy": "sandbox",
      "X-Content-Type-Options": "nosniff",
      "Cache-Control": doc.parent ? "no-store" : "public, max-age=31536000, immutable",
      "Access-Control-Allow-Origin": "*",
    },
    body: doc.file.data, isBase64Encoded: true,
  };
}

function allRoots(activity = true, board) {
  return [
    { $match: { parent: null, ...(board ? { board } : {}) } },
    { $project: { "file.data": 0 } },
    ...Object.entries(MEDIA_KINDS).map(([kind, coll]) => {
      const pipeline = mediaPipeline(kind, {}, activity);
      if (board) pipeline.splice(2, 0, { $match: { board } });
      return { $unionWith: { coll, pipeline } };
    }),
  ];
}

async function getIndex(db, posts, page) {
  const end = (page + 1) * PAGE_SIZE;
  const order = [{ $sort: { when: -1, code: 1 } }, { $limit: end + 1 }];
  const [boards, streams] = await Promise.all([
    posts.aggregate([
      ...allRoots(false),
      { $group: { _id: "$board", threads: { $sum: 1 }, bumped: { $max: "$bumped" } } },
      { $sort: { bumped: -1, _id: 1 } },
    ]).toArray(),
    Promise.all([
      ...["painting", "tape", "kidlisp", "piece"].map((kind) =>
        db.collection(MEDIA_KINDS[kind]).aggregate([...mediaPipeline(kind, {}, false), ...order]).toArray()),
      posts.aggregate([{ $match: { parent: null } }, { $project: { "file.data": 0 } }, ...order]).toArray(),
    ]),
  ]);
  // Round-robin newest-first streams so a burst of one format cannot bury
  // the others. Take enough from each stream to preserve page boundaries.
  const mixed = [];
  for (let i = 0; mixed.length <= end && streams.some((stream) => i < stream.length); i++) {
    for (const stream of streams) if (stream[i]) mixed.push(stream[i]);
  }
  const recent = mixed.slice(page * PAGE_SIZE, end + 1);
  // Resolve activity only for the requested page.
  const ids = recent.filter((p) => p._media).map((p) => p.code);
  const activity = ids.length ? await db.collection(MEDIA_THREADS).find({ _id: { $in: ids } }).toArray() : [];
  const counts = new Map(activity.map((p) => [p._id, p.replies]));
  for (const p of recent) if (p._media) p.replies = counts.get(p.code) || 0;
  const publicRecent = await publicPosts(db, recent.slice(0, PAGE_SIZE));
  await Promise.all(publicRecent.map(async (op) => {
    if (!op.replies) return;
    const first = await posts.find({ parent: op.code }, NO_DATA).sort({ when: 1, _id: 1 }).limit(1).toArray();
    op.preview = (await publicPosts(db, first)).map(({ name, text }) => ({ name, text }));
  }));
  return respond(200, {
    boards: boards.map((b) => ({ board: b._id, threads: b.threads, bumped: b.bumped })),
    recent: publicRecent,
    page, hasMore: recent.length > PAGE_SIZE,
  }, { "Cache-Control": "no-store" });
}

async function getBoard(db, posts, board, page) {
  const ops = await posts.aggregate([
    ...allRoots(true, board), { $sort: { bumped: -1, code: 1 } },
    { $skip: page * PAGE_SIZE }, { $limit: PAGE_SIZE + 1 },
  ]).toArray();
  const publicOps = await publicPosts(db, ops.slice(0, PAGE_SIZE));
  const threads = await Promise.all(publicOps.map(async (op) => {
    const tail = await posts.find({ parent: op.code }, NO_DATA).sort({ when: -1, _id: -1 }).limit(3).toArray();
    return { op, replies: await publicPosts(db, tail.reverse()), replyCount: op.replies };
  }));
  return respond(200, { board, page, threads, hasMore: ops.length > PAGE_SIZE }, { "Cache-Control": "no-store" });
}

async function getThread(db, posts, code) {
  const op = await findOp(db, posts, code);
  if (!op) return respond(404, { error: "thread not found" });
  const replies = await posts.find({ parent: code }, NO_DATA).sort({ when: 1, _id: 1 }).toArray();
  const [publicOp, ...publicReplies] = await publicPosts(db, [op, ...replies]);
  return respond(200, { op: publicOp, replies: publicReplies, metadata: { engagement: await engagementTotals(db, code) } }, { "Cache-Control": "no-store" });
}

// Anonymous, cumulative counters per page visit and post. Replays and retries
// use $max, so concurrent/unload deliveries cannot double-count time.
const ENGAGEMENT = "mime-engagement";
const TIME_FIELDS = ["visibleMs", "partialMs", "majorityMs", "focusedMs", "weightedVisibleMs"];
const ENGAGEMENT_FIELDS = [...TIME_FIELDS, "maxVisiblePermille", "commentOpens", "originalOpens"];
async function recordEngagement(db, posts, body) {
  if (typeof body.visit !== "string" || !/^[a-f0-9-]{36}$/.test(body.visit) ||
      !Array.isArray(body.posts) || !body.posts.length || body.posts.length > 24) {
    return respond(400, { error: "invalid engagement" });
  }
  const codes = new Set();
  for (const p of body.posts) {
    if (!p || typeof p.code !== "string" || p.code.length > 100 || codes.has(p.code) ||
        ENGAGEMENT_FIELDS.some((key) => !Number.isSafeInteger(p[key]) || p[key] < 0 ||
          p[key] > (TIME_FIELDS.includes(key) ? 86400000 : key === "maxVisiblePermille" ? 1000 : 10000)) ||
        p.partialMs + p.majorityMs !== p.visibleMs || p.focusedMs > p.visibleMs || p.weightedVisibleMs > p.visibleMs) {
      return respond(400, { error: "invalid engagement counters" });
    }
    codes.add(p.code);
  }
  const collection = db.collection(ENGAGEMENT);
  await collection.createIndex({ code: 1 });
  for (const p of body.posts) {
    if (!await findOp(db, posts, p.code)) continue;
    const counters = Object.fromEntries(ENGAGEMENT_FIELDS.map((key) => [key, p[key]]));
    const _id = p.code + ":" + body.visit;
    try {
      await collection.updateOne({ _id }, {
        $setOnInsert: { code: p.code }, $max: counters,
      }, { upsert: true });
    } catch (error) {
      if (error.code !== 11000) throw error;
      await collection.updateOne({ _id }, { $max: counters });
    }
  }
  return respond(200, { ok: true });
}
async function engagementTotals(db, code) {
  const [totals] = await db.collection(ENGAGEMENT).aggregate([
    { $match: { code } },
    { $group: { _id: null,
      ...Object.fromEntries([...TIME_FIELDS, "commentOpens", "originalOpens"].map((key) => [key, { $sum: "$" + key }])),
      maxVisiblePermille: { $max: "$maxVisiblePermille" },
      impressions: { $sum: { $cond: [{ $gte: ["$majorityMs", 1000] }, 1, 0] } },
    } }, { $project: { _id: 0 } },
  ]).toArray();
  return { version: 1, ...(totals || Object.fromEntries([...ENGAGEMENT_FIELDS, "impressions"].map((key) => [key, 0]))) };
}

async function createPost(db, posts, body, user) {
  if (body.parent != null && typeof body.parent !== "string") return respond(400, { error: "invalid thread" });
  if (body.text != null && typeof body.text !== "string") return respond(400, { error: "invalid comment" });
  if (body.name != null && typeof body.name !== "string") return respond(400, { error: "invalid name" });
  const parent = body.parent || null;
  if (parent && body.file != null) return respond(400, { error: "comments are text only" });
  const text = (body.text || "").slice(0, MAX_TEXT).trim();
  const name = user ? null : (body.name || "").slice(0, MAX_NAME).trim() || null;
  let file = null;
  if (body.file != null) {
    const f = body.file;
    if (typeof f !== "object" || typeof f.data !== "string" || typeof f.name !== "string" ||
      (f.type != null && typeof f.type !== "string")) return respond(400, { error: "invalid file" });
    const data = f.data.replace(/\s/g, "");
    const bytes = Buffer.from(data, "base64");
    if (bytes.length === 0) return respond(400, { error: "empty file" });
    if (bytes.length > MAX_FILE) return respond(400, { error: "file over 8 MB" });
    file = { name: (f.name || "file").slice(0, 128), type: sniffType(f.type, f.name), size: bytes.length, data };
  }
  const op = parent ? await findOp(db, posts, parent) : null;
  if (parent && !op) return respond(404, { error: "thread not found" });
  if (parent && !text) return respond(400, { error: "empty reply" });
  if (!parent && !file) return respond(400, { error: "choose a file" });
  const board = op ? op.board : file.type;
  const now = new Date();
  let code;
  const doc = { parent, board, name, text, file, when: now };
  if (user) doc.user = user.sub;
  if (!parent) { doc.bumped = now; doc.replies = 0; }
  for (let attempt = 0; attempt < 5; attempt++) {
    code = await generateUniqueCode(posts);
    try { await posts.insertOne({ ...doc, code }); break; }
    catch (error) {
      if (error.code !== 11000 || attempt === 4) throw error;
    }
  }
  if (parent) {
    // Pipeline updates keep simultaneous first replies on one stable thread.
    // Metadata and file bytes are never copied from the source collection.
    const target = op._media ? db.collection(MEDIA_THREADS) : posts;
    const query = op._media ? { _id: parent } : { code: parent };
    await target.updateOne(query, [{ $set: {
      replies: { $add: [{ $ifNull: ["$replies", 0] }, 1] },
      bumped: { $cond: [{ $lt: [{ $ifNull: ["$replies", 0] }, BUMP_LIMIT] }, { $max: ["$bumped", now] }, "$bumped"] },
    } }], { upsert: !!op._media });
  }
  return respond(200, { code, board, parent });
}
