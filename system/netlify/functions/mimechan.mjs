// Mimechan, 2026.09.02
// An imageboard where the boards are mimetypes. Post any file and
// its type decides where it lands — image/gif is the gif board,
// text/html is a board of webpages, audio/midi is a board of tunes.
// GET reads (boards, threads, files), POST writes. Anonymous by default.

/* #region 🏁 TODO
  - [] Rate limiting + board pruning once traffic is real.
  - [] Move file bytes from Mongo to Spaces when sizes grow.
#endregion */

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";

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
  lisp: "text/x-lisp", png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg",
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

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});
  const { db } = await connect();
  const posts = db.collection("mimechan");
  const q = event.queryStringParameters || {};

  try {
    if (event.httpMethod === "GET") {
      if (q.file) return await serveFile(posts, q.file);
      if (q.thread) return await getThread(posts, q.thread);
      if (q.board) return await getBoard(posts, q.board, parseInt(q.page) || 0);
      return await getIndex(posts);
    }
    if (event.httpMethod === "POST") {
      await ensureIndexes(posts);
      return await createPost(posts, JSON.parse(event.body || "{}"));
    }
    return respond(405, { error: "method not allowed" });
  } catch (err) {
    console.error("🎭 mimechan:", err);
    return respond(500, { error: err.message });
  }
}

// Serve stored bytes with the file's own Content-Type. `sandbox` CSP
// neuters scripts when html/svg is opened as a document — without it
// any posted webpage would run on this origin.
async function serveFile(posts, code) {
  const doc = await posts.findOne({ code });
  if (!doc?.file?.data) return respond(404, { error: "no file" });
  return {
    statusCode: 200,
    headers: {
      "Content-Type": doc.file.type,
      "Content-Disposition": `inline; filename="${(doc.file.name || code).replace(/["\\\r\n]/g, "")}"`,
      "Content-Security-Policy": "sandbox",
      "X-Content-Type-Options": "nosniff",
      "Cache-Control": "public, max-age=31536000, immutable",
      "Access-Control-Allow-Origin": "*",
    },
    body: doc.file.data,
    isBase64Encoded: true,
  };
}

async function getIndex(posts) {
  const boards = await posts
    .aggregate([
      { $match: { parent: null } },
      { $group: { _id: "$board", threads: { $sum: 1 }, bumped: { $max: "$bumped" } } },
      { $sort: { bumped: -1 } },
    ])
    .toArray();
  const recent = await posts
    .find({ file: { $ne: null } }, NO_DATA)
    .sort({ when: -1 })
    .limit(12)
    .toArray();
  return respond(200, {
    boards: boards.map((b) => ({ board: b._id, threads: b.threads, bumped: b.bumped })),
    recent: recent.map(publicPost),
  });
}

async function getBoard(posts, board, page) {
  const ops = await posts
    .find({ board, parent: null }, NO_DATA)
    .sort({ bumped: -1 })
    .skip(page * PAGE_SIZE)
    .limit(PAGE_SIZE)
    .toArray();
  const threads = [];
  for (const op of ops) {
    const tail = await posts
      .find({ parent: op.code }, NO_DATA)
      .sort({ when: -1 })
      .limit(3)
      .toArray();
    threads.push({ op: publicPost(op), replies: tail.reverse().map(publicPost), replyCount: op.replies || 0 });
  }
  return respond(200, { board, page, threads });
}

async function getThread(posts, code) {
  const op = await posts.findOne({ code, parent: null }, NO_DATA);
  if (!op) return respond(404, { error: "no thread" });
  const replies = await posts.find({ parent: code }, NO_DATA).sort({ when: 1 }).toArray();
  return respond(200, { op: publicPost(op), replies: replies.map(publicPost) });
}

async function createPost(posts, body) {
  const { parent } = body;
  const text = (body.text || "").slice(0, MAX_TEXT).trim();
  const name = (body.name || "").slice(0, MAX_NAME).trim() || null;

  let file = null;
  if (body.file?.data) {
    const data = body.file.data.replace(/\s/g, "");
    const bytes = Buffer.from(data, "base64");
    if (bytes.length === 0) return respond(400, { error: "empty file" });
    if (bytes.length > MAX_FILE) return respond(400, { error: `file over ${MAX_FILE / 1024 / 1024}MB` });
    file = {
      name: (body.file.name || "file").slice(0, 128),
      type: sniffType(body.file.type, body.file.name),
      size: bytes.length,
      data,
    };
  }

  let board;
  if (parent) {
    if (!text && !file) return respond(400, { error: "empty reply" });
    const op = await posts.findOne({ code: parent, parent: null }, NO_DATA);
    if (!op) return respond(404, { error: "no such thread" });
    board = op.board;
  } else {
    if (!file) return respond(400, { error: "a thread starts with a file — the file picks the board" });
    board = file.type;
  }

  const now = new Date();
  const code = await generateUniqueCode(posts);
  const doc = { code, parent: parent || null, board, name, text, file, when: now };
  if (!parent) {
    doc.bumped = now;
    doc.replies = 0;
  }
  await posts.insertOne(doc);

  if (parent) {
    const op = await posts.findOne({ code: parent }, { projection: { replies: 1 } });
    const update = { $inc: { replies: 1 } };
    if ((op?.replies || 0) < BUMP_LIMIT) update.$set = { bumped: now };
    await posts.updateOne({ code: parent }, update);
  }

  return respond(200, { code, board, parent: parent || null });
}

function publicPost(doc) {
  return {
    code: doc.code,
    parent: doc.parent,
    board: doc.board,
    name: doc.name,
    text: doc.text,
    when: doc.when,
    replies: doc.replies,
    file: doc.file
      ? { name: doc.file.name, type: doc.file.type, size: doc.file.size, url: `/api/mimechan?file=${doc.code}` }
      : null,
  };
}
