// menuband-takes.mjs — Menu Band recordings, backed up under your handle.
//
// Menu Band (slab/menuband) uploads every take it drops on the Desktop here
// so it can be browsed from the `menuband` piece, the menuband MCP tools and
// Aesel on any machine. Takes are PRIVATE: every call is owner-only, objects
// are written with a private ACL, and reads go through 1-hour presigned GETs.
//
// POST   { action: "begin", takeId, recordedAt, machine, duration, bpm?, program?, files: [{ name, bytes }] }
//          → { code, committed, uploads: [{ name, url, method, headers }] }
// PUT    each file to its url with exactly those headers.
// POST   { action: "commit", code } → { code, committed: true } | 409 { missing }
// GET    ?limit=50&before=<ISO>     → { handle, count, takes: [Take] }
// GET    ?code=X                    → { take: Take }
//        Each Take file carries `url` (inline) and `download` (attachment),
//        both presigned GETs that last an hour.
// DELETE ?code=X                    → { code, deleted: true }
//
// Storage: USER_SPACE_NAME bucket, key `<sub>/menuband/<code>/<name>`.
// Mongo: `menuband-takes` { user, code, takeId, recordedAt, machine, duration,
//        bpm, program, files: { name: { key, bytes, contentType } }, committed, when, nuked? }

import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import {
  S3Client,
  PutObjectCommand,
  GetObjectCommand,
  HeadObjectCommand,
  DeleteObjectsCommand,
} from "@aws-sdk/client-s3";
import { authorize, hasAdmin, handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";

const COLLECTION = "menuband-takes";
const MAX_BYTES = 96 * 1024 * 1024;
const URL_SECONDS = 3600;
const DEFAULT_LIMIT = 50;
const MAX_LIMIT = 200;
const METHODS = "GET, POST, DELETE, OPTIONS";

// The only files a take may carry, and what each one is.
export const TAKE_FILES = {
  "mix.mp3": "audio/mpeg",
  "mix.wav": "audio/wav",
  "tones.wav": "audio/wav",
  "percussion.wav": "audio/wav",
  "voice.wav": "audio/wav",
  "notes.mid": "audio/midi",
  "mix.json": "application/json",
  "cover.png": "image/png",
};

const reply = (status, body) =>
  respond(status, body, { "Access-Control-Allow-Methods": METHODS });

let s3;
function storage() {
  const accessKeyId = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const secretAccessKey = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  if (!accessKeyId || !secretAccessKey) return null;
  s3 ||= new S3Client({
    endpoint: "https://" + (process.env.USER_ENDPOINT || "sfo3.digitaloceanspaces.com"),
    region: "us-east-1",
    credentials: { accessKeyId, secretAccessKey },
    // Otherwise presigned PUTs carry a CRC32 of an empty body and a real
    // upload fails its checksum.
    requestChecksumCalculation: "WHEN_REQUIRED",
  });
  return { s3, bucket: process.env.USER_SPACE_NAME || "user-aesthetic-computer" };
}

export const objectKey = (sub, code, name) => `${sub}/menuband/${code}/${name}`;

// Validate a `begin` body into the record fields it may set. Throws with a
// message fit for a 400.
export function parseBegin(body) {
  const takeId = typeof body.takeId === "string" ? body.takeId.trim() : "";
  if (!takeId || takeId.length > 128) throw new Error("takeId is required (≤128 chars)");
  const recordedAt = new Date(body.recordedAt ?? Date.now());
  if (Number.isNaN(recordedAt.getTime())) throw new Error("recordedAt must be a date");
  const duration = Number(body.duration);
  if (!Number.isFinite(duration) || duration < 0 || duration > 3600)
    throw new Error("duration must be seconds");
  const optional = (value) => (Number.isFinite(Number(value)) && value !== null ? Number(value) : null);
  if (!Array.isArray(body.files) || body.files.length === 0)
    throw new Error("files must list at least one file");
  const files = {};
  for (const file of body.files) {
    const contentType = TAKE_FILES[file?.name];
    if (!contentType) throw new Error(`Unsupported file: ${file?.name}`);
    const bytes = Number(file.bytes);
    if (!Number.isInteger(bytes) || bytes <= 0 || bytes > MAX_BYTES)
      throw new Error(`${file.name} must be 1..${MAX_BYTES} bytes`);
    files[file.name] = { bytes, contentType };
  }
  return {
    takeId,
    recordedAt,
    machine: String(body.machine || "").slice(0, 64) || null,
    duration,
    bpm: optional(body.bpm),
    program: optional(body.program),
    files,
  };
}

async function presignPut({ s3, bucket }, key, contentType) {
  const command = new PutObjectCommand({ Bucket: bucket, Key: key, ContentType: contentType, ACL: "private" });
  // Keep the ACL and type as signed headers (not query params) so the client
  // must send exactly these and can't swap them for public-read.
  const url = await getSignedUrl(s3, command, {
    expiresIn: URL_SECONDS,
    signableHeaders: new Set(["content-type", "x-amz-acl"]),
    unhoistableHeaders: new Set(["x-amz-acl"]),
  });
  return { url, method: "PUT", headers: { "Content-Type": contentType, "x-amz-acl": "private" } };
}

async function presignGet({ s3, bucket }, key, name, code, disposition = "inline") {
  const command = new GetObjectCommand({
    Bucket: bucket,
    Key: key,
    ResponseContentDisposition: `${disposition}; filename="menuband-${code}-${name}"`,
  });
  return getSignedUrl(s3, command, { expiresIn: URL_SECONDS });
}

async function present(store, doc) {
  const files = {};
  for (const [name, file] of Object.entries(doc.files || {})) {
    files[name] = {
      url: await presignGet(store, file.key, name, doc.code),
      download: await presignGet(store, file.key, name, doc.code, "attachment"),
      bytes: file.bytes,
      contentType: file.contentType,
    };
  }
  return {
    code: doc.code,
    takeId: doc.takeId,
    recordedAt: doc.recordedAt,
    machine: doc.machine,
    duration: doc.duration,
    bpm: doc.bpm,
    program: doc.program,
    files,
  };
}

async function begin(store, takes, user, body) {
  let fields;
  try { fields = parseBegin(body); }
  catch (error) { return reply(400, { error: error.message }); }

  const existing = await takes.findOne({ user: user.sub, takeId: fields.takeId, nuked: { $ne: true } });
  if (existing?.committed) return reply(200, { code: existing.code, committed: true, uploads: [] });

  const code = existing?.code || (await generateUniqueCode(takes, { type: "menuband" }));
  const files = {};
  for (const [name, file] of Object.entries(fields.files))
    files[name] = { ...file, key: objectKey(user.sub, code, name) };

  await takes.updateOne(
    { user: user.sub, takeId: fields.takeId },
    {
      $set: { ...fields, files, code, committed: false, when: new Date() },
      $unset: { nuked: "" },
    },
    { upsert: true },
  );

  const uploads = [];
  for (const [name, file] of Object.entries(files))
    uploads.push({ name, ...(await presignPut(store, file.key, file.contentType)) });
  return reply(200, { code, committed: false, uploads });
}

async function commit({ s3, bucket }, takes, user, body) {
  const code = String(body.code || "");
  const doc = await takes.findOne({ user: user.sub, code, nuked: { $ne: true } });
  if (!doc) return reply(404, { error: "No such take" });

  const missing = [];
  const files = { ...doc.files };
  for (const [name, file] of Object.entries(files)) {
    try {
      const head = await s3.send(new HeadObjectCommand({ Bucket: bucket, Key: file.key }));
      files[name] = { ...file, bytes: head.ContentLength ?? file.bytes };
    } catch {
      missing.push(name);
    }
  }
  if (missing.length) return reply(409, { error: "Files not uploaded", missing });

  await takes.updateOne({ _id: doc._id }, { $set: { files, committed: true, when: new Date() } });
  return reply(200, { code, committed: true });
}

async function list(store, takes, user, params) {
  if (params.code) {
    const doc = await takes.findOne({ user: user.sub, code: params.code, committed: true, nuked: { $ne: true } });
    if (!doc) return reply(404, { error: "No such take" });
    return reply(200, { take: await present(store, doc) });
  }
  const limit = Math.min(MAX_LIMIT, Math.max(1, parseInt(params.limit, 10) || DEFAULT_LIMIT));
  const query = { user: user.sub, committed: true, nuked: { $ne: true } };
  if (params.before) {
    const before = new Date(params.before);
    if (!Number.isNaN(before.getTime())) query.recordedAt = { $lt: before };
  }
  const docs = await takes.find(query).sort({ recordedAt: -1 }).limit(limit).toArray();
  const presented = [];
  for (const doc of docs) presented.push(await present(store, doc));
  return reply(200, { handle: "@" + (await handleFor(user.sub)), count: presented.length, takes: presented });
}

async function remove({ s3, bucket }, takes, user, params) {
  const code = String(params.code || "");
  const doc = await takes.findOne({ user: user.sub, code, nuked: { $ne: true } });
  if (!doc) return reply(404, { error: "No such take" });
  const objects = Object.values(doc.files || {}).map((file) => ({ Key: file.key }));
  if (objects.length)
    await s3.send(new DeleteObjectsCommand({ Bucket: bucket, Delete: { Objects: objects, Quiet: true } }));
  await takes.updateOne({ _id: doc._id }, { $set: { nuked: true, when: new Date() } });
  return reply(200, { code, deleted: true });
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return reply(204, "");
  if (!["GET", "POST", "DELETE"].includes(event.httpMethod))
    return reply(405, { error: "Method not allowed" });

  const user = await authorize(event.headers);
  if (!user?.sub) return reply(401, { error: "Log in to reach your Menu Band takes" });

  // Admin only (@jeffrey's verified ADMIN_SUB) while the feature is greenlit
  // for him alone.
  if (!(await hasAdmin(user)))
    return reply(403, { error: "Menu Band backup isn't open to your account yet" });

  const store = storage();
  if (!store) return reply(503, { error: "Take storage is not configured" });

  let database;
  try {
    database = await connect();
    const takes = database.db.collection(COLLECTION);
    await takes.createIndex({ user: 1, takeId: 1 }, { unique: true });
    await takes.createIndex({ user: 1, recordedAt: -1 });
    await takes.createIndex({ code: 1 }, { unique: true });

    const params = event.queryStringParameters || {};
    if (event.httpMethod === "GET") return await list(store, takes, user, params);
    if (event.httpMethod === "DELETE") return await remove(store, takes, user, params);

    let body;
    try { body = JSON.parse(event.body || "{}"); }
    catch { return reply(400, { error: "Body must be JSON" }); }
    if (body.action === "begin") return await begin(store, takes, user, body);
    if (body.action === "commit") return await commit(store, takes, user, body);
    return reply(400, { error: "action must be begin or commit" });
  } catch (error) {
    console.error("[menuband-takes] error:", error);
    return reply(500, { error: error.message || "Internal error" });
  } finally {
    await database?.disconnect?.();
  }
}
