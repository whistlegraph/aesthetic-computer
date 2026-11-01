// nelabel-drop.mjs — the nela bel sampler drop box.
//
// Club members send a short clip of the track they'd put on the release,
// with a working title, the full track's rough length and when they could
// hand in a master. No sign-in: the club is Signal-only and has no AC
// accounts. Clips are PRIVATE (owner-only presigned reads); the list of who
// dropped what is public so the page doubles as the tracklist-in-progress.
//
// POST   { action: "begin", word?, who, title, length, eta, note?, file: { name, bytes } }
//          → { code, upload: { url, method, headers } }
// PUT    the clip to upload.url with exactly those headers.
// POST   { action: "commit", code } → { code, committed: true } | 409
// GET    → { count, drops: [{ code, who, title, length, eta, note, when }] }
//        Admin (Authorization: Bearer …, @jeffrey) also gets `url` per drop.
// DELETE ?code=X (admin) → { code, deleted: true }
//
// NELABEL_DROP_WORD, when set, is a shared passphrase `begin` must carry.
// Storage: USER_SPACE_NAME bucket, key `nelabel/sampler/<code>/<file>`.
// Mongo: `nelabel-drops` { code, who, title, length, eta, note, file: { name,
//        key, bytes, contentType }, committed, when, nuked? }

import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import {
  S3Client,
  PutObjectCommand,
  GetObjectCommand,
  HeadObjectCommand,
  DeleteObjectCommand,
} from "@aws-sdk/client-s3";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";

const COLLECTION = "nelabel-drops";
const MAX_BYTES = 48 * 1024 * 1024;
const URL_SECONDS = 3600;
const METHODS = "GET, POST, DELETE, OPTIONS";

// Clip formats the drop accepts, by extension.
export const CLIP_TYPES = {
  wav: "audio/wav",
  flac: "audio/flac",
  mp3: "audio/mpeg",
  m4a: "audio/mp4",
  aif: "audio/aiff",
  aiff: "audio/aiff",
  ogg: "audio/ogg",
  opus: "audio/ogg",
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
    requestChecksumCalculation: "WHEN_REQUIRED",
  });
  return { s3, bucket: process.env.USER_SPACE_NAME || "user-aesthetic-computer" };
}

export const objectKey = (code, name) => `nelabel/sampler/${code}/${name}`;

const text = (value, max) => String(value ?? "").trim().replace(/\s+/g, " ").slice(0, max);

// A filename safe to use as an object key, keeping its extension.
export function safeName(name) {
  const base = String(name || "clip").split(/[\\/]/).pop();
  const ext = base.includes(".") ? base.split(".").pop().toLowerCase() : "";
  const stem = base.replace(/\.[^.]*$/, "").replace(/[^a-z0-9._-]+/gi, "-").replace(/^-+|-+$/g, "") || "clip";
  return { name: `${stem.slice(0, 64)}.${ext}`, ext };
}

// Validate a `begin` body into the record it may set. Throws with a message
// fit for a 400.
export function parseBegin(body, word = process.env.NELABEL_DROP_WORD) {
  if (word && text(body.word, 64).toLowerCase() !== word.trim().toLowerCase())
    throw new Error("That's not the club word");
  const who = text(body.who, 64);
  if (!who) throw new Error("Tell us who you are");
  const title = text(body.title, 120);
  if (!title) throw new Error("Give the track a working title");
  const length = text(body.length, 32);
  const eta = text(body.eta, 120);
  if (!eta) throw new Error("Say roughly when you could hand in a finished version");
  const note = text(body.note, 500) || null;
  const { name, ext } = safeName(body.file?.name);
  const contentType = CLIP_TYPES[ext];
  if (!contentType) throw new Error(`Send a ${Object.keys(CLIP_TYPES).join(", ")} file`);
  const bytes = Number(body.file?.bytes);
  if (!Number.isInteger(bytes) || bytes <= 0 || bytes > MAX_BYTES)
    throw new Error(`The clip must be under ${MAX_BYTES / 1024 / 1024} MB`);
  return { who, title, length, eta, note, file: { name, bytes, contentType } };
}

async function presignPut({ s3, bucket }, key, contentType) {
  const command = new PutObjectCommand({ Bucket: bucket, Key: key, ContentType: contentType, ACL: "private" });
  const url = await getSignedUrl(s3, command, {
    expiresIn: URL_SECONDS,
    signableHeaders: new Set(["content-type", "x-amz-acl"]),
    unhoistableHeaders: new Set(["x-amz-acl"]),
  });
  return { url, method: "PUT", headers: { "Content-Type": contentType, "x-amz-acl": "private" } };
}

async function presignGet({ s3, bucket }, key, filename) {
  const command = new GetObjectCommand({
    Bucket: bucket,
    Key: key,
    ResponseContentDisposition: `attachment; filename="${filename}"`,
  });
  return getSignedUrl(s3, command, { expiresIn: URL_SECONDS });
}

export function present(doc, { admin = false } = {}) {
  return {
    code: doc.code,
    who: doc.who,
    title: doc.title,
    length: doc.length || null,
    eta: doc.eta,
    note: doc.note || null,
    when: doc.when,
    ...(admin ? { file: doc.file.name, bytes: doc.file.bytes } : {}),
  };
}

async function begin(store, drops, body) {
  let fields;
  try { fields = parseBegin(body); }
  catch (error) { return reply(400, { error: error.message }); }

  const code = await generateUniqueCode(drops, { type: "nelabel" });
  const file = { ...fields.file, key: objectKey(code, fields.file.name) };
  await drops.insertOne({ ...fields, file, code, committed: false, when: new Date() });
  const upload = await presignPut(store, file.key, file.contentType);
  return reply(200, { code, upload });
}

async function commit({ s3, bucket }, drops, body) {
  const code = String(body.code || "");
  const doc = await drops.findOne({ code, nuked: { $ne: true } });
  if (!doc) return reply(404, { error: "No such drop" });
  if (doc.committed) return reply(200, { code, committed: true });
  try {
    const head = await s3.send(new HeadObjectCommand({ Bucket: bucket, Key: doc.file.key }));
    await drops.updateOne(
      { _id: doc._id },
      { $set: { "file.bytes": head.ContentLength ?? doc.file.bytes, committed: true, when: new Date() } },
    );
  } catch {
    return reply(409, { error: "The clip never arrived" });
  }
  return reply(200, { code, committed: true });
}

async function list(store, drops, admin) {
  const docs = await drops.find({ committed: true, nuked: { $ne: true } }).sort({ when: 1 }).toArray();
  const presented = [];
  for (const doc of docs) {
    const item = present(doc, { admin });
    if (admin) item.url = await presignGet(store, doc.file.key, `nelabel-${doc.code}-${doc.file.name}`);
    presented.push(item);
  }
  return reply(200, { count: presented.length, drops: presented });
}

async function remove({ s3, bucket }, drops, code) {
  const doc = await drops.findOne({ code, nuked: { $ne: true } });
  if (!doc) return reply(404, { error: "No such drop" });
  try { await s3.send(new DeleteObjectCommand({ Bucket: bucket, Key: doc.file.key })); } catch {}
  await drops.updateOne({ _id: doc._id }, { $set: { nuked: true, nukedAt: new Date() } });
  return reply(200, { code, deleted: true });
}

async function isAdmin(headers) {
  if (!headers?.authorization) return false;
  try {
    const user = await authorize(headers);
    return !!user && (await hasAdmin(user));
  } catch {
    return false;
  }
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return reply(204, "");

  const store = storage();
  if (!store) return reply(503, { error: "Storage is not configured" });

  const database = await connect();
  const drops = database.db.collection(COLLECTION);
  try {
    if (event.httpMethod === "GET") {
      return await list(store, drops, await isAdmin(event.headers));
    }
    if (event.httpMethod === "POST") {
      let body;
      try { body = JSON.parse(event.body || "{}"); }
      catch { return reply(400, { error: "Body must be JSON" }); }
      if (body.action === "begin") return await begin(store, drops, body);
      if (body.action === "commit") return await commit(store, drops, body);
      return reply(400, { error: "action must be begin or commit" });
    }
    if (event.httpMethod === "DELETE") {
      if (!(await isAdmin(event.headers))) return reply(401, { error: "Admin only" });
      const code = String(event.queryStringParameters?.code || "");
      return await remove(store, drops, code);
    }
    return reply(405, { error: "Method not allowed" });
  } finally {
    await database.disconnect();
  }
}
