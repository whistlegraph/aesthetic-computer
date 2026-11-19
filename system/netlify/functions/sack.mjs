// sack.mjs — sacks: shareable directories a handle owns.
//
// Drop files into a sack; they are kept under your handle, never expire, and
// anyone with the sack's code can download them, one by one or as one zip.
//
// GET    ?code=X                 → { sack }                      (public)
// GET    ?code=X&file=ID[&inline=1] → 302 to the file              (public)
// GET    ?code=X&zip=1           → the whole sack as a .zip       (public)
// GET                            → { handle, used, limit, sacks } (yours)
// POST   { action: "begin", code?, name?, files: [{ name, bytes, type }] }
//          → { code, partBytes, uploads: [{ id, name, parts: [url] }] }
// PUT    each part (partBytes long, the last one shorter) to its url, in order.
// POST   { action: "commit", code, ids: [ID] } → { sack, failed: [ID] }
// POST   { action: "rename", code, name }      → { sack }
// DELETE ?code=X[&file=ID]       → { sack } | { code, deleted: true }
//
// Storage: USER_SPACE_NAME bucket, key `<sub>/sack/<code>/<id>`, private ACL;
// every read is a presigned GET. Mongo: `sacks` { user, code, name, when,
// updated, files: [{ id, name, type, bytes, key, done, at, uploadId? }], nuked? }

import { randomBytes } from "node:crypto";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import {
  S3Client, GetObjectCommand, HeadObjectCommand, DeleteObjectsCommand,
  CreateMultipartUploadCommand, UploadPartCommand, ListPartsCommand,
  CompleteMultipartUploadCommand, AbortMultipartUploadCommand,
} from "@aws-sdk/client-s3";
import { authorize as authorizeUser, handleFor as handleForUser } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import {
  SACKS, HANDLE_LIMIT, PART_BYTES, cleanName, parseDrop, partCount, sackBytes,
  showsInline, storageFor, zipStream,
} from "../../backend/sack.mjs";

const UPLOAD_SECONDS = 12 * 60 * 60;
const READ_SECONDS = 60 * 60;
const CORS = { "Access-Control-Allow-Methods": "GET, POST, DELETE, OPTIONS" };
const reply = (status, body) => respond(status, body, { ...CORS, "Cache-Control": "no-store" });

let s3;
function storage() {
  const accessKeyId = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const secretAccessKey = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  if (!accessKeyId || !secretAccessKey) return null;
  s3 ||= new S3Client({
    endpoint: "https://" + (process.env.USER_ENDPOINT || "sfo3.digitaloceanspaces.com"),
    region: "us-east-1",
    credentials: { accessKeyId, secretAccessKey },
    requestChecksumCalculation: "WHEN_REQUIRED", // or a presigned PUT fails its checksum
  });
  const Bucket = process.env.USER_SPACE_NAME || "user-aesthetic-computer";
  const send = (Command, input) => s3.send(new Command({ Bucket, ...input }));
  return {
    begin: async (Key, ContentType) =>
      (await send(CreateMultipartUploadCommand, { Key, ContentType, ACL: "private" })).UploadId,
    partUrl: (Key, UploadId, PartNumber) =>
      getSignedUrl(s3, new UploadPartCommand({ Bucket, Key, UploadId, PartNumber }), { expiresIn: UPLOAD_SECONDS }),
    // Finish an upload from the parts storage itself holds, so the browser
    // never has to read an ETag back across origins.
    complete: async (Key, UploadId) => {
      const Parts = [];
      let PartNumberMarker;
      do {
        const page = await send(ListPartsCommand, { Key, UploadId, PartNumberMarker });
        for (const part of page.Parts || []) Parts.push({ ETag: part.ETag, PartNumber: part.PartNumber });
        PartNumberMarker = page.IsTruncated ? page.NextPartNumberMarker : undefined;
      } while (PartNumberMarker);
      if (!Parts.length) throw new Error("nothing uploaded");
      await send(CompleteMultipartUploadCommand, { Key, UploadId, MultipartUpload: { Parts } });
      return (await send(HeadObjectCommand, { Key })).ContentLength;
    },
    abort: (Key, UploadId) => send(AbortMultipartUploadCommand, { Key, UploadId }).catch(() => {}),
    remove: (keys) => keys.length
      ? send(DeleteObjectsCommand, { Delete: { Objects: keys.map((Key) => ({ Key })), Quiet: true } }) : null,
    readUrl: (Key, name, inline) => getSignedUrl(s3, new GetObjectCommand({
      Bucket, Key,
      ResponseContentDisposition: `${inline ? "inline" : "attachment"}; filename="${name.replace(/[^\x20-\x7e]|["\\]/g, "_")}"; filename*=UTF-8''${encodeURIComponent(name)}`,
    }), { expiresIn: READ_SECONDS }),
    read: async (Key) => (await send(GetObjectCommand, { Key })).Body,
  };
}

const fileId = () => randomBytes(6).toString("hex");

// What anyone holding the code may see.
function present(sack, handle) {
  const link = `/api/sack?code=${encodeURIComponent(sack.code)}`;
  const files = (sack.files || []).filter((file) => file.done).map((file) => ({
    id: file.id, name: file.name, type: file.type, bytes: file.bytes,
    download: `${link}&file=${file.id}`,
    ...(showsInline(file.type) ? { inline: `${link}&file=${file.id}&inline=1` } : {}),
  }));
  return {
    code: sack.code, name: sack.name, handle, when: sack.when, updated: sack.updated,
    bytes: files.reduce((sum, file) => sum + file.bytes, 0), files, zip: `${link}&zip=1`,
  };
}

export function createHandler({
  connectDatabase = connect, openStorage = storage,
  authorize = authorizeUser, handleFor = handleForUser, newCode = generateUniqueCode,
} = {}) {
  const handleOf = async (sub) => {
    const handle = await handleFor(sub);
    return handle ? "@" + String(handle).replace(/^@/, "") : null;
  };

  async function begin(store, db, user, body) {
    const sacks = db.collection(SACKS);
    let sack = null;
    if (body.code) {
      sack = await sacks.findOne({ user: user.sub, code: String(body.code), nuked: { $ne: true } });
      if (!sack) return reply(404, { error: "No such sack of yours." });
    }
    let drop;
    try { drop = parseDrop(body.files, new Set((sack?.files || []).map((file) => file.name))); }
    catch (error) { return reply(400, { error: error.message }); }

    const adding = drop.reduce((sum, file) => sum + file.bytes, 0);
    const { bytes: used } = await storageFor(db, user.sub);
    if (used + adding > HANDLE_LIMIT)
      return reply(413, { error: "That is more than your sack space has left.", used, limit: HANDLE_LIMIT, adding });

    const now = new Date();
    if (!sack) {
      sack = { user: user.sub, code: await newCode(sacks, { type: "sack" }),
        name: cleanName(body.name || drop[0].name).slice(0, 80), when: now, files: [] };
      await sacks.insertOne({ ...sack, updated: now });
    }
    const files = [], uploads = [];
    for (const file of drop) {
      const id = fileId(), key = `${user.sub}/sack/${sack.code}/${id}`;
      const uploadId = await store.begin(key, file.type);
      const parts = [];
      for (let n = 1; n <= partCount(file.bytes); n++) parts.push(await store.partUrl(key, uploadId, n));
      files.push({ id, ...file, key, uploadId, done: false, at: now });
      uploads.push({ id, name: file.name, parts });
    }
    await sacks.updateOne({ user: user.sub, code: sack.code },
      { $push: { files: { $each: files } }, $set: { updated: now } });
    return reply(200, { code: sack.code, partBytes: PART_BYTES, uploads });
  }

  async function commit(store, db, user, body) {
    const sacks = db.collection(SACKS);
    const sack = await sacks.findOne({ user: user.sub, code: String(body.code || ""), nuked: { $ne: true } });
    if (!sack) return reply(404, { error: "No such sack of yours." });
    const ids = new Set(Array.isArray(body.ids) ? body.ids.map(String) : []);
    const failed = [];
    let { bytes: used } = await storageFor(db, user.sub);
    for (const file of sack.files) {
      if (file.done || !ids.has(file.id)) continue;
      try {
        const bytes = await store.complete(file.key, file.uploadId);
        // The space was reserved at the declared size; charge what arrived.
        used += bytes - file.bytes;
        if (used > HANDLE_LIMIT) { used -= bytes; await store.remove([file.key]); throw new Error("over the limit"); }
        Object.assign(file, { bytes, done: true, at: new Date() });
        delete file.uploadId;
      } catch (error) {
        console.warn("[sack] commit failed:", sack.code, file.id, error.message);
        await store.abort(file.key, file.uploadId);
        failed.push(file.id);
      }
    }
    sack.files = sack.files.filter((file) => !failed.includes(file.id));
    sack.updated = new Date();
    await sacks.updateOne({ _id: sack._id }, { $set: { files: sack.files, updated: sack.updated } });
    return reply(200, { sack: present(sack, await handleOf(user.sub)), failed });
  }

  async function rename(db, user, body) {
    const sacks = db.collection(SACKS);
    const name = cleanName(body.name).slice(0, 80);
    const sack = await sacks.findOneAndUpdate(
      { user: user.sub, code: String(body.code || ""), nuked: { $ne: true } },
      { $set: { name, updated: new Date() } }, { returnDocument: "after" });
    if (!sack) return reply(404, { error: "No such sack of yours." });
    return reply(200, { sack: present(sack, await handleOf(user.sub)) });
  }

  async function remove(store, db, user, params) {
    const sacks = db.collection(SACKS);
    const sack = await sacks.findOne({ user: user.sub, code: String(params.code || ""), nuked: { $ne: true } });
    if (!sack) return reply(404, { error: "No such sack of yours." });
    const gone = params.file ? sack.files.filter((file) => file.id === params.file) : sack.files;
    if (params.file && !gone.length) return reply(404, { error: "No such file in this sack." });
    for (const file of gone) if (file.uploadId) await store.abort(file.key, file.uploadId);
    await store.remove(gone.filter((file) => file.done).map((file) => file.key));
    if (!params.file) {
      await sacks.updateOne({ _id: sack._id }, { $set: { nuked: true, files: [], updated: new Date() } });
      return reply(200, { code: sack.code, deleted: true });
    }
    sack.files = sack.files.filter((file) => file.id !== params.file);
    sack.updated = new Date();
    await sacks.updateOne({ _id: sack._id }, { $set: { files: sack.files, updated: sack.updated } });
    return reply(200, { sack: present(sack, await handleOf(user.sub)) });
  }

  // Everything reachable by code alone.
  async function open(store, db, params) {
    const sack = await db.collection(SACKS).findOne({ code: String(params.code), nuked: { $ne: true } });
    if (!sack) return reply(404, { error: "No such sack." });
    const done = sack.files.filter((file) => file.done);
    if (params.file) {
      const file = done.find((each) => each.id === params.file);
      if (!file) return reply(404, { error: "No such file in this sack." });
      const inline = params.inline === "1" && showsInline(file.type);
      return { statusCode: 302, body: "", headers: { ...CORS, "Access-Control-Allow-Origin": "*",
        "Cache-Control": "no-store", Location: await store.readUrl(file.key, file.name, inline) } };
    }
    if (params.zip === "1") {
      if (!done.length) return reply(404, { error: "This sack is empty." });
      const name = `${sack.name.replace(/\.[a-z0-9]{1,5}$/i, "")}-${sack.code}.zip`;
      // An S3 body is already an async iterable of Buffers.
      const entries = done.map((file) => ({ name: file.name,
        open: async function* () { yield* await store.read(file.key); } }));
      return { statusCode: 200, body: ReadableStream.from(zipStream(entries)), headers: {
        ...CORS, "Access-Control-Allow-Origin": "*", "Cache-Control": "no-store", "Content-Type": "application/zip",
        "Content-Disposition": `attachment; filename="${name.replace(/[^\x20-\x7e]|["\\]/g, "_")}"; filename*=UTF-8''${encodeURIComponent(name)}`,
      } };
    }
    return reply(200, { sack: present(sack, await handleOf(sack.user)) });
  }

  return async function handler(event) {
    if (event.httpMethod === "OPTIONS") return reply(204, "");
    if (!["GET", "POST", "DELETE"].includes(event.httpMethod)) return reply(405, { error: "Method not allowed." });
    const store = openStorage();
    if (!store) return reply(503, { error: "Sack storage is not configured." });
    const params = event.queryStringParameters || {};

    let database;
    try {
      database = await connectDatabase();
      const db = database.db;
      if (event.httpMethod === "GET" && params.code) return await open(store, db, params);

      const user = await authorize(event.headers);
      if (!user?.sub) return reply(401, { error: "Log in to keep a sack." });
      const handle = await handleOf(user.sub);
      if (!handle) return reply(403, { error: "Sacks belong to handles. Claim one first." });

      if (event.httpMethod === "GET") {
        const sacks = await db.collection(SACKS).find({ user: user.sub, nuked: { $ne: true } })
          .sort({ updated: -1 }).limit(500).toArray();
        const { bytes: used } = await storageFor(db, user.sub);
        return reply(200, { handle, used, limit: HANDLE_LIMIT, sacks: sacks.map((sack) => ({
          code: sack.code, name: sack.name, when: sack.when, updated: sack.updated,
          bytes: sackBytes(sack), files: sack.files.filter((file) => file.done).length })) });
      }
      if (event.httpMethod === "DELETE") return await remove(store, db, user, params);

      let body;
      try { body = JSON.parse(event.body || "{}"); }
      catch { return reply(400, { error: "Body must be JSON." }); }
      if (body.action === "begin") {
        await db.collection(SACKS).createIndex({ code: 1 }, { unique: true });
        await db.collection(SACKS).createIndex({ user: 1, updated: -1 });
        return await begin(store, db, user, body);
      }
      if (body.action === "commit") return await commit(store, db, user, body);
      if (body.action === "rename") return await rename(db, user, body);
      return reply(400, { error: "action must be begin, commit or rename." });
    } catch (error) {
      console.error("[sack] error:", error);
      return reply(500, { error: "Sack is unavailable. Try again." });
    } finally {
      await database?.disconnect?.();
    }
  };
}

export const handler = createHandler();
