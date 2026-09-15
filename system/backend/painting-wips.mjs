import { createHash, timingSafeEqual } from "node:crypto";
import { gunzipSync } from "node:zlib";
import { Readable } from "node:stream";
import { pipeline } from "node:stream/promises";
import { GridFSBucket } from "mongodb";
import { generateUniqueCode } from "./generate-short-code.mjs";

const HOUR = 60 * 60 * 1000;
const MAX_STATE_BYTES = 32 * 1024 * 1024;
const idPattern = /^[a-zA-Z0-9_-]{20,80}$/;
const keyPattern = /^[a-zA-Z0-9_-]{32,100}$/;
const digest = (value) => createHash("sha256").update(value).digest("hex");

export class PaintingWipError extends Error {
  constructor(status, message) { super(message); this.status = status; }
}
const fail = (status, message) => { throw new PaintingWipError(status, message); };

export function paintingWipExpired(painting, now = Date.now()) {
  return painting?.status === "wip" && painting.wip?.steps === 0 &&
    new Date(painting.wip.expiresAt).getTime() <= now;
}

export function canEditPaintingWip(painting, user, key) {
  if (!painting?.wip) return false;
  if (painting.user && user?.sub === painting.user) return true;
  if (painting.user || !keyPattern.test(key || "")) return false;
  const expected = Buffer.from(painting.wip.editorHash, "hex");
  const supplied = Buffer.from(digest(key), "hex");
  return expected.length === supplied.length && timingSafeEqual(expected, supplied);
}

export function paintingWipMetadata(painting) {
  return {
    id: painting.wip.id, code: painting.code, status: painting.status,
    revision: painting.wip.revision, steps: painting.wip.steps,
    width: painting.wip.width, height: painting.wip.height,
    parent: painting.wip.parent || null, updatedAt: painting.updatedAt,
  };
}

function statePayload(encoded) {
  if (typeof encoded !== "string" || encoded.length > Math.ceil(MAX_STATE_BYTES * 4 / 3) ||
      !/^[A-Za-z0-9+/]*={0,2}$/.test(encoded)) fail(413, "Painting state is too large or invalid");
  const data = Buffer.from(encoded, "base64");
  let state;
  try { state = JSON.parse(gunzipSync(data, { maxOutputLength: 128 * 1024 * 1024 })); }
  catch { fail(400, "Invalid painting state"); }
  const piece = state?.piece;
  if (state?.format !== "aesthetic.computer/painting-state" || state.version !== 1 ||
      piece?.schema !== "aesthetic.computer/nopaint-piece" || piece.version !== 1 ||
      !Number.isInteger(piece.width) || !Number.isInteger(piece.height) ||
      piece.width < 1 || piece.height < 1 || piece.width * piece.height > 8 * 1024 * 1024 ||
      !Array.isArray(piece.layers) || !piece.layers.length || piece.layers.length > 8192) fail(400, "Invalid painting dimensions or layers");
  function pixels(value, length) {
    return value && typeof value.$pixels === "string" && Number.isSafeInteger(length) && length >= 0 &&
      /^[A-Za-z0-9+/]*={0,2}$/.test(value.$pixels) && Buffer.from(value.$pixels, "base64").length === length;
  }
  if (!pixels(piece.composite?.pixels, piece.width * piece.height * 4)) fail(400, "Invalid painting pixels");
  for (const layer of piece.layers) {
    const p = layer?.pixels;
    if (!p || !["composite", "overlay"].includes(p.mode) || !layer.id || !layer.code?.source ||
        !Number.isInteger(p.width) || !Number.isInteger(p.height) || p.width < 0 || p.height < 0 ||
        !pixels(p.data, p.width * p.height * 4)) fail(400, "Invalid painting layer");
  }
  return { data, hash: digest(data), width: piece.width, height: piece.height, layers: piece.layers.length };
}

// Repository operations keep the lifecycle testable without a live database.
export function createPaintingWipService(repo, now = () => Date.now()) {
  async function find(code) {
    if (typeof code !== "string" || !/^[A-Za-z0-9]{3,12}$/.test(code)) fail(400, "Invalid painting code");
    const painting = await repo.find({ code });
    if (!painting?.wip || paintingWipExpired(painting, now())) fail(404, "Painting not found");
    return painting;
  }
  function authorize(painting, user, key) {
    if (!canEditPaintingWip(painting, user, key)) fail(403, "This painting belongs to another editor");
  }
  return {
    async create(input, user) {
      if (!idPattern.test(input.id || "") || !keyPattern.test(input.key || "")) fail(400, "Invalid editor identity");
      if (![input.width, input.height].every((n) => Number.isInteger(n) && n > 0) ||
          input.width * input.height > 8 * 1024 * 1024) fail(400, "Invalid painting dimensions");
      if (!Number.isInteger(input.initialLayers) || input.initialLayers < 1 || input.initialLayers > 8192) fail(400, "Invalid initial steps");
      const existing = await repo.find({ "wip.id": input.id });
      if (existing) {
        authorize(existing, user, input.key);
        if (paintingWipExpired(existing, now())) fail(410, "This empty painting has expired");
        return paintingWipMetadata(existing);
      }
      if (input.parent && (typeof input.parent !== "string" || !/^[A-Za-z0-9]{3,12}$/.test(input.parent) ||
          !await repo.find({ code: input.parent }))) fail(404, "Starting painting not found");
      const date = new Date(now());
      for (let attempt = 0; attempt < 8; attempt++) {
        const painting = {
          code: await repo.code(), slug: `wip/${input.id}`, status: "wip", when: date, updatedAt: date,
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
          ...(user?.sub ? { user: user.sub } : {}),
          wip: { id: input.id, editorHash: digest(input.key), revision: 0, steps: 0,
            width: input.width, height: input.height, initialLayers: input.initialLayers || 1,
            parent: input.parent || null, expiresAt: new Date(now() + HOUR), snapshot: null },
        };
        try { await repo.insert(painting); return paintingWipMetadata(painting); }
        catch (error) {
          if (error.code !== 11000) throw error;
          const raced = await repo.find({ "wip.id": input.id });
          if (raced) { authorize(raced, user, input.key); return paintingWipMetadata(raced); }
        }
      }
      fail(503, "Could not allocate a painting code");
    },
    async read(code, includeState = false, user, key) {
      let painting = await find(code);
      let state = null;
      if (includeState && painting.wip.snapshot) {
        try { state = (await repo.readState(painting.wip.snapshot)).toString("base64"); }
        catch {
          // A successful save may have reclaimed the snapshot after our read.
          painting = await find(code);
          state = (await repo.readState(painting.wip.snapshot)).toString("base64");
        }
      }
      return { ...paintingWipMetadata(painting), canEdit: painting.status === "wip" && canEditPaintingWip(painting, user, key),
        ...(includeState ? { state } : {}) };
    },
    async save(input, user) {
      const painting = await find(input.code);
      authorize(painting, user, input.key);
      if (painting.status !== "wip") fail(409, "This painting is Done. Start a new painting from it.");
      const state = statePayload(input.state);
      // Retrying a response that was lost does not add a revision.
      if (state.hash === painting.wip.hash) return paintingWipMetadata(painting);
      if (input.revision !== painting.wip.revision) fail(409, "This painting changed in another tab. Reload it before continuing.");
      const snapshot = await repo.writeState(state.data);
      const wip = { ...painting.wip, revision: painting.wip.revision + 1,
        snapshot, hash: state.hash, width: state.width, height: state.height,
        steps: Math.max(0, state.layers - painting.wip.initialLayers) };
      if (wip.steps > 0) delete wip.expiresAt;
      const update = { wip, updatedAt: new Date(now()) };
      const changed = await repo.update({ code: input.code, status: "wip", "wip.revision": input.revision }, update);
      if (!changed) { await repo.deleteState(snapshot); fail(409, "This painting changed in another tab"); }
      if (painting.wip.snapshot) await repo.deleteState(painting.wip.snapshot).catch(() => {});
      return paintingWipMetadata({ ...painting, ...update });
    },
    async seal(input, user, slug) {
      if (typeof slug !== "string" || !slug.length || slug.length > 512) fail(400, "Invalid painting slug");
      const painting = await find(input.code);
      authorize(painting, user, input.key);
      if (painting.status === "done") return { code: painting.code, slug: painting.slug, paintingId: String(painting._id) };
      if (!painting.wip.snapshot || input.revision !== painting.wip.revision) fail(409, "Save the latest painting state before Done");
      const wip = { ...painting.wip };
      delete wip.expiresAt;
      const changed = await repo.update({ code: input.code, status: "wip", "wip.revision": input.revision },
        { status: "done", slug, wip, updatedAt: new Date(now()), completedAt: new Date(now()),
          bucket: user ? "user-aesthetic-computer" : "art-aesthetic-computer",
          ...(user?.sub ? { user: user.sub } : {}) });
      if (!changed) fail(409, "This painting changed before Done");
      return { code: painting.code, slug, paintingId: String(painting._id), newlyDone: true };
    },
  };
}

export async function mongoPaintingWips(db) {
  const paintings = db.collection("paintings");
  await paintings.createIndex({ "wip.id": 1 }, { unique: true, sparse: true });
  await paintings.createIndex({ code: 1 }, { unique: true, sparse: true });
  await paintings.createIndex({ status: 1, "wip.expiresAt": 1 });
  const files = new GridFSBucket(db, { bucketName: "painting-states" });
  const repo = {
    find: (query) => paintings.findOne(query),
    code: () => generateUniqueCode(paintings),
    insert: (record) => paintings.insertOne(record),
    update: async (query, fields) => (await paintings.updateOne(query, { $set: fields })).modifiedCount === 1,
    writeState: async (buffer) => {
      const stream = files.openUploadStream("painting-state.json.gz", { metadata: { createdAt: new Date() } });
      await pipeline(Readable.from([buffer]), stream);
      return stream.id;
    },
    readState: async (id) => {
      const chunks = [];
      for await (const chunk of files.openDownloadStream(id)) chunks.push(chunk);
      return Buffer.concat(chunks);
    },
    deleteState: (id) => files.delete(id),
  };
  return { service: createPaintingWipService(repo), repo, paintings };
}

// Like tape drafts, reclaim a bounded batch as new WIP traffic arrives.
export async function pruneEmptyPaintingWips(db) {
  const { paintings, repo } = await mongoPaintingWips(db);
  const expired = await paintings.find({ status: "wip", "wip.steps": 0, "wip.expiresAt": { $lte: new Date() } }).limit(20).toArray();
  for (const painting of expired) {
    const deleted = await paintings.deleteOne({ _id: painting._id, status: "wip", "wip.steps": 0, "wip.revision": painting.wip.revision });
    if (deleted.deletedCount && painting.wip.snapshot) await repo.deleteState(painting.wip.snapshot).catch(() => {});
  }
}

export async function paintingWipPixels(db, painting) {
  if (paintingWipExpired(painting)) fail(404, "Painting not found");
  if (!painting.wip.snapshot) {
    return { width: painting.wip.width, height: painting.wip.height,
      pixels: Buffer.alloc(painting.wip.width * painting.wip.height * 4, 255) };
  }
  const { repo } = await mongoPaintingWips(db);
  const data = await repo.readState(painting.wip.snapshot);
  const state = JSON.parse(gunzipSync(data, { maxOutputLength: 128 * 1024 * 1024 }));
  return { width: state.piece.width, height: state.piece.height,
    pixels: Buffer.from(state.piece.composite.pixels.$pixels, "base64") };
}
