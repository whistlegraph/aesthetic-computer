import { randomBytes, createHash } from "node:crypto";
import { validateGame, starterEngineGame } from "../public/oskie/format.mjs";
import { revisionBundle, gameObjects, OSKIE_TYPE } from "./oskie-objects.mjs";
const fail = (status, message) => Object.assign(Error(message), { status });
const id = () => randomBytes(16).toString("hex");
const hash = (x) => createHash("sha256").update(x).digest("hex");
export function createGameStore(db, { now = () => new Date(), objects } = {}) {
  const games = db.collection("oskie-games"),
    revisions = db.collection("oskie-revisions"),
    grants = db.collection("oskie-previews");
  const publicRow = (x) => ({
    id: x._id,
    title: x.title,
    revision: x.revision,
    publishedRevision: x.publishedRevision || null,
    author: x.author,
  });
  async function owned(owner, gameId) {
    const row = await games.findOne({ _id: String(gameId), owner: owner.sub });
    if (!row || row.deleting) throw fail(404, "Game not found.");
    return row;
  }
  async function revision(gameId, revisionId) {
    const r = await revisions.findOne({
      _id: String(revisionId),
      gameId: String(gameId),
    });
    if (!r) throw fail(404, "Revision not found.");
    return r;
  }
  const bundleFor = r => revisionBundle(r, objects);
  const manifestFor = async r => r.manifest || (await bundleFor(r)).manifest;
  async function discard(r) {
    if (r.object) await objects.remove(r);
    await revisions.deleteOne({ _id: r._id });
  }
  async function saveRevision(owner, gameId, bundle, parent = null) {
    const game = validateGame(bundle);
    game.manifest.author = owner.handle;
    if ((await revisions.countDocuments({ owner: owner.sub })) >= 200)
      throw fail(
        429,
        "The beta supports 200 saved revisions per account. Download and delete an old project to make room.",
      );
    const r = {
      _id: id(),
      gameId,
      owner: owner.sub,
      parent,
      manifest: game.manifest,
      created: now(),
    };
    if (objects) r.object = await objects.put(r, game);
    else r.bundle = game; // Explicit in-memory/test stores; production requires Spaces.
    try { await revisions.insertOne(r); }
    catch (error) { if (r.object) await objects.remove(r); throw error; }
    return r;
  }
  async function open(owner, gameId) {
    const row = await owned(owner, gameId),
      token = randomBytes(32).toString("base64url");
    await grants.insertOne({
      _id: hash(token),
      gameId: row._id,
      owner: owner.sub,
      expiresAt: new Date(+now() + 24 * 60 * 60 * 1000),
    });
    return {
      ...publicRow(row),
      preview: `https://aesthetic.computer/oskie/${row._id}#preview=${token}`,
    };
  }
  async function grant(token, gameId) {
    if (typeof token !== "string" || token.length > 100)
      throw fail(401, "Preview link required.");
    const g = await grants.findOne({
      _id: hash(token),
      gameId: String(gameId),
    });
    if (!g || g.expiresAt <= now())
      throw fail(401, "Preview expired. Ask Claude to open a new preview.");
    return g;
  }
  async function readPreview(gameId, token, knownRevision) {
    const g = await grant(token, gameId),
      row = await games.findOne({ _id: g.gameId, owner: g.owner });
    if (!row || row.deleting) throw fail(404, "Game not found.");
    if (knownRevision === row.revision) return { id: row._id, revision: row.revision, unchanged: true };
    const r = await revision(row._id, row.revision);
    return { ...publicRow(row), bundle: await bundleFor(r) };
  }
  return {
    async indexes() {
      await grants.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
      await games.createIndex({ owner: 1 });
      await games.createIndex({ publishedRevision: 1, _id: -1 });
      await games.createIndex({ author: 1, _id: -1 });
      await revisions.createIndex({ gameId: 1, created: -1 });
    },
    async create(owner, { slug, title, bundle } = {}) {
      if ((await games.countDocuments({ owner: owner.sub })) >= 100)
        throw fail(429, "The beta supports 100 projects per account.");
      const gameId = id(),
        r = await saveRevision(
          owner,
          gameId,
          bundle || starterEngineGame(slug, title),
        );
      await games.insertOne({
        _id: gameId,
        owner: owner.sub,
        author: owner.handle,
        title: r.manifest.title,
        revision: r._id,
        created: now(),
        updated: now(),
      });
      return open(owner, gameId);
    },
    async list(owner) {
      return (
        await games
          .find({ owner: owner.sub })
          .sort({ updated: -1 })
          .limit(100)
          .toArray()
      ).map(publicRow);
    },
    async read(owner, gameId) {
      const row = await owned(owner, gameId);
      return {
        ...publicRow(row),
        bundle: await bundleFor(await revision(gameId, row.revision)),
      };
    },
    async update(owner, gameId, bundle, expected) {
      const row = await owned(owner, gameId);
      if (!expected || row.revision !== expected)
        throw fail(
          409,
          "Revision changed. Read the current game before editing.",
        );
      const r = await saveRevision(owner, gameId, bundle, expected);
      const changed = await games.updateOne(
        { _id: row._id, owner: owner.sub, revision: expected, deleting: { $ne: true } },
        {
          $set: {
            revision: r._id,
            title: r.manifest.title,
            author: owner.handle,
            updated: now(),
          },
          $unset: { feedback: "" },
        },
      );
      if (!changed.matchedCount) {
        await discard(r);
        throw fail(
          409,
          "Revision changed. Read the current game before editing.",
        );
      }
      return { id: row._id, revision: r._id, author: owner.handle };
    },
    async history(owner, gameId) {
      const row = await owned(owner, gameId);
      const out = [];
      let rev = row.revision;
      while (rev && out.length < 50) {
        const r = await revision(gameId, rev);
        out.push({
          revision: r._id,
          parent: r.parent,
          created: r.created,
          title: (await manifestFor(r)).title,
        });
        rev = r.parent;
      }
      return out;
    },
    async restore(owner, gameId, target, expected) {
      await owned(owner, gameId);
      const r = await revision(gameId, target);
      if (r.owner !== owner.sub) throw fail(404, "Revision not found.");
      return this.update(owner, gameId, await bundleFor(r), expected);
    },
    async publish(owner, gameId, expected) {
      const row = await owned(owner, gameId);
      if (!expected) throw fail(409, "Read the current revision first.");
      if (row.revision !== expected) throw fail(409, "Revision changed.");
      const r = await revision(gameId, expected);
      // Publication verifies that the exact downloadable bytes are readable.
      const bundle = await bundleFor(r);
      const result = await games.updateOne(
        { _id: row._id, owner: owner.sub, revision: expected, deleting: { $ne: true } },
        { $set: { publishedRevision: expected, publishedAt: now(), publishedManifest: bundle.manifest, author: owner.handle } },
      );
      if (!result.matchedCount) throw fail(409, "Revision changed.");
      return {
        id: row._id,
        revision: expected,
        url: `https://aesthetic.computer/oskie/${row._id}`,
        download: `https://aesthetic.computer/api/oskie/${row._id}/download`,
      };
    },
    async unpublish(owner, gameId) {
      await owned(owner, gameId);
      await games.updateOne(
        { _id: gameId, owner: owner.sub },
        { $unset: { publishedRevision: "", publishedAt: "", publishedManifest: "" } },
      );
      return { id: gameId, published: false };
    },
    async remove(owner, gameId, expected) {
      // Keep a tombstone until object cleanup succeeds, so a failed delete is
      // retryable while previews, publication and edits are already disabled.
      const row = await games.findOne({ _id: gameId, owner: owner.sub });
      if (!row) throw fail(404, "Game not found.");
      const locked = await games.updateOne({ _id: gameId, owner: owner.sub, revision: expected },
        { $set: { deleting: true }, $unset: { publishedRevision: "", publishedManifest: "", publishedAt: "" } });
      if (!locked.matchedCount)
        throw fail(
          409,
          "Revision changed. Read the current game before deleting.",
        );
      await grants.deleteMany({ gameId, owner: owner.sub });
      for (const r of await revisions.find({ gameId, owner: owner.sub }).toArray())
        if (r.object) await (objects || gameObjects()).remove(r);
      await revisions.deleteMany({ gameId, owner: owner.sub });
      await games.deleteOne({ _id: gameId, owner: owner.sub, deleting: true });
      return { id: gameId, deleted: true };
    },
    open,
    readPreview,
    async readPublic(gameId, knownRevision) {
      const row = await games.findOne({ _id: String(gameId) });
      if (!row?.publishedRevision || row.deleting) throw fail(404, "Game not published.");
      if (knownRevision === row.publishedRevision) return { id: row._id, revision: row.publishedRevision, unchanged: true };
      const published = await revision(gameId, row.publishedRevision);
      const bundle = await bundleFor(published);
      return {
        ...publicRow(row),
        title: bundle.manifest.title,
        author: bundle.manifest.author,
        revision: row.publishedRevision,
        bundle,
      };
    },
    async listPublished({ author, cursor, limit = 24 } = {}) {
      if (author != null && (typeof author !== "string" || !/^@?[a-z0-9_-]{1,60}$/i.test(author)))
        throw fail(400, "Invalid author.");
      if (cursor != null && (typeof cursor !== "string" || !/^[a-f0-9]{32}$/.test(cursor)))
        throw fail(400, "Invalid catalog cursor.");
      if (!Number.isInteger(Number(limit)) || Number(limit) < 1 || Number(limit) > 50)
        throw fail(400, "Catalog limit must be 1–50.");
      const query = { publishedRevision: { $type: "string" }, deleting: { $ne: true } };
      if (author) query.author = "@" + author.replace(/^@/, "").toLowerCase();
      if (cursor) query._id = { $lt: cursor };
      const rows = await games.find(query).sort({ _id: -1 }).limit(Number(limit) + 1).toArray();
      const items = [];
      for (const row of rows.slice(0, Number(limit))) {
        const r = await revision(row._id, row.publishedRevision);
        const m = await manifestFor(r);
        const url = `https://aesthetic.computer/oskie/${row._id}`;
        const download = `https://aesthetic.computer/api/oskie/${row._id}/download`;
        items.push({ id: row._id, revision: r._id, title: m.title, author: m.author,
          description: m.description || "", runtime: m.runtime, publishedAt: row.publishedAt || r.created,
          url, download,
          file: { name: `${m.slug}.oskie`, type: OSKIE_TYPE,
            size: r.object?.bytes ?? Buffer.byteLength(JSON.stringify(r.bundle)), url: download },
          media: { kind: "oskie", id: row._id, url, author: m.author },
        });
      }
      return { games: items, nextCursor: rows.length > Number(limit) ? rows[Number(limit) - 1]._id : null };
    },
    async feedback(owner, gameId) {
      const row = await owned(owner, gameId);
      return row.feedback?.revision === row.revision
        ? row.feedback
        : {
            revision: row.revision,
            status: "waiting",
            message: "Open the preview in a browser to inspect this revision.",
          };
    },
    async report(gameId, token, data) {
      const g = await grant(token, gameId);
      if (
        !data ||
        !["running", "error"].includes(data.status) ||
        typeof data.revision !== "string"
      )
        throw fail(400, "Invalid preview report.");
      const frame =
        typeof data.frame === "string" &&
        /^data:image\/png;base64,[A-Za-z0-9+/=]+$/.test(data.frame) &&
        data.frame.length <= 900000
          ? data.frame
          : null;
      const feedback = {
        revision: data.revision,
        status: data.status,
        error:
          typeof data.error === "string" ? data.error.slice(0, 2000) : null,
        frame,
        at: now(),
        untrusted: true,
      };
      const r = await games.updateOne(
        { _id: g.gameId, owner: g.owner, revision: data.revision },
        { $set: { feedback } },
      );
      if (!r.matchedCount) throw fail(409, "Stale preview.");
      return { ok: true };
    },
  };
}
let shared;
export async function gameStore() {
  return (shared ||= (async () => {
    const { db } = await (await import("./database.mjs")).connect();
    const store = createGameStore(db, { objects: gameObjects() });
    await store.indexes();
    return store;
  })().catch((e) => {
    shared = null;
    throw e;
  }));
}
