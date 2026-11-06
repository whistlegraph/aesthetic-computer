import { randomBytes, createHash } from "node:crypto";
import { validateGame, starterGame } from "../public/oskie/format.mjs";
const fail = (status, message) => Object.assign(Error(message), { status });
const id = () => randomBytes(16).toString("hex");
const hash = (x) => createHash("sha256").update(x).digest("hex");
export function createGameStore(db, { now = () => new Date() } = {}) {
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
    if (!row) throw fail(404, "Game not found.");
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
      bundle: game,
      created: now(),
    };
    await revisions.insertOne(r);
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
  async function readPreview(gameId, token) {
    const g = await grant(token, gameId),
      row = await games.findOne({ _id: g.gameId, owner: g.owner });
    if (!row) throw fail(404, "Game not found.");
    const r = await revision(row._id, row.revision);
    return { ...publicRow(row), bundle: r.bundle };
  }
  return {
    async indexes() {
      await grants.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 });
      await games.createIndex({ owner: 1 });
      await revisions.createIndex({ gameId: 1, created: -1 });
    },
    async create(owner, { slug, title, bundle } = {}) {
      if ((await games.countDocuments({ owner: owner.sub })) >= 100)
        throw fail(429, "The beta supports 100 projects per account.");
      const gameId = id(),
        r = await saveRevision(
          owner,
          gameId,
          bundle || starterGame(slug, title),
        );
      await games.insertOne({
        _id: gameId,
        owner: owner.sub,
        author: owner.handle,
        title: r.bundle.manifest.title,
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
        bundle: (await revision(gameId, row.revision)).bundle,
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
        { _id: row._id, owner: owner.sub, revision: expected },
        {
          $set: {
            revision: r._id,
            title: r.bundle.manifest.title,
            author: owner.handle,
            updated: now(),
          },
          $unset: { feedback: "" },
        },
      );
      if (!changed.matchedCount) {
        await revisions.deleteOne({ _id: r._id });
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
          title: r.bundle.manifest.title,
        });
        rev = r.parent;
      }
      return out;
    },
    async restore(owner, gameId, target, expected) {
      await owned(owner, gameId);
      const r = await revision(gameId, target);
      if (r.owner !== owner.sub) throw fail(404, "Revision not found.");
      return this.update(owner, gameId, r.bundle, expected);
    },
    async publish(owner, gameId, expected) {
      const row = await owned(owner, gameId);
      if (!expected) throw fail(409, "Read the current revision first.");
      const result = await games.updateOne(
        { _id: row._id, owner: owner.sub, revision: expected },
        { $set: { publishedRevision: expected, author: owner.handle } },
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
        { $unset: { publishedRevision: "" } },
      );
      return { id: gameId, published: false };
    },
    async remove(owner, gameId, expected) {
      await owned(owner, gameId);
      const row = await games.findOneAndDelete({
        _id: gameId,
        owner: owner.sub,
        revision: expected,
      });
      if (!row)
        throw fail(
          409,
          "Revision changed. Read the current game before deleting.",
        );
      await grants.deleteMany({ gameId, owner: owner.sub });
      await revisions.deleteMany({ gameId, owner: owner.sub });
      return { id: gameId, deleted: true };
    },
    open,
    readPreview,
    async readPublic(gameId) {
      const row = await games.findOne({ _id: String(gameId) });
      if (!row?.publishedRevision) throw fail(404, "Game not published.");
      const published = await revision(gameId, row.publishedRevision);
      return {
        ...publicRow(row),
        title: published.bundle.manifest.title,
        author: published.bundle.manifest.author,
        revision: row.publishedRevision,
        bundle: published.bundle,
      };
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
    const store = createGameStore(db);
    await store.indexes();
    return store;
  })().catch((e) => {
    shared = null;
    throw e;
  }));
}
