// Whistlegraph archive desk — Auth0-gated live curation overlays.
//
// GET   /api/whistlegraph-admin?action=curation  public overlay for the site
// GET   /api/whistlegraph-admin?action=session   authenticated admin session
// GET   /api/whistlegraph-admin?action=data      authenticated overlay + audit view
// PATCH /api/whistlegraph-admin                  upsert/reset one post or work patch

import { readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { authorize, handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  curationPayload,
  isWhistlegraphAdmin,
  normalizePostPatch,
  normalizeWorkPatch,
  whistlegraphAdminSubs,
} from "./whistlegraph-admin-lib.mjs";

const COLLECTION = "whistlegraph-curation";
const AUDIT_COLLECTION = "whistlegraph-curation-audit";
const MODEL_DIR = join(process.cwd(), "public", "whistlegraph.org");
const HEADERS = {
  "Cache-Control": "no-store",
  "Access-Control-Allow-Methods": "GET, PATCH, OPTIONS",
};

let modelCache = { stamp: "", workCodes: new Set(), postIds: new Set() };

function loadBaseModel() {
  const graphsPath = join(MODEL_DIR, "graphs.json");
  const postsPath = join(MODEL_DIR, "posts.json");
  const stamp = `${statSync(graphsPath).mtimeMs}:${statSync(postsPath).mtimeMs}`;
  if (modelCache.stamp === stamp) return modelCache;
  const graphs = JSON.parse(readFileSync(graphsPath, "utf8"));
  const posts = JSON.parse(readFileSync(postsPath, "utf8"));
  modelCache = {
    stamp,
    workCodes: new Set((graphs.works || []).map((work) => String(work.code))),
    postIds: new Set((posts.posts || []).map((post) => String(post.id))),
  };
  return modelCache;
}

function parseBody(event) {
  try {
    return JSON.parse(event.body || "{}");
  } catch {
    throw new Error("Invalid JSON body.");
  }
}

async function requireAdmin(event, authorizeFn, allowed) {
  const user = await authorizeFn(event.headers || {});
  if (!user?.sub) return { error: respond(401, { message: "Sign in with Aesthetic Computer." }, HEADERS) };
  if (!isWhistlegraphAdmin(user, allowed)) {
    return { error: respond(403, { message: "This account cannot edit Whistlegraph." }, HEADERS) };
  }
  return { user };
}

async function readCuration(database) {
  const documents = await database.db.collection(COLLECTION).find({}).toArray();
  return curationPayload(documents);
}

export function createHandler({
  authorizeFn = authorize,
  handleForFn = handleFor,
  connectFn = connect,
  allowedSubs = whistlegraphAdminSubs(),
  loadModelFn = loadBaseModel,
} = {}) {
  return async function whistlegraphAdminHandler(event) {
    if (event.httpMethod === "OPTIONS") return respond(204, "", HEADERS);
    const action = event.queryStringParameters?.action || "session";

    if (event.httpMethod === "GET" && action === "curation") {
      let database;
      try {
        database = await connectFn();
        const payload = await readCuration(database);
        return respond(200, payload, HEADERS);
      } catch (error) {
        console.error("Whistlegraph public curation read failed:", error?.message || error);
        return respond(200, { revision: null, works: {}, posts: {}, degraded: true }, HEADERS);
      } finally {
        try { await database?.disconnect?.(); } catch {}
      }
    }

    const auth = await requireAdmin(event, authorizeFn, allowedSubs);
    if (auth.error) return auth.error;
    const user = auth.user;

    if (event.httpMethod === "GET" && action === "session") {
      let handle = null;
      try { handle = await handleForFn(user.sub); } catch {}
      return respond(200, { authorized: true, sub: user.sub, handle: handle || null }, HEADERS);
    }

    if (event.httpMethod === "GET" && action === "data") {
      const database = await connectFn();
      try {
        const curation = await readCuration(database);
        const recent = await database.db.collection(AUDIT_COLLECTION)
          .find({}, { projection: { admin: 0 } })
          .sort({ when: -1 })
          .limit(30)
          .toArray();
        return respond(200, { ...curation, recent }, HEADERS);
      } finally {
        await database.disconnect?.();
      }
    }

    if (event.httpMethod !== "PATCH") {
      return respond(405, { message: "Method Not Allowed." }, HEADERS);
    }

    let body;
    try {
      body = parseBody(event);
    } catch (error) {
      return respond(400, { message: error.message }, HEADERS);
    }
    const type = body.type === "work" ? "work" : body.type === "post" ? "post" : null;
    const key = String(body.key || "").trim();
    if (!type || !key) return respond(400, { message: "Send a post or work key." }, HEADERS);

    let model;
    try { model = loadModelFn(); }
    catch (error) { return respond(500, { message: `Archive model unavailable: ${error.message}` }, HEADERS); }
    if (type === "post" && !model.postIds.has(key)) return respond(404, { message: "Post not found." }, HEADERS);
    if (type === "work" && !model.workCodes.has(key)) return respond(404, { message: "Whistlegraph not found." }, HEADERS);

    const database = await connectFn();
    try {
      const collection = database.db.collection(COLLECTION);
      const audit = database.db.collection(AUDIT_COLLECTION);
      await collection.createIndex({ type: 1, key: 1 }, { unique: true, background: true });
      await audit.createIndex({ when: -1 }, { background: true });
      const id = `${type}:${key}`;
      const previous = await collection.findOne({ _id: id });

      if (body.reset === true) {
        await collection.deleteOne({ _id: id });
        await audit.insertOne({ type, key, action: "reset", before: previous?.patch || null, after: null, admin: user.sub, when: new Date() });
        return respond(200, { saved: true, reset: true, type, key, patch: null }, HEADERS);
      }

      let patch;
      try {
        patch = type === "post"
          ? normalizePostPatch(body.patch, model.workCodes)
          : normalizeWorkPatch(body.patch);
      } catch (error) {
        return respond(400, { message: error.message }, HEADERS);
      }
      const now = new Date();
      await collection.updateOne(
        { _id: id },
        { $set: { type, key, patch, updatedAt: now, updatedBy: user.sub } },
        { upsert: true },
      );
      await audit.insertOne({ type, key, action: "save", before: previous?.patch || null, after: patch, admin: user.sub, when: now });
      return respond(200, { saved: true, type, key, patch, revision: now.toISOString() }, HEADERS);
    } finally {
      await database.disconnect?.();
    }
  };
}

export const handler = createHandler();
