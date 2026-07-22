// Whistlegraph Desk — Auth0-gated live curation overlays.
//
// GET   /api/whistlegraph-admin?action=curation  public overlay for the site
// GET   /api/whistlegraph-admin?action=session   authenticated admin session
// GET   /api/whistlegraph-admin?action=data      authenticated overlay + audit view
// POST  /api/whistlegraph-admin?action=work      create a recoverable work
// PATCH /api/whistlegraph-admin                  upsert/reset one post or work patch
// DELETE /api/whistlegraph-admin                 soft-delete a work

import { execFile as execFileCallback, spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { promisify } from "node:util";
import { authorize, handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  curationPayload,
  isWhistlegraphAdmin,
  normalizeDeployRequest,
  normalizePostPatch,
  normalizeWorkCode,
  normalizeWorkPatch,
  deriveVisualTags,
  visualSearchText,
  validateDeployEvidence,
  whistlegraphAdminSubs,
} from "./whistlegraph-admin-lib.mjs";

const COLLECTION = "whistlegraph-curation";
const AUDIT_COLLECTION = "whistlegraph-curation-audit";
const DEPLOY_COLLECTION = "whistlegraph-deployments";
const MODEL_DIR = join(process.cwd(), "public", "whistlegraph.org");
const REPO_ROOT = join(process.cwd(), "..");
const VISUALS_PATH = join(REPO_ROOT, "toolchain", "whistlegraph", "downloads", "VISUALS.json");
const DEPLOY_SCRIPT = join(REPO_ROOT, "lith", "webhook.sh");
const execFile = promisify(execFileCallback);
const HEADERS = {
  "Cache-Control": "no-store",
  "Access-Control-Allow-Methods": "GET, PATCH, POST, DELETE, OPTIONS",
};

let modelCache = { stamp: "", workCodes: new Set(), workByCode: new Map(), postIds: new Set(), postWorks: new Map(), postById: new Map() };
let visualCache = { stamp: "", byId: new Map(), searchable: [], metadata: { count: 0 } };

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
    workByCode: new Map((graphs.works || []).map((work) => [String(work.code), work])),
    postIds: new Set((posts.posts || []).map((post) => String(post.id))),
    postWorks: new Map((posts.posts || []).map((post) => [String(post.id), post.works || []])),
    postById: new Map((posts.posts || []).map((post) => [String(post.id), post])),
  };
  return modelCache;
}

function loadVisualModel() {
  if (!existsSync(VISUALS_PATH)) return { byId: new Map(), searchable: [], metadata: { count: 0 } };
  const stamp = String(statSync(VISUALS_PATH).mtimeMs);
  if (visualCache.stamp === stamp) return visualCache;
  const data = JSON.parse(readFileSync(VISUALS_PATH, "utf8"));
  const records = Array.isArray(data.visuals) ? data.visuals : [];
  const enriched = records.map((record) => ({ ...record, autoTags: deriveVisualTags(record) }));
  visualCache = {
    stamp,
    byId: new Map(enriched.map((record) => [String(record.postId), record])),
    searchable: enriched.map((record) => ({ id: String(record.postId), text: `${visualSearchText(record)} ${record.autoTags.join(" ")}` })),
    metadata: { count: enriched.length, generated: data.generated || null, promptVersion: data.promptVersion || null },
  };
  return visualCache;
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

async function readCuration(database, loadModelFn = loadBaseModel) {
  const documents = await database.db.collection(COLLECTION).find({}).toArray();
  const payload = curationPayload(documents);
  const model = loadModelFn();
  const validCodes = new Set(model.workCodes);
  for (const code of Object.keys(payload.createdWorks)) validCodes.add(code);
  for (const code of payload.deletedWorks) validCodes.delete(code);
  const activeCodes = new Set();
  const materialized = new Map();
  for (const [postId, baseWorks] of model.postWorks || []) {
    const assigned = payload.posts[postId]?.works || baseWorks;
    const post = model.postById?.get(postId) || {};
    for (const code of assigned) if (validCodes.has(code)) {
      activeCodes.add(code);
      const row = materialized.get(code) || { perf: 0, views: 0, posts: [] };
      row.perf += 1;
      row.views += Number(post.views) || 0;
      row.posts.push(post);
      materialized.set(code, row);
    }
  }
  const materializedWorks = {};
  for (const [code, row] of materialized) {
    const work = { ...(model.workByCode?.get(code) || {}), ...(payload.works[code] || {}), ...(payload.createdWorks[code] || {}) };
    const featured = row.posts.find((post) => String(post.id) === String(work.featuredPost || ""));
    const hero = featured || row.posts.sort((a, b) => (Number(b.views) || 0) - (Number(a.views) || 0))[0];
    const thumb = hero?.thumb || (hero?.media !== "audio" && hero?.id
      ? `https://assets.aesthetic.computer/whistlegraph/index/posts/${hero.id}.jpg`
      : null);
    materializedWorks[code] = { perf: row.perf, views: row.views, ...(thumb ? { thumb } : {}) };
  }
  return { ...payload, activeCodes: [...activeCodes], materializedWorks };
}

function deployError(message, statusCode = 409) {
  const error = new Error(message);
  error.statusCode = statusCode;
  return error;
}

async function deployPushedCommit({ commit, changeId, branch, user }) {
  if (!existsSync(DEPLOY_SCRIPT) || REPO_ROOT !== "/opt/ac") {
    throw deployError("Whistlegraph deployment is available only on the production publisher.", 503);
  }
  await execFile("git", ["-C", REPO_ROOT, "fetch", "origin", "main", branch, "--quiet"], { timeout: 60_000 });
  const { stdout } = await execFile("git", ["-C", REPO_ROOT, "rev-parse", "origin/main"], { timeout: 10_000 });
  const originHead = stdout.trim();
  const { stdout: branchStdout } = await execFile("git", ["-C", REPO_ROOT, "rev-parse", `origin/${branch}`], { timeout: 10_000 });
  const { stdout: ancestry } = await execFile("git", ["-C", REPO_ROOT, "rev-list", "--parents", "-n", "1", commit], { timeout: 10_000 });
  const { stdout: changedStdout } = await execFile("git", ["-C", REPO_ROOT, "diff-tree", "--no-commit-id", "--name-only", "-r", "-z", commit], { timeout: 10_000 });
  const changed = changedStdout.split("\0").filter(Boolean);
  const { stdout: treeStdout } = await execFile("git", ["-C", REPO_ROOT, "ls-tree", "-r", "-z", commit, "--", "system/public/whistlegraph.org"], { timeout: 10_000 });
  const { stdout: message } = await execFile("git", ["-C", REPO_ROOT, "show", "-s", "--format=%B", commit], { timeout: 10_000 });
  try {
    validateDeployEvidence({
      commit,
      changeId,
      actorSub: user.sub,
      originHead,
      branchHead: branchStdout.trim(),
      ancestry,
      changed,
      treeEntries: treeStdout.split("\0").filter(Boolean),
      message,
    });
  } catch (error) {
    const conflict = /^(origin\/main|The review branch)/.test(error.message);
    throw deployError(`Refusing deploy: ${error.message}`, conflict ? 409 : 403);
  }
  const child = spawn(DEPLOY_SCRIPT, [], {
    cwd: REPO_ROOT,
    env: { ...process.env, DEPLOY_BRANCH: "main", EXPECTED_COMMIT: commit, WHISTLEGRAPH_CHANGE_ID: changeId || "" },
    stdio: "ignore",
  });
  child.on("error", (error) => console.error("Whistlegraph deploy launch failed:", error?.message || error));
  child.unref();
  return { queued: true, commit };
}

export function createHandler({
  authorizeFn = authorize,
  handleForFn = handleFor,
  connectFn = connect,
  allowedSubs = whistlegraphAdminSubs(),
  loadModelFn = loadBaseModel,
  loadVisualsFn = loadVisualModel,
  deployFn = deployPushedCommit,
} = {}) {
  let mutationIndexesReady = null;
  function ensureMutationIndexes(database) {
    if (!mutationIndexesReady) {
      mutationIndexesReady = Promise.all([
        database.db.collection(COLLECTION).createIndex({ type: 1, key: 1 }, { unique: true, background: true }),
        database.db.collection(AUDIT_COLLECTION).createIndex({ when: -1 }, { background: true }),
      ]).catch((error) => {
        mutationIndexesReady = null;
        throw error;
      });
    }
    return mutationIndexesReady;
  }
  return async function whistlegraphAdminHandler(event) {
    if (event.httpMethod === "OPTIONS") return respond(204, "", HEADERS);
    const action = event.queryStringParameters?.action || "session";

    if (event.httpMethod === "GET" && action === "curation") {
      let database;
      try {
        database = await connectFn();
        const { trashedWorks: _privateTrash, ...payload } = await readCuration(database, loadModelFn);
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
        const [curation, recent, deployments] = await Promise.all([
          readCuration(database, loadModelFn),
          database.db.collection(AUDIT_COLLECTION).find({}, { projection: { admin: 0 } }).sort({ when: -1 }).limit(30).toArray(),
          database.db.collection(DEPLOY_COLLECTION).find({}).sort({ when: -1 }).limit(20).toArray(),
        ]);
        let visualCoverage = { count: 0 };
        try { visualCoverage = loadVisualsFn().metadata; } catch (error) { console.error("Whistlegraph visual coverage unavailable:", error?.message || error); }
        return respond(200, { ...curation, recent, deployments, visualCoverage }, HEADERS);
      } finally {
        await database.disconnect?.();
      }
    }

    if (event.httpMethod === "GET" && action === "visual-search") {
      const query = String(event.queryStringParameters?.q || "").trim().toLowerCase();
      if (!query || query.length > 200) return respond(400, { message: "Send a search query up to 200 characters." }, HEADERS);
      const terms = query.split(/\s+/).filter(Boolean);
      let visuals;
      try { visuals = loadVisualsFn(); }
      catch (error) { return respond(503, { message: `Machine visual index unavailable: ${error.message}` }, HEADERS); }
      const ids = visuals.searchable.filter((entry) => terms.every((term) => entry.text.includes(term))).map((entry) => entry.id);
      return respond(200, { query, total: ids.length, ids }, HEADERS);
    }

    if (event.httpMethod === "GET" && action === "visual") {
      const id = String(event.queryStringParameters?.id || "").trim();
      if (!/^\d+$/.test(id)) return respond(400, { message: "Send a post id." }, HEADERS);
      let record;
      try { record = loadVisualsFn().byId.get(id); }
      catch (error) { return respond(503, { message: `Machine visual index unavailable: ${error.message}` }, HEADERS); }
      if (!record) return respond(404, { message: "Machine visual record not found." }, HEADERS);
      return respond(200, { record }, HEADERS);
    }

    if (event.httpMethod === "POST" && action === "deploy") {
      let body;
      try { body = parseBody(event); }
      catch (error) { return respond(400, { message: error.message }, HEADERS); }
      let request;
      try { request = normalizeDeployRequest(body); }
      catch (error) { return respond(400, { message: error.message }, HEADERS); }
      const { commit, changeId, branch } = request;
      const deploymentId = randomUUID();
      const database = await connectFn();
      let result;
      try {
        const deployments = database.db.collection(DEPLOY_COLLECTION);
        await deployments.createIndex({ when: -1 }, { background: true });
        await deployments.insertOne({
          _id: deploymentId,
          commit,
          changeId: changeId || null,
          branch: branch || null,
          actorSub: user.sub,
          status: "requested",
          when: new Date(),
        });
        try {
          result = await deployFn({ commit, changeId, branch, user });
          await deployments.updateOne({ _id: deploymentId }, { $set: { status: "queued", queuedAt: new Date() } });
        } catch (error) {
          await deployments.updateOne(
            { _id: deploymentId },
            { $set: { status: "refused", error: String(error.message || error).slice(0, 500), finishedAt: new Date() } },
          );
          return respond(error.statusCode || 500, { message: error.message || "Deployment could not start." }, HEADERS);
        }
      } finally {
        await database.disconnect?.();
      }
      return respond(202, { deploymentId, status: "queued", ...result }, HEADERS);
    }

    if (event.httpMethod === "POST" && action === "work") {
      let body;
      try { body = parseBody(event); }
      catch (error) { return respond(400, { message: error.message }, HEADERS); }
      let model;
      try { model = loadModelFn(); }
      catch (error) { return respond(500, { message: `Archive model unavailable: ${error.message}` }, HEADERS); }
      let key, patch;
      try {
        key = normalizeWorkCode(body.code);
        patch = normalizeWorkPatch(body);
      } catch (error) { return respond(400, { message: error.message }, HEADERS); }
      const database = await connectFn();
      try {
        await ensureMutationIndexes(database);
        const collection = database.db.collection(COLLECTION);
        const audit = database.db.collection(AUDIT_COLLECTION);
        const id = `work:${key}`;
        const previous = await collection.findOne({ _id: id });
        if (model.workCodes.has(key) || (previous && !previous.deleted)) {
          return respond(409, { message: `Whistlegraph [${key}] already exists.` }, HEADERS);
        }
        const now = new Date();
        await collection.updateOne(
          { _id: id },
          { $set: { type: "work", key, patch, created: true, deleted: false, updatedAt: now, updatedBy: user.sub } },
          { upsert: true },
        );
        await audit.insertOne({ type: "work", key, action: previous?.deleted ? "restore" : "create", before: previous?.patch || null, after: patch, admin: user.sub, when: now });
        return respond(201, { saved: true, created: true, type: "work", key, patch, revision: now.toISOString() }, HEADERS);
      } finally {
        await database.disconnect?.();
      }
    }

    if (event.httpMethod === "POST" && action === "work-rename") {
      let body;
      try { body = parseBody(event); }
      catch (error) { return respond(400, { message: error.message }, HEADERS); }
      let model;
      try { model = loadModelFn(); }
      catch (error) { return respond(500, { message: `Archive model unavailable: ${error.message}` }, HEADERS); }
      let from, key, patch;
      try {
        from = normalizeWorkCode(body.from);
        key = normalizeWorkCode(body.code);
        patch = normalizeWorkPatch(body);
      } catch (error) { return respond(400, { message: error.message }, HEADERS); }
      if (from === key) return respond(400, { message: "Choose a different code." }, HEADERS);
      const database = await connectFn();
      try {
        await ensureMutationIndexes(database);
        const collection = database.db.collection(COLLECTION);
        const audit = database.db.collection(AUDIT_COLLECTION);
        const [source, target, overlays] = await Promise.all([
          collection.findOne({ _id: `work:${from}` }),
          collection.findOne({ _id: `work:${key}` }),
          collection.find({ type: "post" }).toArray(),
        ]);
        if ((!model.workCodes.has(from) && !source?.created) || source?.deleted) return respond(404, { message: `Whistlegraph [${from}] not found.` }, HEADERS);
        if (model.workCodes.has(key) || (target && !target.deleted)) return respond(409, { message: `Whistlegraph [${key}] already exists.` }, HEADERS);
        const overlayById = new Map(overlays.filter(row=>row.type==="post").map(row=>[row.key,row]));
        const ids = new Set([...(model.postIds || []), ...overlayById.keys()]);
        const now = new Date();
        let reassigned = 0;
        for (const postId of ids) {
          const previous = overlayById.get(postId);
          const works = previous?.patch?.works || model.postWorks?.get(postId) || [];
          if (!works.includes(from)) continue;
          const nextWorks = [...new Set(works.map(code => code === from ? key : code))];
          const nextPatch = { ...(previous?.patch || {}), works: nextWorks };
          await collection.updateOne({ _id: `post:${postId}` }, { $set: { type: "post", key: postId, patch: nextPatch, updatedAt: now, updatedBy: user.sub } }, { upsert: true });
          reassigned += 1;
        }
        const oldPatch = source?.patch || model.workByCode?.get(from) || {};
        patch.asset = oldPatch.asset || from;
        await collection.updateOne({ _id: `work:${key}` }, { $set: { type: "work", key, patch, created: true, deleted: false, updatedAt: now, updatedBy: user.sub } }, { upsert: true });
        await collection.updateOne({ _id: `work:${from}` }, { $set: { type: "work", key: from, patch: oldPatch, created: Boolean(source?.created), deleted: true, renamedTo: key, updatedAt: now, updatedBy: user.sub } }, { upsert: true });
        await audit.insertOne({ type: "work", key: from, action: "rename", renamedTo: key, reassigned, before: oldPatch, after: patch, admin: user.sub, when: now });
        return respond(200, { saved: true, renamed: true, from, key, patch, reassigned, revision: now.toISOString() }, HEADERS);
      } finally { await database.disconnect?.(); }
    }

    if (event.httpMethod === "POST" && action === "bulk-assign") {
      let body;
      try { body = parseBody(event); } catch (error) { return respond(400, { message: error.message }, HEADERS); }
      let code;
      try { code = normalizeWorkCode(body.code); } catch (error) { return respond(400, { message: error.message }, HEADERS); }
      const postIds = [...new Set((Array.isArray(body.postIds) ? body.postIds : []).map(String))];
      if (!postIds.length || postIds.length > 500) return respond(400, { message: "Choose between 1 and 500 posts." }, HEADERS);
      const operation = body.operation === "remove" ? "remove" : "add";
      let model;
      try { model = loadModelFn(); } catch (error) { return respond(500, { message: `Archive model unavailable: ${error.message}` }, HEADERS); }
      if (postIds.some(id => !model.postIds.has(id))) return respond(404, { message: "One or more posts were not found." }, HEADERS);
      const database = await connectFn();
      try {
        await ensureMutationIndexes(database);
        const collection = database.db.collection(COLLECTION), audit = database.db.collection(AUDIT_COLLECTION);
        const work = await collection.findOne({ _id: `work:${code}` });
        if ((!model.workCodes.has(code) && !work?.created) || work?.deleted) return respond(404, { message: `Whistlegraph [${code}] not found.` }, HEADERS);
        const now = new Date();
        for (const postId of postIds) {
          const previous = await collection.findOne({ _id: `post:${postId}` });
          const current = previous?.patch?.works || model.postWorks?.get(postId) || [];
          const works = operation === "add" ? [...new Set([...current, code])] : current.filter(value => value !== code);
          await collection.updateOne({ _id: `post:${postId}` }, { $set: { type: "post", key: postId, patch: { ...(previous?.patch || {}), works }, updatedAt: now, updatedBy: user.sub } }, { upsert: true });
        }
        await audit.insertOne({ type: "post", key: `${postIds.length} posts`, action: `bulk-${operation}`, code, postIds, admin: user.sub, when: now });
        return respond(200, { saved: true, operation, code, count: postIds.length, revision: now.toISOString() }, HEADERS);
      } finally { await database.disconnect?.(); }
    }

    if (event.httpMethod !== "PATCH" && event.httpMethod !== "DELETE") {
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
    // Created works are stored only in the curation overlay and are checked below.

    const database = await connectFn();
    try {
      const collection = database.db.collection(COLLECTION);
      const audit = database.db.collection(AUDIT_COLLECTION);
      await ensureMutationIndexes(database);
      const id = `${type}:${key}`;
      const previous = await collection.findOne({ _id: id });

      if (type === "work" && !model.workCodes.has(key) && !previous?.created) {
        return respond(404, { message: "Whistlegraph not found." }, HEADERS);
      }

      if (event.httpMethod === "DELETE") {
        if (type !== "work") return respond(400, { message: "Only whistlegraphs can be retired." }, HEADERS);
        const now = new Date();
        const base = model.workByCode?.get(key) || {};
        const patch = previous?.patch || { title: base.title, by: base.by, year: base.year, c: base.c };
        await collection.updateOne(
          { _id: id },
          { $set: { type, key, patch, created: Boolean(previous?.created), deleted: true, updatedAt: now, updatedBy: user.sub } },
          { upsert: true },
        );
        await audit.insertOne({ type, key, action: "delete", before: patch, after: null, admin: user.sub, when: now });
        return respond(200, { saved: true, deleted: true, type, key, revision: now.toISOString() }, HEADERS);
      }

      if (body.restore === true) {
        if (type !== "work" || !previous?.deleted) return respond(400, { message: "This whistlegraph is not in trash." }, HEADERS);
        const now = new Date();
        await collection.updateOne(
          { _id: id },
          { $set: { type, key, patch: previous.patch || {}, created: Boolean(previous.created), deleted: false, updatedAt: now, updatedBy: user.sub } },
        );
        await audit.insertOne({ type, key, action: "restore", before: null, after: previous.patch || {}, admin: user.sub, when: now });
        return respond(200, { saved: true, restored: true, created: Boolean(previous.created), type, key, patch: previous.patch || {}, revision: now.toISOString() }, HEADERS);
      }

      if (body.reset === true) {
        await collection.deleteOne({ _id: id });
        await audit.insertOne({ type, key, action: "reset", before: previous?.patch || null, after: null, admin: user.sub, when: new Date() });
        return respond(200, { saved: true, reset: true, type, key, patch: null }, HEADERS);
      }

      let patch;
      try {
        const validWorkCodes = new Set(model.workCodes);
        if (type === "post") {
          const overlays = await collection.find({ type: "work", created: true, deleted: { $ne: true } }).toArray();
          for (const work of overlays) if (work.created && !work.deleted) validWorkCodes.add(work.key);
          for (const work of overlays) if (work.deleted) validWorkCodes.delete(work.key);
        }
        patch = type === "post"
          ? normalizePostPatch(body.patch, validWorkCodes)
          : normalizeWorkPatch(body.patch);
        if (type === "work" && previous?.patch?.asset) patch.asset = previous.patch.asset;
      } catch (error) {
        return respond(400, { message: error.message }, HEADERS);
      }
      const now = new Date();
      await collection.updateOne(
        { _id: id },
        { $set: { type, key, patch, created: Boolean(previous?.created), deleted: false, updatedAt: now, updatedBy: user.sub } },
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
