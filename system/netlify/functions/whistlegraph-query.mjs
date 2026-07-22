// Fast, public Whistlegraph reads backed by a Mongo-materialized projection.
// Generated JSON remains the reproducible source; the projection folds the
// live Desk curation overlay into queryable work/post documents.

import { existsSync, readFileSync, statSync } from "node:fs";
import { join } from "node:path";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { curationPayload, deriveVisualTags, visualSearchText } from "./whistlegraph-admin-lib.mjs";

const DIR = join(process.cwd(), "public", "whistlegraph.org");
const POSTS_PATH = join(DIR, "posts.json");
const GRAPHS_PATH = join(DIR, "graphs.json");
const VISUALS_PATH = join(process.cwd(), "..", "toolchain", "whistlegraph", "downloads", "VISUALS.json");
const CURATION = "whistlegraph-curation";
const POSTS = "whistlegraph-public-posts";
const WORKS = "whistlegraph-public-works";
const META = "whistlegraph-public-meta";
const HEADERS = { "Cache-Control": "public, max-age=15, stale-while-revalidate=60" };
const MAX_LIMIT = 100;
let syncPromise = null;
let indexesPromise = null;

const clean = (value, max = 120) => String(value || "").trim().slice(0, max);
const pageLimit = (value) => Math.max(1, Math.min(MAX_LIMIT, Number.parseInt(value || "40", 10) || 40));
const encodeCursor = (value) => Buffer.from(JSON.stringify(value)).toString("base64url");
const decodeCursor = (value) => {
  if (!value) return null;
  try { return JSON.parse(Buffer.from(String(value), "base64url").toString("utf8")); }
  catch { return null; }
};
const thumbFor = (post) => post?.thumb || (post?.media !== "audio" && post?.id
  ? `https://assets.aesthetic.computer/whistlegraph/index/posts/${post.id}.jpg`
  : null);
const searchText = (parts) => parts.filter(Boolean).join(" ").toLowerCase();

async function ensureIndexes(db) {
  if (indexesPromise) return indexesPromise;
  indexesPromise = Promise.all([
    db.collection(POSTS).createIndex({ works: 1, views: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ kind: 1, views: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ workCount: 1, date: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ kind: 1, date: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ plots: 1, views: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ tags: 1, views: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ date: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ views: -1, _id: 1 }, { background: true }),
    db.collection(POSTS).createIndex({ searchText: "text" }, { background: true, name: "post_taxonomy_text" }),
    db.collection(WORKS).createIndex({ views: -1, _id: 1 }, { background: true }),
    db.collection(WORKS).createIndex({ year: 1, _id: 1 }, { background: true }),
    db.collection(WORKS).createIndex({ titleLower: 1, _id: 1 }, { background: true }),
    db.collection(WORKS).createIndex({ byLower: 1, _id: 1 }, { background: true }),
    db.collection(WORKS).createIndex({ searchText: "text" }, { background: true, name: "work_taxonomy_text" }),
    db.collection(CURATION).createIndex({ updatedAt: -1 }, { background: true }),
  ]).catch((error) => { indexesPromise = null; throw error; });
  return indexesPromise;
}

async function projectionRevision(db) {
  const latest = await db.collection(CURATION).find({}).sort({ updatedAt: -1 }).limit(1).toArray();
  const visualsStamp = existsSync(VISUALS_PATH) ? statSync(VISUALS_PATH).mtimeMs : "none";
  return `${statSync(GRAPHS_PATH).mtimeMs}:${statSync(POSTS_PATH).mtimeMs}:${visualsStamp}:${latest[0]?.updatedAt?.toISOString?.() || "none"}`;
}

async function rebuildProjection(db, revision) {
  const [graphData, postData, documents] = await Promise.all([
    Promise.resolve(JSON.parse(readFileSync(GRAPHS_PATH, "utf8"))),
    Promise.resolve(JSON.parse(readFileSync(POSTS_PATH, "utf8"))),
    db.collection(CURATION).find({}).toArray(),
  ]);
  const visualData = existsSync(VISUALS_PATH) ? JSON.parse(readFileSync(VISUALS_PATH, "utf8")) : { visuals: [] };
  const visuals = new Map((visualData.visuals || []).map((record) => [String(record.postId), {
    ...(record.visual || {}),
    autoTags: deriveVisualTags(record),
    sampledSeconds: record.source?.sampledSeconds || [],
    model: record.provenance?.model || null,
    promptVersion: record.provenance?.promptVersion || null,
    generated: record.provenance?.createdAt || null,
    searchText: visualSearchText(record),
  }]));
  const overlay = curationPayload(documents);
  const deleted = new Set(overlay.deletedWorks || []);
  const workMap = new Map();
  for (const work of graphData.works || []) {
    if (!deleted.has(work.code)) workMap.set(work.code, { ...work, ...(overlay.works[work.code] || {}) });
  }
  for (const [code, work] of Object.entries(overlay.createdWorks || {})) {
    if (!deleted.has(code)) workMap.set(code, { code, ...work, kind: "graph", status: "confirmed" });
  }
  const aggregates = new Map();
  const posts = (postData.posts || []).map((base) => {
    const post = { ...base, ...(overlay.posts[base.id] || {}) };
    post.works = (post.works || []).filter((code) => workMap.has(code));
    post.workCount = post.works.length;
    post.relationships = post.works.map((work) => ({ work, role: "contributes" }));
    post.plots = post.plots || [];
    post.tags = post.tags || [];
    const visual = visuals.get(String(post.id));
    if (visual) post.visual = visual;
    post.searchText = searchText([post.id, post.desc, post.platform, post.kind, ...post.works, ...post.plots, ...post.tags, visual?.searchText, ...(visual?.autoTags || [])]);
    post._id = String(post.id);
    post.projectionRevision = revision;
    for (const code of post.works) {
      const row = aggregates.get(code) || { perf: 0, views: 0, posts: [] };
      row.perf += 1; row.views += Number(post.views) || 0; row.posts.push(post); aggregates.set(code, row);
    }
    return post;
  });
  const works = [...workMap.values()].flatMap((work) => {
    const row = aggregates.get(work.code); if (!row) return [];
    const selected = row.posts.find((post) => String(post.id) === String(work.featuredPost || ""));
    const hero = selected && thumbFor(selected)
      ? selected
      : [...row.posts].sort((a, b) => (Number(b.views) || 0) - (Number(a.views) || 0)).find(thumbFor);
    return [{
      ...work,
      _id: work.code,
      perf: row.perf,
      views: row.views,
      ...(thumbFor(hero) ? { thumb: thumbFor(hero) } : {}),
      titleLower: String(work.title || "").toLowerCase(),
      byLower: String(work.by || "").toLowerCase(),
      searchText: searchText([work.code, work.title, work.by, work.year]),
      projectionRevision: revision,
    }];
  });
  if (posts.length) await db.collection(POSTS).bulkWrite(posts.map((post) => ({
    replaceOne: { filter: { _id: post._id }, replacement: post, upsert: true },
  })), { ordered: false });
  if (works.length) await db.collection(WORKS).bulkWrite(works.map((work) => ({
    replaceOne: { filter: { _id: work._id }, replacement: work, upsert: true },
  })), { ordered: false });
  await Promise.all([
    db.collection(POSTS).deleteMany({ projectionRevision: { $ne: revision } }),
    db.collection(WORKS).deleteMany({ projectionRevision: { $ne: revision } }),
    db.collection(META).updateOne({ _id: "projection" }, { $set: { revision, posts: posts.length, works: works.length, updatedAt: new Date() } }, { upsert: true }),
  ]);
}

async function ensureProjection(db) {
  if (syncPromise) return syncPromise;
  syncPromise = (async () => {
    await ensureIndexes(db);
    const revision = await projectionRevision(db);
    const current = await db.collection(META).findOne({ _id: "projection" });
    if (current?.revision !== revision) await rebuildProjection(db, revision);
    return revision;
  })().finally(() => { syncPromise = null; });
  return syncPromise;
}

function cursorQuery(field, direction, cursor) {
  if (!cursor || cursor.v === undefined || !cursor.id) return null;
  const op = direction < 0 ? "$lt" : "$gt";
  return { $or: [{ [field]: { [op]: cursor.v } }, { [field]: cursor.v, _id: { $gt: cursor.id } }] };
}

export async function listCollection(collection, params, kind) {
  const limit = pageLimit(params.limit);
  const q = clean(params.q, 200);
  const cursor = decodeCursor(params.cursor);
  const filter = {};
  let sortField, direction;
  if (kind === "posts") {
    if (params.work) filter.works = clean(params.work, 24).toLowerCase();
    if (params.kind === "unfiled") filter.workCount = 0;
    else if (params.kind && params.kind !== "all") filter.kind = clean(params.kind, 24);
    if (params.plot) filter.plots = clean(params.plot, 100);
    if (params.tag) filter.tags = clean(params.tag, 80);
    sortField = params.sort === "oldest" || params.sort === "newest" ? "date" : "views";
    direction = params.sort === "oldest" ? 1 : -1;
  } else {
    sortField = params.sort === "year" ? "year" : params.sort === "title" ? "titleLower" : params.sort === "author" ? "byLower" : "views";
    direction = sortField === "views" ? -1 : 1;
  }
  if (q) filter.$text = { $search: q };
  const ranged = !q ? cursorQuery(sortField, direction, cursor) : null;
  const query = ranged ? { $and: [filter, ranged] } : filter;
  const total = await collection.countDocuments(filter);
  let find = collection.find(query, { projection: { projectionRevision: 0, searchText: 0, titleLower: 0, byLower: 0, visual: 0 } });
  if (q) find = find.sort({ score: { $meta: "textScore" }, _id: 1 }).skip(Number(cursor?.offset) || 0);
  else find = find.sort({ [sortField]: direction, _id: 1 });
  const rows = await find.limit(limit + 1).toArray();
  const hasMore = rows.length > limit;
  const items = rows.slice(0, limit);
  const last = items.at(-1);
  const nextCursor = hasMore && last
    ? encodeCursor(q ? { offset: (Number(cursor?.offset) || 0) + limit } : { v: last[sortField] ?? null, id: last._id })
    : null;
  return { items, nextCursor, limit, total };
}

export function createHandler({ connectFn = connect } = {}) {
  return async (event) => {
    if (event.httpMethod !== "GET") return respond(405, { message: "Method Not Allowed." }, HEADERS);
    let database;
    try {
      database = await connectFn();
      await ensureProjection(database.db);
      const p = event.queryStringParameters || {};
      const action = p.action || "posts";
      if (action === "post") {
        const item = await database.db.collection(POSTS).findOne({ _id: clean(p.id, 32) }, { projection: { projectionRevision: 0, searchText: 0 } });
        return item ? respond(200, { item }, HEADERS) : respond(404, { message: "Post not found." }, HEADERS);
      }
      if (action === "work") {
        const code = clean(p.code, 24).toLowerCase();
        const item = await database.db.collection(WORKS).findOne({ _id: code }, { projection: { projectionRevision: 0, searchText: 0, titleLower: 0, byLower: 0 } });
        if (!item) return respond(404, { message: "Whistlegraph not found." }, HEADERS);
        const page = await listCollection(database.db.collection(POSTS), { ...p, work: code }, "posts");
        return respond(200, { item, posts: page.items, nextCursor: page.nextCursor }, HEADERS);
      }
      const kind = action === "works" ? "works" : "posts";
      const page = await listCollection(database.db.collection(kind === "works" ? WORKS : POSTS), p, kind);
      return respond(200, { ...page, action: kind }, HEADERS);
    } catch (error) {
      console.error("Whistlegraph query failed:", error?.message || error);
      return respond(500, { message: "Whistlegraph query unavailable." }, { ...HEADERS, "Cache-Control": "no-store" });
    } finally {
      try { await database?.disconnect?.(); } catch {}
    }
  };
}

export const handler = createHandler();
