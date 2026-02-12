#!/usr/bin/env node
// silo - data & storage dashboard for aesthetic.computer

import "dotenv/config";
import express from "express";
import https from "https";
import http from "http";
import fs from "fs";
import { MongoClient } from "mongodb";
import { createClient } from "redis";
import { S3Client, ListObjectsV2Command } from "@aws-sdk/client-s3";
import { WebSocketServer } from "ws";

const app = express();
const PORT = process.env.PORT || 3003;
const dev = process.env.NODE_ENV === "development";
const SERVER_START_TIME = Date.now();

// --- Auth0 ---
const AUTH0_DOMAIN = process.env.AUTH0_DOMAIN || "aesthetic.us.auth0.com";
const AUTH0_CLIENT_ID = process.env.AUTH0_CLIENT_ID || "";
const AUTH0_CUSTOM_DOMAIN = "hi.aesthetic.computer";
const ADMIN_SUB = process.env.ADMIN_SUB || "";
const AUTH_CACHE_TTL = 300_000;
const authCache = new Map();

// --- Activity Log ---
const activityLog = [];
const MAX_LOG = 200;
let wss = null;

// --- Firehose (MongoDB change stream) ---
let changeStream = null;
const firehoseThrottle = { count: 0, resetAt: 0 };
const FIREHOSE_MAX_PER_SEC = 50;
const firehoseBuffer = [];
const FIREHOSE_BUFFER_SIZE = 50;

function log(type, msg) {
  const entry = { time: new Date().toISOString(), type, msg };
  activityLog.unshift(entry);
  if (activityLog.length > MAX_LOG) activityLog.pop();
  if (wss?.clients) {
    const data = JSON.stringify({ logEntry: entry });
    wss.clients.forEach((c) => c.readyState === 1 && c.send(data));
  }
  const prefix = { info: "  ", warn: "! ", error: "x " }[type] || "  ";
  console.log(`${prefix}${msg}`);
}

log("info", "silo starting...");

// Parse a MongoDB URI into a display-friendly host string (no credentials)
function parseMongoHost(uri) {
  if (!uri) return "not set";
  try {
    const u = new URL(uri.replace("mongodb+srv://", "https://").replace("mongodb://", "http://"));
    return u.hostname + (u.port ? ":" + u.port : "");
  } catch { return uri.split("@").pop()?.split("/")[0] || uri; }
}

// --- MongoDB (primary) ---
let mongoClient, db;

async function connectMongo() {
  const uri = process.env.MONGODB_CONNECTION_STRING;
  if (!uri) { log("error", "MONGODB_CONNECTION_STRING not set"); return; }
  try {
    const isLocal = uri.includes("localhost") || uri.includes("127.0.0.1");
    mongoClient = new MongoClient(uri, {
      ...(isLocal ? {} : { tls: true }),
      serverSelectionTimeoutMS: 10000,
      connectTimeoutMS: 10000,
      socketTimeoutMS: 45000,
      maxPoolSize: 5,
    });
    await mongoClient.connect();
    db = mongoClient.db(process.env.MONGODB_NAME || "aesthetic");
    log("info", `mongo connected (${parseMongoHost(uri)})`);
  } catch (err) {
    log("error", `mongo connect failed: ${err.message}`);
  }
}

// Extract a short summary from the full document (admin-only dashboard, safe to show)
function firehoseSummary(coll, op, doc) {
  if (!doc || op === "delete") return null;
  switch (coll) {
    case "chat-system": case "chat-clock": case "chat-sotce":
      return doc.user ? `@${doc.user}` : (doc.text?.slice(0, 40) || null);
    case "@handles": return doc.handle ? `@${doc.handle}` : null;
    case "users": return doc.email?.split("@")[0] || doc.name || null;
    case "paintings": return doc.slug || doc.title || null;
    case "tapes": return doc.piece || null;
    case "pieces": return doc.slug || doc.name || null;
    case "kidlisp": return doc.name || null;
    case "moods": return doc.mood || null;
    case "verifications": return doc.handle ? `@${doc.handle}` : null;
    default: return null;
  }
}

function startFirehose() {
  if (!db) return;
  try {
    changeStream = db.watch(
      [{ $match: { operationType: { $in: ["insert", "update", "replace", "delete"] } } }],
      { fullDocument: "updateLookup" }
    );
    log("info", "firehose change stream started");

    changeStream.on("change", (change) => {
      const now = Date.now();
      if (now > firehoseThrottle.resetAt) {
        firehoseThrottle.count = 0;
        firehoseThrottle.resetAt = now + 1000;
      }
      if (firehoseThrottle.count >= FIREHOSE_MAX_PER_SEC) return;
      firehoseThrottle.count++;

      const coll = change.ns?.coll || "unknown";
      const op = change.operationType;
      const event = {
        ns: coll,
        op,
        time: now,
        id: change.documentKey?._id?.toString() || null,
        summary: firehoseSummary(coll, op, change.fullDocument),
      };

      // Buffer for history replay on new connections
      firehoseBuffer.push(event);
      if (firehoseBuffer.length > FIREHOSE_BUFFER_SIZE) firehoseBuffer.shift();

      if (wss?.clients) {
        const data = JSON.stringify({ firehose: event });
        wss.clients.forEach((c) => c.readyState === 1 && c.send(data));
      }
    });

    changeStream.on("error", (err) => {
      log("error", `firehose stream error: ${err.message}`);
      changeStream = null;
      setTimeout(startFirehose, 5000);
    });
  } catch (err) {
    log("error", `firehose setup failed: ${err.message}`);
  }
}

// --- MongoDB Atlas (for comparison) ---
let atlasClient, atlasDb;

async function connectAtlas() {
  const uri = process.env.ATLAS_CONNECTION_STRING;
  if (!uri) { log("info", "ATLAS_CONNECTION_STRING not set, skipping atlas"); return; }
  // Skip if same as primary
  if (uri === process.env.MONGODB_CONNECTION_STRING) {
    log("info", "atlas URI same as primary, sharing connection");
    atlasClient = mongoClient;
    atlasDb = db;
    return;
  }
  try {
    atlasClient = new MongoClient(uri, {
      tls: true,
      serverSelectionTimeoutMS: 10000,
      connectTimeoutMS: 10000,
      socketTimeoutMS: 45000,
      maxPoolSize: 3,
    });
    await atlasClient.connect();
    atlasDb = atlasClient.db(process.env.MONGODB_NAME || "aesthetic");
    log("info", "atlas connected (comparison)");
  } catch (err) {
    log("error", `atlas connect failed: ${err.message}`);
  }
}

// --- Redis ---
let redisClient;

async function connectRedis() {
  const url = process.env.REDIS_CONNECTION_STRING;
  if (!url) { log("info", "REDIS_CONNECTION_STRING not set, skipping redis"); return; }
  try {
    redisClient = createClient({ url });
    redisClient.on("error", (err) => log("error", `redis error: ${err.message}`));
    await redisClient.connect();
    log("info", "redis connected");
  } catch (err) {
    log("error", `redis connect failed: ${err.message}`);
  }
}

// --- S3 (DigitalOcean Spaces) ---
const s3 = new S3Client({
  endpoint: process.env.SPACES_ENDPOINT,
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.SPACES_KEY || "",
    secretAccessKey: process.env.SPACES_SECRET || "",
  },
  forcePathStyle: false,
});
const BUCKETS = (process.env.BUCKETS || "").split(",").filter(Boolean);

// --- Cached Stats ---
let cachedDbStats = null, cachedStorageStats = null, cachedAtlasStats = null;
let dbStatsAge = 0, storageStatsAge = 0, atlasStatsAge = 0;
const DB_CACHE_TTL = 30_000;
const STORAGE_CACHE_TTL = 300_000;

async function getCollectionStats(database, label) {
  if (!database) return null;
  try {
    const collections = await database.listCollections().toArray();
    const stats = [];
    for (const col of collections) {
      const count = await database.collection(col.name).estimatedDocumentCount();
      stats.push({ name: col.name, count });
    }
    stats.sort((a, b) => b.count - a.count);
    return stats;
  } catch (err) {
    log("error", `${label} stats error: ${err.message}`);
    return null;
  }
}

async function getDbStats() {
  if (cachedDbStats && Date.now() - dbStatsAge < DB_CACHE_TTL) return cachedDbStats;
  const stats = await getCollectionStats(db, "primary");
  if (stats) { cachedDbStats = stats; dbStatsAge = Date.now(); }
  return cachedDbStats;
}

async function getAtlasStats() {
  if (cachedAtlasStats && Date.now() - atlasStatsAge < DB_CACHE_TTL) return cachedAtlasStats;
  const stats = await getCollectionStats(atlasDb, "atlas");
  if (stats) { cachedAtlasStats = stats; atlasStatsAge = Date.now(); }
  return cachedAtlasStats;
}

async function getDbSize(database) {
  if (!database) return null;
  try {
    const stats = await database.stats();
    return { dataSize: stats.dataSize, storageSize: stats.storageSize, indexSize: stats.indexSize };
  } catch { return null; }
}

async function getStorageStats() {
  if (cachedStorageStats && Date.now() - storageStatsAge < STORAGE_CACHE_TTL) return cachedStorageStats;
  try {
    const results = [];
    for (const bucket of BUCKETS) {
      let totalSize = 0, totalObjects = 0, continuationToken;
      do {
        const resp = await s3.send(new ListObjectsV2Command({
          Bucket: bucket, ContinuationToken: continuationToken,
        }));
        if (resp.Contents) {
          totalObjects += resp.Contents.length;
          totalSize += resp.Contents.reduce((sum, obj) => sum + (obj.Size || 0), 0);
        }
        continuationToken = resp.IsTruncated ? resp.NextContinuationToken : undefined;
      } while (continuationToken);
      results.push({ bucket, objects: totalObjects, bytes: totalSize, gb: (totalSize / 1e9).toFixed(2) });
    }
    cachedStorageStats = results;
    storageStatsAge = Date.now();
    return results;
  } catch (err) {
    log("error", `storage stats error: ${err.message}`);
    return cachedStorageStats || [];
  }
}

async function getRedisStats() {
  if (!redisClient?.isReady) return null;
  try {
    const info = await redisClient.info();
    const parse = (section, key) => {
      const m = info.match(new RegExp(`${key}:(.+)`));
      return m ? m[1].trim() : null;
    };
    return {
      connected: true,
      version: parse("server", "redis_version"),
      usedMemory: parse("memory", "used_memory_human"),
      peakMemory: parse("memory", "used_memory_peak_human"),
      totalKeys: parseInt(parse("keyspace", "keys") || "0") || await redisClient.dbSize(),
      connectedClients: parseInt(parse("clients", "connected_clients") || "0"),
      uptimeSeconds: parseInt(parse("server", "uptime_in_seconds") || "0"),
      hitRate: (() => {
        const hits = parseInt(parse("stats", "keyspace_hits") || "0");
        const misses = parseInt(parse("stats", "keyspace_misses") || "0");
        return hits + misses > 0 ? ((hits / (hits + misses)) * 100).toFixed(1) : "n/a";
      })(),
    };
  } catch (err) {
    log("error", `redis stats error: ${err.message}`);
    return { connected: false, error: err.message };
  }
}

// --- Auth ---
async function validateToken(authorization) {
  if (!authorization) return null;
  const cached = authCache.get(authorization);
  if (cached && Date.now() - cached.timestamp < AUTH_CACHE_TTL) return cached;
  try {
    const resp = await fetch(`https://${AUTH0_DOMAIN}/userinfo`, {
      headers: { Authorization: authorization },
    });
    if (!resp.ok) return null;
    const user = await resp.json();
    let handle = null, isAdmin = false;
    if (db) {
      const doc = await db.collection("@handles").findOne({ _id: user.sub });
      handle = doc?.handle;
      isAdmin = !!(user.email_verified && handle === "jeffrey" && user.sub === ADMIN_SUB);
    }
    const result = { user, handle, isAdmin, timestamp: Date.now() };
    authCache.set(authorization, result);
    if (isAdmin) log("info", `auth: @${handle} verified`);
    return result;
  } catch (err) {
    log("error", `token validation failed: ${err.message}`);
    return null;
  }
}

async function requireAdmin(req, res, next) {
  const auth = await validateToken(req.headers.authorization);
  if (!auth) return res.status(401).json({ error: "Unauthorized" });
  if (!auth.isAdmin) return res.status(403).json({ error: "Admin access required" });
  req.auth = auth;
  next();
}

// --- Express Routes ---
app.use(express.json());

app.get("/auth/config", (req, res) => {
  res.json({ domain: AUTH0_CUSTOM_DOMAIN, clientId: AUTH0_CLIENT_ID });
});

app.get("/auth/me", async (req, res) => {
  const auth = await validateToken(req.headers.authorization);
  if (!auth) return res.status(401).json({ error: "Unauthorized" });
  res.json({ handle: auth.handle, isAdmin: auth.isAdmin });
});

app.use("/api", requireAdmin);

app.get("/health", async (req, res) => {
  const mongoOk = db ? await db.admin().ping().then(() => true).catch(() => false) : false;
  res.json({ status: "ok", uptime: Date.now() - SERVER_START_TIME, mongo: mongoOk });
});

app.get("/api/overview", async (req, res) => {
  log("info", "overview requested");
  const [collections, storage, redis, dbSize] = await Promise.all([
    getDbStats(), getStorageStats(), getRedisStats(), getDbSize(db),
  ]);
  const find = (name) => collections?.find((c) => c.name === name)?.count || 0;
  res.json({
    db: {
      users: find("users"), handles: find("@handles"), paintings: find("paintings"),
      pieces: find("pieces"), kidlisp: find("kidlisp"), moods: find("moods"),
      chatSystem: find("chat-system"), chatClock: find("chat-clock"),
      verifications: find("verifications"),
      totalCollections: collections?.length || 0,
      totalDocuments: collections?.reduce((s, c) => s + c.count, 0) || 0,
      size: dbSize,
    },
    storage: {
      buckets: storage,
      totalGB: storage.reduce((s, b) => s + parseFloat(b.gb), 0).toFixed(2),
    },
    redis,
    uptime: Date.now() - SERVER_START_TIME,
  });
});

app.get("/api/db/collections", async (req, res) => {
  log("info", "collections requested");
  res.json(await getDbStats() || []);
});

app.get("/api/db/compare", async (req, res) => {
  log("info", "db compare requested");
  const [primary, atlas, primarySize, atlasSize] = await Promise.all([
    getDbStats(), getAtlasStats(), getDbSize(db), getDbSize(atlasDb),
  ]);
  const primaryHost = parseMongoHost(process.env.MONGODB_CONNECTION_STRING);
  const atlasHost = parseMongoHost(process.env.ATLAS_CONNECTION_STRING);

  // The production Netlify site uses Atlas until we switch its env vars
  const activeDb = "atlas";

  const allNames = new Set([
    ...(primary || []).map(c => c.name),
    ...(atlas || []).map(c => c.name),
  ]);
  const comparison = [...allNames].sort().map(name => {
    const p = primary?.find(c => c.name === name);
    const a = atlas?.find(c => c.name === name);
    return {
      name,
      primary: p?.count ?? null,
      atlas: a?.count ?? null,
      synced: p?.count === a?.count,
    };
  });
  const allSynced = comparison.every(c => c.synced);

  res.json({
    primaryLabel: primaryHost, atlasLabel: atlasHost,
    activeDb, allSynced,
    primarySize, atlasSize,
    primaryConnected: !!db,
    atlasConnected: !!atlasDb,
    sameConnection: process.env.MONGODB_CONNECTION_STRING === process.env.ATLAS_CONNECTION_STRING,
    collections: comparison,
  });
});

// Sync: copy all collections from Atlas → primary
let syncInProgress = false;
app.post("/api/db/sync", async (req, res) => {
  if (syncInProgress) return res.status(409).json({ error: "Sync already in progress" });
  if (!db || !atlasDb) return res.status(500).json({ error: "Both databases must be connected" });
  if (atlasDb === db) return res.status(400).json({ error: "Primary and Atlas are the same connection" });

  syncInProgress = true;
  log("info", "sync started: atlas -> primary");

  try {
    const collections = await atlasDb.listCollections().toArray();
    const results = [];
    for (const col of collections) {
      const name = col.name;
      const docs = await atlasDb.collection(name).find({}).toArray();
      // Drop and re-insert
      await db.collection(name).deleteMany({});
      if (docs.length > 0) {
        await db.collection(name).insertMany(docs);
      }
      results.push({ name, docs: docs.length });
      log("info", `synced ${name}: ${docs.length} docs`);
    }
    // Invalidate cache
    cachedDbStats = null;
    dbStatsAge = 0;
    log("info", `sync complete: ${results.length} collections`);
    res.json({ ok: true, collections: results });
  } catch (err) {
    log("error", `sync failed: ${err.message}`);
    res.status(500).json({ error: err.message });
  } finally {
    syncInProgress = false;
  }
});

app.get("/api/db/health", async (req, res) => {
  if (!db) return res.json({ connected: false });
  try {
    const status = await db.admin().serverStatus();
    res.json({
      connected: true, version: status.version, uptime: status.uptime,
      connections: status.connections, opcounters: status.opcounters,
    });
  } catch (err) { res.json({ connected: false, error: err.message }); }
});

app.get("/api/redis", async (req, res) => {
  log("info", "redis stats requested");
  res.json(await getRedisStats() || { connected: false });
});

app.get("/api/storage/buckets", async (req, res) => {
  log("info", "storage buckets requested");
  res.json(await getStorageStats());
});

app.get("/api/services/oven", async (req, res) => {
  const url = process.env.OVEN_URL || "https://localhost:3002";
  try {
    const start = Date.now();
    const resp = await fetch(`${url}/health`, { signal: AbortSignal.timeout(5000) });
    const data = await resp.json();
    res.json({ status: "ok", responseMs: Date.now() - start, ...data });
  } catch (err) { res.json({ status: "unreachable", error: err.message }); }
});

app.get("/api/services/session", async (req, res) => {
  const url = process.env.SESSION_URL || "https://localhost:8889";
  try {
    const start = Date.now();
    const resp = await fetch(`${url}/health`, { signal: AbortSignal.timeout(5000) });
    const data = await resp.json();
    res.json({ status: "ok", responseMs: Date.now() - start, ...data });
  } catch (err) { res.json({ status: "unreachable", error: err.message }); }
});

// --- Dashboard ---
app.get("/", (req, res) => {
  res.setHeader("Content-Type", "text/html");
  res.send(DASHBOARD_HTML);
});

const DASHBOARD_HTML = `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>silo</title>
<link rel="icon" href="https://aesthetic.computer/icon/128x128/prompt.png" type="image/png" />
<link rel="stylesheet" href="https://aesthetic.computer/type/webfonts/berkeley-mono-variable.css" />
<style>
:root {
  --bg: #111; --bg2: #1a1a1a; --fg: #ccc; --fg2: #888;
  --accent: #6c6; --border: #333; --hover: #222;
  --ok: #6c6; --err: #c66; --warn: #cc6;
}
[data-theme="light"] {
  --bg: #f4f4f4; --bg2: #fff; --fg: #222; --fg2: #777;
  --accent: #396; --border: #ccc; --hover: #eee;
  --ok: #396; --err: #c44; --warn: #a80;
}
* { margin: 0; padding: 0; box-sizing: border-box; }
html, body { height: 100%; overflow: hidden; }
body {
  font-family: 'Berkeley Mono Variable', monospace;
  font-size: 13px; line-height: 1.4;
  background: var(--bg); color: var(--fg);
  cursor: url('https://aesthetic.computer/aesthetic.computer/cursors/precise.svg') 12 12, auto;
}
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }

/* login */
#login {
  display: flex; position: fixed; inset: 0; z-index: 100;
  background: var(--bg); align-items: center; justify-content: center;
  flex-direction: column; gap: 8px;
}
#login h1 { font-size: 18px; font-weight: normal; color: var(--fg); letter-spacing: 3px; }
#login p { font-size: 11px; color: var(--fg2); }
#loginBtn {
  font-family: inherit; font-size: 12px; padding: 6px 16px; margin-top: 8px;
  background: var(--bg2); color: var(--fg); border: 1px solid var(--border);
  cursor: pointer;
}
#loginBtn:hover { border-color: var(--accent); }
#authStatus { font-size: 11px; color: var(--fg2); }

/* layout */
#dashboard {
  display: none; flex-direction: column; height: 100vh; overflow: hidden;
}
#dashboard.visible { display: flex; }

/* header */
.bar {
  display: flex; align-items: center; gap: 6px; padding: 6px 8px;
  border-bottom: 1px solid var(--border);
  background: var(--bg); z-index: 10; flex-shrink: 0; flex-wrap: wrap;
}
.bar-title { font-size: 14px; color: var(--fg); cursor: pointer; letter-spacing: 2px; }
.bar-title:hover { color: var(--accent); }
.bar-sep { color: var(--border); }
.dot { width: 7px; height: 7px; border-radius: 50%; background: var(--fg2); display: inline-block; flex-shrink: 0; }
.dot.ok { background: var(--ok); }
.dot.err { background: var(--err); }
.bar-dim { font-size: 11px; color: var(--fg2); }
.bar-right { margin-left: auto; display: flex; align-items: center; gap: 6px; }
.btn {
  font-family: inherit; font-size: 11px; padding: 2px 8px;
  background: var(--bg2); color: var(--fg2); border: 1px solid var(--border);
  cursor: pointer;
}
.btn:hover { color: var(--fg); border-color: var(--fg2); }

/* tabs */
.tab-bar {
  display: flex; overflow-x: auto; white-space: nowrap;
  gap: 0; border-bottom: 1px solid var(--border);
  padding: 0 6px; background: var(--bg); flex-shrink: 0;
}
.tab-btn {
  font-family: inherit; font-size: 11px; padding: 5px 10px;
  background: none; color: var(--fg2); border: none; border-bottom: 2px solid transparent;
  cursor: pointer; white-space: nowrap;
}
.tab-btn:hover { color: var(--fg); }
.tab-btn.active { color: var(--accent); border-bottom-color: var(--accent); }

/* panels */
.panels { flex: 1; overflow: hidden; position: relative; }
.panel {
  display: none; position: absolute; inset: 0;
  overflow-y: auto; padding: 8px;
}
.panel.active { display: block; }

/* overview two-col on wide screens */
.overview-grid { display: grid; grid-template-columns: 1fr; gap: 6px; }
@media (min-width: 700px) {
  .overview-grid { grid-template-columns: 1fr 1fr; }
}

.card {
  border: 1px solid var(--border); background: var(--bg2); padding: 8px;
}
.card-hd {
  font-size: 11px; color: var(--fg2); text-transform: uppercase;
  letter-spacing: 1px; padding-bottom: 4px; margin-bottom: 6px;
  border-bottom: 1px solid var(--border);
  display: flex; justify-content: space-between; align-items: center;
}
.card-hd b { color: var(--fg); font-weight: normal; }

/* stats */
.kv { display: flex; justify-content: space-between; padding: 2px 0; font-size: 12px; }
.kv .k { color: var(--fg2); }
.kv .v { color: var(--fg); font-variant-numeric: tabular-nums; }
.kv .v.ok { color: var(--ok); }
.kv .v.err { color: var(--err); }
.kv .v.warn { color: var(--warn); }

/* table */
.tbl { width: 100%; border-collapse: collapse; font-size: 12px; }
.tbl th { text-align: left; font-weight: normal; color: var(--fg2); font-size: 10px;
  text-transform: uppercase; letter-spacing: 1px; padding: 2px 4px;
  border-bottom: 1px solid var(--border); }
.tbl td { padding: 2px 4px; border-bottom: 1px solid color-mix(in srgb, var(--border) 50%, transparent); }
.tbl td.r { text-align: right; font-variant-numeric: tabular-nums; }
.tbl tr:hover td { background: var(--hover); }

/* storage bars */
.sbar { display: flex; align-items: center; gap: 6px; padding: 3px 0; font-size: 12px; }
.sbar-name { min-width: 100px; color: var(--fg); overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.sbar-track { flex: 1; height: 6px; background: var(--border); overflow: hidden; }
.sbar-fill { height: 100%; background: var(--accent); }
.sbar-val { min-width: 80px; text-align: right; color: var(--fg2); font-size: 11px; font-variant-numeric: tabular-nums; }

/* log */
.log-panel { display: none; position: absolute; inset: 0; flex-direction: column; }
.log-panel.active { display: flex; }
.log-hd {
  font-size: 11px; color: var(--fg2); text-transform: uppercase;
  letter-spacing: 1px; padding: 8px 8px 4px; flex-shrink: 0;
}
.log-hd b { color: var(--fg); font-weight: normal; }
.log-scroll { flex: 1; overflow-y: auto; padding: 0 8px 8px; }
.log-row { display: flex; gap: 8px; padding: 1px 0; font-size: 11px; }
.log-t { color: var(--fg2); min-width: 60px; font-variant-numeric: tabular-nums; }
.log-m { color: var(--fg); word-break: break-word; }
.log-m.error { color: var(--err); }
.log-m.warn { color: var(--warn); }

/* compare */
.sync-badge { font-size: 10px; padding: 1px 4px; border: 1px solid; display: inline-block; }
.sync-badge.synced { color: var(--ok); border-color: var(--ok); }
.sync-badge.unsynced { color: var(--err); border-color: var(--err); }
.active-badge { font-size: 10px; padding: 1px 4px; background: var(--accent); color: var(--bg); }

.loading { color: var(--fg2); }

/* sync button */
.sync-btn {
  font-family: inherit; font-size: 10px; padding: 1px 6px;
  background: var(--bg); color: var(--fg2); border: 1px solid var(--border);
  cursor: pointer; margin-left: 4px;
}
.sync-btn:hover { color: var(--fg); border-color: var(--fg2); }
.sync-btn:disabled { opacity: 0.5; cursor: not-allowed; }

/* firehose */
.firehose-panel { padding: 0 !important; overflow: hidden !important; }
.firehose-panel canvas { display: block; width: 100%; height: 100%; }
.firehose-hud {
  position: absolute; top: 8px; right: 12px; z-index: 2;
  display: flex; gap: 12px; font-size: 11px; color: var(--fg2);
  pointer-events: none;
}
.firehose-counter { color: var(--fg); }
.firehose-rate { color: var(--accent); }
.firehose-sidebar {
  position: absolute; bottom: 8px; left: 8px; z-index: 2;
  font-size: 10px; pointer-events: none;
  max-height: calc(100% - 40px); overflow: hidden;
}
.fh-counter-row {
  display: flex; align-items: center; gap: 4px; padding: 1px 0;
  opacity: 0.7;
}
.fh-counter-dot { width: 6px; height: 6px; border-radius: 50%; flex-shrink: 0; }
.fh-counter-ns { color: var(--fg); min-width: 80px; }
.fh-counter-val { color: var(--fg); font-variant-numeric: tabular-nums; min-width: 30px; text-align: right; }
.fh-counter-ops { color: var(--fg2); margin-left: 2px; }

@media (max-width: 640px) {
  .bar { gap: 4px; padding: 4px 6px; }
  body { font-size: 12px; }
  .card { padding: 6px; }
}
</style>
</head>
<body>
<div id="login">
  <h1>silo</h1>
  <p>aesthetic.computer data dashboard</p>
  <button id="loginBtn">sign in</button>
  <p id="authStatus">loading...</p>
</div>

<div id="dashboard">
  <div class="bar">
    <span class="bar-title" id="logo">silo</span>
    <span class="bar-sep">/</span>
    <span class="dot" id="wsStatus"></span>
    <span class="bar-dim" id="wsStatusText">ws</span>
    <span class="bar-sep">/</span>
    <span class="dot" id="mongoStatus"></span>
    <span class="bar-dim" id="mongoStatusText">db</span>
    <span class="bar-sep">/</span>
    <span class="dot" id="redisStatus"></span>
    <span class="bar-dim" id="redisStatusText">redis</span>
    <span class="bar-dim" id="uptimeText"></span>
    <div class="bar-right">
      <button class="btn" id="themeBtn">light</button>
      <button class="btn" id="logoutBtn" style="display:none">logout</button>
    </div>
  </div>

  <div class="tab-bar" id="tabBar">
    <button class="tab-btn active" data-tab="0">overview</button>
    <button class="tab-btn" data-tab="1">data</button>
    <button class="tab-btn" data-tab="2">services</button>
    <button class="tab-btn" data-tab="3">storage</button>
    <button class="tab-btn" data-tab="4">log</button>
    <button class="tab-btn" data-tab="5">firehose</button>
  </div>

  <div class="panels">
    <!-- overview -->
    <div class="panel active" data-panel="0">
      <div class="overview-grid">
        <div class="card">
          <div class="card-hd">stats</div>
          <div class="kv"><span class="k">users</span><span class="v" id="s-users">-</span></div>
          <div class="kv"><span class="k">paintings</span><span class="v" id="s-paintings">-</span></div>
          <div class="kv"><span class="k">kidlisp</span><span class="v" id="s-kidlisp">-</span></div>
          <div class="kv"><span class="k">moods</span><span class="v" id="s-moods">-</span></div>
          <div class="kv"><span class="k">chat</span><span class="v" id="s-chat">-</span></div>
          <div class="kv"><span class="k">collections</span><span class="v" id="s-cols">-</span></div>
          <div class="kv"><span class="k">total docs</span><span class="v" id="s-docs">-</span></div>
          <div class="kv"><span class="k">db size</span><span class="v" id="s-dbsize">-</span></div>
          <div class="kv"><span class="k">storage</span><span class="v" id="s-storage">-</span></div>
        </div>
        <div class="card">
          <div class="card-hd">services</div>
          <div class="kv"><span class="k"><span class="dot" id="svc-oven-dot"></span> oven</span><span class="v" id="svc-oven-meta">...</span></div>
          <div class="kv"><span class="k"><span class="dot" id="svc-session-dot"></span> session</span><span class="v" id="svc-session-meta">...</span></div>
          <div style="margin-top:8px">
            <div class="card-hd">redis</div>
            <div id="redisTile" class="loading">loading...</div>
          </div>
        </div>
      </div>
    </div>

    <!-- data (databases + collections) -->
    <div class="panel" data-panel="1">
      <div class="card" style="margin-bottom:6px">
        <div class="card-hd">databases <b id="dbSyncStatus"></b> <button class="sync-btn" id="syncBtn" title="Sync Atlas to Primary">sync from atlas</button></div>
        <div id="dbCompare" class="loading">loading...</div>
      </div>
      <div class="card">
        <div class="card-hd">collections <b id="colCount"></b></div>
        <table class="tbl">
          <thead><tr><th>name</th><th style="text-align:right">docs</th></tr></thead>
          <tbody id="collectionsBody"><tr><td colspan="2" class="loading">loading...</td></tr></tbody>
        </table>
      </div>
    </div>

    <!-- services detail -->
    <div class="panel" data-panel="2">
      <div class="card">
        <div class="card-hd">services</div>
        <div class="kv"><span class="k"><span class="dot" id="svc-oven-dot2"></span> oven</span><span class="v" id="svc-oven-meta2">...</span></div>
        <div class="kv"><span class="k"><span class="dot" id="svc-session-dot2"></span> session</span><span class="v" id="svc-session-meta2">...</span></div>
      </div>
      <div class="card" style="margin-top:6px">
        <div class="card-hd">redis</div>
        <div id="redisTile2" class="loading">loading...</div>
      </div>
    </div>

    <!-- storage -->
    <div class="panel" data-panel="3">
      <div class="card">
        <div class="card-hd">storage <b id="stoTotal"></b></div>
        <div id="storageBuckets" class="loading">loading...</div>
      </div>
    </div>

    <!-- log -->
    <div class="log-panel" data-panel="4">
      <div class="log-hd">log <b id="logCount">0</b></div>
      <div class="log-scroll" id="logEntries"></div>
    </div>

    <!-- firehose -->
    <div class="panel firehose-panel" data-panel="5">
      <div class="firehose-hud">
        <span class="firehose-counter"><span id="firehoseCount">0</span> events</span>
        <span class="firehose-rate" id="firehoseRate">0/s</span>
      </div>
      <div class="firehose-sidebar" id="firehoseCounters"></div>
      <canvas id="firehoseCanvas"></canvas>
    </div>
  </div>
</div>

<script>
let auth0Client = null, accessToken = null, logCount = 0;
let darkMode = localStorage.getItem('silo-theme') !== 'light';
applyTheme();

function applyTheme() {
  document.documentElement.setAttribute('data-theme', darkMode ? 'dark' : 'light');
  const btn = document.getElementById('themeBtn');
  if (btn) btn.textContent = darkMode ? 'light' : 'dark';
}

document.getElementById('themeBtn').onclick = () => {
  darkMode = !darkMode;
  localStorage.setItem('silo-theme', darkMode ? 'dark' : 'light');
  applyTheme();
};

async function authFetch(url) {
  const resp = await fetch(url, {
    headers: accessToken ? { Authorization: 'Bearer ' + accessToken } : {},
  });
  if (resp.status === 401 && auth0Client) {
    try {
      accessToken = await auth0Client.getTokenSilently({ cacheMode: 'off' });
      return fetch(url, { headers: { Authorization: 'Bearer ' + accessToken } });
    } catch { auth0Client.loginWithRedirect(); }
  }
  return resp;
}

async function initAuth() {
  document.getElementById('authStatus').textContent = 'loading...';
  try {
    await new Promise((resolve, reject) => {
      const s = document.createElement('script');
      s.src = 'https://cdn.auth0.com/js/auth0-spa-js/2.1/auth0-spa-js.production.js';
      s.onload = resolve; s.onerror = reject;
      document.head.appendChild(s);
    });
    const config = await fetch('/auth/config').then(r => r.json());
    auth0Client = await window.auth0.createAuth0Client({
      domain: config.domain, clientId: config.clientId,
      cacheLocation: 'localstorage', useRefreshTokens: true,
      authorizationParams: { redirect_uri: location.origin + location.pathname },
    });
    if (location.search.includes('state=') && location.search.includes('code=')) {
      await auth0Client.handleRedirectCallback();
      history.replaceState({}, '', location.pathname);
    }
    if (await auth0Client.isAuthenticated()) {
      accessToken = await auth0Client.getTokenSilently();
      const me = await authFetch('/auth/me').then(r => r.json());
      if (!me.isAdmin) {
        document.getElementById('authStatus').textContent = 'access denied [@' + (me.handle || '?') + ']';
        document.getElementById('loginBtn').textContent = 'sign out';
        document.getElementById('loginBtn').onclick = () =>
          auth0Client.logout({ logoutParams: { returnTo: location.origin + location.pathname } });
        return;
      }
      document.getElementById('login').style.display = 'none';
      document.getElementById('dashboard').classList.add('visible');
      document.getElementById('logoutBtn').style.display = 'inline';
      connectWS(); loadAll();
    } else {
      document.getElementById('authStatus').textContent = '';
      document.getElementById('loginBtn').onclick = () => auth0Client.loginWithRedirect();
    }
  } catch (err) {
    console.error('Auth init failed:', err);
    document.getElementById('authStatus').textContent = 'error: ' + err.message;
  }
  document.getElementById('logoutBtn').onclick = () => {
    auth0Client?.logout({ logoutParams: { returnTo: location.origin + location.pathname } });
  };
}

// ws
const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
const wsUrl = protocol + '//' + location.host + '/ws';
let ws = null;
function connectWS() {
  ws = new WebSocket(wsUrl);
  ws.onopen = () => dot('wsStatus', true);
  ws.onclose = () => { dot('wsStatus', false); setTimeout(connectWS, 2000); };
  ws.onmessage = (e) => {
    const data = JSON.parse(e.data);
    if (data.logEntry) addLog(data.logEntry);
    if (data.firehose) fhIngest(data.firehose);
  };
}
function dot(id, ok) {
  const el = document.getElementById(id);
  if (el) el.className = 'dot ' + (ok ? 'ok' : 'err');
}

// data
async function loadOverview() {
  try {
    const d = await authFetch('/api/overview').then(r => r.json());
    const db = d.db || {};
    document.getElementById('s-users').textContent = fmt(db.users);
    document.getElementById('s-paintings').textContent = fmt(db.paintings);
    document.getElementById('s-kidlisp').textContent = fmt(db.kidlisp);
    document.getElementById('s-moods').textContent = fmt(db.moods);
    document.getElementById('s-chat').textContent = fmt((db.chatSystem||0) + (db.chatClock||0));
    document.getElementById('s-cols').textContent = fmt(db.totalCollections);
    document.getElementById('s-docs').textContent = fmt(db.totalDocuments);
    if (db.size) {
      document.getElementById('s-dbsize').textContent = fmtBytes(db.size.dataSize);
    }
    document.getElementById('s-storage').textContent = (d.storage?.totalGB || '0') + ' GB';
    dot('mongoStatus', true);

    // redis (update both overview and detail tabs)
    if (d.redis?.connected) {
      dot('redisStatus', true);
      const redisHtml =
        kv('memory', d.redis.usedMemory || '-') +
        kv('peak', d.redis.peakMemory || '-') +
        kv('keys', fmt(d.redis.totalKeys)) +
        kv('clients', d.redis.connectedClients) +
        kv('hit rate', d.redis.hitRate + '%') +
        kv('uptime', fmtDuration(d.redis.uptimeSeconds)) +
        kv('version', d.redis.version || '-');
      document.getElementById('redisTile').innerHTML = redisHtml;
      const r2 = document.getElementById('redisTile2');
      if (r2) r2.innerHTML = redisHtml;
    } else {
      dot('redisStatus', false);
      const msg = '<span class="loading">not connected</span>';
      document.getElementById('redisTile').innerHTML = msg;
      const r2 = document.getElementById('redisTile2');
      if (r2) r2.innerHTML = msg;
    }

    // uptime
    const up = Math.floor((d.uptime || 0) / 1000);
    document.getElementById('uptimeText').textContent =
      Math.floor(up/3600) + 'h' + Math.floor((up%3600)/60) + 'm';
  } catch (e) { console.error(e); }
}

async function loadCompare() {
  try {
    const d = await authFetch('/api/db/compare').then(r => r.json());
    const el = document.getElementById('dbCompare');
    const badge = document.getElementById('dbSyncStatus');

    if (d.sameConnection) {
      badge.innerHTML = '<span class="sync-badge synced">same db</span>';
      el.innerHTML =
        kv('primary', d.primaryLabel) +
        kv('active', '<span class="active-badge">' + d.activeDb + '</span>') +
        kv('status', 'single connection') +
        (d.primarySize ? kv('data', fmtBytes(d.primarySize.dataSize)) + kv('indexes', fmtBytes(d.primarySize.indexSize)) : '');
    } else {
      badge.innerHTML = d.allSynced
        ? '<span class="sync-badge synced">synced</span>'
        : '<span class="sync-badge unsynced">out of sync</span>';

      let html = kv('primary', d.primaryLabel + (d.primaryConnected ? '' : ' (down)')) +
        kv('atlas', d.atlasLabel + (d.atlasConnected ? '' : ' (down)')) +
        kv('active', '<span class="active-badge">' + d.activeDb + '</span>');

      if (d.primarySize) html += kv('primary size', fmtBytes(d.primarySize.dataSize));
      if (d.atlasSize) html += kv('atlas size', fmtBytes(d.atlasSize.dataSize));

      // show mismatched collections
      const mismatched = (d.collections || []).filter(c => !c.synced);
      if (mismatched.length > 0) {
        html += '<div style="margin-top:6px; font-size:11px; color:var(--fg2)">mismatched:</div>';
        html += '<table class="tbl" style="margin-top:2px"><thead><tr><th>collection</th><th class="r">primary</th><th class="r">atlas</th></tr></thead><tbody>';
        for (const c of mismatched) {
          html += '<tr><td>' + esc(c.name) + '</td><td class="r">' + (c.primary ?? '-') + '</td><td class="r">' + (c.atlas ?? '-') + '</td></tr>';
        }
        html += '</tbody></table>';
      }
      el.innerHTML = html;
    }
  } catch (e) {
    document.getElementById('dbCompare').innerHTML = '<span class="loading">error loading</span>';
  }
}

async function loadCollections() {
  try {
    const data = await authFetch('/api/db/collections').then(r => r.json());
    document.getElementById('colCount').textContent = data.length;
    document.getElementById('collectionsBody').innerHTML = data.map(c =>
      '<tr><td>' + esc(c.name) + '</td><td class="r">' + fmt(c.count) + '</td></tr>'
    ).join('');
  } catch (e) {
    document.getElementById('collectionsBody').innerHTML = '<tr><td colspan="2" class="loading">error</td></tr>';
  }
}

async function loadStorage() {
  try {
    const data = await authFetch('/api/storage/buckets').then(r => r.json());
    const el = document.getElementById('storageBuckets');
    if (!data.length) { el.textContent = 'none'; return; }
    const maxGB = Math.max(...data.map(b => parseFloat(b.gb)), 0.01);
    const totalGB = data.reduce((s, b) => s + parseFloat(b.gb), 0).toFixed(2);
    document.getElementById('stoTotal').textContent = totalGB + ' GB';
    el.innerHTML = data.map(b => {
      const pct = (parseFloat(b.gb) / maxGB * 100).toFixed(0);
      return '<div class="sbar"><span class="sbar-name">' + esc(b.bucket) + '</span>' +
        '<div class="sbar-track"><div class="sbar-fill" style="width:' + pct + '%"></div></div>' +
        '<span class="sbar-val">' + b.gb + ' GB / ' + fmt(b.objects) + '</span></div>';
    }).join('');
    el.classList.remove('loading');
  } catch (e) {
    document.getElementById('storageBuckets').textContent = 'error';
  }
}

async function loadServices() { checkSvc('oven'); checkSvc('session'); }
async function checkSvc(name) {
  try {
    const d = await authFetch('/api/services/' + name).then(r => r.json());
    const ok = d.status === 'ok';
    dot('svc-' + name + '-dot', ok);
    dot('svc-' + name + '-dot2', ok);
    const txt = ok ? d.responseMs + 'ms' : 'down';
    const cls = 'v ' + (ok ? 'ok' : 'err');
    for (const suffix of ['', '2']) {
      const el = document.getElementById('svc-' + name + '-meta' + suffix);
      if (el) { el.textContent = txt; el.className = cls; }
    }
  } catch (e) {
    dot('svc-' + name + '-dot', false);
    dot('svc-' + name + '-dot2', false);
    for (const suffix of ['', '2']) {
      const m = document.getElementById('svc-' + name + '-meta' + suffix);
      if (m) { m.textContent = 'error'; m.className = 'v err'; }
    }
  }
}

// log (append at bottom, auto-scroll when near bottom)
function addLog(entry) {
  logCount++;
  document.getElementById('logCount').textContent = logCount;
  const el = document.getElementById('logEntries');
  const atBottom = el.scrollHeight - el.scrollTop - el.clientHeight < 40;
  const div = document.createElement('div');
  div.className = 'log-row';
  const t = new Date(entry.time);
  const ts = String(t.getHours()).padStart(2,'0') + ':' + String(t.getMinutes()).padStart(2,'0') + ':' + String(t.getSeconds()).padStart(2,'0');
  div.innerHTML = '<span class="log-t">' + ts + '</span><span class="log-m ' + (entry.type || '') + '">' + esc(entry.msg) + '</span>';
  el.appendChild(div);
  while (el.children.length > 200) el.removeChild(el.firstChild);
  if (atBottom) el.scrollTop = el.scrollHeight;
}

// util
function fmt(n) { return n == null ? '-' : Number(n).toLocaleString(); }
function esc(t) { const d = document.createElement('div'); d.textContent = t; return d.innerHTML; }
function kv(k, v) { return '<div class="kv"><span class="k">' + k + '</span><span class="v">' + v + '</span></div>'; }
function fmtBytes(b) {
  if (b == null) return '-';
  if (b > 1e9) return (b/1e9).toFixed(2) + ' GB';
  if (b > 1e6) return (b/1e6).toFixed(1) + ' MB';
  if (b > 1e3) return (b/1e3).toFixed(0) + ' KB';
  return b + ' B';
}
function fmtDuration(s) {
  if (!s) return '-';
  const d = Math.floor(s/86400), h = Math.floor((s%86400)/3600), m = Math.floor((s%3600)/60);
  if (d > 0) return d + 'd ' + h + 'h';
  if (h > 0) return h + 'h ' + m + 'm';
  return m + 'm';
}

document.getElementById('logo').onclick = () => {
  location.href = location.host === 'silo.aesthetic.computer' ? 'https://aesthetic.computer' : '/';
};

// --- Firehose Visualization ---
const FIREHOSE_COLORS = {
  'chat-system': '#4a4', 'chat-clock': '#4c4', 'chat-sotce': '#6a4',
  'users': '#48f', '@handles': '#68c', 'verifications': '#cc4',
  'paintings': '#a6f', 'pieces': '#c6a', 'kidlisp': '#fa4',
  'moods': '#f8a', 'tapes': '#f80',
};
const FH_DEFAULT_COLOR = '#888';
const OP_SYM = { insert: '+', update: '~', replace: '=', delete: '-' };

const fh = {
  particles: [], ambient: [], canvas: null, ctx: null, running: false,
  totalEvents: 0, eventsThisSec: 0, eps: 0, lastRateReset: Date.now(),
  MAX_P: 200, MAX_AMB: 30,
  counters: {},  // { 'chat-system': { insert: 5, update: 2 }, ... }
};

function fhParticle(ev) {
  const c = fh.canvas; if (!c) return null;
  const dpr = window.devicePixelRatio || 1;
  const w = c.width / dpr, h = c.height / dpr;
  return {
    x: 40 + Math.random() * 60,
    y: 40 + Math.random() * (h - 80),
    vx: 0.5 + Math.random() * 1.5,
    vy: (Math.random() - 0.5) * 0.3,
    color: FIREHOSE_COLORS[ev.ns] || FH_DEFAULT_COLOR,
    label: (OP_SYM[ev.op] || '?') + ' ' + ev.ns + (ev.summary ? ' ' + ev.summary : ''),
    alpha: 1, size: 4 + Math.random() * 3,
    life: 5000 + Math.random() * 3000, born: Date.now(),
  };
}

function fhAmbient() {
  const c = fh.canvas; if (!c) return null;
  const dpr = window.devicePixelRatio || 1;
  return {
    x: Math.random() * (c.width / dpr),
    y: Math.random() * (c.height / dpr),
    vx: (Math.random() - 0.5) * 0.2,
    vy: (Math.random() - 0.5) * 0.15,
    alpha: 0.08 + Math.random() * 0.12,
    size: 1 + Math.random() * 2,
    color: darkMode ? '#444' : '#ccc',
    life: 8000 + Math.random() * 6000, born: Date.now(),
  };
}

function fhIngest(ev) {
  fh.totalEvents++;
  fh.eventsThisSec++;
  // aggregate counters
  if (!fh.counters[ev.ns]) fh.counters[ev.ns] = {};
  fh.counters[ev.ns][ev.op] = (fh.counters[ev.ns][ev.op] || 0) + 1;
  // update HUD
  const el = document.getElementById('firehoseCount');
  if (el) el.textContent = fh.totalEvents.toLocaleString();
  fhUpdateCounters();
  // spawn particle
  if (fh.particles.length >= fh.MAX_P) fh.particles.shift();
  const p = fhParticle(ev);
  if (p) fh.particles.push(p);
}

function fhUpdateCounters() {
  const el = document.getElementById('firehoseCounters');
  if (!el) return;
  const entries = Object.entries(fh.counters)
    .map(([ns, ops]) => {
      const total = Object.values(ops).reduce((s, n) => s + n, 0);
      return { ns, total, ops };
    })
    .sort((a, b) => b.total - a.total);
  el.innerHTML = entries.map(e => {
    const color = FIREHOSE_COLORS[e.ns] || FH_DEFAULT_COLOR;
    const parts = Object.entries(e.ops).map(([op, n]) => (OP_SYM[op] || op) + n).join(' ');
    return '<div class="fh-counter-row">' +
      '<span class="fh-counter-dot" style="background:' + color + '"></span>' +
      '<span class="fh-counter-ns">' + e.ns + '</span>' +
      '<span class="fh-counter-val">' + e.total + '</span>' +
      '<span class="fh-counter-ops">' + parts + '</span></div>';
  }).join('');
}

function fhTick() {
  if (!fh.running) return;
  requestAnimationFrame(fhTick);
  const { canvas, ctx, particles, ambient } = fh;
  if (!canvas || !ctx) return;
  const now = Date.now();
  const dpr = window.devicePixelRatio || 1;

  // rate counter
  if (now - fh.lastRateReset >= 1000) {
    fh.eps = fh.eventsThisSec; fh.eventsThisSec = 0; fh.lastRateReset = now;
    const rEl = document.getElementById('firehoseRate');
    if (rEl) rEl.textContent = fh.eps + '/s';
  }

  // clear
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = darkMode ? '#111' : '#f4f4f4';
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  // ambient
  while (ambient.length < fh.MAX_AMB) { const a = fhAmbient(); if (a) ambient.push(a); }
  for (let i = ambient.length - 1; i >= 0; i--) {
    const p = ambient[i];
    const age = now - p.born;
    if (age > p.life) { ambient.splice(i, 1); continue; }
    p.x += p.vx; p.y += p.vy;
    const prog = age / p.life;
    const fade = prog < 0.1 ? prog / 0.1 : prog > 0.8 ? (1 - prog) / 0.2 : 1;
    ctx.globalAlpha = p.alpha * fade;
    ctx.beginPath();
    ctx.arc(p.x * dpr, p.y * dpr, p.size * dpr, 0, Math.PI * 2);
    ctx.fillStyle = p.color;
    ctx.fill();
  }

  // event particles
  ctx.textBaseline = 'middle';
  ctx.font = (11 * dpr) + 'px "Berkeley Mono Variable", monospace';
  for (let i = particles.length - 1; i >= 0; i--) {
    const p = particles[i];
    const age = now - p.born;
    if (age > p.life) { particles.splice(i, 1); continue; }
    p.x += p.vx; p.y += p.vy;
    const prog = age / p.life;
    p.alpha = prog < 0.05 ? prog / 0.05 : 1 - prog * prog;
    if (p.alpha <= 0) { particles.splice(i, 1); continue; }
    ctx.globalAlpha = p.alpha;
    ctx.beginPath();
    ctx.arc(p.x * dpr, p.y * dpr, p.size * dpr, 0, Math.PI * 2);
    ctx.fillStyle = p.color;
    ctx.fill();
    ctx.globalAlpha = p.alpha * 0.85;
    ctx.fillStyle = p.color;
    ctx.fillText(p.label, (p.x + p.size + 6) * dpr, p.y * dpr);
  }
  ctx.globalAlpha = 1;
}

function fhResize() {
  const c = fh.canvas; if (!c) return;
  const panel = c.parentElement; if (!panel) return;
  const dpr = window.devicePixelRatio || 1;
  const rect = panel.getBoundingClientRect();
  c.width = rect.width * dpr;
  c.height = rect.height * dpr;
  c.style.width = rect.width + 'px';
  c.style.height = rect.height + 'px';
}

function fhInit() {
  fh.canvas = document.getElementById('firehoseCanvas');
  if (!fh.canvas) return;
  fh.ctx = fh.canvas.getContext('2d');
  fhResize();
  window.addEventListener('resize', fhResize);
}
function fhStart() { if (fh.running) return; fh.running = true; fhResize(); fhTick(); }
function fhStop() { fh.running = false; }

// --- tabs (always visible, persisted) ---
let activeTab = parseInt(localStorage.getItem('silo-tab') || '0');
const tabBtns = document.querySelectorAll('.tab-btn');
const panels = document.querySelectorAll('[data-panel]');

function setTab(idx) {
  activeTab = idx;
  localStorage.setItem('silo-tab', idx);
  tabBtns.forEach((b, i) => b.classList.toggle('active', i === idx));
  panels.forEach(p => p.classList.toggle('active', parseInt(p.dataset.panel) === idx));
  // auto-scroll log to bottom when switching to log tab
  if (idx === 4) {
    const el = document.getElementById('logEntries');
    requestAnimationFrame(() => el.scrollTop = el.scrollHeight);
  }
  // firehose animation: start/stop with tab
  if (idx === 5) { fhStart(); } else { fhStop(); }
}
tabBtns.forEach(b => b.addEventListener('click', () => setTab(parseInt(b.dataset.tab))));
setTab(activeTab);

// --- sync button ---
document.getElementById('syncBtn').onclick = async () => {
  if (!confirm('Sync all collections from Atlas to primary?\\nThis will overwrite primary data.')) return;
  const btn = document.getElementById('syncBtn');
  btn.disabled = true; btn.textContent = 'syncing...';
  try {
    const resp = await fetch('/api/db/sync', {
      method: 'POST',
      headers: accessToken ? { Authorization: 'Bearer ' + accessToken, 'Content-Type': 'application/json' } : {},
    });
    const data = await resp.json();
    if (data.ok) {
      btn.textContent = 'done (' + data.collections.length + ' collections)';
      loadCompare(); loadOverview(); loadCollections();
      setTimeout(() => { btn.textContent = 'sync from atlas'; btn.disabled = false; }, 4000);
    } else {
      alert('Sync failed: ' + (data.error || 'unknown'));
      btn.textContent = 'sync from atlas'; btn.disabled = false;
    }
  } catch (e) {
    alert('Sync failed: ' + e.message);
    btn.textContent = 'sync from atlas'; btn.disabled = false;
  }
};

function loadAll() {
  loadOverview(); loadCompare(); loadCollections(); loadStorage(); loadServices();
  fhInit();
  setInterval(loadOverview, 30000);
  setInterval(loadCompare, 60000);
  setInterval(loadServices, 60000);
}

initAuth();
</script>
</body>
</html>`;

// --- 404 ---
app.use((req, res) => res.status(404).json({ error: "Not found" }));

// --- Server Start ---
let server;
if (dev) {
  try {
    const httpsOpts = {
      key: fs.readFileSync("../ssl-dev/localhost-key.pem"),
      cert: fs.readFileSync("../ssl-dev/localhost.pem"),
    };
    server = https.createServer(httpsOpts, app);
  } catch {
    log("warn", "no SSL certs, falling back to HTTP");
    server = http.createServer(app);
  }
} else {
  server = http.createServer(app);
}

// --- WebSocket ---
wss = new WebSocketServer({ server, path: "/ws" });

wss.on("connection", async (ws) => {
  log("info", "dashboard client connected");
  for (const entry of activityLog.slice(0, 30).reverse()) {
    ws.send(JSON.stringify({ logEntry: entry }));
  }
  // Send firehose history so the tab isn't empty on load
  for (const event of firehoseBuffer) {
    ws.send(JSON.stringify({ firehose: event }));
  }
  ws.on("close", () => log("info", "dashboard client disconnected"));
});

// --- Boot ---
await connectMongo();
startFirehose();
await connectAtlas();
await connectRedis();

server.listen(PORT, () => {
  const proto = dev ? "https" : "http";
  log("info", `silo running on ${proto}://localhost:${PORT}`);
});

// --- Shutdown ---
function shutdown(signal) {
  log("info", `received ${signal}, shutting down...`);
  if (changeStream) changeStream.close().catch(() => {});
  wss.clients.forEach((ws) => ws.close());
  server.close();
  mongoClient?.close();
  if (atlasClient && atlasClient !== mongoClient) atlasClient?.close();
  redisClient?.quit().catch(() => {});
  setTimeout(() => process.exit(0), 500);
}
process.on("SIGTERM", () => shutdown("SIGTERM"));
process.on("SIGINT", () => shutdown("SIGINT"));
