#!/usr/bin/env node
// silo - data & storage dashboard for aesthetic.computer

import "dotenv/config";
import express from "express";
import https from "https";
import http from "http";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { MongoClient } from "mongodb";
import { createClient } from "redis";
import { S3Client, ListObjectsV2Command } from "@aws-sdk/client-s3";
import { WebSocketServer } from "ws";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

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

// --- Collection Categories ---
const COLLECTION_CATEGORIES = {
  "users": "identity", "@handles": "identity", "verifications": "identity",
  "paintings": "content", "pieces": "content", "kidlisp": "content",
  "tapes": "content", "moods": "content",
  "chat-system": "communication", "chat-clock": "communication", "chat-sotce": "communication",
  "boots": "system", "oven-bakes": "system", "_firehose": "system",
};
const CATEGORY_META = {
  identity: { label: "Users & Identity", color: "#48f" },
  content: { label: "Content", color: "#a6f" },
  communication: { label: "Communication", color: "#4a4" },
  system: { label: "System & Logs", color: "#f80" },
  other: { label: "Other", color: "#888" },
};

// --- Handle Cache (Auth0 sub → handle) ---
const handleCache = new Map();

async function loadHandleCache() {
  if (!db) return;
  try {
    const docs = await db.collection("@handles").find({}).toArray();
    for (const doc of docs) {
      if (doc._id && doc.handle) handleCache.set(String(doc._id), doc.handle);
    }
    log("info", `handle cache loaded: ${handleCache.size} entries`);
  } catch (err) {
    log("error", `handle cache load failed: ${err.message}`);
  }
}

function resolveHandle(str) {
  if (!str || typeof str !== "string") return null;
  if (/^(auth0|google-oauth2|apple|windowslive|github)\|/.test(str)) {
    return handleCache.get(str) || null;
  }
  return str;
}

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
  // Try to resolve user handle from common fields, resolving Auth0 subs
  const rawHandle = doc.handle || doc.user;
  const resolved = rawHandle ? resolveHandle(String(rawHandle)) : null;
  // Also try resolving the doc _id (often an Auth0 sub in user-related collections)
  const resolvedId = doc._id ? resolveHandle(String(doc._id)) : null;
  const who = resolved ? `@${resolved}` : (resolvedId && resolvedId !== String(doc._id) ? `@${resolvedId}` : null);
  switch (coll) {
    case "chat-system": case "chat-clock": case "chat-sotce":
      return (who || "anon") + (doc.text ? ` "${doc.text.slice(0, 30)}"` : "");
    case "@handles": return who || (doc.handle ? `@${doc.handle}` : null);
    case "users": {
      const name = doc.name ? resolveHandle(String(doc.name)) : null;
      const displayName = name && name !== doc.name ? `@${name}` : doc.name;
      return doc.email?.split("@")[0] || displayName || who || null;
    }
    case "paintings": return (who ? who + " " : "") + (doc.slug || doc.title || "");
    case "tapes": return (who ? who + " " : "") + (doc.piece || "");
    case "pieces": return doc.slug || doc.name || null;
    case "kidlisp": return (who ? who + " " : "") + (doc.code ? "$" + doc.code : "") + (doc.name ? " " + doc.name : "");
    case "moods": return (who ? who + " " : "") + (doc.mood || "");
    case "verifications": return who || null;
    case "boots": {
      const m = doc.meta || {};
      const rawUser = m.user?.handle || m.user?.sub || m.user;
      const bootUser = rawUser ? (resolveHandle(String(rawUser)) || rawUser) : null;
      const who = bootUser ? `@${bootUser}` : "visitor";
      const status = doc.status || "";
      const statusTag = status !== "started" ? ` [${status}]` : "";
      // Rich info: browser, device, referrer, geo
      const browser = m.browser || "";
      const device = m.mobile ? "mobile" : "";
      const geo = doc.server?.country || "";
      const referrer = m.referrer ? ` via ${m.referrer.replace(/^https?:\/\//, "").split("/")[0]}` : "";
      const parts = [who, m.host + (m.path || "/"), browser, device, geo].filter(Boolean);
      return parts.join(" ") + referrer + statusTag;
    }
    case "oven-bakes": return doc.status || null;
    default: {
      return who || doc.slug || doc.name || doc.email?.split("@")[0] || null;
    }
  }
}

const FIREHOSE_COLLECTION = "_firehose";
const FIREHOSE_TTL_DAYS = 30;

async function ensureFirehoseCollection() {
  if (!db) return;
  try {
    const col = db.collection(FIREHOSE_COLLECTION);
    // TTL index: auto-delete events older than 30 days
    await col.createIndex({ time: 1 }, { expireAfterSeconds: FIREHOSE_TTL_DAYS * 86400 });
    // Index for recent queries
    await col.createIndex({ ns: 1, time: -1 });
    log("info", `_firehose collection ready (${FIREHOSE_TTL_DAYS}d TTL)`);
  } catch (err) {
    log("error", `_firehose index setup: ${err.message}`);
  }
}

function startFirehose() {
  if (!db) return;
  const firehoseCol = db.collection(FIREHOSE_COLLECTION);
  try {
    changeStream = db.watch(
      [{
        $match: {
          operationType: { $in: ["insert", "update", "replace", "delete"] },
          "ns.coll": { $ne: FIREHOSE_COLLECTION },  // avoid infinite loop
        },
      }],
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

      // Keep handle cache fresh when @handles collection changes
      if (coll === "@handles" && change.fullDocument) {
        const hDoc = change.fullDocument;
        if (hDoc._id && hDoc.handle) handleCache.set(String(hDoc._id), hDoc.handle);
      }

      const event = {
        ns: coll,
        op,
        time: new Date(now),
        docId: change.documentKey?._id?.toString() || null,
        summary: firehoseSummary(coll, op, change.fullDocument),
      };

      // Persist to MongoDB (fire-and-forget)
      firehoseCol.insertOne(event).catch(() => {});

      if (wss?.clients) {
        const data = JSON.stringify({ firehose: { ...event, time: now } });
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
      try {
        const cs = await database.command({ collStats: col.name });
        stats.push({
          name: col.name, count: cs.count || 0,
          size: cs.size || 0, storageSize: cs.storageSize || 0,
          indexSize: cs.totalIndexSize || 0,
          category: COLLECTION_CATEGORIES[col.name] || "other",
        });
      } catch {
        const count = await database.collection(col.name).estimatedDocumentCount();
        stats.push({
          name: col.name, count, size: 0, storageSize: 0, indexSize: 0,
          category: COLLECTION_CATEGORIES[col.name] || "other",
        });
      }
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
  const stats = await getDbStats() || [];
  res.json({ collections: stats, categories: CATEGORY_META });
});

app.get("/api/db/backups", async (req, res) => {
  log("info", "backups requested");
  const backups = [];
  // Check local filesystem for mongodump backups
  const backupPath = process.env.BACKUP_PATH || "/var/backups/mongodb";
  try {
    if (fs.existsSync(backupPath)) {
      const entries = fs.readdirSync(backupPath, { withFileTypes: true });
      for (const entry of entries) {
        try {
          const fullPath = backupPath + "/" + entry.name;
          const stat = fs.statSync(fullPath);
          let totalSize = stat.size;
          // For directories (mongodump output), sum up contents
          if (entry.isDirectory()) {
            totalSize = 0;
            const walk = (dir) => {
              for (const f of fs.readdirSync(dir, { withFileTypes: true })) {
                const fp = dir + "/" + f.name;
                if (f.isDirectory()) walk(fp);
                else totalSize += fs.statSync(fp).size;
              }
            };
            walk(fullPath);
          }
          backups.push({
            name: entry.name, source: "local", size: totalSize,
            date: stat.mtime.toISOString(), isDirectory: entry.isDirectory(),
          });
        } catch {}
      }
    }
  } catch (err) {
    log("error", `backup scan (local): ${err.message}`);
  }
  // Check S3 for backups
  const backupBucket = process.env.BACKUP_BUCKET;
  const backupPrefix = process.env.BACKUP_PREFIX || "backups/";
  if (backupBucket) {
    try {
      let continuationToken;
      do {
        const resp = await s3.send(new ListObjectsV2Command({
          Bucket: backupBucket, Prefix: backupPrefix,
          ContinuationToken: continuationToken,
        }));
        if (resp.Contents) {
          for (const obj of resp.Contents) {
            const name = obj.Key.replace(backupPrefix, "");
            if (!name) continue;
            backups.push({
              name, source: "s3", bucket: backupBucket,
              size: obj.Size, date: obj.LastModified?.toISOString(),
            });
          }
        }
        continuationToken = resp.IsTruncated ? resp.NextContinuationToken : undefined;
      } while (continuationToken);
    } catch (err) {
      log("error", `backup scan (s3): ${err.message}`);
    }
  }
  backups.sort((a, b) => new Date(b.date) - new Date(a.date));
  res.json({ backups, backupPath, backupBucket: backupBucket || null });
});

app.get("/api/db/compare", async (req, res) => {
  log("info", "db compare requested");
  const [primary, atlas, primarySize, atlasSize] = await Promise.all([
    getDbStats(), getAtlasStats(), getDbSize(db), getDbSize(atlasDb),
  ]);
  const primaryHost = parseMongoHost(process.env.MONGODB_CONNECTION_STRING);
  const atlasHost = parseMongoHost(process.env.ATLAS_CONNECTION_STRING);

  // Silo is now the source of truth; Atlas is deprecated
  const activeDb = "silo";

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

app.get("/api/firehose/history", async (req, res) => {
  if (!db) return res.json([]);
  const limit = Math.min(parseInt(req.query.limit) || 100, 1000);
  const ns = req.query.ns || null;
  const query = ns ? { ns } : {};
  try {
    const events = await db.collection(FIREHOSE_COLLECTION)
      .find(query).sort({ time: -1 }).limit(limit).toArray();
    res.json(events.reverse());
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

app.get("/api/firehose/stats", async (req, res) => {
  if (!db) return res.json({});
  try {
    const col = db.collection(FIREHOSE_COLLECTION);
    const total = await col.estimatedDocumentCount();
    const pipeline = [
      { $group: { _id: { ns: "$ns", op: "$op" }, count: { $sum: 1 } } },
      { $sort: { count: -1 } },
    ];
    const breakdown = await col.aggregate(pipeline).toArray();
    res.json({ total, breakdown });
  } catch (err) {
    res.status(500).json({ error: err.message });
  }
});

// --- Dashboard (loaded from file, reloadable via SIGHUP) ---
const DASHBOARD_PATH = path.join(__dirname, "dashboard.html");
let dashboardHtml = "";

function loadDashboard() {
  try {
    dashboardHtml = fs.readFileSync(DASHBOARD_PATH, "utf-8");
    log("info", `dashboard loaded (${(dashboardHtml.length / 1024).toFixed(0)} KB)`);
  } catch (err) {
    log("error", `dashboard load failed: ${err.message}`);
  }
}
loadDashboard();

app.get("/", (req, res) => {
  res.setHeader("Content-Type", "text/html");
  res.send(dashboardHtml);
});

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
  ws.on("close", () => log("info", "dashboard client disconnected"));
});

// --- Boot ---
await connectMongo();
await loadHandleCache();
await ensureFirehoseCollection();
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

// --- SIGHUP: hot-reload dashboard.html without dropping connections ---
process.on("SIGHUP", () => {
  log("info", "SIGHUP received, reloading dashboard...");
  loadDashboard();
});
