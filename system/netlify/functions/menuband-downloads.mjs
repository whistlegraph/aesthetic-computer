// Menu Band downloads counter.
//
// GET  /api/menuband-downloads
//   → { total, byVersion: { "0.9.1": 42, ... }, latest: "0.9.1" }
//   Cached for 60s in Redis (alongside the other give:* keys).
//
// POST /api/menuband-downloads   body: { version: "0.9.1" }
//   → { ok: true, version, count, total }
//   Validates `version` against the live manifest's allowlist before
//   incrementing — anyone with the URL can POST, so we don't want
//   arbitrary strings inflating the per-version breakdown.
//
// MongoDB collection: "menuband-downloads"
//   Document shape: { _id: "0.9.1", count: N, firstDownload: Date, lastDownload: Date }
//   Per-version is the storage primitive; `total` is summed at read time.

import fs from "fs";
import path from "path";

import { connect } from "../../backend/database.mjs";
import {
  connect as cacheConnect,
  invalidate as cacheInvalidate,
} from "../../backend/cache.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "menuband-downloads";
const CACHE_KEY = "menuband:downloads";
const CACHE_TTL_SECONDS = 60;

// Read the deployed manifest from disk (system/public/menuband/manifest.json
// — committed, served by lith at /menuband/manifest.json same-origin, so no
// CORS dance for the website's fetch). The macOS app reads a smaller-schema
// sibling at assets.aesthetic.computer/menuband/latest.json; both are kept
// in sync by slab/menuband/bin/publish-release.fish. Falls back to an empty
// allowlist if the file is missing or malformed; the caller still gets a
// 400 for any version they send (with `manifestMissing` reason).
function loadManifest() {
  const candidates = [
    path.join(process.cwd(), "public", "menuband", "manifest.json"),
    path.join(process.cwd(), "..", "system", "public", "menuband", "manifest.json"),
    path.join(process.cwd(), "system", "public", "menuband", "manifest.json"),
  ];
  for (const p of candidates) {
    try {
      const raw = fs.readFileSync(p, "utf8");
      const parsed = JSON.parse(raw);
      if (parsed && Array.isArray(parsed.versions)) return parsed;
    } catch {
      // try next
    }
  }
  return { latest: null, versions: [], _missing: true };
}

function allowedVersions(manifest) {
  return new Set(
    (manifest.versions || [])
      .map((v) => (v && typeof v.version === "string" ? v.version : null))
      .filter(Boolean),
  );
}

async function readCounts() {
  const database = await connect();
  try {
    const docs = await database.db
      .collection(COLLECTION)
      .find({}, { projection: { _id: 1, count: 1 } })
      .toArray();
    const byVersion = {};
    let total = 0;
    for (const d of docs) {
      const n = Number(d.count) || 0;
      byVersion[String(d._id)] = n;
      total += n;
    }
    return { total, byVersion };
  } finally {
    await database.disconnect();
  }
}

async function getCached() {
  try {
    const client = await cacheConnect();
    const cached = await client.get(CACHE_KEY);
    if (cached) return JSON.parse(cached);
  } catch {
    // Redis down — fall through and read from Mongo.
  }
  const fresh = await readCounts();
  try {
    const client = await cacheConnect();
    await client.setEx(CACHE_KEY, CACHE_TTL_SECONDS, JSON.stringify(fresh));
  } catch {
    // best-effort
  }
  return fresh;
}

export async function handler(event) {
  const method = event.httpMethod;
  if (method === "OPTIONS") return respond(200, { ok: true });

  if (method === "GET") {
    try {
      const manifest = loadManifest();
      const { total, byVersion } = await getCached();
      return respond(200, {
        total,
        byVersion,
        latest: manifest.latest || null,
      });
    } catch (e) {
      console.error("[menuband-downloads] GET failed:", e?.message || e);
      return respond(500, { error: "read failed" });
    }
  }

  if (method === "POST") {
    let body = {};
    try {
      body = event.body ? JSON.parse(event.body) : {};
    } catch {
      return respond(400, { error: "invalid JSON body" });
    }

    const version = String(body.version || "").trim();
    if (!version) return respond(400, { error: "version required" });
    // Defense-in-depth: even if the manifest is wrong, refuse anything that
    // looks like an injection / mongo operator / huge string.
    if (!/^[0-9A-Za-z._-]{1,32}$/.test(version)) {
      return respond(400, { error: "version format invalid" });
    }

    const manifest = loadManifest();
    const allowed = allowedVersions(manifest);
    if (manifest._missing) {
      return respond(503, { error: "manifest unavailable" });
    }
    if (!allowed.has(version)) {
      return respond(400, { error: "version not in manifest" });
    }

    try {
      const database = await connect();
      try {
        const now = new Date();
        const result = await database.db.collection(COLLECTION).findOneAndUpdate(
          { _id: version },
          {
            $inc: { count: 1 },
            $set: { lastDownload: now },
            $setOnInsert: { firstDownload: now },
          },
          { upsert: true, returnDocument: "after" },
        );
        const updated = result?.value || result; // driver returns shape varies
        const count = Number(updated?.count) || 1;

        // Invalidate the read cache so the next GET picks up the bump.
        await cacheInvalidate(CACHE_KEY);

        return respond(200, { ok: true, version, count });
      } finally {
        await database.disconnect();
      }
    } catch (e) {
      console.error("[menuband-downloads] POST failed:", e?.message || e);
      return respond(500, { error: "write failed" });
    }
  }

  return respond(405, { error: "method not allowed" });
}
