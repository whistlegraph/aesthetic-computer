// App Open, 2026.09.28
// Counts a launch of one of our native apps.
// POST /api/app-open {app, version, platform, install, fresh}
//
// `install` is a random UUID the app mints on its first launch and keeps in
// its own storage, so it goes when the app is deleted. It is never tied to an
// account, handle, device or address. `fresh` is true only on that first
// launch, which is how new installs (and reinstalls, which look the same)
// get counted. One row per app + install + day holds the day's open count,
// so daily active installs are a count of rows. Rows expire like visits do.
// See toolchain/analytics/VISITS.md.
import { connect } from "../../backend/database.mjs";
import { createHmac, randomBytes } from "node:crypto";
import { respond } from "../../backend/http.mjs";
import { RETENTION_DAYS } from "../../public/aesthetic.computer/lib/visit-model.mjs";

export const APP_OPEN_COLLECTION = "app-opens";
export const APP_OPEN_APPS = Object.freeze([
  "menuband", "macpal", "slab", "aesel", "oskiewar", "aestheticcomputer", "desktop", "tapes",
]);
const PLATFORMS = ["mac", "ios", "ipados", "tvos", "windows", "xbox", "linux"];
const UUID = /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i;
const VERSION = /^\d+(?:\.\d+){0,3}$/;

// Ephemeral abuse guard only, as in visit-track: nothing here reaches the DB.
const rateSalt = randomBytes(32), rates = new Map();
function permitted(event) {
  const now = Date.now();
  const source = event.headers?.["cf-connecting-ip"] || event.headers?.["x-forwarded-for"] || "unknown";
  const key = createHmac("sha256", rateSalt).update(source).digest("hex");
  for (const [id, row] of rates) if (row.until <= now) rates.delete(id);
  let row = rates.get(key);
  if (!row) {
    if (rates.size >= 10000) return false;
    row = { count: 0, until: now + 60000 }; rates.set(key, row);
  }
  return ++row.count <= 60;
}

export function validateOpen(body) {
  if (!body || !APP_OPEN_APPS.includes(body.app) || !PLATFORMS.includes(body.platform) ||
      typeof body.version !== "string" || !VERSION.test(body.version) ||
      typeof body.install !== "string" || !UUID.test(body.install) ||
      typeof body.fresh !== "boolean") return null;
  return { app: body.app, platform: body.platform, version: body.version,
    install: body.install.toLowerCase(), fresh: body.fresh };
}

let indexes;
export async function handler(event) {
  const headers = { "Cache-Control": "no-store" };
  if (event.httpMethod === "OPTIONS") return respond(204, "", headers);
  if (event.httpMethod !== "POST") return respond(405, { error: "POST required" }, headers);
  let body = event.body;
  if (typeof body === "string") {
    if (body.length > 1024) return respond(400, { error: "Invalid open" }, headers);
    try { body = JSON.parse(body); } catch { return respond(400, { error: "Invalid JSON" }, headers); }
  }
  const open = validateOpen(body);
  if (!open) return respond(400, { error: "Invalid open" }, headers);
  if (!permitted(event)) return respond(429, { error: "Open rate limit reached" }, headers);
  try {
    const { db } = await connect();
    const collection = db.collection(APP_OPEN_COLLECTION);
    indexes ||= Promise.all([
      collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
      collection.createIndex({ day: 1, app: 1 }),
    ]).catch(error => { indexes = null; throw error; });
    await indexes;
    const now = new Date(), day = now.toISOString().slice(0, 10);
    const update = {
      $setOnInsert: { app: open.app, install: open.install, day, platform: open.platform,
        country: event.headers?.["cf-ipcountry"] || null,
        expiresAt: new Date(+now + RETENTION_DAYS * 86400000) },
      $set: { version: open.version },
      $inc: { opens: 1 }, $max: { fresh: open.fresh, lastAt: now }, $min: { firstAt: now },
    };
    const _id = `${open.app}:${open.install}:${day}`;
    try {
      await collection.updateOne({ _id }, update, { upsert: true });
    } catch (error) {
      if (error.code !== 11000) throw error; // Two first opens raced the upsert.
      await collection.updateOne({ _id }, update);
    }
    return respond(204, "", headers);
  } catch {
    return respond(503, { error: "Open tracking unavailable" }, headers);
  }
}
