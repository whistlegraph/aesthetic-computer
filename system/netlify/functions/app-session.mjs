// First-party AC iOS usage; payloads contain counters and random app-local IDs.
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { createHmac, randomBytes } from "node:crypto";
import { NATIVE_USAGE_COLLECTION, validateNativeUsage, nativeUsageWrite } from "../../backend/native-usage.mjs";

const salt = randomBytes(32), rates = new Map();
let indexes;
export async function handler(event) {
  const headers = { "Cache-Control": "no-store" };
  if (event.httpMethod === "OPTIONS") return respond(204, "", headers);
  if (event.httpMethod !== "POST") return respond(405, { error: "POST required" }, headers);
  let body = event.body;
  if (typeof body === "string") {
    if (body.length > 1024) return respond(400, { error: "Invalid session" }, headers);
    try { body = JSON.parse(body); } catch { return respond(400, { error: "Invalid session" }, headers); }
  }
  const value = validateNativeUsage(body);
  if (!value) return respond(400, { error: "Invalid session" }, headers);
  const now = Date.now();
  for (const [key, row] of rates) if (row.until <= now) rates.delete(key);
  const address = event.headers?.["cf-connecting-ip"] || event.headers?.["x-forwarded-for"] || "unknown";
  const key = createHmac("sha256", salt).update(address).digest("hex");
  let rate = rates.get(key);
  if (!rate) {
    if (rates.size >= 10000) return respond(429, "", headers);
    rate = { count: 0, until: now + 60000 }; rates.set(key, rate);
  }
  if (++rate.count > 120) return respond(429, "", headers);
  try {
    const { db } = await connect(), collection = db.collection(NATIVE_USAGE_COLLECTION);
    indexes ||= Promise.all([
      collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
      collection.createIndex({ startedAt: 1, app: 1 }),
    ]).catch(e => { indexes = null; throw e; });
    await indexes;
    const { id, update } = nativeUsageWrite(value);
    try { await collection.updateOne({ _id: id }, update, { upsert: true }); }
    catch (e) { if (e.code !== 11000) throw e; await collection.updateOne({ _id: id }, update); }
    return respond(204, "", headers);
  } catch { return respond(503, { error: "Usage counts unavailable" }, headers); }
}
