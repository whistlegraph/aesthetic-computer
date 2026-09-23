// First-party aggregate visit signals. No IP, UA, URL, identity or input storage.
import { connect } from "../../backend/database.mjs";
import { createHmac, randomBytes } from "node:crypto";
import { respond } from "../../backend/http.mjs";
import { validateVisit, visitUpdate, VISIT_COLLECTION } from "../../public/aesthetic.computer/lib/visit-model.mjs";

let indexes;
// Ephemeral abuse guard only: neither addresses nor these salted keys enter DB.
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
  return ++row.count <= 240;
}
export async function handler(event) {
  const headers = { "Cache-Control": "no-store" };
  if (event.httpMethod === "OPTIONS") return respond(204, "", headers);
  if (event.httpMethod !== "POST") return respond(405, { error: "POST required" }, headers);
  if (event.isBase64Encoded || typeof event.body !== "string" || event.body.length > 2048)
    return respond(400, { error: "Invalid visit" }, headers);
  let body;
  try { body = JSON.parse(event.body); } catch { return respond(400, { error: "Invalid JSON" }, headers); }
  const visit = validateVisit(body, event.headers?.origin, event.headers?.["user-agent"]);
  if (!visit) return respond(400, { error: "Invalid visit" }, headers);
  if (!permitted(event)) return respond(429, { error: "Visit rate limit reached" }, headers);
  try {
    const { db } = await connect();
    const collection = db.collection(VISIT_COLLECTION);
    // Retry a failed migration instead of silently keeping data indefinitely.
    indexes ||= Promise.all([
      collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
      collection.createIndex({ startedAt: 1, property: 1 }),
    ]).catch(error => { indexes = null; throw error; });
    await indexes;
    const id = `${visit.property}:${visit.id}`;
    try {
      await collection.updateOne({ _id: id }, visitUpdate(visit), { upsert: true });
    } catch (error) {
      // Two first snapshots can arrive together; merge after the unique-id race.
      if (error.code !== 11000) throw error;
      await collection.updateOne({ _id: id }, visitUpdate(visit));
    }
    return respond(204, "", headers);
  } catch {
    return respond(503, { error: "Visit tracking unavailable" }, headers);
  }
}
