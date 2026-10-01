import { validateAccountActivity, ACCOUNT_ACTIVITY_COLLECTION } from "../public/aesthetic.computer/lib/account-activity-model.mjs";
import { automatedVisit, RETENTION_DAYS } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { respond } from "./http.mjs";
import { createHash } from "node:crypto";

export function createAccountActivityHandler({ authorize, connect }) {
  let indexes;
  const rates = new Map();
  return async event => {
    if (event.httpMethod === "OPTIONS") return respond(204, "");
    if (event.httpMethod !== "POST") return respond(405, { error: "POST required" });
    if (event.isBase64Encoded || typeof event.body !== "string" || event.body.length > 2048)
      return respond(400, { error: "Invalid activity" });
    let body;
    try { body = JSON.parse(event.body); } catch { return respond(400, { error: "Invalid JSON" }); }
    const activity = validateAccountActivity(body, event.headers?.origin);
    if (!activity) return respond(400, { error: "Invalid activity" });
    if (activity.automated || automatedVisit({ userAgent: event.headers?.["user-agent"] })) return respond(204, "");
    try {
      const user = await authorize(event.headers || {}, activity.tenant);
      if (!user?.sub) return respond(401, { error: "Authentication required" });
      const now = new Date(), owner = `${activity.tenant}:${user.sub}`;
      for (const [key, row] of rates) if (row.until <= +now) rates.delete(key);
      const rate = rates.get(owner) || { count: 0, until: +now + 60000 };
      if (rates.size >= 10000 && !rates.has(owner)) return respond(429, { error: "Activity rate limit" });
      rates.set(owner, rate);
      if (++rate.count > 240) return respond(429, { error: "Activity rate limit" });
      const { db } = await connect();
      const collection = db.collection(ACCOUNT_ACTIVITY_COLLECTION);
      indexes ||= Promise.all([
        collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
        collection.createIndex({ user: 1, at: -1 }),
        collection.createIndex({ at: -1 }),
      ]).catch(error => { indexes = null; throw error; });
      await indexes;
      const { id, automated, ...fields } = activity;
      try {
        const key = createHash("sha256").update(`${owner}:${id}`).digest("hex");
        await collection.updateOne({ _id: key }, { $setOnInsert: {
          ...fields, user: user.sub, at: now, expiresAt: new Date(+now + RETENTION_DAYS * 86400000),
        } }, { upsert: true });
      } catch (error) { if (error.code !== 11000) throw error; }
      return respond(204, "", { "Cache-Control": "no-store" });
    } catch { return respond(503, { error: "Activity recording unavailable" }); }
  };
}
