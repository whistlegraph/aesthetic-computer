import { createHmac, randomBytes } from "node:crypto";
import { respond } from "./http.mjs";
import { automatedVisit } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { SIGNUP_COLLECTION, validateSignupEvent, signupUpdate } from "../public/aesthetic.computer/lib/signup-model.mjs";

export function createSignupTrackingHandler({ connect }) {
  let indexes;
  const rates = new Map(), salt = randomBytes(32);
  return async event => {
    const headers = { "Cache-Control": "no-store" };
    const reply = (status, body = "") => respond(status, body, headers);
    if (event.httpMethod === "OPTIONS") return reply(204);
    if (event.httpMethod !== "POST") return reply(405);
    if (event.isBase64Encoded || typeof event.body !== "string" || event.body.length > 2048) return reply(400);
    let body;
    try { body = JSON.parse(event.body); } catch { return reply(400); }
    const value = validateSignupEvent(body, event.headers?.origin);
    if (!value) return reply(400);
    if (event.headers?.dnt === "1" || event.headers?.["sec-gpc"] === "1" || value.automated ||
        automatedVisit({ userAgent: event.headers?.["user-agent"] })) return reply(204);
    // Rate keys live only in memory. No address, UA, email, handle, URL or token
    // enters this collection, and nothing is forwarded to third-party analytics.
    const now = new Date();
    for (const [key, rate] of rates) if (rate.until <= +now) rates.delete(key);
    const key = createHmac("sha256", salt).update(event.headers?.["cf-connecting-ip"] || event.headers?.["x-forwarded-for"] || "unknown").digest("hex");
    if (!rates.has(key) && rates.size >= 10000) return reply(429);
    const rate = rates.get(key) || { count: 0, until: +now + 60000 };
    rates.set(key, rate);
    if (++rate.count > 60) return reply(429);
    try {
      const { db } = await connect();
      const collection = db.collection(SIGNUP_COLLECTION);
      indexes ||= Promise.all([
        collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
        collection.createIndex({ startedAt: 1, mode: 1 }),
      ]).catch(error => { indexes = null; throw error; });
      await indexes;
      const filter = { _id: `${value.property}:${value.id}` };
      try { await collection.updateOne(filter, signupUpdate(value, now), { upsert: true }); }
      catch (error) {
        if (error.code !== 11000) throw error;
        await collection.updateOne(filter, signupUpdate(value, now));
      }
      return reply(204);
    } catch { return reply(503); }
  };
}
