import { createHmac, randomBytes } from "node:crypto";
import { respond } from "./http.mjs";
import { automatedVisit, RETENTION_DAYS } from "../public/aesthetic.computer/lib/visit-model.mjs";
import { RECOGNITION_COLLECTION, RECOGNITION_ORIGIN, RECOGNITION_PARENTS, validateRecognition } from "../public/aesthetic.computer/lib/network-recognition-model.mjs";

export function createNetworkRecognitionHandler({ authorize, connect, now = () => new Date() }) {
  let indexes;
  // Abuse control only: process-salted keys expire in one minute, never stored.
  const salt = randomBytes(32), rates = new Map();
  return async event => {
    const origin = event.headers?.origin;
    const headers = { "Cache-Control": "no-store", "Vary": "Origin", "Access-Control-Allow-Origin":
      [RECOGNITION_ORIGIN, ...RECOGNITION_PARENTS].includes(origin) ? origin : RECOGNITION_ORIGIN };
    if (![RECOGNITION_ORIGIN, ...RECOGNITION_PARENTS].includes(origin)) return respond(403, "", headers);
    if (event.httpMethod === "OPTIONS") return respond(204, "", headers);
    if (event.httpMethod !== "POST") return respond(405, "", headers);
    if (event.isBase64Encoded || typeof event.body !== "string" || event.body.length > 256) return respond(400, "", headers);
    let body;
    try { body = JSON.parse(event.body); } catch { return respond(400, "", headers); }
    const check = validateRecognition(body, origin);
    if (!check) return respond(400, "", headers);
    if (event.headers?.dnt === "1" || event.headers?.["sec-gpc"] === "1" ||
        automatedVisit({ userAgent: event.headers?.["user-agent"] })) return respond(204, "", headers);
    const at = now();
    const key = createHmac("sha256", salt).update(event.headers?.["cf-connecting-ip"] || event.headers?.["x-forwarded-for"] || "unknown").digest("hex");
    for (const [id, row] of rates) if (row.until <= +at) rates.delete(id);
    const rate = rates.get(key) || { count: 0, until: +at + 60000 };
    if (rates.size >= 10000 && !rates.has(key)) return respond(429, "", headers);
    rates.set(key, rate);
    if (++rate.count > 120) return respond(429, "", headers);
    try {
      const { db } = await connect();
      const collection = db.collection(RECOGNITION_COLLECTION);
      indexes ||= Promise.all([
        collection.createIndex({ expiresAt: 1 }, { expireAfterSeconds: 0 }),
        collection.createIndex({ startedAt: 1, property: 1 }),
      ]).catch(error => { indexes = null; throw error; });
      await indexes;
      const id = `${check.property}:${check.id}`;
      if (check.phase === "started") {
        try {
          await collection.updateOne({ _id: id }, { $setOnInsert: {
            property: check.property, startedAt: at, expiresAt: new Date(+at + RETENTION_DAYS * 86400000),
            acRecognized: false, bridgeCompleted: false,
          } }, { upsert: true });
        } catch (error) { if (error.code !== 11000) throw error; }
      } else {
        // Only upgrade a recently started check. No account or credential is
        // retained, and even this random check ID is unrelated to the fishery.
        const query = { _id: id, startedAt: { $gte: new Date(+at - 60000), $lte: at } };
        if (!await collection.findOne(query, { projection: { _id: 1 } })) return respond(204, "", headers);
        const user = event.headers.authorization ? await authorize(event.headers, "aesthetic") : null;
        await collection.updateOne(query, {
          $max: { acRecognized: Boolean(user?.sub), bridgeCompleted: true },
        });
      }
      // The public collector never discloses an account or recognition result.
      return respond(204, "", headers);
    } catch { return respond(503, "", headers); }
  };
}
