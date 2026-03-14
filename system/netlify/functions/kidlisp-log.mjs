// kidlisp-log.mjs - GPU/rendering telemetry from KidLisp + graph.mjs
// POST /api/kidlisp-log — accepts batched GPU failure + rendering reports
// GET  /api/kidlisp-log — query recent logs (with optional filters)

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "kidlisp-logs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod === "GET") {
    try {
      const database = await connect();
      const col = database.db.collection(COLLECTION);
      const limit = Math.min(500, Math.max(1, Number(event.queryStringParameters?.limit || 100)));
      const type = event.queryStringParameters?.type || null; // "gpu-disabled", "gpu-failure", etc.
      const ua = event.queryStringParameters?.ua || null; // filter by userAgent substring
      const since = event.queryStringParameters?.since || null; // ISO date string

      const query = {};
      if (type) query.type = type;
      if (ua) query["device.userAgent"] = { $regex: ua, $options: "i" };
      if (since) query.createdAt = { $gte: new Date(since) };

      const results = await col
        .find(query)
        .sort({ createdAt: -1 })
        .limit(limit)
        .toArray();

      // Also return collection stats
      const stats = await col.aggregate([
        { $group: { _id: "$type", count: { $sum: 1 } } },
      ]).toArray();

      await database.disconnect();
      return respond(200, { logs: results, stats });
    } catch (err) {
      return respond(500, { error: err.message || "Failed to fetch logs" });
    }
  }

  if (event.httpMethod === "DELETE") {
    // Purge logs (for silo admin use)
    try {
      const database = await connect();
      const col = database.db.collection(COLLECTION);
      const before = event.queryStringParameters?.before; // ISO date
      const query = before ? { createdAt: { $lt: new Date(before) } } : {};
      const result = await col.deleteMany(query);
      await database.disconnect();
      return respond(200, { ok: true, deleted: result.deletedCount });
    } catch (err) {
      return respond(500, { error: err.message || "Failed to purge logs" });
    }
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return respond(400, { error: "Invalid JSON" });
  }

  const { events } = body || {};
  if (!Array.isArray(events) || events.length === 0) {
    return respond(400, { error: "Missing events array" });
  }

  // Cap batch size
  const batch = events.slice(0, 50);

  try {
    const database = await connect();
    const col = database.db.collection(COLLECTION);
    const now = new Date();

    const headers = event.headers || {};
    const server = {
      ip: headers["x-nf-client-connection-ip"] || headers["x-forwarded-for"]?.split(",")[0]?.trim() || null,
      country: headers["x-country"] || headers["x-nf-country-code"] || null,
    };

    const docs = batch.map((evt) => ({
      type: evt.type || "unknown",
      effect: evt.effect || null,
      detail: evt.detail || null,
      device: evt.device || {},
      gpuStatus: evt.gpuStatus || null,
      server,
      createdAt: now,
    }));

    await col.insertMany(docs);

    // Ensure TTL index exists (auto-delete after 30 days)
    try {
      await col.createIndex({ createdAt: 1 }, { expireAfterSeconds: 30 * 86400 });
    } catch {
      // Index may already exist
    }

    await database.disconnect();
    return respond(201, { ok: true, count: docs.length });
  } catch (err) {
    return respond(500, { error: err.message || "Failed to store logs" });
  }
}
