// piece-log.mjs - Store per-piece runtime telemetry
// POST /api/piece-log
// GET  /api/piece-log (admin) — list recent runs

import { connect } from "../../backend/database.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "piece-runs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  if (event.httpMethod === "GET") {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { error: "Authentication required" });
    if (!(await hasAdmin(user))) return respond(403, { error: "Admin access required" });

    try {
      const database = await connect();
      const runs = database.db.collection(COLLECTION);
      const qs = event.queryStringParameters || {};
      const limit = Math.min(200, Math.max(1, Number(qs.limit || 50)));
      const query = {};
      if (qs.pieceId) query.pieceId = qs.pieceId;
      if (qs.bootId) query.bootId = qs.bootId;
      if (qs.slug) query.slug = qs.slug;
      if (qs.host) query["meta.host"] = qs.host;

      const results = await runs
        .find(query)
        .sort({ createdAt: -1 })
        .limit(limit)
        .toArray();

      await database.disconnect();
      return respond(200, { runs: results });
    } catch (err) {
      return respond(500, { error: err.message || "Failed to fetch piece runs" });
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

  const { pieceId, phase, meta = {}, data = {} } = body || {};
  if (!pieceId || !phase) return respond(400, { error: "Missing pieceId or phase" });

  try {
    const database = await connect();
    const runs = database.db.collection(COLLECTION);
    const now = new Date();

    const headers = event.headers || {};
    const server = {
      ip: headers["x-nf-client-connection-ip"] || headers["x-forwarded-for"]?.split(",")[0]?.trim() || null,
      country: headers["x-country"] || headers["x-nf-country-code"] || null,
      region: headers["x-nf-subdivision-code"] || null,
      city: headers["x-nf-city"] || null,
    };

    if (phase === "start") {
      await runs.updateOne(
        { pieceId },
        {
          $setOnInsert: {
            pieceId,
            bootId: meta.bootId || null,
            slug: meta.slug || null,
            createdAt: now,
            meta,
            server,
            status: "started",
          },
          $set: { updatedAt: now },
        },
        { upsert: true },
      );
      await database.disconnect();
      return respond(201, { ok: true });
    }

    if (phase === "log") {
      const events = Array.isArray(data.events) ? data.events : [];
      if (events.length) {
        await runs.updateOne(
          { pieceId },
          {
            $push: { events: { $each: events, $slice: -500 } },
            $set: { updatedAt: now },
          },
          { upsert: true },
        );
      }
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "error") {
      await runs.updateOne(
        { pieceId },
        {
          $set: { updatedAt: now, status: "error", error: data },
        },
        { upsert: true },
      );
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "complete") {
      await runs.updateOne(
        { pieceId },
        {
          $set: {
            updatedAt: now,
            completedAt: now,
            status: "complete",
            summary: data,
          },
        },
        { upsert: true },
      );
      await database.disconnect();
      return respond(200, { ok: true });
    }

    await database.disconnect();
    return respond(400, { error: "Unknown phase" });
  } catch (err) {
    return respond(500, { error: err.message || "Failed to log piece run" });
  }
}
