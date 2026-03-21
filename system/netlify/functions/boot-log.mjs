// boot-log.mjs - Store boot telemetry
// POST /api/boot-log

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod === "GET") {
    try {
      const database = await connect();
      const boots = database.db.collection("boots");
      const limit = Math.min(200, Math.max(1, Number(event.queryStringParameters?.limit || 50)));
      const bootId = event.queryStringParameters?.bootId || null;
      const host = event.queryStringParameters?.host || null;
      const query = {};
      if (bootId) query.bootId = bootId;
      if (host) query["meta.host"] = host;

      const results = await boots
        .find(query)
        .sort({ createdAt: -1 })
        .limit(limit)
        .toArray();

      await database.disconnect();
      return respond(200, { boots: results });
    } catch (err) {
      return respond(500, { error: err.message || "Failed to fetch boots" });
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

  const { bootId, phase, meta = {}, data = {} } = body || {};
  if (!bootId || !phase) {
    return respond(400, { error: "Missing bootId or phase" });
  }

  try {
    const database = await connect();
    const boots = database.db.collection("boots");
    const now = new Date();

    // Capture server-side request headers (Cloudflare, IP, etc.)
    const headers = event.headers || {};
    const server = {
      ip: headers["x-nf-client-connection-ip"] || headers["x-forwarded-for"]?.split(",")[0]?.trim() || null,
      country: headers["x-country"] || headers["x-nf-country-code"] || null,
      region: headers["x-nf-subdivision-code"] || null,
      city: headers["x-nf-city"] || null,
      referer: headers["referer"] || headers["referrer"] || null,
    };

    if (phase === "start") {
      await boots.updateOne(
        { bootId },
        {
          $setOnInsert: {
            bootId,
            createdAt: now,
            meta,
            server,
            status: "started",
          },
          $set: {
            updatedAt: now,
          },
        },
        { upsert: true },
      );
      await database.disconnect();
      return respond(201, { ok: true });
    }

    if (phase === "log") {
      const events = Array.isArray(data.events) ? data.events : [];
      if (events.length) {
        await boots.updateOne(
          { bootId },
          {
            $push: { events: { $each: events } },
            $set: { updatedAt: now },
          },
          { upsert: true },
        );
      } else {
        await boots.updateOne(
          { bootId },
          { $set: { updatedAt: now } },
          { upsert: true },
        );
      }
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "error") {
      await boots.updateOne(
        { bootId },
        {
          $set: {
            updatedAt: now,
            status: "error",
            error: data,
          },
        },
        { upsert: true },
      );
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "complete") {
      await boots.updateOne(
        { bootId },
        {
          $set: {
            updatedAt: now,
            status: "success",
            completedAt: now,
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
    return respond(500, { error: err.message || "Failed to log boot" });
  }
}
