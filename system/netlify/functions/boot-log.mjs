// boot-log.mjs - Store boot telemetry
// POST /api/boot-log

import { connect } from "../../backend/database.mjs";
import { authorize, hasAdmin } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod === "GET") {
    // Admin-only: require auth
    const user = await authorize(event.headers);
    if (!user) return respond(401, { error: "Authentication required" });
    if (!(await hasAdmin(user))) return respond(403, { error: "Admin access required" });

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

    // Capture server-side request headers.
    //
    // These used to read Netlify's x-nf-* headers. Production is lith (Caddy +
    // Express) behind Cloudflare, where those headers simply do not exist — so
    // country, region and city were null on every boot ever recorded.
    //
    // Cloudflare was sending the real thing the entire time: the
    // `add_visitor_location_headers` Managed Transform is enabled on the zone,
    // so cf-ipcountry / cf-ipcity / cf-region arrive on every request. Nothing
    // read them. If geo ever goes null again, check that transform first.
    const headers = event.headers || {};
    const server = {
      ip:
        headers["cf-connecting-ip"] ||
        headers["x-forwarded-for"]?.split(",")[0]?.trim() ||
        null,
      country: headers["cf-ipcountry"] || null,
      region: headers["cf-region"] || null,
      city: headers["cf-ipcity"] || null,
      referer: headers["referer"] || headers["referrer"] || null,
    };

    // The signed-in user is not known when `start` fires — auth resolves later —
    // and `start` writes meta with $setOnInsert, so it can never be corrected.
    // Let any later phase upgrade a null user to a real one, without letting it
    // rewrite the rest of meta.
    const lateUser = meta?.user
      ? { $set: { "meta.user": meta.user } }
      : null;
    const withUser = (update) => {
      if (!lateUser) return update;
      return { ...update, $set: { ...(update.$set || {}), ...lateUser.$set } };
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
      await boots.updateOne(
        { bootId },
        withUser(
          events.length
            ? { $push: { events: { $each: events } }, $set: { updatedAt: now } }
            : { $set: { updatedAt: now } },
        ),
        { upsert: true },
      );
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "error") {
      await boots.updateOne(
        { bootId },
        withUser({
          $set: {
            updatedAt: now,
            status: "error",
            error: data,
          },
        }),
        { upsert: true },
      );
      await database.disconnect();
      return respond(200, { ok: true });
    }

    if (phase === "complete") {
      await boots.updateOne(
        { bootId },
        withUser({
          $set: {
            updatedAt: now,
            status: "success",
            completedAt: now,
            summary: data,
          },
        }),
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
