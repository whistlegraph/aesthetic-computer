// paper-hit.mjs - Track paper PDF views/downloads
// POST /api/paper-hit  { paper, format, lang }
// GET  /api/paper-hit   → aggregate counts

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  const database = await connect();
  const hits = database.db.collection("paper-hits");

  try {
    if (event.httpMethod === "GET") {
      const pipeline = [
        {
          $group: {
            _id: { paper: "$paper", format: "$format" },
            count: { $sum: 1 },
            lastHit: { $max: "$createdAt" },
          },
        },
        { $sort: { count: -1 } },
      ];
      const stats = await hits.aggregate(pipeline).toArray();
      await database.disconnect();
      return respond(200, { stats });
    }

    if (event.httpMethod !== "POST") {
      await database.disconnect();
      return respond(405, { error: "Method not allowed" });
    }

    let body;
    try {
      body = JSON.parse(event.body || "{}");
    } catch {
      await database.disconnect();
      return respond(400, { error: "Invalid JSON" });
    }

    const { paper, format = "pdf", lang = "en" } = body || {};
    if (!paper) {
      await database.disconnect();
      return respond(400, { error: "Missing paper" });
    }

    const headers = event.headers || {};
    await hits.insertOne({
      paper,
      format,
      lang,
      createdAt: new Date(),
      ip:
        headers["x-nf-client-connection-ip"] ||
        headers["x-forwarded-for"]?.split(",")[0]?.trim() ||
        null,
      country: headers["x-country"] || headers["x-nf-country-code"] || null,
      referer: headers["referer"] || headers["referrer"] || null,
      ua: headers["user-agent"] || null,
    });

    await database.disconnect();
    return respond(200, { ok: true });
  } catch (err) {
    try { await database.disconnect(); } catch {}
    return respond(500, { error: err.message || "Failed to log hit" });
  }
}
