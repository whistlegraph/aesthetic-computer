// Bundle Telemetry API
// Collects performance data from inline bundles running on aesthetic.computer domain
// POST /api/bundle-telemetry
//
// Payload: { type: 'boot' | 'perf' | 'error', data: {...} }
//
// Used for debugging FF1, OBJKT embeds, and other bundle deployments

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Headers": "Content-Type",
      "Access-Control-Allow-Methods": "POST, OPTIONS",
    },
    body: JSON.stringify(body),
  };
}

export async function handler(event) {
  // Handle preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, { ok: true });
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  if (!MONGODB_CONNECTION_STRING) {
    return respond(500, { error: "MongoDB not configured" });
  }

  let payload;
  try {
    payload = JSON.parse(event.body);
  } catch (e) {
    return respond(400, { error: "Invalid JSON" });
  }

  const { type, data } = payload;
  if (!type || !data) {
    return respond(400, { error: "Missing type or data" });
  }

  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const collection = db.collection("bundle-telemetry");

    // Build document
    const doc = {
      type,
      timestamp: new Date(),
      ...data,
      // Add request metadata
      meta: {
        ip: event.headers["x-forwarded-for"] || event.headers["client-ip"] || "unknown",
        userAgent: event.headers["user-agent"] || "unknown",
        referer: event.headers["referer"] || null,
      },
    };

    await collection.insertOne(doc);

    return respond(200, { ok: true, id: doc._id?.toString() });
  } catch (e) {
    console.error("Bundle telemetry error:", e);
    return respond(500, { error: "Database error" });
  } finally {
    await client.close();
  }
}
