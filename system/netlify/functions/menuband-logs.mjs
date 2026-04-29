// Menu Band crash-log intake.
// POST /menuband-logs
//
// The macOS Menu Band app reads its own crash reports from
// ~/Library/Logs/DiagnosticReports/MenuBand-*.ips and POSTs them here
// when the user clicks "Send" in the popover. We never auto-receive —
// uploads are always user-initiated. Body is the raw .ips text;
// metadata comes from request headers so the body stays unmodified.
//
// Headers:
//   Content-Type:           text/plain; charset=utf-8
//   X-Menuband-Filename:    "MenuBand-2026-04-28-211718.ips"
//   X-Menuband-Version:     "0.1"
//
// Stored in collection "menuband-logs" for offline triage.

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

const MAX_BODY_BYTES = 5 * 1024 * 1024; // 5 MB — real .ips files are 80–200 KB

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Headers": "Content-Type, X-Menuband-Filename, X-Menuband-Version",
      "Access-Control-Allow-Methods": "POST, OPTIONS",
    },
    body: JSON.stringify(body),
  };
}

function sanitizeFilename(raw) {
  return String(raw || "")
    .replace(/[^a-zA-Z0-9._-]/g, "_")
    .slice(0, 200) || "crash.ips";
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, { ok: true });
  if (event.httpMethod !== "POST") return respond(405, { error: "POST only" });
  if (!MONGODB_CONNECTION_STRING) return respond(500, { error: "MongoDB not configured" });

  const body = String(event.body || "");
  if (body.length === 0) return respond(400, { error: "empty body" });
  if (body.length > MAX_BODY_BYTES) return respond(413, { error: "too large" });

  const headers = event.headers || {};
  const filename = sanitizeFilename(headers["x-menuband-filename"] || headers["X-Menuband-Filename"]);
  const version  = String(headers["x-menuband-version"]  || headers["X-Menuband-Version"]  || "?")
    .replace(/[^0-9.A-Za-z_-]/g, "")
    .slice(0, 32);

  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  try {
    await client.connect();
    const coll = client.db(MONGODB_NAME).collection("menuband-logs");
    const doc = {
      receivedAt: new Date(),
      filename,
      version,
      bytes: body.length,
      log: body,
      meta: {
        ip: headers["x-forwarded-for"] || headers["client-ip"] || "unknown",
        userAgent: headers["user-agent"] || "unknown",
      },
    };
    const { insertedId } = await coll.insertOne(doc);
    return respond(200, { ok: true, id: insertedId?.toString() });
  } catch (e) {
    return respond(500, { error: "Database error" });
  } finally {
    await client.close();
  }
}
