// OS Install Report API
// Collects AC Native OS install/update failures from devices.
// POST /api/os-install-report
//
// Payload:
//   {
//     currentVersion:  "...",  // build name running on device
//     targetVersion:   "...",  // build name we tried to install
//     targetSize:      <int>,  // expected kernel bytes (from .version)
//     failedState:     "downloading" | "downloading-initramfs" | "flashing" | ...
//     errorMsg:        "kernel download failed",
//     progress:        0.47,
//     actualBytes:     <int>, // bytes written to /tmp/vmlinuz.new before failure
//     machineId:       "...", // device machineId
//     handle:          "...", // device-configured handle (optional)
//     bootDevice:      "/dev/sda1",
//     telemetry:       ["...", "..."],  // last N telemetry lines
//   }
//
// Stored in collection "os-install-reports" for later triage.

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
  if (event.httpMethod === "OPTIONS") return respond(200, { ok: true });
  if (event.httpMethod !== "POST") return respond(405, { error: "POST only" });
  if (!MONGODB_CONNECTION_STRING) return respond(500, { error: "MongoDB not configured" });

  let payload;
  try {
    payload = JSON.parse(event.body);
  } catch {
    return respond(400, { error: "Invalid JSON" });
  }

  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  try {
    await client.connect();
    const coll = client.db(MONGODB_NAME).collection("os-install-reports");

    const doc = {
      receivedAt: new Date(),
      currentVersion: String(payload.currentVersion || "").slice(0, 128),
      targetVersion:  String(payload.targetVersion  || "").slice(0, 128),
      targetSize:     Number.isFinite(+payload.targetSize) ? +payload.targetSize : null,
      failedState:    String(payload.failedState || "").slice(0, 64),
      errorMsg:       String(payload.errorMsg    || "").slice(0, 256),
      progress:       Number.isFinite(+payload.progress)   ? +payload.progress   : null,
      actualBytes:    Number.isFinite(+payload.actualBytes) ? +payload.actualBytes : null,
      machineId:      String(payload.machineId || "").slice(0, 128),
      handle:         String(payload.handle    || "").slice(0, 64),
      bootDevice:     String(payload.bootDevice || "").slice(0, 64),
      telemetry:      Array.isArray(payload.telemetry)
                        ? payload.telemetry.slice(-40).map((l) => String(l).slice(0, 200))
                        : [],
      meta: {
        ip: event.headers["x-forwarded-for"] || event.headers["client-ip"] || "unknown",
        userAgent: event.headers["user-agent"] || "unknown",
      },
    };

    const { insertedId } = await coll.insertOne(doc);
    return respond(200, { ok: true, id: insertedId?.toString() });
  } catch (e) {
    console.error("os-install-report error:", e);
    return respond(500, { error: "Database error" });
  } finally {
    await client.close();
  }
}
