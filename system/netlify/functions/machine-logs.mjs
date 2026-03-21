// AC Machine Logs — REST API for machine log retrieval
// GET /api/machine-logs?machineId=X&limit=50       — recent logs for a machine
// GET /api/machine-logs?machineId=X&type=crash      — filter by type
// GET /api/machine-logs?machineId=X&since=ISO_DATE  — logs since timestamp

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

async function ensureIndexes(db) {
  const logs = db.collection("ac-machine-logs");

  await Promise.all([
    logs.createIndex({ machineId: 1, when: -1 }),
    logs.createIndex({ user: 1, type: 1, when: -1 }),
    // TTL index: auto-expire log documents 7 days after receivedAt
    logs.createIndex({ receivedAt: 1 }, { expireAfterSeconds: 604800 }),
  ]);
}

export async function handler(event) {
  // ── Preflight ──────────────────────────────────────────────────────
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  // ── Auth (optional for POST — devices send sub in body) ──────────
  const authHeader =
    event.headers.authorization || event.headers.Authorization;
  const user = authHeader ? await authorize({ authorization: authHeader }) : null;
  // GET requires JWT auth; POST allows device auth via body sub
  if (event.httpMethod === "GET" && (!user || !user.sub)) {
    return respond(401, { success: false, error: "Unauthorized" });
  }

  const database = await connect();

  try {
    const db = database.db;

    // Ensure indexes (idempotent, fast on subsequent calls)
    await ensureIndexes(db);

    const logs = db.collection("ac-machine-logs");

    // ── POST — upload session log from device ─────────────────────────
    // Accepts both JWT auth (from dashboard) and device auth (sub in body)
    if (event.httpMethod === "POST") {
      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        return respond(400, { success: false, error: "Invalid JSON" });
      }

      const { machineId, lines, sessionType, sub: deviceSub } = body || {};
      if (!machineId || !Array.isArray(lines)) {
        return respond(400, {
          success: false,
          error: "Need machineId (string) and lines (array of strings)",
        });
      }

      // Use JWT user if available, fall back to device sub from body
      // (device sends its config.json sub for identification)
      const ownerSub = user?.sub || deviceSub;
      if (!ownerSub) {
        return respond(401, { success: false, error: "No user identity" });
      }

      // Verify machine belongs to user (or register if new)
      const machinesCol = db.collection("ac-machines");
      const existing = await machinesCol.findOne({ machineId });
      if (existing && existing.user !== ownerSub) {
        return respond(403, { success: false, error: "Machine belongs to another user" });
      }
      if (!existing) {
        // Auto-register machine on first log upload
        await machinesCol.insertOne({
          machineId,
          user: ownerSub,
          label: machineId,
          registeredAt: new Date(),
          lastSeen: new Date(),
        });
      } else {
        await machinesCol.updateOne(
          { machineId },
          { $set: { lastSeen: new Date() } },
        );
      }

      const now = new Date();
      const docs = lines.slice(0, 500).map((line, i) => ({
        machineId,
        user: ownerSub,
        type: sessionType || "log",
        level: "info",
        message: typeof line === "string" ? line : String(line),
        when: new Date(now.getTime() - (lines.length - i) * 10),
        receivedAt: now,
      }));

      if (docs.length > 0) {
        await logs.insertMany(docs, { ordered: false });
      }

      return respond(200, { success: true, inserted: docs.length });
    }

    // ── GET — retrieve logs ───────────────────────────────────────────
    if (event.httpMethod !== "GET") {
      return respond(405, { success: false, error: "Method not allowed" });
    }

    const params = event.queryStringParameters || {};

    const { machineId, type, since, limit: rawLimit } = params;

    if (!machineId || typeof machineId !== "string") {
      return respond(400, {
        success: false,
        error: "Missing machineId query parameter",
      });
    }

    const limit = Math.min(200, Math.max(1, Number(rawLimit) || 50));

    // Build query — always scoped to user + machineId
    const query = { machineId, user: user.sub };

    // Optional type filter
    if (type) {
      const validTypes = ["log", "crash", "perf", "boot", "update"];
      if (!validTypes.includes(type)) {
        return respond(400, {
          success: false,
          error: `Invalid type. Must be one of: ${validTypes.join(", ")}`,
        });
      }
      query.type = type;
    }

    // Optional since filter
    if (since) {
      const sinceDate = new Date(since);
      if (isNaN(sinceDate.getTime())) {
        return respond(400, {
          success: false,
          error: "Invalid since parameter. Must be a valid ISO date string.",
        });
      }
      query.when = { $gte: sinceDate };
    }

    const results = await logs
      .find(query)
      .sort({ when: -1 })
      .limit(limit)
      .toArray();

    return respond(200, { success: true, logs: results });
  } catch (err) {
    return respond(500, { success: false, error: err.message });
  } finally {
    await database.disconnect();
  }
}
