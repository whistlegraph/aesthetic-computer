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

  // ── Auth ───────────────────────────────────────────────────────────
  const authHeader =
    event.headers.authorization || event.headers.Authorization;
  const user = await authorize({ authorization: authHeader });
  if (!user || !user.sub) {
    return respond(401, { success: false, error: "Unauthorized" });
  }

  // Only GET is supported
  if (event.httpMethod !== "GET") {
    return respond(405, { success: false, error: "Method not allowed" });
  }

  const database = await connect();

  try {
    const db = database.db;

    // Ensure indexes (idempotent, fast on subsequent calls)
    await ensureIndexes(db);

    const logs = db.collection("ac-machine-logs");
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
