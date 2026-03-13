// AC Machines — REST API for machine management
// GET    /api/machines              — list all machines for authenticated user
// GET    /api/machines?machineId=X  — single machine detail
// GET    /api/machines?machineId=X&logs=true&limit=50 — machine + recent logs
// POST   /api/machines              — update machine label { machineId, label }
// DELETE /api/machines?machineId=X  — soft-delete (set nuked: true)

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const CORS = {
  "Access-Control-Allow-Methods": "GET, POST, DELETE, OPTIONS",
};

const ONLINE_THRESHOLD_MS = 90 * 1000; // 90 seconds

async function ensureIndexes(db) {
  const machines = db.collection("ac-machines");
  const logs = db.collection("ac-machine-logs");

  await Promise.all([
    machines.createIndex({ user: 1, machineId: 1 }, { unique: true }),
    machines.createIndex({ user: 1, lastSeen: -1 }),
    // Log indexes are created in machine-logs.mjs but ensure the one we
    // need for the crash-count aggregation exists here too.
    logs.createIndex({ machineId: 1, when: -1 }),
  ]);
}

function computeStatus(machine) {
  if (!machine.lastSeen) return "offline";
  const age = Date.now() - new Date(machine.lastSeen).getTime();
  if (age > ONLINE_THRESHOLD_MS) return "offline";
  return machine.status || "online";
}

export async function handler(event) {
  // ── Preflight ──────────────────────────────────────────────────────
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "", CORS);
  }

  // ── Auth ───────────────────────────────────────────────────────────
  const authHeader =
    event.headers.authorization || event.headers.Authorization;
  const user = await authorize({ authorization: authHeader });
  if (!user || !user.sub) {
    return respond(401, { success: false, error: "Unauthorized" }, CORS);
  }

  const database = await connect();

  try {
    const db = database.db;
    const machines = db.collection("ac-machines");

    // Ensure indexes (idempotent, fast on subsequent calls)
    await ensureIndexes(db);

    const params = event.queryStringParameters || {};

    // ── GET ────────────────────────────────────────────────────────
    if (event.httpMethod === "GET") {
      const { machineId, logs: wantLogs, limit: rawLimit } = params;

      // Single machine detail
      if (machineId) {
        const machine = await machines.findOne({
          user: user.sub,
          machineId,
          nuked: { $ne: true },
        });

        if (!machine) {
          return respond(
            404,
            { success: false, error: "Machine not found" },
            CORS,
          );
        }

        machine.status = computeStatus(machine);

        let recentLogs = undefined;
        if (wantLogs === "true") {
          const logLimit = Math.min(
            200,
            Math.max(1, Number(rawLimit) || 50),
          );
          const logsCol = db.collection("ac-machine-logs");
          recentLogs = await logsCol
            .find({ machineId, user: user.sub })
            .sort({ when: -1 })
            .limit(logLimit)
            .toArray();
        }

        return respond(
          200,
          { success: true, machine, ...(recentLogs ? { logs: recentLogs } : {}) },
          CORS,
        );
      }

      // List all machines for user
      const allMachines = await machines
        .find({ user: user.sub, nuked: { $ne: true } })
        .sort({ lastSeen: -1 })
        .toArray();

      // Compute live status for each machine
      for (const m of allMachines) {
        m.status = computeStatus(m);
      }

      // Aggregate recent crash counts (last 24h) per machine
      const twentyFourHoursAgo = new Date(Date.now() - 24 * 60 * 60 * 1000);
      const machineIds = allMachines.map((m) => m.machineId);

      let crashCounts = {};
      if (machineIds.length > 0) {
        const logsCol = db.collection("ac-machine-logs");
        const crashAgg = await logsCol
          .aggregate([
            {
              $match: {
                machineId: { $in: machineIds },
                user: user.sub,
                type: "crash",
                when: { $gte: twentyFourHoursAgo },
              },
            },
            {
              $group: {
                _id: "$machineId",
                count: { $sum: 1 },
              },
            },
          ])
          .toArray();

        for (const entry of crashAgg) {
          crashCounts[entry._id] = entry.count;
        }
      }

      // Attach crash counts
      for (const m of allMachines) {
        m.recentCrashes = crashCounts[m.machineId] || 0;
      }

      return respond(200, { success: true, machines: allMachines }, CORS);
    }

    // ── POST — update machine label ────────────────────────────────
    if (event.httpMethod === "POST") {
      let body;
      try {
        body = JSON.parse(event.body || "{}");
      } catch {
        return respond(
          400,
          { success: false, error: "Invalid JSON body" },
          CORS,
        );
      }

      const { machineId, label } = body || {};
      if (!machineId || typeof machineId !== "string") {
        return respond(
          400,
          { success: false, error: "Missing machineId" },
          CORS,
        );
      }
      if (label !== undefined && typeof label !== "string") {
        return respond(
          400,
          { success: false, error: "label must be a string" },
          CORS,
        );
      }

      const now = new Date();
      const result = await machines.updateOne(
        { user: user.sub, machineId, nuked: { $ne: true } },
        {
          $set: {
            label: label || null,
            updatedAt: now,
          },
        },
      );

      if (result.matchedCount === 0) {
        return respond(
          404,
          { success: false, error: "Machine not found" },
          CORS,
        );
      }

      return respond(200, { success: true }, CORS);
    }

    // ── DELETE — soft-delete a machine ─────────────────────────────
    if (event.httpMethod === "DELETE") {
      const { machineId } = params;
      if (!machineId) {
        return respond(
          400,
          { success: false, error: "Missing machineId query parameter" },
          CORS,
        );
      }

      const now = new Date();
      const result = await machines.updateOne(
        { user: user.sub, machineId, nuked: { $ne: true } },
        {
          $set: {
            nuked: true,
            updatedAt: now,
          },
        },
      );

      if (result.matchedCount === 0) {
        return respond(
          404,
          { success: false, error: "Machine not found" },
          CORS,
        );
      }

      return respond(200, { success: true }, CORS);
    }

    // ── Unsupported method ─────────────────────────────────────────
    return respond(405, { success: false, error: "Method not allowed" }, CORS);
  } catch (err) {
    return respond(500, { success: false, error: err.message }, CORS);
  } finally {
    await database.disconnect();
  }
}
