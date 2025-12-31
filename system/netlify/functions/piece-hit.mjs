// Piece Hit, 2025.12.31
// Track and retrieve piece analytics - hits, unique users, top fans.
// POST: Record a hit (with optional auth for per-user tracking)
// GET: Retrieve stats for a piece or top pieces overall

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  let database;
  try {
    database = await connect();
  } catch (err) {
    console.error("❌ Database connection failed:", err);
    return respond(500, { error: "Database connection failed" });
  }

  try {
    // GET: Return stats for a piece or all pieces
    if (event.httpMethod === "GET") {
      const { piece, top, users } = event.queryStringParameters || {};
      const hitsCol = database.db.collection("piece-hits");
      const userHitsCol = database.db.collection("piece-user-hits");
      const handlesCol = database.db.collection("@handles");

      if (piece) {
        const stats = await hitsCol.findOne({ piece });

        // Optionally include top users for this piece
        let topUsers = [];
        if (users) {
          const userStats = await userHitsCol
            .find({ piece, user: { $ne: "anonymous" } })
            .sort({ hits: -1 })
            .limit(10)
            .toArray();

          // Resolve handles from subs
          for (const u of userStats) {
            const handleDoc = await handlesCol.findOne({ user: u.user });
            topUsers.push({
              handle: handleDoc?._id || null,
              hits: u.hits,
              lastHit: u.lastHit,
            });
          }
        }

        await database.disconnect();
        return respond(200, {
          piece,
          hits: stats?.hits || 0,
          uniqueUsers: stats?.uniqueUsers || 0,
          firstHit: stats?.firstHit || null,
          lastHit: stats?.lastHit || null,
          topUsers: users ? topUsers : undefined,
        });
      }

      // Return top pieces overall
      const pieces = await hitsCol
        .find({})
        .sort({ hits: -1 })
        .limit(parseInt(top) || 50)
        .toArray();

      await database.disconnect();
      return respond(200, { pieces });
    }

    // POST: Record a hit
    if (event.httpMethod === "POST") {
      const body = JSON.parse(event.body || "{}");
      const { piece, type = "system" } = body;

      if (!piece) {
        await database.disconnect();
        return respond(400, { error: "piece required" });
      }

      // Try to get user from auth header (silent fail for anonymous)
      let user = null;
      try {
        user = await authorize(event.headers);
      } catch (e) {
        /* anonymous hit */
      }

      const now = new Date();
      const today = now.toISOString().split("T")[0];
      const hitsCol = database.db.collection("piece-hits");
      const userHitsCol = database.db.collection("piece-user-hits");

      // Ensure indexes exist (safe to call multiple times)
      try {
        await hitsCol.createIndex({ piece: 1 }, { unique: true });
        await hitsCol.createIndex({ hits: -1 });
        await userHitsCol.createIndex({ piece: 1, user: 1 }, { unique: true });
        await userHitsCol.createIndex({ piece: 1, hits: -1 });
      } catch (indexErr) {
        // Indexes already exist - that's fine
      }

      // 1. Update aggregate stats
      await hitsCol.updateOne(
        { piece },
        {
          $inc: {
            hits: 1,
            [`daily.${today}.hits`]: 1,
          },
          $set: { lastHit: now, type },
          $setOnInsert: { firstHit: now, uniqueUsers: 0 },
        },
        { upsert: true },
      );

      // 2. Update per-user stats
      const userKey = user?.sub || "anonymous";
      const userResult = await userHitsCol.updateOne(
        { piece, user: userKey },
        {
          $inc: { hits: 1 },
          $set: { lastHit: now },
          $setOnInsert: { firstHit: now },
        },
        { upsert: true },
      );

      // If this was a new user for this piece, increment uniqueUsers
      if (userResult.upsertedCount > 0 && userKey !== "anonymous") {
        await hitsCol.updateOne(
          { piece },
          { $inc: { uniqueUsers: 1, [`daily.${today}.unique`]: 1 } },
        );
      }

      await database.disconnect();
      return respond(200, { success: true });
    }

    await database.disconnect();
    return respond(405, { error: "Method not allowed" });
  } catch (err) {
    console.error("❌ piece-hit error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: "Internal server error" });
  }
}
