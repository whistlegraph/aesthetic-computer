// Piece Hit, 2025.12.31
// Track and retrieve piece analytics - hits, unique users, top fans.
// POST: Record a hit (with optional auth for per-user tracking)
// GET: Retrieve stats for a piece or top pieces overall

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { recordPieceHit, looksAutomated } from "../../backend/piece-hits.mjs";

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
            const handleDoc = await handlesCol.findOne(
              { user: u.user },
              { projection: { _id: 1, handle: 1 } }, // never pull secret fields into memory
            );
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

      // Machines reach this endpoint the same way a reader does. A signed-in
      // request is a person by definition; an anonymous automated one is not.
      // Answer 200 either way, so a caller has nothing to retry.
      if (!user?.sub && looksAutomated(event.headers)) {
        await database.disconnect();
        return respond(200, { success: true, counted: false });
      }

      await recordPieceHit(database.db, { piece, type, user: user?.sub });

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
