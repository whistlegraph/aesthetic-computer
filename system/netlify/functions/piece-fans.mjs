// Piece Fans, 2025.12.31
// Get top users ("fans") who visit a specific piece the most.
// Resolves handles from subs at query time.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const { piece, limit = 20 } = event.queryStringParameters || {};

  if (!piece) {
    return respond(400, { error: "piece query parameter required" });
  }

  let database;
  try {
    database = await connect();
  } catch (err) {
    console.error("❌ Database connection failed:", err);
    return respond(500, { error: "Database connection failed" });
  }

  try {
    const userHitsCol = database.db.collection("piece-user-hits");
    const handlesCol = database.db.collection("@handles");

    // Get top users by hits (excluding anonymous)
    const userStats = await userHitsCol
      .find({ piece, user: { $ne: "anonymous" } })
      .sort({ hits: -1 })
      .limit(parseInt(limit))
      .toArray();

    // Resolve handles from subs at query time
    const fans = [];
    for (const u of userStats) {
      const handleDoc = await handlesCol.findOne({ user: u.user });
      if (handleDoc) {
        // Only include users with handles
        fans.push({
          handle: handleDoc._id,
          hits: u.hits,
          firstHit: u.firstHit,
          lastHit: u.lastHit,
        });
      }
    }

    await database.disconnect();
    return respond(200, { piece, fans, total: fans.length });
  } catch (err) {
    console.error("❌ piece-fans error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: "Internal server error" });
  }
}
