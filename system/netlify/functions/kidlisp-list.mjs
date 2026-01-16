// KidLisp List, 2025.01.16
// Returns a list of user-created KidLisp pieces with metadata
// GET ?limit=N - Return up to N pieces (default 100)

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  let database;
  try {
    database = await connect();
  } catch (err) {
    console.error("❌ Database connection failed:", err);
    return respond(500, { error: "Database connection failed" });
  }

  try {
    const { limit = "100" } = event.queryStringParameters || {};
    const limitNum = Math.min(parseInt(limit) || 100, 500);
    
    const collection = database.db.collection("kidlisp");
    const handlesCol = database.db.collection("@handles");
    
    // Get pieces that have a code (published/cached), sorted by date descending
    const pieces = await collection
      .find({ 
        code: { $exists: true, $ne: null },
      })
      .sort({ when: -1 })
      .limit(limitNum)
      .project({
        code: 1,
        when: 1,
        user: 1,
        title: 1,
        hits: 1,
      })
      .toArray();
    
    // Resolve handles for users
    const userSubs = [...new Set(pieces.filter(p => p.user).map(p => p.user))];
    const handleDocs = userSubs.length > 0 
      ? await handlesCol.find({ user: { $in: userSubs } }).toArray()
      : [];
    
    const handleMap = {};
    handleDocs.forEach(h => {
      handleMap[h.user] = h._id; // _id is the handle string
    });
    
    // Format response
    const result = pieces.map(p => ({
      code: p.code,
      when: p.when,
      handle: handleMap[p.user] || null,
      title: p.title || null,
      hits: p.hits || 0,
    }));
    
    await database.disconnect();
    return respond(200, { pieces: result });
  } catch (err) {
    console.error("❌ kidlisp-list error:", err);
    if (database) await database.disconnect();
    return respond(500, { error: "Internal server error" });
  }
}
