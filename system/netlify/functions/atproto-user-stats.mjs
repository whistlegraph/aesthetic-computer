// atproto-user-stats, 25.10.20
// Returns aggregated user statistics from MongoDB for the AT Protocol landing page
// GET /api/atproto-user-stats?limit=100

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 200,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Headers": "Content-Type",
        "Access-Control-Allow-Methods": "GET, OPTIONS"
      },
      body: ""
    };
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const limit = parseInt(event.queryStringParameters?.limit || "100");
    const database = await connect();

    // First, build maps of Auth0 IDs and user codes to actual handles from the users collection
    const userCodeToHandle = new Map();
    const auth0IdToCode = new Map();
    const auth0IdToHandle = new Map();
    const usersCollection = database.db.collection("users");
    const allUsersData = await usersCollection.find({}, { 
      projection: { _id: 1, code: 1, "atproto.handle": 1 } 
    }).toArray();
    
    for (const userData of allUsersData) {
      const handle = userData.atproto?.handle || null;
      if (userData.code) {
        userCodeToHandle.set(userData.code, handle);
      }
      if (userData._id) {
        auth0IdToCode.set(userData._id, userData.code);
        auth0IdToHandle.set(userData._id, handle);
      }
    }

    // Aggregate user stats across all collections
    const userStats = new Map();

    // Helper to add user record count
    const addUserStats = (identifier, collection, count) => {
      if (!identifier) return;
      
      // Check if this is a user code (ac + year format)
      const isUserCode = /^ac\d{2}/.test(identifier);
      
      // Get the actual handle if this is a user code
      let handle = identifier;
      let code = null;
      
      if (isUserCode) {
        code = identifier;
        handle = userCodeToHandle.get(identifier) || identifier;
      } else {
        // It's already a handle, try to find the code
        for (const [userCode, userHandle] of userCodeToHandle.entries()) {
          if (userHandle === identifier) {
            code = userCode;
            break;
          }
        }
      }
      
      // Use code as the key if available, otherwise use handle
      const key = code || handle;
      
      if (!userStats.has(key)) {
        userStats.set(key, {
          handle: handle,
          code: code,
          isUserCode: isUserCode && !userCodeToHandle.get(identifier),
          collections: [],
          recordCounts: {},
          totalRecords: 0
        });
      }

      const stats = userStats.get(key);
      if (!stats.collections.includes(collection)) {
        stats.collections.push(collection);
      }
      stats.recordCounts[collection] = count;
      stats.totalRecords += count;
    };

    // 1. Count paintings per user (paintings have 'user' auth0 ID field)
    const paintingStats = await database.db.collection("paintings").aggregate([
      {
        $match: { user: { $exists: true, $ne: null } }
      },
      {
        $group: {
          _id: "$user",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of paintingStats) {
      const code = auth0IdToCode.get(stat._id);
      if (code) {
        addUserStats(code, "computer.aesthetic.painting", stat.count);
      }
    }

    // 2. Count moods per user (moods have 'user' auth0 ID field)
    const moodStats = await database.db.collection("moods").aggregate([
      {
        $match: { user: { $exists: true, $ne: null } }
      },
      {
        $group: {
          _id: "$user",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of moodStats) {
      const code = auth0IdToCode.get(stat._id);
      if (code) {
        addUserStats(code, "computer.aesthetic.mood", stat.count);
      }
    }

    // 3. Count pieces per user (pieces have 'user' auth0 ID field)
    const pieceStats = await database.db.collection("pieces").aggregate([
      {
        $match: { user: { $exists: true, $ne: null } }
      },
      {
        $group: {
          _id: "$user",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of pieceStats) {
      const code = auth0IdToCode.get(stat._id);
      if (code) {
        addUserStats(code, "computer.aesthetic.piece", stat.count);
      }
    }

    // 4. Count kidlisp per user (kidlisp have 'user' auth0 ID field)
    const kidlispStats = await database.db.collection("kidlisp").aggregate([
      {
        $match: { user: { $exists: true, $ne: null } }
      },
      {
        $group: {
          _id: "$user",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of kidlispStats) {
      const code = auth0IdToCode.get(stat._id);
      if (code) {
        addUserStats(code, "computer.aesthetic.kidlisp", stat.count);
      }
    }

    // Convert to array and sort by total records (descending)
    const users = Array.from(userStats.values())
      .sort((a, b) => b.totalRecords - a.totalRecords)
      .slice(0, limit);

    // Calculate totals
    const totalUsers = userStats.size;
    const totalRecords = users.reduce((sum, u) => sum + u.totalRecords, 0);
    const activeUsers = users.filter(u => u.totalRecords > 0).length;

    await database.disconnect();

    return respond(200, {
      users,
      stats: {
        totalUsers,
        totalRecords,
        activeUsers
      }
    });

  } catch (error) {
    console.error("Error getting user stats:", error);
    return respond(500, { 
      message: "Failed to get user stats",
      error: error.message 
    });
  }
}
