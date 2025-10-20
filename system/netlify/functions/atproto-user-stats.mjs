// atproto-user-stats, 25.10.20
// Returns aggregated user statistics from MongoDB for the AT Protocol landing page
// GET /api/atproto-user-stats?limit=100

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const limit = parseInt(event.queryStringParameters?.limit || "100");
    const database = await connect();

    // First, build a map of user codes to actual handles from the users collection
    const userCodeToHandle = new Map();
    const usersCollection = database.db.collection("users");
    const allUsersData = await usersCollection.find({}, { 
      projection: { code: 1, handle: 1 } 
    }).toArray();
    
    for (const userData of allUsersData) {
      if (userData.code) {
        userCodeToHandle.set(userData.code, userData.handle || null);
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

    // 1. Count paintings per user
    const paintingStats = await database.db.collection("paintings").aggregate([
      {
        $group: {
          _id: "$owner.handle",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of paintingStats) {
      addUserStats(stat._id, "computer.aesthetic.painting", stat.count);
    }

    // 2. Count moods per user
    const moodStats = await database.db.collection("moods").aggregate([
      {
        $group: {
          _id: "$for",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of moodStats) {
      addUserStats(stat._id, "computer.aesthetic.mood", stat.count);
    }

    // 3. Count pieces per user
    const pieceStats = await database.db.collection("pieces").aggregate([
      {
        $group: {
          _id: "$owner.handle",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of pieceStats) {
      addUserStats(stat._id, "computer.aesthetic.piece", stat.count);
    }

    // 4. Count kidlisp per user
    const kidlispStats = await database.db.collection("kidlisp").aggregate([
      {
        $group: {
          _id: "$owner.handle",
          count: { $sum: 1 }
        }
      }
    ]).toArray();

    for (const stat of kidlispStats) {
      addUserStats(stat._id, "computer.aesthetic.kidlisp", stat.count);
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
