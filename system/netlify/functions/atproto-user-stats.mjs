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
    const codeToAuth0Id = new Map();
    const handleToAuth0Id = new Map();
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
        if (userData.code) {
          codeToAuth0Id.set(userData.code, userData._id);
        }
        if (handle) {
          handleToAuth0Id.set(handle, userData._id);
        }
      }
    }

    // Aggregate user stats across all collections
    const userStats = new Map();

    // Helper to add user record count
    const addUserStats = (identifier, collection, count, userId) => {
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
          userId: userId || codeToAuth0Id.get(code) || handleToAuth0Id.get(handle) || null,
          collections: [],
          recordCounts: {},
          totalRecords: 0
        });
      }

      const stats = userStats.get(key);
      if (!stats.userId) {
        stats.userId = userId || codeToAuth0Id.get(code) || handleToAuth0Id.get(handle) || null;
      }
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
        addUserStats(code, "computer.aesthetic.painting", stat.count, stat._id);
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
        addUserStats(code, "computer.aesthetic.mood", stat.count, stat._id);
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
        addUserStats(code, "computer.aesthetic.piece", stat.count, stat._id);
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
        addUserStats(code, "computer.aesthetic.kidlisp", stat.count, stat._id);
      }
    }

    const latestMoodStats = await database.db.collection("moods").aggregate([
      { $match: { user: { $exists: true, $ne: null } } },
      { $sort: { when: -1 } },
      { $group: { _id: "$user", mood: { $first: "$mood" }, when: { $first: "$when" } } }
    ]).toArray();

    const latestKidlispStats = await database.db.collection("kidlisp").aggregate([
      { $match: { user: { $exists: true, $ne: null } } },
      { $sort: { when: -1 } },
      { $group: { _id: "$user", code: { $first: "$code" }, when: { $first: "$when" } } }
    ]).toArray();

    const latestMoodByUser = new Map(latestMoodStats.map(stat => [stat._id, stat]));
    const latestKidlispByUser = new Map(latestKidlispStats.map(stat => [stat._id, stat]));

    // Convert to array and sort by total records (descending)
    const users = Array.from(userStats.values())
      .map(user => {
        if (user.userId) {
          const mood = latestMoodByUser.get(user.userId);
          const kidlisp = latestKidlispByUser.get(user.userId);
          if (mood) {
            user.latestMood = mood.mood;
            user.latestMoodWhen = mood.when;
          }
          if (kidlisp) {
            user.latestKidlispCode = kidlisp.code;
            user.latestKidlispWhen = kidlisp.when;
          }
        }
        return user;
      })
      .sort((a, b) => b.totalRecords - a.totalRecords)
      .slice(0, limit);

    // Calculate totals
    const totalUsers = allUsersData.length; // Total users in the users collection
    const totalRecordsAll = Array.from(userStats.values()).reduce((sum, u) => sum + u.totalRecords, 0);
    const activeUsers = userStats.size; // Users with at least one record

    await database.disconnect();

    return respond(200, {
      users,
      stats: {
        totalUsers,
        totalRecords: totalRecordsAll,
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
