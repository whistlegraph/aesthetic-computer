// Bundle Telemetry Query API
// GET /api/bundle-telemetry-query
//
// Query params:
//   type: 'boot' | 'perf' | 'error' | 'summary'
//   limit: number (default 20)
//   piece: filter by piece name

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

function respond(statusCode, body) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Headers": "Content-Type",
      "Access-Control-Allow-Methods": "GET, OPTIONS",
    },
    body: JSON.stringify(body, null, 2),
  };
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") {
    return respond(200, { ok: true });
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  if (!MONGODB_CONNECTION_STRING) {
    return respond(500, { error: "MongoDB not configured" });
  }

  const { type = 'boot', limit = '20', piece } = event.queryStringParameters || {};
  const limitNum = Math.min(parseInt(limit) || 20, 100);

  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const collection = db.collection("bundle-telemetry");

    // Build query
    const query = {};
    if (type !== 'summary') {
      query.type = type;
    }
    if (piece) {
      query.piece = piece;
    }

    // Summary mode
    if (type === 'summary') {
      const yesterday = new Date(Date.now() - 24 * 60 * 60 * 1000);
      
      const [boots, perfs, errors, avgBootResult] = await Promise.all([
        collection.countDocuments({ type: 'boot', timestamp: { $gte: yesterday } }),
        collection.countDocuments({ type: 'perf', timestamp: { $gte: yesterday } }),
        collection.countDocuments({ type: 'error', timestamp: { $gte: yesterday } }),
        collection.aggregate([
          { $match: { type: 'boot', timestamp: { $gte: yesterday }, bootTime: { $exists: true } } },
          { $group: { _id: null, avgBootTime: { $avg: '$bootTime' } } }
        ]).toArray(),
      ]);

      return respond(200, {
        period: '24h',
        boots,
        perfs,
        errors,
        avgBootTime: avgBootResult[0]?.avgBootTime ? Math.round(avgBootResult[0].avgBootTime) : null,
      });
    }

    // Regular query
    const docs = await collection
      .find(query)
      .sort({ timestamp: -1 })
      .limit(limitNum)
      .toArray();

    // Format for readability
    const results = docs.map(doc => ({
      timestamp: doc.timestamp,
      piece: doc.piece,
      density: doc.density,
      screen: doc.screenWidth && doc.screenHeight ? `${doc.screenWidth}x${doc.screenHeight}` : null,
      bootTime: doc.bootTime,
      samples: doc.samples,
      message: doc.message,
      meta: doc.meta,
    }));

    return respond(200, {
      type,
      count: results.length,
      results,
    });
  } catch (e) {
    console.error("Telemetry query error:", e);
    return respond(500, { error: "Database error" });
  } finally {
    await client.close();
  }
}
