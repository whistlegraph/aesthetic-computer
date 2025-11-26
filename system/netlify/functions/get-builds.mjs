// Get builds from MongoDB for builds.false.work
// Requires password authentication via Authorization header

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export default async (req, context) => {
  const headers = {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Headers": "Authorization, Content-Type",
    "Access-Control-Allow-Methods": "GET, OPTIONS"
  };

  // Handle CORS preflight
  if (req.method === "OPTIONS") {
    return new Response(null, { status: 204, headers });
  }

  // Verify password from Authorization header
  const authHeader = req.headers.get("Authorization");
  const password = authHeader?.replace("Bearer ", "");

  if (!password || password !== BUILDS_PASSWORD) {
    return new Response(JSON.stringify({ error: "Unauthorized" }), {
      status: 401,
      headers
    });
  }

  if (!MONGODB_CONNECTION_STRING) {
    return new Response(JSON.stringify({ error: "Database not configured" }), {
      status: 500,
      headers
    });
  }

  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");

    // Get query params for filtering
    const url = new URL(req.url);
    const platform = url.searchParams.get("platform");
    const limit = parseInt(url.searchParams.get("limit")) || 50;

    const query = platform ? { platform } : {};

    const results = await builds
      .find(query)
      .sort({ timestamp: -1 })
      .limit(limit)
      .toArray();

    return new Response(JSON.stringify({ builds: results }), {
      status: 200,
      headers
    });
  } catch (error) {
    console.error("Error fetching builds:", error);
    return new Response(JSON.stringify({ error: "Database error" }), {
      status: 500,
      headers
    });
  } finally {
    await client.close();
  }
};
