// Update a build's status in MongoDB for builds.false.work
// Called to mark builds as stale/failed

import { MongoClient, ObjectId } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const BUILDS_API_KEY = process.env.BUILDS_API_KEY;

export default async (req, context) => {
  const headers = {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Headers": "X-API-Key, Content-Type",
    "Access-Control-Allow-Methods": "POST, PATCH, OPTIONS"
  };

  // Handle CORS preflight
  if (req.method === "OPTIONS") {
    return new Response(null, { status: 204, headers });
  }

  if (req.method !== "POST" && req.method !== "PATCH") {
    return new Response(JSON.stringify({ error: "Method not allowed" }), {
      status: 405,
      headers
    });
  }

  // Verify API key
  const apiKey = req.headers.get("X-API-Key");
  if (!apiKey || apiKey !== BUILDS_API_KEY) {
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
    const body = await req.json();
    const { id, platform, version, status, reason } = body;

    // Must provide either id OR (platform + version)
    if (!id && !(platform && version)) {
      return new Response(JSON.stringify({ 
        error: "Must provide either 'id' or both 'platform' and 'version'" 
      }), {
        status: 400,
        headers
      });
    }

    // Validate status
    const validStatuses = ["ok", "stale", "failed", "superseded"];
    if (status && !validStatuses.includes(status)) {
      return new Response(JSON.stringify({ 
        error: `Invalid status: ${status}. Must be one of: ${validStatuses.join(", ")}` 
      }), {
        status: 400,
        headers
      });
    }

    await client.connect();
    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");

    // Build query
    const query = id 
      ? { _id: new ObjectId(id) }
      : { platform, version };

    // Build update
    const update = {
      $set: {
        updatedAt: new Date()
      }
    };

    if (status) update.$set.status = status;
    if (reason) update.$set.statusReason = reason;

    const result = await builds.updateOne(query, update);

    if (result.matchedCount === 0) {
      return new Response(JSON.stringify({ error: "Build not found" }), {
        status: 404,
        headers
      });
    }

    console.log(`✅ Build updated: ${id || `${platform} ${version}`} -> status: ${status}`);

    return new Response(JSON.stringify({
      success: true,
      modifiedCount: result.modifiedCount
    }), {
      status: 200,
      headers
    });
  } catch (error) {
    console.error("Error updating build:", error);
    return new Response(JSON.stringify({ error: error.message }), {
      status: 500,
      headers
    });
  } finally {
    await client.close();
  }
};
