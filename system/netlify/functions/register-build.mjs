// Register a new build in MongoDB for builds.false.work
// Called by build scripts with API key authentication

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";
const BUILDS_API_KEY = process.env.BUILDS_API_KEY;

export default async (req, context) => {
  const headers = {
    "Content-Type": "application/json",
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Headers": "X-API-Key, Content-Type",
    "Access-Control-Allow-Methods": "POST, OPTIONS"
  };

  // Handle CORS preflight
  if (req.method === "OPTIONS") {
    return new Response(null, { status: 204, headers });
  }

  if (req.method !== "POST") {
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
    const build = await req.json();

    // Validate required fields
    const required = ["platform", "version", "timestamp", "sizeMB", "downloadUrl"];
    for (const field of required) {
      if (build[field] === undefined || build[field] === null) {
        return new Response(JSON.stringify({ error: `Missing required field: ${field}` }), {
          status: 400,
          headers
        });
      }
    }

    // Validate platform
    const validPlatforms = ["windows", "mac", "ios"];
    if (!validPlatforms.includes(build.platform)) {
      return new Response(JSON.stringify({ error: `Invalid platform: ${build.platform}. Must be one of: ${validPlatforms.join(", ")}` }), {
        status: 400,
        headers
      });
    }

    await client.connect();
    const db = client.db(MONGODB_NAME);
    const builds = db.collection("false.work-builds");

    // Ensure index exists (idempotent)
    await builds.createIndex({ timestamp: -1 });
    await builds.createIndex({ platform: 1, version: 1 }, { unique: true });

    // Convert timestamp string to Date
    const buildDoc = {
      ...build,
      timestamp: new Date(build.timestamp),
      createdAt: new Date()
    };

    // Check for duplicate (same platform + version)
    const existing = await builds.findOne({
      platform: build.platform,
      version: build.version
    });

    if (existing) {
      return new Response(JSON.stringify({
        error: "Build already exists",
        existingId: existing._id.toString()
      }), {
        status: 409,
        headers
      });
    }

    const result = await builds.insertOne(buildDoc);

    console.log(`✅ Build registered: ${build.platform} ${build.version}`);

    return new Response(JSON.stringify({
      success: true,
      insertedId: result.insertedId.toString(),
      build: {
        ...buildDoc,
        _id: result.insertedId.toString()
      }
    }), {
      status: 201,
      headers
    });
  } catch (error) {
    console.error("Error registering build:", error);

    // Handle duplicate key error
    if (error.code === 11000) {
      return new Response(JSON.stringify({ error: "Build already exists (duplicate key)" }), {
        status: 409,
        headers
      });
    }

    return new Response(JSON.stringify({ error: error.message }), {
      status: 500,
      headers
    });
  } finally {
    await client.close();
  }
};
