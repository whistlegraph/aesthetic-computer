// Register a new build in MongoDB for builds.false.work
// Called by build scripts with API key authentication

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const BUILDS_API_KEY = process.env.BUILDS_API_KEY;

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod !== "POST") {
    return respond(405, { error: "Method not allowed" });
  }

  // Verify API key
  const apiKey = event.headers["x-api-key"] || event.headers["X-API-Key"];
  if (!apiKey || apiKey !== BUILDS_API_KEY) {
    return respond(401, { error: "Unauthorized" });
  }

  try {
    const build = JSON.parse(event.body);

    // Validate required fields
    const required = ["platform", "version", "timestamp", "sizeMB", "downloadUrl"];
    for (const field of required) {
      if (build[field] === undefined || build[field] === null) {
        return respond(400, { error: `Missing required field: ${field}` });
      }
    }

    // Validate platform
    const validPlatforms = ["windows", "mac", "ios"];
    if (!validPlatforms.includes(build.platform)) {
      return respond(400, { error: `Invalid platform: ${build.platform}. Must be one of: ${validPlatforms.join(", ")}` });
    }

    const database = await connect();
    const builds = database.db.collection("false.work-builds");

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
      await database.disconnect();
      return respond(409, {
        error: "Build already exists",
        existingId: existing._id.toString()
      });
    }

    const result = await builds.insertOne(buildDoc);
    await database.disconnect();

    console.log(`✅ Build registered: ${build.platform} ${build.version}`);

    return respond(201, {
      success: true,
      insertedId: result.insertedId.toString(),
      build: {
        ...buildDoc,
        _id: result.insertedId.toString()
      }
    });
  } catch (error) {
    console.error("Error registering build:", error);

    // Handle duplicate key error
    if (error.code === 11000) {
      return respond(409, { error: "Build already exists (duplicate key)" });
    }

    return respond(500, { error: error.message });
  }
}
