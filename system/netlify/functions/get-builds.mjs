// Get builds from MongoDB for builds.false.work
// Requires password authentication via Authorization header

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  // Verify password from Authorization header
  const authHeader = event.headers.authorization || event.headers.Authorization;
  const password = authHeader?.replace("Bearer ", "");

  if (!password || password !== BUILDS_PASSWORD) {
    return respond(401, { error: "Unauthorized" });
  }

  try {
    const database = await connect();
    const builds = database.db.collection("false.work-builds");

    // Get query params for filtering
    const platform = event.queryStringParameters?.platform;
    const limit = parseInt(event.queryStringParameters?.limit) || 50;

    const query = platform ? { platform } : {};

    const results = await builds
      .find(query)
      .sort({ timestamp: -1 })
      .limit(limit)
      .toArray();

    await database.disconnect();

    return respond(200, { builds: results });
  } catch (error) {
    console.error("Error fetching builds:", error);
    return respond(500, { error: "Database error" });
  }
}
