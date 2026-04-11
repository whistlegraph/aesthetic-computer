// Get UE plugin releases from MongoDB for builds.false.work
// Reads from the `false.work-plugins` collection populated by
// register-plugin.mjs. Gated by the same builds password as get-builds.mjs.

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

  // Verify password from Authorization header (same pattern as get-builds.mjs)
  const authHeader = event.headers.authorization || event.headers.Authorization;
  const password = authHeader?.replace("Bearer ", "");

  if (!password || password !== BUILDS_PASSWORD) {
    return respond(401, { error: "Unauthorized" });
  }

  try {
    const database = await connect();
    const plugins = database.db.collection("false.work-plugins");

    // Optional filters
    const name = event.queryStringParameters?.name;
    const engineVersion = event.queryStringParameters?.engineVersion;
    const limit = parseInt(event.queryStringParameters?.limit) || 50;

    const query = {};
    if (name) query.name = name;
    if (engineVersion) query.engineVersion = engineVersion;

    const results = await plugins
      .find(query)
      .sort({ timestamp: -1 })
      .limit(limit)
      .toArray();

    await database.disconnect();

    return respond(200, { plugins: results });
  } catch (error) {
    console.error("Error fetching plugins:", error);
    return respond(500, { error: "Database error" });
  }
}
