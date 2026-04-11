// Register a new UE plugin release in MongoDB for builds.false.work
// Called by false.work/unreal-builder/scripts/register-serialcom-plugin.fish
// (and similar future helper scripts) with API key authentication.
//
// Plugins live in a separate MongoDB collection from game builds so the
// existing game-build schema and register-build.mjs function stay untouched.
// Collection: `false.work-plugins`
// Upsert key: { name, version } (re-registering with same version replaces)

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
    const plugin = JSON.parse(event.body);

    // Validate required fields
    const required = ["name", "version", "engineVersion", "artifacts"];
    for (const field of required) {
      if (plugin[field] === undefined || plugin[field] === null) {
        return respond(400, { error: `Missing required field: ${field}` });
      }
    }

    // Validate artifacts shape
    if (!Array.isArray(plugin.artifacts) || plugin.artifacts.length === 0) {
      return respond(400, { error: "artifacts must be a non-empty array" });
    }
    for (const a of plugin.artifacts) {
      if (!a.kind || !a.label || !a.downloadUrl) {
        return respond(400, {
          error: "each artifact needs { kind, label, downloadUrl }",
        });
      }
    }

    const database = await connect();
    const plugins = database.db.collection("false.work-plugins");

    // Indexes (idempotent)
    await plugins.createIndex({ timestamp: -1 });
    await plugins.createIndex({ name: 1, version: 1 }, { unique: true });

    const now = new Date();
    const pluginDoc = {
      ...plugin,
      timestamp: plugin.timestamp ? new Date(plugin.timestamp) : now,
      updatedAt: now,
    };

    // Upsert so re-running the registration script with the same name+version
    // updates in place instead of erroring.
    const result = await plugins.findOneAndUpdate(
      { name: plugin.name, version: plugin.version },
      { $set: pluginDoc, $setOnInsert: { createdAt: now } },
      { upsert: true, returnDocument: "after" },
    );

    await database.disconnect();

    console.log(`✅ Plugin registered: ${plugin.name} ${plugin.version}`);

    return respond(200, {
      success: true,
      plugin: {
        ...result,
        _id: result?._id?.toString(),
      },
    });
  } catch (error) {
    console.error("Error registering plugin:", error);
    return respond(500, { error: error.message });
  }
}
