// Stories API for false.work builds
// GET: Fetch all stories
// POST: Save a new story
// Requires password authentication via Authorization header

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const BUILDS_PASSWORD = process.env.BUILDS_PASSWORD;

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, "");
  }

  // Verify password
  const authHeader = event.headers.authorization || event.headers.Authorization;
  const password = authHeader?.replace("Bearer ", "");

  if (!password || password !== BUILDS_PASSWORD) {
    return respond(401, { error: "Unauthorized" });
  }

  try {
    const database = await connect();
    // Keep using false.work-reports collection for backwards compatibility
    const stories = database.db.collection("false.work-reports");

    // GET - Fetch all stories
    if (event.httpMethod === "GET") {
      const allStories = await stories
        .find({})
        .sort({ createdAt: -1 })
        .toArray();

      await database.disconnect();
      return respond(200, { stories: allStories });
    }

    // POST - Save a new story
    if (event.httpMethod === "POST") {
      const body = JSON.parse(event.body);
      const { 
        buildPlatform, 
        buildVersion, 
        tester, 
        summary, 
        lowHangingFruit, 
        onTheSide, 
        biggerThoughts 
      } = body;

      if (!buildPlatform || !buildVersion) {
        await database.disconnect();
        return respond(400, { error: "Missing buildPlatform or buildVersion" });
      }

      const story = {
        buildPlatform,
        buildVersion,
        tester: tester || "Anonymous",
        summary: summary || "",
        lowHangingFruit: lowHangingFruit || [],
        onTheSide: onTheSide || [],
        biggerThoughts: biggerThoughts || [],
        createdAt: new Date(),
        updatedAt: new Date(),
        deleted: false
      };

      const result = await stories.insertOne(story);
      await database.disconnect();

      return respond(201, { 
        success: true,
        storyId: result.insertedId,
        story
      });
    }

    await database.disconnect();
    return respond(405, { error: "Method not allowed" });

  } catch (err) {
    console.error("Stories API error:", err);
    return respond(500, { error: "Failed to process request" });
  }
}
