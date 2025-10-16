// Update Painting Slug, 25.10.16
// Updates an anonymous painting's slug to include recording code (colon format)

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body);
  } catch (error) {
    return respond(400, { message: "Cannot parse input body." });
  }

  const { oldSlug, newSlug } = body;

  if (!oldSlug || !newSlug) {
    return respond(400, { message: "oldSlug and newSlug are required" });
  }

  // Verify newSlug is in colon format (imageCode:recordingCode)
  if (!newSlug.includes(":")) {
    return respond(400, { message: "newSlug must be in colon format (imageCode:recordingCode)" });
  }

  const database = await connect();

  try {
    const collection = database.db.collection("paintings");

    // Find the painting by oldSlug (anonymous paintings have no user field)
    const result = await collection.updateOne(
      { slug: oldSlug, user: null },
      { $set: { slug: newSlug } }
    );

    if (result.matchedCount === 0) {
      return respond(404, { message: "Painting not found" });
    }

    if (result.modifiedCount === 1) {
      console.log(`✅ Updated painting slug: ${oldSlug} → ${newSlug}`);
      return respond(200, { 
        message: "Slug updated successfully",
        oldSlug,
        newSlug
      });
    }

    return respond(200, { message: "No change needed" });

  } catch (error) {
    console.error(`❌ Failed to update painting slug:`, error);
    return respond(500, { message: error.message || String(error) });
  } finally {
    await database.disconnect();
  }
}
