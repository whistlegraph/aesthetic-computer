// handles, 2025.10.16
// GET endpoint to list/search handles with autocomplete support

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const { search, limit = "100", tenant = "aesthetic" } = event.queryStringParameters || {};

  try {
    const database = await connect();
    const collection = database.db.collection("@handles");

    let query = {};

    // Filter by tenant (aesthetic vs sotce)
    if (tenant === "sotce") {
      query._id = { $regex: "^sotce-" };
    } else if (tenant === "aesthetic") {
      query._id = { $not: { $regex: "^sotce-" } };
    }

    // Add search filter if provided
    if (search) {
      query.handle = { $regex: `^${search}`, $options: "i" };
    }

    const handles = await collection
      .find(query, { projection: { handle: 1, _id: 0 } })
      .limit(parseInt(limit))
      .sort({ handle: 1 })
      .toArray();

    await database.disconnect();

    return respond(200, {
      handles: handles
        .map((doc) => doc.handle)
        .filter((h) => h && typeof h === "string"), // Filter out null/undefined/non-strings
      count: handles.length,
    });
  } catch (error) {
    console.error("Error fetching handles:", error);
    return respond(500, { error: "Failed to fetch handles" });
  }
}
