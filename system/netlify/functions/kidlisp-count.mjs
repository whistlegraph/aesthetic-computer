// api/kidlisp-count, 2026.01.03
// Returns total count of kidlisp programs in the database.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  try {
    const database = await connect();
    const collection = database.db.collection("kidlisp");
    const count = await collection.countDocuments();
    await database.disconnect();

    return respond(200, { 
      count,
      collection: "kidlisp"
    });
  } catch (error) {
    console.error("Error fetching kidlisp count:", error);
    return respond(500, { error: "Failed to fetch count" });
  }
}
