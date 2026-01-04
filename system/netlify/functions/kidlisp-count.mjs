// api/kidlisp-count, 2026.01.03
// Returns total count of kidlisp programs in the database.
// Now with Redis caching (30 min TTL).

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";
import { getOrCompute, CACHE_KEYS, CACHE_TTLS } from "../../backend/cache.mjs";

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  try {
    const result = await getOrCompute(
      CACHE_KEYS.KIDLISP_COUNT,
      async () => {
        const database = await connect();
        const collection = database.db.collection("kidlisp");
        const count = await collection.countDocuments();
        await database.disconnect();
        return { count, collection: "kidlisp" };
      },
      CACHE_TTLS.KIDLISP_COUNT
    );

    return respond(200, result);
  } catch (error) {
    console.error("Error fetching kidlisp count:", error);
    return respond(500, { error: "Failed to fetch count" });
  }
}
