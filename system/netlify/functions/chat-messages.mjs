// chat-messages, 25.11.21.18.30
// GET: Returns recent chat messages from a specific chat instance.
//      Examples: "clock" for Laer-Klokken, "system" for main chat
// Now with Redis caching (2 min TTL).

/* #region 🏁 TODO 
  - [] Add pagination support
  - [] Add date range filtering
#endregion */

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import { getOrCompute, CACHE_TTLS } from "../../backend/cache.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") {
    return respond(204, null);
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const params = event.queryStringParameters || {};
    const instance = params.instance || "system";
    const limit = parseInt(params.limit || "50", 10);

    if (limit > 100) {
      return respond(400, { message: "Limit cannot exceed 100" });
    }

    // Cache key includes instance and limit
    const cacheKey = `give:chat:${instance}:${limit}`;

    const result = await getOrCompute(
      cacheKey,
      async () => {
        shell.log(`📨 Fetching ${limit} messages for chat instance: ${instance}`);

        const database = await connect();

        // Use different collections based on instance
        const collectionName = instance === "clock" ? "chat-clock" : "chat-system";
        const collection = database.db.collection(collectionName);

        shell.log(`📂 Using collection: ${collectionName} for instance: ${instance}`);

        // Query for messages, sorted by timestamp descending
        const messages = await collection
          .find({})
          .sort({ when: -1 })
          .limit(limit)
          .toArray();

        // Reverse to get chronological order (oldest to newest)
        messages.reverse();

        // Resolve handles via direct DB lookup (not HTTP) to avoid N+1 self-calls
        const uniqueUserIds = [...new Set(messages.map(m => m.user).filter(Boolean))];
        const handleMap = new Map();

        if (uniqueUserIds.length > 0) {
          const handles = database.db.collection("@handles");
          const handleRecords = await handles
            .find({ _id: { $in: uniqueUserIds } })
            .toArray();
          for (const rec of handleRecords) {
            handleMap.set(rec._id, "@" + rec.handle);
          }
        }

        const messagesWithHandles = messages.map((msg) => ({
          from: (msg.user && handleMap.get(msg.user)) || "anon",
          text: msg.text,
          when: msg.when,
        }));

        await database.disconnect();

        shell.log(`✅ Found ${messagesWithHandles.length} messages for instance: ${instance}`);

        return {
          instance,
          count: messagesWithHandles.length,
          messages: messagesWithHandles,
        };
      },
      CACHE_TTLS.CHAT // 2 minutes
    );

    return respond(200, result);
  } catch (error) {
    shell.error("Error fetching chat messages:", error);
    return respond(500, { message: error.message });
  }
}
