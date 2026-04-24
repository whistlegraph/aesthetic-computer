// chat-messages, 25.11.21.18.30
// GET: Returns recent chat messages from a specific chat instance.
//      Examples: "clock" for Laer-Klokken, "system" for main chat
// Query params:
//   instance  — "clock" or "system" (default: "system")
//   limit     — max messages to return, up to 100 (default: 50)
//   before    — ISO timestamp; return messages strictly older than this
//               (use the oldest `when` from the previous page to paginate back)
// Response is still chronological (oldest → newest) within each page.
// Redis caching: 2 min TTL, keyed on (instance, limit, before).

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

    // Optional `before` cursor for paginating further back in history.
    // Accepts ISO timestamp; rejected if it doesn't parse.
    let before = null;
    if (params.before) {
      const parsed = new Date(params.before);
      if (isNaN(parsed.getTime())) {
        return respond(400, {
          message: "Invalid `before` timestamp; expected ISO 8601 (e.g. 2026-04-20T00:00:00Z)",
        });
      }
      before = parsed;
    }

    // Cache key includes instance, limit, and pagination cursor
    const cacheKey = `give:chat:${instance}:${limit}:${before ? before.toISOString() : "head"}`;

    const result = await getOrCompute(
      cacheKey,
      async () => {
        shell.log(
          `📨 Fetching ${limit} messages for chat instance: ${instance}` +
            (before ? ` (before ${before.toISOString()})` : ""),
        );

        const database = await connect();

        // Use different collections based on instance
        const collectionName = instance === "clock" ? "chat-clock" : "chat-system";
        const collection = database.db.collection(collectionName);

        shell.log(`📂 Using collection: ${collectionName} for instance: ${instance}`);

        // Query for messages, sorted by timestamp descending
        const filter = before ? { when: { $lt: before } } : {};
        const messages = await collection
          .find(filter)
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

        // Join heart counts from the shared hearts collection
        const messageIds = messages.map((m) => m._id.toString());
        const heartCounts = await database.db
          .collection("hearts")
          .aggregate([
            {
              $match: {
                type: `chat-${instance}`,
                for: { $in: messageIds },
              },
            },
            { $group: { _id: "$for", count: { $sum: 1 } } },
          ])
          .toArray();
        const heartMap = Object.fromEntries(
          heartCounts.map((h) => [h._id, h.count]),
        );

        const messagesWithHandles = messages.map((msg) => ({
          id: msg._id.toString(),
          from: (msg.user && handleMap.get(msg.user)) || "anon",
          text: msg.text,
          when: msg.when,
          hearts: heartMap[msg._id.toString()] ?? 0,
        }));

        await database.disconnect();

        shell.log(`✅ Found ${messagesWithHandles.length} messages for instance: ${instance}`);

        // `nextBefore` is the oldest `when` in this page, ready to be passed
        // back as the `before=` query to fetch the previous page.
        const nextBefore =
          messagesWithHandles.length > 0
            ? (messagesWithHandles[0].when instanceof Date
                ? messagesWithHandles[0].when.toISOString()
                : new Date(messagesWithHandles[0].when).toISOString())
            : null;

        return {
          instance,
          count: messagesWithHandles.length,
          messages: messagesWithHandles,
          nextBefore,
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
