// chat-messages, 25.11.21.18.30
// GET: Returns recent chat messages from a specific chat instance.
//      Examples: "clock" for Laer-Klokken, "system" for main chat
// Query params:
//   instance  — "clock", "system", or "all" (default: "system")
//   limit     — max messages to return, up to 100 (default: 50)
//   before    — ISO timestamp; return messages strictly older than this
//               (use the oldest `when` from the previous page to paginate back)
//   search/q  — case-insensitive substring filter over message text.
//               When set, results are newest-first relevance (no reverse) and
//               `instance=all` searches both chat-system and chat-clock.
// Response is chronological (oldest → newest) per page UNLESS searching.
// Redis caching: 2 min TTL, keyed on (instance, limit, before, search).

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

    // Optional case-insensitive text search. When present, `instance=all`
    // spans both chat-system and chat-clock, and results are newest-first
    // relevance (not reversed to chronological).
    const searchRaw = (params.search || params.q || "").trim();
    const searching = searchRaw.length > 0;
    const escaped = searchRaw.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");

    // Cache key includes instance, limit, pagination cursor, and search
    const cacheKey = `give:chat:${instance}:${limit}:${before ? before.toISOString() : "head"}:${searching ? "q=" + searchRaw.toLowerCase() : "noq"}`;

    const result = await getOrCompute(
      cacheKey,
      async () => {
        shell.log(
          `📨 Fetching ${limit} messages for chat instance: ${instance}` +
            (before ? ` (before ${before.toISOString()})` : "") +
            (searching ? ` (search "${searchRaw}")` : ""),
        );

        const database = await connect();

        // "all" is only meaningful while searching; otherwise single instance.
        const instances =
          instance === "all" ? ["system", "clock"] : [instance];

        const baseFilter = {};
        if (before) baseFilter.when = { $lt: before };
        if (searching) baseFilter.text = { $regex: escaped, $options: "i" };

        // Gather from each instance, tagging the source.
        let gathered = [];
        for (const inst of instances) {
          const collectionName = inst === "clock" ? "chat-clock" : "chat-system";
          const collection = database.db.collection(collectionName);
          shell.log(`📂 Using collection: ${collectionName} for instance: ${inst}`);
          const rows = await collection
            .find(baseFilter)
            .sort({ when: -1 })
            .limit(limit)
            .toArray();
          for (const r of rows) r.__instance = inst;
          gathered = gathered.concat(rows);
        }

        // Merge across instances by recency, then cap to the requested limit.
        gathered.sort((a, b) => {
          const ta = a.when instanceof Date ? a.when.getTime() : new Date(a.when).getTime();
          const tb = b.when instanceof Date ? b.when.getTime() : new Date(b.when).getTime();
          return tb - ta;
        });
        const messages = gathered.slice(0, limit);

        // Non-search pages stay chronological (oldest → newest) as before.
        if (!searching) messages.reverse();

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

        // Join heart counts per source instance (heart `type` is chat-<instance>).
        const heartMap = {};
        for (const inst of instances) {
          const idsForInst = messages
            .filter((m) => (m.__instance || instance) === inst)
            .map((m) => m._id.toString());
          if (idsForInst.length === 0) continue;
          const heartCounts = await database.db
            .collection("hearts")
            .aggregate([
              { $match: { type: `chat-${inst}`, for: { $in: idsForInst } } },
              { $group: { _id: "$for", count: { $sum: 1 } } },
            ])
            .toArray();
          for (const h of heartCounts) heartMap[h._id] = h.count;
        }

        const messagesWithHandles = messages.map((msg) => ({
          id: msg._id.toString(),
          from: (msg.user && handleMap.get(msg.user)) || "anon",
          text: msg.text,
          when: msg.when,
          instance: msg.__instance || instance,
          hearts: heartMap[msg._id.toString()] ?? 0,
        }));

        await database.disconnect();

        shell.log(`✅ Found ${messagesWithHandles.length} messages for instance: ${instance}`);

        // `nextBefore` = the oldest `when` in this page (works whether the
        // page is chronological or newest-first relevance).
        const oldest =
          messagesWithHandles.length > 0
            ? messagesWithHandles.reduce((acc, m) => {
                const t = m.when instanceof Date ? m.when : new Date(m.when);
                return t < acc ? t : acc;
              }, new Date(8640000000000000))
            : null;
        const nextBefore = oldest ? oldest.toISOString() : null;

        return {
          instance,
          search: searching ? searchRaw : undefined,
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
