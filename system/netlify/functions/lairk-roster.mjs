// lairk-roster, 26.09.23
// GET: Who has a spot in `lairk` — every handle that has spoken in Laer
//      Klokken (chat-clock) AND been @mentioned there by someone else.
//      Read over the whole history, not a recent page, so an old mention
//      still counts. The same list will gate walking in lairk.
// Response: { handles: ["jeffrey", ...], colors: { jeffrey: [{r,g,b}, ...] }, at }
//   `colors` are the per-letter handle colors from @handles (index 0 is the
//   "@"), only for handles that have set them.
// Redis caching: 2 min TTL.

import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import { getOrCompute, CACHE_TTLS } from "../../backend/cache.mjs";

// Mirrors validateHandle in lib/text.mjs: letters and digits, with single
// `.` or `_` between them.
const MENTION = /@([a-z0-9]+(?:[._][a-z0-9]+)*)/gi;

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, null);
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const result = await getOrCompute(
      "lairk:roster:v2", // v2 carries colors
      async () => {
        const database = await connect();
        const chat = database.db.collection("chat-clock");
        const kept = { deleted: { $ne: true }, user: { $exists: true } };

        // Sub → handle, for everyone who has spoken.
        const subs = await chat.distinct("user", kept);
        const handleOf = new Map();
        const records = await database.db
          .collection("@handles")
          .find({ _id: { $in: subs } })
          .project({ handle: 1, colors: 1 })
          .toArray();
        const colorsOf = new Map();
        for (const r of records) {
          const h = r.handle.toLowerCase();
          handleOf.set(r._id, h);
          if (Array.isArray(r.colors) && r.colors.length > 0) colorsOf.set(h, r.colors);
        }
        const speakers = new Set(handleOf.values());

        // Mentions, counted only when the mentioner is someone else.
        const mentioned = new Set();
        const cursor = chat
          .find({ ...kept, text: /@/ })
          .project({ user: 1, text: 1 });
        for await (const m of cursor) {
          const by = handleOf.get(m.user);
          for (const [, name] of String(m.text).matchAll(MENTION)) {
            const h = name.toLowerCase();
            if (h !== by) mentioned.add(h);
          }
        }

        await database.disconnect();
        const handles = [...speakers].filter((h) => mentioned.has(h)).sort();
        shell.log(`🗼 lairk roster: ${handles.length} of ${speakers.size} speakers`);
        const colors = {};
        for (const h of handles) if (colorsOf.has(h)) colors[h] = colorsOf.get(h);
        return { handles, colors, at: new Date().toISOString() };
      },
      CACHE_TTLS.CHAT,
    );
    return respond(200, result);
  } catch (error) {
    shell.error?.("lairk-roster failed:", error);
    return respond(500, { message: "Failed to build the lairk roster" });
  }
}
