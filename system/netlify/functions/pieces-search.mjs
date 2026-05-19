// pieces-search, 2026.05.18
// GET: search published JavaScript pieces by code/name across all users.
// Powers the universal AC search box (cross-handle piece discovery —
// Feature Vector 1). Patterned on handles.mjs.
//
// Query params:
//   name | q  — case-insensitive substring over `code` and `name`
//   handle    — optional: restrict to one @handle's pieces
//   limit     — max results (default 20, max 50)
//
// Results are ranked by `hits` (popularity) then recency. Each result's
// `enter` is the globally-unique short code (always resolvable at
// aesthetic.computer/<code>); `handle` is provided separately for the
// color-typed result tag.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  const {
    name,
    q,
    handle,
    limit = "20",
  } = event.queryStringParameters || {};

  const term = (name || q || "").trim();
  const max = Math.min(parseInt(limit, 10) || 20, 50);

  try {
    const database = await connect();
    const pieces = database.db.collection("pieces");
    const handlesCol = database.db.collection("@handles");

    // Optional handle scoping: resolve @handle → user sub first.
    let userScope = null;
    if (handle) {
      const clean = handle.replace(/^@/, "");
      const escClean = clean.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      const rec = await handlesCol.findOne(
        { handle: { $regex: `^${escClean}$`, $options: "i" } },
        { projection: { _id: 1 } },
      );
      if (!rec) {
        await database.disconnect();
        return respond(200, { pieces: [], count: 0 });
      }
      userScope = rec._id;
    }

    const filter = { type: "piece" };
    if (userScope) filter.user = userScope;
    if (term) {
      const escaped = term.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      filter.$or = [
        { code: { $regex: escaped, $options: "i" } },
        { name: { $regex: escaped, $options: "i" } },
      ];
    }

    const docs = await pieces
      .find(filter, {
        projection: {
          code: 1,
          name: 1,
          slug: 1,
          user: 1,
          hits: 1,
          when: 1,
          _id: 0,
        },
      })
      .sort({ hits: -1, when: -1 })
      .limit(max)
      .toArray();

    // Resolve author handles for display tagging.
    const subs = [...new Set(docs.map((d) => d.user).filter(Boolean))];
    const handleMap = {};
    if (subs.length > 0) {
      const recs = await handlesCol
        .find(
          { _id: { $in: subs } },
          { projection: { _id: 1, handle: 1 } },
        )
        .toArray();
      for (const r of recs) handleMap[r._id] = r.handle;
    }

    await database.disconnect();

    const results = docs.map((d) => {
      const h = d.user ? handleMap[d.user] : null;
      return {
        code: d.code,
        name: d.name || null,
        handle: h ? `@${h}` : null,
        // Always resolvable (store-piece returns aesthetic.computer/<code>).
        enter: d.code,
        hits: d.hits || 0,
      };
    });

    return respond(200, {
      search: term || undefined,
      count: results.length,
      pieces: results,
    });
  } catch (error) {
    console.error("Error searching pieces:", error);
    return respond(500, { error: "Failed to search pieces" });
  }
}
