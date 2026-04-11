// user-tapes.mjs — Handle-scoped tape listing
//
// GET /api/user-tapes?handle=<handle>[&limit=50]
//   → returns the most recent tapes uploaded by <handle>, newest first.
//
// Used by the ac-native `tapes.mjs` piece to browse a user's own cloud
// tapes (both ZIP-origin from web AC and MP4-origin from ac-native).
// The handle is resolved to a user sub via the @handles collection, then
// the tapes collection is filtered by { user: sub, nuked: { $ne: true } }.
//
// Response shape:
//   {
//     tapes: [
//       { code, slug, when, mp4Url, thumbnailUrl, kind, source },
//       ...
//     ]
//   }
//
// kind is "zip" (web-origin) or "mp4" (ac-native). source is "ac-native"
// when the tape was uploaded directly from a device without going through
// the oven bake step. Clients can use these to differentiate video vs
// audio-only display where useful.

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

const DEFAULT_LIMIT = 50;
const MAX_LIMIT = 200;

export async function handler(event) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Method not allowed" });
  }

  try {
    const params = event.queryStringParameters || {};
    const handle = (params.handle || "").replace(/^@/, "").trim().toLowerCase();
    const limit = Math.min(
      MAX_LIMIT,
      Math.max(1, parseInt(params.limit || `${DEFAULT_LIMIT}`, 10) || DEFAULT_LIMIT)
    );

    if (!handle) {
      return respond(400, { error: "handle query parameter is required" });
    }

    const database = await connect();
    const handles = database.db.collection("@handles");
    const tapes = database.db.collection("tapes");

    // Resolve handle → user sub
    const handleDoc = await handles.findOne({ handle });
    if (!handleDoc || !handleDoc._id) {
      return respond(404, { error: `Unknown handle @${handle}` });
    }

    const userSub = handleDoc._id;

    // Fetch most-recent non-nuked tapes for this user
    const results = await tapes
      .find({ user: userSub, nuked: { $ne: true } })
      .sort({ when: -1 })
      .limit(limit)
      .project({
        _id: 0,
        code: 1,
        slug: 1,
        when: 1,
        bucket: 1,
        mp4Url: 1,
        thumbnailUrl: 1,
        mp4Status: 1,
        kind: 1,
        source: 1,
      })
      .toArray();

    return respond(200, {
      handle: `@${handle}`,
      count: results.length,
      tapes: results,
    });
  } catch (err) {
    console.error("[user-tapes] error:", err);
    return respond(500, { error: err.message || "Internal error" });
  }
}
