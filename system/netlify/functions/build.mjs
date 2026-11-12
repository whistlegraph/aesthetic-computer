// Build, 2026.11.12
// GET /api/build → { commit } — the deployed commit (null in local dev).
// GET /api/build?commit=<sha> → { commit, files: { "/path": hash } } — every
// file that commit serves under the build roots. A commit's file list never
// changes, so that answer is cached for good; the service worker asks for a
// new one only when /api/build names a commit it hasn't seen.

import { respond } from "../../backend/http.mjs";
import { deployedCommit, buildManifest, HASH_CHARS } from "../../backend/build-ref.mjs";

export async function handler(event) {
  if (event.httpMethod !== "GET") return respond(405, { error: "GET only" });
  const asked = event.queryStringParameters?.commit;
  if (!asked) {
    return respond(200, { commit: deployedCommit() }, { "Cache-Control": "no-store" });
  }
  if (!/^[0-9a-f]{40}$/.test(asked)) return respond(400, { error: "commit must be a full sha" });
  try {
    const manifest = await buildManifest(asked);
    return respond(200, { ...manifest, hashChars: HASH_CHARS }, {
      "Cache-Control": "public, max-age=31536000, immutable",
    });
  } catch (error) {
    return respond(404, { error: "unknown commit" }, { "Cache-Control": "no-store" });
  }
}
