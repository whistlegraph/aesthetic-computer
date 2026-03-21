// Permahandle, 25.12.31
// Look up a user's handle from their permahandle code (e.g., ac25namuc → @jeffrey)

import { handleFromPermahandle } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";

// GET /api/permahandle/ac25namuc → { handle: "jeffrey", sub: "auth0|..." }
export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const code = event.path.split("/").slice(-1)[0]; // Last path slug.

  if (!code) {
    return respond(400, { error: "Malformed request." });
  }

  // Validate format: 9 chars, starts with "ac"
  if (code.length !== 9 || !code.startsWith("ac")) {
    return respond(400, { error: "Invalid permahandle format." });
  }

  try {
    const result = await handleFromPermahandle(code);

    if (result) {
      return respond(200, { 
        handle: result.handle,
        code,
      });
    } else {
      return respond(404, { error: "Permahandle not found." });
    }
  } catch (err) {
    console.error("❌ Permahandle lookup error:", err);
    return respond(500, { error: "Server error." });
  }
}
