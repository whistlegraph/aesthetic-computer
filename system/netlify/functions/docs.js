// Docs, 24.01.29.21.54
// Return up to date data for the aesthetic.computer pieces api.

/* #region 🏁 TODO 
#endregion */

import { respond } from "../../backend/http.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  return respond(200, { apiWords: ["line", "ink", "dog"], whatever: "ok" });
}
