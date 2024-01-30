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

  return respond(200, {
    api: {
      wipe: {
        sig: "wipe(color)",
        desc: "Paint all pixels the same `color`.",
      },
      ink: {
        sig: "ink(color)",
        desc: "Select a `color` for painting with."
      },
      line: {
        sig: "line(x1, y1, x2, y2)",
        desc: "Paint straight a 1px line from two points."
      },
    },
  });
}
