// Metrics, 24.10.20.00.06
// Reports platform activity and statistics for Aesthetic Computer and Sotce Net.

// 1. GET `api/activity`
// Return JSON data.

/* #region 🏁 TODO 
  - [] 
#endregion */

import { activeUsers } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
// const dev = process.env.CONTEXT === "dev";

import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    try {
      const database = await connect();
      await database.disconnect();

      const signups = await querySignups();
      const stats = await activeUsers();

      return respond(200, {
        signups,
        stats,
        handles: 0,
        pieces: 0,
        paintings: 0,
        moods: 0,
      });
    } catch (err) {
      return respond(500, { message: error });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
