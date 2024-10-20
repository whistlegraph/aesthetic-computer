// Metrics, 24.10.20.00.06
// Reports platform activity and statistics for Aesthetic Computer and Sotce Net.

// 1. GET `api/metrics`
// Return JSON data.

/* #region 🏁 TODO 
#endregion */

import { activeUsers } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
// const dev = process.env.CONTEXT === "dev";

import { shell } from "../../backend/shell.mjs";

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    try {

      // Get active users from `auth0`.
      const active = await activeUsers("aesthetic");
      const activeSotce = await activeUsers("sotce");

      // Pull some stats from the Mongo database.
      const database = await connect();
      const handles = await database.db.collection("@handles").countDocuments();
      const pieces = await database.db.collection("pieces").countDocuments();
      const paintings = await database.db.collection("paintings").countDocuments();
      const moods = await database.db.collection("moods").countDocuments();
      const logs = await database.db.collection("logs").countDocuments();
      await database.disconnect();

      // TODO: Also pull the `sotce-net` subscription count here.

      // TODO: Is there a way I can get number of orders from printful for
      //       stickers printed?

      // Add data from either Cloudflare or Google Analytics.
      // Cloudflare URL: https://dash.cloudflare.com/a23b54e8877a833a1cf8db7765bce3ca/aesthetic.computer/analytics/traffic

      return respond(200, {
        active,
        activeSotce,
        handles,
        pieces,
        paintings,
        moods,
        logs,
      });
    } catch (error) {
      shell.log(error);
      return respond(500, { message: error });
    }
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
