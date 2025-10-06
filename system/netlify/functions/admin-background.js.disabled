// admin, 23.10.01.12.57
// Maintenance operations and system administration tasks.
// GET: Migrate all paintings from buckets into the database.

/* #region 🏁 TODO 
#endregion */

import {
  authorize,
  hasAdmin,
} from "../../backend/authorization.mjs";
import { listAndSaveMedia } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  const user = await authorize(event.headers);
  const mediaType = event.queryStringParameters.migrate;

  // A GET request to get a handle from a user `sub`.
  if ((await hasAdmin(user)) && event.httpMethod === "GET") {
    console.log(`Migrating ${mediaType}s from buckets to database...`);
    await listAndSaveMedia(mediaType);

    // ⚠️ This response shouldn't really matter so long as
    //    this is a background task. 23.10.01.14.55
    //return respond(200, { message: "Paintings migrated!" });
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
