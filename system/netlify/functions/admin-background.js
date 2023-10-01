// admin, 23.10.01.12.57
// Maintenance operations and system administration tasks.
// GET: Migrate all paintings from buckets into the database.

/* #region 🏁 TODO 
#endregion */

import { authorize, handleFor } from "../../backend/authorization.mjs";
import { listAndSavePaintings } from "../../backend/database.mjs";
// import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  const user = await authorize(event.headers);
  const handle = await handleFor(user.sub);

  let hasAdmin =
    user &&
    user.email_verified &&
    handle === "jeffrey" &&
    user.sub === process.env.ADMIN_SUB;

  // A GET request to get a handle from a user `sub`.
  if (hasAdmin && event.httpMethod === "GET") {
    console.log("Migrating paintings from buckets to database...");
    await listAndSavePaintings();
    // ⚠️ These responses shouldn't really matter so long as
    //    this is a background task. 23.10.01.14.55
    //return respond(200, { message: "Paintings migrated!" });
  } else {
    //return respond(405, { message: "Method Not Allowed" });
  }
}
