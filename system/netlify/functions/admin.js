// admin, 23.10.01.12.57
// Maintenance operations and system administration tasks.
// GET: Migrate all paintings from buckets into the database.

/* #region 🏁 TODO 
#endregion */

import { authorize, handleFor } from "../../backend/authorization.mjs";
import { listAndSavePaintings } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  const user = await authorize(event.headers);
  const handle = await handleFor(user.sub);
  let hasPermission = user && user.email_verified && handle === "@jeffrey";

  console.log("HANDLE:", handle, "Sub:", user.sub);

  // A GET request to get a handle from a user `sub`.
  if (hasPermission && event.httpMethod === "GET") {
    // await listAndSavePaintings();
    console.log("PERMISSION GOT!");
  } else {
    return respond(405, { message: "Method Not Allowed" });
  }
}
