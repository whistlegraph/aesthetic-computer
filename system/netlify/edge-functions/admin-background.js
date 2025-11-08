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

const dev = Netlify.env.get("CONTEXT") === "dev";

export default async (request, context) => {
  const headers = Object.fromEntries(request.headers);
  const url = new URL(request.url);
  const mediaType = url.searchParams.get("migrate");
  
  const user = await authorize(headers);

  // A GET request to migrate media from buckets to database.
  if ((await hasAdmin(user)) && request.method === "GET") {
    console.log(`Migrating ${mediaType}s from buckets to database...`);
    await listAndSaveMedia(mediaType);

    return new Response(
      JSON.stringify({ message: `${mediaType}s migrated!` }),
      {
        status: 200,
        headers: { "Content-Type": "application/json" },
      }
    );
  } else {
    return new Response(
      JSON.stringify({ message: "Method Not Allowed" }),
      {
        status: 405,
        headers: { "Content-Type": "application/json" },
      }
    );
  }
};
