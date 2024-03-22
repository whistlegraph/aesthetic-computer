// Profile, 23.07.07.23.29
// Get a user's profile / profile page if it exists, as JSON.

// 📕 Examples
// `/profile/user@email.com` (Email)
// `/profile/@jeffrey` (Handle)

/* #region 🏁 TODO 
  + Done
  - [x] Add the current mood to the basic profile request. 
#endregion */

import {
  userIDFromHandle,
  userIDFromEmail,
} from "../../backend/authorization.mjs";
import { connect, moodFor } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const handleOrEmail = event.path.split("/").slice(-1)[0]; // Last path slug.

  if (!handleOrEmail) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Malformed request." }),
    };
  }

  let database;
  let sub;
  if (handleOrEmail.startsWith("@")) {
    // Try and look up `sub` from `handle` in MongoDB.
    database = await connect();
    sub = await userIDFromHandle(handleOrEmail.slice(1), database);
  } else {
    // Assume email and try to look up sub from email via auth0.
    sub = await userIDFromEmail(handleOrEmail);
  }

  if (sub) {
    // Get the user's latest mood.
    database ||= await connect();
    const mood = await moodFor(sub, database); // Could be empty.
    await database.disconnect();
    return respond(200, { sub, mood });
  } else {
    return respond(400, { message: "User not found." });
  }
}
