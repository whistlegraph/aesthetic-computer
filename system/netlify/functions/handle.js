// @handle, 23.04.30.18.12 🤚
// GET: Get a user @handle out of MongoDB based on their `sub` id from auth0.
// POST: Allows a logged in user to set their social `@handle`. (via MongoDB)

/* #region 🏁 TODO 
  - [x] Abstract MongoDB into an included header for other api calls.
#endregion */

import { authorize, handleFor } from "../../backend/authorization.mjs";
import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { connect } from "../../backend/database.mjs";
import * as KeyValue from "../../backend/kv.mjs";
import { respond } from "../../backend/http.mjs";
import * as logger from "../../backend/logger.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  const database = await connect(); // 📕 Database
  const collection = database.db.collection("@handles");

  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    const count = event.queryStringParameters.count;

    if (count) {
      // Return total handle count.
      try {
        const handles = await collection.estimatedDocumentCount();
        await database.disconnect();
        return respond(200, { handles });
      } catch (error) {
        await database.disconnect();
        return respond(500, { message: "Failed to retrieve handle count." });
      }
    } else {
      // Get handle `for`
      const id = event.queryStringParameters.for;
      const result = await handleFor(id);
      await database.disconnect();

      if (typeof result === "string") {
        return respond(200, { handle: result });
      } else if (Array.isArray(result) && result.length > 0) {
        return respond(200, { handles: result });
      } else {
        return respond(400, { message: "No handle(s) found." });
      }
    }
  } else if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  // A POST request to set the handle.

  // Parse the body of the HTTP request
  let body;
  try {
    // Make sure we have a username present to set.
    body = JSON.parse(event.body);

    const handle = body.handle;

    // Make sure handle entry is well formed.
    if (!validateHandle(handle)) {
      return respond(400, { message: "Bad handle formatting." });
    }

    // And that we are logged in...
    const user = await authorize(event.headers);
    if (user && user.email_verified) {
      // 🔑 We are logged in!
      const database = await connect(); // 📕 Database
      const handles = database.db.collection("@handles");

      // Make an "handle" index on the @handles collection that forces
      // them all to be unique, (if it doesn't already exist).
      await handles.createIndex({ handle: 1 }, { unique: true });
      await KeyValue.connect();

      logger.link(database, KeyValue);

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        // Check if a document with this user's sub already exists
        const existingUser = await handles.findOne({ _id: user.sub });
        if (existingUser) {
          if (dev) console.log("Current user handle:", existingUser.handle);
          // Replace existing handle or fail if the new handle is already taken
          // by someone else.
          await handles.updateOne({ _id: user.sub }, { $set: { handle } });
          await logger.log(`@${existingUser.handle} is now @${handle}`); // 🪵
        } else {
          // Add a new `@handles` document for this user.
          await handles.insertOne({ _id: user.sub, handle });
          await logger.log(`hi @${handle}`); // 🪵 Log initial handle creation.
        }

        // Update the redis handle cache...
        if (existingUser?.handle)
          await KeyValue.del("@handles", existingUser.handle);

        await KeyValue.set("@handles", handle, user.sub);
        await KeyValue.set("userIDs", user.sub, handle);

        await KeyValue.disconnect();
      } catch (error) {
        return respond(500, { message: error });
      } finally {
        await database.disconnect();
      }
      // Successful handle change...
      return respond(200, { handle: body.handle });
    } else {
      if (user) {
        return respond(401, { message: "unverified" });
      } else {
        return respond(401, { message: "unauthorized" });
      }
    }
  } catch (error) {
    return respond(400, { message: "Cannot parse input body." });
  }
}
