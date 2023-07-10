// @handle, 23.04.30.18.12 🤚
// GET: Get a user @handle out of MongoDB based on their `sub` id from auth0.
// POST: Allows a logged in user to set their social `@handle`. (via MongoDB)

/* #region 🏁 TODO 
  - [x] Abstract MongoDB into an included header for other api calls.
#endregion */

import { authorize, handleFor } from "../../backend/authorization.mjs";
import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    const id = event.queryStringParameters.for;
    const handle = await handleFor(id);

    if (handle) {
      return respond(200, { handle });
    } else {
      return respond(400, { message: "No handle found." });
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
    if (user) {
      // 🔑 We are logged in!
      const database = await connect(); // 📕 Database
      const collection = database.db.collection("@handles");

      // Make an "handle" index on the @handles collection that forces
      // them all to be unique, (if it doesn't already exist).
      await collection.createIndex({ handle: 1 }, { unique: true });

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        // Check if a document with this user's sub already exists
        const existingUser = await collection.findOne({ _id: user.sub });
        if (existingUser) {
          if (dev)
            console.log("User handle is currently:", existingUser.handle);
          // Fail if the new handle is already taken by someone else.
          await collection.updateOne({ _id: user.sub }, { $set: { handle } });
        } else {
          // Add a new `@handles` document for this user.
          await collection.insertOne({ _id: user.sub, handle });
        }
      } catch (error) {
        return respond(500, { message: error });
      } finally {
        await database.disconnect();
      }
      // Successful handle change...
      return respond(200, { handle: body.handle });
    } else {
      return respond(401, { message: "Authorization failure..." });
    }
  } catch (error) {
    return respond(400, { message: "Cannot parse input body." });
  }
}
