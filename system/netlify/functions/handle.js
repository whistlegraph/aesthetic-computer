// @handle, 23.04.30.18.12 🤚
// GET: Get a user @handle out of MongoDB based on their `sub` id from auth0.
// POST: Allows a logged in user to set their social `@handle`. (via MongoDB)
//       Or to "strip" a user of their handle.

/* #region 🏁 TODO 
#endregion */

import {
  authorize,
  userIDFromHandle,
  handleFor,
  hasAdmin,
} from "../../backend/authorization.mjs";

import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { filter } from "../../backend/filter.mjs";
import { connect } from "../../backend/database.mjs";
import * as KeyValue from "../../backend/kv.mjs";
import { respond } from "../../backend/http.mjs";
import * as logger from "../../backend/logger.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    const count = event.queryStringParameters.count;

    if (count) {
      // Return total handle count.
      try {
        const database = await connect(); // 📕 Database
        const collection = database.db.collection("@handles");
        const handles = await collection.estimatedDocumentCount();
        await database.disconnect();
        return respond(200, { handles });
      } catch (error) {
        return respond(500, { message: "Failed to retrieve handle count." });
      }
    } else {
      // Get handle `for`
      const id = event.queryStringParameters.for;
      const result = await handleFor(id);

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
    const tenant = body.tenant || "aesthetic"; // Could be 'sotce'.
    const action = body.action;

    if (action !== "strip") {
      // Make sure handle entry is well formed.
      const validated = validateHandle(handle);

      if (validated !== "valid") {
        return respond(400, { message: validated });
      }

      // Filter handle for profanities.
      if (filter(handle) !== handle) {
        return respond(400, { message: "naughty" });
      }
    }

    // And that we are logged in...
    const user = await authorize(event.headers, tenant);

    if (user && user.email_verified) {
      console.log("User authorized?", user);

      // 🔑 We are logged in!

      console.log("🤖 Connecting to MongoDB...");
      const database = await connect(); // 📕 Database
      console.log("🤖 Connected...");

      const handles = database.db.collection("@handles");

      // Make a "handle" index on the @handles collection that forces
      // them all to be unique, (if it doesn't already exist).
      await handles.createIndex({ handle: 1 }, { unique: true });
      await KeyValue.connect();

      if (tenant === "aesthetic") await logger.link(database);

      // Admin action to delete a user handle from the system, as opposed
      // to setting it.
      // TODO: This currently only works for the 'aesthetic' network.
      if (action === "strip") {
        if ((await hasAdmin(user)) === false) {
          return respond(500, { message: "unauthorized" });
        }
        console.log("🩹 Stripping handle from:", handle);
        let status, response;
        try {
          const sub = await userIDFromHandle(handle, database, true);
          const handledUser = await handles.findOne({ _id: sub });
          if (handledUser) {
            await handles.deleteOne({ _id: sub });
            await KeyValue.del("@handles", handle);
            await KeyValue.del("userIDs", sub);

            await logger.log(`@${handle}'s handle was stripped!`, {
              user: sub,
              action: "handle:strip",
              value: "???",
            });

            status = 200;
            response = { message: "stripped" };
          } else {
            status = 404;
            response = { message: "not found" };
          }
        } catch (error) {
          status = 500;
          response = { message: "error" };
        } finally {
          await database.disconnect();
          await KeyValue.disconnect();
        }
        return respond(status, response);
      }

      // 🌟 Otherwise assume we are creating or modifying a handle.
      console.log(`💁 Setting a handle on ${tenant}:`, handle);

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        let sub = user.sub;

        // 🪷 TODO: Implement `sotce-net` handle creation.
        // - [-] Check for existing handle by checking auth0 tenant / etc.

        if (tenant === "sotce") {
          sub = "sotce-" + user.sub; // Prefix the stored handle subs with 'sotce-'
          // because they are not guaranteed to be unique by auth0: https://community.auth0.com/t/ensuring-unique-user-ids-in-auth0-across-multiple-tenants/120970
          return respond(400, { message: "handles are wip:" + sub });
        }

        // Check if a document with this user's sub already exists
        const existingUser = await handles.findOne({ _id: sub });

        if (existingUser) {
          if (dev) console.log("Current user handle:", existingUser.handle);
          // Replace existing handle or fail if the new handle is already taken
          // by someone else.
          const existingHandle = await handles.findOne({
            handle: existingUser.handle,
          });

          if (existingHandle && existingHandle._id !== sub) {
            throw new Error("Handle taken.");
          }

          if (existingHandle && existingHandle.handle === handle) {
            return respond(400, { message: "same" });
          }

          await handles.updateOne({ _id: sub }, { $set: { handle } });

          if (tenant === "aesthetic") {
            await logger.log(`@${existingUser.handle} is now @${handle}`, {
              user: sub,
              action: "handle:update",
              value: handle,
            }); // 🪵
          }
        } else {
          const existingHandle = await handles.findOne({ handle });
          if (existingHandle) throw new Error("Handle taken");

          // Add a new `@handles` document for this user.
          await handles.insertOne({ _id: sub, handle });

          if (tenant === "aesthetic") {
            await logger.log(`hi @${handle}`, {
              user: sub,
              action: "handle:create",
              value: handle,
            }); // 🪵 Log initial handle creation.
          }
        }

        // Update the redis handle <-> userID cache...
        if (existingUser?.handle)
          await KeyValue.del("@handles", existingUser.handle);

        await KeyValue.set("@handles", handle, user.sub);
        await KeyValue.set("userIDs", user.sub, handle);

        // 🔥 Publish the new handle association to redis.
        //  - [-] `world` needs to pick this up somehow.
        //    - [] handle changes from others
        //    - [] self-handle change from another window
        //  - [] `pond` needs to also do this
        //    - [] handle changes from others
        //    - [] self-handle change from another window
        //  - [] `prompt` also needs to do this
        // + Done
        // - [x] `chat` needs to pick this up somehow.
        //   - [x] self-handle change from another window
        //   - [x] handle changes from others
        // - [x] Test same handle change.
        // - [x] Test naughty handle change.
      } catch (error) {
        console.log("👱 Handle set error:", error);
        return respond(500, {
          message: error.code === 11000 ? "taken" : "error",
        });
      } finally {
        await database.disconnect();
        await KeyValue.disconnect();
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
    console.log("👱 Handle error:", error);
    return respond(400, { message: "error" });
  }
}
