// @handle, 23.04.30.18.12 🤚
// GET: Get a user @handle out of MongoDB based on their `sub` id from auth0.
// POST: Allows a logged in user to set their social `@handle`. (via MongoDB)
//       Or to "strip" a user of their handle.

/* #region 🏁 TODO 
#endregion */

import {
  authorize,
  userIDFromHandle,
  userIDFromHandleOrEmail,
  findSisterSub,
  handleFor,
  hasAdmin,
} from "../../backend/authorization.mjs";

import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { filter } from "../../backend/filter.mjs";
import { connect } from "../../backend/database.mjs";
import * as KeyValue from "../../backend/kv.mjs";
import { respond } from "../../backend/http.mjs";
import * as logger from "../../backend/logger.mjs";
import { shell } from "../../backend/shell.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    const count = event.queryStringParameters.count;

    if (count) {
      // Return total handle count.

      // TODO: 👻 Count only "sotce-" prefixed primary keys if tenant is "sotce"
      //          and discount them if tenant is "aesthetic".
      const tenant = event.queryStringParameters.tenant || "all";

      try {
        const database = await connect(); // 📕 Database
        const collection = database.db.collection("@handles");

        let handles;
        if (tenant === "sotce") {
          handles = await collection.countDocuments({
            _id: { $regex: "^sotce-" },
          });
        } else if (tenant === "aesthetic") {
          handles = await collection.countDocuments({
            _id: { $not: { $regex: "^sotce-" } },
          });
        } else {
          handles = await collection.estimatedDocumentCount(); // Default to all documents
        }

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
        return respond(404, { message: "No handle(s) found." });
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

    let handle = body.handle;
    if (handle[0] === "@") handle = handle.slice(1); // Remove any user prefixed "@".

    const tenant = body.tenant || "aesthetic"; // Could be 'sotce'.
    const action = body.action;

    if (
      action !== "strip" &&
      action !== "chat-system:mute" &&
      action !== "chat-system:unmute"
    ) {
      // Make sure handle entry is well formed.
      const validated = validateHandle(handle);

      if (validated !== "valid") {
        return respond(400, { message: validated });
      }

      // Filter handle for profanities.
      if (filter(handle) !== handle) {
        return respond(400, { message: "naughty" });
      }

      // ⚠️ "too long", "invalid", and "naughty" are invalid handle responses
    }

    // And that we are logged in...
    const user = await authorize(event.headers, tenant);

    // shell.log("🙆 Handle update for user:", user);

    if (user && user.email_verified) {
      // 🔑 We are logged in!

      shell.log("🤖 Connecting to MongoDB...");
      const database = await connect(); // 📕 Database
      shell.log("🤖 Connected...");

      const handles = database.db.collection("@handles");

      // Make a "handle" index on the @handles collection that forces
      // them all to be unique, (if it doesn't already exist).
      await handles.createIndex({ handle: 1 }, { unique: true });

      await KeyValue.connect(); // 📓 Note: Not actually needed for mute actions.

      /* if (tenant === "aesthetic") */ await logger.link(database);

      // Admin action to delete a user handle from the system, as opposed
      // to setting it. Handles are stripped regardless of what
      // network they were created on.
      if (action === "strip") {
        if ((await hasAdmin(user)) === false) {
          return respond(500, { message: "unauthorized" });
        }
        shell.log("🩹 Stripping handle from:", handle);

        let status, response;
        try {
          const sub = await userIDFromHandle(handle, database, true);
          const handledUser = await handles.findOne({ _id: sub });

          if (handledUser) {
            await handles.deleteOne({ _id: sub });
            await KeyValue.del("@handles", handle); // Delete original handle cache.

            // Delete sister network redis cache reference.
            const sisterSub = await findSisterSub(sub);
            if (sisterSub) await KeyValue.del("userIDs", sisterSub);
            await KeyValue.del("userIDs", sub); // Delete original cache.

            if (!sub.startsWith("sotce-")) {
              await logger.log(`@${handle}'s handle was stripped!`, {
                user: sub,
                action: "handle:strip",
                value: "???",
              });
            }
            // ⚠️
            // TODO: Stripping a handle from a 'sotce' user
            //       will not refresh their client automatically. 24.08.31.01.18
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

      if (action === "chat-system:mute" || action === "chat-system:unmute") {
        if ((await hasAdmin(user)) === false) {
          return respond(500, { message: "unauthorized" });
        }

        let errorMessage = "error";
        let status, response;

        try {
          const muting = action === "chat-system:mute";

          if (muting) {
            shell.log("🩹 Muting user:", handle);
          } else {
            shell.log("🩹 Unmuting user:", handle);
          }

          // ⭐
          // 1. Pull the user sub from 'handle' which could be
          //    either a handle, sub, or email address.

          // TODO: 🧑‍🚒 Does this need to be sent an alternate tenancy?
          //       🟪 It will only work on AC for now. 25.02.07.19.05
          const sub = await userIDFromHandleOrEmail(handle, database);
          console.log("🤐 User sub to mute:", sub);

          if (!sub) {
            errorMessage = "user not found";
            throw new Error("User not found.");
          }

          // 2. Add or remove the user sub from mutes collection.
          const mutesCollection = database.db.collection("chat-system-mutes");
          await mutesCollection.createIndex({ user: 1 }, { unique: true });
          //    ^ Force a unique index on mutes.

          let inert = true;

          if (muting) {
            // Add the sub to the mutesCollection if necessary.
            try {
              await mutesCollection.insertOne({ user: sub });
              inert = false;
            } catch (error) {
              if (error.code !== 11000) {
                // Ignore duplicate key error, since user is already muted.
                errorMessage = "already muted";
                throw error;
              }
            }
          } else {
            // Remove the sub from the mutesCollection if necessary.

            // Check if the user is actually muted before deleting
            const muteRecord = await mutesCollection.findOne({ user: sub });

            if (muteRecord) {
              await mutesCollection.deleteOne({ user: sub });
              console.log(`User ${sub} unmuted.`);
              inert = false;
            } else {
              console.log(`User ${sub} was not muted.`);
            }
          }

          // 3. Log it and create a log event to update the message buffer
          //    for users.
          if (!inert) {
            const trueHandle = await handleFor(sub);
            await logger.log(
              `${trueHandle ? ("@" + trueHandle) : "someone"} was ${action.split(":")[1]}d!`,
              {
                user: sub,
                action,
                // value: sub,
              },
            );
          }

          // 4. Return the proper status and response.
          status = 200;
          response = { message: muting ? "muted" : "unmuted" };
        } catch (error) {
          status = 500;
          console.error("🔥 " + error);
          response = { message: errorMessage };
        } finally {
          await database.disconnect();
          await KeyValue.disconnect();
        }

        return respond(status, response);
      }

      // 🌟 Otherwise assume we are creating or modifying a handle.
      shell.log(`💁 Setting a handle on ${tenant}:`, handle);

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        let sub = user.sub;
        if (tenant === "sotce") sub = "sotce-" + user.sub;
        // Prefix the stored handle subs with 'sotce-'
        // because they are not guaranteed to be unique by auth0: https://community.auth0.com/t/ensuring-unique-user-ids-in-auth0-across-multiple-tenants/120970
        let primarySub = sub;
        let otherSub;

        // Check if a document with this user's sub already exists on the
        // current tenant.
        let existingUser = await handles.findOne({ _id: sub });

        // Or try to find an existingUser on the sister tenant, via the user's
        // email address.
        if (!existingUser) {
          let sisterSub = await findSisterSub(sub, { prefixed: true });
          existingUser = await handles.findOne({ _id: sisterSub });
          if (existingUser) primarySub = sisterSub;
        }

        if (existingUser) {
          shell.log("Current user handle:", existingUser.handle);
          // Replace existing handle or fail if the new handle is already taken
          // by someone else.
          const existingHandle = await handles.findOne({ handle });

          if (
            existingHandle &&
            (existingHandle._id !== sub || existingHandle._id !== otherSub)
          ) {
            throw new Error("taken");
          }

          if (existingHandle && existingHandle.handle === handle) {
            return respond(400, { message: "same" });
          }

          await handles.updateOne({ _id: primarySub }, { $set: { handle } });

          if (!primarySub.startsWith("sotce-")) {
            await logger.log(`@${existingUser.handle} is now @${handle}`, {
              user: primarySub,
              action: "handle:update",
              value: handle,
            }); // 🪵
          }
        } else {
          const existingHandle = await handles.findOne({ handle });
          if (existingHandle) throw new Error("taken");

          // Add a new `@handles` document for this user.
          await handles.insertOne({ _id: primarySub, handle });

          if (!primarySub.startsWith("sotce-")) {
            await logger.log(`hi @${handle}`, {
              user: primarySub,
              action: "handle:create",
              value: handle,
            }); // 🪵 Log initial handle creation.
          }
        }

        // Update the redis handle <-> userID cache...
        if (existingUser?.handle)
          await KeyValue.del("@handles", existingUser.handle);

        await KeyValue.set("@handles", handle, primarySub);
        await KeyValue.set("userIDs", sub, handle);

        if (otherSub) {
          await KeyValue.set("userIDs", otherSub, handle);
        }

        // 🔥 Publish the new handle association to redis.
        //  - [-] `world` needs to pick this up somehow.
        //    - [] handle changes from others
        //    - [] self-handle change from another window
        //  - [] `pond` needs to also do this
        //    - [] handle changes from others
        //    - [] self-handle change from another window
        //  - [] `prompt` also needs to do this
      } catch (error) {
        shell.log("👱 Handle set error:", error.message);
        return respond(500, {
          message: error.message === "taken" ? "taken" : "error",
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
    shell.log("👱 Handle error:", error);
    return respond(400, { message: "error" });
  }
}
