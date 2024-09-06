// delete-erase-and-forget-me, 23.12.15.13.44

// 1. POST `api/delete-erase-and-forget-me`
// Allow a logged in user to delete their account
// permanently.

/* #region 🏁 TODO 
#endregion */

import {
  authorize,
  getHandleOrEmail,
  userIDFromEmail,
  deleteUser,
} from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  S3Client,
  DeleteObjectsCommand,
  ListObjectsV2Command,
} from "@aws-sdk/client-s3";
import * as KeyValue from "../../backend/kv.mjs";
import { shell } from "../../backend/shell.mjs";

const s3User = new S3Client({
  endpoint: "https://" + process.env.USER_ENDPOINT,
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

// const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // 1. POST: Delete the user's account.
  try {
    const user = await authorize(event.headers);

    if (user) {
      console.log("⚠️  Deleting user:", user.sub);
      // 1. Delete the entire user directory located at the s3User endpoint
      const userDirectory = user.sub + "/"; // Assuming the directory name is the `user.sub`
      const bucketName = process.env.USER_SPACE_NAME; // Replace with your bucket name
      let continuationToken = null;

      do {
        const listParams = {
          Bucket: bucketName,
          Prefix: userDirectory,
          ...(continuationToken && { ContinuationToken: continuationToken }),
        };

        let listedObjects;
        try {
          listedObjects = await s3User.send(
            new ListObjectsV2Command(listParams),
          );
        } catch (err) {
          console.error("List error:", err);
        }

        if (
          !listedObjects ||
          !listedObjects.Contents ||
          listedObjects.Contents.length === 0
        )
          break;

        console.log(
          "🚫 Deleting from bucket storage:",
          listedObjects.Contents.length,
        );

        const deleteParams = {
          Bucket: bucketName,
          Delete: {
            Objects: listedObjects.Contents.map(({ Key }) => ({ Key })),
          },
        };

        await s3User.send(new DeleteObjectsCommand(deleteParams));
        continuationToken = listedObjects.NextContinuationToken;
      } while (continuationToken);

      const database = await connect();
      const sub = user.sub;

      // Delete `paintings` and `moods` associated with the user's sub.
      await database.db.collection("paintings").deleteMany({ user: sub });
      console.log("️🎨 Deleted paintings.");

      // Delete `paintings` and `moods` associated with the user's sub.
      await database.db.collection("pieces").deleteMany({ user: sub });
      console.log("🧩️ Deleted pieces.");

      await database.db.collection("moods").deleteMany({ user: sub });
      console.log("🧠 Deleted moods.");

      await database.db.collection("verifications").deleteOne({ _id: sub });
      console.log("🧏 Deleted verification count.");

      // Rewrite the "text" field to be null / empty and rewrite the user field to be empty
      // rather than simply deleting the records associated with the user sub.
      await database.db
        .collection("chat-system")
        .updateMany({ user: sub }, { $set: { text: "", user: "" } });
      console.log("🧠 Erased chats.");

      // ⚠️ Don't erase any logs for now, in case of partially deleted accounts
      // etc, it may be the last place where the sub / handle connection
      // is stored to benefit the user.

      // Go through the "logs" collection and replace any matches in the "users"
      // array with an empty string.
      // await database.db.collection("logs").updateMany(
      //   { users: { $elemMatch: { $eq: sub } } },
      //   { $set: { "users.$": "" } }
      // );
      // console.log("📜 Removed log references.");

      // Remove the user's handle cache from redis.
      const handle = await getHandleOrEmail(sub);
      if (handle?.startsWith("@")) {
        await KeyValue.connect();
        await KeyValue.del("@handles", handle);
        await KeyValue.del("userIDs", sub);
        await KeyValue.disconnect();
      }

      console.log("❌ Deleted network cache.");

      // If the user has a sotce-net account then don't delete
      // here, but change the primary key of the handle instead.
      if (handle) {
        shell.log(
          "📚 Checking for any `sotce` user with the same email and handle:",
          handle,
        );
        const bareHandle = handle.slice(1); // Remove the "@" from the handle.
        const idRes = await userIDFromEmail(user.email, "sotce");

        // console.log("Sotce user:", idRes);

        if (idRes?.userID && idRes?.email_verified) {
          const handles = database.db.collection("@handles");

          const sotceSub = "sotce-" + idRes.userID;
          // Check if an entry with the same _id already exists
          const existingHandle = await handles.findOne({ _id: sotceSub });

          if (!existingHandle) {
            // If no existing entry, proceed with deletion and insertion
            await handles.deleteOne({ _id: sub });
            await handles.insertOne({ _id: sotceSub, handle: bareHandle });
            shell.log(
              "🧔 Changed primary handle key of 'aesthetic' user:",
              sub,
              "to 'sotce' user:",
              sotceSub,
            );
          } else {
            // If an entry already exists, skip deletion and insertion
            shell.log(
              "🩹 Handle already native to `sotce`, skipping reassignment.",
            );
          }
        } else {
          await database.db.collection("@handles").deleteOne({ _id: sub });
          shell.log("🧔 Deleted user handle for:", sub);
        }
      }

      console.log("❌ Deleted database data.");

      await database.disconnect();

      // 3. Delete the user's auth0 account.
      const deleted = await deleteUser(sub);
      console.log("❌ Deleted user registration:", deleted);

      return respond(200, { result: "Deleted!" }); // Successful account deletion.
    } else {
      return respond(401, { message: "Authorization failure..." });
    }
  } catch (error) {
    return respond(500, { message: error });
  }
}
