// delete-erase-and-forget-me, 23.12.15.13.44

// 1. POST `api/delete-erase-and-forget-me`
// Allow a logged in user to delete their account
// permanently.

/* #region 🏁 TODO 
#endregion */

import {
  authorize,
  getHandleOrEmail,
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
          console.log(err);
        }

        if (!listedObjects.Contents || listedObjects.Contents.length === 0)
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
      console.log("🖼️ Deleted paintings.");

      // Delete `paintings` and `moods` associated with the user's sub.
      await database.db.collection("pieces").deleteMany({ user: sub });
      console.log("🧩️ Deleted pieces.");

      await database.db.collection("moods").deleteMany({ user: sub });
      console.log("🧠 Deleted moods.");

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

      // Assuming @handles collection uses _id as the primary key.
      await database.db.collection("@handles").deleteOne({ _id: sub });
      console.log("🧔 Deleted any handle.");

      // ❤️‍🔥 TODO: Delete moods and chat messages and pieces also...
      // And what about logs related to the user?
      console.log("❌ Deleted database data.");

      await database.disconnect();

      const handle = await getHandleOrEmail(sub);
      if (handle.startsWith("@")) {
        await KeyValue.connect(); // Delete the user's handle from redis.
        await KeyValue.del("@handles", handle);
        await KeyValue.disconnect();
      }

      console.log("❌ Deleted network cache.");

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
