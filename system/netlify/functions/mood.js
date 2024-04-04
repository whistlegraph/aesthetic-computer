// mood, 23.07.09.17.54

// 1. GET `api/mood/@handle`
// Get the most recent mood from the user's handle or email.

// 2. POST `api/mood`
// Allows a logged in user to set their mood, which
// updates the aesthetic.computer motd as well.

/* #region 🏁 TODO 
  - [] Write a GET request that retrieves all of a user's moods, paginated? 
    `const moods = await collection.find({ user }).toArray();` // Get all moods.
  - [] Write a GET request for getting a mood feed that consists of multiple users.
       (Just use the latest for now?)
  + Later
  - [] Trigger a notification via FCM on a user status update.
  💡
  - Actually... should the statuses be recordable text?
  - And should these be stored as separate files in a user's
    bucket, with merely an association for the status?
  - Or what is a cool way to store the recording in a linear
    fashion?
  + Done
  - [x] Test GET request.
  - [x] Write GET request for retrieving the latest mood from individual users.
  - [x] Test POST request.
#endregion */

import {
  authorize,
  userIDFromHandleOrEmail,
  getHandleOrEmail,
} from "../../backend/authorization.mjs";
import { connect, moodFor, allMoods } from "../../backend/database.mjs";
import { respond, pathParams } from "../../backend/http.mjs";
// const dev = process.env.CONTEXT === "dev";

import { initializeApp, cert } from "firebase-admin/app"; // Firebase notifications.
import { getMessaging } from "firebase-admin/messaging";

import { filter } from "../../backend/filter.mjs"; // Profanity filtering.
// import { promises as fs } from "fs";
// import path from "path";

let app;

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    // 1. GET: Look up moods for user.
    const database = await connect();
    const slug = pathParams(event.path)[2];
    const handle = event.queryStringParameters?.for || null;

    if (slug === "all") {
      // List all moods from the database and return
      // them as { moods }.
      const moods = await allMoods(database, handle);
      await database.disconnect();
      return moods && moods.length > 0
        ? respond(200, { moods })
        : respond(500, { message: "No mood found." });
    } else {
      const user = await userIDFromHandleOrEmail(slug, database);
      if (user && user.length > 0) {
        const mood = await moodFor(user, database);
        await database.disconnect();
        return mood
          ? respond(200, mood)
          : respond(500, { message: "No mood found." });
      } else {
        return respond(401, { message: "User not found." });
      }
    }
  } else if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // 2. POST: Set / post a mood.
  try {
    const body = JSON.parse(event.body);
    const user = await authorize(event.headers);
    const handle = await getHandleOrEmail(user.sub);

    if (user && handle?.startsWith("@")) {
      const database = await connect();
      const collection = database.db.collection("moods"); // Make tweet-like collection.
      // Assume a mood update.
      if (body.mood) {
        const mood = filter(body.mood.trim()); // Trim extra whitespace off mood.

        // 📕 Database
        await collection.createIndex({ user: 1 }); // Index for `user`.
        await collection.createIndex({ when: 1 }); // Index for `when`.

        const lastMood = await collection.findOne(
          { user: user.sub },
          { sort: { when: -1 } },
        );

        if (!lastMood || lastMood.mood !== mood) {
          await collection.insertOne({
            user: user.sub,
            mood,
            when: new Date(),
          });

          const { got } = await import("got");

          const serviceAccount = (
            await got(process.env.GCM_FIREBASE_CONFIG_URL, {
              responseType: "json",
            })
          ).body;

          console.log(serviceAccount, typeof serviceAccount);

          app ||= initializeApp({ credential: cert(serviceAccount) }); // Send a notification.

          console.log("💕 Setting a mood for:", user);
          getMessaging()
            .send({
              notification: { title: `${handle}'s mood is`, body: `${mood}` },
              topic: "mood",
            })
            .then((response) => {
              console.log("☎️  Successfully sent notification:", response);
            })
            .catch((error) => {
              console.log("📵  Error sending notification:", error);
            });
        }

        await database.disconnect();
        return respond(200, { mood }); // Successful mood change.
      } else if (body.nuke !== undefined) {
        // Flag all a user's moods as deleted.
        const result = await collection.updateMany(
          { user: user.sub },
          { $set: { deleted: body.nuke } },
        );
        await database.disconnect();
        return respond(200, { altered: result.modifiedCount });
        // Reply with number of moods modified.
      }
    } else {
      return respond(401, { message: "Authorization failure..." });
    }
  } catch (error) {
    return respond(500, { message: error });
  }
}
