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
} from "../../backend/authorization.mjs";
import { connect, moodFor } from "../../backend/database.mjs";
import { respond, pathParams } from "../../backend/http.mjs";
// const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    // 1. GET: Look up moods for user.
    const database = await connect();
    const user = await userIDFromHandleOrEmail(
      pathParams(event.path)[2],
      database
    );
    if (user) {
      const mood = await moodFor(user, database);
      await database.disconnect();
      return mood
        ? respond(200, mood)
        : respond(500, { message: "No mood found." });
    } else {
      return respond(401, { message: "User not found." });
    }
  } else if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // 2. POST: Set / post a mood.
  try {
    const body = JSON.parse(event.body);
    const mood = body.mood;
    const user = await authorize(event.headers);
    if (user) {
      // 📕 Database
      const database = await connect();
      const collection = database.db.collection("moods"); // Make tweet-like collection.
      await collection.createIndex({ user: 1 }); // Index for `user`.
      await collection.insertOne({ user: user.sub, mood, when: new Date() });
      await database.disconnect();
      return respond(200, { mood: body.mood }); // Successful mood change.
    } else {
      return respond(401, { message: "Authorization failure..." });
    }
  } catch (error) {
    return respond(500, { message: error });
  }
}
