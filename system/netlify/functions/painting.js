// Painting 23.10.01.15.29
// POST: Create a new record in the database for a user uploaded painting.

/* #region 🏁 TODO 
  - [] Eventually add metadata to paintings... like titles.
#endregion */

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  if (event.httpMethod !== "POST")
    return respond(405, { message: "Method Not Allowed" });

  // A POST request to create the painting.
  // Parse the body of the HTTP request
  let body;
  try {
    // Make sure we have a username present to set.
    body = JSON.parse(event.body);

    const slug = body.slug;

    console.log("SLUG", slug, body);

    // And that we are logged in...
    const user = await authorize(event.headers);
    if (user) {
      // 🔑 We are logged in!
      const database = await connect(); // 📕 Database
      const collection = database.db.collection("paintings");
      await collection.createIndex({ user: 1 });
      await collection.createIndex({ when: 1 });
      await collection.createIndex({ slug: 1 });
      await collection.createIndex({ slug: 1, user: 1 }, { unique: true });

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        // Check if a document with this user's sub already exists
        // Add a new `@handles` document for this user.
        await collection.insertOne({ slug, user: user.sub, when: new Date() });
      } catch (error) {
        return respond(500, { message: error });
      } finally {
        await database.disconnect();
      }
      // Successful handle change...
      return respond(200, { slug });
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
