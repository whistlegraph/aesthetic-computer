// @handle, 23.04.30.18.12
// Allows a logged in user to set their social `@handle`. (via M"ongoDB)

// TODO
// - [-] Import `mongodb` and create the index / persist the @user.

import { authorize } from "../../backend/authorization.mjs";
import { MongoClient } from "mongodb";

const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
const mongoDBName = process.env.MONGODB_NAME;

export async function handler(event, context) {
  if (event.httpMethod !== "POST")
    return { statusCode: 405, body: "Method Not Allowed" };

  // Parse the body of the HTTP request
  let body;
  try {
    // Make sure we have a username present to set.
    debugger;

    body = JSON.parse(event.body);

    // And that we are logged in...
    const user = await authorize(event.headers); // We are logged in!
    if (user) {
      // 📕 Database
      // Connect to mongo & add the uniqueness index for `@handles`.
      const client = await MongoClient.connect(mongoDBConnectionString, {
        useUnifiedTopology: true,
      });
      const db = client.db(mongoDBName);
      const collection = db.collection("@handles");
      await collection.createIndex({ handle: 1 }, { unique: true });

      // Insert or update the handle using the `provider|id` key from auth0.
      try {
        // Check if a document with this user's sub already exists
        const existingUser = await collection.findOne({ _id: user.sub });
        if (existingUser) {
          // Note: This could fail if the new handle is already taken by someone else
          await collection.updateOne(
            { _id: user.sub },
            { $set: { handle: body.handle } }
          );
        } else {
          // Add a new `@handles` document for this user.
          await collection.insertOne({ _id: user.sub, handle: body.handle });
        }
      } catch (error) {
        console.log(error);
        return {
          statusCode: 400,
          body: JSON.stringify({ message: error }),
        };
      } finally {
        await client.close(); // Close the connection.
      }

      // Successful result...

      return {
        statusCode: 200,
        body: JSON.stringify({
          message: `Handle changed from ${user.handle} to:`,
          to: body.handle,
        }),
      };
    } else {
      return {
        statusCode: 401,
        body: "Authorization failure...",
      };
    }
  } catch (error) {
    console.log("Error parsing body", error);
    return {
      statusCode: 400,
      body: JSON.stringify({ message: "Cannot parse body" }),
    };
  }
}
