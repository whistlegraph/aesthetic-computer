// @handle, 23.04.30.18.12 🤚
// GET: Get a user @handle out of MongoDB based on their `sub` id from auth0.
// POST: Allows a logged in user to set their social `@handle`. (via MongoDB)

// Future: Abstract MongoDB into an included header for other
//         api calls.

import { authorize } from "../../backend/authorization.mjs";
import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { MongoClient } from "mongodb";

const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
const mongoDBName = process.env.MONGODB_NAME;
const dev = process.env.CONTEXT === "dev";

export async function handler(event, context) {
  // A GET request to get a handle from a user `sub`.
  if (event.httpMethod === "GET") {
    const id = event.queryStringParameters.for;
    // Check the database for the entry.
    const client = await MongoClient.connect(mongoDBConnectionString, {
      useUnifiedTopology: true,
    });
    const db = client.db(mongoDBName);
    const collection = db.collection("@handles");
    const existingUser = await collection.findOne({ _id: id });
    await client.close(); // Close the connection.

    if (existingUser) {
      return {
        statusCode: 200,
        body: JSON.stringify({
          handle: existingUser.handle,
        }),
      };
    } else {
      return {
        statusCode: 400,
        body: JSON.stringify({ message: "No handle found." }),
      };
    }
  } else if (event.httpMethod !== "POST")
    return { statusCode: 405, body: "Method Not Allowed" };
  // A POST request to set the handle.

  // Parse the body of the HTTP request
  let body;
  try {
    // Make sure we have a username present to set.
    debugger;

    body = JSON.parse(event.body);

    const handle = body.handle;

    // Make sure handle entry is well formed.
    if (!validateHandle(handle)) {
      return {
        statusCode: 400,
        body: JSON.stringify({ message: "Bad handle formatting." }),
      };
    }

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
          if (dev)
            console.log("User handle is currently:", existingUser.handle);
          // Fail if the new handle is already taken by someone else.
          await collection.updateOne(
            { _id: user.sub },
            { $set: { handle } }
          );
        } else {
          // Add a new `@handles` document for this user.
          await collection.insertOne({ _id: user.sub, handle });
        }
      } catch (error) {
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
        body: JSON.stringify({ handle: body.handle }),
      };
    } else {
      return {
        statusCode: 401,
        body: "Authorization failure...",
      };
    }
  } catch (error) {
    return {
      statusCode: 400,
      body: JSON.stringify({ message: "Cannot parse body." }),
    };
  }
}
