// Authorization, 23.04.30.17.47

// Authenticates a user to make sure they are logged in
// and their local keys match the user database.
// 🧠 (So that they can run authorized server functions.)

export async function authorize({ authorization }) {
  try {
    const { got } = await import("got");
    return (
      await got("https://auth0.aesthetic.computer/userinfo", {
        headers: {
          Authorization: authorization,
        },
        responseType: "json",
      })
    ).body;
  } catch {
    return undefined;
  }
}

// Connect to Auth0 and return the user ID (`sub`) for a given email address.
export async function userIDFromEmail(email) {
  try {
    const { got } = await import("got");

    // First get an access token to the auth0 management API for the email lookup.
    const tokenResponse = await got(
      `https://aesthetic.us.auth0.com/oauth/token`,
      {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        json: {
          client_id: process.env.AUTH0_M2M_CLIENT_ID,
          client_secret: process.env.AUTH0_M2M_SECRET,
          audience: "https://aesthetic.us.auth0.com/api/v2/",
          grant_type: "client_credentials", // Use "client_credentials" for M2M
        },
        responseType: "json",
      }
    );
    const accessToken = tokenResponse.body.access_token;

    // Then check to get the user ID via their email.
    const idResponse = await got(
      "https://aesthetic.us.auth0.com/api/v2/users-by-email",
      {
        searchParams: { email },
        headers: {
          Authorization: `Bearer ${accessToken}`,
        },
        responseType: "json",
      }
    );

    const user = idResponse.body[0];
    const userID = user?.user_id;
    return userID;
  } catch (error) {
    console.error(`Error retrieving user ID from Auth0: ${error}`);
    return undefined;
  }
}

import { MongoClient } from "mongodb";
const mongoDBConnectionString = process.env.MONGODB_CONNECTION_STRING;
const mongoDBName = process.env.MONGODB_NAME;

// Connects to the MongoDB database to obtain a user's handle from their ID.
export async function handleFor(id) {
  const client = await connect();
  const db = client.db(mongoDBName);
  const collection = db.collection("@handles");
  const existingUser = await collection.findOne({ _id: id });
  await client.close();
  return existingUser?.handle;
}

// Connects to the MongoDB database to obtain a user ID from a handle.
export async function userIDFromHandle(handle) {
  const client = await connect();
  const db = client.db(mongoDBName);
  const collection = db.collection("@handles");
  const user = await collection.findOne({ handle });
  const userID = user?._id;
  await client.close();
  return userID;
}

// 📚 Library (Useful functions used throughout the file.)
async function connect() {
  return await MongoClient.connect(mongoDBConnectionString, {
    useUnifiedTopology: true,
  });
}
