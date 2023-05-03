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
    // Get an access token to the auth0 management API for the email lookup.

    // Then check to get the user ID via their email.
    const { got } = await import("got");
    const token = getAccessToken(got);
    const userResponse = await got(
      "https://aesthetic.us.auth0.com/api/v2/users-by-email",
      {
        searchParams: { email },
        headers: { Authorization: `Bearer ${token}` },
        responseType: "json",
      }
    );

    const user = userResponse.body[0];
    const userID = user?.user_id;
    return userID;
  } catch (error) {
    console.error(`Error retrieving user ID from Auth0: ${error}`);
    return undefined;
  }
}

// Takes in a user ID (sub) and returns the user's @handle (preferred) or email.
export async function getHandleOrEmail(sub) {
  try {
    // Attempt to get the user's handle.
    const handle = await handleFor(sub);
    if (handle) return "@" + handle;

    // If no handle is found, fetch the user's email from Auth0.
    const { got } = await import("got");
    const token = getAccessToken(got); // Get access token for auth0.
    const userResponse = await got(
      `https://aesthetic.us.auth0.com/api/v2/users/${encodeURIComponent(sub)}`,
      { headers: { Authorization: `Bearer ${token}` }, responseType: "json" }
    );

    return userResponse.body.email;
  } catch (error) {
    console.error(`Error retrieving user handle or email: ${error}`);
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
// Obtain an auth0 access token for our M2M API.
async function getAccessToken(got) {
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

  return tokenResponse.body.access_token;
}

// Connect to MongoDB
async function connect() {
  return await MongoClient.connect(mongoDBConnectionString, {
    useUnifiedTopology: true,
  });
}
