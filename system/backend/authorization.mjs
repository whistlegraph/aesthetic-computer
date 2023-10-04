// Authorization, 23.04.30.17.47

// Authenticates a user to make sure they are logged in
// and their local keys match the user database.
// 🧠 (And so they can run authorized server functions.)

import { connect } from "./database.mjs";
import * as KeyValue from "./kv.mjs";
const dev = process.env.CONTEXT === "dev";

export async function authorize({ authorization }) {
  try {
    const { got } = await import("got");
    return (
      await got("https://aesthetic.us.auth0.com/userinfo", {
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

export async function userIDFromEmail(email) {
  try {
    // Get an access token to the auth0 management API for the email lookup.

    // Then check to get the user ID via their email.
    const { got } = await import("got");
    const token = await getAccessToken(got);

    const userResponse = await got(
      "https://aesthetic.us.auth0.com/api/v2/users-by-email",
      {
        searchParams: { email },
        headers: { Authorization: `Bearer ${token}` },
        responseType: "json",
      },
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
    const token = await getAccessToken(got); // Get access token for auth0.
    const userResponse = await got(
      `https://aesthetic.us.auth0.com/api/v2/users/${encodeURIComponent(sub)}`,
      { headers: { Authorization: `Bearer ${token}` }, responseType: "json" },
    );

    return userResponse.body.email;
  } catch (error) {
    console.error(`Error retrieving user handle or email: ${error}`);
    return undefined;
  }
}

// Connects to the MongoDB database to obtain a user's handle from their ID.
// (With redis cache)
export async function handleFor(id) {
  // const time = performance.now();

  await KeyValue.connect();
  let cachedID = await KeyValue.get("userIDs", id);

  if (cachedID) {
    // console.log("ID: Found in redis...");
    await KeyValue.disconnect();
    return cachedID;
  }

  const database = await connect();
  const collection = database.db.collection("@handles");

  if (id === "all") {
    const randomHandles = await collection
      .aggregate([{ $sample: { size: 100 } }, { $project: { handle: 1 } }])
      .toArray();

    await database.disconnect();
    return randomHandles.map((doc) => "@" + doc.handle);
  } else {
    const existingUser = await collection.findOne({ _id: id });

    // Cache the handle in redis...
    if (existingUser?.handle) {
      // console.log("Caching in redis...", id);
      await KeyValue.set("userIDs", id, existingUser.handle);
    }

    await database.disconnect();
    await KeyValue.disconnect();

    // console.log("Time taken...", performance.now() - time);
    return existingUser?.handle;
  }
}

// Connects to the MongoDB database to obtain a user ID from a handle.
// Handle should not be prefixed with "@".
export async function userIDFromHandle(handle, database) {
  // TODO: Read from redis, otherwise check the database, and store in
  //       redis afterwards...
  let userID;

  const time = performance.now();
  await KeyValue.connect();
  const cachedHandle = await KeyValue.get("@handles", handle);

  if (!cachedHandle) {
    // Look in database.
    // if (dev) console.log("Handle: Looking in database...");
    const keepOpen = database; // Keep the db connection if database is defined.
    // if (dev) console.log("Handle: Connecting...", time);
    if (!database) database = await connect();
    const collection = database.db.collection("@handles");
    const user = await collection.findOne({ handle });
    userID = user?._id;
    if (!keepOpen) database.disconnect();
  } else {
    // if (dev) console.log("Handle: Found in redis...");
    userID = cachedHandle;
  }

  // Cache userID in redis...
  if (!cachedHandle && userID) {
    if (dev) console.log("Caching in redis...", handle);
    await KeyValue.set("@handles", handle, userID);
    await KeyValue.disconnect();
  }

  console.log("Time taken...", performance.now() - time);
  return userID;
}

// Assume prefixed handle.
export async function userIDFromHandleOrEmail(handleOrEmail, database) {
  if (!handleOrEmail) return;
  if (handleOrEmail.startsWith("@")) {
    return userIDFromHandle(handleOrEmail.slice(1), database);
  } else {
    return userIDFromEmail(handleOrEmail); // Assume email.
  }
}

// Sets the user's email and triggers a re-verification email.
export async function setEmailAndReverify(id, email) {
  try {
    const { got } = await import("got");
    const token = await getAccessToken(got);

    // 1. Update the user's email
    const updateEmailResponse = await got(
      `https://aesthetic.us.auth0.com/api/v2/users/${encodeURIComponent(id)}`,
      {
        method: "PATCH",
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        },
        json: { email, email_verified: false },
        responseType: "json",
      },
    );

    if (!updateEmailResponse.body) {
      throw new Error("Failed to update user email");
    }

    // 2. Trigger the verification email
    const verificationResponse = await got(
      `https://aesthetic.us.auth0.com/api/v2/jobs/verification-email`,
      {
        method: "POST",
        headers: {
          Authorization: `Bearer ${token}`,
          "Content-Type": "application/json",
        },
        json: { user_id: id },
        responseType: "json",
      },
    );

    if (!verificationResponse.body) {
      throw new Error("Failed to send verification email");
    }

    return {
      success: true,
      message: "Email updated and verification email sent successfully!",
    };
  } catch (error) {
    console.error(`Error setting email and sending verification: ${error}`);
    return {
      success: false,
      message: error.message,
    };
  }
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
    },
  );

  return tokenResponse.body.access_token;
}
