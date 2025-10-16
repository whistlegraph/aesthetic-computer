// User, 23.05.02.22.09
// A simple API for getting information about a user.

import {
  userIDFromHandle,
  userIDFromEmail,
  handleFor
} from "../../backend/authorization.mjs";

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const handleOrEmail = event.queryStringParameters.from;
  const userCode = event.queryStringParameters.code; // NEW: Support user code lookup
  const tenant = event.queryStringParameters.tenant || "aesthetic";
  
  if (!handleOrEmail && !userCode) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Malformed request. Provide 'from' or 'code' parameter." }),
    };
  }

  let user;
  
  if (userCode) {
    // NEW: Look up user by code (acXXXXX format)
    const { connect } = await import("../../backend/database.mjs");
    const db = await connect();
    const users = db.db.collection("users");
    const userDoc = await users.findOne({ code: userCode });
    await db.disconnect();
    
    if (userDoc) {
      user = userDoc._id; // The user ID (sub)
    }
  } else if (handleOrEmail.startsWith("@")) {
    // Try and look up `sub` from `handle` in MongoDB.
    user = await userIDFromHandle(
      handleOrEmail.slice(1),
      undefined,
      undefined,
      tenant,
    ); // Returns a string.
  } else {
    // Assume email and try to look up sub from email via auth0.
    shell.log("Getting user id from email:", handleOrEmail);
    user = await userIDFromEmail(handleOrEmail, tenant); // Returns an object.
  }

  if (user) {
    if (typeof user === "string") {
      const out = { sub: user };
      // Also pull in the user's handle here if it's requested.
      if (event.queryStringParameters.withHandle === "true") {
        const handle = await handleFor(user);
        if (handle) out.handle = handle;
      }
      return respond(200, out);
    } else {
      const out = {
        sub: user.userID,
        email_verified: user.email_verified,
      };
      // Pull in handle as in the above.
      if (event.queryStringParameters.withHandle === "true") {
        const handle = await handleFor(user.userID);
        if (handle) out.handle = handle;
      }
      return respond(200, out);
    }
  } else {
    return respond(400, { message: "User not found." });
  }
}
