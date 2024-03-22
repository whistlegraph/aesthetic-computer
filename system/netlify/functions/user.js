// User, 23.05.02.22.09
// A simple API for getting information about a user.

import {
  userIDFromHandle,
  userIDFromEmail,
} from "../../backend/authorization.mjs";

import { respond } from "../../backend/http.mjs";

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const handleOrEmail = event.queryStringParameters.from;
  if (!handleOrEmail) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Malformed request." }),
    };
  }

  // 1. Download / go to individual files.
  let sub;
  if (handleOrEmail.startsWith("@")) {
    // Try and look up `sub` from `handle` in MongoDB.
    sub = await userIDFromHandle(handleOrEmail.slice(1));
  } else {
    // Assume email and try to look up sub from email via auth0.
    sub = await userIDFromEmail(handleOrEmail);
  }

  if (sub) {
    return respond(200, { sub });
  } else {
    return respond(400, { message: "User not found." });
  }
}
