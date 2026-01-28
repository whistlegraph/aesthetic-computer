// User, 23.05.02.22.09
// A simple API for getting information about a user.
// 🚀 Redis caching added 2026.01.28 to speed up boot time

import {
  userIDFromHandle,
  userIDFromEmail,
  handleFor
} from "../../backend/authorization.mjs";

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import * as KeyValue from "../../backend/kv.mjs";

// Cache TTL in seconds (5 minutes - user data rarely changes)
const CACHE_TTL_SECONDS = 300;

// GET A user's `sub` id from either their handle or email address.
export async function handler(event, context) {
  // Make sure this is a GET request
  if (event.httpMethod !== "GET") {
    return respond(405, { error: "Wrong request type." });
  }

  const handleOrEmail = event.queryStringParameters.from;
  const userCode = event.queryStringParameters.code; // NEW: Support user code lookup
  const tenant = event.queryStringParameters.tenant || "aesthetic";
  const withHandle = event.queryStringParameters.withHandle === "true";
  const noCache = event.queryStringParameters.nocache === "true";
  
  if (!handleOrEmail && !userCode) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Malformed request. Provide 'from' or 'code' parameter." }),
    };
  }

  // 🚀 Check Redis cache first (skip for code lookups which are rare)
  const cacheKey = userCode 
    ? `code:${userCode}:${tenant}:${withHandle}`
    : `email:${handleOrEmail}:${tenant}:${withHandle}`;
  
  if (!noCache) {
    try {
      await KeyValue.connect();
      const cached = await KeyValue.get("userCache", cacheKey);
      if (cached) {
        shell.log(`🚀 Cache HIT for user: ${cacheKey}`);
        return respond(200, JSON.parse(cached));
      }
      shell.log(`⏳ Cache MISS for user: ${cacheKey}`);
    } catch (err) {
      shell.log(`⚠️ Redis cache error (continuing without cache): ${err.message}`);
    }
  }

  let user;
  
  if (userCode) {
    // NEW: Look up user by code (acXXXXX format)
    const { connect } = await import("../../backend/database.mjs");
    const db = await connect();
    const users = db.db.collection("users");
    const userDoc = await users.findOne({ code: userCode });
    // Don't disconnect - let the connection be reused across serverless invocations
    // await db.disconnect();
    
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
    let out;
    if (typeof user === "string") {
      out = { sub: user };
      // Also pull in the user's handle here if it's requested.
      if (withHandle) {
        const handle = await handleFor(user);
        if (handle) out.handle = handle;
      }
    } else {
      out = {
        sub: user.userID,
        email_verified: user.email_verified,
      };
      // Pull in handle as in the above.
      if (withHandle) {
        const handle = await handleFor(user.userID);
        if (handle) out.handle = handle;
      }
    }
    
    // 🚀 Cache the result in Redis
    try {
      await KeyValue.connect();
      await KeyValue.set("userCache", cacheKey, JSON.stringify(out));
      shell.log(`💾 Cached user result for: ${cacheKey}`);
    } catch (err) {
      shell.log(`⚠️ Failed to cache user result: ${err.message}`);
    }
    
    return respond(200, out);
  } else {
    return respond(400, { message: "User not found." });
  }
}
