// auth0-events, 24.09.05.23.50
// Receive user events from auth0 in order to trigger actions on aesthetic.
// Configured at: https://manage.auth0.com/dashboard/us/aesthetic/log-streams/new-stream/settings
// Event information: https://auth0.com/docs/customize/log-streams/event-filters

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";

// import * as logger from "../../backend/logger.mjs";

const AUTH0_LOG_TOKEN = process.env.AUTH0_LOG_TOKEN;

export async function handler(event, context) {
  if (event.httpMethod !== "POST")
    return respond(405, { error: "Wrong request type." });

  // 🚧 Check that the auth bearer token matches AUTH0_LOG_TOKEN
  if (event.headers.authorization !== AUTH0_LOG_TOKEN) {
    return respond(403, { error: "Invalid authorization token." });
  }

  const body = JSON.parse(event.body);
  const tenant = "aesthetic"; // TODO: Eventually add `sotce-net` support.

  body.logs.forEach((log) => {
    // 📧 Successful signup.
    shell.log("🧏 Auth0:", "Type:", log.data.type, "User:", log.data.user_id);
    if (log.data.type === "ss") {
      const aestheticSub = log.data.user_id;

      shell.log(log.data);
      // Check to see if this user has a handle
    }
  });

  return respond(200, { message: "Log received." });
}
