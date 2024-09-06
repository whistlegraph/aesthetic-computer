// auth0-events, 24.09.05.23.50
// Receive user events from auth0 in order to trigger actions on aesthetic.
// Configured at: https://manage.auth0.com/dashboard/us/aesthetic/log-streams/new-stream/settings

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";

const AUTH0_LOG_TOKEN = process.env.AUTH0_LOG_TOKEN;

export async function handler(event, context) {
  if (event.httpMethod !== "POST")
    return respond(405, { error: "Wrong request type." });

  // 🚧 Check that the auth bearer token matches AUTH0_LOG_TOKEN
  const authHeader = event.headers.Authorization || event.headers.authorization;
  if (authHeader !== AUTH0_LOG_TOKEN) {
    return respond(403, { error: "Invalid authorization token." });
  }

  const body = JSON.parse(event.body);

  shell.log(event.headers); // TODO: 🟡 Make sure headers are proper.
  shell.log(body);

  // Example: Checking if the user's email was verified
  // 🔵 TODO: Check to see if we can send a logger event to the ac chat for when
  //          sotce-net users first join aesthetic computer and also have a handle set.

  return respond(200, { message: "Log received." });
}