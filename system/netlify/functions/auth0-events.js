// auth0-events, 24.09.05.23.50
// Receive user events from auth0 in order to trigger actions on aesthetic.
// Configured at: https://manage.auth0.com/dashboard/us/aesthetic/log-streams/new-stream/settings
// Event information: https://auth0.com/docs/customize/log-streams/event-filters

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import { handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
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

  const database = await connect(); // 📕 Database

  // Ensure that sub is unique in the "verifieds" collection

  body.logs.forEach(async (log) => {
    shell.log("🧏 Auth0 Event Type:", log.data.type, "User:", log.data.user_id);

    // 🖋️ Signed up
    if (log.data.type === "ss") {
      const aestheticSub = log.data.user_id;
      const email = log.data.details.body.email;
      shell.log("🖋️ Signed up:", aestheticSub, "Email:", email);

      // Insert into "verifieds" collection, with verification count 0
      const verifieds = database.db.collection("verifieds");
      await verifieds.insertOne({ _id: aestheticSub, verifications: 0 });
    }

    // 💌 Email verified
    if (log.data.type === "sv") {
      const aestheticSub = log.data.user_id;
      const email = log.data.email;
      shell.log("💌 Email verified:", aestheticSub, "Email:", email);

      const verifieds = database.db.collection("verifieds");
      const verified = await verifieds.findOne({ sub: aestheticSub });

      if (verified) {
        const verifications = verified.verifications + 1;
        await verifieds.updateOne(
          { _id: aestheticSub },
          { $set: { verifications } },
        );

        // Detect if this is the first verification
        if (verifications === 1) {
          const handle = await handleFor(aestheticSub);
          if (handle) {
            shell.log("🌠 Inherited handle:", handle);
            // 🪵 Add the logger stuff here eventually... 24.09.06.02.22
          }
        }
      } else {
        shell.log("🚫 No `verifications` record for:", aestheticSub);
      }
    }
  });

  await database.disconnect();

  return respond(200, { message: "Log received." });
}
