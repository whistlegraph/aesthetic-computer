// auth0-events, 24.09.05.23.50
// Receive user events from auth0 in order to trigger actions on aesthetic.
// Configured at: https://manage.auth0.com/dashboard/us/aesthetic/log-streams/new-stream/settings
// Event information: https://auth0.com/docs/customize/log-streams/event-filters

import { respond } from "../../backend/http.mjs";
import { shell } from "../../backend/shell.mjs";
import { handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import * as logger from "../../backend/logger.mjs";
import { generateUniqueUserCode, ensureUserCodeIndex } from "../../public/aesthetic.computer/lib/user-code.mjs";
import { createAtprotoAccount } from "../../backend/at.mjs";

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

  for (const log of body.logs) {
    shell.log("🧏 Auth0 Event Type:", log.data.type, "User:", log.data.user_id);

    // 🖋️ Signed up
    if (log.data.type === "ss") {
      const aestheticSub = log.data.user_id;
      const email = log.data.details.body.email;
      shell.log("🖋️ Signed up:", aestheticSub, "Email:", email);

      // Insert into "verifieds" collection, with verification count 0
      const verifications = database.db.collection("verifications");
      await verifications.insertOne({ _id: aestheticSub, count: 0 });

      // 🎫 Generate unique user code
      try {
        await ensureUserCodeIndex(database);
        const signupDate = new Date(log.data.date); // Use Auth0 event timestamp
        const code = await generateUniqueUserCode(database, signupDate);
        
        const users = database.db.collection("users");
        await users.insertOne({ 
          _id: aestheticSub, 
          code,
          when: signupDate
        });
        
        shell.log("🎫 Generated user code:", code, "for:", aestheticSub);
      } catch (error) {
        shell.log("⚠️ Failed to generate user code:", aestheticSub, error);
        // Don't block signup on code generation failure
      }
    }

    // 💌 Email verified
    if (log.data.type === "sv") {
      const aestheticSub = log.data.user_id;
      shell.log("💌 Email verified:", aestheticSub);

      const verifications = database.db.collection("verifications");
      const verified = await verifications.findOne({ _id: aestheticSub });

      if (verified) {
        const count = verified.count + 1;
        await verifications.updateOne(
          { _id: aestheticSub },
          { $set: { count } },
        );

        // Detect if this is the first verification.

        // ⚠️ Some users in the system will not have this record because
        // they were grandfathered in, in which case it could be synthesized
        // here if it never existed if it's ever necessary for future logic.
        // 24.09.06.03.04
        // 💡 This could be done by going through every verified user and
        //    setting verifications on that user to a count of 1 with a mongo task. 24.10.11.01.21
        if (count === 1) {
          const handle = await handleFor(aestheticSub);
          if (handle) {
            shell.log("🌠 Inherited handle:", handle);
            // 🪵 Add the logger stuff here eventually... 24.09.06.02.22
            await logger.link(database);
            await logger.log(`@${handle} is with us`, {
              user: aestheticSub,
              action: "handle:inherit",
              value: handle,
            }); // 🪵 Log first cross-over handle creation.
          }

          // 🦋 Create ATProto account on first verification
          shell.log("🦋 Creating ATProto account for newly verified user...");
          const atprotoResult = await createAtprotoAccount(
            database,
            aestheticSub,
          );
          if (atprotoResult.created) {
            shell.log(
              `✅ ATProto account created: ${atprotoResult.did} @${atprotoResult.handle}`,
            );
          } else {
            shell.log(
              `⚠️  ATProto account creation: ${atprotoResult.reason || "failed"}`,
            );
          }
        }
      } else {
        shell.log("🚫 No `verifications` record for:", aestheticSub);
      }
    }
  }

  await database.disconnect();

  return respond(200, { message: "Log received." });
}
