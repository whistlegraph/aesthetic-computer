#!/usr/bin/env node

// Interactive Instagram login script.
// Performs the same login flow as instagram-cli (preLoginSync, challenge handling,
// session persistence) and saves the resulting session to MongoDB for use by
// the insta-api serverless function.
//
// Usage:
//   source ../aesthetic-computer-vault/.env  (or set MONGODB_CONNECTION_STRING)
//   node scripts/insta-login.mjs
//
// If INSTA_USER / INSTA_PASS are not set, it prompts interactively.

import { MongoClient } from "mongodb";
import {
  IgApiClient,
  IgCheckpointError,
  IgLoginTwoFactorRequiredError,
  IgLoginBadPasswordError,
} from "instagram-private-api";
import { createInterface } from "readline";

const rl = createInterface({ input: process.stdin, output: process.stdout });
const ask = (q) => new Promise((res) => rl.question(q, res));

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("MONGODB_CONNECTION_STRING not set.");
  console.error("Run: source aesthetic-computer-vault/.env");
  process.exit(1);
}

async function main() {
  // Get credentials
  let username = process.env.INSTA_USER;
  let password = process.env.INSTA_PASS;

  if (!username) username = await ask("Instagram username: ");
  if (!password) password = await ask("Instagram password: ");

  console.log(`\nLogging in as @${username}...`);

  const ig = new IgApiClient();
  ig.state.generateDevice(username);

  // Pre-login flow (instagram-cli pattern — reduces challenge risk)
  console.log("  Running preLoginSync...");
  try {
    await ig.launcher.preLoginSync();
    console.log("  preLoginSync OK");
  } catch (e) {
    console.warn("  preLoginSync failed (continuing):", e.message);
  }

  // Attempt login
  let loggedIn = false;
  try {
    await ig.account.login(username, password);
    loggedIn = true;
    console.log("  Login successful!");
  } catch (err) {
    if (err instanceof IgCheckpointError) {
      console.log("\n  Instagram checkpoint triggered.");
      console.log("  Requesting verification code...");

      try {
        await ig.challenge.auto(true);
        console.log("  Verification code sent (check email/SMS).");

        const code = await ask("\n  Enter verification code: ");
        await ig.challenge.sendSecurityCode(code.trim());
        loggedIn = true;
        console.log("  Challenge completed!");
      } catch (challengeErr) {
        console.error("  Challenge failed:", challengeErr.message);
        rl.close();
        process.exit(1);
      }
    } else if (err instanceof IgLoginTwoFactorRequiredError) {
      console.log("\n  Two-factor authentication required.");
      const twoFactorInfo = err.response.body.two_factor_info;
      const totp = twoFactorInfo.totp_two_factor_on;
      const method = totp ? "authenticator app" : "SMS";
      console.log(`  Method: ${method}`);

      const code = await ask(`\n  Enter 2FA code from ${method}: `);
      try {
        await ig.account.twoFactorLogin({
          username,
          verificationCode: code.trim(),
          twoFactorIdentifier: twoFactorInfo.two_factor_identifier,
          verificationMethod: totp ? "0" : "1",
        });
        loggedIn = true;
        console.log("  2FA login successful!");
      } catch (tfaErr) {
        console.error("  2FA failed:", tfaErr.message);
        rl.close();
        process.exit(1);
      }
    } else if (err instanceof IgLoginBadPasswordError) {
      console.error("  Bad password. Check credentials.");
      rl.close();
      process.exit(1);
    } else {
      console.error("  Login error:", err.message);
      rl.close();
      process.exit(1);
    }
  }

  if (!loggedIn) {
    console.error("Login failed.");
    rl.close();
    process.exit(1);
  }

  // Post-login flow (instagram-cli pattern)
  console.log("  Running postLoginFlow...");
  try {
    await ig.feed.reelsTray("cold_start").request();
    await ig.feed.timeline("cold_start_fetch").request();
    console.log("  postLoginFlow OK");
  } catch (e) {
    console.warn("  postLoginFlow failed (continuing):", e.message);
  }

  // Verify login
  const currentUser = await ig.account.currentUser();
  console.log(`\n  Logged in as: @${currentUser.username} (pk: ${currentUser.pk})`);

  // Serialize and save session to MongoDB
  console.log("\n  Saving session to MongoDB...");
  const serialized = await ig.state.serialize();
  delete serialized.constants;
  delete serialized.supportedCapabilities;

  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  try {
    await client.connect();
    const db = client.db(MONGODB_NAME);

    await db.collection("insta-sessions").updateOne(
      { _id: username },
      {
        $set: {
          _id: username,
          state: serialized,
          updatedAt: new Date(),
          loginMethod: "insta-login.mjs",
          verifiedUser: currentUser.username,
        },
      },
      { upsert: true },
    );
    console.log("  Session saved to MongoDB (insta-sessions collection)");

    // Verify by reading it back
    const doc = await db
      .collection("insta-sessions")
      .findOne({ _id: username });
    console.log(`  Session document size: ~${JSON.stringify(doc.state).length} bytes`);
    console.log(`  Updated at: ${doc.updatedAt}`);
  } finally {
    await client.close();
  }

  console.log("\n  Done! The insta-api function will now reuse this session.");
  console.log("  Try: /api/insta?action=profile&username=whistlegraph\n");

  rl.close();
}

main().catch((e) => {
  console.error("Fatal:", e.message);
  rl.close();
  process.exit(1);
});
