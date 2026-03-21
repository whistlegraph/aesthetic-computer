#!/usr/bin/env node

// Add Facebook/Instagram and Threads credentials to MongoDB secrets collection
// Run: node scripts/add-facebook-secrets.mjs

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("❌ MONGODB_CONNECTION_STRING environment variable is required");
  console.error("   Load from vault first: source ../aesthetic-computer-vault/.env");
  process.exit(1);
}

// Facebook/Instagram credentials from vault
const FACEBOOK_APP_ID = process.env.FACEBOOK_APP_ID;
const FACEBOOK_APP_SECRET = process.env.FACEBOOK_APP_SECRET;
const THREADS_APP_ID = process.env.THREADS_APP_ID;
const THREADS_APP_SECRET = process.env.THREADS_APP_SECRET;

if (!FACEBOOK_APP_ID || !FACEBOOK_APP_SECRET) {
  console.error("❌ FACEBOOK_APP_ID and FACEBOOK_APP_SECRET required");
  console.error("   Load from vault first: source ../aesthetic-computer-vault/.env");
  process.exit(1);
}

async function main() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    console.log("✅ Connected to MongoDB");

    const db = client.db(MONGODB_NAME);
    const secrets = db.collection("secrets");

    // Upsert Facebook/Instagram credentials
    const facebookResult = await secrets.updateOne(
      { _id: "facebook" },
      {
        $set: {
          _id: "facebook",
          appId: FACEBOOK_APP_ID,
          appSecret: FACEBOOK_APP_SECRET,
          displayName: "Aesthetic.Computer",
          purpose: "Instagram oEmbed and Graph API access",
          updatedAt: new Date(),
        },
      },
      { upsert: true }
    );

    if (facebookResult.upsertedCount > 0) {
      console.log("✅ Facebook/Instagram secret created");
    } else if (facebookResult.modifiedCount > 0) {
      console.log("✅ Facebook/Instagram secret updated");
    } else {
      console.log("ℹ️  Facebook/Instagram secret unchanged");
    }

    // Upsert Threads credentials (if available)
    if (THREADS_APP_ID && THREADS_APP_SECRET) {
      const threadsResult = await secrets.updateOne(
        { _id: "threads" },
        {
          $set: {
            _id: "threads",
            appId: THREADS_APP_ID,
            appSecret: THREADS_APP_SECRET,
            displayName: "Aesthetic.Computer",
            purpose: "Threads API access",
            updatedAt: new Date(),
          },
        },
        { upsert: true }
      );

      if (threadsResult.upsertedCount > 0) {
        console.log("✅ Threads secret created");
      } else if (threadsResult.modifiedCount > 0) {
        console.log("✅ Threads secret updated");
      } else {
        console.log("ℹ️  Threads secret unchanged");
      }
    }

    // Verify
    const facebookDoc = await secrets.findOne({ _id: "facebook" });
    console.log("\n📋 Stored Facebook/Instagram document:");
    console.log(`   _id: ${facebookDoc._id}`);
    console.log(`   appId: ${facebookDoc.appId.slice(0, 10)}...`);
    console.log(`   displayName: ${facebookDoc.displayName}`);
    console.log(`   purpose: ${facebookDoc.purpose}`);
    console.log(`   updatedAt: ${facebookDoc.updatedAt}`);

    if (THREADS_APP_ID) {
      const threadsDoc = await secrets.findOne({ _id: "threads" });
      console.log("\n📋 Stored Threads document:");
      console.log(`   _id: ${threadsDoc._id}`);
      console.log(`   appId: ${threadsDoc.appId.slice(0, 10)}...`);
      console.log(`   displayName: ${threadsDoc.displayName}`);
      console.log(`   purpose: ${threadsDoc.purpose}`);
      console.log(`   updatedAt: ${threadsDoc.updatedAt}`);
    }

  } catch (error) {
    console.error("❌ Error:", error.message);
    process.exit(1);
  } finally {
    await client.close();
  }
}

main();
