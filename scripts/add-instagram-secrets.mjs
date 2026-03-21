#!/usr/bin/env node

// Add Instagram credentials to MongoDB secrets collection
// Used by the insta-api serverless function to browse public profiles.
// Run: INSTA_USER=username INSTA_PASS=password node scripts/add-instagram-secrets.mjs

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("MONGODB_CONNECTION_STRING environment variable is required");
  console.error("   Load from vault first: source ../aesthetic-computer-vault/.env");
  process.exit(1);
}

const INSTA_USER = process.env.INSTA_USER;
const INSTA_PASS = process.env.INSTA_PASS;

if (!INSTA_USER || !INSTA_PASS) {
  console.error("INSTA_USER and INSTA_PASS environment variables are required");
  process.exit(1);
}

async function main() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);

  try {
    await client.connect();
    console.log("Connected to MongoDB");

    const db = client.db(MONGODB_NAME);
    const secrets = db.collection("secrets");

    const result = await secrets.updateOne(
      { _id: "instagram" },
      {
        $set: {
          _id: "instagram",
          username: INSTA_USER,
          password: INSTA_PASS,
          purpose: "instagram-private-api login for insta piece",
          updatedAt: new Date(),
        },
      },
      { upsert: true },
    );

    if (result.upsertedCount > 0) {
      console.log("Instagram secret created");
    } else if (result.modifiedCount > 0) {
      console.log("Instagram secret updated");
    } else {
      console.log("Instagram secret unchanged");
    }

    const doc = await secrets.findOne({ _id: "instagram" });
    console.log(`\n  _id: ${doc._id}`);
    console.log(`  username: ${doc.username}`);
    console.log(`  purpose: ${doc.purpose}`);
    console.log(`  updatedAt: ${doc.updatedAt}`);
  } catch (error) {
    console.error("Error:", error.message);
    process.exit(1);
  } finally {
    await client.close();
  }
}

main();
