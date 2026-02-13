#!/usr/bin/env node
// Fix tezos-kidlisp network in production database (via Silo)

import "dotenv/config";
import { MongoClient } from "mongodb";

const connectionString = process.env.MONGODB_CONNECTION_STRING;
const dbName = process.env.MONGODB_NAME;

console.log("🔧 Fixing tezos-kidlisp network in production database...\n");

if (!connectionString) {
  console.error("❌ MONGODB_CONNECTION_STRING not found in .env");
  process.exit(1);
}

const client = new MongoClient(connectionString);

try {
  await client.connect();
  console.log("✅ Connected to MongoDB");

  const db = client.db(dbName);
  const secrets = db.collection("secrets");

  // Check current value
  const current = await secrets.findOne({ _id: "tezos-kidlisp" });

  if (!current) {
    console.log("❌ tezos-kidlisp secret not found!");
    process.exit(1);
  }

  console.log("\nCurrent configuration:");
  console.log(`  - address: ${current.address}`);
  console.log(`  - network: ${current.network}`);

  if (current.network === "mainnet") {
    console.log("\n✅ Already set to mainnet, no change needed!");
  } else {
    console.log("\n🔧 Updating network from 'ghostnet' to 'mainnet'...");

    const result = await secrets.updateOne(
      { _id: "tezos-kidlisp" },
      { $set: { network: "mainnet" } }
    );

    if (result.modifiedCount > 0) {
      console.log("✅ Successfully updated to mainnet!");

      // Verify
      const updated = await secrets.findOne({ _id: "tezos-kidlisp" });
      console.log("\nVerified:");
      console.log(`  - address: ${updated.address}`);
      console.log(`  - network: ${updated.network}`);

      console.log("\n🎉 Production database fixed! keep-mint should now work.");
    } else {
      console.log("⚠️  No changes made");
    }
  }

  await client.close();
  console.log("\n✅ Done!");
} catch (error) {
  console.error("\n❌ Error:", error.message);
  process.exit(1);
}
