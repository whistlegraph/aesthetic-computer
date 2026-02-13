#!/usr/bin/env node
// Fix the tezos-kidlisp network from ghostnet to mainnet

import { connect } from './system/backend/database.mjs';

console.log("🔧 Fixing tezos-kidlisp network configuration...\n");

try {
  const database = await connect();
  const secrets = database.db.collection("secrets");

  // Check current value
  const current = await secrets.findOne({ _id: "tezos-kidlisp" });
  console.log("Current configuration:");
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
    } else {
      console.log("⚠️  No changes made");
    }
  }

  await database.disconnect();
  console.log("\n✅ Done!");
} catch (error) {
  console.error("\n❌ Error:", error.message);
  process.exit(1);
}
