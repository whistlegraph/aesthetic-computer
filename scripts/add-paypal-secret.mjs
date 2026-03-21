#!/usr/bin/env node

// Add PayPal credentials to MongoDB secrets collection
// Run: node scripts/add-paypal-secret.mjs

import { MongoClient } from "mongodb";

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("❌ MONGODB_CONNECTION_STRING environment variable is required");
  console.error("   Load from vault first: source ../aesthetic-computer-vault/.env");
  process.exit(1);
}

// PayPal credentials from vault
const PAYPAL_CLIENT_ID = process.env.PAYPAL_CLIENT_ID;
const PAYPAL_CLIENT_SECRET = process.env.PAYPAL_CLIENT_SECRET;

if (!PAYPAL_CLIENT_ID || !PAYPAL_CLIENT_SECRET) {
  console.error("❌ PAYPAL_CLIENT_ID and PAYPAL_CLIENT_SECRET required");
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
    
    // Upsert PayPal credentials
    const result = await secrets.updateOne(
      { _id: "paypal" },
      {
        $set: {
          _id: "paypal",
          clientId: PAYPAL_CLIENT_ID,
          clientSecret: PAYPAL_CLIENT_SECRET,
          account: "mail@aesthetic.computer",
          apiUrl: "https://api-m.paypal.com",
          updatedAt: new Date(),
        },
      },
      { upsert: true }
    );
    
    if (result.upsertedCount > 0) {
      console.log("✅ PayPal secret created");
    } else if (result.modifiedCount > 0) {
      console.log("✅ PayPal secret updated");
    } else {
      console.log("ℹ️  PayPal secret unchanged");
    }
    
    // Verify
    const doc = await secrets.findOne({ _id: "paypal" });
    console.log("📋 Stored document:");
    console.log(`   _id: ${doc._id}`);
    console.log(`   clientId: ${doc.clientId.slice(0, 10)}...`);
    console.log(`   account: ${doc.account}`);
    console.log(`   updatedAt: ${doc.updatedAt}`);
    
  } catch (error) {
    console.error("❌ Error:", error.message);
    process.exit(1);
  } finally {
    await client.close();
  }
}

main();
