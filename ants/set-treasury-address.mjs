#!/usr/bin/env node
// ants/set-treasury-address.mjs
// Sets treasuryAddress on the tezos-kidlisp secrets document in MongoDB.
//
// Usage:
//   node ants/set-treasury-address.mjs <tz1address>
//
// Required env vars: MONGODB_CONNECTION_STRING, MONGODB_NAME

import { MongoClient } from "mongodb";

const address = process.argv[2];
if (!address || !address.startsWith("tz")) {
  console.error("Usage: node ants/set-treasury-address.mjs <tz1address>");
  process.exit(1);
}

const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING);
await client.connect();
const db = client.db(process.env.MONGODB_NAME);

const result = await db.collection("secrets").updateOne(
  { _id: "tezos-kidlisp" },
  { $set: { treasuryAddress: address } }
);

if (result.matchedCount === 0) {
  console.error("❌ tezos-kidlisp secrets document not found");
} else {
  console.log(`✅ treasuryAddress set to ${address}`);
}

await client.close();
