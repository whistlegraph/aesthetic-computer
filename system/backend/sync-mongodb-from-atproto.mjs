#!/usr/bin/env node
// sync-mongodb-from-atproto.mjs
// Sync MongoDB moods with existing ATProto records by reading ATProto and updating MongoDB with rkeys

import { ObjectId } from "mongodb";
import { AtpAgent } from "@atproto/api";
import { connect } from "./database.mjs";
import { userIDFromHandle } from "./authorization.mjs";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

// Parse command line arguments
const handle = process.argv[2];

if (!handle) {
  console.error("Usage: node sync-mongodb-from-atproto.mjs <handle>");
  console.error("Example: node sync-mongodb-from-atproto.mjs jeffrey");
  process.exit(1);
}

async function main() {
  console.log("=".repeat(80));
  console.log(`🔄 SYNCING MONGODB WITH ATPROTO FOR @${handle}`);
  console.log("=".repeat(80));

  // 1. Connect to MongoDB
  console.log("\n📋 Step 1: Connecting to MongoDB...");
  const database = await connect();
  const users = database.db.collection("users");
  const moods = database.db.collection("moods");

  // 2. Look up user
  console.log(`\n📋 Step 2: Looking up user @${handle}...`);
  const sub = await userIDFromHandle(handle, database);
  if (!sub) {
    console.error(`❌ User @${handle} not found`);
    await database.disconnect();
    process.exit(1);
  }
  console.log(`   User sub: ${sub}`);

  // 3. Check ATProto credentials
  console.log("\n📋 Step 3: Getting ATProto credentials...");
  const user = await users.findOne({ _id: sub });
  if (!user?.atproto?.did || !user?.atproto?.password) {
    console.error(`❌ User @${handle} has no ATProto account`);
    await database.disconnect();
    process.exit(1);
  }
  const { did, password: appPassword } = user.atproto;
  console.log(`   DID: ${did}`);

  // 4. Login to ATProto
  console.log("\n📋 Step 4: Logging into ATProto PDS...");
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({
    identifier: did,
    password: appPassword,
  });
  console.log("   ✅ Logged in to PDS");

  // 5. Fetch all ATProto moods
  console.log("\n📋 Step 5: Fetching all ATProto moods...");
  const atprotoMoods = [];
  let cursor;

  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: did,
      collection: "computer.aesthetic.mood",
      limit: 100,
      cursor,
    });

    atprotoMoods.push(...response.data.records);
    cursor = response.data.cursor;
    console.log(`   Fetched ${response.data.records.length} moods (total: ${atprotoMoods.length})`);
  } while (cursor);

  console.log(`   Total ATProto moods: ${atprotoMoods.length}`);

  // 6. Build ref → rkey mapping (support both old mongoId and new ref fields)
  console.log("\n📋 Step 6: Building ref → rkey mapping...");
  const refToRkey = new Map();

  for (const record of atprotoMoods) {
    const ref = record.value.ref || record.value.mongoId; // Support both old and new
    const rkey = record.uri.split("/").pop();

    if (ref) {
      refToRkey.set(ref, rkey);
    }
  }

  console.log(`   Mapped ${refToRkey.size} moods`);

  // 7. Update MongoDB with rkeys
  console.log("\n📋 Step 7: Updating MongoDB with rkeys...");
  let updateCount = 0;
  let skipCount = 0;
  let notFoundCount = 0;

  for (const [refStr, rkey] of refToRkey.entries()) {
    try {
      const ref = new ObjectId(refStr);

      // Check if mood exists
      const mood = await moods.findOne({ _id: ref });

      if (!mood) {
        console.log(`   ⚠️  Mood ${refStr} not found in MongoDB`);
        notFoundCount++;
        continue;
      }

      // Skip if already has rkey
      if (mood.atproto?.rkey === rkey) {
        skipCount++;
        continue;
      }

      // Update with rkey
      await moods.updateOne(
        { _id: ref },
        { $set: { atproto: { rkey } } }
      );

      updateCount++;

      if (updateCount % 50 === 0) {
        console.log(`   Updated ${updateCount} moods...`);
      }
    } catch (error) {
      console.error(`   ❌ Failed to update ${refStr}: ${error.message}`);
    }
  }

  // 8. Summary
  console.log("\n" + "=".repeat(80));
  console.log("\n📊 Summary:");
  console.log(`   Total ATProto moods: ${atprotoMoods.length}`);
  console.log(`   MongoDB moods updated: ${updateCount}`);
  console.log(`   Already synced (skipped): ${skipCount}`);
  if (notFoundCount > 0) {
    console.log(`   Not found in MongoDB: ${notFoundCount}`);
  }
  console.log("=".repeat(80) + "\n");

  await database.disconnect();
  process.exit(0);
}

main().catch((error) => {
  console.error("\n❌ Error:", error);
  process.exit(1);
});
