#!/usr/bin/env node

/**
 * Backfill KidLisp to ATProto
 *
 * Syncs all existing MongoDB kidlisp records that don't have ATProto records.
 * Handles both user content and guest/anonymous content (via art account).
 *
 * Usage:
 *   node backfill-kidlisp-to-atproto.mjs [options]
 *
 * Options:
 *   --dry-run          Show what would be synced without making changes
 *   --limit N          Only process N records (for testing)
 *   --user @handle     Only process for specific user
 *   --guests-only      Only backfill guest/anonymous records
 *   --users-only       Only backfill user records (skip guests)
 *   --batch-size N     Process N records at a time (default: 20)
 *   --delay MS         Delay between batches in ms (default: 500)
 */

import { connect } from "../../../system/backend/database.mjs";
import { createMediaRecord, MediaTypes } from "../../../system/backend/media-atproto.mjs";
import { config } from "dotenv";

config({ path: "../../../system/.env" });

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const limitIndex = args.indexOf("--limit");
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;
const userIndex = args.indexOf("--user");
const targetUser =
  userIndex !== -1 ? args[userIndex + 1]?.replace("@", "") : null;
const guestsOnly = args.includes("--guests-only");
const usersOnly = args.includes("--users-only");
const batchSizeIndex = args.indexOf("--batch-size");
const batchSize =
  batchSizeIndex !== -1 ? parseInt(args[batchSizeIndex + 1]) : 20;
const delayIndex = args.indexOf("--delay");
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 500;

async function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function backfillKidlisp() {
  console.log("🎨 Backfill KidLisp to ATProto\n");
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "✍️  LIVE"}`);
  if (limit) console.log(`Limit: ${limit} records`);
  if (targetUser) console.log(`User: @${targetUser}`);
  if (guestsOnly) console.log(`Scope: guests only`);
  if (usersOnly) console.log(`Scope: users only`);
  console.log(`Batch size: ${batchSize}`);
  console.log(`Delay: ${delay}ms`);
  console.log();

  const database = await connect();
  const kidlisp = database.db.collection("kidlisp");
  const users = database.db.collection("users");
  const handles = database.db.collection("@handles");

  const query = {
    $or: [
      { atproto: { $exists: false } },
      { "atproto.rkey": { $exists: false } },
    ],
  };

  if (guestsOnly) {
    query.$and = [{ $or: [{ user: { $exists: false } }, { user: null }] }];
  } else if (usersOnly) {
    query.user = { $exists: true, $ne: null };
  }

  if (targetUser) {
    const handleDoc = await handles.findOne({ handle: targetUser });
    if (!handleDoc) {
      console.error(`❌ User @${targetUser} not found`);
      await database.disconnect();
      process.exit(1);
    }
    query.user = handleDoc._id;
  }

  const cursor = kidlisp.find(query).sort({ when: -1 });
  if (limit) cursor.limit(limit);

  const allRecords = await cursor.toArray();
  console.log(`Found ${allRecords.length} unsynced kidlisp records\n`);

  if (allRecords.length === 0) {
    console.log("✨ Nothing to backfill!");
    await database.disconnect();
    return;
  }

  let synced = 0,
    skipped = 0,
    failed = 0;

  // Cache user lookups
  const userCache = {};

  for (let i = 0; i < allRecords.length; i++) {
    const item = allRecords[i];
    const isGuest = !item.user;
    let h = "guest";

    if (!isGuest) {
      if (!(item.user in userCache)) {
        const handleDoc = await handles.findOne({ _id: item.user });
        userCache[item.user] = handleDoc?.handle || "unknown";
      }
      h = userCache[item.user];
    }

    const label = item.code
      ? item.code.slice(0, 30)
      : item._id.toString().slice(-8);

    if (dryRun) {
      console.log(
        `  [${i + 1}/${allRecords.length}] Would sync: @${h} — ${label}`,
      );
      synced++;
      continue;
    }

    try {
      const result = await createMediaRecord(
        database,
        MediaTypes.KIDLISP,
        item,
        { userSub: item.user || null },
      );

      if (result.error) {
        console.log(
          `  [${i + 1}/${allRecords.length}] ⏭️  @${h}: ${result.error}`,
        );
        skipped++;
      } else {
        await kidlisp.updateOne(
          { _id: item._id },
          {
            $set: {
              "atproto.rkey": result.rkey,
              "atproto.uri": result.uri,
              "atproto.syncedAt": new Date().toISOString(),
            },
          },
        );
        if ((i + 1) % 100 === 0 || i < 5) {
          console.log(
            `  [${i + 1}/${allRecords.length}] ✅ @${h} → ${result.rkey}`,
          );
        }
        synced++;
      }
    } catch (error) {
      console.log(
        `  [${i + 1}/${allRecords.length}] ❌ @${h}: ${error.message}`,
      );
      failed++;
    }

    if ((i + 1) % batchSize === 0 && i < allRecords.length - 1) {
      if ((i + 1) % 100 === 0) {
        console.log(`  ... ${i + 1}/${allRecords.length} processed (${synced} synced, ${failed} failed)`);
      }
      await sleep(delay);
    }
  }

  console.log("\n" + "═".repeat(50));
  console.log(`✅ Synced:  ${synced}`);
  console.log(`⏭️  Skipped: ${skipped}`);
  console.log(`❌ Failed:  ${failed}`);
  console.log(`📊 Total:   ${allRecords.length}\n`);

  await database.disconnect();
}

backfillKidlisp().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
