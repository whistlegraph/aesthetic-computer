#!/usr/bin/env node

/**
 * Backfill Pieces to ATProto
 *
 * Syncs all existing MongoDB pieces that don't have ATProto records yet.
 * Only syncs for users who have ATProto accounts.
 *
 * Usage:
 *   node backfill-pieces-to-atproto.mjs [options]
 *
 * Options:
 *   --dry-run          Show what would be synced without making changes
 *   --limit N          Only process N pieces (for testing)
 *   --user @handle     Only process pieces for specific user
 *   --batch-size N     Process N pieces at a time (default: 10)
 *   --delay MS         Delay between batches in ms (default: 1000)
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
const batchSizeIndex = args.indexOf("--batch-size");
const batchSize =
  batchSizeIndex !== -1 ? parseInt(args[batchSizeIndex + 1]) : 10;
const delayIndex = args.indexOf("--delay");
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 1000;

async function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function backfillPieces() {
  console.log("🧩 Backfill Pieces to ATProto\n");
  console.log(`Mode: ${dryRun ? "🔍 DRY RUN" : "✍️  LIVE"}`);
  if (limit) console.log(`Limit: ${limit} pieces`);
  if (targetUser) console.log(`User: @${targetUser}`);
  console.log(`Batch size: ${batchSize}`);
  console.log(`Delay: ${delay}ms`);
  console.log();

  const database = await connect();
  const pieces = database.db.collection("pieces");
  const users = database.db.collection("users");
  const handles = database.db.collection("@handles");

  // Build query for pieces without ATProto records
  const query = {
    user: { $exists: true, $ne: null },
    $or: [{ atproto: { $exists: false } }, { "atproto.rkey": { $exists: false } }],
  };

  // Filter by user handle if specified
  if (targetUser) {
    const handleDoc = await handles.findOne({ handle: targetUser });
    if (!handleDoc) {
      console.error(`❌ User @${targetUser} not found`);
      await database.disconnect();
      process.exit(1);
    }
    query.user = handleDoc._id;
  }

  const cursor = pieces.find(query).sort({ when: -1 });
  if (limit) cursor.limit(limit);

  const allPieces = await cursor.toArray();
  console.log(`Found ${allPieces.length} unsynced pieces\n`);

  if (allPieces.length === 0) {
    console.log("✨ Nothing to backfill!");
    await database.disconnect();
    return;
  }

  let synced = 0,
    skipped = 0,
    failed = 0;

  for (let i = 0; i < allPieces.length; i++) {
    const piece = allPieces[i];
    const handle = await handles.findOne({ _id: piece.user });
    const h = handle?.handle || "unknown";

    if (dryRun) {
      console.log(
        `  [${i + 1}/${allPieces.length}] Would sync: @${h} — ${piece.slug || piece.name || piece._id}`,
      );
      synced++;
      continue;
    }

    try {
      const result = await createMediaRecord(database, MediaTypes.PIECE, piece, {
        userSub: piece.user,
      });

      if (result.error) {
        console.log(
          `  [${i + 1}/${allPieces.length}] ⏭️  @${h} — ${piece.slug}: ${result.error}`,
        );
        skipped++;
      } else {
        // Store rkey back in MongoDB
        await pieces.updateOne(
          { _id: piece._id },
          {
            $set: {
              "atproto.rkey": result.rkey,
              "atproto.uri": result.uri,
              "atproto.syncedAt": new Date().toISOString(),
            },
          },
        );
        console.log(
          `  [${i + 1}/${allPieces.length}] ✅ @${h} — ${piece.slug} → ${result.rkey}`,
        );
        synced++;
      }
    } catch (error) {
      console.log(
        `  [${i + 1}/${allPieces.length}] ❌ @${h} — ${piece.slug}: ${error.message}`,
      );
      failed++;
    }

    // Batch delay
    if ((i + 1) % batchSize === 0 && i < allPieces.length - 1) {
      await sleep(delay);
    }
  }

  console.log("\n" + "═".repeat(50));
  console.log(`✅ Synced:  ${synced}`);
  console.log(`⏭️  Skipped: ${skipped}`);
  console.log(`❌ Failed:  ${failed}`);
  console.log(`📊 Total:   ${allPieces.length}\n`);

  await database.disconnect();
}

backfillPieces().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
