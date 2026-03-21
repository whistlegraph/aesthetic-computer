#!/usr/bin/env node

/**
 * Backfill Moods to ATProto
 * 
 * Syncs all existing MongoDB moods that don't have ATProto records yet.
 * Only syncs for users who have ATProto accounts.
 * 
 * Usage:
 *   node backfill-moods-to-atproto.mjs [options]
 *   
 * Options:
 *   --dry-run          Show what would be synced without making changes
 *   --limit N          Only process N moods (for testing)
 *   --user @handle     Only process moods for specific user
 *   --batch-size N     Process N moods at a time (default: 10)
 *   --delay MS         Delay between batches in ms (default: 1000)
 */

import { connect } from '../../../system/backend/database.mjs';
import { createMoodOnAtproto } from '../../../system/backend/mood-atproto.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const args = process.argv.slice(2);
const dryRun = args.includes('--dry-run');
const limitIndex = args.indexOf('--limit');
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;
const userIndex = args.indexOf('--user');
const targetUser = userIndex !== -1 ? args[userIndex + 1]?.replace('@', '') : null;
const batchSizeIndex = args.indexOf('--batch-size');
const batchSize = batchSizeIndex !== -1 ? parseInt(args[batchSizeIndex + 1]) : 10;
const delayIndex = args.indexOf('--delay');
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 1000;

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function backfillMoods() {
  console.log('🔄 Backfill Moods to ATProto\n');
  console.log(`Mode: ${dryRun ? '🔍 DRY RUN' : '✍️  LIVE'}`);
  if (limit) console.log(`Limit: ${limit} moods`);
  if (targetUser) console.log(`User: @${targetUser}`);
  console.log(`Batch size: ${batchSize}`);
  console.log(`Delay: ${delay}ms\n`);

  const database = await connect();
  const moods = database.db.collection('moods');
  const users = database.db.collection('users');
  const handles = database.db.collection('@handles');

  // Build query for moods without ATProto records
  const query = {
    deleted: { $ne: true },
    $or: [
      { atproto: { $exists: false } },
      { 'atproto.rkey': { $exists: false } }
    ]
  };

  // If targeting specific user, filter by their handle
  if (targetUser) {
    const handleDoc = await handles.findOne({ handle: targetUser });
    if (!handleDoc) {
      console.error(`❌ User @${targetUser} not found`);
      await database.disconnect();
      return;
    }
    query.user = handleDoc._id;
  }

  // Count total moods to backfill
  const totalCount = await moods.countDocuments(query);
  console.log(`📊 Found ${totalCount} moods without ATProto records\n`);

  if (totalCount === 0) {
    console.log('✅ All moods already synced!');
    await database.disconnect();
    return;
  }

  // Get moods sorted by date (oldest first for chronological order)
  const moodsCursor = moods
    .find(query)
    .sort({ when: 1 })
    .limit(limit || totalCount);

  const moodsToProcess = await moodsCursor.toArray();
  console.log(`Processing ${moodsToProcess.length} moods...\n`);

  let synced = 0;
  let skipped = 0;
  let failed = 0;
  const failures = [];

  // Process in batches
  for (let i = 0; i < moodsToProcess.length; i += batchSize) {
    const batch = moodsToProcess.slice(i, Math.min(i + batchSize, moodsToProcess.length));
    
    console.log(`\n📦 Batch ${Math.floor(i / batchSize) + 1}/${Math.ceil(moodsToProcess.length / batchSize)}`);
    console.log(`Processing moods ${i + 1}-${Math.min(i + batchSize, moodsToProcess.length)} of ${moodsToProcess.length}\n`);

    for (const mood of batch) {
      const num = i + batch.indexOf(mood) + 1;
      
      // Get user info
      const user = await users.findOne({ _id: mood.user });
      const handleDoc = await handles.findOne({ _id: mood.user });
      const handle = handleDoc?.handle || 'unknown';

      process.stdout.write(`[${num}/${moodsToProcess.length}] @${handle} - ${mood.mood.substring(0, 40)}... `);

      // Check if user has ATProto account
      if (!user?.atproto?.did || !user?.atproto?.password) {
        console.log('⏭️  No ATProto account');
        skipped++;
        continue;
      }

      if (dryRun) {
        console.log('✓ Would sync');
        synced++;
        continue;
      }

      // Sync to ATProto
      try {
        const result = await createMoodOnAtproto(
          database,
          mood.user,
          mood.mood,
          mood.when,
          mood._id.toString()
        );

        if (result.rkey) {
          // Update MongoDB with rkey
          await moods.updateOne(
            { _id: mood._id },
            { $set: { atproto: { rkey: result.rkey } } }
          );
          console.log(`✅ Synced (${result.rkey})`);
          synced++;
        } else if (result.error) {
          console.log(`⚠️  ${result.error}`);
          if (result.error === 'No ATProto account') {
            skipped++;
          } else {
            failed++;
            failures.push({ handle, mood: mood.mood.substring(0, 40), error: result.error });
          }
        }
      } catch (error) {
        console.log(`❌ ${error.message}`);
        failed++;
        failures.push({ handle, mood: mood.mood.substring(0, 40), error: error.message });
      }
    }

    // Delay between batches (except for last batch)
    if (i + batchSize < moodsToProcess.length) {
      console.log(`\n⏳ Waiting ${delay}ms before next batch...`);
      await sleep(delay);
    }
  }

  console.log('\n\n✨ Backfill Summary\n');
  console.log(`✅ Synced: ${synced}`);
  console.log(`⏭️  Skipped (no ATProto account): ${skipped}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`📊 Total: ${moodsToProcess.length}`);

  if (failures.length > 0) {
    console.log('\n\n❌ Failures:\n');
    failures.forEach((f, i) => {
      console.log(`${i + 1}. @${f.handle} - "${f.mood}"`);
      console.log(`   Error: ${f.error}\n`);
    });
  }

  if (dryRun) {
    console.log('\n💡 This was a dry run. Run without --dry-run to actually sync.');
  }

  await database.disconnect();
}

backfillMoods().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
