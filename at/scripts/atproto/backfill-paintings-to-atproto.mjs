#!/usr/bin/env node

/**
 * Backfill Paintings to ATProto
 * 
 * Syncs all existing MongoDB paintings that don't have ATProto records yet.
 * Only syncs for users who have ATProto accounts.
 * Downloads images and uploads as blobs to ATProto.
 * 
 * Usage:
 *   node backfill-paintings-to-atproto.mjs [options]
 *   
 * Options:
 *   --dry-run          Show what would be synced without making changes
 *   --limit N          Only process N paintings (for testing)
 *   --user @handle     Only process paintings for specific user
 *   --batch-size N     Process N paintings at a time (default: 5)
 *   --delay MS         Delay between batches in ms (default: 2000)
 *   --skip-blobs       Skip uploading image blobs (faster, no thumbnails)
 */

import { connect } from '../../../system/backend/database.mjs';
import { createPaintingOnAtproto } from '../../../system/backend/painting-atproto.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const args = process.argv.slice(2);
const dryRun = args.includes('--dry-run');
const limitIndex = args.indexOf('--limit');
const limit = limitIndex !== -1 ? parseInt(args[limitIndex + 1]) : null;
const userIndex = args.indexOf('--user');
const targetUser = userIndex !== -1 ? args[userIndex + 1]?.replace('@', '') : null;
const batchSizeIndex = args.indexOf('--batch-size');
const batchSize = batchSizeIndex !== -1 ? parseInt(args[batchSizeIndex + 1]) : 5;
const delayIndex = args.indexOf('--delay');
const delay = delayIndex !== -1 ? parseInt(args[delayIndex + 1]) : 2000;
const skipBlobs = args.includes('--skip-blobs');

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function getHandleForUser(database, userId) {
  const handles = database.db.collection('@handles');
  const handleDoc = await handles.findOne({ _id: userId });
  return handleDoc?.handle || null;
}

async function backfillPaintings() {
  console.log('🎨 Backfill Paintings to ATProto\n');
  console.log(`Mode: ${dryRun ? '🔍 DRY RUN' : '✍️  LIVE'}`);
  if (limit) console.log(`Limit: ${limit} paintings`);
  if (targetUser) console.log(`User: @${targetUser}`);
  console.log(`Batch size: ${batchSize}`);
  console.log(`Delay: ${delay}ms`);
  if (skipBlobs) console.log(`Skip blobs: yes (no thumbnails)`);
  console.log();

  const database = await connect();
  const paintings = database.db.collection('paintings');
  const users = database.db.collection('users');
  const handles = database.db.collection('@handles');

  // Build query for paintings without ATProto records and with a user (not guest)
  const query = {
    user: { $exists: true, $ne: null },
    nuked: { $ne: true },
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

  // Count total paintings to backfill
  const totalCount = await paintings.countDocuments(query);
  console.log(`📊 Found ${totalCount} paintings without ATProto records\n`);

  if (totalCount === 0) {
    console.log('✅ All paintings already synced!');
    await database.disconnect();
    return;
  }

  // Get paintings sorted by date (oldest first for chronological order)
  const paintingsCursor = paintings
    .find(query)
    .sort({ when: 1 })
    .limit(limit || totalCount);

  const paintingsToProcess = await paintingsCursor.toArray();
  console.log(`Processing ${paintingsToProcess.length} paintings...\n`);

  let synced = 0;
  let skipped = 0;
  let failed = 0;
  const failures = [];

  // Process in batches
  for (let i = 0; i < paintingsToProcess.length; i += batchSize) {
    const batch = paintingsToProcess.slice(i, Math.min(i + batchSize, paintingsToProcess.length));
    
    console.log(`\n📦 Batch ${Math.floor(i / batchSize) + 1}/${Math.ceil(paintingsToProcess.length / batchSize)}`);
    console.log(`Processing paintings ${i + 1}-${Math.min(i + batchSize, paintingsToProcess.length)} of ${paintingsToProcess.length}\n`);

    for (const painting of batch) {
      const num = i + batch.indexOf(painting) + 1;
      
      // Get user info
      const user = await users.findOne({ _id: painting.user });
      const handle = await getHandleForUser(database, painting.user);

      process.stdout.write(`[${num}/${paintingsToProcess.length}] @${handle || 'unknown'} - ${painting.code || painting.slug.substring(0, 20)} `);

      // Check if user has ATProto account
      if (!user?.atproto?.did || !user?.atproto?.password) {
        console.log('⏭️  No ATProto account');
        skipped++;
        continue;
      }

      if (!handle) {
        console.log('⏭️  No handle found');
        skipped++;
        continue;
      }

      if (dryRun) {
        console.log('✓ Would sync');
        synced++;
        continue;
      }

      // Construct image URL
      const imageUrl = `https://aesthetic.computer/media/@${handle}/painting/${painting.slug}.png`;

      // Sync to ATProto
      try {
        const result = await createPaintingOnAtproto(
          database,
          painting.user,
          painting.slug,
          painting.code,
          imageUrl,
          painting.when,
          painting._id.toString(),
          painting.bucket
        );

        if (result.rkey) {
          // Update MongoDB with rkey
          await paintings.updateOne(
            { _id: painting._id },
            { $set: { atproto: { rkey: result.rkey } } }
          );
          console.log(`✅ Synced (${result.rkey})`);
          synced++;
        } else if (result.error) {
          console.log(`⚠️  ${result.error}`);
          if (result.error.includes('No ATProto account') || result.error.includes('Guest painting')) {
            skipped++;
          } else {
            failed++;
            failures.push({ 
              handle, 
              slug: painting.slug, 
              code: painting.code,
              error: result.error 
            });
          }
        }
      } catch (error) {
        console.log(`❌ ${error.message}`);
        failed++;
        failures.push({ 
          handle, 
          slug: painting.slug, 
          code: painting.code,
          error: error.message 
        });
      }
    }

    // Delay between batches (except for last batch)
    if (i + batchSize < paintingsToProcess.length) {
      console.log(`\n⏳ Waiting ${delay}ms before next batch...`);
      await sleep(delay);
    }
  }

  console.log('\n\n✨ Backfill Summary\n');
  console.log(`✅ Synced: ${synced}`);
  console.log(`⏭️  Skipped (no ATProto account or handle): ${skipped}`);
  console.log(`❌ Failed: ${failed}`);
  console.log(`📊 Total: ${paintingsToProcess.length}`);

  if (failures.length > 0) {
    console.log('\n\n❌ Failures:\n');
    failures.forEach((f, i) => {
      console.log(`${i + 1}. @${f.handle} - ${f.code || f.slug}`);
      console.log(`   Error: ${f.error}\n`);
    });
  }

  if (dryRun) {
    console.log('\n💡 This was a dry run. Run without --dry-run to actually sync.');
  }

  console.log('\n⚠️  Note: This backfill downloads and uploads images, so it may take a while.');
  console.log('Consider using --batch-size and --delay to tune performance vs. API limits.');

  await database.disconnect();
}

backfillPaintings().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
