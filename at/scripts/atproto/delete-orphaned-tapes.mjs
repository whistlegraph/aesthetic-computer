#!/usr/bin/env node

/**
 * Delete orphaned tape records from MongoDB and ATProto
 * For tapes that have no ZIP or MP4 files and cannot be recovered
 */

import { AtpAgent } from '@atproto/api';
import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const DRY_RUN = process.argv.includes('--dry-run');
const VERBOSE = process.argv.includes('--verbose');

const handleArg = process.argv[2];
if (!handleArg || !handleArg.startsWith('@') || handleArg.startsWith('--')) {
  console.error('Usage: node delete-orphaned-tapes.mjs @handle [--dry-run] [--verbose]');
  console.error('Example: node delete-orphaned-tapes.mjs @jeffrey --dry-run');
  process.exit(1);
}

const handle = handleArg.replace('@', '');

async function fetchAllTapesFromPds(agent, did) {
  const allRecords = [];
  let cursor = undefined;

  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: did,
      collection: 'computer.aesthetic.tape',
      limit: 100,
      cursor,
    });

    const { records = [], cursor: nextCursor } = response.data ?? response;
    allRecords.push(...records);
    cursor = nextCursor;
  } while (cursor);

  return allRecords;
}

async function main() {
  console.log(`\n${'═'.repeat(80)}`);
  console.log(`🗑️  DELETE ORPHANED TAPES FOR @${handle}`);
  console.log(`${'═'.repeat(80)}`);
  if (DRY_RUN) {
    console.log('⚠️  DRY RUN MODE - No changes will be made');
  }
  console.log('');

  const database = await connect();

  try {
    const userSub = await userIDFromHandle(handle, database);
    if (!userSub) {
      console.error(`❌ Could not find user for handle @${handle}`);
      process.exit(1);
    }

    const users = database.db.collection('users');
    const tapes = database.db.collection('tapes');
    const ovenBakes = database.db.collection('oven-bakes');

    const userDoc = await users.findOne({ _id: userSub });
    if (!userDoc?.atproto?.did || !userDoc?.atproto?.password) {
      console.error(`❌ User @${handle} does not have ATProto credentials stored`);
      process.exit(1);
    }

    console.log(`🔐 Authenticating as ${userDoc.atproto.did}...`);

    // Login to ATProto
    const pdsUrl = `https://${userDoc.atproto.pdsHost || 'jeffrey.at.aesthetic.computer'}`;
    const agent = new AtpAgent({ service: pdsUrl });
    await agent.login({
      identifier: userDoc.atproto.did,
      password: userDoc.atproto.password
    });
    console.log('✅ Logged in to PDS\n');

    // Fetch all ATProto records
    console.log('🦋 Fetching ATProto tape records...');
    const atprotoRecords = await fetchAllTapesFromPds(agent, userDoc.atproto.did);
    console.log(`📊 Found ${atprotoRecords.length} ATProto records\n`);

    // Fetch MongoDB tapes without MP4 or ZIP
    console.log('📼 Fetching orphaned MongoDB tapes...');
    const orphanedTapes = await tapes.find({
      user: userSub,
      $and: [
        {
          $or: [
            { mp4Url: { $exists: false } },
            { mp4Url: null },
            { mp4Url: '' }
          ]
        },
        {
          $or: [
            { zipUrl: { $exists: false } },
            { zipUrl: null },
            { zipUrl: '' }
          ]
        }
      ]
    }).sort({ when: 1 }).toArray();

    console.log(`📊 Found ${orphanedTapes.length} orphaned tapes\n`);

    if (orphanedTapes.length === 0) {
      console.log('✅ No orphaned tapes found!');
      await database.disconnect();
      process.exit(0);
    }

    const results = {
      deletedMongo: 0,
      deletedAtproto: 0,
      deletedOvenBakes: 0,
      failed: 0
    };

    for (const tape of orphanedTapes) {
      console.log(`${'─'.repeat(80)}`);
      console.log(`🗑️  Deleting tape: ${tape.code} (${tape.slug})`);
      console.log(`   MongoDB _id: ${tape._id}`);
      
      try {
        // Find ATProto records for this slug
        const matchingRecords = atprotoRecords.filter(r => r.value?.slug === tape.slug);
        
        if (matchingRecords.length > 0) {
          console.log(`   Found ${matchingRecords.length} ATProto record(s)`);
          
          for (const record of matchingRecords) {
            const rkey = record.uri.split('/').pop();
            console.log(`   🗑️  Deleting ATProto record: ${rkey}`);
            
            if (!DRY_RUN) {
              try {
                await agent.com.atproto.repo.deleteRecord({
                  repo: agent.session.did,
                  collection: 'computer.aesthetic.tape',
                  rkey: rkey
                });
                console.log(`   ✅ Deleted ATProto record ${rkey}`);
                results.deletedAtproto++;
              } catch (err) {
                console.log(`   ⚠️  Failed to delete ATProto record: ${err.message}`);
                results.failed++;
              }
            } else {
              console.log(`   [DRY RUN] Would delete ATProto record ${rkey}`);
            }
          }
        } else {
          console.log(`   No ATProto records found for this slug`);
        }
        
        // Delete from oven-bakes
        const ovenBakeCount = await ovenBakes.countDocuments({ code: tape.code });
        if (ovenBakeCount > 0) {
          console.log(`   🗑️  Deleting ${ovenBakeCount} oven-bake record(s)`);
          if (!DRY_RUN) {
            const ovenResult = await ovenBakes.deleteMany({ code: tape.code });
            console.log(`   ✅ Deleted ${ovenResult.deletedCount} oven-bake record(s)`);
            results.deletedOvenBakes += ovenResult.deletedCount;
          } else {
            console.log(`   [DRY RUN] Would delete ${ovenBakeCount} oven-bake record(s)`);
          }
        }
        
        // Delete from MongoDB tapes
        console.log(`   🗑️  Deleting MongoDB tape record`);
        if (!DRY_RUN) {
          await tapes.deleteOne({ _id: tape._id });
          console.log(`   ✅ Deleted MongoDB tape record`);
          results.deletedMongo++;
        } else {
          console.log(`   [DRY RUN] Would delete MongoDB tape record`);
        }
        
      } catch (error) {
        console.error(`   ❌ Error: ${error.message}`);
        if (VERBOSE) {
          console.error(error);
        }
        results.failed++;
      }
    }

    // Final summary
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📊 CLEANUP COMPLETE`);
    console.log(`${'═'.repeat(80)}`);
    console.log(`🗑️  MongoDB tapes deleted: ${results.deletedMongo}`);
    console.log(`🗑️  ATProto records deleted: ${results.deletedAtproto}`);
    console.log(`🗑️  Oven-bake records deleted: ${results.deletedOvenBakes}`);
    console.log(`❌ Failed: ${results.failed}`);
    console.log(`${'═'.repeat(80)}\n`);

    if (DRY_RUN) {
      console.log('ℹ️  This was a DRY RUN. Run without --dry-run to apply changes.\n');
    }

    await database.disconnect();
    process.exit(results.failed > 0 ? 1 : 0);

  } catch (error) {
    console.error('❌ Fatal error:', error);
    if (VERBOSE) {
      console.error(error);
    }
    await database.disconnect();
    process.exit(1);
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
