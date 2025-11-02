#!/usr/bin/env node

/**
 * Batch fix tape ATProto records with missing blobs
 * - Downloads MP4/thumbnail from URLs in MongoDB
 * - Uploads as blobs to ATProto PDS
 * - Creates/updates ATProto records with proper blob references
 * - Handles duplicates by deleting extras and keeping the best one
 * - Updates MongoDB with correct rkey references
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
  console.error('Usage: node batch-fix-tape-blobs.mjs @handle [--dry-run] [--verbose]');
  console.error('Example: node batch-fix-tape-blobs.mjs @jeffrey --dry-run');
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

function hasVideoBlob(record) {
  const video = record.value?.video;
  if (!video || !video.ref || typeof video.ref !== 'object') {
    return false;
  }
  // Check for deserialized blob (has multihash) or raw $link
  return !!(video.ref.$link || (video.ref.multihash && video.ref.code));
}

async function downloadBlob(url, description) {
  console.log(`📥 Downloading ${description}...`);
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Failed to download ${description}: ${response.status} ${response.statusText}`);
  }
  const buffer = Buffer.from(await response.arrayBuffer());
  console.log(`✅ Downloaded ${description}: ${(buffer.length / 1024).toFixed(2)} KB`);
  return buffer;
}

async function fixTape(db, agent, tape, atprotoRecords) {
  const code = tape.code;
  console.log(`\n${'═'.repeat(80)}`);
  console.log(`📼 Processing tape: ${code}`);
  console.log(`   Slug: ${tape.slug}`);
  console.log(`   MongoDB _id: ${tape._id}`);
  console.log(`   ZIP: ${tape.zipUrl || 'MISSING'}`);
  console.log(`   MP4: ${tape.mp4Url || 'MISSING'}`);
  console.log(`   Thumbnail: ${tape.thumbnailUrl || 'none'}`);
  
  // Check if we need to rebake (no MP4 URL but have ZIP)
  if (!tape.mp4Url && tape.zipUrl) {
    console.log(`⚠️  Tape needs rebaking - has ZIP but no MP4`);
    console.log(`💡 To rebake, you can either:`);
    console.log(`   1. Trigger a new bake via the oven API`);
    console.log(`   2. Use the baker.mjs directly to process: ${tape.zipUrl}`);
    console.log(`\n   POST to oven with:`);
    console.log(`   {`);
    console.log(`     "mongoId": "${tape._id}",`);
    console.log(`     "slug": "${tape.slug}",`);
    console.log(`     "code": "${code}",`);
    console.log(`     "zipUrl": "${tape.zipUrl}",`);
    console.log(`     "callbackUrl": "https://aesthetic.computer/api/oven-complete",`);
    console.log(`     "callbackSecret": "<your-secret>"`);
    console.log(`   }`);
    return { skipped: true, reason: 'Needs rebaking - no MP4' };
  }
  
  if (!tape.mp4Url && !tape.zipUrl) {
    console.log(`⚠️  No MP4 or ZIP URL in MongoDB, cannot process`);
    return { skipped: true, reason: 'No MP4 or ZIP URL' };
  }
  
  if (!tape.mp4Url) {
    console.log(`⚠️  No MP4 URL in MongoDB, skipping...`);
    return { skipped: true, reason: 'No MP4 URL' };
  }

  // Check for existing ATProto records for this slug
  const existingRecords = atprotoRecords.filter(r => r.value?.slug === tape.slug);
  
  if (existingRecords.length > 1) {
    console.log(`⚠️  Found ${existingRecords.length} duplicate ATProto records for slug ${tape.slug}`);
    
    // Find the best record (one with blob if exists)
    const recordsWithBlobs = existingRecords.filter(hasVideoBlob);
    const recordsWithoutBlobs = existingRecords.filter(r => !hasVideoBlob(r));
    
    console.log(`   - ${recordsWithBlobs.length} with video blobs`);
    console.log(`   - ${recordsWithoutBlobs.length} without video blobs`);
    
    // Delete duplicates, keep one with blob if available
    const toDelete = existingRecords.slice(1); // Keep first one for now
    
    for (const record of toDelete) {
      const rkey = record.uri.split('/').pop();
      console.log(`   🗑️  Deleting duplicate record: ${rkey}`);
      if (!DRY_RUN) {
        try {
          await agent.com.atproto.repo.deleteRecord({
            repo: agent.session.did,
            collection: 'computer.aesthetic.tape',
            rkey: rkey
          });
          console.log(`   ✅ Deleted ${rkey}`);
        } catch (err) {
          console.log(`   ⚠️  Failed to delete ${rkey}: ${err.message}`);
        }
      } else {
        console.log(`   [DRY RUN] Would delete ${rkey}`);
      }
    }
    
    // Update existingRecords to just the one we kept
    existingRecords.length = 1;
  }
  
  const existingRecord = existingRecords[0];
  
  // Check if existing record has blob
  if (existingRecord && hasVideoBlob(existingRecord)) {
    console.log(`✅ ATProto record already has video blob`);
    const rkey = existingRecord.uri.split('/').pop();
    
    // Make sure MongoDB has the rkey
    if (tape.at?.rkey !== rkey) {
      console.log(`   🔄 Updating MongoDB with rkey: ${rkey}`);
      if (!DRY_RUN) {
        await db.db.collection('tapes').updateOne(
          { _id: tape._id },
          { $set: { at: { 
            rkey, 
            uri: existingRecord.uri, 
            cid: existingRecord.cid 
          } } }
        );
        await db.db.collection('oven-bakes').updateMany(
          { code },
          { $set: { atprotoRkey: rkey } }
        );
        console.log(`   ✅ MongoDB updated`);
      } else {
        console.log(`   [DRY RUN] Would update MongoDB`);
      }
    }
    return { success: true, hasBlob: true };
  }

  // Need to download and upload blobs
  try {
    // Download MP4
    const mp4Buffer = await downloadBlob(tape.mp4Url, 'MP4');
    
    // Download thumbnail if available
    let thumbnailBuffer = null;
    if (tape.thumbnailUrl) {
      try {
        thumbnailBuffer = await downloadBlob(tape.thumbnailUrl, 'thumbnail');
      } catch (err) {
        console.log(`⚠️  Failed to download thumbnail: ${err.message}`);
      }
    }
    
    if (DRY_RUN) {
      console.log(`[DRY RUN] Would upload blobs and create/update ATProto record`);
      return { success: true, dryRun: true };
    }
    
    // Upload video blob
    console.log('📤 Uploading video blob to PDS...');
    const videoBlob = await agent.uploadBlob(mp4Buffer, {
      encoding: 'video/mp4'
    });
    console.log(`✅ Video blob uploaded: ${videoBlob.data.blob.ref.$link || 'CID hidden'}`);
    
    // Upload thumbnail blob if we have it
    let thumbnailBlob = null;
    if (thumbnailBuffer) {
      console.log('📤 Uploading thumbnail blob to PDS...');
      thumbnailBlob = await agent.uploadBlob(thumbnailBuffer, {
        encoding: 'image/jpeg'
      });
      console.log(`✅ Thumbnail blob uploaded: ${thumbnailBlob.data.blob.ref.$link || 'CID hidden'}`);
    }
    
    // Delete old record if exists and doesn't have blob
    if (existingRecord) {
      const oldRkey = existingRecord.uri.split('/').pop();
      console.log(`🗑️  Deleting old record without blob: ${oldRkey}`);
      try {
        await agent.com.atproto.repo.deleteRecord({
          repo: agent.session.did,
          collection: 'computer.aesthetic.tape',
          rkey: oldRkey
        });
        console.log('✅ Old record deleted');
      } catch (err) {
        console.log('⚠️  Old record not found or already deleted');
      }
    }
    
    // Create new record with blobs
    console.log('📝 Creating ATProto record with blobs...');
    const record = {
      $type: 'computer.aesthetic.tape',
      slug: tape.slug,
      code: tape.code,
      acUrl: `https://aesthetic.computer/!${code}`,
      when: tape.when.toISOString(),
      ref: tape._id.toString(),
      video: videoBlob.data.blob
    };
    
    if (thumbnailBlob) {
      record.thumbnail = thumbnailBlob.data.blob;
    }
    
    const result = await agent.com.atproto.repo.createRecord({
      repo: agent.session.did,
      collection: 'computer.aesthetic.tape',
      record
    });
    
    const newRkey = result.data?.uri ? result.data.uri.split('/').pop() : result.uri.split('/').pop();
    const newUri = result.data?.uri || result.uri;
    const newCid = result.data?.cid || result.cid;
    
    console.log('✅ ATProto record created');
    console.log(`   Rkey: ${newRkey}`);
    if (VERBOSE) {
      console.log('   Full result:', JSON.stringify(result, null, 2));
    }
    
    // Update MongoDB with new rkey
    console.log('🔄 Updating MongoDB...');
    await db.db.collection('tapes').updateOne(
      { _id: tape._id },
      { $set: { at: { rkey: newRkey, uri: newUri, cid: newCid } } }
    );
    await db.db.collection('oven-bakes').updateMany(
      { code },
      { $set: { atprotoRkey: newRkey } }
    );
    console.log('✅ MongoDB updated with new rkey');
    
    return { success: true, created: true, rkey: newRkey };
    
  } catch (error) {
    console.error(`❌ Error processing tape ${code}: ${error.message}`);
    if (VERBOSE) {
      console.error(error);
    }
    return { success: false, error: error.message };
  }
}

async function main() {
  console.log(`\n${'═'.repeat(80)}`);
  console.log(`🔧 BATCH FIX TAPE BLOBS FOR @${handle}`);
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

    // Fetch MongoDB tapes
    console.log('📼 Fetching MongoDB tapes...');
    const mongoTapes = await tapes.find({ user: userSub }).sort({ when: 1 }).toArray();
    console.log(`📊 Found ${mongoTapes.length} MongoDB tapes\n`);

    // Identify tapes that need fixing
    const tapesToFix = [];
    
    for (const tape of mongoTapes) {
      const existingRecords = atprotoRecords.filter(r => r.value?.slug === tape.slug);
      
      // Needs fixing if:
      // 1. No ATProto record exists
      // 2. ATProto record exists but has no blob
      // 3. Multiple duplicate records exist
      if (existingRecords.length === 0) {
        tapesToFix.push({ tape, reason: 'No ATProto record' });
      } else if (existingRecords.length > 1) {
        tapesToFix.push({ tape, reason: `${existingRecords.length} duplicate records` });
      } else if (!hasVideoBlob(existingRecords[0])) {
        tapesToFix.push({ tape, reason: 'Missing video blob' });
      }
    }

    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📋 TAPES REQUIRING FIXES: ${tapesToFix.length}`);
    console.log(`${'═'.repeat(80)}`);
    
    if (tapesToFix.length === 0) {
      console.log('\n🎉 All tapes are in good shape! Nothing to fix.');
      return;
    }

    for (const { tape, reason } of tapesToFix) {
      console.log(`   - ${tape.code} (${tape.slug}): ${reason}`);
    }

    // Process each tape
    const results = {
      success: 0,
      failed: 0,
      skipped: 0,
      hasBlob: 0,
      created: 0
    };

    for (const { tape } of tapesToFix) {
      const result = await fixTape(database, agent, tape, atprotoRecords);
      
      if (result.skipped) {
        results.skipped++;
      } else if (result.success) {
        results.success++;
        if (result.hasBlob) results.hasBlob++;
        if (result.created) results.created++;
      } else {
        results.failed++;
      }
    }

    // Final summary
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📊 BATCH FIX COMPLETE`);
    console.log(`${'═'.repeat(80)}`);
    console.log(`✅ Successful: ${results.success}`);
    console.log(`   - Already had blobs: ${results.hasBlob}`);
    console.log(`   - Created new records: ${results.created}`);
    console.log(`❌ Failed: ${results.failed}`);
    console.log(`⚠️  Skipped: ${results.skipped}`);
    console.log(`${'═'.repeat(80)}\n`);

    if (DRY_RUN) {
      console.log('ℹ️  This was a DRY RUN. Run without --dry-run to apply changes.\n');
    }

    // Exit cleanly
    process.exit(results.failed > 0 ? 1 : 0);

  } catch (error) {
    console.error('❌ Fatal error:', error);
    if (VERBOSE) {
      console.error(error);
    }
    await database.disconnect();
    process.exit(1);
  } finally {
    await database.disconnect();
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
