#!/usr/bin/env node

/**
 * Verify that MongoDB tapes and ATProto tape records match for a given user.
 * - Confirms every Mongo tape has an ATProto rkey and matching record.
 * - Checks that ATProto records have video blobs properly attached.
 * - Identifies tapes missing blobs that need to be re-uploaded.
 */

import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const handleArg = process.argv[2];
if (!handleArg || !handleArg.startsWith('@')) {
  console.error('Usage: node verify-tapes-sync.mjs @handle [--limit N] [--verbose]');
  process.exit(1);
}

const limitIndex = process.argv.indexOf('--limit');
const limit = limitIndex !== -1 ? parseInt(process.argv[limitIndex + 1], 10) : null;
const verbose = process.argv.includes('--verbose');

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

    console.log(`\n🔍 Verifying tapes for @${handle} (${userDoc.atproto.did})`);
    console.log('─'.repeat(80));

    // Fetch MongoDB tapes
    const mongoQuery = { user: userSub };
    const mongoTapesCursor = tapes.find(mongoQuery).sort({ when: 1 });
    const mongoTapes = limit ? await mongoTapesCursor.limit(limit).toArray() : await mongoTapesCursor.toArray();

    console.log(`\n📼 MongoDB tapes for @${handle}: ${mongoTapes.length}`);

    // Check for missing rkeys in MongoDB
    const missingRkey = mongoTapes.filter(t => !t.at?.rkey);
    if (missingRkey.length > 0) {
      console.log(`⚠️  ${missingRkey.length} MongoDB tapes missing at.rkey`);
      if (verbose) {
        missingRkey.slice(0, 10).forEach(t => console.log(`   - ${t.code} (${t.slug})`));
        if (missingRkey.length > 10) {
          console.log(`   ... and ${missingRkey.length - 10} more`);
        }
      }
    } else {
      console.log('✅ All MongoDB tapes have at.rkey values');
    }

    // Login to ATProto
    const agent = new AtpAgent({ service: process.env.PDS_URL || 'https://at.aesthetic.computer' });
    await agent.login({ identifier: userDoc.atproto.did, password: userDoc.atproto.password });

    // Fetch ATProto records
    console.log(`\n🦋 Fetching ATProto tape records...`);
    const pdsRecords = await fetchAllTapesFromPds(agent, userDoc.atproto.did);
    console.log(`🗂️  ATProto tape records for @${handle}: ${pdsRecords.length}`);

    // Check for duplicate slugs
    const slugToRecords = new Map();
    const duplicateSlugs = new Set();

    for (const record of pdsRecords) {
      const slug = record.value?.slug;
      if (!slug) continue;
      const list = slugToRecords.get(slug) ?? [];
      list.push(record);
      slugToRecords.set(slug, list);
      if (list.length > 1) duplicateSlugs.add(slug);
    }

    if (duplicateSlugs.size > 0) {
      console.log(`\n❌ Duplicate ATProto records detected for ${duplicateSlugs.size} slug(s):`);
      for (const slug of duplicateSlugs) {
        const records = slugToRecords.get(slug);
        console.log(`   - ${slug} (${records.length} records)`);
        if (verbose) {
          records.forEach(r => console.log(`      • ${r.uri}`));
        }
      }
    } else {
      console.log('✅ No duplicate ATProto records found');
    }

    // Check for missing blobs in ATProto records
    console.log(`\n🔍 Checking for missing blobs in ATProto records...`);
    const missingBlobs = [];
    const hasBlobs = [];
    const hasBlobRefButNotUploaded = [];
    
    for (const record of pdsRecords) {
      const video = record.value?.video;
      
      if (!video) {
        // No video field at all
        missingBlobs.push(record);
      } else if (video.ref && typeof video.ref === 'object') {
        // Has blob reference - when retrieved from PDS, the $link is deserialized 
        // into an object with code, version, multihash, bytes properties
        // OR it still has $link property (depending on API version)
        if (video.ref.$link || (video.ref.multihash && video.ref.code)) {
          hasBlobs.push(record);
        } else {
          hasBlobRefButNotUploaded.push(record);
        }
      } else {
        // Video field exists but doesn't have valid blob reference
        hasBlobRefButNotUploaded.push(record);
      }
    }

    console.log(`\n📊 Blob Status Summary:`);
    console.log(`   ✅ Records with video blobs: ${hasBlobs.length}`);
    console.log(`   ⚠️  Records with invalid blob refs: ${hasBlobRefButNotUploaded.length}`);
    console.log(`   ❌ Records missing video field: ${missingBlobs.length}`);

    if (hasBlobRefButNotUploaded.length > 0) {
      console.log(`\n⚠️  Tapes with invalid blob references:`);
      hasBlobRefButNotUploaded.slice(0, 20).forEach(record => {
        const code = record.value?.code || 'N/A';
        const slug = record.value?.slug || 'N/A';
        const rkey = record.uri.split('/').pop();
        console.log(`   - Code: ${code}, Slug: ${slug}, Rkey: ${rkey}`);
        if (verbose && record.value) {
          console.log(`      Video field:`, JSON.stringify(record.value.video));
        }
      });
      if (hasBlobRefButNotUploaded.length > 20) {
        console.log(`   ... and ${hasBlobRefButNotUploaded.length - 20} more`);
      }
    }

    if (missingBlobs.length > 0) {
      console.log(`\n❌ Tapes missing video field entirely:`);
      missingBlobs.slice(0, 20).forEach(record => {
        const code = record.value?.code || 'N/A';
        const slug = record.value?.slug || 'N/A';
        const rkey = record.uri.split('/').pop();
        console.log(`   - Code: ${code}, Slug: ${slug}, Rkey: ${rkey}`);
        if (verbose && record.value) {
          console.log(`      Video field:`, JSON.stringify(record.value.video));
        }
      });
      if (missingBlobs.length > 20) {
        console.log(`   ... and ${missingBlobs.length - 20} more`);
      }
    }
    
    if (missingBlobs.length > 0 || hasBlobRefButNotUploaded.length > 0) {
      console.log(`\n💡 To fix these tapes, run:`);
      console.log(`   node system/backend/recreate-tape-with-blobs.mjs <code>`);
    }

    // Check for MongoDB tapes with rkeys but no matching ATProto record
    console.log(`\n🔍 Checking for orphaned MongoDB rkeys...`);
    const atprotoRkeys = new Set(pdsRecords.map(r => r.uri.split('/').pop()));
    const orphanedMongo = mongoTapes.filter(t => t.at?.rkey && !atprotoRkeys.has(t.at.rkey));
    
    if (orphanedMongo.length > 0) {
      console.log(`⚠️  ${orphanedMongo.length} MongoDB tapes have rkeys but no matching ATProto record:`);
      if (verbose) {
        orphanedMongo.slice(0, 10).forEach(t => {
          console.log(`   - ${t.code} (rkey: ${t.at.rkey})`);
        });
        if (orphanedMongo.length > 10) {
          console.log(`   ... and ${orphanedMongo.length - 10} more`);
        }
      }
    } else {
      console.log('✅ All MongoDB rkeys have matching ATProto records');
    }

    // Summary
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📊 SUMMARY FOR @${handle}`);
    console.log(`${'═'.repeat(80)}`);
    console.log(`MongoDB Tapes:          ${mongoTapes.length}`);
    console.log(`  - Missing rkey:       ${missingRkey.length}`);
    console.log(`  - Have rkey:          ${mongoTapes.length - missingRkey.length}`);
    console.log(`\nATProto Records:        ${pdsRecords.length}`);
    console.log(`  - With video blobs:   ${hasBlobs.length}`);
    console.log(`  - Invalid blob refs:  ${hasBlobRefButNotUploaded.length}`);
    console.log(`  - Missing video:      ${missingBlobs.length}`);
    console.log(`  - Duplicates:         ${duplicateSlugs.size}`);
    console.log(`\nOrphaned MongoDB rkeys: ${orphanedMongo.length}`);
    console.log(`${'═'.repeat(80)}\n`);

    // Exit with error code if issues found
    if (missingBlobs.length > 0 || hasBlobRefButNotUploaded.length > 0 || duplicateSlugs.size > 0 || orphanedMongo.length > 0 || missingRkey.length > 0) {
      await database.disconnect();
      process.exit(1);
    }

    await database.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('❌ Error:', error);
    await database.disconnect();
    process.exit(1);
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
