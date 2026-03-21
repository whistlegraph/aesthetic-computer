#!/usr/bin/env node

/**
 * Check if ZIP files exist in Digital Ocean Spaces for tapes missing MP4s
 * This helps determine if we can rebake or if we need to delete orphaned records
 */

import { S3Client, HeadObjectCommand, ListObjectsV2Command } from '@aws-sdk/client-s3';
import { connect } from '../system/backend/database.mjs';
import { userIDFromHandle } from '../system/backend/authorization.mjs';
import { config } from 'dotenv';

config({ path: './system/.env' });

const handleArg = process.argv[2];
if (!handleArg || !handleArg.startsWith('@')) {
  console.error('Usage: node check-tape-zips-in-spaces.mjs @handle');
  console.error('Example: node check-tape-zips-in-spaces.mjs @jeffrey');
  process.exit(1);
}

const handle = handleArg.replace('@', '');

// Initialize S3 clients for both buckets
const artSpacesClient = new S3Client({
  endpoint: process.env.ART_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.ART_SPACES_KEY,
    secretAccessKey: process.env.ART_SPACES_SECRET,
  },
});

const atBlobsSpacesClient = new S3Client({
  endpoint: process.env.AT_BLOBS_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.AT_BLOBS_SPACES_KEY,
    secretAccessKey: process.env.AT_BLOBS_SPACES_SECRET,
  },
});

const ART_BUCKET = process.env.ART_SPACES_BUCKET || 'art-aesthetic-computer';
const AT_BLOBS_BUCKET = process.env.AT_BLOBS_SPACES_BUCKET || 'at-blobs-aesthetic-computer';

async function checkFileExists(client, bucket, key) {
  try {
    await client.send(new HeadObjectCommand({ Bucket: bucket, Key: key }));
    return true;
  } catch (error) {
    if (error.name === 'NotFound' || error.$metadata?.httpStatusCode === 404) {
      return false;
    }
    throw error;
  }
}

async function listTapesInBucket(client, bucket, prefix = 'tapes/') {
  const files = [];
  let continuationToken = undefined;

  do {
    const response = await client.send(new ListObjectsV2Command({
      Bucket: bucket,
      Prefix: prefix,
      ContinuationToken: continuationToken,
    }));

    if (response.Contents) {
      files.push(...response.Contents.map(obj => obj.Key));
    }

    continuationToken = response.NextContinuationToken;
  } while (continuationToken);

  return files;
}

async function main() {
  console.log(`\n${'═'.repeat(80)}`);
  console.log(`🔍 CHECKING TAPE ZIP FILES IN DIGITAL OCEAN SPACES`);
  console.log(`${'═'.repeat(80)}\n`);

  const database = await connect();

  try {
    const userSub = await userIDFromHandle(handle, database);
    if (!userSub) {
      console.error(`❌ Could not find user for handle @${handle}`);
      process.exit(1);
    }

    const tapes = database.db.collection('tapes');

    // Find tapes without MP4 or ZIP URLs
    const orphanedTapes = await tapes.find({ 
      user: userSub,
      $or: [
        { mp4Url: { $exists: false } },
        { mp4Url: null },
        { mp4Url: '' }
      ]
    }).sort({ when: 1 }).toArray();

    console.log(`📼 Found ${orphanedTapes.length} tapes without MP4 URLs\n`);

    if (orphanedTapes.length === 0) {
      console.log('✅ No orphaned tapes found!');
      await database.disconnect();
      process.exit(0);
    }

    // List all files in both buckets
    console.log('📋 Listing files in art-aesthetic-computer bucket...');
    const artFiles = await listTapesInBucket(artSpacesClient, ART_BUCKET, 'tapes/');
    console.log(`   Found ${artFiles.length} tape files in art bucket`);

    console.log('📋 Listing files in at-blobs-aesthetic-computer bucket...');
    const atBlobsFiles = await listTapesInBucket(atBlobsSpacesClient, AT_BLOBS_BUCKET, 'tapes/');
    console.log(`   Found ${atBlobsFiles.length} tape files in at-blobs bucket\n`);

    const results = {
      hasZip: [],
      hasMP4: [],
      hasBoth: [],
      hasNeither: []
    };

    for (const tape of orphanedTapes) {
      const code = tape.code;
      console.log(`${'─'.repeat(80)}`);
      console.log(`📼 Tape: ${code} (${tape.slug})`);
      
      // Check for ZIP in art bucket
      const zipKeyArt = `tapes/${code}.zip`;
      const hasZipArt = artFiles.includes(zipKeyArt);
      
      // Check for ZIP in at-blobs bucket
      const zipKeyAtBlobs = `tapes/${code}.zip`;
      const hasZipAtBlobs = atBlobsFiles.includes(zipKeyAtBlobs);
      
      // Check for MP4 in both buckets
      const mp4KeyArt = `tapes/${code}.mp4`;
      const hasMP4Art = artFiles.includes(mp4KeyArt);
      
      const mp4KeyAtBlobs = `tapes/${code}.mp4`;
      const hasMP4AtBlobs = atBlobsFiles.includes(mp4KeyAtBlobs);
      
      const hasZip = hasZipArt || hasZipAtBlobs;
      const hasMP4 = hasMP4Art || hasMP4AtBlobs;

      console.log(`   ZIP in art bucket: ${hasZipArt ? '✅' : '❌'}`);
      console.log(`   ZIP in at-blobs bucket: ${hasZipAtBlobs ? '✅' : '❌'}`);
      console.log(`   MP4 in art bucket: ${hasMP4Art ? '✅' : '❌'}`);
      console.log(`   MP4 in at-blobs bucket: ${hasMP4AtBlobs ? '✅' : '❌'}`);

      if (hasZip && hasMP4) {
        console.log(`   ✅ Can update MongoDB with URLs`);
        results.hasBoth.push({
          code,
          slug: tape.slug,
          mongoId: tape._id.toString(),
          zipUrl: hasZipArt 
            ? `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKeyArt}`
            : `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKeyAtBlobs}`,
          mp4Url: hasMP4Art
            ? `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${mp4KeyArt}`
            : `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${mp4KeyAtBlobs}`,
        });
      } else if (hasZip) {
        console.log(`   🔄 Can rebake (has ZIP)`);
        results.hasZip.push({
          code,
          slug: tape.slug,
          mongoId: tape._id.toString(),
          zipUrl: hasZipArt 
            ? `https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKeyArt}`
            : `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${zipKeyAtBlobs}`,
        });
      } else if (hasMP4) {
        console.log(`   📹 Can add MP4 URL to MongoDB`);
        results.hasMP4.push({
          code,
          slug: tape.slug,
          mongoId: tape._id.toString(),
          mp4Url: hasMP4Art
            ? `https://art-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${mp4KeyArt}`
            : `https://at-blobs-aesthetic-computer.sfo3.digitaloceanspaces.com/${mp4KeyAtBlobs}`,
        });
      } else {
        console.log(`   ❌ Should be deleted (no source files)`);
        results.hasNeither.push({
          code,
          slug: tape.slug,
          mongoId: tape._id.toString(),
        });
      }
    }

    // Summary
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📊 SUMMARY`);
    console.log(`${'═'.repeat(80)}`);
    console.log(`✅ Has both ZIP & MP4: ${results.hasBoth.length}`);
    console.log(`🔄 Has ZIP only (can rebake): ${results.hasZip.length}`);
    console.log(`📹 Has MP4 only (can update DB): ${results.hasMP4.length}`);
    console.log(`❌ Has neither (should delete): ${results.hasNeither.length}`);
    console.log(`${'═'.repeat(80)}\n`);

    // Output actionable info
    if (results.hasBoth.length > 0) {
      console.log(`\n💡 Tapes with both ZIP & MP4 (can update MongoDB):`);
      for (const tape of results.hasBoth) {
        console.log(`   ${tape.code}: ${tape.mp4Url}`);
      }
    }

    if (results.hasZip.length > 0) {
      console.log(`\n💡 Tapes that can be rebaked (have ZIP):`);
      for (const tape of results.hasZip) {
        console.log(`   ${tape.code}: ${tape.zipUrl}`);
      }
    }

    if (results.hasMP4.length > 0) {
      console.log(`\n💡 Tapes with MP4 only (can update MongoDB):`);
      for (const tape of results.hasMP4) {
        console.log(`   ${tape.code}: ${tape.mp4Url}`);
      }
    }

    if (results.hasNeither.length > 0) {
      console.log(`\n⚠️  Tapes that should be deleted (no source files):`);
      for (const tape of results.hasNeither) {
        console.log(`   ${tape.code} (${tape.slug})`);
      }
    }

    await database.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('❌ Fatal error:', error);
    await database.disconnect();
    process.exit(1);
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
