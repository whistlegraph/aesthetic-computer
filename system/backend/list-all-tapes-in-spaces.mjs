#!/usr/bin/env node

/**
 * List ALL tape files in Digital Ocean Spaces buckets
 * Shows what's actually stored, regardless of MongoDB state
 */

import { S3Client, ListObjectsV2Command } from '@aws-sdk/client-s3';
import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
config({ path: resolve(__dirname, '../.env') });

const artSpacesClient = new S3Client({
  endpoint: process.env.DO_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET,
  },
});

const atBlobsSpacesClient = new S3Client({
  endpoint: process.env.DO_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET,
  },
});

const ART_BUCKET = process.env.ART_SPACE_NAME || 'art-aesthetic-computer';
const AT_BLOBS_BUCKET = 'at-blobs-aesthetic-computer';

async function listTapesInBucket(client, bucket, prefix = 'tapes/') {
  const files = [];
  let continuationToken = undefined;

  console.log(`\n🔍 Listing ${bucket}/${prefix}...`);

  do {
    const response = await client.send(new ListObjectsV2Command({
      Bucket: bucket,
      Prefix: prefix,
      ContinuationToken: continuationToken,
    }));

    if (response.Contents) {
      files.push(...response.Contents.map(obj => ({
        key: obj.Key,
        size: obj.Size,
        lastModified: obj.LastModified
      })));
    }

    continuationToken = response.NextContinuationToken;
  } while (continuationToken);

  return files;
}

async function main() {
  console.log(`\n${'═'.repeat(80)}`);
  console.log(`📋 ALL TAPE FILES IN DIGITAL OCEAN SPACES`);
  console.log(`${'═'.repeat(80)}`);

  try {
    // List all files in art bucket
    const artFiles = await listTapesInBucket(artSpacesClient, ART_BUCKET, 'tapes/');
    console.log(`✅ Found ${artFiles.length} tape files in ${ART_BUCKET}\n`);
    
    if (artFiles.length > 0) {
      console.log('📁 Art bucket files:');
      artFiles.forEach(file => {
        console.log(`   ${file.key} (${(file.size / 1024 / 1024).toFixed(2)} MB)`);
      });
    }

    // List all files in at-blobs bucket
    const atBlobsFiles = await listTapesInBucket(atBlobsSpacesClient, AT_BLOBS_BUCKET, 'tapes/');
    console.log(`\n✅ Found ${atBlobsFiles.length} tape files in ${AT_BLOBS_BUCKET}\n`);
    
    if (atBlobsFiles.length > 0) {
      console.log('📁 At-blobs bucket files:');
      atBlobsFiles.forEach(file => {
        console.log(`   ${file.key} (${(file.size / 1024 / 1024).toFixed(2)} MB)`);
      });
    }

    // Analyze what we have
    console.log(`\n${'═'.repeat(80)}`);
    console.log('📊 ANALYSIS:');
    console.log(`${'═'.repeat(80)}`);

    const allFiles = [...artFiles, ...atBlobsFiles];
    const codes = new Set();
    
    allFiles.forEach(file => {
      const match = file.key.match(/tapes\/([^/\.]+)/);
      if (match) codes.add(match[1]);
    });

    console.log(`\n📼 Unique tape codes found: ${codes.size}`);
    if (codes.size > 0) {
      console.log('   Codes:', Array.from(codes).sort().join(', '));
    }

    // Group by file type
    const zips = allFiles.filter(f => f.key.endsWith('.zip'));
    const mp4s = allFiles.filter(f => f.key.endsWith('.mp4'));
    const thumbs = allFiles.filter(f => f.key.includes('-thumb'));

    console.log(`\n📦 File types:`);
    console.log(`   ZIPs: ${zips.length}`);
    console.log(`   MP4s: ${mp4s.length}`);
    console.log(`   Thumbnails: ${thumbs.length}`);

  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

main();
