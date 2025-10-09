#!/usr/bin/env node

/**
 * Inspect Digital Ocean Spaces
 * 
 * Browse buckets, list files, check sizes, etc.
 * 
 * Usage:
 *   node inspect-spaces.mjs --list
 *   node inspect-spaces.mjs --count
 *   node inspect-spaces.mjs --user auth0|123
 *   node inspect-spaces.mjs --recent 10
 */

import { S3Client, ListObjectsV2Command, GetObjectCommand } from '@aws-sdk/client-s3';
import { config } from 'dotenv';

config();

const client = new S3Client({
  endpoint: process.env.DO_SPACES_ENDPOINT || 'https://nyc3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET
  }
});

const BUCKET = process.env.DO_SPACES_BUCKET || 'aesthetic-computer';

const args = process.argv.slice(2);
const command = args[0];

async function listPaintings({ prefix = '', maxKeys = 1000 } = {}) {
  console.log(`\n📦 Listing paintings from Digital Ocean Spaces`);
  console.log(`   Bucket: ${BUCKET}`);
  console.log(`   Prefix: ${prefix || '(all)'}`);
  console.log('═'.repeat(70) + '\n');

  const params = {
    Bucket: BUCKET,
    Prefix: prefix,
    MaxKeys: maxKeys
  };

  try {
    let continuationToken;
    let totalFiles = 0;
    let totalSize = 0;
    const files = [];

    do {
      if (continuationToken) {
        params.ContinuationToken = continuationToken;
      }

      const command = new ListObjectsV2Command(params);
      const response = await client.send(command);

      if (response.Contents) {
        for (const item of response.Contents) {
          // Filter for painting files
          if (item.Key.includes('/painting/') && item.Key.endsWith('.png')) {
            files.push({
              key: item.Key,
              size: item.Size,
              modified: item.LastModified
            });
            totalSize += item.Size;
            totalFiles++;
          }
        }
      }

      continuationToken = response.NextContinuationToken;
    } while (continuationToken);

    // Sort by modified date (most recent first)
    files.sort((a, b) => b.modified - a.modified);

    // Display results
    console.log(`📊 Found ${totalFiles} paintings\n`);

    files.slice(0, 20).forEach((file, i) => {
      const sizeKB = (file.size / 1024).toFixed(1);
      const date = file.modified.toISOString().split('T')[0];
      const parts = file.key.split('/');
      const user = parts[0];
      const filename = parts[parts.length - 1];
      
      console.log(`   ${(i + 1).toString().padStart(3)}. ${filename}`);
      console.log(`        User: ${user}`);
      console.log(`        Size: ${sizeKB} KB`);
      console.log(`        Date: ${date}`);
      console.log();
    });

    if (files.length > 20) {
      console.log(`   ... and ${files.length - 20} more`);
      console.log();
    }

    console.log('═'.repeat(70));
    console.log(`\n📈 Summary:`);
    console.log(`   Total paintings: ${totalFiles}`);
    console.log(`   Total size: ${(totalSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`   Average size: ${(totalSize / totalFiles / 1024).toFixed(1)} KB\n`);

    return { files, totalFiles, totalSize };
  } catch (error) {
    console.error('❌ Error listing paintings:', error.message);
    throw error;
  }
}

async function countByUser() {
  console.log(`\n👥 Counting paintings by user...\n`);

  const { files } = await listPaintings({ maxKeys: 10000 });
  
  const userCounts = {};
  
  for (const file of files) {
    const user = file.key.split('/')[0];
    userCounts[user] = (userCounts[user] || 0) + 1;
  }

  const sorted = Object.entries(userCounts)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 20);

  console.log('🏆 Top 20 users by painting count:\n');
  
  sorted.forEach(([user, count], i) => {
    console.log(`   ${(i + 1).toString().padStart(3)}. ${user}: ${count} paintings`);
  });

  console.log();
}

async function inspectUser(userId) {
  console.log(`\n🔍 Inspecting paintings for user: ${userId}\n`);

  const prefix = `${userId}/painting/`;
  const { files, totalFiles, totalSize } = await listPaintings({ prefix });

  if (totalFiles === 0) {
    console.log(`⚠️  No paintings found for user: ${userId}\n`);
    return;
  }

  console.log(`\n📁 All paintings for ${userId}:\n`);
  
  files.forEach((file, i) => {
    const filename = file.key.split('/').pop();
    const sizeKB = (file.size / 1024).toFixed(1);
    const date = file.modified.toISOString();
    
    console.log(`   ${(i + 1).toString().padStart(3)}. ${filename} (${sizeKB} KB)`);
    console.log(`        Modified: ${date}`);
  });

  console.log();
}

async function getRecent(limit = 10) {
  console.log(`\n🕒 Getting ${limit} most recent paintings...\n`);

  const { files } = await listPaintings({ maxKeys: 1000 });
  const recent = files.slice(0, limit);

  console.log(`📸 ${limit} most recent paintings:\n`);

  recent.forEach((file, i) => {
    const parts = file.key.split('/');
    const user = parts[0];
    const filename = parts[parts.length - 1];
    const sizeKB = (file.size / 1024).toFixed(1);
    const date = file.modified.toISOString();
    
    console.log(`   ${i + 1}. ${filename}`);
    console.log(`      User: ${user}`);
    console.log(`      Size: ${sizeKB} KB`);
    console.log(`      Date: ${date}`);
    console.log();
  });
}

async function main() {
  if (!process.env.DO_SPACES_KEY || !process.env.DO_SPACES_SECRET) {
    console.error('❌ Error: Missing Digital Ocean credentials');
    console.error('\nPlease set in .env:');
    console.error('  DO_SPACES_KEY=your-key');
    console.error('  DO_SPACES_SECRET=your-secret');
    console.error('  DO_SPACES_ENDPOINT=nyc3.digitaloceanspaces.com');
    console.error('  DO_SPACES_BUCKET=aesthetic-computer\n');
    process.exit(1);
  }

  try {
    if (args.includes('--list')) {
      await listPaintings();
    } else if (args.includes('--count')) {
      await countByUser();
    } else if (args.includes('--user')) {
      const userIndex = args.indexOf('--user');
      const userId = args[userIndex + 1];
      if (!userId) {
        console.error('❌ Error: --user requires a user ID');
        process.exit(1);
      }
      await inspectUser(userId);
    } else if (args.includes('--recent')) {
      const recentIndex = args.indexOf('--recent');
      const limit = parseInt(args[recentIndex + 1]) || 10;
      await getRecent(limit);
    } else {
      console.log('\n📦 Digital Ocean Spaces Inspector\n');
      console.log('Usage:');
      console.log('  node inspect-spaces.mjs --list           # List all paintings');
      console.log('  node inspect-spaces.mjs --count          # Count by user');
      console.log('  node inspect-spaces.mjs --user <id>      # Inspect specific user');
      console.log('  node inspect-spaces.mjs --recent 10      # Get 10 most recent');
      console.log();
    }
  } catch (error) {
    console.error('\n💥 Error:', error.message);
    process.exit(1);
  }
}

main();
