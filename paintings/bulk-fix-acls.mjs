#!/usr/bin/env node
// Bulk fix ACLs for recent paintings by listing from S3

import { S3Client, ListObjectsV2Command, PutObjectAclCommand } from '@aws-sdk/client-s3';
import { config } from 'dotenv';

config();

const DRY_RUN = process.argv.includes('--dry-run');
const DAYS = parseInt(process.argv.find(arg => arg.startsWith('--days='))?.split('=')[1]) || 30;

const s3Client = new S3Client({
  endpoint: 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.DO_SPACES_KEY,
    secretAccessKey: process.env.DO_SPACES_SECRET
  }
});

console.log('╔═══════════════════════════════════════════════════════════════╗');
console.log('║           🔓 Fix Painting ACLs to Public-Read                ║');
console.log('╚═══════════════════════════════════════════════════════════════╝\n');

if (DRY_RUN) {
  console.log('🔍 DRY RUN MODE - No changes will be made\n');
}

const bucket = 'user-aesthetic-computer';
const cutoffDate = new Date();
cutoffDate.setDate(cutoffDate.getDate() - DAYS);

console.log(`📊 Scanning bucket: ${bucket}`);
console.log(`   Looking for .png files modified after: ${cutoffDate.toISOString()}\n`);

let processed = 0;
let fixed = 0;
let skipped = 0;
let errors = 0;

async function processPrefix(prefix = '') {
  let continuationToken;
  
  do {
    const listCommand = new ListObjectsV2Command({
      Bucket: bucket,
      Prefix: prefix,
      ContinuationToken: continuationToken,
      MaxKeys: 1000
    });
    
    const response = await s3Client.send(listCommand);
    
    if (!response.Contents) break;
    
    for (const object of response.Contents) {
      if (!object.Key.endsWith('.png')) continue;
      if (!object.Key.includes('/painting/')) continue;
      if (object.LastModified < cutoffDate) continue;
      
      processed++;
      const shortKey = object.Key.length > 70 ? object.Key.substring(0, 67) + '...' : object.Key;
      console.log(`[${processed}] ${shortKey}`);
      console.log(`    Modified: ${object.LastModified.toISOString()}`);
      
      if (!DRY_RUN) {
        try {
          const aclCommand = new PutObjectAclCommand({
            Bucket: bucket,
            Key: object.Key,
            ACL: 'public-read'
          });
          await s3Client.send(aclCommand);
          console.log('    ✅ ACL set to public-read');
          fixed++;
        } catch (error) {
          console.log(`    ❌ Error: ${error.message}`);
          errors++;
        }
      } else {
        console.log('    🔍 Would set ACL to public-read');
      }
    }
    
    continuationToken = response.NextContinuationToken;
  } while (continuationToken);
}

// Process the bucket
await processPrefix();

console.log('\n' + '═'.repeat(65));
console.log('📊 Summary:');
console.log(`   Processed: ${processed}`);
if (!DRY_RUN) {
  console.log(`   Fixed: ${fixed}`);
  console.log(`   Errors: ${errors}`);
} else {
  console.log(`   Would fix: ${processed}`);
}
console.log('═'.repeat(65) + '\n');
