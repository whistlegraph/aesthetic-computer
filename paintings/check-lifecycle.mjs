#!/usr/bin/env node

/**
 * Check Bucket Lifecycle Policies
 * 
 * Inspect Digital Ocean Spaces bucket lifecycle rules
 * to see if guest uploads have expiration policies.
 */

import { S3Client, GetBucketLifecycleConfigurationCommand } from '@aws-sdk/client-s3';
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

async function checkLifecyclePolicy(bucketName) {
  console.log(`\n🔍 Checking lifecycle policy for bucket: ${bucketName}\n`);
  
  try {
    const command = new GetBucketLifecycleConfigurationCommand({
      Bucket: bucketName
    });
    
    const response = await client.send(command);
    
    if (response.Rules && response.Rules.length > 0) {
      console.log(`✅ Found ${response.Rules.length} lifecycle rule(s):\n`);
      
      response.Rules.forEach((rule, i) => {
        console.log(`Rule ${i + 1}: ${rule.ID || '(unnamed)'}`);
        console.log(`  Status: ${rule.Status}`);
        
        if (rule.Prefix) {
          console.log(`  Prefix: ${rule.Prefix}`);
        }
        
        if (rule.Expiration) {
          if (rule.Expiration.Days) {
            console.log(`  ⏰ Expires after: ${rule.Expiration.Days} days`);
          }
          if (rule.Expiration.Date) {
            console.log(`  ⏰ Expires on: ${rule.Expiration.Date}`);
          }
          if (rule.Expiration.ExpiredObjectDeleteMarker) {
            console.log(`  🗑️  Delete expired object markers: ${rule.Expiration.ExpiredObjectDeleteMarker}`);
          }
        } else {
          console.log(`  ♾️  No expiration set (files are permanent)`);
        }
        
        if (rule.NoncurrentVersionExpiration) {
          console.log(`  📦 Non-current versions expire after: ${rule.NoncurrentVersionExpiration.NoncurrentDays} days`);
        }
        
        if (rule.AbortIncompleteMultipartUpload) {
          console.log(`  🔄 Abort incomplete uploads after: ${rule.AbortIncompleteMultipartUpload.DaysAfterInitiation} days`);
        }
        
        console.log();
      });
    } else {
      console.log(`✅ No lifecycle rules configured`);
      console.log(`   → Files are permanent (never expire)\n`);
    }
  } catch (error) {
    if (error.name === 'NoSuchLifecycleConfiguration') {
      console.log(`✅ No lifecycle configuration found`);
      console.log(`   → Files are permanent (never expire)\n`);
    } else {
      console.error(`❌ Error: ${error.message}\n`);
      throw error;
    }
  }
}

async function main() {
  console.log(`
╔═══════════════════════════════════════════════════════════════╗
║           🪣 Bucket Lifecycle Policy Inspector                ║
╚═══════════════════════════════════════════════════════════════╝
`);

  if (!process.env.DO_SPACES_KEY || !process.env.DO_SPACES_SECRET) {
    console.error('❌ Error: Missing Digital Ocean credentials');
    console.error('\nPlease set in .env or environment:');
    console.error('  DO_SPACES_KEY=your-key');
    console.error('  DO_SPACES_SECRET=your-secret\n');
    process.exit(1);
  }

  const buckets = [
    process.env.ART_SPACE_NAME || 'art',
    process.env.USER_SPACE_NAME || 'aesthetic-computer',
    process.env.WAND_SPACE_NAME || 'wand'
  ];

  console.log(`📦 Checking buckets: ${buckets.join(', ')}\n`);
  console.log('─'.repeat(65));

  for (const bucket of buckets) {
    await checkLifecyclePolicy(bucket);
    console.log('─'.repeat(65));
  }

  console.log(`
💡 Summary:

If a bucket shows "No lifecycle rules" or "Files are permanent",
then guest uploads DO NOT expire automatically.

To set expiration (e.g., 24 hours for guest bucket):
  1. Go to Digital Ocean Spaces console
  2. Select bucket → Settings → Lifecycle
  3. Add rule: Expire objects after 1 day

To keep files permanent (recommended for art bucket now):
  → No action needed if no lifecycle rules exist
  → Or remove any existing expiration rules
`);
}

main().catch(error => {
  console.error('\n💥 Fatal error:', error.message);
  process.exit(1);
});
