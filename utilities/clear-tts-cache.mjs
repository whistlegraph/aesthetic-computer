#!/usr/bin/env node
// Clear TTS Cache - Removes all cached TTS audio from Digital Ocean Spaces
// Usage: node utilities/clear-tts-cache.mjs [--dry-run]
//
// Requires environment variables:
//   ART_ENDPOINT - Digital Ocean Spaces endpoint (e.g., nyc3.digitaloceanspaces.com)
//   ART_KEY - Access key ID
//   ART_SECRET - Secret access key
//   ART_SPACE_NAME - Bucket name

import { S3Client, ListObjectsV2Command, DeleteObjectsCommand } from "@aws-sdk/client-s3";

const CACHE_PREFIX = "tts-cache/";
const BATCH_SIZE = 1000; // Max objects per delete request

// Check for required env vars
const requiredEnvVars = ["ART_ENDPOINT", "ART_KEY", "ART_SECRET", "ART_SPACE_NAME"];
const missingEnvVars = requiredEnvVars.filter(v => !process.env[v]);

if (missingEnvVars.length > 0) {
  console.error("❌ Missing required environment variables:");
  missingEnvVars.forEach(v => console.error(`   - ${v}`));
  console.error("\nMake sure you have a .env file or export these variables.");
  process.exit(1);
}

const dryRun = process.argv.includes("--dry-run");

const s3 = new S3Client({
  endpoint: `https://${process.env.ART_ENDPOINT}`,
  region: "us-east-1",
  credentials: {
    accessKeyId: process.env.ART_KEY,
    secretAccessKey: process.env.ART_SECRET,
  },
});

const BUCKET = process.env.ART_SPACE_NAME;

async function listAllCachedFiles() {
  const files = [];
  let continuationToken;

  do {
    const command = new ListObjectsV2Command({
      Bucket: BUCKET,
      Prefix: CACHE_PREFIX,
      ContinuationToken: continuationToken,
    });

    const response = await s3.send(command);
    
    if (response.Contents) {
      files.push(...response.Contents);
    }
    
    continuationToken = response.IsTruncated ? response.NextContinuationToken : undefined;
  } while (continuationToken);

  return files;
}

async function deleteFiles(files) {
  if (files.length === 0) return 0;

  let deleted = 0;
  
  // Process in batches
  for (let i = 0; i < files.length; i += BATCH_SIZE) {
    const batch = files.slice(i, i + BATCH_SIZE);
    
    const command = new DeleteObjectsCommand({
      Bucket: BUCKET,
      Delete: {
        Objects: batch.map(f => ({ Key: f.Key })),
        Quiet: true,
      },
    });

    await s3.send(command);
    deleted += batch.length;
    
    console.log(`  🗑️  Deleted ${deleted}/${files.length} files...`);
  }

  return deleted;
}

async function main() {
  console.log("🔍 Scanning TTS cache in Digital Ocean Spaces...");
  console.log(`   Bucket: ${BUCKET}`);
  console.log(`   Prefix: ${CACHE_PREFIX}`);
  
  if (dryRun) {
    console.log("\n⚠️  DRY RUN MODE - No files will be deleted\n");
  }

  try {
    const files = await listAllCachedFiles();
    
    if (files.length === 0) {
      console.log("\n✅ TTS cache is already empty!");
      return;
    }

    // Calculate total size
    const totalSize = files.reduce((sum, f) => sum + (f.Size || 0), 0);
    const sizeMB = (totalSize / 1024 / 1024).toFixed(2);

    console.log(`\n📊 Found ${files.length} cached TTS files (${sizeMB} MB)`);
    
    // Show some sample files
    console.log("\n📋 Sample files:");
    files.slice(0, 5).forEach(f => {
      console.log(`   - ${f.Key} (${(f.Size / 1024).toFixed(1)} KB)`);
    });
    if (files.length > 5) {
      console.log(`   ... and ${files.length - 5} more`);
    }

    if (dryRun) {
      console.log("\n✅ Dry run complete. Run without --dry-run to delete files.");
      return;
    }

    console.log("\n🗑️  Deleting all cached TTS files...");
    const deleted = await deleteFiles(files);
    
    console.log(`\n✅ Successfully deleted ${deleted} TTS cache files (${sizeMB} MB freed)`);
    
  } catch (err) {
    console.error("\n❌ Error:", err.message);
    process.exit(1);
  }
}

main();
