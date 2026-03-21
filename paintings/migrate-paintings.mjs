#!/usr/bin/env node
// migrate-paintings.mjs
// Backfill short codes for existing paintings (user and guest)

import 'dotenv/config';
import { generateCode } from './generate-code.mjs';
import { S3Client, ListObjectsV2Command } from '@aws-sdk/client-s3';
import { MongoClient } from 'mongodb';

const DRY_RUN = process.argv.includes('--dry-run');

// MongoDB connection
async function getMongoClient() {
  const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING);
  await client.connect();
  const db = client.db(process.env.MONGODB_NAME);
  return { client, db };
}

// List S3 bucket contents (all pages)
async function listS3Paintings(bucketName) {
  const s3Client = new S3Client({
    region: process.env.DO_SPACES_REGION || 'sfo3',
    endpoint: process.env.DO_SPACES_ENDPOINT,
    credentials: {
      accessKeyId: process.env.DO_SPACES_KEY,
      secretAccessKey: process.env.DO_SPACES_SECRET,
    },
  });

  const allFiles = [];
  let continuationToken = null;

  do {
    const command = new ListObjectsV2Command({
      Bucket: bucketName,
      MaxKeys: 1000,
      ContinuationToken: continuationToken,
    });

    const response = await s3Client.send(command);
    const pngFiles = (response.Contents || []).filter(f => f.Key.endsWith('.png'));
    allFiles.push(...pngFiles);
    
    continuationToken = response.NextContinuationToken;
    
    if (allFiles.length % 5000 === 0 && allFiles.length > 0) {
      console.log(`   ... ${allFiles.length} PNG files found so far...`);
    }
  } while (continuationToken);

  return allFiles;
}

async function migrate() {
  console.log('╔═══════════════════════════════════════════════════════════════╗');
  console.log('║           🎨 Painting Short Codes Migration                  ║');
  console.log('╚═══════════════════════════════════════════════════════════════╝\n');
  
  if (DRY_RUN) {
    console.log('🔍 DRY RUN MODE - No changes will be made\n');
  }
  
  // Phase 1: Check existing user paintings in MongoDB
  console.log('📊 Phase 1: Checking user paintings in MongoDB...');
  
  const { client, db } = await getMongoClient();
  const paintings = db.collection('paintings');
  
  const totalPaintings = await paintings.countDocuments();
  const paintingsWithCodes = await paintings.countDocuments({ code: { $exists: true } });
  const paintingsNeedingCodes = totalPaintings - paintingsWithCodes;
  
  console.log(`\n✓ Database statistics:`);
  console.log(`  Total paintings: ${totalPaintings}`);
  console.log(`  With codes: ${paintingsWithCodes}`);
  console.log(`  Need codes: ${paintingsNeedingCodes}`);
  
  if (paintingsNeedingCodes > 0) {
    console.log(`\n📝 Sample paintings needing codes (first 5):`);
    const samples = await paintings.find({ code: { $exists: false } })
      .limit(5)
      .toArray();
    
    samples.forEach(p => {
      const date = new Date(p.when).toISOString().split('T')[0];
      const userId = p.user?.substring(0, 12) || 'none';
      console.log(`  ${p.slug.padEnd(15)} | ${date} | user: ${userId}`);
    });
  }
  
  // Phase 2: Check guest paintings in S3
  console.log('\n📦 Phase 2: Checking guest paintings in S3...');
  console.log('   (This may take a moment to list all files...)');
  
  const guestFiles = await listS3Paintings(process.env.ART_SPACE_NAME);
  
  console.log(`\n✓ Found ${guestFiles.length} guest painting files in S3`);
  
  // Show just summary statistics
  if (guestFiles.length > 0) {
    const totalSize = guestFiles.reduce((sum, f) => sum + f.Size, 0);
    const avgSize = totalSize / guestFiles.length;
    const oldestFile = guestFiles.reduce((oldest, f) => 
      f.LastModified < oldest.LastModified ? f : oldest, guestFiles[0]);
    const newestFile = guestFiles.reduce((newest, f) => 
      f.LastModified > newest.LastModified ? f : newest, guestFiles[0]);
    
    console.log(`\n  Guest paintings summary:`);
    console.log(`    Total files: ${guestFiles.length}`);
    console.log(`    Total size: ${(totalSize / 1024 / 1024).toFixed(2)} MB`);
    console.log(`    Average size: ${(avgSize / 1024).toFixed(1)} KB`);
    console.log(`    Date range: ${oldestFile.LastModified.toISOString().split('T')[0]} to ${newestFile.LastModified.toISOString().split('T')[0]}`);
    
    console.log(`\n  Sample guest paintings (first 5):`);
    guestFiles.slice(0, 5).forEach(file => {
      const slug = file.Key.replace('.png', '');
      const date = file.LastModified.toISOString().split('T')[0];
      const size = (file.Size / 1024).toFixed(1);
      console.log(`    ${slug.padEnd(12)} | ${date} | ${size.padStart(6)} KB`);
    });
  }
  
  // Summary
  console.log('\n─────────────────────────────────────────────────────────────────');
  console.log('\n📊 Migration Summary:\n');
  console.log(`  User paintings needing codes: ${paintingsNeedingCodes}`);
  console.log(`  Guest paintings to import: ${guestFiles.length}`);
  console.log(`  Total operations needed: ${paintingsNeedingCodes + guestFiles.length}`);
  
  if (DRY_RUN) {
    console.log('\n💡 This was a dry run. To execute migration, run:');
    console.log('   npm run migrate\n');
    await client.close();
    return;
  }
  
  // ACTUAL MIGRATION
  console.log('\n🚀 Starting migration...\n');
  
  // Track existing codes to avoid collisions
  const existingCodes = new Set();
  const existingCodesArray = await paintings.find({ code: { $exists: true } })
    .project({ code: 1 })
    .toArray();
  existingCodesArray.forEach(p => existingCodes.add(p.code));
  console.log(`📋 Loaded ${existingCodes.size} existing codes for collision detection`);
  
  // Phase 3: Update user paintings
  console.log('\n📝 Phase 3: Adding codes to user paintings...');
  const userPaintings = await paintings.find({ code: { $exists: false }, user: { $exists: true } })
    .toArray();
  
  let userUpdated = 0;
  for (const painting of userPaintings) {
    const code = generateCode(existingCodes);
    await paintings.updateOne(
      { _id: painting._id },
      { 
        $set: { 
          code,
          bucket: "user-aesthetic-computer"
        } 
      }
    );
    userUpdated++;
    if (userUpdated % 100 === 0) {
      console.log(`   ... ${userUpdated}/${userPaintings.length} user paintings updated`);
    }
  }
  console.log(`✅ Updated ${userUpdated} user paintings with codes`);
  
  // Phase 4: Import guest paintings
  console.log('\n📦 Phase 4: Importing guest paintings to database...');
  
  let guestImported = 0;
  let guestSkipped = 0;
  
  for (const file of guestFiles) {
    const slug = file.Key.replace('.png', '');
    
    // Check if already exists
    const existing = await paintings.findOne({ slug, user: { $exists: false } });
    if (existing) {
      guestSkipped++;
      continue;
    }
    
    const code = generateCode(existingCodes);
    
    try {
      await paintings.insertOne({
        code,
        slug,
        // user: undefined,  // Don't set user field for guests
        when: file.LastModified,
        nuked: false,
        bucket: "art-aesthetic-computer"
      });
      guestImported++;
      
      if (guestImported % 100 === 0) {
        console.log(`   ... ${guestImported}/${guestFiles.length} guest paintings imported`);
      }
    } catch (error) {
      console.error(`   ⚠️  Failed to import ${slug}:`, error.message);
    }
  }
  
  console.log(`✅ Imported ${guestImported} guest paintings`);
  if (guestSkipped > 0) {
    console.log(`ℹ️  Skipped ${guestSkipped} guest paintings (already in database)`);
  }
  
  // Final summary
  console.log('\n─────────────────────────────────────────────────────────────────');
  console.log('\n🎉 Migration Complete!\n');
  console.log(`  User paintings with codes: ${userUpdated}`);
  console.log(`  Guest paintings imported: ${guestImported}`);
  console.log(`  Total codes generated: ${userUpdated + guestImported}`);
  console.log(`  Total codes in database: ${existingCodes.size + userUpdated + guestImported}`);
  console.log('\n📊 Verification:');
  const finalCount = await paintings.countDocuments({ code: { $exists: true } });
  console.log(`  Paintings with codes: ${finalCount}`);
  console.log('\n✨ Ready for Component 4: API endpoint implementation\n');
  
  await client.close();
}

migrate().catch(console.error);
