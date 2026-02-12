#!/usr/bin/env node
// Migrate Piece Paths - Standalone Script
// Run with: node system/backend/migrate-pieces.mjs

import { connect } from "./database.mjs";
import { S3Client, CopyObjectCommand, PutObjectAclCommand } from "@aws-sdk/client-s3";

async function migrate() {
  console.log('🔧 Starting piece path migration...\n');

  let database;
  try {
    database = await connect();
    console.log('✅ Connected to database\n');
  } catch (connectError) {
    console.error('❌ MongoDB connection failed:', connectError.message);
    process.exit(1);
  }

  try {
    const collection = database.db.collection('pieces');

    // Find all anonymous pieces with date-based slugs
    // Note: Some pieces may not have type/extension fields if published before those were added
    const piecesToMigrate = await collection.find({
      anonymous: true,
      slug: /^\d{4}\/\d{2}\/\d{2}\//,  // Matches "YYYY/MM/DD/..." pattern
      bucket: "art-aesthetic-computer"  // Anonymous pieces bucket
    }).toArray();

    console.log(`📦 Found ${piecesToMigrate.length} pieces to migrate\n`);

    if (piecesToMigrate.length === 0) {
      console.log('✅ No pieces need migration');
      await database.disconnect();
      process.exit(0);
    }

    const s3Client = new S3Client({
      endpoint: `https://sfo3.digitaloceanspaces.com`,
      region: 'us-east-1',
      credentials: {
        accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
        secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
      },
    });

    const results = {
      total: piecesToMigrate.length,
      migrated: 0,
      skipped: 0,
      errors: []
    };

    for (const piece of piecesToMigrate) {
      try {
        const oldSlug = piece.slug;
        const newSlug = piece.code;  // Just use the code!
        const bucket = piece.bucket || "art-aesthetic-computer";

        console.log(`🔄 Migrating ${piece.code}: ${oldSlug} → ${newSlug}`);

        // Copy S3 file from old path to new path
        const copyCommand = new CopyObjectCommand({
          Bucket: bucket,
          Key: `${newSlug}.mjs`,
          CopySource: `${bucket}/${oldSlug}.mjs`,
          ContentType: 'text/javascript',
        });

        await s3Client.send(copyCommand);
        console.log(`   ✅ Copied S3: ${oldSlug}.mjs → ${newSlug}.mjs`);

        // Make the new file publicly readable
        const aclCommand = new PutObjectAclCommand({
          Bucket: bucket,
          Key: `${newSlug}.mjs`,
          ACL: "public-read",
        });

        await s3Client.send(aclCommand);

        // Update database record with new slug and add missing fields
        await collection.updateOne(
          { _id: piece._id },
          {
            $set: {
              slug: newSlug,
              migratedAt: new Date(),
              oldSlug: oldSlug,  // Keep reference to old path
              type: "piece",      // Add if missing
              extension: ".mjs"   // Add if missing
            }
          }
        );

        console.log(`   💾 Updated database for ${piece.code}\n`);
        results.migrated++;

      } catch (error) {
        console.error(`   ❌ Failed to migrate ${piece.code}:`, error.message);
        results.errors.push({
          code: piece.code,
          error: error.message
        });
      }
    }

    console.log('\n' + '='.repeat(50));
    console.log('📊 Migration Results:');
    console.log(`   Total: ${results.total}`);
    console.log(`   ✅ Migrated: ${results.migrated}`);
    console.log(`   ⏭️  Skipped: ${results.skipped}`);
    console.log(`   ❌ Errors: ${results.errors.length}`);

    if (results.errors.length > 0) {
      console.log('\n⚠️  Failed migrations:');
      results.errors.forEach(err => {
        console.log(`   - ${err.code}: ${err.error}`);
      });
    }

    console.log('='.repeat(50));

    await database.disconnect();
    process.exit(results.errors.length > 0 ? 1 : 0);

  } catch (error) {
    console.error('❌ Migration error:', error);
    await database.disconnect();
    process.exit(1);
  }
}

migrate();
