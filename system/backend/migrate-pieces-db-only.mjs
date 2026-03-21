#!/usr/bin/env node
// Migrate Piece Paths - Database Only
// Updates database records without touching S3

import { connect } from "./database.mjs";

async function migrate() {
  console.log('🔧 Starting database-only piece migration...\n');

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
    const piecesToMigrate = await collection.find({
      anonymous: true,
      slug: /^\d{4}\/\d{2}\/\d{2}\//,  // Matches "YYYY/MM/DD/..." pattern
      bucket: "art-aesthetic-computer"
    }).toArray();

    console.log(`📦 Found ${piecesToMigrate.length} pieces to migrate\n`);

    if (piecesToMigrate.length === 0) {
      console.log('✅ No pieces need migration');
      await database.disconnect();
      process.exit(0);
    }

    const results = {
      total: piecesToMigrate.length,
      migrated: 0,
      errors: []
    };

    for (const piece of piecesToMigrate) {
      try {
        const oldSlug = piece.slug;
        const newSlug = piece.code;  // Just use the code!

        console.log(`🔄 Updating ${piece.code}: ${oldSlug} → ${newSlug}`);

        // Update database record with new slug and add missing fields
        await collection.updateOne(
          { _id: piece._id },
          {
            $set: {
              slug: newSlug,
              migratedAt: new Date(),
              oldSlug: oldSlug,  // Keep reference to old path
              type: "piece",      // Add if missing
              extension: ".mjs",  // Add if missing
              needsS3Copy: true   // Flag for S3 migration
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
    console.log(`   ❌ Errors: ${results.errors.length}`);

    if (results.errors.length > 0) {
      console.log('\n⚠️  Failed migrations:');
      results.errors.forEach(err => {
        console.log(`   - ${err.code}: ${err.error}`);
      });
    }

    console.log('\n💡 Note: S3 files still need to be copied.');
    console.log('   Run the /migrate-piece-paths Netlify function to copy S3 files.');
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
