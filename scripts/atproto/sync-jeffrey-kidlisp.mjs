#!/usr/bin/env node
// Sync missing kidlisp records for @jeffrey to ATProto

import { connect } from "../../system/backend/database.mjs";
import { createMediaRecord, MediaTypes } from "../../system/backend/media-atproto.mjs";

const isDryRun = process.argv[2] !== 'live';

async function main() {
  console.log(`\n🔄 SYNC @JEFFREY KIDLISP TO ATPROTO`);
  console.log(`   Mode: ${isDryRun ? '🔍 DRY RUN' : '⚡ LIVE'}\n`);
  
  const database = await connect();
  
  // Get @jeffrey user
  const jeffreyUser = await database.db.collection('users').findOne({'atproto.handle': 'jeffrey.at.aesthetic.computer'});
  if (!jeffreyUser) {
    console.error('❌ @jeffrey user not found');
    await database.disconnect();
    process.exit(1);
  }
  
  // Find kidlisp missing rkey
  const kidlispCollection = database.db.collection('kidlisp');
  const missing = await kidlispCollection.find({
    user: jeffreyUser._id,
    $or: [
      { 'atproto.rkey': { $exists: false } },
      { 'atproto.rkey': null }
    ]
  }).toArray();
  
  console.log(`📝 Found ${missing.length} kidlisp records missing ATProto records`);
  
  if (missing.length === 0) {
    console.log('✅ All kidlisp already synced!');
    await database.disconnect();
    process.exit(0);
  }
  
  if (isDryRun) {
    console.log(`\n🔍 DRY RUN - Would create ATProto records for:`);
    missing.forEach(k => {
      console.log(`  - ${k.code}: "${k.source?.substring(0, 50)}..."`);
    });
    console.log(`\n💡 Run with 'live' argument to apply changes`);
    await database.disconnect();
    process.exit(0);
  }
  
  // Create ATProto records
  let created = 0;
  let failed = 0;
  
  for (const kidlisp of missing) {
    try {
      const result = await createMediaRecord(
        database,
        MediaTypes.KIDLISP,
        kidlisp,
        { userSub: jeffreyUser._id }
      );
      
      if (result.error) {
        console.log(`⚠️  Failed ${kidlisp.code}: ${result.error}`);
        failed++;
      } else {
        console.log(`✅ Created ${kidlisp.code} → rkey: ${result.rkey}`);
        created++;
      }
    } catch (error) {
      console.error(`❌ Error creating ${kidlisp.code}:`, error.message);
      failed++;
    }
  }
  
  console.log(`\n📊 Results:`);
  console.log(`  ✅ Created: ${created}`);
  console.log(`  ❌ Failed: ${failed}`);
  
  await database.disconnect();
  process.exit(0);
}

main().catch(err => {
  console.error('❌ Error:', err);
  process.exit(1);
});
