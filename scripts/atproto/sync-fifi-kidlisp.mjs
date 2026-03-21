#!/usr/bin/env node
// Sync all kidlisp records for @fifi to ATProto

import { connect } from "../../system/backend/database.mjs";
import { createMediaRecord, MediaTypes } from "../../system/backend/media-atproto.mjs";

const isDryRun = process.argv[2] !== 'live';
const BATCH_SIZE = 10; // Process in batches to avoid overwhelming the PDS

async function main() {
  console.log(`\n🔄 SYNC @FIFI KIDLISP TO ATPROTO`);
  console.log(`   Mode: ${isDryRun ? '🔍 DRY RUN' : '⚡ LIVE'}\n`);
  
  const database = await connect();
  
  // Get @fifi user
  const fifiUser = await database.db.collection('users').findOne({'atproto.handle': 'fifi.at.aesthetic.computer'});
  if (!fifiUser) {
    console.error('❌ @fifi user not found');
    await database.disconnect();
    process.exit(1);
  }
  
  console.log(`👤 User: ${fifiUser.atproto.handle} (${fifiUser.atproto.did})`);
  
  // Find kidlisp missing rkey
  const kidlispCollection = database.db.collection('kidlisp');
  const missing = await kidlispCollection.find({
    user: fifiUser._id,
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
    console.log(`\n🔍 DRY RUN - Would create ATProto records for ${missing.length} kidlisp`);
    console.log(`   Processing in batches of ${BATCH_SIZE}`);
    console.log(`   Estimated time: ~${Math.ceil(missing.length / BATCH_SIZE)} batches`);
    console.log(`\nFirst 10 samples:`);
    missing.slice(0, 10).forEach(k => {
      console.log(`  - ${k.code}: "${k.source?.substring(0, 50)}..."`);
    });
    if (missing.length > 10) {
      console.log(`  ... and ${missing.length - 10} more`);
    }
    console.log(`\n💡 Run with 'live' argument to apply changes`);
    await database.disconnect();
    process.exit(0);
  }
  
  // Create ATProto records in batches
  let created = 0;
  let failed = 0;
  const total = missing.length;
  
  for (let i = 0; i < missing.length; i += BATCH_SIZE) {
    const batch = missing.slice(i, i + BATCH_SIZE);
    const batchNum = Math.floor(i / BATCH_SIZE) + 1;
    const totalBatches = Math.ceil(missing.length / BATCH_SIZE);
    
    console.log(`\n📦 Batch ${batchNum}/${totalBatches} (${i + 1}-${Math.min(i + BATCH_SIZE, total)} of ${total})`);
    
    const results = await Promise.all(
      batch.map(async (kidlisp) => {
        try {
          const result = await createMediaRecord(
            database,
            MediaTypes.KIDLISP,
            kidlisp,
            { userSub: fifiUser._id }
          );
          
          if (result.error) {
            return { code: kidlisp.code, success: false, error: result.error };
          } else {
            return { code: kidlisp.code, success: true, rkey: result.rkey };
          }
        } catch (error) {
          return { code: kidlisp.code, success: false, error: error.message };
        }
      })
    );
    
    // Report batch results
    results.forEach(r => {
      if (r.success) {
        console.log(`  ✅ ${r.code} → ${r.rkey}`);
        created++;
      } else {
        console.log(`  ⚠️  ${r.code}: ${r.error}`);
        failed++;
      }
    });
    
    console.log(`  Progress: ${created + failed}/${total} (${created} ✅, ${failed} ❌)`);
  }
  
  console.log(`\n📊 Final Results:`);
  console.log(`  ✅ Created: ${created}`);
  console.log(`  ❌ Failed: ${failed}`);
  console.log(`  📈 Success rate: ${((created / total) * 100).toFixed(1)}%`);
  
  await database.disconnect();
  process.exit(0);
}

main().catch(err => {
  console.error('❌ Error:', err);
  process.exit(1);
});
