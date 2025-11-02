#!/usr/bin/env node
// Quick backfill script for missing kidlisp ATProto rkeys
// Only checks @jeffrey and art-guest accounts

import { connect } from "../../system/backend/database.mjs";
import { AtpAgent } from "@atproto/api";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";
const isDryRun = process.argv[2] !== 'live';

async function backfillAccount(handle, label, mongoQuery) {
  console.log(`\n=== ${label} (${handle}) ===`);
  
  const database = await connect();
  
  // Get user
  const user = await database.db.collection('users').findOne({'atproto.handle': handle});
  if (!user?.atproto?.did || !user?.atproto?.password) {
    console.log('❌ No ATProto credentials found');
    await database.disconnect();
    return;
  }
  
  // Find kidlisp records missing rkey
  const kidlispCollection = database.db.collection('kidlisp');
  const missingRkey = await kidlispCollection.find({
    ...mongoQuery,
    $or: [
      { 'atproto.rkey': { $exists: false } },
      { 'atproto.rkey': null }
    ]
  }).toArray();
  
  console.log(`📝 Found ${missingRkey.length} kidlisp records missing atproto.rkey`);
  
  if (missingRkey.length === 0) {
    await database.disconnect();
    return;
  }
  
  if (isDryRun) {
    console.log(`🔍 DRY RUN - Would backfill ${missingRkey.length} records`);
    missingRkey.slice(0, 5).forEach(k => {
      console.log(`  - ${k.code || k._id}: "${k.source?.substring(0, 50)}..."`);
    });
    if (missingRkey.length > 5) {
      console.log(`  ... and ${missingRkey.length - 5} more`);
    }
    await database.disconnect();
    return;
  }
  
  // Login to ATProto
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ identifier: user.atproto.did, password: user.atproto.password });
  
  // Get all ATProto records
  console.log(`📥 Fetching ATProto records...`);
  let atprotoRecords = [];
  let cursor;
  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: user.atproto.did,
      collection: 'computer.aesthetic.kidlisp',
      limit: 100,
      cursor
    });
    atprotoRecords.push(...response.data.records);
    cursor = response.data.cursor;
  } while (cursor);
  
  console.log(`📊 Found ${atprotoRecords.length} ATProto records`);
  
  // Create lookup map by MongoDB _id (stored as ref in ATProto)
  const atprotoByRef = new Map();
  atprotoRecords.forEach(record => {
    if (record.value.ref) {
      atprotoByRef.set(record.value.ref, record.uri.split('/').pop());
    }
  });
  
  // Match and update
  let matched = 0;
  let notFound = 0;
  
  for (const kidlisp of missingRkey) {
    const mongoIdStr = kidlisp._id.toString();
    const rkey = atprotoByRef.get(mongoIdStr);
    
    if (rkey) {
      await kidlispCollection.updateOne(
        { _id: kidlisp._id },
        { $set: { 'atproto.rkey': rkey } }
      );
      matched++;
      console.log(`✅ Matched ${kidlisp.code || mongoIdStr} → rkey: ${rkey}`);
    } else {
      notFound++;
      console.log(`⚠️  No ATProto record found for ${kidlisp.code || mongoIdStr}`);
    }
  }
  
  console.log(`\n📊 Results:`);
  console.log(`  ✅ Matched and updated: ${matched}`);
  console.log(`  ⚠️  Not found in ATProto: ${notFound}`);
  
  await database.disconnect();
}

async function main() {
  console.log(`\n🔄 KIDLISP RKEY BACKFILL`);
  console.log(`   Mode: ${isDryRun ? '🔍 DRY RUN' : '⚡ LIVE'}\n`);
  
  // Get @jeffrey user ID first
  const database = await connect();
  const jeffreyUser = await database.db.collection('users').findOne({'atproto.handle': 'jeffrey.at.aesthetic.computer'});
  await database.disconnect();
  
  if (jeffreyUser) {
    await backfillAccount(
      'jeffrey.at.aesthetic.computer',
      '@jeffrey',
      { user: jeffreyUser._id }
    );
  }
  
  // Art-guest (anonymous)
  await backfillAccount(
    'art.at.aesthetic.computer',
    'art-guest',
    { user: { $exists: false } }
  );
  
  // @fifi
  const database2 = await connect();
  const fifiUser = await database2.db.collection('users').findOne({'atproto.handle': 'fifi.at.aesthetic.computer'});
  await database2.disconnect();
  
  if (fifiUser) {
    await backfillAccount(
      'fifi.at.aesthetic.computer',
      '@fifi',
      { user: fifiUser._id }
    );
  }
  
  if (isDryRun) {
    console.log(`\n💡 Run with 'live' argument to apply changes`);
  }
  
  process.exit(0);
}

main().catch(err => {
  console.error('❌ Error:', err);
  process.exit(1);
});
