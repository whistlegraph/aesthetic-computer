#!/usr/bin/env node
// Check for duplicate moods on ATProto by ref

import { connect } from '../../system/backend/database.mjs';
import { userIDFromHandle } from '../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';

const handle = process.argv[2] || 'jeffrey';
const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const users = database.db.collection('users');

const user = await users.findOne({ _id: sub });
const { did, password } = user.atproto;

// Login to ATProto
const agent = new AtpAgent({ service: PDS_URL });
await agent.login({ identifier: did, password });

// Fetch ALL ATProto moods
console.log(`\n🔍 Fetching all ATProto moods for @${handle}...`);
const allMoods = [];
let cursor;

do {
  const response = await agent.com.atproto.repo.listRecords({
    repo: did,
    collection: 'computer.aesthetic.mood',
    limit: 100,
    cursor,
  });
  
  allMoods.push(...response.data.records);
  cursor = response.data.cursor;
  console.log(`   Fetched ${response.data.records.length} (total: ${allMoods.length})`);
} while (cursor);

console.log(`\n📊 Total ATProto moods: ${allMoods.length}`);

// Check for duplicates by ref (or fallback to mongoId for old records)
const refCounts = new Map();

for (const record of allMoods) {
  const ref = record.value.ref || record.value.mongoId;
  if (ref) {
    refCounts.set(ref, (refCounts.get(ref) || 0) + 1);
  }
}

const duplicates = Array.from(refCounts.entries()).filter(([_, count]) => count > 1);

console.log(`\n🔍 Duplicate Analysis:`);
console.log(`   Unique refs: ${refCounts.size}`);
console.log(`   Duplicates: ${duplicates.length}\n`);

if (duplicates.length > 0) {
  console.log(`⚠️  Found ${duplicates.length} refs with multiple ATProto records:\n`);
  
  // Show first 10 duplicates
  for (const [ref, count] of duplicates.slice(0, 10)) {
    console.log(`   ${ref}: ${count} copies`);
    
    // Find the records with this ref
    const dupes = allMoods.filter(r => (r.value.ref || r.value.mongoId) === ref);
    dupes.forEach((r, i) => {
      const rkey = r.uri.split('/').pop();
      console.log(`      [${i+1}] rkey: ${rkey} | mood: ${r.value.mood.substring(0, 40)}`);
    });
    console.log('');
  }
  
  if (duplicates.length > 10) {
    console.log(`   ... and ${duplicates.length - 10} more\n`);
  }
  
  console.log(`💡 Total duplicate records to clean: ${allMoods.length - refCounts.size}\n`);
} else {
  console.log(`✅ No duplicates found!\n`);
}

await database.disconnect();
process.exit(0);
