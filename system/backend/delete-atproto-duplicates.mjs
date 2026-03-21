#!/usr/bin/env node
// Delete duplicate ATProto mood records, keeping only the ones MongoDB references

import { connect } from '../../system/backend/database.mjs';
import { userIDFromHandle } from '../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';
import { ObjectId } from 'mongodb';

const handle = process.argv[2];
const dryRun = !process.argv.includes('--delete');

if (!handle) {
  console.error('Usage: node delete-atproto-duplicates.mjs <handle> [--delete]');
  console.error('Example: node delete-atproto-duplicates.mjs jeffrey --delete');
  process.exit(1);
}

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

console.log('='.repeat(80));
console.log(`🧹 CLEANING DUPLICATE ATPROTO MOODS FOR @${handle}`);
if (dryRun) {
  console.log('   [DRY RUN MODE - use --delete to actually delete]');
}
console.log('='.repeat(80));

const database = await connect();
const sub = await userIDFromHandle(handle, database);
const users = database.db.collection('users');
const moods = database.db.collection('moods');

// Get user's ATProto credentials
const user = await users.findOne({ _id: sub });
const { did, password } = user.atproto;

// Login to ATProto
console.log('\n📋 Step 1: Logging into ATProto...');
const agent = new AtpAgent({ service: PDS_URL });
await agent.login({ identifier: did, password });
console.log('   ✅ Logged in');

// Fetch ALL ATProto moods
console.log('\n📋 Step 2: Fetching all ATProto moods...');
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
} while (cursor);

console.log(`   Total ATProto moods: ${allMoods.length}`);

// Build ref -> rkeys map (support both old mongoId and new ref)
console.log('\n📋 Step 3: Analyzing duplicates...');
const refToRkeys = new Map();

for (const record of allMoods) {
  const ref = record.value.ref || record.value.mongoId; // Support both
  const rkey = record.uri.split('/').pop();
  
  if (!refToRkeys.has(ref)) {
    refToRkeys.set(ref, []);
  }
  refToRkeys.get(ref).push({ rkey, record });
}

// Find duplicates
const duplicates = Array.from(refToRkeys.entries()).filter(([_, rkeys]) => rkeys.length > 1);
console.log(`   Found ${duplicates.length} refs with duplicates`);

// Get MongoDB's preferred rkeys
console.log('\n📋 Step 4: Getting MongoDB references...');
const mongoRkeys = new Set();

for (const [ref, _] of duplicates) {
  try {
    const mood = await moods.findOne({ _id: new ObjectId(ref) });
    if (mood?.atproto?.rkey) {
      mongoRkeys.add(mood.atproto.rkey);
    }
  } catch (e) {
    console.log(`   ⚠️  Could not find MongoDB record for ${ref}`);
  }
}

console.log(`   MongoDB references ${mongoRkeys.size} rkeys`);

// Identify records to delete
console.log('\n📋 Step 5: Identifying records to delete...');
const toDelete = [];

for (const [ref, rkeys] of duplicates) {
  // Keep the one MongoDB references, delete the rest
  for (const { rkey, record } of rkeys) {
    if (!mongoRkeys.has(rkey)) {
      toDelete.push({ ref, rkey, mood: record.value.mood.substring(0, 40) });
    }
  }
}

console.log(`   ${toDelete.length} duplicate records to delete`);

// Delete duplicates
if (toDelete.length > 0) {
  console.log('\n📋 Step 6: Deleting duplicates...');
  
  let deleteCount = 0;
  
  for (const { ref, rkey, mood } of toDelete) {
    try {
      if (dryRun) {
        console.log(`   [DRY RUN] Would delete rkey: ${rkey} | ${mood}`);
      } else {
        await agent.com.atproto.repo.deleteRecord({
          repo: did,
          collection: 'computer.aesthetic.mood',
          rkey,
        });
        deleteCount++;
        
        if (deleteCount % 20 === 0) {
          console.log(`   Deleted ${deleteCount}/${toDelete.length}...`);
        }
      }
    } catch (error) {
      console.error(`   ❌ Failed to delete ${rkey}: ${error.message}`);
    }
  }
  
  if (!dryRun) {
    console.log(`   ✅ Deleted ${deleteCount} duplicate records`);
  }
}

// Summary
console.log('\n' + '='.repeat(80));
console.log('\n📊 Summary:');
console.log(`   Total ATProto moods before: ${allMoods.length}`);
console.log(`   Unique refs: ${refToRkeys.size}`);
console.log(`   Duplicates found: ${duplicates.length}`);
console.log(`   Records to delete: ${toDelete.length}`);
if (!dryRun && toDelete.length > 0) {
  console.log(`   Expected remaining: ${allMoods.length - toDelete.length}`);
}
console.log('='.repeat(80) + '\n');

await database.disconnect();
process.exit(0);
