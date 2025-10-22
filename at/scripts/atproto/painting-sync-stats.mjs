#!/usr/bin/env node

// Get painting sync statistics

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();
const paintings = database.db.collection('paintings');
const users = database.db.collection('users');
const handles = database.db.collection('@handles');

console.log('🎨 Painting Sync Statistics\n');

// Total paintings
const totalPaintings = await paintings.countDocuments({});
console.log(`Total paintings in MongoDB: ${totalPaintings}`);

// Paintings with ATProto records
const paintingsWithAtproto = await paintings.countDocuments({
  'atproto.rkey': { $exists: true, $ne: null }
});
console.log(`Paintings synced to ATProto: ${paintingsWithAtproto}`);

// Paintings without ATProto records
const paintingsWithoutAtproto = await paintings.countDocuments({
  $or: [
    { 'atproto.rkey': { $exists: false } },
    { 'atproto.rkey': null }
  ]
});
console.log(`Paintings NOT synced: ${paintingsWithoutAtproto}`);

// Percentage
if (totalPaintings > 0) {
  const percentSynced = ((paintingsWithAtproto / totalPaintings) * 100).toFixed(2);
  console.log(`\nSync rate: ${percentSynced}%`);
}

// Check recent paintings (last 30 days)
const thirtyDaysAgo = new Date();
thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);

const recentPaintings = await paintings.countDocuments({
  when: { $gte: thirtyDaysAgo }
});

const recentPaintingsWithAtproto = await paintings.countDocuments({
  when: { $gte: thirtyDaysAgo },
  'atproto.rkey': { $exists: true, $ne: null }
});

console.log(`\n📅 Last 30 days:`);
console.log(`   Total paintings: ${recentPaintings}`);
console.log(`   Synced: ${recentPaintingsWithAtproto}`);
console.log(`   Not synced: ${recentPaintings - recentPaintingsWithAtproto}`);

if (recentPaintings > 0) {
  const recentSyncRate = ((recentPaintingsWithAtproto / recentPaintings) * 100).toFixed(2);
  console.log(`   Sync rate: ${recentSyncRate}%`);
}

// Show latest synced paintings
console.log('\n✅ Latest synced paintings:');
const syncedPaintings = await paintings.find({
  'atproto.rkey': { $exists: true, $ne: null }
}).sort({ when: -1 }).limit(5).toArray();

for (const painting of syncedPaintings) {
  const handleDoc = await handles.findOne({ _id: painting.user });
  const handle = handleDoc?.handle || 'unknown';
  
  console.log(`   @${handle} - "${painting.slug}"`);
  console.log(`      When: ${painting.when.toISOString()}`);
  console.log(`      rkey: ${painting.atproto.rkey}`);
  console.log('');
}

// Show latest unsynced paintings
console.log('❌ Latest unsynced paintings:');
const unsyncedPaintings = await paintings.find({
  $or: [
    { 'atproto.rkey': { $exists: false } },
    { 'atproto.rkey': null }
  ]
}).sort({ when: -1 }).limit(10).toArray();

if (unsyncedPaintings.length === 0) {
  console.log('   None! All paintings are synced.\n');
} else {
  for (const painting of unsyncedPaintings) {
    // Check if user has ATProto account
    const user = await users.findOne({ _id: painting.user });
    const hasAtproto = user?.atproto?.did ? '✅ Has account' : '❌ No account';
    
    const handleDoc = await handles.findOne({ _id: painting.user });
    const handle = handleDoc?.handle || 'unknown';
    
    console.log(`   @${handle} - "${painting.slug}"`);
    console.log(`      User: ${painting.user} ${hasAtproto}`);
    console.log(`      When: ${painting.when.toISOString()}`);
    console.log('');
  }
}

// User breakdown using aggregation instead of distinct
console.log('📊 User breakdown:');
const userGroups = await paintings.aggregate([
  { $group: { _id: '$user' } }
]).toArray();
console.log(`   Users with paintings: ${userGroups.length}`);

await database.disconnect();
