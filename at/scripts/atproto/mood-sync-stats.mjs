#!/usr/bin/env node

// Get overall mood sync statistics

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();
const moods = database.db.collection('moods');
const users = database.db.collection('users');

console.log('📊 Mood Sync Statistics\n');

// Total moods
const totalMoods = await moods.countDocuments({ deleted: { $ne: true } });
console.log(`Total active moods in MongoDB: ${totalMoods}`);

// Moods with ATProto records
const moodsWithAtproto = await moods.countDocuments({
  deleted: { $ne: true },
  'atproto.rkey': { $exists: true, $ne: null }
});
console.log(`Moods synced to ATProto: ${moodsWithAtproto}`);

// Moods without ATProto records
const moodsWithoutAtproto = await moods.countDocuments({
  deleted: { $ne: true },
  $or: [
    { 'atproto.rkey': { $exists: false } },
    { 'atproto.rkey': null }
  ]
});
console.log(`Moods NOT synced: ${moodsWithoutAtproto}`);

// Percentage
const percentSynced = ((moodsWithAtproto / totalMoods) * 100).toFixed(2);
console.log(`\nSync rate: ${percentSynced}%`);

// Check recent moods (last 30 days)
const thirtyDaysAgo = new Date();
thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);

const recentMoods = await moods.countDocuments({
  deleted: { $ne: true },
  when: { $gte: thirtyDaysAgo }
});

const recentMoodsWithAtproto = await moods.countDocuments({
  deleted: { $ne: true },
  when: { $gte: thirtyDaysAgo },
  'atproto.rkey': { $exists: true, $ne: null }
});

console.log(`\n📅 Last 30 days:`);
console.log(`   Total moods: ${recentMoods}`);
console.log(`   Synced: ${recentMoodsWithAtproto}`);
console.log(`   Not synced: ${recentMoods - recentMoodsWithAtproto}`);

if (recentMoods > 0) {
  const recentSyncRate = ((recentMoodsWithAtproto / recentMoods) * 100).toFixed(2);
  console.log(`   Sync rate: ${recentSyncRate}%`);
}

// Show latest unsynced moods
console.log('\n❌ Latest unsynced moods:');
const unsyncedMoods = await moods.find({
  deleted: { $ne: true },
  $or: [
    { 'atproto.rkey': { $exists: false } },
    { 'atproto.rkey': null }
  ]
}).sort({ when: -1 }).limit(5).toArray();

for (const mood of unsyncedMoods) {
  // Check if user has ATProto account
  const user = await users.findOne({ _id: mood.user });
  const hasAtproto = user?.atproto?.did ? '✅ Has account' : '❌ No account';
  
  console.log(`   "${mood.mood.substring(0, 40)}..."`);
  console.log(`      User: ${mood.user} ${hasAtproto}`);
  console.log(`      When: ${mood.when.toISOString()}`);
  console.log('');
}

await database.disconnect();
