#!/usr/bin/env node

// Check @fifi's recent paintings specifically

import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();
const paintings = database.db.collection('paintings');
const users = database.db.collection('users');

console.log('🔍 Checking @fifi\'s recent paintings\n');

// Get @fifi's user ID
const fifiSub = await userIDFromHandle('fifi', database);
if (!fifiSub) {
  console.log('❌ User @fifi not found');
  await database.disconnect();
  process.exit(1);
}

console.log(`User ID: ${fifiSub}\n`);

// Check if @fifi has ATProto account
const fifiUser = await users.findOne({ _id: fifiSub });
console.log(`ATProto DID: ${fifiUser?.atproto?.did || 'NOT SET'}\n`);

// Get @fifi's paintings from last 7 days
const sevenDaysAgo = new Date();
sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);

const recentPaintings = await paintings.find({
  user: fifiSub,
  when: { $gte: sevenDaysAgo }
}).sort({ when: -1 }).toArray();

console.log(`Found ${recentPaintings.length} paintings from @fifi in last 7 days:\n`);

let synced = 0;
let notSynced = 0;

for (const painting of recentPaintings) {
  const hasRkey = !!painting.atproto?.rkey;
  if (hasRkey) synced++;
  else notSynced++;
  
  const status = hasRkey ? '✅ Synced' : '❌ NOT synced';
  
  console.log(`${status} - ${painting.slug}`);
  console.log(`   When: ${painting.when.toISOString()}`);
  console.log(`   rkey: ${painting.atproto?.rkey || 'none'}`);
  console.log('');
}

console.log(`\nSummary: ${synced} synced, ${notSynced} not synced`);

await database.disconnect();
process.exit(0);
