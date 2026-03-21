#!/usr/bin/env node

// Debug what's happening with the backfill queries

import { connect } from './database.mjs';
import { config } from 'dotenv';

config({ path: '../.env' });

const database = await connect();
const moods = database.db.collection('moods');
const users = database.db.collection('users');
const handles = database.db.collection('@handles');

console.log('🔍 Debugging backfill queries\n');

// Get one mood without atproto.rkey
const mood = await moods.findOne({
  deleted: { $ne: true },
  'atproto.rkey': { $exists: false }
});

if (!mood) {
  console.log('❌ No moods without ATProto records found');
  await database.disconnect();
  process.exit(0);
}

console.log('📋 Sample mood:');
console.log(`   _id: ${mood._id}`);
console.log(`   user: ${mood.user}`);
console.log(`   mood: ${mood.mood.substring(0, 50)}`);
console.log(`   when: ${mood.when}`);
console.log(`   atproto: ${JSON.stringify(mood.atproto)}`);

console.log('\n🔍 Looking up user...');
const user = await users.findOne({ _id: mood.user });

if (!user) {
  console.log('❌ User NOT FOUND with _id:', mood.user);
} else {
  console.log('✅ User found:');
  console.log(`   _id: ${user._id}`);
  console.log(`   email: ${user.email}`);
  console.log(`   atproto.did: ${user.atproto?.did || 'NOT SET'}`);
  console.log(`   atproto.password: ${user.atproto?.password ? '***' : 'NOT SET'}`);
}

console.log('\n🔍 Looking up handle...');
const handleDoc = await handles.findOne({ _id: mood.user });

if (!handleDoc) {
  console.log('❌ Handle NOT FOUND with _id:', mood.user);
  
  // Try to find handle by other means
  console.log('\n🔍 Searching all handles to see structure...');
  const allHandles = await handles.find({}).limit(3).toArray();
  console.log('Sample handles:', JSON.stringify(allHandles, null, 2));
  
} else {
  console.log('✅ Handle found:');
  console.log(`   _id: ${handleDoc._id}`);
  console.log(`   handle: ${handleDoc.handle}`);
}

await database.disconnect();
