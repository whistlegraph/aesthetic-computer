#!/usr/bin/env node

// Check when ATProto accounts were created

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();
const users = database.db.collection('users');

console.log('📊 ATProto Account Creation Timeline\n');

// Get some recent users with ATProto
const recentWithAtproto = await users.find({
  'atproto.did': { $exists: true, $ne: null },
  created_at: { $exists: true }
}).sort({ created_at: -1 }).limit(5).toArray();

console.log('Recent users WITH ATProto:');
for (const user of recentWithAtproto) {
  console.log(`   ${user._id}: ${user.created_at}`);
}

// Get oldest users with ATProto
const oldestWithAtproto = await users.find({
  'atproto.did': { $exists: true, $ne: null },
  created_at: { $exists: true }
}).sort({ created_at: 1 }).limit(5).toArray();

console.log('\nOldest users WITH ATProto:');
for (const user of oldestWithAtproto) {
  console.log(`   ${user._id}: ${user.created_at}`);
}

// Users without ATProto
const withoutAtproto = await users.find({
  $or: [
    { 'atproto.did': { $exists: false } },
    { 'atproto.did': null }
  ],
  created_at: { $exists: true }
}).sort({ created_at: 1 }).toArray();

console.log('\nUsers WITHOUT ATProto (by creation date):');
for (const user of withoutAtproto) {
  console.log(`   ${user._id}: ${user.created_at || 'no created_at field'}`);
}

await database.disconnect();
