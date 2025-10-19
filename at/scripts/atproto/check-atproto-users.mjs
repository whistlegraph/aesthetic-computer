#!/usr/bin/env node

// Check how many users have ATProto accounts

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();
const users = database.db.collection('users');
const moods = database.db.collection('moods');
const paintings = database.db.collection('paintings');

console.log('📊 ATProto Account Statistics\n');

// Count users with ATProto accounts
const totalUsers = await users.countDocuments();
const usersWithAtproto = await users.countDocuments({
  'atproto.did': { $exists: true, $ne: null }
});

console.log(`👥 Users:`);
console.log(`   Total: ${totalUsers}`);
console.log(`   With ATProto: ${usersWithAtproto}`);
console.log(`   Without ATProto: ${totalUsers - usersWithAtproto}`);

// Sample some users with ATProto
console.log('\n🔍 Sample users with ATProto accounts:');
const sampleUsers = await users.find({
  'atproto.did': { $exists: true, $ne: null }
}).limit(5).toArray();

for (const user of sampleUsers) {
  console.log(`   ${user._id} - DID: ${user.atproto.did}`);
}

// Count moods for users with ATProto
const userIdsWithAtproto = sampleUsers.map(u => u._id);
const moodsForAtprotoUsers = await moods.countDocuments({
  user: { $in: userIdsWithAtproto },
  deleted: { $ne: true },
  'atproto.rkey': { $exists: false }
});

console.log(`\n🦋 Moods needing sync for sample ATProto users: ${moodsForAtprotoUsers}`);

// Count paintings for users with ATProto
const paintingsForAtprotoUsers = await paintings.countDocuments({
  user: { $in: userIdsWithAtproto },
  'atproto.rkey': { $exists: false }
});

console.log(`🎨 Paintings needing sync for sample ATProto users: ${paintingsForAtprotoUsers}`);

await database.disconnect();
