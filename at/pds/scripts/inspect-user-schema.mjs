#!/usr/bin/env node
// inspect-user-schema.mjs - Check the actual user schema

import { connect } from '../../../system/backend/database.mjs';

try {
  const database = await connect();
  const users = database.db.collection('users');
  
  console.log('\n🔍 Finding users with ATProto accounts...\n');
  
  // Find a few users that have ATProto data
  const usersWithAtproto = await users.find({
    $or: [
      { atproto: { $exists: true } },
      { at: { $exists: true } }
    ]
  }).limit(3).toArray();
  
  console.log(`Found ${usersWithAtproto.length} users with ATProto data:\n`);
  
  usersWithAtproto.forEach((user, index) => {
    console.log(`\n--- User ${index + 1}: ${user._id} ---`);
    console.log(JSON.stringify(user, null, 2));
  });
  
  // Check field names
  const hasAtproto = await users.countDocuments({ atproto: { $exists: true } });
  const hasAt = await users.countDocuments({ at: { $exists: true } });
  
  console.log(`\n\n📊 Field usage:`);
  console.log(`   "atproto" field: ${hasAtproto} users`);
  console.log(`   "at" field: ${hasAt} users`);
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
