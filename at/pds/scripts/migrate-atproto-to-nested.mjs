#!/usr/bin/env node
// migrate-atproto-to-nested.mjs
// Converts flat atproto_* fields to nested atproto {} object

import { connect } from '../../../system/backend/database.mjs';

async function main() {
  console.log('🔄 Migrating ATProto fields to nested structure...\n');
  
  const database = await connect();
  const users = database.db.collection('users');
  
  // Find users with flat atproto_* fields
  const usersWithFlatFields = await users.find({
    $or: [
      { atproto_did: { $exists: true } },
      { atproto_handle: { $exists: true } },
      { atproto_password: { $exists: true } },
      { atproto_created_at: { $exists: true } }
    ]
  }).toArray();
  
  console.log(`📊 Found ${usersWithFlatFields.length} users with flat ATProto fields\n`);
  
  if (usersWithFlatFields.length === 0) {
    console.log('✅ No migration needed - all fields are already nested!\n');
    await database.disconnect();
    return;
  }
  
  let migrated = 0;
  
  for (const user of usersWithFlatFields) {
    // Build nested atproto object
    const atproto = {};
    
    if (user.atproto_did) atproto.did = user.atproto_did;
    if (user.atproto_handle) atproto.handle = user.atproto_handle;
    if (user.atproto_password) atproto.password = user.atproto_password;
    if (user.atproto_created_at) atproto.created = new Date(user.atproto_created_at);
    
    // Update user with nested structure and remove flat fields
    await users.updateOne(
      { _id: user._id },
      {
        $set: { atproto },
        $unset: {
          atproto_did: '',
          atproto_handle: '',
          atproto_password: '',
          atproto_created_at: ''
        }
      }
    );
    
    migrated++;
    
    if (migrated % 100 === 0) {
      process.stdout.write(`\r   Migrated ${migrated}/${usersWithFlatFields.length}...`);
    }
  }
  
  console.log(`\r✅ Migrated ${migrated} users to nested structure!\n`);
  
  // Show example
  const sample = await users.findOne({ 'atproto.did': { $exists: true } });
  if (sample) {
    console.log('Example migrated record:');
    console.log(JSON.stringify(sample, null, 2));
  }
  
  await database.disconnect();
}

main().catch(console.error);
