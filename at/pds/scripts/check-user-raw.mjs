#!/usr/bin/env node
// check-user-raw.mjs - Check raw user document in MongoDB

import { connect } from '../../../system/backend/database.mjs';

const sub = 'auth0|68e99d2f23027e65c0ae6416';

try {
  const database = await connect();
  const users = database.db.collection('users');
  
  console.log('\n🔍 Checking users collection for:', sub);
  const user = await users.findOne({ _id: sub });
  
  if (user) {
    console.log('\n✅ User document found:');
    console.log(JSON.stringify(user, null, 2));
  } else {
    console.log('\n❌ No user document found');
    
    // Check if any documents exist at all
    const count = await users.countDocuments();
    console.log(`\n📊 Total users in collection: ${count}`);
  }
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
