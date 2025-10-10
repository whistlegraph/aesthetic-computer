#!/usr/bin/env node
// query-user.mjs - Quick script to query a user's @handles record

import { connect } from '../../../system/backend/database.mjs';

const handle = process.argv[2] || 'jeffrey';

try {
  const database = await connect();
  const handles = database.db.collection('@handles');
  
  console.log(`\n🔍 Querying @handles for: ${handle}\n`);
  
  const user = await handles.findOne({ handle });
  
  if (user) {
    console.log(JSON.stringify(user, null, 2));
  } else {
    console.log(`❌ No user found with handle: ${handle}`);
  }
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
