#!/usr/bin/env node
// query-unhandled.mjs - Find an unhandled user

import { connect } from '../../../system/backend/database.mjs';

try {
  const database = await connect();
  const handles = database.db.collection('@handles');
  const verifications = database.db.collection('verifications');
  
  console.log(`\n🔍 Finding unhandled users...\n`);
  
  // Get a verified user who is NOT in @handles
  const allVerified = await verifications.find({}).limit(100).toArray();
  
  for (const verified of allVerified) {
    const handleRecord = await handles.findOne({ _id: verified._id });
    if (!handleRecord) {
      console.log('Found unhandled user:');
      console.log('Sub:', verified._id);
      console.log('Verification count:', verified.count);
      console.log('No @handles record\n');
      break;
    }
  }
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
