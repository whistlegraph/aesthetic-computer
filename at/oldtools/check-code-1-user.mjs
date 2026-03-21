#!/usr/bin/env node
// check-code-1-user.mjs
// Check if there's actually a user with code "code_1" or if it's just index naming confusion

import { connect } from '../../system/backend/database.mjs';
import { config } from 'dotenv';

config();

async function checkCodeOneUser() {
  console.log('\n🔍 Checking for user with code "code_1"...\n');
  
  const database = await connect();
  const users = database.db.collection('users');

  // Check for user with code "code_1"
  const codeOneUser = await users.findOne({ code: 'code_1' });
  
  if (codeOneUser) {
    console.log('⚠️  FOUND USER WITH CODE "code_1":');
    console.log(JSON.stringify(codeOneUser, null, 2));
  } else {
    console.log('✅ No user has code "code_1"');
  }

  // List all indexes on the users collection
  console.log('\n📋 All indexes on users collection:\n');
  const indexes = await users.indexes();
  
  indexes.forEach(index => {
    console.log(`Index: ${index.name}`);
    console.log(`  Keys:`, JSON.stringify(index.key));
    console.log(`  Unique: ${index.unique || false}`);
    console.log(`  Sparse: ${index.sparse || false}`);
    console.log('');
  });

  // Check if there's a "code_1" index
  const codeOneIndex = indexes.find(idx => idx.name === 'code_1');
  
  if (codeOneIndex) {
    console.log('🔍 Found "code_1" index - this is MongoDB\'s DEFAULT naming!');
    console.log('   When you create an index without specifying a name,');
    console.log('   MongoDB automatically names it "{field}_1" (ascending) or "{field}_-1" (descending)');
    console.log('   This is NOT a user code, it\'s just the index name.\n');
  }

  // Show some actual user codes
  console.log('📊 Sample of actual user codes:\n');
  const sampleUsers = await users.find({ code: { $exists: true } })
    .limit(10)
    .toArray();
  
  sampleUsers.forEach(user => {
    console.log(`  ${user.code} - ${user._id}`);
  });

  await database.disconnect();
}

checkCodeOneUser().catch(console.error);
