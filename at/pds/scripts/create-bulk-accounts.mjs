#!/usr/bin/env node
// create-bulk-accounts.mjs
// Create ATProto accounts for multiple users in bulk

import { createAccount } from './create-account.mjs';
import { connect } from '../../../system/backend/database.mjs';
import { shell } from '../../../system/backend/shell.mjs';

async function main() {
  const batchSize = parseInt(process.argv[2]) || 10;
  
  console.log(`🦋 Creating ATProto accounts for ${batchSize} users...\n`);
  
  // Connect to database
  const database = await connect();
  const users = database.db.collection('users');
  
  // Find users without ATProto accounts
  const usersToProcess = await users.find({
    'atproto.did': { $exists: false }
  }).limit(batchSize).toArray();
  
  console.log(`📊 Found ${usersToProcess.length} users without ATProto accounts\n`);
  
  if (usersToProcess.length === 0) {
    console.log('✅ All users already have ATProto accounts!\n');
    await database.disconnect();
    return;
  }
  
  let created = 0;
  let failed = 0;
  const errors = [];
  
  for (let i = 0; i < usersToProcess.length; i++) {
    const user = usersToProcess[i];
    const progress = `[${i + 1}/${usersToProcess.length}]`;
    
    try {
      console.log(`\n${'═'.repeat(80)}`);
      console.log(`${progress} Processing: ${user._id}`);
      console.log(`${'═'.repeat(80)}`);
      
      await createAccount(user._id);
      created++;
      
      console.log(`\n✅ ${progress} Success!`);
      
      // Small delay to avoid overwhelming the PDS
      if (i < usersToProcess.length - 1) {
        await new Promise(resolve => setTimeout(resolve, 1000));
      }
      
    } catch (error) {
      failed++;
      errors.push({ sub: user._id, error: error.message });
      console.error(`\n❌ ${progress} Failed: ${error.message}`);
    }
  }
  
  await database.disconnect();
  
  // Summary
  console.log(`\n${'═'.repeat(80)}`);
  console.log('📊 BULK CREATION SUMMARY');
  console.log(`${'═'.repeat(80)}`);
  console.log(`✅ Created:  ${created}`);
  console.log(`❌ Failed:   ${failed}`);
  console.log(`📊 Total:    ${usersToProcess.length}`);
  
  if (errors.length > 0) {
    console.log(`\n❌ ERRORS:`);
    errors.forEach(({ sub, error }) => {
      console.log(`   ${sub}: ${error}`);
    });
  }
  
  console.log(`\n${'═'.repeat(80)}\n`);
}

main().catch(console.error);
