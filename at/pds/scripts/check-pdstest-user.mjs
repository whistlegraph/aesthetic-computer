#!/usr/bin/env node
// check-pdstest-user.mjs
// Check if the pdstest user has a PDS account

import { connect } from '../../../system/backend/database.mjs';
import { userEmailFromID, handleFor } from '../../../system/backend/authorization.mjs';

const PDSTEST_SUB = 'auth0|68e99d2f23027e65c0ae6416';

async function main() {
  try {
    const database = await connect();
    
    console.log('\n🔍 Checking pdstest user...\n');
    console.log(`Sub: ${PDSTEST_SUB}`);
    
    // Check Auth0 email
    console.log('\n📧 Checking Auth0 email...');
    const emailResult = await userEmailFromID(PDSTEST_SUB, 'aesthetic');
    if (emailResult?.email) {
      console.log(`   Email: ${emailResult.email}`);
      console.log(`   Verified: ${emailResult.email_verified ? 'Yes' : 'No'}`);
    } else {
      console.log('   ❌ No email found in Auth0');
    }
    
    // Check for handle
    console.log('\n🏷️  Checking for handle...');
    const handle = await handleFor(PDSTEST_SUB, database);
    if (handle) {
      console.log(`   Handle: @${handle}`);
    } else {
      console.log('   ❌ No handle set yet');
    }
    
    // Check users collection for ATProto info
    console.log('\n🌐 Checking ATProto account...');
    const users = database.db.collection('users');
    const user = await users.findOne({ _id: PDSTEST_SUB });
    
    if (user) {
      console.log('   ✅ User document found:');
      if (user.atproto) {
        console.log(`   DID: ${user.atproto.did || 'Not set'}`);
        console.log(`   Handle: ${user.atproto.handle || 'Not set'}`);
        console.log(`   Created: ${user.atproto.createdAt || 'Not set'}`);
        console.log(`   Updated: ${user.atproto.updatedAt || 'Not set'}`);
      } else {
        console.log('   ⚠️  No atproto field in user document');
      }
      
      if (user.code) {
        console.log(`   Code: ${user.code}`);
      }
    } else {
      console.log('   ❌ No user document found');
    }
    
    await database.disconnect();
    console.log('\n✅ Check complete!\n');
    
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}\n`);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
