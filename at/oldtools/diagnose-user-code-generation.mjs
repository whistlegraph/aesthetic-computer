#!/usr/bin/env node
// diagnose-user-code-generation.mjs
// Test the user code generation function to identify the actual error

import { shell } from '../../system/backend/shell.mjs';
import { connect } from '../../system/backend/database.mjs';
import { generateUniqueUserCode, ensureUserCodeIndex } from '../../system/public/aesthetic.computer/lib/user-code.mjs';
import { config } from 'dotenv';

config();

async function testUserCodeGeneration() {
  console.log('\n================================================================================');
  console.log('🧪 TESTING USER CODE GENERATION');
  console.log('================================================================================\n');

  const database = await connect();

  try {
    console.log('📋 Step 1: Ensuring user code index exists...');
    await ensureUserCodeIndex(database);
    console.log('   ✅ Index check complete\n');

    console.log('📋 Step 2: Testing code generation with current timestamp...');
    const now = new Date();
    console.log(`   Test date: ${now.toISOString()}`);
    
    try {
      const code1 = await generateUniqueUserCode(database, now);
      console.log(`   ✅ Generated code: ${code1}\n`);

      console.log('📋 Step 3: Testing second generation (should get different code)...');
      const code2 = await generateUniqueUserCode(database, now);
      console.log(`   ✅ Generated code: ${code2}`);
      console.log(`   ${code1 === code2 ? '⚠️  SAME CODE!' : '✅ Different codes'}\n`);

      console.log('📋 Step 4: Checking users collection for test codes...');
      const users = database.db.collection('users');
      
      const testUser1 = await users.findOne({ code: code1 });
      const testUser2 = await users.findOne({ code: code2 });
      
      console.log(`   Code ${code1}: ${testUser1 ? '✅ Exists in DB' : '❌ Not in DB'}`);
      console.log(`   Code ${code2}: ${testUser2 ? '✅ Exists in DB' : '❌ Not in DB'}\n`);

      console.log('📋 Step 5: Testing with past date (like Auth0 signup)...');
      const pastDate = new Date('2025-10-14T13:29:20.565Z');
      console.log(`   Test date: ${pastDate.toISOString()}`);
      
      const code3 = await generateUniqueUserCode(database, pastDate);
      console.log(`   ✅ Generated code: ${code3}\n`);

      console.log('📋 Step 6: Checking database state...');
      const userCount = await users.countDocuments();
      const usersWithCode = await users.countDocuments({ code: { $exists: true } });
      
      console.log(`   Total users: ${userCount}`);
      console.log(`   Users with code: ${usersWithCode}`);
      
      // Sample some recent codes
      const recentUsers = await users.find({ code: { $exists: true } })
        .sort({ when: -1 })
        .limit(5)
        .toArray();
      
      console.log(`\n   Recent codes (last 5):`);
      recentUsers.forEach(u => {
        console.log(`      ${u.code} - ${u._id} - ${u.when}`);
      });

      console.log('\n================================================================================');
      console.log('✅ USER CODE GENERATION TEST PASSED');
      console.log('================================================================================\n');
      console.log('💡 The user code generation function is working correctly.');
      console.log('   The issue must be elsewhere in the auth0-events webhook flow.\n');

    } catch (error) {
      console.log(`   ❌ Code generation failed!\n`);
      console.log('   Error name:', error.name);
      console.log('   Error message:', error.message);
      console.log('   Error stack:', error.stack);
      
      console.log('\n================================================================================');
      console.log('❌ USER CODE GENERATION TEST FAILED');
      console.log('================================================================================\n');
      console.log('🔍 This is likely the root cause of why users records are not being created.\n');
      
      throw error;
    }

  } catch (error) {
    console.error('\n❌ Test suite failed:', error.message);
    throw error;
  } finally {
    await database.disconnect();
  }
}

// Additional test: Simulate the exact auth0-events flow
async function testAuth0EventsFlow() {
  console.log('\n================================================================================');
  console.log('🧪 SIMULATING AUTH0-EVENTS.MJS FLOW');
  console.log('================================================================================\n');

  const database = await connect();

  try {
    // Simulate auth0 signup event data
    const mockAuthSub = `auth0|TEST_${Date.now()}`;
    const mockEmail = 'test@example.com';
    const mockSignupDate = new Date();

    console.log('📋 Simulating signup event for:', mockAuthSub);
    console.log('   Email:', mockEmail);
    console.log('   Date:', mockSignupDate.toISOString(), '\n');

    // Step 1: Create verifications record (this works)
    console.log('   Step 1: Creating verifications record...');
    const verifications = database.db.collection("verifications");
    await verifications.insertOne({ _id: mockAuthSub, count: 0 });
    console.log('   ✅ Verifications record created\n');

    // Step 2: Generate user code and create user record (this fails)
    console.log('   Step 2: Generating user code and creating user record...');
    try {
      await ensureUserCodeIndex(database);
      const code = await generateUniqueUserCode(database, mockSignupDate);
      
      const users = database.db.collection("users");
      await users.insertOne({ 
        _id: mockAuthSub, 
        code,
        when: mockSignupDate
      });
      
      console.log(`   ✅ User record created with code: ${code}\n`);

      // Clean up test records
      console.log('   Cleaning up test records...');
      await verifications.deleteOne({ _id: mockAuthSub });
      await users.deleteOne({ _id: mockAuthSub });
      console.log('   ✅ Test records deleted\n');

      console.log('================================================================================');
      console.log('✅ AUTH0 FLOW SIMULATION PASSED');
      console.log('================================================================================\n');
      console.log('💡 The auth0-events flow simulation works correctly.');
      console.log('   This suggests the webhook might not be receiving events,');
      console.log('   or there\'s an environment/permissions difference in production.\n');

    } catch (error) {
      console.log(`   ❌ User record creation failed!\n`);
      console.log('   Error:', error.message);
      console.log('   Stack:', error.stack);
      
      // Try to clean up verifications record
      await verifications.deleteOne({ _id: mockAuthSub }).catch(() => {});
      
      throw error;
    }

  } catch (error) {
    console.error('\n❌ Flow simulation failed:', error.message);
    throw error;
  } finally {
    await database.disconnect();
  }
}

// Run both tests
try {
  await testUserCodeGeneration();
  await testAuth0EventsFlow();
  
  console.log('🎉 ALL TESTS PASSED!\n');
  console.log('🔍 Next steps:');
  console.log('   1. Check Netlify function logs for actual production errors');
  console.log('   2. Verify Auth0 webhook is actually being called');
  console.log('   3. Check if there are any environment differences in production');
  console.log('   4. Consider running backfill script to fix existing users\n');
  
} catch (error) {
  console.error('\n💥 TESTS FAILED\n');
  process.exit(1);
}
