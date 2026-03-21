#!/usr/bin/env node
// test-user-creation-flow.mjs
// Manually test the user creation flow for recent signups
// This simulates what auth0-events.mjs SHOULD have done on signup

import { shell } from '../../system/backend/shell.mjs';
import { connect } from '../../system/backend/database.mjs';
import { generateUniqueUserCode, ensureUserCodeIndex } from '../../system/public/aesthetic.computer/lib/user-code.mjs';
import { createAtprotoAccount } from '../../system/backend/at.mjs';
import { config } from 'dotenv';

config();

async function getAuth0Token(tenant = 'aesthetic') {
  const { got } = await import('got');
  
  const clientId = tenant === 'aesthetic' 
    ? process.env.AUTH0_M2M_CLIENT_ID 
    : process.env.SOTCE_AUTH0_M2M_CLIENT_ID;
  const clientSecret = tenant === 'aesthetic'
    ? process.env.AUTH0_M2M_SECRET
    : process.env.SOTCE_AUTH0_M2M_SECRET;
  const baseURI = tenant === 'aesthetic'
    ? 'https://aesthetic.us.auth0.com'
    : 'https://sotce.us.auth0.com';

  const tokenResponse = await got.post(`${baseURI}/oauth/token`, {
    json: {
      client_id: clientId,
      client_secret: clientSecret,
      audience: `${baseURI}/api/v2/`,
      grant_type: 'client_credentials',
    },
    responseType: 'json',
  });

  return { token: tokenResponse.body.access_token, baseURI };
}

async function getAuth0User(sub, tenant = 'aesthetic') {
  const { got } = await import('got');
  const { token, baseURI } = await getAuth0Token(tenant);
  
  const response = await got(`${baseURI}/api/v2/users/${encodeURIComponent(sub)}`, {
    headers: { Authorization: `Bearer ${token}` },
    responseType: 'json',
  });
  
  return response.body;
}

async function fixUserCreation(sub, tenant = 'aesthetic', dryRun = true) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`🔧 ${dryRun ? 'SIMULATING' : 'FIXING'} USER CREATION FOR: ${sub}`);
  console.log(`${'='.repeat(80)}\n`);

  const database = await connect();
  const users = database.db.collection('users');
  const verifications = database.db.collection('verifications');

  try {
    // 1. Get user info from Auth0
    console.log('📋 Step 1: Fetching user info from Auth0...\n');
    const auth0User = await getAuth0User(sub, tenant);
    
    console.log(`   Sub: ${auth0User.user_id}`);
    console.log(`   Email: ${auth0User.email}`);
    console.log(`   Email Verified: ${auth0User.email_verified ? '✅' : '❌'}`);
    console.log(`   Created: ${auth0User.created_at}`);

    // 2. Check current state in MongoDB
    console.log('\n📋 Step 2: Checking MongoDB state...\n');
    
    const verification = await verifications.findOne({ _id: sub });
    console.log(`   Verifications: ${verification ? `✅ count=${verification.count}` : '❌ Missing'}`);
    
    const existingUser = await users.findOne({ _id: sub });
    console.log(`   Users record: ${existingUser ? '✅ Exists' : '❌ Missing'}`);
    
    if (existingUser) {
      console.log(`      Code: ${existingUser.code}`);
      console.log(`      Created: ${existingUser.when}`);
      console.log(`      ATProto: ${existingUser.atproto ? `✅ ${existingUser.atproto.did}` : '❌ Missing'}`);
    }

    // 3. Create missing records
    const actions = [];
    
    // Create verifications if missing
    if (!verification) {
      actions.push('Create verifications record');
      if (!dryRun) {
        await verifications.insertOne({ _id: sub, count: 0 });
        console.log('\n   ✅ Created verifications record');
      }
    }

    // Create user record if missing
    if (!existingUser) {
      actions.push('Create users record with code');
      if (!dryRun) {
        await ensureUserCodeIndex(database);
        const signupDate = new Date(auth0User.created_at);
        const code = await generateUniqueUserCode(database, signupDate);
        
        await users.insertOne({
          _id: sub,
          code,
          when: signupDate
        });
        
        console.log(`\n   ✅ Created users record with code: ${code}`);
      }
    }

    // Create ATProto account if missing and email verified
    if (auth0User.email_verified && (!existingUser || !existingUser.atproto)) {
      actions.push('Create ATProto account');
      if (!dryRun) {
        console.log('\n   🦋 Creating ATProto account...');
        const result = await createAtprotoAccount(database, sub, auth0User.email);
        
        if (result.created) {
          console.log(`   ✅ ATProto account created:`);
          console.log(`      DID: ${result.did}`);
          console.log(`      Handle: ${result.handle}`);
        } else {
          console.log(`   ⚠️  ATProto creation result: ${result.reason}`);
        }
      }
    } else if (!auth0User.email_verified) {
      console.log('\n   ℹ️  Email not verified, skipping ATProto account creation');
    }

    // 4. Summary
    console.log(`\n${'─'.repeat(80)}`);
    if (dryRun) {
      console.log(`\n📋 DRY RUN - Would perform ${actions.length} action(s):`);
      actions.forEach(action => console.log(`   - ${action}`));
      console.log(`\n💡 Run with --fix flag to actually perform these actions`);
    } else {
      console.log(`\n✅ Fixed ${actions.length} issue(s)`);
    }
    console.log(`${'─'.repeat(80)}\n`);

  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    throw error;
  } finally {
    await database.disconnect();
  }
}

async function fixAllRecentUsers(limit = 10, dryRun = true) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`🔧 ${dryRun ? 'SIMULATING' : 'FIXING'} USER CREATION FOR RECENT SIGNUPS`);
  console.log(`${'='.repeat(80)}\n`);

  const { got } = await import('got');
  const { token, baseURI } = await getAuth0Token('aesthetic');
  
  const response = await got(`${baseURI}/api/v2/users`, {
    searchParams: {
      sort: 'created_at:-1',
      per_page: limit,
      page: 0,
      fields: 'user_id,email,email_verified,created_at',
      include_fields: true,
    },
    headers: { Authorization: `Bearer ${token}` },
    responseType: 'json',
  });

  const users = response.body;
  console.log(`Found ${users.length} recent users\n`);

  for (const user of users) {
    await fixUserCreation(user.user_id, 'aesthetic', dryRun);
  }

  console.log(`\n${'='.repeat(80)}`);
  console.log(`✅ COMPLETED ${dryRun ? 'SIMULATION' : 'FIX'} FOR ${users.length} USERS`);
  console.log(`${'='.repeat(80)}\n`);
}

// Parse command line args
const args = process.argv.slice(2);
const command = args[0];
const dryRun = !args.includes('--fix');

if (dryRun) {
  console.log('\n⚠️  DRY RUN MODE - No changes will be made');
  console.log('   Add --fix flag to actually fix issues\n');
}

try {
  if (command && command.startsWith('auth0|')) {
    // Fix single user
    await fixUserCreation(command, 'aesthetic', dryRun);
  } else {
    // Fix all recent users
    const limit = command ? parseInt(command) : 10;
    await fixAllRecentUsers(limit, dryRun);
  }
} catch (error) {
  console.error('❌ Script failed:', error);
  process.exit(1);
}
