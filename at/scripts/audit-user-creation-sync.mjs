#!/usr/bin/env node
// audit-user-creation-sync.mjs
// Comprehensive audit of user creation flow from Auth0 -> MongoDB -> ATProto
// Checks: Auth0 signups, verifications collection, users collection, @handles collection, and ATProto accounts

import { shell } from '../../system/backend/shell.mjs';
import { connect } from '../../system/backend/database.mjs';
import { userEmailFromID } from '../../system/backend/authorization.mjs';
import { config } from 'dotenv';
import { AtpAgent } from '@atproto/api';

config();

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

// Get Auth0 access token for a tenant
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

// Get recent Auth0 users
async function getAuth0RecentUsers(tenant = 'aesthetic', limit = 10) {
  try {
    const { got } = await import('got');
    const { token, baseURI } = await getAuth0Token(tenant);

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

    return response.body;
  } catch (error) {
    shell.error(`Error querying Auth0 ${tenant}: ${error.message}`);
    return null;
  }
}

// Check if ATProto account exists and is accessible
async function checkAtprotoAccount(did, password) {
  const agent = new AtpAgent({ service: PDS_URL });
  try {
    await agent.login({ identifier: did, password });
    return { exists: true, accessible: true };
  } catch (error) {
    if (error.message?.includes('not found') || error.message?.includes('Unknown')) {
      return { exists: false, accessible: false, error: 'Account not found on PDS' };
    }
    return { exists: true, accessible: false, error: error.message };
  }
}

// Main audit function
async function auditUserCreationSync(tenant = 'aesthetic', limit = 10) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`🔍 AUDITING USER CREATION & ATPROTO SYNC - ${tenant.toUpperCase()} TENANT`);
  console.log(`${'='.repeat(80)}\n`);

  const database = await connect();
  const users = database.db.collection('users');
  const handles = database.db.collection('@handles');
  const verifications = database.db.collection('verifications');

  // 1. Get recent Auth0 signups
  console.log('📋 STEP 1: Fetching recent Auth0 signups...\n');
  const auth0Users = await getAuth0RecentUsers(tenant, limit);
  
  if (!auth0Users || auth0Users.length === 0) {
    console.log('❌ No Auth0 users found or error fetching users\n');
    await database.disconnect();
    return;
  }

  console.log(`✅ Found ${auth0Users.length} recent Auth0 users\n`);

  // 2. Check each user's sync status
  const results = [];

  for (const auth0User of auth0Users) {
    const sub = auth0User.user_id;
    const email = auth0User.email;
    const emailVerified = auth0User.email_verified;
    const createdAt = new Date(auth0User.created_at);
    const hoursAgo = (Date.now() - createdAt.getTime()) / (1000 * 60 * 60);

    console.log(`\n${'─'.repeat(80)}`);
    console.log(`👤 User: ${sub}`);
    console.log(`   Email: ${email || 'N/A'}`);
    console.log(`   Email Verified: ${emailVerified ? '✅' : '❌'}`);
    console.log(`   Created: ${createdAt.toISOString()} (${hoursAgo.toFixed(1)}h ago)`);

    const userStatus = {
      sub,
      email,
      emailVerified,
      createdAt,
      hoursAgo: hoursAgo.toFixed(1),
      verification: null,
      userRecord: null,
      handleRecord: null,
      atproto: null,
      issues: [],
    };

    // Check verifications collection
    const verification = await verifications.findOne({ _id: sub });
    if (verification) {
      console.log(`   ✅ Verifications record: count=${verification.count}`);
      userStatus.verification = verification;
    } else {
      console.log(`   ❌ No verifications record found`);
      userStatus.issues.push('Missing verifications record');
    }

    // Check users collection
    const userRecord = await users.findOne({ _id: sub });
    if (userRecord) {
      console.log(`   ✅ Users record found:`);
      console.log(`      Code: ${userRecord.code || 'N/A'}`);
      console.log(`      Created: ${userRecord.when ? new Date(userRecord.when).toISOString() : 'N/A'}`);
      
      if (userRecord.atproto) {
        console.log(`      ATProto:`);
        console.log(`         DID: ${userRecord.atproto.did || 'N/A'}`);
        console.log(`         Handle: ${userRecord.atproto.handle || 'N/A'}`);
        console.log(`         Created: ${userRecord.atproto.created || 'N/A'}`);
        
        // Check if ATProto account actually exists on PDS
        if (userRecord.atproto.did && userRecord.atproto.password) {
          console.log(`      🔍 Checking PDS account...`);
          const pdsCheck = await checkAtprotoAccount(
            userRecord.atproto.did,
            userRecord.atproto.password
          );
          
          if (pdsCheck.accessible) {
            console.log(`         ✅ PDS account exists and is accessible`);
          } else if (pdsCheck.exists) {
            console.log(`         ⚠️  PDS account exists but login failed: ${pdsCheck.error}`);
            userStatus.issues.push(`ATProto login failed: ${pdsCheck.error}`);
          } else {
            console.log(`         ❌ PDS account not found: ${pdsCheck.error}`);
            userStatus.issues.push(`ATProto account missing on PDS`);
          }
          
          userStatus.atproto = {
            ...userRecord.atproto,
            pdsCheck,
          };
        } else {
          console.log(`         ⚠️  Missing DID or password`);
          userStatus.issues.push('ATProto credentials incomplete');
        }
      } else if (emailVerified) {
        console.log(`      ⚠️  No ATProto data (but email is verified!)`);
        userStatus.issues.push('ATProto account not created despite email verification');
      } else {
        console.log(`      ℹ️  No ATProto data (email not verified yet)`);
      }
      
      userStatus.userRecord = userRecord;
    } else {
      console.log(`   ❌ No users record found`);
      userStatus.issues.push('Missing users record');
    }

    // Check @handles collection
    const handleRecord = await handles.findOne({ _id: sub });
    if (handleRecord) {
      console.log(`   ✅ Handle record: @${handleRecord.handle}`);
      userStatus.handleRecord = handleRecord;
    } else {
      console.log(`   ℹ️  No handle set yet (optional)`);
    }

    // Summary for this user
    if (userStatus.issues.length === 0) {
      console.log(`   ✅ ALL CHECKS PASSED`);
    } else {
      console.log(`   ⚠️  ISSUES FOUND:`);
      userStatus.issues.forEach(issue => console.log(`      - ${issue}`));
    }

    results.push(userStatus);
  }

  // 3. Generate summary report
  console.log(`\n\n${'='.repeat(80)}`);
  console.log(`📊 SUMMARY REPORT - ${tenant.toUpperCase()} TENANT`);
  console.log(`${'='.repeat(80)}\n`);

  const totalUsers = results.length;
  const usersWithVerifications = results.filter(r => r.verification).length;
  const usersWithUserRecord = results.filter(r => r.userRecord).length;
  const usersWithHandle = results.filter(r => r.handleRecord).length;
  const usersWithAtproto = results.filter(r => r.atproto).length;
  const usersWithAtprotoOnPds = results.filter(r => r.atproto?.pdsCheck?.accessible).length;
  const emailVerified = results.filter(r => r.emailVerified).length;
  const usersWithIssues = results.filter(r => r.issues.length > 0).length;

  console.log(`Total Users Checked: ${totalUsers}`);
  console.log(`Email Verified: ${emailVerified}/${totalUsers}`);
  console.log(`\nMongoDB Collections:`);
  console.log(`  Verifications: ${usersWithVerifications}/${totalUsers}`);
  console.log(`  Users: ${usersWithUserRecord}/${totalUsers}`);
  console.log(`  Handles: ${usersWithHandle}/${totalUsers}`);
  console.log(`\nATProto Status:`);
  console.log(`  ATProto Data in Mongo: ${usersWithAtproto}/${totalUsers}`);
  console.log(`  ATProto Accounts on PDS: ${usersWithAtprotoOnPds}/${totalUsers}`);
  console.log(`\nIssues:`);
  console.log(`  Users with Issues: ${usersWithIssues}/${totalUsers}`);

  if (usersWithIssues > 0) {
    console.log(`\n⚠️  DETAILED ISSUES:\n`);
    results.filter(r => r.issues.length > 0).forEach(user => {
      console.log(`  ${user.sub}:`);
      user.issues.forEach(issue => console.log(`    - ${issue}`));
    });
  }

  // 4. Check for orphaned records (in Mongo but not in Auth0)
  console.log(`\n\n${'='.repeat(80)}`);
  console.log(`🔍 CHECKING FOR ORPHANED RECORDS`);
  console.log(`${'='.repeat(80)}\n`);

  const allAuth0Subs = new Set(auth0Users.map(u => u.user_id));
  
  // Check recent user records
  const recentUserRecords = await users.find({})
    .sort({ when: -1 })
    .limit(limit)
    .toArray();
  
  const orphanedUsers = recentUserRecords.filter(u => !allAuth0Subs.has(u._id));
  if (orphanedUsers.length > 0) {
    console.log(`⚠️  Found ${orphanedUsers.length} user records not in recent Auth0 users:`);
    orphanedUsers.forEach(u => {
      console.log(`   - ${u._id} (created: ${u.when})`);
    });
  } else {
    console.log(`✅ No orphaned user records found in recent ${limit} records`);
  }

  await database.disconnect();

  console.log(`\n${'='.repeat(80)}`);
  console.log(`✅ AUDIT COMPLETE`);
  console.log(`${'='.repeat(80)}\n`);

  return results;
}

// Run audit for both tenants
const tenantArg = process.argv[2] || 'both';
const limitArg = parseInt(process.argv[3]) || 10;

console.log(`\n⏰ Audit started at: ${new Date().toISOString()}\n`);

try {
  if (tenantArg === 'both' || tenantArg === 'aesthetic') {
    await auditUserCreationSync('aesthetic', limitArg);
  }
  
  if (tenantArg === 'both' || tenantArg === 'sotce') {
    console.log('\n\n');
    await auditUserCreationSync('sotce', limitArg);
  }
} catch (error) {
  console.error('❌ Audit failed:', error);
  process.exit(1);
}

console.log(`\n⏰ Audit completed at: ${new Date().toISOString()}\n`);
