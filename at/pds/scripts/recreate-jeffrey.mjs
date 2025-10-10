#!/usr/bin/env node
// recreate-jeffrey.mjs
// Delete the old account and create a new one with correct email

import { spawn } from 'child_process';
import { AtpAgent } from '@atproto/api';
import { connect } from '../../../system/backend/database.mjs';
import { userEmailFromID } from '../../../system/backend/authorization.mjs';

const PDS_URL = 'https://pds.aesthetic.computer';
const JEFFREY_SUB = 'auth0|63effeeb2a7d55f8098d62f9';
const OLD_DID = 'did:plc:5bfzht3pdvwzqoyisuznw2xk';

async function main() {
  const inviteCode = process.argv[2];
  
  if (!inviteCode) {
    console.log('❌ Please provide an invite code');
    console.log('Usage: node recreate-jeffrey.mjs <invite-code>');
    process.exit(1);
  }
  
  console.log('🗑️  Deleting old account...\n');
  
  // Delete old account via SSH
  const deleteCmd = spawn('ssh', [
    '-o', 'StrictHostKeyChecking=no',
    '-i', process.env.HOME + '/.ssh/aesthetic_pds',
    'root@138.197.35.160',
    `echo "y" | pdsadmin account delete ${OLD_DID}`
  ]);
  
  await new Promise((resolve) => {
    deleteCmd.on('close', resolve);
  });
  
  console.log('\n✅ Old account deleted\n');
  console.log('🦋 Creating new account with correct email...\n');
  
  try {
    // Get jeffrey's real email from Auth0
    const emailResult = await userEmailFromID(JEFFREY_SUB, 'aesthetic');
    if (!emailResult?.email) {
      console.error('❌ Could not fetch email from Auth0');
      process.exit(1);
    }
    const email = emailResult.email;
    
    console.log(`📧 Email: ${email}`);
    console.log(`🆔 Sub:   ${JEFFREY_SUB}\n`);
    
    // Generate a secure random password
    const crypto = await import('crypto');
    const password = 'ac-' + crypto.randomBytes(16).toString('hex');
    
    // Create account with temporary PDS handle
    const agent = new AtpAgent({ service: PDS_URL });
    const response = await agent.createAccount({
      email: email,
      handle: 'jeffrey.pds.aesthetic.computer',
      password: password,
      inviteCode: inviteCode,
    });
    
    const { did, handle } = response.data;
    
    console.log('✅ Account Created Successfully!\n');
    console.log('═══════════════════════════════════════════');
    console.log(`📧 Email:    ${email}`);
    console.log(`🌐 Handle:   ${handle}`);
    console.log(`🔑 DID:      ${did}`);
    console.log(`🔒 Password: ${password}`);
    console.log('═══════════════════════════════════════════');
    
    // Update MongoDB
    const database = await connect();
    const handles = database.db.collection('@handles');
    
    await handles.updateOne(
      { _id: JEFFREY_SUB },
      {
        $set: {
          atproto_did: did,
          atproto_handle: handle,
          atproto_created_at: new Date(),
          atproto_password: password,
        }
      },
      { upsert: true }
    );
    
    console.log('\n💾 Updated MongoDB with new DID');
    await database.disconnect();
    
    console.log('\n🔄 Next Steps:');
    console.log('   1. Run: node pds/scripts/assign-handle.mjs jeffrey');
    console.log('   2. This will map @jeffrey → your DID via DNS');
    console.log('   3. Login as @jeffrey.aesthetic.computer in Bluesky!');
    
  } catch (error) {
    console.error('\n❌ Error:', error.message);
    process.exit(1);
  }
}

main();
