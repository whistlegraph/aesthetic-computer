#!/usr/bin/env node
// test-create-jeffrey.mjs
// Create jeffrey's ATProto account and store DID in MongoDB

import { AtpAgent } from '@atproto/api';
import { connect } from '../../../system/backend/database.mjs';
import { userEmailFromID } from '../../../system/backend/authorization.mjs';
import { shell } from '../../../system/backend/shell.mjs';

const PDS_URL = 'https://pds.aesthetic.computer';
const JEFFREY_SUB = 'auth0|63effeeb2a7d55f8098d62f9';

async function main() {
  console.log('🦋 Creating ATProto account for @jeffrey...\n');
  
  const agent = new AtpAgent({ service: PDS_URL });
  
  try {
    // Get jeffrey's email from Auth0
    const emailResult = await userEmailFromID(JEFFREY_SUB, 'aesthetic');
    if (!emailResult?.email) {
      console.error('❌ Could not fetch email from Auth0');
      process.exit(1);
    }
    const JEFFREY_EMAIL = emailResult.email;
    
    // First, check if jeffrey already has an ATProto account
    const database = await connect();
    const handles = database.db.collection('@handles');
    const jeffrey = await handles.findOne({ _id: JEFFREY_SUB });
    
    if (jeffrey?.atproto_did) {
      console.log('⚠️  Jeffrey already has an ATProto account!');
      console.log(`   DID: ${jeffrey.atproto_did}`);
      console.log(`   Handle: ${jeffrey.atproto_handle || 'unknown'}`);
      console.log(`   Email: ${JEFFREY_EMAIL}`);
      await database.disconnect();
      return;
    }
    
    console.log(`📧 Email: ${JEFFREY_EMAIL}`);
    console.log(`🆔 Sub:   ${JEFFREY_SUB}\n`);
    
    // Generate a secure random password
    const crypto = await import('crypto');
    const password = 'ac-' + crypto.randomBytes(16).toString('hex');
    
    // Create account with temporary PDS handle
    const response = await agent.createAccount({
      email: JEFFREY_EMAIL,
      handle: 'jeffrey.pds.aesthetic.computer',
      password: password,
      inviteCode: process.argv[2], // Pass invite code as first argument
    });
    
    const { did, handle } = response.data;
    
    console.log('✅ Account Created Successfully!\n');
    console.log('═══════════════════════════════════════════');
    console.log(`📧 Email:    ${JEFFREY_EMAIL}`);
    console.log(`🌐 Handle:   ${handle}`);
    console.log(`🔑 DID:      ${did}`);
    console.log(`🔒 Password: ${password}`);
    console.log('═══════════════════════════════════════════');
    
    // Store DID in MongoDB
    await handles.updateOne(
      { _id: JEFFREY_SUB },
      {
        $set: {
          atproto_did: did,
          atproto_handle: handle,
          atproto_created_at: new Date(),
          atproto_password: password, // Temporary storage
        }
      },
      { upsert: true }
    );
    
    console.log('\n💾 Stored DID in MongoDB @handles collection');
    console.log(`   Collection: @handles`);
    console.log(`   Document ID: ${JEFFREY_SUB}`);
    console.log(`   Field: atproto_did = "${did}"`);
    
    await database.disconnect();
    
    console.log('\n🔄 Next Steps:');
    console.log('   1. Run: node pds/scripts/assign-handle.mjs jeffrey');
    console.log('   2. This will map @jeffrey → your DID via DNS');
    console.log('   3. Then you can login as @jeffrey.aesthetic.computer in any Bluesky app!');
    
  } catch (error) {
    console.error('\n❌ Error creating account:');
    console.error(error.message);
    if (error.cause) {
      console.error('\nDetails:', JSON.stringify(error.cause, null, 2));
    }
    if (error.message.includes('invite code')) {
      console.error('\n💡 You need an invite code! Generate one with:');
      console.error('   ac-pds account create-invite-code --uses 1');
      console.error('\nThen run this script again with the invite code:');
      console.error('   node test-create-jeffrey.mjs <invite-code>');
    }
    process.exit(1);
  }
}

main();
