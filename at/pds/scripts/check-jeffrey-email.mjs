#!/usr/bin/env node
// check-jeffrey-email.mjs
// Check what email is stored for jeffrey in Auth0

import { userEmailFromID } from '../../../system/backend/authorization.mjs';
import { shell } from '../../../system/backend/shell.mjs';

const JEFFREY_SUB = 'auth0|63effeeb2a7d55f8098d62f9';

async function main() {
  try {
    console.log('📧 Checking email for jeffrey...\n');
    
    const result = await userEmailFromID(JEFFREY_SUB, 'aesthetic');
    
    if (result?.email) {
      console.log('✅ Found email:');
      console.log(`   Email: ${result.email}`);
      console.log(`   Verified: ${result.email_verified ? 'Yes' : 'No'}`);
    } else {
      console.log('❌ No email found for jeffrey');
    }
    
  } catch (error) {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  }
}

main();
