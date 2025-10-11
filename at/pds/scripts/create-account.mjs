#!/usr/bin/env node
// create-account.mjs
// Creates an ATProto PDS account for an AC user and stores their DID

import { AtpAgent } from '@atproto/api';
import { connect } from '../../../system/backend/database.mjs';
import { userEmailFromID } from '../../../system/backend/authorization.mjs';
import { shell } from '../../../system/backend/shell.mjs';
import crypto from 'crypto';

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';
const PDS_ADMIN_PASSWORD = process.env.PDS_ADMIN_PASSWORD;

if (!PDS_ADMIN_PASSWORD) {
  console.error('❌ PDS_ADMIN_PASSWORD environment variable is required');
  process.exit(1);
}

/**
 * Generate an invite code via admin API
 * @returns {Promise<string>}
 */
async function generateInviteCode() {
  const auth = Buffer.from(`admin:${PDS_ADMIN_PASSWORD}`).toString('base64');
  
  const response = await fetch(`${PDS_URL}/xrpc/com.atproto.server.createInviteCode`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Basic ${auth}`,
    },
    body: JSON.stringify({ useCount: 1 }),
  });
  
  if (!response.ok) {
    throw new Error(`Failed to create invite code: ${response.statusText}`);
  }
  
  const data = await response.json();
  return data.code;
}

/**
 * Generate a secure random password
 * @returns {Promise<string>}
 */
async function generatePassword() {
  const chars = 'ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789!@#$%&*';
  let password = '';
  for (let i = 0; i < 32; i++) {
    const randomIndex = crypto.randomInt(0, chars.length);
    password += chars[randomIndex];
  }
  return password;
}

/**
 * Creates an ATProto account for a user
 * @param {string} sub - Auth0 user ID (e.g., "auth0|12345")
 * @param {object} options - Optional parameters
 * @param {string} options.email - Override email (if not in Auth0)
 * @param {string} options.password - Custom password (otherwise auto-generated)
 * @param {string} options.inviteCode - PDS invite code (optional for v0.4)
 * @returns {Promise<object>} - { did, handle, email, password, success, error }
 */
export async function createAccount(sub, options = {}) {
  try {
    shell.log(`🦋 Creating ATProto account for: ${sub}`);
    
    // 1. Get user data from database
    const database = await connect();
    const users = database.db.collection('users');
    const handles = database.db.collection('@handles');
    
    const userBefore = await users.findOne({ _id: sub });
    const handleRecord = await handles.findOne({ _id: sub });
    
    shell.log(`\n📋 User record BEFORE:`);
    shell.log(JSON.stringify(userBefore, null, 2));
    
    // 2. Get user email from Auth0
    let email = options.email;
    let tenant = 'aesthetic';
    
    if (!email) {
      shell.log(`🔍 Fetching email from Auth0 for: ${sub}`);
      
      // Detect tenant from sub prefix
      tenant = sub.startsWith('sotce-') ? 'sotce' : 'aesthetic';
      shell.log(`   Tenant: ${tenant}`);
      
      const result = await userEmailFromID(sub, tenant);
      shell.log(`   Result:`, result);
      
      if (!result?.email) {
        throw new Error(`No email found for user: ${sub} (tenant: ${tenant})`);
      }
      email = result.email;
    } else {
      // If email is provided directly, still detect tenant for potential modification
      tenant = sub.startsWith('sotce-') ? 'sotce' : 'aesthetic';
    }
    
    shell.log(`📧 Email: ${email}`);
    
    // 3. Generate a secure random password (or use provided one)
    const password = options.password || await generatePassword();
    
    // 4. Determine handle: use AC handle if exists, otherwise use user code
    // Format: {handle}.at.aesthetic.computer or {code}.at.aesthetic.computer
    // Note: Sanitize handle for ATProto compatibility (dots and underscores → dashes)
    let pdsHandle;
    if (handleRecord?.handle) {
      const sanitizedHandle = handleRecord.handle.replace(/[._]/g, '-');
      pdsHandle = `${sanitizedHandle}.at.aesthetic.computer`;
      if (sanitizedHandle !== handleRecord.handle) {
        shell.log(`🏷️  Handle: ${pdsHandle} (from AC handle @${handleRecord.handle}, sanitized)`);
      } else {
        shell.log(`🏷️  Handle: ${pdsHandle} (from AC handle @${handleRecord.handle})`);
      }
    } else {
      pdsHandle = `${userBefore.code}.at.aesthetic.computer`;
      shell.log(`🏷️  Handle: ${pdsHandle} (from user code)`);
    }
    
    // 5. Generate invite code
    let inviteCode = options.inviteCode;
    if (!inviteCode) {
      shell.log(`🎫 Generating invite code...`);
      inviteCode = await generateInviteCode();
      shell.log(`   Code: ${inviteCode}`);
    }
    
    // 6. Create account on PDS
    const agent = new AtpAgent({ service: PDS_URL });
    
    let accountCreated = false;
    let finalDid, finalHandle;
    let attempts = 0;
    let currentEmail = email;
    
    while (!accountCreated && attempts < 3) {
      attempts++;
      
      try {
        const response = await agent.createAccount({
          email: currentEmail,
          handle: pdsHandle,
          password,
          inviteCode,
        });
        
        finalDid = response.data.did;
        finalHandle = response.data.handle;
        accountCreated = true;
        
      } catch (error) {
        // Handle duplicate email by appending tenant
        if (error.message.includes('Email already taken') && 
            attempts === 1 && 
            tenant === 'sotce') {
          
          shell.log(`⚠️  Email "${currentEmail}" already taken`);
          shell.log(`🔄 Trying with tenant suffix...`);
          
          // Append +sotce to email (before @)
          const [localPart, domain] = currentEmail.split('@');
          currentEmail = `${localPart}+sotce@${domain}`;
          shell.log(`   New email: ${currentEmail}`);
          
        }
        // If handle is reserved/invalid/too short/taken and we haven't tried user code yet
        else if ((error.message.includes('Reserved handle') || 
             error.message.includes('Invalid handle') ||
             error.message.includes('must be a valid handle') ||
             error.message.includes('Handle too short') ||
             error.message.includes('Handle already taken')) && 
            attempts <= 2 && 
            userBefore?.code) {
          
          shell.log(`⚠️  Handle "${pdsHandle}" failed: ${error.message}`);
          shell.log(`🔄 Falling back to user code...`);
          
          // Try again with user code as handle
          pdsHandle = `${userBefore.code}.at.aesthetic.computer`;
          shell.log(`   New handle: ${pdsHandle}`);
          
        } else {
          throw error;
        }
      }
    }
    
    if (!accountCreated) {
      throw new Error('Failed to create account after all attempts');
    }
    
    shell.log(`\n✅ Account Created!`);
    shell.log(`   DID: ${finalDid}`);
    shell.log(`   Handle: ${finalHandle}`);
    if (currentEmail !== email) {
      shell.log(`   Email: ${currentEmail} (modified from ${email})`);
    }
    
    // 6. Store atproto data in MongoDB (nested structure)
    const atprotoData = {
      did: finalDid,
      handle: finalHandle,
      password: password,
      created: new Date().toISOString(),
    };
    
    await users.updateOne(
      { _id: sub },
      { $set: { atproto: atprotoData } },
      { upsert: true }
    );
    
    shell.log(`\n💾 Stored in MongoDB`);
    shell.log(`   Collection: users`);
    shell.log(`   Document: ${sub}`);
    
    // 7. Get updated record to show
    const userAfter = await users.findOne({ _id: sub });
    shell.log(`\n📋 User record AFTER:`);
    shell.log(JSON.stringify(userAfter, null, 2));
    
    await database.disconnect();
    
    return {
      success: true,
      did: finalDid,
      handle: finalHandle,
      email: currentEmail, // Return the email that was actually used
      originalEmail: currentEmail !== email ? email : undefined,
      password,
    };
    
  } catch (error) {
    shell.error('❌ Failed to create account:', error.message);
    return {
      success: false,
      error: error.message,
    };
  }
}

// CLI usage
if (import.meta.url === `file://${process.argv[1]}`) {
  const sub = process.argv[2];
  
  if (!sub) {
    console.error('Usage: node create-account.mjs <auth0_sub> [invite_code]');
    console.error('Example: node create-account.mjs "auth0|63effeeb2a7d55f8098d62f9"');
    process.exit(1);
  }
  
  const inviteCode = process.argv[3];
  const result = await createAccount(sub, { inviteCode });
  
  if (!result.success) {
    console.error('\n❌ Error:', result.error);
    process.exit(1);
  }
  
  process.exit(0);
}
