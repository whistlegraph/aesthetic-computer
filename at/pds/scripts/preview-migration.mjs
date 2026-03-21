#!/usr/bin/env node
// preview-migration.mjs - Preview migration for specific user

import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect } from '../../../system/backend/database.mjs';
import { generateUniqueUserCode } from '../../../system/public/aesthetic.computer/lib/user-code.mjs';

// Load environment from vault
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const vaultEnvPath = join(__dirname, '../../../aesthetic-computer-vault/at/.env');
config({ path: vaultEnvPath });

const handle = process.argv[2] || 'jeffrey';

try {
  const database = await connect();
  const users = database.db.collection('users');
  const handles = database.db.collection('@handles');
  
  console.log(`\n🔍 Preview migration for @${handle}\n`);
  
  // Find current handle record
  const handleRecord = await handles.findOne({ handle });
  
  if (!handleRecord) {
    console.log(`❌ No user found with handle: ${handle}`);
    process.exit(1);
  }
  
  const userId = handleRecord._id;
  
  console.log('CURRENT STATE:');
  console.log('==============\n');
  console.log('@handles collection (keyed by user sub):');
  console.log(JSON.stringify(handleRecord, null, 2));
  
  // Determine creation date
  let createdAt = handleRecord.created_at || 
                  handleRecord.createdAt || 
                  handleRecord.atproto_created_at ||
                  new Date();
  
  // Generate code
  const userCode = await generateUniqueUserCode(database, new Date(createdAt));
  
  console.log('\n\nAFTER MIGRATION:');
  console.log('================\n');
  
  // Show new users record
  const userDoc = {
    _id: userId,
    code: userCode,
    created_at: createdAt,
    code_created_at: new Date(),
  };
  
  if (handleRecord.atproto_did) {
    userDoc.atproto_did = handleRecord.atproto_did;
    userDoc.atproto_handle = handleRecord.atproto_handle;
    userDoc.atproto_created_at = handleRecord.atproto_created_at;
    userDoc.atproto_password = handleRecord.atproto_password;
  }
  
  console.log('users collection (identity):');
  console.log(JSON.stringify(userDoc, null, 2));
  
  // Show new handles record - keep the simple structure!
  const cleanHandleDoc = {
    _id: userId,      // Still keyed by user sub
    handle: handle    // Just the handle name
  };
  
  console.log('\n@handles collection (cleaned):');
  console.log(JSON.stringify(cleanHandleDoc, null, 2));
  
  console.log('\n✨ Clean separation achieved!');
  console.log('   - Identity data (code, ATProto) → users collection');
  console.log('   - Handle namespace (simple!) → @handles: { _id: sub, handle: name }');
  console.log('   - ATProto cruft removed from @handles\n');
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
