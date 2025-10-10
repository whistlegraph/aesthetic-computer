#!/usr/bin/env node
// migrate-to-users-collection.mjs
// Refactor database to separate users (identity) from @handles (namespace)

import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect } from '../../../system/backend/database.mjs';
import { shell } from '../../../system/backend/shell.mjs';
import { generateUniqueUserCode, ensureUserCodeIndex } from '../../../system/public/aesthetic.computer/lib/user-code.mjs';

// Load environment from vault
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const vaultEnvPath = join(__dirname, '../../../aesthetic-computer-vault/at/.env');
config({ path: vaultEnvPath });

async function main() {
  const isDryRun = process.argv.includes('--dry-run');
  
  if (isDryRun) {
    console.log('🔍 DRY RUN MODE - No database changes will be made\n');
  }
  
  console.log('🔄 Migrating to users collection structure...\n');
  console.log('New structure:');
  console.log('  - users: identity (sub, code, atproto_did, etc.)');
  console.log('  - @handles: namespace (handle → user sub)\n');
  
  try {
    const database = await connect();
    const users = database.db.collection('users');
    const handles = database.db.collection('@handles');
    const verifications = database.db.collection('verifications');
    
    // Ensure unique index on code field in users collection
    if (!isDryRun) {
      console.log('📇 Creating unique index on users.code...');
      await users.createIndex({ code: 1 }, { unique: true, sparse: true });
      console.log('✅ Index created\n');
    }
    
    // Step 1: Find all users (from both verifications AND @handles)
    console.log('📊 Analyzing existing data...\n');
    const allVerifiedUsers = await verifications.find({}).toArray();
    const allHandledUsers = await handles.find({}).toArray();
    
    // Create a Set of all unique user IDs
    const allUserIds = new Set();
    allVerifiedUsers.forEach(v => allUserIds.add(v._id));
    allHandledUsers.forEach(h => allUserIds.add(h._id));
    
    console.log(`Found ${allVerifiedUsers.length} verified users`);
    console.log(`Found ${allHandledUsers.length} handled users`);
    console.log(`Total unique users: ${allUserIds.size}\n`);
    
    let stats = {
      usersCreated: 0,
      usersUpdated: 0,
      handlesCreaned: 0,
      codesGenerated: 0,
      verificationsCreated: 0,
      errors: 0
    };
    
    const total = allUserIds.size;
    let processed = 0;
    
    // Step 2: Process each unique user
    for (const userId of allUserIds) {
      processed++;
      
      // Show progress bar
      const percent = Math.floor((processed / total) * 100);
      const filledBars = Math.floor(percent / 2);
      const emptyBars = 50 - filledBars;
      const bar = '█'.repeat(filledBars) + '░'.repeat(emptyBars);
      process.stdout.write(`\r[${bar}] ${percent}% (${processed}/${total}) - ${stats.codesGenerated} codes generated`);
      
      try {
        const handleRecord = await handles.findOne({ _id: userId });
        const existingUser = await users.findOne({ _id: userId });
        const verificationRecord = await verifications.findOne({ _id: userId });
        
        // If user has a handle but no verification, create one
        if (handleRecord?.handle && !verificationRecord && !isDryRun) {
          await verifications.insertOne({
            _id: userId,
            count: 1
          });
          stats.verificationsCreated++;
        }
        
        // Determine creation date (use 'when' field if exists)
        let createdAt;
        if (handleRecord?.when) {
          createdAt = handleRecord.when;
        } else if (handleRecord?.created_at || handleRecord?.createdAt) {
          createdAt = handleRecord.created_at || handleRecord.createdAt;
        } else if (handleRecord?.atproto_created_at) {
          createdAt = new Date(handleRecord.atproto_created_at);
        } else {
          // Try to extract from ObjectId
          try {
            createdAt = userId.getTimestamp?.() || new Date();
          } catch {
            createdAt = new Date();
          }
        }
        
        // Generate code if user doesn't have one
        let userCode;
        if (existingUser?.code) {
          userCode = existingUser.code;
        } else if (handleRecord?.code) {
          userCode = handleRecord.code;
        } else {
          userCode = await generateUniqueUserCode(database, createdAt);
          stats.codesGenerated++;
        }
        
        // Build user record (identity data)
        const userDoc = {
          _id: userId,
          code: userCode,
          when: createdAt,
        };
        
        // Add ATProto data if exists
        if (handleRecord?.atproto_did) {
          userDoc.atproto_did = handleRecord.atproto_did;
          userDoc.atproto_handle = handleRecord.atproto_handle;
          userDoc.atproto_created_at = handleRecord.atproto_created_at;
          userDoc.atproto_password = handleRecord.atproto_password;
        }
        
        // Create or update user record
        if (!isDryRun) {
          await users.updateOne(
            { _id: userId },
            { $set: userDoc },
            { upsert: true }
          );
        }
        
        if (existingUser) {
          stats.usersUpdated++;
        } else {
          stats.usersCreated++;
        }
        
        // Clean up @handles record if it exists
        if (handleRecord) {
          if (handleRecord.handle) {
            // User has a handle - keep the simple structure: { _id: sub, handle: name }
            // Remove any ATProto cruft that may have been added
            const cleanHandleDoc = {
              _id: userId,
              handle: handleRecord.handle
            };
            
            if (!isDryRun) {
              // Replace with clean version (removing atproto_*, created_at, etc.)
              await handles.replaceOne(
                { _id: userId },
                cleanHandleDoc
              );
            }
            
            stats.handlesCreaned++;
          } else {
            // User has no handle - just delete the @handles record
            if (!isDryRun) {
              await handles.deleteOne({ _id: userId });
            }
          }
        }
        
      } catch (error) {
        console.error(`\n❌ Failed for ${userId}:`, error.message);
        stats.errors++;
      }
    }
    
    // Clear progress bar and show final stats
    console.log('\n\n═══════════════════════════════════════════');
    if (isDryRun) {
      console.log('🔍 DRY RUN COMPLETE - No changes written');
    }
    console.log('📊 Migration Statistics:');
    console.log(`   Users created:          ${stats.usersCreated}`);
    console.log(`   Users updated:          ${stats.usersUpdated}`);
    console.log(`   Handles cleaned:        ${stats.handlesCreaned}`);
    console.log(`   Codes generated:        ${stats.codesGenerated}`);
    console.log(`   Verifications created:  ${stats.verificationsCreated}`);
    if (stats.errors > 0) {
      console.log(`   ❌ Errors:              ${stats.errors}`);
    }
    console.log('═══════════════════════════════════════════\n');
    
    if (isDryRun) {
      console.log('New structure preview:');
      console.log('  users collection:');
      console.log('    { _id: "auth0|...", code: "ac25wila", atproto_did: "...", ... }');
      console.log('  @handles collection (unchanged):');
      console.log('    { _id: "auth0|...", handle: "jeffrey" }\n');
      console.log('To apply changes, run without --dry-run:\n');
      console.log('  node migrate-to-users-collection.mjs\n');
    } else {
      console.log('✅ Migration complete!\n');
    }
    
    await database.disconnect();
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
