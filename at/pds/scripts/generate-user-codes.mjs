#!/usr/bin/env node
// generate-user-codes.mjs
// Retroactively add permanent user code to all existing users

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
  // Check for --dry-run flag
  const isDryRun = process.argv.includes('--dry-run');
  
  if (isDryRun) {
    console.log('🔍 DRY RUN MODE - No database changes will be made\n');
  }
  
  console.log('🔑 Generating permanent user codes for all AC users...\n');
  
  try {
    const database = await connect();
    const handles = database.db.collection('@handles');
    const verifications = database.db.collection('verifications');
    
    // Ensure unique index exists (skip in dry-run)
    if (!isDryRun) {
      await ensureUserCodeIndex(database);
    }
    
    // Strategy: Find all verified users, then check which ones need codes
    // This catches both:
    // 1. Users in @handles without codes (handled users)
    // 2. Users in verifications but NOT in @handles (unhandled users)
    
    const allVerifiedUsers = await verifications.find({}).toArray();
    console.log(`📊 Found ${allVerifiedUsers.length} total verified users`);
    
    const usersNeedingCodes = [];
    
    for (const verified of allVerifiedUsers) {
      const userId = verified._id;
      const handleRecord = await handles.findOne({ _id: userId });
      
      // User needs a code if:
      // - They have no @handles record at all, OR
      // - They have a @handles record but no code field
      if (!handleRecord || !handleRecord.code) {
        usersNeedingCodes.push({
          _id: userId,
          handle: handleRecord?.handle,
          created_at: handleRecord?.created_at || handleRecord?.createdAt,
          existsInHandles: !!handleRecord
        });
      }
    }
    
    console.log(`📊 Found ${usersNeedingCodes.length} users without codes`);
    console.log(`   - ${usersNeedingCodes.filter(u => u.handle).length} with handles`);
    console.log(`   - ${usersNeedingCodes.filter(u => !u.handle).length} without handles\n`);
    
    if (usersNeedingCodes.length === 0) {
      console.log('✅ All users already have codes!');
      await database.disconnect();
      return;
    }
    
    let successCount = 0;
    let errorCount = 0;
    
    for (const user of usersNeedingCodes) {
      try {
        // Determine creation date (use existing field or default to now)
        const createdAt = user.created_at || 
                         user.createdAt || 
                         user._id.getTimestamp?.() || // Extract from ObjectId if available
                         new Date(); // Fallback to now
        
        // Generate unique code based on creation year
        const userCode = await generateUniqueUserCode(database, createdAt);
        
        // Update or insert user record (skip in dry-run)
        if (!isDryRun) {
          if (user.existsInHandles) {
            // User already in @handles, just add code
            await handles.updateOne(
              { _id: user._id },
              { 
                $set: { 
                  code: userCode,
                  code_created_at: new Date()
                } 
              }
            );
          } else {
            // User not in @handles yet, create entry with code
            await handles.insertOne({ 
              _id: user._id,
              code: userCode,
              code_created_at: new Date(),
              created_at: createdAt
            });
          }
        }
        
        const handle = user.handle ? `@${user.handle}` : '(no handle)';
        const year = createdAt.getFullYear();
        const prefix = isDryRun ? '🔍' : '✅';
        const action = user.existsInHandles ? 'updated' : 'created';
        console.log(`${prefix} ${userCode} → ${handle} (${user._id}) [${year}] [${action}]`);
        successCount++;
        
      } catch (error) {
        console.error(`❌ Failed for ${user._id}:`, error.message);
        errorCount++;
      }
    }
    
    console.log('\n═══════════════════════════════════════════');
    if (isDryRun) {
      console.log('🔍 DRY RUN COMPLETE - No changes written to database');
    }
    console.log(`✅ Success: ${successCount}`);
    if (errorCount > 0) {
      console.log(`❌ Errors:  ${errorCount}`);
    }
    console.log('═══════════════════════════════════════════\n');
    
    if (isDryRun) {
      console.log('To apply these changes, run without --dry-run flag:\n');
      console.log('  node generate-user-codes.mjs\n');
    }
    
    await database.disconnect();
    
  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

main();
