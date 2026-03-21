#!/usr/bin/env node
// migrate-pds-accounts.mjs - Recreate PDS accounts for users with stale .pds. handles

import { connect } from '../../../system/backend/database.mjs';
import { createAtprotoAccount } from '../../../system/backend/at.mjs';
import { shell } from '../../../system/backend/shell.mjs';

const DRY_RUN = process.argv.includes('--dry-run');
const limitArg = process.argv.find(arg => arg.startsWith('--limit='));
const LIMIT = limitArg ? parseInt(limitArg.split('=')[1]) : null;

async function main() {
  try {
    console.log('\n🔄 Migrating stale .pds. ATProto accounts...\n');
    
    if (DRY_RUN) {
      console.log('🔍 DRY RUN MODE - No changes will be made\n');
    }
    
    if (LIMIT) {
      console.log(`⚠️  LIMITED TO ${LIMIT} accounts\n`);
    }
    
    const database = await connect();
    const users = database.db.collection('users');
    
    // Find all users with .pds. handles
    const query = users.find({
      'atproto.handle': { $regex: '\\.pds\\.aesthetic\\.computer$' }
    });
    
    const staleUsers = LIMIT ? await query.limit(LIMIT).toArray() : await query.toArray();
    
    console.log(`📊 Found ${staleUsers.length} users with .pds. handles\n`);
    
    let successful = 0;
    let failed = 0;
    let skipped = 0;
    
    for (let i = 0; i < staleUsers.length; i++) {
      const user = staleUsers[i];
      const sub = user._id;
      const oldHandle = user.atproto.handle;
      const oldDid = user.atproto.did;
      
      console.log(`\n[${i + 1}/${staleUsers.length}] ${sub}`);
      console.log(`   Old handle: ${oldHandle}`);
      console.log(`   Old DID: ${oldDid}`);
      
      // Verify account doesn't exist on PDS
      try {
        const checkResponse = await fetch(
          `https://at.aesthetic.computer/xrpc/com.atproto.repo.describeRepo?repo=${oldDid}`
        );
        
        if (checkResponse.ok) {
          console.log('   ✅ Account still exists on PDS - skipping');
          skipped++;
          continue;
        }
      } catch (error) {
        console.log('   ⚠️  Error checking PDS:', error.message);
      }
      
      if (DRY_RUN) {
        console.log('   🔍 Would recreate account');
        continue;
      }
      
      // Remove old atproto data first
      await users.updateOne(
        { _id: sub },
        { $unset: { atproto: '' } }
      );
      
      // Try to recreate the account
      try {
        const result = await createAtprotoAccount(database, sub);
        
        if (result.created) {
          console.log(`   ✅ Created: ${result.handle}`);
          console.log(`   New DID: ${result.did}`);
          successful++;
        } else {
          console.log(`   ❌ Failed: ${result.reason || result.error}`);
          failed++;
        }
      } catch (error) {
        console.log(`   ❌ Error: ${error.message}`);
        failed++;
      }
      
      // Small delay to avoid rate limiting
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    
    await database.disconnect();
    
    console.log('\n\n📊 Migration Summary:');
    console.log(`   Total: ${staleUsers.length}`);
    console.log(`   ✅ Successful: ${successful}`);
    console.log(`   ❌ Failed: ${failed}`);
    console.log(`   ⏭️  Skipped (already exist): ${skipped}`);
    
    if (DRY_RUN) {
      console.log('\n💡 Run without --dry-run to apply changes\n');
    }
    
  } catch (error) {
    console.error('\n❌ Error:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
