#!/usr/bin/env node
// migrate-moods-to-atproto.mjs
// Migrate existing MongoDB moods to ATProto with bi-directional sync

import { connect } from '../../system/backend/database.mjs';
import { handleFor, userIDFromHandle } from '../../system/backend/authorization.mjs';
import { shell } from '../../system/backend/shell.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config();

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

async function getUserDid(database, sub) {
  const users = database.db.collection('users');
  const user = await users.findOne({ _id: sub });
  
  if (!user?.atproto?.did) {
    throw new Error(`User ${sub} has no ATProto account (no DID found)`);
  }
  
  return {
    did: user.atproto.did,
    password: user.atproto.password
  };
}

async function migrateMoodsForUser(handle, dryRun = true) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`🦋 ${dryRun ? 'SIMULATING' : 'MIGRATING'} MOODS FOR @${handle}`);
  console.log(`${'='.repeat(80)}\n`);

  const database = await connect();
  
  try {
    // 1. Get user sub from handle
    console.log(`📋 Step 1: Looking up user @${handle}...`);
    const sub = await userIDFromHandle(handle, database);
    
    if (!sub) {
      console.log(`❌ No user found with handle: @${handle}`);
      return;
    }
    
    console.log(`   User sub: ${sub}`);
    
    // 2. Get user's ATProto credentials
    console.log(`\n📋 Step 2: Getting ATProto credentials...`);
    const { did, password } = await getUserDid(database, sub);
    console.log(`   DID: ${did}`);
    
    // 3. Login to ATProto
    console.log(`\n📋 Step 3: Logging into ATProto PDS...`);
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({ identifier: did, password });
    console.log(`   ✅ Logged in to PDS`);
    
    // 4. Find unsynced moods
    console.log(`\n📋 Step 4: Finding moods to migrate...`);
    const moods = database.db.collection('moods');
    
    const unsyncedMoods = await moods.find({
      user: sub,
      deleted: { $ne: true },
      'atproto.rkey': { $exists: false }  // Not yet synced
    }).sort({ when: 1 }).toArray();  // Oldest first
    
    console.log(`   Found ${unsyncedMoods.length} unsynced moods`);
    
    if (unsyncedMoods.length === 0) {
      console.log(`\n✅ All moods already synced!`);
      return;
    }
    
    // 5. Migrate each mood
    console.log(`\n📋 Step 5: ${dryRun ? 'Simulating' : 'Migrating'} moods...`);
    
    let successCount = 0;
    let failureCount = 0;
    
    for (let i = 0; i < unsyncedMoods.length; i++) {
      const mood = unsyncedMoods[i];
      
      console.log(`\n   [${i + 1}/${unsyncedMoods.length}] Mood ID: ${mood._id}`);
      console.log(`      Mood: "${mood.mood.substring(0, 50)}${mood.mood.length > 50 ? '...' : ''}"`);
      console.log(`      Date: ${mood.when.toISOString()}`);
      
      if (!dryRun) {
        try {
          // Create ATProto record
          const record = await agent.com.atproto.repo.createRecord({
            repo: did,
            collection: 'computer.aesthetic.mood',
            record: {
              $type: 'computer.aesthetic.mood',
              mood: mood.mood,
              when: mood.when.toISOString(),
              mongoId: mood._id.toString()
            }
          });
          
          const rkey = record.uri.split('/').pop();
          console.log(`      ✅ Created ATProto record: ${rkey}`);
          
          // Update MongoDB with ATProto reference
          await moods.updateOne(
            { _id: mood._id },
            { 
              $set: { 
                atproto: { rkey }
              }
            }
          );
          
          console.log(`      ✅ Updated MongoDB with rkey`);
          successCount++;
          
        } catch (error) {
          console.log(`      ❌ Failed: ${error.message}`);
          failureCount++;
        }
      } else {
        console.log(`      [DRY RUN] Would create ATProto record`);
        console.log(`      [DRY RUN] Would update MongoDB with rkey`);
        successCount++;
      }
    }
    
    // 6. Summary
    console.log(`\n${'─'.repeat(80)}`);
    console.log(`\n📊 Summary:`);
    console.log(`   Total moods: ${unsyncedMoods.length}`);
    console.log(`   ${dryRun ? 'Would migrate' : 'Migrated'}: ${successCount}`);
    if (!dryRun && failureCount > 0) {
      console.log(`   Failed: ${failureCount}`);
    }
    console.log(`${'─'.repeat(80)}\n`);
    
    if (dryRun) {
      console.log(`💡 Run with --migrate flag to actually perform migration`);
    } else {
      console.log(`✅ Migration complete!`);
    }
    
  } catch (error) {
    console.error(`\n❌ Error: ${error.message}`);
    console.error(error.stack);
    throw error;
  } finally {
    await database.disconnect();
  }
}

// Parse command line args
const args = process.argv.slice(2);
const handle = args[0] || 'jeffrey';
const dryRun = !args.includes('--migrate');

if (dryRun) {
  console.log('\n⚠️  DRY RUN MODE - No changes will be made');
  console.log('   Add --migrate flag to actually migrate moods\n');
}

try {
  await migrateMoodsForUser(handle, dryRun);
} catch (error) {
  console.error('❌ Migration script failed:', error);
  process.exit(1);
}
