#!/usr/bin/env node
// Migrate all users' moods to ATProto - optimized approach
// 1. Find users with moods first
// 2. Check if they have ATProto accounts
// 3. Migrate or flag for later
//
// NOTE: 6 users cannot get ATProto accounts and should be skipped:
// - ac23menap: Unsupported email domain (peterpansen@existiert.net)
// - ac23juhud: Handle already taken (duplicate?)
// - ac23sezew, ac23ruhup, ac24zekas, ac24gahoc: Auth0 accounts deleted (404)
// Their moods remain in MongoDB only.

import { connect } from './database.mjs';
import { AtpAgent } from '@atproto/api';
import { writeFile } from 'fs/promises';

const skipUsers = process.argv.filter(arg => arg.startsWith('--skip=')).map(arg => arg.replace('--skip=', ''));
const doIt = process.argv.includes('--migrate');
const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

console.log('='.repeat(80));
console.log('🔄 MIGRATING ALL USERS\' MOODS TO ATPROTO');
if (!doIt) {
  console.log('   [DRY RUN MODE - use --migrate to actually migrate]');
}
if (skipUsers.length > 0) {
  console.log(`   Skipping: ${skipUsers.join(', ')}`);
}
console.log('='.repeat(80));

const database = await connect();
const users = database.db.collection('users');
const moods = database.db.collection('moods');

// Step 1: Find all users who have moods
console.log('\n📋 Step 1: Finding users with moods...');
const usersWithMoods = await moods.aggregate([
  { $match: { deleted: { $ne: true } } },
  { $group: { _id: '$user', count: { $sum: 1 } } },
  { $sort: { count: -1 } }
]).toArray();

console.log(`   Found ${usersWithMoods.length} users with moods`);

// Step 2: Check which have ATProto accounts
console.log('\n📋 Step 2: Checking ATProto account status...');

const stats = {
  totalUsers: usersWithMoods.length,
  withATProto: 0,
  withoutATProto: 0,
  skipped: 0,
  migrated: 0,
  failed: 0,
  totalMoodsMigrated: 0,
};

const usersWithoutATProto = [];
const migrationErrors = [];

// Process each user
for (let i = 0; i < usersWithMoods.length; i++) {
  const { _id: userSub, count: moodCount } = usersWithMoods[i];
  
  // Get user details
  const user = await users.findOne({ _id: userSub });
  const handle = user?.code || 'unknown';
  
  // Check if should skip
  if (skipUsers.includes(handle)) {
    console.log(`\n[${i+1}/${usersWithMoods.length}] @${handle}: ${moodCount} moods - SKIPPED`);
    stats.skipped++;
    continue;
  }
  
  console.log(`\n[${i+1}/${usersWithMoods.length}] @${handle}: ${moodCount} moods`);
  
  // Check ATProto account
  if (!user?.atproto?.did || !user?.atproto?.password) {
    console.log(`   ⚠️  No ATProto account - flagging for later`);
    usersWithoutATProto.push({
      handle,
      sub: userSub,
      moodCount,
      email: user?.email || 'unknown',
    });
    stats.withoutATProto++;
    continue;
  }
  
  stats.withATProto++;
  console.log(`   ✅ Has ATProto account (DID: ${user.atproto.did})`);
  
  if (!doIt) {
    console.log(`   [DRY RUN] Would migrate ${moodCount} moods`);
    continue;
  }
  
  // Migrate this user's moods
  try {
    // Login to ATProto
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password,
    });
    
    // Get user's moods
    const userMoods = await moods.find({ 
      user: userSub,
      deleted: { $ne: true }
    }).sort({ when: 1 }).toArray();
    
    console.log(`   Migrating ${userMoods.length} moods...`);
    
    let migratedCount = 0;
    let failedCount = 0;
    
    for (const mood of userMoods) {
      try {
        // Show mood preview (first 60 chars)
        const preview = mood.mood.substring(0, 60) + (mood.mood.length > 60 ? '...' : '');
        console.log(`      "${preview}"`);
        
        // Create ATProto record
        const record = await agent.com.atproto.repo.createRecord({
          repo: user.atproto.did,
          collection: 'computer.aesthetic.mood',
          record: {
            $type: 'computer.aesthetic.mood',
            mood: mood.mood,
            when: mood.when.toISOString(),
            ref: mood._id.toString(),
          },
        });
        
        const uri = record.uri || record.data?.uri;
        if (!uri) {
          throw new Error('Response missing URI');
        }
        
        const rkey = uri.split('/').pop();
        
        // Update MongoDB
        await moods.updateOne(
          { _id: mood._id },
          { $set: { atproto: { rkey } } }
        );
        
        migratedCount++;
      } catch (error) {
        console.error(`   ❌ Failed to migrate mood ${mood._id}: ${error.message}`);
        failedCount++;
        migrationErrors.push({
          handle,
          moodId: mood._id.toString(),
          error: error.message,
        });
      }
    }
    
    console.log(`   ✅ Migrated ${migratedCount}/${userMoods.length} moods`);
    if (failedCount > 0) {
      console.log(`   ⚠️  Failed: ${failedCount}`);
    }
    
    stats.migrated++;
    stats.totalMoodsMigrated += migratedCount;
    
  } catch (error) {
    console.error(`   ❌ Migration failed: ${error.message}`);
    stats.failed++;
    migrationErrors.push({
      handle,
      error: error.message,
    });
  }
}

// Save reports
console.log('\n📋 Saving reports...');

if (usersWithoutATProto.length > 0) {
  const reportPath = '/workspaces/aesthetic-computer/users-without-atproto.json';
  await writeFile(reportPath, JSON.stringify({
    generatedAt: new Date().toISOString(),
    totalUsers: usersWithoutATProto.length,
    users: usersWithoutATProto,
  }, null, 2));
  console.log(`   📄 Users without ATProto: ${reportPath}`);
}

if (migrationErrors.length > 0) {
  const errorPath = '/workspaces/aesthetic-computer/migration-errors.json';
  await writeFile(errorPath, JSON.stringify({
    generatedAt: new Date().toISOString(),
    totalErrors: migrationErrors.length,
    errors: migrationErrors,
  }, null, 2));
  console.log(`   📄 Migration errors: ${errorPath}`);
}

// Summary
console.log('\n' + '='.repeat(80));
console.log('\n📊 MIGRATION SUMMARY:');
console.log(`   Total users with moods: ${stats.totalUsers}`);
console.log(`   With ATProto accounts: ${stats.withATProto}`);
console.log(`   Without ATProto accounts: ${stats.withoutATProto} ⚠️`);
console.log(`   Skipped: ${stats.skipped}`);

if (doIt) {
  console.log(`\n   Successfully migrated: ${stats.migrated} users`);
  console.log(`   Failed: ${stats.failed} users`);
  console.log(`   Total moods migrated: ${stats.totalMoodsMigrated}`);
  
  if (usersWithoutATProto.length > 0) {
    console.log(`\n⚠️  ${usersWithoutATProto.length} users need ATProto accounts created:`);
    usersWithoutATProto.slice(0, 10).forEach(u => {
      console.log(`   • @${u.handle} (${u.moodCount} moods)`);
    });
    if (usersWithoutATProto.length > 10) {
      console.log(`   ... and ${usersWithoutATProto.length - 10} more (see users-without-atproto.json)`);
    }
  }
} else {
  console.log(`\n   Would migrate: ${stats.withATProto} users`);
  console.log(`   Run with --migrate to perform migration`);
}

console.log('='.repeat(80) + '\n');

await database.disconnect();
process.exit(0);
