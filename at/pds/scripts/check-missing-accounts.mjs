#!/usr/bin/env node

// Check which users are missing ATProto accounts

import { connect } from '../../../system/backend/database.mjs';
import { shell } from '../../../system/backend/shell.mjs';

async function checkMissingAccounts() {
  try {
    const { db, disconnect } = await connect();
    const users = db.collection('users');

    shell.log('🔍 Checking for users without ATProto accounts...\n');

    // Get total user count
    const totalUsers = await users.countDocuments();
    shell.log(`📊 Total users in database: ${totalUsers}`);

    // Get users WITH atproto accounts
    const usersWithAccounts = await users.countDocuments({
      'atproto.did': { $exists: true }
    });
    shell.log(`✅ Users with ATProto accounts: ${usersWithAccounts}`);

    // Get users WITHOUT atproto accounts
    const usersWithoutAccounts = await users.countDocuments({
      'atproto.did': { $exists: false }
    });
    shell.log(`❌ Users WITHOUT ATProto accounts: ${usersWithoutAccounts}`);

    if (usersWithoutAccounts === 0) {
      shell.log('\n🎉 All users have ATProto accounts!');
      await disconnect();
      return;
    }

    // Get the list of users without accounts
    shell.log('\n📋 Users missing ATProto accounts:\n');
    
    const missingUsers = await users.find({
      'atproto.did': { $exists: false }
    }).toArray();

    // Group by tenant
    const aesthetic = missingUsers.filter(u => !u._id.startsWith('sotce-'));
    const sotce = missingUsers.filter(u => u._id.startsWith('sotce-'));

    shell.log(`Aesthetic tenant: ${aesthetic.length} users`);
    aesthetic.slice(0, 10).forEach(u => {
      shell.log(`  - ${u._id} (code: ${u.code})`);
    });
    if (aesthetic.length > 10) {
      shell.log(`  ... and ${aesthetic.length - 10} more`);
    }

    shell.log(`\nSotce tenant: ${sotce.length} users`);
    sotce.slice(0, 10).forEach(u => {
      shell.log(`  - ${u._id} (code: ${u.code})`);
    });
    if (sotce.length > 10) {
      shell.log(`  ... and ${sotce.length - 10} more`);
    }

    shell.log('\n💡 To create accounts for these users, run:');
    shell.log('   ./with-env.sh node create-bulk-accounts.mjs');

    await disconnect();

  } catch (error) {
    shell.error('Error:', error);
    process.exit(1);
  }
}

checkMissingAccounts();
