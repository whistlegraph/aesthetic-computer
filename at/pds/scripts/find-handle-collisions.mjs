#!/usr/bin/env node

// Find handle collisions (case-insensitive duplicates) in AC handles

import { connect } from '../../../system/backend/database.mjs';
import { shell } from '../../../system/backend/shell.mjs';

async function findHandleCollisions() {
  try {
    const { db, disconnect } = await connect();
    const handles = db.collection('@handles');

    shell.log('🔍 Finding handle collisions (case-insensitive duplicates)...\n');

    // Get all handles
    const allHandles = await handles.find({}).toArray();
    shell.log(`📊 Total handles in system: ${allHandles.length}`);

    // Group handles by lowercase version
    const handleGroups = {};
    
    for (const handleDoc of allHandles) {
      const lowerHandle = handleDoc.handle.toLowerCase();
      
      if (!handleGroups[lowerHandle]) {
        handleGroups[lowerHandle] = [];
      }
      
      handleGroups[lowerHandle].push({
        handle: handleDoc.handle,
        sub: handleDoc.sub,
        when: handleDoc.when
      });
    }

    // Find collisions (groups with more than 1 handle)
    const collisions = Object.entries(handleGroups)
      .filter(([_, group]) => group.length > 1)
      .sort((a, b) => b[1].length - a[1].length); // Sort by most duplicates first

    if (collisions.length === 0) {
      shell.log('\n✅ No handle collisions found!');
      await disconnect();
      return;
    }

    shell.log(`\n⚠️  Found ${collisions.length} handle collisions:\n`);

    for (const [lowerHandle, group] of collisions) {
      shell.log(`\n📌 "${lowerHandle}" (${group.length} variations):`);
      
      // Sort by creation date
      group.sort((a, b) => new Date(a.when) - new Date(b.when));
      
      for (const item of group) {
        const date = item.when ? new Date(item.when).toISOString().split('T')[0] : 'unknown';
        const tenant = item.sub?.startsWith('sotce-') ? 'sotce' : 'aesthetic';
        const subInfo = item.sub || 'no-sub';
        shell.log(`   - @${item.handle} (${tenant}, ${date}, ${subInfo})`);
      }
    }

    shell.log('\n\n📋 SUMMARY:');
    shell.log(`   Total collisions: ${collisions.length}`);
    shell.log(`   Total affected handles: ${collisions.reduce((sum, [_, group]) => sum + group.length, 0)}`);
    
    // Show most problematic cases
    const top10 = collisions.slice(0, 10);
    shell.log(`\n🔝 Top ${Math.min(10, collisions.length)} most duplicated handles:`);
    for (const [lowerHandle, group] of top10) {
      shell.log(`   ${group.length}x - "${lowerHandle}"`);
    }

    shell.log('\n💡 Recommendations:');
    shell.log('   1. Update AC handle validation to be case-insensitive');
    shell.log('   2. Create a migration script to resolve collisions');
    shell.log('   3. Options for resolution:');
    shell.log('      - Keep oldest handle, append numbers to newer ones (jeffrey2, jeffrey3)');
    shell.log('      - Keep oldest handle, ask others to change');
    shell.log('      - Convert all to lowercase');

    await disconnect();

  } catch (error) {
    shell.error('Error:', error);
    process.exit(1);
  }
}

findHandleCollisions();
