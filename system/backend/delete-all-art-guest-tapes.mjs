#!/usr/bin/env node

/**
 * Delete all ATProto tape records from art-guest account (art.at.aesthetic.computer)
 * This clears duplicates so they can be properly backfilled later
 */

import { AtpAgent } from '@atproto/api';
import { connect } from './database.mjs';
import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
config({ path: resolve(__dirname, '../.env') });

const dryRun = process.argv.includes('--dry-run');
const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';

async function main() {
  const db = await connect();
  
  try {
    // Get art-guest user
    const users = db.db.collection('users');
    const artGuest = await users.findOne({ _id: 'art-guest' });
    
    if (!artGuest || !artGuest.atproto) {
      console.error('❌ art-guest user not found or missing ATProto credentials');
      process.exit(1);
    }
    
    console.log('🎨 art-guest account');
    console.log('   DID:', artGuest.atproto.did);
    console.log('   PDS:', PDS_URL);
    
    // Login to PDS
    const agent = new AtpAgent({ service: PDS_URL });
    await agent.login({
      identifier: artGuest.atproto.did,
      password: artGuest.atproto.password
    });
    console.log('✅ Logged in to PDS\n');
    
    // List all tape records (handle pagination)
    console.log('🔍 Fetching all tape records...');
    let allRecords = [];
    let cursor = undefined;
    
    do {
      const response = await agent.com.atproto.repo.listRecords({
        repo: artGuest.atproto.did,
        collection: 'computer.aesthetic.tape',
        limit: 100,
        cursor: cursor
      });
      
      allRecords.push(...response.data.records);
      cursor = response.data.cursor;
    } while (cursor);
    
    console.log(`📼 Found ${allRecords.length} tape records for art-guest\n`);
    const records = { data: { records: allRecords } };
    
    if (records.data.records.length === 0) {
      console.log('✅ No tape records to delete');
      await db.disconnect();
      process.exit(0);
    }
    
    // List all records
    console.log('Records to delete:');
    records.data.records.forEach(record => {
      const value = record.value;
      console.log(`   - ${value.code} (${value.slug}) - rkey: ${record.uri.split('/').pop()}`);
    });
    
    if (dryRun) {
      console.log('\n🔍 DRY RUN - Would delete', records.data.records.length, 'records');
      await db.disconnect();
      process.exit(0);
    }
    
    console.log('\n🗑️  Deleting records...');
    let deleted = 0;
    let failed = 0;
    
    for (const record of records.data.records) {
      const rkey = record.uri.split('/').pop();
      try {
        await agent.com.atproto.repo.deleteRecord({
          repo: artGuest.atproto.did,
          collection: 'computer.aesthetic.tape',
          rkey: rkey
        });
        console.log(`   ✅ Deleted ${record.value.code} (${rkey})`);
        deleted++;
      } catch (err) {
        console.error(`   ❌ Failed to delete ${record.value.code} (${rkey}):`, err.message);
        failed++;
      }
    }
    
    console.log('\n════════════════════════════════════════════════════════════════');
    console.log('📊 DELETION SUMMARY');
    console.log('════════════════════════════════════════════════════════════════');
    console.log(`ATProto records deleted: ${deleted}`);
    console.log(`Failed deletions: ${failed}`);
    console.log('════════════════════════════════════════════════════════════════\n');
    
    await db.disconnect();
    process.exit(0);
    
  } catch (err) {
    console.error('❌ Fatal error:', err);
    await db.disconnect();
    process.exit(1);
  }
}

main();
