#!/usr/bin/env node
// Delete and recreate tape ATProto record with fixed slug

import { deleteMediaRecord, MediaTypes } from './media-atproto.mjs';
import { connect } from './database.mjs';

async function main() {
  const rkey = process.argv[2];
  const code = process.argv[3];
  
  if (!rkey || !code) {
    console.error('Usage: node delete-and-recreate-tape.mjs <rkey> <code>');
    console.error('Example: node delete-and-recreate-tape.mjs 3m4hn7do6n22e ez2');
    process.exit(1);
  }
  
  const db = await connect();
  
  try {
    // Get the tape to find the user
    const tape = await db.db.collection('tapes').findOne({ code });
    if (!tape) {
      console.error(`❌ Tape not found: ${code}`);
      process.exit(1);
    }
    
    console.log(`📼 Tape: ${code}`);
    console.log(`   Slug: ${tape.slug}`);
    console.log(`   User: ${tape.user}`);
    
    // Get user info
    const users = db.db.collection('users');
    const user = await users.findOne({ _id: tape.user });
    
    if (!user || !user.atproto) {
      console.error(`❌ User not found or no ATProto credentials`);
      process.exit(1);
    }
    
    console.log(`\n🗑️  Deleting old ATProto record: ${rkey}`);
    
    // Delete the old record
    await deleteMediaRecord(
      db,
      MediaTypes.TAPE,
      tape.user,
      rkey
    );
    
    console.log('✅ Old ATProto record deleted');
    
    // Now run sync-atproto to recreate it with the correct slug
    console.log('\n🔄 Now run: node sync-atproto.mjs live --user ' + user._id + ' --tapes-only');
    
  } catch (error) {
    console.error('❌ Error:', error.message);
    throw error;
  } finally {
    await db.disconnect();
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
