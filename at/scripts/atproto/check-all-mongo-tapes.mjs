#!/usr/bin/env node

/**
 * Check all tapes in MongoDB for a user
 */

import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const handleArg = process.argv[2];
if (!handleArg || !handleArg.startsWith('@')) {
  console.error('Usage: node check-all-mongo-tapes.mjs @handle');
  process.exit(1);
}

const handle = handleArg.replace('@', '');

async function main() {
  const database = await connect();

  try {
    const userSub = await userIDFromHandle(handle, database);
    if (!userSub) {
      console.error(`❌ Could not find user for handle @${handle}`);
      process.exit(1);
    }

    const tapes = database.db.collection('tapes');
    
    console.log(`\n${'═'.repeat(80)}`);
    console.log(`📼 ALL TAPES IN MONGODB FOR @${handle}`);
    console.log(`${'═'.repeat(80)}\n`);

    const allTapes = await tapes.find({ user: userSub }).sort({ when: -1 }).toArray();
    
    console.log(`Found ${allTapes.length} total tapes\n`);

    for (const tape of allTapes) {
      console.log(`${'─'.repeat(80)}`);
      console.log(`Code: ${tape.code}`);
      console.log(`Slug: ${tape.slug}`);
      console.log(`When: ${tape.when}`);
      console.log(`MongoDB _id: ${tape._id}`);
      console.log(`ZIP URL: ${tape.zipUrl || 'NONE'}`);
      console.log(`MP4 URL: ${tape.mp4Url || 'NONE'}`);
      console.log(`Thumbnail: ${tape.thumbnailUrl || 'NONE'}`);
      console.log(`ATProto rkey: ${tape.at?.rkey || 'NONE'}`);
      console.log(`ATProto URI: ${tape.at?.uri || 'NONE'}`);
    }

    console.log(`\n${'═'.repeat(80)}`);
    console.log(`Total: ${allTapes.length} tapes`);
    console.log(`${'═'.repeat(80)}\n`);

    await database.disconnect();
    process.exit(0);

  } catch (error) {
    console.error('❌ Fatal error:', error);
    await database.disconnect();
    process.exit(1);
  }
}

main().catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
