#!/usr/bin/env node

// Check users without ATProto and their content

import { connect } from './database.mjs';
import { config } from 'dotenv';

config({ path: '../.env' });

const database = await connect();
const users = database.db.collection('users');
const handles = database.db.collection('@handles');
const moods = database.db.collection('moods');
const paintings = database.db.collection('paintings');

console.log('🔍 Users Without ATProto Accounts\n');

// Get all users without ATProto
const usersWithoutAtproto = await users.find({
  $or: [
    { 'atproto.did': { $exists: false } },
    { 'atproto.did': null }
  ]
}).toArray();

console.log(`Found ${usersWithoutAtproto.length} users without ATProto:\n`);

for (const user of usersWithoutAtproto) {
  // Get handle
  const handleDoc = await handles.findOne({ _id: user._id });
  const handle = handleDoc?.handle || 'no-handle';
  
  // Count their content
  const moodCount = await moods.countDocuments({
    user: user._id,
    deleted: { $ne: true }
  });
  
  const paintingCount = await paintings.countDocuments({
    user: user._id
  });
  
  // Get latest activity
  const latestMood = await moods.findOne(
    { user: user._id },
    { sort: { when: -1 } }
  );
  
  const latestPainting = await paintings.findOne(
    { user: user._id },
    { sort: { when: -1 } }
  );
  
  const latestActivity = latestMood?.when || latestPainting?.when || user.created_at;
  
  console.log(`@${handle} (${user._id})`);
  console.log(`   Moods: ${moodCount}, Paintings: ${paintingCount}`);
  console.log(`   Latest activity: ${latestActivity}`);
  console.log(`   Created: ${user.created_at || 'unknown'}`);
  console.log('');
}

await database.disconnect();
