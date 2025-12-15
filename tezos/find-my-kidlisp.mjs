#!/usr/bin/env node
// Find KidLisp pieces by @jeffrey

import { connect } from '../system/backend/database.mjs';

const { db } = await connect();

// Find user by handle
const user = await db.collection('users').findOne({ 
  'atproto.handle': 'jeffrey.at.aesthetic.computer' 
});

if (!user) {
  console.log('❌ User @jeffrey not found');
  process.exit(1);
}

console.log(`✅ Found user: ${user._id} (@jeffrey)`);

// Find their KidLisp pieces
const pieces = await db.collection('kidlisp')
  .find({ user: user._id })
  .sort({ when: -1 })
  .limit(20)
  .toArray();

console.log(`\n🎨 Your KidLisp pieces (${pieces.length}):\n`);

pieces.forEach((p, i) => {
  const preview = p.source.substring(0, 60).replace(/\n/g, ' ');
  console.log(`${(i+1).toString().padStart(2)}. $${p.code}`);
  console.log(`    ${preview}...`);
  console.log('');
});

process.exit(0);
