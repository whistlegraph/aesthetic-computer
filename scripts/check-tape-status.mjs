#!/usr/bin/env node
// Check tape status and URLs
// Usage: node check-tape-status.mjs <code>

import { connect } from '../system/backend/database.mjs';

async function checkTapeStatus(code) {
  console.log(`\n📼 Checking tape status for code: ${code}\n`);
  
  const database = await connect();
  const tapes = database.db.collection('tapes');
  
  try {
    const tape = await tapes.findOne({ code });
    
    if (!tape) {
      console.error(`❌ Tape not found with code: ${code}`);
      process.exit(1);
    }
    
    console.log(`✅ Found tape: ${tape.slug}`);
    console.log(`   MongoDB ID: ${tape._id}`);
    console.log(`   Bucket: ${tape.bucket}`);
    console.log(`   Created: ${tape.created}`);
    
    // Check for MP4
    if (tape.mp4) {
      console.log(`\n🎬 MP4 Status:`);
      console.log(`   ✅ MP4 exists: ${tape.mp4}`);
    } else {
      console.log(`\n🎬 MP4 Status:`);
      console.log(`   ❌ No MP4 found - tape may need baking`);
    }
    
    // Get user info
    let user = null;
    if (tape.user) {
      const users = database.db.collection('users');
      user = await users.findOne({ _id: tape.user });
      console.log(`\n👤 User: ${user?.email || tape.user}`);
    } else {
      console.log(`\n👤 User: anonymous`);
    }
    
    // Construct URLs
    const key = user ? `${user._id}/video/${tape.slug}.zip` : `${tape.slug}.zip`;
    const zipUrl = `https://${tape.bucket}.sfo3.digitaloceanspaces.com/${key}`;
    const mediaZipUrl = `https://aesthetic.computer/media/tapes/${code}.zip`;
    
    console.log(`\n📦 ZIP URLs:`);
    console.log(`   Direct: ${zipUrl}`);