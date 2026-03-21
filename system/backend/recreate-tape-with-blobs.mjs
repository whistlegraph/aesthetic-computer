#!/usr/bin/env node
// Recreate tape ATProto record with existing MP4/thumbnail blobs

import { AtpAgent } from '@atproto/api';
import { connect } from './database.mjs';
import fetch from 'node-fetch';

async function main() {
  const code = process.argv[2];
  
  if (!code) {
    console.error('Usage: node recreate-tape-with-blobs.mjs <code>');
    console.error('Example: node recreate-tape-with-blobs.mjs ez2');
    process.exit(1);
  }
  
  const db = await connect();
  
  try {
    // Get the tape
    const tape = await db.db.collection('tapes').findOne({ code });
    if (!tape) {
      console.error(`❌ Tape not found: ${code}`);
      process.exit(1);
    }
    
    console.log(`📼 Tape: ${code}`);
    console.log(`   Slug: ${tape.slug}`);
    console.log(`   MP4: ${tape.mp4Url}`);
    console.log(`   Thumbnail: ${tape.thumbnailUrl}`);
    
    // Get user info
    const users = db.db.collection('users');
    const user = await users.findOne({ _id: tape.user });
    
    if (!user || !user.atproto) {
      console.error(`❌ User not found or no ATProto credentials`);
      process.exit(1);
    }
    
    // Login to PDS
    const pdsUrl = `https://${user.atproto.pdsHost || 'jeffrey.at.aesthetic.computer'}`;
    const agent = new AtpAgent({ service: pdsUrl });
    await agent.login({
      identifier: user.atproto.did,
      password: user.atproto.password
    });
    console.log('✅ Logged in to PDS');
    
    // Download MP4
    console.log('📥 Downloading MP4...');
    const mp4Response = await fetch(tape.mp4Url);
    const mp4Buffer = Buffer.from(await mp4Response.arrayBuffer());
    console.log(`✅ MP4 downloaded: ${(mp4Buffer.length / 1024).toFixed(2)} KB`);
    
    // Download thumbnail (optional)
    let thumbnailBuffer = null;
    if (tape.thumbnailUrl) {
      console.log('📥 Downloading thumbnail...');
      const thumbResponse = await fetch(tape.thumbnailUrl);
      if (thumbResponse.ok) {
        thumbnailBuffer = Buffer.from(await thumbResponse.arrayBuffer());
        console.log(`✅ Thumbnail downloaded: ${(thumbnailBuffer.length / 1024).toFixed(2)} KB`);
      }
    }
    
    // Upload video blob
    console.log('📤 Uploading video blob...');
    const videoBlob = await agent.uploadBlob(mp4Buffer, {
      encoding: 'video/mp4'
    });
    console.log(`✅ Video blob uploaded: ${videoBlob.data.blob.ref.$link}`);
    
    // Upload thumbnail blob (optional)
    let thumbnailBlob = null;
    if (thumbnailBuffer) {
      console.log('📤 Uploading thumbnail blob...');
      thumbnailBlob = await agent.uploadBlob(thumbnailBuffer, {
        encoding: 'image/jpeg'
      });
      console.log(`✅ Thumbnail blob uploaded: ${thumbnailBlob.data.blob.ref.$link}`);
    }
    
    // Delete old record if exists
    if (tape.at && tape.at.rkey) {
      console.log(`🗑️  Deleting old record: ${tape.at.rkey}`);
      try {
        await agent.com.atproto.repo.deleteRecord({
          repo: agent.session.did,
          collection: 'computer.aesthetic.tape',
          rkey: tape.at.rkey
        });
        console.log('✅ Old record deleted');
      } catch (err) {
        console.log('⚠️  Old record not found or already deleted');
      }
    }
    
    // Create new record with blobs
    console.log('📝 Creating ATProto record with blobs...');
    const record = {
      $type: 'computer.aesthetic.tape',
      slug: tape.slug,
      code: tape.code,
      acUrl: `https://aesthetic.computer/!${code}`,
      when: tape.when.toISOString(),
      ref: tape._id.toString(),
      video: videoBlob.data.blob
    };
    
    if (thumbnailBlob) {
      record.thumbnail = thumbnailBlob.data.blob;
    }
    
    const result = await agent.com.atproto.repo.createRecord({
      repo: agent.session.did,
      collection: 'computer.aesthetic.tape',
      record
    });
    
    console.log('✅ ATProto record created');
    console.log('   Full result:', JSON.stringify(result, null, 2));
    
    // Update MongoDB with new rkey
    const newRkey = result.data?.uri ? result.data.uri.split('/').pop() : result.uri.split('/').pop();
    const newUri = result.data?.uri || result.uri;
    const newCid = result.data?.cid || result.cid;
    
    console.log('   Rkey:', newRkey);
    
    await db.db.collection('tapes').updateOne(
      { code },
      { $set: { at: { rkey: newRkey, uri: newUri, cid: newCid } } }
    );
    await db.db.collection('oven-bakes').updateMany(
      { code },
      { $set: { atprotoRkey: newRkey } }
    );
    console.log('✅ MongoDB updated with new rkey');
    
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
