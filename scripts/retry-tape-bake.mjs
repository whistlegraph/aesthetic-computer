#!/usr/bin/env node
// Retry a failed tape bake
// Usage: node retry-tape-bake.mjs <code>

import { connect } from '../system/backend/database.mjs';
import { S3Client, PutObjectAclCommand, HeadObjectCommand } from '@aws-sdk/client-s3';
import fetch from 'node-fetch';

async function retryTapeBake(code) {
  console.log(`\n🔄 Retrying tape bake for code: ${code}\n`);
  
  const ART_KEY = process.env.ART_KEY || process.env.DO_SPACES_KEY;
  const ART_SECRET = process.env.ART_SECRET || process.env.DO_SPACES_SECRET;
  const OVEN_URL = process.env.OVEN_URL || 'https://oven.aesthetic.computer';
  const OVEN_CALLBACK_SECRET = process.env.OVEN_CALLBACK_SECRET;
  
  const database = await connect();
  const tapes = database.db.collection('tapes');
  
  try {
    // Find the tape
    const tape = await tapes.findOne({ code });
    
    if (!tape) {
      console.error(`❌ Tape not found with code: ${code}`);
      process.exit(1);
    }
    
    console.log(`📼 Found tape: ${tape.slug}`);
    console.log(`   MongoDB ID: ${tape._id}`);
    console.log(`   Bucket: ${tape.bucket}`);
    
    // Get user info
    let user = null;
    if (tape.user) {
      const users = database.db.collection('users');
      user = await users.findOne({ _id: tape.user });
      console.log(`   User: ${user?.email || tape.user}`);
    } else {
      console.log(`   User: anonymous`);
    }
    
    // Construct ZIP key and URL
    // Use user._id as the sub (it's the Auth0 sub)
    const key = user ? `${user._id}/video/${tape.slug}.zip` : `${tape.slug}.zip`;
    const zipUrl = `https://${tape.bucket}.sfo3.digitaloceanspaces.com/${key}`;
    
    console.log(`\n📦 ZIP Info:`);
    console.log(`   Key: ${key}`);
    console.log(`   URL: ${zipUrl}`);
    
    // Step 1: Verify the media URL works
    console.log(`\n� ZIP Info:`);
    console.log(`   Key: ${key}`);
    console.log(`   URL: ${zipUrl}`);
    
    // Step 1: Check if ZIP exists and is accessible
    console.log(`\n🔍 Step 1: Checking ZIP accessibility...`);
    try {
      const headResponse = await fetch(zipUrl, { method: 'HEAD' });
      
      if (headResponse.ok) {
        console.log(`   ✅ ZIP is already publicly accessible`);
      } else {
        console.log(`   ❌ ZIP not accessible (${headResponse.status}), will try to fix ACL`);
        
        // Step 2: Fix the ACL
        console.log(`\n🔧 Step 2: Setting public-read ACL...`);
        
        const s3Client = new S3Client({
          endpoint: `https://sfo3.digitaloceanspaces.com`,
          region: 'us-east-1', // Required for DigitalOcean Spaces
          credentials: {
            accessKeyId: ART_KEY,
            secretAccessKey: ART_SECRET,
          },
        });
        
        const aclCommand = new PutObjectAclCommand({
          Bucket: tape.bucket,
          Key: key,
          ACL: 'public-read',
        });
        
        await s3Client.send(aclCommand);
        console.log(`   ✅ ACL set to public-read`);
        
        // Wait for ACL propagation
        console.log(`   ⏳ Waiting 500ms for ACL propagation...`);
        await new Promise(resolve => setTimeout(resolve, 500));
        
        // Verify it worked
        const verifyResponse = await fetch(zipUrl, { method: 'HEAD' });
        if (verifyResponse.ok) {
          console.log(`   ✅ ZIP is now publicly accessible`);
        } else {
          console.log(`   ⚠️  ZIP still not accessible (${verifyResponse.status}), but continuing...`);
        }
      }
    } catch (error) {
      console.error(`   ❌ Error checking/fixing ZIP: ${error.message}`);
      console.log(`   Continuing anyway...`);
    }
    
    // Step 3: Send to oven
    console.log(`\n🔥 Step 3: Sending to oven for processing...`);
    
    const isDev = process.env.CONTEXT === 'dev' || process.env.NODE_ENV === 'development';
    const baseUrl = isDev ? 'https://localhost:8888' : (process.env.URL || 'https://aesthetic.computer');
    const callbackUrl = `${baseUrl}/api/oven-complete`;
    
    const payload = {
      mongoId: tape._id.toString(),
      slug: tape.slug,
      code: tape.code,
      zipUrl,
      callbackUrl,
      callbackSecret: OVEN_CALLBACK_SECRET,
    };
    
    console.log(`   Oven: ${OVEN_URL}/bake`);
    console.log(`   Callback: ${callbackUrl}`);
    console.log(`   Secret: ${OVEN_CALLBACK_SECRET ? OVEN_CALLBACK_SECRET.substring(0, 10) + '...' : 'MISSING!'}`);
    
    const ovenResponse = await fetch(`${OVEN_URL}/bake`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(payload),
    });
    
    if (!ovenResponse.ok) {
      const errorText = await ovenResponse.text();
      console.error(`   ❌ Oven request failed: ${ovenResponse.status}`);
      console.error(`   Response: ${errorText}`);
      process.exit(1);
    }
    
    const ovenResult = await ovenResponse.json();
    console.log(`   ✅ Oven accepted the bake request`);
    console.log(`   Response:`, ovenResult);
    
    console.log(`\n✨ Success! Tape ${code} has been queued for processing.`);
    console.log(`   Check the oven dashboard at: ${OVEN_URL}`);
    console.log(`   The MP4 will be available at: https://aesthetic.computer/!${code}\n`);
    
  } catch (error) {
    console.error(`\n❌ Fatal error:`, error.message);
    console.error(error);
    process.exit(1);
  } finally {
    await database.disconnect();
  }
}

// Parse command line
const code = process.argv[2];

if (!code) {
  console.error('Usage: node retry-tape-bake.mjs <code>');
  console.error('Example: node retry-tape-bake.mjs 5b9');
  process.exit(1);
}

const ART_KEY = process.env.ART_KEY;
const ART_SECRET = process.env.ART_SECRET;
const OVEN_CALLBACK_SECRET = process.env.OVEN_CALLBACK_SECRET;

if (!ART_KEY || !ART_SECRET) {
  console.error('❌ Missing required environment variables: ART_KEY and ART_SECRET');
  process.exit(1);
}

retryTapeBake(code).catch(err => {
  console.error('💥 Fatal error:', err);
  process.exit(1);
});
