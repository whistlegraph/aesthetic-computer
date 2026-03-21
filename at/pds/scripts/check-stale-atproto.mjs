#!/usr/bin/env node
// check-stale-atproto.mjs - Check for stale ATProto records

import { connect } from '../../../system/backend/database.mjs';

try {
  const database = await connect();
  const users = database.db.collection('users');
  
  console.log('\n🔍 Analyzing ATProto handles in database...\n');
  
  // Count different handle patterns
  const pdsHandles = await users.countDocuments({
    'atproto.handle': { $regex: '\\.pds\\.aesthetic\\.computer$' }
  });
  
  const atHandles = await users.countDocuments({
    'atproto.handle': { $regex: '\\.at\\.aesthetic\\.computer$' }
  });
  
  const totalWithAtproto = await users.countDocuments({
    'atproto': { $exists: true }
  });
  
  console.log('📊 Handle patterns:');
  console.log(`   Total with atproto field: ${totalWithAtproto}`);
  console.log(`   Using .pds.aesthetic.computer: ${pdsHandles}`);
  console.log(`   Using .at.aesthetic.computer: ${atHandles}`);
  console.log(`   Other/missing handles: ${totalWithAtproto - pdsHandles - atHandles}`);
  
  // Sample a few .pds. handles to check if they exist on PDS
  console.log('\n🔬 Sampling .pds. handles...\n');
  
  const pdsSamples = await users.find({
    'atproto.handle': { $regex: '\\.pds\\.aesthetic\\.computer$' }
  }).limit(5).toArray();
  
  for (const user of pdsSamples) {
    const did = user.atproto.did;
    const handle = user.atproto.handle;
    
    try {
      const response = await fetch(
        `https://at.aesthetic.computer/xrpc/com.atproto.repo.describeRepo?repo=${did}`
      );
      
      if (response.ok) {
        const data = await response.json();
        console.log(`✅ ${handle}`);
        console.log(`   DID: ${did}`);
        console.log(`   Actual PDS handle: ${data.handle}`);
        console.log(`   Handle correct: ${data.handleIsCorrect}`);
      } else {
        console.log(`❌ ${handle}`);
        console.log(`   DID: ${did}`);
        console.log(`   Status: Account not found on PDS`);
      }
    } catch (error) {
      console.log(`❌ ${handle} - Error: ${error.message}`);
    }
    console.log();
  }
  
  await database.disconnect();
} catch (error) {
  console.error('❌ Error:', error.message);
  process.exit(1);
}
