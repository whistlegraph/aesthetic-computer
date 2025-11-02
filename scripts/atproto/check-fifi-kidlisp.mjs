#!/usr/bin/env node
// Check @fifi kidlisp on ATProto PDS vs MongoDB

import { connect } from "../../system/backend/database.mjs";
import { AtpAgent } from "@atproto/api";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

async function main() {
  const database = await connect();
  
  // Get @fifi user
  const fifiUser = await database.db.collection('users').findOne({'atproto.handle': 'fifi.at.aesthetic.computer'});
  if (!fifiUser?.atproto?.did || !fifiUser?.atproto?.password) {
    console.log('❌ @fifi has no ATProto credentials');
    await database.disconnect();
    process.exit(1);
  }
  
  console.log('=== @fifi (fifi.at.aesthetic.computer) ===');
  console.log('DID:', fifiUser.atproto.did);
  
  // Get MongoDB count
  const kidlispCollection = database.db.collection('kidlisp');
  const mongoRecords = await kidlispCollection.find({user: fifiUser._id}).toArray();
  const mongoMissing = mongoRecords.filter(k => !k.atproto || !k.atproto.rkey);
  
  console.log('\n📊 MongoDB:');
  console.log('  Total records:', mongoRecords.length);
  console.log('  With atproto.rkey:', mongoRecords.length - mongoMissing.length);
  console.log('  Missing atproto.rkey:', mongoMissing.length);
  
  // Get ATProto PDS count
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ identifier: fifiUser.atproto.did, password: fifiUser.atproto.password });
  
  console.log('\n📥 Fetching ATProto PDS records...');
  let atprotoRecords = [];
  let cursor;
  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: fifiUser.atproto.did,
      collection: 'computer.aesthetic.kidlisp',
      limit: 100,
      cursor
    });
    atprotoRecords.push(...response.data.records);
    cursor = response.data.cursor;
  } while (cursor);
  
  console.log('\n📊 ATProto PDS:');
  console.log('  Total records:', atprotoRecords.length);
  
  console.log('\n📈 Analysis:');
  console.log('  Discrepancy:', atprotoRecords.length - (mongoRecords.length - mongoMissing.length), 'records on PDS not tracked in MongoDB');
  console.log('  To sync:', mongoMissing.length, 'MongoDB records need to be created on ATProto');
  
  await database.disconnect();
  process.exit(0);
}

main().catch(err => {
  console.error('❌ Error:', err);
  process.exit(1);
});
