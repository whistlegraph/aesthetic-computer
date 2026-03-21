#!/usr/bin/env node
// Check kidlisp records on ATProto PDS vs MongoDB

import { connect } from "../../system/backend/database.mjs";
import { AtpAgent } from "@atproto/api";

const PDS_URL = process.env.PDS_URL || "https://at.aesthetic.computer";

async function checkAccount(handle, label) {
  const database = await connect();
  
  const user = await database.db.collection('users').findOne({'atproto.handle': handle});
  if (!user?.atproto?.did || !user?.atproto?.password) {
    console.log(`\n=== ${label} ===`);
    console.log('No ATProto credentials found');
    await database.disconnect();
    return;
  }
  
  // Get MongoDB count
  const mongoQuery = handle === 'art.at.aesthetic.computer' 
    ? { user: { $exists: false } }
    : { user: user._id };
  const mongoRecords = await database.db.collection('kidlisp').find(mongoQuery).toArray();
  const mongoMissing = mongoRecords.filter(k => !k.atproto || !k.atproto.rkey);
  
  // Get ATProto PDS count
  const agent = new AtpAgent({ service: PDS_URL });
  await agent.login({ identifier: user.atproto.did, password: user.atproto.password });
  
  let atprotoRecords = [];
  let cursor;
  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: user.atproto.did,
      collection: 'computer.aesthetic.kidlisp',
      limit: 100,
      cursor
    });
    atprotoRecords.push(...response.data.records);
    cursor = response.data.cursor;
  } while (cursor);
  
  console.log(`\n=== ${label} (${handle}) ===`);
  console.log(`MongoDB records: ${mongoRecords.length}`);
  console.log(`  - With atproto.rkey: ${mongoRecords.length - mongoMissing.length}`);
  console.log(`  - Missing atproto.rkey: ${mongoMissing.length}`);
  console.log(`ATProto PDS records: ${atprotoRecords.length}`);
  console.log(`Discrepancy: ${atprotoRecords.length - (mongoRecords.length - mongoMissing.length)} records on PDS not tracked in MongoDB`);
  
  await database.disconnect();
}

async function main() {
  await checkAccount('jeffrey.at.aesthetic.computer', '@jeffrey');
  await checkAccount('art.at.aesthetic.computer', 'art-guest');
  process.exit(0);
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
