#!/usr/bin/env node
// Check ATProto records for a user

import { connect } from '../../system/backend/database.mjs';
import { userIDFromHandle } from '../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config();

const PDS_URL = process.env.PDS_URL || 'https://at.aesthetic.computer';
const handle = process.argv[2] || 'jeffrey';

const database = await connect();
const sub = await userIDFromHandle(handle, database);

// Get user's ATProto credentials
const users = database.db.collection('users');
const user = await users.findOne({ _id: sub });

if (!user?.atproto?.did) {
  console.log(`❌ User @${handle} has no ATProto account`);
  process.exit(1);
}

const { did, password } = user.atproto;

// Login to ATProto
const agent = new AtpAgent({ service: PDS_URL });
await agent.login({ identifier: did, password });

// List mood records
const result = await agent.com.atproto.repo.listRecords({
  repo: did,
  collection: 'computer.aesthetic.mood',
  limit: 100
});

console.log(`\n🦋 ATProto moods for @${handle}:\n`);
console.log(`   DID: ${did}`);

const moods = result.data || result;
const records = moods.records || [];

console.log(`   Total moods on PDS: ${records.length}\n`);

if (records.length > 0) {
  console.log(`Recent moods (last 5):\n`);
  records.slice(0, 5).forEach((record, i) => {
    const rkey = record.uri.split('/').pop();
    console.log(`[${i+1}] rkey: ${rkey}`);
    console.log(`    Mood: ${record.value.mood.substring(0, 50)}`);
    console.log(`    When: ${record.value.when}`);
    console.log(`    Database Ref: ${record.value.ref || record.value.mongoId || 'N/A'}`);
    console.log('');
  });
} else {
  console.log(`No moods found on PDS yet.`);
}

await database.disconnect();
process.exit(0);
