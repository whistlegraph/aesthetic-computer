#!/usr/bin/env node

import { AtpAgent } from '@atproto/api';
import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const database = await connect();

try {
  const userSub = await userIDFromHandle('jeffrey', database);
  const users = database.db.collection('users');
  const userDoc = await users.findOne({ _id: userSub });

  const agent = new AtpAgent({ service: 'https://at.aesthetic.computer' });
  await agent.login({ 
    identifier: userDoc.atproto.did, 
    password: userDoc.atproto.password 
  });

  const response = await agent.com.atproto.repo.listRecords({
    repo: userDoc.atproto.did,
    collection: 'computer.aesthetic.tape',
    limit: 2
  });

  const records = response.data.records;
  for (const record of records) {
    console.log('\n─'.repeat(40));
    console.log('Code:', record.value.code);
    console.log('Full record:', JSON.stringify(record, null, 2));
    console.log('\nVideo field type:', typeof record.value.video);
    console.log('Video keys:', Object.keys(record.value.video || {}));
    console.log('Has $type?', record.value.video?.$type);
    console.log('Has ref?', !!record.value.video?.ref);
    console.log('Ref type:', typeof record.value.video?.ref);
    console.log('Ref keys:', Object.keys(record.value.video?.ref || {}));
    console.log('Has ref.$link?', record.value.video?.ref?.$link);
  }
} finally {
  await database.disconnect();
}
