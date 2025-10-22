#!/usr/bin/env node

// Report users whose paintings still lack ATProto records

import { connect } from '../../../system/backend/database.mjs';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const limit = parseInt(process.argv[2] ?? '10', 10);

const database = await connect();
const paintings = database.db.collection('paintings');
const handles = database.db.collection('@handles');

const unsynced = await paintings.aggregate([
  {
    $match: {
      $or: [
        { atproto: { $exists: false } },
        { 'atproto.rkey': { $exists: false } },
        { 'atproto.rkey': null }
      ],
      nuked: { $ne: true }
    }
  },
  {
    $group: {
      _id: '$user',
      count: { $sum: 1 },
      latest: { $max: '$when' }
    }
  },
  { $sort: { count: -1 } },
  { $limit: limit }
]).toArray();

console.log(`📊 Top ${unsynced.length} users with unsynced paintings`);

for (const entry of unsynced) {
  const handleDoc = entry._id ? await handles.findOne({ _id: entry._id }) : null;
  const label = handleDoc?.handle ? `@${handleDoc.handle}` : '(no handle)';
  console.log(`- ${label}: ${entry.count} unsynced (latest: ${entry.latest?.toISOString() ?? 'N/A'})`);
}

await database.disconnect();
process.exit(0);
