#!/usr/bin/env node
// Quick script to query recent tapes from MongoDB for testing

import 'dotenv/config';
import { MongoClient } from 'mongodb';

const mongoUri = process.env.MONGODB_CONNECTION_STRING;
const dbName = process.env.MONGODB_NAME;

if (!mongoUri || !dbName) {
  console.error('Missing MONGODB_CONNECTION_STRING or MONGODB_NAME in environment');
  process.exit(1);
}

async function queryRecentTapes() {
  const client = await MongoClient.connect(mongoUri);
  const db = client.db(dbName);
  const tapes = db.collection('tapes');

  // Get 5 most recent tapes
  const recent = await tapes
    .find({})
    .sort({ when: -1 })
    .limit(5)
    .toArray();

  console.log(`\n📼 Found ${recent.length} recent tapes:\n`);
  
  for (const tape of recent) {
    const user = tape.user || 'guest';
    const bucket = user === 'guest' ? 'art-aesthetic-computer' : 'user-aesthetic-computer';
    const key = user === 'guest' ? `${tape.slug}.zip` : `${user}/video/${tape.slug}.zip`;
    const zipUrl = `https://${bucket}.sfo3.digitaloceanspaces.com/${key}`;
    
    console.log(`Slug: ${tape.slug}`);
    console.log(`MongoDB ID: ${tape._id}`);
    console.log(`User: ${user}`);
    console.log(`Created: ${tape.when}`);
    console.log(`ZIP URL: ${zipUrl}`);
    console.log(`Has MP4: ${tape.mp4Status || 'unknown'}`);
    console.log(`Code: ${tape.code || 'none'}`);
    console.log('---');
  }

  await client.close();
}

queryRecentTapes().catch(console.error);
