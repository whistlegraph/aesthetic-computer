#!/usr/bin/env node
// regenerate-codes.mjs
// Regenerate all painting codes with new algorithm

import 'dotenv/config';
import { generateCode } from './generate-code.mjs';
import { MongoClient } from 'mongodb';

const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING);
await client.connect();
const db = client.db(process.env.MONGODB_NAME);
const paintings = db.collection('paintings');

console.log('🔄 Regenerating all painting codes...\n');

const total = await paintings.countDocuments();
console.log(`📊 Found ${total} paintings to regenerate\n`);

const existingCodes = new Set();
let updated = 0;

const allPaintings = await paintings.find({}).toArray();

for (const painting of allPaintings) {
  const newCode = generateCode(existingCodes);
  
  await paintings.updateOne(
    { _id: painting._id },
    { $set: { code: newCode } }
  );
  
  updated++;
  if (updated % 500 === 0) {
    console.log(`   ... ${updated}/${total} codes regenerated`);
  }
}

console.log(`\n✅ Regenerated ${updated} codes\n`);

// Show samples
console.log('📝 Sample new codes:\n');
const samples = await paintings.find({}).limit(20).toArray();
samples.forEach(p => {
  const user = p.user ? 'user' : 'guest';
  console.log(`  #${p.code.padEnd(4)} | ${user}`);
});

await client.close();
