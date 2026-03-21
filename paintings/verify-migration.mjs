// Quick verification script
import { MongoClient } from 'mongodb';
import 'dotenv/config';

const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING);
await client.connect();
const db = client.db(process.env.MONGODB_NAME);
const paintings = db.collection('paintings');

const total = await paintings.countDocuments();
const withCodes = await paintings.countDocuments({ code: { $exists: true } });
const users = await paintings.countDocuments({ user: { $exists: true }, code: { $exists: true } });
const guests = await paintings.countDocuments({ user: { $exists: false }, code: { $exists: true } });

console.log('\n📊 Verification Results:\n');
console.log(`  Total paintings: ${total}`);
console.log(`  With codes: ${withCodes} (${(withCodes/total*100).toFixed(1)}%)`);
console.log(`  User paintings: ${users}`);
console.log(`  Guest paintings: ${guests}`);

console.log('\n📝 Sample paintings:\n');
const samples = await paintings.find({ code: { $exists: true } }).sort({ when: -1 }).limit(10).toArray();

for (const p of samples) {
  const user = p.user ? `user: ${p.user.substring(0,12)}` : 'guest';
  const date = new Date(p.when).toISOString().split('T')[0];
  console.log(`  #${p.code.padEnd(4)} | ${date} | ${user.padEnd(18)} | ${p.slug.substring(0,25)}`);
}

await client.close();
