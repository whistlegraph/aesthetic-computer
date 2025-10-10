#!/usr/bin/env node

/**
 * Inspect MongoDB Paintings Collection
 * 
 * Query paintings, check for codes, get statistics.
 * 
 * Usage:
 *   node inspect-mongodb.mjs --count
 *   node inspect-mongodb.mjs --stats
 *   node inspect-mongodb.mjs --recent 10
 *   node inspect-mongodb.mjs --no-codes
 *   node inspect-mongodb.mjs --user auth0|123
 */

import { MongoClient } from 'mongodb';
import { config } from 'dotenv';

config();

const MONGODB_URI = process.env.MONGODB_URI || 'mongodb://localhost:27017/aesthetic';
const args = process.argv.slice(2);

let client;
let db;

async function connect() {
  console.log(`\n🔌 Connecting to MongoDB...`);
  console.log(`   URI: ${MONGODB_URI.replace(/\/\/.*@/, '//***@')}\n`);
  
  client = new MongoClient(MONGODB_URI);
  await client.connect();
  db = client.db();
  
  console.log('✅ Connected successfully\n');
  return db;
}

async function disconnect() {
  if (client) {
    await client.close();
    console.log('\n👋 Disconnected from MongoDB\n');
  }
}

async function getCount() {
  const paintings = db.collection('paintings');
  const total = await paintings.countDocuments({ nuked: { $ne: true } });
  const withCodes = await paintings.countDocuments({ 
    nuked: { $ne: true }, 
    code: { $exists: true } 
  });
  const noCodes = total - withCodes;
  
  console.log('📊 Painting Counts:\n');
  console.log(`   Total paintings: ${total}`);
  console.log(`   With codes: ${withCodes} (${((withCodes / total) * 100).toFixed(1)}%)`);
  console.log(`   Without codes: ${noCodes} (${((noCodes / total) * 100).toFixed(1)}%)`);
  console.log();
}

async function getStats() {
  const paintings = db.collection('paintings');
  
  console.log('📈 Database Statistics:\n');
  
  // Basic counts
  const total = await paintings.countDocuments({ nuked: { $ne: true } });
  const withCodes = await paintings.countDocuments({ 
    nuked: { $ne: true }, 
    code: { $exists: true } 
  });
  const withHashes = await paintings.countDocuments({ 
    nuked: { $ne: true }, 
    hash: { $exists: true } 
  });
  
  console.log('📝 Counts:');
  console.log(`   Total paintings: ${total}`);
  console.log(`   With codes: ${withCodes}`);
  console.log(`   With hashes: ${withHashes}`);
  console.log();
  
  // User stats
  const userCount = await paintings.distinct('user', { nuked: { $ne: true } });
  console.log('👥 Users:');
  console.log(`   Unique users: ${userCount.length}`);
  console.log();
  
  // Top users
  const topUsers = await paintings.aggregate([
    { $match: { nuked: { $ne: true } } },
    { $group: { _id: '$user', count: { $sum: 1 } } },
    { $sort: { count: -1 } },
    { $limit: 10 }
  ]).toArray();
  
  console.log('🏆 Top 10 Users by Painting Count:');
  topUsers.forEach((user, i) => {
    console.log(`   ${(i + 1).toString().padStart(2)}. ${user._id}: ${user.count} paintings`);
  });
  console.log();
  
  // Recent activity
  const oldest = await paintings.findOne(
    { nuked: { $ne: true } },
    { sort: { when: 1 } }
  );
  const newest = await paintings.findOne(
    { nuked: { $ne: true } },
    { sort: { when: -1 } }
  );
  
  console.log('📅 Date Range:');
  console.log(`   Oldest: ${oldest?.when?.toISOString() || 'N/A'}`);
  console.log(`   Newest: ${newest?.when?.toISOString() || 'N/A'}`);
  console.log();
  
  // Index info
  const indexes = await paintings.indexes();
  console.log('🔍 Indexes:');
  indexes.forEach(index => {
    const keys = Object.keys(index.key).join(', ');
    const unique = index.unique ? ' [UNIQUE]' : '';
    console.log(`   - ${keys}${unique}`);
  });
  console.log();
}

async function getRecent(limit = 10) {
  const paintings = db.collection('paintings');
  
  const recent = await paintings.find(
    { nuked: { $ne: true } },
    { sort: { when: -1 }, limit }
  ).toArray();
  
  console.log(`🕒 ${limit} Most Recent Paintings:\n`);
  
  recent.forEach((painting, i) => {
    console.log(`   ${i + 1}. ${painting.slug}`);
    console.log(`      User: ${painting.user}`);
    console.log(`      Code: ${painting.code || '(none)'}`);
    console.log(`      Hash: ${painting.hash ? painting.hash.substring(0, 12) + '...' : '(none)'}`);
    console.log(`      Date: ${painting.when?.toISOString() || 'N/A'}`);
    console.log();
  });
}

async function getNoCodes() {
  const paintings = db.collection('paintings');
  
  const noCodes = await paintings.find(
    { 
      nuked: { $ne: true },
      code: { $exists: false }
    },
    { limit: 20, sort: { when: -1 } }
  ).toArray();
  
  console.log(`🔍 Paintings Without Codes (showing first 20):\n`);
  
  if (noCodes.length === 0) {
    console.log('   ✨ All paintings have codes!\n');
    return;
  }
  
  noCodes.forEach((painting, i) => {
    console.log(`   ${(i + 1).toString().padStart(2)}. ${painting.slug}`);
    console.log(`      User: ${painting.user}`);
    console.log(`      Date: ${painting.when?.toISOString() || 'N/A'}`);
  });
  console.log();
  
  const totalNoCodes = await paintings.countDocuments({
    nuked: { $ne: true },
    code: { $exists: false }
  });
  
  console.log(`📊 Total paintings without codes: ${totalNoCodes}\n`);
}

async function inspectUser(userId) {
  const paintings = db.collection('paintings');
  
  const userPaintings = await paintings.find(
    { 
      user: userId,
      nuked: { $ne: true }
    },
    { sort: { when: -1 } }
  ).toArray();
  
  console.log(`🔍 Paintings for user: ${userId}\n`);
  
  if (userPaintings.length === 0) {
    console.log('   ⚠️  No paintings found for this user\n');
    return;
  }
  
  console.log(`📊 Found ${userPaintings.length} paintings\n`);
  
  const withCodes = userPaintings.filter(p => p.code).length;
  console.log(`   With codes: ${withCodes} (${((withCodes / userPaintings.length) * 100).toFixed(1)}%)\n`);
  
  console.log('📸 Recent paintings:\n');
  
  userPaintings.slice(0, 10).forEach((painting, i) => {
    console.log(`   ${i + 1}. ${painting.slug}`);
    console.log(`      Code: ${painting.code || '(none)'}`);
    console.log(`      Date: ${painting.when?.toISOString() || 'N/A'}`);
    console.log();
  });
  
  if (userPaintings.length > 10) {
    console.log(`   ... and ${userPaintings.length - 10} more\n`);
  }
}

async function checkDuplicates() {
  const paintings = db.collection('paintings');
  
  console.log('🔍 Checking for duplicate hashes...\n');
  
  const duplicates = await paintings.aggregate([
    { $match: { nuked: { $ne: true }, hash: { $exists: true } } },
    { $group: { _id: '$hash', count: { $sum: 1 }, paintings: { $push: '$slug' } } },
    { $match: { count: { $gt: 1 } } },
    { $sort: { count: -1 } }
  ]).toArray();
  
  if (duplicates.length === 0) {
    console.log('   ✅ No duplicate hashes found\n');
    return;
  }
  
  console.log(`⚠️  Found ${duplicates.length} duplicate hashes:\n`);
  
  duplicates.slice(0, 10).forEach((dup, i) => {
    console.log(`   ${i + 1}. Hash: ${dup._id.substring(0, 12)}... (${dup.count} paintings)`);
    dup.paintings.forEach(slug => {
      console.log(`      - ${slug}`);
    });
    console.log();
  });
}

async function main() {
  try {
    await connect();
    
    if (args.includes('--count')) {
      await getCount();
    } else if (args.includes('--stats')) {
      await getStats();
    } else if (args.includes('--recent')) {
      const recentIndex = args.indexOf('--recent');
      const limit = parseInt(args[recentIndex + 1]) || 10;
      await getRecent(limit);
    } else if (args.includes('--no-codes')) {
      await getNoCodes();
    } else if (args.includes('--user')) {
      const userIndex = args.indexOf('--user');
      const userId = args[userIndex + 1];
      if (!userId) {
        console.error('❌ Error: --user requires a user ID');
        process.exit(1);
      }
      await inspectUser(userId);
    } else if (args.includes('--duplicates')) {
      await checkDuplicates();
    } else {
      console.log('\n📊 MongoDB Paintings Inspector\n');
      console.log('Usage:');
      console.log('  node inspect-mongodb.mjs --count         # Basic counts');
      console.log('  node inspect-mongodb.mjs --stats         # Full statistics');
      console.log('  node inspect-mongodb.mjs --recent 10     # Recent paintings');
      console.log('  node inspect-mongodb.mjs --no-codes      # Paintings without codes');
      console.log('  node inspect-mongodb.mjs --user <id>     # User paintings');
      console.log('  node inspect-mongodb.mjs --duplicates    # Check for duplicates');
      console.log();
    }
    
    await disconnect();
  } catch (error) {
    console.error('\n💥 Error:', error.message);
    await disconnect();
    process.exit(1);
  }
}

main();
