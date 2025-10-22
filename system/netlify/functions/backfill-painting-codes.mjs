#!/usr/bin/env node

// Backfill Painting Codes Migration Script
// Generates and assigns short codes to paintings that don't have them

import { MongoClient } from "mongodb";
import { customAlphabet } from 'nanoid';

const MONGODB_CONNECTION_STRING = process.env.MONGODB_CONNECTION_STRING;
const MONGODB_NAME = process.env.MONGODB_NAME || "aesthetic";

if (!MONGODB_CONNECTION_STRING) {
  console.error("❌ MONGODB_CONNECTION_STRING environment variable is required");
  process.exit(1);
}

// Code generator - same as track-media.mjs
const consonants = 'bcdfghjklmnpqrstvwxyz' + 'bcdfghjklmnpqrstvwxyz' + 'BCDFGHJKLMNPQRSTVWXYZ';
const vowels = 'aeiou' + 'aeiou' + 'AEIOU';
const numbers = '23456789';
const alphabet = consonants + vowels + numbers;
const CODE_LENGTH = 3;
const nanoid = customAlphabet(alphabet, CODE_LENGTH);
const MAX_COLLISION_ATTEMPTS = 100;

async function generateUniqueCode(collection, existingCodes) {
  for (let attempt = 0; attempt < MAX_COLLISION_ATTEMPTS; attempt++) {
    const code = nanoid();
    
    // Check against both database and our in-memory set
    if (!existingCodes.has(code)) {
      const existing = await collection.findOne({ code });
      if (!existing) {
        existingCodes.add(code); // Add to in-memory set
        return code;
      }
    }
    
    console.log(`⚠️  Code collision detected: ${code}, retrying...`);
  }
  
  // If we hit max attempts, use a longer code
  const longerNanoid = customAlphabet(alphabet, CODE_LENGTH + 1);
  const longerCode = longerNanoid();
  console.log(`⚠️  Max collisions reached, using longer code: ${longerCode}`);
  existingCodes.add(longerCode);
  return longerCode;
}

async function backfillPaintingCodes() {
  const client = new MongoClient(MONGODB_CONNECTION_STRING);
  
  try {
    await client.connect();
    console.log("✅ Connected to MongoDB");
    
    const db = client.db(MONGODB_NAME);
    const paintings = db.collection("paintings");
    
    // Create index if it doesn't exist
    await paintings.createIndex({ code: 1 }, { unique: true, sparse: true });
    console.log("✅ Ensured code index exists");
    
    // Find all paintings without codes
    const paintingsWithoutCodes = await paintings.find({ 
      code: { $exists: false } 
    }).toArray();
    
    console.log(`\n📊 Found ${paintingsWithoutCodes.length} paintings without codes`);
    
    if (paintingsWithoutCodes.length === 0) {
      console.log("✅ All paintings already have codes!");
      return;
    }
    
    // Get all existing codes to avoid collisions
    const existingCodesArray = await paintings.distinct("code");
    const existingCodes = new Set(existingCodesArray.filter(c => c)); // Filter out null/undefined
    console.log(`📦 Loaded ${existingCodes.size} existing codes into memory`);
    
    let updated = 0;
    let failed = 0;
    
    for (const painting of paintingsWithoutCodes) {
      try {
        const code = await generateUniqueCode(paintings, existingCodes);
        
        await paintings.updateOne(
          { _id: painting._id },
          { $set: { code } }
        );
        
        updated++;
        const userInfo = painting.user ? ` (user: ${painting.user})` : " (guest)";
        console.log(`✅ ${updated}/${paintingsWithoutCodes.length}: ${painting.slug} → #${code}${userInfo}`);
      } catch (err) {
        failed++;
        console.error(`❌ Failed to update ${painting.slug}:`, err.message);
      }
    }
    
    console.log(`\n📊 Migration complete:`);
    console.log(`   ✅ Updated: ${updated}`);
    console.log(`   ❌ Failed: ${failed}`);
    console.log(`   📦 Total codes in database: ${existingCodes.size + updated}`);
    
  } catch (error) {
    console.error("❌ Migration failed:", error);
    process.exit(1);
  } finally {
    await client.close();
    console.log("\n✅ Disconnected from MongoDB");
  }
}

// Run the migration
console.log("🚀 Starting painting codes backfill migration...\n");
backfillPaintingCodes()
  .then(() => {
    console.log("\n🎉 Migration completed successfully!");
    process.exit(0);
  })
  .catch((err) => {
    console.error("\n💥 Migration failed:", err);
    process.exit(1);
  });
