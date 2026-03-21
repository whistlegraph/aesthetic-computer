// Test script for /api/tv hits sorting
// Run with: node system/netlify/functions/test-tv-hits.mjs

import { connect } from "../../backend/database.mjs";

async function testKidlispHitsSorting() {
  console.log("🧪 Testing kidlisp hits sorting...\n");
  
  try {
    const database = await connect();
    const collection = database.db.collection("kidlisp");
    
    // Test 1: Get top 25 by hits with proper sorting
    console.log("Test 1: Top 25 by hits (entire collection)");
    const pipeline = [
      { $match: { nuked: { $ne: true } } },
      { $addFields: { hits: { $ifNull: ["$hits", 0] } } },
      { $sort: { hits: -1, when: -1 } },
      { $limit: 25 },
      {
        $project: {
          code: 1,
          hits: 1,
          when: 1,
        },
      },
    ];
    
    const topHits = await collection.aggregate(pipeline, { allowDiskUse: true }).toArray();
    
    console.log("\nTop 25 kidlisp programs by hits:");
    console.log("─".repeat(60));
    topHits.forEach((record, index) => {
      const date = new Date(record.when).toISOString().split('T')[0];
      console.log(`${(index + 1).toString().padStart(2)}. ${record.code.padEnd(6)} - ${String(record.hits).padStart(4)} hits - ${date}`);
    });
    
    // Test 2: Check date range
    const oldestDate = new Date(Math.min(...topHits.map(r => new Date(r.when))));
    const newestDate = new Date(Math.max(...topHits.map(r => new Date(r.when))));
    
    console.log("\n📅 Date range in results:");
    console.log(`   Oldest: ${oldestDate.toISOString().split('T')[0]}`);
    console.log(`   Newest: ${newestDate.toISOString().split('T')[0]}`);
    
    // Test 3: Verify hits are properly sorted (descending)
    let sorted = true;
    for (let i = 0; i < topHits.length - 1; i++) {
      if (topHits[i].hits < topHits[i + 1].hits) {
        sorted = false;
        console.log(`\n❌ Sort error at index ${i}: ${topHits[i].hits} < ${topHits[i + 1].hits}`);
      }
    }
    
    if (sorted) {
      console.log("\n✅ Results are properly sorted by hits");
    }
    
    // Test 4: Get total count and average hits
    const stats = await collection.aggregate([
      { $match: { nuked: { $ne: true } } },
      {
        $group: {
          _id: null,
          total: { $sum: 1 },
          avgHits: { $avg: { $ifNull: ["$hits", 0] } },
          maxHits: { $max: { $ifNull: ["$hits", 0] } },
        }
      }
    ]).toArray();
    
    console.log("\n📊 Collection stats:");
    console.log(`   Total kidlisp records: ${stats[0].total}`);
    console.log(`   Average hits: ${stats[0].avgHits.toFixed(2)}`);
    console.log(`   Max hits: ${stats[0].maxHits}`);
    
    // Verify we got the max
    if (topHits[0].hits === stats[0].maxHits) {
      console.log(`\n✅ Top result (${topHits[0].code}) has max hits: ${topHits[0].hits}`);
    } else {
      console.log(`\n❌ ERROR: Top result has ${topHits[0].hits} hits, but max is ${stats[0].maxHits}`);
    }
    
    console.log("\n✅ Tests complete");
    process.exit(0);
    
  } catch (error) {
    console.error("❌ Test failed:", error);
    process.exit(1);
  }
}

testKidlispHitsSorting();
