#!/usr/bin/env node
// MongoDB Performance Benchmark
// Compares write/read performance between Atlas and silo.aesthetic.computer

import { MongoClient } from "mongodb";
import * as readline from "readline/promises";

// ANSI colors for output
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[36m',
  red: '\x1b[31m',
};

// Database configurations
const configs = {
  atlas: {
    name: "MongoDB Atlas (Production)",
    connectionString: process.env.MONGODB_CONNECTION_STRING,
    dbName: process.env.MONGODB_NAME || "aesthetic",
  },
  silo: {
    name: "silo.aesthetic.computer",
    connectionString: process.env.SILO_MONGODB_CONNECTION_STRING || "mongodb://silo.aesthetic.computer:27017/aesthetic",
    dbName: process.env.SILO_MONGODB_NAME || "aesthetic",
  },
};

// Test data generators
function generateTestDocument(id) {
  return {
    _id: `benchmark_${id}_${Date.now()}`,
    user: `auth0|benchmark_${Math.random().toString(36).substr(2, 9)}`,
    content: `This is test content ${id} with some random data: ${Math.random()}`,
    metadata: {
      created: new Date(),
      tags: ["test", "benchmark", `tag_${id % 10}`],
      counter: id,
    },
    nested: {
      level1: {
        level2: {
          value: `Deep value ${id}`,
        },
      },
    },
  };
}

// Benchmark functions
async function benchmarkConnection(config) {
  const start = performance.now();
  const client = new MongoClient(config.connectionString, {
    serverSelectionTimeoutMS: 10000,
    connectTimeoutMS: 10000,
  });

  try {
    await client.connect();
    await client.db(config.dbName).admin().ping();
    const duration = performance.now() - start;
    await client.close();
    return { success: true, duration };
  } catch (error) {
    return { success: false, error: error.message, duration: performance.now() - start };
  }
}

async function benchmarkWrites(db, count = 100) {
  const collection = db.collection("benchmark_test");
  const results = {};

  // Single insertOne operations
  const singleStart = performance.now();
  for (let i = 0; i < count; i++) {
    await collection.insertOne(generateTestDocument(i));
  }
  results.singleInserts = {
    count,
    duration: performance.now() - singleStart,
    perOp: (performance.now() - singleStart) / count,
  };

  // Bulk insertMany operation
  const bulkDocs = Array.from({ length: count }, (_, i) =>
    generateTestDocument(count + i)
  );
  const bulkStart = performance.now();
  await collection.insertMany(bulkDocs);
  results.bulkInsert = {
    count,
    duration: performance.now() - bulkStart,
    perOp: (performance.now() - bulkStart) / count,
  };

  return results;
}

async function benchmarkReads(db, count = 100) {
  const collection = db.collection("benchmark_test");
  const results = {};

  // Get some IDs to query
  const sampleDocs = await collection.find().limit(count).toArray();
  const ids = sampleDocs.map(doc => doc._id);

  // findOne by _id
  const findOneStart = performance.now();
  for (const id of ids) {
    await collection.findOne({ _id: id });
  }
  results.findById = {
    count: ids.length,
    duration: performance.now() - findOneStart,
    perOp: (performance.now() - findOneStart) / ids.length,
  };

  // find with query and sort
  const findStart = performance.now();
  for (let i = 0; i < 50; i++) {
    await collection.find({ "metadata.counter": { $gte: 0 } })
      .sort({ "metadata.created": -1 })
      .limit(20)
      .toArray();
  }
  results.findWithSort = {
    count: 50,
    duration: performance.now() - findStart,
    perOp: (performance.now() - findStart) / 50,
  };

  // Aggregation pipeline (simulating moods + handles lookup)
  const aggStart = performance.now();
  for (let i = 0; i < 20; i++) {
    await collection.aggregate([
      { $match: { "metadata.counter": { $gte: 0 } } },
      { $sort: { "metadata.created": -1 } },
      { $limit: 10 },
      { $project: { _id: 1, user: 1, content: 1, "metadata.created": 1 } },
    ]).toArray();
  }
  results.aggregation = {
    count: 20,
    duration: performance.now() - aggStart,
    perOp: (performance.now() - aggStart) / 20,
  };

  return results;
}

async function benchmarkUpdates(db, count = 100) {
  const collection = db.collection("benchmark_test");
  const sampleDocs = await collection.find().limit(count).toArray();
  const ids = sampleDocs.map(doc => doc._id);

  const start = performance.now();
  for (const id of ids) {
    await collection.updateOne(
      { _id: id },
      { $set: { updated: new Date() }, $inc: { "metadata.counter": 1 } }
    );
  }

  return {
    count: ids.length,
    duration: performance.now() - start,
    perOp: (performance.now() - start) / ids.length,
  };
}

async function cleanup(db) {
  const collection = db.collection("benchmark_test");
  const result = await collection.deleteMany({
    _id: { $regex: /^benchmark_/ }
  });
  return result.deletedCount;
}

// Display functions
function printHeader(text) {
  console.log(`\n${colors.bright}${colors.blue}${'='.repeat(60)}${colors.reset}`);
  console.log(`${colors.bright}${colors.blue}  ${text}${colors.reset}`);
  console.log(`${colors.bright}${colors.blue}${'='.repeat(60)}${colors.reset}\n`);
}

function printResults(label, results) {
  console.log(`${colors.bright}${label}:${colors.reset}`);
  console.log(`  Operations: ${results.count}`);
  console.log(`  Total time: ${colors.green}${results.duration.toFixed(2)}ms${colors.reset}`);
  console.log(`  Per operation: ${colors.yellow}${results.perOp.toFixed(3)}ms${colors.reset}`);
  console.log(`  Ops/second: ${colors.green}${(1000 / results.perOp).toFixed(1)}${colors.reset}`);
  console.log();
}

function printComparison(label, atlasResult, siloResult) {
  const atlasFaster = atlasResult.perOp < siloResult.perOp;
  const ratio = atlasFaster
    ? (siloResult.perOp / atlasResult.perOp).toFixed(2)
    : (atlasResult.perOp / siloResult.perOp).toFixed(2);
  const winner = atlasFaster ? "Atlas" : "Silo";
  const color = atlasFaster ? colors.blue : colors.green;

  console.log(`${colors.bright}${label}:${colors.reset}`);
  console.log(`  Atlas: ${atlasResult.perOp.toFixed(3)}ms/op (${(1000/atlasResult.perOp).toFixed(1)} ops/s)`);
  console.log(`  Silo:  ${siloResult.perOp.toFixed(3)}ms/op (${(1000/siloResult.perOp).toFixed(1)} ops/s)`);
  console.log(`  ${color}Winner: ${winner} (${ratio}x faster)${colors.reset}`);
  console.log();
}

// Main benchmark runner
async function runBenchmark(config, options = {}) {
  const writeCount = options.writeCount || 100;
  const readCount = options.readCount || 100;

  printHeader(`Benchmarking: ${config.name}`);

  console.log(`${colors.bright}Connection:${colors.reset} ${config.connectionString.replace(/:[^:@]+@/, ':****@')}`);
  console.log(`${colors.bright}Database:${colors.reset} ${config.dbName}\n`);

  // Test connection
  console.log("Testing connection...");
  const connResult = await benchmarkConnection(config);
  if (!connResult.success) {
    console.log(`${colors.red}❌ Connection failed: ${connResult.error}${colors.reset}`);
    return null;
  }
  console.log(`${colors.green}✅ Connected in ${connResult.duration.toFixed(2)}ms${colors.reset}\n`);

  // Connect for benchmarks
  const client = new MongoClient(config.connectionString, {
    maxPoolSize: 20,
    minPoolSize: 5,
  });

  try {
    await client.connect();
    const db = client.db(config.dbName);

    const results = {
      connection: connResult.duration,
      writes: null,
      reads: null,
      updates: null,
    };

    // Write benchmarks
    console.log(`${colors.bright}Running write benchmarks (${writeCount} docs)...${colors.reset}`);
    results.writes = await benchmarkWrites(db, writeCount);
    printResults("  Single insertOne operations", results.writes.singleInserts);
    printResults("  Bulk insertMany operation", results.writes.bulkInsert);

    // Read benchmarks
    console.log(`${colors.bright}Running read benchmarks...${colors.reset}`);
    results.reads = await benchmarkReads(db, readCount);
    printResults("  findOne by _id", results.reads.findById);
    printResults("  find with query + sort", results.reads.findWithSort);
    printResults("  Aggregation pipeline", results.reads.aggregation);

    // Update benchmarks
    console.log(`${colors.bright}Running update benchmarks...${colors.reset}`);
    results.updates = await benchmarkUpdates(db, readCount);
    printResults("  updateOne with $set + $inc", results.updates);

    // Cleanup
    console.log(`${colors.bright}Cleaning up...${colors.reset}`);
    const deleted = await cleanup(db);
    console.log(`${colors.green}✅ Deleted ${deleted} test documents${colors.reset}\n`);

    await client.close();
    return results;

  } catch (error) {
    console.error(`${colors.red}Error during benchmark: ${error.message}${colors.reset}`);
    await client.close();
    return null;
  }
}

// CLI
async function main() {
  const args = process.argv.slice(2);
  const options = {
    writeCount: 100,
    readCount: 100,
  };

  // Parse args
  for (let i = 0; i < args.length; i++) {
    if (args[i] === '--writes' || args[i] === '-w') {
      options.writeCount = parseInt(args[i + 1]) || 100;
      i++;
    } else if (args[i] === '--reads' || args[i] === '-r') {
      options.readCount = parseInt(args[i + 1]) || 100;
      i++;
    } else if (args[i] === '--help' || args[i] === '-h') {
      console.log(`
${colors.bright}MongoDB Performance Benchmark${colors.reset}

Usage: node benchmark-mongodb.mjs [options]

Options:
  -w, --writes <count>    Number of documents for write tests (default: 100)
  -r, --reads <count>     Number of operations for read tests (default: 100)
  -h, --help             Show this help

Environment Variables:
  MONGODB_CONNECTION_STRING       Atlas connection string
  MONGODB_NAME                    Atlas database name
  SILO_MONGODB_CONNECTION_STRING  Silo connection string (default: mongodb://silo.aesthetic.computer:27017/aesthetic)
  SILO_MONGODB_NAME              Silo database name (default: aesthetic)

Examples:
  node benchmark-mongodb.mjs
  node benchmark-mongodb.mjs --writes 500 --reads 200
      `);
      process.exit(0);
    }
  }

  console.log(`${colors.bright}${colors.blue}`);
  console.log(`╔════════════════════════════════════════════════════════════╗`);
  console.log(`║         MongoDB Performance Benchmark Tool                 ║`);
  console.log(`║  Comparing: Atlas vs silo.aesthetic.computer               ║`);
  console.log(`╚════════════════════════════════════════════════════════════╝`);
  console.log(colors.reset);

  // Check environment variables
  if (!configs.atlas.connectionString) {
    console.log(`${colors.red}❌ MONGODB_CONNECTION_STRING not set${colors.reset}`);
    console.log(`${colors.yellow}Set your Atlas connection string to test both databases${colors.reset}\n`);
    process.exit(1);
  }

  // Run benchmarks for both databases
  const atlasResults = await runBenchmark(configs.atlas, options);
  if (!atlasResults) {
    console.log(`${colors.red}Failed to benchmark Atlas${colors.reset}`);
    process.exit(1);
  }

  const siloResults = await runBenchmark(configs.silo, options);
  if (!siloResults) {
    console.log(`${colors.red}Failed to benchmark Silo${colors.reset}`);
    process.exit(1);
  }

  // Print comparison
  printHeader("Performance Comparison");

  console.log(`${colors.bright}Connection Time:${colors.reset}`);
  console.log(`  Atlas: ${atlasResults.connection.toFixed(2)}ms`);
  console.log(`  Silo:  ${siloResults.connection.toFixed(2)}ms`);
  const connFaster = atlasResults.connection < siloResults.connection ? "Atlas" : "Silo";
  const connRatio = atlasResults.connection < siloResults.connection
    ? (siloResults.connection / atlasResults.connection).toFixed(2)
    : (atlasResults.connection / siloResults.connection).toFixed(2);
  console.log(`  ${colors.green}Winner: ${connFaster} (${connRatio}x faster)${colors.reset}\n`);

  printComparison(
    "Single insertOne (per operation)",
    atlasResults.writes.singleInserts,
    siloResults.writes.singleInserts
  );

  printComparison(
    "Bulk insertMany (per operation)",
    atlasResults.writes.bulkInsert,
    siloResults.writes.bulkInsert
  );

  printComparison(
    "findOne by _id",
    atlasResults.reads.findById,
    siloResults.reads.findById
  );

  printComparison(
    "find with query + sort",
    atlasResults.reads.findWithSort,
    siloResults.reads.findWithSort
  );

  printComparison(
    "Aggregation pipeline",
    atlasResults.reads.aggregation,
    siloResults.reads.aggregation
  );

  printComparison(
    "updateOne operations",
    atlasResults.updates,
    siloResults.updates
  );

  // Summary
  printHeader("Summary");

  const tests = [
    { name: "Single Inserts", atlas: atlasResults.writes.singleInserts, silo: siloResults.writes.singleInserts },
    { name: "Bulk Inserts", atlas: atlasResults.writes.bulkInsert, silo: siloResults.writes.bulkInsert },
    { name: "Find by ID", atlas: atlasResults.reads.findById, silo: siloResults.reads.findById },
    { name: "Find + Sort", atlas: atlasResults.reads.findWithSort, silo: siloResults.reads.findWithSort },
    { name: "Aggregation", atlas: atlasResults.reads.aggregation, silo: siloResults.reads.aggregation },
    { name: "Updates", atlas: atlasResults.updates, silo: siloResults.updates },
  ];

  let atlasWins = 0;
  let siloWins = 0;

  tests.forEach(test => {
    if (test.atlas.perOp < test.silo.perOp) {
      atlasWins++;
    } else {
      siloWins++;
    }
  });

  console.log(`Atlas won ${atlasWins} of ${tests.length} tests`);
  console.log(`Silo won ${siloWins} of ${tests.length} tests\n`);

  const avgAtlas = tests.reduce((sum, t) => sum + t.atlas.perOp, 0) / tests.length;
  const avgSilo = tests.reduce((sum, t) => sum + t.silo.perOp, 0) / tests.length;

  console.log(`${colors.bright}Average operation time:${colors.reset}`);
  console.log(`  Atlas: ${avgAtlas.toFixed(3)}ms`);
  console.log(`  Silo:  ${avgSilo.toFixed(3)}ms`);

  const overallWinner = avgAtlas < avgSilo ? "Atlas" : "Silo";
  const overallRatio = avgAtlas < avgSilo
    ? (avgSilo / avgAtlas).toFixed(2)
    : (avgAtlas / avgSilo).toFixed(2);
  console.log(`  ${colors.green}Overall: ${overallWinner} is ${overallRatio}x faster on average${colors.reset}\n`);

  console.log(`${colors.bright}${colors.yellow}Note:${colors.reset} Network latency, server load, and other factors can affect results.`);
  console.log(`${colors.yellow}Run multiple times and average results for more accurate comparison.${colors.reset}\n`);
}

main().catch(console.error);
