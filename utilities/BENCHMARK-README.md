# MongoDB Performance Benchmark

Compare read/write performance between MongoDB Atlas (production) and silo.aesthetic.computer (self-hosted).

## Quick Start

### 1. Set Environment Variables

You need your Atlas credentials. The silo connection will use defaults if not specified:

```bash
# Required: Your production Atlas credentials
export MONGODB_CONNECTION_STRING="mongodb+srv://user:pass@aesthetic.qencn.mongodb.net/?retryWrites=true&w=majority"
export MONGODB_NAME="aesthetic"

# Optional: Override silo connection (defaults shown)
export SILO_MONGODB_CONNECTION_STRING="mongodb://silo.aesthetic.computer:27017/aesthetic"
export SILO_MONGODB_NAME="aesthetic"
```

### 2. Run the Benchmark

```bash
# Basic benchmark (100 writes, 100 reads)
node utilities/benchmark-mongodb.mjs

# Custom test sizes
node utilities/benchmark-mongodb.mjs --writes 500 --reads 200

# Show help
node utilities/benchmark-mongodb.mjs --help
```

## What It Tests

### Connection
- Time to establish connection and ping database

### Write Operations
- **Single insertOne**: Inserts documents one at a time (simulates real-time operations)
- **Bulk insertMany**: Batch insert (simulates migrations or bulk imports)

### Read Operations
- **findOne by _id**: Point queries by primary key
- **find with query + sort**: Range queries with sorting (e.g., recent moods)
- **Aggregation pipeline**: Complex queries with lookups (e.g., moods + handles)

### Update Operations
- **updateOne with $set + $inc**: Update fields and increment counters

## Sample Output

```
╔════════════════════════════════════════════════════════════╗
║         MongoDB Performance Benchmark Tool                 ║
║  Comparing: Atlas vs silo.aesthetic.computer               ║
╚════════════════════════════════════════════════════════════╝

============================================================
  Benchmarking: MongoDB Atlas (Production)
============================================================

Connection: mongodb+srv://****@aesthetic.qencn.mongodb.net/...
Database: aesthetic

Testing connection...
✅ Connected in 245.32ms

Running write benchmarks (100 docs)...
  Single insertOne operations:
    Operations: 100
    Total time: 3421.45ms
    Per operation: 34.215ms
    Ops/second: 29.2

  Bulk insertMany operation:
    Operations: 100
    Total time: 156.78ms
    Per operation: 1.568ms
    Ops/second: 637.8

...

============================================================
  Performance Comparison
============================================================

Single insertOne (per operation):
  Atlas: 34.215ms/op (29.2 ops/s)
  Silo:  12.453ms/op (80.3 ops/s)
  Winner: Silo (2.75x faster)

...

============================================================
  Summary
============================================================

Atlas won 2 of 6 tests
Silo won 4 of 6 tests

Average operation time:
  Atlas: 18.542ms
  Silo:  8.321ms
  Overall: Silo is 2.23x faster on average
```

## Interpreting Results

### Network Latency
- **Atlas**: Hosted on MongoDB's cloud, latency depends on:
  - Your network connection
  - Geographic distance to Atlas cluster
  - Internet routing

- **Silo**: Self-hosted, latency depends on:
  - Your connection to DigitalOcean datacenter
  - Server location (same datacenter as your other services = faster)

### When Atlas Might Be Faster
- Aggregation pipelines (more CPU/RAM on Atlas clusters)
- Very large datasets (sharding, replication)
- If your app servers are geographically distributed

### When Silo Might Be Faster
- Point queries (findOne by _id)
- Simple writes (insertOne, updateOne)
- If app servers are in same datacenter as silo
- Bulk operations on small-medium datasets

## Important Notes

### Network Conditions
Run the benchmark multiple times at different times of day. Network latency can vary significantly.

### Server Load
Results can vary based on current load on both databases. For fairest comparison:
- Run during off-peak hours
- Run multiple times and average
- Clear caches between runs if possible

### Data Cleanup
The benchmark creates a temporary collection `benchmark_test` and automatically cleans up all test documents after completion.

### Production Safety
- Uses a separate test collection, doesn't touch real data
- Only requires read/write access, no admin operations
- Safe to run on production Atlas (minimal load)
- Confirm silo connection before running to avoid errors

## Advanced Usage

### Custom Connection Strings

Test with different connection parameters:

```bash
# Test with different read preferences
export MONGODB_CONNECTION_STRING="mongodb+srv://user:pass@cluster/?readPreference=secondaryPreferred"

# Test with compression
export MONGODB_CONNECTION_STRING="mongodb+srv://user:pass@cluster/?compressors=snappy"
```

### Testing Specific Workloads

Modify the script to match your actual usage patterns:
- Change document sizes in `generateTestDocument()`
- Adjust query patterns in `benchmarkReads()`
- Add indexes to test collection for realistic comparison

## Troubleshooting

### Connection Timeout to Silo
```
❌ Connection failed: connect ETIMEDOUT
```
- Check if silo.aesthetic.computer is accessible
- Verify MongoDB is running on port 27017
- Check firewall rules

### Atlas Authentication Failed
```
❌ Connection failed: bad auth
```
- Verify MONGODB_CONNECTION_STRING credentials
- Check IP allowlist in Atlas (allow your IP)

### Out of Memory
```
Error: Cannot allocate memory
```
- Reduce test size: `--writes 50 --reads 50`
- Close other applications
