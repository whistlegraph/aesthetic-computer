# KidLisp V2 Architecture Tests

This directory contains external tests to validate the proposed KidLisp V2 architecture before implementation.

## Test Structure

- `mock-network.mjs` - Mock network layer for /api/store-kidlisp
- `cache-manager.test.mjs` - Test unified cache manager
- `instance-manager.test.mjs` - Test instance hierarchy management
- `recursion-detection.test.mjs` - Test cycle detection and recursion limits
- `performance-benchmark.mjs` - Measure network call redundancies
- `integration.test.mjs` - End-to-end architecture validation

## Running Tests

```bash
cd /workspaces/aesthetic-computer/tests/kidlisp-v2-architecture
node cache-manager.test.mjs
node instance-manager.test.mjs
node recursion-detection.test.mjs
node performance-benchmark.mjs
node integration.test.mjs
```

## Test Scenarios

### Network Call Redundancy Tests
- $cow example (embeds $39i and $r2f)
- Multiple embedded layers with same $codes
- Disk API vs main KidLisp cache separation
- Batch vs individual fetch optimization

### Recursion Detection Tests  
- Simple cycles (A → B → A)
- Complex cycles (A → B → C → A)
- Deep recursion (linear chain)
- Mixed recursion patterns

### Instance Management Tests
- Parent-child hierarchy
- Memory cleanup and garbage collection
- State isolation between instances
- Shared vs isolated caches
