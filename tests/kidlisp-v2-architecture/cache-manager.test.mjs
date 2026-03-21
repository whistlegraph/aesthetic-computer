// Test the unified cache manager for network call deduplication
// Validates our assumptions about redundant network requests

import { KidLispCacheManager } from './cache-manager-prototype.mjs';
import { mockStore } from './mock-network.mjs';

async function testCacheManager() {
  console.log('🧪 Testing KidLisp Cache Manager\n');
  
  const cacheManager = new KidLispCacheManager();
  let testsPassed = 0;
  let totalTests = 0;
  
  function assert(condition, message) {
    totalTests++;
    if (condition) {
      console.log(`✅ ${message}`);
      testsPassed++;
    } else {
      console.log(`❌ ${message}`);
    }
  }
  
  // Test 1: Basic cache functionality
  console.log('📦 Test 1: Basic Cache Functionality');
  cacheManager.clearCache();
  mockStore.clearRequestLog();
  
  const cowCode = await cacheManager.getCachedCode('cow');
  assert(cowCode !== null, 'Successfully fetched $cow code');
  assert(mockStore.getRequestStats().total === 1, 'Made exactly 1 network request');
  
  // Second request should hit cache
  const cowCode2 = await cacheManager.getCachedCode('cow');
  assert(cowCode === cowCode2, 'Second request returned same code');
  assert(mockStore.getRequestStats().total === 1, 'No additional network request (cache hit)');
  
  const efficiency = cacheManager.getCacheEfficiency();
  assert(efficiency.hitRate === 50, `Cache hit rate is 50% (got ${efficiency.hitRate}%)`);
  
  // Test 2: Request deduplication
  console.log('\n🔄 Test 2: Request Deduplication');
  cacheManager.clearCache();
  mockStore.clearRequestLog();
  
  // Make multiple concurrent requests for same code
  const promises = [
    cacheManager.getCachedCode('39i'),
    cacheManager.getCachedCode('39i'),
    cacheManager.getCachedCode('39i')
  ];
  
  const results = await Promise.all(promises);
  assert(results.every(r => r === results[0]), 'All concurrent requests returned same result');
  assert(mockStore.getRequestStats().total === 1, 'Only 1 network request despite 3 concurrent calls');
  assert(cacheManager.getCacheEfficiency().deduplicatedRequests === 2, 'Detected 2 deduplicated requests');
  
  // Test 3: Batch fetching optimization
  console.log('\n📦 Test 3: Batch Fetching');
  cacheManager.clearCache();
  mockStore.clearRequestLog();
  
  const batchCodes = ['39i', 'r2f', 'cow'];
  const batchResult = await cacheManager.getBatchCachedCodes(batchCodes);
  
  assert(Object.keys(batchResult).length === 3, 'Batch fetch returned all 3 codes');
  assert(mockStore.getRequestStats().total === 1, 'Used single batch request instead of 3 individual requests');
  assert(mockStore.getRequestStats().batch === 1, 'Made exactly 1 batch request');
  
  // Test 4: $cow scenario simulation (the key redundancy case)
  console.log('\n🐄 Test 4: $cow Scenario (Redundancy Detection)');
  cacheManager.clearCache();
  mockStore.clearRequestLog();
  
  // Simulate current architecture behavior (separate fetches)
  const cowCodeSimulated = await cacheManager.getCachedCode('cow');
  const extractedCodes = cacheManager.extractDollarCodes(cowCodeSimulated);
  
  assert(extractedCodes.includes('39i'), 'Extracted $39i from $cow code');
  assert(extractedCodes.includes('r2f'), 'Extracted $r2f from $cow code');
  
  // Simulate embedded layer creation (would normally trigger separate fetches)
  const embeddedResults = await cacheManager.getBatchCachedCodes(extractedCodes);
  
  assert(Object.keys(embeddedResults).length === 2, 'Successfully batch-fetched embedded codes');
  
  const stats = mockStore.getRequestStats();
  assert(stats.total === 2, `Total requests: ${stats.total} (should be 2: cow + batch)`);
  assert(stats.batch === 1, 'Used batch request for embedded codes');
  
  console.log('   📊 $cow scenario stats:', {
    totalRequests: stats.total,
    individualRequests: stats.single,
    batchRequests: stats.batch,
    codesInBatch: extractedCodes.length
  });
  
  // Test 5: Compare old vs new architecture performance
  console.log('\n⚡ Test 5: Performance Comparison');
  
  // Simulate OLD architecture with REALISTIC redundancy patterns
  // Based on actual analysis: separate caches, preload+render duplication, no deduplication
  
  // OLD: Simulate the actual redundant pattern we identified
  const oldCacheManager = new KidLispCacheManager();
  const diskCache = new Map(); // Separate singleton cache (current reality)
  mockStore.clearRequestLog();
  
  // 1. Main KidLisp fetch
  await oldCacheManager.getCachedCode('cow');
  
  // 2. Embedded layer preloading (separate requests)
  await oldCacheManager.getCachedCode('39i');
  await oldCacheManager.getCachedCode('r2f');
  
  // 3. Disk API cache (separate from main cache - this is the key redundancy!)
  await mockStore.fetchCode('39i');  // Disk API doesn't share cache
  await mockStore.fetchCode('r2f');  // Disk API doesn't share cache
  
  // 4. Embedded layer rendering (separate requests due to poor coordination)
  await mockStore.fetchCode('39i');  // preloadEmbeddedLayers
  await mockStore.fetchCode('r2f');  // preloadEmbeddedLayers
  
  // 5. Additional context switches (module vs embedded)
  await mockStore.fetchCode('39i');  // createEmbeddedLayer
  await mockStore.fetchCode('r2f');  // createEmbeddedLayer
  
  const oldArchStats = mockStore.getRequestStats();
  
  // NEW: Unified architecture with proper deduplication
  cacheManager.clearCache();
  mockStore.clearRequestLog();
  
  const cowNew = await cacheManager.getCachedCode('cow');
  const embeddedNew = cacheManager.extractDollarCodes(cowNew);
  await cacheManager.getBatchCachedCodes(embeddedNew);
  // All additional requests hit unified cache
  await cacheManager.getCachedCode('39i');  // Cache hit
  await cacheManager.getCachedCode('r2f');  // Cache hit
  await cacheManager.getCachedCode('39i');  // Cache hit
  await cacheManager.getCachedCode('r2f');  // Cache hit
  
  const newArchStats = mockStore.getRequestStats();
  
  const improvement = ((oldArchStats.total - newArchStats.total) / oldArchStats.total * 100);
  
  console.log('   📊 Performance comparison:');
  console.log(`   Old architecture: ${oldArchStats.total} requests`);
  console.log(`   New architecture: ${newArchStats.total} requests`);
  console.log(`   Improvement: ${improvement.toFixed(1)}% reduction`);
  
  assert(newArchStats.total < oldArchStats.total, 'New architecture uses fewer requests');
  assert(improvement >= 50, `Significant improvement (${improvement.toFixed(1)}% >= 50%)`);
  
  // Test 6: Cache efficiency metrics
  console.log('\n📈 Test 6: Cache Efficiency Metrics');
  const finalEfficiency = cacheManager.getCacheEfficiency();
  
  console.log('   📊 Final cache metrics:', finalEfficiency);
  
  assert(finalEfficiency.hitRate > 0, 'Cache hit rate > 0%');
  assert(finalEfficiency.networkRequests > 0, 'Made some network requests');
  assert(finalEfficiency.totalRequests > finalEfficiency.networkRequests, 'Total requests > network requests (cache working)');
  
  // Summary
  console.log('\n📋 Test Summary');
  console.log(`✅ Passed: ${testsPassed}/${totalTests} tests`);
  
  if (testsPassed === totalTests) {
    console.log('🎉 All cache manager tests passed!');
    return true;
  } else {
    console.log('❌ Some tests failed');
    return false;
  }
}

// Run tests if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  testCacheManager().then(success => {
    process.exit(success ? 0 : 1);
  });
}

export { testCacheManager };
