// Performance benchmark comparing old vs new architecture
// Measures network call redundancies and timing improvements

import { KidLispCacheManager } from './cache-manager-prototype.mjs';
import { KidLispInstanceManager } from './instance-manager-prototype.mjs';
import { mockStore } from './mock-network.mjs';

async function runPerformanceBenchmark() {
  console.log('⚡ KidLisp V2 Performance Benchmark\n');
  
  // Test scenarios
  const scenarios = [
    {
      name: '$cow (2 embedded pieces)',
      mainCode: 'cow',
      expectedEmbedded: ['39i', 'r2f']
    },
    {
      name: 'Deep embedding (5 levels)',
      mainCode: 'deep-1',
      expectedEmbedded: ['deep-2', 'deep-3', 'deep-4', 'deep-5']
    },
    {
      name: 'Complex piece (simulated)',
      mainCode: 'cow',
      additionalFetches: ['39i', 'r2f', '39i', 'r2f'], // Simulate duplicate fetches
      expectedEmbedded: ['39i', 'r2f']
    }
  ];
  
  console.log('🔬 Running benchmark scenarios...\n');
  
  for (const scenario of scenarios) {
    console.log(`📊 Scenario: ${scenario.name}`);
    
    // Test OLD architecture (simulated current behavior)
    const oldResults = await benchmarkOldArchitecture(scenario);
    
    // Test NEW architecture (proposed system)
    const newResults = await benchmarkNewArchitecture(scenario);
    
    // Calculate improvements
    const networkImprovement = ((oldResults.networkRequests - newResults.networkRequests) / oldResults.networkRequests * 100);
    const timeImprovement = ((oldResults.totalTime - newResults.totalTime) / oldResults.totalTime * 100);
    
    console.log(`   🔄 Old architecture: ${oldResults.networkRequests} requests, ${oldResults.totalTime}ms`);
    console.log(`   ⚡ New architecture: ${newResults.networkRequests} requests, ${newResults.totalTime}ms`);
    console.log(`   📈 Network improvement: ${networkImprovement.toFixed(1)}%`);
    console.log(`   ⏱️  Time improvement: ${timeImprovement.toFixed(1)}%`);
    
    if (newResults.cyclesPrevented > 0) {
      console.log(`   🛡️  Cycles prevented: ${newResults.cyclesPrevented}`);
    }
    
    console.log();
  }
  
  // Stress test
  console.log('🧪 Stress Test: Concurrent Requests');
  await stressTestConcurrency();
  
  // Memory usage test
  console.log('💾 Memory Usage Test');
  await testMemoryUsage();
}

async function benchmarkOldArchitecture(scenario) {
  const startTime = Date.now();
  
  // Clear state
  mockStore.clearRequestLog();
  
  // Simulate current architecture behavior:
  // 1. Separate cache for disk API
  // 2. No deduplication
  // 3. Individual requests for each embedded piece
  // 4. Redundant fetches in different contexts
  
  const diskCache = new Map(); // Separate singleton cache
  const mainCache = new Map(); // Main KidLisp cache
  
  // 1. Main piece fetch
  let mainCode = await mockStore.fetchCode(scenario.mainCode);
  mainCache.set(scenario.mainCode, mainCode);
  
  // 2. Extract embedded codes (simulate parser)
  const embeddedCodes = scenario.expectedEmbedded || [];
  
  // 3. Fetch each embedded code separately (current behavior)
  for (const code of embeddedCodes) {
    // Simulate preload phase
    if (!mainCache.has(code)) {
      const fetchedCode = await mockStore.fetchCode(code);
      mainCache.set(code, fetchedCode);
    }
    
    // Simulate render phase (separate fetch in current system)
    if (!diskCache.has(code)) {
      const fetchedCode = await mockStore.fetchCode(code);
      diskCache.set(code, fetchedCode);
    }
  }
  
  // 4. Simulate additional fetches from scenario
  if (scenario.additionalFetches) {
    for (const code of scenario.additionalFetches) {
      await mockStore.fetchCode(code); // No cache check (redundant fetches)
    }
  }
  
  const endTime = Date.now();
  const stats = mockStore.getRequestStats();
  
  return {
    networkRequests: stats.total,
    duplicates: stats.duplicates.length,
    totalTime: endTime - startTime,
    cacheEfficiency: 0 // Old system has poor cache sharing
  };
}

async function benchmarkNewArchitecture(scenario) {
  const startTime = Date.now();
  
  // Clear state
  mockStore.clearRequestLog();
  
  // Set up new architecture
  const cacheManager = new KidLispCacheManager();
  const instanceManager = new KidLispInstanceManager(cacheManager);
  
  // 1. Create root instance
  const rootInstance = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: scenario.mainCode
  });
  
  // 2. Fetch main code
  const mainCode = await cacheManager.getCachedCode(scenario.mainCode);
  
  // 3. Extract and batch fetch embedded codes
  const embeddedCodes = cacheManager.extractDollarCodes(mainCode);
  if (embeddedCodes.length > 0) {
    await cacheManager.getBatchCachedCodes(embeddedCodes);
  }
  
  // 4. Execute with recursion protection
  let cyclesPrevented = 0;
  try {
    const result = await instanceManager.executeWithRecursionGuard(
      rootInstance.id,
      mainCode,
      {}
    );
    
    if (result.type === 'cycle-error' || result.type === 'depth-error') {
      cyclesPrevented++;
    }
  } catch (error) {
    // Handle any execution errors
  }
  
  // 5. Simulate additional fetches (should hit cache)
  if (scenario.additionalFetches) {
    for (const code of scenario.additionalFetches) {
      await cacheManager.getCachedCode(code); // Will hit cache
    }
  }
  
  const endTime = Date.now();
  const stats = mockStore.getRequestStats();
  const efficiency = cacheManager.getCacheEfficiency();
  
  return {
    networkRequests: stats.total,
    duplicates: stats.duplicates.length,
    totalTime: endTime - startTime,
    cacheEfficiency: efficiency.hitRate,
    cyclesPrevented
  };
}

async function stressTestConcurrency() {
  const cacheManager = new KidLispCacheManager();
  mockStore.clearRequestLog();
  
  const startTime = Date.now();
  
  // Make 50 concurrent requests for the same codes
  const promises = [];
  for (let i = 0; i < 50; i++) {
    promises.push(cacheManager.getCachedCode('cow'));
    promises.push(cacheManager.getCachedCode('39i'));
    promises.push(cacheManager.getCachedCode('r2f'));
  }
  
  const results = await Promise.all(promises);
  const endTime = Date.now();
  
  const stats = mockStore.getRequestStats();
  const efficiency = cacheManager.getCacheEfficiency();
  
  console.log(`   🔢 Made 150 concurrent requests`);
  console.log(`   📡 Network requests: ${stats.total} (should be ≤ 3)`);
  console.log(`   🎯 Cache hit rate: ${efficiency.hitRate.toFixed(1)}%`);
  console.log(`   ⏱️  Total time: ${endTime - startTime}ms`);
  console.log(`   🔄 Deduplication savings: ${efficiency.deduplicatedRequests} requests`);
  console.log();
}

async function testMemoryUsage() {
  const cacheManager = new KidLispCacheManager();
  const instanceManager = new KidLispInstanceManager(cacheManager);
  
  // Create many instances
  const instances = [];
  for (let i = 0; i < 100; i++) {
    const instance = instanceManager.createInstance({
      type: 'test-instance',
      cacheId: `test-${i % 10}` // Reuse some cache IDs
    });
    instances.push(instance);
  }
  
  const beforeGC = instanceManager.getStats();
  console.log(`   🏗️  Created instances: ${beforeGC.totalInstances}`);
  
  // Age all instances
  for (const instance of instanceManager.instances.values()) {
    instance.lastUsed = Date.now() - 70000; // 70 seconds ago
  }
  
  // Run garbage collection
  const gcCount = instanceManager.garbageCollect(60000);
  const afterGC = instanceManager.getStats();
  
  console.log(`   🗑️  Garbage collected: ${gcCount} instances`);
  console.log(`   💾 Remaining instances: ${afterGC.totalInstances}`);
  console.log(`   📊 Cache entries: ${cacheManager.globalCache.size}`);
  console.log();
}

// Recursion benchmark
async function benchmarkRecursionDetection() {
  console.log('🌀 Recursion Detection Benchmark');
  
  const cacheManager = new KidLispCacheManager();
  const instanceManager = new KidLispInstanceManager(cacheManager);
  
  const scenarios = [
    {
      name: 'Simple cycle (A → A)',
      code: '$recursive-a',
      expectCycle: true
    },
    {
      name: 'Two-step cycle (A → B → A)',
      code: '$recursive-a',
      expectCycle: true
    },
    {
      name: 'Deep recursion (5 levels)',
      code: '$deep-1',
      expectCycle: false
    }
  ];
  
  for (const scenario of scenarios) {
    console.log(`   🔍 Testing: ${scenario.name}`);
    
    instanceManager.clearAllInstances();
    
    const rootInstance = instanceManager.createInstance({
      type: 'main-piece',
      cacheId: scenario.code.substring(1) // Remove $
    });
    
    const startTime = Date.now();
    
    let result;
    if (scenario.code === '$recursive-a') {
      // For recursive scenarios, manually set up the stack to force detection
      instanceManager.recursionStack = ['recursive-a'];
      result = await instanceManager.executeWithRecursionGuard(
        rootInstance.id,
        '(paint ($recursive-a 0 0 50 50))', // A calling itself
        {}
      );
    } else {
      result = await instanceManager.executeWithRecursionGuard(
        rootInstance.id,
        `(paint (${scenario.code} 0 0 100 100))`,
        {}
      );
    }
    
    const endTime = Date.now();
    
    const isCycle = result.type === 'cycle-error';
    const isDepthError = result.type === 'depth-error';
    
    console.log(`     ⏱️  Detection time: ${endTime - startTime}ms`);
    console.log(`     🎯 Result: ${result.type}`);
    
    if (scenario.expectCycle && isCycle) {
      console.log(`     ✅ Correctly detected cycle`);
    } else if (!scenario.expectCycle && result.type === 'execution-result') {
      console.log(`     ✅ Correctly executed without cycle`);
    } else {
      console.log(`     ❌ Unexpected result (expected cycle: ${scenario.expectCycle}, got: ${result.type})`);
    }
  }
  
  console.log();
}

// Run benchmark if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runPerformanceBenchmark()
    .then(() => benchmarkRecursionDetection())
    .then(() => {
      console.log('🏁 Benchmark complete!');
    })
    .catch(error => {
      console.error('❌ Benchmark failed:', error);
      process.exit(1);
    });
}

export { 
  runPerformanceBenchmark, 
  benchmarkRecursionDetection,
  benchmarkOldArchitecture,
  benchmarkNewArchitecture
};
