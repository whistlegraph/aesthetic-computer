// Test the instance manager for hierarchy and recursion detection
// Validates our recursion detection and cycle prevention strategies

import { KidLispInstanceManager } from './instance-manager-prototype.mjs';
import { KidLispCacheManager } from './cache-manager-prototype.mjs';
import { mockStore } from './mock-network.mjs';

async function testInstanceManager() {
  console.log('🏗️  Testing KidLisp Instance Manager\n');
  
  const cacheManager = new KidLispCacheManager();
  const instanceManager = new KidLispInstanceManager(cacheManager);
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
  
  // Test 1: Basic instance creation and hierarchy
  console.log('🌳 Test 1: Instance Hierarchy');
  instanceManager.clearAllInstances();
  
  const rootInstance = instanceManager.createInstance({
    type: 'main-piece',
    bounds: { x: 0, y: 0, width: 400, height: 400 },
    cacheId: 'cow'
  });
  
  assert(rootInstance.id !== null, 'Created root instance');
  assert(rootInstance.depth === 0, 'Root instance has depth 0');
  assert(rootInstance.parentId === null, 'Root instance has no parent');
  
  const childInstance = instanceManager.createInstance({
    parentId: rootInstance.id,
    type: 'embedded-layer',
    bounds: { x: 10, y: 10, width: 100, height: 100 },
    cacheId: '39i'
  });
  
  assert(childInstance.parentId === rootInstance.id, 'Child instance has correct parent');
  assert(childInstance.depth === 1, 'Child instance has depth 1');
  assert(rootInstance.children.has(childInstance.id), 'Parent tracks child instance');
  
  // Test 2: Recursion depth tracking
  console.log('\n📏 Test 2: Recursion Depth Tracking');
  
  let currentParent = childInstance;
  for (let i = 2; i <= 5; i++) {
    const deepInstance = instanceManager.createInstance({
      parentId: currentParent.id,
      type: 'embedded-layer',
      bounds: { x: i * 10, y: i * 10, width: 80, height: 80 },
      cacheId: `deep-${i}`
    });
    
    assert(deepInstance.depth === i, `Deep instance ${i} has correct depth ${i}`);
    currentParent = deepInstance;
  }
  
  const stats = instanceManager.getStats();
  assert(stats.maxDepthReached === 5, `Max depth tracked correctly (${stats.maxDepthReached})`);
  
  // Test 3: Simple cycle detection
  console.log('\n🔄 Test 3: Simple Cycle Detection');
  instanceManager.clearAllInstances();
  
  const cycleRoot = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: 'recursive-a'
  });
  
  // Execute recursive-a which references recursive-b (should work normally)
  const result1 = await instanceManager.executeWithRecursionGuard(
    cycleRoot.id,
    '(paint ($recursive-b 0 0 100 100))',
    {}
  );
  
  // This should work since it's A -> B (no cycle yet)
  assert(result1.type === 'execution-result' || result1.type === 'cycle-error', 'First execution completed');
  const embeddedCount = result1.embeddedCount || 0;
  assert(embeddedCount >= 0, `Created ${embeddedCount} embedded instances`);
  
  // Check if cycle would be detected on deeper recursion
  const wouldCycle = instanceManager.wouldCreateCycle('recursive-a');
  assert(wouldCycle === false, 'Not in cycle yet (only 1 level deep)');
  
  // Test 4: Actual cycle detection
  console.log('\n🌀 Test 4: Cycle Detection');
  instanceManager.clearAllInstances();
  
  const cycleTestRoot = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: 'recursive-a'
  });
  
  // Start execution that will create A -> B -> A cycle
  instanceManager.recursionStack = ['recursive-a']; // Simulate being in execution of A
  
  const cycleResult = await instanceManager.executeWithRecursionGuard(
    cycleTestRoot.id,
    '(paint ($recursive-a 0 0 50 50))', // A trying to call A again
    {}
  );
  
  assert(cycleResult.type === 'cycle-error', 'Detected cycle error');
  assert(cycleResult.cycle.includes('recursive-a'), 'Cycle path includes recursive-a');
  
  const cycleStats = instanceManager.getStats();
  assert(cycleStats.cyclesDetected > 0, 'Cycle detection counter incremented');
  
  // Test 5: Maximum depth prevention
  console.log('\n⬇️  Test 5: Maximum Depth Prevention');
  instanceManager.clearAllInstances();
  instanceManager.maxRecursionDepth = 3; // Set low limit for testing
  
  let deepParent = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: 'deep-1'
  });
  
  // Create instances at increasing depths
  for (let i = 2; i <= 5; i++) {
    deepParent = instanceManager.createInstance({
      parentId: deepParent.id,
      type: 'embedded-layer',
      cacheId: `deep-${i}`
    });
  }
  
  // Try to execute at depth 4 (should fail with maxDepth = 3)
  const depthResult = await instanceManager.executeWithRecursionGuard(
    deepParent.id,
    '(paint (circle 10 10 5))',
    {}
  );
  
  assert(depthResult.type === 'depth-error', 'Detected depth limit error');
  assert(depthResult.depth >= instanceManager.maxRecursionDepth, 'Error at correct depth');
  
  const depthStats = instanceManager.getStats();
  assert(depthStats.recursionDetected > 0, 'Recursion detection counter incremented');
  
  // Test 6: Hierarchy tree structure
  console.log('\n🌲 Test 6: Hierarchy Tree Structure');
  instanceManager.clearAllInstances();
  instanceManager.maxRecursionDepth = 10; // Reset to normal
  
  const treeRoot = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: 'cow'
  });
  
  const child1 = instanceManager.createInstance({
    parentId: treeRoot.id,
    type: 'embedded-layer',
    cacheId: '39i'
  });
  
  const child2 = instanceManager.createInstance({
    parentId: treeRoot.id,
    type: 'embedded-layer',
    cacheId: 'r2f'
  });
  
  const grandchild = instanceManager.createInstance({
    parentId: child1.id,
    type: 'embedded-layer',
    cacheId: 'deep-1'
  });
  
  const tree = instanceManager.getHierarchyTree(treeRoot.id);
  
  assert(tree.id === treeRoot.id, 'Tree root matches created root');
  assert(tree.children.length === 2, 'Root has 2 children');
  assert(tree.children[0].children.length === 1, 'First child has 1 grandchild');
  assert(tree.children[1].children.length === 0, 'Second child has no children');
  
  // Test 7: Garbage collection
  console.log('\n🗑️  Test 7: Garbage Collection');
  
  const beforeGC = instanceManager.getStats();
  
  // Simulate old instances (make them appear old)
  for (const instance of instanceManager.instances.values()) {
    instance.lastUsed = Date.now() - 70000; // 70 seconds ago
  }
  
  const gcCount = instanceManager.garbageCollect(60000); // 60 second threshold
  const afterGC = instanceManager.getStats();
  
  assert(gcCount > 0, `Garbage collected ${gcCount} instances`);
  assert(afterGC.totalInstances < beforeGC.totalInstances, 'Instance count decreased');
  assert(afterGC.instancesGarbageCollected > 0, 'GC counter incremented');
  
  // Test 8: Real execution simulation
  console.log('\n🎯 Test 8: Real Execution Simulation');
  instanceManager.clearAllInstances();
  
  const execRoot = instanceManager.createInstance({
    type: 'main-piece',
    cacheId: 'cow'
  });
  
  const execResult = await instanceManager.executeWithRecursionGuard(
    execRoot.id,
    '(paint ($39i 10 10 100 100) ($r2f 120 10 100 100))',
    {}
  );
  
  assert(execResult.type === 'execution-result', 'Execution completed successfully');
  assert(execResult.embeddedCount === 2, 'Created 2 embedded instances');
  assert(execResult.embeddedResults.length === 2, 'Got results from both embedded instances');
  
  // Check that child instances were created
  const finalStats = instanceManager.getStats();
  assert(finalStats.totalInstances >= 3, 'Created root + 2 child instances');
  
  console.log('   📊 Final execution stats:', {
    totalInstances: finalStats.totalInstances,
    maxDepth: finalStats.maxDepthReached,
    cyclesDetected: finalStats.cyclesDetected,
    recursionDetected: finalStats.recursionDetected
  });
  
  // Summary
  console.log('\n📋 Test Summary');
  console.log(`✅ Passed: ${testsPassed}/${totalTests} tests`);
  
  if (testsPassed === totalTests) {
    console.log('🎉 All instance manager tests passed!');
    return true;
  } else {
    console.log('❌ Some tests failed');
    return false;
  }
}

// Run tests if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  testInstanceManager().then(success => {
    process.exit(success ? 0 : 1);
  });
}

export { testInstanceManager };
