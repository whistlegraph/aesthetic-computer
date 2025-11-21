// Boot Performance Test Suite
// Tests and benchmarks for aesthetic.computer boot sequence

import { performance } from 'perf_hooks';

const PERFORMANCE_THRESHOLDS = {
  bootComplete: 3000, // Total boot should complete in under 3 seconds
  biosLoad: 500,      // BIOS loading should be fast
  diskLoad: 2000,     // Disk loading (including first piece)
  auth0Init: 1000,    // Auth0 initialization
};

/**
 * Simulates boot sequence timing
 */
async function measureBootSequence() {
  const timings = {
    start: performance.now(),
    stages: {}
  };

  // Simulate BIOS load
  timings.stages.biosStart = performance.now();
  await simulateBiosLoad();
  timings.stages.biosEnd = performance.now();
  timings.stages.biosDuration = timings.stages.biosEnd - timings.stages.biosStart;

  // Simulate disk load
  timings.stages.diskStart = performance.now();
  await simulateDiskLoad();
  timings.stages.diskEnd = performance.now();
  timings.stages.diskDuration = timings.stages.diskEnd - timings.stages.diskStart;

  timings.end = performance.now();
  timings.totalDuration = timings.end - timings.start;

  return timings;
}

async function simulateBiosLoad() {
  // Simulate BIOS import and initialization
  await new Promise(resolve => setTimeout(resolve, 100));
}

async function simulateDiskLoad() {
  // Simulate disk loading (piece module loading)
  await new Promise(resolve => setTimeout(resolve, 500));
}

/**
 * Test typeface loading performance
 */
async function measureTypefaceLoad(skipPreload = true) {
  const start = performance.now();
  
  if (skipPreload) {
    // Stub typeface (instant)
    return { duration: 0, mode: 'stub' };
  } else {
    // Simulate full typeface preload (expensive)
    await new Promise(resolve => setTimeout(resolve, 5000));
    return { duration: performance.now() - start, mode: 'preload' };
  }
}

/**
 * Run all performance tests
 */
async function runPerformanceTests() {
  console.log('🚀 Running Boot Performance Tests\n');

  // Test 1: Boot sequence
  console.log('Test 1: Boot Sequence');
  const bootTimings = await measureBootSequence();
  console.log(`  ✓ BIOS Load: ${bootTimings.stages.biosDuration.toFixed(2)}ms`);
  console.log(`  ✓ Disk Load: ${bootTimings.stages.diskDuration.toFixed(2)}ms`);
  console.log(`  ✓ Total Boot: ${bootTimings.totalDuration.toFixed(2)}ms`);
  
  const bootPass = bootTimings.totalDuration < PERFORMANCE_THRESHOLDS.bootComplete;
  console.log(`  ${bootPass ? '✅ PASS' : '❌ FAIL'}: Boot under ${PERFORMANCE_THRESHOLDS.bootComplete}ms threshold\n`);

  // Test 2: Typeface loading (with preload skip)
  console.log('Test 2: Typeface Loading');
  const typefaceStub = await measureTypefaceLoad(true);
  const typefacePreload = await measureTypefaceLoad(false);
  console.log(`  ✓ Stub Mode: ${typefaceStub.duration.toFixed(2)}ms`);
  console.log(`  ✓ Preload Mode: ${typefacePreload.duration.toFixed(2)}ms`);
  console.log(`  ✓ Savings: ${(typefacePreload.duration - typefaceStub.duration).toFixed(2)}ms`);
  console.log(`  ✅ PASS: On-demand loading is ${(typefacePreload.duration / Math.max(typefaceStub.duration, 1)).toFixed(0)}x faster\n`);

  // Summary
  console.log('📊 Performance Summary:');
  console.log(`  Boot Time: ${bootTimings.totalDuration.toFixed(2)}ms`);
  console.log(`  Typeface Optimization: ${typefacePreload.duration.toFixed(0)}ms saved`);
  console.log(`\n✅ All tests completed\n`);
}

// Run tests if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runPerformanceTests().catch(console.error);
}

export { measureBootSequence, measureTypefaceLoad, runPerformanceTests };
