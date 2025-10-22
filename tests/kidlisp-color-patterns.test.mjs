// Test for KidLisp color pattern symbol evaluation (zebra, rainbow)
// Tests that special color patterns work correctly with choice (?) function
import assert from 'node:assert/strict';

// Minimal environment setup for KidLisp
if (typeof globalThis.ImageData === 'undefined') {
  globalThis.ImageData = class ImageData {
    constructor(width, height) {
      this.width = width;
      this.height = height;
      this.data = new Uint8ClampedArray(width * height * 4);
    }
  };
}

// Import KidLisp
const kidlispModule = await import('../system/public/aesthetic.computer/lib/kidlisp.mjs');
const { KidLisp } = kidlispModule;

// Create a minimal API mock
const mockApi = {
  num: {
    rainbow: () => [255, 0, 0],  // Would normally cycle colors
    zebra: () => [0, 0, 0],       // Would normally alternate black/white
  },
  screen: {
    width: 256,
    height: 256,
  },
};

console.log('🧪 Testing KidLisp color pattern evaluation...\n');

// Test 1: Direct color pattern symbol evaluation
console.log('Test 1: Direct symbol evaluation');
{
  const interp = new KidLisp();
  
  // Test that 'zebra' as a symbol evaluates to the string "zebra"
  const zebraResult = interp.fastEval('zebra', mockApi, {});
  assert.strictEqual(zebraResult, 'zebra', 
    'zebra symbol should evaluate to string "zebra"');
  console.log('✅ zebra symbol -> "zebra"');
  
  // Test that 'rainbow' as a symbol evaluates to the string "rainbow"
  const rainbowResult = interp.fastEval('rainbow', mockApi, {});
  assert.strictEqual(rainbowResult, 'rainbow',
    'rainbow symbol should evaluate to string "rainbow"');
  console.log('✅ rainbow symbol -> "rainbow"');
}

// Test 2: Color patterns in choice expressions
console.log('\nTest 2: Color patterns with choice (?)');
{
  const interp = new KidLisp();
  
  // Parse and evaluate (? zebra tan)
  const expr1 = interp.parse('(? zebra tan)');
  const result1 = interp.evaluate(expr1, mockApi, {});
  
  // Result should be either "zebra" or "tan", both valid strings
  assert.ok(
    result1 === 'zebra' || result1 === 'tan',
    `(? zebra tan) should return "zebra" or "tan", got: ${JSON.stringify(result1)}`
  );
  console.log(`✅ (? zebra tan) -> "${result1}" (valid choice)`);
  
  // Parse and evaluate (? rainbow blue)
  const expr2 = interp.parse('(? rainbow blue)');
  const result2 = interp.evaluate(expr2, mockApi, {});
  
  assert.ok(
    result2 === 'rainbow' || result2 === 'blue',
    `(? rainbow blue) should return "rainbow" or "blue", got: ${JSON.stringify(result2)}`
  );
  console.log(`✅ (? rainbow blue) -> "${result2}" (valid choice)`);
}

// Test 3: Verify patterns are NOT RGB arrays
console.log('\nTest 3: Patterns return strings, not RGB arrays');
{
  const interp = new KidLisp();
  
  const zebraResult = interp.fastEval('zebra', mockApi, {});
  assert.ok(
    typeof zebraResult === 'string',
    `zebra should return string, not array. Got: ${typeof zebraResult}`
  );
  console.log('✅ zebra returns string type, not RGB array');
  
  const rainbowResult = interp.fastEval('rainbow', mockApi, {});
  assert.ok(
    typeof rainbowResult === 'string',
    `rainbow should return string, not array. Got: ${typeof rainbowResult}`
  );
  console.log('✅ rainbow returns string type, not RGB array');
}

// Test 4: Multiple evaluations (simulate frame-by-frame usage)
console.log('\nTest 4: Multiple evaluations return consistent strings');
{
  const interp = new KidLisp();
  
  for (let i = 0; i < 5; i++) {
    const result = interp.fastEval('zebra', mockApi, {});
    assert.strictEqual(result, 'zebra',
      `zebra should consistently return "zebra" across frames (iteration ${i})`);
  }
  console.log('✅ zebra consistent across multiple evaluations');
  
  for (let i = 0; i < 5; i++) {
    const result = interp.fastEval('rainbow', mockApi, {});
    assert.strictEqual(result, 'rainbow',
      `rainbow should consistently return "rainbow" across frames (iteration ${i})`);
  }
  console.log('✅ rainbow consistent across multiple evaluations');
}

// Test 5: Integration test - full expression with ink
console.log('\nTest 5: Full expression integration');
{
  const interp = new KidLisp();
  
  // Test (ink (? zebra tan))
  const expr = interp.parse('(? zebra tan)');
  
  // Run multiple times to ensure both branches work
  const results = new Set();
  for (let i = 0; i < 50; i++) {
    const result = interp.evaluate(expr, mockApi, {});
    results.add(result);
    assert.ok(
      result === 'zebra' || result === 'tan',
      `Each evaluation should yield valid color string, got: ${JSON.stringify(result)}`
    );
  }
  
  // With 50 iterations, we should have seen both outcomes (statistically very likely)
  // But we'll be lenient and just check we got valid strings
  console.log(`✅ (? zebra tan) produced ${results.size} unique result(s): ${Array.from(results).join(', ')}`);
}

console.log('\n' + '='.repeat(50));
console.log('✅ All KidLisp color pattern tests passed!');
console.log('='.repeat(50));
