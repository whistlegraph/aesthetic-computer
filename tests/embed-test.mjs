// Simple test for KidLisp embed functionality
// This is not a formal test suite, just a basic verification

import { KidLisp, fetchCachedCode } from '../system/public/aesthetic.computer/lib/kidlisp.mjs';

// Mock API object for testing
const mockApi = {
  screen: { width: 256, height: 256, pixels: new Uint8ClampedArray(256 * 256 * 4) },
  wipe: (color) => console.log(`Mock wipe: ${color}`),
  ink: (...args) => console.log(`Mock ink: ${args.join(', ')}`),
  box: (...args) => console.log(`Mock box: ${args.join(', ')}`),
  paste: (...args) => console.log(`Mock paste: ${args.join(', ')}`),
  painting: (width, height, callback) => {
    console.log(`Mock painting: ${width}x${height}`);
    const buffer = {
      width,
      height, 
      pixels: new Uint8ClampedArray(width * height * 4)
    };
    if (callback) callback(mockApi);
    return buffer;
  }
};

// Test the embed command
async function testEmbedCommand() {
  console.log('Testing KidLisp embed command...');
  
  const kidlisp = new KidLisp();
  const globalEnv = kidlisp.getGlobalEnv();
  
  // Test that embed command exists
  if (globalEnv.embed) {
    console.log('✅ embed command found in global environment');
    
    // Test embed with invalid cache code
    const result1 = globalEnv.embed(mockApi, ['invalid']);
    console.log('embed with invalid code:', result1);
    
    // Test embed with $-prefixed code format
    const result2 = globalEnv.embed(mockApi, ['$abc123XY']);
    console.log('embed with $-prefixed code:', result2);
    
  } else {
    console.log('❌ embed command not found in global environment');
  }
}

// Test the $-prefixed function resolution
async function testDollarPrefixedFunctions() {
  console.log('\nTesting $-prefixed function resolution...');
  
  const kidlisp = new KidLisp();
  
  // Test resolving a $-prefixed function
  const resolved = kidlisp.resolveFunction('$abc123XY', mockApi, {});
  
  if (resolved && resolved.type === 'cache') {
    console.log('✅ $-prefixed function correctly resolved as cache type');
    
    // Test calling the resolved function
    try {
      const result = resolved.value(mockApi, []);
      console.log('Cache function call result:', result);
    } catch (error) {
      console.log('Cache function call error (expected):', error.message);
    }
  } else {
    console.log('❌ $-prefixed function not resolved correctly');
  }
}

// Test parsing and evaluating $-prefixed function calls
async function testParsing() {
  console.log('\nTesting parsing of $-prefixed function calls...');
  
  const kidlisp = new KidLisp();
  
  // Test parsing a simple $-prefixed call
  try {
    const parsed = kidlisp.parse('($abc123XY)');
    console.log('Parsed ($abc123XY):', JSON.stringify(parsed, null, 2));
    
    // Test parsing with arguments
    const parsed2 = kidlisp.parse('($abc123XY 64 64)');
    console.log('Parsed ($abc123XY 64 64):', JSON.stringify(parsed2, null, 2));
    
    console.log('✅ Parsing works correctly');
  } catch (error) {
    console.log('❌ Parsing error:', error.message);
  }
}

// Run all tests
async function runTests() {
  console.log('🚀 Running KidLisp embed functionality tests...\n');
  
  await testEmbedCommand();
  await testDollarPrefixedFunctions(); 
  await testParsing();
  
  console.log('\n✨ Tests completed!');
  console.log('\nNote: Network-dependent tests will show expected errors');
  console.log('since we don\'t have actual cached codes to test with.');
}

// Export for potential use in other test files
export { testEmbedCommand, testDollarPrefixedFunctions, testParsing, runTests };

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runTests().catch(console.error);
}
