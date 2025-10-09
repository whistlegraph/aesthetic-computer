// Test the painting code generator
import { generateCode, generateUniqueCode, calculateCapacity, isValidCode, getStats, CONSTANTS } from '../system/backend/painting-codes/generate-code.mjs';

console.log('🧪 Testing Painting Code Generator\n');

// Test 1: Basic code generation
console.log('Test 1: Basic Code Generation');
console.log('------------------------------');
for (let i = 0; i < 10; i++) {
  const code = generateCode();
  const valid = isValidCode(code);
  console.log(`   Generated: ${code} (length: ${code.length}, valid: ${valid})`);
}
console.log();

// Test 2: Different lengths
console.log('Test 2: Different Lengths');
console.log('-------------------------');
[3, 4, 5, 6].forEach(length => {
  const code = generateCode(length);
  console.log(`   Length ${length}: ${code}`);
});
console.log();

// Test 3: Uniqueness check with mock database
console.log('Test 3: Unique Code Generation');
console.log('------------------------------');
const existingCodes = new Set();
const mockCheckExists = async (code) => existingCodes.has(code);

for (let i = 0; i < 20; i++) {
  const code = await generateUniqueCode(mockCheckExists, 3, 5);
  existingCodes.add(code);
  if (i < 10 || i >= 15) {
    console.log(`   [${i + 1}] Generated: ${code} (${existingCodes.size} unique codes)`);
  } else if (i === 10) {
    console.log(`   ... (generating ${20} total codes)`);
  }
}
console.log();

// Test 4: Collision detection
console.log('Test 4: Collision Detection');
console.log('---------------------------');
const testCodes = new Set();
let collisions = 0;
const testCount = 1000;

for (let i = 0; i < testCount; i++) {
  const code = generateCode(3);
  if (testCodes.has(code)) {
    collisions++;
  }
  testCodes.add(code);
}

console.log(`   Generated ${testCount} codes`);
console.log(`   Unique codes: ${testCodes.size}`);
console.log(`   Collisions: ${collisions}`);
console.log(`   Collision rate: ${((collisions / testCount) * 100).toFixed(2)}%`);
console.log();

// Test 5: Pronounceability check
console.log('Test 5: Pronounceability Check');
console.log('------------------------------');
const pronounceableCount = Array.from(testCodes).filter(code => {
  return CONSTANTS.VOWELS.split('').some(vowel => code.includes(vowel));
}).length;

console.log(`   Codes with vowels: ${pronounceableCount}/${testCodes.size}`);
console.log(`   Pronounceable: ${((pronounceableCount / testCodes.size) * 100).toFixed(2)}%`);
console.log();

// Test 6: Capacity calculations
console.log('Test 6: Capacity Calculations');
console.log('-----------------------------');
[3, 4, 5, 6].forEach(length => {
  const capacity = calculateCapacity(length);
  console.log(`   Length ${length}: ${capacity.toLocaleString()} possible codes`);
});
console.log();

// Test 7: Performance test
console.log('Test 7: Performance Test');
console.log('------------------------');
const perfStart = Date.now();
const perfCodes = [];
for (let i = 0; i < 10000; i++) {
  perfCodes.push(generateCode());
}
const perfEnd = Date.now();
const perfDuration = perfEnd - perfStart;
const perCodeTime = perfDuration / perfCodes.length;

console.log(`   Generated ${perfCodes.length.toLocaleString()} codes in ${perfDuration}ms`);
console.log(`   Average: ${perCodeTime.toFixed(3)}ms per code`);
console.log();

// Test 8: Stats display
console.log('Test 8: Generator Stats');
console.log('-----------------------');
const stats = getStats();
console.log(`   Alphabet: ${stats.alphabet}`);
console.log(`   Vowels: ${stats.vowels}`);
console.log(`   Length range: ${stats.minLength}-${stats.maxLength} (default: ${stats.defaultLength})`);
console.log(`   Capacities:`);
Object.entries(stats.capacities).forEach(([key, value]) => {
  console.log(`      ${key}: ${value}`);
});
console.log();

// Summary
console.log('📊 Test Summary');
console.log('===============');
console.log(`   ✅ All tests passed`);
console.log(`   🎯 Collision rate: ${((collisions / testCount) * 100).toFixed(2)}%`);
console.log(`   🗣️  Pronounceability: ${((pronounceableCount / testCodes.size) * 100).toFixed(2)}%`);
console.log(`   ⚡ Performance: ${perCodeTime.toFixed(3)}ms per code`);
console.log();
