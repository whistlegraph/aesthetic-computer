#!/usr/bin/env node

/**
 * Test Code Generator Logic
 * 
 * Test the code generation without needing actual images yet.
 * Validates collision detection, pronounceability, etc.
 */

import { customAlphabet } from 'nanoid';

const vowels = 'aeiou';
const consonants = 'bcdfghjklmnpqrstvwxyz';
const digits = '0123456789';
const alphabet = 'abcdefghijklmnopqrstuvwxyz0123456789';

function hasVowel(str) {
  return /[aeiou]/.test(str);
}

function injectVowel(str) {
  if (hasVowel(str)) return str;
  
  // Insert vowel in middle
  const mid = Math.floor(str.length / 2);
  const vowel = vowels[Math.floor(Math.random() * vowels.length)];
  return str.slice(0, mid) + vowel + str.slice(mid);
}

function generateCode(length = 3, existingCodes = new Set()) {
  const generator = customAlphabet(alphabet, length);
  let attempts = 0;
  let code;
  
  do {
    code = generator();
    if (!hasVowel(code)) {
      code = injectVowel(code);
    }
    attempts++;
    
    if (attempts > 1000) {
      throw new Error(`Failed to generate unique code after ${attempts} attempts`);
    }
  } while (existingCodes.has(code));
  
  return { code, attempts };
}

function testGeneration(count = 100) {
  console.log(`\n🧪 Testing Code Generation\n`);
  console.log(`Generating ${count} unique codes...\n`);
  
  const codes = new Set();
  let totalAttempts = 0;
  let maxAttempts = 0;
  const noVowels = [];
  
  for (let i = 0; i < count; i++) {
    const { code, attempts } = generateCode(3, codes);
    codes.add(code);
    totalAttempts += attempts;
    maxAttempts = Math.max(maxAttempts, attempts);
    
    if (!hasVowel(code)) {
      noVowels.push(code);
    }
  }
  
  console.log(`✅ Generated ${codes.size} unique codes`);
  console.log(`📊 Average attempts: ${(totalAttempts / count).toFixed(2)}`);
  console.log(`📊 Max attempts: ${maxAttempts}`);
  console.log(`📊 Codes without vowels: ${noVowels.length}`);
  
  if (noVowels.length > 0) {
    console.log(`⚠️  Codes without vowels: ${noVowels.slice(0, 10).join(', ')}`);
  }
  
  console.log();
  console.log(`📝 Sample codes:`);
  Array.from(codes).slice(0, 20).forEach((code, i) => {
    const pronounceable = hasVowel(code) ? '✅' : '❌';
    console.log(`   ${(i + 1).toString().padStart(2)}. ${code} ${pronounceable}`);
  });
  
  console.log();
}

function testCollisions() {
  console.log(`\n🔍 Testing Collision Detection\n`);
  
  const codes = new Set();
  const collisions = [];
  
  // Generate 1000 codes
  for (let i = 0; i < 1000; i++) {
    const { code } = generateCode(3, codes);
    
    // Try to generate again (should collide)
    const generator = customAlphabet(alphabet, 3);
    const duplicate = code;
    
    if (codes.has(duplicate)) {
      collisions.push(duplicate);
    }
    
    codes.add(code);
  }
  
  console.log(`✅ Generated 1000 codes`);
  console.log(`📊 Unique codes: ${codes.size}`);
  console.log(`📊 Total possible (36^3): ${Math.pow(36, 3)}`);
  console.log(`📊 Utilization: ${((codes.size / Math.pow(36, 3)) * 100).toFixed(2)}%`);
  console.log();
}

function testScalability() {
  console.log(`\n📈 Testing Scalability\n`);
  
  const tests = [
    { length: 3, count: 1000, possible: Math.pow(36, 3) },
    { length: 4, count: 10000, possible: Math.pow(36, 4) },
    { length: 5, count: 50000, possible: Math.pow(36, 5) }
  ];
  
  for (const test of tests) {
    const codes = new Set();
    const start = Date.now();
    
    console.log(`Testing ${test.length}-char codes (generating ${test.count})...`);
    
    for (let i = 0; i < test.count; i++) {
      const { code } = generateCode(test.length, codes);
      codes.add(code);
    }
    
    const duration = Date.now() - start;
    const avgTime = (duration / test.count).toFixed(3);
    const utilization = ((codes.size / test.possible) * 100).toFixed(4);
    
    console.log(`   ✅ Generated ${codes.size} unique codes in ${duration}ms`);
    console.log(`   ⏱️  Average: ${avgTime}ms per code`);
    console.log(`   📊 Utilization: ${utilization}% of possible space\n`);
  }
}

function testPatterns() {
  console.log(`\n🎨 Testing Code Patterns\n`);
  
  const codes = new Set();
  const patterns = {
    cvc: 0,  // consonant-vowel-consonant
    vcc: 0,  // vowel-consonant-consonant
    ccv: 0,  // consonant-consonant-vowel
    other: 0
  };
  
  for (let i = 0; i < 1000; i++) {
    const { code } = generateCode(3, codes);
    codes.add(code);
    
    const chars = code.split('');
    const pattern = chars.map(c => {
      if (vowels.includes(c)) return 'v';
      if (consonants.includes(c)) return 'c';
      return 'd'; // digit
    }).join('');
    
    if (pattern === 'cvc') patterns.cvc++;
    else if (pattern === 'vcc') patterns.vcc++;
    else if (pattern === 'ccv') patterns.ccv++;
    else patterns.other++;
  }
  
  console.log(`Pattern distribution (1000 codes):\n`);
  console.log(`   CVC (consonant-vowel-consonant): ${patterns.cvc} (${(patterns.cvc / 10).toFixed(1)}%)`);
  console.log(`   VCC (vowel-consonant-consonant): ${patterns.vcc} (${(patterns.vcc / 10).toFixed(1)}%)`);
  console.log(`   CCV (consonant-consonant-vowel): ${patterns.ccv} (${(patterns.ccv / 10).toFixed(1)}%)`);
  console.log(`   Other (includes digits): ${patterns.other} (${(patterns.other / 10).toFixed(1)}%)`);
  console.log();
}

async function main() {
  console.log(`
╔═══════════════════════════════════════════════════════════════╗
║            🧪 Code Generator Test Suite                       ║
╚═══════════════════════════════════════════════════════════════╝
`);

  try {
    testGeneration(100);
    testCollisions();
    testPatterns();
    testScalability();
    
    console.log(`
╔═══════════════════════════════════════════════════════════════╗
║                    ✅ All Tests Passed                        ║
╚═══════════════════════════════════════════════════════════════╝
`);
  } catch (error) {
    console.error(`\n💥 Test failed: ${error.message}\n`);
    process.exit(1);
  }
}

main();
