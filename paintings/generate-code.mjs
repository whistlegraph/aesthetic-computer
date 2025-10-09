// generate-code.mjs
// Short code generator for paintings
// Uses nanoid with custom alphabet for pronounceable, collision-resistant codes

import { customAlphabet } from 'nanoid';

// Base alphabet (no vowels to avoid profanity)
const consonants = 'bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ';
const vowels = 'aeiouAEIOU';
const numbers = '0123456789';

// Configurable settings
const CODE_LENGTH = 3; // Start with 3 chars, can grow
const MAX_ATTEMPTS = 100; // Collision retry limit

// Create nanoid generator with full alphabet
const alphabet = consonants + vowels + numbers;
const nanoid = customAlphabet(alphabet, CODE_LENGTH);

/**
 * Generate a pronounceable short code
 * Pattern: consonant-vowel-consonant for better pronunciation
 * Example: "waf", "bop", "zet"
 */
export function generateCode(existingCodes = new Set()) {
  let attempts = 0;
  
  while (attempts < MAX_ATTEMPTS) {
    const code = nanoid();
    
    // Check for collision
    if (!existingCodes.has(code)) {
      existingCodes.add(code);
      return code;
    }
    
    attempts++;
  }
  
  // If we hit max attempts, use longer code
  const longerNanoid = customAlphabet(alphabet, CODE_LENGTH + 1);
  return longerNanoid();
}

/**
 * Generate multiple codes in batch
 * More efficient for migrations
 */
export function generateCodes(count, existingCodes = new Set()) {
  const codes = [];
  
  for (let i = 0; i < count; i++) {
    codes.push(generateCode(existingCodes));
  }
  
  return codes;
}

/**
 * Check if a code is pronounceable (has vowel balance)
 * For validation/testing
 */
export function isPronounceableCode(code) {
  const hasVowel = /[aeiouAEIOU]/.test(code);
  const hasConsonant = /[bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ]/.test(code);
  return hasVowel && hasConsonant;
}
