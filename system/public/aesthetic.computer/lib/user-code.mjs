// user-code.mjs
// Generate and validate permanent AC user codes (format: ac[2-9][a-z0-9]{5})

import { customAlphabet } from 'nanoid';

/**
 * Generate a pronounceable AC user code
 * Format: ac + YY + 5 chars (consonant-vowel patterns)
 * Examples: ac25wila (2025), ac24neto (2024), ac26jero (2026)
 * 
 * @param {Date} createdAt - User account creation date (defaults to now)
 */
export function generateUserCode(createdAt = new Date()) {
  // Get last 2 digits of year
  const year = createdAt.getFullYear();
  const yearSuffix = String(year).slice(-2); // "2025" -> "25"
  
  // Generate pronounceable 5-character suffix
  const suffix = generatePronounceable(5);
  
  return `ac${yearSuffix}${suffix}`;
}

/**
 * Generate pronounceable suffix using consonant-vowel patterns
 * Follows KidLisp makePronounceable strategy
 */
function generatePronounceable(length) {
  const vowels = 'aeou'; // Skip 'i' to avoid confusion with 'l'
  const consonants = 'bcdfghjkmnprstvwxyz'; // Skip 'l' and 'q' for clarity
  
  let result = '';
  
  // Pattern: C-V-C-V-C or similar alternating patterns
  for (let i = 0; i < length; i++) {
    if (i % 2 === 0) {
      // Consonant positions (0, 2, 4)
      result += consonants[Math.floor(Math.random() * consonants.length)];
    } else {
      // Vowel positions (1, 3)
      result += vowels[Math.floor(Math.random() * vowels.length)];
    }
  }
  
  return result;
}

/**
 * Validate user code format
 * Must match: ac[0-9]{2}[a-z]{5} with pronounceable pattern
 */
export function isValidUserCode(code) {
  if (!code || typeof code !== 'string') return false;
  
  // Format check: ac + 2 digits + 5 lowercase chars
  const pattern = /^ac[0-9]{2}[a-z]{5}$/;
  if (!pattern.test(code)) return false;
  
  // Phonetic check: should have good consonant-vowel balance
  const suffix = code.substring(4); // Get 5-char suffix (skip "ac" + 2 digits)
  return hasGoodPhonetics(suffix);
}

/**
 * Check if string has pronounceable pattern (borrowed from KidLisp)
 */
function hasGoodPhonetics(str) {
  const hasVowel = /[aeou]/.test(str);
  const hasConsonant = /[bcdfghjkmnprstvwxyz]/.test(str);
  return hasVowel && hasConsonant;
}

/**
 * Parse user code from text (case-insensitive)
 * Extracts ac[0-9]{2}[a-z]{5} pattern from strings
 */
export function parseUserCode(text) {
  if (!text) return null;
  
  // Normalize to lowercase
  const normalized = text.toLowerCase();
  
  // Extract pattern: ac + 2 digits + 5 letters
  const match = normalized.match(/ac[0-9]{2}[a-z]{5}/);
  return match ? match[0] : null;
}

/**
 * Ensure MongoDB index exists for user codes
 */
export async function ensureUserCodeIndex(database) {
  const users = database.db.collection('users');
  
  // Create unique index on code field
  await users.createIndex(
    { code: 1 }, 
    { 
      unique: true,
      sparse: true, // Allow documents without code (during migration)
      name: 'code_unique'
    }
  );
  
  console.log('✅ User code unique index ensured');
}

/**
 * Generate unique user code (check MongoDB for collisions)
 * @param {Object} database - MongoDB connection
 * @param {Date} createdAt - User account creation date
 */
export async function generateUniqueUserCode(database, createdAt = new Date()) {
  const users = database.db.collection('users');
  const maxAttempts = 100;
  
  for (let attempt = 0; attempt < maxAttempts; attempt++) {
    const code = generateUserCode(createdAt);
    
    // Check if code already exists
    const existing = await users.findOne({ code: code });
    
    if (!existing) {
      return code;
    }
    
    console.log(`⚠️  Collision on ${code}, retrying... (${attempt + 1}/${maxAttempts})`);
  }
  
  throw new Error('Failed to generate unique user code after ' + maxAttempts + ' attempts');
}
