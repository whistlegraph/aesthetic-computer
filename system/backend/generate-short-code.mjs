// Generate Short Code, 25.10.22
// Shared code generator for paintings, tapes, and kidlisp
// Supports both random and inferred (source-aware) code generation

import { customAlphabet } from 'nanoid';

// Code generator alphabets - try lowercase first, add uppercase only on collision
const lowercaseConsonants = 'bcdfghjklmnpqrstvwxyz';
const lowercaseVowels = 'aeiou';
const uppercaseConsonants = 'BCDFGHJKLMNPQRSTVWXYZ';
const uppercaseVowels = 'AEIOU';
const numbers = '23456789'; // Exclude 0,1 (look like O,l)

// Lowercase-only alphabet for initial attempts
const lowercaseAlphabet = lowercaseConsonants + lowercaseVowels + numbers;
// Full alphabet with uppercase for collisions
const fullAlphabet = lowercaseConsonants + lowercaseVowels + uppercaseConsonants + uppercaseVowels + numbers;

const CODE_LENGTH = 3;
const LOWERCASE_ATTEMPTS = 50; // Try lowercase-only first
const MAX_COLLISION_ATTEMPTS = 100;

const lowercaseNanoid = customAlphabet(lowercaseAlphabet, CODE_LENGTH);
const fullNanoid = customAlphabet(fullAlphabet, CODE_LENGTH);

/**
 * Generate a unique short code with MongoDB collision checking
 * @param {Object} collection - MongoDB collection to check for existing codes
 * @param {Object} options - Generation options
 * @param {string} options.mode - 'random' or 'inferred' (default: 'random')
 * @param {string} options.sourceText - Source text for inference (required if mode='inferred')
 * @param {string} options.type - Content type: 'kidlisp', 'painting', or 'tape'
 * @returns {Promise<string>} Unique 3-character code (or longer if exhausted)
 */
export async function generateUniqueCode(collection, options = {}) {
  const { mode = 'random', sourceText, type } = options;
  
  // For inferred mode, try intelligent codes first
  if (mode === 'inferred' && sourceText && type === 'kidlisp') {
    console.log(`🧠 Attempting inferred code generation from source...`);
    const inferredCodes = await generateInferredCodes(sourceText, type);
    
    // Try each inferred code
    for (const inferredCode of inferredCodes) {
      const existing = await collection.findOne({ code: inferredCode });
      if (!existing) {
        console.log(`✨ Using inferred code: ${inferredCode}`);
        return inferredCode;
      }
    }
    
    console.log(`⚠️  All inferred codes taken, falling back to random generation`);
  }
  
  // Random generation (or fallback from inferred)
  return await generateRandomCode(collection);
}

/**
 * Generate random pronounceable code with collision checking
 * @param {Object} collection - MongoDB collection to check for existing codes
 * @returns {Promise<string>} Unique code
 */
async function generateRandomCode(collection) {
  // First 50 attempts: lowercase only
  for (let attempt = 0; attempt < LOWERCASE_ATTEMPTS; attempt++) {
    const code = lowercaseNanoid();
    
    const existing = await collection.findOne({ code });
    if (!existing) {
      return code;
    }
    
    console.log(`⚠️  Lowercase code collision detected: ${code}, retrying...`);
  }
  
  // Next 50 attempts: include uppercase
  for (let attempt = LOWERCASE_ATTEMPTS; attempt < MAX_COLLISION_ATTEMPTS; attempt++) {
    const code = fullNanoid();
    
    const existing = await collection.findOne({ code });
    if (!existing) {
      console.log(`⚠️  Using uppercase in code after ${LOWERCASE_ATTEMPTS} lowercase attempts: ${code}`);
      return code;
    }
    
    console.log(`⚠️  Full alphabet code collision detected: ${code}, retrying...`);
  }
  
  // If we hit max attempts, use a longer code with full alphabet
  const longerNanoid = customAlphabet(fullAlphabet, CODE_LENGTH + 1);
  const longerCode = longerNanoid();
  console.log(`⚠️  Max collisions reached, using longer code: ${longerCode}`);
  return longerCode;
}

/**
 * Generate inferred codes from source text (KidLisp source code)
 * Analyzes text to create meaningful, pronounceable codes
 * @param {string} sourceText - Source code to analyze
 * @param {string} type - Content type (currently only 'kidlisp' supported)
 * @returns {Promise<string[]>} Array of candidate codes, sorted by quality
 */
async function generateInferredCodes(sourceText, type) {
  const codes = [];
  const cleanSource = sourceText.trim().toLowerCase();
  
  // Temporal hints (day of week, date markers)
  const now = new Date();
  const dayOfWeek = ['sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat'][now.getDay()];
  const dayChar = dayOfWeek.charAt(0); // 's', 'm', 't', 'w', etc.
  const monthChar = ['j', 'f', 'm', 'a', 'm', 'j', 'j', 'a', 's', 'o', 'n', 'd'][now.getMonth()];
  const dayOfMonth = now.getDate();
  const isWeekend = now.getDay() === 0 || now.getDay() === 6;
  
  // Phonemic helpers for better human readability
  const vowels = 'aeiou';
  const consonants = 'bcdfghjklmnpqrstvwxyz';
  
  // Check if a string has good vowel-consonant balance for pronunciation
  function hasGoodPhonetics(str) {
    const hasVowel = /[aeiou]/.test(str);
    const hasConsonant = /[bcdfghjklmnpqrstvwxyz]/.test(str);
    return hasVowel && hasConsonant;
  }
  
  // Create pronounceable combinations by inserting vowels
  function makePronounceable(consonantString, targetLength = 3) {
    if (consonantString.length === 0) return '';
    if (consonantString.length >= targetLength && hasGoodPhonetics(consonantString)) {
      return consonantString.substring(0, targetLength);
    }
    
    let result = consonantString.charAt(0);
    const vowelChoices = ['a', 'e', 'i', 'o', 'u'];
    
    for (let i = 1; i < consonantString.length && result.length < targetLength; i++) {
      if (result.length < targetLength - 1) {
        result += vowelChoices[i % vowelChoices.length];
      }
      if (result.length < targetLength) {
        result += consonantString.charAt(i);
      }
    }
    
    while (result.length < targetLength) {
      result += vowelChoices[result.length % vowelChoices.length];
    }
    
    return result.substring(0, targetLength);
  }
  
  // KidLisp-specific inference
  if (type === 'kidlisp') {
    // Common KidLisp functions
    const kidlispFunctions = [
      'wipe', 'ink', 'line', 'box', 'circle', 'rect', 'def', 'later', 
      'scroll', 'resolution', 'gap', 'frame', 'brush', 'clear', 'repeat'
    ];
    
    const colors = ['red', 'blue', 'green', 'yellow', 'white', 'black', 'gray', 'purple', 'orange'];
    
    // Find functions in source
    const foundFunctions = kidlispFunctions.filter(fn => 
      cleanSource.includes(`(${fn}`) || cleanSource.includes(` ${fn} `)
    );
    
    const foundColors = colors.filter(color => cleanSource.includes(color));
    
    // Strategy 1: First letters of functions with vowel insertion
    if (foundFunctions.length >= 2) {
      const firstLetters = foundFunctions.slice(0, 3).map(fn => fn.charAt(0)).join('');
      codes.push(makePronounceable(firstLetters, 3));
      codes.push(makePronounceable(firstLetters, 4));
    }
    
    // Strategy 2: Function + color combinations
    if (foundFunctions.length > 0 && foundColors.length > 0) {
      const f = foundFunctions[0].charAt(0);
      const c = foundColors[0].charAt(0);
      codes.push(f + 'a' + c); // "wab" for wipe blue
      codes.push(f + 'i' + c); // "wib"
      codes.push(f + 'o' + c); // "wob"
    }
    
    // Strategy 3: Line-by-line first letters
    const lines = cleanSource.split('\n').map(l => l.trim()).filter(l => l.length > 0);
    if (lines.length >= 2 && lines.length <= 6) {
      const lineWords = lines.map(line => (line.match(/[a-z]+/g) || [])[0]).filter(w => w);
      if (lineWords.length >= 2) {
        const lineCode = lineWords.map(w => w.charAt(0)).join('');
        codes.push(makePronounceable(lineCode, 3));
      }
    }
    
    // Strategy 4: Extract numbers
    const numbers = cleanSource.match(/\d+/g) || [];
    if (numbers.length > 0 && foundFunctions.length > 0) {
      const digit = numbers[0].charAt(0);
      const f = foundFunctions[0].charAt(0);
      codes.push(f + digit + (foundFunctions[1] ? foundFunctions[1].charAt(0) : 'a'));
    }
    
    // Temporal hints - VERY sparingly, only 2 codes, lowest priority
    // These appear after all meaningful strategies as a last resort
    if (foundFunctions.length > 0) {
      const f = foundFunctions[0].charAt(0);
      codes.push(f + dayChar + 'a'); // e.g., "wwa" for wipe-Wednesday
      if (dayOfMonth < 10) {
        codes.push(f + dayOfMonth + 'a'); // e.g., "w2a" for wipe on 2nd
      }
    }
  }
  
  // Filter and score codes
  const validCodes = codes.filter(code => 
    /^[a-z0-9]+$/.test(code) && 
    code.length >= 3 && 
    code.length <= 4 && 
    /[a-z]/.test(code)
  );
  
  // Score codes by phonetic quality
  const scorePhonetics = (code) => {
    let score = 0;
    if (hasGoodPhonetics(code)) score += 10;
    if (/^[bcdfghjklmnpqrstvwxyz][aeiou]/.test(code)) score += 5;
    if (/[aeiou][bcdfghjklmnpqrstvwxyz]$/.test(code)) score += 3;
    if (code.length === 3) score += 2;
    return score;
  };
  
  return [...new Set(validCodes)]
    .sort((a, b) => scorePhonetics(b) - scorePhonetics(a))
    .slice(0, 20); // Return top 20 candidates
}
