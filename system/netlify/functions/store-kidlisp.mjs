// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import crypto from 'crypto';

// Feature flag for Tezos integration
const TEZOS_ENABLED = process.env.TEZOS_ENABLED !== 'false';

// Dynamically extract KidLisp function names from kidlisp.mjs
async function getKidLispFunctionNames() {
  try {
    // Import the kidlisp module
    const kidlispModule = await import('../../public/aesthetic.computer/lib/kidlisp.mjs');
    
    // Create a temporary KidLisp instance to access the global environment
    const { KidLisp } = kidlispModule;
    if (!KidLisp) {
      throw new Error('KidLisp class not found in kidlisp.mjs');
    }
    
    const tempLisp = new KidLisp();
    const globalEnv = tempLisp.getGlobalEnv();
    
    // Extract all function names from the global environment
    const functionNames = Object.keys(globalEnv).filter(key => {
      const value = globalEnv[key];
      return typeof value === 'function' || 
             (typeof value === 'object' && value !== null);
    });
    
    // Add common colors that might appear in KidLisp code
    const commonColors = ['red', 'green', 'blue', 'yellow', 'white', 'black', 'gray', 'purple', 'orange', 'pink', 'brown', 'cyan', 'magenta'];
    
    return [...new Set([...functionNames, ...commonColors])];
    
  } catch (error) {
    console.warn('Failed to dynamically load KidLisp functions, using fallback list:', error.message);
    
    // Fallback to a basic list if dynamic loading fails
    return ['wipe', 'ink', 'line', 'box', 'circle', 'rect', 'def', 'later', 'scroll', 'resolution', 'gap', 'frame', 'brush', 'clear', 'cls', 'help', 'reset', 'dot', 'pixel', 'stamp', 'paste', 'copy', 'move', 'rotate', 'scale', 'translate', 'fill', 'stroke', 'point', 'arc', 'bezier', 'noise', 'random', 'sin', 'cos', 'tan', 'sqrt', 'abs', 'floor', 'ceil', 'round', 'min', 'max', 'pow', 'log', 'exp', 'atan2', 'dist', 'lerp', 'map', 'norm', 'constrain', 'hue', 'sat', 'bright', 'alpha', 'red', 'green', 'blue', 'rgb', 'hsb', 'gray', 'background', 'foreground', 'text', 'font', 'repeat', 'rep', 'choose', 'overtone', 'rainbow', 'mic', 'amplitude'];
  }
}

// Ensure indexes exist (reentrant - safe to call multiple times)
async function ensureIndexes(collection) {
  try {
    // Create unique index on code field
    await collection.createIndex({ code: 1 }, { 
      unique: true, 
      background: true,
      name: 'kidlisp_code_unique'
    });
    
    // Create unique index on hash field for deduplication
    await collection.createIndex({ hash: 1 }, { 
      unique: true, 
      background: true,
      name: 'kidlisp_hash_unique'
    });
    
    // Create index on when field for analytics/cleanup queries
    await collection.createIndex({ when: 1 }, { 
      background: true,
      name: 'kidlisp_when'
    });
    
    // Create index on user field for user-specific queries
    await collection.createIndex({ user: 1 }, { 
      background: true,
      sparse: true, // Only index documents that have the user field
      name: 'kidlisp_user'
    });
    
    console.log('📦 Kidlisp indexes ensured');
  } catch (error) {
    // Ignore index creation errors (they're likely already created)
    console.warn('Index creation warning (likely already exist):', error.message);
  }
}

export async function handler(event, context) {
  // Log all incoming requests for debugging
  console.log(`📥 Kidlisp store request: ${event.httpMethod} ${event.path || event.rawUrl || 'unknown'}`);
  console.log(`📊 Headers:`, Object.keys(event.headers || {}).length > 0 ? Object.keys(event.headers) : 'none');
  if (event.body) {
    console.log(`📦 Body length: ${event.body.length} characters`);
  }

  if (event.httpMethod === 'OPTIONS') {
    console.log(`✅ Handling OPTIONS preflight request`);
    return respond(200, '');
  }

  try {
    const database = await connect();
    const collection = database.db.collection('kidlisp');
    
    // Ensure indexes exist (safe to call multiple times)
    await ensureIndexes(collection);

    if (event.httpMethod === 'POST') {
      const { source } = JSON.parse(event.body || '{}');
      
      if (!source || typeof source !== 'string' || source.length > 50000) {
        return respond(400, { error: 'Invalid source' });
      }

      // Extract user from authorization (optional)
      let user;
      try {
        user = await authorize(event.headers);
        console.log(`👤 User authorized: ${user ? user.sub : 'none'}`);
      } catch (error) {
        console.log(`🔓 No user authorization (anonymous cache)`);
      }

      const hash = crypto.createHash('sha256').update(source.trim()).digest('hex');
      console.log(`🔍 Source hash: ${hash.substring(0, 16)}...`);
      
      // Check for existing by hash first
      const existing = await collection.findOne({ hash });
      if (existing) {
        // Paranoid collision check: verify source actually matches
        if (existing.source === source.trim()) {
          await collection.updateOne(
            { hash }, 
            { 
              $inc: { hits: 1 },
              $set: { lastAccessed: new Date() }
            }
          );
          console.log(`♻️ Found existing cache: ${existing.code}`);
          await database.disconnect();
          return respond(200, { code: existing.code, cached: true });
        } else {
          // Extremely rare: hash collision detected!
          console.error('💥 SHA-256 collision detected!', { hash, existing: existing.source, new: source.trim() });
          // Continue to create new entry with different code
        }
      }

      // Generate unique nanoid with smart inference from KidLisp source
      const { customAlphabet } = await import("nanoid");
      let code;
      let attempts = 0;
      let currentLength = 3; // Start with 3 characters (1-2 reserved for system)
      let useFullAlphabet = false; // Start with lowercase preference
      const maxAttemptsPerAlphabet = 15;
      const maxAttemptsPerLength = 30;
      const maxLength = 12;
      
      // Prefer lowercase + numbers first (more readable/typable)
      const lowercaseAlphabet = '0123456789abcdefghijklmnopqrstuvwxyz';
      const fullAlphabet = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
      
      // 🧠 Smart inference: Try to generate meaningful codes from source content
      async function generateInferredCodes(source) {
        const codes = [];
        const cleanSource = source.trim().toLowerCase();
        
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
            // Add vowel between consonants for better pronunciation
            if (result.length < targetLength - 1) {
              result += vowelChoices[i % vowelChoices.length];
            }
            if (result.length < targetLength) {
              result += consonantString.charAt(i);
            }
          }
          
          // Ensure we hit target length
          while (result.length < targetLength) {
            result += vowelChoices[result.length % vowelChoices.length];
          }
          
          return result.substring(0, targetLength);
        }
        
        // Extract KidLisp function names dynamically from kidlisp.mjs
        const kidlispFunctions = await getKidLispFunctionNames();
        
        // 🎯 HOLISTIC ANALYSIS: Find functions used throughout entire source (not just beginning)
        const foundFunctions = kidlispFunctions.filter(fn => 
          cleanSource.includes(`(${fn}`) || cleanSource.includes(` ${fn} `) || cleanSource.startsWith(fn)
        );
        
        // 🌍 WHOLE-SOURCE SAMPLING: Extract functions from different sections
        const sourceThirds = [
          cleanSource.substring(0, Math.floor(cleanSource.length / 3)),
          cleanSource.substring(Math.floor(cleanSource.length / 3), Math.floor(2 * cleanSource.length / 3)),
          cleanSource.substring(Math.floor(2 * cleanSource.length / 3))
        ];
        
        const functionsFromBeginning = kidlispFunctions.filter(fn => sourceThirds[0].includes(`(${fn}`) || sourceThirds[0].includes(` ${fn} `));
        const functionsFromMiddle = kidlispFunctions.filter(fn => sourceThirds[1].includes(`(${fn}`) || sourceThirds[1].includes(` ${fn} `));
        const functionsFromEnd = kidlispFunctions.filter(fn => sourceThirds[2].includes(`(${fn}`) || sourceThirds[2].includes(` ${fn} `));
        
        console.log(`🔍 Functions by section: Start[${functionsFromBeginning.join(',')}] Mid[${functionsFromMiddle.join(',')}] End[${functionsFromEnd.join(',')}]`);
        
        // Extract numbers from source for informed seeding
        const numbers = cleanSource.match(/\d+/g) || [];
        const singleDigits = numbers.map(n => n.charAt(0)).filter(d => d !== '0'); // Avoid leading zeros
        const doubleDigits = numbers.filter(n => n.length >= 2).map(n => n.substring(0, 2));
        
        // Extract words from each line for more granular analysis
        const lines = cleanSource.split('\n').map(line => line.trim()).filter(line => line.length > 0);
        const wordsPerLine = lines.map(line => (line.match(/[a-z]+/g) || []).filter(word => kidlispFunctions.includes(word) || ['red', 'green', 'blue', 'yellow', 'white', 'black', 'gray', 'purple', 'orange'].includes(word)));
        
        // Extract ALL words (not just kidlisp functions) for broader seeding
        const allWordsPerLine = lines.map(line => line.match(/[a-z]+/g) || []);
        
        // 🌊 WAVE SAMPLING: Extract from beginning, middle, and end lines
        const totalLines = lines.length;
        const beginningLines = lines.slice(0, Math.ceil(totalLines / 3));
        const middleLines = lines.slice(Math.ceil(totalLines / 3), Math.ceil(2 * totalLines / 3));
        const endLines = lines.slice(Math.ceil(2 * totalLines / 3));
        
        const beginningWords = beginningLines.flatMap(line => line.match(/[a-z]+/g) || []);
        const middleWords = middleLines.flatMap(line => line.match(/[a-z]+/g) || []);
        const endWords = endLines.flatMap(line => line.match(/[a-z]+/g) || []);
        
        // Helper function to get second characters from word arrays
        function getSecondChars(wordArrays) {
          return wordArrays.flat().map(word => word.length >= 2 ? word.charAt(1) : null).filter(c => c !== null);
        }
        
        // Helper function to get nth character from words
        function getNthChars(wordArrays, n) {
          return wordArrays.flat().map(word => word.length > n ? word.charAt(n) : null).filter(c => c !== null);
        }
        
        if (foundFunctions.length > 0) {
          const primaryFunction = foundFunctions[0];
          
          // 🎵 STRATEGY 1: Phonemic First Letters with Vowel Insertion
          // For "wipe blue line ink red" -> extract first letters "wblir" -> make pronounceable "walir", "welir", "wilir" etc.
          if (foundFunctions.length >= 2) {
            const firstLetters = foundFunctions.map(fn => fn.charAt(0)).join('');
            const phoneticCodes = [];
            
            // Try different vowel patterns for the consonant string
            if (firstLetters.length >= 3) {
              phoneticCodes.push(makePronounceable(firstLetters, 3));
              phoneticCodes.push(makePronounceable(firstLetters, 4));
              
              // Alternative patterns: consonant-vowel-consonant (CVC)
              if (firstLetters.length >= 2) {
                phoneticCodes.push(firstLetters.charAt(0) + 'a' + firstLetters.charAt(1)); // wa + next consonant
                phoneticCodes.push(firstLetters.charAt(0) + 'i' + firstLetters.charAt(1)); // wi + next consonant  
                phoneticCodes.push(firstLetters.charAt(0) + 'o' + firstLetters.charAt(1)); // wo + next consonant
              }
            } else if (firstLetters.length === 2) {
              // Two functions: make CVC or CVCV patterns
              phoneticCodes.push(firstLetters.charAt(0) + 'a' + firstLetters.charAt(1)); // "wl" -> "wal"
              phoneticCodes.push(firstLetters.charAt(0) + 'i' + firstLetters.charAt(1)); // "wl" -> "wil"
              phoneticCodes.push(firstLetters.charAt(0) + 'o' + firstLetters.charAt(1)); // "wl" -> "wol"
              phoneticCodes.push(firstLetters + 'a'); // "wl" -> "wla"
              phoneticCodes.push(firstLetters + 'i'); // "wl" -> "wli"
            }
            
            codes.push(...phoneticCodes.filter(code => code.length >= 3));
          }
          
          // 🎵 STRATEGY 1B: Second Character Extraction (NEW!)
          // Extract second characters from functions for variation
          if (foundFunctions.length >= 2) {
            const secondLetters = foundFunctions.map(fn => fn.length >= 2 ? fn.charAt(1) : '').filter(c => c !== '').join('');
            
            if (secondLetters.length >= 2) {
              // Mix first and second characters
              const firstChar = foundFunctions[0].charAt(0);
              const secondChar = foundFunctions[0].length >= 2 ? foundFunctions[0].charAt(1) : '';
              const thirdChar = foundFunctions[1].charAt(0);
              
              if (secondChar) {
                codes.push(firstChar + secondChar + thirdChar); // "wip" for wipe+line
                codes.push(makePronounceable(firstChar + secondChar + thirdChar, 3));
              }
              
              // Pure second character combinations  
              codes.push(makePronounceable(secondLetters, 3));
              codes.push(makePronounceable(secondLetters, 4));
            }
          }
          
          // 🎵 STRATEGY 1C: Number Integration (NEW!)
          // Incorporate numbers from the source as meaningful elements
          if (singleDigits.length > 0 && foundFunctions.length > 0) {
            const primaryFunc = foundFunctions[0].charAt(0);
            const primarySecond = foundFunctions[0].length >= 2 ? foundFunctions[0].charAt(1) : '';
            const digit = singleDigits[0];
            
            // Create number-enhanced codes
            codes.push(primaryFunc + digit + (foundFunctions[1] ? foundFunctions[1].charAt(0) : 'a')); // "w5l" for wipe 5 line
            codes.push(primaryFunc + primarySecond + digit); // "wi5" for wipe 5
            codes.push(digit + primaryFunc + (primarySecond || 'a')); // "5wi" for 5 wipe
            
            // Try with double digits for more specific seeding
            if (doubleDigits.length > 0) {
              const doubleDigit = doubleDigits[0];
              codes.push(primaryFunc + doubleDigit); // "w25" 
              codes.push(doubleDigit + primaryFunc); // "25w"
            }
          }
          
          // 🎵 STRATEGY 2: Enhanced Line-by-line Analysis (ENHANCED!)
          // For multiline code, try multiple character extraction patterns
          if (lines.length >= 2 && lines.length <= 6) {
            const lineStarters = [];
            const lineSeconds = [];
            const lineNumbers = [];
            
            // Extract first chars, second chars, and numbers from each line
            for (let i = 0; i < allWordsPerLine.length; i++) {
              const words = allWordsPerLine[i];
              if (words.length > 0) {
                lineStarters.push(words[0].charAt(0));
                if (words[0].length >= 2) {
                  lineSeconds.push(words[0].charAt(1));
                }
                
                // Extract numbers from this line
                const lineNumberMatches = lines[i].match(/\d+/g);
                if (lineNumberMatches && lineNumberMatches.length > 0) {
                  lineNumbers.push(lineNumberMatches[0].charAt(0));
                }
              }
            }
            
            // PATTERN A: Traditional first characters (enhanced)
            if (lineStarters.length >= 2) {
              const lineCode = lineStarters.join('');
              
              // For your example: "wipe blue" -> "w", "line" -> "l", "ink red" -> "i", "iine" -> "i" = "wlii"
              // Create multiple phonemic variations
              if (lineCode.length >= 3) {
                // Direct concatenation if already pronounceable
                if (hasGoodPhonetics(lineCode.substring(0, 3))) {
                  codes.push(lineCode.substring(0, 3));
                }
                
                // Create consonant-vowel-consonant patterns
                codes.push(makePronounceable(lineCode, 3));
                codes.push(makePronounceable(lineCode, 4));
                
                // Manual vowel insertion patterns for better pronunciation
                if (lineCode.length >= 3) {
                  const c1 = lineCode.charAt(0), c2 = lineCode.charAt(1), c3 = lineCode.charAt(2);
                  codes.push(c1 + 'a' + c2 + c3); // "wlii" -> "wali"
                  codes.push(c1 + 'i' + c2 + c3); // "wlii" -> "wili" 
                  codes.push(c1 + 'o' + c2 + c3); // "wlii" -> "woli"
                  codes.push(c1 + c2 + 'a' + c3); // "wlii" -> "wlai"
                  codes.push(c1 + c2 + 'i' + c3); // "wlii" -> "wlii" (same but good pattern)
                  codes.push(c1 + c2 + 'o' + c3); // "wlii" -> "wloi"
                }
                
                // Alternative: Take first 2 consonants and add vowel ending
                if (lineCode.length >= 2) {
                  const first2 = lineCode.substring(0, 2);
                  codes.push(first2 + 'a'); // "wl" -> "wla"
                  codes.push(first2 + 'i'); // "wl" -> "wli" 
                  codes.push(first2 + 'o'); // "wl" -> "wlo"
                  codes.push(first2 + 'e'); // "wl" -> "wle"
                  codes.push(first2 + 'u'); // "wl" -> "wlu"
                }
                
              } else if (lineCode.length === 2) {
                codes.push(makePronounceable(lineCode, 3));
                codes.push(lineCode + 'a');
                codes.push(lineCode + 'i');
                codes.push(lineCode + 'o');
                codes.push(lineCode + 'e');
              }
              
              // Try reverse order for variety
              if (lineCode.length >= 3) {
                const reversed = lineCode.split('').reverse().join('');
                codes.push(makePronounceable(reversed, 3));
              }
            }
            
            // PATTERN B: Second character extraction (NEW!)
            if (lineSeconds.length >= 2) {
              const secondsCode = lineSeconds.join('');
              codes.push(makePronounceable(secondsCode, 3));
              codes.push(makePronounceable(secondsCode, 4));
              
              // Mix first and second characters for variety
              if (lineStarters.length >= 2 && lineSeconds.length >= 1) {
                const mixed = lineStarters[0] + lineSeconds[0] + lineStarters[1];
                codes.push(mixed); // "wil" for wipe(w) + wipe(i) + line(l)
                codes.push(makePronounceable(mixed, 3));
              }
            }
            
            // PATTERN C: Number integration (NEW!)
            if (lineNumbers.length > 0 && lineStarters.length >= 1) {
              const numCode = lineNumbers[0];
              const firstChar = lineStarters[0];
              const secondChar = lineStarters.length >= 2 ? lineStarters[1] : 'a';
              
              // Create number-informed codes
              codes.push(firstChar + numCode + secondChar); // "w5l" 
              codes.push(numCode + firstChar + secondChar); // "5wl"
              codes.push(firstChar + secondChar + numCode); // "wl5"
              
              // If we have multiple numbers, create rich combinations
              if (lineNumbers.length >= 2) {
                codes.push(firstChar + lineNumbers[0] + lineNumbers[1]); // "w52"
                codes.push(lineNumbers[0] + lineNumbers[1] + firstChar); // "52w"
              }
            }
          }
          
          // 🎵 STRATEGY 3: Syllable-based abbreviations with vowel harmony
          const syllableMap = {
            'wipe': 'wi', 'line': 'li', 'circle': 'cir', 'rectangle': 'rec',
            'background': 'bak', 'foreground': 'for', 'repeat': 'rep',
            'random': 'ran', 'rainbow': 'rai', 'amplitude': 'amp'
          };
          
          foundFunctions.forEach(fn => {
            if (syllableMap[fn]) {
              const syl = syllableMap[fn];
              if (syl.length >= 3) {
                codes.push(syl);
              } else {
                // Create 3-char versions with vowel endings
                codes.push(syl + 'a');
                codes.push(syl + 'i');
                codes.push(syl + 'o');
              }
            }
          });
          
          // 🎵 STRATEGY 4: Color-aware phonemic combinations
          const colors = ['red', 'blue', 'green', 'yellow', 'white', 'black', 'gray', 'purple', 'orange'];
          const foundColors = colors.filter(color => cleanSource.includes(color));
          
          if (foundColors.length > 0) {
            const primaryColor = foundColors[0];
            const colorCode = primaryColor.charAt(0);
            const functionCode = primaryFunction.charAt(0);
            
            // Create pronounceable function+color combinations
            codes.push(functionCode + 'a' + colorCode); // "w" + "a" + "b" = "wab" for wipe blue
            codes.push(functionCode + 'i' + colorCode); // "w" + "i" + "b" = "wib" for wipe blue
            codes.push(functionCode + 'o' + colorCode); // "w" + "o" + "b" = "wob" for wipe blue
            
            // Reverse order too
            codes.push(colorCode + 'a' + functionCode); // "b" + "a" + "w" = "baw" for blue wipe
            codes.push(colorCode + 'i' + functionCode); // "b" + "i" + "w" = "biw" for blue wipe
          }
          
          // 🎵 STRATEGY 5: Memorable word-like patterns
          // Create word-like codes that are easy to remember and pronounce
          const wordPatterns = {
            'wipe': ['wop', 'wap', 'wip'],
            'line': ['lin', 'lon', 'lan'],
            'ink': ['ink', 'unk', 'onk'],
            'box': ['bax', 'bix', 'bux'],
            'circle': ['cir', 'cur', 'cor'],
            'red': ['rad', 'rid', 'rod'],
            'blue': ['blu', 'bla', 'ble'],
            'green': ['grn', 'gra', 'gre']
          };
          
          foundFunctions.forEach(fn => {
            if (wordPatterns[fn]) {
              codes.push(...wordPatterns[fn]);
            }
          });
          
          foundColors.forEach(color => {
            if (wordPatterns[color]) {
              codes.push(...wordPatterns[color]);
            }
          });
          
          // 🎵 STRATEGY 6: Context-aware combinations
          // Look for common KidLisp patterns and create memorable codes
          if (cleanSource.includes('wipe') && cleanSource.includes('blue')) codes.push('wub', 'wib', 'wab');
          if (cleanSource.includes('line') && cleanSource.includes('red')) codes.push('lir', 'lar', 'lor');
          if (cleanSource.includes('ink') && cleanSource.includes('red')) codes.push('ira', 'ire', 'iro');
          if (cleanSource.includes('ink') && cleanSource.includes('blue')) codes.push('iba', 'ibe', 'ibo');
          if (cleanSource.includes('box') && cleanSource.includes('red')) codes.push('bra', 'bre', 'bro');
          
          // Multi-color combinations
          if (foundColors.length >= 2) {
            const colorInitials = foundColors.slice(0, 2).map(c => c.charAt(0)).join('');
            codes.push(primaryFunction.charAt(0) + colorInitials); // e.g., "lbr" for line blue red
            codes.push(makePronounceable(primaryFunction.charAt(0) + colorInitials, 3));
          }
        }
        
          // 🎵 STRATEGY 7: Variable Names and Custom Identifiers (NEW!)
          // Extract user-defined variables and custom names that appear in def/later statements
          const defMatches = cleanSource.match(/\(def\s+([a-z][a-z0-9]*)/g) || [];
          const laterMatches = cleanSource.match(/\(later\s+([a-z][a-z0-9]*)/g) || [];
          const customNames = [
            ...defMatches.map(m => m.split(/\s+/)[1]),
            ...laterMatches.map(m => m.split(/\s+/)[1])
          ].filter(name => name && name.length >= 2);
          
          customNames.forEach(name => {
            if (name.length >= 3) {
              codes.push(name.substring(0, 3)); // Use first 3 chars of custom name
              codes.push(makePronounceable(name, 3));
            }
            // Mix custom name with function
            if (foundFunctions.length > 0) {
              const funcChar = foundFunctions[0].charAt(0);
              codes.push(funcChar + name.substring(0, 2)); // "w" + "my" = "wmy"
              codes.push(name.charAt(0) + funcChar + name.charAt(1)); // "m" + "w" + "y" = "mwy"
            }
          });
          
          // 🎵 STRATEGY 8: Enhanced Fallback with Informed Seeding (ENHANCED!)
          // If we have any words at all, create pronounceable patterns with numbers
          const allWords = cleanSource.match(/[a-z]+/g) || [];
          if (allWords.length >= 2) {
            const initials = allWords.slice(0, 4).map(w => w.charAt(0)).join('');
            const seconds = allWords.slice(0, 4).map(w => w.length >= 2 ? w.charAt(1) : '').filter(c => c !== '').join('');
            
            codes.push(makePronounceable(initials, 3));
            codes.push(makePronounceable(initials, 4));
            
            // Try second character combinations
            if (seconds.length >= 2) {
              codes.push(makePronounceable(seconds, 3));
            }
            
            // Mix initials with numbers
            if (singleDigits.length > 0) {
              const digit = singleDigits[0];
              if (initials.length >= 2) {
                codes.push(initials.charAt(0) + digit + initials.charAt(1)); // "w5l"
                codes.push(digit + initials.substring(0, 2)); // "5wl"
                codes.push(initials.substring(0, 2) + digit); // "wl5"
              }
            }
            
            // Try different vowel insertions for first 3 letters
            if (initials.length >= 3) {
              const base = initials.substring(0, 3);
              codes.push(base);
              if (!hasGoodPhonetics(base)) {
                // Insert vowels to improve pronunciation
                codes.push(base.charAt(0) + 'a' + base.substring(1));
                codes.push(base.charAt(0) + 'i' + base.substring(1));
                codes.push(base.charAt(0) + 'o' + base.substring(1));
              }
            }
          }
          
          // 🎵 STRATEGY 9: Contextual Number Patterns (NEW!)
          // Create meaningful patterns based on common number contexts
          if (numbers.length > 0) {
            // Common KidLisp number contexts
            const repeatMatch = cleanSource.match(/repeat\s+(\d+)/);
            const sizeMatch = cleanSource.match(/(\d+)\s+(\d+)/); // width height patterns
            
            if (repeatMatch) {
              const count = repeatMatch[1];
              const digit = count.charAt(0);
              if (foundFunctions.length > 0) {
                const func = foundFunctions[0].charAt(0);
                codes.push('r' + digit + func); // "r5w" for repeat 5 wipe
                codes.push(func + 'r' + digit); // "wr5"
              }
            }
            
            if (sizeMatch && foundFunctions.length > 0) {
              const w = sizeMatch[1].charAt(0);
              const h = sizeMatch[2].charAt(0);
              const func = foundFunctions[0].charAt(0);
              codes.push(func + w + h); // "w32" for wipe 320 240
              codes.push(w + h + func); // "32w"
            }
          }
          
          // 🌍 STRATEGY 10: HOLISTIC SAMPLING - Beginning/Middle/End Combinations (NEW!)
          // Sample from different sections of the source to capture the full scope
          if (functionsFromBeginning.length > 0 && functionsFromEnd.length > 0) {
            const startChar = functionsFromBeginning[0].charAt(0);
            const endChar = functionsFromEnd[0].charAt(0);
            
            // Create beginning-end bridges
            codes.push(startChar + 'to' + endChar); // "wtor" for wipe...to...red
            codes.push(startChar + 'n' + endChar); // "wnr" 
            codes.push(startChar + 'x' + endChar); // "wxr"
            codes.push(makePronounceable(startChar + endChar, 3)); // "war", "wer", etc.
            
            // Add middle if available
            if (functionsFromMiddle.length > 0) {
              const midChar = functionsFromMiddle[0].charAt(0);
              codes.push(startChar + midChar + endChar); // "wlr" for wipe...line...red
              codes.push(makePronounceable(startChar + midChar + endChar, 3));
              
              // Alternative patterns
              codes.push(endChar + midChar + startChar); // "rlw" (reverse)
              codes.push(midChar + startChar + endChar); // "lwr" (middle-first)
            }
          }
          
          // 🌊 STRATEGY 11: WAVE PATTERN - Distributed Word Sampling (NEW!)
          // Instead of just first words, sample from throughout the source
          const distributedWords = [];
          if (beginningWords.length > 0) distributedWords.push(beginningWords[0]);
          if (middleWords.length > 0) distributedWords.push(middleWords[0]);
          if (endWords.length > 0) distributedWords.push(endWords[0]);
          
          if (distributedWords.length >= 2) {
            const distributedInitials = distributedWords.map(w => w.charAt(0)).join('');
            codes.push(makePronounceable(distributedInitials, 3));
            codes.push(makePronounceable(distributedInitials, 4));
            
            // Mix with second characters from distributed words
            const distributedSeconds = distributedWords.map(w => w.length >= 2 ? w.charAt(1) : '').filter(c => c !== '').join('');
            if (distributedSeconds.length >= 2) {
              codes.push(makePronounceable(distributedSeconds, 3));
              // Interleave first and second chars
              if (distributedInitials.length >= 2 && distributedSeconds.length >= 1) {
                const interleaved = distributedInitials.charAt(0) + distributedSeconds.charAt(0) + distributedInitials.charAt(1);
                codes.push(interleaved);
                codes.push(makePronounceable(interleaved, 3));
              }
            }
          }
          
          // 🔄 STRATEGY 12: REVERSE AND INSIDE-OUT SAMPLING (NEW!)
          // Analyze from end-to-beginning and center-outward
          const reversedLines = [...lines].reverse();
          const reversedWordsPerLine = reversedLines.map(line => line.match(/[a-z]+/g) || []);
          const reverseFirstChars = reversedWordsPerLine.slice(0, 3).map(words => words.length > 0 ? words[0].charAt(0) : '').filter(c => c !== '').join('');
          
          if (reverseFirstChars.length >= 2) {
            codes.push(makePronounceable(reverseFirstChars, 3));
            codes.push('rev' + reverseFirstChars.charAt(0)); // "revw" for reverse-wipe
            
            // Mix forward and backward patterns
            const forwardFirstChars = allWordsPerLine.slice(0, 3).map(words => words.length > 0 ? words[0].charAt(0) : '').filter(c => c !== '').join('');
            if (forwardFirstChars.length >= 2 && reverseFirstChars.length >= 1) {
              codes.push(forwardFirstChars.charAt(0) + reverseFirstChars.charAt(0) + (forwardFirstChars.charAt(1) || 'a'));
              codes.push(reverseFirstChars.charAt(0) + forwardFirstChars.charAt(0) + (reverseFirstChars.charAt(1) || 'a'));
            }
          }
          
          // 🎯 STRATEGY 13: CENTER-OUT EXPANSION (NEW!)
          // Start from middle of source and work outward
          const centerLineIndex = Math.floor(lines.length / 2);
          const centerWords = lines[centerLineIndex] ? (lines[centerLineIndex].match(/[a-z]+/g) || []) : [];
          
          if (centerWords.length > 0) {
            const centerChar = centerWords[0].charAt(0);
            
            // Find words that come before and after center
            const beforeCenterWords = lines.slice(0, centerLineIndex).flatMap(line => line.match(/[a-z]+/g) || []);
            const afterCenterWords = lines.slice(centerLineIndex + 1).flatMap(line => line.match(/[a-z]+/g) || []);
            
            if (beforeCenterWords.length > 0 && afterCenterWords.length > 0) {
              const beforeChar = beforeCenterWords[beforeCenterWords.length - 1].charAt(0); // Last before center
              const afterChar = afterCenterWords[0].charAt(0); // First after center
              
              codes.push(beforeChar + centerChar + afterChar); // "wcr" for wipe-center-red
              codes.push(makePronounceable(beforeChar + centerChar + afterChar, 3));
              codes.push(centerChar + beforeChar + afterChar); // "cwr" (center-first)
            }
          }
          
          // 🔀 STRATEGY 14: RANDOM SAMPLING FROM WHOLE SOURCE (NEW!)
          // Pick random words from throughout the source instead of sequential
          const allSourceWords = cleanSource.match(/[a-z]+/g) || [];
          if (allSourceWords.length >= 3) {
            const randomIndices = [];
            const step = Math.max(1, Math.floor(allSourceWords.length / 3));
            randomIndices.push(0); // Still include beginning
            randomIndices.push(Math.floor(allSourceWords.length / 2)); // Middle
            randomIndices.push(allSourceWords.length - 1); // End
            
            // Add a truly random sample
            if (allSourceWords.length > 3) {
              const randomIndex = Math.floor(Math.random() * allSourceWords.length);
              randomIndices.push(randomIndex);
            }
            
            const randomSample = randomIndices.map(i => allSourceWords[i]).filter(w => w).slice(0, 3);
            const randomInitials = randomSample.map(w => w.charAt(0)).join('');
            
            if (randomInitials.length >= 2) {
              codes.push(makePronounceable(randomInitials, 3));
              codes.push(makePronounceable(randomInitials, 4));
              
              // Mix random sampling with numbers from source
              if (singleDigits.length > 0) {
                const digit = singleDigits[Math.floor(Math.random() * singleDigits.length)];
                codes.push(randomInitials.charAt(0) + digit + (randomInitials.charAt(1) || 'a'));
                codes.push(digit + randomInitials.substring(0, 2));
              }
            }
          }        // Filter for valid, pronounceable codes
        const validCodes = codes.filter(code => 
          /^[a-z0-9]+$/.test(code) && 
          code.length >= 3 && 
          code.length <= 12 && 
          /[a-z]/.test(code)
        );
        
        // Sort by phonemic quality (vowel-consonant balance) and length
        const scorePhonetics = (code) => {
          let score = 0;
          if (hasGoodPhonetics(code)) score += 10;
          if (/^[bcdfghjklmnpqrstvwxyz][aeiou]/.test(code)) score += 5; // Starts with consonant-vowel (good pronunciation)
          if (/[aeiou][bcdfghjklmnpqrstvwxyz]$/.test(code)) score += 3; // Ends with vowel-consonant
          if (/^[bcdfghjklmnpqrstvwxyz][aeiou][bcdfghjklmnpqrstvwxyz]$/.test(code)) score += 8; // Perfect CVC pattern
          if (!/\d/.test(code)) score += 3; // Prefer letters over numbers, but don't exclude numbers
          if (code.length === 3) score += 2; // Prefer 3-char codes
          
          // Bonus for common phonemic patterns
          if (/^(wa|wi|wo|we|wu|ba|bi|bo|be|bu|ka|ki|ko|ke|ku|la|li|lo|le|lu|ma|mi|mo|me|mu|na|ni|no|ne|nu|ra|ri|ro|re|ru|sa|si|so|se|su|ta|ti|to|te|tu)/.test(code)) score += 4;
          
          // Penalty for difficult consonant clusters
          if (/[bcdfghjklmnpqrstvwxyz]{3,}/.test(code)) score -= 5;
          
          // Bonus for balanced vowel distribution
          const vowelCount = (code.match(/[aeiou]/g) || []).length;
          const consonantCount = (code.match(/[bcdfghjklmnpqrstvwxyz]/g) || []).length;
          if (vowelCount > 0 && consonantCount > 0 && Math.abs(vowelCount - consonantCount) <= 1) score += 3;
          
          // NEW: Bonus for meaningful number integration
          const numberCount = (code.match(/\d/g) || []).length;
          if (numberCount === 1 && code.length === 3) score += 2; // Single digit in 3-char code is good
          if (numberCount >= 2) score -= 2; // Too many numbers reduce readability
          
          // NEW: Bonus for source-derived patterns (numbers from actual source)
          if (numberCount > 0 && singleDigits.includes(code.match(/\d/g)?.[0])) score += 3;
          
          // NEW: Penalty for starting with numbers (less readable)
          if (/^\d/.test(code)) score -= 1;
          
          return score;
        };
        
        return [...new Set(validCodes)]
          .sort((a, b) => scorePhonetics(b) - scorePhonetics(a) || a.length - b.length);
      }
      
      // Try inferred codes first, then fall back to random generation
      const inferredCodes = await generateInferredCodes(source);
      console.log(`🧠 Generated ${inferredCodes.length} inferred codes: [${inferredCodes.join(', ')}]`);
      
      // First, try all inferred codes
      for (const inferredCode of inferredCodes) {
        if (!(await collection.findOne({ code: inferredCode }))) {
          code = inferredCode;
          console.log(`✨ Using inferred code: ${code}`);
          break;
        }
      }
      
      // If no inferred code worked, fall back to random generation
      if (!code) {
        console.log(`🎲 Falling back to random generation`);
        
        do {
          // Switch to full alphabet if we've tried enough lowercase attempts
          if (!useFullAlphabet && attempts >= maxAttemptsPerAlphabet) {
            useFullAlphabet = true;
            console.log(`🔄 Switching to full alphabet for length ${currentLength}`);
          }
          
          // Grow length and reset if we've exhausted both alphabets
          if (attempts >= maxAttemptsPerLength && currentLength < maxLength) {
            currentLength++; // Grow by 1 character
            attempts = 0; // Reset attempts for new length
            useFullAlphabet = false; // Start with lowercase preference again
            console.log(`🔄 Growing nanoid length to ${currentLength} characters`);
          }
          
          const alphabet = useFullAlphabet ? fullAlphabet : lowercaseAlphabet;
          const generator = customAlphabet(alphabet, currentLength);
          code = generator();
          attempts++;
          
          if (attempts > maxAttemptsPerLength && currentLength >= maxLength) {
            console.error(`💥 Failed to generate unique code after maximum attempts`);
            break;
          }
        } while (await collection.findOne({ code }));
      }

      console.log(`🎲 Generated code: ${code} (attempt ${attempts})`);

      // Create document with optional user attribution (matching painting structure)
      const doc = {
        code,
        source: source.trim(),
        hash,
        when: new Date(),
        lastAccessed: new Date(),
        hits: 1
      };

      // Add user sub if authenticated (same as painting records)
      if (user?.sub) {
        doc.user = user.sub;
        console.log(`🔗 Linked to user: ${user.sub}`);
      }

      try {
        await collection.insertOne(doc);
        console.log(`💾 Cached new source: ${code}`);

        // 🪙 Tezos Integration: Attempt to mint KidLisp meme coin
        let tezosResult = null;
        if (TEZOS_ENABLED && user?.sub) {
          try {
            // Import Tezos integration (only when needed)
            const { integrateWithKidLispCache } = await import('../../../tezos/src/integration.js');
            
            console.log(`🪙 Attempting Tezos token mint for user: ${user.sub}`);
            tezosResult = await integrateWithKidLispCache(source.trim(), user, code);
            
            if (tezosResult.minted) {
              console.log(`✨ Tezos token minted successfully: Token ID ${tezosResult.tokenId} on ${tezosResult.network}`);
              
              // Update the document with Tezos information
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: true,
                      tokenId: tezosResult.tokenId,
                      txHash: tezosResult.txHash,
                      creatorAddress: tezosResult.creatorAddress,
                      codeHash: tezosResult.codeHash,
                      network: tezosResult.network,
                      mintedAt: new Date()
                    }
                  }
                }
              );
            } else if (tezosResult.exists) {
              console.log(`🎯 Tezos token already exists: Token ID ${tezosResult.tokenId} on ${tezosResult.network}`);
              
              // Update with existing token info
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: false,
                      exists: true,
                      tokenId: tezosResult.tokenId,
                      codeHash: tezosResult.codeHash,
                      network: tezosResult.network,
                      reason: tezosResult.reason,
                      checkedAt: new Date()
                    }
                  }
                }
              );
            } else {
              console.log(`⚠️ Tezos token operation skipped: ${tezosResult.reason}`);
              
              // Store the reason for debugging
              await collection.updateOne(
                { code },
                {
                  $set: {
                    tezos: {
                      minted: false,
                      exists: false,
                      reason: tezosResult.reason,
                      error: tezosResult.error,
                      attemptedAt: new Date()
                    }
                  }
                }
              );
            }
          } catch (tezosError) {
            console.error('🚨 Tezos integration error:', tezosError);
            
            // Store error information for debugging
            await collection.updateOne(
              { code },
              {
                $set: {
                  tezos: {
                    minted: false,
                    error: tezosError.message,
                    failedAt: new Date()
                  }
                }
              }
            );
          }
        } else if (!TEZOS_ENABLED) {
          console.log(`🚫 Tezos integration disabled by feature flag`);
        }

        await database.disconnect();
        
        // Include Tezos information in response
        const responseData = { 
          code, 
          cached: false,
          ...(tezosResult && { tezos: tezosResult })
        };
        
        return respond(201, responseData);

      } catch (insertError) {
        // Handle duplicate key errors (race conditions)
        if (insertError.code === 11000) {
          if (insertError.keyPattern?.hash) {
            // Hash collision - return existing record
            const existing = await collection.findOne({ hash });
            if (existing) {
              await collection.updateOne(
                { hash }, 
                { 
                  $inc: { hits: 1 },
                  $set: { lastAccessed: new Date() }
                }
              );
              console.log(`🔄 Race condition resolved: ${existing.code}`);
              await database.disconnect();
              return respond(200, { code: existing.code, cached: true });
            }
          } else if (insertError.keyPattern?.code) {
            await database.disconnect();
            return respond(500, { error: 'Code generation collision, please retry' });
          }
        }
        throw insertError;
      }

    } else if (event.httpMethod === 'GET') {
      const code = event.queryStringParameters?.code;
      const codes = event.queryStringParameters?.codes;
      
      // Handle batch retrieval of multiple codes
      if (codes) {
        console.log(`🔍 Batch lookup request for codes parameter: ${codes}`);
        
        let codeList;
        try {
          // Support both comma-separated string and JSON array format
          if (codes.startsWith('[')) {
            codeList = JSON.parse(codes);
          } else {
            codeList = codes.split(',').map(c => c.trim()).filter(c => c.length > 0);
          }
        } catch (error) {
          return respond(400, { error: 'Invalid codes format. Use comma-separated or JSON array.' });
        }
        
        if (!Array.isArray(codeList) || codeList.length === 0) {
          return respond(400, { error: 'Codes must be a non-empty array' });
        }
        
        if (codeList.length > 50) { // Limit batch size
          return respond(400, { error: 'Too many codes. Maximum 50 per request.' });
        }
        
        console.log(`🔍 Looking up ${codeList.length} codes: ${codeList.join(', ')}`);

        // Fetch all documents in a single database query
        const docs = await collection.find({ code: { $in: codeList } }).toArray();
        
        // Update hit counts for found documents
        if (docs.length > 0) {
          const foundCodes = docs.map(doc => doc.code);
          await collection.updateMany(
            { code: { $in: foundCodes } }, 
            { 
              $inc: { hits: 1 },
              $set: { lastAccessed: new Date() }
            }
          );
        }

        // Create response map with found and missing codes
        const results = {};
        const found = [];
        const missing = [];
        
        codeList.forEach(requestedCode => {
          const doc = docs.find(d => d.code === requestedCode);
          if (doc) {
            results[requestedCode] = {
              source: doc.source,
              when: doc.when,
              hits: doc.hits + 1,
              user: doc.user || null
            };
            found.push(requestedCode);
          } else {
            results[requestedCode] = null;
            missing.push(requestedCode);
          }
        });

        console.log(`📤 Batch retrieved: ${found.length} found, ${missing.length} missing`);
        if (found.length > 0) console.log(`✅ Found: ${found.join(', ')}`);
        if (missing.length > 0) console.log(`❌ Missing: ${missing.join(', ')}`);

        await database.disconnect();
        return respond(200, { 
          results,
          summary: {
            requested: codeList.length,
            found: found.length,
            missing: missing.length,
            foundCodes: found,
            missingCodes: missing
          }
        });
      }
      
      // Handle single code retrieval (existing functionality)
      if (!code) {
        return respond(400, { error: 'Code or codes parameter required' });
      }

      console.log(`🔍 Looking up code: ${code}`);

      const doc = await collection.findOne({ code });
      if (!doc) {
        await database.disconnect();
        return respond(404, { error: 'Not found' });
      }

      await collection.updateOne(
        { code }, 
        { 
          $inc: { hits: 1 },
          $set: { lastAccessed: new Date() }
        }
      );

      console.log(`📤 Retrieved source: ${code} (${doc.source.length} chars, ${doc.hits + 1} hits)`);

      await database.disconnect();
      return respond(200, { 
        source: doc.source,
        when: doc.when,
        hits: doc.hits + 1,
        user: doc.user || null
      });
    }

    await database.disconnect();
    return respond(405, { error: 'Method not allowed' });

  } catch (error) {
    console.error('❌ Kidlisp cache error:', error);
    return respond(500, { error: 'Internal server error' });
  }
}
