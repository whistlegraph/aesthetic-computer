// Store KidLisp, 2025.01.16
// Caches KidLisp source code and generates short URLs for QR codes

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import crypto from 'crypto';

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
      function generateInferredCodes(source) {
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
        
        // Extract KidLisp function names and create abbreviations
        const kidlispFunctions = ['wipe', 'ink', 'line', 'box', 'circle', 'rect', 'def', 'later', 'scroll', 'resolution', 'gap', 'frame', 'brush', 'clear', 'cls', 'help', 'reset', 'dot', 'pixel', 'stamp', 'paste', 'copy', 'move', 'rotate', 'scale', 'translate', 'fill', 'stroke', 'point', 'arc', 'bezier', 'noise', 'random', 'sin', 'cos', 'tan', 'sqrt', 'abs', 'floor', 'ceil', 'round', 'min', 'max', 'pow', 'log', 'exp', 'atan2', 'dist', 'lerp', 'map', 'norm', 'constrain', 'hue', 'sat', 'bright', 'alpha', 'red', 'green', 'blue', 'rgb', 'hsb', 'gray', 'background', 'foreground', 'text', 'font', 'repeat', 'rep', 'choose', 'overtone', 'rainbow', 'mic', 'amplitude'];
        
        // Find functions used in source and create smart abbreviations
        const foundFunctions = kidlispFunctions.filter(fn => 
          cleanSource.includes(`(${fn}`) || cleanSource.includes(` ${fn} `) || cleanSource.startsWith(fn)
        );
        
        // Extract words from each line for more granular analysis
        const lines = cleanSource.split('\n').map(line => line.trim()).filter(line => line.length > 0);
        const wordsPerLine = lines.map(line => (line.match(/[a-z]+/g) || []).filter(word => kidlispFunctions.includes(word) || ['red', 'green', 'blue', 'yellow', 'white', 'black', 'gray', 'purple', 'orange'].includes(word)));
        
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
          
          // 🎵 STRATEGY 2: Line-by-line First Letters (preserving structure)
          // For multiline code, try first letter of first word per line
          if (lines.length >= 2 && lines.length <= 4) {
            const lineStarters = [];
            for (const wordList of wordsPerLine) {
              if (wordList.length > 0) {
                lineStarters.push(wordList[0].charAt(0));
              }
            }
            
            if (lineStarters.length >= 2) {
              const lineCode = lineStarters.join('');
              if (lineCode.length >= 3) {
                codes.push(lineCode);
                codes.push(makePronounceable(lineCode, 3));
              } else if (lineCode.length === 2) {
                codes.push(makePronounceable(lineCode, 3));
                codes.push(lineCode + 'a');
                codes.push(lineCode + 'i');
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
        
        // 🎵 STRATEGY 7: Fallback pronounceable patterns
        // If we have any words at all, create pronounceable patterns
        const allWords = cleanSource.match(/[a-z]+/g) || [];
        if (allWords.length >= 2) {
          const initials = allWords.slice(0, 4).map(w => w.charAt(0)).join('');
          codes.push(makePronounceable(initials, 3));
          codes.push(makePronounceable(initials, 4));
          
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
        
        // Filter for valid, pronounceable codes
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
          if (/^[bcdfghjklmnpqrstvwxyz][aeiou]/.test(code)) score += 5; // Starts with consonant-vowel
          if (!/\d/.test(code)) score += 3; // Prefer letters over numbers
          if (code.length === 3) score += 2; // Prefer 3-char codes
          return score;
        };
        
        return [...new Set(validCodes)]
          .sort((a, b) => scorePhonetics(b) - scorePhonetics(a) || a.length - b.length);
      }
      
      // Try inferred codes first, then fall back to random generation
      const inferredCodes = generateInferredCodes(source);
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

        await database.disconnect();
        return respond(201, { code, cached: false });

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
      
      if (!code) {
        return respond(400, { error: 'Code required' });
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
